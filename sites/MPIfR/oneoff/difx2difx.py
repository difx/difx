#!/usr/bin/python
"""
Usage: difx2difx.py <difx2difx.cfg> <difx basename>

Stitches together in frequency domain the visibility data on the
baselines of a antenna, attempting to piece together a full band
out of shorter subbands. In other words assembles immediately
adjecent DiFX zoombands into wider bands as listed in the config file.

Output:
  <difx basename>/DIFX_*.stitched     frequency stitched visibility data
  <difx basename>/DIFX_*.ref_input    entries to use in a replacement .input file

"""
import glob, sys, os, struct, time, math, numpy, ConfigParser
import parseDiFX

"""Return the cross product of two sets"""
def setCrossProd(a,b):
	gen = ((x, y) for x in a for y in b)
	strconcatd = [ str(x)+str(y) for x,y in gen]
	return (strconcatd,gen)

"""Check if a frequency falls into one band in a set of frequency bands"""
def getGlueIndex(f,cfg):
	N = len(cfg['stitch_basefreqs'])
	for n in range(N):
		if (f >= cfg['stitch_basefreqs'][n]) and (f <= cfg['stitch_endfreqs'][n]):
			return n
	return -1

"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
def getVisibilityHeader(f):
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)

"""
Read a difx2difx configuration file.
Example contents:
  [config]
  target_bw: 32.000
  target_nchan: 4096
  stitch_antenna: AA
  stitch_basefreqs: 86476.00, 86412.00, 86380.00, 86316.00, 86252.00, 86188.00, 86124.00, 86060.00, 86028.00
"""
def getConfig(cfgfilename):

	cfg = {}

	cfgparser = ConfigParser.ConfigParser()
	cfgparser.read(cfgfilename)
	section = (cfgparser.sections())[0]

	# User settings
	cfg['target_bw'] = cfgparser.getfloat(section, 'target_bw')
	cfg['target_nchan'] = cfgparser.getint(section, 'target_nchan')
	cfg['stitch_antenna'] = cfgparser.get(section, 'stitch_antenna')

	# User frequencies
	fqlist = cfgparser.get(section, 'stitch_basefreqs')
	cfg['stitch_basefreqs'] = []
	cfg['stitch_endfreqs'] = []
	for fq in fqlist.split(', '):
		f = float(fq)
		cfg['stitch_basefreqs'].append(f)
		cfg['stitch_endfreqs'].append(f + cfg['target_bw'])

	# Internal settings
	MaxAnt = 16 # max nr of antennas
	cfg['stitch_bandIDs'] =  [n for n in range(len(cfg['stitch_basefreqs']))]
	cfg['verbose'] = False

	return cfg

def stitchVisibilityfile(basename,cfg):

	# Get settings
	target_bw = cfg['target_bw']
	target_nchan = cfg['target_nchan']
	stitch_basefreqs = cfg['stitch_basefreqs']
	stitch_endfreqs = cfg['stitch_endfreqs']
	stitch_bandIDs = cfg['stitch_bandIDs']
	stitch_antenna = cfg['stitch_antenna']
	blank_viz = numpy.fromstring('\0'*8*target_nchan, dtype='complex64')

	# All polarisation pairs 'RR', 'RL', ... to 'YY'
	# pols_list = ['R','L','X','Y']
	pols_list = ['R','L']
	polpairs,tmp = setCrossProd(pols_list, pols_list)

	# Extract meta-infos from the DiFX .INPUT file
	inputfile = basename + '.input'
	difxfileslist = glob.glob(basename + '.difx/DIFX_*.s*.b*')
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		parser.error("Couldn't parse input file " + inputfile + " correctly")

	# Read the DiFX .difx/DIFX_* file
	difxfilename = difxfileslist[0]
	difxfile = open(difxfilename, 'r')
	difxout = open(difxfilename+'.stitched', 'w')
	templateinp = open(difxfilename+'.ref_input', 'w')
	(vishdr,binhdr) = getVisibilityHeader(difxfile)

	# Affected baselines (make work arrays only for those, save some memory)
	refID = -1
	for n in range(numtelescopes):
		if (stitch_antenna == telescopes[n].name):
			refID = n
			break
	if refID < 0:
		print ('Error: could not find %s in list of telescopes.' % (stitch_antenna))
		return
	baseline_list = [refID+1 + (refID+1)*256]
	for n in range(numtelescopes):
		if n != refID:
			baseline_list.append(n+1 + (refID+1)*256)
			baseline_list.append((refID+1) + (n+1)*256)

	# Dictionary of per-polarizationpair working buffers into which spectra are stitched
	#work_items_gen = lambda defaultval : { bline:{ polkey:{bandkey:defaultval for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	#stitch_workbufs = work_items_gen(numpy.copy(blank_viz))
	#stitch_chcounts = work_items_gen(0)
	#stitch_freqids = work_items_gen([])  # TODO: makes one [] and n references to same, how to avoid, and create n new []...?
	#stitch_timestamps = work_items_gen(-1)
	stitch_workbufs = { bline:{ polkey:{bandkey:numpy.copy(blank_viz) for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_chcounts = { bline:{ polkey:{bandkey:0 for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_freqids = { bline:{ polkey:{bandkey:[] for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_timestamps = { bline:{ polkey:{bandkey:-1 for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	freqIDs_stitched = []  # the last freqID out of each freq group stitched, useful to "fix" .INPUT file 'NUM CHANNELS <freqID> : <target_nchan>'
	nstitched = 0; ncopied = 0; nskipped = 0; nincomplete = 0

	# Parse each visibility entry
	seconds_prev = -1
	while len(vishdr) > 0:

		# Visibility properties
		baseline = vishdr[0]
		mjd = vishdr[1]
		seconds = vishdr[2]
		freqindex = vishdr[5]
		polpair = vishdr[6]
		weight = vishdr[8]
		uvw = vishdr[9:12]
		ant1 = baseline % 256
		ant2 = (baseline-ant1)/256
		ant1name = telescopes[ant1-1].name
		ant2name = telescopes[ant2-1].name
		T = mjd + seconds/86400.0
		baselinestr = '%s-%s' % (ant1name,ant2name)					

		# Number of channels in this baseband
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg
		fsky = freqs[freqindex].freq
		bw = freqs[freqindex].bandwidth

		# Read the entire visibility data from disk
		rawvis = difxfile.read(8*nchan)

		if (seconds != seconds_prev):
			print (('\n---- %d %12.7f ' + '-'*115) % (mjd,seconds))
			seconds_prev = seconds

		# Write out visibility to output file
		if (ant1name == stitch_antenna) or (ant2name == stitch_antenna):

			if baseline not in baseline_list:
				print refID, ant1, ant2
				print ('Didn''t find %d in hardcoded list of all baselines. Check MaxAnt in difx2difx.py source code...' % (baseline))
				# TODO: replace ant1name==stitch_antenna etc with just "if baseline in baseline_list" once certain...
				return

			if (bw == target_bw):

				# Visibility already at target bandwidth, just copy the data
				if cfg['verbose']:
					print ('take  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))
				difxout.write(binhdr)
				difxout.write(rawvis)
				ncopied += 1

				# Update where we are at
				gi = getGlueIndex(fsky,cfg)
				if (gi >= 0):
					stitch_timestamps[baseline][polpair][gi] = seconds
					stitch_chcounts[baseline][polpair][gi] = 0
					stitch_freqids[baseline][polpair][gi] = []
			else:
				# Need to stitch narrowband visibility into wideband context
				gi = getGlueIndex(fsky,cfg)
				if (gi < 0):
					print ('Crap.. could not locate %.6f MHz! Check hard-coded frequency table...' % (fsky))
					sys.exit(0)

				# We assume visibilities are stored increasing-in-time in the DiFX file, so a timestamp change
				# indicates that all data of the previous time should've been fully read by now (no older data follows)
				ncurr = stitch_chcounts[baseline][polpair][gi]
				if (stitch_timestamps[baseline][polpair][gi] < 0):
					stitch_timestamps[baseline][polpair][gi] = seconds
				if (seconds != stitch_timestamps[baseline][polpair][gi]):
					if (ncurr != 0) and (ncurr < target_nchan):
						TS = mjd + stitch_timestamps[baseline][polpair][gi] / 86400.0
						msg = '%s-%s/%d:%.7f/%s mjd:%12.8f only %d of %d channels' % (ant1name,ant2name,gi,stitch_basefreqs[gi],polpair,TS,ncurr,target_nchan)
						print ('Warning: incomplete stitch: %s' % (msg))
						nincomplete += 1

					#print (('---- %d %.7f ' + '-'*80 + '\n') % (mjd,seconds))
					stitch_timestamps[baseline][polpair][gi] = seconds
					stitch_chcounts[baseline][polpair][gi] = 0
					stitch_freqids[baseline][polpair][gi] = []
					stitch_workbufs[baseline][polpair][gi].fill(0)

				# Insert the new visibility data
				visdata = numpy.fromstring(rawvis, dtype='complex64')
				choffset = int( ((fsky - stitch_basefreqs[gi]) / target_bw) * target_nchan )
				stitch_workbufs[baseline][polpair][gi][choffset:(choffset+nchan)] = visdata
				stitch_chcounts[baseline][polpair][gi] += nchan
				stitch_freqids[baseline][polpair][gi].append(freqindex)

				# print (' glue bline %s %s band %d : ch %4d ... %4d : %d new chs : stitched %d ch total from FQ %s.' % (baselinestr,polpair,gi,choffset,choffset+nchan-1,nchan,stitch_chcounts[baseline][polpair][gi],str(stitch_freqids[baseline][polpair][gi])))
				# print choffset, nchan, choffset+nchan, stitch_chcounts[baseline][polpair][gi], (100.0*stitch_chcounts[baseline][polpair][gi])/target_nchan

				if stitch_chcounts[baseline][polpair][gi] >= target_nchan:

					TS = mjd + stitch_timestamps[baseline][polpair][gi] / 86400.0
					if cfg['verbose']:
						print ('stitch: %s-%s/%d(%d):%.4f/%s  mjd:%12.8f nchan:%d bw:%.7f fq=%s uvw=%s'
							% (ant1name,ant2name,freqindex,gi,fsky,polpair,TS,stitch_chcounts[baseline][polpair][gi],target_bw,str(stitch_freqids[baseline][polpair][gi],str(uvw))))
					#else:
					#	print stitch_workbufs[baseline][polpair][gi]
					#	print (' writeout bline %s %s band %d' % (baselinestr,polpair,gi))

					# Write header, then data
					difxout.write(binhdr)  # TODO: any need to tamper with the header? perhaps the Freq ID?
					stitch_workbufs[baseline][polpair][gi].tofile(difxout)
					nstitched += 1

					# Grow the template .INPUT file
					if not (freqindex in freqIDs_stitched):
						templateinp.write('NUM CHANNELS %2d: %7d\n' % (freqindex,target_nchan))
						freqIDs_stitched.append(freqindex)

					# Reset
					stitch_workbufs[baseline][polpair][gi].fill(0)
					stitch_chcounts[baseline][polpair][gi] = 0
					stitch_freqids[baseline][polpair][gi] = []

		else:

			if (bw == target_bw) and (nchan == target_nchan):
				# Just copy out the data
				if cfg['verbose']:
					print ('copy  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))
				difxout.write(binhdr)
				difxout.write(rawvis)
				ncopied += 1
			else:
				# Zoomband on non-ALMA baseline, ignore it because it is covered
				# by some other complete zoomband that doesn't need stitching
				nskipped += 1
				if cfg['verbose']:
					print ('skip  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))

		# Next header
		(vishdr,binhdr) = getVisibilityHeader(difxfile)

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. copied        : %d' % (ncopied))
	print ('    vis. skipped       : %d' % (nskipped))
	print ('    vis. stitched      : %d' % (nstitched))
	print ('    incomplete stitch  : %d' % (nincomplete))
	print ('\nOutput files:')
	print ('    visbility data     : %s' % (difxfilename+'.stitched'))
	print ('    changes for .input : %s' % (difxfilename+'.ref_input'))
	print (' ')

if len(sys.argv) < 3:
	print __doc__
	sys.exit(-1)

cfg = getConfig(sys.argv[1])
for difxf in sys.argv[2:]:
	stitchVisibilityfile(difxf,cfg)

