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
		if (f >= cfg['stitch_basefreqs'][n]) and (f < cfg['stitch_endfreqs'][n]):
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
	cfg['stitch_bandIDs'] = range(len(cfg['stitch_basefreqs']))
	cfg['verbose'] = True

	return cfg

"""Write a new .input file"""
def modifyInpFile(basename,cfg):

	inputfile = basename + '.input'
	fin = open(inputfile,'r')
	lines = fin.readlines()
	fin.close()

	outputinputfile = basename + '.newinput'
	fout = open(outputinputfile, 'w')
	for line in lines:
		# Replace lines like "NUM CHANNELS 36:    8000" with the same line except modify the num of channels
		# and similar with lines like "BW (MHZ) "
		if line[:13] == 'NUM CHANNELS ':
			line = line[:20] + str(cfg['target_nchan']) + '\n'
		elif line[:9] == 'BW (MHZ) ':
			line = '%s%.12f\n' % (line[:20],cfg['target_bw'])
		fout.write(line)
	fout.close()

"""Copy visibilities from given base DiFX fileset into new file, while doing frequency stitching during the process"""
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

	# Determine starting freq index for the re-numbered output freq indexing
	# 1) retain recorded bands as-is i.e. retain all non-zoom bands
	stitch_basefreqindex = 9999
	for d in datastreams:
		mz = min(d.zoomfreqindex)
		stitch_basefreqindex = min(mz,stitch_basefreqindex)
	# 2) retain zoombands that meet target bandwidth just need to re-number them
	zoom_remaps = [-1]*500
	for d in datastreams:
		shortened_zfreqs = []
		shortened_zpols = []
		shortened_zbpols = []
		shortened_zbindices = []
		for z in d.zoomfreqindex:
			if (freqs[z].bandwidth == target_bw) and ((freqs[z].numchan / freqs[z].specavg) == target_nchan):
				if zoom_remaps[z] < 0:
					zoom_remaps[z] = stitch_basefreqindex
					stitch_basefreqindex += 1
				nn = d.zoomfreqindex.find(z)
				shortened_zfreqs.append(zoom_remaps[z])
				shortened_zpols.append(d.zoomfreqpols[nn])
				zbidx = sum(d.zoomfreqpols[:nn])
				shortened_zbpols.append(d.zoombandpol[zbidx])
				shortened_zbindices.append(d.zoombandindex[zbidx:(zbidx+d.zoomfreqpols[nn])])
		d.zoomfreqindex = list(shortened_zfreqs)
		d.zoomfreqpols = list(shortened_zpols)
		d.zoombandpol = list(shortened_zbpols)
		d.zoombandindex = list(shortened_zbindices)
		d.nzoomfreq = len(d.zoomfreqindex)
		d.nzoomband = sum(d.zoomfreqpols)

	# Read the DiFX .difx/DIFX_* file
	difxfilename = difxfileslist[0]
	difxfile = open(difxfilename, 'r')
	difxout = open(difxfilename+'.stitched', 'w')
	(vishdr,binhdr) = getVisibilityHeader(difxfile)

	# Affected baselines (make work arrays only for those, save some memory)
	## TODO: can expand this to include more than just one affected antenna: refID=[]
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
	stitch_workbufs = { bline:{ polkey:{bandkey:numpy.copy(blank_viz) for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_chcounts = { bline:{ polkey:{bandkey:0 for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_freqids = { bline:{ polkey:{bandkey:[] for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_freqbws = { bline:{ polkey:{bandkey:[] for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitch_timestamps = { bline:{ polkey:{bandkey:-1 for bandkey in stitch_bandIDs } for polkey in polpairs } for bline in baseline_list }
	stitched_freqs = []
	copied_freqs = []
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
					stitch_freqbws[baseline][polpair][gi] = []
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
					stitch_freqbws[baseline][polpair][gi] = []
					stitch_workbufs[baseline][polpair][gi].fill(0)

				# Insert the new visibility data
				visdata = numpy.fromstring(rawvis, dtype='complex64')
				choffset = int( ((fsky - stitch_basefreqs[gi]) / target_bw) * target_nchan )
				stitch_workbufs[baseline][polpair][gi][choffset:(choffset+nchan)] = visdata
				stitch_chcounts[baseline][polpair][gi] += nchan
				stitch_freqids[baseline][polpair][gi].append(freqindex)
				stitch_freqbws[baseline][polpair][gi].append(bw)

				# print (' glue bline %s %s band %d : ch %4d ... %4d : %d new chs : stitched %d ch total from FQ %s.' % (baselinestr,polpair,gi,choffset,choffset+nchan-1,nchan,stitch_chcounts[baseline][polpair][gi],str(stitch_freqids[baseline][polpair][gi])))
				# print choffset, nchan, choffset+nchan, stitch_chcounts[baseline][polpair][gi], (100.0*stitch_chcounts[baseline][polpair][gi])/target_nchan

				if stitch_chcounts[baseline][polpair][gi] >= target_nchan:

					TS = mjd + stitch_timestamps[baseline][polpair][gi] / 86400.0
					if cfg['verbose']:
						print ('stitch: %s-%s/%d(%d):%.4f/%s  mjd:%12.8f nchan:%d bw:%.7f fq=%s uvw=%s'
							% (ant1name,ant2name,freqindex,gi,fsky,polpair,TS,stitch_chcounts[baseline][polpair][gi],target_bw,str(stitch_freqids[baseline][polpair][gi]),str(uvw)))
					#else:
					#	print stitch_workbufs[baseline][polpair][gi]
					#	print (' writeout bline %s %s band %d' % (baselinestr,polpair,gi))

					# Double-check the bandwidth
					bwsum = numpy.sum(stitch_freqbws[baseline][polpair][gi])
					if numpy.abs(target_bw - bwsum) > 100:
						print ('Warning: stitched bands gave %.6f MHz bandwidth, differs from target %.6f MHz!' % (bwsum,target_bw))

					# Write header, with an updated freq index
					vishdr[5] = stitch_basefreqindex + gi
					newbinhdr = parseDiFX.make_output_header_v1(vishdr)
					difxout.write(newbinhdr)
					# Write data
					stitch_workbufs[baseline][polpair][gi].tofile(difxout)

					# Update count
					nstitched += 1
					fid = min(stitch_freqids[baseline][polpair][gi])
					if fid not in stitched_freqs:
						stitched_freqs.append(fid)

					# Reset
					stitch_workbufs[baseline][polpair][gi].fill(0)
					stitch_chcounts[baseline][polpair][gi] = 0
					stitch_freqids[baseline][polpair][gi] = []
					stitch_freqbws[baseline][polpair][gi] = []

		else:
			# Different baseline, without the target antenna

			if (bw == target_bw) and (nchan == target_nchan):
				# Just copy out the data
				if cfg['verbose']:
					print ('copy  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))
				if zoom_remaps[freqindex] >= 0:
					print ('remapping copied zoomband index %d to %d' % (freqindex,zoom_remaps[freqindex]))
					vishdr[5] = zoom_remaps[freqindex]
					newbinhdr = parseDiFX.make_output_header_v1(vishdr)
					difxout.write(newbinhdr)
				else:
					difxout.write(binhdr)
				difxout.write(rawvis)
				copied_freqs.append(freqindex)
				ncopied += 1

			else:
				# Zoomband on non-ALMA baseline, ignore it because it is covered
				# by some other complete zoomband that doesn't need stitching
				nskipped += 1
				if cfg['verbose']:
					#print ('skip  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))
					pass

		# Next header
		(vishdr,binhdr) = getVisibilityHeader(difxfile)

#####
		if nstitched>=20:
			print ('---quitting early, for DEBUG')
			break

	# Generate new reference .input for later for the user
	outputinputfile = basename + '.newinput'
	f = open(outputinputfile,"w") # truncate
	f.close()
	new_freqs = []
	for n in range(stitch_basefreqindex):
		# recorded bands, without zoombands
		new_freqs.append(freqs[n])
	for n in stitched_freqs:
		# new stitched bands
		ff = freqs[n]
		ff.bandwidth = target_bw
		ff.numchan = target_nchan
		ff.specavg = 1
		new_freqs.append(ff)

	#print stitched_freqs
	#print copied_freqs
	#print new_freqs
	#print stitch_basefreqindex, len(new_freqs)-1

        parseDiFX.put_freqtable_info(outputinputfile, new_freqs)
	parseDiFX.put_datastreamtable_info(outputinputfile, datastreams)

	#modifyInpFile(basename,cfg)

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. copied        : %d' % (ncopied))
	print ('    vis. skipped       : %d' % (nskipped))
	print ('    vis. stitched      : %d' % (nstitched))
	print ('    incomplete stitch  : %d' % (nincomplete))
	print ('\nOutput files:')
	print ('    visbility data     : %s' % (difxfilename+'.stitched'))
	print ('    changes for .input : %s' % (difxfilename+'.newinput'))
	print (' ')

if len(sys.argv) < 3:
	print __doc__
	sys.exit(-1)

cfg = getConfig(sys.argv[1])
for difxf in sys.argv[2:]:
	stitchVisibilityfile(difxf,cfg)

