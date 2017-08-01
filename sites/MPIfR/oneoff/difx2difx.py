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
import glob, sys, os, struct, time, math, numpy, copy, ConfigParser
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
  stitch_antennas: AA, PV
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
	cfg['stitch_antennas'] = []
	salist = cfgparser.get(section, 'stitch_antennas')
	for sa in salist.split(', '):
		cfg['stitch_antennas'].append(sa)
	cfg['stitch_nstokes'] = int(cfgparser.get(section, 'stitch_nstokes'))

	# User frequencies
	fqlist = cfgparser.get(section, 'stitch_basefreqs')
	cfg['stitch_basefreqs'] = []
	cfg['stitch_endfreqs'] = []
	for fq in fqlist.split(', '):
		f = float(fq)
		cfg['stitch_basefreqs'].append(f)
		cfg['stitch_endfreqs'].append(f + cfg['target_bw']) # USB assumed

	# Internal settings
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
	stitch_antennas = cfg['stitch_antennas']
	blank_viz = numpy.fromstring('\0'*8*target_nchan, dtype='complex64')

	# All polarisation pairs 'RR', 'RL', ... to 'YY'
	if cfg['stitch_nstokes'] > 2:
		#pols_list = ['R','L']
		pols_list = ['R','L','X','Y']
		polpairs,tmp = setCrossProd(pols_list, pols_list)
	else:
		#polpairs = ['RR','LL']
		#polpairs = ['RR','LL','XX','YY']
		polpairs = ['RR','LL','XX','YY','RX','LY','XR','YL']

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','input')):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		parser.error("Couldn't parse input file " + inputfile + " correctly")

	# Internal frequency tables and remappings from old to new freq indices
	out_freqs = {}    # dict out['id']=Freq()
	in_rec_freqs = 0  # num of distinct recorded freqs
	n_max_freqs = 256
	freq_remaps = [-1]*n_max_freqs
	freq_remaps_isNew = [False]*n_max_freqs

	# Collect all recorded freqs listed in DATASTREAMS
	for d in datastreams:
		for fqidx in d.recfreqindex:
			if fqidx not in out_freqs.keys():
				freq_remaps[fqidx] = in_rec_freqs
				out_freqs[fqidx] = copy.deepcopy(freqs[fqidx])
				in_rec_freqs += 1
				print ("Keeping recorded frequency  : index %2d : %s" % (fqidx,out_freqs[fqidx].str()))
	# Collect all zoom bands listed in DATASTREAMS that already match the target bandwidth
	for d in datastreams:
		for fqidx in d.zoomfreqindex:
			zf = freqs[fqidx]
			if (zf.bandwidth != target_bw) or ((zf.numchan/zf.specavg) != target_nchan):
				# print ("Skipping zoom frequency     : index %d : %s" % (fqidx,zf.str()))
				continue
			if fqidx not in out_freqs.keys():
				out_freqs[fqidx] = copy.deepcopy(zf)
				print ("Keeping zoom frequency      : index %2d : %s" % (fqidx,zf.str()))
	# Invent new zoom bands if necessary
	stitch_out_ids = []
	for fsky in stitch_basefreqs:
		exists = any([fsky==zf for zf in out_freqs])
		if not exists:
			zf = parseDiFX.Freq()
			zf.freq = fsky
			zf.bandwidth = target_bw
			zf.numchan = target_nchan
			zf.specavg = 1
			zf.lsb = False # TODO
			id = len(out_freqs)
			while id in out_freqs.keys():
				id += 1
			out_freqs[id] = copy.deepcopy(zf)
			stitch_out_ids.append(id)
			print ("Creating new zoom frequency : index %2d : %s" % (id,zf.str()))
		else:
			print ("Zoom exists")
	stitch_ids = [n for n in range(len(stitch_out_ids))]

	# Map from old to new frequencies (recorded and zoom)
	for fi in range(len(freqs)):
		newidx = [ni for ni in range(len(out_freqs)) if freqs[fi]==out_freqs[ni]]
		if (len(newidx) > 0):
			ni = min(newidx)
			if (ni < in_rec_freqs):
				freq_remaps[fi] = ni
	# Also map each to-be-stitched-multi-zoom into respective new single post-stitch zoom
	for fi in range(in_rec_freqs,len(freqs)):
		if (freqs[fi].bandwidth == target_bw) and (freqs[fi].numchan/freqs[fi].specavg == target_nchan):
			continue
		stid = getGlueIndex(freqs[fi].freq,cfg)
		if (stid >= 0):
			freq_remaps[fi] = stitch_out_ids[stid]
			freq_remaps_isNew[fi] = True
			print ("Map zoom %s to stitched single %12.6f--%12.6f : in fq#%2d -> stitch#%d -> out fq#%2d" % (freqs[fi].str(), stitch_basefreqs[stid], stitch_endfreqs[stid],fi,stid,stitch_out_ids[stid]))

	# Read the DiFX .difx/DIFX_* file
	difxfileslist = glob.glob(basename + '.difx/DIFX_*.s*.b*')
	difxfilename = difxfileslist[0]
	difxfile = open(difxfilename, 'r')
	difxoutdir = basename + 'D2D.difx'
	difxoutname = difxoutdir+'/'+difxfilename[difxfilename.find('/')+1:]
	try:
		os.mkdir(difxoutdir)
	except:
		pass
	difxout = open(difxoutname, 'w')
	(vishdr,binhdr) = getVisibilityHeader(difxfile)

	# Pull out antenna indices based on telescope names
	stAntIDs = []
	for sa in stitch_antennas:
		id = [n for n in range(len(telescopes)) if telescopes[n].name==sa]
		if len(id) != 1:
			print ("Error: got %d hits for telescope %s in telescope list!" % (len(id),sa))
			return
		id = id[0] + 1 # index (0..N-1) into antenna nr (1..N)
		stAntIDs.append(id)

	# Affected baselines; data of these are stored in stitching memory, other baselines need no stitching
	baseline_list = []
	for sid in stAntIDs:
		# Cross
		for t in range(numtelescopes):
			tid = t + 1 # index (0..N-1) into antenna nr (1..N)
			if (tid != sid):
				baseline_list.append(tid + sid*256)
				baseline_list.append(sid + tid*256)
		# Auto
		baseline_list.append(sid + sid*256)
	#print stitch_out_ids, baseline_list, polpairs

	# Dictionary of per-polarizationpair working buffers into which spectra are stitched
	stitch_workbufs = { bline:{ polkey:{bandkey:numpy.copy(blank_viz) for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	stitch_chcounts = { bline:{ polkey:{bandkey:0 for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	stitch_freqbws = { bline:{ polkey:{bandkey:[] for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	stitch_timestamps = { bline:{ polkey:{bandkey:-1 for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
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

		# Remap the frequency reference
		out_freqindex = freqindex
		if freq_remaps[freqindex] >= 0:
			# print ('Remapping input freq index %d to old/new output freq %d' % (freqindex,freq_remaps[freqindex]))
			out_freqindex = freq_remaps[freqindex]
			vishdr[5] = out_freqindex
			binhdr = parseDiFX.make_output_header_v1(vishdr)
		stid = getGlueIndex(fsky,cfg)

		# Info string
		vis_info = '%s-%s/%d(%d):sf<%d>:%.7f/%s  mjd:%12.8f nchan:%4d bw:%.7f uvw=%s' % (ant1name,ant2name,out_freqindex,freqindex,stid,fsky,polpair,T,nchan,bw,str(uvw))

		# Write out visibility to output file
		if baseline not in baseline_list:

			# Baseline for which no freq stitching was requested

			# Keep data if bandwidth matches
			if (bw == target_bw) and (nchan == target_nchan):
				if cfg['verbose']:
					print ('copy  : %s' % (vis_info))
				assert(freq_remaps[freqindex] >= 0)
				difxout.write(binhdr)
				difxout.write(rawvis)
				ncopied += 1

			# Discard data if bandwidth mismatch. Happens e.g. for zoom bands on the non-stitch baselines,
			# can ignore these because covered by a different and complete zoomband that doesn't need stitching
			else:
				nskipped += 1
				if cfg['verbose']:
					print ('skip  : %s' % (vis_info))
					pass
		else:

			# Baseline where freq stitching was requested
			resetStitch = False

			if (bw == target_bw):

				# Visibility already at target bandwidth, just copy the data
				if cfg['verbose']:
					print ('take  : %s' % (vis_info))
				assert( (freq_remaps[freqindex] >= 0) and (freq_remaps[freqindex] not in stitch_out_ids))
				difxout.write(binhdr)
				difxout.write(rawvis)
				ncopied += 1

				# Signal for further below: clear old working data, set new timestamp
				resetStitch = True

			else:

				# Need to stitch narrowband visibility into wideband context
				if (stid < 0):
					print ('Crap.. could not locate %.6f MHz! Check the stitch frequencies in your config file!' % (fsky))
					return

				# Keep track of timestamp
				if (stitch_timestamps[baseline][polpair][stid] < 0):
					stitch_timestamps[baseline][polpair][stid] = seconds

				# Detect a jump in timestamp
				if (seconds != stitch_timestamps[baseline][polpair][stid]):
					# We assume the input visibilities are stored in time-increasing order,
					# so a timestamp change indicates all data of previous time should've been fully read by now (no older data follows)
					ncurr = stitch_chcounts[baseline][polpair][stid]
					if (ncurr > 0):
						TS_old = mjd + stitch_timestamps[baseline][polpair][stid] / 86400.0
						nincomplete += 1
						msg = '%s-%s/%d:%.7f/%s mjd:%12.8f only %d of %d channels' % (ant1name,ant2name,stid,stitch_basefreqs[stid],polpair,TS_old,ncurr,target_nchan)
						print ('Warning: discarded an incomplete stitch: %s' % (msg))
					assert(ncurr < target_nchan)
					stitch_timestamps[baseline][polpair][stid] = seconds
					stitch_chcounts[baseline][polpair][stid] = 0
					stitch_freqbws[baseline][polpair][stid] = []
					stitch_workbufs[baseline][polpair][stid].fill(0)
					#print (('---- %d %.7f ' + '-'*80 + '\n') % (mjd,seconds))

				# Insert the new visibility data into the correct working buffer at the correct location
				visdata = numpy.fromstring(rawvis, dtype='complex64')
				choffset = int( ((fsky - stitch_basefreqs[stid]) / target_bw) * target_nchan )
				stitch_workbufs[baseline][polpair][stid][choffset:(choffset+nchan)] = visdata
				stitch_chcounts[baseline][polpair][stid] += nchan
				stitch_freqbws[baseline][polpair][stid].append(bw)

				#print (' glue bline %s %s band %d : ch %4d ... %4d : %d new chs : stitched %d ch total' % (baselinestr,polpair,stid,choffset,choffset+nchan-1,nchan,stitch_chcounts[baseline][polpair][stid]))
				#print choffset, nchan, choffset+nchan, stitch_chcounts[baseline][polpair][stid], (100.0*stitch_chcounts[baseline][polpair][stid])/target_nchan

				# Assembled the full bandwidth now?
				if stitch_chcounts[baseline][polpair][stid] >= target_nchan:
					TS = mjd + stitch_timestamps[baseline][polpair][stid] / 86400.0
					bwsum = numpy.sum(stitch_freqbws[baseline][polpair][stid])
					vis_info = '%s-%s/%d(%d):sf<%d>:%.7f/%s  mjd:%12.8f nchan:%4d bw:%.7f uvw=%s' % (ant1name,ant2name,out_freqindex,freqindex,stid,fsky,polpair,TS,stitch_chcounts[baseline][polpair][stid],bwsum,str(uvw))
					if cfg['verbose']:
						print ('stitch: %s' % (vis_info))

					# Double-check the bandwidth
					if numpy.abs(target_bw - bwsum) > 100:
						print ('Warning: stitched bands gave %.6f MHz bandwidth, differs from target %.6f MHz!' % (bwsum,target_bw))

					# Write header and assembled data
					difxout.write(binhdr)
					stitch_workbufs[baseline][polpair][stid].tofile(difxout)
					nstitched += 1

					# Reset
					resetStitch = True

			if resetStitch and (stid >= 0):
				stitch_timestamps[baseline][polpair][stid] = seconds
				stitch_chcounts[baseline][polpair][stid] = 0
				stitch_freqbws[baseline][polpair][stid] = []
				stitch_workbufs[baseline][polpair][stid].fill(0)

		# Next header
		(vishdr,binhdr) = getVisibilityHeader(difxfile)

	## Generate new .input

	# New FREQ table
	new_freqs = []
	for of in out_freqs.keys(): # dict into list
		new_freqs.append(out_freqs[of])

	# New DATASTREAM table
	new_datastreams = []
	for ds in datastreams:
		npol = 2
		newds = copy.deepcopy(ds)
		ds_specific_remaps = [freq_remaps[zfi] for zfi in ds.zoomfreqindex]

		# Retain all recorded freqs and bands
		# newds.recfreqindex = newds.recfreqindex
		# newds.recfreqpols = newds.recfreqpols
		# newds.recbandindex = newds.recbandindex
		# newds.recbandpol = newds.recbandpol

		# Retain all bandwidth-matching zoom freqs
		newds.zoomfreqindex = [freq_remaps[zfi] for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0 and not freq_remaps_isNew[zfi])]
		newds.zoomfreqpols = [ds.zoomfreqpols[zfi] for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0 and not freq_remaps_isNew[zfi])]

		# Add all stitched invented freqs
		newds.zoomfreqindex += [nzfi for nzfi in stitch_out_ids if nzfi in ds_specific_remaps]
		newds.zoomfreqpols += [npol for nzfi in stitch_out_ids if nzfi in ds_specific_remaps]

		# Translate freqs into bands
		newds.nrecband = npol * len(newds.recfreqindex)
		newds.nzoomband = npol * len(newds.zoomfreqindex)
		newds.zoombandindex = [int(n/npol) for n in range(npol*newds.nzoomband)]
		newds.zoombandpol = ds.zoombandpol[:npol] * newds.nzoomband

		# Update the counts
		newds.nzoomfreq = len(newds.zoomfreqindex)
		new_datastreams.append(newds)

	# New BASELINE table
	new_baselines = []
	remapped_freqs = [n for n in range(len(freq_remaps)) if freq_remaps[n]>=0]
	for b in baselines:
		newbl = copy.deepcopy(b)
		nds1 = new_datastreams[newbl.dsaindex]
		nds2 = new_datastreams[newbl.dsbindex]
		nds1_allfreqs = nds1.recfreqindex + nds1.zoomfreqindex
		nds2_allfreqs = nds2.recfreqindex + nds2.zoomfreqindex
		common_freqs = list(set(nds1_allfreqs) & set(nds2_allfreqs)) # keeps only unique elements
		newbl.nfreq = len(common_freqs)
		newbl.freqpols = [cfg['stitch_nstokes']] * newbl.nfreq
		newbl.dsabandindex = []
		newbl.dsbbandindex = []
		for f in common_freqs:
			npol = 2
			assert(f in nds1_allfreqs)
			assert(f in nds2_allfreqs)
			i1 = nds1_allfreqs.index(f)
			i2 = nds2_allfreqs.index(f)
			if cfg['stitch_nstokes']==4:
				newbl.dsabandindex.append([npol*i1+0,npol*i1+0,npol*i1+1,npol*i1+1])
				newbl.dsbbandindex.append([npol*i2+0,npol*i2+1,npol*i2+0,npol*i2+1])
			elif cfg['stitch_nstokes']==2:
				newbl.dsabandindex.append([npol*i1+0,npol*i1+1])
				newbl.dsbbandindex.append([npol*i2+0,npol*i2+1])
			else:
				print("Warning: unexpected nstokes of %d" % (cfg['stitch_nstokes']))
				newbl.dsabandindex.append([i1])
				newbl.dsbbandindex.append([i2])
		new_baselines.append(newbl)

	# Read original .input without parsing
	fin = open(inputfile,"r")
	in_lines = fin.readlines()
	fin.close()

	# Generate new reference .input for later, re-use parts of original .input
	outputinputfile = basename + 'D2D.input'
	fout = open(outputinputfile,"w");
	idx = [i for i,elem in enumerate(in_lines) if "FREQ TABLE" in elem]
	for n in range(idx[0]):
		fout.write(in_lines[n])
        parseDiFX.put_freqtable_info(fout, new_freqs)
	idx1 = [i for i,elem in enumerate(in_lines) if "TELESCOPE TABLE" in elem]
	idx2 = [i for i,elem in enumerate(in_lines) if "DATASTREAM TABLE" in elem]
	for n in range(idx1[0],idx2[0]):
		fout.write(in_lines[n])
	parseDiFX.put_datastreamtable_info(fout, new_datastreams)
        parseDiFX.put_baselinetable_info(fout, new_baselines)
	idx = [i for i,elem in enumerate(in_lines) if "DATA TABLE" in elem]
	for trailing in in_lines[idx[0]:]:
		fout.write(trailing)
	fout.close()

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. copied        : %d' % (ncopied))
	print ('    vis. skipped       : %d' % (nskipped))
	print ('    vis. stitched      : %d' % (nstitched))
	print ('    incomplete stitch  : %d' % (nincomplete))
	print ('\nOutput files:')
	print ('    visbility data     : %s' % (difxoutname))
	print ('    changes for .input : %s' % (outputinputfile))
	print (' ')

if len(sys.argv) < 3:
	print __doc__
	sys.exit(-1)

cfg = getConfig(sys.argv[1])
for difxf in sys.argv[2:]:
	stitchVisibilityfile(difxf,cfg)

