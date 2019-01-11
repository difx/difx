#!/usr/bin/python
"""
Usage: difx2difx.py [-m|--meta-only] <difx2difx.cfg> <difx basename>

Stitches together in frequency domain the visibility data on the
baselines of a antenna, attempting to piece together a full band
out of shorter subbands. In other words assembles immediately
adjecent DiFX zoombands into wider bands as listed in the config file.

Config file example:
  [config]
  target_bw: 32.000                        total bandwidth to produce per band
  target_nchan: 4096                       number of points across target bandwidth
  extra_chavg: 1                           further spectral averaging before output
  stitch_antennas: AA, PV
  stitch_basefreqs: 86476.00, 86412.00
  verbose: false

Options:
  --meta-only   write only new .input file, do not generate visibility data

Output:
  <difx basename>/DIFX_*.stitched     frequency stitched visibility data
  <difx basename>/DIFX_*.ref_input    entries to use in a replacement .input file
"""
import glob, sys, os, shutil, struct, time, math, numpy, copy, ConfigParser
import hashlib
import parseDiFX
import fileinput
import shutil

vis_hashtable = {}
vis_hashtable_cleanupSec = 0

def setCrossProd(a,b):
	"""Return the cross product of two sets"""
	gen = ((x, y) for x in a for y in b)
	strconcatd = [ str(x)+str(y) for x,y in gen]
	return (strconcatd,gen)


def tupleCrossProd(a,b):
	"""Returns the cross product of two sets, returned as a list of tuples"""
	gen = ((x, y) for x in a for y in b)
	res = [(x,y) for x,y in gen]
	return res


def getGlueIndex(f,cfg):
	"""Check if a frequency falls into one band in a set of frequency bands"""
	N = len(cfg['stitch_basefreqs'])
	for n in range(N):
		if (f >= cfg['stitch_basefreqs'][n]) and (f < cfg['stitch_endfreqs'][n]):
			return n
	return -1


def getVisibilityHeader(f):
	"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)


def spectralAvgRaw(rawvis, chavg, doComplex=True):
	"""Spectral averaging of binary data reinterpreted as 32-bit real or complex float"""
	if doComplex:
		visdata = numpy.fromstring(rawvis, dtype='complex64')
	else:
		visdata = numpy.fromstring(rawvis, dtype='float32')
	N = len(visdata)
	assert((N % chavg)==0)
	visdata = numpy.average(visdata.reshape((N/chavg,chavg)), axis=1)
	#visdata = numpy.sum(visdata.reshape((N/chavg,chavg)), axis=1)
	rawout = visdata.tostring()
	assert(len(rawout)==len(rawvis)/chavg)
	return rawout


def spectralAvgNumpy(visdata, chavg):
	"""Spectral averaging of a numpy array"""
	N = len(visdata)
	assert((N % chavg)==0)
	visdata = numpy.average(visdata.reshape((N/chavg,chavg)), axis=1)
	#visdata = numpy.sum(visdata.reshape((N/chavg,chavg)), axis=1)
	return visdata


def getConfig(cfgfilename):
	"""Read a difx2difx configuration file."""

	cfg = {}

	cfgparser = ConfigParser.ConfigParser()
	cfgparser.read(cfgfilename)
	section = (cfgparser.sections())[0]

	# Catch configs from too old versions
	try:
		tmp = cfgparser.getint(section, 'target_chavg')
		print("Error: obsolete config file keyword: please replace 'target_chavg' with 'extra_chavg', adjust for its changed meaning!")
		return None
	except:
		pass

	# User settings
	cfg['target_bw'] = cfgparser.getfloat(section, 'target_bw')
	cfg['target_nchan'] = cfgparser.getint(section, 'target_nchan')
	cfg['stitch_antennas'] = []
	salist = cfgparser.get(section, 'stitch_antennas')
	for sa in salist.split(', '):
		cfg['stitch_antennas'].append(sa)
	cfg['stitch_nstokes'] = int(cfgparser.get(section, 'stitch_nstokes'))

	# Optional settings
	try:
		cfg['extra_chavg'] = cfgparser.getint(section, 'extra_chavg')
	except:
		cfg['extra_chavg'] = 1
	try:
		cfg['verbose'] = cfgparser.getboolean(section, 'verbose')
	except:
		cfg['verbose'] = False

	# User frequencies
	fqlist = cfgparser.get(section, 'stitch_basefreqs')
	cfg['stitch_basefreqs'] = []
	cfg['stitch_endfreqs'] = []
	for fq in fqlist.split(', '):
		f = float(fq)
		cfg['stitch_basefreqs'].append(f)
		cfg['stitch_endfreqs'].append(f + cfg['target_bw']) # USB assumed

	return cfg


def findFreqObj(dict,freqobj):
	"""Look through dictionary for object, return key of object if it exists"""
	for k in dict.keys():
		if (dict[k] == freqobj):
			return k
	return None


def inventNextKey(dict):
	"""Invent next unique (numerical) key for dictionary"""
	idNr = len(dict)
	while idNr in dict.keys():
		idNr += 1
	return idNr


def wasVisAlreadyWritten(mjd,seconds,baseline,freqnr,polpair):
	"""
	Keep track of visibilities already written, via looking up Time Baseline Freq Pol in a hash table
	Returns True if hash table already contains an entry for this visibility,
	otherwise returns False and inserts a bookkeeping entry for this visibility.
	"""
	global vis_hashtable
	global vis_hashtable_cleanupSec

	longsecs = seconds + (mjd-57846)*31622400

	# Keep the hashtable from growing too much; keeping track for the latest 10 seconds is probably enough
	history_secs = 5
	if (longsecs - vis_hashtable_cleanupSec) > (2*history_secs):
		vis_hashtable_cleanupSec = longsecs - history_secs
		curr_keys = vis_hashtable.keys()
		nremoved = 0
		for key in curr_keys:
			if not (key in vis_hashtable):
				continue
			if vis_hashtable[key] < vis_hashtable_cleanupSec:
				del vis_hashtable[key]
				nremoved += 1
		#print ('wasVisAlreadyWritten() : removed %d entries older than %d seconds' % (nremoved,history_secs))

	# Generate hash for this visibility
	m = hashlib.sha256()
	m.update(str(mjd))
	m.update(str(seconds))
	m.update(str(baseline * 8563))
	m.update(str(freqnr * 12503))
	m.update(str(polpair))
	key = m.hexdigest()

	# Check if visibility exists
	if key in vis_hashtable:
		#print ('wasVisAlreadyWritten(%d, %d, %d, %d, %s) : visibility exists' % (mjd,seconds,baseline,freqnr,polpair))
		return True
	else:
		#print ('wasVisAlreadyWritten(%d, %d, %d, %d, %s) : visibility is NEW, hash %s' % (mjd,seconds,baseline,freqnr,polpair,key))
		vis_hashtable[key] = longsecs
		return False


def getPolsForFreq(ds,fqId):
	"""Look up frequency ID in datastream recorded and zoom frequencies, return 'recbandpols' for that frequency"""
	pols = []
	npol_expected = 0
	if fqId in ds.recfreqindex:
		i = ds.recfreqindex.index(fqId)
		npol_expected = ds.recfreqpols[i]
		nsubbands = len(ds.recbandindex)
		pols = [ds.recbandpol[j] for j in range(nsubbands) if ds.recbandindex[j] == i]
	if fqId in ds.zoomfreqindex:
		i = ds.zoomfreqindex.index(fqId)
		npol_expected = ds.zoomfreqpols[i]
		nsubbands = len(ds.zoombandindex)
		pols = [ds.zoombandpol[j] for j in range(nsubbands) if ds.zoombandindex[j] == i]
	assert(npol_expected == len(pols))
	return pols


def getBandIndexOfFreqPol(ds,fqId,pol):
	"""Look up the 'band index' in datastream where the given frequency and polarization are found"""
	bandindex = -1
	if fqId in ds.recfreqindex:
		i = ds.recfreqindex.index(fqId)
		nsubbands = len(ds.recbandindex)
		bandpolpairs = [(ds.recbandpol[j],j) for j in range(nsubbands) if ds.recbandindex[j] == i]
		for (polzn,band) in bandpolpairs:
			if polzn == pol: bandindex = band
	if fqId in ds.zoomfreqindex:
		i = ds.zoomfreqindex.index(fqId)
		nsubbands = len(ds.zoombandindex)
		bandpolpairs = [(ds.zoombandpol[j],j) for j in range(nsubbands) if ds.zoombandindex[j] == i]
		for (polzn,band) in bandpolpairs:
			if polzn == pol: bandindex = band + ds.nrecband
	return bandindex


def getFreqPolOfBand(ds,band):
	"""Look up the frequency and polarization of a given band"""
	pol = ''
	fqId = -1
	if band < len(ds.recbandindex):
		recfreq = ds.recbandindex[band]
		fqId = ds.recfreqindex[recfreq]
		pol = ds.recbandpol[band]
	else:
		band = band - len(ds.recbandindex)
		recfreq = ds.zoombandindex[band]
		fqId = ds.zoomfreqindex[recfreq]
		pol = ds.zoombandpol[band]
	return (fqId,pol)


def stitchVisibilityfile(basename,cfg,writeMetaOnly=False):
	"""Copy visibilities from given base DiFX fileset into new file, while doing frequency stitching during the process"""

	# Get settings
	target_bw = cfg['target_bw']
	target_nchan = cfg['target_nchan']
	stitch_basefreqs = cfg['stitch_basefreqs']
	stitch_endfreqs = cfg['stitch_endfreqs']
	stitch_antennas = cfg['stitch_antennas']
	blank_viz = numpy.fromstring('\0'*8*target_nchan, dtype='complex64')

	# All polarisation pairs 'RR', 'RL', ... to 'YL'
	if cfg['stitch_nstokes'] > 2:
		pols_list = ['R','L','X','Y']
		polpairs,tmp = setCrossProd(pols_list, pols_list)
	else:
		polpairs = ['RR','LL','XX','YY']   # parallel-hand only
		polpairs += ['RX', 'LY','XR','YL'] # potential parallel-hand
		polpairs += ['LX', 'RY','XL','YR'] # potential parallel-hand (depends on polconvert.py)

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','input')):
		basename = basename[:basename.rfind('.')]
	basename_pathless = basename
	if basename.rfind('/') >= 0:
		basename_pathless = basename[(basename.rfind('/')+1):]
	inputfile = basename + '.input'
	inputfile_cfg = parseDiFX.get_common_settings(inputfile)
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if 'difxfile' not in inputfile_cfg:
		parser.error("Couldn't parse COMMON SETTINGS of input file " + inputfile + " correctly")
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
			i = findFreqObj(out_freqs,freqs[fqidx])
			if (i == None):
				id = inventNextKey(out_freqs)
				freq_remaps[fqidx] = id
				out_freqs[id] = copy.deepcopy(freqs[fqidx])
				in_rec_freqs += 1
				print ("Keeping recorded frequency  : index %2d/%2d : %s" % (fqidx,id,freqs[fqidx].str()))
			else:
				if (freq_remaps[fqidx] < 0):
					existing_fq = freq_remaps.index(id)
					print ('Warning: DiFX produced duplicate FREQ entries! For telescope %d freq %d now using first identical freq %d (%s)' % (d.telescopeindex,fqidx,existing_fq,out_freqs[i].str().strip()))
					freq_remaps[fqidx] = id

	# Collect all zoom bands listed in DATASTREAMS that already match the target bandwidth
	for d in datastreams:
		for fqidx in d.zoomfreqindex:
			zf = freqs[fqidx]
			if (zf.bandwidth != target_bw) or ((zf.numchan/zf.specavg) != target_nchan):
				# print ("Skipping zoom frequency     : index %d : %s" % (fqidx,zf.str()))
				continue
			i = findFreqObj(out_freqs,zf)
			if (i == None):
				id = inventNextKey(out_freqs)
				freq_remaps[fqidx] = id
				out_freqs[id] = copy.deepcopy(zf)
				print ("Keeping zoom frequency      : index %2d/%2d : %s" % (fqidx,id,zf.str()))

	# Determine how much averaging post-FFT was done in DiFX itself
	common_difx_avgfactor = -1
	for key in out_freqs:
		common_difx_avgfactor = out_freqs[key].specavg
	if common_difx_avgfactor < 1:
		print("Warning: may have failed to detect original .input 'CHANS TO AVG' factor! Assuming factor 1!")
		common_difx_avgfactor = 1

	# Check stitch config: re-use existing USB zoom bands or freqs where possible, invent new zoom bands if necessary
	stitch_out_ids = []
	for fsky in stitch_basefreqs:
		match_existing = [(out_freqs[key].freq==fsky 
				and out_freqs[key].bandwidth == target_bw
				and out_freqs[key].numchan/out_freqs[key].specavg == target_nchan
				and not out_freqs[key].lsb)
			for key in out_freqs.keys()]
		exists = any(match_existing)
		if exists:
			# Re-use an existing matching zoom frequency
			i = match_existing.index(True)
			id = out_freqs.keys()[i]
			zf = out_freqs[id]
			zf.numchan = target_nchan * common_difx_avgfactor
			zf.specavg = common_difx_avgfactor
			stitch_out_ids.append(id)
			old_id = freq_remaps.index(id)
			print ("Re-using zoom for stitch    : index %2d/%2d : %s" % (old_id,id,zf.str()))
		else:
			# Invent new zoom
			zf = parseDiFX.Freq()
			zf.freq = fsky
			zf.bandwidth = target_bw
			zf.numchan = target_nchan * common_difx_avgfactor
			zf.specavg = common_difx_avgfactor
			zf.lsb = False
			id = inventNextKey(out_freqs)
			out_freqs[id] = copy.deepcopy(zf)
			stitch_out_ids.append(id)
			print ("Creating new zoom frequency : index --/%2d : %s" % (id,zf.str()))

	stitch_ids = [n for n in range(len(stitch_out_ids))]

	# Also map each to-be-stitched-multi-zoom into respective new single post-stitch zoom
	for fi in range(in_rec_freqs,len(freqs)):
		if (freqs[fi].bandwidth == target_bw) and (freqs[fi].numchan/freqs[fi].specavg == target_nchan):
			# Retain one-to-one map for existing matching zoom freqs
			freq_remaps_isNew[fi] = False
			continue
		stid = getGlueIndex(freqs[fi].freq,cfg)
		if (stid >= 0):
			if freqs[fi].lsb:
				# Ignore LSB. All zoom outputs should be USB.
				print ("Ignore LSB %s" % (freqs[fi].str()))
				continue
			# Add to many-to-one map of invented zoom freqs
			freq_remaps[fi] = stitch_out_ids[stid]
			freq_remaps_isNew[fi] = True
			print ("Map zoom %s to stitched single %12.6f--%12.6f : in fq#%2d -> stitch#%d -> out fq#%2d" % (freqs[fi].str(), stitch_basefreqs[stid], stitch_endfreqs[stid],fi,stid,stitch_out_ids[stid]))

	# Propagate any averaging to be done by difx2difx.py into the frequency table
	if cfg['extra_chavg']>1:
		for of in out_freqs.keys():
			out_freqs[of].specavg = out_freqs[of].specavg * cfg['extra_chavg']

	# Read the DiFX .difx/DIFX_* file
	#glob_pattern = basename + '.difx/DIFX_*.s*.b*'
	glob_pattern = inputfile_cfg['difxfile'] + '/DIFX_*.s*.b*'
	difxfileslist = glob.glob(glob_pattern)
	if len(difxfileslist) <= 0:
		print ('Error: no visibility data file found in %s!' % (glob_pattern))
		return
	difxfilename = difxfileslist[0]
	difxfilename_pathless = difxfilename
	if difxfilename.rfind('/') >= 0:
		difxfilename_pathless = difxfilename[(difxfilename.rfind('/')+1):]
	difxfile = open(difxfilename, 'r')

	# Output DiFX file
	difxoutdir = basename_pathless + 'D2D.difx'
	difxoutname = difxoutdir + '/' + difxfilename_pathless
	if not writeMetaOnly:
		# New output directory and file
		try:
			os.mkdir(difxoutdir)
		except:
			pass
		if os.path.exists(difxoutname):
			print ('Warning: %s already exists, skipping the processing of %s!' % (difxoutname,difxfilename))
			return
		difxout = open(difxoutname, 'w')

		# Copy PCAL files if any
		for pcalfile in glob.glob(inputfile_cfg['difxfile'] + '/PCAL_*'):
			print ('Copying %s' % (pcalfile))
			shutil.copy(pcalfile, difxoutdir + '/')
	else:
		difxout = None

	# Pull out antenna indices based on telescope names
	telNames = [t.name for t in telescopes]
	stAntIDs = []
	for sa in stitch_antennas:
		id = [n for n in range(len(telescopes)) if telescopes[n].name==sa]
		if len(id) != 1:
			print ("Warning: got %d hits for telescope %s in .difx file telescope list of %s!" % (len(id),sa,str(telNames)))
			continue
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

	# Dictionary of per-polarizationpair working buffers into which spectra are stitched
	stitch_workbufs = { bline:{ polkey:{bandkey:numpy.copy(blank_viz) for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	stitch_chcounts = { bline:{ polkey:{bandkey:0 for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	stitch_freqbws = { bline:{ polkey:{bandkey:[] for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	stitch_timestamps = { bline:{ polkey:{bandkey:-1 for bandkey in stitch_ids } for polkey in polpairs } for bline in baseline_list }
	nstitched = 0; ncopied = 0; nskipped = 0; nincomplete = 0

	# Parse each visibility entry
	seconds_prev = -1
	seconds_first = 0
	seconds_report_prev = 0
	while not writeMetaOnly:

		(vishdr,binhdr) = getVisibilityHeader(difxfile)
		if len(vishdr) <= 0:
			break

		# Visibility properties
		baseline = vishdr[0]
		mjd = vishdr[1]
		seconds = vishdr[2]
		freqindex = vishdr[5]
		polpair = vishdr[6]
		weight = vishdr[8]
		uvw = vishdr[9:12]
                # Antenna order as in difx2mark4: ref=ant1="256*nr", rem=ant2="nr%256"
		ant2 = baseline % 256
		ant1 = (baseline-ant2)/256
		ant1name = telescopes[ant1-1].name
		ant2name = telescopes[ant2-1].name
		T = mjd + seconds/86400.0
		baselinestr = '%s-%s' % (ant1name,ant2name)					

		# Stokes check:
		if polpair not in polpairs:
			print('Error: unexpected pol pair %s, perhaps increase stitch_nstokes (currently %d) in config file?' % (polpair,cfg['stitch_nstokes']))
			sys.exit(-1)

		# Number of channels in this baseband
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg
		fsky = freqs[freqindex].freq
		bw = freqs[freqindex].bandwidth

		# Read the entire visibility data from disk
		rawvis = difxfile.read(8*nchan)
		if len(rawvis) != 8*nchan:
			print ('Short read! Stopping.')
			break

		if (seconds != seconds_prev):
			seconds_prev = seconds
			if cfg['verbose']:
				print (('\n---- %d %12.7f ' + '-'*115) % (mjd,seconds))
			elif (seconds - seconds_report_prev)>1.0:
				if seconds_first <= 0:
					seconds_first = mjd*24*60*60 + seconds
				dTstart = mjd*24*60*60 + seconds - seconds_first
				seconds_report_prev = seconds
				print ("at %d %12.7f, %.3f seconds from start" % (mjd,seconds,dTstart))
				print ("\033[F\033[F")

		# Remap the frequency reference
		out_freqindex = freqindex
		if freq_remaps[freqindex] >= 0:
			# print ('Remapping input freq index %d to old/new output freq %d' % (freqindex,freq_remaps[freqindex]))
			out_freqindex = freq_remaps[freqindex]
			vishdr[5] = out_freqindex
			binhdr = parseDiFX.make_output_header_v1(vishdr)

		# Destination stitch band index
		if not freqs[freqindex].lsb:
			stid = getGlueIndex(fsky,cfg)
		else:
			stid = -1

		# Info string
		vis_info = '%s-%s/%d/%d(%d):sf<%d>:%.7f/%s  mjd:%12.8f nchan:%4d bw:%.7f uvw=%s' % (ant1name,ant2name,baseline,out_freqindex,freqindex,stid,fsky,polpair,T,nchan,bw,str(uvw))

		# Write out visibility to output file
		if baseline not in baseline_list:

			# Baseline for which no freq stitching was requested

			# Keep data if bandwidth matches
			if (bw == target_bw) and (nchan == target_nchan):

				if wasVisAlreadyWritten(mjd,seconds,baseline,out_freqindex,polpair):
					if cfg['verbose']:
						print ('(copy): %s' % (vis_info))
					continue
				if cfg['verbose']:
					print ('copy  : %s' % (vis_info))

				assert(freq_remaps[freqindex] >= 0)
				difxout.write(binhdr)
				if cfg['extra_chavg'] > 1:
					rawvis = spectralAvgRaw(rawvis, cfg['extra_chavg'])
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

			if (bw == target_bw) and freqs[freqindex].lsb:

				# Visibility already at target bandwidth, but LSB i.e. not the expected zoom (all-USB)
				# As the full-bw zoom ought to exist or is going to be stitched together, we skip the LSB x LSB
				# visibility here. This avoids duplicates (since e.g. 86188 LSB x LSB rec 32 MHz == 86156 USB x USB zoom 32 MHz)

				nskipped += 1
				if cfg['verbose']:
					print ('skip L: %s' % (vis_info))

			elif (bw == target_bw):

				# Visibility already at target bandwidth, just copy the data

				if wasVisAlreadyWritten(mjd,seconds,baseline,out_freqindex,polpair):
					if cfg['verbose']:
						print ('(take): %s' % (vis_info))
					continue
				if cfg['verbose']:
					print ('take  : %s' % (vis_info))

				assert(freq_remaps[freqindex] >= 0)
				difxout.write(binhdr)
				if cfg['extra_chavg'] > 1:
					rawvis = spectralAvgRaw(rawvis, cfg['extra_chavg'])
				difxout.write(rawvis)
				ncopied += 1

				# Signal for further below: clear old working data, set new timestamp
				resetStitch = True

			else:

				# Need to stitch narrowband visibility into wideband context

				# Ignore bands not requested to be stitched
				if (stid < 0):
					if cfg['verbose']:
						print ('ignore: %s (not in stitch freqs list)' % (vis_info))
					continue

				# Get the complex data
				visdata = numpy.fromstring(rawvis, dtype='complex64')

				# Determine the region into which this visibility data would be inserted
				choffset = int( ((fsky - stitch_basefreqs[stid]) / target_bw) * target_nchan )
				if (choffset+nchan)>target_nchan:
					# Sometimes a wide recorded band falls into the stitching frequency region,
					# but then may not fit fully into the stitching freq region.
					# Options are:
					# 1) discard and hope the corresponding zoom band data will appear soon
					#if cfg['verbose']:
					#	print ('ignore: %s (too wide, maybe a recorded instead of zoom freq)' % (vis_info))
					#continue
					# 2) use the maximum possible amount of the data
					nchan_new = target_nchan - choffset
					if (nchan_new < 1):
						print ('Unexpected, after cropping a too-wide band ended up with %d remaining channels' % (nchan_new))
						continue
					visdata = visdata[:nchan_new]
					bw = bw * nchan_new/nchan
					nchan = nchan_new
					if cfg['verbose']:
						print('crop  : %s to %d chan' % (vis_info,nchan_new))

				# Keep track of timestamp
				if (stitch_timestamps[baseline][polpair][stid] < 0):
					stitch_timestamps[baseline][polpair][stid] = seconds

				# Detect a jump in timestamp
				if (seconds != stitch_timestamps[baseline][polpair][stid]):
					# We assume the input visibilities are stored in time-increasing order,
					# so a timestamp change indicates all data of previous time should've been fully read by now (no older data follows)
					ncurr = stitch_chcounts[baseline][polpair][stid]
					if (ncurr > 0):
						T_old = mjd + stitch_timestamps[baseline][polpair][stid] / 86400.0
						nincomplete += 1
						msg = '%s-%s/%d/%d:%.7f/%s mjd:%12.8f only %d of %d channels' % (ant1name,ant2name,baseline,stid,stitch_basefreqs[stid],polpair,T_old,ncurr,target_nchan)
						if cfg['verbose']:
							print ('Warning: discarded an incomplete stitch: %s' % (msg))
					assert(ncurr <= target_nchan)
					stitch_timestamps[baseline][polpair][stid] = seconds
					stitch_chcounts[baseline][polpair][stid] = 0
					stitch_freqbws[baseline][polpair][stid] = []
					stitch_workbufs[baseline][polpair][stid].fill(0)
					#print (('---- %d %.7f ' + '-'*80 + '\n') % (mjd,seconds))
	
				# Insert the new visibility data into the correct working buffer at the correct location
				stitch_workbufs[baseline][polpair][stid][choffset:(choffset+nchan)] = visdata
				stitch_chcounts[baseline][polpair][stid] += nchan
				stitch_freqbws[baseline][polpair][stid].append(bw)

				#print (' glue bline %s %s band %d : ch %4d ... %4d : %d new chs : stitched %d ch total' % (baselinestr,polpair,stid,choffset,choffset+nchan-1,nchan,stitch_chcounts[baseline][polpair][stid]))
				#print choffset, nchan, choffset+nchan, stitch_chcounts[baseline][polpair][stid], (100.0*stitch_chcounts[baseline][polpair][stid])/target_nchan

				# Assembled the full bandwidth now?
				if stitch_chcounts[baseline][polpair][stid] >= target_nchan:
					TS = mjd + stitch_timestamps[baseline][polpair][stid] / 86400.0
					bwsum = numpy.sum(stitch_freqbws[baseline][polpair][stid])
					vis_info = '%s-%s/%d/%d(%d):sf<%d>:%.7f/%s  mjd:%12.8f nchan:%4d bw:%.7f uvw=%s' % (ant1name,ant2name,baseline,out_freqindex,freqindex,stid,fsky,polpair,TS,stitch_chcounts[baseline][polpair][stid],bwsum,str(uvw))
	
					if wasVisAlreadyWritten(mjd,seconds,baseline,out_freqindex,polpair):
						if cfg['verbose']:
							print ('(stch dup): %s' % (vis_info))
						continue
					if cfg['verbose']:
						print ('stitch: %s' % (vis_info))

					# Double-check the bandwidth
					if numpy.abs(target_bw - bwsum) > 100:
						print ('Warning: stitched bands gave %.6f MHz bandwidth, differs from target %.6f MHz!' % (bwsum,target_bw))

					# Write header and assembled data
					difxout.write(binhdr)
					if cfg['extra_chavg'] > 1:
						vis = spectralAvgNumpy(stitch_workbufs[baseline][polpair][stid], cfg['extra_chavg'])
					else:
						vis = stitch_workbufs[baseline][polpair][stid]
					vis.tofile(difxout)
					nstitched += 1

					# Reset
					resetStitch = True

			if resetStitch and (stid >= 0):
				stitch_timestamps[baseline][polpair][stid] = seconds
				stitch_chcounts[baseline][polpair][stid] = 0
				stitch_freqbws[baseline][polpair][stid] = []
				stitch_workbufs[baseline][polpair][stid].fill(0)

		#if nstitched > 100: break  # quick-exit for debug

	## Generate new .input

	# New FREQ table
	new_freqs = copy.deepcopy(out_freqs)

	# New DATASTREAM table
	new_datastreams = []
	for ds in datastreams:

		# Copy the existing values, then update/replace
		newds = copy.deepcopy(ds)

		# Retain all recorded freqs and bands
		newds.recfreqindex = [freq_remaps[rfi] for rfi in newds.recfreqindex]
		newds.recfreqpols = [p for p in ds.recfreqpols]	
		newds.nrecfreq = len(newds.recfreqindex)
		if True:
			# Sort the bands to have L R L R L R L R rather than L L L L R R R R,
			# makes baseline table re-creation easier
			newds.recbandpol = [x for _, x in sorted(zip(newds.recbandindex,newds.recbandpol), key=lambda pair: pair[0])]
			newds.recbandindex.sort()
		assert(len(newds.recfreqindex) == len(newds.recfreqpols))
		assert(newds.nrecband == sum(newds.recfreqpols))

		# Retain bandwidth-matching existing zoom freqs & bands
		# and also add all stitched/invented zoom freqs & bands
		newds.zoomfreqindex, newds.zoomfreqpols, newds.zoombandindex, newds.zoombandpol = [], [], [], []
		#ds_old_zooms = [zfi for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0 and not freq_remaps_isNew[zfi])]
		#ds_new_zooms = [zfi for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0 and freq_remaps_isNew[zfi])]
		all_zooms = [zfi for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0)]
		for zf in all_zooms:
			if freq_remaps[zf] not in newds.zoomfreqindex:
				pols = getPolsForFreq(ds,zf) # from original DS & Freq
				if len(pols) < 1: continue
				newds.zoomfreqindex.append(freq_remaps[zf])
				newds.zoomfreqpols.append(len(pols))
				n = newds.zoomfreqindex.index(freq_remaps[zf])
				newds.zoombandindex += [n]*len(pols)
				newds.zoombandpol += pols
		newds.nzoomfreq = len(newds.zoomfreqpols)
		newds.nzoomband = sum(newds.zoomfreqpols)

		# Store
		new_datastreams.append(newds)

		if cfg['verbose']:
			print ("DS%d : rec freqs      : %s" % (datastreams.index(ds),str(newds.recfreqindex)))
			print ("      rec freq pols  : %s" % (str(newds.recfreqpols)))
			print ("      rec bands      : %s" % (str(newds.recbandindex)))
			print ("      rec band pols  : %s" % (str(newds.recbandpol)))
			print ("      zoom freqs     : %s" % (str(newds.zoomfreqindex)))
			print ("      zoom freq pols : %s" % (str(newds.zoomfreqpols)))
			print ("      zoom bands     : %s" % (str(newds.zoombandindex)))
			print ("      zoom band pols : %s" % (str(newds.zoombandpol)))
			print ("      counts         : %d rec fq, %d zoom fq, %d rec band, %d zoom band" % (newds.nrecfreq,newds.nzoomfreq,newds.nrecband,newds.nzoomband))

	# New BASELINE table
	full_freq_remaps = [freq_remaps[n] if freq_remaps[n]>=0 else n for n in range(len(freq_remaps))]
	#print ('Full remap table:',str(len(full_freq_remaps)), ' entries, ', str(full_freq_remaps))
	new_baselines = []
	for b in baselines:
		newbl = copy.deepcopy(b)
		ods1 = datastreams[b.dsaindex]
		ods2 = datastreams[b.dsbindex]
		nds1 = new_datastreams[newbl.dsaindex]
		nds2 = new_datastreams[newbl.dsbindex]
		if cfg['verbose']:
			print ("Baseline DS%d x DS%d" % (newbl.dsaindex, newbl.dsbindex))
			print ("     stream %d x stream %d" % (newbl.dsaindex, newbl.dsbindex))

		newbl.dsabandindex = []
		newbl.dsbbandindex = []
		newbl.freqpols = []
		copied_ids = []
		for i in range(len(b.dsabandindex)):
			# For each baseline "frequency" entry there are two lists of bands e.g. [0,1,2,3] x [0,2,1,3]
			# that were correlated against each other in DiFX.
			# Datastream and Freq were updated above, hence the band indexing has changed.
			# Re-map the referenced band numbers/indices in the lists from old to new indices.
			# Also, look up Freq of the respective band indices and skip narrow pre-stitch zooms from Baseline.
			bl_bands_A = b.dsabandindex[i]
			bl_bands_B = b.dsbbandindex[i]
			bl_new_bands_A, bl_new_bands_B = [], []
			for (bl_band_A,bl_band_B) in zip(bl_bands_A,bl_bands_B):
				oldFqA,polA = getFreqPolOfBand(ods1,bl_band_A)
				oldFqB,polB = getFreqPolOfBand(ods2,bl_band_B)
				if oldFqA < 0 or oldFqB < 0:
					print ("Warning: unexpectedly could not locate old input band information!")
					continue
				newFqA = full_freq_remaps[oldFqA]
				newFqB = full_freq_remaps[oldFqB]
				id = '%d_%d_%s%s' % (newFqA,newFqB,polA,polB)
				if id not in copied_ids:
					newBandA = getBandIndexOfFreqPol(nds1,newFqA,polA)
					newBandB = getBandIndexOfFreqPol(nds2,newFqB,polB)
					if newBandA < 0 or newBandB < 0:
						continue
					bl_new_bands_A.append(newBandA)
					bl_new_bands_B.append(newBandB)
					copied_ids.append(id)
					if cfg['verbose']:
						print ("     corr %2d : bands %2d x %2d %s%s : fq %2d x %2d --> new fq %2d x %2d : copied, %s" % (i,bl_band_A,bl_band_B,polA,polB,oldFqA,oldFqB,newFqA,newFqB,id))
				else:
					if cfg['verbose']:
						print ("     corr %2d : bands %2d x %2d %s%s : fq %2d x %2d --> new fq %2d x %2d : skip, sub-stitch" % (i,bl_band_A,bl_band_B,polA,polB,oldFqA,oldFqB,newFqA,newFqB))
			if len(bl_new_bands_A) > 0:
				newbl.dsabandindex.append(bl_new_bands_A)
				newbl.dsbbandindex.append(bl_new_bands_B)
				newbl.freqpols.append(len(bl_new_bands_A))

		newbl.nfreq = len(newbl.dsabandindex)
		new_baselines.append(newbl)
	print ('\n')

	# Read original .input without parsing
	fin = open(inputfile,"r")
	in_lines = fin.readlines()
	fin.close()

	# Replace OUTPUT FILENAME entry
	for l in in_lines:
		if l[:16]=="OUTPUT FILENAME:":
			i = in_lines.index(l)
			in_lines[i] = "%-20s%s\n" % ("OUTPUT FILENAME:",difxoutdir)

	# Replace CALC FILENAME and CORE CONF FILENAME and point them to a local, renamed copy of these files
	for l in in_lines:
		if l[:19]=='CORE CONF FILENAME:':
			i = in_lines.index(l)
			orig_threadfile = l[20:].strip() 
			in_lines[i] = "%-20s%s\n" % ("CORE CONF FILENAME:",basename_pathless+'D2D.threads')
			try:
				shutil.copyfile(orig_threadfile, basename_pathless+'D2D.threads')
			except:
				print ('Note: could not copy %s ' % (orig_threadfile))
		elif l[:14]=="CALC FILENAME:":
			i = in_lines.index(l)
			orig_calcfile = l[20:].strip() 
			in_lines[i] = "%-20s%s\n" % ("CALC FILENAME:",basename_pathless+'D2D.calc')
			try:
				shutil.copyfile(orig_calcfile, basename_pathless+'D2D.calc')
			except:
				print ('Note: could not copy %s ' % (orig_calcfile))

	# Generate new reference .input for later, re-use parts of original .input
	outputinputfile = basename_pathless + 'D2D.input'
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

	# Go through the local copy .calc file and point references therein to other local copies
	for line in fileinput.input(basename_pathless+'D2D.calc', inplace=True):
		# fileinput module: in this loop STDOUT is redirected by the module into the file
		if 'IM FILENAME' in line:
			orig_imfile = line[15:].strip()
			copied_imfile = basename_pathless+'D2D.im'
			shutil.copyfile(orig_imfile, copied_imfile)
			line = '%-20s%s' % ('IM FILENAME:',copied_imfile)
		elif 'FLAG FILENAME' in line:
			orig_flagfile = line[15:].strip()
			copied_flagfile = basename_pathless+'D2D.flag'
			shutil.copyfile(orig_flagfile, copied_flagfile)
			line = '%-20s%s' % ('FLAG FILENAME:',copied_flagfile)
		print (line)

	# Finished
	if not writeMetaOnly:
		print ('\nDone! Final statistics:')
		print ('    vis. copied        : %d' % (ncopied))
		print ('    vis. skipped       : %d' % (nskipped))
		print ('    vis. stitched      : %d' % (nstitched))
		print ('    incomplete stitch  : %d' % (nincomplete))
		print ('\nOutput files:')
		print ('    visbility data     : %s' % (difxoutname))
		print ('    changes for .input : %s' % (outputinputfile))
		print (' ')
	else:
		print('\nDone! Metadata-only mode.')
		print ('\nOutput files:')
		print ('    changes for .input : %s' % (outputinputfile))

if __name__ == "__main__":

	writeMetaOnly = False
	if '-m' in sys.argv:
		sys.argv.remove('-m')
		writeMetaOnly = True
 	if '--meta-only' in sys.argv:
		sys.argv.remove('--meta-only')
		writeMetaOnly = True

	if len(sys.argv) < 3:
		print __doc__
		sys.exit(-1)

	cfg = getConfig(sys.argv[1])
	if cfg == None:
		sys.exit(-1)

	for difxf in sys.argv[2:]:
		stitchVisibilityfile(difxf,cfg,writeMetaOnly)

