#!/usr/bin/python
"""
Usage: difx2difx.py <difx2difx.cfg> <difx basename>

Stitches together in frequency domain the visibility data on the
baselines of a antenna, attempting to piece together a full band
out of shorter subbands, discarding any overlapping oversampled
region.

Output:
  <difx basename>/DIFX_*.stitched     frequency stitched visibility data
  <difx basename>/DIFX_*.ref_input    entries to use in a replacement .input file

"""
import glob, sys, os, struct, time, math, numpy, copy, ConfigParser
import hashlib
import parseDiFX

"""Return the cross product of two sets"""
def setCrossProd(a,b):
	gen = ((x, y) for x in a for y in b)
	strconcatd = [ str(x)+str(y) for x,y in gen]
	return (strconcatd,gen)

"""Check if (part of) a frequency falls into one band in a set of frequency bands"""
def getGlueIndex(f,bw,cfg):
	N = len(cfg['stitch_basefreqs'])
	for n in range(N):
		if (f+bw >= cfg['stitch_basefreqs'][n]) and (f < cfg['stitch_endfreqs'][n]):
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

"""Spectral averaging"""
def spectralAvgRaw(rawvis, chavg, doComplex=True):
	if doComplex:
		visdata = numpy.fromstring(rawvis, dtype='complex64')
	else:
		visdata = numpy.fromstring(rawvis, dtype='float32')
	N = len(visdata)
	assert((N % chavg)==0)
	visdata = numpy.average(visdata.reshape((N/chavg,chavg)), axis=1)
	rawout = visdata.tostring()
	assert(len(rawout)==len(rawvis)/chavg)
	return rawout

def spectralAvgNumpy(visdata, chavg):
	N = len(visdata)
	assert((N % chavg)==0)
	visdata = numpy.average(visdata.reshape((N/chavg,chavg)), axis=1)
	return visdata

"""
Read a difx2difx configuration file.
Example contents:
  [config]
  target_bw: 8.000
  target_nchan: 216
  target_chavg: 4
  stitch_oversamplenum: 32
  stitch_oversampledenom: 27
  stitch_nstokes: LL
  stitch_antennas: *
  stitch_basefreqs: 797.5
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

	# Optional settings
	try:
		cfg['target_chavg'] = cfgparser.getint(section, 'target_chavg')
	except:
		cfg['target_chavg'] = 1
	try:
		cfg['verbose'] = cfgparser.getboolean(section, 'verbose')
	except:
		cfg['verbose'] = False
        try:
                cfg['stitch_oversamplenum'] = cfgparser.getint(section, 'stitch_oversamplenum')
        except:
                cfg['stitch_oversamplenum'] = 1
        try:
                cfg['stitch_oversampledenom'] = cfgparser.getint(section, 'stitch_oversampledenom')
        except:
                cfg['stitch_oversampledenom'] = 1

	# User frequencies
	fqlist = cfgparser.get(section, 'stitch_basefreqs')
	cfg['stitch_basefreqs'] = []
	cfg['stitch_endfreqs'] = []
	for fq in fqlist.split(', '):
		f = float(fq)
		cfg['stitch_basefreqs'].append(f)
		cfg['stitch_endfreqs'].append(f + cfg['target_bw']) # USB assumed

	return cfg

"""Look through dictionary for object, return key of object if it exists"""
def findFreqObj(dict,freqobj):
	for k in dict.keys():
		if (dict[k] == freqobj):
			return k
	return None

"""Invent next unique (numerical) key for dictionary"""
def inventNextKey(dict):
	idNr = len(dict)
	while idNr in dict.keys():
		idNr += 1
	return idNr

"""Keep track of visibilities already written, via looking up Time Baseline Freq Pol in a hash table
   Returns True if hash table already contains an entry for this visibility
   Returns False otherwise and inserts an entry for this visibility
"""
vis_hashtable = {}
vis_hashtable_cleanupSec = 0
def vis_already_written(mjd,seconds,baseline,freqnr,polpair):
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
		#print ('vis_already_written() : removed %d entries older than %d seconds' % (nremoved,history_secs))

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
		#print ('vis_already_written(%d, %d, %d, %d, %s) : visibility exists' % (mjd,seconds,baseline,freqnr,polpair))
		return True
	else:
		#print ('vis_already_written(%d, %d, %d, %d, %s) : visibility is NEW, hash %s' % (mjd,seconds,baseline,freqnr,polpair,key))
		vis_hashtable[key] = longsecs
		return False


"""Copy visibilities from given base DiFX fileset into new file, while doing frequency stitching during the process"""
def stitchVisibilityfile(basename,cfg):

	# Get settings
	target_bw = cfg['target_bw']
	target_nchan = cfg['target_nchan']
	stitch_basefreqs = cfg['stitch_basefreqs']
	stitch_endfreqs = cfg['stitch_endfreqs']
	stitch_antennas = cfg['stitch_antennas']
        stitch_oversamplenum = cfg['stitch_oversamplenum']
        stitch_oversampledenom = cfg['stitch_oversampledenom']
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

        # Check stitch config: invent new zoom bands if necessary
        stitch_out_ids = []
        for fsky in stitch_basefreqs:
                match_existing = [(out_freqs[key].freq==fsky and not out_freqs[key].lsb) for key in out_freqs.keys()]
                exists = any(match_existing)
                if exists:
                        # Re-use an existing matching zoom frequency
                        i = match_existing.index(True)
                        id = out_freqs.keys()[i]
                        zf = out_freqs[id]
                        stitch_out_ids.append(id)
                        old_id = freq_remaps.index(id)
                        print ("Re-using zoom for stitch    : index %2d/%2d : %s" % (old_id,id,zf.str()))
                else:
                        # Invent new zoom
                        zf = parseDiFX.Freq()
                        zf.freq = fsky
                        zf.bandwidth = target_bw
                        zf.numchan = target_nchan
                        zf.specavg = 1
                        zf.lsb = False # TODO
                        id = inventNextKey(out_freqs)
                        out_freqs[id] = copy.deepcopy(zf)
                        stitch_out_ids.append(id)
                        print ("Creating new zoom frequency : index --/%2d : %s" % (id,zf.str()))

        stitch_ids = [n for n in range(len(stitch_out_ids))]

        # Also map each to-be-stitched-multi-zoom into respective new single post-stitch zoom
        #for fi in range(in_rec_freqs,len(freqs)):
        for fi in range(0,len(freqs)):
                if (freqs[fi].bandwidth == target_bw) and (freqs[fi].numchan/freqs[fi].specavg == target_nchan):
                        # Retain one-to-one map for existing matching zoom freqs
                        freq_remaps_isNew[fi] = False
                        continue
                lowedgefreq = freqs[fi].freq
                if freqs[fi].lsb:
                        lowedgefreq -= freqs[fi].bandwidth
                stid = getGlueIndex(lowedgefreq,freqs[fi].bandwidth,cfg)
                #print "For freqindex", fi, ", stid is", stid, ", and stitch_out_ids for this freqid is", stitch_out_ids[stid]
                if (stid >= 0):
                        #if freqs[fi].lsb:
                        #        # Ignore LSB. All zoom outputs should be USB.
                        #        print ("Ignore LSB %s" % (freqs[fi].str()))
                        #        continue
                        # Add to some many-to-one map of invented zoom freqs
                        freq_remaps[fi] = stitch_out_ids[stid]
                        freq_remaps_isNew[fi] = True
                        print ("Map zoom %s to stitched single %12.6f--%12.6f : in fq#%2d -> stitch#%d -> out fq#%2d" % (freqs[fi].str(), stitch_basefreqs[stid], stitch_endfreqs[stid],fi,stid,stitch_out_ids[stid]))

        # Read the DiFX .difx/DIFX_* file
        #glob_pattern = basename + '.difx/DIFX_*.s*.b*'
        glob_pattern = inputfile_cfg['difxfile'] + '/DIFX_*.s*.b*'
        difxfileslist = glob.glob(glob_pattern)
        if len(difxfileslist) <= 0:
                print ('Error: no visibility data file found in %s!' % (glob_pattern))
                return
        #difxfilename = difxfileslist[0]
        for difxfilename in difxfileslist:
                global vis_hashtable
                vis_hashtable = {}
                global vis_hashtable_cleanupSec
                vis_hashtable_cleanupSec = 0

                difxfilename_pathless = difxfilename
                if difxfilename.rfind('/') >= 0:
                        difxfilename_pathless = difxfilename[(difxfilename.rfind('/')+1):]
                difxfile = open(difxfilename, 'r')
                difxoutdir = basename_pathless + 'D2D.difx'
                difxoutname = difxoutdir + '/' + difxfilename_pathless
                try:
                        os.mkdir(difxoutdir)
                except:
                        pass
                if os.path.exists(difxoutname):
                        print ('Warning: %s already exists, skipping the processing of %s!' % (difxoutname,difxfilename))
                        return
                difxout = open(difxoutname, 'w')

                # Pull out antenna indices based on telescope names
                telNames = [t.name for t in telescopes]
                stAntIDs = []
                if len(stitch_antennas) == 1 and stitch_antennas[0] == "*":
                        for i in range(len(telescopes)):
                                stAntIDs.append(i+1)
                else:
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
                while True:

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

                        # Number of channels in this baseband
                        nchan = freqs[freqindex].numchan / freqs[freqindex].specavg
                        fsky = freqs[freqindex].freq
                        bw = freqs[freqindex].bandwidth
                        if freqs[freqindex].lsb:
                                fsky -= bw

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
                        stid = getGlueIndex(fsky,bw,cfg)

                        # Info string
                        vis_info = '%s-%s/%d/%d(%d):sf<%d>:%.7f/%s  mjd:%12.8f nchan:%4d bw:%.7f uvw=%s' % (ant1name,ant2name,baseline,out_freqindex,freqindex,stid,fsky,polpair,T,nchan,bw,str(uvw))

                        # Write out visibility to output file
                        if baseline not in baseline_list and not stitch_antennas == "*":

                                # Baseline for which no freq stitching was requested

                                # Keep data if bandwidth matches
                                if (bw == target_bw) and (nchan == target_nchan):

                                        if vis_already_written(mjd,seconds,baseline,out_freqindex,polpair):
                                                if cfg['verbose']:
                                                        print ('(copy): %s' % (vis_info))
                                                continue
                                        if cfg['verbose']:
                                                print ('copy  : %s' % (vis_info))

                                        assert(freq_remaps[freqindex] >= 0)
                                        difxout.write(binhdr)
                                        if cfg['target_chavg'] > 1:
                                                rawvis = spectralAvgRaw(rawvis, cfg['target_chavg'])
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

                                        if vis_already_written(mjd,seconds,baseline,out_freqindex,polpair):
                                                if cfg['verbose']:
                                                        print ('(take): %s' % (vis_info))
                                                continue
                                        if cfg['verbose']:
                                                print ('take  : %s' % (vis_info))

                                        assert(freq_remaps[freqindex] >= 0)
                                        difxout.write(binhdr)
                                        if cfg['target_chavg'] > 1:
                                                rawvis = spectralAvgRaw(rawvis, cfg['target_chavg'])
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

                                        # Do the oversampling cutout if needed
                                        if stitch_oversamplenum > 1:
                                                oversampleoffsetbw = (bw - bw*(float(stitch_oversampledenom)/float(stitch_oversamplenum))) / 2.0
                                                oversamplelengthchannels = int((float(stitch_oversampledenom)/float(stitch_oversamplenum))*nchan + 0.5)
                                                oversampleoffsetchannels = (nchan - oversamplelengthchannels)/2
                                                visdata = visdata[oversampleoffsetchannels:oversampleoffsetchannels + oversamplelengthchannels]
                                                fsky += oversampleoffsetbw
                                                bw -= 2*oversampleoffsetbw
                                                nchan = oversamplelengthchannels

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

                                        if choffset < 0:
                                                print "This shouldn't happen!"
                                                continue

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
                                                assert(ncurr < target_nchan)
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
	        
	        				if vis_already_written(mjd,seconds,baseline,out_freqindex,polpair):
	        					if cfg['verbose']:
	        						print ('(stch): %s' % (vis_info))
	        					continue
	        				if cfg['verbose']:
	        					print ('stitch: %s' % (vis_info))

	        				# Double-check the bandwidth
	        				if numpy.abs(target_bw - bwsum) > 100:
	        					print ('Warning: stitched bands gave %.6f MHz bandwidth, differs from target %.6f MHz!' % (bwsum,target_bw))

	        				# Write header and assembled data
	        				difxout.write(binhdr)
	        				if cfg['target_chavg'] > 1:
	        					vis = spectralAvgNumpy(stitch_workbufs[baseline][polpair][stid], cfg['target_chavg'])
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
	new_freqs = []
	for of in out_freqs.keys(): # dict into list
		out_freqs[of].specavg = out_freqs[of].specavg * cfg['target_chavg']
		new_freqs.append(out_freqs[of])

	# New DATASTREAM table
	new_datastreams = []
	for ds in datastreams:
		newds = copy.deepcopy(ds)
		ds_specific_remaps = [freq_remaps[zfi] for zfi in ds.zoomfreqindex]

		# Determine number of pols in this datastream (for example ALMA has two single-pol datastreams)
		npol = len(list(set(newds.recbandpol)))

		# Retain all recorded freqs and bands
		# newds.recfreqindex = newds.recfreqindex
		# newds.recfreqpols = newds.recfreqpols
		# newds.recbandindex = newds.recbandindex
		# newds.recbandpol = newds.recbandpol
		newds.recfreqindex = [freq_remaps[rfi] for rfi in newds.recfreqindex]

		# Retain all bandwidth-matching zoom freqs
		newds.zoomfreqindex = [freq_remaps[zfi] for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0 and not freq_remaps_isNew[zfi])]
		newds.zoomfreqpols = [ds.zoomfreqpols[ds.zoomfreqindex.index(zfi)] for zfi in ds.zoomfreqindex if (freq_remaps[zfi]>=0 and not freq_remaps_isNew[zfi])]

		# Add all stitched invented freqs
                # TODO: This is a quick hack: find ouy why the commented code wasn't working!
		#newds.zoomfreqindex += [nzfi for nzfi in stitch_out_ids if nzfi in ds_specific_remaps]
		#newds.zoomfreqpols += [npol for nzfi in stitch_out_ids if nzfi in ds_specific_remaps]
                newds.zoomfreqindex += [nzfi for nzfi in stitch_out_ids]
                newds.zoomfreqpols += [npol for nzfi in stitch_out_ids]

		newds.zoomfreqindex = list(set(newds.zoomfreqindex)) # keep uniques only, TODO: should shorten zoomfreqpols list equally!

		# Translate freqs into bands
		newds.nrecband = npol * len(newds.recfreqindex)
		newds.nzoomband = npol * len(newds.zoomfreqindex)
		newds.zoombandindex = [int(n/npol) for n in range(newds.nzoomband)]
                #TODO: The following is necessary if a datastream had no zoom bands to begin with, but I'm not sure it will always work
                if len(ds.zoombandpol) == 0:
                    ds.zoombandpol = newds.recbandpol[:npol]
		newds.zoombandpol = ds.zoombandpol[:npol] * (newds.nzoomband/npol)

		# Update the counts
		newds.nzoomfreq = len(newds.zoomfreqindex)
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

	# New BASELINE table
	new_baselines = []
	remapped_freqs = [n for n in range(len(freq_remaps)) if freq_remaps[n]>=0]
	for b in baselines:

		newbl = copy.deepcopy(b)

		nds1 = new_datastreams[newbl.dsaindex]
		nds2 = new_datastreams[newbl.dsbindex]
		npol1 = len(list(set(nds1.recbandpol)))
		npol2 = len(list(set(nds2.recbandpol)))
		max_stokes = npol1*npol2

		# Keep only freqs common to both datastreams, and that match the target bandwidth
		allfreqs1 = nds1.recfreqindex + nds1.zoomfreqindex
		allfreqs2 = nds2.recfreqindex + nds2.zoomfreqindex
		common_freqs = list(set(allfreqs1) & set(allfreqs2))
		common_freqs = [fnr for fnr in common_freqs if (out_freqs[fnr].bandwidth == target_bw)]

		newbl.nfreq = len(common_freqs)
		newbl.dsabandindex = []
		newbl.dsbbandindex = []
		newbl.freqpols = []

		if cfg['verbose']:
			print ("Baseline DS%d x DS%d" % (newbl.dsaindex, newbl.dsbindex))
			print ("     stream %d freqs %s" % (newbl.dsaindex, str(allfreqs1)))
			print ("     stream %d freqs %s" % (newbl.dsbindex, str(allfreqs2)))
			print ("     common freqs %s" % (str(common_freqs)))

		for f in common_freqs:
			npol = 2
			assert(f in allfreqs1)
			assert(f in allfreqs2)
			i1 = allfreqs1.index(f)
			i2 = allfreqs2.index(f)
			if max_stokes == 1:
				newbl.dsabandindex.append([npol1*i1])
				newbl.dsbbandindex.append([npol2*i2])
			elif max_stokes == 2:  # one of XL XR / YL YR / LL LR / RL RR
				if npol1 == 1:
					newbl.dsabandindex.append([npol1*i1,  npol1*i1  ])
					newbl.dsbbandindex.append([npol2*i2+0,npol2*i2+1])
				else:
					newbl.dsabandindex.append([npol1*i1+0,npol1*i1+1])
					newbl.dsbbandindex.append([npol2*i2,  npol2*i2  ])
			elif max_stokes == 4:
				if cfg['stitch_nstokes']==4:
					newbl.dsabandindex.append([npol1*i1+0,npol1*i1+0,npol1*i1+1,npol1*i1+1])
					newbl.dsbbandindex.append([npol2*i2+0,npol2*i2+1,npol2*i2+0,npol2*i2+1])
				elif cfg['stitch_nstokes']==2:
					# one XX YY / XL YR / LL RR
					newbl.dsabandindex.append([npol1*i1+0,npol1*i1+1])
					newbl.dsbbandindex.append([npol2*i2+0,npol2*i2+1])
				else:
					print("Warning: unexpected nstokes of %d" % (cfg['stitch_nstokes']))
					newbl.dsabandindex.append([npol1*i1])
					newbl.dsbbandindex.append([npol2*i2])

			newbl.freqpols.append(len(newbl.dsabandindex[-1]))

			assert( len(newbl.dsabandindex[-1]) == len(newbl.dsbbandindex[-1]) )
			if cfg['verbose']:
				print ("        now freq %d = (ds1 frq %d, ds2 frq %d)" % (f,i1,2))
				print ("            num pols = (%d, %d)" % (npol1,npol2))
				print ("            dsa[end] = %s" % (str(newbl.dsabandindex[-1])))
				print ("            dsb[end] = %s" % (str(newbl.dsbbandindex[-1])))
				print ("        has %d freqpols" % (newbl.freqpols[-1]))
				#print newbl.dsabandindex[-1], newbl.dsbbandindex[-1]

		new_baselines.append(newbl)

	# Read original .input without parsing
	fin = open(inputfile,"r")
	in_lines = fin.readlines()
	fin.close()

	# Replace OUTPUT FILENAME entry
	for l in in_lines:
		if l[:16]=="OUTPUT FILENAME:":
			i = in_lines.index(l)
			in_lines[i] = "%-20s%s\n" % ("OUTPUT FILENAME:",difxoutdir)
	
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

