#!/usr/bin/python
"""
Usage: replaceAntennaDiFX.py <antennaslist> <difx basename dst> <difx basename src>

Replaces visibilities on baselines to antenna(s) by visibilities from a second DiFX file.

The result of the visibility-replacement is written into a new output .difx file. This
new output file is a copy of DiFX file 'dst' where visibilities associated with antennas
of 'antennasList' have been replaced by the respective visibilities from DiFX file 'src'.
If no matching visibility record exists in 'src' the visibility from 'dst' is discarded.

Options:
    <antennaslist>      a comma separated list of antenna names in upper case, e.g., EB,PV,MH
    <difx basename dst> difx data to be 'patched' by new visibility data on baselines to <antennaslist>
    <difx basename src> difx data from which to take visibility data for baselines to <antennaslist>

Output:
  <difx basename>_antreplaced/DIFX_*
"""
import glob, sys, os, shutil
import parseDiFX

"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
def getVisibilityHeader(f):
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)

"""Return difx dataset base name for a given file/basename"""
def getBasename(str):
	basename = str
	if basename.endswith(('.difx','.input','.calc','.im')):
		basename = basename[:basename.rfind('.')]
	pathless_basename = basename
	if pathless_basename.rfind('/'):
		pathless_basename = pathless_basename[pathless_basename.rfind('/')+1:]
	return (pathless_basename,basename)

"""Build index of file-offsets into a DiFX file, for given antenna(s)"""
def buildIndex(inputfilename,difxfile,antList):

	# Get metadata
	print ('Checking and indexing %s' % (inputfilename))
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfilename)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfilename)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfilename)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfilename)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		print ("Couldn't parse input file " + inputfilename + " correctly")
		sys.exit(-1)

	# Read visibility records one by one
	indices={}
	peak_sec=0
	while True:
		offset = difxfile.tell()
		(vishdr,binhdr) = getVisibilityHeader(difxfile)
		if len(vishdr) <= 0:
			break

		# Visibility properties
		baseline = vishdr[0]
                seconds = vishdr[2]
		freqindex = vishdr[5]
		polpair = vishdr[6]
		uvw = vishdr[9:12]
		freq = freqs[freqindex]

		# Antenna order as in difx2mark4: ref=ant1="256*nr", rem=ant2="nr%256"
		ant2 = baseline % 256
		ant1 = (baseline-ant2)/256
		ant1name = telescopes[ant1-1].name.upper()
		ant2name = telescopes[ant2-1].name.upper()

		# Number of channels in this baseband
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg

		# Info string
		sband = 'U'
		if freq.lsb:
			sband = 'L'
		#key = ant1name + '-' + ant2name + '/' + polpair + '/' + str(freqindex) + '@' + str(freq.freq) + sband + ':' + str(seconds)
		key = ant1name + '-' + ant2name + '/' + str(freq.freq) + sband + '_' + polpair + ':' + str(seconds) + ':' + str(nchan)

		# Read the entire visibility data from disk
		rawvis = difxfile.read(8*nchan)
		if len(rawvis) < 8*nchan:
			break

		# Indexing
		if (ant1name in antList) or (ant2name in antList):
			indices[key] = offset
			#print key, uvw
		if seconds > peak_sec:
			peak_sec = seconds
			print ('.'),
			sys.stdout.flush()

	print ('\nIndexing complete, found %d records' % (len(indices)))
	return indices

"""Process one or more files"""
def patchDiFX(basename_dst,basename_src,antList):

	# Derive file names
	(pathless_basename_dst, basename_dst) = getBasename(basename_dst)
	(pathless_basename_src, basename_src) = getBasename(basename_src)

	# Open DiFX files
	difxdstfile = glob.glob(basename_dst + '.difx/DIFX_*.s*.b*')[0]
	fdst = open(difxdstfile, 'r')
	difxsrcfile = glob.glob(basename_src + '.difx/DIFX_*.s*.b*')[0]
	fsrc = open(difxsrcfile, 'r')
	difxoutdir = pathless_basename_dst + '_antreplaced.difx'
	difxoutfile = pathless_basename_dst + '_antreplaced.difx/' + difxdstfile[difxdstfile.rfind('/')+1:]
	try:
		os.mkdir(difxoutdir)
	except:
		pass
	fout = open(difxoutfile, 'w')		
	print ('Destination  : %s' % (difxdstfile))
	print ('Source       : %s' % (difxsrcfile))
	print ('Merged into  : %s' % (difxoutfile))

	# Make index of visibilities in source dataset
	src_index = buildIndex(basename_src+'.input',fsrc,antList)
	fsrc.seek(0)

	# Extract meta-infos from the DiFX .INPUT file
	inputfilename = basename_dst + '.input'
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfilename)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfilename)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfilename)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfilename)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		print ("Couldn't parse input file " + inputfilename + " correctly")
		sys.exit(-1)
	newinputfile = pathless_basename_dst + '_antreplaced.input'
	try:
		shutil.copyfile(inputfilename, newinputfile)
	except Exception as e:
		print ('Error copying %s to %s : %s' % (inputfilename,newinputfile,str(e)))

	# Copy/replace visibility data
	Ncopied = 0
	Nreplaced = 0
	Nnotfound = 0
	Vnotfound = []
	peak_sec = 0
	print ('Replacing visibilities to %s...' % (str(antList)))
	while True:

		(vishdr,binhdr) = getVisibilityHeader(fdst)
		if len(vishdr) <= 0:
			break

		# Visibility properties
		baseline = vishdr[0]
                seconds = vishdr[2]
		freqindex = vishdr[5]
		polpair = vishdr[6]
		freq = freqs[freqindex]

		# Antenna order as in difx2mark4: ref=ant1="256*nr", rem=ant2="nr%256"
		ant2 = baseline % 256
		ant1 = (baseline-ant2)/256
		ant1name = telescopes[ant1-1].name.upper()
		ant2name = telescopes[ant2-1].name.upper()

		# Number of channels in this baseband
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg

		# Info string
		sband = 'U'
		if freq.lsb:
			sband = 'L'
		#key = ant1name + '-' + ant2name + '/' + polpair + '/' + str(freqindex) + '@' + str(freq.freq) + sband + ':' + str(seconds)
		key = ant1name + '-' + ant2name + '/' + str(freq.freq) + sband + '_' + polpair + ':' + str(seconds) + ':' + str(nchan)

		if seconds > peak_sec:
			peak_sec = seconds
			print ('.'),
			sys.stdout.flush()

		# Read the entire visibility data from disk
		rawvis = fdst.read(8*nchan)
		if len(rawvis) < 8*nchan:
			continue

		# Replace with data from source if key matches
		# note: vis record header from 'src' is thrown away; use Freq ID, Ant ID etc from 'dst' file since numerical IDs can mismatch between src/dst!
		if (ant1name in antList) or (ant2name in antList):
			if key in src_index:
				offset = src_index[key]
				fsrc.seek(offset)
				(vishdr2,binhdr2) = getVisibilityHeader(fsrc)
				rawvis = fsrc.read(8*nchan)
				if len(rawvis) < 8*nchan:
					print ('Error: could not get %s with %d nchannels from source file!' % (key,nchan))
					continue
				Nreplaced += 1
			else:
				Nnotfound += 1
				brief_key = ant1name + '-' + ant2name + '/' + str(freq.freq) + sband + '_' + polpair
				if not (brief_key in Vnotfound):
					Vnotfound.append(brief_key)
				continue
		else:
			Ncopied += 1

		fout.write(binhdr)
		fout.write(rawvis)

	# Finished
	fout.close()
	print('')
	print('Vis. copied     : %d' % (Ncopied))
	print('Vis. replaced   : %d' % (Nreplaced))
	print('Vis. not in src : %d' % (Nnotfound))
	if Nnotfound > 0:
		print('        details : %s' % (str(Vnotfound)))

	# TODO:
	print ('Generated visibilities and %s, but please manually edit its OUTPUT FILENAME line to fix the path (TODO: automatic path fix)' % (newinputfile))

# Cmd line args
if __name__ == "__main__":
	args = sys.argv[1:]
	if len(args) != 3:
		print __doc__
		sys.exit(-1)
	antList = args[0].split(',')
	patchDiFX(args[1],args[2],antList)
