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


def buildIndex(inputfilename, difxfile, antList, verbosity=0):
	"""Build index of file-offsets into a DiFX file, for given antenna(s)"""

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

		## DEBUG
		#if offset > 128*1024:
		#	print('DEBUG: exiting early from buildIndex()')
		#	break

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
			if verbosity >= 1:
				print (key, uvw)
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
	inputfilename = basename_dst + '.input'

	# Open DiFX files
	difxdstfile = glob.glob(basename_dst + '.difx/DIFX_*.s*.b*')[0]
	difxsrcfile = glob.glob(basename_src + '.difx/DIFX_*.s*.b*')[0]
	fdst = open(difxdstfile, 'r')
	fsrc = open(difxsrcfile, 'r')

	# Prepare output copy of .input and .difx
	difxoutdir = pathless_basename_dst + '_antreplaced.difx'
	difxoutswin = difxoutdir + '/' + difxdstfile[difxdstfile.rfind('/')+1:]
	difxoutinp = pathless_basename_dst + '_antreplaced.input'

	fin = open(inputfilename,"r")
	fo = open(difxoutinp, "w")
	for line in fin.readlines():
		if line[:16]=="OUTPUT FILENAME:":
			line = "%-20s%s\n" % ("OUTPUT FILENAME:",difxoutdir)
		fo.write(line)
	fo.close()

	try:
		os.mkdir(difxoutdir)
	except:
		pass

	try:
		shutil.copyfile(difxdstfile, difxoutswin)
	except Exception as e:
		print ('Error copying %s to %s : %s' % (difxdstfile,difxoutswin,str(e)))

	print ('Destination  : %s' % (difxdstfile))
	print ('Source       : %s' % (difxsrcfile))
	print ('Merged into  : %s' % (difxoutswin))

	# Make index of visibilities in patch input (source) and to-patch (destination) datasets
	src_index = buildIndex(basename_src+'.input',fsrc,antList)
	dst_index = buildIndex(basename_dst+'.input',fdst,antList)
	fsrc.seek(0)
	fdst.seek(0)

	# Report records that can be patched
	Nmatches = 0
	for k in src_index.keys():
		if k in dst_index:
			Nmatches += 1
		else:
			print('No counterpart for %s in destination' % (k))
	print('Records: %d match out of %d in patch source, patch destination has %d in total' % (Nmatches,len(src_index),len(dst_index)))

	# Copy/replace visibility data
	fout = open(difxoutswin, 'r+')
	Nreplaced = 0
	Nnotfound = 0
	Vnotfound = []

	print ('Replacing visibilities to %s...' % (str(antList)))
	for k in src_index.keys():
		if k in dst_index:

			in_offset = src_index[k]
			out_offset = dst_index[k]
			fsrc.seek(in_offset)
			fout.seek(out_offset)
			#print("fsrc tell()=%d, fout tell()=%d - copy patchfile from offset @%d to output file offset @%d" % (fsrc.tell(),fout.tell(),in_offset,out_offset))

			# Skip headers
			(skip1,skip2) = getVisibilityHeader(fsrc)
			(skip3,skip4) = getVisibilityHeader(fout)
			if len(skip1) < 1:
				print("Error: could not read next record header from source file")
				break
			if len(skip3) < 1:
				print("Error: could not read next record header from destination file")
				break

			#print("post header fsrc tell()=%d, fout tell()=%d" % (fsrc.tell(),fout.tell()))

			# Read patch viz data
			nchan = int(k.split(':')[-1])
			rawvis = fsrc.read(8*nchan)
			if len(rawvis) < 8*nchan:
				continue

			# Write out viz at position after the skipped header
			fout.write(rawvis)
			Nreplaced += 1

		else:

			Nnotfound += 1
			Vnotfound.append(k)

	fout.close()
	print('')
	print('Vis. replaced   : %d' % (Nreplaced))
	print('Vis. not in src : %d' % (Nnotfound))
	if Nnotfound > 0:
		print('        no counterpart for : %s' % (str(Vnotfound)))

	print ('Replaced visibilities and generated %s)' % (difxoutinp))


# Cmd line args
if __name__ == "__main__":

	args = sys.argv[1:]

	if len(args) != 3:
		print (__doc__)
		sys.exit(-1)
	antList = args[0].split(',')

	patchDiFX(args[1],args[2],antList)
