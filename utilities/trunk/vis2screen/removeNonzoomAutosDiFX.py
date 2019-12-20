#!/usr/bin/python
"""
Usage: removeNonzoomAutosDiFX.py <difx basename>
Or   : pypy removeNonzoomAutosDiFX.py <difx basename>

Removes all autocorrelations that are not from zoom bands,
preserving only those non-zoom autocorrelations that come
from antennas that lack zoom bands.

The purpose of this filtering is to get rid of duplicate
auto records (zoom & rec) that DiFX produces in a corner
case of zoom correlation (when zoom band == parent band).
Duplicate autos confuse difx2fits and lead to incorrectly
low weights in AIPS ACCOR. Filtering and reducing auto
records to only the non-redundant non-duplicate records
fixes difx2fits and recovers correct AIPS ACCOR weights.

Output:
  <difx basename>_filtered/DIFX_*

"""
try:
	import numpy
	haveNumpy = True
except:
	haveNumpy = False
import sys, os, shutil, glob
import parseDiFX

def filterVisibilityfile(basename):

	# Determine name of input file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	if basename.rfind('/')>=0:
		pathless_basename = basename[basename.rfind('/')+1:]
	else:
		pathless_basename = basename
	inputfile = basename + '.input'
	inputfilenew = pathless_basename + '_filtered.input'

	# Open the source file
	difx = parseDiFX.DiFXFile()
	difx.open(inputfile)

	# Determine which antennas have zoom bands that are identical to parent band,
	# only these antennas need to have parent band autos discarded
	# (or more correctly, but TODO, ought to check all Baselines and determine which freqs reflect the final output freq!)
	cfg = difx.metainfo
	filterableAntennas = set()
	for d in cfg.datastreams:
		if d.nzoomfreq==0 or d.nzoomband==0:
			print ('Telescope %s datastream %d: no zoom bands : -> no filtering' % (cfg.telescopes[d.telescopeindex].name, cfg.datastreams.index(d)))
			continue
		if d.nzoomband != d.nrecband:
			print ('Telescope %s datastream %d: %d zoom bands != %d rec bands : unsupported -> no filtering ' % (cfg.telescopes[d.telescopeindex].name, cfg.datastreams.index(d), d.nzoomband, d.nrecband))
			continue
		# TODO: ought to check if zooms cover all parents 1:1
		print ('Telescope %s datastream %d: %d zoom bands == %d rec bands : -> filtering!' % (cfg.telescopes[d.telescopeindex].name, cfg.datastreams.index(d), d.nzoomband, d.nrecband))
		filterableAntennas.add(d.telescopeindex)
	names = [cfg.telescopes[a].name for a in filterableAntennas]
	print ('Discarding non-zoom autos for: %s / %s' % (str(filterableAntennas), ' '.join(names)))

	# Prepare output
	difxoutdir = pathless_basename + '_filtered.difx'
	difxoutname = difxoutdir + '/' + difx.difxfilename[difx.difxfilename.rfind('/')+1:]
	print ('Outputting to file %s' % (difxoutname))
	try:
		os.mkdir(difxoutdir)
	except:
		pass
	for pcalfile in glob.glob(difx.difxfilepath + '/PCAL_*'):
		print ('Copying %s' % (pcalfile))
		shutil.copy(pcalfile, difxoutdir + '/')
	difxout = open(difxoutname, 'w')

	# Generate copy of .input that refers to new output .difx
	fin = open(inputfile,"r")
	inp_lines = fin.readlines()
	fin.close()
	fo = open(inputfilenew, 'w')
	for line in inp_lines:
		if line[:16]=="OUTPUT FILENAME:":
			line = "%-20s%s\n" % ("OUTPUT FILENAME:",difxoutdir)
		fo.write(line)
	fo.close()

	# Copy data
	npassed = 0
	nremoved = 0
	seconds_prev = -1
	while True:

		# Get the next visibility record
		visrec = difx.nextVisibilityRecord()
		if not visrec.header.isvalid():
			break

		# Get details of the visibility record
		fq = difx.getFrequency(visrec.header.freqindex)
		nchan = fq.numchan / fq.specavg

		# Copy the record?
		doCopy = True
		if visrec.header.antenna1 == visrec.header.antenna2:
			antIdx = visrec.header.antenna1 - 1
			if antIdx not in filterableAntennas:
				# print ('Retaining auto record of recfreqs-only non-zoom antenna %d/%s' % (visrec.header.antenna1, cfg.telescopes[antIdx].name))
				pass
			elif fq.lsb:
				doCopy = False

		if doCopy:
			difxout.write(visrec.header.raw)
			if haveNumpy:
				visrec.vis.tofile(difxout)
			else:
				difxout.write(visrec.vis)
			npassed += 1
		else:
			nremoved += 1

		seconds_now = visrec.header.mjd*24*60*60 + visrec.header.seconds
		if seconds_prev == -1:
			seconds_prev = seconds_now
		if seconds_now != seconds_prev:
			print ("at %d %12.7f, %.3f seconds from start" % (visrec.header.mjd,visrec.header.seconds,seconds_now-seconds_prev))
			print ("\033[F\033[F")

	difxout.close()

	# copy pcal files

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. passed through : %d' % (npassed))
	print ('    vis. removed        : %d' % (nremoved))
	print ('\nOutput file:')
	print ('    difx .input        : %s' % (inputfilenew))
	print ('    visbility data     : %s' % (difxoutname))
	print (' ')


if __name__ == '__main__':

	if len(sys.argv) < 2:
		print __doc__
		sys.exit(-1)

	for difxf in sys.argv[1:]:
		filterVisibilityfile(difxf)
