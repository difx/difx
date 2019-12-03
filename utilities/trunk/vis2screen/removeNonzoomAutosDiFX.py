#!/usr/bin/python
"""
Usage: removeNonzoomAutosDiFX.py <difx basename>

Removes all autocorrelations that are not from zoom bands.

Output:
  <difx basename>_filtered/DIFX_*

"""
import numpy
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

	# Prepare output
	difxoutdir = pathless_basename + '_filtered.difx'
	difxoutname = difxoutdir + '/' + difx.difxfilename[difx.difxfilename.rfind('/')+1:]
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
			# ToD: now should look up 'fq' under all datastreams of both antennas...
			if fq.lsb:
				doCopy = False

		if doCopy:
			difxout.write(visrec.header.raw)
			visrec.vis.tofile(difxout)
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
