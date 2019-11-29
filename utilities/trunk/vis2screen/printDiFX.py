#!/usr/bin/python
"""
Usage: printDiFX.py <difx base name>

Displays text based summaries of all visibility records in a DiFX data set.
"""
import glob, sys
import parseDiFX

try:
    import numpy
    M_HAVE_NUMPY = True
except:
    M_HAVE_NUMPY = False


"""Print visibility data records"""
def printVisibilityfile(basename, Nprintchans=4):

	# Determine name of input file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'

	# Open the file set
	difx = parseDiFX.DiFXFile()
	difx.open(inputfile)

	# Loop through the difx file
	while True:

		# Get the next visibility record
		visrec = difx.nextVisibilityRecord()
		if not visrec.header.isvalid():
			break

		# Get details of the visibility record
		fq = difx.getFrequency(visrec.header.freqindex)
		ant1name = difx.getTelescope(visrec.header.antenna1 - 1).name
		ant2name = difx.getTelescope(visrec.header.antenna2 - 1).name
		baselinestr = '%s-%s' % (ant1name,ant2name)					
		nchan = fq.numchan / fq.specavg
		T = visrec.header.mjd + visrec.header.seconds/86400.0

		# Print out the details
		info_str = '%s-%s/%d/%d:%.7f%s/%s  mjd:%-14.8f nchan:%-3d bw:%.4f w:%.3f : ' % (
			ant1name, ant2name,
			visrec.header.baseline,
			visrec.header.freqindex,
			fq.freq, 'L' if fq.lsb else 'U',
			str(visrec.header.polpair), T, fq.numchan / fq.specavg, fq.bandwidth,
			visrec.header.weight )

		if M_HAVE_NUMPY:
			is_complex = any(numpy.iscomplex(visrec.vis))
			if is_complex:
				info_str += 'complex : '
			else:
				info_str += 'real    : '
			info_str += numpy.array2string(numpy.array(visrec.header.uvw), precision=6, separator=' ', prefix='', suppress_small=False)
			data_str = numpy.array2string(visrec.vis[:Nprintchans], precision=3, max_line_width=1e99, separator=' ', prefix='', suppress_small=True)
			data_str += "... " + numpy.array2string(visrec.vis[nchan-Nprintchans:nchan], precision=3, max_line_width=1e99, separator=' ', prefix='', suppress_small=True)
			print ('%s : %s' % (info_str, data_str))

			if (visrec.header.antenna1 == visrec.header.antenna2) and (visrec.header.polpair[0] == visrec.header.polpair[1]) and is_complex:
				imags = numpy.abs(numpy.imag(visrec.vis))
				reals = numpy.abs(numpy.real(visrec.vis))
				peakbin = imags.argmax()
				print ('Warning: complex data in parallel-hand autocorrelation! Peak %s in channel %d, %.3e %% imag/real ratio'	% (str(visrec.vis[peakbin]), peakbin, 100*imags[peakbin]/reals[peakbin]))
		else:
			print (info_str)

	print ('Done')


if __name__ == '__main__':

	if len(sys.argv) < 2:
		print (__doc__)
		sys.exit(-1)

	for difxf in sys.argv[1:]:
		printVisibilityfile(difxf)
