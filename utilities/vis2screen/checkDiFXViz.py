#!/usr/bin/python
"""
Usage: checkDiFXViz.py [-p|--phasecenter <n>] [-b|--pulsarbin <n>] <difx base name>

Inspects visibility data records and attempts to detect and report various
potential errors in the data weights, freq/band/pol presence (TODO), timestamps,
raw data values.
"""
import glob, sys
import parseDiFX

try:
    import numpy
    M_HAVE_NUMPY = True
except:
    M_HAVE_NUMPY = False


"""Check visibility data records"""
def checkVisibilityfile(basename, Nprintchans=4, phasecenterId=None, pulsarbinId=None):

	# Determine name of input file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'

	# Open the file set
	difx = parseDiFX.DiFXFile()
	difx.open(inputfile,phasecenterId=phasecenterId,pulsarbinId=pulsarbinId)
	
	# Counters
	nAPs = 0
	nTimestampErrors = 0
	nVisibilityDataInfErrors = 0
	nVisibilityDataNaNErrors = 0
	nLowWeightsCount = 0

	# Loop through the difx file
	TcurrAP = -1
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
		if TcurrAP < 0:
			TcurrAP = T

		is_complex = any(numpy.iscomplex(visrec.vis))
		is_finite = any(numpy.isfinite(visrec.vis))

		info_str = '%s-%s/%d/%d:%.7f%s/%s  mjd:%-14.8f nchan:%-3d bw:%.4f w:%.3f : ' % (
			ant1name, ant2name,
			visrec.header.baseline,
			visrec.header.freqindex,
			fq.freq, 'L' if fq.lsb else 'U',
			str(visrec.header.polpair), T, fq.numchan / fq.specavg, fq.bandwidth,
			visrec.header.weight )

		if is_complex:
			info_str += 'complex'
		else:
			info_str += 'real   '

		if T < TcurrAP:
			print('Incorrect time order of records: MJD %f too old wrt current AP MJD %f: record %s' % (T,TcurrAP,info_str))
			nTimestampErrors += 1

		if visrec.header.weight < 0.3:
			print('Low data weight record: %s' % (info_str))
			nLowWeightsCount += 1

		if not is_finite:

			info_str += numpy.array2string(numpy.array(visrec.header.uvw), precision=6, separator=' ', prefix='', suppress_small=False)
			data_str = " : " + numpy.array2string(visrec.vis[:Nprintchans], precision=3, max_line_width=1e99, separator=' ', prefix='', suppress_small=True)
			data_str += "... " + numpy.array2string(visrec.vis[nchan-Nprintchans:nchan], precision=3, max_line_width=1e99, separator=' ', prefix='', suppress_small=True)

			is_nan = any(numpy.isnan(visrec.vis))
			is_inf = any(numpy.isinf(visrec.vis))
			if is_nan and is_inf:
				print('Suspect visibility data with both NaN and inf: %s' % (info_str, data_str))
			else:
				if is_nan:
					print('Suspect visibility data with NaN values: %s' % (info_str, data_str))
				if is_inf:
					print('Suspect visibility data with inf values: %s' % (info_str, data_str))
			if is_nan:
				nVisibilityDataNaNErrors += 1
			if is_inf:
				nVisibilityDataInfErrors += 1

		if (visrec.header.antenna1 == visrec.header.antenna2) and (visrec.header.polpair[0] == visrec.header.polpair[1]) and is_complex:
			imagabs = numpy.abs(numpy.imag(visrec.vis))
			realabs = numpy.abs(numpy.real(visrec.vis))
			peakbin = imagabs.argmax()
			if imagabs[peakbin]/realabs[peakbin] > 1e-5:
				print ('Warning: complex data in parallel-hand autocorrelation! Peak %s in channel %d, %.3e %% imag/real ratio'	% (str(visrec.vis[peakbin]), peakbin, 100*imagabs[peakbin]/realabs[peakbin]))

		if T != TcurrAP:
			TcurrAP = T
			nAPs += 1
			print('New avg period %d with MJD %f' % (nAPs, TcurrAP))

	print ('Done, after approx %d avg periods' % (nAPs))
	print ('')
	print ('Low weights count           : %d' % (nLowWeightsCount))
	print ('Wrong timestamp order       : %d' % (nTimestampErrors))
	print ('Visibility records with Inf : %d' % (nVisibilityDataInfErrors))
	print ('Visibility records with NaN : %d' % (nVisibilityDataNaNErrors))


if __name__ == '__main__':

	phasecenterId = None
	pulsarbinId = None

	args = sys.argv[1:]
	while len(args) > 0 and args[0][0]=='-':
		if ('-p' in args[0] or '--phasecenter' in args[0]) and len(args) > 1:
			phasecenterId = int(args[1])
			args = args[2:]
		elif ('-b' in args[0] or '--pulsarbin' in args[0]) and len(args) > 1:
			pulsarbinId = int(args[1])
			args = args[2:]
		elif args[0][0] == '-':
			args = args[1:]

	if not M_HAVE_NUMPY:
		print('Error: Python numpy is required but could not be found')
		sys.exit(-1)

	if len(args) < 1:
		print (__doc__)
		sys.exit(-1)

	for difxf in args:
		checkVisibilityfile(difxf,phasecenterId=phasecenterId,pulsarbinId=pulsarbinId)

