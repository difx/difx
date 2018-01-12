#!/usr/bin/python
"""
Usage: printDiFX.py <difx base name>
"""
import glob, sys, numpy
import parseDiFX

"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
def getVisibilityHeader(f):
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)

"""Print visibility data records"""
def printVisibilityfile(basename,Nprintchans=8):

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'
	inputfile_cfg = parseDiFX.get_common_settings(inputfile)
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		parser.error("Couldn't parse input file " + inputfile + " correctly")

	# Read the DiFX .difx/DIFX_* file
	glob_pattern = inputfile_cfg['difxfile'] + '/DIFX_*.s*.b*'
	difxfileslist = glob.glob(glob_pattern)
	if len(difxfileslist) <= 0:
		print ('Error: no visibility data file found in %s!' % (glob_pattern))
		return
	difxfilename = difxfileslist[0]
	difxfile = open(difxfilename, 'r')

	# Parse each visibility entry
	seconds_prev = -1
	seconds_first = 0
	seconds_report_prev = 0
	while True:
		(vishdr,binhdr) = getVisibilityHeader(difxfile)
		if len(vishdr)==0:
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

		# Read the entire visibility data from disk
		rawvis = difxfile.read(8*nchan)
		if len(rawvis) != 8*nchan:
			print ('Short read! Stopping.')
			break

		# Info string
		visdata = numpy.fromstring(rawvis, dtype='complex64')
		is_complex = any(numpy.iscomplex(visdata))
		if is_complex:
			dtype_str = 'complex'
		else:
			dtype_str = 'real   '
		uvw_str = numpy.array2string(numpy.array(uvw), precision=6, separator=' ', prefix='', suppress_small=False)
		vis_info = '%s-%s/%d/%d:%.7f/%s  mjd:%-14.8f nchan:%-3d bw:%.4f uvw=%s : %s' % (ant1name,ant2name,baseline,freqindex,fsky,polpair,T,nchan,bw,uvw_str,dtype_str)
		if (ant1 == ant2) and (polpair[0] == polpair[1]) and is_complex:
			print ('warning: complex data in parallel-hand autocorrelation!')
		data = visdata[:Nprintchans]
		data_str = numpy.array2string(data, precision=3, max_line_width=1e99, separator=' ', prefix='', suppress_small=True)
		print ('%s : %s' % (vis_info, data_str))

	print ('Done')

if len(sys.argv) < 2:
	print __doc__
	sys.exit(-1)

for difxf in sys.argv[1:]:
	printVisibilityfile(difxf)

