#!/usr/bin/python
import sys, numpy, ctypes
import mark5access as m5lib
from datetime import datetime
from scipy import stats

def usage():
	print (' ')
	print ('A data statistics checker for raw VLBI data. Reads formats supported by the mark5access library.')
	print (' ')
	print ('Usage : m5stat <file> <dataformat> <nframes> [<offset>]')
	print (' ')
	print ('  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:')
	print ('    VLBA1_2-256-8-2')
	print ('    MKIV1_4-128-2-1')
	print ('    Mark5B-512-16-2')
	print ('    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)')
	print (' ')
	print ('  <nframes> is number of data frames to read')
	print (' ')
	print ('  <offset> is the byte offset into the file')
	print (' ')

def m5stat(fn, fmt, nframes, offset):
	"""Reports statistics for file"""

	# Open file
	try:
		m5file = m5lib.new_mark5_stream_file(fn, ctypes.c_longlong(offset))
		m5fmt  = m5lib.new_mark5_format_generic_from_string(fmt)
		ms     = m5lib.new_mark5_stream_absorb(m5file, m5fmt)
		dms    = ms.contents
	except:
		print ("Error: problem opening or decoding %s\n" % (fn))
		return 1

	# Read sample data
	nsamples = dms.framesamples*nframes
	FLT32ARR = ctypes.c_float*nsamples
	PFLT32ARR = ctypes.POINTER(ctypes.c_float)*dms.nchan
	pdata = PFLT32ARR()
	for i in range(dms.nchan):
		pdata[i] = FLT32ARR()
	m5lib.mark5_stream_decode(ms, nsamples, pdata)

	# Statistics
	A = 8.0*(numpy.pi-3.0) / (3.0*numpy.pi*(4.0-numpy.pi))
	eps = 0.1
	hist_limits = [[],                                     # histogram ranges for
		[-1.0-eps,0,+1.0+eps],                         # 1-bit data
		[-3.3359-eps,-1.0-eps,0,+1.0+eps,+3.3359+eps], # 2-bit data
		[-1.0-eps,0-eps,0+eps,+1.0-eps,+1.0+eps],      # 3-bit data
		[x/2.95-eps for x in range(-8,8+1)],           # 4-bit data
		[], [], [],
		[(x*2-255)/71.0-eps for x in range(0,255+1)] ] # 8-bit data

	print (' Ch     mean    std    skew  kurt    gfact   state counts from -HiMag to +HiMag')
	for i in range(dms.nchan):
		d = pdata[i][0:nsamples]
		m0 = numpy.mean(d)
		m1 = numpy.std(d)
		m2 = stats.skew(d)
		m3 = stats.kurtosis(d, fisher=False, bias=False)
		(hcounts,hlimits) = numpy.histogram(d, bins=hist_limits[dms.nbit])
		hpdf = [x/float(sum(hcounts)) for x in hcounts]

		if dms.nbit==2:
			x = hpdf[1] + hpdf[2]
		else:
			x = hpdf[0] + hpdf[1]
		if dms.nbit==2 or dms.nbit==1:
			k = numpy.log(1.0-x*x)
			g = numpy.sqrt( -4.0/(A*numpy.pi) - k 
				+ 2.0*numpy.sqrt( 2.0**(2.0/(A*numpy.pi)+0.5*k) - k/A)) / 0.91
		else:
			g = float('NaN')

		hstr = ' / '.join(['%4.1f' % (100.0*x) for x in hpdf])
		print ('%3d   %+6.2f %+6.2f   %+5.2f %+5.2f   %+6.2f   %s' % (i,m0,m1,m2,m3,g,hstr))

	print 'based on %u samples (%u invalid)' % (dms.framesamples*(dms.nvalidatepass+dms.nvalidatefail),dms.framesamples*dms.nvalidatefail)
	return 0


def main(argv=sys.argv):

	if len(argv) not in [4,5]:
		usage()
		sys.exit(1)

	offset = 0
	if len(argv) == 5:
		offset = int(argv[4])

	rc = m5stat(argv[1], argv[2], int(argv[3]), offset)
	return rc

if __name__ == "__main__":
	sys.exit(main())
