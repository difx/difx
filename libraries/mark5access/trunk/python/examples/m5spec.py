#!/usr/bin/python
"""
m5spec.py ver. 1.0   Jan Wagner  20150427
 
Shows time-averaged autocorrelation spectra of raw VLBI data.

Usage : m5spec.py <infile> <dataformat> <T_int (ms)> <Ldft> [offset]
 
  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:
    VLBA1_2-256-8-2
    MKIV1_4-128-2-1
    Mark5B-512-16-2
    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)
 
  <T_int>     approximate integration time per spectrum in milliseconds
  <Ldft>      length in points of Fourier transform across full bandwidth
 
  <offset>    is the byte offset into the file
"""
import ctypes, numpy, sys, pylab
import mark5access as m5lib
from datetime import datetime
try:
	import matplotlib as mpl
	mpl.rcParams['path.simplify'] = False # http://stackoverflow.com/questions/15795720/matplotlib-major-display-issue-with-dense-data-sets
except:
	pass    

def usage():
	print __doc__


def m5spec(fn, fmt, fout, T_int_ms, nfft, offset):
	"""Form time-averaged autocorrelation spectra"""

	# Open file
	try:
		m5file = m5lib.new_mark5_stream_file(fn, ctypes.c_longlong(offset))
		m5fmt  = m5lib.new_mark5_format_generic_from_string(fmt)
		ms     = m5lib.new_mark5_stream_absorb(m5file, m5fmt)
		dms    = ms.contents
	except:
		print ('Error: problem opening or decoding %s\n' % (fn))
		return 1

	# Settings
	nint  = numpy.round(float(dms.samprate)*T_int_ms*1e-3/float(nfft))
	Tint  = float(nint*nfft)/float(dms.samprate)
	df    = float(dms.samprate)/float(nfft)
	iter  = 0
	print ('Averaging a total of %u DFTs, each with %u points, for a %f millisecond time average.' % (nint,nfft,Tint*1e3))

	# Collection of vectors for mark5access decode() raw sample output data
        pdata = m5lib.helpers.make_decoder_array(ms, nfft, dtype=ctypes.c_float)

	# Result arrays
	ch_data = [ctypes.cast(pdata[ii],ctypes.POINTER(ctypes.c_float*nfft)) for ii in range(dms.nchan)]
	freqs = numpy.linspace(0.0, dms.samprate*1e-6, num=nfft)
	specs = numpy.zeros(shape=(dms.nchan,nfft), dtype='float64')

	# Process the recorded data
	while True:

		# Read data
		rc = m5lib.mark5_stream_decode(ms, nfft, pdata)
		if (rc < 0):
			print ('\n<EOF> status=%d' % (rc))
			return 0

		# Averaging of 'Abs(FFT(x))'
		for ii in range(dms.nchan):
			x = numpy.frombuffer(ch_data[ii].contents, dtype='float32')
			specs[ii] += numpy.abs(numpy.fft.fft(x))

		# Save data and plot at the end of an averaging period
		iter = iter + 1
		if (iter % nint)==0:

			layouts = [(None,None),  (1,1), (1,2), (1,3), (2,2), (1,5), # 1 to 5 channels
				   (2,3), (2,4), (2,4), (3,3), (2,5), (3,4), (2,6), # 6 to 12 channels
				   (3,5), (3,5), (3,5), (4,4)] # 13 to 16 channels

			N = int(numpy.floor(float(nfft)/2 + 1))
			M = int(numpy.min([16,dms.nchan]))
			rows,cols = layouts[M]

			f = freqs[0:N]
			s = specs[:,0:N]
			s /= float(nint)
			s[:,0]  = s[:,1]
			s[:,-1] = s[:,-2]

			writeOut(fout,f,s)

			a_min = numpy.amin(s)
			a_max = numpy.amax(s)

			pylab.gcf().set_facecolor('white')
			for ch in range(M):
				pylab.subplot(rows,cols,ch+1)
				pylab.plot(f,abs(s[ch]),'k-')
				pylab.title('Channel %d' % (ch+1))
				pylab.axis('tight')
				pylab.ylim([a_min,a_max])
				if ch >= (M-cols):
					pylab.xlabel('Frequency (MHz)')
				else:
					pylab.gca().set_xticklabels([])
				if (ch % rows) == 0:
					pylab.ylabel('Amplitude (Sum|FFT(x)|)')
				else:
					pylab.gca().set_yticklabels([])
			pylab.show()

			# Current version: stop after 1 integration period
			break

	return 0


def writeOut(fout,f,s):
	print ('Writing %u values into %s ...' % (s.size,fout.name))
	for ch in range(len(f)):
		chvals = numpy.array_str(s[:,ch], max_line_width=1e9)
		fout.write('%f %s\n' % (f[ch],chvals[1:-1]))
	print ('Done.')


def main(argv=sys.argv):

	# Args
	if len(argv) not in [5,6]:
		usage()
		sys.exit(1)
	offset = 0
	if len(argv) == 6:
		offset = int(argv[5])

	# Start processing
	fout = open('m5spec.txt', 'wt')
	rc = m5spec(argv[1],argv[2], fout, abs(float(argv[3])), int(argv[4]), offset)
	fout.close()

	return rc

if __name__ == "__main__":
	sys.exit(main())
