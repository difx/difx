#!/usr/bin/python
"""
m5subband.py ver. 1.1   Jan Wagner  20150603
 
Extracts a narrow subband via filtering raw VLBI data. 
Reads formats supported by the mark5access library.
 
Usage : m5subband.py <infile> <dataformat> <outfile> 
                     <if_nr> <factor> <Ldft> 
                     <start_bin> <stop_binN> [<offset>]
 
  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:
    VLBA1_2-256-8-2
    MKIV1_4-128-2-1
    Mark5B-512-16-2
    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)

  <outfile>   output file for 32-bit float subband data (VDIF format)
  <if_nr>     the IF i.e. baseband channel to be filtered (1...nchan)
  <factor>    overlap-add factor during filtering (typ. 4)
  <Ldft>      length of DFT
  <start_bin> take output starting from bin (0...Ldft-2)
  <stop_bin>  take output ending with bin (start_bin...Ldft-1)
 
  <offset> is the byte offset into the file
"""

import ctypes, numpy, re, struct, sys
import mark5access as m5lib
from datetime import datetime
from scipy import stats

refMJD_Mark5B = 57000 # reference MJD for Mark5B input data

def usage():
	print __doc__

def m5subband(fn, fmt, fout, if_nr, factor, Ldft, start_bin, stop_bin, offset):
	"""Extracts narrow-band signal out from file"""

	# Derived settings
	nin  = Ldft
	nout = stop_bin - start_bin + 1
	#Lout = next_pow2(2*(nout-nout%2)) # time-domain output data will be somewhat oversampled
	Lout = next_even(2*(nout-nout%2)) # time-domain output data will be closer to critically sampled
	iter = 0

	# Open file
	try:
		m5file = m5lib.new_mark5_stream_file(fn, ctypes.c_longlong(offset))
		m5fmt  = m5lib.new_mark5_format_generic_from_string(fmt)
		ms     = m5lib.new_mark5_stream_absorb(m5file, m5fmt)
		dms    = ms.contents
		m5lib.mark5_stream_fix_mjd(ms, refMJD_Mark5B)
		(mjd,sec,ns) = m5lib.helpers.get_sample_time(ms)
	except:
		print ('Error: problem opening or decoding %s\n' % (fn))
		return 1

	# Safety checks
	if (if_nr<0) or (if_nr>=dms.nchan) or (factor<0) or (factor>32) or (Ldft<2) or (start_bin>stop_bin) or (stop_bin>=Ldft):
		print ('Error: invalid command line arguments')
		return 1
	if (Ldft % factor)>0:
		print ('Error: length of DFT (Ldft=%u) must be divisible by overlap-add factor (factor=%u)' % (Ldft,factor))
		return 1
	if (Lout %  factor)>0:
		print ('Error: length derived for output IDFT (Lout=%u) does not divide the overlap-add factor (factor=%u)' % (Lout,factor))
		return 1

	# Get storage for raw sample data from m5lib.mark5_stream_decode()
	pdata = m5lib.helpers.make_decoder_array(ms, nin, dtype=ctypes.c_float)
	if_data = ctypes.cast(pdata[if_nr], ctypes.POINTER(ctypes.c_float*nin))

	# Numpy 2D arrays for processed data
	fp = 'float32'
	cp = 'complex64' # complex64 is 2 x float32
	flt_in  = numpy.zeros(shape=(factor,nin), dtype=fp)
	flt_out = numpy.zeros(shape=(factor,Lout), dtype=cp)
	iconcat = numpy.array([0.0 for x in range(2*nin)], dtype=fp)
	oconcat = numpy.array([0.0+0.0j for x in range(2*Lout)], dtype=cp)

	# Coefficient for coherent phase connection between overlapped input segments
	r       = float(start_bin)/float(factor)
	rfrac   = r - numpy.floor(r)
	rot_f0  = numpy.exp(2j*numpy.pi*rfrac)
	if (abs(numpy.imag(rot_f0)) < 1e-5):
		# set near-zero values to zero
		rot_f0 = numpy.real(rot_f0) + 0.0j
	rot_f = rot_f0**0.0

	# Window functions for DFT and IDFT
	win_in  = numpy.cos((numpy.pi/nin)*(numpy.linspace(0,nin-1,nin) - 0.5*(nin-1)))
	win_in  = numpy.resize(win_in.astype(fp), new_shape=(factor,nin))
	win_out = numpy.cos((numpy.pi/Lout)*(numpy.linspace(0,Lout-1,Lout) - 0.5*(Lout-1)))
	win_out = numpy.resize(win_out.astype(fp), new_shape=(factor,Lout))

	# Prepare VDIF output file with reduced data rate and same starting timestamp
	fsout   = float(dms.samprate)*(nout/float(nin))
	outMbps = fsout*1e-6 * 32
	vdiffmt = 'VDIF_8192-%u-1-32' % (outMbps)
	if not(int(outMbps) == outMbps):
		print ('*** Warning: output rate is non-integer (%e Ms/s)! ***' % (outMbps))

	(vdifref,vdifsec) = m5lib.helpers.get_VDIF_time_from_MJD(mjd,sec+1e-9*ns)

	vdif = m5lib.writers.VDIFEncapsulator()
	vdif.open(fout, format=vdiffmt, complex=False, station='SB')
	vdif.set_time(vdifref,vdifsec, framenr=0)
	vdiffmt = vdif.get_format()

	# Report
	bw = float(dms.samprate)*0.5
	print ('Input file   : start MJD %u/%.6f sec' % (mjd,sec+ns*1e-9))
	print ('Bandwidth    : %u kHz in, %.2f kHz out, bandwidth reduction of ~%.2f:1' % (1e-3*bw, nout*1e-3*bw/nin, float(nin)/nout))
	print ('Input side   : %u-point DFT with %u bins (%u...%u) extracted' % (nin,nout,start_bin,stop_bin))
	print ('Output side  : %u-point IDFT with %u-point zero padding' % (Lout,Lout-nout))
	print ('Overlap      : %u samples on input, %u samples on output' % (nin-nin/factor,Lout-Lout/factor))
	print ('Phasors      : %s^t : %s ...' % (str(rot_f0), str([rot_f0**t for t in range(factor+2)])))
	print ('Output file  : rate %.3f Mbps, %u fps, format %s' 
		% (outMbps,vdif.get_fps(),vdif.get_format()) )

	# Do filtering
	print ('Filtering...')
	while True:

		# Get next full slice of data
		rc = m5lib.mark5_stream_decode(ms, nin, pdata)
		if (rc < 0):
			print ('\n<EOF> status=%d' % (rc))
			return 0
		in_new = numpy.frombuffer(if_data.contents, dtype='float32')

		# Debug: replace data with noise + tone
		if False:
			t = iter*nin + numpy.array(range(nin))
			f = (start_bin + numpy.floor(nout/2.0)) / float(nin)
			in_new = numpy.random.standard_normal(size=in_new.size) + 10*numpy.sin(2*numpy.pi * f*t)
			in_new = in_new.astype('float32')

		# Feed the window-overlap-DFT processing input stage
		iconcat = numpy.concatenate([iconcat[0:nin],in_new]) # [old,new]
		for ii in range(factor):
			iconcat = numpy.roll(iconcat, -nin/factor)
			flt_in[ii] = iconcat[0:nin]
	
		# Window and do 1D DFT of 2D array
		flt_in = numpy.multiply(flt_in,win_in)
		F = numpy.fft.fft(flt_in)

		# Copy the desired bins and fix DC/Nyquist bins
		for ii in range(factor):
			flt_out[ii][0:nout] = F[ii][start_bin:(start_bin+nout)]
			flt_out[ii][0]      = numpy.real(flt_out[ii][0])
			flt_out[ii][nout-1] = numpy.real(flt_out[ii][nout-1])

		# Do inverse 1D DFT and window the result
		F = numpy.fft.ifft(flt_out)
		F = numpy.multiply(F,win_out)

		# Reconstruct time domain signal by shifting and stacking overlapped segments coherently
		for ii in range(factor):
			oconcat[Lout:] = oconcat[Lout:] + F[ii]*rot_f
			rot_f = rot_f * rot_f0
			oconcat = numpy.roll(oconcat, -Lout/factor)
			# note: numpy has a circular shift (numpy.roll), but no "shift array left/right" function,
			# so we need to zero out the undesired values shifted back in by the circular shift:
			oconcat[(-Lout/factor):] = 0

		# Output real part of complex time domain data
		# (If suppression of upper Nyquist is zone desired, should write out both real&imag)
		vdif.write(numpy.real(oconcat[0:Lout]).view('float32').tostring())

		# Reporting
		if (iter % 100)==0:
			(mjd,sec,ns) = m5lib.helpers.get_sample_time(ms)
			T_abs = sec + 1e-9*ns
			T_count = 1e-9*dms.framens * dms.nvalidatepass
			print ('Iter %7d : %u/%f : %u : %f sec\r' % (iter, mjd,T_abs, dms.nvalidatepass, T_count)),
		iter = iter + 1

	vdif.close()
	return 0


def next_pow2(n):
	"""Returns the power-of-2 closest to and larger than or equal to n"""
	return int(2.0**numpy.ceil(numpy.log(n)/numpy.log(2)))

def next_even(n):
	"""Returns the even number closest to and larger than or equal to n"""
	return int(n + n%2)

def main(argv=sys.argv):
	if len(argv) not in [9,10]:
		usage()
		sys.exit(1)

	offset = 0
	if len(argv) == 10:
		offset = int(argv[9])

	if_nr  = int(argv[4])-1
	factor = int(argv[5])
	Ldft   = int(argv[6])
	start_bin = int(argv[7])
	stop_bin  = int(argv[8])

	rc = m5subband(argv[1],argv[2],argv[3], if_nr, factor,Ldft,start_bin,stop_bin, offset)

	return rc

if __name__ == "__main__":
	sys.exit(main())
