#!/usr/bin/python
"""
m5subband.py ver. 1.2   Jan Wagner  20160228
 
Extracts a narrow subband via filtering raw VLBI data. 
Reads formats supported by the mark5access library.
 
Usage : m5subband.py <infile> <dataformat> <outfile>
                     <if_nr> <quality factor>
                     <start_MHz> <stop_MHz> [<offset>]
 
  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:
    VLBA1_2-256-8-2
    MKIV1_4-128-2-1
    Mark5B-512-16-2
    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)

  <outfile>   output file for 32-bit float subband data (VDIF format)
  <if_nr>     the baseband channel to select for filtering (1...<nchan>)
  <factor>    any quality factor >=1, with larger factors giving
              progressively steeper filter cutoffs i.e. less leakage
  <start_MHz> starting frequency of desired output band
  <stop_MHz>  stop frequency of desired output band

  <offset> is the optional byte offset into the file
"""

import ctypes, numpy, re, struct, sys
import mark5access as m5lib
from datetime import datetime
from scipy import stats

def usage():
	print __doc__

def m5subband(fn, fmt, fout, if_nr, factor, start_MHz, stop_MHz, offset):
	"""Extracts narrow-band signal out from file"""

	# Hard-coded settings
	refMJD_Mark5B = 57000 # reference MJD for Mark5B input data
	Ldft_out = 64         # number of DFT points for output transform
	m5_t = ctypes.c_float # mark5access output type for decoded samples
	fp_t = 'float32'      # numpy array type for DFT input
	cp_t = 'complex64'    # numpy array type for DFT output, complex64 is 2 x float32
	nbits_out = 8         # quantization (32 or 8 bits) to use in output VDIF file

	stats_done = False
	out_std = 1.0	

	# Open the VLBI recording
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

	if (factor<0) or (factor>16) or (if_nr<0) or (if_nr>=dms.nchan):
		print ('Error: invalid arguments')
		return 1

	# Derive input-side DFT length
	bw_in   = float(dms.samprate)*0.5*1e-6
        bw_out  = numpy.abs(stop_MHz - start_MHz)
        R       = bw_in/bw_out
	Ldft_in = int(R*Ldft_out+0.5)
	radix   = [2,3,5,7,11]
	while not(any([(Ldft_in%rx)==0 for rx in radix])) and not((Ldft_in % factor)==0):
		Ldft_in = Ldft_in + 1
	R = Ldft_in/float(Ldft_out)
	start_bin = int(Ldft_in*start_MHz/bw_in)
	stop_bin  = int(Ldft_in*stop_MHz/bw_in)
	start_MHz = (bw_in*start_bin)/Ldft_in
	stop_MHz  = (bw_in*stop_bin)/Ldft_in
	bw_out    = numpy.abs(stop_MHz - start_MHz)
	Ldft_in   = 2*Ldft_in    # due to r2c DFT rather than c2c DFT

	# Increase output-side DFT's input length, i.e., use zero padding
	Ldft_copy = Ldft_out
	#Ldft_out = next_pow2(2*(Ldft_out-Ldft_out%2)) # time-domain output data will be somewhat oversampled
	Ldft_out = next_even(2*(Ldft_out-Ldft_out%2)) # time-domain output data will be closer to critically sampled

	print('Derived settings : actual extraction range %.3f--%.3f MHz, %d-point DFT bins %d--%d (%d bins) padded to %d-point IDFT\n' % (start_MHz,stop_MHz,Ldft_in,start_bin,stop_bin,Ldft_copy,Ldft_out))

	# Safety checks
	if (Ldft_in<Ldft_out) or (start_bin>stop_bin) or (stop_bin>=Ldft_in):
		print ('Error: invalid derived settings')
		return 1
	if (Ldft_in % factor)>0:
		print ('Error: length of input-side DFT (%u) must be divisible by overlap-add factor (quality factor %u)' % (Ldft_in,factor))
		return 1
	if (Ldft_out %  factor)>0:
		print ('Error: length derived for output-side IDFT (%u) does not divide the overlap-add factor (quality factor %u)' % (Ldft_out,factor))
		return 1

	# Get storage for raw sample data from m5lib.mark5_stream_decode()
	pdata = m5lib.helpers.make_decoder_array(ms, Ldft_in, dtype=m5_t)
	if_data = ctypes.cast(pdata[if_nr], ctypes.POINTER(m5_t*Ldft_in))

	# Numpy 2D arrays for processed data
	flt_in  = numpy.zeros(shape=(factor,Ldft_in), dtype=fp_t)
	flt_out = numpy.zeros(shape=(factor,Ldft_out), dtype=cp_t)
	iconcat = numpy.array([0.0 for x in range(2*Ldft_in)], dtype=fp_t)
	oconcat = numpy.array([0.0+0.0j for x in range(2*Ldft_out)], dtype=cp_t)

	# Coefficient for coherent phase connection between overlapped input segments
	r       = float(start_bin)/float(factor)
	rfrac   = r - numpy.floor(r)
	rot_f0  = numpy.exp(2j*numpy.pi*rfrac)
	if (abs(numpy.imag(rot_f0)) < 1e-5):
		# set near-zero values to zero
		rot_f0 = numpy.real(rot_f0) + 0.0j
	rot_f = rot_f0**0.0

	# Window functions for DFT and IDFT
	win_in  = numpy.cos((numpy.pi/Ldft_in)*(numpy.linspace(0,Ldft_in-1,Ldft_in) - 0.5*(Ldft_in-1)))
	win_in  = numpy.resize(win_in.astype(fp_t), new_shape=(factor,Ldft_in))
	win_out = numpy.cos((numpy.pi/Ldft_out)*(numpy.linspace(0,Ldft_out-1,Ldft_out) - 0.5*(Ldft_out-1)))
	win_out = numpy.resize(win_out.astype(fp_t), new_shape=(factor,Ldft_out))

	# Prepare VDIF output file with reduced data rate and same starting timestamp
	bwout   = float(dms.samprate)*(Ldft_copy/float(Ldft_in))
        fsout   = 2*bwout
	outMbps = fsout*1e-6 * nbits_out
	vdiffmt = 'VDIF_8192-%u-1-%d' % (outMbps,nbits_out)
	#if not(int(outMbps) == outMbps):
	#	print ('*** Warning: output rate is non-integer (%e Ms/s)! ***' % (outMbps))

	(vdifref,vdifsec) = m5lib.helpers.get_VDIF_time_from_MJD(mjd,sec+1e-9*ns)

	vdif = m5lib.writers.VDIFEncapsulator()
	vdif.open(fout, format=vdiffmt, complex=False, station='SB')
	vdif.set_time(vdifref,vdifsec, framenr=0)
	vdiffmt = vdif.get_format()

	# Report
	bw = float(dms.samprate)*0.5
	print ('Input file   : start MJD %u/%.6f sec' % (mjd,sec+ns*1e-9))
	print ('Bandwidth    : %u kHz in, %.2f kHz out, bandwidth reduction of ~%.2f:1' % (1e-3*bw, Ldft_copy*1e-3*bw/Ldft_in, float(Ldft_in)/Ldft_copy))
	print ('Input side   : %u-point DFT with %u bins (%u...%u) extracted' % (Ldft_in,Ldft_copy,start_bin,stop_bin))
	print ('Output side  : %u-point IDFT with %u-point zero padding' % (Ldft_out,Ldft_out-Ldft_copy))
	print ('Overlap      : %u samples on input, %u samples on output' % (Ldft_in-Ldft_in/factor,Ldft_out-Ldft_out/factor))
	print ('Phasors      : %s^t : %s ...' % (str(rot_f0), str([rot_f0**t for t in range(factor+2)])))
	print ('Output file  : rate %.3f Mbps, %u fps, format %s' 
		% (outMbps,vdif.get_fps(),vdif.get_format()) )

	# Do filtering
	print ('Filtering...')
	iter = 0
	while True:

		# Get next full slice of data
		rc = m5lib.mark5_stream_decode(ms, Ldft_in, pdata)
		if (rc < 0):
			print ('\n<EOF> status=%d' % (rc))
			return 0
		in_new = numpy.frombuffer(if_data.contents, dtype='float32')

		# Debug: replace data with noise + tone
		if False:
			t = iter*Ldft_in + numpy.array(range(Ldft_in))
			f = (start_bin + numpy.floor(Ldft_copy/2.0)) / float(Ldft_in)
			in_new = numpy.random.standard_normal(size=in_new.size) + 10*numpy.sin(2*numpy.pi * f*t)
			in_new = in_new.astype('float32')

		# Feed the window-overlap input-side DFT processing input stage
		iconcat = numpy.concatenate([iconcat[0:Ldft_in],in_new]) # [old,new]
		for ii in range(factor):
			iconcat = numpy.roll(iconcat, -Ldft_in/factor)
			flt_in[ii] = iconcat[0:Ldft_in]
	
		# Window and do 1D DFT of 2D array
		flt_in = numpy.multiply(flt_in,win_in)
		F = numpy.fft.fft(flt_in)

		# Copy the desired bins and fix DC/Nyquist bins
		for ii in range(factor):
			flt_out[ii][0:Ldft_copy] = F[ii][start_bin:(start_bin+Ldft_copy)]
			flt_out[ii][0] = 0.0 # numpy.real(flt_out[ii][0])
			flt_out[ii][Ldft_copy-1] = 0.0 # numpy.real(flt_out[ii][Ldft_copy-1])

		# Do inverse 1D DFT of 2D array and window the result
		F = numpy.fft.ifft(flt_out)
		F = numpy.multiply(F,win_out)

		# Reconstruct time domain signal by shifting and stacking overlapped segments coherently
		for ii in range(factor):
			oconcat[Ldft_out:] = oconcat[Ldft_out:] + F[ii]*rot_f
			rot_f = rot_f * rot_f0
			oconcat = numpy.roll(oconcat, -Ldft_out/factor)
			# note: numpy has a circular shift (numpy.roll), but no "shift array left/right" function,
			# so we need to zero out the undesired values shifted back in by the circular shift:
			oconcat[(-Ldft_out/factor):] = 0

		# Output real part of complex time domain data
		# (If suppression of upper Nyquist is zone desired, should write out both real&imag)
		if not stats_done:
			out_std = numpy.std(numpy.real(oconcat[0:Ldft_out]))
			stats_done = True
		if nbits_out==32:
			raw = numpy.real(oconcat[0:Ldft_out]).view('float32').tostring()
			vdif.write(raw)
		elif nbits_out==8:
			a = numpy.real(oconcat[0:Ldft_out]) * (127.5/(8*out_std))
			a8 = a.astype(numpy.int8)
			vdif.write(a8.view('int8').tostring())
		elif nbits_out==2:
			# TODO: add actual conversion to 4-level (first float to int8, then divide by some integer?)
			# TODO: add some "fast" means of packing four 2-bit samples into a byte in Python
			# TODO: ensure Ldft_out * 2-bit is still an 8-byte multiple to meet VDIF requirements
			pass

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
	if len(argv) not in [8,9]:
		usage()
		sys.exit(1)

	offset = 0
	if len(argv) == 9:
		offset = int(argv[8])

	if_nr  = int(argv[4])-1
	factor = int(argv[5])
	start_MHz = float(argv[6])
	stop_MHz  = float(argv[7])

	rc = m5subband(argv[1],argv[2],argv[3], if_nr, factor,start_MHz,stop_MHz, offset)

	return rc

if __name__ == "__main__":
	sys.exit(main())
