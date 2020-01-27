#!/usr/bin/python
""" 
m5tone.py ver. 1.1   Jan Wagner  20200127
 
Extracts a single Phase Calibration tone from one channel in raw VLBI data.
Reads the formats supported by the mark5access library.
 
Usage : m5tone.py [--plot] <infile> <dataformat> <outfile>
                  <if_nr> <Tint (s)> <tonefreq (Hz)> <Ldft> [<offset>] [rate Hz]
 
  --plot      plot the tone phase, amplitude, etc, against time
 
  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:
    VLBA1_2-256-8-2
    MKIV1_4-128-2-1
    Mark5B-512-16-2
    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)
 
  <outfile>   output text file for PCal time, amplitude, phase, coherence
  <if_nr>     the IF i.e. baseband channel that contains the tone
  <Tint>      integration time in seconds, for example 0.256
  <tonefreq>  baseband frequency in Hz of the tone, for example 125e3
  <Ldft>      the desired length of the DFT/FFT across the baseband
 
  <offset>    is the byte offset into the file
  <rate Hz>   is a drift rate (Hz) by which to phase rotate the sample
              data prior to complex DFT
"""

import ctypes, numpy, sys
import pylab
import mark5access as m5lib
from datetime import datetime

def usage():
	print (__doc__)

def m5tone(fn, fmt, fout, if_nr, Tint_sec, tonefreq_Hz, Ldft, offset, phaseRateHz=0, doPlot=False, doFast=True):
	"""Extracts a single tone from the desired channel in a VLBI recording"""

	# Open file
	try:
		m5file = m5lib.new_mark5_stream_file(fn, ctypes.c_longlong(offset))
		m5fmt  = m5lib.new_mark5_format_generic_from_string(fmt)
		ms     = m5lib.new_mark5_stream_absorb(m5file, m5fmt)
		dms    = ms.contents
	except:
		print ('Error: problem opening or decoding %s\n' % (fn))
		return 1

	# Get storage for raw sample data from m5lib.mark5_stream_decode()
	pdata   = m5lib.helpers.make_decoder_array(ms, Ldft, dtype=ctypes.c_float)
	if_data = ctypes.cast(pdata[if_nr-1], ctypes.POINTER(ctypes.c_float*Ldft))

	# Derived settings
	Lnyq  = numpy.floor(Ldft/2 - 1)
	nint  = numpy.round(float(dms.samprate)*Tint_sec/float(Ldft))
	Tint  = float(nint*Ldft)/float(dms.samprate)
	pcbin = Ldft*float(tonefreq_Hz)/float(dms.samprate)
	drift_phase0 = 0
	iter  = 0

	# Safety checks
	if (if_nr<1) or (if_nr>dms.nchan):
		print ('Error: requested nonexistent channel %d (file has channels 1...%d).' % (if_nr,dms.nchan))
		return 1
	if (tonefreq_Hz <= 0) or (tonefreq_Hz >= 0.5*dms.samprate):
		print ('Error: tone frequency of %u Hz not within channel bandwidth of %.1f Hz.' % (tonefreq_Hz,0.5*dms.samprate))
		return 1
	if (Ldft < 16):
		print ('Error: length of DFT (Ldft=%d) must be >=16 points.' % (Ldft))
		return 1
	if (pcbin != int(pcbin)):
		print ('Error: tone bin is not an integer (bin=%f). Adjust Ldft or frequency.' % (pcbin))
		return 1

	# Spectral data
	winf  = numpy.kaiser(Ldft, 7.0)                      # Kaiser window function
	spec  = numpy.zeros(shape=(Ldft), dtype='complex64') # Accumulated spectrum
	tavg  = numpy.zeros(shape=(Ldft), dtype='float32')   # Accumulated time domain data
	pcamp = 0.0                                          # Total abs amplitude
	history = {'amp':[], 'phase':[], 'coh':[], 'T':[], 'MJD':[]}

	# Plotting
	Lsgram = 8
	specgram = numpy.zeros(shape=(Lsgram,Lnyq), dtype='complex64')
	if doPlot:
		pylab.ion()
		pylab.figure()
		pylab.gcf().set_facecolor('white')

	# Report
	print ('Tone at %.3f kHz in the %.3f MHz wide band lands in %.3f kHz-wide bin %u.' 
		% (tonefreq_Hz*1e-3,dms.samprate*0.5e-6,1e-3*dms.samprate/float(Ldft),pcbin))
	print ('Integrating for %.2f milliseconds with %u spectra per integration.' % (Tint*1e3,nint))
	if doFast:
		print ("Note: Using fast ingration, coherence 'r' will not actually be calculated.")

	# Detect tone phase and amplitude
	(mjd0,sec0,ns0) = m5lib.helpers.get_frame_time(ms)
	while True:

		# Get next full slice of data
		rc = m5lib.mark5_stream_decode(ms, Ldft, pdata)
		if (rc < 0):
			print ('\n<EOF> status=%d' % (rc))
			return 0
		dd = numpy.frombuffer(if_data.contents, dtype='float32')

		# Complex rotate the data if requested
		if not phaseRateHz==0:
			phasestep = 2*numpy.pi * (phaseRateHz / float(dms.samprate))
			rotator = numpy.exp(1j * (drift_phase0 + phasestep*numpy.linspace(0, Ldft-1, Ldft)))
			dd = numpy.multiply(dd, rotator)
			drift_phase0 = numpy.unwrap([drift_phase0 + Ldft*phasestep])[0]

		# Extract the tone
		if not(doFast):
			# Brute force method, benefit is that coherence can be measured
			ddwin = numpy.multiply(dd,winf)
			F     = numpy.fft.fft(ddwin)
			spec  = numpy.add(spec,F)
			pcamp = pcamp + numpy.abs(F[pcbin])
		else:
			# Faster method, but cannot measure coherence
			tavg = numpy.add(tavg,dd)

		# Report the result at end of each averaging period
		iter = iter + 1
		print ('%d/%d  \r' % (iter, nint)), 
		if (iter % nint)==0:

			# Timestamp at mid-point of integration
			T_count = Tint * ((iter/nint) - 0.5) # data-second, relative time
			(mjd1,sec1,ns1) = m5lib.helpers.get_sample_time(ms)
			T_stamp = mjd1 + (sec1 + ns1*1e-9 - Tint/2.0)/86400.0 # fractional MJD, absolute time

			# Extract final tone amp, phase, coherence
			if doFast:
				spec  = numpy.fft.fft(numpy.multiply(tavg,winf))
			pctone    = spec[pcbin] / float(nint*Ldft)
			pcamp     = pcamp / float(nint*Ldft)
			pctone_A  = numpy.abs(pctone)
			pctone_ph = numpy.angle(pctone,deg=True)
			if doFast:
				pctone_C = 1.0
			else:
				pctone_C = pctone_A/pcamp

			# Store results
			print ('%.9f mjd : %.6f sec : %e /_ %+.2f deg : r=%.3f' % (T_stamp, T_count, pctone_A,pctone_ph,pctone_C))
			history['amp'].append(pctone_A)
			history['phase'].append(pctone_ph)
			history['coh'].append(pctone_C)
			history['T'].append(T_count)
			history['MJD'].append(T_stamp)
			line = '%.9f %.6f %e %+.2f %.3f\n' % (T_stamp, T_count, pctone_A, pctone_ph, pctone_C)
			fout.write(line)

			# Plotting
			if doPlot:
				specgram[1:] = specgram[0:-1]
				specgram[0] = numpy.abs(spec[0:Lnyq])
			if doPlot and ((iter/nint) % Lsgram) == 0:
				pylab.clf()
				pylab.subplot(311)
				pylab.plot(history['T'],history['amp'],'rx')
				# pylab.plot(numpy.real(pcvec))
				pylab.xlabel('Time (s)')
				pylab.ylabel('Amplitude')

				pylab.subplot(312)
				pylab.plot(history['T'],history['phase'],'gx')
				pylab.ylim(-180.0,180.0)
				pylab.xlabel('Time (s)')
				pylab.ylabel('Phase (deg)')

				pylab.subplot(313)
				xyextent = [0, 0.5e-6*dms.samprate, T_count*1e3, (T_count+Lsgram)*Tint*1e3]
				pylab.imshow(numpy.abs(specgram),aspect='auto',extent=xyextent)
				Fpeak = 1e-3*dms.samprate*float(numpy.argmax(specgram[0]))/Ldft
				pylab.text(Fpeak,Lsgram/2, 'Peak at %.3f kHz' % (Fpeak))
				pylab.xlabel('Frequency (MHz)')
				pylab.ylabel('Time (ms)')
				pylab.draw() # non-blocking
				pylab.draw()

			# Clear accumulated values
			spec  = numpy.zeros_like(spec)
			tavg  = numpy.zeros_like(tavg)
			pcamp = 0.0

	return 0

def main(argv=sys.argv):
	doPlot = False
	doFast = False # False to measure also tone coherence, True to skip coherence measurement
	offset = 0
	rate = 0

	if len(argv) not in [8,9,10]:
		usage()
		sys.exit(1)

	if (argv[1] == '--plot'):
		doPlot = True
		argv = argv[1:]
	if len(argv) == 9:
		offset = int(argv[8])
	if len(argv) == 10:
		rate = float(argv[9])

	fout  = open(argv[3], 'wb', 1)
	if_nr = int(argv[4])
	Tint  = float(argv[5])
	tfreq = float(argv[6])
	Ldft  = int(argv[7])
	rc = m5tone(argv[1],argv[2], fout, if_nr, Tint,tfreq,Ldft, offset, phaseRateHz=rate, doPlot=doPlot, doFast=doFast)
	fout.close()

	if doPlot and (rc == 0):
		try:
			pylab.show() # call blocks, shows last plot until user closes it
		except:
			pass

	return rc

if __name__ == "__main__":
	sys.exit(main())
