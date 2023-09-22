#!/usr/bin/env python
#
# Reads data from two files and cross correlates them.
# File 1 is read once for Nint*Nfft samples per channel.
# File 2 is read sequentially. Each Nint*Nfft piece is correlated
# against the first chunk of data in File 1.
#
# Cross correlation uses no sampling clock delay modesl.
#

_show_plot = True

import sys, string, os, struct
import pylibMk5B

if not(_show_plot):
	# use a non-GUI backend if no window plotting to be done
	import matplotlib
	matplotlib.use('agg')
import pylab

import numpy.fft
import gc

#import cmath
import math
def phase(c):
	return math.atan2(c.imag, c.real)


def main(argv):
	Nfft = 3
	Lfft = 128*1024
	channel = 7

	if len(sys.argv)<3:
		print 'Usage: %s <file1.m5b> <file2.m5b> [<ch 0..15>] [<num FFT>] [<len FFT>]' % (sys.argv[0])
		print 'Only one numFFT*lenFFT segment is read from file 1, once.'
		print 'This segment is correlated against each segment read from file 2.'
		sys.exit()

	fname1 = sys.argv[1]
	fname2 = sys.argv[2]
        try:
		fd1 = open(fname1, 'r')
	except:
		print '\nCould not open file %s' % (fname1)
		sys.exit(-1)
        try:
		fd2 = open(fname2, 'r')
	except:
		print '\nCould not open file %s' % (fname2)
		sys.exit(-1)
	if len(sys.argv)>=4:
		channel = int(sys.argv[3])
	if len(sys.argv)>=5:
		Nfft = int(sys.argv[4])
	if len(sys.argv)>=6:
		Lfft = int(sys.argv[5])

	fdout = open('delay.bin', 'w')

	LfftC = int(Lfft/2)-1
	Tplot = (Nfft*Lfft)/32e6
	print 'Using: Lfft=%u Nfft=%u  =>  each plot corresponds to %.2e seconds (%.2e plots/sec)' % (Lfft,Nfft,Tplot,1.0/Tplot)

	data1freqdom = []
	print 'Reading file %s' % (fname1)
	Fnorm = float(Nfft)*float(Lfft-1)*float(Lfft-1)
	for ii in range(0,Nfft):
		print 'File 1: Channel %u FFT %u/%u' % (channel,ii+1,Nfft)
		(chdata,offtset) = pylibMk5B.decodeData(fd1, num_samples=Lfft, channel=channel)
		if (chdata==None):
			sys.exit('EOF before having read enough data')
		# to improve speed of cross-correlation: FFT this data, conjugate, normalize
		chdata = numpy.fft.fft(chdata)
		chdata = [(chdata[nn].conjugate()/Fnorm) for nn in range(0,LfftC)]
		data1freqdom.append(chdata)
	fd1.close()
	print 'File done!'

	xxplot = [16.0*float(ii)/LfftC for ii in range(0,LfftC)]
	pylab.figure(figsize=[12.0,12.0])
	Nplots = 0

	while True:

		print 'Reading file %s and averaging cross correlations... ' % (fname2)
		offset_begin = 0
		xcorr = [0.0 for jj in range(0,LfftC)]
		for ii in range(0,Nfft):
			print 'File 2: Channel %u FFT %u/%u' % (channel,ii+1,Nfft)
			(chdata,offset) = pylibMk5B.decodeData(fd2, num_samples=Lfft, channel=channel)
			if (chdata==None):
				break
			if (ii==0):
				offset_begin = offset

			d1 = data1freqdom[ii]
			d2 = numpy.fft.fft(chdata)
			xcorr = [xcorr[nn] + d2[nn]*d1[nn]/float(Nfft) for nn in range(0,LfftC)]

		print 'File done!'

		titlestr = 'Ch#%02u %s@0 X %s@%u (%ux%u-pt FFT)' % (channel,fname1,fname2,offset_begin,Nfft,Lfft)
		xcabs = [abs(xcorr[jj]) for jj in range(0,LfftC)]
		xcpha = [phase(xcorr[jj]) for jj in range(0,LfftC)]
		# xcpha = numpy.unwrap(xcpha)

		if True:
			print 'Appending to complex-float binary file...'
			for nn in range(0,LfftC):
				fdout.write(struct.pack('ff', xcorr[nn].real, xcorr[nn].imag))

		plotfile = 'xcorr5b_Lfft%u_Nfft%u_plot_%u.png' % (Lfft,Nfft,Nplots)
		print 'Generating plot file %s' % plotfile
	
		if True:
			pylab.clf()
			pylab.subplot(2,1,1)
			pylab.semilogy(xxplot,xcabs)
			pylab.title(titlestr)
			pylab.xlabel('MHz')
			pylab.ylabel('xc')
			pylab.subplot(2,1,2)
			pylab.plot(xxplot,xcpha)
			pylab.ylabel('phase [rad]')
			pylab.xlabel('MHz')

			pylab.savefig(plotfile, format='png')

			if _show_plot:
				print 'Plot of T0 vs T0+%.2e seconds' % (Nplots*Tplot)
				print 'Close plot window to continue...'
				pylab.show()
			else:
				print 'Plotting disabled in code, saving only PNG file'

		Nplots = Nplots + 1

		print 'Garbage collection...'
		gc.collect()


	fd2.close()
	fdout.close()
	
if __name__ == "__main__":
    main(sys.argv)
