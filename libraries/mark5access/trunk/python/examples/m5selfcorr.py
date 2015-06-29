#!/usr/local/bin/python
"""
m5selfcorr.py ver. 1.0   Jan Wagner  20150619

Produces plots (*.png) of the cross-correlation between all channels (subbands)
of a VLBI recording. This is useful for self zero baseline testing of a backend,
when all separate analog input bands are combined into the same VDIF/Mark5B/other
output file. The current version does not distinguish upper/lower sideband.

Usage: m5selfcorr.py <infile> <dataformat>

  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:
    VLBA1_2-256-8-2
    MKIV1_4-128-2-1
    Mark5B-512-16-2
    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)
"""

import ctypes, numpy, os, sys
import mark5access as m5lib
from datetime import datetime
try:
        import matplotlib as mpl
        mpl.rcParams['path.simplify'] = False # http://stackoverflow.com/questions/15795720/matplotlib-major-display-issue-with-dense-data-sets
except:
        pass
import pylab

def usage():
        print __doc__


def m5selfcorr(fn, fmt):

        Nint = 500
        nfft = 1024
        offset = 0

        # Open file
        try:
                m5file = m5lib.new_mark5_stream_file(fn, ctypes.c_longlong(offset))
                m5fmt  = m5lib.new_mark5_format_generic_from_string(fmt)
                ms     = m5lib.new_mark5_stream_absorb(m5file, m5fmt)
                dms    = ms.contents
        except:
                print ('Error: problem opening or decoding %s using format %s\n' % (fn,fmt))
                return 1
        basename = os.path.basename(fn)
        basename = os.path.splitext(basename)[0]

        # Collection of vectors for mark5access decode() raw sample output data
        pdata = m5lib.helpers.make_decoder_array(ms, nfft, dtype=ctypes.c_float)

        # Result arrays
        ch_data = [ctypes.cast(pdata[ii], ctypes.POINTER(ctypes.c_float*nfft)) for ii in range(dms.nchan)]
        xspecs = numpy.zeros(shape=(dms.nchan*dms.nchan,nfft), dtype='complex128')

        # Process the recorded data
        iter = 0
        while (iter < Nint):
                iter = iter + 1

                # Read data
                rc = m5lib.mark5_stream_decode(ms, nfft, pdata)
                if (rc < 0):
                        print ('\n<EOF> status=%d' % (rc))
                        return 0

                # Calculate normalized cross-correlations
                for ii in range(dms.nchan):
                    for jj in range(dms.nchan):
                        # print ('Computing pair %d--%d...' % (ii,jj))
                        k = ii*dms.nchan + jj
                        a = numpy.frombuffer(ch_data[ii].contents, dtype='float32')
                        b = numpy.frombuffer(ch_data[jj].contents, dtype='float32')
                        A = numpy.fft.fft(a)
                        B = numpy.fft.fft(b)
			xspecs[k] = numpy.add(xspecs[k], A*numpy.conj(B))

        # Calculate lag spectra
        xspecs   /= Nint*nfft
        lagspecs  = numpy.zeros_like(xspecs)
        normspecs = numpy.zeros_like(xspecs)
        for ii in range(dms.nchan):
            for jj in range(dms.nchan):

                k = ii*dms.nchan + jj
                xc = numpy.fft.ifft(xspecs[k])
                lagspecs[k] = numpy.fft.fftshift(xc)

                xc = xspecs[k]
                ac1 = xspecs[ii*dms.nchan + ii]
                ac2 = xspecs[jj*dms.nchan + jj]
                norm = numpy.sqrt(numpy.multiply(ac1,ac2))
                normspecs[k] = numpy.divide(xc, norm)

        figsize = (5*dms.nchan,5*dms.nchan)

        # Show lag spectra
        ymin = numpy.amin(numpy.abs(lagspecs))
        ymax = numpy.amax(numpy.abs(lagspecs))
        pylab.figure(figsize=figsize,dpi=75)
        for ii in range(dms.nchan):
            for jj in range(dms.nchan):
                if (jj < ii):
                    continue
                print ('Plotting lag spec. %d--%d' % (ii,jj))
                if (ii == jj):
                    linespec = 'kx-'
                else:
                    linespec = 'bx-'
                k = ii*dms.nchan + jj
                pylab.subplot(dms.nchan, dms.nchan, k+1)
                pylab.ylim([ymin,ymax])
                pylab.xlim([0,nfft-1])
                pylab.plot(numpy.abs(lagspecs[k]), linespec)
                pylab.title('IF %d x %d' % (ii+1,jj+1))
                if (ii==jj):
                    pylab.xlabel('Lag')
                    pylab.ylabel('Cross-corr.')
        pylab.savefig(basename + '.lag.png')

        # Show normalized xcorr spectra: phase
        pylab.figure(figsize=figsize,dpi=75)
        for ii in range(dms.nchan):
            for jj in range(dms.nchan):
                if (jj < ii):
                    continue
                print ('Plotting xcorr spec. phase %d--%d' % (ii,jj))
                if (ii == jj):
                    linespec = 'kx'
                else:
                    linespec = 'bx'
                k = ii*dms.nchan + jj
                pylab.subplot(dms.nchan, dms.nchan, k+1)
                pylab.plot(numpy.angle(normspecs[k],deg=True), linespec)
                #pylab.ylim([-numpy.pi,+numpy.pi])
                pylab.ylim([-180.0,+180.0])
                pylab.xlim([0,nfft-1])
                pylab.title('IF %d x %d' % (ii+1,jj+1))
                if (ii==jj):
                    pylab.xlabel('FFT bin')
                    pylab.ylabel('Phase (deg)')
        pylab.savefig(basename + '.phase.png')

        # Show normalized xcorr spectra: amp
        ymin = numpy.amin(numpy.abs(normspecs))
        ymax = numpy.amax(numpy.abs(normspecs))
        pylab.figure(figsize=figsize,dpi=75)
        for ii in range(dms.nchan):
            for jj in range(dms.nchan):
                if (jj < ii):
                    continue
                print ('Plotting xcorr spec. amp %d--%d' % (ii,jj))
                if (ii == jj):
                    linespec = 'kx:'
                else:
                    linespec = 'bx:'
                k = ii*dms.nchan + jj
                pylab.subplot(dms.nchan, dms.nchan, k+1)
                pylab.plot(numpy.abs(normspecs[k]), linespec)
                pylab.ylim([ymin,ymax])
                pylab.xlim([0,nfft-1])
                pylab.title('IF %d x %d' % (ii+1,jj+1))
                if (ii==jj):
                    pylab.xlabel('FFT bin')
                    pylab.ylabel('Coherence')
        pylab.savefig(basename + '.coherence.png')
 
        # Show autocorrelations (without normalization)
        pylab.figure(figsize=figsize,dpi=75)
        Nnyq = int(nfft/2+1)
        for ii in range(dms.nchan):
                k = ii*dms.nchan + ii
                print ('Plotting auto power spec. %d' % (ii))
                pylab.subplot(dms.nchan,1, ii+1)
                ac = numpy.abs(xspecs[k])
                pylab.semilogy(ac[0:Nnyq], 'k-')
                pylab.xlim([0,Nnyq-1])
                pylab.title('Auto power spectrum IF %d' % (ii+1))
                if ii==(dms.nchan-1):
                    pylab.xlabel('FFT bin')
        pylab.savefig(basename + '.autospec.png')

        pylab.show()

def main(argv=sys.argv):

        # Args
        if len(argv) not in [3]:
                usage()
                sys.exit(1)

        # Start processing
        rc = m5selfcorr(argv[1],argv[2])

        return rc

if __name__ == "__main__":
        sys.exit(main())

