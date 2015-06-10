#!/usr/bin/python
import sys, os, struct, time, math
import parseDiFX
from optparse import OptionParser
from numpy import fft
import numpy

helpstr = "fringeFindDiFX.py [options] <difx file 1>\n\n"
helpstr += "Searches for fringes in DiFX output files"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="targetfreq", default="-1",
                  help="Only display visibilities from this frequency index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="targetbaseline", default="-1",
                  help="Only display visibilities from this baseline num")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
                  default="16384",
                  help="The length of the array that will be allocated to hold vis results")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq")
parser.add_option("-Z", "--nozero", dest="nozero", action="store_true", default=False,
                  help="Do not show visibilities on zero-baselines (e.g., exclude cross-pol autocorrs)")
parser.add_option("-z", "--zero", dest="zero", action="store_true", default=False,
                  help="Show only visibilities on zero-baselines (e.g., cross-pol autocorrs)")
parser.add_option("--minsnr", dest="minsnr", default="6.5", help="Minimum S/N to print detection")
(options, args) = parser.parse_args()

if len(args) != 1:
    parser.error("You must supply one DiFX output file!")

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
maxchannels    = int(options.maxchannels)
verbose        = options.verbose
inputfile      = options.inputfile
minsnr         = float(options.minsnr)
nozero         = options.nozero
dozero         = options.zero

if inputfile == "":
    parser.error("You must supply an input file!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
if numfreqs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly")

lagampsum = numpy.zeros(maxchannels)

difxinput = open(args[0])
nextheader = parseDiFX.parse_output_header(difxinput)

nchan = -1
while not len(nextheader) == 0:
    baseline  = nextheader[0]
    mjd       = nextheader[1]
    seconds   = nextheader[2]
    freqindex = nextheader[5]
    polpair   = nextheader[6]
    nchan     = freqs[freqindex].numchan/freqs[freqindex].specavg
    ant1      = baseline % 256
    ant2      = (baseline-ant1)/256
    if nchan > maxchannels:
        print "How embarrassing - you have tried to read files with more than " + \
            str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
        sys.exit()
    vis = numpy.fromstring(difxinput.read(8*nchan), dtype='complex64')
    amp = numpy.abs(vis)
    phase = numpy.angle(vis, deg=True)
    if (targetbaseline < 0 or targetbaseline == baseline) and \
       not (nozero and (ant1 == ant2)) and \
       not (dozero and (ant1 != ant2)) and \
       (targetfreq < 0 or targetfreq == freqindex):
        lag = fft.ifft(vis, nchan)
        lagamp = fft.fftshift(abs(lag))
        lagampsum[0:nchan] = numpy.add(lagampsum[0:nchan], lagamp)

        maxlagamp = numpy.max(lagamp[:nchan])
        lagamprms = numpy.std(lagamp[:nchan])
        maxlagchan = numpy.argmax(lagamp[:nchan])
        if maxlagamp/lagamprms > minsnr:
            print "Detection on time %d:%f, baseline %d (%d-%d), freq index %d, pol %s on channel %d at S/N: %f" % \
                  (mjd, seconds, baseline, ant2, ant1, freqindex, polpair, maxlagchan - nchan/2, maxlagamp/lagamprms)
    nextheader = parseDiFX.parse_output_header(difxinput)

if nchan < 0:
    print "Problem opening file " + args[0]
    sys.exit()

maxlagamp = numpy.max(lagampsum[:nchan])
lagamprms = numpy.std(lagampsum[:nchan])
maxlagchan = numpy.argmax(lagampsum[:nchan])
if maxlagamp/lagamprms > minsnr:
    print "Detection on SUM at channel %d with S/N: %f" % (maxlagchan - nchan/2, maxlagamp/lagamprms)
