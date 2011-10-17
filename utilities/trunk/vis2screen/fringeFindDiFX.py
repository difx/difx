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

if inputfile == "":
    parser.error("You must supply an input file!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
if numfreqs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly")

chans = []
amp = []
phase = []
vis = []
lag = []
lagamp = []
lagampsum = []
for j in range(maxchannels):
    amp.append(0.0)
    phase.append(0.0)
    vis.append(0.0)
    lag.append(0.0)
    lagamp.append(0.0)
    lagampsum.append(0.0)
    
for i in range(maxchannels):
    chans.append(i)

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
    if nchan > maxchannels:
        print "How embarrassing - you have tried to read files with more than " + \
            str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
        sys.exit()
    buffer    = difxinput.read(8*nchan)
    for j in range(nchan):
        cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
        vis[j] = complex(cvis[0], cvis[1])
        amp[j] = math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1])
        phase[j] = math.atan2(cvis[1], cvis[0])*180.0/math.pi
    if (targetbaseline < 0 or targetbaseline == baseline) and \
       (targetfreq < 0 or targetfreq == freqindex):
        lag = fft.ifft(vis, nchan)
        for j in range(nchan/2):
            lagamp[j+nchan/2] = abs(lag[j])
        for j in range(nchan/2):
            lagamp[j] = abs(lag[j+nchan/2])
            lagampsum[j] += lagamp[j]
        maxlagamp = numpy.max(lagamp[:nchan])
        lagamprms = numpy.std(lagamp[:nchan])
        maxlagchan = numpy.argmax(lagamp[:nchan])
        if maxlagamp/lagamprms > minsnr:
            print "Detection on time %d:%f, baseline %d, freq index %d, pol %s on channel %d at S/N: %f" % \
                  (mjd, seconds, baseline, freqindex, polpair, maxlagchan - nchan/2, maxlagamp/lagamprms)
    nextheader = parseDiFX.parse_output_header(difxinput)

if nchan < 0:
    print "Problem opening file " + args[0]
    sys.exit()

maxlagamp = numpy.max(lagampsum[:nchan])
lagamprms = numpy.std(lagampsum[:nchan])
maxlagchan = numpy.argmax(lagampsum[:nchan])
if maxlagamp/lagamprms > minsnr:
    print "Detection on SUM at channel %d with S/N: %f" % (maxlagchan - nchan/2, maxlagamp/lagamprms)
