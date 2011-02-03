#!/usr/bin/python
#import matplotlib
#matplotlib.use("pdf")
import sys, os, struct, time, pylab, math, numpy
import parseDiFX
from optparse import OptionParser
from matplotlib.ticker import FuncFormatter
from numpy import fft

## GLOBAL VARIABLES ##
inttime   = 1.0 #s
chanwidth = 1.0 # MHz
offsetfreq= 0.0 # MHz

## FUNCTIONS #########
def xindex2ms(x, pos):
    return '%1.1f' % (x*inttime)

def yindex2MHz(y, pos):
    return '%1.1f' % (offsetfreq + y*chanwidth)

## OPTIONS PARSING ###
helpstr = "plotDynamicSpectrum.py [options] <difx file 1>\n\n"
helpstr += "Makes a dynamic spectrum from visibility output, either a single baseline or scalar averaged"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="targetfreq", default="-1",
                  help="Only display visibilities from this frequency index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="targetbaseline", default="-1",
                  help="Only display visibilities from this baseline num")
parser.add_option("-p", "--polpair", dest="polpair", default="RR",
                  help="Plot this polarisations only e.g. RR, LL, RL, LR def RR")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
                  default="33000",
                  help="The length of the array that will be allocated to hold vis results")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq")
parser.add_option("--toscreen", dest="toscreen", default=False, action="store_true",
                  help="Plot to the screen, otherwise to png files")
parser.add_option("--scrunchbaselines", dest="scrunchbaselines", action="store_true",
                  help="Scalar add all baseline amplitudes")
(options, args) = parser.parse_args()

if len(args) != 1:
    parser.error("You must supply one DiFX output file!")

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
targetpolpair  = options.polpair
maxchannels    = int(options.maxchannels)
verbose        = options.verbose
inputfile      = options.inputfile
toscreen       = options.toscreen
scrunchbaselines= options.scrunchbaselines

if inputfile == "":
    parser.error("You must supply an input file!")

if targetbaseline < 0 and not scrunchbaselines:
    parser.error("If you want all baselines, they must be scrunched (--scrunchbaselines)")

if scrunchbaselines and targetbaseline >= 0:
    parser.error("If you scrunch baselines, you must allow all of them (targetbaseline < 0)")

(numconfigs, configs) = parseDiFX.get_configtable_info(inputfile)
(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)

if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0 or numconfigs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly")

inttime = configs[0].inttime
chans = []
amp = []
phase = []
for i in range(numfreqs):
    amp.append([])
    phase.append([])
    for j in range(maxchannels):
        amp[i].append([])
        phase[i].append([])
    
for i in range(maxchannels):
    chans.append(i)

difxinput = open(args[0])
nextheader = parseDiFX.parse_output_header(difxinput)
savednchan = -1
vislen = 0
lastseconds = -1
while not len(nextheader) == 0:
    baseline  = nextheader[0]
    mjd       = nextheader[1]
    seconds   = nextheader[2]
    freqindex = nextheader[5]
    polpair   = nextheader[6]
    nchan     = freqs[freqindex].numchan/freqs[freqindex].specavg
    if nchan >= maxchannels:
        print "How embarrassing - you have tried to diff files with more than " + \
            str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
        sys.exit()
    if seconds != lastseconds:
        for i in range(numfreqs):
	    for j in range(nchan):
	        amp[i][j].append(0.0)
		phase[i][j].append(0.0)
        lastseconds = seconds
	vislen += 1
    buffer    = difxinput.read(8*nchan)
    if (targetbaseline < 0 or targetbaseline == baseline) and \
       (targetfreq < 0 or targetfreq == freqindex):
	if scrunchbaselines and baseline%257 == 0:
	    nextheader = parseDiFX.parse_output_header(difxinput)
	    continue
	if polpair != targetpolpair:
	    nextheader = parseDiFX.parse_output_header(difxinput)
	    continue
        savednchan = nchan
        for j in range(nchan):
            cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
            amp[freqindex][j][-1] = math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1])
            phase[freqindex][j][-1] = math.atan2(cvis[1], cvis[0])*180.0/math.pi
    nextheader = parseDiFX.parse_output_header(difxinput)

if savednchan > 0:
    if targetfreq >= 0: # We want amplitude and phase from just one freq
        offsetfreq = freqs[targetfreq].freq
        chanwidth = freqs[targetfreq].bandwidth / (freqs[freqindex].numchan/freqs[freqindex].specavg)
        numpyamp = numpy.zeros((savednchan, vislen), numpy.float32)
        numpyphase = numpy.zeros((savednchan, vislen), numpy.float32)
        for i in range(savednchan):
            for j in range(vislen):
	        numpyamp[i][j] = amp[0][i][j]
	        numpyphase[i][j] = phase[0][i][j]
	ax = pylab.subplot(2,1,1)
	xformatter = FuncFormatter(xindex2ms)
        yformatter = FuncFormatter(yindex2MHz)
	pylab.title("Amplitude")
        pylab.ylabel("Freq (MHz)")
	ax.xaxis.set_major_formatter(xformatter)
        ax.yaxis.set_major_formatter(yformatter)
        pylab.imshow(numpyamp, aspect='auto')
	ax = pylab.subplot(2,1,2)
	pylab.title("Phase")
        pylab.xlabel("Time (s)")
        pylab.ylabel("Freq (MHz)")
        ax.xaxis.set_major_formatter(xformatter)
        ax.yaxis.set_major_formatter(yformatter)
        pylab.imshow(numpyphase, aspect='auto')
	if targetbaseline < 0:
            pylab.savefig("dynamicspectra.bscrunch.f%d.png" % (targetfreq), format="png")
	else:
	    pylab.savefig("dynamicspectra.b%d.f%d.png" % (targetbaseline, targetfreq), format="png")
    else: # Want to display all freqs, one after another
        for i in range(numfreqs):
            offsetfreq = freqs[i].freq
            chanwidth = freqs[i].bandwidth / (freqs[i].numchan/freqs[i].specavg)
	    numpyamp = numpy.zeros((savednchan, vislen), numpy.float32)
	    for j in range(savednchan):
	        for k in range(vislen):
		    numpyamp[j][k] = amp[i][j][k]
	    ax = pylab.subplot(numfreqs,1,i+1)
	    xformatter = FuncFormatter(xindex2ms)
	    yformatter = FuncFormatter(yindex2MHz)
            ax.xaxis.set_major_formatter(xformatter)
	    ax.yaxis.set_major_formatter(yformatter)
	    pylab.ylabel("Freq (MHz)")
            if i==numfreqs-1:
	        pylab.xlabel("Time (s)")
	    pylab.imshow(numpyamp, aspect='auto')
	if targetbaseline < 0:
	    pylab.savefig("dynamicspectra.bscrunch.png", format="png")
	else:
	    pylab.savefig("dynamicspectra.b%d.png" % (targetbaseline), format="png")
