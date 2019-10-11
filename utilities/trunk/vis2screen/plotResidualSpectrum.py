#!/usr/bin/python
import sys, os, struct, time, pylab, math, numpy
import parseDiFX
from optparse import OptionParser
from matplotlib.ticker import FuncFormatter, MaxNLocator, NullFormatter
from numpy import fft
from numpy import resize

## GLOBAL VARIABLES ##
inttime   = 1.0 #s
chanwidth = 1.0 # MHz
offsetfreq= 0.0 # MHz

## FUNCTIONS #########
def xindex2ms(x, pos):
    return '%1.1f' % (x*inttime)

# Use a lambda function to generate the function we need
def make_yindex2MHz(minfreq):
    return lambda y,pos: '%1.1f' % (minfreq + y*chanwidth)

## OPTIONS PARSING ###
helpstr = "plotResidualSpectrum.py [options] <difx file A> <difx file B>\n\n"
helpstr += "Makes a residual spectrum from two visibility outputs A, B (plotted residual=|B|-|A|,), either a single baseline or scalar averaged. "
helpstr += "Output files must be from identical correlation settings but could be produced by different DiFX versions"
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
parser.add_option("-m", "--maxvis", dest="maxvis", default="0",
                  help="Maximum number of visibilities to plot")
parser.add_option("-x", "--toscreen", dest="toscreen", default=False, action="store_true",
                  help="Plot to the screen, otherwise to png files")
parser.add_option("--scrunchbaselines", dest="scrunchbaselines", action="store_true",
                  help="Scalar add all baseline amplitudes")
(options, args) = parser.parse_args()

if len(args) != 2:
    parser.error("You must supply two DiFX output files!")

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
targetpolpair  = options.polpair
maxchannels    = int(options.maxchannels)
verbose        = options.verbose
inputfile      = options.inputfile
toscreen       = options.toscreen
maxvis         = int(options.maxvis)
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

amp = resize([0.0], (numfreqs, maxchannels));
phase = resize([0.0], (numfreqs,maxchannels));
ampA = resize([0.0], (numfreqs, maxchannels));
phaseA = resize([0.0], (numfreqs,maxchannels));
ampB = resize([0.0], (numfreqs, maxchannels));
phaseB = resize([0.0], (numfreqs,maxchannels));

amp=[]
phase=[]
for i in range(numfreqs):
    amp.append([])
    phase.append([])
    for j in range(maxchannels):
        amp[i].append([])
        phase[i].append([])
    
chans = []
for i in range(maxchannels):
    chans.append(i)

difxinputA = open(args[0])
difxinputB = open(args[1])
nextheaderA = parseDiFX.parse_output_header(difxinputA)
nextheaderB = parseDiFX.parse_output_header(difxinputB)
savednchan = -1
vislen = 0
lastseconds = -1
while not len(nextheaderA) == 0:
    baseline  = nextheaderA[0]
    mjd       = nextheaderA[1]
    seconds   = nextheaderA[2]
    freqindex = nextheaderA[5]
    polpair   = nextheaderA[6]
    nchan     = freqs[freqindex].numchan/freqs[freqindex].specavg
    if nchan > maxchannels:
        print ("How embarrassing - you have tried to read files with more than " + \
            str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!")
        sys.exit()
    if seconds != lastseconds:
        for i in range(numfreqs):
            for j in range(nchan):
                amp[i][j].append(0.0)
                phase[i][j].append(0.0)
        lastseconds = seconds
        vislen += 1
    if (maxvis>0) and (vislen>maxvis):
        break
    bufferA = difxinputA.read(8*nchan)
    bufferB = difxinputB.read(8*nchan)
    if (targetbaseline < 0 or targetbaseline == baseline) and \
       (targetfreq < 0 or targetfreq == freqindex):
        if scrunchbaselines and baseline%257 == 0:
            nextheaderA = parseDiFX.parse_output_header(difxinputA)
            nextheaderB = parseDiFX.parse_output_header(difxinputB)
            continue
        if polpair != targetpolpair:
            nextheaderA = parseDiFX.parse_output_header(difxinputA)
            nextheaderB = parseDiFX.parse_output_header(difxinputB)
            continue
        savednchan = nchan
        ampAmax = 0
        ampBmax = 0
        for j in range(nchan):
            cvisA = struct.unpack("ff", bufferA[8*j:8*(j+1)])
            cvisB = struct.unpack("ff", bufferB[8*j:8*(j+1)])
            ampA[freqindex][j] = math.sqrt(cvisA[0]*cvisA[0] + cvisA[1]*cvisA[1])
            phaseA[freqindex][j] = math.atan2(cvisA[1], cvisA[0])*180.0/math.pi
            ampB[freqindex][j] = math.sqrt(cvisB[0]*cvisB[0] + cvisB[1]*cvisB[1])
            phaseB[freqindex][j] = math.atan2(cvisB[1], cvisB[0])*180.0/math.pi
            ampAmax = max(ampAmax, ampA[freqindex][j])
            ampBmax = max(ampBmax, ampB[freqindex][j])
        for j in range(nchan):
            amp[freqindex][j][-1] = ampAmax*ampB[freqindex][j] - ampBmax*ampA[freqindex][j]
            amp[freqindex][j][-1] = amp[freqindex][j][-1] / (ampAmax*ampBmax)
            phase[freqindex][j][-1] = phaseB[freqindex][j] - phaseA[freqindex][j]
        print ("Found and unpacked a visibility (%d/%d)" % (vislen,maxvis))
    nextheaderA = parseDiFX.parse_output_header(difxinputA)
    nextheaderB = parseDiFX.parse_output_header(difxinputB)

if savednchan > 0:
    if targetfreq >= 0: # We want amplitude and phase from just one freq
        print ("Single-freq plot")
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
        yformatter = FuncFormatter(make_yindex2MHz(offsetfreq))
        pylab.title("Amplitude")
        pylab.ylabel("Freq (MHz)")
        ax.xaxis.set_major_formatter(xformatter)
        ax.yaxis.set_major_formatter(yformatter)
        pylab.imshow(numpyamp, aspect='auto', origin='lower')
        ax = pylab.subplot(2,1,2)
        pylab.title("Phase")
        pylab.xlabel("Time (s)")
        pylab.ylabel("Freq (MHz)")
        ax.xaxis.set_major_formatter(xformatter)
        ax.yaxis.set_major_formatter(yformatter)
        pylab.imshow(numpyphase, aspect='auto', origin='lower')
        if toscreen:
            pylab.show()
        if targetbaseline < 0:
            pylab.savefig("residualspectra.bscrunch.f%d.png" % (targetfreq), format="png")
        else:
            pylab.savefig("residualspectra.b%d.f%d.png" % (targetbaseline, targetfreq), format="png")
    else: # Want to display all freqs, one after another
        pylab.figure(figsize=(15,9))
        pylab.suptitle('All frequencies for %s, delta between %s and %s' % (inputfile,args[1],args[0]))
        freqvals = [f.freq for f in freqs]
        # Sort by freq, then reverse the order so highest is first
        sortedfreqinds = numpy.argsort(freqvals)[::-1]
        for i in range(numfreqs):
            f = freqs[sortedfreqinds[i]]
            a = amp[sortedfreqinds[i]]
            offsetfreq = f.freq
            chanwidth = f.bandwidth / (f.numchan/f.specavg)
            numpyamp = numpy.zeros((savednchan, vislen), numpy.float32)
            for j in range(savednchan):
                for k in range(vislen):
                    numpyamp[j][k] = a[j][k]
            ax = pylab.subplot(numfreqs,1,i+1)
            pylab.subplots_adjust(wspace=0.05, hspace=0.02,
                                  top=0.95, bottom=0.08, left=0.125, right = 0.9)
            # Only show the x axis ticks and labels for the final plot
            if i==numfreqs-1:
                xformatter = FuncFormatter(xindex2ms)
                ax.xaxis.set_major_formatter(xformatter)
                pylab.xlabel("Time (s)")
            else:
                ax.xaxis.set_major_formatter(NullFormatter())
            yformatter = FuncFormatter(make_yindex2MHz(offsetfreq))
            ax.yaxis.set_major_formatter(yformatter)
            # Limit the number of y tick labels to 4
            ax.yaxis.set_major_locator(MaxNLocator(4))
            pylab.ylabel("Freq (MHz)")
            pylab.imshow(numpyamp, aspect='auto', origin='lower')

        if toscreen:
            pylab.show()
        else:
            if targetbaseline < 0:
                pylab.savefig("residualspectra.bscrunch.png", format="png")
            else:
                pylab.savefig("residualspectra.b%d.png" % (targetbaseline), format="png")
