#!/usr/bin/python
import matplotlib
matplotlib.use('Agg')
import sys, os, struct, time, pylab, math, numpy
import parseDiFX
from optparse import OptionParser
from matplotlib.ticker import FuncFormatter, MaxNLocator, NullFormatter
from numpy import fft

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
helpstr = "plotDynamicSpectrum.py [options] <difx file 1>\n\n"
helpstr += "Makes a dynamic spectrum from visibility output, either a single baseline or scalar averaged"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="targetfreq", default="-1",
                  help="Only display visibilities from this frequency index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="targetbaseline", default="-1",
                  help="Only display visibilities from this baseline num")
parser.add_option("-p", "--polpair", dest="polpair", default="[RR,LL,RL,LR]",
                  help="Plot this polarisations only e.g. RR, LL, RL, LR, default [RR,LL,RL,LR]")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
                  default="33000",
                  help="The length of the array that will be allocated to hold vis results")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq")
parser.add_option("--toscreen", dest="toscreen", default=False, action="store_true",
                  help="Plot to the screen, otherwise to png files")
parser.add_option("--logamp", dest="logamp", default=False, action="store_true",
                  help="Take the log of amplitudes, for a flatter scaling")
parser.add_option("--maxtimestep", dest="maxtimestep", default="-1",
                  help="Max timestep number, if you want to limit the range")
parser.add_option("--chanrange", dest="chanrange", default="-1,-1",
                  help="Channel range to plot, in form min,max (-1,-1 for all)")
parser.add_option("--secondswindow", dest="secondswindow", default="-1,-1",
                  help="Time range to plot, in form min,max (-1,-1 for all)")
parser.add_option("--scrunchbaselines", dest="scrunchbaselines", action="store_true",
                  help="Scalar add all baseline amplitudes")
parser.add_option("--scrunchautocorrs", dest="scrunchautocorrs", action="store_true",
                  help="Scalar add all autocorrelation amplitudes")
parser.add_option("--showlegend", dest="showlegend", default=False, action="store_true",
                  help="Show a legend on the plot")
(options, args) = parser.parse_args()

if len(args) != 1:
    parser.error("You must supply one DiFX output file!")

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
targetpolpair  = options.polpair
maxchannels    = int(options.maxchannels)
maxtimestep    = int(options.maxtimestep)
verbose        = options.verbose
inputfile      = options.inputfile
toscreen       = options.toscreen
logamp         = options.logamp
showlegend     = options.showlegend
scrunchbaselines= options.scrunchbaselines
scrunchautocorrs= options.scrunchautocorrs
chanrange      = options.chanrange.split(',')
secondswindow  = options.secondswindow.split(',')
print "Target baseline: %d\nTarget freq: %d\nTarget pol: %s" % (targetbaseline,targetfreq,targetpolpair)

if len(chanrange) != 2:
    parser.error("Channel range must be in form min,max")
chanrange[0] = int(chanrange[0])
chanrange[1] = int(chanrange[1])
secondswindow[0] = int(secondswindow[0])
secondswindow[1] = int(secondswindow[1])

if inputfile == "":
    parser.error("You must supply an input file!")

if targetbaseline < 0 and not (scrunchbaselines or scrunchautocorrs):
    parser.error("If you want all baselines, they must be scrunched (--scrunchbaselines or --scrunchautocorrs)")

if scrunchbaselines and targetbaseline >= 0:
    parser.error("If you scrunch baselines, you must allow all of them (targetbaseline < 0)")

if scrunchbaselines and scrunchautocorrs:
    parser.error("You can only scrunch baselines, or autocorrs, not both!")

if scrunchautocorrs and targetbaseline >= 0:
    parser.error("If you scrunch autocorrs, you must allow all of them (targetbaseline < 0)")

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
    baseline  = int(nextheader[0])
    mjd       = nextheader[1]
    seconds   = int(nextheader[2])
    freqindex = int(nextheader[5])
    polpair   = nextheader[6]
    nchan     = freqs[freqindex].numchan/freqs[freqindex].specavg
    if nchan >= maxchannels:
        print "How embarrassing - you have tried to diff files with more than " + \
            str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
        sys.exit()
    else:
	pass
        #print "Got visibility for baseline %d freq %d pol %s with %d channels, time %d" \
        #      % (baseline, freqindex, polpair, nchan, seconds)
    if seconds != lastseconds:
        for i in range(numfreqs):
	    for j in range(nchan):
	        amp[i][j].append(0.0)
		phase[i][j].append(0.0)
        lastseconds = seconds
	if (seconds > secondswindow[0] or secondswindow[0] < 0) and \
	   (seconds < secondswindow[1] or secondswindow[1] < 0):
    	    vislen += 1
        if maxtimestep > 0 and vislen > maxtimestep:
            break
    buffer    = difxinput.read(8*nchan)
    if (targetbaseline < 0 or targetbaseline == baseline) and \
       (targetfreq < 0 or targetfreq == freqindex):
        if (seconds < secondswindow[0] and secondswindow[0] >= 0) or \
           (seconds > secondswindow[1] and secondswindow[1] >= 0):
	    print "Skipping data not in specified time range"
            nextheader = parseDiFX.parse_output_header(difxinput)
            continue
        if scrunchautocorrs and baseline%257 != 0:
	    print "Skipping non-autocorrelation data"
            nextheader = parseDiFX.parse_output_header(difxinput)
            continue
	if scrunchbaselines and baseline%257 == 0:
	    print "Skipping autocorrelation data"
	    nextheader = parseDiFX.parse_output_header(difxinput)
	    continue
	if not(polpair in targetpolpair):
	    print "Skipping polpair %s not in specified target pairs %s" % (polpair,str(targetpolpair))
	    nextheader = parseDiFX.parse_output_header(difxinput)
	    continue
        savednchan = nchan
        for j in range(nchan):
            cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
            amp[freqindex][j][-1] = math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1])
            phase[freqindex][j][-1] = math.atan2(cvis[1], cvis[0])*180.0/math.pi
    nextheader = parseDiFX.parse_output_header(difxinput)

if chanrange[0] < 0:
    chanrange[0] = 0
if chanrange[1] < 0:
    chanrange[1] = savednchan

if savednchan <= 0:
    print "Found no data for plotting"

if savednchan > 0:
    if targetfreq >= 0: # We want amplitude and phase from just one freq
        offsetfreq = freqs[targetfreq].freq
        chanwidth = freqs[targetfreq].bandwidth / (freqs[freqindex].numchan/freqs[freqindex].specavg)
        numpyamp = numpy.zeros((chanrange[1]-chanrange[0], vislen), numpy.float32)
        numpyphase = numpy.zeros((chanrange[1]-chanrange[0], vislen), numpy.float32)
        for i in range(chanrange[1]-chanrange[0]):
            for j in range(vislen):
	        numpyamp[i][j] = amp[0][i+chanrange[0]][j]
	        numpyphase[i][j] = phase[0][i+chanrange[0]][j]
            if logamp:
                numpyamp[i] = numpy.log10(numpyamp[i])
	ax = pylab.subplot(2,1,1)
	xformatter = FuncFormatter(xindex2ms)
        yformatter = FuncFormatter(make_yindex2MHz(offsetfreq))
        if logamp:
            pylab.title("Amplitude (log10 correlation coefficients)")
        else:
            pylab.title("Amplitude (correlation coefficients)")
        pylab.ylabel("Freq (MHz)")
	ax.xaxis.set_major_formatter(xformatter)
        ax.yaxis.set_major_formatter(yformatter)
        stddev = numpy.std(numpyamp, dtype=numpy.float64)
        print "Std deviation of the amplitudes was %.6f" % stddev
        pylab.imshow(numpyamp, aspect='auto', origin='lower')
        if showlegend:
            matplotlib.pyplot.colorbar(ax=ax)
	ax = pylab.subplot(2,1,2)
	pylab.title("Phase")
        pylab.xlabel("Time (s)")
        pylab.ylabel("Freq (MHz)")
        ax.xaxis.set_major_formatter(xformatter)
        ax.yaxis.set_major_formatter(yformatter)
        pylab.imshow(numpyphase, aspect='auto', origin='lower')
        if showlegend:
            matplotlib.pyplot.colorbar(ax=ax)
	if targetbaseline < 0:
	    if toscreen:
	       pylab.show()
	    else:
               pylab.savefig("dynamicspectra.bscrunch.f%d.%s.png" % (targetfreq,targetpolpair), format="png")
	else:
	    if toscreen:
	       pylab.show()
	    else:
	       pylab.savefig("dynamicspectra.b%d.f%d.%s.png" % (targetbaseline, targetfreq, targetpolpair), format="png")
    else: # Want to display all freqs, one after another
        pylab.figure(figsize=(15,9))
        pylab.suptitle('All frequencies for %s' % inputfile)
        freqvals = [f.freq for f in freqs]
        # Sort by freq, then reverse the order so highest is first
        sortedfreqinds = numpy.argsort(freqvals)[::-1]
        for i in range(numfreqs):
            f = freqs[sortedfreqinds[i]]
            a = amp[sortedfreqinds[i]]
            offsetfreq = f.freq
            chanwidth = f.bandwidth / (f.numchan/f.specavg)
	    numpyamp = numpy.zeros((chanrange[1]-chanrange[0], vislen), numpy.float32)
	    for j in range(chanrange[1]-chanrange[0]):
	        for k in range(vislen):
		    numpyamp[j][k] = a[j+chanrange[0]][k]
                if logamp:
                    numpyamp[i] = numpy.log10(numpyamp[i])
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
            if showlegend:
                matplotlib.pyplot.colorbar(ax=ax)
	if targetbaseline < 0:
	    if toscreen:
	       pylab.show()
	    else:
	       pylab.savefig("dynamicspectra.bscrunch.png", format="png")
	else:
	    if toscreen:
	       pylab.show()
	    else:
	       pylab.savefig("dynamicspectra.b%d.png" % (targetbaseline), format="png")
else:
    print "Didn't find any matching visibilities!"
