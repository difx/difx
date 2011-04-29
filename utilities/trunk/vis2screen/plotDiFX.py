#!/usr/bin/python
import sys, os, struct, time, pylab, math, numpy
import parseDiFX
from optparse import OptionParser
from numpy import fft

helpstr = "plotDiFX.py [options] <difx file 1> <difx file 2> ... [difx file N]\n\n"
helpstr += "Flashes bandpasses of selected bands overlaid"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="targetfreq", default="-1",
                  help="Only display visibilities from this frequency index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="targetbaseline", default="-1",
                  help="Only display visibilities from this baseline num")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
                  default="33000",
                  help="The length of the array that will be allocated to hold vis results")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq")
parser.add_option("--toscreen", dest="toscreen", default=False, action="store_true",
                  help="Plot to the screen, otherwise to png files")
parser.add_option("--singlevis", dest="singlevis", default=False, action="store_true",
                  help="Stop plotting as soon as there is a time change")
(options, args) = parser.parse_args()

if len(args) < 1:
    parser.error("You must supply at least one DiFX output file!")

numfiles = len(args)

targetbaseline = int(options.baseline)
targetfreq = int(options.freq)
maxchannels    = int(options.maxchannels)
verbose        = options.verbose
inputfile      = options.inputfile
toscreen       = options.toscreen
singlevis      = options.singlevis

if inputfile == "":
    parser.error("You must supply an input file!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)

if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly")

chans = []
amp = []
phase = []
vis = []
lag = []
lagamp = []
for i in range(numfiles):
    amp.append([])
    phase.append([])
    vis.append([])
    lag.append([])
    lagamp.append([])
    for j in range(maxchannels):
        amp[i].append(0.0)
        phase[i].append(0.0)
        vis[i].append(0.0)
        lag[i].append(0.0)
        lagamp[i].append(0.0)
    
for i in range(maxchannels):
    chans.append(i)

pylab.xlabel("Channel")
pylab.ylabel("Amplitude")
linestyles = ['b', 'r', 'g', 'k', 'y']
difxinputs  = []
nextheader  = []
mjd         = []
seconds     = []
freqindex   = []
baseline    = []
polpair     = []
nchan       = []
for filename in args:
    difxinputs.append(open(filename))
    freqindex.append(0)
    nchan.append(0)
    baseline.append(0)
    mjd.append(0)
    seconds.append(0.0)
    polpair.append("")
    nextheader.append([])

for i in range(numfiles):
    nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])

if not len(nextheader[0]) == 0:
    startseconds = nextheader[0][2]
while not len(nextheader[0]) == 0:
    print "Looping..."
    for i in range(numfiles):
        snr = 0
        delayoffsetus = 0
        if len(nextheader[i]) == 0:
            nextheader[0] = [] #Will cause an exit
            break
        baseline[i] = nextheader[i][0]
        mjd[i] = nextheader[i][1]
        seconds[i] = nextheader[i][2]
        if singlevis and seconds[i] > startseconds:
            print "Exiting since singlevis was specified"
            sys.exit()
        freqindex[i] = nextheader[i][5]
        polpair[i] = nextheader[i][6]
        nchan[i] = freqs[freqindex[i]].numchan/freqs[freqindex[i]].specavg
        buffer = difxinputs[i].read(8*nchan[i])
        if nchan[i] >= maxchannels:
            print "How embarrassing - you have tried to diff files with more than " + \
                str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
            sys.exit()
        for j in range(nchan[i]):
            cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
            vis[i][j] = complex(cvis[0], cvis[1])
            amp[i][j] = math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1])
            phase[i][j] = math.atan2(cvis[1], cvis[0])*180.0/math.pi
	if (targetbaseline < 0 or targetbaseline == baseline[i]) and \
	    (targetfreq < 0 or targetfreq == freqindex[i]):
            lag[i] = fft.ifft(vis[i], nchan[i])
            for j in range(nchan[i]/2):
                lagamp[i][j+nchan[i]/2] = abs(lag[i][j])
            for j in range(nchan[i]/2):
                lagamp[i][j] = abs(lag[i][j+nchan[i]/2])
            if i > 0:
                for j in range(len(nextheader[0])):
                    if nextheader[i][j] != nextheader[0][j]:
                        print "Headers disagree!"
                        if verbose:
                            print nextheader[0]
                            print nextheader[i]
                        break
            if (targetbaseline < 0 or baseline[0] == targetbaseline) and \
               (targetfreq < 0 or freqindex[0] == targetfreq):
                maxlag = max(lagamp[i])
                maxindex = lagamp[i].index(maxlag)
                delayoffsetus = (maxindex - nchan[i]/2) * 1.0/(freqs[freqindex[0]].bandwidth*2)
                pylab.subplot(311)
                pylab.plot(chans[:nchan[i]], amp[i][:nchan[i]], linestyles[i])
                pylab.subplot(312)
                pylab.plot(chans[:nchan[i]], phase[i][:nchan[i]], linestyles[i])
                pylab.subplot(313)
                pylab.plot(chans[:nchan[i]], lagamp[i][:nchan[i]], linestyles[i])
                lagamp[i][maxindex] = 0
                if maxindex > 0:
                    lagamp[i][maxindex-1] = 0
                if maxindex < len(lagamp)-1:
                    lagamp[i][maxindex+1] = 0
                rms = numpy.std(lagamp[i])
                snr = maxlag/rms
                print snr
                print maxlag
    if (targetbaseline < 0 or baseline[0] == targetbaseline) and \
       (targetfreq < 0 or freqindex[0] == targetfreq):
        pylab.subplot(311)
        ant1index = baseline[0] / 256 - 1
        ant2index = baseline[0] % 256 - 1
        ant1name  = telescopes[ant1index].name
        ant2name  = telescopes[ant2index].name
        lowfreq   = freqs[freqindex[0]].freq
        hifreq    = freqs[freqindex[0]].freq + freqs[freqindex[0]].bandwidth
        if freqs[freqindex[0]].lsb:
            lowfreq -= freqs[freqindex[0]].bandwidth
            hifreq -= freqs[freqindex[0]].bandwidth
        print seconds[0]
        hour      = int(seconds[0]/3600)
        minute    = int(seconds[0]/60 - 60*hour)
        second    = int(seconds[0] - (3600*hour + 60*minute))
        mjdstring = "%d %02d:%02d:%02d" % (mjd[0], hour, minute, second)
        titlestr = "Baseline %s-%s, Freq %.2f-%.2f, pol %s, date %s" % \
                   (ant1name, ant2name, lowfreq, hifreq, polpair[0], mjdstring)
        pylab.title(titlestr)
        pylab.ylabel("Amplitude")
        pylab.subplot(312)
        pylab.ylabel("Phase (deg)")
        pylab.subplot(313)
        pylab.ylabel("Lag")
        pylab.xlabel("Channel")
        pylab.figtext(0.0,0.0,"Fringe S/N %0.2f @ offset %0.2f us (%s)" % \
                      (snr, delayoffsetus, "raw S/N is overestimated - corrected value ~%0.2f" % ((snr-3)/2)))
        if toscreen:
            pylab.show()
        else:
            pylab.savefig("%s_baseline%03d_freq_%02d_pol_%s.png" % (inputfile, baseline[i], freqindex[i], polpair[i]))
        pylab.clf()
    for i in range(numfiles):
        nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])
    
