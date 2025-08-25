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
parser.add_option("-p", "--pols", dest="pollist", default="RR,RL,LR,LL,YY,YX,XY,XX,XL,XR,YL,YR,LX,RX,LY,RY",
                  help="Only display polarization pairs from this comma-separated list")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq")
parser.add_option("-x", "--toscreen", dest="toscreen", default=False, action="store_true",
                  help="Plot to the screen, otherwise to png files")
parser.add_option("--singlevis", dest="singlevis", default=False, action="store_true",
                  help="Stop plotting as soon as there is a time change")
parser.add_option("--firstpermatch", dest="firstpermatch", default=False, action="store_true",
                  help="For each baseline plot only the first matching entry")
parser.add_option("-1", "--singleplot", dest="singleplot", default=False, action="store_true",
                  help="Plot everything on one axis")
parser.add_option("--unwrap", dest="unwrap", default=False, action="store_true",
                  help="Unwrap the phase")
parser.add_option("--noauto", dest="noauto", default=False, action="store_true",
                  help="Exclude autocorrelation data")
parser.add_option("--amprange", dest="amprange", default="-1,-1", 
                  help="Range for the y axis for amplitude subplot in form min,max")
(options, args) = parser.parse_args()

if len(args) < 1:
    parser.error("You must supply at least one DiFX output file!")

numfiles = len(args)

targetbaseline = int(options.baseline)
targetfreq = int(options.freq)
maxchannels    = int(options.maxchannels)
pollist        = options.pollist.upper().split(",")
verbose        = options.verbose
inputfile      = options.inputfile
toscreen       = options.toscreen
singlevis      = options.singlevis
singleplot     = options.singleplot
firstpermatch  = options.firstpermatch
amprange       = options.amprange.split(',')
unwrap         = options.unwrap
noauto         = options.noauto

try:
    import matplotlib as mpl
    # Must disable path simplifcation to allow fringe peaks to be seen even in dense plots
    # http://stackoverflow.com/questions/15795720/matplotlib-major-display-issue-with-dense-data-sets
    mpl.rcParams['path.simplify'] = False
except:
    pass

if inputfile == "":
    parser.error("You must supply an input file!")
if len(amprange) != 2:
    parser.error("Supply amprange in the form min,max!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
amprange[0] = float(amprange[0])
amprange[1] = float(amprange[1])

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
med         = []
for filename in args:
    difxinputs.append(open(filename))
    freqindex.append(0)
    nchan.append(0)
    baseline.append(0)
    mjd.append(0)
    seconds.append(0.0)
    med.append(0.0)
    polpair.append("")
    nextheader.append([])
unplottedbaselines=[]
for i in range(16):
	for j in range(16):
		unplottedbaselines.append(j*256+i)

for i in range(numfiles):
    nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])

count = 0
keeplooping = True
if not len(nextheader[0]) == 0:
    startseconds = nextheader[0][2]
while not len(nextheader[0]) == 0 and keeplooping:
    print ("Looping...")
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
            print ("Exiting since singlevis was specified")
            keeplooping = False
            break
        freqindex[i] = nextheader[i][5]
        polpair[i] = nextheader[i][6]
        nchan[i] = freqs[freqindex[i]].numchan/freqs[freqindex[i]].specavg
        vis[i] = numpy.fromstring(difxinputs[i].read(8*nchan[i]), dtype='complex64')
        if nchan[i] > maxchannels:
            print ("How embarrassing - you have tried to read files with more than " + \
                str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!")
            sys.exit()
        if firstpermatch:
            match_freq = (targetfreq < 0) or (freqindex[i] == targetfreq)
            match_pol = (polpair[i]) in pollist
            match_new = baseline[i] in unplottedbaselines
            if match_new and match_freq and match_pol:
                targetbaseline = baseline[i]
                try:
                    unplottedbaselines.remove(baseline[i])
                    unplottedbaselines.remove(baseline[i])
                except:
                    pass
                print ('Got new baseline %s, freq %d' % (str(targetbaseline),freqindex[i]))
            else:
                if match_new:
                    print ('Skip new baseline %s with wrong freq %d != %d' % (str(baseline[i]),freqindex[i],targetfreq))
                if match_freq:
                    print ('Skip old baseline %s with desired freq %d = %d' % (str(baseline[i]),freqindex[i],targetfreq))
                targetbaseline = 13

        amp[i] = numpy.abs(vis[i])
        phase[i] = numpy.angle(vis[i])

        if unwrap:
            phase[i] = (numpy.unwrap(phase[i]))
        phase[i] *= 180.0/math.pi

        if (targetbaseline < 0 or targetbaseline == baseline[i]) and \
            (targetfreq < 0 or targetfreq == freqindex[i]) and (polpair[i] in pollist) and \
            not (noauto and (baseline[i] % 257) == 0):
            lag[i] = fft.ifft(vis[i], nchan[i])
            for j in range(nchan[i]/2):
                lagamp[i][j+nchan[i]/2] = abs(lag[i][j])
            for j in range(nchan[i]/2):
                lagamp[i][j] = abs(lag[i][j+nchan[i]/2])
            med[i] = numpy.median(amp[i][:nchan[i]])
            if i > 0:
                for j in range(len(nextheader[0])):
                    if nextheader[i][j] != nextheader[0][j]:
                        print ("Headers disagree!")
                        if verbose:
                            print (nextheader[0])
                            print (nextheader[i])
                        break
            if (targetbaseline < 0 or baseline[0] == targetbaseline) and \
               (targetfreq < 0 or freqindex[0] == targetfreq) and (polpair[0] in pollist):
                maxlag = max(lagamp[i])
                maxindex = lagamp[i].index(maxlag)
                delayoffsetus = (maxindex - nchan[i]/2) * 1.0/(freqs[freqindex[0]].bandwidth)
                if singleplot:
                    ls = linestyles[count%len(linestyles)]
                    print ("Setting linestyle to " + ls)
                    count += 1
                else:
                    ls = linestyles[i]
                pylab.gcf().set_facecolor('white')
                pylab.subplot(311)
                pylab.plot(chans[:nchan[i]], amp[i][:nchan[i]], ls)
                if amprange[1] > 0:
                    pylab.ylim(amprange)
                pylab.xlim([0, nchan[i]])
                pylab.subplot(312)
                pylab.plot(chans[:nchan[i]], phase[i][:nchan[i]], ls+'+')
                pylab.ylim([-200, 200])
                pylab.xlim([0, nchan[i]])
                pylab.subplot(313)
                pylab.plot(chans[:nchan[i]], lagamp[i][:nchan[i]], ls)
                pylab.xlim([0, nchan[i]])
                lagamp[i][maxindex] = 0
                if maxindex > 0:
                    lagamp[i][maxindex-1] = 0
                if maxindex < len(lagamp)-1:
                    lagamp[i][maxindex+1] = 0
                rms = numpy.std(lagamp[i])
                snr = maxlag/rms
                print (snr)
                print (maxlag)

    if (targetbaseline < 0 or baseline[0] == targetbaseline) and \
       (targetfreq < 0 or freqindex[0] == targetfreq) and (polpair[0] in pollist) and \
        not (noauto and (baseline[i] % 257) == 0):
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
        print (seconds[0])
        hour      = int(seconds[0]/3600)
        minute    = int(seconds[0]/60 - 60*hour)
        second    = int(seconds[0] - (3600*hour + 60*minute))
        mjdstring = "%d %02d:%02d:%02d" % (mjd[0], hour, minute, second)
        titlestr = "Baseline %s-%s, Freq %.2f-%.2f, pol %s, date %s" % \
                   (ant1name, ant2name, lowfreq, hifreq, polpair[0], mjdstring)
        pylab.ylabel("Amplitude")
        pylab.subplot(312)
        pylab.ylabel("Phase (deg)")
        pylab.subplot(313)
        pylab.ylabel("Lag")
        pylab.xlabel("Channel")
        if not singleplot:
            pylab.subplot(311)
            pylab.title(titlestr)
            pylab.subplot(313)
            pylab.figtext(0.0,0.0,"Fringe S/N %0.2f @ offset %0.3f us (%s)" % \
                          (snr, delayoffsetus, "raw S/N is overestimated - corrected value ~%0.2f" % ((snr-3)/2)))
            if toscreen:
                pylab.show()
            else:
                pylab.savefig("%s_baseline%03d_freq_%02d_pol_%s.png" % (inputfile, baseline[i], freqindex[i], polpair[i]))
            pylab.clf()
        print ("Median values were:")
        for i in range(numfiles):
            print ("File %d: %.6f" % (i, med[i]))
        if numfiles == 2:
            print ("Ratio of medians was " + str(med[1]/med[0]))
    if keeplooping:
        for i in range(numfiles):
            nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])
if singleplot:
    if toscreen:
        pylab.show()
    else:
        pylab.savefig("%s_baselineALL_freq_ALL_pol_ALL.png" % (inputfile))
