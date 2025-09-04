#!/usr/bin/python
import sys, os, struct, time, math
import parseDiFX
from optparse import OptionParser
import numpy
from numpy import fft

## OPTIONS PARSING ###
helpstr = "plotFringeSpectrum.py [options] <difx file 1>\n\n"
helpstr += "Makes a fringe spectrum from visibility output on a single baseline at a single frequency"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="targetfreq", default="0",
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
parser.add_option("-x", "--toscreen", dest="toscreen", default=False, action="store_true",
                  help="Plot to the screen, otherwise to png files")
parser.add_option("--logamp", dest="logamp", default=False, action="store_true",
                  help="Take the log of amplitudes, for a flatter scaling")
parser.add_option("--maxtimestep", dest="maxtimestep", default="-1",
                  help="Max timestep number, if you want to limit the range")
parser.add_option("--chanrange", dest="chanrange", default="-1,-1",
                  help="Channel range to plot, in form min,max (-1,-1 for all)")
parser.add_option("--secondswindow", dest="secondswindow", default="-1,-1",
                  help="Time range to plot, in form min,max (-1,-1 for all)")
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
chanrange      = options.chanrange.split(',')
secondswindow  = options.secondswindow.split(',')
print ("Target baseline: %d\nTarget freq: %d\nTarget pol: %s" % (targetbaseline,targetfreq,targetpolpair))

import matplotlib
if not toscreen:
    matplotlib.use('Agg')
from matplotlib.ticker import FuncFormatter, MaxNLocator, NullFormatter
from mpl_toolkits.mplot3d import Axes3D
import pylab

if len(chanrange) != 2:
    parser.error("Channel range must be in form min,max")
chanrange[0] = int(chanrange[0])
chanrange[1] = int(chanrange[1])
secondswindow[0] = int(secondswindow[0])
secondswindow[1] = int(secondswindow[1])

if inputfile == "":
    parser.error("You must supply an input file!")

difx = parseDiFX.DiFXFile()
difx.open(inputfile,phasecenterId=0,pulsarbinId=0)

chans = []
vis = []
for i in range(difx.metainfo.numfreqs):
    vis.append([])
    for j in range(maxchannels):
        vis[i].append([])

for i in range(maxchannels):
    chans.append(i)

savednchan = -1
lastseconds = -1
nvis_used = 0
nvis_total = 0

while True:

    visrec = difx.nextVisibilityRecord()
    if not visrec.header.isvalid():
        break

    nvis_total += 1
    if maxtimestep > 0 and nvis_total > maxtimestep:
        break

    fq = difx.getFrequency(visrec.header.freqindex)
    nchan = fq.numchan / fq.specavg
    if nchan >= maxchannels:
        print ("How embarrassing - you have tried to diff files with more than " + \
            str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!")
        sys.exit()

    if visrec.header.baseline != targetbaseline:
        continue
    if visrec.header.freqindex != targetfreq:
        continue
    if (visrec.header.seconds < secondswindow[0] and secondswindow[0] >= 0) or \
       (visrec.header.seconds > secondswindow[1] and secondswindow[1] >= 0):
        print ("Skipping data not in specified time range")
        continue
    if not(visrec.header.polpair in targetpolpair):
        print ("Skipping polpair %s not in specified target pair(s) of %s" % (visrec.header.polpair,str(targetpolpair)))
        continue

    if visrec.header.seconds != lastseconds:
        for i in range(difx.metainfo.numfreqs):
            for j in range(nchan):
                vis[i][j].append(0.0)
        lastseconds = visrec.header.seconds

    f = visrec.header.freqindex
    for j in range(nchan):
        vis[f][j][-1] = visrec.vis[j]
    nvis_used += 1
    savednchan = nchan
    print ("Added visibility of baseline %d freq %d pol %s with %d channels, time %.5f : now copied %d in total"
          % (visrec.header.baseline, f, visrec.header.polpair, nchan, visrec.header.seconds, nvis_used))

if chanrange[0] < 0:
    chanrange[0] = 0
if chanrange[1] < 0:
    chanrange[1] = savednchan

if savednchan <= 0:
    print ("Found no data for plotting")
    sys.exit(0)

if savednchan > 0:

    antenna1 = (targetbaseline - (targetbaseline % 256)) // 256
    antenna2 = targetbaseline % 256
    ant1name = difx.getTelescope(antenna1 - 1).name
    ant2name = difx.getTelescope(antenna2 - 1).name

    fq = difx.getFrequency(visrec.header.freqindex)
    fq.freq = fq.freq
    chanwidth = fq.bandwidth / (fq.numchan/fq.specavg)
    numpyvis = numpy.zeros((chanrange[1]-chanrange[0], nvis_used), numpy.csingle)
    for i in range(chanrange[1]-chanrange[0]):
        for j in range(nvis_used):
            numpyvis[i][j] = vis[targetfreq][i+chanrange[0]][j]

    ax = matplotlib.pyplot.axes(projection='3d')

    ax.set_xlabel("Delay Rate (ps/s)")
    ax.set_ylabel("Delay (us)")
    ax.set_zlabel("Fringe Amplitude")
    ax.view_init(35, 20)
    ax.set_title("Fringe on %s-%s at %.3f MHz in %s-pol" % (ant1name,ant2name,fq.freq,targetpolpair))

    numpylagspec = numpy.fft.fft2(numpyvis)
    numpylagspec = numpy.fft.fftshift(numpylagspec)
    numpylagspec_amp = numpy.abs(numpylagspec)
    if logamp:
        numpylagspec_amp = numpy.log10(numpylagspec_amp)

    dly_halfrange_usec = 0.5 / chanwidth
    rate_halfrange_psecsec = 0.5e6 / (difx.metainfo.configs[0].inttime * fq.freq)

    spc_x = numpy.linspace(-rate_halfrange_psecsec, rate_halfrange_psecsec, numpylagspec.shape[1])
    spc_y = numpy.linspace(-dly_halfrange_usec, dly_halfrange_usec, numpylagspec.shape[0])
    mesh_x, mesh_y = numpy.meshgrid(spc_x, spc_y)

    scamap = matplotlib.cm.ScalarMappable(cmap='binary')

    surf = ax.plot_surface(mesh_x, mesh_y, numpylagspec_amp, rstride=1, cstride=1, facecolors=scamap.to_rgba(numpylagspec_amp), shade=True)
    # surf.set_facecolor((0,0,0,0))

    if toscreen:
        pylab.show()
    else:
        pylab.savefig("fringespectrum.b%d.f%d.%s.png" % (targetbaseline, targetfreq, targetpolpair), format="png")
