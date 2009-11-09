#!/usr/bin/python
import sys, os, struct, time, pylab, math
from optparse import OptionParser
from numpy import fft

parser = OptionParser()
parser.add_option("-f", "--freq", dest="freq", metavar="FREQ", default="-1",
                  help="Only look at visibilities from this frequency index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="BASELINE", default="-1",
                  help="Only look at visibilities from this baseline num")
parser.add_option("-t", "--threshold", dest="threshold", metavar="THRESHOLD", default="0.0005",
                  help="Only display difference if greater than THRESHOLD")
(options, args) = parser.parse_args()

if len(args) != 2:
    print "Usage: diffDiFX [options] <difx file 1> <difx file 2>\n\nShows percentage difference in abs values between two files"
    sys.exit()

numfiles = len(args)
MAX_CHANNELS = 16384

BASELINE = int(options.baseline)
FREQ = int(options.freq)
threshold = float(options.threshold)

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
    
for i in range(MAX_CHANNELS):
    chans.append(i)
    for j in range(numfiles):
        amp[j].append(0.0)
        phase[j].append(0.0)
        vis[j].append(0.0)
        lag[j].append(0.0)
        lagamp[j].append(0.0)
    

pylab.xlabel("Channel")
pylab.ylabel("Amplitude")
difxinputs  = []
lines       = []
baseline    = []
mjd         = []
seconds     = []
configindex = []
sourceindex = []
freqindex   = []
polpair     = []
psrbin      = []
flag        = []
weight      = []
u           = []
v           = []
w           = []
nchan       = []
for i in range(numfiles):
    lines.append("")
    baseline.append(0)
    mjd.append(0)
    seconds.append(0.0)
    configindex.append(0)
    sourceindex.append(0)
    freqindex.append(0)
    polpair.append("")
    psrbin.append(0)
    flag.append(0)
    weight.append(0.0)
    u.append(0.0)
    v.append(0.0)
    w.append(0.0)
    nchan.append(0)

for filename in args:
    difxinputs.append(open(filename))

for i in range(numfiles):
    lines[i] = difxinputs[i].readline()
while not lines[0] == "":
    for i in range(numfiles):
        baseline[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        mjd[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        seconds[i] = float((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        configindex[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        sourceindex[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        freqindex[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        polpair[i] = (lines[i].split(':')[1]).strip()
        lines[i] = difxinputs[i].readline()
        psrbin[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        flag[i] = int((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        weight[i] = float((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        u[i] = float((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        v[i] = float((lines[i].split(':')[1]).strip())
        lines[i] = difxinputs[i].readline()
        w[i] = float((lines[i].split(':')[1]).strip())
        nchan[i] = 0
        nextcfloat = difxinputs[i].read(8)
        while not nextcfloat == "BASELINE" and not nextcfloat == "":
            cvis = struct.unpack("ff", nextcfloat)
            vis[i][nchan[i]] = complex(cvis[0], cvis[1])
            #amp[i][nchan[i]] = math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1])
            #phase[i][nchan[i]] = math.atan2(cvis[1], cvis[0])
            nchan[i] = nchan[i] + 1
            nextcfloat = difxinputs[i].read(8)
	if (BASELINE < 0 or BASELINE == baseline[i]) and \
	    (FREQ < 0 or FREQ == freqindex[i]):
            if i == 1:
                if baseline[i] != baseline[0] or \
                   mjd[i] != mjd[0] or seconds[i] != seconds[0] or \
                   configindex[i] != configindex[0] or \
                   sourceindex[i] != sourceindex[0] or \
                   freqindex[i] != freqindex[0] or \
                   polpair[i] != polpair[0] or psrbin[i] != psrbin[0] or \
                   flag[i] != flag[0] or weight[i] != weight[0] or \
                   u[i] != u[0] or v[i] != v[0] or w[i] != w[0] or \
                   nchan[i] != nchan[0]:
                    print "Warning - file " + str(i) + " does not match file 0!"
                    print "baselines are " + str(baseline[i]) + '/' + str(baseline[0])
                    print "Time is " + str(mjd[i]) + ":" + str(seconds[i]) + " / " + \
                          str(mjd[0]) + ":" + str(seconds[0])
                    print "uvw is " + str(u[i]) + "," + str(v[i]) + "," + str(w[i]) + \
                          " / " + str(u[0]) + "," + str(v[0]) + "," + str(w[0])
                    print "polpair is " + polpair[i] + "/" + polpair[0]
                    print "weight is " + str(weight[i]) + "/" + str(weight[0])
                    print "indices are " + str(configindex[i]) + "," + str(sourceindex[i]) + \
                          " / " + str(configindex[0]) + "," + str(sourceindex[0])
                    print "nchan is " + str(nchan[i]) + "/" + str(nchan[0])
                    print "flag is " + str(flag[i]) + "/" + str(flag[0])
                    print "psrbin is " + str(psrbin[i]) + "/" + str(psrbin[0])
		diffavg = 0.0
		refavg = 0.0
                for j in range(nchan[0]):
		    diff = vis[1][j] - vis[0][j]
                    diffavg = diffavg + abs(diff)/nchan[0]
                    refavg = refavg + abs(vis[0][j])/nchan[0]
    if (BASELINE < 0 or baseline[0] == BASELINE) and \
       (FREQ < 0 or freqindex[0] == FREQ):
        if 100.0*diffavg/refavg > threshold:
            print "The percentage difference on baseline %d, freq %d is %10.8f" % (baseline[0], freqindex[0], 100.0*diffavg/refavg)
    for i in range(numfiles):
        lines[i] = difxinputs[i].readline()
    
