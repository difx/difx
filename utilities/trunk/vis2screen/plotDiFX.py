#!/usr/bin/python
import sys, os, struct, time, pylab, math
from numpy import fft

if len(sys.argv) < 2:
    print "Usage: plotDiFX <difx file 1> [difx file 2] ... [difx file N]\n\nFlashes bandpasses at a rate of 5 Hz"
    sys.exit()

numfiles = len(sys.argv[1:])
MAX_CHANNELS = 16384

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
linestyles = ['b', 'r', 'g', 'k', 'y']
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

for filename in sys.argv[1:]:
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
            amp[i][nchan[i]] = math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1])
            phase[i][nchan[i]] = math.atan2(cvis[1], cvis[0])
            nchan[i] = nchan[i] + 1
            nextcfloat = difxinputs[i].read(8)
        lag[i] = fft.ifft(vis[i], nchan[i])
        for j in range(nchan[i]/2):
            lagamp[i][j+nchan[i]/2] = abs(lag[i][j])
        for j in range(nchan[i]/2):
            lagamp[i][j] = abs(lag[i][j+nchan[i]/2])
        if i > 0:
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
        pylab.subplot(311)
        pylab.plot(chans[:nchan[i]], amp[i][:nchan[i]], linestyles[i])
        pylab.subplot(312)
        pylab.plot(chans[:nchan[i]], phase[i][:nchan[i]], linestyles[i])
        pylab.subplot(313)
        pylab.plot(chans[:nchan[i]], lagamp[i][:nchan[i]], linestyles[i])
    pylab.subplot(311)
    titlestr = "Baseline " + str(baseline[0]) + ", Freq " + str(freqindex[0]) + ", pol " + polpair[0] + ", MJD " + str(mjd[0]+seconds[0]/86400.0)
    pylab.title(titlestr)
    pylab.ylabel("Amplitude")
    pylab.subplot(312)
    pylab.ylabel("Phase")
    pylab.subplot(313)
    pylab.ylabel("Lag")
    pylab.xlabel("Channel")
    pylab.show()
    for i in range(numfiles):
        lines[i] = difxinputs[i].readline()
    
