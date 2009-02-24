#!/usr/bin/python
import sys, os, struct, time, pylab, math
from numpy import fft

if not len(sys.argv) == 2:
    print "Usage: plotDiFX <difx file>\n\nFlashes bandpasses at a rate of 5 Hz"
    sys.exit()

chans = []
for i in range(16384):
    chans.append(i)
pylab.xlabel("Channel")
pylab.ylabel("Amplitude")

difxin = open(sys.argv[1])
line = difxin.readline()
while not line == "":
    amp = []
    phase = []
    vis = []
    lag = []
    lagamp = []
    baseline = int((line.split(':')[1]).strip())
    line = difxin.readline()
    mjd = int((line.split(':')[1]).strip())
    line = difxin.readline()
    seconds = float((line.split(':')[1]).strip())
    line = difxin.readline()
    configindex = int((line.split(':')[1]).strip())
    line = difxin.readline()
    sourceindex = int((line.split(':')[1]).strip())
    line = difxin.readline()
    freqindex = int((line.split(':')[1]).strip())
    line = difxin.readline()
    polpair = (line.split(':')[1]).strip()
    line = difxin.readline()
    psrbin = int((line.split(':')[1]).strip())
    line = difxin.readline()
    flag = int((line.split(':')[1]).strip())
    line = difxin.readline()
    weight = float((line.split(':')[1]).strip())
    line = difxin.readline()
    u = float((line.split(':')[1]).strip())
    line = difxin.readline()
    v = float((line.split(':')[1]).strip())
    line = difxin.readline()
    w = float((line.split(':')[1]).strip())
    nchan = 0
    nextcfloat = difxin.read(8)
    while not nextcfloat == "BASELINE" and not nextcfloat == "":
        cvis = struct.unpack("ff", nextcfloat)
        vis.append(complex(cvis[0], cvis[1]))
        amp.append(math.sqrt(cvis[0]*cvis[0] + cvis[1]*cvis[1]))
        phase.append(math.atan2(cvis[1], cvis[0]))
        nchan = nchan + 1
        nextcfloat = difxin.read(8)
    lag = fft.ifft(vis, nchan)
    for l in lag:
        lagamp.append(abs(l))
    lagamp = lagamp[nchan/2:] + lagamp[:nchan/2]
    pylab.subplot(311)
    titlestr = "Baseline " + str(baseline) + ", Freq " + str(freqindex) + ", pol " + polpair + ", MJD " + str(mjd+seconds/86400.0)
    pylab.title(titlestr)
    pylab.ylabel("Amplitude")
    pylab.plot(chans[:nchan], amp)
    pylab.subplot(312)
    pylab.ylabel("Phase")
    pylab.plot(chans[:nchan], phase)
    pylab.subplot(313)
    pylab.ylabel("Lag")
    pylab.xlabel("Channel")
    pylab.plot(chans[:nchan], lagamp)
    pylab.show()
    line = difxin.readline()
    
