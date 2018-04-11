#!/usr/bin/python
import os,sys

if not len(sys.argv) == 3:
    print "Usage: %s <vex file> <freq def file>" % sys.argv[0]
    sys.exit()

vexfile = sys.argv[1]
freqdeffile = sys.argv[2]

if not os.path.exists(vexfile):
    print vexfile + " doesn't exist"
    sys.exit(1)

if not os.path.exists(freqdeffile):
    print freqdeffile + " doesn't exist"
    sys.exit(1)

freqdeflines = open(freqdeffile).readlines()
vexfilelines = open(vexfile).readlines()

fcount = 0
vcount = 0
freqsplitline = freqdeflines[fcount].split()
if len(freqsplitline) != 3:
    print "Invalid chandef line", freqdeflines[fcount]
    sys.exit()
initbw = float(freqsplitline[2])
for line in vexfilelines:
    if "sample_rate" in line:
        newline = "sample_rate =  %.15f Ms/sec;  * (2bits/sample)\n" % (2*initbw)
        vexfilelines[vcount] = newline
    if "chan_def" in line:
        if fcount >= len(freqdeflines):
            print "Too many chan_defs in the vex file!  Length of freqdef was ", len(freqdeflines)
            sys.exit(1)
        vexsplitline = line.split()
        freqsplitline = freqdeflines[fcount].split()
        if len(freqsplitline) != 3:
            print "Invalid chandef line", freqdeflines[fcount]
            sys.exit()
        freq = float(freqsplitline[0])
        sideband = freqsplitline[1]
        bw = float(freqsplitline[2])
        if sideband == "U":
            freq -= bw/2.0
        elif sideband == "L":
            freq += bw/2.0
        else:
            print "Invalid sideband ", freqsplitline[1]
            sys.exit()
        newline = "     chan_def = : %.12f MHz : %s :   %.12f MHz : &CH%02d : &BBC%02d : &U_Cal; *Rcp\n" % (freq, sideband, bw, fcount+1, fcount+1)
        vexfilelines[vcount] = newline
        fcount += 1
    vcount += 1

if not fcount == len(freqdeflines):
    print "Only replaced ", fcount, " frequencies, but there were ", len(freqdeflines), " in the chandef file!"
    sys.exit()

vexout = open(vexfile, "w")
for line in vexfilelines:
    vexout.write(line)
vexout.close()
