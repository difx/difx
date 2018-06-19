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
        newline = "     chan_def = : %.12f MHz : %s :   %.15f MHz : &CH%02d : &BBC%02d : &U_Cal; *Rcp\n" % (freq, sideband, bw, fcount+1, fcount+1)
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

freqs = []
for line in freqdeflines:
    freqs.append(int(line.split()[0]))
stitchfreqs = []
for freq in freqs:
    if freq+1 in freqs and freq+2 in freqs and freq+3 in freqs:
        if freq-1 in stitchfreqs or freq-2 in stitchfreqs or freq-3 in stitchfreqs:
            continue
        stitchfreqs.append(freq)
stitchfreqs.sort()
if len(stitchfreqs) == 0:
    print "Couldn't find any freqs to stitch! aborting"
    sys.exit()

# Now write the stitchconfig file, too
basename = vexfile.split('/')[-1].split('.')[0]
stitchout = open("%s.stitchconfig" % basename, "w")
stitchout.write("[config]\n")
stitchout.write("target_bw: 4.000\n")
stitchout.write("target_nchan: 432\n")
stitchout.write("target_chavg: 1\n")
stitchout.write("stitch_oversamplenum: 32\n")
stitchout.write("stitch_oversampledenom: 27\n")
stitchout.write("stitch_nstokes: 1\n")
stitchout.write("stitch_antennas: *\n")
stitchout.write("stitch_basefreqs: ")
for i, freq in enumerate(stitchfreqs):
    stitchout.write("%.1f" % (freq-0.5))
    if not i == len(stitchfreqs)-1:
        stitchout.write(", ")
stitchout.write("\n")
stitchout.write("verbose: True\n")
stitchout.close()
