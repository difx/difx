#!/usr/bin/python
import os,sys, argparse


parser = argparse.ArgumentParser()
parser.add_argument("vexfile",  help="Vexfile to update")
parser.add_argument("chan", help="Flat text file containing 1 line per subband, centre freq, sideband, and bandwidth")
parser.add_argument("-n", "--nchan", type=int, default=128, help="Number of spectral channels")
args = parser.parse_args()

vexfile = args.vexfile
freqdeffile = args.chan
nchan = args.nchan

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

if nchan%32==0: # Must be divisible by 32 to merge
    # Now write the stitchconfig file, too
    basename = vexfile.split('/')[-1].split('.')[0]
    stitchout = open("%s.stitchconfig" % basename, "w")

    stitchout.write('''\
[config]
target_bw: 4.000
target_nchan: {}
target_chavg: 1
stitch_oversamplenum: 32
stitch_oversampledenom: 27
stitch_nstokes: 1
stitch_antennas: *
stitch_basefreqs: '''.format(nchan*fcount/2/32*27))
    for i, freq in enumerate(stitchfreqs):
        stitchout.write("%.1f" % (freq-0.5))
        if not i == len(stitchfreqs)-1:
            stitchout.write(", ")
    stitchout.write("\n")
    stitchout.write("verbose: True\n")
    stitchout.close()
else:
    print "Warning: Cannot merge data in number of frequency points per coarse channel ({}) is not divisible by 32".format(nchan)
