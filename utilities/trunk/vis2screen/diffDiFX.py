#!/usr/bin/python
import sys, os, struct, time, math
from optparse import OptionParser

helpstr = "diffDiFX [options] <difx file 1> <difx file 2>\n\n"
helpstr += "prints an error message if mean difference ever exceeds THRESHOLD, "
helpstr += "or every PRINTINTERVAL records if PRINTINTERVAL>0"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="FREQ", default="-1",
                  help="Only look at visibilities from this FREQ index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="BASELINE", default="-1",
                  help="Only look at visibilities from this BASELINE num")
parser.add_option("-t", "--threshold", dest="threshold", metavar="THRESHOLD", default="0.0005",
                  help="Display any difference that exceeds THRESHOLD")
parser.add_option("-s", "--skiprecords", dest="skiprecords", metavar="SKIPRECORDS",
                  default="0", help="Skip SKIPRECORDS records before starting comparison")
parser.add_option("-m", "--maxrecords", dest="maxrecords", metavar="MAXRECORDS",
                  default="-1", help="Stop after comparing MAXRECORDS (if >0) records")
parser.add_option("-p", "--printinterval", dest="printinterval", metavar="PRINTINTERVAL",
                  default="1000", help="Print a summary every PRINTINTERVAL records")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
		  default="16384", 
		  help="The length of the array that will be allocated to hold vis results")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
(options, args) = parser.parse_args()

if len(args) != 2:
    parser.error("You must supply two (and only two) difx files to diff!")

numfiles = len(args)

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
threshold      = float(options.threshold)
skiprecords    = int(options.skiprecords)
maxrecords     = int(options.maxrecords)
printinterval  = int(options.printinterval)
maxchannels    = int(options.maxchannels)
verbose        = options.verbose

if skiprecords < 0:
   parser.error("You can't skip a negative number of records!!")

chans = []
vis = []
for i in range(numfiles):
    vis.append([])
    
for i in range(maxchannels):
    chans.append(i)
    for j in range(numfiles):
        vis[j].append(0.0)
    
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
recordcount = 0
longtermabsdiff = 0.0
longtermmeandiff = 0.0
while not lines[0] == "" and not lines[1] == "":
    if maxrecords > 0:
        readrecords = recordcount - skiprecords
	if readrecords >= maxrecords:
	    print "Have examined " + str(readrecords) + " - finishing up!"
	    break
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
        while not nextcfloat == "BASELINE" and \
		not nextcfloat == "" and not nchan[i] >= maxchannels:
            cvis = struct.unpack("ff", nextcfloat)
            vis[i][nchan[i]] = complex(cvis[0], cvis[1])
            nchan[i] = nchan[i] + 1
            nextcfloat = difxinputs[i].read(8)
	if nchan[i] >= maxchannels:
	    print "How embarrassing - you have tried to diff files with more than " + \
	    	str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
	    sys.exit()
    if (targetbaseline < 0 or targetbaseline == baseline[0]) and \
        (targetfreq < 0 or targetfreq == freqindex[0]):
        if baseline[1] != baseline[0] or \
            mjd[1] != mjd[0] or seconds[1] != seconds[0] or \
            configindex[1] != configindex[0] or \
            sourceindex[1] != sourceindex[0] or \
            freqindex[1] != freqindex[0] or \
            polpair[1] != polpair[0] or psrbin[1] != psrbin[0] or \
            flag[1] != flag[0] or weight[1] != weight[0] or \
            u[1] != u[0] or v[1] != v[0] or w[1] != w[0] or \
            nchan[1] != nchan[0]:
            print "Warning - file 0 does not match file 1!"
	    if verbose:
                print "baselines are " + str(baseline[0]) + '/' + str(baseline[1])
                print "Time is " + str(mjd[0]) + ":" + str(seconds[0]) + " / " + \
                       str(mjd[1]) + ":" + str(seconds[1])
                print "uvw is " + str(u[0]) + "," + str(v[0]) + "," + str(w[0]) + \
                      " / " + str(u[1]) + "," + str(v[1]) + "," + str(w[1])
                print "polpair is " + polpair[0] + "/" + polpair[1]
                print "weight is " + str(weight[0]) + "/" + str(weight[1])
                print "indices are " + str(configindex[0]) + "," + str(sourceindex[0]) + \
                      " / " + str(configindex[1]) + "," + str(sourceindex[1])
                print "nchan is " + str(nchan[0]) + "/" + str(nchan[1])
                print "flag is " + str(flag[0]) + "/" + str(flag[1])
                print "psrbin is " + str(psrbin[0]) + "/" + str(psrbin[1])
        absdiffavg = 0.0
        meandiffavg = 0.0
        refavg = 0.0
	if recordcount >= skiprecords:
            for j in range(nchan[0]):
                diff = vis[1][j] - vis[0][j]
                absdiffavg  = absdiffavg + abs(diff)/nchan[0]
	        meandiffavg = meandiffavg + diff/nchan[0]
                refavg = refavg + abs(vis[0][j])/nchan[0]
	    longtermabsdiff += absdiffavg/refavg
	    longtermmeandiff += meandiffavg/refavg
            if 100.0*absdiffavg/refavg > threshold:
                print "THRESHOLD EXCEEDED! The percentage absolute difference on baseline %d, freq %d at MJD/sec %d/%7.2f is %10.8f, and the percentage mean difference is %10.8f + %10.8f i" % (baseline[0], freqindex[0], mjd[0], seconds[0], 100.0*absdiffavg/refavg, 100.0*meandiffavg.real/refavg, 100.0*meandiffavg.imag/refavg)
    for i in range(numfiles):
        lines[i] = difxinputs[i].readline()
    if (targetbaseline < 0 or targetbaseline == baseline[0]) and \
            (targetfreq < 0 or targetfreq == freqindex[0]):
        recordcount += 1
    if recordcount > 0 and printinterval>0 and recordcount%printinterval == 0:
        print "After %d records, the mean percentage absolute difference is %10.8f and the mean percentage mean difference is %10.8f + %10.8f i" % (recordcount-skiprecords, 100.0*longtermabsdiff/(recordcount-skiprecords), 100.0*longtermmeandiff.real/(recordcount-skiprecords), 100.0*longtermmeandiff.imag/(recordcount-skiprecords))

if recordcount > 0:
    print "After %d records, the mean percentage absolute difference is %10.8f and the mean percentage mean difference is %10.8f + %10.8f i" % (recordcount-skiprecords, 100.0*longtermabsdiff/  (recordcount-skiprecords), 100.0*longtermmeandiff.real/(recordcount-skiprecords), 100.0*             longtermmeandiff.imag/(recordcount-skiprecords))
