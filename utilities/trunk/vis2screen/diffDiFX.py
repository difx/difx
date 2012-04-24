#!/usr/bin/python
import sys, os, struct, time, math
import parseDiFX
from optparse import OptionParser

helpstr = "diffDiFX.py [options] <difx file 1> <difx file 2>\n\n"
helpstr += "prints an error message if mean difference ever exceeds THRESHOLD, "
helpstr += "or every PRINTINTERVAL records if PRINTINTERVAL>0"
parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="FREQ", default="-1",
                  help="Only look at visibilities from this FREQ index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="BASELINE", default="-1",
                  help="Only look at visibilities from this BASELINE num")
parser.add_option("-t", "--threshold", dest="threshold", metavar="THRESHOLD", default="0.0005",
                  help="Display any difference that exceeds THRESHOLD percent")
parser.add_option("-e", "--epsilon", dest="epsilon", metavar="EPSILON", default="-1",
                  help="Display any difference that exceeds allowed numerical error EPSILON")
parser.add_option("-s", "--skiprecords", dest="skiprecords", metavar="SKIPRECORDS",
                  default="0", help="Skip SKIPRECORDS records before starting comparison")
parser.add_option("-m", "--maxrecords", dest="maxrecords", metavar="MAXRECORDS",
                  default="-1", help="Stop after comparing MAXRECORDS (if >0) records")
parser.add_option("-p", "--printinterval", dest="printinterval", metavar="PRINTINTERVAL",
                  default="1000", help="Print a summary every PRINTINTERVAL records")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
		  default="16384", 
		  help="The length of the array that will be allocated to hold vis results")
parser.add_option("--matchheaders", dest="matchheaders", action="store_true", default=False,
                  help="On seeing a header mismatch, skip through file 2 looking for match")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq") 
(options, args) = parser.parse_args()

if len(args) != 2:
    parser.error("You must supply two (and only two) difx files to diff!")

numfiles = len(args)

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
threshold      = float(options.threshold)
epsilon        = float(options.epsilon)
skiprecords    = int(options.skiprecords)
maxrecords     = int(options.maxrecords)
printinterval  = int(options.printinterval)
maxchannels    = int(options.maxchannels)
verbose        = options.verbose
matchheaders   = options.matchheaders
inputfile      = options.inputfile

if skiprecords < 0:
    parser.error("You can't skip a negative number of records!!")

if inputfile == "":
    parser.error("You must supply an input file!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
if numfreqs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly!")

vis = []
for i in range(numfiles):
    vis.append([])
    for j in range(maxchannels):
        vis[i].append(0.0)
    
difxinputs  = []
nextheader  = []
freqindex   = []
baseline    = []
nchan       = []
mjd         = []
seconds     = []
for filename in args:
    difxinputs.append(open(filename))
    freqindex.append(0)
    nchan.append(0)
    baseline.append(0)
    mjd.append(0)
    seconds.append(0)
    nextheader.append([])

for i in range(numfiles):
    nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])

recordcount = 0
headerdisagreecount = 0
longtermabsdiff = 0.0
longtermmeandiff = complex(0.0, 0.0)
disagreeskip = -1
while not len(nextheader[0]) == 0 and not len(nextheader[1]) == 0:
    if maxrecords > 0:
        readrecords = recordcount - skiprecords
	if readrecords >= maxrecords:
	    print "Have examined " + str(readrecords) + " - finishing up!"
	    break
    for i in range(numfiles):
        baseline[i] = nextheader[i][0]
        freqindex[i] = nextheader[i][5]
        nchan[i] = freqs[freqindex[i]].numchan/freqs[freqindex[i]].specavg
        mjd[i] = nextheader[i][1]
        seconds[i] = nextheader[i][2]
        if nchan[i] > maxchannels:
            print "How embarrassing - you have tried to read files with more than " + \
                  str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!"
            sys.exit()
	if i != disagreeskip:
	    buffer = difxinputs[i].read(8*nchan[i])
            for j in range(nchan[i]):
                cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
                vis[i][j] = complex(cvis[0], cvis[1])
    compare = []
    compare.append([mjd[0], mjd[1]])
    compare.append([seconds[0], seconds[1]])
    compare.append([baseline[0], baseline[1]])
    compare.append([freqindex[0], freqindex[1]])
    disagreeskip = -1
    for j in range(len(nextheader[0])):
        if nextheader[0][j] != nextheader[1][j]:
            headerdisagreecount += 1
            if verbose:
	        print "Headers disagree!"
                print nextheader[0]
                print nextheader[1]
            break
    for c in compare:
        if c[0] > c[1]:
            disagreeskip = 0
        elif c[1] > c[0]:
            disagreeskip = 1
    if disagreeskip >= 0 and verbose:
        print "Disagreeskip is now set to %d" % disagreeskip
	print compare
    if (targetbaseline < 0 or targetbaseline == baseline[0]) and \
        (targetfreq < 0 or targetfreq == freqindex[0]):
        absdiffavg = 0.0
        meandiffavg = 0.0
        refavg = 0.0
	if recordcount >= skiprecords and disagreeskip < 0:
	    nonequalchanslist = []
            for j in range(nchan[0]):
                diff = vis[1][j] - vis[0][j]
	        if (epsilon > 0 and abs(diff) > epsilon):
	            nonequalchanslist.append(j)
                absdiffavg  = absdiffavg + abs(diff)/nchan[0]
	        meandiffavg = meandiffavg + diff/nchan[0]
                refavg = refavg + abs(vis[0][j])/nchan[0]
	    longtermabsdiff += absdiffavg/refavg
	    longtermmeandiff += meandiffavg/refavg
            if 100.0*absdiffavg/refavg > threshold:
                print "THRESHOLD EXCEEDED! The percentage absolute difference on baseline %d, freq %d at MJD/sec %d/%7.2f is %10.8f, and the percentage mean difference is %10.8f + %10.8f i" % (baseline[0], freqindex[0], mjd[0], seconds[0], 100.0*absdiffavg/refavg, 100.0*meandiffavg.real/refavg, 100.0*meandiffavg.imag/refavg)
	    if nonequalchanslist:
	        print "EPSILON EXCEEDED! Numerically distinct data on baseline %d, freq %d at MJD/sec %d/%7.2f found in %d/%d channels according to specified numerical precision." % (baseline[0], freqindex[0], mjd[0], seconds[0], len(nonequalchanslist), nchan[0])
	        if verbose:
	           print "Data that follows: [chX]={leftfile re,im + rightfile residual re,im}:"
	           for c in nonequalchanslist:
	              v0 = vis[0][c]
	              v1 = vis[1][c]
	              print '[ch%d]={%e,%e + %e,%e}  ' % (c, v0.real, v0.imag, (v1-v0).real, (v1-v0).imag)
	    else:
	        if (epsilon > 0) and verbose:
	            print "Record: baseline %d, freq %d at MJD/sec %d/%7.2f: data identical to numerical precision" % (baseline[0], freqindex[0], mjd[0], seconds[0])
    if (targetbaseline < 0 or targetbaseline == baseline[0]) and \
            (targetfreq < 0 or targetfreq == freqindex[0]) and \
	    disagreeskip < 0:
        recordcount += 1
    if (recordcount-skiprecords) > 0 and printinterval>0 and recordcount%printinterval == 0:
        print "After %d records, %d headers disagreed, the mean percentage absolute difference is %10.8f and the mean percentage mean difference is %10.8f + %10.8f i" % (recordcount-skiprecords, headerdisagreecount, 100.0*longtermabsdiff/(recordcount-skiprecords), 100.0*longtermmeandiff.real/(recordcount-skiprecords), 100.0*longtermmeandiff.imag/(recordcount-skiprecords))
    for i in range(numfiles):
        if i != disagreeskip:
            nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])

print "At the end, %d records disagreed on the header" % headerdisagreecount
if recordcount > 0:
    print "After %d records, the mean percentage absolute difference is %10.8f and the mean percentage mean difference is %10.8f + %10.8f i" % (recordcount-skiprecords, 100.0*longtermabsdiff/  (recordcount-skiprecords), 100.0*longtermmeandiff.real/(recordcount-skiprecords), 100.0*longtermmeandiff.imag/(recordcount-skiprecords))
