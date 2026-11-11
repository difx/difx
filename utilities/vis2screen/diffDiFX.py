#!/usr/bin/env python
import sys, os, struct, time, math, cmath
import parseDiFX

# Field order produced by parseDiFX.parse_output_header (both file styles)
headerfieldnames = ["baseline", "mjd", "seconds", "configindex", "srcindex",
                    "freqindex", "polpair", "pulsarbin", "weight", "u", "v", "w"]
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
parser.add_option("-d", "--diagnose", dest="diagnose", action="store_true", default=False,
                  help="On any difference, print extra diagnostics: labelled per-field " + \
                       "header comparisons, and a best-fit complex gain between differing " + \
                       "records (distinguishes amplitude scaling / phase rotation / corruption)")
parser.add_option("-w", "--weighttolerance", dest="weighttolerance", metavar="WEIGHTTOL",
                  default="1e-6",
                  help="Treat the weight header field as equal if the relative " + \
                       "difference is within WEIGHTTOL (it is a floating-point " + \
                       "accumulation/average, so the last few bits legitimately depend " + \
                       "on summation order) [default %default]")
parser.add_option("-u", "--uvwtolerance", dest="uvwtolerance", metavar="UVWTOL",
                  default="1e-9",
                  help="Treat the u/v/w header fields as equal if their relative " + \
                       "difference is within UVWTOL (they come from the same model " + \
                       "evaluation, so they should agree far tighter than weights) " + \
                       "[default %default]")
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
diagnose       = options.diagnose
matchheaders   = options.matchheaders
inputfile      = options.inputfile
weighttolerance = float(options.weighttolerance)
uvwtolerance   = float(options.uvwtolerance)
fieldtolerances = {headerfieldnames.index("weight"): weighttolerance}
for n in ("u", "v", "w"):
    fieldtolerances[headerfieldnames.index(n)] = uvwtolerance

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
    difxinputs.append(open(filename, 'rb'))
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
            print ("Have examined " + str(readrecords) + " - finishing up!")
            break
    for i in range(numfiles):
        baseline[i] = nextheader[i][0]
        freqindex[i] = nextheader[i][5]
        nchan[i] = freqs[freqindex[i]].numchan // freqs[freqindex[i]].specavg
        mjd[i] = nextheader[i][1]
        seconds[i] = nextheader[i][2]
        if nchan[i] > maxchannels:
            print ("How embarrassing - you have tried to read files with more than " + \
                  str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!")
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
    headerdiffers = False
    for j in range(len(nextheader[0])):
        if j >= len(headerfieldnames):
            continue   # raw header bytes; any real difference shows in the named fields
        if j in fieldtolerances:
            v1 = nextheader[0][j]
            v2 = nextheader[1][j]
            if abs(v2 - v1) <= fieldtolerances[j] * max(abs(v1), abs(v2)):
                continue
        if nextheader[0][j] != nextheader[1][j]:
            if not headerdiffers:
                headerdiffers = True
                headerdisagreecount += 1
                if diagnose:
                    print ("HEADER MISMATCH on baseline %d, freq %d at MJD/sec %d/%7.2f:" % \
                          (nextheader[0][0], nextheader[0][5], nextheader[0][1], nextheader[0][2]))
                elif verbose:
                    print ("Headers disagree!")
                    print (nextheader[0])
                    print (nextheader[1])
            if not diagnose:
                break
            fieldname = headerfieldnames[j] if j < len(headerfieldnames) else ("field%d" % j)
            try:
                ratio = float(nextheader[1][j]) / float(nextheader[0][j])
                print ("    %-11s file1 %s vs file2 %s (file2/file1 = %.6f)" % \
                      (fieldname, str(nextheader[0][j]), str(nextheader[1][j]), ratio))
            except (ValueError, ZeroDivisionError, TypeError):
                print ("    %-11s file1 %s vs file2 %s" % \
                      (fieldname, str(nextheader[0][j]), str(nextheader[1][j])))
    for c in compare:
        if c[0] > c[1]:
            disagreeskip = 0
        elif c[1] > c[0]:
            disagreeskip = 1
    if disagreeskip >= 0 and verbose:
        print ("Disagreeskip is now set to %d" % disagreeskip)
        print (compare)
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
                print ("THRESHOLD EXCEEDED! The percentage absolute difference on baseline %d, freq %d at MJD/sec %d/%7.2f is %10.8f, and the percentage mean difference is %10.8f + %10.8f i" % (baseline[0], freqindex[0], mjd[0], seconds[0], 100.0*absdiffavg/refavg, 100.0*meandiffavg.real/refavg, 100.0*meandiffavg.imag/refavg))
                if diagnose:
                    # Characterise the difference: fit the single complex gain g
                    # minimising |file2 - g*file1|^2. A pure amplitude scaling
                    # (e.g. a weight mismatch) shows as |g| != 1 with ~zero
                    # residual; a pure phase rotation (e.g. a fringe rotation /
                    # timing difference) shows as |g| ~= 1 with a nonzero angle
                    # and ~zero residual; corrupted/unrelated data shows as a
                    # large unexplained residual.
                    xy = complex(0.0, 0.0)
                    xx = 0.0
                    amp1avg = 0.0
                    for j in range(nchan[0]):
                        xy += vis[1][j] * vis[0][j].conjugate()
                        xx += abs(vis[0][j])**2
                        amp1avg = amp1avg + abs(vis[1][j])/nchan[0]
                    if xx > 0.0:
                        g = xy/xx
                        resid = 0.0
                        for j in range(nchan[0]):
                            resid += abs(vis[1][j] - g*vis[0][j])**2
                        residfrac = math.sqrt(resid/xx)
                        print ("    DIAGNOSTIC: mean amplitude file1 %.6e, file2 %.6e (file2/file1 = %.6f)" % \
                              (refavg, amp1avg, amp1avg/refavg if refavg > 0 else float('nan')))
                        print ("    DIAGNOSTIC: best-fit gain file2/file1: amplitude %.6f, phase %+.2f deg; unexplained residual %.4f%% of file1 rms" % \
                              (abs(g), math.degrees(cmath.phase(g)), 100.0*residfrac))
            if nonequalchanslist:
                print ("EPSILON EXCEEDED! Numerically distinct data on baseline %d, freq %d at MJD/sec %d/%7.2f found in %d/%d channels according to specified numerical precision." % (baseline[0], freqindex[0], mjd[0], seconds[0], len(nonequalchanslist), nchan[0]))
                if verbose:
                    print ("Data that follows: [chX]={leftfile re,im + rightfile residual re,im}:")
                    for c in nonequalchanslist:
                        v0 = vis[0][c]
                        v1 = vis[1][c]
                        print ('[ch%d]={%e,%e + %e,%e}  ' % (c, v0.real, v0.imag, (v1-v0).real, (v1-v0).imag))
            else:
                if (epsilon > 0) and verbose:
                    print ("Record: baseline %d, freq %d at MJD/sec %d/%7.2f: data identical to numerical precision" % (baseline[0], freqindex[0], mjd[0], seconds[0]))
    if (targetbaseline < 0 or targetbaseline == baseline[0]) and \
            (targetfreq < 0 or targetfreq == freqindex[0]) and \
	    disagreeskip < 0:
        recordcount += 1
    if (recordcount-skiprecords) > 0 and printinterval>0 and recordcount%printinterval == 0:
        print ("After %d records, %d headers disagreed, the mean percentage absolute difference is %10.8f and the mean percentage mean difference is %10.8f + %10.8f i" % (recordcount-skiprecords, headerdisagreecount, 100.0*longtermabsdiff/(recordcount-skiprecords), 100.0*longtermmeandiff.real/(recordcount-skiprecords), 100.0*longtermmeandiff.imag/(recordcount-skiprecords)))
    for i in range(numfiles):
        if i != disagreeskip:
            nextheader[i] = parseDiFX.parse_output_header(difxinputs[i])

print ("At the end, %d records disagreed on the header" % headerdisagreecount)
if recordcount > 0:
    print ("After %d records, the mean percentage absolute difference is %10.8f and the mean percentage mean difference is %10.8f + %10.8f i" % (recordcount-skiprecords, 100.0*longtermabsdiff/  (recordcount-skiprecords), 100.0*longtermmeandiff.real/(recordcount-skiprecords), 100.0*longtermmeandiff.imag/(recordcount-skiprecords)))
