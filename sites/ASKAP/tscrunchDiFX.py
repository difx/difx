#!/usr/bin/env python
import sys, os, struct, time, math
import parseDiFX
from optparse import OptionParser
from copy import deepcopy

def compareHeaders(header1, header2, freqs):
    match = True
    if header1.baseline != header2.baseline: #Different baselines
        match = False
    if header1.freqindex != header2.freqindex: #Different frequency index
        match = False
    #outputchan1 = freqs[header1.freqindex].numchan // freqs[header1.freqindex].specavg
    #outputchan2 = freqs[header2.freqindex].numchan // freqs[header2.freqindex].specavg
    #if outputchan1 != outputchan2: #Different number of channels
    #    match = False
    if header1.mjd != header2.mjd: #Different MJD
        match = False
    if header1.seconds != header2.seconds: #Different seconds
        #print(header1.seconds, header2.seconds)
        match = False
    return match


helpstr = "tscrunchDiFX.py [options] <reference difx file> <modified difx file>\n\n"
helpstr += "if a record is present in file 2 that is not present in file 1, insert a dummy entry in file 2 with zero val and low weight"
parser = OptionParser(helpstr)
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq") 
parser.add_option("-m", "--mjd", dest="mjd", default=-1.0, type=float,
                  help="Force the value of the header time to be this MJD")
(options, args) = parser.parse_args()

if len(args) != 2:
    parser.error("You must supply one (and only one) extant difx file, as well as a second non-extant difx filename. The first file will be read, all time integrations averaged, and written out into the second file")

verbose        = options.verbose
inputfile      = options.inputfile
forcemjd       = options.mjd

if inputfile == "":
    parser.error("You must supply an input file!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
if numfreqs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly!")

inputfilename = args[0]
if not os.path.exists(inputfilename):
    parser.error(inputfilename + " doesn't exist")
difxinput = open(inputfilename, 'rb')
record = parseDiFX.VisibilityRecord()
recordvalid = True
recordcount = 0
if os.path.exists(args[-1]):
    parser.error("Output file " + args[-1] + " already exists")

# Read and store all the visibilities, averaging in time
visibilities = {}
recordvalid = record.fromfile(difxinput, freqs)
mjdoffsetsum = 0
if recordvalid:
    refmjd = record.header.mjd + record.header.seconds/86400.0
else:
    refmjd = 0
while recordvalid:
    viskey = "{0:05d}-{1}-{2}-{3}".format(record.header.baseline, record.header.freqindex, record.header.polpair, record.header.pulsarbin)
    if not viskey in visibilities.keys():
        visibilities[viskey] = deepcopy(record)
    else:
        visibilities[viskey].vis *= visibilities[viskey].header.weight
        visibilities[viskey].header.u *= visibilities[viskey].header.weight
        visibilities[viskey].header.v *= visibilities[viskey].header.weight
        visibilities[viskey].header.w *= visibilities[viskey].header.weight
        visibilities[viskey].vis += record.header.weight*record.vis
        visibilities[viskey].header.u += record.header.weight*record.header.u
        visibilities[viskey].header.v += record.header.weight*record.header.v
        visibilities[viskey].header.w += record.header.weight*record.header.w
        visibilities[viskey].header.weight += record.header.weight
        visibilities[viskey].vis /= visibilities[viskey].header.weight
        visibilities[viskey].header.u /= visibilities[viskey].header.weight
        visibilities[viskey].header.v /= visibilities[viskey].header.weight
        visibilities[viskey].header.w /= visibilities[viskey].header.weight
        visibilities[viskey].header.uvw = [visibilities[viskey].header.u, visibilities[viskey].header.v, visibilities[viskey].header.w]

    recordcount += 1
    mjdoffsetsum += record.header.mjd + record.header.seconds/86400 - refmjd
    recordvalid = record.fromfile(difxinput, freqs)

# Figure out what time to use
if recordcount > 0:
    meanmjd = refmjd + mjdoffsetsum/recordcount
else:
    meanmjd = refmjd
if forcemjd >= 0:
    headermjd = int(forcemjd)
    headerseconds = (forcemjd - headermjd)*86400
else:
    headermjd = int(meanmjd)
    headerseconds = (meanmjd - headermjd)*86400

# Now write them all back out
outputdifxdir = args[-1][:args[-1].rindex('/')]
if not os.path.exists(outputdifxdir):
    os.mkdir(outputdifxdir)
difxoutput = open(args[-1], 'wb')

for key in sorted(visibilities.keys()):
    record = visibilities[key]
    record.header.mjd = headermjd
    record.header.seconds = headerseconds
    difxoutput.write(record.header.tobinary())
    difxoutput.write(record.vis)
difxoutput.close()

print("Read {0} records in, wrote {1} back out after averaging".format(recordcount, len(visibilities.keys())))
print("The difference (in seconds) between the chosen header time and the mean MJD in the input data was {0}".format(86400*(headermjd+headerseconds/86400 - meanmjd)))
print("All visibility times were set to MJD {0}".format(headermjd + headerseconds/86400.0))
