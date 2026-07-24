#!/usr/bin/env python
import sys, os, struct, time, math, random
import parseDiFX
from optparse import OptionParser

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


helpstr = "fillDiFX.py [options] <check difx file> <reference difx file> <modified difx file>\n\n"
helpstr += "if a record is present in file 2 that is not present in file 1, insert a dummy entry in file 2 with zero val and low weight"
parser = OptionParser(helpstr)
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="An input file to use as guide for number of channels for each freq") 
(options, args) = parser.parse_args()

if len(args) != 3:
    parser.error("You must supply two (and only two) extant difx files, as well as a third non-extant difx filename. Any records missing from the first that are present in the second will have a dummy value inserted - the modified file will be written out to the third")

numfiles = len(args) - 1

verbose        = options.verbose
inputfile      = options.inputfile

if inputfile == "":
    parser.error("You must supply an input file!")

(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
if numfreqs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly!")

difxinputs  = []
recordvalid = []
records     = []
recordcount = []
for filename in args[:-1]:
    if not os.path.exists(filename):
        parser.error(filename + " doesn't exist")
    difxinputs.append(open(filename, 'rb'))
    records.append(parseDiFX.VisibilityRecord())
    recordvalid.append(True)
    recordcount.append(0)
if os.path.exists(args[-1]):
    parser.error("Output file " + args[-1] + " already exists")
outputdifxdir = args[-1][:args[-1].rindex('/')]
if not os.path.exists(outputdifxdir):
    os.mkdir(outputdifxdir)
difxoutput = open(args[-1], 'wb')

for i in range(numfiles):
    recordvalid[i] = records[i].fromfile(difxinputs[i], freqs)

present = {}
while recordvalid[0]:
    didskip = False
    while recordvalid[1] and not compareHeaders(records[0].header, records[1].header, freqs) and (86400*records[0].header.mjd + records[0].header.seconds) >= (86400*records[1].header.mjd + records[1].header.seconds):
        # Check if the baseline is not already there, or this frequency was not written for this baseline
        if not str(records[1].header.baseline) in present.keys() or not records[1].header.freqindex in present[str(records[1].header.baseline)]:
            # Catch the case where low frequencies are missing from the first time integration of the reference correlation!
            if 86400*records[0].header.mjd + records[0].header.seconds == 86400*records[1].header.mjd + records[1].header.seconds and (records[1].header.baseline > records[0].header.baseline or (records[1].header.baseline == records[0].header.baseline and records[1].header.freqindex > records[0].header.freqindex)):
                if verbose:
                    print("*Not* inserting a record from frequency index {0}".format(records[1].header.freqindex))
                    break
            # Write out header 2 into the output (but with a weight of 0.0001
            if verbose:
                print("Filling in a record: baseline {0}, freqindex {1}, time {2}:{3}".format(records[1].header.baseline, records[1].header.freqindex, records[1].header.mjd, records[1].header.seconds))
            records[1].header.weight = 0.0001
            difxoutput.write(records[1].header.tobinary())
            records[1].vis.fill(0) 
            middlechan = (freqs[records[1].header.freqindex].numchan // freqs[records[1].header.freqindex].specavg) // 2
            records[1].vis[middlechan] = random.random()/1e9 + 0j
            difxoutput.write(records[1].vis)
            recordcount[1] += 1
        # Catch the case where low frequencies are missing from the first time integration of the reference correlation!
        if 86400*records[0].header.mjd + records[0].header.seconds == 86400*records[1].header.mjd + records[1].header.seconds and (records[1].header.baseline > records[0].header.baseline or (records[1].header.baseline == records[0].header.baseline and records[1].header.freqindex > records[0].header.freqindex)):
            break
        recordvalid[1] = records[1].fromfile(difxinputs[i], freqs)
        didskip = True
    # now write out this record too
    if verbose:
        print("Writing a desired record: baseline {0}, freqindex {1}, time {2}:{3}".format(records[0].header.baseline, records[0].header.freqindex, records[0].header.mjd, records[0].header.seconds))
    if not str(records[0].header.baseline) in present.keys():
        present[str(records[0].header.baseline)] = [records[0].header.freqindex]
    else:
        if not records[0].header.freqindex in present[str(records[0].header.baseline)]:
            present[str(records[0].header.baseline)].append(records[0].header.freqindex)
    difxoutput.write(records[0].header.tobinary())
    difxoutput.write(records[0].vis)
    recordcount[0] += 1
    if compareHeaders(records[0].header, records[1].header, freqs):
        recordvalid[1] = records[1].fromfile(difxinputs[1], freqs)
        if verbose:
            print("Advanced to next reference file visibility: baseline {0}, freq {1}, pol {2}".format(records[1].header.baseline, records[1].header.freqindex, records[1].header.polpair))
    recordvalid[0] = records[0].fromfile(difxinputs[0], freqs)
while recordvalid[1]:
    if not str(records[1].header.baseline) in present.keys() or not records[1].header.freqindex in present[str(records[1].header.baseline)]:
        records[1].header.weight = 0.0001
        difxoutput.write(records[1].header.tobinary())
        records[1].vis.fill(0)
        middlechan = (freqs[records[1].header.freqindex].numchan // freqs[records[1].header.freqindex].specavg) // 2
        records[1].vis[middlechan] = random.random()/1e9 + 0j
        difxoutput.write(records[1].vis)
        recordcount[1] += 1
    recordvalid[1] = records[1].fromfile(difxinputs[i], freqs)
#if recordcount[0] == 0: # Must have been totally empty - fill it with the filler file
#    while recordvalid[1]:
#        records[1].header.weight = 0.0001
#        difxoutput.write(records[1].header.tobinary())
#        records[1].vis.fill(0)
#        difxoutput.write(records[1].vis)
#        recordvalid[1] = records[1].fromfile(difxinputs[i], freqs)
#        recordcount[1] += 1
difxoutput.close()
print("Wrote {0} records from the desired file plus {1} records from the filler file".format(recordcount[0], recordcount[1]))
