#!/usr/bin/python
import sys, os, struct, time, math
import parseDiFX
from optparse import OptionParser

helpstr = "snipDiFX.py [options] <input difx file> <output difx file>\n\n"
parser = OptionParser(helpstr)
parser.add_option("-t", "--timerange", dest="timerange", metavar="TIMERANGE", default="",
                  help="starttime,stoptime where times are fractional days from corr start day")
parser.add_option("-c", "--maxchannels", dest="maxchannels", metavar="MAXCHANNELS",
		  default="16384", 
		  help="The length of the array that will be allocated to hold vis results")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")
parser.add_option("-i", "--inputfile", dest="inputfile", default="",
                  help="The input file used for the correlation") 
parser.add_option("-k", "--clobber", dest="clobber", default=False,
                  action='store_true',
                  help="clobber any exiting output file") 
(options, args) = parser.parse_args()
maxchannels     = int(options.maxchannels)
verbose         = options.verbose
inputfile       = options.inputfile
timerange       = options.timerange.split(',')
clobber            = options.clobber
if len(args) != 2:
    parser.error("You must supply an input difx file to snip and an output filename!")
if inputfile == "":
    parser.error("You must supply an input file!")
if len(timerange) != 2:
    parser.error("You must supply --timerange=starttime,stoptime")
(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
if numfreqs == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly!")
if not os.path.exists(args[0]):
    parser.error("Input difx file %s doesn't exist!" % (args[0]))
if os.path.exists(args[1]) and clobber:
    os.unlink(args[1])
if os.path.exists(args[1]):
    parser.error("Output difx file %s already exists!" % (args[1]))
starttime = float(timerange[0])
stoptime  = float(timerange[1])
    
if sys.version_info.major < 3:
    difxinput  = open(args[0], 'r')
    difxoutput = open(args[1], 'w')
else:   # Python3 requires binary opens
    difxinput  = open(args[0], 'rb')
    difxoutput = open(args[1], 'wb')
vis        = []
for j in range(maxchannels):
    vis.append(0.0)

nextheader = parseDiFX.parse_output_header(difxinput)
if len(nextheader) == 0:
    print ("Problem with input file " + args[0] + " - aborting")
    sys.exit()

startmjd = nextheader[1]
while not len(nextheader) == 0:
    freqindex = nextheader[5]
    nchan = freqs[freqindex].numchan/freqs[freqindex].specavg
    if nchan > maxchannels:
        print ("How embarrassing - you have tried to read files with more than " + \
              str(maxchannels) + " channels.  Please rerun with --maxchannels=<bigger number>!")
        sys.exit()
    mjd = nextheader[1]
    seconds = nextheader[2]
    buffer = difxinput.read(int(8*nchan))
    dayfrac = (mjd - startmjd) + float(seconds)/86400.0
    if dayfrac >= starttime and dayfrac <= stoptime:
        if verbose:
            print ("Writing time %d/%.6f to output" % (mjd, seconds))
        difxoutput.write(nextheader[-1])
        difxoutput.write(buffer)
    nextheader = parseDiFX.parse_output_header(difxinput)

difxinput.close()
difxoutput.close()
