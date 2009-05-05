#!/usr/bin/python
import sys, os, math, numpy, pylab
from optparse import OptionParser
from numpy import *

#Subroutine to calculate number of bins
def getNumMergedBins(profile, numprofilebins):
    numbins = 0
    for i in range(numprofilebins):
        if profile[i] != profile[i-1]:
            numbins = numbins + 1
    return numbins

#Subroutine to merge smallest delta(profile)
def mergeSmallestDifference(profile, numprofilebins):
    smallestchange = 9e99
    index = 0
    for i in range(numprofilebins):
        deltaprofile = abs(profile[i] - profile[i-1])
        if deltaprofile > 0:
            leftwidth = 1
            rightwidth = 1
            while profile[i-leftwidth-1] == profile[i-1]:
                leftwidth = leftwidth + 1
            while profile[(i+rightwidth)%numprofilebins] == profile[i]:
                rightwidth = rightwidth + 1
            if deltaprofile < smallestchange:
                smallestchange = deltaprofile
                savedlw = leftwidth
                savedrw = rightwidth
                index = i

    if profile[index] == 0.0 or profile[index-1] == 0.0:
        profile[index] = 0.0
        profile[index-1] = 0.0
        return
    
    meanvalue = (savedlw*profile[index-1] + savedrw*profile[index])\
                /(savedlw+savedrw)
    for i in range(savedlw+savedrw):
        profile[(index-savedlw+i)%numprofilebins] = meanvalue
    return

#Main code
usage = "usage: %prog -f <file> [options]"
parser = OptionParser(usage)
parser.add_option("--profile", dest="profile", default="",
                  help="Filename of the pulsar profile file")
parser.add_option("--polyco", dest="polyco", default="",
                  help="Filename(s) (comma separated) of the polyco file(s)")
parser.add_option("-n", "--numbins", dest="numbins", default="20",
                  help="Number of bins in the output binconfig file")
parser.add_option("-s", "--scrunch", dest="doscrunch", default=False,
                  action="store_true",
                  help="Turn scrunching on in binconfig file")
parser.add_option("--binconfigfile", dest="binconfigfile", default="",
                  help="Filename of the output binconfig file")
(options, junk) = parser.parse_args()

profilefile = options.profile
polycofiles = (options.polyco).split(',')
numbins     = int(options.numbins)
doscrunch   = options.doscrunch
binconffile = options.binconfigfile
numpolycos  = len(polycofiles)

#Check everything necessary was supplied
if profilefile == "" or len(options.polyco) == "" or binconffile == "":
    print "You must supply a profile file, polyco file(s) and binconfig filename! Aborting!"
    sys.exit(1)

#Write the header of the output file
binconfout  = open(binconffile, 'w')
binconfout.write("NUM POLYCO FILES:   %i\n" % (numpolycos))
for i in range(numpolycos):
    binconfout.write(("POLYCO FILE %i:" % (i)).ljust(20))
    binconfout.write(polycofiles[i])
    binconfout.write("\n")
binconfout.write("NUM PULSAR BINS:    %i\n" % (numbins))
binconfout.write("SCRUNCH OUTPUT:     %s\n" % (str(doscrunch).upper()))

#If not scrunching, the bins are a piece of cake
if not doscrunch:
    for i in range(numbins):
        binconfout.write(("BIN PHASE END %i:"%(i)).ljust(20))
        binconfout.write("%f\n" % (float(2*i + 1)/float(2*numbins)))
        binconfout.write(("BIN WEIGHT %i:"%(i)).ljust(20))
        binconfout.write("1.0\n")
else:
    #Load the bins into memory
    profilein = open(profilefile, 'r')
    profilelines = profilein.readlines()
    profilein.close()
    numprofilebins = len(profilelines)-1
    profile = numpy.zeros(numprofilebins, numpy.float64)
    xvals = numpy.zeros(numprofilebins, numpy.float64)
    for i in range(numprofilebins):
        profile[i] = float((profilelines[i+1].split())[1])
        xvals[i] = float(i)/float(numprofilebins)

    #Calculate zero phase of the profile (to get the correct pulse phase)
    firstfourierterm = sum(profile * exp(-1j*2*math.pi*(1)*arange(numprofilebins)/numprofilebins))
    zerophase = math.atan2(float(firstfourierterm.imag), float(firstfourierterm.real))/(2.0*math.pi)
    if zerophase > 0.0:
        zerophase = zerophase - 1.0
    print "Zero phase is " + str(zerophase)
    for i in range(numprofilebins):
        xvals[i] = xvals[i] + 1.0 + zerophase

    #Plot the original profile
    pylab.plot(xvals, profile, 'b-')

    #Try and guess the noise
    profilemin = numpy.min(profile)
    profilemax = numpy.max(profile)
    noiseonly = []
    for val in profile:
        if val < -profilemin:
            noiseonly.append(val)
    stddev = numpy.std(noiseonly)

    #Zero all the noisy bits of the image
    windowsize = 1
    while math.pow(2, windowsize) < numprofilebins:
        windowsize = windowsize + 1
    windowsize = (windowsize-1)*2
    for i in range(numprofilebins):
        if not numpy.mean(profile[i-windowsize/2:i+windowsize/2]) > \
               4*stddev/math.sqrt(windowsize):
            profile[i] = 0.0

    #Go back and check for isolated noisy bits, squash them too
    for i in range(numprofilebins):
        if profile[i-1] == 0 and profile[(i+1)%numprofilebins] == 0:
            if profile[i-2] == 0 or profile[(i+2)%numprofilebins] == 0:
                profile[i] = 0.0
    print "Windowsize is " + str(windowsize) + ", stddev is " + str(stddev) + ", numprofilebins is " + str(numprofilebins)

    #Now merge bins until you reach required number
    outputbins = getNumMergedBins(profile, numprofilebins)
    print "Number of merged bins is " + str(outputbins)
    while outputbins > numbins:
        mergeSmallestDifference(profile, numprofilebins)
        outputbins = getNumMergedBins(profile, numprofilebins)

    #Dump the output to a png file for inspection
    pylab.xlabel("Pulsar phase", fontsize=16)
    pylab.ylabel("Unnormalised amplitude", fontsize=16)
    pylab.plot(xvals, profile, 'r-') #Plot the bin profile
    pylab.ylim(profilemin,profilemax)
    pylab.xlim(1.0+zerophase,2.0+zerophase)
    pylab.savefig("pulsarfilter.png")

    #Work out the bin phase edges and the values
    binphases = []
    binvalues = []
    lastvalue = profile[-1]
    for i in range(numprofilebins):
        if profile[i] != lastvalue:
            binphases.append(float(i)/float(numprofilebins))
            binvalues.append(lastvalue)
        lastvalue = profile[i]
    if len(binphases) != numbins:
        print "Whoops - I somehow averaged down to " + str(len(binphases)) + " bins, rather than " + str(numbins) + " like you asked! Sorry - aborting!"
        sys.exit(1)
    startpos = 0
    while startpos < numbins and (binphases[startpos]+zerophase)< 0.0:
        startpos = startpos + 1
    if startpos == numbins:
        zerophase = zerophase + 1.0
        startpos = 0
    while zerophase < 0.0:
        zerophase = zerophase + 1.0

    #Normalise the weights
    weightsum = 0.0
    for i in range(numbins):
        binw = binphases[i] - binphases[i-1]
        if binw < 0.0:
            binw = binw + 1.0
        weightsum = weightsum + binw*binvalues[i]

    for i in range(numbins):
        binvalues[i] = binvalues[i]/weightsum
        
    #Write the rest of the binconfig file
    for i in range(numbins):
        phase = binphases[(i+startpos)%numbins] + zerophase
        while phase > 1.0:
            phase = phase - 1.0
        binconfout.write(("BIN PHASE END %i:" % (i)).ljust(20))
        binconfout.write("%f\n" % (phase))
        binconfout.write(("BIN WEIGHT %i:"%(i)).ljust(20))
        binconfout.write("%f\n" % (binvalues[(i+startpos)%numbins]))

binconfout.close()
