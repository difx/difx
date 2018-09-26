#!/usr/bin/python
import os,sys,glob,numpy

## ORIGINALLY, THIS LOOPED OVER DIRECTORIES, BUT NOW IT IS JUST TO BE RUN ONCE PER DIRECTORY, IN THE DIRECTORY
#targetdirs = sorted(glob.glob("c?_f?"))
#if len(targetdirs) == 0:
#    print "No directories with the expected format found!"
#    sys.exit()
#
#for t in targetdirs:
#    print t
#    os.chdir(t)

difxfile = glob.glob("craftfrbD2D.difx/DIFX*")[0]

# Look at the first and second chunk of 4 MHz separately
for freqgroup in range(2):
    difxfreq = 8 + freqgroup
    dscounts = {}
    os.system("rm -f offsets.txt")
    os.system('fringeFindDiFX.py -i craftfrbD2D.input %s -f %d | grep Detection | grep -v "channel 0" > offsets.txt' % (difxfile, difxfreq))
    for line in open("offsets.txt").readlines():
        splitline = line.split()
        dsids = splitline[6].split('/')[0][1:].split('-')
        if not dsids[0] in dscounts.keys():
            dscounts[dsids[0]] = 0
        if not dsids[1] in dscounts.keys():
            dscounts[dsids[1]] = 0
        channel = int(splitline[-4])
        # Assuming the fringe should be at 0, it will show up at + or - 28 channels if there
        # is an FPGA offset (depending on whether the antenna is first or second in the baseline)
        if channel == 28:
            dscounts[dsids[0]] -= 1
            dscounts[dsids[1]] += 1
        elif channel == -28:
            dscounts[dsids[0]] += 1
            dscounts[dsids[1]] -= 1
        else:
            print "Strange channel:", channel # This is usually a spurious fringe at low S/N
    countlist = []
    for key in sorted(dscounts.keys()):
        #print "Dscount for DS", key, "was", dscounts[key]
        countlist.append(numpy.abs(dscounts[key]))

    # Get the median: we're assuming that most antennas are good and so just a few will have high counts
    mediancount = numpy.median(countlist)
    os.system("cp craftfrb.input craftfrb.input.save") # Save of copy of the original input file
    for key in sorted(dscounts.keys()):
        if numpy.abs(dscounts[key]) > 2*mediancount:
            clockoffset = 7.0 # microsec
            if dscounts[key] < 0:
                clockoffset = -7.0 # microsec
            print "Adding offset of %f to datastream %s" % (clockoffset, key)

            # Go through and replace clock offset of 0 with 7 microseconds where necessary
            frbinputlines = open("craftfrb.input").readlines()
            linecount = 0
            while not "TELESCOPE INDEX:    %s" % key in frbinputlines[linecount]:
                linecount += 1
            overwrittencount = 0
            while overwrittencount < 4:
                while not "CLK OFFSET" in frbinputlines[linecount]:
                    linecount += 1
                if int(frbinputlines[linecount].split()[2]) / 4 == freqgroup:
                    frbinputlines[linecount] = "%s%.5f\n" % (frbinputlines[linecount][:20], clockoffset)
                    overwrittencount += 1
                linecount += 1

            # Now write the edited file back out
            newoutput = open("craftfrb.input","w")
            for line in frbinputlines:
                newoutput.write(line)
            newoutput.close()

#os.chdir("../")
