#!/usr/bin/python
# simplistic script to add file timeranges to a mk5 filelist simply by
# comparing file names to scan names in the vex file.
# Cormac Reynolds: September 2011

import optparse, re, os, sys, time, fileinput, pprint, copy


def parse_vexsched(vexfilename):
    # get a list of scan start times from the VEX file.

    vexfile = open(vexfilename).read()
    #vexfile = re.sub(r'\n', '', vexfile)
    vexfile = re.split(r'[\n;]', vexfile)

    scans = []
    schedblock = False
    #prevscanname = str()
    for line in vexfile:
        # remove comments
        line = re.sub(r'\*.*', '', line)

        # restrict ourselves to the sched block.
        if '$SCHED' in line:
            schedblock = True
        elif schedblock and re.match(r'\s*\$', line):
            schedblock = False
        if schedblock:
            scannameline = re.match(r'\s*scan\s+(\w*)', line)
            if scannameline:
                scanname = scannameline.group(1).lower()

            scanstartline = re.match(r'\s*start=\s*(\w*)', line)
            if scanstartline:
                scanstart = scanstartline.group(1)

                scans.append([scanname, scanstart])

    # and the last scan
    scans.append(['final', '2999y001d00h00m00s'])

    return scans

# Main program start.
#parse the options
usage = '''%prog <vexfile> <filelist1> <filelist2> ...
    will append the start times of each scan from the <vexfile> to matching
    lines in the <filelist> files.'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')

(options, args) = parser.parse_args()
if len(args) < 2:
    parser.print_help()
    parser.error("Give a VEX file and at least one .filelist file")

vexfilename = args.pop(0)
filelistnames = args

scans = parse_vexsched(vexfilename)

for filelistfilename in filelistnames:
    # figure out the timeranges of the files.
    filelistfile = open(filelistfilename).readlines()
    filelength = len(filelistfile)
    scanrange = []
    for line in filelistfile:
        for iscan in range(len(scans)):
            scanname = scans[iscan][0]
            if scanname.lower() in line.lower():
                if len(scanrange):
                    scanrange[-1] += ' ' + scans[iscan][1]
                scanrange.append(scans[iscan][1] )

    scanrange[-1] += ' ' + '2999y001d00h00m00s'


    # add the timeranges to the filelist.
    filelistfile = fileinput.FileInput(filelistfilename, inplace=1, backup='.org')
    for line in filelistfile:
        line = line.rstrip('\n')
        if (filelistfile.lineno() == filelength):
            line += ' #'
        line += ' ' * 7 + scanrange.pop(0)
        print line

    fileinput.close()
