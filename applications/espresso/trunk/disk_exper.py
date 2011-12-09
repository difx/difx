#!/usr/bin/python
# simple script to figure out where the data for a given experiment are from
# the output of disk_reporty.py and to format a default file for input to
# lbafilecheck.py
# This script assumes the canonical directory names: <exper>-<station>
# Cormac Reynolds. Original progam: July 2010

import sys, os, re

try:
    exper = sys.argv[1]
    disk_report_filename = sys.argv[2]
except:
    print 'Usage:', sys.argv[0], '<experiment> <disk.txt>', '''
will produce an input file for lbafilecheck.py for <experiment>, where
<disk.txt> is the output of disk_report.py'''
    sys.exit()

if not os.path.isfile(disk_report_filename):
    raise Exception( disk_report_filename + " not found!" )

disk_report = open(disk_report_filename).readlines()
disk_report = [ disk_report[i].strip('\n') for i in range(len(disk_report)) ]
disk_report = [ disk_report[i].strip() for i in range(len(disk_report)) ]

# parse the output file from disk_report.py
stationfiles = dict()
for line in disk_report:
    if 'Disk report for' in line:
        machine = line.split()[-1]
    if re.search('/' + exper + '-\w*$', line):
        directory = line.split()[-1]
        station = line.split('-')[-1].lower()
        if not station in stationfiles:
            stationfiles[station] = dict()
            stationfiles[station]['dir'] = []
        stationfiles[station]['dir'].append(directory)
        stationfiles[station]['machine'] = machine

# and format the output file as expected by lbafilecheck.py
outfile_name = exper + '.datafiles'
OUTFILE = open(outfile_name, 'w')

#print>>OUTFILE, exper, '*.lba *no*'
print>>OUTFILE, exper, '*'
for station in stationfiles:
    print>>OUTFILE, station, '=', 
    print>>OUTFILE, stationfiles[station]['machine'] + ':',
    for directory in stationfiles[station]['dir']:
        print>>OUTFILE, directory + os.sep,
    print>>OUTFILE

