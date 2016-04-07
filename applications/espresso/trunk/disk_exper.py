#!/usr/bin/env python
#=======================================================================
# Copyright (C) 2016 Cormac Reynolds
#                                                                       
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by  
# the Free Software Foundation; either version 3 of the License, or     
# (at your option) any later version.                                   
#                                                                       
# This program is distributed in the hope that it will be useful,       
# but WITHOUT ANY WARRANTY; without even the implied warranty of        
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
# GNU General Public License for more details.                          
#                                                                       
# You should have received a copy of the GNU General Public License     
# along with this program; if not, write to the                         
# Free Software Foundation, Inc.,                                       
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             
#=======================================================================

# simple script to figure out where the data for a given experiment are from
# the output of disk_reporty.py and to format a default file for input to
# lbafilecheck.py
# This script assumes the canonical directory names: <exper>-<station>
# Cormac Reynolds. Original progam: July 2010

import sys, os, re
import json

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

disk_report = json.load(open(disk_report_filename))

# parse the output file from disk_report.py
stationfiles = dict()
for machine in disk_report.keys():
    for data_area in disk_report[machine].keys():
        for line in disk_report[machine][data_area]['du']:
            directory =  line[1]
            #print directory
            if re.search('/' + exper + '-\w*$', directory):
                station = directory.split('-')[-1].lower()
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
for station in sorted(stationfiles):
    print>>OUTFILE, station, '=', 
    print>>OUTFILE, stationfiles[station]['machine'] + ':',
    for directory in stationfiles[station]['dir']:
        print>>OUTFILE, directory + os.sep,
    print>>OUTFILE

