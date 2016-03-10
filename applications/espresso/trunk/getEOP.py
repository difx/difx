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

# Cormac Reynolds, December 2011: program to fetch 5 days of EOPs around the
# given date and return in format suitable for appending to .v2d file.
# Takes dates in MJD or VEX format.
# EOP data from http://gemini.gsfc.nasa.gov/solve_save/usno_finals.erp
# Leap second data come from: http://gemini.gsfc.nasa.gov/500/oper/solve_apriori_files/ut1ls.dat

import optparse, re, urllib, espressolib, sys, time

def get_leapsec(leapsec_page, targetJD):
    # parse the leap seconds page
    for line in leapsec_page:
        linedate = float(line[17:27])
        if linedate > targetJD:
            break
        else:
            tai_utc = float(line[38:49])
    return tai_utc

usage = '''%prog <date>
<date> can either be MJD or VEX time
Returns 5 days of EOPs around <date> in .v2d format'''
parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
(options, args) = parser.parse_args()

if len(args) != 1:
    parser.print_help()
    parser.error("Give a date in MJD or VEX format")


# get target MJD
targetMJD = args[0];
# convert to MJD if necessary
targetMJD = espressolib.convertdate(targetMJD, 'mjd')

#print targetMJD

targetJD  = round(targetMJD) + 2400000.5;

# dates before June 1979 not valid (earliest EOPs)
if (targetJD < 2444055.5):
    raise Exception('Date too early. No valid EOPs before July 1979')

# fetch EOP data
eop_url = "http://gemini.gsfc.nasa.gov/solve_save/usno_finals.erp"
leapsec_url = "http://gemini.gsfc.nasa.gov/500/oper/solve_apriori_files/ut1ls.dat"

print >>sys.stderr, "Fetching EOP data..."
eop_page = urllib.FancyURLopener().open(eop_url).readlines()
print >>sys.stderr, "...got it."

print >>sys.stderr, "Fetching Leap second data..."
leapsec_page = urllib.FancyURLopener().open(leapsec_url).readlines()
print >>sys.stderr, "...got it.\n";


#print "#EOPs downloaded at", datetime.datetime.now()
print "#EOPs downloaded at", time.strftime('%Y-%m-%d %H:%M:%S (%z)')

# parse the eop page
nlines = 0
for line in eop_page:
    nlines += 1
    # skip the first line, which isn't commented for some reason
    if (nlines == 1):
        continue
    # strip comments
    if (line[0] == '#'):
        continue
    # split the line on whitespace and convert to floats
    eop_fields = line.split()
    eop_fields = [float(field) for field in eop_fields]
    # print an EOP line if we're within 3 days of the target day
    if (abs(eop_fields[0] - targetJD) < 3):
        tai_utc = None;
        tai_utc = get_leapsec(leapsec_page, eop_fields[0])
        if not tai_utc:
            raise Exception("Leap seconds not found! Check your dates")
        print "EOP %d { xPole=%f yPole=%f tai_utc=%d ut1_utc=%f }" % (eop_fields[0]-2400000.5,eop_fields[1]/10.,eop_fields[2]/10.,tai_utc,tai_utc+eop_fields[3]/1000000.)

print >>sys.stderr, "Processed %d lines" % nlines;
