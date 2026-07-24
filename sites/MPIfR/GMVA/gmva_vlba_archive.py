#!/usr/bin/env python
#**************************************************************************
#   Copyright (C) 2008-2012 by Walter Brisken and Helge Rottmann          *
#                                                                         *
#   This program is free software; you can redistribute it and/or modify  *
#   it under the terms of the GNU General Public License as published by  *
#   the Free Software Foundation; either version 3 of the License, or     *
#   (at your option) any later version.                                   *
#                                                                         *
#   This program is distributed in the hope that it will be useful,       *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
#   GNU General Public License for more details.                          *
#                                                                         *
#   You should have received a copy of the GNU General Public License     *
#   along with this program; if not, write to the                         *
#   Free Software Foundation, Inc.,                                       *
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
#**************************************************************************

import datetime
import os
import re
import shutil
import sys
import pyfits

PROGRAM = 'gmva_vlba_archive'
VERSION = '1.3'
VERDATE = '20220609'
AUTHOR  = 'Laura La Porta, Hermann Sturm, Walter Alef, Jan Wagner'

#####################################################################################

def vlba2mjd(d, t):
        '''Take strings date d ('yyyyMONdd') and time of day t ('hh mm ss'), and convert to fractional MJD'''

        mjd0 = datetime.datetime(1858, 11, 17, 0, 0)

        monnames = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
        yr = int(d[0:4])
        monname = d[4:7]
        mn = 0
        for m in range(len(monnames)):
                if monname == monnames[m]:
                        mn = m+1

        da = int(d[7:9])
        hr = int(t[0:2])
        mi = int(t[3:5])
        se = int(t[6:8])
        mjd = (datetime.datetime(yr, mn, da, 0, 0) - mjd0).days + hr/24.0 + mi/1440.0 + se/86400.0

        return mjd


def vex2vlba(vexdate):
        '''Take a VEX datetime string and return VLBA-style date and time of day'''

        monnames = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
        dt = datetime.datetime.strptime(vexdate, "%Yy%jd%Hh%Mm%Ss")
        vlbadate = '%4d%3s%02d' % (dt.year, monnames[dt.month-1], dt.day)
        vlbatime = '%02d %02d %02d' % (dt.hour, dt.minute, dt.second)

        return vlbadate, vlbatime


def vex2mjd(vexdate):
        '''Take a VEX datetime string and convert it to a fractional MJD,
           via an internal detour conversion to a VLBA date for legacy code reasons'''

        tmpdate, tmptime = vex2vlba(vexdate)
        return vlba2mjd(tmpdate, tmptime)


def readVEX(vexfile):
        '''From a VEX file grab the experiment name (exper_name) and time span (exper_nominal_start, exper_nominal_stop)'''

        expt, vexstart, vexstop = 'none', '2000y001d12h00m00s', '2000y001d12h00m00s'

        r = re.compile(r'=\s*([\d\w]*)\s*;')

        with open(vexfile,'r') as f:
            lines = f.readlines()

            for line in lines:
                if 'exper_nominal_start' in line:
                    vexstart = r.search(line).groups()[0]
                elif 'exper_nominal_stop' in line:
                    vexstop = r.search(line).groups()[0]
                elif 'exper_name' in line:
                    expt = r.search(line).groups()[0]

        return expt,vexstart,vexstop

def input_with_default(query, defaultchoice):
	msg = '%s [%s] ? ' % (query, defaultchoice)
	try:
		t = raw_input(msg) or defaultchoice
	except:
		# when cat /dev/null | gmva_vlba_archive.py ...
		# raw_input() errors out with "EOFError: EOF when reading a line":
		t = defaultchoice
	return str(t)

######################################################################################


if len(sys.argv) == 1:
    sys.exit(0)

if '-h' in sys.argv or '--help' in sys.argv:
    print('Produces a metadata file to use when uploading GMVA data to the NRAO Archive')
    print('Usage: gmva_vlba_archive.py <vexfile.vex> [<segment> <fitsfile>]')
    sys.exit(0)

default_segment  = 'none'
default_fitsfile = ''
default_tscope   = 'VLBA'
default_archfmt  = 'IDIFITS'
segmlist = ['none','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

exper, vexstart, vexstop = readVEX(sys.argv[1])
if len(sys.argv) >= 3:
	default_segment = sys.argv[2]
if len(sys.argv) >= 4:
	default_fitsfile = sys.argv[3]

print ("Please insert the Project Code of the experiment, that is composed by")
print ("2 letter and 3 numbers plus possibly a capital letter (called segment) at the end")
print ("E.g. gb077a")

exper    = input_with_default("Project Code", exper).upper()
vexstart = input_with_default("Observations started - vex time (format <yyyy>y<doy>d<hh>h<mm>m<ss>s)", vexstart)
vexstop  = input_with_default("Observations ended   - vex time (format <yyyy>y<doy>d<hh>h<mm>m<ss>s)", vexstop)
segm     = input_with_default("Segment (must be a capital letter, or 'none')", default_segment)
if segm not in segmlist:
	print("Error: Segment should be one of 'A' to 'Z' or 'none")
	sys.exit()
	
tscope   = input_with_default("Telescope", default_tscope)
arch_fmt = input_with_default("Archive format (UV_FITS or IDIFITS)", default_archfmt)
expname  = exper.lower()
if exper[-1] in segmlist:
	exper = exper[:-1]

suggestedName = 'GMVA_'+ expname + 'Part1.' + arch_fmt.lower()

print ("Please insert the filename of the FITS file.")
print ("The filename must be composed as follows: ")
print ("GMVA_<experiment name><part>.uvfits or with ending .idifits")
print ("(e.g. %s)." % (suggestedName))

fitsfilename = input_with_default("Filename of the FITS file", default_fitsfile)

check = 0
for i in range (26):
	rightname = 'GMVA_'+ expname + 'Part' + str(i) + '.uvfits'
	leftname = 'GMVA_'+ expname + 'Part' + str(i) + '.idifits'
	if  fitsfilename == rightname or  fitsfilename == leftname : check = 1
if check == 0:
	print ("The filename should be :")
	print (rightname)
	print ("Please check the filename and run again the program.")
	sys.exit()

pre, ext = os.path.splitext(fitsfilename)
ofilename = pre + '.metadata.txt'

file_size = float(os.path.getsize(fitsfilename))/float(1000)

suggested_bandcode = ''
try:
	ff = pyfits.open(fitsfilename)
	nu0_GHz = ff['FREQUENCY'].header['REF_FREQ'] * 1e-9
	if nu0_GHz >= 36 and nu0_GHz <= 46:
		suggested_bandcode = 'Q'
	elif nu0_GHz >= 75 and nu0_GHz <= 110:
		suggested_bandcode = 'W'
	print("Ref freq in FITS file was %.3f GHz, assuming band %s" % (nu0_GHz,suggested_bandcode))
except:
	pass
if suggested_bandcode == '':
	suggested_bandcode = 'W'

bandcode = input_with_default("Band code (Q, W, 'Q,W')", suggested_bandcode)

meta = open(ofilename,'w')
meta.write('PROJECT_CODE     = %s\n' % (exper))
meta.write('SEGMENT          = %s\n' % (segm))
meta.write('STARTTIME        = %13.7f\n' % (vex2mjd(vexstart)))
meta.write('STOPTIME         = %13.7f\n' % (vex2mjd(vexstop)))
meta.write('TELESCOPE        = %s\n'%(tscope))
meta.write('ARCH_FORMAT      = %s\n' % (arch_fmt))
meta.write('DATA_TYPE        = raw\n')
meta.write('ARCH_FILE        = %s\n'%(fitsfilename))
meta.write('FILE_SIZE        = %f\n'%file_size)
meta.write('RAW_PROJECT_CODE = %s\n'%(expname))
meta.write('OBS_BANDS        = %s\n'%(bandcode))
meta.close()

print('Wrote %s' % (ofilename))
