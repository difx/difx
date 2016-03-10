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

# Simple script to check the headers of VLBI data and print out a list of 
# valid and corrupt files.
# Cormac Reynolds: June 2012 - python parallel version replaces old perl script

from math import *
import os, sys, re, mx.DateTime, espressolib, subprocess

def lbaFileLength(filesize, headervals):

    byterate = headervals['BANDWIDTH'] * headervals['NUMBITS'] * headervals['NCHAN'] * 2 * 1e6/8.;
    filelength = (filesize - headervals['HEADERSIZE'])/byterate;
    return filelength;

def lbafile_timerange(filename, header):
    headerkeys = {'TIME': str, 'HEADERSIZE': int, 'NUMBITS': int, 'NCHAN': int, 'BANDWIDTH': float}

    parsehead = dict();
    for headline in header:
        for headerkey in headerkeys.keys():
            headline = headline.strip();
            if re.match(headerkey, headline):
                headline = headline.replace(headerkey, '')
                parsehead[headerkey] = headerkeys[headerkey](headline.strip())

    vexstarttime = '';
    vexendtime = '';

    if 'TIME' in parsehead.keys():
        headerdate = parsehead['TIME']
        headerformat = '%Y%m%d:%H%M%S'
        vexformat = '%Yy%jd%Hh%Mm%Ss'

        startdate = mx.DateTime.strptime(headerdate, headerformat)
        vexstarttime = startdate.strftime(vexformat)

        # calculate the length of the file in seconds
        filesize = os.path.getsize(filename)
        filelength = lbaFileLength(filesize, parsehead)

        enddate = startdate + mx.DateTime.RelativeDateTime(seconds=filelength)
        vexendtime = enddate.strftime(vexformat)

    return vexstarttime, vexendtime;

def vsib_header(filename):
    FILE = open(filename);
    header = FILE.read(4096).split("\n");

    return header;

def m5_to_vextime(m5time):
    '''Convert from m5time (MJD = MJD/hh:mm:ss.ss) to vex time'''
    
    m5time = m5time.split('=')[1]
    m5time = m5time.strip()
    # convert m5time to constitute parts, noting this match truncates the seconds (which vextime requires anyway)
    day, hours, mins, secs = re.match('(\d+)/(\d+):(\d+):(\d+)', m5time).groups()
    day = int(day)
    hours = int(hours)
    mins = int(mins)
    secs = int(secs)

    # account for fact that sometimes day wraps in m5time aren't recognised so get times like: 56990/24:00:01.00 instead of the expected 56991/00:00:01.00
    while hours >= 24:
        hours -= 24
        day += 1

    fracday = hours/24. + mins/(24.*60.) + secs/(24.*60.*60.)
    vextime = espressolib.convertdate(day + fracday, outformat='vex')

    return vextime

def check_file(infile):
    outfile = infile;
    m5time = espressolib.which('m5time')
    if not os.path.exists(infile):
        sys.stderr.write(infile +  " missing\n")
        outfile = '#' + outfile;
    elif re.search(r'.lba$', infile):
        header = vsib_header(infile);
        if not (header and re.match(r'^TIME\s\d{8}:\d{6}',  header[0]) ):
            if (header):
                sys.stderr.write("header for " + infile + " is corrupt: " + header[0] + "\n\n");
            else:
                sys.stderr.write("header for " + infile + " is corrupt or missing\n\n")
            outfile = '#' + outfile;

        starttime, endtime = lbafile_timerange(infile, header);

        # the last file should always get in the input so we can be sure the
        # D/STREAM is not empty (let's just hope it's not corrupt...). 
        comment = '';
        if (infile == filelist[len(filelist)-1]):
            comment = '#';
        outfile += " "  * 3 + comment + starttime + " " + endtime
    #elif re.search(r'.vdif$', infile):
    #    # arbitrary vdif format to get the time (not precise, but good enough).
    #    vdif_format = 'VDIF_1000-64-1-2'
    #    command = " ".join([m5time, infile, vdif_format])

    elif m5time:
        # assume it is a mark5 or vdif file of some description. Details of
        # the format are not important for extracting the start time. If we
        # don't have m5time in our path we simply will not do this.
        m5formats = ['Mark5B-512-16-2', 'VDIF_1000-64-1-2']
        # for MkIV and VLBA must get a matching (but not necessarily correct)
        # combination of fanout, number of bits and number channels. Check
        # nbits=2 formats first as they are much more common. Assume even
        # number channels. Datarate doesn't matter. I think this will match all
        # possible formats (without actually trying them).
        for nbits in [2,1]:
            for fanout in [1,2,4]:
                for nchan in [2,4,8,16]:
                    m4format = 'MKIV1_{0}-1024-{1}-{2}'.format(str(fanout), str(nchan), str(nbits))
                    m5formats.append(m4format)
                    vlbaformat = 'VLBA1_{0}-1024-{1}-{2}'.format(str(fanout), str(nchan), str(nbits))
                    m5formats.append(vlbaformat)
        starttime_m5 = []
        error = None
        for m5format in m5formats:
            command = " ".join([m5time, infile, m5format])
            starttime_m5, error = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
            if starttime_m5:
                #sys.stderr.write(m5format + '\n')
                # we have the right format. Find the time of a sample near
                # the end of the file (1 MB should be enough data)
                lastsample = 1000000
                if 'VDIF' in m5format:
                    # our own little millennium bug
                    endtime_m5 = 'MJD = 88069/00:00:00.00'
                else:
                    filesize = os.path.getsize(infile)
                    command = " ".join([m5time, infile, m5format, str(filesize-lastsample)])
                    try:
                        endtime_m5, error = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
                    except:
                        # old versions of m5time don't accept the byte offset
                        endtime_m5 = 'MJD = 88069/00:00:00.00'

                break

        if starttime_m5:
            starttime = m5_to_vextime(starttime_m5);
            endtime = m5_to_vextime(endtime_m5);
            # the last file should always get in the input so we can be sure the
            # D/STREAM is not empty (let's just hope it's not corrupt...). 
            comment = '';
            if (infile == filelist[len(filelist)-1]):
                comment = '#';
            outfile += " "  * 3 + comment + starttime + " " + endtime
        else:
            sys.stderr.write("cannot decode time for " + infile + "\n\n")
            outfile = '#' + outfile;

    return outfile


if __name__ == '__main__':
    # read the list of files
    filelist = []
    outfilelist = []
    for filelistname in sys.argv[1:]:
        filelist += open(filelistname).readlines()
        filelist = [line.rstrip() for line in filelist]
    
    # check each file, then print it. Corrupt/missing files get a comment
    # character prepended. File durations get appended for LBA and Mk5 files.
    for infile in filelist:
        outfile = check_file(infile)
        #outfilelist.append(outfile)
        print outfile
