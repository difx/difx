#!/usr/bin/python
# Simple script to check the headers of VLBI data and print out a list of 
# valid and corrupt files.
# Cormac Reynolds: June 2012 - python parallel version replaces old perl script

from math import *
import os, sys, re, mx.DateTime

def lbaFileLength(filesize, headervals):

    byterate = headervals['BANDWIDTH'] * headervals['NUMBITS'] * headervals['NCHAN'] * 2 * 1e6/8.;
    filelength = (filesize - headervals['HEADERSIZE'])/byterate;
    return filelength;

def file_timerange(filename, header):
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

def check_file(infile):
    outfile = infile;
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

        starttime, endtime = file_timerange(infile, header);

        # the last file should always get in the input so we can be sure the
        # D/STREAM is not empty (let's just hope it's not corrupt...).
        comment = '';
        if (infile == filelist[len(filelist)-1]):
            comment = '#';
        outfile += " "  * 3 + comment + starttime + " " + endtime

    return outfile

if __name__ == '__main__':
    # read the list of files
    filelist = []
    outfilelist = []
    for filelistname in sys.argv[1:]:
        filelist += open(filelistname).readlines()
        filelist = [line.rstrip() for line in filelist]
    
    # check each file, then print it. Corrupt/missing files get a comment
    # character prepended. File durations get appended for LBA files.
    for infile in filelist:
        outfile = check_file(infile)
        #outfilelist.append(outfile)
        print outfile
