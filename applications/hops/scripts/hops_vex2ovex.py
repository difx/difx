#!/usr/bin/env python 
# coding: latin-1
#===========================================================================
# Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id: vex2ovex.py 7638 2017-02-06 20:53:05Z HelgeRottmann $
# $HeadURL: https://svn.atnf.csiro.au/difx/utilities/trunk/vex2ovex/vex2ovex.py $
# $LastChangedRevision: 7638 $
# $Author: HelgeRottmann $
# $LastChangedDate: 2017-02-06 15:53:05 -0500 (Mon, 06 Feb 2017) $
#
#============================================================================
# Modified for use in HOPS, by gbc Jun 2017...
# Updated for Py3, by gbc Sep 2021...
#============================================================================
from __future__ import absolute_import
from __future__ import print_function
import sys
import argparse
import os.path
import re

__version__ = "1.1"
__author__ = "Helge Rottmann (MPIfR) <rottmann@mpifr.de>"

content = []
index = []
codes = {}
out = ""
bands = {'L': 1250, 'S': 2000, "C": 4000, "X": 7000,
    "K": 13000, "Q": 40000, "V": 50000, "W": 75000}

reSection = re.compile("^\$(.*);")

def getExpname():
    '''
    finds and returns the experiment name in the EXPER section.
    exits program if no exper_name statement was found
    '''

    # parse EXPER section
    for i in range(index["EXPER"]["start"],index["EXPER"]["stop"]+1):
        match = re.match("^exper_name\s*=\s*(.*);", content[i])
        if match:
            return match.group(1)

    # no exper_name statement found
    print("Error: no exper_name statement found in the vex-file")
    sys.exit(1)

def getEOPname():
    '''
    finds and returns the name of the first EOP definition found in the vexfile
    exits program if no EOP definition was found
    '''

    # parse EOP section
    for i in range(index["EOP"]["start"],index["EOP"]["stop"]+1):
        match = re.match("^def\s*(.*);", content[i])
        if match:
            return match.group(1)

    # no def statement found
    print("Error: no EOP definitions found in the vex-file")
    sys.exit(1)

def printGlobal(expname):
    ''' 
    outputs the GLOBAL section to the ovex file
    '''

    eopname = getEOPname()

    out.write("$OVEX_REV;\n")
    out.write("rev = 1.5;\n")
    out.write("$GLOBAL;\n");
    out.write("ref $EXPER = %s;\n" % expname)
    out.write("ref $EOP = %s;\n" % eopname)

def printExper():
    ''' 
    Prints the EXPER section
    '''
    for i in range(index["EXPER"]["start"],index["EXPER"]["stop"]+1):
        # don't output these lines
        if content[i].startswith("target_correlator"):
            continue
        out.write(content[i])

def printMode():
    ''' 
    Prints the MODE section
    '''
    for i in range(index["MODE"]["start"],index["MODE"]["stop"]+1):
        # don't output these lines
        if content[i].find("$PROCEDURE") != -1:
            continue
        out.write(content[i])

def printStation():
    ''' 
    Prints the STATION section
    '''
    station = ""
    addClock = False

    for i in range(index["STATION"]["start"],index["STATION"]["stop"]+1):
        # new definition
        match = re.match("^def\s*(.*);", content[i])
        if match:
            station = match.group(1)
            out.write(content[i])
            out.write("ref $CLOCK = %s;\n" % station)
            continue
        out.write(content[i])

def printSite():
    ''' 
    Prints the SITE section
    '''
    temp = {}
    #determine the station to site mapping
    for i in range(index["STATION"]["start"],index["STATION"]["stop"]+1):
        # new definition
        match = re.match("^def\s*(.*);", content[i])
        if match:
            station = match.group(1)
            temp[station] = ""
        match = re.match("^ref\s*\$SITE\s*=\s*(.*)\s*;", content[i])
        if match:
            temp[station] = match.group(1)
            # print(station, '=', temp[station])
    station2site = {temp[k]:k for k in temp}
    # print(station2site)

    for i in range(index["SITE"]["start"],index["SITE"]["stop"]+1):
        # new definition
        match = re.match("^def\s*(.*);", content[i])
        if match:
            out.write(content[i])
            # look up station identifier
            # print(match.group(1))
            station = station2site[match.group(1)]
            out.write("mk4_site_ID = %s;\n" % codes[station])
            continue
        out.write(content[i])

def printAntenna():
    ''' 
    Prints the ANTENNA section
    '''
    for i in range(index["ANTENNA"]["start"],index["ANTENNA"]["stop"]+1):
        # don't output these lines
        if content[i].startswith("axis_offset"):
            continue
        out.write(content[i])

def printDas():
    ''' 
    Prints the DAS section
    '''
    out.write(content[index["DAS"]["start"]])
    for i in range(index["DAS"]["start"],index["DAS"]["stop"]+1):
        if content[i].startswith("def") or content[i].startswith("enddef"):
            out.write(content[i])

def printSource():
    ''' 
    Prints the SOURCE section
    '''
    for i in range(index["SOURCE"]["start"],index["SOURCE"]["stop"]+1):
        out.write(content[i])

def printFreq():
    ''' 
    Prints the FREQ section
    '''
#   count = 0
    for i in range(index["FREQ"]["start"],index["FREQ"]["stop"]+1):
        match = re.match(
            "\s*chan_def\s*=\s*[&]*([A-Z])\s*:\s*(\d+\.\d+)\s+(.*)",
            content[i])
        if match:
            if match.group(1) == None:
                selBand = "B"
            else:
                selBand = match.group(1)
            if match.group(2):
                freq = float(match.group(2))
            else:
                freq = 0.0
#                   freq = float(match.group(2))
#                   selFreq = 0.0
#                   for key, value in bands.iteritems():
#                       if value < freq and value > selFreq:
#                           selFreq = value
#                           selBand = key
#                   count += 1
#                   out.write("chan_def = %s%d : &%s :%f %s\n" % (
            out.write("chan_def = %s :  :%f %s\n" % (
                selBand, freq,match.group(3)) )
            continue
        else:
            out.write(content[i])

def printIf():
    ''' 
    Prints the IF section
    '''
    for i in range(index["IF"]["start"],index["IF"]["stop"]+1):
        out.write(content[i])

def printBbc():
    ''' 
    Prints the BBC section
    '''
    for i in range(index["BBC"]["start"],index["BBC"]["stop"]+1):
        out.write(content[i])

def printPhaseCalDetect():
    ''' 
    Prints the PHASE_CAL_DETECT section
    '''
    for i in range(index["PHASE_CAL_DETECT"]["start"],
        index["PHASE_CAL_DETECT"]["stop"]+1):
            out.write(content[i])

def printTracks():
    ''' 
    Prints the TRACKS section
    '''
    for i in range(index["TRACKS"]["start"],index["TRACKS"]["stop"]+1):
        match = re.match("(.*track_frame_format\s*=\s*)([^;]*)(.*;)",
                    content[i])
        if match:
            if match.group(2)[0:4] == 'VDIF':
                out.write("%sVDIF%s\n"%(match.group(1),match.group(3)));
            elif match.group(2) == 'INTERLACEDVDIF':
                out.write("%sVDIF%s\n"%(match.group(1),match.group(3)));
        else:
            out.write(content[i])

def printPassOrder():
    ''' 
    Prints the PASS_ORDER section
    '''
    if ("PASS_ORDER" in index):
        for i in range(index["PASS_ORDER"]["start"],
            index["PASS_ORDER"]["stop"]+1):
                out.write(content[i])

def printHeadPos():
    ''' 
    Prints the HEAD_POS section
    '''
    if ("HEAD_POS" in index):
        for i in range(index["HEAD_POS"]["start"],index["HEAD_POS"]["stop"]+1):
            out.write(content[i])

def printRoll():
    ''' 
    Prints the ROLL section
    '''
    if ("ROLL" in index):
        for i in range(index["ROLL"]["start"],index["ROLL"]["stop"]+1):
            out.write(content[i])

def printSched():
    ''' 
    Prints the SCHED section
    '''
    for i in range(index["SCHED"]["start"],index["SCHED"]["stop"]+1):
        out.write(content[i])

def printClock():
    ''' 
    Prints the CLOCK section
    #  def Az; clock_early=2015y085d00h00m :
    #          0.0 usec :2015y085d00h00m0s : 0 ; enddef;
    '''
    startTime = "" 
    # determine the start time of the first scan
    for i in range(index["SCHED"]["start"],index["SCHED"]["stop"]+1):
        match = re.match("start\s*=\s*(.*);\s*mode.*", content[i])
        if match:
            startTime = match.group(1)
            break
    out.write("$CLOCK;\n")
    for i in range(index["CLOCK"]["start"],index["CLOCK"]["stop"]+1):
        #match = re.match("def\s(.*)\s*;\s+clock_early.*", content[i])
        match = re.match("def\s(.*?)\s*;", content[i])
        if match:
            out.write("def %s; clock_early=%s : 0.0 usec :%s : 0; enddef; \n" % (
                match.group(1), startTime, startTime ))
            continue

def printEop():
    ''' 
    Prints the EOP section
    '''
    for i in range(index["EOP"]["start"],index["EOP"]["stop"]+1):
        out.write(content[i])

def parseVex():
    '''
    Parses the vex file, discards any comments or empty lines
    The remaining lines are stored into an array.
    An index is constructed that contains the start and end lines
    of all sections discovered in the vex file
    Returns: vexcontent and index
    '''
    vexContent = []
    vexIndex = {}
    section = {}
    count = 0

    for vexline in args.vexfile:
        # strip leading whitespaces
        line = vexline.lstrip()
        # skip comments or empty lines
        if line.startswith('*') or len(line) == 0:
            continue

        # append line to vex buffer
        vexContent.append(line)

        # build index of the vex sections

        # new section start
        match = reSection.match(line)
        if match:
            
            if 'start' in section.keys():
                section["stop"] = count-1
                vexIndex[sectionName] = section
                section = {}
            section["start"] = count
            sectionName = match.group(1)
        count += 1

    # last section must be appended
    section["stop"] = count-1
    vexIndex[sectionName] = section
    return vexContent, vexIndex

def buildCodeMap(verb=False):
    '''
    builds a dictionary containing the mapping from one to
    two letter station codes
    '''
    # parse the STATION section
    for i in range(index["STATION"]["start"],index["STATION"]["stop"]+1):
        # new definition
        match = re.match("^def\s*(.*);", content[i])
        if match:
            codes[match.group(1).strip()] = ""
    # read external mapping file (if supplied)
    if args.codes is not None:
        for line in args.codes:
            token = line.split()
            if len(token) < 2:
                print("Error: Illegal format of the code mapping file."  +
                    "See help for details about the correct format")
                sys.exit(1)
            codes[token[1]] = token[0]
    else:
        # assign one letter codes starting at 'A'
        ascii = 65
        for key in codes:
            codes[key] =  chr(ascii)
            ascii += 1
        if verb:
            print("Using the following station code mapping" +
                   "(use --codes option to change):")
            for key in codes:
                value = codes[key]
                # print("%s = %s" % (key,value))

if __name__ == "__main__":
    description = ("A script to convert a vex file to an ovex file " +
        "to be used by the HOPS suite of programs (e.g. aedit)")
    epilog = ("If no file for mapping one letter to two letter station " +
        "codes is supplied (via the -c option) random one letter codes " +
            "will be created.\n")
    epilog += ("The mapping file should list in the first column the one " +
        "letter code and in the second column the two letter code. it must " +
            "contain all two letter codes found in the vex file")

    parser = argparse.ArgumentParser(description=description, epilog=epilog)
    parser.add_argument('vexfile', metavar='vexfile', type=argparse.FileType('r'),
        help='the vex file to be converted to ovex')
    parser.add_argument('ovexfile', metavar='ovexfile',
        help='the name of the output ovex file')
    parser.add_argument('-k', "--clobber" , dest="clobber",
        action="store_true", default=False,
        help='overwrite outputfile without complaint')
    parser.add_argument('-c', "--codes" ,
        type=argparse.FileType('r'), dest="codes",
        help='the name of the file containing the mappings of one letter '
        'to two letter station codes. For format of the mapping file see below.')
    parser.add_argument('-v', "--verbose", dest="verb",
        action="store_true", default=False,
        help='chatter some more')
    parser.add_argument('--version', action='version',
        version='%(prog)s ' + __version__)
    args = parser.parse_args()

    # check that output file does not exist yet
    if os.path.exists(args.ovexfile):
        if args.clobber:
            print('Overwriting output file (%s).' % args.ovexfile)
        else:
            print(("Output file (%s) already exist. Delete it first" %
                args.ovexfile))
            sys.exit(1)

    # open output file
    out = open(args.ovexfile, "w")
    content, index = parseVex()

    # create the one letter to two letter station code mapping
    buildCodeMap(args.verb)
    expName = getExpname()

    printGlobal(expName)
    printExper()
    printMode()
    printStation()
    printSite()
    printAntenna()
    printDas()
    printSource()
    printFreq()
    printIf()
    printBbc()
    printPhaseCalDetect()
    printTracks()
    printPassOrder()
    printHeadPos()
    printRoll()
    printSched()
    printClock()
    printEop()
    out.close()

    print("Successfully wrote %s" % args.ovexfile)
# eof
