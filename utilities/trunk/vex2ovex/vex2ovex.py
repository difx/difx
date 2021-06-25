#!/usr/bin/env python3
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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================
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
bands = {'L': 1250, 'S': 2000, "C": 4000, "X": 7000, "K": 13000, "Q": 40000, "V": 50000, "W": 75000, "B": 200000}

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
			
	station2site = {v: k for k, v in temp.items()}
			

	for i in range(index["SITE"]["start"],index["SITE"]["stop"]+1):
		# new definition
		match = re.match("^def\s*(.*);", content[i])
		if match:
			out.write(content[i])
			# look up station identifier
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
	count = 0
	for i in range(index["FREQ"]["start"],index["FREQ"]["stop"]+1):

		match = re.match("chan_def\s*=\s*:{0,1}\s*(\d+\.\d+)\s+(.*)", content[i])
		if match:
			freq = float(match.group(1))
			selFreq = 0.0
			for key, value in bands.items():
				if value < freq and value > selFreq:
					selFreq = value
					selBand = key
			if selFreq == 0.0:
				print("Error: Unknown frequency found in the FREQ section")
				sys.exit(1)
			count += 1

			out.write("chan_def = %s%d : &%s :%f %s\n" % (selBand, count, selBand, freq,match.group(2)) )
			continue
				
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
	for i in range(index["PHASE_CAL_DETECT"]["start"],index["PHASE_CAL_DETECT"]["stop"]+1):
		out.write(content[i])

def printTracks():
	''' 
	Prints the TRACKS section
	'''
	for i in range(index["TRACKS"]["start"],index["TRACKS"]["stop"]+1):
		match = re.match("track_frame_format\s*=\s*(.*);", content[i])
		if match:
			out.write("track_frame_format = Mark5B;\n");
			continue
		out.write(content[i])

def printPassOrder():
	''' 
	Prints the PASS_ORDER section
	'''
	if "PASS_ORDER" in index:
		for i in range(index["PASS_ORDER"]["start"],index["PASS_ORDER"]["stop"]+1):
			out.write(content[i])
	else:
		print("Warning: vex-file lacks the (obsolete) PASS_ORDER sectiong, adding a dummy into ovex-file for downstream compatibility.")
		out.write("$PASS_ORDER;\n");
		out.write("def DiskVoid;\n");
		out.write("enddef;\n");

def printHeadPos():
	''' 
	Prints the HEAD_POS section
	'''
	if "HEAD_POS" in index:
		for i in range(index["HEAD_POS"]["start"],index["HEAD_POS"]["stop"]+1):
			out.write(content[i])
	else:
		print("Warning: vex-file lacks the (obsolete) HEAD_POS sectiong, adding a dummy into ovex-file for downstream compatibility.")
		out.write("$HEAD_POS;\n");
		out.write("def DiskVoid;\n");
		out.write("enddef;\n");

def printRoll():
	''' 
	Prints the ROLL section
	'''
	for i in range(index["ROLL"]["start"],index["ROLL"]["stop"]+1):
		out.write(content[i])

def printSched():
	''' 
	Prints the SCHED section
	'''
	for i in range(index["SCHED"]["start"],index["SCHED"]["stop"]+1):
		out.write(content[i])

def printClock(preserveClocks=True):
	''' 
	Prints the CLOCK section
	'''
	startTime = "" 

	# determine the start time of the first scan
	for i in range(index["SCHED"]["start"],index["SCHED"]["stop"]+1):
		# match = re.match("start\s*=\s*(.*);\s*mode.*", content[i])
		match = re.match("start\s*=\s*(.*);", content[i])
		if match:
			startTime = match.group(1)
			break

	out.write("$CLOCK;\n")
	for i in range(index["CLOCK"]["start"]+1,index["CLOCK"]["stop"]+1):
		if preserveClocks:
			out.write(content[i])
		else:
			#match = re.match("def\s(.*)\s*;\s+clock_early.*", content[i])
			match = re.match("\s*def\s(.*?)\s*;", content[i])
			if match:
				#  def Az; clock_early=2015y085d00h00m :   0.0 usec :2015y085d00h00m0s : 0 ; enddef;
				out.write("def %s; clock_early=%s : 0.0 usec :%s : 0; enddef;\n" % ( match.group(1), startTime, startTime ))
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
			
			if 'start' in list(section.keys()):
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
		
def buildCodeMap():
	'''
	builds a dictionary containing the mapping from one to two letter station codes
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
			if len(token) == 0:
				continue
			if len(token) > 2:
				print("Error: Illegal format of the code mapping file. See help for details about the correct format")
				sys.exit(1)
			codes[token[1]] = token[0]
	else:
		# assign one letter codes starting at 'A'
		ascii = 65
		for key in codes:
			codes[key] =  chr(ascii)
			ascii += 1
	print("Using the following station code mapping (use --codes option to change):")
	for key,value in codes.items():
		print("%s = %s" % (key,value))
				
	
if __name__ == "__main__":

	description = "A script to convert a vex file to an ovex file to be used by the HOPS suite of programs (e.g. aedit)"

	epilog = "If no file for mapping one letter to two letter station codes is supplied (via the -c option) random one letter codes will be created.\n"
	epilog += "The mapping file should list in the first column the one letter code and in the second column the two letter code. it must contain all two letter codes found in the vex file"

	parser = argparse.ArgumentParser(description=description, epilog=epilog)
	parser.add_argument('vexfile', metavar='vexfile', type=argparse.FileType('r'),
		    help='the vex file to be converted to ovex')
	parser.add_argument('ovexfile', metavar='ovexfile',
		    help='the name of the output ovex file')
	parser.add_argument('-c', "--codes" , type=argparse.FileType('r'), dest="codes",
		    help='the name of the file containing the mappings of one letter to two letter station codes. For format of the mapping file see below.')
	parser.add_argument('-z', "--zero-clocks" , dest="doZeroClocks", action='store_true',
		    help='instead of copying the VEX $CLOCK section, set offsets and delays to zero and use the time of the first scan.')
	parser.add_argument('--version', action='version', version='%(prog)s ' + __version__)

	args = parser.parse_args()

	# check that output file does not exist yet
	if os.path.exists(args.ovexfile):
		print("Output file (%s) already exist. Delete it first" % args.ovexfile)
		sys.exit(1)

	# open output file
	out = open(args.ovexfile, "w")
	
	content, index = parseVex()

	# create the one letter to two letter station code mapping
	buildCodeMap()

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
	printClock(preserveClocks=not args.doZeroClocks)
	printEop()


	out.close()

	print("Successfully wrote %s" % args.ovexfile)
