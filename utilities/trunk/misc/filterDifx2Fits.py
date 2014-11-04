#!/usr/bin/python
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id: $
# $HeadURL: $
# $LastChangedRevision:  $
# $Author: $
# $LastChangedDate: $
#
#============================================================================
import os
import sys
import glob
import re
import datetime
from optparse import OptionParser

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision: $"
__date__ ="$Date: $"
__lastAuthor__="$Author: $"


def getUsage():

	usage = "%prog [options] <outname> <baseFilename1> [<baseFilename2> ... ]\n\n"
        usage += '\nA program to allow filtering of scans to be used for running difx2fits.'
        usage += '\nScans can be filtered by: timerange, source and mode (see below).'
        usage += '\nAddtional difx2fits options can be passed with -d (see below)'
        usage += '\n\n This program produces a bash script (<outname>.difx2fits) to execute difx2fits.'
        usage += '\n The fits file produced by the script will be named <outname>.fits'
        usage += '\n\n<outname> : base name for the output files <outname>.difx2fits and <outname>.fits)'
        usage += '\n<baseFileNameN> : name of the .difx output directories to consider.'
        usage += '\n\tCan contain wildcards (e.g. r1111_*)\n'
        return usage

def getVersion():
	version = "%s\nSVN  %s\nOriginal author: %s\nLast changes by: %s\nLast changes on: %s" % (__prog__, __build__, __author__, __lastAuthor__, __date__)
	
	return version

def commaseparate_callback(option, opt, value, parser):
	setattr(parser.values, option.dest, value.split(','))

def buildCommandFile(baseName, fileList):

	commandFileName = baseName + ".difx2fits"
	fitsFileName = baseName + ".fits"

	commandFile = open(commandFileName, "w")

	commandFile.write("#!/bin/bash\n\n")
	commandFile.write("# this file was produced by filterDifx2Fits\n")
	if options.source == None:
		commandFile.write("# sources: all\n")
	else:
		sources = ""
		commandFile.write("# sources: " )
		for source in options.source:
			sources  += source + ", "
		commandFile.write(sources[:-2] + "\n")

	if options.mode == None:
		commandFile.write("# mode: all\n")
	else:
		commandFile.write("# mode: " + options.mode + "\n")

	if options.timerange == None:
		commandFile.write("# time range: all\n")
	else:
		ranges = ""
		commandFile.write("# time range: " )
		for range in options.timerange:
                        ranges  += range + ", "
                commandFile.write(ranges [:-2] + "\n")
	
	if len(fileList) == 0:
		return
	commandFile.write("difx2fits \\\n")
	if options.d2fOptions is not None:
		commandFile.write(options.d2fOptions + " \\\n")
	
	for file in fileList:
		commandFile.write(file + " \\\n")
	commandFile.write (fitsFileName + "\n")
	commandFile.close()

	print "Wrote %s" % commandFileName


#main
if __name__ == "__main__":

	usage = getUsage()
	version = getVersion()

	parser = OptionParser(version=version, usage=usage)
	parser.add_option("-m", "--mode", type="string", help="select mode to be included in FITS file. Must match vex mode.")
	parser.add_option("-t", "--timerange", type="string", action='callback', callback=commaseparate_callback, help="select timerange (start-stop) in the vex-format, e.g. 2014y293d17h00m00s-2014y293d18h45m00s. Multiple timeranges can be separated by comma.")
	parser.add_option("-s", "--source", type="string", action='callback', callback=commaseparate_callback, help="select sources to be included in the fits file. Multiple sources can be separated by comma")
	parser.add_option("-d", "--difx2fits", type="string", dest="d2fOptions", help='specify extra options to be passed to difx2fits. Note: when supplying multiple options these must be surrounded with quotes, e.g. -d "--zero -v" .')

	(options, args) = parser.parse_args()

	if len(args) < 2:
		print getUsage()
		sys.exit(1)

	# parse timeranges
	pattern = re.compile("(\d{4}y\d{1,3}d\d{1,2}h\d{1,2}m\d{1,2}s)\-(\d{4}y\d{1,3}d\d{1,2}h\d{1,2}m\d{1,2}s)")
	# 
	rangeList = []
	if options.timerange is not None:
		for range in options.timerange:
			m = pattern.match(range)
			rangeItem = {}
			if m:
				start = datetime.datetime.strptime(m.group(1),'%Yy%jd%Hh%Mm%Ss')
				stop = datetime.datetime.strptime(m.group(2),'%Yy%jd%Hh%Mm%Ss')
				rangeItem["start"] = start
				rangeItem["stop"] = stop
				rangeList.append(rangeItem)
			else:
				print "Error in the timerange specification"
				print "Must be in vex syntax, e.g. 2014y193d17h00m00s-2014y193d17h45m00s"
				sys.exit(1)

	# use first argument as the project code and construct output filenames
	baseName = args[0]


	# loop over calc files
	files = glob.glob(args[0])
	
	includeList = []
	for arg in args:
		if not arg.endswith(".difx"):
			continue

		# open the corresponding calc file
		calc = open(arg[:-5]+ ".calc", "r")

		sourceName = ""
		mode = ""
		year = month = day = hour = minute = second = ""

		modeMatch = True
		rangeMatch = True
		sourceMatch = True

		for line in calc:
			cols = line.split(":")

			# check that output was produced in scan based mode
			if cols[0] == "NUM SCANS":
				if cols[1].strip() != "1":
                                                sys.exit("calc file contains multiple scans: %s" % arg[:-5]+ ".calc")
			
			# check that the calc file contains a single source only
			if cols[0] == "NUM SOURCES":
				if cols[1].strip() != "1":
					sys.exit("calc file contains multiple sources: %s" % arg[:-5]+ ".calc")
			# parse source name 
			if cols[0] == "SOURCE 0 NAME":
				sourceName = cols[1].strip()

			# parse mode
			if cols[0] == "SCAN 0 OBS MODE NAME":
				mode = cols[1].strip()

			# parse start time
			if cols[0] == "START YEAR":
				year = int(cols[1].strip())
			if cols[0] == "START MONTH":
				month = int(cols[1].strip())
			if cols[0] == "START DAY":
				day = int(cols[1].strip())
			if cols[0] == "START HOUR":
				hour = int(cols[1].strip())
			if cols[0] == "START MINUTE":
				minute = int(cols[1].strip())
			if cols[0] == "START SECOND":
				second = int(cols[1].strip())


		# check if mode matches
		if options.mode is not None:
			modeMatch = False
			if mode in options.mode:
				print "mode found"	
				modeMatch = True

		# check if source name in selected source list
		if options.source is not None:
			sourceMatch = False
			if sourceName in options.source:
				sourceMatch = True

		# check if timerange matches
		if options.timerange is not None:
			rangeMatch = False
			calcdate = datetime.datetime(year, month, day, hour,minute,second)

			for range in rangeList:
				if range["start"] < calcdate < range["stop"]:
					rangeMatch = True
					break
			
		if sourceMatch and modeMatch and rangeMatch:
			includeList.append(arg)

		calc.close()
	buildCommandFile(baseName, includeList)
	print "Finished"
