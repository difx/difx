#!/usr/bin/python

program = 'getObsFiles'
author  = 'Helge Rottmann'
version = '0.2'
verdate = '20120120'

import os
import sys

baseURLs = ["http://www.ira.inaf.it/vlb_arc/vlb_archive/", "http://www.vlba.nrao.edu/astro/VOBS/astronomy/"]
#baseURLs = ["http://www.vlba.nrao.edu/astro/VOBS/astronomy/"]
localDir = "FILES"

def printUsage():

	print '\n%s ver. %s  %s %s' % (program, version, author, verdate)
        print '\nA program to retrieve the observation files (logs, vex etc.)'
        print 'from the Bologna and NRAO servers.'

	print "\nUsage: \n"
	print sys.argv[0] + " <experiment> <session> \n\n"

	print "experiment:	experiment code to retrieve logs files for"
	print "session:		session of the observation (e.g. mar10, nov09)\n"

	print "Files are downloaded to the %s subdirectory\n" % (localDir)

	exit(1)

if len(sys.argv) < 3:
	printUsage()

exp = str.lower(sys.argv[1])
date = sys.argv[2]

try:
	os.mkdir("./%s" % (localDir))
except:
	print "Directory %s exists. Delete and try again\n" % localDir
	exit(1)

os.chdir("./%s" % (localDir))

for baseURL in baseURLs:

	if "ira.inaf" in baseURL:
		url = baseURL + date + "/"
		wget = 'wget -r -nd --no-parent -nv -A' + exp + '* -e robots=off ' + url
		os.system(wget)

		urlLatest = url + ".latest/"		
		wgetLatest = 'wget -r -nd --no-parent -nv -A' + exp + '* -e robots=off ' + urlLatest
		os.system(wgetLatest)

	if "vlba" in baseURL:
		url = baseURL + date + "/" + exp + "/"
		wget = 'wget -r -nd --no-parent --level=1 -nv '
		wget += '-A' + exp + '.*,'+ exp + 'cal.vlba,'+ exp + 'log.vlba ' # <exp>cal.vlba has VLBA Tsys and everything else
		wget += '-Rindex* -e robots=off ' + url
		print wget
		os.system(wget)
