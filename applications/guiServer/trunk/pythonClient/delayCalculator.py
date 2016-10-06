#!/usr/bin/env python
#\if DOXYGEN_IGNORE ############################################################
#                                                                              #
#   Copyright (C) 2016 by John Spitzak                                         #
#                                                                              #
#   This program is free software; you can redistribute it and/or modify       #
#   it under the terms of the GNU General Public License as published by       #
#   the Free Software Foundation; either version 3 of the License, or          #
#   (at your option) any later version.                                        #
#                                                                              #
#   This program is distributed in the hope that it will be useful,            #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#   GNU General Public License for more details.                               #
#                                                                              #
#   You should have received a copy of the GNU General Public License          #
#   along with this program; if not, write to the                              #
#   Free Software Foundation, Inc.,                                            #
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  #
#                                                                              #
#\endif ########################################################################
################################################################################
#\defgroup delayCalculator
#
#\brief Using an iterative process, compute delay and delay rates for an experiment.
#
#  Usage:  <b><code>delayCalculator [options] VEX_FILE</code></b>
#
#  <i>DiFXMkdir</i> can be used to make a new directory on the DiFX server.
#  If given a complex path serveral levels of which don't exist it will create
#  all necessary directories.  The directory is built on the server using a
#  standard <i>mkdir</i> command which cannot be modified with any arguments.
#
#  The user running the DiFX server must have permission to perform the <i>mkdir</i>
#  operation.
#
#  <h3>Command Line Arguments</h3>
#
#  <table border="0" cellspacing="25">
#  <tr><td><pre><b>-D, --difx <i>VERSION</i></b></pre>    <td>Run using a specific DiFX version.  If not specified
#                                                          the value of the DIFX_VERSION environment variable will
#                                                          be used.  Failing that, "DIFX-DEVEL" will be used.
#  <tr><td><pre><b>-h, --help</b></pre>                   <td>Print help information and quit.
#  <tr><td><pre><b>-dh, --difxhost <i>NAME</i></b></pre>  <td>Use <i>NAME</i> as the host of the difxServer program.
#                                                          Default is to use DIFX_CONTROL_HOST environment variable.
#  <tr><td><pre><b>-dp, --difxport <i>PORT</i></b></pre>  <td>Use <i>PORT</i> as the TCP port to communicated with the difxServer.
#                                                          Default is to use DIFX_CONTROL_PORT environment variable.
#  <tr><td><pre><b>--dir <i>PATH</i></b></pre>            <td>Path to the working directory, either as a subdirectory
#                                                          of the current path or (if beginning with a "/"), a full path.
#                                                          By default delayCalculator wants the directory to be "NAME/ClockWork"
#                                                          where NAME matches the .vex file name without the ".vex" extension.
#                                                          It will do what it needs to to make this the case.
#  <tr><td><pre><b>-hh, --hopshost <i>NAME</i></b></pre>  <td>Use <i>NAME</i> as the host of the HOPS program.
#                                                          Default is to use HOPS_CONTROL_HOST environment variable.
#  <tr><td><pre><b>-hp, --hopsport <i>PORT</i></b></pre>  <td>Use <i>PORT</i> as the TCP port to communicated with the HOPS host.
#                                                          Default is to use HOPS_CONTROL_PORT environment variable.
#  </table>
#
################################################################################
program = 'delayCalculator'
version = '0.1'
author  = 'John Spitzak'
verdate = '20160923'

import sys

difxVersion = None
difxHost = None
difxPort = None
hopsHost = None
hopsPort = None
workingDir = None
vexFile = None
verbose = 0

#===============================================================================
#  MAIN
#===============================================================================

#  Locate a "default" DiFX Version from environment variables.  User may change this
#  with command line arguments.  
try:
	difxVersion = os.environ["DIFX_VERSION"]
except:
	difxVersion = "DIFX-DEVEL"
	
#  Same approach for host names and ports for the DiFX and HOPS machines.  Use the
#  environment variables if they are there, but he user can still replace them with
#  command line arguments.
try:
	difxHost = os.environ["DIFX_CONTROL_HOST"]
except:
	difxHost = None
try:
	difxPort = os.environ["DIFX_CONTROL_PORT"]
except:
	difxPort = None
try:
	hopsHost = os.environ["HOPS_CONTROL_HOST"]
except:
	hopsHost = None
try:
	hopsPort = os.environ["HOPS_CONTROL_PORT"]
except:
	hopsPort = None

try:
	i = 1
	otherArgs = []
	argStr = None
	pathStr = None
	while i < len( sys.argv ):
		#  Check against legal argument types.  Anything we don't recognize is assumed
		#  to be part of a list of message types.
		if sys.argv[i] in [ "-h", "--help" ]:
			print '\n%s ver %s  %s  %s' % (program, version, author, verdate)
			print "Runs an iterative process to determine delays and delay rates for observations"
			print "described in a user-specified .vex file."
			print "Usage: %s [options] VEX_FILE" % ( sys.argv[0] )
			print ""
			print "Options can include:"
			print ""
			print "   --difx VERSION"
			print "   -D VERSION Run using a specific DiFX version.  If not specified"
			print "              the value of the DIFX_VERSION environment variable will"
			print "              be used.  Failing that, \"DIFX-DEVEL\" will be used."
			print ""
			print "   --help"
			print "   -h         Print this help information and quit."
			print ""
			print "   --difxhost NAME"
			print "   -dh NAME   Use NAME as the host of the difxServer program."
			print "              Default is to use DIFX_CONTROL_HOST environment variable."
			print ""
			print "   --difxport PORT"
			print "   -dp PORT   Use PORT as the TCP port to communicated with the difxServer."
			print "              Default is to use DIFX_CONTROL_PORT environment variable."
			print ""
			exit( 0 )
		elif sys.argv[i] in [ "-dh", "--difxhost" ]:
			difxHost = sys.argv[i+1]
			i = i + 1
		elif sys.argv[i] in [ "-D", "--difx" ]:
			DiFXVersion = sys.argv[i+1]
			i = i + 1
		elif sys.argv[i] in [ "-dp", "--difxport" ]:
			difxPort = int( sys.argv[i+1] )
			i = i + 1
		elif sys.argv[i] in [ "-v", "--verbose" ]:
			verbose = verbose + 1
			i = i + 1
		else:
			vexFile = sys.argv[i]
		i = i + 1
		
except RuntimeError:
	usageError = True
	
if verbose > 0:
	print "difxVersion is " + str( difxVersion )
	print "difxHost is " + str( difxHost )
	print "difxPort is " + str( difxPort )
	print "hopsHost is " + str( hopsHost )
	print "hopsPort is " + str( hopsPort )
	print "workingDir is " + str( workingDir )
	print "vexFile is " + str( vexFile )

#  To begin with, we need a .vex file that hopefully has delay values of some sort
#  in it.  Open in vex parser.

#  There are two machines used in this process, the DiFX machine and the HOPS machine.
#  They may be the same.  They need to have a read/writable storage area in common
#  (where this process can do its work).  This is probably /san02.  Probably should
#  have a provision whereby the top of the path can be different on each machine.

#  Make connections to the DiFX machine and the HOPS machine.  If these fail, bail
#  out.

#  Create a directory in which to do work in the common storage area.  Dump a copy
#  of the current .vex file there.

#  Examine the scans in the .vex file.  Pick scans that have short durations
#  (indicating strong sources) that (hopefully) pair the "base" antenna with all
#  other antennas.  Each pairing should have a fair number of scans, covering the
#  full range of observing times.  Could get quite tricky if the "base" antenna
#  doesn't pair up with all of the others over the full range.  

#  Create a new instance of the .v2d parser.  Use this to create a .v2d
#  file containing the scans we want to use.

#  Does the .vex file have delays/rates?  If not, create some - maybe using some
#  external resources to get sensible starts (optional item).

#  Set all antennas to "not done"

#  Start the iterative process to find delays/delay rates.

#  On DiFX machine....clean the working directory of everything except .vex and v2d

#  Run vex2difx to create jobs

#  Run calcif2

#  Run DiFX

#  Run difx2mark4

#  On HOPS machine... run fourfit

#  Extract HOPS data (alist??)

#  For each antenna that is "not done"....

#     Compute new delay/rate adjustments using range of scans over time

#     if delay or rate above defined thresholds

#        Adjust delay/rate in .vex data

#     Otherwise, declare this antenna "done"

#     Trigger rerun

#  If rerun, go to "Start the iterative process" above

