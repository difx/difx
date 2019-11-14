#!/usr/bin/env python2
# -*- coding: utf-8 -*-

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
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

from optparse import OptionParser
from difxdb.business.versionhistoryaction import *
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.queueaction import *
from difxdb.business.passaction import *
from difxdb.business.jobaction import *
from difxutil.dateutil import *
from difxfile.difxjoblist import *
from os import getenv, system, umask, getcwd, remove, rmdir
from os.path import isfile, isdir
from string import lower, strip
from glob import glob
import sys 

# "main" starts here

program = 'difxqueue'
version = "0.5"
author  = 'Helge Rottmann'
verdate = '2012-05-02'

queueBaseDir = ""
passName = ""
options = None

clusterHorsepower = 0.24

# minimum database schema version required by difxqueue
minSchemaMajor = 1
minSchemaMinor = 0

def readDBOptionsFile():
	"""
	Read database connection parameters from a MySQL options file. If not specified otherwise by the -c command line argument,
	the options file will be searched under $DIFXROOT/conf/difxdb.cnf.
	Note: The options file mechanism will work only for MySQL database server.
	"""

	optionsFile = ""
	
	# if command line option -c was given, read the connection parameters from the 
	# file specified else use the default $DIFXROOT/conf/difxdb.cnf
	if (len(options.dbConf) > 0):

		optionsFile = options.dbConf
		

		optionsFile = options.dbConf
	else:
		# otherwise check if the default file exists
		difxRoot = getenv('DIFXROOT')
		if difxRoot == None :
			exitOnError("DIFXROOT environent not defined")

		optionsFile = difxRoot + "/conf/difxdb.cnf"
	
	# check that the file exists
	if not isfile(optionsFile):
		exitOnError("Database options file not found: %s" % optionsFile)

	return(optionsFile)
		

def setQueueDirectoryOwnership(queueDir):
	"""
	Recursively set group ownership of queueDir according to contents of the
	DIFX_GROUP_ID environment variable. If the variable has not been set
	do nothing.
	"""

	# check for DIFX_GROUP_ID
	groupId = getenv('DIFX_GROUP_ID')

	# change queue directory permission
	if groupId != None:
		cmd = 'chown -R :%s %s' % (groupId, queueDir)
		if verbose > 0:
			print 'Executing: %s' % cmd
		system(cmd)
	
def setQueueDirectory(experiment):
	"""
	Determines the queue staging directory by reading the DIFX_QUEUE_BASE environment and
	adding the experiment code 
	Alternatively, if the user directory was given via the command line option this directory is used instead.
	Finally the queue directory is created (if it doesn't exist) 

	Returns the full path name of the queue directory
	"""	

	# check if command line option has been set
	if options.queueDir == None:
		
		difxQueueBase = getenv('DIFX_QUEUE_BASE')
		if difxQueueBase == None and not options.dbOnly:
			exitOnError("No queue directory defined. Either use -q option or set DIFX_QUEUE_BASE")

		queueDir = difxQueueBase + '/' + str.lower(experiment)
	else:
		queueDir = options.queueDir

	if (options.verbose > 0):
		print "VERBOSE: Using queue directory: %s" % queueDir
	
	# make queue directory (if it doesn't exist yet)
	if not isdir(queueDir):
		cmd = 'mkdir -p %s' % queueDir
		if options.verbose > 0:
			print 'VERBOSE: Executing: %s' % cmd
		system(cmd)

		# check if directory was created successfully
		if not isdir(queueDir):
			exitOnError("Could not create directory: %s" % (queueDir))

		

	return(queueDir)

def processSetPassTypeAction(args):
	"""
	Change the type of one or more passes 
	This function is called when the user selects the "setpasstype" command
	Input:
		args: 		the command line arguments given by the user
	"""

	newType = ""
	passes = list()
	project = ""

	if len(args) < 3:
		parser.print_help()
		exit(0)

	newType = lower(args[1])
	project = args[2]

	if len (args) > 3:
		passes.extend(args[3:])

	# check that the requested type exists
        if not passTypeExists(session, newType):
		exitOnError("Unknown pass type '%s' requested" % newType)
                
        passType = getPassType(session, newType)

	if not options.yes:
		confirmAction("change pass type")

        passList = getPasses(session, project, passes)
        
	# loop over all passes
	for Pass in passList:
		Pass.type = passType
                session.commit()
		print "Changing pass type to %s on  %s / %s (Exp/Pass)" % (newType, project, Pass.passName)

	return

def processSetStateAction(args):
	"""
	Change the state of a job 
	This function is called when the user selects the "setstate" command
	Input:
		args: 		the command line arguments given by the user
	"""

	jobNumbers = []
	project = ""
	#Pass = []

	

	if len(args) < 4:
		parser.print_help()
		exit(0)

	newStatus = lower(args[1])
	project = args[2]
        passName = args[3]
	
	
	if len (args) > 4:
		jobNumbers = getnumbers(args[4:])

        queueList = getJobs(session, project, passName, jobNumbers)
        
	if len(queueList) == 0:
		print "No queued jobs matching your search criteria found"
		return

	# check that the requested status exists
	if not jobStatusExists(session, newStatus):
		exitOnError("Unknown job status '%s' requested" % newStatus)

        jobStatus = getJobStatus(session, newStatus)
        
	if not options.yes:
		confirmAction("change job state")

	for queue in queueList:
		print "Processing  %s / %s / %s (Exp/Pass/Number)" % (project, passName , queue.jobNumber)
                
		queue.status = jobStatus
                session.commit()
		
	
	return
	

def processDeleteAction(args):
	"""
	Delete jobs from the database and from the correlation staging area
	This function is called when the user selects the "delete" command
	Input:
		args: 		the command line arguments given by the user
	"""
	project = ""
	passes = []
	jobNumbers = []

	if len(args) < 2:
		parser.print_help()
		exit(0)

	project = args[1]

	if len(args) > 2:
		passes.append(args[2])

	if len(args) > 3:
		jobNumbers = getnumbers(args[3:])

        queueList = getActiveJobsByPasses(session, project, passes)

	if len(queueList) == 0:
		print "No queued jobs matching your search criteria found"
		return

	# get user confirmation before doing the delete from the DB
	confirmAction ("delete")

	
	dontAsk = False

	for queue in queueList:

		queueDir = setQueueDirectory(queue.Pass.experiment.code)

		if len(jobNumbers) > 0 and queue.jobNumber not in jobNumbers:
			continue

		sys.stdout.write("Processing  %s / %s / %s (Exp/Pass/Number)" % (queue.Pass.experiment.code, queue.Pass.passName, queue.jobNumber))

		# skip file deletion if option dbonly was selected by the user
		if (not options.dbOnly):

			# check if output data exists for this job
			if isdir(queue.inputFile[0:-6]+ '.difx' ):
				print '\nWarning: correlator output for job %d of pass %s exists.' % (queue.jobNumber, queue.Pass.passName)

				
				if (not dontAsk):
					print 'Continue anyway, destroying this data? [y/N/a/c] (y=yes, n=no (default), a = all, c = cancel)'
					reply = strip(lower(sys.stdin.readline()))

					if reply == 'a':
						dontAsk = True
						reply = 'y'
						print 'Assuming yes for all remaining'
					elif reply == 'c':
						print 'Canceling'
						return;
			
				else:
					reply = 'y'

				if reply =='y':
					if (dontAsk):
						print 'Continuing anyway'
					else:
						print 'OK -- proceeding'
				else:
					continue
				
			# delete all files
			for filename in glob('%s.*' % queue.inputFile[0:-6]):
				try:
					remove(filename)	
				except:
					print("\n")
					exitOnError("Could not delete file:%s" % (filename))

				
			sys.stdout.write("...deleted files")
			
			# delete directory if it contains no more files
			if len(glob('%s/*' % queueDir)) == 0:

				try:
					rmdir(queueDir)
				except:
					print("\n")
					exitOnError("Could not delete directory:%s" % (queueDir))
				

		# delete job from database
                deleteQueueItem(session, queue)
		
		sys.stdout.write("...deleted from database\n")

	
def processPriorityAction(args, priorityInc):
	"""
	Increase or decrease the job priority.
	This function is called when the user selects the "bump" or "slide" commands
	Input:
		args: 		the command line arguments given by the user
		priorityInc:	the desired priority increment (decrement)
	"""

	project = ""
	passes = []
	jobNumbers = []

	if len(args) < 2:
		parser.print_help()
		exit(0)

	project = args[1]

	if len(args) > 2:
		passes.append(args[2])

	if len(args) > 3:
		jobNumbers = getnumbers(args[3:])


	queueList = getActiveJobsByPasses(session, project, passes)

	if len(queueList) == 0:
		print "No queued jobs matching your search criteria found"
		return

	# get user confirmation before changing the priority
	if (priorityInc > 0):
		confirmAction ("bump")
	else:
		confirmAction ("slide")

	for queue in queueList:

		if len(jobNumbers) > 0 and queue.jobNumber not in jobNumbers:
			continue
		
		queue.priority += priorityInc
                session.commit()

                print "Set priority to %s on %s/%s/%s (Exp/Pass/Number)" % (queue.priority, queue.Pass.experiment.code, queue.Pass.passName, queue.jobNumber)

	

def processPrintQueueAction(args):
	

	projects = []
	for a in args[1:]:
		projects.append(a)

	totalDur = 0;
	totalCor = 0;

	formatTable = '%-10s%-10s %5d %3d %-10s%2d/%2d %-19s%4d %-8.1f  %-4.4f'
	formatHeader = '%-10s%-10s %5s %3s %-10s%5s %-19s%4s %-10s%-10s'

	queueList = getActiveJobs(session , projects)

	if len(queueList) == 0:
		print "No queued jobs matching your search criteria found"
		return

	header = formatHeader % ('Exper', 'Pass', 'Num', 'Pri', 'Status', 'nAnt', 'Start', 'Dur', 'SpeedUp', "Total(hrs)")
	print header
	
	for queue in queueList:

		totalDur += queue.jobDuration

		if queue.speedupFactor is not None:
			totalCor += queue.jobDuration / queue.speedupFactor
		else:
			queue.speedupFactor = 0.0

		line = formatTable % \
			(queue.Pass.experiment.code, queue.Pass.passName, queue.jobNumber, \
			queue.priority, queue.status.status, queue.numAntennas, queue.numForeign, mjd2vexTime(queue.jobStart),\
			queue.jobDuration, queue.speedupFactor, totalCor/3600)

		print line

	print 'Total observe time        = %4.2f hours' % (totalDur / 3600.0)
	print 'Total correlation time    = %4.2f hours' % (totalCor / 3600.0)

def readJobList(passName):

	print joblist.parameters
	print joblist.jobs[0]

def getnumbers(stringlist):
	"""
	Separates a range string (e.g. 1 3-5 10 12-18) and populates a
	list with the individual numbers
	"""
	numbers = []
	for s in stringlist:
		sepIndex = s.find("-")

		if sepIndex > 0:
			try:
				start = int(s[:sepIndex])
				stop = int(s[sepIndex+1:])
			except Exception, e:
				exitOnError(e)

			if stop < start:
				exitOnError("Illegal range of jobs given: start value exceeds stop value")
		else:
			try:
				start = stop = int(s)
			except Exception, e:
				exitOnError(e)
				

		for i in range (start, stop+1):
			if not i in numbers:
				numbers.append(i)
	numbers.sort()

	return numbers

def processAddAction(args):
	'''
	Adds one or more job to the database and copies job files to the correlation staging area.
	'''

	global queueDir

	queueDir = ""
	jobNumbers =[]

	if len(args) < 2:
		parser.print_help()
		exit(0)

	passName = args[1]
	if passName[-8:] == '.joblist':
		jobListFile = passName
	else:
		jobListFile = passName + '.joblist' 

	if len(args) > 2:
		jobNumbers = getnumbers(args[2:])

	try:
		joblist = DiFXJoblist(".", jobListFile)
	except IOError, ex:
		exitOnError (ex)
		
	OK = True
	for job in joblist.getJobs(jobNumbers):
		OK &= job.validate(preCorr=True)

	if not OK:
		exitOnError('one or more jobs failed validation')
	
	queueDir = setQueueDirectory(lower(joblist.parameters["exper"]))

		
	# check if any jobs (=identical input files) have already been queued and are active
	badList = []
        activeList = getActiveJobs(session)

	for job in joblist.getJobs(jobNumbers):
		for activeJob in activeList:
			if activeJob.inputFile == queueDir + "/" + job.name + ".input":
				badList.append(queueDir + "/" + job.name)
				
	if len(badList) > 0 and not options.force :
		print '\nThe following jobs are already in the queue:'
		for badJob in badList:
			print '  %s' % badJob
		print 'Please rerun, including only those jobs not already queued.'
		return

	# adding jobs to the database			
	for job in joblist.getJobs(jobNumbers):       
                
		sys.stdout.write("Processing %s ..." % job.name)
                
                queueItem = model.Job()
                
                queueItem.jobNumber = job.number
		queueItem.jobStart = job.mjdStart
		queueItem.jobDuration = (job.mjdStop - job.mjdStart)  * 86400.0
		queueItem.inputFile = queueDir + "/" + job.name + ".input"
		queueItem.difxVersion = joblist.parameters["DiFX"]
		queueItem.numAntennas = job.nAnt
		queueItem.numForeign = job.nNonVLBAAntennas
		queueItem.priority = options.priority
		queueItem.speedupFactor = ((job.mjdStop - job.mjdStart)  * 86400.0) / job.computationalLoad * clusterHorsepower;
                
		
		# add job to the database
		try:
                        addQueueItem(session, queueItem, joblist.parameters["pass"], joblist.parameters["exper"])
		except Exception, e:
			exitOnError(e)
		else:
			sys.stdout.write("... added to database" )

		
		# copy file
		if not options.dbOnly:

			# build options for running difxcopy and calcif2
			optStr = ""
			for i in range(0, options.verbose):
				optStr += "-v "
			# if this script was started in the queue directory skip copying files
			if(queueDir != getcwd()):
				cmd = 'difxcopy %s %s %s' % (optStr, job.name, queueDir)
				if options.verbose > 0:
					print 'VERBOSE: Executing: %s' % cmd
				system(cmd)
				sys.stdout.write("...copied to the queue directory")
			else:
				sys.stdout.write("...not copied -- using local directory")

			# run calcif2
			if options.overrideVersion:
				optStr += " --override-version "
			cmd = 'calcif2 %s -f %s/%s' % (optStr, queueDir, job.name)
			if options.verbose > 0:
				print 'VERBOSE: Executing: %s' % cmd
			system(cmd)
			sys.stdout.write("...ran calcif2")


		print ("...done")

	# change group ownership
	setQueueDirectoryOwnership(queueDir)
	
	
def processProdAction(args):
		
	return

def confirmAction(action):
	"""
	Get user confirmation before executing commands
	"""
	
	# if --yes option was used skip confirmation
	if (options.yes):
		return

	print '\nThe %s action is about to be applied to all matching jobs' %\
		action
	print '\nAre you sure you want to proceed? [y/N]'

	a = lower(sys.stdin.readline())
	if strip(a) == 'y':
		print 'OK -- proceeding\n'
	else:
		print 'Not continuing.\n'
		exit(0)

def exitOnError(exception):
	'''
	Exit routine to be called whenever an error/exception has occured
	'''
	print "ERROR: %s. Aborting" % exception
	
	exit(1)

def getUsage():

	usage=  '\n%s ver %s  %s  %s' % (program, version, author, verdate)
	usage += '\nA program to send DiFX jobs to the correlator queue and update the database.'
	usage += '\nFunctionality to examine and modify the contents of the queue are also provided.\n'
	
	usage += "\nUsage: %prog [options] <command> [<args>]\n"

	usage += "\nCommands: \n"

	usage += "add	Add job(s) to the queue\n"
	usage += "\targuments = <pass name> [list of job numbers]\n"
	usage += "\texample 1:   difxqueue add clock\n"
	usage += "\texample 2:   difxqueue add geodesy 3 4-9 12\n"

	usage += "delete Delete job(s) from the queue\n"
	usage += "\targuments = <experiment> <pass name> [list of job numbers]\n"
	usage += "\texample 1:   difxqueue delete r1455 clock\n"
	usage += "\texample 2:   difxqueue delete r1455 geodesy 3 4-9 12\n"

	usage += "printQueue Lists the jobs currently queued for processing.\n"
	usage += "\targuments = [list of experiments]\n"
	usage += "\texample 1:   difxqueue printQueue \n"
	usage += "\texample 2:   difxqueue printQueue r1455 r1456\n"

	usage += "bump	Increase the priority of job(s) by 1\n"
	usage += "\targuments = <experiment> [<pass name>] [list of job numbers]\n"
	usage += "\texample 1:   difxqueue bump r1455\n"
	usage += "\texample 2:   difxqueue bump r1455 clock \n"
	usage += "\texample 3:   difxqueue bump r1455 clock  3 4-9 12\n"

	usage += "slide	Decrease the priority of job(s) by 1\n"
	usage += "\targuments = <experiment> [<pass name>] [list of job numbers]\n"
	usage += "\texample 1:   difxqueue slide r1455\n"
	usage += "\texample 2:   difxqueue slide r1455 clock \n"
	usage += "\texample 3:   difxqueue slide r1455 clock  3 4-9 12\n"

	usage += "setState Change the state of job(s)\n"
	usage += "\targuments = <state> <experiment> [<pass name>] [list of job numbers]\n"
	usage += "\texample 1:   difxqueue setState complete r1455\n"
	usage += "\texample 2:   difxqueue setState queued r1455 clock \n"
	usage += "\texample 3:   difxqueue setState killed r1455 test  3 4-9 12\n"

	usage += "setPassType Change type of pass(es)\n"
	usage += "\targuments = <type> <experiment> [list of passes] \n"
	usage += "\texample 1:   difxqueue setPassType production r1455\n"
	usage += "\texample 2:   difxqueue setPassType production r1455 clock test \n"
	return usage

	
umask(02)

usage = getUsage()

parser = OptionParser(version="%prog " + version, usage=usage)

parser.add_option("-v", "--verbose", dest="verbose", action="count", default = 0, help="Send more output to the screen  (use -v -v for extra info)")
parser.add_option("-o", "--override-version", dest="overrideVersion", action="store_true", help="Force operation with mixed DiFX versions")
parser.add_option("-p", "--priority", dest="priority", type="int" , default = 0, help="Set the priority of the job to <PRIORITY> when adding to the queue")
parser.add_option("-d", "--db-only", dest="dbOnly", action="store_true", help="Operate only on database (some commands only)")
parser.add_option("-q", "--queuedir", dest="queueDir", type="string" ,action="store", help="Specify the directory to use as the correlation base (overrides DIFX_QUEUE_BASE environment)")
parser.add_option("-t", "--type", dest="passType", type="string" ,action="store", default="production", help="Specify the type of the jobs when adding to the queue")
parser.add_option("-y", "--yes", dest="yes" ,action="store_true", default=False, help="Answer yes to all confirmation requests")
parser.add_option("-f", "--force", dest="force" ,action="store_true", default=False, help="Force adding jobs to the queue even they qre queued already")
parser.add_option("-c", "--conf", dest="dbConf", type="string", default= "", help="Specify the option file that contains the database connection parameters (default: $DIFXROOT/conf/difxdb.cnf)")


# parse the command line. Options will be stored in the options list. Leftover arguments will be stored in the args list
(options, args) = parser.parse_args()

if (options.verbose > 0):
	print "Using options:"
	for key, value in options.__dict__.items():
		print "%s  : %s" % ( key, value)
	
if len(args) == 0:
	parser.print_help()
	exit(0)

if (getenv("DIFXROOT") == None):
            sys.exit("Error: DIFXROOT environment must be defined.")

config = DifxDbConfig(getenv("DIFXROOT") + "/conf/difxdb.ini")

# try to open the database connection
connection = Connection(config)
connection.echo = False

try:
    dbConn = Schema(connection)
    session = dbConn.session()
except Exception as e:
    exitOnError(e)

if not isSchemaVersion(session, minSchemaMajor, minSchemaMinor):
	major, minor = getCurrentSchemaVersionNumber(session)
        exitOnError("Current difxdb database schema is %s.%s but %s.%s is minimum requirement" % (major, minor, minSchemaMajor, minSchemaMinor))

action = lower(args[0])

if action == 'printqueue':
	processPrintQueueAction(args)
elif action == 'prod':
	processProdAction(args)
elif action == 'add':
	processAddAction(args)
elif action == 'bump':
	processPriorityAction(args, +1)
elif action == 'slide':
	processPriorityAction(args, -1)
elif action == 'delete':
	processDeleteAction(args)
elif action == 'setstate':
	processSetStateAction(args)
elif action == 'setpasstype':
	processSetPassTypeAction(args)
else:
	parser.print_help()
	exit(0)


