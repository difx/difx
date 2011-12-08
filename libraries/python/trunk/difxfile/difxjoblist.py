# -*- coding: utf-8 -*-
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
from string import split, strip, upper
#from difxutil import splitObservationCode, parseKeyValue
import os

class DiFXJoblist:
	"""
	Storage class for the contents of a difx .joblist file
	"""
	def __init__(self, path, filename):
		"""
		Constructor
		"""
		
		# check that path exists
		if not os.path.isdir(path):
			raise IOError ("Directory does not exist: " + path);
		# check that joblist file exists
		if not os.path.isfile(os.path.join(path, filename)):
			raise IOError ("Joblist does not exist: " + os.path.join(path, filename))

		self.path = path
		self.filename = filename
		self.overrideVersion = False
		self.parameters = None
		self.jobs = []

		self.__parse__()
		

	def __parse__(self):

		# Parse the joblist file 
		try:
			data = open(os.path.join(self.path, self.filename)).readlines()
		except Exception, ex:
			raise IOError(ex)

		if len(data) < 2:
			print 'Malformed .joblist file %s' % filename
			#TODO throw exception instead
			exit(0)

		# parse the first line of the joblist file and 
		# separate it into key/value pairs
		self.parameters = parseKeyValue(data[0])

		if len(self.parameters) < 1:
			# TODO throw exception instead
			print 'Malformed .joblist file %s line 1' % filename
			exit(0)

		# parse through the rest of the joblist file
		for line in range(1, len(data)):

			
			job = self.Job(self.path)

			# split the line in two: job description and antenna list
			(jobDesc, antennaList) = split(data[line], '#')

			
			# parse the job description
			s = split(strip(jobDesc))

			if len(s) >= 7:
				jobName = s[0]
				jobParts = split(jobName, '_')
				if (len(jobParts) != 2):
					#TODO throw exception instead
					print 'Malformed job name "%s" in .joblist file should be of form name_number' % self.name
					exit(0)
				else:
					job.name = jobName
					job.number = int(jobParts[1])

				job.mjdStart = float(s[1])
				job.mjdStop = float(s[2])
				job.nAnt = int(s[3])
				job.nPulsarBins = int(s[4])
				job.nSourceFiles = int(s[5])
				job.computationalLoad = float(s[6])

			if len(s) == 8:
				job.outputSize = int(float(s[7]))
			else:
				job.outputSize = 0

			# parse the antenna list
			antennas = []
			s = split(strip(antennaList))
			for antenna in s:
				antennas.append(antenna)

			job.antennas = antennas
			job.nNonVLBAAntennas = job.countNonVLBAAntennas()

			self.jobs.append(job)

	def getJobs(self, jobNumbersList=None):
		'''
		Returns a list of jobs from the joblist file. If the parameter list: jobNumbersList is not given
		all jobs of the joblist file will be returned. If jobNumbersList is given, only those jobs
		matching the given jobnumber will be returned
		'''

		jobs = []
		found = False

		# if jobNumberList wasn't given then return all jobs
		if jobNumbersList == None or len(jobNumbersList) == 0:
			for job in self.jobs:
				jobs.append(job)
		else:	
			for job in self.jobs:
				
				if job.number in jobNumbersList:
					jobs.append(job)

			# check if all requested job numbers were found
			badList = []
			for jobNumber in jobNumbersList:
				for job in jobs:
					found = False
					
					if job.number == jobNumber:
						found = True
						break
				if (found == False):
					badList.append(jobNumber)

			if len(badList) > 0:
				# TODO throw an exception instead
				print 'Some requested jobs are not in the .jobList file:', badList
				return []
		return jobs
					
	
	class Job(object):
                '''
                Inner class to handle Job
                '''

		

		def __init__(self, path):
			'''
			Constructor
			'''
                        self.vlbaAntennas = ['BR', 'FD', 'HN', 'KP', 'LA', 'MK', 'NL', 'OV', 'PT', 'SC']

			# check that path exists
			if not os.path.isdir(path):
				raise IOError ("Directory does not exist: " + path)
			self.path = path
			self.name = None
			self.number = None
			self.mjdStart = None
			self.mjdStop = None
			self.nAnt = None
			self.nPulsarBins = None
			self.nSourceFiles = None
			self.nNonVLBAAntennas = None
			self.computationalLoad = None
			self.outputSize = None
			self.antennas = None
		
		def countNonVLBAAntennas(self):
			'''
			Returns the number of non-VLBA antennas contained in the antenna list
			'''
			if self.antennas == None:
				return None
			
			foreign = 0
			for antenna in self.antennas:
				if upper(antenna) not in self.vlbaAntennas:
					foreign += 1
			return foreign


		def validate(self, preCorr=False):
			'''
			Performs various sanity checks for this job
			If preCorr is set to True the input file contents are also being validated
			If preCorr is set to False (or ommited) the exitance of the .difx putput directory is checked for
			Returns true if all checks succeeded, false otherwise
			'''
			if self.nPulsarBins > 1 and self.nSourceFiles > 1:
				print "Can't do multiple phase centres and pulsar mode in one pass!"
				return False

			extensions = ['.calc', '.input']
			for extension in extensions:
				file = os.path.join(self.path, self.name + extension)
				if not os.path.isfile(file):
					print 'Error : %s not found' % file
					return False
			if preCorr:
				if not self.inputSanityCheck(os.path.join(self.path, self.name + '.input')):
					return False
			else:
				ddir = os.path.join(self.path, self.name + '.difx')
				if not os.path.isdir(ddir):
					print 'difx output dir %s not found' % ddir
					 
					return False
			return True

		# TODO this should eventually go into a Class that handles input files
		def inputSanityCheck(self, inputFile):

			with open(inputFile, 'r') as f:
				data = f.readlines()

			OK = True

			# check for proper mark5 modules
			l = 1;
			for d in data:
				s = split(strip(d))
				if len(s) == 3:
					if s[0] == 'FILE' and upper(s[2]) == 'NONE':
						print 'Error: %s line %d: Cannot have VSN or filename of "%s"' \
							% (inputFile, l, s[2])
						OK = False
				l += 1

                        return OK
  
def parseKeyValue(str):
	"""
	Parses a string to find key=value pairs
	"""
	kv = {}
	ss = split(str)
	for s in ss:
		p = split(s, '=')
		if len(p) != 2:
			# TODO throw exception instead
			print 'Error parsing key=value statement: %s', s
			return {}
		kv[p[0]] = p[1]
	return kv