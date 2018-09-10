#!/usr/bin/python
#
# (C) 2018 Jan Wagner
#
'''
Usage: summarizeDifxlogs.py [--help|-h] [--color|-c]

Produces a summary of a DiFX correlation run in the current working directory.
Inspects the *.difxlog and associated *.input files and reports average
station (datastream) weights, run times, and (non)clean DiFX completion.
'''

import os, fnmatch, sys
from operator import add

class bcolors:
	DEFAULT = '\033[95m'
	GREEN = '\033[92m'
	ORANGE = '\033[93m'
	RED = '\033[91m'
	ENDC = '\033[0m'

def getLatestCorrelationOffset(logfile):
	'''Look through the log file and return byte offset of the last line containing a DiFX "STARTING" message. Useful if logfile was appended to.'''
	found = False
	offset = os.stat(logfile).st_size
	for line in reversed(open(logfile,'r').readlines()):
		if not found:
			offset -= len(line)
		if 'STARTING' in line:
			found = True
			break
	if not found:
		offset = 0
	return offset

def getWeights(logfile):
	'''Return per-datastream weights found in a DiFX log file. Also determine the display format (nr of decimals) of the weights in the log file.'''
	weights = []
	N = 1
	Ndecimals = 2
	f = open(logfile, 'r')
	f.seek(getLatestCorrelationOffset(logfile))
	while True:
		l = f.readline()
		if len(l) <= 0:
			break
		if not('WEIGHTS' in l):
			continue
		idx = l.find('WEIGHTS') + len('WEIGHTS') + 1
		curr_weights_strlist = l[idx:].split()
		if len(curr_weights_strlist) < 1:
			continue
		Ndecimals = len(curr_weights_strlist[0]) - (curr_weights_strlist[0].find('.') + 1)
		curr_weights = [float(v) for v in curr_weights_strlist]
		if len(weights) != len(curr_weights):
			weights = curr_weights
		else:
			weights = map(add, weights, curr_weights)
			N += 1
	weightfmt = '%%.%df' % (Ndecimals)
	weighttrunc = 10.0**Ndecimals
	weights = [w/float(N) for w in weights] # average value
	weights = [int(w*weighttrunc)/weighttrunc for w in weights] # truncate decimals
	return (weights,weightfmt)

def getWeightlabels(inputfile):
	'''Get list of antennas associated with each datastream from DiFX .input file'''
	telescopes = {}
	labels = []
	f = open(inputfile, 'r')
	while True:
		l = f.readline()
		if len(l) <= 0:
			break
		elif 'TELESCOPE NAME ' in l:
			antindex = int(l[15:20].replace(':',' '))
			name = l[17:].strip()
			if not name in telescopes:
				telescopes[antindex] = name
		elif 'TELESCOPE INDEX:' in l:
			antindex = int(l[l.find(':')+1:])
			labels.append(telescopes[antindex])
	return labels

def getTimingsStr(logfile):
	'''Get runtime and scan length from a DiFX log file, as well as MPI completion status'''
	mpiDone = False
	wallclockTime = -1
	peakDatatime = -1

	f = open(logfile, 'r')
	f.seek(getLatestCorrelationOffset(logfile))
	while True:
		l = f.readline()
		if len(l) <= 0:
			break
		# STATUS MpiDone
		if ('STATUS MpiDone' in l) or ('STATUS Done' in l):
			mpiDone = True
			continue
		# Vis. 39 to write out time 239.6
		if 'to write out time' in l:
			T = l[(l.find('out time ') + len('out time ')):]
			if float(T) > peakDatatime:
				peakDatatime = float(T)
			continue
		# 'Fri Feb 23 22:45:21 2018   0 fxmanager INFO  Total wallclock time was **1011.36** seconds'
		if 'Total wallclock' in l:
			elems = l.split('**')
			T = elems[1]
			wallclockTime = float(T)

	s = ''
	if wallclockTime == -1:
		s = s + '%s%s,%s ' % (bcolors.RED,'no runtime',bcolors.ENDC)
	else:
		s = s + '%s%s sec,%s ' % (bcolors.GREEN,str(wallclockTime),bcolors.ENDC)
		if peakDatatime > 0:
			factor = wallclockTime/peakDatatime
			if factor <= 1:
				ccode = bcolors.GREEN
			elif factor <= 10L:
				ccode = bcolors.ORANGE
			else:
				ccode = bcolors.RED
			s = s + '%s%.1fx slowdown, %s' % (ccode,factor,bcolors.ENDC)
	if not mpiDone:
		s = s + bcolors.RED + 'no MpiDone' + bcolors.ENDC
	else:
		s = s + bcolors.GREEN + 'MpiDone' + bcolors.ENDC

	return s

def weights2text(weights, labels, alltelescopes, weightfmt='%3.2f'):
	s = ''
	#for a in alltelescopes:
	#	indexes = [i for i,x in enumerate(labels) if x==a]
	for i in range(len(weights)):
		ss = ('%s:' + weightfmt + ' ') % (labels[i],weights[i])
		if weights[i] >= 0.90:
			s += bcolors.GREEN + ss
		elif weights[i] >= 0.40:
			s += bcolors.ORANGE + ss
		else:
			s += bcolors.RED + ss
	s += bcolors.ENDC
	return s

# Defaults
doColor = False
doTimefactors = True
doWeights = True

# Args
if ('--color' in sys.argv) or ('-c' in sys.argv):
	doColor = True

if ('--help' in sys.argv) or ('-h' in sys.argv):
	print (__doc__)
	sys.exit(0)

if not doColor:
	# awful hack...
	bcolors.DEFAULT = ''
	bcolors.GREEN = ''
	bcolors.ORANGE = ''
	bcolors.RED = ''
	bcolors.ENDC = ''

# List all log files in CWD
telescopes = set()
files = fnmatch.filter(os.listdir('.'), '*_*.difxlog')
files.sort()

# Summaries
if doTimefactors:
	print ('## Wallclock times:')
	for logname in files:
		jobname = logname[:logname.rfind('.')]
		timingsstr = getTimingsStr(logname)
		print ('# %s : %s' % (jobname,timingsstr))
	print ('#')
if doWeights:
	print ('## Weights:')
	for logname in files:
		jobname = logname[:logname.rfind('.')]
		(weightvalues,weightfmt) = getWeights(logname)
		weightantennas = getWeightlabels(jobname + '.input')
		telescopes = telescopes | set(weightantennas)
		weightsstr = weights2text(weightvalues, weightantennas, telescopes, weightfmt)
		print ('# %s : %s' % (jobname,weightsstr))
