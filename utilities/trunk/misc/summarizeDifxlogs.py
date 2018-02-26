#!/usr/bin/python
import os, fnmatch
from operator import add

class bcolors:
	DEFAULT = '\033[95m'
	GREEN = '\033[92m'
	ORANGE = '\033[93m'
	RED = '\033[91m'
	ENDC = '\033[0m'

def getWeights(logfile):
	weights = []
	N = 1
	f = open(logfile, 'r')
	while True:
		l = f.readline()
		if len(l) <= 0:
			break
		if not('WEIGHTS' in l):
			continue
		idx = l.find('WEIGHTS') + len('WEIGHTS') + 1
		curr_weights = [float(v) for v in l[idx:].split()]
		if len(weights) != len(curr_weights):
			weights = curr_weights
		else:
			weights = map(add, weights, curr_weights)
			N += 1
	weights = [w/float(N) for w in weights] # average value
	weights = [int(w*100)/100.0 for w in weights] # truncate decimals
	return weights

def getWeightlabels(inputfile):
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

def getWallclockStr(logfile):
	mpiDone = False
	wallclockFound = False
	s = ''
	f = open(logfile, 'r')
	while True:
		l = f.readline()
		if len(l) <= 0:
			break
		# STATUS MpiDone
		if ('STATUS MpiDone' in l) or ('STATUS Done' in l):
			mpiDone = True
			continue
		# 'Fri Feb 23 22:45:21 2018   0 fxmanager INFO  Total wallclock time was **1011.36** seconds'
		if 'Total wallclock' in l:
			elems = l.split('**')
			T = elems[1]
			s = '%s%s sec%s' % (bcolors.GREEN,T,bcolors.ENDC)
			wallclockFound = True
	if not wallclockFound:
		s = bcolors.RED + 'no runtime' + bcolors.ENDC
	if not mpiDone:
		s = s + ', ' + bcolors.RED + 'no MpiDone' + bcolors.ENDC
	else:
		s = s + ', ' + bcolors.GREEN + 'MpiDone' + bcolors.ENDC

	return s

def weights2text(weights, labels, alltelescopes):
	s = ''
	#for a in alltelescopes:
	#	indexes = [i for i,x in enumerate(labels) if x==a]
	for i in range(len(weights)):
		ss = '%s:%3.2f ' % (labels[i],weights[i])
		if weights[i] >= 0.90:
			s += bcolors.GREEN + ss
		elif weights[i] >= 0.40:
			s += bcolors.ORANGE + ss
		else:
			s += bcolors.RED + ss
	s += bcolors.ENDC
	return s

telescopes = set()
files = fnmatch.filter(os.listdir('.'), '*.difxlog')
files.sort()

print ('Wallclock times:')
for logname in files:
	jobname = logname[:logname.rfind('.')]
	wallclockstr = getWallclockStr(logname)
	print ('%s : %s' % (jobname,wallclockstr))
print ('')

print ('Weights:')
for logname in files:
	jobname = logname[:logname.rfind('.')]
	weightvalues = getWeights(logname)
	weightantennas = getWeightlabels(jobname + '.input')
	telescopes = telescopes | set(weightantennas)
	weightsstr = weights2text(weightvalues, weightantennas, telescopes)
	print ('%s : %s' % (jobname,weightsstr))
