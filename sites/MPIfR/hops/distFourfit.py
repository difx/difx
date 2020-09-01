#!/usr/bin/python
'''
Usage: distFourfit.py [-c <controlfile>] [-n <max nr of nodes>]
                     [-f <fourfit wrapper script>] <expt_root>

Runs fourfit using control file (default: cf_1234) on all scans
located under ./<expt_root>/. Fourfit is invoked over a warpper
script that sets up HOPS environment vars and launches fourfit.

Fourfit is executed in parallel as multiple processes with one
fourfit instance per baseline per scan, distributed over CPUs
and cluster nodes.

NB: currently nodes node02..node32 are used. In a later version,
    support might be added for a $DIFX_MACHINES machine file.

(C) 2019 Jan Wagner
'''

import argparse
import glob, sys, os
import subprocess
# Note: mpi4py is not used as it needs CentOS openmpi but the MPIfR
#       cluster runs a custom built Gemmatix/GxHive OpenMPI...


################################################################################################################

CONTROLFILE = ''
MAX_MACHINES = 0

parser = argparse.ArgumentParser(add_help=False,
        description='Run fourfit on multiple nodes in parallel')
parser.add_argument('-h', '--help', help='Help', action='store_true')
parser.add_argument('-a', '--ant', help='Fit only baselines to these antenna(s). Comma separated 1-letter codes.',
	dest='antennas', default='')
parser.add_argument('-f', '--fourfit', help='Fourfit setup and invocation script to use',
	dest='fourfitlauncher', default='/cluster/hops/fourfit_d260')
parser.add_argument('-c', '--controlfile', default='cf_1234', dest='controlfile',
        help='Fourfit control file to use for fringe fitting (default: cf_1234)')
parser.add_argument('-n','--maxnodes', dest='maxnodes', default=32, help='Run at most n nodes')
parser.add_argument('rootdir', nargs='*')

################################################################################################################

FOURFIT = '/cluster/hops/fourfit_d260'
FOURFIT_OPTS = ['-m1', '-u']
ANTENNAS_SUBSET = []

## Cluster-dependent vars
# ToDo: read file pointed to by $DIFX_MACHINES
NCORE = 4
MACHINES = ['fxmanager']
MACHINES += ['node%02d' % (n) for n in range(2,32)]   # node33 is down
MACHINES = MACHINES * NCORE

################################################################################################################

def getScandirs(rootdir):
	'''Return a list of subdirectories under experiment root dir'''
	scandirs = [dname for dname in glob.glob(rootdir + '/*') if os.path.isdir(dname)]
	return scandirs


def getBaselines(subdir, doAuto=False):
	'''Return a list of baselines in subdirectory. Example: ['AX', 'AL', 'AS', 'SX', 'LX', 'SL']'''
	blines = set()
	for fpath in glob.glob(subdir + '/' + '??..*'):
		fn = os.path.split(os.path.basename(fpath))[-1]
		if (fn[0] == fn[1]) and not doAuto:
			pass
		elif ANTENNAS_SUBSET and not any([antId in fn[0:2] for antId in ANTENNAS_SUBSET]):
			pass
		else:
			blines.add(fn[:2])
	return blines


def getCompletedBaselines(subdir, doAuto=False):
	'''Return a list of fringe fitted baselines in subdirectory. Example: ['AX', 'AL', 'AS', 'SX', 'LX', 'SL']'''
	blines = set()
	for fpath in glob.glob(subdir + '/' + '??.?.*'):
		fn = os.path.split(os.path.basename(fpath))[-1]
		if (fn[0] == fn[1]) and not doAuto:
			pass
		elif ANTENNAS_SUBSET and not any([antId in fn[0:2] for antId in ANTENNAS_SUBSET]):
			pass
		else:
			blines.add(fn[:2])
	return blines


def makeJoblist(exptroot):
	'''Make a job list: list of (subdir,baseline) tuples'''
	jobs = []
	scans = getScandirs(exptroot)
	for scan in scans:
		blines = getBaselines(scan)
		completed = getCompletedBaselines(scan)
		blines = list(blines - completed)
		jobs += zip([scan]*len(blines), blines)
	return jobs


def chunks(L, N):
	'''Yield successive N-sized chunks from list L.'''
	last = len(L)
	for i in range(0, len(L), N):
		end = i+N
		if end > last:
			end = last
		yield L[i:end]


def parallel_Popen(cmdlist, dryrun=False):
	'''Launch commands in parallel and wait for their completion'''
	processes = {}
	processdetails = {}
	devnull = open(os.devnull, 'w')
	if dryrun:
		for cmd in cmdlist:
			print (cmd)
		return
	for cmd in cmdlist:
		# p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False)
		p = subprocess.Popen(cmd, stdout=devnull, stderr=devnull, close_fds=True, shell=False)
		# p = subprocess.Popen(cmd, stdin=None, stdout=None, stderr=None, close_fds=True, shell=False)
		processes[p.pid] = p
		processdetails[p.pid] = '%s %s' % (cmd[-1], cmd[-2])
		print ('Process %d exec: %s' % (p.pid,cmd))
	while processes:
		pid, status = os.wait()
		if pid in processes:
			(s,rc) = processes[pid].communicate()
			print ('%s done: Process %d returned' % (processdetails[pid], pid))
			print ('\r\n')
			del processes[pid]
			del processdetails[pid]


def distributedFourfit(exptroot):
	'''Split the series of scans and baselines into a series of small jobs that run in parallel'''
	cwd = os.getcwd()
	jobs = makeJoblist(exptroot)
	Nmax = min(len(MACHINES),MAX_MACHINES)
	jobblocks = chunks(jobs, Nmax)
	for block in jobblocks:
		jobs = []
		for entry in block:
			i = block.index(entry)
			host = MACHINES[i]
			subdir,baseline = entry
			ssh = ["/usr/bin/ssh", "-t", host, "cd", cwd, ";"]
			prog = [FOURFIT]
			ffargs = FOURFIT_OPTS + ['-c', CONTROLFILE, '-b', baseline, subdir]
			cmd = ssh + prog + ffargs
			print ('Fitting %s %s on %s' % (subdir,baseline,host))
			jobs.append(cmd)
		parallel_Popen(jobs)

################################################################################################################

if __name__ == "__main__":

	args = parser.parse_args(sys.argv[1:])
	if args.help or len(args.rootdir) != 1:
		print(__doc__)
		sys.exit(0)

	CONTROLFILE = args.controlfile
	MAX_MACHINES = args.maxnodes
	FOURFIT = args.fourfitlauncher
	ANTENNAS_SUBSET = [ant for ant in args.antennas.split(',') if len(ant)==1]

	distributedFourfit(args.rootdir[0])

	# tidy up the terminal again, messed up by subprocess.Popen() before
	p = subprocess.Popen(["/usr/bin/stty", "sane"], stdin=None, stdout=None, stderr=None, close_fds=True, shell=False)
	p.communicate()
