#!/usr/bin/python3
'''
Usage: distFourfit.py [-c <controlfile>] [-F <frequency group] [-n <max nr of nodes>]
                      [-m <difx_machines>] [-f <fourfit wrapper script>]
                      <expt_root>

Runs fourfit on all baselines of the scans located under ./<expt_root>/. 
Fourfit is executed in parallel, one instance fitting one baseline of one scan.
The fourfit instances are distributed over the cluster nodes.

Options:
   -c <controlfile>      HOPS fourfit control file to use for fringe fitting
   -F <frequency group>  Frequeny group to process, e.g., S
   -n <max nr of nodes>  Integer number, how many nodes to use at most
   -m <difx_machines>    DiFX machines file to use. NB: DiFX format, not MPI machinesfile.
                         The default is $DIFX_MACHINES when available.
   -f <wrapper>          Wrapper script that sets up HOPS environment and calls fourfit
                         The simplest wrapper script would be
                           <</cluster/hops/fourfit_dtrunk>>
                           source /data/cluster/difx/DiFX-trunk_64/setup_difx
                           fourfit $@

Required arguments:
   <expt_root>           The HOPS Mark4 top-level folder; experiment directory.
'''

import argparse
import glob, os, sys, time
import subprocess

# Note: mpi4py is not used as it needs CentOS openmpi but the MPIfR
#       cluster runs a custom built Gemmatix/GxHive OpenMPI...
#
# Todo: add logging, but: (1) popen() PIPE hangs as too much output, 
#       (2) fourfit has no --logfile <fn> option
#       -> use " |& tee <scan_baseline.log> ", despite producing a clutter of thousands (IVS..) of log files?

from difxfile.difxmachines import DifxMachines
from typing import List, Set

################################################################################################################

DEFAULT_FOURFIT = '/cluster/hops/fourfit_dtrunk'
DEFAULT_FOURFIT_OPTS = ['-m0', '-u']
DEFAULT_CF = 'cf_1234'
DEFAULT_FREQ_GROUP = ''
DEFAULT_MAX_HOSTS = 80
CPU_PER_NODE = 4

parser = argparse.ArgumentParser(add_help=False, description='Runs fourfit on all baselines of the scans located under ./<expt_root>/')
parser.add_argument('-h', '--help',        help='Help', action='store_true')
parser.add_argument('-a', '--ant',         help='Fit only baselines to these antenna(s). Comma separated 1-letter codes.', dest='antennas', default='')
parser.add_argument('-F', '--fgroup',      dest='freqgroup', help='Frequeny group to process, e.g., S', default=DEFAULT_FREQ_GROUP)
parser.add_argument('-f', '--fourfit',     dest='fourfitwrapper', help='Fourfit setup and invocation script to use', default=DEFAULT_FOURFIT)
parser.add_argument('-c', '--controlfile', dest='controlfile', help='Fourfit control file to use for fringe fitting', default=DEFAULT_CF)
parser.add_argument('-n', '--maxnodes',    dest='maxnodes', help='Run at most n nodes', default=DEFAULT_MAX_HOSTS)
parser.add_argument('-m', '--difxmachines',dest='difxmachinesfile', help='Get machines from DiFX machines file', default='')
parser.add_argument('expt_root', nargs='*')

################################################################################################################

class MachineList:
	'''
	Class for keeping a list of hostnames on which fourfit is permitted to be run.
	Hostnames are collected either from: (a) internal default list, (b) $DIFX_MACHINES env var
    difx machines file, (c) user-specified DiFX machines file
	'''

	def __init__(self, machinesfile=None):
		self._machines = []
		self.loadMachines(machinesfile)

	def loadMachines(self, machinesfile=None):
		self._machines = []
		if machinesfile:
			return self.parseMachinesFile(machinesfile)
		if len(os.environ['DIFX_MACHINES']) > 0:
			print('Getting machine list from $DIFX_MACHINES env file')
			return self.parseEnvMachinesFile()
		return self.setDefaultMachines()

	def setDefaultMachines(self) -> List[str]:
		nCores = 4
		self._machines = ['fxmanager']
		self._machines += ['node%02d' % (n) for n in range(2,32)]
		self._machines = self._machines * 4
		return True

	def parseMachinesFile(self, machinesfile) -> List[str]:
		difxmachines = DifxMachines(machinesfile)
		self._machines = [node.name for node in difxmachines.getComputeNodes()]
		return len(self._machines) > 0

	def parseEnvMachinesFile(self) -> List[str]:
		return self.parseMachinesFile(os.environ['DIFX_MACHINES'])

	def machines(self):
		return self._machines


################################################################################################################

class ExperimentDirAnalyzer:
	'''
	Inspect the content of subdirectories under an experiment dir (e.g., subdirs under ./1234/).
	Can optionally limit the inspection to only baselines to specific antennas (1-letter IDs).
	'''

	def __init__(self, antennaSubset=[]):
		self.limitToAntennas(antennaSubset)

	def limitToAntennas(self, antennaSubset):
		self._antennaSubset = antennaSubset

	def listScans(self, rootdir: str) -> List[str]:
		'''Return a list of subdirectories (scans) under experiment root dir'''
		scandirs = [dname for dname in glob.glob(rootdir + '/*') if os.path.isdir(dname)]
		return scandirs

	def listScanBaselines(self, scandir: str, doAuto=False) -> Set[str]:
		'''Return baselines in subdirectory. Example: ('AX', 'AL', 'AS', 'SX', 'LX', 'SL')'''
		baselines = set()
		for fpath in glob.glob(scandir + '/' + '??..*'):
			fn = os.path.split(os.path.basename(fpath))[-1]
			if (fn[0] == fn[1]) and not doAuto:
				continue
			if self._antennaSubset and not any([antId in fn[0:2] for antId in self._antennaSubset]):
				continue
			baselines.add(fn[:2])
		return baselines

	def listScanFittedBaselines(self, scandir: str, doAuto=False, freqgroup='?') -> Set[str]:
		'''Return a list of already fringe fitted baselines in subdirectory. Example: ['AX', 'AL', 'AS', 'SX', 'LX', 'SL']'''
		baselines = set()
		if len(freqgroup) is not 1:
			freqgroup = '?'
		globpattern = scandir + '/' + '??.' + freqgroup + '.*'
		for fpath in glob.glob(globpattern):
			fn = os.path.split(os.path.basename(fpath))[-1]
			if (fn[0] == fn[1]) and not doAuto:
				pass
			elif self._antennaSubset and not any([antId in fn[0:2] for antId in self._antennaSubset]):
				pass
			else:
				baselines.add(fn[:2])
		return baselines

	def generateFourfitJobs(self, exptroot: str, freqgroup: str=''):
		'''Make a job list: list of (subdir,baseline) tuples'''
		jobs = []
		Ncompleted = 0
		for scan in self.listScans(exptroot):
			blines = self.listScanBaselines(scan)
			completed = self.listScanFittedBaselines(scan, freqgroup=freqgroup)
			blines = list(blines - completed)
			jobs += zip([scan]*len(blines), blines)
			Ncompleted += len(completed)
		print('Fourfit jobs: %d new, %d previously completed' % (len(jobs), Ncompleted))
		return jobs


################################################################################################################

class JobDispatch:
	'''
	Launch all jobs across the list of hosts in MachineList.

	If there are more jobs than hosts, a smaller group of jobs is
	launched first. Upone completion the next group is launched.

 	Currently, the slowest fourfit in a group becomes the bottleneck; a more
	dynamic approach with an idle hosts pool fed by pending tasks is ToDo.
	'''

	def __init__(self, fourfit=DEFAULT_FOURFIT, hopscf=DEFAULT_CF, fourfitopts=DEFAULT_FOURFIT_OPTS):
		self._fourfit = fourfit
		self._controlfile = hopscf
		self._fourfitopts = fourfitopts
		self._freqgroupname = ''
		self._freqgrouparg = ''
		self._machines = []
		self._maxnodes = 1

	def setFourfitWrapper(self, wrapperpath: str):
		self._fourfit = wrapperpath

	def setFourfitControlFile(self, controlfile: str):
		self._controlfile = controlfile

	def setFourfitOptions(self, fourfitopts: str):
		self._fourfitopts = fourfitopts

	def setFreqGroup(self, freqgroupname: str):
		if len(freqgroupname) > 1:
			print("Unexpected frequency group '%s', longer than 1 character" % (freqgroupname))
		else:
			self._freqgroupname = freqgroupname
			if len(freqgroupname) == 0:
				self._freqgrouparg = ''
			else:
				self._freqgrouparg = ":" + self._freqgroupname

	def setMaxNodes(self, N=20):
		self._maxnodes = N

	def setMachines(self, machines=[]):
		self._machines = machines

	def _group(self, L: List[str], N: int) -> List[str]:
		'''Yield successive N-sized group from list L.'''
		last = len(L)
		for i in range(0, len(L), N):
			end = i+N
			if end > last:
				end = last
			yield L[i:end]

	def generateJobs(self, exptroot: str, antennaSubset=None):
		'''
		Split the set of scans+baselines under experiment dir into a series of small jobs that run in parallel
		'''
		analyzer = ExperimentDirAnalyzer()
		if antennaSubset:
		    analyzer.limitToAntennas(antennaSubset)
		jobs = analyzer.generateFourfitJobs(exptroot, self._freqgroupname)
		return jobs

	def executeJobs(self, jobs):
		'''
		Executes a list of fourfit jobs in parallel.
		The task is split into a series of smaller jobs if necessary.
		'''
		hosts = self._machines * CPU_PER_NODE  # todo: some better approach to this!
		Jmax = min(len(hosts), self._maxnodes)
		cwd = os.getcwd()

		jobblocks = self._group(jobs, Jmax)
		for block in jobblocks:

			subjobs = []
			for job in block:
				i = block.index(job)
				subdir,baseline = job
				host = hosts[i]

				ssh = ["/usr/bin/ssh", "-t", host]
				cdwork = ["cd", cwd]
				fourfit = [self._fourfit] + self._fourfitopts + ['-c', self._controlfile, '-b', baseline + self._freqgrouparg, subdir]
				cmd = ssh + cdwork + ['&&'] + fourfit

				print ('Fitting %s %s on %s' % (subdir,baseline,host))
				subjobs.append([subdir,baseline,host,cmd])

			self._runJobgroup(subjobs)

	def _runJobgroup(self, joblist, dryrun=False):
		'''Launch jobs in parallel and wait for their completion'''
		if dryrun:
			for job in joblist:
				cmd = job[-1]
				print (cmd)
			return

		processes = {}
		processdetails = {}
		for job in joblist:
			cmd = job[-1]
			p = subprocess.Popen(cmd, stdin=None, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, shell=False)
			processdetails[p.pid] = '%s:%s on %s' % (job[0],job[1],job[2])
			processes[p.pid] = p

		while processes:

			completed = {}
			for pid in processes.keys():
				if pid in completed:
					continue
				rc = processes[pid].poll()
				if rc is not None:
					(s,rc) = processes[pid].communicate()
					self._cleanTerm()
					print ('Finished %s, %d remain\n' % (processdetails[pid], len(processes) - len(completed) - 1), flush=True)
					completed[pid] = 1

			if len(completed) > 0:
				for pid in completed.keys():
					del processes[pid]
					del processdetails[pid]

		self._cleanTerm()
		print('Done')

	def _cleanTerm(self):
		'''Reset terminal. Workaround for SSH+popen() which mess up the local terminal such that print() newlines cease to work'''
		p = subprocess.Popen(["/usr/bin/stty", "sane"], stdin=None, stdout=None, stderr=None, close_fds=True, shell=False)
		p.communicate()

################################################################################################################

if __name__ == "__main__":

	args = parser.parse_args(sys.argv[1:])
	if args.help or len(args.expt_root) != 1:
		print(__doc__)
		sys.exit(0)

	nodepool = MachineList()
	handler = JobDispatch()

	if args.difxmachinesfile:
		nodepool.loadMachines(difxmachinesfile)

	expt_path = args.expt_root[0]
	antennaSubset = [ant for ant in args.antennas.split(',') if len(ant)==1]

	handler.setFourfitWrapper(args.fourfitwrapper)
	handler.setFourfitControlFile(args.controlfile)
	handler.setFreqGroup(args.freqgroup)
	handler.setMaxNodes(int(args.maxnodes))
	handler.setMachines(nodepool.machines())

	jobs = handler.generateJobs(expt_path, antennaSubset)
	handler.executeJobs(jobs)

	# tidy up the terminal again, messed up by subprocess.Popen() before
	p = subprocess.Popen(["/usr/bin/stty", "sane"], stdin=None, stdout=None, stderr=None, close_fds=True, shell=False)
	p.communicate()
