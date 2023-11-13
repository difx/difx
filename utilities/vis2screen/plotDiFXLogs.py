#!/bin/python3
'''
Plot some project statistics based upon .difxlog and .machines files
in one or more project top level directories.

Produces histograms of "wallclock time" durations of the DiFX job,
number of nodes per job, the idle time between jobs, as well as a
scatter plot of job duration vs its node count.

Allows comparisons between DiFX runs of the same project run with
different cluster node allocations, and also different projects of
generally the same kind e.g. IVS R1.
'''

import matplotlib.pyplot as plt
import numpy as np

import argparse
import datetime
import glob
import os, sys

__author__ = "Jan Wagner (MPIfR)"
__version__ = "1.0.1"


def parse_args(args: []):

	parser = argparse.ArgumentParser(description=__doc__, add_help=True, formatter_class=argparse.RawDescriptionHelpFormatter)
	parser.add_argument('-f', '--maxfiles',	    help='Inspect at most <n> files per project dir (0=unlimited)', default=0, type=int)
	parser.add_argument('-r', '--maxruntime',   help='Max expected job run time (default: %(default)d)', default=150, type=int)
	parser.add_argument('-i', '--maxidletime',  help='Max expected idle time between jobs (default: %(default)d)', default=180, type=int)
	parser.add_argument('-n', '--maxnodes',     help='Max number of nodes expected (default: %(default)d)', default=70, type=int)
	parser.add_argument('-d', '--use-dirname',  help='Label data not by project but by subdirectory', action='store_true')
	parser.add_argument('directories', nargs='*')
	return parser.parse_args(args)


def getStartingTime(difxlog):
	'''
	Open 'difxlog' and get the timestamp of the first entry.
	Return it as a Unix time ie seconds since Unix Epoch.
	'''

	# Note, none of os.path.getatime, .getmtime, .getctime,
	# nor os.stat (C fstat()) return a file creation date,
	# only at best the last file modification time.
	# Hence, read the time from the difx log instead.

	with open(difxlog, 'r') as df:
		line = df.readline()
		# line = "Fri Aug 26 10:54:20 2022   2 io02 INFO  MPI Process 2 is running on host io02"
		# line = "Tue Apr  5 17:13:37 2022  16 node15.service INFO  MPI Process 16 is running on host node15.service"
		try:
			tstamp = datetime.datetime.strptime(line[:25].strip(), '%a %b %d %H:%M:%S %Y')
			return tstamp.timestamp()
		except ValueError:
			print ("Could not parse time from %s entry '%s'" % (difxlog, line))

	return 0


def getWallclockTime(difxlog):
	'''
	Open 'difxlog' and look for a line similar to
	INFO  Total wallclock time was **5.10744** seconds

	NB: Since .difxlog get appended, its possible to encounter
	multiple wallclock time entries. Use the last one found.
	'''
	t = 0
	with open(difxlog, 'r') as df:
		for line in df.readlines():
			if 'wallclock time' in line:
				t = float( line.split('**')[1] )
	return t


def getNodeCount(difxlog):
	'''
	Check .machines file corresponding to the difxlog,
	return the number of nodes (nr of lines) in the machinesfile
	'''
	machinesfile = difxlog.split('.difxlog')[0] + '.machines'
	with open(machinesfile, 'r') as mf:
		lines = mf.readlines()
		return len(lines)
	return 0


def getDataSeries(path, Nmax=0):
	'''
	Return various data for all (or at most Nmax) .difxlog logs
	and related files that can be found under the given 'path'.
	'''
	starttimes, wallclocktimes, nodecounts = [], [], []

	for difxlog in glob.iglob(path + '/*.difxlog'):

		t = getStartingTime(difxlog)
		wt = getWallclockTime(difxlog)
		nc = getNodeCount(difxlog)
		# print('From ',difxlog, ' got ', t, wt, nc)

		starttimes.append(t)
		wallclocktimes.append(wt)
		nodecounts.append(nc)

		if Nmax > 0 and len(starttimes) == Nmax:
			break

	return (starttimes,wallclocktimes, nodecounts)


def getExperimentName(path):
	'''Return the name of experiment, determined from the prefix of the first log file (<expt>_<jobN>r.difxlog)'''
	for difxlog in glob.iglob(path + '/*.difxlog'):
		logname = os.path.split(difxlog)[1]		  # eg /path/r1999_0013.difxlog => r1999_0013.difxlog
		basename = logname.split('.difxlog')[0]   # eg r1999_0013.difxlog => r1999_0013
		exptname = basename.split('_')[0]
		return exptname
	return 'n/a'


def getExperimentSubdirname(path):
	'''
	Return "subdir" part of 'path'.
	Can be used to label plots of the same experiment correlated multiple times.
	'''
	s = path.split('/')
	if len(s[-1]) > 0:
		# format was /path/subdir (no trailing slash)
		return s[-1]
	# format was /path/subdir/
	return s[-2]


if __name__ == "__main__":

	userargs = parse_args(sys.argv[1:])
	if len(userargs.directories) < 1:
		sys.exit(0)

	# Bins

	bins_time = range(0, userargs.maxruntime, 5)
	bins_idletime = range(0, userargs.maxidletime, 5)
	bins_numnodes = range(0, userargs.maxnodes, 2)

	# Data and plots

	fig, ((ax1,ax2),(ax3,ax4)) = plt.subplots(2, 2, constrained_layout=True)

	for path in userargs.directories:

		if userargs.use_dirname:
			expt = getExperimentSubdirname(path)
		else:
			expt = getExperimentName(path)

		data_tstartUnix, data_twall, data_nodecount = getDataSeries(path, Nmax=userargs.maxfiles)

		iasc = np.argsort(data_tstartUnix) # indices that provide data_tstartUnix sorted by increasing time
		tstartUnixSorted = [data_tstartUnix[ii] for ii in iasc]
		tendUnixSorted = [data_tstartUnix[ii] + data_twall[ii] for ii in iasc]
		t_gaps = [tstartUnixSorted[i+1] - tendUnixSorted[i] for i in range(len(tstartUnixSorted)-1)]

		ax1.hist(data_twall, bins_time, alpha=0.5, label=expt)
		ax2.hist(data_nodecount, bins_numnodes, alpha=0.5, label=expt)
		ax3.scatter(data_nodecount, data_twall, alpha=0.5, label=expt)
		ax4.hist(t_gaps, bins_idletime, alpha=0.5, label=expt)

		plt.draw()

	# Captions

	ax1.set_title("Distribution of job durations")
	ax1.legend(loc='upper right')
	ax1.set_xlabel('Time (seconds)')

	ax2.set_title("Distribution of nodes per job")
	ax2.legend(loc='upper right')
	ax2.set_xlabel('Nodes used (#)')

	ax3.set_title("Job duration against nodes allocated")
	ax3.legend(loc='upper right')
	ax3.set_xlabel('Nodes used (#)')
	ax3.set_ylabel('Time (seconds)')

	ax4.set_title("Distribution of time idle between jobs")
	ax4.legend(loc='upper right')
	ax4.set_xlabel('Time (seconds)')

	fig.set_size_inches(10, 6)
	plt.show()
