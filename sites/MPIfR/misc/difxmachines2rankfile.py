#!/usr/bin/python
'''
Convert DiFX <difxjob>.machines MPI hostfiles into OpenMPI rank files.

Usage: difxmachines2rankfile.py <difxjob[.machines]> [<difxjob[.machines]>]

Rank files can be used like:
$ mpirun --oversubscribe -v --mca mpi_yield_when_idle 1 -rf job_1000.rankfile mpifxcorr job_1000.input
'''

import sys

def convert(basename):
	if basename.endswith(('.difx','.input','.machines','.calc','.im','.difxlog')):
		basename = basename[:basename.rfind('.')]
	corecounts = {}
	with open(basename + '.machines','r') as fi:
		fo = open(basename + '.rankfile', 'w')
		hosts = fi.readlines()
		for n in range(len(hosts)):
			hostname = hosts[n].strip()
			if hostname not in corecounts:
				corecounts[hostname] = 0
			core = corecounts[hostname]
			fo.write('rank %d=%s slot=%d\n' % (n, hostname, core))
			# or even more specific: fo.write('rank %d=%s slot=%d:%d\n' % (n, hostname, socket, core))
			corecounts[hostname] += 1
		fo.close()

if __name__ == "__main__":

	if len(sys.argv) < 2 or sys.argv[1][0] == '-':
		print(__doc__)
		sys.exit(-1)

	for job in sys.argv[1:]:
		convert(job)
