#!/usr/bin/env python3

from os import popen, system
import argparse

programname = 'mpifxcorr'
verbose = 1
killfirst = False
force = False

parser = argparse.ArgumentParser()
parser.add_argument('-f', '-force', '--force', help="Kill all processes", action="store_true")
parser.add_argument('--first', '-first', help="Kill first process", action="store_true")
args = parser.parse_args()

if args.force:
	force = True
if args.first:
	killfirst = True

def getpids():
	cmd = 'ps -e | grep %s' % programname
	ps = popen(cmd, 'r')
	pslines = ps.readlines()
	ps.close()
	pids = []
	for p in pslines:
		s = p.strip().split()
		if len(s) < 2:
			continue
		if s[3] != programname:
			continue
		pids.append(int(s[0]))
	return pids

pids = getpids()

if verbose > 0:
	print('PIDs = ', pids)

if len(pids) == 0:
	print('no %s process appears to be running' % programname)
elif len(pids) == 1:
	cmd = 'kill -INT %d' % (pids[0])
	print('killing a process with: ', cmd)
	system(cmd)
elif killfirst:
	cmd = 'kill -INT %d' % (pids[0])
	print('killing first matching process with: ', cmd)
	system(cmd)
elif force:
	for p in pids:
		cmd = 'kill -INT %d' % (p)
		print('killing a process with: ', cmd)
		system(cmd)
else:
	print('too many processes called %s running.  not killing' % programname)
	print('kill by hand with "kill -INT <pid>" where <pid> is one of ', pids)
