#!/usr/bin/python
'''
Usage: difxCheckMk6Modulepresence.py <difxjob_0001.input> [<difxjob_0002.input> ...]

Looks at the Mark6 recordings listed in the DiFX .input file(s) and for files that
match the pattern "/mark6-<nr>_fuse/<rest of path>.vdif" attempts to log in on
the respective Mark6 and checks if the scan is really available.
'''

import re, sys, commands, subprocess

def check_inputfile(fn):
	print ('File %s : ' % (fn))
	with open(fn, 'r') as f:
		datastreamfiles = []
		for line in f:
			if not 'FILE ' in line: continue
			dstreamfile = line[20:].strip()
			if not '/mark6-' in dstreamfile: continue
			datastreamfiles.append(dstreamfile)
		spawned = []
		for dstreamfile in datastreamfiles:
			host = dstreamfile[1:].split('/')[0]
			host = host.split('_fuse')[0]
			p = subprocess.Popen(["/usr/bin/ssh", host, "/usr/bin/du", "-h", dstreamfile], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
			spawned.append(p)
		for i in range(len(datastreamfiles)):
			(st,rc) = spawned[i].communicate()
			dstreamfile = datastreamfiles[i]		
			host = dstreamfile[1:].split('/')[0]
			host = host.split('_fuse')[0]
			# print (host, st)
			if len(st)<1:
				print('%s %s : missing ' % (host,dstreamfile))
			else:
				print('%s %s : ok ' % (host,dstreamfile))


if '--help' in sys.argv:
	print(__doc__)
	sys.exit(0)

if '-h' in sys.argv:
	print(__doc__)
	sys.exit(0)

for fn in sys.argv[1:]:
	check_inputfile(fn)
