#!/usr/bin/python3
'''
Usage: difxCheckMk6Modulepresence.py [-l|-L] <difxjob_0001.input> [<difxjob_0002.input> ...]

Verifies that any files referenced by the DiFX jobs are indeed available.

Files that match the pattern "/mark6-<nr>_fuse/<rest of path>.vdif" are
considered to be part of Mark6 FUSE file based correlation. Accessibility
of these Mark6 FUSE files is verified via SSH login and a file size check.

Files that do not match the above pattern are considered to be e-transferred
or copied Mark6 VDIF files that reside on the cluster file system. Accessibility
in this case is verified by a file size check done on the local host.

Options:
 -l    list only those jobs where all files all available
 -L    list only those jobs that have inaccessible files

(C) 2020 Jan Wagner
'''

# TODO: add MSN-based lookup once libmark6gather & .input support Mark6 MSNs,
#       the latter being critical (and ToDo...) to restrict the slots that libmark6gather collects data from

import subprocess
import sys

def checkDatastreamAvailabilities(difxinputfile, checkLocalFiles=True, verbose=True, listCompletes=False, listIncompletes=False):
	'''
	Collect all DATASTREAM entries from an .input file and check for availability of the
	referenced Mark6 FUSE files via SSH to the respective hosts on the MPIfR VLBI cluster.
	Optionally look up and check availability of non-Mark6 DATASTREAM files on BeeGFS.
	'''

	if verbose:
		print ('Job %s : ' % (difxinputfile))

	with open(difxinputfile, 'r') as f:

		datastreamfiles = []
		clusterfsfiles = []
		isComplete = True

		for line in f:
			if not 'FILE ' in line: continue
			dstreamfile = line[20:].strip()
			if not '/mark6-' in dstreamfile:
				clusterfsfiles.append(dstreamfile)
			else:
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
				isComplete = False
				if verbose:
					print('%s %s : MISSING' % (host,dstreamfile))
			else:
				if verbose:
					print('%s %s : ok' % (host,dstreamfile))

		if checkLocalFiles:
			for clusterfsfile in clusterfsfiles:
				p = subprocess.Popen(["/usr/bin/du", "-h", clusterfsfile], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
				(st,rc) = p.communicate()
				if len(st)<1:
					isComplete = False
					if verbose:
						print('%s %s : MISSING' % ("localhost",clusterfsfile))
				else:
					if verbose:
						print('%s %s : ok' % ("localhost",clusterfsfile))

		if listCompletes and isComplete:
			print(difxinputfile)
		if listIncompletes and not isComplete:
			print(difxinputfile)

if __name__ == "__main__":

	verbose = True
	listCompleteJobs = False
	listIncompleteJobs = False

	if '--help' in sys.argv or '-h' in sys.argv:
		print(__doc__)
		sys.exit(0)

	if '-l' in sys.argv:
		verbose = False
		listCompleteJobs = True
		sys.argv.remove('-l')

	if '-L' in sys.argv:
		verbose = False
		listIncompleteJobs = True
		sys.argv.remove('-L')

	for difxinputfile in sys.argv[1:]:
		checkDatastreamAvailabilities(difxinputfile, verbose=verbose, listCompletes=listCompleteJobs, listIncompletes=listIncompleteJobs)
