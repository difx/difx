#!/usr/bin/python
'''
Usage: difxCopyMk6Goldenscans.py [-b|--prefix=<b1|b2|b3|b4>] [-d|--dry-run]
                                 <difxjob_0001.input> [<difxjob_0002.input> ...]

Script to copy out all Mark6 VDIF recordings related to a correlation job.
May be used to extract "golden scan" sets from currently loaded Mark6 modules.
The Mark6 units are assumed to have their modules already FUSE-mounted.

Scans are copied to

  /data/eht2018/goldenscans/<2-letter station ID>/<prefix>/<slots>/

where <prefix> can designate e.g. an EHTC 2018 band (default: b1).

(C) 2019 Jan Wagner
'''

import argparse, os, sys, subprocess

## Defaults
OUT_BAND = 'b1'                           # default band name (acceptable might be: b1, b2, b3, b4)
OUT_ROOT = '/data/eht2018/goldenscans/'   # default base path for copying output into

## Args setup; --help is overriden to display doc string of module, am lazy to reformat...
parser = argparse.ArgumentParser(add_help=False,
	description='Copy all Mark6 VDIF recordings of a correlation job from module to disk.')
parser.add_argument('-h', '--help', help='Help', action='store_true')
parser.add_argument('-b', '--prefix', default='b1', dest='bandprefix',
	help='Prefix to use as part of output file paths, e.g, b1, b2, b3, or b4 (default: b1)')
parser.add_argument('-d','--dry-run', dest='dryrun', help='Show commands but do not actually copy out scans',
	action='store_true')
parser.add_argument('jobfiles', nargs='*')


def build_dst_path(src):
	'''Generate string with file path for Mark6->destination file'''
	return '%s/%s/%s/%s/' % (OUT_ROOT, src['station'], OUT_BAND, src['slot'])


def get_input_filelist(fn):
	'''
	Extract all references to Mark6 files from .input
	and return them as a list of hostnames, paths, station, slot.

	Files of a normal DiFX correlation with FUSE mounts are
	referenced from .input by, for example,

	D/STREAM 0 FILES:   1
	FILE 0/0:           /mark6-07_fuse/12/e18e22_Aa_No0006.vdif
	D/STREAM 1 FILES:   1
	FILE 1/0:           /mark6-07_fuse/34/e18e22_Aa_No0006.vdif
	D/STREAM 2 FILES:   1
	FILE 2/0:           /mark6-06_fuse/12/e18e22_Sz_111-2328.vdif
	D/STREAM 3 FILES:   1
	FILE 3/0:           /mark6-06_fuse/34/e18e22_Sz_111-2328.vdif

	where the file paths are the same as mountpoint and path
	on each particular Mark6 unit. For example,

	/mark6-07_fuse/12/e18e22_Aa_No0006.vdif

	is the path and filename on a Mark6 unit having hostname 'mark6-07'.

	'''
	datastreamfiles = []
	with open(fn, 'r') as f:
		for line in f:
			if not 'FILE ' in line: continue
			dstreamfile = line[20:].strip()
			if not '/mark6-' in dstreamfile: continue
			datastreamfiles.append(dstreamfile)

	filelist = []
	for dstreamfile in datastreamfiles:
		pathparts = dstreamfile[1:].split('/')
		file = {}
		file['host'] = pathparts[0].split('_fuse')[0]
		file['slot'] = pathparts[1]
		file['scan'] = pathparts[-1]
		file['station'] = file['scan'].split('_')[1]
		file['src'] = dstreamfile
		file['dst_path'] = build_dst_path(file)
		file['dst'] = build_dst_path(file) + file['scan'] 
		filelist.append(file)

	return filelist


def parallel_Popen(cmdlist, dryrun=False):
	'''Launch commands in parallel and wait for their completion'''
	processes = {}
	if dryrun:
		for cmd in cmdlist:
			print (cmd)
		return
	for cmd in cmdlist:
		p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False)
		processes[p.pid] = p
		print ('Process %d exec: %s' % (p.pid,cmd))
	while processes:
		pid, status = os.wait()
		if pid in processes:
			(s,rc) = processes[pid].communicate()
			del processes[pid]
			print ('Process %d returned:\n%s' % (pid,s))


def copy_files(filelist, dryrun=False):
	'''
	Copy all specified files in parallel via SSH-invoked 'cp' after first
	making sure the output directories exist.
	'''
	mkdir, du, cp = [], [], []
	for file in filelist:
		mkdir.append( ['/usr/bin/ssh', 'oper@%s'%(file['host']), '/usr/bin/mkdir', '-v', '-p', file['dst_path']] )
	parallel_Popen(mkdir, dryrun)
	for file in filelist:
		du.append( ['/usr/bin/ssh', 'oper@%s'%(file['host']), '/usr/bin/du', '-h', file['src']] )
	parallel_Popen(du, dryrun)
	for file in filelist:
		cp.append( ['/usr/bin/ssh', 'oper@%s'%(file['host']), '/usr/bin/cp', '-a', file['src'], file['dst']] )
	parallel_Popen(cp, dryrun)


def handle_jobs(args):
	'''
	Process a list of DiFX .input file(s) and copy out all Mark6 scans
	from the mounted modules onto disk, in parallel, on all Mark6 units.
	'''
	copied = []
	for fn in args.jobfiles:
		filelist = get_input_filelist(fn)
		copy_files(filelist, args.dryrun)
		for file in filelist:
			copied.append(file['dst'])
	print('')
	print ('Finished. The copied files are found at:')
	for fn in copied:
		print(fn)

args = parser.parse_args(sys.argv[1:])
if args.help or not args.jobfiles:
	print(__doc__)
	sys.exit(0)

OUT_BAND = args.bandprefix

handle_jobs(args)


