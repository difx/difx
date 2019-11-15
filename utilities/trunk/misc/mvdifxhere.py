#!/usr/bin/env python3

# Note: this utility can run under python2.7 or python3

from os import getcwd, system, popen
from sys import argv, exit

program = 'mvdifxhere'
author = 'wbrisken@nrao.edu'
version = '0.2'
verdate = '20191110'

def usage():
	print('\n%s  ver. %s  %s  %s\n' % (program, version, author, verdate))
	print('Changes paths inside .input and .calc files to the current directory.')
	print('Note: this does not actually move the files.\n')
	print('Usage: %s [options] [ <file1> [ <file2> ... ] ]\n' % program)
	print('options can be:')
	print('  --help')
	print('  -h     print this help info and quit\n')

def processFile(fileName, outDir, searchKey):
	line = popen('grep "%s" %s' % (searchKey, fileName)).readlines()[0]
	inDir = line.strip().split()[-1]
	p = inDir.rfind('/')
	inDir = inDir[:(p+1)]
	if inDir != outDir:
		print('%s: %s -> %s' % (fileName, inDir, outDir))
		cmd = 'sed -i "s/%s/%s/" %s' % (inDir.replace('/', '\\/'), outDir.replace('/', '\\/'), fileName);
		system(cmd)
	else:
		print('%s: already in correct place; skipping' % fileName)

if len(argv) < 2:
	usage()
	exit(0)

cwd = getcwd()
if cwd[-1] != '/':
	cwd += '/'

print('Repositioning files to %s' % cwd)

for a in argv[1:]:
	if a in ['-h', '--help']:
		usage()
		exit(0)
	if a[-5:] == '.calc':
		processFile(a, cwd, "IM FILENAME:")
	elif a[-6:] == '.input':
		processFile(a, cwd, "CALC FILENAME:")
	else:
		print('%s: unrecognized file type; skipping' % a)
