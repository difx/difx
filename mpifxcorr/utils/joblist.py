#!/usr/bin/env python3

#**************************************************************************
#   Copyright (C) 2008-2021 by Walter Brisken                             *
#                                                                         *
#   This program is free software; you can redistribute it and/or modify  *
#   it under the terms of the GNU General Public License as published by  *
#   the Free Software Foundation; either version 3 of the License, or     *
#   (at your option) any later version.                                   *
#                                                                         *
#   This program is distributed in the hope that it will be useful,       *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
#   GNU General Public License for more details.                          *
#                                                                         *
#   You should have received a copy of the GNU General Public License     *
#   along with this program; if not, write to the                         *
#   Free Software Foundation, Inc.,                                       *
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
#**************************************************************************
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL: $
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

from sys import argv, exit
from glob import glob

program = 'joblist'
version = '1.7'
verdate = '20210417'
author  = 'Walter Brisken'

showstatus = 1

def printVersion():
	print('%s ver. %s   %s %s' % (program, version, verdate, author))

def usage():
	print('')
	printVersion()
	print('\n%s ver. %s   %s %s' % (program, version, verdate, author))
	print('\nA program to list information about DiFX job files\n')
	print('Usage: [options] [<dir 1> [<dir 2> [ ... ] ] ]\n')
	print('<dir> is a directory containing .input files.  Many directories can be')
	print('      listed.  If no directory is listed, the current directory is assumed.\n')
	print('options can include:')
	print('   -h or --help  : print this usage information\n')
	print('   --version     : print version info and quit\n')
	print('\nThe characters printed within [ ] indicated the following:')
	print('  c  .calc       model parameters and others')
	print('  m  .machines   MPI input file -- cluster configuration')
	print('  t  .threads    how many processing threads per core node')
	print('  i  .im         polynomials for delay, u,v,w, and atmosphere')
	print('  v  .difx/      visibilities produced by mpifxcorr\n')

	exit(0)

def hasfits(input):
	s = input.split('job')
	if len(s) == 1:
		f = s[0]
	elif len(s) == 2:
		f = s[0] + s[1]
	else:
		b = s[0]
		for q in s[1:-1]:
			b = b + 'job' + q
		f = b + s[-1]

	fitsfile = f[:-5]+'FITS'
	
	if len(glob(fitsfile)) > 0:
		return 1
	
	return 0

def genband(fq):
	band = ''
	bands = [ \
		[300.0, 400.0, 'P'], \
		[400.0, 800.0, '6'], \
		[1000.0, 2000.0, 'L'], \
		[2000.0, 4000.0, 'S'], \
		[4000.0, 7000.0, 'C'], \
		[7000.0, 10000.0, 'X'], \
		[10000.0, 16000.0, 'U'], \
		[16000.0, 26500.0, 'K'], \
		[26000.0, 38000.0, 'A'], \
		[38000.0, 50000.0, 'Q'], \
		[75000.0, 98000.0, 'W'] ]
		
	for b in bands:
		ok = 1
		for f in fq:
			if f > b[0] and f < b[1] and ok:
				band = band + b[2]
				ok = 0
	while len(band) < 2:
		band = band + ' '

	return band
	
def getstat(input):
	base = input[0:-5]
	g = glob(base+'*')

	files = [['calc', 'c'], \
		 ['machines', 'm'], \
		 ['threads', 't'], \
		 ['im', 'i'], \
		 ['difx', 'v']]

	s = '['
	for f in files:
		if base+f[0] in g:
			s = s + f[1]
		else:
			s = s + ' '
	s = s + ']'
	return s
	
def summarize(input, i, n, dotree):
	if i == n-1:
		c = '`'
	else:
		c = '|'
	s = input.split('/')
	data = open(input).readlines()
	if showstatus:
		stat = getstat(input)
	else:
		stat = ' '
	sum = ''
	ants = ''
	nfreq = 0
	nbit = 0
	bw = 0.0
	npol = 0
	fq = []
	nsec = 0
	triple = ''
	oversamp = 1
	for d in data:
		if d[:19] == 'EXECUTE TIME (SEC):':
			nsec = int(d[20:])
			sum = sum + (' %4.1fmin' % (float(nsec)/60.))
		if d[:14] == 'TELESCOPE NAME':
			ants = ants + d[20:].strip() + ','
		if d[:13] == 'FREQ ENTRIES:':
			nfreq = int(d[20:])
		if d[:18] == 'QUANTISATION BITS:':
			nbit = int(d[20:])
		if d[:9]  == 'BW (MHZ) ':
			bw = float(d[20:])
		if d[:11] == 'FREQ (MHZ) ':
			fq.append(float(d[20:]))
		if d[:17] == 'POL PRODUCTS 0/0:':
			npol = int(d[20:])
		if d[:14] == 'CONFIG SOURCE:':
			oversamp = 1
		if d[:18] == 'OVERSAMPLE FACTOR:':
			oversamp *= int(d[20:])
		if d[:11] == 'DATA FORMAT':
			p = d[20:].find('-')
			if p > 0:
				triple = d[p+21:].strip()
			
	if npol == 4:
		nchan = nfreq*2
	else:
		nchan = nfreq*npol

	if triple == '':
		triple = '%d-%d-%d' % (int(oversamp*bw*nchan*nbit*2+0.5), nchan, nbit)
			
	band = genband(fq)
	
	sum = '%s %s%6.1fm %8s  %s' % (stat, band, nsec/60., triple, ants[:-1])
	if dotree:
		tree = '%c- ' % c
	else:
		tree = ''
	print('%s%s %s' % (tree, s[-1][0:-6], sum))
	
def run(path, dotree):
	if dotree:
		print(path)
	inputs = glob(path+'/*.input')
	inputs.sort()
	n = len(inputs)
	for i in range(n):
		summarize(inputs[i], i, n, dotree)

paths = []

if len(argv) == 1:
	paths.append('.')
else:
	for a in argv[1:]:
		if a[0] == '-':
			if a in ['-h', '--help']:
				usage()
			elif a == '--version':
				printVersion()
				exit(0)
		else:
			paths.append(a)

if len(paths) > 1:
	dotree = 1
else:
	dotree = 0

for p in paths:
	run(p, dotree)
