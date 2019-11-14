#!/usr/bin/env python3

#**************************************************************************
#   Copyright (C) 2008-2019 by Walter Brisken                             *
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
import os
import stat

program = 'jobstatus'
version = '1.2'
verdate = '20191114'
author  = 'Walter Brisken'

def usage():
	print('\n%s ver. %s   %s %s' % (program, version, verdate, author))
	print('\nA program to list the status of DiFX job files\n')
	print('Usage: [options] [<dir 1> [<dir 2> [ ... ] ] ]\n')
	print('<dir> is a directory containing .input files.  Many directories can be')
	print('      listed.  If no directory is listed, the current directory is assumed.\n')
	print('options can include:')
	print('   -h or --help  : print this usage information\n')
	exit(0)

def getelapsed(file1, file2, N):
	P = os.popen('head -c %d %s' % (256, file1), 'r')
	D1 = P.read(256)
	P.close()
	P = os.popen('tail -c %d %s' % (N, file2), 'r')
	D2 = P.read(N)
	P.close()
	p1 = D1.find("MJD:      ");
	p2 = D2[:-50].rfind("MJD:      ");
	
	if p1 < 0 or p2 < 0:
		return -1;

	S1 = D1[p1:p1+80].split('\n')
	S2 = D2[p2:p2+80].split('\n')

	m1 = int(S1[0][5:].strip())
	m2 = int(S2[0][5:].strip())
	s1 = float(S1[1][9:].strip())
	s2 = float(S2[1][9:].strip())

	return (m2-m1)*86400.0+s2-s1
	
def summarize(input):
	s = input.split('/')
	data = open(input).readlines()
	ants = []
	nsec = 0
	tInt = 1.0
	trippel = ''
	for d in data:
		if d[:19] == 'EXECUTE TIME (SEC):':
			nsec = int(d[20:])
		if d[:14] == 'TELESCOPE NAME':
			ants.append(d[20:].strip())
		if d[:11] == 'DATA FORMAT':
			p = d[20:].find('-')
			if p > 0:
				trippel = d[p+21:].strip()
		if d[:14] == 'INT TIME (SEC)':
			tInt = float(d[20:])
			
	return [nsec, ants, trippel, tInt]

def gettimes(input):
	difx = input[0:-6]+'.difx'
	dir = glob(difx)
	if len(dir) == 0:
		return [-1, 0, 0]
	file = glob(difx+'/*')
	if len(file) == 0:
		return [-1, 0, 0]
	S = os.stat(dir[0])
	dtime = S[stat.ST_MTIME]
	S = os.stat(file[-1])
	ftime = S[stat.ST_MTIME]
	fsize = S[stat.ST_SIZE]
	elapsed = getelapsed(file[0], file[-1], 10000)
	return [float(ftime - dtime), fsize, float(elapsed)]
	
	
def run(path):
	inputs = glob(path+'/*.input')
	if len(inputs) == 0:
		return
	inputs.sort()
	n = len(inputs)
	SS = []
	AA = []
	su = []
	sectot = 0.0
	secdone = 0.0
	ttot = 0.0
	tdone = 0.0
	wtime = 0.0
	
	for i in range(n):
		S = summarize(inputs[i])
		A = gettimes(inputs[i])
		nsta = len(S[1])
		if A[2] > 0.0:
			A[2] += S[3]
			if A[2] > 0.9*S[0]:
				A[2] += (S[3]*1.5)
			if A[2] > S[0]:
				A[2] = S[0]
		if A[2] > 0.0 and A[0] > 0.0:
			speedup = A[2]/A[0]
			wtime += A[0]
		else:
			speedup = 0
		sectot += nsta*S[0]
		secdone += nsta*A[2]
		ttot += S[0]
		tdone += A[2]
	
		su.append(speedup)
		SS.append(S)
		AA.append(A)
	
	
	for i in range(n):
		A = AA[i]
		S = SS[i]
		speedup = su[i]
		nsta = len(S[1])
		fn = inputs[i].split('/')[-1][0:-6]
		fraction = 100*A[2]/S[0]
		sta = "%s  %4.1f min  %s  ns=%2d  su=%5.2f  %3d%%" % (fn, S[0]/60.0, S[2], nsta, speedup, fraction)
		if A[2] > 10 and fraction < 100:
			tremain = S[0]*(1.0 - fraction/100.0)/speedup
			sta += '  %4.1f min left' % (tremain/60.0)
		print(sta)
			
		
	fraction = int(100*secdone/sectot)
	print("Total job time      = %4.1f min" % (ttot/60.0))
	print("Fraction complete   = %3d%%" % fraction)
	jremain = ttot - tdone
	if tdone > 0:
		wremain = jremain*wtime/tdone
		print("Job time remaining  = %4.1f min" % (jremain/60.0))
		print("Wall time remaining = %4.1f min" % (wremain/60.0))
		print("Average speedup     = %4.2f" % (tdone/wtime))

if len(argv) == 1:
	paths = ['.']
else:
	paths = []
	for a in argv:
		if a == '-h' or a == '--help':
			usage()
		elif a[0] == '-':
			print('Unknown option %s .  Quitting' % a)
			exit(0)
		else:
			paths.append(a)

for p in paths:
	run(p)
