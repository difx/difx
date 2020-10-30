#!/usr/bin/python3
# -*- coding: utf-8 -*-
'''
Reads an ASC delay polynomial file, or the delay polynomials found in a DiFX .im file.
Polynomials are converted into a numerical-only file easily evaluated in Matlab/Octave.

Usage:  raClosedLoop_convert.py <RA1_K.TXT | gs042_1000.im>

Output: <inputfilename.matlab>
'''

from raPatchClosedloop import PolySet
from datetime import datetime, timedelta
import sys

__author__ = 'Jan Wagner (MPIfR)'
__copyright__ = 'Copyright 2020, MPIfR'
__license__ = 'GNU GPL 3'
__version__ = '1.0.0'


def convertASCPolyFile(filename: str, sign: int = -1):

	dly = PolySet(filename, coeffscale=1e6)
	outfilename = filename + '.matlab'
	outfile = open(outfilename, 'w')

	ref_year = 1e9
	for poly in dly.piecewisePolys:
		ref_year = min(ref_year, poly.tstart.year)
	ref_time = datetime(ref_year, 1, 1, 0, 0)

	for poly in dly.piecewisePolys:
		sec_of_year_start = (poly.tstart - ref_time).total_seconds()
		sec_of_year_stop = (poly.tstop - ref_time).total_seconds()
		data = [sec_of_year_start, sec_of_year_stop]
		for coeff in poly.coeffs:
			data.append(sign*coeff[0]) # note: coeffs are {[dim0] x order} for time delay poly, and {[dim0,dim1,dim2] x order} for uvw coordinates poly
		datastr = ' '.join(map(str,data))
		outfile.write(datastr + '\n')

		#ref_year_mjd = (datetime(ref_year, 1, 1, 0, 0) - mjd0).days
		#poly_mjd = (poly.tstart - mjd0).days
		#print('Poly for MJD %d sec -- : sec %d of year %d (secs since mjd %d)' % (poly_mjd, sec_of_year_start, ref_year, ref_year_mjd))

	outfile.close()

	print ('Created  %s  with %d ASC time poly segments and with timestamps converted to second-of-year %d.' % (outfilename, len(dly.piecewisePolys), ref_year))


def convertDiFXPolyFile(filename: str, groundstations=['R1','GT']):

	mjd0 = datetime(1858, 11, 17, 0, 0)

	outfilename = filename + '.matlab'
	outfile = open(outfilename, 'w')
	ref_year = 1e9
	ref_year_mjd = 1e9
	antenna_id = -1

	Nwritten = 0
	curr_poly_mjd = -1
	curr_poly_sec = -1
	curr_poly_sec_of_year = -1
	poly_deltaT = 1
	dly_tag = ''

	with open(filename, 'r') as f:
		while True:
			line = f.readline()
			if not line:
				break
			line = line.strip()

			if 'START YEAR' in line:
				ref_year = int(line.split(':')[-1])
				ref_year_mjd = (datetime(ref_year, 1, 1, 0, 0) - mjd0).days
				continue

			if 'INTERVAL (SECS)' in line:
				poly_deltaT = int(line.split(':')[-1])
				continue

			if 'TELESCOPE' in line and 'NAME' in line:
				# example: "TELESCOPE 0 NAME:   EF"
				name = line[20:]
				if name in groundstations:
					antenna_id = int(line.split()[1])
					dly_tag = 'SRC 0 ANT %d DELAY (us)' % (antenna_id)
					print('Found ground station %s with difx id %d' % (name, antenna_id))
				continue

			if 'SCAN' in line and 'POLY' in line and 'MJD' in line:
				curr_poly_mjd = int(line.split(':')[-1])

			if 'SCAN' in line and 'POLY' in line and 'SEC' in line:
				curr_poly_sec = int(line.split(':')[-1])

			if len(dly_tag) > 0 and dly_tag in line:
				curr_poly_sec_of_year = (curr_poly_mjd - ref_year_mjd) * 24*60*60 + curr_poly_sec
				data = [curr_poly_sec_of_year, curr_poly_sec_of_year + poly_deltaT]
				data += [float(s) for s in line.split(':')[-1].split()]
				datastr = ' '.join(map(str,data))
				outfile.write(datastr + '\n')
				Nwritten += 1
				#print('Poly for MJD %d sec %d : sec %d of year %d (secs since mjd %d)' % (curr_poly_mjd, curr_poly_sec, curr_poly_sec_of_year, ref_year, ref_year_mjd))

	print ('Created  %s  with %d DiFX time poly segments and with timestamps converted to second-of-year %d.' % (outfilename, Nwritten, ref_year))


if __name__ == "__main__":

	if len(sys.argv) != 2:
		print (__doc__)
		sys.exit(1)

	filename = sys.argv[1]

	if '.TXT' in filename.upper():
		convertASCPolyFile(filename)
	elif '.im' in filename:
		convertDiFXPolyFile(filename)
	else:
		print (__doc__)
