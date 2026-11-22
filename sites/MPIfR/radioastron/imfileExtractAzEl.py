#!/usr/bin/python3
# -*- coding: utf-8 -*-
'''
Reads a DiFX .im file and extracts the azimuth and elevation polynomials for the reference station.
Polynomials are converted into a numerical-only file easily evaluated in Matlab/Octave.

Usage:  imfileExtractAZ.py <gs042_1000.im>

Output: <inputfilename.az.matlab>
        <inputfilename.el.matlab>

'''

from datetime import datetime, timedelta
import sys

__author__ = 'Jan Wagner (MPIfR)'
__copyright__ = 'Copyright 2026, MPIfR'
__license__ = 'GNU GPL 3'
__version__ = '1.0.0'

def convertDiFXPolyFile(filename: str, groundstations=['R1','GT','NN'], keyword='AZ'):

	mjd0 = datetime(1858, 11, 17, 0, 0)

	outfilename = filename + '.%s.matlab' % (keyword[0:2].lower()) # .az.matlab, .el.matlab
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
					dly_tag = 'SRC 0 ANT %d %s' % (antenna_id, keyword)
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

	if '.im' in filename:
		convertDiFXPolyFile(filename, keyword="AZ")
		convertDiFXPolyFile(filename, keyword="EL GEOM")
	else:
		print (__doc__)
