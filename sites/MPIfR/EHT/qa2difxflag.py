#!/usr/bin/python3
"""
usage: qa2difxflag.py [-h] [-a ANTENNA] [-e EXPT] [-s SPW] [-V]
                      [flagfiles [flagfiles ...]]

Convert the ALMA APP QA2 deliverable .flg file with time-based data flags
into a flag file that is readable by difx2fits and gets propagated into
the FITS-IDI FG#1 table during difx2fits conversion.

positional arguments:
  flagfiles             Input ALMA APP QA2 flag file(s); track-label.flg

optional arguments:
  -h, --help            show this help message and exit
  -a ANTENNA, --antenna ANTENNA
                        Antenna name to put in DiFX flag file (default: AA)
  -e EXPT, --expt EXPT  Experiment name e.g. e18c21 (default: expt)
  -s SPW, --spw SPW     If >=0, use only flags of specific SPW (default: -1)
  -V, --version         show program's version number and exit

Note: The ALMA flag file is provided as part of ALMA APP QA2 deliverable
tarball and is found under folder *.qa2-forPI/
"""

__author__ = 'Jan Wagner (MPIfR)'
__copyright__ = 'Copyright 2021, MPIfR'
__license__ = 'GNU GPL 3'
__version__ = '1.0.0'


import argparse
import math
import re
import sys
from datetime import datetime, timedelta, timezone


def qa2DifxFlagfile(qa2file: str, difxflag: str, antennaName: str, spw: int = -1) -> bool:
	"""
	Convert ALMA APP QA2 deliverable .flg into DiFX '<expt>.<antenna>.flag' file
	"""
	mjd0 = datetime(1858, 11, 17, 0, 0)

	### Syntax reference
	#
	# ALMA APP QA2 deliverable "<label>.qa2-forPI/<label>.flg" file syntax:
	#   mode='manual' timerange='2018/04/20/23:03:22.18~2018/04/20/23:03:40.31' spw='0' reason='Unphased data'
	#   mode='manual' timerange='2018/04/20/23:10:26.98~2018/04/20/23:10:45.11' spw='0' reason='Unphased data'
	#
	# Difx2fits <expt>.<antenna>.flag syntax:
	#   <antenna name> <mjd start> <mjd stop> <rec band> <"flagging reason">
	#   <antenna name> <mjd start> <mjd stop> <rec band> <"flagging reason">
	#   ...
	#   where rec band of -1 means all rec bands should be flagged. The flagging
	#   propagates to FITS-IDI IFs, existing logic in difx2fits takes care of it.

	Nconverted = 0
	splitpattern = re.compile(r"['=~]+")
	recBand = -1

	with open(difxflag, 'w') as fo:
		with open(qa2file, 'r') as fi:

			for line in fi.readlines():

				line = line.strip()
				if len(line) < 1:
					print('EOF?')
					continue
				items = splitpattern.split(line)
				#print(items)

				# Ignore unrelated "spectral windows", if explicitly specified
				if spw >= 0 and int(items[-4]) != spw:
					continue

				Tstart = datetime.strptime(items[3], '%Y/%m/%d/%H:%M:%S.%f')
				Tstop = datetime.strptime(items[4], '%Y/%m/%d/%H:%M:%S.%f')
				reason = items[-2]

				# Convert from UTC times to fractional-day MJDs (e.g., 58229.152396)
				mjdStart = (Tstart - mjd0).total_seconds() / timedelta(days=1).total_seconds()
				mjdStop = (Tstop - mjd0).total_seconds() / timedelta(days=1).total_seconds()

				fo.write('%s %.6f %.6f %d "%s"\n' % (antennaName, mjdStart, mjdStop, recBand, reason))
				Nconverted += 1

	success = Nconverted > 0
	return success


if __name__ == "__main__":

	parser = argparse.ArgumentParser(add_help=True,
		description='Convert ALMA APP QA2 deliverable .flg file into a flag file that is readable by difx2fits.',
		epilog='Note: The ALMA flag file is provided as part of ALMA APP QA2 deliverable tarball and is found under folder *.qa2-forPI/')
	parser.add_argument('-a', '--antenna', default='AA', help='Antenna name to put in DiFX flag file (default: %(default)s)') 
	parser.add_argument('-e', '--expt', default='expt', help='Experiment name e.g. e18c21 (default: %(default)s)')
	parser.add_argument('-s', '--spw', default=-1, help='If >=0, use only flags of this SPW (default: %(default)d)') 
	parser.add_argument('-V', '--version', action='version', version='%(prog)s {version}'.format(version=__version__))
	parser.add_argument('flagfiles', nargs='*', help='Input ALMA APP QA2 flag file(s); track-label.flg')

	args = parser.parse_args(sys.argv[1:])
	if len(args.flagfiles) < 1:
		print('No QA2 flag files given, see --help for usage')

	for flagfile in args.flagfiles:
		difxflagname = '%s.%s.flag' % (args.expt, args.antenna)
		ok = qa2DifxFlagfile(flagfile, difxflagname, args.antenna, args.spw)
		if not ok:
			print('Failed to convert %s' % (flagfile))

