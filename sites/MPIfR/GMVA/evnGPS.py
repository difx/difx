#!/usr/bin/env python3
'''
A script to load EVN station Maser-vs-GPS logs of one or
more months, and fit a maser drift rate.

Usage: evnGPS.py <station> <monthYear> [<monthYear> ...]

For example for a fit spanning three months:
$ evnGPS.py Ef mar22 apr22 jun22
'''

import datetime
import configparser, pathlib
import getpass
import requests
import ftplib
import sys

from scipy.optimize import curve_fit

__version__ = "1.0.0"
__author__="Jan Wagner <jwagner@mpifr.de>"
__build__= "$Revision$"
__date__ ="$Date$"

# EVN_GPS_ARCHIVE = "https://www.ira.inaf.it/vlbi_arch/gps/"	# superseded in 2023 by
EVN_GPS_ARCHIVE = "vlbeer.ira.inaf.it"							# new FTP-only server
USER_AUTH_FILE = '~/.vlbeer_ftp.conf'

MONTH_SUBDIRS = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']


def getCredentials(configfile: str = USER_AUTH_FILE):
	'''
	Load FTP user name and password (plain text...) from a config file
	'''

	config = configparser.ConfigParser()
	config.optionxform = lambda option: option	# Preserve case
	configfullpath = pathlib.PosixPath(configfile).expanduser()  # turn ~ into a full path, since Python ConfigParser doesn't cope with ~/<file>

	config.read(configfullpath)

	if 'ftp' in config.sections() and all(key in ['User','Password'] for (key,value) in config.items('ftp')):
		user = config['ftp'].get('User')
		passwd = config['ftp'].get('Password')
	else:
		print('Config file %s not found, or lacks section [ftp] with both User = ... and Password = ...' % (configfile))
		user = input("User (most likely evn):")
		passwd = getpass.getpass("Password for {:}: ".format(user))

	return user, passwd


def fetchGPSLogData(station, month_year):
	'''
	Download GPS log of station (2-letter ID) of a certain  month-year (e.g. oct21)
	'''

	## HTTPS, server no longer in use due to break-in (late 2022)
	# remotefile = EVN_GPS_ARCHIVE + month_year.lower() + "/gps." + station.lower()
	# remotedata = requests.get(remotefile, verify=True)
	# lines = [line.decode('ascii').strip() for line in remotedata.iter_lines()]
	# if remotedata.status_code != 200:
	#	print('Could not access %s - Error %d' % (remotefile,remotedata.status_code))
	#	return mjds,offsets,rmses

	## FTP, replacement server (2023 yet does not do HTTP(S)...)
	remotefile = 'gps/' + month_year.lower() + "/gps." + station.lower()

	ftp = ftplib.FTP(EVN_GPS_ARCHIVE)
	ftp.connect()
	ftpuser, ftppasswd = getCredentials()
	ftp.login(ftpuser, ftppasswd)

	lines = []
	ftp.retrlines('RETR ' + remotefile, lines.append)

	mjds, offsets, rmses = [], [], []

	for line in lines:

		if not line or line[0]=='#':
			continue

		elems = line.split()
		mjd,offset,rms = float(elems[0]),float(elems[1]),float(elems[2])
		mjds.append(mjd)
		offsets.append(offset)
		rmses.append(rms)

	print('Read %d data points from %s' % (len(mjds),remotefile))

	return mjds,offsets,rmses


def getGPSLogs(station, month_year_list):

	mjds, offsets, rmses = [], [], []

	for month_year in sys.argv[2:]:
		ep_mjds, ep_offsets, ep_rmses = fetchGPSLogData(station, month_year)
		mjds += ep_mjds
		offsets += ep_offsets
		rmses += ep_rmses

	print('In total read %d data points' % (len(mjds)))

	return mjds,offsets,rmses


def linearfunc(x, m, c):
	return m * x + c


def fitRate(mjds,offsets,rmses):
	'''
	Weighted least squares, linear slope.
	mjds : MJD fractional day of data point
	offsets : clock offsets in microseconds
	rmses : clock offset rms within data point
	'''

	#popt2, pcov2 = curve_fit(linearfunc, mjds, offsets, sigma=rmses, absolute_sigma=True)
	popt2, pcov2 = curve_fit(linearfunc, mjds, offsets, sigma=rmses)
	#popt2, pcov2 = curve_fit(linearfunc, mjds, offsets)

	#yhat = linearfunc(mjds, popt2[0], popt2[1])
	#print('ydata', offsets)
	#print('yhat', yhat)

	rate = popt2[0]*1e-6 / (24*60*60)

	mjdref = 59671.7917  ## TODO: make an optional arg, e.g. in VEX timestamp format, for clock_early ref epoch
	starting_offset = linearfunc(mjdref, popt2[0], popt2[1])
	print('At MJD %.3f offset %.3f usec, drift %.4e sec/sec' % (mjdref,starting_offset,rate))

	return rate


if __name__ == "__main__":

	if len(sys.argv) < 2:
		print(__doc__)
		sys.exit(-1)

	station = sys.argv[1].lower()

	if len(station) != 2:
		print('Error: Station must be a 2-letter code, like Ef')
		sys.exit(-1)

	if True:
		mjds, offsets, rmses = getGPSLogs(station, sys.argv[2:])
	else:
		# test data
		mjds = [59670.291, 59671.291, 59672.291, 59673.291, 59674.291, 59675.291, 59676.291, 59677.291, 59678.291, 59679.291, 59680.291, 59681.291, 59682.291, 59683.291, 59684.291, 59685.291, 59686.291, 59687.291, 59688.291, 59689.291, 59690.291, 59691.291, 59692.291, 59693.291, 59694.291, 59695.291, 59696.291, 59697.291, 59698.291, 59699.291]
		offsets = [27.02, 27.0, 27.0, 26.97, 26.99, 26.97, 26.95, 26.95, 26.92, 26.92, 26.91, 26.9, 26.89, 26.87, 26.87, 26.85, 26.84, 26.84, 26.82, 26.83, 26.78, 26.78, 26.83, 26.76, 26.76, 26.75, 26.75, 26.73, 26.72, 26.71]
		rmses = [0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005, 0.005]

	fitRate(mjds,offsets,rmses)
