# -*- coding: utf-8 -*-

from string import split
import datetime

def mjd2vexTime (mjd, dateonly=False):
	'''
	Converts a Mean Julian Date (MJD) into a vex compatible datetime
	If the parameter dateonly is set to True only the date portion will be returned
	If dateonly is set to False (default) the full datetime string will be returned
	'''

	
	if mjd < 50001 or mjd > 99999:
		return ''

	mjd0 = datetime.datetime(1858, 11, 17, 0, 0)
	md = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]

	d = int(mjd)
	s = int((mjd - d)*86400.0 + 0.5)
	dt = datetime.timedelta(d, s)
	t = mjd0 + dt
	d = t.day + md[t.month-1]
	if t.year % 4 == 0 and t.month > 2:
		d += 1
	if dateonly:
		return '%dy%03dd' % \
			(t.year, d)
	else:
		return '%dy%03dd%02dh%02dm%02ds' % \
			(t.year, d, t.hour, t.minute, t.second)

def splitObservationCode(obsCode):
	"""
	Splits a combined observation code (e.g. em077a) in its  code (em077) and segment (a) portions
	"""

	obsSeg = ''
	proposal = obsCode[:]

	if len(proposal) > 3:
		if proposal[0].isalpha() and proposal[1].isalpha() and proposal[2].isdigit():
			for i in range(3, len(proposal)):
				if proposal[i].isalpha():
					obsSeg = proposal[i:]
					proposal = proposal[0:i]
					break
		if proposal[0].isalpha() and proposal[1].isdigit():
			for i in range(2, len(proposal)):
				if proposal[i].isalpha():
					obsSeg = proposal[i:]
					proposal = proposal[0:i]
					break

	return proposal, obsSeg


def makeObservationCode(proposal, segment):
	"""
	Builds an observation code (e.g. em077a) from its experiment code (em077) and the segment (a)
	"""
	if segment == None:
		return proposal
	else:
		return proposal+segment

def parseKeyValue(str):
	"""
	Parses a string to find key=value pairs
	"""
	kv = {}
	ss = split(str)
	for s in ss:
		p = split(s, '=')
		if len(p) != 2:
			# TODO throw exception instead
			print 'Error parsing key=value statement: %s', s
			return {}
		kv[p[0]] = p[1]
	return kv