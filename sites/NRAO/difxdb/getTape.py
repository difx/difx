#!/bin/env python

import datetime
import cx_Oracle
from sys import argv, exit
from os import getenv
from string import upper,lower

databaseName = getenv("VLBA_DB")

mjd0 = datetime.datetime(1858, 11, 17, 0, 0)

def mjd2vex(mjd):
	md = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
	d = int(mjd)
	s = int((mjd - d)*86400.0 + 0.5)
	dt = datetime.timedelta(d, s)
	t = mjd0 + dt
	d = t.day + md[t.month-1]
	if t.year % 4 == 0 and t.month > 2:
		d += 1
	return '%dy%03dd%02dh%02dm%02ds' % (t.year, d, t.hour, t.minute, t.second)

def Ulow(s):
	return upper(s[0]) + lower(s[1:])

def calcMJD(t):
	dt = t - mjd0
	mjd = dt.days + dt.seconds/86400.0
	return mjd

def oh2zero(x):
	return x.replace('O', '0')

def getShelf(shelves, vsn):
	for s in shelves:
		if oh2zero(s[0]) == oh2zero(vsn):
			return s[1]
	return 'none'

def getShelvesFromDB(db):
	cursor = db.cursor()
	cursor.execute("select * from SHELVES")
	shelves = cursor.fetchall()
	return shelves

def getTapesFromDB(db, prop, seg):
	cursor = db.cursor()
	if len(seg) > 0:
		query = "select * from TAPE where PROPOSAL='%s' and SEGMENT='%s'" % (prop, seg)
	else:
		query = "select * from TAPE where PROPOSAL='%s'" % prop
	cursor.execute(query)
	tapes = cursor.fetchall()
	return tapes

def getVSN(vsns, stn, u, t):
	vsn = 'none'
	if not stn in vsns:
		return vsn
	for v in vsns[stn]:
		if t + 0.5/86400.0 >= v[0] and u == v[2]:
			vsn = v[1]
	return vsn

def getTapeMotionFromDB(db, prop, seg):
	cursor = db.cursor()
	collect = "STAID,TAPESPEED1,TAPESPEED2,START_TIME,STOP_TIME"
	if len(seg) > 0:
		query = "select %s from OBSSTA where PROPOSAL='%s' and SEGMENT='%s' order by START_TIME" % (collect, prop, seg)
	else:
		query = "select %s from OBSSTA where PROPOSAL='%s' order by START_TIME" % (collect, prop)
	cursor.execute(query)
	motions = cursor.fetchall()
	return motions

def getClockFromDB(db, stations, start, stop):
	gps_offsets = {}
	clocks = {}

	for stn in stations:
		cursor = db.cursor()
		query = "select VERSION,GPS_EPOCH,GPS_OFFSET from STATIONS where STN_ID='%s' order by VERSION,GPS_EPOCH" % stn
		cursor.execute(query)
		gps_offsets[stn] = cursor.fetchall()

		cursor = db.cursor()
		query = "select * from CLOCKS where STAID='%s' order by EPOCH,VERSION" % stn
		cursor.execute(query)
		clocks[stn] = cursor.fetchall()

	for stn in stations:
		gps_offset = gps_offsets[stn][-1]
		print gps_offset

	return clocks
"""
Return something that can populate:

$CLOCK;
def AR;
  clock_early = 1995y263d12h00m : 1.5 usec;
  clock_early = 1996y263d12h00m : 1.6 usec;
  clock_early = 1997y263d12h00m : 1.7 usec;
  clock_early = 1999y263d12h00m : 1.7 usec;
enddef;
def GB;
  clock_early = : 1.5 usec;
  clock_early = 1996y263d12h00m : 1.6 usec : 2000y1d00h00m : 1.2e-12;
enddef;
"""	

def makeVSNDict(tapes):
	td = {}
	for t in tapes:
		stn = t[2]
		mjd = calcMJD(t[3])
		row = [mjd, t[4], t[5]-4]
		if not stn in td:
			td[stn] = []
		td[stn].append(row)
	return td

def calcVSNList(motions, vsns, stn):
	r = []
	for m in motions:
		if m[0] == stn and (m[1] != 0 or m[2] != 0):
			if m[1] != 0:
				u = 0
			else:
				u = 1
			t1 = calcMJD(m[3])
			t2 = calcMJD(m[4])
			r.append([u, t1, t2, getVSN(vsns, stn, u, t1)])
	return r;

def genBlocks(scans):
	blocks = []
	curBlock = [0.0, 0.0, 'none', -1]
	for s in scans:
		if s[3] == curBlock[2]:
			if s[2] > curBlock[1]:
				curBlock[1] = s[2]
			else:
				print 'Warning: not time ordered!', s[3], curBlock[1], s[2]
		else:
			curBlock = [s[1], s[2], s[3], s[0]]
			blocks.append(curBlock);
	return blocks

if databaseName == None:
	print 'Please set env. var. VLBA_DB to point to the VLBA database'
	exit(0)

if len(argv) < 2:
	print 'Need proposal code'
	exit(0)

if len(argv) > 2:
	seg = argv[2]
else:
	seg = ''

db = cx_Oracle.connect(databaseName)

shelves = getShelvesFromDB(db)
tapes = getTapesFromDB(db, argv[1], seg)
motions = getTapeMotionFromDB(db, argv[1], seg)
vsns = makeVSNDict(tapes)
stations = vsns.keys()
stations.sort()
clocks = getClockFromDB(db, stations, 50000.0, 60000.0)

print '$TAPELOG_OBS;'
for stn in stations:
	print 'dev %s;' % Ulow(stn)
	scans = calcVSNList(motions, vsns, stn)
	blocks = genBlocks(scans)
	for b in blocks:
		b.append(getShelf(shelves, b[2]))
		print '  VSN = %d : %s : %s : %s;  * shelf = %s' % \
			((b[3]+1), b[2], mjd2vex(b[0]), mjd2vex(b[1]), b[4])
	print 'enddef;'

