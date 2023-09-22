#!/bin/env python3

from sys import argv, exit

# Shamelessly extracted from a javascript converter at:
# http://www.csgnetwork.com/julianmodifdateconv.html

def mjd2day(mjd):
	jd = int(mjd+2400000.)
	l = jd + 68570
	n = 4*l//146097
	l = l - (146097*n + 3)//4
	y = 4000 * (l + 1) // 1461001
	l = l - (1461*y)//4 + 31
	m = 80*l//2447
	d = l - (2447*m)//80
	l = m//11
	m = m + 2 - 12*l
	y = 100*(n-49) + y + l
	return y, m, d

def d2time(d):
	d = d*24.0
	h = int(d)
	d = 60.0*(d - h)
	m = int(d)
	d = 60.0*(d - m)
	return h, m, d

if len(argv) < 0:
	print('Usage : %s <mjd>' % argv[0])
	exit(0)

for d in argv[1:]:
	mjd = float(d)
	if mjd > 200000.0:
		jd = mjd
		mjd -= 2400000.5
	else:
		jd = mjd + 2400000.5
	y, mo, d = mjd2day(int(mjd))
	h, mi, s = d2time(mjd - int(mjd))
	print('MJD %f = JD %f = %d/%d/%d %02d:%02d:%6.3f' % (mjd, jd, y, mo, d, h, mi, s))


