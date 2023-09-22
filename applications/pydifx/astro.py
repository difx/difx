"""
Utility functions for basic coordinate transformations etc.

Executing the script runs the tests:

python astro.py -v
"""
from datetime import date, datetime, timedelta
from math import pi

def _pos(l):
    """
Work out if list is +ve or -ve.

If first non-zero value in list is +ve return True.
If first non-zero value in list is -ve return False.
If entire list is 0 return True.

>>> print _pos([1])
True
>>> print _pos([-0.1])
False
>>> print _pos([0, 0.0, 5])
True
>>> print _pos((0.0, 0, 0, -7e-34))
False
>>> print _pos([0, 0, 0, 0])
True
    """
    for a in l:
        if a < 0: 
            return False
    return True

def _negate(l):
    """
take a list and change the first non-zero member to
a negative number

>>> print _negate([3, 4, 2])
[-3, 4, 2]
>>> print _negate([0, 0, 0])
[0, 0, 0]
    """
    for i in range(len(l)):
        if not l[i] == 0:
            l[i] = -l[i]
            break
    return l

def hms2df(h, m, s):
    """
Convert h,m,s to day fraction.

>>> print hms2df(6, 20, 20)
0.26412037037

Note that hours minutes and seconds can be any number:

>>> print hms2df(12 + 15, 2 + 59, 3 + 20 + 200)
1.16994212963

    """
    return float(h)/24 + float(m)/1440 + float(s)/86400

def df2hms(df):
    """
Convert day fraction to hours, minutes, and float seconds

>>> print df2hms(1.25)
(6, 0, 0.0)

>>> print df2hms(-40.25)
(18, 0, 0.0)
    """
    # Todo
    # change to always use built in time?

    d, h = divmod(df, 1)
    h, m = divmod(24 * h, 1)
    m, s = divmod(60 * m, 1)
    s = s * 60
    return int(h), int(m), float(s)

def df2hhms(df):
    """
Convert day fraction to hours, minutes, and float seconds

>>> print df2hhms(0.25)
(6, 0, 0.0)

>>> print df2hhms(40.25)
(966, 0, 0.0)
    """

    d, h = divmod(df, 1)
    h, m = divmod(24 * h, 1)
    m, s = divmod(60 * m, 1)
    s = s * 60
    h += d * 24
    return int(h), int(m), float(s)

    
def hms2r(h, m, s):
    """
Convert from hours, minutes, second to radians.

If the left-most non zero value is -ve, all of the following values are 
assumed to be -ve

>>> print hms2r( 3,  5,  20)
0.808669220091
>>> print hms2r( 5, 17,  20)
1.38462787325
>>> print hms2r(-3,  5,  20)
-0.808669220091
>>> print hms2r( 0, -5,  20)
-0.0232710566933
>>> print hms2r( 0,  0, -20)
-0.00145444104333
>>> print hms2r(33,  7,  17)
8.67115933417
>>> print hms2r( 9, 95,  20)
2.77216462858
>>> print hms2r( 1,  5,  71)
0.288779269153
>>> print hms2r(23, 59,  59)
6.28311258513
>>> print hms2r(23, 59,  61)
6.28325802923
    """
    if _pos((h, m, s)):
        return 2 * pi * (float(h) + m/60. + s/3600.) / 24.
    else:
        return 2 * pi * (float(-abs(h)) +\
                         float(-abs(m))/60 +\
                         float(-abs(s))/3600) / 24.


def dms2r(d, m, s):
    """
    Convert from degrees, minutes, second to radians.

    if the left-most non zero value is -ve, all of the following values are assumed to be -ve
>>> print dms2r(253,  5, 20)
4.41723441133
>>> print dms2r(-23,  5, 20)
-0.402977131738
>>> print dms2r(  0, -5, 20)
-0.00155140377955
>>> print dms2r(  0,  0,-20)
-9.69627362219e-05
>>> print dms2r(733,  7, 17)
12.7953820529
>>> print dms2r( 19, 95, 20)
0.359343900438
>>> print dms2r(  1,  5, 71)
0.0192519512769
>>> print dms2r(359, 59, 59)
6.28318045904
>>> print dms2r(359, 59, 61)
6.28319015532
    """
    return hms2r(d, m, s) / 15

def r2hms(r):
    """
Convert from radians to degrees, minutes and seconds.

Should always result in something between 0 and 24

>>> print r2hms(0)
(0, 0, 0.0)
>>> print r2hms(pi / 4)
(3, 0, 0.0)
>>> print r2hms(pi / 2)
(6, 0, 0.0)
>>> print r2hms(pi)
(12, 0, 0.0)
>>> print r2hms(2 * pi)
(0, 0, 0.0)
>>> print r2hms(3 * pi)
(12, 0, 0.0)
>>> print r2hms(-pi / 4)
(21, 0, 0.0)
>>> print r2hms(-pi / 2)
(18, 0, 0.0)
>>> print r2hms(-pi)
(12, 0, 0.0)
>>> print r2hms(5 * pi)
(12, 0, 0.0)
    """
    r /= 2 * pi
    h, m, s = df2hms(r)
    return h, m, s

def r2dms(r):
    """
Convert from radians to hours, minutes, seconds.

should always result in something between -90 and + 90
>>> print r2dms(0)
(0, 0, 0.0)
>>> print r2dms(pi / 8)
(22, 30, 0.0)
>>> print r2dms(pi / 4)
(45, 0, 0.0)
>>> print r2dms(pi / 2)
(90, 0, 0.0)
>>> print r2dms(-pi / 8)
(-22, 30, 0.0)
>>> print r2dms(-pi / 4)
(-45, 0, 0.0)
>>> print r2dms(-pi / 2)
(-90, 0, 0.0)
>>> print r2dms(pi)
Traceback (most recent call last):
    ...
ValueError: r must be in range -pi/2 to pi/2
>>> print r2dms(-2 * pi)
Traceback (most recent call last):
    ...
ValueError: r must be in range -pi/2 to pi/2
    """
    if not -pi/2 <= r <= pi/2:
        raise ValueError, "r must be in range -pi/2 to pi/2"
    if r < 0:
        r = abs(r)
        neg = True
    else:
        neg = False
    r /= 2 * pi
    d, h = divmod(r, 1)
    h, m = divmod(360 * h, 1)
    m, s = divmod(60 * m, 1)
    s = s * 60
    if h > 180:
        h -= 360
    hms = [int(h), int(m), float(s)]
    if neg:
        hms = _negate(hms)
    return tuple(hms)


def ymd2mjd(y, m, d):
    """
Return a julian day number from a gregorian y, m, d.

>>> print ymd2mjd(2010, 12, 11)
55541
>>> print ymd2mjd(2010, 1, 1)
55197
>>> print ymd2mjd(2010, 12, 31)
55561
>>> print ymd2mjd(2000, 2, 29)
51603
>>> print ymd2mjd(2000, 3, 1)
51604
>>> print ymd2mjd(1987, 2, 28)
46854
>>> print ymd2mjd(1987, 3, 1)
46855
>>> print ymd2mjd(1957, 10, 4)
36115
    """
    return date(y, m, d).toordinal() - 678576

def mjd2ymd(mjd):
    """
Return a gregorian y, m, d from a julian day number.

>>> print mjd2ymd(55541)
(2010, 12, 11)
>>> print mjd2ymd(55197)
(2010, 1, 1)
>>> print mjd2ymd(55561)
(2010, 12, 31)
>>> print mjd2ymd(51603)
(2000, 2, 29)
>>> print mjd2ymd(51604)
(2000, 3, 1)
>>> print mjd2ymd(46854)
(1987, 2, 28)
>>> print mjd2ymd(46855)
(1987, 3, 1)
>>> print mjd2ymd(36115)
(1957, 10, 4)
    """
    d = date.fromordinal(mjd + 678576)
    return d.year, d.month, d.day

def datetime2mjd(dt):
    """
Return Modified Julian Day (MJD).

>>> print datetime2mjd(datetime(2010, 12, 11, 0, 0))
55541.0
    """
    mjd = ymd2mjd(dt.year, dt.month, dt.day)
    mjd += hms2df(dt.hour, dt.minute, dt.second + (dt.microsecond / 1000000.))
    return mjd


def mjd2datetime(mjd):
    """
Return astrotime object from a Modified Julian Date (JD).
Parameters:
    jdf        : Julian Day number
>>> mjd2datetime(55541)
datetime.datetime(2010, 12, 11, 0, 0)
>>> mjd2datetime(55197)
datetime.datetime(2010, 1, 1, 0, 0)
>>> mjd2datetime(55561).isoformat()
'2010-12-31T00:00:00'
>>> mjd2datetime(51603.999999999)
datetime.datetime(2000, 2, 29, 23, 59, 59, 999914)
>>> mjd2datetime(51604.9999999999999)
datetime.datetime(2000, 3, 2, 0, 0)
>>> mjd2datetime(55561.473847384783).isoformat()
'2010-12-31T11:22:20.414045'
    """
    intmjd, df = divmod(mjd, 1)
    yr, mo, day = mjd2ymd(int(intmjd))
    h, m, s = df2hms(df)
    s, ms = divmod(s * 1000000, 1000000)
    ms, msf = divmod(ms, 1)
    dt = datetime(yr, mo, day, h, m, int(s), int(ms))
    if msf > 0.5:
        dt += timedelta(0, 0, 1)
    return dt

def mjd2jd(mjd):
    """
    Convert mjd to jd.

    >>> mjd2jd(54321.0)
    2454321.5
    >>> mjd2jd(44443.5)
    2444444.0
    """
    return mjd + 2400000.5

def jd2mjd(jd):
    """
    Convert jd to mjd.

    >>> jd2mjd(2454321.5)
    54321.0
    >>> jd2mjd(2444444)
    44443.5
    """
    return jd - 2400000.5

def res(jd):
    """
    work out error in microseconds given a jd/mjd etc.
    """
    sy = 86400 * 365.25
    error = jd - (jd - (1/sy))
    error -= (1/sy)
    error = abs(error) * sy * 1000000
    return error

def test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    test()
