"""
Functions for classifying vex file statements, converting to SI units, and returning as the correct data type

TODO improve testing and do properly using unit tests
TODO change reblock so that it only finds blocks *outside* comments, strings and string_literals
TODO change recomment so that it only finds comments *outside* strings and string_literals
"""
import re
from math import pi
from time import strptime
from datetime import datetime, timedelta

from astro import dms2r, hms2r

# convert anything to SI baseunits
# these are all the valid units taken from the vex definition

units = {'psec': 1e-12,
         'nsec': 1e-9,
         'usec': 1e-6,
         'msec': 1e-3,
         'sec' : 1.,
         'min' : 60.,
         'hr'  : 3600.,
         'yr'  : 365.25 * 86400,
         'um'  : 1e-6,
         'mm'  : 1e-3,
         'cm'  : 1e-2,
         'm'   : 1.,
         'km'  : 1e3,
         'in'  : 0.0254,
         'ft'  : 0.3048,
         'mHz' : 1e-3,
         'Hz'  : 1.,
         'kHz' : 1e3,
         'MHz' : 1e6,
         'GHz' : 1e9,
         's'   : 1.,
         'ks'  : 1e3,
         'Ms'  : 1e6,
         'mdeg': 2e-3 * pi / 360.,
         'deg' : 2 * pi / 360.,
         'amin': 2 * pi / (360. * 60.),
         'asec': 2 * pi / (360. * 3600.),
         'rad' : 1.,
         'mJy' : 1e-3,
         'Jy'  : 1.,
         'bpi' : 1.,
         'kbpi': 1e3,
         'GB'  : 1e9}

#match within statement
recolon = re.compile(':(?=(?:[^"]*"[^"]*")*(?![^"]*"))')
refl = re.compile('^\s*([-+]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?)(?:(?:\s+)([A-Za-z]+))?(?:\/([A-Za-z]+))?\s*$')
rehms = re.compile(r"""^\s*(?:([+-])?([0-9]*)h([0-9]*)m([0-9]*\.?[0-9]+)s)\s*$""")
redms = re.compile(r"""^\s*(?:([+-])?([0-9]*)d([0-9]*)'([0-9]*\.?[0-9]+)")\s*$""")
#reepoch = re.compile('\s*^([0-9]*)y([0-9]*)d([0-9]*)h([0-9]*)m([0-9]*\.?[0-9]+)s$\s*')
reepoch = re.compile('([0-9]*)y([0-9]*)d([0-9]*)h([0-9]*)m(?:([0-9]+)(\.[0-9]+)?|[0-9]+)s')
requotestring = re.compile('^\s*"([^"]*)"\s*$')
restring = re.compile('^\s*(\S*)\s*$')
relink = re.compile('^\s*\&(.*)\s*$')

#match on line
reblock = re.compile('\$([A-Z]*)')
recomment = re.compile('\*') #this will fail on asterisks in strings but they shouldn't be there according to the vex definition


def floatcheck(string):
    """
    Check if string is real/int.

    >>> print floatcheck('1')
    1.0
    >>> print floatcheck('-871')
    -871.0
    >>> print floatcheck('-1.234 ')
    -1.234
    >>> print floatcheck('  +5.67e-12 ')
    5.67e-12
    >>> print floatcheck('-.987E+04')
    -9870.0
    >>> print floatcheck('1 m')
    1.0
    >>> print floatcheck('-1.234 psec')
    -1.234e-12
    >>> print floatcheck('+5.67e-12 mHz')
    5.67e-15
    >>> print floatcheck('-.987E+04 ks/sec')
    -9870000.0
    >>> print floatcheck('0.267e2 mJy')
    0.0267
    >>> print floatcheck('-871 m/sec')
    -871.0
    """
    m = refl.match(string)
    if m:
        g = m.groups()
        value = float(g[0])
        if g[1]:
            value *= units[g[1]]
        if g[2]:
            value /= units[g[2]]
        return value
    else:
        return None

def epochcheck(string):
    """
    Check if string is epoch.

    >>> print epochcheck('2007y346d06h16m00s')
    2007-12-12 06:16:00
    >>> print epochcheck('2008y1d03h12m05.0123456789s')
    2008-01-01 03:12:05.012346
    """
    m = reepoch.match(string)
    if m:
        g = m.groups()
        time = datetime(*strptime(' '.join(g[0:5]), "%Y %j %H %M %S")[0:6])
        if g[5]:
            time += timedelta(0, float(g[5]), 0)
        return time
    else:
        return None
    
def anglecheck(string):
    """
    Check if string is source coordinates ra or dec.

    >>> print anglecheck('05h23m2.56s')
    1.40953953944
    >>> print anglecheck('''-32d94'4.4989485495849584"''')
    -0.585870663771
    >>> print anglecheck('13h23m02.3892839s')
    3.50392222701
    """
    m = redms.match(string)
    n = rehms.match(string)
    if m:
        g = m.groups()
        d = int(g[1])
        m = int(g[2])
        s = float(g[3])
        if g[0] == '-':
            d, m, s = -d, -m, -s
        return dms2r(d, m, s)
    elif n:
        g = n.groups()
        h = int(g[1])
        m = int(g[2])
        s = float(g[3])
        if g[0] == '-':
            h, m, s = -h, -m, -s
        return hms2r(h, m, s)
    else:
        return None

def quotestringcheck(string):
    """
    Test if string is quote string. 
    """
    m = requotestring.match(string)
    if m:
        g = m.groups()
        return g[0]
    else:
        return None
   
def linkcheck(string):
    """
    Test string is link. 
    """
    # we could somehow make this recursively copy the string
    # but need to wait until we have classes for every
    # possible block
    m = relink.match(string)
    if m:
        g = m.groups()
        return g[0]
    else:
        return None

def stringcheck(string):
    """
    Test if string is string. 
    """
    m = restring.match(string)
    if m:
        g = m.groups()
        return g[0]
    else:
        return None

def classify(statement):
    """
    convert stripped statement to correct type (int, float datetime etc.)

    Real/Integer
    Character String (remove quotes if necessary
    Epoch - convert to datetime
    RA/DEC both converted to radians (float)

    units are reduced to the following:
    time s
    frequency Hz
    sample rate s / sec
    length m
    angle rad
    ang rate rad / s
    velocity m / s
    flux Jy
    bit density bpi
    
    statement -- a string containing the list of values
    returns the statement with the converted value as a list
    """
    statements = recolon.split(statement)
    flist = [quotestringcheck, linkcheck, epochcheck, anglecheck, floatcheck, stringcheck]
    rlist = []
    for s in statements:
        for f in flist:
            a = f(s)
            if not a == None:
                rlist.append(a)
                break
        else:
            raise RuntimeError, 'Faulty statement ' + s
    return rlist

def _test():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    _test()
