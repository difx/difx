#!/usr/bin/python
#
# Script to ingest scan check stats and reduce it; presumes you did something
# like this:
#
# for v in ....vdif ; do
# scan_check -v -cloops=0 -cruns=0 -csdelta=1250 -cchans=31 $v > ${v/vdif/data}
# done
#
# and are processing on the 'data' output files.
#
'''
This script is designed to ingest scan check stats and reduce
it suitable for further processing (e.g. gnuplot).  Presumably
you did something like this:

  (for v in ....vdif ;
  do
    scan_check -v -cloops=0 -cruns=0 -csdelta=1250 -cchans=31 $v \
        > ${v/vdif/data} ;
  done )

and are applying this program to the data files to reduce them.
The -n/-x min/max arguments are to cull for an interesting period.
'''
from __future__ import absolute_import
from __future__ import print_function
import re
import os
import sys

def parseOptions():
    '''
    This script reduces scan check bit statistics output for postprocessing.
    '''
    (major, minor, micro, releaselevel, serial) = sys.version_info
    if minor > 6:
        import argparse
        des = parseOptions.__doc__
        epi = __doc__
        use = '%(prog)s [options] [input_file [...]]'
        parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
        parser.add_argument('-v', '--verbose', dest='verb',
            default=False, action='store_true',
            help='be chatty about the work')
        parser.add_argument('-n', '--min', dest='min', type=float,
            default=0.0, help='minimum time or 0.0 for all')
        parser.add_argument('-x', '--max', dest='max', type=float,
            default=86400.0, help='maximum time or 86400.0 for all')
        parser.add_argument('nargs', nargs='*',
            help='List of scan check output files to process')
        rv = parser.parse_args()
    else:
        import optparse
        parser = optparse.OptionParser()
        parser.add_option('-v', '--verbose', dest='verb',
            default=False, action='store_true',
            help='be chatty about the work')
        parser.add_option('-n', '--min', dest='min', type=float,
            default=0.0, help='minimum time or 0.0 for all')
        parser.add_option('-x', '--max', dest='max', type=float,
            default=86400.0, help='maximum time or 86400.0 for all')
        (rv, args) = parser.parse_args()
        rv.nargs = args
    return rv


def process(o):
    '''
    Parse the file and return a list of data
    '''
    print('# Processing',o.sc)
    # year 1, day 2, hour 3, min 4, sec 5, ns 6
    vex = r'([0-9]{4})y([0-9]{3})d([0-9]{2})h([0-9]{2})m([0-9]{2})s.([0-9]{9})'
    stamp_re = re.compile(r'[0-9]{5}:delta:ts ' + vex)
    pcs = r'\s+([0-9.]+)%\s+([0-9.]+)%\s+([0-9.]+)%\s+([0-9.]+)%'
    pcent_re = re.compile(r'[0-9]{5}:delta:' + pcs)
    time = -1.0
    high = -1.0
    loww = -1.0
    tilt = -1.0
    stats = []
    for line in o.si.readlines():
        stamp = stamp_re.search(line)
        if stamp:
            if (o.verb): print(stamp.group(4),stamp.group(5),stamp.group(6), end=' ')
            time = (float(stamp.group(3))*3600.0 +
                    float(stamp.group(4))*60.0 +
                    float(stamp.group(5))*1.0 +
                    float(stamp.group(6))*1e-9)
        pcent = pcent_re.search(line)
        if pcent:
            if (o.verb): print(float(pcent.group(2))+float(pcent.group(3)))
            high = float(pcent.group(2))+float(pcent.group(3))
            loww = float(pcent.group(1))+float(pcent.group(4))
            tilt = 100.0 * (
                (float(pcent.group(1))+float(pcent.group(2))) /
                (float(pcent.group(3))+float(pcent.group(4))) )
        if time >= 0.0 and high >= 0.0 and loww >= 0.0 and tilt >= 0.0:
            if (o.verb): print(time,high,loww)
            stats.append([time, high, loww, tilt])
            time = -1.0
            high = -1.0
            loww = -1.0
            tilt = -1.0
    return(stats)

def report(o, stats):
    '''
    Dump to file
    '''
    for data in stats:
        if data[0] >= o.min and data[0] <= o.max:
            o.sr.write("%.9f %.4f %.4f %.4f\n" % (
                data[0], data[1], data[2], data[3]))
    print('# Wrote',o.sc + '.data')

if __name__ == "__main__":
    o = parseOptions()
    for sc in o.nargs:
        if os.path.exists(sc) and not os.path.isdir(sc):
            o.sc = sc
            o.si = open(sc, 'r')
            stats = process(o)
            o.si.close()
            o.sr = open(sc + '.data', 'w')
            report(o, stats)
            o.sr.close()
        else:
            print('dude',sc,'is not a file...')

#
# eof
#
