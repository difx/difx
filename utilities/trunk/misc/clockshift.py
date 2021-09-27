#!/usr/bin/env python
'''
Script to shift clock_early to new epoch
'''
from __future__ import print_function
import argparse
import datetime
import os
import re

def parseOptions():
    '''
    Present human with a usable command line
    '''
    des = 'Program to shift $CLOCK section entries of vex.obs '
    des += 'to an new epoch.  The clock drifts are used to calculate '
    des += 'a new offset via:  clock_early = old + (dt) * drift.  The '
    des += 'units are asssumed to be sec and sec/sec if not specified. '
    use = '%(prog)s [options]\n'
    epi = ''
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    parser.add_argument('-i', '--input', dest='input',
        metavar='FILE', default='',
        help='Input vex.obs file to edit')
    parser.add_argument('-e', '--epoch', dest='epoch',
        metavar='VexTime', default='',
        help='Vex time (YYYYyDOYdHHhMMmSSs) of new epoch')
    parser.add_argument('-u', '--ounit', dest='ounit',
        metavar='Unit', default='usec',
        help='Clock early unit to output (usec or sec)')
    parser.add_argument('-o', '--output', dest='output',
        metavar='FILE', default='',
        help='Output vex.obs file to create')
    parser.add_argument('-k', '--clobber', dest='clobber',
        action='store_true', default=False,
        help='Remove output file if found')
    parser.add_argument('-v', '--verb', dest='verb',
        action='store_true', default=False,
        help='Provide some commentary')
    parser.add_argument('-w', '--very', dest='very',
        action='store_true', default=False,
        help='Provide detailed commentary')
    return parser.parse_args()

def grokVexTime(vt):
    '''
    Parse a vex time into something PyNative
    '''
    zdt = datetime.datetime.strptime(vt, '%Yy%jd%Hh%Mm%Ss')
    return zdt

def makeVexTime(dt):
    '''
    Convert a datetime into a vextime
    '''
    vt = dt.strftime('%Yy%jd%Hh%Mm%Ss')
    return vt

def updateLine(ln, ligne, epo, oun, mtch, verb, very):
    '''
    Update the clock early line to the new epoch
    For sanity's sake, convert offset to secs and end up usec
    '''
    if verb: print("@%d: %s" % (ln, ligne.rstrip()))
    cet = mtch.group(1)         # clock_early vextime
    cdt = grokVexTime(cet)      # clock_early datetime
    cof = float(mtch.group(2))  # clock_offset value
    cfu = mtch.group(3).rstrip()
    if cfu == 'usec':           # clock_offset unit: usec, sec, ''
        cof = cof * 1e-6        # clock_offset value now seconds
        cfu = 'sec'
    crt = mtch.group(4)         # clock_rate vextime
    rdt = grokVexTime(crt)      # clock_rate datetime
    rate = float(mtch.group(5)) # assume units of sec/sec
    delta = epo - cdt           # time shift
    deltas = delta.total_seconds()
    evt = makeVexTime(epo)      # should be o.epoch
    new = cof + deltas * rate   # new epoch
    if oun == 'usec':
        new = new * 1e6         # clock_offset not usec
    if very: print('  ', evt, epo, delta, deltas, ':\n  ',
        crt, rdt, rate, '\n  ',
        cet, cdt, cof, '"' + cfu + '"\n  ',
        evt, '0000-00-00 0' + str(datetime.timedelta(0)), new, '"' + oun + '"')
    nouveau = '  clock_early = %s : %.10g %s : %s : %+.8e; %s %g usec\n' % (
        evt, new, oun, evt, rate, '* adj by', deltas * rate * 1e6)
    if verb: print(" %d: %s" % (ln, nouveau.rstrip()))
    return nouveau

def doTheMathLuke(epo, oun, fin, fou, verb, very):
    '''
    Scan the lines of fin and when a clock early is found, convert to
    the requested epoch and write lines out to fou in any case.
    '''
    ln = 0
    inside = False
    vtp = '2[0-9]{3}y[0-9]{3}d[0-9]{2}h[0-9]{2}m[0-9]{2}s'
    rate = '|sec/sec'
    clk_re = re.compile(r'^[$]CLOCK;')
    new_re = re.compile(r'^[$]')
    cke_re = re.compile(r'\s*clock_early\s*=\s*(' + vtp + ')\s*:' +
        '\s*([-+0-9.e]+)\s*' + '(u?sec\s|\s)\s*:\s*(' + vtp + ')\s*:' +
        '\s*([-+0-9.e]+)\s*(' + rate + ')\s*;.*')
    for line in fin.readlines():
        ln = ln + 1
        lines = line.rstrip();
        if inside:
            ckhit = cke_re.search(lines)
            if ckhit:
                fou.write(' *' + line.lstrip())
                line = updateLine(ln, line, epo, oun, ckhit, verb, very)
                ckhit = None
            if new_re.search(lines):
                if very: print("%d: %s (%s)" % (ln, lines, str(inside)))
                inside = False
        if clk_re.search(lines):
            if very: print("%d: %s (%s)" % (ln, lines, str(inside)))
            inside = True
        fou.write(line)
    if verb: print('Wrote %d lines' % ln)

if __name__ == '__main__':
    o = parseOptions()
    o.edate = grokVexTime(o.epoch)
    if o.very: print("New epoch '%s' is %s" % (o.epoch, str(o.edate)))
    if os.path.exists(o.output):
        if not o.clobber:
            raise Exception("Output '%s' exists--move it aside" % o.output)
        else:
            os.unlink(o.output)
    if not os.path.exists(o.input):
        raise Exception("Input '%s' missing--provide it" % o.input)
    fi = open(o.input, 'r')
    fo = open(o.output, 'w')
    doTheMathLuke(o.edate, o.ounit, fi, fo, o.verb, o.very)
    fi.close()
    fo.close()

#
# eof
#
