#!/usr/bin/python
#
# Script to compare PolConvert text products with a previous execution
#
# Py2/3 compatible as written we expect...
# PolConvert itself still requires a Py2 version of CASA (5.x)
#
'''
comparepolconvert.py -- a program to compare polconvert outputs
'''

from __future__ import absolute_import
from __future__ import print_function
import argparse
import math
import os
import re
import sys

def parseOptions():
    '''
    Parse the argument list to understand what files are to be compared
    and how it is to be done.  At present only ANTAB and FRINGE.PEAKS
    files are understood for this, but that may be sufficent.  A nonzero
    value on completion indicates some final pass/fail judgement.
    '''
    des = parseOptions.__doc__
    epi = '''
    For example:
      comparepolconvert.py -t testname -- *ANTAB FRINGE.PEAKS/*
    '''
    use = '$Id'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    parser.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work, really only for debugging')
    parser.add_argument('-c', '--create', dest='create',
        default=False, action='store_true',
        help='create missing files with appropriate names.  This is '
        'useful as a bootstrap in the creation of the test.')
    parser.add_argument('-q', '--quality', dest='qual',
        default='exact', metavar='METHOD',
        help='the METHOD for assessing agrement: for "approx", the '
            '-r and -a tolerances are respected; for "exact", the files '
            'should contain identical results (to the printed precision')
    parser.add_argument('-r', '--rtol', dest='rtol',
        type=float, default='1e-4', metavar='FLOAT',
        help='relative error tolerance (1e-4)')
    parser.add_argument('-a', '--atol', dest='atol',
        type=float, default='1e-4', metavar='FLOAT',
        help='absolute error tolerance (1e-4)')
    parser.add_argument('-t', '--name', dest='name',
        default='', metavar='STRING',
        help='the test name which allows the reference files to be '
        'matched to the test files of a similar name')
    # remaining arguments are the files to compare
    parser.add_argument('nargs', nargs='*',
        help='files to examine (ANTAB and PEAKS are implemented)')
    return parser.parse_args()

def remapArgs(verb, name, args, create):
    '''
    Process each "file" argument into a trio for comparison.
    A list of viable trios is returned.  For the PEAKS files,
    we need to also match the IF in the file name.  The named
    file in the list of args is to be matched with a comparison
    file with the test name + something appropriate.
    '''
    files = []
    antab_re = re.compile(r'.*ANTAB.*')
    peaks_re = re.compile(r'.*PEAKS.*')
    pksif_re = re.compile(r'.*PEAKS(\d+).*')
    for ar in args:
        if verb: print('what about ' + ar)
        cm = 'no-such-file'
        if antab_re.match(ar):
            if verb: print(ar + ' is an ANTAB file')
            cm = name + '.ANTAB'
            ty = 'antab'
        if peaks_re.match(ar):
            if verb: print(ar + ' is a PEAKS file')
            ifm = pksif_re.search(ar)
            ty = 'peaks'
            try: cm = name + ('.PEAKS%d'%int(ifm.group(1))) + '-ANT1.dat'
            except Exception as ex: cm = 'unable to identify IF: ' + str(ex)
        if os.path.exists(cm) and os.path.exists(ar):
            files.append((ar, cm, ty))
        else:
            if verb:
                if not os.path.exists(cm): print(cm + ' is missing')
                if not os.path.exists(ar): print(ar + ' is missing')
            if create and os.path.exists(ar):
                if verb: print('  creating ' + cm)
                os.system('cp -p %s %s' % (ar, cm))
    return files

def checkOptions(o):
    '''
    Check that the argument are sensible and extract from the
    argument list the files that we know how to assess.
    '''
    o.status = 0
    if o.atol < 0:
        print('the absolute tolerance (-a) must be non-negative')
        o.status += 1
    if o.rtol < 0:
        o.status += 1
        print('the relative tolerance (-r) must be non-negative')
    if o.name == '':
        o.status += 1
        print('a non-empty test name (-t ...) must be provided')
    if not (o.qual == 'exact' or o.qual == 'approx'):
        o.status += 1
        print('the test method must be "exact" or "approx"')
    o.files = remapArgs(o.verb, o.name, o.nargs, o.create)
    if len(o.files) == 0:
        o.status += 1
        print('no testable files were presented')
    return o

def initLines(verb, ths, ref, tf, rf):
    '''
    This function counts the number of lines (which should be the
    same for any of our cases) and initializes the counters.
    '''
    tlines = tf.readlines()
    rlines = rf.readlines()
    if (len(tlines) != len(rlines)):
        if verb: print("  %d lines in %s\n  %d lines in %s" % (
            len(tlines), ths, len(rlines), ref))
        linerr = 1
    else:
        linerr = 0
    return 0, 0, tlines, rlines, linerr

def exactCompare(verb, ths, ref, tf, rf):
    '''
    Return 0 if the files are identical, otherwise 1
    Comparison is made per line.
    '''
    sames,diffs,tlines,rlines,linerr = initLines(verb, ths, ref, tf, rf)
    for tl,rl in zip(tlines,rlines):
        if tl == rl: sames += 1
        else:        diffs += 1
    if verb: print("  exact (%d+%d)/%d == (same+diff)/total by lines" % (
        sames, diffs, sames+diffs))
    return diffs + linerr

def deviant(verb, what, tval, rval, atol, rtol):
    '''
    Consider the difference between "tval" and "rval" based on
    an absolute tolerance "atol" and a relative tolerance "rtol".
    Returns True (different) or False (about the same).
    '''
    if math.fabs(tval - rval) > atol:
        judgement,how = (True,'abs')
    elif tval + rval == 0.0:
        # prevent div by zero in next elif clause
        if tval == 0.0 or rval == 0.0:
            judgement,how = (False,'zer')
        else:
            judgement,how = (True,'inf')
    elif math.fabs(2 * (tval - rval) / (tval + rval)) > rtol:
        judgement,how = (True,'rel')
    else:
        judgement,how = (False,'all')
    if verb: print("%s: %f v %f -> %s (%s)" % (
        what, tval, rval, judgement, how))
    return judgement

def approxCompareAntab(verb, ths, ref, tf, rf, atol, rtol):
    '''
    Return 0 if the ANTABs are similar, otherwise 1
    Comments and the header must be the same; data lines maybe entirely
    identical as well.  If they are not, compare the numbers--lines
    look like this
        ^doy h:minutes ~32 values
    and whitespace splitting suffices
    '''
    sames,diffs,tlines,rlines,linerr = initLines(verb, ths, ref, tf, rf)
    for tl,rl in zip(tlines,rlines):
        if tl == rl: sames += 1
        elif re.match(rl, r'^[GPTI/]') != None: diffs += 1
        else:
            tls = tl.split()
            rls = rl.split()
            # day and time must match
            if len(tls) != len(rls): diffs += 1
            elif tls[0] != rls[0] or tls[1] != rls[1]: diffs += 1
            else:
                for tv,rv in zip(tls[2:],rls[2:]):
                    ill = deviant(verb, '  antab',
                        float(tv), float(rv), atol, rtol)
                    if ill: diffs += 1
                    else:   sames += 1
    if verb: print("  approx (%d+%d)/%d == (same+diff)/total by objects" % (
        sames, diffs, sames+diffs))
    return diffs + linerr

def approxComparePeaks(verb, ths, ref, tf, rf, atol, rtol):
    '''
    Return 0 if the PEAKSs are similar, otherwise 1
    Comments and the header must be the same; data lines maybe entirely
    identical as well.  If they are not, compare the numbers--lines
    look like:
        ^...{RR,LL,RL,LR}: value ; SNR: value
        ^...AMPLITUDE: value
        ^...Norm: value
        ^...RL: optimum... value
        ^...LR: optimum... value
    and whitespace splitting suffices.
    '''
    sames,diffs,tlines,rlines,linerr = initLines(verb, ths, ref, tf, rf)
    for tl,rl in zip(tlines,rlines):
        if tl == rl: sames += 1
        elif re.match(rl, r'^FOR') != None: diffs += 1
        else:
            tls = tl.split()
            rls = rl.split()
            # format of line must match, otherwise a list of indices to compare
            ix = []
            if (len(tls) != len(rls)): diffs += 1
            elif (tls[0] == 'FOR' and rls[0] == 'FOR' and
                  tls[2] != rls[2]): diffs += 1
            elif (tls[3] == 'SNR:' and rls[3] == 'SNR:'): ix = [1,4]
            elif (tls[0] == 'AMPLITUDE:' and rls[0] == 'AMPLITUDE:'): ix = [1]
            elif (tls[0] == 'RL/LR' and rls[0] == 'RL/LR'): ix = [2]
            elif (tls[2] == 'optimum' and rls[2] == 'optimum'): ix = [6]
            for xx in ix:
                ill = deviant(verb, '  pks['+str(xx)+']',
                    float(tls[xx]), float(rls[xx]), atol, rtol)
                if ill: diffs += 1
                else:   sames += 1
    if verb: print("  approx (%d+%d)/%d == (same+diff)/total by objects" % (
        sames, diffs, sames+diffs))
    return diffs + linerr

#
# enter here to do the work
#
if __name__ == '__main__':
    opts = parseOptions()
    nargs = len(opts.nargs)
    opts = checkOptions(opts)
    if opts.status != 0:
        print('have %d errors in arguments'%opts.status)
        sys.exit(opts.status)
    files = len(opts.files)
    if nargs != files: opts.status += 1
    print("\nOf %d test files, %d were paired to ref files (%d)\n" % (
        nargs, files, opts.status))
    for ths,ref,typ in opts.files:
        print('comparing:  ' + ths + '\n with ref:  ' + ref)
        tf = open(ths, 'r')
        rf = open(ref, 'r')
        if opts.qual == 'exact':
            status = exactCompare(
                opts.verb, ths, ref, tf, rf)
        elif typ == 'antab':
            status = approxCompareAntab(
                opts.verb, ths, ref, tf, rf, opts.atol, opts.rtol)
        else:
            status = approxComparePeaks(
                opts.verb, ths, ref, tf, rf, opts.atol, opts.rtol)
        tf.close()
        rf.close()
        opts.status += status
        print(" comparison status (%s) %d\n" % (opts.qual, status))
    print('Final status (%s) %d\n' % (opts.qual, opts.status))
    sys.exit(opts.status)

#
# eof
#
