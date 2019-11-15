#!/usr/bin/env python3
#
'''
Script to generate calc im files for use with the ALMA case.
The operative theory is that the -A model is correct for ALMA
but that the normal processing is correct for the others.
'''
#

import argparse
import glob
import os
import re
import sys

def parseOptions():
    des = '''
    This script is intended as a replacement for calcif2 or difxcalc
    as invoked via the DIFX_CALC_PROGRAM environment variable in startdifx.
    It will turn adjust atmospheric corrections for stations in the
    noatmos list and use the default atmosphere for the others.
    The resulting .im file will be labelled as a MIXED correction.
    You can specify the calc files to process with either the -j
    argument (job name or job.calc) or as files listed on the commandline.
    Defaults are given in parentheses.
    '''
    epi = '''
    Assuming this is in the path, "export DIFX_CALC_PROGRAM=calcifMixed"
    and "export DIFX_CALC_OPTIONS=-v" would be sufficient to get the normal
    startdifx to use this machinery on ALMA data (Aa).  The -d and -w options
    adjust the correction factor for the dry and wet components.  Note that
    there are no detailed corrections for U,V and W.
    '''
    use = '%(prog)s [options]\n'
    use += '  Version $Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    parser.add_argument('-a', '--noatmos', dest='noatmos',
        metavar='COMMASEPLIST', default='Aa',
        help='List of stations (Aa) which should have no atmospheric delay')
    parser.add_argument('-d', '--dry', dest='dry',
        metavar='COMMASEPLIST', default='1.0',
        help='Dry component adjustment factor list, one per station (1.0)')
    parser.add_argument('-w', '--wet', dest='wet',
        metavar='COMMASEPLIST', default='1.0',
        help='Wet component adjustment factor list, one per station (1.0)')
    parser.add_argument('-c', '--calc', dest='calc',
        metavar='STRING', default='calcif2',
        help='Name of the calc executable (calcif2)')
    parser.add_argument('-o', '--options', dest='options',
        metavar='STRING', default='-v --override-version',
        help='List of the options to pass to calc program ' +
             'specified by -c (-v --override-version)')
    parser.add_argument('-j', '--job', dest='job',
        metavar='STRING', default='',
        help='Job name (use $job.calc) or empty for all .calc files')
    parser.add_argument('-v', '--verb', dest='verb',
        action="store_true", default=False, help='be chatty in our work')
    parser.add_argument('-O', '--override-version',
        action="store_true", default=False, help='ignored, present for compatibility only')
    parser.add_argument("nargs", nargs='*')
    return parser.parse_args()

def genPatterns(verb, stations, normallines):
    '''
    Look for TELESCOPE lines that match the stations
    and return a list of compiled search patterns.
    Note we need to preserve the order of stations.
    '''
    if verb: print('No atmosphere on these:')
    patt = re.compile(r'TELESCOPE\s+(\d+)\s+NAME:\s*(\w\w)')
    patterns = [r'MATCHES-NOTHING' for x in range(len(stations))]
    for line in normallines:
        tele = patt.search(line)
        if tele != None:
            for ii in range(len(stations)):
                if tele.group(2) == stations[ii]:
                    patterns[ii] = re.compile(
                        r'SRC\s+\d+\s+ANT\s+' + tele.group(1))
                    if verb: print('',line.rstrip(),\
                        'matches SRC\s+\d+\s+ANT\s+' + tele.group(1) + \
                        'at index ' + str(ii))
    return patterns

def writeDelayDryWet(save, dryfac, wetfac, f):
    '''
    The dict save contains delay, dry and wet components of the
    polynominal coefficients from the 3 lines.  Each line looks
    like:   SRC n ANT m what (us): poly coef ...
    (6 coeficients by default for calcif2).  We need to decrement
    the DELAY parts by the DRY and WET parts and zero the latter.
    Finally rebuild the 3 lines for output.  The path through
    calcif and difxcalc is tortuous, but it looks like 'delay'
    is the negative of 'geom' + 'dry' + 'wet' so to undo the
    atmospheric corrections, ADDING 'dry' and 'wet' is correct.
    (SUBJECT TO VERIFICATION.)
    '''
    parts = {}
    llen = 0
    npts = 0
    plen = 0
    prec = 0
    for k in list(save.keys()):
        # capture line length
        if len(save[k]) > llen: llen = len(save[k])
        parts[k] = save[k].split()
        # capture number of items per line
        if len(parts[k]) > npts: npts = len(parts[k])
        # capture floating field size
        for p in parts[k]:
            if len(p) > plen: plen = len(p)
    pad = (llen - 22) / (npts - 6) - plen - 1
    fmt = '%%+%d.%de' % (plen + pad + 6, plen - 6)
    for c in range(6,npts):
        dryval = dryfac * float(parts['dry'][c])
        wetval = wetfac * float(parts['wet'][c])
        delay = float(parts['delay'][c]) + dryval + wetval
        parts['delay'][c] = (fmt % delay)
        parts['dry'][c] = (fmt % dryval)
        parts['wet'][c] = (fmt % wetval)
    for k in list(save.keys()):
        save[k] = ' '.join(parts[k]) + '\n'
        f.write(save[k])
        del(save[k])

def mergeCalc(verb, stations, drylst, wetlst, job, normal, noatmo):
    '''
    This function mergets the two calc output files using
    the noatmo data for the station in the list
    '''
    im = re.sub(r'calc', 'im', job)
    f = open(im + '-' + normal, 'r')
    normallines = f.readlines()
    f.close()
    f = open(im + '-' + noatmo, 'r')
    noatmolines = f.readlines()
    f.close()
    if len(normallines) != len(noatmolines):
        print('Calc output for %s have disparate lengths' % (
            im, len(normallines), len(noatmolines)))
        return 1
    f = open(im, 'w')
    srelist = genPatterns(verb, stations, normallines)
    aber = re.compile(r'ABERRATION CORR:')
    delay = re.compile(r'SRC\s+\d+\s+ANT\s+\d+\s+DELAY\s+.us.:')
    dry = re.compile(r'SRC\s+\d+\s+ANT\s+\d+\s+DRY\s+.us.:')
    wet = re.compile(r'SRC\s+\d+\s+ANT\s+\d+\s+WET\s+.us.:')
    commout = 0
    mathout = 0
    normout = 0
    natmout = 0
    save = {}
    for norm,natm in map(lambda x,y:(x,y), normallines, noatmolines):
        if norm == natm:    # copy duplicated lines, maybe
            if delay.match(norm): what = 'delay'
            elif dry.match(norm): what = 'dry'
            elif wet.match(norm): what = 'wet'
            else:                 what = 'common'
            # the lines come in order, so we just need to
            # verify that we are doing some math here
            if what != 'common':
                for ii in range(len(srelist)):
                    patt = srelist[ii]
                    if patt != r'MATCHES-NOTHING' and patt.match(norm):
                        save[what] = norm
                        if what == 'delay':
                            dryfac = 0.0
                            wetfac = 0.0
                        if what == 'dry': dryfac = drylst[ii]
                        if what == 'wet': wetfac = wetlst[ii]
                        break
            if len(save) == 0:
                # this is a line for an antenna not to adjust
                f.write(norm)
                commout += 1
            # if len(save) == 1 or 2, we need 3 lines to proceed
            if len(save) == 3:
                # save cleared as side-effect of writing 3 lines
                writeDelayDryWet(save, dryfac, wetfac, f)
                mathout += 3
        else:               # copy no-atm lines that match
            if aber.match(norm):
                # UNCORRECTED, APPROXIMATE, EXACT, NO ATMOS, or:
                f.write('ABERRATION CORR:    MIXED\n')
            else:   # these are the U,V,W lines which should differ
              for patt in srelist:
                if patt != r'MATCHES-NOTHING' and patt.match(norm):
                    f.write(natm)
                    natmout += 1
                else:
                    f.write(norm)
                    normout += 1
    f.close()
    if verb: print('%s w/ %d common %d mathout %d normal %d noatmo lines' % (
        im, commout, mathout, normout, natmout))
    return 0

def runCalc(verb, calc, options, extra, label, job):
    '''
    This routine runs calc with some options.  The
    defaults are for calcif2, but with adjustments
    made here, this should work with difxcalc.
    '''
    if calc == 'difxcalc':
        options = re.sub('--override-version', '', options)
        if extra == '-A':
            extra = '-noatmo'
    calcmd = '%s %s %s %s' % (calc, options, extra, job)
    if verb: print('Running ' + calcmd)
    im = re.sub(r'calc', 'im', job)
    if os.path.exists(im): os.unlink(im)
    rv = os.system(calcmd)
    if rv == 0 and os.path.exists(im):
        os.rename(im, im + '-' + label)
    else:
        print(im + ' not found, death will result')
        return 1
    if verb: print(im + '-' + label + ' was created\n')
    return 0

# main entry point
if __name__ == '__main__':
    o = parseOptions()
    if len(o.nargs) > 0:
        jobs = o.nargs
    elif o.job == '':
        jobs = glob.glob('*.calc')
    else:
        jobs = [re.sub(r'.calc', '', o.job) + '.calc']
    if ',' in o.noatmos:
        stations = o.noatmos.upper().split(',')
    else:
        stations = [ o.noatmos.upper() ]
    if ',' in o.dry:
        dry = list(map(float, o.dry.split(',')))
    else:
        dry = [ float(o.dry) ]
    if ',' in o.wet:
        wet = list(map(float, o.wet.split(',')))
    else:
        wet = [ float(o.wet) ]
    errors = 0
    for j in jobs:
        errors += runCalc(o.verb, o.calc, o.options, '',   'normal', j)
        errors += runCalc(o.verb, o.calc, o.options, '-A', 'noatmo', j)
        errors += mergeCalc(o.verb, stations, dry, wet, j, 'normal', 'noatmo')
    sys.exit(errors)

#
# eof
#
