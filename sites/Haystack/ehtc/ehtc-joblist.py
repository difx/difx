#!/usr/bin/env python
#
# Script to parse a joblist and a vex file and produce
# a variety of things in support of generating EHTC
# correlator tarball products.
#
'''
Script to parse a joblist and a vex file and produce lists of job numbers

$Id$
'''

import argparse
import math
import re
import os
import subprocess
import sys
import xml.etree.ElementTree

def parseOptions():
    '''
    Build a parser for command-line options
    '''
    des = 'This script requires a DiFX joblist file, the vex.obs file and '
    des += 'selection criteria; and it produces a list of jobs to process.'
    inp = 'Inputs:'
    act = 'Actions:'
    sel = 'Selections:'
    tst = 'Tests:'
    epi = 'For example...'
    use = '%(prog)s [options]\n'
    use += '  Version $Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    inputs = parser.add_argument_group('Input Options', inp)
    action = parser.add_argument_group('Action Options', act)
    select = parser.add_argument_group('Selection Options', sel)
    tester = parser.add_argument_group('Testing Options', tst)
    inputs.add_argument('-v', '--verbose', dest='verb',
        action='store_true', default=False,
        help='provide some commentary')
    inputs.add_argument('-j', '--job', dest='job',
        metavar='FILE', default='',
        help='The name of the DiFX job.')
    inputs.add_argument('-l', '--joblist', dest='joblist',
        metavar='FILE', default='',
        help='The full name of the difx *.joblist')
    inputs.add_argument('-o', '--vexobs', dest='vexobs',
        metavar='FILE', default='',
        help='The name of the *.vex.obs joblist')
    action.add_argument('-B', '--baselines', dest='baselines',
        action='store_true', default=False,
        help='provide a list of baselines')
    action.add_argument('-N', '--numbers', dest='numbers',
        action='store_true', default=False,
        help='provide a list of job numbers')
    action.add_argument('-S', '--sources', dest='sources',
        action='store_true', default=False,
        help='provide a list of sources from the SOURCE section')
    action.add_argument('-P', '--projects', dest='projects',
        action='store_true', default=False,
        help='provide a list of ALMA projects from the vex file')
    select.add_argument('-s', '--source', dest='source',
        metavar='STRING', default='',
        help='The name of the target source as found in the SOURCE section')
    select.add_argument('-p', '--project', dest='project',
        metavar='STRING', default='',
        help='The name of the ALMA project as declared by intent')
    tester.add_argument('-V', '--Vex', dest='vex',
        metavar='VEXTIME', default='',
        help='convert a Vex Time into MJD (and exit)')
    tester.add_argument('-M', '--MJD', dest='mjd',
        metavar='MJDATE', default='',
        help='convert an MJD Time into Vex time (and exit)')
    return parser.parse_args()

def vex2MJD(vex):
    '''
    Convert Vex into MJD.  Crudely J2000 is 51544.0 and years
    divisible by 4 (including 2000) are leap years.  Ok for life of EHTC.
    '''
    dte_re = re.compile(r'(....)y(...)d(..)h(..)m(..)s{0,1}')
    dte = dte_re.search(vex)
    if dte:
        mjd = int((int(dte.group(1)) - 2000) * 365.25 + 0.76) + 51544.0
        mjd += float(int(dte.group(2)) - 1)
        mjd += float(int(dte.group(3))) / 24.0
        mjd += float(int(dte.group(4))) / 1440.0
        mjd += float(int(dte.group(5))) / 86400.0
    return mjd

def MJD2Vex(mjd, verb=False):
    '''
    Convert MJD into Vex, by inverting the above.
    '''
    epoch = float(mjd) - 51544.0
    years = 2000 + int((epoch) / 365.25)
    epoch -= int((years - 2000) * 365.25 + 0.76)
    doy = int(epoch + 1)
    epoch -= doy - 1
    epoch *= 24
    hours = int(epoch)
    epoch -= hours
    epoch *= 60
    mins = int(epoch)
    epoch -= mins
    epoch *= 60
    secs = int(epoch)
    epoch -= secs
    if verb: print '%04d %03d %02d %02d %02d  rem %.9f d' % (
        years, doy, hours, mins, secs, (epoch/86400.0))
    return '%04dy%03dd%02dh%02dm%02ds' % (years, doy, hours, mins, secs)

def doTestVex(vex, verb=False):
    m = vex2MJD(vex)
    print vex, '->', m
    v = MJD2Vex(m, verb)
    print v
    sys.exit(0)
def doTestMJD(mjd, verb=False):
    v = MJD2Vex(mjd, verb)
    print mjd, '->', v
    m = vex2MJD(v)
    print m
    sys.exit(0)

def parseFirst(line, o):
    '''
    Parse the first line of the job file and gather some info.
    '''
    task = line.split()
    EXPER = task[0].split('=')[1]
    V2D   = task[1].split('=')[1]
    PASS  = task[2].split('=')[1]
    MJD   = task[3].split('=')[1]
    VER   = task[4].split('=')[1]
    V2DV  = task[5].split('=')[1]
    VEX   = task[6].split('=')[1]
    if o.verb:
        print '# Job EXPER %s V2D %s PASS %s MJD %s' % (EXPER, V2D, PASS, MJD)
        print '# DiFX/V2D version is %s/%s' % (VER, V2DV)
        print '# Vexfile is %s' % VEX
    ovp = os.path.abspath(o.vexobs)
    vxp = os.path.abspath(VEX)
    if ovp != vxp and o.verb:
        print '#Warning, this job file refers to a different vexfile:'
        print '# ',ovp
        print '# ',vxp

def doJobList(o):
    '''
    Parse the joblist file and gather useful information
    into a dict, o.jobbage
    '''
    f = open(o.joblist)
    first = True
    o.jobbage = {}
    o.baseline = set()
    for line in f.readlines():
        if line[0] == '#': continue
        if first:
            parseFirst(line.rstrip(), o)
            first = False
        dets = line.rstrip().split()
        if len(dets) > 9:
            JOBNUM = dets[0].split('_')[1]
            MSTART = float(dets[1])
            MSTOP  = float(dets[2])
            BASELINES = dets[9:]
            if o.verb:
                print '#Job %s MJD (%.7f..%.7f) baselines %s' % (
                    JOBNUM, MSTART, MSTOP, '-'.join(BASELINES))
            o.jobbage[JOBNUM] = [MSTART, MSTOP, BASELINES]
            for b in BASELINES: o.baseline.add(b)
    f.close()
    if o.verb:
        print '# Unique baselines: ' + ' '.join(o.baseline)

def doParseVex(o):
    '''
    Parse the vex obs file and gather useful information
    '''
    if os.path.exists(o.vexobs) and os.path.isfile(o.vexobs):
        o.vxoxml = os.path.basename(o.vexobs[0:-8]) + '.xml'
        args = ['VEX2XML', '-in', o.vexobs, '-out', o.vxoxml]
        if o.verb:
            print '#Converting VEX to XML with:\n# ' + ' '.join(args)
        p = subprocess.Popen(args,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (v2xout, v2xerr) = p.communicate()
        p.wait()
        if p.returncode:
            err = 'Return code %d from VEX2XML' % p.returncode
            raise RuntimeError, err
    o.vextree = xml.etree.ElementTree.parse(o.vxoxml)
    os.unlink(o.vxoxml)

def doFindProj(o):
    '''
    Read the lines locating the scans assigned to projects:
        ...
        scan x
        * intent = "ALMA:PROJECT_FINAL_SCAN:yyyy"
        ...
        scan x++
        * intent = "ALMA:PROJECT_FIRST_SCAN:zzzz"
        ...
    '''
    f = open(o.vexobs)
    o.projscans = {}
    lastscan = ''
    thisproj = ''
    scan_re = re.compile(r'\s*scan\s*([^;]+);')
    first_re = re.compile(
        r'.*intent.*=.*ALMA:PROJECT_FIRST_SCAN:(.*).$')
    final_re = re.compile(
        r'.*intent.*=.*ALMA:PROJECT_FINAL_SCAN:(.*).$')
    for line in f.readlines():
        sre = scan_re.search(line)
        if sre: lastscan = sre.group(1)
        first = first_re.search(line)
        if first: thisproj = first.group(1)
        if len(thisproj) > 0 and len(lastscan) > 0:
            if thisproj in o.projscans:
                o.projscans[thisproj].append(lastscan)
            else:
                o.projscans[thisproj] = [lastscan]
            lastscan = ''
        final = final_re.search(line)
        if final: thisproj = ''
    f.close()
    if o.verb:
        for p in sorted(o.projscans.keys()):
            print p, ': ', o.projscans[p]

def doFindSrcs(o):
    '''
    Generate a list of sources as found in SOURCE section
    def <dname>; source_name = <sname> ; ra = <ra> ; dec = <dec> ; ...
    '''
    if not o.vextree: return
    o.srcs = []
    for sd in o.vextree.findall('SOURCE/def'):
        dname = sd.find('defname').text
        sname = sd.find('source_name/value').text
        ra    = sd.find('ra/value').text
        dec   = sd.find('dec/value').text
        frame = sd.find('ref_coord_frame/value').text
        if o.verb:
            print dname,sname,ra,dec,frame
        o.srcs.append(sname)

def findJobMatch(o, smjd):
    '''
    Find the job with the matching mjd start smjd.
    2s/86400s  = .00002314814814814814
    '''
    for j in o.jobbage.keys():
        if math.fabs(o.jobbage[j][0] - smjd) < 0.00002:
            return j
    return None

def doScanVTree(o):
    '''
    Scan the vex file matching jobs with scans
    When jobs are so matched, the vex info is appended
    '''
    if not o.vextree: return
    for sn in o.vextree.findall('SCHED/scan'):
        name = sn.find('scanname').text
        start = sn.find('start/value').text
        smjd = vex2MJD(start)
        mode = sn.find('mode/value').text
        vsrc = sn.find('source/value').text
        sits = []
        for station in sn.findall('station'):
            sc = station.find('value').text
            if sc: sits.append(sc)
        if o.jobbage: job = findJobMatch(o, smjd)
        else:         job = None
        if job:
            dur = 86400.0 * (o.jobbage[job][1] - o.jobbage[job][0])
            if o.verb:
                print job, name, start, smjd
                print job,' ', mode, vsrc, int(dur + 0.5), o.jobbage[job][0]
                print job,' ', o.jobbage[job][2]
                print job,' ', sits
            o.jobbage[job].append([name, start, smjd, dur, vsrc, mode])
            o.jobbage[job].append(sits)

def adjustOptions(o):
    '''
    Grok arguments and make some adjustments.
    '''
    if len(o.vex) > 0:       doTestVex(o.vex, o.verb)
    if len(o.mjd) > 0:       doTestMJD(o.mjd, o.verb)
    if o.joblist == '' and o.job != '': o.joblist = o.job + '.joblist'
    if o.vexobs  == '' and o.job != '': o.vexobs  = o.job + '.vex.obs'
    o.jobbage = None
    o.baseline = None
    o.projscans = None
    o.srcs = None
    if len(o.joblist) > 0:   doJobList(o)
    o.vextree = None
    if len(o.vexobs) > 0:
        doParseVex(o)
        doFindProj(o)
        doFindSrcs(o)
        doScanVTree(o)
    return o

def doSelectSource(o):
    '''
    Pretty trivial: select on source
    '''
    if o.source == '': return
    if o.source in o.srcs:
        o.srcs = [o.source]
    else:
        o.srcs = None
    # FIXME: attack o.jobbage

def doSelectProject(o):
    '''
    Pretty trivial: select on project
    '''
    if o.project == '': return
    if o.project in o.projscans:
        o.projscans = { o.project : o.projscans[o.project] }
    else:
        o.projscans = None
    # FIXME: attack o.jobbage

def selectOptions(o):
    '''
    Apply selections to limit things reported
    '''
    doSelectSource(o)
    doSelectProject(o)
    return o

def doBaselines(o):
    '''
    Generate a list of baselines from the joblist file
    '''
    if len(o.baseline) == 0: return
    print ' '.join(o.baseline)

def doNumbers(o):
    '''
    Generate a list of job numbers from the joblist file
    '''
    if len(o.jobbage) == 0: return
    print ' '.join(sorted(o.jobbage.keys()))

def doSources(o):
    '''
    Generate a list of sources from the vex file
    '''
    if len(o.srcs) == 0: return
    print ' '.join(o.srcs)

def doProjects(o):
    '''
    Generate a list of projects from the ALMA project intent comments.
    '''
    if len(o.projscans) == 0: return
    for p in o.projscans:
        print 'ALMA Project %-9s:' % p,
        for s in o.projscans[p]: print s,
        print

# main entry point
if __name__ == '__main__':
    o = parseOptions()
    o = adjustOptions(o)
    o = selectOptions(o)

    if o.baselines: doBaselines(o)
    if o.numbers:   doNumbers(o)
    if o.sources:   doSources(o)
    if o.projects:  doProjects(o)

    sys.exit(0)

#
# eof
#
