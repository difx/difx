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
import glob
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
        help='The path of the difx *.joblist joblist')
    inputs.add_argument('-o', '--vexobs', dest='vexobs',
        metavar='FILE', default='',
        help='The path of the *.vex.obs vex file')
    inputs.add_argument('-i', '--inputs', dest='inputs',
        metavar='FILE-PATTERN', default='',
        help='The path to job input/calc files up to the '
            + ' underscore preceding the job number.')
    action.add_argument('-A', '--antennas', dest='antennas',
        action='store_true', default=False,
        help='provide a list of antennas')
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

def parseInputCalc(inp, clc, vrb):
    '''
    Read lines of .input and .calc to find the things we want.
    o.jobbage[#] = [start,stop [antennas]]
    o.cabbage[jn] = [MJDstart, MJDstop, [antennas], [vexinfo]]
    vexinfo is [scan,vexstart,mjdstart,sdur,vsrc,mode]
    '''
    jni = inp.split('_')[-1].split('.')[0]
    jnc = clc.split('_')[-1].split('.')[0]
    if len(jni) != len(jnc): return None,None,None
    jid_re = re.compile(r'^JOB ID:\s*([0-9]+)')
    sta_re = re.compile(r'^JOB START TIME:\s*([0-9.]+)')
    stp_re = re.compile(r'^JOB STOP TIME:\s*([0-9.]+)')
    stn_re = re.compile(r'^TELESCOPE\s*([0-9]+)\s*NAME:\s*([A-Z0-9]+)')
    src_re = re.compile(r'^SOURCE\s*([0-9]+)\s*NAME:\s*(.*)$')
    scn_re = re.compile(r'^SCAN\s*([0-9]+)\s*IDENTIFIER:\s*(.*)$')
    dur_re = re.compile(r'^SCAN\s*([0-9]+)\s*DUR.*:\s*([0-9]+)$')
    mde_re = re.compile(r'^SCAN\s*([0-9]+)\s*OBS.*:\s*(.*)$')
    jid = None
    mjdstart = None
    vsrc = None
    scan = None
    sdur = None
    mode = None
    antenniset = set()
    jfmt = '%' + ('0%dd' % len(jni))
    # parse calc file
    fc = open(clc)
    for liner in fc.readlines():
        line = liner.rstrip()
        jid_hit = jid_re.search(line)
        if jid_hit: jid = jfmt % (int(jid_hit.group(1)))
        sta_hit = sta_re.search(line)
        if sta_hit: mjdstart = float(sta_hit.group(1))
        stp_hit = stp_re.search(line)
        if stp_hit: mjdstop  = float(stp_hit.group(1))
        stn_hit = stn_re.search(line)
        if stn_hit: antenniset.add(stn_hit.group(2))
        src_hit = src_re.search(line)
        if src_hit: vsrc = src_hit.group(2)
        scn_hit = scn_re.search(line)
        if scn_hit: scan = scn_hit.group(2)
        dur_hit = dur_re.search(line)
        if dur_hit: sdur = dur_hit.group(2)
        mde_hit = mde_re.search(line)
        if mde_hit: mode = mde_hit.group(2)
    fc.close()
    if mjdstart: vexstart = MJD2Vex(mjdstart, vrb)
    vexinfo = [scan,vexstart,mjdstart,sdur,vsrc,mode]
    if jid != jni or jid != jnc: print '#bogus job',jni,jnc,jid
    answer = [mjdstart, mjdstop, list(antenniset), vexinfo]
    if vrb: print '# ',jid,answer
    return jid,answer,antenniset

def doInputs(o):
    '''
    Use the input pattern to glob for matching input/calc
    files and read them to provide the job information.
    o.jobbage[#] = [start,stop,[antennas],[name,start,smjd,dur,vsrc,mode]]
    '''
    o.inptfiles = glob.glob(o.inputs + '_*.input')
    o.calcfiles = glob.glob(o.inputs + '_*.calc')
    if len(o.inptfiles) != len(o.calcfiles):
        print 'Mismatch in number of input/calc files, bailing'
        return
    o.pairs = map(lambda x,y:(x,y), sorted(o.inptfiles), sorted(o.calcfiles))
    o.cabbage = {}
    for inp,clc in o.pairs:
        if o.verb: print '#Input:',inp,'\n#Calc: ',clc
        jn,dets,o.antset = parseInputCalc(inp,clc,o.verb)
        if dets and jn: o.cabbage[jn] = dets

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
    into a dict, o.jobbage.  After this routine completes,
    o.jobbage[#] = [start,stop [antennas]]
    '''
    f = open(o.joblist)
    first = True
    o.jobbage = {}
    o.antennaset = set()
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
            ANTENNAS = dets[9:]
            if o.verb:
                print '#Job %s MJD (%.7f..%.7f) antennas %s' % (
                    JOBNUM, MSTART, MSTOP, '-'.join(ANTENNAS))
            o.jobbage[JOBNUM] = [MSTART, MSTOP, ANTENNAS]
            for b in ANTENNAS: o.antennaset.add(b)
    f.close()
    if o.verb:
        print '# Unique antennas: ' + ' '.join(o.antennaset)

def doParseVex(o):
    '''
    Parse the vex obs file and gather useful information
    '''
    if os.path.exists(o.vexobs) and os.path.isfile(o.vexobs):
        o.vxoxml = os.path.basename(o.vexobs[0:-8]) + '.xml'
        args = ['VEX2XML', '-in', o.vexobs, '-out', o.vxoxml]
        if o.verb:
            print '#Converting VEX to XML with:\n# ' + ' '.join(args)
        try:
            p = subprocess.Popen(args,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except Exception, ex:
            raise Exception, 'VEX2XML failed: ' + str(ex)
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
        if first: thisproj = first.group(1)[:-1]
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

def doInputSrcs(o):
    '''
    Generate a list of sources as found in o.cabbage
    '''
    if not o.cabbage: return
    o.srcset = set()
    for c in o.cabbage:
        o.srcset.add(o.cabbage[c][3][4])
    o.srcs = list(o.srcset)

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
    When jobs are so matched, the vex info is appended:
    o.jobbage[#] = [start,stop,[antennas],[name,start,smjd,dur,vsrc,mode]]
    So source is o.jobbage[#][3][4]
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
    o.cabbage = None
    o.antset = None
    if len(o.inputs) > 0:    doInputs(o)
    o.jobbage = None
    o.antennaset = None
    o.projscans = None
    o.srcs = None
    if len(o.joblist) > 0:   doJobList(o)
    o.vextree = None
    if len(o.vexobs) > 0:
        rc = os.system('type VEX2XML 2>&-')
        if rc == 0: doParseVex(o)
        if o.vextree:
            doFindProj(o)
            doFindSrcs(o)
            doScanVTree(o)
            o.rubbage = o.jobbage
            o.antlers = o.antennaset
        else:
            doFindProj(o)
            doInputSrcs(o)
            o.rubbage = o.cabbage
            o.antlers = o.antset
    return o

def doSelectSource(o):
    '''
    Pretty trivial: select on source
    So source is o.jobbage[#][3][4]
    '''
    if o.source == '': return
    newjobs = {}
    if o.source in o.srcs:
        for j in o.rubbage:
            if o.rubbage[j][3][4] == o.source:
                newjobs[j] = o.rubbage[j]
                if o.verb: print '#S',j,str(newjobs[j])
    o.rubbage = newjobs

def doSelectProject(o):
    '''
    Pretty trivial: select on project
    So scan name is o.rubbage[#][3][0]
    '''
    if o.project == '': return
    newjobs = {}
    if o.project in o.projscans:
        for j in o.rubbage:
            if o.rubbage[j][3][0] in o.projscans[o.project]:
                newjobs[j] = o.rubbage[j]
                if o.verb: print '#P',j,str(newjobs[j])
    o.rubbage = newjobs

def selectOptions(o):
    '''
    Apply selections to limit things reported
    '''
    doSelectSource(o)
    doSelectProject(o)
    return o

def doAntennas(o):
    '''
    Generate a list of antennas from the joblist file
    '''
    if len(o.antlers) == 0: return
    print 'antennas="' + ' '.join(o.antlers) + '"'

def doNumbers(o):
    '''
    Generate a list of job numbers from the joblist file
    '''
    if len(o.rubbage) == 0: return
    jl = map(lambda x:"%s_%s.input" % (o.job, x), sorted(o.rubbage.keys()))
    #print 'jobs="' + ' '.join(sorted(o.rubbage.keys())) + '"'
    print 'jobs="' + ' '.join(jl) + '"'

def doSources(o):
    '''
    Generate a list of sources from the vex file
    '''
    if len(o.srcs) == 0: return
    print 'sources="' + ' '.join(o.srcs) + '"'

def doProjects(o):
    '''
    Generate a list of projects from the ALMA project intent comments.
    '''
    if len(o.projscans) == 0: return
    for p in o.projscans:
        print 'project_' + p + '="' + ' '.join(o.projscans[p]) + '"'

# main entry point
if __name__ == '__main__':
    o = parseOptions()
    o = adjustOptions(o)
    o = selectOptions(o)

    if o.antennas:  doAntennas(o)
    if o.numbers:   doNumbers(o)
    if o.sources:   doSources(o)
    if o.projects:  doProjects(o)

    sys.exit(0)

#
# eof
#
