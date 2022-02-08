#!/usr/bin/python
#
# Script to drive PolConvert at the correlators intended for
# less CASA-aware users.
#
# Py2/3 compatible via python-modernize and __future__ imports.
# PolConvert itself still requires a Py2 version of CASA (5.x)
#
'''
drivepolconvert.py -- a program to drive the polconvert process
'''

from __future__ import absolute_import
from __future__ import print_function
import argparse
import datetime
import glob
import os
import re
import stat
import sys
import threading
import time
# not needed:
# from six.moves import map
# from six.moves import range
# from six.moves import zip

# this Kluge is for re-use by singlepolsolve.py
# import solvepclib as spc

def parseOptions():
    '''
    PolConvert is executed within CASA which is inconvenient for
    production work.  This script generates appropriate (Python)
    commands with which CASA may be executed as a background task.
    Multiple conversion jobs can and should be executed in parallel
    (no more than one job per processor core is best, controlled with -P).
    The DiFX compilation assumes DIFXCASAPATH is defined to point
    to the CASA bin directory.  If DIFXCASAPATH is not in your
    environment when this script is run, but the same version is to
    be found as "casa" you should have no trouble.
    '''
    des = parseOptions.__doc__
    epi =  'In the typical use case, you would first unpack the QA2 tarball '
    epi += 'and then process some number of similar jobs '
    epi += 'first with prepolconvert.py, then with '
    epi += 'drivepolconvert.py, and finally difx2mark4 and/or difx2fits. '
    epi += 'In normal usage, you only need '
    epi += 'to supply the list of jobs, the QA2 label (-l) '
    epi += 'the -q version (which tables to use) and -r to run.'
    epi += 'You may omit the -r option, in which case the jobs will be '
    epi += 'prepared with instructions for running them manually.  You '
    epi += 'can then edit the input files as needed. '
    epi += 'The number of diagnostic plots of per-IF fringes '
    epi += 'is controlled with the '
    epi += '-f option; you can also use -m, -S, -X and -T, see above.  In '
    epi += 'particular, with the test option (-T), no conversion is '
    epi += 'written to disk, '
    epi += 'but all of the diagnostic plots are made and saved. '
    epi += 'This is useful to manually tweak things prior to committing. '
    use = '%(prog)s [options] [input_file [...]]\n  Version'
    use += '$Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    primary = parser.add_argument_group('Primary Options')
    secondy = parser.add_argument_group('Secondary Options')
    plotter = parser.add_argument_group('Plotting Options')
    develop = parser.add_argument_group(
        'Development Options (that may disappear some day)')
    # essential options
    primary.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    primary.add_argument('-r', '--run', dest='run',
        default=False, action='store_true',
        help='execute CASA with the generated input')
    primary.add_argument('-l', '--label', dest='label',
        default='', metavar='STRING',
        help='prefix to the QA2 polconvert calibration directories. '
        'The names despend on the QA2 version (see -q option).')
    primary.add_argument('-q', '--qa2', dest='qa2',
        default='v8', metavar='STRING',
        help='table naming scheme: v{0..11} are for QA2 tables, or '
            's{0...} for non-QA2 cases.  Use "help" for details.')
    # not normally needed, secondary arguments
    secondy.add_argument('-P', '--parallel', dest='parallel',
        default=6, metavar='INT', type=int,
        help='Number of jobs to run in parallel. '
        '(The default is 6.)')
    secondy.add_argument('-p', '--prep', dest='prep',
        default=False, action='store_true',
        help='run prepolconvert.py on the same joblist--'
        'generally not a good idea unless you are certain it will work')
    secondy.add_argument('-a', '--ant', dest='ant',
        default=1, metavar='INT', type=int,
        help='1-based index of linear (ALMA) antenna (normally 1)')
    secondy.add_argument('-L', '--lin', dest='lin',
        default='AA', metavar='SC', # 'alma'
        help='2-letter station code (all caps) for linear pol station (AA)')
    # plotting arguments
    plotter.add_argument('-f', '--fringe', dest='fringe',
        default=4, metavar='INT', type=int,
        help='Activate plotting diagnostics after conversion with the '
            'number of IFs (channels) to produce fringe diagnostics on. '
            'The default is 4.  Sensible values are 1 (pick a middle channel), '
            'N for that many channels spread through the IF range, '
            'or 0 for off.')
    plotter.add_argument('-S', '--sites', dest='sites',
        default='', metavar='LIST',
        help='comma-sep list of 2-letter station codes (Xx,Yy,...) to try'
            ' (in this order) to use for plot diagnostics')
    plotter.add_argument('-X', '--npix', dest='npix',
        default=50, metavar='INT', type=int,
        help='The number of pixels to show in the fringe plots (50)')
    plotter.add_argument('-T', '--test', dest='test',
        default=False, action='store_true',
        help='Turns off processing of files, just does plotting')
    # developmental or convenience arguments
    develop.add_argument('-i', '--input', dest='input',
        default='', metavar='FILE',
        help='name of input file that will be created for CASA.')
    develop.add_argument('-o', '--output', dest='output',
        default='', metavar='FILE',
        help='name of output file to collect CASA output chatter.')
    develop.add_argument('-e', '--exp', dest='exp',
        default='', metavar='STRING',
        help='VEX experiment name, prefix of job input files; it will '
        'be derived from the list of jobs if not supplied')
    develop.add_argument('-x', '--xyadd', dest='xyadd',
        default='', metavar='STRING',
        help='user supplied per station XY angle adjustment dict, e.g. '
        ' "Xx:0.0, ..."  (empty by default).  Normally 180.0 or 0.0 are '
        ' the values you might need to supply ')
    develop.add_argument('-E', '--avgtime', dest='avgtime',
        default=0.0, metavar='FLOAT', type=float,
        help='If >0 this will time-average the gains to reduce noise')
    develop.add_argument('-y', '--gainmeth', dest='gainmeth',
        default='T', metavar='CHAR',
        help='Specify the gain method to use on all calibration tables'
            ' except ones with "XY0", "bandpass" or "Gxyamp" in name;'
            ' "T" combines the gains, "G" retains separation of X and Y.')
    develop.add_argument('-d', '--noDterm', dest='nodt',
        default=False, action='store_true',
        help='disable use of Dterm calibration tables')
    develop.add_argument('-A', '--ampNorm', dest='ampnrm',
        default=1.0, type=float,
        help='value for the DPFU in ANTAB or <=0 to just apply it (0)')
    develop.add_argument('-G', '--gainDel', dest='gaindel',
        default='', metavar='LIST',
        help='comma-sep list of gain tables to delete: del(gains[x])'
            'will be applied for every x in the list AFTER checks for'
            'existence of tables has been carried out')
    develop.add_argument('-s', '--spw', dest='spw',
        default=-1, metavar='INT', type=int,
        help='Index of SPW for PolConvert to use: 0,1,2,3 for the '
            'four basebands, or -1 (default) for PolConvert to select')
    develop.add_argument('-m', '--remote', dest='remote',
        default=-1, metavar='INT', type=int,
        help='Index of remote antenna on baseline to converted antenna. '
            'The default is -1 (disabled).  The vex file will be searched'
            'for the appropriate indices based on the site list, see -S.'
            'This value may be used only if there are issues...')
    develop.add_argument('-z', '--zmchk', dest='zmchk',
        default=False, action='store_true',
        help='the default (False) assumes that a PolConvert fix (to not'
            ' crash if the IFs mentioned cannot be converted); set this'
            ' to recover the original behavior which protects PolConvert.')
    # the remaining arguments provide the list of input files
    parser.add_argument('nargs', nargs='*',
        help='List of DiFX input job files to process')
    return parser.parse_args()

def tableSchemeHelp():
    '''
    Provide some help on the complex scheme
    '''
    story='''
    There are two modes of operation: QA2 mode, where a set of CASA
    calibration tables have been (separately) derived from polarimetric
    observations, and non-QA2 (solve) mode where a solution is derived
    from one scan and applied to others.

    The first scheme has twelve versions, v0 .. v11 based on the development
    history.  Currently v4 .. v11 are sensible; the default is v8.  This
    scheme requires a set of tables (named with the the -l label argument):

        a <label>.concatenated.ms.ANTENNA
        c <label>.concatenated.ms.calappphase
        d <label>.calibrated.ms.Df0.<APP|ALMA>
        b <label>.concatenated.ms.bandpass-zphs
        g <label>.concatenated.ms.flux_inf.<APP|ALMA>
        p <label>.concatenated.ms.phase_int.<APP|ALMA><.XYsmooth>
        x <label>.calibrated.ms.XY0.<APP|ALMA>
        y <label>.calibrated.ms.Gxyamp.<APP|ALMA>
    
    Here APP or ALMA refers to ALMA Phasing Project/System (APP/APS) scans
    used for VLBI or normal ALMA calibration scans and calibrated or
    concatenated refer to the details of the QA2 production script.
    (APS data is used for the tables labelled with APP, ALMA data for
    the tables with ALMA.)  Finally ".XYsmooth" appears if the data was
    smoothed, and absent if not.

        a    c    d    b    g    p    x    y    XYsmooth
    v4  .    .    APP  .    APP  APP  APP  APP  no
    v5  .    .    APP  .    APP  ALMA APP  ALMA no
    v6  .    .    APP  .    APP  ALMA APP  APP  no
    v7  .    .    APP  .    APP  APP  APP  ALMA no
    v8  .    .    APP  .    APP  APP  APP  APP  yes
    v9  .    .    APP  .    APP  ALMA APP  ALMA yes
    v10 .    .    APP  .    APP  ALMA APP  APP  yes
    v11 .    .    APP  .    APP  APP  APP  ALMA yes

    The second scheme assumes an initial reduction with <TBD.py>.

    Finally an environment variable QA2TABLES may be set to a comma-sep
    list of compete table names (ignoring label, concatenated and calibrated)
    may be used for any arbitrary set of tables.
    '''
    print(story)

def calibrationChecks(o):
    '''
    Check that required files are present.
    '''
    ### help on -q option
    if o.qa2 == 'help':
        tableSchemeHelp()
        sys.exit(0)
    ### label processing
    if o.label == '':
        raise Exception('A label (-l) is required to proceed')
    if o.verb: print('Using label %s' % o.label)
    o.constXYadd = 'False'
    o.conlabel = o.label
    o.callabel = o.label
    ### developmental
    if o.qa2 == 'v0':   # original 1mm names
        o.qal = ['antenna.tab','calappphase.tab', 'NONE', 'bandpass-zphs.cal',
               'ampgains.cal.fluxscale', 'phasegains.cal', 'XY0amb-tcon']
    elif o.qa2 == 'v1': # revised 3mm names
        o.qal = ['ANTENNA', 'calappphase', 'NONE', 'bandpass-zphs',
               'flux_inf', 'phase_int.APP', 'XY0.APP' ]
    elif o.qa2 == 'v2': # revised 3mm names with Dterms (default)
        o.qal = ['ANTENNA', 'calappphase', 'Df0', 'bandpass-zphs',
               'flux_inf', 'phase_int.APP', 'XY0.APP' ]
    elif o.qa2 == 'v3': # revised 3mm names with Dterms, constant XYadd
        o.constXYadd = 'True'
        o.qal = ['ANTENNA', 'calappphase', 'Df0', 'bandpass-zphs',
               'flux_inf', 'phase_int.APP', 'XY0.APP' ]
    ### production default
    elif o.qa2 == 'v4' or o.qa2 == 'v8': # v3+D-APP/G-APP
        o.qal = ['ANTENNA', 'calappphase', 'Df0.APP', 'bandpass-zphs',
               'flux_inf.APP', 'phase_int.APP', 'XY0.APP', 'Gxyamp.APP' ]
        o.conlabel = o.label + '.concatenated.ms'
        o.callabel = o.label + '.calibrated.ms'
        if o.qa2 == 'v8': o.qal[5] += '.XYsmooth'
    ### or other desperation plans
    elif o.qa2 == 'v5' or o.qa2 == 'v9': # v3+D-ALMA/G-ALMA
        o.qal = ['ANTENNA', 'calappphase', 'Df0.ALMA', 'bandpass-zphs',
               'flux_inf.APP', 'phase_int.APP', 'XY0.APP', 'Gxyamp.ALMA' ]
        o.conlabel = o.label + '.concatenated.ms'
        o.callabel = o.label + '.calibrated.ms'
        if o.qa2 == 'v9': o.qal[5] += '.XYsmooth'
    elif o.qa2 == 'v6' or o.qa2 == 'v10': # v3+D-ALMA/G-APP
        o.qal = ['ANTENNA', 'calappphase', 'Df0.ALMA', 'bandpass-zphs',
               'flux_inf.APP', 'phase_int.APP', 'XY0.APP', 'Gxyamp.APP' ]
        o.conlabel = o.label + '.concatenated.ms'
        o.callabel = o.label + '.calibrated.ms'
        if o.qa2 == 'v10': o.qal[5] += '.XYsmooth'
    elif o.qa2 == 'v7' or o.qa2 == 'v11': # v3+D-APP/G-ALMA
        o.qal = ['ANTENNA', 'calappphase', 'Df0.APP', 'bandpass-zphs',
               'flux_inf.APP', 'phase_int.APP', 'XY0.APP', 'Gxyamp.ALMA' ]
        o.conlabel = o.label + '.concatenated.ms'
        o.callabel = o.label + '.calibrated.ms'
        if o.qa2 == 'v11': o.qal[5] += '.XYsmooth'
    ### if push comes to shove
    else:               # supply via environment variable
        o.qal = os.environ['QA2TABLES'].split(',')
    if len(o.qal) < 7:
        raise Exception('at least 7 QA2 tables are required, see --qa2 option')
    keys = ['a', 'c', 'd', 'b', 'g', 'p', 'x', 'y']
    # o.qa2_dict = dict(list(zip(keys,o.qal)))
    o.qa2_dict = dict(zip(keys,o.qal))
    if o.nodt:
        print('nodt option is', o.nodt)
        o.qa2_dict['d'] = 'NONE'
    for key in o.qa2_dict:
        d = 'programmer-error'
        if key in ['a', 'c', 'b', 'g', 'p']:
            d = ('%s.' + o.qa2_dict[key]) % o.conlabel
        if key in ['d', 'x', 'y']:
            d = ('%s.' + o.qa2_dict[key]) % o.callabel
        if not os.path.exists(d) or not os.path.isdir(d):
            if key == 'd' and d == 'NONE':
                if o.verb: print('Skipping D Terms as requested')
            else:
                raise Exception('Required directory %s is missing' % d)
        elif o.verb:
            print('Calibration table %s is present' % d)
    if o.gainmeth != 'T' and o.gainmeth != 'G':
        raise Exception('Illegal gainmeth %s' % o.gainmeth)
    if o.avgtime < 0:
        raise Exception('The gain average time must be non-negative')

def inputRelatedChecks(o):
    '''
    Check things that will end up in the CASA input file.
    We introduce the jobset to make sure we only process
    each input file once.
    '''
    expchk = set()
    jobset = set()
    if len(o.nargs) < 1:
        raise Exception('No input files to work on...this is pointless')
    for j in o.nargs:
        if not os.path.exists(j):
            raise Exception('Input file %s is missing' % j)
        js = j.split('_')
        ee = js[0]
        expchk.add(ee)
        jss = js[1].split('.')
        jobset.add(jss[0])
        jsbe = ee + '_' + jss[0] + '.input'
        if j != jsbe:
            raise Exception('Input file %s not %s' % (j, jsbe))
    if len(expchk) > 1 or len(expchk) == 0:
        raise Exception(('Only one experiment may be processed ' +
            'but %d are present: %s') % (len(expchk), ','.join(expchk)))
    if o.exp == '': o.exp = expchk.pop()
    if o.verb: print('Processing experiment %s' % o.exp)
    if len(jobset) < 1:
        raise Exception('No job inputs to process (%d)' % len(jobset))
    djobs = list(jobset)
    djobs.sort()
    o.jobnums = djobs
    o.djobs = str(list(map(str,o.jobnums)))
    o.nargs = []
    # make sure o.nargs is co-ordered with o.jobnums for reference
    for jn in o.jobnums: o.nargs.append(o.exp + '_' + jn + '.input')
    if o.verb: print('Processing jobs "%s"\n(%s)' % (o.djobs,str(o.nargs)))

def runRelatedChecks(o):
    '''
    Check things that are required to run CASA.
    '''
    if o.input == '':
        o.input = o.exp + '.pc-casa-input'
    if o.output == '':
        o.output = o.exp + '.pc-casa-output'
    if os.path.exists(o.input):
        os.rename(o.input, o.input + '.save')
        print('(Warning, input file %s.save was destroyed.)' % o.input)
    if os.path.exists(o.output):
        os.rename(o.output, o.output + '.save')
        print('(Warning, output file %s.save was destroyed.)' % o.output)
    if o.verb: print('Input/output files are %s/%s' % (o.input, o.output))

    if 'DIFXCASAPATH' in os.environ:
        o.casa = '%s/casa' % os.environ['DIFXCASAPATH']
        msg = 'from DIFXCASAPATH'
    else:
        o.casa = 'casa'
        msg = 'from "casa"'
    if o.verb: cmd = 'type %s'
    else:      cmd = 'type %s 1>/dev/null 2>/dev/null'
    if o.verb: print('CASA executable is %s (%s)' % (o.casa,msg))

    if o.run:
        if os.system(cmd % o.casa):
            raise Exception('CASA does not appear to be in your path')

def checkOptions(o):
    '''
    Check that all options make sense, and other startup items.
    We do this prior to any real work, but after tarball extraction
    if such was provided.  The subfunctions throw exceptions on issues.
    '''
    calibrationChecks(o)
    inputRelatedChecks(o)
    runRelatedChecks(o)

def runPrePolconvert(o):
    '''
    Run prepolconvert using the supplied jobs.
    '''
    cmd = 'prepolconvert.py'
    if o.verb: cmd += ' -v'
    for ii in o.nargs: cmd += ' ' + ii
    cmd += ' > prepol.log 2>&1'
    if o.verb:
        print('Running ' + cmd)
    if os.system(cmd):
        raise Exception('Error while running prepolconvert, see prepol.log')

def deduceZoomIndices(o):
    '''
    Pull the Zoom frequency indicies from the input files and check
    that all input files produce the same first and last values.
    (PolConvert is now more forgiving of varying zoom channel usage.)
    The user can specify a remote antenna, but now logic here (and
    in runpolconvert) switches to o.remotelist assuming it is of the
    proper length.  This should solve poor plotting choices.
    '''
    sitelist = o.sites.split(',')
    if o.verb: print('Sitelist is',sitelist)
    o.amap_dicts = list()
    o.remotelist = []
    o.remotename = []
    o.remote_map = []
    zoompatt = r'^ZOOM.FREQ.INDEX.\d+:\s*(\d+)'
    # we call it 'alma' but o.lin holds the choice
    almapatt = r'^TELESCOPE NAME %d:\s*%s' % (o.ant-1,o.lin)
    amap_re = re.compile(r'^TELESCOPE NAME\s*([0-9])+:\s*([A-Z0-9][A-Z0-9])')
    freqpatt = r'^FREQ..MHZ..\d+:\s*(\d+)'
    zfirst = set()
    zfinal = set()
    mfqlst = set()
    antmap = {}
    newargs = []
    for jobin in o.nargs:
        almaline = ''
        zfirch = 1000
        zfinch = -1
        cfrq = []
        ji = open(jobin, 'r')
        for line in ji.readlines():
            zoom = re.search(zoompatt, line)
            alma = re.search(almapatt, line)
            freq = re.search(freqpatt, line)
            if almaline == '' and alma: almaline = line
            if freq: cfrq.append(freq.group(1))
            if zoom:
                zoomch = int(zoom.group(1))
                if zoomch < zfirch: zfirch = zoomch
                if zoomch > zfinch: zfinch = zoomch
            amap = amap_re.search(line)
            if amap:
                antmap[amap.group(2)] = int(amap.group(1))
        ji.close()
        zfir = str(zfirch)
        zfin = str(zfinch)

        # cull jobs that do not appear to have 'alma' as telescope 0
        if almaline == '':
            issue = True
            for jn in o.jobnums:
                if '_' + str(jn) + '.input' in jobin:
                    print('ALMA not in',jobin,', skipping (',jn,')')
                    o.jobnums.remove(jn)
                    issue = False
                    break
            if issue: raise Exception('Unable to purge job ' + jobin)
            else:     continue
        else:
            print('Found %s in'%o.lin,jobin,almaline.rstrip())
            newargs.append(jobin)
            # workout plot ant for this job
            plotant = -1
            for site in sitelist:
                if site in antmap:
                    plotant = antmap[site] + 1
                    o.remotename.append(site)
                    o.remote_map.append(str(sorted(antmap.keys())))
                    break
            o.remotelist.append(plotant)
            o.amap_dicts.append(antmap)
            antmap = {}

        if o.verb: print('Zoom bands %s..%s from %s' % (zfir, zfin, jobin))
        if len(cfrq) < 1:
            raise Exception('Very odd, no frequencies in input file ' + jobin)
        cfrq.sort()
        zfirst.add(zfir)
        zfinal.add(zfin)
        mfqlst.add(cfrq[len(cfrq)//2])

    o.nargs = newargs
    # o.jobnums is also synchronized
    # and o.nargs should be synchronized with o.remotelist
    # o.remotename and o.remote_map are just for readable diagnostics below
    # and we resync o.djobs here
    o.djobs = str(list(map(str,o.jobnums)))
    if len(zfirst) != 1 or len(zfinal) != 1:
        if o.zmchk:
            raise Exception('Encountered ambiguities in zoom freq ranges: ' +
                'first is ' + str(zfirst) + ' and final is ' + str(zfinal))
        elif o.verb:
            print('global zoom first',str(zfirst),'and final',str(zfinal))
    if len(zfirst) > 0 and len(zfinal) > 0:
        o.zfirst = int(sorted(list(zfirst))[0])  # int(zfirst.pop())
        o.zfinal = int(sorted(list(zfinal))[-1]) # int(zfinal.pop())
    else:
        o.zfirst = -1
        o.zfinal = -2
    if (len(o.nargs) > 0) and o.verb:
        print('Zoom freq. indices %d..%d found in \n  %s..%s' % (
            o.zfirst, o.zfinal, o.nargs[0], o.nargs[-1]))
    elif o.verb:
        print('Not going to be doing any real work after this: No jobs')
    # Report on remote peer for polconvert plot diagnostics
    if (len(o.remotelist) != len(o.nargs)): o.remotelist = []
    if o.verb:
        for j,r,s,m in map(lambda x,y,z,w:(x,y,z,w),
            o.nargs, o.remotelist, o.remotename, o.remote_map):
            print("%s<->%s(%s) %s" % (j,s,r,m))
        print('Remote list len is',len(o.remotelist),'index is',o.remote)
        if o.remote == -1: print('(index of -1 means "not used")')
        print('Remote list is',o.remotelist,'(indices start at 1)')
        print('Jobs now',o.djobs)
    # If the user supplied a band, check that it agrees
    print('mfqlst is', mfqlst)
    if len(mfqlst) == 1:
        medianfreq = float(mfqlst.pop())
    elif len(mfqlst) > 1:
        print(('Input files have disparate frequency structures:\n' +
            '  Median frequencies: ' + str(mfqlst) + '\n'))
        mfqlist = list(mfqlst)
        medianfreq = float(mfqlist[len(mfqlist)/2])
        print('Using the median of medians: ', medianfreq)
    else:
        print('No median frequency, so no idea about medianband')
        print('Leaving it up to PolConvert to sort out')
        return
    # finally the diagnostic message
    if o.lin == 'AA':
        if   medianfreq <  90000.0: medianband = '3 (GMVA)'
        elif medianfreq < 214100.0: medianband = 'b1 (Cycle5 6[LSB]Lo)'
        elif medianfreq < 216100.0: medianband = 'b2 (Cycle5 6[LSB]Hi)'
        elif medianfreq < 228100.0: medianband = 'b3 (Cycle4 6[USB]Lo)'
        elif medianfreq < 230100.0: medianband = 'b4 (Cycle4 6[USB]Hi)'
        else:                       medianband = '??? band 7 ???'
        print('Working with band %s based on median freq (%f)' % (
                medianband, medianfreq))
    else:
        print('Non-ALMA case, no comment on median band or freq')

def plotPrep(o):
    '''
    This function sets a few things related to plotting.
    '''
    if o.fringe > o.zfinal+1-o.zfirst:
        o.fringe = o.zfinal+1-o.zfirst
        print('Revising number of fringed channels to %d' % o.fringe)
    if o.fringe == 0:
        o.doPlot = ['#','','#','#']
        o.remote = -1
        o.flist = ''
    elif o.fringe == 1:
        o.doPlot = ['','#','','#']
        o.flist = ''
    else:
        o.doPlot = ['','#','#','']
        o.flist = '0'
        for ii in range(1,o.fringe):
            o.flist += ',(%d*len(doIF)//%d)' % (ii, o.fringe)
    if o.remote == o.ant:
        o.remote = o.ant + 1
        print('Shifting baseline from %d-%d to %d-%d' % (
            o.ant, o.remote - 1, o.ant, o.remote))

def getInputTemplate(o):
    '''
    This is the input script with %-able adjustments.
    '''
    template='''    #!/usr/bin/python
    # This file contains python commands that may either be fed
    # directly to CASA as standard input, or else cut&pasted into
    # the interactive CASA prompts (which you should do if you
    # are having trouble or wish to see some of the plots).
    #
    import os
    import sys
    print('Debug data follows')
    '''
    # found this via inspect...
    for key,val in o._get_kwargs():
        if type(val) is str or type(val) is datetime.datetime:
            template += ('\n    print("debug: %12s = ","%s")' % (key,val))
        else:   # str(val) is acceptable to print
            template += ('\n    print("debug: %12s = ", %s )' % (key,val))
    template += '''
    %simport pylab as pl
    %spl.ioff()
    #
    # variables from drivepolconvert.py required for runpolconvert.py:
    #
    DiFXout = '%s'
    label = '%s'
    print('label is ', label)
    expName = '%s'
    linAnt = [%s]
    rpcpath = os.environ['DIFXROOT'] + '/share/polconvert/runpolconvert.py'
    zfirst=%d
    zfinal=%d
    doIF = list(range(zfirst+1, zfinal+2))
    %splotIF = -1                       # plot no channels
    %splotIF = doIF[int(len(doIF)/2)]   # plot the middle channel
    %splotIF = [doIF[i] for i in [%s]]  # plot a set of channels
    print('using doIF value: ' + str(doIF))
    #
    # calibration tables
    #
    # --qa2 = %s
    # qal = %s
    qa2 = %s
    gainmeth = '%s'
    XYavgTime = %.3g
    ampNorm = %.3g
    gainDel = '%s'
    #
    # other variables that can be set in interactive mode
    # here we set them not to make any interactive plots
    #
    # plotAnt=-1                        # no plotting
    # plotAnt=2                         # specifies antenna 2 to plot
    plotAnt=%d
    plotAntList=%s
    numFrPltPix=%d
    doTest=%s
    timeRange=[]                        # don't care, but must be defined
    %stimeRange = [0,0,0,0, 14,0,0,0]   # first 14 days
    #
    spwToUse = %d
    # default is empty dictionary, equivalent to 'AA':0.0
    XYadd = {}
    constXYadd = %s
    %sXYadd = {%s}
    print('using XYadd   %%s' %% (str(XYadd)))
    # default is empty dictionary, equivalent to 'AA':1.0
    XYratio = {}
    print('using XYratio %%s' %% (str(XYratio)))
    #
    djobs = %s
    print('djobs contains these jobs: ' + str(djobs))
    #
    workdir = '%s'
    # actually do the work:
    print('executing "execfile(rpcpath)" with rcpath and working directory')
    print(rpcpath)
    print(workdir)
    print(os.getcwd())
    if sys.version_info.major < 3:
        print('Python 2 execution')
        execfile(rpcpath)               # Py2 form
    else:
        print('Python 3 execution')
        execfile(rpcpath, globals())    # Py3 form
    quit()
    '''
    return template

def createCasaInput(o, joblist, caldir, workdir):
    '''
    This function creates a file of python commands that can be piped
    directly into CASA.  It now only supports parallel execution.
    Note that joblist is now a list with precisely one job:  [job],
    caldir is '..', and workdir is where we cd'd to for the work.
    '''
    oinput = workdir + '/' + o.input
    if o.verb: print('Creating CASA input file\n  ' + oinput)
    if o.xyadd != '': userXY = ''
    else:             userXY = '#'
    if o.test:   dotest = 'True'
    else:        dotest = 'False'

    template = getInputTemplate(o)
    # o.doPlot is set in plotPrep()
    script = template % (o.doPlot[0], o.doPlot[0],
        caldir, o.label, o.exp, o.ant, o.zfirst, o.zfinal,
        o.doPlot[1], o.doPlot[2], o.doPlot[3], o.flist,
        o.qa2, o.qal, o.qa2_dict, o.gainmeth, o.avgtime, o.ampnrm, o.gaindel,
        o.remote, o.remotelist, o.npix, dotest, o.doPlot[0],
        o.spw, o.constXYadd, userXY, o.xyadd, joblist, workdir)
    # write the file, stripping indents
    ci = open(oinput, 'w')
    for line in script.split('\n'):
        ci.write(line[4:] + '\n')
    ci.close()
    return os.path.exists(oinput)

def createCasaCommand(o, job, workdir):
    '''
    The required shell incantation to run one job (it is the
    same incantation for all jobs, but it is convenient to put
    it into the workdir.  Some of these are the same as in
    the original executeCasa() function.  The base filename
    of the command is returned as that is what will be executed
    in the specified working directory.
    '''
    basecmd = o.exp + '.' + job + '.pc-casa-command'
    cmdfile = workdir + '/' + basecmd
    os.mkdir(workdir + '/casa-logs')
    # cmd = list(map(str,list(range(14))))
    cmd = list(map(str,range(14)))
    cmd[0]  = '#!/bin/sh'
    cmd[1]  = '[ -f killcasa ] && exit 0'
    cmd[2]  = 'cd ' + workdir + ' && echo "starting" > ./status || exit 1'
    cmd[3]  = 'date -u +%Y-%m-%dT%H:%M:%S > ./timing'
    # --nocrashreport available with v 5 and later
    cmd[4]  = '%s --nologger --nogui -c %s > %s 2>&1 < /dev/null' % (
        o.casa, o.input, o.output)
    cmd[5]  = 'casarc=$?'
    cmd[6]  = ('if [ "$casarc" == 0 ]; then echo "conversion"; ' +
        'else echo "conversion failed with code $casarc"; fi > ./status')
    cmd[7]  = 'mv casa*.log ipython-*.log casa-logs 2>&-'
    # do not move self (basecmd) until self is done.
    # cmd[8]  = 'mv %s %s *.pc-casa-command casa-logs' % (o.input, o.output)
    cmd[8]  = 'mv %s %s casa-logs' % (o.input, o.output)
    # until CASA tasks are working properly, polconvert.last may not exist
    cmd[9]  = '[ -f polconvert.last ] && mv polconvert.last casa-logs'
    cmd[10] = ('if [ "$casarc" == 0 ]; then echo "completed"; ' +
        'else echo "failed with code $casarc"; fi > ./status')
    cmd[11] = 'date -u +%Y-%m-%dT%H:%M:%S >> ./timing'
    cmd[12] = ('echo " "CASA for job %s finished with return code $casarc.' %
        job)
    # cmd[13] = 'exit 0'
    cmd[13] = 'exec sh -c "mv %s casa-logs; echo PolConvert done."' % basecmd
    # add newlines and give it execute permissions so it can run
    cs = open(cmdfile, 'w')
    for ii in range(len(cmd)): cs.write(cmd[ii] + '\n')
    cs.close()
    execbits = stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR
    execbits |= stat.S_IRGRP | stat.S_IROTH
    os.chmod(cmdfile, execbits)
    if os.path.exists(cmdfile): return basecmd
    else:                       return 'error'

def createCasaInputParallel(o):
    '''
    Create all of the CASA working directories and input files.
    This is only used if o.parallel is > 0.  The work is similar
    to the createCasaInput() input case except we must first create
    a working directory of the appropriate name before creating the
    script file.  The working directory (in the other case) was
    created in runpolconvert after the execution.
    '''
    if o.verb: print('Creating CASA work dirs and input files ' + o.input)
    o.workdirs = {}
    o.workcmds = {}
    if checkDifxSaveDirsError(o, True):
        raise Exception('Fix these *.difx or *.save dirs issues')
    o.now = datetime.datetime.now()
    remotelist = o.remotelist
    o.remotelist = []
    for job,rem in map(lambda x,y:(x,y), o.jobnums, remotelist):
        savename = o.exp + '_' + job
        workdir = o.now.strftime(savename + '.polconvert-%Y-%m-%dT%H.%M.%S')
        os.mkdir(workdir)
        odjobs = str(list(map(str,[job])))
        o.remote = rem
        if createCasaInput(o, odjobs, '..', workdir):
            cmdfile = createCasaCommand(o, job, workdir)
            o.workdirs[job] = workdir
            o.workcmds[job] = cmdfile
            # print '--- created working dir with inputs for job %s ---' % job
        else:
            print('*** unable to create workdir or input for job %s ***' % job)

def removeTrash(o, misc):
    '''
    A cleanup function only necessary when CASA crashes to
    sweep garbage aside so that it cannot cause problems later.
    '''
    for m in misc:
        if os.path.exists(m):
            print('Removing prior garbage ',m)
            os.system('rm -rf %s' % m)
    print('Removing prior ipython & casa logs')
    os.system('rm -f ipython-*.log casa*.log')

def checkDifxSaveDirsError(o, vrb):
    '''
    Verify that that which must exist does, and that which should not doesn't.
    '''
    swine = []
    saved = []
    for job in o.jobnums:
        swin = './%s_%s.difx' % (o.exp,str(job))
        save = './%s_%s.save' % (o.exp,str(job))
        if os.path.exists(save): saved.append(save)
        if not os.path.exists(swin): swine.append(swin)
    if (len(saved) > 0 or len(swine) > 0):
        if vrb and len(swine) > 0: print('These are missing (get them):',swine)
        if vrb and len(saved) > 0: print('These are present (nuke them):',saved)
        return True
    return False

def convertOneScan(o,job,wdr,cmd):
    '''
    Process one scan for this job as laid out in wdr using cmd
    '''
    print(' job', job, 'in', wdr)
    # sanity check
    if o.checkdir:
        os.chdir(wdr)
        print(' job', job, 'is', os.path.basename(os.getcwd()))
        os.chdir('..')
    print(' job', job, 'w/', cmd)
    # either doit or just vet the thread machinery
    if o.run: os.system(wdr + '/' + cmd)
    else: os.system('/usr/bin/sleep 10')
    time.sleep(o.sleeper)

def launchNewScanWorker(o, where):
    '''
    This pulls a job for one scan out of the dictionaries
    and launches a thread to have CASA process it.
    '''
    job = sorted(o.workdirs)[0]
    wdr = o.workdirs.pop(job)
    cmd = o.workcmds.pop(job)
    th = threading.Thread(target=convertOneScan,args=(o,job,wdr,cmd))
    o.workbees.append(th)
    print('Spawning thread',th.name,'on job', job,where)
    th.start()
    time.sleep(o.sleeper)

def waitForNextWorker(o):
    '''
    Monitor the threads; when one is no longer alive spawn a new job.
    When we run out of jobs, the number of active threads will normally
    drop to zero and then we are done.  Need to consider how to kill
    this beast.
    '''
    print('Please wait for all processing threads to complete')
    while True:
        for th in o.workbees:
            if th.is_alive():
                time.sleep(o.sleeper)
            else: # it finished
                o.workbees.remove(th)
                if len(o.workdirs) > 0:
                    launchNewScanWorker(o, '(recycled)')
                else:
                    print('Have',len(o.workbees),'threads still running')
                    if len(o.workbees) == 0:
                        print('Done! -- all CASA workers have finished')
                        return

def executeThreads(o):
    '''
    Create and managed o.parallel threads of execution so that each
    job is delivered to a thread for processing.  We keep processing
    as long as there is work to do and we are able to launch threads.
    Once we have reached the limit, we launch new threads for the
    remaining scans as thread slots become available
    '''
    if len(o.workdirs) == 0:
        print('Golly, gee, it seems we have no work to do...moving on.')
        return
    while len(o.workdirs) > 0 and len(o.workbees) < o.parallel:
        launchNewScanWorker(o, '(original)')
    waitForNextWorker(o)

def reportWorkTodo(o):
    '''
    Give the user a hint of what happened.
    '''
    if len(o.workdirs) > 0:
        print('Several jobs were not processed:')
        for job in o.workdirs:
            print(' job', job, 'in', o.workdirs[job])
            print(' job', job, 'w/', o.workcmds[job])
    pcdirstamps = o.now.strftime('*.polconvert-%Y-%m-%dT%H.%M.%S')
    print('Results are in',pcdirstamps)
    pcdirs = glob.glob(pcdirstamps)
    pclogs = glob.glob(pcdirstamps+'/PolConvert.log')
    if len(pcdirs) == len(o.jobnums):
        print('The number of polconvert dirs (%d) is correct.' % len(pcdirs))
    else:
        print('Error: %d workdirs and %d jobs'%(len(pcdirs),len(o.jobnums)))
    if len(pclogs) == len(o.jobnums):
        print('The number of polconvert log files (%d) is correct.' % (
            len(pclogs)))
    else:
        print('Error: %d polconvert log files and %d jobs'%(
            len(pclogs),len(o.jobnums)))
    if o.verb:
        for pc in pcdirs: print('   ',pc)
    print('drivepolconvert is finished.')

def executeCasaParallel(o):
    '''
    Drive o.parallel executions of CASA in parallel.
    Aside from o.workdirs and o.workcmds, o.workbees contain the
    references to threading objects, and o.sleeper puts in pauses
    to avoid race conditions and give the human time to read.
    '''
    if not o.run:
        if len(o.workdirs) > 0:
            print('CASA commands and working directories have been prepared.')
            print('You can execute the jobs manually, with these commands:')
            for job in o.workdirs:
                wdir = o.workdirs[job]
                ccmd = o.workcmds[job]
                print('  pushd',wdir,'; ./'+ccmd,'& popd')
            return
        else:
            print('There are no jobs to be run at this point, move on.')
    if o.verb:
        print('Driving %d parallel CASA jobs' % (o.parallel))
        print('  Touch "killcasa" to terminate prematurely.')
        print('  (Current CASA jobs will continue until done.)')
    try:
        o.workbees = []
        o.sleeper = 1
        # developer option to double-check working dir
        o.checkdir = False
        # developer option to test threading logic.
        # o.run = False
        if o.parallel > 0:
            executeThreads(o)
    except KeyboardInterrupt:   #^C
        print(', Shutting down...')
        print('You may need to use ps to find and kill some stuck')
        print('processes in a few minutes if they are still running')
    except KeyError:
        print('Developer screwup...')
        raise
    except Exception:
        print('Some other problem...')
        raise
    finally:
        try:
            reportWorkTodo(o)
            os.unlink('killcasa')
        except: pass
        finally: pass

#
# enter here to do the work
#
if __name__ == '__main__':
    argv = ' '.join(sys.argv)
    opts = parseOptions()
    opts.argv = argv
    checkOptions(opts)
    if opts.prep:
        runPrePolconvert(opts)
    deduceZoomIndices(opts)
    plotPrep(opts)
    # run the jobs in parallel
    if opts.verb:
        print('\nParallel execution with %d threads\n' % opts.parallel)
    createCasaInputParallel(opts)
    executeCasaParallel(opts)
    if opts.verb:
        print('\nDrivePolconvert execution is complete\n')
    # explicit 0 exit 
    sys.exit(0)

#
# eof
#
