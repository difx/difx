#!/usr/bin/python
#
# Script to drive PolConvert at the correlators intended for
# less CASA-aware users.
#
'''
drivepolconvert.py -- a program to drive the polconvert process
'''

import argparse
import datetime
import os
import re
import stat
import threading
import time

def parseOptions():
    '''
    Normally CASA is intended to be run interactively, and
    that requires the user to be familiar with its quirks.
    This script generates the appropriate (Python) commands
    that could be typed into an interactive session, or for the
    more likely use case, piped into CASA for the desired work.
    If CASA is not found in your path, you must supply it via
    the environment variable DIFXCASAPATH (which is used to
    build these tools and hence is probably set in your DiFX setup).
    '''
    des = parseOptions.__doc__
    epi =  'In the typical use case, you would first unpack the QA2 tarball '
    epi += 'and then process some number of similar jobs '
    epi += 'first with prepolconvert.py, then with '
    epi += 'drivepolconvert.py, and finally difx2mark4 and/or difx2fits. '
    epi += 'If you want to adjust the CASA invocation beyond what the '
    epi += 'script provides, edit the output file and '
    epi += 'then run it manually using the instructions provided. '
    epi += 'In normal usage, you only need '
    epi += 'to supply the list of jobs and the label (-l). '
    epi += 'Diagnostic plots of per-IF fringes is controlled with the '
    epi += '-f option; if used -m, -S, -X and -T become relevant.  In '
    epi += 'particular, with -T, no conversion is written to disk, '
    epi += 'but all of the diagnostic plots are made and saved.'
    use = '%(prog)s [options] [input_file [...]]\n  Version'
    use += '$Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    # essential options
    parser.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    parser.add_argument('-p', '--prep', dest='prep',
        default=False, action='store_true',
        help='run prepolconvert.py on the same joblist--'
        'generally not a good idea unless you are certain it will work')
    parser.add_argument('-r', '--run', dest='run',
        default=False, action='store_true',
        help='execute CASA with the generated input')
    parser.add_argument('-l', '--label', dest='label',
        default='', metavar='STRING',
        help='prefix to the QA2 polconvert calibration directories. '
        'The exact names despend on the QA2 version (see -q option).')
    # optional, developmental or convenience arguments
    parser.add_argument('-P', '--parallel', dest='parallel',
        default=0, metavar='INT', type=int,
        help='Number of CASA jobs to run in parallel.  The best value '
        'depends on the number of physical cores and the memory available. '
        '0 reverts to the non-parallel execution logic; 1 should provide '
        'similar results, >1 should simply be that much faster.')
    parser.add_argument('-i', '--input', dest='input',
        default='', metavar='FILE',
        help='name of input file that will be created for CASA.')
    parser.add_argument('-o', '--output', dest='output',
        default='', metavar='FILE',
        help='name of output file to collect CASA output chatter.')
    parser.add_argument('-e', '--exp', dest='exp',
        default='', metavar='STRING',
        help='VEX experiment name, prefix of job input files; it will '
        'be derived from the list of jobs if not supplied')
    parser.add_argument('-a', '--ant', dest='ant',
        default=1, metavar='INT', type=int,
        help='1-based index of linear (ALMA) antenna (normally 1)')
    parser.add_argument('-x', '--xyadd', dest='xyadd',
        default='', metavar='STRING',
        help='user supplied XY angle adjustment or empty for defaults, '
        'normally 180.0 or 0.0')
    parser.add_argument('-q', '--qa2', dest='qa2',
        default='v8', metavar='STRING',
        help='table naming scheme for the QA2 tables; there should be ' +
            'eight tables for antennas, appphase, dterms, bandpass, ' +
            'ampgains, phasegains and xy phase and xy gains.  ' +
            'Options are "v0" .. "v11" or a ' +
            'comma-sep list in an environment variable QA2TABLES.  In '
            'versions prior to v4, ".concatenated.ms" was part of the '
            'label.  For v4-v11 and subsequent the label is just the '
            'uid name (and/or other identifiers).   The default is "v8". '
            'Examine the script for the details....')
    parser.add_argument('-E', '--avgtime', dest='avgtime',
        default=0.0, metavar='FLOAT', type=float,
        help='If >0 this will time-average the gains to reduce noise')
    parser.add_argument('-y', '--gainmeth', dest='gainmeth',
        default='T', metavar='CHAR',
        help='Specify the gain method to use on all calibration tables'
            ' except ones with "XY0", "bandpass" or "Gxyamp" in name;'
            ' "T" combines the gains, "G" retains separation of X and Y.')
    parser.add_argument('-d', '--noDterm', dest='nodt',
        default=False, action='store_true',
        help='disable use of Dterm calibration tables')
    parser.add_argument('-A', '--ampNorm', dest='ampnrm',
        default=1.0, type=float,
        help='set the DPFU in ANTAB or <=0 to apply it (0)')
    parser.add_argument('-G', '--gainDel', dest='gaindel',
        default='', metavar='LIST',
        help='comma-sep list of gain tables to delete: del(gains[x])' +
            'will be applied for every x in the list AFTER checks for' +
            'existence of tables has been carried out')
    parser.add_argument('-s', '--spw', dest='spw',
        default=-1, metavar='INT', type=int,
        help='Index of SPW for PolConvert to use: 0,1,2,3 for the ' +
            'four basebands, or -1 (default) for PolConvert to select')
    # plotting arguments
    parser.add_argument('-f', '--fringe', dest='fringe',
        default=4, metavar='INT', type=int,
        help='Activate plotting diagnostics during conversion with the ' +
            'number of IFs (channels) to produce fringe diagnostics on. ' +
            'The default is 4.  Sensible values are 1 (a middle channel), ' +
            'N for that many channels spread through the IF range, '
            'or 0 for off.')
    parser.add_argument('-m', '--remote', dest='remote',
        default=-1, metavar='INT', type=int,
        help='Index of remote antenna on baseline to converted antenna. ' +
            'The default is -1 (disabled).  The vex file will be searched' +
            'for the appropriate indices based on the site list, see -S.' +
            'This value may be used only if there are issues...')
    parser.add_argument('-S', '--sites', dest='sites',
        default='', metavar='LIST',
        help='comma-sep list of 2-letter station codes to try' +
            ' (in order) to use for plot diagnostics')
    parser.add_argument('-X', '--npix', dest='npix',
        default=50, metavar='INT', type=int,
        help='The number of pixels to show in the fringe plots (50)')
    parser.add_argument('-T', '--test', dest='test',
        default=False, action='store_true',
        help='Turns off processing of files, just does plotting')
    parser.add_argument('-z', '--zmchk', dest='zmchk',
        default=False, action='store_true',
        help='the default (False) assumes that a PolConvert fix (to not' +
            ' crash if the IFs mentioned cannot be converted); set this' +
            ' to recover the original behavior which protects PolConvert.')
        
    # the remaining arguments provide the list of input files
    parser.add_argument('nargs', nargs='*',
        help='List of DiFX input job files')
    return parser.parse_args()

def calibrationChecks(o):
    '''
    Check that required files are present.
    '''
    if o.label == '':
        raise Exception, 'A label (-l) is required to proceed'
    if o.verb: print 'Using label %s' % o.label
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
        raise Exception, 'at least 7 QA2 tables are required, see --qa2 option'
    keys = ['a', 'c', 'd', 'b', 'g', 'p', 'x', 'y']
    o.qa2_dict = dict(zip(keys,o.qal))
    if o.nodt:
        print 'nodt option is', o.nodt
        o.qa2_dict['d'] = 'NONE'
    for key in o.qa2_dict:
        d = 'programmer-error'
        if key in ['a', 'c', 'b', 'g', 'p']:
            d = ('%s.' + o.qa2_dict[key]) % o.conlabel
        if key in ['d', 'x', 'y']:
            d = ('%s.' + o.qa2_dict[key]) % o.callabel
        if not os.path.exists(d) or not os.path.isdir(d):
            if key == 'd' and d == 'NONE':
                if o.verb: print 'Skipping D Terms as requested'
            else:
                raise Exception, 'Required directory %s is missing' % d
        elif o.verb:
            print 'Calibration table %s is present' % d
    if o.gainmeth != 'T' and o.gainmeth != 'G':
        raise Exception, 'Illegal gainmeth %s' % o.gainmeth
    if o.avgtime < 0:
        raise Exception, 'The gain average time must be non-negative'

def inputRelatedChecks(o):
    '''
    Check things that will end up in the CASA input file.
    '''
    expchk = set()
    jobset = set()
    if len(o.nargs) < 1:
        raise Exception, 'No input files to work on...this is pointless'
    for j in o.nargs:
        if not os.path.exists(j):
            raise Exception, 'Input file %s is missing' % j
        expchk.add(j.split('_')[0])
        jobtmp = j.split('_')[1]
        jobset.add(jobtmp.split('.')[0])
    if len(expchk) > 1 or len(expchk) == 0:
        raise Exception, ('Only one experiment may be processed ' +
            'but %d are present: %s') % (len(expchk), ','.join(expchk))
    if o.exp == '': o.exp = expchk.pop()
    if o.verb: print 'Processing experiment %s' % o.exp
    if len(jobset) < 1:
        raise Exception, 'No job inputs to process (%d)' % len(jobset)
    djobs = list(jobset)
    djobs.sort()
    o.jobnums = djobs
    o.djobs = str(map(str,djobs))
    if o.verb: print 'Processing jobs "%s"' % o.djobs

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
        print '(Warning, input file %s.save was destroyed.)' % o.input
    if os.path.exists(o.output):
        os.rename(o.output, o.output + '.save')
        print '(Warning, output file %s.save was destroyed.)' % o.output
    if o.verb: print 'Input/output files are %s/%s' % (o.input, o.output)

    if 'DIFXCASAPATH' in os.environ:
        o.casa = '%s/casa' % os.environ['DIFXCASAPATH']
    else:
        o.casa = 'casa'
    if o.verb: cmd = 'type %s'
    else:      cmd = 'type %s 1>/dev/null 2>/dev/null'
    if o.verb: print 'CASA executable is %s' % o.casa

    if o.run:
        if os.system(cmd % o.casa):
            raise Exception, 'CASA does not appear to be in your path'

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
        print 'Running ' + cmd
    if os.system(cmd):
        raise Exception, 'Error while running prepolconvert, see prepol.log'

def deduceZoomIndicies(o):
    '''
    Pull the Zoom frequency indicies from the input files and check
    that all input files produce the same first and last values.
    (PolConvert is now more forgiving of varying zoom channel usage.)
    The user can specify a remote antenna, but now logic here (and
    in runpolconvert) switches to o.remotelist assuming it is of the
    proper length.  This should solve poor plotting choices.
    '''
    sitelist = o.sites.split(',')
    if o.verb: print 'Sitelist is',sitelist
    o.remotelist = []
    zoompatt = r'^ZOOM.FREQ.INDEX.\d+:\s*(\d+)'
    almapatt = r'^TELESCOPE NAME %d:\s*AA' % (o.ant-1)
    amap_re = re.compile(r'^TELESCOPE NAME\s*([0-9])+:\s*([A-Z0-9][A-Z0-9])')
    freqpatt = r'^FREQ..MHZ..\d+:\s*(\d+)'
    zfirst = set()
    zfinal = set()
    mfqlst = set()
    antmap = {}
    almaline = ''
    plotant = -1
    for jobin in o.nargs:
        zfir = ''
        zfin = ''
        cfrq = []
        ji = open(jobin, 'r')
        for line in ji.readlines():
            zoom = re.search(zoompatt, line)
            alma = re.search(almapatt, line)
            freq = re.search(freqpatt, line)
            if almaline == '' and alma: almaline = line
            if freq: cfrq.append(freq.group(1))
            if zoom:
                if zfir == '': zfir = zoom.group(1)
                else:          zfin = zoom.group(1)
            amap = amap_re.search(line)
            if amap:
                antmap[amap.group(2)] = int(amap.group(1))
        for site in sitelist:
            if site in antmap:
                plotant = antmap[site] + 1
                break
        o.remotelist.append(plotant)
        plotant = -1
        antmap = {}
        ji.close()
        if o.verb: print 'Zoom bands %s..%s from %s' % (zfir, zfin, jobin)
        if len(cfrq) < 1:
            raise Exception, 'Very odd, no frequencies in input file ' + jobin
        cfrq.sort()
        zfirst.add(zfir)
        zfinal.add(zfin)
        mfqlst.add(cfrq[len(cfrq)/2])
    if len(zfirst) != 1 or len(zfinal) != 1:
        if o.zmchk:
            raise Exception, ('Encountered ambiguities in zoom freq ranges: ' +
                'first is ' + str(zfirst) + ' and final is ' + str(zfinal))
        elif o.verb:
            print 'global zoom first',str(zfirst),'and final',str(zfinal)
    o.zfirst = int(sorted(list(zfirst))[0])  # int(zfirst.pop())
    o.zfinal = int(sorted(list(zfinal))[-1]) # int(zfinal.pop())
    if o.verb: print 'Zoom frequency indices %d..%d found in %s\n  ..%s' % (
        o.zfirst, o.zfinal, o.nargs[0], o.nargs[-1])
    # This could be relaxed to allow AA to be not 0 using antmap
    if o.verb: print 'Alma search pattern: "' + str(almapatt) + '"'
    if almaline == '':
        raise Exception, 'Telescope Name 0 is not Alma (AA)'
    if o.verb: print 'Found ALMA Telescope line: ' + almaline.rstrip()
    if o.verb: print 'Remote antenna index is', o.remote
    if (len(o.remotelist) != len(o.nargs)): o.remotelist = []
    if o.verb: print 'Remote antenna list is',o.remotelist
    # if the user supplied a band, check that it agrees
    if len(mfqlst) > 1:
        raise Exception, ('Input files have disparate frequency structures:\n'
            '  Median frequencies: ' + str(mfqlst) + '\n'
            '  and these must be processed separately')
    medianfreq = float(mfqlst.pop())
    if   medianfreq <  90000.0: medianband = '3 (GMVA)'
    elif medianfreq < 214100.0: medianband = 'b1 (Cycle5 6[LSB]Lo)'
    elif medianfreq < 216100.0: medianband = 'b2 (Cycle5 6[LSB]Hi)'
    elif medianfreq < 228100.0: medianband = 'b3 (Cycle4 6[USB]Lo)'
    elif medianfreq < 230100.0: medianband = 'b4 (Cycle4 6[USB]Hi)'
    else:                       medianband = '??? band 7 ???'
    print 'Working with band %s based on median freq (%f)' % (
            medianband, medianfreq)

def plotPrep(o):
    '''
    This function sets a few things related to plotting.
    '''
    if o.fringe > o.zfinal+1-o.zfirst:
        o.fringe = o.zfinal+1-o.zfirst
        print 'Revising number of fringed channels to %d' % o.fringe
    if o.fringe == 0:
        o.doPlot = ['#','','#','#']
        o.remote = -1
        o.flist = ''
    elif o.fringe == 1:
        o.doPlot = ''
        o.doPlot = ['','#','','#']
        o.flist = ''
    else:
        o.doPlot = ['','#','#','']
        o.flist = '0'
        for ii in range(1,o.fringe):
            o.flist += ',(%d*len(doIF)/%d)' % (ii, o.fringe)
    if o.remote == o.ant:
        o.remote == o.ant + 1
        print 'Shifting baseline from %d-%d to %d-%d' % (
            o.ant, o.remote - 1, o.ant, o.remote)

def getInputTemplate():
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
    %simport pylab as pl
    %spl.ioff()
    #
    # variables from drivepolconvert.py required for runpolconvert.py:
    #
    DiFXout = '%s'
    label = '%s'
    expName = '%s'
    linAnt = [%s]
    rpcpath = os.environ['DIFXROOT'] + '/share/polconvert/runpolconvert.py'
    zfirst=%d
    zfinal=%d
    doIF = range(zfirst+1, zfinal+2)
    %splotIF = -1                       # plot no channels
    %splotIF = doIF[len(doIF)/2]        # plot the middle channel
    %splotIF = [doIF[i] for i in [%s]]  # plot a set of channels
    print 'using doIF value: ' + str(doIF)
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
    # timeRange=[]                      # don't care
    %stimeRange = [0,0,0,0, 14,0,0,0]   # first 14 days
    XYadd = [0.0]
    #
    spwToUse = %d
    constXYadd = %s
    %sXYadd = [%f]
    XYratio = [1.0]
    print 'using XYadd %%s' %% (str(XYadd))
    #
    djobs = %s
    print 'djobs contains these jobs: ' + str(djobs)
    #
    workdir = '%s'
    # actually do the work:
    print 'executing "execfile(rpcpath)" with rcpath and working directory'
    print rpcpath
    print workdir
    print os.getcwd()
    execfile(rpcpath)
    quit()
    '''
    return template

def createCasaInput(o, joblist, caldir, workdir):
    '''
    This function creates a file of python commands that can be piped
    directly into CASA.  It is now written to support both the single
    processing as well as the parallel processing case.
    '''
    oinput = workdir + '/' + o.input
    if o.verb: print 'Creating CASA input file\n  ' + oinput
    if o.xyadd != '':
        userXY = ''
        XYvalu = float(o.xyadd)
    else:
        userXY = '#'
        XYvalu = 0.0

    if o.test:   dotest = 'True'
    else:        dotest = 'False'

    template = getInputTemplate()
    script = template % (o.doPlot[0], o.doPlot[0],
        caldir, o.label, o.exp, o.ant, o.zfirst, o.zfinal,
        o.doPlot[1], o.doPlot[2], o.doPlot[3], o.flist,
        o.qa2, o.qal, o.qa2_dict, o.gainmeth, o.avgtime, o.ampnrm, o.gaindel,
        o.remote, o.remotelist, o.npix, dotest, o.doPlot[0],
        o.spw, o.constXYadd, userXY, XYvalu, joblist, workdir)
    # write the file, stripping indents
    ci = open(oinput, 'w')
    for line in script.split('\n'):
        ci.write(line[4:] + '\n')
    ci.close()
    return os.path.exists(oinput)

def createCasaInputSingle(o):
    '''
    Create the input to process all jobs in the current
    working directory, sequentially.
    '''
    if createCasaInput(o, o.djobs, '.', '.'):
        if o.verb: print 'Created %s for single-threaded execution' % o.input
    else:
        print 'Problem creating',o.input
        o.run = False

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
    cmd = map(str,range(13))
    cmd[0]  = '#!/bin/sh'
    cmd[1]  = '[ -f killcasa ] && exit 0'
    cmd[2]  = 'cd ' + workdir + ' && echo "starting" > ./status || exit 1'
    cmd[3]  = 'date -u +%Y-%m-%dT%H:%M:%S > ./timing'
    cmd[4]  = '%s --nologger -c %s > %s 2>&1 < /dev/null' % (
        o.casa, o.input, o.output)
    cmd[5]  = 'echo "converted" > ./status'
    cmd[6]  = 'mv casa*.log ipython-*.log casa-logs'
    cmd[7]  = 'mv %s %s *.pc-casa-command casa-logs' % (o.input, o.output)
    cmd[8]  = 'mv polconvert.last casa-logs'
    cmd[9]  = 'echo "complete" > ./status'
    cmd[10] = 'date -u +%Y-%m-%dT%H:%M:%S >> ./timing'
    cmd[11] = 'echo " "CASA for job %s finished.' % job
    cmd[12] = 'exit 0'
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
    if o.verb: print 'Creating CASA work dirs and input files ' + o.input
    o.workdirs = {}
    o.workcmds = {}
    if checkDifxSaveDirsError(o, True):
        print 'Either *.difx dirs are missing or *.save dirs are present'
        o.jobnums = []
    o.now = datetime.datetime.now()
    for job in sorted(o.jobnums):
        savename = o.exp + '_' + job
        workdir = o.now.strftime(savename + '.polconvert-%Y-%m-%dT%H.%M.%S')
        os.mkdir(workdir)
        odjobs = str(map(str,[job]))
        if createCasaInput(o, odjobs, '..', workdir):
            cmdfile = createCasaCommand(o, job, workdir)
            o.workdirs[job] = workdir
            o.workcmds[job] = cmdfile
            # print '--- created working dir with inputs for job %s ---' % job
        else:
            print '*** unable to create workdir or input for job %s ***' % job

def removeTrash(o, misc):
    '''
    A cleanup function only necessary when CASA crashes to
    sweep garbage aside so that it cannot cause problems later.
    '''
    for m in misc:
        if os.path.exists(m):
            print 'Removing prior garbage ',m
            os.system('rm -rf %s' % m)
    print 'Removing prior ipython & casa logs'
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
        if vrb and len(swine) > 0: print 'These are missing (get them):',swine
        if vrb and len(saved) > 0: print 'These are present (nuke them):',saved
        return True
    return False

def doFinalRunCheck(o):
    '''
    runpolconvert is careful, but the overhead of getting to it
    is high, so we check for this last bit of operator fatigue
    '''
    if o.run and checkDifxSaveDirsError(o, o.run):
        print '\n\n### Disabling Run so you can fix the issue\n'
        o.run=False

def executeCasa(o):
    '''
    This function pipes input to CASA and collects output.  The
    various developer debugging files (if present) are swept into
    the casa-logs directory (which is subsequently timestamped).
    '''
    misc = [ 'polconvert.last', 'POLCONVERT_STATION1.ANTAB',
             'POLCONVERT.FRINGE', 'POLCONVERT.GAINS', 'PolConvert.log',
             'CONVERSION.MATRIX', 'FRINGE.PEAKS', 'FRINGE.PLOTS' ]
    removeTrash(o, misc)
    cmd1 = 'rm -f %s' % (o.output)
    cmd2 = '%s --nologger -c %s > %s 2>&1 < /dev/null' % (
        o.casa, o.input, o.output)
    cmd3 = '[ -d casa-logs ] || mkdir casa-logs'
    if o.prep: cmd4 = 'mv prepol*.log '
    else:      cmd4 = 'mv '
    cmd4 += ' casa*.log ipython-*.log casa-logs'
    cmd5 = 'mv %s %s casa-logs' % (o.input, o.output)
    cmd6 = ''
    casanow = o.exp + '-casa-logs.' + datetime.datetime.now().isoformat()[:-7]
    if o.run:
        doFinalRunCheck(o)
    if o.run:
        if os.system(cmd1):
            raise Exception, 'That which cannot fail (rm -f), failed'
        if o.verb:
            print 'Note, ^C will not stop CASA (or polconvert).'
            print 'If it appears to hang, use kill -9 and then'
            print '"touch killcasa" to allow normal cleanup.'
            print 'Follow CASA run with:\n  tail -n +1 -f %s\n' % (o.output)
        if os.system(cmd2):
            if os.path.exists('killcasa'):
                print 'Removing killcasa'
                os.unlink('killcasa')
                print 'Proceeding with remaining cleanup'
            else:
                raise Exception, 'CASA execution "failed"'
        if o.verb:
            print 'Success!  See %s for output' % o.output
        logerr = False
        mscerr = False
        for m in misc:
            if os.path.exists(m):
                cmd6 += '[ -f %s ] && mv %s casa-logs ;' % (m,m)
        if os.system(cmd3 + ' ; ' + cmd4 + ' ; ' + cmd5):
            logerr = True
        elif os.system(cmd6):
            mscerr = True
        elif o.verb:
            print 'Swept CASA logs to ' + casanow
        if logerr: print '  There was a problem collecting CASA logs'
        if mscerr: print '  There was a problem collecting misc trash'
        jl = open('casa-logs/%s.joblist'%o.exp, 'w')
        o.nargs.sort()
        for jb in o.nargs: jl.write(jb + '\n')
        jl.close()
        os.rename('casa-logs', casanow)
        print 'Completed job list is in %s/%s.joblist' % (casanow,o.exp)
        if o.verb: print 'Review CASA run with:\n  tail -n +1 %s/%s\n' % (
            casanow, o.output)
    else:
        for m in misc:
            cmd6 += '[ -f %s ] && mv %s casa-logs ;' % (m,m)
        print ''
        print 'You can run casa manually with input from ' + o.input
        print 'Or just do what this script would do now, viz: '
        print '    ' + cmd1
        print '    ' + cmd2 + ' &'
        print '    tail -n +1 -f ' + o.output
        print '    ' + cmd3
        print '    ' + cmd4
        print '    ' + cmd5
        print '    ' + cmd6
        print '    mv casa-logs ' + casanow
        print ''
    if o.test:
        print ''
        print 'The *.difx and *.save directories should have identical'
        print 'contents, and you will need to remove *.save to continue'
        print 'additional test runs on the same jobs.'
        print ''

def convertOneScan(job,wdr,cmd):
    '''
    Process one scan for this job as laid out in wdr using cmd
    '''
    print ' job', job, 'in', wdr
    # sanity check
    if o.checkdir:
        os.chdir(wdr)
        print ' job', job, 'is', os.path.basename(os.getcwd())
        os.chdir('..')
    print ' job', job, 'w/', cmd
    # either doit or just vet the thread machinery
    if o.run: os.system(wdr + '/' + cmd)
    else: os.system('/usr/bin/sleep 10')
    time.sleep(o.sleeper)

def launchNewScanWorker(o, where):
    '''
    This pulls a job for one scan out of the dictionaries
    and launches a thread to have CASA process it.
    '''
    job = sorted(o.workdirs)[0];
    wdr = o.workdirs.pop(job)
    cmd = o.workcmds.pop(job)
    th = threading.Thread(target=convertOneScan,args=(job,wdr,cmd))
    o.workbees.append(th)
    print 'Spawning thread',th.name,'on job', job,where
    th.start()
    time.sleep(o.sleeper)

def waitForNextWorker(o):
    '''
    Monitor the threads; when one is no longer alive spawn a new job.
    When we run out of jobs, the number of active threads will normally
    drop to zero and then we are done.  Need to consider how to kill
    this beast.
    '''
    while True:
        for th in o.workbees:
            if th.isAlive():
                time.sleep(o.sleeper)
            else: # it finished
                o.workbees.remove(th)
                if len(o.workdirs) > 0:
                    launchNewScanWorker(o, '(recycled)')
                else:
                    print 'Have',len(o.workbees),'threads still running'
                    if len(o.workbees) == 0:
                        print 'Done! -- all CASA workers have finished'
                        return

def executeThreads(o):
    '''
    Create and managed o.parallel threads of execution so that each
    job is delivered to a thread for processing.  We keep processing
    as long as there is work to do and we are able to launch threads.
    Once we have reached the limit, we launch new threads for the
    remaining scans as thread slots become available
    '''
    while len(o.workdirs) > 0:
        if len(o.workbees) < o.parallel:
            launchNewScanWorker(o, '(original)')
        else:
            waitForNextWorker(o)
            return
    print 'Wait for processing threads to finish.'

def reportWorkTodo(o):
    '''
    Give the user a hint of what happened.
    '''
    if len(o.workdirs) > 0:
        print 'Several jobs were not processed:'
        for job in o.workdirs:
            print ' job', job, 'in', o.workdirs[job]
            print ' job', job, 'w/', o.workcmds[job]
    print 'Results are in',o.now.strftime('*.polconvert-%Y-%m-%dT%H.%M.%S')

def executeCasaParallel(o):
    '''
    Drive o.parallel executions of CASA in parallel.
    Aside from o.workdirs and o.workcmds, o.workbees contain the
    references to threading objects, and o.sleeper puts in pauses
    to avoid race conditions and give the human time to read.
    '''
    if not o.run:
        print 'CASA commands and working directories have been prepared.'
        print 'You can execute the jobs manually, with these commands:'
        for job in o.workdirs:
            wdir = o.workdirs[job]
            ccmd = o.workcmds[job]
            print '  pushd',wdir,'; ./'+ccmd,'& popd'
        return
    if o.verb:
        print 'Driving %d parallel CASA jobs' % (o.parallel)
        print '  Touch "killcasa" to terminate prematurely.'
        print '  (Current CASA jobs will continue until done.)'
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
        print ', Shutting down...'
        print 'You may need to use ps to find and kill some stuck'
        print 'processes in a few minutes if they are still running'
    except KeyError:
        print 'Developer screwup...'
        raise
    except Exception:
        print 'Some other problem...'
        raise
    finally:
        reportWorkTodo(o)
        try: os.unlink('killcasa')
        except: pass

#
# enter here to do the work
#
if __name__ == '__main__':
    o = parseOptions()
    checkOptions(o)
    if o.prep:
        runPrePolconvert(o)
    deduceZoomIndicies(o)
    plotPrep(o)
    if o.parallel == 0:
        createCasaInputSingle(o)
        executeCasa(o)
    else:
        createCasaInputParallel(o)
        executeCasaParallel(o)

#
# eof
#
