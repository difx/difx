#!/usr/bin/python
#
# Script to run PolConvert in Solve mode for non-ALMA case.
# Derived from POLCONVERT_EVN script.
#
# This script has similar machinery to that of drivepolconvert/runpolconvert
# but does not rely on a "runpolconvert" script, and thus is self-contained.
#
from __future__ import absolute_import
import pickle as pk

import argparse
import datetime
import glob
import os
import re
import stat
import sys
import threading
import time
# not needed
# from six.moves import range

# runpolconvert.py is actually in $DIFXROOT/share/polconvert
# rather than on PYTHONPATH since it is not meant to be run
# that way.  It is simpler to stash drivepclib.py in the same
# place for similiar reasons.  Likewise, there is an import
# in drivepolconvert that is enabled to allow drivepclib to
# call back to functions in this file.  See Kluge in Makefile.am
rcpath = 'path not defined'
try:
    rcpath = os.environ['DIFXROOT'] + '/share/polconvert'
    sys.path.insert(0, rcpath)
    import drivepclib as dpc
except Exception as ex:
    print('Exception was',str(ex))
    print('Unable to import drivepclib functionality from:')
    print(rcpath)
    print('This is some sort of build issue you will have to solve.')
    sys.exit(1)

def parseOptions():
    '''
    This script uses its arguments to set up an invocation of PolConvert
    to derive suitable calibrations for a VLBI antenna with linear feeds
    to allow subsequent conversion from mixed-pol calibration products to
    circular basis calibration products.

    It estimates XY-phase, X/Y amplitude ratio, and X-Y multiband delay,
    from a scan on one strong calibrator.  The resulting calibrations
    may be used in a subsequent execution of drivepolconvert to complete
    the circularization process.  This script accepts multiple calibration
    scans (and runs them in parallel, see -P option).

    No script yet exists to collate output products, so your only option
    at present is to try them and see what works best.
    '''
    des = parseOptions.__doc__
    epi = ''
    use = '%(prog)s [options] [input_file [...]]\n  Version'
    use += '$Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    primary = parser.add_argument_group('Primary Options')
    secondy = parser.add_argument_group('Secondary Options')
    ### develop = parser.add_argument_group(
    ###    'Development Options (that may disappear some day)')
    # essential options
    primary.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    primary.add_argument('-r', '--run', dest='run',
        default=False, action='store_true',
        help='execute CASA with the generated input')
    primary.add_argument('-l', '--label', dest='label',
        default='', metavar='STRING',
        help='prefix with which to label output calibrations')
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
    secondy.add_argument('-I', '--indices', dest='indices',
        default='ZOOM', metavar='INDICES',
        help='comma-sep list of indices or index ranges (this-that, '
        'inclusive) or ZOOM to do find all zoom channels (such as is '
        'done for the ALMA case); at least two indices are required. '
        'Note that the underlying code currently only supports one range.')
    secondy.add_argument('-S', '--sites', dest='sites',
        default='', metavar='LIST',
        help='comma-sep list of 2-letter station codes (Xx,Yy,...) to try'
            ' (in this order) to use as a reference antenna')
    secondy.add_argument('-s', '--solve', dest='solve',
        default=1000.0, metavar='FLOAT', type=float,
        help='doSolve argument: for the chi^2 minimizer function which is '
            'computed as sum[ dosolve*(RR/LL-1)^2 + (RL^2 + LR^2) ].  If '
            'doSolve == 0 you minimize the cross-hands; if doSolve >> 1, '
            'you are assuming negligible Stokes V.')
    secondy.add_argument('-m', '--method', dest='method',
        default='gradient', metavar='STRING',
        help='solve method: "gradient" or "LM" (or "Levenberg-Marquardt"), or '
            'if scipy is available, also "COBYLA" or "NM" (or "Nelder-Mead")')
    secondy.add_argument('-A', '--exAnts', dest='exAnts',
        default='', metavar='LIST',
        help='comma-separated list of antennas to exclude from gain solution')
    secondy.add_argument('-B', '--exBase', dest='exBase',
        default='', metavar='LIST',
        help='colon-separated list of ant,ant pairs to exclude from gain '
        'solution, e.g.:  AA,BB:CC,DD to exclude AA-BB and CC-DD baselines.')
    secondy.add_argument('-x', '--xyadd', dest='xyadd',
        default='0.0', metavar='STRING',
        help='a priori x-y manual phase adjustment')
    secondy.add_argument('-d', '--xydel', dest='xydel',
        default='0.0', metavar='STRING',
        help='a priori x-y manual delay adjustment')
    secondy.add_argument('-R', '--xyratio', dest='xyratio',
        default='1.0', metavar='STRING',
        help='a priori x/y manual ratio adjustment')
    secondy.add_argument('-F', '--feed', dest='feed',
        default='', metavar='STRING',
        help='comma-sep dictionary of feed rotation angles (in degrees) '
        'keyed by SC, e.g., AA:0.0,BB:10.0')
    secondy.add_argument('-X', '--npix', dest='npix',
        default=50, metavar='INT', type=int,
        help='The number of pixels for fringe search area')
    secondy.add_argument('-b', '--bpmode', dest='bpmode', #solint
        default='1,1', metavar='LIST',
        help='comma-sep list of "solint" values; 0,...for MBD mode, '
            'C,... for BP mode (with average of C channels')
    # the remaining arguments provide the list of input files
    parser.add_argument('nargs', nargs='*',
        help='List of DiFX input job files to process')
    return parser.parse_args()

def parseInputIndices(o):
    '''
    Parse input index list into a doIFs argument string.
    Input should be a comma-sep list of indices or this-that (inclusive).
    '''
    if o.verb: print("  Parsing '%s'"%o.indices)
    if len(o.indices) == 0:
        raise Exception('Some range of IFs needs to be supplied')
    o.doIF = list()
    parts = o.indices.split(',')
    for pp in parts:
        if '-' in pp:
            this,that = pp.split('-')
            [o.doIF.append(pi) for pi in range(int(this),int(that)+1)]
        else:
            o.doIF.append(int(pp))
    o.doIF.sort()
    o.zfirst = o.doIF[0]
    o.zfinal = o.doIF[-1]
    if o.verb:
        print('  Working with indices',str(o.doIF),o.zfirst,'..',o.zfinal)

def populateRemotelist(o):
    '''
    If ZOOM is used, o.remotelist is populated with a list of antennas.
    If it is not used, we need to do the same.  The code here is derived
    from dpc.deduceZoomIndices(o):
        o.remotelist    is the index + 1 of the remote in antmap
        o.remotename    is the list of the name of the remote station
        o.remote_map    is the list of stations from input file
    '''
    sitelist = o.sites.split(',')
    amap_re = re.compile(r'^TELESCOPE NAME\s*([0-9])+:\s*([A-Z0-9][A-Z0-9])')
    o.amap_dicts = list()
    o.remotelist = list()
    o.remotename = list()
    o.remote_map = list()
    for jobin in o.nargs:
        antmap = dict()
        ji = open(jobin, 'r')
        for line in ji.readlines():
            amap = amap_re.search(line)
            if amap: antmap[amap.group(2)] = int(amap.group(1))
        ji.close()
        o.amap_dicts.append(antmap)
        for site in sitelist:
            if site in antmap:
                plotant = antmap[site] + 1
                o.remotename.append(site)
                o.remote_map.append(str(sorted(antmap.keys())))
                break
        o.remotelist.append(plotant)
    if o.verb:
        print('  amap_dicts: ',o.amap_dicts)
        print('  remotelist: ',o.remotelist)
        print('  remotename: ',o.remotename)
        print('  remote_map: ',o.remote_map)

def getInputTemplate(o):
    '''
    This is the input script with %-able adjustments.
    It is similar to what is done in drivepolconvert except
    that we cut out the middle-man (runpolconvert) for one
    less layer of obfuscation.
    '''
    template='''    #!/usr/bin/python
    # This file contains python commands that may either be fed
    # directly to CASA as standard input, or else cut&pasted into
    # the interactive CASA prompts (which you should do if you
    # are having trouble or wish to see some of the plots).
    #
    import re
    import sys
    %sprint('Debug data follows')
    '''
    # found this via inspect...
    for key,val in o._get_kwargs():
        if type(val) is str or type(val) is datetime.datetime:
            template += ('\n    print("debug: %12s = ","%s")' % (key,val))
        else:   # str(val) is acceptable to print
            template += ('\n    print("debug: %12s = ", %s )' % (key,val))
    template += '''
    print('If Polconvert was loaded a short description follows:')
    print(polconvert.__doc__[0:200],'...')
    %sprint('Real command assembly follows:')
    nargs   = '%s'
    joblist =  %s
    caldir  = '%s'
    workdir = '%s'
    label   = '%s'
    # as with to runpolconvert, we set and copy defaults from polconvert.xml:
    theJob  = joblist[0]
    print('theJob is',theJob, 'nargs is',nargs)
    DiFXinput  = '%%s/%%s' %% (caldir, nargs)
    DiFXoutput = '%%s' %% (re.sub(r'.input$', '.difx', DiFXinput))
    DiFXcalc   = '%%s' %% (re.sub(r'.input$', '.calc', DiFXinput))
    print('DiFXinput is', DiFXinput)
    print('DiFXoutput is', DiFXoutput, 'DiFXcalc is', DiFXcalc)
    zfirst=%d
    zfinal=%d
    doIF = list(range(zfirst+1, zfinal+2))
    NIF = len(doIF)
    linAnt = [%d]
    #remote_map = #s # ['..', '..', ... ]
    #remlistone = re.sub(r'[" \\'\[\]]','',remote_map[0]).split(',')
    #remotename = #s
    linAntName = '%s'
    #plotAnt = 1 + remlistone.index(remotename[0])
    plotAnt = %d
    print('linAnt is', linAnt, '('+linAntName+')', 'plotAnt is', plotAnt)
    Range = []      # restriction on input data
    aantpath = ''   # ALMA Antenna MS table
    spw = -1        # spectral window to use
    calapphs = ''   # ASDM_CALAPPPHASE table
    calAPPTime = [0.,5.]
    gains = ['NONE']
    XYavgTime = 0.0
    dterm = gains[0]
    amp_norm = 0.0
    # these are dicts on station
    XYadd = {}
    XYdel = {}
    XYratio = {}
    XYadd[linAntName] = [%s for i in range(NIF)]
    XYdel[linAntName] = [%s for i in range(NIF)]
    XYratio[linAntName] = [%s for i in range(NIF)]
    feedRot = []
    # IDI_conjugated is irrelevant for SWIN
    plotIF = doIF
    # timeRange = []
    timeRange = [0,0,0,0, 14,0,0,0]   # first 14 days seen for plotting
    npix = %d
    # solveMethod may be 'gradient', 'Levenberg-Marquardt' or 'COBYLA'
    # calstokes is [I,Q,U,V] for the calibrator; I is ignored.
    #
    # the following line is similar to what is in runpolconvert, but
    # we are working read-only for the Solve case with doTest=True
    # sys.environ['POLCONVERTDEBUG'] = 'True'
    print('Running polconvert....')
    print('IDI=',DiFXoutput, 'OUTPUTIDI=',DiFXoutput,'\\n',
        'DiFXinput=',DiFXinput, 'DiFXcalc=',DiFXcalc,'\\n',
        'doIF=',doIF, 'linAntIdx=',linAnt,
        'Range=',Range, 'ALMAant=',aantpath, '...\\n')
    CGains = polconvert(IDI=DiFXoutput, OUTPUTIDI=DiFXoutput,
            DiFXinput=DiFXinput, DiFXcalc=DiFXcalc,
            doIF=doIF,
            linAntIdx=linAnt, Range=Range, ALMAant=aantpath,
            spw=spw, calAPP=calapphs, calAPPTime=calAPPTime,
            APPrefant='',
            gains=[gains], interpolation=[],
            gainmode=[], XYavgTime=XYavgTime,
            dterms=[dterm], amp_norm=amp_norm,
            XYadd=XYadd,
            XYdel=XYdel,
            XYratio=XYratio, usePcal=[], swapXY=[False],
            swapRL=False, feedRotation=feedRot,
            IDI_conjugated=True,
            plotIF=plotIF, plotRange=timeRange,
            plotAnt=plotAnt,
            excludeAnts=%s, excludeBaselines=%s,
            doSolve=%f,
            solint=%s,
            doTest=True, npix=npix,
            solveAmp=True,
            solveMethod='%s', calstokes=[1.,0.,0.,0.], calfield=-1
            ) 
    print('Polconvert finished')
    #
    # polconvert returns the gains and saves it in PolConvert.XYGains.dat
    # we shall make a copy and read it so as to verify that this is all
    # working correctly.
    #
    ok = False
    if %s:  # gain debug
        print('Pickling CGains as a test')
        import pickle as pk
        if sys.version_info.major < 3:
            pname = '%s' + '.PolConvert.XYGains.pkl'
            ofile = open(pname,'w')
        else:
            pname = '%s' + '.PolConvert.XYGains.pickle'
            ofile = open(pname,'wb')
        try:
            pk.dump(CGains,ofile)
        except Exception as ex:
            print(str(ex))
        ofile.close()
        if sys.version_info.major < 3:
            ifile = open(pname,'r')
        else:
            ifile = open(pname,'rb')
        try:
            Gcopy = pk.load(ifile)
            print([(k,len(Gcopy[k])) for k in Gcopy.keys()])
            print('It appears we can read',pname)
            ok = True
        except Exception as ex:
            print(str(ex))
        ifile.close()
        if ok: print('Pickling test is a PASS')
        else:  print('Pickling test is a FAIL')
    else:
        print('Not pickling or testing CGains file')
        print('Pickling test is a SKIP')
    print('Pickling test finished')
    sys.exit(0)
    '''
    return template

def createCasaInput(o, joblist, caldir, workdir):
    '''
    This function creates a file of python commands that can be piped
    directly into CASA.  It now only supports parallel execution.
    Note that joblist is now a list with precisely one job:  [job],
    caldir is '..', and workdir is where we cd'd to for the work.
    o.remote is set in createCasaInputParallel() which calls us.
    '''
    oinput = workdir + '/' + o.input
    if o.verb: print('Creating CASA input file\n  ' + oinput)
    if o.verb: verb = ''
    else:      verb = '#'
    template = getInputTemplate(o)
    # remember to keep the next two statements synchronized!
    # xyadd/xydel/xyratio are one constant for all chans of the
    # one linear antenna, but type is a string to allow other options.
    print('  .' +verb+verb+ '.\n',
        '  ',o.nargs[0], str(joblist), caldir, workdir, '\n',
        '  ',o.label, o.zfirst, o.zfinal, o.ant, o.lin, o.remote, '\n',
        '  ',o.xyadd, o.xydel, o.xyratio, o.npix,
        '  ',str(o.exAntList), str(o.exBaseLists), '\n',
        '  ',o.solve, str(o.solint), o.method, '\n',
        '  ','True', o.label, o.label)
    script = template % (verb, verb,
        o.nargs[0], str(joblist), caldir, workdir,
        o.label, o.zfirst, o.zfinal, o.ant, o.lin, o.remote,
        o.xyadd, o.xydel, o.xyratio, o.npix,
        str(o.exAntList), str(o.exBaseLists),
        o.solve, str(o.solint), o.method,
        'True', o.label, o.label)
    # write the file, stripping indents
    ci = open(oinput, 'w')
    for line in script.split('\n'):
        ci.write(line[4:] + '\n')
    ci.close()
    return os.path.exists(oinput)

def compatChecks(o):
    '''
    In order to re-use drivepolconvert code, we need to define a
    few things that are set in some of the developmental arguments.
    '''
    o.exp = ''
    o.input = ''
    o.output = ''
    o.remote = -1
    o.test = False

def methodChecks(o):
    '''
    Check that the method names are legal, and update with longer names.
    '''
    if o.method == 'gradient':
        return
    elif o.method == 'COBYLA':
        return
    elif o.method == 'LM':
        o.method = 'Levenberg-Marquardt'
        return
    elif o.method == 'NM':
        o.method = 'Nelder-Mead'
        return
    else:
        raise Exception('Unsupported solution method:  ' + o.method)

def solintChecks(o):
    '''
    The solint parameter is complicated...  Here we expect o.bpmode
    to contain a comma-sep list which we convert to o.solint and pass
    to PolConvert (eventually with some sanity checks).
    '''
    o.solint = o.bpmode.split(',')
    if ((type(o.solint) is not list) or
        not (len(o.solint) == 2 or len(o.solint) == 3)):
            raise Exception('Solint must be a list of 2 or 3 values')

def checkAntBase(o):
    '''
    Check that the antenna and baseline names are found in inputs.
    It should be ok to exclude things that are not present in every
    scan, but the user should be stopped if a name isn't found at all.
    '''
    if not o.exAnts == '': o.exAntList = o.exAnts.split(',')
    else: o.exAntList = list()
    if not o.exBase == '': o.exBaseList = o.exBase.split(':')
    else: o.exBaseList = list()
    o.exBaseLists = [b for b in o.exBaseList]
    exSet = set(o.exAntList)
    for bl in o.exBaseList:
        [exSet.add(a) for a in bl.split(',')]
    antSet = set()
    for am in o.amap_dicts:
        [antSet.add(a) for a in am.keys()]
    if o.verb and o.exAnts+o.exBase != '':
        print('Exclusions:')
        print('  excluding antennas:', o.exAntList)
        print('  excluding baselines:', o.exBaseList)
        print('  excluding baseline list:', o.exBaseLists)
        print('  excluded set:', exSet)
        print('  input antennas:', antSet)
    for ant in exSet:
        if not ant in antSet:
            raise Exception('Antenna %s not found in input' % str(ant))

def checkOptions(o):
    '''
    Check that all options make sense, and other startup items.
    We do this prior to any real work, but after tarball extraction
    if such was provided.  The subfunctions throw exceptions on issues.
    '''
    compatChecks(o)
    methodChecks(o)
    solintChecks(o)
    dpc.inputRelatedChecks(o)
    dpc.runRelatedChecks(o)

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
    if opts.indices == 'ZOOM':
        if opts.verb: print('deducing indices for ZOOM case')
        dpc.deduceZoomIndices(opts)
    else:
        if opts.verb: print('mapping antenna names and indices')
        parseInputIndices(opts)
        if opts.verb: print('setting up remote antenna list')
        populateRemotelist(opts)
    # finally
    checkAntBase(opts)
    # run the jobs in parallel
    if opts.verb:
        print('\nParallel execution with %d threads\n' % opts.parallel)
    dpc.createCasaInputParallel(opts)
    dpc.executeCasaParallel(opts)
    if opts.verb:
        print('\nDrivePolconvert execution is complete\n')
    # explicit 0 exit
    sys.exit(0)

#
# eof
#
