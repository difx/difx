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

def parseOptions():
    '''
    Normally CASA is intended to be run interactively, and
    that requires the user to be familiar with its quirks.
    This script generates the appropriate (Python) commands
    that could be typed into an interactive session, or for the
    more likely use case, piped into CASA for the desired work.
    '''
    des = parseOptions.__doc__
    epi =  'In the typical use case, a number of similar jobs '
    epi += 'would be processed first with prepolconvert.py, then with '
    epi += 'drivepolconvert.py, and then difx2mark4 or difx2fits. '
    epi += 'If you want to see some of the plots, edit the output '
    epi += 'file and run it manually.  In normal usage, you need '
    epi += 'to supply only the list of jobs and the QA2 tarball or label.'
    use = '%(prog)s [options] [input_file [...]]\n'
    use += '  Version $Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    # options
    parser.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    parser.add_argument('-p', '--prep', dest='prep',
        default=False, action='store_true',
        help='run prepolconvert.py on the same joblist')
    parser.add_argument('-r', '--run', dest='run',
        default=False, action='store_true',
        help='execute CASA with the generated input')
    parser.add_argument('-i', '--input', dest='input',
        default='', metavar='FILE',
        help='name of input file that will be created for CASA.')
    parser.add_argument('-o', '--output', dest='output',
        default='', metavar='FILE',
        help='name of output file to collect CASA output chatter.')
    parser.add_argument('-l', '--label', dest='label',
        default='', metavar='STRING',
        help='prefix to the QA2 polconvert calibration directories. '
        'Normally we expect the directories *.bandpass-zphs.cal, '
        '*.ampgains.cal.fluxscale, *.phasegains.cal, *.XY0amb-tcon, '
        '*.antenna.tab, and *.calappphase.tab to be present.')
    parser.add_argument('-t', '--qa2tar', dest='qa2tar',
        default='', metavar='TARFILE',
        help='tarfile with QA2 results.  If supplied, the results are'
        'extracted and the -l label argument is updated appropriately')
    parser.add_argument('-e', '--exp', dest='exp',
        default='', metavar='STRING',
        help='VEX experiment name, prefix of job input files; it will '
        'be derived from the list of jobs if not supplied')
    parser.add_argument('-b', '--band', dest='band',
        default='', metavar='STRING',
        help='ALMA band: one of "3" (86.268 GHz), '
        '"6Lo" (227.1 GHz) or "6Hi" (229.1 GHz)')
    parser.add_argument('-a', '--ant', dest='ant',
        default=1, metavar='INT', type=int,
        help='1-based index of linear (ALMA) antenna (normally 1)')
    parser.add_argument('-x', '--xyadd', dest='xyadd',
        default='', metavar='STRING',
        help='user supplied XY angle adjustment or empty for defaults, '
        'normally 180.0 or 0.0')
    parser.add_argument('-q', '--qa2', dest='qa2',
        default='v2', metavar='STRING',
        help='table naming scheme for the QA2 tables; there should be ' +
            'six tables for antennas, appphase, bandpass, ampgains, ' +
            'phasegains and xy phase.  Options are "v0", "v1", "v2", '
            '"v3" or a ' +
            'comma-sep list in an environment variable QA2TABLES')
    parser.add_argument('-d', '--noDterm', dest='nodt',
        default=False, action='store_true',
        help='disable use of Dterm calibration tables')
    # list of input files
    parser.add_argument('nargs', nargs='*',
        help='List of DiFX input job files')
    return parser.parse_args()

def tarballExtraction(o):
    '''
    Extract the tarball if it is supplied, and work out the label
    if none was given.
    '''
    if o.qa2tar != '' and os.path.exists(o.qa2tar):
        cmd = 'tar zxf %s' % o.qa2tar
        if o.verb: print 'Extracting tarfile with: %s' % cmd
        if os.system(cmd):
            raise Exception, 'Unable to untar %s' % o.qa2tar
        parts = o.qa2tar.split('.')
        # work out the label from the tarball name
        if o.label != '':
            pass
        elif parts[-1] == 'tgz':
            o.label = '.'.join(parts[0:-1])
        elif parts[-1] == 'gz' and parts[-2] == 'tar':
            o.label = '.'.join(parts[0:-2])
        else:
            raise Exception, 'Unable to create processing label'

def calibrationChecks(o):
    '''
    Check that required files are present.
    '''
    if o.label == '':
        raise Exception, 'A label (-l) is required to proceed'
    if o.verb: print 'Using label %s' % o.label
    o.constXYadd = 'False'
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
    else:               # supply via environment variable
        o.qal = os.environ['QA2TABLES'].split(',')
    if len(o.qal) < 7:
        raise Exception, '7 QA2 tables are required, see --qa2 option'
    keys = ['a', 'c', 'd', 'b', 'g', 'p', 'x']
    o.qa2_dict = dict(zip(keys,o.qal))
    if o.nodt:
        o.qa2_dict['d'] = 'NONE'
    for key in o.qa2_dict:
        d = ('%s.' + o.qa2_dict[key]) % o.label
        if not os.path.exists(d) or not os.path.isdir(d):
            if key == 'd' and d == 'NONE':
                pass    # Dterms are optional
            else:
                raise Exception, 'Required director %s is missing' % d

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
    tarballExtraction(o)
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
    '''
    zoompatt = r'^ZOOM.FREQ.INDEX.\d+:\s*(\d+)'
    almapatt = r'^TELESCOPE NAME %d:\s*AA' % (o.ant-1)
    freqpatt = r'^FREQ..MHZ..\d+:\s*(\d+)'
    if o.verb: print 'Alma search pattern: "' + str(almapatt) + '"'
    zfirst = set()
    zfinal = set()
    mfqlst = set()
    almaline = ''
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
        ji.close()
        if o.verb: print 'Zoom bands %s..%s from %s' % (zfir, zfin, jobin)
        if len(cfrq) < 1:
            raise Exception, 'Very odd, no frequencies in input file ' + jobin
        cfrq.sort()
        zfirst.add(zfir)
        zfinal.add(zfin)
        mfqlst.add(cfrq[len(cfrq)/2])
    if len(zfirst) != 1 or len(zfinal) != 1:
        raise Exception, ('Encountered ambiguities in zoom freq ranges: ' +
            'first is ' + str(zfirst) + ' and final is ' + str(zfinal))
    o.zfirst = int(zfirst.pop())
    o.zfinal = int(zfinal.pop())
    if o.verb: print 'Zoom frequency indices %d..%d found in %s..%s' % (
        o.zfirst, o.zfinal, o.nargs[0], o.nargs[-1])
    if almaline == '':
        raise Exception, 'Telescope Name 0 is not Alma (AA)'
    if o.verb: print 'Found ALMA Telescope line: ' + almaline.rstrip()
    # if the user supplied a band, check that it agrees
    if len(mfqlst) > 1:
        raise Exception, ('Input files have disparate frequency structures:\n'
            '  Median frequencies: ' + str(mfqlst) + '\n'
            '  and these must be processed separately')
    medianfreq = float(mfqlst.pop())
    if   medianfreq <  90000.0: medianband = '3'
    elif medianfreq < 228100.0: medianband = '6Lo'
    elif medianfreq < 230100.0: medianband = '6Hi'
    if o.band == '':
        o.band = medianband
        print 'Working with band %s based on median freq (%f)' % (
            o.band, medianfreq)
    elif o.band == medianband:
        if o.verb:
            print ('Supplied band (%s) agrees with median freq. (%f, %s)' %
                (o.band, medianfreq, medianband))
    else:
        raise Exception, ('User-supplied band (%s) disagrees with '
            'input frequencies (%f, %s)' % (o.band, medianfreq, medianband))
    # this should not be needed with the above
    if not (o.band == '3' or o.band == '6Lo' or o.band == '6Hi'):
        raise Exception, 'Observing band mis-specified, use -b 3|6Lo|6Hi'

def createCasaInput(o):
    '''
    This function creates a file of python commands that can be piped
    directly into CASA.
    '''
    if o.verb: print 'Creating CASA input file ' + o.input
    ci = open(o.input, 'w')
    template='''    #!/usr/bin/python
    # This file contains python commands that may either be fed
    # directly to CASA as standard input, or else cut&pasted into
    # the interactive CASA prompts (which you should do if you
    # are having trouble or wish to see some of the plots).
    #
    import os
    #
    # variables initialized from drivepolconvert.py:
    #
    label = '%s'
    band%s = True
    band%s = False
    band%s = False
    expName = '%s'
    linAnt = [%s]
    rpcpath = os.environ['DIFXROOT'] + '/share/polconvert/runpolconvert.py'
    zfirst=%d
    zfinal=%d
    doIF = range(zfirst+1, zfinal+2)
    # plotIF = doIF[len(doIF)/2]        # plot a middle channel
    plotIF = -1
    print 'using doIF value: ' + str(doIF)
    #
    # calibration tables
    #
    # --qa2 = %s
    # qal = %s
    qa2 = %s
    #
    # other variables that can be set in interactive mode
    # here we set them not to make any interactive plots
    #
    plotAnt=-1                          # no plotting
    # plotAnt=2                         # specifies antenna to plot
    doTest=False
    # timeRange = [0,0,0,0, 10,0,0,0]   # first 10 days
    timeRange=[]

    if not (band3 or band6Hi or band6Lo):
        print 'Pilot error, only one of band3, band6Hi or band6Lo may be true'
        quit()

    if band3:
        band = 'band3'
        XYadd=[0.0]

    if band6Lo:
        band = 'band6Lo'
        XYadd=[0.0]

    if band6Hi:
        band = 'band6Hi'
        XYadd=[0.0]

    constXYadd = %s
    %sXYadd = [%f]
    XYratio = [1.0]
    print 'using %%s with XYadd %%s' %% (band, str(XYadd))

    djobs = %s
    print 'djobs contains these jobs: ' + str(djobs)

    #
    # actually do the work:
    print 'executing "execfile(rpcpath)" with rcpath:'
    print rpcpath
    execfile(rpcpath)
    quit()
    '''
    # transfer script parameters to the input script
    if o.band == '3':
        bandnot = '6Lo'
        bandaid = '6Hi'
    elif o.band == '6Lo':
        bandnot = '3'
        bandaid = '6Hi'
    elif o.band == '6Hi':
        bandnot = '3'
        bandaid = '6Lo'
    else:
        raise Exception, 'Programmer error, illegal case'

    if o.xyadd != '':
        userXY = ''
        XYvalu = float(o.xyadd)
    else:
        userXY = '#'
        XYvalu = 0.0

    script = template % (o.label, o.band, bandnot, bandaid, o.exp,
        o.ant, o.zfirst, o.zfinal, o.qa2, o.qal, o.qa2_dict,
        o.constXYadd, userXY, XYvalu, o.djobs)

    for line in script.split('\n'):
        ci.write(line[4:] + '\n')
    ci.close()

def executeCasa(o):
    '''
    This function pipes input to CASA and collects output.  The
    various developer debugging files (if present) are swept into
    the casa-logs directory (which is subsequently timestamped).
    '''
    misc = [ 'polconvert.last', 'POLCONVERT_STATION1.ANTAB',
             'POLCONVERT.FRINGE', 'POLCONVERT.GAINS', 'PolConvert.log',
             'CONVERSION.MATRIX', 'FRINGE.PEAKS', 'FRINGE.PLOTS' ]
    cmd1 = 'rm -f %s' % (o.output)
    cmd2 = '%s --nologger < %s > %s 2>&1' % (o.casa, o.input, o.output)
    cmd3 = '[ -d casa-logs ] || mkdir casa-logs'
    if o.prep: cmd4 = 'mv prepol*.log '
    else:      cmd4 = 'mv '
    cmd4 += ' casapy-*.log ipython-*.log casa-logs'
    cmd5 = 'mv %s %s casa-logs' % (o.input, o.output)
    cmd6 = ''
    casanow = o.exp + '-casa-logs.' + datetime.datetime.now().isoformat()[:-7]
    for m in misc:
        if os.path.exists(m):
            cmd6 += '[ -f %s ] && mv %s casa-logs ;' % (m,m)
    if o.run:
        if o.verb: print 'Note, ^C will not stop CASA (or polconvert).'
        if o.verb: print 'Follow CASA run with:\n  tail -n +1 -f %s\n' % (
            o.output)
        if os.system(cmd1 + ' ; ' + cmd2):
            raise Exception, 'CASA execution "failed"'
        if o.verb:
            print 'Success!  See %s for output' % o.output
        logerr = False
        mscerr = False
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
        print ''
        print 'You can run casa manually with input from ' + o.input
        print 'Or just do what this script would do now, viz: '
        print '    ' + cmd1
        print '    ' + cmd2
        print '    ' + cmd3
        print '    ' + cmd4
        print '    ' + cmd5
        print '    ' + cmd6
        print '     mv casa-logs ' + casanow
        print ''

#
# enter here to do the work
#
if __name__ == '__main__':
    o = parseOptions()
    checkOptions(o)
    if o.prep:
        runPrePolconvert(o)
    deduceZoomIndicies(o)
    createCasaInput(o)
    executeCasa(o)

#
# eof
#
