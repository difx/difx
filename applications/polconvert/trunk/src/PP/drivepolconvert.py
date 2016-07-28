#!/usr/bin/python
#
# Script to drive PolConvert at the correlators intended for
# less CASA-aware users.
#
'''
drivepolconvert.py -- a program to drive the polconvert process
'''

import argparse
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
    # list of input files
    parser.add_argument('nargs', nargs='*',
        help='List of DiFX input job files')
    return parser.parse_args()

def checkOptions(o):
    '''
    Check that options make sense, and other startup items.
    '''
    # extract the tarball if supplied
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
    if o.label == '':
        raise Exception, 'A label (-l) is required to proceed'
    if o.verb: print 'Using label %s' % o.label
    # check that required files are present
    exhibits = ['bandpass-zphs.cal', 'ampgains.cal.fluxscale',
                'phasegains.cal', 'XY0amb-tcon', 'antenna.tab',
                'calappphase.tab']
    for e in exhibits:
        d = o.label + '.' + e
        if not os.path.exists(d) or not os.path.isdir(d):
            raise Exception, 'Required director %s is missing' % d
    expchk = set()
    if len(o.nargs) < 1:
        raise Exception, 'No input files to work on...this is pointless'
    for j in o.nargs:
        if not os.path.exists(j):
            raise Exception, 'Input file %s is missing' % j
        expchk.add(j.split('_')[0])
    if len(expchk) > 1 or len(expchk) == 0:
        raise Exception, ('Only one experiment may be processed ' +
            'but %d are present: %s') % (len(expchk), ','.join(expchk))
    if o.exp == '': o.exp = expchk.pop()
    if o.verb: print 'Processing experiment %s' % o.exp
    if o.input == '':
        o.input = o.exp + '.pc-casa-input'
    if o.output == '':
        o.output = o.exp + '.pc-casa-output'
    if os.path.exists(o.input):
        print 'Warning, input file %s will be destroyed' % o.input
    if os.path.exists(o.output):
        print 'Warning, output file %s will be destroyed' % o.output
    if o.verb: print 'Input/output files: %s/%s' % (o.input, o.output)
    if not (o.band == '3' or o.band == '6Lo' or o.band == '6Hi'):
        raise Exception, 'Observing band mis-specified, use -b 3|6Lo|6Hi'

def runPrePolconvert(o):
    '''
    Run prepolconvert using the supplied jobs.
    '''
    cmd = 'prepolconvert.py'
    if o.verb: cmd += ' -v'
    for i in o.nargs: cmd += ' ' + j
    if os.system(cmd):
        raise Exception, 'Error while running prepolconvert'

def deduceZoomIndicies(o):
    '''
    Pull the Zoom frequency indicies from the input files and check
    that all input files produce the same first and last values.
    '''
    zoompatt = r'^ZOOM.FREQ.INDEX.\d+:\s*(\d+)'
    almapatt = r'^TELESCOPE NAME %d:\s*AA' % (o.ant-1)
    if o.verb: print 'Alma search pattern: "' + str(almapatt) + '"'
    zfirst = set()
    zfinal = set()
    almaline = ''
    for jobin in o.nargs:
        zfir = ''
        zfin = ''
        ji = open(jobin, 'r')
        for line in ji.readlines():
            zoom = re.search(zoompatt, line)
            alma = re.search(almapatt, line)
            if almaline == '' and alma: almaline = line
            if not zoom: continue
            if zfir == '': zfir = zoom.group(1)
            else:          zfin = zoom.group(1)
        ji.close()
        if o.verb: print 'Zoom bands %s..%s from %s' % (zfir, zfin, jobin)
        zfirst.add(zfir)
        zfinal.add(zfin)
    if len(zfirst) != 1 or len(zfinal) != 1:
        raise Exception, ('Encountered ambiguities in zoom freq ranges: ' +
            'first is ' + str(zfirst) + ' and final is ' + str(zfinal))
    o.zfirst = zfirst.pop()
    o.zfinal = zfinal.pop()
    if o.verb: print 'Zoom frequency indices %s..%s found in %s..%s' % (
        o.zfirst, o.zfinal, o.nargs[0], o.nargs[-1])
    if almaline == '':
        raise Exception, 'Telescope Name 0 is not Alma (AA)'
    if o.verb: print 'Found ALMA Telescope line: ' + almaline.rstrip()
    if o.run:
        if o.verb: cmd = 'type casa'
        else:      cmd = 'type casa 1>/dev/null 2>/dev/null'
        if os.system(cmd):
            raise Exception, 'CASA does not appear to be in your path'

def createCasaInput(o):
    '''
    This function creates a file of python commands that can be piped
    directly into CASA.
    '''
    if o.verb: print 'Creating CASA input file ' + o.input
    ci = open(o.input, 'w')
    #
    #
    #
    ci.write('quit')
    ci.close()

def executeCasa(o):
    '''
    This function pipes input to CASA and collects output
    '''
    cmd1 = 'rm -f %s' % (o.output)
    cmd2 = 'casa --nologger < %s > %s 2>&1' % (o.input, o.output)
    cmd3 = '[ -d casa-logs ] || mkdir casa-logs'
    cmd4 = 'mv casapy-*.log ipython-*.log casa-logs'
    if o.run:
        if o.verb: print 'Follow CASA run with: tail -n +1 -f ' + o.output
        if os.system(cmd1 + ' ; ' + cmd2):
            raise Exception, 'CASA execution "failed"'
        if o.verb:
            print 'Success!  See %s for output' % o.output
        if os.system(cmd3 + ' ; ' + cmd4):
            raise Exception, 'Problem collecting CASA logs'
        elif o.verb:
            print 'Swept CASA logs to casa-logs'
    else:
        print 'You can run casa manually with input from ' + o.input
        print 'Or just do: '
        print '    ' + cmd1
        print '    ' + cmd2
        print '    ' + cmd3
        print '    ' + cmd4

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
