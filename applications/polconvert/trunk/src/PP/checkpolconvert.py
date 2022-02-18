#!/usr/bin/python
#
# Script to check polconvert QA2 tables.  Optionally, it can
# slice out a time-restricted version of the main tables.
#
# Py2/3 compatible via python-modernize and __future__ imports.
# PolConvert itself still requires a Py2 version of CASA (5.x)
#
# Portions of this file should be synchronized with drivepolconvert;
# look for "(Sync with drivepolconvert.)" in the doc strings.
#
'''
checkpolconvert.py -- a program to check the polconvert tables
'''

from __future__ import absolute_import
from __future__ import print_function
import argparse
import datetime
import re
import os
import sys

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
    epi += 'This script is designed to report the shape of the tables, and '
    epi += 'optionally (-c), extract some time-selected interval (-b ~ -e).'
    use = '%(prog)s [options]\n  Version'
    use += '$Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    # essential options
    parser.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    parser.add_argument('-r', '--run', dest='run',
        default=False, action='store_true',
        help='execute CASA with the generated input')
    parser.add_argument('-l', '--label', dest='label',
        default='', metavar='STRING',
        help='prefix to the QA2 polconvert calibration directories. '
        'The exact names despend on the QA2 version (see -q option).')
    parser.add_argument('-d', '--ldir', dest='ldir',
        default='', metavar='DIR',
        help='directory in which to find the QA2 calibration directories. '
        'Files needed will be linked into the current directory')
    # about qa2
    parser.add_argument('-i', '--input', dest='input',
        default='', metavar='FILE',
        help='name of input file that will be created for CASA.')
    parser.add_argument('-o', '--output', dest='output',
        default='', metavar='FILE',
        help='name of output file to collect CASA output chatter.')
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
    # about making a copy
    parser.add_argument('-c', '--copy', dest='copy',
        default='', metavar='STRING',
        help='prefix to use for the copy of the QA2 tables')
    parser.add_argument('-k', '--nuke', dest='nuke',
        default=False, action='store_true',
        help='if set, delete any tables bearing the copy name')
    parser.add_argument('-b', '--begin', dest='begin',
        default='', metavar='TIME',
        help='something timelike for qa.time() for start of interval')
    parser.add_argument('-e', '--end', dest='end',
        default='', metavar='TIME',
        help='something timelike for qa.time() for end of interval')
    # the remaining arguments provide the list of input files
    parser.add_argument('nargs', nargs='*',
        help='There should be no additional arguments')
    return parser.parse_args()

def calibrationChecks(o):
    '''
    Check that required files are present.
    (Sync with drivepolconvert.)
    '''
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
    o.qa2_dict = dict(zip(keys,o.qal))
    o.qa2_full = dict()
    o.qa2_copy = dict()
    for key in o.qa2_dict:
        d = 'programmer-error'
        c = ''
        if key in ['a', 'c', 'b', 'g', 'p']:
            d = ('%s.' + o.qa2_dict[key]) % o.conlabel
        if key in ['d', 'x', 'y']:
            d = ('%s.' + o.qa2_dict[key]) % o.callabel
        if not os.path.exists(d) or not os.path.isdir(d):
            if o.ldir != '' and os.path.exists(o.ldir):
                os.symlink(o.ldir + '/' + d, d)
                print('Linked %s from %s' % (d, o.ldir))
        if not os.path.exists(d) or not os.path.isdir(d):
            raise Exception('Required directory %s is missing' % d)
        elif o.verb:
            print('Calibration table %s is present' % d)
        o.qa2_full[key] = d
        if o.copy != '':
            c = re.sub(o.label, o.copy, d)
            if c == d:
                print('No, the copies must be copies--skipping: ' + c)
                c = ''
            if os.path.exists(c):
                if o.nuke:
                    print('-k is set, so we delete this table first')
                else:
                    print('Sorry, you must first delete--skipping: ' + c)
                    c = ''
        if c != '':
            o.qa2_copy[key] = c
    o.exp = 'checkpolconvert'

def reportTableNames(o):
    '''
    Report on the table names which will be processed
    '''
    print('Tables identified for processing:')
    for key in sorted(o.qa2_full):
        print('  key ' + key + ' table ' + o.qa2_full[key])
        if key in o.qa2_copy:
            print('  ->    table ' + o.qa2_copy[key])
    if o.begin + o.end != '':
        print('Extraction is from time "%s" to time "%s"' % (o.begin, o.end))

def runRelatedChecks(o):
    '''
    Check things that are required to run CASA.
    (Sync with drivepolconvert.)
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
    reportTableNames(o)
    runRelatedChecks(o)

def checkTemplate(key, caltab):
    '''
    Add code to check the calfile.  Checking consists of updating the
    "ok" variable and using the term ERROR.  Because CASA does not allow
    a return value, post-processing of the output file is needed.  The
    ANTENNA table need not have a time column
    '''
    cs='''
    key='%s'
    tab='%s'
    print('Table (%%s) %%s' %% (key,tab))
    cnames, nmrows, tcname, tshape, tfirst, tfinal, tm_col = calopen(key, tab)
    for line in textwrap.wrap('  columns: %%s' %% str(cnames),
        break_long_words=False, subsequent_indent='    '):
            print(line)
    if tcname in cnames:
        print('  time index: %%d' %% cnames.index(tcname))
    else:
        if key != 'a':
            print('  time column ' + str(tcname) + ' is missing (ERROR)')
            ok += 1
    print('  time shape: %%s %%f %%f duration %%f' %% (
        tshape, tfirst, tfinal, (tfinal - tfirst)))
    if tfirst > 0 and tfinal > 0:
        tfirst_str = str(qa.time(qa.quantity(tfirst,'s'), form='ymd')[0])
        tfinal_str = str(qa.time(qa.quantity(tfinal,'s'), form='ymd')[0])
        tfirst_val = strToTime(tfirst_str)
        tfinal_val = strToTime(tfinal_str)
        print('  tfirst: %%s (%%f)' %% (tfirst_str, tfirst_val))
        print('  tfinal: %%s (%%f)' %% (tfinal_str, tfinal_val))
    ''' % (key, caltab)

    if key == 'a':
        pass
    elif key == 'c':
        cs+='''
    if cnames != APPcols:
        ok += 1
        print('  Column names differ from APP standard (ERROR)')
    '''
    else:
        cs+='''
    if cnames != MScols:
        ok += 1
        print('  Column names differ from MS standard (ERROR)')
    '''
    return cs

def cloneTemplate(cpytab):
    '''
    Make a full copy with the new name.  The table should still
    be open from the previous check.
    '''
    cs='''
    cpy='%s'
    print('Making a copy to ' + cpy)
    tb.copy(cpy)
    ''' % (cpytab)
    return cs

def pruneTemplate(cpytab, beg, end):
    '''
    Add code to copy the calfile and extract the necessary rows.
    The table should be still be open from the previous check.
    We make an empty copy of the table, and then copy the rows.
    '''
    ps='''
    cpy='%s'
    beg='%s'    # beginning of the selection
    end='%s'    # end of the selected period
    print('Pruning data to   ' + cpy)
    print('original data was ' + tfirst_str+ ' to ' + tfinal_str)
    print('data requested is ' + beg + ' to ' + end)
    try:
        # tm_col is the time column; get needed indices
        # grab the rows with time > beg or the final value
        bg = indicesOfTime(strToTime(beg), tm_col, 'b', tfinal)
        # grab the rows with time < end or the first value
        ed = indicesOfTime(strToTime(end), tm_col, 'e', tfirst)
        try:
            # older numpy (casa 4.7...)
            be = np.intersect1d(bg,ed)
        except:
            # older numpy (casa 5/6)
            be = np.intersect1d(bg[0],ed[0])
        bs = np.sort(be)
        ck = set(be - bs)
        np.set_printoptions(threshold=8)
        print('  bg',bg)
        print('  ed',ed)
        print('  be',be)
        np.set_printoptions(threshold=100)
        print('nr',len(be),'ck',ck)
        if len(ck) != 1 and 0 != ck.pop():
            raise Exception('no good')
        tmsel = tb.selectrows(be)
        print('nr',tmsel.nrows())
        tcopy = tmsel.copy(cpy,deep=True)
        tmsel.close()
    except Exception as ex:
        print('ERROR: Problems with the process:  ',str(ex))
        ok += 1
    ''' % (cpytab, beg, end)
    return ps

def doneTemplate(key):
    '''
    Add code to close the table
    '''
    ds='''
    print('Done with %s')
    tb.close()
    sys.stdout.flush()
    ''' % (key)
    return ds

def duTemplate(key, qa2full, qa2copy):
    '''
    Report disk size per table for all tables
    CASA eats the output, so an alternative to system is needed.
    '''
    ut = '    #\n    print()\n    print("Disk Usage Summary")\n'
    for key in sorted(qa2full): # original keys
        ut += ("    os.system('du -shL %s > /dev/tty 2>&1')\n" %
            qa2full[key])
        if key in qa2copy:      # the copies
            ut += ("    os.system('du -shL %s > /dev/tty 2>&1')\n" %
                qa2copy[key])
    ut += '''

    print()
    # ok is a counter of ERROR issues
    if ok: print('ERROR status is a FAIL, (ok is %d)' % ok)
    else:  print('FINAL status is a PASS, (ok is %d)' % ok)
    quit()
    '''
    return ut

def getInputTemplate(copy, begin, end, qa2full, qa2copy, nuke):
    '''
    This is the input script with %-able adjustments.
    '''
    template='''    #!/usr/bin/python
    # This file contains python commands that may either be fed
    # directly to CASA as standard input, or else cut&pasted into
    # the interactive CASA prompts (which you should do if you
    # are having trouble or wish to see some of the plots).
    #
    import numpy as np
    import os
    import textwrap

    ok = 0  # actually !ok

    MScols = ['TIME', 'FIELD_ID', 'SPECTRAL_WINDOW_ID',
        'ANTENNA1', 'ANTENNA2', 'INTERVAL', 'SCAN_NUMBER',
        'OBSERVATION_ID', 'CPARAM', 'PARAMERR', 'FLAG', 'SNR', 'WEIGHT']

    APPcols = ['basebandName', 'scanNumber', 'calDataId', 'calReductionId',
        'startValidTime', 'endValidTime', 'adjustTime', 'adjustToken',
        'phasingMode', 'numPhasedAntennas', 'phasedAntennas',
        'refAntennaIndex', 'candRefAntennaIndex', 'phasePacking',
        'numReceptors', 'numChannels', 'numPhaseValues', 'phaseValues',
        'numCompare', 'numEfficiencies', 'compareArray',
        'efficiencyIndices', 'efficiencies', 'quality', 'phasedSumAntenna',
        'typeSupports', 'numSupports', 'phaseSupports']

    # a function that opens the table and extracts what we need
    def calopen(key,tab):
        tb.open(tab)
        cnames = tb.colnames()
        nmrows = tb.nrows()
        if key in 'bdgpxy':
            tcname = 'TIME'
        elif key in 'c':
            tcname = 'startValidTime'
        else:               # no time column in ANTENNA
            tcname = None
        if tcname == None or tb.isvarcol(tcname):
            tm_col = None
            tshape = None
            tfirst = 0.0
            tfinal = 0.0
        else:
            tm_col = tb.getcol(tcname) 
            tshape = tm_col.shape
            # what if these are not in order--sort to be sure?
            tm_sort = sorted(tm_col)
            tfirst = float(tm_sort[0])
            tfinal = float(tm_sort[-1])
        return cnames, nmrows, tcname, tshape, tfirst, tfinal, tm_col

    def strToTime(targ):
        tm = float(qa.convert(targ, 's')['value'])
        return tm

    def indicesOfTime(tm, tary, dir, targ):
        try:
            if dir == 'b': nz = np.nonzero(tary > tm)
            else:          nz = np.nonzero(tary < tm)
        except:
            print('Unable to get row indices')
        if len(nz[0]) == 0:
            nz = np.nonzero(tary == targ)
        return nz

    '''
    for key in sorted(qa2full):
        template += checkTemplate(key, qa2full[key])
        if key in qa2copy:
            if nuke and os.path.exists(qa2copy[key]):
                os.system('rm -rf ' + qa2copy[key])
                print('Nuked ', qa2copy[key])
            if key in 'abdxy':  # need the full table for these
                template += cloneTemplate(qa2copy[key])
            else:
                template += pruneTemplate(qa2copy[key], begin, end)
        template += doneTemplate(key)
    template += duTemplate(key, qa2full, qa2copy)
    return template

def createCasaInput(o, caldir, workdir):
    '''
    This function creates a file of python commands that can be piped
    directly into CASA.  It is now written to support both the single
    processing as well as the parallel processing case.
    '''
    oinput = workdir + '/' + o.input
    if o.verb: print('Creating CASA input file\n  ' + oinput)
    script = getInputTemplate(
        o.copy, o.begin, o.end, o.qa2_full, o.qa2_copy, o.nuke)
    # write the file, stripping initial indent
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
    if createCasaInput(o, '.', '.'):
        if o.verb: print('Created %s for single-threaded execution' % o.input)
    else:
        print('Problem creating',o.input)
        o.run = False

def executeCasa(o):
    '''
    This function pipes input to CASA and collects output.  The
    various developer debugging files (if present) are swept into
    the casa-logs directory (which is subsequently timestamped).
    '''
    misc = [ o.input + '.save', o.output + '.save' ]
    cmd1 = 'rm -f %s' % (o.output)
    cmd2 = '%s --nologger --nogui -c %s > %s 2>&1 < /dev/null' % (
        o.casa, o.input, o.output)
    cmd3 = '[ -d casa-logs ] || mkdir casa-logs'
    cmd4 = 'mv casa*.log ipython-*.log casa-logs 2>&-'
    cmd5 = 'mv %s %s casa-logs 2>&-' % (o.input, o.output)
    cmd6 = ' '
    casanow = o.exp + '-casa-logs.' + datetime.datetime.now().isoformat()[:-7]
    if o.run:
        if os.system(cmd1):
            raise Exception('That which cannot fail (rm -f), failed')
        if o.verb:
            print('Note, ^C will not stop CASA (or polconvert).')
            print('If it appears to hang, use kill -9 and then')
            print('"touch killcasa" to allow normal cleanup.')
            print('Follow CASA run with:\n  tail -n +1 -f %s\n' % (o.output))
        rc = os.system(cmd2)
        if rc:
            if os.path.exists('killcasa'):
                print('Removing killcasa')
                os.unlink('killcasa')
                print('Proceeding with remaining cleanup')
            else:
                raise Exception('CASA execution "failed" with code %d' % (rc))
        if o.verb:
            print('Success!  See %s for output' % o.output)
        logerr = False
        mscerr = False
        for m in misc:
            if os.path.exists(m):
                print('caching',m)
                cmd6 += '[ -f %s ] && mv %s casa-logs && echo %s ;' % (m,m,m)
        if os.system(cmd3 + ' ; ' + cmd4 + ' ; ' + cmd5):
            logerr = True
        if os.system(cmd6):
            mscerr = True
        if o.verb:
            print('Swept CASA logs to ' + casanow)
        if logerr: print('  There was a problem collecting CASA logs')
        if mscerr: print('  There was a problem collecting misc trash')
        os.rename('casa-logs', casanow)
    else:
        for m in misc:
            cmd6 += '[ -f %s ] && mv %s casa-logs ;' % (m,m)
        print('')
        print('You can run casa manually with input from ' + o.input)
        print('Or just do what this script would do now, viz: ')
        print('    ' + cmd1)
        print('    ' + cmd2 + ' &')
        print('    tail -n +1 -f ' + o.output)
        print('    ' + cmd3)
        print('    ' + cmd4)
        print('    ' + cmd5)
        print('    ' + cmd6)
        print('    mv casa-logs ' + casanow)
        print('')

#
# enter here to do the work
#
if __name__ == '__main__':
    opts = parseOptions()
    if len(opts.nargs) > 0:
        print('Extra arguments on command line: ' + str(opts.nargs))
        sys.exit(1)
    checkOptions(opts)
    createCasaInputSingle(opts)
    executeCasa(opts)
    # explicit 0 exit 
    sys.exit(0)

#
# eof
#
