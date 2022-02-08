#!/usr/bin/python
#
# Script to modify input files so that difx2fits and difx2mark4
# will be happy with polconvert output.  As an option, the files
# may be moved to a new location.
#
# Py2/3 compatible via python-modernize and __future__ imports.
# PolConvert itself still requires a Py2 version of CASA (5.x)
#
'''
preconvert.py -- a program to prepare for the PolConvert step
'''

from __future__ import absolute_import
from __future__ import print_function
import argparse
import os
import re
import shutil
import sys

def parseOptions():
    '''
    This script is intended to be run after the DiFX correlation concludes,
    but prior to running post-processing scripts such as difx2fits or
    difx2mark4.  It saves the files from the original correlation and
    provides new versions with polarizations labels adjusted to reflect 
    such as would result after conversion by PolConvert (i.e. X->R and Y->L).
    '''
    des = parseOptions.__doc__
    epi = 'In standard use, the input files for the jobs to be processed '
    epi += 'are provided as the "nargs" positional arguments after the '
    epi += 'options.  It is recommended to use the destination directory '
    epi += 'option.  You will need to provide the VEX *.vex.obs file '
    epi += 'separately if you plan to use difx2mark4.'
    use = '%(prog)s [options] [input_file] [...]\n  Version'
    use += ' $Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    primary = parser.add_argument_group('Primary Options')
    secondy = parser.add_argument_group('Secondary Options')
    develop = parser.add_argument_group(
        'Development Options (not yet supported)')
    # options
    primary.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    primary.add_argument('-s', '--srcdir', dest='srcdir',
        metavar='DIR', default='.',
        help='source directory with DiFX output (.)')
    primary.add_argument('-d', '--dstdir', dest='dstdir',
        metavar='DIR', default='.',
        help='destination directory for PolConvert inputs (.)')
    secondy.add_argument('-k', '--clobber', dest='clobber',
        default=False, action='store_true',
        help='clobber destination files/dirs found')
    secondy.add_argument('-p', '--path', dest='path',
        metavar='STRING', default='', 
        help='the original (absolute) path for the directory that holds'
        ' the job files.  You need this if the files were copied from'
        '"path/" to "srcdir/" prior to executing this script.')
    secondy.add_argument('-l', '--suffices', dest='suffices',
        metavar='LIST', default='input,calc,flag,im,difx,save',
        help='comma-separated list of file/dir suffices ' +
             'to process, default is "input,calc,flag,im,difx,save"')
    secondy.add_argument('-o', '--orig', dest='orig',
        metavar='STRING', default='orig',
        help='suffix to be appended to original names')
    secondy.add_argument('-r', '--recbands', dest='updaterecpols',
        default=False, action='store_true',
        help='relabel not only ZOOM but also REC band X->R and Y->L')
    develop.add_argument('-e', '--exp', dest='exp',
        metavar='STRING', default='',
        help='VEX exper_name to allow import of exper_name.vex.obs')
    develop.add_argument('-j', '--jobs', dest='jobs',
        metavar='LIST', default='',
        help='a comma-sep list of job numbers ' +
             'or job from-to ranges (inclusive).  This requires -e ' +
             'to also be used, and there should then be no positional ' +
             'arguments.')
    # list of input files
    parser.add_argument('nargs', nargs='*',
        help='one or more DiFX job input files')
    return parser.parse_args()

def checkOptions(o):
    '''
    Check that options make sense, and other startup items.
    '''
    if o.jobs != '':
        raise Exception('Sorry, not yet supported')
    if o.exp != '':
        raise Exception('Sorry, not yet supported')
    # or else build a list of input files and put it in o.nargs
    if o.path != '':
        if o.path[0] != '/':
            raise Exception('the --path argument must be an absolute path')
    o.srcdir = os.path.abspath(o.srcdir)
    o.dstdir = os.path.abspath(o.dstdir)

def do_save(o, src, dst):
    '''
    Rename the src using the 'orig' suffix if src and dst are the same.
    '''
    if src == dst:
        tmp = src + '.' + o.orig
        if os.path.exists(tmp):
            if o.verb: print(('Backup copy already exists: ' + tmp))
        else:
            if os.path.exists(src):
                os.rename(src, tmp)
            else:
                if o.verb: print(('No source file to save: ' + src))
        src = tmp
    if os.path.exists(dst):
        if o.clobber:
            if os.path.isdir(dst):
                shutil.rmtree(dst)
            else:
                os.unlink(dst)
        else:
            raise Exception('refusing to overwrite %s with %s' % (dst, src))
    return src

def do_input(o, src, dst):
    'editing input %s\n    to output %s\n    to fix pols (X/Y->L/R) and paths'
    src = do_save(o, src, dst)
    if o.verb: print(do_input.__doc__ % (src,dst))
    inp = open(src, 'r')
    out = open(dst, 'w')
    for line in inp.readlines():
        if (re.search(r'ZOOM.*POL:\s+X$', line)):
            line = re.sub(r'X$', 'R', line)
        if (re.search(r'ZOOM.*POL:\s+Y$', line)):
            line = re.sub(r'Y$', 'L', line)
        if o.updaterecpols:
            if (re.search(r'REC.*POL:\s+X$', line)):
                line = re.sub(r'X$', 'R', line)
            if (re.search(r'REC.*POL:\s+Y$', line)):
                line = re.sub(r'Y$', 'L', line)
        if (re.search(r'.*FILENAME:', line) or
            re.search(r'.*VEX FILE:', line)):
            if o.path != '':
                line = re.sub(o.path, os.path.dirname(o.srcdir), line)
            line = re.sub(o.srcdir, o.dstdir, line)
        out.write(line)
    inp.close()
    out.close()

def do_pathfix(o, src, dst):
    'editing input %s\n    to output %s\n    to relocate paths'
    src = do_save(o, src, dst)
    if o.verb: print(do_pathfix.__doc__ % (src,dst))
    inp = open(src, 'r')
    out = open(dst, 'w')
    for line in inp.readlines():
        if (re.search(r'.*FILENAME:', line) or
            re.search(r'.*VEX FILE:', line)):
            if o.path != '':
                line = re.sub(o.path, os.path.dirname(o.srcdir), line)
            line = re.sub(o.srcdir, o.dstdir, line)
        out.write(line)
    inp.close()
    out.close()

def do_copy(o, src, dst):
    'copying %s  %s\n    to output %s\n    which is used unmodified'
    src = do_save(o, src, dst)
    if os.path.isfile(src):
        if o.verb: print(do_copy.__doc__ % ('file', src, dst))
        shutil.copy(src, dst)
        os.chmod(dst, 0o644)
    else:
        if o.verb: print(do_copy.__doc__ % (' dir', src, dst))
        shutil.copytree(src, dst)
        os.chmod(dst, 0o755)

#
# enter here to do the work
#
if __name__ == '__main__':
    o = parseOptions()
    checkOptions(o)
    for jobin in o.nargs:
        if o.verb: print('Copying files for job ' + jobin)
        else:      print(jobin)
        try:
            job,suf = jobin.split('.')
        except:
            raise Exception('This is not an input file: %s' % jobin)
        for suf in o.suffices.split(','):
            src = "%s/%s.%s" % (o.srcdir, job, suf)
            dst = "%s/%s.%s" % (o.dstdir, job, suf)
            if suf == 'input': do_input(o, src, dst)
            if suf == 'calc':  do_pathfix(o, src, dst)
            if suf == 'flag':  do_copy(o, src, dst)
            if suf == 'im':    do_pathfix(o, src, dst)
            if suf == 'difx':  do_copy(o, src, dst)
            if suf == 'save':  do_save(o, src, dst)

#
# eof
#
