#!/usr/bin/python
# Cormac Reynolds, December 2011: almost trivial install script for espresso
import os, sys, shutil, optparse, re

usage = '''%prog <difxroot>
will install the scripts in <difxroot>/bin
'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
(options, args) = parser.parse_args()

difxroot = args[0]
if not difxroot:
    raise RuntimeError('DIFXROOT must be given')

difxbin = difxroot + os.sep + 'bin'

if not os.path.exists(difxbin):
    raise RuntimeError(difxbin + ' does not exist!')

dirlist = os.listdir(os.environ.get('PWD'))

for file in dirlist:
    if re.search('.py$', file) or re.search('.pl$', file) or re.search('.sh$', file):
        print file, difxbin
        shutil.copy2(file, difxbin)

