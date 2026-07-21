#!/usr/bin/python
#
# Script to verify that installed modules can be imported
#  For chops, this is $pythondir as configured.
#  For HOPS3, this is $pkgpythondir as configured.
# and for simplicity, we get these from the Makefile
#
# For the ease of the maintainer, this script checks its
# "personality" as configured (see configure.ac) and then
# does either the chops checks or the sub checks.
#
#-3.13++#from __future__ import absolute_import
#-3.13++#from __future__ import print_function
import sys
import os
import glob
import re

if 3 == 2:
    if "3.14" == '2.4': sys.exit(77)
    if "3.14" == '2.5': sys.exit(77)
    if "3.14" == '2.6': sys.exit(77)
import importlib

# undefined is configured to either 'chops' or 'sub'
personality = 'sub'
print('executing as the', personality, 'personality')
print('')

# Py3 makes use of PYTHONUSERBASE in the environment to allow
# installation into user areas.  Unfortunately, the time for
# manipulating the environment is past and we need to fix it now.
prefix = "/home/gbc/HOPS/x86_64-3.26"
pyprefix = re.sub(r'\${prefix}', prefix, "${prefix}")
sitedir = re.sub(r'\${PYTHON_PREFIX}', pyprefix, "${PYTHON_PREFIX}/lib/python3.14/site-packages")

# unmangle the parts from the autoconf substitutions
print('prefix is',prefix)
print('pyprefix is',pyprefix)
print('sitedir is',sitedir)
print()

if personality == 'chops':
    # abs path to installed location for os.exists
    pythondir = sitedir
    if 3 == 3:
        import site
        print('adding sitedir for imports from',sitedir)
        site.addsitedir(sitedir)
    print()
    packages = ['ffcontrol', 'hopstestb', 'mk4', 'vexpy', 'vpal']
    # mk4b is installed as mk4 and has two parts
    mk4_subs = ['afiob', 'mk4b']
    # vpal is imported by submodules:
    vpal_subs = [ 'vpal.ffres2pcp_lib', 'vpal.fourphase_lib',
        'vpal.fringe_file_manipulation', 'vpal.pcc_delay_fitting',
        'vpal.pcc_plotting_utils', 'vpal.phasecal', 'vpal.processing',
        'vpal.proxy_cable_cal', 'vpal.report_lib', 'vpal.utility'
    ]
    expected_count = len(vpal_subs) + len(mk4_subs) + len(packages) - 2
elif personality == 'sub':
    # abs path to installed location for os.exists
    pythondir = sitedir + '/hops'
    if 3 == 3:
        import site
        print('adding pythondir for imports from',pythondir)
        site.addsitedir(pythondir)
    print()
    packages = ['afio', 'difxio', 'hopstest', 'mk4']
    expected_count = len(packages)

def find_pkgs3(pkgs):
    mods = list()
    for p in pkgs:
        pdir = pythondir + '/' + p + '*' + '.egg'
        print('considering\n  ',pdir)
        crate = glob.glob(pdir)
        if len(crate) == 1:
            print('found module for', p, 'with:\n  ', crate[0])
            if p == 'mk4':
                for m in mk4_subs: mods.append(m)
            elif p == 'vpal':
                for m in vpal_subs: mods.append(m)
            else:
                mods.append(p)
        else:
            print('crate was',crate)
            print('found zero or many modules for', p)
    return mods, expected_count

def find_pkgs2(pkgs):
    mods = list()
    for p in pkgs:
        pdir = pythondir + '/' + p + '.py'
        if os.path.exists(pdir):
            print('found module for', p)
            mods.append(p)
        else:
            print('not finding module "' + p + '" in', pdir)
    return mods, expected_count

def run_checks(found):
    mods,ecnt = found
    errors = 0
    print('running checks for modules',mods)
    print('expecting count of modules',ecnt)
    print('actual count of modules is',len(mods))
    print()
    if len(mods) == 0:
        print('no modules installed, so skipping this test')
        sys.exit(77)
    elif ecnt < len(mods):
        errors += 1
    for m in mods:
        print('importlib.import_module(%s)'%m)
        try:
            mod = importlib.import_module(m)
            print('pass import of', mod.__name__,'\n  ',mod)
        except Exception as ex:
            print(str(ex))
            errors += 1
    print('errors is',errors)
    return errors

if sys.version_info.major == 3:
    print('checking for ',personality, 'packages in python version 3')
    if personality == 'chops':
        sys.exit(run_checks(find_pkgs3(packages)))
    else:
        sys.exit(run_checks(find_pkgs2(packages)))

else:
    print('checking for ',personality, 'packages in python version 2')
    sys.exit(run_checks(find_pkgs2(packages)))

# should not get here
sys.exit(-1)

#
# eof
#
