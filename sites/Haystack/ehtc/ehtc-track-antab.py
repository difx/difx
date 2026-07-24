#!/usr/bin/env python
#
'''
Script to generate a consolidated antab for the full track

This script locates all the ANTABs produced by polconvert,
assembles them in the order specified in the jobs map and
writes out a single antab.  If there are multiple polconvert
runs for a particular job, the most recent is used unless
the user supplies a preference.
'''
#
# Algorithmically, use the jobs file to get all the jobs
#   find the corresponding *polconvert*/*AA.ANTAB
#   resolve duplicates
#   verify time order
# write out header
# write out antab data
# 
# A minimal shell version looks like this...
#  cat *.polc*/*.ANTAB > tmp.antab
#  head -6 e21d15-1-b4_1057.polconvert-2022-06-28T04.41.40/\
#   POLCONVERT_STATION_AA.ANTAB > e21d15-1-b4-Aa.antab
#  grep -v GAIN tmp.antab | grep -v POLY | grep -v TSYS |\
#  grep -v INDEX | grep -v "^/" >> e21d15-1-b4-Aa.antab
#  echo "/" >> e21d15-1-b4-Aa.antab

from __future__ import absolute_import
from __future__ import print_function
import argparse
import glob
import os
import re
import sys

def parseOptions():
    des = __doc__
    epi = 'Typical usage requires simply the name of the output file.'
    use = '%(prog)s [options]'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    parser.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    parser.add_argument('-w', '--veryverbose', dest='very',
        default=False, action='store_true',
        help='be even more chatty about the work')
    parser.add_argument('-k', '--nuke', dest='nuke',
        default=False, action='store_true',
        help='delete the output antab file if found.  It is normally '
        'considered to be precious.')
    parser.add_argument('-a', '--antab', dest='antab',
        default='', help='output antab file')
    parser.add_argument('-j', '--jobsmap', dest='jobsmap',
        default='', help='explicit reference to the jobs map.  You may also '
        'use "all" for all antabs and "only" for just the ones that appear '
        'as arguments.  The jobs map name is derived from the antab output '
        'file name.')
    parser.add_argument('-m', '--minsize', dest='minsize',
        default=432, type=int,
        help='smallest ANTAB with content (432); files smaller than this '
            'will not be used as they just contain a header')
    parser.add_argument('nargs', nargs='*',
        help='list of polconvert ANTAB product files--antabs in this '
            'list will be used in preference to peer-job antabs if there are '
            'conflicts (jobs-map and all cases).  A conflict refers to two '
            'polconvert jobs for the same underlying job number.  In the '
            '"only" case, just use these files.')
    return parser.parse_args()

def getEvs(antab):
    '''
    construct the job prefix (glob)
    '''
    ersplit = re.sub(r'-AA.antab', '', antab).split('-')
    if len(ersplit) > 2:
        evs = ersplit[0] + '-*-' + ersplit[2]
    else:
        evs = ersplit[0]
    return evs

def getJnum(antab):
    '''
    construct the job number from an antab--we use this as a key
    into the list of possible conflicts
    '''
    try:
        jnp = antab.split('_')
        jnum = re.sub(r'.polconvert-.*', '', jnp[1])
        # make sure it looks like an integer
        jflt = int(jnum)
    except Exception as ex:
        print('unable to parse',antab,'for job number',jflt)
        print(str(ex))
        jnum = 'no-such-job'
    return jnum

def createConflict(o):
    '''
    When nargs is populated, and we are not in the only case, we need to
    construct a dictionary of job numbers that might be conflicted.
    Note that if you list a job multiple times (which is stupid) the
    last one mentioned will silently win.
    '''
    cfd = dict()
    if o.jobsmap == 'only' or len(o.nargs) == 0:
        if o.verb: print('no conflicts on "only" or no args')
        return cfd
    evs = getEvs(o.antab)
    for ant in o.nargs:
        jnum = getJnum(ant)
        patt = evs + '_' + jnum + '.polconvert-.*'
        if o.very: print('(conflicts) searching for files',patt)
        for oa in glob.glob(patt):
            if oa == ant:
                cfd[jnum] = ant
            # otherwise it won't get used
    if o.verb: print('created dict of %d conflicts'%len(cfd))
    return cfd

def locateANTABsByAll(o):
    '''
    Implement the 'all' case, do a glob for all possible files.
    We reconstruct the job prefix from the antab.
    '''
    evs = getEvs(o.antab)
    if o.verb:
        print('looking for ANTABS ("all" case) evs=',evs)
    patt = evs + '_[0-9]*.polconvert-*'
    if o.very: print('(by "all") searching for files',patt)
    for pcd in sorted(glob.glob(patt)):
        ant = pcd + '/POLCONVERT_STATION_AA.ANTAB'
        jnum = getJnum(ant)
        if os.path.exists(ant) and os.path.getsize(ant) > o.minsize:
            # add to list only if it is not conflicted
            if not jnum in o.conflict or o.conflict[jnum] == ant:
                o.files.append(ant)

def locateANTABsByOnly(o):
    '''
    Implement the 'only' case, just process what is in o.nargs.
    Check that the files actually exist so we do not have to worry
    about that later.
    '''
    if o.verb:
        print('looking for ANTABS ("only" case)')
    for antab in sorted(o.nargs):
        if os.path.exists(ant) and os.path.getsize(ant) > o.minsize:
            # there are no conflicts to resolve
            o.files.append(antab)

def locateANTABsMyJobMap(o):
    '''
    Find ANTABs via the jobs map.  If o.nargs includes some files,
    these override any outputs of the same name.
    '''
    evs = getEvs(o.antab)
    if o.verb:
        print('looking for ANTABS using job map %s, evs=%s' %(o.jobsmap,evs))
    fj = open(o.jobsmap, 'r')
    for line in fj.readlines():
        parts = line.split()
        patt = evs + '_' + parts[0] + '.polconvert-*'
        if o.very: print('(by job map) searching for files:',patt)
        for pcd in sorted(glob.glob(patt)):
            ant = pcd + '/POLCONVERT_STATION_AA.ANTAB'
            jnum = getJnum(ant)
            if os.path.exists(ant) and os.path.getsize(ant) > o.minsize:
                # add to list only if it is not conflicted
                if not jnum in o.conflict or o.conflict[jnum] == ant:
                    o.files.append(ant)
    fj.close()

def getFinalHeader(o):
    '''
    Read all the file headers and check that they are the same.
    We are going to open all these files anyway, so the kernel
    might as well get started reading them.
    '''
    hdr = ''
    for ant in o.files:
        fa = open(ant, 'r')
        hdx = ''
        for ll in range(6):
            hdx += fa.readline()
        if hdr == '' and len(hdx) > 0:
            hdr = hdx
            if o.verb: print('header has %d bytes',len(hdr))
            if o.very: print('header is\n'+'#'*80+'\n'+hdx+'#'*80)
        if hdr != hdx:
            print("Warning, deviant header (%d != %d) found in"
                % (len(hdr),len(hdx)))
            print('  ', ant)
            if o.very: print('header is\n'+'#'*80+'\n'+hdx+'#'*80)
        fa.close()
    return hdr

def writeFinalANTAB(fo, o):
    '''
    Write out the data part (lines 7-end) in the files.
    '''
    for ant in o.files:
        fa = open(ant, 'r')
        data = fa.readlines()
        fa.close()
        fo.write(''.join(data[6:-1]))
        if o.verb: print('wrote %d lines from %s' % (len(data)-6,ant))
    fo.write('/\n')

def deduceJobsMap(antab, verb):
    '''
    The output antab is normally $ers-AA.antab, the jobs map is normally
    $ers-jobs-map.txt.  Note that polconvert jobs are $evs (which uses $vers
    instead of $relv).
    '''
    jm = re.sub(r'-AA.antab', '-jobs-map.txt', antab)
    if verb: print('Deduced jobs map is', jm)
    if os.path.exists(jm):
        return jm
    raise Exception('Unable to deduce jobs-map from output filename')

def idiotChecks(o):
    '''
    Review the options and emerge ready to work
    '''
    if o.antab == '':
        raise Exception('You need to supply an output filename via -a')
    if os.path.exists(o.antab) and not o.nuke:
        raise Exception('ANTAB %s exists and -k is not set' % o.antab)
    if o.jobsmap == 'all' or o.jobsmap == 'only':
        if o.verb: print("proceeding with '%s' case" % o.jobsmap)
        return
    if o.jobsmap == '':
        o.jobsmap = deduceJobsMap(o.antab, o.verb)
    if not os.path.exists(o.jobsmap) and not o.jobsmap is None:
        raise Exception('Jobs map %s not found' % o.jobsmap)
    if o.verb: print('assembling ANTAB from job map list')
    if o.very:
        o.verb = True
        print('very verbose output selected')

#
# enter here to do the work
#
if __name__ == '__main__':
    o = parseOptions()
    idiotChecks(o)
    o.files = list()
    o.conflict = createConflict(o)
    if o.jobsmap == 'all':
        locateANTABsByAll(o)
    elif o.jobsmap == 'only':
        locateANTABsByOnly(o)
    else:
        locateANTABsMyJobMap(o)
    if len(o.files) > 0:
        fo = open(o.antab, 'w')
        fo.write(getFinalHeader(o))
        writeFinalANTAB(fo, o)
        fo.close()
    else:
        raise Exception('No ANTAB files found, nothing written')
    print('wrote',o.antab)

#
# eof vim: nospell
#
