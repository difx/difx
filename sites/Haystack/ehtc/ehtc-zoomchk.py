#!/usr/bin/python
#
# Script to check on zoom band uniformity across a set of jobs
#
'''
ehtc-zoomchk.py -- a program to check zoom band uniformity across jobs
'''

import argparse
import re

def parseOptions():
    '''
    This is a helper program for drivepolconvert.  The working assumption
    is that all jobs share the same zoom band setup; however, sometimes
    this is not the case.  This program provides a quick check to allow
    the drivepolconvert to be broken into several runs if necessary.
    '''
    des = parseOptions.__doc__
    epi = ''
    use = '%(prog)s [options] [input_file [...]]\n  Version'
    use += '$Id: ehtc-zoomchk.py 1888 2017-07-21 20:21:47Z gbc $'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    # essential options
    parser.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    parser.add_argument('-a', '--ant', dest='ant',
        default=1, metavar='INT', type=int,
        help='1-based index of linear (ALMA) antenna (normally 1)')
    # the remaining arguments provide the list of input files
    parser.add_argument('nargs', nargs='*',
        help='List of DiFX input job files')
    return parser.parse_args()

def deduceZoomIndicies(o):
    '''
    Pull the Zoom frequency indicies from the input files and check
    that all input files produce the same first and last values.
    '''
    zoompatt = r'^ZOOM.FREQ.INDEX.\d+:\s*(\d+)'
    almapatt = r'^TELESCOPE NAME %d:\s*AA' % (o.ant-1)
    amap_re = re.compile(r'^TELESCOPE NAME\s*([0-9])+:\s*([A-Z0-9][A-Z0-9])')
    freqpatt = r'^FREQ..MHZ..\d+:\s*(\d+)'
    zfirst = set()
    zfinal = set()
    mfqlst = set()
    zoomys = set()
    antmap = {}
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
            amap = amap_re.search(line)
            if amap:
                antmap[amap.group(2)] = int(amap.group(1))
        ji.close()
        zoomys.add('%s..%s' % (zfir, zfin))
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
    if len(zoomys) > 1:
        print 'Multiple zoom ranges seen:',
        for z in zoomys: print z
        print 'You will need to subdivide the job list into portions'
        print 'that share the same zoom range.  Rerun with -v to see'
        print 'the list and work it out manually'
    else:
        print 'All jobs are compatible with the same zoom range:',
        for z in zoomys: print z

#
# enter here to do the work
#
if __name__ == '__main__':
    o = parseOptions()
    deduceZoomIndicies(o)

#
# eof
#
