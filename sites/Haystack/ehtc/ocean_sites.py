#!/usr/bin/env python
#
# Script to rip through a VEX file and generate input for 
# the ocean loading catalog
#
import re
import sys

def grok(vex):
    '''
    Look for SITE = and site_position = and return result
        ref $SITE = ...;
        site_position = ...;
    Not fully robust against anything stupid in the vex file.
    '''
    fv = open(vex)
    blck_re = re.compile(r'^\$SITE;')
    site_re = re.compile(r'^\s*site_name\s+=' +
        '\s*([^;]+);')
    spos_re = re.compile(r'^\s*site_position\s*=' +
        '\s*([-0-9.+]+)\s*m:\s*([-0-9.+]+)\s*m:\s*([-0-9.+]+)\s*m;')
    answers = {}
    station = None
    inside = False
    for liner in fv.readlines():
        line = liner.rstrip()
        blck_hit = blck_re.search(line)
        if blck_hit:
            inside = True
            continue;
        if inside:
            site_hit = site_re.search(line)
            if site_hit:
                station = site_hit.group(1)
            spos_hit = spos_re.search(line)
            if spos_hit:
                answers[station] = map(float, [
                    spos_hit.group(1), spos_hit.group(2), spos_hit.group(3)])
            if line[0] == '$':
                inside = False
    fv.close()
    return answers


# main entry point
if __name__ == '__main__':
    vex = sys.argv[1]
    print '//', vex
    nxyz = grok(vex)
    for station in sorted(nxyz.keys()):
        print "%-24s %16.3f%16.3f%16.3f" % (
            station, nxyz[station][0], nxyz[station][1], nxyz[station][2])

#
# eof
#
