#!/usr/bin/env python2
from __future__ import absolute_import
from __future__ import print_function
import os, sys

def parseBinConfigFile(binconffile):
    lines = open(binconffile).readlines()
    if len(lines) < 1:
        raise RunTimeError("binconfig file {0} is way too short, 1 line or less".format(binconffile))
    numpolycofiles = int(lines[0].split(':')[-1].strip())
    if len(lines) < 2+numpolycofiles:
        raise RunTimeError("binconfig file {0} is too short, not enough lines for num pulsar bins".format(binconffile))
    numbins = int(lines[1+numpolycofiles].split(':')[-1].strip())
    scrunchoutput = lines[2+numpolycofiles].split(':')[-1].strip().upper() == "TRUE"
    if not len(lines) >= numbins + numpolycofiles + 3:
        raise RunTimeError("binconfig file {0} is too short, not enough lines for all pulsar bins".format(binconffile))
    boundaries = []
    weights = []
    for i in range(numbins):
        boundaries.append(float(lines[numpolycofiles + 3 + i*2].split(':')[-1].strip()))
        weights.append(float(lines[numpolycofiles + 3 + i*2 + 1].split(':')[-1].strip()))
    widths = []
    for i in range(numbins):
        widths.append(boundaries[i] - boundaries[(i+numbins-1) % numbins])
    scales = []
    if scrunchoutput:
        scales.append(0.0)
        for i in range(numbins):
            scales[-1] += weights[i]*widths[i]
    else:
        for i in range(numbins):
            scales.append(widths[i])
    return scales

if __name__ == "__main__":
    if not len(sys.argv) == 3:
        print("Usage: {0} <FRB file> <RFI file>".format(sys.argv[0]))
        sys.exit()
    
    frbscale = parseBinConfigFile(sys.argv[1])
    rfiscale = parseBinConfigFile(sys.argv[2])
    if len(rfiscale) > 1:
        raise RunTimeError("RFI binconfigfile {0} did not have scrunch turned on")
    for i, scale in enumerate(frbscale):
        print("Bin {0}: scale {1}".format(i, scale/rfiscale[0]))
