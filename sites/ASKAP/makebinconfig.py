#!/usr/bin/env python2

# A script to write a binconfig file for use in correlating data with pulsar tools in DiFX

import os, sys

if len(sys.argv) != 6:
    print "usage: %s <start phase> <num bins> <step phase> <polyco name> <binconfig name>" % sys.argv[0]
    sys.exit()

if os.path.exists(sys.argv[5]):
    print sys.argv[5], "exists"
    sys.exit()

polyconame = sys.argv[4]
if polyconame[0] != '/':
    polyconame = os.getcwd() + '/' + polyconame

output = open(sys.argv[5], "w")
output.write("NUM POLYCO FILES:   1\n")
output.write("POLYCO FILE 0:      %s\n" % polyconame)
output.write("NUM PULSAR BINS:    %d\n" % int(sys.argv[2]))
output.write("SCRUNCH OUTPUT:     FALSE\n")
for i in range(int(sys.argv[2])):
    output.write("{line:<{width}}".format(line="BIN PHASE END {0:d}:".format(i), width='17') + "{line:>{width}}".format(line="{0:.9f}\n".format(float(sys.argv[1]) + i * float(sys.argv[3])), width='15'))
    output.write("{line:<{width}}".format(line="BIN WEIGHT {0:d}:".format(i), width='14') + "{line:>{width}}".format(line="1.0\n" , width='10'))
output.close()
