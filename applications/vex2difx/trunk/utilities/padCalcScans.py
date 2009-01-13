#!/usr/bin/python

import sys, os

if not len(sys.argv) == 2:
    print "Usage: padCalcScans <calcfile>"
    sys.exit()

calcfile = sys.argv[1]

if not os.path.exists(calcfile):
    print "Supplied .calc file " + calcfile + " does not exist - aborting"
    sys.exit()

os.system("mv " + calcfile + " " + calcfile + ".org")

#Read the calc file
calcin = open(calcfile + ".org")
calclines = calcin.readlines()
calcin.close()

#Open a new calc file and fill it
calcout = open(calcfile, 'w')
at = 0
tag = calclines[at].split(':')[0]
while not tag == "NUM SCANS":
    calcout.write(calclines[at])
    at = at + 1
    tag = calclines[at].split(':')[0]
numscans = int((calclines[at].split(':')[1]).lstrip())
scanstarts = []
for i in range(numscans):
    scanstarts.append(int((calclines[at+2+i*8].split(':')[1]).lstrip()))
scanstarts.append(int((calclines[at+2+(numscans-1)*8].split(':')[1]).lstrip()) +
                  int((calclines[at+1+(numscans-1)*8].split(':')[1]).lstrip()))
calcout.write(calclines[at])
at = at+1
for i in range(numscans):
    calcout.write(calclines[at+8*i][:20] + ("%i" % (scanstarts[i+1] - scanstarts[i])) + "\n")
    calcout.write(calclines[at+8*i+1])
    calcout.write(calclines[at+8*i+2])
    calcout.write(calclines[at+8*i+3])
    calcout.write(calclines[at+8*i+4])
    calcout.write(calclines[at+8*i+5])
    calcout.write(calclines[at+8*i+6])
    calcout.write(calclines[at+8*i+7])
for i in range(len(calclines) - (at+8*numscans)):
    calcout.write(calclines[at+8*numscans+i])
calcout.close()
