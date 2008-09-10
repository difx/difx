#!/usr/bin/python
import os, sys

calcdbdir = os.environ.get('CALCDB')

dowait = True
if sys.argv > 1:
    dowait = False

utctai = open(calcdbdir + '/UTC-TAI.dat', 'r')
lines = utctai.readlines()

splitline = lines[-2].split()
latestsec = int(splitline[-1][:-1])

fordat = open(calcdbdir + '/dat.for', 'r')
lines = fordat.readlines()
datsec = 0
for line in lines:
    splitline = line.split()
    if len(splitline) == 3 and splitline[0] == 'IF':
        datsec = int(splitline[2][3:-2])

if latestsec == datsec:
    print "UTC-TAI.dat and dat.for agree the latest UTC-TAI value is " + str(latestsec)
else:
    print "WARNING!!!! dat.for seems to be out of date on the leap seconds."
    print "dat.for has the latest leap second as " + str(datsec)
    print "UTC-TAI has the latest leap second as " + str(latestsec)
    if dowait:
        raw_input("Don't press enter until you've updated dat.for, recompiled it *AND* recompiled gencalc_delays!!!")
    else:
        print("Don't press enter until you've updated dat.for, recompiled it *AND* recompiled gencalc_delays!!!")
