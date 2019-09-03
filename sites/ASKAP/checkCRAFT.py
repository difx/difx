#!/usr/bin/env python

from __future__ import print_function

import os


def checkBin(binary):
  ret = os.system("which {} > /dev/null".format(binary))
  if ret:
    print(" {}: Error - not found".format(binary))
  else:
    print(" {}: OK".format(binary))

def checkImport(module):
  try:
    __import__(module)
    print(" {}: OK".format(module))
  except ImportError:
    print(" {}: Failed to import".format(module))

print()
print("Checking Python Modules")
checkImport('parseDiFX')
checkImport('mx.DateTime')


print()
print("Checking DIFX binaries")
checkBin("errormon2")
checkBin("mpirun")
checkBin("mpifxcorr")
checkBin("vex2difx")
checkBin("difxcalc")
checkBin("difx2fits")
checkBin("getEOP.py")

print()
print("Checking DIFX ASKAP binaries")
checkBin("askap2difx.py")
checkBin("vcraft2obs.py")
checkBin("updatefreqs.py")
checkBin("mergeOversampledDiFX.py")

print()
print("Checking CRAFT binaries")
checkBin("bat0.pl")
checkBin("CRAFTConverter")

print()
print("Checking other binaries")
checkBin("tsp")
checkBin("sched")
