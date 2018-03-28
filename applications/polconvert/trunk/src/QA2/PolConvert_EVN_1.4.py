# POLCONVERT_EVN - Part of the PolConvert Package
# Script for the calibration of VLBI antennas with linear feeds 
# in mixed-polarization observations. Tested with EVN observations
# having Effelsberg with a linear-feed receiver.
#
# The script uses PolConvert to estimate the XY-phase, X/Y amplitude
# ratio, and X-Y multiband delay, from the scan of a strong calibrator. 
# All these gains are then pre-applied to all data before conversion in
# a second PolConvert run, and new FITS-IDI files are generated.
######
import pickle as pk

REFANT = 4 # Antenna to which refer the conversion gain solution (O8)
LINANT = 2 # Antenna with linear feed (EB)

REF_IDI = 'eo014_1_1.IDI6' # IDI with calibrator scan
CALRANGE = [0,23,28,0,0,23,39,45] # time range of calibrator scan (J0927+3902)
NCHAN = 32 # Number of channels per IF (to compute the multi-band delay)
NIF = 8  # Number of IFs.

NITER = 1  # Number of X-Y phase-estimate iterations (just 1 should suffice)

# List with the names of all FITS-IDIs to convert:
ALL_IDIs = ['eo014_1_1.IDI6']

# Add this suffix to the converted IDIs (empty string -> overwrite!)
SUFFIX = '.POLCONVERT'





import os
import numpy as np


# Initial gain estimates (dummy gains):
EndGainsAmp = [1.0 for i in range(NIF)]
EndGainsPhase = [0.0 for i in range(NIF)]
TotGains = []


# Estimate cross-gains with PolConvert:
for i in range(NITER):


##################################
# Convert XYadd and XYratio to lists, in order
# to avoid corruption of the *.last file
  if i==0:
    XYadd = [EndGainsPhase]
    XYratio = [EndGainsAmp]
  else:
    XYadd = [[list(ll) for ll in EndGainsPhase]]
    XYratio = [[list(ll) for ll in EndGainsAmp]]
##################################



  polconvert(IDI=REF_IDI,
             OUTPUTIDI=REF_IDI,
             linAntIdx=[LINANT],
             plotIF = [],
             doIF = [],
             XYadd = XYadd,
             XYratio = XYratio,
             Range = CALRANGE,
             plotRange = CALRANGE,
             IDI_conjugated = False,
             doSolve = 1000.,   #4,
             solint = [1,1],  #BP MODE
             plotAnt = REFANT,
             amp_norm = 0.0,
             solveMethod = 'COBYLA',
             excludeAnts = [8,9],
             doTest=True)
  
  ifile = open('PolConvert.XYGains.dat')
  GainsIt = pk.load(ifile)
  ifile.close()

  TotGains.append(GainsIt)


  for k in range(NIF):
    EndGainsAmp[k] = EndGainsAmp[k]*np.array(GainsIt['XYratio'][LINANT][k])
    EndGainsPhase[k] = EndGainsPhase[k] + np.array(GainsIt['XYadd'][LINANT][k])

  os.system('rm -rf FRINGE.PLOTS.ITER%i'%i)
  os.system('mv FRINGE.PLOTS FRINGE.PLOTS.ITER%i'%i)
  os.system('mv Cross-Gains.png Cross-Gains.ITER%i.png'%i)



# HERE WE CAN CONVERT ALL IDIs:
if False:
 XYadd = [[list(ll) for ll in EndGainsAmp]]
 XYratio = [[list(ll) for ll in EndGainsPhase]]

 for IDI in ALL_IDIs:
  polconvert(IDI=IDI,
             OUTPUTIDI=IDI+SUFFIX,
             linAntIdx=[LINANT],
             plotIF = [],
             XYadd = [list(Phases)],
             XYdel = [multiBand],
             IDI_conjugated = False,
             XYratio = [AmpRat],
             amp_norm = 0.0,
             doTest=False)
