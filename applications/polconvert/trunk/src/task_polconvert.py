# Copyright (c) Ivan Marti-Vidal 2012. 
#               EU ALMA Regional Center. Nordic node.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>,
# or write to the Free Software Foundation, Inc., 
# 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# a. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# b. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
# c. Neither the name of the author nor the names of contributors may 
#    be used to endorse or promote products derived from this software 
#    without specific prior written permission.
#
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
#

__version__ = "1.2-r8"
date = 'AUG 2016'     


################
# Import all necessary modules. 

########
# Execute twice, to avoid the silly (and harmless) 
# error regarding the different API versions of 
# numpy between the local system and CASA:
try: 
 import _PolConvert as PC
 goodclib = True
 print '\nC++ shared library loaded successfully\n'
except:
 goodclib=False
 print '\n There has been an error related to the numpy' 
 print ' API version used by CASA. This is related to PolConvert'
 print ' (which uses the API version of your system) and should' 
 print ' be *harmless*.\n'

if not goodclib:
  try: 
   import _PolConvert as PC
   goodclib = True
   print '\nC++ shared library loaded successfully\n'
  except:
   goodclib=False
#############

import os,sys,shutil
import time
import struct as stk
import numpy as np
import pylab as pl
import datetime as dt
from taskinit import *
ms = gentools(['ms'])[0]
tb = gentools(['tb'])[0]




#########################################################
###########################
# THESE LINES ARE JUST FOR TESTING AND DEBUGGING.
# NOT EXECUTED IF THE FILE IS LOADED AS A MODULE:

if __name__=='__main__':



  IDI                 = './bm452c_135.save' #  Input FITS-IDI file with VLBI visibilities. It can  also be a
                                        #   direcotry containing SWIN files from DiFX.
  OUTPUTIDI           = './bm452c_135.difx' #  Output FITS-IDI file (or SWIN directory). If equal  to IDI, the
                                        #   file(s) will be overwritten
  DiFXinput           = './bm452c_135.input' #  If SWIN files are being converted, this must  be the *.input
                                        #   file used by DiFX.
  doIF                =       [37]        #  List of IFs to convert. Default means all.
  linAntIdx           =        [1]        #  List of indices of the linear-polarization  antennas in the IDI
                                        #   file
  Range               =         []        #  Time range to convert (integer list; AIPS format).  Default means
                                        #   all data
  ALMAant             = 'uid___A002_Xb542b2.concatenated.ms.ANTENNA' #  If ALMA has been used, this is the
                                        #   antenna table  from the MS with the intra-ALMA visibilities.
  spw                 =          2        #  Spectral window in ALMAvis that contains the VLBI band.
  calAPP              = 'uid___A002_Xb542b2.calappphase' #  If ALMA has been used, this is the combined
                                        #   ASDM_CALAPPPHASE table from the ASDM. The list of measurement
                                        #   sets can also be given (so the table is concatenated from all of
                                        #   them).
  calAPPTime          = [0.0, 8.0]        #  Time shift and time tolerance (in sec) for the  CALAPPPHASE table
                                        #   obtained from the ASDM.
  gains               = [['uid___A002_Xb542b2.concatenated.ms.bandpass-zphs', 'uid___A002_Xb542b2.concatenated.ms.flux_inf', 'uid___A002_Xb542b2.concatenated.ms.phase_int.APP', 'uid___A002_Xb542b2.concatenated.ms.XY0.APP']] #  G
                                        #   ain tables to pre-calibrate the linear-pol VLBI  stations (one
                                        #   list of gains per linear-pol station).
  dterms              =   ['uid___A002_Xb542b2.concatenated.ms.Df0']        #  D-term tables to pre-calibrate the linear-pol VLBI  stations (one
                                        #   table per linear-pol station).
  amp_norm            =       True        #  If True, normalize the amplitude correction to the X-Y average,
                                        #   and save the scaling factor (vs time) in an external (ASCII)
                                        #   file. If False, apply the amplitude correction as is.
  XYadd               =      [0.0]        #  Add manually a phase between X and Y before  conversion (in
                                        #   deg.). One value per linear-pol station.
  swapXY              =    [False]        #  Swap X-Y before conversion. One value per linear-pol  VLBI
                                        #   station.
  swapRL              =      False        #  Swap R-L of the OTHER antenna(s) when plotting the fringes.
  IDI_conjugated      =       True        #  Assume a swap in the baseline defintion (i.e., conjugation) of
                                        #   the FITS-IDI file. This has NO effect on SWIN files and shall be
                                        #   set to True.
  plotIF              =         37        #  IF index to plot. Default means to NOT plot.
  plotRange           = [0, 0, 0, 0, 1, 0, 0, 0] #  Time range to plot (integer list; AIPS format).  Default
                                        #   means to NOT plot
  plotAnt             =          2        #  Index of the other antenna in the baseline to plot. Default means
                                        #   to NOT plot.
  doTest              =       True        #  If true, only compute (and eventually plot), the data, but leave
                                        #   OUTPUTIDI untouched.

#
#
#
###########################
#########################################################





############################################
# COMMENT OUT THIS LINE WHEN DEBUGGING
# YOU SHALL THEN RUN THIS FILE WITH "execfile(...)"

def polconvert(IDI, OUTPUTIDI, DiFXinput, doIF, linAntIdx, Range, ALMAant, spw, calAPP, calAPPTime, gains, dterms, amp_norm, XYadd, swapXY, swapRL, IDI_conjugated, plotIF, plotRange, plotAnt,doTest):

############################################


# Auxiliary function: print error and raise exception:
  def printError(msg):
    print msg,'\n' 
    casalog.post('PolConvert: '+msg+'\n')
    raise Exception(msg)


# Auxiliary function: print message (terminal and log):
  def printMsg(msg, doterm=True, dolog=True):
    if doterm:
      print msg
    if dolog:
      casalog.post('PolConvert: '+msg)


# Auxiliary function: unwrap phases for time interpolation
  def unwrap(phases):
    if len(np.shape(phases))==1:
     for i in range(len(phases)-1):
      if phases[i+1]-phases[i] > np.pi:
        phases[i+1,:] -= 2.*np.pi
      elif phases[i+1]-phases[i] < np.pi:
        phases[i+1,:] += 2.*np.pi


# Auxiliary function: prepare the CALAPPPHASE, from a set of MSs:
  def makeCalAPP(mslist):
    printMsg('Will create CALAPPPHASE table from measurement sets.')
    os.system('rm -rf ./CALAPPPHASE.tab')
    for i,asd in enumerate(mslist):
      printMsg('Working out MS #%i - %s'%(i+1,asd))
      if i==0:
       os.system('cp -rf %s/ASDM_CALAPPPHASE ./CALAPPPHASE.tab'%asd)
      else:
       tb.open(asd+'/ASDM_CALAPPPHASE')
       tb.copyrows('./CALAPPPHASE.tab')
       tb.close()

    tb.open(asd+'/ASDM_CALAPPPHASE')
    time0 = tb.getcol('startValidTime')
    time1 = tb.getcol('endValidTime')
    tb.close()
    tb.open(asd)
    mstime = tb.getcol('TIME')
    tb.close()
    if len(time0)==0:
      printMsg('WARNING! NO APPCAL DATA FOR %s\n'%asd)
    else:
      print '\n\nMEAS SET %s:'%asd
      tmin = np.min(time0)/86400.
      tmax = np.max(time1)/86400.
    mstmin = np.min(mstime)/86400.
    mstmax = np.max(mstime)/86400.
    printMsg('MS TIME RUNS FROM %8.5f TO %8.5f DAYS'%(mstmin,mstmax))
    if len(time0)>0:
     printMsg('FOR APP TABLE, TIME RUNS FROM %8.5f TO %8.5f DAYS'%(tmin,tmax))





  tic = time.time()



  greetings  =   ' ##########################################################################\n'
  greetings +=   ' # POLCONVERT --  '+date+'. EUROPEAN ALMA REGIONAL CENTER (NORDIC NODE).  #\n'
  greetings +=   ' #       Please, add the POLCONVERT reference to your publications:       #\n'
  greetings +=   ' #                                                                        #\n'
  greetings +=   ' #          Marti-Vidal, Roy, Conway & Zensus 2016, A&A, 587, 143         #\n'
  greetings +=   ' #                                                                        #\n'
  greetings +=   ' ##########################################################################\n\n'


  printMsg(greetings,dolog=False)
  printMsg('\n\nPOLCONVERT - VERSION %s  - Nordic ARC Node'%__version__, doterm=False)


#########################################
# DO SOME SANITY CHECKS OF PARAMETERS
  
  doConj = True
  if type(IDI_conjugated) is not bool:
    printError("ERROR! IDI_cojugated should be a boolean!")
  else:
    doConj = IDI_conjugated

  if type(swapRL) is not bool:
    printError("ERROR! swapRL should be a boolean!")


  if type(doIF) is not list:
    printError("ERROR! doIF should be a list of integers!")
  else:
    for elem in doIF:
      if type(elem) is not int:
        printError("ERROR! doIF should be a list of integers!")


  if type(doTest) is not bool:
    printError("ERROR! doTest should be a boolean!")

  if doTest:
    printMsg("Will only compute, but not update the output file(s)",doterm=False)

  if type(linAntIdx) is not list:
    printError("ERROR! linAntIdx should be a list of integers!")
  for elem in linAntIdx:
    if type(elem) is not int:
      printError("ERROR! linAntIdx should be a list of integers!")


  nALMA = len(linAntIdx)


  if not os.path.exists(IDI):
    printError("ERROR! IDI file (or SWIN folder) does not exist!")


  if type(calAPP) is list:
    try:
      makeCalAPP(calAPP)
      calAPP = './CALAPPPHASE.tab'
    except:
      printError('ERROR: Could not create nor interprete the CALAPPPHASE table!')
  

  c0 = len(calAPP) == 0 ; c1 = len(ALMAant) == 0
  if c0 != c1: 
    printError("ERROR! either both or none of calAPP and ALMAant need to be set!")

  if c0:
    printMsg("WARNING: calAPP and ALMAant are not set.\nALMA DID NOT SEEM TO BE USED, ACCORDING TO USER\n\n")
    isPhased=False
  else:
    if not os.path.exists(calAPP) or not os.path.exists(ALMAant):
      printError("ERROR! calAPP and/or ALMAant WERE NOT FOUND!")
    else:
      isPhased = True



  if type(OUTPUTIDI) is not str:
    printError("ERROR! OUTPUTIDI should be a string!")

  if type(plotIF) is not int:
    printError("ERROR! plotIF should be an integer!")

  try:
    plotIF = int(plotIF)
  except:
    printError("ERROR! plotAnt should be an integer!")

  try:
    plotAnt = int(plotAnt)
  except:
    printError("ERROR! plotAnt should be an integer!")

  try:
    spw = int(spw)
  except:
    printError("ERROR! spw should be an integer!")



  if len(gains)!= nALMA or len(dterms)!= nALMA:
    printError("Invalid format for gains and/or dterms!\n Should be lists as large as the number of linear-polarization VLBI stations!")
  
  for g in gains:
    if type(g) is not list:
      printError("Invalid format for gains!\n Each element should be a list with the names of the gain tables for the ith linear-pol. VLBI station") 
    else:
      for elem in g:
        if type(elem) is not str:
          printError("Invalid format for gains!\n Each element (of each element) should be a string (the name of a calibration table)") 
    for elem in dterms:
      if type(elem) is not str:
        printError("Invalid format for dterms!\n Each element should be a string (the name of a calibration table)") 

  if type(XYadd) is not list or len(XYadd) != nALMA:
    printError("Invalid format for XYadd!\n Should be a list of integers, as large as the number of linear-polarization VLBI stations!")
    for i in range(len(XYadd)):
      try:
        XYadd[i] = float(XYadd[i])
      except:
       printError("Invalid format for XYadd!\n Should be a list of numbers, as large as the number of linear-polarization VLBI stations!")


  if len(swapXY) != nALMA:
    printError("Invalid format for swapXY!\n Should be a list of booleans, as large as the number of linear-polarization VLBI stations!")
    for sxy in swapXY:
      if type(xya) is not bool:
       printError("Invalid format for swapXY!\n Should be a list of booleans, as large as the number of linear-polarization VLBI stations!")


  if len(Range) not in [0,8]:
    printError("Invalid format for Range! Should be either an empty list or a list of 8 integers!")
  for rr in Range:
    if type(rr) is not int:
      printError("Invalid format for Range! Should be either an empty list or a list of 8 integers!")

  if len(plotRange) not in [0,8]:
    printError("Invalid format for Range! Should be either an empty list or a list of 8 integers!")
  for rr in plotRange:
    if type(rr) is not int:
      printError("Invalid format for Range! Should be either an empty list or a list of 8 integers!")


  if len(calAPPTime) != 2:
    printError("Bad format for calAPPTime. Should be a list of 2 floats!")

  try:
    CALAPPTSHIFT, CALAPPDT = [float(cc) for cc in calAPPTime]
  except:
    printError("Bad format for calAPPTime. Should be a list of 2 floats!")


  if plotAnt in linAntIdx:
    printMsg("WARNING: Plotting may involve autocorrelations. \nThis has not been fully tested!") 

#########################################




######################
# READ IDs OF ALMA ANTENNAS USED IN THE PHASING:

  if isPhased:


   success = tb.open(calAPP)
   if not success:
    printError('ERROR: INVALID calAPP TABLE!')
    
   try:
    time0 = tb.getcol('startValidTime')-CALAPPDT+CALAPPTSHIFT
    time1 = tb.getcol('endValidTime')+CALAPPDT+CALAPPTSHIFT
    nphant = tb.getcol('numPhasedAntennas')
    refs = tb.getcol('refAntennaIndex')
    asdmtimes = [time0,time1]
    phants = []
    for i in range(len(nphant)):
      phants.append(list(tb.getcell('phasedAntennas', rownr = i)))

    tb.close()

   except:
    printError('ERROR: INVALID calAPP TABLE!')

   success = tb.open(ALMAant)
   if not success:
    printError('ERROR: NO VALID ANTENNA TABLE FOUND!')

   allants = list(tb.getcol('NAME'))
   allantidx = range(len(allants))
   tb.close()

   refants = np.array([allants.index(phants[t][refs[t]]) for t in range(len(phants))])

   antimes = [[[0.,0.]] for ian in allants]
   for ian,antenna in enumerate(allants):
    for j,phan in enumerate(phants):
      if antenna in phan:
        antimes[ian].append([time0[j]-CALAPPDT+CALAPPTSHIFT,time1[j]+CALAPPDT+CALAPPTSHIFT])

    auxarr = np.zeros((len(antimes[ian]),2))
    auxarr[:] = antimes[ian]
    antimes[ian] = auxarr
    
   nphtimes = [len(anti) for anti in antimes]

  else:

# Not a phased array. Dummy values for phasing info:
   allants = [0]
   allantidx = [0]
   antimes = [np.array([0.,1.e20])]
   nphtimes = [1]
   refants = np.array([0],dtype=np.int)
   asdmtimes = [np.array([0.]),np.array([1.e20])]

######################



#######
# CHECK IF THIS IS A FITS-IDI FILE OR A SWIN DIR.:
  if os.path.isdir(IDI):
    isSWIN = True
    printMsg('\n\nYou have asked to convert a set of SWIN files.')
    if len(DiFXinput)==0 or not os.path.exists(DiFXinput) or not os.path.isfile(DiFXinput):
      printError("Invalid DiFX input file!") 
  elif os.path.isfile(IDI):
    isSWIN = False
    printMsg('\n\nYou have asked to convert a FITS-IDI file.')
  else:
    printError("Invalid input data!") 


######


######
# IF THIS IS A SWIN DATASET, READ THE INPUT FILE INTO 
# A METADATA LIST:

  if isSWIN:
    printMsg('Reading the DiFX input file\n')
    ifile = open(DiFXinput)
    inputlines = ifile.readlines()
    ifile.close()
    FreqL = [inputlines.index(l) for l in inputlines if 'FREQ TABLE' in l]

# ONLY ONE FREQ TABLE IS ALLOWED:
    try:
      fr = FreqL[0]
      Nfreq = int((filter(lambda x: 'FREQ ENTRIES' in x, inputlines[fr+1:])[0]).split()[-1])
      Nr = range(Nfreq)
    except:
      printError("BAD input file!")

    FrInfo = {'FREQ (MHZ)':[0. for i in Nr], 'BW (MHZ)':[0. for i in Nr], 
              'SIDEBAND':['U' for i in Nr], 'NUM CHANNELS':[1 for i in Nr],
              'CHANS TO AVG':[1 for i in Nr], 'OVERSAMPLE FAC.':[1 for i in Nr], 
              'DECIMATION FAC.':[1 for i in Nr]}

# READ METADATA FOR ALL FREQUENCIES:
    for entry in FrInfo.keys():
     for line in inputlines[fr+1:]:
       if entry in line:
         index = int((line.split(':')[0]).split()[-1])
         FrInfo[entry][index] = type(FrInfo[entry][0])(line.split()[-1])

# SORT OUT THE CHANNEL FREQUENCIES:
    metadata = []
    for nu in Nr:
      nu0 = FrInfo['FREQ (MHZ)'][nu] 
      bw = FrInfo['BW (MHZ)'][nu]
      nchan = FrInfo['NUM CHANNELS'][nu]
      chav = FrInfo['CHANS TO AVG'][nu]
      sb = {True: 1.0 , False: -1.0}[FrInfo['SIDEBAND'][nu] == 'U']
      freqs = (nu0 + sb*np.linspace(0.,1.,nchan/chav,endpoint=False)*bw)*1.e6
      metadata.append(freqs)
#####

   # print FrInfo 


#######
# WARNING! UNCOMMENT THIS IF NOT DEBUGGING!
  if os.path.exists(OUTPUTIDI) and IDI != OUTPUTIDI:
    printMsg('Will REMOVE the existing OUTPUT file (or directory)!\n')
    printMsg('Copying IDI to OUTPUTIDI!\n')
    os.system('rm -rf %s'%OUTPUTIDI)
    os.system('cp -rf %s %s'%(IDI,OUTPUTIDI))
  elif not os.path.exists(OUTPUTIDI):
    os.system('cp -rf %s %s'%(IDI,OUTPUTIDI))
#     
#######



# TEMPORARY FILE TO STORE FRINGE FOR PLOTTING:
  if os.path.exists('POLCONVERT.FRINGE'):
    os.system('rm -rf POLCONVERT.FRINGE')



  ngain = [len(g) for g in gains]
  kind = []


# ONLY FIRST ANTENNA IN LIN-POL LIST IS ALLOWED TO BE A PHASED ARRAY (ALMA):
  NSUM = [1 for i in range(nALMA)]
  NSUM[0] = len(allants)


####################################
# READ CALIBRATION TABLES:
  gaindata = []
  dtdata = []
  for i in range(nALMA):
   gaindata.append([])
   kind.append([])
   dtdata.append([])
   if dterms[i]=="NONE":
     nchan = 1
     ntime = 1
     dtdata[i].append(np.ones(nchan).astype(np.float64))
     for ant in range(NSUM[i]):
       dtdata[i].append([])
       dtdata[i][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((nchan,ntime)).astype(np.bool))
   else:
    success = tb.open(dterms[i]+'/SPECTRAL_WINDOW')
    if not success:
      printError("ERROR READING TABLE %s"%dterms[i])
    dtfreqs = tb.getcol('CHAN_FREQ')[:,int(spw)]
    nchan = len(dtfreqs)
    tb.close()
    tb.open(dterms[i])
    spmask = tb.getcol('SPECTRAL_WINDOW_ID')==int(spw)
    data = tb.getcol('CPARAM')[:,:,spmask]
    antrow = tb.getcol('ANTENNA1')[spmask]
    trow = tb.getcol('TIME')[spmask]
    flagrow = tb.getcol('FLAG')[:,:,spmask]
    flagsf = np.logical_or(flagrow[0,:,:],flagrow[1,:,:])
    flagsd = np.logical_or(np.abs(data[0,:,:])==0.0,np.abs(data[1,:,:])==0.0)
    flags = np.logical_or(flagsf,flagsd)
    tb.close()
    dtdata[i].append(np.zeros(nchan).astype(np.float64))
    dtdata[i][0][:] = dtfreqs
    for ant in range(NSUM[i]):
      dtdata[i].append([])
      dd0 = data[0,:,:]
      dd1 = data[1,:,:]
      dims = np.shape(dd0[:,antrow==ant])
      dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
      dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
      dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
      dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
      dtdata[i][-1].append(np.zeros(dims).astype(np.bool))
      dtdata[i][-1][0][:] = np.abs(dd0[:,antrow==ant])
      dtdata[i][-1][1][:] = np.angle(dd0[:,antrow==ant])
      unwrap(dtdata[i][-1][1][:])
      dtdata[i][-1][2][:] = np.abs(dd1[:,antrow==ant])
      dtdata[i][-1][3][:] = np.angle(dd1[:,antrow==ant])
      unwrap(dtdata[i][-1][3][:])
      dtdata[i][-1][4][:] = flags[:,antrow==ant]

   for j,gain in enumerate(gains[i]):
     gaindata[i].append([])
     if gain=="NONE":
      nchan = 1
      ntime = 1
      kind[-1].append(0)
      gaindata[i][j].append(np.ones(nchan).astype(np.float64))
      for ant in range(NSUM[i]):
       gaindata[i][j].append([])
       gaindata[i][j][-1].append(np.ones(ntime).astype(np.float64))
       gaindata[i][j][-1].append(np.ones((nchan,ntime)).astype(np.float64))
       if j==0:
         gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       else:
         gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.ones((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.bool))
     else:
      tb.open(gain+'/SPECTRAL_WINDOW')
      if not success:
        printError("ERROR READING TABLE %s"%gain)
      gfreqs = tb.getcol('CHAN_FREQ')[:,int(spw)]
      nchan = len(gfreqs)
      tb.close()
      tb.open(gain)
      spmask = tb.getcol('SPECTRAL_WINDOW_ID')==int(spw)
      trowns = tb.getcol('TIME')[spmask]
      tsort = np.argsort(trowns)
      trow = trowns[tsort]
      if 'CPARAM' in tb.colnames():
        data = (tb.getcol('CPARAM')[:,:,spmask])[:,:,tsort]
        kind[-1].append(0)
      else:
        data = (tb.getcol('FPARAM')[:,:,spmask])[:,:,tsort]
        kind[-1].append(1)

      antrow = (tb.getcol('ANTENNA1')[spmask])[tsort]
      flagrow = (tb.getcol('FLAG')[:,:,spmask])[:,:,tsort]
      if np.shape(data)[0] == 2:  # A DUAL-POL GAIN (i.e., mode 'G')
        flagsf = np.logical_or(flagrow[0,:],flagrow[1,:])
        flagsd = np.logical_or(np.abs(data[0,:])==0.0,np.abs(data[1,:])==0.0)
      else: # A GAIN IN MODE 'T'
        flagsf = np.copy(flagrow[0,:])
        flagsd = np.abs(data[0,:])==0.0
      flags = np.logical_or(flagsf,flagsd)
      tb.close()
      gaindata[i][j].append(np.zeros(nchan).astype(np.float64))
      gaindata[i][j][0][:] = gfreqs
      for ant in range(NSUM[i]):
        gaindata[i][j].append([])
        if np.shape(data)[0] == 2:  # A DUAL-POL GAIN (i.e., mode 'G')
          dd0 = data[0,:,:]
          dd1 = data[1,:,:]
        else:  # A GAIN IN MODE 'T'
          dd0 = data[0,:,:]
          dd1 = data[0,:,:]
        dims = np.shape(dd0[:,antrow==ant])
        gaindata[i][j][-1].append(np.zeros(np.shape(trow[antrow==ant])).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.bool))
        gaindata[i][j][-1][0][:] = trow[antrow==ant]
        gaindata[i][j][-1][1][:] = np.abs(dd0[:,antrow==ant])
    #    if j==0:
        gaindata[i][j][-1][2][:] = np.angle(dd0[:,antrow==ant])
    #    else:
    #      gaindata[i][j][-1][2][:] = np.angle(dd0[:,antrow==ant])
        unwrap(gaindata[i][j][-1][2])
        gaindata[i][j][-1][3][:] = np.abs(dd1[:,antrow==ant])
        gaindata[i][j][-1][4][:] = np.angle(dd1[:,antrow==ant])
        unwrap(gaindata[i][j][-1][4])
        gaindata[i][j][-1][5][:] = flags[:,antrow==ant]


# COMPUTE TIME RANGES:

  if len(plotRange)==0:
    plRan = np.array([0.,0.])
    plotFringe = False
  else:
   try:
    plRan = np.array([plotRange[0]+plotRange[1]/24.+plotRange[2]/1440.+plotRange[3]/86400.,plotRange[4]+plotRange[5]/24.+plotRange[6]/1440.+plotRange[7]/86400.])
    plotFringe = True
   except:
    printError("Bad time range format for plotRange!")

  if len(Range) == 0:
    Ran = np.array([0.,1.e20])
  else:
   try:
    Ran = np.array([Range[0]+Range[1]/24.+Range[2]/1440.+Range[3]/86400.,Range[4]+Range[5]/24.+Range[6]/1440.+Range[7]/86400.])
   except:
    printError("Bad time range format for Range!")


# CALL POLCONVERT. THE LENGTH OF THE "metadata" LIST WILL TELL
# POLCONVERT WHETHER THIS IS A FITS-IDI OR A SWIN DATASET:

  if isSWIN:
    OUTPUT = []
    walk = [f for f in os.walk(OUTPUTIDI)]
    for subd in walk:
      OUTPUT += [os.path.join(subd[0],fi) for fi in filter(lambda x: x.startswith('DIFX_'),subd[2])]
    if len(OUTPUT) == 0:
      printError("No *.difx files found in directory!")

# Derive the days of observation of each difx file:
    mjd = np.zeros(len(OUTPUT))
    mjs = np.zeros(len(OUTPUT))
    for i,fi in enumerate(OUTPUT):
      mjd[i],mjs[i] = map(float,((os.path.basename(fi)).split('.')[0]).split('_')[1:3])
    mjd0 = np.min(mjd)
    mjp = Ran + mjd0
    metadata.append(mjd0)

# Filter out files outside the computing time window:
    t0 = mjd + mjs/86400.
    i0 = np.logical_and(t0<=mjp[1],t0>=mjp[0])
    OUTPUT = [OUTPUT[i] for i in range(len(i0)) if i0[i]]

  else:
    metadata = []
    OUTPUT = OUTPUTIDI


#  print OUTPUT
  os.system('rm -rf POLCONVERT.FRINGE')
  if amp_norm:
    os.system('rm -rf POLCONVERT.GAINS')

  printMsg("Going to CONVERT!")

  didit = PC.PolConvert(nALMA, plotIF, plotAnt, len(allants), doIF, swapXY, ngain, NSUM, kind, gaindata, dtdata, OUTPUT, linAntIdx, plRan, Ran, allantidx, nphtimes, antimes, refants, asdmtimes, doTest, doConj, amp_norm, XYadd[0]*np.pi/180., metadata)



# GENERATE ANTAB FILE(s):


  if amp_norm:
    printMsg('Generating ANTAB file(s).')
    try:
      gfile = open("POLCONVERT.GAINS")
    except:
      printErr("No gain file written!")

    entries = [l.split() for l in gfile.readlines()]; gfile.close()
    IFs = np.zeros(len(entries),dtype=np.int)
    Data = np.zeros((len(entries),2))
    AntIdx = np.zeros(len(entries),dtype=np.int)

    for i,entry in enumerate(entries):
      IFs[i] = int(entry[0])
      AntIdx[i] = int(entry[1])
      Data[i,:] = map(float,entry[2:])

    Times = np.unique(Data[:,0])
    Tsys = np.zeros((len(Times),len(doIF)+1))
    Tsys[:,0] = Times

    for j in linAntIdx:
     for ii,i in enumerate(doIF):
      mask = np.logical_and(IFs==i,AntIdx==j)
      for datum in Data[mask]:
        itime = np.where(Times==datum[0])[0]
        Tsys[itime,ii+1] = datum[1]

     outf = open("POLCONVERT_STATION%i.ANTAB"%j,"w")
     print >> outf,"GAIN AA  ELEV DPFU=1.000   FREQ=10,100000"
     print >> outf,"POLY=1.0000E+00"
     print >> outf,"/"
     print >> outf,"TSYS AA  FT=1.0  TIMEOFF=0"
     print >> outf,"INDEX= "+', '.join(['\'L%i|R%i\''%(i,i) for i in doIF])
     print >> outf,"/"
     fmt0 = "%i %i:%2.2f  "
     fmt1 = "%4.2f  "*len(doIF)
     prevT = " "
     for entry in Tsys:
       MJD2000 = 51544
       Tfr,Tin = np.modf(entry[0])
       tobs = (dt.date(2000,1,1) + dt.timedelta(Tin-MJD2000)).timetuple().tm_yday
       minute,hour = np.modf(Tfr*24.)
       minute *= 60.
       currT = fmt0%(tobs,hour,minute)
       if currT != prevT:  # Limited time resolution in ANTAB
         prevT = currT
         print >> outf, currT + fmt1%tuple(entry[1:]**2.)
     print >> outf, "/"
     outf.close()




  tac = time.time()

  printMsg('PolConvert took %.1f seconds.'%(tac-tic))



# PLOT FRINGE:

  if plotFringe and didit==0:

    print '\n\n'
    printMsg("Plotting selected fringe")


    frfile = open("POLCONVERT.FRINGE","rb")
    alldats = frfile.read(4)
    nchPlot = stk.unpack("i",alldats[:4])[0]
    dtype = np.dtype([("JDT",np.float64),
                      ("A00",np.complex64),("A01",np.complex64),
                      ("A10",np.complex64),("A11",np.complex64),
                      ("CA00",np.complex64),("CA01",np.complex64),
                      ("CA10",np.complex64),("CA11",np.complex64),
                      ("M00",np.complex64),("M01",np.complex64),
                      ("M10",np.complex64),("M11",np.complex64)])

# There is a silly bug in Python 2.7, which generates
# an "integer is required" error in the first try to read:
    try:
      fringe = np.fromfile(frfile,dtype=dtype)
    except:
      fringe = np.fromfile(frfile,dtype=dtype)

    frfile.close()

    if swapRL:
      printMsg('Will swap R <-> L in plots')

    if nchPlot > 0 and len(fringe)>0:

      fig = pl.figure(figsize=(12,6))


      rchan = int(len(fringe[:]["A00"])/float(nchPlot))


      if swapRL:
      
        uncal = [acc[:rchan*nchPlot].reshape(rchan,nchPlot) for acc in [fringe[:]["A01"], fringe[:]["A00"], fringe[:]["A11"], fringe[:]["A10"]]]
        cal = [acc[:rchan*nchPlot].reshape(rchan,nchPlot) for acc in [fringe[:]["CA01"], fringe[:]["CA00"], fringe[:]["CA11"], fringe[:]["CA10"]]]

      else:

        uncal = [acc[:rchan*nchPlot].reshape(rchan,nchPlot) for acc in [fringe[:]["A00"], fringe[:]["A01"], fringe[:]["A10"], fringe[:]["A11"]]]
        cal = [acc[:rchan*nchPlot].reshape(rchan,nchPlot) for acc in [fringe[:]["CA00"], fringe[:]["CA01"], fringe[:]["CA10"], fringe[:]["CA11"]]]


# Zoom for the image plots:
      ToZoom = min(rchan,nchPlot)

      Ch0 = nchPlot/2 - ToZoom/2
      Ch1 = nchPlot/2 + ToZoom/2

      t0 = len(fringe)/nchPlot/2 - ToZoom/2
      t1 = len(fringe)/nchPlot/2 + ToZoom/2

# Fringes in delay-rate space:
      FRRu = np.fft.fftshift(np.fft.fft2(uncal[0]))
      FRLu = np.fft.fftshift(np.fft.fft2(uncal[1]))
      FLRu = np.fft.fftshift(np.fft.fft2(uncal[2]))
      FLLu = np.fft.fftshift(np.fft.fft2(uncal[3]))

      RRu = np.abs(FRRu) 
      RLu = np.abs(FRLu) 
      LRu = np.abs(FLRu) 
      LLu = np.abs(FLLu) 
 
      RR = (np.abs(np.fft.fftshift(np.fft.fft2(cal[0])))) 
      RL = (np.abs(np.fft.fftshift(np.fft.fft2(cal[1])))) 
      LR = (np.abs(np.fft.fftshift(np.fft.fft2(cal[2])))) 
      LL = (np.abs(np.fft.fftshift(np.fft.fft2(cal[3])))) 

# Calibration matrix (in frequency-time space)
      Kmat = [k[:rchan*nchPlot].reshape(rchan,nchPlot) for k in [fringe[:]['M00'],fringe[:]['M01'],fringe[:]['M10'],fringe[:]['M11']]]
      MAXK = max(map(np.max,map(np.abs,Kmat)))    
      MINK = min(map(np.min,map(np.abs,Kmat)))    


# Peaks to scale plots:
      RMAXu = [np.unravel_index(np.argmax(ar),np.shape(ar)) for ar in [RRu,RLu,LRu,LLu]]
      MAXu = max([RRu[RMAXu[0]],RLu[RMAXu[1]],LRu[RMAXu[2]],LLu[RMAXu[3]]])
      MAXl = [np.max(RR),np.max(RL),np.max(LR),np.max(LL)]
      MAX = max(MAXl)


# Plot fringes:

      fig.clf()
      ratt = 1.0   

      fig.subplots_adjust(left=0.02,right=0.98,wspace=0.05,hspace=0.2)


      abLL = np.abs(LL)
      abRR = np.abs(RR)
      abRL = np.abs(RL)
      abLR = np.abs(LR)


      sub0 = fig.add_subplot(241)
      sub0.imshow(np.abs(RRu),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
      pl.title('XR mixed')
      pl.setp(sub0.get_xticklabels(),visible=False)
      pl.setp(sub0.get_yticklabels(),visible=False)

      sub = fig.add_subplot(242,sharex=sub0,sharey=sub0)
      sub.imshow(np.abs(RLu),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
      pl.title('XL mixed')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(243,sharex=sub0,sharey=sub0)
      sub.imshow(abRR,vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
      pl.title('RR cal')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(244,sharex=sub0,sharey=sub0)
      sub.imshow(abRL,vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
      pl.title('RL cal')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)


      sub = fig.add_subplot(245,sharex=sub0,sharey=sub0)
      sub.imshow(np.abs(LRu),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
      pl.title('YR mixed')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(246,sharex=sub0,sharey=sub0)
      sub.imshow(np.abs(LLu),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
      pl.title('YL mixed')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(247,sharex=sub0,sharey=sub0)
      sub.imshow(abLR,vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
      pl.title('LR cal')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(248,sharex=sub0,sharey=sub0)
      sub.imshow(abLL,vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
      pl.title('LL cal')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub0.set_xlim((Ch0,Ch1))
      sub0.set_ylim((t0,t1))

      pl.suptitle('DELAY-RATE FRINGE FOR IF %i (BASELINE TO ANT #%i) FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i'%tuple([plotIF,plotAnt]+plotRange))
      pl.savefig('Fringe.plot%i.png'%plotAnt)



# Plot calibration matrix:

      fig = pl.figure()


      ratt = float(np.shape(Kmat[0])[1])/float(np.shape(Kmat[0])[0])


      fig.subplots_adjust(right=0.8)
      cbar_ax = fig.add_axes([0.85, 0.15, 0.05, 0.7])

      sub = fig.add_subplot(221)
      im = sub.imshow(np.abs(Kmat[0]),vmin=MINK,vmax=MAXK,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{XR}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(222)
      sub.imshow(np.abs(Kmat[1]),vmin=MINK,vmax=MAXK,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{XL}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(223)
      sub.imshow(np.abs(Kmat[2]),vmin=MINK,vmax=MAXK,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{YR}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(224)
      sub.imshow(np.abs(Kmat[3]),vmin=MINK,vmax=MAXK,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{YL}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      cbar = fig.colorbar(im, cax=cbar_ax)
      cbar.set_label("Amplitude (Norm)")
      pl.suptitle('CAL. MATRIX FOR IF %i FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i - FREQ = X ; TIME = Y'%tuple([plotIF]+plotRange))

      pl.savefig('Kmatrix_AMP.png')

      fig.clf()

      fig.subplots_adjust(right=0.8)
      cbar_ax = fig.add_axes([0.85, 0.15, 0.05, 0.7])

      sub = fig.add_subplot(221)
      im = sub.imshow(180./np.pi*np.angle(Kmat[0]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{XR}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(222)
      sub.imshow(180./np.pi*np.angle(Kmat[1]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{XL}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(223)
      sub.imshow(180./np.pi*np.angle(Kmat[2]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{YR}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      sub = fig.add_subplot(224)
      sub.imshow(180./np.pi*np.angle(Kmat[3]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
      pl.title(r'$K_{YL}$')
      pl.setp(sub.get_xticklabels(),visible=False)
      pl.setp(sub.get_yticklabels(),visible=False)

      cbar = fig.colorbar(im, cax=cbar_ax)
      cbar.set_label("Phase (deg.)")
      pl.suptitle('CAL. MATRIX FOR IF %i FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i - FREQ = X ; TIME = Y'%tuple([plotIF]+plotRange))

      pl.savefig('Kmatrix_PHAS.png')
 

# Estimate the Dynamic range:
      DLL = np.max(abLL)/np.std(np.sort(abLL.flatten())[:-nchPlot])
      DRR = np.max(abRR)/np.std(np.sort(abRR.flatten())[:-nchPlot])
      DRL = np.max(abRL)/np.std(np.sort(abRL.flatten())[:-nchPlot])
      DLR = np.max(abLR)/np.std(np.sort(abLR.flatten())[:-nchPlot])

      toprint = [MAXl[0]/MAX,DRR,MAXl[3]/MAX,DLL,MAXl[1]/MAX,DRL,MAXl[2]/MAX,DLR]

      pmsg = '\n\nNORM. FRINGE PEAKS: \n  RR: %.2e ; SNR: %.1f \n  LL: %.2e ; SNR: %.1f \n  RL: %.2e ; SNR: %.1f \n  LR: %.2e ; SNR: %.1f\n\n'%tuple(toprint)

      pfile = open('FRINGE.PEAKS.dat','w' )
      printMsg(pmsg)
      pfile.write(pmsg)

      NUM =  np.angle(FRRu[RMAXu[0]]*np.average(Kmat[2]))
      DEN =  np.angle(FLRu[RMAXu[2]]*np.average(Kmat[3]))
      optph = (180./np.pi*((NUM-DEN)-np.pi))%360.
      if optph > 180.:
        optph -= 360.
      pmsg = '\nFor RL: optimum X/Y phase is %.1f deg.'%(optph)
      NUM =  np.angle(FRLu[RMAXu[1]]*np.average(Kmat[0]))
      DEN =  np.angle(FLLu[RMAXu[3]]*np.average(Kmat[1]))
      optph = (180./np.pi*((NUM-DEN)-np.pi))%360.
      if optph > 180.:
        optph -= 360.
      pmsg += '\nFor LR: optimum X/Y phase is %.1f deg.\n'%(optph) 

      printMsg(pmsg) 

      pfile.write(pmsg)
      pfile.close()


    else:

      printMsg('NO DATA TO PLOT!') 


  printMsg('Please, check the PolConvert.log file for special messages.')

#  return True


