# Copyright (c) Ivan Marti-Vidal 2012-2018 
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

__version__ = "1.7.5"
date = 'June 26, 2018'     


################
# Import all necessary modules. 

########
# Execute twice, to avoid the silly (and harmless) 
# error regarding the different API versions of 
# numpy between the local system and CASA:
import os, sys
try: 
# mypath = os.path.dirname(os.path.realpath('__file__'))
# sys.path.append(mypath)
 import _PolConvert as PC
 goodclib = True
 print '\nC++ shared library loaded successfully\n'
except:
 goodclib=False
 print '\n There has been an error related to the numpy' 
 print ' API version used by CASA. This is related to PolConvert'
 print ' (which uses the API version of your system) and should' 
 print ' be *harmless*.\n'

#mypath = os.path.dirname(os.path.realpath('__file__'))
#print mypath

if not goodclib:
  try: 
#   mypath = os.path.dirname(os.path.realpath('__file__'))
#   sys.path.append(mypath)
   import _PolConvert as PC
   goodclib = True
   print '\nC++ shared library loaded successfully\n'
  except:
   goodclib=False
#############

import os,sys,shutil,re
import gc
import time
import struct as stk
import scipy.optimize as spopt
import numpy as np
import pylab as pl
import datetime as dt
import sys
from taskinit import *
import pickle as pk
ms = gentools(['ms'])[0]
tb = gentools(['tb'])[0]




#########################################################
###########################
# THESE LINES ARE JUST FOR TESTING AND DEBUGGING.
# NOT EXECUTED IF THE FILE IS LOADED AS A MODULE.
# GENERAL USERS SHOULD *NOT* BOTHER OF THESE LINES
# (DO NOT EDIT THEM, SINCE THEY HAVE *NO EFFECT* ON
# THE PROGRAM WHEN YOU LOAD IT AS A MODULE!)
###########################


if __name__=='__main__':
  IDI                =  "./e17e11-3-hi_1083.difx"
  OUTPUTIDI          =  "TESTING"
  DiFXinput          =  "./e17e11-3-hi_1083.input"
  DiFXcalc           =  "./e17e11-3-hi_1083.calc"
  doIF               =  [37, 38]
  linAntIdx          =  [1]
  Range              =  []
  ALMAant            =  "../../TRACK_E/TRACK_E.concatenated.ms.ANTENNA"
  spw                =  -1
  calAPP             =  "../../TRACK_E/TRACK_E.concatenated.ms.calappphase"
  calAPPTime         =  [0.0, 5.0]
  APPrefant          =  ""
  gains              =  [['../../TRACK_E/TRACK_E.concatenated.ms.bandpass-zphs', '../../TRACK_E/TRACK_E.calibrated.ms.Gxyamp.APP', '../../TRACK_E/TRACK_E.calibrated.ms.XY0.APP', '../../TRACK_E/TRACK_E.concatenated.ms.flux_inf.APP', '../../TRACK_E/TRACK_E.concatenated.ms.phase_int.APP']]
  interpolation      =  []
  gainmode           =  [['G', 'G', 'G', 'T', 'G']]
  XYavgTime          =  0.0
  dterms             =  ['../../TRACK_E/TRACK_E.calibrated.ms.Df0.APP']
  amp_norm           =  0.031
  XYadd              =  [0.0]
  XYdel              =  [0.0]
  XYratio            =  [1.0]
  swapXY             =  [False]
  swapRL             =  False
  IDI_conjugated     =  False
  plotIF             =  [37, 38]
  plotRange          =  [0, 0, 0, 0, 2, 0, 0, 0]
  plotAnt            =  2
  excludeAnts        =  []
  doSolve            =  -1
  solint             =  [1, 1]
  doTest             =  True
  npix               =  50
  solveAmp           =  True
  solveMethod        =  "gradient"
  calstokes          =  [1.0, 0.0, 0.0, 0.0]
  calfield           =  -1
  

#
#
#
###########################
#########################################################





############################################
# COMMENT OUT THIS LINE WHEN DEBUGGING
# YOU SHALL THEN RUN THIS FILE WITH "execfile(...)"

def polconvert(IDI, OUTPUTIDI, DiFXinput, DiFXcalc, doIF, linAntIdx, Range, ALMAant, spw, calAPP, calAPPTime, APPrefant, gains, interpolation, gainmode, XYavgTime, dterms, amp_norm, XYadd, XYdel, XYratio, swapXY, swapRL, IDI_conjugated, plotIF, plotRange, plotAnt,excludeAnts,doSolve,solint,doTest,npix,solveAmp,solveMethod, calstokes, calfield):

# if True:
############################################


  DEBUG = True



# Auxiliary function: print error and raise exception:
  def printError(msg):
    print msg,'\n' 
    casalog.post('PolConvert: '+msg+'\n')
    lfile = open("PolConvert.log","a")
    print >> lfile,'\n'+msg+'\n'
    lfile.close()
    raise Exception(msg)




# Auxiliary function: print message (terminal and log):
  def printMsg(msg, doterm=True, dolog=True):
    if doterm:
      print msg
    if dolog:
      casalog.post('PolConvert: '+msg)
      lfile = open("PolConvert.log","a")
      print >> lfile,'\n'+msg+'\n'
      lfile.close()




# Auxiliary function: Geometric Median of a complex number:
  def geoMedian(Window, method='phasor'):

    WinData = np.array(Window)

 # Simplest approach (Amp & Phase separate). Assume NO WRAPS in phase:
    pAvg = np.median(np.abs(WinData))*np.exp(1.j*np.median(np.angle(WinData)))
    if method=='phasor': 
      return pAvg


 # A bit more complicated (point of minimum distance).
    elif method=='Torricelli':

      def L1Dist(p):
        return np.sum(np.abs(WinData-p[0]-1.j*p[1]))

      Torr = spopt.minimize(L1Dist, [pAvg.real, pAvg.imag], method='COBYLA')
      return Torr.x[0] + 1.j*Torr.x[1]








# Auxiliary function: Smooth the X-Y difference of G-mode gains
# using a running average:
  def XYsmooth(GAINTABLE, DTIME, SPW, IANT=-1):

    os.system('rm -rf %s.XYsmooth.PolConvert'%GAINTABLE)
    try:
      os.system('cp -r %s %s.XYsmooth.PolConvert'%(GAINTABLE,GAINTABLE))
      tb.open('%s.XYsmooth.PolConvert/ANTENNA'%GAINTABLE)
      GallAnts = tb.getcol('NAME')
      tb.close()

      if IANT>=0:
        allAnts = [IANT]
      else:
        allAnts = range(len(GallAnts))

      tb.open('%s.XYsmooth.PolConvert'%GAINTABLE,nomodify=False)
      Gtimes = tb.getcol('TIME')
      Gants = tb.getcol('ANTENNA1')
      Gspws = tb.getcol('SPECTRAL_WINDOW_ID')
      Gflg = np.logical_not(tb.getcol('FLAG'))
      if np.shape(Gflg)[0] > 1:
        isT = 1
      else: 
        isT = 0
  #    print isT, np.shape(Gflg)
      Ggood = np.logical_and(Gflg[0,0,:],Gflg[isT,0,:])
      Mask = np.logical_and(Gspws == SPW, Ggood)
      Ggains = tb.getcol('CPARAM')

    except:
      printError('ERROR: Bad gain table %s!'%GAINTABLE)

   # Get the X-Y cross-phase gain:
    GDiff = Ggains[0,0,:]/Ggains[isT,0,:]  

   # Get the polarization-independent gain:
    TMode = Ggains[0,0,:]*Ggains[isT,0,:]



  # Smooth them:
    for iant in allAnts:
      Mask2 = np.where(np.logical_and(Mask,Gants==iant))[0]
      sys.stdout.write('\rSmoothing X-Y difference for antenna %s (%i of %i)   '%(GallAnts[iant],iant+1,len(GallAnts)))
      sys.stdout.flush()

      for tii,ti in enumerate(Mask2):
        Window = []
        tij = tii
        while tij>=0 and np.abs(Gtimes[ti]-Gtimes[Mask2[tij]])<DTIME/2.:
          Window.append(GDiff[Mask2[tij]])
          tij -= 1
        tij = tii+1
        while tij<len(Mask2) and np.abs(Gtimes[ti]-Gtimes[Mask2[tij]])<DTIME/2.:
          Window.append(GDiff[Mask2[tij]])
          tij += 1


  # Median (normalized):
        AvgDiff = geoMedian(Window)
        AvgDiff /= np.abs(AvgDiff)


        Ggains[0,0,ti] = np.sqrt(TMode[ti]*AvgDiff)
        Ggains[isT,0,ti] = np.sqrt(TMode[ti]/AvgDiff)


  # Update the table:
    tb.putcol('CPARAM',Ggains) 
    tb.close()         
    print '\nDONE!\n\n' 











# Auxiliary function: Re-reference XY-phases to 
# another ALMA refant, using the CalAPPPhase table:
  def ReReference(CALAPPPHASE,XY0,SPW,REFANT):

    printMsg("\n\n  GOING TO RE-REFERENCE X-Y PHASES TO %s.\n\n"%REFANT)

    DTMax = 180. # Minimum time gap (sec) to assume that TelCal has been reset.

# Figure out the baseband:
    tb.open('%s/SPECTRAL_WINDOW'%XY0)
    try:
      spname = tb.getcol('NAME')[SPW]
      BB =  [ii for ii in spname.split('#') if "BB_" in ii][0]
    except:
      printError("\n ERROR: BAD NAME FOR SPW %i IN TABLE %s\n"%(SPW,XY0))
    tb.close()


# Read TelCal's phases:
    tb.open(CALAPPPHASE)
    IMAX = np.argmax(tb.getcol('numPhasedAntennas'))
    ANT = list(tb.getcell('phasedAntennas',IMAX))

    try:
      REFIDX = ANT.index(REFANT)
    except:
      printError("\n\n ERROR: ANTENNA %s IS NOT IN CALAPP-PHASE TABLE!"%REFANT)

    ti = tb.getcol('startValidTime')
    bb = tb.getcol('basebandName')

    Tsort = np.argsort(ti)
    Torder = np.copy(ti[Tsort])
    UT = 24.*(Torder/86400.-int(Torder[0]/86400.))

# Arrange phases of the REFANT:
    Gains = []
    for i in range(len(ti)):
      aux = list(tb.getcell('phasedAntennas', rownr = i))
      try:
        REFI = aux.index(REFANT)
        aux2 = tb.getcell('phaseValues', rownr = i)
        NIF = tb.getcell('numChannels', rownr = i)
        Gains.append([aux2[NIF*REFI:NIF*(REFI+1)],aux2[NIF*(REFI+len(aux)):NIF*(REFI+len(aux)+1)]])
      except:
        printMsg("WARNING: ANTENNA %s NOT IN LIST OF PHASE ANTENNAS AT TIME %.1f.\n      THE RESULTING X-Y PHASES MAY BE *WRONG* AT THIS TIME!"%(REFANT,ti))
        Gains.append([np.zeros(NIF),np.zeros(NIF)])

    GainsA = np.array(Gains)


# Filter phases for the SPW:
    b1 = bb[Tsort] == BB

    Nchan = np.shape(GainsA)[-1]
    
    IX = np.copy(GainsA[Tsort,:][b1,0,:])
    IY = np.copy(GainsA[Tsort,:][b1,1,:])

    tb.close()
    Mask = np.where(np.logical_and(Torder[b1][1:]-Torder[b1][:-1]>DTMax,IX[:-1,0]!=0.0))

# Plot TelCal phases for REFANT:
    plfig = pl.figure()
    plsub = plfig.add_subplot(111)
    symb = ['or','og','ob','ok','om','oy','^r','^g','^b','^k','^m','^y']
    XYDiff = []
    AvXY = []
    for ni in range(Nchan):
      IntX = IX[1:,ni][Mask]
      IntY = IY[1:,ni][Mask]
      XYDiff.append(np.mod(IntX-IntY,2.*np.pi))

# We take the time MEDIAN as the best XY-phase estimate for the REFANT:
      AvXY.append(np.median(XYDiff[-1]))

      plsub.plot(UT[b1][1:][Mask],XYDiff[-1]*180./np.pi,symb[ni],label='CH %i'%(ni+1))
      printMsg('For antenna %s, Chan %i, found TelCal median X-Y phase of %.1f deg.'%(REFANT,ni+1,180./np.pi*AvXY[-1]))

    pl.legend(numpoints=1)
    plsub.set_xlabel('UT (h)')
    plsub.set_ylabel('X-Y phase (deg.)')
    plsub.set_xlim((0,24))
    pl.title('TelCal X-Y phases for new REFANT: %s'%REFANT)
    pl.savefig('%s.RE-REFERENCING.png'%CALAPPPHASE)


# Correct XY=phase table:
    printMsg("\n\n ADDING PHASES TO NEW XY0 TABLE\n\n")
    os.system("rm -rf %s.REFANT_%s"%(XY0,REFANT))
    os.system("cp -r %s %s.REFANT_%s"%(XY0,XY0,REFANT))

    tb.open("%s.REFANT_%s"%(XY0,REFANT),nomodify=False)
    spwi = tb.getcol('SPECTRAL_WINDOW_ID')
    XYData = tb.getcol("CPARAM")
    Mask = spwi==SPW
    NNu = np.shape(XYData)[1]
    NuPerChan = NNu/Nchan
    for ni in range(Nchan):
      XYData[0,ni*NuPerChan:(ni+1)*NuPerChan,Mask] *= np.exp(-1.j*(AvXY[ni]))

    tb.putcol("CPARAM",XYData)
    tb.close()

# Plot new vs. old XY-phases:
    Idx = np.where(Mask)[0][0]
    tb.open(XY0)
    XOrig = tb.getcol("CPARAM")[0,:,Idx]
    YOrig = tb.getcol("CPARAM")[1,:,Idx]
    tb.close()
    plfig.clf()
    plsub = plfig.add_subplot(111)
    plsub.plot(np.angle(XOrig/YOrig)*180./np.pi,'or',label='Original')

    tb.open("%s.REFANT_%s"%(XY0,REFANT))
    XOrig = tb.getcol("CPARAM")[0,:,Idx]
    YOrig = tb.getcol("CPARAM")[1,:,Idx]
    tb.close()
    plsub.plot(np.angle(XOrig/YOrig)*180./np.pi,'ob',label='Re-Referenced')

    pl.legend(loc=0,numpoints=1)
    plsub.set_ylim((-250,250))
    pl.savefig('%s_RE-REF_XYPHASES.png'%XY0)













# Auxiliary function: derive job label from DiFXinput
  def jobLabel(inputname):
    label = inputname
    try:
      label = re.sub('.input','', os.path.basename(label))
    except:
      pass
    return label





# Auxiliary function: unwrap phases for time interpolation
  def unwrap(phases, check=False):

    dims = np.shape(phases)
 #   print dims
    if dims[1]==0:  # Bandpass type
     for i in range(len(phases)-1):
      if phases[i+1]-phases[i] > np.pi:
        phases[i+1,:] -= 2.*np.pi
      elif phases[i+1]-phases[i] < -np.pi:
        phases[i+1,:] += 2.*np.pi
     if check:
       pl.figure()
       pl.plot(180./np.pi*phases)
       pl.show()
       raw_input('CHECK')


    elif dims[0]>1:  # Bandpass-gain type
 #    pl.plot(phases[:,0])
     for j in range(dims[1]):
      for i in range(dims[0]-1):
       if phases[i+1,j]-phases[i,j] > np.pi:
        phases[i+1:,j] -= 2.*np.pi
        printMsg('Adding phase wrap to gain at channel %i'%i)
       elif phases[i+1,j]-phases[i,j] < -np.pi:
        phases[i+1:,j] += 2.*np.pi
        printMsg('Removing phase wrap to gain at channel %i'%i)
     if check:
       pl.figure()
       pl.plot(180./np.pi*phases[:,0])
       pl.show()
       raw_input('CHECK')




# Auxiliary function: prepare the CALAPPPHASE, from a set of MSs:
  def makeCalAPP(mslist):
    printMsg('Will create CALAPPPHASE table from measurement sets.')
    os.system('rm -rf ./CALAPPPHASE.tab')
    for i,asd in enumerate(mslist):
      printMsg('Working out MS #%i - %s'%(i+1,asd))
      if i==0:
       os.system('cp -rf %s/ASDM_CALAPPPHASE ./CALAPPPHASE.tab'%asd)
      else:
       tb.open(os.path.join(asd,'ASDM_CALAPPPHASE'))
       tb.copyrows('./CALAPPPHASE.tab')
       tb.close()

    tb.open(os.path.join(asd,'ASDM_CALAPPPHASE'))
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


    return timeranges


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
  

  allMethods = ['gradient','Levenberg-Marquardt','COBYLA']
  scipyMethods = ['COBYLA']
  if solveMethod not in allMethods:
    printError("ERROR! \'solveMethod\' must be any of: %s"%(', '.join(allMethods)))


  if type(calstokes) is not list:
    printError("ERROR! Wrong calstokes!")
  elif len(calstokes)!=4:
    printError("ERROR! calstokes should have 4 elements!")
  for item in calstokes:
    if type(item) is not float:
      printError("ERROR! calstokes should only have float elements!")

  Stokes = list(calstokes)

  if Stokes[0]<=0. or Stokes[0]<np.sqrt(Stokes[1]**2.+Stokes[2]**2.+Stokes[3]**2.):
      printError("ERROR! Inconsistent Stokes parameters!")

# Will implement solveQU soon!
  solveQU = False
#  calfield = -1

  if type(solveQU) is not bool:
    printError("ERROR! Wrong solveQU!")
  if type(calfield) is not int:
    printError("ERROR! Wrong calfield!")

#  if calfield>=0:
#    printMsg("Will use field #%i\n"%calfield)


  doConj = True
  if type(IDI_conjugated) is not bool:
    printError("ERROR! IDI_cojugated should be a boolean!")
  else:
    doConj = IDI_conjugated

  if type(swapRL) is not bool:
    printError("ERROR! swapRL should be a boolean!")

  try:
    doSolve = float(doSolve)
  except:
    printError("ERROR! doSolve should be a float!")

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


  if type(solint) is not list or (len(solint) not in [2,3]):
    printError("ERROR! solint must be a list of two/three numbers!")
  else:
    try:
      solint[0] = int(solint[0])
      solint[1] = int(solint[1])
    except:
      printError("ERROR! solint must be a list of two/three numbers!")

  if len(solint)==3:
    solint[2] = float(solint[2])
  else: # Default dt
    solint.append(100.)

  nALMA = len(linAntIdx)


  if not os.path.exists(IDI):
    printError("ERROR! IDI file (or SWIN folder) does not exist!")

  timeranges = np.array([0.,0.])

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

  if type(plotIF) is int:
    plotIF = [plotIF]
  for pli in plotIF:
    if type(pli) is not int:
      printError("ERROR! plotIF should be an integer or a list of integers!")

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
  

# Sanity check for interpolation:
  if type(interpolation) is not list:
    printError("Interpolation must be a list (or a list of lists)!")

  if len(interpolation)==0:
    interpolation = [[] for i in range(nALMA)]

  if len(interpolation)!= nALMA:
    printError("Wrong length for interpolation!")

  for i,intype in enumerate(interpolation):
    if type(intype) is not list:
      printError("Interpolation must be a list (or a list of of lists)!")
    if len(intype)==0:
      interpolation[i] = ['linear' for g in gains[i]]
      intype = interpolation[i]
    if len(intype) != len(gains[i]):
      printError("Interpolation must have the same dimensions as gains!")
    for ints in intype:
      if ints not in ['linear','nearest']:
        printMsg("integration type " + ints + " requested.")
        printError("Only \'linear\' and \'nearest\' interpolations are supported!")


  try:
    XYavgTime = float(XYavgTime)

  except:
    printError("XYavgTime must be a positive (or zero) double!")

  if XYavgTime < 0.0:
    printError("XYavgTime must be positive or zero!")



  if type(gainmode) is not list:
    printError("gainmode must be a list (or a list of lists)!")

  if len(gainmode)==0:
    gainmode = [[] for i in range(nALMA)]

  if len(gainmode)!= nALMA:
    printError("Wrong length for gainmode!")

  for i,gtype in enumerate(gainmode):
    if type(gtype) is not list:
      printError("gainmode must be a list (or a list of of lists)!")
    if len(gtype)==0:
      gainmode[i] = [{True:'G',False:'T'}['XY0' in g or 'bandpass' in g or 'Gxyamp' in g] for g in gains[i]]
      gtype = gainmode[i]
    if len(gtype) != len(gains[i]):
      printError("gainmode must have the same dimensions as gains!")
    for ints in gtype:
      if ints not in ['G','T','S']:
        printMsg("Gain type " + ints + " requested.")
        printError("Only \'G\', \'S\' and \'T\' interpolations are supported!")

  for gi in range(len(gains)):
    for gii in range(len(gains[gi])):
      printMsg('Will calibrate with table %s in %s mode.'%(os.path.basename(gains[gi][gii]),gainmode[gi][gii]))

# Sanity check for gains and dterms:
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


  XYdelF = [0.0 for i in range(len(XYdel))]
  if type(XYdel) is not list or len(XYdel) != nALMA:
    printError("Invalid format for XYdel!\n Should be a LIST of integers, as large as the number of linear-polarization VLBI stations!")
  for i in range(len(XYdel)):
      try:
        XYdelF[i] = float(XYdel[i]*np.pi/180.)
      except:
       printError("Invalid format for XYdel!\n Should be a LIST of numbers, as large as the number of linear-polarization VLBI stations!")




  if len(swapXY) != nALMA:
    printError("Invalid format for swapXY!\n Should be a list of booleans, as large as the number of linear-polarization VLBI stations!")
  for sxy in swapXY:
      if type(sxy) is not bool:
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

#   print calAPP
#   os.system('ls %s'%calAPP)

   success = tb.open(calAPP)

   if not success:
    printError('ERROR: INVALID calAPP TABLE!')
    
   try:

    time0 = tb.getcol('startValidTime')-CALAPPDT+CALAPPTSHIFT
    time1 = tb.getcol('endValidTime')+CALAPPDT+CALAPPTSHIFT



#########
# Figure out times with unphased data:
    ADJ = tb.getcol('adjustToken')
    sc2flag = [i for i in range(len(ADJ)) if ('PHASE_UPDATED' not in ADJ[i]) and ('PHASE_NOCHANGE' not in ADJ[i])]
    scgood = [i for i in range(len(ADJ)) if ('PHASE_UPDATED' in ADJ[i] or 'PHASE_NOCHANGE' in ADJ[i])]

    SUBSCANDUR = time1[sc2flag[0]] - time0[sc2flag[0]]
    sec = 1.
    timeranges = []
    if len(sc2flag)>0:
      for si in sc2flag:
        timerange = [time1[si]-SUBSCANDUR-sec, time1[si]+sec]
        if timerange not in timeranges:
          timeranges.append(timerange)

    for si in scgood:
      timerange = [time1[si]-SUBSCANDUR-sec,time0[si]+sec]
      if si-1 in sc2flag:  
        if timerange not in timeranges:
          timeranges.append(timerange)

# For testing:
#    timeranges.append([4998635280., 4998635350.])
    timeranges = np.array(timeranges)
#########


    nphant = tb.getcol('numPhasedAntennas')
    refs = tb.getcol('refAntennaIndex')
    asdmtimes = [time0,time1]
    phants = []
    for i in range(len(nphant)):
      phants.append(list(tb.getcell('phasedAntennas', rownr = i)))

    tb.close()

   except:
#   else:
    printError('ERROR: INVALID calAPP TABLE CONTENT!')

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
    printMsg('Opening calc file... %s' % DiFXcalc)
    try:
      printMsg('Opening "%s"' % (DiFXcalc))
      antcoords = []  ; soucoords = [[],[]]; antmounts = []
      calc = open(DiFXcalc)
      lines = calc.readlines()
      calc.close()
      printMsg('Read %d lines from %s' % (len(lines), DiFXcalc))
      for ii, line in enumerate(lines):
        if 'TELESCOPE' in line and 'X (m):' in line:
          printMsg(line.rstrip())
          antcoords.append(map(float,[ll.split()[-1] for ll in lines[ii:ii+3]]))
          antmounts.append(0)
        if 'SOURCE' in line and ' RA: ' in line:
          printMsg(line.rstrip())
          soucoords[0].append(float(line.split()[-1]))
          soucoords[1].append(float(lines[ii+1].split()[-1]))
      antcoords = np.array(antcoords,dtype=np.float)
      antmounts = np.array(antmounts)
      soucoords[0] = np.array(soucoords[0],dtype=np.float)
      soucoords[1] = np.array(soucoords[1],dtype=np.float)
      printMsg('done parsing calc')
    except:
      printMsg(("WARNING! Invalid DiFX calc file '%s'!\n" + 
        "PolConvert may not calibrate properly.") % DiFXcalc)
  elif os.path.isfile(IDI):
    isSWIN = False
    printMsg('\n\nYou have asked to convert a FITS-IDI file.')
    print 'Reading array geometry...'
    try:
      import pyfits as pf
      ffile = pf.open(IDI)
      for ii,group in enumerate(ffile):
        if group.name == 'ARRAY_GEOMETRY':
          grarr = ii
        elif group.name == 'SOURCE':
          grsou = ii


      raappUnit = ffile[grsou].data.columns[ [coln.name for coln in ffile[grsou].data.columns].index('RAAPP') ].unit
      decappUnit = ffile[grsou].data.columns[ [coln.name for coln in ffile[grsou].data.columns].index('DECAPP') ].unit

      soucoords = [np.array(ffile[grsou].data['RAAPP'],dtype=np.float),np.array(ffile[grsou].data['DECAPP'],dtype=np.float)]

      if raappUnit=='DEGREES':
        soucoords[0] *= np.pi/180.

      if decappUnit=='DEGREES':
        soucoords[1] *= np.pi/180.

      ffile.close()

# THESE LINES FAIL IF ORBPARM IS PRESENT IN ARRAY GEOMETRY!
#      antcoords = np.array(ffile[grarr].data['STABXYZ'],dtype=np.float)
#      antmounts = np.array(ffile[grarr].data['MNTSTA'],dtype=np.float)
      import _getAntInfo as gA
      
      success = gA.getAntInfo(IDI)
      if success != 0:
        printError("ERROR GETTING FITS-IDI METADATA! ERR: %i"%success)
      else:
        antcoords = gA.getCoords()
        antmounts = gA.getMounts()

    except:
      printMsg('WARNING! This FITS-IDI file has missing information!\nPolConvert may not calibrate properly.')
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


    if len(antcoords) == 0:
      Nteles = [inputlines.index(l) for l in inputlines if 'TELESCOPE ENTRIES' in l][0]
      antcoords = np.ones((Nteles,3),dtype=np.float)
      antmounts = np.zeros(Nteles,dtype=np.int)
    if len(soucoords[0])==0:
      soucoords = [np.zeros(1,dtype=np.float),np.zeros(1,dtype=np.float)]

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
              'DECIMATION FAC.':[1 for i in Nr], 'SIGN' :[1 for i in Nr]}

# READ METADATA FOR ALL FREQUENCIES:
    for entry in FrInfo.keys():
     for line in inputlines[fr+1:]:
       if entry in line:
         index = int((line.split(':')[0]).split()[-1])
         FrInfo[entry][index] = type(FrInfo[entry][0])(line.split()[-1])

# SORT OUT THE CHANNEL FREQUENCIES:
    metadata = []
    IFchan = 0
    for nu in Nr:
      nu0 = FrInfo['FREQ (MHZ)'][nu] 
      bw = FrInfo['BW (MHZ)'][nu]
      nchan = FrInfo['NUM CHANNELS'][nu]
# MAX. NUMBER OF CHANNELS:
      IFchan = max([IFchan,nchan])
      chav = FrInfo['CHANS TO AVG'][nu]
      sb = {True: 1.0 , False: -1.0}[FrInfo['SIDEBAND'][nu] == 'U']
      FrInfo['SIGN'][nu] = float(sb)
      freqs = (nu0 + sb*np.linspace(0.,1.,nchan/chav,endpoint=False)*bw)*1.e6
      metadata.append(freqs)

    if len(doIF)==0:
     doIF = list(Nr)

#####
 

  else:
# READ FREQUENCY INFO TO HELP SELECTING THE SPW AUTOMATICALLY:
    import pyfits as pf
    fitsf = pf.open(IDI)
    nu0 = fitsf['FREQUENCY'].header['REF_FREQ']
    bw = fitsf['FREQUENCY'].header['CHAN_BW']
    nch = fitsf['FREQUENCY'].header['NO_CHAN'] #*fitsf['FREQUENCY'].header['NO_BAND']
    IFchan = nch
    Nr = fitsf['FREQUENCY'].header['NO_BAND']
    sgn = {True:1.0,False:-1.0}[bw>0.0]
    FrInfo = {'FREQ (MHZ)':[nu0/1.e6], 'BW (MHZ)':[bw*nch/1.e6], 'SIGN':[sgn]}
    for i in range(Nr):
      FrInfo['FREQ (MHZ)'] += [(nu0 + bw*nch/1.e6)/1.e6]
      FrInfo['BW (MHZ)'] += [bw*nch/1.e6]
      FrInfo['SIGN'] += [sgn]

    if len(doIF)==0:
     doIF = range(1,1+fitsf['FREQUENCY'].header['NO_BAND'])

    fitsf.close()



# ANTENNAS TO PARTICIPATE IN THE GAIN ESTIMATES:
  calAnts = [i+1 for i in range(len(antcoords)) if i+1 not in excludeAnts]
  if plotAnt not in calAnts:
    printError("ERROR! Reference antenna is NOT in list of calibratable antennas!")

#######################
##### GET SPECTRAL WINDOW AUTOMATICALLY:
  if isPhased and spw < 0:
    printMsg("Deducing SPW...\n")
 
    from collections import Counter

    BPidx = -1
    for i in range(len(gains[0])):
      tb.open(os.path.join(gains[0][i],'SPECTRAL_WINDOW'))
      if tb.getcol('NUM_CHAN')[0]>1:
        BPidx = i
        tb.close()
        break
      tb.close()

    if BPidx >= 0:
      printMsg("Using BP table %d\n" % BPidx)

    if BPidx == -1:
      printError("I cannot guess the right spw if there are no BP-like tables!") 
      
    tb.open(os.path.join(gains[0][BPidx],'SPECTRAL_WINDOW'))
    calfreqs = tb.getcol('CHAN_FREQ')[0,:]/1.e6
    nchansp = np.shape(tb.getcol('CHAN_FREQ'))[0]
    calfreqs2 = calfreqs + tb.getcol('CHAN_WIDTH')[0,:]*nchansp/1.e6
    tb.close()
    nurange = [[np.min([calfreqs[i],calfreqs2[i]]),np.max([calfreqs[i],calfreqs2[i]])] for i in range(len(calfreqs))]
    spwsel = -np.ones(len(doIF),dtype=np.int)   #[-1 for nu in doIF]
    slop = 5.0 # MHz
    for nui,nu in enumerate(doIF):
      for spwi in range(len(calfreqs)):
       try:
        nu0 = FrInfo['FREQ (MHZ)'][nu-1]
        nu1 = FrInfo['FREQ (MHZ)'][nu-1] + FrInfo['BW (MHZ)'][nu-1]*FrInfo['SIGN'][nu-1]
        nus = [np.min([nu0,nu1]),np.max([nu0,nu1])]
        print nu, ':', nurange[spwi][0], '<', nus[0], nus[1], '<', nurange[spwi][1],
        if (nurange[spwi][0] - slop) < nus[0] and nus[1] < (nurange[spwi][1] + slop):
          spwsel[nui] = spwi
          print ' pass'
        else:
          print ' fail'
       except:
        printMsg("WARNING! spw %i is NOT in SWIN file! Will skip it!"%nu)
        spwsel[nui] = -2
    errmsg = []
    isErr = False
    for i,spws in enumerate(spwsel):
       if spws == -1:
         isErr = True
         errmsg += [str(doIF[i])]

    if isErr:
         printMsg("WARNING! There is no spw that covers all the IF frequencies!\n" +
            "Problematic IFs are:  %s"%(','.join(errmsg)))

         doIF = [doIF[i] for i in range(len(doIF)) if i in list(np.where(spwsel>=0)[0])]
         printMsg('\n\n  NEW LIST OF IFs: '+','.join(map(str,doIF)))

    spwsel = list(set(spwsel[spwsel>=0]))
    if len(spwsel)>1:
       printError("There is more than one possible spw for some IFs!")

    spw = spwsel[0]
    printMsg('Selected spw: %d\n' % spw)
########################






# Set XYadd and XYratio:

  if type(XYadd) is not list or len(XYadd) != nALMA:
    printError("Invalid format for XYadd!\n Should be a list as large as the number of linear-polarization VLBI stations!\nThe elements of that list shall be either numbers or lists as large as the number of IFs")

  XYaddF = [[] for i in range(len(XYadd))]
  for i in range(len(XYadd)):
      if type(XYadd[i]) is list:
        for j in range(len(XYadd[i])):
         if type(XYadd[i][j]) in [list,np.ndarray]:
           arrSize = np.shape(np.array(XYadd[i][j]))
           if len(arrSize)!=1 or arrSize[0] != IFchan:
             printError("Shape of XYadd array(s) does not coincide with number of IF channels\n")
           else:
             XYaddF[i].append(np.array(XYadd[i][j])*np.pi/180.)
         else:
           try:
             XYaddF[i].append(np.ones(IFchan)*float(XYadd[i][j])*np.pi/180.)
           except:
             printError("Invalid format for XYadd!\nShould be a LIST of numbers (or a list of lists of numbers),\nor a list of lists of arrays")
      else:
        try:
          XYaddF[i] = [np.ones(IFchan)*float(XYadd[i])*np.pi/180. for k in doIF]
        except:
          printError("Invalid format for XYadd!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!")


  if type(XYratio) is not list or len(XYratio) != nALMA:
    printError("Invalid format for XYratio!\n Should be a list as large as the number of linear-polarization VLBI stations!\nThe elements of that list shall be either numbers or lists as large as the number of IFs")

  XYratioF = [[] for i in range(len(XYratio))]
  for i in range(len(XYratio)):
      if type(XYratio[i]) is list:
        for j in range(len(XYratio[i])):
         if type(XYratio[i][j]) in [list,np.ndarray]:
           arrSize = np.shape(np.array(XYratio[i][j]))
           if len(arrSize)!=1 or arrSize[0] != IFchan:
             printError("Shape of XYratio array(s) does not coincide with number of IF channels\n")
           else:
             XYratioF[i].append(np.copy(XYratio[i][j]))
         else:
           try:
             XYratioF[i].append(np.ones(IFchan)*float(XYratio[i][j]))
           except:
             printError("Invalid format for XYratio!\nShould be a list (or a list of lists,\nor a list of lists of arrays))")
      else:
        try:
          XYratioF[i] = [np.ones(IFchan)*float(XYratio[i]) for k in doIF]
        except:
          printError("Invalid format for XYratio!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!")




# A-PRIORI GAINS:
  PrioriGains = []
  for i in range(len(XYaddF)):
    PrioriGains.append([])
    for j in range(len(XYaddF[i])):
       PrioriGains[i].append(np.array(XYratioF[i][j]*np.exp(1.j*XYaddF[i][j]),dtype=np.complex64))





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



########################################
# Re-reference phases, if needed:
  if len(APPrefant)>0:
    if APPrefant not in allants:
      printError("\n\nERROR: Antenna %s NOT FOUND in the table!"%APPrefant)

# Get the first gain table in list that has the ".XY0" string:
    XY0 = [gi for gi in range(ngain[0]) if '.XY0' in gains[0][gi]][0]

# Re-reference and re-assign gain table:
    ReReference(calAPP, gains[0][XY0], int(spw), APPrefant)
    gains[0][XY0] = "%s.REFANT_%s"%(gains[0][XY0],APPrefant) 

########################################









####################################
# READ CALIBRATION TABLES:
  gaindata = []
  dtdata = []
  isLinear = []
  for i in range(nALMA):
   isLinear.append(np.zeros(len(gains[i]),dtype=np.bool))
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
    success = tb.open(os.path.join(dterms[i],'SPECTRAL_WINDOW'))
    if not success:
      printError("ERROR READING TABLE %s"%dterms[i])
    dtfreqs = tb.getcol('CHAN_FREQ')[:,int(spw)]
    nchan = len(dtfreqs)
    tb.close()
    success = tb.open(os.path.join(dterms[i],'ANTENNA'))
    tabants = tb.getcol('NAME')
    tb.close()
    print 'Reading ',dterms[i]
    tb.open(dterms[i])
    spmask = tb.getcol('SPECTRAL_WINDOW_ID')==int(spw)
    data = tb.getcol('CPARAM')[:,:,spmask]

    antrow = []
    for ai in tb.getcol('ANTENNA1')[spmask]:
      if tabants[ai] in allants: 
        antrow.append(allants.index(tabants[ai]))
      else:
        antrow.append(-1)
    antrow = np.array(antrow)


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
      if dims[1]>0:
       dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[i][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[i][-1].append(np.zeros(dims).astype(np.bool))
       dtdata[i][-1][0][:] = (dd0[:,antrow==ant]).real
       dtdata[i][-1][1][:] = (dd0[:,antrow==ant]).imag
 #     unwrap(dtdata[i][-1][1][:])
       dtdata[i][-1][2][:] = (dd1[:,antrow==ant]).real
       dtdata[i][-1][3][:] = (dd1[:,antrow==ant]).imag
 #     unwrap(dtdata[i][-1][3][:])
       dtdata[i][-1][4][:] = flags[:,antrow==ant]
      else:
       dtdata[i][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[i][-1].append(np.zeros((dims[0],1)).astype(np.bool))
 
   
  
   for j,gain in enumerate(gains[i]):
     gaindata[i].append([])
     print 'Reading ',gain
     isLinear[i][j] = interpolation[i][j]=='linear'
     if gain=="NONE":
      nchan = 1
      ntime = 1
      kind[-1].append(0)
      gaindata[i][j].append(np.ones(nchan).astype(np.float64))
      for ant in range(NSUM[i]):
       gaindata[i][j].append([])
       gaindata[i][j][-1].append(np.ones(ntime).astype(np.float64))
       gaindata[i][j][-1].append(np.ones((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.ones((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       gaindata[i][j][-1].append(np.zeros((nchan,ntime)).astype(np.bool))
     else:

# Smooth X-Y differences:
      if gainmode[i][j]=='S' and XYavgTime>0.0:
        printMsg("Will average X-Y phase differences over %.1f seconds"%XYavgTime)
        XYsmooth(gain, XYavgTime, int(spw)) 
        gain = gain+'.XYsmooth.PolConvert'

# Read data and metadata:
      sucess = tb.open(os.path.join(gain,'SPECTRAL_WINDOW'))

      if not success:
        printError("ERROR READING TABLE %s"%gain)
      gfreqs = tb.getcol('CHAN_FREQ')[:,int(spw)]
      nchan = len(gfreqs)
      tb.close()
      tb.open(os.path.join(gain,'ANTENNA'))
      tabants = tb.getcol('NAME')
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
        if tb.info()['subType'] == 'B TSYS':
          kind[-1].append(2)
        else:
          kind[-1].append(1)


   #   antrow = np.array([allants.index(tabants[ai]) for ai in tb.getcol('ANTENNA1')[spmask]])[tsort]

      antrow = []
      for ai in tb.getcol('ANTENNA1')[spmask]:
        if tabants[ai] in allants: 
          antrow.append(allants.index(tabants[ai]))
        else:
          antrow.append(-1)
      antrow = np.array(antrow)[tsort]



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
          if gainmode[i][j] in ['G','S']:
            dd0 = data[0,:,:]
            dd1 = data[1,:,:]
          else:  # DUAL-POL GAIN FORCED TO 'T' MODE:
            Aux = np.sqrt(data[0,:,:]*data[1,:,:])
            dd0 = Aux
            dd1 = Aux
        else:  # A GAIN ALREADY IN MODE 'T'
          dd0 = data[0,:,:]
          dd1 = data[0,:,:]
        antrowant = antrow==ant
        dims = np.shape(dd0[:,antrowant])
        isFlagged=False
   # All antennas MUST have the re-ref XY0 phase, even if not used 
   # in the pol. calibration!
        if ".XY0" in gain:
          if dims[1]==0:
            antrowant = antrow==refants[0]
            dims = np.shape(dd0[:,antrowant])
        else:
          if dims[1]==0:
            dims = (dims[0],1)
            isFlagged=True
            antrowant = antrow==refants[0]
       #     ant=refants[0]
        gaindata[i][j][-1].append(np.zeros(np.shape(trow[antrowant])).astype(np.float64))
        gaindata[i][j][-1].append(np.ones(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.ones(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[i][j][-1].append(np.zeros(dims).astype(np.bool))
        if not isFlagged:
         gaindata[i][j][-1][0][:] = trow[antrowant]
         if j==0:
          gaindata[i][j][-1][1][:] = np.abs(dd0[:,antrowant])
         else: # CHANGE TO = 1.0 FOR TESTING:
          gaindata[i][j][-1][1][:] = np.abs(dd0[:,antrowant])
         gaindata[i][j][-1][2][:] = np.angle(dd0[:,antrowant])
         unwrap(gaindata[i][j][-1][2]) #, check=ant<3)
         gaindata[i][j][-1][3][:] = np.abs(dd1[:,antrowant])
         gaindata[i][j][-1][4][:] = np.angle(dd1[:,antrowant])
         unwrap(gaindata[i][j][-1][4]) #, check=ant<3)
         gaindata[i][j][-1][5][:] = flags[:,antrowant]

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


  if plotFringe and len(plotIF)==0:
    plotIF = list(map(int,doIF))

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


  if amp_norm>0.0:
    os.system('rm -rf POLCONVERT.GAINS')

  if len(plotIF)>0:
    os.system('rm -rf CONVERSION.MATRIX; mkdir CONVERSION.MATRIX')
    os.system('rm -rf FRINGE.PEAKS; mkdir FRINGE.PEAKS')
    os.system('rm -rf FRINGE.PLOTS; mkdir FRINGE.PLOTS')
    os.system('rm -rf POLCONVERT.FRINGE; mkdir POLCONVERT.FRINGE')


  printMsg("\n###\n### Going to PolConvert\n###")

  doAmpNorm = amp_norm>0.0
  didit = PC.PolConvert(nALMA, plotIF, plotAnt, len(allants), doIF, swapXY, ngain, NSUM, kind, gaindata, dtdata, OUTPUT, linAntIdx, plRan, Ran, allantidx, nphtimes, antimes, refants, asdmtimes,  doTest, doSolve, doConj, doAmpNorm, PrioriGains, np.array(XYdelF), metadata, soucoords, antcoords, antmounts, isLinear,calfield,timeranges)

  printMsg("\n###\n### Done with PolConvert (status %d).\n###" % (didit))


  if didit != 0:
    printError("\n\n ERROR IN POLCONVERT!\n\n")

# GENERATE ANTAB FILE(s):

  DPFU = float(amp_norm)

  if doAmpNorm:
    printMsg('Generating ANTAB file(s).')
    try:
      gfile = open("POLCONVERT.GAINS")
    except:
      printError("No gain file written!")

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
        Tsys[itime,ii+1] = datum[1]*DPFU

     outf = open("POLCONVERT_STATION%i.ANTAB"%j,"w")
     print >> outf,"GAIN AA  ELEV DPFU=%.3f   FREQ=10,100000"%DPFU
     print >> outf,"POLY=1.0000E+00"
     print >> outf,"/"
     print >> outf,"TSYS AA  FT=1.0  TIMEOFF=0"
     print >> outf,"INDEX= "+', '.join(['\'L%i|R%i\''%(i+1,i+1) for i in range(len(doIF))])
     print >> outf,"/"
     fmt0 = "%i %i:%2.4f  "
     # boost field width to retain significant figures
     fmt1 = "%10.4f  "*len(doIF)
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
         print >> outf, currT + fmt1%tuple(entry[1:])
     print >> outf, "/"
     outf.close()




  tac = time.time()

  printMsg('PolConvert took %.1f seconds.'%(tac-tic))











































# SOLVE FOR THE CROSS-POLARIZATION GAINS:
  if doSolve >= 0.0:

   CGains = {'XYadd':{},'XYratio':{}}

#   solveMethod = 'Levenberg-Marquardt'
#   fitMethod = 'COBYLA'   # 'Newton-CG'  # 'nelder-mead'
   fitMethod = solveMethod

   useCov = False
   if fitMethod=='Levenberg-Marquardt':
     useCov = True

# Fine-tunning parameters for Levenberg-Marquardt:
   LMLambda = 1.e-3
   KFacRaise = 5.0  # 2.0
   KFacDecr = 10.0   # 3.0
   maxErr = 1.e-5
   maxIter = 20  # Per gain.


# Load the solver library:
   try: 
     import _PolGainSolve as PS
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
       import _PolGainSolve as PS
       goodclib = True
       print '\nC++ shared library loaded successfully\n'
     except:
       goodclib=False



############################################################
# Levenberg-Marquardt minimizer of the GCPFF problem:
   def LMMin(p0,Ch0,Ch1):

     MAXIT = maxIter*len(fitAnts)
     relchange = 1.0
     Gchange = 1.0

     i = 0

     # First iteration:
     pini = np.array(p0)

     ptst0 = np.copy(pini)
     ptst1 = np.copy(pini)

     LMTune = LMLambda

#     currChi2,TheorImpr_0 = PS.GetChi2(ptst0,IFlist,fitAnts,Ch0,Ch1,solveAmp,LMTune)
#     Chi2_0,TheorImpr_0 = PS.GetChi2(ptst0,IFlist,fitAnts,Ch0,Ch1,solveAmp,-1.0)

     minChi2 = 0.0 
     minGains = np.copy(pini)

     currP = np.copy(pini)

     while i<MAXIT:

       ptst0 = np.copy(currP)
       currChi2 = PS.GetChi2(ptst0,LMTune,Ch0,Ch1)
       Chi2_0 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1)

       if i==0 or currChi2<minChi2:
         minChi2 = currChi2
         minGains[:] = currP


       i += 1

       while Chi2_0>currChi2: 

         i += 1     
         LMTune *= KFacRaise
         ptst0 = np.copy(currP)

         Chi2_ini = PS.GetChi2(ptst0,LMTune,Ch0,Ch1)
         Chi2_0 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1)
         if i>=MAXIT:
           break

       relchange = (currChi2 - Chi2_0)/Chi2_0

# No improvement:
       if currChi2<minChi2:
         minChi2 = currChi2
         minGains[:] = currP

# Improvement:
       if Chi2_0<currChi2:
         currP = np.copy(ptst0)
         currChi2 = Chi2_0
# Absolute improvement:
         if Chi2_0<minChi2:
           minChi2 = Chi2_0
           minGains[:] = ptst0



       if i>=MAXIT or np.abs(relchange)<maxErr:
         break
    

       ptst0 = np.copy(currP)
       Chi2_ini = PS.GetChi2(ptst0,LMTune,Ch0,Ch1)
       Chi2_0 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1)

       i += 1

       if Chi2_0 > currChi2:
         LMTune *= KFacRaise

       else:         

         while True:   # Chi2_0<currChi2: 
           i += 1
           LMTune /= KFacDecr
           ptst0 = np.copy(currP)
           Chi2_ini = PS.GetChi2(ptst0,LMTune,Ch0,Ch1)
           Chi2_1 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1)

           if Chi2_1<minChi2:
             minChi2 = Chi2_1
             minGains = np.copy(ptst0)
             currP = np.copy(ptst0)
             currChi2 = Chi2_1

           if Chi2_1<Chi2_0 and i<=MAXIT:
             Chi2_0 = Chi2_1
             ptst1[:] = ptst0
             currP = np.copy(ptst0)
             currChi2 = Chi2_1
           else:
             break

         relchange = (currChi2 - Chi2_0)/Chi2_0
         LMTune *= KFacDecr  # Come back to state of last successful decrease

         if i>=MAXIT or np.abs(relchange)<maxErr:
           break




     if i >= MAXIT:
        printMsg("\n WARNING! Gain estimate did NOT converge! \n   Potential problem in IF(s): %s | Chan(s): %i-%i\n     Check the outputs!\n"%(','.join(map(str,IFlist)),Ch0,Ch1-1))
        printMsg("\n    Best error: %.3e in ChSq /  %.3e in gains\n"%(np.abs(relchange),Gchange))


     return minGains








   if goodclib:

    selAnts = np.array(calAnts,dtype=np.int32)

    doSolveD = float(doSolve)

    if doSolveD>0.0:
      fitAnts = list(calAnts)
    else:
      fitAnts = list(linAntIdx)


    cAnts = np.array(calAnts,dtype=np.int32)
    lAnts = np.array(linAntIdx,dtype=np.int32)
    MySolve = PS.PolGainSolve(doSolveD,solint,selAnts,lAnts)

    AllFreqs = []
    print '\n\n'
    for pli in plotIF:
      printMsg("Reading back IF #%i"%pli)
      file1 = "POLCONVERT.FRINGE/OTHERS.FRINGE_%i"%pli
      file2 = "POLCONVERT.FRINGE/POLCONVERT.FRINGE_%i"%pli
      success = PS.ReadData(pli, file1, file2)
      NScan = PS.GetNScan(0)
      if success != 0:
        printError('Failed PolGainSolve: ERROR %i'%success)
      AllFreqs.append(PS.GetIFs(pli))

    MaxChan = max([np.shape(pp)[0] for pp in AllFreqs])

    printMsg("\nWill now estimate the residual cross-polarization gains.\n")



# ESTIMATE RATES FOR ALL STATIONS (PLOTANT IS THE REFERENCE):

    dropAnt = calAnts.index(plotAnt)
    rateAnts = calAnts[:dropAnt] + calAnts[dropAnt+1:]
    printMsg("\n Estimate antenna delays & rates\n")
    for nsi in range(NScan):
      PS.DoGFF(rateAnts,npix,True,nsi)


# BP MODE:
    if solint[0] != 0:
     printMsg("\n Estimate antenna cross-pol gains: BP mode\n")

     ChAv = abs(solint[0]) 
     for ci in fitAnts:
       CGains['XYadd'][ci] = []
       CGains['XYratio'][ci] = []
     for plii,pli in enumerate(plotIF):
       Nchans = np.shape(AllFreqs[plii])[0]
       temp = [np.zeros(Nchans,dtype=np.complex64) for ci in fitAnts]
       BPChan = range(0,Nchans,ChAv)
       if BPChan[-1]<Nchans-1:
         BPChan.append(Nchans-1)
       BPChan = np.array(BPChan,dtype=np.int32)
       Npar = len(fitAnts)*{True:2,False:1}[solveAmp]
       laux = [pli]
       PS.SetFit(Npar,laux,fitAnts,solveAmp,solveQU,Stokes,useCov)
       for chran in range(len(BPChan)-1):
         if chran==0 and plii==0:
           p0 = []
           for ci in fitAnts:
             if solveAmp:
               p0 += [ 1.0, 0.0]
             else:
               p0 += [0.0]
         else:
           p0 = list(myfit)

         laux = [pli]
         sys.stdout.write('\r Apply rates and estimate cross-gains for IF #%i, channels %i to %i   '%(pli,BPChan[chran],BPChan[chran+1]-1))
         sys.stdout.flush()

         if fitMethod not in scipyMethods:
           myfit = LMMin(p0,BPChan[chran],BPChan[chran+1])
         else:
           mymin = spopt.minimize(PS.GetChi2,p0,args=(-1.0, BPChan[chran],BPChan[chran+1]),method=fitMethod)

       #    mymin = spopt.minimize(PS.GetChi2,p0,args=(laux,fitAnts, BPChan[chran],BPChan[chran+1],solveAmp,-1.0,useCov),method=fitMethod)

           myfit = mymin.values()[5]

         for ci,calant in enumerate(fitAnts):

           if solveAmp:
# AMP+PHASE SPACE:
             temp[ci][BPChan[chran]:BPChan[chran+1]+1]= (myfit[2*ci]*np.exp(1.j*myfit[2*ci+1]))
           else:
             temp[ci][BPChan[chran]:BPChan[chran+1]+1]= np.exp(1.j*myfit[ci])

       for ci,calant in enumerate(fitAnts):
         CGains['XYratio'][calant].append(list(1./np.abs(temp[ci])))
         CGains['XYadd'][calant].append(list(-180./np.pi*np.angle(temp[ci])))


# MBD MODE:
    else:
      p0 = []
      for ci in fitAnts:
        if solveAmp:
          p0 += [ 1.0, 0.0]
        else:
          p0 += [0.0]
      for ci in fitAnts:
        p0 += [0.0] 
      laux = list(plotIF)
      PS.SetFit(Npar,laux,fitAnts,solveAmp,solveQU,Stokes,useCov)
 #     print 'First Chi2: ', PS.GetChi2(np.array(p0),-1.0,0,MaxChan-1,solveAmp,solveQU,Stokes,-1.0,False)
      if fitMethod not in scipyMethods: #=='Levenberg-Marquardt':
        myfit = LMMin(p0,0,MaxChan-1)
      else:
        mymin = spopt.minimize(PS.GetChi2,p0,args=(-1.0, 0,MaxChan-1),method=fitMethod)
        myfit = mymin.values()[5]

      RefFreq = AllFreqs[0][0]
      for ci,calant in enumerate(fitAnts):
       CGains['XYadd'][calant] = []
       CGains['XYratio'][calant] = []
       for plii,pli in enumerate(plotIF):
        if solveAmp:
# RE+IM SPACE:
#          CrossGain = (myfit[2*ci] + 1.j*myfit[2*ci+1])*np.exp(1.j*(AllFreqs[plii]-RefFreq)*myfit[2*len(fitAnts)+ci]) 
# AMP+PHASE SPACE:
          CrossGain = (myfit[2*ci]*np.exp(1.j*myfit[2*ci+1]))*np.exp(1.j*(AllFreqs[plii]-RefFreq)*myfit[2*len(fitAnts)+ci])
        else:
          CrossGain = (np.exp(1.j*myfit[ci]))*np.exp(1.j*(AllFreqs[plii]-RefFreq)*myfit[len(fitAnts)+ci])

        CGains['XYadd'][calant].append(list(-180./np.pi*np.angle(CrossGain)))
        CGains['XYratio'][calant].append(list(1./np.abs(CrossGain)))



    fig = pl.figure()
    sub1 = fig.add_subplot(211)
    MaxG = 0.0
    color = ['r','g','b','k','m','y','c']
    symbol = ['o','^','x']
    Freq2Plot = np.concatenate(AllFreqs)/1.e9
    for antii,anti in enumerate(CGains['XYadd'].keys()):
      sub1.plot(Freq2Plot,np.concatenate([np.array(ll) for ll in CGains['XYadd'][anti]]),symbol[((anti-1)/len(color))%len(symbol)]+color[(anti-1)%len(color)],label='ANT. '+str(anti))
    sub2 = fig.add_subplot(212,sharex=sub1)
    for antii,anti in enumerate(CGains['XYratio'].keys()):
      toplot = np.concatenate([np.array(ll) for ll in CGains['XYratio'][anti]])
      MaxG = max(MaxG,np.max(toplot))
      sub2.plot(Freq2Plot,toplot,symbol[((anti-1)/len(color))%len(symbol)]+color[(anti-1)%len(color)],label='ANT. '+str(anti))




    sub1.set_ylim((-180.,180.))
    sub2.set_ylim((0.,1.1*MaxG))
    pl.setp(sub1.get_xticklabels(),'visible',False)
    Dnu = np.max(Freq2Plot)-np.min(Freq2Plot)
    sub1.set_xlim((np.min(Freq2Plot) - Dnu*0.1,np.max(Freq2Plot) + Dnu*0.45))
    sub2.set_xlim((np.min(Freq2Plot) - Dnu*0.1,np.max(Freq2Plot) + Dnu*0.45))
    sub2.set_ylim((0.,2.5))


    sub2.legend(numpoints=1)
    sub1.set_ylabel('Cross-Phase (deg.)')
    sub2.set_ylabel('Cross-Amp (Norm.)')
    sub2.set_xlabel('Frequency (GHz)')


    fig.suptitle('CROSS-POLARIZATION GAINS')
    pl.savefig('Cross-Gains.png')
    pl.show()

    PS.FreeData()

   else:
    printMsg("\n\n  doSolve can ONLY work with the source was compiled with DO_SOLVE=True\n  PLEASE, RECOMPILE!\n\n")

  else:

    CGains = None
































# PLOT FRINGES:

  if plotFringe and didit==0:



   fig = pl.figure(figsize=(12,6))

   fig2 = pl.figure()

   fringeAmps = {} 
   fringeAmpsMix = {} 
   ResidGains = {} 
   MixedCalib = {} 


# Filter out IFs with no data:
   GoodIFs = []
   for pli in plotIF:
     if os.stat("POLCONVERT.FRINGE/POLCONVERT.FRINGE_%i"%pli).st_size>10:
       GoodIFs.append(pli)
     else:
       printMsg("WARNING! IF %i was NOT polconverted properly\n"%pli)

   for pli in GoodIFs:

    print '\n\n'
    printMsg("Plotting selected fringe for IF #%i"%pli)

    frfile = open("POLCONVERT.FRINGE/POLCONVERT.FRINGE_%i"%pli,"rb")


    alldats = frfile.read(4)
    nchPlot = stk.unpack("i",alldats[:4])[0]
    dtype = np.dtype([("JDT",np.float64),("ANT1",np.int32),("ANT2",np.int32),
                      ("PANG1",np.float64),("PANG2",np.float64), ("MATRICES",np.complex64,12*nchPlot)])

# There is a silly bug in Python 2.7, which generates
# an "integer is required" error in the first try to read:
    try:
      fringe = np.fromfile(frfile,dtype=dtype)
    except:
      fringe = np.fromfile(frfile,dtype=dtype)

    frfile.close()


    for ant1 in linAntIdx:

     if pli == plotIF[0]:
      MixedCalib[ant1] = {}
      fringeAmps[ant1] = []
      fringeAmpsMix[ant1] = []
      ResidGains[ant1] = []

     for ant2 in [plotAnt]:

      AntEntry1 = np.logical_and(fringe[:]["ANT1"] == ant1,fringe[:]["ANT2"] == ant2)
      AntEntry2 = np.logical_and(fringe[:]["ANT2"] == ant1,fringe[:]["ANT1"] == ant2)
      AntEntry = np.logical_or(AntEntry1,AntEntry2)

      if np.sum(AntEntry)>0:

       if pli == plotIF[0]:
        MixedCalib[ant1][ant2] = []

# This is to store all fringes with linear-feeds involved:

       if nchPlot > 0 and len(fringe)>0:

         uncal = [(fringe[AntEntry]["MATRICES"])[:,i::12] for i in range(4)]
         cal = [(fringe[AntEntry]["MATRICES"])[:,i::12] for i in range(4,8)]
         Kmat = [(fringe[AntEntry]["MATRICES"])[:,i::12] for i in range(8,12)]

         rchan = np.shape(uncal[0])[0] 

# Zoom for the image plots:
         ToZoom = min(rchan,nchPlot,npix)

         t0 = (nchPlot - ToZoom)/2
         t1 = (nchPlot + ToZoom)/2

         Ch0 = (rchan - ToZoom)/2
         Ch1 = (rchan + ToZoom)/2

# Fringes in delay-rate space:
         if ant2==plotAnt or ant1==plotAnt:
           FRRu = np.fft.fftshift(np.fft.fft2(uncal[0]))
           FRLu = np.fft.fftshift(np.fft.fft2(uncal[1]))
           FLRu = np.fft.fftshift(np.fft.fft2(uncal[2]))
           FLLu = np.fft.fftshift(np.fft.fft2(uncal[3]))

           RRu = np.abs(FRRu) 
           RLu = np.abs(FRLu) 
           LRu = np.abs(FLRu) 
           LLu = np.abs(FLLu) 

# Calibration matrix (in frequency-time space)

           MAXK = max(map(np.max,map(np.abs,Kmat)))    
           MINK = min(map(np.min,map(np.abs,Kmat)))   

# Peaks to scale plots:

           RMAXu = np.unravel_index(np.argmax(RRu+RLu+LRu+LLu),np.shape(RRu))
           MAXu = max([RRu[RMAXu],RLu[RMAXu],LRu[RMAXu],LLu[RMAXu]])
           MAXmix = [np.max(RRu),np.max(RLu),np.max(LRu),np.max(LLu)]
           PEAK = np.unravel_index(np.argmax(RRu+LLu),np.shape(RRu))


         RRVis = np.fft.fftshift(np.fft.fft2(cal[0]))
         RLVis = np.fft.fftshift(np.fft.fft2(cal[1]))
         LRVis = np.fft.fftshift(np.fft.fft2(cal[2]))
         LLVis = np.fft.fftshift(np.fft.fft2(cal[3]))

         RR = np.abs(RRVis) 
         RL = np.abs(RLVis) 
         LR = np.abs(LRVis) 
         LL = np.abs(LLVis) 

# Peaks of converted fringes:
         RMAX = np.unravel_index(np.argmax(RR+LL),np.shape(RRVis))
         MAXVis = [RRVis[RMAX],RLVis[RMAX],LRVis[RMAX],LLVis[RMAX]]
         MixedCalib[ant1][ant2].append([np.array(MM) for MM in MAXVis])
         MAXl = [RR[RMAX],RL[RMAX],LR[RMAX],LL[RMAX]]
         MAX = max(MAXl)



# Plot fringes:
         if ant2==plotAnt or ant1==plotAnt:

          fig.clf()
          ratt = 1.0   

          fig.subplots_adjust(left=0.02,right=0.98,wspace=0.05,hspace=0.2)
 
          try:
            del sub0
            del sub1
            del sub2
            del sub3
            del sub4
            del sub5
            del sub6
            del sub7
            del sub
            del cbar
          except:
            pass

          sub0 = fig.add_subplot(241)
          sub0.imshow(np.abs(RRu[Ch0:Ch1,t0:t1]),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub0.set_title('XR mixed')
          pl.setp(sub0.get_xticklabels(),visible=False)
          pl.setp(sub0.get_yticklabels(),visible=False)

          sub1 = fig.add_subplot(242,sharex=sub0,sharey=sub0)
          sub1.imshow(np.abs(RLu[Ch0:Ch1,t0:t1]),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub1.set_title('XL mixed')
          pl.setp(sub1.get_xticklabels(),visible=False)
          pl.setp(sub1.get_yticklabels(),visible=False)

          sub2 = fig.add_subplot(243,sharex=sub0,sharey=sub0)
          sub2.imshow(RR[Ch0:Ch1,t0:t1],vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub2.set_title('RR cal')
          pl.setp(sub2.get_xticklabels(),visible=False)
          pl.setp(sub2.get_yticklabels(),visible=False)

          sub3 = fig.add_subplot(244,sharex=sub0,sharey=sub0)
          sub3.imshow(RL[Ch0:Ch1,t0:t1],vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub3.set_title('RL cal')
          pl.setp(sub3.get_xticklabels(),visible=False)
          pl.setp(sub3.get_yticklabels(),visible=False)


          sub4 = fig.add_subplot(245,sharex=sub0,sharey=sub0)
          sub4.imshow(np.abs(LRu[Ch0:Ch1,t0:t1]),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub4.set_title('YR mixed')
          pl.setp(sub4.get_xticklabels(),visible=False)
          pl.setp(sub4.get_yticklabels(),visible=False)

          sub5 = fig.add_subplot(246,sharex=sub0,sharey=sub0)
          sub5.imshow(np.abs(LLu[Ch0:Ch1,t0:t1]),vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub5.set_title('YL mixed')
          pl.setp(sub5.get_xticklabels(),visible=False)
          pl.setp(sub5.get_yticklabels(),visible=False)

          sub6 = fig.add_subplot(247,sharex=sub0,sharey=sub0)
          sub6.imshow(LR[Ch0:Ch1,t0:t1],vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub6.set_title('LR cal')
          pl.setp(sub6.get_xticklabels(),visible=False)
          pl.setp(sub6.get_yticklabels(),visible=False)

          sub7 = fig.add_subplot(248,sharex=sub0,sharey=sub0)
          sub7.imshow(LL[Ch0:Ch1,t0:t1],vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub7.set_title('LL cal')
          pl.setp(sub7.get_xticklabels(),visible=False)
          pl.setp(sub7.get_yticklabels(),visible=False)

          fig.suptitle('DELAY-RATE FRINGE FOR IF %i (BASELINE TO ANT #%i) FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i'%tuple([pli,plotAnt]+plotRange))
          fig.savefig('FRINGE.PLOTS/Fringe.plot.ANT%i-%i.IF%i.png'%(ant1,ant2,pli))



# Plot calibration matrix:

          ratt = float(np.shape(Kmat[0])[1])/float(np.shape(Kmat[0])[0])

          fig2.clf()

          fig2.subplots_adjust(right=0.8)
          cbar_ax = fig2.add_axes([0.85, 0.15, 0.05, 0.7])

          sub = fig2.add_subplot(221)
          im = sub.imshow(np.abs(Kmat[0]),vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(222)
          sub.imshow(np.abs(Kmat[1]),vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(223)
          sub.imshow(np.abs(Kmat[2]),vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(224)
          sub.imshow(np.abs(Kmat[3]),vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          cbar = fig2.colorbar(im, cax=cbar_ax)
          cbar.set_label("Amplitude (Norm)")
          pl.suptitle('CAL. MATRIX FOR IF %i FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i - FREQ = X ; TIME = Y'%tuple([pli]+plotRange))

          pl.savefig('CONVERSION.MATRIX/Kmatrix_AMP_IF%i-ANT%i.png'%(pli,ant1))

          fig2.clf()

          fig2.subplots_adjust(right=0.8)
          cbar_ax = fig2.add_axes([0.85, 0.15, 0.05, 0.7])

          sub = fig2.add_subplot(221)
          im = sub.imshow(180./np.pi*np.angle(Kmat[0]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(222)
          sub.imshow(180./np.pi*np.angle(Kmat[1]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(223)
          sub.imshow(180./np.pi*np.angle(Kmat[2]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(224)
          sub.imshow(180./np.pi*np.angle(Kmat[3]),vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          cbar = fig2.colorbar(im, cax=cbar_ax)
          cbar.set_label("Phase (deg.)")
          pl.suptitle('CAL. MATRIX FOR IF %i FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i - FREQ = X ; TIME = Y'%tuple([pli]+plotRange))

          pl.savefig('CONVERSION.MATRIX/Kmatrix_PHAS_IF%i-ANT%i.png'%(pli,ant1))
 


# Estimate the Dynamic range:
          DLL = np.max(LL)/np.std(np.sort(LL.flatten())[:-nchPlot])
          DRR = np.max(RR)/np.std(np.sort(RR.flatten())[:-nchPlot])
          DRL = np.max(RL)/np.std(np.sort(RL.flatten())[:-nchPlot])
          DLR = np.max(LR)/np.std(np.sort(LR.flatten())[:-nchPlot])

          DLLu = np.max(LLu)/np.std(np.sort(LLu.flatten())[:-nchPlot])
          DRRu = np.max(RRu)/np.std(np.sort(RRu.flatten())[:-nchPlot])
          DRLu = np.max(RLu)/np.std(np.sort(RLu.flatten())[:-nchPlot])
          DLRu = np.max(LRu)/np.std(np.sort(LRu.flatten())[:-nchPlot])

          RLRatio = (MAXl[0]/MAXl[3])/(MAXl[2]/MAXl[1])

          toprint = [pli,MAXl[0]/MAX,DRR,MAXl[3]/MAX,DLL,MAXl[1]/MAX,DRL,MAXl[2]/MAX,DLR,MAX/len(fringe),RLRatio]
          fringeAmps[ant1].append([pli,MAXl[0],MAXl[0]/DRR,MAXl[3],MAXl[3]/DLL,MAXl[1],MAXl[1]/DRL,MAXl[2],MAXl[2]/DLR,RLRatio])
          fringeAmpsMix[ant1].append([pli,MAXmix[0],MAXmix[0]/DRRu,MAXmix[3],MAXmix[3]/DLLu,MAXmix[1],MAXmix[1]/DRLu,MAXmix[2],MAXmix[2]/DLRu])


          pmsg = '\n\n\nFOR IF #%i. NORM. FRINGE PEAKS: \n  RR: %.2e ; SNR: %.1f \n  LL: %.2e ; SNR: %.1f \n  RL: %.2e ; SNR: %.1f \n  LR: %.2e ; SNR: %.1f\n\n AMPLITUDE: %.2e\nRL/LR Norm.: %.2e\n\n\n'%tuple(toprint)

          pfile = open('FRINGE.PEAKS/FRINGE.PEAKS%i-ANT%i.dat'%(pli,ant1),'w' )
          printMsg(pmsg)
          pfile.write(pmsg)


          NUM =  np.angle(FRRu[RMAXu]*np.average(Kmat[2]))
          DEN =  np.angle(FLRu[RMAXu]*np.average(Kmat[3]))
          optph = (180./np.pi*((NUM-DEN)-np.pi))%360.
          if optph > 180.:
            optph -= 360.
          pmsg = '\n\nFor RL: optimum X/Y phase is %.1f deg.'%(optph)
          NUM =  np.angle(FRLu[RMAXu]*np.average(Kmat[0]))
          DEN =  np.angle(FLLu[RMAXu]*np.average(Kmat[1]))
          optph = (180./np.pi*((NUM-DEN)-np.pi))%360.
          if optph > 180.:
            optph -= 360.
          pmsg += '\nFor LR: optimum X/Y phase is %.1f deg.\n'%(optph) 

          printMsg(pmsg) 

          pfile.write(pmsg)

    try:
      del uncal[3],uncal[2],uncal[1],uncal[0],uncal
      del cal[3],cal[2],cal[1],cal[0],cal
      del RRVis, RLVis, LRVis, LLVis, RR, LL, RL, LR
    except:
      pass

    if ant2==plotAnt or ant1==plotAnt:
     try:
       del FRRu, FRLu, FLRu, FLLu
       del RRu, RLu, LRu, LLu
       del Kmat[3],Kmat[2],Kmat[1],Kmat[0],Kmat
     except:
       pass
    try:
      del fringe
    except:
      pass
    gc.collect()


 #  try:
   for thisAnt in linAntIdx:
     fig.clf()
     pl.figure(fig.number)
     fig.subplots_adjust(left=0.1,right=0.95,bottom=0.15,top=0.95)
     sub = fig.add_subplot(121)
    # CIRCULAR-CIRCULAR FRINGE AMPLITUDES (MUST NORMALIZE FROM FFT):
     CONVAMP = np.array(fringeAmps[thisAnt])
     CONVAMP[:,1:-1] *= 1.e3/(rchan*nchPlot)
    # MIXED FRINGE APLITUDES (MUST NORMALIZE FROM FFT):
     MIXAMP = np.array(fringeAmpsMix[thisAnt])
     MIXAMP[:,1:] *= 1.e3/(rchan*nchPlot)


     sub.plot(MIXAMP[:,0],MIXAMP[:,1],'sr',label='XR',markersize=15)
     sub.plot(MIXAMP[:,0],MIXAMP[:,3],'dg',label='YL',markersize=15)
     sub.plot(MIXAMP[:,0],MIXAMP[:,5],'+r',label='XL',markersize=15)
     sub.plot(MIXAMP[:,0],MIXAMP[:,7],'xg',label='YR',markersize=15)
     pl.legend(numpoints=1)

     sub.errorbar(MIXAMP[:,0],MIXAMP[:,1],MIXAMP[:,2],linestyle='None',fmt='k')
     sub.errorbar(MIXAMP[:,0],MIXAMP[:,3],MIXAMP[:,4],linestyle='None',fmt='k')
     sub.errorbar(MIXAMP[:,0],MIXAMP[:,5],MIXAMP[:,6],linestyle='None',fmt='k')
     sub.errorbar(MIXAMP[:,0],MIXAMP[:,7],MIXAMP[:,8],linestyle='None',fmt='k')

     pl.xlabel('IF NUMBER')
     pl.ylabel(r'AMP (CORR. $\times 10^3$)')
     sub2 = fig.add_subplot(122,sharex=sub,sharey=sub)
     sub2.plot(CONVAMP[:,0],CONVAMP[:,1],'sr',label='RR',markersize=15)
     sub2.plot(CONVAMP[:,0],CONVAMP[:,3],'dg',label='LL',markersize=15)
     sub2.plot(CONVAMP[:,0],CONVAMP[:,5],'+r',label='RL',markersize=15)
     sub2.plot(CONVAMP[:,0],CONVAMP[:,7],'xg',label='LR',markersize=15)
     pl.legend(numpoints=1)

     sub2.errorbar(CONVAMP[:,0],CONVAMP[:,1],CONVAMP[:,2],linestyle='None',fmt='k')
     sub2.errorbar(CONVAMP[:,0],CONVAMP[:,3],CONVAMP[:,4],linestyle='None',fmt='k')
     sub2.errorbar(CONVAMP[:,0],CONVAMP[:,5],CONVAMP[:,6],linestyle='None',fmt='k')
     sub2.errorbar(CONVAMP[:,0],CONVAMP[:,7],CONVAMP[:,8],linestyle='None',fmt='k')
     pl.setp(sub2.get_yticklabels(),visible=False)
     pl.xlabel('IF NUMBER')
     dChan = max(plotIF)-min(plotIF) + 1
     pl.xlim((min(plotIF)-dChan*0.2,max(plotIF)+0.4*dChan))
     pl.ylim((0.,1.1*np.max(CONVAMP[:,[1,3,5,7]])))
     fig.suptitle(jobLabel(DiFXinput)+' ANT: %i v %i'%(thisAnt,plotAnt))
     fig.savefig('FRINGE.PLOTS/ALL_IFs_ANT_%i_%i.png'%(thisAnt,plotAnt))

     fig3 = pl.figure()
     sub1 = fig3.add_subplot(111)
     sub1.plot(CONVAMP[:,0],CONVAMP[:,-1],'sk')
     RatioError = CONVAMP[:,-1]*np.sqrt((CONVAMP[:,2]/CONVAMP[:,1])**2.+(CONVAMP[:,4]/CONVAMP[:,3])**2.+(CONVAMP[:,6]/CONVAMP[:,5])**2.+(CONVAMP[:,8]/CONVAMP[:,7])**2.)
     sub1.errorbar(CONVAMP[:,0],CONVAMP[:,-1],RatioError,linestyle='None',fmt='k')
     sub1.plot([min(CONVAMP[:,0])-1, max(CONVAMP[:,0])+1], [1, 1], 'r')

     pl.xlabel('IF NUMBER')
     pl.ylabel('(RR/LL)/(LR/RL)')
     pl.xlim((min(CONVAMP[:,0])-1,max(CONVAMP[:,0])+1))
     pl.ylim((0.,np.max(CONVAMP[:,-1])*1.05))

     fig3.suptitle(jobLabel(DiFXinput)+' ANT: %i v %i'%(thisAnt,plotAnt))
     fig3.savefig('FRINGE.PLOTS/RL_LR_RATIOS_ANT_%i_%i.png'%(thisAnt,plotAnt))




  else:

      printMsg('NO DATA TO PLOT!') 






  printMsg('Please, check the PolConvert.log file for special messages.',dolog=False)

  ofile = open('PolConvert.XYGains.dat','w')
  pk.dump(CGains,ofile)
  ofile.close()
#  return CGains


