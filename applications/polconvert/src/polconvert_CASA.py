# Copyright (c) Ivan Marti-Vidal 2012-2023
#               EU ALMA Regional Center. Nordic node.
#               Universitat de Valencia (Spain)
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

from __future__ import absolute_import
from __future__ import print_function
__version__ = "2.0.7  "  # 7 characters
date = 'Aug 14, 2023'


################
# Import all necessary modules. 

import os,sys,shutil,re
import gc
import time
import struct as stk
import scipy.optimize as spopt
import numpy as np
import pylab as pl
import datetime as dt
import pickle as pk

mypath = os.path.dirname(__file__)
sys.path.append(mypath)
import polconvert_standalone as PCONV




if sys.version_info.major < 3:
  try:
    from taskinit import *
    tb = gentools(['tb'])[0]
  except Exception as ex:
    print('unable to load casa tools in python2\n\n')
    raise ex
else:
  try:
    # ms is not actually used
    # from casatools import ms as ms_casa
    from casatools import table as tb_casa
    tb = tb_casa()
  except Exception as ex:
    print('unable to load casa tools in python3\n\n')
    raise ex
################

# this is the CASA xml-based command sequence:
# defaults are supplied & consistent with version 2.0.7
def polconvert(IDI='', OUTPUTIDI='', DiFXinput='', DiFXcalc='', doIF=[], linAntIdx=[1], 
               Range=[], ALMAant='', spw=-1, calAPP='', calAPPTime=[0.,5.], APPrefant='', 
               gains=[['NONE']], interpolation=[], gainmode=[], XYavgTime=0.0, 
               dterms=['NONE'], amp_norm=0.01, XYadd={}, XYdel={}, XYratio={}, 
               usePcal={}, swapXY=[False], swapRL=False, feedRotation=[],
               correctParangle=False, IDI_conjugated=False, plotIF=-1,
               plotRange=[], plotAnt=-1, excludeAnts=[], excludeBaselines=[],
               doSolve=-1, solint=[1,1], doTest=True, npix=50, solveAmp=True,
               solveMethod='COBYLA', calstokes=[1.,0.,0.,0.], calfield=-1,
               saveArgs=False):

  """ POLCONVERT - CASA INTERFACE VERSION 2.0.7.

Converts VLBI visibilities from mixed-polarization (linear-circular)
into a circular basis. Works with single VLBI stations as well as with
calibrated phased arrays (i.e., phased ALMA).

  IDI: Input FITS-IDI file with VLBI visibilities. It can also be a
       directory containing SWIN files from DiFX.

  OUTPUTIDI: Output FITS-IDI file (or SWIN directory). If equal to 
             IDI, the file(s) will be overwritten.

  DiFXinput: If SWIN files are being converted, this must be the 
             *.input file used by DiFX.

  DiFXcalc: If SWIN files are being converted, this must be the 
            *.calc file used by DiFX.  This is optional, but if it is not
            provided, the cross-polarization gain estimates may be incorrect
            if doSolve>0.

  doIF = List of IFs to convert (starts from 1). Default means all.

  linAntIdx: List of indices of the linear-polarization antennas 
             in the IDI/SWIN file (lowest index starts with 1).
             It can also be a list of the antenna code names.

  Range: Time range to convert (integer list; AIPS format). Default
         (empty list) means all data.

  ALMAant: If ALMA has been used, this is the antenna table from 
           the MS with the intra-ALMA visibilities.

  spw: Spectral window in ALMA visibilities that contains the VLBI band. If
       negative, the program will derive it automatically.

  calAPP: If ALMA has been used, this is the combined 
          ASDM_CALAPPPHASE table from the ASDM. The list of 
          measurement sets can also be given (so the table is 
          concatenated from all of them).

  calAPPTime: Time shift and time tolerance (in sec) for the 
              CALAPPPHASE table obtained from the ASDM.

  APPrefant: If not empty, re-reference the TelCal phases, assuming
             that the X-Y phase-difference table provided in 'gains'
             (see keyword below) uses APPrefant as the reference 
             antenna. Notice that the name of the gain table with 
             the X-Y phase differences has to contain the string 
             '.XY0'.

  gains: Gain tables to pre-calibrate the linear-pol VLBI stations 
         (one list of gains per linear-pol station).

  interpolation: Interpolation type to use (one per calibration 
                 table). Tells whether to apply linear or nearest
                 interpolation. Default is to use linear for all 
                 tables.

  gainmode: Mode of gain calibration to impose (one per calibration
            table). Default is 'T' for all tables, unless either 
            the string 'XY0', 'bandpass' or 'Gxyamp' appears in the
            table name. The gain types can be either 'G' (i.e., 
            split gains per polarization) or 'T' (i.e., combine 
            polarizations).

  XYavgTime: Re-compute the G-mode gains by adding a time smoothing
             of X-Y differences to the T-mode gains. Default is NOT
             to do this (i.e., use the G-mode gains as given). If 
             positive, use a running time average with this size 
             (in seconds).

  dterms: D-term tables to pre-calibrate the linear-pol VLBI 
          stations (one table per linear-pol station).

  amp_norm: If positive, normalize the amplitude correction to the
            X-Y average, and save the scaling factor (vs time) in 
            an external (ASCII) file (ANTAB format, assuming a 
            DPFU=amp_norm). If zero, or negative, apply the 
            amplitude correction as is.

  XYadd: Add manually a phase between X and Y before conversion (in
         deg.). This is a dictionary (antenna codes as keywords) with
         elements set to either a list of one value per IF OR a 
         list of lists (one value per channel, for each IF).

  XYdel: Add manually a multiband delay between X and Y before 
         conversion (in deg./chanel). This is a dictionary (antenna codes
         as keywords) with one value per linear-pol station.

  XYratio: Add manually an amplitude ratio between X and Y before 
           conversion (R=X/Y). Follows the same format as XYadd. If
           a negative value is given for an antenna, the X/Y ratio 
           will be estimated from its autocorrelations (the 
           spectrum for antenna i will be computed using a 
           running-median filter of width equal to -1/XYratio[i] of
           the IF bandwidth). If 0.0 is given for an antenna, the 
           ratio will be estimated from the phasecal amplitudes (as
           long as usePcal is True and there are pcal files 
           available; not supported for FITS-IDI).

  usePcal: Dictionary (antenna codes as keywords), whose elements 
           are booleans (i.e., one boolean per linear-polarization 
           station). If True, use the X-Y difference of phasecal 
           tones as an estimate of the X-Y cross-polarization 
           phase. Default means to NOT use the phasecals.

  swapXY: If true, swap the X-Y channels (one boolean per antenna).

  swapRL: Swap R-L of the OTHER antenna(s) when plotting the 
          fringes.

  feedRotation: Rotation angle of the feed (one value per antenna,
                in degrees). Default means zero degrees (so that 
                X is truly horizontal for the linear-pol. 
                antennas). These angles are used in the 
                gain-solver step.

  correctParangle: If True, the correction for parallactic angle
                   is applied to the converted antenna(s).

  IDI_conjugated: Assume a swap in the baseline defintion (i.e., 
                  conjugation) of the FITS-IDI file. This has NO 
                  effect on SWIN files.

  plotIF: IF index(es) to plot. Default means to NOT plot. An 
          empty list, [], means to plot ALL IFs being converted 
          (but do not forget to set plotRange and plotAnt!)

  plotRange: Time range to plot (integer list; AIPS format). 
             Default means to NOT plot.

  plotAnt: Index of the other antenna in the baseline to plot. 
           Default means to NOT plot.

  excludeAnts: List of antennas (i.e., list of antenna codenames)
               to NOT use in the cross-polarization gain 
               estimates.

  excludeBaselines: List of baselines (i.e., a list of lists of
                    two antenna codenames) to NOT use in the 
                    cross-polarization gain estimates.

  doSolve: If negative, do not estimate the cross-polarization 
           gains. If positive or zero, estimate the gains using
           a Global Cross-Pol Fringe Fitting (GCPFF). The gains
           are fitted with an error function (Chi Square) 
           defined as:

               sum( doSolve*(RR/LL-1)^2 + (RL^2 + LR^2) ), 

           so that doSolve=0 minimizes the cross-hand 
           polarizations (so it assumes a small linear 
           polarization of the source),  whereas doSolve >> 1
           assumes a negligible Stokes V.

  solint: If solint[0] null or negative, solve the 
          cross-polarization phase plus a multi-band delay (MBD).
          If not, solve in bandpass mode by averaging solint[0]
          channels per solution. Divide the solution time range
          (per scan) in solint[1] chunks (i.e., in solint[1] 
          subscans). I.e., if solint[1]==1, the fringes are fully
          averaged in time for each scan (but corrected for the 
          scan fringe rates) before the GPLFF condition is 
          computed. solint[2] is the minimum time jump (in 
          seconds) to split the data into different scans 
          (default: 50 seconds).

  doTest: If true, only compute (and eventually plot), the data,
          but leave OUTPUTIDI untouched.

  npix: Number of pixels for the fringe plots (and fringe search).
  
  solveAmp: If the cross-polarization gains are being estimated,
            solve also for the X/Y amplitude ratios.

  solveMethod: Method for the minimization of the Chi squared in
               the GCPFF. Can be, for instance: 'COBYLA'.

  calstokes: Stokes parameters, [I,Q,U,V] of the calibrator (of
             course, this is only used if doSolve is not negative).
             The total intensity is not needed in the calibration 
             (i.e., calstokes[0] can be left to 1.0, so that the 
             other parameters will correspond to fractional 
             polarization).

  calfield: If not negative, field ID of the calibrator (useful 
            if a time range covering several scans is being used
            in the GCPFF). If negative, use all data in the time
            range, regardless of the field ID.

  saveArgs: If true, arguments are saved in a file PolConvert_CASA.last
            (a pickled dictionary); note that nothing currently reads this file.
  """


############################################

# this turns into the verbosity argument of _PolConvert.so
  print('Entered polconvert_CASA::polconvert()')
  DEBUG = False

  if 'POLCONVERTDEBUG' in os.environ:
     if os.environ['POLCONVERTDEBUG'] == 'True': DEBUG = True
     else:                                       DEBUG = False
  print('DEBUG setting is ' + str(DEBUG))
  print('__name__ is ' + __name__)

# Auxiliary function: print error and raise exception:
  def printError(msg):
    print(msg,'\n') 
    lfile = open("PolConvert.log","a")
    print('\n'+msg+'\n', file=lfile)
    lfile.close()
    sys.stdout.flush()
    raise Exception(msg)

# Auxiliary function: print message (terminal and log):
  def printMsg(msg, doterm=True, dolog=True):
    if doterm:
      print(msg)
    if dolog:
      lfile = open("PolConvert.log","a")
      print(msg,file=lfile)
      sys.stdout.flush()
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
        allAnts = list(range(len(GallAnts)))

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
    print('\nDONE!\n\n') 











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
    XYData = []
    Mask = spwi==SPW

    for si in np.where(Mask)[0]:
      XYData.append(tb.getcell("CPARAM",si))
    XYData = np.array(XYData)

#    XYData = tb.getcol("CPARAM")
#    Mask = spwi==SPW

    NNu = np.shape(XYData)[1]
    NuPerChan = NNu/Nchan
    for ni in range(Nchan):
      XYData[0,ni*NuPerChan:(ni+1)*NuPerChan,Mask] *= np.exp(-1.j*(AvXY[ni]))

    for sii,si in enumerate(np.where(Mask)[0]):
      tb.putcell("CPARAM",si,XYData[sii])
   
#    tb.putcol("CPARAM",XYData)
    tb.close()

# Plot new vs. old XY-phases:
    Idx = np.where(Mask)[0][0]
    tb.open(XY0)
    XOrig = tb.getcell("CPARAM",Idx)[0,:]
    YOrig = tb.getcell("CPARAM",Idx)[1,:]
    tb.close()
    plfig.clf()
    plsub = plfig.add_subplot(111)
    plsub.plot(np.angle(XOrig/YOrig)*180./np.pi,'or',label='Original')

    tb.open("%s.REFANT_%s"%(XY0,REFANT))
    XOrig = tb.getcell("CPARAM",Idx)[0,:]
    YOrig = tb.getcell("CPARAM",Idx)[1,:]
    tb.close()
    plsub.plot(np.angle(XOrig/YOrig)*180./np.pi,'ob',label='Re-Referenced')

    pl.legend(loc=0,numpoints=1)
    plsub.set_ylim((-250,250))
    pl.savefig('%s_RE-REF_XYPHASES.png'%XY0)

# Auxiliary function: unwrap phases for time interpolation
  def unwrap(phases, check=False):

    dims = np.shape(phases)
    if dims[1]==0:  # Bandpass type
     for i in range(len(phases)-1):
      if phases[i+1]-phases[i] > np.pi:
        phases[i+1,:] -= 2.*np.pi
      elif phases[i+1]-phases[i] < -np.pi:
        phases[i+1,:] += 2.*np.pi
     if check:
       pl.figure()
       pl.plot(180./np.pi*phases)


    elif dims[0]>1:  # Bandpass-gain type
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
      print('\n\nMEAS SET %s:'%asd)
      tmin = np.min(time0)/86400.
      tmax = np.max(time1)/86400.
    mstmin = np.min(mstime)/86400.
    mstmax = np.max(mstime)/86400.
    printMsg('MS TIME RUNS FROM %8.5f TO %8.5f DAYS'%(mstmin,mstmax))
    if len(time0)>0:
     printMsg('FOR APP TABLE, TIME RUNS FROM %8.5f TO %8.5f DAYS'%(tmin,tmax))


    return timeranges


  tic = time.time()





#########################################
# DO SOME SANITY CHECKS OF PARAMETERS

  try:
    doSolve = float(doSolve)
  except:
    printError("ERROR! doSolve should be a float!")
  

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
    printMsg("WARNING: calAPP and ALMAant are not set.")
    printMsg("ALMA CAL TABLES DO NOT SEEM TO BE USED, ACCORDING TO USER.")
    printMsg("This is not a problem in some cases...(isPhased now False).")
    isPhased=False
  else:
    if not os.path.exists(calAPP) or not os.path.exists(ALMAant):
      printError("ERROR! calAPP and/or ALMAant WERE NOT FOUND!")
    else:
      isPhased = True


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





  if len(calAPPTime) != 2:
    printError("Bad format for calAPPTime. Should be a list of 2 floats!")

  try:
    CALAPPTSHIFT, CALAPPDT = [float(cc) for cc in calAPPTime]
  except:
    printError("Bad format for calAPPTime. Should be a list of 2 floats!")


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



#########
# Figure out times with unphased data:
    ADJ = tb.getcol('adjustToken')
    BBN = tb.getcol('basebandName')
    BBC = ['BB_%i'%i for i in range(1,5)]
    sc2flag = [[] for i in range(4)] # Currently, we assume 1 spw per BBC in the APP mode.
    scgood = [[] for i in range(4)] # Currently, we assume 1 spw per BBC in the APP mode.
    timeranges = [[] for i in range(4)] # Currently, we assume 1 spw per BBC in the APP mode.

    for j in range(4):
      sc2flag[j] = [i for i in range(len(ADJ)) if BBN[i]==BBC[j] and ('PHASE_UPDATED' not in ADJ[i]) and ('PHASE_NOCHANGE' not in ADJ[i])]
      scgood[j] = [i for i in range(len(ADJ)) if BBN[i]==BBC[j] and ('PHASE_UPDATED' in ADJ[i] or 'PHASE_NOCHANGE' in ADJ[i])]

    SUBSCANDUR = time1[sc2flag[0][0]] - time0[sc2flag[0][0]]
    sec = 1.

    for j in range(4):
     if len(sc2flag[j])>0:
      for si in sc2flag[j]:
        timerange = [time1[si]-SUBSCANDUR-sec, time1[si]+sec]
        if timerange not in timeranges[j]:
          timeranges[j].append(timerange)

     for si in scgood[j]:
      timerange = [time1[si]-SUBSCANDUR-sec,time0[si]+sec]
      if si-1 in sc2flag[j]:  
        if timerange not in timeranges[j]:
          timeranges[j].append(timerange)

# For testing:
    timerangesArr = [np.array(ti) for ti in timeranges]
#########


    nphant = tb.getcol('numPhasedAntennas')
    refs = tb.getcol('refAntennaIndex')
    asdmtimes = [time0,time1]
    phants = []
    for i in range(len(nphant)):
      phants.append(list(tb.getcell('phasedAntennas', rownr = i)))

    tb.close()

   except:
    printError('ERROR: INVALID calAPP TABLE CONTENT!')

   success = tb.open(ALMAant)
   if not success:
    printError('ERROR: NO VALID ANTENNA TABLE FOUND!')

   allants = list(tb.getcol('NAME'))
   allantidx = list(range(len(allants)))
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


  else:  # is(not)Phased

# Not a phased array. Dummy values for phasing info:
   allants = [1]
   allantidx = [0]
   antimes = [np.array([0.,1.e20])]
   nphtimes = [1]
   refants = np.array([0],dtype=np.int)
   asdmtimes = [np.array([0.]),np.array([1.e20])]
   timerangesArr = [np.array([0.,0.]) for i in range(4)]

######################



#######
# CHECK IF THIS IS A FITS-IDI FILE OR A SWIN DIR.:
  if os.path.isdir(IDI):
    isSWIN = True
    try:
      printMsg('Opening "%s"' % (DiFXcalc))
      antcodes = [];
      calc = open(DiFXcalc)
      lines = calc.readlines()
      calc.close()
      for ii, line in enumerate(lines):
        if 'TELESCOPE' in line and 'NAME' in line:
          antcodes.append(line.split()[-1])
    except:
      raise Exception('ERROR! NO ANTENNAS FOUND IN CALC FILE!')

    ifile = open(DiFXinput)
    inputlines = ifile.readlines()
    ifile.close()
    FreqL = [inputlines.index(l) for l in inputlines if 'FREQ TABLE' in l]
# ONLY ONE FREQ TABLE IS ALLOWED:
    try:
      fr = FreqL[0]
      Nfreq = int(list(filter(
        lambda x: 'FREQ ENTRIES' in x, inputlines[fr+1:]))[0].split()[-1])
      Nr = list(range(Nfreq))
    except Exception as ex:
      printMsg(str(ex))
      printError("BAD input file!")

    FrInfo = {'FREQ (MHZ)':[0. for i in Nr], 'BW (MHZ)':[0. for i in Nr], 
              'SIDEBAND':['U' for i in Nr], 'NUM CHANNELS':[1 for i in Nr],
              'CHANS TO AVG':[1 for i in Nr], 'OVERSAMPLE FAC.':[1 for i in Nr], 
              'DECIMATION FAC.':[1 for i in Nr], 'SIGN' :[1. for i in Nr]}

# READ METADATA FOR ALL FREQUENCIES:
    for entry in FrInfo.keys():
     for line in inputlines[fr+1:]:
       if entry in line:
         index = int((line.split(':')[0]).split()[-1])
         FrInfo[entry][index] = type(FrInfo[entry][0])(line.split(':')[-1].strip())

# SORT OUT THE CHANNEL FREQUENCIES:

    if len(doIF)==0:
      doIF = list(range(1,len(Nr)+1))

    metadata = []
    IFchan = 0
    for nu in Nr:
      nu0 = FrInfo['FREQ (MHZ)'][nu] 
      bw = FrInfo['BW (MHZ)'][nu]
      nchan = FrInfo['NUM CHANNELS'][nu]
# MAX. NUMBER OF CHANNELS:
      chav = FrInfo['CHANS TO AVG'][nu]
      if nu in doIF:
        IFchan = max([IFchan,int(nchan/chav)])
      sb = {True: 1.0 , False: -1.0}[FrInfo['SIDEBAND'][nu] == 'U']
      FrInfo['SIGN'][nu] = float(sb)
      if float(nchan//chav) != nchan/chav:
        printMsg("linspace check chan: %d / %d = %f" %
            (nchan, chav, nchan/chav))
      freqs = (nu0 + np.linspace((sb-1.)/2.,(sb+1.)/2.,
        nchan//chav,    # should be exactly divisible
        endpoint=False)*bw)*1.e6
      metadata.append(freqs)





  elif os.path.isfile(IDI):
    isSWIN = False
    try:
      import pyfits as pf
      ffile = pf.open(IDI)
      antcodes = [ff[:2] for ff in ffile['ANTENNA'].data['ANNAME']]
    except:
      raise Exception('ERROR! NO ANTENNAS FOUND IN FITS FILE!')


# READ FREQUENCY INFO TO HELP SELECTING THE SPW AUTOMATICALLY:
    fitsf = pf.open(IDI)
    nu0 = fitsf['FREQUENCY'].header['REF_FREQ']
    bw = fitsf['FREQUENCY'].header['CHAN_BW']
    nch = fitsf['FREQUENCY'].header['NO_CHAN'] #*fitsf['FREQUENCY'].header['NO_BAND']
    IFchan = nch
    Nr = fitsf['FREQUENCY'].header['NO_BAND']
    sgn = {True:1.0,False:-1.0}[bw>0.0]
    FrInfo = {'FREQ (MHZ)':[], 'BW (MHZ)':[], 'SIGN':[], 'NUM CHANNELS':[]}
    if sgn:
      FrInfo['SIDEBAND'] = ['U' for i in range(Nr)]
    else:
      FrInfo['SIDEBAND'] = ['L' for i in range(Nr)]

    metadata = []
    for i in range(Nr):
      FrInfo['FREQ (MHZ)'] += [(nu0 + i*bw*nch)/1.e6]
      FrInfo['BW (MHZ)'] += [bw*nch/1.e6]
      FrInfo['SIGN'] += [sgn]
      FrInfo['NUM CHANNELS'] += [int(nch)]
      freqs = nu0 + np.linspace((sgn-1.)/2.,(sgn+1.)/2.,nch,endpoint=False)*bw
      metadata.append(freqs)

    FrInfo['CHANS TO AVG'] = [1 for i in range(Nr)]
    FrInfo['OVERSAMPLE FAC.'] = [1 for i in range(Nr)] 
    FrInfo['DECIMATION FAC.']=[1 for i in range(Nr)]


    if len(doIF)==0:
     doIF = list(range(1,1+fitsf['FREQUENCY'].header['NO_BAND']))

    fitsf.close()


######
  OrigLinIdx = []
  for i,idd in enumerate(linAntIdx):
    if type(idd) is int and idd<=len(antcodes):
      OrigLinIdx.append(i)
    elif idd in antcodes:
      OrigLinIdx.append(i)
    else:
      raise Exception("Antenna %s NOT found in data!"%(str(idd)))







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

    calfreqs = []
    calfreqs2 = []
    for nis in range(len(tb.getcol('NAME'))):
      calfreqs.append(tb.getcell('CHAN_FREQ',nis)[0]/1.e6)
      calfreqs2.append(tb.getcell('CHAN_FREQ',nis)[-1]/1.e6)


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
        print(nu, ':', nurange[spwi][0], '<', nus[0], nus[1], '<', nurange[spwi][1], end=' ')
        if (nurange[spwi][0] - slop) < nus[0] and nus[1] < (nurange[spwi][1] + slop):
          spwsel[nui] = spwi
          print(' pass')
        else:
          print(' fail')
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
         printMsg('\n\n  NEW LIST OF IFs: '+','.join(list(map(str,doIF))))

    spwsel = list(set(spwsel[spwsel>=0]))
    if len(spwsel)>1:
       printError("There is more than one possible spw for some IFs!")

    spw = spwsel[0]
    printMsg('Selected spw: %d\n' % spw)
########################


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
  for i in OrigLinIdx:
   isLinear.append(np.zeros(len(gains[i]),dtype=np.bool))
   gaindata.append([])
   kind.append([])
   dtdata.append([])
   if dterms[i]=="NONE":
     nchan = 1
     ntime = 1
     dtdata[-1].append(np.ones(nchan).astype(np.float64))
     for ant in range(NSUM[i]):
       dtdata[-1].append([])
       dtdata[-1][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((nchan,ntime)).astype(np.bool))
   else:
    success = tb.open(os.path.join(dterms[i],'SPECTRAL_WINDOW'))
    if not success:
      printError("ERROR READING TABLE %s"%dterms[i])

    
    dtfreqs = tb.getcell('CHAN_FREQ',int(spw))   #[:,int(spw)]
    nchan = len(dtfreqs)
    tb.close()
    success = tb.open(os.path.join(dterms[i],'ANTENNA'))
    tabants = tb.getcol('NAME')
    tb.close()
    print('Reading ',dterms[i])
    tb.open(dterms[i])
    spmask = tb.getcol('SPECTRAL_WINDOW_ID')==int(spw)
    data = []
    flagrow = []
    for di in np.where(spmask)[0]: 
      data.append(tb.getcell('CPARAM',di))  #[:,:,spmask]
      flagrow.append(tb.getcell('FLAG',di))  #[:,:,spmask]

    data = np.array(data).transpose(1,2,0)
    flagrow = np.array(flagrow).transpose(1,2,0)
    antrow = []
    for ai in tb.getcol('ANTENNA1')[spmask]:
      if tabants[ai] in allants: 
        antrow.append(allants.index(tabants[ai]))
      else:
        antrow.append(-1)
    antrow = np.array(antrow)


    trow = tb.getcol('TIME')[spmask]
    flagsf = np.logical_or(flagrow[0,:,:],flagrow[1,:,:])
    flagsd = np.logical_or(np.abs(data[0,:,:])==0.0,np.abs(data[1,:,:])==0.0)
    flags = np.logical_or(flagsf,flagsd)
    tb.close()
    dtdata[-1].append(np.zeros(nchan).astype(np.float64))
    dtdata[-1][0][:] = dtfreqs
   
    for ant in range(NSUM[i]):
      dtdata[-1].append([])
      dd0 = data[0,:,:]
      dd1 = data[1,:,:]
      dims = np.shape(dd0[:,antrow==ant])
      if dims[1]>0:
       dtdata[-1][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[-1][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[-1][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[-1][-1].append(np.zeros(dims).astype(np.float64))
       dtdata[-1][-1].append(np.zeros(dims).astype(np.bool))
       dtdata[-1][-1][0][:] = (dd0[:,antrow==ant]).real
       dtdata[-1][-1][1][:] = (dd0[:,antrow==ant]).imag
       dtdata[-1][-1][2][:] = (dd1[:,antrow==ant]).real
       dtdata[-1][-1][3][:] = (dd1[:,antrow==ant]).imag
       dtdata[-1][-1][4][:] = flags[:,antrow==ant]
      else:
       dtdata[-1][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((dims[0],1)).astype(np.float64))
       dtdata[-1][-1].append(np.zeros((dims[0],1)).astype(np.bool)) 
  
   for j,gain in enumerate(gains[i]):
     gaindata[-1].append([])
     print('Reading ',gain)
     isLinear[-1][j] = interpolation[i][j]=='linear'
     if gain=="NONE":
      nchan = 1
      ntime = 1
      kind[-1].append(0)
      gaindata[-1][j].append(np.ones(nchan).astype(np.float64))
      for ant in range(NSUM[i]):
       gaindata[-1][j].append([])
       gaindata[-1][j][-1].append(np.ones(ntime).astype(np.float64))
       gaindata[-1][j][-1].append(np.ones((nchan,ntime)).astype(np.float64))
       gaindata[-1][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       gaindata[-1][j][-1].append(np.ones((nchan,ntime)).astype(np.float64))
       gaindata[-1][j][-1].append(np.zeros((nchan,ntime)).astype(np.float64))
       gaindata[-1][j][-1].append(np.zeros((nchan,ntime)).astype(np.bool))
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

      gfreqs = tb.getcell('CHAN_FREQ',int(spw))  #[:,int(spw)]
      nchan = len(gfreqs)
      tb.close()
      tb.open(os.path.join(gain,'ANTENNA'))
      tabants = tb.getcol('NAME')
      tb.close()
      tb.open(gain)
      gainType = tb.getkeyword('VisCal')
      spmask = tb.getcol('SPECTRAL_WINDOW_ID')==int(spw)
      trowns = tb.getcol('TIME')[spmask]
      tsort = np.argsort(trowns)
      trow = trowns[tsort]
      data = []
      flagrow = []

      if 'CPARAM' in tb.colnames():
        kind[-1].append(0)
        for di in np.where(spmask)[0]:
          data.append(tb.getcell('CPARAM',di))
          flagrow.append(tb.getcell('FLAG',di))
      else:
        if tb.info()['subType'] == 'B TSYS':
          kind[-1].append(2)
        else:
          kind[-1].append(1)

        for di in np.where(spmask)[0]:
          data.append(tb.getcell('FPARAM',di)) #[:,:,spmask])
          flagrow.append(tb.getcell('FLAG',di))

      data = (np.array(data)).transpose(1,2,0)[:,:,tsort]
      flagrow = (np.array(flagrow)).transpose(1,2,0)[:,:,tsort]

#############

      antrow = []
      for ai in tb.getcol('ANTENNA1')[spmask]:
        if tabants[ai] in allants: 
          antrow.append(allants.index(tabants[ai]))
        else:
          antrow.append(-1)
      antrow = np.array(antrow)[tsort]


      if np.shape(data)[0] == 2:  # A DUAL-POL GAIN (i.e., mode 'G')
        flagsf = np.logical_or(flagrow[0,:],flagrow[1,:])
        flagsd = np.logical_or(np.abs(data[0,:])==0.0,np.abs(data[1,:])==0.0)
      else: # A GAIN IN MODE 'T'
        flagsf = np.copy(flagrow[0,:])
        flagsd = np.abs(data[0,:])==0.0
      flags = np.logical_or(flagsf,flagsd)
      tb.close()
      gaindata[-1][j].append(np.zeros(nchan).astype(np.float64))
      gaindata[-1][j][0][:] = gfreqs
      for ant in range(NSUM[i]):
        gaindata[-1][j].append([])
        if np.shape(data)[0] == 2:  # A DUAL-POL GAIN (i.e., mode 'G')
          if gainmode[i][j] in ['G','S']:
            dd0 = data[0,:,:]
            dd1 = data[1,:,:]
          else:  # DUAL-POL GAIN FORCED TO 'T' MODE OR NEW XY-PHASE:
            if gainType == 'Xfparang Jones':
               dd0 = data[0,:,:]
               dd1 = data[0,:,:]; dd1[:] = 1.0
            else:
               Aux = np.sqrt(data[0,:,:]*data[1,:,:])
               dd0 = Aux
               dd1 = Aux
        else:  # A GAIN ALREADY IN MODE 'T' OR NEW XY-PHASE:
         #  print("ONLY ONE POL", gainType)
         dd0 = np.copy(data[0,:,:])
         dd1 = np.copy(data[0,:,:])
         if gainType == 'Xfparang Jones':
            dd1[:] = 1.0
        antrowant = antrow==ant
        dims = np.shape(dd0[:,antrowant])
        isFlagged=False
        # All antennas MUST have the re-ref XY0 phase, even if not used
        # in the pol. calibration!  Traditionally .XY0 appears in the file
        # name, but there may be other gainTypes defined now or in the future.
        if (gainType == 'Xfparang Jones' or
            gainType == 'GlinXphf Jones' or
            ".XY0" in gain):
          if dims[1]==0:
            antrowant = antrow==refants[0]
            dims = np.shape(dd0[:,antrowant])
        else:
          if dims[1]==0:
            dims = (dims[0],1)
            isFlagged=True
            antrowant = antrow==refants[0]
        gaindata[-1][j][-1].append(np.zeros(np.shape(trow[antrowant])).astype(np.float64))
        gaindata[-1][j][-1].append(np.ones(dims).astype(np.float64))
        gaindata[-1][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[-1][j][-1].append(np.ones(dims).astype(np.float64))
        gaindata[-1][j][-1].append(np.zeros(dims).astype(np.float64))
        gaindata[-1][j][-1].append(np.zeros(dims).astype(np.bool))
        if not isFlagged:
          gaindata[-1][j][-1][0][:] = trow[antrowant]

          if j==0:
            gaindata[-1][j][-1][1][:] = np.abs(dd0[:,antrowant])
          else: # CHANGE TO = 1.0 FOR TESTING:
            gaindata[-1][j][-1][1][:] = np.abs(dd0[:,antrowant])
          gaindata[-1][j][-1][2][:] = np.angle(dd0[:,antrowant])
          unwrap(gaindata[-1][j][-1][2]) #, check=ant<3)
          gaindata[-1][j][-1][3][:] = np.abs(dd1[:,antrowant])
          gaindata[-1][j][-1][4][:] = np.angle(dd1[:,antrowant])
          unwrap(gaindata[-1][j][-1][4]) #, check=ant<3)
          gaindata[-1][j][-1][5][:] = flags[:,antrowant]


  doAmpNorm = amp_norm>0.0

  ALMAstuff = [ngain, NSUM, kind,
               gaindata, dtdata, allantidx,
               nphtimes, antimes, refants,
               asdmtimes, timerangesArr[int(spw)], isLinear]

  ARGS = {'IDI':IDI, 'OUTPUTIDI':OUTPUTIDI, 'DiFXinput':DiFXinput, 'DiFXcalc':DiFXcalc, 
          'doIF':doIF, 'linAntIdx':linAntIdx, 'Range':Range, 'XYadd':XYadd, 'XYdel':XYdel, 
          'XYratio':XYratio, 'usePcal':usePcal, 'swapXY':swapXY, 'swapRL':swapRL, 
          'feedRotation':feedRotation, 'correctParangle':correctParangle, 
          'IDI_conjugated':IDI_conjugated, 'plotIF':plotIF, 'plotRange':plotRange, 
          'plotAnt':plotAnt, 'excludeAnts':excludeAnts, 'excludeBaselines':excludeBaselines, 
          'doSolve':doSolve, 'solint':solint, 'doTest':doTest, 'npix':npix, 
          'solveAmp':solveAmp, 'solveMethod':solveMethod, 'calstokes':calstokes, 
          'calfield':calfield, 'ALMAstuff':ALMAstuff,'saveArgs':saveArgs,'amp_norm':amp_norm}

  if saveArgs:
    OFF = open('PolConvert_CASA.last','wb')
    pk.dump(ARGS,OFF); OFF.close()


  PCONV.polconvert(**ARGS)

