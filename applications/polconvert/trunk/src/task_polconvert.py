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

try:
 import _PolConvert as PC
 print('\nC++ shared library loaded successfully\n')
except Exception as ex:
 print('\nNo, the shared library did not load successfully\n')
 raise ex

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

# Note that runPolConvert invokes PolConvert with standard values
# to some of the above arguments.

### polconvert_standalone.py has extra arguments...
  UVTaper=1.e9

  NEWPCSO = False   # 2.7.1 and earlier...
  NEWPCSO = True    # 2.7.2 and earlier...

# polconvert_CASA.py has two additional arguments (as of 2.0.6).
# TOP/polconvert.xml would need to be updated to use these.
###

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
  print('Entered task_polconvert::polconvert()')
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
      print(msg, file=lfile)
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

#### this block INPC
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

#### this block INPC
  greet  = '''
##########################################################################
# POLCONVERT -- version.                                                 #
#       Please, add the POLCONVERT reference to your publications:       #
#                                                                        #
#          Marti-Vidal, Roy, Conway & Zensus 2016, A&A, 587, 143         #
#                                                                        #
##########################################################################
'''
  greetings = re.sub('version', __version__ + '  ', greet)
  printMsg(greetings,dolog=False)
  printMsg('\n\nPOLCONVERT - VERSION %s'%__version__, doterm=False)




#########################################
# DO SOME SANITY CHECKS OF PARAMETERS
  
  try:
    doSolve = float(doSolve)
  except:
    printError("ERROR! doSolve should be a float!")

#### these blocks INPC
  allMethods = ['gradient','Levenberg-Marquardt','COBYLA','Nelder-Mead']
  scipyMethods = ['COBYLA','Nelder-Mead']
  if solveMethod not in allMethods:
    printError("ERROR! \'solveMethod\' must be any of: %s"%(', '.join(allMethods)))


  if type(calstokes) is not list:
    printError("ERROR! Wrong calstokes!")
  elif len(calstokes)!=4 and doSolve >= 0 :
    printError("ERROR! calstokes should have 4 elements, not %d (%s)!" % (
        len(calstokes), str(calstokes)))
  for item in calstokes:
    if type(item) is not float:
      printError("ERROR! calstokes should have float elements; got %s!" %
        str(type(item)))

  Stokes = list(calstokes)

  if doSolve >=0 and (Stokes[0]<=0. or
      Stokes[0]<np.sqrt(Stokes[1]**2.+Stokes[2]**2.+Stokes[3]**2.)):
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

  if type(doIF) is not list:
    printError("ERROR! doIF should be a list of integers!")
  else:
    for elem in doIF:
      if type(elem) is not int:
        printError("ERROR! doIF should be a list of integers!")
      if elem==0:
        printError("ERROR! IF numbers are given in AIPS (and FITS-IDI) convention\n(i.e., starting from 1; not from 0).\n")  

  if type(doTest) is not bool:
    printError("ERROR! doTest should be a boolean!")

  if doTest:
    printMsg("Will only compute, but not update the output file(s)",doterm=False)

  if type(linAntIdx) is not list:
    printError("ERROR! linAntIdx should be a list of antenna indices or names!")
  for elem in linAntIdx:
    if type(elem) not in [int, str]:
      printError("ERROR! linAntIdx should be a list of antenna indices or names!")


  if type(solint) is not list or (len(solint) not in [2,3]):
    printError("ERROR! solint (%s) must be a list of two/three numbers!" %
        str(solint))
  else:
    try:
      solint[0] = int(solint[0])
      solint[1] = int(solint[1])
    except:
      printError("ERROR! solint (%s) must be a list of two/three numbers!" %
        str(solint))

  if len(solint)==3:
    solint[2] = float(solint[2])
  else: # Default dt
    solint.append(100.)
#### above INPC

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

#### this block INPC
  if type(OUTPUTIDI) is not str:
    printError("ERROR! OUTPUTIDI should be a string!")
  if type(plotIF) is int:
    if plotIF >0:
      plotIF = [plotIF]
    else:
      plotIF = []
  for pli in plotIF:
    if type(pli) is not int:
      printError("ERROR! plotIF should be an integer or a list of integers!")

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



#### these blocks INPC
  if type(usePcal) is not list:
    printError("Invalid format for usePcal! Should be a list of booleans!\n")
  elif len(usePcal)==0:
    usePcal = [False for i in range(nALMA)]
  else:
    for pi in usePcal:
      if type(pi) is not bool:
        printError("Invalid format for usePcal! " +
            "It should be a list of booleans!\n")
  if len(np.where(usePcal)[0]) > 0:
    isPcalUsed = True
    printMsg("Info: Pcal used in %s" % str(np.where(usePcal)[0])) 
  else:
    isPcalUsed = False
    printMsg("Info: Pcal is not in use")

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
#### INPC


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



#### some of the following is INPC
#######
# CHECK IF THIS IS A FITS-IDI FILE OR A SWIN DIR.:
  if os.path.isdir(IDI):
    isSWIN = True
    printMsg('\n\nYou have asked to convert a set of SWIN files.')
    if len(DiFXinput)==0 or not os.path.exists(DiFXinput) or not os.path.isfile(DiFXinput):
      printError("Invalid DiFX input file! %s"%DiFXinput)
    printMsg('Opening calc file... %s' % DiFXcalc)
    try:
      printMsg('Opening "%s"' % (DiFXcalc))
      antcoords = []  ; soucoords = [[],[]]; antmounts = [];
      antcodes = [];
      calc = open(DiFXcalc)
      lines = calc.readlines()
      calc.close()
      printMsg('Read %d lines from %s' % (len(lines), DiFXcalc))
      for ii, line in enumerate(lines):
        if 'TELESCOPE' in line and 'NAME' in line:
          antcodes.append(line.split()[-1])
        if 'TELESCOPE' in line and 'X (m):' in line:
    #      printMsg(line.rstrip())
          antcoords.append(list(map(float,[ll.split()[-1] for ll in lines[ii:ii+3]])))
          printMsg('TELESCOPE %s AT X: %.3f ; Y: %.3f ; Z: %.3f'%tuple([antcodes[-1]] + antcoords[-1]))
# CURRENTLY, ONLY ALTAZ MOUNTS SUPPORTED FOR SWIN FILES:
          antmounts.append(0)

        if 'SOURCE' in line and ' NAME: ' in line:
          SNAM = line.split()[-1]
          soucoords[0].append(float(lines[ii+1].split()[-1]))
          soucoords[1].append(float(lines[ii+2].split()[-1]))
          printMsg('SOURCE %s AT RA: %.8f rad, DEC: %.8f rad'%(SNAM,soucoords[0][-1],soucoords[1][-1]))
      antcoords = np.array(antcoords,dtype=np.float)
      antmounts = np.array(antmounts)
      soucoords[0] = np.array(soucoords[0],dtype=np.float)
      soucoords[1] = np.array(soucoords[1],dtype=np.float)
      printMsg('done parsing calc')
    except Exception as ex:
      printMsg(str(ex))
      printMsg(("WARNING! Invalid DiFX calc file '%s'!\n" + 
        "PolConvert may not calibrate properly.") % DiFXcalc)
    if len(antmounts)==0:
      printError('ERROR! NO ANTENNAS FOUND IN CALC FILE!')
    else:
      printMsg('There are %i antennas.'%len(antmounts))
  elif os.path.isfile(IDI):
    isSWIN = False
    printMsg('\n\nYou have asked to convert a FITS-IDI file.')
    print('Reading array geometry...')
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

##z
      antcodes = [ff[:2] for ff in ffile['ANTENNA'].data['ANNAME']]
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
##z #   antcodes = ['%02i'%i for i in range(1,len(antmounts)+1)]
    except:
      printMsg('WARNING! This FITS-IDI file has missing information!\nPolConvert may not calibrate properly.')
  else:
    printError("Invalid input data!") 

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
 
  else: # is(not)Swin
    import pyfits as pf

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

# ANTENNAS TO PARTICIPATE IN THE GAIN ESTIMATES:
  nTotAnt = len(antcoords)
  calAnts = []
  for exA in antcodes:
    if exA not in excludeAnts:
      calAnts.append(antcodes.index(exA)+1)
      ### = [i+1 for i in range(len(antcoords)) if i+1 not in excludeAnts]
    else:
      printMsg("Excluding antenna %s"%str(exA))
  try:
    plotAnt = int(plotAnt)
  except:
    if plotAnt not in antcodes:
      printError("Reference antenna %s is not found in metadata among %s! " % (str(plotAnt),antcodes))
    else:
      plotAnt = antcodes.index(plotAnt)+1
  for i in range(len(linAntIdx)):
    try:
      linAntIdx[i] = int(linAntIdx[i])
    except:
      if linAntIdx[i] not in antcodes:
        linAntIdx[i] = antcodes.index(linAntIdx[i])+1
  if plotAnt in linAntIdx:
    printMsg(
        "WARNING: Plotting will involve autocorrelations. \nThis has not been fully tested!")

  FlagBas1 = []
  FlagBas2 = []
  for fbi in excludeBaselines:
    printMsg("Excluding baseline %s"%str(fbi))
    if fbi[0] in antcodes and fbi[1] in antcodes:
      FlagBas1.append(antcodes.index(fbi[0])+1) ### = np.array([int(i[0]+1) for i in excludeBaselines])
      FlagBas2.append(antcodes.index(fbi[1])+1) ### = np.array([int(i[1]+1) for i in excludeBaselines])
    else:
      printError('Unknown antenna(s) %s and/or %s in excludeBaselines!\n'%(fbi[0],fbi[1]))
  FlagBas1 = np.array(FlagBas1); FlagBas2 = np.array(FlagBas2)

  if plotAnt not in calAnts:
    if (doSolve>=0):
      printError("ERROR! plotAnt/Reference antenna is NOT in list of calibratable antennas!")
    else:
      printMsg("plotAnt (%d) is not in antenna list, so plots will be missing" % plotAnt)

  if type(feedRotation) is not list:
    printError("feedRotation must be a list of numbers")
  elif len(feedRotation)==0:
    feedRot = np.zeros(nTotAnt) 
  elif len(feedRotation)!= nTotAnt:
    printError("feedRotation must have %i entries!"%nTotAnt)
  else:
    feedRot = np.pi/180.*np.array(feedRotation, dtype=np.float)  







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


#### these blocks INPC
# Get the REAL number (and names) of linear-pol antennas in this dataset:
  nALMATrue = 0
  linAntIdxTrue = []
  linAntNamTrue = []
  OrigLinIdx = []
  for i,idd in enumerate(linAntIdx):
    if type(idd) is int and idd<=len(antcodes):
      nALMATrue += 1
      linAntIdxTrue.append(idd)
      linAntNamTrue.append(antcodes[idd-1])
      OrigLinIdx.append(i)
    elif idd in antcodes:
      nALMATrue += 1
      linAntNamTrue.append(idd)
      linAntIdxTrue.append(antcodes.index(idd)+1)
      OrigLinIdx.append(i)
      
  printMsg("There are %i linear-polarization antennas in THIS dataset"%nALMATrue)

# COMPUTE XY delays:

  XYdelF = [[0.0,0.0] for i in range(nALMATrue)]
  if type(XYdel) is not dict: #or len(XYdel) != nALMA:
    printError("Invalid format for XYdel!\n") # Should be a LIST of numbers, as large as the number of linear-polarization VLBI stations!")

  for i,doant in enumerate(linAntNamTrue):
    if doant in XYdel.keys():
      if type(XYdel[doant]) is list:
        try:
          XYdelF[i] = list(map(float,XYdel[doant])) #float(XYdel[i]*np.pi/180.)
        except:
          printError("Invalid format for XYdel!\n Should be a dictionary with LISTS of numbers!")
      else:
        try:
          XYdelF[i] = [float(XYdel[doant]),0.0] #float(XYdel[i]*np.pi/180.)
        except:
          printError("Invalid format for XYdel!\n Should be a dictionary with LISTS of numbers!")

  ### in general, now have an iteration: for difxdfile in OUTPUT

  XYaddF = [[] for i in range(nALMATrue)]

  for i in range(nALMATrue):
    for j in doIF: # range(len(FrInfo['SIGN'])):
      sgn = FrInfo['SIGN'][j-1]
      if (float(FrInfo['NUM CHANNELS'][j-1]//FrInfo['CHANS TO AVG'][j-1]) !=
          FrInfo['NUM CHANNELS'][j-1]/FrInfo['CHANS TO AVG'][j-1]):
            printMsg("linspace check freq: %d / %d = %f" % (
              FrInfo['NUM CHANNELS'][j-1],FrInfo['CHANS TO AVG'][j-1],
              FrInfo['NUM CHANNELS'][j-1]/FrInfo['CHANS TO AVG'][j-1]))
      if isSWIN:
        NuChan = np.linspace((sgn-1.)/2.,(sgn+1.)/2.,
            FrInfo['NUM CHANNELS'][j-1]//FrInfo['CHANS TO AVG'][j-1],
            endpoint=False)
      else:
        NuChan = np.linspace(0.,sgn,
            FrInfo['NUM CHANNELS'][j-1]//FrInfo['CHANS TO AVG'][j-1],
            endpoint=False)

      Nus = 1.e6*np.array(
        FrInfo['FREQ (MHZ)'][j-1] + FrInfo['BW (MHZ)'][j-1]*NuChan,
            dtype=np.float)
      XYaddF[i].append(2.*np.pi*(Nus-XYdelF[i][1])*XYdelF[i][0])

# Prepare memory of XY amplitude ratios:

  if type(XYratio) is not dict: # or len(XYratio) != nALMA:
    printError("Invalid format for XYratio!") #\n Should be a list as large as the number of linear-polarization VLBI stations!\nThe elements of that list shall be either numbers or lists as large as the number of IFs")

  XYratioF = [[] for i in range(nALMATrue)]
  for i in range(nALMATrue):
    for j in doIF:
      if (float(FrInfo['NUM CHANNELS'][j-1]//FrInfo['CHANS TO AVG'][j-1]) !=
          FrInfo['NUM CHANNELS'][j-1]/FrInfo['CHANS TO AVG'][j-1]):
            printMsg("linspace check freq: %d / %d = %f" % (
              FrInfo['NUM CHANNELS'][j-1],FrInfo['CHANS TO AVG'][j-1],
              FrInfo['NUM CHANNELS'][j-1]/FrInfo['CHANS TO AVG'][j-1]))
      XYratioF[i].append(np.ones(
        FrInfo['NUM CHANNELS'][j-1]//FrInfo['CHANS TO AVG'][j-1],
        dtype=np.float))

# COMPUTE TIME RANGES:
# [ days, hours, minutes, seconds, days, hours, minutes, seconds ]
# default is to plot nothing, convert from now until centuries from now.

  if len(plotRange)==0:
    plRan = np.array([0.,0.])
    plotFringe = False
  else:
   try:
     plRan = np.array([plotRange[0]+plotRange[1]/24.+plotRange[2]/1440.+plotRange[3]/86400.,plotRange[4]+plotRange[5]/24.+plotRange[6]/1440.+plotRange[7]/86400.])
     plotFringe = True
     if len(plotIF)==0: plotIF = list(doIF)
   except:
     printError("Bad time range format for plotRange!")

  if len(Range) == 0:
    Ran = np.array([0.,1.e20])
  else:
   try:
     Ran = np.array([Range[0]+Range[1]/24.+Range[2]/1440.+Range[3]/86400.,Range[4]+Range[5]/24.+Range[6]/1440.+Range[7]/86400.])
   except:
     printError("Bad time range format for Range!")

  printMsg('plotRange ' + str(plotRange) + ' -> ' + str(plRan) + ' plRan');
  printMsg('  doRange ' +     str(Range) + ' -> ' + str(Ran)   + '   Ran');

#######
# WARNING! UNCOMMENT THIS IF NOT DEBUGGING!
  if os.path.exists(OUTPUTIDI) and IDI != OUTPUTIDI:
    printMsg('Will REMOVE the existing OUTPUT file (or directory)!\n')
    printMsg('Copying IDI to OUTPUTIDI!\n')
    os.system('rm -rf %s'%OUTPUTIDI)
    os.system('cp -rf %s %s'%(IDI,OUTPUTIDI))
  elif not os.path.exists(OUTPUTIDI):
    printMsg('Copying IDI to OUTPUTIDI!\n')
    os.system('cp -rf %s %s'%(IDI,OUTPUTIDI))
#######

  if isSWIN:
    OUTPUT = []
    PHASECALS = []
    walk = [f for f in os.walk(OUTPUTIDI)]
    for subd in walk:
      OUTPUT += [os.path.join(subd[0],fi) for fi in filter(lambda x: x.startswith('DIFX_'),subd[2])]
      PHASECALS += [os.path.join(subd[0],fi) for fi in filter(lambda x: x.startswith('PCAL_'),subd[2])]

    if len(OUTPUT) == 0:
      printError("No *.difx files found in directory!")

# Derive the days of observation of each difx file:
    mjd = np.zeros(len(OUTPUT))
    mjs = np.zeros(len(OUTPUT))
    for i,fi in enumerate(OUTPUT):
      mjd[i],mjs[i] = list(map(float,((os.path.basename(fi)).split('.')[0]).split('_')[1:3]))
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

# Set XYadd and XYratio:

  if type(XYadd) is not dict: # or len(XYadd.keys) != nALMA:
    printError("Invalid format for XYadd!\n") # Should be a list as large as the number of linear-polarization VLBI stations!\nThe elements of that list shall be either numbers or lists as large as the number of IFs")
  printMsg('keys of XYadd:' + str(XYadd.keys()))

  if isPcalUsed:
      import _XPCal as XP
      import scipy.interpolate as spint

  for i,doant in enumerate(linAntNamTrue):
        
#########################
#### CORRECTIONS BASED ON PHASECAL TONES:

    if usePcal[i]:
        printMsg("Using Pcal for %d" % i)
        PCFile = filter(lambda x: x.endswith(doant),PHASECALS)
        if len(PCFile)==0:
          printError("\n\n SANITY-TEST FAILURE! NO PHASECAL FILE FOR %i\n"%doant)
        tempArr = XP.XPCal(PCFile[0],0,0.0)

        # Update pcal files (if not doing a test):
        if not doTest:
          ErrCode = XP.XPConvert(PCFile[0])  
          if ErrCode != 0:
            printError("\n\n ERROR Converting phasecal file %s\n"%os.path.basename(PCFile[0]))

        if len(tempArr[0])==0:
          printError("\n\n ERROR! No phasecal information for antenna %i\n Will NOT convert!\n"%i)
        else:

          CPhase = spint.interp1d(tempArr[0],-tempArr[1],bounds_error=False,fill_value = 'extrapolate')
          CAmpl = spint.interp1d(tempArr[0],tempArr[4],bounds_error=False,fill_value = 1.0)

          for ji,j in enumerate(doIF):
            sgn = FrInfo['SIGN'][j-1]  
            if isSWIN:
              NuChan = np.linspace((sgn-1.)/2.,(sgn+1)/2.,FrInfo['NUM CHANNELS'][j-1]/FrInfo['CHANS TO AVG'][j-1],endpoint=False)
            else:
              NuChan = np.linspace(0.,sgn,FrInfo['NUM CHANNELS'][j-1]/FrInfo['CHANS TO AVG'][j-1],endpoint=False)
            Nus = np.array(FrInfo['FREQ (MHZ)'][j-1] + FrInfo['BW (MHZ)'][j-1]*NuChan,dtype=np.float)
            XYaddF[i][ji] += (CPhase(Nus))*np.pi/180.

            if doant in XYratio.keys() and XYratio[doant] == 0.0:
              XYratioF[i][ji] *= CAmpl(Nus) 
                      
          del CPhase
          Nelem = len(tempArr)
          for j in range(Nelem-1,-1,-1):
              del tempArr[j]
          del tempArr
##################################

    if doant not in XYadd.keys():
       printMsg('WARNING IN APPLYING XYadd! ANTENNA %s IS NOT FOUND IN DICT!'%doant)

    else:
        
      #printMsg("Processing XYadd for %d,%d" % (i,doant))
      printMsg("Processing XYadd for %d,%s" % (i,doant))
      if type(XYadd[doant]) is list:
        for j in range(len(XYadd[doant])):
         if type(XYadd[doant][j]) in [list,np.ndarray]:
           arrSize = np.shape(np.array(XYadd[doant][j]))
           if len(arrSize)!=1 or arrSize[0] != IFchan:
             printError("Shape of XYadd array(s) does not coincide with number of IF channels\n")
           else:
             XYaddF[i][j] += np.array(XYadd[doant][j])*np.pi/180.
         else:
           try:
             XYaddF[i][j] += np.ones(IFchan)*float(XYadd[doant][j])*np.pi/180.
           except:
             printError("Invalid format for XYadd!\nShould be a LIST of numbers (or a list of lists of numbers),\nor a list of lists of arrays")
      else:
        try:
          for j in range(len(doIF)):           
            XYaddF[i][j] += np.ones(len(XYaddF[i][j]))*float(XYadd[doant])*np.pi/180.
        except:
          printError("Invalid format for XYadd!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!")

  UseAutoCorrs = np.zeros(nALMATrue,dtype=np.int32)
  
  printMsg('keys of XYratio: ' + str(XYratio.keys()))
  for i,doant in enumerate(linAntNamTrue):

    try:  
  
      if doant not in XYratio.keys():
        printMsg('WARNING IN APPLYING XYratio! ANTENNA %s IS NOT FOUND IN DICT!'%doant)

      else:
          
        #printMsg("Processing XYratio for %d,%d" % (i,doant))
        printMsg("Processing XYratio for %d,%s" % (i,doant))
        if type(XYratio[doant]) not in [list,np.ndarray]:

          if float(XYratio[doant]) < 0.0:
            NchanAutos = max([1,int(float(IFchan)/float(-XYratio[doant]))])
            UseAutoCorrs[i] = NchanAutos
            printMsg("Will correct Antenna %i with auto-correlations, applying a median filter of %i channels.\n"%(linAntIdxTrue[i],NchanAutos))
          elif float(XYratio[doant]) > 0.0:
            for j in range(len(doIF)): 
              XYratioF[i][j] *= float(XYratio[doant])

        else:
          
          for j in range(len(XYratio[doant])):
            if type(XYratio[doant][j]) in [list,np.ndarray]:
              tempArr = np.array(XYratio[doant][j])
              arrSize = np.shape(tempArr)
              if len(arrSize)!=1 or arrSize[0] != IFchan:
                printError("Shape of XYratio array(s) does not coincide with number of IF channels\n")
              else:
                XYratioF[i][j] *= tempArr
            else:
              try:
                XYratioF[i][j] *= float(XYratio[doant][j])
              except:
                printError("Invalid format for XYratio!\nShould be a list (or a list of lists,\nor a list of lists of arrays))")

    except:

      printError("Invalid format for XYratio!\n Should be a LIST of numbers (or a list of lists),\n as large as the number of linear-polarization VLBI stations!")

### PriorGains now iterates on number of swin files (difxdfile)

# A-PRIORI GAINS:
  PrioriGainsZERO = []
  for i in range(len(linAntIdxTrue)):
    PrioriGainsZERO.append([])
    for j in range(len(XYaddF[i])):
       PrioriGainsZERO[i].append(np.array(XYratioF[i][j]*np.exp(1.j*XYaddF[i][j]),dtype=np.complex64))

  PrioriGains = [PrioriGainsZERO]
  #  print PrioriGains


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

# CALL POLCONVERT. THE LENGTH OF THE "metadata" LIST WILL TELL
# POLCONVERT WHETHER THIS IS A FITS-IDI OR A SWIN DATASET:

  if amp_norm>0.0:
    os.system('rm -rf POLCONVERT.GAINS')

  if len(plotIF)>0:
    os.system('rm -rf CONVERSION.MATRIX; mkdir CONVERSION.MATRIX')
    os.system('rm -rf FRINGE.PEAKS; mkdir FRINGE.PEAKS')
    os.system('rm -rf FRINGE.PLOTS; mkdir FRINGE.PLOTS')
  os.system('rm -rf POLCONVERT.FRINGE; mkdir POLCONVERT.FRINGE')

  printMsg("\n###\n### Going to PolConvert\n###")

  doAmpNorm = amp_norm>0.0

  if NEWPCSO:
    ALMAstuff = [ngain, NSUM, kind,
        gaindata, dtdata, allantidx,
        nphtimes, antimes, refants,
        asdmtimes, timerangesArr[int(spw)], isLinear]

    # defaults for other new variables
    IFoffset = 0
    AC_MedianWindow = 0
    correctParangle=False
    plotSuffix = ''
    logName = logName = "PolConvert%s.log"%plotSuffix
    np.set_printoptions(precision=4, threshold=6)
    # new calling sequence
    PC_Params = [nALMATrue, plotIF, plotAnt, doIF, IFoffset,
        AC_MedianWindow, swapXY, OUTPUT, linAntIdxTrue, plRan, Ran,
        doTest, doSolve, doConj, doAmpNorm, PrioriGains, metadata,
        soucoords, antcoords, antmounts, isLinear,calfield,
        UseAutoCorrs,bool(correctParangle),DEBUG, logName, ALMAstuff]
    if DEBUG:
      printMsg("NEW POLCONVERT.SO CALLED WITH: %s"%str(PC_Params))

# polconvert_CASA.py calls polconvert_standalone.py with this dict()
#
#   ARGS = {'IDI':IDI, 'OUTPUTIDI':OUTPUTIDI, 'DiFXinput':DiFXinput, 'DiFXcalc':DiFXcalc,
#         'doIF':doIF, 'linAntIdx':linAntIdx, 'Range':Range, 'XYadd':XYadd, 'XYdel':XYdel,
#         'XYratio':XYratio, 'usePcal':usePcal, 'swapXY':swapXY, 'swapRL':swapRL,
#         'feedRotation':feedRotation, 'correctParangle':correctParangle,
#         'IDI_conjugated':IDI_conjugated, 'plotIF':plotIF, 'plotRange':plotRange,
#         'plotAnt':plotAnt, 'excludeAnts':excludeAnts, 'excludeBaselines':excludeBaselines,
#         'doSolve':doSolve, 'solint':solint, 'doTest':doTest, 'npix':npix,
#         'solveAmp':solveAmp, 'solveMethod':solveMethod, 'calstokes':calstokes,
#         'calfield':calfield, 'ALMAstuff':ALMAstuff,'saveArgs':saveArgs,'amp_norm':amp_norm}
#
# and can save it
#   if False:  # saveArgs:
#     OFF = open('PolConvert_CASA.last','wb')
#     pk.dump(ARGS,OFF); OFF.close()

  else:
    # old calling sequence
    PC_Params = [nALMATrue, plotIF, plotAnt, len(allants), doIF,
        swapXY, ngain, NSUM, kind, len(gaindata), len(dtdata), OUTPUT,
        linAntIdxTrue, plRan, Ran, allantidx, len(nphtimes), len(antimes),
        len(refants), len(asdmtimes),  doTest, doSolve, doConj, doAmpNorm,
        np.shape(PrioriGains), len(metadata), soucoords, antcoords, antmounts,
        isLinear,calfield,timerangesArr[int(spw)],UseAutoCorrs,DEBUG]
    if DEBUG:
      printMsg("POLCONVERT CALLED WITH: %s"%str(PC_Params))
      
  # plotAnt is no longer used by PC.PolConvert(), but is required by doSolve
  # the second argument is "PC:PolConvert::plIF" and controls whether the
  # huge binary fringe files are written.

  try:
   if NEWPCSO:
    printMsg('NEWPCSO path True (new)')
    didit = PC.PolConvert(nALMATrue, plotIF, plotAnt, doIF, IFoffset,
        AC_MedianWindow, swapXY, OUTPUT, linAntIdxTrue, plRan, Ran,
        doTest, doSolve, doConj, doAmpNorm, PrioriGains, metadata,
        soucoords, antcoords, antmounts, isLinear,calfield,
        UseAutoCorrs,bool(correctParangle),DEBUG, logName, ALMAstuff)
   else:
    printMsg('NEWPCSO path False (old)')
    didit = PC.PolConvert(nALMATrue, plotIF, plotAnt, len(allants), doIF,
        swapXY, ngain, NSUM, kind, gaindata, dtdata, OUTPUT,
        linAntIdxTrue, plRan, Ran, allantidx, nphtimes, antimes,
        refants, asdmtimes,  doTest, doSolve, doConj, doAmpNorm,
        PrioriGains, metadata, soucoords, antcoords, antmounts,
        isLinear,calfield,timerangesArr[int(spw)],UseAutoCorrs,DEBUG)
  except Exception as ex:
    printMsg(str(ex))
    printMsg("Continuing despite the exception, just for the fun of it")
    didit = 0

  printMsg("\n###\n### Done with PolConvert (status %d).\n###" % (didit))

  if didit != 0:
    printError("\n\n ERROR IN POLCONVERT!\n\n")

# GENERATE ANTAB FILE(s):

  if doAmpNorm:
    printMsg('Generating ANTAB file(s).')
    DPFU = float(amp_norm)
    printMsg("DPFU(amp_norm) is %f\n" % DPFU)

    try:
      gfile = open("POLCONVERT.GAINS")
    except:
      printError("No gain file written!")

    entries = [l.split() for l in gfile.readlines()]; gfile.close()
    if entries == 0:
      printMsg("Gain file is empty!  ANTAB will be as well.")
    else:
      printMsg("Gain file had %d entries" % len(entries))

    IFs = np.zeros(len(entries),dtype=np.int)
    Data = np.zeros((len(entries),2))
    AntIdx = np.zeros(len(entries),dtype=np.int)

    for i,entry in enumerate(entries):
      IFs[i] = int(entry[0])
      AntIdx[i] = int(entry[1])
      Data[i,:] = list(map(float,entry[2:]))

    Times = np.unique(Data[:,0])
    Tsys = np.zeros((len(Times),len(doIF)+1))
    Tsys[:,0] = Times

    for j,jnam in enumerate(linAntNamTrue):
     for ii,i in enumerate(doIF):
      mask = np.logical_and(IFs==i,AntIdx==linAntIdxTrue[j])
      for datum in Data[mask]:
        itime = np.where(Times==datum[0])[0]
        Tsys[itime,ii+1] = datum[1]*DPFU

     outf = open("POLCONVERT_STATION_%s.ANTAB"%jnam,"w")
     print("GAIN AA  ELEV DPFU=%.3f   FREQ=10,100000"%DPFU, file=outf)
     print("POLY=1.0000E+00", file=outf)
     print("/", file=outf)
     print("TSYS AA  FT=1.0  TIMEOFF=0", file=outf)
     print("INDEX= "+', '.join(
        ['\'L%i|R%i\''%(i+1,i+1) for i in range(len(doIF))]), file=outf)
     print("/", file=outf)
     fmt0 = "%i %i:%2.4f  "
     # boost field width to retain significant figures
     fmt1 = "%10.4f  "*len(doIF)
     prevT = " "
     for entry in Tsys:
       MJD2000 = 51544
       Tfr,Tin = np.modf(entry[0])
       tobs = (dt.date(2000,1,1) +
        dt.timedelta(Tin-MJD2000)).timetuple().tm_yday
       minute,hour = np.modf(Tfr*24.)
       minute *= 60.
       currT = fmt0%(tobs,hour,minute)
       if currT != prevT:  # Limited time resolution in ANTAB
         prevT = currT
         print(currT + fmt1%tuple(entry[1:]), file=outf)
     print("/", file=outf)
     outf.close()
    printMsg('Finished with ANTAB file(s).')

  tac = time.time()

  printMsg('PolConvert took %.1f seconds.\n\n'%(tac-tic))


























# SOLVE FOR THE CROSS-POLARIZATION GAINS:
  if doSolve >= 0.0:

   CGains = {'XYadd':{},'XYratio':{},'aPrioriXYGain':PrioriGains}

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
     print('\nC++ PolGainSolve shared library loaded successfully\n')
   except Exception as ex:
     print('\nC++ PolGainSolve shared library did not loaded successfully\n')
     raise ex


############################################################
# Levenberg-Marquardt minimizer of the GCPFF problem:
   def LMMin(p0,Ch0,Ch1):

     MAXIT = maxIter*len(fitAnts)
     relchange = 1.0
     #Gchange = 1.0

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
       currChi2 = PS.GetChi2(ptst0,LMTune,Ch0,Ch1,0)
       Chi2_0 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1,0)
       sys.stdout.write('%.3g/1 '%Chi2_0) ; sys.stdout.flush()

       if i==0 or currChi2<minChi2:
         minChi2 = currChi2
         minGains[:] = currP


       i += 1

##z    while Chi2_0>currChi2: 
       while Chi2_0>=currChi2:
         i += 1     
         LMTune *= KFacRaise
         ptst0 = np.copy(currP)

         Chi2_ini = PS.GetChi2(ptst0,LMTune,Ch0,Ch1,0)
         Chi2_0 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1,0)
         sys.stdout.write('%.3g/2 '%Chi2_0) ; sys.stdout.flush()
         if i>=MAXIT:
           break

##z    relchange = (currChi2 - Chi2_0)/Chi2_0
       if Chi2_0 >0.0:
         relchange = (currChi2 - Chi2_0)/Chi2_0
       else:
         printError("\n\n  Problem in PolGainSolve.\n")         


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
       Chi2_ini = PS.GetChi2(ptst0,LMTune,Ch0,Ch1,0)
       Chi2_0 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1,0)
       sys.stdout.write('%.3g/3 '%Chi2_0) ; sys.stdout.flush()

       i += 1

       if Chi2_0 > currChi2:
         LMTune *= KFacRaise

       else:         

         while True:   # Chi2_0<currChi2: 
           i += 1
           LMTune /= KFacDecr
           ptst0 = np.copy(currP)
           Chi2_ini = PS.GetChi2(ptst0,LMTune,Ch0,Ch1,0)
           Chi2_1 = PS.GetChi2(ptst0,-1.0,Ch0,Ch1,0)
           sys.stdout.write('%.3g/4'%Chi2_1)

           if Chi2_1<minChi2:
             minChi2 = Chi2_1
             minGains = np.copy(ptst0)
             currP = np.copy(ptst0)
             currChi2 = Chi2_1
             sys.stdout.write('a')

           if Chi2_1<Chi2_0 and i<=MAXIT:
             Chi2_0 = Chi2_1
             ptst1[:] = ptst0
             currP = np.copy(ptst0)
             currChi2 = Chi2_1
             sys.stdout.write('b')
           else:
             break
           sys.stdout.write(' ') ; sys.stdout.flush()

         # this is not the correct expression if the 'b' path was visited
         relchange = (currChi2 - Chi2_0)/Chi2_0
         LMTune *= KFacDecr  # Come back to state of last successful decrease

         if i>=MAXIT or np.abs(relchange)<maxErr:
           break


     Chi2_final = PS.GetChi2(minGains,LMTune,Ch0,Ch1,1)

##z  FLIP = Chi2_final < 0.0  # Flip gains by 180 degrees.
     FLIP = False # Chi2_final < 0.0  # Flip gains by 180 degrees.

# GBC debugging:
     if FLIP: sys.stdout.write(' Flipped\n')
     else:    sys.stdout.write(' NotFlip\n')
     printMsg("    Final error: %.3e in ChSq"%(np.abs(relchange)))
     if i >= MAXIT:
      if np.abs(relchange)>maxErr:
        printMsg("    WARNING! Slow cross-pol gain convergence (%d)! | Chan(s): %i-%i !"%(i,Ch0,Ch1-1))
      else:
        printMsg("    Warning: too many iterations (%d) why is that?"%i)
     #printMsg("\n    Final error: %.3e in ChSq / %.3e in gains\n"%(np.abs(relchange),Gchange))

     return [minGains,FLIP]   
   # end of Levenberg-Marquardt minimizer of the GCPFF problem   LMMin







   if doSolve >= 0.0:

    selAnts = np.array(calAnts,dtype=np.int32)

    doSolveD = float(doSolve)

    if doSolveD>0.0:
      fitAnts = list(calAnts)
    else:
      fitAnts = list(linAntIdxTrue)


    cAnts = np.array(calAnts,dtype=np.int32)
    lAnts = np.array(linAntIdxTrue,dtype=np.int32)
#   if (len(FlagBas1) > 0 or len(FlagBas1) > 0):
#    printMsg("Using 5 argument method of PolGainSolve")
     #MySolve = PS.PolGainSolve(doSolveD,solint,selAnts,lAnts,FlagBas1,FlagBas2)
    printMsg('\n%%% initializing PolGainSolve\n')
    # using a null for a logfile should be harmless...  NOPE! --> "basic_string::_S_construct null not valid"
    MySolve = PS.PolGainSolve(doSolveD,UVTaper,solint,selAnts,lAnts,[FlagBas1,FlagBas2],'PolConvert.GainSolve.log')
    printMsg(PS.__doc__ + ('\nInitialization rv %d\n'%MySolve) + '%%%\n')
#   else:
#    printMsg("Using 4 argument method of PolGainSolve:")
#    printMsg("  selAnts is " + str(selAnts))
#    printMsg("    lAnts is " + str(lAnts))
#    MySolve = PS.PolGainSolve(doSolveD,solint,selAnts,lAnts)
#   printMsg('PolGainSolve finished with return: <%s>' % str(MySolve))

### POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i

    AllFreqs = []
    for pli in doIF:
      printMsg("Reading back IF #%i"%pli)
      file1 = "POLCONVERT.FRINGE/OTHERS.FRINGE_IF%i"%pli
      file2 = "POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i"%pli
      printMsg("  file1 is %s" % file1)
      printMsg("  file2 is %s" % file2)
      success = PS.ReadData(pli, file1, file2, 25.0)
      printMsg("  calling GetNScan(0) (success = %d)"% success)
      NScan = PS.GetNScan(0)
      if success != 0:
        printError('Failed PolGainSolve: ERROR %i'%success)
      AllFreqs.append(np.zeros(PS.GetNchan(pli), order="C", dtype=np.float))
      rc = PS.GetIFs(pli, AllFreqs[-1])
    MaxChan = max([np.shape(pp)[0] for pp in AllFreqs])
    printMsg("  length of AllFreqs is %d MaxChan %d"%(len(AllFreqs),MaxChan))
    printMsg("\nWill now estimate the residual cross-polarization gains.\n")



# ESTIMATE RATES FOR ALL STATIONS (plotAnt IS THE [plotting] REFERENCE):

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
       CGains['XYadd'][antcodes[ci-1]] = []
       CGains['XYratio'][antcodes[ci-1]] = []
     for plii,pli in enumerate(doIF):
       printMsg("  working %s,%s"%(str(plii),str(pli)))
       Nchans = np.shape(AllFreqs[plii])[0]
       temp = [np.zeros(Nchans,dtype=np.complex64) for ci in fitAnts]
       BPChan = list(range(0,Nchans,ChAv))
       if BPChan[-1]<Nchans-1:
         BPChan.append(Nchans-1)
       BPChan = np.array(BPChan,dtype=np.int32)
       Npar = len(fitAnts)*{True:2,False:1}[solveAmp]
       laux = [pli]
       rv = PS.SetFit(Npar,laux,fitAnts,solveAmp,solveQU,Stokes,useCov,feedRot)
       printMsg("  PS.SetFit rv %d" % rv)
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

   #      pArr = np.array(p0)
   #      print PS.GetChi2(pArr,-1.0,BPChan[chran],BPChan[chran+1])
   #      raw_input('HOLD\n\n')

   ###
   #H: scipyMethods are probably broken since the interface has changed.
   ###

         if fitMethod not in scipyMethods:
           sys.stdout.write('+') ; sys.stdout.flush()
           myfit,FLIP = LMMin(p0,BPChan[chran],BPChan[chran+1])
           sys.stdout.write('.') ; sys.stdout.flush()
         else:
         #  if fitMethid == 'COBYLA':  
         #    mymin = spopt.minimize(PS.GetChi2,p0,args=(-1.0, BPChan[chran],BPChan[chran+1]),method=fitMethod,options={'rhobeg':0.01})
         #  else:
           mymin = spopt.minimize(PS.GetChi2,p0,args=(-1.0, BPChan[chran],BPChan[chran+1],0),method=fitMethod)
           FLIP = False

       #    mymin = spopt.minimize(PS.GetChi2,p0,args=(laux,fitAnts, BPChan[chran],BPChan[chran+1],solveAmp,-1.0,useCov),method=fitMethod)

           myfit = mymin.values()[5]

   ###
   #T: scipyMethods are probably broken since the interface has changed.
   ###

         for ci,calant in enumerate(fitAnts):
           PhasFactor = {True:np.pi, False: 0.0}[FLIP and (calant in linAntIdxTrue)]
           if solveAmp:
# AMP+PHASE SPACE:
             temp[ci][BPChan[chran]:BPChan[chran+1]+1]= (myfit[2*ci]*np.exp(1.j*(PhasFactor + myfit[2*ci+1])))
           else:
             temp[ci][BPChan[chran]:BPChan[chran+1]+1]= np.exp(1.j*(PhasFactor+myfit[ci]))

       for ci,calant in enumerate(fitAnts):
         CGains['XYratio'][antcodes[calant-1]].append(np.copy(1./np.abs(temp[ci])))
         CGains['XYadd'][antcodes[calant-1]].append(np.copy(-180./np.pi*np.angle(temp[ci])))

       printMsg("Done with BP mode\n")

# MBD MODE:
    else:   # not BP MODE
      printMsg("\n Estimate antenna cross-pol gains: MBD mode\n")
      p0 = []
      for ci in fitAnts:
        if solveAmp:
          p0 += [ 1.0, 0.0]
        else:
          p0 += [0.0]
      for ci in fitAnts:
        p0 += [0.0] 
      laux = list(doIF)
      ### this line was missing:
      Npar = len(fitAnts)*{True:2,False:1}[solveAmp]
      ### ^^^^ line was missing,
      ###PS.SetFit(Npar,laux,fitAnts,solveAmp,solveQU,Stokes,useCov)
      ### wrong number of arguments
      PS.SetFit(Npar,laux,fitAnts,solveAmp,solveQU,Stokes,useCov,feedRot)
 #     print 'First Chi2: ', PS.GetChi2(np.array(p0),-1.0,0,MaxChan-1,solveAmp,solveQU,Stokes,-1.0,False)
      if fitMethod not in scipyMethods: #=='Levenberg-Marquardt':
        myfit = LMMin(p0,0,MaxChan-1)
        sys.stdout.write('.') ; sys.stdout.flush()
      else:
        mymin = spopt.minimize(PS.GetChi2,p0,args=(-1.0, 0,MaxChan-1,0),method=fitMethod)
        myfit = mymin.values()[5]

      RefFreq = AllFreqs[0][0]
      for ci,calant in enumerate(fitAnts):
       CGains['XYadd'][antcodes[calant-1]] = []
       CGains['XYratio'][antcodes[calant-1]] = []
       for plii,pli in enumerate(doIF):
        if solveAmp:
# RE+IM SPACE:
#          CrossGain = (myfit[2*ci] + 1.j*myfit[2*ci+1])*np.exp(1.j*(AllFreqs[plii]-RefFreq)*myfit[2*len(fitAnts)+ci]) 
# AMP+PHASE SPACE:
          CrossGain = (myfit[2*ci]*np.exp(1.j*myfit[2*ci+1]))*np.exp(1.j*(AllFreqs[plii]-RefFreq)*myfit[2*len(fitAnts)+ci])
        else:
          CrossGain = (np.exp(1.j*myfit[ci]))*np.exp(1.j*(AllFreqs[plii]-RefFreq)*myfit[len(fitAnts)+ci])

        CGains['XYadd'][antcodes[calant-1]].append(np.copy(-180./np.pi*np.angle(CrossGain)))
        CGains['XYratio'][antcodes[calant-1]].append(cp.copy(1./np.abs(CrossGain)))
      printMsg("Done with MBD mode\n")
    # end of if BP else MBD MODE: not BP MODE

    # see what we got...
    #print(CGains['XYadd'])
    #print(CGains['XYratio'])
    #print(CGains['aPrioriXYGain'])
    print('CGains contains: ',[(k,len(CGains[k])) for k in CGains.keys()])

    #if False:
    try:    # to plot something
      fig = pl.figure()
      MaxG = 0.0
      color = ['r','g','b','k','m','y','c']
      symbol = ['o','^','x']
      Freq2Plot = np.concatenate(AllFreqs)/1.e9

      printMsg('Working subplot 1')
      sub1 = fig.add_subplot(211)
      for antii,anti in enumerate(CGains['XYadd'].keys()):
        printMsg('antii,anti is',antii,anti)
        sub1.plot(Freq2Plot,np.concatenate(
            [np.array(ll) for ll in CGains['XYadd'][anti]]),
            symbol[((antii)//len(color))%len(symbol)]+color[(antii)%len(color)],
            label='ANT. '+str(anti))

      printMsg('Working subplot 2')
      sub2 = fig.add_subplot(212,sharex=sub1)
      for antii,anti in enumerate(CGains['XYratio'].keys()):
        printMsg('antii,anti is',antii,anti)
        toplot = np.concatenate([np.array(ll) for ll in CGains['XYratio'][anti]])
        MaxG = max(MaxG,np.max(toplot))
        sub2.plot(Freq2Plot,toplot,
            symbol[((antii)//len(color))%len(symbol)]+color[(antii)%len(color)],
            label='ANT. '+str(anti))

      printMsg('Labelling and adjusting')
      sub1.set_ylim((-180.,180.))
      sub2.set_ylim((0.,1.1*MaxG))
      pl.setp(sub1.get_xticklabels(),'visible',False)
      Dnu = np.max(Freq2Plot)-np.min(Freq2Plot)
      sub1.set_xlim((np.min(Freq2Plot) - Dnu*0.1,np.max(Freq2Plot) + Dnu*0.45))
      sub2.set_xlim((np.min(Freq2Plot) - Dnu*0.1,np.max(Freq2Plot) + Dnu*0.45))
      sub2.set_ylim((0.,2.5))

      sub1.legend(numpoints=1)
      sub1.set_ylabel('Cross-Phase (deg.)')
      sub2.set_ylabel('Cross-Amp (Norm.)')
      sub2.set_xlabel('Frequency (GHz)')

      fig.suptitle('CROSS-POLARIZATION GAINS')
      pl.savefig('Cross-Gains.png')
     #pl.show()
    except Exception as ex:
      printMsg('Sorry amigo, plots did not work:\n %s' % str(ex))
    # end of try to plot something

    PS.FreeData()
    printMsg("PS Data freed\n")

   else: # if goodclib
    printMsg("\n\n  doSolve can ONLY work with the source was compiled with DO_SOLVE=True\n  PLEASE, RECOMPILE!\n\n")

  else:  # if doSolve >= 0.0:

    printMsg("\n\n  doSolve was not requested\n\n")
    CGains = {'aPrioriXYGain':PrioriGains}

  # end of doSolve if...else



# PLOT FRINGES:
  if plotAnt not in calAnts:
   didit = 1
   printMsg("plotAnt not in calAnts, so skipping plots")

  # didit is returned from PC, and if nonzero will have terminated above.
  if plotFringe and didit==0:
   printMsg("proceding to fringe plots with plotAnt %d..." % plotAnt)

#### remainder of file INPC

   fig = pl.figure(figsize=(12,6))

   fig2 = pl.figure()

   fringeAmps = {} 
   fringeAmpsMix = {} 
   ResidGains = {} 
   MixedCalib = {} 


# Filter out IFs with no data:
   GoodIFs = []
   for pli in plotIF:
     if os.stat("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i"%pli).st_size>10:
       GoodIFs.append(pli)
     else:
       printMsg(("WARNING! IF %i was NOT polconverted properly\n"%pli) +
         ("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i missing"%pli))

# start of GoodIFs pli loop
   for pli in GoodIFs:
    print('\n\n')
    printMsg("Plotting selected fringe for IF #%i"%pli)

    frfile = open("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i"%pli,"rb")

### header format changed with addition of "FILE" for difxdfile iteration
#
#    alldats = frfile.read(4)
#    nchPlot = stk.unpack("i",alldats[:4])[0]
#    dtype = np.dtype([("FILE",np.int32),
#                      ("JDT",np.float64),("ANT1",np.int32),("ANT2",np.int32),
#                      ("PANG1",np.float64),("PANG2",np.float64),
#                      ("MATRICES",np.complex64,12*nchPlot)])
### and again with UVDIST getting added in 2.0.4-ivan

#   frfile = open("POLCONVERT.FRINGE/POLCONVERT.FRINGE_IF%i"%pli,"rb")

    alldats = frfile.read(5)
    #   isParang is a 1-byte bool
    nchPlot, isParang = stk.unpack("i?", alldats)
    dtype = np.dtype(
        [
            ("FILE", np.int32),
            ("JDT", np.float64),
            ("ANT1", np.int32),
            ("ANT2", np.int32),
            ("PANG1", np.float64),
            ("PANG2", np.float64),
            ("UVDIST", np.float64),
            ("MATRICES", np.complex64, 12 * nchPlot),
        ]
    )

# There is a silly bug in Python 2.7, which generates
# an "integer is required" error in the first try to read:
    try:
      fringe = np.fromfile(frfile,dtype=dtype)
      printMsg("Read fringe data from file POLCONVERT.FRINGE_IF%i"%pli)
    except:
      fringe = np.fromfile(frfile,dtype=dtype)
      printMsg("Exceptional Read of fringe data POLCONVERT.FRINGE_IF%i"%pli)
      printMsg("len(fringe) = %d" % len(fringe))
    frfile.close()


    # start of for ant1 in linAntIdx
    for ant1 in linAntIdxTrue:

     if pli == GoodIFs[0]:
      MixedCalib[ant1] = {}
      fringeAmps[ant1] = []
      fringeAmpsMix[ant1] = []
      ResidGains[ant1] = []

     # start of fringe loop on ant1-ant2 baseline
     for ant2 in [plotAnt]:

      AntEntry1 = np.logical_and(
        fringe[:]["ANT1"] == ant1,fringe[:]["ANT2"] == ant2)
      AntEntry2 = np.logical_and(
        fringe[:]["ANT2"] == ant1,fringe[:]["ANT1"] == ant2)
      ### all the SWIN files (difxdfile) go to the same place
      ### to only plot the first one, AntEntry changes to
      ### np.logical_and(fringe[:]["FILE"]==0,
      ###        np.logical_or(AntEntry1,AntEntry2))
      AntEntry = np.logical_or(AntEntry1,AntEntry2)
      printMsg("np.sum(AntEntry) > 0: '" + str(np.sum(AntEntry)) + "'")

      # start of np.sum(AntEntry)
      if np.sum(AntEntry)>0:
       printMsg("Something to plot, apparently")

       if pli == GoodIFs[0]:
        MixedCalib[ant1][ant2] = []

# This is to store all fringes with linear-feeds involved:
       # if len(fringe)>0
       if nchPlot > 0 and len(fringe)>0:
         uncal = [(fringe[AntEntry]["MATRICES"])[:,i::12] for i in range(4)]
         cal = [(fringe[AntEntry]["MATRICES"])[:,i::12] for i in range(4,8)]
         Kmat = [(fringe[AntEntry]["MATRICES"])[:,i::12] for i in range(8,12)]
         rchan = np.shape(uncal[0])[0] 

# Zoom for the image plots: a square centered on nchPlot and rchan:
         ToZoom = min(rchan,nchPlot,npix)

         t0 = (nchPlot - ToZoom)//2
         t1 = (nchPlot + ToZoom)//2
         Ch0 = (rchan - ToZoom)//2
         Ch1 = (rchan + ToZoom)//2

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

           MAXK = max(list(map(np.max,list(map(np.abs,Kmat)))))
           MINK = min(list(map(np.min,list(map(np.abs,Kmat)))))

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
         if DEBUG: print("fringe maximum debugging:")
         RMAX = np.unravel_index(np.argmax(RR+LL),np.shape(RRVis))
         if DEBUG: print(' unravel_index:',RMAX)
         MAXVis = [RRVis[RMAX],RLVis[RMAX],LRVis[RMAX],LLVis[RMAX]]
         if DEBUG: print(' MAXVis:',MAXVis)
         try:
             MixedCalib[ant1][ant2].append([np.array(MM) for MM in MAXVis])
         except Exception as ex:
            print('  MixedCalib np exception',str(ex))
         MAXl = [RR[RMAX],RL[RMAX],LR[RMAX],LL[RMAX]]
         if DEBUG: print(' MAXl:',MAXl)
         MAX = max(MAXl)
         if DEBUG: print(' Final MAX',MAX,'done')

         #  raw_input('HOLD')

# Plot fringes:
         if ant2==plotAnt or ant1==plotAnt:
          printMsg('making Fringe.plot.ANT%i-%i.IF%i.png'%(ant1,ant2,pli))
          fig.clf()
          ratt = 1.0   

          fig.subplots_adjust(left=0.02,right=0.98,wspace=0.05,hspace=0.2)
 
          try:  #to clear out previous plot stuff -- is this necessary?
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
          sub0.imshow(np.abs(RRu[Ch0:Ch1,t0:t1]),
            vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub0.set_title('XR mixed')
          pl.setp(sub0.get_xticklabels(),visible=False)
          pl.setp(sub0.get_yticklabels(),visible=False)

          sub1 = fig.add_subplot(242,sharex=sub0,sharey=sub0)
          sub1.imshow(np.abs(RLu[Ch0:Ch1,t0:t1]),
            vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub1.set_title('XL mixed')
          pl.setp(sub1.get_xticklabels(),visible=False)
          pl.setp(sub1.get_yticklabels(),visible=False)

          sub2 = fig.add_subplot(243,sharex=sub0,sharey=sub0)
          sub2.imshow(RR[Ch0:Ch1,t0:t1],
            vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub2.set_title('RR cal')
          pl.setp(sub2.get_xticklabels(),visible=False)
          pl.setp(sub2.get_yticklabels(),visible=False)

          sub3 = fig.add_subplot(244,sharex=sub0,sharey=sub0)
          sub3.imshow(RL[Ch0:Ch1,t0:t1],
            vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub3.set_title('RL cal')
          pl.setp(sub3.get_xticklabels(),visible=False)
          pl.setp(sub3.get_yticklabels(),visible=False)


          sub4 = fig.add_subplot(245,sharex=sub0,sharey=sub0)
          sub4.imshow(np.abs(LRu[Ch0:Ch1,t0:t1]),
            vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub4.set_title('YR mixed')
          pl.setp(sub4.get_xticklabels(),visible=False)
          pl.setp(sub4.get_yticklabels(),visible=False)

          sub5 = fig.add_subplot(246,sharex=sub0,sharey=sub0)
          sub5.imshow(np.abs(LLu[Ch0:Ch1,t0:t1]),
            vmin=0.0,vmax=MAXu,interpolation='nearest',aspect=ratt)
          sub5.set_title('YL mixed')
          pl.setp(sub5.get_xticklabels(),visible=False)
          pl.setp(sub5.get_yticklabels(),visible=False)

          sub6 = fig.add_subplot(247,sharex=sub0,sharey=sub0)
          sub6.imshow(LR[Ch0:Ch1,t0:t1],
            vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub6.set_title('LR cal')
          pl.setp(sub6.get_xticklabels(),visible=False)
          pl.setp(sub6.get_yticklabels(),visible=False)

          sub7 = fig.add_subplot(248,sharex=sub0,sharey=sub0)
          sub7.imshow(LL[Ch0:Ch1,t0:t1],
            vmin=0.0,vmax=MAX,interpolation='nearest',aspect=ratt)
          sub7.set_title('LL cal')
          pl.setp(sub7.get_xticklabels(),visible=False)
          pl.setp(sub7.get_yticklabels(),visible=False)

          fig.suptitle('DELAY-RATE FRINGE FOR IF %i (BASELINE TO ANT #%i) FROM %i-%02i:%02i:%02i TO %i-%02i:%02i:%02i'%tuple([pli,plotAnt]+plotRange))
          fig.savefig('FRINGE.PLOTS/Fringe.plot.ANT%i-%i.IF%i.png'%(ant1,ant2,pli))

# Plot calibration matrix:
          ratt = float(np.shape(Kmat[0])[1])/float(np.shape(Kmat[0])[0])

          printMsg('creating Kmatrix_AMP_IF%i-ANT%i.png'%(pli,ant1))
          fig2.clf()

          fig2.subplots_adjust(right=0.8)
          cbar_ax = fig2.add_axes([0.85, 0.15, 0.05, 0.7])

          sub = fig2.add_subplot(221)
          im = sub.imshow(np.abs(Kmat[0]),
            vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(222)
          sub.imshow(np.abs(Kmat[1]),
            vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(223)
          sub.imshow(np.abs(Kmat[2]),
            vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(224)
          sub.imshow(np.abs(Kmat[3]),
            vmin=0.0,vmax=MAXK,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          cbar = fig2.colorbar(im, cax=cbar_ax)
          cbar.set_label("Amplitude (Norm)")
          pl.suptitle('CAL.MATRIX.AMP IF %i - FREQ = Y - TIME = X'%pli)

          pl.savefig('CONVERSION.MATRIX/Kmatrix_AMP_IF%i-ANT%i.png'%(pli,ant1))

          printMsg('creating Kmatrix_PHAS_IF%i-ANT%i.png'%(pli,ant1))
          fig2.clf()

          fig2.subplots_adjust(right=0.8)
          cbar_ax = fig2.add_axes([0.85, 0.15, 0.05, 0.7])

          sub = fig2.add_subplot(221)
          im = sub.imshow(180./np.pi*np.angle(Kmat[0]),
            vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(222)
          sub.imshow(180./np.pi*np.angle(Kmat[1]),
            vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{XL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(223)
          sub.imshow(180./np.pi*np.angle(Kmat[2]),
            vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YR}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          sub = fig2.add_subplot(224)
          sub.imshow(180./np.pi*np.angle(Kmat[3]),
            vmin=-180.,vmax=180.,interpolation='nearest',aspect=ratt)
          pl.title(r'$K_{YL}$')
          pl.setp(sub.get_xticklabels(),visible=False)
          pl.setp(sub.get_yticklabels(),visible=False)

          cbar = fig2.colorbar(im, cax=cbar_ax)
          cbar.set_label("Phase (deg.)")
          pl.suptitle('CAL.MATRIX.PHAS IF %i - FREQ = Y - TIME = X'%pli)

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

          toprint = [pli,
            MAXl[0]/MAX,DRR,
            MAXl[3]/MAX,DLL,
            MAXl[1]/MAX,DRL,
            MAXl[2]/MAX,DLR,
            MAX/len(fringe),RLRatio]
          fringeAmps[ant1].append([pli,MAXl[0],MAXl[0]/DRR,
            MAXl[3],MAXl[3]/DLL,MAXl[1],MAXl[1]/DRL,MAXl[2],MAXl[2]/DLR,
            RLRatio])
          fringeAmpsMix[ant1].append([pli,MAXmix[0],MAXmix[0]/DRRu,
            MAXmix[3],MAXmix[3]/DLLu,MAXmix[1],MAXmix[1]/DRLu,MAXmix[2],
            MAXmix[2]/DLRu])

          printMsg("writing FRINGE.PEAKS%i-ANT%i.dat"%(pli,ant1))
          pfile = open('FRINGE.PEAKS/FRINGE.PEAKS%i-ANT%i.dat'%(pli,ant1),'w' )
          pmsg = (('\n\n\nFOR IF #%i. NORM. FRINGE PEAKS: \n' +
            '  RR: %.2e ; SNR: %.1f \n' +
            '  LL: %.2e ; SNR: %.1f \n' +
            '  RL: %.2e ; SNR: %.1f \n' +
            '  LR: %.2e ; SNR: %.1f\n\n' +
            ' AMPLITUDE: %.2e\nRL/LR Norm.: %.2e\n\n\n')%tuple(toprint))

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
          printMsg("wrote FRINGE.PEAKS%i-ANT%i.dat"%(pli,ant1))
         # end of if for this baseline to be plotted
       # end of if len(fringe)>0
       else:
          printMsg("Fringe length was zero")

      # end of np.sum(AntEntry)
      else:
       printMsg("Nothing to plot, apparently")
    # start of for ant1 in linAntIdx

     # end of fringe loop on ant1-ant2 baseline

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
    # end of if ant1 in linAntIdx

 #  try:
   for thisAnt in linAntIdxTrue:
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
     uylim = np.max(CONVAMP[:,-1])
     if np.isfinite(uylim):
        pl.ylim((0,uylim))
     else:
        pl.ylim((0,2))
     # pl.ylim((0.,np.max(CONVAMP[:,-1])*1.05))

     fig3.suptitle(jobLabel(DiFXinput)+' ANT: %i v %i'%(thisAnt,plotAnt))
     fig3.savefig('FRINGE.PLOTS/RL_LR_RATIOS_ANT_%i_%i.png'%(thisAnt,plotAnt))

  else:

      printMsg('NO DATA TO PLOT!') 
# end of plotting

  printMsg('Please, check the PolConvert.log file for special messages.',dolog=False)

  if sys.version_info.major < 3:
    ofile = open('PolConvert.XYGains.dat','w')
  else:
    ofile = open('PolConvert.XYGains.dat','wb')
  cgs = str(CGains)
  if len(cgs) > 79: printMsg("%s..." % cgs[0:78])
  else:             printMsg("%s" % cgs)
  try:
    pk.dump(CGains,ofile)
  except Exception as ex:
    printMsg(str(ex))
  ofile.close()
  printMsg('PolConvert.XYGains.dat was written with CGains' + str(CGains.keys()))
  printMsg('Task PolConvert is Done\n\n')
  return CGains   # RETURN!

#
# vim: set nospell:
#eof
#
