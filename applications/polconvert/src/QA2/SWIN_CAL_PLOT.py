#!/bin/python3
#
#

######
### SWIN_CAL_PLOT: A script for a fast "quick&dirty" calibration and plotting
###                in full polarization, based on SWIN files.
#
######    Ivan Marti-Vidal (Univ. Valencia) -- Version 1.0b (05 Mar. 2026)


import struct as stk
import sys, os, glob
import numpy as np
import matplotlib as mpl
import pylab as pl
import gc
from multiprocessing import Pool
import pickle as pk
import time

mpl.use("Agg")

helptxt = """
SWIN CAL PLOT (version 1.0b). I. Marti-Vidal & C. Goddi (2026)

Script for a quick assessment of the quality of a polconversion.

Works on SWIN products (no need to export to FITS, MS, nor MK4).

Performs a basic baseline-based fringe fitting (with one 
solution per scan) and a bandpass calibration (averaging scans).
Then, calibrates the data and plots the cross-spectra of all 
polarizations.

Only the baselines to the reference antenna (ALMA) are used.

WARNING!!!
THIS SCRIPT IS QUITE MEMORY-HUNGRY! IT IS DESIGNED TO WORK ON 
RELATIVELY SMALL DATASETS (i.e., CALIBRATOR DATA AND A SMALL 
SET OF BASELINES).

Argument list:

--datadir :: Directory with the SWIN products.

             NOTE: It is assumed that the product names have the structure:
 
             EXPT-N-bI_SCN.*

             where "EXPT" is experiment code, N is a number, I is the band
             and SCN is scan number. Example: e22c20-1-b2_1088.difx


--plotdir :: Directory where the plots will be stored.

--source :: Name of (calibrator) source to plot.
            ONLY ONE SOURCE IS ALLOWED.

--nproc :: Number of allowed processes to run in parallel.
           Default is 20

--bands :: Bands to plot. List of integers. Can be 
           separated by spaces or commas.
           Default is all bands found in datadir.

--refant :: ID of the reference antenna (default AA).

--antennas :: The other antennas to calibrate and plot.
              (in principle, AX and/or LM should suffice
              for a quick assessment of the polconversion).
              Default is all antennas available.

--noEHT :: If present, the naming assumptions for the EHT are not used. In this case,
           the "--bands" option will not work.


EXAMPLE:

- Plot the AA-AX baseline for all bands and scans of 3C279. The plots
  will be saved in directory 3C279_PC_PLOTS and the SWIN products are 
  stored in the ../DiFX folder:

SWIN_CAL_PLOT.py --datadir ../DiFX --plotdir 3C279_PC_PLOTS  --source 3C279 --antennas AX


"""


print(sys.argv)
print('\n\n')

if len(sys.argv)==1:
   print(helptxt)
   os._exit(0)

allOptions = ['--nproc','--datadir','--plotdir','--source','--bands','--refant','--antennas','--noEHT']
for arg in sys.argv:
    if arg.startswith('-') and arg not in allOptions:
        raise Exception("Unknown argument %s"%arg)


##################################
## Read command-line arguments:

noEHT = "--noEHT" in sys.argv

if '--datadir' in sys.argv:
   DIR = sys.argv[sys.argv.index('--datadir')+1]
   if not os.path.exists(DIR):
      raise Exception("%s does NOT exist."%DIR)
else:
   raise Exception("--datadir is a MANDATORY argument")


if '--plotdir' in sys.argv:
   PLOTDIR = sys.argv[sys.argv.index('--plotdir')+1]
   if not os.path.exists(PLOTDIR):
      print("Creating directory %s"%PLOTDIR)
      os.system("mkdir %s"%PLOTDIR)
    #  raise Exception("%s does NOT exist."%PLOTDIR)
else:
   raise Exception("--plotdir is a MANDATORY argument")



if '--source' in sys.argv:
   SOURCE = sys.argv[sys.argv.index('--source')+1]
else:
   raise Exception("--source is a MANDATORY argument")


if '--nproc' in sys.argv:
   NPROC = int(sys.argv[sys.argv.index('--nproc')+1])
else:
   NPROC = 20


BANDS = []
if noEHT:
   BANDS = [0]
elif '--bands' in sys.argv:
   i0 = sys.argv.index('--bands')
   i1 = -1
   for k in range(i0+1,len(sys.argv)):
      if sys.argv[k].startswith('-'):
         i1 = k
         break
   if i1<0:
      BANDSTR = sys.argv[i0+1:]
   else:
      BANDSTR = sys.argv[i0+1:i1]
   for band in BANDSTR:
      BANDS += list(map(int,band.split(",")))


if '--refant' in sys.argv:
   PLANT = sys.argv[sys.argv.index('--refant')+1]
else:
   PLANT = 'AA'


OTHERANT = []
if '--antennas' in sys.argv:
   i0 = sys.argv.index('--antennas')
   i1 = -1
   for k in range(i0+1,len(sys.argv)):
      if sys.argv[k].startswith('-'):
         i1 = k
         break
   if i1<0:
      ANTSTR = sys.argv[i0+1:]
   else:
      ANTSTR = sys.argv[i0+1:i1]
   for ant in ANTSTR:
      OTHERANT += list(ant.split(","))











######################
### SCRIPT STARTS: ###
######################


tic = time.time()

# All bands will be processed if no band specified:
if len(BANDS)==0:
  temp = [os.path.basename(fi).split("_")[0].split("-")[-1][1:] for fi in glob.glob(os.path.join(DIR,"*.calc"))]
  BANDS = np.unique(list(map(int,temp)))

print("\n Will calibrate & plot bands ",BANDS)


#raw_input("CACA")

for BAND in BANDS:

 print("\n Processing band %i"%BAND)

## Look for the scans:
 if noEHT:
    allScans = glob.glob(os.path.join(DIR,"*_[0-9]*.calc"))
 else:
    allScans = glob.glob(os.path.join(DIR,"*b%s_*.calc"%BAND))
 SCAN = [] ; SCANNAMES = []
 for scan in allScans:
   infi = open(scan,"r")
   for line in infi.readlines():
      if "SOURCE 0 NAME" in line:
         sour = line.split()[-1]
         if sour == SOURCE:
            SCAN.append(os.path.basename(scan).split("_")[-1].split(".")[0])
            SCANNAMES.append('.'.join(scan.split('.')[:-1]))

 print("Will read scans: ",SCAN)


 SCANS = []

 for scnum,scan in enumerate(SCAN):
  print("\n Reading scan %s [%i of %i]"%(scan,scnum+1,len(SCAN)))
     
  #imf = open(os.path.join(DIR,'*-b%i_%s.im'%(BAND,scan)),"r")
  imf = open(SCANNAMES[scnum]+'.im','r')

  TNAME = {}
  PLOTID = -1
  TOPLOT = []
  for line in imf.readlines():
      if line.startswith("TELESCOPE") and "NAME:" in line:
          temp = line.split()
          IDX = int(temp[1])
          NAM = str(temp[-1])
          TNAME[IDX+1] = NAM
          if NAM == PLANT:
            PLOTID = IDX+1
          if len(OTHERANT)==0 or NAM in OTHERANT:
            TOPLOT.append(IDX+1)
      elif line.startswith("SCAN ") and "POINTING SRC:" in line:
          SNAME = str(line.split(":")[-1].split()[0])
  imf.close()


## Only if plotant is in the data:
  if PLOTID > 0:

   # pathname = os.path.join(DIR,'*-b%i_%s.difx'%(BAND,scan))
    pathname = SCANNAMES[scnum]+'.difx'
    filename = glob.glob(os.path.join(pathname,'DIFX_*.b*'))[0]

    frfile = open(filename,"rb")

    WORD = b'\x00\xff\x00\xff\x01\x00\x00\x00'


## Figure out number of channels:
    temp = frfile.read(8+4+4+8+4+4+4+2+4+8+8*3)
    for i in range(4096):
      test = frfile.read(8)
      if test==WORD:
        break

    NCHAN = i
    print(" There seem to be %i channels."%i)
    frfile.close()


## Read data:
    frfile = open(filename,"rb")
    alldats = frfile.read(8)
    i=0
    DATA = {'ANTS':[],'JDT':[],'IF':[],'POL':[],'VIS':[],'UVW':[],"NAME":SNAME,"SCAN":scan}
    BANDPASS = {} # Bandpass amplitudes will be derived from autocorrs (if available).
    for ant in TOPLOT:
      BANDPASS[TNAME[ant]] = {} 
      for poli in ['R','L']:
          BANDPASS[TNAME[ant]][poli] = {}


    ALLANTS = []
    while True:
      if i%1024==0:
        sys.stdout.write('\r Reading VIS %i'%i)
        sys.stdout.flush()
      alldats = frfile.read(4+4+8+4+4+4)
      if not alldats: break
      BASEL,MJD,SEC,CFI,SI,SPI = stk.unpack("iidiii",alldats)
      A1 = BASEL//256
      A2 = BASEL%256
      alldats = frfile.read(2)
      P1,P2 = stk.unpack("cc",alldats)
      STRPOL = str(P1.decode('utf-8')+P2.decode('utf-8'))

# Autocorrelations:
      if A1 == A2 and A1 in TOPLOT:
        POLCOPY = ''
        if STRPOL in ['RR','XR','RX','XX']:
           POLCOPY ='R'
        if STRPOL in ['LL','YL','LY','YY']:
           POLCOPY = 'L'

        if len(POLCOPY)>0: # Generate entry for this IF:
          if SPI not in BANDPASS[TNAME[A1]][POLCOPY].keys():
             BANDPASS[TNAME[A1]][POLCOPY][SPI] = 0.0

          alldats = frfile.read(4)
          PB = stk.unpack("i",alldats)
          alldats = frfile.read(8)
          PW = stk.unpack("d",alldats)
          alldats = frfile.read(8*3)
## Accumulate the bandpass for this IF as the channel average:
          ACORR = 0.0
          for chi in range(NCHAN):
            alldats = frfile.read(8)
            Re,Im = stk.unpack("ff",alldats)
            ACORR += np.sqrt(Re**2. + Im**2.)
          BANDPASS[TNAME[A1]][POLCOPY][SPI] = np.sqrt(ACORR/NCHAN)
          hola = frfile.read(8)

        else:
          alldats=frfile.read(38+8*NCHAN+6)  



## Cross-correlations:
      elif (A1 == PLOTID and A2 in TOPLOT) or (A2 == PLOTID and A1 in TOPLOT):
        alldats = frfile.read(4)
        PB = stk.unpack("i",alldats)
        alldats = frfile.read(8)
        PW = stk.unpack("d",alldats)
        alldats = frfile.read(8*3)
        U,V,W = stk.unpack("ddd",alldats)
        visib = np.zeros(NCHAN,dtype=np.complex64)
        for chi in range(NCHAN):
          alldats = frfile.read(8)
          Re,Im = stk.unpack("ff",alldats)
          visib[chi] = Re + 1.j*Im
        hola = frfile.read(8)

        i += 1
        DATA['ANTS'].append("%s-%s"%(TNAME[A1],TNAME[A2]))
        DATA['JDT'].append(MJD+SEC)
        DATA['IF'].append(SPI)
        DATA['POL'].append(STRPOL)
        DATA['UVW'].append([U,V,W])
        DATA['VIS'].append(visib)
      else:
          alldats=frfile.read(38+8*NCHAN+6)  

    frfile.close()
    print("\n Done!")

    if len(DATA['ANTS'])>0:
      DATA['ANTS'] = np.array(DATA['ANTS'])
      DATA['JDT'] = np.array(DATA['JDT'])
      DATA['IF'] = np.array(DATA['IF'])
      DATA['POL'] = np.array(DATA['POL'])
      DATA['VIS'] = np.array(DATA['VIS'])
      DATA['UVW'] = np.array(DATA['UVW'])
      SCANS.append(DATA)


# Normalize the amplitude bandpass across the whole band:
      for anti in BANDPASS.keys():
        for pol in ['R','L']:
          TOTSUM = 0.0
          for SPI in BANDPASS[anti][pol].keys():
            TOTSUM += BANDPASS[anti][pol][SPI]
          TOTSUM /= np.max([1,len(BANDPASS[anti][pol].keys())])
          if TOTSUM > 0.0:
            for SPI in BANDPASS[anti][pol].keys():
              BANDPASS[anti][pol][SPI] /= TOTSUM

# Apply the amplitude bandpass:
      for datum in range(len(DATA['ANTS'])):
        ant2 = DATA['ANTS'][datum].split('-')[1]
        STRPOL = DATA['POL'][datum]
        SPI = int(DATA['IF'][datum])
        if STRPOL in ['RR','XR','RX','XX']:
           POLCOPY ='R'
        if STRPOL in ['RL','XL','RY','XY']:
           POLCOPY = 'L'
        if STRPOL in ['LR','YR','LX','YX']:
           POLCOPY = 'R'
        if pol in ['LL','YL','LY','YY']:
           POLCOPY = 'L'
        if ant2 in BANDPASS.keys() and SPI in BANDPASS[ant2][POLCOPY].keys():
           DATA['VIS'][datum][:] /= BANDPASS[ant2][POLCOPY][SPI]



 RLPHAS = []
 ALLIFS = [np.unique(SCANS[i]['IF']) for i in range(len(SCANS))]


 for i,scan in enumerate(SCANS):
   IFMASK = {}
   for k in ALLIFS[i]:
     IFMASK[k] = scan['IF']==k
   scan['mask'] = IFMASK



## Calibrate single-band delays and rates. 
## Will apply their median across the band:
 def CALIB_PARALLEL(scan):

    IFMASK = scan['mask']
    ALLBAS = np.unique(scan['ANTS'])
    ALLIFS = np.unique(scan['IF'])

    SBAS = np.unique(scan["ANTS"])
    scanName = scan["SCAN"]
    AVERAGES = {'scanName':scanName, "source":scan["NAME"], 'vis':{},'corrProds':{}}
    for AVKEY in SBAS:
       AVERAGES['vis'][AVKEY] = {}
       POLS = np.unique(scan['POL'])
       dataPol = {}
       for pol in POLS:
             if pol in ['RR','XR','RX','XX']:
                 dataPol['RR'] = str(pol)
             if pol in ['RL','XL','RY','XY']:
                 dataPol['RL'] = str(pol)
             if pol in ['LR','YR','LX','YX']:
                 dataPol['LR'] = str(pol)
             if pol in ['LL','YL','LY','YY']:
                 dataPol['LL'] = str(pol)
       AVERAGES['corrProds'][AVKEY] = dataPol
       mask0 = scan["ANTS"]==AVKEY
       maskRR = np.logical_and(mask0,scan['POL']==dataPol['RR'])
       maskLL = np.logical_and(mask0,scan['POL']==dataPol['LL'])
       maskRL = np.logical_and(mask0,scan['POL']==dataPol['RL'])
       maskLR = np.logical_and(mask0,scan['POL']==dataPol['LR'])
       RATES = []; SBDs = []; WGT = []; SHAPES = {}

       if np.sum(maskRR)>0 and np.sum(maskLL)>0:
         thisWgtR = []; thisWgtL = []
         for ki,k in enumerate(ALLIFS):
           RR = np.copy(scan['VIS'][np.logical_and(maskRR,IFMASK[k]),:])
           SHAPE = np.shape(RR)
           thisRate = []; thisDelay = []
           if SHAPE[0]>0: ## Will fringe-fit with zero-padding factor 5
             SHAPES[k] = SHAPE
             ZPAD = 11 # Must be an ODD number
             SHAPE2 = (int(SHAPE[0]*ZPAD), int(SHAPE[1]*ZPAD))
             RRSHAPE = np.zeros(SHAPE2,dtype=np.complex64)
             RRSHAPE[int((ZPAD-1)/2)*SHAPE[0]:int((ZPAD+1)/2)*SHAPE[0],int((ZPAD-1)/2)*SHAPE[1]:int((ZPAD+1)/2)*SHAPE[1]] = RR
             FRINGE = np.fft.fftshift(np.abs(np.fft.fft2(np.fft.ifftshift(RRSHAPE))))
             MAX = np.unravel_index(np.argmax(FRINGE),SHAPE2)
             DELR = (MAX[0]-SHAPE2[0]//2)/SHAPE2[0]; RATR = (MAX[1]-SHAPE2[1]//2)/SHAPE2[1]
             thisRate.append(RATR); thisDelay.append(DELR); thisWgtR.append(np.max(FRINGE)**2.)
             del FRINGE, RR
           else:
             thisWgtR.append(0.0)
           LL = np.copy(scan['VIS'][np.logical_and(maskLL,IFMASK[k]),:])
           SHAPE = np.shape(LL)
           if SHAPE[0]>0: ## Will fringe-fit with zero-padding factor 5
             SHAPES[k] = SHAPE
             SHAPE2 = (int(SHAPE[0]*ZPAD), int(SHAPE[1]*ZPAD))
             LLSHAPE = np.zeros(SHAPE2,dtype=np.complex64)
             LLSHAPE[int((ZPAD-1)/2)*SHAPE[0]:int((ZPAD+1)/2)*SHAPE[0],int((ZPAD-1)/2)*SHAPE[1]:int((ZPAD+1)/2)*SHAPE[1]] = LL
             FRINGE = np.fft.fftshift(np.abs(np.fft.fft2(np.fft.ifftshift(LLSHAPE))))
             MAX = np.unravel_index(np.argmax(FRINGE),SHAPE2)
             DELL = (MAX[0]-SHAPE2[0]//2)/SHAPE2[0]; RATL = (MAX[1]-SHAPE2[1]//2)/SHAPE2[1]
             thisRate.append(RATL); thisDelay.append(DELL); thisWgtL.append(np.max(FRINGE)**2.)
             del FRINGE, LL
           else:
             thisWgtL.append(0.0)

           gc.collect()

           thisWgt = (thisWgtR[-1]+thisWgtL[-1])/2.

           if thisWgt > 0.0:
             RATES.append(2.*np.pi*np.average(thisRate))
             if len(thisDelay)==2:
               SBDs.append([2.*np.pi*thisDelay[0], 2.*np.pi*thisDelay[1]]) 
             else:
               SBDs.append([2.*np.pi*thisDelay[0], 2.*np.pi*thisDelay[0]]) 
             WGT.append(thisWgt)
           else:
             RATES.append(0.0); SBDs.append([0.,0.]); WGT.append(0.0)

         RATES = np.array(RATES); SBDs = np.array(SBDs); WGT = np.array(WGT)
         RATE = np.median(RATES[WGT>0.0])
         SBD = np.median(SBDs[WGT>0.0],axis=0)
         for ki,k in enumerate(ALLIFS):
           if k in SHAPES.keys():
             SHAPE = SHAPES[k]
           else:
             SHAPE = (0,0)

           SHAPE2 = (SHAPE[0]*5, SHAPE[1]*5)
           if SHAPE[0]>0:
             if thisWgtR[ki]>0.0:                        
               RCORR = np.linspace(SBD[0]*(-SHAPE[0]/2.+1),SBD[0]*(SHAPE[0]/2.),SHAPE[0])[:,np.newaxis]
               RCORR2 = RCORR + np.linspace(RATE*(-SHAPE[1]/2.+1),RATE*(SHAPE[1]/2.),SHAPE[1])[np.newaxis,:]
              # AVRR = np.average(scan['VIS'][np.logical_and(maskRR,IFMASK[k]),:]*np.exp(-1.j*RCORR2))
               VISRR = scan['VIS'][np.logical_and(maskRR, IFMASK[k]),:]
               n = min(VISRR.shape[0], RCORR2.shape[0])
               AVRR = np.average(VISRR[:n, :] * np.exp(-1.j * RCORR2[:n, :]))
             else:
               RCORR = 0.0; RCORR2 = 0.0; AVRR = 0.0
             if thisWgtL[ki]>0.0:
               LCORR = np.linspace(SBD[1]*(-SHAPE[0]/2.+1),SBD[1]*(SHAPE[0]/2.),SHAPE[0])[:,np.newaxis]
               LCORR2 = LCORR + np.linspace(RATE*(-SHAPE[1]/2.+1),RATE*(SHAPE[1]/2.),SHAPE[1])[np.newaxis,:]
            #   AVLL = np.average(scan['VIS'][np.logical_and(maskLL,IFMASK[k]),:]*np.exp(-1.j*LCORR2))
               VISLL = scan['VIS'][np.logical_and(maskLL, IFMASK[k]),:]
               n = min(VISLL.shape[0], LCORR2.shape[0])
               AVLL = np.average(VISLL[:n, :] * np.exp(-1.j * LCORR2[:n, :]))
             else:
               LCORR = 0.0; LCORR2 = 0.0; AVLL = 0.0
             if thisWgtL[ki]>0.0 and thisWgtR[ki]>0.0:
             #  AVRL = np.average(scan['VIS'][np.logical_and(maskRL,IFMASK[k]),:]*np.exp(-1.j*LCORR2))
             #  AVLR = np.average(scan['VIS'][np.logical_and(maskLR,IFMASK[k]),:]*np.exp(-1.j*RCORR2))
               VISRL = scan['VIS'][np.logical_and(maskRL, IFMASK[k]),:]
               n = min(VISRL.shape[0], RCORR2.shape[0])
               AVRL = np.average(VISRL[:n, :] * np.exp(-1.j * LCORR2[:n, :]))
               VISLR = scan['VIS'][np.logical_and(maskLR, IFMASK[k]),:]
               n = min(VISLR.shape[0], LCORR2.shape[0])
               AVLR = np.average(VISLR[:n, :] * np.exp(-1.j * RCORR2[:n, :]))
             else:
               AVRL = 0.0; AVLR = 0.0
             AVERAGES['vis'][AVKEY][ki] = [AVRR,AVLL,AVRL,AVLR] 
             del RCORR,RCORR2,LCORR,LCORR2 #,VISRR,VISLL,VISRL,VISLR
             gc.collect()
           else:
             AVERAGES['vis'][AVKEY][ki] = [0.0,0.0,0.0,0.0]

    gc.collect()
    print("Done for scan %s"%(str(scan["SCAN"])))
    return AVERAGES





 print("\nCalibrating delays & rates.")
 pool = Pool(processes = NPROC)
 AVERAGES = pool.map(CALIB_PARALLEL, SCANS)
 pool.close()
 pool.join()

 print("\nFinished!")




## Derive the bandpass (only phase) by averaging all scans
## and solving first for the multi-band delays:
 ALLBAS = np.unique(np.concatenate([sc['ANTS'] for sc in SCANS]))

 BPASS = {}
 NIF = len(ALLIFS[0])
 zPAD = 11
 for BAS in ALLBAS:
  print("Deriving bandpass from %s"%BAS)
  BPASS[BAS] = [] 
  TEMP = [[],[],[],[]]
  for average in AVERAGES:
    if BAS in average['vis'].keys() and len(average['vis'][BAS].keys())>0:
       MBD = np.zeros(NIF*zPAD,dtype=np.complex64)
       RGAIN = np.array([average['vis'][BAS][i][0] for i in range(NIF)]) # ALLIFS])
       LGAIN = np.array([average['vis'][BAS][i][1] for i in range(NIF)]) #ALLIFS])
       BADS_R = np.abs(RGAIN)==0.0 ; BADS_L = np.abs(LGAIN)==0.0
       GOODS_R = np.logical_not(BADS_R); GOODS_L = np.logical_not(BADS_L)
       RGAIN[BADS_R] = 1.0 ; LGAIN[BADS_L] = 1.0
       RNORM = RGAIN/np.abs(RGAIN) 
       LNORM = LGAIN/np.abs(LGAIN)
       RNORM[BADS_R] = 0.0 ; LNORM[BADS_L] = 0.0
       NHF = NIF//2
       MBD[:NHF] = RNORM[-NHF:] 
       MBD[-NHF:] = RNORM[:NHF] 
       FRINGE = np.fft.fftshift(np.abs(np.fft.fft(MBD))); 
       PEAK = np.argmax(FRINGE)
       DELAY_R = (PEAK-len(FRINGE)//2)/(NIF*zPAD)*2.*np.pi
       WGT_R = float(FRINGE[PEAK])**2.
       MBD[:NHF] = LNORM[-NHF:]
       MBD[-NHF:] = LNORM[:NHF]
       FRINGE = np.fft.fftshift(np.abs(np.fft.fft(MBD))); 
       PEAK = np.argmax(FRINGE)
       DELAY_L = (PEAK-len(FRINGE)//2)/(NIF*zPAD)*2.*np.pi
       WGT_L = float(FRINGE[PEAK])**2.
       BP_SCAN_R = RGAIN*np.exp(-1.j*DELAY_R*np.linspace(-NIF/2.+1,NIF/2.,NIF))
       BP_SCAN_L = LGAIN*np.exp(-1.j*DELAY_L*np.linspace(-NIF/2.+1,NIF/2.,NIF))
       R_PHAS = np.average(BP_SCAN_R[GOODS_R]) ; R_PHAS /= np.abs(R_PHAS)
       L_PHAS = np.average(BP_SCAN_L[GOODS_L]) ; L_PHAS /= np.abs(L_PHAS)
       BP_SCAN_R[BADS_R] = 1.0 ; BP_SCAN_L[BADS_L] = 1.0
       TEMP[0].append(BP_SCAN_R/R_PHAS)
       TEMP[1].append(BP_SCAN_L/L_PHAS)
       TEMP[2].append(WGT_R)
       TEMP[3].append(WGT_L)
  BPASS[BAS].append(np.average(TEMP[0],axis=0,weights=TEMP[2]))
  BPASS[BAS].append(np.average(TEMP[1],axis=0,weights=TEMP[3]))
  BPASS[BAS][0] /= np.abs(BPASS[BAS][0])
  BPASS[BAS][1] /= np.abs(BPASS[BAS][1])




## Apply bandpass and recompute+apply multi-band delays:
 print("\nPlotting!")
 fig = pl.figure(figsize=(12,4)); sub = fig.add_subplot(121); sub2 = fig.add_subplot(122)

 for average in AVERAGES:
  scanName = average['scanName']
  for BAS in average['vis'].keys():
      sub.cla() ; sub2.cla()
      VIS2PLOT_RR = np.array([average['vis'][BAS][i][0] for i in range(NIF)])/BPASS[BAS][0]
      VIS2PLOT_LL = np.array([average['vis'][BAS][i][1] for i in range(NIF)])/BPASS[BAS][1]
      VIS2PLOT_RL = np.array([average['vis'][BAS][i][2] for i in range(NIF)])/BPASS[BAS][1]
      VIS2PLOT_LR = np.array([average['vis'][BAS][i][3] for i in range(NIF)])/BPASS[BAS][0]

      sub.plot(np.abs(VIS2PLOT_RR),'ob',label=average['corrProds'][BAS]['RR'])
      sub.plot(np.abs(VIS2PLOT_LL),'or',label=average['corrProds'][BAS]['LL'])
      sub.plot(np.abs(VIS2PLOT_RL),'xg',label=average['corrProds'][BAS]['RL'])
      sub.plot(np.abs(VIS2PLOT_LR),'xk',label=average['corrProds'][BAS]['LR'])
      sub.set_xlabel("IF")
      sub.set_ylabel("Amp")
      sub.legend(ncol=2)
      R2P = 180./np.pi
      #NIF = len(ALLIFS); 
      zPAD = 11 # Larger zero-padding for the MBD
      MBD = np.zeros(NIF*zPAD,dtype=np.complex128)
      RGAIN = np.array(VIS2PLOT_RR)
      LGAIN = np.array(VIS2PLOT_LL)
      NHF = NIF//2
      MBD[:NHF] = RGAIN[-NHF:]
      MBD[-NHF:] = RGAIN[:NHF]
 
      FRINGE = np.fft.fftshift(np.abs(np.fft.fft(MBD))); 
      DELAY = (np.argmax(FRINGE)-len(FRINGE)//2)/(NIF*zPAD)*2.*np.pi
      RGAIN[:] = np.exp(1.j*DELAY*np.linspace(-NIF/2.+1,NIF/2.,NIF))

      MBD[:NHF] = LGAIN[-NHF:]
      MBD[-NHF:] = LGAIN[:NHF]
      FRINGE2 = np.fft.fftshift(np.abs(np.fft.fft(MBD))); 
      DELAY = (np.argmax(FRINGE2)-len(FRINGE)//2)/(NIF*zPAD)*2.*np.pi
      LGAIN[:] = np.exp(1.j*DELAY*np.linspace(-NIF/2.+1,NIF/2.,NIF))

      VIS2PLOT_RR /= RGAIN
      VIS2PLOT_LL /= LGAIN
      VIS2PLOT_RL /= LGAIN
      VIS2PLOT_LR /= RGAIN

      R_PHAS = np.average(VIS2PLOT_RR) ; R_PHAS /= np.abs(R_PHAS)
      L_PHAS = np.average(VIS2PLOT_LL) ; L_PHAS /= np.abs(L_PHAS)


      # Plot
      sub2.plot(R2P*np.angle(VIS2PLOT_RR/R_PHAS),'ob')
      sub2.plot(R2P*np.angle(VIS2PLOT_LL/L_PHAS),'or')
      sub2.plot(R2P*np.angle(VIS2PLOT_RL/L_PHAS),'xg')
      sub2.plot(R2P*np.angle(VIS2PLOT_LR/R_PHAS),'xk')

      sub2.set_xlabel("IF")
      sub2.set_ylabel("Phase (deg)")
      sub2.set_ylim((-180.,180.))
      fig.suptitle(BAS)
      pl.savefig(os.path.join(PLOTDIR,"%s_%s_%s_b%s_CALIB.png"%(BAS,average["source"],scanName,BAND)))

 del MBD,FRINGE,FRINGE2
 SCANS.clear()
 gc.collect() 

tac = time.time()

print("\n Script took %.1f seconds."%(tac-tic))
