#!/usr/bin/python
#
# Script to make a check of data readability from Ivan, hacked by Geoff.
#
from __future__ import absolute_import
from __future__ import print_function
import numpy as np
import pylab as pl
import os, sys, glob
import struct
import argparse

##################
# FOR TESTING:
#if __name__=='__main__':
# DIFX_DIR = 'DATA'
# EXPNAME  = 'e17e11'
# SNRCut = 10.0
# DOIF = []
##################

def parseOptions():
    '''
    Reads all the scans in a SWIN directory and creates an ASCII file
    with information about the sources, participating antennas, and SNRs 
    of the correlation products. If the minimum SNR is equal or higher
    than the SNRCut, the scan is marked as good. 
    If "DOIF" is an empty list, all IFs are analyzed.
    '''
    des = parseOptions.__doc__
    epi = ''
    use = '%(prog)s [options] arguments\n  Version'
    use += '$Id$'
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    primary = parser.add_argument_group('Primary Options')
    secondy = parser.add_argument_group('Secondary Options')
    # primary args
    primary.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    primary.add_argument('-e', '--expn', dest='expn',
        default='', metavar='STRING',
        help='name of the experiment')
    primary.add_argument('-d', '--dir', dest='dir',
        default='', metavar='DIRECTORY',
        help='SWIN output dir name')
    primary.add_argument('-s', '--snrcut', dest='snrcut',
        default=10.0, type=float, metavar='FLOAT',
        help='SNR Cutoff for marking scan as good')
    primary.add_argument('-i', '--ifs', dest='ifs',
        default='', metavar='LIST',
        help='comma-sep list of IFs to examine')
    # secondary args
    secondy.add_argument('-a', '--ants', dest='ants',
        default='', metavar='LIST',
        help='comma-sep list of Antennas to exclude (0-based)')
    parser.add_argument('nargs', nargs='*',
        help='(there should be no non-option arguments)')
    return parser.parse_args()

#######################################
# COMMENT THESE LINES OUT WHEN DEBUGGING AS execfile(...)
def SCAN_SCANNER(EXPNAME='', DIFX_DIR='',SNRCut = 10.0, DOIF = [],
    VERB=False, EXCANTS=[]):
 """ Reads all the scans in a SWIN directory and creates an ASCII file
  with information about the sources, participating antennas, and SNRs 
  of the correlation products. If the minimum SNR is equal or higher than 
  SNRCut, the scan is marked as good. 
  If "DOIF" is an empty list, all IFs are analyzed.""" 
########################################
 files = list()

# try:
 if True:

  if VERB: print('  Exp, dir, snr, doif', EXPNAME, DIFX_DIR, SNRCut, DOIF)
  EXP = EXPNAME; DIRE = DIFX_DIR

# Figure out number of antennas, IFs and channels per IF:
  inps = [f[:-1] for f in os.popen('find ./%s -name \"*.input\" -print'%(DIRE))]
  Nants = 0; NIF = 0; Nchan={}
  Adict = {}

  for i,inp in enumerate(inps):
    if VERB: print('  Working ',i,inp)
    temp = open(inp)
    lines = temp.readlines()
    for lii,li in enumerate(lines):
        if li.startswith('TELESCOPE ENTRIES:'):
          Nants = max([Nants,int(li.split()[-1])])
        if i==0 and li.startswith('FREQ ENTRIES:'):
            NIF = int(li.split()[-1])
        if i==0 and li.startswith('NUM CHANNELS'):
          whichIF = int(li.split()[2][:-1])
          if lines[lii+1].startswith('CHANS TO AVG'):
            Nchan[whichIF] = int(float(li.split()[-1])/float(lines[lii+1].split()[-1]))
          else:
            Nchan[whichIF] = int(li.split()[-1])
        if i==0 and li.startswith('TELESCOPE NAME'):
          parts = [x.split() for x in li.split(':')]
          Adict[int(parts[0][2])] = parts[1][0]

    if EXP == '':
      try: EXP = os.path.basename(inp.split('_')[0])
      except: pass
    temp.close()


  print('There are %i IFs with a maximum of %i channels each.'%(NIF,max(Nchan.values())))

  if len(DOIF)==0:
    DOIF = range(NIF)
  if VERB: print('  DOIF is', DOIF)
  if VERB: print('  antenna map',Adict)
  if VERB: print('  EXP is', EXP)

# ID list of baselines:
  SNRBas = []
  for i in range(Nants-1):
    if i in EXCANTS: continue
    for j in range(i+1,Nants):
      if j in EXCANTS: continue
      SNRBas.append([i,j])


# Read visibilities for SNR estimate:
  files.append('SOURCES_%s.txt'%EXP)
  OUTPUT = open('SOURCES_%s.txt'%EXP,'w')
  calcs = [f[:-1] for f in os.popen('find ./%s -name \"*.calc\" -print'%(DIRE))]

  Naex = len(EXCANTS)
  fmt = ("  ANT %i(%s): X = [%6.2f  %6.2f] | Y = [%6.2f  %6.2f]  %s  \n"
    * (Nants-Naex))
  fmt += "  SNR PASS: %s\n\n"


## Iterate over SWIN files (i.e., scans):
  for dd in sorted(calcs):

    print('\n\nDOING %s:\n\n'%dd)

################
## Data and metadata will be arranged by IF:
    BAS = {}; MJD = {}; SEC={};
    POL1 ={}; POL2={};
    VISIB={}

# List to save all IFs with no data:
    NODATA = []

    for i in DOIF:
      BAS[i] = []
      MJD[i] = []
      SEC[i] = []
      POL1[i] = []
      POL2[i] = []
      VISIB[i] = []
################




# Read visibilities and metadata:
    DIFXFile = glob.glob('%s.difx/DIFX*'%dd[:-5])[0]
    if VERB: print('  Reading', DIFXFile)
    frfile = open(DIFXFile,"rb")

    while True:
       buff = frfile.read(74)
       if not buff:
          break
 
       var = struct.unpack("iiiidiiibb",buff[:38])
       IF = var[7]
       if IF in DOIF:
         BAS[IF].append(var[2])
         MJD[IF].append(var[3])
         SEC[IF].append(var[4])

         POL1[IF].append(chr(var[8]))
         POL2[IF].append(chr(var[9]))
         buff = frfile.read((Nchan[IF])*8)
         VREIM = np.array(struct.unpack("f"*(2*(Nchan[IF])),buff))
         VISIB[IF].append(VREIM[:-1:2]+1.j*VREIM[1::2])
       else:
         buff = frfile.read((Nchan[IF])*8)
        


## Create numpy arrays for all quantities (per IF):
    for i in DOIF:
      BAS[i] = np.array(BAS[i],dtype=np.int32)
      MJD[i] = np.array(MJD[i],dtype=np.int32)
      SEC[i] = np.array(SEC[i],dtype=np.float64)
      POL1[i] = np.array(POL1[i])
      POL2[i] = np.array(POL2[i])
      VISIB[i] = np.array(VISIB[i],dtype=np.complex64)


## Dummy initial values for min/max SNRs per antenna:
    SNR_X = [[1.e18,0.] for i in range(Nants)]  
    SNR_Y = [[1.e18,0.] for i in range(Nants)]  

## List to save the total number of visibs in each corr. product:
    NVIS = [0, 0, 0, 0]


## Analyze each baseline:
    for bb in SNRBas:

      if VERB: print('  Analyzing baseline', bb)

      BSel = (bb[0]+1)*256 + (bb[1]+1)
      X1m = 1.e18 ; X2m = 1.e18
      Y1m = 1.e18 ; Y2m = 1.e18
    
      X1M = 0. ; X2M = 0.
      Y1M = 0. ; Y2M = 0.

      for IFp in DOIF:

## Select only data for the current baseline:
        MASK = BAS[IFp]==BSel

        if np.sum(MASK)>0:
          P1 = POL1[IFp][MASK]
          P2 = POL2[IFp][MASK]
          XXp = np.logical_and(np.logical_or(P1=='R',P1=='X'),np.logical_or(P2=='R',P2=='X'))
          XYp = np.logical_and(np.logical_or(P1=='R',P1=='X'),np.logical_or(P2=='L',P2=='Y'))
          YXp = np.logical_and(np.logical_or(P1=='L',P1=='Y'),np.logical_or(P2=='R',P2=='X'))
          YYp = np.logical_and(np.logical_or(P1=='L',P1=='Y'),np.logical_or(P2=='L',P2=='Y'))

          NVIS[0] += np.sum(XXp); NVIS[1] += np.sum(XYp); NVIS[2] += np.sum(YXp); NVIS[3] += np.sum(YYp); 

## FFT to delay-rate space:
          XXmatrix = np.fft.fftshift(np.abs(np.fft.fft2(VISIB[IFp][MASK,:][XXp,:-1])))
          XYmatrix = np.fft.fftshift(np.abs(np.fft.fft2(VISIB[IFp][MASK,:][XYp,:-1])))
          YXmatrix = np.fft.fftshift(np.abs(np.fft.fft2(VISIB[IFp][MASK,:][YXp,:-1])))
          YYmatrix = np.fft.fftshift(np.abs(np.fft.fft2(VISIB[IFp][MASK,:][YYp,:-1])))

## Find fringe peaks:
          Peak = np.unravel_index(np.argmax(XXmatrix),np.shape(XXmatrix))
          SNR_XX = XXmatrix[Peak[0],Peak[1]]
          XXmatrix[Peak[0]-1:Peak[0]+1,Peak[1]-1:Peak[1]+1] = 0.0; 

          Peak = np.unravel_index(np.argmax(XYmatrix),np.shape(XYmatrix))
          SNR_XY = XYmatrix[Peak[0],Peak[1]]
          XYmatrix[Peak[0]-1:Peak[0]+1,Peak[1]-1:Peak[1]+1] = 0.0; 

          Peak = np.unravel_index(np.argmax(YXmatrix),np.shape(YXmatrix))
          SNR_YX = YXmatrix[Peak[0],Peak[1]]
          YXmatrix[Peak[0]-1:Peak[0]+1,Peak[1]-1:Peak[1]+1] = 0.0; 

          Peak = np.unravel_index(np.argmax(YYmatrix),np.shape(YYmatrix))
          SNR_YY = YYmatrix[Peak[0],Peak[1]]
          YYmatrix[Peak[0]-1:Peak[0]+1,Peak[1]-1:Peak[1]+1] = 0.0; 

# Compute the std:
          SNR_XX /= np.sqrt(np.var(XXmatrix) + np.average(XXmatrix)**2.)
          SNR_XY /= np.sqrt(np.var(XYmatrix) + np.average(XYmatrix)**2.)
          SNR_YX /= np.sqrt(np.var(YXmatrix) + np.average(YXmatrix)**2.)
          SNR_YY /= np.sqrt(np.var(YYmatrix) + np.average(YYmatrix)**2.)

## Find extreme SNR values (min and max):
          X1m = np.min([SNR_XX, SNR_XY, X1m]) 
          X2m = np.min([SNR_XX, SNR_YX, X2m]) 
          Y1m = np.min([SNR_YY, SNR_YX, Y1m]) 
          Y2m = np.min([SNR_YY, SNR_XY, Y2m]) 

          X1M = np.max([SNR_XX, SNR_XY, X1M]) 
          X2M = np.max([SNR_XX, SNR_YX, X2M]) 
          Y1M = np.max([SNR_YY, SNR_YX, Y1M]) 
          Y2M = np.max([SNR_YY, SNR_XY, Y2M]) 

          SNR_X[bb[0]][0] = np.min([SNR_X[bb[0]][0],X1m])
         #SNR_X[bb[0]][1] = np.max([SNR_X[bb[1]][1],X1M])
          SNR_X[bb[0]][1] = np.max([SNR_X[bb[0]][1],X1M])
          SNR_X[bb[1]][0] = np.min([SNR_X[bb[1]][0],X2m])
          SNR_X[bb[1]][1] = np.max([SNR_X[bb[1]][1],X2M])

          SNR_Y[bb[0]][0] = np.min([SNR_Y[bb[0]][0],Y1m])
         #SNR_Y[bb[0]][1] = np.max([SNR_Y[bb[1]][1],Y1M])
          SNR_Y[bb[0]][1] = np.max([SNR_Y[bb[0]][1],Y1M])
          SNR_Y[bb[1]][0] = np.min([SNR_Y[bb[1]][0],Y2m])
          SNR_Y[bb[1]][1] = np.max([SNR_Y[bb[1]][1],Y2M])

## Free some memory:
          del P1, P2, XXp, YYp, XYp, YXp, XXmatrix, YYmatrix, XYmatrix, YXmatrix


### If no data are found:
        else:
          NODATA.append(IFp)
          if VERB: print("Problematic baseline %i-%i, IF %i"%(bb[0],bb[1],IFp))
   #       print "Problem with baseline %i-%i, IF %i"%(bb[0],bb[1],IFp)
          


## Free more memory:
        del MASK

    for i in DOIF:
      del BAS[i] 
      del MJD[i] 
      del SEC[i] 
      del POL1[i] 
      del POL2[i] 
      del VISIB[i] 


## Print number of visibilities in each correlation product:
    print('\nNUMBER OF VISIBS IN POL. MATRIX: \n\n   | %6i %6i |\n   | %6i %6i |\n'%tuple(NVIS)) 
    if len(NODATA)>0:
      NOIFS = np.unique(NODATA)
      print('IFs with some (or all) missing baseline(s):\n')
      print(('  '+'%i '*len(NOIFS))%tuple(NOIFS))

## Save extreme SNRs in outfile:
    IFF = open(dd)
    lines = IFF.readlines()
    IFF.close()
    for li, line in enumerate(lines):
      if line.startswith('NUM SOURCES:'):
        lsou = li
        Nsou = int(line.split()[-1])
        break
  
    for j in range(Nsou):
      Snam = lines[lsou+1+j*5].split()[-1]
      print('%s:   %s'%(os.path.basename(dd).split('.')[0], Snam), file=OUTPUT)
    
    if VERB: print(
      '# ANT N(??): X = [%6s  %6s] | Y = [%6s  %6s]  Observed'%(
        'min','max','min','max'), file=OUTPUT)
    IsGood = True    
    SNROut = []  
    for j in range(Nants):
      if j in EXCANTS: continue
      Observes = '+'
      if SNR_X[j][1]<SNRCut or SNR_Y[j][1]<SNRCut:
          IsGood = False
          if (SNR_X[j][1]==0.00 and SNR_Y[j][1]==0.0): Observes = '-'
      SNROut += [j, Adict[j],
        SNR_X[j][0],SNR_X[j][1], SNR_Y[j][0],SNR_Y[j][1], Observes]
    SNROut += [{True:'Y',False:'N'}[IsGood]]
  
    print(fmt%tuple(SNROut), file=OUTPUT)
    
  OUTPUT.close()  



# if os.path.exists('SOURCE_SCANNER.FAILED'):   
#   os.system('rm -rf SOURCE_SCANNER.FAILED')   
#   return list()

  return files

# except:

#  e = sys.exc_info()[0]  
#  OFF = open('SOURCE_SCANNER.FAILED','w')
#  print >> OFF, e
#  OFF.close()

def convertIFlist(o):
    '''
    Parse input index list into a DOIF list
    '''
    if o.verb: print("  Parsing '%s'"%o.ifs)
    o.doif = list()
    if len(o.ifs) > 0:
        parts = o.ifs.split(',')
        for pp in parts:
            if '-' in pp:
                this,that = pp.split('-')
                [o.doif.append(pi) for pi in range(int(this),int(that)+1)]
            else:
                o.doif.append(int(pp))
        o.doif.sort()
    if o.verb: print('  Working with IFs ', o.doif)

def convertEXCANTlist(o):
    '''
    Convert excluded antenna list into list
    '''
    if o.verb: print("  Parsing '%s'"%o.ants)
    if len(o.ants) == 0: o.excants = list()
    else: o.excants = list(map(int, o.ants.split(',')))
    if o.verb: print("  Excluding ",o.excants)

#
# enter here to do the work
#
if __name__ == '__main__':
    argv = ' '.join(sys.argv)
    opts = parseOptions()
    opts.argv = argv
    convertIFlist(opts)
    convertEXCANTlist(opts)
    files = SCAN_SCANNER(
        EXPNAME = opts.expn, DIFX_DIR = opts.dir, SNRCut = opts.snrcut,
        DOIF = opts.doif, VERB = opts.verb,
        EXCANTS = opts.excants)
    print('Look at',files,'\n')

#
# eof
#
