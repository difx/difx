import multiprocessing
import os, sys, glob
import pickle as pk
import numpy as np
import re
import struct as stk


######################################
#### READ CONFIGURATION:

CONFIG = open(sys.argv[-1],"rb")
NPROC,TRACK,CASA_CALL,DIFX_DIR,DEST_DIR,BAND,JOBS,QA2_ROOT,addKcrs,useGpol2 = pk.load(CONFIG)
CONFIG.close()



## Add XY phases (in deg.) to these bands:
addXYPhase = {} #{2:130.}

AmpTable = {True:"Gpol2",False:"Gxyamp"}[useGpol2]


########################
#### SCRIPT STARTS: ####
########################


# QA2 prefixes:
QA2 =  os.path.join(QA2_ROOT,TRACK) + '.concatenated.ms'
QA2c = os.path.join(QA2_ROOT,TRACK) + '.calibrated.ms'




######################
### PART 1: METADATA

if BAND!='0':
  allCalc = glob.glob(os.path.join(DIFX_DIR,"*-b%i_*.calc"%int(BAND)))
  if len(allCalc)==0:
   raise Exception("Missing calc files in DIFX_DIR!!")
else:
  allCalc = glob.glob(os.path.join(DIFX_DIR,"*.calc"))


# Copy metadata:
if not os.path.exists(DEST_DIR):
   os.system("mkdir %s"%(DEST_DIR))
for job in JOBS:
   allExtensions =[K.split(".")[-1] for K in glob.glob(os.path.join(DIFX_DIR,"%s.*"%job))]
   for ext in allExtensions: #["calc","difxlog","errs","flag","im","input","machines","threads"]:
      if ext != "difx":
       #  print(ext)
         if os.path.exists(os.path.join(DIFX_DIR,"%s.%s"%(job,ext))):
            os.system("cp %s %s"%(os.path.join(DIFX_DIR,"%s.%s"%(job,ext)),os.path.join(DEST_DIR,"%s.%s"%(job,ext))))

# Absolute destination directory:
ABSWORK = os.path.abspath(DEST_DIR)

# Edit the metadata:
DURs = []
for job in JOBS:
   calc = os.path.join(DEST_DIR,"%s.calc"%job)
   inpu = os.path.join(DEST_DIR,"%s.input"%job)

## Update input:
   temp = open(inpu,'r')
   inpu_lines = temp.readlines()
   temp.close()
   temp = open(inpu,'w')
   for line in inpu_lines:
      if "EXECUTE TIME (SEC)" in line:
         dt = float(line.split()[-1])
      if "FILENAME" in line:
         fnameOrig = os.path.basename(line.split()[-1])
         itemOrig = line.split(":")[0]
         line = '%s%s \n'%((itemOrig+":").ljust(20), os.path.join(ABSWORK,fnameOrig))
      if re.search(r"ZOOM.*POL:\s+X$", line):
         line = re.sub(r"X$","R",line)     
      if re.search(r"ZOOM.*POL:\s+Y$", line):
         line = re.sub(r"Y$","L",line)     
      if re.search(r"REC.*POL:\s+X$", line):
         line = re.sub(r"X$","R",line)     
      if re.search(r"REC.*POL:\s+Y$", line):
         line = re.sub(r"Y$","L",line)     
      temp.write(line)
   temp.close()
   DURs.append(dt)

## Update calc:
   temp = open(calc,'r')
   calc_lines = temp.readlines()
   temp.close()
   temp = open(calc,'w')
   for line in calc_lines:
      if "FILENAME" in line:
         fnameOrig = os.path.basename(line.split()[-1])
         itemOrig = line.split(":")[0]
         line = '%s%s \n'%((itemOrig+":").ljust(20), os.path.join(ABSWORK,fnameOrig))
      temp.write(line)
   temp.close()


###################
## Get the IFs with data:
WORD = b'\x00\xff\x00\xff\x01\x00\x00\x00'
print("Checking SWIN contents")
DATA_IFS = {}
for job in JOBS:

  DATA_IFS[job] = []
  auxSWIN = glob.glob(os.path.join(os.path.join(DIFX_DIR,"%s.difx"%job),"DIFX_*.b*"))
  if len(auxSWIN)==0:
    raise Exception("Missing SWIN data!")
  else:
    print("Checking %s"%auxSWIN[0])
  frfile = open(auxSWIN[0],'rb')

## Figure out number of channels:
  temp = frfile.read(8+4+4+8+4+4+4+2+4+8+8*3)
  for i in range(8192):
    test = frfile.read(8)
    if test==WORD:
      break
  NCHAN = int(i)
  print("There seem to be %i channels in the IFs.\n"%i)
  frfile.close()
  SKIP = 2+4+8*(5+NCHAN)
## Read data:
  frfile = open(auxSWIN[0],"rb")
  alldats = frfile.read(8)
  #i = 0
  while True: #i<1000000: # TODO: Remove i condition
  #  i += 1
  #  if i%1024==0:
  #    sys.stdout.write('\r Checking VIS %i'%i)
  #    sys.stdout.flush()
    alldats = frfile.seek(24,1)
    alldats = frfile.read(4)
    if not alldats: break
    SPI = stk.unpack("i",alldats)[0]
    alldats = frfile.seek(SKIP,1)
    if int(SPI+1) not in DATA_IFS[job]:
      DATA_IFS[job].append(int(SPI+1))
  frfile.close()

  print("\nIFs with data: ",DATA_IFS[job])



# Figure out the ALMA band (by comparing frequencies):

tb.open(os.path.join("%s.Df0.APP"%QA2c,"SPECTRAL_WINDOW"))
ALMA_NUs = tb.getcol("CHAN_FREQ")/1.e9
ALMA_BANDs = [[np.min(ALMA_NUs[:,k]),np.max(ALMA_NUs[:,k])] for k in range(np.shape(ALMA_NUs)[1])]
tb.close()

spw = {}
for job in JOBS:

  inpF = open(os.path.join(DIFX_DIR,"%s.input"%job),"r")
  VLBI_NUs = []
  for line in inpF.readlines():
   if "FREQ (MHZ) " in line:
#      print(line)
      idx = int(line.split(":")[0].split()[-1])
      Nui = float(line.split()[-1])/1.e3
      if int(idx+1) in DATA_IFS[job]:
         VLBI_NUs.append(Nui)
  inpF.close()

  spw[job] = -1
  for k in range(len(ALMA_BANDs)):
    for nui in VLBI_NUs:
       if nui>= ALMA_BANDs[k][0] and nui<= ALMA_BANDs[k][1]:
          spw[job] = int(k)
          break

  if spw[job]<0:
     raise Exception("Problem finding ALMA spw matching VLBI band for job %s"%job)

  else:
     print("Job %s seems to correspond to ALMA spw %i"%(job,spw[job]))

#print("ALMA spw: %i"%spw)





######################
### PART 2: POLCONVERSION

Script = "from PolConvert import polconvert_CASA as PC\nimport sys, os\nimport pickle as pk\n"
Script += 'IFF = open(\'%s\',\'rb\'); kww = pk.load(IFF); IFF.close()\n'
Script += 'PC.polconvert(**kww)'


JOB_SCRIPTS = []
TEMP_DIR = "POLCONVERT_AUXDIR"
TSYS_DIR = "POLCONVERT_AMPGAINS"
if not os.path.exists(TEMP_DIR):
  os.system("mkdir %s"%TEMP_DIR)

if not os.path.exists(TSYS_DIR):
  os.system("mkdir %s"%TSYS_DIR)
os.system("rm -rf %s/*.*"%TSYS_DIR)

for job in JOBS:
## Check whether ALMA was observing:
   calcFile = open(os.path.join(DIFX_DIR,"%s.calc"%job),"r")
   isALMA = False
   for line in calcFile.readlines():
      if "NUM TELESCOPES: " in line and line.split()[-1]=="1":
         isALMA = False
         break
      if "TELESCOPE " in line and " NAME:" in line and line.split()[-1]=="AA":
         isALMA = True
         break
   calcFile.close()

## Only polconvert if there is ALMA:
   if isALMA:
      IDI = os.path.join(DIFX_DIR,"%s.difx"%job)
      OUTPUTIDI = os.path.join(DEST_DIR,"%s.difx"%job)
      DiFXinput = os.path.join(DIFX_DIR,"%s.input"%job)
      DiFXcalc = os.path.join(DIFX_DIR,"%s.calc"%job)
      doIF = sorted(DATA_IFS[job])
      linAntIdx = [1]
      ALMAant = '%s.ANTENNA'%QA2
      calAPP = '%s.calappphase'%QA2
      calAPPTime = [0.0,0.0]
      plotRange = []
      XYadd = {}
      if int(BAND) in addXYPhase.keys():
        XYadd['AA'] = {}
        for key in doIF:
          XYadd['AA'][key] = addXYPhase[int(BAND)]
### GENERATE XY0KCRS WITHIN POLCONVERT, FROM KCRS AND XY0:
      if addKcrs:
        gains = [['%s.bandpassAPP'%QA2,
          '%s.XY0.APP'%QA2c,
          '%s.Kcrs.APP'%QA2c,
          '%s.flux_inf.APP.OpCorr'%QA2,
          '%s.phase_int.APP.XYsmooth'%QA2,
          '%s.%s.APP'%(QA2c,AmpTable)]] 
        calintrp = [['linear', 'linear','linear','nearest', 'linear', 'linear']]
        gtype = [['G','G','G','T','G','G']]
      else:
### OR DO IT THE OLD WAY, USING XY0KCRS:
        gains = [['%s.bandpassAPP'%QA2,
          '%s.XY0kcrs.APP'%QA2c,
          '%s.flux_inf.APP.OpCorr'%QA2,
          '%s.phase_int.APP.XYsmooth'%QA2,
          '%s.%s.APP'%(QA2c,AmpTable)]] 
        calintrp = [['linear', 'linear','nearest', 'linear', 'linear']]
        gtype = [['G','G','T','G','G']]
      dterms = ['%s.Df0gen.APP'%QA2c]
      plotAnt = 2
      kww = {'IDI':IDI, 'OUTPUTIDI':OUTPUTIDI, 'DiFXinput':DiFXinput, 'DiFXcalc':DiFXcalc,
          'doIF':doIF, 'linAntIdx':linAntIdx, 'Range':plotRange, 'ALMAant':ALMAant,
          'spw':spw[job], 'calAPP':calAPP, 'gains':gains, 'interpolation':calintrp,'XYadd':XYadd,
          'gainmode':gtype, 'dterms':dterms, 'plotIF':[], 'plotRange':plotRange, 
          'plotAnt':plotAnt, 'doTest':False, 'npix':512, 'calAPPTime':calAPPTime, 
          'plotSuffix':str(job)+'_'+str(BAND)}

      outF = open(os.path.join(TEMP_DIR,"KWW_%s.dat"%job),"wb")
      pk.dump(kww,outF)
      outF.close()

      outF = open(os.path.join(TEMP_DIR,"%s_pconv.py"%job),"w")
      print(Script%(os.path.join(TEMP_DIR,"KWW_%s.dat"%job)),file=outF)
      outF.close()
      JOB_SCRIPTS.append(os.path.join(TEMP_DIR,"%s_pconv.py"%job))

## Otherwise, just copy the data:
   else:
      os.system("cp -r %s %s"%(os.path.join(DIFX_DIR,"%s.difx"%job),os.path.join(DEST_DIR)))

def DO_PARALLEL(script):
   print("Running %s"%os.path.basename(script))
   job = os.path.basename(script).split("_pconv")[0]
   os.system('rm -f %s.msg %s.error'%(script,script))
   os.system("%s --nologger -c %s >%s.msg 2>%s.error"%(CASA_CALL,script,script,script))
   print("Done with %s"%os.path.basename(script))
   os.system("mv POLCONVERT_%s_%s_STATION_AA.ANTAB %s"%(job,BAND,TSYS_DIR))
   os.system("mv POLCONVERT.GAINS_%s_%s %s"%(job,BAND,TSYS_DIR))
   os.system("mv PolConvert_%s_%s.log %s"%(job,BAND,TEMP_DIR))


 
if True:
   pool = multiprocessing.Pool(processes = NPROC)
   pool.map(DO_PARALLEL, JOB_SCRIPTS)
   pool.close()
   pool.join()


allErrs = glob.glob(os.path.join(TEMP_DIR,"*.error"))
BADS = []
for err in allErrs:
   if os.path.getsize(err)>0:
      BADS.append(os.path.basename(err).split("_")[1])

BADFILE = open("POLCONVERT_ERRORS_BAND%i.txt"%int(BAND),"w")
MSG = "SCANS WITH POLCONVERT ERRORS:\n %s"%(",".join(BADS))
print(MSG,file=BADFILE)
print(MSG)
BADFILE.close()

