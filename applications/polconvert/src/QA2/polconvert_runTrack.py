#!/bin/python3


## Script for batch polconversion using QA2-provided auxiliary scripts.
## I. Marti-Vidal & C. Goddi version 1.1 - May 20, 2026.


import sys,os
import pickle as pk
import tempfile
import glob

helptxt = """
PolConvert runTrack (version 1.1b). I. Marti-Vidal & C. Goddi (2026)

The environment variable $CASABASE has to be set to the CASA path. 
Otherwise, the script will fail.

argument list:

--nproc :: Number of processes allowed to run in parallel. Default is 20.

--origdata :: Path to the SWIN products (i.e., ".difx" directories et al.).

              NOTE: It is assumed that the product names have the structure:
 
              EXPT-N-bI_SCN.*

              where "EXPT" is experiment code, N is a number, I is the band
              and SCN is scan number. Example: e22c20-1-b2_1088.difx

--destdata :: Path where the polconverted SWIN products will be stored.

--qa2dir :: Path to the QA2 deliverables.

--track :: Name of the ALMA track.

--jobs :: List of job IDs to convert. Can be separated by spaces or commas. 
           If this is provided, the arguments --source and --bands are NOT used.

NOTE: IF "--jobs" IS NOT PROVIDED, THE DATA ARE SELECTED FROM:

--bands :: List of band numbers. Can be separated by spaces or commas.

--source :: Name of the source to convert.

--separateXY0Kcrs :: If present, it will use XY0.APP and Kcrs tables separately.
                     If not present (default), it will use the XY0Kcrs from 
                     the QA2 tarball.

--useGpol2 :: If present, will use Gpol2 to interpolate X/Y amplitude ratios.
              If not (default), will use Gxyamp (constant ratios assumed).

--noEHT :: If present, the naming assumptions for the EHT are not used. In this case,
           the "--bands" option will not work (only "--jobs" and "--source" will work).

EXAMPLES:

1.- PolConvert all scans of 3C279 in band 2, from track e22c20.
The SWIN products are in: /data/E22C20/DiFX
The polconverted products will be in: /data/E22C20/DiFX_PC
The QA2 deliverables are in: /data/ALMA/e22c20_QA2

polconvert_runTrack.py --qa2dir /data/ALMA/e22c20_QA2  --track e22c20 --origdata /data/E22C20/DiFX --destdata /data/E22C20/DiFX_PC --source 3C279 --bands 2

2.- PolConvert a list of jobs (e.g., scans 1082, 1090, 1073 in bands 2 and 4), 
    using 2 processes.
The data are in the same location as the example above

polconvert_runTrack.py --qa2dir /data/ALMA/e22c20_QA2 --track e22c20 --origdata /data/E22C20/DiFX --destdata /data/E22C20/DiFX_PC --jobs e22c20-1-b2_1073 e22c20-1-b2_1082 e22c20-1-b2_1090  e22c20-1-b4_1073 e22c20-1-b4_1082 e22c20-1-b4_1090 --nproc 2

"""


print(sys.argv)
print('\n\n')

if len(sys.argv)==1:
   print(helptxt)
   os._exit(0)


allOptions = ['--nproc','--origdata','--destdata','--qa2dir','--track','--jobs','--bands','--source','--separateXY0Kcrs','--useGpol2','--noEHT']
for arg in sys.argv:
    if arg.startswith('-') and arg not in allOptions:
        raise Exception("Unknown argument %s"%arg)



#####################
### Get CASA path:
if 'CASABASE' in os.environ.keys():
   CASABASE = os.environ['CASABASE']
else:
   raise Exception("$CASABASE is not defined!")

CASA_CALL = os.path.join(CASABASE,'bin/casa')
if not os.path.exists(CASA_CALL):
   raise Exception("CASA not found at %s"%CASA_CALL)
else:
   print('Will use CASA from: ',CASA_CALL)
#####################


##################################
## Read command-line arguments:

if '--nproc' in sys.argv:
   NPROC = int(sys.argv[sys.argv.index('--nproc')+1])
else:
   NPROC = 20

DIFX_DIR = ''
if '--origdata' in sys.argv:
   DIFX_DIR = sys.argv[sys.argv.index('--origdata')+1]
   if not os.path.exists(DIFX_DIR):
      raise Exception("%s does NOT exist."%DIFX_DIR)

DEST_DIR = ''
if '--destdata' in sys.argv:
   DEST_DIR = sys.argv[sys.argv.index('--destdata')+1]

QA2_DIR = ''
if '--qa2dir' in sys.argv:
   QA2_DIR = sys.argv[sys.argv.index('--qa2dir')+1]
   if not os.path.exists(QA2_DIR):
      raise Exception("%s does NOT exist."%QA2_DIR)

TRACK = ''
if '--track' in sys.argv:
   TRACK = sys.argv[sys.argv.index('--track')+1]

BANDS = []
if '--bands' in sys.argv:
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
      BANDS += list(band.split(","))

SOURCE = ''
if '--source' in sys.argv:
   SOURCE = sys.argv[sys.argv.index('--source')+1]

JOBS = []
if '--jobs' in sys.argv:
   i0 = sys.argv.index('--jobs')
   i1 = -1
   for k in range(i0+1,len(sys.argv)):
      if sys.argv[k].startswith('-'):
         i1 = k
         break
   if i1<0:
      JOBSTR = sys.argv[i0+1:]
   else:
      JOBSTR = sys.argv[i0+1:i1]
   for job in JOBSTR:
      JOBS += list(job.split(","))

# Check if job data exist:
   for job in JOBS:
     if not os.path.exists(os.path.join(DIFX_DIR,job+'.difx')):
       raise Exception("Job %s not found at %s"%(job,DIFX_DIR))

# Finetune QA2:
addKcrs = "--separateXY0Kcrs" in sys.argv
GPol2 = "--useGpol2" in sys.argv

noEHT = "--noEHT" in sys.argv


##################################


##################################
##### ARGUMENT LOGIC:

# If there is a job list,
# get bands from it:
if len(JOBS)>0:
  if noEHT: #The EHT naming convention is not used:
    BANDS = ['0']

  else: # EHT naming convention used:

    JTRACK = JOBS[0].split('-')[0]
    if JTRACK != TRACK:
      print("\n\nWARNING! There seems to be an inconsistency between TRACK and the JOB IDs!!\n\n")
    BANDS = []
    for job in JOBS:
      AUXBAND = job.split('-')[-1]
      if AUXBAND.startswith('b'): 
         if AUXBAND[1] not in BANDS:
            BANDS.append(AUXBAND[1])
      else:
         raise Exception("Error parsing Band from the job ids")
 

# Otherwise, use the arguments:
else:
   if len(SOURCE)==0 and len(BANDS)==0 and not noEHT:
      raise Exception("Neither source name, nor band(s), nor job list are defined!")

# First, get all JOB IDs. Then, we'll filter them out:
   INI_JOBS = [".".join(os.path.basename(DD).split(".")[:-1]) for DD in glob.glob(os.path.join(DIFX_DIR,"*.difx"))]

# Filter by band:
   if noEHT:
## All scans are selected by default for noEHT:
     BANDS = ['0']
     BAND_JOBS = INI_JOBS
   elif len(BANDS)>0:
      BAND_JOBS = []
      for job in INI_JOBS:
         AUXBAND = job.split("-")[-1][1]
         if AUXBAND in BANDS:
            BAND_JOBS.append(job)
   else:
      BAND_JOBS = INI_JOBS
      for job in BAND_JOBS:
         AUXBAND = job.split('-')[-1]
         if AUXBAND.startswith('b'):
            if AUXBAND[1] not in BANDS:
               BANDS.append(AUXBAND[1])
         else:
            raise Exception("Error parsing Band from the job ids")

# Filter by source:
   if len(SOURCE)>0:
      JOBS = []
      for job in BAND_JOBS:
         temp = open(os.path.join(DIFX_DIR,job+".calc"),"r")
         for line in temp.readlines():
            if line.startswith("SOURCE 0 NAME:"):
               tempSou = line.split()[-1]
               if SOURCE in tempSou:
                  JOBS.append(job)
                  break
         temp.close()
   else:
      JOBS = BAND_JOBS

# Classify jobs by band:
JOBS_BY_BAND = {}
if noEHT:
  JOBS_BY_BAND['0'] = JOBS
else:

  for band in BANDS:
     JOBS_BY_BAND[band] = []

  for job in JOBS:
    AUXBAND = job.split("-")[-1][1]
    JOBS_BY_BAND[AUXBAND].append(job)

  
## RUN POLCONVERT (BAND BY BAND):
if True:
 for B in BANDS:
   SUFFIX = "_PolConvert_%s"%(B)
   CID,CNAME = tempfile.mkstemp() #open("PolConvert_configuration.dat","wb")
   CONFIG = open(CNAME,"wb")
   pk.dump([NPROC,TRACK,CASA_CALL,DIFX_DIR,DEST_DIR,B,JOBS_BY_BAND[B],QA2_DIR,addKcrs,GPol2],CONFIG)
   CONFIG.close()
   os.system("%s --nologger -c %s_polconvert_apply.py %s"%(CASA_CALL,os.path.join(QA2_DIR,TRACK),CNAME))
   os.remove(CNAME)








