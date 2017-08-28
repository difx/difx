# ALMA Phasing Project (APP) QA2 Data Reduction Script
# Version 1.9 - I. Marti-Vidal (August 16, 2017)



########################################
# HEADER KEYWORDS TO RUN THE SCRIPT
#
#
scriptForImaging = 'scriptForImagingAPP_C4.py'  # Must be in this directory.
QA2script = 'APP_QA2script_template_C4.py'
APP_Readme = 'README_APP_C4.txt'
PROJECTS = 'PROJECTS.dict'

# FORCE THIS CASA VERSION(S):
CASAVER = ['4.7.0','4.7.1','4.7.2']

asdmdir='/c3se/users/mivan/Hebbe/QA2/EHT-GMVA/2016.1.01216.V_GMVA'

ASDMs = ['uid___A002_Xbeb207_X79e',
         'uid___A002_Xbeb207_X91f',
         'uid___A002_Xbeb207_Xa04',
         'uid___A002_Xbeb207_Xb4c',
         'uid___A002_Xbeb207_Xc91',
         'uid___A002_Xbeb207_Xe03']


#########################

# Unique identification label for the products:

# SESSION 1:
UID = '2016.1.01216.V'

# Plot tables?
doPlot = True

# Show GUI in plotcal?
showgui = False

################################################
#########################################
# KEYWORDS TO FINE-TUNE THE CALIBRATION

# Whether to use System temperatures:
Use_Tsys = False

# Whether to use APP scans to calibrate X/Y amplitude 
# for pol. calibrator:
Use_APP_for_Gpol = True

# USe the Pol. calibrator model derived from the APP
# observations (instead of the ALMA-mode observations):
Use_PolModel_from_APP = True

# Do not calibrate the polarization on the ALMA-mode scans:
# WARNING: If this is set to True, then 
# "Use_APP_for_Gpol" HAS TO BE SET TO True AS WELL!!
Avoid_ALMA_PolCal = True

# Force the Stokes parameters of the pol. calibrator:
PolStokes = [] # Empty list means to estimate the Stokes parameters.
               # [I, Q, U, V] means to FORCE the Stokes parameters.
               # Example: [1.0, 0.01, 0.01, 0.0]

# Take into account possible RM of the pol. calibrator,
# when estimating the Dterms:
Calib_RM = True

# Highest allowed inter-antenna Gpol dispersion:
BadGpol = 1.0 

# Flag Gpol based on time span between solutions:
GpolTmax = 60. # seconds
GpolNFlag = 1

# The four science spws for each ASDM. An empty list will make
# the script find them automatically:
SPWs = [] #'17,19,21,23',
       # '17,19,21,23',
       # '9,11,13,15',
       # '9,11,13,15']

# Name of "antenna" with APP signal
# (to be flagged in the calibration)
APPAnt = "DV03"


# Reference antenna (set it to '' for using the same as APP TelCal):
REFANT = ""


#############################
# Absolute flux calibrator (Name or id). Must be a string:
FluxCal = "Callisto"
# Is the flux calibrator a Solar System Object?
IsPlanet = True
# If a Solar-System Object is used:
JyStandard = 'Butler-JPL-Horizons 2012'


# If a quasar is used, instead, 
# set the flux density and spectral index, 
# as taken from the getALMAflux() function:

# From the flux estimate in track B (which used Ganymede): 
QuasarFlux = 8.73
QuasarSpix = -0.6 
QuasarRefFreq = '221.0GHz'
#############################

# All these FIELD IDs must be strings. Empty
# strings ask the script to find them automatically:
BandPassCal = '0' # Field Id of the BP calibrator
PolCal = '0' # Field id of the Polarization calibrator
GainCal = '2' # Field id of the gain calibrator
VLBICal = [] # Field ids of all the other calibrators
             # It must be a list of strings.
Target = '3' # Field id of main target.

# Do we try to concatenate the amplitude gains, or bootstrap the fluxes from ALMA to APP??
BootStrapVLBIFlux = False # False means to (try to) concatenate
VLBIfluxCal = '0'  # If "BootstrapVLBIflux" is True and the flux calibrator 
                   # was not used in VLBI, set this to one of the VLBICal sources
                   # Default means to take the polarization calibrator as bootstrap fluxcal

#######################################
#####################
# CHANNELS THAT JUMP 180 DEGREES IN X-Y PHASE, FOR EACH SPW,
# AT THE TELCAL'S REFERENCE ANTENNA:

# Stop the script when showing the XY-phase plots, to allow user 
# to find you the 180 deg. jumps. This should be set to TRUE 
# on the first time that this script is run:
checkJumps = False

# Write here the list of "jumping channels" for each spw:

# APP CROSS-PHASE JUMPS:
XYJUMPS_APP = [[], #31,61,122,153,213], # SPW0
               [], # SPW1 
               [], # SPW2
               []] # SPW3

# ALMA-MODE CROSS-PHASE JUMPS:
XYJUMPS_ALMA = [[], # SPW0
                [], # SPW1 
                [], # SPW2
                []] # SPW3

#####################
#######################################




#
# END OF HEADER KEYWORDS
######################################################






# FOR VLBI. FUNCTION TO CREATE A NEW LOG:
def writeLog(msg):
  fi = open(UID+'.VLBI.LOG',mode='a')
  print >> fi,'%s - %s'%(str(dt.datetime.now())[:-4],msg)
  casalog.post(msg, 'INFO')
  print msg
  fi.close()





# Calibration

thesteps = []
step_title = {0: ' Import of the ASDMs',
              1: ' Fix of SYSCAL table times',
              2: ' Listobs, get Tsys, and split ALMA-calibration scans (for ordinary QA2)',
              3: ' A priori flagging (autocorrs and phased-signal antenna)',
              4: ' Apply Tsys, split out science SPWs, concatenate, listobs, and build CALAPP table',
              5: ' Save original flags',
              6: ' Initial flagging',
              7: ' Putting a model for the flux calibrator(s)',
              8: ' Save flags before bandpass cal',
              9: ' Bandpass calibration',
              10: ' Save flags before gain cal',
              11: ' Gain calibration',
              12: ' Apply ordinary calibration',
              13: ' Split calibrated data',
              14: ' Save flags before polarization calibration',
              15: ' Polarization calibration',
              16: ' Save flags before applycal',
              17: ' Apply calibration and split corrected column',
              18: ' Save flags after applycal',
              19: ' Run the imaging script on ALL sources',
              20: ' Tar up APP deliverables and make QA2 package'}

if not Use_Tsys:
  step_title[2] = ' Listobs and split ALMA-calibration scans (for ordinary QA2)'
  step_title[4] = ' Split out science SPWs, concatenate, and build CALAPP table'

T = True
F = False

if 'applyonly' not in globals(): applyonly = False
try:
  print 'List of steps to be executed ...', mysteps
  thesteps = mysteps
except:
  print 'global variable mysteps not set.'
if (thesteps==[]):
  thesteps = list(step_title.keys())   #range(0,len(step_title))
  print 'Executing all steps: ', thesteps

# The Python variable 'mysteps' will control which steps
# are executed when you start the script using
#   execfile('scriptForCalibration.py')
# e.g. setting
#   mysteps = [2,3,4]# before starting the script will make the script execute
# only steps 2, 3, and 4
# Setting mysteps = [] will make it execute all steps.
import datetime as dt
import re
import os
import shutil
import numpy as np
import pylab as pl

if applyonly != True: es = aU.stuffForScienceDataReduction() 


isVer=False
for ver in CASAVER:
  if ver in casadef.casa_version:
    isVer=True
    break

if not isVer: 
 sys.exit('ERROR: ONLY CASA VERSIONS   %s   ARE SUPPORTED!'%(', '.join(CASAVER)))




# Import of the ASDMs
mystep = 0
if(mystep in thesteps):
  msg = 'Step '+str(mystep)+' '+step_title[mystep]
  writeLog(msg)

#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]

  for i,asd in enumerate(ASDMs):
    print 'Working out ASDM #%i - %s'%(i+1,asd)
    os.system('rm -rf %s.ms' % asd)
    os.system('rm -rf %s.ms.flagversions' % asd)
    importasdm(asdm=os.path.join(asdmdir, asd), vis=asd+'.ms', 
      asis='Antenna Station Receiver Source CalAtmosphere CorrelatorMode SBSummary CalAppPhase')

    if applyonly != True: es.fixForCSV2555('%s.ms'%asd)



# Fix of SYSCAL table times
mystep = 1
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  from recipes.almahelpers import fixsyscaltimes
  for i,asd in enumerate(ASDMs):
    print 'Working out %s.ms'%asd
    fixsyscaltimes(vis = '%s.ms'%asd)




# listobs, get Tsys, and split ALMA-cal scans
mystep = 2
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  from recipes.almahelpers import tsysspwmap

  os.system('rm -rf %s.ALMA_CALIB_DATA'%UID)
  os.system('mkdir %s.ALMA_CALIB_DATA'%UID)

  os.system('rm -rf %s.APP_CALIB_DATA'%UID)
  os.system('mkdir %s.APP_CALIB_DATA'%UID)

  tsysmap = []

  for i,asd in enumerate(ASDMs):
    os.system('rm -rf %s.ms.listobs'%asd)
    listobs(vis = '%s.ms'%asd,
      listfile = '%s.ms.listobs'%asd)

    if Use_Tsys:
      os.system('rm -rf %s.ms.tsys'%asd) 
      gencal(vis = '%s.ms'%asd,
        caltable = '%s.ms.tsys'%asd,
        caltype = 'tsys')
      tsysmap.append(tsysspwmap(vis = '%s.ms'%asd, tsystable = '%s.ms.tsys'%asd, tsysChanTol = 1))

    tb.open('%s.ms/STATE'%asd)
    modes = tb.getcol('OBS_MODE')


##########################
# SPECIFIC FOR VLBI
    ALMAMode = [i for i in range(len(modes)) 
      if 'APPPHASE_ACTIVE' not in modes[i]]

    APPMode = [i for i in range(len(modes)) 
      if 'APPPHASE_ACTIVE' in modes[i]]

    tb.close()
    tb.open('%s.ms'%asd)
    states = tb.getcol('STATE_ID')
    scans = tb.getcol('SCAN_NUMBER')
    tb.close()


    vset = set()
    for w in ALMAMode: 
      vset = vset.union(set(scans[np.where(states == w)]))
    ALMAscans = ','.join(map(str,list(vset)))

    dirname = '%s.ALMA_CALIB_DATA/%s'%(UID,asd.split('_')[-1])
    os.system('mkdir %s'%dirname)
    split(vis='%s.ms'%asd, outputvis = '%s/%s.ms'%(dirname,asd),
      datacolumn = 'data', scan = ALMAscans)

    vset = set()
    for w in APPMode: 
      vset = vset.union(set(scans[np.where(states == w)]))
    APPscans = ','.join(map(str,list(vset)))

    dirname = '%s.APP_CALIB_DATA/%s'%(UID,asd.split('_')[-1])
    os.system('mkdir %s'%dirname)
    split(vis='%s.ms'%asd, outputvis = '%s/%s.ms'%(dirname,asd),
      datacolumn = 'data', scan = APPscans)
##########################






# A priori flagging
mystep = 3
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  for i,asd in enumerate(ASDMs):

    flagdata(vis = '%s.ms'%asd,
      mode = 'manual',
      spw = '',
      autocorr = T,
      flagbackup = F)
  
    flagdata(vis = '%s.ms'%asd,
      mode = 'manual',
      intent = '*POINTING*,*SIDEBAND_RATIO*,*ATMOSPHERE*',
      flagbackup = F)
  
  
    flagcmd(vis = '%s.ms'%asd,
      inpmode = 'table',
      useapplied = True,
      action = 'apply')
  




# Apply Tsys, split out science SPWs, concatenate, and build CALAPP table
mystep = 4
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  if len(SPWs)==0:
    print '\n\nWill try to find science SPWs automatically\n'
    for i,asd in enumerate(ASDMs):
      tb.open('%s.ms/SPECTRAL_WINDOW'%asd)
      spnames = tb.getcol('NAME')
      tb.close()
      tb.open('%s.ms/DATA_DESCRIPTION'%asd)
      poltypes = tb.getcol('POLARIZATION_ID')
      tb.close()
      tb.open('%s.ms/POLARIZATION'%asd)
      fpcol = np.where(tb.getcol('NUM_CORR')==4)[0]
      tb.close()
      SPWs.append(','.join([str(i) for i in range(len(poltypes)) if poltypes[i]==fpcol and 'FULL_RES' in spnames[i]]))
    print '\n\n Found SPWs: ',SPWs 

 #   raw_input("HOLD")

# Apply Tsys:
  if Use_Tsys:
    for i,asd in enumerate(ASDMs):
      applycal(vis = '%s.ms'%asd,
        field = '',
        spw = SPWs[i],
        gaintable = ['%s.ms.tsys'%asd],
        interp = 'linear,linear',
        spwmap = [tsysmap[i]],
        calwt = T,
        flagbackup = F)
    splitCol = 'corrected'
  else:
    splitCol = 'data'




##########################
# SPECIFIC FOR VLBI
################
# Prepare CALAPP Table:

  if os.path.exists('./%s.calappphase'%UID):
    os.system('rm -rf ./%s.calappphase ./%s.concatenated.ms.calappphase'%(UID,UID))


  for i,asd in enumerate(ASDMs):
    os.system('rm -rf %s.ms.split'%asd) 
    os.system('rm -rf %s.ms.split.flagversions'%asd) 
    print 'Splitting column %s of %s (spw: %s)'%(splitCol,asd,SPWs[i])
    split(vis = '%s.ms'%asd,
      outputvis = '%s.ms.split'%asd,
      datacolumn = splitCol,
      spw = SPWs[i],
      keepflags = T)

    os.system('rm -rf %s.ms.split.listobs'%asd) 
    listobs(vis = '%s.ms.split'%asd,
      listfile = '%s.ms.split.listobs'%asd)


    # Collecting data from CALAPPPHASE into one single table
    if i==0:
      os.system('cp -rf %s.ms.split/ASDM_CALAPPPHASE ./%s.calappphase'%(asd,UID))
    else:
      tb.open(asd+'.ms.split/ASDM_CALAPPPHASE')
      tb.copyrows('./%s.calappphase'%UID)
      tb.close()

  if os.path.exists('%s.concatenated.ms'%UID):
    os.system('rm -rf %s.concatenated.ms*'%UID)

  concat(vis=['%s.ms.split'%asd for asd in ASDMs],
         concatvis='%s.concatenated.ms'%UID,
         timesort = True,
         copypointing = False)

  os.rename('./%s.calappphase'%UID, '%s.concatenated.ms.calappphase'%UID)

################

  listobs(vis = '%s.concatenated.ms'%UID,
    listfile = '%s.concatenated.ms.listobs'%UID)




################
# Build Tsys table for PolConvert:

  if Use_Tsys:

# First, change spw ids to the right (i.e., post-split) values:
    for i,asd in enumerate(ASDMs):
      tb.open('%s.ms.tsys'%asd,nomodify=False)
      spwi = tb.getcol('SPECTRAL_WINDOW_ID')
      for j, spi in enumerate(SPWs[i].split(',')):
        spwi[spwi==tsysmap[i][int(spi)]] = j
      tb.putcol('SPECTRAL_WINDOW_ID',spwi)
      tb.close()

# Concatenate all the Tsys (PolConvert will later sort the entries in time):
    if os.path.exists('./%s.concatenated.ms.tsys'%UID):
      os.system('rm -rf ./%s.concatenated.ms.tsys'%UID)

    os.system('cp -rf %s.ms.tsys ./%s.concatenated.ms.tsys'%(ASDMs[0],UID))
    for asd in ASDMs[1:]:
      tb.open(asd+'.ms.tsys')
      tb.copyrows('./%s.concatenated.ms.tsys'%UID)
      tb.close()

# Plot Tsys:
    aU.plotbandpass(caltable='./%s.concatenated.ms.tsys'%UID, overlay='time', 
      xaxis='freq', yaxis='amp', subplot=22, buildpdf=False, interactive=False,
      showatm=True,pwv='auto',chanrange='5~123',showfdm=True, 
      field='', figfile='%s.concatenated.ms.tsys.overlayTime/%s.tsys'%(UID,UID)) 
 

################







##########################
# SPECIFIC FOR VLBI
#################################################
# THIS CODE WILL ALWAYS BE RUN
# IF THERE IS AT LEAST ONE STEP > 4

if sum([sti>4 for sti in thesteps])>0:

  from collections import Counter

  # Get list of all antennas:
  tb.open('%s.concatenated.ms/ANTENNA'%UID)
  allants = tb.getcol('NAME')
  tb.close()

  # Figure out the (most used) APS-TelCal reference antenna:
  tb.open('%s.concatenated.ms.calappphase'%UID)

  nphant = tb.getcol('numPhasedAntennas')
  appref = tb.getcol('refAntennaIndex')
  allrefs = list(set(appref))
  phants = set(tb.getcell('phasedAntennas', rownr = 0))
  almaref = []



  # Figure out which antennas are ALWAYS phased:
  nphant = tb.getcol('numPhasedAntennas')
  phants = set(tb.getcell('phasedAntennas', rownr = 0))
  for i in range(len(nphant)):
    aux = tb.getcell('phasedAntennas', rownr = i)
    phants = phants.intersection(aux)
    almaref.append(aux[appref[i]])

  allalmaref = list(set(almaref))
  if len(allalmaref)>1:
    print "WARNING: THERE WAS MORE THAN ONE REFANT BY TELCAL: ",allalmaref

  phants = list(phants)

  tb.close()


  # if REFANT is not set, assign it to the APP refant:
  if len(REFANT)==0:
    REFANT = Counter(almaref).most_common()[0][0]
  message = '\n\n\n######################################\n' 
  message += "Will use %s as Reference Antenna.\n\n"%REFANT
  writeLog(message)
#  print message



  # Build a list of observing modes including APP / ALMA mode:
  tb.open('%s.concatenated.ms/STATE'%UID)
  modes = tb.getcol('OBS_MODE')


  APPMode = [i for i in range(len(modes)) 
    if 'APPPHASE_ACTIVE' in modes[i]]

  ALMAMode = [i for i in range(len(modes)) 
    if 'APPPHASE_ACTIVE' not in modes[i]]


  gainmode = [i for i in range(len(modes))
    if 'CALIBRATE_PHASE' in modes[i]]

  bandpassmode = [i for i in range(len(modes))
    if 'CALIBRATE_BANDPASS' in modes[i]]

  targetmode = [i for i in range(len(modes))
    if 'OBSERVE_TARGET' in modes[i]]

  polcalmode = [i for i in range(len(modes))
    if 'CALIBRATE_POLARIZATION' in modes[i]]

  tb.close()

  # Build a list of active phasing scans:
  tb.open('%s.concatenated.ms'%UID)
  states = tb.getcol('STATE_ID')
  scans = tb.getcol('SCAN_NUMBER')
  fields = list(set(tb.getcol('FIELD_ID')))

  vset = set()
  for w in APPMode: 
    vset = vset.union(set(scans[np.where(states == w)]))
  APPscans = ','.join(map(str,list(vset)))
  tb.close()


  tb.open('%s.concatenated.ms/FIELD'%UID) 
  FieldNames = tb.getcol('NAME')
  tb.close()

  try:
    FluxCalID = str(int(FluxCal))
  except:
    tb.open('%s.concatenated.ms/FIELD'%UID) 
    FluxCalID = str(np.where(FieldNames==FluxCal)[0][0])



  # Build a list of ALMA-calibration scans:
  vset = set()
  for w in ALMAMode: 
    vset = vset.union(set(scans[np.where(states == w)]))
  ALMAscans = ','.join(map(str,list(vset)))



# Find calibrators automatically, if not given in header:

  if len(BandPassCal) == 0:
    BandPassCal = str(fields[np.where(states==bandpassmode[0])[0][0]])

  if len(PolCal) == 0:
    PolCal = str(fields[np.where(states==polcalmode[0])[0][0]])

  PolCalName = str(FieldNames[int(PolCal)])

  if len(VLBIfluxCal)==0:
    VLBIfluxCalName = PolCalName
  else:
    VLBIfluxCalName = str(FieldNames[int(VLBIfluxCal)])

  if len(GainCal) == 0:
    GainCal = str(fields[np.where(states==gainmode[0])[0][0]])
    GainCalName = str(FieldNames[int(GainCal)])
  if len(Target) == 0:
    Target = ','.join(map(str,list(set(fields[np.where(states==targetmode[0])[0]]))))

  if len(VLBICal) == 0:
    Others = set(map(int,','.join([BandPassCal,PolCal,GainCal,Target,FluxCalID]).split(',')))
    All = set(fields)
    VLBICal = map(str,All.difference(Others))

  message = 'SELECTED SOURCES AND INTENTS:\n\n'
  message += 'BANDPASS: %s\n'%BandPassCal
  message += 'POLARIZATION: %s\n'%PolCal
  message += 'GAIN: %s\n'%GainCal
  message += 'TARGET(S): %s\n'%Target
  message += 'OTHER VLBI CALIBS.: %s\n'%VLBICal
  message += 'ABSOLUTE FLUX TAKEN FROM: %s\n'%FluxCal
  if BootStrapVLBIFlux:
    message += 'VLBI FLUXES WILL BE BOOTSTRAPPED FROM: %s\n\n'%VLBIfluxCalName
  else:
    message += 'WILL CONCANTENATE ALMA AND APP AMPLITUDE GAINS.\n\n'
  message += '######################################\n\n\n' 

#  print message
  writeLog(message)

#################################################





print "# Calibration"

# Save original flags
mystep = 5
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)
    
  if not os.path.exists('%s.concatenated.ms.flagversions/flags.Original'%UID):
    flagmanager(vis = '%s.concatenated.ms'%UID,
      mode = 'save',
      versionname = 'Original')
  else:
    flagmanager(vis = '%s.concatenated.ms'%UID,
      mode = 'restore',
      versionname = 'Original')



# Initial flagging
mystep = 6
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'restore',
    versionname = 'Original')


  # Flagging shadowed data
#  flagdata(vis = '%s.concatenated.ms'%UID,
#    mode = 'shadow',
#    flagbackup = F)

  # Flagging autocorrelations
  flagdata(vis='%s.concatenated.ms'%UID,
    autocorr = T,
    flagbackup = F)
  
  # Flagging "APP" antenna:
  flagdata(vis='%s.concatenated.ms'%UID, 
    mode = 'manual', antenna = APPAnt,
    flagbackup = F)


  # Once all bad data are flagged, we save the flags:
  if os.path.exists('%s.concatenated.ms.flagversions/flags.BeforeCalibration'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforeCalibration')

  flagmanager(vis='%s.concatenated.ms'%UID, 
    mode = 'save', versionname='BeforeCalibration')




# Putting a model for the flux calibrator(s)
mystep = 7
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')

  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  flagmanager(vis='%s.concatenated.ms'%UID, mode = 'restore', versionname='BeforeCalibration')

  clearcal(vis='%s.concatenated.ms'%UID)

  if IsPlanet:
    setjy(vis='%s.concatenated.ms'%UID, #usescratch=True, 
      field = FluxCal, standard = JyStandard)
  else:
    setjy(vis =  '%s.concatenated.ms'%UID,
      standard = 'manual',
      field = FluxCal,
      fluxdensity = QuasarFlux,
      spix = QuasarSpix,
   #   usescratch = True,
      reffreq = QuasarRefFreq)




# Save flags before bandpass cal
mystep = 8
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  if os.path.exists('%s.concatenated.ms.flagversions/flags.BeforeBandpassCalibration'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforeBandpassCalibration')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeBandpassCalibration')





# Bandpass calibration
mystep = 9
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  flagmanager(vis='%s.concatenated.ms'%UID, 
    mode = 'restore', versionname='BeforeBandpassCalibration')

  os.system('rm -rf %s.concatenated.ms.ap_pre_bandpass*'%UID) 

  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ap_pre_bandpass'%UID,
    field = BandPassCal,
    scan = ALMAscans,
    spw = '',
    gaintype = 'G',
    solint = 'int',
    refant = REFANT,
    calmode = 'p')


  if applyonly != True: 
    es.checkCalTable('%s.concatenated.ms.ap_pre_bandpass'%UID, 
      msName='%s.concatenated.ms'%UID, interactive=False) 



  os.system('rm -rf %s.concatenated.ms.bandpass*'%UID) 
  bandpass(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.bandpass'%UID,
    field = BandPassCal,
    scan = ALMAscans,
    solint = 'inf',
    combine = 'obs,scan',
    refant = REFANT,
    solnorm = True,
    bandtype = 'B',
    gaintable = '%s.concatenated.ms.ap_pre_bandpass'%UID)
  
  if applyonly != True: 
    es.checkCalTable('%s.concatenated.ms.bandpass'%UID, 
      msName='%s.concatenated.ms'%UID, interactive=False) 


##########################
# SPECIFIC FOR VLBI
  # Make a BP table with zero phases (APS TelCal ON)
  shutil.copytree('%s.concatenated.ms.bandpass'%UID, 
     '%s.concatenated.ms.bandpass-zphs'%UID)
  tb.open('%s.concatenated.ms.bandpass-zphs'%UID, nomodify=False)
  gains = tb.getcol('CPARAM')
  gains[:] = np.abs(gains)
  tb.putcol('CPARAM', gains)
  tb.close()
##########################




# Save flags before gain cal
mystep = 10
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  if os.path.exists('%s.concatenated.ms.flagversions/flags.BeforeGainCalibration'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforeGainCalibration')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeGainCalibration')



# Gain calibration
mystep = 11
if(mystep in thesteps):

#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  flagmanager(vis='%s.concatenated.ms'%UID, 
    mode = 'restore', versionname='BeforeGainCalibration')

  # List of unique source ids to transfer the flux:
  gfields = ','.join([s for s in list(set([BandPassCal,PolCal,GainCal,Target]+VLBICal)) if s!=FluxCalID])

#  clearcal(vis = '%s.concatenated.ms'%UID,field=fields,addmodel=True)


# Calibrate ALMA scans (phase):
  os.system('rm -rf %s.concatenated.ms.phase_int'%UID)
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.phase_int'%UID,
    field = '',
    scan = ALMAscans,
    solint = 'int',
    refant = REFANT,
    gaintype = 'G',
    calmode = 'p',
    gainfield = BandPassCal,
    gaintable = '%s.concatenated.ms.bandpass'%UID)

##########################
# SPECIFIC FOR VLBI
# Calibrate APP scans (phase):
  os.system('rm -rf %s.concatenated.ms.phase_int.APP'%UID)
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.phase_int.APP'%UID,
    field = '',
    scan = APPscans,
    solint = 'int',
    refant = REFANT,
    gaintype = 'G',
    calmode = 'p',
    gainfield = BandPassCal,
    gaintable = '%s.concatenated.ms.bandpass-zphs'%UID)


#######################################
# Re-reference phases to REFANT:
  writeLog('Re-referencing all phases to the REFANT')
  tb.open('%s.concatenated.ms/ANTENNA'%UID)
  REFANT_IDX = int(np.where(tb.getcol('NAME')==REFANT)[0][0])
 

  for gaintable in ['%s.concatenated.ms.phase_int.APP'%UID,'%s.concatenated.ms.phase_int'%UID]:
    tb.open(gaintable,nomodify=False)
    timearr = tb.getcol('TIME')
    calant = tb.getcol('ANTENNA2')
    solant = tb.getcol('ANTENNA1')
    ifsol = tb.getcol('SPECTRAL_WINDOW_ID')
    allifs = list(set(ifsol))
    alltimes = list(set(timearr))
    gains = tb.getcol('CPARAM')
    for ti in alltimes:
      is2 = (timearr==ti)  #*(calant==REFANT_IDX)
      is22 = is2*(solant==REFANT_IDX)
      for IFi in allifs:
        isifref = is22*(ifsol==IFi)
        isifcal = is2*(ifsol==IFi)
        if np.sum(isifref)>0:
          gains[:,:,isifcal] /= gains[:,:,isifref]
    tb.putcol('CPARAM',gains)
    tb.close()
#######################################
##########################

#if True:
# Calibrate ALMA scans (amp):
  os.system('rm -rf %s.concatenated.ms.ampli_inf'%UID)
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ampli_inf'%UID,
    field = '',
    scan = ALMAscans,
    solint = 'inf',
    combine = '',
    refant = REFANT,
    gaintype = 'T',
    calmode = 'a',
    gainfield = [BandPassCal,''],
    gaintable = ['%s.concatenated.ms.bandpass'%UID,'%s.concatenated.ms.phase_int'%UID])


##########################
# SPECIFIC FOR VLBI
################################
# CASA CRASHED WHEN TRYING TO APPEND THESE TO THE
# PREVIOUS TABLE (???). HENCE, WE HAVE TO CREATE A NEW
# TABLE FOR THE APP AMPLITUDE GAINS.

# Calibrate APP scans (amp):
  os.system('rm -rf %s.concatenated.ms.ampli_inf.APP'%UID)

  if BootStrapVLBIFlux:
    caltable = '%s.concatenated.ms.ampli_inf.APP'%UID
    append = False
  else:
    caltable = '%s.concatenated.ms.ampli_inf'%UID
    append = True

  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = caltable,
    append = append,
    field = '',
    scan = APPscans,
    solint = 'inf',
    combine = '',
    refant = REFANT,
    gaintype = 'T',
    calmode = 'a',
    gainfield = [BandPassCal,''],
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID,'%s.concatenated.ms.phase_int.APP'%UID])
################################

  print 'Deriving flux densities for all sources'
# Calibrate absolute flux density the ordinary way:
  os.system('rm -rf %s.concatenated.ms.flux_inf*'%UID) 
  os.system('rm -rf %s.concatenated.ms.fluxscale*'%UID) 
  mylogfile = casalog.logfile()
  casalog.setlogfile('%s.concatenated.ms.fluxscale'%UID)
  
  fluxscaleDict = fluxscale(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ampli_inf'%UID,
    fluxtable = '%s.concatenated.ms.flux_inf'%UID,
    transfer = gfields,antenna=','.join(phants),
    reference = FluxCal)

##########################
# SPECIFIC FOR VLBI
##################################################################
#############################
# THE FLUX CALIBRATOR USED BY ALMA MIGHT NOT BE OBSERVED IN APP MODE.
# WE NEED TO BROADCAST THE FLUX FROM THE GAIN CALIBRATOR INTO THE 
# APP AMPLITUDE TABLE.
#############################

  if BootStrapVLBIFlux:
# Scale the APP-only amplitude gains: 
    os.system('rm -rf %s.concatenated.ms.flux_inf.APP Aux.tab'%UID) 
    os.system('cp -r  %s.concatenated.ms.ampli_inf.APP Aux.tab'%UID)

# Will use a VLBI calibrator (the polcal is the default) as secondary flux-density calibrator:

    tb.open('Aux.tab',nomodify=False)
    tspwids = tb.getcol('SPECTRAL_WINDOW_ID')
    tfields = tb.getcol('FIELD_ID')
    tgains = tb.getcol('CPARAM')
    for entr in range(len(tspwids)):
     if str(tfields[entr]) == VLBIfluxCal:
      flux = fluxscaleDict[VLBIfluxCal][str(tspwids[entr])]['fitFluxd']
      tgains[:,:,entr] /= np.sqrt(flux)

    tb.putcol('CPARAM',tgains)
    tb.close()

    fluxscaleDict = fluxscale(vis = '%s.concatenated.ms'%UID,
     caltable = 'Aux.tab',
     fluxtable = '%s.concatenated.ms.flux_inf.APP'%UID,
     transfer = '',antenna=','.join(phants),
     reference = str(VLBIfluxCalName)) 

    casalog.setlogfile(mylogfile)
    os.system('rm -rf Aux.tab')

  else:
 
    os.system('cp -r %s.concatenated.ms.flux_inf %s.concatenated.ms.flux_inf.APP'%(UID,UID))
    casalog.setlogfile(mylogfile)

  if not IsPlanet:  # Insert the flux density of the flux cal. (in case it is also the pol. cal).
    fluxscaleDict[FluxCalID] = {}; fluxscaleDict[FluxCalID]['fitFluxd'] = QuasarFlux
    nuRef = qa.convertfreq(QuasarRefFreq,'Hz')['value']; 
    for spwi in range(4):
      fluxscaleDict[FluxCalID][str(spwi)] = {}
      fluxscaleDict[FluxCalID][str(spwi)]['fluxd'] = [QuasarFlux*np.power(fluxscaleDict['freq'][spwi]/nuRef,QuasarSpix),0.,0.,0.]

##################################################################
# Save the fluxscale dictionary into an external file:
  import pickle as pk
  outf = open('%s.concatenated.ms.fluxscale.dict'%UID,'w')
  pk.dump(fluxscaleDict,outf)
  outf.close()

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeApplycal')

  if applyonly != True: 
    es.checkCalTable('%s.concatenated.ms.ampli_inf'%UID, 
      msName='%s.concatenated.ms'%UID, interactive=False) 

    es.checkCalTable('%s.concatenated.ms.flux_inf'%UID,
      msName='%s.concatenated.ms'%UID, interactive=False)

##########################
# SPECIFIC FOR VLBI
  # Plot gains for PHASED and CONTROL antennas separately:
    os.system('rm -rf %s.concatenated.ms.GAINS.plots'%UID)
    os.system('mkdir %s.concatenated.ms.GAINS.plots'%UID)
    os.system('mkdir %s.concatenated.ms.GAINS.plots/PHASED'%UID)
    os.system('mkdir %s.concatenated.ms.GAINS.plots/CONTROL'%UID)

    if doPlot:
     for antnam in allants:

      if antnam in phants:
        dirname = '%s.concatenated.ms.GAINS.plots/PHASED'%UID
      else:
        dirname = '%s.concatenated.ms.GAINS.plots/CONTROL'%UID

      plotcal(caltable = '%s.concatenated.ms.phase_int'%UID,
        xaxis = 'time', yaxis = 'phase',
        antenna = antnam, spw = '', plotrange = [0,0,-180,180],
        iteration = 'spw', subplot = 411,showgui=showgui,
        figfile = '%s/%s.PHASE.png'%(dirname,antnam))

      plotcal(caltable = '%s.concatenated.ms.ampli_inf'%UID,
        xaxis = 'time', yaxis = 'amp',showgui=showgui,
        antenna = antnam, spw = '', 
        iteration = 'spw', subplot=411,
        figfile = '%s/%s.AMP.png'%(dirname,antnam))

      plotcal(caltable = '%s.concatenated.ms.flux_inf'%UID, 
        xaxis = 'time', yaxis = 'amp',showgui=showgui,
        antenna = antnam, spw = '', 
        iteration = 'spw', subplot = 411,
        figfile = '%s/%s.FLUX.png'%(dirname,antnam))

      plotcal(caltable = '%s.concatenated.ms.phase_int.APP'%UID,
        xaxis = 'time', yaxis = 'phase',showgui=showgui,
        antenna = antnam, spw = '', plotrange = [0,0,-180,180],
        iteration = 'spw', subplot = 411,
        figfile = '%s/%s.PHASE.APP.png'%(dirname,antnam))

##########################








# Apply calibration
mystep = 12
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  writeLog('\n\n\n     SOME CASA ERRORS MAY APPEAR SOON.\n    THEY ARE RELATED TO POSSIBLE MISSING SCANS IN APP/ALMA MODE.\n    THESE ERRORS SHOULD BE HARMLESS\n\n\n')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforeApplycal')


# BANDPASS (ALMA):
#  print 'Applying calibration to Bandpass (ALMA Obs.)'
#  applycal(vis = '%s.concatenated.ms'%UID,
#    field = BandPassCal,
#    gaintable = ['%s.concatenated.ms.bandpass'%UID, 
#      '%s.concatenated.ms.flux_inf'%UID,
#      '%s.concatenated.ms.phase_int'%UID],
#    gainfield = [BandPassCal, BandPassCal,BandPassCal],
#    interp = ['linear','nearest','nearest'],
#    antenna = ','.join(phants)+'&',
#    scan = ALMAscans,
#    calwt = [T,T,F],
#    parang = F,
#    flagbackup = F)

# BANDPASS (APP):
#  print 'Applying calibration to Bandpass (APP Obs.)'
#  applycal(vis = '%s.concatenated.ms'%UID,
#    field = BandPassCal,
#    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID, 
#      '%s.concatenated.ms.flux_inf.APP'%UID,
#      '%s.concatenated.ms.phase_int.APP'%UID],
#    gainfield = [BandPassCal, BandPassCal,BandPassCal],
#    interp = ['linear','nearest','nearest'],
#    scan = APPscans,
#    antenna = ','.join(phants)+'&',
#    calwt = [T,T,F],
#    parang = F,
#    flagbackup = F)

# POL. CALIBRATOR (ALMA):
#  print 'Applying calibration to Pol. calibrator (ALMA Obs.)'
#  applycal(vis = '%s.concatenated.ms'%UID,
#    field = PolCal,
#    gaintable = ['%s.concatenated.ms.bandpass'%UID,
#      '%s.concatenated.ms.flux_inf'%UID, 
#      '%s.concatenated.ms.phase_int'%UID],
#    gainfield = [BandPassCal, PolCal,PolCal],
#    interp = ['linear','nearest','nearest'],
#    scan = ALMAscans,
#    antenna = ','.join(phants)+'&',
#    calwt = [T,T,F],
#    parang = F,
#    flagbackup = F)

# POL. CALIBRATOR (APP):
#  print 'Applying calibration to Pol. calibrator (APP Obs.)'
#  applycal(vis = '%s.concatenated.ms'%UID,
#    field = PolCal,
#    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID,
#      '%s.concatenated.ms.flux_inf.APP'%UID, 
#      '%s.concatenated.ms.phase_int.APP'%UID],
#    gainfield = [BandPassCal, PolCal,PolCal],
#    interp = ['linear','nearest','nearest'],
#    scan = APPscans,
#    antenna = ','.join(phants)+'&',
#    calwt = [T,T,F],
#    parang = F,
#    flagbackup = F)



# ALL SOURCES: 
  for si in FieldNames: # [GainCal,Target]+VLBICal:
   # ALMA
    print 'Applying calibration to %s (ALMA Obs.)'%si
    applycal(vis = '%s.concatenated.ms'%UID,
      field = si,
      gaintable = ['%s.concatenated.ms.bandpass'%UID, 
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.phase_int'%UID],
      gainfield = [BandPassCal,si,si],
      interp = ['linear','nearest','nearest'],
      scan = ALMAscans,
      antenna = ','.join(phants)+'&',
      calwt = [T,T,F],
      parang = F,
      flagbackup = F)

   # APP
    print 'Applying calibration to %s (APP Obs.)'%si
    applycal(vis = '%s.concatenated.ms'%UID,
      field = si,
      gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID, 
      '%s.concatenated.ms.flux_inf.APP'%UID,
      '%s.concatenated.ms.phase_int.APP'%UID],
      gainfield = [BandPassCal,si,si],
      interp = ['linear','nearest','nearest'],
      scan = APPscans,
      antenna = ','.join(phants)+'&',
      calwt = [T,T,F],
      parang = F,
      flagbackup = F)






##########################
# SPECIFIC FOR VLBI
# Split calibrated data (without the control antennas):
mystep = 13
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  os.system('rm -rf %s.calibrated.ms*'%UID)
  split(vis = '%s.concatenated.ms'%UID,
    datacolumn='corrected',
    keepflags = False,
    antenna=','.join(phants)+'&',
    outputvis = '%s.calibrated.ms'%UID)
##########################



# Save flags before polarization calibration
mystep = 14
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  if os.path.exists('%s.calibrated.ms.flagversions/flags.BeforePolCal'%UID):
    flagmanager(vis='%s.calibrated.ms'%UID, 
      mode = 'delete', versionname='BeforePolCal')

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'save',
    versionname = 'BeforePolCal')







# Polarization calibration 
mystep = 15
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)

  import recipes.almahelpers as ah
  import recipes.almapolhelpers as aph

  if True:
   clearcal(vis = '%s.calibrated.ms'%UID)
#  if True:
   flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforePolCal')

##########################
# SPECIFIC FOR VLBI
# FLAG SHADOWING (we can do it now):
   flagdata(vis = '%s.calibrated.ms'%UID,
    mode = 'shadow',
    flagbackup = F)


# FLAG first seconds of scans:
   flagdata(vis='%s.calibrated.ms'%UID,
    mode='quack',
    scan = APPscans,
    quackinterval=10.0,
    quackmode = 'beg',
    flagbackup=F)
##########################

# FLAG noisy scans on 3c279:
   flagdata(vis='%s.calibrated.ms'%UID,
     mode='manual',
     scan = '306~333',
     flagbackup=F)
##########################




###########################
# Figure out typical scan duration of the polcalib scans:

   ms.open('%s.calibrated.ms'%UID)
   summary = ms.summary()
   ms.close()

   tb.open('%s.calibrated.ms'%UID)
   scnum = tb.getcol('SCAN_NUMBER')
   stid = tb.getcol('STATE_ID')
   tb.close()

   tb.open('%s.calibrated.ms/STATE'%UID)
   obsm = tb.getcol('OBS_MODE')
   tb.close()

   polst = [i for i in range(len(obsm)) if 'CALIBRATE_POLARIZATION' in obsm[i]]
   polscans = ['scan_%i'%i for i in np.unique(scnum[np.in1d(stid,polst)])]

   scandur = int(np.median([summary[ss]['0']['EndTime'] - summary[ss]['0']['BeginTime'] 
    for ss in polscans])*86400.)

   print 'Will pre-average pol. data by %i seconds for calibration'%scandur

# Re-calibrate polarization calibrator:

   print 'Calibrating X/Y amplitude ratios'

# Read the flux-density of the pol. calibrator:
   import pickle as pk
   fluxDict = pk.load(open('%s.concatenated.ms.fluxscale.dict'%UID))
   PolCalFlux = [fluxDict[PolCal][str(spwi)]['fluxd'][0] for spwi in range(4)]

# ALMA Calibration data:
   os.system('rm -rf %s.calibrated.ms.Gpol1*'%UID)
   if Use_APP_for_Gpol:
    gaincal(vis = '%s.calibrated.ms'%UID,
      caltable = '%s.calibrated.ms.Gpol1'%UID,
      field = PolCal,
      solint = 'inf',
      combine = '',
      gaintype = 'G',
      calmode = 'a',
      refant = REFANT)
   else:
##########################
# SPECIFIC FOR VLBI
    gaincal(vis = '%s.calibrated.ms'%UID,
      caltable = '%s.calibrated.ms.Gpol1'%UID,
      field = PolCal,
      scan = ALMAscans,
      solint = 'inf',
      combine = '',
      gaintype = 'G',
      calmode = 'a',
      refant = REFANT)
##########################



##########################
# SPECIFIC FOR VLBI
# Pre-scaling Gpol solutions:
# (i.e., we multiply the gain of each antenna by its time average)

  os.system('rm -rf %s.calibrated.ms.Gpol1.scaled'%UID)
  os.system('cp -r %s.calibrated.ms.Gpol1 %s.calibrated.ms.Gpol1.scaled'%(UID,UID))

  tb.open('%s.calibrated.ms.Gpol1.scaled'%UID,nomodify = False)
  ants = tb.getcol('ANTENNA1')
  amps = tb.getcol('CPARAM')
  spwids = tb.getcol('SPECTRAL_WINDOW_ID')
  scannum = tb.getcol('SCAN_NUMBER')
  flagstat = tb.getcol('FLAG')
  times = tb.getcol('TIME')

  AntMax = np.max(ants)+1
  spwMax = np.max(spwids)+1
  RatiosALMA = np.ones((AntMax,spwMax))
  RatiosAPP = np.ones((AntMax,spwMax))

  for ant in range(AntMax):
   antmask = ants==ant
   for spw in range(spwMax):
     rows = np.where(np.logical_and(antmask,spwids==spw))[0]
     if len(rows)>0:
       avrat = np.average(np.abs(amps[1,:,rows]/amps[0,:,rows]))
       amps[0,:,rows] *= avrat
       tt = times[rows] ; Dtt = tt[1:] - tt[:-1] ; jumps = np.where(Dtt>GpolTmax)
       RatiosALMA[ant,spw] = avrat.real
       flagstat[0,0,rows[0]] = True; flagstat[1,0,rows[0]] = True
       for jump in jumps[0]:
        for k in range(1,GpolNFlag+1):
          flagstat[0,0,rows[jump+k]] = True
          flagstat[1,0,rows[jump+k]] = True

  scilist = list(set(scannum))
  badGscans = []

  for sci in scilist:
    rows = np.where(scannum==sci)
    amprms = np.std(np.abs(amps[1,:,rows])/np.abs(amps[0,:,rows]))
    if amprms > BadGpol:
      badGscans.append(sci)
      print 'Scan %i will be flagged (G ratio rms of %.2e)\n'%(sci,amprms)

  for sci in badGscans:
    rows = np.where(scannum==sci)
    flagstat[0,0,rows] = True
    flagstat[1,0,rows] = True

  tb.putcol('FLAG',flagstat)
  tb.putcol('CPARAM',amps)
  tb.close()

  tb.open('%s.calibrated.ms.Gpol1'%UID,nomodify = False)
  flagstat2 = tb.getcol('FLAG')
  flagstat2[:] = flagstat
#  for sci in badGscans:
#    rows = np.where(scannum==sci)
#    flagstat2[0,0,rows] = True
#    flagstat2[1,0,rows] = True
  tb.putcol('FLAG',flagstat2)
  tb.close()

#  flagdata(vis='%s.calibrated.ms'%UID,
#    mode='manual',
#    scan = ','.join(map(str,badGscans)),
#    flagbackup=F)
##########################

  if doPlot:
    os.system('rm -rf %s.calibrated.ms.Gpol1.png'%UID)
    plotcal('%s.calibrated.ms.Gpol1'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol1.png'%UID,showgui=showgui)

    os.system('rm -rf %s.calibrated.ms.Gpol1.scaled.png'%UID)
    plotcal('%s.calibrated.ms.Gpol1.scaled'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol1.scaled.png'%UID,showgui=showgui)

 # raw_input("HOLD")

##########
# Save QU output in external file:
  print 'Estimating QU from gains'

  orig_stdout = sys.stdout
  f = open('%s.QUfromGain.txt'%UID, 'w')
  sys.stdout = f

# Rough estimate of QU:
  qu = aph.qufromgain('%s.calibrated.ms.Gpol1.scaled'%UID) 
  print qu
  sys.stdout = orig_stdout
  f.close()





# Impose PolStokes:
  if len(PolStokes)==4:
    qu = {0:list(PolStokes)}
    f = open('%s.QUfromGain.txt'%UID,mode='a')
    print >> f, '\nWILL FORCE THE STOKES PARAMETERS TO BE: ',qu
    f.close()


  f = open('%s.QUfromGain.txt'%UID)
  print f.read()
  f.close()
##########

##########################
# NOT QUITE NEEDED FOR VLBI
##################################################
# We search for the scan where the polarization signal is minimum in XX and YY
# (i.e., maximum in XY and YX):
#
  tb.open('%s.calibrated.ms.Gpol1.scaled'%UID)
  scans = tb.getcol('SCAN_NUMBER')
  gains = np.squeeze(tb.getcol('CPARAM'))

  scanlist = np.array(list(set(scans)))
  ratios = np.zeros(len(scanlist))
  ALMAscanL = map(int,ALMAscans.split(','))
  for si, s in enumerate(scanlist):
    filt = scans == s
    ratio = np.sqrt(
      np.average(np.power(np.abs(gains[0,filt])/np.abs(gains[1,filt])-1.0,2.)))
    ratios[si] = ratio

  bestscidx = np.argmin(ratios)
  bestscan = scanlist[bestscidx]
  bestratio = ratios[bestscidx]
  print 'Scan with highest expected X-Y signal: ', bestscan


  tb.close()


#####################################################
#
#
# Cross-hand delay:
#  os.system('rm -rf %s.concatenated.ms.Kcrs'%UID)
#  gaincal(vis='%s.concatenated.ms'%UID,
#        caltable='%s.concatenated.ms.Kcrs'%UID,
#        selectdata=T,
#        field = PolCal,
#        scan=bestscan,
#        gaintype='KCROSS',
#        solint='inf',
#        refant=REFANT, 
#        smodel = [1,0,1,0],
#        gaintable=['%s.concatenated.ms.bandpass'%UID,
#          '%s.concatenated.ms.flux_inf'%UID,
#          '%s.concatenated.ms.phase_int'%UID,
#          '%s.concatenated.ms.Gpol1'%UID])



  orig_stdout = sys.stdout

#  f = open('%s.PolFromGaincal.txt'%UID, 'w')

# XY phase offset:
  if not Avoid_ALMA_PolCal:

    os.system('rm -rf %s.calibrated.ms.XY0amb.ALMA'%UID)

#    print >> f,"From ALMA Calibration Data:\n" 
#    sys.stdout = f

    gaincal(vis='%s.calibrated.ms'%UID,
      caltable='%s.calibrated.ms.XY0amb.ALMA'%UID, 
      field= PolCal,
      scan = ALMAscans,
      gaintype='XYf+QU',
      solint='inf',
      combine='scan,obs',
      preavg=scandur, 
      refant=REFANT,
      smodel=[1,0,1,0],
      gaintable='%s.calibrated.ms.Gpol1'%UID)

    print 'END OF GAINCAL'

##########################
# SPECIFIC FOR VLBI
#  print >> f,"\nFrom APS-TelCal Calibrated Data:\n" 

  os.system('rm -rf %s.calibrated.ms.XY0amb.APP'%UID)

  gaincal(vis='%s.calibrated.ms'%UID,
    caltable='%s.calibrated.ms.XY0amb.APP'%UID, 
    field= PolCal,
    scan = APPscans,
    gaintype='XYf+QU',
    solint='inf',
    combine='scan,obs',
    preavg=scandur, 
    refant=REFANT,
    smodel=[1,0,1,0],
    gaintable='%s.calibrated.ms.Gpol1'%UID)

#  print 'END OF GAINCAL'

  #raw_input("HOLD")

################
# CORRECT 180 DEG. AMBIGUITIES WITHIN EACH SPW:

  tb.open('%s.calibrated.ms.XY0amb.APP'%UID,nomodify=False)
  spwids = tb.getcol('SPECTRAL_WINDOW_ID')
  data = tb.getcol('CPARAM')

  for spwi in range(4):
    spmask = spwids == spwi
    spdata = data[:,:,spmask]
    for ji in XYJUMPS_APP[spwi]:
      data[0,ji:,spmask] *= -1.0

  tb.putcol('CPARAM',data)

  tb.close()

  if not Avoid_ALMA_PolCal:

    tb.open('%s.calibrated.ms.XY0amb.ALMA'%UID,nomodify=False)
    spwids = tb.getcol('SPECTRAL_WINDOW_ID')
    data = tb.getcol('CPARAM')

    for spwi in range(4):
      spmask = spwids == spwi
      spdata = data[:,:,spmask]
      for ji in XYJUMPS_ALMA[spwi]:
        data[0,ji:,spmask] *= -1.0

    tb.putcol('CPARAM',data)

    tb.close()


################


#  sys.stdout = orig_stdout
#  f.close()


#  f = open('%s.PolFromGaincal.txt'%UID)
#  print f.read()
#  f.close()


# Solve QU ambiguity:
  os.system('rm -rf %s.calibrated.ms.XY0.APP'%UID)
  os.system('rm -rf %s.calibrated.ms.XY0.ALMA'%UID)

  orig_stdout = sys.stdout
  f = open('%s.XY-Ambiguity.txt'%UID, 'w')
  sys.stdout = f

  if not Avoid_ALMA_PolCal:
    f.write('\nFROM ALMA SCANS:\n')
    S=aph.xyamb(xytab='%s.calibrated.ms.XY0amb.ALMA'%UID, 
      qu=qu[qu.keys()[0]], xyout='%s.calibrated.ms.XY0.ALMA'%UID)

  f.write('\nFROM APP SCANS:\n')
  S2=aph.xyamb(xytab='%s.calibrated.ms.XY0amb.APP'%UID, 
    qu=qu[qu.keys()[0]], xyout='%s.calibrated.ms.XY0.APP'%UID)


  sys.stdout = orig_stdout
  f.close()

  quspwALMA = []
  quspwAPP = []
  quspw = []



##########################
# SPECIFIC FOR VLBI
  if Use_PolModel_from_APP:
    S = S2    

# Correct sign of the Stokes parameters:
  QUsgn = [si/np.abs(si) for si in S[:-1]]

# Scale model to flux density of pol. calibrator:
  S = [si*np.average(PolCalFlux) for si in S]

# Impose PolStokes:
  if len(PolStokes)==4:
    S = [si*np.average(PolCalFlux) for si in PolStokes]
    f = open('%s.XY-Ambiguity.txt'%UID,mode='a')
    print >> f,'WILL FORCE THE STOKES PARAMETERS TO BE:\n',S
    f.close()


# Account for RM (parse the output file from XY-Ambiguity):
  elif Calib_RM:
    f = open('%s.XY-Ambiguity.txt'%UID)
    lines = (f.read()).split('\n')
    f.close()
    APPline = lines.index('FROM APP SCANS:')
    for li, l in enumerate(lines):
      if 'Spw = ' in l:
        temp= l.split()
        temp2 = (l[l.find('[')+1:].replace(']','')).split()
        spi = int(temp[2][:-1])
        Qi = np.abs(float(temp2[0]))*QUsgn[1]
        Ui = np.abs(float(temp2[1]))*QUsgn[2]
        if li > APPline:
          quspwAPP.append([spi,PolCalFlux[spi],Qi*PolCalFlux[spi],Ui*PolCalFlux[spi]])
        else:
          quspwALMA.append([spi,PolCalFlux[spi],Qi*PolCalFlux[spi],Ui*PolCalFlux[spi]])

    if Use_PolModel_from_APP:
      quspw = quspwAPP
    else:
      quspw = quspwALMA
##########################
    







  f = open('%s.XY-Ambiguity.txt'%UID)
  print f.read()
  f.close()

  if doPlot:
   if not Avoid_ALMA_PolCal:
    plotcal('%s.calibrated.ms.XY0amb.ALMA'%UID,'freq','phase',showgui=showgui,
     antenna='0', poln='X', subplot=121,plotrange=[0,0,-180,180])

    plotcal('%s.calibrated.ms.XY0.ALMA'%UID,'freq','phase',showgui=showgui,
     antenna='0',poln='X',subplot=122,plotrange=[0,0,-180,180],
     figfile='%s.XY0_Amb-NoAmb.ALMA.png'%UID)

    plotcal('%s.calibrated.ms.XY0.ALMA'%UID,'chan','phase',showgui=showgui,
     antenna='0',poln='X',subplot=221,plotrange=[0,0,-180,180],
     iteration='spw',figfile='%s.XY-CrossPhase.ALMA.png'%UID)

    if checkJumps:
     raw_input('PLEASE, CHECK FOR POSSIBLE 180 DEG. JUMPS IN EACH SPW (Ctrl+D TO ABORT)')

   plotcal('%s.calibrated.ms.XY0amb.APP'%UID,'freq','phase',plotrange=[0,0,-180,180],
     antenna='0', poln='X', subplot=121,showgui=showgui)

   plotcal('%s.calibrated.ms.XY0.APP'%UID,'freq','phase',
     antenna='0',poln='X',subplot=122,plotrange=[0,0,-180,180],
     figfile='%s.XY0_Amb-NoAmb.APP.png'%UID,showgui=showgui)

   plotcal('%s.calibrated.ms.XY0.APP'%UID,'chan','phase',
     antenna='0',poln='X',subplot=221,plotrange=[0,0,-180,180],
     iteration='spw',figfile='%s.XY-CrossPhase.APP.png'%UID,showgui=showgui)


   if checkJumps:
    raw_input('PLEASE, CHECK FOR POSSIBLE 180 DEG. JUMPS IN EACH SPW (Ctrl+D TO ABORT)')



## Re-calibrate polarization calibrator (with right pol. model): 
  os.system('rm -rf %s.calibrated.ms.Gpol2*'%UID) 


# ALMA Calibration data:
  if not Avoid_ALMA_PolCal:
   gaincal(vis = '%s.calibrated.ms'%UID,
    caltable = '%s.calibrated.ms.Gpol2.ALMA'%UID,
    scan = ALMAscans,
    field = PolCal,
    solint = 'inf',
    combine = '',
    calmode = 'a',
    smodel = S,
    refant = REFANT,
    parang = T,
    solnorm = T,
    gaintable='%s.calibrated.ms.XY0.ALMA'%UID)

##########################
# SPECIFIC FOR VLBI
# APP Data:
  gaincal(vis = '%s.calibrated.ms'%UID,
    caltable = '%s.calibrated.ms.Gpol2.APP'%UID,
    scan = APPscans,
    field = PolCal,
    solint = '%is'%scandur,
    combine = 'scan,obs',
    calmode = 'a',
    smodel = S,
    parang = T,
    solnorm = T,
    refant = REFANT,
    gaintable='%s.calibrated.ms.XY0.APP'%UID)

##########
## Check for any residual polarization signal:

  orig_stdout = sys.stdout
  f = open('%s.QUfromGain.txt'%UID, 'a')
  f.write('\n\n USING POLCAL MODEL:\n\n')
  sys.stdout = f

  if not Avoid_ALMA_PolCal:
   f.write('\n ALMA SCANS:\n')
   qu2 = aph.qufromgain('%s.calibrated.ms.Gpol2.ALMA'%UID)
   print qu2
  f.write('\n APP SCANS:\n')
  qu3 = aph.qufromgain('%s.calibrated.ms.Gpol2.APP'%UID)
  print qu3
  sys.stdout = orig_stdout
  f.close()

  f = open('%s.QUfromGain.txt'%UID)
  print f.read()
  f.close()
##########


  if doPlot:

   if not Avoid_ALMA_PolCal:
    os.system('rm -rf %s.calibrated.ms.Gpol2.ALMA.png'%UID)
    plotcal('%s.calibrated.ms.Gpol2.ALMA'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol2.ALMA.png'%UID,showgui=showgui)

   os.system('rm -rf %s.calibrated.ms.Gpol2.APP.png'%UID)
   plotcal('%s.calibrated.ms.Gpol2.APP'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol2.APP.png'%UID,showgui=showgui)


##########################
# NOT QUITE NEEDED FOR VLBI
# Plot RMS of gain ratios around 1.0:

  if not Avoid_ALMA_PolCal:
   tb.open('%s.calibrated.ms.Gpol2.ALMA'%UID)
   scans2 = tb.getcol('SCAN_NUMBER')
   gains = np.squeeze(tb.getcol('CPARAM'))
   tb.close()
   scanlist2 = np.array(list(set(scans2)))
   ratios2 = np.zeros(len(scanlist2))
   for si, s in enumerate(scanlist2):
    filt = scans2 == s
    ratio = np.sqrt(np.average(np.power(np.abs(gains[0,filt])/np.abs(gains[1,filt])-1.0,2.)))
    ratios2[si] = ratio

#   if doPlot:
#    os.system('rm -rf %s.GainRatiosPol.png'%UID)
#    pl.ioff()
#    fig = pl.gcf()
#    pl.clf() #pl.figure()
#    sub = fig.add_subplot(111)
#    sub.plot(scanlist, ratios, 'or', label = 'No Polcal')
#    sub.plot(scanlist2, ratios2, 'ob', label = 'Polcal')
#    sub.plot([scanlist[bestscidx]], [ratios[bestscidx]], 'xk', label = 'Best Pol. Scan')
#    sub.set_xlabel('Scan Number')
#    sub.set_ylabel('Gain Ratio RMS')
#    pl.legend(numpoints=1)
#    pl.savefig('%s.GainRatiosPol.png'%UID)
#    pl.clf()
#
#   pl.ion()
##########################




# Calibrate D-terms:


# ALMA:
  os.system('rm -rf %s.calibrated.ms.Df0*'%UID) 
  if not Avoid_ALMA_PolCal:
   if len(quspw)==0:
    polcal(vis='%s.calibrated.ms'%UID,
      caltable='%s.calibrated.ms.Df0.ALMA'%UID, 
      field= PolCal,
      scan = ALMAscans, 
      solint='inf',
      combine='obs,scan',
      preavg=scandur,
      poltype='Dflls',
      refant='', #solve absolute D-term
      smodel=S,
      gaintable=['%s.calibrated.ms.Gpol2.ALMA'%UID,
        '%s.calibrated.ms.XY0.ALMA'%UID])
   else:
    for ii,spi in enumerate(quspw):
      polcal(vis='%s.calibrated.ms'%UID,
        caltable='%s.calibrated.ms.Df0.ALMA'%UID, 
        field= PolCal,
        scan = ALMAscans, 
        spw = str(spi[0]),
        solint='inf',
        combine='obs,scan',
        append = ii>0,
        preavg=scandur,
        poltype='Dflls',
        refant='', #solve absolute D-term
        smodel=[spi[1], spi[2], spi[3], 0.0],
        gaintable=['%s.calibrated.ms.Gpol2.ALMA'%UID,
          '%s.calibrated.ms.XY0.ALMA'%UID])

## APP:
  if Use_APP_for_Gpol:
   if len(quspw)==0:
    polcal(vis='%s.calibrated.ms'%UID,
      caltable='%s.calibrated.ms.Df0.APP'%UID, 
      field= PolCal,
      scan = APPscans, 
      solint='inf',
      combine='obs,scan',
      preavg=scandur,
      poltype='Dflls',
      refant='', #solve absolute D-term
      smodel=S,
      gaintable=['%s.calibrated.ms.Gpol2.APP'%UID,
        '%s.calibrated.ms.XY0.APP'%UID])
   else:
    for ii,spi in enumerate(quspw):
     polcal(vis='%s.calibrated.ms'%UID,
      caltable='%s.calibrated.ms.Df0.APP'%UID, 
      field= PolCal,
      append = ii>0,
      spw = str(spi[0]),
      scan = APPscans, 
      solint='inf',
      combine='obs,scan',
      preavg=scandur,
      poltype='Dflls',
      refant='', #solve absolute D-term
      smodel=[spi[1],spi[2],spi[3],0.0],
      gaintable=['%s.calibrated.ms.Gpol2.APP'%UID,
        '%s.calibrated.ms.XY0.APP'%UID])
  else:
   if len(quspw)==0:
    polcal(vis='%s.calibrated.ms'%UID,
      caltable='%s.calibrated.ms.Df0.APP'%UID, 
      field= PolCal,
      scan = APPscans, 
      solint='inf',
      combine='obs,scan',
      preavg=scandur,
      poltype='Dflls',
      refant='', #solve absolute D-term
      smodel=S,
      gaintable=['%s.calibrated.ms.Gpol2.ALMA'%UID,
        '%s.calibrated.ms.XY0.APP'%UID])
   else:
    for ii,spi in enumerate(quspw):
     polcal(vis='%s.calibrated.ms'%UID,
      caltable='%s.calibrated.ms.Df0.APP'%UID, 
      field= PolCal,
      append = ii>0,
      spw = str(spi[0]),
      scan = APPscans, 
      solint='inf',
      combine='obs,scan',
      preavg=scandur,
      poltype='Dflls',
      refant='', #solve absolute D-term
      smodel=[spi[1],spi[2],spi[3],0.0],
      gaintable=['%s.calibrated.ms.Gpol2.ALMA'%UID,
        '%s.calibrated.ms.XY0.APP'%UID])


#
# Allow applying solutions to the parallel hands too:
  if not Avoid_ALMA_PolCal:
   aph.Dgen(dtab='%s.calibrated.ms.Df0.ALMA'%UID,
    dout='%s.calibrated.ms.Df0gen.ALMA'%UID)

# Allow applying solutions to the parallel hands too:
  aph.Dgen(dtab='%s.calibrated.ms.Df0.APP'%UID,
    dout='%s.calibrated.ms.Df0gen.APP'%UID)

# amp-only and normalized, so only X/Y amp ratios matter
  if not Avoid_ALMA_PolCal:
    os.system('rm -rf  %s.calibrated.ms.Gxyamp.ALMA'%UID)
    gaincal(vis='%s.calibrated.ms'%UID,
        caltable='%s.calibrated.ms.Gxyamp.ALMA'%UID, 
        field=PolCal,
        scan = ALMAscans, 
        spw = '',
        solint='inf',
        combine='scan,obs',
        refant=REFANT,
        gaintype='G',
        smodel=S,
        calmode='a',
        gaintable=['%s.calibrated.ms.XY0.ALMA'%UID,
         '%s.calibrated.ms.Df0gen.ALMA'%UID],
        solnorm=T,
        parang=T)

# amp-only and normalized, so only X/Y amp ratios matter
  os.system('rm -rf  %s.calibrated.ms.Gxyamp.APP'%UID)
  gaincal(vis='%s.calibrated.ms'%UID,
        caltable='%s.calibrated.ms.Gxyamp.APP'%UID, 
        field=PolCal,
        scan = APPscans, 
        spw = '',
        solint='inf',
        combine='scan,obs',
        refant=REFANT,
        gaintype='G',
        smodel=S,
        calmode='a',
        gaintable=['%s.calibrated.ms.XY0.APP'%UID,
         '%s.calibrated.ms.Df0gen.APP'%UID],
        solnorm=T,
        parang=T)


  if doPlot:
   if not Avoid_ALMA_PolCal:
     plotcal('%s.calibrated.ms.Gxyamp.ALMA'%UID, 'antenna','amp', spw='',
        iteration='spw',subplot=221,showgui=showgui,
        figfile='%s.calibrated.ms.Gxyamp.ALMA.png'%UID)
     plotcal('%s.calibrated.ms.Gxyamp.ALMA'%UID, 'antenna','amp', spw='',poln='/',
        iteration='spw',subplot=221,showgui=showgui,
        figfile='%s.calibrated.ms.GxyampRatio.ALMA.png'%UID)

   plotcal('%s.calibrated.ms.Gxyamp.APP'%UID, 'antenna','amp', spw='',
        iteration='spw',subplot=221,showgui=showgui,
        figfile='%s.calibrated.ms.Gxyamp.APP.png'%UID)
   plotcal('%s.calibrated.ms.Gxyamp.APP'%UID, 'antenna','amp', spw='',poln='/',
        iteration='spw',subplot=221,showgui=showgui,
        figfile='%s.calibrated.ms.GxyampRatio.APP.png'%UID)

# Save D-term plots for all antennas:
   if not Avoid_ALMA_PolCal:
     plotcal('%s.calibrated.ms.Df0.ALMA'%UID,'chan','real', spw='',showgui=showgui,
       iteration='spw',subplot=221,figfile='%s.Df0.plot.REAL.ALMA.png'%UID)

     plotcal('%s.calibrated.ms.Df0.ALMA'%UID,'chan','imag', spw='',showgui=showgui,
       iteration='spw',subplot=221,figfile='%s.Df0.plot.IMAG.ALMA.png'%UID)


   plotcal('%s.calibrated.ms.Df0.APP'%UID,'chan','real', spw='',showgui=showgui,
       iteration='spw',subplot=221,figfile='%s.Df0.plot.REAL.APP.png'%UID)

   plotcal('%s.calibrated.ms.Df0.APP'%UID,'chan','imag', spw='',showgui=showgui,
       iteration='spw',subplot=221,figfile='%s.Df0.plot.IMAG.APP.png'%UID)





# Save flags before applycal
mystep = 16
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  if os.path.exists('%s.calibrated.ms.flagversions/flags.BeforeApplycal'%UID):
    flagmanager(vis='%s.calibrated.ms'%UID, 
      mode = 'delete', versionname='BeforeApplycal')

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeApplycal')




# Application of all the calibration tables
mystep = 17
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforeApplycal')



##########################
# SPECIFIC FOR VLBI
# POL. CALIBRATOR WITH NO D-TERMS (FOR CHECKING):
  if not Avoid_ALMA_PolCal:
   applycal(vis = '%s.calibrated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.calibrated.ms.Gpol2.ALMA'%UID,
      '%s.calibrated.ms.XY0.ALMA'%UID],
    gainfield = [PolCal,PolCal,'',''],
    interp = ['nearest','linear'],
    scan = ALMAscans,
    calwt = [T,F],
    parang = T,
    flagbackup = F)

  if Use_APP_for_Gpol:
    applycal(vis = '%s.calibrated.ms'%UID,
      field = PolCal,
      gaintable = ['%s.calibrated.ms.Gpol2.APP'%UID,
        '%s.calibrated.ms.XY0.APP'%UID],
      gainfield = [PolCal,PolCal,'',''],
      interp = ['nearest','linear'],
      scan = APPscans,
      calwt = [T,F],
      parang = T,
      flagbackup = F)
  else:
    applycal(vis = '%s.calibrated.ms'%UID,
      field = PolCal,
      gaintable = ['%s.calibrated.ms.Gpol2.ALMA'%UID,
        '%s.calibrated.ms.XY0.APP'%UID],
      gainfield = [PolCal,PolCal,'',''],
      interp = ['nearest','linear'],
      scan = APPscans,
      calwt = [T,F],
      parang = T,
      flagbackup = F)
##########################


  os.system('rm -rf %s.calibrated.PolCal.NoDterms.ms'%UID)
  split(vis = '%s.calibrated.ms'%UID,
    field = PolCal,
    datacolumn='corrected',
    outputvis = '%s.calibrated.PolCal.NoDterms.ms'%UID)


# POL. CALIBRATOR (ALMA):
  if not Avoid_ALMA_PolCal:
   applycal(vis = '%s.calibrated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.calibrated.ms.Gpol2.ALMA'%UID,
      '%s.calibrated.ms.XY0.ALMA'%UID,
      '%s.calibrated.ms.Df0gen.ALMA'%UID],
    gainfield = [PolCal,PolCal,PolCal],
    interp = ['nearest','linear','linear'],
    scan = ALMAscans,
    calwt = [T,F,F],
    parang = T,
    flagbackup = F)


##########################
# SPECIFIC FOR VLBI
# POL. CALIBRATOR (APP):
  if Use_APP_for_Gpol:
    applycal(vis = '%s.calibrated.ms'%UID,
      field = PolCal,
      gaintable = ['%s.calibrated.ms.Gpol2.APP'%UID,
        '%s.calibrated.ms.XY0.APP'%UID,
        '%s.calibrated.ms.Df0gen.APP'%UID],
      gainfield = [PolCal,PolCal,PolCal],
      interp = ['nearest','linear','linear'],
      scan = APPscans,
      calwt = [T,F,F],
      parang = T,
      flagbackup = F)
  else:
    applycal(vis = '%s.calibrated.ms'%UID,
      field = PolCal,
      gaintable = ['%s.calibrated.ms.Gpol2.ALMA'%UID,
        '%s.calibrated.ms.XY0.APP'%UID,
        '%s.calibrated.ms.Df0gen.APP'%UID],
      gainfield = [PolCal,PolCal,PolCal],
      interp = ['nearest','linear','linear'],
      scan = APPscans,
      calwt = [T,F,F],
      parang = T,
      flagbackup = F)
##########################


# OTHER SOURCES (ALMA):
  if not Avoid_ALMA_PolCal:
   applycal(vis = '%s.calibrated.ms'%UID,
    field = ','.join([GainCal,Target]+VLBICal),
    gaintable = ['%s.calibrated.ms.XY0.ALMA'%UID,
      '%s.calibrated.ms.Gxyamp.ALMA'%UID,
      '%s.calibrated.ms.Df0gen.ALMA'%UID],
    gainfield = [PolCal,PolCal,PolCal],
    interp = ['linear','nearest','linear'],
    scan = ALMAscans,
    calwt = [F,T,F],
    parang = T,
    flagbackup = F)


# OTHER SOURCES (APP):
  if Use_APP_for_Gpol:
    applycal(vis = '%s.calibrated.ms'%UID,
      field = ','.join([GainCal,Target]+VLBICal),
      gaintable = ['%s.calibrated.ms.XY0.APP'%UID,
        '%s.calibrated.ms.Gxyamp.APP'%UID,
        '%s.calibrated.ms.Df0gen.APP'%UID],
      gainfield = [PolCal,PolCal,PolCal],
      interp = ['linear','nearest','linear'],
      scan = APPscans,
      calwt = [F,T,F],
      parang = T,
      flagbackup = F)
  else:
    applycal(vis = '%s.calibrated.ms'%UID,
      field = ','.join([GainCal,Target]+VLBICal),
      gaintable = ['%s.calibrated.ms.XY0.APP'%UID,
        '%s.calibrated.ms.Gxyamp.ALMA'%UID,
        '%s.calibrated.ms.Df0gen.APP'%UID],
      gainfield = [PolCal,PolCal,PolCal],
      interp = ['linear','nearest','linear'],
      scan = APPscans,
      calwt = [F,T,F],
      parang = T,
      flagbackup = F)


# SPLIT ALMA AND APP SEPARATELY:
  os.system('rm -rf %s.polarization-calibrated.ALMA.ms'%UID)
  if not Avoid_ALMA_PolCal:
   split(vis = '%s.calibrated.ms'%UID,
    field = ','.join([PolCal,GainCal,Target]+VLBICal),
    datacolumn='corrected',
    antenna=','.join(phants),
    scan = ALMAscans,
    outputvis = '%s.polarization-calibrated.ALMA.ms'%UID)

  os.system('rm -rf %s.polarization-calibrated.APP.ms'%UID)
  split(vis = '%s.calibrated.ms'%UID,
    field = ','.join([PolCal,GainCal,Target]+VLBICal),
    datacolumn='corrected',
    antenna=','.join(phants),
    scan = APPscans,
    outputvis = '%s.polarization-calibrated.APP.ms'%UID)


# Save flags after applycal
mystep = 18
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)


  if os.path.exists('%s.calibrated.ms.flagversions/flags.AfterApplycal'%UID):
    flagmanager(vis='%s.calibrated.ms'%UID, 
      mode = 'delete', versionname='AfterApplycal')

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'save',
    versionname = 'AfterApplycal')


##########################
# SPECIFIC FOR VLBI
# image all sources
mystep = 19
if(mystep in thesteps):
   execfile(scriptForImaging)
   CLEAN_ALL(UID, '%s.CLEAN_IMAGES'%UID)
   
##########################
# SPECIFIC FOR VLBI
# Tar up deliverables for APP and package
mystep = 20
if(mystep in thesteps):
#  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
#  print 'Step ', mystep, step_title[mystep]
  msg = 'Step ' + str(mystep) + step_title[mystep]
  writeLog(msg)





# Get the Tsys information:
  NTSYS = 32 # Average the spectra to this number of channels
  tb.open('%s.concatenated.ms.calappphase'%UID)
  T0 = np.copy(tb.getcol('startValidTime'))
  T1 = np.copy(tb.getcol('endValidTime'))
  tb.close()


  os.system('rm -rf *.TSYS_MEDIAN.dat')

  for asd in ASDMs:

    print '\n\n    Getting Tsys from %s'%asd
    tb.open('%s.ms/SYSCAL'%asd)
    TSYS = tb.getcol('TSYS_SPECTRUM')
    Nchan = np.shape(TSYS)[1]
    ChPerIF = Nchan/NTSYS
    TIME = tb.getcol('TIME')
    SPW = tb.getcol('SPECTRAL_WINDOW_ID')
    ANTI = tb.getcol('ANTENNA_ID')
    tb.close()
    ALLSP = list(set(SPW))

    tb.open('%s.ms/ANTENNA'%asd)
    ANAME = tb.getcol('NAME')
    tb.close()


    tb.open('%s.concatenated.ms.calappphase'%UID)

    for ai,name in enumerate(ANAME):
      ifile = open('%s.TSYS_MEDIAN.dat'%name,'a')
      print >> ifile,'! ASDM: %s\n! Time (MJD) |  SPW  |  Phased? (1=Y) | TX (K) x%i | TY (K) x%i '%(os.path.basename(asd),NTSYS,NTSYS)
      Mask = ANTI==ai
      Tsys = []
      for j,spi in enumerate(ALLSP):
        Mask2 = SPW[Mask]==spi
        TEMP = np.copy(TSYS[:,:,Mask][:,:,Mask2])
        TX = [] ; TY = []
        for k in range(NTSYS):
          TX.append(np.median(TEMP[0,ChPerIF*k:ChPerIF*(k+1),:],axis=0))
          TY.append(np.median(TEMP[1,ChPerIF*k:ChPerIF*(k+1),:],axis=0))

        TX = np.array(TX)
        TY = np.array(TY)

        T = TIME[Mask][Mask2]
        for ti in range(len(T)):
          Tobs = np.where(np.logical_and(T[ti]>T0-60.,T[ti]<T1+60.0))[0]
          if len(Tobs)>0:
            isPhased = name in tb.getcell('phasedAntennas', rownr = Tobs[0])
            TXstr = ' '.join(['%.1f '%txi for txi in TX[:,ti]])
            TYstr = ' '.join(['%.1f '%tyi for tyi in TY[:,ti]])

            print >> ifile,'%.4f   |    %i  | %i              |  %s  |  %s '%(T[ti]/86400.,j,isPhased,TXstr,TYstr)

      ifile.close()  
       
    tb.close()

  os.system('rm -rf %s.TSYS_INFO'%UID)
  os.system('mkdir %s.TSYS_INFO'%UID)
  os.system('mv *.TSYS_MEDIAN.dat %s.TSYS_INFO'%UID)
  for asd in ASDMs:
    shutil.copytree('%s.ms/SYSCAL'%asd,'%s.TSYS_INFO/%s.SYSCAL'%(UID,asd))








# Copy ANTENNA table:
  if os.path.exists('%s.concatenated.ms.ANTENNA'%UID):
    os.system('rm -rf %s.concatenated.ms.ANTENNA'%UID)
  os .system('cp -r %s.concatenated.ms/ANTENNA %s.concatenated.ms.ANTENNA'%(UID,UID))

  README =  '# YOU SHOULD RUN POLCONVERT WITH THE FOLLOWING PARAMETERS:\n'
  README += '\ntget(polconvert)\n\n'
  README += 'IDI = \'\' # DIRECTORY WITH SWIN FILES (OR PATH TO FITS-IDI FILE)\n'
  README += 'OUTPUTIDI = \'\' # OUTPUT SWIN DIRECTORY (OR FITS-IDI FILE)\n'
  README += 'DiFXinput = \'\' # PATH TO AN *.input FILE, IF SWIN FILES ARE BEING CONVERTED\n'
  README += 'doIF = [] # LIST OF IFs TO PROCESS (EMPTY MEANS ALL)\n\n\n'
  README += '################################################################\n'
  README += '# SET THESE PARAMETERS FOR A DIAGNOSTIC PLOT OF THE CONVERSION #\n'
  README += '################################################################\n\n'
  README += 'plotIF = -1   # IF TO PLOT. SET IT TO MAKE A PLOT!\n'
  README += 'plotRange = [] # INTEGER LIST (AIPS TIMERANGE FORMAT).\n'
  README += '               # SET IT TO MAKE A PLOT!\n'
  README += 'plotAnt = -1  # THE OTHER ANTENNA IN THE BASELINE TO PLOT. SET IT!\n'
  README += 'doTest = True  # JUST PLOT (GOOD IDEA FOR THE FIRST RUN!).\n\n'
  README += '################################################################\n\n\n'
  README += 'linAntIdx = [1] # ASSUMES ALMA IS FIRST ANTENNA\n'
  README += 'Range = [] # TIME RANGE TO CONVERT (INTEGER LIST; AIPS FORMAT).\n'
  README += '           # LEAVE EMPTY TO CONVERT ALL DATA.\n\n'
  README += 'ALMAant = \'%s.concatenated.ms.ANTENNA\'\n'%UID
  README += 'calAPP = \'%s.concatenated.ms.calappphase\'\n\n'%UID
  README += 'spw = -1\n'
  README += 'calAPPTime = [0.0,8.0]\n\n'
  README += 'gains = [[\'%s.concatenated.ms.bandpass-zphs\',\n'%UID
  if Use_Tsys:
    README += '          \'%s.concatenated.ms.tsys\',\n'%UID
  README += '          \'%s.concatenated.ms.flux_inf.APP\',\n'%UID
  README += '          \'%s.concatenated.ms.phase_int.APP\',\n'%UID
  README += '          \'%s.calibrated.ms.XY0.APP\',\n'%UID
  README += '          \'%s.calibrated.ms.Gxyamp.APP\']]  # Check if \'calibrated.ms.Gxyamp.ALMA\' gives better results. \n\n'%UID
  README += 'dterms = [\'%s.calibrated.ms.Df0.APP\']  # Check if \'calibrated.ms.Df0.ALMA\' gives better results. \n\n'%UID
  README += 'amp_norm = True  # DON\'T APPLY AMPLITUDE CORRECTION.\n' 
  README += '                 # BUILD AN ANTAB FILE INSTEAD.\n'
  README += 'XYadd = [0.0] # CHANGE TO 180. IF R <-> L\n'
  README += 'swapXY = False # UNLIKELY TO CHANGE\n'
  README += 'swapRL = False # THIS IS FOR THE OTHER ANTENNA *IN THE PLOT*.\n'
  README += '               # I.E., THIS DOESN\'T CHANGE THE DATA\n'
  README += 'IDI_conjugated = True # JUST IF CONVERTING A FITS-IDI FILE.\n\n'
  README += 'go polconvert\n\n'

  rfile = open('README.POLCONVERT','w')
  print >> rfile, README
  rfile.close()

  import tarfile
  import glob
  deliverables = [
    'README.POLCONVERT',
    '%s.concatenated.ms.calappphase'%UID,
    '%s.concatenated.ms.ANTENNA'%UID,
    '%s.concatenated.ms.bandpass'%UID, 
    '%s.concatenated.ms.bandpass-zphs'%UID,
    '%s.concatenated.ms.flux_inf.APP'%UID,
    '%s.concatenated.ms.phase_int'%UID,
    '%s.concatenated.ms.phase_int.APP'%UID,
    '%s.calibrated.ms.Gpol1.scaled'%UID,
    '%s.calibrated.ms.Gpol2.APP'%UID,
    '%s.calibrated.ms.Gxyamp.APP'%UID,
    '%s.calibrated.ms.XY0amb.APP'%UID,  
    '%s.calibrated.ms.XY0.APP'%UID, 
    '%s.calibrated.ms.Df0.APP'%UID,
    '%s.calibrated.ms.Df0gen.APP'%UID]

  if Use_Tsys:
    deliverables.append('%s.concatenated.ms.tsys'%UID)

  if not Avoid_ALMA_PolCal:
    deliverables.append('%s.calibrated.ms.Df0gen.ALMA'%UID)
    deliverables.append('%s.calibrated.ms.Df0.ALMA'%UID)
    deliverables.append('%s.calibrated.ms.XY0.ALMA'%UID)
    deliverables.append('%s.calibrated.ms.XY0amb.ALMA'%UID)
    deliverables.append('%s.calibrated.ms.Gxyamp.ALMA'%UID)
    deliverables.append('%s.calibrated.ms.Gpol2.ALMA'%UID)


  if os.path.exists('%s.APP.artifacts'%UID):
    shutil.rmtree('%s.APP.artifacts'%UID)
  os.system('mkdir %s.APP.artifacts'%UID)
  os.system('mkdir %s.APP.artifacts/casalogs'%UID)
  os.system('mkdir %s.APP.artifacts/fluxscales'%UID)
  os.system('mkdir %s.APP.artifacts/listobs'%UID)
  for a in (glob.glob("*.png") + glob.glob("*.txt") +
            glob.glob("*.plots") + glob.glob("*.py")+glob.glob("*.LOG")):

    if os.path.isfile(a):
      shutil.copy(a, '%s.APP.artifacts/%s'%(UID,a))
    else:
      shutil.copytree(a, '%s.APP.artifacts/%s'%(UID,a))


  for a in glob.glob("*.log"):
      shutil.copy(a, '%s.APP.artifacts/casalogs/%s'%(UID,a))

  for a in (glob.glob("%s.CLEAN_IMAGES/*.txt"%UID) + glob.glob('*.fluxscale*') ):
      shutil.copy(a, '%s.APP.artifacts/fluxscales/%s'%(UID,os.path.basename(a)))

  for a in glob.glob("*.listobs"):
      shutil.copy(a, '%s.APP.artifacts/listobs/%s'%(UID,a))

  shutil.copytree('%s.TSYS_INFO'%UID,'%s.APP.artifacts/%s.TSYS_INFO'%(UID,UID))

  deliverables += ['%s.APP.artifacts'%UID]

  if os.path.exists('%s.APP_DELIVERABLES.tgz'%UID): 
    os.unlink('%s.APP_DELIVERABLES.tgz'%UID)

  tf = tarfile.open('%s.APP_DELIVERABLES.tgz'%UID, mode='w:gz')
  for d in deliverables: tf.add(d) 
  tf.close()

  print '\n\nTarball %s.APP_DELIVERABLES.tgz'%UID + ' is ready for delivery\n\n  NOW, MAKING QA2 PACKAGE\n'


#mystep = 21
#if(mystep in thesteps):

#######################################################################
# QA2 PACKAGE:

  print '\n   Generating common package contents'

  import inspect
  thisname = os.path.basename(inspect.getfile(inspect.currentframe()))

  os.system('rm -rf %s.QA2_PACKAGE'%UID)
  os.system('mkdir %s.QA2_PACKAGE'%(UID))
  os.system('mkdir %s.QA2_PACKAGE/%s.ALL'%(UID,UID))



#########################
# Calibration tables:
  os.system('mkdir %s.QA2_PACKAGE/%s.ALL/calibration'%(UID,UID))

  allcaltables = [
    '%s.concatenated.ms.bandpass'%UID, 
    '%s.concatenated.ms.bandpass-zphs'%UID,
    '%s.concatenated.ms.flux_inf.APP'%UID,
    '%s.concatenated.ms.flux_inf'%UID,
    '%s.concatenated.ms.phase_int'%UID,
    '%s.concatenated.ms.phase_int.APP'%UID,
    '%s.calibrated.ms.Gpol2.APP'%UID,
    '%s.calibrated.ms.Gxyamp.APP'%UID,
    '%s.calibrated.ms.XY0.APP'%UID, 
    '%s.calibrated.ms.Df0gen.APP'%UID]

  if Use_Tsys:
    allcaltables.append('%s.concatenated.ms.tsys'%UID)

  if not Avoid_ALMA_PolCal:
    allcaltables.append('%s.calibrated.ms.Df0gen.ALMA'%UID)
    allcaltables.append('%s.calibrated.ms.XY0.ALMA'%UID)

  tf = tarfile.open('%s.QA2_PACKAGE/%s.ALL/calibration/%s.calibration.tgz'%(UID,UID,UID), mode='w:gz')
  for d in allcaltables: tf.add(d)
  tf.close()

  os.system('mkdir %s.QA2_PACKAGE/%s.ALL/calibration/%s.calibration.plots'%(UID,UID,UID))
  for ff in glob.glob('*.plots'):
    shutil.copytree(ff, '%s.QA2_PACKAGE/%s.ALL/calibration/%s.calibration.plots/%s'%(UID,UID,UID,ff))

  for ff in glob.glob('*.png'):
    shutil.copy(ff, '%s.QA2_PACKAGE/%s.ALL/calibration/%s.calibration.plots/%s'%(UID,UID,UID,ff))



#########################
# LOGS:
  os.system('mkdir %s.QA2_PACKAGE/%s.ALL/logs'%(UID,UID))
  tf = tarfile.open('%s.QA2_PACKAGE/%s.ALL/logs/%s.log.tgz'%(UID,UID,UID), mode='w:gz')
  for d in glob.glob('*.log') + glob.glob('*.LOG'): tf.add(d)
  tf.close()




#########################
# CALIBRATION SCRIPTS:

  pyfile = open(QA2script).read()

  PROJLIST = eval(open(PROJECTS).read())
  ObsProj = {}
  for project in PROJLIST.keys():
    for i,asd1 in enumerate(ASDMs):
     for asd2 in PROJLIST[project]['ASDM']:
      if asd2 in asd1:
       if project not in ObsProj.keys():
         ObsProj[project] = []
       ObsProj[project].append(i)

  
  for proj in ObsProj.keys():

   print '\nGenerating package for %s'%proj

   packpath = '%s.QA2_PACKAGE/%s.package/sg_ouss_id/group_ouss_id/member_ouss_id'%(UID,proj)
   os.system('rm -rf %s'%packpath)
   shutil.copytree('%s.QA2_PACKAGE/%s.ALL'%(UID,UID),packpath)
   os.system('mkdir %s/scripts'%packpath)
   os.system('mkdir %s/products'%packpath)

# README:
   rfile = (open(APP_Readme).read()).replace('FLUXSOURCE',FluxCal).replace('PROJECT',proj).replace('TRACK',UID).replace('PINAME',PROJLIST[proj]['PI']).replace('PROJNAME',PROJLIST[proj]['TITLE'])
   ofile = open('%s/README_%s'%(packpath,UID),'w')
   print >> ofile, rfile
   ofile.close()


   AllFields = []

   for si, asdi in enumerate(ObsProj[proj]):
    asd = ASDMs[asdi]
    print 'Adding ASDM: %s'%asd 
    tb.open('%s.ms.split/STATE'%asd) 
    modes = tb.getcol('OBS_MODE')
    ALMAMode = [i for i in range(len(modes)) 
      if 'APPPHASE_ACTIVE' not in modes[i]]
    APPMode = [i for i in range(len(modes)) 
      if 'APPPHASE_ACTIVE' in modes[i]]
    tb.close()


    tb.open('%s.ms.split'%asd)
    states = tb.getcol('STATE_ID')
    scans = tb.getcol('SCAN_NUMBER')


    vset = set()
    for w in ALMAMode: 
      vset = vset.union(set(scans[np.where(states == w)]))
    ALMAscans = ','.join(map(str,list(vset)))

    vset = set()
    for w in APPMode: 
      vset = vset.union(set(scans[np.where(states == w)]))
    APPscans = ','.join(map(str,list(vset)))

#    raw_input("HOLD")

    if len(SPWs)==0:
      tb.open('%s.ms/SPECTRAL_WINDOW'%asd)
      spnames = tb.getcol('NAME')
      tb.close()
      tb.open('%s.ms/DATA_DESCRIPTION'%asd)
      poltypes = tb.getcol('POLARIZATION_ID')
      tb.close()
      tb.open('%s.ms/POLARIZATION'%asd)
      fpcol = np.where(tb.getcol('NUM_CORR')==4)[0]
      tb.close()
      spwi = ','.join([str(i) for i in range(len(poltypes)) if poltypes[i]==fpcol and 'FULL_RES' in spnames[i]])
    else:
      spwi = SPWs[si]

    ms.open('%s.ms.split'%asd) 
    summ = ms.summary()
    FieldNames = [summ[ai]['name'] for ai in summ.keys() if ai.startswith('field')]
    AllFields += FieldNames
    FieldStr = ','.join(['\'%s\''%ai for ai in FieldNames])
    ms.close()

    Phased = ','.join(phants)

    outscript = open('%s/scripts/%s.scriptForCalibration.py'%(packpath,asd),'w')
    cutline = pyfile.find('# IMAGING SCRIPT')

    print >> outscript, pyfile[:cutline].replace('SPWS','\'%s\''%spwi).replace('VISIB',asd).replace('ALMAscans','\'%s\''%ALMAscans).replace('APPscans','\'%s\''%APPscans).replace('UID',UID).replace('APPAnt',APPAnt).replace('ALLFIELDS',FieldStr).replace('Phased',Phased).replace('THISQA2',thisname)
    outscript.close()

#########################
# PRODUCTS:

    for field in FieldNames:
      for ff in glob.glob('%s.CLEAN_IMAGES/%s.*APP*.fits'%(UID,field.replace(' ','_'))):
        if not os.path.exists('%s/products/%s'%(packpath, os.path.basename(ff))):
          shutil.copy(ff, '%s/products/%s_%s'%(packpath, UID, os.path.basename(ff)))

#########################
# IMAGING SCRIPT:
   FieldStr = ','.join(['\'%s\''%ai for ai in list(set(AllFields))])
   outscript = open('%s/scripts/%s_scriptForImaging.py'%(packpath,UID),'w')
   AllVis = ','.join(['\'%s.polcalibrated.APP.ms\''%ASDMs[sdi] for sdi in ObsProj[proj]])
   print >> outscript, pyfile[cutline:].replace('VISIB',AllVis).replace('ALLFIELDS',FieldStr).replace('THISQA2',thisname)
   outscript.close()

# TAR THE PACKAGE:
   tf = tarfile.open('%s.QA2_PACKAGE/%s_%s.package.tgz'%(UID,proj,UID), mode='w:gz')
   tf.add('%s.QA2_PACKAGE/%s.package'%(UID,proj),arcname=proj)
   tf.close()

