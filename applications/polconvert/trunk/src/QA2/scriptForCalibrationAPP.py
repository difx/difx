# ALMA Phasing Project (APP) QA2 Data Reduction Script
# Version 1.3 - I. Marti-Vidal (June 1, 2017)




########################################
# HEADER KEYWORDS TO RUN THE SCRIPT
#
#


# FORCE THIS CASA VERSION(S):
CASAVER = ['4.7.0','4.7.1','4.7.2']

#########################
# Give list of ASDMs:
asdmdir='/c3se/users/mivan/Hebbe/QA2/EHT-GMVA/Proj2016.1.01154.V_M87/calibration' # Root directory of ASDMs



ASDMs =[  'uid___A002_Xbebcb7_Xa7b',
  'uid___A002_Xbebcb7_Xb48',
  'uid___A002_Xbebcb7_Xcab',
  'uid___A002_Xbebcb7_Xf01',
  'uid___A002_Xbec3cb_X1084',
  'uid___A002_Xbec3cb_X666',
  'uid___A002_Xbec3cb_X921',
  'uid___A002_Xbec3cb_Xaa8',
  'uid___A002_Xbec3cb_Xcaa',
  'uid___A002_Xbec3cb_Xe97',
  'uid___A002_Xbeef47_X627',
  'uid___A002_Xbeef47_X827',
  'uid___A002_Xbeef47_Xa05',
  'uid___A002_Xbeef47_Xbe7']

#ASDMs = [ 'uid___A002_Xb187bd_X2d5', 
#         'uid___A002_Xb187bd_X4fe',  
#         'uid___A002_Xb187bd_X733',  
#         'uid___A002_Xb187bd_X863' ]  

 
#########################

# Unique identification label for the products:
#UID = 'uid___A002_Xbebcb7'
UID = 'uid___TEST'



# Whether to use System temperatures:
Use_Tsys = False


# Whether to use APP scans to calibrate X/Y amplitude 
# for pol. calibrator:
Use_APP_for_Gpol = True


# Plot tables?
doPlot = True


# Highest allowed inter-antenna Gpol dispersion:
BadGpol = 0.03

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
FluxCal = "3C279"

# Is the flux calibrator a Solar System Object?
IsPlanet = False  


# If a Solar-System Object is used:
JyStandard = 'Butler-JPL-Horizons 2012'


# If a quasar is used, instead, 
# set the flux density and spectral index, 
# as taken from the getALMAflux() function:

# aU.getALMAFlux(sourcename='3C279',frequency='214GHz',date='2017-04-05')

QuasarFlux = 8.615
QuasarSpix = -0.53
QuasarRefFreq = '214GHz'
#############################

# All these FIELD IDs must be strings. Empty
# strings ask the script to find them automatically:
BandPassCal = '0' # Field Id of the BP calibrator
PolCal = '4' # Field id of the Polarization calibrator
GainCal = '0' # Field id of the gain calibrator
VLBICal = ['2','3'] # Field ids of all the other calibrators
             # It must be a list of strings.
Target = '1' # Field id of main target.

VLBIfluxCal = ''  # If flux calibrator was not used in VLBI
                  # set this to one of the VLBICal sources
                  # Default means to take the polarization
                  # calibrator one with

# 

#######################################
#####################
# CHANNELS THAT JUMP 180 DEGREES IN X-Y PHASE, FOR EACH SPW,
# AT THE TELCAL'S REFERENCE ANTENNA:

# Stop the script when showing the XY-phase plots, to allow user 
# to find you the 180 deg. jumps. This should be set to TRUE 
# on the first time that this script is run:
checkJumps = True

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









# Calibration

thesteps = []
step_title = {0: 'Import of the ASDMs',
              1: 'Fix of SYSCAL table times',
              2: 'Listobs, get Tsys, and split ALMA-calibration scans (for ordinary QA2)',
              3: 'A priori flagging',
              4: 'Apply Tsys, split out science SPWs, concatenate, and build CALAPP table',
              5: 'Listobs and save original flags',
              6: 'Initial flagging',
              7: 'Putting a model for the flux calibrator(s)',
              8: 'Save flags before bandpass cal',
              9: 'Bandpass calibration',
              10: 'Save flags before gain cal',
              11: 'Gain calibration',
              12: 'Apply ordinary calibration',
              13: 'Split calibrated data',
              14: 'Save flags before polarization calibration',
              15: 'Polarization calibration',
              16: 'Save flags before applycal',
              17: 'Apply calibration and split corrected column',
              18: 'Save flags after applycal',
              19: 'Tar up APP deliverables'}

if not Use_Tsys:
  step_title[2] = 'Listobs and split ALMA-calibration scans (for ordinary QA2)'
  step_title[4] = 'Split out science SPWs, concatenate, and build CALAPP table'


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
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  from recipes.almahelpers import fixsyscaltimes
  for i,asd in enumerate(ASDMs):
    print 'Working out %s.ms'%asd
    fixsyscaltimes(vis = '%s.ms'%asd)




# listobs, get Tsys, and split ALMA-cal scans
mystep = 2
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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






# A priori flagging
mystep = 3
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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
      action = 'plot')
  
    flagcmd(vis = '%s.ms'%asd,
      inpmode = 'table',
      useapplied = True,
      action = 'apply')
  




# Apply Tsys, split out science SPWs, concatenate, and build CALAPP table
mystep = 4
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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
  if len(allrefs)>1:
    print "WARNING: THERE WAS MORE THAN ONE REFANT BY TELCAL: ",allrefs
  phants = set(tb.getcell('phasedAntennas', rownr = 0))
  almaref = []


  # Figure out which antennas are ALWAYS phased:
  nphant = tb.getcol('numPhasedAntennas')
  phants = set(tb.getcell('phasedAntennas', rownr = 0))
  for i in range(len(nphant)):
    aux = tb.getcell('phasedAntennas', rownr = i)
    phants = phants.intersection(aux)
    almaref.append(aux[appref[i]])

  phants = list(phants)

  tb.close()


  # if REFANT is not set, assign it to the APP refant:
  if len(REFANT)==0:
    REFANT = Counter(almaref).most_common()[0][0]
  message = '\n\n\n######################################\n' 
  message += "Will use %s as Reference Antenna.\n\n"%REFANT
  casalog.post(message)
  print message



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
  fields = tb.getcol('FIELD_ID')

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
  message += 'VLBI FLUXES WILL BE BOOTSTRAPPED FROM: %s\n\n'%VLBIfluxCalName
  message += '######################################\n\n\n' 

  print message
  casalog.post(message)

#################################################




print "# Calibration"

# Listobs and save original flags
mystep = 5
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  os.system('rm -rf %s.concatenated.ms.listobs'%UID)
  listobs(vis = '%s.concatenated.ms'%UID,
    listfile = '%s.concatenated.ms.listobs'%UID)
    
  if not os.path.exists('%s.concatenated.ms.flagversions/flags.Original'%UID):
    flagmanager(vis = '%s.concatenated.ms'%UID,
      mode = 'save',
      versionname = 'Original')



# Initial flagging
mystep = 6
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  flagmanager(vis='%s.concatenated.ms'%UID, mode = 'restore', versionname='BeforeCalibration')

  clearcal(vis='%s.concatenated.ms'%UID)

  if IsPlanet:
    setjy(vis='%s.concatenated.ms'%UID, usescratch=True, 
      field = FluxCal, standard = JyStandard)
  else:
    setjy(vis =  '%s.concatenated.ms'%UID,
      standard = 'manual',
      field = FluxCal,
      fluxdensity = QuasarFlux,
      spix = QuasarSpix,
      usescratch = True,
      reffreq = QuasarRefFreq)




# Save flags before bandpass cal
mystep = 8
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  if os.path.exists('%s.concatenated.ms.flagversions/flags.BeforeBandpassCalibration'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforeBandpassCalibration')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeBandpassCalibration')





# Bandpass calibration
mystep = 9
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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

  # Make a BP table with zero phases (APS TelCal ON)
  shutil.copytree('%s.concatenated.ms.bandpass'%UID, 
     '%s.concatenated.ms.bandpass-zphs'%UID)
  tb.open('%s.concatenated.ms.bandpass-zphs'%UID, nomodify=False)
  gains = tb.getcol('CPARAM')
  gains[:] = np.abs(gains)
  tb.putcol('CPARAM', gains)
  tb.close()




# Save flags before gain cal
mystep = 10
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
  if os.path.exists('%s.concatenated.ms.flagversions/flags.BeforeGainCalibration'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforeGainCalibration')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeGainCalibration')



# Gain calibration
mystep = 11
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  flagmanager(vis='%s.concatenated.ms'%UID, 
    mode = 'restore', versionname='BeforeGainCalibration')

  fields = ','.join([BandPassCal,PolCal,GainCal,Target]+VLBICal)
  # List of unique source ids:
  fields = ','.join(list(set(fields.split(','))))

  clearcal(vis = '%s.concatenated.ms'%UID,field=fields,addmodel=True)


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
  casalog.post('Re-referencing all phases to the REFANT','INFO')
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


################################
# CASA CRASHED WHEN TRYING TO APPEND THESE TO THE
# PREVIOUS TABLE (???). HENCE, WE HAVE TO CREATE A NEW
# TABLE FOR THE APP AMPLITUDE GAINS.

# Calibrate APP scans (amp):
  os.system('rm -rf %s.concatenated.ms.ampli_inf.APP'%UID)
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ampli_inf.APP'%UID,
#    caltable = '%s.concatenated.ms.ampli_inf'%UID,
#    append = True,
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




# Calibrate absolute flux density:
  os.system('rm -rf %s.concatenated.ms.flux_inf*'%UID) 
  os.system('rm -rf %s.concatenated.ms.fluxscale*'%UID) 
  mylogfile = casalog.logfile()
  casalog.setlogfile('%s.concatenated.ms.fluxscale'%UID)
  
  fluxscaleDict = fluxscale(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ampli_inf'%UID,
    fluxtable = '%s.concatenated.ms.flux_inf'%UID,
    transfer = '',
    reference = FluxCal) 




##################################################################
#############################
# THE FLUX CALIBRATOR USED BY ALMA MIGHT NOT BE OBSERVED IN APP MODE.
# WE NEED TO BROADCAST THE FLUX FROM THE GAIN CALIBRATOR INTO THE 
# APP AMPLITUDE TABLE. ONCE THE CASA CRASH PROBLEM ABOVE IS SOLVED,
# THESE LINES CAN BE COMMENTED OUT.
#############################

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
    flux = fluxscaleDict[GainCal][str(tspwids[entr])]['fluxd'][0]
    tgains[:,:,entr] /= np.sqrt(flux)

  tb.putcol('CPARAM',tgains)
  tb.close()

  fluxscaleDict = fluxscale(vis = '%s.concatenated.ms'%UID,
    caltable = 'Aux.tab',
    fluxtable = '%s.concatenated.ms.flux_inf.APP'%UID,
    transfer = '',
    reference = str(VLBIfluxCalName)) 

  casalog.setlogfile(mylogfile)

  os.system('rm -rf Aux.tab')

##################################################################





  if applyonly != True: 
    es.checkCalTable('%s.concatenated.ms.ampli_inf'%UID, 
      msName='%s.concatenated.ms'%UID, interactive=False) 

    es.checkCalTable('%s.concatenated.ms.flux_inf'%UID,
      msName='%s.concatenated.ms'%UID, interactive=False)

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
        iteration = 'spw', subplot = 411,
        figfile = '%s/%s.PHASE.png'%(dirname,antnam))

      plotcal(caltable = '%s.concatenated.ms.ampli_inf'%UID,
        xaxis = 'time', yaxis = 'amp',
        antenna = antnam, spw = '', 
        iteration = 'spw', subplot=411,
        figfile = '%s/%s.AMP.png'%(dirname,antnam))

      plotcal(caltable = '%s.concatenated.ms.flux_inf'%UID, 
        xaxis = 'time', yaxis = 'amp',
        antenna = antnam, spw = '', 
        iteration = 'spw', subplot = 411,
        figfile = '%s/%s.FLUX.png'%(dirname,antnam))

      plotcal(caltable = '%s.concatenated.ms.phase_int.APP'%UID,
        xaxis = 'time', yaxis = 'phase',
        antenna = antnam, spw = '', plotrange = [0,0,-180,180],
        iteration = 'spw', subplot = 411,
        figfile = '%s/%s.PHASE.APP.png'%(dirname,antnam))










# Apply calibration
mystep = 12
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]


  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforeApplycal')


# BANDPASS (ALMA):
  print 'Applying calibration to Bandpass (ALMA Obs.)'
  applycal(vis = '%s.concatenated.ms'%UID,
    field = BandPassCal,
    gaintable = ['%s.concatenated.ms.bandpass'%UID, 
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.phase_int'%UID],
    gainfield = [BandPassCal, BandPassCal,BandPassCal],
    interp = ['linear','nearest','nearest'],
    antenna = ','.join(phants)+'&',
    scan = ALMAscans,
    calwt = [T,T,F],
    parang = F,
    flagbackup = F)

# BANDPASS (APP):
  print 'Applying calibration to Bandpass (APP Obs.)'
  applycal(vis = '%s.concatenated.ms'%UID,
    field = BandPassCal,
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID, 
      '%s.concatenated.ms.flux_inf.APP'%UID,
      '%s.concatenated.ms.phase_int.APP'%UID],
    gainfield = [BandPassCal, BandPassCal,BandPassCal],
    interp = ['linear','nearest','nearest'],
    scan = APPscans,
    antenna = ','.join(phants)+'&',
    calwt = [T,T,F],
    parang = F,
    flagbackup = F)

# POL. CALIBRATOR (ALMA):
  print 'Applying calibration to Pol. calibrator (ALMA Obs.)'
  applycal(vis = '%s.concatenated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.concatenated.ms.bandpass'%UID,
      '%s.concatenated.ms.ampli_inf'%UID, 
      '%s.concatenated.ms.phase_int'%UID],
    gainfield = [BandPassCal, PolCal,PolCal],
    interp = ['linear','nearest','nearest'],
    scan = ALMAscans,
    antenna = ','.join(phants)+'&',
    calwt = [T,T,F],
    parang = F,
    flagbackup = F)

# POL. CALIBRATOR (APP):
  print 'Applying calibration to Pol. calibrator (APP Obs.)'
  applycal(vis = '%s.concatenated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID,
      '%s.concatenated.ms.ampli_inf.APP'%UID, 
      '%s.concatenated.ms.phase_int.APP'%UID],
    gainfield = [BandPassCal, PolCal,PolCal],
    interp = ['linear','nearest','nearest'],
    scan = APPscans,
    antenna = ','.join(phants)+'&',
    calwt = [T,T,F],
    parang = F,
    flagbackup = F)



# OTHER SOURCES: 
  for si in [GainCal,Target]+VLBICal:
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







# Split calibrated data (without the control antennas):
mystep = 13
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  os.system('rm -rf %s.calibrated.ms*'%UID)
  split(vis = '%s.concatenated.ms'%UID,
    datacolumn='corrected',
    keepflags = False,
    antenna=','.join(phants)+'&',
    outputvis = '%s.calibrated.ms'%UID)



# Save flags before polarization calibration
mystep = 14
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
  if os.path.exists('%s.calibrated.ms.flagversions/flags.BeforePolCal'%UID):
    flagmanager(vis='%s.calibrated.ms'%UID, 
      mode = 'delete', versionname='BeforePolCal')

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'save',
    versionname = 'BeforePolCal')







# Polarization calibration 
mystep = 15
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforePolCal')


  import recipes.almahelpers as ah
  import recipes.almapolhelpers as aph


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

  print 'Calibrating ALMA-only polarization data'

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
    gaincal(vis = '%s.calibrated.ms'%UID,
      caltable = '%s.calibrated.ms.Gpol1'%UID,
      field = PolCal,
      scan = ALMAscans,
      solint = 'inf',
      combine = '',
      gaintype = 'G',
      calmode = 'a',
      refant = REFANT)



# Pre-scaling Gpol solutions:
# (i.e., we multiply the gain of each antenna by its time average)

  os.system('rm -rf TEMP.Gpol1')
  os.system('cp -r %s.calibrated.ms.Gpol1 TEMP.Gpol1'%UID)

  tb.open('TEMP.Gpol1',nomodify = False)
  ants = tb.getcol('ANTENNA1')
  amps = tb.getcol('CPARAM')
  spwids = tb.getcol('SPECTRAL_WINDOW_ID')
  scannum = tb.getcol('SCAN_NUMBER')
  flagstat = tb.getcol('FLAG')

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
  #     print 'ANT ',ant,' SPW ',spw,' : ',avrat
       RatiosALMA[ant,spw] = avrat.real

  amprms = []
  scilist = list(set(scannum))
  for sci in scilist:
    rows = np.where(scannum==sci)
    amprms.append(np.std(1.-np.abs(amps[1,:,rows])/np.abs(amps[0,:,rows])))


  for scii,sci in enumerate(scilist):
    if amprms[scii] > BadGpol:
      rows = np.where(scannum==sci)
      flagstat[0,0,rows] = True
      flagstat[1,0,rows] = True

  tb.putcol('FLAG',flagstat)
  tb.putcol('CPARAM',amps)
  tb.close()



  if doPlot:
    os.system('rm -rf %s.calibrated.ms.Gpol1.png'%UID)
    plotcal('%s.calibrated.ms.Gpol1'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol1.png'%UID)

    os.system('rm -rf TEMP.Gpol1.png')
    plotcal('TEMP.Gpol1','scan','amp',field='',poln='/',subplot=111, figfile='TEMP.Gpol1.png')


#  raw_input("HOLD")

##########
# Save QU output in external file:
  print 'Estimating QU from gains'

  orig_stdout = sys.stdout
  f = open('%s.QUfromGain.txt'%UID, 'w')
  sys.stdout = f

# Rough estimate of QU:
  qu = aph.qufromgain('TEMP.Gpol1')

  sys.stdout = orig_stdout
  f.close()

  f = open('%s.QUfromGain.txt'%UID)
  print f.read()
  f.close()
##########


##################################################
# We search for the scan where the polarization signal is minimum in XX and YY
# (i.e., maximum in XY and YX):
#
  tb.open('TEMP.Gpol1')
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





# XY phase offset:
  os.system('rm -rf %s.calibrated.ms.XY0amb.ALMA'%UID)

  orig_stdout = sys.stdout
  f = open('%s.PolFromGaincal.txt'%UID, 'w')

  print >> f,"From ALMA Calibration Data:\n" 
  sys.stdout = f

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

  print >> f,"\nFrom APS-TelCal Calibrated Data:\n" 

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

  print 'END OF GAINCAL'


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


  tb.open('%s.calibrated.ms.XY0amb.ALMA'%UID,nomodify=False)
  spwids = tb.getcol('SPECTRAL_WINDOW_ID')
  data = tb.getcol('CPARAM')

  for spwi in [2]: #range(4):
    spmask = spwids == spwi
    spdata = data[:,:,spmask]
    for ji in XYJUMPS_ALMA[spwi]:
      data[0,ji:,spmask] *= -1.0

  tb.putcol('CPARAM',data)

  tb.close()


################


  sys.stdout = orig_stdout
  f.close()


  f = open('%s.PolFromGaincal.txt'%UID)
  print f.read()
  f.close()


# Solve QU ambiguity:
  os.system('rm -rf %s.calibrated.ms.XY0.APP'%UID)
  os.system('rm -rf %s.calibrated.ms.XY0.ALMA'%UID)

  orig_stdout = sys.stdout
  f = open('%s.XY-Ambiguity.txt'%UID, 'w')
  sys.stdout = f

  S=aph.xyamb(xytab='%s.calibrated.ms.XY0amb.ALMA'%UID, 
    qu=qu[qu.keys()[0]], xyout='%s.calibrated.ms.XY0.ALMA'%UID)

  S2=aph.xyamb(xytab='%s.calibrated.ms.XY0amb.APP'%UID, 
    qu=qu[qu.keys()[0]], xyout='%s.calibrated.ms.XY0.APP'%UID)


  sys.stdout = orig_stdout
  f.close()

  f = open('%s.XY-Ambiguity.txt'%UID)
  print f.read()
  f.close()

  if doPlot:
   plotcal('%s.calibrated.ms.XY0amb.ALMA'%UID,'freq','phase',
     antenna='0', poln='X', subplot=121,plotrange=[0,0,-180,180])

   plotcal('%s.calibrated.ms.XY0.ALMA'%UID,'freq','phase',
     antenna='0',poln='X',subplot=122,plotrange=[0,0,-180,180],
     figfile='%s.XY0_Amb-NoAmb.ALMA.png'%UID)

   plotcal('%s.calibrated.ms.XY0.ALMA'%UID,'chan','phase',
     antenna='0',poln='X',subplot=221,plotrange=[0,0,-180,180],
     iteration='spw',figfile='%s.XY-CrossPhase.ALMA.png'%UID)

   if checkJumps:
    raw_input('PLEASE, CHECK FOR POSSIBLE 180 DEG. JUMPS IN EACH SPW (Ctrl+D TO ABORT)')

   plotcal('%s.calibrated.ms.XY0amb.APP'%UID,'freq','phase',plotrange=[0,0,-180,180],
     antenna='0', poln='X', subplot=121)

   plotcal('%s.calibrated.ms.XY0.APP'%UID,'freq','phase',
     antenna='0',poln='X',subplot=122,plotrange=[0,0,-180,180],
     figfile='%s.XY0_Amb-NoAmb.APP.png'%UID)

   plotcal('%s.calibrated.ms.XY0.APP'%UID,'chan','phase',
     antenna='0',poln='X',subplot=221,plotrange=[0,0,-180,180],
     iteration='spw',figfile='%s.XY-CrossPhase.APP.png'%UID)


   if checkJumps:
    raw_input('PLEASE, CHECK FOR POSSIBLE 180 DEG. JUMPS IN EACH SPW (Ctrl+D TO ABORT)')



## Re-calibrate polarization calibrator (with right pol. model): 
  os.system('rm -rf %s.calibrated.ms.Gpol2*'%UID) 


# ALMA Calibration data:
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

  qu2 = aph.qufromgain('%s.calibrated.ms.Gpol2.ALMA'%UID)
  qu3 = aph.qufromgain('%s.calibrated.ms.Gpol2.APP'%UID)

  sys.stdout = orig_stdout
  f.close()

  f = open('%s.QUfromGain.txt'%UID)
  print f.read()
  f.close()
##########


  if doPlot:
   os.system('rm -rf %s.calibrated.ms.Gpol2.ALMA.png'%UID)
   plotcal('%s.calibrated.ms.Gpol2.ALMA'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol2.ALMA.png'%UID)



   os.system('rm -rf %s.calibrated.ms.Gpol2.APP.png'%UID)
   plotcal('%s.calibrated.ms.Gpol2.APP'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.calibrated.ms.Gpol2.APP.png'%UID)



# Plot RMS of gain ratios around 1.0:

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

  if doPlot:
   os.system('rm -rf %s.GainRatiosPol.png'%UID)
   pl.ioff()
   fig = pl.gcf()
   pl.clf() #pl.figure()
   sub = fig.add_subplot(111)
   sub.plot(scanlist, ratios, 'or', label = 'No Polcal')
   sub.plot(scanlist2, ratios2, 'ob', label = 'Polcal')
   sub.plot([scanlist[bestscidx]], [ratios[bestscidx]], 'xk', label = 'Best Pol. Scan')
   sub.set_xlabel('Scan Number')
   sub.set_ylabel('Gain Ratio RMS')
   pl.legend(numpoints=1)
   pl.savefig('%s.GainRatiosPol.png'%UID)
   pl.clf()

   pl.ion()


# Calibrate D-terms:


# ALMA:
  os.system('rm -rf %s.calibrated.ms.Df0*'%UID) 
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

## ALMA:
#  os.system('rm -rf %s.calibrated.APP.ms.Df0*'%UID)
  if Use_APP_for_Gpol:
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


#
# Allow applying solutions to the parallel hands too:
  aph.Dgen(dtab='%s.calibrated.ms.Df0.ALMA'%UID,
    dout='%s.calibrated.ms.Df0gen.ALMA'%UID)

# Allow applying solutions to the parallel hands too:
  aph.Dgen(dtab='%s.calibrated.ms.Df0.APP'%UID,
    dout='%s.calibrated.ms.Df0gen.APP'%UID)

# amp-only and normalized, so only X/Y amp ratios matter
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
   plotcal('%s.calibrated.ms.Gxyamp.ALMA'%UID, 'antenna','amp', spw='',
        iteration='spw',subplot=221,
        figfile='%s.calibrated.ms.Gxyamp.ALMA.png'%UID)
   plotcal('%s.calibrated.ms.Gxyamp.ALMA'%UID, 'antenna','amp', spw='',poln='/',
        iteration='spw',subplot=221,
        figfile='%s.calibrated.ms.GxyampRatio.ALMA.png'%UID)

   plotcal('%s.calibrated.ms.Gxyamp.APP'%UID, 'antenna','amp', spw='',
        iteration='spw',subplot=221,
        figfile='%s.calibrated.ms.Gxyamp.APP.png'%UID)
   plotcal('%s.calibrated.ms.Gxyamp.APP'%UID, 'antenna','amp', spw='',poln='/',
        iteration='spw',subplot=221,
        figfile='%s.calibrated.ms.GxyampRatio.APP.png'%UID)

# Save D-term plots for all antennas:
   plotcal('%s.calibrated.ms.Df0.ALMA'%UID,'chan','real', spw='',
       iteration='spw',subplot=221,figfile='%s.Df0.plot.REAL.ALMA.png'%UID)

   plotcal('%s.calibrated.ms.Df0.ALMA'%UID,'chan','imag', spw='',
       iteration='spw',subplot=221,figfile='%s.Df0.plot.IMAG.ALMA.png'%UID)


   plotcal('%s.calibrated.ms.Df0.APP'%UID,'chan','real', spw='',
       iteration='spw',subplot=221,figfile='%s.Df0.plot.REAL.APP.png'%UID)

   plotcal('%s.calibrated.ms.Df0.APP'%UID,'chan','imag', spw='',
       iteration='spw',subplot=221,figfile='%s.Df0.plot.IMAG.APP.png'%UID)





# Save flags before applycal
mystep = 16
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
  if os.path.exists('%s.calibrated.ms.flagversions/flags.BeforeApplycal'%UID):
    flagmanager(vis='%s.calibrated.ms'%UID, 
      mode = 'delete', versionname='BeforeApplycal')

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeApplycal')




# Application of all the calibration tables
mystep = 17
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]


  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforeApplycal')


# POL. CALIBRATOR WITH NO D-TERMS (FOR CHECKING):
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


  os.system('rm -rf %s.calibrated.PolCal.NoDterms.ms'%UID)
  split(vis = '%s.calibrated.ms'%UID,
    field = PolCal,
    datacolumn='corrected',
    outputvis = '%s.calibrated.PolCal.NoDterms.ms'%UID)


# POL. CALIBRATOR (ALMA):
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


# OTHER SOURCES (ALMA):
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
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  if os.path.exists('%s.calibrated.ms.flagversions/flags.AfterApplycal'%UID):
    flagmanager(vis='%s.calibrated.ms'%UID, 
      mode = 'delete', versionname='AfterApplycal')

  flagmanager(vis = '%s.calibrated.ms'%UID,
    mode = 'save',
    versionname = 'AfterApplycal')




# Tar up deliverables for APP
mystep = 19
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

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
  README += '          \'%s.calibrated.ms.Gxyamp.ALMA\']]  # Check if \'calibrated.ms.Gxyamp.APP\' gives better results. \n\n'%UID
  README += 'dterms = [\'%s.calibrated.ms.Df0.ALMA\']  # Check if \'calibrated.ms.Df0.APP\' gives better results. \n\n'%UID
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
    '%s.calibrated.ms.Gpol2.APP'%UID,
    '%s.calibrated.ms.Gpol2.ALMA'%UID,
    '%s.calibrated.ms.Gxyamp.APP'%UID,
    '%s.calibrated.ms.Gxyamp.ALMA'%UID,
    '%s.calibrated.ms.XY0amb.APP'%UID, 
    '%s.calibrated.ms.XY0amb.ALMA'%UID, 
    '%s.calibrated.ms.XY0.APP'%UID, 
    '%s.calibrated.ms.XY0.ALMA'%UID, 
    '%s.calibrated.ms.Df0.APP'%UID,
    '%s.calibrated.ms.Df0gen.APP'%UID,
    '%s.calibrated.ms.Df0.ALMA'%UID,
    '%s.calibrated.ms.Df0gen.ALMA'%UID]

  if Use_Tsys:
    deliverables.append('%s.concatenated.ms.tsys'%UID)


  if os.path.exists('%s.APP.artifacts'%UID):
    shutil.rmtree('%s.APP.artifacts'%UID)
  os.system('mkdir %s.APP.artifacts'%UID)
  for a in (glob.glob("*.png") +glob.glob("*.listobs") + glob.glob("*.txt") +
            glob.glob("*.plots") + glob.glob("*.py")):
    if os.path.isfile(a):
      shutil.copy(a, '%s.APP.artifacts/%s'%(UID,a))
    else:
      shutil.copytree(a, '%s.APP.artifacts/%s'%(UID,a))

  deliverables += ['%s.APP.artifacts'%UID]

  if os.path.exists('%s.APP_DELIVERABLES.tgz'%UID): 
    os.unlink('%s.APP_DELIVERABLES.tgz'%UID)

  tf = tarfile.open('%s.APP_DELIVERABLES.tgz'%UID, mode='w:gz')
  for d in deliverables: tf.add(d)
  tf.close()

  print '\n\nTarball %s.APP_DELIVERABLES.tgz'%UID + ' is ready for delivery\n'

