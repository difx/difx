# ALMA Phasing Project (APP) QA2 Data Reduction Script
# Version 1.0 - I. Marti-Vidal (August 25, 2016)




########################################
# DEFINITIONS TO RUN THE SCRIPT
#
#


# FORCE THIS CASA VERSION:
CASAVER = '4.6.0'

#########################
# Give list of ASDMs:
asdmdir='/mnt/bure_1/arcshared/APP_B3/CALIBRATION' # Root directory of ASDMs
ASDMs = [ 'uid___A002_Xb542b2_X1a4', 
         'uid___A002_Xb542b2_X395',  
         'uid___A002_Xb542b2_X459',  
         'uid___A002_Xb542b2_X5f2' ]  
#########################

# Unique identification label for the products:
UID = 'uid___A002_Xb542b2'


# The four science spws, for each ASDM:
SPWs = ['17,19,21,23',
        '17,19,21,23',
        '9,11,13,15',
        '9,11,13,15']



# Name of "antenna" with APP signal
# (to be flagged in the calibration)
APPAnt = "DV03"


# Reference antenna (set it to '' for using the same as APP TelCal):
REFANT = ""


#############################
# Absolute flux calibrator:
FluxCal = "J0423-0120"

# Is the flux calibrator a Solar System Object?
IsPlanet = False  


# If a Solar-System Object is used:
JyStandard = 'Butler-JPL-Horizons 2012'


# If a quasar is used, instead, 
# set the flux density and spectral index, 
# as taken from the getALMAflux() function:
QuasarFlux = 0.914
QuasarSpix = -0.775
QuasarRefFreq = '100GHz'
#############################

BandPassCal = '0' # Name, or index, of the BP calibrator
PolCal = '2' # Name, or index, of the Polarization calibrator
GainCal = '4' # Name, or index, of the gain calibrator
VLBICal = '3,5,7,8' # Name, or index, of all the other calibrators
Target = '6' # Name, or index, of main target.



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
XYJUMPS_APP = [[], # SPW0
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
# END OF DEFINITIONS
######################################################




# Calibration

thesteps = []
step_title = {0: 'Import of the ASDMs',
              1: 'Fix of SYSCAL table times',
              2: 'Listobs and split ALMA-calibration scans (for ordinary QA2)',
              3: 'A priori flagging',
              4: 'Split out science SPWs, concatenate, and build CALAPP table',
              5: 'Listobs and save original flags',
              6: 'Initial flagging',
              7: 'Putting a model for the flux calibrator(s)',
              8: 'Save flags before bandpass cal',
              9: 'Bandpass calibration',
              10: 'Save flags before gain cal',
              11: 'Gain calibration',
              12: 'Save flags before polarization calibration',
              13: 'Polarization calibration',
              14: 'Save flags before applycal',
              15: 'Apply calibration and split corrected column',
              16: 'Save flags after applycal',
              17: 'Tar up APP deliverables'}



if 'applyonly' not in globals(): applyonly = False
try:
  print 'List of steps to be executed ...', mysteps
  thesteps = mysteps
except:
  print 'global variable mysteps not set.'
if (thesteps==[]):
  thesteps = range(0,len(step_title))
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
import numpy as np
import pylab as pl

if applyonly != True: es = aU.stuffForScienceDataReduction() 


if re.search('^%s'%CASAVER, casadef.casa_version) == None:
 sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: %s'%CASAVER)




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




# listobs and split ALMA-cal scans
mystep = 2
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  os.system('rm -rf %s.ALMA_CALIB_DATA'%UID)
  os.system('mkdir %s.ALMA_CALIB_DATA'%UID)

  os.system('rm -rf %s.APP_CALIB_DATA'%UID)
  os.system('mkdir %s.APP_CALIB_DATA'%UID)

  for i,asd in enumerate(ASDMs):
    os.system('rm -rf %s.ms.listobs'%asd)
    listobs(vis = '%s.ms'%asd,
      listfile = '%s.ms.listobs'%asd)

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





# Split out science SPWs, concatenate, and build CALAPP table
mystep = 4
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  for i,asd in enumerate(ASDMs):
    os.system('rm -rf %s.ms.split'%asd) 
    os.system('rm -rf %s.ms.split.flagversions'%asd) 
    split(vis = '%s.ms'%asd,
      outputvis = '%s.ms.split'%asd,
      datacolumn = 'data',
      spw = SPWs[i],
      keepflags = T)

    # Collecting data from CALAPPPHASE into one single table
    if i==0:
      os.system('cp -rf %s.ms.split/ASDM_CALAPPPHASE ./%s.calappphase'%(asd,UID))
    else:
      tb.open(asd+'.ms.split/ASDM_CALAPPPHASE')
      tb.copyrows('./%s.calappphase'%UID)
      tb.close()
  os.rename('./%s.calappphase'%UID, '%s.concatenated.ms.calappphase'%UID)

  concat(vis=['%s.ms.split'%asd for asd in ASDMs],
         concatvis='%s.concatenated.ms'%UID,
         timesort = True,
         copypointing = False)




#################################################
# THIS CODE WILL ALWAYS BE RUN
# IF THERE IS AT LEAST ONE STEP > 4

if sum([sti>4 for sti in thesteps])>0:

  from collections import Counter

  # Figure out the (most used) APS-TelCal reference antenna:
  tb.open('%s.concatenated.ms.calappphase'%UID)

  rfants = Counter(tb.getcol('refAntennaIndex'))
  REFANTidx = rfants.most_common()[0][0]

  # Figure out which antennas are ALWAYS phased:
  nphant = tb.getcol('numPhasedAntennas')
  phants = set(tb.getcell('phasedAntennas', rownr = 0))
  for i in range(1,len(nphant)):
    phants = phants.intersection(tb.getcell('phasedAntennas', rownr = i))
  phants = list(phants)

  tb.close()

  # Get list of all antennas:
  tb.open('%s.concatenated.ms/ANTENNA'%UID)
  allants = tb.getcol('NAME')
  tb.close()

  # if REFANT is not set, assign it to the APP refant:
  if len(REFANT)==0:
    REFANT = allants[REFANTidx]
  print "Will use %s as Reference Antenna."%REFANT

  # Build a list of observing modes including APP / ALMA mode:
  tb.open('%s.concatenated.ms/STATE'%UID)
  modes = tb.getcol('OBS_MODE')

  APPMode = [i for i in range(len(modes)) 
    if 'APPPHASE_ACTIVE' in modes[i]]

  ALMAMode = [i for i in range(len(modes)) 
    if 'APPPHASE_ACTIVE' not in modes[i]]

  tb.close()

  # Build a list of active phasing scans:
  tb.open('%s.concatenated.ms'%UID)
  states = tb.getcol('STATE_ID')
  scans = tb.getcol('SCAN_NUMBER')
  vset = set()
  for w in APPMode: 
    vset = vset.union(set(scans[np.where(states == w)]))
  APPscans = ','.join(map(str,list(vset)))
  tb.close()

  # Build a list of ALMA-calibration scans:
  vset = set()
  for w in ALMAMode: 
    vset = vset.union(set(scans[np.where(states == w)]))
  ALMAscans = ','.join(map(str,list(vset)))


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
    
  if not os.path.exists('%s.concatenated.ms.flagversions/Original.flags'%UID):
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
  if os.path.exists('%s.concatenated.ms.flagversions/BeforeCalibration.flags'%UID):
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
    setjy(vis='%s.concatenated.ms'%UID, 
      field = FluxCal, standard = JyStandard)
  else:
    setjy(vis =  '%s.concatenated.ms'%UID,
      standard = 'manual',
      field = FluxCal,
      fluxdensity = QuasarFlux,
      spix = QuasarSpix,
      reffreq = QuasarRefFreq)




# Save flags before bandpass cal
mystep = 8
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  if os.path.exists('%s.concatenated.ms.flagversions/BeforeBandpassCalibration.flags'%UID):
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
    combine = 'scan',
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
  if os.path.exists('%s.concatenated.ms.flagversions/BeforeGainCalibration.flags'%UID):
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

  fields = ','.join([BandPassCal,PolCal,GainCal,VLBICal,Target])
  # List of unique source ids:
  fields = ','.join(list(set(fields.split(','))))



# Calibrate ALMA scans (phase):
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


# Calibrate ALMA scans (amp):
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


# Calibrate APP scans (amp):
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ampli_inf'%UID,
    field = '',
    scan = APPscans,
    append = T,
    solint = 'inf',
    combine = '',
    refant = REFANT,
    gaintype = 'T',
    calmode = 'a',
    gainfield = [BandPassCal,''],
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID,'%s.concatenated.ms.phase_int.APP'%UID])

# Calibrate absolute flux density:
  os.system('rm -rf %s.concatenated.ms.flux_inf*'%UID) 
  os.system('rm -rf %s.concatenated.ms.fluxscale*'%UID) 
  mylogfile = casalog.logfile()
  casalog.setlogfile('%s.concatenated.ms.fluxscale'%UID)
  
  fluxscaleDict = fluxscale(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.ampli_inf'%UID,
    fluxtable = '%s.concatenated.ms.flux_inf'%UID,
    reference = FluxCal)

  casalog.setlogfile(mylogfile)

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




# Save flags before polarization calibration
mystep = 12
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
  if os.path.exists('%s.concatenated.ms.flagversions/BeforePolCal.flags'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforePolCal')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforePolCal')




# Polarization calibration 
mystep = 13
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforePolCal')

  import recipes.almahelpers as ah
  import recipes.almapolhelpers as aph


###########################
# Figure out typical scan duration of the polcalib scans:

  ms.open('%s.concatenated.ms'%UID)
  summary = ms.summary()
  ms.close()

  tb.open('%s.concatenated.ms'%UID)
  scnum = tb.getcol('SCAN_NUMBER')
  stid = tb.getcol('STATE_ID')
  tb.close()

  tb.open('%s.concatenated.ms/STATE'%UID)
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
  os.system('rm -rf %s.concatenated.ms.Gpol1*'%UID)
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.Gpol1'%UID,
    scan = ALMAscans,
    field = PolCal,
    solint = 'inf',
    combine = '',
    calmode = 'a',
    refant = REFANT,
    gaintable = ['%s.concatenated.ms.bandpass'%UID,
        '%s.concatenated.ms.phase_int'%UID, '%s.concatenated.ms.flux_inf'%UID])


#  print 'Calibrating APP polarization data'

# APP Data:
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.Gpol1.APP'%UID,
    scan = APPscans,
    field = PolCal,
    solint = 'inf',
    combine = '',
    calmode = 'a',
    refant = REFANT,
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID,
       '%s.concatenated.ms.phase_int.APP'%UID,'%s.concatenated.ms.flux_inf'%UID])


  os.system('rm -rf %s.concatenated.ms.Gpol1.png'%UID)
  plotcal('%s.concatenated.ms.Gpol1'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.concatenated.ms.Gpol1.png'%UID)

  os.system('rm -rf %s.concatenated.ms.Gpol1.APP.png'%UID)
  plotcal('%s.concatenated.ms.Gpol1.APP'%UID,'scan','amp',field='',poln='/',subplot=111, figfile='%s.concatenated.ms.Gpol1.APP.png'%UID)



##########
# Save QU output in external file:
  print 'Estimating QU from gains'

  orig_stdout = sys.stdout
  f = open('%s.QUfromGain.txt'%UID, 'w')
  sys.stdout = f

# Rough estimate of QU:
  qu = aph.qufromgain('%s.concatenated.ms.Gpol1'%UID)

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
  tb.open('%s.concatenated.ms.Gpol1'%UID)
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



# FLAG SHADOWING (we can do it now):
  flagdata(vis = '%s.concatenated.ms'%UID,
    mode = 'shadow',
    flagbackup = F)



# XY phase offset:
  os.system('rm -rf %s.concatenated.ms.XY0amb.ALMA'%UID)

  orig_stdout = sys.stdout
  f = open('%s.PolFromGaincal.txt'%UID, 'w')

  print >> f,"From ALMA Calibration Data:\n" 
  sys.stdout = f

  gaincal(vis='%s.concatenated.ms'%UID,
    caltable='%s.concatenated.ms.XY0amb.ALMA'%UID, 
    field= PolCal,
    scan = ALMAscans,
    gaintype='XYf+QU',
    solint='inf',
    combine='scan,obs',
    preavg=scandur, 
    refant=refant,
    smodel=[1,0,1,0],
    gaintable=['%s.concatenated.ms.bandpass'%UID,
      '%s.concatenated.ms.phase_int'%UID,
      '%s.concatenated.ms.flux_inf'%UID])

  print 'END OF GAINCAL'

  print >> f,"\nFrom APS-TelCal Calibrated Data:\n" 

  os.system('rm -rf %s.concatenated.ms.XY0amb.APP'%UID)

  gaincal(vis='%s.concatenated.ms'%UID,
    caltable='%s.concatenated.ms.XY0amb.APP'%UID, 
    field= PolCal,
    scan = APPscans,
    gaintype='XYf+QU',
    solint='inf',
    combine='scan,obs',
    preavg=scandur, 
    refant=refant,
    smodel=[1,0,1,0],
    gaintable=['%s.concatenated.ms.bandpass-zphs'%UID,
      '%s.concatenated.ms.phase_int.APP'%UID,
      '%s.concatenated.ms.flux_inf'%UID])

  print 'END OF GAINCAL'


################
# CORRECT 180 DEG. AMBIGUITIES WITHIN EACH SPW:

  tb.open('%s.concatenated.ms.XY0amb.APP'%UID,nomodify=False)
  spwids = tb.getcol('SPECTRAL_WINDOW_ID')
  data = tb.getcol('CPARAM')

  for spwi in range(4):
    spmask = spwids == spwi
    spdata = data[:,:,spmask]
    for ji in XYJUMPS_APP[spwi]:
      data[0,ji:,spmask] *= -1.0

  tb.putcol('CPARAM',data)

  tb.close()


  tb.open('%s.concatenated.ms.XY0amb.ALMA'%UID,nomodify=False)
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
  os.system('rm -rf %s.concatenated.ms.XY0.ALMA'%UID)
  os.system('rm -rf %s.concatenated.ms.XY0.APP'%UID)

  orig_stdout = sys.stdout
  f = open('%s.XY-Ambiguity.txt'%UID, 'w')
  sys.stdout = f

  S=aph.xyamb(xytab='%s.concatenated.ms.XY0amb.ALMA'%UID, 
    qu=qu[qu.keys()[0]], xyout='%s.concatenated.ms.XY0.ALMA'%UID)

  S2=aph.xyamb(xytab='%s.concatenated.ms.XY0amb.APP'%UID, 
    qu=qu[qu.keys()[0]], xyout='%s.concatenated.ms.XY0.APP'%UID)


  sys.stdout = orig_stdout
  f.close()

  f = open('%s.XY-Ambiguity.txt'%UID)
  print f.read()
  f.close()

  plotcal('%s.concatenated.ms.XY0amb.ALMA'%UID,'freq','phase',
     antenna='0', poln='X', subplot=121)

  plotcal('%s.concatenated.ms.XY0.ALMA'%UID,'freq','phase',
     antenna='0',poln='X',subplot=122,
     figfile='%s.XY0_Amb-NoAmb.ALMA.png'%UID)

  plotcal('%s.concatenated.ms.XY0.ALMA'%UID,'chan','phase',
     antenna='0',poln='X',subplot=221,
     iteration='spw',figfile='%s.XY-CrossPhase.ALMA.png'%UID)

  if checkJumps:
    raw_input('PLEASE, CHECK FOR POSSIBLE 180 DEG. JUMPS IN EACH SPW')

  plotcal('%s.concatenated.ms.XY0amb.APP'%UID,'freq','phase',
     antenna='0', poln='X', subplot=121)

  plotcal('%s.concatenated.ms.XY0.APP'%UID,'freq','phase',
     antenna='0',poln='X',subplot=122,
     figfile='%s.XY0_Amb-NoAmb.APP.png'%UID)

  plotcal('%s.concatenated.ms.XY0.APP'%UID,'chan','phase',
     antenna='0',poln='X',subplot=221,
     iteration='spw',figfile='%s.XY-CrossPhase.APP.png'%UID)


  if checkJumps:
    raw_input('PLEASE, CHECK FOR POSSIBLE 180 DEG. JUMPS IN EACH SPW')



## Re-calibrate polarization calibrator (with right pol. model): 
  os.system('rm -rf %s.concatenated.ms.Gpol2*'%UID) 


# ALMA Calibration data:
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.Gpol2'%UID,
    scan = ALMAscans,
    field = PolCal,
    solint = 'inf',
    combine = '',
    calmode = 'a',
    smodel = S,
    refant = REFANT,
    parang = T,
    gaintable = ['%s.concatenated.ms.bandpass'%UID,
      '%s.concatenated.ms.phase_int'%UID,'%s.concatenated.ms.flux_inf'%UID])

# APP Data:
  gaincal(vis = '%s.concatenated.ms'%UID,
    caltable = '%s.concatenated.ms.Gpol2.APP'%UID,
    scan = APPscans,
    field = PolCal,
    solint = 'inf',
    combine = '',
    calmode = 'a',
    smodel = S,
    parang = T,
    refant = REFANT,
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID,
      '%s.concatenated.ms.phase_int.APP'%UID,'%s.concatenated.ms.flux_inf'%UID])


##########
## Check for any residual polarization signal:

  orig_stdout = sys.stdout
  f = open('%s.QUfromGain.txt'%UID, 'a')
  f.write('\n\n USING POLCAL MODEL:\n\n')
  sys.stdout = f

  qu2 = aph.qufromgain('%s.concatenated.ms.Gpol2'%UID)

  sys.stdout = orig_stdout
  f.close()

  f = open('%s.QUfromGain.txt'%UID)
  print f.read()
  f.close()
##########



# Plot RMS of gain ratios around 1.0:

  tb.open('%s.concatenated.ms.Gpol2'%UID)
  scans2 = tb.getcol('SCAN_NUMBER')
  gains = np.squeeze(tb.getcol('CPARAM'))
  tb.close()
  scanlist2 = np.array(list(set(scans2)))
  ratios2 = np.zeros(len(scanlist2))
  for si, s in enumerate(scanlist2):
    filt = scans2 == s
    ratio = np.sqrt(np.average(np.power(np.abs(gains[0,filt])/np.abs(gains[1,filt])-1.0,2.)))
    ratios2[si] = ratio


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
  os.system('rm -rf %s.concatenated.ms.Df0*'%UID) 
  polcal(vis='%s.concatenated.ms'%UID,
    caltable='%s.concatenated.ms.Df0'%UID, 
    field= PolCal,
    scan = ALMAscans, 
    solint='inf',
    combine='obs,scan',
    preavg=scandur,
    poltype='Dflls',
    refant='', #solve absolute D-term
    smodel=S,
    gaintable=['%s.concatenated.ms.bandpass'%UID,
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.XY0.ALMA'%UID,
      '%s.concatenated.ms.phase_int'%UID])




# Save D-term plots for all antennas:
  plotcal('%s.concatenated.ms.Df0'%UID,'chan','real', spw='',
       iteration='spw',subplot=221,figfile='%s.Df0.plot.REAL.png'%UID)

  plotcal('%s.concatenated.ms.Df0'%UID,'chan','imag', spw='',
       iteration='spw',subplot=221,figfile='%s.Df0.plot.IMAG.png'%UID)


# Allow applying solutions to the parallel hands too:
  aph.Dgen(dtab='%s.concatenated.ms.Df0'%UID,
    dout='%s.concatenated.ms.Df0gen'%UID)





# Save flags before applycal
mystep = 14
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
  if os.path.exists('%s.concatenated.ms.flagversions/BeforeApplycal.flags'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='BeforeApplycal')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'BeforeApplycal')



# Application of all the calibration tables
mystep = 15
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]


  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'restore',
    versionname = 'BeforeApplycal')


# BANDPASS (ALMA):
  applycal(vis = '%s.concatenated.ms'%UID,
    field = BandPassCal,
    gaintable = ['%s.concatenated.ms.bandpass'%UID, 
      '%s.concatenated.ms.phase_int'%UID, 
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.XY0.ALMA'%UID,
      '%s.concatenated.ms.Df0gen'%UID],
    gainfield = [BandPassCal, PolCal, PolCal, '',''],
    interp = ['linear','nearest','nearest','linear','linear'],
    scan = ALMAscans,
    calwt = [T,F,T,F,F],
    parang = T,
    flagbackup = F)

# POL. CALIBRATOR (ALMA) WITH NO D-TERMS (FOR CHECKING):
  applycal(vis = '%s.concatenated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.concatenated.ms.bandpass'%UID,
      '%s.concatenated.ms.flux_inf'%UID, 
      '%s.concatenated.ms.phase_int'%UID, 
      '%s.concatenated.ms.XY0.ALMA'%UID],
    gainfield = [BandPassCal,PolCal,PolCal,PolCal,''],
    interp = ['linear','nearest','nearest','linear'],
    scan = ALMAscans,
    calwt = [T,T,F,F],
    parang = T,
    flagbackup = F)

  os.system('rm -rf %s.calibrated.PolCal.NoDterms.ALMA.ms'%UID)
  split(vis = '%s.concatenated.ms'%UID,
    field = PolCal,
    datacolumn='corrected',
    scan = ALMAscans,
    outputvis = '%s.calibrated.PolCal.NoDterms.ALMA.ms'%UID)


# POL. CALIBRATOR (ALMA):
  applycal(vis = '%s.concatenated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.concatenated.ms.bandpass'%UID,
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.phase_int'%UID,
      '%s.concatenated.ms.XY0.ALMA'%UID,
      '%s.concatenated.ms.Df0gen'%UID],
    gainfield = [BandPassCal,PolCal,PolCal,PolCal,'',''],
    interp = ['linear','nearest','nearest','linear','linear'],
    scan = ALMAscans,
    calwt = [T,T,F,F,F],
    parang = T,
    flagbackup = F)





# POL. CALIBRATOR (APP):
  applycal(vis = '%s.concatenated.ms'%UID,
    field = PolCal,
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID, 
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.phase_int.APP'%UID, 
      '%s.concatenated.ms.XY0.APP'%UID,
      '%s.concatenated.ms.Df0gen'%UID],
    gainfield = ['',PolCal,PolCal,PolCal,'',''],
    interp = ['linear','nearest','nearest','linear','linear'],
    scan = APPscans,
    calwt = [T,T,F,F,F],
    parang = T,
    flagbackup = F)


# OTHER SOURCES (ALMA):
  applycal(vis = '%s.concatenated.ms'%UID,
    field = ','.join([GainCal,VLBICal,Target]),
    gaintable = ['%s.concatenated.ms.bandpass'%UID, 
      '%s.concatenated.ms.phase_int'%UID, 
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.XY0.ALMA'%UID,
      '%s.concatenated.ms.Df0gen'%UID],
    gainfield = [BandPassCal,'','','','',''],
    interp = ['linear','nearest','nearest','linear','linear'],
    scan = ALMAscans,
    calwt = [T,F,T,F,F],
    parang = T,
    flagbackup = F)


# OTHER SOURCES (APP):
  applycal(vis = '%s.concatenated.ms'%UID,
    field = ','.join([GainCal,VLBICal,Target]),
    gaintable = ['%s.concatenated.ms.bandpass-zphs'%UID, 
      '%s.concatenated.ms.phase_int.APP'%UID, 
      '%s.concatenated.ms.flux_inf'%UID,
      '%s.concatenated.ms.XY0.APP'%UID,
      '%s.concatenated.ms.Df0gen'%UID],
    gainfield = [BandPassCal,'','','','',''],
    interp = ['linear','nearest','nearest','linear','linear'],
    scan = APPscans,
    calwt = [T,F,T,F,F],
    parang = T,
    flagbackup = F)

# SPLIT ALMA AND APP SEPARATELY:
  os.system('rm -rf %s.calibrated.ALMA.ms'%UID)
  split(vis = '%s.concatenated.ms'%UID,
    field = ','.join([BandPassCal,PolCal,GainCal,VLBICal,Target]),
    datacolumn='corrected',
    scan = ALMAscans,
    outputvis = '%s.calibrated.ALMA.ms'%UID)

  os.system('rm -rf %s.calibrated.APP.ms'%UID)
  split(vis = '%s.concatenated.ms'%UID,
    field = ','.join([PolCal,GainCal,VLBICal,Target]),
    datacolumn='corrected',
    scan = APPscans,
    outputvis = '%s.calibrated.APP.ms'%UID)


# Save flags after applycal
mystep = 16
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
  if os.path.exists('%s.concatenated.ms.flagversions/AfterApplycal.flags'%UID):
    flagmanager(vis='%s.concatenated.ms'%UID, 
      mode = 'delete', versionname='AfterApplycal')

  flagmanager(vis = '%s.concatenated.ms'%UID,
    mode = 'save',
    versionname = 'AfterApplycal')




# Tar up deliverables for APP
mystep = 17
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

# Copy ANTENNA table:
  if os.path.exists('%s.concatenated.ms.ANTENNA'%UID):
    os.system('rm -rf %s.concatenated.ms.ANTENNA'%UID)
  os .system('cp -r %s.concatenated.ms/ANTENNA %s.concatenated.ms.ANTENNA'%(UID,UID))

  import tarfile
  import glob
  deliverables = [
    '%s.concatenated.ms.calappphase'%UID,
    '%s.concatenated.ms.ANTENNA'%UID,
    '%s.concatenated.ms.bandpass'%UID, 
    '%s.concatenated.ms.bandpass-zphs'%UID,
    '%s.concatenated.ms.flux_inf'%UID, 
    '%s.concatenated.ms.Gpol2'%UID,
    '%s.concatenated.ms.phase_int'%UID,
    '%s.concatenated.ms.phase_int.APP'%UID,
    '%s.concatenated.ms.XY0amb.ALMA'%UID, 
    '%s.concatenated.ms.XY0amb.APP'%UID, 
    '%s.concatenated.ms.XY0.ALMA'%UID, 
    '%s.concatenated.ms.XY0.APP'%UID, 
    '%s.concatenated.ms.Df0'%UID,
    '%s.concatenated.ms.Df0gen'%UID]

  #deliverables += glob.glob("*.png")
  #deliverables += glob.glob("*.txt")
  #deliverables += glob.glob("*.plots")
  #deliverables += glob.glob("*.py")
  # this is perhaps a bit cleaner for the correlator
  os.system('mkdir %s.concatenated.ms.artifacts'%UID)
  for a in (glob.glob("*.png") + glob.glob("*.txt") +
            glob.glob("*.plots") + glob.glob("*.py")):
    os.rename(a, '%s.concatenated.ms.artifacts/%s'%(UID,a))
  deliverables += '%s.concatenated.ms.artifacts'%UID

  if os.path.exists('%s.APP_DELIVERABLES.tgz'%UID): 
    os.unlink('%s.APP_DELIVERABLES.tgz'%UID)

  tf = tarfile.open('%s.APP_DELIVERABLES.tgz'%UID, mode='w:gz')
  for d in deliverables: tf.add(d)
  tf.close()

  print '\n\nTarball %s.APP_DELIVERABLES.tgz'%UID + ' is ready for delivery\n'

