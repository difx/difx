###########################################################
# ALMA Phasing Project (APP) QA2 Data Calibration Script
# EU-ARC (Nordic/Allegro Nodes) -- v 1.0 - August 16, 2017
#
# This script has been generated automatically from: 
#
#    THISQA2
###########################################################


thesteps = []
step_title = {0: ' Import of the ASDM',
              1: ' Fix of SYSCAL table times',
              2: ' Listobs and a-priori flagging',
              3: ' Split out science SPWs',
              4: ' Save original flags',
              5: ' Initial flagging',
              6: ' Apply ordinary calibration',
              7: ' Save flags after applycal'
              8: ' Split calibrated data',
              9: ' Apply polarization calibration (APP scans)', 
              10: ' Split corrected column (APP scans)',
              11: ' Save flags after polarization applycal'}

T = True
F = False


try:
  print 'List of steps to be executed ...', mysteps
  thesteps = mysteps
except:
  print 'global variable mysteps not set.'
if (thesteps==[]):
  thesteps = list(step_title.keys())   #range(0,len(step_title))
  print 'Executing all steps: ', thesteps


# CHECK CASA VERSION
isVer=False
for ver in ['4.7.0','4.7.1','4.7.2','5.0.0']:
  if ver in casadef.casa_version:
    isVer=True
    break

if not isVer: 
 sys.exit('ERROR: ONLY CASA VERSIONS   %s   ARE SUPPORTED!'%(', '.join(CASAVER)))



# Import of the ASDM
mystep = 0
if(mystep in thesteps):

  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  os.system('rm -rf VISIB.ms')
  os.system('rm -rf VISIB.ms.flagversions')
  importasdm(asdm='VISIB', vis='VISIB.ms', 
      asis='Antenna Station Receiver Source CalAtmosphere CorrelatorMode SBSummary CalAppPhase')



# Fix of SYSCAL table times
mystep = 1
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  from recipes.almahelpers import fixsyscaltimes
  fixsyscaltimes(vis = 'VISIB.ms')




# listobs and a-priori flagging:
mystep = 2
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  listobs(vis = 'VISIB.ms',
      listfile = 'VISIB.ms.listobs')

  flagdata(vis = 'VISIB.ms',
      mode = 'manual',
      spw = '',
      autocorr = T,
      flagbackup = F)
  
  flagdata(vis = 'VISIB.ms',
      mode = 'manual',
      intent = '*POINTING*,*SIDEBAND_RATIO*,*ATMOSPHERE*',
      flagbackup = F)
  
  
  flagcmd(vis = 'VISIB.ms',
      inpmode = 'table',
      useapplied = True,
      action = 'apply')




# Split:
mystep = 3
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  split(vis = 'VISIB.ms',
      outputvis = 'VISIB.ms.split',
      datacolumn='data',      
      spw = SPWS,
      keepflags = T)



# Save original flags
mystep = 4
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]
    
  if not os.path.exists('VISIB.ms.split.flagversions/flags.Original'):
    flagmanager(vis = 'VISIB.ms.split',
      mode = 'save',
      versionname = 'Original')
  else:
    flagmanager(vis = 'VISIB.ms.split',
      mode = 'restore',
      versionname = 'Original')



# Initial flagging
mystep = 5
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  flagmanager(vis = 'VISIB.ms.split',
    mode = 'restore',
    versionname = 'Original')


  # Flagging shadowed data
  flagdata(vis = 'VISIB.ms.split',
     mode = 'shadow',
     flagbackup = F)

  # Flagging autocorrelations
  flagdata(vis='VISIB.ms.split',
    autocorr = T,
    flagbackup = F)
  
  # Flagging "APP" antenna:
  flagdata(vis='VISIB.ms.split', 
    mode = 'manual', antenna = 'APPAnt',
    flagbackup = F)


  # Once all bad data are flagged, we save the flags:
  if os.path.exists('VISIB.ms.split.flagversions/flags.BeforeCalibration'):
    flagmanager(vis='VISIB.ms.split', 
      mode = 'delete', versionname='BeforeCalibration')

  flagmanager(vis='VISIB.ms.split', 
    mode = 'save', versionname='BeforeCalibration')




# Apply calibration:
mystep = 6
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  message = '\n\n\n     SOME CASA ERRORS MAY APPEAR SOON.\n    THEY ARE RELATED TO POSSIBLE MISSING SCANS IN APP/ALMA MODES.\n    THESE ERRORS SHOULD BE HARMLESS\n\n\n'
  casalog.post(message)
  print message

  for field in [ALLFIELDS]:

  # APP Scans:
    print 'Applying calibration to %s (APP Obs.)'%field
    applycal(vis = 'VISIB.ms.split',
      field = field,
      gaintable = ['UID.concatenated.ms.bandpass-zphs', 
      'UID.concatenated.ms.flux_inf.APP',
      'UID.concatenated.ms.phase_int.APP'],
      gainfield = ['',field,field],
      interp = ['linear','nearest','nearest'],
      scan = APPscans,
      antenna = 'Phased&',
      calwt = [T,T,F],
      parang = F,
      flagbackup = F)




# Save flags
mystep = 7
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]


  flagmanager(vis = 'VISIB.ms.split',
    mode = 'save',
    versionname = 'AfterApplycal')




# Split
mystep = 8
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  os.system('rm -rf VISIB.ms.split.cal*')
  split(vis = 'VISIB.ms.split',
    datacolumn='corrected',
    keepflags = False,
    antenna='Phased&',
    scan = APPscans,
    outputvis = 'VISIB.ms.split.cal')





# Apply polarization calibration:
mystep = 9
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  applycal(vis = 'VISIB.ms.split.cal',
      field = '',
      gaintable = ['UID.calibrated.ms.XY0.APP',
        'UID.calibrated.ms.Gxyamp.APP',
        'UID.calibrated.ms.Df0gen.APP'],
      interp = ['linear','nearest','linear'],
      calwt = [F,T,F],
      parang = T,
      flagbackup = F)




# Split
mystep = 10
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]

  os.system('rm -rf VISIB.polcalibrated.APP.ms')
  split(vis = 'VISIB.ms.split.cal',
    datacolumn='corrected',
    outputvis = 'VISIB.polcalibrated.APP.ms')




# Save flags
mystep = 11
if(mystep in thesteps):
  casalog.post('Step '+str(mystep)+' '+step_title[mystep],'INFO')
  print 'Step ', mystep, step_title[mystep]


  flagmanager(vis = 'VISIB.ms.split.cal',
    mode = 'save',
    versionname = 'AfterApplycal')












# IMAGING SCRIPT
# Run this script after a full-polarization calibration
# This script has been generated automatically from: 
#
#    THISQA2
###########################################################




for field in [ALLFIELDS]:
  for spi in range(4):
    os.system('rm -rf %s.APP.spw%i.*'%(field,spi))
    clean(vis= [VISIB],
        spw=str(spi),
        imagename = '%s.APP.spw%i'%(field,spi),
        field=field,
        mode='mfs',
        cell='0.2arcsec',
        imsize=256,
        outframe='BARY',
        stokes = 'IQUV',
        niter=100,
        mask='',
        interactive=F,
        pbcor=False,
        weighting='briggs',
        robust=0.5,
        phasecenter='')





