import numpy as np
import datetime as dt
import os

# Sample script to make rough images of all the sources observed in VLBI experiments,
# estimate their flux densities, and compare them to the ALMA monitoring database.
#C. Goddi - 2017-08-04 / IMV - 2017-08-10








def CLEAN_ALL(trackname='', CLEANDIR = '',cell='0.2arcsec',
             imagesize=256, niter = 100,
             cyclefactor = 0):  # Just a quick, rough (and fast) CLEAN

#################
# SCRIPT STARTS #
#################

  if len(CLEANDIR)==0:
    CLEANDIR = '%s.CLEAN_IMAGES'%trackname
 
  os.system('rm -rf %s'%CLEANDIR)
  os.system('mkdir %s'%CLEANDIR)

#input uv-files  
  visname_ALMA = '%s.polarization-calibrated.ALMA.ms'%trackname
  visname_APP = '%s.polarization-calibrated.APP.ms'%trackname
  visname = '%s.calibrated.ms'%trackname
#in principle only the last one is needed but using the first two is easier to select ALMA and APP scans, resp.


# Field names (only sources with observations):
  ms.open(visname) 
  summ = ms.summary()
  FieldNames = [summ[ai]['name'] for ai in summ.keys() if ai.startswith('field')]
  ms.close()



# Field names in ALMA mode (only sources with observations):
  if os.path.exists(visname_ALMA):
    ms.open(visname_ALMA)
    summ = ms.summary()
    FieldNamesALMA = [summ[ai]['name'] for ai in summ.keys() if ai.startswith('field')]
    ms.close()
  else:
    FieldNamesALMA = []


# Field names in APP mode (only sources with observations):
  ms.open(visname_APP) 
  summ = ms.summary()
  FieldNamesAPP = [summ[ai]['name'] for ai in summ.keys() if ai.startswith('field')]
  ms.close()


# Sources not calibrated in polarization:
  FieldNamesNoPol = filter(lambda x: x not in FieldNamesAPP and x not in FieldNamesALMA, FieldNames)


# Observing frequency:
  tb.open(visname+'/SPECTRAL_WINDOW')
  AvgFreq = np.average(tb.getcol('REF_FREQUENCY'))/1.e9
  tb.close()



# Observing date:
  tb.open(visname+'/OBSERVATION')
  JDTime = int(round(np.average(tb.getcol('TIME_RANGE'))/86400.+2400000.5) - 2451545)
  obsday = dt.date(2000,1,1) + dt.timedelta(JDTime)
  obsdate = '%i-%02i-%02i'%(obsday.year,obsday.month,obsday.day)


  message =  '\n   WARNING: THE QUANTITIES GIVEN IN THIS FILE ARE ROUGH ESTIMATES'
  message += '\n            ONLY MEANT FOR ASSESSMENT PURPOSES. THEY SHOULD *NOT*'
  message += '\n            BE USED IN A SCIENTIFIC PUBLICATION'
  message += '\n\n  Observations took place on %s at %.1f GHz\n'%(obsdate,AvgFreq)
  message +=  '\nSources in APP mode: \n\n %s'%('\n '.join(FieldNamesAPP))
  message += '\n\nSources in ALMA mode: \n\n %s'%('\n '.join(FieldNamesALMA))
  message +=  '\n\nAdditional sources (not pol. calibrated): \n\n %s\n\n'%('\n '.join(FieldNamesNoPol))

  print message
  f = open('%s.FluxesAll.txt'%trackname, 'w')

  f.write(message)
  f.flush()

#raw_input('HOLD')


  import analysisUtils as au

  FluxesALMA = {}
  FluxesAPP = {}
  FluxesALL = {}



  for i in xrange(len(FieldNames)):
    FName = FieldNames[i].replace(" ","_")
    print 'Doing field ', FieldNames[i]
    isALMA = False; isAPP = False

    os.system('rm -rf %s.*'%FName)
#Image only the ALMA scans (if present)

    if FieldNames[i] in FieldNamesALMA:
      isALMA = True
      FluxesALMA[str(FieldNames[i])] = {}
      for spi in range(4):
     #   if False:
        clean(vis=visname_ALMA,
          spw=str(spi),
          cyclefactor = cyclefactor,
          imagename = '%s.spw%i.ALMA'%(FName,spi),
          field=FieldNames[i],
          mode='mfs',
          cell=cell,
          imsize=imagesize,
          outframe='BARY',
          stokes = 'IQUV',
          niter=niter,
          mask='',
          interactive=F,
          pbcor=False,
          weighting='briggs',
          robust=0.5,
          phasecenter='')

# Get the total CLEANed flux and polarization:
        try:
          ia.open('%s.spw%i.ALMA.model'%(FName,spi))
          imdata = ia.getchunk()
          if np.shape(imdata)[2]>1:
            CFlux = [np.sum(imdata[:,:,j,:]) for j in range(4)]
          else:
            CFlux = [np.sum(imdata),0.,0.,0.]

          ia.close() ; del imdata
          ia.open('%s.spw%i.ALMA.residual'%(FName,spi))
          imdata = ia.getchunk()
          if np.shape(imdata)[2]>1:
            CRMS = [np.std(imdata[:,:,j,:]) for j in range(4)]
          else:
            CRMS = [np.std(imdata),0.,0.,0.]
          P = np.sqrt(CFlux[1]**2. + CFlux[2]**2.)/CFlux[0]
          ia.close(); del imdata
          exportfits(imagename='%s.spw%i.ALMA.image'%(FName,spi),
                 fitsimage = '%s.spw%i.ALMA.image.fits'%(FName,spi))
        except:
          print 'NO VALID ALMA IMAGE FOR THIS SOURCE!!'
          CFlux = [0.,0.,0.,0.] ; CRMS = [0.,0.,0.,0.]; P = 0.0

        PAng = np.arctan2(CFlux[2],CFlux[1])*0.5*180./np.pi
        FluxesALMA[str(FieldNames[i])][spi] = (spi,CFlux[0],CRMS[0],P,PAng,CFlux[3],CRMS[3])
        os.system('mv %s.spw%i.ALMA.* %s/.'%(FName,spi,CLEANDIR))


#Image only the APP scans
    if FieldNames[i] in FieldNamesAPP:
      isAPP = True
      FluxesAPP[str(FieldNames[i])] = {}
      for spi in range(4):
   #     if False:
        clean(vis=visname_APP,
          spw=str(spi),
          cyclefactor = cyclefactor,
          imagename = '%s.spw%i.APP'%(FName,spi),
          field=FieldNames[i],
          mode='mfs',
          cell=cell,
          imsize=imagesize,
          stokes = 'IQUV',
          outframe='BARY',
          niter=niter,
          mask='',
          interactive=F,
          pbcor=False,
          weighting='briggs',
          robust=0.5,
          phasecenter='')


# Get the total CLEANed flux and polarization:
        try:
          ia.open('%s.spw%i.APP.model'%(FName,spi))
          imdata = ia.getchunk()
          if np.shape(imdata)[2]>1:
            CFlux = [np.sum(imdata[:,:,j,:]) for j in range(4)]
          else:
            CFlux = [np.sum(imdata), 0., 0., 0.]

          ia.close() ; del imdata
          ia.open('%s.spw%i.APP.residual'%(FName,spi))
          imdata = ia.getchunk()
          if np.shape(imdata)[2]>1:
            CRMS = [np.std(imdata[:,:,j,:]) for j in range(4)]
          else:
            CRMS = [np.std(imdata), 0., 0., 0.]
          P = np.sqrt(CFlux[1]**2. + CFlux[2]**2.)/CFlux[0]
          ia.close(); del imdata
          exportfits(imagename='%s.spw%i.APP.image'%(FName,spi),
                 fitsimage = '%s.spw%i.APP.image.fits'%(FName,spi))
        except:
          print 'NO VALID APP IMAGE FOR THIS SOURCE!!'
          CFlux = [0.,0.,0.,0.] ; CRMS = [0.,0.,0.,0.]; P = 0.0

        PAng = np.arctan2(CFlux[2],CFlux[1])*0.5*180./np.pi
        FluxesAPP[str(FieldNames[i])][spi] = (spi,CFlux[0],CRMS[0],P,PAng,CFlux[3],CRMS[3])
        os.system('mv %s.spw%i.APP.* %s/.'%(FName,spi,CLEANDIR))

#Image both the ALMA+APP scans
    FluxesALL[str(FieldNames[i])] = {}
    for spi in range(4):
    #  if False:
      clean(vis=visname,
        spw=str(spi),
        scan = '',
        cyclefactor = cyclefactor,
        imagename = '%s.ALL.spw%i'%(FName,spi),
        field=FieldNames[i],
        mode='mfs',
        cell=cell,
        imsize=imagesize,
        outframe='BARY',
        stokes = 'IQUV',
        niter=niter,
        mask='',
        interactive=F,
        pbcor=False,
        weighting='briggs',
        robust=0.5,
        phasecenter='')


# Get the total CLEANed flux and polarization:
      try:
        ia.open('%s.ALL.spw%i.model'%(FName,spi))
        imdata = ia.getchunk()
        if np.shape(imdata)[2]>1:
          CFlux = [np.sum(imdata[:,:,j,:]) for j in range(4)]
        else:
          CFlux = [np.sum(imdata), 0., 0., 0.]

        ia.close() ; del imdata
        ia.open('%s.ALL.spw%i.residual'%(FName,spi))
        imdata = ia.getchunk()
        if np.shape(imdata)[2]>1:
          CRMS = [np.std(imdata[:,:,j,:]) for j in range(4)]
        else:
          CRMS = [np.std(imdata), 0., 0., 0.]
        P = np.sqrt(CFlux[1]**2. + CFlux[2]**2.)/CFlux[0]
        ia.close(); del imdata
        exportfits(imagename='%s.ALL.spw%i.image'%(FName,spi),
                 fitsimage = '%s.ALL.spw%i.image.fits'%(FName,spi))
      except:
        print 'NO VALID TOTAL IMAGE FOR THIS SOURCE!!'
        CFlux = [0.,0.,0.,0.] ; CRMS = [0.,0.,0.,0.]; P = 0.0
     
      PAng = np.arctan2(CFlux[2],CFlux[1])*0.5*180./np.pi
      FluxesALL[str(FieldNames[i])][spi] = (spi,CFlux[0],CRMS[0],P,PAng,CFlux[3],CRMS[3])
      os.system('mv %s.ALL.spw%i.* %s/.'%(FName,spi,CLEANDIR))


#Check the fluxes from ALMA calibrators catalog
    print 'Retrieving flux density of ', FieldNames[i]
    a = au.getALMAFlux(str(FieldNames[i]),AvgFreq,date=obsdate)
    f2 = open(FName+'.getALMAFlux.txt', 'w')
    f2.write(str(a))

    f.write('\nSOURCE %s:\n\n'%str(FieldNames[i]))
    if type(a) is dict and 'fluxDensity' in a.keys():
      print >> f,  '  FROM ALMA DATABASE:  I: %.3e +/- %.1e Jy'%(a['fluxDensity'],a['fluxDensityUncertainty'])
    if isAPP:
      print >> f,'\n  FROM APP IMAGE: \n'
      for spi in range(4):   
        print >> f,'  SPW %i  =>  I: %.3e +/- %.1e Jy | P: %.3f , %.1f deg. | V: %.3e +/- %.1e Jy'%FluxesAPP[str(FieldNames[i])][spi]

    if isALMA:
      print >> f,'\n  FROM ALMA IMAGE: \n'
      for spi in range(4):
        print >> f,'  SPW %i  =>  I: %.3e +/- %.1e Jy | P: %.3f , %.1f deg. | V: %.3e +/- %.1e Jy'%FluxesALMA[str(FieldNames[i])][spi]

    print >> f,  '\n  FROM TOTAL IMAGE: \n'
    for spi in range(4):
      print >> f,  '  SPW %i  =>  I: %.3e +/- %.1e Jy | P: %.3f , %.1f deg. | V: %.3e +/- %.1e Jy'%FluxesALL[str(FieldNames[i])][spi]

    f2.close()
    os.system('mv %s.getALMAFlux.txt %s/.'%(FName,CLEANDIR))
    f.flush()

  f.close()
  os.system('mv %s.FluxesAll.txt %s/.'%(trackname,CLEANDIR))

