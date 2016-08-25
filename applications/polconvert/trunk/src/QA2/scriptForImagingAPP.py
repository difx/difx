import re






# FORCE THIS CASA VERSION:
CASAVER = '4.6.0'


VIS = 'uid___A002_Xb187bd.calibrated.ALMA.ms'

VISNODT = 'uid___A002_Xb187bd.calibrated.PolCal.NoDterms.ALMA.ms'

# SOURCE INDICES AND NAMES:
SOURCES = {'POLCAL' : '0'}
           'GAINCAL' : '1',
           'VLBI_CAL1':'2',
           'VLBI_CAL2':'3',
           'VLBI_CAL3':'4'}
           'TARGET' : '5'}

CELL = '0.07arcsec'
IMSIZE = 350
NITER = 1000
NPERCYCLE = 500
THRESHOLD = '' # '0.1mJy'

if re.search('^%s'%CASAVER, casadef.casa_version) == None:
 sys.exit('ERROR: PLEASE USE THE SAME VERSION OF CASA THAT YOU USED FOR GENERATING THE SCRIPT: %s'%CASAVER)



if True:

 print "# Imaging Pol. Calibrator with no Dterms."

 imname = 'POLCAL.NODT'
 os.system('rm -rf %s*'%imname)
 clean(vis = VISNODT,
    imagename = imname,
    field = '',
    niter = NITER,
    npercycle = NPERCYCLE,
    cyclefactor = 0.0,
    threshold = THRESHOLD,
    outframe = 'lsrk',
    spw = '0,1,2,3',
    mode = 'mfs',
    nterms = 1,
    psfmode='clarkstokes',
    stokes='IQUV',
    interactive = F,
    imsize = [IMSIZE, IMSIZE],
    cell = CELL,
    weighting = 'briggs',
    robust = 0.5)

 immath(imagename=imname+'.image', outfile=imname+'.I.image',expr='IM0',stokes='I')
 immath(imagename=imname+'.image', outfile=imname+'.Q.image',expr='IM0',stokes='Q')
 immath(imagename=imname+'.image', outfile=imname+'.U.image',expr='IM0',stokes='U')

 immath(outfile=imname+'.P.image',
       mode='poli',
       imagename=[imname+'.Q.image',imname+'.U.image'],
       sigma='0.0Jy/beam')

 calstat=imstat(imagename='%s.P.image'%imname, axes=[0,1])
 rms=(calstat['rms'])[0]
 os.system('rm -rf %s.ANG.image'%imname)
 immath(outfile=imname+'.ANG.image',
       mode='pola',
       imagename=[imname+'.Q.image',imname+'.U.image'],
       polithresh='%.16fJy/beam'%(3.*rms))

 impbcor(imagename=imname+'.image', pbimage=imname+'.flux', outfile=imname+'.image.pbcor', overwrite=True)
 exportfits(imagename=imname+'.image.pbcor', fitsimage=imname+'.image.pbcor.fits')
 exportfits(imagename=imname+'.flux', fitsimage=imname+'.flux.fits')





for source in SOURCES.keys():
  print 'CLEANING ',source

  imnameNoStokes =source
  imname = imnameNoStokes+'.IQUV'
  print "Imaging field (imagename):", imname
  os.system('rm -rf %s.*'%imnameNoStokes)
  clean(vis = VIS,
    imagename = imname,
    field = SOURCES[source], 
    niter = NITER,
    npercycle = NPERCYCLE,
    cyclefactor = 0.0,
    threshold = THRESHOLD,
    outframe = 'lsrk',
    spw = '0,1,2,3',
    mode = 'mfs',
    nterms = 1,
    psfmode='clarkstokes',
    stokes='IQUV', 
    interactive = F,
    imsize = [IMSIZE, IMSIZE],
    cell = CELL,
    weighting = 'briggs',
    robust = 0.5)

  immath(imagename=imname+'.image', outfile=imnameNoStokes+'.I.image',expr='IM0',stokes='I')
  immath(imagename=imname+'.image', outfile=imnameNoStokes+'.Q.image',expr='IM0',stokes='Q')
  immath(imagename=imname+'.image', outfile=imnameNoStokes+'.U.image',expr='IM0',stokes='U')
  immath(outfile=imnameNoStokes+'.P.image',
       mode='poli',
       imagename=[imnameNoStokes+'.Q.image',imnameNoStokes+'.U.image'],
       sigma='0.0Jy/beam')

  calstat=imstat(imagename='%s.residual'%imname, axes=[0,1])
  rms=(calstat['rms'])
  calstat=imstat(imagename='%s.image'%imname, axes=[0,1])
  peak=(calstat['max'])

  print 'rms',rms
  print 'peak',peak
  print 'DR', peak/rms

  calstat=imstat(imagename='%s.P.image'%imnameNoStokes, axes=[0,1])
  rms=(calstat['rms'])[0]
  os.system('rm -rf %s.ANG.image'%imnameNoStokes)
  immath(outfile=imnameNoStokes+'.ANG.image',
       mode='pola',
       imagename=[imnameNoStokes+'.Q.image',imnameNoStokes+'.U.image'],
       polithresh='%.16fJy/beam'%(3.*rms))


  impbcor(imagename=imname+'.image', pbimage=imname+'.flux', outfile=imname+'.image.pbcor', overwrite=True) 
  exportfits(imagename=imname+'.image.pbcor', fitsimage=imname+'.image.pbcor.fits')
  exportfits(imagename=imname+'.flux', fitsimage=imname+'.flux.fits')



