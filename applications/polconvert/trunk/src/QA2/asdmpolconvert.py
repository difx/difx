#!/usr/bin/python
#
# This script is to be run to import the ASDMs and
# prepare a MS with only the required data.  (This
# is more efficient in the long run since many bands
# are not needed.)  There is some chance this script
# can just be run from casa with prepared arguments.
# Otherwise, it can be cut and pasted....
#

import os

### code here has only been run cut&paste into a casa session,
### however it might work unattended if there are no suprises.

### the "msname" should be unique and correspond to a project
# msname = 'concat.ms'
# msname = 'end2end.ms'

CALAPP = 'calappphase.tab'

### 1mm Apr08 2016 commissioning session
# asdmdir='/alma/data/alma-apr08-2016'
# ASDM = [ 'uid___A002_Xb187bd_X2d5',
#          'uid___A002_Xb187bd_X4fe',
#          'uid___A002_Xb187bd_X733',
#          'uid___A002_Xb187bd_X863' ]
### 3mm Jul10 2016 commissioning session
# asdmdir='/alma/data/alma-jul09-2016'
# ASDM = [ 'uid___A002_Xb542b2_X1a4',
#          'uid___A002_Xb542b2_X395',
#          'uid___A002_Xb542b2_X459',
#          'uid___A002_Xb542b2_X5f2' ]

try:
    if not (band3 or band6):
        raise Exception, 'Exactly one of "band3" or "band6" must True'
    if not os.path.exists(asdmdir):
        raise Exception, 'No directory corresponding to "asdmdir"'
    if not type(ASDM) is list or len(ASDM) < 1:
        raise Exception, 'ASDM must be a list of exported uids'
    if not type(msname) is str:
        raise Exception, '"msname" must be defined for the project'
    if os.path.exists(msname) and os.path.isdir(msname):
        raise Exception, ('%s exists...remove it or change names'%msname)
    if os.path.exists(CALAPP) and os.path.isdir(CALAPP):
        raise Exception, ('%s exists...remove it'%CALAPP)
except Exception, ex:
    raise ex

# Iterate over ASDMs and import to MS:
for i,asd in enumerate(ASDM):
  print 'Working out ASDM #%i - %s'%(i+1,asd)
  os.system('rm -rf %s.ms' % asd)
  os.system('rm -rf %s.ms.flagversions' % asd)
  importasdm(asdm=os.path.join(asdmdir, asd), vis=asd+'.ms', asis='CalAppPhase')

### identify the SPWs used for the recorded bands
# band3 uses the first vlbi window
# band6 uses the third and fourth vlbi window

### this was the 1mm result (expected to be typical, but not guaranteed)
# VSPW = [ [17, 19, 21, 23],
#          [9, 11, 13, 15],
#          [9, 11, 13, 15],
#          [9, 11, 13, 15] ]
### this was the 3mm result (CF. previous)
# VSPW = [ [17, 19, 21, 23],
#          [17, 19, 21, 23],
#          [9, 11, 13, 15],
#          [9, 11, 13, 15] ]

### this should work to generate it automatically
VSPW = []
for i,asd in enumerate(ASDM):
    vis = asd + '.ms'
    tb.open(vis + '/SPECTRAL_WINDOW')
    sws = [x for x in range(len(tb.getcol('NUM_CHAN')))
        if tb.getcol('NUM_CHAN')[x] == 240]
    tb.close()
    print 'SWs of interest for ASDM%i - %s are '%(i+1,asdm) + str(sws)
    VSPW.append(sws)
print VSPW
### 3mm:[[17, 19, 21, 23], [17, 19, 21, 23], [9, 11, 13, 15], [9, 11, 13, 15]]

# split out the spw(s) we need into new MS for just the scans we need.
### this was 1mm code
if band6:
    print 'Band6 visibility splitting'
    for i,asd in enumerate(ASDM):
      print 'Working out ASDM #%i - %s'%(i+1,asd)
      os.system('rm -rf %s.ms-split' % asd)
      split(vis=asd+'.ms',outputvis=asd+'.ms-split', datacolumn='data',
            spw=','.join([str(VSPW[i][2]), str(VSPW[i][3])]))
### this is 3mm code
if band3:
    print 'Band3 visibility splitting'
    for i,asd in enumerate(ASDM):
      print 'Working out ASDM #%i - %s'%(i+1,asd)
      os.system('rm -rf %s.ms-split' % asd)
      split(vis=asd+'.ms',outputvis=asd+'.ms-split', datacolumn='data',
            spw=','.join([str(VSPW[i][0])]))

# create a concatenated working MS with what we need.
print 'Concatenating the data'
concat(vis=[a+'.ms-split' for a in ASDM],concatvis=msname)

# collecting data from CALAPPPHASE into a single table
print 'Creating App Phase Table (%s)'%CALAPP
for i,asd in enumerate(ASDM):
  if i==0:
    os.system('cp -rf %s.ms-split/ASDM_CALAPPPHASE ./%s'%(asd,CALAPP))
  else:
    tb.open(asd+'.ms/ASDM_CALAPPPHASE')
    tb.copyrows('./%s'%CALAPP)
    tb.close()

#
# eof
#
