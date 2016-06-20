#!/usr/bin/python
#
# This script is to be run to import the ASDMs and
# prepare a MS with only the required data.  (This
# is more efficient in the long run since many bands
# are not needed.)

print 'FIXME'
raise Exception, 'Code not correct, yet'

import os

msname = FIXME # 'concat.ms'
CALAPP = FIXME # 'calappphase.tab'

asdmdir='/alma/data/alma-apr08-2016'
ASDM = [ 'uid___A002_Xb187bd_X2d5',
         'uid___A002_Xb187bd_X4fe',
         'uid___A002_Xb187bd_X733',
         'uid___A002_Xb187bd_X863' ]

# remove previous products

# Iterate over ASDMs and import to MS:
for i,asd in enumerate(ASDM):
  print 'Working out ASDM #%i - %s'%(i+1,asd)
  os.system('rm -rf %s.ms' % asd)
  os.system('rm -rf %s.ms.flagversions' % asd)
  importasdm(asdm=os.path.join(asdmdir, asd), vis=asd+'.ms', asis='CalAppPhase')

# 100 GB

# identify the spw used for the recorded bands
# band3 uses the first vlbi window
# band6 uses the third and fourth vlbi window
# tget listobs
# default listobs
# OR
# tb.open(vis + '/SPECTRAL_WINDOW')
# tb.colnames()
# import numpy as np
# list(np.where(tb.getcol('NUM_CHAN') == 240)[0])
#   [17, 19, 21, 23]
# tb.close()
# [x for x in range(len(tb.getcol('NUM_CHAN')))
#    if tb.getcol('NUM_CHAN')[x] == 240]

VSPW = [ [17, 19, 21, 23],
         [9, 11, 13, 15],
         [9, 11, 13, 15],
         [9, 11, 13, 15] ]

# split out the spw(s) we need into new MS for just the scans we need.
for i,asd in enumerate(ASDM):
  print 'Working out ASDM #%i - %s'%(i+1,asd)
  os.system('rm -rf %s.ms-split' % asd)
  split(vis=asd+'.ms',outputvis=asd+'.ms-split', datacolumn='data',
        spw=','.join([str(VSPW[i][2]), str(VSPW[i][3])]))

# create a concatenated working MS with what we need.
print 'Concatenating the data'
concat(vis=[a+'.ms-split' for a in ASDM],concatvis='CONCAT.ms')

# 41 GB

# collecting data from CALAPPPHASE into a single table
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
