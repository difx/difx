# Invoke this file with casa as follows:
#
# casa --nologger --nologfile -c foo.py
#

import glob
import os

artsy = glob.glob('*.APP.artifacts')

if len(artsy) != 1:
    raise Exception, 'There can be only one APP.artifacts directory'
if not os.path.isdir(artsy[0]):
    raise Exception, ('%s must be a directory' % artsy[0])

# this should now be the label
label = artsy[0].split('.')[0]
print '-'*78
print 'Using label',label

Flux_inf = label + '.concatenated.ms.flux_inf.APP'
CalAppPhase = label + '.concatenated.ms.calappphase'

dpfu_estimator= os.environ['DIFXROOT'] + '/bin/Estimate_DPFU.py'

Answers = []
dpfu = []

print '-'*78
# run the script to estimate DPFU for each execblk
for uid in glob.glob(
    '%s.APP.artifacts/%s.TSYS_INFO/uid*.SYSCAL' % (label,label)):
    IStart = ''
    DPFUave = 1.0
    TsysTable = uid
    Execblk,ignore = os.path.basename(uid).split('.')
    execfile(dpfu_estimator)
    print '( saving',Execblk,IStart,DPFUave,')'
    Answers.append( (Execblk,IStart,DPFUave) )
    dpfu.append(DPFUave)
    print '-'*78

    ### FIXME: sweep GAIN_MEDIAN.png PHASED_TSYS_MED.png DPFU.png
    ### plots into directories named for IStart and/or Execblk

print ''
# report the answers sorted by time (also execblk)
for e,i,d in sorted(Answers,lambda x,y: cmp(x[1],y[1])):
    print ('%-28s' % e),'at',i,'has DPFU',('%f' % d)
print ''
print '-'*78
print 'Average DPFU for Trak %s is %f' % (label, np.average(dpfu))
print ''

# eof
