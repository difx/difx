# Invoke this file with casa as follows:
#
# casa --nologger --nologfile -c /...path-to.../DPFU_scanner.py
#

from __future__ import absolute_import
from __future__ import print_function
import glob
import os

#note: execfile imports pylab and numpy as pl and np

artsy = glob.glob('*.APP.artifacts')

if len(artsy) != 1:
    raise Exception('There can be only one APP.artifacts directory')
if not os.path.isdir(artsy[0]):
    raise Exception('%s must be a directory' % artsy[0])

# this should now be the label
label = artsy[0].split('.APP.artifacts')[0]
ddir = ('%s-DPFU.artifacts' % label)

# one level of protection
if os.path.exists(ddir + '.backup'): os.system('rm -rf ' + ddir + '.backup')
if os.path.exists(ddir): os.rename(ddir, ddir + '.backup')
if not os.path.exists(ddir): os.mkdir(ddir)
if not os.path.isdir(ddir):
    raise Exception('%s problematic for output results' % ddir)
dof = open('%s/%s-dpfu.log' % (ddir, label), 'w')

print('-'*78)
print('Using label',label,'sending results to',ddir)

Flux_inf = label + '.concatenated.ms.flux_inf.APP'
CalAppPhase = label + '.concatenated.ms.calappphase'

dpfu_estimator= os.environ['DIFXROOT'] + '/share/polconvert/Estimate_DPFU.py'

Answers = []
dpfu = []

print('-'*78)
# run the script to estimate DPFU for each execblk
for uid in glob.glob(
    '%s.APP.artifacts/%s.TSYS_INFO/uid*.SYSCAL' % (label,label)):
    IStart = ''
    DPFUave = 1.0
    TsysTable = uid
    Execblk,ignore = os.path.basename(uid).split('.')
    exec(compile(open(dpfu_estimator, "rb").read(), dpfu_estimator, 'exec'))
    print('(',Execblk,IStart,MStart,DPFUave,')')
    Answers.append( (Execblk,IStart,MStart,DPFUave) )
    dpfu.append(DPFUave)
    for fig in ['GAIN_MEDIAN.png', 'PHASED_TSYS_MED.png', 'DPFU.png']:
        os.rename(fig, ('%s/%s-%s' % (ddir, Execblk, fig)))
    print('-'*78)

print('')
print('## DPFU reduction on ',label, file=dof)
# report the answers sorted by time (also execblk)
for e,i,m,d in sorted(Answers,lambda x,y: cmp(x[1],y[1])):
    print(('DPFU %.6f at %s (%f) %-24s' % (d,i,m,e)))
    print(('%f %f # %-24s at %s' % (m,d,e,i)), file=dof)
avedpfu = np.average(dpfu)
short = 'Average DPFU for Trak %s is %f' % (label, avedpfu)
print('')
print('-'*78)
print(short)
print('')

print('##',short, file=dof)
dof.close()

# make summary plot
adpfu = [avedpfu for x in dpfu]

pl.ioff()
fig = pl.figure()
sub = fig.add_subplot(1,1,1)
sub.plot(dpfu,'or')
sub.plot(adpfu,'-g')
sub.set_xlabel('execblock samples')
sub.set_ylabel('DPFU')
pl.title('DPFU for %s' % label)
pl.savefig('%s/%s-DPFU-summary.png' % (ddir, label))

# take out the trash that casa insists on leaving
for log in glob.glob('ipython-2*-*.log'):
    print('removing',log)
    os.unlink(log)

# eof
