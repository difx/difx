import pylab as pl
import numpy as np

TsysTable = '/home/marti/NO_BACKUP/EHT_TARS/B/TRACK_B.APP.artifacts/TRACK_B.TSYS_INFO/uid___A002_Xbec3cb_X1aa5.SYSCAL'

Flux_inf = '/home/marti/NO_BACKUP/EHT_TARS/B/TRACK_B.concatenated.ms.flux_inf.APP'



###################################
# READ TSYS:

tb.open(TsysTable)

#Trec = tb.getcol('TRX_SPECTRUM')
Tsys = tb.getcol('TSYS_SPECTRUM')
Antenna = tb.getcol('ANTENNA_ID')
AllAntenna = np.unique(Antenna)
Time = tb.getcol('TIME')
AllTime = np.unique(Time)
Spw = tb.getcol('SPECTRAL_WINDOW_ID')
AllSpw = np.unique(Spw)
tb.close()

# MJD in hours of day:
HH = AllTime/86400.
HH -= np.floor(np.min(HH))
HH *= 24.
HT = HH%24.
####################################


###################################
# READ AMPLITUDE GAINS:

tb.open(Flux_inf)
GTime = tb.getcol('TIME')
AllGTime = np.unique(GTime)
GSpw = tb.getcol('SPECTRAL_WINDOW_ID')
AllGSpw = np.unique(GSpw)
GAntenna = tb.getcol('ANTENNA1')
AllGAntenna = np.unique(GAntenna)
Gains = tb.getcol('CPARAM')
GFlag = tb.getcol('FLAG')
tb.close()
####################################


# AVERAGE THE STATION GAINS:
AvgGain = np.zeros((len(AllGAntenna),len(AllGSpw)))
mask = np.logical_not(GFlag[0,0,:])
for ai,ant in enumerate(AllGAntenna):
  mask2 = mask*(GAntenna==ant)
  for si,spi in enumerate(AllGSpw):
    mask3 = mask2*(GSpw==spi)
    AvgGain[ai,si] = np.median(np.abs(Gains[0,0,mask3]))

fig = pl.figure()
fig.subplots_adjust(wspace=0.3,hspace=0.3)
for si in range(len(AllGSpw)):
  sub = fig.add_subplot(2,2,si+1)
  sub.plot(AvgGain[:,si],'or')
  sub.set_xlabel('Antenna Id')
  sub.set_ylabel('Gain Median')
  pl.title('SPW %i'%si)

pl.savefig('GAIN_MEDIAN.png')





# AVERAGE TSYS MEASUREMENTS:


MedTsys = {}

for spi in AllSpw:
  MedTsys[spi] = {}
  mask = Spw==spi

  for ant in AllAntenna:
    MedTsys[spi][ant] = []
    mask2 = mask*(Antenna==ant)

    for time in AllTime:

      mask3 = mask2*(Time==time)
      # Average over polarizations:
      SelTsys = np.average(Tsys[:,:,mask3][:,:,0],axis=0)
      # Median over spectral channels:
      MedTsys[spi][ant].append(np.median(SelTsys)) 



 
AllMeds = np.zeros((len(AllSpw),len(AllTime)))
NMeds = np.copy(AllMeds)

for i,spi in enumerate(AllSpw):
  for j,ti in enumerate(AllTime):
    for ant in AllAntenna:
      if MedTsys[spi][ant][j]>0.0:
        AllMeds[i,j] += 1./MedTsys[spi][ant][j]
        NMeds[i,j] += 1.
    if NMeds[i,j]>0.0:
      AllMeds[i,j] = 1./AllMeds[i,j]


fig = pl.figure()
fig.subplots_adjust(wspace=0.3,hspace=0.3)

for i,spi in enumerate(AllSpw):
  sub = fig.add_subplot(2,2,i+1)
  sub.plot(HT,AllMeds[i],'or')
  sub.set_xlabel('UT (h)')
  sub.set_ylabel('Phased Tsys (K)')
  pl.title('SPW %i'%spi)


pl.savefig('PHASED_TSYS_MED.png')




# Estimate DPFUs:
DPFU = np.zeros((len(AllGAntenna),len(AllGSpw)))

for ai,ant in enumerate(AllGAntenna):
  for si,spi in enumerate(AllSpw):

    if AvgGain[ai,si]>0.0 and ant in MedTsys[spi].keys() and np.min(MedTsys[spi][ant])>0.0:
      DPFU[ai,si] = np.median(MedTsys[spi][ant])*(AvgGain[ai,si])**2.

fig = pl.figure()
fig.subplots_adjust(wspace=0.3,hspace=0.3)

for i,spi in enumerate(AllSpw):
  sub = fig.add_subplot(2,2,i+1)
  sub.plot(DPFU[:,i],'or')
  sub.set_xlabel('Antenna Id')
  sub.set_ylabel('DPFU (K/Jy)')
  pl.title('SPW %i'%spi)


pl.savefig('DPFU.png')


Phased = DPFU[:,0]>0.0
NPhas = np.sum(Phased)
print 'Average DPFU over %i antennas: %.3f K/Jy'%(NPhas,np.average(DPFU[Phased]))
