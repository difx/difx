import os
import numpy as np
import pylab as pl


IDI = 'ALMAVLBA.FITS'
CALAPP = 'CALAPPPHASE.tab'
msname = 'CONCAT_spw1.ms'
Range = [0,8,50,0,0,8,52,0]
gains=['%s.bandpass.cal'%msname,
    '%s.gains.cal.fluxscale'%msname,
    '%s.XY0amb'%msname]


XYs = np.linspace(-90,90,20)

for i,xy in enumerate(XYs):
  print 'Doing phase %i of %i'%(i,len(XYs))
  polconvert(IDI=IDI, OUTPUTIDI=IDI, linAntIdx=[1], Range=Range,
    ALMAvis=msname, spw=0, calAPP=CALAPP, gains=[gains],
    dterms=['NONE'], XYadd=[xy], plotIF=2, plotRange=Range,
    plotAnt=2, doTest=True)
  os.system('mv Fringe.plot2.png Fringe.plot2.%i.png'%i)
  os.system('mv FRINGE.PEAKS.dat FRINGE.PEAKS.%i.dat'%i)


# Read and plot polconvert fringe results:
RR = np.zeros(len(XYs))
RL = np.zeros(len(XYs))
LR = np.zeros(len(XYs))
LL = np.zeros(len(XYs))

for i, xy in enumerate(XYs):
  iff = open('FRINGE.PEAKS.%i.dat'%i)
  for line in iff.readlines():
    if 'RR' in line:
      RR[i] = float(line.split()[1])
    if 'RL' in line:
      RL[i] = float(line.split()[1])
    if 'LR' in line:
      LR[i] = float(line.split()[1])
    if 'LL' in line:
      LL[i] = float(line.split()[1])
  iff.close()


pl.plot(XYs,RR,'or')
pl.plot(XYs,RR,'-r',label='RR')
pl.plot(XYs,RL,'og')
pl.plot(XYs,RL,'-g',label='RL')
pl.plot(XYs,LR,'ok')
pl.plot(XYs,LR,'-k',label='LR')
pl.plot(XYs,LL,'ob')
pl.plot(XYs,LL,'-b',label='LL')
pl.legend()

pl.ylabel('Fringe Amplitude (norm.)')
pl.xlabel('XYadd (deg.)')

pl.show()

