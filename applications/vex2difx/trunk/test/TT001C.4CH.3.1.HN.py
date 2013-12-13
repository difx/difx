from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'TT001C.4CH.3.1'
stnCode = 'HN'
mjdStart = 56450 + 15720*second

# File written by vex2script version 0.24 vintage 20131209

dbe0 = RDBE(0, 'ddc', 'ddc_1501383.bin')
dbe0.setALC(1)
dbe0.setFormat('VDIF')
dbe0.setPSNMode(0)
dbe0.setPacket(0, 0, 28, 5032)
subarray.setDBE(dbe0)

recorder0 = Mark5C('-1')
recorder0.setMode('VDIF')
recorder0.setPSNMode(0)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecorder(recorder0)

# XCUBE init
essr = ESSR()          # constructor
essr.setMode(4)        # one in, one out
essr.setRoute(2, 4)    # route incoming traffic from input 1 to output 1
essr.setPace(4, 5)     # set port 4 packet pacing to 5
subarray.setESSR(essr)

loif0 = VLBALoIfSetup() 
loif0.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 766.875)
loif0.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 766.875)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBEParams(1, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
loif0.setDBERemember(1, 1)
channelSet0 = [ \
  bbc(0, 766.875, 2, 'U', 2, 1), \  # IF C
  bbc(0, 766.625, 2, 'U', 2, 2), \  # IF C
  bbc(0, 766.625, 2, 'U', 2, 3), \  # IF C
  bbc(1, 766.875, 2, 'U', 2, 0)  \  # IF A
  ]

loif1 = VLBALoIfSetup() 
loif1.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 766.875)
loif1.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 766.875)
loif1.setPhaseCal(1)
loif1.setDBEParams(0, -1, -1, 10, 0)
loif1.setDBEParams(1, -1, -1, 10, 0)
loif1.setDBERemember(0, 1)
loif1.setDBERemember(1, 1)
channelSet1 = [ \
  bbc(0, 766.875, 2, 'U', 2, 1), \  # IF C
  bbc(0, 766.75, 2, 'U', 2, 2), \  # IF C
  bbc(0, 766.75, 2, 'U', 2, 3), \  # IF C
  bbc(1, 766.875, 2, 'U', 2, 0)  \  # IF A
  ]

loif2 = VLBALoIfSetup() 
loif2.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 765.875)
loif2.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 765.875)
loif2.setPhaseCal(1)
loif2.setDBEParams(0, -1, -1, 10, 0)
loif2.setDBEParams(1, -1, -1, 10, 0)
loif2.setDBERemember(0, 1)
loif2.setDBERemember(1, 1)
channelSet2 = [ \
  bbc(0, 765.875, 4, 'U', 2, 1), \  # IF C
  bbc(0, 765.75, 4, 'U', 2, 2), \  # IF C
  bbc(0, 765.75, 4, 'U', 2, 3), \  # IF C
  bbc(1, 765.875, 4, 'U', 2, 0)  \  # IF A
  ]

loif3 = VLBALoIfSetup() 
loif3.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 763.875)
loif3.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 763.875)
loif3.setPhaseCal(1)
loif3.setDBEParams(0, -1, -1, 10, 0)
loif3.setDBEParams(1, -1, -1, 10, 0)
loif3.setDBERemember(0, 1)
loif3.setDBERemember(1, 1)
channelSet3 = [ \
  bbc(0, 763.875, 8, 'U', 2, 1), \  # IF C
  bbc(0, 763.75, 8, 'U', 2, 2), \  # IF C
  bbc(0, 763.75, 8, 'U', 2, 3), \  # IF C
  bbc(1, 763.875, 8, 'U', 2, 0)  \  # IF A
  ]

loif4 = VLBALoIfSetup() 
loif4.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 759.875)
loif4.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 759.875)
loif4.setPhaseCal(1)
loif4.setDBEParams(0, -1, -1, 10, 0)
loif4.setDBEParams(1, -1, -1, 10, 0)
loif4.setDBERemember(0, 1)
loif4.setDBERemember(1, 1)
channelSet4 = [ \
  bbc(0, 759.875, 16, 'U', 2, 1), \  # IF C
  bbc(0, 759.75, 16, 'U', 2, 2), \  # IF C
  bbc(0, 759.75, 16, 'U', 2, 3), \  # IF C
  bbc(1, 759.875, 16, 'U', 2, 0)  \  # IF A
  ]

loif5 = VLBALoIfSetup() 
loif5.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 751.875)
loif5.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 751.875)
loif5.setPhaseCal(1)
loif5.setDBEParams(0, -1, -1, 10, 0)
loif5.setDBEParams(1, -1, -1, 10, 0)
loif5.setDBERemember(0, 1)
loif5.setDBERemember(1, 1)
channelSet5 = [ \
  bbc(0, 751.875, 32, 'U', 2, 1), \  # IF C
  bbc(0, 751.75, 32, 'U', 2, 2), \  # IF C
  bbc(0, 751.75, 32, 'U', 2, 3), \  # IF C
  bbc(1, 751.875, 32, 'U', 2, 0)  \  # IF A
  ]

loif6 = VLBALoIfSetup() 
loif6.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 735.875)
loif6.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 735.875)
loif6.setPhaseCal(1)
loif6.setDBEParams(0, -1, -1, 10, 0)
loif6.setDBEParams(1, -1, -1, 10, 0)
loif6.setDBERemember(0, 1)
loif6.setDBERemember(1, 1)
channelSet6 = [ \
  bbc(0, 735.875, 64, 'U', 2, 1), \  # IF C
  bbc(0, 735.75, 64, 'U', 2, 2), \  # IF C
  bbc(0, 735.75, 64, 'U', 2, 3), \  # IF C
  bbc(1, 735.875, 64, 'U', 2, 0)  \  # IF A
  ]

loif7 = VLBALoIfSetup() 
loif7.setIf('C', '6cm', 'L', 5100, 'U', 'NA', 0, '6cm', 703.875)
loif7.setIf('A', '6cm', 'R', 5100, 'U', 'NA', 0, '6cm', 703.875)
loif7.setPhaseCal(1)
loif7.setDBEParams(0, -1, -1, 10, 0)
loif7.setDBEParams(1, -1, -1, 10, 0)
loif7.setDBERemember(0, 1)
loif7.setDBERemember(1, 1)
channelSet7 = [ \
  bbc(0, 703.875, 128, 'U', 2, 1), \  # IF C
  bbc(0, 703.75, 128, 'U', 2, 2), \  # IF C
  bbc(0, 703.75, 128, 'U', 2, 3), \  # IF C
  bbc(1, 703.875, 128, 'U', 2, 0)  \  # IF A
  ]

source0 = Source(3.65612819167311, 1.33906048251936)
source0.setName('1357+769')

# Setup Scan 
# changing to mode ddc-2MHz-a
subarray.setChannels(dbe0, channelSet0)
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
subarray.setSource(source0)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 0*second, mjdStart+45*second, 'No0001', obsCode, stnCode )
if array.time() < mjdStart + (45-10)*second:
  subarray.execute(mjdStart + 40*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+45*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0002
# changing to mode ddc-2MHz-b
subarray.setChannels(dbe0, channelSet1)
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 60*second, mjdStart+105*second, 'No0002', obsCode, stnCode )
if array.time() < mjdStart + (105-10)*second:
  subarray.execute(mjdStart + 100*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+105*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0003
# changing to mode ddc-4MHz
subarray.setChannels(dbe0, channelSet2)
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 120*second, mjdStart+165*second, 'No0003', obsCode, stnCode )
if array.time() < mjdStart + (165-10)*second:
  subarray.execute(mjdStart + 160*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+165*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0004
# changing to mode ddc-8MHz
subarray.setChannels(dbe0, channelSet3)
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 180*second, mjdStart+225*second, 'No0004', obsCode, stnCode )
if array.time() < mjdStart + (225-10)*second:
  subarray.execute(mjdStart + 220*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+225*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0005
# changing to mode ddc-16MHz
subarray.setChannels(dbe0, channelSet4)
subarray.setVLBALoIfSetup(dbe0, loif4)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 240*second, mjdStart+285*second, 'No0005', obsCode, stnCode )
if array.time() < mjdStart + (285-10)*second:
  subarray.execute(mjdStart + 280*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+285*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0006
# changing to mode ddc-32MHz
subarray.setChannels(dbe0, channelSet5)
subarray.setVLBALoIfSetup(dbe0, loif5)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 300*second, mjdStart+345*second, 'No0006', obsCode, stnCode )
if array.time() < mjdStart + (345-10)*second:
  subarray.execute(mjdStart + 340*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+345*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0007
# changing to mode ddc-64MHz
subarray.setChannels(dbe0, channelSet6)
subarray.setVLBALoIfSetup(dbe0, loif6)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 360*second, mjdStart+405*second, 'No0007', obsCode, stnCode )
if array.time() < mjdStart + (405-10)*second:
  subarray.execute(mjdStart + 400*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+405*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0008
# changing to mode ddc-128MHz
subarray.setChannels(dbe0, channelSet7)
subarray.setVLBALoIfSetup(dbe0, loif7)
subarray.set4x4Switch('1A', 3)
subarray.set4x4Switch('1B', 1)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 420*second, mjdStart+465*second, 'No0008', obsCode, stnCode )
if array.time() < mjdStart + (465-10)*second:
  subarray.execute(mjdStart + 460*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+465*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 466*second)
