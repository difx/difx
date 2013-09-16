from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'TX006C.6CH.2.1.2.1'
stnCode = 'HN'
mjdStart = 56461 + 67500*second

# File written by vex2script version 0.19 vintage 20130912

dbe0 = RDBE(1, 'ddc', 'ddc_1501283.bin')
dbe0.setALC(1)
dbe0.setFormat('VDIF')
dbe0.setPSNMode(0)
dbe0.setPacket(0, 0, 28, 5032)
subarray.setDBE(dbe0)

dbe1 = RDBE(2, 'ddc', 'ddc_1501183.bin')
dbe1.setALC(1)
dbe1.setFormat('VDIF')
dbe1.setPSNMode(0)
dbe1.setPacket(0, 0, 28, 5032)
subarray.setDBE(dbe1)

recorder0 = Mark5C('-1')
recorder0.setMode('VDIF')
recorder0.setPSNMode(0)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecorder(recorder0)

# XCUBE init
essr = ESSR()          # constructor
essr.setMode(3)        # two in, one out
essr.setRoute(2, 4)    # route incoming traffic from input 1 and 2
essr.setRoute(3, 4)    # to output 1
essr.setPace(4, 5)     # set port 4 packet pacing to 5
subarray.setESSR(essr)

loif0a = VLBALoIfSetup() 
loif0a.setIf('A', '6cm', 'R', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif0a.setPhaseCal(5)
loif0a.setDBEParams(0, -1, -1, 10, 0)
loif0a.setDBERemember(0, 1)
loif0b = VLBALoIfSetup() 
loif0b.setIf('C', '6cm', 'L', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif0b.setIf('B', '6cm', 'R', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif0b.setPhaseCal(5)
loif0b.setDBEParams(0, -1, -1, 10, 0)
loif0b.setDBERemember(0, 1)
channelSet0a = [ \
  bbc(0, 752.25, 64, 'U', 2, 0), \  # IF A
  bbc(0, 816.25, 64, 'U', 2, 2)  \  # IF A
  ]
channelSet0b = [ \
  bbc(0, 752.25, 64, 'U', 2, 1), \  # IF C
  bbc(0, 816.25, 64, 'U', 2, 3), \  # IF C
  bbc(1, 816.25, 64, 'L', 2, 4)  \  # IF B
  ]

loif1a = VLBALoIfSetup() 
loif1a.setIf('A', '6cm', 'R', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif1a.setIf('D', '6cm', 'L', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif1a.setPhaseCal(5)
loif1a.setDBEParams(0, -1, -1, 10, 0)
loif1a.setDBERemember(0, 1)
loif1b = VLBALoIfSetup() 
loif1b.setIf('C', '6cm', 'L', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif1b.setIf('B', '6cm', 'R', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif1b.setPhaseCal(5)
loif1b.setDBEParams(0, -1, -1, 10, 0)
loif1b.setDBERemember(0, 1)
channelSet1a = [ \
  bbc(0, 752.25, 64, 'U', 2, 0), \  # IF A
  bbc(0, 816.25, 64, 'U', 2, 2), \  # IF A
  bbc(1, 816.25, 64, 'L', 2, 5)  \  # IF D
  ]
channelSet1b = [ \
  bbc(0, 752.25, 64, 'U', 2, 1), \  # IF C
  bbc(0, 816.25, 64, 'U', 2, 3), \  # IF C
  bbc(1, 816.25, 64, 'L', 2, 4)  \  # IF B
  ]

loif2a = VLBALoIfSetup() 
loif2a.setIf('A', '6cm', 'R', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif2a.setIf('D', '6cm', 'L', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif2a.setPhaseCal(5)
loif2a.setDBEParams(0, -1, -1, 10, 0)
loif2a.setDBERemember(0, 1)
loif2b = VLBALoIfSetup() 
loif2b.setIf('B', '6cm', 'R', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif2b.setIf('C', '6cm', 'L', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif2b.setPhaseCal(5)
loif2b.setDBEParams(0, -1, -1, 10, 0)
loif2b.setDBERemember(0, 1)
channelSet2a = [ \
  bbc(0, 752.25, 64, 'U', 2, 0), \  # IF A
  bbc(0, 816.25, 64, 'U', 2, 2), \  # IF A
  bbc(1, 816.25, 64, 'L', 2, 5)  \  # IF D
  ]
channelSet2b = [ \
  bbc(0, 816.25, 64, 'L', 2, 4), \  # IF B
  bbc(0, 752.25, 64, 'L', 2, 6), \  # IF B
  bbc(1, 752.25, 64, 'U', 2, 1), \  # IF C
  bbc(1, 816.25, 64, 'U', 2, 3)  \  # IF C
  ]

loif3a = VLBALoIfSetup() 
loif3a.setIf('A', '6cm', 'R', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif3a.setIf('D', '6cm', 'L', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif3a.setPhaseCal(5)
loif3a.setDBEParams(0, -1, -1, 10, 0)
loif3a.setDBERemember(0, 1)
loif3b = VLBALoIfSetup() 
loif3b.setIf('B', '6cm', 'R', 7900, 'L', 'NA', 0, '6cm', 7900, 4100, 816.25)
loif3b.setIf('C', '6cm', 'L', 4100, 'U', 'NA', 0, '6cm', 7900, 4100, 752.25)
loif3b.setPhaseCal(5)
loif3b.setDBEParams(0, -1, -1, 10, 0)
loif3b.setDBERemember(0, 1)
channelSet3a = [ \
  bbc(0, 752.25, 64, 'U', 2, 0), \  # IF A
  bbc(0, 816.25, 64, 'U', 2, 2), \  # IF A
  bbc(1, 816.25, 64, 'L', 2, 5), \  # IF D
  bbc(1, 752.25, 64, 'L', 2, 7)  \  # IF D
  ]
channelSet3b = [ \
  bbc(0, 816.25, 64, 'L', 2, 4), \  # IF B
  bbc(0, 752.25, 64, 'L', 2, 6), \  # IF B
  bbc(1, 752.25, 64, 'U', 2, 1), \  # IF C
  bbc(1, 816.25, 64, 'U', 2, 3)  \  # IF C
  ]

source0 = Source(0.600016080526788, 1.28850203083902)
source0.setName('0212+735')

# Setup Scan 
# changing to mode tx006d8
subarray.setChannels(dbe0, channelSet3a)
subarray.setVLBALoIfSetup(dbe0, loif3a)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe1, channelSet3b)
subarray.setVLBALoIfSetup(dbe1, loif3b)
subarray.set4x4Switch('2A', 2)
subarray.set4x4Switch('2B', 3)
subarray.setSource(source0)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 0*second, mjdStart+300*second, 'No0001', obsCode, stnCode )
if array.time() < mjdStart + (300-10)*second:
  subarray.execute(mjdStart + 295*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+300*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0002
# changing to mode tx006d7
subarray.setChannels(dbe0, channelSet2a)
subarray.setVLBALoIfSetup(dbe0, loif2a)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe1, channelSet2b)
subarray.setVLBALoIfSetup(dbe1, loif2b)
subarray.set4x4Switch('2A', 2)
subarray.set4x4Switch('2B', 3)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 420*second, mjdStart+720*second, 'No0002', obsCode, stnCode )
if array.time() < mjdStart + (720-10)*second:
  subarray.execute(mjdStart + 715*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+720*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0003
# changing to mode tx006d6
subarray.setChannels(dbe0, channelSet1a)
subarray.setVLBALoIfSetup(dbe0, loif1a)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe1, channelSet1b)
subarray.setVLBALoIfSetup(dbe1, loif1b)
subarray.set4x4Switch('2A', 3)
subarray.set4x4Switch('2B', 2)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 840*second, mjdStart+1140*second, 'No0003', obsCode, stnCode )
if array.time() < mjdStart + (1140-10)*second:
  subarray.execute(mjdStart + 1135*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1140*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0004
# changing to mode tx006d5
subarray.setChannels(dbe0, channelSet0a)
subarray.setVLBALoIfSetup(dbe0, loif0a)
subarray.set4x4Switch('1A', 1)
subarray.setChannels(dbe1, channelSet0b)
subarray.setVLBALoIfSetup(dbe1, loif0b)
subarray.set4x4Switch('2A', 3)
subarray.set4x4Switch('2B', 2)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 1260*second, mjdStart+1560*second, 'No0004', obsCode, stnCode )
if array.time() < mjdStart + (1560-10)*second:
  subarray.execute(mjdStart + 1555*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1560*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0005
# changing to mode tx006d8
subarray.setChannels(dbe0, channelSet3a)
subarray.setVLBALoIfSetup(dbe0, loif3a)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe1, channelSet3b)
subarray.setVLBALoIfSetup(dbe1, loif3b)
subarray.set4x4Switch('2A', 2)
subarray.set4x4Switch('2B', 3)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 1680*second, mjdStart+1980*second, 'No0005', obsCode, stnCode )
if array.time() < mjdStart + (1980-10)*second:
  subarray.execute(mjdStart + 1975*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1980*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0006
# changing to mode tx006d7
subarray.setChannels(dbe0, channelSet2a)
subarray.setVLBALoIfSetup(dbe0, loif2a)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe1, channelSet2b)
subarray.setVLBALoIfSetup(dbe1, loif2b)
subarray.set4x4Switch('2A', 2)
subarray.set4x4Switch('2B', 3)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 2100*second, mjdStart+2400*second, 'No0006', obsCode, stnCode )
if array.time() < mjdStart + (2400-10)*second:
  subarray.execute(mjdStart + 2395*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2400*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0007
# changing to mode tx006d6
subarray.setChannels(dbe0, channelSet1a)
subarray.setVLBALoIfSetup(dbe0, loif1a)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe1, channelSet1b)
subarray.setVLBALoIfSetup(dbe1, loif1b)
subarray.set4x4Switch('2A', 3)
subarray.set4x4Switch('2B', 2)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 2520*second, mjdStart+2820*second, 'No0007', obsCode, stnCode )
if array.time() < mjdStart + (2820-10)*second:
  subarray.execute(mjdStart + 2815*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2820*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0008
# changing to mode tx006d5
subarray.setChannels(dbe0, channelSet0a)
subarray.setVLBALoIfSetup(dbe0, loif0a)
subarray.set4x4Switch('1A', 1)
subarray.setChannels(dbe1, channelSet0b)
subarray.setVLBALoIfSetup(dbe1, loif0b)
subarray.set4x4Switch('2A', 3)
subarray.set4x4Switch('2B', 2)
recorder0.setPacket(0, 0, 28, 5032)
subarray.setRecord(mjdStart + 2940*second, mjdStart+3240*second, 'No0008', obsCode, stnCode )
if array.time() < mjdStart + (3240-10)*second:
  subarray.execute(mjdStart + 3235*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3240*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 3241*second)
