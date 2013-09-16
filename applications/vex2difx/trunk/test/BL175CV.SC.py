from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'BL175CV'
stnCode = 'SC'
mjdStart = 56453 + 59144*second

# File written by vex2script version 0.19 vintage 20130912

dbe0 = RDBE(0, 'pfb')
dbe0.setALC(1)
dbe0.setFormat('Mark5B')
dbe0.setPSNMode(0)
dbe0.setPacket(0, 0, 36, 5008)
subarray.setDBE(dbe0)

recorder0 = Mark5C('-1')
recorder0.setMode('Mark5B')
recorder0.setPSNMode(0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecorder(recorder0)

# XCUBE init
essr = ESSR()          # constructor
essr.setMode(4)        # one in, one out
essr.setRoute(2, 4)    # route incoming traffic from input 1 to output 1
essr.setPace(4, 5)     # set port 4 packet pacing to 5
subarray.setESSR(essr)

loif0 = VLBALoIfSetup()
loif0.setIf('D', '4cm', 'L', 7600, 'U', 'NA', 0, '4cm', 528)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
channelSet0 = [ \
  bbc(0, 528, 32, 'L', 2, 0), \  # IF D
  bbc(0, 560, 32, 'L', 2, 0), \  # IF D
  bbc(0, 592, 32, 'L', 2, 0), \  # IF D
  bbc(0, 624, 32, 'L', 2, 0), \  # IF D
  bbc(0, 656, 32, 'L', 2, 0), \  # IF D
  bbc(0, 688, 32, 'L', 2, 0), \  # IF D
  bbc(0, 720, 32, 'L', 2, 0), \  # IF D
  bbc(0, 752, 32, 'L', 2, 0), \  # IF D
  bbc(0, 784, 32, 'L', 2, 0), \  # IF D
  bbc(0, 816, 32, 'L', 2, 0), \  # IF D
  bbc(0, 848, 32, 'L', 2, 0), \  # IF D
  bbc(0, 880, 32, 'L', 2, 0), \  # IF D
  bbc(0, 912, 32, 'L', 2, 0), \  # IF D
  bbc(0, 944, 32, 'L', 2, 0), \  # IF D
  bbc(0, 976, 32, 'L', 2, 0), \  # IF D
  bbc(0, 1008, 32, 'L', 2, 0) \  # IF D
  ]

loif1 = VLBALoIfSetup()
loif1.setIf('B', '4cm', 'R', 7600, 'U', 'NA', 0, '4cm', 720)
loif1.setIf('D', '4cm', 'L', 7600, 'U', 'NA', 0, '4cm', 720)
loif1.setPhaseCal(1)
loif1.setDBEParams(0, -1, -1, 10, 0)
loif1.setDBEParams(1, -1, -1, 10, 0)
loif1.setDBERemember(0, 1)
loif1.setDBERemember(1, 1)
channelSet1 = [ \
  bbc(0, 720, 32, 'L', 2, 0), \  # IF B
  bbc(1, 720, 32, 'L', 2, 0), \  # IF D
  bbc(0, 752, 32, 'L', 2, 0), \  # IF B
  bbc(1, 752, 32, 'L', 2, 0), \  # IF D
  bbc(0, 784, 32, 'L', 2, 0), \  # IF B
  bbc(1, 784, 32, 'L', 2, 0), \  # IF D
  bbc(0, 816, 32, 'L', 2, 0), \  # IF B
  bbc(1, 816, 32, 'L', 2, 0), \  # IF D
  bbc(0, 848, 32, 'L', 2, 0), \  # IF B
  bbc(1, 848, 32, 'L', 2, 0), \  # IF D
  bbc(0, 880, 32, 'L', 2, 0), \  # IF B
  bbc(1, 880, 32, 'L', 2, 0), \  # IF D
  bbc(0, 912, 32, 'L', 2, 0), \  # IF B
  bbc(1, 912, 32, 'L', 2, 0), \  # IF D
  bbc(0, 944, 32, 'L', 2, 0), \  # IF B
  bbc(1, 944, 32, 'L', 2, 0) \  # IF D
  ]

source0 = Source(1.18958078435377, 0.424960523294477)
source0.setName('JTAU1')

source1 = Source(1.16469710102754, 0.409471859095637)
source1.setName('J0426+2327')

source2 = Source(1.20242880333102, 0.445930154365802)
source2.setName('J0435+2532')

source3 = Source(1.17758533769946, 0.478403844627707)
source3.setName('J0429+2724')

source4 = Source(1.217067898458, 0.381986187627242)
source4.setName('J0438+2153')

source5 = Source(0.0862328174574395, 1.28208986492441)
source5.setName('0016+731')

source6 = Source(1.45414039523877, 0.844413600550685)
source6.setName('0529+483')

source7 = Source(1.92811238318364, 1.24517782782741)
source7.setName('0716+714')

source8 = Source(2.0446611657045, 0.418995908766266)
source8.setName('0745+241')

source9 = Source(2.10661473171755, 0.31701602619337)
source9.setName('0759+183')

source10 = Source(2.22908261113727, 0.422077919418333)
source10.setName('0827+243')

source11 = Source(2.81165725094784, 1.4121625900691)
source11.setName('1039+811')

source12 = Source(2.87190549676401, 1.41794754248881)
source12.setName('J1058+81')

source13 = Source(3.65612819167311, 1.33906048251936)
source13.setName('1357+769')

source14 = Source(4.65232304751702, 1.089932608429)
source14.setName('1745+624')

source15 = Source(4.81762276202166, 0.992226907118727)
source15.setName('1823+568')

source16 = Source(5.24547203898111, 0.827716452585102)
source16.setName('2000+472')

source17 = Source(5.8931383783612, 1.21779416207866)
source17.setName('2229+695')

# Setup Scan 
# changing to mode v4cm-geodetic
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 4)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source6)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0113
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 0*second, mjdStart+60*second, 'No0113', obsCode, stnCode )
if array.time() < mjdStart + (60-10)*second:
  subarray.execute(mjdStart + 55*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+60*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0114
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 142*second, mjdStart+201*second, 'No0114', obsCode, stnCode )
if array.time() < mjdStart + (201-10)*second:
  subarray.execute(mjdStart + 196*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+201*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0115
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 229*second, mjdStart+289*second, 'No0115', obsCode, stnCode )
if array.time() < mjdStart + (289-10)*second:
  subarray.execute(mjdStart + 284*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+289*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0116
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 360*second, mjdStart+420*second, 'No0116', obsCode, stnCode )
if array.time() < mjdStart + (420-10)*second:
  subarray.execute(mjdStart + 415*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+420*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0117
# Antenna SC not in scan No0117

# Scan 5 = No0118
# Antenna SC not in scan No0118

# Scan 6 = No0119
# Antenna SC not in scan No0119

# Scan 7 = No0120
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 807*second, mjdStart+866*second, 'No0120', obsCode, stnCode )
if array.time() < mjdStart + (866-10)*second:
  subarray.execute(mjdStart + 861*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+866*second) + ' since array.time is ' + str(array.time())

# Scan 8 = No0121
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 893*second, mjdStart+954*second, 'No0121', obsCode, stnCode )
if array.time() < mjdStart + (954-10)*second:
  subarray.execute(mjdStart + 949*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+954*second) + ' since array.time is ' + str(array.time())

# Scan 9 = No0122
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1064*second, mjdStart+1124*second, 'No0122', obsCode, stnCode )
if array.time() < mjdStart + (1124-10)*second:
  subarray.execute(mjdStart + 1119*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1124*second) + ' since array.time is ' + str(array.time())

# Scan 10 = No0123
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1204*second, mjdStart+1264*second, 'No0123', obsCode, stnCode )
if array.time() < mjdStart + (1264-10)*second:
  subarray.execute(mjdStart + 1259*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1264*second) + ' since array.time is ' + str(array.time())

# Scan 11 = No0124
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1326*second, mjdStart+1386*second, 'No0124', obsCode, stnCode )
if array.time() < mjdStart + (1386-10)*second:
  subarray.execute(mjdStart + 1381*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1386*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0125
# changing to mode rdbe_pfb_8416_dual
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1542*second, mjdStart+1602*second, 'No0125', obsCode, stnCode )
if array.time() < mjdStart + (1602-10)*second:
  subarray.execute(mjdStart + 1597*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1602*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0126
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1615*second, mjdStart+1661*second, 'No0126', obsCode, stnCode )
if array.time() < mjdStart + (1661-10)*second:
  subarray.execute(mjdStart + 1656*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1661*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0127
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1673*second, mjdStart+1721*second, 'No0127', obsCode, stnCode )
if array.time() < mjdStart + (1721-10)*second:
  subarray.execute(mjdStart + 1716*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1721*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0128
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1739*second, mjdStart+1781*second, 'No0128', obsCode, stnCode )
if array.time() < mjdStart + (1781-10)*second:
  subarray.execute(mjdStart + 1776*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1781*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0129
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1795*second, mjdStart+1841*second, 'No0129', obsCode, stnCode )
if array.time() < mjdStart + (1841-10)*second:
  subarray.execute(mjdStart + 1836*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1841*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0130
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1851*second, mjdStart+1961*second, 'No0130', obsCode, stnCode )
if array.time() < mjdStart + (1961-10)*second:
  subarray.execute(mjdStart + 1956*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1961*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0131
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1971*second, mjdStart+2020*second, 'No0131', obsCode, stnCode )
if array.time() < mjdStart + (2020-10)*second:
  subarray.execute(mjdStart + 2015*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2020*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0132
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2030*second, mjdStart+2140*second, 'No0132', obsCode, stnCode )
if array.time() < mjdStart + (2140-10)*second:
  subarray.execute(mjdStart + 2135*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2140*second) + ' since array.time is ' + str(array.time())

# Scan 20 = No0133
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2150*second, mjdStart+2200*second, 'No0133', obsCode, stnCode )
if array.time() < mjdStart + (2200-10)*second:
  subarray.execute(mjdStart + 2195*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2200*second) + ' since array.time is ' + str(array.time())

# Scan 21 = No0134
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2210*second, mjdStart+2320*second, 'No0134', obsCode, stnCode )
if array.time() < mjdStart + (2320-10)*second:
  subarray.execute(mjdStart + 2315*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2320*second) + ' since array.time is ' + str(array.time())

# Scan 22 = No0135
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2330*second, mjdStart+2379*second, 'No0135', obsCode, stnCode )
if array.time() < mjdStart + (2379-10)*second:
  subarray.execute(mjdStart + 2374*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2379*second) + ' since array.time is ' + str(array.time())

# Scan 23 = No0136
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2389*second, mjdStart+2499*second, 'No0136', obsCode, stnCode )
if array.time() < mjdStart + (2499-10)*second:
  subarray.execute(mjdStart + 2494*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2499*second) + ' since array.time is ' + str(array.time())

# Scan 24 = No0137
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2509*second, mjdStart+2559*second, 'No0137', obsCode, stnCode )
if array.time() < mjdStart + (2559-10)*second:
  subarray.execute(mjdStart + 2554*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2559*second) + ' since array.time is ' + str(array.time())

# Scan 25 = No0138
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2569*second, mjdStart+2679*second, 'No0138', obsCode, stnCode )
if array.time() < mjdStart + (2679-10)*second:
  subarray.execute(mjdStart + 2674*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2679*second) + ' since array.time is ' + str(array.time())

# Scan 26 = No0139
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2689*second, mjdStart+2738*second, 'No0139', obsCode, stnCode )
if array.time() < mjdStart + (2738-10)*second:
  subarray.execute(mjdStart + 2733*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2738*second) + ' since array.time is ' + str(array.time())

# Scan 27 = No0140
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2748*second, mjdStart+2858*second, 'No0140', obsCode, stnCode )
if array.time() < mjdStart + (2858-10)*second:
  subarray.execute(mjdStart + 2853*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2858*second) + ' since array.time is ' + str(array.time())

# Scan 28 = No0141
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2868*second, mjdStart+2918*second, 'No0141', obsCode, stnCode )
if array.time() < mjdStart + (2918-10)*second:
  subarray.execute(mjdStart + 2913*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2918*second) + ' since array.time is ' + str(array.time())

# Scan 29 = No0142
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2928*second, mjdStart+3038*second, 'No0142', obsCode, stnCode )
if array.time() < mjdStart + (3038-10)*second:
  subarray.execute(mjdStart + 3033*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3038*second) + ' since array.time is ' + str(array.time())

# Scan 30 = No0143
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3048*second, mjdStart+3097*second, 'No0143', obsCode, stnCode )
if array.time() < mjdStart + (3097-10)*second:
  subarray.execute(mjdStart + 3092*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3097*second) + ' since array.time is ' + str(array.time())

# Scan 31 = No0144
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3107*second, mjdStart+3217*second, 'No0144', obsCode, stnCode )
if array.time() < mjdStart + (3217-10)*second:
  subarray.execute(mjdStart + 3212*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3217*second) + ' since array.time is ' + str(array.time())

# Scan 32 = No0145
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3227*second, mjdStart+3277*second, 'No0145', obsCode, stnCode )
if array.time() < mjdStart + (3277-10)*second:
  subarray.execute(mjdStart + 3272*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3277*second) + ' since array.time is ' + str(array.time())

# Scan 33 = No0146
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3287*second, mjdStart+3397*second, 'No0146', obsCode, stnCode )
if array.time() < mjdStart + (3397-10)*second:
  subarray.execute(mjdStart + 3392*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3397*second) + ' since array.time is ' + str(array.time())

# Scan 34 = No0147
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3407*second, mjdStart+3456*second, 'No0147', obsCode, stnCode )
if array.time() < mjdStart + (3456-10)*second:
  subarray.execute(mjdStart + 3451*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3456*second) + ' since array.time is ' + str(array.time())

# Scan 35 = No0148
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3466*second, mjdStart+3576*second, 'No0148', obsCode, stnCode )
if array.time() < mjdStart + (3576-10)*second:
  subarray.execute(mjdStart + 3571*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3576*second) + ' since array.time is ' + str(array.time())

# Scan 36 = No0149
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3586*second, mjdStart+3636*second, 'No0149', obsCode, stnCode )
if array.time() < mjdStart + (3636-10)*second:
  subarray.execute(mjdStart + 3631*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3636*second) + ' since array.time is ' + str(array.time())

# Scan 37 = No0150
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3648*second, mjdStart+3696*second, 'No0150', obsCode, stnCode )
if array.time() < mjdStart + (3696-10)*second:
  subarray.execute(mjdStart + 3691*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3696*second) + ' since array.time is ' + str(array.time())

# Scan 38 = No0151
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3707*second, mjdStart+3756*second, 'No0151', obsCode, stnCode )
if array.time() < mjdStart + (3756-10)*second:
  subarray.execute(mjdStart + 3751*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3756*second) + ' since array.time is ' + str(array.time())

# Scan 39 = No0152
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3772*second, mjdStart+3815*second, 'No0152', obsCode, stnCode )
if array.time() < mjdStart + (3815-10)*second:
  subarray.execute(mjdStart + 3810*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3815*second) + ' since array.time is ' + str(array.time())

# Scan 40 = No0153
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3829*second, mjdStart+3875*second, 'No0153', obsCode, stnCode )
if array.time() < mjdStart + (3875-10)*second:
  subarray.execute(mjdStart + 3870*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3875*second) + ' since array.time is ' + str(array.time())

# Scan 41 = No0154
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3885*second, mjdStart+3995*second, 'No0154', obsCode, stnCode )
if array.time() < mjdStart + (3995-10)*second:
  subarray.execute(mjdStart + 3990*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3995*second) + ' since array.time is ' + str(array.time())

# Scan 42 = No0155
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4005*second, mjdStart+4055*second, 'No0155', obsCode, stnCode )
if array.time() < mjdStart + (4055-10)*second:
  subarray.execute(mjdStart + 4050*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4055*second) + ' since array.time is ' + str(array.time())

# Scan 43 = No0156
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4065*second, mjdStart+4174*second, 'No0156', obsCode, stnCode )
if array.time() < mjdStart + (4174-10)*second:
  subarray.execute(mjdStart + 4169*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4174*second) + ' since array.time is ' + str(array.time())

# Scan 44 = No0157
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4184*second, mjdStart+4234*second, 'No0157', obsCode, stnCode )
if array.time() < mjdStart + (4234-10)*second:
  subarray.execute(mjdStart + 4229*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4234*second) + ' since array.time is ' + str(array.time())

# Scan 45 = No0158
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4244*second, mjdStart+4354*second, 'No0158', obsCode, stnCode )
if array.time() < mjdStart + (4354-10)*second:
  subarray.execute(mjdStart + 4349*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4354*second) + ' since array.time is ' + str(array.time())

# Scan 46 = No0159
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4364*second, mjdStart+4414*second, 'No0159', obsCode, stnCode )
if array.time() < mjdStart + (4414-10)*second:
  subarray.execute(mjdStart + 4409*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4414*second) + ' since array.time is ' + str(array.time())

# Scan 47 = No0160
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4424*second, mjdStart+4534*second, 'No0160', obsCode, stnCode )
if array.time() < mjdStart + (4534-10)*second:
  subarray.execute(mjdStart + 4529*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4534*second) + ' since array.time is ' + str(array.time())

# Scan 48 = No0161
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4544*second, mjdStart+4593*second, 'No0161', obsCode, stnCode )
if array.time() < mjdStart + (4593-10)*second:
  subarray.execute(mjdStart + 4588*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4593*second) + ' since array.time is ' + str(array.time())

# Scan 49 = No0162
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4603*second, mjdStart+4713*second, 'No0162', obsCode, stnCode )
if array.time() < mjdStart + (4713-10)*second:
  subarray.execute(mjdStart + 4708*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4713*second) + ' since array.time is ' + str(array.time())

# Scan 50 = No0163
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4724*second, mjdStart+4773*second, 'No0163', obsCode, stnCode )
if array.time() < mjdStart + (4773-10)*second:
  subarray.execute(mjdStart + 4768*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4773*second) + ' since array.time is ' + str(array.time())

# Scan 51 = No0164
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4783*second, mjdStart+4893*second, 'No0164', obsCode, stnCode )
if array.time() < mjdStart + (4893-10)*second:
  subarray.execute(mjdStart + 4888*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4893*second) + ' since array.time is ' + str(array.time())

# Scan 52 = No0165
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4904*second, mjdStart+4952*second, 'No0165', obsCode, stnCode )
if array.time() < mjdStart + (4952-10)*second:
  subarray.execute(mjdStart + 4947*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4952*second) + ' since array.time is ' + str(array.time())

# Scan 53 = No0166
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4962*second, mjdStart+5072*second, 'No0166', obsCode, stnCode )
if array.time() < mjdStart + (5072-10)*second:
  subarray.execute(mjdStart + 5067*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5072*second) + ' since array.time is ' + str(array.time())

# Scan 54 = No0167
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5083*second, mjdStart+5132*second, 'No0167', obsCode, stnCode )
if array.time() < mjdStart + (5132-10)*second:
  subarray.execute(mjdStart + 5127*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5132*second) + ' since array.time is ' + str(array.time())

# Scan 55 = No0168
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5142*second, mjdStart+5252*second, 'No0168', obsCode, stnCode )
if array.time() < mjdStart + (5252-10)*second:
  subarray.execute(mjdStart + 5247*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5252*second) + ' since array.time is ' + str(array.time())

# Scan 56 = No0169
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5263*second, mjdStart+5311*second, 'No0169', obsCode, stnCode )
if array.time() < mjdStart + (5311-10)*second:
  subarray.execute(mjdStart + 5306*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5311*second) + ' since array.time is ' + str(array.time())

# Scan 57 = No0170
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5321*second, mjdStart+5431*second, 'No0170', obsCode, stnCode )
if array.time() < mjdStart + (5431-10)*second:
  subarray.execute(mjdStart + 5426*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5431*second) + ' since array.time is ' + str(array.time())

# Scan 58 = No0171
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5442*second, mjdStart+5491*second, 'No0171', obsCode, stnCode )
if array.time() < mjdStart + (5491-10)*second:
  subarray.execute(mjdStart + 5486*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5491*second) + ' since array.time is ' + str(array.time())

# Scan 59 = No0172
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5501*second, mjdStart+5611*second, 'No0172', obsCode, stnCode )
if array.time() < mjdStart + (5611-10)*second:
  subarray.execute(mjdStart + 5606*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5611*second) + ' since array.time is ' + str(array.time())

# Scan 60 = No0173
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5622*second, mjdStart+5670*second, 'No0173', obsCode, stnCode )
if array.time() < mjdStart + (5670-10)*second:
  subarray.execute(mjdStart + 5665*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5670*second) + ' since array.time is ' + str(array.time())

# Scan 61 = No0174
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5682*second, mjdStart+5730*second, 'No0174', obsCode, stnCode )
if array.time() < mjdStart + (5730-10)*second:
  subarray.execute(mjdStart + 5725*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5730*second) + ' since array.time is ' + str(array.time())

# Scan 62 = No0175
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5741*second, mjdStart+5790*second, 'No0175', obsCode, stnCode )
if array.time() < mjdStart + (5790-10)*second:
  subarray.execute(mjdStart + 5785*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5790*second) + ' since array.time is ' + str(array.time())

# Scan 63 = No0176
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5805*second, mjdStart+5850*second, 'No0176', obsCode, stnCode )
if array.time() < mjdStart + (5850-10)*second:
  subarray.execute(mjdStart + 5845*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5850*second) + ' since array.time is ' + str(array.time())

# Scan 64 = No0177
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5864*second, mjdStart+5910*second, 'No0177', obsCode, stnCode )
if array.time() < mjdStart + (5910-10)*second:
  subarray.execute(mjdStart + 5905*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5910*second) + ' since array.time is ' + str(array.time())

# Scan 65 = No0178
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5921*second, mjdStart+6029*second, 'No0178', obsCode, stnCode )
if array.time() < mjdStart + (6029-10)*second:
  subarray.execute(mjdStart + 6024*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6029*second) + ' since array.time is ' + str(array.time())

# Scan 66 = No0179
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6040*second, mjdStart+6089*second, 'No0179', obsCode, stnCode )
if array.time() < mjdStart + (6089-10)*second:
  subarray.execute(mjdStart + 6084*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6089*second) + ' since array.time is ' + str(array.time())

# Scan 67 = No0180
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6100*second, mjdStart+6209*second, 'No0180', obsCode, stnCode )
if array.time() < mjdStart + (6209-10)*second:
  subarray.execute(mjdStart + 6204*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6209*second) + ' since array.time is ' + str(array.time())

# Scan 68 = No0181
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6220*second, mjdStart+6269*second, 'No0181', obsCode, stnCode )
if array.time() < mjdStart + (6269-10)*second:
  subarray.execute(mjdStart + 6264*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6269*second) + ' since array.time is ' + str(array.time())

# Scan 69 = No0182
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6280*second, mjdStart+6388*second, 'No0182', obsCode, stnCode )
if array.time() < mjdStart + (6388-10)*second:
  subarray.execute(mjdStart + 6383*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6388*second) + ' since array.time is ' + str(array.time())

# Scan 70 = No0183
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6399*second, mjdStart+6448*second, 'No0183', obsCode, stnCode )
if array.time() < mjdStart + (6448-10)*second:
  subarray.execute(mjdStart + 6443*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6448*second) + ' since array.time is ' + str(array.time())

# Scan 71 = No0184
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6459*second, mjdStart+6568*second, 'No0184', obsCode, stnCode )
if array.time() < mjdStart + (6568-10)*second:
  subarray.execute(mjdStart + 6563*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6568*second) + ' since array.time is ' + str(array.time())

# Scan 72 = No0185
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6579*second, mjdStart+6628*second, 'No0185', obsCode, stnCode )
if array.time() < mjdStart + (6628-10)*second:
  subarray.execute(mjdStart + 6623*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6628*second) + ' since array.time is ' + str(array.time())

# Scan 73 = No0186
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6639*second, mjdStart+6747*second, 'No0186', obsCode, stnCode )
if array.time() < mjdStart + (6747-10)*second:
  subarray.execute(mjdStart + 6742*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6747*second) + ' since array.time is ' + str(array.time())

# Scan 74 = No0187
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6758*second, mjdStart+6807*second, 'No0187', obsCode, stnCode )
if array.time() < mjdStart + (6807-10)*second:
  subarray.execute(mjdStart + 6802*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6807*second) + ' since array.time is ' + str(array.time())

# Scan 75 = No0188
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6818*second, mjdStart+6927*second, 'No0188', obsCode, stnCode )
if array.time() < mjdStart + (6927-10)*second:
  subarray.execute(mjdStart + 6922*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6927*second) + ' since array.time is ' + str(array.time())

# Scan 76 = No0189
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6938*second, mjdStart+6987*second, 'No0189', obsCode, stnCode )
if array.time() < mjdStart + (6987-10)*second:
  subarray.execute(mjdStart + 6982*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6987*second) + ' since array.time is ' + str(array.time())

# Scan 77 = No0190
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6998*second, mjdStart+7106*second, 'No0190', obsCode, stnCode )
if array.time() < mjdStart + (7106-10)*second:
  subarray.execute(mjdStart + 7101*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7106*second) + ' since array.time is ' + str(array.time())

# Scan 78 = No0191
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7117*second, mjdStart+7166*second, 'No0191', obsCode, stnCode )
if array.time() < mjdStart + (7166-10)*second:
  subarray.execute(mjdStart + 7161*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7166*second) + ' since array.time is ' + str(array.time())

# Scan 79 = No0192
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7177*second, mjdStart+7286*second, 'No0192', obsCode, stnCode )
if array.time() < mjdStart + (7286-10)*second:
  subarray.execute(mjdStart + 7281*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7286*second) + ' since array.time is ' + str(array.time())

# Scan 80 = No0193
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7297*second, mjdStart+7346*second, 'No0193', obsCode, stnCode )
if array.time() < mjdStart + (7346-10)*second:
  subarray.execute(mjdStart + 7341*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7346*second) + ' since array.time is ' + str(array.time())

# Scan 81 = No0194
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7357*second, mjdStart+7465*second, 'No0194', obsCode, stnCode )
if array.time() < mjdStart + (7465-10)*second:
  subarray.execute(mjdStart + 7460*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7465*second) + ' since array.time is ' + str(array.time())

# Scan 82 = No0195
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7476*second, mjdStart+7525*second, 'No0195', obsCode, stnCode )
if array.time() < mjdStart + (7525-10)*second:
  subarray.execute(mjdStart + 7520*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7525*second) + ' since array.time is ' + str(array.time())

# Scan 83 = No0196
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7536*second, mjdStart+7645*second, 'No0196', obsCode, stnCode )
if array.time() < mjdStart + (7645-10)*second:
  subarray.execute(mjdStart + 7640*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7645*second) + ' since array.time is ' + str(array.time())

# Scan 84 = No0197
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7656*second, mjdStart+7705*second, 'No0197', obsCode, stnCode )
if array.time() < mjdStart + (7705-10)*second:
  subarray.execute(mjdStart + 7700*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7705*second) + ' since array.time is ' + str(array.time())

# Scan 85 = No0198
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7717*second, mjdStart+7765*second, 'No0198', obsCode, stnCode )
if array.time() < mjdStart + (7765-10)*second:
  subarray.execute(mjdStart + 7760*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7765*second) + ' since array.time is ' + str(array.time())

# Scan 86 = No0199
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7776*second, mjdStart+7825*second, 'No0199', obsCode, stnCode )
if array.time() < mjdStart + (7825-10)*second:
  subarray.execute(mjdStart + 7820*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7825*second) + ' since array.time is ' + str(array.time())

# Scan 87 = No0200
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7839*second, mjdStart+7884*second, 'No0200', obsCode, stnCode )
if array.time() < mjdStart + (7884-10)*second:
  subarray.execute(mjdStart + 7879*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7884*second) + ' since array.time is ' + str(array.time())

# Scan 88 = No0201
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7898*second, mjdStart+7944*second, 'No0201', obsCode, stnCode )
if array.time() < mjdStart + (7944-10)*second:
  subarray.execute(mjdStart + 7939*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7944*second) + ' since array.time is ' + str(array.time())

# Scan 89 = No0202
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7955*second, mjdStart+8064*second, 'No0202', obsCode, stnCode )
if array.time() < mjdStart + (8064-10)*second:
  subarray.execute(mjdStart + 8059*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8064*second) + ' since array.time is ' + str(array.time())

# Scan 90 = No0203
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8075*second, mjdStart+8124*second, 'No0203', obsCode, stnCode )
if array.time() < mjdStart + (8124-10)*second:
  subarray.execute(mjdStart + 8119*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8124*second) + ' since array.time is ' + str(array.time())

# Scan 91 = No0204
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8135*second, mjdStart+8243*second, 'No0204', obsCode, stnCode )
if array.time() < mjdStart + (8243-10)*second:
  subarray.execute(mjdStart + 8238*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8243*second) + ' since array.time is ' + str(array.time())

# Scan 92 = No0205
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8254*second, mjdStart+8303*second, 'No0205', obsCode, stnCode )
if array.time() < mjdStart + (8303-10)*second:
  subarray.execute(mjdStart + 8298*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8303*second) + ' since array.time is ' + str(array.time())

# Scan 93 = No0206
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8314*second, mjdStart+8423*second, 'No0206', obsCode, stnCode )
if array.time() < mjdStart + (8423-10)*second:
  subarray.execute(mjdStart + 8418*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8423*second) + ' since array.time is ' + str(array.time())

# Scan 94 = No0207
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8434*second, mjdStart+8483*second, 'No0207', obsCode, stnCode )
if array.time() < mjdStart + (8483-10)*second:
  subarray.execute(mjdStart + 8478*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8483*second) + ' since array.time is ' + str(array.time())

# Scan 95 = No0208
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8494*second, mjdStart+8602*second, 'No0208', obsCode, stnCode )
if array.time() < mjdStart + (8602-10)*second:
  subarray.execute(mjdStart + 8597*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8602*second) + ' since array.time is ' + str(array.time())

# Scan 96 = No0209
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8613*second, mjdStart+8662*second, 'No0209', obsCode, stnCode )
if array.time() < mjdStart + (8662-10)*second:
  subarray.execute(mjdStart + 8657*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8662*second) + ' since array.time is ' + str(array.time())

# Scan 97 = No0210
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8673*second, mjdStart+8782*second, 'No0210', obsCode, stnCode )
if array.time() < mjdStart + (8782-10)*second:
  subarray.execute(mjdStart + 8777*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8782*second) + ' since array.time is ' + str(array.time())

# Scan 98 = No0211
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8793*second, mjdStart+8842*second, 'No0211', obsCode, stnCode )
if array.time() < mjdStart + (8842-10)*second:
  subarray.execute(mjdStart + 8837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8842*second) + ' since array.time is ' + str(array.time())

# Scan 99 = No0212
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8853*second, mjdStart+8961*second, 'No0212', obsCode, stnCode )
if array.time() < mjdStart + (8961-10)*second:
  subarray.execute(mjdStart + 8956*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8961*second) + ' since array.time is ' + str(array.time())

# Scan 100 = No0213
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8972*second, mjdStart+9021*second, 'No0213', obsCode, stnCode )
if array.time() < mjdStart + (9021-10)*second:
  subarray.execute(mjdStart + 9016*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9021*second) + ' since array.time is ' + str(array.time())

# Scan 101 = No0214
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9032*second, mjdStart+9141*second, 'No0214', obsCode, stnCode )
if array.time() < mjdStart + (9141-10)*second:
  subarray.execute(mjdStart + 9136*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9141*second) + ' since array.time is ' + str(array.time())

# Scan 102 = No0215
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9152*second, mjdStart+9201*second, 'No0215', obsCode, stnCode )
if array.time() < mjdStart + (9201-10)*second:
  subarray.execute(mjdStart + 9196*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9201*second) + ' since array.time is ' + str(array.time())

# Scan 103 = No0216
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9212*second, mjdStart+9320*second, 'No0216', obsCode, stnCode )
if array.time() < mjdStart + (9320-10)*second:
  subarray.execute(mjdStart + 9315*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9320*second) + ' since array.time is ' + str(array.time())

# Scan 104 = No0217
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9331*second, mjdStart+9380*second, 'No0217', obsCode, stnCode )
if array.time() < mjdStart + (9380-10)*second:
  subarray.execute(mjdStart + 9375*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9380*second) + ' since array.time is ' + str(array.time())

# Scan 105 = No0218
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9391*second, mjdStart+9500*second, 'No0218', obsCode, stnCode )
if array.time() < mjdStart + (9500-10)*second:
  subarray.execute(mjdStart + 9495*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9500*second) + ' since array.time is ' + str(array.time())

# Scan 106 = No0219
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9511*second, mjdStart+9560*second, 'No0219', obsCode, stnCode )
if array.time() < mjdStart + (9560-10)*second:
  subarray.execute(mjdStart + 9555*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9560*second) + ' since array.time is ' + str(array.time())

# Scan 107 = No0220
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9571*second, mjdStart+9679*second, 'No0220', obsCode, stnCode )
if array.time() < mjdStart + (9679-10)*second:
  subarray.execute(mjdStart + 9674*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9679*second) + ' since array.time is ' + str(array.time())

# Scan 108 = No0221
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9690*second, mjdStart+9739*second, 'No0221', obsCode, stnCode )
if array.time() < mjdStart + (9739-10)*second:
  subarray.execute(mjdStart + 9734*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9739*second) + ' since array.time is ' + str(array.time())

# Scan 109 = No0222
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9751*second, mjdStart+9799*second, 'No0222', obsCode, stnCode )
if array.time() < mjdStart + (9799-10)*second:
  subarray.execute(mjdStart + 9794*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9799*second) + ' since array.time is ' + str(array.time())

# Scan 110 = No0223
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9810*second, mjdStart+9859*second, 'No0223', obsCode, stnCode )
if array.time() < mjdStart + (9859-10)*second:
  subarray.execute(mjdStart + 9854*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9859*second) + ' since array.time is ' + str(array.time())

# Scan 111 = No0224
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9872*second, mjdStart+9919*second, 'No0224', obsCode, stnCode )
if array.time() < mjdStart + (9919-10)*second:
  subarray.execute(mjdStart + 9914*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9919*second) + ' since array.time is ' + str(array.time())

# Scan 112 = No0225
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9932*second, mjdStart+9979*second, 'No0225', obsCode, stnCode )
if array.time() < mjdStart + (9979-10)*second:
  subarray.execute(mjdStart + 9974*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9979*second) + ' since array.time is ' + str(array.time())

# Scan 113 = No0226
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9990*second, mjdStart+10098*second, 'No0226', obsCode, stnCode )
if array.time() < mjdStart + (10098-10)*second:
  subarray.execute(mjdStart + 10093*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10098*second) + ' since array.time is ' + str(array.time())

# Scan 114 = No0227
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10109*second, mjdStart+10158*second, 'No0227', obsCode, stnCode )
if array.time() < mjdStart + (10158-10)*second:
  subarray.execute(mjdStart + 10153*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10158*second) + ' since array.time is ' + str(array.time())

# Scan 115 = No0228
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10169*second, mjdStart+10278*second, 'No0228', obsCode, stnCode )
if array.time() < mjdStart + (10278-10)*second:
  subarray.execute(mjdStart + 10273*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10278*second) + ' since array.time is ' + str(array.time())

# Scan 116 = No0229
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10289*second, mjdStart+10338*second, 'No0229', obsCode, stnCode )
if array.time() < mjdStart + (10338-10)*second:
  subarray.execute(mjdStart + 10333*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10338*second) + ' since array.time is ' + str(array.time())

# Scan 117 = No0230
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10349*second, mjdStart+10457*second, 'No0230', obsCode, stnCode )
if array.time() < mjdStart + (10457-10)*second:
  subarray.execute(mjdStart + 10452*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10457*second) + ' since array.time is ' + str(array.time())

# Scan 118 = No0231
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10468*second, mjdStart+10517*second, 'No0231', obsCode, stnCode )
if array.time() < mjdStart + (10517-10)*second:
  subarray.execute(mjdStart + 10512*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10517*second) + ' since array.time is ' + str(array.time())

# Scan 119 = No0232
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10528*second, mjdStart+10637*second, 'No0232', obsCode, stnCode )
if array.time() < mjdStart + (10637-10)*second:
  subarray.execute(mjdStart + 10632*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10637*second) + ' since array.time is ' + str(array.time())

# Scan 120 = No0233
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10648*second, mjdStart+10697*second, 'No0233', obsCode, stnCode )
if array.time() < mjdStart + (10697-10)*second:
  subarray.execute(mjdStart + 10692*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10697*second) + ' since array.time is ' + str(array.time())

# Scan 121 = No0234
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10708*second, mjdStart+10816*second, 'No0234', obsCode, stnCode )
if array.time() < mjdStart + (10816-10)*second:
  subarray.execute(mjdStart + 10811*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10816*second) + ' since array.time is ' + str(array.time())

# Scan 122 = No0235
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10827*second, mjdStart+10876*second, 'No0235', obsCode, stnCode )
if array.time() < mjdStart + (10876-10)*second:
  subarray.execute(mjdStart + 10871*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10876*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 10877*second)
