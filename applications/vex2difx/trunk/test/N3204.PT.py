from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'N3204'
stnCode = 'PT'
mjdStart = 56496 + 23400*second

# File written by vex2script version 0.24 vintage 20131209

dbe0 = RDBE(0, 'pfb', 'PFBG_1_4.bin')
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
loif0.setIf('A', '13cm', 'R', 2900, 'L', 'NA', 0, '13cm', 7900, 2900, 720)
loif0.setIf('B', '4cm', 'R', 7900, 'U', 'NA', 0, '4cm', 7900, 2900, 560)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBEParams(1, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
loif0.setDBERemember(1, 1)
channelSet0 = [ \
  bbc(0, 720, 32, 'L', 2, 0), \  # IF A
  bbc(0, 688, 32, 'L', 2, 0), \  # IF A
  bbc(0, 656, 32, 'L', 2, 0), \  # IF A
  bbc(0, 624, 32, 'L', 2, 0), \  # IF A
  bbc(1, 560, 32, 'L', 2, 0), \  # IF B
  bbc(1, 592, 32, 'L', 2, 0), \  # IF B
  bbc(1, 624, 32, 'L', 2, 0), \  # IF B
  bbc(1, 656, 32, 'L', 2, 0), \  # IF B
  bbc(1, 720, 32, 'L', 2, 0), \  # IF B
  bbc(1, 752, 32, 'L', 2, 0), \  # IF B
  bbc(1, 816, 32, 'L', 2, 0), \  # IF B
  bbc(1, 848, 32, 'L', 2, 0), \  # IF B
  bbc(1, 912, 32, 'L', 2, 0), \  # IF B
  bbc(1, 944, 32, 'L', 2, 0), \  # IF B
  bbc(1, 976, 32, 'L', 2, 0), \  # IF B
  bbc(1, 1008, 32, 'L', 2, 0) \  # IF B
  ]

source0 = Source(0.0862328174574395, 1.28208986492441)
source0.setName('0016+731')

source1 = Source(1.34698428498347, 1.47540702477102)
source1.setName('0454+844')

source2 = Source(1.94689552809086, 1.38216025020028)
source2.setName('0718+792')

source3 = Source(2.87190549676401, 1.41794754248881)
source3.setName('J1058+81')

source4 = Source(3.44002280316295, 0.624426271710679)
source4.setName('1306+360')

source5 = Source(3.45134516119141, 0.568271687316265)
source5.setName('1308+328')

source6 = Source(3.62539328417558, 0.533748788768708)
source6.setName('1348+308')

source7 = Source(3.92258067627274, 0.0745343842690392)
source7.setName('1456+044')

source8 = Source(3.94927846478641, 0.060072354006705)
source8.setName('1502+036')

source9 = Source(4.14293435813425, 0.0456750875314805)
source9.setName('1546+027')

source10 = Source(4.30128333932629, -0.444372109075153)
source10.setName('1622-253')

source11 = Source(4.45445506498244, -0.456945259760361)
source11.setName('1657-261')

source12 = Source(4.55634184311183, 0.576850414255655)
source12.setName('1722+330')

source13 = Source(4.61934124699188, 0.565530434359507)
source13.setName('1736+324')

source14 = Source(4.71418996825925, 0.677336574920263)
source14.setName('1758+388')

source15 = Source(4.72421940833989, -0.692352276902354)
source15.setName('1759-396')

source16 = Source(4.92343481741833, 0.564044859599687)
source16.setName('1846+322')

source17 = Source(5.02642152051373, 0.282679659370251)
source17.setName('1909+161')

source18 = Source(5.08760607009647, 0.368391308641242)
source18.setName('1923+210')

source19 = Source(5.11126338451566, 0.396632173847407)
source19.setName('1929+226')

source20 = Source(5.22724796710286, -0.676345899226635)
source20.setName('1954-388')

source21 = Source(5.24776867272886, 0.262160932462812)
source21.setName('2000+148')

source22 = Source(5.50497456664876, 0.0644381413476692)
source22.setName('2059+034')

source23 = Source(5.63007497548481, -0.165115105866391)
source23.setName('2127-096')

source24 = Source(5.68799491200897, 0.309450694286333)
source24.setName('2141+175')

source25 = Source(5.69550204617875, 0.196481954228537)
source25.setName('2142+110')

source26 = Source(5.70360240285326, 0.165741664523447)
source26.setName('2144+092')

source27 = Source(5.72307100698919, 0.102455454250802)
source27.setName('2149+056')

source28 = Source(5.8931383783612, 1.21779416207866)
source28.setName('2229+695')

# Setup Scan 
# changing to mode rdbe.sx
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 2)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source24)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0002
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4*second, mjdStart+20*second, 'No0002', obsCode, stnCode )
if array.time() < mjdStart + (20-10)*second:
  subarray.execute(mjdStart + 15*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0003
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 145*second, mjdStart+161*second, 'No0003', obsCode, stnCode )
if array.time() < mjdStart + (161-10)*second:
  subarray.execute(mjdStart + 156*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+161*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0004
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 218*second, mjdStart+234*second, 'No0004', obsCode, stnCode )
if array.time() < mjdStart + (234-10)*second:
  subarray.execute(mjdStart + 229*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+234*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0005
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 347*second, mjdStart+363*second, 'No0005', obsCode, stnCode )
if array.time() < mjdStart + (363-10)*second:
  subarray.execute(mjdStart + 358*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+363*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0006
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 545*second, mjdStart+561*second, 'No0006', obsCode, stnCode )
if array.time() < mjdStart + (561-10)*second:
  subarray.execute(mjdStart + 556*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+561*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0007
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 601*second, mjdStart+617*second, 'No0007', obsCode, stnCode )
if array.time() < mjdStart + (617-10)*second:
  subarray.execute(mjdStart + 612*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+617*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0008
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 704*second, mjdStart+720*second, 'No0008', obsCode, stnCode )
if array.time() < mjdStart + (720-10)*second:
  subarray.execute(mjdStart + 715*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+720*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0009
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 748*second, mjdStart+764*second, 'No0009', obsCode, stnCode )
if array.time() < mjdStart + (764-10)*second:
  subarray.execute(mjdStart + 759*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+764*second) + ' since array.time is ' + str(array.time())

# Scan 8 = No0010
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 880*second, mjdStart+896*second, 'No0010', obsCode, stnCode )
if array.time() < mjdStart + (896-10)*second:
  subarray.execute(mjdStart + 891*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+896*second) + ' since array.time is ' + str(array.time())

# Scan 9 = No0011
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 928*second, mjdStart+944*second, 'No0011', obsCode, stnCode )
if array.time() < mjdStart + (944-10)*second:
  subarray.execute(mjdStart + 939*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+944*second) + ' since array.time is ' + str(array.time())

# Scan 10 = No0012
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1071*second, mjdStart+1087*second, 'No0012', obsCode, stnCode )
if array.time() < mjdStart + (1087-10)*second:
  subarray.execute(mjdStart + 1082*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1087*second) + ' since array.time is ' + str(array.time())

# Scan 11 = No0013
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1119*second, mjdStart+1135*second, 'No0013', obsCode, stnCode )
if array.time() < mjdStart + (1135-10)*second:
  subarray.execute(mjdStart + 1130*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1135*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0014
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1233*second, mjdStart+1249*second, 'No0014', obsCode, stnCode )
if array.time() < mjdStart + (1249-10)*second:
  subarray.execute(mjdStart + 1244*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1249*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0015
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1274*second, mjdStart+1290*second, 'No0015', obsCode, stnCode )
if array.time() < mjdStart + (1290-10)*second:
  subarray.execute(mjdStart + 1285*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1290*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0016
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1429*second, mjdStart+1445*second, 'No0016', obsCode, stnCode )
if array.time() < mjdStart + (1445-10)*second:
  subarray.execute(mjdStart + 1440*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1445*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0017
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1466*second, mjdStart+1482*second, 'No0017', obsCode, stnCode )
if array.time() < mjdStart + (1482-10)*second:
  subarray.execute(mjdStart + 1477*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1482*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0018
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1616*second, mjdStart+1632*second, 'No0018', obsCode, stnCode )
if array.time() < mjdStart + (1632-10)*second:
  subarray.execute(mjdStart + 1627*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1632*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0019
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1648*second, mjdStart+1664*second, 'No0019', obsCode, stnCode )
if array.time() < mjdStart + (1664-10)*second:
  subarray.execute(mjdStart + 1659*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1664*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0020
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1688*second, mjdStart+1704*second, 'No0020', obsCode, stnCode )
if array.time() < mjdStart + (1704-10)*second:
  subarray.execute(mjdStart + 1699*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1704*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0021
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1743*second, mjdStart+1759*second, 'No0021', obsCode, stnCode )
if array.time() < mjdStart + (1759-10)*second:
  subarray.execute(mjdStart + 1754*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1759*second) + ' since array.time is ' + str(array.time())

# Scan 20 = No0022
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1881*second, mjdStart+1897*second, 'No0022', obsCode, stnCode )
if array.time() < mjdStart + (1897-10)*second:
  subarray.execute(mjdStart + 1892*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1897*second) + ' since array.time is ' + str(array.time())

# Scan 21 = No0023
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1928*second, mjdStart+1944*second, 'No0023', obsCode, stnCode )
if array.time() < mjdStart + (1944-10)*second:
  subarray.execute(mjdStart + 1939*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1944*second) + ' since array.time is ' + str(array.time())

# Scan 22 = No0024
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2135*second, mjdStart+2151*second, 'No0024', obsCode, stnCode )
if array.time() < mjdStart + (2151-10)*second:
  subarray.execute(mjdStart + 2146*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2151*second) + ' since array.time is ' + str(array.time())

# Scan 23 = No0025
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2165*second, mjdStart+2181*second, 'No0025', obsCode, stnCode )
if array.time() < mjdStart + (2181-10)*second:
  subarray.execute(mjdStart + 2176*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2181*second) + ' since array.time is ' + str(array.time())

# Scan 24 = No0026
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2210*second, mjdStart+2226*second, 'No0026', obsCode, stnCode )
if array.time() < mjdStart + (2226-10)*second:
  subarray.execute(mjdStart + 2221*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2226*second) + ' since array.time is ' + str(array.time())

# Scan 25 = No0027
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2321*second, mjdStart+2337*second, 'No0027', obsCode, stnCode )
if array.time() < mjdStart + (2337-10)*second:
  subarray.execute(mjdStart + 2332*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2337*second) + ' since array.time is ' + str(array.time())

# Scan 26 = No0028
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2373*second, mjdStart+2389*second, 'No0028', obsCode, stnCode )
if array.time() < mjdStart + (2389-10)*second:
  subarray.execute(mjdStart + 2384*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2389*second) + ' since array.time is ' + str(array.time())

# Scan 27 = No0029
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2451*second, mjdStart+2467*second, 'No0029', obsCode, stnCode )
if array.time() < mjdStart + (2467-10)*second:
  subarray.execute(mjdStart + 2462*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2467*second) + ' since array.time is ' + str(array.time())

# Scan 28 = No0030
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2501*second, mjdStart+2517*second, 'No0030', obsCode, stnCode )
if array.time() < mjdStart + (2517-10)*second:
  subarray.execute(mjdStart + 2512*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2517*second) + ' since array.time is ' + str(array.time())

# Scan 29 = No0031
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2536*second, mjdStart+2552*second, 'No0031', obsCode, stnCode )
if array.time() < mjdStart + (2552-10)*second:
  subarray.execute(mjdStart + 2547*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2552*second) + ' since array.time is ' + str(array.time())

# Scan 30 = No0032
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2683*second, mjdStart+2699*second, 'No0032', obsCode, stnCode )
if array.time() < mjdStart + (2699-10)*second:
  subarray.execute(mjdStart + 2694*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2699*second) + ' since array.time is ' + str(array.time())

# Scan 31 = No0033
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2719*second, mjdStart+2735*second, 'No0033', obsCode, stnCode )
if array.time() < mjdStart + (2735-10)*second:
  subarray.execute(mjdStart + 2730*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2735*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 2736*second)
