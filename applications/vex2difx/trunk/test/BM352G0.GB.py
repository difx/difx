import os

isAstrid = 0
if 1:
    try:
        if os.getenv('ASTRIDVLBA') == '1':
            isAstrid = 1
    except:
        pass

if not isAstrid:
    from edu.nrao.evla.observe import Mark5C
    from edu.nrao.evla.observe import MatrixSwitch
    from edu.nrao.evla.observe import RDBE
    from edu.nrao.evla.observe import VLBALoIfSetup
    from edu.nrao.evla.observe import Parameters
    from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'BM352G0'
stnCode = 'GB'
mjdStart = 56493 + 29700*second

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

loif0 = VLBALoIfSetup()
loif0.setIf('A', '4cm', 'R', 9136, 'L', 'NA', 0, '4cm', 0, 0, 848)
loif0.setIf('C', '4cm', 'L', 9136, 'L', 'NA', 0, '4cm', 0, 0, 848)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBEParams(1, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
loif0.setDBERemember(1, 1)
channelSet0 = [ \
  bbc(0, 848, 32, 'L', 2, 0),   # IF A
  bbc(1, 848, 32, 'L', 2, 0),   # IF C
  bbc(0, 816, 32, 'L', 2, 0),   # IF A
  bbc(1, 816, 32, 'L', 2, 0),   # IF C
  bbc(0, 784, 32, 'L', 2, 0),   # IF A
  bbc(1, 784, 32, 'L', 2, 0),   # IF C
  bbc(0, 752, 32, 'L', 2, 0),   # IF A
  bbc(1, 752, 32, 'L', 2, 0),   # IF C
  bbc(0, 720, 32, 'L', 2, 0),   # IF A
  bbc(1, 720, 32, 'L', 2, 0),   # IF C
  bbc(0, 688, 32, 'L', 2, 0),   # IF A
  bbc(1, 688, 32, 'L', 2, 0),   # IF C
  bbc(0, 656, 32, 'L', 2, 0),   # IF A
  bbc(1, 656, 32, 'L', 2, 0),   # IF C
  bbc(0, 624, 32, 'L', 2, 0),   # IF A
  bbc(1, 624, 32, 'L', 2, 0)   # IF C
  ]

source0 = Source(0.978846770680455, 0.43277172496923)
source0.setName('HII314')

source1 = Source(0.994627631260716, 0.413038590150661)
source1.setName('0347+2339')

source2 = Source(1.03567909706364, 0.407213557553677)
source2.setName('0357+2319')

source3 = Source(0.600016079799567, 1.28850203093598)
source3.setName('0217+734')

source4 = Source(1.55121995817696, 0.694879400940061)
source4.setName('0555+394')

source5 = Source(0.273853968535629, 1.01932627502206)
source5.setName('0102+582')

source6 = Source(0.544718270674844, 0.265925362925417)
source6.setName('0204+151')

source7 = Source(2.33356883625791, 0.350959730829113)
source7.setName('0854+200')

source8 = Source(1.06069375018784, 0.45379289144397)
source8.setName('0403+260')

# Setup Scan 
# changing to mode v4cm-phaseref
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 3)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source3)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 0*second, mjdStart+120*second, 'No0001', obsCode, stnCode )
if array.time() < mjdStart + (120-10)*second:
  subarray.execute(mjdStart + 115*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+120*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0002
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 227*second, mjdStart+352*second, 'No0002', obsCode, stnCode )
if array.time() < mjdStart + (352-10)*second:
  subarray.execute(mjdStart + 347*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+352*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0003
# pointing scan for the GBT
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 522*second, mjdStart+1132*second, 'No0003', obsCode, stnCode )
if isAstrid:
  source8.setPeak(True)
if array.time() < mjdStart + (1132-10)*second:
  subarray.execute(mjdStart + 1127*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1132*second) + ' since array.time is ' + str(array.time())
if isAstrid:
  source8.setPeak(False)

# Scan 3 = No0004
# Antenna GB not in scan No0004

# Scan 4 = No0005
# Antenna GB not in scan No0005

# Scan 5 = No0006
# Antenna GB not in scan No0006

# Scan 6 = No0007
# Antenna GB not in scan No0007

# Scan 7 = No0008
# Antenna GB not in scan No0008

# Scan 8 = No0009
# Antenna GB not in scan No0009

# Scan 9 = No0010
# Antenna GB not in scan No0010

# Scan 10 = No0011
# Antenna GB not in scan No0011

# Scan 11 = No0012
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1173*second, mjdStart+1222*second, 'No0012', obsCode, stnCode )
if array.time() < mjdStart + (1222-10)*second:
  subarray.execute(mjdStart + 1217*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1222*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0013
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1252*second, mjdStart+1307*second, 'No0013', obsCode, stnCode )
if array.time() < mjdStart + (1307-10)*second:
  subarray.execute(mjdStart + 1302*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1307*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0014
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1337*second, mjdStart+1352*second, 'No0014', obsCode, stnCode )
if array.time() < mjdStart + (1352-10)*second:
  subarray.execute(mjdStart + 1347*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1352*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0015
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1382*second, mjdStart+1437*second, 'No0015', obsCode, stnCode )
if array.time() < mjdStart + (1437-10)*second:
  subarray.execute(mjdStart + 1432*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1437*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0016
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1467*second, mjdStart+1482*second, 'No0016', obsCode, stnCode )
if array.time() < mjdStart + (1482-10)*second:
  subarray.execute(mjdStart + 1477*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1482*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0017
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1512*second, mjdStart+1567*second, 'No0017', obsCode, stnCode )
if array.time() < mjdStart + (1567-10)*second:
  subarray.execute(mjdStart + 1562*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1567*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0018
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1597*second, mjdStart+1612*second, 'No0018', obsCode, stnCode )
if array.time() < mjdStart + (1612-10)*second:
  subarray.execute(mjdStart + 1607*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1612*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0019
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1642*second, mjdStart+1697*second, 'No0019', obsCode, stnCode )
if array.time() < mjdStart + (1697-10)*second:
  subarray.execute(mjdStart + 1692*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1697*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0020
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1727*second, mjdStart+1742*second, 'No0020', obsCode, stnCode )
if array.time() < mjdStart + (1742-10)*second:
  subarray.execute(mjdStart + 1737*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1742*second) + ' since array.time is ' + str(array.time())

# Scan 20 = No0021
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1772*second, mjdStart+1827*second, 'No0021', obsCode, stnCode )
if array.time() < mjdStart + (1827-10)*second:
  subarray.execute(mjdStart + 1822*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1827*second) + ' since array.time is ' + str(array.time())

# Scan 21 = No0022
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1857*second, mjdStart+1872*second, 'No0022', obsCode, stnCode )
if array.time() < mjdStart + (1872-10)*second:
  subarray.execute(mjdStart + 1867*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1872*second) + ' since array.time is ' + str(array.time())

# Scan 22 = No0023
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1902*second, mjdStart+1957*second, 'No0023', obsCode, stnCode )
if array.time() < mjdStart + (1957-10)*second:
  subarray.execute(mjdStart + 1952*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1957*second) + ' since array.time is ' + str(array.time())

# Scan 23 = No0024
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1987*second, mjdStart+2002*second, 'No0024', obsCode, stnCode )
if array.time() < mjdStart + (2002-10)*second:
  subarray.execute(mjdStart + 1997*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2002*second) + ' since array.time is ' + str(array.time())

# Scan 24 = No0025
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2032*second, mjdStart+2087*second, 'No0025', obsCode, stnCode )
if array.time() < mjdStart + (2087-10)*second:
  subarray.execute(mjdStart + 2082*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2087*second) + ' since array.time is ' + str(array.time())

# Scan 25 = No0026
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2117*second, mjdStart+2132*second, 'No0026', obsCode, stnCode )
if array.time() < mjdStart + (2132-10)*second:
  subarray.execute(mjdStart + 2127*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2132*second) + ' since array.time is ' + str(array.time())

# Scan 26 = No0027
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2162*second, mjdStart+2217*second, 'No0027', obsCode, stnCode )
if array.time() < mjdStart + (2217-10)*second:
  subarray.execute(mjdStart + 2212*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2217*second) + ' since array.time is ' + str(array.time())

# Scan 27 = No0028
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2247*second, mjdStart+2262*second, 'No0028', obsCode, stnCode )
if array.time() < mjdStart + (2262-10)*second:
  subarray.execute(mjdStart + 2257*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2262*second) + ' since array.time is ' + str(array.time())

# Scan 28 = No0029
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2292*second, mjdStart+2347*second, 'No0029', obsCode, stnCode )
if array.time() < mjdStart + (2347-10)*second:
  subarray.execute(mjdStart + 2342*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2347*second) + ' since array.time is ' + str(array.time())

# Scan 29 = No0030
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2377*second, mjdStart+2392*second, 'No0030', obsCode, stnCode )
if array.time() < mjdStart + (2392-10)*second:
  subarray.execute(mjdStart + 2387*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2392*second) + ' since array.time is ' + str(array.time())

# Scan 30 = No0031
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2422*second, mjdStart+2477*second, 'No0031', obsCode, stnCode )
if array.time() < mjdStart + (2477-10)*second:
  subarray.execute(mjdStart + 2472*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2477*second) + ' since array.time is ' + str(array.time())

# Scan 31 = No0032
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2507*second, mjdStart+2522*second, 'No0032', obsCode, stnCode )
if array.time() < mjdStart + (2522-10)*second:
  subarray.execute(mjdStart + 2517*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2522*second) + ' since array.time is ' + str(array.time())

# Scan 32 = No0033
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2552*second, mjdStart+2607*second, 'No0033', obsCode, stnCode )
if array.time() < mjdStart + (2607-10)*second:
  subarray.execute(mjdStart + 2602*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2607*second) + ' since array.time is ' + str(array.time())

# Scan 33 = No0034
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2637*second, mjdStart+2652*second, 'No0034', obsCode, stnCode )
if array.time() < mjdStart + (2652-10)*second:
  subarray.execute(mjdStart + 2647*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2652*second) + ' since array.time is ' + str(array.time())

# Scan 34 = No0035
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2682*second, mjdStart+2737*second, 'No0035', obsCode, stnCode )
if array.time() < mjdStart + (2737-10)*second:
  subarray.execute(mjdStart + 2732*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2737*second) + ' since array.time is ' + str(array.time())

# Scan 35 = No0036
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2767*second, mjdStart+2782*second, 'No0036', obsCode, stnCode )
if array.time() < mjdStart + (2782-10)*second:
  subarray.execute(mjdStart + 2777*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2782*second) + ' since array.time is ' + str(array.time())

# Scan 36 = No0037
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2812*second, mjdStart+2867*second, 'No0037', obsCode, stnCode )
if array.time() < mjdStart + (2867-10)*second:
  subarray.execute(mjdStart + 2862*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2867*second) + ' since array.time is ' + str(array.time())

# Scan 37 = No0038
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2897*second, mjdStart+2912*second, 'No0038', obsCode, stnCode )
if array.time() < mjdStart + (2912-10)*second:
  subarray.execute(mjdStart + 2907*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2912*second) + ' since array.time is ' + str(array.time())

# Scan 38 = No0039
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2942*second, mjdStart+2997*second, 'No0039', obsCode, stnCode )
if array.time() < mjdStart + (2997-10)*second:
  subarray.execute(mjdStart + 2992*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2997*second) + ' since array.time is ' + str(array.time())

# Scan 39 = No0040
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3027*second, mjdStart+3042*second, 'No0040', obsCode, stnCode )
if array.time() < mjdStart + (3042-10)*second:
  subarray.execute(mjdStart + 3037*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3042*second) + ' since array.time is ' + str(array.time())

# Scan 40 = No0041
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3072*second, mjdStart+3127*second, 'No0041', obsCode, stnCode )
if array.time() < mjdStart + (3127-10)*second:
  subarray.execute(mjdStart + 3122*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3127*second) + ' since array.time is ' + str(array.time())

# Scan 41 = No0042
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3157*second, mjdStart+3172*second, 'No0042', obsCode, stnCode )
if array.time() < mjdStart + (3172-10)*second:
  subarray.execute(mjdStart + 3167*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3172*second) + ' since array.time is ' + str(array.time())

# Scan 42 = No0043
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3202*second, mjdStart+3257*second, 'No0043', obsCode, stnCode )
if array.time() < mjdStart + (3257-10)*second:
  subarray.execute(mjdStart + 3252*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3257*second) + ' since array.time is ' + str(array.time())

# Scan 43 = No0044
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3287*second, mjdStart+3302*second, 'No0044', obsCode, stnCode )
if array.time() < mjdStart + (3302-10)*second:
  subarray.execute(mjdStart + 3297*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3302*second) + ' since array.time is ' + str(array.time())

# Scan 44 = No0045
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3332*second, mjdStart+3387*second, 'No0045', obsCode, stnCode )
if array.time() < mjdStart + (3387-10)*second:
  subarray.execute(mjdStart + 3382*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3387*second) + ' since array.time is ' + str(array.time())

# Scan 45 = No0046
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3417*second, mjdStart+3432*second, 'No0046', obsCode, stnCode )
if array.time() < mjdStart + (3432-10)*second:
  subarray.execute(mjdStart + 3427*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3432*second) + ' since array.time is ' + str(array.time())

# Scan 46 = No0047
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3462*second, mjdStart+3517*second, 'No0047', obsCode, stnCode )
if array.time() < mjdStart + (3517-10)*second:
  subarray.execute(mjdStart + 3512*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3517*second) + ' since array.time is ' + str(array.time())

# Scan 47 = No0048
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3547*second, mjdStart+3562*second, 'No0048', obsCode, stnCode )
if array.time() < mjdStart + (3562-10)*second:
  subarray.execute(mjdStart + 3557*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3562*second) + ' since array.time is ' + str(array.time())

# Scan 48 = No0049
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3592*second, mjdStart+3647*second, 'No0049', obsCode, stnCode )
if array.time() < mjdStart + (3647-10)*second:
  subarray.execute(mjdStart + 3642*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3647*second) + ' since array.time is ' + str(array.time())

# Scan 49 = No0050
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3677*second, mjdStart+3692*second, 'No0050', obsCode, stnCode )
if array.time() < mjdStart + (3692-10)*second:
  subarray.execute(mjdStart + 3687*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3692*second) + ' since array.time is ' + str(array.time())

# Scan 50 = No0051
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3722*second, mjdStart+3777*second, 'No0051', obsCode, stnCode )
if array.time() < mjdStart + (3777-10)*second:
  subarray.execute(mjdStart + 3772*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3777*second) + ' since array.time is ' + str(array.time())

# Scan 51 = No0052
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3807*second, mjdStart+3822*second, 'No0052', obsCode, stnCode )
if array.time() < mjdStart + (3822-10)*second:
  subarray.execute(mjdStart + 3817*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3822*second) + ' since array.time is ' + str(array.time())

# Scan 52 = No0053
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3852*second, mjdStart+3907*second, 'No0053', obsCode, stnCode )
if array.time() < mjdStart + (3907-10)*second:
  subarray.execute(mjdStart + 3902*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3907*second) + ' since array.time is ' + str(array.time())

# Scan 53 = No0054
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3937*second, mjdStart+3952*second, 'No0054', obsCode, stnCode )
if array.time() < mjdStart + (3952-10)*second:
  subarray.execute(mjdStart + 3947*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3952*second) + ' since array.time is ' + str(array.time())

# Scan 54 = No0055
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3985*second, mjdStart+4037*second, 'No0055', obsCode, stnCode )
if array.time() < mjdStart + (4037-10)*second:
  subarray.execute(mjdStart + 4032*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4037*second) + ' since array.time is ' + str(array.time())

# Scan 55 = No0056
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4070*second, mjdStart+4152*second, 'No0056', obsCode, stnCode )
if array.time() < mjdStart + (4152-10)*second:
  subarray.execute(mjdStart + 4147*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4152*second) + ' since array.time is ' + str(array.time())

# Scan 56 = No0057
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4182*second, mjdStart+4237*second, 'No0057', obsCode, stnCode )
if array.time() < mjdStart + (4237-10)*second:
  subarray.execute(mjdStart + 4232*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4237*second) + ' since array.time is ' + str(array.time())

# Scan 57 = No0058
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4267*second, mjdStart+4282*second, 'No0058', obsCode, stnCode )
if array.time() < mjdStart + (4282-10)*second:
  subarray.execute(mjdStart + 4277*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4282*second) + ' since array.time is ' + str(array.time())

# Scan 58 = No0059
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4312*second, mjdStart+4367*second, 'No0059', obsCode, stnCode )
if array.time() < mjdStart + (4367-10)*second:
  subarray.execute(mjdStart + 4362*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4367*second) + ' since array.time is ' + str(array.time())

# Scan 59 = No0060
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4397*second, mjdStart+4412*second, 'No0060', obsCode, stnCode )
if array.time() < mjdStart + (4412-10)*second:
  subarray.execute(mjdStart + 4407*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4412*second) + ' since array.time is ' + str(array.time())

# Scan 60 = No0061
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4442*second, mjdStart+4497*second, 'No0061', obsCode, stnCode )
if array.time() < mjdStart + (4497-10)*second:
  subarray.execute(mjdStart + 4492*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4497*second) + ' since array.time is ' + str(array.time())

# Scan 61 = No0062
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4527*second, mjdStart+4542*second, 'No0062', obsCode, stnCode )
if array.time() < mjdStart + (4542-10)*second:
  subarray.execute(mjdStart + 4537*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4542*second) + ' since array.time is ' + str(array.time())

# Scan 62 = No0063
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4572*second, mjdStart+4627*second, 'No0063', obsCode, stnCode )
if array.time() < mjdStart + (4627-10)*second:
  subarray.execute(mjdStart + 4622*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4627*second) + ' since array.time is ' + str(array.time())

# Scan 63 = No0064
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4657*second, mjdStart+4672*second, 'No0064', obsCode, stnCode )
if array.time() < mjdStart + (4672-10)*second:
  subarray.execute(mjdStart + 4667*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4672*second) + ' since array.time is ' + str(array.time())

# Scan 64 = No0065
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4702*second, mjdStart+4757*second, 'No0065', obsCode, stnCode )
if array.time() < mjdStart + (4757-10)*second:
  subarray.execute(mjdStart + 4752*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4757*second) + ' since array.time is ' + str(array.time())

# Scan 65 = No0066
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4787*second, mjdStart+4802*second, 'No0066', obsCode, stnCode )
if array.time() < mjdStart + (4802-10)*second:
  subarray.execute(mjdStart + 4797*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4802*second) + ' since array.time is ' + str(array.time())

# Scan 66 = No0067
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4832*second, mjdStart+4887*second, 'No0067', obsCode, stnCode )
if array.time() < mjdStart + (4887-10)*second:
  subarray.execute(mjdStart + 4882*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4887*second) + ' since array.time is ' + str(array.time())

# Scan 67 = No0068
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4917*second, mjdStart+4932*second, 'No0068', obsCode, stnCode )
if array.time() < mjdStart + (4932-10)*second:
  subarray.execute(mjdStart + 4927*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4932*second) + ' since array.time is ' + str(array.time())

# Scan 68 = No0069
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4962*second, mjdStart+5017*second, 'No0069', obsCode, stnCode )
if array.time() < mjdStart + (5017-10)*second:
  subarray.execute(mjdStart + 5012*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5017*second) + ' since array.time is ' + str(array.time())

# Scan 69 = No0070
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5047*second, mjdStart+5062*second, 'No0070', obsCode, stnCode )
if array.time() < mjdStart + (5062-10)*second:
  subarray.execute(mjdStart + 5057*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5062*second) + ' since array.time is ' + str(array.time())

# Scan 70 = No0071
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5092*second, mjdStart+5147*second, 'No0071', obsCode, stnCode )
if array.time() < mjdStart + (5147-10)*second:
  subarray.execute(mjdStart + 5142*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5147*second) + ' since array.time is ' + str(array.time())

# Scan 71 = No0072
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5177*second, mjdStart+5192*second, 'No0072', obsCode, stnCode )
if array.time() < mjdStart + (5192-10)*second:
  subarray.execute(mjdStart + 5187*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5192*second) + ' since array.time is ' + str(array.time())

# Scan 72 = No0073
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5222*second, mjdStart+5277*second, 'No0073', obsCode, stnCode )
if array.time() < mjdStart + (5277-10)*second:
  subarray.execute(mjdStart + 5272*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5277*second) + ' since array.time is ' + str(array.time())

# Scan 73 = No0074
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5307*second, mjdStart+5322*second, 'No0074', obsCode, stnCode )
if array.time() < mjdStart + (5322-10)*second:
  subarray.execute(mjdStart + 5317*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5322*second) + ' since array.time is ' + str(array.time())

# Scan 74 = No0075
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5352*second, mjdStart+5407*second, 'No0075', obsCode, stnCode )
if array.time() < mjdStart + (5407-10)*second:
  subarray.execute(mjdStart + 5402*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5407*second) + ' since array.time is ' + str(array.time())

# Scan 75 = No0076
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5437*second, mjdStart+5452*second, 'No0076', obsCode, stnCode )
if array.time() < mjdStart + (5452-10)*second:
  subarray.execute(mjdStart + 5447*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5452*second) + ' since array.time is ' + str(array.time())

# Scan 76 = No0077
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5482*second, mjdStart+5537*second, 'No0077', obsCode, stnCode )
if array.time() < mjdStart + (5537-10)*second:
  subarray.execute(mjdStart + 5532*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5537*second) + ' since array.time is ' + str(array.time())

# Scan 77 = No0078
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5567*second, mjdStart+5582*second, 'No0078', obsCode, stnCode )
if array.time() < mjdStart + (5582-10)*second:
  subarray.execute(mjdStart + 5577*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5582*second) + ' since array.time is ' + str(array.time())

# Scan 78 = No0079
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5612*second, mjdStart+5667*second, 'No0079', obsCode, stnCode )
if array.time() < mjdStart + (5667-10)*second:
  subarray.execute(mjdStart + 5662*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5667*second) + ' since array.time is ' + str(array.time())

# Scan 79 = No0080
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5697*second, mjdStart+5712*second, 'No0080', obsCode, stnCode )
if array.time() < mjdStart + (5712-10)*second:
  subarray.execute(mjdStart + 5707*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5712*second) + ' since array.time is ' + str(array.time())

# Scan 80 = No0081
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5742*second, mjdStart+5797*second, 'No0081', obsCode, stnCode )
if array.time() < mjdStart + (5797-10)*second:
  subarray.execute(mjdStart + 5792*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5797*second) + ' since array.time is ' + str(array.time())

# Scan 81 = No0082
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5827*second, mjdStart+5842*second, 'No0082', obsCode, stnCode )
if array.time() < mjdStart + (5842-10)*second:
  subarray.execute(mjdStart + 5837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5842*second) + ' since array.time is ' + str(array.time())

# Scan 82 = No0083
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5872*second, mjdStart+5927*second, 'No0083', obsCode, stnCode )
if array.time() < mjdStart + (5927-10)*second:
  subarray.execute(mjdStart + 5922*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5927*second) + ' since array.time is ' + str(array.time())

# Scan 83 = No0084
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5957*second, mjdStart+5972*second, 'No0084', obsCode, stnCode )
if array.time() < mjdStart + (5972-10)*second:
  subarray.execute(mjdStart + 5967*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5972*second) + ' since array.time is ' + str(array.time())

# Scan 84 = No0085
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6002*second, mjdStart+6057*second, 'No0085', obsCode, stnCode )
if array.time() < mjdStart + (6057-10)*second:
  subarray.execute(mjdStart + 6052*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6057*second) + ' since array.time is ' + str(array.time())

# Scan 85 = No0086
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6087*second, mjdStart+6102*second, 'No0086', obsCode, stnCode )
if array.time() < mjdStart + (6102-10)*second:
  subarray.execute(mjdStart + 6097*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6102*second) + ' since array.time is ' + str(array.time())

# Scan 86 = No0087
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6132*second, mjdStart+6187*second, 'No0087', obsCode, stnCode )
if array.time() < mjdStart + (6187-10)*second:
  subarray.execute(mjdStart + 6182*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6187*second) + ' since array.time is ' + str(array.time())

# Scan 87 = No0088
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6217*second, mjdStart+6232*second, 'No0088', obsCode, stnCode )
if array.time() < mjdStart + (6232-10)*second:
  subarray.execute(mjdStart + 6227*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6232*second) + ' since array.time is ' + str(array.time())

# Scan 88 = No0089
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6262*second, mjdStart+6317*second, 'No0089', obsCode, stnCode )
if array.time() < mjdStart + (6317-10)*second:
  subarray.execute(mjdStart + 6312*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6317*second) + ' since array.time is ' + str(array.time())

# Scan 89 = No0090
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6347*second, mjdStart+6362*second, 'No0090', obsCode, stnCode )
if array.time() < mjdStart + (6362-10)*second:
  subarray.execute(mjdStart + 6357*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6362*second) + ' since array.time is ' + str(array.time())

# Scan 90 = No0091
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6392*second, mjdStart+6447*second, 'No0091', obsCode, stnCode )
if array.time() < mjdStart + (6447-10)*second:
  subarray.execute(mjdStart + 6442*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6447*second) + ' since array.time is ' + str(array.time())

# Scan 91 = No0092
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6477*second, mjdStart+6492*second, 'No0092', obsCode, stnCode )
if array.time() < mjdStart + (6492-10)*second:
  subarray.execute(mjdStart + 6487*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6492*second) + ' since array.time is ' + str(array.time())

# Scan 92 = No0093
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6522*second, mjdStart+6577*second, 'No0093', obsCode, stnCode )
if array.time() < mjdStart + (6577-10)*second:
  subarray.execute(mjdStart + 6572*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6577*second) + ' since array.time is ' + str(array.time())

# Scan 93 = No0094
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6607*second, mjdStart+6622*second, 'No0094', obsCode, stnCode )
if array.time() < mjdStart + (6622-10)*second:
  subarray.execute(mjdStart + 6617*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6622*second) + ' since array.time is ' + str(array.time())

# Scan 94 = No0095
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6652*second, mjdStart+6707*second, 'No0095', obsCode, stnCode )
if array.time() < mjdStart + (6707-10)*second:
  subarray.execute(mjdStart + 6702*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6707*second) + ' since array.time is ' + str(array.time())

# Scan 95 = No0096
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6737*second, mjdStart+6752*second, 'No0096', obsCode, stnCode )
if array.time() < mjdStart + (6752-10)*second:
  subarray.execute(mjdStart + 6747*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6752*second) + ' since array.time is ' + str(array.time())

# Scan 96 = No0097
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6782*second, mjdStart+6867*second, 'No0097', obsCode, stnCode )
if array.time() < mjdStart + (6867-10)*second:
  subarray.execute(mjdStart + 6862*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6867*second) + ' since array.time is ' + str(array.time())

# Scan 97 = No0098
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6897*second, mjdStart+6912*second, 'No0098', obsCode, stnCode )
if array.time() < mjdStart + (6912-10)*second:
  subarray.execute(mjdStart + 6907*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6912*second) + ' since array.time is ' + str(array.time())

# Scan 98 = No0099
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6942*second, mjdStart+6997*second, 'No0099', obsCode, stnCode )
if array.time() < mjdStart + (6997-10)*second:
  subarray.execute(mjdStart + 6992*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6997*second) + ' since array.time is ' + str(array.time())

# Scan 99 = No0100
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7027*second, mjdStart+7042*second, 'No0100', obsCode, stnCode )
if array.time() < mjdStart + (7042-10)*second:
  subarray.execute(mjdStart + 7037*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7042*second) + ' since array.time is ' + str(array.time())

# Scan 100 = No0101
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7072*second, mjdStart+7127*second, 'No0101', obsCode, stnCode )
if array.time() < mjdStart + (7127-10)*second:
  subarray.execute(mjdStart + 7122*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7127*second) + ' since array.time is ' + str(array.time())

# Scan 101 = No0102
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7157*second, mjdStart+7172*second, 'No0102', obsCode, stnCode )
if array.time() < mjdStart + (7172-10)*second:
  subarray.execute(mjdStart + 7167*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7172*second) + ' since array.time is ' + str(array.time())

# Scan 102 = No0103
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7202*second, mjdStart+7257*second, 'No0103', obsCode, stnCode )
if array.time() < mjdStart + (7257-10)*second:
  subarray.execute(mjdStart + 7252*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7257*second) + ' since array.time is ' + str(array.time())

# Scan 103 = No0104
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7287*second, mjdStart+7302*second, 'No0104', obsCode, stnCode )
if array.time() < mjdStart + (7302-10)*second:
  subarray.execute(mjdStart + 7297*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7302*second) + ' since array.time is ' + str(array.time())

# Scan 104 = No0105
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7332*second, mjdStart+7387*second, 'No0105', obsCode, stnCode )
if array.time() < mjdStart + (7387-10)*second:
  subarray.execute(mjdStart + 7382*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7387*second) + ' since array.time is ' + str(array.time())

# Scan 105 = No0106
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7417*second, mjdStart+7432*second, 'No0106', obsCode, stnCode )
if array.time() < mjdStart + (7432-10)*second:
  subarray.execute(mjdStart + 7427*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7432*second) + ' since array.time is ' + str(array.time())

# Scan 106 = No0107
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7462*second, mjdStart+7517*second, 'No0107', obsCode, stnCode )
if array.time() < mjdStart + (7517-10)*second:
  subarray.execute(mjdStart + 7512*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7517*second) + ' since array.time is ' + str(array.time())

# Scan 107 = No0108
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7547*second, mjdStart+7562*second, 'No0108', obsCode, stnCode )
if array.time() < mjdStart + (7562-10)*second:
  subarray.execute(mjdStart + 7557*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7562*second) + ' since array.time is ' + str(array.time())

# Scan 108 = No0109
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7592*second, mjdStart+7647*second, 'No0109', obsCode, stnCode )
if array.time() < mjdStart + (7647-10)*second:
  subarray.execute(mjdStart + 7642*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7647*second) + ' since array.time is ' + str(array.time())

# Scan 109 = No0110
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7677*second, mjdStart+7692*second, 'No0110', obsCode, stnCode )
if array.time() < mjdStart + (7692-10)*second:
  subarray.execute(mjdStart + 7687*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7692*second) + ' since array.time is ' + str(array.time())

# Scan 110 = No0111
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7722*second, mjdStart+7777*second, 'No0111', obsCode, stnCode )
if array.time() < mjdStart + (7777-10)*second:
  subarray.execute(mjdStart + 7772*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7777*second) + ' since array.time is ' + str(array.time())

# Scan 111 = No0112
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7807*second, mjdStart+7822*second, 'No0112', obsCode, stnCode )
if array.time() < mjdStart + (7822-10)*second:
  subarray.execute(mjdStart + 7817*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7822*second) + ' since array.time is ' + str(array.time())

# Scan 112 = No0113
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7852*second, mjdStart+7907*second, 'No0113', obsCode, stnCode )
if array.time() < mjdStart + (7907-10)*second:
  subarray.execute(mjdStart + 7902*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7907*second) + ' since array.time is ' + str(array.time())

# Scan 113 = No0114
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7937*second, mjdStart+7952*second, 'No0114', obsCode, stnCode )
if array.time() < mjdStart + (7952-10)*second:
  subarray.execute(mjdStart + 7947*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7952*second) + ' since array.time is ' + str(array.time())

# Scan 114 = No0115
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7982*second, mjdStart+8037*second, 'No0115', obsCode, stnCode )
if array.time() < mjdStart + (8037-10)*second:
  subarray.execute(mjdStart + 8032*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8037*second) + ' since array.time is ' + str(array.time())

# Scan 115 = No0116
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8067*second, mjdStart+8082*second, 'No0116', obsCode, stnCode )
if array.time() < mjdStart + (8082-10)*second:
  subarray.execute(mjdStart + 8077*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8082*second) + ' since array.time is ' + str(array.time())

# Scan 116 = No0117
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8112*second, mjdStart+8167*second, 'No0117', obsCode, stnCode )
if array.time() < mjdStart + (8167-10)*second:
  subarray.execute(mjdStart + 8162*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8167*second) + ' since array.time is ' + str(array.time())

# Scan 117 = No0118
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8197*second, mjdStart+8212*second, 'No0118', obsCode, stnCode )
if array.time() < mjdStart + (8212-10)*second:
  subarray.execute(mjdStart + 8207*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8212*second) + ' since array.time is ' + str(array.time())

# Scan 118 = No0119
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8242*second, mjdStart+8297*second, 'No0119', obsCode, stnCode )
if array.time() < mjdStart + (8297-10)*second:
  subarray.execute(mjdStart + 8292*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8297*second) + ' since array.time is ' + str(array.time())

# Scan 119 = No0120
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8327*second, mjdStart+8342*second, 'No0120', obsCode, stnCode )
if array.time() < mjdStart + (8342-10)*second:
  subarray.execute(mjdStart + 8337*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8342*second) + ' since array.time is ' + str(array.time())

# Scan 120 = No0121
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8372*second, mjdStart+8427*second, 'No0121', obsCode, stnCode )
if array.time() < mjdStart + (8427-10)*second:
  subarray.execute(mjdStart + 8422*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8427*second) + ' since array.time is ' + str(array.time())

# Scan 121 = No0122
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8457*second, mjdStart+8472*second, 'No0122', obsCode, stnCode )
if array.time() < mjdStart + (8472-10)*second:
  subarray.execute(mjdStart + 8467*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8472*second) + ' since array.time is ' + str(array.time())

# Scan 122 = No0123
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8502*second, mjdStart+8557*second, 'No0123', obsCode, stnCode )
if array.time() < mjdStart + (8557-10)*second:
  subarray.execute(mjdStart + 8552*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8557*second) + ' since array.time is ' + str(array.time())

# Scan 123 = No0124
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8587*second, mjdStart+8602*second, 'No0124', obsCode, stnCode )
if array.time() < mjdStart + (8602-10)*second:
  subarray.execute(mjdStart + 8597*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8602*second) + ' since array.time is ' + str(array.time())

# Scan 124 = No0125
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8632*second, mjdStart+8687*second, 'No0125', obsCode, stnCode )
if array.time() < mjdStart + (8687-10)*second:
  subarray.execute(mjdStart + 8682*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8687*second) + ' since array.time is ' + str(array.time())

# Scan 125 = No0126
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8717*second, mjdStart+8732*second, 'No0126', obsCode, stnCode )
if array.time() < mjdStart + (8732-10)*second:
  subarray.execute(mjdStart + 8727*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8732*second) + ' since array.time is ' + str(array.time())

# Scan 126 = No0127
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8762*second, mjdStart+8817*second, 'No0127', obsCode, stnCode )
if array.time() < mjdStart + (8817-10)*second:
  subarray.execute(mjdStart + 8812*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8817*second) + ' since array.time is ' + str(array.time())

# Scan 127 = No0128
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8847*second, mjdStart+8862*second, 'No0128', obsCode, stnCode )
if array.time() < mjdStart + (8862-10)*second:
  subarray.execute(mjdStart + 8857*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8862*second) + ' since array.time is ' + str(array.time())

# Scan 128 = No0129
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8892*second, mjdStart+8947*second, 'No0129', obsCode, stnCode )
if array.time() < mjdStart + (8947-10)*second:
  subarray.execute(mjdStart + 8942*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8947*second) + ' since array.time is ' + str(array.time())

# Scan 129 = No0130
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8977*second, mjdStart+8992*second, 'No0130', obsCode, stnCode )
if array.time() < mjdStart + (8992-10)*second:
  subarray.execute(mjdStart + 8987*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8992*second) + ' since array.time is ' + str(array.time())

# Scan 130 = No0131
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9022*second, mjdStart+9077*second, 'No0131', obsCode, stnCode )
if array.time() < mjdStart + (9077-10)*second:
  subarray.execute(mjdStart + 9072*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9077*second) + ' since array.time is ' + str(array.time())

# Scan 131 = No0132
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9107*second, mjdStart+9122*second, 'No0132', obsCode, stnCode )
if array.time() < mjdStart + (9122-10)*second:
  subarray.execute(mjdStart + 9117*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9122*second) + ' since array.time is ' + str(array.time())

# Scan 132 = No0133
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9152*second, mjdStart+9207*second, 'No0133', obsCode, stnCode )
if array.time() < mjdStart + (9207-10)*second:
  subarray.execute(mjdStart + 9202*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9207*second) + ' since array.time is ' + str(array.time())

# Scan 133 = No0134
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9237*second, mjdStart+9252*second, 'No0134', obsCode, stnCode )
if array.time() < mjdStart + (9252-10)*second:
  subarray.execute(mjdStart + 9247*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9252*second) + ' since array.time is ' + str(array.time())

# Scan 134 = No0135
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9282*second, mjdStart+9337*second, 'No0135', obsCode, stnCode )
if array.time() < mjdStart + (9337-10)*second:
  subarray.execute(mjdStart + 9332*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9337*second) + ' since array.time is ' + str(array.time())

# Scan 135 = No0136
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9367*second, mjdStart+9382*second, 'No0136', obsCode, stnCode )
if array.time() < mjdStart + (9382-10)*second:
  subarray.execute(mjdStart + 9377*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9382*second) + ' since array.time is ' + str(array.time())

# Scan 136 = No0137
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9412*second, mjdStart+9467*second, 'No0137', obsCode, stnCode )
if array.time() < mjdStart + (9467-10)*second:
  subarray.execute(mjdStart + 9462*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9467*second) + ' since array.time is ' + str(array.time())

# Scan 137 = No0138
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9497*second, mjdStart+9512*second, 'No0138', obsCode, stnCode )
if array.time() < mjdStart + (9512-10)*second:
  subarray.execute(mjdStart + 9507*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9512*second) + ' since array.time is ' + str(array.time())

# Scan 138 = No0139
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9544*second, mjdStart+9575*second, 'No0139', obsCode, stnCode )
if array.time() < mjdStart + (9575-10)*second:
  subarray.execute(mjdStart + 9570*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9575*second) + ' since array.time is ' + str(array.time())

# Scan 139 = No0140
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9619*second, mjdStart+9718*second, 'No0140', obsCode, stnCode )
if array.time() < mjdStart + (9718-10)*second:
  subarray.execute(mjdStart + 9713*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9718*second) + ' since array.time is ' + str(array.time())

# Scan 140 = No0141
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9962*second, mjdStart+10112*second, 'No0141', obsCode, stnCode )
if array.time() < mjdStart + (10112-10)*second:
  subarray.execute(mjdStart + 10107*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10112*second) + ' since array.time is ' + str(array.time())

# Scan 141 = No0142
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10537*second, mjdStart+10582*second, 'No0142', obsCode, stnCode )
if array.time() < mjdStart + (10582-10)*second:
  subarray.execute(mjdStart + 10577*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10582*second) + ' since array.time is ' + str(array.time())

# Scan 142 = No0143
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10612*second, mjdStart+10662*second, 'No0143', obsCode, stnCode )
if array.time() < mjdStart + (10662-10)*second:
  subarray.execute(mjdStart + 10657*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10662*second) + ' since array.time is ' + str(array.time())

# Scan 143 = No0144
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10692*second, mjdStart+10712*second, 'No0144', obsCode, stnCode )
if array.time() < mjdStart + (10712-10)*second:
  subarray.execute(mjdStart + 10707*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10712*second) + ' since array.time is ' + str(array.time())

# Scan 144 = No0145
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10742*second, mjdStart+10782*second, 'No0145', obsCode, stnCode )
if array.time() < mjdStart + (10782-10)*second:
  subarray.execute(mjdStart + 10777*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10782*second) + ' since array.time is ' + str(array.time())

# Scan 145 = No0146
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10812*second, mjdStart+10842*second, 'No0146', obsCode, stnCode )
if array.time() < mjdStart + (10842-10)*second:
  subarray.execute(mjdStart + 10837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10842*second) + ' since array.time is ' + str(array.time())

# Scan 146 = No0147
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10872*second, mjdStart+10912*second, 'No0147', obsCode, stnCode )
if array.time() < mjdStart + (10912-10)*second:
  subarray.execute(mjdStart + 10907*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10912*second) + ' since array.time is ' + str(array.time())

# Scan 147 = No0148
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10942*second, mjdStart+10972*second, 'No0148', obsCode, stnCode )
if array.time() < mjdStart + (10972-10)*second:
  subarray.execute(mjdStart + 10967*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10972*second) + ' since array.time is ' + str(array.time())

# Scan 148 = No0149
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11002*second, mjdStart+11042*second, 'No0149', obsCode, stnCode )
if array.time() < mjdStart + (11042-10)*second:
  subarray.execute(mjdStart + 11037*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11042*second) + ' since array.time is ' + str(array.time())

# Scan 149 = No0150
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11072*second, mjdStart+11102*second, 'No0150', obsCode, stnCode )
if array.time() < mjdStart + (11102-10)*second:
  subarray.execute(mjdStart + 11097*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11102*second) + ' since array.time is ' + str(array.time())

# Scan 150 = No0151
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11132*second, mjdStart+11172*second, 'No0151', obsCode, stnCode )
if array.time() < mjdStart + (11172-10)*second:
  subarray.execute(mjdStart + 11167*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11172*second) + ' since array.time is ' + str(array.time())

# Scan 151 = No0152
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11202*second, mjdStart+11232*second, 'No0152', obsCode, stnCode )
if array.time() < mjdStart + (11232-10)*second:
  subarray.execute(mjdStart + 11227*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11232*second) + ' since array.time is ' + str(array.time())

# Scan 152 = No0153
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11262*second, mjdStart+11302*second, 'No0153', obsCode, stnCode )
if array.time() < mjdStart + (11302-10)*second:
  subarray.execute(mjdStart + 11297*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11302*second) + ' since array.time is ' + str(array.time())

# Scan 153 = No0154
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11332*second, mjdStart+11362*second, 'No0154', obsCode, stnCode )
if array.time() < mjdStart + (11362-10)*second:
  subarray.execute(mjdStart + 11357*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11362*second) + ' since array.time is ' + str(array.time())

# Scan 154 = No0155
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11392*second, mjdStart+11432*second, 'No0155', obsCode, stnCode )
if array.time() < mjdStart + (11432-10)*second:
  subarray.execute(mjdStart + 11427*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11432*second) + ' since array.time is ' + str(array.time())

# Scan 155 = No0156
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11462*second, mjdStart+11492*second, 'No0156', obsCode, stnCode )
if array.time() < mjdStart + (11492-10)*second:
  subarray.execute(mjdStart + 11487*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11492*second) + ' since array.time is ' + str(array.time())

# Scan 156 = No0157
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11523*second, mjdStart+11562*second, 'No0157', obsCode, stnCode )
if array.time() < mjdStart + (11562-10)*second:
  subarray.execute(mjdStart + 11557*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11562*second) + ' since array.time is ' + str(array.time())

# Scan 157 = No0158
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11592*second, mjdStart+11622*second, 'No0158', obsCode, stnCode )
if array.time() < mjdStart + (11622-10)*second:
  subarray.execute(mjdStart + 11617*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11622*second) + ' since array.time is ' + str(array.time())

# Scan 158 = No0159
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11653*second, mjdStart+11752*second, 'No0159', obsCode, stnCode )
if array.time() < mjdStart + (11752-10)*second:
  subarray.execute(mjdStart + 11747*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11752*second) + ' since array.time is ' + str(array.time())

# Scan 159 = No0160
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11782*second, mjdStart+11812*second, 'No0160', obsCode, stnCode )
if array.time() < mjdStart + (11812-10)*second:
  subarray.execute(mjdStart + 11807*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11812*second) + ' since array.time is ' + str(array.time())

# Scan 160 = No0161
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11843*second, mjdStart+11887*second, 'No0161', obsCode, stnCode )
if array.time() < mjdStart + (11887-10)*second:
  subarray.execute(mjdStart + 11882*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11887*second) + ' since array.time is ' + str(array.time())

# Scan 161 = No0162
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11917*second, mjdStart+11942*second, 'No0162', obsCode, stnCode )
if array.time() < mjdStart + (11942-10)*second:
  subarray.execute(mjdStart + 11937*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11942*second) + ' since array.time is ' + str(array.time())

# Scan 162 = No0163
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11973*second, mjdStart+12017*second, 'No0163', obsCode, stnCode )
if array.time() < mjdStart + (12017-10)*second:
  subarray.execute(mjdStart + 12012*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12017*second) + ' since array.time is ' + str(array.time())

# Scan 163 = No0164
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12047*second, mjdStart+12072*second, 'No0164', obsCode, stnCode )
if array.time() < mjdStart + (12072-10)*second:
  subarray.execute(mjdStart + 12067*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12072*second) + ' since array.time is ' + str(array.time())

# Scan 164 = No0165
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12103*second, mjdStart+12147*second, 'No0165', obsCode, stnCode )
if array.time() < mjdStart + (12147-10)*second:
  subarray.execute(mjdStart + 12142*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12147*second) + ' since array.time is ' + str(array.time())

# Scan 165 = No0166
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12177*second, mjdStart+12202*second, 'No0166', obsCode, stnCode )
if array.time() < mjdStart + (12202-10)*second:
  subarray.execute(mjdStart + 12197*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12202*second) + ' since array.time is ' + str(array.time())

# Scan 166 = No0167
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12233*second, mjdStart+12277*second, 'No0167', obsCode, stnCode )
if array.time() < mjdStart + (12277-10)*second:
  subarray.execute(mjdStart + 12272*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12277*second) + ' since array.time is ' + str(array.time())

# Scan 167 = No0168
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12307*second, mjdStart+12332*second, 'No0168', obsCode, stnCode )
if array.time() < mjdStart + (12332-10)*second:
  subarray.execute(mjdStart + 12327*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12332*second) + ' since array.time is ' + str(array.time())

# Scan 168 = No0169
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12363*second, mjdStart+12407*second, 'No0169', obsCode, stnCode )
if array.time() < mjdStart + (12407-10)*second:
  subarray.execute(mjdStart + 12402*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12407*second) + ' since array.time is ' + str(array.time())

# Scan 169 = No0170
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12437*second, mjdStart+12462*second, 'No0170', obsCode, stnCode )
if array.time() < mjdStart + (12462-10)*second:
  subarray.execute(mjdStart + 12457*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12462*second) + ' since array.time is ' + str(array.time())

# Scan 170 = No0171
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12493*second, mjdStart+12537*second, 'No0171', obsCode, stnCode )
if array.time() < mjdStart + (12537-10)*second:
  subarray.execute(mjdStart + 12532*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12537*second) + ' since array.time is ' + str(array.time())

# Scan 171 = No0172
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12567*second, mjdStart+12592*second, 'No0172', obsCode, stnCode )
if array.time() < mjdStart + (12592-10)*second:
  subarray.execute(mjdStart + 12587*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12592*second) + ' since array.time is ' + str(array.time())

# Scan 172 = No0173
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12623*second, mjdStart+12667*second, 'No0173', obsCode, stnCode )
if array.time() < mjdStart + (12667-10)*second:
  subarray.execute(mjdStart + 12662*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12667*second) + ' since array.time is ' + str(array.time())

# Scan 173 = No0174
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12697*second, mjdStart+12722*second, 'No0174', obsCode, stnCode )
if array.time() < mjdStart + (12722-10)*second:
  subarray.execute(mjdStart + 12717*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12722*second) + ' since array.time is ' + str(array.time())

# Scan 174 = No0175
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12758*second, mjdStart+12799*second, 'No0175', obsCode, stnCode )
if array.time() < mjdStart + (12799-10)*second:
  subarray.execute(mjdStart + 12794*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12799*second) + ' since array.time is ' + str(array.time())

# Scan 175 = No0176
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12836*second, mjdStart+12889*second, 'No0176', obsCode, stnCode )
if array.time() < mjdStart + (12889-10)*second:
  subarray.execute(mjdStart + 12884*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12889*second) + ' since array.time is ' + str(array.time())

# Scan 176 = No0177
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12920*second, mjdStart+12964*second, 'No0177', obsCode, stnCode )
if array.time() < mjdStart + (12964-10)*second:
  subarray.execute(mjdStart + 12959*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12964*second) + ' since array.time is ' + str(array.time())

# Scan 177 = No0178
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12994*second, mjdStart+13019*second, 'No0178', obsCode, stnCode )
if array.time() < mjdStart + (13019-10)*second:
  subarray.execute(mjdStart + 13014*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13019*second) + ' since array.time is ' + str(array.time())

# Scan 178 = No0179
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13050*second, mjdStart+13094*second, 'No0179', obsCode, stnCode )
if array.time() < mjdStart + (13094-10)*second:
  subarray.execute(mjdStart + 13089*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13094*second) + ' since array.time is ' + str(array.time())

# Scan 179 = No0180
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13124*second, mjdStart+13149*second, 'No0180', obsCode, stnCode )
if array.time() < mjdStart + (13149-10)*second:
  subarray.execute(mjdStart + 13144*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13149*second) + ' since array.time is ' + str(array.time())

# Scan 180 = No0181
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13180*second, mjdStart+13224*second, 'No0181', obsCode, stnCode )
if array.time() < mjdStart + (13224-10)*second:
  subarray.execute(mjdStart + 13219*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13224*second) + ' since array.time is ' + str(array.time())

# Scan 181 = No0182
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13254*second, mjdStart+13279*second, 'No0182', obsCode, stnCode )
if array.time() < mjdStart + (13279-10)*second:
  subarray.execute(mjdStart + 13274*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13279*second) + ' since array.time is ' + str(array.time())

# Scan 182 = No0183
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13310*second, mjdStart+13354*second, 'No0183', obsCode, stnCode )
if array.time() < mjdStart + (13354-10)*second:
  subarray.execute(mjdStart + 13349*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13354*second) + ' since array.time is ' + str(array.time())

# Scan 183 = No0184
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13384*second, mjdStart+13409*second, 'No0184', obsCode, stnCode )
if array.time() < mjdStart + (13409-10)*second:
  subarray.execute(mjdStart + 13404*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13409*second) + ' since array.time is ' + str(array.time())

# Scan 184 = No0185
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13440*second, mjdStart+13484*second, 'No0185', obsCode, stnCode )
if array.time() < mjdStart + (13484-10)*second:
  subarray.execute(mjdStart + 13479*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13484*second) + ' since array.time is ' + str(array.time())

# Scan 185 = No0186
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13514*second, mjdStart+13539*second, 'No0186', obsCode, stnCode )
if array.time() < mjdStart + (13539-10)*second:
  subarray.execute(mjdStart + 13534*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13539*second) + ' since array.time is ' + str(array.time())

# Scan 186 = No0187
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13570*second, mjdStart+13614*second, 'No0187', obsCode, stnCode )
if array.time() < mjdStart + (13614-10)*second:
  subarray.execute(mjdStart + 13609*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13614*second) + ' since array.time is ' + str(array.time())

# Scan 187 = No0188
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13644*second, mjdStart+13669*second, 'No0188', obsCode, stnCode )
if array.time() < mjdStart + (13669-10)*second:
  subarray.execute(mjdStart + 13664*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13669*second) + ' since array.time is ' + str(array.time())

# Scan 188 = No0189
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13700*second, mjdStart+13744*second, 'No0189', obsCode, stnCode )
if array.time() < mjdStart + (13744-10)*second:
  subarray.execute(mjdStart + 13739*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13744*second) + ' since array.time is ' + str(array.time())

# Scan 189 = No0190
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13774*second, mjdStart+13799*second, 'No0190', obsCode, stnCode )
if array.time() < mjdStart + (13799-10)*second:
  subarray.execute(mjdStart + 13794*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13799*second) + ' since array.time is ' + str(array.time())

# Scan 190 = No0191
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13830*second, mjdStart+13874*second, 'No0191', obsCode, stnCode )
if array.time() < mjdStart + (13874-10)*second:
  subarray.execute(mjdStart + 13869*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13874*second) + ' since array.time is ' + str(array.time())

# Scan 191 = No0192
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13904*second, mjdStart+13929*second, 'No0192', obsCode, stnCode )
if array.time() < mjdStart + (13929-10)*second:
  subarray.execute(mjdStart + 13924*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13929*second) + ' since array.time is ' + str(array.time())

# Scan 192 = No0193
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14022*second, mjdStart+14177*second, 'No0193', obsCode, stnCode )
if array.time() < mjdStart + (14177-10)*second:
  subarray.execute(mjdStart + 14172*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14177*second) + ' since array.time is ' + str(array.time())

# Scan 193 = No0194
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14276*second, mjdStart+14321*second, 'No0194', obsCode, stnCode )
if array.time() < mjdStart + (14321-10)*second:
  subarray.execute(mjdStart + 14316*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14321*second) + ' since array.time is ' + str(array.time())

# Scan 194 = No0195
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14352*second, mjdStart+14401*second, 'No0195', obsCode, stnCode )
if array.time() < mjdStart + (14401-10)*second:
  subarray.execute(mjdStart + 14396*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14401*second) + ' since array.time is ' + str(array.time())

# Scan 195 = No0196
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14431*second, mjdStart+14456*second, 'No0196', obsCode, stnCode )
if array.time() < mjdStart + (14456-10)*second:
  subarray.execute(mjdStart + 14451*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14456*second) + ' since array.time is ' + str(array.time())

# Scan 196 = No0197
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14487*second, mjdStart+14531*second, 'No0197', obsCode, stnCode )
if array.time() < mjdStart + (14531-10)*second:
  subarray.execute(mjdStart + 14526*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14531*second) + ' since array.time is ' + str(array.time())

# Scan 197 = No0198
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14561*second, mjdStart+14586*second, 'No0198', obsCode, stnCode )
if array.time() < mjdStart + (14586-10)*second:
  subarray.execute(mjdStart + 14581*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14586*second) + ' since array.time is ' + str(array.time())

# Scan 198 = No0199
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14617*second, mjdStart+14661*second, 'No0199', obsCode, stnCode )
if array.time() < mjdStart + (14661-10)*second:
  subarray.execute(mjdStart + 14656*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14661*second) + ' since array.time is ' + str(array.time())

# Scan 199 = No0200
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14691*second, mjdStart+14716*second, 'No0200', obsCode, stnCode )
if array.time() < mjdStart + (14716-10)*second:
  subarray.execute(mjdStart + 14711*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14716*second) + ' since array.time is ' + str(array.time())

# Scan 200 = No0201
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14747*second, mjdStart+14791*second, 'No0201', obsCode, stnCode )
if array.time() < mjdStart + (14791-10)*second:
  subarray.execute(mjdStart + 14786*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14791*second) + ' since array.time is ' + str(array.time())

# Scan 201 = No0202
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14821*second, mjdStart+14846*second, 'No0202', obsCode, stnCode )
if array.time() < mjdStart + (14846-10)*second:
  subarray.execute(mjdStart + 14841*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14846*second) + ' since array.time is ' + str(array.time())

# Scan 202 = No0203
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14877*second, mjdStart+14921*second, 'No0203', obsCode, stnCode )
if array.time() < mjdStart + (14921-10)*second:
  subarray.execute(mjdStart + 14916*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14921*second) + ' since array.time is ' + str(array.time())

# Scan 203 = No0204
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14951*second, mjdStart+14976*second, 'No0204', obsCode, stnCode )
if array.time() < mjdStart + (14976-10)*second:
  subarray.execute(mjdStart + 14971*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14976*second) + ' since array.time is ' + str(array.time())

# Scan 204 = No0205
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15007*second, mjdStart+15051*second, 'No0205', obsCode, stnCode )
if array.time() < mjdStart + (15051-10)*second:
  subarray.execute(mjdStart + 15046*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15051*second) + ' since array.time is ' + str(array.time())

# Scan 205 = No0206
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15081*second, mjdStart+15106*second, 'No0206', obsCode, stnCode )
if array.time() < mjdStart + (15106-10)*second:
  subarray.execute(mjdStart + 15101*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15106*second) + ' since array.time is ' + str(array.time())

# Scan 206 = No0207
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15137*second, mjdStart+15181*second, 'No0207', obsCode, stnCode )
if array.time() < mjdStart + (15181-10)*second:
  subarray.execute(mjdStart + 15176*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15181*second) + ' since array.time is ' + str(array.time())

# Scan 207 = No0208
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15211*second, mjdStart+15236*second, 'No0208', obsCode, stnCode )
if array.time() < mjdStart + (15236-10)*second:
  subarray.execute(mjdStart + 15231*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15236*second) + ' since array.time is ' + str(array.time())

# Scan 208 = No0209
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15267*second, mjdStart+15311*second, 'No0209', obsCode, stnCode )
if array.time() < mjdStart + (15311-10)*second:
  subarray.execute(mjdStart + 15306*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15311*second) + ' since array.time is ' + str(array.time())

# Scan 209 = No0210
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15341*second, mjdStart+15366*second, 'No0210', obsCode, stnCode )
if array.time() < mjdStart + (15366-10)*second:
  subarray.execute(mjdStart + 15361*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15366*second) + ' since array.time is ' + str(array.time())

# Scan 210 = No0211
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15410*second, mjdStart+15454*second, 'No0211', obsCode, stnCode )
if array.time() < mjdStart + (15454-10)*second:
  subarray.execute(mjdStart + 15449*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15454*second) + ' since array.time is ' + str(array.time())

# Scan 211 = No0212
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15516*second, mjdStart+15561*second, 'No0212', obsCode, stnCode )
if array.time() < mjdStart + (15561-10)*second:
  subarray.execute(mjdStart + 15556*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15561*second) + ' since array.time is ' + str(array.time())

# Scan 212 = No0213
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15592*second, mjdStart+15641*second, 'No0213', obsCode, stnCode )
if array.time() < mjdStart + (15641-10)*second:
  subarray.execute(mjdStart + 15636*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15641*second) + ' since array.time is ' + str(array.time())

# Scan 213 = No0214
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15671*second, mjdStart+15701*second, 'No0214', obsCode, stnCode )
if array.time() < mjdStart + (15701-10)*second:
  subarray.execute(mjdStart + 15696*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15701*second) + ' since array.time is ' + str(array.time())

# Scan 214 = No0215
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15732*second, mjdStart+15776*second, 'No0215', obsCode, stnCode )
if array.time() < mjdStart + (15776-10)*second:
  subarray.execute(mjdStart + 15771*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15776*second) + ' since array.time is ' + str(array.time())

# Scan 215 = No0216
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15806*second, mjdStart+15831*second, 'No0216', obsCode, stnCode )
if array.time() < mjdStart + (15831-10)*second:
  subarray.execute(mjdStart + 15826*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15831*second) + ' since array.time is ' + str(array.time())

# Scan 216 = No0217
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15862*second, mjdStart+15906*second, 'No0217', obsCode, stnCode )
if array.time() < mjdStart + (15906-10)*second:
  subarray.execute(mjdStart + 15901*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15906*second) + ' since array.time is ' + str(array.time())

# Scan 217 = No0218
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15936*second, mjdStart+15961*second, 'No0218', obsCode, stnCode )
if array.time() < mjdStart + (15961-10)*second:
  subarray.execute(mjdStart + 15956*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15961*second) + ' since array.time is ' + str(array.time())

# Scan 218 = No0219
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15992*second, mjdStart+16036*second, 'No0219', obsCode, stnCode )
if array.time() < mjdStart + (16036-10)*second:
  subarray.execute(mjdStart + 16031*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16036*second) + ' since array.time is ' + str(array.time())

# Scan 219 = No0220
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16066*second, mjdStart+16091*second, 'No0220', obsCode, stnCode )
if array.time() < mjdStart + (16091-10)*second:
  subarray.execute(mjdStart + 16086*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16091*second) + ' since array.time is ' + str(array.time())

# Scan 220 = No0221
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16122*second, mjdStart+16166*second, 'No0221', obsCode, stnCode )
if array.time() < mjdStart + (16166-10)*second:
  subarray.execute(mjdStart + 16161*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16166*second) + ' since array.time is ' + str(array.time())

# Scan 221 = No0222
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16196*second, mjdStart+16221*second, 'No0222', obsCode, stnCode )
if array.time() < mjdStart + (16221-10)*second:
  subarray.execute(mjdStart + 16216*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16221*second) + ' since array.time is ' + str(array.time())

# Scan 222 = No0223
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16252*second, mjdStart+16296*second, 'No0223', obsCode, stnCode )
if array.time() < mjdStart + (16296-10)*second:
  subarray.execute(mjdStart + 16291*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16296*second) + ' since array.time is ' + str(array.time())

# Scan 223 = No0224
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16326*second, mjdStart+16351*second, 'No0224', obsCode, stnCode )
if array.time() < mjdStart + (16351-10)*second:
  subarray.execute(mjdStart + 16346*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16351*second) + ' since array.time is ' + str(array.time())

# Scan 224 = No0225
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16383*second, mjdStart+16426*second, 'No0225', obsCode, stnCode )
if array.time() < mjdStart + (16426-10)*second:
  subarray.execute(mjdStart + 16421*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16426*second) + ' since array.time is ' + str(array.time())

# Scan 225 = No0226
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16457*second, mjdStart+16481*second, 'No0226', obsCode, stnCode )
if array.time() < mjdStart + (16481-10)*second:
  subarray.execute(mjdStart + 16476*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16481*second) + ' since array.time is ' + str(array.time())

# Scan 226 = No0227
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16529*second, mjdStart+16560*second, 'No0227', obsCode, stnCode )
if array.time() < mjdStart + (16560-10)*second:
  subarray.execute(mjdStart + 16555*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16560*second) + ' since array.time is ' + str(array.time())

# Scan 227 = No0228
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16612*second, mjdStart+16660*second, 'No0228', obsCode, stnCode )
if array.time() < mjdStart + (16660-10)*second:
  subarray.execute(mjdStart + 16655*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16660*second) + ' since array.time is ' + str(array.time())

# Scan 228 = No0229
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16693*second, mjdStart+16740*second, 'No0229', obsCode, stnCode )
if array.time() < mjdStart + (16740-10)*second:
  subarray.execute(mjdStart + 16735*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16740*second) + ' since array.time is ' + str(array.time())

# Scan 229 = No0230
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16772*second, mjdStart+16790*second, 'No0230', obsCode, stnCode )
if array.time() < mjdStart + (16790-10)*second:
  subarray.execute(mjdStart + 16785*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16790*second) + ' since array.time is ' + str(array.time())

# Scan 230 = No0231
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16824*second, mjdStart+16870*second, 'No0231', obsCode, stnCode )
if array.time() < mjdStart + (16870-10)*second:
  subarray.execute(mjdStart + 16865*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16870*second) + ' since array.time is ' + str(array.time())

# Scan 231 = No0232
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16903*second, mjdStart+16920*second, 'No0232', obsCode, stnCode )
if array.time() < mjdStart + (16920-10)*second:
  subarray.execute(mjdStart + 16915*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16920*second) + ' since array.time is ' + str(array.time())

# Scan 232 = No0233
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16954*second, mjdStart+17000*second, 'No0233', obsCode, stnCode )
if array.time() < mjdStart + (17000-10)*second:
  subarray.execute(mjdStart + 16995*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17000*second) + ' since array.time is ' + str(array.time())

# Scan 233 = No0234
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17033*second, mjdStart+17050*second, 'No0234', obsCode, stnCode )
if array.time() < mjdStart + (17050-10)*second:
  subarray.execute(mjdStart + 17045*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17050*second) + ' since array.time is ' + str(array.time())

# Scan 234 = No0235
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17085*second, mjdStart+17130*second, 'No0235', obsCode, stnCode )
if array.time() < mjdStart + (17130-10)*second:
  subarray.execute(mjdStart + 17125*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17130*second) + ' since array.time is ' + str(array.time())

# Scan 235 = No0236
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17164*second, mjdStart+17180*second, 'No0236', obsCode, stnCode )
if array.time() < mjdStart + (17180-10)*second:
  subarray.execute(mjdStart + 17175*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17180*second) + ' since array.time is ' + str(array.time())

# Scan 236 = No0237
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17216*second, mjdStart+17260*second, 'No0237', obsCode, stnCode )
if array.time() < mjdStart + (17260-10)*second:
  subarray.execute(mjdStart + 17255*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17260*second) + ' since array.time is ' + str(array.time())

# Scan 237 = No0238
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17294*second, mjdStart+17310*second, 'No0238', obsCode, stnCode )
if array.time() < mjdStart + (17310-10)*second:
  subarray.execute(mjdStart + 17305*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17310*second) + ' since array.time is ' + str(array.time())

# Scan 238 = No0239
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17346*second, mjdStart+17390*second, 'No0239', obsCode, stnCode )
if array.time() < mjdStart + (17390-10)*second:
  subarray.execute(mjdStart + 17385*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17390*second) + ' since array.time is ' + str(array.time())

# Scan 239 = No0240
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17425*second, mjdStart+17440*second, 'No0240', obsCode, stnCode )
if array.time() < mjdStart + (17440-10)*second:
  subarray.execute(mjdStart + 17435*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17440*second) + ' since array.time is ' + str(array.time())

# Scan 240 = No0241
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17477*second, mjdStart+17520*second, 'No0241', obsCode, stnCode )
if array.time() < mjdStart + (17520-10)*second:
  subarray.execute(mjdStart + 17515*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17520*second) + ' since array.time is ' + str(array.time())

# Scan 241 = No0242
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17555*second, mjdStart+17570*second, 'No0242', obsCode, stnCode )
if array.time() < mjdStart + (17570-10)*second:
  subarray.execute(mjdStart + 17565*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17570*second) + ' since array.time is ' + str(array.time())

# Scan 242 = No0243
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17607*second, mjdStart+17650*second, 'No0243', obsCode, stnCode )
if array.time() < mjdStart + (17650-10)*second:
  subarray.execute(mjdStart + 17645*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17650*second) + ' since array.time is ' + str(array.time())

# Scan 243 = No0244
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17686*second, mjdStart+17700*second, 'No0244', obsCode, stnCode )
if array.time() < mjdStart + (17700-10)*second:
  subarray.execute(mjdStart + 17695*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17700*second) + ' since array.time is ' + str(array.time())

# Scan 244 = No0245
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17778*second, mjdStart+17948*second, 'No0245', obsCode, stnCode )
if array.time() < mjdStart + (17948-10)*second:
  subarray.execute(mjdStart + 17943*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17948*second) + ' since array.time is ' + str(array.time())

# Scan 245 = No0246
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18001*second, mjdStart+18031*second, 'No0246', obsCode, stnCode )
if array.time() < mjdStart + (18031-10)*second:
  subarray.execute(mjdStart + 18026*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18031*second) + ' since array.time is ' + str(array.time())

# Scan 246 = No0247
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18088*second, mjdStart+18121*second, 'No0247', obsCode, stnCode )
if array.time() < mjdStart + (18121-10)*second:
  subarray.execute(mjdStart + 18116*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18121*second) + ' since array.time is ' + str(array.time())

# Scan 247 = No0248
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18161*second, mjdStart+18201*second, 'No0248', obsCode, stnCode )
if array.time() < mjdStart + (18201-10)*second:
  subarray.execute(mjdStart + 18196*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18201*second) + ' since array.time is ' + str(array.time())

# Scan 248 = No0249
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18239*second, mjdStart+18251*second, 'No0249', obsCode, stnCode )
if array.time() < mjdStart + (18251-10)*second:
  subarray.execute(mjdStart + 18246*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18251*second) + ' since array.time is ' + str(array.time())

# Scan 249 = No0250
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18291*second, mjdStart+18326*second, 'No0250', obsCode, stnCode )
if array.time() < mjdStart + (18326-10)*second:
  subarray.execute(mjdStart + 18321*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18326*second) + ' since array.time is ' + str(array.time())

# Scan 250 = No0251
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18364*second, mjdStart+18381*second, 'No0251', obsCode, stnCode )
if array.time() < mjdStart + (18381-10)*second:
  subarray.execute(mjdStart + 18376*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18381*second) + ' since array.time is ' + str(array.time())

# Scan 251 = No0252
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18421*second, mjdStart+18456*second, 'No0252', obsCode, stnCode )
if array.time() < mjdStart + (18456-10)*second:
  subarray.execute(mjdStart + 18451*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18456*second) + ' since array.time is ' + str(array.time())

# Scan 252 = No0253
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18495*second, mjdStart+18511*second, 'No0253', obsCode, stnCode )
if array.time() < mjdStart + (18511-10)*second:
  subarray.execute(mjdStart + 18506*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18511*second) + ' since array.time is ' + str(array.time())

# Scan 253 = No0254
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18552*second, mjdStart+18586*second, 'No0254', obsCode, stnCode )
if array.time() < mjdStart + (18586-10)*second:
  subarray.execute(mjdStart + 18581*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18586*second) + ' since array.time is ' + str(array.time())

# Scan 254 = No0255
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18625*second, mjdStart+18641*second, 'No0255', obsCode, stnCode )
if array.time() < mjdStart + (18641-10)*second:
  subarray.execute(mjdStart + 18636*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18641*second) + ' since array.time is ' + str(array.time())

# Scan 255 = No0256
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18682*second, mjdStart+18716*second, 'No0256', obsCode, stnCode )
if array.time() < mjdStart + (18716-10)*second:
  subarray.execute(mjdStart + 18711*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18716*second) + ' since array.time is ' + str(array.time())

# Scan 256 = No0257
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18755*second, mjdStart+18771*second, 'No0257', obsCode, stnCode )
if array.time() < mjdStart + (18771-10)*second:
  subarray.execute(mjdStart + 18766*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18771*second) + ' since array.time is ' + str(array.time())

# Scan 257 = No0258
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18812*second, mjdStart+18846*second, 'No0258', obsCode, stnCode )
if array.time() < mjdStart + (18846-10)*second:
  subarray.execute(mjdStart + 18841*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18846*second) + ' since array.time is ' + str(array.time())

# Scan 258 = No0259
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18885*second, mjdStart+18901*second, 'No0259', obsCode, stnCode )
if array.time() < mjdStart + (18901-10)*second:
  subarray.execute(mjdStart + 18896*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18901*second) + ' since array.time is ' + str(array.time())

# Scan 259 = No0260
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18943*second, mjdStart+18976*second, 'No0260', obsCode, stnCode )
if array.time() < mjdStart + (18976-10)*second:
  subarray.execute(mjdStart + 18971*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18976*second) + ' since array.time is ' + str(array.time())

# Scan 260 = No0261
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19016*second, mjdStart+19031*second, 'No0261', obsCode, stnCode )
if array.time() < mjdStart + (19031-10)*second:
  subarray.execute(mjdStart + 19026*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19031*second) + ' since array.time is ' + str(array.time())

# Scan 261 = No0262
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19073*second, mjdStart+19106*second, 'No0262', obsCode, stnCode )
if array.time() < mjdStart + (19106-10)*second:
  subarray.execute(mjdStart + 19101*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19106*second) + ' since array.time is ' + str(array.time())

# Scan 262 = No0263
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19146*second, mjdStart+19161*second, 'No0263', obsCode, stnCode )
if array.time() < mjdStart + (19161-10)*second:
  subarray.execute(mjdStart + 19156*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19161*second) + ' since array.time is ' + str(array.time())

# Scan 263 = No0264
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19203*second, mjdStart+19236*second, 'No0264', obsCode, stnCode )
if array.time() < mjdStart + (19236-10)*second:
  subarray.execute(mjdStart + 19231*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19236*second) + ' since array.time is ' + str(array.time())

# Scan 264 = No0265
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19276*second, mjdStart+19291*second, 'No0265', obsCode, stnCode )
if array.time() < mjdStart + (19291-10)*second:
  subarray.execute(mjdStart + 19286*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19291*second) + ' since array.time is ' + str(array.time())

# Scan 265 = No0266
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19333*second, mjdStart+19366*second, 'No0266', obsCode, stnCode )
if array.time() < mjdStart + (19366-10)*second:
  subarray.execute(mjdStart + 19361*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19366*second) + ' since array.time is ' + str(array.time())

# Scan 266 = No0267
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19406*second, mjdStart+19421*second, 'No0267', obsCode, stnCode )
if array.time() < mjdStart + (19421-10)*second:
  subarray.execute(mjdStart + 19416*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19421*second) + ' since array.time is ' + str(array.time())

# Scan 267 = No0268
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19463*second, mjdStart+19496*second, 'No0268', obsCode, stnCode )
if array.time() < mjdStart + (19496-10)*second:
  subarray.execute(mjdStart + 19491*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19496*second) + ' since array.time is ' + str(array.time())

# Scan 268 = No0269
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19536*second, mjdStart+19551*second, 'No0269', obsCode, stnCode )
if array.time() < mjdStart + (19551-10)*second:
  subarray.execute(mjdStart + 19546*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19551*second) + ' since array.time is ' + str(array.time())

# Scan 269 = No0270
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19593*second, mjdStart+19626*second, 'No0270', obsCode, stnCode )
if array.time() < mjdStart + (19626-10)*second:
  subarray.execute(mjdStart + 19621*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19626*second) + ' since array.time is ' + str(array.time())

# Scan 270 = No0271
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19666*second, mjdStart+19681*second, 'No0271', obsCode, stnCode )
if array.time() < mjdStart + (19681-10)*second:
  subarray.execute(mjdStart + 19676*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19681*second) + ' since array.time is ' + str(array.time())

# Scan 271 = No0272
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19723*second, mjdStart+19756*second, 'No0272', obsCode, stnCode )
if array.time() < mjdStart + (19756-10)*second:
  subarray.execute(mjdStart + 19751*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19756*second) + ' since array.time is ' + str(array.time())

# Scan 272 = No0273
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19796*second, mjdStart+19811*second, 'No0273', obsCode, stnCode )
if array.time() < mjdStart + (19811-10)*second:
  subarray.execute(mjdStart + 19806*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19811*second) + ' since array.time is ' + str(array.time())

# Scan 273 = No0274
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19853*second, mjdStart+19886*second, 'No0274', obsCode, stnCode )
if array.time() < mjdStart + (19886-10)*second:
  subarray.execute(mjdStart + 19881*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19886*second) + ' since array.time is ' + str(array.time())

# Scan 274 = No0275
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19926*second, mjdStart+19941*second, 'No0275', obsCode, stnCode )
if array.time() < mjdStart + (19941-10)*second:
  subarray.execute(mjdStart + 19936*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19941*second) + ' since array.time is ' + str(array.time())

# Scan 275 = No0276
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19983*second, mjdStart+20016*second, 'No0276', obsCode, stnCode )
if array.time() < mjdStart + (20016-10)*second:
  subarray.execute(mjdStart + 20011*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20016*second) + ' since array.time is ' + str(array.time())

# Scan 276 = No0277
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20057*second, mjdStart+20071*second, 'No0277', obsCode, stnCode )
if array.time() < mjdStart + (20071-10)*second:
  subarray.execute(mjdStart + 20066*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20071*second) + ' since array.time is ' + str(array.time())

# Scan 277 = No0278
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20113*second, mjdStart+20146*second, 'No0278', obsCode, stnCode )
if array.time() < mjdStart + (20146-10)*second:
  subarray.execute(mjdStart + 20141*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20146*second) + ' since array.time is ' + str(array.time())

# Scan 278 = No0279
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20186*second, mjdStart+20201*second, 'No0279', obsCode, stnCode )
if array.time() < mjdStart + (20201-10)*second:
  subarray.execute(mjdStart + 20196*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20201*second) + ' since array.time is ' + str(array.time())

# Scan 279 = No0280
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20243*second, mjdStart+20276*second, 'No0280', obsCode, stnCode )
if array.time() < mjdStart + (20276-10)*second:
  subarray.execute(mjdStart + 20271*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20276*second) + ' since array.time is ' + str(array.time())

# Scan 280 = No0281
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20316*second, mjdStart+20331*second, 'No0281', obsCode, stnCode )
if array.time() < mjdStart + (20331-10)*second:
  subarray.execute(mjdStart + 20326*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20331*second) + ' since array.time is ' + str(array.time())

# Scan 281 = No0282
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20373*second, mjdStart+20406*second, 'No0282', obsCode, stnCode )
if array.time() < mjdStart + (20406-10)*second:
  subarray.execute(mjdStart + 20401*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20406*second) + ' since array.time is ' + str(array.time())

# Scan 282 = No0283
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20446*second, mjdStart+20461*second, 'No0283', obsCode, stnCode )
if array.time() < mjdStart + (20461-10)*second:
  subarray.execute(mjdStart + 20456*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20461*second) + ' since array.time is ' + str(array.time())

# Scan 283 = No0284
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20513*second, mjdStart+20673*second, 'No0284', obsCode, stnCode )
if array.time() < mjdStart + (20673-10)*second:
  subarray.execute(mjdStart + 20668*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20673*second) + ' since array.time is ' + str(array.time())

# Scan 284 = No0285
# pointing scan for the GBT
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20693*second, mjdStart+21633*second, 'No0285', obsCode, stnCode )
if isAstrid:
  source8.setPeak(True)
if array.time() < mjdStart + (21633-10)*second:
  subarray.execute(mjdStart + 21628*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21633*second) + ' since array.time is ' + str(array.time())
if isAstrid:
  source8.setPeak(False)

# Scan 285 = No0286
# Antenna GB not in scan No0286

# Scan 286 = No0287
# Antenna GB not in scan No0287

# Scan 287 = No0288
# Antenna GB not in scan No0288

# Scan 288 = No0289
# Antenna GB not in scan No0289

# Scan 289 = No0290
# Antenna GB not in scan No0290

# Scan 290 = No0291
# Antenna GB not in scan No0291

# Scan 291 = No0292
# Antenna GB not in scan No0292

# Scan 292 = No0293
# Antenna GB not in scan No0293

# Scan 293 = No0294
# Antenna GB not in scan No0294

# Scan 294 = No0295
# Antenna GB not in scan No0295

# Scan 295 = No0296
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21669*second, mjdStart+21699*second, 'No0296', obsCode, stnCode )
if array.time() < mjdStart + (21699-10)*second:
  subarray.execute(mjdStart + 21694*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21699*second) + ' since array.time is ' + str(array.time())

# Scan 296 = No0297
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21746*second, mjdStart+21789*second, 'No0297', obsCode, stnCode )
if array.time() < mjdStart + (21789-10)*second:
  subarray.execute(mjdStart + 21784*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21789*second) + ' since array.time is ' + str(array.time())

# Scan 297 = No0298
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21829*second, mjdStart+21844*second, 'No0298', obsCode, stnCode )
if array.time() < mjdStart + (21844-10)*second:
  subarray.execute(mjdStart + 21839*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21844*second) + ' since array.time is ' + str(array.time())

# Scan 298 = No0299
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21883*second, mjdStart+21899*second, 'No0299', obsCode, stnCode )
if array.time() < mjdStart + (21899-10)*second:
  subarray.execute(mjdStart + 21894*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21899*second) + ' since array.time is ' + str(array.time())

# Scan 299 = No0300
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21939*second, mjdStart+21974*second, 'No0300', obsCode, stnCode )
if array.time() < mjdStart + (21974-10)*second:
  subarray.execute(mjdStart + 21969*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21974*second) + ' since array.time is ' + str(array.time())

# Scan 300 = No0301
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22013*second, mjdStart+22029*second, 'No0301', obsCode, stnCode )
if array.time() < mjdStart + (22029-10)*second:
  subarray.execute(mjdStart + 22024*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22029*second) + ' since array.time is ' + str(array.time())

# Scan 301 = No0302
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22069*second, mjdStart+22104*second, 'No0302', obsCode, stnCode )
if array.time() < mjdStart + (22104-10)*second:
  subarray.execute(mjdStart + 22099*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22104*second) + ' since array.time is ' + str(array.time())

# Scan 302 = No0303
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22143*second, mjdStart+22159*second, 'No0303', obsCode, stnCode )
if array.time() < mjdStart + (22159-10)*second:
  subarray.execute(mjdStart + 22154*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22159*second) + ' since array.time is ' + str(array.time())

# Scan 303 = No0304
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22199*second, mjdStart+22234*second, 'No0304', obsCode, stnCode )
if array.time() < mjdStart + (22234-10)*second:
  subarray.execute(mjdStart + 22229*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22234*second) + ' since array.time is ' + str(array.time())

# Scan 304 = No0305
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22273*second, mjdStart+22289*second, 'No0305', obsCode, stnCode )
if array.time() < mjdStart + (22289-10)*second:
  subarray.execute(mjdStart + 22284*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22289*second) + ' since array.time is ' + str(array.time())

# Scan 305 = No0306
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22329*second, mjdStart+22364*second, 'No0306', obsCode, stnCode )
if array.time() < mjdStart + (22364-10)*second:
  subarray.execute(mjdStart + 22359*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22364*second) + ' since array.time is ' + str(array.time())

# Scan 306 = No0307
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22402*second, mjdStart+22419*second, 'No0307', obsCode, stnCode )
if array.time() < mjdStart + (22419-10)*second:
  subarray.execute(mjdStart + 22414*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22419*second) + ' since array.time is ' + str(array.time())

# Scan 307 = No0308
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22458*second, mjdStart+22494*second, 'No0308', obsCode, stnCode )
if array.time() < mjdStart + (22494-10)*second:
  subarray.execute(mjdStart + 22489*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22494*second) + ' since array.time is ' + str(array.time())

# Scan 308 = No0309
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22532*second, mjdStart+22549*second, 'No0309', obsCode, stnCode )
if array.time() < mjdStart + (22549-10)*second:
  subarray.execute(mjdStart + 22544*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22549*second) + ' since array.time is ' + str(array.time())

# Scan 309 = No0310
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22588*second, mjdStart+22624*second, 'No0310', obsCode, stnCode )
if array.time() < mjdStart + (22624-10)*second:
  subarray.execute(mjdStart + 22619*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22624*second) + ' since array.time is ' + str(array.time())

# Scan 310 = No0311
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22662*second, mjdStart+22679*second, 'No0311', obsCode, stnCode )
if array.time() < mjdStart + (22679-10)*second:
  subarray.execute(mjdStart + 22674*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22679*second) + ' since array.time is ' + str(array.time())

# Scan 311 = No0312
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22718*second, mjdStart+22754*second, 'No0312', obsCode, stnCode )
if array.time() < mjdStart + (22754-10)*second:
  subarray.execute(mjdStart + 22749*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22754*second) + ' since array.time is ' + str(array.time())

# Scan 312 = No0313
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22792*second, mjdStart+22809*second, 'No0313', obsCode, stnCode )
if array.time() < mjdStart + (22809-10)*second:
  subarray.execute(mjdStart + 22804*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22809*second) + ' since array.time is ' + str(array.time())

# Scan 313 = No0314
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22848*second, mjdStart+22884*second, 'No0314', obsCode, stnCode )
if array.time() < mjdStart + (22884-10)*second:
  subarray.execute(mjdStart + 22879*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22884*second) + ' since array.time is ' + str(array.time())

# Scan 314 = No0315
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22922*second, mjdStart+22939*second, 'No0315', obsCode, stnCode )
if array.time() < mjdStart + (22939-10)*second:
  subarray.execute(mjdStart + 22934*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22939*second) + ' since array.time is ' + str(array.time())

# Scan 315 = No0316
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22978*second, mjdStart+23014*second, 'No0316', obsCode, stnCode )
if array.time() < mjdStart + (23014-10)*second:
  subarray.execute(mjdStart + 23009*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23014*second) + ' since array.time is ' + str(array.time())

# Scan 316 = No0317
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23052*second, mjdStart+23069*second, 'No0317', obsCode, stnCode )
if array.time() < mjdStart + (23069-10)*second:
  subarray.execute(mjdStart + 23064*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23069*second) + ' since array.time is ' + str(array.time())

# Scan 317 = No0318
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23107*second, mjdStart+23144*second, 'No0318', obsCode, stnCode )
if array.time() < mjdStart + (23144-10)*second:
  subarray.execute(mjdStart + 23139*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23144*second) + ' since array.time is ' + str(array.time())

# Scan 318 = No0319
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23182*second, mjdStart+23199*second, 'No0319', obsCode, stnCode )
if array.time() < mjdStart + (23199-10)*second:
  subarray.execute(mjdStart + 23194*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23199*second) + ' since array.time is ' + str(array.time())

# Scan 319 = No0320
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23237*second, mjdStart+23274*second, 'No0320', obsCode, stnCode )
if array.time() < mjdStart + (23274-10)*second:
  subarray.execute(mjdStart + 23269*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23274*second) + ' since array.time is ' + str(array.time())

# Scan 320 = No0321
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23311*second, mjdStart+23329*second, 'No0321', obsCode, stnCode )
if array.time() < mjdStart + (23329-10)*second:
  subarray.execute(mjdStart + 23324*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23329*second) + ' since array.time is ' + str(array.time())

# Scan 321 = No0322
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23367*second, mjdStart+23404*second, 'No0322', obsCode, stnCode )
if array.time() < mjdStart + (23404-10)*second:
  subarray.execute(mjdStart + 23399*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23404*second) + ' since array.time is ' + str(array.time())

# Scan 322 = No0323
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23441*second, mjdStart+23459*second, 'No0323', obsCode, stnCode )
if array.time() < mjdStart + (23459-10)*second:
  subarray.execute(mjdStart + 23454*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23459*second) + ' since array.time is ' + str(array.time())

# Scan 323 = No0324
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23497*second, mjdStart+23534*second, 'No0324', obsCode, stnCode )
if array.time() < mjdStart + (23534-10)*second:
  subarray.execute(mjdStart + 23529*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23534*second) + ' since array.time is ' + str(array.time())

# Scan 324 = No0325
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23571*second, mjdStart+23589*second, 'No0325', obsCode, stnCode )
if array.time() < mjdStart + (23589-10)*second:
  subarray.execute(mjdStart + 23584*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23589*second) + ' since array.time is ' + str(array.time())

# Scan 325 = No0326
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23627*second, mjdStart+23664*second, 'No0326', obsCode, stnCode )
if array.time() < mjdStart + (23664-10)*second:
  subarray.execute(mjdStart + 23659*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23664*second) + ' since array.time is ' + str(array.time())

# Scan 326 = No0327
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23701*second, mjdStart+23719*second, 'No0327', obsCode, stnCode )
if array.time() < mjdStart + (23719-10)*second:
  subarray.execute(mjdStart + 23714*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23719*second) + ' since array.time is ' + str(array.time())

# Scan 327 = No0328
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23757*second, mjdStart+23794*second, 'No0328', obsCode, stnCode )
if array.time() < mjdStart + (23794-10)*second:
  subarray.execute(mjdStart + 23789*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23794*second) + ' since array.time is ' + str(array.time())

# Scan 328 = No0329
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23831*second, mjdStart+23849*second, 'No0329', obsCode, stnCode )
if array.time() < mjdStart + (23849-10)*second:
  subarray.execute(mjdStart + 23844*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23849*second) + ' since array.time is ' + str(array.time())

# Scan 329 = No0330
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23886*second, mjdStart+23924*second, 'No0330', obsCode, stnCode )
if array.time() < mjdStart + (23924-10)*second:
  subarray.execute(mjdStart + 23919*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23924*second) + ' since array.time is ' + str(array.time())

# Scan 330 = No0331
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23961*second, mjdStart+23979*second, 'No0331', obsCode, stnCode )
if array.time() < mjdStart + (23979-10)*second:
  subarray.execute(mjdStart + 23974*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23979*second) + ' since array.time is ' + str(array.time())

# Scan 331 = No0332
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24016*second, mjdStart+24054*second, 'No0332', obsCode, stnCode )
if array.time() < mjdStart + (24054-10)*second:
  subarray.execute(mjdStart + 24049*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24054*second) + ' since array.time is ' + str(array.time())

# Scan 332 = No0333
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24091*second, mjdStart+24109*second, 'No0333', obsCode, stnCode )
if array.time() < mjdStart + (24109-10)*second:
  subarray.execute(mjdStart + 24104*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24109*second) + ' since array.time is ' + str(array.time())

# Scan 333 = No0334
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24146*second, mjdStart+24184*second, 'No0334', obsCode, stnCode )
if array.time() < mjdStart + (24184-10)*second:
  subarray.execute(mjdStart + 24179*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24184*second) + ' since array.time is ' + str(array.time())

# Scan 334 = No0335
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24220*second, mjdStart+24239*second, 'No0335', obsCode, stnCode )
if array.time() < mjdStart + (24239-10)*second:
  subarray.execute(mjdStart + 24234*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24239*second) + ' since array.time is ' + str(array.time())

# Scan 335 = No0336
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24276*second, mjdStart+24314*second, 'No0336', obsCode, stnCode )
if array.time() < mjdStart + (24314-10)*second:
  subarray.execute(mjdStart + 24309*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24314*second) + ' since array.time is ' + str(array.time())

# Scan 336 = No0337
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24350*second, mjdStart+24369*second, 'No0337', obsCode, stnCode )
if array.time() < mjdStart + (24369-10)*second:
  subarray.execute(mjdStart + 24364*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24369*second) + ' since array.time is ' + str(array.time())

# Scan 337 = No0338
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24406*second, mjdStart+24444*second, 'No0338', obsCode, stnCode )
if array.time() < mjdStart + (24444-10)*second:
  subarray.execute(mjdStart + 24439*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24444*second) + ' since array.time is ' + str(array.time())

# Scan 338 = No0339
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24480*second, mjdStart+24499*second, 'No0339', obsCode, stnCode )
if array.time() < mjdStart + (24499-10)*second:
  subarray.execute(mjdStart + 24494*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24499*second) + ' since array.time is ' + str(array.time())

# Scan 339 = No0340
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24536*second, mjdStart+24574*second, 'No0340', obsCode, stnCode )
if array.time() < mjdStart + (24574-10)*second:
  subarray.execute(mjdStart + 24569*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24574*second) + ' since array.time is ' + str(array.time())

# Scan 340 = No0341
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24610*second, mjdStart+24629*second, 'No0341', obsCode, stnCode )
if array.time() < mjdStart + (24629-10)*second:
  subarray.execute(mjdStart + 24624*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24629*second) + ' since array.time is ' + str(array.time())

# Scan 341 = No0342
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24665*second, mjdStart+24704*second, 'No0342', obsCode, stnCode )
if array.time() < mjdStart + (24704-10)*second:
  subarray.execute(mjdStart + 24699*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24704*second) + ' since array.time is ' + str(array.time())

# Scan 342 = No0343
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24740*second, mjdStart+24759*second, 'No0343', obsCode, stnCode )
if array.time() < mjdStart + (24759-10)*second:
  subarray.execute(mjdStart + 24754*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24759*second) + ' since array.time is ' + str(array.time())

# Scan 343 = No0344
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24795*second, mjdStart+24834*second, 'No0344', obsCode, stnCode )
if array.time() < mjdStart + (24834-10)*second:
  subarray.execute(mjdStart + 24829*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24834*second) + ' since array.time is ' + str(array.time())

# Scan 344 = No0345
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24870*second, mjdStart+24889*second, 'No0345', obsCode, stnCode )
if array.time() < mjdStart + (24889-10)*second:
  subarray.execute(mjdStart + 24884*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24889*second) + ' since array.time is ' + str(array.time())

# Scan 345 = No0346
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24925*second, mjdStart+24964*second, 'No0346', obsCode, stnCode )
if array.time() < mjdStart + (24964-10)*second:
  subarray.execute(mjdStart + 24959*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24964*second) + ' since array.time is ' + str(array.time())

# Scan 346 = No0347
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25000*second, mjdStart+25019*second, 'No0347', obsCode, stnCode )
if array.time() < mjdStart + (25019-10)*second:
  subarray.execute(mjdStart + 25014*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25019*second) + ' since array.time is ' + str(array.time())

# Scan 347 = No0348
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25056*second, mjdStart+25086*second, 'No0348', obsCode, stnCode )
if array.time() < mjdStart + (25086-10)*second:
  subarray.execute(mjdStart + 25081*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25086*second) + ' since array.time is ' + str(array.time())

# Scan 348 = No0349
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25124*second, mjdStart+25196*second, 'No0349', obsCode, stnCode )
if array.time() < mjdStart + (25196-10)*second:
  subarray.execute(mjdStart + 25191*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25196*second) + ' since array.time is ' + str(array.time())

# Scan 349 = No0350
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25232*second, mjdStart+25266*second, 'No0350', obsCode, stnCode )
if array.time() < mjdStart + (25266-10)*second:
  subarray.execute(mjdStart + 25261*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25266*second) + ' since array.time is ' + str(array.time())

# Scan 350 = No0351
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25301*second, mjdStart+25316*second, 'No0351', obsCode, stnCode )
if array.time() < mjdStart + (25316-10)*second:
  subarray.execute(mjdStart + 25311*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25316*second) + ' since array.time is ' + str(array.time())

# Scan 351 = No0352
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25352*second, mjdStart+25391*second, 'No0352', obsCode, stnCode )
if array.time() < mjdStart + (25391-10)*second:
  subarray.execute(mjdStart + 25386*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25391*second) + ' since array.time is ' + str(array.time())

# Scan 352 = No0353
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25426*second, mjdStart+25446*second, 'No0353', obsCode, stnCode )
if array.time() < mjdStart + (25446-10)*second:
  subarray.execute(mjdStart + 25441*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25446*second) + ' since array.time is ' + str(array.time())

# Scan 353 = No0354
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25482*second, mjdStart+25521*second, 'No0354', obsCode, stnCode )
if array.time() < mjdStart + (25521-10)*second:
  subarray.execute(mjdStart + 25516*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25521*second) + ' since array.time is ' + str(array.time())

# Scan 354 = No0355
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25556*second, mjdStart+25576*second, 'No0355', obsCode, stnCode )
if array.time() < mjdStart + (25576-10)*second:
  subarray.execute(mjdStart + 25571*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25576*second) + ' since array.time is ' + str(array.time())

# Scan 355 = No0356
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25612*second, mjdStart+25651*second, 'No0356', obsCode, stnCode )
if array.time() < mjdStart + (25651-10)*second:
  subarray.execute(mjdStart + 25646*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25651*second) + ' since array.time is ' + str(array.time())

# Scan 356 = No0357
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25686*second, mjdStart+25706*second, 'No0357', obsCode, stnCode )
if array.time() < mjdStart + (25706-10)*second:
  subarray.execute(mjdStart + 25701*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25706*second) + ' since array.time is ' + str(array.time())

# Scan 357 = No0358
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25741*second, mjdStart+25781*second, 'No0358', obsCode, stnCode )
if array.time() < mjdStart + (25781-10)*second:
  subarray.execute(mjdStart + 25776*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25781*second) + ' since array.time is ' + str(array.time())

# Scan 358 = No0359
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25816*second, mjdStart+25836*second, 'No0359', obsCode, stnCode )
if array.time() < mjdStart + (25836-10)*second:
  subarray.execute(mjdStart + 25831*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25836*second) + ' since array.time is ' + str(array.time())

# Scan 359 = No0360
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25871*second, mjdStart+25911*second, 'No0360', obsCode, stnCode )
if array.time() < mjdStart + (25911-10)*second:
  subarray.execute(mjdStart + 25906*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25911*second) + ' since array.time is ' + str(array.time())

# Scan 360 = No0361
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25946*second, mjdStart+25966*second, 'No0361', obsCode, stnCode )
if array.time() < mjdStart + (25966-10)*second:
  subarray.execute(mjdStart + 25961*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25966*second) + ' since array.time is ' + str(array.time())

# Scan 361 = No0362
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26001*second, mjdStart+26041*second, 'No0362', obsCode, stnCode )
if array.time() < mjdStart + (26041-10)*second:
  subarray.execute(mjdStart + 26036*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26041*second) + ' since array.time is ' + str(array.time())

# Scan 362 = No0363
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26076*second, mjdStart+26096*second, 'No0363', obsCode, stnCode )
if array.time() < mjdStart + (26096-10)*second:
  subarray.execute(mjdStart + 26091*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26096*second) + ' since array.time is ' + str(array.time())

# Scan 363 = No0364
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26131*second, mjdStart+26171*second, 'No0364', obsCode, stnCode )
if array.time() < mjdStart + (26171-10)*second:
  subarray.execute(mjdStart + 26166*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26171*second) + ' since array.time is ' + str(array.time())

# Scan 364 = No0365
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26206*second, mjdStart+26226*second, 'No0365', obsCode, stnCode )
if array.time() < mjdStart + (26226-10)*second:
  subarray.execute(mjdStart + 26221*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26226*second) + ' since array.time is ' + str(array.time())

# Scan 365 = No0366
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26261*second, mjdStart+26301*second, 'No0366', obsCode, stnCode )
if array.time() < mjdStart + (26301-10)*second:
  subarray.execute(mjdStart + 26296*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26301*second) + ' since array.time is ' + str(array.time())

# Scan 366 = No0367
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26336*second, mjdStart+26356*second, 'No0367', obsCode, stnCode )
if array.time() < mjdStart + (26356-10)*second:
  subarray.execute(mjdStart + 26351*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26356*second) + ' since array.time is ' + str(array.time())

# Scan 367 = No0368
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26391*second, mjdStart+26431*second, 'No0368', obsCode, stnCode )
if array.time() < mjdStart + (26431-10)*second:
  subarray.execute(mjdStart + 26426*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26431*second) + ' since array.time is ' + str(array.time())

# Scan 368 = No0369
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26465*second, mjdStart+26486*second, 'No0369', obsCode, stnCode )
if array.time() < mjdStart + (26486-10)*second:
  subarray.execute(mjdStart + 26481*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26486*second) + ' since array.time is ' + str(array.time())

# Scan 369 = No0370
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26521*second, mjdStart+26561*second, 'No0370', obsCode, stnCode )
if array.time() < mjdStart + (26561-10)*second:
  subarray.execute(mjdStart + 26556*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26561*second) + ' since array.time is ' + str(array.time())

# Scan 370 = No0371
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26595*second, mjdStart+26616*second, 'No0371', obsCode, stnCode )
if array.time() < mjdStart + (26616-10)*second:
  subarray.execute(mjdStart + 26611*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26616*second) + ' since array.time is ' + str(array.time())

# Scan 371 = No0372
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26651*second, mjdStart+26691*second, 'No0372', obsCode, stnCode )
if array.time() < mjdStart + (26691-10)*second:
  subarray.execute(mjdStart + 26686*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26691*second) + ' since array.time is ' + str(array.time())

# Scan 372 = No0373
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26725*second, mjdStart+26746*second, 'No0373', obsCode, stnCode )
if array.time() < mjdStart + (26746-10)*second:
  subarray.execute(mjdStart + 26741*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26746*second) + ' since array.time is ' + str(array.time())

# Scan 373 = No0374
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26781*second, mjdStart+26821*second, 'No0374', obsCode, stnCode )
if array.time() < mjdStart + (26821-10)*second:
  subarray.execute(mjdStart + 26816*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26821*second) + ' since array.time is ' + str(array.time())

# Scan 374 = No0375
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26855*second, mjdStart+26876*second, 'No0375', obsCode, stnCode )
if array.time() < mjdStart + (26876-10)*second:
  subarray.execute(mjdStart + 26871*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26876*second) + ' since array.time is ' + str(array.time())

# Scan 375 = No0376
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26910*second, mjdStart+26951*second, 'No0376', obsCode, stnCode )
if array.time() < mjdStart + (26951-10)*second:
  subarray.execute(mjdStart + 26946*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26951*second) + ' since array.time is ' + str(array.time())

# Scan 376 = No0377
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26985*second, mjdStart+27006*second, 'No0377', obsCode, stnCode )
if array.time() < mjdStart + (27006-10)*second:
  subarray.execute(mjdStart + 27001*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27006*second) + ' since array.time is ' + str(array.time())

# Scan 377 = No0378
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27040*second, mjdStart+27081*second, 'No0378', obsCode, stnCode )
if array.time() < mjdStart + (27081-10)*second:
  subarray.execute(mjdStart + 27076*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27081*second) + ' since array.time is ' + str(array.time())

# Scan 378 = No0379
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27115*second, mjdStart+27136*second, 'No0379', obsCode, stnCode )
if array.time() < mjdStart + (27136-10)*second:
  subarray.execute(mjdStart + 27131*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27136*second) + ' since array.time is ' + str(array.time())

# Scan 379 = No0380
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27170*second, mjdStart+27211*second, 'No0380', obsCode, stnCode )
if array.time() < mjdStart + (27211-10)*second:
  subarray.execute(mjdStart + 27206*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27211*second) + ' since array.time is ' + str(array.time())

# Scan 380 = No0381
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27245*second, mjdStart+27266*second, 'No0381', obsCode, stnCode )
if array.time() < mjdStart + (27266-10)*second:
  subarray.execute(mjdStart + 27261*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27266*second) + ' since array.time is ' + str(array.time())

# Scan 381 = No0382
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27300*second, mjdStart+27341*second, 'No0382', obsCode, stnCode )
if array.time() < mjdStart + (27341-10)*second:
  subarray.execute(mjdStart + 27336*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27341*second) + ' since array.time is ' + str(array.time())

# Scan 382 = No0383
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27375*second, mjdStart+27396*second, 'No0383', obsCode, stnCode )
if array.time() < mjdStart + (27396-10)*second:
  subarray.execute(mjdStart + 27391*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27396*second) + ' since array.time is ' + str(array.time())

# Scan 383 = No0384
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27430*second, mjdStart+27471*second, 'No0384', obsCode, stnCode )
if array.time() < mjdStart + (27471-10)*second:
  subarray.execute(mjdStart + 27466*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27471*second) + ' since array.time is ' + str(array.time())

# Scan 384 = No0385
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27505*second, mjdStart+27526*second, 'No0385', obsCode, stnCode )
if array.time() < mjdStart + (27526-10)*second:
  subarray.execute(mjdStart + 27521*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27526*second) + ' since array.time is ' + str(array.time())

# Scan 385 = No0386
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27560*second, mjdStart+27601*second, 'No0386', obsCode, stnCode )
if array.time() < mjdStart + (27601-10)*second:
  subarray.execute(mjdStart + 27596*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27601*second) + ' since array.time is ' + str(array.time())

# Scan 386 = No0387
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27635*second, mjdStart+27656*second, 'No0387', obsCode, stnCode )
if array.time() < mjdStart + (27656-10)*second:
  subarray.execute(mjdStart + 27651*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27656*second) + ' since array.time is ' + str(array.time())

# Scan 387 = No0388
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27690*second, mjdStart+27731*second, 'No0388', obsCode, stnCode )
if array.time() < mjdStart + (27731-10)*second:
  subarray.execute(mjdStart + 27726*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27731*second) + ' since array.time is ' + str(array.time())

# Scan 388 = No0389
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27765*second, mjdStart+27786*second, 'No0389', obsCode, stnCode )
if array.time() < mjdStart + (27786-10)*second:
  subarray.execute(mjdStart + 27781*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27786*second) + ' since array.time is ' + str(array.time())

# Scan 389 = No0390
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27820*second, mjdStart+27861*second, 'No0390', obsCode, stnCode )
if array.time() < mjdStart + (27861-10)*second:
  subarray.execute(mjdStart + 27856*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27861*second) + ' since array.time is ' + str(array.time())

# Scan 390 = No0391
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27894*second, mjdStart+27916*second, 'No0391', obsCode, stnCode )
if array.time() < mjdStart + (27916-10)*second:
  subarray.execute(mjdStart + 27911*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27916*second) + ' since array.time is ' + str(array.time())

# Scan 391 = No0392
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27950*second, mjdStart+27991*second, 'No0392', obsCode, stnCode )
if array.time() < mjdStart + (27991-10)*second:
  subarray.execute(mjdStart + 27986*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27991*second) + ' since array.time is ' + str(array.time())

# Scan 392 = No0393
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28024*second, mjdStart+28046*second, 'No0393', obsCode, stnCode )
if array.time() < mjdStart + (28046-10)*second:
  subarray.execute(mjdStart + 28041*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28046*second) + ' since array.time is ' + str(array.time())

# Scan 393 = No0394
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28080*second, mjdStart+28121*second, 'No0394', obsCode, stnCode )
if array.time() < mjdStart + (28121-10)*second:
  subarray.execute(mjdStart + 28116*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28121*second) + ' since array.time is ' + str(array.time())

# Scan 394 = No0395
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28154*second, mjdStart+28176*second, 'No0395', obsCode, stnCode )
if array.time() < mjdStart + (28176-10)*second:
  subarray.execute(mjdStart + 28171*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28176*second) + ' since array.time is ' + str(array.time())

# Scan 395 = No0396
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28210*second, mjdStart+28251*second, 'No0396', obsCode, stnCode )
if array.time() < mjdStart + (28251-10)*second:
  subarray.execute(mjdStart + 28246*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28251*second) + ' since array.time is ' + str(array.time())

# Scan 396 = No0397
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28284*second, mjdStart+28306*second, 'No0397', obsCode, stnCode )
if array.time() < mjdStart + (28306-10)*second:
  subarray.execute(mjdStart + 28301*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28306*second) + ' since array.time is ' + str(array.time())

# Scan 397 = No0398
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28340*second, mjdStart+28381*second, 'No0398', obsCode, stnCode )
if array.time() < mjdStart + (28381-10)*second:
  subarray.execute(mjdStart + 28376*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28381*second) + ' since array.time is ' + str(array.time())

# Scan 398 = No0399
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28414*second, mjdStart+28436*second, 'No0399', obsCode, stnCode )
if array.time() < mjdStart + (28436-10)*second:
  subarray.execute(mjdStart + 28431*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28436*second) + ' since array.time is ' + str(array.time())

# Scan 399 = No0400
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28470*second, mjdStart+28500*second, 'No0400', obsCode, stnCode )
if array.time() < mjdStart + (28500-10)*second:
  subarray.execute(mjdStart + 28495*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28500*second) + ' since array.time is ' + str(array.time())

# Scan 400 = No0401
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28534*second, mjdStart+28630*second, 'No0401', obsCode, stnCode )
if array.time() < mjdStart + (28630-10)*second:
  subarray.execute(mjdStart + 28625*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28630*second) + ' since array.time is ' + str(array.time())

# Scan 401 = No0402
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28663*second, mjdStart+28700*second, 'No0402', obsCode, stnCode )
if array.time() < mjdStart + (28700-10)*second:
  subarray.execute(mjdStart + 28695*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28700*second) + ' since array.time is ' + str(array.time())

# Scan 402 = No0403
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28733*second, mjdStart+28745*second, 'No0403', obsCode, stnCode )
if array.time() < mjdStart + (28745-10)*second:
  subarray.execute(mjdStart + 28740*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28745*second) + ' since array.time is ' + str(array.time())

# Scan 403 = No0404
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28778*second, mjdStart+28825*second, 'No0404', obsCode, stnCode )
if array.time() < mjdStart + (28825-10)*second:
  subarray.execute(mjdStart + 28820*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28825*second) + ' since array.time is ' + str(array.time())

# Scan 404 = No0405
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28858*second, mjdStart+28875*second, 'No0405', obsCode, stnCode )
if array.time() < mjdStart + (28875-10)*second:
  subarray.execute(mjdStart + 28870*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28875*second) + ' since array.time is ' + str(array.time())

# Scan 405 = No0406
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28908*second, mjdStart+28955*second, 'No0406', obsCode, stnCode )
if array.time() < mjdStart + (28955-10)*second:
  subarray.execute(mjdStart + 28950*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28955*second) + ' since array.time is ' + str(array.time())

# Scan 406 = No0407
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28988*second, mjdStart+29005*second, 'No0407', obsCode, stnCode )
if array.time() < mjdStart + (29005-10)*second:
  subarray.execute(mjdStart + 29000*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29005*second) + ' since array.time is ' + str(array.time())

# Scan 407 = No0408
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29038*second, mjdStart+29085*second, 'No0408', obsCode, stnCode )
if array.time() < mjdStart + (29085-10)*second:
  subarray.execute(mjdStart + 29080*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29085*second) + ' since array.time is ' + str(array.time())

# Scan 408 = No0409
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29118*second, mjdStart+29135*second, 'No0409', obsCode, stnCode )
if array.time() < mjdStart + (29135-10)*second:
  subarray.execute(mjdStart + 29130*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29135*second) + ' since array.time is ' + str(array.time())

# Scan 409 = No0410
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29168*second, mjdStart+29215*second, 'No0410', obsCode, stnCode )
if array.time() < mjdStart + (29215-10)*second:
  subarray.execute(mjdStart + 29210*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29215*second) + ' since array.time is ' + str(array.time())

# Scan 410 = No0411
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29248*second, mjdStart+29265*second, 'No0411', obsCode, stnCode )
if array.time() < mjdStart + (29265-10)*second:
  subarray.execute(mjdStart + 29260*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29265*second) + ' since array.time is ' + str(array.time())

# Scan 411 = No0412
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29298*second, mjdStart+29345*second, 'No0412', obsCode, stnCode )
if array.time() < mjdStart + (29345-10)*second:
  subarray.execute(mjdStart + 29340*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29345*second) + ' since array.time is ' + str(array.time())

# Scan 412 = No0413
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29378*second, mjdStart+29395*second, 'No0413', obsCode, stnCode )
if array.time() < mjdStart + (29395-10)*second:
  subarray.execute(mjdStart + 29390*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29395*second) + ' since array.time is ' + str(array.time())

# Scan 413 = No0414
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29428*second, mjdStart+29475*second, 'No0414', obsCode, stnCode )
if array.time() < mjdStart + (29475-10)*second:
  subarray.execute(mjdStart + 29470*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29475*second) + ' since array.time is ' + str(array.time())

# Scan 414 = No0415
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29508*second, mjdStart+29525*second, 'No0415', obsCode, stnCode )
if array.time() < mjdStart + (29525-10)*second:
  subarray.execute(mjdStart + 29520*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29525*second) + ' since array.time is ' + str(array.time())

# Scan 415 = No0416
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29558*second, mjdStart+29605*second, 'No0416', obsCode, stnCode )
if array.time() < mjdStart + (29605-10)*second:
  subarray.execute(mjdStart + 29600*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29605*second) + ' since array.time is ' + str(array.time())

# Scan 416 = No0417
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29638*second, mjdStart+29655*second, 'No0417', obsCode, stnCode )
if array.time() < mjdStart + (29655-10)*second:
  subarray.execute(mjdStart + 29650*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29655*second) + ' since array.time is ' + str(array.time())

# Scan 417 = No0418
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29688*second, mjdStart+29735*second, 'No0418', obsCode, stnCode )
if array.time() < mjdStart + (29735-10)*second:
  subarray.execute(mjdStart + 29730*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29735*second) + ' since array.time is ' + str(array.time())

# Scan 418 = No0419
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29768*second, mjdStart+29785*second, 'No0419', obsCode, stnCode )
if array.time() < mjdStart + (29785-10)*second:
  subarray.execute(mjdStart + 29780*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29785*second) + ' since array.time is ' + str(array.time())

# Scan 419 = No0420
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29818*second, mjdStart+29865*second, 'No0420', obsCode, stnCode )
if array.time() < mjdStart + (29865-10)*second:
  subarray.execute(mjdStart + 29860*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29865*second) + ' since array.time is ' + str(array.time())

# Scan 420 = No0421
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29898*second, mjdStart+29915*second, 'No0421', obsCode, stnCode )
if array.time() < mjdStart + (29915-10)*second:
  subarray.execute(mjdStart + 29910*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29915*second) + ' since array.time is ' + str(array.time())

# Scan 421 = No0422
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29948*second, mjdStart+29995*second, 'No0422', obsCode, stnCode )
if array.time() < mjdStart + (29995-10)*second:
  subarray.execute(mjdStart + 29990*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29995*second) + ' since array.time is ' + str(array.time())

# Scan 422 = No0423
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30027*second, mjdStart+30045*second, 'No0423', obsCode, stnCode )
if array.time() < mjdStart + (30045-10)*second:
  subarray.execute(mjdStart + 30040*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30045*second) + ' since array.time is ' + str(array.time())

# Scan 423 = No0424
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30078*second, mjdStart+30125*second, 'No0424', obsCode, stnCode )
if array.time() < mjdStart + (30125-10)*second:
  subarray.execute(mjdStart + 30120*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30125*second) + ' since array.time is ' + str(array.time())

# Scan 424 = No0425
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30157*second, mjdStart+30175*second, 'No0425', obsCode, stnCode )
if array.time() < mjdStart + (30175-10)*second:
  subarray.execute(mjdStart + 30170*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30175*second) + ' since array.time is ' + str(array.time())

# Scan 425 = No0426
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30208*second, mjdStart+30255*second, 'No0426', obsCode, stnCode )
if array.time() < mjdStart + (30255-10)*second:
  subarray.execute(mjdStart + 30250*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30255*second) + ' since array.time is ' + str(array.time())

# Scan 426 = No0427
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30287*second, mjdStart+30305*second, 'No0427', obsCode, stnCode )
if array.time() < mjdStart + (30305-10)*second:
  subarray.execute(mjdStart + 30300*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30305*second) + ' since array.time is ' + str(array.time())

# Scan 427 = No0428
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30338*second, mjdStart+30385*second, 'No0428', obsCode, stnCode )
if array.time() < mjdStart + (30385-10)*second:
  subarray.execute(mjdStart + 30380*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30385*second) + ' since array.time is ' + str(array.time())

# Scan 428 = No0429
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30417*second, mjdStart+30435*second, 'No0429', obsCode, stnCode )
if array.time() < mjdStart + (30435-10)*second:
  subarray.execute(mjdStart + 30430*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30435*second) + ' since array.time is ' + str(array.time())

# Scan 429 = No0430
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30468*second, mjdStart+30515*second, 'No0430', obsCode, stnCode )
if array.time() < mjdStart + (30515-10)*second:
  subarray.execute(mjdStart + 30510*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30515*second) + ' since array.time is ' + str(array.time())

# Scan 430 = No0431
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30547*second, mjdStart+30565*second, 'No0431', obsCode, stnCode )
if array.time() < mjdStart + (30565-10)*second:
  subarray.execute(mjdStart + 30560*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30565*second) + ' since array.time is ' + str(array.time())

# Scan 431 = No0432
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30598*second, mjdStart+30645*second, 'No0432', obsCode, stnCode )
if array.time() < mjdStart + (30645-10)*second:
  subarray.execute(mjdStart + 30640*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30645*second) + ' since array.time is ' + str(array.time())

# Scan 432 = No0433
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30677*second, mjdStart+30695*second, 'No0433', obsCode, stnCode )
if array.time() < mjdStart + (30695-10)*second:
  subarray.execute(mjdStart + 30690*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30695*second) + ' since array.time is ' + str(array.time())

# Scan 433 = No0434
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30727*second, mjdStart+30775*second, 'No0434', obsCode, stnCode )
if array.time() < mjdStart + (30775-10)*second:
  subarray.execute(mjdStart + 30770*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30775*second) + ' since array.time is ' + str(array.time())

# Scan 434 = No0435
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30807*second, mjdStart+30825*second, 'No0435', obsCode, stnCode )
if array.time() < mjdStart + (30825-10)*second:
  subarray.execute(mjdStart + 30820*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30825*second) + ' since array.time is ' + str(array.time())

# Scan 435 = No0436
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30857*second, mjdStart+30905*second, 'No0436', obsCode, stnCode )
if array.time() < mjdStart + (30905-10)*second:
  subarray.execute(mjdStart + 30900*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30905*second) + ' since array.time is ' + str(array.time())

# Scan 436 = No0437
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30937*second, mjdStart+30955*second, 'No0437', obsCode, stnCode )
if array.time() < mjdStart + (30955-10)*second:
  subarray.execute(mjdStart + 30950*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30955*second) + ' since array.time is ' + str(array.time())

# Scan 437 = No0438
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30987*second, mjdStart+31035*second, 'No0438', obsCode, stnCode )
if array.time() < mjdStart + (31035-10)*second:
  subarray.execute(mjdStart + 31030*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31035*second) + ' since array.time is ' + str(array.time())

# Scan 438 = No0439
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31067*second, mjdStart+31085*second, 'No0439', obsCode, stnCode )
if array.time() < mjdStart + (31085-10)*second:
  subarray.execute(mjdStart + 31080*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31085*second) + ' since array.time is ' + str(array.time())

# Scan 439 = No0440
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31117*second, mjdStart+31165*second, 'No0440', obsCode, stnCode )
if array.time() < mjdStart + (31165-10)*second:
  subarray.execute(mjdStart + 31160*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31165*second) + ' since array.time is ' + str(array.time())

# Scan 440 = No0441
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31197*second, mjdStart+31215*second, 'No0441', obsCode, stnCode )
if array.time() < mjdStart + (31215-10)*second:
  subarray.execute(mjdStart + 31210*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31215*second) + ' since array.time is ' + str(array.time())

# Scan 441 = No0442
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31247*second, mjdStart+31295*second, 'No0442', obsCode, stnCode )
if array.time() < mjdStart + (31295-10)*second:
  subarray.execute(mjdStart + 31290*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31295*second) + ' since array.time is ' + str(array.time())

# Scan 442 = No0443
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31327*second, mjdStart+31345*second, 'No0443', obsCode, stnCode )
if array.time() < mjdStart + (31345-10)*second:
  subarray.execute(mjdStart + 31340*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31345*second) + ' since array.time is ' + str(array.time())

# Scan 443 = No0444
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31377*second, mjdStart+31425*second, 'No0444', obsCode, stnCode )
if array.time() < mjdStart + (31425-10)*second:
  subarray.execute(mjdStart + 31420*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31425*second) + ' since array.time is ' + str(array.time())

# Scan 444 = No0445
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31457*second, mjdStart+31475*second, 'No0445', obsCode, stnCode )
if array.time() < mjdStart + (31475-10)*second:
  subarray.execute(mjdStart + 31470*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31475*second) + ' since array.time is ' + str(array.time())

# Scan 445 = No0446
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31507*second, mjdStart+31555*second, 'No0446', obsCode, stnCode )
if array.time() < mjdStart + (31555-10)*second:
  subarray.execute(mjdStart + 31550*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31555*second) + ' since array.time is ' + str(array.time())

# Scan 446 = No0447
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31587*second, mjdStart+31605*second, 'No0447', obsCode, stnCode )
if array.time() < mjdStart + (31605-10)*second:
  subarray.execute(mjdStart + 31600*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31605*second) + ' since array.time is ' + str(array.time())

# Scan 447 = No0448
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31637*second, mjdStart+31685*second, 'No0448', obsCode, stnCode )
if array.time() < mjdStart + (31685-10)*second:
  subarray.execute(mjdStart + 31680*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31685*second) + ' since array.time is ' + str(array.time())

# Scan 448 = No0449
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31717*second, mjdStart+31735*second, 'No0449', obsCode, stnCode )
if array.time() < mjdStart + (31735-10)*second:
  subarray.execute(mjdStart + 31730*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31735*second) + ' since array.time is ' + str(array.time())

# Scan 449 = No0450
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31767*second, mjdStart+31815*second, 'No0450', obsCode, stnCode )
if array.time() < mjdStart + (31815-10)*second:
  subarray.execute(mjdStart + 31810*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31815*second) + ' since array.time is ' + str(array.time())

# Scan 450 = No0451
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31847*second, mjdStart+31865*second, 'No0451', obsCode, stnCode )
if array.time() < mjdStart + (31865-10)*second:
  subarray.execute(mjdStart + 31860*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31865*second) + ' since array.time is ' + str(array.time())

# Scan 451 = No0452
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31897*second, mjdStart+31995*second, 'No0452', obsCode, stnCode )
if array.time() < mjdStart + (31995-10)*second:
  subarray.execute(mjdStart + 31990*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31995*second) + ' since array.time is ' + str(array.time())

# Scan 452 = No0453
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32027*second, mjdStart+32035*second, 'No0453', obsCode, stnCode )
if array.time() < mjdStart + (32035-10)*second:
  subarray.execute(mjdStart + 32030*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32035*second) + ' since array.time is ' + str(array.time())

# Scan 453 = No0454
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32067*second, mjdStart+32120*second, 'No0454', obsCode, stnCode )
if array.time() < mjdStart + (32120-10)*second:
  subarray.execute(mjdStart + 32115*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32120*second) + ' since array.time is ' + str(array.time())

# Scan 454 = No0455
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32152*second, mjdStart+32165*second, 'No0455', obsCode, stnCode )
if array.time() < mjdStart + (32165-10)*second:
  subarray.execute(mjdStart + 32160*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32165*second) + ' since array.time is ' + str(array.time())

# Scan 455 = No0456
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32197*second, mjdStart+32250*second, 'No0456', obsCode, stnCode )
if array.time() < mjdStart + (32250-10)*second:
  subarray.execute(mjdStart + 32245*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32250*second) + ' since array.time is ' + str(array.time())

# Scan 456 = No0457
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32282*second, mjdStart+32295*second, 'No0457', obsCode, stnCode )
if array.time() < mjdStart + (32295-10)*second:
  subarray.execute(mjdStart + 32290*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32295*second) + ' since array.time is ' + str(array.time())

# Scan 457 = No0458
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32327*second, mjdStart+32380*second, 'No0458', obsCode, stnCode )
if array.time() < mjdStart + (32380-10)*second:
  subarray.execute(mjdStart + 32375*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32380*second) + ' since array.time is ' + str(array.time())

# Scan 458 = No0459
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32412*second, mjdStart+32425*second, 'No0459', obsCode, stnCode )
if array.time() < mjdStart + (32425-10)*second:
  subarray.execute(mjdStart + 32420*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32425*second) + ' since array.time is ' + str(array.time())

# Scan 459 = No0460
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32457*second, mjdStart+32510*second, 'No0460', obsCode, stnCode )
if array.time() < mjdStart + (32510-10)*second:
  subarray.execute(mjdStart + 32505*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32510*second) + ' since array.time is ' + str(array.time())

# Scan 460 = No0461
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32542*second, mjdStart+32555*second, 'No0461', obsCode, stnCode )
if array.time() < mjdStart + (32555-10)*second:
  subarray.execute(mjdStart + 32550*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32555*second) + ' since array.time is ' + str(array.time())

# Scan 461 = No0462
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32587*second, mjdStart+32640*second, 'No0462', obsCode, stnCode )
if array.time() < mjdStart + (32640-10)*second:
  subarray.execute(mjdStart + 32635*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32640*second) + ' since array.time is ' + str(array.time())

# Scan 462 = No0463
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32672*second, mjdStart+32685*second, 'No0463', obsCode, stnCode )
if array.time() < mjdStart + (32685-10)*second:
  subarray.execute(mjdStart + 32680*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32685*second) + ' since array.time is ' + str(array.time())

# Scan 463 = No0464
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32717*second, mjdStart+32770*second, 'No0464', obsCode, stnCode )
if array.time() < mjdStart + (32770-10)*second:
  subarray.execute(mjdStart + 32765*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32770*second) + ' since array.time is ' + str(array.time())

# Scan 464 = No0465
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32802*second, mjdStart+32815*second, 'No0465', obsCode, stnCode )
if array.time() < mjdStart + (32815-10)*second:
  subarray.execute(mjdStart + 32810*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32815*second) + ' since array.time is ' + str(array.time())

# Scan 465 = No0466
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32847*second, mjdStart+32900*second, 'No0466', obsCode, stnCode )
if array.time() < mjdStart + (32900-10)*second:
  subarray.execute(mjdStart + 32895*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32900*second) + ' since array.time is ' + str(array.time())

# Scan 466 = No0467
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32932*second, mjdStart+32945*second, 'No0467', obsCode, stnCode )
if array.time() < mjdStart + (32945-10)*second:
  subarray.execute(mjdStart + 32940*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32945*second) + ' since array.time is ' + str(array.time())

# Scan 467 = No0468
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32977*second, mjdStart+33030*second, 'No0468', obsCode, stnCode )
if array.time() < mjdStart + (33030-10)*second:
  subarray.execute(mjdStart + 33025*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33030*second) + ' since array.time is ' + str(array.time())

# Scan 468 = No0469
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33062*second, mjdStart+33075*second, 'No0469', obsCode, stnCode )
if array.time() < mjdStart + (33075-10)*second:
  subarray.execute(mjdStart + 33070*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33075*second) + ' since array.time is ' + str(array.time())

# Scan 469 = No0470
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33107*second, mjdStart+33160*second, 'No0470', obsCode, stnCode )
if array.time() < mjdStart + (33160-10)*second:
  subarray.execute(mjdStart + 33155*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33160*second) + ' since array.time is ' + str(array.time())

# Scan 470 = No0471
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33192*second, mjdStart+33205*second, 'No0471', obsCode, stnCode )
if array.time() < mjdStart + (33205-10)*second:
  subarray.execute(mjdStart + 33200*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33205*second) + ' since array.time is ' + str(array.time())

# Scan 471 = No0472
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33237*second, mjdStart+33290*second, 'No0472', obsCode, stnCode )
if array.time() < mjdStart + (33290-10)*second:
  subarray.execute(mjdStart + 33285*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33290*second) + ' since array.time is ' + str(array.time())

# Scan 472 = No0473
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33322*second, mjdStart+33335*second, 'No0473', obsCode, stnCode )
if array.time() < mjdStart + (33335-10)*second:
  subarray.execute(mjdStart + 33330*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33335*second) + ' since array.time is ' + str(array.time())

# Scan 473 = No0474
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33367*second, mjdStart+33420*second, 'No0474', obsCode, stnCode )
if array.time() < mjdStart + (33420-10)*second:
  subarray.execute(mjdStart + 33415*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33420*second) + ' since array.time is ' + str(array.time())

# Scan 474 = No0475
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33451*second, mjdStart+33465*second, 'No0475', obsCode, stnCode )
if array.time() < mjdStart + (33465-10)*second:
  subarray.execute(mjdStart + 33460*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33465*second) + ' since array.time is ' + str(array.time())

# Scan 475 = No0476
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33497*second, mjdStart+33550*second, 'No0476', obsCode, stnCode )
if array.time() < mjdStart + (33550-10)*second:
  subarray.execute(mjdStart + 33545*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33550*second) + ' since array.time is ' + str(array.time())

# Scan 476 = No0477
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33581*second, mjdStart+33595*second, 'No0477', obsCode, stnCode )
if array.time() < mjdStart + (33595-10)*second:
  subarray.execute(mjdStart + 33590*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33595*second) + ' since array.time is ' + str(array.time())

# Scan 477 = No0478
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33627*second, mjdStart+33680*second, 'No0478', obsCode, stnCode )
if array.time() < mjdStart + (33680-10)*second:
  subarray.execute(mjdStart + 33675*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33680*second) + ' since array.time is ' + str(array.time())

# Scan 478 = No0479
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33711*second, mjdStart+33725*second, 'No0479', obsCode, stnCode )
if array.time() < mjdStart + (33725-10)*second:
  subarray.execute(mjdStart + 33720*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33725*second) + ' since array.time is ' + str(array.time())

# Scan 479 = No0480
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33757*second, mjdStart+33810*second, 'No0480', obsCode, stnCode )
if array.time() < mjdStart + (33810-10)*second:
  subarray.execute(mjdStart + 33805*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33810*second) + ' since array.time is ' + str(array.time())

# Scan 480 = No0481
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33841*second, mjdStart+33855*second, 'No0481', obsCode, stnCode )
if array.time() < mjdStart + (33855-10)*second:
  subarray.execute(mjdStart + 33850*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33855*second) + ' since array.time is ' + str(array.time())

# Scan 481 = No0482
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33887*second, mjdStart+33940*second, 'No0482', obsCode, stnCode )
if array.time() < mjdStart + (33940-10)*second:
  subarray.execute(mjdStart + 33935*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33940*second) + ' since array.time is ' + str(array.time())

# Scan 482 = No0483
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33971*second, mjdStart+33985*second, 'No0483', obsCode, stnCode )
if array.time() < mjdStart + (33985-10)*second:
  subarray.execute(mjdStart + 33980*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33985*second) + ' since array.time is ' + str(array.time())

# Scan 483 = No0484
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34017*second, mjdStart+34070*second, 'No0484', obsCode, stnCode )
if array.time() < mjdStart + (34070-10)*second:
  subarray.execute(mjdStart + 34065*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34070*second) + ' since array.time is ' + str(array.time())

# Scan 484 = No0485
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34101*second, mjdStart+34115*second, 'No0485', obsCode, stnCode )
if array.time() < mjdStart + (34115-10)*second:
  subarray.execute(mjdStart + 34110*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34115*second) + ' since array.time is ' + str(array.time())

# Scan 485 = No0486
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34147*second, mjdStart+34200*second, 'No0486', obsCode, stnCode )
if array.time() < mjdStart + (34200-10)*second:
  subarray.execute(mjdStart + 34195*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34200*second) + ' since array.time is ' + str(array.time())

# Scan 486 = No0487
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34231*second, mjdStart+34245*second, 'No0487', obsCode, stnCode )
if array.time() < mjdStart + (34245-10)*second:
  subarray.execute(mjdStart + 34240*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34245*second) + ' since array.time is ' + str(array.time())

# Scan 487 = No0488
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34277*second, mjdStart+34330*second, 'No0488', obsCode, stnCode )
if array.time() < mjdStart + (34330-10)*second:
  subarray.execute(mjdStart + 34325*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34330*second) + ' since array.time is ' + str(array.time())

# Scan 488 = No0489
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34361*second, mjdStart+34375*second, 'No0489', obsCode, stnCode )
if array.time() < mjdStart + (34375-10)*second:
  subarray.execute(mjdStart + 34370*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34375*second) + ' since array.time is ' + str(array.time())

# Scan 489 = No0490
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34406*second, mjdStart+34460*second, 'No0490', obsCode, stnCode )
if array.time() < mjdStart + (34460-10)*second:
  subarray.execute(mjdStart + 34455*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34460*second) + ' since array.time is ' + str(array.time())

# Scan 490 = No0491
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34491*second, mjdStart+34505*second, 'No0491', obsCode, stnCode )
if array.time() < mjdStart + (34505-10)*second:
  subarray.execute(mjdStart + 34500*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34505*second) + ' since array.time is ' + str(array.time())

# Scan 491 = No0492
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34536*second, mjdStart+34590*second, 'No0492', obsCode, stnCode )
if array.time() < mjdStart + (34590-10)*second:
  subarray.execute(mjdStart + 34585*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34590*second) + ' since array.time is ' + str(array.time())

# Scan 492 = No0493
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34621*second, mjdStart+34635*second, 'No0493', obsCode, stnCode )
if array.time() < mjdStart + (34635-10)*second:
  subarray.execute(mjdStart + 34630*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34635*second) + ' since array.time is ' + str(array.time())

# Scan 493 = No0494
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34666*second, mjdStart+34720*second, 'No0494', obsCode, stnCode )
if array.time() < mjdStart + (34720-10)*second:
  subarray.execute(mjdStart + 34715*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34720*second) + ' since array.time is ' + str(array.time())

# Scan 494 = No0495
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34751*second, mjdStart+34765*second, 'No0495', obsCode, stnCode )
if array.time() < mjdStart + (34765-10)*second:
  subarray.execute(mjdStart + 34760*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34765*second) + ' since array.time is ' + str(array.time())

# Scan 495 = No0496
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34796*second, mjdStart+34850*second, 'No0496', obsCode, stnCode )
if array.time() < mjdStart + (34850-10)*second:
  subarray.execute(mjdStart + 34845*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34850*second) + ' since array.time is ' + str(array.time())

# Scan 496 = No0497
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34881*second, mjdStart+34895*second, 'No0497', obsCode, stnCode )
if array.time() < mjdStart + (34895-10)*second:
  subarray.execute(mjdStart + 34890*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34895*second) + ' since array.time is ' + str(array.time())

# Scan 497 = No0498
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34926*second, mjdStart+34980*second, 'No0498', obsCode, stnCode )
if array.time() < mjdStart + (34980-10)*second:
  subarray.execute(mjdStart + 34975*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34980*second) + ' since array.time is ' + str(array.time())

# Scan 498 = No0499
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35011*second, mjdStart+35025*second, 'No0499', obsCode, stnCode )
if array.time() < mjdStart + (35025-10)*second:
  subarray.execute(mjdStart + 35020*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35025*second) + ' since array.time is ' + str(array.time())

# Scan 499 = No0500
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35056*second, mjdStart+35110*second, 'No0500', obsCode, stnCode )
if array.time() < mjdStart + (35110-10)*second:
  subarray.execute(mjdStart + 35105*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35110*second) + ' since array.time is ' + str(array.time())

# Scan 500 = No0501
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35141*second, mjdStart+35155*second, 'No0501', obsCode, stnCode )
if array.time() < mjdStart + (35155-10)*second:
  subarray.execute(mjdStart + 35150*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35155*second) + ' since array.time is ' + str(array.time())

# Scan 501 = No0502
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35283*second, mjdStart+35432*second, 'No0502', obsCode, stnCode )
if array.time() < mjdStart + (35432-10)*second:
  subarray.execute(mjdStart + 35427*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35432*second) + ' since array.time is ' + str(array.time())

# Scan 502 = No0503
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35852*second, mjdStart+36000*second, 'No0503', obsCode, stnCode )
if array.time() < mjdStart + (36000-10)*second:
  subarray.execute(mjdStart + 35995*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+36000*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 36001*second)
