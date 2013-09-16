from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'BD170AR'
stnCode = 'LA'
mjdStart = 56493 + 10216*second

# File written by vex2script version 0.19 vintage 20130912

dbe0 = RDBE(0, 'pfb')
dbe0.setALC(1)
dbe0.setFormat('Mark5B')
dbe0.setPSNMode(0)
dbe0.setPacket(0, 0, 36, 5008)
subarray.setDBE(dbe0)

# XCUBE init
essr = ESSR()          # constructor
essr.setMode(4)        # one in, one out
essr.setRoute(2, 4)    # route incoming traffic from input 1 to output 1
essr.setPace(4, 5)     # set port 4 packet pacing to 5
subarray.setESSR(essr)

loif0 = VLBALoIfSetup()
loif0.setIf('A', '20cm', 'R', 2100, 'L', 'NA', 0, '20cm', 15100, 2100, 756.51)
loif0.setIf('C', '20cm', 'L', 2100, 'L', 'NA', 0, '20cm', 15100, 2100, 756.51)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBEParams(1, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
loif0.setDBERemember(1, 1)
channelSet0 = [ \
  bbc(0, 912, 32, 'L', 2, 0), \
  bbc(0, 880, 32, 'L', 2, 0), \
  bbc(0, 848, 32, 'L', 2, 0), \
  bbc(0, 816, 32, 'L', 2, 0), \
  bbc(0, 784, 32, 'L', 2, 0), \
  bbc(0, 752, 32, 'L', 2, 0), \
  bbc(0, 720, 32, 'L', 2, 0), \
  bbc(0, 688, 32, 'L', 2, 0), \
  bbc(1, 912, 32, 'L', 2, 0), \
  bbc(1, 880, 32, 'L', 2, 0), \
  bbc(1, 848, 32, 'L', 2, 0), \
  bbc(1, 816, 32, 'L', 2, 0), \
  bbc(1, 784, 32, 'L', 2, 0), \
  bbc(1, 752, 32, 'L', 2, 0), \
  bbc(1, 720, 32, 'L', 2, 0), \
  bbc(1, 688, 32, 'L', 2, 0) \
  ]

source0 = Source(4.20023136890419, 1.00033140773818)
source0.setName('J1603+5730-1')

source1 = Source(4.20023136890419, 1.00732483054377)
source1.setName('J1603+5730-2')

source2 = Source(4.21166387909779, 1.00033140773818)
source2.setName('J1603+5730-3')

source3 = Source(4.21166387909779, 1.00732483054377)
source3.setName('J1603+5730-4')

# Setup Scan 
# changing to mode 21cm-vlba
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 3)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source0)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
print 'Not a recording scan but still set switches for No0001.'
subarray.setSwitches(mjdStart + 0*second, mjdStart+179*second, obsCode+'_'+stnCode+'_'+'No0001')
if array.time() < mjdStart + (179-10)*second:
  subarray.execute(mjdStart + 174*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+179*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0002
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0002.'
subarray.setSwitches(mjdStart + 187*second, mjdStart+359*second, obsCode+'_'+stnCode+'_'+'No0002')
if array.time() < mjdStart + (359-10)*second:
  subarray.execute(mjdStart + 354*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+359*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0003
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0003.'
subarray.setSwitches(mjdStart + 367*second, mjdStart+538*second, obsCode+'_'+stnCode+'_'+'No0003')
if array.time() < mjdStart + (538-10)*second:
  subarray.execute(mjdStart + 533*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+538*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0004
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0004.'
subarray.setSwitches(mjdStart + 547*second, mjdStart+718*second, obsCode+'_'+stnCode+'_'+'No0004')
if array.time() < mjdStart + (718-10)*second:
  subarray.execute(mjdStart + 713*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+718*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0005
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0005.'
subarray.setSwitches(mjdStart + 727*second, mjdStart+899*second, obsCode+'_'+stnCode+'_'+'No0005')
if array.time() < mjdStart + (899-10)*second:
  subarray.execute(mjdStart + 894*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+899*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0006
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0006.'
subarray.setSwitches(mjdStart + 908*second, mjdStart+1079*second, obsCode+'_'+stnCode+'_'+'No0006')
if array.time() < mjdStart + (1079-10)*second:
  subarray.execute(mjdStart + 1074*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1079*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0007
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0007.'
subarray.setSwitches(mjdStart + 1087*second, mjdStart+1258*second, obsCode+'_'+stnCode+'_'+'No0007')
if array.time() < mjdStart + (1258-10)*second:
  subarray.execute(mjdStart + 1253*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1258*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0008
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0008.'
subarray.setSwitches(mjdStart + 1267*second, mjdStart+1438*second, obsCode+'_'+stnCode+'_'+'No0008')
if array.time() < mjdStart + (1438-10)*second:
  subarray.execute(mjdStart + 1433*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1438*second) + ' since array.time is ' + str(array.time())

# Scan 8 = No0009
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0009.'
subarray.setSwitches(mjdStart + 1447*second, mjdStart+1617*second, obsCode+'_'+stnCode+'_'+'No0009')
if array.time() < mjdStart + (1617-10)*second:
  subarray.execute(mjdStart + 1612*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1617*second) + ' since array.time is ' + str(array.time())

# Scan 9 = No0010
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0010.'
subarray.setSwitches(mjdStart + 1626*second, mjdStart+1799*second, obsCode+'_'+stnCode+'_'+'No0010')
if array.time() < mjdStart + (1799-10)*second:
  subarray.execute(mjdStart + 1794*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1799*second) + ' since array.time is ' + str(array.time())

# Scan 10 = No0011
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0011.'
subarray.setSwitches(mjdStart + 1808*second, mjdStart+1978*second, obsCode+'_'+stnCode+'_'+'No0011')
if array.time() < mjdStart + (1978-10)*second:
  subarray.execute(mjdStart + 1973*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1978*second) + ' since array.time is ' + str(array.time())

# Scan 11 = No0012
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0012.'
subarray.setSwitches(mjdStart + 1987*second, mjdStart+2158*second, obsCode+'_'+stnCode+'_'+'No0012')
if array.time() < mjdStart + (2158-10)*second:
  subarray.execute(mjdStart + 2153*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2158*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0013
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0013.'
subarray.setSwitches(mjdStart + 2166*second, mjdStart+2337*second, obsCode+'_'+stnCode+'_'+'No0013')
if array.time() < mjdStart + (2337-10)*second:
  subarray.execute(mjdStart + 2332*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2337*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0014
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0014.'
subarray.setSwitches(mjdStart + 2346*second, mjdStart+2517*second, obsCode+'_'+stnCode+'_'+'No0014')
if array.time() < mjdStart + (2517-10)*second:
  subarray.execute(mjdStart + 2512*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2517*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0015
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0015.'
subarray.setSwitches(mjdStart + 2526*second, mjdStart+2698*second, obsCode+'_'+stnCode+'_'+'No0015')
if array.time() < mjdStart + (2698-10)*second:
  subarray.execute(mjdStart + 2693*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2698*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0016
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0016.'
subarray.setSwitches(mjdStart + 2707*second, mjdStart+2878*second, obsCode+'_'+stnCode+'_'+'No0016')
if array.time() < mjdStart + (2878-10)*second:
  subarray.execute(mjdStart + 2873*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2878*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0017
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0017.'
subarray.setSwitches(mjdStart + 2886*second, mjdStart+3057*second, obsCode+'_'+stnCode+'_'+'No0017')
if array.time() < mjdStart + (3057-10)*second:
  subarray.execute(mjdStart + 3052*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3057*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0018
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0018.'
subarray.setSwitches(mjdStart + 3065*second, mjdStart+3237*second, obsCode+'_'+stnCode+'_'+'No0018')
if array.time() < mjdStart + (3237-10)*second:
  subarray.execute(mjdStart + 3232*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3237*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0019
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0019.'
subarray.setSwitches(mjdStart + 3246*second, mjdStart+3416*second, obsCode+'_'+stnCode+'_'+'No0019')
if array.time() < mjdStart + (3416-10)*second:
  subarray.execute(mjdStart + 3411*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3416*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0020
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0020.'
subarray.setSwitches(mjdStart + 3424*second, mjdStart+3598*second, obsCode+'_'+stnCode+'_'+'No0020')
if array.time() < mjdStart + (3598-10)*second:
  subarray.execute(mjdStart + 3593*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3598*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 3599*second)
