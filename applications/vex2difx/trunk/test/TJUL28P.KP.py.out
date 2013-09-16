from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'TJUL28P'
stnCode = 'KP'
mjdStart = 56501 + 7200*second

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
loif0.setIf('B', '90cm', 'R', -500, 'U', 'NARROW', 0, '90cm', 15400, 15400, 824.49)
loif0.setIf('D', '90cm', 'L', -500, 'U', 'NARROW', 0, '90cm', 15400, 15400, 824.49)
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
  bbc(0, 624, 32, 'L', 2, 0), \
  bbc(1, 912, 32, 'L', 2, 0), \
  bbc(1, 880, 32, 'L', 2, 0), \
  bbc(1, 848, 32, 'L', 2, 0), \
  bbc(1, 816, 32, 'L', 2, 0), \
  bbc(1, 784, 32, 'L', 2, 0), \
  bbc(1, 752, 32, 'L', 2, 0), \
  bbc(1, 720, 32, 'L', 2, 0), \
  bbc(1, 624, 32, 'L', 2, 0) \
  ]
# implicit conversion performed on basebands: 2 3

source0 = Source(3.27608648947359, 0.216265880794814)
source0.setName('3C274')

source1 = Source(5.23372683818036, 0.711007232350209)
source1.setName('CYGA')

# Setup Scan 
# changing to mode pt90cm
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source1)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
print 'Not a recording scan but still set switches for No0001.'
subarray.setSwitches(mjdStart + 0*second, mjdStart+345*second, obsCode+'_'+stnCode+'_'+'No0001')
if array.time() < mjdStart + (345-10)*second:
  subarray.execute(mjdStart + 340*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+345*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0003
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0003.'
subarray.setSwitches(mjdStart + 495*second, mjdStart+840*second, obsCode+'_'+stnCode+'_'+'No0003')
if array.time() < mjdStart + (840-10)*second:
  subarray.execute(mjdStart + 835*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+840*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0004
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0004.'
subarray.setSwitches(mjdStart + 972*second, mjdStart+1317*second, obsCode+'_'+stnCode+'_'+'No0004')
if array.time() < mjdStart + (1317-10)*second:
  subarray.execute(mjdStart + 1312*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1317*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0006
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0006.'
subarray.setSwitches(mjdStart + 1449*second, mjdStart+1794*second, obsCode+'_'+stnCode+'_'+'No0006')
if array.time() < mjdStart + (1794-10)*second:
  subarray.execute(mjdStart + 1789*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1794*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0007
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0007.'
subarray.setSwitches(mjdStart + 1925*second, mjdStart+2270*second, obsCode+'_'+stnCode+'_'+'No0007')
if array.time() < mjdStart + (2270-10)*second:
  subarray.execute(mjdStart + 2265*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2270*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0009
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0009.'
subarray.setSwitches(mjdStart + 2400*second, mjdStart+2745*second, obsCode+'_'+stnCode+'_'+'No0009')
if array.time() < mjdStart + (2745-10)*second:
  subarray.execute(mjdStart + 2740*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2745*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0010
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0010.'
subarray.setSwitches(mjdStart + 2874*second, mjdStart+3219*second, obsCode+'_'+stnCode+'_'+'No0010')
if array.time() < mjdStart + (3219-10)*second:
  subarray.execute(mjdStart + 3214*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3219*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0012
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0012.'
subarray.setSwitches(mjdStart + 3348*second, mjdStart+3693*second, obsCode+'_'+stnCode+'_'+'No0012')
if array.time() < mjdStart + (3693-10)*second:
  subarray.execute(mjdStart + 3688*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3693*second) + ' since array.time is ' + str(array.time())

# Scan 8 = No0013
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0013.'
subarray.setSwitches(mjdStart + 3821*second, mjdStart+4166*second, obsCode+'_'+stnCode+'_'+'No0013')
if array.time() < mjdStart + (4166-10)*second:
  subarray.execute(mjdStart + 4161*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4166*second) + ' since array.time is ' + str(array.time())

# Scan 9 = No0015
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0015.'
subarray.setSwitches(mjdStart + 4293*second, mjdStart+4638*second, obsCode+'_'+stnCode+'_'+'No0015')
if array.time() < mjdStart + (4638-10)*second:
  subarray.execute(mjdStart + 4633*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4638*second) + ' since array.time is ' + str(array.time())

# Scan 10 = No0016
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0016.'
subarray.setSwitches(mjdStart + 4764*second, mjdStart+5109*second, obsCode+'_'+stnCode+'_'+'No0016')
if array.time() < mjdStart + (5109-10)*second:
  subarray.execute(mjdStart + 5104*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5109*second) + ' since array.time is ' + str(array.time())

# Scan 11 = No0018
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0018.'
subarray.setSwitches(mjdStart + 5235*second, mjdStart+5580*second, obsCode+'_'+stnCode+'_'+'No0018')
if array.time() < mjdStart + (5580-10)*second:
  subarray.execute(mjdStart + 5575*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5580*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0019
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0019.'
subarray.setSwitches(mjdStart + 5705*second, mjdStart+6050*second, obsCode+'_'+stnCode+'_'+'No0019')
if array.time() < mjdStart + (6050-10)*second:
  subarray.execute(mjdStart + 6045*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6050*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0021
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0021.'
subarray.setSwitches(mjdStart + 6175*second, mjdStart+6520*second, obsCode+'_'+stnCode+'_'+'No0021')
if array.time() < mjdStart + (6520-10)*second:
  subarray.execute(mjdStart + 6515*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6520*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0022
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0022.'
subarray.setSwitches(mjdStart + 6643*second, mjdStart+6988*second, obsCode+'_'+stnCode+'_'+'No0022')
if array.time() < mjdStart + (6988-10)*second:
  subarray.execute(mjdStart + 6983*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6988*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0024
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0024.'
subarray.setSwitches(mjdStart + 7111*second, mjdStart+7456*second, obsCode+'_'+stnCode+'_'+'No0024')
if array.time() < mjdStart + (7456-10)*second:
  subarray.execute(mjdStart + 7451*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7456*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0025
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0025.'
subarray.setSwitches(mjdStart + 7579*second, mjdStart+7924*second, obsCode+'_'+stnCode+'_'+'No0025')
if array.time() < mjdStart + (7924-10)*second:
  subarray.execute(mjdStart + 7919*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7924*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0027
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0027.'
subarray.setSwitches(mjdStart + 8045*second, mjdStart+8390*second, obsCode+'_'+stnCode+'_'+'No0027')
if array.time() < mjdStart + (8390-10)*second:
  subarray.execute(mjdStart + 8385*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8390*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0028
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0028.'
subarray.setSwitches(mjdStart + 8511*second, mjdStart+8856*second, obsCode+'_'+stnCode+'_'+'No0028')
if array.time() < mjdStart + (8856-10)*second:
  subarray.execute(mjdStart + 8851*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8856*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0031
print 'Not a recording scan but still set switches for No0031.'
subarray.setSwitches(mjdStart + 8862*second, mjdStart+9207*second, obsCode+'_'+stnCode+'_'+'No0031')
if array.time() < mjdStart + (9207-10)*second:
  subarray.execute(mjdStart + 9202*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9207*second) + ' since array.time is ' + str(array.time())

# Scan 20 = No0034
print 'Not a recording scan but still set switches for No0034.'
subarray.setSwitches(mjdStart + 9213*second, mjdStart+9558*second, obsCode+'_'+stnCode+'_'+'No0034')
if array.time() < mjdStart + (9558-10)*second:
  subarray.execute(mjdStart + 9553*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9558*second) + ' since array.time is ' + str(array.time())

# Scan 21 = No0037
print 'Not a recording scan but still set switches for No0037.'
subarray.setSwitches(mjdStart + 9564*second, mjdStart+9909*second, obsCode+'_'+stnCode+'_'+'No0037')
if array.time() < mjdStart + (9909-10)*second:
  subarray.execute(mjdStart + 9904*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9909*second) + ' since array.time is ' + str(array.time())

# Scan 22 = No0040
print 'Not a recording scan but still set switches for No0040.'
subarray.setSwitches(mjdStart + 9915*second, mjdStart+10260*second, obsCode+'_'+stnCode+'_'+'No0040')
if array.time() < mjdStart + (10260-10)*second:
  subarray.execute(mjdStart + 10255*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10260*second) + ' since array.time is ' + str(array.time())

# Scan 23 = No0043
print 'Not a recording scan but still set switches for No0043.'
subarray.setSwitches(mjdStart + 10266*second, mjdStart+10611*second, obsCode+'_'+stnCode+'_'+'No0043')
if array.time() < mjdStart + (10611-10)*second:
  subarray.execute(mjdStart + 10606*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10611*second) + ' since array.time is ' + str(array.time())

# Scan 24 = No0046
print 'Not a recording scan but still set switches for No0046.'
subarray.setSwitches(mjdStart + 10617*second, mjdStart+10962*second, obsCode+'_'+stnCode+'_'+'No0046')
if array.time() < mjdStart + (10962-10)*second:
  subarray.execute(mjdStart + 10957*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10962*second) + ' since array.time is ' + str(array.time())

# Scan 25 = No0049
print 'Not a recording scan but still set switches for No0049.'
subarray.setSwitches(mjdStart + 10968*second, mjdStart+11313*second, obsCode+'_'+stnCode+'_'+'No0049')
if array.time() < mjdStart + (11313-10)*second:
  subarray.execute(mjdStart + 11308*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11313*second) + ' since array.time is ' + str(array.time())

# Scan 26 = No0052
print 'Not a recording scan but still set switches for No0052.'
subarray.setSwitches(mjdStart + 11319*second, mjdStart+11664*second, obsCode+'_'+stnCode+'_'+'No0052')
if array.time() < mjdStart + (11664-10)*second:
  subarray.execute(mjdStart + 11659*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11664*second) + ' since array.time is ' + str(array.time())

# Scan 27 = No0055
print 'Not a recording scan but still set switches for No0055.'
subarray.setSwitches(mjdStart + 11670*second, mjdStart+12015*second, obsCode+'_'+stnCode+'_'+'No0055')
if array.time() < mjdStart + (12015-10)*second:
  subarray.execute(mjdStart + 12010*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12015*second) + ' since array.time is ' + str(array.time())

# Scan 28 = No0058
print 'Not a recording scan but still set switches for No0058.'
subarray.setSwitches(mjdStart + 12021*second, mjdStart+12366*second, obsCode+'_'+stnCode+'_'+'No0058')
if array.time() < mjdStart + (12366-10)*second:
  subarray.execute(mjdStart + 12361*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12366*second) + ' since array.time is ' + str(array.time())

# Scan 29 = No0061
print 'Not a recording scan but still set switches for No0061.'
subarray.setSwitches(mjdStart + 12372*second, mjdStart+12717*second, obsCode+'_'+stnCode+'_'+'No0061')
if array.time() < mjdStart + (12717-10)*second:
  subarray.execute(mjdStart + 12712*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12717*second) + ' since array.time is ' + str(array.time())

# Scan 30 = No0064
print 'Not a recording scan but still set switches for No0064.'
subarray.setSwitches(mjdStart + 12723*second, mjdStart+13068*second, obsCode+'_'+stnCode+'_'+'No0064')
if array.time() < mjdStart + (13068-10)*second:
  subarray.execute(mjdStart + 13063*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13068*second) + ' since array.time is ' + str(array.time())

# Scan 31 = No0067
print 'Not a recording scan but still set switches for No0067.'
subarray.setSwitches(mjdStart + 13074*second, mjdStart+13419*second, obsCode+'_'+stnCode+'_'+'No0067')
if array.time() < mjdStart + (13419-10)*second:
  subarray.execute(mjdStart + 13414*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13419*second) + ' since array.time is ' + str(array.time())

# Scan 32 = No0070
print 'Not a recording scan but still set switches for No0070.'
subarray.setSwitches(mjdStart + 13425*second, mjdStart+13770*second, obsCode+'_'+stnCode+'_'+'No0070')
if array.time() < mjdStart + (13770-10)*second:
  subarray.execute(mjdStart + 13765*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13770*second) + ' since array.time is ' + str(array.time())

# Scan 33 = No0073
print 'Not a recording scan but still set switches for No0073.'
subarray.setSwitches(mjdStart + 13776*second, mjdStart+14121*second, obsCode+'_'+stnCode+'_'+'No0073')
if array.time() < mjdStart + (14121-10)*second:
  subarray.execute(mjdStart + 14116*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14121*second) + ' since array.time is ' + str(array.time())

# Scan 34 = No0076
print 'Not a recording scan but still set switches for No0076.'
subarray.setSwitches(mjdStart + 14127*second, mjdStart+14472*second, obsCode+'_'+stnCode+'_'+'No0076')
if array.time() < mjdStart + (14472-10)*second:
  subarray.execute(mjdStart + 14467*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14472*second) + ' since array.time is ' + str(array.time())

# Scan 35 = No0079
print 'Not a recording scan but still set switches for No0079.'
subarray.setSwitches(mjdStart + 14478*second, mjdStart+14823*second, obsCode+'_'+stnCode+'_'+'No0079')
if array.time() < mjdStart + (14823-10)*second:
  subarray.execute(mjdStart + 14818*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14823*second) + ' since array.time is ' + str(array.time())

# Scan 36 = No0082
print 'Not a recording scan but still set switches for No0082.'
subarray.setSwitches(mjdStart + 14829*second, mjdStart+15174*second, obsCode+'_'+stnCode+'_'+'No0082')
if array.time() < mjdStart + (15174-10)*second:
  subarray.execute(mjdStart + 15169*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15174*second) + ' since array.time is ' + str(array.time())

# Scan 37 = No0085
print 'Not a recording scan but still set switches for No0085.'
subarray.setSwitches(mjdStart + 15180*second, mjdStart+15525*second, obsCode+'_'+stnCode+'_'+'No0085')
if array.time() < mjdStart + (15525-10)*second:
  subarray.execute(mjdStart + 15520*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15525*second) + ' since array.time is ' + str(array.time())

# Scan 38 = No0088
print 'Not a recording scan but still set switches for No0088.'
subarray.setSwitches(mjdStart + 15531*second, mjdStart+15876*second, obsCode+'_'+stnCode+'_'+'No0088')
if array.time() < mjdStart + (15876-10)*second:
  subarray.execute(mjdStart + 15871*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15876*second) + ' since array.time is ' + str(array.time())

# Scan 39 = No0091
print 'Not a recording scan but still set switches for No0091.'
subarray.setSwitches(mjdStart + 15882*second, mjdStart+16227*second, obsCode+'_'+stnCode+'_'+'No0091')
if array.time() < mjdStart + (16227-10)*second:
  subarray.execute(mjdStart + 16222*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16227*second) + ' since array.time is ' + str(array.time())

# Scan 40 = No0094
print 'Not a recording scan but still set switches for No0094.'
subarray.setSwitches(mjdStart + 16233*second, mjdStart+16578*second, obsCode+'_'+stnCode+'_'+'No0094')
if array.time() < mjdStart + (16578-10)*second:
  subarray.execute(mjdStart + 16573*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16578*second) + ' since array.time is ' + str(array.time())

# Scan 41 = No0097
print 'Not a recording scan but still set switches for No0097.'
subarray.setSwitches(mjdStart + 16584*second, mjdStart+16929*second, obsCode+'_'+stnCode+'_'+'No0097')
if array.time() < mjdStart + (16929-10)*second:
  subarray.execute(mjdStart + 16924*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16929*second) + ' since array.time is ' + str(array.time())

# Scan 42 = No0100
print 'Not a recording scan but still set switches for No0100.'
subarray.setSwitches(mjdStart + 16935*second, mjdStart+17280*second, obsCode+'_'+stnCode+'_'+'No0100')
if array.time() < mjdStart + (17280-10)*second:
  subarray.execute(mjdStart + 17275*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17280*second) + ' since array.time is ' + str(array.time())

# Scan 43 = No0103
print 'Not a recording scan but still set switches for No0103.'
subarray.setSwitches(mjdStart + 17286*second, mjdStart+17631*second, obsCode+'_'+stnCode+'_'+'No0103')
if array.time() < mjdStart + (17631-10)*second:
  subarray.execute(mjdStart + 17626*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17631*second) + ' since array.time is ' + str(array.time())

# Scan 44 = No0106
print 'Not a recording scan but still set switches for No0106.'
subarray.setSwitches(mjdStart + 17637*second, mjdStart+17982*second, obsCode+'_'+stnCode+'_'+'No0106')
if array.time() < mjdStart + (17982-10)*second:
  subarray.execute(mjdStart + 17977*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17982*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 17983*second)
