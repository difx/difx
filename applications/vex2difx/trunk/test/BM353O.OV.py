from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'BM353O'
stnCode = 'OV'
mjdStart = 56473 + 45450*second

# File written by vex2script version 0.21 vintage 20130923

dbe0 = RDBE(0, 'ddc')
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
loif0.setIf('A', '7mm', 'R', 42400, 'U', 'NA', 34800, '7mm', 703.25)
loif0.setIf('C', '7mm', 'L', 42400, 'U', 'NA', 34800, '7mm', 703.25)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBEParams(1, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
loif0.setDBERemember(1, 1)
channelSet0 = [ \
  bbc(0, 703.25, 32, 'U', 2, 0), \  # IF A
  bbc(1, 703.25, 32, 'U', 2, 0), \  # IF C
  bbc(0, 735.25, 32, 'U', 2, 0), \  # IF A
  bbc(1, 735.25, 32, 'U', 2, 0) \  # IF C
  ]

source0 = Source(1.12728489280644, 0.663689373338876)
source0.setName('3C111')

source1 = Source(0.622472477666351, 0.751111165393883)
source1.setName('3C66A')

source2 = Source(0.692236131809859, 0.290012026716468)
source2.setName('0235+164')

source3 = Source(0.871803603444499, 0.724515766111718)
source3.setName('3C84')

source4 = Source(1.14870304447943, -0.0234313630033918)
source4.setName('0420-014')

source5 = Source(1.19199410397944, 0.0934508390486579)
source5.setName('3C120')

source6 = Source(1.44399937465916, 0.236177709258076)
source6.setName('0528+134')

source7 = Source(1.92811238318364, 1.24517782782741)
source7.setName('0716+714')

source8 = Source(1.99893968235216, 0.309015383039429)
source8.setName('0735+178')

source9 = Source(2.22908261113727, 0.422077919418333)
source9.setName('0827+243')

source10 = Source(2.23321255211217, 0.0784384213883689)
source10.setName('0829+046')

source11 = Source(2.27506324410399, 1.23735201202601)
source11.setName('0836+710')

source12 = Source(2.33356883662152, 0.350959730344299)
source12.setName('OJ287')

source13 = Source(2.61270299344916, 1.14432908994296)
source13.setName('0954+658')

source14 = Source(2.87321957093852, 0.0273377882179519)
source14.setName('1055+018')

source15 = Source(2.89923288444375, 0.666869939921883)
source15.setName('1101+384')

source16 = Source(3.01120583734455, -0.258732399971657)
source16.setName('1127-145')

source17 = Source(3.13954435786893, 0.510430396225105)
source17.setName('1156+295')

source18 = Source(3.25027273487882, 0.373143581594805)
source18.setName('1222+216')

source19 = Source(3.26861624240122, 0.0358209353036639)
source19.setName('3C273')

source20 = Source(3.38675080453689, -0.101042563630718)
source20.setName('3C279')

source21 = Source(3.44910976675651, 0.564535389670429)
source21.setName('1308+326')

source22 = Source(3.98302555262777, -0.15882413682708)
source22.setName('1510-089')

source23 = Source(4.24849968142511, 0.597134872501953)
source23.setName('1611+343')

source24 = Source(4.34263319527206, 0.66557404012752)
source24.setName('1633+382')

source25 = Source(4.37632655758843, 0.694820394363896)
source25.setName('3C345')

source26 = Source(4.59477602625844, -0.228296573015897)
source26.setName('1730-130')

source27 = Source(4.67550570611983, 0.168427803667011)
source27.setName('1749+096')

source28 = Source(5.77146141518148, 0.73788632505572)
source28.setName('BLLAC')

source29 = Source(5.87210640274727, -0.0864005412956912)
source29.setName('3C446')

source30 = Source(5.90186060202957, 0.204741199957346)
source30.setName('CTA102')

source31 = Source(5.99504220595755, 0.281839456704788)
source31.setName('3C454.3')

# Setup Scan 
# changing to mode bm353
subarray.setChannels(dbe0, channelSet0)
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 1)
subarray.set4x4Switch('1B', 3)
subarray.setSource(source2)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 0*second, mjdStart+300*second, 'No0001', obsCode, stnCode )
if array.time() < mjdStart + (300-10)*second:
  subarray.execute(mjdStart + 295*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+300*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0002
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 378*second, mjdStart+599*second, 'No0002', obsCode, stnCode )
if array.time() < mjdStart + (599-10)*second:
  subarray.execute(mjdStart + 594*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+599*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0003
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 641*second, mjdStart+898*second, 'No0003', obsCode, stnCode )
if array.time() < mjdStart + (898-10)*second:
  subarray.execute(mjdStart + 893*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+898*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0004
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 919*second, mjdStart+1137*second, 'No0004', obsCode, stnCode )
if array.time() < mjdStart + (1137-10)*second:
  subarray.execute(mjdStart + 1132*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1137*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0005
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1245*second, mjdStart+1377*second, 'No0005', obsCode, stnCode )
if array.time() < mjdStart + (1377-10)*second:
  subarray.execute(mjdStart + 1372*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1377*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0006
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1407*second, mjdStart+1616*second, 'No0006', obsCode, stnCode )
if array.time() < mjdStart + (1616-10)*second:
  subarray.execute(mjdStart + 1611*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1616*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0007
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1694*second, mjdStart+1855*second, 'No0007', obsCode, stnCode )
if array.time() < mjdStart + (1855-10)*second:
  subarray.execute(mjdStart + 1850*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1855*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0008
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 1868*second, mjdStart+2155*second, 'No0008', obsCode, stnCode )
if array.time() < mjdStart + (2155-10)*second:
  subarray.execute(mjdStart + 2150*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2155*second) + ' since array.time is ' + str(array.time())

# Scan 8 = No0009
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2204*second, mjdStart+2394*second, 'No0009', obsCode, stnCode )
if array.time() < mjdStart + (2394-10)*second:
  subarray.execute(mjdStart + 2389*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2394*second) + ' since array.time is ' + str(array.time())

# Scan 9 = No0010
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2498*second, mjdStart+2693*second, 'No0010', obsCode, stnCode )
if array.time() < mjdStart + (2693-10)*second:
  subarray.execute(mjdStart + 2688*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2693*second) + ' since array.time is ' + str(array.time())

# Scan 10 = No0011
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2902*second, mjdStart+2932*second, 'No0011', obsCode, stnCode )
if array.time() < mjdStart + (2932-10)*second:
  subarray.execute(mjdStart + 2927*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2932*second) + ' since array.time is ' + str(array.time())

# Scan 11 = No0012
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 2971*second, mjdStart+3172*second, 'No0012', obsCode, stnCode )
if array.time() < mjdStart + (3172-10)*second:
  subarray.execute(mjdStart + 3167*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3172*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0013
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3194*second, mjdStart+3411*second, 'No0013', obsCode, stnCode )
if array.time() < mjdStart + (3411-10)*second:
  subarray.execute(mjdStart + 3406*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3411*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0014
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3499*second, mjdStart+3650*second, 'No0014', obsCode, stnCode )
if array.time() < mjdStart + (3650-10)*second:
  subarray.execute(mjdStart + 3645*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3650*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0015
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3692*second, mjdStart+3890*second, 'No0015', obsCode, stnCode )
if array.time() < mjdStart + (3890-10)*second:
  subarray.execute(mjdStart + 3885*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3890*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0016
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 3920*second, mjdStart+4129*second, 'No0016', obsCode, stnCode )
if array.time() < mjdStart + (4129-10)*second:
  subarray.execute(mjdStart + 4124*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4129*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0017
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4161*second, mjdStart+4369*second, 'No0017', obsCode, stnCode )
if array.time() < mjdStart + (4369-10)*second:
  subarray.execute(mjdStart + 4364*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4369*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0018
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4422*second, mjdStart+4608*second, 'No0018', obsCode, stnCode )
if array.time() < mjdStart + (4608-10)*second:
  subarray.execute(mjdStart + 4603*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4608*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0019
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4621*second, mjdStart+4787*second, 'No0019', obsCode, stnCode )
if array.time() < mjdStart + (4787-10)*second:
  subarray.execute(mjdStart + 4782*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4787*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0020
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 4918*second, mjdStart+5146*second, 'No0020', obsCode, stnCode )
if array.time() < mjdStart + (5146-10)*second:
  subarray.execute(mjdStart + 5141*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5146*second) + ' since array.time is ' + str(array.time())

# Scan 20 = No0021
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5363*second, mjdStart+5386*second, 'No0021', obsCode, stnCode )
if array.time() < mjdStart + (5386-10)*second:
  subarray.execute(mjdStart + 5381*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5386*second) + ' since array.time is ' + str(array.time())

# Scan 21 = No0022
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5423*second, mjdStart+5625*second, 'No0022', obsCode, stnCode )
if array.time() < mjdStart + (5625-10)*second:
  subarray.execute(mjdStart + 5620*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5625*second) + ' since array.time is ' + str(array.time())

# Scan 22 = No0023
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5647*second, mjdStart+5864*second, 'No0023', obsCode, stnCode )
if array.time() < mjdStart + (5864-10)*second:
  subarray.execute(mjdStart + 5859*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5864*second) + ' since array.time is ' + str(array.time())

# Scan 23 = No0024
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 5953*second, mjdStart+6104*second, 'No0024', obsCode, stnCode )
if array.time() < mjdStart + (6104-10)*second:
  subarray.execute(mjdStart + 6099*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6104*second) + ' since array.time is ' + str(array.time())

# Scan 24 = No0025
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6157*second, mjdStart+6343*second, 'No0025', obsCode, stnCode )
if array.time() < mjdStart + (6343-10)*second:
  subarray.execute(mjdStart + 6338*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6343*second) + ' since array.time is ' + str(array.time())

# Scan 25 = No0026
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6372*second, mjdStart+6582*second, 'No0026', obsCode, stnCode )
if array.time() < mjdStart + (6582-10)*second:
  subarray.execute(mjdStart + 6577*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6582*second) + ' since array.time is ' + str(array.time())

# Scan 26 = No0027
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6614*second, mjdStart+6822*second, 'No0027', obsCode, stnCode )
if array.time() < mjdStart + (6822-10)*second:
  subarray.execute(mjdStart + 6817*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6822*second) + ' since array.time is ' + str(array.time())

# Scan 27 = No0028
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 6876*second, mjdStart+7061*second, 'No0028', obsCode, stnCode )
if array.time() < mjdStart + (7061-10)*second:
  subarray.execute(mjdStart + 7056*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7061*second) + ' since array.time is ' + str(array.time())

# Scan 28 = No0029
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7075*second, mjdStart+7300*second, 'No0029', obsCode, stnCode )
if array.time() < mjdStart + (7300-10)*second:
  subarray.execute(mjdStart + 7295*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7300*second) + ' since array.time is ' + str(array.time())

# Scan 29 = No0030
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7321*second, mjdStart+7540*second, 'No0030', obsCode, stnCode )
if array.time() < mjdStart + (7540-10)*second:
  subarray.execute(mjdStart + 7535*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7540*second) + ' since array.time is ' + str(array.time())

# Scan 30 = No0031
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 7663*second, mjdStart+7839*second, 'No0031', obsCode, stnCode )
if array.time() < mjdStart + (7839-10)*second:
  subarray.execute(mjdStart + 7834*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7839*second) + ' since array.time is ' + str(array.time())

# Scan 31 = No0032
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8062*second, mjdStart+8078*second, 'No0032', obsCode, stnCode )
if array.time() < mjdStart + (8078-10)*second:
  subarray.execute(mjdStart + 8073*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8078*second) + ' since array.time is ' + str(array.time())

# Scan 32 = No0033
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8113*second, mjdStart+8318*second, 'No0033', obsCode, stnCode )
if array.time() < mjdStart + (8318-10)*second:
  subarray.execute(mjdStart + 8313*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8318*second) + ' since array.time is ' + str(array.time())

# Scan 33 = No0034
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8340*second, mjdStart+8557*second, 'No0034', obsCode, stnCode )
if array.time() < mjdStart + (8557-10)*second:
  subarray.execute(mjdStart + 8552*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8557*second) + ' since array.time is ' + str(array.time())

# Scan 34 = No0035
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8641*second, mjdStart+8796*second, 'No0035', obsCode, stnCode )
if array.time() < mjdStart + (8796-10)*second:
  subarray.execute(mjdStart + 8791*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8796*second) + ' since array.time is ' + str(array.time())

# Scan 35 = No0036
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 8875*second, mjdStart+9036*second, 'No0036', obsCode, stnCode )
if array.time() < mjdStart + (9036-10)*second:
  subarray.execute(mjdStart + 9031*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9036*second) + ' since array.time is ' + str(array.time())

# Scan 36 = No0037
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9063*second, mjdStart+9275*second, 'No0037', obsCode, stnCode )
if array.time() < mjdStart + (9275-10)*second:
  subarray.execute(mjdStart + 9270*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9275*second) + ' since array.time is ' + str(array.time())

# Scan 37 = No0038
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9306*second, mjdStart+9514*second, 'No0038', obsCode, stnCode )
if array.time() < mjdStart + (9514-10)*second:
  subarray.execute(mjdStart + 9509*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9514*second) + ' since array.time is ' + str(array.time())

# Scan 38 = No0039
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9571*second, mjdStart+9754*second, 'No0039', obsCode, stnCode )
if array.time() < mjdStart + (9754-10)*second:
  subarray.execute(mjdStart + 9749*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9754*second) + ' since array.time is ' + str(array.time())

# Scan 39 = No0040
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 9770*second, mjdStart+9993*second, 'No0040', obsCode, stnCode )
if array.time() < mjdStart + (9993-10)*second:
  subarray.execute(mjdStart + 9988*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9993*second) + ' since array.time is ' + str(array.time())

# Scan 40 = No0041
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10016*second, mjdStart+10232*second, 'No0041', obsCode, stnCode )
if array.time() < mjdStart + (10232-10)*second:
  subarray.execute(mjdStart + 10227*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10232*second) + ' since array.time is ' + str(array.time())

# Scan 41 = No0042
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10297*second, mjdStart+10532*second, 'No0042', obsCode, stnCode )
if array.time() < mjdStart + (10532-10)*second:
  subarray.execute(mjdStart + 10527*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10532*second) + ' since array.time is ' + str(array.time())

# Scan 42 = No0043
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10584*second, mjdStart+10771*second, 'No0043', obsCode, stnCode )
if array.time() < mjdStart + (10771-10)*second:
  subarray.execute(mjdStart + 10766*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10771*second) + ' since array.time is ' + str(array.time())

# Scan 43 = No0044
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10792*second, mjdStart+10951*second, 'No0044', obsCode, stnCode )
if array.time() < mjdStart + (10951-10)*second:
  subarray.execute(mjdStart + 10946*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10951*second) + ' since array.time is ' + str(array.time())

# Scan 44 = No0045
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 10973*second, mjdStart+11190*second, 'No0045', obsCode, stnCode )
if array.time() < mjdStart + (11190-10)*second:
  subarray.execute(mjdStart + 11185*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11190*second) + ' since array.time is ' + str(array.time())

# Scan 45 = No0046
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11209*second, mjdStart+11429*second, 'No0046', obsCode, stnCode )
if array.time() < mjdStart + (11429-10)*second:
  subarray.execute(mjdStart + 11424*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11429*second) + ' since array.time is ' + str(array.time())

# Scan 46 = No0047
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11577*second, mjdStart+11728*second, 'No0047', obsCode, stnCode )
if array.time() < mjdStart + (11728-10)*second:
  subarray.execute(mjdStart + 11723*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11728*second) + ' since array.time is ' + str(array.time())

# Scan 47 = No0048
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 11849*second, mjdStart+11968*second, 'No0048', obsCode, stnCode )
if array.time() < mjdStart + (11968-10)*second:
  subarray.execute(mjdStart + 11963*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11968*second) + ' since array.time is ' + str(array.time())

# Scan 48 = No0049
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12035*second, mjdStart+12147*second, 'No0049', obsCode, stnCode )
if array.time() < mjdStart + (12147-10)*second:
  subarray.execute(mjdStart + 12142*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12147*second) + ' since array.time is ' + str(array.time())

# Scan 49 = No0050
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12177*second, mjdStart+12327*second, 'No0050', obsCode, stnCode )
if array.time() < mjdStart + (12327-10)*second:
  subarray.execute(mjdStart + 12322*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12327*second) + ' since array.time is ' + str(array.time())

# Scan 50 = No0051
# Antenna OV not in scan No0051

# Scan 51 = No0052
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12550*second, mjdStart+12746*second, 'No0052', obsCode, stnCode )
if array.time() < mjdStart + (12746-10)*second:
  subarray.execute(mjdStart + 12741*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12746*second) + ' since array.time is ' + str(array.time())

# Scan 52 = No0053
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 12771*second, mjdStart+12985*second, 'No0053', obsCode, stnCode )
if array.time() < mjdStart + (12985-10)*second:
  subarray.execute(mjdStart + 12980*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12985*second) + ' since array.time is ' + str(array.time())

# Scan 53 = No0054
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13057*second, mjdStart+13284*second, 'No0054', obsCode, stnCode )
if array.time() < mjdStart + (13284-10)*second:
  subarray.execute(mjdStart + 13279*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13284*second) + ' since array.time is ' + str(array.time())

# Scan 54 = No0055
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13351*second, mjdStart+13583*second, 'No0055', obsCode, stnCode )
if array.time() < mjdStart + (13583-10)*second:
  subarray.execute(mjdStart + 13578*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13583*second) + ' since array.time is ' + str(array.time())

# Scan 55 = No0056
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13823*second, mjdStart+13823*second, 'No0056', obsCode, stnCode )
if array.time() < mjdStart + (13823-10)*second:
  subarray.execute(mjdStart + 13818*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13823*second) + ' since array.time is ' + str(array.time())

# Scan 56 = No0057
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 13910*second, mjdStart+14062*second, 'No0057', obsCode, stnCode )
if array.time() < mjdStart + (14062-10)*second:
  subarray.execute(mjdStart + 14057*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14062*second) + ' since array.time is ' + str(array.time())

# Scan 57 = No0058
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14142*second, mjdStart+14301*second, 'No0058', obsCode, stnCode )
if array.time() < mjdStart + (14301-10)*second:
  subarray.execute(mjdStart + 14296*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14301*second) + ' since array.time is ' + str(array.time())

# Scan 58 = No0059
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14352*second, mjdStart+14541*second, 'No0059', obsCode, stnCode )
if array.time() < mjdStart + (14541-10)*second:
  subarray.execute(mjdStart + 14536*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14541*second) + ' since array.time is ' + str(array.time())

# Scan 59 = No0060
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14610*second, mjdStart+14780*second, 'No0060', obsCode, stnCode )
if array.time() < mjdStart + (14780-10)*second:
  subarray.execute(mjdStart + 14775*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14780*second) + ' since array.time is ' + str(array.time())

# Scan 60 = No0061
# Antenna OV not in scan No0061

# Scan 61 = No0062
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 14994*second, mjdStart+15199*second, 'No0062', obsCode, stnCode )
if array.time() < mjdStart + (15199-10)*second:
  subarray.execute(mjdStart + 15194*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15199*second) + ' since array.time is ' + str(array.time())

# Scan 62 = No0063
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15227*second, mjdStart+15438*second, 'No0063', obsCode, stnCode )
if array.time() < mjdStart + (15438-10)*second:
  subarray.execute(mjdStart + 15433*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15438*second) + ' since array.time is ' + str(array.time())

# Scan 63 = No0064
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15490*second, mjdStart+15678*second, 'No0064', obsCode, stnCode )
if array.time() < mjdStart + (15678-10)*second:
  subarray.execute(mjdStart + 15673*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15678*second) + ' since array.time is ' + str(array.time())

# Scan 64 = No0065
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15701*second, mjdStart+15917*second, 'No0065', obsCode, stnCode )
if array.time() < mjdStart + (15917-10)*second:
  subarray.execute(mjdStart + 15912*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15917*second) + ' since array.time is ' + str(array.time())

# Scan 65 = No0066
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 15948*second, mjdStart+16096*second, 'No0066', obsCode, stnCode )
if array.time() < mjdStart + (16096-10)*second:
  subarray.execute(mjdStart + 16091*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16096*second) + ' since array.time is ' + str(array.time())

# Scan 66 = No0067
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16116*second, mjdStart+16336*second, 'No0067', obsCode, stnCode )
if array.time() < mjdStart + (16336-10)*second:
  subarray.execute(mjdStart + 16331*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16336*second) + ' since array.time is ' + str(array.time())

# Scan 67 = No0068
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16381*second, mjdStart+16575*second, 'No0068', obsCode, stnCode )
if array.time() < mjdStart + (16575-10)*second:
  subarray.execute(mjdStart + 16570*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16575*second) + ' since array.time is ' + str(array.time())

# Scan 68 = No0069
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16722*second, mjdStart+16934*second, 'No0069', obsCode, stnCode )
if array.time() < mjdStart + (16934-10)*second:
  subarray.execute(mjdStart + 16929*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16934*second) + ' since array.time is ' + str(array.time())

# Scan 69 = No0070
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 16990*second, mjdStart+17114*second, 'No0070', obsCode, stnCode )
if array.time() < mjdStart + (17114-10)*second:
  subarray.execute(mjdStart + 17109*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17114*second) + ' since array.time is ' + str(array.time())

# Scan 70 = No0071
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17144*second, mjdStart+17353*second, 'No0071', obsCode, stnCode )
if array.time() < mjdStart + (17353-10)*second:
  subarray.execute(mjdStart + 17348*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17353*second) + ' since array.time is ' + str(array.time())

# Scan 71 = No0072
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17449*second, mjdStart+17592*second, 'No0072', obsCode, stnCode )
if array.time() < mjdStart + (17592-10)*second:
  subarray.execute(mjdStart + 17587*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17592*second) + ' since array.time is ' + str(array.time())

# Scan 72 = No0073
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17768*second, mjdStart+17832*second, 'No0073', obsCode, stnCode )
if array.time() < mjdStart + (17832-10)*second:
  subarray.execute(mjdStart + 17827*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17832*second) + ' since array.time is ' + str(array.time())

# Scan 73 = No0074
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 17854*second, mjdStart+18011*second, 'No0074', obsCode, stnCode )
if array.time() < mjdStart + (18011-10)*second:
  subarray.execute(mjdStart + 18006*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18011*second) + ' since array.time is ' + str(array.time())

# Scan 74 = No0075
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18042*second, mjdStart+18191*second, 'No0075', obsCode, stnCode )
if array.time() < mjdStart + (18191-10)*second:
  subarray.execute(mjdStart + 18186*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18191*second) + ' since array.time is ' + str(array.time())

# Scan 75 = No0076
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18286*second, mjdStart+18490*second, 'No0076', obsCode, stnCode )
if array.time() < mjdStart + (18490-10)*second:
  subarray.execute(mjdStart + 18485*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18490*second) + ' since array.time is ' + str(array.time())

# Scan 76 = No0077
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18556*second, mjdStart+18789*second, 'No0077', obsCode, stnCode )
if array.time() < mjdStart + (18789-10)*second:
  subarray.execute(mjdStart + 18784*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18789*second) + ' since array.time is ' + str(array.time())

# Scan 77 = No0078
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 18811*second, mjdStart+19028*second, 'No0078', obsCode, stnCode )
if array.time() < mjdStart + (19028-10)*second:
  subarray.execute(mjdStart + 19023*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19028*second) + ' since array.time is ' + str(array.time())

# Scan 78 = No0079
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19060*second, mjdStart+19268*second, 'No0079', obsCode, stnCode )
if array.time() < mjdStart + (19268-10)*second:
  subarray.execute(mjdStart + 19263*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19268*second) + ' since array.time is ' + str(array.time())

# Scan 79 = No0080
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19289*second, mjdStart+19507*second, 'No0080', obsCode, stnCode )
if array.time() < mjdStart + (19507-10)*second:
  subarray.execute(mjdStart + 19502*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19507*second) + ' since array.time is ' + str(array.time())

# Scan 80 = No0081
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19562*second, mjdStart+19746*second, 'No0081', obsCode, stnCode )
if array.time() < mjdStart + (19746-10)*second:
  subarray.execute(mjdStart + 19741*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19746*second) + ' since array.time is ' + str(array.time())

# Scan 81 = No0082
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 19768*second, mjdStart+19986*second, 'No0082', obsCode, stnCode )
if array.time() < mjdStart + (19986-10)*second:
  subarray.execute(mjdStart + 19981*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19986*second) + ' since array.time is ' + str(array.time())

# Scan 82 = No0083
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20146*second, mjdStart+20285*second, 'No0083', obsCode, stnCode )
if array.time() < mjdStart + (20285-10)*second:
  subarray.execute(mjdStart + 20280*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20285*second) + ' since array.time is ' + str(array.time())

# Scan 83 = No0084
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20327*second, mjdStart+20524*second, 'No0084', obsCode, stnCode )
if array.time() < mjdStart + (20524-10)*second:
  subarray.execute(mjdStart + 20519*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20524*second) + ' since array.time is ' + str(array.time())

# Scan 84 = No0085
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20554*second, mjdStart+20704*second, 'No0085', obsCode, stnCode )
if array.time() < mjdStart + (20704-10)*second:
  subarray.execute(mjdStart + 20699*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20704*second) + ' since array.time is ' + str(array.time())

# Scan 85 = No0086
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20736*second, mjdStart+20883*second, 'No0086', obsCode, stnCode )
if array.time() < mjdStart + (20883-10)*second:
  subarray.execute(mjdStart + 20878*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20883*second) + ' since array.time is ' + str(array.time())

# Scan 86 = No0087
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 20954*second, mjdStart+21123*second, 'No0087', obsCode, stnCode )
if array.time() < mjdStart + (21123-10)*second:
  subarray.execute(mjdStart + 21118*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21123*second) + ' since array.time is ' + str(array.time())

# Scan 87 = No0088
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21146*second, mjdStart+21362*second, 'No0088', obsCode, stnCode )
if array.time() < mjdStart + (21362-10)*second:
  subarray.execute(mjdStart + 21357*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21362*second) + ' since array.time is ' + str(array.time())

# Scan 88 = No0089
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21390*second, mjdStart+21601*second, 'No0089', obsCode, stnCode )
if array.time() < mjdStart + (21601-10)*second:
  subarray.execute(mjdStart + 21596*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21601*second) + ' since array.time is ' + str(array.time())

# Scan 89 = No0090
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 21721*second, mjdStart+22200*second, 'No0090', obsCode, stnCode )
if array.time() < mjdStart + (22200-10)*second:
  subarray.execute(mjdStart + 22195*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22200*second) + ' since array.time is ' + str(array.time())

# Scan 90 = No0091
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22280*second, mjdStart+22499*second, 'No0091', obsCode, stnCode )
if array.time() < mjdStart + (22499-10)*second:
  subarray.execute(mjdStart + 22494*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22499*second) + ' since array.time is ' + str(array.time())

# Scan 91 = No0092
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22522*second, mjdStart+22738*second, 'No0092', obsCode, stnCode )
if array.time() < mjdStart + (22738-10)*second:
  subarray.execute(mjdStart + 22733*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22738*second) + ' since array.time is ' + str(array.time())

# Scan 92 = No0093
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 22772*second, mjdStart+22978*second, 'No0093', obsCode, stnCode )
if array.time() < mjdStart + (22978-10)*second:
  subarray.execute(mjdStart + 22973*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22978*second) + ' since array.time is ' + str(array.time())

# Scan 93 = No0094
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23001*second, mjdStart+23217*second, 'No0094', obsCode, stnCode )
if array.time() < mjdStart + (23217-10)*second:
  subarray.execute(mjdStart + 23212*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23217*second) + ' since array.time is ' + str(array.time())

# Scan 94 = No0095
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23281*second, mjdStart+23516*second, 'No0095', obsCode, stnCode )
if array.time() < mjdStart + (23516-10)*second:
  subarray.execute(mjdStart + 23511*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23516*second) + ' since array.time is ' + str(array.time())

# Scan 95 = No0096
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23534*second, mjdStart+23815*second, 'No0096', obsCode, stnCode )
if array.time() < mjdStart + (23815-10)*second:
  subarray.execute(mjdStart + 23810*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23815*second) + ' since array.time is ' + str(array.time())

# Scan 96 = No0097
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 23891*second, mjdStart+24114*second, 'No0097', obsCode, stnCode )
if array.time() < mjdStart + (24114-10)*second:
  subarray.execute(mjdStart + 24109*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24114*second) + ' since array.time is ' + str(array.time())

# Scan 97 = No0098
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24145*second, mjdStart+24414*second, 'No0098', obsCode, stnCode )
if array.time() < mjdStart + (24414-10)*second:
  subarray.execute(mjdStart + 24409*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24414*second) + ' since array.time is ' + str(array.time())

# Scan 98 = No0099
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24452*second, mjdStart+24713*second, 'No0099', obsCode, stnCode )
if array.time() < mjdStart + (24713-10)*second:
  subarray.execute(mjdStart + 24708*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24713*second) + ' since array.time is ' + str(array.time())

# Scan 99 = No0100
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 24770*second, mjdStart+25012*second, 'No0100', obsCode, stnCode )
if array.time() < mjdStart + (25012-10)*second:
  subarray.execute(mjdStart + 25007*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25012*second) + ' since array.time is ' + str(array.time())

# Scan 100 = No0101
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25135*second, mjdStart+25371*second, 'No0101', obsCode, stnCode )
if array.time() < mjdStart + (25371-10)*second:
  subarray.execute(mjdStart + 25366*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25371*second) + ' since array.time is ' + str(array.time())

# Scan 101 = No0102
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25595*second, mjdStart+25610*second, 'No0102', obsCode, stnCode )
if array.time() < mjdStart + (25610-10)*second:
  subarray.execute(mjdStart + 25605*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25610*second) + ' since array.time is ' + str(array.time())

# Scan 102 = No0103
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25648*second, mjdStart+25850*second, 'No0103', obsCode, stnCode )
if array.time() < mjdStart + (25850-10)*second:
  subarray.execute(mjdStart + 25845*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+25850*second) + ' since array.time is ' + str(array.time())

# Scan 103 = No0104
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 25967*second, mjdStart+26089*second, 'No0104', obsCode, stnCode )
if array.time() < mjdStart + (26089-10)*second:
  subarray.execute(mjdStart + 26084*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26089*second) + ' since array.time is ' + str(array.time())

# Scan 104 = No0105
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26257*second, mjdStart+26328*second, 'No0105', obsCode, stnCode )
if array.time() < mjdStart + (26328-10)*second:
  subarray.execute(mjdStart + 26323*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26328*second) + ' since array.time is ' + str(array.time())

# Scan 105 = No0106
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26374*second, mjdStart+26568*second, 'No0106', obsCode, stnCode )
if array.time() < mjdStart + (26568-10)*second:
  subarray.execute(mjdStart + 26563*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26568*second) + ' since array.time is ' + str(array.time())

# Scan 106 = No0107
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26607*second, mjdStart+26807*second, 'No0107', obsCode, stnCode )
if array.time() < mjdStart + (26807-10)*second:
  subarray.execute(mjdStart + 26802*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+26807*second) + ' since array.time is ' + str(array.time())

# Scan 107 = No0108
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 26829*second, mjdStart+27046*second, 'No0108', obsCode, stnCode )
if array.time() < mjdStart + (27046-10)*second:
  subarray.execute(mjdStart + 27041*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27046*second) + ' since array.time is ' + str(array.time())

# Scan 108 = No0109
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27124*second, mjdStart+27286*second, 'No0109', obsCode, stnCode )
if array.time() < mjdStart + (27286-10)*second:
  subarray.execute(mjdStart + 27281*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27286*second) + ' since array.time is ' + str(array.time())

# Scan 109 = No0110
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27304*second, mjdStart+27585*second, 'No0110', obsCode, stnCode )
if array.time() < mjdStart + (27585-10)*second:
  subarray.execute(mjdStart + 27580*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27585*second) + ' since array.time is ' + str(array.time())

# Scan 110 = No0111
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 27650*second, mjdStart+27884*second, 'No0111', obsCode, stnCode )
if array.time() < mjdStart + (27884-10)*second:
  subarray.execute(mjdStart + 27879*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+27884*second) + ' since array.time is ' + str(array.time())

# Scan 111 = No0112
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28016*second, mjdStart+28183*second, 'No0112', obsCode, stnCode )
if array.time() < mjdStart + (28183-10)*second:
  subarray.execute(mjdStart + 28178*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28183*second) + ' since array.time is ' + str(array.time())

# Scan 112 = No0113
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28224*second, mjdStart+28423*second, 'No0113', obsCode, stnCode )
if array.time() < mjdStart + (28423-10)*second:
  subarray.execute(mjdStart + 28418*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28423*second) + ' since array.time is ' + str(array.time())

# Scan 113 = No0114
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28463*second, mjdStart+28662*second, 'No0114', obsCode, stnCode )
if array.time() < mjdStart + (28662-10)*second:
  subarray.execute(mjdStart + 28657*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28662*second) + ' since array.time is ' + str(array.time())

# Scan 114 = No0115
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 28761*second, mjdStart+28901*second, 'No0115', obsCode, stnCode )
if array.time() < mjdStart + (28901-10)*second:
  subarray.execute(mjdStart + 28896*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+28901*second) + ' since array.time is ' + str(array.time())

# Scan 115 = No0116
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29044*second, mjdStart+29141*second, 'No0116', obsCode, stnCode )
if array.time() < mjdStart + (29141-10)*second:
  subarray.execute(mjdStart + 29136*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29141*second) + ' since array.time is ' + str(array.time())

# Scan 116 = No0117
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29179*second, mjdStart+29380*second, 'No0117', obsCode, stnCode )
if array.time() < mjdStart + (29380-10)*second:
  subarray.execute(mjdStart + 29375*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29380*second) + ' since array.time is ' + str(array.time())

# Scan 117 = No0118
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29424*second, mjdStart+29619*second, 'No0118', obsCode, stnCode )
if array.time() < mjdStart + (29619-10)*second:
  subarray.execute(mjdStart + 29614*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29619*second) + ' since array.time is ' + str(array.time())

# Scan 118 = No0119
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29650*second, mjdStart+29859*second, 'No0119', obsCode, stnCode )
if array.time() < mjdStart + (29859-10)*second:
  subarray.execute(mjdStart + 29854*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+29859*second) + ' since array.time is ' + str(array.time())

# Scan 119 = No0120
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 29953*second, mjdStart+30098*second, 'No0120', obsCode, stnCode )
if array.time() < mjdStart + (30098-10)*second:
  subarray.execute(mjdStart + 30093*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30098*second) + ' since array.time is ' + str(array.time())

# Scan 120 = No0121
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30190*second, mjdStart+30397*second, 'No0121', obsCode, stnCode )
if array.time() < mjdStart + (30397-10)*second:
  subarray.execute(mjdStart + 30392*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30397*second) + ' since array.time is ' + str(array.time())

# Scan 121 = No0122
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30421*second, mjdStart+30696*second, 'No0122', obsCode, stnCode )
if array.time() < mjdStart + (30696-10)*second:
  subarray.execute(mjdStart + 30691*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30696*second) + ' since array.time is ' + str(array.time())

# Scan 122 = No0123
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 30739*second, mjdStart+30996*second, 'No0123', obsCode, stnCode )
if array.time() < mjdStart + (30996-10)*second:
  subarray.execute(mjdStart + 30991*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+30996*second) + ' since array.time is ' + str(array.time())

# Scan 123 = No0124
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31092*second, mjdStart+31295*second, 'No0124', obsCode, stnCode )
if array.time() < mjdStart + (31295-10)*second:
  subarray.execute(mjdStart + 31290*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31295*second) + ' since array.time is ' + str(array.time())

# Scan 124 = No0125
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31322*second, mjdStart+31594*second, 'No0125', obsCode, stnCode )
if array.time() < mjdStart + (31594-10)*second:
  subarray.execute(mjdStart + 31589*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31594*second) + ' since array.time is ' + str(array.time())

# Scan 125 = No0126
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31673*second, mjdStart+31893*second, 'No0126', obsCode, stnCode )
if array.time() < mjdStart + (31893-10)*second:
  subarray.execute(mjdStart + 31888*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+31893*second) + ' since array.time is ' + str(array.time())

# Scan 126 = No0127
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 31937*second, mjdStart+32192*second, 'No0127', obsCode, stnCode )
if array.time() < mjdStart + (32192-10)*second:
  subarray.execute(mjdStart + 32187*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32192*second) + ' since array.time is ' + str(array.time())

# Scan 127 = No0128
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32247*second, mjdStart+32492*second, 'No0128', obsCode, stnCode )
if array.time() < mjdStart + (32492-10)*second:
  subarray.execute(mjdStart + 32487*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32492*second) + ' since array.time is ' + str(array.time())

# Scan 128 = No0129
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32529*second, mjdStart+32791*second, 'No0129', obsCode, stnCode )
if array.time() < mjdStart + (32791-10)*second:
  subarray.execute(mjdStart + 32786*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+32791*second) + ' since array.time is ' + str(array.time())

# Scan 129 = No0130
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 32845*second, mjdStart+33090*second, 'No0130', obsCode, stnCode )
if array.time() < mjdStart + (33090-10)*second:
  subarray.execute(mjdStart + 33085*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33090*second) + ' since array.time is ' + str(array.time())

# Scan 130 = No0131
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33123*second, mjdStart+33389*second, 'No0131', obsCode, stnCode )
if array.time() < mjdStart + (33389-10)*second:
  subarray.execute(mjdStart + 33384*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33389*second) + ' since array.time is ' + str(array.time())

# Scan 131 = No0132
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33418*second, mjdStart+33628*second, 'No0132', obsCode, stnCode )
if array.time() < mjdStart + (33628-10)*second:
  subarray.execute(mjdStart + 33623*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33628*second) + ' since array.time is ' + str(array.time())

# Scan 132 = No0133
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 33688*second, mjdStart+33987*second, 'No0133', obsCode, stnCode )
if array.time() < mjdStart + (33987-10)*second:
  subarray.execute(mjdStart + 33982*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+33987*second) + ' since array.time is ' + str(array.time())

# Scan 133 = No0134
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34033*second, mjdStart+34287*second, 'No0134', obsCode, stnCode )
if array.time() < mjdStart + (34287-10)*second:
  subarray.execute(mjdStart + 34282*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34287*second) + ' since array.time is ' + str(array.time())

# Scan 134 = No0135
subarray.setSource(source0)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34409*second, mjdStart+34586*second, 'No0135', obsCode, stnCode )
if array.time() < mjdStart + (34586-10)*second:
  subarray.execute(mjdStart + 34581*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34586*second) + ' since array.time is ' + str(array.time())

# Scan 135 = No0136
subarray.setSource(source5)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34623*second, mjdStart+34825*second, 'No0136', obsCode, stnCode )
if array.time() < mjdStart + (34825-10)*second:
  subarray.execute(mjdStart + 34820*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+34825*second) + ' since array.time is ' + str(array.time())

# Scan 136 = No0137
subarray.setSource(source6)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 34867*second, mjdStart+35064*second, 'No0137', obsCode, stnCode )
if array.time() < mjdStart + (35064-10)*second:
  subarray.execute(mjdStart + 35059*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35064*second) + ' since array.time is ' + str(array.time())

# Scan 137 = No0138
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35125*second, mjdStart+35304*second, 'No0138', obsCode, stnCode )
if array.time() < mjdStart + (35304-10)*second:
  subarray.execute(mjdStart + 35299*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35304*second) + ' since array.time is ' + str(array.time())

# Scan 138 = No0139
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35338*second, mjdStart+35543*second, 'No0139', obsCode, stnCode )
if array.time() < mjdStart + (35543-10)*second:
  subarray.execute(mjdStart + 35538*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35543*second) + ' since array.time is ' + str(array.time())

# Scan 139 = No0140
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35567*second, mjdStart+35842*second, 'No0140', obsCode, stnCode )
if array.time() < mjdStart + (35842-10)*second:
  subarray.execute(mjdStart + 35837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+35842*second) + ' since array.time is ' + str(array.time())

# Scan 140 = No0141
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 35961*second, mjdStart+36082*second, 'No0141', obsCode, stnCode )
if array.time() < mjdStart + (36082-10)*second:
  subarray.execute(mjdStart + 36077*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+36082*second) + ' since array.time is ' + str(array.time())

# Scan 141 = No0142
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 36097*second, mjdStart+36381*second, 'No0142', obsCode, stnCode )
if array.time() < mjdStart + (36381-10)*second:
  subarray.execute(mjdStart + 36376*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+36381*second) + ' since array.time is ' + str(array.time())

# Scan 142 = No0143
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 36464*second, mjdStart+36680*second, 'No0143', obsCode, stnCode )
if array.time() < mjdStart + (36680-10)*second:
  subarray.execute(mjdStart + 36675*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+36680*second) + ' since array.time is ' + str(array.time())

# Scan 143 = No0144
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 36715*second, mjdStart+36919*second, 'No0144', obsCode, stnCode )
if array.time() < mjdStart + (36919-10)*second:
  subarray.execute(mjdStart + 36914*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+36919*second) + ' since array.time is ' + str(array.time())

# Scan 144 = No0145
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 36962*second, mjdStart+37159*second, 'No0145', obsCode, stnCode )
if array.time() < mjdStart + (37159-10)*second:
  subarray.execute(mjdStart + 37154*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+37159*second) + ' since array.time is ' + str(array.time())

# Scan 145 = No0146
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 37201*second, mjdStart+37398*second, 'No0146', obsCode, stnCode )
if array.time() < mjdStart + (37398-10)*second:
  subarray.execute(mjdStart + 37393*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+37398*second) + ' since array.time is ' + str(array.time())

# Scan 146 = No0147
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 37502*second, mjdStart+37697*second, 'No0147', obsCode, stnCode )
if array.time() < mjdStart + (37697-10)*second:
  subarray.execute(mjdStart + 37692*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+37697*second) + ' since array.time is ' + str(array.time())

# Scan 147 = No0148
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 37718*second, mjdStart+37996*second, 'No0148', obsCode, stnCode )
if array.time() < mjdStart + (37996-10)*second:
  subarray.execute(mjdStart + 37991*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+37996*second) + ' since array.time is ' + str(array.time())

# Scan 148 = No0149
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 38151*second, mjdStart+38296*second, 'No0149', obsCode, stnCode )
if array.time() < mjdStart + (38296-10)*second:
  subarray.execute(mjdStart + 38291*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+38296*second) + ' since array.time is ' + str(array.time())

# Scan 149 = No0150
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 38342*second, mjdStart+38595*second, 'No0150', obsCode, stnCode )
if array.time() < mjdStart + (38595-10)*second:
  subarray.execute(mjdStart + 38590*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+38595*second) + ' since array.time is ' + str(array.time())

# Scan 150 = No0151
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 38667*second, mjdStart+38894*second, 'No0151', obsCode, stnCode )
if array.time() < mjdStart + (38894-10)*second:
  subarray.execute(mjdStart + 38889*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+38894*second) + ' since array.time is ' + str(array.time())

# Scan 151 = No0152
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 38930*second, mjdStart+39193*second, 'No0152', obsCode, stnCode )
if array.time() < mjdStart + (39193-10)*second:
  subarray.execute(mjdStart + 39188*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+39193*second) + ' since array.time is ' + str(array.time())

# Scan 152 = No0153
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 39247*second, mjdStart+39492*second, 'No0153', obsCode, stnCode )
if array.time() < mjdStart + (39492-10)*second:
  subarray.execute(mjdStart + 39487*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+39492*second) + ' since array.time is ' + str(array.time())

# Scan 153 = No0154
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 39530*second, mjdStart+39792*second, 'No0154', obsCode, stnCode )
if array.time() < mjdStart + (39792-10)*second:
  subarray.execute(mjdStart + 39787*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+39792*second) + ' since array.time is ' + str(array.time())

# Scan 154 = No0155
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 39821*second, mjdStart+40091*second, 'No0155', obsCode, stnCode )
if array.time() < mjdStart + (40091-10)*second:
  subarray.execute(mjdStart + 40086*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+40091*second) + ' since array.time is ' + str(array.time())

# Scan 155 = No0156
subarray.setSource(source7)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 40208*second, mjdStart+40390*second, 'No0156', obsCode, stnCode )
if array.time() < mjdStart + (40390-10)*second:
  subarray.execute(mjdStart + 40385*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+40390*second) + ' since array.time is ' + str(array.time())

# Scan 156 = No0157
subarray.setSource(source8)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 40594*second, mjdStart+40689*second, 'No0157', obsCode, stnCode )
if array.time() < mjdStart + (40689-10)*second:
  subarray.execute(mjdStart + 40684*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+40689*second) + ' since array.time is ' + str(array.time())

# Scan 157 = No0158
subarray.setSource(source9)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 40726*second, mjdStart+40928*second, 'No0158', obsCode, stnCode )
if array.time() < mjdStart + (40928-10)*second:
  subarray.execute(mjdStart + 40923*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+40928*second) + ' since array.time is ' + str(array.time())

# Scan 158 = No0159
subarray.setSource(source10)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 40964*second, mjdStart+41168*second, 'No0159', obsCode, stnCode )
if array.time() < mjdStart + (41168-10)*second:
  subarray.execute(mjdStart + 41163*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+41168*second) + ' since array.time is ' + str(array.time())

# Scan 159 = No0160
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 41208*second, mjdStart+41407*second, 'No0160', obsCode, stnCode )
if array.time() < mjdStart + (41407-10)*second:
  subarray.execute(mjdStart + 41402*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+41407*second) + ' since array.time is ' + str(array.time())

# Scan 160 = No0161
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 41486*second, mjdStart+41706*second, 'No0161', obsCode, stnCode )
if array.time() < mjdStart + (41706-10)*second:
  subarray.execute(mjdStart + 41701*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+41706*second) + ' since array.time is ' + str(array.time())

# Scan 161 = No0162
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 41731*second, mjdStart+42005*second, 'No0162', obsCode, stnCode )
if array.time() < mjdStart + (42005-10)*second:
  subarray.execute(mjdStart + 42000*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+42005*second) + ' since array.time is ' + str(array.time())

# Scan 162 = No0163
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 42130*second, mjdStart+42305*second, 'No0163', obsCode, stnCode )
if array.time() < mjdStart + (42305-10)*second:
  subarray.execute(mjdStart + 42300*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+42305*second) + ' since array.time is ' + str(array.time())

# Scan 163 = No0164
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 42348*second, mjdStart+42604*second, 'No0164', obsCode, stnCode )
if array.time() < mjdStart + (42604-10)*second:
  subarray.execute(mjdStart + 42599*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+42604*second) + ' since array.time is ' + str(array.time())

# Scan 164 = No0165
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 42695*second, mjdStart+42903*second, 'No0165', obsCode, stnCode )
if array.time() < mjdStart + (42903-10)*second:
  subarray.execute(mjdStart + 42898*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+42903*second) + ' since array.time is ' + str(array.time())

# Scan 165 = No0166
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 43026*second, mjdStart+43202*second, 'No0166', obsCode, stnCode )
if array.time() < mjdStart + (43202-10)*second:
  subarray.execute(mjdStart + 43197*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+43202*second) + ' since array.time is ' + str(array.time())

# Scan 166 = No0167
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 43318*second, mjdStart+43501*second, 'No0167', obsCode, stnCode )
if array.time() < mjdStart + (43501-10)*second:
  subarray.execute(mjdStart + 43496*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+43501*second) + ' since array.time is ' + str(array.time())

# Scan 167 = No0168
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 43546*second, mjdStart+43801*second, 'No0168', obsCode, stnCode )
if array.time() < mjdStart + (43801-10)*second:
  subarray.execute(mjdStart + 43796*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+43801*second) + ' since array.time is ' + str(array.time())

# Scan 168 = No0169
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 43829*second, mjdStart+44100*second, 'No0169', obsCode, stnCode )
if array.time() < mjdStart + (44100-10)*second:
  subarray.execute(mjdStart + 44095*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+44100*second) + ' since array.time is ' + str(array.time())

# Scan 169 = No0170
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 44165*second, mjdStart+44399*second, 'No0170', obsCode, stnCode )
if array.time() < mjdStart + (44399-10)*second:
  subarray.execute(mjdStart + 44394*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+44399*second) + ' since array.time is ' + str(array.time())

# Scan 170 = No0171
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 44519*second, mjdStart+44698*second, 'No0171', obsCode, stnCode )
if array.time() < mjdStart + (44698-10)*second:
  subarray.execute(mjdStart + 44693*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+44698*second) + ' since array.time is ' + str(array.time())

# Scan 171 = No0172
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 44764*second, mjdStart+45057*second, 'No0172', obsCode, stnCode )
if array.time() < mjdStart + (45057-10)*second:
  subarray.execute(mjdStart + 45052*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+45057*second) + ' since array.time is ' + str(array.time())

# Scan 172 = No0173
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 45083*second, mjdStart+45356*second, 'No0173', obsCode, stnCode )
if array.time() < mjdStart + (45356-10)*second:
  subarray.execute(mjdStart + 45351*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+45356*second) + ' since array.time is ' + str(array.time())

# Scan 173 = No0174
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 45459*second, mjdStart+45655*second, 'No0174', obsCode, stnCode )
if array.time() < mjdStart + (45655-10)*second:
  subarray.execute(mjdStart + 45650*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+45655*second) + ' since array.time is ' + str(array.time())

# Scan 174 = No0175
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 45692*second, mjdStart+45955*second, 'No0175', obsCode, stnCode )
if array.time() < mjdStart + (45955-10)*second:
  subarray.execute(mjdStart + 45950*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+45955*second) + ' since array.time is ' + str(array.time())

# Scan 175 = No0176
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 46056*second, mjdStart+46254*second, 'No0176', obsCode, stnCode )
if array.time() < mjdStart + (46254-10)*second:
  subarray.execute(mjdStart + 46249*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+46254*second) + ' since array.time is ' + str(array.time())

# Scan 176 = No0177
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 46319*second, mjdStart+46553*second, 'No0177', obsCode, stnCode )
if array.time() < mjdStart + (46553-10)*second:
  subarray.execute(mjdStart + 46548*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+46553*second) + ' since array.time is ' + str(array.time())

# Scan 177 = No0178
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 46635*second, mjdStart+46852*second, 'No0178', obsCode, stnCode )
if array.time() < mjdStart + (46852-10)*second:
  subarray.execute(mjdStart + 46847*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+46852*second) + ' since array.time is ' + str(array.time())

# Scan 178 = No0179
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 46900*second, mjdStart+47151*second, 'No0179', obsCode, stnCode )
if array.time() < mjdStart + (47151-10)*second:
  subarray.execute(mjdStart + 47146*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+47151*second) + ' since array.time is ' + str(array.time())

# Scan 179 = No0180
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 47176*second, mjdStart+47451*second, 'No0180', obsCode, stnCode )
if array.time() < mjdStart + (47451-10)*second:
  subarray.execute(mjdStart + 47446*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+47451*second) + ' since array.time is ' + str(array.time())

# Scan 180 = No0181
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 47531*second, mjdStart+47750*second, 'No0181', obsCode, stnCode )
if array.time() < mjdStart + (47750-10)*second:
  subarray.execute(mjdStart + 47745*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+47750*second) + ' since array.time is ' + str(array.time())

# Scan 181 = No0182
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 47863*second, mjdStart+48049*second, 'No0182', obsCode, stnCode )
if array.time() < mjdStart + (48049-10)*second:
  subarray.execute(mjdStart + 48044*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+48049*second) + ' since array.time is ' + str(array.time())

# Scan 182 = No0183
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 48097*second, mjdStart+48348*second, 'No0183', obsCode, stnCode )
if array.time() < mjdStart + (48348-10)*second:
  subarray.execute(mjdStart + 48343*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+48348*second) + ' since array.time is ' + str(array.time())

# Scan 183 = No0184
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 48362*second, mjdStart+48647*second, 'No0184', obsCode, stnCode )
if array.time() < mjdStart + (48647-10)*second:
  subarray.execute(mjdStart + 48642*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+48647*second) + ' since array.time is ' + str(array.time())

# Scan 184 = No0185
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 48657*second, mjdStart+48946*second, 'No0185', obsCode, stnCode )
if array.time() < mjdStart + (48946-10)*second:
  subarray.execute(mjdStart + 48941*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+48946*second) + ' since array.time is ' + str(array.time())

# Scan 185 = No0186
subarray.setSource(source12)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 49097*second, mjdStart+49246*second, 'No0186', obsCode, stnCode )
if array.time() < mjdStart + (49246-10)*second:
  subarray.execute(mjdStart + 49241*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+49246*second) + ' since array.time is ' + str(array.time())

# Scan 186 = No0187
subarray.setSource(source11)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 49301*second, mjdStart+49545*second, 'No0187', obsCode, stnCode )
if array.time() < mjdStart + (49545-10)*second:
  subarray.execute(mjdStart + 49540*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+49545*second) + ' since array.time is ' + str(array.time())

# Scan 187 = No0188
subarray.setSource(source13)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 49570*second, mjdStart+49844*second, 'No0188', obsCode, stnCode )
if array.time() < mjdStart + (49844-10)*second:
  subarray.execute(mjdStart + 49839*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+49844*second) + ' since array.time is ' + str(array.time())

# Scan 188 = No0189
subarray.setSource(source14)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 49924*second, mjdStart+50143*second, 'No0189', obsCode, stnCode )
if array.time() < mjdStart + (50143-10)*second:
  subarray.execute(mjdStart + 50138*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+50143*second) + ' since array.time is ' + str(array.time())

# Scan 189 = No0190
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 50172*second, mjdStart+50383*second, 'No0190', obsCode, stnCode )
if array.time() < mjdStart + (50383-10)*second:
  subarray.execute(mjdStart + 50378*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+50383*second) + ' since array.time is ' + str(array.time())

# Scan 190 = No0191
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 50473*second, mjdStart+50682*second, 'No0191', obsCode, stnCode )
if array.time() < mjdStart + (50682-10)*second:
  subarray.execute(mjdStart + 50677*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+50682*second) + ' since array.time is ' + str(array.time())

# Scan 191 = No0192
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 50712*second, mjdStart+50921*second, 'No0192', obsCode, stnCode )
if array.time() < mjdStart + (50921-10)*second:
  subarray.execute(mjdStart + 50916*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+50921*second) + ' since array.time is ' + str(array.time())

# Scan 192 = No0193
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 50969*second, mjdStart+51160*second, 'No0193', obsCode, stnCode )
if array.time() < mjdStart + (51160-10)*second:
  subarray.execute(mjdStart + 51155*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+51160*second) + ' since array.time is ' + str(array.time())

# Scan 193 = No0194
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 51202*second, mjdStart+51400*second, 'No0194', obsCode, stnCode )
if array.time() < mjdStart + (51400-10)*second:
  subarray.execute(mjdStart + 51395*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+51400*second) + ' since array.time is ' + str(array.time())

# Scan 194 = No0195
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 51420*second, mjdStart+51639*second, 'No0195', obsCode, stnCode )
if array.time() < mjdStart + (51639-10)*second:
  subarray.execute(mjdStart + 51634*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+51639*second) + ' since array.time is ' + str(array.time())

# Scan 195 = No0196
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 51722*second, mjdStart+51938*second, 'No0196', obsCode, stnCode )
if array.time() < mjdStart + (51938-10)*second:
  subarray.execute(mjdStart + 51933*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+51938*second) + ' since array.time is ' + str(array.time())

# Scan 196 = No0197
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 52033*second, mjdStart+52237*second, 'No0197', obsCode, stnCode )
if array.time() < mjdStart + (52237-10)*second:
  subarray.execute(mjdStart + 52232*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+52237*second) + ' since array.time is ' + str(array.time())

# Scan 197 = No0198
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 52293*second, mjdStart+52537*second, 'No0198', obsCode, stnCode )
if array.time() < mjdStart + (52537-10)*second:
  subarray.execute(mjdStart + 52532*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+52537*second) + ' since array.time is ' + str(array.time())

# Scan 198 = No0199
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 52552*second, mjdStart+52836*second, 'No0199', obsCode, stnCode )
if array.time() < mjdStart + (52836-10)*second:
  subarray.execute(mjdStart + 52831*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+52836*second) + ' since array.time is ' + str(array.time())

# Scan 199 = No0200
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 52847*second, mjdStart+53135*second, 'No0200', obsCode, stnCode )
if array.time() < mjdStart + (53135-10)*second:
  subarray.execute(mjdStart + 53130*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+53135*second) + ' since array.time is ' + str(array.time())

# Scan 200 = No0201
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 53199*second, mjdStart+53434*second, 'No0201', obsCode, stnCode )
if array.time() < mjdStart + (53434-10)*second:
  subarray.execute(mjdStart + 53429*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+53434*second) + ' since array.time is ' + str(array.time())

# Scan 201 = No0202
subarray.setSource(source16)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 53527*second, mjdStart+53793*second, 'No0202', obsCode, stnCode )
if array.time() < mjdStart + (53793-10)*second:
  subarray.execute(mjdStart + 53788*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+53793*second) + ' since array.time is ' + str(array.time())

# Scan 202 = No0203
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 53875*second, mjdStart+54092*second, 'No0203', obsCode, stnCode )
if array.time() < mjdStart + (54092-10)*second:
  subarray.execute(mjdStart + 54087*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+54092*second) + ' since array.time is ' + str(array.time())

# Scan 203 = No0204
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 54117*second, mjdStart+54392*second, 'No0204', obsCode, stnCode )
if array.time() < mjdStart + (54392-10)*second:
  subarray.execute(mjdStart + 54387*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+54392*second) + ' since array.time is ' + str(array.time())

# Scan 204 = No0205
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 54428*second, mjdStart+54691*second, 'No0205', obsCode, stnCode )
if array.time() < mjdStart + (54691-10)*second:
  subarray.execute(mjdStart + 54686*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+54691*second) + ' since array.time is ' + str(array.time())

# Scan 205 = No0206
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 54727*second, mjdStart+54990*second, 'No0206', obsCode, stnCode )
if array.time() < mjdStart + (54990-10)*second:
  subarray.execute(mjdStart + 54985*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+54990*second) + ' since array.time is ' + str(array.time())

# Scan 206 = No0207
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 55007*second, mjdStart+55289*second, 'No0207', obsCode, stnCode )
if array.time() < mjdStart + (55289-10)*second:
  subarray.execute(mjdStart + 55284*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+55289*second) + ' since array.time is ' + str(array.time())

# Scan 207 = No0208
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 55360*second, mjdStart+55588*second, 'No0208', obsCode, stnCode )
if array.time() < mjdStart + (55588-10)*second:
  subarray.execute(mjdStart + 55583*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+55588*second) + ' since array.time is ' + str(array.time())

# Scan 208 = No0209
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 55661*second, mjdStart+55887*second, 'No0209', obsCode, stnCode )
if array.time() < mjdStart + (55887-10)*second:
  subarray.execute(mjdStart + 55882*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+55887*second) + ' since array.time is ' + str(array.time())

# Scan 209 = No0210
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 55958*second, mjdStart+56187*second, 'No0210', obsCode, stnCode )
if array.time() < mjdStart + (56187-10)*second:
  subarray.execute(mjdStart + 56182*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+56187*second) + ' since array.time is ' + str(array.time())

# Scan 210 = No0211
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 56206*second, mjdStart+56486*second, 'No0211', obsCode, stnCode )
if array.time() < mjdStart + (56486-10)*second:
  subarray.execute(mjdStart + 56481*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+56486*second) + ' since array.time is ' + str(array.time())

# Scan 211 = No0212
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 56498*second, mjdStart+56785*second, 'No0212', obsCode, stnCode )
if array.time() < mjdStart + (56785-10)*second:
  subarray.execute(mjdStart + 56780*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+56785*second) + ' since array.time is ' + str(array.time())

# Scan 212 = No0213
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 56850*second, mjdStart+57084*second, 'No0213', obsCode, stnCode )
if array.time() < mjdStart + (57084-10)*second:
  subarray.execute(mjdStart + 57079*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+57084*second) + ' since array.time is ' + str(array.time())

# Scan 213 = No0214
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 57123*second, mjdStart+57383*second, 'No0214', obsCode, stnCode )
if array.time() < mjdStart + (57383-10)*second:
  subarray.execute(mjdStart + 57378*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+57383*second) + ' since array.time is ' + str(array.time())

# Scan 214 = No0215
subarray.setSource(source17)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 57485*second, mjdStart+57683*second, 'No0215', obsCode, stnCode )
if array.time() < mjdStart + (57683-10)*second:
  subarray.execute(mjdStart + 57678*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+57683*second) + ' since array.time is ' + str(array.time())

# Scan 215 = No0216
subarray.setSource(source15)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 57706*second, mjdStart+57982*second, 'No0216', obsCode, stnCode )
if array.time() < mjdStart + (57982-10)*second:
  subarray.execute(mjdStart + 57977*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+57982*second) + ' since array.time is ' + str(array.time())

# Scan 216 = No0217
subarray.setSource(source18)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 58012*second, mjdStart+58281*second, 'No0217', obsCode, stnCode )
if array.time() < mjdStart + (58281-10)*second:
  subarray.execute(mjdStart + 58276*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+58281*second) + ' since array.time is ' + str(array.time())

# Scan 217 = No0218
subarray.setSource(source19)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 58313*second, mjdStart+58580*second, 'No0218', obsCode, stnCode )
if array.time() < mjdStart + (58580-10)*second:
  subarray.execute(mjdStart + 58575*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+58580*second) + ' since array.time is ' + str(array.time())

# Scan 218 = No0219
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 58597*second, mjdStart+58879*second, 'No0219', obsCode, stnCode )
if array.time() < mjdStart + (58879-10)*second:
  subarray.execute(mjdStart + 58874*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+58879*second) + ' since array.time is ' + str(array.time())

# Scan 219 = No0220
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 58942*second, mjdStart+59178*second, 'No0220', obsCode, stnCode )
if array.time() < mjdStart + (59178-10)*second:
  subarray.execute(mjdStart + 59173*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+59178*second) + ' since array.time is ' + str(array.time())

# Scan 220 = No0221
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 59243*second, mjdStart+59478*second, 'No0221', obsCode, stnCode )
if array.time() < mjdStart + (59478-10)*second:
  subarray.execute(mjdStart + 59473*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+59478*second) + ' since array.time is ' + str(array.time())

# Scan 221 = No0222
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 59574*second, mjdStart+59777*second, 'No0222', obsCode, stnCode )
if array.time() < mjdStart + (59777-10)*second:
  subarray.execute(mjdStart + 59772*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+59777*second) + ' since array.time is ' + str(array.time())

# Scan 222 = No0223
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 59822*second, mjdStart+60076*second, 'No0223', obsCode, stnCode )
if array.time() < mjdStart + (60076-10)*second:
  subarray.execute(mjdStart + 60071*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+60076*second) + ' since array.time is ' + str(array.time())

# Scan 223 = No0224
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 60092*second, mjdStart+60375*second, 'No0224', obsCode, stnCode )
if array.time() < mjdStart + (60375-10)*second:
  subarray.execute(mjdStart + 60370*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+60375*second) + ' since array.time is ' + str(array.time())

# Scan 224 = No0225
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 60443*second, mjdStart+60674*second, 'No0225', obsCode, stnCode )
if array.time() < mjdStart + (60674-10)*second:
  subarray.execute(mjdStart + 60669*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+60674*second) + ' since array.time is ' + str(array.time())

# Scan 225 = No0226
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 60720*second, mjdStart+60974*second, 'No0226', obsCode, stnCode )
if array.time() < mjdStart + (60974-10)*second:
  subarray.execute(mjdStart + 60969*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+60974*second) + ' since array.time is ' + str(array.time())

# Scan 226 = No0227
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 61039*second, mjdStart+61333*second, 'No0227', obsCode, stnCode )
if array.time() < mjdStart + (61333-10)*second:
  subarray.execute(mjdStart + 61328*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+61333*second) + ' since array.time is ' + str(array.time())

# Scan 227 = No0228
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 61393*second, mjdStart+61632*second, 'No0228', obsCode, stnCode )
if array.time() < mjdStart + (61632-10)*second:
  subarray.execute(mjdStart + 61627*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+61632*second) + ' since array.time is ' + str(array.time())

# Scan 228 = No0229
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 61692*second, mjdStart+61931*second, 'No0229', obsCode, stnCode )
if array.time() < mjdStart + (61931-10)*second:
  subarray.execute(mjdStart + 61926*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+61931*second) + ' since array.time is ' + str(array.time())

# Scan 229 = No0230
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 62032*second, mjdStart+62230*second, 'No0230', obsCode, stnCode )
if array.time() < mjdStart + (62230-10)*second:
  subarray.execute(mjdStart + 62225*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+62230*second) + ' since array.time is ' + str(array.time())

# Scan 230 = No0231
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 62290*second, mjdStart+62529*second, 'No0231', obsCode, stnCode )
if array.time() < mjdStart + (62529-10)*second:
  subarray.execute(mjdStart + 62524*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+62529*second) + ' since array.time is ' + str(array.time())

# Scan 231 = No0232
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 62576*second, mjdStart+62828*second, 'No0232', obsCode, stnCode )
if array.time() < mjdStart + (62828-10)*second:
  subarray.execute(mjdStart + 62823*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+62828*second) + ' since array.time is ' + str(array.time())

# Scan 232 = No0233
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 62968*second, mjdStart+63128*second, 'No0233', obsCode, stnCode )
if array.time() < mjdStart + (63128-10)*second:
  subarray.execute(mjdStart + 63123*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+63128*second) + ' since array.time is ' + str(array.time())

# Scan 233 = No0234
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 63179*second, mjdStart+63427*second, 'No0234', obsCode, stnCode )
if array.time() < mjdStart + (63427-10)*second:
  subarray.execute(mjdStart + 63422*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+63427*second) + ' since array.time is ' + str(array.time())

# Scan 234 = No0235
subarray.setSource(source20)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 63490*second, mjdStart+63726*second, 'No0235', obsCode, stnCode )
if array.time() < mjdStart + (63726-10)*second:
  subarray.execute(mjdStart + 63721*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+63726*second) + ' since array.time is ' + str(array.time())

# Scan 235 = No0236
subarray.setSource(source21)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 63785*second, mjdStart+64025*second, 'No0236', obsCode, stnCode )
if array.time() < mjdStart + (64025-10)*second:
  subarray.execute(mjdStart + 64020*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+64025*second) + ' since array.time is ' + str(array.time())

# Scan 236 = No0237
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 64080*second, mjdStart+64324*second, 'No0237', obsCode, stnCode )
if array.time() < mjdStart + (64324-10)*second:
  subarray.execute(mjdStart + 64319*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+64324*second) + ' since array.time is ' + str(array.time())

# Scan 237 = No0238
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 64419*second, mjdStart+64624*second, 'No0238', obsCode, stnCode )
if array.time() < mjdStart + (64624-10)*second:
  subarray.execute(mjdStart + 64619*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+64624*second) + ' since array.time is ' + str(array.time())

# Scan 238 = No0239
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 64644*second, mjdStart+64923*second, 'No0239', obsCode, stnCode )
if array.time() < mjdStart + (64923-10)*second:
  subarray.execute(mjdStart + 64918*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+64923*second) + ' since array.time is ' + str(array.time())

# Scan 239 = No0240
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 64939*second, mjdStart+65222*second, 'No0240', obsCode, stnCode )
if array.time() < mjdStart + (65222-10)*second:
  subarray.execute(mjdStart + 65217*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+65222*second) + ' since array.time is ' + str(array.time())

# Scan 240 = No0241
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 65316*second, mjdStart+65521*second, 'No0241', obsCode, stnCode )
if array.time() < mjdStart + (65521-10)*second:
  subarray.execute(mjdStart + 65516*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+65521*second) + ' since array.time is ' + str(array.time())

# Scan 241 = No0242
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 65576*second, mjdStart+65820*second, 'No0242', obsCode, stnCode )
if array.time() < mjdStart + (65820-10)*second:
  subarray.execute(mjdStart + 65815*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+65820*second) + ' since array.time is ' + str(array.time())

# Scan 242 = No0243
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 65913*second, mjdStart+66179*second, 'No0243', obsCode, stnCode )
if array.time() < mjdStart + (66179-10)*second:
  subarray.execute(mjdStart + 66174*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+66179*second) + ' since array.time is ' + str(array.time())

# Scan 243 = No0244
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 66303*second, mjdStart+66478*second, 'No0244', obsCode, stnCode )
if array.time() < mjdStart + (66478-10)*second:
  subarray.execute(mjdStart + 66473*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+66478*second) + ' since array.time is ' + str(array.time())

# Scan 244 = No0245
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 66568*second, mjdStart+66778*second, 'No0245', obsCode, stnCode )
if array.time() < mjdStart + (66778-10)*second:
  subarray.execute(mjdStart + 66773*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+66778*second) + ' since array.time is ' + str(array.time())

# Scan 245 = No0246
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 66796*second, mjdStart+67017*second, 'No0246', obsCode, stnCode )
if array.time() < mjdStart + (67017-10)*second:
  subarray.execute(mjdStart + 67012*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+67017*second) + ' since array.time is ' + str(array.time())

# Scan 246 = No0247
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 67029*second, mjdStart+67256*second, 'No0247', obsCode, stnCode )
if array.time() < mjdStart + (67256-10)*second:
  subarray.execute(mjdStart + 67251*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+67256*second) + ' since array.time is ' + str(array.time())

# Scan 247 = No0248
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 67337*second, mjdStart+67556*second, 'No0248', obsCode, stnCode )
if array.time() < mjdStart + (67556-10)*second:
  subarray.execute(mjdStart + 67551*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+67556*second) + ' since array.time is ' + str(array.time())

# Scan 248 = No0249
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 67612*second, mjdStart+67795*second, 'No0249', obsCode, stnCode )
if array.time() < mjdStart + (67795-10)*second:
  subarray.execute(mjdStart + 67790*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+67795*second) + ' since array.time is ' + str(array.time())

# Scan 249 = No0250
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 67892*second, mjdStart+68094*second, 'No0250', obsCode, stnCode )
if array.time() < mjdStart + (68094-10)*second:
  subarray.execute(mjdStart + 68089*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+68094*second) + ' since array.time is ' + str(array.time())

# Scan 250 = No0251
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 68221*second, mjdStart+68393*second, 'No0251', obsCode, stnCode )
if array.time() < mjdStart + (68393-10)*second:
  subarray.execute(mjdStart + 68388*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+68393*second) + ' since array.time is ' + str(array.time())

# Scan 251 = No0252
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 68480*second, mjdStart+68692*second, 'No0252', obsCode, stnCode )
if array.time() < mjdStart + (68692-10)*second:
  subarray.execute(mjdStart + 68687*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+68692*second) + ' since array.time is ' + str(array.time())

# Scan 252 = No0253
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 68710*second, mjdStart+68932*second, 'No0253', obsCode, stnCode )
if array.time() < mjdStart + (68932-10)*second:
  subarray.execute(mjdStart + 68927*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+68932*second) + ' since array.time is ' + str(array.time())

# Scan 253 = No0254
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 68943*second, mjdStart+69171*second, 'No0254', obsCode, stnCode )
if array.time() < mjdStart + (69171-10)*second:
  subarray.execute(mjdStart + 69166*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+69171*second) + ' since array.time is ' + str(array.time())

# Scan 254 = No0255
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 69243*second, mjdStart+69470*second, 'No0255', obsCode, stnCode )
if array.time() < mjdStart + (69470-10)*second:
  subarray.execute(mjdStart + 69465*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+69470*second) + ' since array.time is ' + str(array.time())

# Scan 255 = No0256
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 69526*second, mjdStart+69710*second, 'No0256', obsCode, stnCode )
if array.time() < mjdStart + (69710-10)*second:
  subarray.execute(mjdStart + 69705*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+69710*second) + ' since array.time is ' + str(array.time())

# Scan 256 = No0257
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 69817*second, mjdStart+70009*second, 'No0257', obsCode, stnCode )
if array.time() < mjdStart + (70009-10)*second:
  subarray.execute(mjdStart + 70004*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+70009*second) + ' since array.time is ' + str(array.time())

# Scan 257 = No0258
subarray.setSource(source22)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 70140*second, mjdStart+70308*second, 'No0258', obsCode, stnCode )
if array.time() < mjdStart + (70308-10)*second:
  subarray.execute(mjdStart + 70303*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+70308*second) + ' since array.time is ' + str(array.time())

# Scan 258 = No0259
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 70393*second, mjdStart+70607*second, 'No0259', obsCode, stnCode )
if array.time() < mjdStart + (70607-10)*second:
  subarray.execute(mjdStart + 70602*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+70607*second) + ' since array.time is ' + str(array.time())

# Scan 259 = No0260
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 70626*second, mjdStart+70847*second, 'No0260', obsCode, stnCode )
if array.time() < mjdStart + (70847-10)*second:
  subarray.execute(mjdStart + 70842*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+70847*second) + ' since array.time is ' + str(array.time())

# Scan 260 = No0261
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 70858*second, mjdStart+71086*second, 'No0261', obsCode, stnCode )
if array.time() < mjdStart + (71086-10)*second:
  subarray.execute(mjdStart + 71081*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+71086*second) + ' since array.time is ' + str(array.time())

# Scan 261 = No0262
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 71150*second, mjdStart+71385*second, 'No0262', obsCode, stnCode )
if array.time() < mjdStart + (71385-10)*second:
  subarray.execute(mjdStart + 71380*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+71385*second) + ' since array.time is ' + str(array.time())

# Scan 262 = No0263
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 71440*second, mjdStart+71624*second, 'No0263', obsCode, stnCode )
if array.time() < mjdStart + (71624-10)*second:
  subarray.execute(mjdStart + 71619*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+71624*second) + ' since array.time is ' + str(array.time())

# Scan 263 = No0264
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 71740*second, mjdStart+71924*second, 'No0264', obsCode, stnCode )
if array.time() < mjdStart + (71924-10)*second:
  subarray.execute(mjdStart + 71919*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+71924*second) + ' since array.time is ' + str(array.time())

# Scan 264 = No0265
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 71991*second, mjdStart+72283*second, 'No0265', obsCode, stnCode )
if array.time() < mjdStart + (72283-10)*second:
  subarray.execute(mjdStart + 72278*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+72283*second) + ' since array.time is ' + str(array.time())

# Scan 265 = No0266
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 72313*second, mjdStart+72522*second, 'No0266', obsCode, stnCode )
if array.time() < mjdStart + (72522-10)*second:
  subarray.execute(mjdStart + 72517*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+72522*second) + ' since array.time is ' + str(array.time())

# Scan 266 = No0267
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 72536*second, mjdStart+72821*second, 'No0267', obsCode, stnCode )
if array.time() < mjdStart + (72821-10)*second:
  subarray.execute(mjdStart + 72816*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+72821*second) + ' since array.time is ' + str(array.time())

# Scan 267 = No0268
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 72957*second, mjdStart+73120*second, 'No0268', obsCode, stnCode )
if array.time() < mjdStart + (73120-10)*second:
  subarray.execute(mjdStart + 73115*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+73120*second) + ' since array.time is ' + str(array.time())

# Scan 268 = No0269
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 73139*second, mjdStart+73419*second, 'No0269', obsCode, stnCode )
if array.time() < mjdStart + (73419-10)*second:
  subarray.execute(mjdStart + 73414*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+73419*second) + ' since array.time is ' + str(array.time())

# Scan 269 = No0270
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 73431*second, mjdStart+73659*second, 'No0270', obsCode, stnCode )
if array.time() < mjdStart + (73659-10)*second:
  subarray.execute(mjdStart + 73654*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+73659*second) + ' since array.time is ' + str(array.time())

# Scan 270 = No0271
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 73716*second, mjdStart+73958*second, 'No0271', obsCode, stnCode )
if array.time() < mjdStart + (73958-10)*second:
  subarray.execute(mjdStart + 73953*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+73958*second) + ' since array.time is ' + str(array.time())

# Scan 271 = No0272
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 74010*second, mjdStart+74197*second, 'No0272', obsCode, stnCode )
if array.time() < mjdStart + (74197-10)*second:
  subarray.execute(mjdStart + 74192*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+74197*second) + ' since array.time is ' + str(array.time())

# Scan 272 = No0273
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 74323*second, mjdStart+74497*second, 'No0273', obsCode, stnCode )
if array.time() < mjdStart + (74497-10)*second:
  subarray.execute(mjdStart + 74492*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+74497*second) + ' since array.time is ' + str(array.time())

# Scan 273 = No0274
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 74568*second, mjdStart+74796*second, 'No0274', obsCode, stnCode )
if array.time() < mjdStart + (74796-10)*second:
  subarray.execute(mjdStart + 74791*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+74796*second) + ' since array.time is ' + str(array.time())

# Scan 274 = No0275
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 74829*second, mjdStart+75035*second, 'No0275', obsCode, stnCode )
if array.time() < mjdStart + (75035-10)*second:
  subarray.execute(mjdStart + 75030*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+75035*second) + ' since array.time is ' + str(array.time())

# Scan 275 = No0276
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 75050*second, mjdStart+75274*second, 'No0276', obsCode, stnCode )
if array.time() < mjdStart + (75274-10)*second:
  subarray.execute(mjdStart + 75269*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+75274*second) + ' since array.time is ' + str(array.time())

# Scan 276 = No0277
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 75406*second, mjdStart+75574*second, 'No0277', obsCode, stnCode )
if array.time() < mjdStart + (75574-10)*second:
  subarray.execute(mjdStart + 75569*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+75574*second) + ' since array.time is ' + str(array.time())

# Scan 277 = No0278
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 75593*second, mjdStart+75813*second, 'No0278', obsCode, stnCode )
if array.time() < mjdStart + (75813-10)*second:
  subarray.execute(mjdStart + 75808*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+75813*second) + ' since array.time is ' + str(array.time())

# Scan 278 = No0279
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 75825*second, mjdStart+76052*second, 'No0279', obsCode, stnCode )
if array.time() < mjdStart + (76052-10)*second:
  subarray.execute(mjdStart + 76047*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+76052*second) + ' since array.time is ' + str(array.time())

# Scan 279 = No0280
subarray.setSource(source26)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 76105*second, mjdStart+76351*second, 'No0280', obsCode, stnCode )
if array.time() < mjdStart + (76351-10)*second:
  subarray.execute(mjdStart + 76346*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+76351*second) + ' since array.time is ' + str(array.time())

# Scan 280 = No0281
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 76401*second, mjdStart+76591*second, 'No0281', obsCode, stnCode )
if array.time() < mjdStart + (76591-10)*second:
  subarray.execute(mjdStart + 76586*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+76591*second) + ' since array.time is ' + str(array.time())

# Scan 281 = No0282
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 76726*second, mjdStart+76890*second, 'No0282', obsCode, stnCode )
if array.time() < mjdStart + (76890-10)*second:
  subarray.execute(mjdStart + 76885*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+76890*second) + ' since array.time is ' + str(array.time())

# Scan 282 = No0283
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 76965*second, mjdStart+77129*second, 'No0283', obsCode, stnCode )
if array.time() < mjdStart + (77129-10)*second:
  subarray.execute(mjdStart + 77124*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+77129*second) + ' since array.time is ' + str(array.time())

# Scan 283 = No0284
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 77165*second, mjdStart+77369*second, 'No0284', obsCode, stnCode )
if array.time() < mjdStart + (77369-10)*second:
  subarray.execute(mjdStart + 77364*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+77369*second) + ' since array.time is ' + str(array.time())

# Scan 284 = No0285
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 77385*second, mjdStart+77608*second, 'No0285', obsCode, stnCode )
if array.time() < mjdStart + (77608-10)*second:
  subarray.execute(mjdStart + 77603*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+77608*second) + ' since array.time is ' + str(array.time())

# Scan 285 = No0286
subarray.setSource(source23)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 77736*second, mjdStart+77907*second, 'No0286', obsCode, stnCode )
if array.time() < mjdStart + (77907-10)*second:
  subarray.execute(mjdStart + 77902*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+77907*second) + ' since array.time is ' + str(array.time())

# Scan 286 = No0287
subarray.setSource(source24)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 77926*second, mjdStart+78206*second, 'No0287', obsCode, stnCode )
if array.time() < mjdStart + (78206-10)*second:
  subarray.execute(mjdStart + 78201*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+78206*second) + ' since array.time is ' + str(array.time())

# Scan 287 = No0288
subarray.setSource(source25)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 78266*second, mjdStart+78565*second, 'No0288', obsCode, stnCode )
if array.time() < mjdStart + (78565-10)*second:
  subarray.execute(mjdStart + 78560*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+78565*second) + ' since array.time is ' + str(array.time())

# Scan 288 = No0289
subarray.setSource(source27)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 78603*second, mjdStart+78865*second, 'No0289', obsCode, stnCode )
if array.time() < mjdStart + (78865-10)*second:
  subarray.execute(mjdStart + 78860*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+78865*second) + ' since array.time is ' + str(array.time())

# Scan 289 = No0290
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 78988*second, mjdStart+79164*second, 'No0290', obsCode, stnCode )
if array.time() < mjdStart + (79164-10)*second:
  subarray.execute(mjdStart + 79159*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+79164*second) + ' since array.time is ' + str(array.time())

# Scan 290 = No0291
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 79353*second, mjdStart+79463*second, 'No0291', obsCode, stnCode )
if array.time() < mjdStart + (79463-10)*second:
  subarray.execute(mjdStart + 79458*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+79463*second) + ' since array.time is ' + str(array.time())

# Scan 291 = No0292
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 79502*second, mjdStart+79762*second, 'No0292', obsCode, stnCode )
if array.time() < mjdStart + (79762-10)*second:
  subarray.execute(mjdStart + 79757*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+79762*second) + ' since array.time is ' + str(array.time())

# Scan 292 = No0293
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 79780*second, mjdStart+80061*second, 'No0293', obsCode, stnCode )
if array.time() < mjdStart + (80061-10)*second:
  subarray.execute(mjdStart + 80056*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+80061*second) + ' since array.time is ' + str(array.time())

# Scan 293 = No0294
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 80155*second, mjdStart+80301*second, 'No0294', obsCode, stnCode )
if array.time() < mjdStart + (80301-10)*second:
  subarray.execute(mjdStart + 80296*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+80301*second) + ' since array.time is ' + str(array.time())

# Scan 294 = No0295
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 80341*second, mjdStart+80540*second, 'No0295', obsCode, stnCode )
if array.time() < mjdStart + (80540-10)*second:
  subarray.execute(mjdStart + 80535*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+80540*second) + ' since array.time is ' + str(array.time())

# Scan 295 = No0296
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 80569*second, mjdStart+80779*second, 'No0296', obsCode, stnCode )
if array.time() < mjdStart + (80779-10)*second:
  subarray.execute(mjdStart + 80774*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+80779*second) + ' since array.time is ' + str(array.time())

# Scan 296 = No0297
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 80904*second, mjdStart+81079*second, 'No0297', obsCode, stnCode )
if array.time() < mjdStart + (81079-10)*second:
  subarray.execute(mjdStart + 81074*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+81079*second) + ' since array.time is ' + str(array.time())

# Scan 297 = No0298
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 81194*second, mjdStart+81378*second, 'No0298', obsCode, stnCode )
if array.time() < mjdStart + (81378-10)*second:
  subarray.execute(mjdStart + 81373*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+81378*second) + ' since array.time is ' + str(array.time())

# Scan 298 = No0299
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 81420*second, mjdStart+81617*second, 'No0299', obsCode, stnCode )
if array.time() < mjdStart + (81617-10)*second:
  subarray.execute(mjdStart + 81612*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+81617*second) + ' since array.time is ' + str(array.time())

# Scan 299 = No0300
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 81635*second, mjdStart+81856*second, 'No0300', obsCode, stnCode )
if array.time() < mjdStart + (81856-10)*second:
  subarray.execute(mjdStart + 81851*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+81856*second) + ' since array.time is ' + str(array.time())

# Scan 300 = No0301
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 81980*second, mjdStart+82215*second, 'No0301', obsCode, stnCode )
if array.time() < mjdStart + (82215-10)*second:
  subarray.execute(mjdStart + 82210*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+82215*second) + ' since array.time is ' + str(array.time())

# Scan 301 = No0302
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 82362*second, mjdStart+82515*second, 'No0302', obsCode, stnCode )
if array.time() < mjdStart + (82515-10)*second:
  subarray.execute(mjdStart + 82510*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+82515*second) + ' since array.time is ' + str(array.time())

# Scan 302 = No0303
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 82557*second, mjdStart+82814*second, 'No0303', obsCode, stnCode )
if array.time() < mjdStart + (82814-10)*second:
  subarray.execute(mjdStart + 82809*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+82814*second) + ' since array.time is ' + str(array.time())

# Scan 303 = No0304
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 82832*second, mjdStart+83113*second, 'No0304', obsCode, stnCode )
if array.time() < mjdStart + (83113-10)*second:
  subarray.execute(mjdStart + 83108*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+83113*second) + ' since array.time is ' + str(array.time())

# Scan 304 = No0305
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 83192*second, mjdStart+83412*second, 'No0305', obsCode, stnCode )
if array.time() < mjdStart + (83412-10)*second:
  subarray.execute(mjdStart + 83407*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+83412*second) + ' since array.time is ' + str(array.time())

# Scan 305 = No0306
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 83449*second, mjdStart+83711*second, 'No0306', obsCode, stnCode )
if array.time() < mjdStart + (83711-10)*second:
  subarray.execute(mjdStart + 83706*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+83711*second) + ' since array.time is ' + str(array.time())

# Scan 306 = No0307
subarray.setSource(source29)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 83783*second, mjdStart+84010*second, 'No0307', obsCode, stnCode )
if array.time() < mjdStart + (84010-10)*second:
  subarray.execute(mjdStart + 84005*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+84010*second) + ' since array.time is ' + str(array.time())

# Scan 307 = No0308
subarray.setSource(source30)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 84053*second, mjdStart+84250*second, 'No0308', obsCode, stnCode )
if array.time() < mjdStart + (84250-10)*second:
  subarray.execute(mjdStart + 84245*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+84250*second) + ' since array.time is ' + str(array.time())

# Scan 308 = No0309
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 84267*second, mjdStart+84519*second, 'No0309', obsCode, stnCode )
if array.time() < mjdStart + (84519-10)*second:
  subarray.execute(mjdStart + 84514*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+84519*second) + ' since array.time is ' + str(array.time())

# Scan 309 = No0310
subarray.setSource(source2)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 84595*second, mjdStart+84758*second, 'No0310', obsCode, stnCode )
if array.time() < mjdStart + (84758-10)*second:
  subarray.execute(mjdStart + 84753*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+84758*second) + ' since array.time is ' + str(array.time())

# Scan 310 = No0311
subarray.setSource(source1)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 84794*second, mjdStart+84998*second, 'No0311', obsCode, stnCode )
if array.time() < mjdStart + (84998-10)*second:
  subarray.execute(mjdStart + 84993*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+84998*second) + ' since array.time is ' + str(array.time())

# Scan 311 = No0312
subarray.setSource(source3)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 85028*second, mjdStart+85237*second, 'No0312', obsCode, stnCode )
if array.time() < mjdStart + (85237-10)*second:
  subarray.execute(mjdStart + 85232*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+85237*second) + ' since array.time is ' + str(array.time())

# Scan 312 = No0313
subarray.setSource(source4)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 85316*second, mjdStart+85476*second, 'No0313', obsCode, stnCode )
if array.time() < mjdStart + (85476-10)*second:
  subarray.execute(mjdStart + 85471*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+85476*second) + ' since array.time is ' + str(array.time())

# Scan 313 = No0314
subarray.setSource(source28)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 85627*second, mjdStart+85776*second, 'No0314', obsCode, stnCode )
if array.time() < mjdStart + (85776-10)*second:
  subarray.execute(mjdStart + 85771*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+85776*second) + ' since array.time is ' + str(array.time())

# Scan 314 = No0315
subarray.setSource(source31)
recorder0.setPacket(0, 0, 36, 5008)
subarray.setRecord(mjdStart + 85967*second, mjdStart+86045*second, 'No0315', obsCode, stnCode )
if array.time() < mjdStart + (86045-10)*second:
  subarray.execute(mjdStart + 86040*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+86045*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 86046*second)
