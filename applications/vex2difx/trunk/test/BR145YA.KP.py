from edu.nrao.evla.observe import Mark5C
from edu.nrao.evla.observe import ESSR
from edu.nrao.evla.observe import MatrixSwitch
from edu.nrao.evla.observe import RDBE
from edu.nrao.evla.observe import VLBALoIfSetup
from edu.nrao.evla.observe import Parameters
from edu.nrao.evla.observe import bbc

second = 1.0/86400.0

deltat2 = 1

obsCode = 'BR145YA'
stnCode = 'KP'
mjdStart = 56453 + 20550*second

# File written by vex2script version 0.21 vintage 20130923

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
loif0.setIf('D', '1cm', 'L', 21500, 'U', 'NA', 12400, '1cm', 502.49)
loif0.setPhaseCal(1)
loif0.setDBEParams(0, -1, -1, 10, 0)
loif0.setDBERemember(0, 1)
channelSet0 = [ \
  bbc(0, 1040, 32, 'L', 2, 0), \
  bbc(0, 1008, 32, 'L', 2, 0), \
  bbc(0, 976, 32, 'L', 2, 0), \
  bbc(0, 944, 32, 'L', 2, 0), \
  bbc(0, 912, 32, 'L', 2, 0), \
  bbc(0, 880, 32, 'L', 2, 0), \
  bbc(0, 848, 32, 'L', 2, 0), \
  bbc(0, 816, 32, 'L', 2, 0), \
  bbc(0, 784, 32, 'L', 2, 0), \
  bbc(0, 752, 32, 'L', 2, 0), \
  bbc(0, 720, 32, 'L', 2, 0), \
  bbc(0, 688, 32, 'L', 2, 0), \
  bbc(0, 656, 32, 'L', 2, 0), \
  bbc(0, 624, 32, 'L', 2, 0), \
  bbc(0, 592, 32, 'L', 2, 0), \
  bbc(0, 560, 32, 'L', 2, 0) \
  ]

loif1 = VLBALoIfSetup()
loif1.setIf('B', '1cm', 'R', 21500, 'U', 'NA', 12400, '1cm', 0, 0, 708.94)
loif1.setIf('D', '1cm', 'L', 21500, 'U', 'NA', 12400, '1cm', 0, 0, 708.94)
loif1.setPhaseCal(0)
loif1.setDBEParams(0, -1, -1, 10, 0)
loif1.setDBEParams(1, -1, -1, 10, 0)
loif1.setDBERemember(0, 1)
loif1.setDBERemember(1, 1)
channelSet1 = [ \
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

loif2 = VLBALoIfSetup()
loif2.setIf('B', '1cm', 'R', 21500, 'U', 'NA', 12400, '1cm', 0, 0, 717.62)
loif2.setIf('D', '1cm', 'L', 21500, 'U', 'NA', 12400, '1cm', 0, 0, 717.62)
loif2.setPhaseCal(0)
loif2.setDBEParams(0, -1, -1, 10, 0)
loif2.setDBEParams(1, -1, -1, 10, 0)
loif2.setDBERemember(0, 1)
loif2.setDBERemember(1, 1)
channelSet2 = [ \
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

loif3 = VLBALoIfSetup()
loif3.setIf('B', '1cm', 'R', 21500, 'U', 'NA', 12400, '1cm', 0, 0, 717.76)
loif3.setIf('D', '1cm', 'L', 21500, 'U', 'NA', 12400, '1cm', 0, 0, 717.76)
loif3.setPhaseCal(0)
loif3.setDBEParams(0, -1, -1, 10, 0)
loif3.setDBEParams(1, -1, -1, 10, 0)
loif3.setDBERemember(0, 1)
loif3.setDBERemember(1, 1)
channelSet3 = [ \
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

source0 = Source(5.03251455925294, 0.189335915857676)
source0.setName('G045.07+0.13')

source1 = Source(5.04047795848661, 0.191039395992717)
source1.setName('J1915+1056')

source2 = Source(5.03209245864555, 0.215447394582495)
source2.setName('J1913+1220')

source3 = Source(4.99890651372095, 0.172246577816278)
source3.setName('J1905+095X')

source4 = Source(5.03719438329837, 0.16365620783393)
source4.setName('G043.89-0.78')

source5 = Source(5.00778329829372, 0.159175948128874)
source5.setName('J1907+0907')

source6 = Source(5.07153655431088, 0.151830923412514)
source6.setName('J1922.043')

source7 = Source(4.99890651590261, 0.172246574907396)
source7.setName('J1905+0952')

source8 = Source(5.01202715290919, 0.210015764144009)
source8.setName('J1908.043')

source9 = Source(5.03682146461486, 0.194681112743465)
source9.setName('G045.47+0.05')

source10 = Source(5.01202715290919, 0.210015764144009)
source10.setName('J1908.045')

source11 = Source(4.99890651372095, 0.172246577816278)
source11.setName('J1905.045')

source12 = Source(5.08623974970897, 0.217478138011881)
source12.setName('J1925.045')

source13 = Source(5.03193014157068, 0.229158485928888)
source13.setName('J1913.045')

source14 = Source(4.71571120865265, 1.36952116868447)
source14.setName('F045.1800')

source15 = Source(4.71571120865265, 1.36952116868447)
source15.setName('F043.1800')

source16 = Source(4.71571120865265, 1.36952116868447)
source16.setName('F04X.1800')

source17 = Source(5.08760606995103, 0.368391309029093)
source17.setName('F045.1925')

source18 = Source(5.08760606995103, 0.368391309029093)
source18.setName('F043.1925')

source19 = Source(5.08760606995103, 0.368391309029093)
source19.setName('F04X.1925')

source20 = Source(5.91830029131392, 0.497115407885748)
source20.setName('F045.2236')

source21 = Source(5.91830029131392, 0.497115407885748)
source21.setName('F043.2236')

source22 = Source(5.91830029131392, 0.497115407885748)
source22.setName('F04X.2236')

source23 = Source(5.99504220559394, 0.281839456510863)
source23.setName('F045.3C454')

source24 = Source(5.99504220559394, 0.281839456510863)
source24.setName('F043.3C454')

source25 = Source(5.99504220559394, 0.281839456510863)
source25.setName('F04X.3C454')

source26 = Source(5.37525006983884, 0.215166810264438)
source26.setName('2031+121')

source27 = Source(5.45787230023807, 0.54903876109788)
source27.setName('2050+312')

source28 = Source(5.77146141561781, 0.737886325395089)
source28.setName('2202+421')

source29 = Source(6.26738963239008, 1.42909622142168)
source29.setName('2356+815')

source30 = Source(2.13218630028016, 0.86993284857836)
source30.setName('0808+495')

source31 = Source(2.45045907252108, 1.08672043684212)
source31.setName('0921+621')

source32 = Source(2.71919138582031, 0.694715632769355)
source32.setName('1023+394')

source33 = Source(2.81165725196595, 1.4121625900691)
source33.setName('1044+805')

source34 = Source(2.87321957108397, 0.0273377888482097)
source34.setName('1058+013')

source35 = Source(3.01456778057965, 0.667678357620636)
source35.setName('1130+381')

source36 = Source(3.44910976632018, 0.564535390543093)
source36.setName('1310+322')

source37 = Source(3.75148322728417, 0.949239915293384)
source37.setName('1419+542')

source38 = Source(3.9492784650773, 0.0600723547339256)
source38.setName('1505+032')

source39 = Source(5.33772716876415, 0.556480322738605)
source39.setName('2023+315')

source40 = Source(5.70017111252372, -0.269284354760263)
source40.setName('2146-152')

source41 = Source(5.72648486460655, 0.306779406391781)
source41.setName('2152+173')

source42 = Source(5.83085486337993, 0.616169975248936)
source42.setName('2216+351')

source43 = Source(5.94209832363127, 0.172750966043688)
source43.setName('2241+095')

source44 = Source(3.65612819029139, 1.33906048314962)
source44.setName('1357+764')

source45 = Source(3.99972879921009, 0.00437257691416127)
source45.setName('1516+001')

source46 = Source(4.02572380122985, -0.480017833538647)
source46.setName('1522-273')

source47 = Source(5.77463226478932, 0.304212089457618)
source47.setName('2203+172')

source48 = Source(5.81238029202996, 0.417621141712403)
source48.setName('2212+235')

source49 = Source(5.99504220559394, 0.281839456510863)
source49.setName('2253+160')

source50 = Source(6.01309290657266, -0.48821346977324)
source50.setName('2258-275')

source51 = Source(6.11191444583992, 0.091290186642129)
source51.setName('2320+051')

source52 = Source(0.178722557961916, -0.0309894172268178)
source52.setName('0040-014')

source53 = Source(0.212867736643357, 0.557754312008908)
source53.setName('0048+315')

source54 = Source(0.248367212211808, 0.286589563898675)
source54.setName('0056+162')

source55 = Source(2.87190549494596, 1.41794754292514)
source55.setName('1058+811')

source56 = Source(3.45162209556543, 0.963975995264093)
source56.setName('1311+551')

source57 = Source(4.00093432284994, 0.340983966919418)
source57.setName('1516+193')

source58 = Source(0.378357757122702, 0.453501023717742)
source58.setName('0126+255')

source59 = Source(0.545773574335029, 0.562141926728386)
source59.setName('0205+321')

source60 = Source(0.592104205157972, -0.0415812667029848)
source60.setName('0215-022')

source61 = Source(0.649411668050306, 1.1754939374851)
source61.setName('0228+672')

source62 = Source(0.662199302416976, 0.233557615312662)
source62.setName('0231+132')

source63 = Source(0.697496331718612, 0.0745711888996406)
source63.setName('0239+041')

source64 = Source(3.95702806824075, 0.744494603825134)
source64.setName('1506+423')

source65 = Source(4.24849968193416, 0.597134874004876)
source65.setName('1613+341')

source66 = Source(4.36547808590432, 0.694282418535547)
source66.setName('1640+394')

source67 = Source(4.57041014059698, 0.794317432871835)
source67.setName('1727+453')

source68 = Source(4.6624173118168, 1.22343105162446)
source68.setName('1748+700')

# Setup Scan 
# changing to mode geodetic
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 4)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source26)
# Setup scan - run right away, but do not start recording
subarray.execute( array.time() + 2*second )

# Scan 0 = No0001
print 'Not a recording scan but still set switches for No0001.'
subarray.setSwitches(mjdStart + 0*second, mjdStart+60*second, obsCode+'_'+stnCode+'_'+'No0001')
if array.time() < mjdStart + (60-10)*second:
  subarray.execute(mjdStart + 55*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+60*second) + ' since array.time is ' + str(array.time())

# Scan 1 = No0002
subarray.setSource(source27)
print 'Not a recording scan but still set switches for No0002.'
subarray.setSwitches(mjdStart + 83*second, mjdStart+155*second, obsCode+'_'+stnCode+'_'+'No0002')
if array.time() < mjdStart + (155-10)*second:
  subarray.execute(mjdStart + 150*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+155*second) + ' since array.time is ' + str(array.time())

# Scan 2 = No0003
subarray.setSource(source28)
print 'Not a recording scan but still set switches for No0003.'
subarray.setSwitches(mjdStart + 179*second, mjdStart+257*second, obsCode+'_'+stnCode+'_'+'No0003')
if array.time() < mjdStart + (257-10)*second:
  subarray.execute(mjdStart + 252*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+257*second) + ' since array.time is ' + str(array.time())

# Scan 3 = No0004
subarray.setSource(source29)
print 'Not a recording scan but still set switches for No0004.'
subarray.setSwitches(mjdStart + 296*second, mjdStart+371*second, obsCode+'_'+stnCode+'_'+'No0004')
if array.time() < mjdStart + (371-10)*second:
  subarray.execute(mjdStart + 366*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+371*second) + ' since array.time is ' + str(array.time())

# Scan 4 = No0005
subarray.setSource(source30)
print 'Not a recording scan but still set switches for No0005.'
subarray.setSwitches(mjdStart + 434*second, mjdStart+499*second, obsCode+'_'+stnCode+'_'+'No0005')
if array.time() < mjdStart + (499-10)*second:
  subarray.execute(mjdStart + 494*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+499*second) + ' since array.time is ' + str(array.time())

# Scan 5 = No0006
subarray.setSource(source31)
print 'Not a recording scan but still set switches for No0006.'
subarray.setSwitches(mjdStart + 537*second, mjdStart+601*second, obsCode+'_'+stnCode+'_'+'No0006')
if array.time() < mjdStart + (601-10)*second:
  subarray.execute(mjdStart + 596*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+601*second) + ' since array.time is ' + str(array.time())

# Scan 6 = No0007
subarray.setSource(source32)
print 'Not a recording scan but still set switches for No0007.'
subarray.setSwitches(mjdStart + 629*second, mjdStart+708*second, obsCode+'_'+stnCode+'_'+'No0007')
if array.time() < mjdStart + (708-10)*second:
  subarray.execute(mjdStart + 703*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+708*second) + ' since array.time is ' + str(array.time())

# Scan 7 = No0008
subarray.setSource(source33)
print 'Not a recording scan but still set switches for No0008.'
subarray.setSwitches(mjdStart + 777*second, mjdStart+842*second, obsCode+'_'+stnCode+'_'+'No0008')
if array.time() < mjdStart + (842-10)*second:
  subarray.execute(mjdStart + 837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+842*second) + ' since array.time is ' + str(array.time())

# Scan 8 = No0009
subarray.setSource(source34)
print 'Not a recording scan but still set switches for No0009.'
subarray.setSwitches(mjdStart + 1052*second, mjdStart+1112*second, obsCode+'_'+stnCode+'_'+'No0009')
if array.time() < mjdStart + (1112-10)*second:
  subarray.execute(mjdStart + 1107*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1112*second) + ' since array.time is ' + str(array.time())

# Scan 9 = No0010
subarray.setSource(source35)
print 'Not a recording scan but still set switches for No0010.'
subarray.setSwitches(mjdStart + 1178*second, mjdStart+1243*second, obsCode+'_'+stnCode+'_'+'No0010')
if array.time() < mjdStart + (1243-10)*second:
  subarray.execute(mjdStart + 1238*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1243*second) + ' since array.time is ' + str(array.time())

# Scan 10 = No0011
subarray.setSource(source36)
print 'Not a recording scan but still set switches for No0011.'
subarray.setSwitches(mjdStart + 1289*second, mjdStart+1349*second, obsCode+'_'+stnCode+'_'+'No0011')
if array.time() < mjdStart + (1349-10)*second:
  subarray.execute(mjdStart + 1344*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1349*second) + ' since array.time is ' + str(array.time())

# Scan 11 = No0012
subarray.setSource(source37)
print 'Not a recording scan but still set switches for No0012.'
subarray.setSwitches(mjdStart + 1393*second, mjdStart+1469*second, obsCode+'_'+stnCode+'_'+'No0012')
if array.time() < mjdStart + (1469-10)*second:
  subarray.execute(mjdStart + 1464*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1469*second) + ' since array.time is ' + str(array.time())

# Scan 12 = No0013
subarray.setSource(source38)
print 'Not a recording scan but still set switches for No0013.'
subarray.setSwitches(mjdStart + 1691*second, mjdStart+1756*second, obsCode+'_'+stnCode+'_'+'No0013')
if array.time() < mjdStart + (1756-10)*second:
  subarray.execute(mjdStart + 1751*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1756*second) + ' since array.time is ' + str(array.time())

# Scan 13 = No0014
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source14)
print 'Not a recording scan but still set switches for No0014.'
subarray.setSwitches(mjdStart + 1885*second, mjdStart+1950*second, obsCode+'_'+stnCode+'_'+'No0014')
if array.time() < mjdStart + (1950-10)*second:
  subarray.execute(mjdStart + 1945*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+1950*second) + ' since array.time is ' + str(array.time())

# Scan 14 = No0015
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source16)
print 'Not a recording scan but still set switches for No0015.'
subarray.setSwitches(mjdStart + 1956*second, mjdStart+2015*second, obsCode+'_'+stnCode+'_'+'No0015')
if array.time() < mjdStart + (2015-10)*second:
  subarray.execute(mjdStart + 2010*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2015*second) + ' since array.time is ' + str(array.time())

# Scan 15 = No0016
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source15)
print 'Not a recording scan but still set switches for No0016.'
subarray.setSwitches(mjdStart + 2021*second, mjdStart+2081*second, obsCode+'_'+stnCode+'_'+'No0016')
if array.time() < mjdStart + (2081-10)*second:
  subarray.execute(mjdStart + 2076*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2081*second) + ' since array.time is ' + str(array.time())

# Scan 16 = No0017
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0017.'
subarray.setSwitches(mjdStart + 2283*second, mjdStart+2303*second, obsCode+'_'+stnCode+'_'+'No0017')
if array.time() < mjdStart + (2303-10)*second:
  subarray.execute(mjdStart + 2298*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2303*second) + ' since array.time is ' + str(array.time())

# Scan 17 = No0018
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0018.'
subarray.setSwitches(mjdStart + 2311*second, mjdStart+2333*second, obsCode+'_'+stnCode+'_'+'No0018')
if array.time() < mjdStart + (2333-10)*second:
  subarray.execute(mjdStart + 2328*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2333*second) + ' since array.time is ' + str(array.time())

# Scan 18 = No0019
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0019.'
subarray.setSwitches(mjdStart + 2341*second, mjdStart+2363*second, obsCode+'_'+stnCode+'_'+'No0019')
if array.time() < mjdStart + (2363-10)*second:
  subarray.execute(mjdStart + 2358*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2363*second) + ' since array.time is ' + str(array.time())

# Scan 19 = No0020
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0020.'
subarray.setSwitches(mjdStart + 2373*second, mjdStart+2393*second, obsCode+'_'+stnCode+'_'+'No0020')
if array.time() < mjdStart + (2393-10)*second:
  subarray.execute(mjdStart + 2388*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2393*second) + ' since array.time is ' + str(array.time())

# Scan 20 = No0021
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0021.'
subarray.setSwitches(mjdStart + 2403*second, mjdStart+2423*second, obsCode+'_'+stnCode+'_'+'No0021')
if array.time() < mjdStart + (2423-10)*second:
  subarray.execute(mjdStart + 2418*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2423*second) + ' since array.time is ' + str(array.time())

# Scan 21 = No0022
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0022.'
subarray.setSwitches(mjdStart + 2433*second, mjdStart+2453*second, obsCode+'_'+stnCode+'_'+'No0022')
if array.time() < mjdStart + (2453-10)*second:
  subarray.execute(mjdStart + 2448*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2453*second) + ' since array.time is ' + str(array.time())

# Scan 22 = No0023
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0023.'
subarray.setSwitches(mjdStart + 2463*second, mjdStart+2483*second, obsCode+'_'+stnCode+'_'+'No0023')
if array.time() < mjdStart + (2483-10)*second:
  subarray.execute(mjdStart + 2478*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2483*second) + ' since array.time is ' + str(array.time())

# Scan 23 = No0024
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0024.'
subarray.setSwitches(mjdStart + 2491*second, mjdStart+2513*second, obsCode+'_'+stnCode+'_'+'No0024')
if array.time() < mjdStart + (2513-10)*second:
  subarray.execute(mjdStart + 2508*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2513*second) + ' since array.time is ' + str(array.time())

# Scan 24 = No0025
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0025.'
subarray.setSwitches(mjdStart + 2521*second, mjdStart+2543*second, obsCode+'_'+stnCode+'_'+'No0025')
if array.time() < mjdStart + (2543-10)*second:
  subarray.execute(mjdStart + 2538*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2543*second) + ' since array.time is ' + str(array.time())

# Scan 25 = No0026
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0026.'
subarray.setSwitches(mjdStart + 2553*second, mjdStart+2573*second, obsCode+'_'+stnCode+'_'+'No0026')
if array.time() < mjdStart + (2573-10)*second:
  subarray.execute(mjdStart + 2568*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2573*second) + ' since array.time is ' + str(array.time())

# Scan 26 = No0027
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0027.'
subarray.setSwitches(mjdStart + 2583*second, mjdStart+2603*second, obsCode+'_'+stnCode+'_'+'No0027')
if array.time() < mjdStart + (2603-10)*second:
  subarray.execute(mjdStart + 2598*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2603*second) + ' since array.time is ' + str(array.time())

# Scan 27 = No0028
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0028.'
subarray.setSwitches(mjdStart + 2613*second, mjdStart+2632*second, obsCode+'_'+stnCode+'_'+'No0028')
if array.time() < mjdStart + (2632-10)*second:
  subarray.execute(mjdStart + 2627*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2632*second) + ' since array.time is ' + str(array.time())

# Scan 28 = No0029
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0029.'
subarray.setSwitches(mjdStart + 2642*second, mjdStart+2662*second, obsCode+'_'+stnCode+'_'+'No0029')
if array.time() < mjdStart + (2662-10)*second:
  subarray.execute(mjdStart + 2657*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2662*second) + ' since array.time is ' + str(array.time())

# Scan 29 = No0030
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0030.'
subarray.setSwitches(mjdStart + 2670*second, mjdStart+2692*second, obsCode+'_'+stnCode+'_'+'No0030')
if array.time() < mjdStart + (2692-10)*second:
  subarray.execute(mjdStart + 2687*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2692*second) + ' since array.time is ' + str(array.time())

# Scan 30 = No0031
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0031.'
subarray.setSwitches(mjdStart + 2700*second, mjdStart+2722*second, obsCode+'_'+stnCode+'_'+'No0031')
if array.time() < mjdStart + (2722-10)*second:
  subarray.execute(mjdStart + 2717*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2722*second) + ' since array.time is ' + str(array.time())

# Scan 31 = No0032
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0032.'
subarray.setSwitches(mjdStart + 2732*second, mjdStart+2752*second, obsCode+'_'+stnCode+'_'+'No0032')
if array.time() < mjdStart + (2752-10)*second:
  subarray.execute(mjdStart + 2747*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2752*second) + ' since array.time is ' + str(array.time())

# Scan 32 = No0033
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0033.'
subarray.setSwitches(mjdStart + 2762*second, mjdStart+2782*second, obsCode+'_'+stnCode+'_'+'No0033')
if array.time() < mjdStart + (2782-10)*second:
  subarray.execute(mjdStart + 2777*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2782*second) + ' since array.time is ' + str(array.time())

# Scan 33 = No0034
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0034.'
subarray.setSwitches(mjdStart + 2792*second, mjdStart+2812*second, obsCode+'_'+stnCode+'_'+'No0034')
if array.time() < mjdStart + (2812-10)*second:
  subarray.execute(mjdStart + 2807*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2812*second) + ' since array.time is ' + str(array.time())

# Scan 34 = No0035
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0035.'
subarray.setSwitches(mjdStart + 2822*second, mjdStart+2842*second, obsCode+'_'+stnCode+'_'+'No0035')
if array.time() < mjdStart + (2842-10)*second:
  subarray.execute(mjdStart + 2837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2842*second) + ' since array.time is ' + str(array.time())

# Scan 35 = No0036
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0036.'
subarray.setSwitches(mjdStart + 2850*second, mjdStart+2872*second, obsCode+'_'+stnCode+'_'+'No0036')
if array.time() < mjdStart + (2872-10)*second:
  subarray.execute(mjdStart + 2867*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2872*second) + ' since array.time is ' + str(array.time())

# Scan 36 = No0037
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0037.'
subarray.setSwitches(mjdStart + 2880*second, mjdStart+2902*second, obsCode+'_'+stnCode+'_'+'No0037')
if array.time() < mjdStart + (2902-10)*second:
  subarray.execute(mjdStart + 2897*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2902*second) + ' since array.time is ' + str(array.time())

# Scan 37 = No0038
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0038.'
subarray.setSwitches(mjdStart + 2912*second, mjdStart+2932*second, obsCode+'_'+stnCode+'_'+'No0038')
if array.time() < mjdStart + (2932-10)*second:
  subarray.execute(mjdStart + 2927*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2932*second) + ' since array.time is ' + str(array.time())

# Scan 38 = No0039
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0039.'
subarray.setSwitches(mjdStart + 2942*second, mjdStart+2962*second, obsCode+'_'+stnCode+'_'+'No0039')
if array.time() < mjdStart + (2962-10)*second:
  subarray.execute(mjdStart + 2957*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2962*second) + ' since array.time is ' + str(array.time())

# Scan 39 = No0040
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0040.'
subarray.setSwitches(mjdStart + 2972*second, mjdStart+2991*second, obsCode+'_'+stnCode+'_'+'No0040')
if array.time() < mjdStart + (2991-10)*second:
  subarray.execute(mjdStart + 2986*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+2991*second) + ' since array.time is ' + str(array.time())

# Scan 40 = No0041
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0041.'
subarray.setSwitches(mjdStart + 3001*second, mjdStart+3021*second, obsCode+'_'+stnCode+'_'+'No0041')
if array.time() < mjdStart + (3021-10)*second:
  subarray.execute(mjdStart + 3016*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3021*second) + ' since array.time is ' + str(array.time())

# Scan 41 = No0042
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0042.'
subarray.setSwitches(mjdStart + 3029*second, mjdStart+3046*second, obsCode+'_'+stnCode+'_'+'No0042')
if array.time() < mjdStart + (3046-10)*second:
  subarray.execute(mjdStart + 3041*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3046*second) + ' since array.time is ' + str(array.time())

# Scan 42 = No0043
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0043.'
subarray.setSwitches(mjdStart + 3057*second, mjdStart+3076*second, obsCode+'_'+stnCode+'_'+'No0043')
if array.time() < mjdStart + (3076-10)*second:
  subarray.execute(mjdStart + 3071*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3076*second) + ' since array.time is ' + str(array.time())

# Scan 43 = No0044
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0044.'
subarray.setSwitches(mjdStart + 3087*second, mjdStart+3106*second, obsCode+'_'+stnCode+'_'+'No0044')
if array.time() < mjdStart + (3106-10)*second:
  subarray.execute(mjdStart + 3101*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3106*second) + ' since array.time is ' + str(array.time())

# Scan 44 = No0045
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0045.'
subarray.setSwitches(mjdStart + 3117*second, mjdStart+3136*second, obsCode+'_'+stnCode+'_'+'No0045')
if array.time() < mjdStart + (3136-10)*second:
  subarray.execute(mjdStart + 3131*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3136*second) + ' since array.time is ' + str(array.time())

# Scan 45 = No0046
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0046.'
subarray.setSwitches(mjdStart + 3147*second, mjdStart+3166*second, obsCode+'_'+stnCode+'_'+'No0046')
if array.time() < mjdStart + (3166-10)*second:
  subarray.execute(mjdStart + 3161*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3166*second) + ' since array.time is ' + str(array.time())

# Scan 46 = No0047
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0047.'
subarray.setSwitches(mjdStart + 3177*second, mjdStart+3196*second, obsCode+'_'+stnCode+'_'+'No0047')
if array.time() < mjdStart + (3196-10)*second:
  subarray.execute(mjdStart + 3191*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3196*second) + ' since array.time is ' + str(array.time())

# Scan 47 = No0048
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0048.'
subarray.setSwitches(mjdStart + 3207*second, mjdStart+3226*second, obsCode+'_'+stnCode+'_'+'No0048')
if array.time() < mjdStart + (3226-10)*second:
  subarray.execute(mjdStart + 3221*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3226*second) + ' since array.time is ' + str(array.time())

# Scan 48 = No0049
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0049.'
subarray.setSwitches(mjdStart + 3236*second, mjdStart+3256*second, obsCode+'_'+stnCode+'_'+'No0049')
if array.time() < mjdStart + (3256-10)*second:
  subarray.execute(mjdStart + 3251*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3256*second) + ' since array.time is ' + str(array.time())

# Scan 49 = No0050
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0050.'
subarray.setSwitches(mjdStart + 3266*second, mjdStart+3286*second, obsCode+'_'+stnCode+'_'+'No0050')
if array.time() < mjdStart + (3286-10)*second:
  subarray.execute(mjdStart + 3281*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3286*second) + ' since array.time is ' + str(array.time())

# Scan 50 = No0051
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0051.'
subarray.setSwitches(mjdStart + 3296*second, mjdStart+3316*second, obsCode+'_'+stnCode+'_'+'No0051')
if array.time() < mjdStart + (3316-10)*second:
  subarray.execute(mjdStart + 3311*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3316*second) + ' since array.time is ' + str(array.time())

# Scan 51 = No0052
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0052.'
subarray.setSwitches(mjdStart + 3326*second, mjdStart+3345*second, obsCode+'_'+stnCode+'_'+'No0052')
if array.time() < mjdStart + (3345-10)*second:
  subarray.execute(mjdStart + 3340*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3345*second) + ' since array.time is ' + str(array.time())

# Scan 52 = No0053
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0053.'
subarray.setSwitches(mjdStart + 3355*second, mjdStart+3375*second, obsCode+'_'+stnCode+'_'+'No0053')
if array.time() < mjdStart + (3375-10)*second:
  subarray.execute(mjdStart + 3370*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3375*second) + ' since array.time is ' + str(array.time())

# Scan 53 = No0054
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0054.'
subarray.setSwitches(mjdStart + 3385*second, mjdStart+3405*second, obsCode+'_'+stnCode+'_'+'No0054')
if array.time() < mjdStart + (3405-10)*second:
  subarray.execute(mjdStart + 3400*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3405*second) + ' since array.time is ' + str(array.time())

# Scan 54 = No0055
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0055.'
subarray.setSwitches(mjdStart + 3416*second, mjdStart+3435*second, obsCode+'_'+stnCode+'_'+'No0055')
if array.time() < mjdStart + (3435-10)*second:
  subarray.execute(mjdStart + 3430*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3435*second) + ' since array.time is ' + str(array.time())

# Scan 55 = No0056
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0056.'
subarray.setSwitches(mjdStart + 3446*second, mjdStart+3465*second, obsCode+'_'+stnCode+'_'+'No0056')
if array.time() < mjdStart + (3465-10)*second:
  subarray.execute(mjdStart + 3460*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3465*second) + ' since array.time is ' + str(array.time())

# Scan 56 = No0057
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0057.'
subarray.setSwitches(mjdStart + 3476*second, mjdStart+3495*second, obsCode+'_'+stnCode+'_'+'No0057')
if array.time() < mjdStart + (3495-10)*second:
  subarray.execute(mjdStart + 3490*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3495*second) + ' since array.time is ' + str(array.time())

# Scan 57 = No0058
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0058.'
subarray.setSwitches(mjdStart + 3506*second, mjdStart+3525*second, obsCode+'_'+stnCode+'_'+'No0058')
if array.time() < mjdStart + (3525-10)*second:
  subarray.execute(mjdStart + 3520*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3525*second) + ' since array.time is ' + str(array.time())

# Scan 58 = No0059
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0059.'
subarray.setSwitches(mjdStart + 3536*second, mjdStart+3555*second, obsCode+'_'+stnCode+'_'+'No0059')
if array.time() < mjdStart + (3555-10)*second:
  subarray.execute(mjdStart + 3550*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3555*second) + ' since array.time is ' + str(array.time())

# Scan 59 = No0060
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0060.'
subarray.setSwitches(mjdStart + 3566*second, mjdStart+3585*second, obsCode+'_'+stnCode+'_'+'No0060')
if array.time() < mjdStart + (3585-10)*second:
  subarray.execute(mjdStart + 3580*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3585*second) + ' since array.time is ' + str(array.time())

# Scan 60 = No0061
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0061.'
subarray.setSwitches(mjdStart + 3596*second, mjdStart+3615*second, obsCode+'_'+stnCode+'_'+'No0061')
if array.time() < mjdStart + (3615-10)*second:
  subarray.execute(mjdStart + 3610*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3615*second) + ' since array.time is ' + str(array.time())

# Scan 61 = No0062
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0062.'
subarray.setSwitches(mjdStart + 3626*second, mjdStart+3645*second, obsCode+'_'+stnCode+'_'+'No0062')
if array.time() < mjdStart + (3645-10)*second:
  subarray.execute(mjdStart + 3640*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3645*second) + ' since array.time is ' + str(array.time())

# Scan 62 = No0063
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0063.'
subarray.setSwitches(mjdStart + 3656*second, mjdStart+3675*second, obsCode+'_'+stnCode+'_'+'No0063')
if array.time() < mjdStart + (3675-10)*second:
  subarray.execute(mjdStart + 3670*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3675*second) + ' since array.time is ' + str(array.time())

# Scan 63 = No0064
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0064.'
subarray.setSwitches(mjdStart + 3686*second, mjdStart+3704*second, obsCode+'_'+stnCode+'_'+'No0064')
if array.time() < mjdStart + (3704-10)*second:
  subarray.execute(mjdStart + 3699*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3704*second) + ' since array.time is ' + str(array.time())

# Scan 64 = No0065
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0065.'
subarray.setSwitches(mjdStart + 3715*second, mjdStart+3734*second, obsCode+'_'+stnCode+'_'+'No0065')
if array.time() < mjdStart + (3734-10)*second:
  subarray.execute(mjdStart + 3729*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3734*second) + ' since array.time is ' + str(array.time())

# Scan 65 = No0066
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0066.'
subarray.setSwitches(mjdStart + 3745*second, mjdStart+3764*second, obsCode+'_'+stnCode+'_'+'No0066')
if array.time() < mjdStart + (3764-10)*second:
  subarray.execute(mjdStart + 3759*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3764*second) + ' since array.time is ' + str(array.time())

# Scan 66 = No0067
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0067.'
subarray.setSwitches(mjdStart + 3774*second, mjdStart+3792*second, obsCode+'_'+stnCode+'_'+'No0067')
if array.time() < mjdStart + (3792-10)*second:
  subarray.execute(mjdStart + 3787*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3792*second) + ' since array.time is ' + str(array.time())

# Scan 67 = No0068
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0068.'
subarray.setSwitches(mjdStart + 3802*second, mjdStart+3822*second, obsCode+'_'+stnCode+'_'+'No0068')
if array.time() < mjdStart + (3822-10)*second:
  subarray.execute(mjdStart + 3817*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3822*second) + ' since array.time is ' + str(array.time())

# Scan 68 = No0069
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0069.'
subarray.setSwitches(mjdStart + 3832*second, mjdStart+3852*second, obsCode+'_'+stnCode+'_'+'No0069')
if array.time() < mjdStart + (3852-10)*second:
  subarray.execute(mjdStart + 3847*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3852*second) + ' since array.time is ' + str(array.time())

# Scan 69 = No0070
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0070.'
subarray.setSwitches(mjdStart + 3862*second, mjdStart+3882*second, obsCode+'_'+stnCode+'_'+'No0070')
if array.time() < mjdStart + (3882-10)*second:
  subarray.execute(mjdStart + 3877*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3882*second) + ' since array.time is ' + str(array.time())

# Scan 70 = No0071
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0071.'
subarray.setSwitches(mjdStart + 3892*second, mjdStart+3912*second, obsCode+'_'+stnCode+'_'+'No0071')
if array.time() < mjdStart + (3912-10)*second:
  subarray.execute(mjdStart + 3907*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3912*second) + ' since array.time is ' + str(array.time())

# Scan 71 = No0072
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0072.'
subarray.setSwitches(mjdStart + 3922*second, mjdStart+3942*second, obsCode+'_'+stnCode+'_'+'No0072')
if array.time() < mjdStart + (3942-10)*second:
  subarray.execute(mjdStart + 3937*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3942*second) + ' since array.time is ' + str(array.time())

# Scan 72 = No0073
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0073.'
subarray.setSwitches(mjdStart + 3952*second, mjdStart+3971*second, obsCode+'_'+stnCode+'_'+'No0073')
if array.time() < mjdStart + (3971-10)*second:
  subarray.execute(mjdStart + 3966*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+3971*second) + ' since array.time is ' + str(array.time())

# Scan 73 = No0074
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0074.'
subarray.setSwitches(mjdStart + 3983*second, mjdStart+4001*second, obsCode+'_'+stnCode+'_'+'No0074')
if array.time() < mjdStart + (4001-10)*second:
  subarray.execute(mjdStart + 3996*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4001*second) + ' since array.time is ' + str(array.time())

# Scan 74 = No0075
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0075.'
subarray.setSwitches(mjdStart + 4013*second, mjdStart+4031*second, obsCode+'_'+stnCode+'_'+'No0075')
if array.time() < mjdStart + (4031-10)*second:
  subarray.execute(mjdStart + 4026*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4031*second) + ' since array.time is ' + str(array.time())

# Scan 75 = No0076
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0076.'
subarray.setSwitches(mjdStart + 4043*second, mjdStart+4061*second, obsCode+'_'+stnCode+'_'+'No0076')
if array.time() < mjdStart + (4061-10)*second:
  subarray.execute(mjdStart + 4056*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4061*second) + ' since array.time is ' + str(array.time())

# Scan 76 = No0077
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0077.'
subarray.setSwitches(mjdStart + 4073*second, mjdStart+4091*second, obsCode+'_'+stnCode+'_'+'No0077')
if array.time() < mjdStart + (4091-10)*second:
  subarray.execute(mjdStart + 4086*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4091*second) + ' since array.time is ' + str(array.time())

# Scan 77 = No0078
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0078.'
subarray.setSwitches(mjdStart + 4103*second, mjdStart+4121*second, obsCode+'_'+stnCode+'_'+'No0078')
if array.time() < mjdStart + (4121-10)*second:
  subarray.execute(mjdStart + 4116*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4121*second) + ' since array.time is ' + str(array.time())

# Scan 78 = No0079
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0079.'
subarray.setSwitches(mjdStart + 4133*second, mjdStart+4151*second, obsCode+'_'+stnCode+'_'+'No0079')
if array.time() < mjdStart + (4151-10)*second:
  subarray.execute(mjdStart + 4146*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4151*second) + ' since array.time is ' + str(array.time())

# Scan 79 = No0080
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0080.'
subarray.setSwitches(mjdStart + 4163*second, mjdStart+4181*second, obsCode+'_'+stnCode+'_'+'No0080')
if array.time() < mjdStart + (4181-10)*second:
  subarray.execute(mjdStart + 4176*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4181*second) + ' since array.time is ' + str(array.time())

# Scan 80 = No0081
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0081.'
subarray.setSwitches(mjdStart + 4193*second, mjdStart+4211*second, obsCode+'_'+stnCode+'_'+'No0081')
if array.time() < mjdStart + (4211-10)*second:
  subarray.execute(mjdStart + 4206*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4211*second) + ' since array.time is ' + str(array.time())

# Scan 81 = No0082
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0082.'
subarray.setSwitches(mjdStart + 4223*second, mjdStart+4241*second, obsCode+'_'+stnCode+'_'+'No0082')
if array.time() < mjdStart + (4241-10)*second:
  subarray.execute(mjdStart + 4236*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4241*second) + ' since array.time is ' + str(array.time())

# Scan 82 = No0083
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0083.'
subarray.setSwitches(mjdStart + 4253*second, mjdStart+4271*second, obsCode+'_'+stnCode+'_'+'No0083')
if array.time() < mjdStart + (4271-10)*second:
  subarray.execute(mjdStart + 4266*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4271*second) + ' since array.time is ' + str(array.time())

# Scan 83 = No0084
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0084.'
subarray.setSwitches(mjdStart + 4283*second, mjdStart+4301*second, obsCode+'_'+stnCode+'_'+'No0084')
if array.time() < mjdStart + (4301-10)*second:
  subarray.execute(mjdStart + 4296*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4301*second) + ' since array.time is ' + str(array.time())

# Scan 84 = No0085
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0085.'
subarray.setSwitches(mjdStart + 4313*second, mjdStart+4330*second, obsCode+'_'+stnCode+'_'+'No0085')
if array.time() < mjdStart + (4330-10)*second:
  subarray.execute(mjdStart + 4325*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4330*second) + ' since array.time is ' + str(array.time())

# Scan 85 = No0086
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0086.'
subarray.setSwitches(mjdStart + 4344*second, mjdStart+4360*second, obsCode+'_'+stnCode+'_'+'No0086')
if array.time() < mjdStart + (4360-10)*second:
  subarray.execute(mjdStart + 4355*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4360*second) + ' since array.time is ' + str(array.time())

# Scan 86 = No0087
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0087.'
subarray.setSwitches(mjdStart + 4374*second, mjdStart+4390*second, obsCode+'_'+stnCode+'_'+'No0087')
if array.time() < mjdStart + (4390-10)*second:
  subarray.execute(mjdStart + 4385*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4390*second) + ' since array.time is ' + str(array.time())

# Scan 87 = No0088
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0088.'
subarray.setSwitches(mjdStart + 4404*second, mjdStart+4420*second, obsCode+'_'+stnCode+'_'+'No0088')
if array.time() < mjdStart + (4420-10)*second:
  subarray.execute(mjdStart + 4415*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4420*second) + ' since array.time is ' + str(array.time())

# Scan 88 = No0089
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0089.'
subarray.setSwitches(mjdStart + 4434*second, mjdStart+4450*second, obsCode+'_'+stnCode+'_'+'No0089')
if array.time() < mjdStart + (4450-10)*second:
  subarray.execute(mjdStart + 4445*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4450*second) + ' since array.time is ' + str(array.time())

# Scan 89 = No0090
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0090.'
subarray.setSwitches(mjdStart + 4464*second, mjdStart+4480*second, obsCode+'_'+stnCode+'_'+'No0090')
if array.time() < mjdStart + (4480-10)*second:
  subarray.execute(mjdStart + 4475*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4480*second) + ' since array.time is ' + str(array.time())

# Scan 90 = No0091
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0091.'
subarray.setSwitches(mjdStart + 4494*second, mjdStart+4510*second, obsCode+'_'+stnCode+'_'+'No0091')
if array.time() < mjdStart + (4510-10)*second:
  subarray.execute(mjdStart + 4505*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4510*second) + ' since array.time is ' + str(array.time())

# Scan 91 = No0092
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0092.'
subarray.setSwitches(mjdStart + 4520*second, mjdStart+4542*second, obsCode+'_'+stnCode+'_'+'No0092')
if array.time() < mjdStart + (4542-10)*second:
  subarray.execute(mjdStart + 4537*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4542*second) + ' since array.time is ' + str(array.time())

# Scan 92 = No0093
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0093.'
subarray.setSwitches(mjdStart + 4550*second, mjdStart+4572*second, obsCode+'_'+stnCode+'_'+'No0093')
if array.time() < mjdStart + (4572-10)*second:
  subarray.execute(mjdStart + 4567*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4572*second) + ' since array.time is ' + str(array.time())

# Scan 93 = No0094
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0094.'
subarray.setSwitches(mjdStart + 4580*second, mjdStart+4602*second, obsCode+'_'+stnCode+'_'+'No0094')
if array.time() < mjdStart + (4602-10)*second:
  subarray.execute(mjdStart + 4597*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4602*second) + ' since array.time is ' + str(array.time())

# Scan 94 = No0095
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0095.'
subarray.setSwitches(mjdStart + 4612*second, mjdStart+4632*second, obsCode+'_'+stnCode+'_'+'No0095')
if array.time() < mjdStart + (4632-10)*second:
  subarray.execute(mjdStart + 4627*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4632*second) + ' since array.time is ' + str(array.time())

# Scan 95 = No0096
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0096.'
subarray.setSwitches(mjdStart + 4642*second, mjdStart+4662*second, obsCode+'_'+stnCode+'_'+'No0096')
if array.time() < mjdStart + (4662-10)*second:
  subarray.execute(mjdStart + 4657*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4662*second) + ' since array.time is ' + str(array.time())

# Scan 96 = No0097
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0097.'
subarray.setSwitches(mjdStart + 4672*second, mjdStart+4692*second, obsCode+'_'+stnCode+'_'+'No0097')
if array.time() < mjdStart + (4692-10)*second:
  subarray.execute(mjdStart + 4687*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4692*second) + ' since array.time is ' + str(array.time())

# Scan 97 = No0098
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0098.'
subarray.setSwitches(mjdStart + 4702*second, mjdStart+4722*second, obsCode+'_'+stnCode+'_'+'No0098')
if array.time() < mjdStart + (4722-10)*second:
  subarray.execute(mjdStart + 4717*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4722*second) + ' since array.time is ' + str(array.time())

# Scan 98 = No0099
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0099.'
subarray.setSwitches(mjdStart + 4730*second, mjdStart+4752*second, obsCode+'_'+stnCode+'_'+'No0099')
if array.time() < mjdStart + (4752-10)*second:
  subarray.execute(mjdStart + 4747*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4752*second) + ' since array.time is ' + str(array.time())

# Scan 99 = No0100
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0100.'
subarray.setSwitches(mjdStart + 4760*second, mjdStart+4782*second, obsCode+'_'+stnCode+'_'+'No0100')
if array.time() < mjdStart + (4782-10)*second:
  subarray.execute(mjdStart + 4777*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4782*second) + ' since array.time is ' + str(array.time())

# Scan 100 = No0101
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0101.'
subarray.setSwitches(mjdStart + 4792*second, mjdStart+4811*second, obsCode+'_'+stnCode+'_'+'No0101')
if array.time() < mjdStart + (4811-10)*second:
  subarray.execute(mjdStart + 4806*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4811*second) + ' since array.time is ' + str(array.time())

# Scan 101 = No0102
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0102.'
subarray.setSwitches(mjdStart + 4821*second, mjdStart+4841*second, obsCode+'_'+stnCode+'_'+'No0102')
if array.time() < mjdStart + (4841-10)*second:
  subarray.execute(mjdStart + 4836*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4841*second) + ' since array.time is ' + str(array.time())

# Scan 102 = No0103
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0103.'
subarray.setSwitches(mjdStart + 4851*second, mjdStart+4871*second, obsCode+'_'+stnCode+'_'+'No0103')
if array.time() < mjdStart + (4871-10)*second:
  subarray.execute(mjdStart + 4866*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4871*second) + ' since array.time is ' + str(array.time())

# Scan 103 = No0104
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0104.'
subarray.setSwitches(mjdStart + 4881*second, mjdStart+4901*second, obsCode+'_'+stnCode+'_'+'No0104')
if array.time() < mjdStart + (4901-10)*second:
  subarray.execute(mjdStart + 4896*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4901*second) + ' since array.time is ' + str(array.time())

# Scan 104 = No0105
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0105.'
subarray.setSwitches(mjdStart + 4909*second, mjdStart+4931*second, obsCode+'_'+stnCode+'_'+'No0105')
if array.time() < mjdStart + (4931-10)*second:
  subarray.execute(mjdStart + 4926*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4931*second) + ' since array.time is ' + str(array.time())

# Scan 105 = No0106
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0106.'
subarray.setSwitches(mjdStart + 4939*second, mjdStart+4961*second, obsCode+'_'+stnCode+'_'+'No0106')
if array.time() < mjdStart + (4961-10)*second:
  subarray.execute(mjdStart + 4956*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4961*second) + ' since array.time is ' + str(array.time())

# Scan 106 = No0107
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0107.'
subarray.setSwitches(mjdStart + 4971*second, mjdStart+4991*second, obsCode+'_'+stnCode+'_'+'No0107')
if array.time() < mjdStart + (4991-10)*second:
  subarray.execute(mjdStart + 4986*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+4991*second) + ' since array.time is ' + str(array.time())

# Scan 107 = No0108
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0108.'
subarray.setSwitches(mjdStart + 5001*second, mjdStart+5021*second, obsCode+'_'+stnCode+'_'+'No0108')
if array.time() < mjdStart + (5021-10)*second:
  subarray.execute(mjdStart + 5016*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5021*second) + ' since array.time is ' + str(array.time())

# Scan 108 = No0109
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0109.'
subarray.setSwitches(mjdStart + 5031*second, mjdStart+5051*second, obsCode+'_'+stnCode+'_'+'No0109')
if array.time() < mjdStart + (5051-10)*second:
  subarray.execute(mjdStart + 5046*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5051*second) + ' since array.time is ' + str(array.time())

# Scan 109 = No0110
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0110.'
subarray.setSwitches(mjdStart + 5061*second, mjdStart+5081*second, obsCode+'_'+stnCode+'_'+'No0110')
if array.time() < mjdStart + (5081-10)*second:
  subarray.execute(mjdStart + 5076*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5081*second) + ' since array.time is ' + str(array.time())

# Scan 110 = No0111
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0111.'
subarray.setSwitches(mjdStart + 5089*second, mjdStart+5111*second, obsCode+'_'+stnCode+'_'+'No0111')
if array.time() < mjdStart + (5111-10)*second:
  subarray.execute(mjdStart + 5106*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5111*second) + ' since array.time is ' + str(array.time())

# Scan 111 = No0112
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0112.'
subarray.setSwitches(mjdStart + 5119*second, mjdStart+5141*second, obsCode+'_'+stnCode+'_'+'No0112')
if array.time() < mjdStart + (5141-10)*second:
  subarray.execute(mjdStart + 5136*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5141*second) + ' since array.time is ' + str(array.time())

# Scan 112 = No0113
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0113.'
subarray.setSwitches(mjdStart + 5151*second, mjdStart+5170*second, obsCode+'_'+stnCode+'_'+'No0113')
if array.time() < mjdStart + (5170-10)*second:
  subarray.execute(mjdStart + 5165*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5170*second) + ' since array.time is ' + str(array.time())

# Scan 113 = No0114
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0114.'
subarray.setSwitches(mjdStart + 5180*second, mjdStart+5200*second, obsCode+'_'+stnCode+'_'+'No0114')
if array.time() < mjdStart + (5200-10)*second:
  subarray.execute(mjdStart + 5195*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5200*second) + ' since array.time is ' + str(array.time())

# Scan 114 = No0115
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0115.'
subarray.setSwitches(mjdStart + 5210*second, mjdStart+5230*second, obsCode+'_'+stnCode+'_'+'No0115')
if array.time() < mjdStart + (5230-10)*second:
  subarray.execute(mjdStart + 5225*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5230*second) + ' since array.time is ' + str(array.time())

# Scan 115 = No0116
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0116.'
subarray.setSwitches(mjdStart + 5240*second, mjdStart+5260*second, obsCode+'_'+stnCode+'_'+'No0116')
if array.time() < mjdStart + (5260-10)*second:
  subarray.execute(mjdStart + 5255*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5260*second) + ' since array.time is ' + str(array.time())

# Scan 116 = No0117
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0117.'
subarray.setSwitches(mjdStart + 5268*second, mjdStart+5285*second, obsCode+'_'+stnCode+'_'+'No0117')
if array.time() < mjdStart + (5285-10)*second:
  subarray.execute(mjdStart + 5280*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5285*second) + ' since array.time is ' + str(array.time())

# Scan 117 = No0118
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0118.'
subarray.setSwitches(mjdStart + 5296*second, mjdStart+5315*second, obsCode+'_'+stnCode+'_'+'No0118')
if array.time() < mjdStart + (5315-10)*second:
  subarray.execute(mjdStart + 5310*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5315*second) + ' since array.time is ' + str(array.time())

# Scan 118 = No0119
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0119.'
subarray.setSwitches(mjdStart + 5326*second, mjdStart+5345*second, obsCode+'_'+stnCode+'_'+'No0119')
if array.time() < mjdStart + (5345-10)*second:
  subarray.execute(mjdStart + 5340*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5345*second) + ' since array.time is ' + str(array.time())

# Scan 119 = No0120
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0120.'
subarray.setSwitches(mjdStart + 5356*second, mjdStart+5375*second, obsCode+'_'+stnCode+'_'+'No0120')
if array.time() < mjdStart + (5375-10)*second:
  subarray.execute(mjdStart + 5370*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5375*second) + ' since array.time is ' + str(array.time())

# Scan 120 = No0121
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0121.'
subarray.setSwitches(mjdStart + 5386*second, mjdStart+5405*second, obsCode+'_'+stnCode+'_'+'No0121')
if array.time() < mjdStart + (5405-10)*second:
  subarray.execute(mjdStart + 5400*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5405*second) + ' since array.time is ' + str(array.time())

# Scan 121 = No0122
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0122.'
subarray.setSwitches(mjdStart + 5416*second, mjdStart+5435*second, obsCode+'_'+stnCode+'_'+'No0122')
if array.time() < mjdStart + (5435-10)*second:
  subarray.execute(mjdStart + 5430*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5435*second) + ' since array.time is ' + str(array.time())

# Scan 122 = No0123
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0123.'
subarray.setSwitches(mjdStart + 5446*second, mjdStart+5465*second, obsCode+'_'+stnCode+'_'+'No0123')
if array.time() < mjdStart + (5465-10)*second:
  subarray.execute(mjdStart + 5460*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5465*second) + ' since array.time is ' + str(array.time())

# Scan 123 = No0124
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0124.'
subarray.setSwitches(mjdStart + 5476*second, mjdStart+5495*second, obsCode+'_'+stnCode+'_'+'No0124')
if array.time() < mjdStart + (5495-10)*second:
  subarray.execute(mjdStart + 5490*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5495*second) + ' since array.time is ' + str(array.time())

# Scan 124 = No0125
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0125.'
subarray.setSwitches(mjdStart + 5506*second, mjdStart+5524*second, obsCode+'_'+stnCode+'_'+'No0125')
if array.time() < mjdStart + (5524-10)*second:
  subarray.execute(mjdStart + 5519*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5524*second) + ' since array.time is ' + str(array.time())

# Scan 125 = No0126
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0126.'
subarray.setSwitches(mjdStart + 5535*second, mjdStart+5554*second, obsCode+'_'+stnCode+'_'+'No0126')
if array.time() < mjdStart + (5554-10)*second:
  subarray.execute(mjdStart + 5549*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5554*second) + ' since array.time is ' + str(array.time())

# Scan 126 = No0127
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0127.'
subarray.setSwitches(mjdStart + 5565*second, mjdStart+5584*second, obsCode+'_'+stnCode+'_'+'No0127')
if array.time() < mjdStart + (5584-10)*second:
  subarray.execute(mjdStart + 5579*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5584*second) + ' since array.time is ' + str(array.time())

# Scan 127 = No0128
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0128.'
subarray.setSwitches(mjdStart + 5595*second, mjdStart+5614*second, obsCode+'_'+stnCode+'_'+'No0128')
if array.time() < mjdStart + (5614-10)*second:
  subarray.execute(mjdStart + 5609*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5614*second) + ' since array.time is ' + str(array.time())

# Scan 128 = No0129
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0129.'
subarray.setSwitches(mjdStart + 5625*second, mjdStart+5644*second, obsCode+'_'+stnCode+'_'+'No0129')
if array.time() < mjdStart + (5644-10)*second:
  subarray.execute(mjdStart + 5639*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5644*second) + ' since array.time is ' + str(array.time())

# Scan 129 = No0130
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0130.'
subarray.setSwitches(mjdStart + 5655*second, mjdStart+5674*second, obsCode+'_'+stnCode+'_'+'No0130')
if array.time() < mjdStart + (5674-10)*second:
  subarray.execute(mjdStart + 5669*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5674*second) + ' since array.time is ' + str(array.time())

# Scan 130 = No0131
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0131.'
subarray.setSwitches(mjdStart + 5685*second, mjdStart+5704*second, obsCode+'_'+stnCode+'_'+'No0131')
if array.time() < mjdStart + (5704-10)*second:
  subarray.execute(mjdStart + 5699*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5704*second) + ' since array.time is ' + str(array.time())

# Scan 131 = No0132
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0132.'
subarray.setSwitches(mjdStart + 5715*second, mjdStart+5734*second, obsCode+'_'+stnCode+'_'+'No0132')
if array.time() < mjdStart + (5734-10)*second:
  subarray.execute(mjdStart + 5729*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5734*second) + ' since array.time is ' + str(array.time())

# Scan 132 = No0133
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0133.'
subarray.setSwitches(mjdStart + 5745*second, mjdStart+5764*second, obsCode+'_'+stnCode+'_'+'No0133')
if array.time() < mjdStart + (5764-10)*second:
  subarray.execute(mjdStart + 5759*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5764*second) + ' since array.time is ' + str(array.time())

# Scan 133 = No0134
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0134.'
subarray.setSwitches(mjdStart + 5775*second, mjdStart+5794*second, obsCode+'_'+stnCode+'_'+'No0134')
if array.time() < mjdStart + (5794-10)*second:
  subarray.execute(mjdStart + 5789*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5794*second) + ' since array.time is ' + str(array.time())

# Scan 134 = No0135
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0135.'
subarray.setSwitches(mjdStart + 5805*second, mjdStart+5824*second, obsCode+'_'+stnCode+'_'+'No0135')
if array.time() < mjdStart + (5824-10)*second:
  subarray.execute(mjdStart + 5819*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5824*second) + ' since array.time is ' + str(array.time())

# Scan 135 = No0136
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0136.'
subarray.setSwitches(mjdStart + 5835*second, mjdStart+5854*second, obsCode+'_'+stnCode+'_'+'No0136')
if array.time() < mjdStart + (5854-10)*second:
  subarray.execute(mjdStart + 5849*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5854*second) + ' since array.time is ' + str(array.time())

# Scan 136 = No0137
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0137.'
subarray.setSwitches(mjdStart + 5865*second, mjdStart+5884*second, obsCode+'_'+stnCode+'_'+'No0137')
if array.time() < mjdStart + (5884-10)*second:
  subarray.execute(mjdStart + 5879*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5884*second) + ' since array.time is ' + str(array.time())

# Scan 137 = No0138
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0138.'
subarray.setSwitches(mjdStart + 5895*second, mjdStart+5913*second, obsCode+'_'+stnCode+'_'+'No0138')
if array.time() < mjdStart + (5913-10)*second:
  subarray.execute(mjdStart + 5908*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5913*second) + ' since array.time is ' + str(array.time())

# Scan 138 = No0139
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0139.'
subarray.setSwitches(mjdStart + 5924*second, mjdStart+5943*second, obsCode+'_'+stnCode+'_'+'No0139')
if array.time() < mjdStart + (5943-10)*second:
  subarray.execute(mjdStart + 5938*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5943*second) + ' since array.time is ' + str(array.time())

# Scan 139 = No0140
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0140.'
subarray.setSwitches(mjdStart + 5954*second, mjdStart+5973*second, obsCode+'_'+stnCode+'_'+'No0140')
if array.time() < mjdStart + (5973-10)*second:
  subarray.execute(mjdStart + 5968*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+5973*second) + ' since array.time is ' + str(array.time())

# Scan 140 = No0141
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0141.'
subarray.setSwitches(mjdStart + 5984*second, mjdStart+6003*second, obsCode+'_'+stnCode+'_'+'No0141')
if array.time() < mjdStart + (6003-10)*second:
  subarray.execute(mjdStart + 5998*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6003*second) + ' since array.time is ' + str(array.time())

# Scan 141 = No0142
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0142.'
subarray.setSwitches(mjdStart + 6013*second, mjdStart+6030*second, obsCode+'_'+stnCode+'_'+'No0142')
if array.time() < mjdStart + (6030-10)*second:
  subarray.execute(mjdStart + 6025*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6030*second) + ' since array.time is ' + str(array.time())

# Scan 142 = No0143
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0143.'
subarray.setSwitches(mjdStart + 6040*second, mjdStart+6060*second, obsCode+'_'+stnCode+'_'+'No0143')
if array.time() < mjdStart + (6060-10)*second:
  subarray.execute(mjdStart + 6055*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6060*second) + ' since array.time is ' + str(array.time())

# Scan 143 = No0144
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0144.'
subarray.setSwitches(mjdStart + 6070*second, mjdStart+6090*second, obsCode+'_'+stnCode+'_'+'No0144')
if array.time() < mjdStart + (6090-10)*second:
  subarray.execute(mjdStart + 6085*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6090*second) + ' since array.time is ' + str(array.time())

# Scan 144 = No0145
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0145.'
subarray.setSwitches(mjdStart + 6100*second, mjdStart+6120*second, obsCode+'_'+stnCode+'_'+'No0145')
if array.time() < mjdStart + (6120-10)*second:
  subarray.execute(mjdStart + 6115*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6120*second) + ' since array.time is ' + str(array.time())

# Scan 145 = No0146
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0146.'
subarray.setSwitches(mjdStart + 6130*second, mjdStart+6150*second, obsCode+'_'+stnCode+'_'+'No0146')
if array.time() < mjdStart + (6150-10)*second:
  subarray.execute(mjdStart + 6145*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6150*second) + ' since array.time is ' + str(array.time())

# Scan 146 = No0147
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0147.'
subarray.setSwitches(mjdStart + 6160*second, mjdStart+6179*second, obsCode+'_'+stnCode+'_'+'No0147')
if array.time() < mjdStart + (6179-10)*second:
  subarray.execute(mjdStart + 6174*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6179*second) + ' since array.time is ' + str(array.time())

# Scan 147 = No0148
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0148.'
subarray.setSwitches(mjdStart + 6189*second, mjdStart+6209*second, obsCode+'_'+stnCode+'_'+'No0148')
if array.time() < mjdStart + (6209-10)*second:
  subarray.execute(mjdStart + 6204*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6209*second) + ' since array.time is ' + str(array.time())

# Scan 148 = No0149
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0149.'
subarray.setSwitches(mjdStart + 6221*second, mjdStart+6239*second, obsCode+'_'+stnCode+'_'+'No0149')
if array.time() < mjdStart + (6239-10)*second:
  subarray.execute(mjdStart + 6234*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6239*second) + ' since array.time is ' + str(array.time())

# Scan 149 = No0150
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0150.'
subarray.setSwitches(mjdStart + 6251*second, mjdStart+6269*second, obsCode+'_'+stnCode+'_'+'No0150')
if array.time() < mjdStart + (6269-10)*second:
  subarray.execute(mjdStart + 6264*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6269*second) + ' since array.time is ' + str(array.time())

# Scan 150 = No0151
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0151.'
subarray.setSwitches(mjdStart + 6281*second, mjdStart+6299*second, obsCode+'_'+stnCode+'_'+'No0151')
if array.time() < mjdStart + (6299-10)*second:
  subarray.execute(mjdStart + 6294*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6299*second) + ' since array.time is ' + str(array.time())

# Scan 151 = No0152
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0152.'
subarray.setSwitches(mjdStart + 6311*second, mjdStart+6329*second, obsCode+'_'+stnCode+'_'+'No0152')
if array.time() < mjdStart + (6329-10)*second:
  subarray.execute(mjdStart + 6324*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6329*second) + ' since array.time is ' + str(array.time())

# Scan 152 = No0153
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0153.'
subarray.setSwitches(mjdStart + 6341*second, mjdStart+6359*second, obsCode+'_'+stnCode+'_'+'No0153')
if array.time() < mjdStart + (6359-10)*second:
  subarray.execute(mjdStart + 6354*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6359*second) + ' since array.time is ' + str(array.time())

# Scan 153 = No0154
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0154.'
subarray.setSwitches(mjdStart + 6371*second, mjdStart+6389*second, obsCode+'_'+stnCode+'_'+'No0154')
if array.time() < mjdStart + (6389-10)*second:
  subarray.execute(mjdStart + 6384*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6389*second) + ' since array.time is ' + str(array.time())

# Scan 154 = No0155
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0155.'
subarray.setSwitches(mjdStart + 6401*second, mjdStart+6419*second, obsCode+'_'+stnCode+'_'+'No0155')
if array.time() < mjdStart + (6419-10)*second:
  subarray.execute(mjdStart + 6414*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6419*second) + ' since array.time is ' + str(array.time())

# Scan 155 = No0156
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0156.'
subarray.setSwitches(mjdStart + 6431*second, mjdStart+6449*second, obsCode+'_'+stnCode+'_'+'No0156')
if array.time() < mjdStart + (6449-10)*second:
  subarray.execute(mjdStart + 6444*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6449*second) + ' since array.time is ' + str(array.time())

# Scan 156 = No0157
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0157.'
subarray.setSwitches(mjdStart + 6461*second, mjdStart+6479*second, obsCode+'_'+stnCode+'_'+'No0157')
if array.time() < mjdStart + (6479-10)*second:
  subarray.execute(mjdStart + 6474*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6479*second) + ' since array.time is ' + str(array.time())

# Scan 157 = No0158
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0158.'
subarray.setSwitches(mjdStart + 6491*second, mjdStart+6509*second, obsCode+'_'+stnCode+'_'+'No0158')
if array.time() < mjdStart + (6509-10)*second:
  subarray.execute(mjdStart + 6504*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6509*second) + ' since array.time is ' + str(array.time())

# Scan 158 = No0159
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0159.'
subarray.setSwitches(mjdStart + 6521*second, mjdStart+6538*second, obsCode+'_'+stnCode+'_'+'No0159')
if array.time() < mjdStart + (6538-10)*second:
  subarray.execute(mjdStart + 6533*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6538*second) + ' since array.time is ' + str(array.time())

# Scan 159 = No0160
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0160.'
subarray.setSwitches(mjdStart + 6550*second, mjdStart+6568*second, obsCode+'_'+stnCode+'_'+'No0160')
if array.time() < mjdStart + (6568-10)*second:
  subarray.execute(mjdStart + 6563*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6568*second) + ' since array.time is ' + str(array.time())

# Scan 160 = No0161
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0161.'
subarray.setSwitches(mjdStart + 6582*second, mjdStart+6598*second, obsCode+'_'+stnCode+'_'+'No0161')
if array.time() < mjdStart + (6598-10)*second:
  subarray.execute(mjdStart + 6593*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6598*second) + ' since array.time is ' + str(array.time())

# Scan 161 = No0162
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0162.'
subarray.setSwitches(mjdStart + 6612*second, mjdStart+6628*second, obsCode+'_'+stnCode+'_'+'No0162')
if array.time() < mjdStart + (6628-10)*second:
  subarray.execute(mjdStart + 6623*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6628*second) + ' since array.time is ' + str(array.time())

# Scan 162 = No0163
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0163.'
subarray.setSwitches(mjdStart + 6642*second, mjdStart+6658*second, obsCode+'_'+stnCode+'_'+'No0163')
if array.time() < mjdStart + (6658-10)*second:
  subarray.execute(mjdStart + 6653*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6658*second) + ' since array.time is ' + str(array.time())

# Scan 163 = No0164
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0164.'
subarray.setSwitches(mjdStart + 6672*second, mjdStart+6688*second, obsCode+'_'+stnCode+'_'+'No0164')
if array.time() < mjdStart + (6688-10)*second:
  subarray.execute(mjdStart + 6683*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6688*second) + ' since array.time is ' + str(array.time())

# Scan 164 = No0165
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0165.'
subarray.setSwitches(mjdStart + 6702*second, mjdStart+6718*second, obsCode+'_'+stnCode+'_'+'No0165')
if array.time() < mjdStart + (6718-10)*second:
  subarray.execute(mjdStart + 6713*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6718*second) + ' since array.time is ' + str(array.time())

# Scan 165 = No0166
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0166.'
subarray.setSwitches(mjdStart + 6732*second, mjdStart+6748*second, obsCode+'_'+stnCode+'_'+'No0166')
if array.time() < mjdStart + (6748-10)*second:
  subarray.execute(mjdStart + 6743*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6748*second) + ' since array.time is ' + str(array.time())

# Scan 166 = No0167
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source17)
print 'Not a recording scan but still set switches for No0167.'
subarray.setSwitches(mjdStart + 6771*second, mjdStart+6842*second, obsCode+'_'+stnCode+'_'+'No0167')
if array.time() < mjdStart + (6842-10)*second:
  subarray.execute(mjdStart + 6837*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6842*second) + ' since array.time is ' + str(array.time())

# Scan 167 = No0168
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source19)
print 'Not a recording scan but still set switches for No0168.'
subarray.setSwitches(mjdStart + 6848*second, mjdStart+6908*second, obsCode+'_'+stnCode+'_'+'No0168')
if array.time() < mjdStart + (6908-10)*second:
  subarray.execute(mjdStart + 6903*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6908*second) + ' since array.time is ' + str(array.time())

# Scan 168 = No0169
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source18)
print 'Not a recording scan but still set switches for No0169.'
subarray.setSwitches(mjdStart + 6914*second, mjdStart+6974*second, obsCode+'_'+stnCode+'_'+'No0169')
if array.time() < mjdStart + (6974-10)*second:
  subarray.execute(mjdStart + 6969*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+6974*second) + ' since array.time is ' + str(array.time())

# Scan 169 = No0170
# changing to mode geodetic
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 4)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source39)
print 'Not a recording scan but still set switches for No0170.'
subarray.setSwitches(mjdStart + 7002*second, mjdStart+7088*second, obsCode+'_'+stnCode+'_'+'No0170')
if array.time() < mjdStart + (7088-10)*second:
  subarray.execute(mjdStart + 7083*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7088*second) + ' since array.time is ' + str(array.time())

# Scan 170 = No0171
subarray.setSource(source27)
print 'Not a recording scan but still set switches for No0171.'
subarray.setSwitches(mjdStart + 7108*second, mjdStart+7172*second, obsCode+'_'+stnCode+'_'+'No0171')
if array.time() < mjdStart + (7172-10)*second:
  subarray.execute(mjdStart + 7167*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7172*second) + ' since array.time is ' + str(array.time())

# Scan 171 = No0172
subarray.setSource(source40)
print 'Not a recording scan but still set switches for No0172.'
subarray.setSwitches(mjdStart + 7330*second, mjdStart+7395*second, obsCode+'_'+stnCode+'_'+'No0172')
if array.time() < mjdStart + (7395-10)*second:
  subarray.execute(mjdStart + 7390*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7395*second) + ' since array.time is ' + str(array.time())

# Scan 172 = No0173
subarray.setSource(source41)
print 'Not a recording scan but still set switches for No0173.'
subarray.setSwitches(mjdStart + 7464*second, mjdStart+7528*second, obsCode+'_'+stnCode+'_'+'No0173')
if array.time() < mjdStart + (7528-10)*second:
  subarray.execute(mjdStart + 7523*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7528*second) + ' since array.time is ' + str(array.time())

# Scan 173 = No0174
subarray.setSource(source42)
print 'Not a recording scan but still set switches for No0174.'
subarray.setSwitches(mjdStart + 7552*second, mjdStart+7627*second, obsCode+'_'+stnCode+'_'+'No0174')
if array.time() < mjdStart + (7627-10)*second:
  subarray.execute(mjdStart + 7622*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7627*second) + ' since array.time is ' + str(array.time())

# Scan 174 = No0175
subarray.setSource(source43)
print 'Not a recording scan but still set switches for No0175.'
subarray.setSwitches(mjdStart + 7668*second, mjdStart+7742*second, obsCode+'_'+stnCode+'_'+'No0175')
if array.time() < mjdStart + (7742-10)*second:
  subarray.execute(mjdStart + 7737*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7742*second) + ' since array.time is ' + str(array.time())

# Scan 175 = No0176
subarray.setSource(source31)
print 'Not a recording scan but still set switches for No0176.'
subarray.setSwitches(mjdStart + 7843*second, mjdStart+7908*second, obsCode+'_'+stnCode+'_'+'No0176')
if array.time() < mjdStart + (7908-10)*second:
  subarray.execute(mjdStart + 7903*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+7908*second) + ' since array.time is ' + str(array.time())

# Scan 176 = No0177
subarray.setSource(source35)
print 'Not a recording scan but still set switches for No0177.'
subarray.setSwitches(mjdStart + 7940*second, mjdStart+8023*second, obsCode+'_'+stnCode+'_'+'No0177')
if array.time() < mjdStart + (8023-10)*second:
  subarray.execute(mjdStart + 8018*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8023*second) + ' since array.time is ' + str(array.time())

# Scan 177 = No0178
subarray.setSource(source44)
print 'Not a recording scan but still set switches for No0178.'
subarray.setSwitches(mjdStart + 8095*second, mjdStart+8160*second, obsCode+'_'+stnCode+'_'+'No0178')
if array.time() < mjdStart + (8160-10)*second:
  subarray.execute(mjdStart + 8155*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8160*second) + ' since array.time is ' + str(array.time())

# Scan 178 = No0179
subarray.setSource(source37)
print 'Not a recording scan but still set switches for No0179.'
subarray.setSwitches(mjdStart + 8189*second, mjdStart+8275*second, obsCode+'_'+stnCode+'_'+'No0179')
if array.time() < mjdStart + (8275-10)*second:
  subarray.execute(mjdStart + 8270*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8275*second) + ' since array.time is ' + str(array.time())

# Scan 179 = No0180
subarray.setSource(source45)
print 'Not a recording scan but still set switches for No0180.'
subarray.setSwitches(mjdStart + 8504*second, mjdStart+8569*second, obsCode+'_'+stnCode+'_'+'No0180')
if array.time() < mjdStart + (8569-10)*second:
  subarray.execute(mjdStart + 8564*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8569*second) + ' since array.time is ' + str(array.time())

# Scan 180 = No0181
subarray.setSource(source46)
print 'Not a recording scan but still set switches for No0181.'
subarray.setSwitches(mjdStart + 8630*second, mjdStart+8695*second, obsCode+'_'+stnCode+'_'+'No0181')
if array.time() < mjdStart + (8695-10)*second:
  subarray.execute(mjdStart + 8690*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8695*second) + ' since array.time is ' + str(array.time())

# Scan 181 = No0182
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0182.'
subarray.setSwitches(mjdStart + 8816*second, mjdStart+8841*second, obsCode+'_'+stnCode+'_'+'No0182')
if array.time() < mjdStart + (8841-10)*second:
  subarray.execute(mjdStart + 8836*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8841*second) + ' since array.time is ' + str(array.time())

# Scan 182 = No0183
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0183.'
subarray.setSwitches(mjdStart + 8849*second, mjdStart+8871*second, obsCode+'_'+stnCode+'_'+'No0183')
if array.time() < mjdStart + (8871-10)*second:
  subarray.execute(mjdStart + 8866*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8871*second) + ' since array.time is ' + str(array.time())

# Scan 183 = No0184
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0184.'
subarray.setSwitches(mjdStart + 8879*second, mjdStart+8901*second, obsCode+'_'+stnCode+'_'+'No0184')
if array.time() < mjdStart + (8901-10)*second:
  subarray.execute(mjdStart + 8896*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8901*second) + ' since array.time is ' + str(array.time())

# Scan 184 = No0185
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0185.'
subarray.setSwitches(mjdStart + 8912*second, mjdStart+8931*second, obsCode+'_'+stnCode+'_'+'No0185')
if array.time() < mjdStart + (8931-10)*second:
  subarray.execute(mjdStart + 8926*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8931*second) + ' since array.time is ' + str(array.time())

# Scan 185 = No0186
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0186.'
subarray.setSwitches(mjdStart + 8942*second, mjdStart+8961*second, obsCode+'_'+stnCode+'_'+'No0186')
if array.time() < mjdStart + (8961-10)*second:
  subarray.execute(mjdStart + 8956*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8961*second) + ' since array.time is ' + str(array.time())

# Scan 186 = No0187
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0187.'
subarray.setSwitches(mjdStart + 8971*second, mjdStart+8991*second, obsCode+'_'+stnCode+'_'+'No0187')
if array.time() < mjdStart + (8991-10)*second:
  subarray.execute(mjdStart + 8986*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+8991*second) + ' since array.time is ' + str(array.time())

# Scan 187 = No0188
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0188.'
subarray.setSwitches(mjdStart + 9001*second, mjdStart+9021*second, obsCode+'_'+stnCode+'_'+'No0188')
if array.time() < mjdStart + (9021-10)*second:
  subarray.execute(mjdStart + 9016*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9021*second) + ' since array.time is ' + str(array.time())

# Scan 188 = No0189
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0189.'
subarray.setSwitches(mjdStart + 9029*second, mjdStart+9051*second, obsCode+'_'+stnCode+'_'+'No0189')
if array.time() < mjdStart + (9051-10)*second:
  subarray.execute(mjdStart + 9046*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9051*second) + ' since array.time is ' + str(array.time())

# Scan 189 = No0190
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0190.'
subarray.setSwitches(mjdStart + 9059*second, mjdStart+9081*second, obsCode+'_'+stnCode+'_'+'No0190')
if array.time() < mjdStart + (9081-10)*second:
  subarray.execute(mjdStart + 9076*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9081*second) + ' since array.time is ' + str(array.time())

# Scan 190 = No0191
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0191.'
subarray.setSwitches(mjdStart + 9092*second, mjdStart+9111*second, obsCode+'_'+stnCode+'_'+'No0191')
if array.time() < mjdStart + (9111-10)*second:
  subarray.execute(mjdStart + 9106*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9111*second) + ' since array.time is ' + str(array.time())

# Scan 191 = No0192
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0192.'
subarray.setSwitches(mjdStart + 9122*second, mjdStart+9141*second, obsCode+'_'+stnCode+'_'+'No0192')
if array.time() < mjdStart + (9141-10)*second:
  subarray.execute(mjdStart + 9136*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9141*second) + ' since array.time is ' + str(array.time())

# Scan 192 = No0193
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0193.'
subarray.setSwitches(mjdStart + 9151*second, mjdStart+9170*second, obsCode+'_'+stnCode+'_'+'No0193')
if array.time() < mjdStart + (9170-10)*second:
  subarray.execute(mjdStart + 9165*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9170*second) + ' since array.time is ' + str(array.time())

# Scan 193 = No0194
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0194.'
subarray.setSwitches(mjdStart + 9180*second, mjdStart+9200*second, obsCode+'_'+stnCode+'_'+'No0194')
if array.time() < mjdStart + (9200-10)*second:
  subarray.execute(mjdStart + 9195*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9200*second) + ' since array.time is ' + str(array.time())

# Scan 194 = No0195
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0195.'
subarray.setSwitches(mjdStart + 9208*second, mjdStart+9230*second, obsCode+'_'+stnCode+'_'+'No0195')
if array.time() < mjdStart + (9230-10)*second:
  subarray.execute(mjdStart + 9225*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9230*second) + ' since array.time is ' + str(array.time())

# Scan 195 = No0196
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0196.'
subarray.setSwitches(mjdStart + 9238*second, mjdStart+9260*second, obsCode+'_'+stnCode+'_'+'No0196')
if array.time() < mjdStart + (9260-10)*second:
  subarray.execute(mjdStart + 9255*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9260*second) + ' since array.time is ' + str(array.time())

# Scan 196 = No0197
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0197.'
subarray.setSwitches(mjdStart + 9271*second, mjdStart+9290*second, obsCode+'_'+stnCode+'_'+'No0197')
if array.time() < mjdStart + (9290-10)*second:
  subarray.execute(mjdStart + 9285*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9290*second) + ' since array.time is ' + str(array.time())

# Scan 197 = No0198
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0198.'
subarray.setSwitches(mjdStart + 9301*second, mjdStart+9320*second, obsCode+'_'+stnCode+'_'+'No0198')
if array.time() < mjdStart + (9320-10)*second:
  subarray.execute(mjdStart + 9315*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9320*second) + ' since array.time is ' + str(array.time())

# Scan 198 = No0199
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0199.'
subarray.setSwitches(mjdStart + 9331*second, mjdStart+9350*second, obsCode+'_'+stnCode+'_'+'No0199')
if array.time() < mjdStart + (9350-10)*second:
  subarray.execute(mjdStart + 9345*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9350*second) + ' since array.time is ' + str(array.time())

# Scan 199 = No0200
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0200.'
subarray.setSwitches(mjdStart + 9360*second, mjdStart+9380*second, obsCode+'_'+stnCode+'_'+'No0200')
if array.time() < mjdStart + (9380-10)*second:
  subarray.execute(mjdStart + 9375*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9380*second) + ' since array.time is ' + str(array.time())

# Scan 200 = No0201
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0201.'
subarray.setSwitches(mjdStart + 9388*second, mjdStart+9410*second, obsCode+'_'+stnCode+'_'+'No0201')
if array.time() < mjdStart + (9410-10)*second:
  subarray.execute(mjdStart + 9405*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9410*second) + ' since array.time is ' + str(array.time())

# Scan 201 = No0202
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0202.'
subarray.setSwitches(mjdStart + 9418*second, mjdStart+9440*second, obsCode+'_'+stnCode+'_'+'No0202')
if array.time() < mjdStart + (9440-10)*second:
  subarray.execute(mjdStart + 9435*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9440*second) + ' since array.time is ' + str(array.time())

# Scan 202 = No0203
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0203.'
subarray.setSwitches(mjdStart + 9451*second, mjdStart+9470*second, obsCode+'_'+stnCode+'_'+'No0203')
if array.time() < mjdStart + (9470-10)*second:
  subarray.execute(mjdStart + 9465*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9470*second) + ' since array.time is ' + str(array.time())

# Scan 203 = No0204
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0204.'
subarray.setSwitches(mjdStart + 9481*second, mjdStart+9500*second, obsCode+'_'+stnCode+'_'+'No0204')
if array.time() < mjdStart + (9500-10)*second:
  subarray.execute(mjdStart + 9495*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9500*second) + ' since array.time is ' + str(array.time())

# Scan 204 = No0205
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0205.'
subarray.setSwitches(mjdStart + 9511*second, mjdStart+9529*second, obsCode+'_'+stnCode+'_'+'No0205')
if array.time() < mjdStart + (9529-10)*second:
  subarray.execute(mjdStart + 9524*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9529*second) + ' since array.time is ' + str(array.time())

# Scan 205 = No0206
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0206.'
subarray.setSwitches(mjdStart + 9540*second, mjdStart+9559*second, obsCode+'_'+stnCode+'_'+'No0206')
if array.time() < mjdStart + (9559-10)*second:
  subarray.execute(mjdStart + 9554*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9559*second) + ' since array.time is ' + str(array.time())

# Scan 206 = No0207
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0207.'
subarray.setSwitches(mjdStart + 9567*second, mjdStart+9584*second, obsCode+'_'+stnCode+'_'+'No0207')
if array.time() < mjdStart + (9584-10)*second:
  subarray.execute(mjdStart + 9579*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9584*second) + ' since array.time is ' + str(array.time())

# Scan 207 = No0208
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0208.'
subarray.setSwitches(mjdStart + 9595*second, mjdStart+9614*second, obsCode+'_'+stnCode+'_'+'No0208')
if array.time() < mjdStart + (9614-10)*second:
  subarray.execute(mjdStart + 9609*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9614*second) + ' since array.time is ' + str(array.time())

# Scan 208 = No0209
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0209.'
subarray.setSwitches(mjdStart + 9625*second, mjdStart+9644*second, obsCode+'_'+stnCode+'_'+'No0209')
if array.time() < mjdStart + (9644-10)*second:
  subarray.execute(mjdStart + 9639*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9644*second) + ' since array.time is ' + str(array.time())

# Scan 209 = No0210
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0210.'
subarray.setSwitches(mjdStart + 9655*second, mjdStart+9674*second, obsCode+'_'+stnCode+'_'+'No0210')
if array.time() < mjdStart + (9674-10)*second:
  subarray.execute(mjdStart + 9669*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9674*second) + ' since array.time is ' + str(array.time())

# Scan 210 = No0211
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0211.'
subarray.setSwitches(mjdStart + 9685*second, mjdStart+9704*second, obsCode+'_'+stnCode+'_'+'No0211')
if array.time() < mjdStart + (9704-10)*second:
  subarray.execute(mjdStart + 9699*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9704*second) + ' since array.time is ' + str(array.time())

# Scan 211 = No0212
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0212.'
subarray.setSwitches(mjdStart + 9715*second, mjdStart+9734*second, obsCode+'_'+stnCode+'_'+'No0212')
if array.time() < mjdStart + (9734-10)*second:
  subarray.execute(mjdStart + 9729*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9734*second) + ' since array.time is ' + str(array.time())

# Scan 212 = No0213
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0213.'
subarray.setSwitches(mjdStart + 9745*second, mjdStart+9764*second, obsCode+'_'+stnCode+'_'+'No0213')
if array.time() < mjdStart + (9764-10)*second:
  subarray.execute(mjdStart + 9759*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9764*second) + ' since array.time is ' + str(array.time())

# Scan 213 = No0214
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0214.'
subarray.setSwitches(mjdStart + 9776*second, mjdStart+9794*second, obsCode+'_'+stnCode+'_'+'No0214')
if array.time() < mjdStart + (9794-10)*second:
  subarray.execute(mjdStart + 9789*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9794*second) + ' since array.time is ' + str(array.time())

# Scan 214 = No0215
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0215.'
subarray.setSwitches(mjdStart + 9806*second, mjdStart+9824*second, obsCode+'_'+stnCode+'_'+'No0215')
if array.time() < mjdStart + (9824-10)*second:
  subarray.execute(mjdStart + 9819*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9824*second) + ' since array.time is ' + str(array.time())

# Scan 215 = No0216
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0216.'
subarray.setSwitches(mjdStart + 9836*second, mjdStart+9854*second, obsCode+'_'+stnCode+'_'+'No0216')
if array.time() < mjdStart + (9854-10)*second:
  subarray.execute(mjdStart + 9849*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9854*second) + ' since array.time is ' + str(array.time())

# Scan 216 = No0217
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0217.'
subarray.setSwitches(mjdStart + 9866*second, mjdStart+9883*second, obsCode+'_'+stnCode+'_'+'No0217')
if array.time() < mjdStart + (9883-10)*second:
  subarray.execute(mjdStart + 9878*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9883*second) + ' since array.time is ' + str(array.time())

# Scan 217 = No0218
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0218.'
subarray.setSwitches(mjdStart + 9895*second, mjdStart+9913*second, obsCode+'_'+stnCode+'_'+'No0218')
if array.time() < mjdStart + (9913-10)*second:
  subarray.execute(mjdStart + 9908*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9913*second) + ' since array.time is ' + str(array.time())

# Scan 218 = No0219
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0219.'
subarray.setSwitches(mjdStart + 9925*second, mjdStart+9943*second, obsCode+'_'+stnCode+'_'+'No0219')
if array.time() < mjdStart + (9943-10)*second:
  subarray.execute(mjdStart + 9938*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9943*second) + ' since array.time is ' + str(array.time())

# Scan 219 = No0220
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0220.'
subarray.setSwitches(mjdStart + 9956*second, mjdStart+9973*second, obsCode+'_'+stnCode+'_'+'No0220')
if array.time() < mjdStart + (9973-10)*second:
  subarray.execute(mjdStart + 9968*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+9973*second) + ' since array.time is ' + str(array.time())

# Scan 220 = No0221
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0221.'
subarray.setSwitches(mjdStart + 9986*second, mjdStart+10003*second, obsCode+'_'+stnCode+'_'+'No0221')
if array.time() < mjdStart + (10003-10)*second:
  subarray.execute(mjdStart + 9998*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10003*second) + ' since array.time is ' + str(array.time())

# Scan 221 = No0222
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0222.'
subarray.setSwitches(mjdStart + 10016*second, mjdStart+10033*second, obsCode+'_'+stnCode+'_'+'No0222')
if array.time() < mjdStart + (10033-10)*second:
  subarray.execute(mjdStart + 10028*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10033*second) + ' since array.time is ' + str(array.time())

# Scan 222 = No0223
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0223.'
subarray.setSwitches(mjdStart + 10046*second, mjdStart+10063*second, obsCode+'_'+stnCode+'_'+'No0223')
if array.time() < mjdStart + (10063-10)*second:
  subarray.execute(mjdStart + 10058*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10063*second) + ' since array.time is ' + str(array.time())

# Scan 223 = No0224
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0224.'
subarray.setSwitches(mjdStart + 10076*second, mjdStart+10093*second, obsCode+'_'+stnCode+'_'+'No0224')
if array.time() < mjdStart + (10093-10)*second:
  subarray.execute(mjdStart + 10088*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10093*second) + ' since array.time is ' + str(array.time())

# Scan 224 = No0225
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0225.'
subarray.setSwitches(mjdStart + 10106*second, mjdStart+10123*second, obsCode+'_'+stnCode+'_'+'No0225')
if array.time() < mjdStart + (10123-10)*second:
  subarray.execute(mjdStart + 10118*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10123*second) + ' since array.time is ' + str(array.time())

# Scan 225 = No0226
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0226.'
subarray.setSwitches(mjdStart + 10135*second, mjdStart+10153*second, obsCode+'_'+stnCode+'_'+'No0226')
if array.time() < mjdStart + (10153-10)*second:
  subarray.execute(mjdStart + 10148*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10153*second) + ' since array.time is ' + str(array.time())

# Scan 226 = No0227
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0227.'
subarray.setSwitches(mjdStart + 10165*second, mjdStart+10183*second, obsCode+'_'+stnCode+'_'+'No0227')
if array.time() < mjdStart + (10183-10)*second:
  subarray.execute(mjdStart + 10178*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10183*second) + ' since array.time is ' + str(array.time())

# Scan 227 = No0228
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0228.'
subarray.setSwitches(mjdStart + 10195*second, mjdStart+10213*second, obsCode+'_'+stnCode+'_'+'No0228')
if array.time() < mjdStart + (10213-10)*second:
  subarray.execute(mjdStart + 10208*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10213*second) + ' since array.time is ' + str(array.time())

# Scan 228 = No0229
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0229.'
subarray.setSwitches(mjdStart + 10225*second, mjdStart+10242*second, obsCode+'_'+stnCode+'_'+'No0229')
if array.time() < mjdStart + (10242-10)*second:
  subarray.execute(mjdStart + 10237*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10242*second) + ' since array.time is ' + str(array.time())

# Scan 229 = No0230
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0230.'
subarray.setSwitches(mjdStart + 10254*second, mjdStart+10272*second, obsCode+'_'+stnCode+'_'+'No0230')
if array.time() < mjdStart + (10272-10)*second:
  subarray.execute(mjdStart + 10267*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10272*second) + ' since array.time is ' + str(array.time())

# Scan 230 = No0231
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0231.'
subarray.setSwitches(mjdStart + 10284*second, mjdStart+10302*second, obsCode+'_'+stnCode+'_'+'No0231')
if array.time() < mjdStart + (10302-10)*second:
  subarray.execute(mjdStart + 10297*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10302*second) + ' since array.time is ' + str(array.time())

# Scan 231 = No0232
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0232.'
subarray.setSwitches(mjdStart + 10313*second, mjdStart+10329*second, obsCode+'_'+stnCode+'_'+'No0232')
if array.time() < mjdStart + (10329-10)*second:
  subarray.execute(mjdStart + 10324*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10329*second) + ' since array.time is ' + str(array.time())

# Scan 232 = No0233
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0233.'
subarray.setSwitches(mjdStart + 10340*second, mjdStart+10359*second, obsCode+'_'+stnCode+'_'+'No0233')
if array.time() < mjdStart + (10359-10)*second:
  subarray.execute(mjdStart + 10354*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10359*second) + ' since array.time is ' + str(array.time())

# Scan 233 = No0234
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0234.'
subarray.setSwitches(mjdStart + 10370*second, mjdStart+10389*second, obsCode+'_'+stnCode+'_'+'No0234')
if array.time() < mjdStart + (10389-10)*second:
  subarray.execute(mjdStart + 10384*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10389*second) + ' since array.time is ' + str(array.time())

# Scan 234 = No0235
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0235.'
subarray.setSwitches(mjdStart + 10400*second, mjdStart+10419*second, obsCode+'_'+stnCode+'_'+'No0235')
if array.time() < mjdStart + (10419-10)*second:
  subarray.execute(mjdStart + 10414*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10419*second) + ' since array.time is ' + str(array.time())

# Scan 235 = No0236
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0236.'
subarray.setSwitches(mjdStart + 10430*second, mjdStart+10449*second, obsCode+'_'+stnCode+'_'+'No0236')
if array.time() < mjdStart + (10449-10)*second:
  subarray.execute(mjdStart + 10444*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10449*second) + ' since array.time is ' + str(array.time())

# Scan 236 = No0237
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0237.'
subarray.setSwitches(mjdStart + 10460*second, mjdStart+10478*second, obsCode+'_'+stnCode+'_'+'No0237')
if array.time() < mjdStart + (10478-10)*second:
  subarray.execute(mjdStart + 10473*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10478*second) + ' since array.time is ' + str(array.time())

# Scan 237 = No0238
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0238.'
subarray.setSwitches(mjdStart + 10489*second, mjdStart+10508*second, obsCode+'_'+stnCode+'_'+'No0238')
if array.time() < mjdStart + (10508-10)*second:
  subarray.execute(mjdStart + 10503*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10508*second) + ' since array.time is ' + str(array.time())

# Scan 238 = No0239
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0239.'
subarray.setSwitches(mjdStart + 10519*second, mjdStart+10538*second, obsCode+'_'+stnCode+'_'+'No0239')
if array.time() < mjdStart + (10538-10)*second:
  subarray.execute(mjdStart + 10533*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10538*second) + ' since array.time is ' + str(array.time())

# Scan 239 = No0240
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0240.'
subarray.setSwitches(mjdStart + 10549*second, mjdStart+10568*second, obsCode+'_'+stnCode+'_'+'No0240')
if array.time() < mjdStart + (10568-10)*second:
  subarray.execute(mjdStart + 10563*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10568*second) + ' since array.time is ' + str(array.time())

# Scan 240 = No0241
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0241.'
subarray.setSwitches(mjdStart + 10579*second, mjdStart+10598*second, obsCode+'_'+stnCode+'_'+'No0241')
if array.time() < mjdStart + (10598-10)*second:
  subarray.execute(mjdStart + 10593*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10598*second) + ' since array.time is ' + str(array.time())

# Scan 241 = No0242
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0242.'
subarray.setSwitches(mjdStart + 10609*second, mjdStart+10628*second, obsCode+'_'+stnCode+'_'+'No0242')
if array.time() < mjdStart + (10628-10)*second:
  subarray.execute(mjdStart + 10623*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10628*second) + ' since array.time is ' + str(array.time())

# Scan 242 = No0243
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0243.'
subarray.setSwitches(mjdStart + 10639*second, mjdStart+10658*second, obsCode+'_'+stnCode+'_'+'No0243')
if array.time() < mjdStart + (10658-10)*second:
  subarray.execute(mjdStart + 10653*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10658*second) + ' since array.time is ' + str(array.time())

# Scan 243 = No0244
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0244.'
subarray.setSwitches(mjdStart + 10669*second, mjdStart+10688*second, obsCode+'_'+stnCode+'_'+'No0244')
if array.time() < mjdStart + (10688-10)*second:
  subarray.execute(mjdStart + 10683*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10688*second) + ' since array.time is ' + str(array.time())

# Scan 244 = No0245
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0245.'
subarray.setSwitches(mjdStart + 10699*second, mjdStart+10718*second, obsCode+'_'+stnCode+'_'+'No0245')
if array.time() < mjdStart + (10718-10)*second:
  subarray.execute(mjdStart + 10713*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10718*second) + ' since array.time is ' + str(array.time())

# Scan 245 = No0246
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0246.'
subarray.setSwitches(mjdStart + 10729*second, mjdStart+10748*second, obsCode+'_'+stnCode+'_'+'No0246')
if array.time() < mjdStart + (10748-10)*second:
  subarray.execute(mjdStart + 10743*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10748*second) + ' since array.time is ' + str(array.time())

# Scan 246 = No0247
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0247.'
subarray.setSwitches(mjdStart + 10759*second, mjdStart+10778*second, obsCode+'_'+stnCode+'_'+'No0247')
if array.time() < mjdStart + (10778-10)*second:
  subarray.execute(mjdStart + 10773*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10778*second) + ' since array.time is ' + str(array.time())

# Scan 247 = No0248
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0248.'
subarray.setSwitches(mjdStart + 10789*second, mjdStart+10808*second, obsCode+'_'+stnCode+'_'+'No0248')
if array.time() < mjdStart + (10808-10)*second:
  subarray.execute(mjdStart + 10803*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10808*second) + ' since array.time is ' + str(array.time())

# Scan 248 = No0249
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0249.'
subarray.setSwitches(mjdStart + 10819*second, mjdStart+10837*second, obsCode+'_'+stnCode+'_'+'No0249')
if array.time() < mjdStart + (10837-10)*second:
  subarray.execute(mjdStart + 10832*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10837*second) + ' since array.time is ' + str(array.time())

# Scan 249 = No0250
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0250.'
subarray.setSwitches(mjdStart + 10848*second, mjdStart+10867*second, obsCode+'_'+stnCode+'_'+'No0250')
if array.time() < mjdStart + (10867-10)*second:
  subarray.execute(mjdStart + 10862*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10867*second) + ' since array.time is ' + str(array.time())

# Scan 250 = No0251
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0251.'
subarray.setSwitches(mjdStart + 10881*second, mjdStart+10897*second, obsCode+'_'+stnCode+'_'+'No0251')
if array.time() < mjdStart + (10897-10)*second:
  subarray.execute(mjdStart + 10892*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10897*second) + ' since array.time is ' + str(array.time())

# Scan 251 = No0252
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0252.'
subarray.setSwitches(mjdStart + 10911*second, mjdStart+10927*second, obsCode+'_'+stnCode+'_'+'No0252')
if array.time() < mjdStart + (10927-10)*second:
  subarray.execute(mjdStart + 10922*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10927*second) + ' since array.time is ' + str(array.time())

# Scan 252 = No0253
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0253.'
subarray.setSwitches(mjdStart + 10941*second, mjdStart+10957*second, obsCode+'_'+stnCode+'_'+'No0253')
if array.time() < mjdStart + (10957-10)*second:
  subarray.execute(mjdStart + 10952*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10957*second) + ' since array.time is ' + str(array.time())

# Scan 253 = No0254
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0254.'
subarray.setSwitches(mjdStart + 10971*second, mjdStart+10987*second, obsCode+'_'+stnCode+'_'+'No0254')
if array.time() < mjdStart + (10987-10)*second:
  subarray.execute(mjdStart + 10982*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+10987*second) + ' since array.time is ' + str(array.time())

# Scan 254 = No0255
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0255.'
subarray.setSwitches(mjdStart + 11001*second, mjdStart+11017*second, obsCode+'_'+stnCode+'_'+'No0255')
if array.time() < mjdStart + (11017-10)*second:
  subarray.execute(mjdStart + 11012*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11017*second) + ' since array.time is ' + str(array.time())

# Scan 255 = No0256
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0256.'
subarray.setSwitches(mjdStart + 11031*second, mjdStart+11047*second, obsCode+'_'+stnCode+'_'+'No0256')
if array.time() < mjdStart + (11047-10)*second:
  subarray.execute(mjdStart + 11042*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11047*second) + ' since array.time is ' + str(array.time())

# Scan 256 = No0257
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0257.'
subarray.setSwitches(mjdStart + 11058*second, mjdStart+11078*second, obsCode+'_'+stnCode+'_'+'No0257')
if array.time() < mjdStart + (11078-10)*second:
  subarray.execute(mjdStart + 11073*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11078*second) + ' since array.time is ' + str(array.time())

# Scan 257 = No0258
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0258.'
subarray.setSwitches(mjdStart + 11086*second, mjdStart+11108*second, obsCode+'_'+stnCode+'_'+'No0258')
if array.time() < mjdStart + (11108-10)*second:
  subarray.execute(mjdStart + 11103*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11108*second) + ' since array.time is ' + str(array.time())

# Scan 258 = No0259
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0259.'
subarray.setSwitches(mjdStart + 11117*second, mjdStart+11138*second, obsCode+'_'+stnCode+'_'+'No0259')
if array.time() < mjdStart + (11138-10)*second:
  subarray.execute(mjdStart + 11133*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11138*second) + ' since array.time is ' + str(array.time())

# Scan 259 = No0260
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0260.'
subarray.setSwitches(mjdStart + 11150*second, mjdStart+11168*second, obsCode+'_'+stnCode+'_'+'No0260')
if array.time() < mjdStart + (11168-10)*second:
  subarray.execute(mjdStart + 11163*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11168*second) + ' since array.time is ' + str(array.time())

# Scan 260 = No0261
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0261.'
subarray.setSwitches(mjdStart + 11180*second, mjdStart+11198*second, obsCode+'_'+stnCode+'_'+'No0261')
if array.time() < mjdStart + (11198-10)*second:
  subarray.execute(mjdStart + 11193*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11198*second) + ' since array.time is ' + str(array.time())

# Scan 261 = No0262
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0262.'
subarray.setSwitches(mjdStart + 11209*second, mjdStart+11227*second, obsCode+'_'+stnCode+'_'+'No0262')
if array.time() < mjdStart + (11227-10)*second:
  subarray.execute(mjdStart + 11222*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11227*second) + ' since array.time is ' + str(array.time())

# Scan 262 = No0263
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0263.'
subarray.setSwitches(mjdStart + 11238*second, mjdStart+11257*second, obsCode+'_'+stnCode+'_'+'No0263')
if array.time() < mjdStart + (11257-10)*second:
  subarray.execute(mjdStart + 11252*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11257*second) + ' since array.time is ' + str(array.time())

# Scan 263 = No0264
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0264.'
subarray.setSwitches(mjdStart + 11265*second, mjdStart+11287*second, obsCode+'_'+stnCode+'_'+'No0264')
if array.time() < mjdStart + (11287-10)*second:
  subarray.execute(mjdStart + 11282*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11287*second) + ' since array.time is ' + str(array.time())

# Scan 264 = No0265
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0265.'
subarray.setSwitches(mjdStart + 11296*second, mjdStart+11317*second, obsCode+'_'+stnCode+'_'+'No0265')
if array.time() < mjdStart + (11317-10)*second:
  subarray.execute(mjdStart + 11312*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11317*second) + ' since array.time is ' + str(array.time())

# Scan 265 = No0266
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0266.'
subarray.setSwitches(mjdStart + 11329*second, mjdStart+11347*second, obsCode+'_'+stnCode+'_'+'No0266')
if array.time() < mjdStart + (11347-10)*second:
  subarray.execute(mjdStart + 11342*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11347*second) + ' since array.time is ' + str(array.time())

# Scan 266 = No0267
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0267.'
subarray.setSwitches(mjdStart + 11359*second, mjdStart+11377*second, obsCode+'_'+stnCode+'_'+'No0267')
if array.time() < mjdStart + (11377-10)*second:
  subarray.execute(mjdStart + 11372*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11377*second) + ' since array.time is ' + str(array.time())

# Scan 267 = No0268
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0268.'
subarray.setSwitches(mjdStart + 11388*second, mjdStart+11407*second, obsCode+'_'+stnCode+'_'+'No0268')
if array.time() < mjdStart + (11407-10)*second:
  subarray.execute(mjdStart + 11402*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11407*second) + ' since array.time is ' + str(array.time())

# Scan 268 = No0269
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0269.'
subarray.setSwitches(mjdStart + 11418*second, mjdStart+11437*second, obsCode+'_'+stnCode+'_'+'No0269')
if array.time() < mjdStart + (11437-10)*second:
  subarray.execute(mjdStart + 11432*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11437*second) + ' since array.time is ' + str(array.time())

# Scan 269 = No0270
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0270.'
subarray.setSwitches(mjdStart + 11446*second, mjdStart+11467*second, obsCode+'_'+stnCode+'_'+'No0270')
if array.time() < mjdStart + (11467-10)*second:
  subarray.execute(mjdStart + 11462*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11467*second) + ' since array.time is ' + str(array.time())

# Scan 270 = No0271
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0271.'
subarray.setSwitches(mjdStart + 11476*second, mjdStart+11497*second, obsCode+'_'+stnCode+'_'+'No0271')
if array.time() < mjdStart + (11497-10)*second:
  subarray.execute(mjdStart + 11492*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11497*second) + ' since array.time is ' + str(array.time())

# Scan 271 = No0272
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0272.'
subarray.setSwitches(mjdStart + 11509*second, mjdStart+11527*second, obsCode+'_'+stnCode+'_'+'No0272')
if array.time() < mjdStart + (11527-10)*second:
  subarray.execute(mjdStart + 11522*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11527*second) + ' since array.time is ' + str(array.time())

# Scan 272 = No0273
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0273.'
subarray.setSwitches(mjdStart + 11539*second, mjdStart+11557*second, obsCode+'_'+stnCode+'_'+'No0273')
if array.time() < mjdStart + (11557-10)*second:
  subarray.execute(mjdStart + 11552*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11557*second) + ' since array.time is ' + str(array.time())

# Scan 273 = No0274
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0274.'
subarray.setSwitches(mjdStart + 11568*second, mjdStart+11586*second, obsCode+'_'+stnCode+'_'+'No0274')
if array.time() < mjdStart + (11586-10)*second:
  subarray.execute(mjdStart + 11581*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11586*second) + ' since array.time is ' + str(array.time())

# Scan 274 = No0275
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0275.'
subarray.setSwitches(mjdStart + 11597*second, mjdStart+11616*second, obsCode+'_'+stnCode+'_'+'No0275')
if array.time() < mjdStart + (11616-10)*second:
  subarray.execute(mjdStart + 11611*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11616*second) + ' since array.time is ' + str(array.time())

# Scan 275 = No0276
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0276.'
subarray.setSwitches(mjdStart + 11625*second, mjdStart+11646*second, obsCode+'_'+stnCode+'_'+'No0276')
if array.time() < mjdStart + (11646-10)*second:
  subarray.execute(mjdStart + 11641*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11646*second) + ' since array.time is ' + str(array.time())

# Scan 276 = No0277
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0277.'
subarray.setSwitches(mjdStart + 11655*second, mjdStart+11676*second, obsCode+'_'+stnCode+'_'+'No0277')
if array.time() < mjdStart + (11676-10)*second:
  subarray.execute(mjdStart + 11671*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11676*second) + ' since array.time is ' + str(array.time())

# Scan 277 = No0278
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0278.'
subarray.setSwitches(mjdStart + 11688*second, mjdStart+11706*second, obsCode+'_'+stnCode+'_'+'No0278')
if array.time() < mjdStart + (11706-10)*second:
  subarray.execute(mjdStart + 11701*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11706*second) + ' since array.time is ' + str(array.time())

# Scan 278 = No0279
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0279.'
subarray.setSwitches(mjdStart + 11718*second, mjdStart+11736*second, obsCode+'_'+stnCode+'_'+'No0279')
if array.time() < mjdStart + (11736-10)*second:
  subarray.execute(mjdStart + 11731*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11736*second) + ' since array.time is ' + str(array.time())

# Scan 279 = No0280
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0280.'
subarray.setSwitches(mjdStart + 11747*second, mjdStart+11766*second, obsCode+'_'+stnCode+'_'+'No0280')
if array.time() < mjdStart + (11766-10)*second:
  subarray.execute(mjdStart + 11761*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11766*second) + ' since array.time is ' + str(array.time())

# Scan 280 = No0281
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0281.'
subarray.setSwitches(mjdStart + 11777*second, mjdStart+11796*second, obsCode+'_'+stnCode+'_'+'No0281')
if array.time() < mjdStart + (11796-10)*second:
  subarray.execute(mjdStart + 11791*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11796*second) + ' since array.time is ' + str(array.time())

# Scan 281 = No0282
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0282.'
subarray.setSwitches(mjdStart + 11804*second, mjdStart+11821*second, obsCode+'_'+stnCode+'_'+'No0282')
if array.time() < mjdStart + (11821-10)*second:
  subarray.execute(mjdStart + 11816*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11821*second) + ' since array.time is ' + str(array.time())

# Scan 282 = No0283
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0283.'
subarray.setSwitches(mjdStart + 11831*second, mjdStart+11851*second, obsCode+'_'+stnCode+'_'+'No0283')
if array.time() < mjdStart + (11851-10)*second:
  subarray.execute(mjdStart + 11846*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11851*second) + ' since array.time is ' + str(array.time())

# Scan 283 = No0284
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0284.'
subarray.setSwitches(mjdStart + 11861*second, mjdStart+11881*second, obsCode+'_'+stnCode+'_'+'No0284')
if array.time() < mjdStart + (11881-10)*second:
  subarray.execute(mjdStart + 11876*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11881*second) + ' since array.time is ' + str(array.time())

# Scan 284 = No0285
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0285.'
subarray.setSwitches(mjdStart + 11891*second, mjdStart+11911*second, obsCode+'_'+stnCode+'_'+'No0285')
if array.time() < mjdStart + (11911-10)*second:
  subarray.execute(mjdStart + 11906*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11911*second) + ' since array.time is ' + str(array.time())

# Scan 285 = No0286
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0286.'
subarray.setSwitches(mjdStart + 11921*second, mjdStart+11940*second, obsCode+'_'+stnCode+'_'+'No0286')
if array.time() < mjdStart + (11940-10)*second:
  subarray.execute(mjdStart + 11935*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11940*second) + ' since array.time is ' + str(array.time())

# Scan 286 = No0287
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0287.'
subarray.setSwitches(mjdStart + 11950*second, mjdStart+11970*second, obsCode+'_'+stnCode+'_'+'No0287')
if array.time() < mjdStart + (11970-10)*second:
  subarray.execute(mjdStart + 11965*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+11970*second) + ' since array.time is ' + str(array.time())

# Scan 287 = No0288
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0288.'
subarray.setSwitches(mjdStart + 11980*second, mjdStart+12000*second, obsCode+'_'+stnCode+'_'+'No0288')
if array.time() < mjdStart + (12000-10)*second:
  subarray.execute(mjdStart + 11995*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12000*second) + ' since array.time is ' + str(array.time())

# Scan 288 = No0289
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0289.'
subarray.setSwitches(mjdStart + 12013*second, mjdStart+12030*second, obsCode+'_'+stnCode+'_'+'No0289')
if array.time() < mjdStart + (12030-10)*second:
  subarray.execute(mjdStart + 12025*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12030*second) + ' since array.time is ' + str(array.time())

# Scan 289 = No0290
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0290.'
subarray.setSwitches(mjdStart + 12042*second, mjdStart+12060*second, obsCode+'_'+stnCode+'_'+'No0290')
if array.time() < mjdStart + (12060-10)*second:
  subarray.execute(mjdStart + 12055*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12060*second) + ' since array.time is ' + str(array.time())

# Scan 290 = No0291
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0291.'
subarray.setSwitches(mjdStart + 12073*second, mjdStart+12090*second, obsCode+'_'+stnCode+'_'+'No0291')
if array.time() < mjdStart + (12090-10)*second:
  subarray.execute(mjdStart + 12085*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12090*second) + ' since array.time is ' + str(array.time())

# Scan 291 = No0292
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0292.'
subarray.setSwitches(mjdStart + 12102*second, mjdStart+12120*second, obsCode+'_'+stnCode+'_'+'No0292')
if array.time() < mjdStart + (12120-10)*second:
  subarray.execute(mjdStart + 12115*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12120*second) + ' since array.time is ' + str(array.time())

# Scan 292 = No0293
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0293.'
subarray.setSwitches(mjdStart + 12132*second, mjdStart+12150*second, obsCode+'_'+stnCode+'_'+'No0293')
if array.time() < mjdStart + (12150-10)*second:
  subarray.execute(mjdStart + 12145*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12150*second) + ' since array.time is ' + str(array.time())

# Scan 293 = No0294
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0294.'
subarray.setSwitches(mjdStart + 12162*second, mjdStart+12180*second, obsCode+'_'+stnCode+'_'+'No0294')
if array.time() < mjdStart + (12180-10)*second:
  subarray.execute(mjdStart + 12175*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12180*second) + ' since array.time is ' + str(array.time())

# Scan 294 = No0295
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0295.'
subarray.setSwitches(mjdStart + 12194*second, mjdStart+12210*second, obsCode+'_'+stnCode+'_'+'No0295')
if array.time() < mjdStart + (12210-10)*second:
  subarray.execute(mjdStart + 12205*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12210*second) + ' since array.time is ' + str(array.time())

# Scan 295 = No0296
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0296.'
subarray.setSwitches(mjdStart + 12224*second, mjdStart+12240*second, obsCode+'_'+stnCode+'_'+'No0296')
if array.time() < mjdStart + (12240-10)*second:
  subarray.execute(mjdStart + 12235*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12240*second) + ' since array.time is ' + str(array.time())

# Scan 296 = No0297
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0297.'
subarray.setSwitches(mjdStart + 12254*second, mjdStart+12270*second, obsCode+'_'+stnCode+'_'+'No0297')
if array.time() < mjdStart + (12270-10)*second:
  subarray.execute(mjdStart + 12265*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12270*second) + ' since array.time is ' + str(array.time())

# Scan 297 = No0298
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0298.'
subarray.setSwitches(mjdStart + 12284*second, mjdStart+12300*second, obsCode+'_'+stnCode+'_'+'No0298')
if array.time() < mjdStart + (12300-10)*second:
  subarray.execute(mjdStart + 12295*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12300*second) + ' since array.time is ' + str(array.time())

# Scan 298 = No0299
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0299.'
subarray.setSwitches(mjdStart + 12314*second, mjdStart+12329*second, obsCode+'_'+stnCode+'_'+'No0299')
if array.time() < mjdStart + (12329-10)*second:
  subarray.execute(mjdStart + 12324*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12329*second) + ' since array.time is ' + str(array.time())

# Scan 299 = No0300
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0300.'
subarray.setSwitches(mjdStart + 12343*second, mjdStart+12359*second, obsCode+'_'+stnCode+'_'+'No0300')
if array.time() < mjdStart + (12359-10)*second:
  subarray.execute(mjdStart + 12354*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12359*second) + ' since array.time is ' + str(array.time())

# Scan 300 = No0301
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0301.'
subarray.setSwitches(mjdStart + 12371*second, mjdStart+12389*second, obsCode+'_'+stnCode+'_'+'No0301')
if array.time() < mjdStart + (12389-10)*second:
  subarray.execute(mjdStart + 12384*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12389*second) + ' since array.time is ' + str(array.time())

# Scan 301 = No0302
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0302.'
subarray.setSwitches(mjdStart + 12401*second, mjdStart+12419*second, obsCode+'_'+stnCode+'_'+'No0302')
if array.time() < mjdStart + (12419-10)*second:
  subarray.execute(mjdStart + 12414*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12419*second) + ' since array.time is ' + str(array.time())

# Scan 302 = No0303
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0303.'
subarray.setSwitches(mjdStart + 12431*second, mjdStart+12449*second, obsCode+'_'+stnCode+'_'+'No0303')
if array.time() < mjdStart + (12449-10)*second:
  subarray.execute(mjdStart + 12444*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12449*second) + ' since array.time is ' + str(array.time())

# Scan 303 = No0304
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0304.'
subarray.setSwitches(mjdStart + 12461*second, mjdStart+12479*second, obsCode+'_'+stnCode+'_'+'No0304')
if array.time() < mjdStart + (12479-10)*second:
  subarray.execute(mjdStart + 12474*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12479*second) + ' since array.time is ' + str(array.time())

# Scan 304 = No0305
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0305.'
subarray.setSwitches(mjdStart + 12491*second, mjdStart+12509*second, obsCode+'_'+stnCode+'_'+'No0305')
if array.time() < mjdStart + (12509-10)*second:
  subarray.execute(mjdStart + 12504*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12509*second) + ' since array.time is ' + str(array.time())

# Scan 305 = No0306
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0306.'
subarray.setSwitches(mjdStart + 12521*second, mjdStart+12539*second, obsCode+'_'+stnCode+'_'+'No0306')
if array.time() < mjdStart + (12539-10)*second:
  subarray.execute(mjdStart + 12534*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12539*second) + ' since array.time is ' + str(array.time())

# Scan 306 = No0307
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0307.'
subarray.setSwitches(mjdStart + 12551*second, mjdStart+12565*second, obsCode+'_'+stnCode+'_'+'No0307')
if array.time() < mjdStart + (12565-10)*second:
  subarray.execute(mjdStart + 12560*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12565*second) + ' since array.time is ' + str(array.time())

# Scan 307 = No0308
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0308.'
subarray.setSwitches(mjdStart + 12576*second, mjdStart+12595*second, obsCode+'_'+stnCode+'_'+'No0308')
if array.time() < mjdStart + (12595-10)*second:
  subarray.execute(mjdStart + 12590*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12595*second) + ' since array.time is ' + str(array.time())

# Scan 308 = No0309
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0309.'
subarray.setSwitches(mjdStart + 12606*second, mjdStart+12625*second, obsCode+'_'+stnCode+'_'+'No0309')
if array.time() < mjdStart + (12625-10)*second:
  subarray.execute(mjdStart + 12620*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12625*second) + ' since array.time is ' + str(array.time())

# Scan 309 = No0310
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0310.'
subarray.setSwitches(mjdStart + 12636*second, mjdStart+12655*second, obsCode+'_'+stnCode+'_'+'No0310')
if array.time() < mjdStart + (12655-10)*second:
  subarray.execute(mjdStart + 12650*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12655*second) + ' since array.time is ' + str(array.time())

# Scan 310 = No0311
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0311.'
subarray.setSwitches(mjdStart + 12666*second, mjdStart+12685*second, obsCode+'_'+stnCode+'_'+'No0311')
if array.time() < mjdStart + (12685-10)*second:
  subarray.execute(mjdStart + 12680*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12685*second) + ' since array.time is ' + str(array.time())

# Scan 311 = No0312
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0312.'
subarray.setSwitches(mjdStart + 12696*second, mjdStart+12715*second, obsCode+'_'+stnCode+'_'+'No0312')
if array.time() < mjdStart + (12715-10)*second:
  subarray.execute(mjdStart + 12710*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12715*second) + ' since array.time is ' + str(array.time())

# Scan 312 = No0313
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0313.'
subarray.setSwitches(mjdStart + 12726*second, mjdStart+12745*second, obsCode+'_'+stnCode+'_'+'No0313')
if array.time() < mjdStart + (12745-10)*second:
  subarray.execute(mjdStart + 12740*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12745*second) + ' since array.time is ' + str(array.time())

# Scan 313 = No0314
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0314.'
subarray.setSwitches(mjdStart + 12756*second, mjdStart+12775*second, obsCode+'_'+stnCode+'_'+'No0314')
if array.time() < mjdStart + (12775-10)*second:
  subarray.execute(mjdStart + 12770*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12775*second) + ' since array.time is ' + str(array.time())

# Scan 314 = No0315
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0315.'
subarray.setSwitches(mjdStart + 12786*second, mjdStart+12805*second, obsCode+'_'+stnCode+'_'+'No0315')
if array.time() < mjdStart + (12805-10)*second:
  subarray.execute(mjdStart + 12800*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12805*second) + ' since array.time is ' + str(array.time())

# Scan 315 = No0316
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0316.'
subarray.setSwitches(mjdStart + 12816*second, mjdStart+12835*second, obsCode+'_'+stnCode+'_'+'No0316')
if array.time() < mjdStart + (12835-10)*second:
  subarray.execute(mjdStart + 12830*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12835*second) + ' since array.time is ' + str(array.time())

# Scan 316 = No0317
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0317.'
subarray.setSwitches(mjdStart + 12846*second, mjdStart+12865*second, obsCode+'_'+stnCode+'_'+'No0317')
if array.time() < mjdStart + (12865-10)*second:
  subarray.execute(mjdStart + 12860*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12865*second) + ' since array.time is ' + str(array.time())

# Scan 317 = No0318
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0318.'
subarray.setSwitches(mjdStart + 12876*second, mjdStart+12895*second, obsCode+'_'+stnCode+'_'+'No0318')
if array.time() < mjdStart + (12895-10)*second:
  subarray.execute(mjdStart + 12890*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12895*second) + ' since array.time is ' + str(array.time())

# Scan 318 = No0319
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0319.'
subarray.setSwitches(mjdStart + 12906*second, mjdStart+12924*second, obsCode+'_'+stnCode+'_'+'No0319')
if array.time() < mjdStart + (12924-10)*second:
  subarray.execute(mjdStart + 12919*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12924*second) + ' since array.time is ' + str(array.time())

# Scan 319 = No0320
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0320.'
subarray.setSwitches(mjdStart + 12936*second, mjdStart+12954*second, obsCode+'_'+stnCode+'_'+'No0320')
if array.time() < mjdStart + (12954-10)*second:
  subarray.execute(mjdStart + 12949*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12954*second) + ' since array.time is ' + str(array.time())

# Scan 320 = No0321
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0321.'
subarray.setSwitches(mjdStart + 12966*second, mjdStart+12984*second, obsCode+'_'+stnCode+'_'+'No0321')
if array.time() < mjdStart + (12984-10)*second:
  subarray.execute(mjdStart + 12979*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+12984*second) + ' since array.time is ' + str(array.time())

# Scan 321 = No0322
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0322.'
subarray.setSwitches(mjdStart + 12996*second, mjdStart+13014*second, obsCode+'_'+stnCode+'_'+'No0322')
if array.time() < mjdStart + (13014-10)*second:
  subarray.execute(mjdStart + 13009*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13014*second) + ' since array.time is ' + str(array.time())

# Scan 322 = No0323
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0323.'
subarray.setSwitches(mjdStart + 13026*second, mjdStart+13044*second, obsCode+'_'+stnCode+'_'+'No0323')
if array.time() < mjdStart + (13044-10)*second:
  subarray.execute(mjdStart + 13039*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13044*second) + ' since array.time is ' + str(array.time())

# Scan 323 = No0324
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0324.'
subarray.setSwitches(mjdStart + 13056*second, mjdStart+13074*second, obsCode+'_'+stnCode+'_'+'No0324')
if array.time() < mjdStart + (13074-10)*second:
  subarray.execute(mjdStart + 13069*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13074*second) + ' since array.time is ' + str(array.time())

# Scan 324 = No0325
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0325.'
subarray.setSwitches(mjdStart + 13086*second, mjdStart+13104*second, obsCode+'_'+stnCode+'_'+'No0325')
if array.time() < mjdStart + (13104-10)*second:
  subarray.execute(mjdStart + 13099*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13104*second) + ' since array.time is ' + str(array.time())

# Scan 325 = No0326
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0326.'
subarray.setSwitches(mjdStart + 13117*second, mjdStart+13134*second, obsCode+'_'+stnCode+'_'+'No0326')
if array.time() < mjdStart + (13134-10)*second:
  subarray.execute(mjdStart + 13129*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13134*second) + ' since array.time is ' + str(array.time())

# Scan 326 = No0327
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0327.'
subarray.setSwitches(mjdStart + 13147*second, mjdStart+13164*second, obsCode+'_'+stnCode+'_'+'No0327')
if array.time() < mjdStart + (13164-10)*second:
  subarray.execute(mjdStart + 13159*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13164*second) + ' since array.time is ' + str(array.time())

# Scan 327 = No0328
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0328.'
subarray.setSwitches(mjdStart + 13177*second, mjdStart+13194*second, obsCode+'_'+stnCode+'_'+'No0328')
if array.time() < mjdStart + (13194-10)*second:
  subarray.execute(mjdStart + 13189*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13194*second) + ' since array.time is ' + str(array.time())

# Scan 328 = No0329
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0329.'
subarray.setSwitches(mjdStart + 13207*second, mjdStart+13224*second, obsCode+'_'+stnCode+'_'+'No0329')
if array.time() < mjdStart + (13224-10)*second:
  subarray.execute(mjdStart + 13219*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13224*second) + ' since array.time is ' + str(array.time())

# Scan 329 = No0330
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0330.'
subarray.setSwitches(mjdStart + 13237*second, mjdStart+13254*second, obsCode+'_'+stnCode+'_'+'No0330')
if array.time() < mjdStart + (13254-10)*second:
  subarray.execute(mjdStart + 13249*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13254*second) + ' since array.time is ' + str(array.time())

# Scan 330 = No0331
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0331.'
subarray.setSwitches(mjdStart + 13267*second, mjdStart+13283*second, obsCode+'_'+stnCode+'_'+'No0331')
if array.time() < mjdStart + (13283-10)*second:
  subarray.execute(mjdStart + 13278*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13283*second) + ' since array.time is ' + str(array.time())

# Scan 331 = No0332
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0332.'
subarray.setSwitches(mjdStart + 13294*second, mjdStart+13314*second, obsCode+'_'+stnCode+'_'+'No0332')
if array.time() < mjdStart + (13314-10)*second:
  subarray.execute(mjdStart + 13309*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13314*second) + ' since array.time is ' + str(array.time())

# Scan 332 = No0333
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0333.'
subarray.setSwitches(mjdStart + 13323*second, mjdStart+13344*second, obsCode+'_'+stnCode+'_'+'No0333')
if array.time() < mjdStart + (13344-10)*second:
  subarray.execute(mjdStart + 13339*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13344*second) + ' since array.time is ' + str(array.time())

# Scan 333 = No0334
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0334.'
subarray.setSwitches(mjdStart + 13353*second, mjdStart+13374*second, obsCode+'_'+stnCode+'_'+'No0334')
if array.time() < mjdStart + (13374-10)*second:
  subarray.execute(mjdStart + 13369*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13374*second) + ' since array.time is ' + str(array.time())

# Scan 334 = No0335
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0335.'
subarray.setSwitches(mjdStart + 13386*second, mjdStart+13404*second, obsCode+'_'+stnCode+'_'+'No0335')
if array.time() < mjdStart + (13404-10)*second:
  subarray.execute(mjdStart + 13399*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13404*second) + ' since array.time is ' + str(array.time())

# Scan 335 = No0336
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0336.'
subarray.setSwitches(mjdStart + 13415*second, mjdStart+13434*second, obsCode+'_'+stnCode+'_'+'No0336')
if array.time() < mjdStart + (13434-10)*second:
  subarray.execute(mjdStart + 13429*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13434*second) + ' since array.time is ' + str(array.time())

# Scan 336 = No0337
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0337.'
subarray.setSwitches(mjdStart + 13445*second, mjdStart+13464*second, obsCode+'_'+stnCode+'_'+'No0337')
if array.time() < mjdStart + (13464-10)*second:
  subarray.execute(mjdStart + 13459*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13464*second) + ' since array.time is ' + str(array.time())

# Scan 337 = No0338
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0338.'
subarray.setSwitches(mjdStart + 13475*second, mjdStart+13494*second, obsCode+'_'+stnCode+'_'+'No0338')
if array.time() < mjdStart + (13494-10)*second:
  subarray.execute(mjdStart + 13489*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13494*second) + ' since array.time is ' + str(array.time())

# Scan 338 = No0339
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0339.'
subarray.setSwitches(mjdStart + 13503*second, mjdStart+13524*second, obsCode+'_'+stnCode+'_'+'No0339')
if array.time() < mjdStart + (13524-10)*second:
  subarray.execute(mjdStart + 13519*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13524*second) + ' since array.time is ' + str(array.time())

# Scan 339 = No0340
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0340.'
subarray.setSwitches(mjdStart + 13533*second, mjdStart+13554*second, obsCode+'_'+stnCode+'_'+'No0340')
if array.time() < mjdStart + (13554-10)*second:
  subarray.execute(mjdStart + 13549*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13554*second) + ' since array.time is ' + str(array.time())

# Scan 340 = No0341
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0341.'
subarray.setSwitches(mjdStart + 13565*second, mjdStart+13584*second, obsCode+'_'+stnCode+'_'+'No0341')
if array.time() < mjdStart + (13584-10)*second:
  subarray.execute(mjdStart + 13579*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13584*second) + ' since array.time is ' + str(array.time())

# Scan 341 = No0342
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0342.'
subarray.setSwitches(mjdStart + 13595*second, mjdStart+13614*second, obsCode+'_'+stnCode+'_'+'No0342')
if array.time() < mjdStart + (13614-10)*second:
  subarray.execute(mjdStart + 13609*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13614*second) + ' since array.time is ' + str(array.time())

# Scan 342 = No0343
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0343.'
subarray.setSwitches(mjdStart + 13625*second, mjdStart+13644*second, obsCode+'_'+stnCode+'_'+'No0343')
if array.time() < mjdStart + (13644-10)*second:
  subarray.execute(mjdStart + 13639*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13644*second) + ' since array.time is ' + str(array.time())

# Scan 343 = No0344
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0344.'
subarray.setSwitches(mjdStart + 13655*second, mjdStart+13673*second, obsCode+'_'+stnCode+'_'+'No0344')
if array.time() < mjdStart + (13673-10)*second:
  subarray.execute(mjdStart + 13668*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13673*second) + ' since array.time is ' + str(array.time())

# Scan 344 = No0345
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0345.'
subarray.setSwitches(mjdStart + 13682*second, mjdStart+13703*second, obsCode+'_'+stnCode+'_'+'No0345')
if array.time() < mjdStart + (13703-10)*second:
  subarray.execute(mjdStart + 13698*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13703*second) + ' since array.time is ' + str(array.time())

# Scan 345 = No0346
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0346.'
subarray.setSwitches(mjdStart + 13712*second, mjdStart+13733*second, obsCode+'_'+stnCode+'_'+'No0346')
if array.time() < mjdStart + (13733-10)*second:
  subarray.execute(mjdStart + 13728*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13733*second) + ' since array.time is ' + str(array.time())

# Scan 346 = No0347
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0347.'
subarray.setSwitches(mjdStart + 13744*second, mjdStart+13763*second, obsCode+'_'+stnCode+'_'+'No0347')
if array.time() < mjdStart + (13763-10)*second:
  subarray.execute(mjdStart + 13758*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13763*second) + ' since array.time is ' + str(array.time())

# Scan 347 = No0348
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0348.'
subarray.setSwitches(mjdStart + 13774*second, mjdStart+13793*second, obsCode+'_'+stnCode+'_'+'No0348')
if array.time() < mjdStart + (13793-10)*second:
  subarray.execute(mjdStart + 13788*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13793*second) + ' since array.time is ' + str(array.time())

# Scan 348 = No0349
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0349.'
subarray.setSwitches(mjdStart + 13804*second, mjdStart+13823*second, obsCode+'_'+stnCode+'_'+'No0349')
if array.time() < mjdStart + (13823-10)*second:
  subarray.execute(mjdStart + 13818*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13823*second) + ' since array.time is ' + str(array.time())

# Scan 349 = No0350
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0350.'
subarray.setSwitches(mjdStart + 13834*second, mjdStart+13853*second, obsCode+'_'+stnCode+'_'+'No0350')
if array.time() < mjdStart + (13853-10)*second:
  subarray.execute(mjdStart + 13848*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13853*second) + ' since array.time is ' + str(array.time())

# Scan 350 = No0351
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0351.'
subarray.setSwitches(mjdStart + 13862*second, mjdStart+13883*second, obsCode+'_'+stnCode+'_'+'No0351')
if array.time() < mjdStart + (13883-10)*second:
  subarray.execute(mjdStart + 13878*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13883*second) + ' since array.time is ' + str(array.time())

# Scan 351 = No0352
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0352.'
subarray.setSwitches(mjdStart + 13892*second, mjdStart+13913*second, obsCode+'_'+stnCode+'_'+'No0352')
if array.time() < mjdStart + (13913-10)*second:
  subarray.execute(mjdStart + 13908*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13913*second) + ' since array.time is ' + str(array.time())

# Scan 352 = No0353
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0353.'
subarray.setSwitches(mjdStart + 13924*second, mjdStart+13943*second, obsCode+'_'+stnCode+'_'+'No0353')
if array.time() < mjdStart + (13943-10)*second:
  subarray.execute(mjdStart + 13938*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13943*second) + ' since array.time is ' + str(array.time())

# Scan 353 = No0354
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0354.'
subarray.setSwitches(mjdStart + 13954*second, mjdStart+13973*second, obsCode+'_'+stnCode+'_'+'No0354')
if array.time() < mjdStart + (13973-10)*second:
  subarray.execute(mjdStart + 13968*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+13973*second) + ' since array.time is ' + str(array.time())

# Scan 354 = No0355
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0355.'
subarray.setSwitches(mjdStart + 13984*second, mjdStart+14003*second, obsCode+'_'+stnCode+'_'+'No0355')
if array.time() < mjdStart + (14003-10)*second:
  subarray.execute(mjdStart + 13998*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14003*second) + ' since array.time is ' + str(array.time())

# Scan 355 = No0356
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0356.'
subarray.setSwitches(mjdStart + 14014*second, mjdStart+14032*second, obsCode+'_'+stnCode+'_'+'No0356')
if array.time() < mjdStart + (14032-10)*second:
  subarray.execute(mjdStart + 14027*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14032*second) + ' since array.time is ' + str(array.time())

# Scan 356 = No0357
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0357.'
subarray.setSwitches(mjdStart + 14040*second, mjdStart+14057*second, obsCode+'_'+stnCode+'_'+'No0357')
if array.time() < mjdStart + (14057-10)*second:
  subarray.execute(mjdStart + 14052*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14057*second) + ' since array.time is ' + str(array.time())

# Scan 357 = No0358
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0358.'
subarray.setSwitches(mjdStart + 14068*second, mjdStart+14087*second, obsCode+'_'+stnCode+'_'+'No0358')
if array.time() < mjdStart + (14087-10)*second:
  subarray.execute(mjdStart + 14082*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14087*second) + ' since array.time is ' + str(array.time())

# Scan 358 = No0359
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0359.'
subarray.setSwitches(mjdStart + 14098*second, mjdStart+14117*second, obsCode+'_'+stnCode+'_'+'No0359')
if array.time() < mjdStart + (14117-10)*second:
  subarray.execute(mjdStart + 14112*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14117*second) + ' since array.time is ' + str(array.time())

# Scan 359 = No0360
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0360.'
subarray.setSwitches(mjdStart + 14128*second, mjdStart+14147*second, obsCode+'_'+stnCode+'_'+'No0360')
if array.time() < mjdStart + (14147-10)*second:
  subarray.execute(mjdStart + 14142*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14147*second) + ' since array.time is ' + str(array.time())

# Scan 360 = No0361
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0361.'
subarray.setSwitches(mjdStart + 14158*second, mjdStart+14177*second, obsCode+'_'+stnCode+'_'+'No0361')
if array.time() < mjdStart + (14177-10)*second:
  subarray.execute(mjdStart + 14172*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14177*second) + ' since array.time is ' + str(array.time())

# Scan 361 = No0362
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0362.'
subarray.setSwitches(mjdStart + 14188*second, mjdStart+14207*second, obsCode+'_'+stnCode+'_'+'No0362')
if array.time() < mjdStart + (14207-10)*second:
  subarray.execute(mjdStart + 14202*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14207*second) + ' since array.time is ' + str(array.time())

# Scan 362 = No0363
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0363.'
subarray.setSwitches(mjdStart + 14218*second, mjdStart+14237*second, obsCode+'_'+stnCode+'_'+'No0363')
if array.time() < mjdStart + (14237-10)*second:
  subarray.execute(mjdStart + 14232*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14237*second) + ' since array.time is ' + str(array.time())

# Scan 363 = No0364
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0364.'
subarray.setSwitches(mjdStart + 14249*second, mjdStart+14267*second, obsCode+'_'+stnCode+'_'+'No0364')
if array.time() < mjdStart + (14267-10)*second:
  subarray.execute(mjdStart + 14262*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14267*second) + ' since array.time is ' + str(array.time())

# Scan 364 = No0365
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0365.'
subarray.setSwitches(mjdStart + 14278*second, mjdStart+14297*second, obsCode+'_'+stnCode+'_'+'No0365')
if array.time() < mjdStart + (14297-10)*second:
  subarray.execute(mjdStart + 14292*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14297*second) + ' since array.time is ' + str(array.time())

# Scan 365 = No0366
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0366.'
subarray.setSwitches(mjdStart + 14309*second, mjdStart+14327*second, obsCode+'_'+stnCode+'_'+'No0366')
if array.time() < mjdStart + (14327-10)*second:
  subarray.execute(mjdStart + 14322*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14327*second) + ' since array.time is ' + str(array.time())

# Scan 366 = No0367
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0367.'
subarray.setSwitches(mjdStart + 14338*second, mjdStart+14357*second, obsCode+'_'+stnCode+'_'+'No0367')
if array.time() < mjdStart + (14357-10)*second:
  subarray.execute(mjdStart + 14352*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14357*second) + ' since array.time is ' + str(array.time())

# Scan 367 = No0368
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0368.'
subarray.setSwitches(mjdStart + 14368*second, mjdStart+14386*second, obsCode+'_'+stnCode+'_'+'No0368')
if array.time() < mjdStart + (14386-10)*second:
  subarray.execute(mjdStart + 14381*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14386*second) + ' since array.time is ' + str(array.time())

# Scan 368 = No0369
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0369.'
subarray.setSwitches(mjdStart + 14397*second, mjdStart+14416*second, obsCode+'_'+stnCode+'_'+'No0369')
if array.time() < mjdStart + (14416-10)*second:
  subarray.execute(mjdStart + 14411*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14416*second) + ' since array.time is ' + str(array.time())

# Scan 369 = No0370
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0370.'
subarray.setSwitches(mjdStart + 14429*second, mjdStart+14446*second, obsCode+'_'+stnCode+'_'+'No0370')
if array.time() < mjdStart + (14446-10)*second:
  subarray.execute(mjdStart + 14441*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14446*second) + ' since array.time is ' + str(array.time())

# Scan 370 = No0371
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0371.'
subarray.setSwitches(mjdStart + 14459*second, mjdStart+14476*second, obsCode+'_'+stnCode+'_'+'No0371')
if array.time() < mjdStart + (14476-10)*second:
  subarray.execute(mjdStart + 14471*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14476*second) + ' since array.time is ' + str(array.time())

# Scan 371 = No0372
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0372.'
subarray.setSwitches(mjdStart + 14489*second, mjdStart+14506*second, obsCode+'_'+stnCode+'_'+'No0372')
if array.time() < mjdStart + (14506-10)*second:
  subarray.execute(mjdStart + 14501*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14506*second) + ' since array.time is ' + str(array.time())

# Scan 372 = No0373
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0373.'
subarray.setSwitches(mjdStart + 14519*second, mjdStart+14536*second, obsCode+'_'+stnCode+'_'+'No0373')
if array.time() < mjdStart + (14536-10)*second:
  subarray.execute(mjdStart + 14531*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14536*second) + ' since array.time is ' + str(array.time())

# Scan 373 = No0374
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0374.'
subarray.setSwitches(mjdStart + 14549*second, mjdStart+14566*second, obsCode+'_'+stnCode+'_'+'No0374')
if array.time() < mjdStart + (14566-10)*second:
  subarray.execute(mjdStart + 14561*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14566*second) + ' since array.time is ' + str(array.time())

# Scan 374 = No0375
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0375.'
subarray.setSwitches(mjdStart + 14579*second, mjdStart+14596*second, obsCode+'_'+stnCode+'_'+'No0375')
if array.time() < mjdStart + (14596-10)*second:
  subarray.execute(mjdStart + 14591*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14596*second) + ' since array.time is ' + str(array.time())

# Scan 375 = No0376
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0376.'
subarray.setSwitches(mjdStart + 14608*second, mjdStart+14626*second, obsCode+'_'+stnCode+'_'+'No0376')
if array.time() < mjdStart + (14626-10)*second:
  subarray.execute(mjdStart + 14621*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14626*second) + ' since array.time is ' + str(array.time())

# Scan 376 = No0377
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0377.'
subarray.setSwitches(mjdStart + 14638*second, mjdStart+14656*second, obsCode+'_'+stnCode+'_'+'No0377')
if array.time() < mjdStart + (14656-10)*second:
  subarray.execute(mjdStart + 14651*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14656*second) + ' since array.time is ' + str(array.time())

# Scan 377 = No0378
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0378.'
subarray.setSwitches(mjdStart + 14668*second, mjdStart+14686*second, obsCode+'_'+stnCode+'_'+'No0378')
if array.time() < mjdStart + (14686-10)*second:
  subarray.execute(mjdStart + 14681*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14686*second) + ' since array.time is ' + str(array.time())

# Scan 378 = No0379
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0379.'
subarray.setSwitches(mjdStart + 14698*second, mjdStart+14716*second, obsCode+'_'+stnCode+'_'+'No0379')
if array.time() < mjdStart + (14716-10)*second:
  subarray.execute(mjdStart + 14711*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14716*second) + ' since array.time is ' + str(array.time())

# Scan 379 = No0380
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0380.'
subarray.setSwitches(mjdStart + 14728*second, mjdStart+14746*second, obsCode+'_'+stnCode+'_'+'No0380')
if array.time() < mjdStart + (14746-10)*second:
  subarray.execute(mjdStart + 14741*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14746*second) + ' since array.time is ' + str(array.time())

# Scan 380 = No0381
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0381.'
subarray.setSwitches(mjdStart + 14758*second, mjdStart+14775*second, obsCode+'_'+stnCode+'_'+'No0381')
if array.time() < mjdStart + (14775-10)*second:
  subarray.execute(mjdStart + 14770*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14775*second) + ' since array.time is ' + str(array.time())

# Scan 381 = No0382
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0382.'
subarray.setSwitches(mjdStart + 14786*second, mjdStart+14802*second, obsCode+'_'+stnCode+'_'+'No0382')
if array.time() < mjdStart + (14802-10)*second:
  subarray.execute(mjdStart + 14797*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14802*second) + ' since array.time is ' + str(array.time())

# Scan 382 = No0383
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0383.'
subarray.setSwitches(mjdStart + 14813*second, mjdStart+14832*second, obsCode+'_'+stnCode+'_'+'No0383')
if array.time() < mjdStart + (14832-10)*second:
  subarray.execute(mjdStart + 14827*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14832*second) + ' since array.time is ' + str(array.time())

# Scan 383 = No0384
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0384.'
subarray.setSwitches(mjdStart + 14843*second, mjdStart+14862*second, obsCode+'_'+stnCode+'_'+'No0384')
if array.time() < mjdStart + (14862-10)*second:
  subarray.execute(mjdStart + 14857*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14862*second) + ' since array.time is ' + str(array.time())

# Scan 384 = No0385
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0385.'
subarray.setSwitches(mjdStart + 14873*second, mjdStart+14892*second, obsCode+'_'+stnCode+'_'+'No0385')
if array.time() < mjdStart + (14892-10)*second:
  subarray.execute(mjdStart + 14887*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14892*second) + ' since array.time is ' + str(array.time())

# Scan 385 = No0386
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0386.'
subarray.setSwitches(mjdStart + 14903*second, mjdStart+14922*second, obsCode+'_'+stnCode+'_'+'No0386')
if array.time() < mjdStart + (14922-10)*second:
  subarray.execute(mjdStart + 14917*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14922*second) + ' since array.time is ' + str(array.time())

# Scan 386 = No0387
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0387.'
subarray.setSwitches(mjdStart + 14933*second, mjdStart+14952*second, obsCode+'_'+stnCode+'_'+'No0387')
if array.time() < mjdStart + (14952-10)*second:
  subarray.execute(mjdStart + 14947*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14952*second) + ' since array.time is ' + str(array.time())

# Scan 387 = No0388
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0388.'
subarray.setSwitches(mjdStart + 14963*second, mjdStart+14982*second, obsCode+'_'+stnCode+'_'+'No0388')
if array.time() < mjdStart + (14982-10)*second:
  subarray.execute(mjdStart + 14977*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+14982*second) + ' since array.time is ' + str(array.time())

# Scan 388 = No0389
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0389.'
subarray.setSwitches(mjdStart + 14994*second, mjdStart+15011*second, obsCode+'_'+stnCode+'_'+'No0389')
if array.time() < mjdStart + (15011-10)*second:
  subarray.execute(mjdStart + 15006*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15011*second) + ' since array.time is ' + str(array.time())

# Scan 389 = No0390
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0390.'
subarray.setSwitches(mjdStart + 15023*second, mjdStart+15041*second, obsCode+'_'+stnCode+'_'+'No0390')
if array.time() < mjdStart + (15041-10)*second:
  subarray.execute(mjdStart + 15036*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15041*second) + ' since array.time is ' + str(array.time())

# Scan 390 = No0391
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0391.'
subarray.setSwitches(mjdStart + 15053*second, mjdStart+15071*second, obsCode+'_'+stnCode+'_'+'No0391')
if array.time() < mjdStart + (15071-10)*second:
  subarray.execute(mjdStart + 15066*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15071*second) + ' since array.time is ' + str(array.time())

# Scan 391 = No0392
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0392.'
subarray.setSwitches(mjdStart + 15083*second, mjdStart+15101*second, obsCode+'_'+stnCode+'_'+'No0392')
if array.time() < mjdStart + (15101-10)*second:
  subarray.execute(mjdStart + 15096*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15101*second) + ' since array.time is ' + str(array.time())

# Scan 392 = No0393
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0393.'
subarray.setSwitches(mjdStart + 15113*second, mjdStart+15131*second, obsCode+'_'+stnCode+'_'+'No0393')
if array.time() < mjdStart + (15131-10)*second:
  subarray.execute(mjdStart + 15126*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15131*second) + ' since array.time is ' + str(array.time())

# Scan 393 = No0394
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0394.'
subarray.setSwitches(mjdStart + 15143*second, mjdStart+15161*second, obsCode+'_'+stnCode+'_'+'No0394')
if array.time() < mjdStart + (15161-10)*second:
  subarray.execute(mjdStart + 15156*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15161*second) + ' since array.time is ' + str(array.time())

# Scan 394 = No0395
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0395.'
subarray.setSwitches(mjdStart + 15173*second, mjdStart+15191*second, obsCode+'_'+stnCode+'_'+'No0395')
if array.time() < mjdStart + (15191-10)*second:
  subarray.execute(mjdStart + 15186*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15191*second) + ' since array.time is ' + str(array.time())

# Scan 395 = No0396
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0396.'
subarray.setSwitches(mjdStart + 15203*second, mjdStart+15221*second, obsCode+'_'+stnCode+'_'+'No0396')
if array.time() < mjdStart + (15221-10)*second:
  subarray.execute(mjdStart + 15216*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15221*second) + ' since array.time is ' + str(array.time())

# Scan 396 = No0397
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0397.'
subarray.setSwitches(mjdStart + 15233*second, mjdStart+15251*second, obsCode+'_'+stnCode+'_'+'No0397')
if array.time() < mjdStart + (15251-10)*second:
  subarray.execute(mjdStart + 15246*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15251*second) + ' since array.time is ' + str(array.time())

# Scan 397 = No0398
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0398.'
subarray.setSwitches(mjdStart + 15263*second, mjdStart+15281*second, obsCode+'_'+stnCode+'_'+'No0398')
if array.time() < mjdStart + (15281-10)*second:
  subarray.execute(mjdStart + 15276*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15281*second) + ' since array.time is ' + str(array.time())

# Scan 398 = No0399
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0399.'
subarray.setSwitches(mjdStart + 15293*second, mjdStart+15311*second, obsCode+'_'+stnCode+'_'+'No0399')
if array.time() < mjdStart + (15311-10)*second:
  subarray.execute(mjdStart + 15306*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15311*second) + ' since array.time is ' + str(array.time())

# Scan 399 = No0400
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0400.'
subarray.setSwitches(mjdStart + 15323*second, mjdStart+15341*second, obsCode+'_'+stnCode+'_'+'No0400')
if array.time() < mjdStart + (15341-10)*second:
  subarray.execute(mjdStart + 15336*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15341*second) + ' since array.time is ' + str(array.time())

# Scan 400 = No0401
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0401.'
subarray.setSwitches(mjdStart + 15353*second, mjdStart+15370*second, obsCode+'_'+stnCode+'_'+'No0401')
if array.time() < mjdStart + (15370-10)*second:
  subarray.execute(mjdStart + 15365*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15370*second) + ' since array.time is ' + str(array.time())

# Scan 401 = No0402
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0402.'
subarray.setSwitches(mjdStart + 15382*second, mjdStart+15400*second, obsCode+'_'+stnCode+'_'+'No0402')
if array.time() < mjdStart + (15400-10)*second:
  subarray.execute(mjdStart + 15395*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15400*second) + ' since array.time is ' + str(array.time())

# Scan 402 = No0403
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0403.'
subarray.setSwitches(mjdStart + 15412*second, mjdStart+15430*second, obsCode+'_'+stnCode+'_'+'No0403')
if array.time() < mjdStart + (15430-10)*second:
  subarray.execute(mjdStart + 15425*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15430*second) + ' since array.time is ' + str(array.time())

# Scan 403 = No0404
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0404.'
subarray.setSwitches(mjdStart + 15442*second, mjdStart+15460*second, obsCode+'_'+stnCode+'_'+'No0404')
if array.time() < mjdStart + (15460-10)*second:
  subarray.execute(mjdStart + 15455*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15460*second) + ' since array.time is ' + str(array.time())

# Scan 404 = No0405
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0405.'
subarray.setSwitches(mjdStart + 15472*second, mjdStart+15490*second, obsCode+'_'+stnCode+'_'+'No0405')
if array.time() < mjdStart + (15490-10)*second:
  subarray.execute(mjdStart + 15485*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15490*second) + ' since array.time is ' + str(array.time())

# Scan 405 = No0406
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0406.'
subarray.setSwitches(mjdStart + 15502*second, mjdStart+15520*second, obsCode+'_'+stnCode+'_'+'No0406')
if array.time() < mjdStart + (15520-10)*second:
  subarray.execute(mjdStart + 15515*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15520*second) + ' since array.time is ' + str(array.time())

# Scan 406 = No0407
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source20)
print 'Not a recording scan but still set switches for No0407.'
subarray.setSwitches(mjdStart + 15618*second, mjdStart+15683*second, obsCode+'_'+stnCode+'_'+'No0407')
if array.time() < mjdStart + (15683-10)*second:
  subarray.execute(mjdStart + 15678*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15683*second) + ' since array.time is ' + str(array.time())

# Scan 407 = No0408
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source22)
print 'Not a recording scan but still set switches for No0408.'
subarray.setSwitches(mjdStart + 15689*second, mjdStart+15749*second, obsCode+'_'+stnCode+'_'+'No0408')
if array.time() < mjdStart + (15749-10)*second:
  subarray.execute(mjdStart + 15744*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15749*second) + ' since array.time is ' + str(array.time())

# Scan 408 = No0409
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source21)
print 'Not a recording scan but still set switches for No0409.'
subarray.setSwitches(mjdStart + 15755*second, mjdStart+15815*second, obsCode+'_'+stnCode+'_'+'No0409')
if array.time() < mjdStart + (15815-10)*second:
  subarray.execute(mjdStart + 15810*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15815*second) + ' since array.time is ' + str(array.time())

# Scan 409 = No0410
# changing to mode geodetic
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 4)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source47)
print 'Not a recording scan but still set switches for No0410.'
subarray.setSwitches(mjdStart + 15841*second, mjdStart+15925*second, obsCode+'_'+stnCode+'_'+'No0410')
if array.time() < mjdStart + (15925-10)*second:
  subarray.execute(mjdStart + 15920*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+15925*second) + ' since array.time is ' + str(array.time())

# Scan 410 = No0411
subarray.setSource(source48)
print 'Not a recording scan but still set switches for No0411.'
subarray.setSwitches(mjdStart + 15942*second, mjdStart+16013*second, obsCode+'_'+stnCode+'_'+'No0411')
if array.time() < mjdStart + (16013-10)*second:
  subarray.execute(mjdStart + 16008*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16013*second) + ' since array.time is ' + str(array.time())

# Scan 411 = No0412
subarray.setSource(source43)
print 'Not a recording scan but still set switches for No0412.'
subarray.setSwitches(mjdStart + 16048*second, mjdStart+16137*second, obsCode+'_'+stnCode+'_'+'No0412')
if array.time() < mjdStart + (16137-10)*second:
  subarray.execute(mjdStart + 16132*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16137*second) + ' since array.time is ' + str(array.time())

# Scan 412 = No0413
subarray.setSource(source49)
print 'Not a recording scan but still set switches for No0413.'
subarray.setSwitches(mjdStart + 16152*second, mjdStart+16224*second, obsCode+'_'+stnCode+'_'+'No0413')
if array.time() < mjdStart + (16224-10)*second:
  subarray.execute(mjdStart + 16219*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16224*second) + ' since array.time is ' + str(array.time())

# Scan 413 = No0414
subarray.setSource(source50)
print 'Not a recording scan but still set switches for No0414.'
subarray.setSwitches(mjdStart + 16325*second, mjdStart+16390*second, obsCode+'_'+stnCode+'_'+'No0414')
if array.time() < mjdStart + (16390-10)*second:
  subarray.execute(mjdStart + 16385*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16390*second) + ' since array.time is ' + str(array.time())

# Scan 414 = No0415
subarray.setSource(source51)
print 'Not a recording scan but still set switches for No0415.'
subarray.setSwitches(mjdStart + 16466*second, mjdStart+16531*second, obsCode+'_'+stnCode+'_'+'No0415')
if array.time() < mjdStart + (16531-10)*second:
  subarray.execute(mjdStart + 16526*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16531*second) + ' since array.time is ' + str(array.time())

# Scan 415 = No0416
subarray.setSource(source52)
print 'Not a recording scan but still set switches for No0416.'
subarray.setSwitches(mjdStart + 16580*second, mjdStart+16640*second, obsCode+'_'+stnCode+'_'+'No0416')
if array.time() < mjdStart + (16640-10)*second:
  subarray.execute(mjdStart + 16635*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16640*second) + ' since array.time is ' + str(array.time())

# Scan 416 = No0417
subarray.setSource(source53)
print 'Not a recording scan but still set switches for No0417.'
subarray.setSwitches(mjdStart + 16677*second, mjdStart+16764*second, obsCode+'_'+stnCode+'_'+'No0417')
if array.time() < mjdStart + (16764-10)*second:
  subarray.execute(mjdStart + 16759*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16764*second) + ' since array.time is ' + str(array.time())

# Scan 417 = No0418
subarray.setSource(source54)
print 'Not a recording scan but still set switches for No0418.'
subarray.setSwitches(mjdStart + 16788*second, mjdStart+16860*second, obsCode+'_'+stnCode+'_'+'No0418')
if array.time() < mjdStart + (16860-10)*second:
  subarray.execute(mjdStart + 16855*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+16860*second) + ' since array.time is ' + str(array.time())

# Scan 418 = No0419
subarray.setSource(source55)
print 'Not a recording scan but still set switches for No0419.'
subarray.setSwitches(mjdStart + 16984*second, mjdStart+17049*second, obsCode+'_'+stnCode+'_'+'No0419')
if array.time() < mjdStart + (17049-10)*second:
  subarray.execute(mjdStart + 17044*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17049*second) + ' since array.time is ' + str(array.time())

# Scan 419 = No0420
subarray.setSource(source56)
print 'Not a recording scan but still set switches for No0420.'
subarray.setSwitches(mjdStart + 17078*second, mjdStart+17166*second, obsCode+'_'+stnCode+'_'+'No0420')
if array.time() < mjdStart + (17166-10)*second:
  subarray.execute(mjdStart + 17161*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17166*second) + ' since array.time is ' + str(array.time())

# Scan 420 = No0421
subarray.setSource(source44)
print 'Not a recording scan but still set switches for No0421.'
subarray.setSwitches(mjdStart + 17200*second, mjdStart+17280*second, obsCode+'_'+stnCode+'_'+'No0421')
if array.time() < mjdStart + (17280-10)*second:
  subarray.execute(mjdStart + 17275*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17280*second) + ' since array.time is ' + str(array.time())

# Scan 421 = No0422
subarray.setSource(source57)
print 'Not a recording scan but still set switches for No0422.'
subarray.setSwitches(mjdStart + 17489*second, mjdStart+17554*second, obsCode+'_'+stnCode+'_'+'No0422')
if array.time() < mjdStart + (17554-10)*second:
  subarray.execute(mjdStart + 17549*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17554*second) + ' since array.time is ' + str(array.time())

# Scan 422 = No0423
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0423.'
subarray.setSwitches(mjdStart + 17786*second, mjdStart+17807*second, obsCode+'_'+stnCode+'_'+'No0423')
if array.time() < mjdStart + (17807-10)*second:
  subarray.execute(mjdStart + 17802*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17807*second) + ' since array.time is ' + str(array.time())

# Scan 423 = No0424
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0424.'
subarray.setSwitches(mjdStart + 17815*second, mjdStart+17837*second, obsCode+'_'+stnCode+'_'+'No0424')
if array.time() < mjdStart + (17837-10)*second:
  subarray.execute(mjdStart + 17832*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17837*second) + ' since array.time is ' + str(array.time())

# Scan 424 = No0425
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0425.'
subarray.setSwitches(mjdStart + 17845*second, mjdStart+17867*second, obsCode+'_'+stnCode+'_'+'No0425')
if array.time() < mjdStart + (17867-10)*second:
  subarray.execute(mjdStart + 17862*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17867*second) + ' since array.time is ' + str(array.time())

# Scan 425 = No0426
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0426.'
subarray.setSwitches(mjdStart + 17879*second, mjdStart+17897*second, obsCode+'_'+stnCode+'_'+'No0426')
if array.time() < mjdStart + (17897-10)*second:
  subarray.execute(mjdStart + 17892*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17897*second) + ' since array.time is ' + str(array.time())

# Scan 426 = No0427
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0427.'
subarray.setSwitches(mjdStart + 17909*second, mjdStart+17927*second, obsCode+'_'+stnCode+'_'+'No0427')
if array.time() < mjdStart + (17927-10)*second:
  subarray.execute(mjdStart + 17922*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17927*second) + ' since array.time is ' + str(array.time())

# Scan 427 = No0428
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0428.'
subarray.setSwitches(mjdStart + 17937*second, mjdStart+17957*second, obsCode+'_'+stnCode+'_'+'No0428')
if array.time() < mjdStart + (17957-10)*second:
  subarray.execute(mjdStart + 17952*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17957*second) + ' since array.time is ' + str(array.time())

# Scan 428 = No0429
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0429.'
subarray.setSwitches(mjdStart + 17967*second, mjdStart+17987*second, obsCode+'_'+stnCode+'_'+'No0429')
if array.time() < mjdStart + (17987-10)*second:
  subarray.execute(mjdStart + 17982*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+17987*second) + ' since array.time is ' + str(array.time())

# Scan 429 = No0430
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0430.'
subarray.setSwitches(mjdStart + 17995*second, mjdStart+18017*second, obsCode+'_'+stnCode+'_'+'No0430')
if array.time() < mjdStart + (18017-10)*second:
  subarray.execute(mjdStart + 18012*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18017*second) + ' since array.time is ' + str(array.time())

# Scan 430 = No0431
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0431.'
subarray.setSwitches(mjdStart + 18025*second, mjdStart+18046*second, obsCode+'_'+stnCode+'_'+'No0431')
if array.time() < mjdStart + (18046-10)*second:
  subarray.execute(mjdStart + 18041*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18046*second) + ' since array.time is ' + str(array.time())

# Scan 431 = No0432
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0432.'
subarray.setSwitches(mjdStart + 18058*second, mjdStart+18076*second, obsCode+'_'+stnCode+'_'+'No0432')
if array.time() < mjdStart + (18076-10)*second:
  subarray.execute(mjdStart + 18071*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18076*second) + ' since array.time is ' + str(array.time())

# Scan 432 = No0433
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0433.'
subarray.setSwitches(mjdStart + 18088*second, mjdStart+18106*second, obsCode+'_'+stnCode+'_'+'No0433')
if array.time() < mjdStart + (18106-10)*second:
  subarray.execute(mjdStart + 18101*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18106*second) + ' since array.time is ' + str(array.time())

# Scan 433 = No0434
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0434.'
subarray.setSwitches(mjdStart + 18116*second, mjdStart+18136*second, obsCode+'_'+stnCode+'_'+'No0434')
if array.time() < mjdStart + (18136-10)*second:
  subarray.execute(mjdStart + 18131*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18136*second) + ' since array.time is ' + str(array.time())

# Scan 434 = No0435
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0435.'
subarray.setSwitches(mjdStart + 18146*second, mjdStart+18166*second, obsCode+'_'+stnCode+'_'+'No0435')
if array.time() < mjdStart + (18166-10)*second:
  subarray.execute(mjdStart + 18161*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18166*second) + ' since array.time is ' + str(array.time())

# Scan 435 = No0436
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0436.'
subarray.setSwitches(mjdStart + 18174*second, mjdStart+18196*second, obsCode+'_'+stnCode+'_'+'No0436')
if array.time() < mjdStart + (18196-10)*second:
  subarray.execute(mjdStart + 18191*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18196*second) + ' since array.time is ' + str(array.time())

# Scan 436 = No0437
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0437.'
subarray.setSwitches(mjdStart + 18204*second, mjdStart+18226*second, obsCode+'_'+stnCode+'_'+'No0437')
if array.time() < mjdStart + (18226-10)*second:
  subarray.execute(mjdStart + 18221*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18226*second) + ' since array.time is ' + str(array.time())

# Scan 437 = No0438
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0438.'
subarray.setSwitches(mjdStart + 18238*second, mjdStart+18256*second, obsCode+'_'+stnCode+'_'+'No0438')
if array.time() < mjdStart + (18256-10)*second:
  subarray.execute(mjdStart + 18251*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18256*second) + ' since array.time is ' + str(array.time())

# Scan 438 = No0439
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0439.'
subarray.setSwitches(mjdStart + 18268*second, mjdStart+18286*second, obsCode+'_'+stnCode+'_'+'No0439')
if array.time() < mjdStart + (18286-10)*second:
  subarray.execute(mjdStart + 18281*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18286*second) + ' since array.time is ' + str(array.time())

# Scan 439 = No0440
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0440.'
subarray.setSwitches(mjdStart + 18296*second, mjdStart+18316*second, obsCode+'_'+stnCode+'_'+'No0440')
if array.time() < mjdStart + (18316-10)*second:
  subarray.execute(mjdStart + 18311*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18316*second) + ' since array.time is ' + str(array.time())

# Scan 440 = No0441
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0441.'
subarray.setSwitches(mjdStart + 18326*second, mjdStart+18346*second, obsCode+'_'+stnCode+'_'+'No0441')
if array.time() < mjdStart + (18346-10)*second:
  subarray.execute(mjdStart + 18341*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18346*second) + ' since array.time is ' + str(array.time())

# Scan 441 = No0442
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0442.'
subarray.setSwitches(mjdStart + 18354*second, mjdStart+18376*second, obsCode+'_'+stnCode+'_'+'No0442')
if array.time() < mjdStart + (18376-10)*second:
  subarray.execute(mjdStart + 18371*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18376*second) + ' since array.time is ' + str(array.time())

# Scan 442 = No0443
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0443.'
subarray.setSwitches(mjdStart + 18384*second, mjdStart+18405*second, obsCode+'_'+stnCode+'_'+'No0443')
if array.time() < mjdStart + (18405-10)*second:
  subarray.execute(mjdStart + 18400*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18405*second) + ' since array.time is ' + str(array.time())

# Scan 443 = No0444
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0444.'
subarray.setSwitches(mjdStart + 18417*second, mjdStart+18435*second, obsCode+'_'+stnCode+'_'+'No0444')
if array.time() < mjdStart + (18435-10)*second:
  subarray.execute(mjdStart + 18430*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18435*second) + ' since array.time is ' + str(array.time())

# Scan 444 = No0445
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0445.'
subarray.setSwitches(mjdStart + 18447*second, mjdStart+18465*second, obsCode+'_'+stnCode+'_'+'No0445')
if array.time() < mjdStart + (18465-10)*second:
  subarray.execute(mjdStart + 18460*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18465*second) + ' since array.time is ' + str(array.time())

# Scan 445 = No0446
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0446.'
subarray.setSwitches(mjdStart + 18475*second, mjdStart+18495*second, obsCode+'_'+stnCode+'_'+'No0446')
if array.time() < mjdStart + (18495-10)*second:
  subarray.execute(mjdStart + 18490*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18495*second) + ' since array.time is ' + str(array.time())

# Scan 446 = No0447
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0447.'
subarray.setSwitches(mjdStart + 18505*second, mjdStart+18525*second, obsCode+'_'+stnCode+'_'+'No0447')
if array.time() < mjdStart + (18525-10)*second:
  subarray.execute(mjdStart + 18520*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18525*second) + ' since array.time is ' + str(array.time())

# Scan 447 = No0448
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0448.'
subarray.setSwitches(mjdStart + 18533*second, mjdStart+18550*second, obsCode+'_'+stnCode+'_'+'No0448')
if array.time() < mjdStart + (18550-10)*second:
  subarray.execute(mjdStart + 18545*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18550*second) + ' since array.time is ' + str(array.time())

# Scan 448 = No0449
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0449.'
subarray.setSwitches(mjdStart + 18560*second, mjdStart+18580*second, obsCode+'_'+stnCode+'_'+'No0449')
if array.time() < mjdStart + (18580-10)*second:
  subarray.execute(mjdStart + 18575*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18580*second) + ' since array.time is ' + str(array.time())

# Scan 449 = No0450
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0450.'
subarray.setSwitches(mjdStart + 18590*second, mjdStart+18610*second, obsCode+'_'+stnCode+'_'+'No0450')
if array.time() < mjdStart + (18610-10)*second:
  subarray.execute(mjdStart + 18605*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18610*second) + ' since array.time is ' + str(array.time())

# Scan 450 = No0451
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0451.'
subarray.setSwitches(mjdStart + 18620*second, mjdStart+18640*second, obsCode+'_'+stnCode+'_'+'No0451')
if array.time() < mjdStart + (18640-10)*second:
  subarray.execute(mjdStart + 18635*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18640*second) + ' since array.time is ' + str(array.time())

# Scan 451 = No0452
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0452.'
subarray.setSwitches(mjdStart + 18650*second, mjdStart+18670*second, obsCode+'_'+stnCode+'_'+'No0452')
if array.time() < mjdStart + (18670-10)*second:
  subarray.execute(mjdStart + 18665*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18670*second) + ' since array.time is ' + str(array.time())

# Scan 452 = No0453
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0453.'
subarray.setSwitches(mjdStart + 18680*second, mjdStart+18700*second, obsCode+'_'+stnCode+'_'+'No0453')
if array.time() < mjdStart + (18700-10)*second:
  subarray.execute(mjdStart + 18695*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18700*second) + ' since array.time is ' + str(array.time())

# Scan 453 = No0454
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0454.'
subarray.setSwitches(mjdStart + 18710*second, mjdStart+18730*second, obsCode+'_'+stnCode+'_'+'No0454')
if array.time() < mjdStart + (18730-10)*second:
  subarray.execute(mjdStart + 18725*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18730*second) + ' since array.time is ' + str(array.time())

# Scan 454 = No0455
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0455.'
subarray.setSwitches(mjdStart + 18743*second, mjdStart+18759*second, obsCode+'_'+stnCode+'_'+'No0455')
if array.time() < mjdStart + (18759-10)*second:
  subarray.execute(mjdStart + 18754*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18759*second) + ' since array.time is ' + str(array.time())

# Scan 455 = No0456
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0456.'
subarray.setSwitches(mjdStart + 18772*second, mjdStart+18789*second, obsCode+'_'+stnCode+'_'+'No0456')
if array.time() < mjdStart + (18789-10)*second:
  subarray.execute(mjdStart + 18784*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18789*second) + ' since array.time is ' + str(array.time())

# Scan 456 = No0457
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0457.'
subarray.setSwitches(mjdStart + 18802*second, mjdStart+18819*second, obsCode+'_'+stnCode+'_'+'No0457')
if array.time() < mjdStart + (18819-10)*second:
  subarray.execute(mjdStart + 18814*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18819*second) + ' since array.time is ' + str(array.time())

# Scan 457 = No0458
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0458.'
subarray.setSwitches(mjdStart + 18832*second, mjdStart+18849*second, obsCode+'_'+stnCode+'_'+'No0458')
if array.time() < mjdStart + (18849-10)*second:
  subarray.execute(mjdStart + 18844*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18849*second) + ' since array.time is ' + str(array.time())

# Scan 458 = No0459
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0459.'
subarray.setSwitches(mjdStart + 18862*second, mjdStart+18879*second, obsCode+'_'+stnCode+'_'+'No0459')
if array.time() < mjdStart + (18879-10)*second:
  subarray.execute(mjdStart + 18874*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18879*second) + ' since array.time is ' + str(array.time())

# Scan 459 = No0460
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0460.'
subarray.setSwitches(mjdStart + 18892*second, mjdStart+18909*second, obsCode+'_'+stnCode+'_'+'No0460')
if array.time() < mjdStart + (18909-10)*second:
  subarray.execute(mjdStart + 18904*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18909*second) + ' since array.time is ' + str(array.time())

# Scan 460 = No0461
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0461.'
subarray.setSwitches(mjdStart + 18923*second, mjdStart+18939*second, obsCode+'_'+stnCode+'_'+'No0461')
if array.time() < mjdStart + (18939-10)*second:
  subarray.execute(mjdStart + 18934*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18939*second) + ' since array.time is ' + str(array.time())

# Scan 461 = No0462
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0462.'
subarray.setSwitches(mjdStart + 18953*second, mjdStart+18969*second, obsCode+'_'+stnCode+'_'+'No0462')
if array.time() < mjdStart + (18969-10)*second:
  subarray.execute(mjdStart + 18964*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18969*second) + ' since array.time is ' + str(array.time())

# Scan 462 = No0463
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0463.'
subarray.setSwitches(mjdStart + 18983*second, mjdStart+18999*second, obsCode+'_'+stnCode+'_'+'No0463')
if array.time() < mjdStart + (18999-10)*second:
  subarray.execute(mjdStart + 18994*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+18999*second) + ' since array.time is ' + str(array.time())

# Scan 463 = No0464
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0464.'
subarray.setSwitches(mjdStart + 19013*second, mjdStart+19029*second, obsCode+'_'+stnCode+'_'+'No0464')
if array.time() < mjdStart + (19029-10)*second:
  subarray.execute(mjdStart + 19024*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19029*second) + ' since array.time is ' + str(array.time())

# Scan 464 = No0465
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0465.'
subarray.setSwitches(mjdStart + 19043*second, mjdStart+19059*second, obsCode+'_'+stnCode+'_'+'No0465')
if array.time() < mjdStart + (19059-10)*second:
  subarray.execute(mjdStart + 19054*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19059*second) + ' since array.time is ' + str(array.time())

# Scan 465 = No0466
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0466.'
subarray.setSwitches(mjdStart + 19073*second, mjdStart+19089*second, obsCode+'_'+stnCode+'_'+'No0466')
if array.time() < mjdStart + (19089-10)*second:
  subarray.execute(mjdStart + 19084*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19089*second) + ' since array.time is ' + str(array.time())

# Scan 466 = No0467
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0467.'
subarray.setSwitches(mjdStart + 19099*second, mjdStart+19118*second, obsCode+'_'+stnCode+'_'+'No0467')
if array.time() < mjdStart + (19118-10)*second:
  subarray.execute(mjdStart + 19113*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19118*second) + ' since array.time is ' + str(array.time())

# Scan 467 = No0468
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0468.'
subarray.setSwitches(mjdStart + 19128*second, mjdStart+19148*second, obsCode+'_'+stnCode+'_'+'No0468')
if array.time() < mjdStart + (19148-10)*second:
  subarray.execute(mjdStart + 19143*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19148*second) + ' since array.time is ' + str(array.time())

# Scan 468 = No0469
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0469.'
subarray.setSwitches(mjdStart + 19158*second, mjdStart+19178*second, obsCode+'_'+stnCode+'_'+'No0469')
if array.time() < mjdStart + (19178-10)*second:
  subarray.execute(mjdStart + 19173*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19178*second) + ' since array.time is ' + str(array.time())

# Scan 469 = No0470
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0470.'
subarray.setSwitches(mjdStart + 19188*second, mjdStart+19208*second, obsCode+'_'+stnCode+'_'+'No0470')
if array.time() < mjdStart + (19208-10)*second:
  subarray.execute(mjdStart + 19203*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19208*second) + ' since array.time is ' + str(array.time())

# Scan 470 = No0471
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0471.'
subarray.setSwitches(mjdStart + 19218*second, mjdStart+19238*second, obsCode+'_'+stnCode+'_'+'No0471')
if array.time() < mjdStart + (19238-10)*second:
  subarray.execute(mjdStart + 19233*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19238*second) + ' since array.time is ' + str(array.time())

# Scan 471 = No0472
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0472.'
subarray.setSwitches(mjdStart + 19248*second, mjdStart+19268*second, obsCode+'_'+stnCode+'_'+'No0472')
if array.time() < mjdStart + (19268-10)*second:
  subarray.execute(mjdStart + 19263*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19268*second) + ' since array.time is ' + str(array.time())

# Scan 472 = No0473
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0473.'
subarray.setSwitches(mjdStart + 19279*second, mjdStart+19294*second, obsCode+'_'+stnCode+'_'+'No0473')
if array.time() < mjdStart + (19294-10)*second:
  subarray.execute(mjdStart + 19289*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19294*second) + ' since array.time is ' + str(array.time())

# Scan 473 = No0474
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0474.'
subarray.setSwitches(mjdStart + 19305*second, mjdStart+19324*second, obsCode+'_'+stnCode+'_'+'No0474')
if array.time() < mjdStart + (19324-10)*second:
  subarray.execute(mjdStart + 19319*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19324*second) + ' since array.time is ' + str(array.time())

# Scan 474 = No0475
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0475.'
subarray.setSwitches(mjdStart + 19335*second, mjdStart+19354*second, obsCode+'_'+stnCode+'_'+'No0475')
if array.time() < mjdStart + (19354-10)*second:
  subarray.execute(mjdStart + 19349*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19354*second) + ' since array.time is ' + str(array.time())

# Scan 475 = No0476
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0476.'
subarray.setSwitches(mjdStart + 19365*second, mjdStart+19384*second, obsCode+'_'+stnCode+'_'+'No0476')
if array.time() < mjdStart + (19384-10)*second:
  subarray.execute(mjdStart + 19379*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19384*second) + ' since array.time is ' + str(array.time())

# Scan 476 = No0477
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0477.'
subarray.setSwitches(mjdStart + 19395*second, mjdStart+19414*second, obsCode+'_'+stnCode+'_'+'No0477')
if array.time() < mjdStart + (19414-10)*second:
  subarray.execute(mjdStart + 19409*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19414*second) + ' since array.time is ' + str(array.time())

# Scan 477 = No0478
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0478.'
subarray.setSwitches(mjdStart + 19425*second, mjdStart+19444*second, obsCode+'_'+stnCode+'_'+'No0478')
if array.time() < mjdStart + (19444-10)*second:
  subarray.execute(mjdStart + 19439*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19444*second) + ' since array.time is ' + str(array.time())

# Scan 478 = No0479
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0479.'
subarray.setSwitches(mjdStart + 19455*second, mjdStart+19474*second, obsCode+'_'+stnCode+'_'+'No0479')
if array.time() < mjdStart + (19474-10)*second:
  subarray.execute(mjdStart + 19469*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19474*second) + ' since array.time is ' + str(array.time())

# Scan 479 = No0480
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0480.'
subarray.setSwitches(mjdStart + 19484*second, mjdStart+19504*second, obsCode+'_'+stnCode+'_'+'No0480')
if array.time() < mjdStart + (19504-10)*second:
  subarray.execute(mjdStart + 19499*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19504*second) + ' since array.time is ' + str(array.time())

# Scan 480 = No0481
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0481.'
subarray.setSwitches(mjdStart + 19514*second, mjdStart+19534*second, obsCode+'_'+stnCode+'_'+'No0481')
if array.time() < mjdStart + (19534-10)*second:
  subarray.execute(mjdStart + 19529*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19534*second) + ' since array.time is ' + str(array.time())

# Scan 481 = No0482
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0482.'
subarray.setSwitches(mjdStart + 19544*second, mjdStart+19564*second, obsCode+'_'+stnCode+'_'+'No0482')
if array.time() < mjdStart + (19564-10)*second:
  subarray.execute(mjdStart + 19559*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19564*second) + ' since array.time is ' + str(array.time())

# Scan 482 = No0483
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0483.'
subarray.setSwitches(mjdStart + 19574*second, mjdStart+19594*second, obsCode+'_'+stnCode+'_'+'No0483')
if array.time() < mjdStart + (19594-10)*second:
  subarray.execute(mjdStart + 19589*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19594*second) + ' since array.time is ' + str(array.time())

# Scan 483 = No0484
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0484.'
subarray.setSwitches(mjdStart + 19604*second, mjdStart+19624*second, obsCode+'_'+stnCode+'_'+'No0484')
if array.time() < mjdStart + (19624-10)*second:
  subarray.execute(mjdStart + 19619*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19624*second) + ' since array.time is ' + str(array.time())

# Scan 484 = No0485
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0485.'
subarray.setSwitches(mjdStart + 19634*second, mjdStart+19654*second, obsCode+'_'+stnCode+'_'+'No0485')
if array.time() < mjdStart + (19654-10)*second:
  subarray.execute(mjdStart + 19649*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19654*second) + ' since array.time is ' + str(array.time())

# Scan 485 = No0486
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0486.'
subarray.setSwitches(mjdStart + 19664*second, mjdStart+19683*second, obsCode+'_'+stnCode+'_'+'No0486')
if array.time() < mjdStart + (19683-10)*second:
  subarray.execute(mjdStart + 19678*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19683*second) + ' since array.time is ' + str(array.time())

# Scan 486 = No0487
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0487.'
subarray.setSwitches(mjdStart + 19693*second, mjdStart+19713*second, obsCode+'_'+stnCode+'_'+'No0487')
if array.time() < mjdStart + (19713-10)*second:
  subarray.execute(mjdStart + 19708*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19713*second) + ' since array.time is ' + str(array.time())

# Scan 487 = No0488
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0488.'
subarray.setSwitches(mjdStart + 19723*second, mjdStart+19743*second, obsCode+'_'+stnCode+'_'+'No0488')
if array.time() < mjdStart + (19743-10)*second:
  subarray.execute(mjdStart + 19738*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19743*second) + ' since array.time is ' + str(array.time())

# Scan 488 = No0489
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0489.'
subarray.setSwitches(mjdStart + 19753*second, mjdStart+19773*second, obsCode+'_'+stnCode+'_'+'No0489')
if array.time() < mjdStart + (19773-10)*second:
  subarray.execute(mjdStart + 19768*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19773*second) + ' since array.time is ' + str(array.time())

# Scan 489 = No0490
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0490.'
subarray.setSwitches(mjdStart + 19783*second, mjdStart+19803*second, obsCode+'_'+stnCode+'_'+'No0490')
if array.time() < mjdStart + (19803-10)*second:
  subarray.execute(mjdStart + 19798*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19803*second) + ' since array.time is ' + str(array.time())

# Scan 490 = No0491
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0491.'
subarray.setSwitches(mjdStart + 19813*second, mjdStart+19833*second, obsCode+'_'+stnCode+'_'+'No0491')
if array.time() < mjdStart + (19833-10)*second:
  subarray.execute(mjdStart + 19828*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19833*second) + ' since array.time is ' + str(array.time())

# Scan 491 = No0492
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0492.'
subarray.setSwitches(mjdStart + 19845*second, mjdStart+19863*second, obsCode+'_'+stnCode+'_'+'No0492')
if array.time() < mjdStart + (19863-10)*second:
  subarray.execute(mjdStart + 19858*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19863*second) + ' since array.time is ' + str(array.time())

# Scan 492 = No0493
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0493.'
subarray.setSwitches(mjdStart + 19875*second, mjdStart+19893*second, obsCode+'_'+stnCode+'_'+'No0493')
if array.time() < mjdStart + (19893-10)*second:
  subarray.execute(mjdStart + 19888*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19893*second) + ' since array.time is ' + str(array.time())

# Scan 493 = No0494
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0494.'
subarray.setSwitches(mjdStart + 19905*second, mjdStart+19923*second, obsCode+'_'+stnCode+'_'+'No0494')
if array.time() < mjdStart + (19923-10)*second:
  subarray.execute(mjdStart + 19918*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19923*second) + ' since array.time is ' + str(array.time())

# Scan 494 = No0495
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0495.'
subarray.setSwitches(mjdStart + 19935*second, mjdStart+19953*second, obsCode+'_'+stnCode+'_'+'No0495')
if array.time() < mjdStart + (19953-10)*second:
  subarray.execute(mjdStart + 19948*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19953*second) + ' since array.time is ' + str(array.time())

# Scan 495 = No0496
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0496.'
subarray.setSwitches(mjdStart + 19965*second, mjdStart+19983*second, obsCode+'_'+stnCode+'_'+'No0496')
if array.time() < mjdStart + (19983-10)*second:
  subarray.execute(mjdStart + 19978*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+19983*second) + ' since array.time is ' + str(array.time())

# Scan 496 = No0497
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0497.'
subarray.setSwitches(mjdStart + 19995*second, mjdStart+20013*second, obsCode+'_'+stnCode+'_'+'No0497')
if array.time() < mjdStart + (20013-10)*second:
  subarray.execute(mjdStart + 20008*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20013*second) + ' since array.time is ' + str(array.time())

# Scan 497 = No0498
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0498.'
subarray.setSwitches(mjdStart + 20023*second, mjdStart+20043*second, obsCode+'_'+stnCode+'_'+'No0498')
if array.time() < mjdStart + (20043-10)*second:
  subarray.execute(mjdStart + 20038*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20043*second) + ' since array.time is ' + str(array.time())

# Scan 498 = No0499
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0499.'
subarray.setSwitches(mjdStart + 20051*second, mjdStart+20073*second, obsCode+'_'+stnCode+'_'+'No0499')
if array.time() < mjdStart + (20073-10)*second:
  subarray.execute(mjdStart + 20068*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20073*second) + ' since array.time is ' + str(array.time())

# Scan 499 = No0500
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0500.'
subarray.setSwitches(mjdStart + 20082*second, mjdStart+20103*second, obsCode+'_'+stnCode+'_'+'No0500')
if array.time() < mjdStart + (20103-10)*second:
  subarray.execute(mjdStart + 20098*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20103*second) + ' since array.time is ' + str(array.time())

# Scan 500 = No0501
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0501.'
subarray.setSwitches(mjdStart + 20115*second, mjdStart+20133*second, obsCode+'_'+stnCode+'_'+'No0501')
if array.time() < mjdStart + (20133-10)*second:
  subarray.execute(mjdStart + 20128*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20133*second) + ' since array.time is ' + str(array.time())

# Scan 501 = No0502
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0502.'
subarray.setSwitches(mjdStart + 20145*second, mjdStart+20163*second, obsCode+'_'+stnCode+'_'+'No0502')
if array.time() < mjdStart + (20163-10)*second:
  subarray.execute(mjdStart + 20158*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20163*second) + ' since array.time is ' + str(array.time())

# Scan 502 = No0503
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0503.'
subarray.setSwitches(mjdStart + 20173*second, mjdStart+20193*second, obsCode+'_'+stnCode+'_'+'No0503')
if array.time() < mjdStart + (20193-10)*second:
  subarray.execute(mjdStart + 20188*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20193*second) + ' since array.time is ' + str(array.time())

# Scan 503 = No0504
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0504.'
subarray.setSwitches(mjdStart + 20203*second, mjdStart+20223*second, obsCode+'_'+stnCode+'_'+'No0504')
if array.time() < mjdStart + (20223-10)*second:
  subarray.execute(mjdStart + 20218*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20223*second) + ' since array.time is ' + str(array.time())

# Scan 504 = No0505
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0505.'
subarray.setSwitches(mjdStart + 20231*second, mjdStart+20252*second, obsCode+'_'+stnCode+'_'+'No0505')
if array.time() < mjdStart + (20252-10)*second:
  subarray.execute(mjdStart + 20247*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20252*second) + ' since array.time is ' + str(array.time())

# Scan 505 = No0506
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0506.'
subarray.setSwitches(mjdStart + 20261*second, mjdStart+20282*second, obsCode+'_'+stnCode+'_'+'No0506')
if array.time() < mjdStart + (20282-10)*second:
  subarray.execute(mjdStart + 20277*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20282*second) + ' since array.time is ' + str(array.time())

# Scan 506 = No0507
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0507.'
subarray.setSwitches(mjdStart + 20294*second, mjdStart+20312*second, obsCode+'_'+stnCode+'_'+'No0507')
if array.time() < mjdStart + (20312-10)*second:
  subarray.execute(mjdStart + 20307*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20312*second) + ' since array.time is ' + str(array.time())

# Scan 507 = No0508
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0508.'
subarray.setSwitches(mjdStart + 20324*second, mjdStart+20342*second, obsCode+'_'+stnCode+'_'+'No0508')
if array.time() < mjdStart + (20342-10)*second:
  subarray.execute(mjdStart + 20337*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20342*second) + ' since array.time is ' + str(array.time())

# Scan 508 = No0509
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0509.'
subarray.setSwitches(mjdStart + 20352*second, mjdStart+20372*second, obsCode+'_'+stnCode+'_'+'No0509')
if array.time() < mjdStart + (20372-10)*second:
  subarray.execute(mjdStart + 20367*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20372*second) + ' since array.time is ' + str(array.time())

# Scan 509 = No0510
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0510.'
subarray.setSwitches(mjdStart + 20382*second, mjdStart+20402*second, obsCode+'_'+stnCode+'_'+'No0510')
if array.time() < mjdStart + (20402-10)*second:
  subarray.execute(mjdStart + 20397*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20402*second) + ' since array.time is ' + str(array.time())

# Scan 510 = No0511
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0511.'
subarray.setSwitches(mjdStart + 20410*second, mjdStart+20432*second, obsCode+'_'+stnCode+'_'+'No0511')
if array.time() < mjdStart + (20432-10)*second:
  subarray.execute(mjdStart + 20427*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20432*second) + ' since array.time is ' + str(array.time())

# Scan 511 = No0512
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0512.'
subarray.setSwitches(mjdStart + 20441*second, mjdStart+20462*second, obsCode+'_'+stnCode+'_'+'No0512')
if array.time() < mjdStart + (20462-10)*second:
  subarray.execute(mjdStart + 20457*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20462*second) + ' since array.time is ' + str(array.time())

# Scan 512 = No0513
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0513.'
subarray.setSwitches(mjdStart + 20474*second, mjdStart+20492*second, obsCode+'_'+stnCode+'_'+'No0513')
if array.time() < mjdStart + (20492-10)*second:
  subarray.execute(mjdStart + 20487*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20492*second) + ' since array.time is ' + str(array.time())

# Scan 513 = No0514
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0514.'
subarray.setSwitches(mjdStart + 20504*second, mjdStart+20522*second, obsCode+'_'+stnCode+'_'+'No0514')
if array.time() < mjdStart + (20522-10)*second:
  subarray.execute(mjdStart + 20517*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20522*second) + ' since array.time is ' + str(array.time())

# Scan 514 = No0515
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0515.'
subarray.setSwitches(mjdStart + 20532*second, mjdStart+20552*second, obsCode+'_'+stnCode+'_'+'No0515')
if array.time() < mjdStart + (20552-10)*second:
  subarray.execute(mjdStart + 20547*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20552*second) + ' since array.time is ' + str(array.time())

# Scan 515 = No0516
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0516.'
subarray.setSwitches(mjdStart + 20562*second, mjdStart+20582*second, obsCode+'_'+stnCode+'_'+'No0516')
if array.time() < mjdStart + (20582-10)*second:
  subarray.execute(mjdStart + 20577*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20582*second) + ' since array.time is ' + str(array.time())

# Scan 516 = No0517
subarray.setSource(source1)
print 'Not a recording scan but still set switches for No0517.'
subarray.setSwitches(mjdStart + 20591*second, mjdStart+20612*second, obsCode+'_'+stnCode+'_'+'No0517')
if array.time() < mjdStart + (20612-10)*second:
  subarray.execute(mjdStart + 20607*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20612*second) + ' since array.time is ' + str(array.time())

# Scan 517 = No0518
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0518.'
subarray.setSwitches(mjdStart + 20621*second, mjdStart+20641*second, obsCode+'_'+stnCode+'_'+'No0518')
if array.time() < mjdStart + (20641-10)*second:
  subarray.execute(mjdStart + 20636*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20641*second) + ' since array.time is ' + str(array.time())

# Scan 518 = No0519
subarray.setSource(source3)
print 'Not a recording scan but still set switches for No0519.'
subarray.setSwitches(mjdStart + 20653*second, mjdStart+20671*second, obsCode+'_'+stnCode+'_'+'No0519')
if array.time() < mjdStart + (20671-10)*second:
  subarray.execute(mjdStart + 20666*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20671*second) + ' since array.time is ' + str(array.time())

# Scan 519 = No0520
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0520.'
subarray.setSwitches(mjdStart + 20683*second, mjdStart+20701*second, obsCode+'_'+stnCode+'_'+'No0520')
if array.time() < mjdStart + (20701-10)*second:
  subarray.execute(mjdStart + 20696*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20701*second) + ' since array.time is ' + str(array.time())

# Scan 520 = No0521
subarray.setSource(source2)
print 'Not a recording scan but still set switches for No0521.'
subarray.setSwitches(mjdStart + 20711*second, mjdStart+20731*second, obsCode+'_'+stnCode+'_'+'No0521')
if array.time() < mjdStart + (20731-10)*second:
  subarray.execute(mjdStart + 20726*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20731*second) + ' since array.time is ' + str(array.time())

# Scan 521 = No0522
subarray.setSource(source0)
print 'Not a recording scan but still set switches for No0522.'
subarray.setSwitches(mjdStart + 20741*second, mjdStart+20761*second, obsCode+'_'+stnCode+'_'+'No0522')
if array.time() < mjdStart + (20761-10)*second:
  subarray.execute(mjdStart + 20756*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20761*second) + ' since array.time is ' + str(array.time())

# Scan 522 = No0523
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0523.'
subarray.setSwitches(mjdStart + 20769*second, mjdStart+20786*second, obsCode+'_'+stnCode+'_'+'No0523')
if array.time() < mjdStart + (20786-10)*second:
  subarray.execute(mjdStart + 20781*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20786*second) + ' since array.time is ' + str(array.time())

# Scan 523 = No0524
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0524.'
subarray.setSwitches(mjdStart + 20796*second, mjdStart+20816*second, obsCode+'_'+stnCode+'_'+'No0524')
if array.time() < mjdStart + (20816-10)*second:
  subarray.execute(mjdStart + 20811*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20816*second) + ' since array.time is ' + str(array.time())

# Scan 524 = No0525
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0525.'
subarray.setSwitches(mjdStart + 20826*second, mjdStart+20846*second, obsCode+'_'+stnCode+'_'+'No0525')
if array.time() < mjdStart + (20846-10)*second:
  subarray.execute(mjdStart + 20841*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20846*second) + ' since array.time is ' + str(array.time())

# Scan 525 = No0526
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0526.'
subarray.setSwitches(mjdStart + 20856*second, mjdStart+20876*second, obsCode+'_'+stnCode+'_'+'No0526')
if array.time() < mjdStart + (20876-10)*second:
  subarray.execute(mjdStart + 20871*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20876*second) + ' since array.time is ' + str(array.time())

# Scan 526 = No0527
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0527.'
subarray.setSwitches(mjdStart + 20886*second, mjdStart+20906*second, obsCode+'_'+stnCode+'_'+'No0527')
if array.time() < mjdStart + (20906-10)*second:
  subarray.execute(mjdStart + 20901*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20906*second) + ' since array.time is ' + str(array.time())

# Scan 527 = No0528
subarray.setSource(source10)
print 'Not a recording scan but still set switches for No0528.'
subarray.setSwitches(mjdStart + 20916*second, mjdStart+20936*second, obsCode+'_'+stnCode+'_'+'No0528')
if array.time() < mjdStart + (20936-10)*second:
  subarray.execute(mjdStart + 20931*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20936*second) + ' since array.time is ' + str(array.time())

# Scan 528 = No0529
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0529.'
subarray.setSwitches(mjdStart + 20946*second, mjdStart+20966*second, obsCode+'_'+stnCode+'_'+'No0529')
if array.time() < mjdStart + (20966-10)*second:
  subarray.execute(mjdStart + 20961*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20966*second) + ' since array.time is ' + str(array.time())

# Scan 529 = No0530
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0530.'
subarray.setSwitches(mjdStart + 20979*second, mjdStart+20995*second, obsCode+'_'+stnCode+'_'+'No0530')
if array.time() < mjdStart + (20995-10)*second:
  subarray.execute(mjdStart + 20990*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+20995*second) + ' since array.time is ' + str(array.time())

# Scan 530 = No0531
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0531.'
subarray.setSwitches(mjdStart + 21008*second, mjdStart+21025*second, obsCode+'_'+stnCode+'_'+'No0531')
if array.time() < mjdStart + (21025-10)*second:
  subarray.execute(mjdStart + 21020*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21025*second) + ' since array.time is ' + str(array.time())

# Scan 531 = No0532
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0532.'
subarray.setSwitches(mjdStart + 21038*second, mjdStart+21055*second, obsCode+'_'+stnCode+'_'+'No0532')
if array.time() < mjdStart + (21055-10)*second:
  subarray.execute(mjdStart + 21050*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21055*second) + ' since array.time is ' + str(array.time())

# Scan 532 = No0533
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0533.'
subarray.setSwitches(mjdStart + 21068*second, mjdStart+21085*second, obsCode+'_'+stnCode+'_'+'No0533')
if array.time() < mjdStart + (21085-10)*second:
  subarray.execute(mjdStart + 21080*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21085*second) + ' since array.time is ' + str(array.time())

# Scan 533 = No0534
subarray.setSource(source11)
print 'Not a recording scan but still set switches for No0534.'
subarray.setSwitches(mjdStart + 21098*second, mjdStart+21115*second, obsCode+'_'+stnCode+'_'+'No0534')
if array.time() < mjdStart + (21115-10)*second:
  subarray.execute(mjdStart + 21110*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21115*second) + ' since array.time is ' + str(array.time())

# Scan 534 = No0535
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0535.'
subarray.setSwitches(mjdStart + 21128*second, mjdStart+21145*second, obsCode+'_'+stnCode+'_'+'No0535')
if array.time() < mjdStart + (21145-10)*second:
  subarray.execute(mjdStart + 21140*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21145*second) + ' since array.time is ' + str(array.time())

# Scan 535 = No0536
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0536.'
subarray.setSwitches(mjdStart + 21159*second, mjdStart+21175*second, obsCode+'_'+stnCode+'_'+'No0536')
if array.time() < mjdStart + (21175-10)*second:
  subarray.execute(mjdStart + 21170*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21175*second) + ' since array.time is ' + str(array.time())

# Scan 536 = No0537
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0537.'
subarray.setSwitches(mjdStart + 21189*second, mjdStart+21205*second, obsCode+'_'+stnCode+'_'+'No0537')
if array.time() < mjdStart + (21205-10)*second:
  subarray.execute(mjdStart + 21200*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21205*second) + ' since array.time is ' + str(array.time())

# Scan 537 = No0538
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0538.'
subarray.setSwitches(mjdStart + 21219*second, mjdStart+21235*second, obsCode+'_'+stnCode+'_'+'No0538')
if array.time() < mjdStart + (21235-10)*second:
  subarray.execute(mjdStart + 21230*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21235*second) + ' since array.time is ' + str(array.time())

# Scan 538 = No0539
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0539.'
subarray.setSwitches(mjdStart + 21249*second, mjdStart+21265*second, obsCode+'_'+stnCode+'_'+'No0539')
if array.time() < mjdStart + (21265-10)*second:
  subarray.execute(mjdStart + 21260*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21265*second) + ' since array.time is ' + str(array.time())

# Scan 539 = No0540
subarray.setSource(source12)
print 'Not a recording scan but still set switches for No0540.'
subarray.setSwitches(mjdStart + 21279*second, mjdStart+21295*second, obsCode+'_'+stnCode+'_'+'No0540')
if array.time() < mjdStart + (21295-10)*second:
  subarray.execute(mjdStart + 21290*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21295*second) + ' since array.time is ' + str(array.time())

# Scan 540 = No0541
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0541.'
subarray.setSwitches(mjdStart + 21309*second, mjdStart+21325*second, obsCode+'_'+stnCode+'_'+'No0541')
if array.time() < mjdStart + (21325-10)*second:
  subarray.execute(mjdStart + 21320*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21325*second) + ' since array.time is ' + str(array.time())

# Scan 541 = No0542
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0542.'
subarray.setSwitches(mjdStart + 21335*second, mjdStart+21354*second, obsCode+'_'+stnCode+'_'+'No0542')
if array.time() < mjdStart + (21354-10)*second:
  subarray.execute(mjdStart + 21349*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21354*second) + ' since array.time is ' + str(array.time())

# Scan 542 = No0543
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0543.'
subarray.setSwitches(mjdStart + 21364*second, mjdStart+21384*second, obsCode+'_'+stnCode+'_'+'No0543')
if array.time() < mjdStart + (21384-10)*second:
  subarray.execute(mjdStart + 21379*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21384*second) + ' since array.time is ' + str(array.time())

# Scan 543 = No0544
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0544.'
subarray.setSwitches(mjdStart + 21394*second, mjdStart+21414*second, obsCode+'_'+stnCode+'_'+'No0544')
if array.time() < mjdStart + (21414-10)*second:
  subarray.execute(mjdStart + 21409*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21414*second) + ' since array.time is ' + str(array.time())

# Scan 544 = No0545
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0545.'
subarray.setSwitches(mjdStart + 21424*second, mjdStart+21444*second, obsCode+'_'+stnCode+'_'+'No0545')
if array.time() < mjdStart + (21444-10)*second:
  subarray.execute(mjdStart + 21439*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21444*second) + ' since array.time is ' + str(array.time())

# Scan 545 = No0546
subarray.setSource(source13)
print 'Not a recording scan but still set switches for No0546.'
subarray.setSwitches(mjdStart + 21454*second, mjdStart+21474*second, obsCode+'_'+stnCode+'_'+'No0546')
if array.time() < mjdStart + (21474-10)*second:
  subarray.execute(mjdStart + 21469*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21474*second) + ' since array.time is ' + str(array.time())

# Scan 546 = No0547
subarray.setSource(source9)
print 'Not a recording scan but still set switches for No0547.'
subarray.setSwitches(mjdStart + 21484*second, mjdStart+21504*second, obsCode+'_'+stnCode+'_'+'No0547')
if array.time() < mjdStart + (21504-10)*second:
  subarray.execute(mjdStart + 21499*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21504*second) + ' since array.time is ' + str(array.time())

# Scan 547 = No0548
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0548.'
subarray.setSwitches(mjdStart + 21514*second, mjdStart+21531*second, obsCode+'_'+stnCode+'_'+'No0548')
if array.time() < mjdStart + (21531-10)*second:
  subarray.execute(mjdStart + 21526*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21531*second) + ' since array.time is ' + str(array.time())

# Scan 548 = No0549
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0549.'
subarray.setSwitches(mjdStart + 21542*second, mjdStart+21561*second, obsCode+'_'+stnCode+'_'+'No0549')
if array.time() < mjdStart + (21561-10)*second:
  subarray.execute(mjdStart + 21556*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21561*second) + ' since array.time is ' + str(array.time())

# Scan 549 = No0550
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0550.'
subarray.setSwitches(mjdStart + 21572*second, mjdStart+21590*second, obsCode+'_'+stnCode+'_'+'No0550')
if array.time() < mjdStart + (21590-10)*second:
  subarray.execute(mjdStart + 21585*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21590*second) + ' since array.time is ' + str(array.time())

# Scan 550 = No0551
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0551.'
subarray.setSwitches(mjdStart + 21601*second, mjdStart+21620*second, obsCode+'_'+stnCode+'_'+'No0551')
if array.time() < mjdStart + (21620-10)*second:
  subarray.execute(mjdStart + 21615*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21620*second) + ' since array.time is ' + str(array.time())

# Scan 551 = No0552
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0552.'
subarray.setSwitches(mjdStart + 21631*second, mjdStart+21650*second, obsCode+'_'+stnCode+'_'+'No0552')
if array.time() < mjdStart + (21650-10)*second:
  subarray.execute(mjdStart + 21645*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21650*second) + ' since array.time is ' + str(array.time())

# Scan 552 = No0553
subarray.setSource(source5)
print 'Not a recording scan but still set switches for No0553.'
subarray.setSwitches(mjdStart + 21661*second, mjdStart+21680*second, obsCode+'_'+stnCode+'_'+'No0553')
if array.time() < mjdStart + (21680-10)*second:
  subarray.execute(mjdStart + 21675*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21680*second) + ' since array.time is ' + str(array.time())

# Scan 553 = No0554
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0554.'
subarray.setSwitches(mjdStart + 21691*second, mjdStart+21710*second, obsCode+'_'+stnCode+'_'+'No0554')
if array.time() < mjdStart + (21710-10)*second:
  subarray.execute(mjdStart + 21705*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21710*second) + ' since array.time is ' + str(array.time())

# Scan 554 = No0555
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0555.'
subarray.setSwitches(mjdStart + 21720*second, mjdStart+21740*second, obsCode+'_'+stnCode+'_'+'No0555')
if array.time() < mjdStart + (21740-10)*second:
  subarray.execute(mjdStart + 21735*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21740*second) + ' since array.time is ' + str(array.time())

# Scan 555 = No0556
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0556.'
subarray.setSwitches(mjdStart + 21750*second, mjdStart+21770*second, obsCode+'_'+stnCode+'_'+'No0556')
if array.time() < mjdStart + (21770-10)*second:
  subarray.execute(mjdStart + 21765*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21770*second) + ' since array.time is ' + str(array.time())

# Scan 556 = No0557
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0557.'
subarray.setSwitches(mjdStart + 21780*second, mjdStart+21800*second, obsCode+'_'+stnCode+'_'+'No0557')
if array.time() < mjdStart + (21800-10)*second:
  subarray.execute(mjdStart + 21795*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21800*second) + ' since array.time is ' + str(array.time())

# Scan 557 = No0558
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0558.'
subarray.setSwitches(mjdStart + 21810*second, mjdStart+21830*second, obsCode+'_'+stnCode+'_'+'No0558')
if array.time() < mjdStart + (21830-10)*second:
  subarray.execute(mjdStart + 21825*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21830*second) + ' since array.time is ' + str(array.time())

# Scan 558 = No0559
subarray.setSource(source6)
print 'Not a recording scan but still set switches for No0559.'
subarray.setSwitches(mjdStart + 21840*second, mjdStart+21860*second, obsCode+'_'+stnCode+'_'+'No0559')
if array.time() < mjdStart + (21860-10)*second:
  subarray.execute(mjdStart + 21855*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21860*second) + ' since array.time is ' + str(array.time())

# Scan 559 = No0560
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0560.'
subarray.setSwitches(mjdStart + 21870*second, mjdStart+21890*second, obsCode+'_'+stnCode+'_'+'No0560')
if array.time() < mjdStart + (21890-10)*second:
  subarray.execute(mjdStart + 21885*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21890*second) + ' since array.time is ' + str(array.time())

# Scan 560 = No0561
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0561.'
subarray.setSwitches(mjdStart + 21901*second, mjdStart+21920*second, obsCode+'_'+stnCode+'_'+'No0561')
if array.time() < mjdStart + (21920-10)*second:
  subarray.execute(mjdStart + 21915*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21920*second) + ' since array.time is ' + str(array.time())

# Scan 561 = No0562
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0562.'
subarray.setSwitches(mjdStart + 21931*second, mjdStart+21949*second, obsCode+'_'+stnCode+'_'+'No0562')
if array.time() < mjdStart + (21949-10)*second:
  subarray.execute(mjdStart + 21944*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21949*second) + ' since array.time is ' + str(array.time())

# Scan 562 = No0563
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0563.'
subarray.setSwitches(mjdStart + 21960*second, mjdStart+21979*second, obsCode+'_'+stnCode+'_'+'No0563')
if array.time() < mjdStart + (21979-10)*second:
  subarray.execute(mjdStart + 21974*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+21979*second) + ' since array.time is ' + str(array.time())

# Scan 563 = No0564
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0564.'
subarray.setSwitches(mjdStart + 21990*second, mjdStart+22009*second, obsCode+'_'+stnCode+'_'+'No0564')
if array.time() < mjdStart + (22009-10)*second:
  subarray.execute(mjdStart + 22004*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22009*second) + ' since array.time is ' + str(array.time())

# Scan 564 = No0565
subarray.setSource(source7)
print 'Not a recording scan but still set switches for No0565.'
subarray.setSwitches(mjdStart + 22020*second, mjdStart+22039*second, obsCode+'_'+stnCode+'_'+'No0565')
if array.time() < mjdStart + (22039-10)*second:
  subarray.execute(mjdStart + 22034*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22039*second) + ' since array.time is ' + str(array.time())

# Scan 565 = No0566
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0566.'
subarray.setSwitches(mjdStart + 22050*second, mjdStart+22069*second, obsCode+'_'+stnCode+'_'+'No0566')
if array.time() < mjdStart + (22069-10)*second:
  subarray.execute(mjdStart + 22064*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22069*second) + ' since array.time is ' + str(array.time())

# Scan 566 = No0567
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0567.'
subarray.setSwitches(mjdStart + 22080*second, mjdStart+22099*second, obsCode+'_'+stnCode+'_'+'No0567')
if array.time() < mjdStart + (22099-10)*second:
  subarray.execute(mjdStart + 22094*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22099*second) + ' since array.time is ' + str(array.time())

# Scan 567 = No0568
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0568.'
subarray.setSwitches(mjdStart + 22110*second, mjdStart+22129*second, obsCode+'_'+stnCode+'_'+'No0568')
if array.time() < mjdStart + (22129-10)*second:
  subarray.execute(mjdStart + 22124*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22129*second) + ' since array.time is ' + str(array.time())

# Scan 568 = No0569
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0569.'
subarray.setSwitches(mjdStart + 22140*second, mjdStart+22159*second, obsCode+'_'+stnCode+'_'+'No0569')
if array.time() < mjdStart + (22159-10)*second:
  subarray.execute(mjdStart + 22154*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22159*second) + ' since array.time is ' + str(array.time())

# Scan 569 = No0570
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0570.'
subarray.setSwitches(mjdStart + 22170*second, mjdStart+22189*second, obsCode+'_'+stnCode+'_'+'No0570')
if array.time() < mjdStart + (22189-10)*second:
  subarray.execute(mjdStart + 22184*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22189*second) + ' since array.time is ' + str(array.time())

# Scan 570 = No0571
subarray.setSource(source8)
print 'Not a recording scan but still set switches for No0571.'
subarray.setSwitches(mjdStart + 22200*second, mjdStart+22219*second, obsCode+'_'+stnCode+'_'+'No0571')
if array.time() < mjdStart + (22219-10)*second:
  subarray.execute(mjdStart + 22214*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22219*second) + ' since array.time is ' + str(array.time())

# Scan 571 = No0572
subarray.setSource(source4)
print 'Not a recording scan but still set switches for No0572.'
subarray.setSwitches(mjdStart + 22230*second, mjdStart+22249*second, obsCode+'_'+stnCode+'_'+'No0572')
if array.time() < mjdStart + (22249-10)*second:
  subarray.execute(mjdStart + 22244*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22249*second) + ' since array.time is ' + str(array.time())

# Scan 572 = No0573
# changing to mode geodetic
subarray.setVLBALoIfSetup(dbe0, loif0)
subarray.set4x4Switch('1A', 4)
subarray.setChannels(dbe0, channelSet0)
subarray.setSource(source58)
print 'Not a recording scan but still set switches for No0573.'
subarray.setSwitches(mjdStart + 22399*second, mjdStart+22464*second, obsCode+'_'+stnCode+'_'+'No0573')
if array.time() < mjdStart + (22464-10)*second:
  subarray.execute(mjdStart + 22459*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22464*second) + ' since array.time is ' + str(array.time())

# Scan 573 = No0574
subarray.setSource(source59)
print 'Not a recording scan but still set switches for No0574.'
subarray.setSwitches(mjdStart + 22484*second, mjdStart+22553*second, obsCode+'_'+stnCode+'_'+'No0574')
if array.time() < mjdStart + (22553-10)*second:
  subarray.execute(mjdStart + 22548*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22553*second) + ' since array.time is ' + str(array.time())

# Scan 574 = No0575
subarray.setSource(source60)
print 'Not a recording scan but still set switches for No0575.'
subarray.setSwitches(mjdStart + 22622*second, mjdStart+22686*second, obsCode+'_'+stnCode+'_'+'No0575')
if array.time() < mjdStart + (22686-10)*second:
  subarray.execute(mjdStart + 22681*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22686*second) + ' since array.time is ' + str(array.time())

# Scan 575 = No0576
subarray.setSource(source61)
print 'Not a recording scan but still set switches for No0576.'
subarray.setSwitches(mjdStart + 22779*second, mjdStart+22844*second, obsCode+'_'+stnCode+'_'+'No0576')
if array.time() < mjdStart + (22844-10)*second:
  subarray.execute(mjdStart + 22839*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22844*second) + ' since array.time is ' + str(array.time())

# Scan 576 = No0577
subarray.setSource(source62)
print 'Not a recording scan but still set switches for No0577.'
subarray.setSwitches(mjdStart + 22925*second, mjdStart+22990*second, obsCode+'_'+stnCode+'_'+'No0577')
if array.time() < mjdStart + (22990-10)*second:
  subarray.execute(mjdStart + 22985*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+22990*second) + ' since array.time is ' + str(array.time())

# Scan 577 = No0578
subarray.setSource(source63)
print 'Not a recording scan but still set switches for No0578.'
subarray.setSwitches(mjdStart + 23011*second, mjdStart+23077*second, obsCode+'_'+stnCode+'_'+'No0578')
if array.time() < mjdStart + (23077-10)*second:
  subarray.execute(mjdStart + 23072*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23077*second) + ' since array.time is ' + str(array.time())

# Scan 578 = No0579
subarray.setSource(source37)
print 'Not a recording scan but still set switches for No0579.'
subarray.setSwitches(mjdStart + 23191*second, mjdStart+23256*second, obsCode+'_'+stnCode+'_'+'No0579')
if array.time() < mjdStart + (23256-10)*second:
  subarray.execute(mjdStart + 23251*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23256*second) + ' since array.time is ' + str(array.time())

# Scan 579 = No0580
subarray.setSource(source64)
print 'Not a recording scan but still set switches for No0580.'
subarray.setSwitches(mjdStart + 23274*second, mjdStart+23342*second, obsCode+'_'+stnCode+'_'+'No0580')
if array.time() < mjdStart + (23342-10)*second:
  subarray.execute(mjdStart + 23337*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23342*second) + ' since array.time is ' + str(array.time())

# Scan 580 = No0581
subarray.setSource(source65)
print 'Not a recording scan but still set switches for No0581.'
subarray.setSwitches(mjdStart + 23365*second, mjdStart+23439*second, obsCode+'_'+stnCode+'_'+'No0581')
if array.time() < mjdStart + (23439-10)*second:
  subarray.execute(mjdStart + 23434*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23439*second) + ' since array.time is ' + str(array.time())

# Scan 581 = No0582
subarray.setSource(source66)
print 'Not a recording scan but still set switches for No0582.'
subarray.setSwitches(mjdStart + 23461*second, mjdStart+23523*second, obsCode+'_'+stnCode+'_'+'No0582')
if array.time() < mjdStart + (23523-10)*second:
  subarray.execute(mjdStart + 23518*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23523*second) + ' since array.time is ' + str(array.time())

# Scan 582 = No0583
subarray.setSource(source67)
print 'Not a recording scan but still set switches for No0583.'
subarray.setSwitches(mjdStart + 23551*second, mjdStart+23613*second, obsCode+'_'+stnCode+'_'+'No0583')
if array.time() < mjdStart + (23613-10)*second:
  subarray.execute(mjdStart + 23608*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23613*second) + ' since array.time is ' + str(array.time())

# Scan 583 = No0584
subarray.setSource(source68)
print 'Not a recording scan but still set switches for No0584.'
subarray.setSwitches(mjdStart + 23644*second, mjdStart+23720*second, obsCode+'_'+stnCode+'_'+'No0584')
if array.time() < mjdStart + (23720-10)*second:
  subarray.execute(mjdStart + 23715*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23720*second) + ' since array.time is ' + str(array.time())

# Scan 584 = No0585
subarray.setSource(source39)
print 'Not a recording scan but still set switches for No0585.'
subarray.setSwitches(mjdStart + 23924*second, mjdStart+23988*second, obsCode+'_'+stnCode+'_'+'No0585')
if array.time() < mjdStart + (23988-10)*second:
  subarray.execute(mjdStart + 23983*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+23988*second) + ' since array.time is ' + str(array.time())

# Scan 585 = No0586
subarray.setSource(source27)
print 'Not a recording scan but still set switches for No0586.'
subarray.setSwitches(mjdStart + 24251*second, mjdStart+24316*second, obsCode+'_'+stnCode+'_'+'No0586')
if array.time() < mjdStart + (24316-10)*second:
  subarray.execute(mjdStart + 24311*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24316*second) + ' since array.time is ' + str(array.time())

# Scan 586 = No0587
# changing to mode Doppler@G045.07+0.13
subarray.setVLBALoIfSetup(dbe0, loif1)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet1)
subarray.setSource(source23)
print 'Not a recording scan but still set switches for No0587.'
subarray.setSwitches(mjdStart + 24553*second, mjdStart+24618*second, obsCode+'_'+stnCode+'_'+'No0587')
if array.time() < mjdStart + (24618-10)*second:
  subarray.execute(mjdStart + 24613*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24618*second) + ' since array.time is ' + str(array.time())

# Scan 587 = No0588
# changing to mode Doppler@G045.47+0.05
subarray.setVLBALoIfSetup(dbe0, loif2)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet2)
subarray.setSource(source25)
print 'Not a recording scan but still set switches for No0588.'
subarray.setSwitches(mjdStart + 24624*second, mjdStart+24684*second, obsCode+'_'+stnCode+'_'+'No0588')
if array.time() < mjdStart + (24684-10)*second:
  subarray.execute(mjdStart + 24679*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24684*second) + ' since array.time is ' + str(array.time())

# Scan 588 = No0589
# changing to mode Doppler@G043.89-0.78
subarray.setVLBALoIfSetup(dbe0, loif3)
subarray.set4x4Switch('1A', 2)
subarray.set4x4Switch('1B', 4)
subarray.setChannels(dbe0, channelSet3)
subarray.setSource(source24)
print 'Not a recording scan but still set switches for No0589.'
subarray.setSwitches(mjdStart + 24690*second, mjdStart+24749*second, obsCode+'_'+stnCode+'_'+'No0589')
if array.time() < mjdStart + (24749-10)*second:
  subarray.execute(mjdStart + 24744*second)
else:
  print 'Skipping scan which ended at time ' + str(mjdStart+24749*second) + ' since array.time is ' + str(array.time())

array.wait(mjdStart + 24750*second)
