#!/usr/bin/env python


import os, sys, argparse, math, getpass
from astropy.time import Time
from six import chr
from six.moves import range
from six.moves import zip

## Convenience function to parse java style properties file
def load_props(filepath, sep='=', comment_char='#'):
    """
    Read the file passed as parameter as a properties file.
    """
    props = {}
    with open(filepath, "rt") as f:
        for line in f:
            l = line.strip()
            if l and not l.startswith(comment_char):
                key_value = l.split(sep)
                key = key_value[0].strip()
                value = sep.join(key_value[1:]).strip().strip('"') 
                keysplit = key.split('.')
                currentdict = props
                while len(keysplit) > 1:
                    if not keysplit[0] in list(currentdict.keys()):
                        currentdict[keysplit[0]] = {}
                    currentdict = currentdict[keysplit[0]]
                    keysplit = keysplit[1:]
                if "[" in value and "]" in value and not "*" in value:
                    v = []
                    for vx in value.strip()[1:-1].split(','):
                        v.append(vx)
                    value = v
                currentdict[keysplit[0]] = value
    return props

## Convenience function to convert MJD to YMDHMS
def mjd2ymdhms(mjd):
    imjd = int(mjd)
    fmjd = mjd - imjd

    j = imjd + 32044 + 2400001
    g = j // 146097
    dg = j % 146097
    c = ((dg/36524 + 1)*3)//4
    dc = dg - c*36524
    b = dc // 1461
    db = dc % 1461
    a = ((db//365 + 1)*3)//4
    da = db - a*365
    y = g*400 + c*100 + b*4 + a
    m = (da*5 + 308)//153 - 2
    d = da - ((m + 4)*153)//5 + 122

    year = y - 4800 + (m + 2)//12;
    month = (m + 2)%12 + 1;
    day = d + 1;

    hour = int(24*fmjd)
    minute = int(1440*fmjd - 60*hour)
    second = 86400*fmjd - (3600*hour + 60*minute)

    return year, month, day, hour, minute, second

# Convert string to MJD
def str2mjd(t):
    try:
        m = Time(t, format='isot', scale='utc')
    except:
        try:
            m = Time(float(t), format='mjd')
        except:
            raise 'Cannot parse {}'
    return m.mjd
        
## Function to write a SCHED frequency block
def writefreqentry(freqout, antenna):
    freqout.write("Name = v20cm_1  Station = ASKAP%s    Priority = 1\n" % (antenna[-2:]))
    freqout.write("  rf1 = 500, 500  ifname = A,    C\n")
    freqout.write("  rf2 = 2000, 2000  fe  = '20cm', '20cm'\n")
    freqout.write("  pol =  RCP,  LCP  lo1 =  2100,  2100  syn(2) = 2.1\n/\n\n")

## Function to write a SCHED antenna block
def writestatentry(statout, antenna, count, itrfpos):
    if count < 10:
        countcode = str(count)
    else:
        countcode = chr(ord('A') + count - 10)
    twoletteranname = "A%s" % countcode
    statout.write("  STATION=ASKAP%s   STCODE=A%s  CONTROL=VLBA\n" % (antenna[-2:], countcode))
    statout.write("    DBNAME = %s-ASKAP\n" % antenna[-2:])
    statout.write("        MOUNT=ALTAZ  AX1LIM=-90,450 AX2LIM=2.25,90 AX1RATE=83.6 AX2RATE=29.0\n")
    statout.write("        AX1ACC=0.75  AX2ACC=0.25\n")
    statout.write("        TSETTLE=6 TLEVSET=5 MINSETUP=5  DAR=RDBE2  NBBC=16\n")
    statout.write("        DISK=MARK5C   MEDIADEF=DISK    TSCAL=CONT\n")
    statout.write("        HOR_AZ =   0,  5, 10, 15, 25, 30, 40, 45, 70, 75,120,125,130,135,\n")
    statout.write("                 155,160,185,190,195,220,225,235,240,245,250,255,265,270,\n")
    statout.write("                 275,300,305,310,315,330,335,340,345,350,360\n")
    statout.write("        HOR_EL =   2,  2,  3,  2,  2,  3,  3,  4,  4,  5,  5,  4,  4,  3,\n")
    statout.write("                   3,  2,  2,  3,  4,  4,  3,  3,  4,  4,  5,  6,  6,  5,\n")
    statout.write("                   6,  6,  5,  6,  5,  5,  4,  4,  3,  2,  2\n")
    statout.write("        AXISOFF=  0.0\n")
    statout.write("        X= %s  Y= %s  Z=  %s\n" % (itrfpos[0], itrfpos[1], itrfpos[2]))
    statout.write("        DXDT= 0.0  DYDT=  0.0  DZDT= 0.0  EPOCH=54466\n")
    statout.write("        FRAME='FROM FCM'\n")
    statout.write("      /\n\n")
    return twoletteranname

def writekeyfile(keyout, obs, twoletterannames, craftcatdir, npol):
    startyear, startmonth, startday, starthh, startmm, startss = mjd2ymdhms(float(obs["startmjd"]))
    #antennalist = []
    #for key in obs.keys():
    #    if "ak" in key:
    #        antennalist.append(key)
    #antennastring = ""
    #linecount = 0
    #print "ANTENNA LIST", antennalist
    #for a in antennalist:
    #    if not a in antennanames:
    #        print "Antenna", a, "not in FCM file: skipping"
    #        continue
    #    if len(antennastring) > 1:
    #        antennastring = antennastring + ", "
    #    if linecount == 5:
    #        antennastring += "\n          "
    #        linecount = 0
    #    antennastring = antennastring + "ASKAP" + a[-2:]
    #    linecount += 1
    antennastring = ""
    linecount = 0
    for a in twoletterannames:
        if len(antennastring) > 1:
            antennastring = antennastring + ", "
        if linecount == 5:
            antennastring += "\n          "
            linecount = 0
        antennastring = antennastring + a
        linecount += 1
    if craftcatdir == "":
        craftcatdir = os.getcwd()
    keyout.write("version  = 1\n")
    keyout.write("expt     = 'craft'\n")
    keyout.write("expcode  = 'craftfrb'\n")
    keyout.write("obstype  = 'VLBI'\n\n")
    keyout.write("piname   = 'A.T. Deller'\n")
    keyout.write("address1 = 'Swinburne'\n")
    keyout.write("address2 = ''\n")
    keyout.write("address3 = 'Australia'\n")
    keyout.write("email    = 'adeller@astro.swin.edu.au'\n")
    keyout.write("phone    = '+61-3-9214-5307 (w)'\n")
    keyout.write("obsphone = '+61-3-9214-5307 (w)'\n")
    keyout.write("obsmode  = 'ASKAP 300 channel'\n")
    keyout.write("note1    = 'ASKAP'\n")
    keyout.write("! ================================================================\n")
    keyout.write("!       Correlator section\n")
    keyout.write("! ================================================================\n")
    keyout.write("correl   = 'Socorro'\n")
    keyout.write("coravg   = 2\n")
    keyout.write("corchan  = 16\n")
    keyout.write("cornant  = 3\n")
    keyout.write("corpol   = 'on'\n")
    keyout.write("corwtfn  = 'uniform'\n")
    keyout.write("corsrcs  = 'from .sum file only'\n")
    keyout.write("cortape  = 'ftp'\n")
    keyout.write("cornote1 = 'This is special ASKAP correlation'\n\n")
    keyout.write("! ================================================================\n")
    keyout.write("!       Catalogs (special askap versions)\n")
    keyout.write("! ================================================================\n")
    keyout.write("stafile  = '$CATDIR/askapstation.dat'\n")
    keyout.write("freqfile = '$CATDIR/askapfreq.dat'\n")
    keyout.write("overwrite\n")
    keyout.write("srccat /\n")
    keyout.write("EQUINOX = J2000\n")
    keyout.write("SOURCE='%s' RA=%s DEC=%s REMARKS='Beam centre for dumped voltage data' /\n" % (obs["srcname"], obs["srcra"], obs["srcdec"]))
    keyout.write("endcat /\n\n")
    keyout.write("setinit = askap.set /\n")
    if npol == 1:
        keyout.write(" dbe      = 'rdbe_ddc'\n")
        keyout.write(" format   = 'vdif'          !  Sched doesn't understand CODIF, so lie and say VDIF.\n")
        keyout.write(" nchan    = %d               !  Put in %dx8 MHz as placeholder, overwrite later\n" % (8*npol, 8*npol))
        keyout.write(" bbfilt   = 8.0\n")
    else: # Need to pretend it is PFB in order to get 16 channels
        keyout.write(" dbe      = 'rdbe_pfb'\n")
        keyout.write(" nchan    = %d               !  Put in %dx8 MHz as placeholder, overwrite later\n" % (8*npol, 8*npol))
        keyout.write(" bbfilt   = 32.0\n")
    keyout.write(" netside  = U\n")
    keyout.write(" bits     = 2\n")
    keyout.write(" firstlo  = 2100.0\n")
    keyout.write(" freqref  = 2100.0\n")
    if npol == 1:
        keyout.write(" freqoff  = -608.0, -600.0, -592.0, -584.0, -576.0, -568.0, -560.0, -552.0\n")
        keyout.write(" pol      = L, L, L, L, L, L, L, L\n")
    else:
        keyout.write(" freqoff  = -1008.0, -976.0, -944.0, -912.0, -880.0, -848.0, -816.0, -784.0,\n")
        keyout.write("            -1008.0, -976.0, -944.0, -912.0, -880.0, -848.0, -816.0, -784.0\n")
        keyout.write(" pol      = L, L, L, L, L, L, L, L,\n")
        keyout.write("            R, R, R, R, R, R, R, R\n")
    keyout.write(" pcal     = 'off'\n")
    keyout.write("   /\n")
    keyout.write("endset /\n\n")
    keyout.write("year     = %d\n" % startyear)
    keyout.write("month    = %d\n" % startmonth)
    keyout.write("day      = %d\n" % startday)
    keyout.write("start    = %02d:%02d:%02d\n\n" % (starthh, startmm, int(startss)))
    keyout.write("stations = %s\n" % antennastring)
    keyout.write("setup  = askap.set\n")
    keyout.write("minpause = 5\n\n")
    keyout.write("source = '%s'  dur = %d  gap = 0   /\n\n" % (obs["srcname"], int(0.99 + 86400.0*(float(obs["stopmjd"])-float(obs["startmjd"])))))

def getFPGAdelays(fpga):
    fpga_delays = {}

    if 'CRAFT_FPGA_DELAYS' in os.environ:
        delayFile = os.environ['CRAFT_FPGA_DELAYS']
        if os.path.exists(delayFile):
            with open(delayFile) as f:
                for line in f:
                    line = line.split('#')[0] # Remove comments
                    if line == "" or line.isspace(): continue
                    keys = line.split()
                    if len(keys)!=3:
                        sys.stderr.write("Cannot parse : "+line)
                        continue
                    ant = keys[0]
                    thisFPGA = keys[1]
                    delay = keys[2]
                    if thisFPGA != fpga: continue
                    fpga_delays[ant] = delay
        else:
            sys.stderr.write("Error: Delay file \"{}\" does not exist.".format(delayFile))

    return fpga_delays

def writev2dfile(v2dout, obs, twoletterannames, antennanames, delays, datafilelist, fpga, nchan, forceFFT, tInt, polyco, npol, profilemode, startseries):
    if fpga is not None:
        fpga_delay = getFPGAdelays(fpga)
    else:
        fpga_delay = {}

    v2dout.write('''\
#  Template v2d file for DiFX correlation of craftfrb

vex = craftfrb.vex

startSeries = {0:d}
minLength = 1
allowAllClockOffsets = True
tweakIntTime = True
exhaustiveAutocorrs = True

'''.format(startseries))
    if profilemode:
        v2dout.write("mode = profile\n")
    v2dout.write("visBufferLength = 8\n")
    v2dout.write("antennas = ")
    for d in datafilelist[0]:
        a = d.split('=')[0]
        v2dout.write(a)
        if not a == twoletterannames[-1]:
            v2dout.write(", ")
    v2dout.write("\n")
    for ant, d0, d1, delay in zip(antennanames, datafilelist[0], datafilelist[-1], delays):
        a = d0.split('=')[0]
        d = [d0]
        if npol > 1:
            d.append(d1)
        v2dout.write("ANTENNA %s\n{\n" % a)
        v2dout.write("  name = %s\n" % ant)
        v2dout.write("  clockOffset=%.6f\n" % (-float(delay[:-2])/1000.0))
        if ant in fpga_delay:
            clkDelays = [str(int(fpga_delay[ant])*6.75)]*4*2*npol
            v2dout.write("  freqClockOffs="+','.join(clkDelays)+"\n")
        v2dout.write("  clockRate=0\n")
        v2dout.write("  clockEpoch=57000.0\n")
        v2dout.write("  phaseCalInt=0\n")
        v2dout.write("  toneSelection=none\n")
        v2dout.write("  format=CODIFC/27/{}/{}\n".format(framesize, bits))
        v2dout.write("  sampling=COMPLEX_DSB\n")
        if npol > 1:
            v2dout.write("  datastreams=%s-P0,%s-P1\n}\n\n" % (a, a))
            for i in range(npol):
                v2dout.write("DATASTREAM %s-P%d\n{\n" % (a, i))
                v2dout.write("  file = %s\n" % d[i].split('=')[1])
                v2dout.write("  format=CODIFC/27/{}/{}\n".format(framesize, bits))
                v2dout.write("  sampling=COMPLEX_DSB\n}\n")
        else:
            v2dout.write("  file = %s\n}\n" % d[0].split('=')[1])

    if forceFFT or nchan>=128:
        nFFTChan = nchan
    else:
        if 128 % nchan == 0:
            nFFTChan = 128
        else:
            nFFTChan = ((128 // nchan) +1) * nchan
            
    v2dout.write("\n# The nChan should never be less than 128.\n")
    v2dout.write("# For numbers of channels < 128, set specAvg so nChan/specAvg\n")
    v2dout.write("# gives the desired number of channels\n")
    v2dout.write("SETUP default\n")
    v2dout.write("{\n")
    fftnSec = 27.0/32.0*1000*nFFTChan
    intNsec = float(tInt)*1e9
    #numFFT = int(round(intNsec/fftnSec))
    if intNsec>5e8:  # 0.5sec
        subintNsec = 13824000 # 13.8 millisec
    else:
        if intNsec<20000000: # 20 millisec
            numFFT = int(round(intNsec/fftnSec))
            intNsec = int(round(numFFT * fftnSec))
            subintNsec = intNsec
            tInt = float(intNsec)/1.0e9
        else:
            subintNsec = 14e6 # 14 millisec nominal
            nSubint = intNsec / float(subintNsec)
            print("Got {:.3f} subint per integration".format(nSubint))
            if abs(round(nSubint)*subintNsec-intNsec)/intNsec < 0.05:  # Within 5%, tweak int time
                print("DEBUG: Tweaking integration' time")
            else: # Otherwise tweak subInt
                print("DEBUG: Tweaking subint' time")
                nSubint = round(nSubint)
                subintNsec = intNsec / float(nSubint)
    numFFT = int(round(subintNsec/fftnSec))
    subintNsec = int(round(numFFT * fftnSec))
    nSubint = int(round(intNsec / float(subintNsec)))
    tInt = float(nSubint * subintNsec)/1.0e9
    v2dout.write("  tInt =  {:.9f}\n".format(tInt))
    v2dout.write("  subintNS = {}\n".format(subintNsec))
            
    v2dout.write("  nFFTChan =    {}\n".format(nFFTChan))
    v2dout.write("  nChan =  {}\n".format(nchan))
    v2dout.write("  doPolar = True # Full stokes\n")
    if polyco is not None:
        v2dout.write("  binConfig = {} # Pulsar Setup\n".format(args.polyco))
    v2dout.write("}\n")
    v2dout.write("\n")
    v2dout.write("# This, along with SETUP default above, should always be done\n")
    v2dout.write("RULE default\n")
    v2dout.write("{\n")
    v2dout.write("  setup = default\n")
    v2dout.write("}\n")
    v2dout.write("\n")
    v2dout.write("#  SETUP place holders (commented)\n")
    v2dout.write("# SETUP askap.set {}\n")
    v2dout.write("\n")
    v2dout.write("# Sources (pointing centers) with recorded data but no offset pointing centers:\n")
    v2dout.write("SOURCE %s { }\n\n" % obs["srcname"])

    
## Argument parser
parser = argparse.ArgumentParser()
parser.add_argument("fcm",  help="ASKAP .fcm file describing array")
parser.add_argument("obs",  help="Flat text .obs file containing start/stop time, source position, baseband files")
parser.add_argument("chan", help="Flat text file containing 1 line per subband, centre freq, sideband, and bandwidth")
parser.add_argument("-a", "--ants", help="Comma separated list of antenna names e.g., ak01,ak02,ak03 etc.  All must be present in .fcm, and all must have a akxx.codif file present in this directory.")
parser.add_argument("-b", "--bits", help="Number of bits/sample. Default 1", type=int)
parser.add_argument("-t", "--threads", help="Number of DIFX threads. Default 8", type=int)
parser.add_argument("-F", "--framesize", help="Codif framesize. Default 8064", type=int)
parser.add_argument("-f", "--fpga", help="FPGA and card for delay correction. E.g. c4_f0")
parser.add_argument("-p", "--polyco", help="Bin config file for pulsar gating")
parser.add_argument("-i", "--integration", default = "1.3824", help="Correlation integration time")
parser.add_argument("-n", "--nchan", type=int, default=128, help="Number of spectral channels")
parser.add_argument("-s", "--slurm", default=False, action="store_true", help="Use slurm batch jobs rather than running locally")
parser.add_argument("--forceFFT", default=False, action="store_true", help="Force FFT size to equal number of channels (don't increase to 128)")
parser.add_argument("--npol", help="Number of polarisations", type=int, choices=[1,2], default=1)
parser.add_argument("--gstar", default=False, action="store_true", help="Set if using gstar for correlation")
parser.add_argument("--large", default=False, action="store_true", help="Set if 32 nodes, 384 tasks are required (i.e., 23GB memory needed per task; else 16 nodes, 192 tasks will be used for 11.5GB per task")
parser.add_argument("--numskylakenodes", default=1, type=int, help="Use 32x this many CPUs")
parser.add_argument("--profile", default=False, action="store_true", help="Run DiFX in profile mode (looking at autocorrelations")
args = parser.parse_args()

## Check arguments
if not os.path.exists(args.fcm):
    parser.error("FCM file " + args.fcm + " does not exist")
if not os.path.exists(args.obs):
    parser.error("obs file " + args.obs + " does not exist")
if not os.path.exists(args.chan):
    parser.error("chan file " + args.chan + " does not exist")
if args.ants == "":
    parser.error("you must supply a list of antennas")
if args.polyco is not None and not os.path.exists(args.polyco):
    parser.error("binconfig file " + args.polyco + " does not exist")
if args.large and not args.gstar:
    parser.error("You can't run large if runnning on skylake (the default, i.e. you didn't use --gstar")
if args.gstar and args.numskylakenodes > 1:
    parser.error("You can't set the number of skylake nodes if you are running on gstar")
bits = args.bits
if bits==None: bits=1
difxThreads = args.threads
if args.slurm:
    difxThreads = 2
if difxThreads is None:
    if 'CRAFT_DIFXTHREADS' in os.environ:
        difxThreads=os.environ['CRAFT_DIFXTHREADS']
    else:
        difxThreads=8
framesize = args.framesize
if framesize==None: framesize=8064

## Load configuration data
fcm = load_props(args.fcm)
obs = load_props(args.obs)

# Convert time to MJD
if 'startmjd' in obs: obs['startmjd'] = str2mjd(obs['startmjd'])
if 'stopmjd'  in obs: obs['stopmjd']  = str2mjd(obs['stopmjd'])

targetants = args.ants.split(',')

# Look and see if there is a catalog directory defined
try:
    craftcatalogbasedir = os.environ['CRAFTCATDIR'] + '/'
    if not os.path.exists(craftcatalogbasedir):
        print(craftcatalogbasedir, "specified by $CRAFTCATDIR doesn't exist")
        sys.exit()
    craftcatalogdir = craftcatalogbasedir + str(os.getpid()) + '/'
    if not os.path.exists(craftcatalogdir):
        os.mkdir(craftcatalogdir)
except KeyError:
    craftcatalogdir = os.getcwd() + "/"
    if len(craftcatalogdir) >= 62:
        print("Catalog directory name is too long! I'm sorry, but I have to abort, because other sched will.")
#        sys.exit()

## Write the SCHED freq and antenna files, and the craftfrb.datafiles
freqout = open(craftcatalogdir + "askapfreq.dat","w")
statout = open(craftcatalogdir + "askapstation.dat","w")
dataout = []
for i in range(args.npol):
    dataout.append(open("craftfrb.p%d.datafiles" % i,"w"))
count = 0
antennanames = []
twoletterannames = []
delays = []
datafilelist = []
for i in range(args.npol):
    datafilelist.append([])
cwd = os.getcwd()

def antnum(a):
    if a=='ant': return 0
    return int(a[3:])

for antenna in sorted(list(fcm["common"]["antenna"].keys()), key=antnum):
    if antenna=='ant': continue
    if not "name" in list(fcm["common"]["antenna"][antenna].keys()):
        continue # This one is probably a test antenna or something, ignore it
    if not "location" in list(fcm["common"]["antenna"][antenna].keys()):
        continue # This one is probably a test antenna or something, ignore it
    antennaname = fcm["common"]["antenna"][antenna]["name"]
    if not antennaname in targetants:
        print("Skipping antenna", antennaname, "from fcm file as it wasn't requested.")
        continue
    writefreqentry(freqout, antennaname)
    #print fcm["common"]["antenna"][antenna]["location"].keys()
    #print fcm["common"]["antenna"][antenna]["location"]["itrf"]
    #if not "itrf" in fcm["common"]["antenna"][antenna]["location"].keys():
    #    wgs84 = fcm["common"]["antenna"][antenna]["location"]["wgs84"]
        
    twolettername = writestatentry(statout, antennaname, count, fcm["common"]["antenna"][antenna]["location"]["itrf"])
    if "delay" in list(fcm["common"]["antenna"][antenna].keys()):
        delay = fcm["common"]["antenna"][antenna]["delay"]
    else:
        delay = "0.0ns"
    for i in range(args.npol):
        print("Looking for %s/%s.p%d.codif" % (cwd, antennaname, i))
        if os.path.exists("%s/%s.p%d.codif" % (cwd, antennaname, i)):
            datafilelist[i].append("%s=%s/%s.p%d.codif" % (twolettername, cwd, antennaname, i))
            dataout[i].write(datafilelist[i][-1] + "\n")
        else:
            #print "Skipping", antennaname, "as we don't have a data file for it"
            print("Couldn't find a codif file for", antennaname, "pol", i, "- aborting!")
            sys.exit()
    antennanames.append(antennaname)
    twoletterannames.append(twolettername)
    delays.append(delay)
    count += 1
freqout.close()
statout.close()
for i in range(args.npol):
    dataout[i].close()

## Check how many datafiles we found
#if len(datafilelist) == 0:
#    print "Didn't find any matching .codif files! Aborting"
#    sys.exit()
#elif len(datafilelist) < len(antennanames):
#    print "WARNING: Expected %d matching codif files, but only found %d" % (len(antennanames), len(datafilelist))

## Write sched running file
runsched = open("runsched.sh", "w")
runsched.write("#!/usr/bin/bash\n")
runsched.write("export CATDIR={0}\n".format(craftcatalogdir))
runsched.write("sched < craftfrb.key\n")
runsched.close()
os.system("chmod 775 runsched.sh")

## Write the key file
keyout = open("craftfrb.key","w")
writekeyfile(keyout, obs, twoletterannames, craftcatalogdir, args.npol)
keyout.close()

## Run it through sched
ret = os.system("./runsched.sh")
if ret!=0:
    print("Warning, Sched failed")
    sys.exit(1)

## Replace Mark5B with VDIF in vex file
ret = os.system("sed -i 's/MARK5B/VDIF/g' craftfrb.vex")
if ret!=0: exit(1)

## Run getEOP and save the results
if not os.path.exists("eop.txt"):
    ret = os.system("getEOP.py -l " + str(int(float(obs["startmjd"]))) + " > eop.txt")
    if (ret!=0): sys.exit(ret)
else:
    print("Using existing EOP data - remove eop.txt if you want to get new data!")
eoplines = open("eop.txt").readlines()

## Write the v2d file
v2dout = open("craftfrb.v2d", "w")
startseries = 0
basename = "craftfrb"
if args.slurm:
    startseries = os.getpid()
    basename = "craftfrb_{0:d}".format(startseries)
writev2dfile(v2dout, obs, twoletterannames, antennanames, delays, datafilelist, args.fpga,
             args.nchan, args.forceFFT, args.integration, args.polyco, args.npol, args.profile, startseries)
for line in eoplines:
   if "xPole" in line or "downloaded" in line:
       v2dout.write(line)
v2dout.close()

## Run updateFreqs
runline = "updatefreqs.py craftfrb.vex --npol={} {}".format(args.npol, args.chan)
if args.nchan is not None: runline += " --nchan={}".format(args.nchan)
print("Running: "+runline)
ret = os.system(runline)
if ret!=0: sys.exit(1)

## Update the vex file to say "CODIF" rather than "VDIF"
ret = os.system("sed -i -e 's/VDIF5032/CODIFD{}/g' craftfrb.vex".format(framesize))
if ret!=0: sys.exit(1)

## Run vex2difx
ret = os.system("vex2difx craftfrb.v2d > vex2difxlog.txt")
if ret!=0:
    print("vex2difx failed!")
    sys.exit(1)

## Run difxcalc to generate the interferometer model
ret = os.system("\\rm -f {0}.im".format(basename))
if ret!=0: sys.exit(1)
ret = os.system("difxcalc {0}.calc".format(basename))
if ret!=0: sys.exit(1)

## Create the machines and threads file and a corresponding run script, or the slurm batch job, as appropriate
if args.slurm:
    currentdir = os.getcwd()
    currentuser = getpass.getuser()
    numprocesses = args.npol*len(datafilelist[0]) + 5
    print("Approx number of processes", numprocesses)
    numprocessingnodes = numprocesses - (args.npol*len(datafilelist[0]) + 1)

    # Create a launchjob script that will handle the logging, etc
    launchout = open("launchjob","w")
    launchout.write("#!/usr/bin/bash\n\n")
    launchout.write("sbatch -W runparallel\n")
    launchout.close()
    os.chmod("launchjob", 0o775)
    print("./launchjob")

    # Create a run script that will actually be executed by sbatch
    if args.gstar:
        # NOTE: use either 192 or 384 for 16 or 32 node request
        if args.large:
            numnodes = 32
            ntasks = 384
        else:
            numnodes = 16
            ntasks = 192

        numprocesses = int(2**(math.floor(math.log(numprocesses, 2))+1))
        print("Rounded to next highest number of 2:", numprocesses)
        
        batchout = open("runparallel","w")
        batchout.write("#!/bin/bash\n")
        batchout.write("#\n")
        batchout.write("#SBATCH --job-name=difx_{0}\n".format(basename))
        batchout.write("#SBATCH --output={0}.mpilog\n".format(basename))
        batchout.write("#\n")
        batchout.write("#SBATCH --nodes={0}\n".format(numnodes))
        batchout.write("#SBATCH --ntasks={0}\n".format(ntasks))
        batchout.write("#SBATCH --time=10:00\n")
        # NOTE: use 46g for 11.5 or 23 GB of ram per node (16 or 32)
        batchout.write("#SBATCH --mem=46g\n\n")
        batchout.write(". /home/{0}/setup_difx.gstar\n\n".format(currentuser))
        batchout.write("export DIFX_MESSAGE_GROUP=`hostname -i`\n")
        batchout.write("export DIFX_BINARY_GROUP=`hostname -i`\n")
        batchout.write("date\n\n")
        batchout.write("difxlog {0} {1}/{0}.difxlog 4 &\n\n".format(basename,currentdir))
        batchout.write("srun -N{0} -n{1:d} -c2 mpifxcorr {2}.input --nocommandthread\n".format(numnodes,numprocesses,basename))
        batchout.write("./runmergedifx\n")
        batchout.close()
    else:
        batchout = open("runparallel","w")
        batchout.write("#!/bin/bash\n")
        batchout.write("#\n")
        batchout.write("#SBATCH --job-name=difx_{0}\n".format(basename))
        batchout.write("#SBATCH --output={0}.mpilog\n".format(basename))
        batchout.write("#\n")
        # Request 32xnumskylakenodes CPUs, so it will fit on numskylakenodes
        batchout.write("#SBATCH --ntasks={0:d}\n".format(32*args.numskylakenodes))
        batchout.write("#SBATCH --time=8:00\n") 
        batchout.write("#SBATCH --cpus-per-task=1\n")
        batchout.write("#SBATCH --mem-per-cpu=4000\n\n")
        batchout.write(". /home/{0}/setup_difx\n\n".format(currentuser))
        batchout.write("export DIFX_MESSAGE_GROUP=`hostname -i`\n")
        batchout.write("export DIFX_BINARY_GROUP=`hostname -i`\n")
        batchout.write("date\n\n")
        batchout.write("difxlog {0} {1}/{0}.difxlog 4 &\n\n".format(basename,currentdir))
        batchout.write("srun -n{0} --overcommit mpifxcorr {1}.input --nocommandthread\n".format(numprocesses,basename))
        batchout.write("./runmergedifx\n")
        batchout.close()

    # Write the threads file
    threadsout = open("{0}.threads".format(basename),"w")
    threadsout.write("NUMBER OF CORES:    {0}\n".format(numprocessingnodes))
    for i in range(numprocessingnodes):
        threadsout.write("{}\n".format(difxThreads))
    threadsout.close()
else:
    # We just want Ndatastream processes, plus a head node, plus one computational node
    numprocesses = args.npol*len(datafilelist[0]) + 2

    # Write the machines file (all running on localhost)
    machinesout = open("machines","w")
    for i in range(numprocesses):
        machinesout.write("localhost\n")
    machinesout.close()

    # Write the threads file
    threadsout = open("craftfrb.threads","w")
    threadsout.write("NUMBER OF CORES:    1\n")
    threadsout.write("{}\n".format(difxThreads))
    threadsout.close()
    
    ## Create a little run file for running the observations
    ## This is used in preference to startdifx because startdifx uses some MPI options we don't want
    runout = open("run.sh", "w")
    runout.write("#!/bin/sh\n\n")
    runout.write("rm -rf craftfrb.difx\n")
    runout.write("rm -rf log*\n")
    runout.write("errormon2 6 &\n")
    runout.write("export ERRORMONPID=$!\n")
    runout.write("mpirun -machinefile machines -np %d mpifxcorr craftfrb.input\n" % (numprocesses))
    runout.write("kill $ERRORMONPID\n")
    runout.write("rm -f craftfrb.difxlog\n")
    runout.write("mv log craftfrb.difxlog\n")
    runout.close()
    os.chmod("run.sh", 0o775)
    
    print("# First run the correlation:")
    runline = "./run.sh\n"
    print(runline)

## Print the line needed to run the stitching and then subsequently difx2fits
print("Then run difx2fits")
runline = "rm -rf {0}D2D.* ; mergeOversampledDiFX.py craftfrb.stitchconfig {0}.difx\n".format(basename)
print(runline)
with open('runmergedifx', 'w') as runmerge:
    runmerge.write(runline)
os.chmod('runmergedifx',0o775)
runline = "difx2fits {0}D2D".format(basename)
print(runline)

## Produce the little script needed to run difx2fits for the solveFPGA script
runline = "difx2fits {0}D2D\n".format(basename)
with open('runfpgadifx2fits', 'w') as rund2f:
    rund2f.write(runline)
os.chmod('runfpgadifx2fits',0o775)
