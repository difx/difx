#!/usr/bin/env python
import os,sys,glob,argparse,re,subprocess

parser = argparse.ArgumentParser()
parser.add_argument("-t", "--timestep", help="Timestep (the directory name to process")
parser.add_argument("-r", "--ra", help="Force RA value")
parser.add_argument("-d", "--dec", help="Force Dec value: use no space if declination is negative, i.e., -d-63:20:23.3")
parser.add_argument("-b", "--bits", type=int, default=1,help="Number of bits. Default 1")
parser.add_argument("-i", "--integration", type=float, help="Correlation integration time")
parser.add_argument("-n", "--nchan", type=int, help="Number of spectral channels")
parser.add_argument("--forceFFT", default=False, action="store_true", help="Force FFT size to equal number of channels (don't increase to 128)")
parser.add_argument("-f", "--fcm", default="fcm.txt", help="Name of the fcm file")
parser.add_argument("-p", "--polyco", help="Bin config file for pulsar gating")
parser.add_argument("-c", "--correctfpgadelays", default=False, action="store_true", help="Figure out and correct 7 microsec FPGA delays")
parser.add_argument("-S", "--suppress", default=False, action="store_true", help="Don't create FITS file")
parser.add_argument("-B", "--beam", help="Correlate a specific beam: blank means both")
parser.add_argument("--card", default="", help="Correlate only a specific card; blank means all")
parser.add_argument("-k", "--keep", default=False, action="store_true", help="Keep existing codif files")
parser.add_argument("-s", "--snoopylog", help="Snoopy log file, default blank, if not default will use this to correlate on-pulse")
parser.add_argument("--slurm", default=False, action="store_true", help="Use slurm batch jobs rather than running locally")
parser.add_argument("--ts", default=0, type=int, help="Use taskspooler to run CRAFTConverter, with N parallel tasks")
parser.add_argument("--gstar", default=False, action="store_true", help="Set if using gstar for correlation")
parser.add_argument("--skylake", default=False, action="store_true", help="Set if using skylake nodes for correlation")
parser.add_argument("--large", default=False, action="store_true", help="Set if 32 nodes, 384 tasks are required (i.e., 23GB memory needed per task; else 16 nodes, 192 tasks will be used for 11.5GB per task")
args = parser.parse_args()

if args.timestep is None:
    parser.error("You must specify a timestep / target directory")

timestep = args.timestep

if not os.path.exists(timestep):
    parser.error("Target directory (timestep) " + timestep + " doesn't exist")

if not args.snoopylog is None and not os.path.exists(args.snoopylog):
    parser.error("Snoopy log file " + args.snoopylog + " doesn't exist")

timestep = os.path.abspath(timestep)

if not os.path.exists(args.fcm):
    parser.error(args.fcm + " doesn't exist")

polyco = args.polyco
if polyco is not None:
    if not os.path.exists(polyco):
        parser.error("binconfig file " + polyco + " does not exist")
    else:
        polyco = os.path.abspath(polyco)
    
fcm = os.path.abspath(args.fcm)

topDir = os.getcwd()

examplefiles = []
antennadirs = sorted(glob.glob(timestep + "/ak*"))

for a in antennadirs:
    if args.beam is None:
        beamdirs = sorted(glob.glob(a + "/*"))
    else:
        beamdirs = [a + "/" + args.beam]
        if not os.path.exists(a + "/" + args.beam):
            print a + "/" + args.beam + " doesn't exist, aborting"
            sys.exit()
    for b in beamdirs:
        vcraftfiles = glob.glob(b + "/*[ac]" + args.card + "*vcraft")
        
        if len(vcraftfiles) > 0:
            examplefiles = sorted(vcraftfiles)
            break
    if len(examplefiles)>0: break
        
if len(examplefiles) == 0:
    print "Couldn't find any vcraft files"
    sys.exit()
    
npol=len(beamdirs)
if npol==0:
    print "Could not find any beams. Aborting"
    sys.exit()

if npol>2:
    print "Too many beams found! Aborting"
    sys.exit()


if npol==1:
    datadir = os.path.basename(beamdirs[0])
else:
    datadir = 'data'


def runCommand(command, log):
    proc = subprocess.Popen(command, shell=True,
                            stdout=subprocess.PIPE, 
                            stderr=subprocess.PIPE)

    with open(log, "w") as log_file:
        outs, errs = proc.communicate()
        print outs
        print errs
        log_file.write(outs)
        log_file.write(errs)
    return proc.returncode

if not os.path.exists(datadir): os.mkdir(datadir)
os.chdir(datadir)

difx2fitscommand = "difx2fits -u"
freqlabels = []
for e in examplefiles:
    freqlabel = e.split('/')[-1][5:10]
    freqlabels.append(freqlabel)
    print "Going to process", freqlabel
    #difx2fitscommand = difx2fitscommand + " " + freqlabel + "/craftfrbD2D.input"
    if not os.path.exists(freqlabel): os.mkdir(freqlabel)
    os.chdir(freqlabel)

    os.system("cp %s fcm.txt" % fcm)
    if os.path.exists("../../eopjunk.txt"):
        os.system("cp ../../eopjunk.txt .")

    # Copy .bat0 file if it exists
    if os.path.exists("../../.bat0"):
        os.system("cp ../../.bat0 .")
                
    torun = "vcraft2obs.py"
    if args.keep:
        torun += " -k"
    if args.ra is not None:
        torun = torun + " -r" + args.ra
    if args.dec is not None:
        torun = torun + " -d" + args.dec
    if not args.bits == "":
        torun = torun + " --bits=" + str(args.bits)
    if polyco is not None:
        torun += " --polyco "+polyco
    if args.integration is not None:
        torun += " --integration={}".format(args.integration)
    if args.ts is not None:
        torun += " --ts={}".format(args.ts)
    if args.nchan is not None:
        torun += " --nchan={}".format(args.nchan)
    if args.forceFFT:
        torun += " --forceFFT"
    if args.gstar:
        torun += " --gstar"
    if args.skylake:
        torun += " --skylake"
    if args.large:
        torun += " --large"
    if args.slurm:
        torun += " --slurm"
        homedir = os.path.expanduser('~') + "/"
        if os.path.exists(homedir + ".eops.new") and os.path.getsize(homedir + ".eops.new") > 0:
            os.system("mv -f " + homedir + ".eops.new " + homedir + ".eops")

    beamname = os.path.basename(beamdirs[0])
    torun += ' --fpga %s "%s/ak*/%s/*%s*vcraft"' % (freqlabel, timestep, beamname, freqlabel)
    if npol==2:
        beamname = os.path.basename(beamdirs[1])
        torun += ' "%s/ak*/%s/*%s*vcraft"' % (timestep, beamname, freqlabel)
    
    print torun
    ret = runCommand(torun, "vcraft2obs.log")
    if ret!=0:
        print "vcraft2obs failed! (", ret, ")"
        sys.exit(ret)
    
    if not os.path.exists("eop.txt"):
        topEOP = "{}/eop.txt".format(topDir)
        if not os.path.exists(topEOP):
            mjd = None
            with open('obs.txt','r') as f:
                for line in f:
                    match = re.search("startmjd\s*=\s*(\S+)", line)
                    if (match):
                        mjd =  match.group(1)
                        break

            if mjd is not None:
                ret = os.system("getEOP.py -l {} > {}".format(mjd, topEOP))
                if (ret!=0): sys.exit(ret)    
            else:
                print "Could not find MJD in obs.txt"
                sys.exit()
                
        print "Copying EOP from top dir"
        os.system("cp {} eop.txt".format(topEOP))

        
    #ret = os.system("./runaskap2difx | tee askap2difx.log")
    ret = runCommand("./runaskap2difx", "askap2difx.log")
    if ret!=0:
        print "askap2difx failed! (", ret, ")"
        sys.exit(ret)

    # if .bat0 does not exisit in upper directory, copy back
    if not os.path.exists("../../.bat0") and os.path.exists(".bat0"):
        os.system("cp .bat0 ../..")
        
    if args.slurm:
        os.system("./launchjob")
    else:
        os.system("./run.sh")
    os.system("./runmergedifx")
    if args.correctfpgadelays:
        os.system("findOffsets.py")
        os.system("./run.sh")
        os.system("rm -rf craftfrbD2D*")
        os.system("./runmergedifx")
#    if args.suppress:
#        os.system("difx2fits craftfrbD2D")
    os.chdir("../")
    
for freqlabel in freqlabels:
    d2dinput = glob.glob(freqlabel + "/*D2D.input")
    if len(d2dinput) > 1:
        print "Too many D2D inputs in", freqlabel, "aborting rundfix2fits!"
        sys.exit()
    difx2fitscommand = difx2fitscommand + " " + d2dinput[0]
output = open("rundifx2fits","w")
output.write(difx2fitscommand + " \"$@\"\n")
output.close()
os.system("chmod 775 rundifx2fits")
if not args.suppress: os.system(difx2fitscommand)
