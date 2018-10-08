#!/usr/bin/python
import os,sys,glob,argparse

parser = argparse.ArgumentParser()
parser.add_argument("-t", "--timestep", help="Timestep (the directory name to process")
parser.add_argument("-r", "--ra", help="Force RA value")
parser.add_argument("-d", "--dec", help="Force Dec value: use no space if declination is negative, i.e., -d-63:20:23.3")
parser.add_argument("-b", "--bits", type=int, default=1,help="Number of bits. Default 1")
parser.add_argument("-f", "--fcm", default="fcm.txt", help="Name of the fcm file")
parser.add_argument("-c", "--correctfpgadelays", default=False, action="store_true", help="Figure out and correct 7 microsec FPGA delays")
parser.add_argument("-s", "--separate", default=False, action="store_true", help="Don't attempt to combine data from each FPGA")
parser.add_argument("-B", "--beam", default="", help="Correlate a specific beam: blank means the first one (numerically)")
parser.add_argument("--card", default="", help="Correate only a specific card; blank means all")
parser.add_argument("-k", "--keep", default=False, action="store_true", help="Keep exisiting codif files")
args = parser.parse_args()

if args.timestep is None:
    parser.error("You must specify a timestep / target directory")

timestep = args.timestep

if not os.path.exists(timestep):
    parser.error("Target directory (timestep) " + timestep + " doesn't exist")

if not os.path.exists(args.fcm):
    parser.error(fcm + " doesn't exist")

fcm = os.path.abspath(args.fcm)
examplefiles = []
antennadirs = sorted(glob.glob(timestep + "/ak*"))

first = True
for a in antennadirs:
    if args.beam == "":
        beamdirs = sorted(glob.glob(a + "/*"))
    else:
        beamdirs = [a + "/" + args.beam]
        if not os.path.exists(a + "/" + args.beam):
            print a + "/" + args.beam + " doesn't exist, aborting"
            sys.exit()
    for b in beamdirs:
        vcraftfiles = glob.glob(b + "/*c" + args.card + "*vcraft")

        if len(vcraftfiles) > 0:
            examplefiles = sorted(vcraftfiles)
            beamname = os.path.basename(b)
            if first: print "Processing", beamname
            break
    first=False
    
if len(examplefiles) == 0:
    print "Couldn't find any vcraft files"
    sys.exit()

if not os.path.exists(beamname): os.mkdir(beamname)
os.chdir(beamname)

difx2fitscommand = "difx2fits -u"
for e in examplefiles:
    freqlabel = e.split('/')[-1][5:10]
    print "Going to process", freqlabel
    difx2fitscommand = difx2fitscommand + " " + freqlabel + "/craftfrbD2D.input"
    if not os.path.exists(freqlabel): os.mkdir(freqlabel)
    os.chdir(freqlabel)

    os.system("cp %s fcm.txt" % fcm)
    if os.path.exists("../../eopjunk.txt"):
        os.system("cp ../../eopjunk.txt .")

    torun = "vcraft2obs.py"
    if args.keep:
        torun += " -k"
    if args.ra is not None:
        torun = torun + " -r" + args.ra
    if args.dec is not None:
        torun = torun + " -d" + args.dec
    if not args.bits == "":
        torun = torun + " --bits=" + str(args.bits)
    torun += ' --fpga %s "%s/ak*/%s/*%s*vcraft"' % (freqlabel, timestep, beamname, freqlabel)

    print torun
    
    os.system(torun + "| tee vcraft2obs.log")
    os.system("./runaskap2difx | tee askap2difx.log")
    os.system("./run.sh")
    os.system("./runmergedifx")
    if args.correctfpgadelays:
        os.system("findOffsets.py")
        os.system("./run.sh")
        os.system("rm -rf craftfrbD2D*")
        os.system("./runmergedifx")
    if args.separate:
        os.system("difx2fits craftfrbD2D")
    os.chdir("../")
    
output = open("rundifx2fits","w")
output.write(difx2fitscommand + "\n")
output.close()
os.system("chmod 775 rundifx2fits")
if not args.separate: os.system(difx2fitscommand)
