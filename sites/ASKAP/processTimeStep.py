#!/usr/bin/python
import os,sys,glob,argparse

parser = argparse.ArgumentParser()
parser.add_argument("-t", "--timestep", help="Timestep (the directory name to process")
parser.add_argument("-r", "--ra", help="Force RA value")
parser.add_argument("-d", "--dec", help="Force Dec value: use no space if declination is negative, i.e., -d-63:20:23.3")
parser.add_argument("-b", "--bits", type=int, default=1,help="Number of bits")
parser.add_argument("-f", "--fcm", default="fcm.txt", help="Name of the fcm file")
parser.add_argument("-c", "--correctfpgadelays", default=False, action="store_true", help="Figure out and correct 7 microsec FPGA delays")
parser.add_argument("--beam", default="", help="Correlate a specific beam: blank means the first one (numerically)")
args = parser.parse_args()

if args.timestep == "":
    parser.error("You must specify a timestep / target directory")

timestep = args.timestep

if not os.path.exists(timestep):
    parser.error("Target directory (timestep)", timestep, "doesn't exist")

if not os.path.exists(args.fcm):
    parser.error(fcm, "doesn't exist")

fcm = os.path.abspath(args.fcm)
examplefiles = []
antennadirs = sorted(glob.glob(timestep + "/ak*"))
for a in antennadirs:
    if args.beam == "":
        beamdirs = sorted(glob.glob(a + "/*"))
    else:
        beamdirs = [a + "/" + args.beam]
        if not os.path.exists(a + "/" + args.beam):
            print a + "/" + args.beam + " doesn't exist, aborting"
            sys.exit()
    for b in beamdirs:
        vcraftfiles = glob.glob(b + "/*vcraft")
        if len(vcraftfiles) > 0:
            examplefiles = sorted(vcraftfiles)
            beamname = b.split('/')[-1]
            break

if len(examplefiles) == 0:
    print "Couldn't find any vcraft files"
    sys.exit()

os.chdir(timestep)
os.mkdir(beamname)
os.chdir(beamname)

difx2fitscommand = "difx2fits -u"
for e in examplefiles:
    freqlabel = e.split('/')[-1][5:10]
    print "Going to process", freqlabel
    difx2fitscommand = difx2fitscommand + " " + freqlabel + "/craftfrbD2D.input"
    os.mkdir(freqlabel)
    os.chdir(freqlabel)
    os.system("cp %s fcm.txt" % fcm)
    if os.path.exists("../../../eopjunk.txt"):
        os.system("cp ../../../eopjunk.txt .")

    torun = "vcraft2obs.py"
    if not args.ra == "":
        torun = torun + " -r" + args.ra
    if not args.dec == "":
        torun = torun + " -d" + args.dec
    if not args.bits == "":
        torun = torun + " --bits=" + str(args.bits)
    torun += ' "../../ak*/%s/*%s*vcraft"' % (beamname, freqlabel)

    print torun
    os.system(torun + "> vcraft2obs.log")
    os.system("tail -n 1 vcraft2obs.log > runaskap2difx")
    os.system("chmod 775 runaskap2difx")
    os.system("./runaskap2difx > askap2difx.log")
    os.system("tail -n 2 askap2difx.log | head -n 1 > runmergedifx")
    os.system("chmod 775 runmergedifx")
    os.system("./run.sh")
    os.system("./runmergedifx")
    if args.correctfpgadelays:
        os.system("findOffsets.py")
        os.system("./run.sh")
        os.system("rm -rf craftfrbD2D*")
        os.system("./runmergedifx")
    os.chdir("../")
    
output = open("rundifx2fits","w")
output.write(difx2fitscommand + "\n")
output.close()
os.system("chmod 775 rundifx2fits")
os.system(difx2fitscommand)
