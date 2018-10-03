#!/usr/bin/env python
import os, sys, glob, math, argparse

# Convenience function
def posradians2string(rarad, decrad):
    rah = rarad * 12 / math.pi
    rhh = int(rah)
    rmm = int(60*(rah - rhh))
    rss = 3600*rah - (3600*rhh + 60*rmm)
    decd = decrad * 180 / math.pi
    decformat = "+%02d:%02d:%010.7f"
    if decd < 0:
        decd = -decd
        decformat = '-' + decformat[1:]
    ddd = int(decd)
    dmm = int(60*(decd - ddd))
    dss = 3600*decd - (3600*ddd + 60*dmm)
    rastring  = "%02d:%02d:%011.8f" % (rhh,rmm,rss)
    decstring = decformat % (ddd, dmm, dss)
    return rastring, decstring

## Argument parser
parser = argparse.ArgumentParser()
parser.add_argument("-r", "--ra", help="Force RA value")
parser.add_argument("-d", "--dec", help="Force Dec value: use no space if declination is negative, i.e., -d-63:20:23.3")
parser.add_argument("-b", "--bits", type=int, default=1,help="Number of bits")
parser.add_argument("-k", "--keep", default=False, action="store_true", help="Keeop exisiting codif files")
parser.add_argument("-f", "--fpga", help="FPGA and card for delay correction. E.g. c4_f0")
parser.add_argument('fileglob', help="glob pattern for vcraft files")
args = parser.parse_args()

vcraftglobpattern = args.fileglob
keepCodif = args.keep # Don't rerun CRAFTConverter

vcraftfiles = glob.glob(vcraftglobpattern)
if len(vcraftfiles) == 0:
    print "Didn't find any vcraft files!"
    sys.exit()

freqs = []
beamra = None
beamdec = None
startmjd = None
mode = None
first = True

for file in vcraftfiles:
    for line in open(file).readlines(10000):
        if (first):
            if line.split()[0] == "FREQS":
                freqs = line.split()[1].split(',')
            if line.split()[0] == "BEAM_RA":
                beamra = float(line.split()[1])
            if line.split()[0] == "BEAM_DEC":
                beamdec = float(line.split()[1])
            if line.split()[0] == "MODE":
                mode = int(line.split()[1])
        if line.split()[0] == "TRIGGER_MJD":
            thisMJD = float(line.split()[1])
            
            if startmjd==None:
                startmjd = thisMJD
            elif thisMJD<startmjd:
                startmjd = thisMJD

        if len(freqs) > 0 and beamra!=None and beamdec!=None and startmjd!=None:
            break

# Pinched from vcraft.py
SAMPS_PER_WORD32 = [1,2,4,16,1,2,4,16]
MODE_BEAMS = [72,72,72,72,2,2,2,2]
MAX_SETS = 29172

#  Number of sample vs mode
SAMPS_MODE = [29172*32*nsamp_per_word*72/nbeams for (nsamp_per_word, nbeams) in zip(SAMPS_PER_WORD32, MODE_BEAMS)]

# Correct time because TRIGGER_MJD is time at END of file
startmjd -= (SAMPS_MODE[mode]-SAMPS_PER_WORD32[mode])*27.0/32.0*1e-6/(24*60*60)

if beamra==None or beamdec==None or startmjd==None:
    print "Didn't find all info in", vcraftheader
    sys.exit()

startmjd = math.floor(startmjd*60*60*24)/(60*60*24) # Round down to nearest second

# Write the obs.txt file
rastring, decstring = posradians2string(beamra*math.pi/180, beamdec*math.pi/180)

if args.ra!=None:
    rastring = args.ra
if args.dec!=None:
    decstring = args.dec

correlateseconds = 20
framesize = 8064
if args.bits == 4:
    correlateseconds = 6
    framesize = 8256
elif args.bits == 8:
    correlateseconds = 4
elif args.bits == 16:
    correlateseconds = 3

output = open("obs.txt", "w")
output.write("startmjd    = %.9f\n" % startmjd)
output.write("stopmjd     = %.9f\n" % (startmjd + float(correlateseconds)/86400.0))
output.write("srcname     = CRAFTSRC\n")
output.write("srcra       = %s\n" % rastring)
output.write("srcdec      = %s\n" % decstring)
output.close()

# Write the chandefs file
output = open("chandefs.txt", "w")
for f in freqs:
    output.write("%s L 1.185185185185185185\n" % str(int(f)-1))
output.close()

# Run the converter for each vcraft file
antlist = ""
codifFiles = []
for f in vcraftfiles:
    antname = f.split('/')[-1].split('_')[0]
    if antname == "":
        print "Didn't find the antenna name in the header!"
        sys.exit()
    antlist += antname + ","
    codifName = "%s.codif" % (antname)
    if not keepCodif or not os.path.exists(codifName):
        print "CRAFTConverter %s %s" % (f, codifName)
        ret = os.system("CRAFTConverter %s %s" % (f, codifName))
        if (ret!=0): sys.exit(ret)
    codifFiles.append(codifName)

# Write a machines file and a run.sh file
output = open("machines","w")
for i in range(len(vcraftfiles)+2):
    output.write("localhost\n")
output.close()

output = open("run.sh","w")
output.write("#!/bin/sh\n\n")
output.write("rm -rf craft.difx\n")
output.write("rm -rf log*\n")
output.write("errormon2 6 &\n")
output.write("export ERRORMONPID=$!\n")
output.write("mpirun -machinefile machines -np %d mpifxcorr craft.input\n" % (len(vcraftfiles)+2))
output.write("kill $ERRORMONPID\n")
output.write("rm -f craft.difxlog\n")
output.write("mv log craft.difxlog\n")
output.close()

# Print out the askap2difx command line to run (ultimately, could just run it ourselves)
runline = "askap2difx.py fcm.txt obs.txt chandefs.txt --ants=" + antlist[:-1] + " --bits=" + str(args.bits) + " --framesize=" + str(framesize)
if args.fpga is not None: runline += " --fpga {}".format(args.fpga)

print "\nNow run:"
print runline
with open('runaskap2difx', 'w') as runaskap:
    runaskap.write(runline)
os.chmod('runaskap2difx',0o775)
