#!/usr/bin/env python2
import os, sys, glob, math, argparse, getpass

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
parser.add_argument("-k", "--keep", default=False, action="store_true", help="Keep exisiting codif files")
parser.add_argument("-f", "--fpga", help="FPGA and card for delay correction. E.g. c4_f0")
parser.add_argument("-p", "--polyco", help="Bin config file for pulsar gating")
parser.add_argument("-i", "--integration", type=float, help="Correlation integration time")
parser.add_argument("--ts", default=0, type=int, help="Use taskspooler to run CRAFTConverter, with N parallel tasks")
parser.add_argument("-s", "--slurm", default=False, action="store_true", help="Use slurm batch jobs rather than running locally")
parser.add_argument("-n", "--nchan", type=int, help="Number of spectral channels")
parser.add_argument("--forceFFT", default=False, action="store_true", help="Force FFT size to equal number of channels (don't increase to 128)")
parser.add_argument('fileglob', help="glob pattern for vcraft files", nargs='+')
parser.add_argument("--gstar", default=False, action="store_true", help="Set if using gstar for correlation")
parser.add_argument("--large", default=False, action="store_true", help="Set if 32 nodes, 384 tasks are required (i.e., 23GB memory needed per task; else 16 nodes, 192 tasks will be used for 11.5GB per task")
parser.add_argument("--numskylakenodes", default=1, type=int, help="Use 32x this many CPUs")
args = parser.parse_args()

# Check that sensible options were given for the queue destination
if args.large and not args.gstar:
    parser.error("You can't run large if runnning on skylake (the default, i.e. you didn't use --gstar")
if args.gstar and args.numskylakenodes > 1:
    parser.error("You can't set the number of skylake nodes if you are running on gstar")

vcraftglobpattern = args.fileglob
npol = len(vcraftglobpattern)
if len(vcraftglobpattern) > 2:
    #print vcraftglobpattern
    parser.error("Can only have at most two fileglobs, corresponding to X and Y pols")

keepCodif = args.keep # Don't rerun CRAFTConverter

vcraftfiles = []
for g in vcraftglobpattern:
    vcraftfiles.append(glob.glob(g))
    if len(vcraftfiles[-1]) == 0:
        print "Didn't find any vcraft files!"
        sys.exit()
if not len(vcraftfiles[0]) == len(vcraftfiles[-1]):
    print "Number of vcraft files for X and Y doesn't match"

nant = len(vcraftfiles[0])
freqs = []
beamra = None
beamdec = None
startmjd = None
mode = None
first = True
samprate = None
nsamps = None

for file in vcraftfiles[0]:
    for line in open(file).readlines(4000):
        #WARN: This seems off because first is never unset, plus we bail out as soon as all have been found anyway...?
        if (first):
            if line.split()[0] == "FREQS":
                freqs = line.split()[1].split(',')
            if line.split()[0] == "BEAM_RA":
                beamra = float(line.split()[1])
            if line.split()[0] == "BEAM_DEC":
                beamdec = float(line.split()[1])
            if line.split()[0] == "MODE":
                mode = int(line.split()[1])
        if line.split()[0] == "SAMP_RATE":
            samprate = float(line.split()[1])
        if line.split()[0] == "NSAMPS_REQUEST": 
            nsamps = int(line.split()[1])
        if line.split()[0] == "TRIGGER_MJD":
            thisMJD = float(line.split()[1])
            
            if samprate != None and nsamps != None:
                thisMJD -= nsamps/(samprate*86400)
                if startmjd==None:
                    startmjd = thisMJD
                elif thisMJD<startmjd:
                    startmjd = thisMJD

        if len(freqs) > 0 and beamra!=None and beamdec!=None and startmjd!=None and mode != None:
            break

# Double check that the frequencies match
if npol > 1:
    for file in vcraftfiles[1]:
        for line in open(file).readlines(4000):
            if line.split()[0] == "FREQS":
                if line.split()[1].split(',') != freqs:
                    print "file", file, " has different FREQS! Aborting"
                    sys.exit()
                else:
                    break

# Check we found everything
if beamra==None or beamdec==None or startmjd==None or mode == None or len(freqs) == 0:
    print "Didn't find all info in", vcraftheader
    sys.exit()

## All the commented out code is no longer needed because correction is done at time of reading TRIGGER_MJD above

## Pinched from vcraft.py
#SAMPS_PER_WORD32 = [1,2,4,16,1,2,4,16]
#MODE_BEAMS = [72,72,72,72,2,2,2,2]
#MAX_SETS = 29172
#
##  Number of sample vs mode
#SAMPS_MODE = [29172*32*nsamp_per_word*72/nbeams for (nsamp_per_word, nbeams) in zip(SAMPS_PER_WORD32, MODE_BEAMS)]
#
## Correct time because TRIGGER_MJD is time at END of file
#startmjd -= (SAMPS_MODE[mode]-SAMPS_PER_WORD32[mode])*27.0/32.0*1e-6/(24*60*60)

startmjd = math.floor(startmjd*60*60*24)/(60*60*24) # Round down to nearest second

# Write the obs.txt file
rastring, decstring = posradians2string(beamra*math.pi/180, beamdec*math.pi/180)

# Overwrite the RA/Dec with command line values if supplied
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
    # vcraft headers apparently currently have a 1 MHz frequency offset - correct this
    #WARN This should probably be regularly checked!
    # Also this can be upper sideband in some cases!
    output.write("%s L 1.185185185185185185\n" % str(int(f)-1))
if npol > 1:
    for f in freqs:
        # vcraft headers apparently currently have a 1 MHz frequency offset - correct this
        #WARN This should probably be regularly checked!
        # Also this can be upper sideband in some cases!
        output.write("%s L 1.185185185185185185\n" % str(int(f)-1))
output.close()

if args.ts > 0:
    print "Waiting on CRAFTConverter to finish"
    ret = os.system("tsp -S {}".format(args.ts))
    if (ret!=0): sys.exit(ret)

# Run through each vcraft file, converting (or writing a mini-script to be run by slurm)
totalnumcodiffiles = 0
ncodifparallel = 8
antlist = ""
codifFiles = []
convertlines = []
for i in range(npol):
    codifFiles.append([])
    for f in vcraftfiles[i]:
        if not os.path.exists(".bat0"):
            ret = os.system("bat0.pl %s" % (f))
            if (ret!=0): sys.exit(ret)    
        
        antname = f.split('/')[-1].split('_')[0]
        if antname == "":
            print "Didn't find the antenna name in the header!"
            sys.exit()
        if i == 0:
            antlist += antname + ","
        codifName = "%s.p%d.codif" % (antname, i)
        if not keepCodif or not os.path.exists(codifName):
            runline = "CRAFTConverter %s %s" % (f, codifName)
            if args.slurm:
                totalnumcodiffiles += 1
                convertlines.append(runline)
            else:
                if args.ts > 0:
                    runline = "tsp " + runline
                print runline
                ret = os.system(runline)
                if (ret!=0): sys.exit(ret)
        codifFiles[i].append(codifName)

if args.slurm and len(convertlines) > 0:
    currentuser = getpass.getuser()
    for i in range(ncodifparallel):
        output = open("convertcodif.%d" % (i+1), "w")
        output.write("#!/bin/bash\n")
        output.write(". /home/{0}/setup_difx\n".format(currentuser))
        output.close()
    for count, runline in enumerate(convertlines):
        output = open("convertcodif.%d" % ((count%ncodifparallel) + 1), "a")
        output.write(runline + "\n")
        output.close()
    for i in range(ncodifparallel):
        os.system("chmod 775 convertcodif.%d" % (i+1))


# If we running via batch, run that now
if args.slurm:
    # Produce a overall sbatch script for the CRAFT Conversion stage
    # This will parallelise over ncodifparallel (default 8) nodes
    output = open("runcraftconversionbatch.sh","w")
    output.write("#!/bin/bash\n")
    output.write("#\n")
    output.write("#SBATCH --job-name=test_craftconverter\n")
    output.write("#SBATCH --output=testt_craftconverter.txt\n")
    output.write("#\n")
    output.write("#SBATCH --ntasks=1\n")
    output.write("#SBATCH --time=02:00\n")
    output.write("#SBATCH --mem-per-cpu=200\n")
    output.write("#SBATCH --array 1-%d\n\n" % ncodifparallel)
    output.write("srun ./convertcodif.$SLURM_ARRAY_TASK_ID\n")
    output.close()

    # Run that sbatch script
    os.system("sbatch --wait runcraftconversionbatch.sh")

# Write a machines file and a run.sh file
output = open("machines","w")
for i in range(nant+2):
    output.write("localhost\n")
output.close()

output = open("run.sh","w")
output.write("#!/bin/sh\n\n")
output.write("rm -rf craft.difx\n")
output.write("rm -rf log*\n")
output.write("errormon2 6 &\n")
output.write("export ERRORMONPID=$!\n")
output.write("mpirun -machinefile machines -np %d mpifxcorr craft.input\n" % (nant+2))
output.write("kill $ERRORMONPID\n")
output.write("rm -f craft.difxlog\n")
output.write("mv log craft.difxlog\n")
output.close()

if args.ts > 0:
    print "Waiting on CRAFTConverter to finish"
    ret = os.system("tsp -w")
    if (ret!=0): sys.exit(ret)
        
# Print out the askap2difx command line to run (ultimately, could just run it ourselves)
runline = "askap2difx.py fcm.txt obs.txt chandefs.txt --ants=" + antlist[:-1] + " --bits=" + str(args.bits) + " --framesize=" + str(framesize) + " --npol=" + str(npol)
if args.slurm: runline += " --slurm"
if args.fpga is not None: runline += " --fpga {}".format(args.fpga)
if args.polyco is not None: runline += " --polyco={}".format(args.polyco)
if args.integration is not None: runline += " --integration={}".format(args.integration)
if args.nchan is not None: runline += " --nchan={}".format(args.nchan)
if args.forceFFT: runline += " --forceFFT"
if args.gstar: runline += " --gstar"
if args.large: runline += " --large"
if args.numskylakenodes > 1:
    runline += " --numskylakenodes=" + str(args.numskylakenodes)
runline += "\n"
print "\nNow run:"
print runline
with open('runaskap2difx', 'w') as runaskap:
    runaskap.write(runline)
os.chmod('runaskap2difx',0o775)
