#!/usr/bin/env ParselTongue
#Imports ########################################################
from AIPS import AIPS
from AIPSTask import AIPSTask
from AIPSData import AIPSUVData
from optparse import OptionParser
import os, sys, glob, vlbatasks

################################################################################
# Global variables and option parsing
################################################################################
try:
    aipsver = os.environ['PSRVLBAIPSVER']
except KeyError:
    aipsver = '31DEC18'
usage = "usage: %prog [options]"
parser = OptionParser(usage)
parser.add_option("-t", "--target", default="",
                  help="The target FITS file, to be calibrated and optionally imaged")
parser.add_option("-c", "--calibrator", default="",
                  help="The calibrator FITS file (usually on 0407)")
parser.add_option("-r", "--refant", type=int, default=3,
                  help="The reference antenna to use.")
parser.add_option("-u", "--userno", type=int, default=2,
                  help="The AIPS user number")
parser.add_option("-s", "--sourcename", default="CRAFTSRC",
                  help="The name of the source in the FITS files")
parser.add_option("-f", "--flux", type=float, default=9.5, # 0407 flux
                  help="Calibrator flux in Jy,  Defaulted to correct value for 0407")
parser.add_option("-i", "--imagecube", default=False, action="store_true",
                  help="Image the FRB in chunks (i.e., make a cube)")
parser.add_option("-j", "--imagejmfit", default=False, action="store_true",
                  help="Jmfit the individual slices of the cube")
parser.add_option("-a", "--averagechannels", type=int, default=24,
                  help="Number of channels to average together per cube slice")
parser.add_option("-F", "--flagfile", default="", 
                  help="Flag file to apply to calibrator data only, if desired. Used to ensure RFI doesn't corrupt FRING or BPASS.")
parser.add_option("-p","--phasecenter", default="",
                  help="phase center for the target field (blank will leave it at correlation centre)")
parser.add_option("--imagesize", type=int, default=128, 
                  help="Size of the image to make")
(options, junk) = parser.parse_args()
AIPS.userno     = options.userno
refant          = options.refant
imagesize       = options.imagesize
snversion       = 1
clversion       = 1
aipsdisk        = 1

# Make path names absolute if needed
options.target = os.path.abspath(options.target)
options.calibrator = os.path.abspath(options.calibrator)

# Get some other path names
outputfilename = options.target[:-5] + "_calibrated_uv.fits"
msfilename = outputfilename[:-4] + "ms"

# Check if the ms already exists, abort if so
if os.path.exists(msfilename):
    print msfilename, "already exists - aborting here!!!"
    sys.exit()

# Load up the target data
targetdata = vlbatasks.zapAndCreateUVData("CRAFTTARG", "UVDATA", aipsdisk, 1)
if targetdata.exists():
    targetdata.zap()
vlbatasks.fitld_corr(options.target, targetdata, [], '', 0.001)

# Get the number of channels in the dataset
numchannels = vlbatasks.getNumChannels(targetdata)
# Load up the calibrator data
caldata = vlbatasks.zapAndCreateUVData("CRAFTCAL","UVDATA", aipsdisk, 1)
if caldata.exists():
    caldata.zap()
vlbatasks.fitld_corr(options.calibrator, caldata, [], '', 0.001)

# Flag the calibrator data, if desired
if options.flagfile != "":
    vlbatasks.userflag(caldata, 1, options.flagfile)

# Run FRING
solintmins = 1 # Long enough that we just get one solutions
inttimesecs = 0.5 # Doesn't really matter if this is wrong
applybandpasscal = False
snrlimit = 6
sumifs = False
modeldata = None
sumpols = False
uvrange = [0,0]
zerorates = True
delaywindow = 0 # Search everything
ratewindow = -1 # Don't search rates
vlbatasks.fring(caldata, snversion, clversion, solintmins, inttimesecs, 
                options.sourcename, refant, applybandpasscal, snrlimit,
                sumifs, modeldata, sumpols, uvrange, zerorates, 
                delaywindow, ratewindow)

# Copy results
vlbatasks.tacop(caldata, "SN", snversion, targetdata, snversion)

# Calibrate
vlbatasks.applysntable(caldata, snversion, "SELN", clversion, refant)
vlbatasks.applysntable(targetdata, snversion, "SELN", clversion, refant)
snversion += 1
clversion += 1

## Run BPASS
#scannumber = 1
#bpversion = 1
#vlbatasks.bpass(caldata, options.sourcename, clversion, scannumber)

# Run CPASS rather than BPASS
scannumber = 1
bpversion = 1
vlbatasks.cpass(caldata, options.sourcename, clversion, scannumber)

# Copy results
vlbatasks.tacop(caldata, "BP", bpversion, targetdata, bpversion)

# Would be nice to plot bpass here....

# Run selfcal
applybandpasscal = True
outklass = "SPLIT"
splitsnversion = 1
doampcal = True
dostokesi = False
soltype = "L1R"
selfcalsnr = 5
splitcaldata = AIPSUVData(options.sourcename, outklass, 1, 1)
if splitcaldata.exists():
    splitcaldata.zap()
vlbatasks.split(caldata, clversion, outklass, options.sourcename)
vlbatasks.singlesource_calib(splitcaldata, options.flux, splitsnversion, options.refant, doampcal, 
                             solintmins, dostokesi, soltype, selfcalsnr, sumifs)

# Copy results
vlbatasks.tacop(splitcaldata, "SN", splitsnversion, caldata, snversion)
vlbatasks.tacop(splitcaldata, "SN", splitsnversion, targetdata, snversion)

# Calibrate
vlbatasks.applysntable(caldata, snversion, "SELN", clversion, refant)
vlbatasks.applysntable(targetdata, snversion, "SELN", clversion, refant)
snversion += 1
clversion += 1

# Run SPLIT and write output data for calibrator
seqno = 1
outputfilename = options.calibrator[:-5] + "_calibrated_uv.fits"
outputdata = vlbatasks.zapAndCreateUVData("CRAFTSRC","SPLIT",aipsdisk,seqno)
vlbatasks.splitmulti(caldata, clversion, outklass, options.sourcename, seqno)
vlbatasks.writedata(outputdata, outputfilename, True)

# Run SPLIT and write output data for target
seqno = 1
outputdata = vlbatasks.zapAndCreateUVData("CRAFTSRC","SPLIT",aipsdisk,seqno)
vlbatasks.splitmulti(targetdata, clversion, outklass, options.sourcename, seqno)
vlbatasks.writedata(outputdata, outputfilename, True)

# Convert to a measurement set
uvfitsfilename = outputfilename
casaout = open("loadtarget.py","w")
casaout.write("importuvfits(fitsfile='%s',vis='%s',antnamescheme='old')\n" % (uvfitsfilename, msfilename))
casaout.close()
os.system("casa -c loadtarget.py")

# Run the imaging via CASA if desired
if options.imagecube:
    # Do the cube
    for pol in ["XX","YY","I","Q","U","V"]:
        casaout = open("imagescript.py","w")
        imagebase = "TARGET.cube.%s" % (pol)
        os.system("rm -rf %s.*" % imagebase)
        imagename = imagebase + ".image"
        maskstr = "'circle [[%dpix,%dpix] ,5pix ]'" % (imagesize/2,imagesize/2)
        #casaout.write('clean(vis="%s",imagename="%s",outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=100,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask="FRB.cube.mask",nchan=-1,start=1,width=24,outframe="",veltype="radio",imsize=128,cell=["1.0arcsec", "1.0arcsec"],phasecenter="%s",restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (msfilename, imagebase, options.phasecentre, pol))
        casaout.write('clean(vis="%s",imagename="%s",outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=100,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask=%s,nchan=-1,start=1,width=%d,outframe="",veltype="radio",imsize=%s,cell=["1.0arcsec", "1.0arcsec"],phasecenter="%s",restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (msfilename, imagebase, maskstr, options.averagechannels, imagesize, options.phasecenter, pol))
        casaout.close()
        os.system("casa -c imagescript.py")

        # If desired, also make the JMFIT output
        if options.imagejmfit:
            casaout = open("imagescript.py","w")
            casaout.write('exportfits(imagename="%s.image",fitsimage="%s.fits")\n' % (imagebase, imagebase))
            casaout.close()
            os.system("casa -c imagescript.py")
            for i in range(numchannels/options.averagechannels):
                locstring = "%d,%d,%d,%d,%d,%d" % (imagesize/2-12, imagesize/2-12, i, imagesize/2+12, imagesize/2+12, i)
                os.system("jmfitfromfile.py %s.fits %s.slice%03d.jmfit.stats %s" % (imagebase, imagebase, i, locstring))

