#!/usr/bin/env ParselTongue
#Imports ########################################################
from AIPS import AIPS
from AIPSTask import AIPSTask
from AIPSData import AIPSUVData, AIPSImage
from optparse import OptionParser
import os, sys, datetime, socket, glob, vlbatasks

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
parser.add_option("-g", "--tarflagfile", default="", 
                  help="Flag file to apply to target data only, if desired. Used to flag any necessary channels for, e.g., RFI or missing data")
parser.add_option("-p","--phasecenter", default="",
                  help="phase center for the target field (blank will leave it at correlation centre)")
#parser.add_option("-l", "--leakagecorrect", default=False, action="store_true", 
#                  help="Run lpcal to try and correct any leakage present")
parser.add_option("-x", "--xpoldelaymodelfile", default="", help="Model to use for xpol delay correction (blank = no correction)")
parser.add_option("--imagesize", type=int, default=128, help="Size of the image to make")
parser.add_option("--pixelsize", type=float, default=1, help="Pixel size in arcseconds")
parser.add_option("--uvsrt", default=False, action="store_true", help="Run UVSRT on the data after loading")
parser.add_option("--noisecentre", default="", help="CASA format position at which noise should be estimated, blank=don't make an off-source image")
(options, junk) = parser.parse_args()
AIPS.userno     = options.userno
refant          = options.refant
imagesize       = options.imagesize
pixelsize       = options.pixelsize
xpolmodelfile   = options.xpoldelaymodelfile
snversion       = 1
clversion       = 1
aipsdisk        = 1


# Make path names absolute if needed
options.target = os.path.abspath(options.target)
options.calibrator = os.path.abspath(options.calibrator)
if xpolmodelfile != '':
    xpolmodelfile = os.path.abspath(xpolmodelfile)

# Get some other path names
if ".uvfits" in options.target:
    targetoutputfilename = os.getcwd() + '/' + options.target.split('/')[-1][:-7] + "_calibrated_uv.fits"
else:
    targetoutputfilename = os.getcwd() + '/' + options.target.split('/')[-1][:-5] + "_calibrated_uv.fits"
targetmsfilename = targetoutputfilename[:-4] + "ms"

if ".uvfits" in options.calibrator:
    calibratoroutputfilename = os.getcwd() + '/' + options.calibrator.split('/')[-1][:-7] + "_calibrated_uv.fits"
else:
    calibratoroutputfilename = os.getcwd() + '/' + options.calibrator.split('/')[-1][:-5] + "_calibrated_uv.fits"
calibratormsfilename = calibratoroutputfilename[:-4] + "ms"

# Define some output filenames
bpfilename = os.path.abspath("bandpasses.bp.txt")
fringsnfilename = os.path.abspath("delays.sn.txt")
selfcalsnfilename = os.path.abspath("selfcal.sn.txt")
xpolsnfilename = os.path.abspath("xpolfring.sn")

# Check if the ms already exists, abort if so
if os.path.exists(targetmsfilename):
    print targetmsfilename, "already exists - aborting here!!!"
    sys.exit()

if os.path.exists(calibratormsfilename):
    print calibratormsfilename, "already exists - aborting here!!!"
    sys.exit()


# Load up the target data
targetdata = vlbatasks.zapAndCreateUVData("CRAFTTARG", "UVDATA", aipsdisk, 1)
vlbatasks.fitld_corr(options.target, targetdata, [], '', 0.0001)
if options.uvsrt:
    sortedtargetdata = vlbatasks.zapAndCreateUVData("CRAFTTARG", "UVSRT", aipsdisk, 1)
    vlbatasks.uvsrt(targetdata, sortedtargetdata)
    targetdata.zap()
    targetdata = sortedtargetdata

# Get the number of channels in the dataset
numchannels = vlbatasks.getNumChannels(targetdata)

# Get the reference frequency of the dataset
reffreqs = []
fqtable = targetdata.table('FQ', 1)
for row in fqtable:
    try:
        for iffreq in row.if_freq:
            freqentry = float(iffreq) + float(targetdata.header.crval[2])
            reffreqs.append(float(iffreq) + float(targetdata.header.crval[2]))
    except (AttributeError, TypeError):
        freqentry = float(row.if_freq) + float(targetdata.header.crval[2])
        reffreqs.append(float(row.if_freq) + float(targetdata.header.crval[2]))

# Load up the calibrator data
caldata = vlbatasks.zapAndCreateUVData("CRAFTCAL","UVDATA", aipsdisk, 1)
vlbatasks.fitld_corr(options.calibrator, caldata, [], '', 0.0001)
if options.uvsrt:
    sortedcaldata = vlbatasks.zapAndCreateUVData("CRAFTCAL", "UVSRT", aipsdisk, 1)
    vlbatasks.uvsrt(caldata, sortedcaldata)
    caldata.zap()
    caldata = sortedcaldata

# Flag the calibrator data, if desired
if options.flagfile != "":
    vlbatasks.userflag(caldata, 1, options.flagfile)

# Flag the target data, if desired
if options.tarflagfile != "":
    vlbatasks.userflag(targetdata, 1, options.tarflagfile)

# Run CLCOR to correct PANG if needed
if xpolmodelfile != "":
    vlbatasks.clcor_pang(caldata, clversion)
    vlbatasks.clcor_pang(targetdata, clversion)
    clversion = clversion + 1

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

# Write SN table to disk
vlbatasks.writetable(caldata, "SN", snversion, fringsnfilename)

# Calibrate
vlbatasks.applysntable(caldata, snversion, "SELN", clversion, refant)
vlbatasks.applysntable(targetdata, snversion, "SELN", clversion, refant)
snversion += 1
clversion += 1

# Correct for leakage if needed
#leakagedopol = 0
if xpolmodelfile != "":
    # First the xpoldelays
    xpolscan = 1
    if not os.path.exists(xpolmodelfile):
        print "Can't find xpol delay model  " + xpolmodelfile
        print "Aborting!!"
        sys.exit(1)
    xpolmodel = AIPSImage("LKGSRC", "CLEAN", 1, 1)
    if xpolmodel.exists():
        xpolmodel.zap()
    vlbatasks.fitld_image(xpolmodelfile, xpolmodel)
    xpolsolintmins = 1
    inttimesecs = 0.5 # Doesn't matter if this is wrong
    if os.path.exists(xpolsnfilename):
        os.remove(xpolsnfilename)
    vlbatasks.xpoldelaycal(caldata, clversion, refant,
                           options.sourcename, xpolscan, xpolmodel, xpolsolintmins,
                           inttimesecs, xpolsnfilename, delaywindow, ratewindow)
    vlbatasks.loadtable(caldata, xpolsnfilename, snversion)
    vlbatasks.applysntable(caldata, snversion, '2PT', clversion, refant)
    vlbatasks.loadtable(targetdata, xpolsnfilename, snversion)
    vlbatasks.applysntable(targetdata, snversion, '2PT', clversion, refant)
    snversion += 1
    clversion += 1

    ## Then the leakage
    #leakagefilename = os.getcwd() + "/leakage.an"
    #hasbptable = False
    #leakagemodel = xpolmodel
    #leakageacalmins = 1
    #leakagepcalmins = 1
    #leakagescan = 1
    #hasbptable = False
    #leakageoutputfile = os.getcwd() + '/' + options.sourcename + "_leakagecal_uv.fits"
    #leakageuvrange = [0,0]
    #leakageweightit = 0
    #vlbatasks.leakagecalc(caldata, options.sourcename, leakagemodel, leakagefilename,
    #            refant, leakageacalmins, leakagepcalmins, leakagescan, clversion,
    #            hasbptable, leakageoutputfile, leakageuvrange, leakageweightit)
    #vlbatasks.deletetable(caldata, "AN", 1)
    #vlbatasks.loadtable(caldata, leakagefilename, 1)
    #vlbatasks.deletetable(targetdata, "AN", 1)
    #vlbatasks.loadtable(targetdata, leakagefilename, 1)
    #leakagedopol = 2
    #print "Need to actually use leakagedopol below here - aborting!"
    #sys.exit()

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

# Write SN table to disk
vlbatasks.writetable(caldata, "BP", bpversion, bpfilename)

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
for i in range(1,300):
    todeletedata = AIPSUVData(options.sourcename, "CALIB", 1, i)
    if todeletedata.exists():
        todeletedata.zap()
vlbatasks.singlesource_calib(splitcaldata, options.flux, splitsnversion, options.refant, doampcal, 
                             solintmins, dostokesi, soltype, selfcalsnr, sumifs)

# Copy results
vlbatasks.tacop(splitcaldata, "SN", splitsnversion, caldata, snversion)
vlbatasks.tacop(splitcaldata, "SN", splitsnversion, targetdata, snversion)

# Write SN table to disk
vlbatasks.writetable(caldata, "SN", snversion, selfcalsnfilename)

# Calibrate
vlbatasks.applysntable(caldata, snversion, "SELN", clversion, refant)
vlbatasks.applysntable(targetdata, snversion, "SELN", clversion, refant)
snversion += 1
clversion += 1

# Run SPLIT and write output data for calibrator
seqno = 1
outputdata = vlbatasks.zapAndCreateUVData("CRAFTSRC","SPLIT",aipsdisk,seqno)
vlbatasks.splitmulti(caldata, clversion, outklass, options.sourcename, seqno)
vlbatasks.writedata(outputdata, calibratoroutputfilename, True)

# Run SPLIT and write output data for target
seqno = 1
outputdata = vlbatasks.zapAndCreateUVData("CRAFTSRC","SPLIT",aipsdisk,seqno)
vlbatasks.splitmulti(targetdata, clversion, outklass, options.sourcename, seqno)
vlbatasks.writedata(outputdata, targetoutputfilename, True)

# Create a README file for the calibration and a tarball with it plus all the calibration
readmeout = open("README.calibration", "w")
tarinputfiles = "%s %s %s" % (fringsnfilename.split('/')[-1], selfcalsnfilename.split('/')[-1], bpfilename.split('/')[-1])
readmeout.write("This calibration was derived as follows:\n")
readmeout.write("Calibrator file: %s\n" % options.calibrator)
readmeout.write("Run on host: %s\n" % socket.gethostname())
readmeout.write("At time: %s\n\n" % (str(datetime.datetime.now())))
readmeout.write("The following set of files was produced and used for calibration:\n")
readmeout.write("%s (frequency-independent delay and phase from FRING)\n" % fringsnfilename.split('/')[-1])
readmeout.write("%s (frequency-independent complex gain [mostly just amplitude] from CALIB to set absolute flux scale)\n" % selfcalsnfilename.split('/')[-1])
if xpolmodelfile != "":
    readmeout.write("%s (frequency-independent, antenna-independent X-Y delay from FRING)\n" % xpolsnfilename.split('/')[-1])
    tarinputfiles = tarinputfiles + " " + xpolsnfilename.split('/')[-1]
readmeout.write("%s (frequency-dependent complex gain from CPASS [polynomial bandpass fit])\n\n" % bpfilename.split('/')[-1])
readmeout.write("Remember that the delay specified in the SN tables generates zero phase at the reference frequency of the observation\n")
readmeout.write("This reference frequency is set per subband (AIPS \"IF\"), but CRAFT datasets should only have one IF and hence one reference frequency.\n")
readmeout.write("Reference frequency(s) of this file:\n")
for i, reffreq in enumerate(reffreqs):
    readmeout.write("AIPS IF %d ref (MHz): %.9f\n" % (i, reffreq))
readmeout.write("\nFinally I note that long-term, we really should also be solving for the leakage and writing both it and the parallactic angle corrections out.\n")
readmeout.close()
calibtarballfile = "calibration.tar.gz"
if os.path.exists(calibtarballfile):
    os.system("rm -f " + calibtarballfile)
os.system("tar cvzf %s README.calibration %s" % (calibtarballfile, tarinputfiles))

# Convert to a measurement set
casaout = open("loadtarget.py","w")
casaout.write("importuvfits(fitsfile='%s',vis='%s',antnamescheme='old')\n" % (calibratoroutputfilename, calibratormsfilename))
casaout.close()
os.system("casa --nologger -c loadtarget.py")
casaout = open("loadtarget.py","w")
casaout.write("importuvfits(fitsfile='%s',vis='%s',antnamescheme='old')\n" % (targetoutputfilename, targetmsfilename))
casaout.close()
os.system("casa --nologger -c loadtarget.py")


# Run the imaging via CASA if desired
if options.imagecube:
    # Do the cube
    for pol in ["XX","YY","I","Q","U","V"]:
        casaout = open("imagescript.py","w")
        imagebase = "TARGET.cube.%s" % (pol)
        os.system("rm -rf %s.*" % imagebase)
        imagename = imagebase + ".image"
        maskstr = "'circle [[%dpix,%dpix] ,5pix ]'" % (imagesize/2,imagesize/2)
        #casaout.write('clean(vis="%s",imagename="%s",outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=100,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask="FRB.cube.mask",nchan=-1,start=1,width=24,outframe="",veltype="radio",imsize=128,cell=["1.0arcsec", "1.0arcsec"],phasecenter="%s",restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (targetmsfilename, imagebase, options.phasecentre, pol))
        casaout.write('clean(vis="%s",imagename="%s",outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=100,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask=%s,nchan=-1,start=1,width=%d,outframe="",veltype="radio",imsize=%s,cell=["%.2farcsec", "%.2farcsec"],phasecenter="%s",restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (targetmsfilename, imagebase, maskstr, options.averagechannels, imagesize, pixelsize, pixelsize, options.phasecenter, pol))

        # If desired, produce the noise image as well
        if len(options.noisecentre) > 1:
            offsourcebase = "OFFSOURCE.cube.%s" % (pol)
            imagebases = '["%s","%s"]' % (imagebase,offsourcebase)
            os.system("rm -rf %s.*" % offsourcebase)
            maskstr2 = "[%s,'']" % (maskstr)
            imsizestr = "[[%d,%d],[%d,%d]]" % (imagesize, imagesize, imagesize*4, imagesize*4)
            cellstr = '["%.2farcsec", "%.2farcsec"]' % (pixelsize, pixelsize)
            phasecenterstr = "['', '%s']" % (options.noisecentre)
            casaout.write('clean(vis="%s",imagename=%s,outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=0,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask=%s,nchan=-1,start=1,width=%d,outframe="",veltype="radio",imsize=%s,cell=%s,phasecenter=%s,restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (targetmsfilename, imagebases, maskstr2, options.averagechannels, imsizestr, cellstr, phasecenterstr, pol))
            
        casaout.close()
        os.system("casa --nologger -c imagescript.py")


        # If desired, also make the JMFIT output
        if options.imagejmfit:
            casaout = open("imagescript.py","w")
            casaout.write('exportfits(imagename="%s.image",fitsimage="%s.fits")\n' % (imagebase, imagebase))
            casaout.close()
            os.system("casa --nologger -c imagescript.py")
            for i in range(numchannels/options.averagechannels):
                locstring = "%d,%d,%d,%d,%d,%d" % (imagesize/2-12, imagesize/2-12, i, imagesize/2+12, imagesize/2+12, i)
                os.system("jmfitfromfile.py %s.fits %s.slice%03d.jmfit.stats %s" % (imagebase, imagebase, i, locstring))
