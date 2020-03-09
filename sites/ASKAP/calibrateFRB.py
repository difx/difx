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
parser.add_option("--dirtyonly", default=False, action="store_true",
                  help="Just make a dirty image, no cleaning")
parser.add_option("--calibrateonly", default=False, action="store_true",
                  help="Only generate the calibration files, don't do anything with target")
parser.add_option("--targetonly", default=False, action="store_true",
                  help="Use saved calibration files")
parser.add_option("--bpass", default=False, action="store_true",
                  help="Use BPASS rather than CPASS to do the bandpass correction")
parser.add_option("-j", "--imagejmfit", default=False, action="store_true",
                  help="Jmfit the individual slices of the cube")
parser.add_option("--cpasspoly", default=10, type=int,
                  help="Number of polynomial terms in CPASS")
parser.add_option("-a", "--averagechannels", type=int, default=24,
                  help="Number of channels to average together per cube slice")
parser.add_option("-F", "--flagfile", default="", 
                  help="Flag file to apply to calibrator data only, if desired. Used to ensure RFI doesn't corrupt FRING or BPASS.")
parser.add_option("-g", "--tarflagfile", default="", 
                  help="Flag file to apply to target data only, if desired. Used to flag any necessary channels for, e.g., RFI or missing data")
parser.add_option("--shadow", nargs=2, default=None,
                  help="Set if flagging due to shadowing is desired. Takes two arguments: arg1 > 0 flag for shadowing; shadow diameter in m. arg2 flag for cross-talk; baseline (BL) in m")
parser.add_option("-p","--phasecenter", default="",
                  help="phase center for the target field (blank will leave it at correlation centre)")
#parser.add_option("-l", "--leakagecorrect", default=False, action="store_true", 
#                  help="Run lpcal to try and correct any leakage present")
parser.add_option("-x", "--xpoldelaymodelfile", default="", help="Model to use for xpol delay correction (blank = no correction)")
parser.add_option("--imagesize", type=int, default=128, help="Size of the image to make")
parser.add_option("--xcorplotsmooth", type=int, default=32, help="Length of the smoothing kernel in channels for xcor plotting")
parser.add_option("--skipplot", default=False, action="store_true",
                  help="Skip the plotting to save time")
parser.add_option("--pixelsize", type=float, default=1, help="Pixel size in arcseconds")
parser.add_option("--uvsrt", default=False, action="store_true", help="Run UVSRT on the data after loading")
parser.add_option("--noisecentre", default="", help="CASA format position at which noise should be estimated, blank=don't make an off-source image")
parser.add_option("--src", default="", help="Name of the target (e.g., FRB or Vela)")
parser.add_option("--pols", type=str, default="XX,YY,I,Q,U,V", help='The polarisations to be imaged if --imagecube is set. Defaulted to all. Input as a list of strings: e.g., "XX,YY"')
(options, junk) = parser.parse_args()
AIPS.userno     = options.userno
refant          = options.refant
imagesize       = options.imagesize
pixelsize       = options.pixelsize
xpolmodelfile   = options.xpoldelaymodelfile
xcorplotsmooth  = options.xcorplotsmooth
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
if xpolmodelfile != '':
    xpol_prefix = '_xpol'
else: xpol_prefix = '_noxpol'
if options.src != '':
    src = '_' + options.src
else: src = options.src
bpfilename = os.path.abspath("bandpasses{0}{1}.bp.txt".format(xpol_prefix, src))
fringsnfilename = os.path.abspath("delays{0}{1}.sn.txt".format(xpol_prefix, src))
selfcalsnfilename = os.path.abspath("selfcal{0}{1}.sn.txt".format(xpol_prefix, src))
xpolsnfilename = os.path.abspath("xpolfring{0}{1}.sn.txt".format(xpol_prefix, src))
bptableplotfilename = os.path.abspath("bptable{0}{1}.ps".format(xpol_prefix, src))
uncalxcorplotfilename = os.path.abspath("uncalxcor{0}{1}.ps".format(xpol_prefix, src))
allcalxcorplotfilename = os.path.abspath("allcalxcor{0}{1}.ps".format(xpol_prefix, src))

# Check if the ms already exists, abort if so
if os.path.exists(targetmsfilename):
    print targetmsfilename, "already exists - aborting here!!!"
    sys.exit()

if os.path.exists(calibratormsfilename):
    print calibratormsfilename, "already exists - aborting here!!!"
    sys.exit()

# If we are running targetonly, then check that all the calibration files exist
if options.targetonly:
    missingfiles = []
    if not os.path.exists(bpfilename):
        missingfiles.append(bpfilename)
    if not os.path.exists(fringsnfilename):
        missingfiles.append(fringsnfilename)
    if not os.path.exists(selfcalsnfilename):
        missingfiles.append(selfcalsnfilename)
    if xpolmodelfile != '' and not os.path.exists(xpolsnfilename):
        missingfiles.append(xpolsnfilename)
    if len(missingfiles) > 0:
        print "Running targetonly but the following files are missing:", missingfiles
        sys.exit()

# Load up the target data if needed
if not options.calibrateonly:
    targetdata = vlbatasks.zapAndCreateUVData("CRAFTTARG", "UVDATA", aipsdisk, 1)
    vlbatasks.fitld_corr(options.target, targetdata, [], '', 0.0001)
    if options.uvsrt:
        sortedtargetdata = vlbatasks.zapAndCreateUVData("CRAFTTARG", "UVSRT", aipsdisk, 1)
        vlbatasks.uvsrt(targetdata, sortedtargetdata)
        targetdata.zap()
        targetdata = sortedtargetdata

    # Get the number of channels in the dataset
    numchannels = vlbatasks.getNumChannels(targetdata)

# Load up the calibrator data
if not options.targetonly:
    caldata = vlbatasks.zapAndCreateUVData("CRAFTCAL","UVDATA", aipsdisk, 1)
    vlbatasks.fitld_corr(options.calibrator, caldata, [], '', 0.0001)
    if options.uvsrt:
        sortedcaldata = vlbatasks.zapAndCreateUVData("CRAFTCAL", "UVSRT", aipsdisk, 1)
        vlbatasks.uvsrt(caldata, sortedcaldata)
        caldata.zap()
        caldata = sortedcaldata
    
    # Get the reference frequency of the dataset
    reffreqs = []
    fqtable = caldata.table('FQ', 1)
    for row in fqtable:
        try:
            for iffreq in row.if_freq:
                freqentry = float(iffreq) + float(caldata.header.crval[2])
                reffreqs.append(float(iffreq) + float(caldata.header.crval[2]))
        except (AttributeError, TypeError):
            freqentry = float(row.if_freq) + float(caldata.header.crval[2])
            reffreqs.append(float(row.if_freq) + float(caldata.header.crval[2]))
    
    # Flag the calibrator data, if desired
    if options.flagfile != "":
        vlbatasks.userflag(caldata, 1, options.flagfile)
    if options.shadow:
        shadowdiameter = float(options.shadow[0])
        xtalkbl = float(options.shadow[1])
        vlbatasks.shadowflag(caldata, 1, shadowdiameter, xtalkbl)
        print "Shadowing diameter: " + str(shadowdiameter)
        print "Cross-talk baseline: " + str(xtalkbl)
    
# Flag the target data, if desired
if not options.calibrateonly:
    if options.tarflagfile != "":
        vlbatasks.userflag(targetdata, 1, options.tarflagfile)
    if options.shadow:
        shadowdiameter = float(options.shadow[0])
        xtalkbl = float(options.shadow[1])
        vlbatasks.shadowflag(targetdata, 1, shadowdiameter, xtalkbl)
        print "Shadowing diameter: " + str(shadowdiameter)
        print "Cross-talk baseline: " + str(xtalkbl)

# Run CLCOR to correct PANG if needed
if xpolmodelfile != "":
    if not options.targetonly:
        vlbatasks.clcor_pang(caldata, clversion)
    if not options.calibrateonly:
        vlbatasks.clcor_pang(targetdata, clversion)
    clversion = clversion + 1

# Run FRING
if not options.targetonly:
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

    # Write SN table to disk
    if os.path.exists(fringsnfilename):
        os.system("rm -f " + fringsnfilename)
    vlbatasks.writetable(caldata, "SN", snversion, fringsnfilename)

# Load FRING SN table into the target
if not options.calibrateonly:
    vlbatasks.loadtable(targetdata, fringsnfilename, snversion)

# Calibrate
if not options.targetonly:
    vlbatasks.applysntable(caldata, snversion, "SELN", clversion, refant)
if not options.calibrateonly:
    vlbatasks.applysntable(targetdata, snversion, "SELN", clversion, refant)
snversion += 1
clversion += 1

# Correct for leakage if needed
#leakagedopol = 0
if xpolmodelfile != "":
    # First the xpoldelays
    if not options.targetonly:
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
    if not options.calibrateonly:
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

# Run bandpass correction - default to CPASS, unless --bpass is specified
scannumber = 1
bpversion = 1
if not options.targetonly:
    if options.bpass:
        vlbatasks.bpass(caldata, options.sourcename, clversion, scannumber)
    else:
        vlbatasks.cpass(caldata, options.sourcename, clversion, scannumber, None, options.cpasspoly)
    
    # Write BP table to disk
    if os.path.exists(bpfilename):
        os.system("rm -f " + bpfilename)
    vlbatasks.writetable(caldata, "BP", bpversion, bpfilename)
    
    # Plot the bandpass table
    if not options.skipplot:
        bptableplotfilename = os.path.abspath("bptable{0}{1}.ps".format(xpol_prefix, src))
        plotsperpage = 4
        plotbptable = True
        vlbatasks.plotbandpass(caldata, bpversion, plotbptable, plotsperpage, bptableplotfilename)

# Load up the bandpass to the target
if not options.calibrateonly:
    vlbatasks.loadtable(targetdata, bpfilename, bpversion)

# Run selfcal
outklass = "SPLIT"
if not options.targetonly:
    applybandpasscal = True
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

    # Write SN table to disk
    if os.path.exists(selfcalsnfilename):
        os.system("rm -f " + selfcalsnfilename)
    vlbatasks.writetable(splitcaldata, "SN", 1, selfcalsnfilename)

# Load up the selfcal SN table
if not options.calibrateonly:
    vlbatasks.loadtable(targetdata, selfcalsnfilename, snversion)
if not options.targetonly:
    vlbatasks.loadtable(caldata, selfcalsnfilename, snversion)

# Calibrate
if not options.targetonly:
    vlbatasks.applysntable(caldata, snversion, "SELN", clversion, refant)
if not options.calibrateonly:
    vlbatasks.applysntable(targetdata, snversion, "SELN", clversion, refant)
snversion += 1
clversion += 1

# Plot the uncalibrated and calibrated cross-correlation results if desired
if not options.targetonly:
    if not options.skipplot:
        uncalxcorplotfilename = os.path.abspath("uncalxcor{0}{1}.ps".format(xpol_prefix, src))
        allcalxcorplotfilename = os.path.abspath("allcalxcor{0}{1}.ps".format(xpol_prefix, src))
        plotbptable = False
        plotsperpage = 4
        ifs = [0,0]
        vlbatasks.plotbandpass(caldata, -1, plotbptable, plotsperpage, uncalxcorplotfilename, 0, ifs, xcorplotsmooth)
        vlbatasks.plotbandpass(caldata, bpversion, plotbptable, plotsperpage, allcalxcorplotfilename, clversion, ifs, xcorplotsmooth)

# Run SPLIT and write output data for calibrator
seqno = 1
if not options.targetonly:
    outputdata = vlbatasks.zapAndCreateUVData("CRAFTSRC","SPLIT",aipsdisk,seqno)
    vlbatasks.splitmulti(caldata, clversion, outklass, options.sourcename, seqno)
    vlbatasks.writedata(outputdata, calibratoroutputfilename, True)

# Run SPLIT and write output data for target
seqno = 1
if not options.calibrateonly:
    outputdata = vlbatasks.zapAndCreateUVData("CRAFTSRC","SPLIT",aipsdisk,seqno)
    vlbatasks.splitmulti(targetdata, clversion, outklass, options.sourcename, seqno)
    vlbatasks.writedata(outputdata, targetoutputfilename, True)

# Create a README file for the calibration and a tarball with it plus all the calibration
if not options.targetonly:
    readmeout = open("README{0}{1}.calibration".format(xpol_prefix, src), "w")
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
    calibtarballfile = "calibration{0}{1}.tar.gz".format(xpol_prefix, src)
    if os.path.exists(calibtarballfile):
        os.system("rm -f " + calibtarballfile)
    os.system("tar cvzf {0} README{1}{2}.calibration {3}".format(calibtarballfile, xpol_prefix, src, tarinputfiles))


# Convert to a measurement set
if not options.targetonly:
    casaout = open("loadtarget.py","w")
    casaout.write("importuvfits(fitsfile='%s',vis='%s',antnamescheme='old')\n" % (calibratoroutputfilename, calibratormsfilename))
    casaout.close()
    os.system("runloadtarget.sh")

if not options.calibrateonly:
    casaout = open("loadtarget.py","w")
    casaout.write("importuvfits(fitsfile='%s',vis='%s',antnamescheme='old')\n" % (targetoutputfilename, targetmsfilename))
    casaout.close()
    os.system("runloadtarget.sh")


# Run the imaging via CASA if desired
if not options.calibrateonly:
    polarisations = options.pols.split(',')
    if options.dirtyonly:
        polarisations = ["I"]
    if options.imagecube:
        # Do the cube
        for pol in polarisations:
            casaout = open("imagescript.py","w")
            imagename = "TARGET.cube.{0}".format(pol)
            offsourcename = "OFFSOURCE.cube.{0}".format(pol)
            maskstr = "'circle[[{0}pix,{0}pix] ,5pix ]'".format(imagesize/2)
            phasecenter = "'{0}'".format(options.phasecenter).encode()
            imsize = "[{0},{0}]".format(imagesize)

            # If desired, produce the noise image
            if options.noisecentre:
                rmscenter = '{0}'.format(options.noisecentre)
                rmsimsize = "[{0},{0}]".format(imagesize*4)
                os.system("rm -rf {0}*".format(offsourcename))
                os.system("rm -rf {0}*".format(imagename))
                outlierfile = open("outlierfield_Stokes{0}.txt".format(pol), "w")
                outlierfile.write("imagename={0}\nimsize={1}\nphasecenter={2}\nmask="'circle[[{3}pix,{3}pix] ,{3}pix ]'"\n".format(offsourcename, rmsimsize, rmscenter, imagesize*2))
                outlierfile.close()
                outlierfields = "'outlierfield_Stokes{0}.txt'".format(pol)
            else:
                outlierfields = '[]'
                os.system("rm -rf {0}.*".format(imagename))

            # If desired, produce the only the dirty image
            if options.dirtyonly:
                imname = "TARGET.cube.dirim.{0}".format(pol)
                os.system("rm -rf {0}.*".format(imname))
                casaout.write("tclean(vis='{0}', imagename='{1}', imsize={2}, cell=['1arcsec', '1arcsec'], stokes='{3}', specmode='cube', width={4}, phasecenter={5}, gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=0, mask={6}, outlierfile={7})".format(targetmsfilename, imagename, imsize, pol, options.averagechannels, phasecenter, maskstr, outlierfields))

            # Default: produce a cleaned image
            else:
                casaout.write("tclean(vis='{0}', imagename='{1}', imsize={2}, cell=['1arcsec', '1arcsec'], stokes='{3}', specmode='cube', width={4}, gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=100, cycleniter=100, mask={5}, savemodel='modelcolumn', phasecenter={6}, outlierfile={7})".format(targetmsfilename, imagename, imsize, pol, options.averagechannels, maskstr, phasecenter, outlierfields))

            casaout.close()
            os.system("chmod 775 imagescript.py")
            os.system("runimagescript.sh")
    
            # If desired, also make the JMFIT output
            if options.imagejmfit:
                casaout = open("imagescript.py","w")
                casaout.write('exportfits(imagename="%s.image",fitsimage="%s.fits")\n' % (imagebase, imagebase))
                casaout.close()
                os.system("casa --nologger -c imagescript.py")
                for i in range(numchannels/options.averagechannels):
                    locstring = "%d,%d,%d,%d,%d,%d" % (imagesize/2-12, imagesize/2-12, i, imagesize/2+12, imagesize/2+12, i)
                    os.system("jmfitfromfile.py %s.fits %s.slice%03d.jmfit.stats %s" % (imagebase, imagebase, i, locstring))
