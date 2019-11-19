#!/usr/bin/env python
import os, sys, argparse

parser = argparse.ArgumentParser()
parser.add_argument("--rfisub", help="Do RFI subtraction", action="store_true")
parser.add_argument("-t", "--targfile", nargs='+', type=str, default="", help="The gated target FITS file in need of RFI subtraction")
parser.add_argument("-r", "--rfifile", type=str, default="", help="The RFI gated FITS file with which the RFI subtraction is performed")
parser.add_argument("-s", "--scalefact", type=float, default=None, help="The scale factor by which to multiply the RFI file before it's subtracted from the target file: the ratio of the two effective integration times (target int time/RFI int time)")
parser.add_argument("-o", "--outfile", type=str, default="", help="The output, RFI subtracted FITS file prefix (e.g. FRB_SUB)")
parser.add_argument("--cal", help="Calibrate the data", action="store_true")
parser.add_argument("-c", "--caldata", type=str, default="", help="The calibrator FITS file data (note: must be uncalibrated")
parser.add_argument("-d", "--targdata", type=str, default="", help="The target FITS file data to be calibrated; if --rfisub is set, this will default to the output FITS file from the RFI subtraction (e.g. FRB_SUB)")
parser.add_argument("--calflag", type=str, default="", help="Text file containing the AIPS flags to be applied to the calibrator data")
parser.add_argument("--targflag", type=str, default="", help="Text file containing the AIPS flags to be applied to the target data")
parser.add_argument("-a", "--avg", type=int, default=24, help="Number of channels to average together per image cube slice")
parser.add_argument("-p","--phasecenter", type=str, default="", help="Phase centre for the target field (blank will leave it at correlation centre); format example: 'J2000 21h49m24s -52d58m15s' ")
parser.add_argument("-n","--nbins", type=int, default=1, help="Number of bins/target files to RFI subtract or calibrate")
parser.add_argument("--binnum", type=str, default='', help="The bin number of the file to RFI subtract or calibrate")
parser.add_argument("-N","--noisecentre", default="", help="CASA format position at which noise should be estimated, blank=don't make an off-source image. Example: --noisecentre='J2000 21h33m23s -54d34m11s' ")
parser.add_argument("-x", "--xpol", help="Do xpol correction during calibration", action="store_true")
parser.add_argument("-i", "--imagecube", default=False, help="Do imaging", action="store_true")
parser.add_argument("--refant", type=int, default=3, help="Reference antenna")
parser.add_argument("-P", "--prefix", type=str, default = "", help="Prefix for bin directory")
parser.add_argument("--src", default="", help="Name of the target (e.g., FRB or Vela)")
parser.add_argument("--dirim", help="Make zero iteration dirty image with CASA tclean", action="store_true")
parser.add_argument("--image", help="Run CASA cleaning post calibration to make full Stokes images", action="store_true")
parser.add_argument("--cpasspoly", default=10, type=int, help="Number of polynomial terms in CPASS")
parser.add_argument("--ast", help="Select if this is being used for bulk astrometry data calibration", action="store_true")
parser.add_argument("--atar", default="", nargs='+', type=str, help="If using the --ast argument, this sets the target object's file name")
parser.add_argument("--acal", default="", type=str, help="If using the --ast argument, this sets the calibrator's file name")
parser.add_argument("--atarname", default="", nargs='+', type=str, help="If using the --ast argument, this sets the target identifier (e.g., the scanID)")
parser.add_argument("--acalname", default="", type=str, help="If using the --ast argument, this sets the calibrator identifier (e.g., the scanID)")
parser.add_argument("--bpass", default=False, action="store_true", help="Use BPASS rather than CPASS to do the bandpass correction")
parser.add_argument("--calibrateonly", default=False, action="store_true",help="Only generate the calibration files, don't do anything with target")
parser.add_argument("--targetonly", default=False, action="store_true",help="Use saved calibration files; note that this uses whatever calibrator data is in the directory!")
parser.add_argument("--pols", type=str, default="XX,YY,I,Q,U,V", help='The polarisations to be imaged if --imagecube is set. Defaulted to all. Input as a list of strings: e.g., "XX,YY"')
parser.add_argument("--imagesize", type=int, default=128, help="Size of the image to make")
parser.add_argument("--jmfit", default=False, action="store_true", help="Run JMFIT on the astrometry source image")
parser.add_argument("-f", "--flux", type=float, default=9.5, # 0407 flux
                    help="Calibrator flux in Jy,  Defaulted to correct value for 0407")
parser.add_argument("--msprefix", type=str, default = "", help="Prefix for the MS file; to be used when just doing the imaging")
parser.add_argument("--pixelsize", type=float, default=1, help="Pixel size in arcseconds")
parser.add_argument("--test", default=False, action="store_true", help="Testing lines of code")

args = parser.parse_args()

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

nbins = args.nbins
if args.binnum != '':
    binnum = '_B{0:02g}'.format(args.binnum)
else: binnum = ''
imagesize = args.imagesize
flux = args.flux
targfile = []
prefix=args.prefix
acalname = args.acalname
atarname = args.atarname
msprefix = args.msprefix

if args.src != '':
    src = "--src={0}".format(args.src)
else: src=''

if args.rfisub:
    if args.targfile is '':
        parser.error("You must specify the target gated data FITS file")
    else:
        for i in range(nbins):
            if nbins>1:
                targfile.append(args.targfile[i].replace('.FITS', ''))
            else: targfile.append(args.targfile[i].replace('.FITS', ''))
    if args.rfifile is '':
        parser.error("You must specify the RFI gated data FITS file")
    else: rfi = args.rfifile
    if args.scalefact is None:
        parser.error("You must specify a scale factor for the RFI gated data")
    else: scale = args.scalefact
    if args.outfile is '':
        parser.error("You must specify the output RFI subtracted FITS file name")
    else:
        outfile = args.outfile.replace('.FITS', '')
        print(outfile)

if args.cal:
    if not args.targetonly:
        if args.caldata is '':
            parser.error("You must specify a calibrator dataset")
        else:
            cdata = args.caldata
            cdatms = cdata.replace('.FITS', '').split('/')[-1]
    else:
        cdata = ''
        cdatms = ''
    if args.targdata is '':
        if args.rfisub:
            tdata = outfile
        else: parser.error("You must specify a target dataset to calibrate")
    else:
        tdata = args.targdata.replace('.FITS', '')
        print("Target data basename: ",tdata)

if args.ast:
    if args.atar is '' and args.targetonly:
        parser.error("You must specify the target's name!")
    else:
        if not args.calibrateonly:
            asttar = []
            for i in range(nbins):
                asttar.append(args.atar[i])
    if args.acal is '' and args.calibrateonly:
        parser.error("You must specify the calibrator's name!")
    else:
        if not args.targetonly:
            astcal = args.acal
    if args.atarname is '' and args.targetonly:
        parser.error("You must specify the target's identifier!")
    if args.acalname is '' and args.targetonly:
        parser.error("You must specify the calibrator's identifier!")

if args.calflag is '': cflag = ''
else: cflag = '--flagfile='+args.calflag

if args.targflag is '': tflag = ''
else: tflag = '--tarflagfile='+args.targflag

cpasspoly = args.cpasspoly
avgchan = args.avg

if args.bpass:
    bpass = '--bpass'
else: bpass = ''

if args.phasecenter is '': pcen = ''
else: pcen = '-p '+args.phasecenter


for i in range(nbins):
#    print i

    if nbins > 1:
        bins = "_B{0:02g}".format(i)
    else: bins = ''
    
    if args.rfisub:
        print "~/packages/src/psrvlbireduce/datareduction/uvsubScaled.py {0}.FITS {1} {2} {3}{4}.FITS".format(targfile[i], rfi, scale, outfile, bins)
        os.system("~/packages/src/psrvlbireduce/datareduction/uvsubScaled.py {0}.FITS {1} {2} {3}{4}.FITS".format(targfile[i], rfi, scale, outfile, bins))
    
    if args.cal:

        polarisations = args.pols

        if args.xpol:
            xpolmodel = "-x /fred/oz002/askap/craft/0407.model.fits"
        else: xpolmodel = ''
        if args.noisecentre != '':
            noisecen = "--noisecentre '" + args.noisecentre + "'"
        else: noisecen=''

        if args.imagecube:
            doimage = '-i'
        else: doimage = ''

        if args.refant != "":
            refant = "--refant=" + str(args.refant)
        else: refant = ""

        if args.calibrateonly:
            calonly = "--calibrateonly"
            skipploting = ''
        else:
            calonly = ''
            skipploting = ''

        if args.targetonly:
            taronly = "--targetonly"
        else: taronly = ""

        if not args.targetonly and not args.calibrateonly:
            skipploting = ''

        if cdata is not '':
            cdata_op = "-c"+cdata
        else: cdata_op = ''

        print "~/packages/src/difx/sites/ASKAP/calibrateFRB.py {0} -t {1}{2}.FITS {3} {4} -a {5} {6} --uvsrt {7} {8} {9} {10} {11} --cpasspoly={12} --pols={13} -f {14} {15} {16} {17}".format(cdata_op, tdata, bins, cflag, tflag, avgchan, pcen, xpolmodel, noisecen, doimage, refant, src, cpasspoly, polarisations, flux, calonly, taronly, skipploting)
        os.system("rm -rf {0}_calibrated_uv.ms".format(cdatms))
        os.system("~/packages/src/difx/sites/ASKAP/calibrateFRB.py {0} -t {1}{2}.FITS {3} {4} -a {5} {6} --uvsrt {7} {8} {9} {10} {11} --cpasspoly={12} --pols={13} -f {14} {15} {16} {17}".format(cdata_op, tdata, bins, cflag, tflag, avgchan, pcen, xpolmodel, noisecen, doimage, refant, src, cpasspoly, polarisations, flux, calonly, taronly, skipploting))
        os.system("mkdir {0}bin{1:02g}".format(prefix, i))
        if doimage != '':
            os.system("mv TARGET* {0}bin{1:02g}".format(prefix, i))
        if noisecen != '':
            os.system("mv OFFSOURCE* {0}bin{1:02g}".format(prefix, i))
        os.system("mv {0}{1}_calibrated_uv.ms {2}bin{3:02g}".format(tdata, bins, prefix, i))
        os.system("mv casa*.log {0}bin{1:02g}".format(prefix, i))


        if args.dirim:
        # Write out a CASA mfs tclean script and run; used for making dirty images of a source

            casaout = open("{0}bin{1:02g}/dirtyimscript.py".format(prefix,i),"w")
            imagename = "{0}bin{1:02g}/TARGET-{2}.B{1:02g}_I".format(prefix,i,src)
            vis = "{0}bin{1:02g}/{2}{3}_calibrated_uv.ms".format(prefix, i, tdata, bins)

            casaout.write("tclean(vis='{0}', imagename='{1}', imsize=[512,512], cell=['3arcsec', '3arcsec'], stokes='I', specmode='mfs', gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=0)".format(vis, imagename))

            casaout.close()

            runclean = open("{0}bin{1:02g}/runclean.sh".format(prefix, i), "w")
            runclean.write("#!/bin/bash \n")
            runclean.write("# Script to run the CASA call for making a dirty image \n")
            runclean.write("module purge \n")
            runclean.write("module load casa/5.5.0 \n")
            runclean.write("casa --nologger -c dirtyimscript.py")
            runclean.close()

            os.system("chmod 775 {0}bin{1:02g}/runclean.sh".format(prefix, i))

            os.system("{0}bin{1:02g}/runclean.sh".format(prefix, i))

#            os.system("mv {0}* {1}bin{2:02g}".format(imagename, prefix, i))


    if args.image:
        bins = "_B{0:02g}".format(i)
        # Run the imaging via CASA if desired
        polarisations = args.pols.split(',')
        # Do the cube
        for pol in polarisations:
            casaout = open("imagescript.py","w")
            imagebase = "TARGET.cube.%s" % (pol)
            os.system("rm -rf %s.*" % imagebase)
            imagename = imagebase + ".image"
            imagesize = 128
            pixelsize = 1
            targetmsfilename= "bin{0:02g}/FRB_HTR_SUB{1}_calibrated_uv.ms".format(i, bins)
            maskstr = "'circle [[%dpix,%dpix] ,5pix ]'" % (imagesize/2,imagesize/2)
#            casaout.write('clean(vis="%s",imagename="%s",outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=100,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask=%s,nchan=-1,start=1,width=%d,outframe="",veltype="radio",imsize=%s,cell=["%.2farcsec", "%.2farcsec"],phasecenter="%s",restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (targetmsfilename, imagebase, maskstr, avgchan, imagesize, pixelsize, pixelsize, args.phasecenter, pol))

            # If desired, produce the noise image as well
            if len(args.noisecentre) > 1:
                offsourcebase = "OFFSOURCE.cube.%s" % (pol)
                imagebases = '["%s","%s"]' % (imagebase,offsourcebase)
                os.system("rm -rf %s.*" % offsourcebase)
                maskstr2 = "[%s,'']" % (maskstr)
                imsizestr = "[[%d,%d],[%d,%d]]" % (imagesize, imagesize, imagesize*4, imagesize*4)
                cellstr = '["%.2farcsec", "%.2farcsec"]' % (pixelsize, pixelsize)
                phasecenterstr = "['', '%s']" % (args.noisecentre)
#                casaout.write('clean(vis="%s",imagename=%s,outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=0,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask=%s,nchan=-1,start=1,width=%d,outframe="",veltype="radio",imsize=%s,cell=%s,phasecenter=%s,restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (targetmsfilename, imagebases, maskstr2, avgchan, imsizestr, cellstr, phasecenterstr, pol))
            
#            casaout.close()
#            os.system("casa --nologger -c imagescript.py")
#            os.system("mv TARGET* {0}bin{1:02g}".format(prefix, i))
            if args.noisecentre != '':
                os.system("mv OFFSOURCE* {0}bin{1:02g}".format(prefix, i))
            os.system("mv casa*.log {0}bin{1:02g}".format(prefix, i))

if args.ast:

    if args.xpol:
        xpolmodel = "-x /fred/oz002/askap/craft/0407.model.fits"
    else: xpolmodel = ''

    if args.refant != "":
        refant = "--refant=" + str(args.refant)
    else: refant = ""

    calonly = "--calibrateonly"
    taronly = "--targetonly"
    cal = args.acal.split('/')[-1][:-5]+"_calibrated_uv.fits"

    if bpass is '':
        cpasspoly = '--cpasspoly={0}'.format(args.cpasspoly)
    else: cpasspoly = ''

    if args.calibrateonly:
        print "~/packages/src/difx/sites/ASKAP/calibrateFRB.py -c {0} {1} -a {2} {3} --uvsrt {4} {5} {6} {7} {8} {9} -f {10}".format(astcal, cflag, avgchan, pcen, xpolmodel, refant, src, cpasspoly, bpass, calonly, flux)
        os.system("~/packages/src/difx/sites/ASKAP/calibrateFRB.py -c {0} {1} -a {2} {3} --uvsrt {4} {5} {6} {7} {8} {9} -f {10}".format(astcal, cflag, avgchan, pcen, xpolmodel, refant, src, cpasspoly, bpass, calonly, flux))
        os.system("mv {0} {1}_cal{2}.FITS".format(cal,src[6:],acalname))
        os.system("mv {0} {1}_cal{2}.ms".format(cal.replace('fits', 'ms'),src[6:],acalname))

    if args.targetonly:
        for b in range(nbins):
            asttar_new = asttar[b]
            tar_cald = asttar_new.split('/')[-1][:-5]+"_calibrated_uv.fits"
            tar_cald_ms = tar_cald.replace('.fits', '.ms')
            print "~/packages/src/difx/sites/ASKAP/calibrateFRB.py -t {0} {1} -a {2} {3} --uvsrt {4} {5} {6} {7} {8} {9}".format(asttar_new, tflag, avgchan, pcen, xpolmodel, refant, src, cpasspoly, bpass, taronly)
            os.system("~/packages/src/difx/sites/ASKAP/calibrateFRB.py -t {0} {1} -a {2} {3} --uvsrt {4} {5} {6} {7} {8} {9}".format(asttar_new, tflag, avgchan, pcen, xpolmodel, refant, src, cpasspoly, bpass, taronly))
            os.system("mv {0} {1}_{2}_cal{3}".format(asttar_new,src,atarname,acalname))
            print "mv {0} {1}_tar{2}_cal{3}.FITS".format(tar_cald,src[6:],atarname[b],acalname)
            print "mv {0} {1}_tar{2}_cal{3}.ms".format(tar_cald_ms,src[6:],atarname[b],acalname)

            os.system("mkdir {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname))
            print "mkdir {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname)
            os.system("mv {0}_tar{1}_cal{2}.FITS {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname))
            print "mv {0}_tar{1}_cal{2}.FITS {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname)
            os.system("mv {0} {1}_tar{2}cal{3}".format(cal,src[6:],atarname[b],acalname))
            print "mv {0}_cal{2}.FITS {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname)
            os.system("mv casa*.log {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname))
            print "mv casa*.log {0}_tar{1}cal{2}".format(src[6:],atarname[b],acalname)

            # Write out and run a CASA mfs tclean script for astrometry data; used to make cleaned image of the target source
            casaout = open("{0}_tar{1}cal{2}/targetimage.py".format(src[6:],atarname[b],acalname), "w")
            imagename = "{0}_tar{1}cal{2}/{0}T{1}_C{2}_mfs_I".format(src[6:],atarname[b],acalname)
            vis = "{0}_tar{1}cal{2}/{0}_tar{1}_cal{2}.ms".format(src[6:],atarname[b],acalname)
            maskstr = "'circle [[%dpix,%dpix] ,5pix ]'" % (imagesize/2,imagesize/2)

            casaout.write("tclean(vis='{0}', imagename='{1}', imsize=[{2}], cell=['1arcsec', '1arcsec'], stokes='I', specmode='mfs', gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=100, cycleniter=100, mask={3}, savemodel='modelcolumn')".format(vis, imagename, imagesize, maskstr))
            casaout.close()

            runclean = open("{0}_tar{1}cal{2}/runasttclean.sh".format(src[6:],atarname[b],acalname), "w")
            runclean.write("#!/bin/bash \n")
            runclean.write("# Script to run the CASA call for making the astrometry source image \n")
            runclean.write("module purge \n")
            runclean.write("module load casa/5.5.0 \n")
            runclean.write("casa --nologger -c {0}_tar{1}cal{2}/targetimage.py".format(src[6:],atarname[b],acalname))
            runclean.close()

            os.system("chmod 775 {0}_tar{1}cal{2}/runasttclean.sh".format(src[6:],atarname[b],acalname))
            os.system("{0}_tar{1}cal{2}/runasttclean.sh".format(src[6:],atarname[b],acalname))

            # If fitting the source position is desired, the following runs JMFIT and outputs a stats file
            if args.jmfit:

                casaout = open("{0}_tar{1}cal{2}/jmfitscript.py".format(src[6:],atarname[b],acalname),"w")
                casaout.write('exportfits(imagename="{0}.image",fitsimage="{0}.fits")\n' % (imagename))
                casaout.close()
                runconvert = open("{0}_tar{1}cal{2}/convert2fits.sh".format(src[6:],atarname[b],acalname),"w")
                runconvert.write("#!/bin/bash \n")
                runconvert.write("# Script to run CASA exportfits on the astrometry image \n")
                runconvert.write("module purge \n")
                runconvert.write("module load casa/5.5.0 \n")
                runconvert.write("casa --nologger -c {0}_tar{1}cal{2}/jmfitscript.py".format(src[6:],atarname[b],acalname))
                runconvert.close()

                os.system("chmod 775 {0}_tar{1}cal{2}/convert2fits.sh".format(src[6:],atarname[b],acalname))
                os.system("{0}_tar{1}cal{2}/convert2fits.sh".format(src[6:],atarname[b],acalname))

                locstring = "{0},{1},{2},{3}".format(imagesize/2-12, imagesize/2-12, imagesize/2+12, imagesize/2+12,)
                os.system("jmfitfromfile.py {0}_tar{1}cal{2}/{3}.fits {0}_tar{1}cal{2}/{3}.jmfit.stats {4}".format(src[6:],atarname[b],acalname,imagename,locstring))

if not args.cal and not args.ast:
    if args.imagecube:
        # Run just the cube imaging
        for i in range(nbins):
            bins = "_B{0:02g}".format(i)
            polarisations = args.pols.split(',')
            for pol in polarisations:
                print pol
                casaout = open("{0}bin{1:02g}/imagescript.py".format(prefix,i,pol), "w")
                imagename = "{0}bin{1:02g}/TARGET.cube.{2}".format(prefix,i,pol)
                offsourcename = "{0}bin{1:02g}/OFFSOURCE.cube.{2}".format(prefix,i,pol)
                vis = "{0}bin{1:02g}/{2}{3}_calibrated_uv.ms".format(prefix,i,msprefix,bins)
                maskstr = "'circle[[{0}pix,{0}pix] ,5pix ]'".format(imagesize/2)
                phasecenter = "''"
                imsize = "[{0},{0}]".format(imagesize)
                if args.noisecentre:
                    rmscenter = '{0}'.format(args.noisecentre)
                    rmsimsize = "[{0},{0}]".format(imagesize*4)
                    os.system("rm -rf {0}*".format(offsourcename))
                    os.system("rm -rf {0}*".format(imagename))
                    outlierfile = open("{0}bin{1:02g}/outlierfield.txt".format(prefix,i,pol), "w")
                    outlierfile.write("imagename={0}\nimsize={1}\nphasecenter={2}\nmask="'circle[[{3}pix,{3}pix] ,{3}pix ]'"\n".format(offsourcename, rmsimsize, rmscenter, imagesize*2))
                    outlierfile.close()
                    outlierfields = "'{0}bin{1:02g}/outlierfield.txt'".format(prefix,i,pol)
                else:
                    outlierfile = ''
                    os.system("rm -rf {0}.*".format(imagename))
                if args.dirim:
                    imname = "{0}bin{1:02g}/TARGET.cube.dirim.{2}".format(prefix,i,pol)
                    os.system("rm -rf {0}.*".format(imname))
#                    casaout.write("tclean(vis='{0}', imagename='{1}', imsize={2}, cell=['1arcsec', '1arcsec'], stokes='{3}', phasecenter={4}, specmode='mfs', gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=0)".format(vis, imagename, imsize, pol, phasecenter))
                    casaout.write("tclean(vis='{0}', imagename='{1}', imsize={2}, cell=['1arcsec', '1arcsec'], stokes='{3}', specmode='cube', width={4}, phasecenter={5}, gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=0, mask={6}, outlierfile={7})".format(vis, imagename, imsize, pol, args.avg, phasecenter, maskstr, outlierfields))
                else:
                    casaout.write("tclean(vis='{0}', imagename='{1}', imsize={2}, cell=['1arcsec', '1arcsec'], stokes='{3}', specmode='cube', width={4}, gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=100, cycleniter=100, mask={5}, savemodel='modelcolumn', phasecenter={6}, outlierfile={7})".format(vis, imagename, imsize, pol, args.avg, maskstr, phasecenter, outlierfields))
                casaout.close()
                os.system("chmod 775 {0}bin{1:02g}/imagescript.py".format(prefix,i,pol))

                runcasa = open("{0}bin{1:02g}/runimagescript.sh".format(prefix,i), "w")
                runcasa.write("#!/bin/bash \n")
                runcasa.write("# Script to run CASA cleaning for each Stokes image \n")
                runcasa.write("module purge \n")
                runcasa.write("module load casa/5.5.0 \n")
                runcasa.write("casa --nologger -c {0}bin{1:02g}/imagescript.py".format(prefix,i,pol))
                runcasa.close()

                os.system("chmod 775 {0}bin{1:02g}/runimagescript.sh".format(prefix,i,pol))
                os.system("{0}bin{1:02g}/runimagescript.sh".format(prefix,i))
                os.system("mv casa*.log {0}bin{1:02g}".format(prefix,i))

if args.test:
    print [pol for pol in args.pols.split(',')]
