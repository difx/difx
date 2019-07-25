#!/usr/bin/env python
import os,sys,argparse

parser = argparse.ArgumentParser()
parser.add_argument("--rfisub", help="Do RFI subtraction", action="store_true")
parser.add_argument("-t", "--targfile", nargs='+', type=str, default="", help="The gated target FITS file in need of RFI subtraction")
parser.add_argument("-r", "--rfifile", type=str, default="", help="The RFI gated FITS file with which the RFI subtraction is performed")
parser.add_argument("-s", "--scalefact", type=float, default=None, help="The scale factor by which to multiply the RFI file before it's subtracted from the target file: the ratio of the two effective integration times (target int time/RFI int time)")
parser.add_argument("-o", "--outfile", type=str, default="", help="The output, RFI subtracted FITS file prefix (e.g. FRB_SUB)")
parser.add_argument("--cal", help="Calibrate the data", action="store_true")
parser.add_argument("-c", "--caldata", type=str, default="", help="The calibrator FITS file data (note: must be uncalibrated")
parser.add_argument("-d", "--targdata", nargs='+', type=str, default="", help="The target FITS file data to be calibrated; if --rfisub is set, this will default to the output FITS file from the RFI subtraction (e.g. FRB_SUB)")
parser.add_argument("--calflag", type=str, default="", help="Text file containing the AIPS flags to be applied to the calibrator data")
parser.add_argument("--targflag", type=str, default="", help="Text file containing the AIPS flags to be applied to the target data")
parser.add_argument("-a", "--avg", type=int, default=24, help="Number of channels to average together per image cube slice")
parser.add_argument("-p","--phasecenter", type=str, default="", help="Phase centre for the target field (blank will leave it at correlation centre); format example: 'J2000 21h49m24s -52d58m15s' ")
parser.add_argument("-n","--nbins", type=int, default=1, help="Number of bins/target files to RFI subtract or calibrate")
parser.add_argument("-N","--noisecentre", default="", help="CASA format position at which noise should be estimated, blank=don't make an off-source image. Example: --noisecentre='J2000 21h33m23s -54d34m11s' ")
parser.add_argument("-x", "--xpol", help="Do xpol correction during calibration", action="store_true")
parser.add_argument("-i", "--imagecube", default=False, help="Do imaging", action="store_true")
parser.add_argument("--refant", type=int, default=3, help="Reference antenna")
parser.add_argument("-P", "--prefix", type=str, default = "", help="Prefix for bin directory")
parser.add_argument("--src", default="", help="Name of the target (e.g., FRB or Vela)")
parser.add_argument("--dirim", help="Make zero iteration dirty image with CASA tclean", action="store_true")
parser.add_argument("--image", help="Run CASA cleaning post calibration to make full Stokes images", action="store_true")

args = parser.parse_args()

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

nbins = args.nbins
tdata = []
targfile = []
prefix=args.prefix
src = args.src

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
    outfile = []
    for i in range(nbins):
        if nbins>1:
            outfile.append(args.outfile.replace('.FITS', ''))
        else: outfile.append(args.outfile.replace('.FITS', ''))


if args.cal:
    if args.caldata is '':
        parser.error("You must specify a calibrator dataset")
    else:
        cdata = args.caldata
        cdatms = cdata.replace('.FITS', '').split('/')[-1]
    if args.targdata is '':
        if args.rfisub:
            tdata = outfile
        else: parser.error("You must specify a target dataset to calibrate")
    else:
        for i in range(nbins):
            if nbins>1:
                tdata.append(args.targdata[i].replace('.FITS', ''))
            else: tdata.append(args.targdata[i].replace('.FITS', ''))


if args.calflag is '': cflag = ''
else: cflag = '--flagfile='+args.calflag

if args.targflag is '': tflag = ''
else: tflag = '--tarflagfile='+args.targflag

avgchan = args.avg

if args.phasecenter is '': pcen = ''
else: pcen = '-p '+args.phasecenter


for i in range(nbins):
    print i

    if nbins > 1:
        bins = "_B{0:02g}".format(i)
    else: bins = ''
    
    if args.rfisub:
        print "~/packages/src/psrvlbireduce/datareduction/uvsubScaled.py {0}.FITS {1} {2} {3}{4}.FITS".format(targfile[i], rfi, scale, outfile[i], bins)
        os.system("~/packages/src/psrvlbireduce/datareduction/uvsubScaled.py {0}.FITS {1} {2} {3}{4}.FITS".format(targfile[i], rfi, scale, outfile[i], bins))
    
    if args.cal:
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

        print "~/packages/src/difx/sites/ASKAP/calibrateFRB.py -c {0} -t {1}{2}.FITS {3} {4} -a {5} {6} --uvsrt {7} {8} {9} {10} --src={11}".format(cdata, tdata[i], bins, cflag, tflag, avgchan, pcen, xpolmodel, noisecen, doimage, refant, src)
        os.system("rm -rf {0}_calibrated_uv.ms".format(cdatms))
        os.system("~/packages/src/difx/sites/ASKAP/calibrateFRB.py -c {0} -t {1}{2}.FITS {3} {4} -a {5} {6} --uvsrt {7} {8} {9} {10} --src={11} --skipplot".format(cdata, tdata[i], bins, cflag, tflag, avgchan, pcen, xpolmodel, noisecen, doimage, refant, src))
        os.system("mkdir {0}bin{1:02g}".format(prefix, i))
        os.system("mv TARGET* {0}bin{1:02g}".format(prefix, i))
        if noisecen != '':
            os.system("mv OFFSOURCE* {0}bin{1:02g}".format(prefix, i))
        os.system("mv {0}{1}_calibrated_uv.ms {2}bin{3:02g}".format(tdata[i], bins, prefix, i))
        os.system("mv casa*.log {0}bin{1:02g}".format(prefix, i))


        if args.dirim:
        # Write out a CASA mfs tclean script and run; used for searching for FRB in a larger region

            casaout = open("dirtyimscript.py","w")
            imagename = "TARGET.SEARCH.B{0}I".format(i)
            vis = "{0}_calibrated_uv.ms".format(tdata[i])

            casaout.write("tclean(vis='{0}', imagename='{1}', imsize=[512,512], cell=['3arcsec', '3arcsec'], stokes='I', specmode='mfs', gridder='widefield', wprojplanes=-1, pblimit=-1, deconvolver='multiscale', weighting='natural', niter=0)".format(vis, imagename))

            casaout.close()

            runclean = open("runclean.sh", "w")
            runclean.write("#!/bin/bash \n")
            runclean.write("# Script to run the CASA call for making a dirty image while coarse searching for the FRB phase range \n")
            runclean.write("module purge \n")
            runclean.write("module load casa/5.3.0 \n")
            runclean.write("casa --nologger -c dirtyimscript.py")
            runclean.close()

            os.system("chmod 775 runclean.sh")

            os.system("runclean.sh")

            os.system("mv {0}* {1}bin{2:02g}".format(imagename, prefix, i))

    if args.image:
        bins = "_B{0:02g}".format(i)
        # Run the imaging via CASA if desired
        polarisations = ["XX","YY","I","Q","U","V"]
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
                casaout.write('clean(vis="%s",imagename=%s,outlierfile="",field="",spw="",selectdata=True,timerange="",uvrange="",mode="channel",gridmode="widefield",wprojplanes=-1,niter=0,gain=0.1,threshold="0.0mJy",psfmode="clark",imagermode="csclean",multiscale=[],interactive=False,mask=%s,nchan=-1,start=1,width=%d,outframe="",veltype="radio",imsize=%s,cell=%s,phasecenter=%s,restfreq="",stokes="%s",weighting="natural",robust=0.0,uvtaper=False,pbcor=False,minpb=0.2,usescratch=False,noise="1.0Jy",npixels=0,npercycle=100,cyclefactor=1.5,cyclespeedup=-1,nterms=1,reffreq="",chaniter=False,flatnoise=True,allowchunk=False)\n' % (targetmsfilename, imagebases, maskstr2, avgchan, imsizestr, cellstr, phasecenterstr, pol))
            
            casaout.close()
            os.system("casa --nologger -c imagescript.py")
            os.system("mv TARGET* {0}bin{1:02g}".format(prefix, i))
            if args.noisecentre != '':
                os.system("mv OFFSOURCE* {0}bin{1:02g}".format(prefix, i))
            os.system("mv casa*.log {0}bin{1:02g}".format(prefix, i))
