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


args = parser.parse_args()

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

nbins = args.nbins
tdata = []
targfile = []
prefix=args.prefix

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

    if nbins > 1:
        bins = "_B{0:02g}".format(i)
    else: bins = ''
    
    if args.rfisub:
        print "~/packages/src/difx/sites/ASKAP/uvsubScaled.py {0}.FITS {1} {2} {3}{4}.FITS".format(targfile[i], rfi, scale, outfile[i], bins)
        os.system("~/packages/src/difx/sites/ASKAP/uvsubScaled.py {0}.FITS {1} {2} {3}{4}.FITS".format(targfile[i], rfi, scale, outfile[i], bins))
    
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
        
        print "~/packages/src/difx/sites/ASKAP/calibrateFRB.py -c {0} -t {1}.FITS {2} {3} -a {4} {5} --uvsrt {6} {7} {8} {9}".format(cdata, tdata[i], cflag, tflag, avgchan, pcen, xpolmodel, noisecen, doimage, refant)
        os.system("rm -rf {0}_calibrated_uv.ms".format(cdatms))
        os.system("~/packages/src/difx/sites/ASKAP/calibrateFRB.py -c {0} -t {1}.FITS {2} {3} -a {4} {5} --uvsrt {6} {7} {8} {9}".format(cdata, tdata[i], cflag, tflag, avgchan, pcen, xpolmodel, noisecen, doimage, refant))
        os.system("mkdir {0}bin{1:02g}".format(prefix, i))
        os.system("mv TARGET* {0}bin{1:02g}".format(prefix, i))
        if noisecen != '':
            os.system("mv OFFSOURCE* {0}bin{1:02g}".format(prefix, i))
        os.system("mv {0}_calibrated_uv.ms {1}bin{2:02g}".format(tdata[i], prefix, i))
        os.system("mv casa*.log {0}bin{1:02g}".format(prefix, i))

