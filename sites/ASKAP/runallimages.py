#!/usr/bin/env python
import os,sys,argparse

parser = argparse.ArgumentParser()
parser.add_argument("--rfisub", type=bool, default=False, help="Do RFI subtraction")
parser.add_argument("-t", "--targfile", nargs='+', type=str, default="", help="The gated target FITS file in need of RFI subtraction")
parser.add_argument("-r", "--rfifile", type=str, default="", help="The RFI gated FITS file with which the RFI subtraction is performed")
parser.add_argument("-s", "--scalefact", type=float, default=None, help="The scale factor by which to multiply the RFI file before it's subtracted from the target file: the ratio of the two effective integration times (target int time/RFI int time)")
parser.add_argument("-o", "--outfile", type=str, default="", help="The output, RFI subtracted FITS file prefix (e.g. FRB_SUB)")
parser.add_argument("--cal", type=bool, default=False, help="Calibrate the data")
parser.add_argument("-c", "--caldata", type=str, default="", help="The calibrator FITS file data (note: must be uncalibrated")
parser.add_argument("-d", "--targdata", nargs='+', type=str, default="", help="The target FITS file data to be calibrated; if --rfisub is set, this will default to the output FITS file from the RFI subtraction (e.g. FRB_SUB)")
parser.add_argument("--calflag", type=str, default="", help="Text file containing the AIPS flags to be applied to the calibrator data")
parser.add_argument("--targflag", type=str, default="", help="Text file containing the AIPS flags to be applied to the target data")
parser.add_argument("-a", "--avg", type=int, default=24, help="Number of channels to average together per image cube slice")
parser.add_argument("-p","--phasecenter", type=str, default="", help="Phase centre for the target field (blank will leave it at correlation centre); format example: 'J2000 21h49m24s -52d58m15s' ")
parser.add_argument("-n","--nbins", type=int, default=1, help="Number of bins/target files to RFI subtract or calibrate")

args = parser.parse_args()

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

nbins = args.nbins
tdata = []
targfile = []

if args.rfisub is True:
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
            outfile.append(args.outfile[i].replace('.FITS', ''))
        else: outfile.append(args.outfile.replace('.FITS', ''))


if args.cal is True:
    if args.caldata is '':
        parser.error("You must specify a calibrator dataset")
    else:
        cdata = args.caldata
        cdatms = cdata.replace('.FITS', '').split('/')[-1]
    if args.targdata is '':
        if args.rfisub is True:
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

    if args.rfisub is True:
        print "~/craft/psrvlbireduce/datareduction/uvsubScaled.py {0}.FITS {1} {2} {3}.FITS".format(targfile[i], rfi, scale, outfile[i])
        os.system("~/craft/psrvlbireduce/datareduction/uvsubScaled.py {0}.FITS {1} {2} {3}.FITS".format(targfile[i], rfi, scale, outfile[i]))
    
    if args.cal is True:
        print "~/difx/sites/ASKAP/calibrateFRB.py -c {0} -t {1}.FITS {2} {3} -i -a {4} {5} --uvsrt -x /data/CRAFT/0407.model.fits".format(cdata, tdata[i], cflag, tflag, avgchan, pcen)
        os.system("rm -rf {0}_calibrated_uv.ms".format(cdatms))
        os.system("~/difx/sites/ASKAP/calibrateFRB.py -c {0} -t {1}.FITS {2} {3} -i -a {4} {5} --uvsrt -x /data/CRAFT/0407.model.fits".format(cdata, tdata[i], cflag, tflag, avgchan, pcen))
        os.system("mkdir bin{0:02g}".format(i))
        os.system("mv TARGET* bin{0:02g}".format(i))
        os.system("mv {0}_calibrated_uv.ms bin{1:02g}".format(tdata[i], i))
        os.system("mv casa*.log bin{0:02g}".format(i))
