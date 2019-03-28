#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
import os, sys,argparse

parser = argparse.ArgumentParser()
parser.add_argument("-n", "--nbins", type=int, default=None, help="Number of images files from which to extract the spectrum; note: zero-indexed, so nbins=11 means bin00 to bin10")
parser.add_argument("-c", "--nchan", type=int, default=None, help="Number of channel slices in each cube image; note: zero-indexed")
parser.add_argument("-s", "--src", type=str, default=None, help="Source name to be used for the spectra text file prefix")

args = parser.parse_args()

print args

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

if args.nbins is None:
    parser.error("You must specify the number of images you're processing")

if args.nchan is None:
    parser.error("You must specify the number of slices in the cube image")

if args.src is None:
    parser.error("You must specify an output spectra file name prefix")

nbins = args.nbins
nchan = args.nchan
src = args.src

os.system("mkdir bins2waterlogs")

for stokes in ["I","Q","U","V","XX","YY"]:
    dynspec = np.zeros(nbins*nchan).reshape(nbins,nchan)
    for i in range(nbins):
        inputimage = "bin{0:02g}/TARGET.cube.{1}.image".format(i, stokes)
        runfile = "waterfall.py"
        output = open(runfile, "w")
        for chan in range(nchan):
            output.write('imstat(imagename="{0}",box="64,63,65,64",chans="{1:d}",logfile="waterfall.bin{2:02d}.chan{3:03d}.log")\n'.format(inputimage, chan, i, chan))
        output.close()
        os.system("casa --nologger -c {0}".format(runfile))
        for chan in range(nchan):
            logfile = "waterfall.bin{0:02d}.chan{1:03d}.log".format(i, chan)
            loglines = open(logfile).readlines()
            for line in loglines:
                if "maximum value [max]:" in line:
                    dynspec[i][chan] = float(line.split()[-2])
                    break
        os.system("rm -f waterfall.*.log")
        os.system("mv casa*.log bins2waterlogs")

    np.savetxt("{0}-imageplane-dynspectrum.stokes{1}.txt".format(src, stokes), dynspec)
