#!/usr/bin/env python2

from __future__ import absolute_import
from __future__ import print_function
import numpy as np
from numpy.polynomial import polynomial as P
import matplotlib
import matplotlib.pyplot as plt
from astropy import constants as const
from scipy.optimize import curve_fit
import os, sys,argparse

def exp_cdf(x, l):
    return 1.0 - np.exp(-l*x)

parser = argparse.ArgumentParser()
parser.add_argument("-n", "--nbins", type=int, default=None, help="Number of images files from which to extract the spectrum; note: zero-indexed, so nbins=11 means bin00 to bin10")
parser.add_argument("-c", "--nchan", type=int, default=None, help="Number of channel slices in each cube image; note: zero-indexed")
parser.add_argument("-s", "--src", type=str, default=None, help="Source name to be used for the spectra text file prefix")
parser.add_argument("-r", "--res", type=float, default=None, help="Temporal resolution of data in ms")
parser.add_argument("-z", "--zero", default=False, help="Set zeroth bin equal to zero; use for nbins>1 when the zeroth bin contains little to no signal but mostly noise", action="store_true")
parser.add_argument("-f", "--basefreq", type=float, default=None, help="The lowest frequency in the observation in MHz")
parser.add_argument("-F", "--fscrunch", default=False, help="Make fscrunched plot", action="store_true")
parser.add_argument("--pols", type=str, default="I", help='The polarisations to be imaged if --imagecube is set. Defaulted to all. Input as a list of strings: e.g., "XX,YY"')
parser.add_argument("-a", "--avg", type=int, default=16, help="Number of channels to average together per image cube slice")
parser.add_argument("--rms", default=False, action="store_true", help="Use the off-source rms estimate")
parser.add_argument("--flagchans", default="", help="comma-separated list of channels to zero in the plot")
parser.add_argument("--frbtitletext", type=str, default="", help="The name of the FRB (or source) to be used as the title of the plots")
parser.add_argument("--rotmeas", type=float, help="The rotation measure for the pulse; used to derotate the data prior to frequency scrunching")
parser.add_argument("-t", "--threshold_factor", type=float, default=2.5, help="Factor to use in thresholding for masking the polarisation position angle for plotting")
parser.add_argument("--isolate", default=False, action="store_true", help="Set if you want to calculate the polarisation fractions or to isolate specific sub-pulses")
parser.add_argument("--pulse_number", type=int, default=None, help="Number of the pulse of interest for determining the polarisation fraction values")
parser.add_argument("--binstartstop", type=str, default=None, help="Start and end bins for integrating over the pulse to get the polarisation fraction totals; input as a comma separated string")
parser.add_argument("--multi_rotmeas", type=str, default=None, help="To be used for cases requiring multiple RMs; input is a list of strings: e.g., RM1,RM2")
parser.add_argument("--rm_bins_starts", type=str, default=None, help="The start bins used for de-rotating at a given RM; to be used with --multi_rotmeas set; input is a list of strings: e.g., RM1_start,RM2_start,RM3_start")
parser.add_argument("--scintillation", default=False, action="store_true", help="Set if you want to calculate the scintillation decorrelation bandwidth for a pulse")
parser.add_argument("--dm_offset", type=float, default=0.0, help="A roughly estimate of the residual DM to be corrected in a pulse")
parser.add_argument("--delta_psi", type=float, default=0.0, help="The polarisation position angle change used in the polarisation calibration; in radians")
parser.add_argument("--delta_t", type=float, default=0.0, help="The delay derived between the linearly polarised feeds to be used in the polarisation calibration; in nanosec")
parser.add_argument("--phi", type=float, default=0.0, help="The phase offset derived between the linearly polarised feeds to be used in the polarisation calibration; in radians")
parser.add_argument("--unwrap", default=False, action="store_true", help="Set if there is clear phase wrapping in the FRB PA")

args = parser.parse_args()

print(args)

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

if args.nbins is None:
    parser.error("You must specify the number of images you're processing")

if args.nchan is None:
    parser.error("You must specify the number of slices in the cube image")

if args.src is None:
    parser.error("You must specify an output spectra file name prefix")

if args.res is None:
    parser.error("You must specify the data's temporal resolution")

if args.binstartstop is None:
    parser.error("You must specify the start and stop bins for the pulse for which you want to obtain the polarisation fractions")

nbins = args.nbins
nchan = args.nchan
flagchans = [int(x) for x in args.flagchans.split(',') if x.strip().isdigit()]
src = args.src
res = args.res
frbtitletext = args.frbtitletext
binstart = int(args.binstartstop.split(',')[0])
binstop = int(args.binstartstop.split(',')[1])

# Define dynamic and fscrunched spectra parameters
if args.avg == 24:
    chunksize = 6 #MHz
if args.avg == 16:
    chunksize = 4 #MHz
bandwidth = args.avg/chunksize * nchan # MHz
startchan = 0
endchan=nchan - 1
dynspec = {}
dynrms = {}
fscrunch = {}
fscrunchrms = {}

# Change global font size
matplotlib.rcParams.update({'font.size': 16})

for stokes in args.pols.split(','):

    # Set figure size
    fig, ax = plt.subplots(figsize=(7,7))
    plt.title("Stokes "+ stokes)
    
    dynspec[stokes] = np.loadtxt("{0}-imageplane-dynspectrum.stokes{1}.txt".format(src, stokes))
    print("Shape of Stokes {0} dynamic spectrum: {1}".format(stokes, dynspec[stokes].shape))

    if args.zero:
        dynspec[stokes][0] = 0
    else:
        print("Plotting all bins")
    for f in flagchans:
        print("Flagging channel", f)
        dynspec[stokes][:,f] = 0

    fscrunch[stokes] = np.mean(dynspec[stokes], 1)
    print(dynspec[stokes][binstart:binstop+1].shape)
    normalised = np.mean(dynspec[stokes][binstart:binstop+1], 0) / np.mean(fscrunch[stokes][binstart:binstop+1])
    orderednormalised = np.sort(normalised)
    y = np.array(range(nchan))/float(nchan)
    lowestval = orderednormalised[0]
    startfitindex = 0
    while startfitindex < nchan and orderednormalised[startfitindex] < -lowestval:
        startfitindex += 1
    print(startfitindex)
    print(orderednormalised)
    popt, pcov = curve_fit(exp_cdf, orderednormalised[startfitindex:], y[startfitindex:])
    print(popt)
    yfit = exp_cdf(orderednormalised, popt[0])
    plt.figure()
    plt.plot(orderednormalised, y, label="Data")
    plt.plot(orderednormalised, yfit, label="Best fit")
    plt.legend()
    plt.ylim(0.0,1.0)
    plt.savefig("{0}-stokes{1}-cdf.png".format(src,stokes))
