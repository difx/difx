#!/usr/bin/env python

from __future__ import absolute_import
from __future__ import print_function
import numpy as np
from numpy.polynomial import polynomial as P
import matplotlib
import matplotlib.pyplot as plt
from astropy import constants as const
import os, sys,argparse
import statsmodels.tsa.stattools as st

#####################################################################################

# SET UP ARGUEMENTS

#####################################################################################

parser = argparse.ArgumentParser()
parser.add_argument("-n", "--nbins", type=int, default=None, help="Number of images files from which to extract the spectrum; note: zero-indexed, so nbins=11 means bin00 to bin10")
parser.add_argument("-c", "--nchan", type=int, default=None, help="Number of channel slices in each cube image; note: zero-indexed")
parser.add_argument("-s", "--src", type=str, default=None, help="Source name to be used for the spectra text file prefix")
parser.add_argument("--timeres", type=float, default=None, help="Temporal resolution of data in ms")
parser.add_argument("--freqres", type=float, default=None, help="Spectral resolution of data in MHz")
parser.add_argument("-f", "--basefreq", type=float, default=None, help="The lowest frequency in the observation in MHz")
parser.add_argument("--frbtitletext", type=str, default="", help="The name of the FRB (or source) to be used as the title of the plots")
parser.add_argument("--ccfstartstop", type=str, default=None, help="Start and end bins for calculating the CCF (startbin1,stopbin1,startbin2,stopbin2)")

args = parser.parse_args()

print(args)

#####################################################################################

# CATCH ERRORS

#####################################################################################

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

if args.nbins is None:
    parser.error("You must specify the number of images you're processing")

if args.nchan is None:
    parser.error("You must specify the number of slices in the cube image")

if args.src is None:
    parser.error("You must specify an input/output spectra file name prefix")

if args.timeres is None:
    parser.error("You must specify the data's temporal resolution")

if args.freqres is None:
    parser.error("You must specify the data's spectral resolution")

if args.basefreq is None:
    parser.error("You must specify the data's lowest frequency")

if args.ccfstartstop == None or len(args.ccfstartstop.split(',')) != 4:
    parser.error("You must specify the bin ranges to use for calculations in format start1,stop1,start2,stop2")

#####################################################################################

# GLOBAL PARAMETER DEFINITIONS

#####################################################################################

# Source specific parameters
src = args.src # source name
frbtitletext = args.frbtitletext # name of source to use in plotting

# Temporal parameters
nbins = args.nbins # number of time bins
timeres = args.timeres # resolution of time bins (ms)
starttime = 0 # start time (ms)
endtime = nbins*timeres # final time (ms)

# Spectral parameters
nchan = args.nchan # number of frequency channels in the data
endchan = nchan - 1 # remove final channel due to bandpass rolloff
freqres = args.freqres # spectral resolution of the data (MHz)
basefreq = args.basefreq # lowest frequency in the data (MHz)
endfreq = basefreq + (endchan*freqres) # highest frequency to be used (MHz)
freqs = np.linspace(basefreq, endfreq, nchan) # MHz

# ACF parameters
ccfstart1 = int(args.ccfstartstop.split(',')[0]) # starting time bin for user selected range
ccfstop1 = int(args.ccfstartstop.split(',')[1]) # final time bin for user selected range
ccfstart2 = int(args.ccfstartstop.split(',')[2]) # starting time bin for user selected range
ccfstop2 = int(args.ccfstartstop.split(',')[3]) # final time bin for user selected range

# Change global font size for easier reading
matplotlib.rcParams.update({'font.size': 16})


#####################################################################################

# 1D CCF

#####################################################################################

# Load calibrated data
print("Loading data: {0}-imageplane-dynspectrum-calibrated.stokesI.txt".format(src))
dynspec_I = np.loadtxt("{0}-imageplane-dynspectrum-calibrated.stokesI.txt".format(src))

# TODO: Weight the time average by fscrunch, rather than doing a simple mean
pulse1spectra = np.mean(dynspec_I[ccfstart1:ccfstop1+1], 0)
pulse2spectra = np.mean(dynspec_I[ccfstart2:ccfstop2+1], 0)

# CCF calculation for individual bins
print("Computing CCF between user-selected pulse1 and pulse2")

# Calculate the 1D CCF across frequency between the two user-selected time ranges using statsmodels.tsa.stattools.ccovf
ccf = st.ccovf(pulse1spectra, pulse2spectra)

# Set up figure and axes
ccf_fig, ccf_ax = plt.subplots(figsize=(7,7))

# Plot the CCF
ccf_ax.plot(ccf, label='CCF (bins {0}-{1} x {2}-{3})'.format(ccfstart1,ccfstop1,ccfstart2,ccfstop2))
plt.legend()
ccf_fig.savefig("{0}-CCF_bin{1}-{2}x{3}-{4}.png".format(src,ccfstart1,ccfstop1,ccfstart2,ccfstop2), bbox_inches='tight')
