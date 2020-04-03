#!/usr/bin/env python2

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
parser.add_argument("--acfstartstop", type=str, default=None, help="Start and end bins for calculating the ACF; input as a comma separated string")
parser.add_argument("--acf_offpulsebin", type=int, default=None, help="Off-pulse bin used to compare ACF of on-pulse bins")

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
acfstart = int(args.acfstartstop.split(',')[0]) # starting time bin for user selected range
acfstop = int(args.acfstartstop.split(',')[1]) # final time bin for user selected range
acfoffpulse = args.acf_offpulsebin # user selected off-pulse time bin

# Change global font size for easier reading
matplotlib.rcParams.update({'font.size': 16})


#####################################################################################

# 1D ACF

#####################################################################################

# Load calibrated data
dynspec_I = np.loadtxt("{0}-imageplane-dynspectrum-calibrated.stokesI.txt".format(src))

# Calculate the 1D ACF across frequency for each time bin using statsmodels.tsa.stattools.acovf
acf_time_onpulse = [st.acovf(time_bin, unbiased=True, demean=True, fft=False, missing='none', nlag=nchan-1) for time_bin in dynspec_I[acfstart:acfstop+1]]
acf_time_offpulse = st.acovf(dynspec_I[acfoffpulse], unbiased=True, demean=True, fft=False, missing='none', nlag=nchan-1)

print("Number of time bins for ACF on-pulse range: {}".format(np.shape(acf_time_onpulse)[0]))
print("Start bin for ACF: {}".format(acfstart))
print("Stop bin for ACF: {}".format(acfstop))
print("Off-pulse time bin used: {}".format(acfoffpulse))

# Set up figure and axes
acf_fig, acf_ax = plt.subplots(figsize=(11,11))

# Plot the ACF for the selected time bins
for binnum, timebin in enumerate(acf_time_onpulse):
    acf_ax.plot(timebin[1:], label='Time bin {}'.format(binnum+acfstart))

# Plot off-pulse time bin
acf_ax.plot(acf_time_offpulse[1:], label='Off-pulse (bin {})'.format(acfoffpulse))
plt.legend()
acf_fig.savefig("{0}-ACF_selected_time_bins_{1}to{2}.png".format(src,acfstart,acfstop), bbox_inches='tight')
