#!/usr/bin/env python2

# A script to take in a high time resolution binconfig file and re-weight the bins according to the 
# frequency-scrunched amplitude for each bin for use in correlating data with pulsar tools in DiFX
__doc__ = "A script to take in a high time resolution binconfig file and re-weight the bins according to the frequency-scrunched amplitude for each bin for use in correlating data with pulsar tools in DiFX"
# INPUT:
# high time resolution binconfig
# frequency-scrunched spectrum file made from the correlated data resulting from the binconfig provided

# OUTPUT:
# A binconfig containing the same number of bins as the input binconfig, weighted appropriately and 
# according to the spectrum file provided, and changing SCRUNCH OUTPUT to TRUE so that a single,
# time-scrunched dataset will be made during the correlation.

# Revision history:
    # Written by Cherie Day on 1 Oct., 2019
    # Python version: 2.7.14

################################################################################
 # GENERAL PYTHON IMPORTS
################################################################################

import os, sys, argparse
import numpy as np

################################################################################
 # SETTING UP THE ARGUMENT PARSER
################################################################################

parser = argparse.ArgumentParser()
parser.add_argument("-b", "--binconfig", default="", type=str, help="The input binconfig file to be re-weighted")
parser.add_argument("-o", "--outbinconfig", default="", type=str, help="The name of the re-weighted output binconfig")
parser.add_argument("-w", "--weights", default="", type=str, help="The input ferquency-scrunched spectrum file containing the weights")
parser.add_argument("-t", "--threshold", default=0.0, type=float, help="Cutoff threshold below which to set weights to zero")

args = parser.parse_args()

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

# Make the paths absolute if necessary
args.binconfig = os.path.abspath(args.binconfig)
args.weights = os.path.abspath(args.weights)
args.outbinconfig = os.path.abspath(args.outbinconfig)

# Check if the output binconfig exists already and exit if so
if os.path.exists(args.outbinconfig):
    print args.outbinconfig, "exists"
    sys.exit()

################################################################################
 # READING IN THE BINCONFIG FILE AND GATHERING INFORMATION
################################################################################

print "Reading file: {0}".format(args.binconfig)
binconfig_input = np.array(open(args.binconfig, "r").readlines())

# Get number of bins in binconfig; -5 allows for the number of bins being in
# the hundreds, while -1 prevents the newline character's inclusion
numbins = int(binconfig_input[2][-5:-1])
print "The number of bins in this config file is {0}".format(numbins)

# Define the SCRUNCH OUTPUT element index
scrunch_output_linenum=3

################################################################################
 # READING IN THE SPECTRUM TO OBTAIN THE WEIGHTS
################################################################################

# Load in the text as a numpy array; note that this works only with files in
# which each row contains the same number of columns; default data type is float
weights = np.loadtxt(args.weights)

# Check spectrum has correct number of bins
print "Checking that the number of weights matches the number of bins"
if len(weights) != numbins:
    print "The number of weights provided does not match the number required for this binconfig!"
    sys.exit()
else: print "The number of weights equals the number of bins. \nProceeding!"

################################################################################
 # UPDATING WEIGHTS BASED ON CUTOFF THRESHOLD
################################################################################

print "Updating weights based on cutoff threshold: {0}".format(args.threshold)
cutoff_index_nums = np.where(weights < args.threshold)
thresholded_weights = weights.copy()
thresholded_weights[cutoff_index_nums] = 0

################################################################################
 # WRITING OUT THE NEW BINCONFIG FILE
################################################################################

print "Writing the new binconfig file: {0}".format(args.outbinconfig)
linenum = 0
weightnum = 0
output = open(args.outbinconfig, "w")
# Write out the unchanged header lines
for header in binconfig_input[:3]:
    output.write(header)
# Change SCRUNCH OUTPUT to TRUE
output.write(binconfig_input[3].replace('FALSE', 'TRUE'))
# Write out the remaining lines, replacing the weights with the thresholded ones
for binline in binconfig_input[4:]:
    if (linenum % 2) != 0:
        output.write(binline.replace('1.0', str(thresholded_weights[weightnum])))
        weightnum += 1
    else: output.write(binline)
    linenum += 1
output.close()
