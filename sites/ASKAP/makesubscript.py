#!/usr/bin/env python

# Writes out a script to perform RFI subtraction using uvsubScaled.py on multiple bins
# Requires the output of getRFISubScale.py to be piped to a file that is read here

import os, sys

if not len(sys.argv) == 5:
        print("Usage: {0} <file with weights for bins> <target data base name> <RFI data with full path> <output file base name>".format(sys.argv[0]))
        sys.exit()

rfisub_weights_file = sys.argv[1]
target_data_basename = sys.argv[2]
rfi_data_path = sys.argv[3]
output_filename = sys.argv[4]

file_lines = open(rfisub_weights_file).readlines()
bins = [file_lines[i].split(' ')[1].strip(':') for i in range(len(file_lines))]
scale = [file_lines[i].split(' ')[3].strip('\n') for i in range(len(file_lines))]

rfi_sub_outfile = open("do_rfi_subtraction.sh","w")

for i in range(len(bins)):
    rfi_sub_outfile.write("~/packages/src/psrvlbireduce/datareduction/uvsubScaled.py {0}_B{1:02g}.FITS {2} {3} {4}_B{1:02g}.FITS\n".format(target_data_basename, i, rfi_data_path, scale[i], output_filename))

rfi_sub_outfile.close()

os.system("chmod 775 do_rfi_subtraction.sh")
