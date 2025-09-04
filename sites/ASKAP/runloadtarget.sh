#!/bin/bash

# Script to run the CASA call for converting the UVFITS file of the calibrator and target data to CASA Measurement Sets

#module purge
#module load casa/5.6.2

casa --nologger -c loadtarget.py
