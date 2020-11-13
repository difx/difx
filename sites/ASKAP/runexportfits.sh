#!/bin/bash

# Script to run the CASA call for converting the calibrated image file to a FITS files

module purge
module load casa/5.6.2

casa --nologger -c exportfits.py
