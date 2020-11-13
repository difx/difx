#!/bin/bash

# Script to run the CASA imaging from calibratefrb.py

module purge
module load casa/5.6.2

casa --nologger -c imagescript.py
