#!/usr/bin/env python
# program to evaluate and plot polyco.
import sys
import os
import re
import optparse
import math
from math import *
import matplotlib
from matplotlib import pyplot
import numpy


usage = """%prog [options] <filename>
plots the contents of polyco in <filename>"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--outfile", "-o",
        type="str", dest="outfile", default=None,
        help="outfile")
(options, args) = parser.parse_args()
if len(args) < 1:
    parser.print_help()
    parser.error("give a file name")

polyco_filename = args[0]

pulsar = []
date = []
utc = []
tmid = []
dm = []
doppler = []
residual = []
rphase = []
f0 = []
obs = []
span = []
obs_freq = []
binary_phase = []
coeffs = []

#polyco_file = open(polyc_filename).readlines()
# parse the polyco file and build up list of coefficients
with open(polyco_filename) as polyco_file:
    # assume 12 coeffs for start - will update when read line
    ncoeff = 12
    coeff = []
    for lineno, line in enumerate(polyco_file):
        p_line = lineno % (2+ncoeff//3)
        if p_line == 0:
            pulsar.append(line[0:10])
            date.append(line[10:19])
            utc.append(line[19:31])
            tmid.append(float(line[31:51]))
            dm.append(float(line[51:72]))
            doppler.append(float(line[73:79]))
            residual.append(float(line[79:86]))
        elif p_line == 1:
            rphase.append(float(line[0:20]))
            f0.append(float(line[20:38]))
            obs.append(line[38:43])
            span.append(int(line[43:49]))
            ncoeff = int(line[49:54])
            obs_freq.append(float(line[54:63]))
            binary_phase.append(float(line[75:80]))
        else:
            coeff.append(float(line[0:25]))
            coeff.append(float(line[25:50]))
            coeff.append(float(line[50:75]))
            if p_line == 1+ncoeff//3:
                coeffs.append(coeff)
                coeff = []

phases = []
times = []
# evaluate the polyco phase as a function of time
for i in range(len(rphase)):
    start = tmid[i]
    end = tmid[i] + span[i]/(24.*60.)
    step = 10./(24.*60.)
    for time in numpy.arange(start, end, step):
        dt = (time - tmid[i])*1440.
        coeff_sum = 0
        for ci in range(len(coeffs[i])):
            coeff_sum += dt**(ci) * coeffs[i][ci]
        phase = rphase[i] + dt*60.*f0[i] + coeff_sum
        phases.append(phase)
        times.append(time)

#pyplot.tight_layout()

pyplot.subplot(2, 1, 1)
pyplot.plot(times, phases, '.', label="Polyco phase")
pyplot.legend(loc="best")
pyplot.ylabel("Phase")

poly_fit = numpy.polyfit(times, phases, 1)
poly_eval = numpy.array(phases) - numpy.polyval(poly_fit, times)
pyplot.subplot(2, 1, 2)
pyplot.plot(times, poly_eval, '.', label="Residual to Linear fit")
pyplot.legend(loc="best")
pyplot.xlabel("MJD")

if options.outfile is not None:
    pyplot.savefig(options.outfile)
else:
    pyplot.show()
pyplot.close()
