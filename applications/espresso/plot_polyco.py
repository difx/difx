#!/usr/bin/env python
# program to evaluate and plot polyco.


from __future__ import print_function, division
import optparse
#import matplotlib
from matplotlib import pyplot
import numpy


usage = """%prog [options] <filename>
plots the contents of polyco in <filename>
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--outfile", "-o",
        type="str", dest="outfile", default=None,
        help="outfile")
parser.add_option(
        "-n",
        type="int", dest="npolyco", default=0,
        help="Print Nth polyco value instead of 0th")
(options, args) = parser.parse_args()
if len(args) < 1:
    parser.print_help()
    parser.error("give a file name")

polyco_filename = args[0]
p = options.npolyco

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
# Note that DiFX splits lines on whitespace, not column number as the polyco
# docs would suggest.
with open(polyco_filename) as polyco_file:
    # assume 12 coeffs for start - will update when read line
    ncoeff = 12
    coeff = []
    for lineno, line in enumerate(polyco_file):
        line = line.rstrip()
        values = line.split()
        p_line = lineno % (2+ncoeff//3)
        if p_line == 0:
            pulsar.append(values[0])
            date.append(values[1])
            utc.append(values[2])
            tmid.append(float(values[3]))
            dm.append(float(values[4]))
            doppler.append(float(values[5]))
            residual.append(float(values[6]))
        elif p_line == 1:
            rphase.append(float(values[0]))
            f0.append(float(values[1]))
            obs.append(values[2])
            span.append(int(values[3]))
            ncoeff = int(values[4])
            obs_freq.append(float(values[5]))
            if len(values) > 6:
                binary_phase.append(float(values[6]))
        else:
            coeff.append(float(values[0]))
            coeff.append(float(values[1]))
            coeff.append(float(values[2]))
            if p_line == 1+ncoeff//3:
                coeffs.append(coeff)
                coeff = []

# print values of a representative polyco entry, for visual check
print (
        pulsar[p], date[p], utc[p], tmid[p], dm[p], doppler[p], residual[p],
        rphase[p], f0[p], obs[p], span[p], obs_freq[p], end=" ")
if binary_phase:
    print (binary_phase[p])
print (coeffs[p])

phases = []
times = []
# evaluate the polyco phase as a function of time
for i in range(len(rphase)):
    start = tmid[i]
    end = tmid[i] + span[i]/(24.*60.)
    step = 1./(24.*60.)
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
pyplot.plot(times, phases, ',', label="Polyco phase")
pyplot.legend(loc="best")
pyplot.ylabel("Phase")

poly_fit = numpy.polyfit(times, phases, 1)
poly_eval = numpy.array(phases) - numpy.polyval(poly_fit, times)
pyplot.subplot(2, 1, 2)
pyplot.plot(times, poly_eval, ',', label="Residual to Linear fit")
pyplot.legend(loc="best")
pyplot.xlabel("MJD")

if options.outfile is not None:
    pyplot.savefig(options.outfile)
else:
    pyplot.show()
pyplot.close()
