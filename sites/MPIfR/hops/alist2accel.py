#!/usr/bin/python3
'''
Usage: alist2accel.py <1-letter station ID> <A-list file>

Fits an acceleration into the residual rates of a station.
Uses fringes from all baselines to that station, cut by
SNR >= 30, uniformly weighted.
'''

from eat.io import hops
from eat.io import util as hopsutil

import matplotlib.pyplot as plt
import numpy as np
import sys


MIN_SNR = 30
verbosity = 1

if len(sys.argv) != 3:
	print(__doc__)
	sys.exit()

stn = sys.argv[1][0]
afilename = sys.argv[2]

alist = hops.read_alist(afilename)
grouped = alist.filter(['year','timetag','baseline','snr','delay_rate']).groupby(['baseline'])

tstart = None
tstop = None

times = []
rates = []

for name, group in grouped:
	if name[0] == name[1] or stn not in name:
		if verbosity > 1:
			print("Ignoring baseline %s" % (name))
		continue
	for index, row in group.iterrows():
		T = hopsutil.tt2dt(row['timetag'], year=int(row['year']))
		if (row['snr'] < MIN_SNR):
			if verbosity > 1:
				print("Ignoring low SNR entry %s %s" % (name, row['timetag']))
			continue
		if not tstart or T < tstart:
			tstart = T
		if not tstop or T > tstop:
			tstop = T
		rate = row['delay_rate']
		if name[0] == stn:
			rate = -rate
		if verbosity > 0:
			print("Adding entry %s %s with SNR %5.1f and rate %+.6f ps/s" % (name, row['timetag'], row['snr'], rate))
		times.append(T)
		rates.append(rate)

print("Fitting clock rate data from %s to %s" % (str(tstart), str(tstop)))
datasecs = [(t - tstart).seconds for t in times]

coeff = np.polyfit(datasecs, rates, 1)
fitted_fn = np.poly1d(coeff)

accel = coeff[0]*1e-12
dcaccel_str = 'deltaClockAccel = %+.6e' % (-accel*1e6/2.0)

midsec = 0.5*(datasecs[0] + datasecs[-1])
midresidrate = fitted_fn(midsec)*1e-12

print('')
print('For DiFX v2d:')
print('   %s  # us/s^2' % (dcaccel_str))
print('')
print('For DiFX VEX:')
print('   add %.6e to clock_early rate' % (midresidrate))
print('')

plt.plot(datasecs, rates, 'yo',  datasecs, fitted_fn(datasecs), '--k')
plt.scatter(midsec, midresidrate*1e12, marker='d')
plt.title('Acceleration fit into fringe rates to %s\n=> v2d %s' % (stn, dcaccel_str))
plt.xlabel('Time (s)')
plt.ylabel('Residual Rate (ps/s)')
plt.legend(['Input A-list rates', 'Accel. %+.6e s/s^2' % (accel), 'Mid rate %+.6e s/s' % (midresidrate)])
plt.grid()
plt.show()
