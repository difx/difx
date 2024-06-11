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
		if verbosity > 0:
			print("Adding entry %s %s with SNR %5.1f and rate %+.6f ps/s" % (name, row['timetag'], row['snr'], row['delay_rate']))
		times.append(T)
		rates.append(row['delay_rate'])

print("Fitting clock rate data from %s to %s" % (str(tstart), str(tstop)))
datasecs = [(t - tstart).seconds for t in times]

coeff = np.polyfit(datasecs, rates, 1)
fitted_fn = np.poly1d(coeff)

accel = coeff[0]*1e-12
print('For DiFX v2d:')
print('   deltaClockAccel = %+.6e  # usec/sec' % (-accel*1e6/2.0))

plt.plot(datasecs, rates, 'yo',  datasecs, fitted_fn(datasecs), '--k')
plt.title('Acceleration fit into fringe rates to %s' % (stn))
plt.xlabel('Time (s)')
plt.ylabel('Residual Rate (ps/s)')
plt.legend(['A-list rates', 'Accel. %+.6e s/s' % (accel)])
plt.grid()
plt.show()
