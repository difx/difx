#!/usr/bin/python3
'''
Usage: alist2clockbreaks.py <1-letter station ID> <2-letter station ID> <A-list file> <vexfile>

Converts an A-list file into a series of VEX clock_early statements for the given station.
These correct for the residual clock offset and rate at the beginning of every VLBI scan.
Scans without fringes in the A-list file get an interpolated clock_early entry.

Useful for stations with an unstable frequency standard whose time behaviour cannot be adequately
expressed by the usual linear (offset, rate) or quadratic (offset, rate, acceleration) clock models.

Example:
$ alist2clockbreaks.py K Kt e23c18-0-b1-1234-v1-nopc.alist_v6 e23c18-0-b1.vex.obs
'''

__author__ = 'Jan Wagner (MPIfR)'
__copyright__ = 'Copyright 2024, MPIfR'
__license__ = 'GNU GPL 3'
__version__ = '1.0.0'

from eat.io import hops
from eat.io import util as hopsutil

import matplotlib.pyplot as plt
import datetime
import numpy as np
import math
import sys
import re

from scipy.interpolate import Akima1DInterpolator

MIN_SNR = 20
MIN_DELAY_DIFF_us = 0.05  # usec
MIN_RATE_DIFF_pss = 0.05  # picosec/sec
DO_PLOT = True

verbosity = 0


def vextime_fmt(dtime):
	return dtime.strftime('%Yy%jd%Hh%Mm%Ss')

def vextime_to_datetime(vextime):
	t = datetime.datetime.strptime(vextime, '%Yy%jd%Hh%Mm%Ss')
	t.replace(tzinfo=datetime.timezone.utc)
	return t

def hopstime_to_datetime(year_str, timetag_str):
	ts = "%s-%s" % (year_str, timetag_str)
	t = datetime.datetime.strptime(ts, '%Y-%j-%H%M%S')
	t.replace(tzinfo=datetime.timezone.utc)
	return t


def read_vex_section(vexfile, sectionname="$CLOCK"):
	'''
	Return an entire VEX section as a one-liner,
	with all comments and linebreaks removed
	'''

	section = ""
	section_started = False

	with open(vexfile) as file:

		for line in file:
			line = line.strip()
			if '*' in line:
				line = line[:line.find('*')].strip()
			if len(line) < 1:
				continue

			if line[0] == "$":
				if sectionname in line:
					section_started = True
				elif section_started:
					# Got desired section (e.g. $CLOCK), now some new $SECTION, finished.
					return section
				continue

			if section_started:
				section += line + " ";

	return section


def read_vex_scan_starttimes(vexfilename):
	'''
	Collect all start times from VEX $SCHED section
	'''

	schedsection = read_vex_section(vexfilename, sectionname="$SCHED")

	re_scan = re.compile(r'start[\s]*=[\s]*(.*?)[\s]*;')
	starttimes = []

	for m in re_scan.finditer(schedsection):
		vextime = m.group(1).strip()
		starttimes.append(vextime_to_datetime(vextime))

	return starttimes


def read_vex_antenna_clocks(vexfilename, vex_station):
	'''
	For a given station return the existing VEX $CLOCK entry/entries.
	Useful to get the base clock offset and clock rate that was used
	in an initial correlation pass without clock breaks in the track.
	'''

	re_clock_early_set = re.compile(r'def[\s]+([\w\d]+)[\s]*;(.*?);[\s]*enddef[\s]*;[\s]*')
	re_clock_early_item = re.compile(r'(clock_early[\s]*=[\s]*(.*?):(.*?):(.*?):(.*?)[\s]*;)+')
	re_antdef = re.compile(r'def[\s]+%s[\s]*;' % (vex_station))

	clksection = read_vex_section(vexfilename, sectionname="$CLOCK")

	clock_entries = []

	for m in re_clock_early_set.finditer(clksection):
		antdefs = m.group(0)
		if not re_antdef.match(antdefs):
			continue
		for c in re_clock_early_item.finditer(antdefs):
			tepoch = vextime_to_datetime(c.group(2).strip())
			trate = vextime_to_datetime(c.group(4).strip())
			delay = float(c.group(3).strip().split(' ')[0])
			if 'usec' not in c.group(3):
				# assume VEX entry was in seconds, convert to usec
				delay = 1e6*delay
			rate = float(c.group(5).strip())
			if True:
				# assume VEX entry was in seconds/second, convert to picoseconds/second
				rate = 1e12 * rate

			clock_entry = {'T': tepoch, 'delay_us':delay, 'rate_ps_s':rate}
			clock_entries.append(clock_entry)

	return clock_entries


def generate_clock_statements(hops_stn, vex_stn, afilename, clk0=None, explicit_times=[]):

	alist = hops.read_alist(afilename)
	scangroups = alist.filter(['year','timetag','baseline','snr','sbdelay','delay_rate']).groupby(['timetag'])

	time_series = []
	rdelay_series = []	# A-list residual delays [usec]
	rrate_series = []	# A-list residual rates [ps/s]
	rate_series = []	# sum of VEX clock_early rate + A-list residual rate  [ps/s]
	delay_series = []	# sum of VEX clock_early delay + time * clock_early rate + A-list residual delay  [usec]

	brk_times = []
	breaks = [] # str

	if clk0:
		model_epoch = vexclks[-1]['T']
		model_delay_usec = vexclks[-1]['delay_us']
		model_rate_pss = vexclks[-1]['rate_ps_s']

		brk_times.append(model_epoch)

		time_series.append(model_epoch)
		rdelay_series.append(0)
		rrate_series.append(0)
		rate_series.append(model_rate_pss)
		delay_series.append(model_delay_usec)

		if verbosity > 0:
			print('Base model: ', vextime_fmt(model_epoch), model_delay_usec, model_rate_pss)

	else:
		model_delay_usec = 0
		model_rate_pss = 0


	for scanname, baselines in scangroups:

		delays = []
		rates = []
		T = None

		for index, baseline in baselines.iterrows():

			if baseline['snr'] < MIN_SNR:
				continue
			if baseline['baseline'][0] == baseline['baseline'][1] or hops_stn not in baseline['baseline']:
				continue
			if abs(baseline['sbdelay']) < MIN_DELAY_DIFF_us and abs(baseline['delay_rate']) < MIN_RATE_DIFF_pss:
				continue

			# T = hopsutil.tt2dt(baseline['timetag'], year=int(baseline['year'])) # hopsutil bug: time ends up not in UTC but in local timezone
			T = hopstime_to_datetime(baseline['year'], baseline['timetag'])
			T_vex = vextime_fmt(T)

			sbdelay = baseline['sbdelay']
			rate = baseline['delay_rate']
			if baseline['baseline'][0] == hops_stn:
				sbdelay = -sbdelay
				rate = -rate

			delays.append(sbdelay)
			rates.append(rate)

			if verbosity > 1:
				print(scanname, ' take ', baseline['baseline'], sbdelay, rate, T_vex)

		if verbosity > 0 and len(delays) > 0:
			print ("Scan %s had %d baselines to %s with high residuals" % (scanname, len(delays), hops_stn))

		if len(delays) < 1 or not T:
			continue

		rdelay = np.mean(delays)
		rrate = np.mean(rates)

		if T in time_series:
			# fixme: look up index of existing entry, update the data
			continue

		time_series.append(T)
		rdelay_series.append(rdelay)
		rrate_series.append(rrate)

		rate_series.append(model_rate_pss + rrate)
		delay_series.append(model_delay_usec + rdelay + 1e-6*model_rate_pss * (T - model_epoch).total_seconds())

		if len(explicit_times) < 1:
			brk_times.append(T)


	# User-provided clock break times, include original VEX model epoch if also provided
	if len(explicit_times) > 0:
			brk_times = []
			if clk0 and model_epoch not in explicit_times:
				brk_times.append(model_epoch)
			brk_times += explicit_times
	brk_times.sort()

	# Interpolate
	xdata = [t.timestamp() for t in time_series]
	xeval = [t.timestamp() for t in brk_times]

	#brk_rates = Akima1DInterpolator(xdata, rate_series, method='makima')(xeval)  # scipy-1.13+ has 'method' arg
	brk_rates = Akima1DInterpolator(xdata, rate_series)(xeval)
	brk_delays = Akima1DInterpolator(xdata, delay_series)(xeval)

	# Generate VEX $CLOCK entries
	for n in range(len(xeval)):
		T_vex = vextime_fmt(brk_times[n])
		prefix = ''
		if math.isnan(brk_delays[n]) or math.isnan(brk_rates[n]):
			clk_str = '  * clock_early = %s : NaN usec : %s : NaN;' % (T_vex, T_vex)
		else:
			#clk_str = '  clock_early = %s : %.6f usec : %s : %.6e;' % (T_vex, brk_delays[n], T_vex, brk_rates[n]*1e-12)
			clk_str = '  clock_early = %s : %.6f usec : %s : %.6fe-12;' % (T_vex, brk_delays[n], T_vex, brk_rates[n])
		breaks.append(clk_str)

	if verbosity > 1:
		print('A-file')
		for n in range(len(time_series)):
			print('%s model rate %.6f ps/s delay %.4f us at t0 - A-file resid rate %.6f ps/s delay %.4f us - interpolated %.6f ps/s %.4f us' % (vextime_fmt(time_series[n]), model_rate_pss, model_delay_usec, rrate_series[n], rdelay_series[n], rate_series[n], delay_series[n]))
		print('Interpolated to scan start times')
		for n in range(len(xeval)):
			print(vextime_fmt(brk_times[n]), brk_rates[n], brk_delays[n])

	if DO_PLOT:
		plt.figure(1)
		plt.title('VEX $CLOCK Rates')
		#plt.plot(xdata, rate_series, 'x-')
		#plt.plot(xeval, brk_rates, 'o')
		plt.plot(time_series, rate_series, 'x-')
		plt.plot(brk_times, brk_rates, 'o')
		plt.legend(['VEX base + A-list residual','Interpolated'])
		plt.xlabel('Time (Unix Seconds)')
		plt.ylabel('Rate (ps/s)')

		plt.figure(2)
		plt.title('VEX $CLOCK Delays')
		plt.plot(xdata, delay_series, 'x-')
		plt.plot(xeval, brk_delays, 'o')
		plt.legend(['VEX base + A-list residual','Interpolated'])
		plt.xlabel('Time (Unix Seconds)')
		plt.ylabel('Delay (usec)')

	return breaks


if len(sys.argv) != 5:
	print(__doc__)
	sys.exit()

hops_stn = sys.argv[1][0]
vex_stn = sys.argv[2][0:2]
afilename = sys.argv[3]
vexfilename = sys.argv[4]

# Get initial delay and rate of station
vexclks = read_vex_antenna_clocks(vexfilename, vex_stn)
clock0 = vexclks[-1]

# Get start times of every scan regardless of station(s)
scantimes = read_vex_scan_starttimes(vexfilename)

# Read A-list file
breaks = generate_clock_statements(hops_stn, vex_stn, afilename, clock0, scantimes)
# breaks = generate_clock_statements(hops_stn, vex_stn, afilename, clock0, [])

if verbosity > 1:
	print("From A-list derived new clocks:")
	for brk in breaks:
		print(brk)

with open("clockbreaks.vex", "w") as f:
	for brk in breaks:
		f.write(brk + '\n')
	print("Wrote 'clockbreaks.vex'")

if DO_PLOT:
	plt.show()
