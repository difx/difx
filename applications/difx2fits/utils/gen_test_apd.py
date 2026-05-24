#!/bin/env python

desc = '''Program to generate test .apd and .channels files for use in testing apd2clock and similar.'''

obscode = 'TEST0'

antennas = ['BR', 'FD', 'HN', 'KP', 'LA', 'MK', 'NL', 'OV', 'PT', 'SC']

# sample times for measurements [mjd]
times = [60000.0, 60000.05, 60000.1, 60000.15]

source = 'CAL0'

# (freq [MHz], bw [MHz], sideband 'U'|'L', pol 'R'|'L')
channels = [(2300.25, 16.0, 'U', 'R'), (2340.25, 16.0, 'U', 'R'), (8400.25, 16.0, 'U', 'R'), (8460.25, 16.0, 'U', 'R')] 

# delays [ns]
delays = [ 5.0, -10.0, 0.0, -6.0, 111.11, -67.0, 0.01, 0.02, -0.05, 12.0 ]

# rates [ns/day]
rates = [ -67.0, 0.01, 0.0, -0.05, 12.0, 5.0, -10.0, 3.0, -6.0, 111.11 ]

def genChannels():
	filename = obscode + '.channels'
	print('Generating %s' % filename)
	o = open(filename, 'w')
	o.write('obscode:  %s\n' % obscode)
	o.write('MJD %14.8f %d 1\n' % (times[0], len(channels)))
	for c in range(len(channels)):
		o.write('%d ' % (c+1))
		o.write('%5.3f %5.3f %s %s\n' % channels[c])
	o.close()

def genAPD():
	filename = obscode + '.apd'
	print('Generating %s' % filename)
	o = open(filename, 'w')
	o.write('obscode:  %s\n' % obscode)
	for timeIndex in range(len(times)):
		t = times[timeIndex]
		day = int(t)	# [day]
		hour = float((t - day)*24)	# [hour]
		deltaday = t - times[0]	# [day]
		for a1index in range(len(antennas)-1):
			a1name = antennas[a1index]
			for a2index in range(a1index + 1, len(antennas)):
				a2name = antennas[a2index]
				o.write('%5d %10.7f %2d %-10s %2d %2d %-3s %-3s %2d' % (day, hour, 1, source, a1index+1, a2index+1, a1name, a2name, len(channels)))
				for c in channels:
					rate = rates[a2index] - rates[a1index]
					delay = delays[a2index] - delays[a1index] + rate*deltaday
					amp = 1.0
					phase = 0.0
					phaseRate = 0.0
					o.write(' %10.4f %7.5f %10.4f %10.6f' % (delay, amp, phase, phaseRate))

				o.write('\n')
	o.close()
	

genChannels()
genAPD()
