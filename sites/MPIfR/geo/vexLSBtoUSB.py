#!/usr/bin/python
'''
Usage: vexLSBtoUSB.py <vexfile>

Scans the VEX file and prints out all $FREQ section LSB chan_def
entries as their USB equivalent with the correct edge frequency.
'''
import sys
import re

def vexLSBtoUSB(fname):
	with open(fname,'r') as f:
		bw = 0
		for line in f.readlines():
			line = line.strip()
			if len(line) < 1:
				continue
			if 'chan_def' not in line:
				continue
			fields = re.split(' *|\t*', line)
			if (fields[7] == 'U'):
				continue
			if (fields[7] != 'L'):
				print('Unexpected decode of line %s\n  got: %s' % (line, str(fields)))
				continue
			lsbfreq, bw = float(fields[4]), float(fields[9])
			fields[4] = '%.2f' % (lsbfreq - bw)
			fields[7] = 'U'
			fields[-1] = '&U_cal;'
			print('    %s' % (' '.join(fields)))
		print('    sample_rate = %.1f Ms/sec;' % (2*bw)) # 2xBW even for complex...


if __name__ == '__main__':

	if len(sys.argv) > 1:
		vexfile = sys.argv[1]
		vexLSBtoUSB(vexfile)
	else:
		print(__doc__)

