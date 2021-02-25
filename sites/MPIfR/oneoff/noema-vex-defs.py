#!/usr/bin/python3
'''
NOEMA VEX helper tool

Script to manufacture VEX entries for use with NOEMA data.
Sky frequencies of filterbank channels are derived from 1st LO
and 2nd LO information. Channels are always 64 MHz wide.
'''

import argparse
import sys

__author__ = "Jan Wagner (MPIfR)"
__version__ = "1.0.0"


def parse_args(args: []):
	cmd='%(prog)s <options>'
	parser = argparse.ArgumentParser(description=__doc__, add_help=True, formatter_class=argparse.RawDescriptionHelpFormatter)
	parser.add_argument('--version', action='version', version='%(prog)s ' + __version__)
	parser.add_argument('-F', '--lo1', dest='lo1', metavar='MHz', default='92101.0', help='frequency of 1st LO in MHz (default: %(default)s)')
	parser.add_argument('-f', '--lo2', dest='lo2', metavar='MHz', default='7744.0', help='frequency of 2nd LO in MHz (default: %(default)s)')
	parser.add_argument('-S', '--sb1', dest='sb1', metavar='U|L', default='L', help='sideband of 1st mixing (default: %(default)s)')
	opts = parser.parse_args(args)
	return opts


def sideband2int(sideband: str) -> int:
	if sideband.lower() in ['u','usb']:
		return +1
	if sideband.lower() in ['l','lsb']:
		return -1
	print('Unknown sideband "%s": expected one of L, LSB, U, or USB')
	return 0


def int2sideband(sideband: int) -> str:
	if sideband < 0:
		return 'L'
	return 'U'


def generate_chandefs(opts, pol='Lcp'):

	# PolyFiX correlator and VLBI interface board properties
	bw = 64
	Nchan = 64
	Nchannels_per_thread = 4
	Nthreads = Nchan // Nchannels_per_thread
	Nthreads_per_10GbE = 4

	# User-specified tuning
	sb1 = sideband2int(opts.sb1)
	lo1 = float(opts.lo1)
	lo2 = float(opts.lo2)

	if pol.lower() in ['r','rcp']:
		bbcnr = 1
	else:
		bbcnr = 2

	print('def Freq_Nq_%dx%d;' % (Nchan,bw))
	print('  sample_rate =   %.1f Ms/sec;  * (2bits/sample)' % (2*bw))
	for sb2 in [-1,+1]:
		netsb = sb1*sb2
		print('  * PolyFiX Polar %s - RX sideband %sSB - backend baseband %sSB - net %sSB' % (pol,int2sideband(sb1),int2sideband(sb2),int2sideband(netsb)))
		for thread in range(0,Nthreads):
			if (thread % Nthreads_per_10GbE) == 0:
				tenGb_iface = thread // Nthreads_per_10GbE
				print('  * 10GbE link %d' % (tenGb_iface))
			for threadchannel in range(0,Nchannels_per_thread):
				ch = thread*Nchannels_per_thread + threadchannel
				# fcenter = lo1 + sb1*lo2 + sb2*ch*bw # would seem correct
				fcenter = lo1 + sb1*lo2 + sb1*sb2*ch*bw # is what Excel sheet essentially uses, also makes sense, ch0 being referenced to middle not edge of IF
				if netsb > 0:
					fedge = fcenter - bw/2
					flabel = 'U'
				else:
					fedge = fcenter + bw/2
					flabel = 'L'
				chan_def = '  chan_def = : %.1f MHz : %s : %.1f MHz : &Ch%02d : &BBC%02d : &%s;' % (fedge,flabel,bw,ch+1,bbcnr,pol)
				note = ' * FX Ch:%d VDIF Th:%d Ch:%d mid %.1f' % (ch,thread,threadchannel,fcenter)
				print(chan_def + note)
	print('enddef;')

if __name__ == "__main__":
	opts = parse_args(sys.argv[1:])
	generate_chandefs(opts)

