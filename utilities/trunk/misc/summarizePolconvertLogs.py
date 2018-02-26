#!/usr/bin/python

import glob

class bcolors:
	DEFAULT = '\033[95m'
	GREEN = '\033[92m'
	ORANGE = '\033[93m'
	RED = '\033[91m'
	ENDC = '\033[0m'

def reportOnLog(fn):
	job = fn.split('.')[0]
	f = open(fn, 'r')
	nprinted = 0
	while True:
		l = f.readline()
		if len(l) < 1:
			break
		if not('NORM. FRINGE PEAKS' in l):
			continue
		#"FOR IF #59. NORM. FRINGE PEAKS:
		#  RR: 1.00e+00 ; SNR: 73.8
		#  LL: 5.64e-01 ; SNR: 49.2
		#  RL: 1.33e-01 ; SNR: 11.7
		#  LR: 2.70e-01 ; SNR: 23.0"
		IF = int(l[8:10])
		RR = float( f.readline().split()[-1] )
		LL = float( f.readline().split()[-1] )
		RL = float( f.readline().split()[-1] )
		LR = float( f.readline().split()[-1] )
		verdict = bcolors.GREEN + 'good' + bcolors.ENDC
		if (RL > 0.5*RR) or (LR > 0.5*LL):
			verdict = bcolors.ORANGE + 'poor' + bcolors.ENDC
		if (RL > 0.9*RR) or (LR > 0.9*LL):
			verdict = bcolors.RED + 'bad' + bcolors.ENDC
		if nprinted == 0:
			print ('# %s' % (job))
		print ('# %s IF#%d SNRs : RR %6.2f, LL %6.2f, LR %6.2f, RL %6.2f : %s' % (4*' ',IF,RR,LL,RL,LR,verdict))
		nprinted += 1
	f.close()

flist = glob.glob('*.polconvert-*/PolConvert.log')
flist.sort()
for fn in flist:
	reportOnLog(fn)
