#!/usr/bin/python
# (C) 2018 Jan Wagner
'''
Usage: summarizePolconvertLogs.py [--help|-h] [--color|-c] [--pols|-p]

Inspects the output of EHT drivepolconvert.py (that invokes CASA polconvert)
and its logfiles in the current working directory. Reports the fringe SNRs
found in the logfiles. Optionally checks the polarizations present in each
output .difx associated with each polconvert log.
'''

import glob, sys
try:
	import parseDiFX
	have_ParseDiFX = True
except ImportError:
	have_ParseDiFX = False

'''Global struct with terminal color codes'''
class bcolors:
	DEFAULT = '\033[95m'
	GREEN = '\033[92m'
	ORANGE = '\033[93m'
	RED = '\033[91m'
	ENDC = '\033[0m'

'''Inspect the DiFX output of a job (<job>.difx/DIFX_*) and look at the polarizations of the visibility data'''
def checkDiFXViz(job):
	# Antennas that are linear polarized
	linear_antennas = ['AA']

	# Read the .input file of the job
	inputfile = job + '.input'
	commoncfg = parseDiFX.get_common_settings(inputfile)
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	if numfreqs == 0:
		print ("Couldn't parse input file " + inputfile + " correctly")
		return False
	antennas = [t.name for t in telescopes]
	has_linpol_ant = any(a in linear_antennas for a in antennas)
	Nvis_stop = numfreqs*4 * numtelescopes*(numtelescopes-1)/2

	# Read the .difx/DIFX_* file referred to from .input
	glob_pattern = commoncfg['difxfile'] + '/DIFX_*.s*.b*'
	difxfileslist = glob.glob(glob_pattern)
	if len(difxfileslist) <= 0:
		print ('Error: no visibility data file found in %s!' % (glob_pattern))
		return
	difxfilename = difxfileslist[0]   # first file only; TODO what if multiple files?
	difxfile = open(difxfilename, 'r')

	# Check visibility records
	Nvis = 0; Nvis_linant = 0
	pols_crossed = False
	pols_found = []
	while True:
	        hdr = parseDiFX.parse_output_header(difxfile)
		if len(hdr)==0:
			break
		[baseline,mjd,seconds,cfgidx,srcidx,freqindex,polpair,pulsarbin,weight,u,v,w,rawhdr] = hdr
		ant2name = telescopes[(baseline % 256) - 1] # order as in difx2mark4: ref=ant1="256*nr", rem=ant2="nr%256"
		ant1name = telescopes[((baseline - (baseline % 256))/256) - 1]
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg
		difxfile.seek(8*nchan, 1)

		Nvis = Nvis + 1
		if polpair[0] != polpair[1]:
			pols_crossed = True
		if polpair[0] not in pols_found:
			pols_found.append(polpair[0])
		if polpair[1] not in pols_found:
			pols_found.append(polpair[1])
		if has_linpol_ant:
			if ((ant1name in linear_antennas) or (ant2name in linear_antennas)):
				Nvis_linant = Nvis_linant + 1
			if Nvis_linant > Nvis_stop:
				break
		else:
			if (Nvis > Nvis_stop):
				break

	# Summary
	pols_circular = ('L' in pols_found) or ('R' in pols_found)
	pols_linear = ('X' in pols_found) or ('Y' in pols_found)
	pols_full = ('L' in pols_found) and ('R' in pols_found)
	pols_full = pols_full or (('X' in pols_found) and ('Y' in pols_found))		
	if pols_circular and pols_linear:
		polinfo = bcolors.RED + 'mixed' + bcolors.ENDC
	elif pols_circular:
		polinfo = 'circular'
	else:
		polinfo = 'linear'
	if pols_full:
		if pols_crossed:
			polinfo += ' full-Stokes'
		else:
			polinfo += ' half-Stokes'
	s = polinfo + ' in ' + difxfilename
	return s

'''Load a polconvert log file and print a summary'''
def reportOnLog(fn,do_pols_check=False):
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
			print ('# %s : %s' % (job,fn))
		print ('# %s IF#%d SNRs : RR %6.2f, LL %6.2f, LR %6.2f, RL %6.2f : %s' % (4*' ',IF,RR,LL,RL,LR,verdict))
		nprinted += 1
	f.close()
	if do_pols_check:
		s = checkDiFXViz(job)
		print ('# %s %s' % (4*' ',s))

# Command line args
doColor = False
doPolCheck = False
if ('--help' in sys.argv) or ('-h' in sys.argv):
	print (__doc__)
	sys.exit(0)
if ('--color' in sys.argv) or ('-c' in sys.argv):
	doColor = True
if ('--pols' in sys.argv) or ('-p' in sys.argv):
	if not have_ParseDiFX:
		print ('Warning: parseDiFX.py module not found, ignoring --pols command line argument')
	else:
		doPolCheck = True
if not doColor:
	bcolors.DEFAULT = ''
	bcolors.GREEN = ''
	bcolors.ORANGE = ''
	bcolors.RED = ''
	bcolors.ENDC = ''

# Process all polconvert log files in the current working directory
flist = glob.glob('*.polconvert-*/PolConvert.log')
flist.sort()
for fn in flist:
	reportOnLog(fn,doPolCheck)
