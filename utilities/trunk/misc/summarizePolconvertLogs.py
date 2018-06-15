#!/usr/bin/python
# (C) 2018 Jan Wagner
'''
Usage: summarizePolconvertLogs.py [--help|-h] [--color|-c] [--pols|-p]
                                  [--short|-s] [--vex|-v <vexfile>]

Inspects the output of EHT drivepolconvert.py (that invokes CASA polconvert)
and its logfiles in the current working directory. Reports the fringe SNRs
found in the logfiles. Optionally checks the polarizations present in each
output .difx associated with each polconvert log.
'''

import argparse, glob, re, sys
try:
	import parseDiFX
	have_ParseDiFX = True
except ImportError:
	have_ParseDiFX = False

def doFindProj(opts):
	'''
	Read opts.vexfile and find lines locating the scans assigned to projects.

	Duplication of the function doFindProj() found in ehtc-joblist.py
	because an import attempt
	   from ehtc-joblist import doFindProj
 	results in "SyntaxError: invalid syntax" due to the '-' in the filename.
	'''
	try:
		f = open(opts.vexfile, 'r')
	except IOError as e:
		print('Warning: could not open %s : %s' % (opts.vexfile,str(e)))
		return {}
	except Exception as e:
		print('Warning: could not open %s')
		return {}
	lastscan = ''
	thisproj = 'na'
	scan_re = re.compile(r'\s*scan\s*([^;]+);')
	first_re = re.compile(r'.*intent.*=.*"ALMA:PROJECT_FIRST_SCAN:(.*)".*$')
	final_re = re.compile(r'.*intent.*=.*"ALMA:PROJECT_FINAL_SCAN:(.*)".*$')
	comnt_re = re.compile(r'\s*[*]')
	projscans = {}
	for line in f.readlines():
		sre = scan_re.search(line)
		if sre:
			lastscan = sre.group(1)
			continue
		first = first_re.search(line)
		final = final_re.search(line)
		comnt = comnt_re.search(line)
		if not first and not final and comnt:
			continue
		if first:
			thisproj = first.group(1)
		if len(thisproj) > 0 and len(lastscan) > 0:
			if thisproj in projscans:
				projscans[thisproj].append(lastscan)
			else:
				projscans[thisproj] = [lastscan]
		if final:
			thisproj = 'na'
	return projscans

def reverseMapProjs(projscans):
	'''Take a dict of {'proj':[scans]} and produce the reverse lookup {'scan<n>':'proj'}'''
	scanprojs = {}
	for proj in projscans:
		for scan in projscans[proj]:
			scanprojs[scan] = proj
	return scanprojs

class bcolors:
	'''Global struct with terminal color codes'''
	def __init__(self, useColors=True):
		self.useColor = useColors
		if useColors:
			self.on()
		else:
			self.off()
	def doColor(self):
		return self.useColor
	def on(self):
		self.DEFAULT = '\033[95m'
		self.GREEN = '\033[92m'
		self.ORANGE = '\033[93m'
		self.RED = '\033[91m'
		self.ENDC = '\033[0m'
	def off(self):
		self.DEFAULT = ''
		self.GREEN = ''
		self.ORANGE = ''
		self.RED = ''
		self.ENDC = ''

def checkDiFXViz(job, opts):
	'''Inspect the DiFX output of a job (<job>.difx/DIFX_*) and look at the polarizations of the visibility data'''

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
		polinfo = opts.colors.RED + 'mixed' + opts.colors.ENDC
	elif pols_circular:
		polinfo = 'circular'
	else:
		polinfo = 'linear'
	if pols_full:
		if pols_crossed:
			polinfo += ' full-Stokes'
		else:
			polinfo += ' half-Stokes'
	if not opts.doShort:
		polinfo += ' in ' + difxfilename
	return polinfo


def getScanInfo(job):
	'''Read scan information (source name, scan ID) for one job'''
	calcfile = job + '.calc'
	src = 'n/a'
	scan = 'n/a'
	with open(calcfile,'rt') as f:
		for line in f:
			if 'SOURCE' in line and 'NAME' in line:
				src = line.split(':')[-1].strip()
			if 'SCAN' in line and 'IDENTIFIER' in line:
				scan = line.split(':')[-1].strip()
	return src, scan


def reportOnLog(fn, opts):
	'''Load a polconvert log file and print a summary'''
	ratings = { 	'good':opts.colors.GREEN+'good'+opts.colors.ENDC,
			'poor':opts.colors.ORANGE+'poor'+opts.colors.ENDC,
			'bad':opts.colors.RED+'bad'+opts.colors.ENDC,
			1:'good', 2:'poor', 3:'bad' }
	nprinted, ngood, npoor, nbad, nerror = 0, 0, 0, 0, 0
	job = fn.split('.')[0]
	src, scan = getScanInfo(job)
	proj = ('proj=%-8s'%(opts.alma_projs[scan])) if opts.alma_projs else ''
	title = '%s %-8s %-10s %s' % (job,scan,src,proj)

	if not opts.doShort:
		print ('# %s' % (title))
		print ('#   %s' % (fn))

	f = open(fn, 'r')
	while True:
		l = f.readline()
		if len(l) < 1:
			break
		if ('ERROR' in l):
			if opts.doShort:
				print ('# %s : %s' % (title,l.strip()))
			else:
				print ('#  %s' % (l.strip()))
			nerror += 1
			continue
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
		min_par = min(RR,LL)
		max_cross = max(LR,RL)
		if (max_cross < 0.3*min_par):
			verdict = ratings['good']
			ngood += 1
		elif (max_cross < 0.6*min_par):
			verdict = ratings['poor']
			npoor += 1
		else:
			verdict = ratings['bad']
			nbad += 1
		if not opts.doShort:
			print ('#   IF#%d SNRs : RR %6.2f, LL %6.2f, LR %6.2f, RL %6.2f : %s' % (IF,RR,LL,RL,LR,verdict))
		nprinted += 1
	f.close()

	polinfo = ' '
	if opts.doPols:
		polinfo = ' : ' + checkDiFXViz(job, opts)

	if opts.doShort and nerror <= 0:
		overall = int( (ngood + 2.0*npoor + 3.0*nbad - 0.5) / 3.0 )
		rating_name = ratings[overall]
		print ('# %s%s : %d good, %d poor, %d bad : %s' % (title,polinfo,ngood,npoor,nbad,ratings[rating_name]))

	if opts.doPols and not opts.doShort:
		print('#   %s' % (polinfo))


if __name__ == "__main__":
	p = argparse.ArgumentParser(usage=__doc__)
	p.add_argument('-c', '--color', dest='colors', default=bcolors(False), action='store_const', const=bcolors(True), help='enable use of terminal color codes')
	p.add_argument('-p', '--pols', dest='doPols', default=False, action='store_true', help='inspect polarizations in visibility data')
	p.add_argument('-s', '--short', dest='doShort', default=False, action='store_true', help='condense the report')
	p.add_argument('-v', '--vex', dest='vexfile', default=None, help='a VEX file to parse for EHTC/GMVA project codes')

	opts = p.parse_args()
	opts.__dict__['alma_projs'] = reverseMapProjs(doFindProj(opts)) if opts.vexfile else None
	if opts.doPols and not have_ParseDiFX:
		print ('Warning: parseDiFX.py module not found, ignoring --pols command line argument')
		opts.doPols = False

	flist = glob.glob('*.polconvert-*/PolConvert.log')
	if len(flist) <= 0:
		print ('Warning: no polconverted jobs found in the current working directory!')

	flist.sort()
	for fn in flist:
		reportOnLog(fn, opts)
