#!/usr/bin/python
"""
Swap polarization labels at the given station(s).

The polarization swaps are performed by changing the polarization labels inside the
binary DiFX SWIN visibility data file, and accompanying PCal file(s) if present.

Output data are written into a new data set '<difx basename>swapped/{DIFX_*,PCAL_*}',
unless the option --inplace is used, which modifies the original data set directly.
"""

import argparse
import glob, sys, os
import shutil
import numpy
import parseDiFX

polswap = {'R':'L', 'L':'R', 'X':'Y', 'Y':'X', 'H':'V', 'V':'H'}
polnames = 'RLXYHV'

def polswapDifxFile(basename, targetAnts, doOverwrite=False, doPcal=True, verbose=False, restrictToFreqs_MHz=None):
	"""
	Swap polarization labels in a binary SWIN DiFX visibility data file
	"""
	global polswap, polnames

	nswapped, npassed = 0, 0

	# Determine name of .input file
	if basename.endswith(('.difx','.input','.calc','.im','.flag','.machines','.difxlog')):
		basename = basename[:basename.rfind('.')]
	pathless_basename = basename
	if basename.rfind('/')>=0:
		pathless_basename = basename[basename.rfind('/')+1:]
	inputfile = basename + '.input'

	# Get access to metadata
	difx = parseDiFX.DiFXFile(inputfile)
	if not difx.isvalid():
		print("Error: could not parse input file " + inputfile + " correctly")
		return
	cfg = difx.metainfo

	# Stop early if telescope has no data in this scan
	telescopenames = [t.name for t in cfg.telescopes]
	if not any(target in telescopenames for target in targetAnts):
		print ('Telescope(s) %s not among in stations %s of %s. Nothing to do!' % (str(targetAnts),str(telescopenames),basename)) 
		return

	# Prepare output file
	if not doOverwrite:
		difxoutdir = pathless_basename + 'swapped.difx'
		try:
			os.mkdir(difxoutdir)
		except:
			pass
		difxoutname = difxoutdir + '/' + difx.difxfilename[difx.difxfilename.rfind('/')+1:]
	else:
		difxoutdir = difx.difxfilename[0:difx.difxfilename.rfind('/')]
		difxoutname = str(difx.difxfilename) + '_polswapped'

	difxout = open(difxoutname, 'w')

	# Parse each visibility entry
	vr = difx.nextVisibilityRecord()
	while vr.isvalid():

		# Visibility properties
		h = vr.header
		rawvis = vr.vis

		ant1name = difx.getTelescope(h.antenna1 -1).name
		ant2name = difx.getTelescope(h.antenna2 -1).name
		origpols = str(h.polpair)

		# Pol-swap only certain frequencies?
		if restrictToFreqs_MHz:
			fq = difx.getFrequency(h.freqindex)
			freqMatch = False
			for [range_lo,range_hi] in restrictToFreqs_MHz:
				if fq.low_edge() >= range_lo and fq.high_edge() <= range_hi:
					freqMatch = True
					break
		else:
			freqMatch = True

		# Modify the header (polpair) if station matches
		if ((ant1name in targetAnts) or (ant2name in targetAnts)) and freqMatch:
			if ant1name in targetAnts:
				h.polpair = polswap[h.polpair[0]] + h.polpair[1]
			if ant2name in targetAnts:
				h.polpair = h.polpair[0] + polswap[h.polpair[1]]
			if verbose:
				print ("swap: %s-%s pol %s --> %s" % (ant1name,ant2name,origpols,h.polpair))
			nswapped += 1
		else:
			if verbose:
				print ("pass: %s-%s pol %s" % (ant1name,ant2name,h.polpair))
			npassed += 1

		# Convert header back to binary format
		binhdr = h.tobinary()

		difxout.write(binhdr)
		difxout.write(rawvis)

		vr = difx.nextVisibilityRecord()

	difxout.close()

	# Process PCal files
	if doPcal:

		pcalfiles = glob.glob(basename + '.difx/PCAL_*_*')
		npcalfound = len(pcalfiles)

		if not doOverwrite:
			for pcalfile in pcalfiles:
				shutil.copy2(pcalfile, difxoutdir + '/')

		npcalswapped = 0
		if npcalfound > 0:
			n = polswapPCalFiles(difxoutdir + '/', targetAnts, verbose)
			npcalswapped += n

	# In-place?
	if doOverwrite:
		difxoriginalname = str(difx.difxfilename)
		del difx # for closing the input file
		shutil.move(difxoutname, difxoriginalname)
		difxoutname = difxoriginalname

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. passed through : %d' % (npassed))
	print ('    vis. pol-swapped    : %d' % (nswapped))
	if doPcal and (npcalfound > 0):
		print ('    pc files pol-swapped: %d' % (npcalswapped))
	print ('\nOutput file:')
	print ('    visibility data    : %s' % (difxoutname))
	if doPcal and (npcalfound > 0):
		print ('    pcal data          : %s/PCAL_*' % (difxoutdir))
	print (' ')


def polswapPCalFiles(difxjobdir, targetAnts, verbose=False):
	"""
	In-place swap the polarization labels in ascii pcal files ([<path>/]<basename>.difx/PCAL_<mjd>_<station>)
	"""
	global polswap, polnames

	nswapped = 0

	for ant in targetAnts:

		globpattern = difxjobdir + '/PCAL_*_' + ant
		pcalfiles = glob.glob(globpattern)

		# print ('Looking for %s found %d files' % (globpattern, len(pcalfiles)))

		for pcalfile in pcalfiles:

			tempfile = difxjobdir + '/' + ant + '.pcaltmp'
			# print (ant, pcalfile, tempfile)

			fin = open(pcalfile, 'rt')
			lines = fin.readlines()
			fin.close()

			for n in range(len(lines)):

				if lines[n][0] == '#':
					continue

				items = lines[n].split(' ')  # note: separator provided here so join() can restore identical whitespace lengths
				for m in range(len(items)):
					if items[m] and items[m] in polnames:
						items[m] = polswap[items[m]]
				replacement = ' '.join(items)
				# todo: the above could perhaps be done more efficiently or more prettily

				# print ('before :', lines[n])
				# print ('after  :', replacement)
				lines[n] = replacement

			fout = open(tempfile, 'wt')
			fout.writelines(lines)
			fout.close()
			del fout

			shutil.move(tempfile, pcalfile)

		nswapped += len(pcalfiles)

	return nswapped


if __name__ == '__main__':

	parser = argparse.ArgumentParser(description=__doc__, add_help=True, formatter_class=argparse.RawDescriptionHelpFormatter)
	parser.add_argument("-f", "--freq-range", dest="freqRange", help="Limit polswapping to certain frequency range(s) in MHz (example: 8000-9000,10000-11000)")
	parser.add_argument("-i", "--inplace", action='store_true', help="Directly modify the input files, do not make a copy")
	parser.add_argument("station", help="Station(s) to polswap <station,[,station,station,...]> (example: Ys)")
	parser.add_argument("basename", nargs='+', metavar="basename", help="The DiFX job(s) to process (example: c221a_1021)")
	args = parser.parse_args()

	ants = [a.upper() for a in args.station.split(',')]
	if args.freqRange:
		args.freqRange = [[float(fq) for fq in entry.split('-')] for entry in args.freqRange.split(',')]

	for difxf in args.basename:
		polswapDifxFile(difxf, ants, args.inplace, doPcal=True, restrictToFreqs_MHz=args.freqRange)
