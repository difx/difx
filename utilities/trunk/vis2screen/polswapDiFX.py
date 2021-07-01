#!/usr/bin/python
"""
Usage: polswapDiFX.py [--inplace] <station[,station,station,...]> <difx basename>

Swaps the polarization labels for the given station(s).

Polarization swaps are performend on the labels in the binary DiFX SWIN visibility data.
The script will retain the original SWIN file and produce a pol-swapped copy, unless the
option --inplace is used which carries out the swap on the original data.

Output file without --inplace option:
  <difx basename>_swapped/DIFX_*
"""

import glob, sys, os
import shutil
import numpy
import parseDiFX


def polswapVisibilityfile(basename, targetAnts, doOverwrite=False, verbose=False):
	"""
	Swap polarization labels in a binary SWIN DiFX visibility data file
	"""

	polswap = {'R':'L', 'L':'R', 'X':'Y', 'Y':'X', 'H':'V', 'V':'H'}
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

		# Modify the header (polpair) if station matches
		if (ant1name in targetAnts) or (ant2name in targetAnts):
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

	if doOverwrite:
		difxoriginalname = str(difx.difxfilename)
		del difx # for closing the input file
		shutil.move(difxoutname, difxoriginalname)
		difxoutname = difxoriginalname

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. passed through : %d' % (npassed))
	print ('    vis. pol-swapped    : %d' % (nswapped))
	print ('\nOutput file:')
	print ('    visbility data     : %s' % (difxoutname))
	print (' ')


if __name__ == '__main__':

	args = sys.argv[1:]
	doOverwrite = False

	if len(args) < 2 or args[0] in ['-h','--help']:
		print (__doc__)
		sys.exit(-1)

	if args[0] == '--inplace':
		doOverwrite = True
		args = args[1:]

	ants = args[0].upper()
	ants = [a.upper() for a in ants.split(',')]

	for difxf in args[1:]:
		polswapVisibilityfile(difxf,ants,doOverwrite)
