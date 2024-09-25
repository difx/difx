#!/usr/bin/python3
"""
Usage: polrelabelDiFX.py [--inplace] [--swap] <station[,station,...]> <difx basename> [<difx basename> ...]

Changes linear polarization labels to circular in visibility records
of baselines with the listed station(s).

The original SWIN data are retained intact and a new SWIN visibility data set is output,
unless the --inplace option is specified, in which case changes are carried out on the
original file.  The default is X => RCP, Y => LCP, but Y => RCP, X => LCP with --swap.

Output file:
  <difx basename>_polrelabeled/DIFX_*
    or
  <difx basename>/DIFX_*  when --inplace
"""

import glob, sys, os
import shutil
import numpy
import parseDiFX


def polrelabelVisibilityfile(basename, targetAnts, doOverwrite=False, verbose=False, swap=False):
	"""
	Relabel polarization labels in a binary SWIN DiFX visibility data file
	Currently relabels just ALMA X => RCP, Y => LCP
	"""

	polrelabel = {'X':'R', 'Y':'L', 'R':'R', 'L':'L' }
	nrelabeled, npassed = 0, 0

	if swap:
		polrelabel['X'] = 'L'
		polrelabel['Y'] = 'R'

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
		difxoutdir = pathless_basename + 'relabeled.difx'
		try:
			os.mkdir(difxoutdir)
		except:
			pass
		difxoutname = difxoutdir + '/' + difx.difxfilename[difx.difxfilename.rfind('/')+1:]
	else:
		difxoutdir = difx.difxfilename[0:difx.difxfilename.rfind('/')]
		difxoutname = str(difx.difxfilename) + '_polrelabeled'
	difxout = open(difxoutname, 'wb')

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
			old_polpair = str(h.polpair)
			if ant1name in targetAnts:
				h.polpair = polrelabel[h.polpair[0]] + h.polpair[1]
			if ant2name in targetAnts:
				h.polpair = h.polpair[0] + polrelabel[h.polpair[1]]
			if h.polpair != old_polpair:
				nrelabeled += 1
				if verbose:
					print ("relabel: %s-%s pol %s --> %s" % (ant1name,ant2name,origpols,h.polpair))
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
	print ('    vis. pol-relabeled  : %d' % (nrelabeled))
	print ('\nOutput file:')
	print ('    visbility data     : %s' % (difxoutname))
	print (' ')


if __name__ == '__main__':

	args = sys.argv[1:]
	doOverwrite = False
	doVerbose = False
	doSwap = False

	if len(args) < 2 or args[0] in ['-h','--help']:
		print (__doc__)
		sys.exit(-1)

	while args[0][0] == '-':
		if args[0] == '--inplace':
			doOverwrite = True
			args = args[1:]
		elif args[0] == '-v':
			doVerbose = True
			args = args[1:]
		elif args[0] == '--swap':
			doSwap = True
			args = args[1:]

	ants = args[0].upper()
	ants = [a.upper() for a in ants.split(',')]

	for difxf in args[1:]:
		polrelabelVisibilityfile(difxf,ants,doOverwrite,doVerbose,doSwap)
