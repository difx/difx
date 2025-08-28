#!/usr/bin/python
"""
Usage: polswapDiFX.py [--inplace] <station[,station,station,...]> <difx basename>

Swaps the polarization labels for the given station(s).

Polarization swaps are performend on the labels in the binary DiFX SWIN visibility data,
as well as in the DiFX text format PCal file data if present.

The script will retain the original SWIN .difx and produce a pol-swapped copy, unless the
option --inplace is used which carries out the swap on the original data.

Output file without --inplace option:
  <difx basename>swapped/DIFX_*
"""

import glob, sys, os
import shutil
import numpy
import parseDiFX

polswap = {'R':'L', 'L':'R', 'X':'Y', 'Y':'X', 'H':'V', 'V':'H'}
polnames = 'RLXYHV'

def polswapDifxFile(basename, targetAnts, doOverwrite=False, doPcal=True, verbose=False):
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
		polswapDifxFile(difxf,ants,doOverwrite,doPcal=True)
