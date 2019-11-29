#!/usr/bin/python
"""
Usage: polswapDiFX.py <station[,station,station,...]> <difx basename>

Swaps the polarization labels for the given station.

Output:
  <difx basename>_swapped/DIFX_*
"""
import glob, sys, os
import numpy
import parseDiFX

"""Return the cross product of two sets"""
def setCrossProd(a,b):
	gen = ((x, y) for x in a for y in b)
	strconcatd = [ str(x)+str(y) for x,y in gen]
	return (strconcatd,gen)

def polswapVisibilityfile(basename,targetAnts):

	polswap = {'R':'L', 'L':'R', 'X':'Y', 'Y':'X', 'H':'V', 'V':'H'}

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	pathless_basename = basename
	if basename.rfind('/')>=0:
		pathless_basename = basename[basename.rfind('/')+1:]
	inputfile = basename + '.input'

	difx = parseDiFX.DiFXFile(inputfile)
	if not difx.isvalid():
		parser.error("Couldn't parse input file " + inputfile + " correctly")
	cfg = difx.metainfo

	# Stop early if telescope has no data in this scan
	telescopenames = [t.name for t in cfg.telescopes]
	if not any(target in telescopenames for target in targetAnts):
		print ('Telescope(s) %s not among in stations %s of %s. Nothing to do!' % (str(targetAnts),str(telescopenames),basename)) 
		return

	# Prepare output file
	difxoutdir = pathless_basename + 'swapped.difx'
	try:
		os.mkdir(difxoutdir)
	except:
		pass
	difxoutname = difxoutdir + '/' + difx.difxfilename[difx.difxfilename.rfind('/')+1:]
	difxout = open(difxoutname, 'w')

	# Parse each visibility entry
	nswapped = 0
	npassed = 0
	vr = difx.nextVisibilityRecord()
	while vr.isvalid():

		# Visibility properties
		h = vr.header
		fq = difx.getFrequency(h.freqindex)
		ant1name = difx.getTelescope(h.antenna1 -1).name
		ant2name = difx.getTelescope(h.antenna2 -1).name
		nchan = fq.numchan / fq.specavg
		origpols = h.polpair

		# Modify the header (polpair) if station matches
		if (ant1name in targetAnts) and (ant2name in targetAnts):
			# Auto-corrs, or both station swaps
			h.polpair = polswap[h.polpair[0]] + polswap[h.polpair[1]]
			binhdr = parseDiFX.make_output_header_v1(h)
			#print ("swap: %s-%s pol %s --> %s" % (ant1name,ant2name,origpols,h.polpair))
			nswapped += 1
		elif ant1name in targetAnts:
			# Cross-corr <station>x<any>
			h.polpair = polswap[h.polpair[0]] + h.polpair[1]
			binhdr = parseDiFX.make_output_header_v1(h)
			#print ("swap: bl %d %s-%s pol %s --> %s" % (baseline,ant1name,ant2name,origpols,h.polpair))
			nswapped += 1
		elif ant2name in targetAnts:
			# Cross-corr <any>x<station>
			h.polpair = h.polpair[0] + polswap[h.polpair[1]]
			binhdr = parseDiFX.make_output_header_v1(h)
#			print ("swap: bl %d %s-%s pol %s --> %s" % (baseline,ant1name,ant2name,origpols,h.polpair))
			nswapped += 1
		else:
#			print ("pass: %s-%s pol %s" % (ant1name,ant2name,h.polpair))
			binhdr = parseDiFX.make_output_header_v1(h)
			npassed += 1

		difxout.write(binhdr)
		difxout.write(rawvis)

		vr = difx.nextVisibilityRecord()

	difxout.close()

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. passed through : %d' % (npassed))
	print ('    vis. pol-swapped    : %d' % (nswapped))
	print ('\nOutput file:')
	print ('    visbility data     : %s' % (difxoutname))
	print (' ')


if __name__ == '__main__':

	if len(sys.argv) < 3:
		print (__doc__)
		sys.exit(-1)

	ants = sys.argv[1].upper()
	ants = [a.upper() for a in ants.split(',')]
	for difxf in sys.argv[2:]:
		polswapVisibilityfile(difxf,ants)
