#!/usr/bin/python
"""
Usage: polswapDiFX.py <station[,station,station,...]> <difx basename>

Swaps the polarization labels for the given station.

Output:
  <difx basename>_swapped/DIFX_*

"""
import glob, sys, os
import parseDiFX

"""Return the cross product of two sets"""
def setCrossProd(a,b):
	gen = ((x, y) for x in a for y in b)
	strconcatd = [ str(x)+str(y) for x,y in gen]
	return (strconcatd,gen)

"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
def getVisibilityHeader(f):
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)

def polswapVisibilityfile(basename,targetAnts):

	polswap = {'R':'L', 'L':'R', 'X':'Y', 'Y':'X'}

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	pathless_basename = basename
	if basename.rfind('/')>=0:
		pathless_basename = basename[basename.rfind('/')+1:]
	inputfile = basename + '.input'
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		parser.error("Couldn't parse input file " + inputfile + " correctly")

	# Stop early if telescope has no data in this scan
	telescopenames = [t.name for t in telescopes]
	if not any(target in telescopenames for target in targetAnts):
		print ('Telescope(s) %s not among in stations %s of %s. Nothing to do!' % (str(targetAnts),str(telescopenames),basename)) 
		return

	# Read the DiFX .difx/DIFX_* file
	difxfileslist = glob.glob(basename + '.difx/DIFX_*.s*.b*')
	difxfilename = difxfileslist[0]
	difxfile = open(difxfilename, 'r')
	difxoutdir = pathless_basename + 'swapped.difx'
	try:
		os.mkdir(difxoutdir)
	except:
		pass
	difxoutname = difxoutdir+'/'+difxfilename[difxfilename.rfind('/')+1:]
	difxout = open(difxoutname, 'w')
	(vishdr,binhdr) = getVisibilityHeader(difxfile)

	# Parse each visibility entry
	nswapped = 0
	npassed = 0
	while len(vishdr) > 0:

		# Visibility properties
		baseline = vishdr[0]
		freqindex = vishdr[5]
		polpair = vishdr[6]

		# Antenna order as in difx2mark4: ref=ant1="256*nr", rem=ant2="nr%256"
		# since if using opposite order (like in DiFX python utils) get "missing autocorr" complaint from difx2mark4
		# and final pols look wrong; with the below order the baseline pols look correctly swapped (in fourfit, fplot)
		ant2 = baseline % 256
		ant1 = (baseline-ant2)/256

		ant1name = telescopes[ant1-1].name
		ant2name = telescopes[ant2-1].name
                seconds = vishdr[2]

		# Number of channels in this baseband
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg

		# Read the entire visibility data from disk
		rawvis = difxfile.read(8*nchan)

		# Modify the header (polpair) if station matches
		if (ant1name in targetAnts) and (ant2name in targetAnts):
			# Auto-corrs, or both station swaps
			vishdr[6] = polswap[polpair[0]] + polswap[polpair[1]]
                        binhdr = parseDiFX.make_output_header_v1(vishdr)
			#print ("swap: %s-%s pol %s --> %s" % (ant1name,ant2name,polpair,vishdr[6]))
			nswapped += 1
		elif ant1name in targetAnts:
			# Cross-corr <station>x<any>
			vishdr[6] = polswap[polpair[0]] + polpair[1]
                        binhdr = parseDiFX.make_output_header_v1(vishdr)
			#print ("swap: bl %d %s-%s pol %s --> %s" % (baseline,ant1name,ant2name,polpair,vishdr[6]))
			nswapped += 1
		elif ant2name in targetAnts:
			# Cross-corr <any>x<station>
			vishdr[6] = polpair[0] + polswap[polpair[1]]
                        binhdr = parseDiFX.make_output_header_v1(vishdr)
#			print ("swap: bl %d %s-%s pol %s --> %s" % (baseline,ant1name,ant2name,polpair,vishdr[6]))
			nswapped += 1
		else:
#			print ("pass: %s-%s pol %s" % (ant1name,ant2name,polpair))
			npassed += 1

		difxout.write(binhdr)
		difxout.write(rawvis)

		(vishdr,binhdr) = getVisibilityHeader(difxfile)

	difxout.close()

	# Finished
	print ('\nDone! Final statistics:')
	print ('    vis. passed through : %d' % (npassed))
	print ('    vis. pol-swapped    : %d' % (nswapped))
	print ('\nOutput file:')
	print ('    visbility data     : %s' % (difxoutname))
	print (' ')

if len(sys.argv) < 3:
	print __doc__
	sys.exit(-1)

ants = sys.argv[1].upper()
ants = [a.upper() for a in ants.split(',')]
for difxf in sys.argv[2:]:
	polswapVisibilityfile(difxf,ants)

