#!/usr/bin/python
"""
Usage: stripantennaDiFX.py [-r|--remove <antennaslist>] [-k|--keep <antennaslist>] <difx basename> [<difx basename> ...]

Copies visibility data from DiFX .difx datasets to new output dataset(s), while removing or keeping
visibility data on baselines to certain antennas during the copying process.

Options:
    --remove <antennas>  all visibilities are copied, except for visibilities on baseline(s) to certain antennas
    --keep <antennas>    no visibilities are copied, except for visibilities on baseline(s) to certain antennas
    <antennas>           a comma separated list of antenna names in upper case, e.g., EB,PV,MH

Output:
  <difx basename>_stripped/DIFX_*

"""
import glob, sys, os, shutil
import parseDiFX

"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
def getVisibilityHeader(f):
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)

"""Process one or more files"""
def mergeDiFX(basename,antRemove,antKeep):

	# Derive file names
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	pathless_basename = basename
	if pathless_basename.rfind('/') >= 0:
		pathless_basename = pathless_basename[pathless_basename.rfind('/')+1:]
	difxoutdir = pathless_basename + '_stripped.difx'

	# Open DiFX files
	difxfilename = glob.glob(basename + '.difx/DIFX_*.s*.b*')[0]
	difxfile = open(difxfilename, 'r')
	try:
		os.mkdir(difxoutdir)
	except:
		pass
	difxoutname = difxoutdir + '/' + difxfilename[difxfilename.rfind('/')+1:]
	difxout = open(difxoutname, 'w')
	print ('Source       : %s' % (difxfilename))
	print ('Destination  : %s' % (difxoutname))

	# Extract meta-infos from the DiFX .INPUT file
	inputfile = basename + '.input'
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		parser.error("Couldn't parse input file " + inputfile + " correctly")

	# Copy/strip
	Ncopied = 0
	Ndropped = 0
	while True:

		(vishdr,binhdr) = getVisibilityHeader(difxfile)
		if len(vishdr) <= 0:
			break

		# Visibility properties
		baseline = vishdr[0]
		seconds = vishdr[2]
		freqindex = vishdr[5]
		polpair = vishdr[6]

		# Antenna order as in difx2mark4: ref=ant1="256*nr", rem=ant2="nr%256"
		ant2 = baseline % 256
		ant1 = (baseline-ant2)/256
		ant1name = telescopes[ant1-1].name.upper()
		ant2name = telescopes[ant2-1].name.upper()

		# Number of channels in this baseband
		nchan = freqs[freqindex].numchan / freqs[freqindex].specavg

		# Read the entire visibility data from disk
		rawvis = difxfile.read(8*nchan)
		if len(rawvis) < 8*nchan:
			continue

		# Copy to output or discard?
		copy = True
		if (len(antRemove) > 0) and (ant1name in antRemove) or (ant2name in antRemove):
			copy = False
		elif (len(antKeep) > 0) and not ((ant1name in antKeep) or (ant2name in antKeep)):
			copy = False
		#print ('  %s-%s : keep=%s' % (ant1name,ant2name,str(copy)))
		if not copy:
			Ndropped += 1
		else:
			difxout.write(binhdr)
			difxout.write(rawvis)
			Ncopied += 1

	difxout.close()

	# Finished
	print ('Vis. dropped : %d' % (Ndropped))
	print ('Vis. copied  : %d' % (Ncopied))

# Cmd line args
args = sys.argv[1:]
antRemove = []
antKeep = []
while len(args) > 0:
	if (args[0] == '--keep') or (args[0] == '-k'):
		antKeep = args[1].split(',')
		args = args[2:]
	elif (args[0] == '--remove') or (args[0] == '-r'):
		antRemove = args[1].split(',')
		args = args[2:]
	elif (args[0][:2] == '--'):
		print (__doc__)
		sys.exit(-1)
	break
if len(args) < 1:
	print (__doc__)
	sys.exit(-1)

print ('Keeping %s and removing %s' % (str(antKeep),str(antRemove)))
for basename in args:
	mergeDiFX(basename, antRemove, antKeep)
