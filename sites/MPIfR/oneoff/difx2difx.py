#!/usr/bin/python
"""
Usage: difx2difx.py <difx basename>

Stitches together in frequency domain the visibility data on the
baselines of a certain hard-coded antenna, in a hard-coded fashion.
Basically pieces together 32 MHz wide bands from shorter subbands
on all baselines to ALMA as produced by DiFX for a specific zoomband set.

Output:
  <difx basename>/DIFX_*.stitched     frequency stitched visibility data
  <difx basename>/DIFX_*.ref_input    entries to use in a replacement .input file

"""
import glob, sys, os, struct, time, math, numpy
import parseDiFX
from optparse import OptionParser
from numpy import fft

"""Return the cross product of two sets"""
def setCrossProd(a,b):
	gen = ((x, y) for x in a for y in b)
	strconcatd = [ str(x)+str(y) for x,y in gen]
	return (strconcatd,gen)

"""Check if a frequency falls into one band in a set of (global) frequency bands"""
def getGlueIndex(f):
	global stitch_basefreqs, stitch_endfreqs
	for n in range(len(stitch_basefreqs)):
		if (f >= stitch_basefreqs[n]) and (f <= stitch_endfreqs[n]):
			return n
	return -1

"""Read next DiFX file visibility header and return it in binary was well as a parsed struct"""
def getVisibilityHeader(f):
	offset = f.tell()
	h = parseDiFX.parse_output_header(f)
	rdlen = f.tell() - offset
	f.seek(offset)
	bin = f.read(rdlen)
	return (h,bin)

###########################################################################################################
# Hardcoded baseband info
target_bw = 32.0000000 # MHz
target_nchan = 4096
target_nstokes = 4
pols_list = ['R','L','X','Y']
# Hardcoded frequencies to glue together on ALMA x others baselines:
stitch_basefreqs = [86476.00, 86412.00, 86380.00, 86316.00, 86252.00, 86188.00, 86124.00, 86060.00, 86028.00] # MHz
stitch_endfreqs = [f + target_bw for f in stitch_basefreqs]
stitch_bandIDs = [n for n in range(len(stitch_basefreqs))]
stitch_antenna = 'AA'
###########################################################################################################

if len(sys.argv) != 2:
	print __doc__
	sys.exit(-1)

# All polarisation pairs 'RR', 'RL', ... to 'YY'
polpairs,tmp = setCrossProd(pols_list, pols_list)

# Dictionary of per-polarizationpair working buffers into which spectra are stitched
stitch_workbufs = { polkey:{ bandkey:(' '*8*target_nchan) for bandkey in stitch_bandIDs } for polkey in polpairs }
stitch_chcounts = { polkey:{ bandkey:0 for bandkey in stitch_bandIDs } for polkey in polpairs }
stitch_timestamps = {  polkey:{ bandkey:-1 for bandkey in stitch_bandIDs } for polkey in polpairs }
freqIDs_stitched = []  # the last freqID out of each freq group stitched, useful to "fix" .INPUT file 'NUM CHANNELS <freqID> : <target_nchan>'

# Extract meta-infos from the DiFX .INPUT file
basename = sys.argv[1]
inputfile = basename + '.input'
difxfileslist = glob.glob(basename + '.difx/DIFX_*.s*.b*')
(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
    parser.error("Couldn't parse input file " + inputfile + " correctly")

# Read the DiFX .difx/DIFX_* file
difxfilename = difxfileslist[0]
difxfile = open(difxfilename, 'r')
difxout = open(difxfilename+'.stitched', 'w')
templateinp = open(difxfilename+'.ref_input', 'w')
(vishdr,binhdr) = getVisibilityHeader(difxfile)

# Parse each visibility entry
seconds_prev = -1
while len(vishdr) > 0:

	# Visibility properties
	baseline = vishdr[0]
	mjd = vishdr[1]
	seconds = vishdr[2]
        freqindex = vishdr[5]
        polpair = vishdr[6]
	weight = vishdr[8]
	uvw = vishdr[9:12]
	ant1 = baseline % 256
    	ant2 = (baseline-ant1)/256
	ant1name = telescopes[ant1-1].name
	ant2name = telescopes[ant2-1].name
	T = mjd + seconds/86400.0

	# Number of channels in this baseband
        nchan = freqs[freqindex].numchan / freqs[freqindex].specavg
	fsky = freqs[freqindex].freq
	bw = freqs[freqindex].bandwidth

	# Read the entire visibility data from disk
	rawvis = difxfile.read(8*nchan)
	visdata = numpy.fromstring(rawvis, dtype='complex64')

	if (seconds != seconds_prev):
		print (('\n---- %d %12.7f ' + '-'*115) % (mjd,seconds))
		seconds_prev = seconds

	# Write out visibility to output file
	if (ant1name == stitch_antenna) or (ant2name == stitch_antenna):

		# Handle glueing; TODO
		if (bw == target_bw):

			# Visibility already at target bandwidth, just copy the data
			print ('take  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))
			difxout.write(binhdr)
			difxout.write(rawvis)

			gi = getGlueIndex(fsky)
			if (gi >= 0):
				stitch_timestamps[polpair][gi] = seconds

		else:

			# Need to stitch narrowband visibility into wideband context
			gi = getGlueIndex(fsky)
			if (gi < 0):
				print ('Crap.. could not locate %.6f MHz! Check hard-coded frequency table...' % (fsky))
				sys.exit(0)

			if (stitch_timestamps[polpair][gi] < 0):
				stitch_timestamps[polpair][gi] = seconds
			if (seconds != stitch_timestamps[polpair][gi]):
				N = stitch_chcounts[polpair][gi]
				if (N != 0) and (N < target_nchan):
					print ('Warning: stitch of %d/%s not complete (%d out of %d channels)!' % (gi,polpair,N,target_nchan))
				print (('---- %d %.7f ' + '-'*80 + '\n') % (mjd,seconds))
				stitch_timestamps[polpair][gi] = seconds
				stitch_chcounts[polpair][gi] = 0
				stitch_workbufs[polpair][gi] = ' '*8*target_nchan

			choffset = ((fsky - stitch_basefreqs[gi]) / target_bw) * target_nchan

			# TODO: insert the data!!
			# stitch_workbufs[polpair][choffset:(choffset+nchan)] = rawvis # TODO; TypeError: 'str' object does not support item assignment
			# print (' glue %s band %d : ch %4d ... %4d : %d new chs : stitched %d ch total' % (polpair,gi,choffset,choffset+nchan-1,nchan,stitch_chcounts[polpair][gi]))
			stitch_chcounts[polpair][gi] += nchan

			if stitch_chcounts[polpair][gi] >= target_nchan:
				#print (' stitch of %d/%s complete' % (gi,polpair))
				TS = mjd + stitch_timestamps[polpair][gi] / 86400.0
				print ('stitch: %s-%s/%d(%d):%.4f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s'
					% (ant1name,ant2name,freqindex,gi,fsky,polpair,TS,stitch_chcounts[polpair][gi],target_bw,str(uvw)))
				difxout.write(binhdr)  # TODO: any need to tamper with the header? perhaps the Freq ID?
				difxout.write(stitch_workbufs[polpair][gi])
				stitch_chcounts[polpair][gi] = 0
				stitch_workbufs[polpair][gi] = ' '*8*target_nchan

				# Grow the template .INPUT file
				if not (freqindex in freqIDs_stitched):
					templateinp.write('NUM CHANNELS %2d: %7d\n' % (freqindex,target_nchan))
					freqIDs_stitched.append(freqindex)

	else:

		if (bw == target_bw):
			# Just copy out the data
			print ('copy  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))
			difxout.write(binhdr)
			difxout.write(rawvis)
		else:
			# Zoomband on non-ALMA baseline, ignore it because it is covered
			# by some other complete zoomband that doesn't need stitching
			print ('skip  : %s-%s/%d:%.7f/%s  mjd:%12.8f nchan:%d bw:%.7f uvw=%s' % (ant1name,ant2name,freqindex,fsky,polpair,T,nchan,bw,str(uvw)))

	# Next header
	(vishdr,binhdr) = getVisibilityHeader(difxfile)

print ('\nDone! Output files:')
print ('    visbility data     : %s' % (difxfilename+'.stitched'))
print ('    changes for .input : %s' % (difxfilename+'.ref_input'))
