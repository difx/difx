#!/usr/bin/python
'''
Usage: autoBands.py <casename | vexfile.vex> [<output_bandwidth_MHz>]

Automatic "output bands" generation based upon sets
of recorded bands per antenna. A test case name
has to be specified, or alternatively a VEX file
from which the recorded bands are then extracted.

Reference implementation for a later native C/C++ port.

01/2019 Jan Wagner
'''
'''
The approach is as follows:

 == Stage 1 ==
 Sky freq coverage segmented into smaller freq spans

 1.1  gather info about all recorded bands,
      which antenna/$FREQ provides what set of recorded bands
 1.2  build a list of low and high edge frequencies of all bands
 1.3  form a set out of (1.2) and sort it, placing in effect many
      'divider marks' across the sky freq coverage of the experiment
 1.4  the divider marks yield a list of frequency "spans" defined
      by a low edge frequency and delta to next span

 == Stage 2 ==
 Discard unusable spans

 2.1  filter the list of frequency "spans", discard all spans
      that are not available from indeed every antenna/$FREQ
 2.2  go through the now shorter list of still retained spans
      a) note if now any band break between a span and the next
      b) count of number of recorded bands that provide the span

      With overlapped rec bands (ALMA) the count 2.2b will exceed
      the number of antennas/$FREQ. This redundancy can aid outputband
      placing.

 == Stage 3 ==
 Place outputbands across spans

 3.1  note the user-requested output bandwidth or
      determine a best suitable bandwidth from info (1.1)
 3.2  set start freq to beginning of first span,
      possibly adjusting it upwards if this helps reduce FFT length
 3.3  fill the span with outputbands (1:1 association between recorded:output)
 3.4  take remaining bandwidth in the span and if by (2.2a) the current
      span is contiguous to the next, flow over to that next span(s) to
      continue on the same outputband (N:1 association between spans:output)
 3.5  for the N:1 association (3.4), loop through the N spans and attempt
      to merge down to M underlying recorded bands when overlap (2.2b) allows
 3.6  clean up final list of outputbands and the band slices needed to assemble them

 == Stage 4 ==
 Build 'instructions' for correlation

 4.1  translate outputbands list into some format useful for correlation
      - per recorded band, keep a list of band slices to come from that rec band
      - per band slice, keep reference to right spot in final destination outputband
      - write out band slice info as a list of DiFX 2.6 ZOOM -equivalent bands,
        to reuse a lot of the zoom band facility

 During actual correlation, after mode.cpp has made freq domain data available,
 handle zoom bands mostly as before. Accumulate zoom content into its location 
 in the referenced destination outputband.

 May need v2d 'loOffsets=f1,f2,f3,...<Hz>' to phase-align the recorded bands
 of an antenna. May need new v2d 'freqGains=g1,g2,g3,..' if digital backends
 output non-flat/sloped bands.

'''

import math, fractions, os, sys
import vex  # same as utilized by autozooms module

verbosity = 3

class Autobands:
	"""Automatic output bands generation based upon a set of recorded frequencies"""

	def __init__(self, outputbandwidth=16e6, verbosity=0, permitgaps=False):
		self.flow = []
		self.fhigh = []
		self.fantenna = []
		self.Nant = 0
		self.verbosity = verbosity
		self.permitGaps = permitgaps
		self.detectedspans = []
		self.outputbandconstituents = []
		self.setBandwidth(outputbandwidth)

	def add(self, flowhigh):
		"""Add antenna with freqs ([band low edges], [band high edges])"""
		self.flow += list(flowhigh[0])
		self.fhigh += list(flowhigh[-1])
		self.fantenna += [self.Nant] * len(flowhigh[0])
		self.Nant += 1

	def addBand(self, flo, fhi):
		"""Add antenna with single band"""
		self.flow.append(flo)
		self.fhigh.append(fhi)
		self.fantenna.append(self.Nant)
		self.Nant += 1

	def setBandwidth(self, outputbandwidth_Hz):
		self.outputbw = outputbandwidth_Hz
		if self.outputbw < 1e6:
			print ('Warning: Autobands() instantiated with small output bandwidth %.3f Hz, treating it as MHz' % (outputbandwidth_Hz))
			self.outputbw = outputbandwidth*1e6

	def genPFB(self, fbase_Hz, bw, fstep, Nch, sb=+1):
		"""Generate frequency list for uniform PFB"""
		f1 = [fbase_Hz + sb*fstep*ch for ch in range(0,Nch)]
		f2 = [fch + sb*bw for fch in f1]
		if sb>0:
			return f1,f2
		return f2,f1

	def genALMA(self, fbase_Hz, sb=+1):
		"""Generate ALMA"""
		Nch = 32
		df = 3125
		dband = df * 18750
		bw = df * 20000
		return self.genPFB(fbase_Hz, bw, dband, Nch, sb)
	
	def genNOEMA(self, fbase_Hz, sb=+1):
		"""Generate NOEMA"""
		bw = 64e6	# PolyFix always 64 MHz
		Nch = 64	# PolyFix always 64 ch
		return self.genPFB(fbase_Hz, bw, bw, Nch, sb)

	def genVLBA(self, fbase_Hz, sb=+1):
		"""Generate VLBA 2 x 128 MHz"""
		bw = 128e6
		Nch = 2
		return self.genPFB(fbase_Hz, bw, bw, Nch, sb)
	
	def genGMVA(self, fbase_Hz, sb=+1):
		"""Generate GMVA in PFB 32 MHz mode"""
		Nch = 8		# generic 8-ch
		bw = 32e6	# generic 32 MHz
		return self.genPFB(fbase_Hz, bw, bw, Nch, sb)

	def genEHT(self, fbase_Hz, sb=+1):
		"""Generate EHT with 1 x 2048 MHz"""
		bw = 2048e6	# R2DBE 1 ch x 2048 MHz
		return self.genPFB(fbase_Hz, bw, bw, 1, sb)

	def genDBBC3(self, fbase_Hz, sb=+1):
		"""Generate DBBC in PFB 32 MHz mode"""
		bw = 32e6	# 32 MHz bands, USB&LSB pairs spaced 64 MHz
		Nch = 2*32	# 32 ch x 2 sideband
		return self.genPFB(fbase_Hz-bw, bw, bw, Nch, sb)

	def addVEX(self, vexfilename):
		"""Add all frequency setups (whether actually used or not!) from a VEX file"""
		f = open(vexfilename, 'r')
		v = vex.parse(f.read())
		f.close()
		# todo: build list of freq names by going through v['SCHED'] scan modes per antenna, 
		# adding freqs from those modes to the list, then use list instead of all v['FREQ'].keys()
		for fqname in v['FREQ']:
			bandlist = [[], []]
			for chinfo in v['FREQ'][fqname].getall('chan_def'):
				f, sb, bw = [s.upper() for s in chinfo[1:4]]
				f_Hz = float(f.split()[0])
				bw_Hz = float(bw.split()[0])
				if 'MHZ' in f:
					f_Hz *= 1e6
				elif 'GHZ' in f:
					f_Hz *= 1e9
				if 'MHZ' in bw:
					bw_Hz *= 1e6
				elif 'GHZ' in bw:
					bw_Hz *= 1e9
				if 'L' in sb:
					f0, f1 = f_Hz-bw_Hz, f_Hz
				else:
					f0, f1 = f_Hz, f_Hz+bw_Hz
				if f0 not in bandlist[0]:
					bandlist[0].append(f0)
					bandlist[1].append(f1)
			if bandlist[0]:
				self.add(bandlist)
			if self.verbosity > 0:
				print ('Autobands::addVEX() found %d recorded bands in VEX Freq definition %s' % (len(bandlist[0]), fqname))

	def bandsHaveRange(self, f0, f1):
		"""Check if freq range falls in its entirety inside any of the recorded bands, at *all* antennas"""
		antennas = []
		for (lo,hi,ant) in zip(self.flow,self.fhigh,self.fantenna):
			if lo <= f0 and f1 <= hi:
				antennas.append(ant)
		return (len(set(antennas)) >= self.Nant)

	def granularity(self, freqs):
		"""Return the greatest-common-divisor for a list of frequencies and bandwidths"""
		if len(freqs) < 1:
			return 1.0
		df = reduce(fractions.gcd, freqs)
		return df

	def autobandwidth(self):
		"""Return automatically determined best-fit common bandwidth, or None on failure"""
		if len(self.detectedspans) < 1:
			self.detectedspans = self.spans()
		(fmin,fmax,minbw,maxbw,df) = self.statistics(self.detectedspans['flow'], self.detectedspans['fhi'])
		#print('autobandwidth: %.3fM %.3fM %.3fk' % (minbw*1e-6,maxbw*1e-6,df*1e-3))
		if minbw == maxbw:
			return minbw
		return None

	def statistics(self, flow=None, fhigh=None):
		"""Return min and max frequencies and common minimum freq spacing. Returns (fmin,fmax,df)."""
		if not flow:
			flow, fhigh = self.flow, self.fhigh
		fqall = list(flow) + list(fhigh)
		if len(fqall) < 1:
			return (None)*5
		bw = [fhigh[n]-flow[n] for n in range(len(flow))]
		minbw = reduce(min, bw)
		maxbw = reduce(max, bw)
		fmin = reduce(min, fqall)
		fmax = reduce(max, fqall)
		fqall.append(self.outputbw)
		fqall += bw
		df = self.granularity(fqall)
		return (fmin,fmax,minbw,maxbw,df)

	def spans(self, Nant=None):
		"""Determine frequency spans where at least Nant antennas overlap.
		Spans are the frequency axis split at the band edges of recorded bands of every antenna.
		Spans without freq gaps to the next span are marked as continued.
		@return dict {'flow':[float list], 'fhi':[float list], 'continued':[bool list], 'bandcounts':[int list]}
		"""
		if not Nant:
			Nant = self.Nant

		# List of all recorded band edges
		Nfq = len(self.flow)
		fedges = list(set(self.flow + self.fhigh))
		fedges.sort()

		# Split the freq axis into spans (smallest band slices between any two rec band edges)
		spans = {'flow':[], 'fhi':[], 'bandcounts':[], 'continued':[]}
		for n in range(len(fedges)-1):

			span_lo, span_hi, count = fedges[n], fedges[n+1], 0

			# Count how many antennas have this span
			ants_in_span = []
			for k in range(Nfq):
				if span_lo >= self.flow[k] and span_hi <= self.fhigh[k]:
					ants_in_span.append(self.fantenna[k])
			antcount = len(set(ants_in_span))
			bandcount = len(ants_in_span)

			# Enough antennas have it, keep span?
			if antcount >= Nant and bandcount >= Nant:
				spans['flow'].append( span_lo )
				spans['fhi'].append( span_hi )
				spans['bandcounts'].append( bandcount )
				if self.verbosity > 2:
					print ('Autobands::spans() retain  %.6f--%.6f MHz bw %.6f MHz with %d rec bands, %d antennas' % (span_lo*1e-6,span_hi*1e-6,(span_hi-span_lo)*1e-6,bandcount,antcount))
			else:
				if self.verbosity > 2:
					print ('Autobands::spans() discard %.6f--%.6f MHz bw %.6f MHz with %d rec bands, %d antennas < %d antennas' % (span_lo*1e-6,span_hi*1e-6,(span_hi-span_lo)*1e-6,bandcount,antcount,Nant))

		# Mark directly adjecent spans as 'continued'
		Nspans = len(spans['flow'])
		spans['continued'] = [False]*Nspans
		for n in range(Nspans-1):
			spans['continued'][n] = (spans['fhi'][n] == spans['flow'][n+1])

		if self.verbosity > 0:
			overlaps = [i for i in range(len(spans['bandcounts'])) if spans['bandcounts'][i]>Nant]
			if len(overlaps) > 0:
				print ('Autobands::spans() these spans have redundancy with >%d bands/span: %s' % (Nant,str(overlaps)))

		return spans

	def visualise(self, flow=None, fhigh=None, id=None, axis=[], startmarker=ord('a')):
		"""Show an ASCII 'bar graph' printout of the currently stored frequencies, or a list of frequencies"""
		screenwidth = 110

		if not flow:
			flow, fhigh, id = self.flow, self.fhigh, self.fantenna
		if not axis:
			(fmin,fmax,__,__,__) = self.statistics(flow, fhigh)
		else:
			fmin,fmax = axis
		fstep = (fmax - fmin) / screenwidth

		axislabel = ['| ', '%12.2f' % (fmin), '', '%12.2f' % (fmax), ' |']
		axislabel[2] = ' ' * (screenwidth - len(''.join(axislabel)))
		axislabel = ''.join(axislabel)

		for k in range(len(flow)):
			bw = fhigh[k] - flow[k]
			L0 = int((flow[k]-fmin)/fstep)
			L1 = int(math.ceil(bw/fstep))
			L2 = screenwidth - L1 - L0
			if L2 < 0:
				L1 += L2
			# indicator = chr(65+k%26)
			indicator = chr(startmarker + id[k] % 26)
			line = '.'*L0 + indicator*L1 + '.'*L2
			print (line)
		print (axislabel)

	def adjustStartFreq(self, f0, f1, finitial, df):
		"""Find better starting frequency in terms of FFTs.
		Within given freq span f0..f1 try to fit a few 'self.outputbw' bands
		and attempt to place the first band at a frequency at/past 'fstat' that
		minimizes the expected length of an FFT needed to extract that band from
		span f0..f1. Starts the search at fstart and increments by df.
		"""

		# Range check
		assert(f1 > f0)
		fstart = finitial
		if fstart > f1:
			return fstart
		if fstart < f0:
			fstart = f0

		# Bandwidths
		band_bw = self.outputbw
		head_bw = fstart - f0
		tail_bw = f1 - fstart
		span_bw = f1 - f0
		Nbands_req = int(tail_bw / band_bw)
		adjrange_bw = tail_bw - Nbands_req*band_bw  # window from fstart .. fstart+adj in which to search without loosing one of output Nband's

		Lfft_best = -1
		fout_curr = fstart
		fout_best = fstart
		while fout_curr <= (fstart + adjrange_bw):

			# FFT length influenced by:
			#   offset of zoom begin from parent start
			#   offset of zoom end from parent start
			#   relation of zoom and parent bandwidths

			# Likely FFT len needed when starting at freq
			res_curr = self.granularity([fout_curr - f0, fout_curr + band_bw - f0, band_bw, span_bw])
			Lfft_curr = int(span_bw / res_curr)
			if Lfft_best < 0:
				fout_best = fout_curr
				Lfft_best = Lfft_curr

			# Likely FFT len needed when starting at freq + df
			res_next = self.granularity([(fout_curr+df) - f0, (fout_curr+df) + band_bw - f0, band_bw, span_bw])
			Lfft_next = int(span_bw / res_next)

			if Lfft_best < 0 or Lfft_next < Lfft_best:
				if self.verbosity > 2:
					print ('Autobands::adjustStartFreq() shift start %12.2f (%d-pt FFT) to %12.f (%d-pt FFT) at %.3f kHz from band start' % 
						(fout_best, Lfft_best, (fout_curr+df), Lfft_next, ((fout_curr+df) - fstart)*1e-3) )
				fout_best = fout_curr + df
				Lfft_best = Lfft_next

			fout_curr = fout_curr + df

		if self.verbosity > 2:
			print ('Autobands::adjustStartFreq() shortest %d-pt FFT achievable at %+.3f kHz from initial freq, %+.3f kHz from span start' % (Lfft_best,(fout_best-finitial)*1e-3,(fout_best-f0)*1e-3))

		return fout_best

	def outputbands_merge(self, bw_inputs):
		"""
		Merge a list of input zooms as far as possible i.e. when from overlapped recorded bands.
		Done by checking whether all inputs fall into a single rec band at all antennas.
		@param bw_inputs a list of input zoom bands given as pairs of [fstart,bandwidth]
		"""
		inputs_merged = []

		# Do the merge
		f0 = bw_inputs[0][0]
		for kk in range(1,len(bw_inputs)):
			f1 = bw_inputs[kk][0] + bw_inputs[kk][1]
			if not self.bandsHaveRange(f0, f1):
				if self.verbosity > 4:
					print ('Autobands::outputbands_merge() no   cover %.6f--%.6f' % (f0*1e-6,f1*1e-6))
				inputs_merged.append([f0, bw_inputs[kk][0] - f0])
				f0 = bw_inputs[kk][0]
				if (kk+1) < len(bw_inputs):
					# Set f1 ahead of exiting loop next
					f1 = bw_inputs[kk+1][0] + bw_inputs[kk+1][1]
			else:
				if self.verbosity > 4:
					print ('Autobands::outputbands_merge() does cover %.6f--%.6f, continue and try merge the next span' % (f0*1e-6,f1*1e-6))

		# Handle anything remainig
		if f1 > f0:
			inputs_merged.append([f0, f1-f0])

		# Print the details if something was indeed merged
		if self.verbosity > 1 and len(inputs_merged) < len(bw_inputs):
			print ('Autobands::outputbands()    merged %d sub-spans into %d' % (len(bw_inputs),len(inputs_merged)))
			for fq,bw in inputs_merged:
				print ('Autobands::outputbands()        at fq %.6f MHz bw %.6f MHz' % (fq*1e-6,bw*1e-6))

		# Done!
		return inputs_merged

	def outputbands(self, Nant=None, fstart=None):
		"""Produce a set of output bands and a list of their input bands.
		Utilizes internally stored previously added information of antenna recorded frequencies.
		Output bands have the requested bandwidth. They can be direct matches to recorded bands, band slices (zoom) of recorded bands,
		or pieces of several band slices (zoom sets) taken from neighbouring recorded bands.
		"""

		# Determine freq list boundaries and tuning granularity
		(fmin,fmax,minbw,maxbw,df) = self.statistics()
		if self.verbosity > 0:
			print ('Autobands::outputbands() found common freq grid %.6f--%.6f MHz with %.3f kHz granularity' % (fmin*1e-6,fmax*1e-6,df*1e-3))

		# Find frequency spans where enough antennas overlap
		if not Nant:
			Nant = self.Nant
		self.detectedspans = self.spans(Nant)
		Nspans = len(self.detectedspans['flow'])

		# Generate a list of output bands, two possibilities:
		# 1) spans are wider or same as 'self.outputbw', must try to split spans or use as-is
		# 2) spans are narrower than 'self.outputbw', must try to combine >=2 spans for one output band
		outputbands = {'flow':[], 'inputs':[]}
		if fstart:
			foutstart = max(fstart,fmin)
		else:
			foutstart = fmin
		span = 0
		while span < Nspans:

			# Get span
			f0,f1,nrec = self.detectedspans['flow'][span], self.detectedspans['fhi'][span], self.detectedspans['bandcounts'][span]

			# Catch when large gaps between spans, e.g., geodetic/DDC mode
			if foutstart < f0:
				foutstart = f0
			elif f1 < foutstart:
				span += 1
				continue

			# Shift start to fall on a 'more integer' MHz if possible
			if span==0 or self.permitGaps:
				foutstart_1 = foutstart
				foutstart = self.adjustStartFreq(f0, f1, foutstart, df)
				if self.verbosity > 0:
					print ('Autobands::outputbands() processing span %d at %.6f--%.6f MHz bw %.6f MHz, best fstart @ %.3f kHz' % (span,f0*1e-6,f1*1e-6,(f1-f0)*1e-6,(foutstart-foutstart_1)*1e-3))

			# Generate band in Case 1 : fill span with complete bands up till max possible
			while (foutstart + self.outputbw) <= f1:
				outputbands['flow'].append(foutstart)
				outputbands['inputs'].append([[foutstart, self.outputbw]])
				if self.verbosity > 1:
					print ('Autobands::outputbands()    adding %.6f MHz bw from span %d @ %.6f MHz to fq %.6f MHz' % (self.outputbw,span,(foutstart-f0)*1e-6,foutstart))
				foutstart += self.outputbw

			# Generate band in Case 2 : pieces of span(s) for a later complete band, patch over to next span(s)
			bw_remain = f1 - foutstart
			if (bw_remain) > 0 and self.detectedspans['continued'][span]:

				# If overlap in rec bands at least at one antenna, may
				# try a slight shift to begin at a more 'integer' MHz
				if self.detectedspans['bandcounts'][span] > Nant and self.permitGaps:
					foutstart_1 = foutstart
					foutstart = self.adjustStartFreq(f0, f1, foutstart, df)
					if self.verbosity > 1 and foutstart_1 != foutstart:
						print ('Autobands::outputbands()    case 2 able to shift by %+.3f kHz in overlapped-bands portion for better FFT' % ((foutstart-foutstart_1)*1e-3))

				# Now piece together 'self.outputbw' amout of band from consecutive spans
				bw_needed = self.outputbw
				bw_inputs = []
				slicestartfreq = foutstart
				while (span < Nspans) and (bw_needed > 0):

					# Append more bw from span
					if (slicestartfreq-f0) < 0:
						break
					bw_utilized = min(bw_needed, bw_remain)
					bw_needed -= bw_utilized
					if self.verbosity > 1:
						print ('Autobands::outputbands()    case 2 adding %10.6f MHz bw from span %d @ %.6f MHz to fq %10.6f MHz, remain %10.6f MHz' % (bw_utilized*1e-6,span,(slicestartfreq-f0)*1e-6,foutstart*1e-6,bw_needed*1e-6))
					bw_inputs.append([slicestartfreq, bw_utilized])
					slicestartfreq += bw_utilized
					bw_remain = f1 - slicestartfreq

					# If out of remaining bw in this span, overflow into the next span
					if (bw_remain <= 0):
						span += 1
						if (span < Nspans):
							f0,f1,nrec = self.detectedspans['flow'][span], self.detectedspans['fhi'][span], self.detectedspans['bandcounts'][span]
							bw_remain = f1 - slicestartfreq
							assert(bw_remain >= 0)

				# Store the complete outputband(s) details
				if bw_needed == 0:

					# First try reducing the introduced input zooms where possible i.e. from overlapped rec bands
					if len(bw_inputs) > 1:
						bw_inputs = self.outputbands_merge(bw_inputs)

					# Store the final outputband constituents
					outputbands['flow'].append(foutstart)
					outputbands['inputs'].append(bw_inputs)
					foutstart += self.outputbw

				# No complete outputbands for current span
				else:
					if self.verbosity > 1:
						print ('Autobands::outputbands()    dropping incomplete out fq %.6f MHz' % (foutstart*1e-6))
					foutstart += bw_needed
					# span += 1 perhaps?


			# Case 3: no band remains in current span
			else:
				span += 1

		# Store final result
		self.outputbandconstituents = outputbands
		return outputbands

	def summarize(self):
		"""
		Print out a complete summary of what processing of ::outputbands() has produced, internally.
		"""

		(fmin,fmax,minbw,maxbw,df) = self.statistics()

		# Show the input
		print ('Recorded bands:')
		self.visualise()

		# Show auto-detected usable spans
		print ('Detected spans with array-wide common cover:')
		Nspans = len(self.detectedspans['flow'])
		self.visualise(self.detectedspans['flow'], self.detectedspans['fhi'], [n for n in range(Nspans)], axis=[fmin,fmax], startmarker=ord('A'))
		for n in range(Nspans):
			f0, f1, Nb = self.detectedspans['flow'][n]*1e-6, self.detectedspans['fhi'][n]*1e-6, self.detectedspans['bandcounts'][n]
			cont = self.detectedspans['continued'][n]
			print ('Span %3d %.6f--%.6f MHz, %11.6f MHz bw, %2d bands, no gap %s' % (n,f0,f1,f1-f0,Nb,str(cont)))

		# Show output bands and how they are assembled
		print ('\nOutput bands:')
		Nout = len(self.outputbandconstituents['flow'])
		f0 = list(self.outputbandconstituents['flow'])
		f1 = [ff + self.outputbw for ff in f0]
		id = [fi + self.Nant for fi in range(len(f0))]
		self.visualise(f0, f1, id, axis=[fmin,fmax])

		for n in range(Nout):
			print ('Output band %d: %12.2f -- %12.2f Hz' % (n, self.outputbandconstituents['flow'][n], self.outputbandconstituents['flow'][n]+self.outputbw))
			for zf,zfbw in self.outputbandconstituents['inputs'][n]:
				print ('   input %10.6f MHz USB @ %12.6f MHz' % (zf*1e-6,zfbw*1e-6))

		# Produce DiFX 2.6 v2d ZOOMs for reference
		print ('\nReference DiFX 2.6 v2d:')
		total_bw = 0.0
		total_gapbw = 0.0
		total_zooms = 0
		granularity_inputlist = []
		for n in range(Nout):
			m, M = 0, len(self.outputbandconstituents['inputs'][n])
			if (n+1) < Nout:
				gap_Hz = self.outputbandconstituents['flow'][n+1] - (self.outputbandconstituents['flow'][n]+self.outputbw)
			else:
				gap_Hz = 0
			total_gapbw += gap_Hz
			for zf,zfbw in self.outputbandconstituents['inputs'][n]:
				v2dline = 'addZoomFreq = freq@%.6f/bw@%.6f/noparent@true' % (zf*1e-6,zfbw*1e-6)
				v2dline = '  ' + v2dline + ' '*(65-len(v2dline)) + '# to output band %d, has %.3f kHz gap to next' % (n,gap_Hz*1e-3)
				if M>1:
					v2dline += ', band slice %d' % m
					m += 1
				total_bw += zfbw
				total_zooms += 1
				granularity_inputlist.append(zf)
				granularity_inputlist.append(zfbw)
				granularity_inputlist.append(zf+zfbw)
				print (v2dline)
		df = self.granularity(granularity_inputlist)
		fft = 2*(maxbw/df)
		print ('  # %d zooms, %d output bands of %.3f MHz, %.3f MHz total bandwidth, %.3f MHz non-covered due gaps' % (total_zooms, Nout, self.outputbw*1e-6, total_bw*1e-6, total_gapbw*1e-6))
		print ('  # at least %.6f kHz needed for fftSpecRes, %.1f-point FFT on widest rec band of %.3f MHz' % (df*1e-3,fft,maxbw*1e-6))


class AutobandsCases:
	"""Populate an Autobands object with data according to a specific case"""

	def __init__(self):
		global verbosity
		self.permitGaps = False
		self.casefuncs = ['EHT', 'EHT_ALMA', 'EHT_ALMA_NOEMA', 'GMVA_ALMA', 'GMVA_NOEMA', 'GMVA_ALMA_NOEMA']
		self.verbosity = verbosity
		pass

	def getCases(self):
		return list(self.casefuncs)

	def generateFromVEX(self, vexfile, outputbw_Hz = None):
		if not outputbw_Hz:
			outputbw_Hz = 32e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		a.addVEX(vexfile)
		return a

	def generateFromCase(self, casename = None, outputbw_Hz = None):
		if not casename or casename not in self.casefuncs:
			print ('Unknown case %s.\nSupported cases are: %s' % (str(casename),str(self.casefuncs)))
			sys.exit(-1)
			#return None
		func = getattr(self, 'case_' + casename, None)	
		return func(outputbw_Hz)

	def case_EHT(self, outputbw_Hz):
		if not outputbw_Hz:
			outputbw_Hz = 58e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		a.add( a.genEHT(214100.0e6) ) # most of EHT
		a.add( a.genEHT(214003.0e6) ) # APEX
		a.add( a.genEHT(214000.0e6) ) # GLT
		return a

	def case_EHT_ALMA(self, outputbw_Hz):
		if not outputbw_Hz:
			outputbw_Hz = 58e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		a.add( a.genEHT(214100.0e6) )
		a.add( a.genALMA(214039.453125e6) )
		return a

	def case_EHT_ALMA_NOEMA(self, outputbw_Hz):
		if not outputbw_Hz:
			outputbw_Hz = 58e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		a.add( a.genEHT(214100.0e6) )
		a.add( a.genALMA(214039.453125e6) )
		a.add( a.genNOEMA(214100.0e6) )
		return a

	def case_GMVA_ALMA(self, outputbw_Hz):
		if not outputbw_Hz:
			outputbw_Hz = 32e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		# a.add( a.genVLBA(86380.0e6) )  #  2 x 128M VLBA RDBE
		a.add( a.genDBBC3(86044.0e6) ) # 64 x 32M, start from -32 MHz (1st LSB)
		a.add( a.genALMA(86380.0e6) )  # 64 x 62.5M, overlapped
		return a

	def case_GMVA_NOEMA(self, outputbw_Hz):
		if not outputbw_Hz:
			outputbw_Hz = 32e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		a.add( a.genDBBC3(86044.0e6) ) # 64 x 32M, start from -32 MHz (1st LSB)
		a.add( a.genNOEMA(86044.0e6) ) # 64 x 64M, start from 0 MHz
		return a

	def case_GMVA_ALMA_NOEMA(self, outputbw_Hz):
		if not outputbw_Hz:
			outputbw_Hz = 32e6
		a = Autobands(outputbw_Hz, self.verbosity, self.permitGaps)
		a.add( a.genVLBA(86380.0e6) )  #  2 x 128M VLBA RDBE
		a.add( a.genDBBC3(86044.0e6) ) # 64 x 32M, start from -32 MHz (1st LSB)
		a.add( a.genALMA(86380.0e6) )  # 64 x 62.5M, overlapped
		a.add( a.genNOEMA(86044.0e6) ) # 64 x 64M, start from 0 MHz
		return a


if __name__ == "__main__":

	gen = AutobandsCases()

	# User args
	if len(sys.argv) < 2:
		print (__doc__)
		print ('Available cases: %s\n' % (str(gen.getCases())))
		sys.exit(0)
	casename = sys.argv[1]
	if len(sys.argv) >= 3:
		outputbw_hz = int(float(sys.argv[2])*1e6)
	else:
		outputbw_hz = None

	# Check if particular case should be run, or 'all' supported ones
	if casename == 'all':
		cases = gen.casefuncs
	else:
		cases = [casename]

	# Handle the case(s)
	for casename in cases:
		print ('\n----------------- %s -----------------' % (casename))

		# Populate frequency tables
		if os.path.isfile(casename):
			a = gen.generateFromVEX(casename, outputbw_hz)
		else:
			a = gen.generateFromCase(casename, outputbw_hz)

		# Process
		# a.outputbands(fstart=212162.796875e6)
		suggested_bw = a.autobandwidth()
		if suggested_bw != None and outputbw_hz != suggested_bw:
			print ('Determined best-fit bandwidth would be %.3f MHz' % (suggested_bw*1e-6))
			if not outputbw_hz:
				a.setBandwidth(suggested_bw)
				print ('Using autodetermined bandwidth of %.3f MHz' % (suggested_bw*1e-6))
		a.outputbands()
		a.summarize()
