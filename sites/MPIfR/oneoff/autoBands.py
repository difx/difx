#!/usr/bin/python
#
# Usage: autoBandsRef.py [<vexfile> <output_bandwidth_MHz>]
#
# Automatic "output bands" generation based upon sets
# of recorded bands per antenna. The recorded bands
# can be taken from a VEX file. If not VEX is specified
# then some band test cases will be generated internally.
#
# Reference implementation for a later native C/C++ port.
#
# 01/2019 Jan Wagner
#

import math, fractions, sys
import vex  # same as utilized by autozooms module

class Autobands:

	def __init__(self, outputbandwidth=16e6, verbosity=0, permitgaps=False):
		self.flow = []
		self.fhigh = []
		self.fantenna = []
		self.outputbw = outputbandwidth
		self.Nant = 0
		self.verbosity = verbosity
		self.permitGaps = permitgaps
		if self.outputbw < 1e6:
			print ('Warning: Autobands() instantiated with small output bandwidth %.3f Hz, treating it as MHz' % (outputbandwidth))
			self.outputbw = outputbandwidth*1e6

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
		Nch = 64
		bw = 64e6
		return self.genPFB(fbase_Hz, bw, bw, Nch, sb)
	
	def genGMVA(self, fbase_Hz, sb=+1):
		"""Generate GMVA in PFB 32 MHz mode"""
		Nch = 8
		bw = 32e6
		return self.genPFB(fbase_Hz, bw, bw, Nch, sb)

	def genEHT(self, fbase_Hz, sb=+1):
		"""Generate EHT with 1 x 2048 MHz"""
		bw = 2048e6
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

	def statistics(self, flow=None, fhigh=None):
		"""Return min and max frequencies and common minimum freq spacing. Returns (fmin,fmax,df)."""
		if not flow:
			flow, fhigh = self.flow, self.fhigh
		fqall = list(flow) + list(fhigh)
		fmin = reduce(min, fqall)
		fmax = reduce(max, fqall)
		fqall += [fhigh[n]-flow[n] for n in range(len(flow))]
		fqall.append(self.outputbw)
		df = reduce(fractions.gcd, fqall)
		return (fmin,fmax,df)

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
			#count = len(set(ants_in_span))
			count = len(ants_in_span)

			# Enough antennas have it, keep span?
			if count >= Nant:
				spans['flow'].append( span_lo )
				spans['fhi'].append( span_hi )
				spans['bandcounts'].append( count )
				if self.verbosity > 2:
					print ('Autobands::spans() retain  %.6f--%.6f MHz bw %.6f MHz with %d rec bands' % (span_lo*1e-6,span_hi*1e-6,(span_hi-span_lo)*1e-6,count))
			else:
				if self.verbosity > 2:
					print ('Autobands::spans() discard %.6f--%.6f MHz bw %.6f MHz with %d rec bands < %d antennas' % (span_lo*1e-6,span_hi*1e-6,(span_hi-span_lo)*1e-6,count,Nant))

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

	def visualise(self, flow=None, fhigh=None, id=None, axis=[]):
		"""Show an ASCII 'bar graph' printout of the currently stored frequencies, or a list of frequencies"""
		screenwidth = 110

		if not flow:
			flow, fhigh, id = self.flow, self.fhigh, self.fantenna
		if not axis:
			(fmin,fmax,__) = self.statistics(flow, fhigh)
		else:
			fmin,fmax = axis
		df = (fmax - fmin) / screenwidth

		axislabel = '| %12.2f' % (fmin) + ' '*(screenwidth - 2*16) + '%12.2f |' % (fmax)
		for k in range(len(flow)):
			bw = fhigh[k] - flow[k]
			L0 = int((flow[k]-fmin)/df)
			L1 = int(math.ceil(bw/df))
			L2 = screenwidth - L1 - L0
			# indicator = chr(65+k%26)
			indicator = chr(ord('a') + id[k] % 26)
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
			res_curr = reduce(fractions.gcd, [fout_curr - f0, fout_curr + band_bw - f0, band_bw, span_bw])
			Lfft_curr = int(span_bw / res_curr)
			if Lfft_best < 0:
				fout_best = fout_curr
				Lfft_best = Lfft_curr

			# Likely FFT len needed when starting at freq + df
			res_next = reduce(fractions.gcd, [(fout_curr+df) - f0, (fout_curr+df) + band_bw - f0, band_bw, span_bw])
			Lfft_next = int(span_bw / res_next)

			if Lfft_best < 0 or Lfft_next < Lfft_best:
				if self.verbosity > 0:
					print ('Autobands::adjustStartFreq() shift start %12.2f (%d-pt FFT) to %12.f (%d-pt FFT) at %.3f kHz from band start' % 
						(fout_best, Lfft_best, (fout_curr+df), Lfft_next, ((fout_curr+df) - fstart)*1e-3) )
				fout_best = fout_curr + df
				Lfft_best = Lfft_next

			fout_curr = fout_curr + df

		if self.verbosity > 1:
			print ('Autobands::adjustStartFreq() shortest %d-pt FFT achievable at %+.3f kHz from initial freq, %+.3f kHz from span start' % (Lfft_best,(fout_best-finitial)*1e-3,(fout_best-f0)*1e-3))

		return fout_best

	def outputbands(self, Nant=None, fstart=None):
		"""Produce a set of output bands and a list of their input bands.
		Utilizes internally stored previously added information of antenna recorded frequencies.
		Output bands have the requested bandwidth. They can be direct matches to recorded bands, band slices (zoom) of recorded bands,
		or pieces of several band slices (zoom sets) taken from neighbouring recorded bands.
		"""

		# Determine freq list boundaries and tuning granularity
		(fmin,fmax,df) = self.statistics()
		if self.verbosity > 0:
			print ('Autobands::outputbands() found Common freq grid %.6f--%.6f MHz with %.3f kHz granularity' % (fmin*1e-6,fmax*1e-6,df*1e-3))

		# Find frequency spans where enough antennas overlap
		if not Nant:
			Nant = self.Nant
		spans = self.spans(Nant)
		Nspans = len(spans['flow'])

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
			f0,f1,nrec = spans['flow'][span], spans['fhi'][span], spans['bandcounts'][span]
			if f0 < foutstart:
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
			if (bw_remain) > 0 and spans['continued'][span]:

				# If overlap in rec bands at least at one antenna, may
				# try a slight shift to begin at a more 'integer' MHz
				if spans['bandcounts'][span] > Nant:
					foutstart_1 = foutstart
					foutstart = self.adjustStartFreq(f0, f1, foutstart, df)
					#print ('--- overlap adj, delta %.3f kHz' % ((foutstart-foutstart_1)*1e-3))

				# Now piece together 'self.outputbw' amout of band from consecutive spans
				bw_needed = self.outputbw
				bw_inputs = []
				slicestartfreq = foutstart
				while (span < Nspans) and (bw_needed > 0):
					# Append remaining bw from span
					bw_utilized = min(bw_needed, bw_remain)
					bw_needed -= bw_utilized
					if self.verbosity > 1:
						print ('Autobands::outputbands()    case 2 adding %10.6f MHz bw from span %d @ %.6f MHz to fq %10.6f MHz, remain %10.6f MHz' % (bw_utilized*1e-6,span,(slicestartfreq-f0)*1e-6,foutstart*1e-6,bw_needed*1e-6))
					bw_inputs.append([slicestartfreq, bw_utilized])
					slicestartfreq += bw_utilized
					# Get next span to add to bandwidth
					span += 1
					if (span < Nspans):
						f0,f1,nrec = spans['flow'][span], spans['fhi'][span], spans['bandcounts'][span]
						bw_remain = f1 - slicestartfreq
						print bw_remain, f1, slicestartfreq
						assert(bw_remain >= 0)

				# Store the complete outputband(s) details
				if bw_needed == 0:

					# Attempt a merge of introduced inputs/zooms where possible, checking if all inputs fall into a single rec band at all antennas
					if len(bw_inputs) > 1:
						inputs_merged = []
						f0 = bw_inputs[0][0]
						for kk in range(1,len(bw_inputs)):
							f1 = bw_inputs[kk][0] + bw_inputs[kk][1]
							if not self.bandsHaveRange(f0, f1):
								# print ('no   cover %.6f--%.6f' % (f0*1e-6,f1*1e-6))
								inputs_merged.append([f0, bw_inputs[kk][0] - f0])
								f0 = bw_inputs[kk][0]
								if (kk+1) < len(bw_inputs):
									# Set f1 ahead of exiting loop next
									f1 = bw_inputs[kk+1][0] + bw_inputs[kk+1][1]
							else:
								# print ('does cover %.6f--%.6f, continue and try merge the next span' % (f0*1e-6,f1*1e-6))
								pass
						if f1 > f0:
							inputs_merged.append([f0, f1-f0])
						if self.verbosity > 1 and len(inputs_merged) < len(bw_inputs):
							print ('Autobands::outputbands()    merged %d sub-spans into %d' % (len(bw_inputs),len(inputs_merged)))
							for fq,bw in inputs_merged:
								print ('Autobands::outputbands()        at fq %.6f MHz bw %.6f MHz' % (fq*1e-6,bw*1e-6))
						bw_inputs = inputs_merged

					# Store
					outputbands['flow'].append(foutstart)
					outputbands['inputs'].append(bw_inputs)
					foutstart += self.outputbw

				# No complete outputbands for current span
				else:
					foutstart += bw_needed
					if self.verbosity > 1:
						print ('Autobands::outputbands()    dropping incomplete out fq %.6f MHz' % (outputbands['flow'][-1]*1e-6))

			# Case 3: no band remains in current span
			else:
				span += 1

		# Visual summary, inputs
		print ('Input bands:')
		self.visualise()

		# Visual summary, segmented freqs axis
		print ('Detected spans:')
		self.visualise(spans['flow'], spans['fhi'], [n for n in range(Nspans)], axis=[fmin,fmax])
		for n in range(Nspans):
			f0, f1, Nb = spans['flow'][n]*1e-6, spans['fhi'][n]*1e-6, spans['bandcounts'][n]
			cont = spans['continued'][n]
			print ('Span %3d %.6f--%.6f MHz, %11.6f MHz bw, %2d bands, no gap %s' % (n,f0,f1,f1-f0,Nb,str(cont)))

		# Visual summary, outputs
		print ('\nOutput bands:')
		Nout = len(outputbands['flow'])
		f0 = list(outputbands['flow'])
		f1 = [ff + self.outputbw for ff in f0]
		id = [fi + self.Nant for fi in range(len(f0))]
		self.visualise(f0, f1, id, axis=[fmin,fmax])

		for n in range(Nout):
			print ('Output band: %12.2f -- %12.2f Hz' % (outputbands['flow'][n], outputbands['flow'][n]+self.outputbw))
			for zf,zfbw in outputbands['inputs'][n]:
				print ('   input %10.6f MHz USB @ %12.6f MHz from a rec band' % (zf*1e-6,zfbw*1e-6))

		# DiFX v2d
		for n in range(Nout):
			m, M = 0, len(outputbands['inputs'][n])
			if (n+1) < Nout:
				gap_Hz = outputbands['flow'][n+1] - (outputbands['flow'][n]+self.outputbw)
			else:
				gap_Hz = 0
			for zf,zfbw in outputbands['inputs'][n]:
				v2dline = 'addZoomFreq = freq@%.6f/bw@%.6f/noparent@true' % (zf*1e-6,zfbw*1e-6)
				v2dline = '  ' + v2dline + ' '*(65-len(v2dline)) + '# to output band %d, has %.3f kHz gap to next' % (n,gap_Hz*1e-3)
				if M>1:
					v2dline += ', band slice %d' % m
					m += 1
				print (v2dline)



if __name__ == "__main__":

	verbosity = 2

	if len(sys.argv)>=2:

		# Use VEX file
		# outbw = 58.59375e6
		outbw = 58e6
		vexf = sys.argv[1]
		if len(sys.argv)>=3:
			outbw = int(float(sys.argv[2])*1e6)

		a = Autobands(outbw, verbosity)

		a.addVEX(vexf)

	else:
		# Use hardcoded cases
		# a = Autobands(32e6, verbosity)
		a = Autobands(58.0e6, verbosity)     # ALMA but try 58M
		# a = Autobands(58.59375e6, verbosity) # ALMA spacing of overlapped 62.5M channels

		a.add( a.genEHT(86380.0e6) )
		## a.addBand( 86380.0e6 + 128e6, 86380.0e6 + 1024e6 )
		#a.addBand( 86380.0e6, 86380.0e6 + 1024e6 )
		## a.addBand( 86380.0e6 + 896e6, 86380.0e6 + 1536e6 )
		a.add( a.genALMA(86380.0e6) )
		## a.add( a.genNOEMA(86380.0e6) )
		## a.add( a.genGMVA(86380.0e6, sb=-1) )
		## a.add( a.genDBBC3(86044.00e6) )

	# Process

	print ('Recorded bands:')
	a.visualise()

	print ('Computed bands:')
	# a.outputbands(fstart=212162.796875e6)
	a.outputbands()

