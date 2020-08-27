#!/usr/bin/python
"""
Usage: printDiFXInput.py [-h] [-f] [-b] [-r] [-u] [-o] [-s] [-a] [-v]
                         <difx base name> [<difx base name> ...]

Prints a summary of a DiFX .input file.

optional arguments:
  -h, --help        show this help message and exit
  -f, --freqs       Show FREQ table
  -d, --datastreams Show DATASTREAMS and provided frequencies
  -b, --baselines   Show BASELINE table
  -u, --unreffreqs  List frequencies not referenced by BASELINEs
  -o, --outfreqs    List frequencies produced by BASELINEs
  -s, --outfreqassy List output frequencies and contributing freqs
  -a, --all         Show all of the above items (default)
  -v                Increase verbosity

"""

import glob, sys
import parseDiFX

class PrintOpts:

	def __init__(self):
		self.verbosity = 0
		self.all()

	def none(self):
		self.setAll(False)

	def all(self):
		self.setAll(True)

	def setAll(self,enabled):
		self.printFREQ = enabled
		self.printBASELINE = enabled
		self.printDSTREAM = enabled
		self.printUnreferenced = enabled
		self.printReferenced = enabled
		self.printOutputbandDetails = enabled


def getPolsForFreq(ds,fqId):
        """Look up frequency ID in datastream recorded and zoom frequencies, return 'recbandpols' for that frequency"""
        pols = []
        npol_expected = 0
        if fqId in ds.recfreqindex:
                i = ds.recfreqindex.index(fqId)
                npol_expected = ds.recfreqpols[i]
                nsubbands = len(ds.recbandindex)
                pols = [ds.recbandpol[j] for j in range(nsubbands) if ds.recbandindex[j] == i]
        if fqId in ds.zoomfreqindex:
                i = ds.zoomfreqindex.index(fqId)
                npol_expected = ds.zoomfreqpols[i]
                nsubbands = len(ds.zoombandindex)
                pols = [ds.zoombandpol[j] for j in range(nsubbands) if ds.zoombandindex[j] == i]
        assert(npol_expected == len(pols))
        return pols


def getBandIndexOfFreqPol(ds,fqId,pol):
        """Look up the 'band index' in datastream where the given frequency and polarization are found"""
        bandindex = -1
        if fqId in ds.recfreqindex:
                i = ds.recfreqindex.index(fqId)
                nsubbands = len(ds.recbandindex)
                bandpolpairs = [(ds.recbandpol[j],j) for j in range(nsubbands) if ds.recbandindex[j] == i]
                for (polzn,band) in bandpolpairs:
                        if polzn == pol: bandindex = band
        if fqId in ds.zoomfreqindex:
                i = ds.zoomfreqindex.index(fqId)
                nsubbands = len(ds.zoombandindex)
                bandpolpairs = [(ds.zoombandpol[j],j) for j in range(nsubbands) if ds.zoombandindex[j] == i]
                for (polzn,band) in bandpolpairs:
                        if polzn == pol: bandindex = band + ds.nrecband
        return bandindex


def getFreqPolOfBand(ds,band):
        """Look up the frequency and polarization of a given band"""
        pol = ''
        fqId = -1
        if band < len(ds.recbandindex):
                recfreq = ds.recbandindex[band]
                fqId = ds.recfreqindex[recfreq]
                pol = ds.recbandpol[band]
        else:
                band = band - len(ds.recbandindex)
                recfreq = ds.zoombandindex[band]
                fqId = ds.zoomfreqindex[recfreq]
                pol = ds.zoombandpol[band]
        return (fqId,pol)


def printDiFXInput(basename,opts,indent=2,version=2.6):
	"""Print summary of DiFX .input file"""

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'
	difx = parseDiFX.DiFXFile(inputfile)
	if not difx.isvalid():
		parser.error("Couldn't parse input file " + inputfile + " correctly")
	cfg = difx.metainfo

	# Print out the full frequency table
	if opts.printFREQ:
		print("Frequencies in FREQ table:")
		for n in range(len(cfg.freqs)):
			print((" "*indent) + "fq %3d : %s" % (n, cfg.freqs[n].str()))
		print("")

	# Print out all recorded freqs listed in DATASTREAMS
	if opts.printDSTREAM:
		print("Frequencies referenced by DATASTREAMs:")
		for d in cfg.datastreams:
			print((" "*indent) + "Datastream %d : telescope %s" % (cfg.datastreams.index(d),cfg.telescopes[d.telescopeindex].name))

			if (len(d.recfreqindex) != d.nrecfreq):
				print((" "*2*indent) + "error: recfreqindex array has %d elements, expected %d" % (len(d.recfreqindex), d.nrecfreq))
			if (len(d.recbandindex) != d.nrecband):
				print((" "*2*indent) + "error: recbandindex array has %d elements, expected %d" % (len(d.recbandindex), d.nrecband))
			if (len(d.zoomfreqindex) != d.nzoomfreq):
				print((" "*2*indent) + "error: zoomfreqindex array has %d elements, expected %d" % (len(d.zoomfreqindex), d.nzoomfreq))
			if (len(d.zoombandindex) != d.nzoomband):
				print((" "*2*indent) + "error: zoombandindex array has %d elements, expected %d" % (len(d.zoombandindex), d.nzoomband))

			if opts.verbosity >= 1:
				print((" "*2*indent) + "recfreqindex: %s" % (str(d.recfreqindex)))
				print((" "*2*indent) + "recbandindex: %s" % (str(d.recbandindex)))
				print((" "*2*indent) + "recbandpol: %s" % (str(d.recbandpol)))
				print((" "*2*indent) + "zoomfreqindex: %s" % (str(d.zoomfreqindex)))
				print((" "*2*indent) + "zoombandindex: %s" % (str(d.zoombandindex)))
				print((" "*2*indent) + "zoombandpol: %s" % (str(d.zoombandpol)))

			for n in range(len(d.recfreqindex)):
				fq = d.recfreqindex[n]
				npol = d.recfreqpols[n]
				pols = getPolsForFreq(d,fq)
				print((" "*2*indent) + "rec  fq %3d %d-pol %s %s" % (fq,npol,str(pols),cfg.freqs[fq].str().strip()))

			for n in range(len(d.zoomfreqindex)):
				fq = d.zoomfreqindex[n]
				npol = d.zoomfreqpols[n]
				pols = getPolsForFreq(d,fq)
				print((" "*2*indent) + "zoom fq %3d %d-pol %s %s" % (fq,npol,str(pols),cfg.freqs[fq].str().strip()))

		print("")

	# Print out all BASELINEs
	if opts.printBASELINE:
		print("Content of BASELINE table:")
		all_dest_fqs = []
		for b in cfg.baselines:
			ds1 = cfg.datastreams[b.dsaindex]
			ds2 = cfg.datastreams[b.dsbindex]		
			print((" "*1*indent) + "Baseline %s x %s / DS %2d x %2d" % (cfg.telescopes[ds1.telescopeindex].name,cfg.telescopes[ds2.telescopeindex].name,b.dsaindex,b.dsbindex))
			if len(b.dsabandindex) != len(b.dsbbandindex):
				print((" "*2*indent) + "error: lenghts of ds<X>bandindex do not match (DS A: %d and DS B: %d)" % (len(b.dsabandindex),len(b.dsbbandindex)))
			baseline_outputfreq_members = {}
			for n in range(len(b.dsabandindex)):
				bl_bands_1 = b.dsabandindex[n]
				bl_bands_2 = b.dsbbandindex[n]
				if b.version >= 2.7:
					version = b.version
					destfreq = b.destfreq[n]
				else:
					destfreq,tmp = getFreqPolOfBand(ds1,min(bl_bands_1))
				if destfreq not in baseline_outputfreq_members:
					baseline_outputfreq_members[destfreq] = []
				all_dest_fqs.append(destfreq)
				sdestfq = cfg.freqs[destfreq].str().strip()
				print((" "*2*indent) + "Cross-products set %d:" % (n))
				for (bl_band_1,bl_band_2) in zip(bl_bands_1,bl_bands_2):
					fq1,pol1 = getFreqPolOfBand(ds1,bl_band_1)
					fq2,pol2 = getFreqPolOfBand(ds2,bl_band_2)
					sfq1 = cfg.freqs[fq1].str().strip()
					sfq2 = cfg.freqs[fq2].str().strip()
					fqtype1, fqtype2 = 'rec ', 'rec '
					if bl_band_1 >= ds1.nrecband: fqtype1 = 'zoom'
					if bl_band_2 >= ds2.nrecband: fqtype2 = 'zoom'
					print((" "*3*indent) + "pols %s%s freqs %s x %s" % (pol1,pol2,sfq1,sfq2))
					if opts.verbosity >= 1:
						print((" "*3*indent) + "%s %2d x %s %2d -> fq %2d x %2d -> outFq %d" % (fqtype1,bl_band_1,fqtype2,bl_band_2, fq1,fq2,destfreq))
		print("")

	# Print all utilized destination freqs of the BASELINEs
	if opts.printReferenced:
		nfreqs, freqs = cfg.determine_outputfreqs()
		freqs.sort()
		print("All output FREQs referenced by BASELINEs:")
		for fq in freqs:
			print((" "*1*indent) + "fq %3d : %s" % (fq, cfg.freqs[fq].str().strip()))
		print((" "*1*indent) + "%d freqs in total expected in output visibility data" % (nfreqs))
		print("")

	# Print out all FREQ entries not referenced by DATASTREAMS
	if opts.printUnreferenced:
		print("Frequencies not referenced by any DATASTREAM:")
		all_fqs_used = []
		for d in cfg.datastreams:
			all_fqs_used = all_fqs_used + d.recfreqindex + d.zoomfreqindex
		all_fqs_used = set(all_fqs_used)
		unused_fqs = list(set(range(len(cfg.freqs))) - all_fqs_used)
		unused_fqs.sort()
		if len(unused_fqs) < 1:
			print((" "*indent) + "(none)")
		else:
			for n in range(len(unused_fqs)):
				fq = unused_fqs[n]
				print((" "*indent) + "fq %3d %s" % (fq, cfg.freqs[fq].str().strip()))
		print("")

	# Print out all Outputbands
	if opts.printOutputbandDetails:
		print("Outputbands and their assembly:")
		nfreqs, freqs = cfg.determine_outputfreqs()
		outputfreq_members = {freq:[] for freq in freqs}
		for b in cfg.baselines:
			ds1, ds2 = cfg.datastreams[b.dsaindex], cfg.datastreams[b.dsbindex]
			for n in range(len(b.dsabandindex)):
				bl_bands_1, bl_bands_2 = b.dsabandindex[n], b.dsbbandindex[n]
				if b.version >= 2.7:
					destfreq = b.destfreq[n]
				else:
					destfreq,tmp = getFreqPolOfBand(ds1,min(bl_bands_1))
				for (bl_band_1,bl_band_2) in zip(bl_bands_1,bl_bands_2):
					fq1,pol1 = getFreqPolOfBand(ds1,bl_band_1)
					fq2,pol2 = getFreqPolOfBand(ds2,bl_band_2)					
					outputfreq_members[destfreq].append(fq1)
					outputfreq_members[destfreq].append(fq2)
		outfreqs = outputfreq_members.keys()
		outfreqs.sort()
		for outfq in outfreqs:
			constituents = list(set(outputfreq_members[outfq]))
			constituents.sort()
			print((" "*1*indent) + "freq %d %s assembled from" % (outfq, cfg.freqs[outfq].str().strip()))
			for confq in constituents:
				f = cfg.freqs[confq]
				fstr = "%.6f MHz %3s at %.6f MHz" % (f.bandwidth, 'LSB' if f.lsb else 'USB', f.freq)
				print((" "*2*indent) + "fq %3d bw %s" % (confq, fstr))
		print((" "*1*indent) + "%d outputbands in total" % (nfreqs))
		print("")

if __name__ == "__main__":

	opts = PrintOpts()
	opts.verbosity = 2
	opts.all()

	args = [arg for arg in sys.argv[1:] if arg[0]=='-']
	files = [arg for arg in sys.argv[1:] if arg[0]!='-']

	if len(args) > 0:
		if '-a' not in args and '--all' not in args:
			opts.none()
		if '-h' in args or '--help' in args:
			print (__doc__)
			sys.exit(-1)
		if '-f' in args or '--freqs' in args:
			opts.printFREQ = True
		if '-d' in args or '--datastreams' in args:
			opts.printDSTREAM = True
		if '-b' in args or '--baselines' in args:
			opts.printBASELINE = True
		if '-u' in args or '--unreffreqs' in args:
			opts.printUnreferenced = True
		if '-o' in args or '--outfreqs' in args:
			opts.printReferenced = True
		if '-s' in args or '--outfreqassy' in args:
			opts.printOutputbandDetails = True
		opts.verbosity = args.count('-v') + args.count('--verbose')

	if len(sys.argv) < 2:
		print (__doc__)
		sys.exit(-1)

	for difxf in files:
		print('\nInspecting %s:\n' % (difxf))
		printDiFXInput(difxf, opts)
