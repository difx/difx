#!/usr/bin/python
"""
Usage: printDiFXInput.py [-h] [-a] [-f] [-b] [-r] [-u] [-v]
                         <difx base name> [<difx base name> ...]

Prints a summary of a DiFX .input file.

optional arguments:
  -h, --help        show this help message and exit
  -f, --freqs       Show FREQ table
  -b, --baselines   Show BASELINE table
  -d, --datastreams Show DATASTREAMS and provided frequencies
  -u, --unreffreqs  List frequencies not referenced by BASELINEs
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
		self.printUnreferenced = enabled
		self.printDSTREAM = enabled


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
			print((" "*indent) + "fq %3d : %s" % (n, cfg.freqs[n].str().strip()))
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
					print((" "*3*indent) + "%s %2d x %s %2d : %s%s : fq %2d x %2d : %s x %s" % (fqtype1,bl_band_1,fqtype2,bl_band_2, pol1,pol2, fq1,fq2,sfq1,sfq2))
					if b.version >= 2.7:
						print((" "*3*indent) + "  part of fq %3d %s" % (destfreq,sdestfq))
						baseline_outputfreq_members[destfreq].append(fq1)
						baseline_outputfreq_members[destfreq].append(fq2)
			if b.version >= 2.7:
				print((" "*2*indent) + "Output band mapping:")
				for outfq in baseline_outputfreq_members.keys():
					constituents = list(set(baseline_outputfreq_members[outfq]))
					print((" "*3*indent) + "output fq %3d created from freq(s) %s" % (outfq,str(constituents)))
		print("")

		# Print all utilized destination freqs of the BASELINEs
		if version >= 2.7:
			all_dest_fqs = list(set(all_dest_fqs))
			all_dest_fqs.sort()
			print("All referenced output band FREQs:")
			for fq in all_dest_fqs:
				print((" "*1*indent) + "fq %3d : %s" % (fq, cfg.freqs[fq].str().strip()))
			print((" "*1*indent) + "%d utilized output freqs in total" % (len(all_dest_fqs)))

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
		if '-b' in args or '--baselines' in args:
			opts.printBASELINE = True
		if '-d' in args or '--datastreams' in args:
			opts.printDSTREAM = True
		if '-u' in args or '--unreffreqs' in args:
			opts.printUnreferenced = True
		opts.verbosity = args.count('-v') + args.count('--verbose')

	if len(sys.argv) < 2:
		print (__doc__)
		sys.exit(-1)

	for difxf in files:
		print('\nInspecting %s:\n' % (difxf))
		printDiFXInput(difxf, opts)

