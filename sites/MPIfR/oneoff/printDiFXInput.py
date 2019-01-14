#!/usr/bin/python
"""
Usage: printDiFXInput.py <difx base name> [<difx base name> ...]

Prints a summary of a DiFX .input file. The printout
has higher detail than the converters (vex2difx, difx2fits, difx2mark4).
"""
import glob, sys, numpy
import parseDiFX

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


def printDiFXInput(basename,indent=2):
	"""Print summary of DiFX .input file"""

	# Extract meta-infos from the DiFX .INPUT file
	if basename.endswith(('.difx','.input','.calc')):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'
	inputfile_cfg = parseDiFX.get_common_settings(inputfile)
	(numfreqs, freqs) = parseDiFX.get_freqtable_info(inputfile)
	(numtelescopes, telescopes) = parseDiFX.get_telescopetable_info(inputfile)
	(numdatastreams, datastreams) = parseDiFX.get_datastreamtable_info(inputfile)
	(numbaselines, baselines) = parseDiFX.get_baselinetable_info(inputfile)
	if numfreqs == 0 or numtelescopes == 0 or numdatastreams == 0 or numbaselines == 0:
		parser.error("Couldn't parse input file " + inputfile + " correctly")

        # Print out all recorded freqs listed in DATASTREAMS
	print("Frequencies actually referenced by the DATASTREAMs:")
	all_fqs_used = []
        for d in datastreams:
		print((" "*indent) + "Datastream %d : telescope %s" % (datastreams.index(d),telescopes[d.telescopeindex].name))

		if (len(d.recfreqindex) != d.nrecfreq):
			print((" "*2*indent) + "error: recfreqindex array has %d elements, expected %d" % (len(d.recfreqindex), d.nrecfreq))
		if (len(d.recbandindex) != d.nrecband):
			print((" "*2*indent) + "error: recbandindex array has %d elements, expected %d" % (len(d.recbandindex), d.nrecband))
		if (len(d.zoomfreqindex) != d.nzoomfreq):
			print((" "*2*indent) + "error: zoomfreqindex array has %d elements, expected %d" % (len(d.zoomfreqindex), d.nzoomfreq))
		if (len(d.zoombandindex) != d.nzoomband):
			print((" "*2*indent) + "error: zoombandindex array has %d elements, expected %d" % (len(d.zoombandindex), d.nzoomband))

		for n in range(len(d.recfreqindex)):
			fq = d.recfreqindex[n]
			npol = d.recfreqpols[n]
			pols = getPolsForFreq(d,fq)
			print((" "*2*indent) + "rec  %d-pol %s %s" % (npol,str(pols),freqs[fq].str().strip()))

		for n in range(len(d.zoomfreqindex)):
			fq = d.zoomfreqindex[n]
			npol = d.zoomfreqpols[n]
			pols = getPolsForFreq(d,fq)
			print((" "*2*indent) + "zoom %d-pol %s %s" % (npol,str(pols),freqs[fq].str().strip()))

		all_fqs_used = all_fqs_used + d.recfreqindex + d.zoomfreqindex

	print("")

	# Print out all FREQ entries not referenced by DATASTREAMS
	all_fqs_used = set(all_fqs_used)
	unused_fqs = set(range(numfreqs)) - all_fqs_used
	print("Frequencies not referenced by any DATASTREAMs:")
	if len(unused_fqs) < 1:
		print((" "*indent) + "(none)")
	else:
		for n in range(len(unused_fqs)):
			fq = unused_fqs[n]
			print((" "*indent) + freqs[fq].str().strip())	
	print("")

	# Print out all BASELINEs
	print("Content of BASELINE table:")
	for b in baselines:
		ds1 = datastreams[b.dsaindex]
		ds2 = datastreams[b.dsbindex]		
		print((" "*1*indent) + "Baseline %s x %s / DS %2d x %2d" % (telescopes[ds1.telescopeindex].name,telescopes[ds2.telescopeindex].name,b.dsaindex,b.dsbindex))
		if len(b.dsabandindex) != len(b.dsbbandindex):
			print((" "*2*indent) + "error: lenghts of ds<X>bandindex do not match (DS A: %d and DS B: %d)" % (len(b.dsabandindex),len(b.dsbbandindex)))
		for n in range(len(b.dsabandindex)):
			bl_bands_1 = b.dsabandindex[n]
			bl_bands_2 = b.dsbbandindex[n]
			print((" "*2*indent) + "Cross-products set %d:" % (n))
			for (bl_band_1,bl_band_2) in zip(bl_bands_1,bl_bands_2):
				fq1,pol1 = getFreqPolOfBand(ds1,bl_band_1)
				fq2,pol2 = getFreqPolOfBand(ds2,bl_band_2)
				sfq1 = freqs[fq1].str().strip()
				sfq2 = freqs[fq2].str().strip()
				fqtype1, fqtype2 = 'rec ', 'rec '
				if bl_bands_1 >= ds1.nrecband: fqtype1 = 'zoom'
				if bl_bands_2 >= ds2.nrecband: fqtype2 = 'zoom'
				print((" "*3*indent) + "%s %2d x %s %2d : %s%s : fq %2d x %2d : %s x %s" % (fqtype1,bl_band_1,fqtype2,bl_band_2, pol1,pol2, fq1,fq2,sfq1,sfq2))


if __name__ == "__main__":

	if len(sys.argv) < 2:
		print __doc__
		sys.exit(-1)

	for difxf in sys.argv[1:]:
		printDiFXInput(difxf)

