#!/usr/bin/python3
import copy, sys, os, struct, time, math
import parseDiFX
import numpy
from optparse import OptionParser

helpstr = "diffDiFX2.py [options] <basename A> <basename B>\n\n"
helpstr += "Compares visibility records in two DiFX output files. "
helpstr += "Reports if the mean difference ever exceeds THRESHOLD."

suffixes= ('.difx','.input','.calc','.im','.difxlog')

parser = OptionParser(helpstr)
parser.add_option("-f", "--freq", dest="freq", metavar="FREQ", default="-1",
                  help="Only look at visibilities from this FREQ index")
parser.add_option("-b", "--baseline", dest="baseline", metavar="BASELINE", default="-1",
                  help="Only look at visibilities from this BASELINE num")
parser.add_option("-t", "--threshold", dest="threshold", metavar="THRESHOLD", default="0.0005",
                  help="Display any difference that exceeds THRESHOLD percent")
parser.add_option("-e", "--epsilon", dest="epsilon", metavar="EPSILON", default="-1",
                  help="Display any difference that exceeds allowed numerical error EPSILON")
parser.add_option("-m", "--maxrecords", dest="maxrecords", metavar="MAXRECORDS",
                  default="-1", help="Stop after comparing MAXRECORDS (if >0) records")
parser.add_option("-w", "--warn-undiffable", dest="warnundiffable", action="store_true", default=False,
                  help="Show visibility records that cannot be diffed")
parser.add_option("-P", "--phasecenter", dest="phasecenterId",
                  default="None", help="Phase center (0..n-1) to compare")
parser.add_option("-B", "--pulsarbin", dest="pulsarbinId",
                  default="None", help="Pulsar bin (0..n-1) to compare")
parser.add_option("-v", "--verbose", dest="verbose", action="store_true", default=False,
                  help="Turn verbose printing on")

(options, args) = parser.parse_args()

if len(args) != 2:
    parser.error("You must supply two (and only two) difx files to diff!")

targetbaseline = int(options.baseline)
targetfreq     = int(options.freq)
threshold      = float(options.threshold) / 100.0
epsilon        = float(options.epsilon)
maxrecords     = int(options.maxrecords)
warnundiffable = options.warnundiffable
verbose        = options.verbose
pulsarbin      = None
phasecenter    = None
try:
    phasecenter = int(options.phasecenterId)
except:
    pass
try:
    pulsarbin = int(options.pulsarbinId)
except:
    pass


class MetaMapper:

	def __init__(self, A: parseDiFX.DiFXFile, B: parseDiFX.DiFXFile):
		self.fqMap = self.generateFreqMap(A, B)
		self.antMap = self.generateAntennaMap(A,B)


	def findFreqIdx(self, fq: parseDiFX.Freq, others: []) -> int:
		foundIdx = -1
		for foidx in range(len(others)):
			if fq == others[foidx]:
				if foundIdx != -1:
					print('Warning: diff may not end well, there are non-unique matches for freq %s' % (fq.str()))
				foundIdx = foidx
		return foundIdx


	def generateFreqMap(self, A: parseDiFX.DiFXFile, B: parseDiFX.DiFXFile) -> []:
		numoutFqA, outFqIdsA = A.metainfo.determine_outputfreqs()
		numoutFqB, outFqIdsB = B.metainfo.determine_outputfreqs()
		outFqsA = [A.metainfo.freqs[j] for j in outFqIdsA]
		outFqsB = [B.metainfo.freqs[j] for j in outFqIdsB]

		mapping = [-1] * (max(max(outFqIdsA),max(outFqIdsB)) + 1)
		trivial = True

		for fqIdA in outFqIdsA:
			fq = A.metainfo.freqs[fqIdA]
			bidx = self.findFreqIdx(fq, outFqsB)
			fqIdB = outFqIdsB[bidx]
			mapping[fqIdA] = fqIdB
			trivial = trivial and fqIdA==fqIdB

		if not trivial:
			nmapped = sum(mp >= 0 for mp in mapping)
			print("Files differ in output frequency IDs, mapped %d of A's %d freqs to B's %d freqs" % (nmapped,numoutFqA,numoutFqB))

		if verbose and not trivial:
			print('File A: .input version %s, num output freqs %d, indices %s' % (A.metainfo.version, numoutFqA, outFqIdsA))
			print('File B: .input version %s, num output freqs %d, indices %s' % (B.metainfo.version, numoutFqB, outFqIdsB))
			print('Freq map A->B : ', end='')
			for j in range(len(mapping)):
				if (mapping[j]>=0):
					print('%d->%d ' % (j,mapping[j]), end='')
			print()
			for j in range(len(mapping)):
				m = mapping[j]
				if (m>=0):
					print('fq#%-2d %s --> fq#%-2d %s' % (j, A.metainfo.freqs[j].str(), m, B.metainfo.freqs[m].str()))

		return mapping


	def generateAntennaMap(self, A: parseDiFX.DiFXFile, B: parseDiFX.DiFXFile) -> []:
		mapping = [{}, [-1]*A.metainfo.numtelescopes]
		trivial = True

		for tIdA in range(A.metainfo.numtelescopes):
			nameA = A.metainfo.telescopes[tIdA].name
			for tIdB in range(B.metainfo.numtelescopes):
				nameB = B.metainfo.telescopes[tIdB].name
				if nameA == nameB:
					trivial = trivial and tIdA == tIdB
					mapping[0][nameA] = tIdB
					mapping[1][tIdA] = tIdB
					break

		if verbose:
			print('Antenna map A->B: %s' % (str(mapping[1])))

		if not trivial or A.metainfo.numtelescopes != B.metainfo.numtelescopes:
			nmapped = sum(mp >= 0 for mp in mapping[1])
			print("Files differ in telescope IDs, mapped %d of A's %d telescopes to B's %d telescopes" % (nmapped,A.metainfo.numtelescopes,B.metainfo.numtelescopes))

		return mapping


	def remapHeader(self, A: parseDiFX.VisibilityHeader, alt: bool = False) -> parseDiFX.VisibilityHeader:

		if (A == None):
			return None

		bb = copy.deepcopy(A)

		antA1_idx, antA2_idx = A.antenna1-1, A.antenna2-1
		antB1_idx, antB2_idx = self.antMap[1][antA1_idx], self.antMap[1][antA2_idx]

		bb.freqindex = self.fqMap[A.freqindex]
		bb.antenna1 = antB1_idx + 1
		bb.antenna2 = antB2_idx + 1

		if alt:
			bb.baseline = 256 * bb.antenna1 + bb.antenna2
		else:
			bb.baseline = 256 * bb.antenna1 + bb.antenna2

		if (bb.antenna1 < 1) or (bb.antenna2 < 1):
			#print('No counterpart for antennas A:%d-%d in B' % (A.antenna1,A.antenna2))
			return None

		return bb



class DiFXAPSet:

	def __init__(self, difx: parseDiFX.DiFXFile):
		self.difx = difx
		self.fmjd = 0
		self.ap_counter = 0
		self.ap_offset = 0
		self.headers = []
		self.offsets = []
		self.moveToNextAP()


	def getVisibilityOffsets(self) -> []:
		return zip(self.headers, self.offsets)


	def scanCurrentAP(self) -> int:
		self.headers = []
		self.offsets = []
		self.difx.difxfile.seek(self.ap_offset)

		while True:

			offset = self.difx.difxfile.tell()
			visrec = self.difx.nextVisibilityRecord()
			if not visrec.header.isvalid():
				break

			T = visrec.header.mjd + visrec.header.seconds/86400.0
			if T > self.fmjd:
				self.difx.difxfile.seek(self.ap_offset)
				break

			#print(visrec.header.baseline, visrec.header.freqindex, visrec.header.polpair)
			self.headers.append(copy.deepcopy(visrec.header))
			self.offsets.append(offset)

		return len(self.offsets)


	def __compareHeader(self, A: parseDiFX.VisibilityHeader, B: parseDiFX.VisibilityHeader) -> bool:
		isSimilar = (A.baseline == B.baseline) and (A.freqindex == B.freqindex) and (A.polpair == B.polpair)
		isSimilar = isSimilar and (A.mjd == B.mjd) and (A.seconds == B.seconds)
		return isSimilar	
	

	def findMatchingHeader(self, hdr: parseDiFX.VisibilityHeader) -> int:
		if (hdr == None):
			return -1
		for n in range(len(self.headers)):
			if self.__compareHeader(hdr, self.headers[n]):
				return self.offsets[n]
		return -1


	def loadRecord(self, offset: int) -> parseDiFX.VisibilityRecord:
		offset_prev = self.difx.difxfile.tell()
		self.difx.difxfile.seek(offset)
		rec = self.difx.nextVisibilityRecord()
		self.difx.difxfile.seek(offset_prev)
		return rec


	def moveToNextAP(self) -> bool:
		# First time to get AP
		if self.fmjd == 0:
			offset = self.difx.difxfile.tell()
			visrec = self.difx.nextVisibilityRecord()

			if not visrec.header.isvalid():
				return False

			self.ap_counter += 1
			self.fmjd = visrec.header.mjd + visrec.header.seconds/86400.0
			self.ap_offset = offset
			self.difx.difxfile.seek(offset)
			return True

		# Look for time change
		while True:
			offset = self.difx.difxfile.tell()
			visrec = self.difx.nextVisibilityRecord()

			if not visrec.header.isvalid():
				return False

			T = visrec.header.mjd + visrec.header.seconds/86400.0
			if self.fmjd != T:
				self.ap_counter += 1
				self.fmjd = T
				self.ap_offset = offset
				self.difx.difxfile.seek(offset)
				return True



def getDiFXFile(basename: str, phasecenter, pulsarbin) -> parseDiFX.DiFXFile:
	'''Open an pre-parse a DiFX file and metadata'''
	if basename.endswith(suffixes):
		basename = basename[:basename.rfind('.')]
	inputfile = basename + '.input'
	difx = parseDiFX.DiFXFile()
	difx.open(inputfile,phasecenterId=phasecenter,pulsarbinId=pulsarbin)
	return difx



if __name__ == "__main__":

	A = getDiFXFile(args[0], phasecenter, pulsarbin)
	B = getDiFXFile(args[1], phasecenter, pulsarbin)
	if A.difxfile is None or B.difxfile is None:
		sys.exit(1)

	metamap = MetaMapper(A,B)

	apsA = DiFXAPSet(A)
	apsB = DiFXAPSet(B)
	allOk = True
	filesIdentical = True

	tmp, outFreqIsA = A.metainfo.determine_outputfreqs()

	while allOk:

		if (maxrecords > 0) and (apsA.ap_counter >= maxrecords):
			break

		# Make sure both files are at the same MJD-sec averaging period
		if apsA.fmjd < apsB.fmjd:
			print('A lags B; A at MJD %f vs. B at %f, num aps %d' % (apsA.fmjd, apsB.fmjd, apsA.ap_counter))
			allOk = apsA.moveToNextAP()
			continue
		if apsB.fmjd < apsA.fmjd:
			print('A leads B; A at MJD %f vs. B at %f, num aps %d' % (apsA.fmjd, apsB.fmjd, apsA.ap_counter))
			allOk = apsB.moveToNextAP()
			continue

		# Determine positions of visibility records in current MJD-sec averaging period
		numVizA = apsA.scanCurrentAP()
		numVizB = apsB.scanCurrentAP()
		if numVizA <=0 or numVizB <= 0:
			allOk = False
			break

		# Compare all visibility records of the current MJD-sec averaging period
		foundOneRecord = False
		for (hdrA, offsetA) in apsA.getVisibilityOffsets():

			if (targetbaseline >= 0) and (hdrA.baseline != targetbaseline):
				continue
			if (targetfreq >= 0) and (hdrA.freqindex != targetfreq):
				continue

			if hdrA.freqindex not in outFreqIsA:
				if warnundiffable:
					print('SPURIOUS: file A has record for freq %d not in output freqs %s: cannot diff %s-%s/%d fq %d/%s MJD %d sec %7.2f' % (
						hdrA.freqindex, str(outFreqIsA), A.metainfo.telescopes[hdrA.antenna1-1].name, A.metainfo.telescopes[hdrA.antenna2-1].name,
						hdrA.baseline, hdrA.freqindex, hdrA.polpair, hdrA.mjd, hdrA.seconds))
				continue

			# Look up record from file B
			hdrB_expect = metamap.remapHeader(hdrA)
			offsetB = apsB.findMatchingHeader(hdrB_expect)
			if (offsetB < 0):
				if warnundiffable:
					print('NOT FOUND: no counterpart for record in B: %s-%s/%d fq %d/%s MJD %d sec %7.2f' % (
						A.metainfo.telescopes[hdrA.antenna1-1].name, A.metainfo.telescopes[hdrA.antenna2-1].name,
						hdrA.baseline, hdrA.freqindex, hdrA.polpair, hdrA.mjd, hdrA.seconds))
				continue

			# Load the full data
			vizA = apsA.loadRecord(offsetA)
			vizB = apsB.loadRecord(offsetB)
			foundOneRecord = True

			# Data tag str for printing
			tag = '%s-%s/A:%-3d/B:%-3d fq %.3f/%s/%d/%d MJD %d sec %7.2f : A@%d B@%d' % (
				A.metainfo.telescopes[vizA.header.antenna1-1].name, A.metainfo.telescopes[vizA.header.antenna2-1].name,
				vizA.header.baseline, vizB.header.baseline,
				A.metainfo.freqs[vizA.header.freqindex].freq, vizA.header.polpair, vizA.header.freqindex, vizB.header.freqindex,
				vizA.header.mjd, vizA.header.seconds,
				offsetA, offsetB
			)

			# Num channels must match for diffing
			if len(vizA.vis) != len(vizB.vis):
				print('NUMBER OF CHANNELS DIFFER: A:%d B:%d on %s' % (len(vizA.vis),len(vizB.vis),tag))
				continue

			# Diffing
			identicalViz = True
			nonequalchanslist = []		
			for j in range(len(vizA.vis)):
				diff = vizA.vis[j] - vizB.vis[j]
				if (epsilon > 0) and (abs(diff) > epsilon):
					nonequalchanslist.append(j)
				elif (threshold > 0) and (abs(diff)/abs(vizA.vis[j]) >= threshold):
					nonequalchanslist.append(j)
			refavg = numpy.mean(numpy.abs(vizA.vis))
			vdiff = vizA.vis - vizB.vis
			absdiffavg = numpy.mean(numpy.abs(vdiff))
			meandiffavg = numpy.mean(vdiff)
			if (absdiffavg/refavg >= threshold):
				print('THRESHOLD EXCEEDED: absolute difference %.4f%%, mean difference %.4f%% + i%-.4f%% on %s' % (100.0*absdiffavg/refavg, 100.0*numpy.real(meandiffavg)/refavg, 100.0*numpy.imag(meandiffavg)/refavg, tag))
				identicalViz = False
			if nonequalchanslist:
				print('EPSILON EXCEEDED: numerically significant difference in %d out of %d channels on %s' % (len(nonequalchanslist),len(vizA.vis),tag))
				identicalViz = False

			# Show differences
			if not identicalViz:
				print('  Channels: %s' % (str(nonequalchanslist)))
				if True: #if verbose:
					prec = 7
					for ch in nonequalchanslist:
						print('    Ch %-3d   A: %s\n'
                              '             B: %s' % (ch,
							numpy.array2string(vizA.vis[ch], precision=prec),
							numpy.array2string(vizB.vis[ch], precision=prec)))

			if verbose and identicalViz:
				print('Identical within numeric precision: %s' % (tag))

			filesIdentical = filesIdentical and identicalViz

		if not foundOneRecord:
			print('No records matching filter criteria in AP set of MJD %f' % (apsA.fmjd))

		allOk = apsA.moveToNextAP()
		allOk = allOk and apsB.moveToNextAP()

	if filesIdentical:
		sys.exit(0)
	sys.exit(1)

