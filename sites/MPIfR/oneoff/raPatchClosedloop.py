#!/usr/bin/python
"""
Patch a DiFX/CALC .im file delay and uvw polynomials with RadioAstron closed-loop correction files.

Usage: raPatchClosedloop.py <dly_polys.txt> <uvw_polys.txt> <difxbasename1.im> [<difxbasename2.im> ...]

Input:
    dly_polys.txt     RadioAstron closed-loop delay polynomials
    uvw_polys.txt     RadioAstron closed-loop u,v,w polynomials
    difxbasename1.im  original DiFX/CALC .im file to patch

Output:
    difxbasename1.im.closedloop
"""
from datetime import datetime, timedelta
from calendar import timegm
import sys, time

class PolyCoeffs:
	"""A single polynomial with coefficients"""

	source = ''
	Ncoeffs = 0
	dims = 0
	coeffs = []
	tstart = datetime.utcnow() 
	tstop = tstart
	interval = 0

	def getArgs(self, line):
		return line.split('=')[-1].strip()

	def raStrToTime(self, s):
		"""Parse 'dd/mm/yyyy HHhMMmSSs' into datetime"""
		# https://aboutsimon.com/blog/2013/06/06/Datetime-hell-Time-zone-aware-to-UNIX-timestamp.html
		gm = timegm( time.strptime(s + ' GMT', '%d/%m/%Y %Hh%Mm%Ss %Z') )
		d = datetime.utcfromtimestamp(gm)
		# print (s, d, d.isoformat())
		return d

	def __init__(self, details):
		"""Initialize coeffs using a list of strings stored in 'details'.
		The list of strings should have the following format, with 'source',
		'start', 'stop', and a varying number of P0..P<n> coefficient lines.
		Coefficient line k should contain one (1D) or more (e.g. 3D) coefficients
		for the k-th power terms (x^k * Px_k + y^k * Py_k + z^k * Pz_k + ...)

		source = 0716+714
		start = 19/10/2017 09h00m00s
		stop = 19/10/2017 09h01m00s
		P0 = -1.00975710299507e-001, -2.49717010234864e-001, 5.28185040398344e-001
		P1 = 9.18124737968820e-007, -4.59083189789325e-006, 1.99785028077570e-006
		P2 = 3.57024938682854e-012, 8.83446311517487e-012, -1.87750268902654e-011
		P3 = -7.78908442044021e-016, 1.48057049755645e-015, 8.27468673485702e-016
		P4 = 1.41735592450728e-017, -2.51428930101850e-017, -1.61417000499427e-017
		P5 = -9.33938621249164e-020, 1.57978134340036e-019, 1.12339170239300e-019
		"""
		self.Ncoeffs = len(details) - 3
		assert(self.Ncoeffs >= 1)
		self.source = self.getArgs(details[0])
		self.tstart = self.raStrToTime( self.getArgs(details[1]) )
		self.tstop = self.raStrToTime( self.getArgs(details[2]) )
		self.interval = (self.tstop - self.tstart).total_seconds()
		self.coeffs = []
		for n in range(self.Ncoeffs):
			cstr = self.getArgs(details[3+n])
			c = [float(s) for s in cstr.split(',')]
			self.dims = len(c)
			self.coeffs.append(c)
		# print ('\n')
		# print (self.source, self.Ncoeffs, self.tstart, self.tstop, self.coeffs)

class PolySet:
	"""Storage of a set of polynomials"""

	piecewisePolys = []
	dims = 0

	def __init__(self, filename):
		"""
		Create object and load a set of polynomials and their coefficients from a file.
		"""
		self.piecewisePolys = []
		with open(filename) as f:
			lines = f.readlines()
			lines = [l.strip() for l in lines]
		if len(lines) < 1:
			print ('Error: could not read %s' % (filename))
			return
		if not 'RASTRON' in lines[0]:
			print ('Unexpected delay poly file content')
			return
		N = int(lines[1].split('=')[1])
		lnr = 3
		while lnr < len(lines):
			assert('source' in lines[lnr])
			polyDefinition = lines[lnr:(lnr+3+N)]
			self.piecewisePolys.append( PolyCoeffs(polyDefinition) )
			self.dims = self.piecewisePolys[-1].dims
			lnr += 3 + N

	def __len__(self):
		return len(self.piecewisePolys)

	def lookupPolyFor(self,MJD,sec):
		"""
		Lookup up poly that was start time identical to the given MJD and second-of-day
		"""
		mjd_t0 = datetime(1858,11,17,0,0,0,0) # MJD 0 = 17 November 1858 at 00:00 UTC
		tlookup = mjd_t0 + timedelta(days=MJD) + timedelta(seconds=sec)
		# print (mjd_t0,tlookup)
		for poly in self.piecewisePolys:
			if poly.tstart == tlookup:
				return poly
			if poly.tstart < tlookup and poly.tstop > tlookup:
				dt = (tlookup - poly.tstart).total_seconds()
				print ('Error: Time %d MJD %d sec (%s) not at start but rather %d seconds into RA poly.' % (MJD,sec,str(tlookup),dt))
				print ('       Poly time-shift not supported yet!')
				return None
		return None


def imDetectNextScanblock(lines,nstart):
	"""
	Look for next SCAN block in IM file lines, starting from line nr 'nstart'.
	Returns (istart,istop,srcname,mjd,sec) of the next scan block, or None.
	"""
	n = nstart
	while n < len(lines):
		if 'SCAN' in lines[n] and 'POINTING SRC' in lines[n]:
			break
		n += 1
	if (n + 6) >= len(lines):
		return None

	srcname = lines[n].split(':')[1].strip()
	mjd = int(lines[n+4].split(':')[1])
	sec = int(lines[n+5].split(':')[1])
	n += 4
	istart = n
	while n < len(lines):
		if 'SCAN' in lines[n] and 'POINTING SRC' in lines[n]:
			break
		n += 1
	istop = n - 1
	return (istart,istop,srcname,mjd,sec)


def imDetectNextScanpolyblock(lines,nstart,nstop):
	"""
	Looks for next "SCAN <n> POLY <m>" block in file lines, starting from line nr 'nstart'.
	Returns (istart,istop,mjd,sec) of the next scan poly block, or None.
	"""
	mjd = 0
	sec = 0
	n = nstart
	while n < len(lines):
		if 'SCAN' in lines[n] and 'POLY' in lines[n]:
			break
		n += 1
	if (n + 2) >= len(lines):
		return None

	mjd = int(lines[n].split(':')[1])
	sec = int(lines[n+1].split(':')[1])
	n += 2
	nstart = n

	while n < len(lines) and n < nstop:
		if 'SCAN' in lines[n] and ('POINTING SRC' in lines[n] or 'POLY' in lines[n]):
			break
		n += 1
	nstop = n - 1
	return (nstart,nstop,mjd,sec)


def imSumPolyCoeffs(telescope_id,lines,polystart,polystop,dpoly,uvwpoly,sign=+1):
	N_updated = 0
	tag_t = 'ANT %d DELAY (us)' % telescope_id
	tag_u = 'ANT %d U (m)' % telescope_id
	tag_v = 'ANT %d V (m)' % telescope_id
	tag_w = 'ANT %d W (m)' % telescope_id
	for n in range(polystart,polystop):
		doPatch = False
		if tag_t in lines[n]:
			P,col = dpoly,0
			doPatch = True
		if tag_u in lines[n]:
			P,col = uvwpoly,0
			doPatch = True
		if tag_v in lines[n]:
			P,col = uvwpoly,1
			doPatch = True
		if tag_w in lines[n]:
			P,col = uvwpoly,2
			doPatch = True
		if doPatch:
			C = [ pp[col] for pp in P.coeffs ]
			key,oldcoeffs = lines[n].split(':')
			oldcoeffs = [float(v) for v in oldcoeffs.split('\t')]
			newcoeffs = list(oldcoeffs)

			# Element wise summation of correction coeffs onto polynomial
			N = min(len(newcoeffs), len(C)) # truncate to either poly
			for k in range(N):
				newcoeffs[k] += sign*C[k]
			newcoeffs_str = ' '.join(['%.16e\t ' % v for v in newcoeffs])
			newline = '%s:  %s' % (key.strip(),newcoeffs_str)
			#print (lines[n])
			#print (newline)
			lines[n] = newline
			N_updated += 1
	return N_updated


def patchImFile(basename, dlypolys, uvwpolys, antname='GT'):
	if basename.endswith(('.difx','input','.calc','.im')):
		basename = basename[:basename.rfind('.')]
	imname = basename + '.im'
	imoutname = basename + '.im.closedloop'

	lines = []
	with open(imname) as f:
		lines = f.readlines()
		lines = [l.strip() for l in lines]
	if len(lines)<16:
		print ('Error: problem loading a valid %s!' % (imname))
		return False

	# Find telescope, poly order, poly interval
	telescope_id = None
	im_poly_order = 0
	im_poly_interval_s = 0
	antname = antname.upper()
	for line in lines:
		# 'TELESCOPE 0 NAME:   GT'
		if 'TELESCOPE' in line and 'NAME' in line:
			[key,val] = line.split(':')
			if val.strip().upper() == antname:
				telescope_id = int( key.split()[1])
				break
		# 'POLYNOMIAL ORDER:   5'
		if 'POLYNOMIAL ORDER' in line:
			im_poly_order = int(line.split(':')[1])
		# 'INTERVAL (SECS):    120'
		if 'INTERVAL (SECS)' in line:
			im_poly_interval_s = int(line.split(':')[1])

	# Consistency check IM <-> RA coeffs
	if telescope_id == None:
		print ('Error: could not find telescope %s in %s' % (antname,imname))
	for poly in dlypolys.piecewisePolys + uvwpolys.piecewisePolys:
		if poly.Ncoeffs > (im_poly_order+1):
			print ('Error: mismatch in polynomial order of .im file (%d) and closed-loop file (%d).' % (im_poly_order, poly.Ncoeffs-1))
			return False
		if poly.interval < im_poly_interval_s:
			print ('Error: too long polynomial validity interval in .im file (%d sec) for appyling closed-loop polys (%d sec).' % (im_poly_interval_s, poly.interval))
			return False

	# Patch all relevant IM file lines
	nupdated_total = 0
	n = 0
	print ("Patching telescope %s with id %d in DiFX/CALC '%s' into new file '%s'" % (antname,telescope_id,imname,imoutname))
	while n < len(lines):
		# Find next 'SCAN' block
		blk = imDetectNextScanblock(lines,n)
		if blk == None:
			break
		(blkstart,blkstop,srcname,mjd,sec) = blk

		# Check all 'SCAN <n> POLY <m>' sections of the block
		pstart = blkstart
		while True:

			# Get next 'POLY <m>'
			blkpoly = imDetectNextScanpolyblock(lines,pstart,blkstop)
			if blkpoly == None:
				break
			(polystart,polystop,mjd,sec) = blkpoly

			# Get the matching Closed Loop polynomial coeffs sets
			dp = dlypolys.lookupPolyFor(mjd,sec)
			uvwp = uvwpolys.lookupPolyFor(mjd,sec)
			if dp == None or uvwp == None:
				print('Error: no suitable poly found in coeffs file to match MJD %d sec %d!' % (MJD,sec))
				return False
			if dp.source != uvwp.source:
				print("Error: RA delay poly source '%s' does not match UVW poly source '%s'!" % (dp.source,uvwp.source))
			if dp.source != srcname:
				print("Error: IM source '%s' does not match poly coeff set source '%s'!" % (srcname,dp.source))
				return False

			# Apply the coeffs
			nupdated = imSumPolyCoeffs(telescope_id,lines,polystart,polystop,dp,uvwp)
			nupdated_total += nupdated

			pstart = polystop + 1

		n = blkstop + 1

	# Write the new IM file
	f = open(imoutname, 'w')
	for line in lines:
		f.write(line + '\n')
	print ("Wrote new file '%s' with %d updated coefficient lines." % (imoutname,nupdated_total))
	print ('Done.')

	return True

if __name__ == "__main__":

	if len(sys.argv) < 3:
		print(__doc__)
		sys.exit(-1)

	dly = PolySet(sys.argv[1])
	if len(dly) < 1:
		print ("Error: could not load delay polynomials from '%s'" % (sys.argv[1]))
		sys.exit(-1)
	uvw = PolySet(sys.argv[2])
	if len(uvw) < 1:
		print ("Error: could not load u,v,w polynomials from '%s'" % (sys.argv[2]))
		sys.exit(-1)

	if dly.dims != 1 or uvw.dims != 3:
		print ("Error: coeff file poly dimensions mismatch! Expected dim. 1 for 1st file '%s' (%d), dim. 3 for 2nd file '%s' (%d)" % (sys.argv[1], dly.dims, sys.argv[2], uvw.dims))
		sys.exit(-1)

	for difxf in sys.argv[3:]:
		patchImFile(difxf, dly, uvw)
