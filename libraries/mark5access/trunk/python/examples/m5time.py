#!/usr/bin/python
import ctypes, sys
import mark5access as m5lib
from datetime import datetime

fmt_supported = [
	m5lib.MK5_FORMAT_VLBA, 
	m5lib.MK5_FORMAT_MARK5B,
	m5lib.MK5_FORMAT_VDIF,
	m5lib.MK5_FORMAT_VDIFL,
	m5lib.MK5_FORMAT_VDIFB ]

EXIT_FAILURE = 1   # sys.exit() return code on errors
Ncheck = 10        # num of consecutive frames to check for consistent MJD

def usage():
	print (' ')
	print ('A time decoder for raw VLBI data. Reads formats supported by the mark5access library.')
	print (' ')
	print ('Usage : m5time <file> <dataformat> <offset>')
	print (' ')
	print ('  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:')
	print ('    VLBA1_2-256-8-2')
	print ('    MKIV1_4-128-2-1')
	print ('    Mark5B-512-16-2')
	print ('    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)')
	print (' ')
	print ('  <offset> is number of bytes into file to return time for')
	print (' ')

def m5time(fn, fmt, offset):
	"""Reports first timestamp in file and verifies additional timestamps are consistent."""
	try:
		m5file = m5lib.new_mark5_stream_file(fn, ctypes.c_longlong(offset))
		m5fmt  = m5lib.new_mark5_format_generic_from_string(fmt)
		ms     = m5lib.new_mark5_stream_absorb(m5file, m5fmt)
		dms    = ms.contents
	except:
		print ("Error: problem opening or decoding %s\n" % (fn));
		return EXIT_FAILURE

	if not(dms.format in fmt_supported):
		print('File format is not supported by m5ime.py')
		return EXIT_FAILURE

	mjd     = dms.mjd
	sec     = dms.sec + 1e-9*dms.ns
	now     = datetime.utcnow()
	now_mjd = date_to_mjd(now)

	if dms.format in [m5lib.MK5_FORMAT_VLBA,m5lib.MK5_FORMAT_MARK5B]:
		mjd = mjd + (now_mjd-(now_mjd%1000))
		if (mjd > now_mjd):
			mjd = mjd-1000	

	(r,ss) = divmod(sec, 60)
	(hh,mm) = divmod(r, 60)

	mjd_curr = ms.contents.mjd
	for ii in range(Ncheck):
		m5lib.mark5_stream_next_frame(ms)
		mjd_next = ms.contents.mjd
		if ((mjd_next - mjd_curr) >= 2) or (mjd_curr > now_mjd):
			print ('Possibly wrong file format specified : MJD in frame %d jumped from %d to %d.' % (ii,mjd_curr,mjd_next))
			return EXIT_FAILURE
		mjd_curr = mjd_next

	print ('MJD = %d/%02d:%02d:%05.2f' % (mjd,hh,mm,ss))
	return 0

def date_to_mjd(dtime):
	"""Returns the Julian day number of a date."""
	a = (14 - dtime.month)//12
	y = dtime.year + 4800 - a
	m = dtime.month + 12*a - 3
	j = dtime.day + ((153*m + 2)//5) + 365*y + y//4 - y//100 + y//400 - 32045
	return j - 2400000.5

def main(argv=sys.argv):

	if len(argv) not in [3,4]:
		usage()
		sys.exit(EXIT_FAILURE)

	offset = 0
	if len(argv) == 4:
		offset = int(argv[3])

	rc = m5time(argv[1], argv[2], offset)
	return rc

if __name__ == "__main__":
	sys.exit(main())
