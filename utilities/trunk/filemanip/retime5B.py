#!/usr/bin/env python
#
# Change the time stamp of a file to start at a new
# base second and MJD. Does not modify the fractional 
# second i.e. the optional user defined field in Mark5B 
# header.
#
# Useful for correcting Mark5B files, especially those
# from RDBE with 03/2012-12/2012 timestamp bug.
#
# (C) 2012 Jan Wagner
#

import sys, string, os, struct
import pylibMk5B

print_all_headers = True

def main(argv):
	
	if len(sys.argv)<5:
		print "Usage: %s <infile> <outfile> <new MJD> <new 1st second> " % (sys.argv[0])
		sys.exit()

	fname_in   = sys.argv[1]
	fname_out  = sys.argv[2]
	new_mjd    = int(sys.argv[3])
	new_second = int(sys.argv[4])

	# Open files
        try:
		fdin = open(fname_in,'r')
	except:
		print "\nCould not open input file %s " % (fname_in)
		sys.exit(-1)
        try: 
		fdout = open(fname_out,'w')
	except:
		print "\nCould create output file %s " % (fname_out)
		sys.exit(-1)

	# First header
	(header,payload,fileoffset) = pylibMk5B.getNextFrame(fdin)
	(day,hour,minute,second,fracSeconds,frameNr,secsInDay) = pylibMk5B.getTime(header)
	timestr = pylibMk5B.getTimeString(header)
	print 'Input file first header at offset=%u: %s' % (fileoffset,timestr)

	# Update all headers
	fileoffset_prev = fileoffset
	secsInDay_prev = secsInDay
	while (True):

		# Modify the header
		header = pylibMk5B.setTime(header, new_mjd, new_second)

		# Write updated data
		fdout.write(header)
		fdout.write(payload)

		# Get next data frame
		secsInDay_prev = secsInDay
		(header,payload,fileoffset) = pylibMk5B.getNextFrame(fdin)
		if (header == None):
			sys.exit('EOF while copying data')

		(day,hour,minute,second,fracSeconds,frameNr,secsInDay) = pylibMk5B.getTime(header)
		if (secsInDay != secsInDay_prev):
			new_second = new_second + 1
			if (new_second >= 24*60*60):
				new_second = 0
				new_mjd = new_mjd + 1

			bytes = fileoffset - fileoffset_prev
			timestr = pylibMk5B.getTimeString(header)
			print 'Input File Time: %s @ %d (0x%x) : offset delta %d bytes' % (timestr, fileoffset, fileofsecondfset, bytes)
			fileoffset_prev = fileoffset

	# Done!
	try:	
		timestr = pylibMk5B.getTimeString(header)
		print 'Stop Time: %s' % (timestr)
	except:
		pass

	fdin.close()
	fdout.close()


if __name__ == "__main__":
    main(sys.argv)
