#!/usr/bin/env python
#
# Extract a time range of Mark5B data into another file.
#
# Useful for preparing files for a quick cross-correlation check
# in some lightweight correlator such as 'vlbi2'.
#
# (C) 2012 Jan Wagner
#

import sys, string, os, struct
import pylibMk5B

print_all_headers = True

# Compares header to certain time stamp
# Returns: (True/False,secsInDay)
def match(header,hh,mm,ss):
	(day,hour,minute,second,fracSeconds,frameNr,secsInDay) = pylibMk5B.getTime(header)
	found = (hour==hh and minute==mm and second==ss)
	return (found,secsInDay)

def main(argv):
	
	if len(sys.argv)<4:
		print "Usage: %s <infile> <outfile> <+s|hh:mm:ss> [<number of seconds to extract>] " % (sys.argv[0])
		sys.exit()

	fname_in   = sys.argv[1]
	fname_out  = sys.argv[2]
	ext_time   = sys.argv[3]
	ext_dur    = 1
	if len(sys.argv)==5:
		ext_dur = int(sys.argv[4])

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

	# Determine time range (start time)
	print ext_time
	if (ext_time[0]=='+'):
		sec_offset = int(ext_time[1:])
		print 'Using relative time offset of %u seconds' % (sec_offset)
                (header,payload,fileoffset) = pylibMk5B.getNextFrame(fdin)
                if (header == None):
                        sys.exit('EOF while looking for first time stamp')
		(day,hour,minute,second,fracSeconds,frameNr,secsInDay) = pylibMk5B.getTime(header)
		match_hh = hour
		match_mm = minute
		match_ss = second + sec_offset
		match_mm = match_mm + int(match_ss/60)
		match_hh = match_hh + int(match_mm/60)
		match_mm = match_mm % 60
		match_ss = match_ss % 60
	else:
		match_hh = int(ext_time[:2])
		match_mm = int(ext_time[3:5])
		match_ss = int(ext_time[6:8])
		
	# Search for timestamp
	print 'Looking for %02u:%02u:%02u' % (match_hh, match_mm, match_ss)
	while True:

		(header,payload,fileoffset) = pylibMk5B.getNextFrame(fdin)
		if (header == None):
			sys.exit('EOF while looking for specified time')

		(found,secsinday) = match(header, match_hh, match_mm, match_ss)

		if (found):
			break

	# Show detected time and position
	(day,hour,minute,second,fracSeconds,frameNr,secsInDay) = pylibMk5B.getTime(header)
	timestr = pylibMk5B.getTimeString(header)
	print 'Found specified time in input file, file offset=%u' % (fileoffset)
	print 'Full timestamp: %s' % (timestr)

	# Extract given number of data seconds
	fileoffset_prev = fileoffset
	secsInDay_prev = secsInDay
	while (ext_dur>0):
		
		# Write previous data to output file
		fdout.write(header)
		fdout.write(payload)

		# Get next data, check time
		secsInDay_prev = secsInDay
		(header,payload,fileoffset) = pylibMk5B.getNextFrame(fdin)
		if (header == None):
			sys.exit('EOF while copying data')

		(day,hour,minute,second,fracSeconds,frameNr,secsInDay) = pylibMk5B.getTime(header)
		if (secsInDay != secsInDay_prev):
			ext_dur = ext_dur - 1
			timestr = pylibMk5B.getTimeString(header)
			bytes = fileoffset - fileoffset_prev
			print 'File Time: %s @ %d (0x%x) : offset delta %d bytes' % (timestr, fileoffset, fileoffset, bytes)
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
