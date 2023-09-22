#!/usr/bin/env python
#
# Mark5B Data Frame Access 
# (C) 2011 Jan Wagner
#
#  getTime()       : decodes header time stamp
#  getTimeString() :  -"- and returns it as a string
#  getNextFrame()  : returns next frame, re-syncing if sync is lost
#

import sys, string, os, struct

# Header is composed of 4 words of 32 bits:
mk5bheaderFormat = 'llLl'	# 32bit OS
mk5bheaderFormat = 'IiIi'	# 64bit OS

# Args:    header as a byte string from getNextHeader()
# Returns: header timestamp (day,hour,minute,second,fracSeconds,frameNr,secsInDay)
def getTime(header):
	(syncWord, frameCode, bcdDay, bcdDay2) = struct.unpack(mk5bheaderFormat, header)
	frameNr = str(frameCode & (2**15-1))
	dayStr = '0x%08X' % int(bcdDay)
	day = dayStr[2:5]
	secsInDay = int(dayStr[5:10])
	hour = int(secsInDay/3600)
	minute = int( (secsInDay - 3600*hour) / 60)
	second = int( (secsInDay - 3600*hour - 60*minute) )
	strbcdDay2 = str(hex(bcdDay2))
	fracSecondsStr = strbcdDay2[2:-4]
	if fracSecondsStr == '':
		fracSeconds = 0
	else:
		fracSeconds = int(fracSecondsStr)
	return (day,hour,minute,second,fracSeconds,frameNr,secsInDay)

# Args: header as byte string, new mjd and new second-of-day
def setTime(header, mjd, second):
	(syncWord, frameCode, bcdDay, bcdDay2) = struct.unpack(mk5bheaderFormat, header)
	mjd = int(mjd) % 1000
	timestr = '%03u%05u' % (mjd,int(second))
	bcdDay = int(timestr, base=16)
	header = struct.pack(mk5bheaderFormat, syncWord, frameCode, bcdDay, bcdDay2)
	return header

# Args:    header as a byte string from getNextHeader()
# Returns: string with header timestamp
def getTimeString(header):
	day,hour,minute,second,fracSeconds,frameNr,secsInDay = getTime(header)
	out = "%s.%02d:%02d:%02d.%04d/%s (%ds)" % ( day, hour, minute, second, fracSeconds, frameNr, secsInDay)
	return out

# Returns: (header,payload,fileoffset)
def getNextFrame(fd):

	# Header and payload sizes
	frameSize = 2500 * 4
	headerSize = 4 * 4
	header = None
	payload = None


	# Look for magic sync word
	sync_lost = False
	sync_lost_prev = False
	while True:
		fileoffset = fd.tell()	

		# Read header
		header = fd.read(headerSize)
		if header == '':
			print 'Hit EOF'
			return (None,None,0)

		# Decode
		(syncWord, word2, bcdDay, bcdDay2) = struct.unpack(mk5bheaderFormat, header)
		sync_lost = not(syncWord==2880298733) # 0xABADDEED

		if (sync_lost and not sync_lost_prev):
			print 'Lost sync at offset %u with word 0x%08X, seeking for sync...' % (fileoffset,syncWord)
		sync_lost_prev = sync_lost

		if (sync_lost):
			# Continue seeking from four bytes later
			fileoffset = fileoffset + 4
			fd.seek(fileoffset, os.SEEK_SET)
			continue
		else:
			# Got a full frame
			if (sync_lost_prev):
				print 'Sync regained at offset %u' % (fileoffset)
			break
	
	# Got a header, now return header and payload
	payload = fd.read(frameSize)
	return (header,payload,fileoffset)


_current_payload = None
_unused_samples = 0
_current_payloadbyte = 0
_lut4level = [-3.3359, 1.0, -1.0, 3.3359]
_prev_read_offset = 0

# Returns: {data[],fileoffset}
def decodeData(fd, num_samples=1024, channel=15):
	global _current_payload
	global _current_payload_byte
	global _unused_samples
	
	offset = 1
	framecnt = 1
	num_channels = 16
	bits_per_sample = 2
	byte_per_sample = 4
	byte_offset = (bits_per_sample * (channel % num_channels)) / 8
	# byte_shift = 8 - bits_per_sample * (channel % 4) - 2  # when channel=4, plots channel 7  !?
	byte_shift = bits_per_sample * (channel % 4)
	m5kb_payload_bytes = 10000
	max_samples = ((m5kb_payload_bytes * 8) / bits_per_sample) / num_channels

	if (_current_payload==None or _unused_samples<=0):
		(header,_current_payload, offset) = getNextFrame(fd)
		# print 'New frame %u' % (framecnt)
		_unused_samples = max_samples
		_current_payload_byte = 0
		_prev_read_offset = offset
	else:
		_prev_read_offset = fd.tell()

	samples = []
	while (num_samples > 0):
		#print '%u  %u' % (_current_payload_byte + byte_offset,  _unused_samples)
		packed = _current_payload[_current_payload_byte + byte_offset]
		packed = struct.unpack('b', packed)[0]
		packed = (packed >> byte_shift) & 0x03
		samples.append(_lut4level[packed])

		_current_payload_byte = _current_payload_byte + byte_per_sample
		_unused_samples = _unused_samples - 1
		num_samples = num_samples - 1

		if (_unused_samples <= 0):
			(header,_current_payload, offset) = getNextFrame(fd)
			# print 'New frame %u' % (framecnt)
			# framecnt = framecnt + 1
			_unused_samples = max_samples
			_current_payload_byte = 0

	return (samples,_prev_read_offset)
