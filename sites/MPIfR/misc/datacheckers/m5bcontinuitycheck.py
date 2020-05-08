#!/usr/bin/python
# may also work in PyPy
#
# Reads Mark5B file and checks for missing frames in each thread,
# out-of-order frames, and Invalid-flagged frames.
#
# From https://bitbucket.org/jwagner313/vdifstream/
#
# (C) 2014 Jan Wagner
#

import sys
import struct
import collections
import getopt

def usage():
	print ('m5bcontinuitycheck.py [-v] [-b|--bigendian] [-p|--progress] [--framesize=<n bytes>] <filename>')
	print ('  --v           : prints every frame')
	print ('  --bigendian   : specify when data are VDIF-like but with wrong Endianness')
	print ('  --progress    : show a progress bar per thread')
	print ('  --framesize=n : override the frame size otherwise taken from VDIF headers')
	sys.exit(-1)

fmt_littleendian = '<IIII'  # 4 words, 32 bits
fmt_bigendian = '>IIII'

verbose = False		# default: don't print every frame
progressbar = False     # default: don't show a per-thread progress bar
fmt = fmt_littleendian  # default: VDIF is little endian, "other" formats are big endian
fmt_is_little = True
user_framesize = 0	# default: use frame size in the headers, do not override

try:
	optlist, args = getopt.getopt(sys.argv[1:], "vbps:", ["verbose","bigendian","framesize="])
except getopt.GetoptError:
	usage()

for opt, optarg in optlist:
	if opt == '-b' or opt == '--bigendian':
		fmt = fmt_bigendian
		fmt_is_little = False
	elif opt == '-s' or opt == '--framesize':
		user_framesize = int(optarg)
	elif opt == '-p' or opt == '--progress':
		progressbar = True
	elif opt == '-v' or opt == '--verbose':
		verbose = True
	else:
		usage()

if (len(args) != 1):
	usage()

frame_len = 10016
header_len = struct.calcsize(fmt)
filename = args[0]

def m5b_decode_header(data,fmt):
	s = struct.unpack(fmt,data)
	h = {}
	# http://vlbi.org/vdif/docs/VDIF_specification_Release_1.1.1.pdf
	#h['I']      = (s[0] & 0x80000000) > 0
	#h['L']      = (s[0] & 0x40000000) > 0 
	#h['sec']    =  s[0] & 0x3FFFFFFF
	#h['ep']     = (s[1] >> 24) & 0x3F
	#h['frame']  =  s[1] & 0x00FFFFFF
	#h['size']   = (s[2] & 0x00FFFFFF) * 8
	#h['nch']    = 2**((s[2] >> 24) & 0x3F)
	#h['st']     = '%c%c' % ((s[3]>>8)&0xFF,s[3]&0xFF)
	#h['thread'] = (s[3] >> 16) & 0x03FF
	#h['bps']    = ((s[3] >> 26) & 0x1F) + 1
	#h['C']      = (s[3] & 0x80000000) > 0

	# Mark5B
	h['sync'] = (s[0] == 0xABADDEED)
	h['fillpattern'] = (s[0] == 0x11223344) or (s[1] == 0x11223344) or (s[2] == 0x11223344)
	h['size'] = 10016
	if h['sync']:
		dayStr = str(hex(s[2]))
		try:
			h['sec'] = int(dayStr[2:5])*86400 + int(dayStr[5:10])
			h['frame'] = s[1] & 0x7FFF  # bit15:TVG_ENA, bit[14:0]:frameNr
		except:
			h['sync'] = False
	else:
		h['sec'] = -1
		h['frame'] = 0
	return h

def issorted(l):
	return all(l[i] <= l[i+1] for i in xrange(len(l)-1))

def unique(l):
	u = []
	for x in l:
		if not(x in u):
			u.append(x)
	return u

f = open(filename, "rb")

th_sec = -1
th_frames = []
th_invalid_frames = []
th_byteoffsets = []
peak_fps = 1
nfill = 0
ngarbagebytes = 0
skipsize = 4  # initially, length of fill 0x11223344

done = False
while not(done):

	streampos = f.tell()
	header = f.read(header_len)
	if not header:
		done = True
		print ('EOF')
		break

	h = m5b_decode_header(header,fmt)
	if verbose:
		print ('%s %d \r' % (str(h),f.tell())),

	if h['fillpattern']:
		if skipsize > 0:
			tmp = f.read(skipsize)
		nfill += 1
		continue

	if not h['sync']:
		olost = f.tell()
		while not h['sync']:
			header = f.read(header_len)
			h = m5b_decode_header(header,fmt)
			if not header:
				done = True
				print ('EOF')
				break
		if done:
			break
		ofound = f.tell()
		ngarbagebytes += (ofound-olost)
		# print('Regained sync after %d bytes' % (ofound-olost))

	skipsize = h['size'] - header_len
	if (user_framesize > 0):
		skipsize = user_framesize - header_len
	#f.seek(skipsize, 1)  # fails at 2GB boundary!?
	tmp = f.read(skipsize)

	# Skip invalid frames (sometimes timestamp is not correct... in Mark6 rec software...)
	if not h['sync']:
		th_invalid_frames.append(h['frame'])
		continue

	# Frames per sec
	if h['frame'] >= peak_fps:
		peak_fps = h['frame'] + 1

	# Summaries at each integer-second change or at EOF:
	if th_sec == -1:
		th_sec = h['sec']
	if (th_sec != h['sec']) or done:
		if len(th_frames) <= 0:
			# no valid frames at all
			nrange = 0
			nrmissing = -1
			nrdup = 0
			nrOO = -1
		else:
			nrange = max(th_frames) - min(th_frames) + 1
			uniques = (list(set(th_frames)))
			nrmissing = peak_fps - len(uniques)
			nrdup = len(th_frames) - len(uniques)
			nrOO = 0
			fn_prev = th_frames[0]
			fn_peak = th_frames[0]
			for fn in th_frames[1:]:
				# Catch non-incrementing frames; misses shifts eg '10 11 12 13 0 1 2 3 4' is just one out of order
				if fn <= fn_prev:
					nrOO = nrOO + 1
				fn_prev = fn
				# Catch shifts eg '10 11 12 13 0 1 2 3 4'  count '0 1 2 3 4' as all out of order
				if fn < fn_peak:
					nrOO = nrOO + 1

		# Current thread reporting
		if verbose:
			sys.stdout.write("\033[K")
		print ('Second %-6d : %6d frames : #%d--#%d : %d lost, %d out-of-order, %d invalid, %d dup, of %d total ' % 
			( th_sec, len(th_frames), min(th_frames),
			  max(th_frames), nrmissing, nrOO, len(th_invalid_frames), nrdup, nrange
			) 
		)
		if nfill > 0 or ngarbagebytes > 0:
			print ('Garbage data : %d fill pattern frames, %d garbage bytes between frames' % (nfill,ngarbagebytes))

		# Restart frame storage for this thread
		th_sec = h['sec']
		th_frames = []
		th_invalid_frames = []
		th_byteoffsets = streampos
		nfill = 0
		ngarbagebytes = 0

	th_frames.append(h['frame'])

	# Visualize the frame count in current thread?
	if progressbar:
		N = len(th_frames)
		Nmax = 80
		if N < Nmax:
			print (tID, str(th_frames[0]) + '.'*(len(th_frames)-1) )
		else:
			print (tID, str(th_frames[0]) + '.'*(Nmax-2) + '<' + str(N-Nmax+1) + ' more>')
