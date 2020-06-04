#!/usr/bin/python
# may also work in PyPy
#
# Reads VDIF file and checks for missing frames in each thread,
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
	print ('vdifcontinuitycheck.py [-v] [-b|--bigendian] [-p|--progress] [--framesize=<n bytes>] <filename>')
	print ('  --v           : prints every frame')
	print ('  --bigendian   : specify when data are VDIF-like but with wrong Endianness')
	print ('  --progress    : show a progress bar per thread')
	print ('  --framesize=n : override the frame size otherwise taken from VDIF headers')
	sys.exit(-1)

fmt_littleendian = '<IIII'
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

header_len = struct.calcsize(fmt)
#filename = 'fila10g.vdif'
#filename = '/mnt/disks/cap2014y289d02h29m.vdif.1312'
filename = args[0]

def vdif_decode_header(data,fmt):
	s = struct.unpack(fmt,data)
	h = {}
	# http://vlbi.org/vdif/docs/VDIF_specification_Release_1.1.1.pdf
	h['I']      = (s[0] & 0x80000000) > 0
	h['L']      = (s[0] & 0x40000000) > 0 
	h['sec']    =  s[0] & 0x3FFFFFFF
	h['ep']     = (s[1] >> 24) & 0x3F
	h['frame']  =  s[1] & 0x00FFFFFF
	h['size']   = (s[2] & 0x00FFFFFF) * 8
	h['nch']    = 2**((s[2] >> 24) & 0x3F)
	h['st']     = '%c%c' % ((s[3]>>8)&0xFF,s[3]&0xFF)
	h['thread'] = (s[3] >> 16) & 0x03FF
	h['bps']    = ((s[3] >> 26) & 0x1F) + 1
	h['C']      = (s[3] & 0x80000000) > 0
	h['fillpattern'] = (s[0] == 0x11223344) or (s[1] == 0x11223344) or (s[2] == 0x11223344)
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

th_sec = {}
th_frames = {}
th_invalid_frames = {}
th_byteoffsets = {}
peak_threadids = []
peak_fps = 1
syncreport_prev_sec = 0
nfill = 0
skipsize = 4  # initially, length of fill 0x11223344

done = False
while not(done):

	streampos = f.tell()
	header = f.read(header_len)
	if not header:
		done = True
		print ('EOF')
		break
	else:
		h = vdif_decode_header(header,fmt)
		if verbose:
			print ('%s %d \r' % (str(h),f.tell())),

		if h['fillpattern']:
			if skipsize > 0:
				tmp = f.read(skipsize)
			nfill += 1
			continue

		if h['size']==0 or h['size']>16*1024*1024:
			print ('Stopping at file offset %d: frame size %d looks bad.' % (f.tell()-header_len,h['size']))
			sys.stdout.write("\033[K")
			hlittle = vdif_decode_header(header,fmt_littleendian)
			hbig    = vdif_decode_header(header,fmt_bigendian)
			if (fmt_is_little):
				print ('Maybe try again with --bigendian? '),
				print ('Then the current header decodes to:\n%s\n' % (str(hbig)))
			else:
				print ('Maybe try again without --bigendian? '),
				print ('Then the current header decodes to:\n%s\n' % (str(hlittle)))
			break

		skipsize = h['size'] - header_len
		if (user_framesize > 0):
			skipsize = user_framesize - header_len
		#f.seek(skipsize, 1)  # fails at 2GB boundary!?
		tmp = f.read(skipsize)


	# First time to see a thread?
	tID = h['thread']
	if not tID in th_sec:
		th_sec[tID] = h['sec']
		th_frames[tID] = []
		th_invalid_frames[tID] = []
		th_byteoffsets[tID] = streampos

	# Skip invalid frames (sometimes timestamp is not correct... in Mark6 rec software...)
	if h['I']:
		th_invalid_frames[tID].append(h['frame'])
		continue

	# Frames per sec
	if h['frame'] >= peak_fps:
		peak_fps = h['frame'] + 1

	# Summaries at each integer-second change or at EOF:
	if (th_sec[tID] != h['sec']) or done:
		if len(th_frames[tID]) <= 0:
			# no valid frames at all
			nrange = 0
			nrmissing = -1
			nrdup = 0
			nrOO = -1
		else:
			nrange = max(th_frames[tID]) - min(th_frames[tID]) + 1
			uniques = (list(set(th_frames[tID])))
			nrmissing = peak_fps - len(uniques)
			nrdup = len(th_frames[tID]) - len(uniques)
			nrOO = 0
			fn_prev = th_frames[tID][0]
			fn_peak = th_frames[tID][0]
			for fn in th_frames[tID][1:]:
				# Catch non-incrementing frames; misses shifts eg '10 11 12 13 0 1 2 3 4' is just one out of order
				if fn <= fn_prev:
					nrOO = nrOO + 1
				fn_prev = fn
				# Catch shifts eg '10 11 12 13 0 1 2 3 4'  count '0 1 2 3 4' as all out of order
				if fn < fn_peak:
					nrOO = nrOO + 1
		# Multi-thread reporting
		if (len(th_sec.values()) > 1):
			# All threads at same second? Give a frame-shift report
			firsttID = min(th_byteoffsets.keys())
			if (th_sec.values().count(th_sec[firsttID]) == len(th_sec.values())):
				firstoffset = th_byteoffsets[firsttID]
				offsets_report = 'Threads alignment       :  '
				for ttid in th_byteoffsets:
					# if ttid == firsttID: continue
					offsets_report += ('%d[%+d] ' % (ttid, (th_byteoffsets[ttid]-firstoffset)/h['size']))
				offsets_report += 'frames'
				print (offsets_report)
			# Any time offset >1 second? Give a sync report
			tleading = max(th_sec.values())
			if (tleading != syncreport_prev_sec) and (max([tleading-t for t in th_sec.values()]) >= 2):
				syncreport_prev_sec = tleading
				sync_report = 'Threads sync fail       :  '
				for ttid in th_sec:
					dT = tleading - th_sec[ttid]
					dframes = (streampos - th_byteoffsets[ttid])/h['size']
					if dT >= 2:
						sync_report += '%d[dT=%ds,dframes=%d] ' % (ttid,dT,dframes)
				print (sync_report)

		# Current thread reporting
		if verbose:
			sys.stdout.write("\033[K")
		print ('Thread %-1d Second %-6d : %6d frames : #%d--#%d : %d lost, %d out-of-order, %d invalid, %d dup, of %d total ' % 
			( tID, th_sec[tID], len(th_frames[tID]), min(th_frames[tID]),
			  max(th_frames[tID]), nrmissing, nrOO, len(th_invalid_frames[tID]), nrdup, nrange
			) 
		)
		if nfill > 0:
			print ('Garbage data : %d fill pattern frames' % (nfill))


		# Restart frame storage for this thread
		th_sec[tID] = h['sec']
		th_frames[tID] = []
		th_invalid_frames[tID] = []
		th_byteoffsets[tID] = streampos
		nfill = 0

	th_frames[tID].append(h['frame'])

	# Visualize the frame count in current thread?
	if progressbar:
		N = len(th_frames[tID])
		Nmax = 80
		if N < Nmax:
			print (tID, str(th_frames[tID][0]) + '.'*(len(th_frames[tID])-1) )
		else:
			print (tID, str(th_frames[tID][0]) + '.'*(Nmax-2) + '<' + str(N-Nmax+1) + ' more>')
