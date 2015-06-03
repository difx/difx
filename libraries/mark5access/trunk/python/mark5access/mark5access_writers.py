"""
Writers

Currently a simple class for writing data into a new 
VDIF file. Performs data encapsulation only. Does not 
convert sample data into some new encoding.

The VDIFEncapsulator accepts a mark5access-like format 
string in a format of VDIF_<payloadlen>-<Mbps>-<nch>-<nbits> 
to fill out VDIF header fields.
"""

import math, re, struct

class VDIFEncapsulator:

	# For the VDIF header definition
	# http://vlbi.org/vdif/docs/VDIF_specification_Release_1.1.1.pdf

	def __init__(self):
		self.file = None
		self.fmt  = ''
		self.hdr  = [int(0) for x in range(8)]
		self.payloadbytes = 0
		self.framenr  = 0
		self.framesec = 0
		self.refepoch = 0
		self.fps      = 0
		self.writepos = 0      # note: position for next byte in current frame

	def open(self, filename, format='VDIF_8192-1024-1-32', complex=False, station='XX'):

		self.__init__()
		# print self.hdr

		## Parse the mark5access-like format string
		fmt = re.split('[\_|-]+', format)
		(self.payloadbytes,Rmbps,nch,nbit) = [int(x) for x in fmt[1:]]

		## Check the data rate

		self.fps = ((Rmbps*1e6/8) / self.payloadbytes)
		if not(int(self.fps) == self.fps):
			print ('*** Warning: VDIF %f Mbps output rate with %u-byte payload is a non-integer %e frames/sec!' % (Rmbps,self.payloadbytes,self.fps))
			while not(int(self.fps) == self.fps):
				self.payloadbytes -= 1
				self.fps = ((Rmbps*1e6/8) / self.payloadbytes)
			print ('*** Reduced to %u-byte payload.' % (self.payloadbytes))

		# Create format description

		if not(complex):
			self.fmt = 'VDIF_%u-%u-%u-%u' % (self.payloadbytes,Rmbps,nch,nbit)
		else:
			self.fmt = 'VDIFC_%u-%u-%u-%u' % (self.payloadbytes,Rmbps,nch,nbit)

		## Create template header

		# word 0: [Invalid(1) | Legacy(1) | Seconds from ref epoch(30)]
		self.hdr[0] = 0

		# word 1: [none(2)  | RefEp(6) | Data Frame#(24)]
		self.hdr[1] = 0

		# word 2:
		# [Version(3) | log2 Nch(5) | Framelen(24) in 8-byte units with 32-byte header included]
		self.hdr[2] = (int(math.log(nch,2.0)) << 24) + (self.payloadbytes + 32)/8

		# word 3:
		# [Complex(1) | bit/sample-1 (5) | Thread ID(10) | Station ID(16)]
		if complex:
			self.hdr[3] = 1 << 31
		self.hdr[3] += (nbit-1) << 26
		self.hdr[3] += 256*ord(station[0]) + ord(station[1])

		# words 4 to 8: extended user data
		self.hdr[4:8] = [0,0,0,0]

		## Internal counters
		self.framenr  = 0
		self.framesec = 0
		self.refepoch = 0

		## Create new file, do not write anything yet
		try:
			self.file.close()
		except:
			pass
		try:
			self.file = open(filename, 'wb')
		except:
			print ('Could not open %s\n' % (filename))
		self.writepos = 0

	def get_time(self):
		"""Returns the current time stamp and frame number"""
		return (self.refepoch, self.framesec, self.framenr)

	def get_fps(self):
		"""Returns the frames rate (frames/sec)"""
		return self.fps

	def get_format(self):
		"""Returns a mark5access-like format string"""
		return self.fmt

	def set_time(self, refep, refsec, framenr=0):
		"""Sets the time stamp in the VDIF header to an integer second."""
		self.refepoch = int(refep)
		self.framesec = int(refsec)
		self.framenr  = int(framenr)
		self.hdr[0] = self.framesec
		self.hdr[1] = (self.refepoch << 24) + self.framenr

	def inc_header(self):
		"""Increments the frame number by 1. Handles integer-second roll-overs."""
		self.framenr += 1

		if (self.framenr >= self.fps):
			self.framenr = 0
			self.framesec += 1

			# update word 0: [Invalid(1) | Legacy(1) | Seconds from ref epoch(30)]
			self.hdr[0] = self.framesec

		# update word 1: [none(2)  | RefEp(6) | Data Frame#(24)]
		self.hdr[1] = (self.refepoch << 24) + self.framenr

		# print ('New frame: %u/%u/%u' % (self.refepoch,self.framesec,self.framenr))

	def close(self):
		self.file.close()

	def write(self, data_string):
		"""Writes new data into VDIF file. Adds headers when necessary."""

		in_len = len(data_string)
		in_idx = 0

		while (in_idx < in_len):

			if (self.writepos <= 0):
				header = struct.pack('<8I', *(self.hdr))
				self.file.write(header)
				self.inc_header()

			nremain = in_len - in_idx
			nfit    = self.payloadbytes - self.writepos
			nwrite  = min(nremain, nfit)
			
			self.file.write(data_string[in_idx:(in_idx+nwrite)])

			in_idx   += nwrite
			self.writepos += nwrite

			if (self.writepos >= self.payloadbytes):
				self.writepos = 0

	def m_test(self):
		N = 2*8192*8/32
		L = 512
		if True:
			# The below should decode with 'm5d':
			# $ m5d /tmp/test.vdif VDIF_8192-1024-1-32 100 0
			data_f   = [float(x) for x in range(N)]
			data_str = struct.pack('<%uf'%(N), *data_f)
			print ('Using %d float values packed into %u-byte string\n' % (len(data_f),len(data_str)))
		else:
			data_i = [int(x) for x in range(N)]
			data_str = struct.pack('<%uI'%(N), *data_i)
			print ('Using %d int64 values packed into %u-byte string\n' % (len(data_i),len(data_str)))
		vdif = VDIFEncapsulator()
		vdif.set_time(28, 123000)
		vdif.open('/tmp/test.vdif', format='VDIF_8192-1024-1-32', complex=False, station='AA')
		for ii in range(L):
			vdif.write(data_str)
		vdif.close()


