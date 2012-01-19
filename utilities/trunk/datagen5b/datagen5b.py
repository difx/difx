#!/usr/bin/env python
#
# Mark5B GPS Recording Generator Version 1.0
# (C) 2011 Jan Wagner
#
# Python script to generate a GPS satellite signal, either modulated or unmodulated.
# This signal is written first to 'model.bin'.
#
# The signal is then copied to several VLBI "station" recordings with added noise.
# The recordings are in Mark5B format, 512 Mbps, 8-channel (all identical) 2-bit 32 MS/s.
#
# The GPS satellite is placed at the celestial north pole. Station data are
# a direct copy of the signal, with a possible integer sample fixed time delay,
# but no fractional sample delay, and no time-varying phase.
#

import sys
import string
import os
import struct
import math
import random
import ctypes
import array

# GPS C/A Gold code generator: http://www.phys-x.org/grblog/2005/08/gps-ca-code-generator.html
from CA_code_gen import CA_code_gen  

MODEL_FILE = 'model.bin'
FS = 32e6
BW = FS/2
F_GPS = 6e6
A_GPS = 1		# GPS carrier amplitude: should be set to 0 for RFI-free model
A_SOURCE = 1		# Astronomical source amplitude stddev
A_ANTNOISE = 1		# Station antenna noise stddev

GPS_MODEL_FULL=0
GPS_MODEL_TONE=1
used_model = GPS_MODEL_TONE

M5B_CHANNELS = 8
M5B_MAX_CHANNELS = 16
M5B_BITS_PER_SAMPLE = 2
M5B_PAYLOAD_BYTES = 10000
M5B_PAYLOAD_WORDS = M5B_PAYLOAD_BYTES/4
M5B_RATE_MBPS = int(M5B_BITS_PER_SAMPLE * M5B_CHANNELS * FS / 1e6)
M5B_RATE_FRAMES_PER_SEC = (FS * M5B_BITS_PER_SAMPLE / (8 * M5B_PAYLOAD_BYTES)) * M5B_CHANNELS

''' 
generateModelFile: Creates binary output file with float32 time series
The data contains GPS C/A and Y code signals and an astronomical source
at the same sky position as the GPS satellite.
Args: model : 0=synthetic GPS at GPS carrier freq, 1=pure tone at GPS carrier freq
'''
def generateModelFile(f, fname, Tdur, model):
	Ns = int(FS * Tdur)
	# GPS C/A coarse acq code: fcarr x 1.023 MHz modulation, 1023 number-PRNG chiplet, 1ms repeat
	#   conveys no data (chip)
	#   37 suitable maximally orthogonal chiplets (GOLD-codes)
	#   C/A used on L-band L1 only
	# GPS P(Y) precise code: fcarr x 10.23 MHz modulation, 7-day repeat
	#   transmits data (code)
	#   P code was uncencrypted, but not used for commercial positioning
	#   since 1996 now anti-spoof encrypted (AS) and set as Y code
	#   decoding keys not available commercially
	#   P used on L-band L1 and L2

	# BPSK modulation, frequencies
	F_CA = 1e6 #1.023e6
	F_Y = 10.23e6
	# CA code table, 1023 values
	satID = 1
	CAchiplet = CA_code_gen(satID)
	# P(Y) code, no table
	Ycode = 0

	print 'Writing GPS signal to model file %s with %u values for %f seconds' % (fname,Ns,Tdur)
	if model==GPS_MODEL_FULL:
		print 'Using full GPS model with modulation'
	else:
		print 'Using pure tone at GPS carrier only, no modulation'

	Ns_1percent = int(0.01*Ns)
	Ns_1sec = int(FS)
	Ns_tick_min = min(Ns_1percent, Ns_1sec)
	CAchip_idx = 0
	CAchip_cntr = 0
	CAchip_rollover = round(FS/F_CA)
	Ycode_idx = 0
	Ycode_cntr = 0
	Ycode_rollover = round(FS/F_Y)

	for n in xrange(Ns):

		# synthetic GPS signal
		if model==GPS_MODEL_FULL:
			# current CA chip
			if True:
				# slow, more accurate
				CAchip = CAchiplet[int(round(n*F_CA/FS)) % len(CAchiplet)]
			else:
				# faster, inaccurate
				if (CAchip_cntr>=CAchip_rollover):
					CAchip_idx = (CAchip_idx + 1) % len(CAchiplet)
					CAchip_cntr = 0
				else:
					CAchip_cntr = CAchip_cntr + 1
				CAchip = CAchiplet[CAchip_idx]
	
			# current Y code value
			if True:
				# slow, accurate
				Ycode_newidx = int(round(n*F_Y/FS))
				if Ycode_newidx!=Ycode_idx:
					Ycode = random.randint(0,1)
					Ycode_newidx = Ycode_idx
			else:
				# faster, inaccurate
				if (Ycode_cntr>=Ycode_rollover):
					Ycode = random.randint(0,1)
					Ycode_cntr = 0
				else:
					Ycode_cntr = Ycode_cntr + 1
			#Ycode = 0 # to disable P(Y) code

			# combine via Modulo 2 Sum
			code = (CAchip+Ycode) % 2
			# BPSK modulate carrier
			mod = 2*code - 1  # 0,1 to +1,-1 (+-180deg)
			sig = A_GPS * (mod*math.cos(2 * math.pi * float(n)*F_GPS/float(FS)))

		# pure unmodulated tone at GPS carrier freq
		else:
			sig = A_GPS * math.cos(2 * math.pi * float(n)*F_GPS/float(FS))

		# add astronomical source correlated noise
		sig = sig + random.gauss(mu=0, sigma=A_SOURCE)

		# output to file
		sigpack = struct.pack('f', float(sig))
		f.write(sigpack)
		if ((n % Ns_tick_min)==0):
			print 'T=%u ms' % int(1e3*n/FS)
	print 'Done!'

''' 
Write one Mark5B frame with header and payload.
Returns next header
'''
def writeMark5BFrame(fd=None, payload='', headerdata={}):
	framenr = headerdata['frame']
	frameday = headerdata['day']
	framesec = headerdata['sec']

	# convert to BCD for header, via cast to hex
	ddd = int(str(frameday), 16) * (2**20)
	sssss = int(str(framesec), 16)

	# convert to "string" since python write() knows only strings
	m5b_hdr_format32 = 'llLl'  # 32-bit OS
	m5b_hdr_format64 = 'IiIi'  # 64-bit OS
	w0 = int('0xABADDEED',base=0)	# bit31-0:magic
	w1 = int(framenr & 0x7FFF)	# bit31-17:user, bit16:TVG, bit15-0:framenr
	w2 = ddd + sssss # 'DDDSSSSS' time in BCD
	w3 = 0 # '.SSSScc' sub-second time and CRC16 in cc

	header = struct.pack(m5b_hdr_format64, w0, w1, w2, w3)
	fd.write(header)
	fd.write(payload)

	framenr = framenr + 1
	if (framenr >= M5B_RATE_FRAMES_PER_SEC):
		framenr = 0
		framesec = framesec + 1
		if (framesec>=24*60*60):
			framesec = 0
			frameday = frameday + 1
			if (frameday>=999):
				frameday = 0
	headerdata['frame'] = framenr
	headerdata['sec'] = framesec
	headerdata['day'] = frameday
	return headerdata

''' 
Create several Mark5B-formatted output files derived from a single
input file but different time offsets.
'''
def main(argv):
	TDAY_DEFAULT = 556
	TSEC_DEFAULT = 12*60*60
	TDUR_DEFAULT = 1.0
	if len(sys.argv)<2:
		print " "
		print " Usage: %s <Nstations> [<sec_duration> <startday> <startsec>]" % (sys.argv[0])
		print " "
		print " Generates several (Nstations) Mark5B-format output files with"
		print " specified or default start time stamps, for certain data duration in"
		print " seconds. The data is %u-channel 16 MHz 2-bit, 32 MS/s, %u Mbps." % (M5B_CHANNELS,M5B_RATE_MBPS)
		print " "
		print "   Nstations    : number of stations"
		print "   sec_duration : duration of data to generate, defaults to %f\n" % (TDUR_DEFAULT)
		print "   startday     : three last digits of MJD data start day, defaults to %u" % (TDAY_DEFAULT)
		print "   startsec     : data start second, defaults to %u" %(TSEC_DEFAULT)
		print " Model:"
		print "   GPS amplitude A=%e" % (A_GPS)
		print "   source gaussian noise stddev=%e" % (A_SOURCE)
		print "   antenna gaussian noise stddev=%e\n" % (A_ANTNOISE)
		sys.exit()

	# defaults
	Tday = TDAY_DEFAULT
	Tsec = TSEC_DEFAULT
	Tdur = TDUR_DEFAULT

	# user args
	Nstations = int(sys.argv[1])
	if len(sys.argv)>2:
		Tdur = float(sys.argv[2])
	if len(sys.argv)>3:
		Tday = float(sys.argv[3])
	if len(sys.argv)>4:
		Tsec = float(sys.argv[4])

	Ns = int(FS * Tdur)

	# prepare common GPS signal data file (float, real-valued)
	try:
		fin = open(MODEL_FILE, 'rb')
		print 'Using the existing model file %s' % (MODEL_FILE)
	except:
		fin = open(MODEL_FILE, 'wb')
		generateModelFile(fin, MODEL_FILE, Tdur, used_model)
		fin.close() # to flush
		fin = open(MODEL_FILE, 'rb')

	# prepare threshold used in two-bit quantization
	#   sampler agc, should keep stats 0.17/0.33/0.33/0.17 
	#   single-sided 0.33 (33% in CDF) is very roughly 1*sigma
	#   unpacked quantized data, voltage ratio high/low should be about 3.336
	#   unpacked decodes to -3.3359, -1, 1, 3.3359
	#
	# => quantize: sign(+/-), magnitude(lte,gt) from x<=std(X)
	#
	#    where std(X) = std(GPS + Nsys + Nant)
	#      1) std(GPS+Nsys) = sqrt(var(GPS)+var(Nsys)+covar(GPS,Nsys)) = sqrt(std(GPS)^2 + std(Nsys)^2 + 0)
	#      2) std(uniform_dist[a,b]) = sqrt((1/3)*(a^2 + ab + b^2))
	#         => std(uniform_dist[-a,a]) = sqrt((1/3)*a^2)
	#      3) std(GPS;code=A*(+-1)) = abs(A_GPS)*sqrt(1/3)
	if used_model==GPS_MODEL_FULL:
		std_gps = math.sqrt((1/3.0))*A_GPS # stddev of uniform dist
	else:
		std_gps = A_GPS/math.sqrt(2.0) # stddev of sine
	std_source = A_SOURCE     # gaussian
	std_antnoise = A_ANTNOISE # gaussian
	std_thresh = math.sqrt(std_gps*std_gps + std_source*std_source + std_antnoise*std_antnoise)

	# note: Matlab on sine+noise+noise data with all A=1 finds std(data)=1.2251
	#       and a slightly lopsided histogram
	#       Whereas a priori std'(data)=sqrt(1/2 + 1 + 1)=1.5811
	#       and would expect fully Gaussian histogram
	#	=> maybe random.gauss(mu=0,sigma=A) is not entirely gaussian!?

	# derive station data: add independent noise, quantize, frame
	stations=[]
	for snum in xrange(Nstations):
		fname = 'station%02d.m5b' % (snum)
		fid = open(fname, 'w')
		Noffset = 0  #2*snum  # increasing time offset of 2 samples (62.5ns) per file
		stationdata = (fname, fid, Noffset, snum)
		stations.append(stationdata)

	for st in stations:
		fout = st[1]
		Noff = st[2]
		Istation = st[3]
		header={'sec':Tsec, 'day':Tday, 'frame':0}

		# process input file one byte at a time,
		# appending to temporary Mark5B frame payload
		# that is dumped out to the framer when it is full
		fin.seek(Noff, 0)
		din = fin.read(struct.calcsize('f'))
		m5b_payload = ''
		m5b_bitstr = ''
		Nsamples = 0
		while ((din!='') and (Nsamples<Ns)):

			# common data plus station-specific noise
			sample = struct.unpack('f',din)
			if (len(sample)!=1):
				print "Oops! len(unpack(\'f\',sample))!=1"
				din = fin.read(struct.calcsize('f'))
				continue
			sample = sample[0]

			sample = sample + random.gauss(mu=0,sigma=A_ANTNOISE)
			Nsamples = Nsamples + 1

			# two-bit quantization: Table 3 in http://www.haystack.mit.edu/tech/vlbi/mark5/docs/230.3.pdf
			#   Mark5B: const float lut4level[4] = {-HiMag, 1.0, -1.0, HiMag}; HiMag=3.3359
			#   'ms'=00 : -HiMag
			#   'ms'=01 : +1.0
			#   'ms'=10 : -1.0
			#   'ms'=11 : +HiMag
                        #   sign bits:      0, 2, 4, 6
			#   magnitude bits: 1, 3, 5, 7
			#   value for sign bit is clear (s=1:positive, s=0:negative)
			#   value for mag bit is complicated
			# reverting to stupid if/else
			if (sample>=0):
				if (sample>std_thresh):
					ms = '11'
				else:
					ms = '01'
			else:
				if (abs(sample)<std_thresh):
					ms = '00'
				else:
					ms = '10'
			# print ' %f||%f => %s ' % (sample, std_thresh, ms)

			# copy into all channels, append, store when 32-bit complete
			m5b_chanstr = M5B_CHANNELS * ms
			m5b_bitstr  = m5b_bitstr + m5b_chanstr
			if (len(m5b_bitstr) >= M5B_MAX_CHANNELS*M5B_BITS_PER_SAMPLE):
				bits2int = int(m5b_bitstr, base=2)
				m5b_payload = m5b_payload + struct.pack('<I', bits2int)
				#print 'Bitstr=%s : val=%x' % (m5b_bitstr,bits2int)
				m5b_bitstr = ''

			# write out when data frame is full
			if (len(m5b_payload)>=M5B_PAYLOAD_BYTES):
				print 'Write station %u after %u samp (%.2f s), len=%u' % (Istation,Nsamples,Nsamples/FS,len(m5b_payload))
				header = writeMark5BFrame(fd=fout, payload=m5b_payload, headerdata=header)
				m5b_payload = ''

			# next
			din = fin.read(struct.calcsize('f'))

		# next station
		fout.close()

	# all stations done
	fin.close()


if __name__ == "__main__":
    main(sys.argv)
