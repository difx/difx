#!/usr/bin/python3
'''
NOEMA VEX helper tool

Script to manufacture VEX entries for use with NOEMA data.
Sky frequencies of filterbank channels are derived from 1st LO
and 2nd LO information. Channels are always 64 MHz wide.

The Mark6 recorder to PolyFix units and channels assignment
of EHT 2021 is assumed, and follows R. Garcias spreadsheet
202104_pNOEMA_VLBI_backend_5-9GHz_freq_setup.xlsx, and the
cabling of 10G/Mark6 in IRAM's pNOEMA_2021_EHT_Backend_HW_Setup.pdf
'''

import argparse
import sys

__author__ = "Jan Wagner (MPIfR)"
__version__ = "1.0.2"

NOEMA_PFB_BANDWIDTH_MHZ = 64.0


def parse_args(args: []):

	cmd='%(prog)s <options>'

	parser = argparse.ArgumentParser(description=__doc__, add_help=True, formatter_class=argparse.RawDescriptionHelpFormatter)
	parser.add_argument('--version', action='version', version='%(prog)s ' + __version__)
	parser.add_argument('--all-basebands', dest='do_full_polyfix_overview', action='store_true', help='Do not output VLBI recorder specific channels but rather the entire 2 x 128 ch x 64 MHz set of PolyFix channels')
	parser.add_argument('-f', '--lo1', dest='lo1', metavar='GHz', default='221.100', help='frequency of 1st LO (GMVA 92.101, EHT 221.100; default: %(default)s)')
	parser.add_argument('-F', '--lo2', dest='lo2', metavar='GHz', default='7.744', help='frequency of 2nd LO (default: %(default)s)')
	parser.add_argument('-r', dest='recorders', default='1,2,3,4', help='list of recorder ID numbers (default: %(default)s), for GMVA2021 use 5')
	parser.add_argument('-c', dest='cabling', default='5-9', help='link cabling config (default "%(default)s"; use "4-8" or "5-9")')
	parser.add_argument('--if', '-i', dest='do_vex_if', action='store_true', help='also output VEX $IF section')
	parser.add_argument('--bbc', '-b', dest='do_vex_bbc', action='store_true', help='also output VEX $BBC section')
	# todo? : parser.add_argument('--v2d', '-v', dest='do_v2d', action='store_true', help='also output v2d ANTENNA, DATASTREAM sections')

	return parser.parse_args(args)


class BandLabel:
	'''Helper class to assign labels to frequency ranges'''

	def __init__(self, name='b1', fstart_GHz=212.053, fstop_GHz=214.100):
		self.name = name
		if fstart_GHz > fstop_GHz:
			fstart_GHz,fstop_GHz = fstop_GHz,fstart_GHz
		self.fstart_GHz = fstart_GHz
		self.fstop_GHz = fstop_GHz

	def isContained(self, freq_GHz):
		return freq_GHz >= self.fstart_GHz and freq_GHz <= self.fstop_GHz

	def getName(self):
		return self.name


class EHTBandLabels:
	'''Definitions of EHT frequency "bands"'''

	def __init__(self):
		self.bands = [
			BandLabel('b1',212.052,214.100),
			BandLabel('b2',214.100,216.148),
			BandLabel('b3',226.052,228.100),
			BandLabel('b4',228.100,230.148),
			BandLabel('b1_0.8mm',334.552,336.600),
			BandLabel('b2_0.8mm',336.600,338.648),
			BandLabel('b3_0.8mm',346.552,348.600),
			BandLabel('b4_0.8mm',348.600,350.648)
		]

	def lookUp(self, freq_GHz, sideband='U'):
		labels = []
		if 'U' in sideband.upper():
			f0,f1 = freq_GHz, freq_GHz + NOEMA_PFB_BANDWIDTH_MHZ*1e-3
		else:
			f0,f1 = freq_GHz - NOEMA_PFB_BANDWIDTH_MHZ*1e-3, freq_GHz
		for band in self.bands:
			if band.isContained(f0) or band.isContained(f1):
				labels.append(band.getName())
		full_labels = ','.join(labels)
		return full_labels


class NoemaVexFreqGenerator:
	'''
	VEX frequency block generator
	Follows 202104_pNOEMA_VLBI_backend_5-9GHz_freq_setup.xlsx
	'''

	def __init__(self, bandlabels=EHTBandLabels()):
		self.lo1_GHz, self.lo2_GHz = 0, 0
		self.recorders = []
		self.bw_MHz = NOEMA_PFB_BANDWIDTH_MHZ
		self.pol2bbcnr = {'L':1, 'R':2, 'H':1, 'V':2}
		self.indent = '   '
		self.bandlabels = bandlabels
		self.nvexchannels = 0

	def __generate_block(self, rxUsb=True, outer=True, subbands=range(16)):
		'''
		fxUsb: True if upper receiver sideband USB,
		outer: True if 'outer' rather than 'inner' polyphase filter bank portion (cf. NOEMA docs)
		subbands: polyfix subband indices for which to compute the VEX channel frequencies
		'''
		lo2_sign, fIF2_sign = +1, +1
		if not rxUsb: lo2_sign = -1
		if not outer: fIF2_sign = -1

		refFreq_MHz = 1e3*(self.lo1_GHz + lo2_sign*self.lo2_GHz)
		netSB_sign = lo2_sign * fIF2_sign

		label = 'U'  # 'U' for USB
		if netSB_sign <= 0:
			label = 'L'

		channels = []
		for subband in subbands:
			edge_freq = refFreq_MHz + netSB_sign*subband*self.bw_MHz - netSB_sign*self.bw_MHz/2
			channels.append([edge_freq, label])

		return channels


	def __print_chan_def(self, freq_MHz, sideband, chNr, pollabel, bandlabel):

		bbc = self.pol2bbcnr[pollabel]
		if len(bandlabel) < 1:
			print('%schan_def = &B: %.2f MHz : %1s : %.2f MHz : &CH%02d : &BBC%02d : &NoCal;' % (self.indent, freq_MHz, sideband, self.bw_MHz, chNr, bbc))
		else:
			print('%schan_def = &B: %.2f MHz : %1s : %.2f MHz : &CH%02d : &BBC%02d : &NoCal; * %s' % (self.indent, freq_MHz, sideband, self.bw_MHz, chNr, bbc, bandlabel))


	def __generate_5G_IF_equivalent(self, lo1_GHz, lo2_GHz=7740.0, recorders=[1,2,3,4]):
		'''
		Generate VEX chan_defs in the 10G-to-Mark6 cabling layout of the EHT 5-9 GHz equivalent
		mode at NOEMA (GMVA 86 GHz, EHT 230 GHz)
		'''

		def subblock(usb, outer, subbands, polzn):
			channelblock = self.__generate_block(rxUsb=usb, outer=outer, subbands=subbands)
			for idx,(freq_MHz,sideband) in enumerate(channelblock):
				bandlabel = self.bandlabels.lookUp(freq_MHz*1e-3,sideband)
				self.__print_chan_def(freq_MHz, sideband, idx + self.nvexchannels, polzn, bandlabel)
			self.nvexchannels += len(subbands)

		self.lo1_GHz, self.lo2_GHz = lo1_GHz, lo2_GHz
		self.nvexchannels = 1

		print('def FREQ_Nn; * derived for on lo1=%.3f lo2=%.3f GHz' % (lo1_GHz, lo2_GHz))
		print('%ssample_rate = %.1f Ms/sec;  * (2bits/sample)' % (self.indent, 2*self.bw_MHz))

		USB, LSB = True, False
		OUTER, INNER = True, False

		if 1 in recorders:

			print('%s* Recorder 1, slot 1, LSB-Inner, LCP, subbands 0-11,44-47' % (self.indent))
			subblock(LSB, INNER, range(12), 'L')
			subblock(LSB, INNER, range(44,48), 'L')
			print('%s* Recorder 1, slot 2, LSB-Outer, LCP, subbands 0-15' % (self.indent))
			subblock(LSB, OUTER, range(16), 'L')
			print('%s* Recorder 1, slot 3, LSB-Outer, RCP, subbands 0-15' % (self.indent))
			subblock(LSB, OUTER, range(16), 'R')
			print('%s* Recorder 1, slot 4, LSB-Inner, RCP, subbands 0-11,44-47' % (self.indent))
			subblock(LSB, INNER, range(12), 'R')
			subblock(LSB, INNER, range(44,48), 'R')

		if 2 in recorders:

			print('%s* Recorder 2, slot 1, LSB-Inner, LCP, subbands 32-43,12-15' % (self.indent))
			subblock(LSB, INNER, range(32,44), 'L')
			subblock(LSB, INNER, range(12,16), 'L')
			print('%s* Recorder 2, slot 2, LSB-Inner, LCP, subbands 16-31' % (self.indent))
			subblock(LSB, INNER, range(16,32), 'L')
			print('%s* Recorder 2, slot 3, LSB-Inner, RCP, subbands 16-31' % (self.indent))
			subblock(LSB, INNER, range(16,32), 'R')
			print('%s* Recorder 2, slot 4, LSB-Inner, RCP, subbands 32-43,12-15' % (self.indent))
			subblock(LSB, INNER, range(32,44), 'R')
			subblock(LSB, INNER, range(12,16), 'R')

		if 3 in recorders:

			print('%s* Recorder 3, slot 1, USB-Inner, LCP, subbands 0-11,44-47' % (self.indent))
			subblock(USB, INNER, range(12), 'L')
			subblock(USB, INNER, range(44,48), 'L')
			print('%s* Recorder 3, slot 2, USB-Outer, LCP, subbands 0-15' % (self.indent))
			subblock(USB, OUTER, range(16), 'L')
			print('%s* Recorder 3, slot 3, USB-Outer, RCP, subbands 0-15' % (self.indent))
			subblock(USB, OUTER, range(16), 'R')
			print('%s* Recorder 3, slot 4, USB-Inner, RCP, subbands 0-11,44-47' % (self.indent))
			subblock(USB, INNER, range(12), 'R')
			subblock(USB, INNER, range(44,48), 'R')

		if 4 in recorders:

			print('%s* Recorder 4, slot 1, USB-Inner, LCP, subbands 32-43,12-15' % (self.indent))
			subblock(USB, INNER, range(32,44), 'L')
			subblock(USB, INNER, range(12,16), 'L')
			print('%s* Recorder 4, slot 2, USB-Inner, LCP, subbands 16-31' % (self.indent))
			subblock(USB, INNER, range(16,32), 'L')
			print('%s* Recorder 4, slot 3, USB-Inner, RCP, subbands 16-31' % (self.indent))
			subblock(USB, INNER, range(16,32), 'R')
			print('%s* Recorder 4, slot 4, USB-Inner, RCP, subbands 32-43,12-15' % (self.indent))
			subblock(USB, INNER, range(32,44), 'R')
			subblock(USB, INNER, range(12,16), 'R')

		if 5 in recorders:

			print('%s* Recorder 5, slot 1, LSB-Inner, LCP, subbands 16-31' % (self.indent))
			subblock(LSB, INNER, range(16,32), 'L')
			print('%s* Recorder 5, slot 2, LSB-Inner, RCP, subbands 16-31' % (self.indent))
			subblock(LSB, INNER, range(16,32), 'R')

		print('enddef;')


	def __generate_4G_IF_equivalent(self, lo1_GHz, lo2_GHz=7740.0, recorders=[1,2,3,4]):
		'''
		Generate VEX chan_defs in the 10G-to-Mark6 cabling layout of the EHT 4-8 GHz equivalent
		mode at NOEMA (EHT 345 GHz).
		'''

		def subblock(usb, outer, subbands, polzn):
			channelblock = self.__generate_block(rxUsb=usb, outer=outer, subbands=subbands)
			for idx,(freq_MHz,sideband) in enumerate(channelblock):
				bandlabel = self.bandlabels.lookUp(freq_MHz*1e-3,sideband)
				self.__print_chan_def(freq_MHz, sideband, idx + self.nvexchannels, polzn, bandlabel)
			self.nvexchannels += len(subbands)

		self.lo1_GHz, self.lo2_GHz = lo1_GHz, lo2_GHz
		self.nvexchannels = 1

		print('def FREQ_Nn; * derived for on lo1=%.3f lo2=%.3f GHz' % (lo1_GHz, lo2_GHz))
		print('%ssample_rate = %.1f Ms/sec;  * (2bits/sample)' % (self.indent, 2*self.bw_MHz))

		USB, LSB = True, False
		OUTER, INNER = True, False

		if 1 in recorders:

			print('%s* Recorder 1, slot 1, LSB-Lower/Inner PolyFiX 6 SFP+ 1, LCP, subbands 16-31' % (self.indent))
			subblock(LSB, INNER, range(16,32), 'L')
			print('%s* Recorder 1, slot 2, LSB-Lower/Inner PolyFiX 6 SFP+ 0, LCP, subbands 0-15' % (self.indent))
			subblock(LSB, INNER, range(16), 'L')
			print('%s* Recorder 1, slot 3, LSB-Lower/Inner PolyFiX 2 SFP+ 0, RCP, subbands 0-15' % (self.indent))
			subblock(LSB, INNER, range(16), 'R')
			print('%s* Recorder 1, slot 4, LSB-Lower/Inner PolyFiX 2 SFP+ 1, RCP, subbands 16-31' % (self.indent))
			subblock(LSB, INNER, range(16,32), 'R')

		if 2 in recorders:

			print('%s* Recorder 2, slot 1, LSB-Lower/Inner PolyFiX 6 SFP+ 3, LCP, subbands 48-63' % (self.indent))
			subblock(LSB, INNER, range(48,64), 'L')
			print('%s* Recorder 2, slot 2, LSB-Lower/Inner PolyFiX 6 SFP+ 2, LCP, subbands 32-47' % (self.indent))
			subblock(LSB, INNER, range(32,48), 'L')
			print('%s* Recorder 2, slot 3, LSB-Lower/Inner PolyFiX 2 SFP+ 2, RCP, subbands 32-47' % (self.indent))
			subblock(LSB, INNER, range(32,48), 'R')
			print('%s* Recorder 2, slot 4, LSB-Lower/Inner PolyFiX 2 SFP+ 3, RCP, subbands 48-63' % (self.indent))
			subblock(LSB, INNER, range(48,64), 'R')

		if 3 in recorders:

			print('%s* Recorder 3, slot 1, USB-Lower/Inner PolyFiX 7 SFP+ 1, LCP, subbands 16-31' % (self.indent))
			subblock(USB, INNER, range(16,32), 'L')
			print('%s* Recorder 3, slot 2, USB-Lower/Inner PolyFiX 7 SFP+ 0, LCP, subbands 0-15' % (self.indent))
			subblock(USB, INNER, range(16), 'L')
			print('%s* Recorder 3, slot 3, USB-Lower/Inner PolyFiX 3 SFP+ 0, RCP, subbands 0-15' % (self.indent))
			subblock(USB, INNER, range(16), 'R')
			print('%s* Recorder 3, slot 4, USB-Lower/Inner PolyFiX 3 SFP+ 1, RCP, subbands 16-31' % (self.indent))
			subblock(USB, INNER, range(16,32), 'R')

		if 4 in recorders:

			print('%s* Recorder 4, slot 1, USB-Lower/Inner PolyFiX 7 SFP+ 3, LCP, subbands 48-63' % (self.indent))
			subblock(USB, INNER, range(48,64), 'L')
			print('%s* Recorder 4, slot 2, USB-Lower/Inner PolyFiX 7 SFP+ 2, LCP, subbands 32-47' % (self.indent))
			subblock(USB, INNER, range(32,48), 'L')
			print('%s* Recorder 4, slot 4, USB-Lower/Inner PolyFiX 3 SFP+ 2, RCP, subbands 32-47' % (self.indent))
			subblock(USB, INNER, range(32,48), 'R')
			print('%s* Recorder 4, slot 3, USB-Lower/Inner PolyFiX 3 SFP+ 3, RCP, subbands 48-63' % (self.indent))
			subblock(USB, INNER, range(48,64), 'R')

		print('enddef;')



	def generate(self, lo1_GHz, lo2_GHz=7740.0, recorders=[1,2,3,4], if_range='5-9'):

		if '4-8' in if_range:
			self.__generate_4G_IF_equivalent(lo1_GHz, lo2_GHz, recorders)

		elif '5-9' in if_range:
			self.__generate_5G_IF_equivalent(lo1_GHz, lo2_GHz, recorders)

		else:
			print('Error: Unknown IF range "%s"! Supported are "4-8" and "5-9".')


	def generateAllSubbands(self, lo1_GHz, lo2_GHz=7740.0):
		'''
		Generate a VEX section of all four NOEMA PolyFix basebands in one polarization.
		This is much wider than used for VLBI, but can help visualize tuning and channel placements on sky.
		'''
		def labeledSubblock(usb, outer, subbands):
			channelblock = self.__generate_block(rxUsb=usb, outer=outer, subbands=subbands)
			for idx,(freq_MHz,sideband) in enumerate(channelblock):
				bandlabel = self.bandlabels.lookUp(freq_MHz*1e-3,sideband)
				self.__print_chan_def(freq_MHz, sideband, idx + self.nvexchannels, 'L', bandlabel)
			self.nvexchannels += len(subbands)

		self.lo1_GHz, self.lo2_GHz = lo1_GHz, lo2_GHz
		self.nvexchannels = 1

		print('def FREQ_Nn; * derived for on lo1=%.3f lo2=%.3f GHz' % (lo1_GHz, lo2_GHz))
		print('%ssample_rate = %.1f Ms/sec;  * (2bits/sample)' % (self.indent, 2*self.bw_MHz))

		for rxUsb in [False,True]:
			baseband_listing_order = [False,True] if rxUsb else [True,False]
			for outer_baseband in baseband_listing_order:
				for startingSubband in [0,16,32,48]:
					endingSubband = startingSubband + 16
					basebandLabel = 'outer' if outer_baseband else 'inner'
					print('%s* RX USB=%s, Baseband_Side=%s, any polzn, subbands %d-%d' % (self.indent, rxUsb, basebandLabel, startingSubband, endingSubband-1))
					labeledSubblock(rxUsb, outer_baseband, range(startingSubband,endingSubband))

		print('enddef;')


	def generateIF(self, lo1_GHz=None):

		ref_lo = 85.5

		if lo1_GHz is not None and lo1_GHz > 0:
			ref_lo = lo1_GHz
		elif self.lo1_GHz is not None and self.lo1_GHz > 0:
			ref_lo = self.lo1_GHz

		print('')
		print('$IF;')
		print('def IF_NN; * station Nn')
		print('    if_def = &IF_LCP : A1 : L : %.2f MHz : U ;' % (ref_lo*1e3))
		print('    if_def = &IF_RCP : B1 : R : %.2f MHz : U ;' % (ref_lo*1e3))
		print('enddef;')


	def generateBBC(self):

		print('')
		print('$BBC;')
		print('def BBC_NN; * station Nn, note, polarizations are swapped vs Excel sheet (at least EHT2021 345G)')
		print('    BBC_assign = &BBC%02d :  1 : &IF_LCP;' % (self.pol2bbcnr['L']))
		print('    BBC_assign = &BBC%02d :  1 : &IF_RCP;' % (self.pol2bbcnr['R']))
		print('enddef;')



if __name__ == "__main__":

	opts = parse_args(sys.argv[1:])
	lo1 = float(opts.lo1)
	lo2 = float(opts.lo2)
	recorders = [int(recNr) for recNr in opts.recorders.split(',')]
	link_config = str(opts.cabling)

	if lo2 > lo1:
		print('Error: LO2 (-F %.3f GHz) should not be greater than LO1 (-f %.3f GHz)!' % (lo2, lo1))
		sys.exit(1)

	labels = EHTBandLabels()
	gen = NoemaVexFreqGenerator(labels)

	if opts.do_full_polyfix_overview:
		gen.generateAllSubbands(lo1, lo2)
	else:
		gen.generate(lo1, lo2, recorders, link_config)

	if opts.do_vex_if:
		gen.generateIF()
	if opts.do_vex_bbc:
		gen.generateBBC()
