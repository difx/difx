#!/usr/bin/python2
# NB: using Python2 because the extremely convenient JIVE 'vex.py' module utilized
# here is based on MultiDict+lex+yacc and is not readily portable to python3 :-(
'''
Usage: plotVexChannels.py <vexfile.vex|.vex.obs> 
                          [<v2dfile.v2d>] [<jobfile.input>]
                          [<freqName> <freqName> ...]

Plots a sky frequency view of all channels in the $FREQ blocks of
the specified VEX file. The $FREQ blocks to be displayed can be
limited to a specific subset.

The produced plot indicates sideband orientation in the standard way.
The channel number is shown in the middle of each channel 'trapezoid'.
This number reflects the order of chan_def appearance in the VEX file,
it does not currently utilize the chan_def channel tags such as &CH01.

If a v2d file is specified and contains ZOOM band definitions, the
respective zoom frequency ranges will be shown underlaid in the plot.

If a DiFX .input file is specified, the frequencies of all visibility
data records to be produced according to the BASELINE table are shown
underlaid in the plot in light-green.
'''

import argparse
import math, fractions, os, re, sys
import vex  # same as utilized by autozooms module

try:
	import matplotlib.cm as colormaps
	import matplotlib.pyplot as plt
	import matplotlib.patches as patches
	from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)
except:
	print('plotVexChannels could not find Python matplotlib, or X11 is not available. Exiting.')
	sys.exit(1)

try:
	import parseDiFX
	gotParseDiFX = True
except:
	gotParseDiFX = False


__title__ = "plotVexChannels - A graphical overview of channels in a VEX file"
__author__ = "Jan Wagner"
__license__ = "GNU GPL v3"
__version__ = "1.0.5"
__copyright__ = "(C) 2020 by Jan Wagner, MPIfR"


def parse_args(args):

	parser = argparse.ArgumentParser(description=__doc__, add_help=True, formatter_class=argparse.RawDescriptionHelpFormatter)
	parser.add_argument('-Z', '--no-zoom',       help='Do not plot zoom bands', action='store_true')
	parser.add_argument('-O', '--no-outputband', help='Do not plot output bands', action='store_true')
	parser.add_argument('-V', '--no-vex',        help='Do not plot VEX chan_defs', action='store_true')
	parser.add_argument('--version', action='version', version='%(prog)s {version}'.format(version=__version__))
	parser.add_argument('files', nargs='*')
	return parser.parse_args(args)


class VexFreq:

	def __init__(self, flow_=22.235, fhigh_=22.235, sideband_=+1):
		self.flow = flow_
		self.fhigh = fhigh_
		self.sideband = sideband_

	def __eq__(self, other):
		if (isinstance(other, VexFreq)):
			return self.flow==other.flow and self.fhigh==other.fhigh and self.sideband==other.sideband
		return False


class VexChannels:
	"""Gathers $FREQ section blocks and their channels from a VEX file. Produces a plot."""

	def __init__(self, verbosity=0):
		self.verbosity = verbosity
		self.clear()

	def clear(self):
		self.freqBlocks = {}
		self.freqBlockNames = []
		self.freqBlockLowestFreq = {}
		self.freqBlockHighestFreq = {}
		self.vexFileName = '<none>'

	def loadVEX(self, vexfilename):
		"""Add all frequency setups (whether actually used or not!) from a VEX file"""

		f = open(vexfilename, 'r')
		v = vex.parse(f.read())
		f.close()

		self.clear()
		self.vexFileName = vexfilename

		for fqname in v['FREQ']:

			channelList = []
			lowestFreq = 1e99
			highestFreq = 0

			for chinfo in v['FREQ'][fqname].getall('chan_def'):

				freq_def, sb_def, bw_def = [s.upper() for s in chinfo[1:4]]
				rescale = 1e-6  # Hz to MHz

				f_Hz = float(freq_def.split()[0])
				bw_Hz = float(bw_def.split()[0])

				if 'MHZ' in freq_def:
					f_Hz *= 1e6
				elif 'GHZ' in freq_def:
					f_Hz *= 1e9

				if 'MHZ' in bw_def:
					bw_Hz *= 1e6
				elif 'GHZ' in bw_def:
					bw_Hz *= 1e9

				if 'L' in sb_def: # LSB
					fqEntry = VexFreq((f_Hz-bw_Hz)*rescale, f_Hz*rescale, -1)
				else: # USB
					fqEntry = VexFreq(f_Hz*rescale, (f_Hz+bw_Hz)*rescale, +1)

				# avoid duplicates (LCP,RCP):
				if fqEntry in channelList:
					#print('dup')
					continue

				lowestFreq = min(lowestFreq, fqEntry.flow)
				highestFreq = max(highestFreq, fqEntry.fhigh)

				channelList.append(fqEntry)

			if len(channelList) > 0:
				self.freqBlocks[fqname] = channelList[:]
				self.freqBlockLowestFreq[fqname] = lowestFreq
				self.freqBlockHighestFreq[fqname] = highestFreq
			if self.verbosity > 0:
				print ('VexChannels::loadVEX() found %d recorded bands in VEX Freq definition %s' % (len(channelList), fqname))

		self.freqBlockNames = self.freqBlocks.keys()


class ZoomFreqs:

	def __init__(self, verbosity=0):
		self.verbosity = verbosity
		self.clear()

	def clear(self):
		self.zoomBlocks = {}
		self.zoomBlockNames = []
		self.zoomBlockLowestFreq = {}
		self.zoomBlockHighestFreq = {}

	def loadInput(self, inputfilename):
		"""Gather all baseline -referenced zoom frequencies listed in the .input file"""

		if not gotParseDiFX:
			print ('No parseDiFX library found, cannot process %s' % (inputfilename))
			return

		difx = parseDiFX.DiFXFile(inputfile)
		if not difx.isvalid():
			print("Could not parse input file %s correctly, skipping it." % (inputfilename))
			return

		cfg = difx.metainfo

		fqIDs = set()

		for b in cfg.baselines:
			ds1 = cfg.datastreams[b.dsaindex]
			ds2 = cfg.datastreams[b.dsbindex]
			for n in range(len(b.dsabandindex)):
				bandnr = min(b.dsabandindex[n])
				if bandnr >= len(ds1.recbandindex):
					f = ds1.zoombandindex[bandnr - len(ds1.recbandindex)]
					fq = ds1.zoomfreqindex[f]
					fqIDs.add(fq)
			for n in range(len(b.dsbbandindex)):
				bandnr = min(b.dsbbandindex[n])
				if bandnr >= len(ds2.recbandindex):
					f = ds2.zoombandindex[bandnr - len(ds2.recbandindex)]
					fq = ds2.zoomfreqindex[f]
					fqIDs.add(fq)

		fqIDs = sorted(list(fqIDs))

		if len(fqIDs) > 0:
			zoomsetName = 'inputfile'
			self.zoomBlocks[zoomsetName] = [VexFreq(cfg.freqs[fq].low_edge(), cfg.freqs[fq].high_edge(), +1) for fq in fqIDs]
			self.zoomBlockLowestFreq[zoomsetName] = min([cfg.freqs[fq].low_edge() for fq in fqIDs])
			self.zoomBlockHighestFreq[zoomsetName] = max([cfg.freqs[fq].high_edge() for fq in fqIDs])
			self.zoomBlockNames = self.zoomBlocks.keys()

	def loadV2D(self, v2dfilename):
		"""Add all zoom setups (whether actually used or not!) from a v2d file"""

		f = open(v2dfilename, 'r')
		lines = f.readlines()
		f.close()

		lines = [line.split('#')[0].strip() for line in lines]
		lines = [re.sub('\s+',' ',line).strip() for line in lines]
		lines = [line for line in lines if len(line)>0]
		n = 0

		currZoomBlock = ''

		for line in lines:
			if line.startswith("ZOOM"):
				currZoomBlock = line.split()[1]
				self.zoomBlocks[currZoomBlock] = []
				self.zoomBlockLowestFreq[currZoomBlock] = 1e99
				self.zoomBlockHighestFreq[currZoomBlock] = 0
			if 'addZoomFreq' in line and 'freq@' in line:
				r = line.find('freq@')
				args = re.split(r'[@/]', line[r:])  # example: ['freq', '86268.000000', 'bw', '32.000000', 'noparent', 'true']
				fqEntry = VexFreq(float(args[1]), float(args[1])+float(args[3]), +1)
				self.zoomBlocks[currZoomBlock].append(fqEntry)
				self.zoomBlockLowestFreq[currZoomBlock] = min(self.zoomBlockLowestFreq[currZoomBlock], fqEntry.flow)
				self.zoomBlockHighestFreq[currZoomBlock] = max(self.zoomBlockHighestFreq[currZoomBlock], fqEntry.fhigh)

		self.zoomBlockNames = self.zoomBlocks.keys()


class OutputbandFreqs:

	def __init__(self, verbosity=0):
		self.verbosity = verbosity
		self.clear()

	def clear(self):
		self.outputFreqs = []
		self.outputLowestFreq = 1e99
		self.outputHighestFreq = 0

	def loadInput(self, inputfilename):
		"""Gather all baseline-outputted frequencies listed in the .input file"""

		if not gotParseDiFX:
			print ('No parseDiFX library found, cannot process %s' % (inputfilename))
			return

		difx = parseDiFX.DiFXFile(inputfile)
		if not difx.isvalid():
			print("Could not parse input file %s correctly, skipping it." % (inputfilename))
			return

		cfg = difx.metainfo
		nfreqs, freqs = cfg.determine_outputfreqs()
		freqs.sort()
		for fq in freqs:
			fqEntry = VexFreq(cfg.freqs[fq].low_edge(), cfg.freqs[fq].high_edge(), +1 if cfg.freqs[fq].lsb else -1)
			self.outputFreqs.append(fqEntry)
			self.outputLowestFreq = min(self.outputLowestFreq, fqEntry.flow)
			self.outputHighestFreq = max(self.outputHighestFreq, fqEntry.fhigh)

	def loadV2D(self, v2dfilename):
		"""Add all outputbands from SETUP section of a v2d file"""

		f = open(v2dfilename, 'r')
		lines = f.readlines()
		f.close()

		lines = [line.split('#')[0].strip() for line in lines]
		lines = [re.sub('\s+',' ',line).strip() for line in lines]
		lines = [line for line in lines if len(line)>0]
		print('OutputbandFreqs::loadV2D')
		print(lines)

		for line in lines:
			#if line.startswith("SETUP"):
			# ...
			if 'addOutputBand' in line and 'freq@' in line:
				r = line.find('freq@')
				args = re.split(r'[@/]', line[r:])  # example: ['freq', '86268.000000', 'bw', '32.000000']
				print('added v2d outputband with args %s' % (str(args)))
				fqEntry = VexFreq(float(args[1]), float(args[1])+float(args[3]), +1)
				self.outputFreqs.append(fqEntry)
				self.outputLowestFreq = min(self.outputLowestFreq, fqEntry.flow)
				self.outputHighestFreq = max(self.outputHighestFreq, fqEntry.fhigh)


class Charting:

	def __init__(self, verbosity=0):
		self.verbosity = verbosity
		self.wedgecount = 0
		self.zoombarcount = 0
		self.obbarcount = 0

	def plotFreqchannelWedge(self, ax, yy, height, channelID, channel, color=[109.0/255,155.0/255,194.0/255]):
		"""Plots a channel:VexFreq in graphical style of a frequency band, as a slanted (USB/LSB) wedge"""

		bw = channel.fhigh-channel.flow
		hh = height/2
		top_pinch = 0.075
		slant = 0.40

		if bw*top_pinch > 10.0:
			top_pinch = 10.0/bw

		# clockwise points (x,y) in a trapezoid/wedge,
		# starting from bottom left:
		x = [channel.flow,channel.fhigh,channel.fhigh-top_pinch*bw,channel.flow+top_pinch*bw]
		if channel.sideband > 0:
			y = [yy-hh, yy-hh, yy+(1-slant)*hh, yy+hh]
		else:
			y = [yy-hh, yy-hh, yy+hh, yy+(1-slant)*hh]

		a = 0.9 + 0.1*(self.wedgecount % 2)
		self.wedgecount += 1

		ax.add_patch(patches.Polygon(xy=zip(x,y), fill=True, edgecolor='black', facecolor=color, alpha=a))

		if channelID:
			ax.text(channel.flow + bw/4, yy-height/4, str(channelID), fontsize=10)


	def plotZoomChannelBar(self, ax, ymin, ymax, channel, color=[255.0/255,152.0/255,143.0/255]):
		"""Plots a channel:VexFreq in graphical style of zoom band"""

		x = [channel.flow, channel.fhigh, channel.fhigh, channel.flow]
		y = [ymin, ymin, ymax, ymax]

		a = 0.3 + 0.1*(self.zoombarcount % 2)
		self.zoombarcount += 1

		ax.add_patch(patches.Polygon(xy=zip(x,y), fill=True, edgecolor='white', facecolor=color, alpha=a))


	def plotOutputbandBar(self, ax, ymin, ymax, channel, color=[0.0/255,152.0/255,143.0/255]):
		"""Plots a channel:VexFreq in graphical style of an visibility data output band"""

		x = [channel.flow, channel.fhigh, channel.fhigh, channel.flow]
		y = [ymin, ymin, ymax, ymax]

		a = 0.1 + 0.1*(self.obbarcount % 2)
		self.obbarcount += 1

		ax.add_patch(patches.Polygon(xy=zip(x,y), fill=True, edgecolor='black', facecolor=color, alpha=a))


	def visualize(self, vexChannels, zoomChannels=None, outputBands=None, fqNameSubset=None):

		allFreqs = vexChannels.freqBlockNames
		wedgeheight = 0.5	# height of USB/LSB direction wedge in plot

		minfreq = 1e99
		maxfreq = -1e99

		# Count freqs to plot
		selectedFreqs = []
		for freq in allFreqs:
			if fqNameSubset and freq not in fqNameSubset:
				if self.verbosity > 0:
					print ('Skipping %s that is not in user specified freq name subset' % (freq))
				continue
			selectedFreqs.append(freq)

		numFreqs = len(selectedFreqs)
		if numFreqs > 0:
			print("Showing VEX $FREQ blocks %s" % (str(selectedFreqs)))

		# Colors
		fig, ax = plt.subplots()
		fig.set_facecolor('white')
		cmap = colormaps.get_cmap(name='winter', lut=(numFreqs+1))

		# Zoom bands
		ymin, ymax = 0, numFreqs+1
		if zoomChannels:
			for zoomblock in zoomChannels.zoomBlockNames:
				zooms = zoomChannels.zoomBlocks[zoomblock]
				for zoom_nr in range(0,len(zooms)): 
					self.plotZoomChannelBar(ax, ymin, ymax, zooms[zoom_nr])
				minfreq = min(minfreq, zoomChannels.zoomBlockLowestFreq[zoomblock])
				maxfreq = max(maxfreq, zoomChannels.zoomBlockHighestFreq[zoomblock])
				ax.text(zoomChannels.zoomBlockLowestFreq[zoomblock], ymin+0.25, 'ZOOM "' + str(zoomblock) + '"', fontsize=10, alpha=0.8)

		# Outputbands; generally identical to zooms
		if outputBands:
			ofreqs = outputBands.outputFreqs
			for outputband_nr in range(len(ofreqs)):
				self.plotOutputbandBar(ax, ymin, ymax, ofreqs[outputband_nr])
			minfreq = min(minfreq, outputBands.outputLowestFreq)
			maxfreq = max(maxfreq, outputBands.outputHighestFreq)

		# VEX channels
		ylevel = 1
		for freq in selectedFreqs:

			minfreq = min(minfreq, vexChannels.freqBlockLowestFreq[freq])
			maxfreq = max(maxfreq, vexChannels.freqBlockHighestFreq[freq])

			channels = vexChannels.freqBlocks[freq]
			lowest_ch_freq = 1e99
			for channel_nr in range(0,len(channels)):
				channel = channels[channel_nr]
				self.plotFreqchannelWedge(ax, ylevel, wedgeheight, channel_nr, channel, color=cmap(allFreqs.index(freq)))
				lowest_ch_freq = min(lowest_ch_freq, channel.flow)
			ax.text(lowest_ch_freq, ylevel+1.10*wedgeheight/2, 'VEX ' + str(freq), fontsize=10, fontweight='bold')

			ylevel += 1

		#ax.spines['right'].set_color('none')
		#ax.spines['top'].set_color('none')
		ax.set_xlim(minfreq, maxfreq)
		ax.set_ylim(0, numFreqs+1)
		ax.set_axisbelow(True)
		ax.xaxis.grid(True, which='major')
		ax.xaxis.set_minor_locator(MultipleLocator(100))
		ax.xaxis.set_major_formatter(FormatStrFormatter('%.2f'))
		ax.tick_params(axis='x', which='minor', colors=[0.95,0.95,0.95])
		plt.tick_params(top=False, bottom=True, left=False, right=False, labelleft=False, labelbottom=True)
		plt.grid(True, which='both', axis='x')

		plt.xlabel('Frequency (MHz)')
		plt.title('Frequency Definitions in VEX file %s' % (vexChannels.vexFileName))



	def visualizeBandComposition(self, zoomChannelBlocks, outputBands):

		wedgeheight = 0.5	# height of USB/LSB direction wedge in plot

		minfreq = 1e99
		maxfreq = -1e99
		ylevel = 1

		numFreqs = len(zoomChannelBlocks.zoomBlocks) + 1

		# Colors
		fig, ax = plt.subplots()
		fig.set_facecolor('white')
		cmap = colormaps.get_cmap(name='winter', lut=(numFreqs+1))

		# Outputbands
		if outputBands:
			lowest_out_freq = 1e99
			for of in outputBands.outputFreqs:
				self.plotFreqchannelWedge(ax, ylevel, wedgeheight, None, of, color=cmap(2))
				self.plotOutputbandBar(ax, 0, ylevel+10, of)
				lowest_out_freq = min(lowest_out_freq, of.flow)
			ax.text(lowest_out_freq, ylevel+1.10*wedgeheight/2, 'Outputbands', fontsize=10, fontweight='bold', alpha=1.0)
			ylevel += 1

		# Zoom bands
		if zoomChannelBlocks:
			for zoomblockname in zoomChannelBlocks.zoomBlockNames:
				for z in zoomChannelBlocks.zoomBlocks[zoomblockname]:
					self.plotFreqchannelWedge(ax, ylevel, wedgeheight, None, z, color=cmap(1))
				ax.text(zoomChannelBlocks.zoomBlockLowestFreq[zoomblockname], ylevel+0.25, 'ZOOM "' + str(zoomblockname) + '"', fontsize=10, fontweight='bold', alpha=1.0)

				minfreq = min(minfreq, zoomChannelBlocks.zoomBlockLowestFreq[zoomblockname])
				maxfreq = max(maxfreq, zoomChannelBlocks.zoomBlockHighestFreq[zoomblockname])
				ylevel += 1

		#ax.spines['right'].set_color('none')
		#ax.spines['top'].set_color('none')
		ax.set_xlim(minfreq, maxfreq)
		ax.set_ylim(0, numFreqs+1)
		ax.set_axisbelow(True)
		ax.xaxis.grid(True, which='major')
		ax.xaxis.set_minor_locator(MultipleLocator(100))
		ax.xaxis.set_major_formatter(FormatStrFormatter('%.2f'))
		ax.tick_params(axis='x', which='minor', colors=[0.95,0.95,0.95])
		plt.tick_params(top=False, bottom=True, left=False, right=False, labelleft=False, labelbottom=True)
		plt.grid(True, which='both', axis='x')

		plt.xlabel('Frequency (MHz)')
		plt.title('DiFX Output Zooms and Outputbands')



if __name__ == "__main__":


	userargs = parse_args(sys.argv[1:])
	if len(userargs.files) < 1:
		sys.exit(0)

	subset = []
	v2dfile = None
	vexfile = None
	inputfile = None

	for fname in userargs.files:
		if fname.endswith('.vex.obs') or fname.endswith('.vex'):
			vexfile = fname
		elif fname.endswith('.v2d'):
			v2dfile = fname
		elif fname.endswith('.input'):
			inputfile = fname
		else:
			subset.append(fname)

	if not vexfile:
		print('Please specify a VEX file')
		sys.exit(1)

	# Containers
	vx = VexChannels(verbosity=0)
	zf = ZoomFreqs(verbosity=0)
	ob = OutputbandFreqs(verbosity=0)
	chart = Charting(verbosity=4)

	# Load vex file, other files if any
	vx.loadVEX(vexfile)

	if v2dfile:
		zf.loadV2D(v2dfile)
		ob.loadV2D(v2dfile)

	if inputfile:
		zf.loadInput(inputfile)
		ob.loadInput(inputfile)

	if userargs.no_zoom:
		zf.clear()
	if userargs.no_outputband:
		ob.clear()
	if userargs.no_vex:
		vx.clear()

	# Plot
	chart.visualize(vx, zoomChannels=zf, outputBands=ob, fqNameSubset=subset)
	if len(ob.outputFreqs)>0 and len(zf.zoomBlockNames)>0:
		chart.visualizeBandComposition(zf, ob)
	plt.show()
