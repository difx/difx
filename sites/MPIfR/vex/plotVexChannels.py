#!/usr/bin/python2
'''
Usage: plotVexChannels.py <vexfile.vex> [<freqName> <freqName> ...]

Plots a sky frequency view of all channels in the $FREQ blocks of
the specified VEX file. The $FREQ blocks to be displayed can be
limited to a specific subset.

The produced plot indicates sideband orientation in the standard way.
The channel number is shown in the middle of each channel 'trapezoid'.
This number reflects the order of chan_def appearance in the VEX file,
it does not currently utilize the chan_def channel tags such as &CH01.
'''

import math, fractions, os, sys
import vex  # same as utilized by autozooms module

try:
	import matplotlib.cm as colormaps
	import matplotlib.pyplot as plt
	import matplotlib.patches as patches
	from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)
except:
	print('plotVexChannels could not find Python matplotlib, or X11 is not available. Exiting.')
	sys.exit(1)


__title__ = "plotVexChannels - A graphical overview of channels in a VEX file"
__author__ = "Jan Wagner"
__license__ = "GNU GPL v3"
__version__ = "1.0.0"
__copyright__ = "(C) 2020 by Jan Wagner, MPIfR"


def usage():
	print('')
	print(__title__)
	print(__copyright__)
	print(__doc__)


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


	def plotFreqchannelWedge(self, ax, yy, height, channel, fstart, fstop, fsideband, color=[109.0/255,155.0/255,194.0/255]):

		bw = fstop-fstart
		hh = height/2
		top_pinch = 0.075
		slant = 0.40

		if bw*top_pinch > 10.0:
			top_pinch = 10.0/bw

		# clockwise points (x,y) in a trapezoid/wedge,
		# starting from bottom left:
		x = [fstart,fstop,fstop-top_pinch*bw,fstart+top_pinch*bw]
		if fsideband > 0:
			y = [yy-hh, yy-hh, yy+(1-slant)*hh, yy+hh]
		else:
			y = [yy-hh, yy-hh, yy+hh, yy+(1-slant)*hh]
		ax.add_patch(patches.Polygon(xy=zip(x,y), fill=True, edgecolor='black', facecolor=color))

		ax.text(fstart + bw/4, yy-height/4, str(channel), fontsize=10)


	def visualize(self, fqNameSubset=None, flow=None, fhigh=None, id=None, axis=[], startmarker=ord('a')):

		allFreqs = self.freqBlockNames
		wedgeheight = 0.5	# height of USB/LSB direction wedge in plot

		minfreq = 1e99
		maxfreq = -1e99
		if fqNameSubset:
			numFreqs = len(fqNameSubset)
		else:
			numFreqs = len(allFreqs)

		fig, ax = plt.subplots()
		fig.set_facecolor('white')
		cmap = colormaps.get_cmap(name='winter', lut=(numFreqs+1))

		ylevel = 1
		for freq in allFreqs:

			if fqNameSubset and freq not in fqNameSubset:
				if self.verbosity > 0:
					print ('Skipping %s that is not in user specified freq name subset' % (freq))
				continue

			minfreq = min(minfreq, self.freqBlockLowestFreq[freq])
			maxfreq = max(maxfreq, self.freqBlockHighestFreq[freq])

			channels = self.freqBlocks[freq]
			lowest_ch_freq = 1e99
			for channel_nr in range(0,len(channels)):
				channel = channels[channel_nr]
				self.plotFreqchannelWedge(ax, ylevel, wedgeheight, channel_nr, channel.flow, channel.fhigh, channel.sideband, color=cmap(allFreqs.index(freq)))
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
		plt.title('Frequency Definitions in VEX file %s' % (self.vexFileName))

		plt.show()


if __name__ == "__main__":

	# User args
	if len(sys.argv) < 2:
		usage()
		sys.exit(0)

	vexfile = sys.argv[1]
	subset = [fqname for fqname in sys.argv[2:]]

	# Help/Version?
	if sys.argv[1]=='-h' or sys.argv[1]=='--help':
		usage()
		sys.exit(1)
	if sys.argv[1]=='-v' or sys.argv[1]=='--version':
		print(__version__)
		sys.exit(1)


	# Load vex file(s)
	a = VexChannels(verbosity=0)
	a.loadVEX(vexfile)

	# Plot
	a.visualize(fqNameSubset=subset)
