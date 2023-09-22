#!/usr/bin/env python3
"""
Outputs scans for a particular station that can be prioritized
for early e-transfer according to VEX scan "intent", e.g., calibrator scans.

The VEX scan names are reformatted into file names.
The file names can subsequently be used in a filelist for e-transfer.

Usage:

$ getPrioritizedScans.py [--fmt=<filenamepattern>] [--intent=<string>] <station>

where

  <station> : two letter station name from VEX for which to get scans

  <filenamepattern> : pattern according to which to generate scan filenames,
      with the following wildcards,

      %e  : experiment name from VEX
      %s  : two letter station name from VEX, in mixed case (e.g. 'On')
      %S  : -"- in upper case (e.g 'ON')
      %n  : scan number (e.g. 0001)
      %d  : scan starting DOY
      %h  : scan starting hour
      %m  : scan starting minute, gets appended with <blank>/'a'/'b'/... if not unique doy-hhmm

      For example pattern "%e_%s_%d-%h%m_0.vdif"
      would produce vo2307_oe_308-1758_0.vdif, vo2307_oe_308-1801_0.vdif, ...

"""

import re

class VexParser:

	def __init__(self, vexfile):
		f = open(vexfile, 'rt')
		self.vexcontent = [line.strip() for line in f.readlines() if not self._isComment(line)]
		f.close()

		self._analyzeVex()


	def _isComment(self, line):
		s = line.strip()
		if len(s) < 1:
			return True
		if s[0] == '*' and 'intent' not in s:
			return True
		return False


	def _analyzeVex(self):

		reExpname = re.compile('exper_name[\s]*=[\s]*([\w]*);')
		reScanname = re.compile('scan[\s]+([\w-]*)[\s]*;')
		reScanstart = re.compile('start[\s]*=[\s]*([\w]*)[\s]*;')
		reScanstation = re.compile('station[\s]*=[\s]*(\w*)[\s]*:')
		reIntent = re.compile('intent[\s]*=[\s]*"(.*?)"')

		for line in self.vexcontent:
			if reExpname.search(line):
				self.experiment = reExpname.search(line).group(1)
				break

		self.scans = []
		currScanname = ''
		currIntents, currStations = [], []
		currScanNr = 0

		for line in self.vexcontent:

			if reScanname.search(line):
				currScanname = reScanname.search(line).group(1)
				if currScanname[0:2].lower() == 'no':
					# if scan name looks like e.g. 'No0328' or 'no0328':
					currScanNr = int(currScanname[2:])
				else:
					currScanNr += 1

			if reIntent.search(line):
				currIntents.append(reIntent.search(line).group(1))

			if reScanstart.search(line):
				currTime = reScanstart.search(line).group(1)

			if reScanstation.search(line):
				currStations.append(reScanstation.search(line).group(1))			

			if 'endscan' in line:
				entry = {'scannum': currScanNr, 'vexname':currScanname, 'vextime':currTime, 'intents':currIntents, 'stations':currStations}
				self.scans.append(entry)
				currIntents, currStations = [], []

		print(self.experiment)
		print(self.scans)


v = VexParser('e22a26-1-b4.vex.obs') # EHT - has scan intent lines, albeit not intent 'Calibrator' or such as will be used for VGOS, still, good for testing parser
# v = VexParser('vo3124.vex')  # VGOS - no Intent lines yet but can use this VEX to test the parser, since e.g. whitespace differs from EHT

# Todo:
# Python ArgParse of command line args

