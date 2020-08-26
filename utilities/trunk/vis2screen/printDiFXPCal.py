#!/usr/bin/python
"""
printDiFXPCal.py version 1.0  Jan Wagner  20200813

Usage: printDiFXPCal.py <output_xxx.difx/PCAL_id_timestamp> [datastream ID number] [toneIdx,[toneIdx,...]]

Prints the content of the PCAL file in a tabular format,
converting complex Re,Im data into polar Mag,Phase form.

"""

import math, sys
import pandas as pd

difxVersion = 240
PC_column_dsId = 3
PC_column_numChan = 4
PC_column_numTones_ch0 = 5

def cart2pol(re,im):
	M = math.sqrt(re*re + im*im)
	P = math.degrees(math.atan2(im,re))
	return M,P

def printTones(pcfile,datastreamId,toneIndices):
	"""Loads selected (or all) PCal tones from a DiFX 2.4.x or later version PCAL file"""

	df = pd.read_csv(pcfile, skiprows=5, header=None, delim_whitespace=True)

	datastreamIDs = list(df[PC_column_dsId].unique())
	if datastreamId is None:
		datastreamId = datastreamIDs[0]
		print('Showing tone(s) of datastream ID %d' % (datastreamId))
	if datastreamId not in datastreamIDs:
		print('Selected datastream IDs %d is not among the available IDs of %s' % (datastreamId, str(datastreamIDs)))
		return

	total_num_chan = 0
	total_num_tones = 0

	ds_dframe = df[df[PC_column_dsId] == datastreamId]
	ds_num_channels = ds_dframe[PC_column_numChan].unique()
	ds_tones_per_channel = ds_dframe.at[1, PC_column_numTones_ch0]

	col = PC_column_numTones_ch0 + 1
	rows_total = df.shape[0]

	if toneIndices is None:
		toneIndices = range(ds_num_channels*ds_tones_per_channel)

	headings = []
	for nn in toneIndices:
		row = 1
		col = PC_column_numTones_ch0 + 1 + 4*nn
		freqMHz = float(ds_dframe.at[row, col])
		bandCode = ds_dframe.at[row, col+1]
		headings.append('%.1f%s' % (freqMHz, bandCode))
	print('%5s %s' % ('', ''.join(['%16s' % h for h in headings])))

	# while row in range(rows)
	for row in range(1,rows_total):
		print('%5d ' % (row)),
		for nn in toneIndices:
			col = PC_column_numTones_ch0 + 1 + 4*nn
			re = float(ds_dframe.at[row, col+2])
			im = float(ds_dframe.at[row, col+3])
			mag,phase = cart2pol(re, im)
			print('%7.2f %+7.2f' % (mag, phase)),
		print('')

if __name__ == '__main__':
	if len(sys.argv) <= 1:
		print(__doc__)
	else:
		pcalfile = sys.argv[1]
		ds = None
		tones = None
		if len(sys.argv) > 2:
			ds = int(sys.argv[2])
		if len(sys.argv) > 3:
			tones = [int(t) for t in sys.argv[3].split(',')]
	
		printTones(pcalfile, ds, tones)
