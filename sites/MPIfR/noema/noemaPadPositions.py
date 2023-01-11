#!/usr/bin/python3
# -*- coding: utf-8 -*-
'''
A Python version of Dave Grahams 2010burepos.pl.

Converts NOEMA pad positions into VLBI wgs84 X,Y,Z ECEF coordinates.
'''
import math, sys

__author__    = 'Jan Wagner, Dave Graham'
__license__   = "LGPL"
__version__   = "1.0.0"
__status__    = "Development"

class noemaPadPositions:

	def __init__(self):

		# VLBI position of pad W00 determined ~2010
		self.refecef = [ 4524000.430, 468042.140, 4460309.760 ]
		self.refpad = 'W00'
		self.refLongitude = -5.907917

		# Fiddle factor from 2010burepos.pl "necessary to get us to VLBI standard positions"
		self.padpos_to_axis_intersect = [-2.2, +1.7, +4.0]

		# NOEMA Pads <dx,dy,dz>
		self.pads = {}
		self.pads['W00'] = [   17.4321,  -54.6713, -17.6563 ]
		self.pads['N01'] = [   11.8205,  -55.0239, -11.9726 ]
		self.pads['N02'] = [    6.2035,  -55.3708,  -6.2833 ]
		self.pads['N03'] = [    0.5849,  -55.7232,  -0.5926 ]
		self.pads['N05'] = [  -10.6409,  -56.4214,  10.7789 ]
		self.pads['N07'] = [  -21.8689,  -57.1214,  22.1518 ]
		self.pads['N09'] = [  -33.1008,  -57.8196,  33.5285 ]
		self.pads['N11'] = [  -44.3355,  -58.5136,  44.9087 ]
		self.pads['N13'] = [  -55.5592,  -59.2193,  56.2766 ]
		self.pads['N15'] = [  -66.7863,  -59.9207,  67.6481 ]
		self.pads['N17'] = [  -78.0155,  -60.6202,  79.0233 ]
		self.pads['N20'] = [  -94.8616,  -61.6677,  96.0841 ]
		self.pads['N29'] = [ -145.4009,  -64.8169, 147.2738 ]
		self.pads['N40'] = [ -207.1579,  -68.6682, 209.8223 ]
		self.pads['N49'] = [ -240.8464,  -70.7678, 243.9441 ]
		self.pads['N54'] = [ -285.7644,  -73.5672, 289.4398 ]
		self.pads['W01'] = [   16.2163,  -62.4889, -16.4266 ]
		self.pads['W05'] = [   11.3515,  -93.7227, -11.5024 ]
		self.pads['W08'] = [    7.7069, -117.1487,  -7.8139 ]
		self.pads['W09'] = [    6.4919, -124.9643,  -6.5813 ]
		self.pads['W10'] = [    5.2780, -132.7724,  -5.3512 ]
		self.pads['W12'] = [    2.8484, -148.3853,  -2.8915 ]
		self.pads['W20'] = [   -6.8766, -210.8708,   6.9551 ]
		self.pads['W23'] = [  -10.5202, -234.3135,  10.6451 ]
		self.pads['W27'] = [  -15.3883, -265.5590,  15.5711 ]
		self.pads['E03'] = [   21.0757,  -31.2318, -21.3473 ]
		self.pads['E04'] = [   22.2913,  -23.4309, -22.5785 ]
		self.pads['E10'] = [   29.5828,   23.4370, -29.9600 ]
		self.pads['E12'] = [   32.0177,   39.0608, -32.4249 ]
		self.pads['E16'] = [   36.8755,   70.3000, -37.3469 ]
		self.pads['E18'] = [   39.3054,   85.9189, -39.8081 ]
		self.pads['E23'] = [   45.3817,  124.9799, -45.9624 ]
		self.pads['E24'] = [   46.5964,  132.7882, -47.1932 ]
		self.pads['E36'] = [   61.1899,  226.5125, -61.9599 ]
		self.pads['E48'] = [   75.7762,  320.2389, -76.7281 ]


	def padExists(self, padname: str) -> bool:
		return padname in self.pads


	def getPadNames(self):
		return self.pads.keys()


	def getPadCoordsECEF(self, padname: str):

		ecef = [0,0,0]

		if padname in self.pads:

			refpadpos = self.pads[self.refpad]
			padpos = self.pads[padname]

			# Offset from reference pad
			dxyz = [padpos[n]-refpadpos[n] for n in range(3)]

			# Projection with Longitude
			rl_rad = self.refLongitude * 3.1415926535/180.0
			projdxyz = [
				dxyz[0]*math.cos(rl_rad)  + dxyz[1]*math.sin(rl_rad),
				-dxyz[1]*math.cos(rl_rad) + dxyz[0]*math.sin(rl_rad),
				dxyz[2]
			]

			# ECEF with diff signs "+-+" as in 2010burepos.pl
			ecef = [self.refecef[0]+projdxyz[0], self.refecef[1]-projdxyz[1], self.refecef[2]+projdxyz[2]]

			# Adjust ECEF coords by 2010burepos.pl fiddle factor
			if padname != 'W00':
				ecef = [ecef[n]+self.padpos_to_axis_intersect[n] for n in range(3)]

		return ecef


if __name__ == "__main__":

	noemapads = noemaPadPositions()

	if '-h' in sys.argv or '--help' in sys.argv:
		print("noemaPadPositions.py [--help|-h] [-x|--vex] [<padname>]")
		sys.exit(0)

	padlist = [s for s in sys.argv[1:] if s[0]!='-']
	if len(padlist) < 1:
		padlist = noemapads.getPadNames()

	doVex = '-x' in sys.argv or '--vex' in sys.argv
	if doVex:
		print('def NOEMA;')
		print('     site_name = NOEMA;')
		print('     site_ID = Nn;')
		print('     site_type = fixed;')
		print('     site_velocity =  0.0 m/yr : 0.0 m/yr : 0.0 m/yr;')
		print('     site_position_epoch = 0;')

	for pad in padlist:
		coord = noemapads.getPadCoordsECEF(pad)
		if doVex:
			print('     site_position = %.4f m : %.4f m : %.4f m; * position of pad %s' % (coord[0],coord[1],coord[2],pad))
		else:
			print(pad, coord)

	if doVex:
		print('enddef;')

