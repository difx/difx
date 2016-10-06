#\if DOXYGEN_IGNORE ############################################################
#                                                                              #
#   Copyright (C) 2016 by John Spitzak                                         #
#                                                                              #
#   This program is free software; you can redistribute it and/or modify       #
#   it under the terms of the GNU General Public License as published by       #
#   the Free Software Foundation; either version 3 of the License, or          #
#   (at your option) any later version.                                        #
#                                                                              #
#   This program is distributed in the hope that it will be useful,            #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#   GNU General Public License for more details.                               #
#                                                                              #
#   You should have received a copy of the GNU General Public License          #
#   along with this program; if not, write to the                              #
#   Free Software Foundation, Inc.,                                            #
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  #
#                                                                              #
#\endif ########################################################################
#<!---======================================================================--->
## \brief Class to provide station names, two-letter and one-letter coded.
#
#  This class provides translations between site names, their two-letter codes,
#  and the one-letter codes used by HOPS.
#
class Translator:
	
	def __init__( self ):
		self._siteList = []
		self._siteList.append( ( "Bd", "B", "BADARY" ) )
		self._siteList.append( ( "Ft", "F", "FORTLEZA" ) )
		self._siteList.append( ( "Ke", "A", "KATH12M" ) )
		self._siteList.append( ( "Kk", "K", "KOKEE" ) )
		self._siteList.append( ( "Ma", "I", "MATERA" ) )
		self._siteList.append( ( "Mc", "M", "MEDICINA" ) )
		self._siteList.append( ( "Ny", "N", "NYALES20" ) )
		self._siteList.append( ( "Sv", "S", "SVETLOE" ) )
		self._siteList.append( ( "Ur", "U", "URUMQI" ) )
		self._siteList.append( ( "Wn", "v", "WETTZ13N" ) )
		self._siteList.append( ( "Ww", "W", "WARK12M" ) )
		self._siteList.append( ( "Wz", "V", "WETTZELL" ) )
		self._siteList.append( ( "Yg", "Y", "YARRA12M" ) )
		self._siteList.append( ( "Ys", "C", "YEBES40M" ) )

	#<!------------------------------------------------------------------------>
	##  Because codes and names are unique, we can have three simple functions
	#   for each component.
	#<!------------------------------------------------------------------------>
	def name( self, code ):
		for site in self._siteList:
			if site[0] == code or site[1] == code:
				return site[2]
		return None
	
	def singleLetter( self, code ):
		for site in self._siteList:
			if site[0] == code or site[2] == code:
				return site[1]
		return None
	
	def twoLetter( self, code ):
		for site in self._siteList:
			if site[1] == code or site[2] == code:
				return site[0]
		return None
	