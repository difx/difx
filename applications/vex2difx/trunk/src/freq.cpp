/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: util.h 4795 2012-09-06 20:21:51Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision: 4795 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-09-06 14:21:51 -0600 (Thu, 06 Sep 2012) $
 *
 *==========================================================================*/

#include "freq.h"

int getFreqId(std::vector<freq>& freqs, double fq, double bw, char sb, double isr, double osr, int os, int d, int iz, unsigned int t)
{
	for(std::vector<freq>::const_iterator it = freqs.begin(); it != freqs.end(); ++it)
	{
		if(fq  == it->fq &&
		   bw  == it->bw &&
		   sb  == it->sideBand &&
		   isr == it->inputSpecRes &&
		   osr == it->outputSpecRes &&
		   os  == it->overSamp &&
		   d   == it->decimation &&
		   iz  == it->isZoomFreq &&
		   t   == it->toneSetId)
		{
			// use iterator math to get index
			return it - freqs.begin();
		}
	}

	// not in list yet, so add
	freqs.push_back(freq(fq, bw, sb, isr, osr, os, d, iz, t));

	return freqs.size() - 1;
}
