/***************************************************************************
 *   Copyright (C) 2015-2016 by Walter Brisken & Adam Deller               *
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
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <algorithm>
#include "vex_channel.h"

void VexChannel::selectTones(float toneIntervalMHz, float toneBaseMHz, enum ToneSelection selection, double guardBandMHz)
{
	double epsilonHz = 1.0;
	unsigned int tonesInBand;
	float firstToneMHz;

	if(toneIntervalMHz <= 0)
	{
		return;
	}

	if(guardBandMHz < 0.0)
	{
		guardBandMHz = bbcBandwidth*1.0e-6/8.0;	// default to 1/8 of the band
	}

	if(bbcSideBand == 'U')
	{
		unsigned int m = static_cast<int>( (bbcFreq + epsilonHz)*1.0e-6/toneIntervalMHz );
		firstToneMHz = (m+1)*toneIntervalMHz + toneBaseMHz;
		tonesInBand = static_cast<int>((bbcFreq + bbcBandwidth)*1.0e-6 - firstToneMHz)/toneIntervalMHz + 1;
	}
	else
	{
		unsigned int m = static_cast<int>( (bbcFreq - epsilonHz)*1.0e-6/toneIntervalMHz );
		firstToneMHz = m*toneIntervalMHz + toneBaseMHz;
		tonesInBand = static_cast<int>(firstToneMHz - (bbcFreq - bbcBandwidth)*1.0e-6)/toneIntervalMHz + 1;
	}

	if(selection == ToneSelectionVex)
	{
		std::vector<unsigned int>::iterator it;

		// Here what we do is turn negative tone indices (i.e., counting from end of band) to positive ones
		for(it = tones.begin(); it != tones.end(); ++it)
		{
			if(*it < 0)
			{
				*it = tonesInBand + *it;	// For 8 tones: -1 -> 7, -2 -> 6, ...
			}
		}
		sort(tones.begin(), tones.end());
	}
	else
	{
		tones.clear();
	}

	switch(selection)
	{
	case ToneSelectionVex:
		// Nothing to do
		break;
	case ToneSelectionNone:
		// Nothing to do
		break;
	case ToneSelectionEnds:
		if(tonesInBand > 0)
		{
			tones.push_back(0);
		}
		if(tonesInBand > 1)
		{
			tones.push_back(tonesInBand - 1);
		}
		break;
	case ToneSelectionAll:
		for(unsigned int i = 0; i < tonesInBand; ++i)
		{
			tones.push_back(i);
		}
		break;
	case ToneSelectionSmart:
		if(tonesInBand == 1)
		{
			tones.push_back(0);
		}
		else if(tonesInBand == 2)
		{
			tones.push_back(0);
			tones.push_back(1);
		}
		else if(tonesInBand > 2)
		{
			for(unsigned int i = 0; i < tonesInBand; ++i)
			{
				if(bbcSideBand == 'U')
				{
					double f = firstToneMHz + i*toneIntervalMHz;
					if(f > (bbcFreq*1.0e-6+guardBandMHz) && f < ((bbcFreq+bbcBandwidth)*1.0e-6-guardBandMHz))
					{
						if(tones.size() < 2)
						{
							tones.push_back(i);
						}
						else
						{
							tones[1] = i;
						}
					}
				}
				else
				{
					double f = firstToneMHz - i*toneIntervalMHz;
					if(f < (bbcFreq*1.0e-6-guardBandMHz) && f > ((bbcFreq-bbcBandwidth)*1.0e-6+guardBandMHz))
					{
						if(tones.size() < 2)
						{
							tones.push_back(i);
						}
						else
						{
							tones[1] = i;
						}
					}
				}
			}
		}
		if(tonesInBand > 2 && tones.size() < 2 && guardBandMHz > bbcBandwidth*1.0e-6/2000)
		{
			// If not enough tones are found, recurse a bit...
			printf("Recursing %f\n", guardBandMHz/2.0);
			selectTones(toneIntervalMHz, toneBaseMHz, selection, guardBandMHz/2.0);	
		}
		break;
	case ToneSelectionMost:
		for(unsigned int i = 0; i < tonesInBand; ++i)
		{
			if(bbcSideBand == 'U')
			{
				double f = firstToneMHz + i*toneIntervalMHz;
				if(f > (bbcFreq*1.0e-6+guardBandMHz) && f < ((bbcFreq+bbcBandwidth)*1.0e-6-guardBandMHz))
				{
					tones.push_back(i);
				}
			}
			else
			{
				double f = firstToneMHz - i*toneIntervalMHz;
				if(f < (bbcFreq*1.0e-6-guardBandMHz) && f > ((bbcFreq-bbcBandwidth)*1.0e-6+guardBandMHz))
				{
					tones.push_back(i);
				}
			}
		}
		if(tones.size() < tonesInBand && tones.size() < 2 && guardBandMHz > bbcBandwidth*1.0e-6/2000)
		{
			// If not enough tones are found, recurse a bit...
			selectTones(toneIntervalMHz, toneBaseMHz, selection, guardBandMHz/2.0);	
		}
		break;
	default:
		std::cerr << "Error: selectTones: unexpected value of selection: " << selection << std::endl;
		
		exit(EXIT_FAILURE);
	}
}

char VexChannel::bandCode() const
{
	if(bbcFreq < 1.0e9)
	{
		return 'P';
	}
	else if(bbcFreq < 2.0e9)
	{
		return 'L';
	}
	else if(bbcFreq < 3.0e9)
	{
		return 'S';
	}
	else if(bbcFreq < 7.9e9)
	{
		return 'C';
	}
	else if(bbcFreq < 9.5e9)
	{
		return 'X';
	}
	else if(bbcFreq < 17.0e9)
	{
		return 'U';
	}
	else if(bbcFreq < 25.0e9)
	{
		return 'K';
	}
	else if(bbcFreq < 40.5e9)
	{
		return 'A';
	}
	else if(bbcFreq < 60.0e9)
	{
		return 'Q';
	}
	else if(bbcFreq < 100.0e9)
	{
		return 'W';
	}

	return '?';
}

bool operator ==(const VexChannel &c1, const VexChannel &c2)
{
	if( (c1.recordChan  != c2.recordChan)   ||
	    (c1.subbandId   != c2.subbandId)    ||
	    (c1.ifName      != c2.ifName)       ||
	    (c1.bbcFreq     != c2.bbcFreq)      ||
	    (c1.bbcSideBand != c2.bbcSideBand)  ||
	    (c1.tones       != c2.tones) )
	{
		return false;
	}

	return true;
}

bool operator <(const VexChannel &c1, const VexChannel &c2)
{
	return c1.name < c2.name;
}

std::ostream& operator << (std::ostream &os, const VexChannel &x)
{
	os << "[name=" << x.name << " BBC=" << x.bbcName << " IF=" << x.ifName << " s=" << x.subbandId << " -> r=" << x.recordChan << " t=" << x.threadId << " tones=";
	for(std::vector<unsigned int>::const_iterator v = x.tones.begin(); v != x.tones.end(); ++v)
	{
		if(v != x.tones.begin())
		{
			os << ",";
		}
		os << *v;
	}
	os << "]";

	return os;
}
