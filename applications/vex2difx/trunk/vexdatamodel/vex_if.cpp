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

#include <regex.h>
#include "vex_if.h"

double VexIF::getLowerEdgeFreq() const
{
	double bandCenter = ifSSLO;

	// Calculate the center of the 500-1000 MHz IF range;
	if(ifSideBand == 'L')
	{
		bandCenter -= 750.0e6;
	}
	else
	{
		bandCenter += 750.0e6;
	}

	return bandCenter - 500.0e6;
}

std::string VexIF::bandName() const
{
	regex_t rxMatch;
	regmatch_t matchPtr[2];
	// Look for a name based on a comment in the Vex file

	if(comment.empty())
	{
		return "";
	}

	regcomp(&rxMatch, " ([0-9]+[cm]m) ", REG_EXTENDED);

	if(regexec(&rxMatch, comment.c_str(), 2, matchPtr, 0) == 0)
	{
		char buffer[8];
		int len = matchPtr[1].rm_eo-matchPtr[1].rm_so;

		comment.copy(buffer, len, matchPtr[1].rm_so);
		buffer[len] = 0;
		
		regfree(&rxMatch);

		return buffer;
	}

	regfree(&rxMatch);

	return "";
}

std::string VexIF::VLBABandName() const
{
	double bandCenter = ifSSLO;
	
	std::string bn = bandName();
	if(!bn.empty())
	{
		return bn;
	}

	// Calculate the center of the 500-1000 MHz IF range;
	if(ifSideBand == 'L')
	{
		bandCenter -= 750.0e6;
	}
	else
	{
		bandCenter += 750.0e6;
	}

	if(bandCenter < 1.0e9)
	{
		return "90cm";
	}
	else if(bandCenter < 2.0e9)
	{
		return "20cm";
	}
	else if(bandCenter < 3.0e9)
	{
		return "13cm";
	}
	else if(bandCenter < 7.9e9)
	{
		return "6cm";
	}
	else if(bandCenter < 9.5e9)
	{
		return "4cm";
	}
	else if(bandCenter < 17.0e9)
	{
		return "2cm";
	}
	else if(bandCenter < 25.0e9)
	{
		return "1cm";
	}
	else if(bandCenter < 40.5e9)
	{
		return "9mm";
	}
	else if(bandCenter < 60.0e9)
	{
		return "7mm";
	}
	else if(bandCenter < 100.0e9)
	{
		return "3mm";
	}

	return "None";
}

std::ostream& operator << (std::ostream &os, const VexIF &x)
{
	os << "[name=" << x.name << ", SSLO=" << x.ifSSLO << ", sb=" << x.ifSideBand << ", pol=" << x.pol << ", phaseCalInterval=" << x.phaseCalIntervalMHz << " MHz, phaseCalBase=" << x.phaseCalBaseMHz << " MHz]";

	return os;
}
