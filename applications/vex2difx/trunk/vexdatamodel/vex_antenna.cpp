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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/vex_antenna.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/


#include <cstring>
#include <set>
#include "vex_antenna.h"
#include "vex_utility.h"

// get the clock epoch as a MJD value (with fractional component), negative 
// means not found.  Also fills in the first two coeffs, returned in seconds
double VexAntenna::getVexClocks(double mjd, double *coeffs) const
{
	double epoch = -1.0;

	for(std::vector<VexClock>::const_iterator it = clocks.begin(); it != clocks.end(); ++it)
	{
		if(it->mjdStart <= mjd)
		{
			epoch = it->offset_epoch;
			coeffs[0] = it->offset;
			coeffs[1] = it->rate;
		}
	}

	return epoch;
}

void VexAntenna::removeBasebandData(int streamId)
{
	removeBasebandDataByStreamId(vsns, streamId);
	removeBasebandDataByStreamId(files, streamId);
}

bool isVLBA(const std::string &antName)
{
	const std::string VLBAantennas[] = {"BR", "FD", "HN", "KP", "LA", "MK", "NL", "OV", "PT", "SC", ""};       // terminate list with "" !

	for(unsigned int i = 0; VLBAantennas[i] != ""; ++i)
	{
		if(strcasecmp(antName.c_str(), VLBAantennas[i].c_str()) == 0)
		{
			return true;
		}
	}

	return false;
}

bool usesCanonicalVDIF(const std::string &antName)
{
	// Add here any known antennas that use VDIF thread ids that start at 0 for the first record channel and increment by 1 for each additional record channel
	static int unset = true;
	static std::set<std::string> canonicalVDIFUsers;
	std::string ant = antName;
	
	Upper(ant);

	if(unset)
	{
		unset = false;

		char *e = getenv("CANONICAL_VDIF_USERS");
		if(!e)
		{
			// must be capitalized
			canonicalVDIFUsers.insert("BR");
			canonicalVDIFUsers.insert("FD");
			canonicalVDIFUsers.insert("HN");
			canonicalVDIFUsers.insert("KP");
			canonicalVDIFUsers.insert("LA");
			canonicalVDIFUsers.insert("MK");
			canonicalVDIFUsers.insert("NL");
			canonicalVDIFUsers.insert("OV");
			canonicalVDIFUsers.insert("PT");
			canonicalVDIFUsers.insert("SC");
			canonicalVDIFUsers.insert("GB");
			canonicalVDIFUsers.insert("Y");
		}
		else
		{
			char separators[] = ",: ";
			char *t;
			std::string token;

			t = strtok(e, separators);
			while(t)
			{
				token = t;
				Upper(token);
				canonicalVDIFUsers.insert(token);

				t = strtok(0, separators);
			}
		}
		
	}

	return (canonicalVDIFUsers.find(ant) != canonicalVDIFUsers.end());
}

std::ostream& operator << (std::ostream &os, const VexAntenna &x)
{
	os << "Antenna " << x.name <<
		"\n  x=" << x.x << "  dx/dt=" << x.dx <<
		"\n  y=" << x.y << "  dy/dt=" << x.dy <<
		"\n  z=" << x.z << "  dz/dt=" << x.dz <<
		"\n  posEpoch=" << x.posEpoch <<
		"\n  axisType=" << x.axisType <<
		"\n  axisOffset=" << x.axisOffset <<
		"\n  tcalFrequency=" << x.tcalFrequency << std::endl;

	for(std::vector<VexClock>::const_iterator it = x.clocks.begin(); it != x.clocks.end(); ++it)
	{
		os << "  " << *it << std::endl;
	}
	// FIXME: print files, vsns, ports here ADDENDUM: really these structures should move to VexStream

	return os;
}
