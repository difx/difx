/***************************************************************************
 *   Copyright (C) 2015-2022 by Walter Brisken & Adam Deller               *
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

// Note: keep this up to date with enum in vex_antenna.h
const char VexAntenna::nasmythName[][8] =
{
	"none",
	"right",
	"left",

	"error"		// not a legal value; list terminator
};

// get the clock epoch as a MJD value (with fractional component), negative 
// means not found.  Also fills in the first two coeffs, returned in seconds
double VexAntenna::getVexClocks(double mjd, double *coeffs) const
{
	static bool first = true;
	double epoch = -1.0;

	if(first)
	{
		std::cerr << "Developer Warning: VexAntenna::getVexClocks with 2 arguments is deprecated; use the 4 argument one!" << std::endl;
		first = false;
	}

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

// get the clock epoch as a MJD value (with fractional component), negative 
// means not found.  Also fills in the first two coeffs, returned in seconds
double VexAntenna::getVexClocks(double mjd, double *coeffs, int *clockorder, int maxorder) const
{
	double epoch = -1.0;

	for(std::vector<VexClock>::const_iterator it = clocks.begin(); it != clocks.end(); ++it)
	{
		if(it->mjdStart <= mjd)
		{
			int n = 1;
			bool bad = false;

			epoch = it->offset_epoch;
			if(coeffs)
			{
				coeffs[0] = it->offset;
				coeffs[1] = it->rate;
				if(it->accel != 0.0 || it->jerk != 0.0)
				{
					if(maxorder > 1)
					{
						coeffs[2] = it->accel;
						n = 2;
					}
					else
					{
						bad = true;
					}
					if(it->jerk != 0.0)
					{
						if(maxorder > 2)
						{
							coeffs[3] = it->jerk;
							n = 3;
						}
						else
						{
							bad = true;
						}
					}
				}

				if(bad)
				{
					std::cerr << "Developer Error: maxorder=" << maxorder << " is too small for the supplied clock model.  Output is truncated." << std::endl;
				}
			}

			if(clockorder)
			{
				*clockorder = n;
			}
		}
	}

	return epoch;
}

void VexAntenna::removeBasebandData(int streamId)
{
	removeBasebandDataByStreamId(vsns, streamId);
	removeBasebandDataByStreamId(files, streamId);
}

VexAntenna::NasmythType VexAntenna::getNasmyth(const std::string &bandLink) const
{
	if(!nasmyth.empty())
	{
		for(std::map<std::string, NasmythType>::const_iterator it = nasmyth.begin(); it != nasmyth.end(); ++it)
		{
			if(bandLink == it->first || it->first == "ALL")
			{
				return it->second;
			}
		}
	}
	
	return VexAntenna::NasmythNone;
}

VexAntenna::NasmythType stringToNasmyth(const std::string &platform)
{
	if(strcasecmp(platform.c_str(), VexAntenna::nasmythName[VexAntenna::NasmythNone]) == 0)
	{
		return VexAntenna::NasmythNone;
	}
	else if(strcasecmp(platform.c_str(), VexAntenna::nasmythName[VexAntenna::NasmythRight]) == 0)
	{
		return VexAntenna::NasmythRight;
	}
	else if(strcasecmp(platform.c_str(), VexAntenna::nasmythName[VexAntenna::NasmythLeft]) == 0)
	{
		return VexAntenna::NasmythLeft;
	}

	return VexAntenna::NasmythError;
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
	os << "Antenna " << x.name << " [" << x.twoCharSiteCode << " " << x.oneCharSiteCode << "] " << x.difxName <<
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
	for(std::vector<VexExtension>::const_iterator it = x.extensions.begin(); it != x.extensions.end(); ++it)
	{
		os << "  " << *it << std::endl;
	}
	if(!x.nasmyth.empty())
	{
		os << "  Nasmyth receivers=";
		for(std::map<std::string, VexAntenna::NasmythType>::const_iterator it = x.nasmyth.begin(); it != x.nasmyth.end(); ++it)
		{
			os << " " << it->first << ":" << VexAntenna::nasmythName[it->second];
		}
		os << std::endl;
	}
	// FIXME: print files, vsns, ports here ADDENDUM: really these structures should move to VexStream

	return os;
}
