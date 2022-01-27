/***************************************************************************
 *   Copyright (C) 2015-2020 by Walter Brisken & Adam Deller               *
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
#include "vex_source.h"

bool VexSource::hasSourceName(const std::string &name) const
{
	// if result of find is .end(), then it is not in the list
	return find(sourceNames.begin(), sourceNames.end(), name) != sourceNames.end();
}

std::ostream& operator << (std::ostream &os, const VexSource &x)
{
	os << "Source " << x.defName << std::endl;
	for(std::vector<std::string>::const_iterator it = x.sourceNames.begin(); it != x.sourceNames.end(); ++it)
	{
		os << "  name=" << *it << std::endl;
	}
	switch(x.type)
	{
	case VexSource::Star:
		os << "  Type=star" << std::endl;
		os << "    ra=" << x.ra << std::endl;
		os << "    dec=" << x.dec << std::endl;
		break;
	case VexSource::BSP:
		os << "  Type=BSP" << std::endl;
		os << "    file=" << x.bspFile << std::endl;
		os << "    object=" << x.bspObject << std::endl;
		break;
	case VexSource::TLE:
		os << "  Type=TLE" << std::endl;
		os << "    line0='" << x.tle[0] << "'" << std::endl;
		os << "    line1='" << x.tle[1] << "'" << std::endl;
		os << "    line2='" << x.tle[2] << "'" << std::endl;
		break;
	case VexSource::Fixed:
		os << "  Type=Fixed" << std::endl;
		os << "    X = " << x.X << " m" << std::endl;
		os << "    Y = " << x.Y << " m" << std::endl;
		os << "    Z = " << x.Z << " m" << std::endl;
		break;
	default:
		os << "  Type: unsupported" << std::endl;
		break;
	}

	return os;
}

// Returns false if the source_type is not one of the supported ones
bool VexSource::setSourceType(const char *t1, const char *t2, const char *t3)
{
	sourceType1 = t1 ? t1 : "";
	sourceType2 = t2 ? t2 : "";
	sourceType3 = t3 ? t3 : "";

	if(sourceType1 == "star")
	{
		type = Star;
	}
	else if(sourceType1 == "earth_satellite")
	{
		type = EarthSatellite;
	}
	else if (sourceType1 == "bsp")
	{
		type = BSP;
	}
	else if (sourceType1 == "tle")
	{
		type = TLE;
	}
	else if (sourceType1 == "ephemeris")
	{
		type = Ephemeris;
	}
	else
	{
		type = Unsupported;

		return false;
	}

	return true;
}

void VexSource::setTLE(int lineNum, const char *line)
{
	if(lineNum >= 0 && lineNum <= 2)
	{
		type = TLE;
		tle[lineNum] = line;
	}
}

void VexSource::setBSP(const char *fileName, const char *objectId)
{
	type = BSP;
	bspFile = fileName;
	bspObject = objectId;
}

void VexSource::setFixed(double x, double y, double z)
{
	type = Fixed;
	X = x;	// (m)
	Y = y;	// (m)
	Z = z;	// (m)
}
