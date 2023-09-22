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

#include <sstream>
#include <cmath>
#include "vex_eop.h"

const double RAD2ASEC=180.0*3600.0/M_PI;

int VexEOP::setkv(const std::string &key, const std::string &value)
{
	std::stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "tai_utc")
	{
		ss >> tai_utc;
	}
	else if(key == "ut1_utc")
	{
		ss >> ut1_utc;
	}
	else if(key == "xPole")
	{
		ss >> xPole;
		xPole /= RAD2ASEC;
	}
	else if(key == "yPole")
	{
		ss >> yPole;
		yPole /= RAD2ASEC;
	}
	else
	{
		std::cerr << "Warning: EOP: Unknown parameter '" << key << "'." << std::endl;
		++nWarn;
	}

	return nWarn;
}

std::ostream& operator << (std::ostream &os, const VexEOP &x)
{
	os << "EOP(" << x.mjd << ", " << x.tai_utc << ", " << x.ut1_utc << ", " << (x.xPole*RAD2ASEC) << ", " << (x.yPole*RAD2ASEC) << ")";

	return os;
}
