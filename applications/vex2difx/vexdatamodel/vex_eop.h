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

#ifndef __VEX_EOP_H__
#define __VEX_EOP_H__

#include <iostream>
#include <string>

extern const double RAD2ASEC;

class VexEOP
{
public:
	VexEOP() : mjd(0), tai_utc(0), ut1_utc(0.0), xPole(0.0), yPole(0.0) {}

	double mjd;		// days
	double tai_utc;		// seconds
	double ut1_utc;		// seconds
	double xPole, yPole;	// radian

	int setkv(const std::string &key, const std::string &value);
};

std::ostream& operator << (std::ostream &os, const VexEOP &x);

#endif
