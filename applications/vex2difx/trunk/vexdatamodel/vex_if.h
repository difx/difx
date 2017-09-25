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

#ifndef __VEX_IF_H__
#define __VEX_IF_H__

#include <iostream>
#include <string>

class VexIF
{
public:
	VexIF() : ifSSLO(0.0), ifSideBand(' '), pol(' '), phaseCalIntervalMHz(0) {}
	std::string bandName() const;
	std::string VLBABandName() const;
	double getLowerEdgeFreq() const;

	std::string name;
	double ifSSLO;		// [Hz] SSLO of the IF
	char ifSideBand;	// U or L
	char pol;		// R or L
	float phaseCalIntervalMHz;// MHz, typically 1 or 5 (or 0 if none)
	float phaseCalBaseMHz; // MHz, typically 0 (VEX Rev 1.5b1, 2002)

	// special values needed for VLBA, extracted from comment line
	std::string comment;
};

std::ostream& operator << (std::ostream &os, const VexIF &x);

#endif
