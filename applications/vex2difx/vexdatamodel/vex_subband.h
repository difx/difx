/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken & Adam Deller               *
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

#ifndef __VEX_SUBBAND_H__
#define __VEX_SUBBAND_H__

#include <iostream>
#include <vector>

class VexSubband		// Mode-specific frequency details for all antennas
{
public:
	VexSubband(double f=0.0, double b=0.0, char s=' ', char p=' ') : freq(f), bandwidth(b), sideBand(s), pol(p) {}

	double freq;		// (Hz)
	double bandwidth;	// (Hz)
	char sideBand;		// net side band of channel (U or L)
	char pol;		// R or L
};

bool hasDuplicates(const std::vector<VexSubband> &subbands);

bool operator == (const VexSubband &s1, const VexSubband &s2);

std::ostream& operator << (std::ostream &os, const VexSubband &x);

#endif
