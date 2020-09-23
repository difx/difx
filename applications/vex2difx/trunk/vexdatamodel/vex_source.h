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

#ifndef __VEX_SOURCE_H__
#define __VEX_SOURCE_H__

#include <iostream>
#include <string>
#include <vector>

class VexSource
{
public:
	VexSource() : ra(0.0), dec(0.0), calCode(' ') {}
	bool hasSourceName(const std::string &name) const;
	void setSourceType(const char *t1, const char *t2);

	std::string defName;			// in the "def ... ;" line in Vex
	std::string sourceType1;
	std::string sourceType2;
	
	std::vector<std::string> sourceNames;	// from source_name statements
	double ra;		// (rad)
	double dec;		// (rad)
	// FIXME: add "ref_coord_frame" value here (e.g., J2000)

	char calCode;

	static const unsigned int MAX_SRCNAME_LENGTH = 12;
};

std::ostream& operator << (std::ostream &os, const VexSource &x);

#endif
