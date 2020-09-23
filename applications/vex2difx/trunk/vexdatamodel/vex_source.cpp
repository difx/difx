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
	os << "  ra=" << x.ra <<
		"\n  dec=" << x.dec << std::endl;

	return os;
}

void VexSource::setSourceType(const char *t1, const char *t2)
{
	sourceType1 = t1 ? t1 : "";
	sourceType2 = t2 ? t2 : "";
}
