/***************************************************************************
 *   Copyright (C) 2021 by Walter Brisken                                  *
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
 * $Id: vex_extension.cpp 10267 2021-10-17 16:30:40Z WalterBrisken $
 * $HeadURL:  $
 * $LastChangedRevision: 10267 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2021-10-17 10:30:40 -0600 (Sun, 17 Oct 2021) $
 *
 *==========================================================================*/

#include "vex_extension.h"

// One VexExtension is one extension parameter line within a def statement
std::ostream& operator << (std::ostream &os, const VexExtension &x)
{
	os << "extension[owner=" << x.owner << ",name=" << x.name;
	if(x.value.size() != x.units.size())
	{
		os << ",<Error: nValue=" << x.value.size() << " != nUnits=" << x.units.size() << ">";
	}
	else
	{
		os << ",values";
		for(unsigned int i = 0; i < x.value.size(); ++i)
		{
			os << (i == 0 ? "=" : ",") << "(" << x.value[i] << (x.units[i].empty() ? "" : " ") << x.units[i] << ")";
		}
	}
	os << "]";

	return os;
}

// One VexExtensionSet is a container of all extensions within a $EXTENSION def block
std::ostream& operator << (std::ostream &os, const VexExtensionSet &x)
{
	os << "Extension " << x.defName << std::endl;
	for(std::vector<VexExtension>::const_iterator iter = x.extension.begin(); iter != x.extension.end(); ++iter)
	{
		os << "    " << *iter << std::endl;
	}

	return os;
}

