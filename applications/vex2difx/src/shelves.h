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

#ifndef __SHELVES_H__
#define __SHELVES_H__

#include <iostream>
#include <map>
#include <string>

class Shelves
{
public:
	int load(const std::string &fileName);
	int snprintShelf(const std::string &vsn, char *outString, int maxLength) const;
	const std::string &getShelf(const std::string &vsn) const;
	bool hasShelf(const std::string &vsn) const { return (shelfMap.find(vsn) != shelfMap.end()); }
	void setShelf(const std::string &vsn, const std::string &location) { shelfMap[vsn] = location; }
	void clear() { shelfMap.clear(); }
	bool empty() { return shelfMap.empty(); }
	size_t size() { return shelfMap.size(); }

	std::map<std::string,std::string> shelfMap;	// maps VSN to shelf location
	static const std::string NoLocation;
};

std::ostream& operator << (std::ostream &os, const Shelves &x);

#endif
