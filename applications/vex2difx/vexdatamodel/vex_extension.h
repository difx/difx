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

#ifndef __VEX_EXTENSION_H__
#define __VEX_EXTENSION_H__

#include <iostream>
#include <vector>
#include <string>

// One VexExtension is one extension parameter line within a def statement
class VexExtension
{
public:
	VexExtension() {};
	std::string owner;
	std::string name;
	std::vector<std::string> value;
	std::vector<std::string> units;
private:
};

// One VexExtensionSet is a container of all extensions within a $EXTENSION def block
class VexExtensionSet
{
public:
	VexExtensionSet() {};
	std::string defName;
	std::vector<VexExtension> extension;
private:
};

std::ostream& operator << (std::ostream &os, const VexExtension &x);

std::ostream& operator << (std::ostream &os, const VexExtensionSet &x);

#endif
