/***************************************************************************
 *   Copyright (C) 2022 by Walter Brisken                                  *
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
 * $Id: $
 * $HeadURL: $
 * $LastChangedRevision: $
 * $Author: $
 * $LastChangedDate: $
 *
 *==========================================================================*/

#ifndef __VEX_EQUIPMENT_H__
#define __VEX_EQUIPMENT_H__

#include <iostream>
#include <string>
#include <map>
#include <vector>

// One instance of this for each "equip" parameter of the $DAS section of a vex2 file
class VexEquipment
{
public:
	VexEquipment(const std::string &t, const std::string &d, const std::string &li, const std::string &la = "") : type(t), device(d), link(li), label(la) {}

	// These next for values are directly from the four "equip" parameter fields
	std::string type;	// Equiment category (typically "rack", "recorder" or "remote"; "guest" for guest equipment)
	std::string device;	// Device (e.g., "DBBC_PFB", "RDBE_DDC", "VNDA", ...)
	std::string link;	// Equipment link name
	std::string label;	// (optional)

	std::map<std::string, std::vector<std::string> > settings;	// From "equip_set" parameter; "function" is the key
	std::map<std::string, std::vector<std::string> > info;		// From "equip_info" parameter; "name" is the key

	void addSetting(const std::string &function, const std::vector<std::string> &values);
	void addInfo(const std::string &name, const std::vector<std::string> &values);
};

std::ostream& operator << (std::ostream &os, const VexEquipment &x);

#endif
