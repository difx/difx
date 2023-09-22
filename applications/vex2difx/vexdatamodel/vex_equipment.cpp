/***************************************************************************
 *   Copyright (C) 2022 Walter Brisken                                     *
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

#include "vex_equipment.h"

void VexEquipment::addSetting(const std::string &function, const std::vector<std::string> &values)
{
	settings[function] = values;
}

void VexEquipment::addInfo(const std::string &name, const std::vector<std::string> &values)
{
	info[name] = values;
}

std::ostream& operator << (std::ostream &os, const VexEquipment &x)
{
	os << "Equipment(type=" << x.type << " device=" << x.device << " link=" << x.link << " label=" << x.label << ")" << std::endl;

	for(std::map<std::string, std::vector<std::string> >::const_iterator s = x.settings.begin(); s != x.settings.end(); ++s)
	{
		os << "    Setting: " << s->first;
		for(std::vector<std::string>::const_iterator v = s->second.begin(); v != s->second.end(); ++v)
		{
			os << (v == s->second.begin() ? " = " : " : ") << *v;
		}
		os << std::endl;
	}

	for(std::map<std::string, std::vector<std::string> >::const_iterator i = x.info.begin(); i != x.info.end(); ++i)
	{
		os << "    Info: " << i->first;
		for(std::vector<std::string>::const_iterator v = i->second.begin(); v != i->second.end(); ++v)
		{
			os << (v == i->second.begin() ? " = " : " : ") << *v;
		}
		os << std::endl;
	}

	return os;
}
