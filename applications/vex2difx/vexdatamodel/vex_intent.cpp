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
 * $Id: vex_intent.cpp 10265 2021-10-16 16:00:54Z WalterBrisken $
 * $HeadURL: $
 * $LastChangedRevision: 10265 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2021-10-16 10:00:54 -0600 (Sat, 16 Oct 2021) $
 *
 *==========================================================================*/

#include "vex_intent.h"

std::ostream& operator << (std::ostream &os, const VexIntent &x)
{
	os << "[id=" << x.identifier << ",value=" << x.value;
	if(!x.source.empty())
	{
		os << ",source=" << x.source;
	}
	os << "]";

 	return os;
 }
