/***************************************************************************
 *   Copyright (C) 2015-2021 by Walter Brisken & Adam Deller               *
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

#include "vex_exper.h"

std::ostream& operator << (std::ostream &os, const VexExper &x)
{
	int p = os.precision();

	os.precision(12);
	os << "Experiment " << x.name;
	if(!x.segment.empty())
	{
		os << " " << x.segment;
	}
	os << " mjd(" << x.mjdStart << "," << x.mjdStop << ")";
	os.precision(p);

	return os;
}

