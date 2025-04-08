/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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

#include "mediachange.h"

std::ostream& operator << (std::ostream& os, const MediaChange& x)
{
	int p = os.precision();

	os.precision(12);
	os << "MediaChange(" << x.ant << ", " << x.mjdStart << ", " << x.mjdStop << ")";
	os.precision(p);

	return os;
}

int nGap(const std::list<MediaChange> &m, double mjd)
{
	int n = 0;

	for(std::list<MediaChange>::const_iterator it = m.begin(); it != m.end(); ++it)
	{
		if(mjd >= it->mjdStart && mjd <= it->mjdStop)
		{
			++n;
		}
	}

	return n;
}
