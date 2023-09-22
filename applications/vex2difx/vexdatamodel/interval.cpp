/***************************************************************************
 *   Copyright (C) 2015-2016 by Walter Brisken                             *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/interval.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <algorithm>
#include "interval.h"

// returns a negative number if no overlap
double Interval::overlap(const Interval &v) const
{
	return std::min(mjdStop, v.mjdStop) - std::max(mjdStart, v.mjdStart);
}

void Interval::logicalAnd(double start, double stop)
{
	if(mjdStart < start)
	{
		mjdStart = start;
	}
	if(mjdStop > stop)
	{
		mjdStop = stop;
	}
}

void Interval::logicalAnd(const Interval &v)
{
	if(mjdStart < v.mjdStart)
	{
		mjdStart = v.mjdStart;
	}
	if(mjdStop > v.mjdStop)
	{
		mjdStop = v.mjdStop;
	}
}

void Interval::logicalOr(double start, double stop)
{
	if(mjdStart > start)
	{
		mjdStart = start;
	}
	if(mjdStop < stop)
	{
		mjdStop = stop;
	}
}

void Interval::logicalOr(const Interval &v)
{
	if(mjdStart > v.mjdStart)
	{
		mjdStart = v.mjdStart;
	}
	if(mjdStop < v.mjdStop)
	{
		mjdStop = v.mjdStop;
	}
}

std::ostream& operator << (std::ostream &os, const Interval &x)
{
	int p = os.precision();

	os.precision(12);
	os << "mjd(" << x.mjdStart << "," << x.mjdStop << ")";
	os.precision(p);

	return os;
}
