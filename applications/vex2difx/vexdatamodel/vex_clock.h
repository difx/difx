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

#ifndef __VEX_CLOCK_H__
#define __VEX_CLOCK_H__

#include <iostream>

class VexClock
{
public:
	VexClock() : mjdStart(0.0), offset(0.0), rate(0.0), accel(0.0), jerk(0.0), offset_epoch(50000.0) {}
	void flipSign() 
	{ 
		offset = -offset;
		rate = -rate;
		if(accel != 0.0)	// don't negate 0.0 to prevent equality test from failing
		{
			accel = -accel;
		}
		if(jerk != 0.0)
		{
			jerk = -jerk;
		}
	}

	double mjdStart;	// [mjd]
	double offset;		// [sec]
	double rate;		// [sec/sec]
	double accel;		// [sec/sec^2]	// Note: this and "jerk" kept as separate items to preserve backward compat.
	double jerk;		// [sec/sec^3]
	double offset_epoch;	// [mjd]
};

std::ostream& operator << (std::ostream &os, const VexClock &x);

#endif
