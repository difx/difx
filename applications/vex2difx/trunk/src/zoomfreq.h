/***************************************************************************
 *   Copyright (C) 2009-2017 by Walter Brisken                             *
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
 * $HeadURL: $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __ZOOMFREQ_H__
#define __ZOOMFREQ_H__

#include <ostream>

class freq;

class ZoomFreq
{
public: 
	//constructor
	ZoomFreq();

	//method
	void initialise(double freq, double bw, bool corrparent, int specavg); // Hz
	bool matchesFreqSense(const freq* rhs);
	bool matchesFreq(const freq* rhs);

	//variables
	double frequency, bandwidth; // Hz
	int spectralaverage;
	bool correlateparent;
};

//zoomfreq.cpp
std::ostream& operator << (std::ostream& os, const ZoomFreq& f);

#endif
