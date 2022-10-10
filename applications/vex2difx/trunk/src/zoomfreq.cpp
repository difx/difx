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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include "zoomfreq.h"
#include "freq.h"

#include <cmath>
#include <iomanip>

ZoomFreq::ZoomFreq()
{
	initialise(-999e6, -999e6, false, -1);
}

// Initialise, with freq and bw supplied in Hz
void ZoomFreq::initialise(double freq, double bw, bool corrparent, int specavg)
{
	frequency = freq;
	bandwidth = bw;
	correlateparent = corrparent;
	spectralaverage = specavg;
}

// Compare 'zoomfreq' against 'freq', ignoring spectral resolution.
// Returns True when 'freq' is USB and the top and bottom edges match those of 'zoomfreq'
bool ZoomFreq::matchesFreqSense(const freq* rhs) const
{
	const double epsilon = 0.000001;
	if(rhs->sideBand == 'L')
	{
		return false;
	}

	if(fabs(this->frequency - rhs->fq) > epsilon)
	{
		return false;
	}

	if(fabs(this->bandwidth - rhs->bw) > epsilon)
	{
		return false;
	}

	return true;
}

// Compare 'zoomfreq' against 'freq', comparing also spectral resolution
// Returns True when 'freq' top and bottom edge (regardless of sideband) are consistent with 'zoomfreq'
bool ZoomFreq::matchesFreq(const freq* rhs) const
{
	const double epsilon = 0.000001;
	double channeloffset;
	double parent_bottom, parent_top;

	if(rhs->sideBand == 'L')           // is parent LSB?
	{
		channeloffset = (rhs->fq - this->frequency - this->bandwidth)/rhs->inputSpecRes;
		parent_bottom = rhs->fq - rhs->bw;
		parent_top = rhs->fq;
	}
	else                            // parent is USB
	{
		channeloffset = (this->frequency - rhs->fq)/rhs->inputSpecRes;
		parent_bottom = rhs->fq;
		parent_top = rhs->fq + rhs->bw;
	}

	if(this->frequency < parent_bottom - epsilon)
	{
		return false;
	}

	if(this->frequency + this->bandwidth > parent_top + epsilon)
	{
		return false;
	}

	if(this->spectralaverage > 0 && this->spectralaverage != rhs->specAvg()) //0 means default to parent
	{
		return false;
	}

	if(fabs(channeloffset - static_cast<int>(channeloffset+0.5)) > epsilon)
	{
		return false;
	}

	return true;
}

std::ostream& operator << (std::ostream& os, const ZoomFreq& f)
{
    os << std::setw(15) << std::setprecision(8)
        << f.frequency*1e-6 << " MHz "<< f.bandwidth*1e-6 << " MHz avg=" << f.spectralaverage << " parent=" << f.correlateparent;
    return os;
}
