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
#include <iomanip>

ZoomFreq::ZoomFreq()
{
	initialise(-999e6, -999e6, false, -1);
}

// freq and bw supplied in Hz
void ZoomFreq::initialise(double freq, double bw, bool corrparent, int specavg)
{
	frequency = freq;
	bandwidth = bw;
	correlateparent = corrparent;
	spectralaverage = specavg;
}

std::ostream& operator << (std::ostream& os, const ZoomFreq& f)
{
    os << std::setw(15) << std::setprecision(8)
        << f.frequency*1e-6 << " MHz "<< f.bandwidth*1e-6 << " MHz avg=" << f.spectralaverage << " parent=" << f.correlateparent;
    return os;
}
