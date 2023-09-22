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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __VEX_SCAN_H__
#define __VEX_SCAN_H__

#include <ostream>
#include <string>
#include <map>
#include <vector>
#include <set>
#include "interval.h"
#include "vex_intent.h"

class VexScan : public Interval
{
public:
	std::string defName;				// name of this scan
	std::string intent;				// intent of this scan (captured from comment).  This field is deprecated.
	std::vector<VexIntent> scanIntent;		// "key=value" form of scan intent from vex2 $SCHED intent lines

	std::string modeDefName;
	std::string sourceDefName;			// pointing center
	std::vector<std::string> phaseCenters;		// correlation centers; must include sourceDefName if that is to be correlated
	std::map<std::string,Interval> stations;
	std::map<std::string,bool> recordEnable;	// This is true if the drive number is non-zero
	double size;					// [bytes] approx. correlated size
	double mjdVex;					// The start time listed in the vex file

	VexScan(): size(0), mjdVex(0.0) {};
	const Interval *getAntennaInterval(const std::string &antName) const;
	bool hasAntenna(const std::string &antName) const;
	bool getRecordEnable(const std::string &antName) const;
	void addToSourceSet(std::set<std::string> &sourceSet, bool incPointingCenter) const;
};

std::ostream& operator << (std::ostream &os, const VexScan &x);

#endif
