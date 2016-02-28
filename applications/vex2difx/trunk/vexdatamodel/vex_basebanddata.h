/***************************************************************************
 *   Copyright (C) 2015-2016 by Walter Brisken & Adam Deller               *
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

#ifndef __VEX_BASEBANDDATA_H__
#define __VEX_BASEBANDDATA_H__

#include <iostream>
#include <string>
#include <vector>
#include "interval.h"

class VexBasebandData : public Interval
{
	public:
	VexBasebandData() : streamId(-1) { }
	std::string filename;	// or VSN, ...
	int recorderId;		// as specified in vex file, can be associated using DATASTREAM::recorder parameter; otherwise sort to assign
	int streamId;		// 0-based; -1 means not assigned.  Must be assigned for > 1 datastream operation

	VexBasebandData(const std::string &name, int drive, const Interval &timeRange) : Interval(timeRange), filename(name), recorderId(drive), streamId(-1) {} 
	VexBasebandData(const std::string &name, int drive, double start=-1.0e9, double stop=1.0e9) : Interval(start, stop), filename(name), recorderId(drive), streamId(-1) {}
	bool isMark5() const;
	bool isMark6() const;
};

// returns number of removed entries; negative streamId implies remove from all streams
int removeBasebandDataByStreamId(std::vector<VexBasebandData> &data, int streamId);

int nRepresentedDatastreams(const std::vector<VexBasebandData> &data);

std::ostream& operator << (std::ostream &os, const VexBasebandData &x);

#endif
