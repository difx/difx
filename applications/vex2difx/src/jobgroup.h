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
 * $Id: jobgroup.h 8543 2018-10-16 17:26:50Z JimJacobs $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision: 8543 $
 * $Author: JimJacobs $
 * $LastChangedDate: 2018-10-17 01:26:50 +0800 (三, 2018-10-17) $
 *
 *==========================================================================*/

#ifndef __JOBGROUP_H__
#define __JOBGROUP_H__

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include "job.h"
#include "event.h"
#include "vex_data.h"

class JobGroup : public Interval
{
public:
	std::vector<std::string> scans;
	std::list<Event> events;

	bool hasScan(const std::string &scanName) const;
	void genEvents(const std::list<Event> &eventList);
	void createJobs(std::vector<Job> &jobs, Interval &jobTimeRange, const VexData *V, double minLength, double maxLength, double maxSize) const;
};

std::ostream& operator << (std::ostream &os, const JobGroup &x);

#endif
