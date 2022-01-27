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

#include "vex_scan.h"

const Interval *VexScan::getAntennaInterval(const std::string &antName) const
{
	std::map<std::string,Interval>::const_iterator it = stations.find(antName);
	if(it != stations.end())
	{
		return &it->second;
	}
	else
	{
		return 0;
	}
}

bool VexScan::getRecordEnable(const std::string &antName) const
{
	std::map<std::string,bool>::const_iterator it = recordEnable.find(antName);
	if(it != recordEnable.end())
	{
		return it->second;
	}
	else
	{
		return false;
	}
}

void VexScan::addToSourceSet(std::set<std::string> &sourceSet, bool incPointingCenter) const
{
	if(incPointingCenter)
	{
		sourceSet.insert(sourceDefName);
	}

	for(std::vector<std::string>::const_iterator it = phaseCenters.begin(); it != phaseCenters.end(); ++it)
	{
		sourceSet.insert(*it);
	}
}

std::ostream& operator << (std::ostream &os, const VexScan &x)
{
	os << "Scan " << x.defName << 
		"\n  timeRange=" << (const Interval&)x <<
		"\n  mode=" << x.modeDefName <<
		"\n  pointing source=" << x.sourceDefName;
	if(!x.phaseCenters.empty())
	{
		os <<   "\n  corr source(s)";
		for(std::vector<std::string>::const_iterator iter = x.phaseCenters.begin(); iter != x.phaseCenters.end(); ++iter)
		{
			os << (iter == x.phaseCenters.begin() ? '=' : ',') << *iter;
		}
	}
	os <<   "\n  size=" << x.size << " bytes \n";

	for(std::map<std::string,Interval>::const_iterator iter = x.stations.begin(); iter != x.stations.end(); ++iter)
	{
		os << "  " << iter->first << " range=" << iter->second << std::endl;
	}

	for(std::map<std::string,bool>::const_iterator iter = x.recordEnable.begin(); iter != x.recordEnable.end(); ++iter)
	{
		os << "  " << iter->first << " enable=" << iter->second << std::endl;
	}

	for(std::vector<VexIntent>::const_iterator iter = x.scanIntent.begin(); iter != x.scanIntent.end(); ++iter)
	{
		os << "  intent=" << *iter << std::endl;
	}

	return os;
}
