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

#include <set>
#include <cctype>
#include "vex_basebanddata.h"

// negative streamId implies remove all
int removeBasebandDataByStreamId(std::vector<VexBasebandData> &data, int streamId)
{
	int n = 0;

	for(std::vector<VexBasebandData>::iterator it = data.begin(); it != data.end(); )
	{
		if(it->streamId == streamId || streamId < 0)
		{
			it = data.erase(it);
			++n;
		}
		else
		{
			++it;
		}
	}

	return n;
}

int nRepresentedDatastreams(const std::vector<VexBasebandData> &data)
{
	std::set<int> streams;

	for(std::vector<VexBasebandData>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		streams.insert(it->streamId);
	}

	return streams.size();
}

bool VexBasebandData::isMark5() const
{
	std::size_t found;

	if(filename.length() != 8)
	{
		return false;
	}
	found = filename.find("-");
	if(found == std::string::npos)
	{
		found = filename.find("+");
	}
	if(found == std::string::npos)
	{
		return false;
	}
	if(found < 1 || found > 6)
	{
		return false;
	}
	for(unsigned int i = 0; i < found; ++i)
	{
		if(!isalpha(filename[i]))
		{
			return false;
		}
	}
	for(unsigned int i = found+1; i < 8; ++i)
	{
		if(!isdigit(filename[i]))
		{
			return false;
		}
	}

	return true;
}

bool VexBasebandData::isMark6() const
{
	std::size_t found;

	if(filename.length() != 8)
	{
		return false;
	}
	found = filename.find("%");
	if(found == std::string::npos)
	{
		return false;
	}
	if(found < 1 || found > 6)
	{
		return false;
	}
	for(unsigned int i = 0; i < found; ++i)
	{
		if(!isalpha(filename[i]))
		{
			return false;
		}
	}
	for(unsigned int i = found+1; i < 8; ++i)
	{
		if(!isdigit(filename[i]))
		{
			return false;
		}
	}

	return true;
}

std::ostream& operator << (std::ostream &os, const VexBasebandData &x)
{
	os << "Baseband(" << x.filename << ", recorderId=" << x.recorderId << ", streamId=" << x.streamId << ", " << (const Interval&)x << ")";

	return os;
}

