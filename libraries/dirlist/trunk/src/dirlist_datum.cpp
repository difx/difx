/***************************************************************************
 *   Copyright (C) 2016-2017 by Walter Brisken                             *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <cstdio>
#include <cstdlib>
#include <ostream>
#include <string>
#include <vector>
#include "dirlist_exception.h"
#include "dirlist_datum.h"
#include "parse.h"

// Parses a line from, e.g., output of vsum
bool DirListDatum::setFromOldFileListString(const char *str)
{
	const int MaxFileNameLength = 256;
	char scanName[MaxFileNameLength];
	char mjd1str[60], mjd2str[60];
	double mjd1, mjd2;
	int n, p;
	std::stringstream error;

	n = sscanf(str, "%s%s%s%n", scanName, mjd1str, mjd2str, &p);
	if(n == 1)
	{
		mjd1 = mjd2 = 0.0;
	}
	else if(n == 3)
	{
		mjd1 = parseTime(mjd1str, error);
		mjd2 = parseTime(mjd2str, error);
	}
	else
	{
		return false;
	}
	if(!error.str().empty())
	{
		return false;
	}
	if(mjd2 < mjd1 || mjd1 < 10000.0 || mjd1 > 1000000.0)
	{
		return false;
	}

	setName(scanName);
	setMjdStart(static_cast<int>(mjd1));
	setSecStart(86400.0*(mjd1 - static_cast<int>(mjd1)));
	setDuration(86400.0*(mjd2 - mjd1));
	setComment(str+p);

	return true;
}

void DirListDatum::print(std::ostream &os, bool doEOL) const
{
	int p = os.precision();
	os.precision(12);
	os << getName() << " " << getMjdStart() << " " << getSecStart() << " " << getDuration();
	os.precision(p);
	if(doEOL)
	{
		os << std::endl;
	}
}

void DirListDatum::printComment(std::ostream &os, bool doEOL) const
{
	os << " # " << comment;
	if(doEOL)
	{
		os << std::endl;
	}
}

bool DirListDatum::setFromTokens(const std::vector<std::string> &tokens)
{
	if(tokens.size() < 4)
	{
		std::stringstream msg;

		msg << "DirListDatum::setFromTokens(): wrong number of tokens.  Should be 4.  Was " << tokens.size();
		throw DirListException(msg.str(), DirListException::TypeParseError);
	}
	setName(tokens[0]);
	setMjdStart(atoi(tokens[1].c_str()));
	setSecStart(atof(tokens[2].c_str()));
	setDuration(atof(tokens[3].c_str()));

	return true;
}

std::ostream& operator << (std::ostream &os, const DirListDatum &x)
{
	os << x.getName() << " " << x.getMjdStart() << " " << x.getSecStart() << " " << x.getDuration();

	return os;
}
