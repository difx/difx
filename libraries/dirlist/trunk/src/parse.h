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

#ifndef __PARSE_H__
#define __PARSE_H__

#include <string>
#include <sstream>
#include <vector>

// inputString here must have already been tokenized
void separateStringList(std::vector<std::string> &outputList, const std::string &inputString);

// For now, this is simple: just look to see if first and last quotes are same, and if so, return 1..-2
// otherwise return 0..-1
std::string unquoteString(const std::string &str);

bool tokenize(std::vector<std::string> &tokens, std::string &comment, const std::string &inputString);

// Turns a string into MJD 
// The following formats are allowd:
// 1. decimal mjd:  55345.113521
// 2. ISO 8601 dateTtime strings:  2009-03-08T12:34:56.121
// 3. VLBA-like time:   2009MAR08-12:34:56.121
// 4. vex time: 2009y245d08h12m24s"
//
// On error, message added to "error" and 0.0 is returned
double parseTime(const std::string &timeStr, std::stringstream &error);

#endif
