/***************************************************************************
 *   Copyright (C) 2021 by Walter Brisken                                  *
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
 * $Id: $
 * $HeadURL: $
 * $LastChangedRevision: $
 * $Author: $
 * $LastChangedDate: $
 *
 *==========================================================================*/

#include <iostream>
#include <fstream>
#include <cstring>
#include "testvex.h"

int testVex(const std::string &vexFile)
{
	const int MaxLineLength=128;
	std::ifstream is;
	char s[MaxLineLength];

	is.open(vexFile.c_str());
	if(is.fail())
	{
		std::cerr << "Error: vex2difx cannot open " << vexFile << std::endl;

		return -1;
	}

	is.getline(s, MaxLineLength);
	if(is.eof())
	{
		std::cerr << "Error: unexpected end of file: " << vexFile << std::endl;

		return -2;
	}

	if(strncmp(s, "$EXPER ", 7) == 0)
	{
		std::cerr << "Error: " << vexFile << " looks like a sked input file and is not vex formatted." << std::endl;

		return -3;
	}

	if(strncmp(s, "VEX", 3) != 0)
	{
		std::cerr << "Error: " << vexFile << " is not a vex file." << std::endl;

		return -4;
	}

	return 0;
}

