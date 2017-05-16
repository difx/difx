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

#include <iostream>
#include "parse.h"

int main(int argc, char **argv)
{
	std::vector<std::string> tokens;
	std::vector<std::string>::const_iterator it;
	std::string comment;
	bool ok;

	for(int a = 1; a < argc; ++a)
	{
		std::cout << "Input string: " << argv[a] << std::endl;

		ok = tokenize(tokens, comment, argv[a]);
		if(!ok)
		{
			std::cout << "Error tokenizing" << std::endl;
		}
		else
		{
			for(it = tokens.begin(); it != tokens.end(); ++it)
			{
				std::cout << "Token: " << *it << std::endl;
			}
			std::cout << "Comment: " << comment << std::endl;
		}
	}

	return 0;
}
