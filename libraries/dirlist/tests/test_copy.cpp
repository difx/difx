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
#include <fstream>
#include "dirlist.h"
#include "old_dirlist.h"
#include "dirlist_exception.h"

int main(int argc, char **argv)
{
	if(argc != 3)
	{
		std::cerr << "Usage: " << argv[0] << " <infile> <outfile>" << std::endl;
	}
	else
	{
		DirList D;

		try
		{
			std::fstream out;

			D.load(argv[1]);

			out.open(argv[2], std::fstream::out);
			if(!out.is_open())
			{
				std::cerr << "Cannot open " << argv[2] << " for write." << std::endl;
			}
			else
			{
				D.print(out);
				out.close();
			}
		}
		catch (DirListException &e)
		{
			std::cerr << "Error: " << e.what() << std::endl;
		}
	}

	return 0;
}
