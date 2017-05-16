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
#include <cstring>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <dirlist.h>
#include <old_dirlist.h>
#include <old_filelist.h>
#include <dirlist_datum_mark6.h>

const char program[] = "2dirlist";
const char version[] = "0.2";
const char author[] = "Walter Brisken <wbrisken@nrao.edu>";
const char verdate[] = "2016 Nov 07";

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "Usage: %s [options] <inputFile> <outputFile>\n\n", pgm);
	fprintf(stderr, "Options can include:\n\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h      print this useful help information and quit\n\n");
	fprintf(stderr, "Currently input files can be from Mark5 .dir file or a file list\n");
	fprintf(stderr, "output file from vsum or mk5bsum.\n\n");

	return EXIT_SUCCESS;
}

int convert(const char *inputFile, const char *outputFile)
{
	DirList D;
	std::stringstream error;
	std::fstream out;
	int v;

	error.clear();
	std::cout << "Trying to interpret as an old style Mark5 dirlist..." << std::endl;
	v = loadOldDirList(D, inputFile, error);
	if(v != 0)
	{
		std::cout << "That did not work.  Reason was:" << std::endl;
		std::cout << "  " << error.str() << std::endl;		

		error.clear();
		std::cout << "Trying to interpret as a simple file list..." << std::endl;
		v = loadOldFileList(D, inputFile, error);
	}
	else
	{
		std::cout << "Success!" << std::endl;
	}
	if(v != 0)
	{
		std::cout << "That did not work.  Reason was:" << std::endl;
		std::cout << "  " << error.str() << std::endl;		

		error.clear();
		std::cout << "Trying to interpret as a Mark6 (C-Plane) scan list..." << std::endl;
		v = loadMark6SList(D, inputFile, error);
	}
	else
	{
		std::cout << "Success!" << std::endl;
	}
	if(v != 0)
	{
		std::cout << "That did not work.  Reason was:" << std::endl;
		std::cout << "  " << error.str() << std::endl;		

		std::cerr << "File conversion failed." << std::endl;
		
		return EXIT_FAILURE;
	}
	else
	{
		std::cout << "Success!" << std::endl;
	}

	D.setParameter("producedByProgram", program);
	D.setParameter("producedByVersion", version);

	out.open(outputFile, std::fstream::out);
	if(!out.is_open())
	{
		fprintf(stderr, "\nCannot open %s for write\n\n", outputFile);
	}
	D.print(out);
	out.close();

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	const char *inputFile = 0;
	const char *outputFile = 0;

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	for(int a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--help") == 0 || strcmp(argv[a], "-h") == 0)
			{
				return usage(argv[0]);
			}
			else
			{
				fprintf(stderr, "\nUnrecongnized option: %s\n\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
		else if(inputFile == 0)
		{
			inputFile = argv[a];
		}
		else if(outputFile == 0)
		{
			outputFile = argv[a];
		}
		else
		{
			fprintf(stderr, "\nUnexpected parameter: %s\n\n", argv[a]);

			return EXIT_FAILURE;
		}
	}

	if(!outputFile)
	{
		fprintf(stderr, "\nIncomplete command line.\n\n");

		return EXIT_FAILURE;
	}

	return convert(inputFile, outputFile);
}
