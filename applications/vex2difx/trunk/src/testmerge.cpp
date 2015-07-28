/***************************************************************************
 *   Copyright (C) 2009-2015 by Walter Brisken & Adam Deller               *
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
 * $Id: vex2difx.cpp 6854 2015-07-18 16:46:05Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision: 6854 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2015-07-18 10:46:05 -0600 (Sat, 18 Jul 2015) $
 *
 *==========================================================================*/

#include <vector>
#include <set>
#include <sstream>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <algorithm>
#include <sys/time.h>
#include <sys/stat.h>
#include <difxio/difx_input.h>
#include <difxmessage.h>
#include "vex_data.h"
#include "event.h"
#include "corrparams.h"
#include "vexload.h"
#include "util.h"
#include "timeutils.h"
#include "applycorrparams.h"
#include "../config.h"

using namespace std;



int main(int argc, char **argv)
{
	CorrParams *P;
	VexData *V;
	const VexScan * S;
	const SourceSetup * sourceSetup;
	list<Event> events;
	string shelfFile;
	string missingDataFile;	// created if file-based and no files for a particular antenna/job are found
	string v2dFile;
	string command;
	int verbose = 0;
	int ok;
	bool writeParams = 0;
	bool deleteOld = 0;
	bool strict = 1;
	int nWarn = 0;
	int nError = 0;
	int nSkip = 0;
	int nDigit;
	int nJob = 0;

	if(argc < 2)
	{
		cout << "Need v2d file" << endl;

		return EXIT_FAILURE;
	}

	// force program to work in Universal Time
	setenv("TZ", "", 1);
	tzset();


	for(int a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
				strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-f") == 0 ||
				strcmp(argv[a], "--force") == 0)
			{
				strict = 0;
			}
			else if(strcmp(argv[a], "-s") == 0 ||
				strcmp(argv[a], "--strict") == 0)
			{
				strict = 1;
			}
			else
			{
				cerr << "Error: unknown option " << argv[a] << endl;
				cerr << "Run with -h for help information." << endl;
				
				exit(EXIT_FAILURE);
			}
		}
		else
		{
			if(!v2dFile.empty())
			{
				cerr << "Error: multiple configuration files provided, only one expected." << endl;
				cerr << "Run with -h for help information." << endl;
				
				exit(EXIT_FAILURE);
			}
			v2dFile = argv[a];
		}
	}

	if(v2dFile.size() > DIFX_MESSAGE_PARAM_LENGTH-2)
	{
		//job numbers into the tens of thousands will be truncated in difxmessage.  Better warn the user.
		cout << "Filename " << v2dFile << " is too long - its job name might be truncated by difxmessage!" << endl;
		cout << "You are strongly suggested to choose a shorter .v2d name (root shorter than 26 characters)" << endl;
	}

	if(v2dFile.empty())
	{
		cerr << "Error: configuration (.v2d) file expected." << endl;
		cerr << "Run with -h for help information." << endl;
		
		exit(EXIT_FAILURE);
	}

	if(v2dFile.find("_") != string::npos)
	{
		cerr << "Error: you cannot have an underscore (_) in the filename!" << endl;
		cerr << "Please rename it and run again." << endl;
		
		exit(EXIT_FAILURE);
	}

	if(!isalpha(v2dFile[0]))
	{
		cerr << "Error: pass name (.v2d file name) must start with a letter!" << endl;
		cerr << "Please rename it and run again." << endl;
		
		exit(EXIT_FAILURE);
	}

	P = new CorrParams(v2dFile);
	if(P->vexFile.empty())
	{
		cerr << "Error: vex file parameter (vex) not found in file." << endl;
		
		exit(EXIT_FAILURE);
	}

	nWarn = P->parseWarnings;

	umask(02);


	// delete "no data" file before starting
	missingDataFile = v2dFile.substr(0, v2dFile.find_last_of('.'));
	missingDataFile += string(".nodata");
	command = "rm -f " + missingDataFile;
	system(command.c_str());

	V = loadVexFile(P->vexFile, &nWarn);

	if(!V)
	{
		cerr << "Error: cannot load vex file: " << P->vexFile << endl;
		
		exit(EXIT_FAILURE);
	}

	applyCorrParams(V, *P, nWarn, nError);
	
	V->generateEvents(events);
	V->addBreakEvents(events, P->manualBreaks);

	cout << "VEX structure:" << endl;
	cout << *V << endl;

	cout << "Warnings: " << nWarn << endl;
	cout << "Errors: " << nError << endl;

	return EXIT_SUCCESS;
}
