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
#include <cstdlib>
#include "old_dirlist.h"
#include "dirlist_datum_mark5.h"

// Imports data from a legacy (Mark5) .dir file
// returns an error code
int loadOldDirList(DirList &D, const char *fileName, std::stringstream &error)
{
	const int MaxLineLength = 255;
	FILE *in;
	char line[MaxLineLength+1];
	int i, j, nscans, n;
	char *v;
	char bankName[2];
	char dirLabel[MaxLineLength];
	char extra[5][20];
	int nNumber = 0;
	long long signature;

	D.clear();
	D.setDefaultIdentifier();

	in = fopen(fileName, "r");
	if(!in)
	{
		error << "Cannot load file: " << fileName << "\n";

		return -1;
	}

	v = fgets(line, MaxLineLength, in);
	if(!v)
	{
		error << "Directory file: " << fileName << " is corrupt.\n";
		fclose(in);

		return -1;
	}

	n = sscanf(line, "%8s %d %1s %Ld %19s %19s %19s %19s %19s",
		dirLabel, &nscans, bankName, &signature, extra[0], extra[1], extra[2], extra[3], extra[4]);
	if(n < 3)
	{
		error << "Directory file: " << fileName << " is corrupt.\n";
		fclose(in);

		return -1;
	}
	if(n == 3)
	{
		signature = ~0;
	}
	D.setParameter("class", "mark5", " Imported from .dir file");
	D.setParameter("version", 1);
	D.setParameter("vsn", dirLabel);
	D.setParameter("hash", signature);
	D.setParameter("realtime", "false");
	D.setParameter("bankname", bankName);

	for(j = 4; j < n; ++j)
	{
		if(strcmp(extra[j-4], "RT") == 0)
		{
			D.setParameter("realtime", "true");
		}
		else if(strcmp(extra[j-4], "Fast") == 0)
		{
			D.setParameter("fastdir", "true");
		}
		else if(strcmp(extra[j-4], "Synth") == 0)
		{
			D.setParameter("synthetic", "true");
		}
		else if(sscanf(extra[j-4], "%d", &i) == 1)
		{
			switch(nNumber)
			{
			case 0:
				D.setParameter("mark5DirVersion", i);
				break;
			case 1:
				D.setParameter("mark5DirSubversion", i);
				break;
			}
			++nNumber;
		}
	}

	if(nscans < 0)
	{
		error << "Directory file: " << fileName << " is corrupt (nscans < 0).\n";
		fclose(in);

		return -1;
	}

	for(int s = 0; !feof(in); ++s)
	{
		DirListDatumMark5 *DM5;
		char *v;
		bool ok;

		v = fgets(line, MaxLineLength, in);
		if(!v)
		{
			break;
		}

		// strip CR/LF chars from end
		for(unsigned int i = 0; line[i]; ++i)
		{
			if(line[i] < ' ')
			{
				line[i] = 0;

				break;
			}
		}
		
		DM5 = new DirListDatumMark5;
		D.addDatum(DM5);
		ok = DM5->setFromOldString(line);
		if(!ok)
		{
			error << "Directory file: " << fileName << " is corrupt: at least one scan line is invalid.\n";
			fclose(in);

			return -1;
		}
	}
	fclose(in);

	D.organize();

	return 0;
}

// looks in $MARK5_DIR_PATH, first for .dirlist, then .dir
int mark5LegacyLoad(DirList &D, const char *vsn, std::stringstream &error)
{
	const int MaxFileNameLength = 256;
	const char *mark5DirPath = getenv("MARK5_DIR_PATH");
	char fileName[MaxFileNameLength];
	std::stringstream err;

	if(mark5DirPath == 0)
	{
		error << "Cannot load directory for Mark5 VSN " << vsn << " because environment variable MARK5_DIR_PATH is not set.";

		return -1;
	}

	// first try loading .dirlist file
	try
	{
		snprintf(fileName, MaxFileNameLength, "%s/%s.dirlist", mark5DirPath, vsn);
		D.load(fileName);
	}
	catch(DirListException &e)
	{
		int rv;

		snprintf(fileName, MaxFileNameLength, "%s/%s.dir", mark5DirPath, vsn);
		rv = loadOldDirList(D, fileName, err);
		if(rv < 0)
		{
			// Here both methods failed.  Populate the error string.
			error << "Cannot load directory for Mark5 VSN " << vsn << ".  Attempt to load .dirlist file resulted in: '" << e.what() << "'.  Attempt to load .dir file resulted in: '" << err.str() << "'.  MARK5_DIR_PATH env. var. was set to '" << mark5DirPath << "'.";

			return -2;
		}
		else
		{
			return rv;
		}
	}

	return 0;
}
