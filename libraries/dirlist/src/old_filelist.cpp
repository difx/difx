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

#include <cstdio>
#include <cstring>
#include "old_filelist.h"
#include "utils.h"

int loadOldFileList(DirList &D, const char *fileName, std::stringstream &error)
{
	const int MaxLineLength = 255;
	FILE *in;
	char line[MaxLineLength+1];

	D.clear();
	D.setDefaultIdentifier();

	in = fopen(fileName, "r");
	if(!in)
	{
		error << "Cannot load file: " << fileName << "\n";

		return -1;
	}

	D.setParameter("class", "file", " Imported from filelist file");
	D.setParameter("version", 1);

	for(int s = 0; !feof(in); ++s)
	{
		DirListDatum*DD;
		char *v;
		bool ok;
		char dummy[257];

		v = fgetsNoCR(line, MaxLineLength, in);
		if(!v)
		{
			break;
		}

		for(int i = 0; line[i]; ++i)
		{
			if(line[i] == '#')
			{
				line[i] = 0;
				break;
			}
		}

		// check for line containing at least 3 items.  skip if not true
		if(sscanf(line, "%256s%256s%256s", dummy, dummy, dummy) != 3)
		{
			continue;
		}

		DD = new DirListDatum;
		D.addDatum(DD);
		ok = DD->setFromOldFileListString(line);
		if(!ok)
		{
			error << "Directory file: " << fileName << " is corrupt: at least one scan line is invalid (scan " << s+1 << ", '" << line << "').\n";
			fclose(in);

			return -1;
		}
	}
	fclose(in);

	D.organize();
	D.setPathPrefix();

	return 0;
}
