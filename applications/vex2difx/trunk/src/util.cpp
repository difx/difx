/***************************************************************************
 *   Copyright (C) 2012 by Walter Brisken                                  *
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
 * $Id$
 * $HeadURL: $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include "util.h"

/* Function to look through a file to make sure it is not DOS formatted */
int checkCRLF(const char *filename)
{
	const int bufferSize = 1024;
	const char cr = 0x0d;
	FILE *in;
	char buffer[bufferSize];
	int n;

	printf("Checking %s\n", filename);

	in = fopen(filename, "rb");
	if(!in)
	{
		fprintf(stderr, "Error: cannot open %s\n", filename);

		return -1;
	}

	for(;;)
	{
		n = fread(buffer, 1, bufferSize, in);
		if(n < 1)
		{
			break;
		}

		for(int i = 0; i < n; ++i)
		{
			if(buffer[i] == cr)
			{
				fprintf(stderr, "Error: %s appears to be in DOS format.  Please run dos2unix or equivalent and try again.\n", filename);

				fclose(in);

				return -1;
			}
		}
	}

	fclose(in);

	return 0;
}
