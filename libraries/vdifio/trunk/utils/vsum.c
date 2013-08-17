/***************************************************************************
 *   Copyright (C) 2013 by Walter Brisken                                  *
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vdifio.h>

const char program[] = "vsum";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20130817";

int main(int argc, char **argv)
{
	if(argc < 2)
	{
		printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
		printf("A utility to summarize the contents of VDIF data files\n\n");
		printf("Usage: %s <file1> [<file2> [ ... ] ]\n\n", argv[0]);
		printf("Where each file contains VDIF data\n\n");
	}
	else
	{
		struct vdif_file_summary sum;
		int a, r;

		for(a = 1; a < argc; ++a)
		{
			r = summarizevdiffile(&sum, argv[a], 0);

			if(r < 0)
			{
				printf("File %s VDIF summary failed with return value %d\n\n", argv[a], r);
			}
			else
			{
				printvdiffilesummary(&sum);
			}
		}
	}

	return 0;
}
