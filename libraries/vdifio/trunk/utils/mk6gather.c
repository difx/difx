/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
#include <stdlib.h>
#include <string.h>
#include "vdifmark6.h"

int main(int argc, char **argv)
{
	const int GatherSize = 10000000;
	Mark6Gatherer *G;
	char *buf;
	FILE *out;
	int i;

	if(argc != 2 || strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)
	{
		printf("Usage: %s <fileset template>\n\n", argv[0]);
		printf("fileset template is a glob expression selecting all Mark 6 files.\n\n");

		exit(EXIT_FAILURE);
	}

	printf("Opening all files matching %s\n", argv[1]);
	G = openMark6GathererFromTemplate(argv[1]);

	seekMark6Gather(G, getMark6GathererFileSize(G)/2);

	out = fopen("gather.out", "w");

	printMark6Gatherer(G);

	buf = (char *)malloc(GatherSize);
	for(i = 0;; ++i)
	{
		int n;

		n = mark6Gather(G, buf, GatherSize);
		if(n <= 0)
		{
			break;
		}
		printf("%d  %d/%d\n", i, n, GatherSize);
		fwrite(buf, 1, n, out);
	}
	free(buf);

	fclose(out);

	closeMark6Gatherer(G);

	return 0;
}
