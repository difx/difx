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

#include <stdio.h>
#include <stdlib.h>

unsigned int calculateMark5DirSignature(const unsigned char *data, int size)
{
	unsigned int signature;
	int j;

	signature = 1;

	if(size > 0)
	{
		for(j = 0; j < size/4; ++j)
		{
			unsigned int x = ((unsigned int *)data)[j] + 1;
			signature = signature ^ x;
		}

		/* prevent a zero signature */
		if(signature == 0)
		{
			signature = 0x55555555;
		}
	}

	return signature;
}

int main(int argc, char **argv)
{
	const int MaxSize = 10000000;
	unsigned char *data;
	unsigned int size;
	FILE *in;

	if(argc != 2)
	{
		fprintf(stderr, "Need filename\n");

		return EXIT_FAILURE;
	}

	in = fopen(argv[1], "r");
	if(!in)
	{
		fprintf(stderr, "File not found\n");
	}

	data = (unsigned char *)malloc(MaxSize);

	size = fread(data, 1, MaxSize, in);
	
	printf("Dir size = %u\n", size);
	printf("Sig = %u\n", calculateMark5DirSignature(data+128, size-128));

	free(data);
	fclose(in);

	return EXIT_SUCCESS;
}
