/***************************************************************************
 *   Copyright (C) 2014-2024 by Walter Brisken                             *
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <vdifio.h>

const char program[] = "testcornerturners";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20131021";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to test internal corner turners\n\n");
	printf("Usage: %s\n\n", pgm);
	printf("\n");
}

int main(int argc, char **argv)
{
	int outputBytes = 102400;
	int nTest = 10000;

	if(argc > 1 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0))
	{
		usage(argv[0]);
		return EXIT_SUCCESS;
	}

	testvdifcornerturners(outputBytes, nTest);

	return EXIT_SUCCESS;
}
