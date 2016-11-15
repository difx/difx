/***************************************************************************
 *   Copyright (C) 2012-2016 by Walter Brisken                             *
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

#include <set>
#include <iostream>
#include <string>
#include <cstdio>
#include <cstdlib>
#include "vex_utility.h"

/* Function to look through a file to make sure it is not DOS formatted */
int checkCRLF(const char *fileName, bool verbose)
{
	static std::set<std::string> processedFiles;
	const int bufferSize = 1024;
	const char cr = 0x0d;
	FILE *in;
	char buffer[bufferSize];
	int n;

	if(processedFiles.find(fileName) == processedFiles.end())
	{
		if(verbose)
		{
			std::cout << "Checking " << fileName << " for proper line termination" << std::endl;
		}
		processedFiles.insert(fileName);

		in = fopen(fileName, "rb");
		if(!in)
		{
			std::cerr << "Error: cannot open " << fileName << std::endl;

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
					std::cerr << "Error: " << fileName << " appears to be in DOS format.  Please run dos2unix or equivalent and try again." << std::endl;

					fclose(in);

					return -1;
				}
			}
		}

		fclose(in);
	}

	return 0;
}

/* round to nearest second */
double roundSeconds(double mjd)
{
	int intmjd, intsec;

	intmjd = static_cast<int>(mjd);
	intsec = static_cast<int>((mjd - intmjd)*86400.0 + 0.5);

	return intmjd + intsec/86400.0;
}

/* check if an integer is a power of 2 */
bool isPowerOf2(int n)
{
	if(!(n & (n - 1))) 
	{
		return true;
	}

	return false; // also true for zero but this shouldn't concern us
}

// round up to the next power of two
// There must be a more elegant solution!
int nextPowerOf2(int x)
{
	int n=0; 
	int m=0;
	
	for(int i=0; i < 31; ++i)
	{
		if(x & (1 << i))
		{
			++n;
			m = i;
		}
	}

	if(n < 2)
	{
		return x;
	}
	else
	{
		return 2<<m;
	}
}

/* Modified from http://www-graphics.stanford.edu/~seander/bithacks.html */
int intlog2(unsigned int v)
{
	const unsigned int b[] = {0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000};
	const unsigned int S[] = {1, 2, 4, 8, 16};
	unsigned int r = 0; // result of log2(v) will go here

	for(int i = 4; i >= 0; --i) 
	{
		if(v & b[i])
		{
			v >>= S[i];
			r |= S[i];
		} 
	}

	return r;
}

char swapPolarizationCode(char pol)
{
	switch(pol)
	{
	case 'R':
		return 'L';
	case 'L':
		return 'R';
	case 'X':
		return 'Y';
	case 'Y':
		return 'X';
	default:
		std::cerr << "Error: unknown polarization: " << pol << std::endl;

		exit(EXIT_FAILURE);
	}
}

