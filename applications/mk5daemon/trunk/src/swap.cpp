/***************************************************************************
 *   Copyright (C) 2016 by Walter Brisken                                  *
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
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include "swap.h"

/* Value returned into "usage": positive number means swap in use [kBytes] */
/* Return value: 0 = success, != 0 means error */
int getSwapUsage(int *usage)
{
	const int MaxLineLength=256;
	FILE *pin;
	char line[MaxLineLength+1];
	const char *c;

	*usage = 0;

	pin = popen("free", "r");
	if(!pin)
	{
		return -1;
	}

	/* read first line: column heads, and discard */
	for(int l = 0; l < 10; ++l)
	{
		char name[20];
		int data[6];
		int n;

		c = fgets(line, MaxLineLength, pin);
		if(c == 0)
		{
			pclose(pin);

			return -2;
		}
		n = sscanf(line, "%s%d%d%d%d%d%d", name, data+0, data+1, data+2, data+3, data+4, data+5);
		if(n > 3 && strcmp(name, "Mem:") == 0)
		{
			*usage -= data[2];
		}
		else if(n > 2 && strcmp(name, "Swap:") == 0)
		{
			*usage += data[1];
			
			break;	/* we got all the info we need */
		}
	}

	pclose(pin);

	return 0;	/* success */
}
