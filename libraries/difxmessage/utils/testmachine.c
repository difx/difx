/***************************************************************************
 *   Copyright (C) 2010-2012 by Walter Brisken                             *
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
#include "testmachine.h"

/* return 0 on success */
int pingtest(const char *hostname)
{
	int MaxCommandLength = 100;
	int MaxMessageLength = 100;
	char cmd[MaxCommandLength];
	char msg[MaxMessageLength];
	FILE *p;

	snprintf(cmd, MaxCommandLength, "ping -c 1 -w 1 -q %s", hostname);

	p = popen(cmd, "r");

	while(!feof(p))
	{
		const char *v;

		v = fgets(msg, MaxMessageLength, p);
		if(!v)
		{
			break;
		}
	}
	pclose(p);

	if(strncmp(msg, "rtt", 3) == 0)
	{
		return 0;	/* Test passed */
	}

	return 1;
}
