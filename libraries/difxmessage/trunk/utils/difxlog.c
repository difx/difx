/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "difxmessage.h"

int usage(const char *program)
{
	printf("Usage: %s <identity> <outfile> [<logLevel> [<pidWatch>] ]\n", program);
	return 0;
}

int checkPid(int pid)
{
	char cmd[100];
	FILE *p;
	int r;
	char s[10];

	sprintf(cmd, "ps -p %d -o pid --no-headers", pid);
	p = popen(cmd, "r");
	r = (fgets(s, 9, p) > 3);
	fclose(p);

	return r;
}

int main(int argc, char **argv)
{
	char identifier[256];
	int logLevel = DIFX_ALERT_LEVEL_INFO;
	int pidWatch = -1;
	int sock;
	int l;
	char message[1024], from[32];
	DifxMessageGeneric G;
	FILE *out;
	time_t t, lastt;
	char timestr[32];

	time(&lastt);

	if(argc < 3)
	{
		return usage(argv[0]);
	}
	if(argc > 3)
	{
		logLevel = atoi(argv[3]);
	}
	if(argc > 4)
	{
		pidWatch = atoi(argv[4]);
	}

	strcpy(identifier, argv[1]);

	out = fopen(argv[2], "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", argv[2]);
		return -1;
	}

	difxMessageInit(-1, argv[0]);
	sock = difxMessageReceiveOpen();

	for(;;)
	{
		from[0] = 0;
		l = difxMessageReceive(sock, message, 1023, from);
		if(l < 0)
		{
			usleep(10000);
			continue;
		}
		message[l] = 0;
		difxMessageParse(&G, message);

		time(&t);
		strcpy(timestr, ctime(&t));
		timestr[strlen(timestr)-1] = 0;

		if(strcmp(G.identifier, identifier) == 0)
		{
			if(G.type == DIFX_MESSAGE_ALERT)
			{
				DifxMessageError *A;

				A = &G.body.error;
				if(A->severity <= logLevel)
				{
					fprintf(out, "%s  %s  %s\n", timestr, difxMessageAlertString[A->severity], A->message);
					fflush(out);
				}
			}
		}

		if(t - lastt > 2 && pidWatch > 0)
		{
			if(!checkPid(pidWatch))
			break;
		}

		lastt = t;
	}

	difxMessageReceiveClose(sock);

	return 0;
}
