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

const char program[] = "difxlog";
const char version[] = "0.2";
const char verdate[] = "20090427";
const char author[]  = "Walter Brisken";

int usage(const char *prog)
{
	printf("\n%s ver. %s  %s %s\n\n", program, version, author, verdate);
	printf("A program to collect multi-cast alert messages for a particular job and\nwrite them to a file.\n\n");
	printf("Usage: %s <identity> <outfile> [<logLevel> [<pidWatch>] ]\n\n", prog);
	printf("<identity> is the identifier for a job -- usually the job prefix.\nSpecifically, this is compared to the identity field of the DifxMessage\nthat is received.\n\n");
	printf("<outfile> is the name of the output file containing the log info.\n\n");
	printf("<logLevel> specifies how much data to collect [default is 4]. Messages\nwith severity less than or equal to this are saved.  See the list of severity\nlevels below.\n\n");
	printf("<pidWatch> specifies the pid of the mpifxcorr process to watch.  This\nprogram will quit automatically when this pid is no longer running.\n\n");
	printf("Alert severity levels are as follows:\n");
	printf("  %d = Fatal\n", DIFX_ALERT_LEVEL_FATAL);
	printf("  %d = Severe\n", DIFX_ALERT_LEVEL_SEVERE);
	printf("  %d = Error\n", DIFX_ALERT_LEVEL_ERROR);
	printf("  %d = Warning\n", DIFX_ALERT_LEVEL_WARNING);
	printf("  %d = Informative\n", DIFX_ALERT_LEVEL_INFO);
	printf("  %d = Verbose\n", DIFX_ALERT_LEVEL_VERBOSE);
	printf("  %d = Debug\n", DIFX_ALERT_LEVEL_DEBUG);
	printf("\n");

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
	r = (*fgets(s, 9, p) >= ' ');
	fclose(p);

	return r;
}

int main(int argc, char **argv)
{
	char identifier[256];
	int logLevel = DIFX_ALERT_LEVEL_INFO;
	int pidWatch = -1;
	int sock;
	int i, l;
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
				DifxMessageAlert *A;

				A = &G.body.alert;
				if(A->severity <= logLevel)
				{
					fprintf(out, "%s  %s  %s\n", timestr, difxMessageAlertString[A->severity], A->message);
					fflush(out);
				}
			}
			else if(G.type == DIFX_MESSAGE_STATUS)
			{
				DifxMessageStatus *S;

				S = &G.body.status;
				
				fprintf(out, "%s  STATUS %s  %s\n", timestr, DifxStateStrings[S->state],
					S->message);
				if(S->maxDS >= 0)
				{
					fprintf(out, "%s  WEIGHTS", timestr);
					for(i = 0; i <= S->maxDS; i++)
					{
						fprintf(out, " %4.2f", S->weight[i]);
					}
					fprintf(out, "\n");
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
