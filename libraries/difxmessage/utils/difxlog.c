/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken                             *
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
#include <time.h>
#include "difxmessage.h"

const char program[] = "difxlog";
const char version[] = "0.6";
const char verdate[] = "20111227";
const char author[]  = "Walter Brisken";

void usage(const char *prog)
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
}

/* There _must_ be a better way to do this test for existence of running PID! */
int checkPid(int pid)
{
	const int CommandLength = 128;
	const int StrLength = 16;
	char cmd[CommandLength];
	const char *c;
	FILE *p;
	char s[StrLength];
	int v;

	v = snprintf(cmd, CommandLength, "ps -p %d -o pid --no-headers", pid);
	if(v >= CommandLength)
	{
		fprintf(stderr, "Developer error: checkPid: CommandLength=%d is too small (%d wanted)\n",
			CommandLength, v);
		exit(0);
	}

	p = popen(cmd, "r");
	c = fgets(s, StrLength-1, p);
	pclose(p);
	if(c == 0)
	{
		return 0;
	}
	if(*c <= 0)
	{
		return 0;
	}

	return 1;
}

int main(int argc, char **argv)
{
	const int TimeLength = 32;
	const int TagLength = 64+DIFX_MESSAGE_PARAM_LENGTH;
	char identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	int logLevel = DIFX_ALERT_LEVEL_INFO;
	int pidWatch = -1;
	int sock;
	int i, l;
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];
	DifxMessageGeneric G;
	FILE *out;
	time_t t, lastt;
	char timestr[TimeLength];
	char tag[TagLength];
        char *wgtfmt = getenv("DIFX_WEIGHTS_FORMAT");

        /* fall back to default short format */
        if (!wgtfmt || wgtfmt[0] != '%') wgtfmt = "%5.2f";

	time(&t);
	lastt = t;

	if(argc < 3)
	{
		usage(argv[0]);

		return EXIT_SUCCESS;
	}
	if(argc > 3)
	{
		logLevel = atoi(argv[3]);
	}
	if(argc > 4)
	{
		pidWatch = atoi(argv[4]);
	}

	l = snprintf(identifier, DIFX_MESSAGE_IDENTIFIER_LENGTH, "%s", argv[1]);
	if(l >= DIFX_MESSAGE_IDENTIFIER_LENGTH)
	{
		fprintf(stderr, "Error: Identifier '%s' exceeds %d character limit\n", argv[1], DIFX_MESSAGE_IDENTIFIER_LENGTH-1);

		return EXIT_FAILURE;
	}

	out = fopen(argv[2], "a");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", argv[2]);

		return EXIT_FAILURE;
	}

	difxMessageInit(-1, argv[0]);
	sock = difxMessageReceiveOpen();

	for(;;)
	{
		if(t - lastt > 2 && pidWatch > 0)
		{
			lastt = t;
			
			if(!checkPid(pidWatch))
			{
				break;
			}
		}

		from[0] = 0;
		l = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
		time(&t);
		if(l <= 0)
		{
			struct timespec ts;
			
			ts.tv_sec = 0;
			ts.tv_nsec = 100000000;
			nanosleep(&ts, 0);

			continue;
		}
		message[l] = 0;
		difxMessageParse(&G, message);

		strcpy(timestr, ctime(&t));
		timestr[strlen(timestr)-1] = 0;

		if(strcmp(G.identifier, identifier) == 0)
		{
			l = snprintf(tag, TagLength, "%s %3d %s", timestr, G.mpiId, G.from);
			if(l >= TagLength)
			{
				fprintf(stderr, "Developer error: TagLength is too small for timestr='%s', mpiIf=%d, from='%s'\n", timestr, G.mpiId, G.from);
			}
			if(G.type == DIFX_MESSAGE_ALERT)
			{
				const DifxMessageAlert *A;

				A = &G.body.alert;
				if(A->severity <= logLevel)
				{
					fprintf(out, "%s %s  %s\n", tag, difxMessageAlertString[A->severity], A->message);
					fflush(out);
				}
			}
			else if(G.type == DIFX_MESSAGE_STATUS)
			{
				const DifxMessageStatus *S;

				S = &G.body.status;
				
				fprintf(out, "%s  STATUS %s  %s\n", tag, DifxStateStrings[S->state], S->message);
				if(S->maxDS >= 0)
				{
					fprintf(out, "%s  WEIGHTS", tag);
					for(i = 0; i <= S->maxDS; ++i)
					{
						fprintf(out, wgtfmt, S->weight[i]);
					}
					fprintf(out, "\n");
				}
			}
			else if(G.type == DIFX_MESSAGE_TRANSIENT)
			{
				const DifxMessageTransient *T;

				T = &G.body.transient;

				fprintf(out, "Transient event received: jobId=%s priority=%f startMJD=%14.8f stopMJD=%14.8f destDir=\"%s\" comment=\"%s\"\n",
					T->jobId, 
					T->priority,
					T->startMJD,
					T->stopMJD,
					T->destDir,
					T->comment);
				fflush(out);
			}
		}
	}

	fclose(out);

	difxMessageReceiveClose(sock);

	return EXIT_SUCCESS;
}
