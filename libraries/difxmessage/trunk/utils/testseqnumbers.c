/***************************************************************************
 *   Copyright (C) 2009-2013 by Walter Brisken                             *
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
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include "difxmessage.h"

const char program[] = "testseqnumbers";
const char version[] = "1.1";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char verdate[] = "20130509";

const int MAX_SENDER=1024;

struct sender
{
	char name[DIFX_MESSAGE_PARAM_LENGTH];
	char identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	int lastseq;
};

void usage(const char *prog)
{
	printf("\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to listen for DiFX multicast messages and identify any that\n");
	printf("come with a sequence number that is not sequential.  This is a good\n");
	printf("way to identify possible packet loss or duplication on a DiFX cluster\n");
	printf("network.\n\n");
	printf("Usage: %s [options]\n\n", prog);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v          increase output verbosity\n\n");
	printf("--help\n");
	printf("-h          print help information and quite\n\n");
	printf("If run without the '-v' option, only unexpected packets will be noted.\n");
	printf("If run with one '-v' flag, each received packet will be identified with a\n");
	printf("period being written to the screen.  If run with 2 '-v' flags, each\n");
	printf("packet received will have its source and sequence number printed.\n\n");
}

void checkseq(const char *name, const char *identifier, int seq, struct sender *senders, int verbose)
{
	int i;
	time_t t;
	char timestr[32];

	if(seq < 0)
	{
		return;
	}

	for(i = 0; i < MAX_SENDER; ++i)
	{
		if(senders[i].lastseq < 0)
		{
			break;
		}
		if(strcmp(name, senders[i].name) == 0 &&
		   strcmp(identifier, senders[i].identifier) == 0)
		{
			if(seq != senders[i].lastseq + 1 && seq > 0)
			{
				time(&t);
				strcpy(timestr, ctime(&t));
				timestr[strlen(timestr)-1] = 0;
				printf("\n[%s]  %s.%s  %d -> %d", timestr, name, identifier, senders[i].lastseq, seq);
				fflush(stdout);
			}
			else if(verbose > 1)
			{
				printf("\n[%s] %d", name, seq);
				fflush(stdout);
			}
			else if(verbose > 0)
			{
				printf(".");
				fflush(stdout);
			}
			senders[i].lastseq = seq;

			return;
		}
	}
	if(i < MAX_SENDER)
	{
		strcpy(senders[i].name, name);
		strcpy(senders[i].identifier, identifier);
		senders[i].lastseq = seq;
	}
	if(i+1 < MAX_SENDER)
	{
		senders[i+1].lastseq = -1;
	}
}

int main(int argc, char **argv)
{
	int sock;
	int l;
	int verbose = 0;
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];
	DifxMessageGeneric G;
	struct sender senders[MAX_SENDER];
	int a;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			exit(EXIT_SUCCESS);
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		        strcmp(argv[a], "--verbose") == 0)
		{
			++verbose;
		}
		else
		{
			fprintf(stderr, "Error: unexpected command line argument '%s'\n\n", argv[a]);

			exit(EXIT_FAILURE);
		}

	}

	senders[0].lastseq = -1;

	difxMessageInit(-1, argv[0]);
	if(verbose > 0)
	{
		difxMessagePrint();
	}

	sock = difxMessageReceiveOpen();

	for(;;)
	{
		from[0] = 0;
		l = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
		if(l < 0)
		{
			struct timespec ts;
			
			ts.tv_sec = 0;
			ts.tv_nsec = 100000000;
			nanosleep(&ts, 0);

			continue;
		}
		message[l] = 0;
		difxMessageParse(&G, message);
	
		checkseq(G.from, G.identifier, G.seqNumber, senders, verbose);
	}

	difxMessageReceiveClose(sock);

	return EXIT_SUCCESS;
}
