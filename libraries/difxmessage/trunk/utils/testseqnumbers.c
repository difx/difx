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
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include "difxmessage.h"

const int MAX_SENDER=1024;

struct sender
{
	char name[DIFX_MESSAGE_PARAM_LENGTH];
	char identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	int lastseq;
};

void checkseq(const char *name, const char *identifier, int seq, struct sender *senders)
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
				printf("[%s]  %s.%s  %d -> %d\n", timestr, name, identifier, senders[i].lastseq, seq);
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
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];
	DifxMessageGeneric G;
	struct sender senders[MAX_SENDER];

	senders[0].lastseq = -1;

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

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
	
		checkseq(G.from, G.identifier, G.seqNumber, senders);
	}

	difxMessageReceiveClose(sock);

	return EXIT_SUCCESS;
}
