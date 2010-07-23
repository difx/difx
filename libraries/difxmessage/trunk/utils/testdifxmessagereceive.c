/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken                             *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
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

const char program[] = "testdifxmessagereceive";
const char version[] = "1.0";
const char verdate[] = "20100717";
const char author[]  = "Walter Brisken";


int usage(const char *cmd)
{
	int i;

	printf("\n%s ver. %s  %s %s\n\n", program, version, author, verdate);
	printf("Usage:  %s [options]  [type]\n\n", cmd);
	printf("Options can be:\n\n");
	printf("  -h or --help   : Print this help info\n\n");
	printf("Type is a number from 1 to %d and refers to the following message types\n\n",
		NUM_DIFX_MESSAGE_TYPES-1);
	for(i = 1; i < NUM_DIFX_MESSAGE_TYPES; i++)
	{
		printf("  %2d : %s\n", i, DifxMessageTypeStrings[i]);
	}
	printf("\nIf no type is listed, all message types will be printed\n\n");

	return 0;
}

int main(int argc, char **argv)
{
	const int TimeLength = 32;
	int sock;
	enum DifxMessageType type = DIFX_MESSAGE_UNKNOWN;
	int i, l;
	int verbose = 1;
	char message[DIFX_MESSAGE_LENGTH], from[DIFX_MESSAGE_PARAM_LENGTH];
	time_t t;
	char timestr[TimeLength];
	DifxMessageGeneric G;
	enum DifxMessageType;

	if(argc > 1)
	{
		if(strcmp(argv[1], "-h") == 0 ||
		   strcmp(argv[1], "--help") == 0)
		{
			return usage(argv[0]);
		}
		else if(strcmp(argv[1], "-v") == 0 ||
		   strcmp(argv[1], "--verbose") == 0)
		{
			verbose++;
		}
		else
		{
			type = atoi(argv[1]);
			if(type <= 0 || type >= NUM_DIFX_MESSAGE_TYPES)
			{
				fprintf(stderr, "Illegal message type requested.  Run with -h for help\n");
				return 0;
			}
			printf("Listening only for message of type %d = %s\n", 
				type, DifxMessageTypeStrings[type]);
		}
	}

	difxMessageInit(-1, argv[0]);

	sock = difxMessageReceiveOpen();

	printf("\n");

	for(;;)
	{
		from[0] = 0;
		l = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
		if(l < 0)
		{
			usleep(100000);
			continue;
		}
		message[l] = 0;
		time(&t);
		l = snprintf(timestr, TimeLength, "%s", ctime(&t));
		if(l >= TimeLength)
		{
			fprintf(stderr, "Developer error: TimeLength=%d is too small (wants %d\n",
				TimeLength, l);
			exit(0);
		}

		for(i = 0; timestr[i]; i++)
		{
			if(timestr[i] < ' ')
			{
				timestr[i] = ' ';
			}
		}

		difxMessageParse(&G, message);
		if(type == DIFX_MESSAGE_UNKNOWN || type == G.type)
		{
			printf("[%s %s] ", timestr, from);
			difxMessageGenericPrint(&G);
			printf("\n");
			if(verbose)
			{
				printf("[%s %s] %s\n", timestr, from, message);
				printf("\n");
			}
			fflush(stdout);
		}
	}

	difxMessageReceiveClose(sock);

	return 0;
}
