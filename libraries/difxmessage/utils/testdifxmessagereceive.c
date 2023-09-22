/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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
#include <time.h>
#include "difxmessage.h"

#define BINARY_OUTPUT_FILE	"binary.out"

const char program[] = "testdifxmessagereceive";
const char version[] = "1.1";
const char verdate[] = "20110409";
const char author[]  = "Walter Brisken";

const int MAX_MTU = 9000;

int usage(const char *cmd)
{
	int i;

	printf("\n%s ver. %s  %s %s\n\n", program, version, author, verdate);
	printf("Usage:  %s [options]  [type]\n\n", cmd);
	printf("Options can be:\n\n");
	printf("  -h or --help   : Print this help info\n\n");
	printf("  -b or --binary : Write binary records to file %s\n\n", BINARY_OUTPUT_FILE);
	printf("  -l or --length : Print lengths, not dots\n\n");
	printf("Type is a number from 1 to %d and refers to the following message types\n\n", NUM_DIFX_MESSAGE_TYPES-1);
	for(i = 1; i < NUM_DIFX_MESSAGE_TYPES; ++i)
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
	int i, l, v;
	int verbose = 1;
	int binary = 0;
	char from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];
	time_t t, lastt = 0;
	int np=0;
	char timestr[TimeLength];
	DifxMessageGeneric G;
	enum DifxMessageType;
	int onlyDots = 1;
	int headers = 0;
	int a;
	int specialMode = 0;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			return usage(argv[0]);
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		   strcmp(argv[a], "--verbose") == 0)
		{
			++verbose;
		}
		else if(strcmp(argv[a], "-B") == 0 ||
		   strcmp(argv[a], "--Binary") == 0)
		{
			binary+=2;
		}
		else if(strcmp(argv[a], "-l") == 0 ||
		   strcmp(argv[a], "--length") == 0)
		{
			onlyDots = 0;
			printf("Printing lengths, not dots\n");
		}
		else if(strcmp(argv[a], "-H") == 0)
		{
			headers = 1;
		}
		else if(strcmp(argv[a], "-b") == 0 ||
		   strcmp(argv[a], "--binary") == 0)
		{
			++binary;
		}
		else
		{
			int t = atoi(argv[a]);

			type = abs(t);
			if(type >= NUM_DIFX_MESSAGE_TYPES)
			{
				fprintf(stderr, "Illegal message type requested.  Run with -h for help\n");

				return 0;
			}
			printf("Listening only for message of type %d = %s\n", type, DifxMessageTypeStrings[type]);
			if(t < 0)
			{
				specialMode = 1;
			}
		}
	}

	if(binary)
	{
		if(binary >= 2)
		{
			printf("Warning: not writing a file, just printing dots\n");
		}

		char message[MAX_MTU];
		FILE *out = 0;

		difxMessageInitBinary();

		sock = difxMessageBinaryOpen(BINARY_STA);

		if(binary == 1)
		{
			out = fopen(BINARY_OUTPUT_FILE, "w");
		}

		for(;;)
		{
			l = difxMessageBinaryRecv(sock, message, MAX_MTU, from);
			time(&t);
			if(t != lastt)
			{
				lastt = t;
				printf("[%d]", np);
				np = 0;
				fflush(stdout);
			}
			if(l <= 0)
			{
				struct timespec ts;
				
				ts.tv_sec = 0;
				ts.tv_nsec = 100000000;
				nanosleep(&ts, 0);

				continue;
			}
			else
			{
				++np;
				if(out)
				{
					v = fwrite(message, 1, l, out);
					if(v != l)
					{
						fprintf(stderr, "Error: write to %s failed; disk full?\n", BINARY_OUTPUT_FILE);

						break;
					}
				}
				if(headers)
				{
					const int *data = (int *)message;
					printf("[%d %d %d %d %d %d %d %d %d %d l = %d]\n",
						data[0], data[1], data[2], data[3], data[4], 
						data[5], data[6], data[7], data[8], data[9], l);
				}
				else if(onlyDots)
				{
					printf(".");
				}
				else
				{
					printf("{%d}", l);
				}
				fflush(stdout);
			}
		}

		if(out)
		{
			fclose(out);
		}

		difxMessageBinaryClose(sock);
	}
	else
	{
		char message[DIFX_MESSAGE_LENGTH];

		difxMessageInit(-1, argv[0]);

		sock = difxMessageReceiveOpen();

		printf("\n");

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
			time(&t);
			v = snprintf(timestr, TimeLength, "%s", ctime(&t));
			if(v >= TimeLength)
			{
				fprintf(stderr, "Developer error: TimeLength=%d is too small (wants %d)\n", TimeLength, v);

				exit(0);
			}

			for(i = 0; timestr[i]; ++i)
			{
				if(timestr[i] < ' ')
				{
					timestr[i] = ' ';
				}
			}

			difxMessageParse(&G, message);
			if(type == DIFX_MESSAGE_UNKNOWN || type == G.type)
			{
				if(specialMode)
				{
					switch(type)
					{
					case DIFX_MESSAGE_DRIVE_STATS:
						printf("%s %d : %d : %8d %8d %8d %7d %6d %5d %5d %5d\n",
							G.body.driveStats.moduleVSN, G.body.driveStats.moduleSlot, G.body.driveStats.type,
							G.body.driveStats.bin[0],
							G.body.driveStats.bin[1],
							G.body.driveStats.bin[2],
							G.body.driveStats.bin[3],
							G.body.driveStats.bin[4],
							G.body.driveStats.bin[5],
							G.body.driveStats.bin[6],
							G.body.driveStats.bin[7]);
						break;
					default:
						printf("Message received\n");
						break;
					}
				}
				else
				{
					printf("[%s %s] <%d> ", timestr, from, l);
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
		}

		difxMessageReceiveClose(sock);

	}

	return EXIT_SUCCESS;
}
