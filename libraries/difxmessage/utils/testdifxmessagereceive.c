/***************************************************************************
 *   Copyright (C) 2008-2024 by Walter Brisken                             *
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
#include <time.h>
#include "difxmessage.h"
#include "../difxmessage/difxmessageinternal.h"

const char program[] = "testdifxmessagereceive";
const char version[] = "1.3";
const char verdate[] = "20240724";
const char author[]  = "Walter Brisken";

const int MAX_MTU = 9000;

const char binaryOutputFile[] = "testdifxmessagereceive.out";

int usage(const char *cmd)
{
	int i;

	printf("\n%s ver. %s  %s %s\n\n", program, version, author, verdate);
	printf("Usage:  %s [options]  [type]\n\n", cmd);
	printf("Options can be:\n\n");
	printf("  -h or --help   : Print this help info\n\n");
	printf("  -b or --binary : Write binary records to file %s\n", binaryOutputFile);
	printf("                   If included twice, just print statistics\n\n");
	printf("  -l or --length : Print lengths, not dots\n\n");
	printf("  -H or --headers: Print in hex the first 48 bytes\n\n");
	printf("Type is a number from 1 to %d and refers to the following message types\n\n", NUM_DIFX_MESSAGE_TYPES-1);
	for(i = 1; i < NUM_DIFX_MESSAGE_TYPES; ++i)
	{
		printf("  %2d : %s\n", i, DifxMessageTypeStrings[i]);
	}
	printf("\nIf no type is listed, all message types will be printed\n\n");
	printf("The following environment variables are used:\n");
	printf("  DIFX_MESSAGE_GROUP : the multicast group to listen to\n");
	printf("  DIFX_MESSAGE_PORT  : the multicast port to listen to\n");
	printf("  DIFX_BINARY_GROUP  : the multicast group for binary listen\n");
	printf("  DIFX_BINARY_PORT   : the multicast port for binary listen\n\n");

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
			binary += 2;
		}
		else if(strcmp(argv[a], "-l") == 0 ||
		   strcmp(argv[a], "--length") == 0)
		{
			onlyDots = 0;
			printf("Printing lengths, not dots\n");
		}
		else if(strcmp(argv[a], "-H") == 0 ||
		   strcmp(argv[a], "--headers") == 0)
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
			fprintf(stderr, "Warning: not writing a file, just printing dots\n");
		}

		char message[MAX_MTU];
		FILE *out = 0;

		difxMessageInitBinary();

		sock = difxMessageBinaryOpen(BINARY_STA);

		if(binary == 1)
		{
			out = fopen(binaryOutputFile, "w");
			if(!out)
			{
				fprintf(stderr, "Cannot open %s for write\n", binaryOutputFile);
				exit(0);
			}
			fprintf(stderr, "Writing data to %s\n", binaryOutputFile);
		}
		else if(binary == 3)
		{
			fprintf(stderr, "Writing binary data to stdout -- hopefully you are piping it somewhere useful!\n");
			out = stdout;
		}

		for(;;)
		{
			l = difxMessageBinaryRecv(sock, message, MAX_MTU, from);
			time(&t);
			if(t != lastt)
			{
				lastt = t;
				fprintf(stderr, "[%d]", np);
				np = 0;
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
						fprintf(stderr, "Error: write to %s failed; disk full?\n", binaryOutputFile);

						break;
					}
					else
					{
						fflush(out);
					}
				}
				if(headers)
				{
					int k, n;
					n = l;
					if(n > 48)
					{
						n = 48;
					}
					printf("[");
					for(k = 0; k < n; ++k)
					{
						printf("%02X", ((unsigned char *)(message))[k]);
						if(k % 4 == 3)
						{
							printf(" ");
						}
					}
					printf(" %d/%d]\n", n, l);
				}
				else if(onlyDots)
				{
					fprintf(stderr, ".");
				}
				else
				{
					fprintf(stderr, "{%d}", l);
				}
			}
		}

		if(binary == 1)
		{
			fclose(out);
		}

		difxMessageBinaryClose(sock);
	}
	else
	{
		char message[DIFX_MESSAGE_LENGTH];

		difxMessageInit(-1, argv[0]);

		fprintf(stderr, "\nReceiving from group=%s, port=%d", difxMessageGroup, difxMessagePort);

		sock = difxMessageReceiveOpen();
		if(sock < 0)
		{
			fprintf(stderr, "\n\nError opening multicast socket\n");
			exit(0);
		}
		printf("-> socket=%d\n", sock);

		fprintf(stderr, "\n");

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

			if(headers)
			{
				int k, n;
				n = l;
				if(n > 48)
				{
					n = 48;
				}
				printf("[");
				for(k = 0; k < n; ++k)
				{
					printf("%02X", ((unsigned char *)(message))[k]);
					if(k % 4 == 3)
					{
						printf(" ");
					}
				}
				printf(" %d/%d]\n", n, l);
			}
			else
			{
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
		}

		difxMessageReceiveClose(sock);
	}

	return EXIT_SUCCESS;
}
