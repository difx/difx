/***************************************************************************
 *   Copyright (C) 2013 by Walter Brisken                                  *
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
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/time.h>
#include <mark5ipc.h>
#include <xlrapi.h>
#include "config.h"
#include "watchdog.h"
#include "mark5dir.h"
#include "../config.h"

const char program[] = "mk5test";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20131205";

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("Performs StreamStor self tests using the XLRSelfTest() function call\n\n");
	printf("Usage: %s [options]\n\n", pgm);
	printf("options can include:\n\n");
	printf("  --help\n");
	printf("  -h     print this help information and quit\n\n");
	printf("  --verbose\n");
	printf("  -v     be more verbose\n\n");

	printf("This program appears to be compiled for SDK version %d\n\n", SDKVERSION);
}


int main(int argc, char **argv)
{
	int a, v;
	int verbose = 0;
	int retval = EXIT_SUCCESS;

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-q") == 0 ||
			   strcmp(argv[a], "--quiet") == 0)
			{
				--verbose;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				return EXIT_SUCCESS;
			}
			else
			{
				fprintf(stderr, "Unknown option: %s\n", argv[a]);
				fprintf(stderr, "Run with -h for help info\n");
				
				return EXIT_FAILURE;
			}
		}
	}

	v = initWatchdog();
	if(v < 0)
	{
		return EXIT_FAILURE;
	}

	/* 60 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(60);

	setWatchdogVerbosity(verbose);

	/* *********** */

	v = lockMark5(MARK5_LOCK_DONT_WAIT);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		XLR_RETURN_CODE xlrRC;
		SSHANDLE xlrDevice;
		int i;

		printf("\nPlease note!!!  This program requires a module to be keyed on in\n");
		printf("bank A.  For XF2-based Mark5 units only drive buses 0-3 will be tested\n");
		printf("if no module is in bank B.  For Amazon-based Mark5 units all buses will\n");
		printf("always be probed.\n\n");

		if(verbose <= 0)
		{
			printf("Run with -v to see more information.\n\n");
		}

		printf("Opening Streamstor...\n");
		WATCHDOGTEST( XLROpen(1, &xlrDevice) );
		printf("  Done\n");

		printf("Testing PCI...\n");
		if(verbose > 0)
		{
			printf("According to the StreamStor manual, this test \"tests\n");
			printf("communications with PCI bus\".  This test may be useful\n");
			printf("in identifying a card that is not properly seated.\n");
		}
		WATCHDOG( xlrRC = XLRSelfTest( xlrDevice, XLR_BIST_PCI) );
		if(xlrRC == XLR_SUCCESS)
		{
			printf("  Success\n");
		}
		else
		{
			int xlrError;
			char msg[XLR_ERROR_LENGTH];

			xlrError = XLRGetLastError();
			XLRGetErrorMessage(msg, xlrError);
			printf("  Error: %s\n", msg);
			exit(EXIT_FAILURE);
		}

		printf("Testing buffer...\n");
		if(verbose > 0)
		{
			printf("According to the StreamStor manual, this test will \"write\n");
			printf("and then read all 512 MB of RAM - checks for errors.\"  This\n");
			printf("test is useful for identifying bad StreamStor memory.\n");
		}
		WATCHDOG( xlrRC = XLRSelfTest( xlrDevice, XLR_BIST_BUFFER) );
		if(xlrRC == XLR_SUCCESS)
		{
			printf("  Success\n");
		}
		else
		{
			int xlrError;
			char msg[XLR_ERROR_LENGTH];

			xlrError = XLRGetLastError();
			XLRGetErrorMessage(msg, xlrError);
			printf("  Error: %s\n", msg);
			exit(EXIT_FAILURE);
		}

		printf("Testing buses...\n");
		if(verbose > 0)
		{
			printf("According to the StreamStor manual, \"This will test all\n");
			printf("disks present on the specifed bus (i.e., master and slave\n");
			printf("if present).\"  Below each bus is to be tested separately.\n");
		}
		for(i = XLR_BIST_DISK0; i <= XLR_BIST_DISK7; ++i)
		{
			printf("  Bus %d (bank %c slots %d and %d): ", i-XLR_BIST_DISK0, (i < XLR_BIST_DISK4 ? 'A' : 'B'), 2*((i-XLR_BIST_DISK0)%4), 2*((i-XLR_BIST_DISK0)%4)+1);

			WATCHDOG( xlrRC = XLRSelfTest( xlrDevice, (SS_SELFTEST)i) );
			if(xlrRC == XLR_SUCCESS)
			{
				printf("Success: %d\n", i-XLR_BIST_DISK0);
			}
			else
			{
				int xlrError;
				char msg[XLR_ERROR_LENGTH];

				xlrError = XLRGetLastError();
				if(xlrError == 7)
				{
					printf("Unpopulated: %d\n", i-XLR_BIST_DISK0);
				}
				else
				{
					XLRGetErrorMessage(msg, xlrError);
					printf("  Bus %d Error %d: %s\n", i-XLR_BIST_DISK0, xlrError, msg);
					exit(EXIT_FAILURE);
				}
			}
		}

		printf("Closing Streamstor...\n");
		WATCHDOG( XLRClose(xlrDevice) );
		printf("  Done\n");
		
		printf("Resetting Streamstor...\n");
		WATCHDOG( XLRCardReset(1) );
		printf("  Done\n");
		
		printf("\nNote: The next program to access the Streamstor program will require many\n");
		printf("seconds to open the device because of the reset.\n\n");
	}

	unlockMark5();

	/* *********** */

	stopWatchdog();

	return retval;
}
