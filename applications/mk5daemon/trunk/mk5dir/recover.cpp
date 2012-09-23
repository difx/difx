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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include <xlrapi.h>
#include "config.h"
#include "mark5dir.h"
#include "watchdog.h"
#include "../config.h"

const char program[] = "recover";
const char author[]  = "Walter Brisken";
const char version[] = "0.3";
const char verdate[] = "20111123";

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program that attempts to recover a Mark5 module\n\n");
	printf("Usage: %s <options> <type> <bank>\n\n", pgm);
	printf("<options> can include:\n\n");
	printf("  --help\n");
	printf("  -h         Print help info and quit\n\n");
	printf("  --verbose\n");
	printf("  -v         Be more verbose in execution\n\n");
	printf("  --quiet\n");
	printf("  -q         Be less verbose in execution\n\n");
	printf("  --force\n");
	printf("  -f         Don't ask before continuing\n\n");
	printf("<type> should be 0, 1 or 2.  See below.\n\n");
	printf("<bank> should be either A or B.\n\n");
	printf("The three types of recovery that can be attempted are:\n");
	printf("  0. Fix directory if power failed during recording.\n");
	printf("  1. Allow access to data that might have been overwritten.\n");
	printf("  2. Unerase module.\n\n");
	printf("This program appears to be compiled for SDK version %d\n\n", SDKVERSION);
}

static int recoverModule(int type, int bank, int force)
{
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	S_BANKSTATUS bankStat;
	S_DIR dir;
	int go = 1;
	char label[XLR_LABEL_LENGTH+1];
	char str[10];
	char *rv;
	int moduleStatus = MODULE_STATUS_UNKNOWN;
	int v;
	char vsn[10];
	int totalCapacity;
	int rate;

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bankStat) );
	if(bankStat.State != STATE_READY)
	{
		fprintf(stderr, "Bank %c not ready\n", 'A'+bank);
		WATCHDOG( XLRClose(xlrDevice) );
		
		return -1;
	}
	if(!bankStat.Selected)
	{
		WATCHDOGTEST( XLRSelectBank(xlrDevice, bank) );
	}

	/* the following line is essential to work around an apparent streamstor bug */
	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );
	label[XLR_LABEL_LENGTH] = 0;

	v = parseModuleLabel(label, vsn, &totalCapacity, &rate, &moduleStatus);
	if(v >= 0)
	{
		printf("\nCurrent extended VSN is %s\n", label);
		if(moduleStatus > 0)
		{
			printf("Current disk module status is %s\n", moduleStatusName(moduleStatus));
		}
	}
	else
	{
		printf("\nNo valid VSN currently set on module\n");
	}

	printf("%lld bytes are apparently recorded on this module\n\n", dir.Length);

	if(!force)
	{
		do
		{
			printf("About to attempt recover=%d on this module.  Continue? [y/n]", type);
			rv = fgets(str, 9, stdin);
			if(rv == 0)
			{
				/* must be ^D or similar */

				go = 0;
			}
			if(str[0] == 'y')
			{
				go = 1;
			}
			else if(str[0] == 'n')
			{
				go = 0;
			}
			else
			{
				go = -1;
			}
		}
		while(go == -1);
	}

	if(go == 1)
	{
		switch(type)
		{
			case 0:
				WATCHDOG( xlrRC = XLRRecoverData(xlrDevice, SS_RECOVER_POWERFAIL) );
				break;
			case 1:
				WATCHDOG( xlrRC = XLRRecoverData(xlrDevice, SS_RECOVER_OVERWRITE) );
				break;
			case 2:
				WATCHDOG( xlrRC = XLRRecoverData(xlrDevice, SS_RECOVER_UNERASE) );
				break;
			default:
				xlrRC = XLR_FAIL;
				fprintf(stderr, "Developer error: type = %d\n", type);
		}

		if(xlrRC == XLR_SUCCESS)
		{
			int dmsMask = 7;
			const char *dmsMaskStr;

			printf("Recovery appears to be successful!\n");
			
			dmsMaskStr = getenv("DEFAULT_DMS_MASK");
			if(dmsMaskStr)
			{
				dmsMask = atoi(dmsMaskStr);
			}
			if(dmsMask & MODULE_STATUS_RECORDED)
			{
				resetModuleDirectory(xlrDevice, vsn, MODULE_STATUS_RECORDED, 1, totalCapacity, rate);
			}
		}
		else
		{
			printf("Recovery appears to have failed.\n");
		}
	}
	else
	{
		printf("\nRecovery not attempted.\n");
	}

	WATCHDOG( XLRClose(xlrDevice) );

	return 0;
}

int main(int argc, char **argv)
{
	int a, v, i;
	int type = -99;
	int bank = -1;
	int verbose = 0;
	int force = 0;
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
			else if(strcmp(argv[a], "-f") == 0 ||
			   strcmp(argv[a], "--force") == 0)
			{
				++force;
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
		else if(type < 0)
		{
			i = sscanf(argv[a], "%d", &type);
			if(i < 0 || type < 0 || type > 2)
			{
				fprintf(stderr, "Error: type %s not recognized.  Want 0, 1, or 2\n",
					argv[a]);
				fprintf(stderr, "Run with -h for help info\n");
				
				return EXIT_FAILURE;
			}
		}
		else if(bank < 0)
		{
			if(strlen(argv[a]) != 1)
			{
				fprintf(stderr, "Error: expecting bank name, got %s\n", argv[a]);
				fprintf(stderr, "Run with -h for help info\n");
				
				return EXIT_FAILURE;
			}
			else if(argv[a][0] == 'A' || argv[a][0] == 'a')
			{
				bank = BANK_A;
			}
			else if(argv[a][0] == 'B' || argv[a][0] == 'b')
			{
				bank = BANK_B;
			}
			else
			{
				fprintf(stderr, "Error: expecting bank name, got %s\n", argv[a]);
				fprintf(stderr, "Run with -h for help info\n");
				
				return EXIT_FAILURE;
			}
		}
		else
		{
			fprintf(stderr, "Error: too many arguments given.\n");
			fprintf(stderr, "Run with -h for help info\n");

			return EXIT_FAILURE;
		}
	}

	if(bank < 0)
	{
		fprintf(stderr, "Error: incomplete command line\n");
		fprintf(stderr, "Run with -h for help info\n");
		
		return EXIT_FAILURE;
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
		v = recoverModule(type, bank, force);
		if(v < 0)
		{
			if(watchdogXLRError[0] != 0)
			{
				char message[DIFX_MESSAGE_LENGTH];
				snprintf(message, DIFX_MESSAGE_LENGTH, "Streamstor error executing: %s : %s", watchdogStatement, watchdogXLRError);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			}

			retval = EXIT_FAILURE;
		}
	}

	unlockMark5();

	/* *********** */

	stopWatchdog();

	return retval;
}
