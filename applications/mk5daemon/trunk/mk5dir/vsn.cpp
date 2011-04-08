/***************************************************************************
 *   Copyright (C) 2010-2011 by Walter Brisken                             *
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
#include <xlrapi.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "config.h"
#include "mark5dir.h"
#include "watchdog.h"
#include "../config.h"

const char program[] = "vsn";
const char author[]  = "Walter Brisken";
const char version[] = "0.2";
const char verdate[] = "20100912";

enum WriteProtectAction
{
	WPA_NONE	= 0,
	WPA_SET,
	WPA_CLEAR
};

int usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to set/get a Mark5 module Volume Serial Number (VSN)\n\n");
	printf("Usage: %s <options> <bank> [<vsn>]\n\n", pgm);
	printf("<options> can include:\n\n");
	printf("  --help\n");
	printf("  -h         Print help info and quit\n\n");
	printf("  --verbose\n");
	printf("  -v         Be more verbose in execution\n\n");
	printf("  --force\n");
	printf("  -f         Don't ask before continuing\n\n");
	printf("  --played\n");
	printf("  -p         Set module state to 'Played'\n\n");
	printf("  --recorded\n");
	printf("  -r         Set module state to 'Recorded'\n\n");
	printf("  --erased\n");
	printf("  -e         Set module state to 'Erased'\n\n");
	printf("  --writeprotect\n");
	printf("  -w         Write protect the module\n\n");
	printf("  --unwriteprotect\n");
	printf("  -u         Unprotect the module against writing\n\n");
	printf("<bank> should be either A or B.\n\n");
	printf("<vsn> is the new module VSN (must be 8 characters long).\n");
	printf("  If not provided, the existing VSN will be returned.\n\n");
	printf("This program appears to be compiled for SDK version %d\n\n", SDKVERSION);

	return 0;
}

int roundsize(int s)
{
	long long a;

	a = (long long)s*512LL;
	a /= 1000000000;
	a = (a+2)/5;

	return a*5;
}

int setvsn(int bank, char *newVSN, int newStatus, enum WriteProtectAction wpa, int force, int verbose)
{
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	S_BANKSTATUS bankStat;
	S_DIR dir;
	char label[XLR_LABEL_LENGTH+1];
	char oldLabel[XLR_LABEL_LENGTH+1];
	int dirVersion = -1;
	int nDrive, capacity, rate;
	int d;
	char oldVSN[10];
	char resp[12]="";
	char *rv;
	int v;
	int moduleStatus = MODULE_STATUS_UNKNOWN;
	int dirLength;
	struct DriveInformation drive[8];
	bool needsNewDir = false;

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

	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_DRVSTATS) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );
	label[XLR_LABEL_LENGTH] = 0;
	strcpy(oldLabel, label);

	if(verbose)
	{
		printf("Raw label = %s\n", label);
	}

	WATCHDOG( dirLength = XLRGetUserDirLength(xlrDevice) );

	if(dirLength % 128 != 0 || dirLength == 0)
	{
		if(dirLength == 0)
		{
			fprintf(stderr, "Warning: there is no directory on this module.\n");
			if(moduleStatus == MODULE_STATUS_UNKNOWN)
			{
				dirVersion = 1;
				moduleStatus = MODULE_STATUS_ERASED;
			}
			else
			{
				dirVersion = 0;
				needsNewDir = true;
				if(verbose)
				{
					fprintf(stderr, "Will have new directory set if a change is made.\n");
				}
			}
		}
		else
		{
			parseModuleLabel(label, 0, 0, 0, &moduleStatus);
			dirVersion = 0;
		}
	}
	else
	{
		v = getModuleDirectoryVersion(xlrDevice, &dirVersion, 0, &moduleStatus);
		if(v < 0)
		{
			return v;
		}
	}

	if(isLegalModuleLabel(oldLabel))
	{
		char vsn[10];

		parseModuleLabel(label, vsn, &capacity, &rate, 0);

		printf("\nCurrent extended VSN is %s/%d/%d\n", vsn, capacity, rate);
		printf("Current disk module status is %s\n", moduleStatusName(moduleStatus) );
		printf("\nModule directory version is %d\n", dirVersion);
		printf("This module contains %lld bytes of recorded data and is %4.2f%% full.\n", dir.Length,
			100.0*roundModuleSize(dir.Length)/capacity);
		printf("Write protection is %s.", bankStat.WriteProtected ? "ON" : "OFF");
	}
	else
	{
		printf("\nNo VSN currently set on module\n");
	}

	printf("\n");

	nDrive = getDriveInformation(xlrDevice, drive, &capacity);

	printf("This module consists of %d drives totalling about %d GB:\n",
		nDrive, capacity);
	for(d = 0; d < 8; d++)
	{
		if(drive[d].model[0] == 0)
		{
			continue;
		}
		printf("Disk %d info : %s : %s : %s : %d : %s\n",
			d, drive[d].model, drive[d].serial, drive[d].rev,
			roundModuleSize(drive[d].capacity),
			drive[d].failed ? "FAILED" : "OK");
	}

	if(newVSN[0] || newStatus)
	{
		printf("\n");

		if(newVSN[0] == 0)
		{
			strncpy(newVSN, oldLabel, 8);
			newVSN[8] = 0;
		}
		else
		{
			if(isLegalModuleLabel(oldLabel))
			{
				strncpy(oldVSN, oldLabel, 8);
				oldVSN[8] = 0;
				printf("About to change the module VSN from %s to %s\n", oldVSN, newVSN);
			}
			else
			{
				printf("About to set the module VSN to %s\n", newVSN);
			}
		}

		if(newStatus != 0)
		{
			moduleStatus = newStatus;
			printf("About to change status to %s\n", moduleStatusName(moduleStatus) );
		}

		if(force == 0)
		{
			printf("Is this OK? [y|n]\n");
			rv = fgets(resp, 10, stdin);
		}
		if(force || resp[0] == 'Y' || resp[0] == 'y')
		{
			WATCHDOG( xlrRC = XLRClearWriteProtect(xlrDevice) );
			if(xlrRC != XLR_SUCCESS)
			{
				printf("Need to do full erase; final directory format will be legacy.\n");
				printf("This is probably due to inserting an SDK9 module into an SDK8 unit.\n");
				printf("Is it OK to proceed? [y|n]\n");
				rv = fgets(resp, 10, stdin);
				if(resp[0] != 'Y' && resp[0] != 'y')
				{
					return -1;
				}
				WATCHDOGTEST( XLRErase(xlrDevice, SS_OVERWRITE_NONE) );
				dirVersion = 0;
				newStatus = MODULE_STATUS_ERASED;
				moduleStatus = newStatus;

				WATCHDOGTEST( XLRClearWriteProtect(xlrDevice) );

				needsNewDir = true;
			}

			if(dirVersion > 0)
			{
				rate = nDrive*256;
			}
			else
			{
				rate = nDrive*128;
			}

			v = setModuleLabel(xlrDevice, newVSN, moduleStatus, dirVersion, capacity, rate);
			if(v < 0)
			{
				return v;
			}

			if(dirVersion == 0)
			{
				if(needsNewDir)
				{
					v = resetModuleDirectory(xlrDevice, newVSN, moduleStatus, dirVersion, capacity, rate);
					if(v < 0)
					{
						return v;
					}
				}
			}
			else
			{
				setDiscModuleVSNNew(xlrDevice, moduleStatus, newVSN, capacity, rate);
			}

			printf("New extended VSN is %s/%d/%d\n", newVSN, capacity, rate);
			printf("New disk module status is %s\n", moduleStatusName(moduleStatus));
		}
		else
		{
			printf("\nNot changing VSN.\n");
		}
	}
	else
	{
		printf("\nNot changing VSN.\n");
	}

	if(wpa != WPA_NONE)
	{
		printf("\nAbout to %s write protection on this module.\n", wpa == WPA_SET ? "SET" : "CLEAR");

		if(force == 0)
		{
			printf("Is this OK? [y|n]\n");
			rv = fgets(resp, 10, stdin);
		}
		if(force || resp[0] == 'Y' || resp[0] == 'y')
		{
			switch(wpa)
			{
			case WPA_NONE:
				printf("Developer error: wpa == WPA_NONE\n");
				break;
			case WPA_SET:
				WATCHDOGTEST( XLRSetWriteProtect(xlrDevice) );
				break;
			case WPA_CLEAR:
				WATCHDOGTEST( XLRClearWriteProtect(xlrDevice) );
				break;
			}
		}
		
	}

	WATCHDOG( XLRClose(xlrDevice) );

	return 0;
}

int main(int argc, char **argv)
{
	int a, v, i;
	char newVSN[10] = "";
	int bank = -1;
	int verbose = 0;
	int force = 0;
	int newStatus = 0;
	int lockWait = MARK5_LOCK_DONT_WAIT;
	enum WriteProtectAction wpa = WPA_NONE;

	for(a = 1; a < argc; a++)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[a], "-f") == 0 ||
			   strcmp(argv[a], "--force") == 0)
			{
				force++;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				return usage(argv[0]);
			}
			else if(strcmp(argv[a], "--wait-forever") == 0)
			{
				lockWait = MARK5_LOCK_WAIT_FOREVER;
			}
			else if(strcmp(argv[a], "-e") == 0 ||
			   strcmp(argv[a], "--erased") == 0)
			{
				if(newStatus != 0 && newStatus != MODULE_STATUS_ERASED)
				{
					fprintf(stderr, "Multiple new states provided!\n");

					return -1;
				}
				newStatus = MODULE_STATUS_ERASED;
			}
			else if(strcmp(argv[a], "-p") == 0 ||
			   strcmp(argv[a], "--played") == 0)
			{
				if(newStatus != 0 && newStatus != MODULE_STATUS_PLAYED)
				{
					fprintf(stderr, "Multiple new states provided!\n");

					return -1;
				}
				newStatus = MODULE_STATUS_PLAYED;
			}
			else if(strcmp(argv[a], "-r") == 0 ||
			   strcmp(argv[a], "--recorded") == 0)
			{
				if(newStatus != 0 && newStatus != MODULE_STATUS_RECORDED)
				{
					fprintf(stderr, "Multiple new states provided!\n");

					return -1;
				}
				newStatus = MODULE_STATUS_RECORDED;
			}
			else if(strcmp(argv[a], "-w") == 0 ||
			   strcmp(argv[a], "--writeprotect") == 0)
			{
				if(wpa == WPA_CLEAR)
				{
					fprintf(stderr, "Conflicting requests for write protect and unprotect!\n");

					return -1;
				}
				wpa = WPA_SET;
			}
			else if(strcmp(argv[a], "-u") == 0 ||
			   strcmp(argv[a], "--unwriteprotect") == 0)
			{
				if(wpa == WPA_SET)
				{
					fprintf(stderr, "Conflicting requests for write protect and unprotect!\n");

					return -1;
				}
				wpa = WPA_CLEAR;
			}
			else
			{
				fprintf(stderr, "Unknown option: %s\n", argv[a]);
				fprintf(stderr, "Run with -h for help info\n");

				return -1;
			}
		}
		else if(bank < 0)
		{
			if(strlen(argv[a]) != 1)
			{
				fprintf(stderr, "Error: expecting bank name, got %s\n", argv[a]);
				fprintf(stderr, "Run with -h for help info\n");

				return -1;
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

				return -1;
			}
		}
		else
		{
			if(strlen(argv[a]) != 8)
			{
				fprintf(stderr, "Error: VSN length must be 8 characters\n");
				fprintf(stderr, "Run with -h for help info\n");

				return 0;
			}
			strcpy(newVSN, argv[a]);
			newVSN[8] = 0;
			for(i = 0; i < 8; i++)
			{
				newVSN[i] = toupper(newVSN[i]);
			}
		}
	}

	if(bank < 0)
	{
		fprintf(stderr, "Error: no bank specified\n");
		fprintf(stderr, "Run with -h for help info\n");

		return -1;
	}

	v = initWatchdog();
	if(v < 0)
	{
		return 0;
	}

	/* 60 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(60);

	setWatchdogVerbosity(verbose);

	/* *********** */

	v = lockMark5(lockWait);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = setvsn(bank, newVSN, newStatus, wpa, force, verbose);
		if(v < 0)
		{
			if(watchdogXLRError[0] != 0)
			{
				char message[DIFX_MESSAGE_LENGTH];
				snprintf(message, DIFX_MESSAGE_LENGTH, 
					"Streamstor error executing: %s : %s",
					watchdogStatement, watchdogXLRError);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			}
		}
	}
	
	unlockMark5();

	/* *********** */

	stopWatchdog();

	return 0;
}
