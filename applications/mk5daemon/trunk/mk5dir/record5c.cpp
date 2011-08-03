/***************************************************************************
 *   Copyright (C) 2011 by Walter Brisken                                  *
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

const char program[] = "record5c";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20110802";

const int defaultPacketSize = 5008;
const int defaultPayloadOffset = 40;
const int defaultDataFrameOffset = 0;
const int defaultPSNMode = 0;
const int defaultPSNOffset = 0;
const int defaultMACFilterControl = 0;
const unsigned int defaultStreamstorChannel = 28;

typedef void (*sighandler_t)(int);
sighandler_t oldsiginthand;
int die = 0;

#define MAC_FLTR_CTRL		0x02
#define DATA_PAYLD_OFFSET 	0x03
#define DATA_FRAME_OFFSET	0x04
#define PSN_OFFSET		0x05
#define BYTE_LENGTH		0x06
#define FILL_PATTERN_ADDR	0x07
#define TOTAL_PACKETS		0x08
#define ETH_FILTER		0x0C
#define ETH_REJECT_PACKETS	0x11

/* Note that the text going to stdout is interpreted by other code, so change with caution */

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program that attempts to recover a Mark5 module\n\n");
	printf("Usage: %s [<options>] <bank> <scanname>\n\n", pgm);
	printf("<options> can include:\n\n");
	printf("  --help\n");
	printf("  -h         Print help info and quit\n\n");
	printf("  --verbose\n");
	printf("  -v         Be more verbose in execution\n\n");

	/* FIXME: options for packet */
}

void siginthand(int j)
{
	die = 1;
	signal(SIGINT, oldsiginthand);
}

static int record(int bank, const char *label, int packetSize, int payloadOffset, int dataFrameOffset, int psnOffset, int psnMode, macFilterControl, int verbose)
{
	unsigned int channel = defaultStreamstorChannel;
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
	long long ptr;	/* record pointer */

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bankStat) );
	if(bankStat.State != STATE_READY)
	{
		printf("Error 6 Bank %c not ready\n", 'A'+bank);
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

	v = parseModuleLabel(label, 0, 0, 0, &moduleStatus);
	if(v >= 0)
	{
		printf("VSN %c %s\n", bank+'A', label);
		if(moduleStatus > 0)
		{
			printf("Current disk module status is %s\n", moduleStatusName(moduleStatus));
		}
	}
	else
	{
		printf("Error 7 No VSN set\n");
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	printf("Used %Ld %Ld\n", dir.Length, 0LL);	/* FIXME: 0-> disk size */

	WATCHDOGTEST( XLRSetMode(xlrDevice, SS_MODE_SINGLE_CHANNEL) );
	WATCHDOGTEST( XLRClearChannels(xlrDevice) );
	WATCHDOGTEST( XLRBindInputChannel(xlrDevice, channel) );
	WATCHDOGTEST( XLRSelectChannel(xlrDevice, channel) );

	/* configure 10G input daughter board */
	WATCHDOGTEST( XLRWriteDB(xlrDevice, DATA_PAYLD_OFFSET, payloadOffset) );
	WATCHDOGTEST( XLRWriteDB(xlrDevice, DATA_FRAME_OFFSET, dataFrameOffset) );
	WATCHDOGTEST( XLRWriteDB(xlrDevice, BYTE_LENGTH, packetLength) );
	WATCHDOGTEST( XLRWriteDB(xlrDevice, PSN_OFFSET, psnOffset) );
	WATCHDOGTEST( XLRWriteDB(xlrDevice, MAC_FLTR_CTRL, macFilterControl) );

	printf("Record %s %Ld\n", label, ptr);
	WATCHDOGTEST( XLRRecord(xlrDevice, 0, 1) );

	for(int n = 1; !die; n++)
	{
		usleep(1000);
		
		if(n % 1000 == 0)
		{
			/* query position and print */
			printf("Pointer %Ld\n", ptr);
		}
	}

	printf("Stop %s %Ld\n", label, ptr);
	WATCHDOGTEST( XLRStop(xlrDevice) );

	WATCHDOGTEST( XLRSetMode(xlrDevice, SS_MODE_SINGLE_CHANNEL)
	WATCHDOGTEST( XLRClearChannels(xlrDevice) );
	WATCHDOGTEST( XLRSelectChannel(xlrDevice, 0) );
	WATCHDOGTEST( XLRBindOutputChannel(xlrDevice, 0) );

	/* Update directory */

	WATCHDOG( XLRClose(xlrDevice) );

	return 0;
}

int main(int argc, char **argv)
{
	int a, v, i;
	int bank = -1;
	int verbose = 0;
	int force = 0;
	int packetSize = defaultPacketSize;
	int dataFrameOffset = defaultDataFrameOffset;
	int payloadOffset = defaultPayloadOffset;
	int psnOffset = defaultPSNOffset;
	int psnMode = defaultPSNMode;
	int macFilterControl = defaultMACFilterControl;
	int retval = EXIT_SUCCESS;
	char label[MaxLabelLength] = "";

	for(a = 1; a < argc; a++)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				return EXIT_SUCCESS;
			}
			else
			{
				printf("Unknown option: %s\n", argv[a]);
				
				return EXIT_FAILURE;
			}
		}
		else if(bank < 0)
		{
			if(strlen(argv[a]) != 1)
			{
				printf("Error 1 bank parameter (%s) not understood.\n", argv[a]);
				
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
				printf("Error 1 bank parameter (%s) not understood.\n", argv[a]);
				
				return EXIT_FAILURE;
			}
		}
		else if(label[0] == 0)
		{
			i = snprintf(label, MaxLabelLength, "%s", argv[a]);
			if(i >= MaxLabelLength)
			{
				printf("Error: scan name too long (%d > %d)\n", i, MaxLabelLength-1);

				return EXIT_FAILURE;
			}
		}
		else
		{
			printf("Error 2 too many arguments given.\n");

			return EXIT_FAILURE;
		}
	}

	if(bank < 0)
	{
		printf("Error3 incomplete command line\n");
		
		return EXIT_FAILURE;
	}

	v = initWatchdog();
	if(v < 0)
	{
		printf("Error4 initWatchdog() failed.\n");

		return EXIT_FAILURE;
	}

	/* 60 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(60);

	setWatchdogVerbosity(verbose);

	/* *********** */

	v = lockMark5(3);

	oldsiginthand = signal(SIGINT, siginthand);

	if(v < 0)
	{
		printf("Error 5 Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = record(bank, label, packetLength, payloadOffset, dtaFrameOffset, psnOffset, psnMode, macFilterControl, verbose);
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

			retval = EXIT_FAILURE;
		}
	}

	unlockMark5();

	/* *********** */

	stopWatchdog();

	return retval;
}
