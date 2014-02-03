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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "mark5dir.h"
#include "watchdog.h"
#include "config.h"

const char program[] = "mk5putdir";
const char author[]  = "Walter Brisken";
const char version[] = "0.2";
const char verdate[] = "20140117";


SSHANDLE xlrDevice;

static void usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to replace the on-disk directory of a Mark5 module\n\n");
	fprintf(stderr, "Usage : %s [<options>] { <bank> | <vsn> } <scanFile> [<version>]\n\n", pgm);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             Print this help message\n\n");
	fprintf(stderr, "  --binary\n");
	fprintf(stderr, "  -b             Push in a binary version of the dir\n\n");
	fprintf(stderr, "<bank> is either A or B\n\n");
	fprintf(stderr, "<vsn> is a valid module VSN (8 characters)\n\n");
	fprintf(stderr, "<scanFile> is a text file as described below.\n\n");
	fprintf(stderr, "<version> is the new version to set (for binary only).\n\n");



	fprintf(stderr, "This program can be used to parse a text file containing Mark5 directory\n");
	fprintf(stderr, "information and write that data to the User Directory on the module.\n\n");

	fprintf(stderr, "The format of the file to be read is simple.  It is a text file with\n");
	fprintf(stderr, "one line per scan and several space-separated fields per line:\n\n");

	fprintf(stderr, "Field  Type    Value\n");
	fprintf(stderr, "1      String  Extended VSN\n");
	fprintf(stderr, "2      Integer Data type (see Mark5 memo 81; 8 or 9 for Mark5B)\n");
	fprintf(stderr, "3      Integer Scan number on module (this is ignored)\n");
	fprintf(stderr, "4      Integer Frame size (0 --> 10016)\n");
	fprintf(stderr, "5      String  Station code\n");
	fprintf(stderr, "6      String  Scan name\n");
	fprintf(stderr, "7      String  Experiment code, with epoch code\n");
	fprintf(stderr, "8      Integer Start byte number \n");
	fprintf(stderr, "9      Integer End byte number\n\n");

	fprintf(stderr, "This file format is equivalent to that produced in the .scan.log files\n");
	fprintf(stderr, "writted on NRAO Mark5C units.  In order to facilitate generation of\n");
	fprintf(stderr, "such files through use of grep, any text before the first colon in a\n");
	fprintf(stderr, "line is ignored.\n\n");

	fprintf(stderr, "The main use of this program is to recover an on-disk directory that was\n");
	fprintf(stderr, "overwritten.  In most cases this file can be made with a simple command\n");
	fprintf(stderr, "such as:\n\n");

	fprintf(stderr, "  zgrep NRAO+343  *.scan.log.gz > NRAO+343.scans\n\n");

	fprintf(stderr, "from the /export/home/mark5/vlbamon/logs directory on a Mark5C unit.\n");
	fprintf(stderr, "After doing this it is a good idea to make sure there isn't scan data\n");
	fprintf(stderr, "from a previous module cycle, typically many weeks or months in the past.\n");
}

static int getBankInfo(SSHANDLE xlrDevice, DifxMessageMk5Status* mk5status, char bank)
{
	S_BANKSTATUS bank_stat;

	if(bank == 'A' || bank == 'a' || bank == ' ')
	{
		WATCHDOGTEST( XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat) );
		if(bank_stat.Label[8] == '/')
		{
			strncpy(mk5status->vsnA, bank_stat.Label, 8);
			mk5status->vsnA[8] = 0;
		}
		else
		{
			mk5status->vsnA[0] = 0;
		}
	}
	if(bank == 'B' || bank == 'b' || bank == ' ')
	{
		WATCHDOGTEST( XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat) );
		if(bank_stat.Label[8] == '/')
		{
			strncpy(mk5status->vsnB, bank_stat.Label, 8);
			mk5status->vsnB[8] = 0;
		}
		else
		{
			mk5status->vsnB[0] = 0;
		}
	}

	return 0;
}


static int mk5putdir(SSHANDLE xlrDevice, const char *vsn, char *dirData, int dirDataLength)
{
	DifxMessageMk5Status mk5status;
	Mark5Module module;
	char bank=' ';
	int v;

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRSetFillData(xlrDevice, MARK5_FILL_PATTERN) );
	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );

	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	if(strcasecmp(vsn, "A") == 0 && mk5status.vsnA[0] != 0)
	{
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_A) );
		bank = 'A';
	}
	if(strcasecmp(vsn, "B") == 0 && mk5status.vsnB[0] != 0)
	{
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_B) );
		bank = 'B';
	}

	if(strlen(vsn) == 8)
	{
		if(strncmp(vsn, mk5status.vsnA, 8) == 0)
		{
			WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_A) );
			bank = 'A';
		}
		else if(strncmp(vsn, mk5status.vsnB, 8) == 0)
		{
			WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_B) );
			bank = 'B';
		}
	}

	if(bank == ' ')
	{
		printf("Cannot set bank for %s\n", vsn);

		return -2;
	}

	WATCHDOGTEST( XLRSetUserDir(xlrDevice, dirData, dirDataLength) );

	WATCHDOG( XLRClose(xlrDevice) );

	return 0;
}

int readBinaryDir(const char *filename, char **dirData, int *dirDataLength)
{
	struct stat st;
	FILE *in;
	int v;

	stat(filename, &st);
	*dirData = (char *)malloc(st.st_size);
	*dirDataLength = st.st_size;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Error opening %s\n", filename);
		free(dirData);
		*dirDataLength = 0;

		return -1;
	}

	v = fread(*dirData, 1, st.st_size, in);

	if(v != st.st_size)
	{
		fprintf(stderr, "Error reading %s\n", filename);
		fclose(in);
		free(dirData);
		*dirDataLength = 0;

		return -2;
	}

	fclose(in);

	return 0;
}

int readScanLog(const char *filename, char **dirData, int *dirDataLength, const char *dumpFile)
{
	const int MaxLineLength = 1000;
	FILE *in;
	char line[MaxLineLength+1];
	int n=0;
	int linenum = 0;
	struct Mark5DirectoryHeaderVer1 *H;
	struct Mark5DirectoryScanHeaderVer1 *D;
	char vsn[MODULE_EXTENDED_VSN_LENGTH+1];

	memset(vsn, 0, MODULE_EXTENDED_VSN_LENGTH);

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Error opening %s\n", filename);

		return -1;
	}

	*dirData = (char *)calloc(128, 10000);	// be optimistic that data will be good
	if(!dirData)
	{
		fprintf(stderr, "Error allocating space for dir\n");

		fclose(in);

		return -2;
	}

	H = (struct Mark5DirectoryHeaderVer1 *)(*dirData);

	for(;;)
	{
		long long start, stop;
		char stn[100], scan[100], experiment[100];
		int type;
		int framelen;

		char *rv;
		char *p;
		int v;

		rv = fgets(line, MaxLineLength, in);
		if(rv == 0)
		{
			break;
		}
		++linenum;

		/* look for colon */
		for(p = line; *p; ++p)
		{
			if(*p == ':')
			{
				break;
			}
		}

		// If no colon, assume the line starts with VSN
		if(*p == 0)
		{
			p = line;
		}

		++p;	// advance to beginning of module label, hopefully

		v = sscanf(p, "%s%d%*d%d%s%s%s%Ld%Ld", vsn, &type, &framelen, stn, scan, experiment, &start, &stop);
		if(v != 8)
		{
			printf("Warning: parsing of line %d failed\n", linenum);
		}

		if(framelen == 0)
		{
			framelen = 10016;	// for Mark5B
		}
		
		D = (struct Mark5DirectoryScanHeaderVer1 *)(*dirData + sizeof(struct Mark5DirectoryHeaderVer1) + n*(sizeof(struct Mark5DirectoryScanHeaderVer1) + sizeof(struct Mark5DirectoryLegacyBodyVer1)));

		D->typeNumber = type + ((n+1) << 8);	// this field contains both the data type and scan number
		D->frameLength = framelen;
		strcpy(D->station, stn);
		strcpy(D->scanName, scan);
		strcpy(D->expName, experiment);
		D->startByte = start;
		D->stopByte = stop;

		++n;

		if(H->vsn[0] == 0)
		{
			strcpy(H->vsn, vsn);
		}
		else
		{
			if(strcmp(H->vsn, vsn) != 0)
			{
				printf("Warning: change of VSN encountered!  %s != %s\n", vsn, H->vsn);
			}
		}
	}

	fclose(in);

	*dirDataLength = sizeof(struct Mark5DirectoryHeaderVer1) + n*(sizeof(struct Mark5DirectoryScanHeaderVer1) + sizeof(struct Mark5DirectoryLegacyBodyVer1));

	H->version = 1;
	H->status = MODULE_STATUS_PLAYED;

	if(dumpFile)
	{
		FILE *out;

		out = fopen(dumpFile, "w");
		fwrite(*dirData, *dirDataLength, 1, out);
		fclose(out);
	}

	return 0;
}

int main(int argc, char **argv)
{
	int v;
	char *dirData;
	int doBinary = 0;
	int dirDataLength;
	const char *inputFile;
	const char *vsn;
	int retval = EXIT_SUCCESS;

	if(argc < 3 || argc > 5)
	{
		usage(argv[0]);

		if(argc == 2 && (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0))
		{
			return EXIT_SUCCESS;
		}
		else
		{
			return EXIT_FAILURE;
		}
	}
	else if(argc == 3)
	{
		vsn = argv[1];
		inputFile = argv[2];
		v = readScanLog(inputFile, &dirData, &dirDataLength, "dump");
		if(v != 0)
		{
			return EXIT_FAILURE;
		}
	}
	else if(argc > 4 && (strcmp(argv[1], "--binary") == 0 || strcmp(argv[1], "-b") == 0))
	{
		doBinary = 1;
		vsn = argv[2];
		inputFile = argv[3];
		v = readBinaryDir(inputFile, &dirData, &dirDataLength);
		if(argc == 5)
		{
			((struct Mark5DirectoryHeaderVer1 *)dirData)->version = atoi(argv[4]);
			printf("Setting version number to %d\n", atoi(argv[4]));
		}
		if(v != 0)
		{
			return EXIT_FAILURE;
		}
	}
	else
	{
		return EXIT_FAILURE;
	}

	v = initWatchdog();
	if(v < 0)
	{
		free(dirData);

		return EXIT_FAILURE;
	}

	/* 60 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(60);

	//setWatchdogVerbosity(verbose);

	/* *********** */

	v = lockMark5(MARK5_LOCK_DONT_WAIT);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = mk5putdir(xlrDevice, vsn, dirData, dirDataLength);

		if(v < 0)
		{
			WATCHDOG( XLRClose(xlrDevice) );

			retval = EXIT_FAILURE;
		}
	}

	unlockMark5();

	/* *********** */

	stopWatchdog();

	free(dirData);

	return retval;
}
