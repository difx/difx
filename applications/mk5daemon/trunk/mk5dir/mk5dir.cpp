/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <difxmessage.h>
#include "config.h"
#include "mark5dir.h"

#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif

const char program[] = "mk5dir";
const char author[]  = "Walter Brisken";
const char version[] = "0.6";
const char verdate[] = "20090918";

int verbose = 0;
int die = 0;
SSHANDLE xlrDevice;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	if(verbose)
	{
		printf("Being killed\n");
	}
	die = 1;
	signal(SIGHUP, oldsiginthand);
}


int usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to extract Mark5 module directory information via XLR calls\n");
	printf("\nUsage : %s [<options>] { <bank> | <vsn> }\n\n", pgm);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --partial\n");
	printf("  -p             Read directory of module with missing disks\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("<bank> is either A or B\n\n");
	printf("<vsn> is a valid module VSN (8 characters)\n\n");
	printf("Environment variable MARK5_DIR_PATH should point to the location of\n");
	printf("the directory to be written.  The output filename will be:\n");
	printf("  $MARK5_DIR_PATH/<vsn>.dir\n\n");

	return 0;
}

static int getBankInfo(SSHANDLE xlrDevice, DifxMessageMk5Status * mk5status, char bank)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	char message[1000];

	if(bank == 'A' || bank == 'a' || bank == ' ')
	{
		xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
		if(xlrRC != XLR_SUCCESS)
		{
			sprintf(message, "Cannot get bank A status");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);
			
			return -1;
		}
		else if(bank_stat.Label[8] == '/')
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
		xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
		if(xlrRC != XLR_SUCCESS)
		{
			sprintf(message, "Cannot get bank B status");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);
			
			return -2;
		}
		else if(bank_stat.Label[8] == '/')
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

int dirCallback(int scan, int nscan, int status, void *data)
{
	static long long seconds=0;
	struct timeval t;
	char message[256];
	DifxMessageMk5Status *mk5status;

	mk5status = (DifxMessageMk5Status *)data;
	mk5status->scanNumber = scan;
	mk5status->position = nscan;
	sprintf(mk5status->scanName, "%s", Mark5DirDescription[status]);
	difxMessageSendMark5Status(mk5status);

	if(verbose)
	{
		printf("%d/%d %d\n", scan, nscan, status);
	}

	gettimeofday(&t, 0);
	if(seconds == 0)
	{
		seconds = t.tv_sec;
	}
	if(t.tv_sec - seconds > 10)
	{
		seconds = t.tv_sec;
		getBankInfo(xlrDevice, mk5status, mk5status->activeBank == 'B' ? 'A' : 'B');
	}

	if(!die) switch(status)
	{
	case MARK5_DIR_READ_ERROR:
		sprintf(message, "XLR read error in decoding of scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		break;
	case MARK5_DIR_DECODE_ERROR:
		sprintf(message, "Cannot decode scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		break;
	default:
		break;
	}

	return die;
}

int main(int argc, char **argv)
{
	int mjdnow;
	const char *mk5dirpath;
	struct Mark5Module module;
	XLR_RETURN_CODE xlrRC;
	DifxMessageMk5Status mk5status;
	int partialModule = 0;
	char message[1000];
	char modules[100] = "";
	char vsn[16] = "";
	int a, v;
	float replacedFrac;

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	for(a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			return usage(argv[0]);
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		        strcmp(argv[a], "--verbose") == 0)
		{
			verbose++;
		}
		else if(strcmp(argv[a], "-p") == 0 ||
			strcmp(argv[a], "--partial") == 0)
		{
			partialModule = 1;
		}
		else if(vsn[0] == 0)
		{
			strncpy(vsn, argv[a], 8);
			vsn[8] = 0;
		}
		else
		{
			return usage(argv[0]);
		}
	}

	difxMessageInit(-1, "mk5dir");

	sprintf(message, "%s ver %s starting", program, version);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);

	memset(&mk5status, 0, sizeof(mk5status));

	xlrRC = XLROpen(1, &xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot open XLR");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);

		return 0;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot set Skip Check Dir");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);

		XLRClose(xlrDevice);
		
		return 0;
	}

	xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot set Bank Mode");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);

		XLRClose(xlrDevice);
		
		return 0;
	}

	if(partialModule)
	{
		if(verbose)
		{
			difxMessageSendDifxAlert("Running in real-time playback mode", DIFX_ALERT_LEVEL_VERBOSE);
		}

		xlrRC = XLRSetOption(xlrDevice, SS_OPT_REALTIMEPLAYBACK);
		if(xlrRC != XLR_SUCCESS)
		{
			sprintf(message, "Cannot set Realtime Playback Mode");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			XLRClose(xlrDevice);
			
			return 0;
		}

		xlrRC = XLRSetFillData(xlrDevice, FILL_PATTERN);
		if(xlrRC != XLR_SUCCESS)
		{
			sprintf(message, "Cannot set Fill Pattern to %8lx", FILL_PATTERN);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			XLRClose(xlrDevice);
			
			return 0;
		}
	}

	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		XLRClose(xlrDevice);

		return -1;
	}

	if(strcasecmp(vsn, "A") == 0 && mk5status.vsnA[0] != 0)
	{
		mk5status.activeBank = 'A';
		strcpy(vsn, mk5status.vsnA);
	}
	if(strcasecmp(vsn, "B") == 0 && mk5status.vsnB[0] != 0)
	{
		mk5status.activeBank = 'B';
		strcpy(vsn, mk5status.vsnB);
	}

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = ".";
	}
	
	mjdnow = (int)(40587.0 + time(0)/86400.0);

	mk5status.state = MARK5_STATE_GETDIR;
	difxMessageSendMark5Status(&mk5status);

	memset(&module, 0, sizeof(module));
	module.bank = -1;

	oldsiginthand = signal(SIGHUP, siginthand);

	if(strcmp(vsn, "AB") == 0)
	{
		if(strlen(mk5status.vsnA) == 8)
		{
			mk5status.activeBank = 'A';
			v = getCachedMark5Module(&module, &xlrDevice, mjdnow, 
				mk5status.vsnA, mk5dirpath, &dirCallback, 
				&mk5status, &replacedFrac);
			if(replacedFrac > 0.01)
			{
				sprintf(message, "Module %s directory read encountered %4.2f%% data replacement rate",
					mk5status.vsnA, replacedFrac*100.0);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
			if(v < 0)
			{
				if(!die)
				{
					sprintf(message, "Directory read for module %s unsuccessful, error code=%d", mk5status.vsnA, v);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "Error: %s\n", message);
				}
			}
			else if(v == 0 && verbose > 0)
			{
				printMark5Module(&module);
			}
			if(v >= 0)
			{
				strcpy(modules, mk5status.vsnA);
			}

			if(sanityCheckModule(&module) < 0)
			{
				sprintf(message, "Module %s directory contains errors", mk5status.vsnA);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
			}
		}

		memset(&module, 0, sizeof(module));
		module.bank = -1;

		if(strlen(mk5status.vsnB) == 8)
		{
			mk5status.activeBank = 'B';
			v = getCachedMark5Module(&module, &xlrDevice, mjdnow, 
				mk5status.vsnB, mk5dirpath, &dirCallback, 
				&mk5status, &replacedFrac);
			if(replacedFrac > 0.01)
			{
				sprintf(message, "Module %s directory read encountered %4.2f%% data replacement rate",
					mk5status.vsnB, replacedFrac*100.0);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
			if(v < 0)
			{
				if(!die)
				{
					sprintf(message, "Directory read for module %s unsuccessful, error code=%d", mk5status.vsnB, v);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "Error: %s\n", message);
				}
			}
			else if(v == 0 && verbose > 0)
			{
				printMark5Module(&module);
			}
			if(v >= 0)
			{
				if(modules[0])
				{
					strcat(modules, " and ");
					strcat(modules, mk5status.vsnB);
				}
				else
				{
					strcpy(modules, mk5status.vsnB);
				}
			}

			if(sanityCheckModule(&module) < 0)
			{
				sprintf(message, "Module %s directory contains errors", mk5status.vsnB);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
			}
		}
	}
	else
	{
		if(strlen(vsn) == 8)
		{
			if(strncmp(vsn, mk5status.vsnA, 8) == 0)
			{
				mk5status.activeBank = 'A';
			}
			else if(strncmp(vsn, mk5status.vsnB, 8) == 0)
			{
				mk5status.activeBank = 'B';
			}

			v = getCachedMark5Module(&module, &xlrDevice, mjdnow, 
				vsn, mk5dirpath, &dirCallback, &mk5status,
				&replacedFrac);
			if(replacedFrac > 0.01)
			{
				sprintf(message, "Module %s directory read encountered %4.2f%% data replacement rate",
					vsn, replacedFrac*100.0);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
			if(v < 0)
			{
				if(!die)
				{
					sprintf(message, "Directory read for module %s unsuccessful, error code=%d", vsn, v);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "Error: %s\n", message);
				}
			}
			else if(verbose > 0)
			{
				printMark5Module(&module);
			}
			if(v >= 0)
			{
				strcpy(modules, vsn);
			}

			if(sanityCheckModule(&module) < 0)
			{
				sprintf(message, "Module %s directory contains errors", vsn);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
			}
		}

	}

	if(die)
	{
		difxMessageSendDifxAlert("Directory read terminated by signal.", DIFX_ALERT_LEVEL_WARNING);
	}
	else if(modules[0])
	{
		sprintf(message, "Successful directory read for %s", modules);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
	}

	XLRClose(xlrDevice);

	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = module.nscans;
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}
