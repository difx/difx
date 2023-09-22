/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "mark5dir.h"
#include "watchdog.h"
#include "config.h"

const char program[] = "mk5dir";
const char author[]  = "Walter Brisken";
const char version[] = "0.15";
const char verdate[] = "20170904";

enum DMS_Mode
{
	DMS_MODE_UPDATE = 0,
	DMS_MODE_NO_UPDATE,
	DMS_MODE_UPDATE_IF_SAFE,
	DMS_MODE_FAIL_UNLESS_SAFE
};

int verbose = 0;
volatile int die = 0;
SSHANDLE xlrDevice;

/* Note: must use the less appropriate signal() rather than sigaction() call 
 * because streamstor library seems to use signal() and mixing the two
 * is bad. */
sighandler_t oldsiginthand;
sighandler_t oldsigtermhand;

static void siginthand(int j)
{
	if(verbose)
	{
		printf("Being killed (INT)\n");
	}
	die = 1;
}

static void sigtermhand(int j)
{
	if(verbose)
	{
		printf("Being killed (TERM)\n");
	}
	die = 1;
}

static void printVersion()
{
	printf("%s ver. %s   %s %s\n", program, version, author, verdate);
}

static void usage(const char *pgm)
{
	printf("\n");
	printVersion();
	printf("\n");
	printf("A program to extract Mark5 module directory information via XLR calls\n");
	printf("\nUsage : %s [<options>] { <bank> | <vsn> }\n\n", pgm);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --version      Print version information and quit\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("  --quiet\n");
	printf("  -q             Be more verbose\n\n");
	printf("  --force\n");
	printf("  -f             Reread directory even if not needed\n\n");
	printf("  --fast\n");
	printf("  -F             Use fast mode (user dir version >=1 only)\n\n");
	printf("  --realtime\n");
	printf("  -r             Use Real-Time (RT) mode\n\n");
	printf("  --nodms\n");
	printf("  -n             Get directory but don't change disk module state\n\n");
	printf("  --safedms\n");
	printf("  -s             Only change disk module state if SDK8 unit or new dir type\n\n");
	printf("  --dmsforce\n");
	printf("  -d             Proceed with change of DMS even if this makes module unreadable in SDK8\n\n");
	printf("  --begin <b>\n");
	printf("  -b <b>         Begin with scan number <b> (a 1-based number)\n\n");  
	printf("  --end <e>\n");
	printf("  -e <e>         End with scan number <e> (a 1-based number)\n\n");  
	printf("  --write <file>\n");
	printf("  -w <file>      Write directory listing in <file> to module instead of reading\n\n");
	printf("  --version1\n");
	printf("  -1             Force (Mark5C) directory version to be 1\n\n");
	printf("<bank> is either A or B\n\n");
	printf("<vsn> is a valid module VSN (8 characters)\n\n");
	printf("Environment variable MARK5_DIR_PATH should point to the location of\n");
	printf("the directory to be written.  The output filename will be:\n");
	printf("  $MARK5_DIR_PATH/<vsn>.dir\n\n");
}

static int getBankInfo(SSHANDLE xlrDevice, DifxMessageMk5Status * mk5status, char bank)
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

int dirCallback(int scan, int nscan, int status, void *data)
{
	static long long seconds=0;
	struct timeval t;
	DifxMessageMk5Status *mk5status;

	mk5status = (DifxMessageMk5Status *)data;
	mk5status->scanNumber = scan;
	mk5status->position = nscan;
	snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s", Mark5DirDescription[status]);
	difxMessageSendMark5Status(mk5status);

	if(verbose)
	{
		printf("%d/%d -> %d %s\n", scan, nscan, status, Mark5DirDescription[status]);
	}

	gettimeofday(&t, 0);
	if(seconds == 0)
	{
		seconds = t.tv_sec;
	}
	if(t.tv_sec - seconds > 10)
	{
		int v;

		seconds = t.tv_sec;
		v = getBankInfo(xlrDevice, mk5status, mk5status->activeBank == 'B' ? 'A' : 'B');
		if(v < 0)
		{
			die = 1;
		}
	}

	return die;
}

static int getDirCore(struct Mark5Module *module, char *vsn, DifxMessageMk5Status *mk5status, int force, int fast, enum DMS_Mode dmsMode, int startScan, int stopScan, int forceVersion)
{
	int v;
	int mjdnow;
	const char *mk5dirpath;
	float replacedFrac;
	char message[DIFX_MESSAGE_LENGTH];
	int dmsUpdate = 0;
	int len;

	if(verbose > 1)
	{
		printf("getDirCore: vsn=%s force=%d fast=%d startScan=%d stopScan=%d forceVersion=%dd\n",
			vsn, force, fast, startScan, stopScan, forceVersion);
	}

	if(dmsMode == DMS_MODE_FAIL_UNLESS_SAFE)
	{
		WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );
		if(len % 128 != 0 && SDKVERSION >= 9)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Not reading the module directory because setting the DMS will render "
				"this module unreadable with SDK8.  Use the --dmsforce option to override.");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
		else
		{
			dmsMode = DMS_MODE_UPDATE;
		}
	}

	mjdnow = (int)(40587.0 + time(0)/86400.0);

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = ".";
	}

	v = module->getCachedDirectory(xlrDevice, mjdnow, vsn, mk5dirpath, &dirCallback, mk5status, &replacedFrac, force, fast, 0, startScan, stopScan, forceVersion);
	if(replacedFrac > 0.01)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Module %s directory read encountered %4.2f%% data replacement rate", vsn, replacedFrac);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
		fprintf(stderr, "Warning: %s\n", message);
	}
	if(v < 0)
	{
		if(!die)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Directory read for module %s unsuccessful, error code=%d", vsn, v);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);
		}
	}
	else if(v == 0 && verbose > 0)
	{
		module->print();
	}

	switch(dmsMode)
	{
	case DMS_MODE_NO_UPDATE:
		fprintf(stderr, "FYI: Not setting disk module state to Played for %s\n\n", vsn);
		dmsUpdate = 0;
		break;
	case DMS_MODE_UPDATE:
		dmsUpdate = 1;
		break;
	case DMS_MODE_UPDATE_IF_SAFE:
		if(SDKVERSION < 9 || module->dirVersion == 0)
		{
			dmsUpdate = 1;
		}
		else
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Not setting disk module state of %s to Played because "
				"this would make the module unreadable in SDK8 Mark5 units.", vsn);
			fprintf(stderr, "Warning: %s\n\n", message);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			dmsUpdate = 0;
		}
		break;
	case DMS_MODE_FAIL_UNLESS_SAFE:	
		if(SDKVERSION < 9 || module->dirVersion == 0)
		{
			dmsUpdate = 1;
		}
		else
		{
			/* should not get here */
			snprintf(message, DIFX_MESSAGE_LENGTH, "Developer error: DMS_MODE_FAIL_UNLESS_SAFE reached after directory read!  vsn = %s", vsn);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);
			dmsUpdate = 0;
		}
		break;
	}
	if(dmsUpdate)
	{
		if(module->dirVersion == 0)
		{
			setDiscModuleStateLegacy(xlrDevice, MODULE_STATUS_PLAYED);
		}
		else
		{
			setDiscModuleStateNew(xlrDevice, MODULE_STATUS_PLAYED);
		}
	}

	return v;
}

static int mk5dir(char *vsn, int force, int fast, enum DMS_Mode dmsMode, int startScan, int stopScan, int realTime, int forceVersion)
{
	Mark5Module module;
	DifxMessageMk5Status mk5status;
	char message[DIFX_MESSAGE_LENGTH];
	const int modulesLength = 100;
	char modules[modulesLength] = "";
	int mv = 0;
	int v;

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );

	if(realTime > 0)
	{
		printf("Setting real-time playback mode\n");
		WATCHDOGTEST( XLRSetFillData(xlrDevice, MARK5_FILL_PATTERN) );
		WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	}
	else
	{
		WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_REALTIMEPLAYBACK) );
		WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	}

	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	if(strcasecmp(vsn, "A") == 0 && mk5status.vsnA[0] != 0)
	{
		mk5status.activeBank = 'A';
		strcpy(vsn, mk5status.vsnA);
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_A) );
	}
	if(strcasecmp(vsn, "B") == 0 && mk5status.vsnB[0] != 0)
	{
		mk5status.activeBank = 'B';
		strcpy(vsn, mk5status.vsnB);
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_B) );
	}

	mk5status.state = MARK5_STATE_GETDIR;
	difxMessageSendMark5Status(&mk5status);

	oldsiginthand = signal(SIGINT, siginthand);
	oldsigtermhand = signal(SIGTERM, sigtermhand);

	if(strcmp(vsn, "AB") == 0)
	{
		if(strlen(mk5status.vsnA) == 8)
		{
			mk5status.activeBank = 'A';
			v = getDirCore(&module, mk5status.vsnA, &mk5status, force, fast, dmsMode, startScan, stopScan, forceVersion);
			if(v >= 0)
			{
				mv = snprintf(modules, modulesLength, "%s", mk5status.vsnA);
			}
		}
		
		module.clear();

		if(strlen(mk5status.vsnB) == 8)
		{
			mk5status.activeBank = 'B';
			v = getDirCore(&module, mk5status.vsnB, &mk5status, force, fast, dmsMode, startScan, stopScan, forceVersion);
			if(v >= 0)
			{
				if(modules[0])
				{
					mv = snprintf(modules, modulesLength, "%s and %s", mk5status.vsnA, mk5status.vsnB);
				}
				else
				{
					mv = snprintf(modules, modulesLength, "%s", mk5status.vsnB);
				}
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

			v = getDirCore(&module, vsn, &mk5status, force, fast, dmsMode, startScan, stopScan, forceVersion);
			if(v >= 0)
			{
				mv = snprintf(modules, modulesLength, "%s", vsn);
			}
		}
	}

	if(mv > modulesLength)
	{
		fprintf(stderr, "Developer error: mk5dir: modulesLength is too small (%d < %d)\n", modulesLength, mv+1);
	}

	if(die)
	{
		difxMessageSendDifxAlert("Directory read terminated by signal.", DIFX_ALERT_LEVEL_WARNING);
	}
	else if(modules[0])
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Successful directory read for %s", modules);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
	}

	WATCHDOG( XLRClose(xlrDevice) );

	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = module.nScans();
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

static int writeDir(char *vsn, int force, const char *filename, enum DMS_Mode dmsMode, int realTime)
{
	Mark5Module module;
	DifxMessageMk5Status mk5status;
	char message[DIFX_MESSAGE_LENGTH];
	int dmsUpdate = 0;
	int v;

	if(dmsMode == DMS_MODE_FAIL_UNLESS_SAFE)
	{
		printf("Not proceeding since dmsMode is DMS_MODE_FAIL_UNLESS_SAFE\n");
		printf("and the safety of the operation cannot be determined.\n\n");

		return -1;
	}

	v = module.load(filename);

	if(v != 0)
	{
		printf("Cannot load directory file %s\n", filename);

		return -1;
	}

	if(module.dirVersion <= 0)
	{
		printf("Promoting the directory version to 1\n");
		
		module.dirVersion = 1;
	}

	if(verbose)
	{
		module.print();
	}

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );

	if(realTime > 0)
	{
		printf("Setting real-time playback mode\n");
		WATCHDOGTEST( XLRSetFillData(xlrDevice, MARK5_FILL_PATTERN) );
		WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	}
	else
	{
		WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_REALTIMEPLAYBACK) );
		WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	}


	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	if(strncmp(mk5status.vsnA, vsn, 8) == 0)
	{
		mk5status.activeBank = 'A';
		strcpy(vsn, mk5status.vsnA);
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_A) );
	}
	else if(strncmp(mk5status.vsnB, vsn, 8) == 0)
	{
		mk5status.activeBank = 'B';
		strcpy(vsn, mk5status.vsnB);
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_B) );
	}
	else
	{
		if(strlen(vsn) == 8)
		{
			printf("Module %s not loaded\n", vsn);
		}
		else
		{
			printf("Explicit module name required for module writing\n");
		}

		return -1;
	}

	v = module.writeDirectory(xlrDevice);
	if(v != 0)
	{
		return v;
	}

	switch(dmsMode)
	{
	case DMS_MODE_NO_UPDATE:
		fprintf(stderr, "FYI: Not setting disk module state to Recorded for %s\n\n", vsn);
		dmsUpdate = 0;
		break;
	case DMS_MODE_UPDATE:
		dmsUpdate = 1;
		break;
	case DMS_MODE_UPDATE_IF_SAFE:
	case DMS_MODE_FAIL_UNLESS_SAFE:
		dmsUpdate = 0;
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Not setting disk module state of %s to Recorded because "
			"the safety of the operation cannot be guaranteed.", vsn);
		fprintf(stderr, "Warning: %s\n\n", message);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		break;
	}
	if(dmsUpdate)
	{
		setDiscModuleStateNew(xlrDevice, MODULE_STATUS_RECORDED);

		printf("Note: the Disk Module State has been changed to Recorded.\n");
		printf("Use program vsn to change this if you would like to set it differently\n\n");
	}

	mk5status.state = MARK5_STATE_GETDIR;
	difxMessageSendMark5Status(&mk5status);

	WATCHDOG( XLRClose(xlrDevice) );

	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = module.nScans();
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

int main(int argc, char **argv)
{
	char vsn[16] = "";
	int force=0;
	int fast=0;
	enum DMS_Mode dmsMode = DMS_MODE_FAIL_UNLESS_SAFE;
	int v;
	const char *dmsMaskStr;
	int lockWait = 10;	/* [sec] set to MARK5_LOCK_WAIT_FOREVER if immediate quit on lock behavior is wanted */
	int dmsMask = 7;
	int startScan = -1;
	int stopScan = -1;
	int retval = EXIT_SUCCESS;
	int realTime = 0;
	int forceVersion = -1;
	const char *writeFile = 0;

	dmsMaskStr = getenv("DEFAULT_DMS_MASK");
	if(dmsMaskStr)
	{
		dmsMask = atoi(dmsMaskStr);
		if((dmsMask & 2) == 0)
		{
			dmsMode = DMS_MODE_NO_UPDATE;
		}
	}

	difxMessageInit(-1, "mk5dir");

	if(argc < 2)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	for(int a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}
		if(strcmp(argv[a], "--version") == 0)
		{
			printVersion();

			return EXIT_SUCCESS;
		}
		else if(strcmp(argv[a], "-v") == 0 ||
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
			force = 1;
		}
		else if(strcmp(argv[a], "-F") == 0 ||
			strcmp(argv[a], "--fast") == 0)
		{
			fast = 1;
		}
		else if(strcmp(argv[a], "-r") == 0 ||
			strcmp(argv[a], "--realtime") == 0)
		{
			realTime = 1;
		}
		else if(strcmp(argv[a], "-n") == 0 ||
			strcmp(argv[a], "--nodms") == 0)
		{
			dmsMode = DMS_MODE_NO_UPDATE;
		}
		else if(strcmp(argv[a], "-s") == 0 ||
			strcmp(argv[a], "--safedms") == 0)
		{
			dmsMode = DMS_MODE_UPDATE_IF_SAFE;
		}
		else if(strcmp(argv[a], "-d") == 0 ||
			strcmp(argv[a], "--dmsforce") == 0)
		{
			dmsMode = DMS_MODE_UPDATE;
		}
		else if(strcmp(argv[a], "-1") == 0 ||
			strcmp(argv[a], "--version1") == 0)
		{
			forceVersion = 1;
		}
		else if(argv[a][0] == '-' && a+1 < argc)
		{
			if(strcmp(argv[a], "-b") == 0 ||
			   strcmp(argv[a], "--begin") == 0)
			{
				++a;
				startScan = atoi(argv[a]) - 1;
			}
			else if(strcmp(argv[a], "-e") == 0 ||
			   strcmp(argv[a], "--end") == 0)
			{
				++a;
				stopScan = atoi(argv[a]);
			}
			else if(strcmp(argv[a], "-w") == 0 ||
			   strcmp(argv[a], "--write") == 0)
			{
				++a;
				writeFile = argv[a];
			}
			else
			{
				usage(argv[0]);

				return EXIT_FAILURE;
			}
		}
		else if(vsn[0] == 0)
		{
			strncpy(vsn, argv[a], 8);
			vsn[8] = 0;
		}
		else
		{
			usage(argv[0]);

			return EXIT_FAILURE;
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

	v = lockMark5(lockWait);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		if(writeFile == 0)
		{
			v = mk5dir(vsn, force, fast, dmsMode, startScan, stopScan, realTime, forceVersion);
		}
		else
		{
			v = writeDir(vsn, force, writeFile, dmsMode, realTime);
		}
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
