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
#include <xlrapi.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "config.h"
#include "mark5dir.h"
#include "watchdog.h"
#include "nraobaddrives.h"
#include "../config.h"

const char program[] = "vsn";
const char author[]  = "Walter Brisken";
const char version[] = "0.6";
const char verdate[] = "20120713";



enum WriteProtectAction
{
	WPA_NONE	= 0,
	WPA_SET,
	WPA_CLEAR
};

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to set/get a Mark5 module Volume Serial Number (VSN)\n\n");
	printf("Usage: %s <options> <bank> [<vsn>]\n\n", pgm);
	printf("<options> can include:\n\n");
	printf("  --help\n");
	printf("  -h         Print help info and quit\n\n");
	printf("  --quiet\n");
	printf("  -q         Just print the VSN and quit\n\n");
	printf("  --verbose\n");
	printf("  -v         Be more verbose in execution\n\n");
	printf("  --force\n");
	printf("  -f         Don't ask before continuing\n\n");
	printf("  --played\n");
	printf("  -p         Set module state to 'Played'\n\n");
	printf("  --recorded\n");
	printf("  -r         Set module state to 'Recorded'\n\n");
	printf("  --erased\n");
	printf("  -e         Set module state to 'Erased'; this doesn't erase the module\n\n");
	printf("  --writeprotect\n");
	printf("  -w         Write protect the module\n\n");
	printf("  --unwriteprotect\n");
	printf("  -u         Unprotect the module against writing\n\n");
#ifdef XLR_MAX_SMARTVALUES
	printf("  --smart\n");
	printf("  -s         Get S.M.A.R.T. data from disks (writes to file)\n\n");
#endif
	printf("<bank> should be either A or B.\n\n");
	printf("<vsn> is the new module VSN (must be 8 characters long).\n");
	printf("  If not provided, the existing VSN will be returned.\n\n");
	printf("This program appears to be compiled for SDK version %d\n\n", SDKVERSION);
}

int roundsize(int s)
{
	long long a;

	a = (long long)s*512LL;
	a /= 1000000000;
	a = (a+2)/5;

	return a*5;
}

#ifdef XLR_MAX_SMARTVALUES
/* isCritical is set to 1 if a non-zero value indicates a critical issue */
long long interpretSMART(char *smartDescription, int maxLength, const S_SMARTVALUES *smart, int *isCritical)
{
	long long V=0;
	int i;

	for(i = 0; i < 6; ++i)
	{
		V = (V << 8) + smart->raw[i];
	}

	*isCritical = 0;

	switch(smart->ID)
	{
	case 1:
		snprintf(smartDescription, maxLength, "Read error count = %Ld", V);
		*isCritical = 1;
		break;
	case 3:
		snprintf(smartDescription, maxLength, "Spin up time = %Ld ms", V);
		break;
	case 4:
		snprintf(smartDescription, maxLength, "Start/stop count = %Ld", V);
		break;
	case 5:
		snprintf(smartDescription, maxLength, "Reallocated sector count = %Ld", V);
		*isCritical = 1;
		break;
	case 7:
		snprintf(smartDescription, maxLength, "Seek error rate = %Ld", V);
		break;
	case 8:
		snprintf(smartDescription, maxLength, "Seek time performance = %Ld", V);
		break;
	case 9:
		snprintf(smartDescription, maxLength, "Power on time = %Ld hours", V);
		break;
	case 10:
		snprintf(smartDescription, maxLength, "Spin retry count = %Ld", V);
		*isCritical = 1;
		break;
	case 11:
		snprintf(smartDescription, maxLength, "Recalibration retry count = %Ld", V);
		break;
	case 12:
		snprintf(smartDescription, maxLength, "Power cycle count = %Ld", V);
		break;
	case 187:
		snprintf(smartDescription, maxLength, "Reported Uncorrectable Errors = %Ld\n", V);
		break;
	case 189:
		snprintf(smartDescription, maxLength, "High Fly Writes = %Ld", V);
		break;
	case 190:
		snprintf(smartDescription, maxLength, "Airflow Temperature = %Ld", V);
		break;
	case 192:
		snprintf(smartDescription, maxLength, "Retract cycle count = %Ld", V);
		break;
	case 193:
		snprintf(smartDescription, maxLength, "Landing zone load count = %Ld", V);
		break;
	case 194:
		snprintf(smartDescription, maxLength, "Temperature = %Ld C", V);
		break;
	case 195:
		snprintf(smartDescription, maxLength, "Hardware ECC Recovered = %Ld", V);
		break;
	case 196:
		snprintf(smartDescription, maxLength, "Relocation event count = %Ld", V);
		*isCritical = 1;
		break;
	case 197:
		snprintf(smartDescription, maxLength, "Questionable sector count = %Ld", V);
		*isCritical = 1;
		break;
	case 198:
		snprintf(smartDescription, maxLength, "Uncorrectable sector count = %Ld", V);
		*isCritical = 1;
		break;
	case 199:
		snprintf(smartDescription, maxLength, "DMA CRC error count = %Ld", V);
		break;
	case 200:
		snprintf(smartDescription, maxLength, "Multi-zone error count = %Ld", V);
		break;
	case 201:
		snprintf(smartDescription, maxLength, "Off-track error count = %Ld", V);
		*isCritical = 1;
		break;
	case 202:
		snprintf(smartDescription, maxLength, "Data Address Mark error count = %Ld", V);
		break;
	case 209:
		snprintf(smartDescription, maxLength, "Offline Seek Performance = %Ld\n", V);
		break;
	case 212:
		snprintf(smartDescription, maxLength, "Shock During Write = %Ld\n", V);
		break;
	default:
		snprintf(smartDescription, maxLength, "Unknown SMART value = %Ld", V);
		break;
	}

	return V;
}
#endif

int setvsn(int bank, char *newVSN, int newStatus, enum WriteProtectAction wpa, int force, int verbose, int getSMART)
{
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	S_BANKSTATUS bankStat;
	S_DIR dir;
#ifdef XLR_MAX_SMARTVALUES
	const int MaxSmartDescriptionLen=100;
	S_SMARTVALUES smartValues[XLR_MAX_SMARTVALUES];
	USHORT smartVersion;
#endif
	char label[XLR_LABEL_LENGTH+1];
	char oldLabel[XLR_LABEL_LENGTH+1];
	int dirVersion = -1;	/* 0 = Legacy or NeoLegacy, 1 = Mark5C, -1 = unset */
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
	FILE *out = 0;
	struct Mark5DirectoryInfo dirInfo;
	unsigned char *dirData;

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bankStat) );
	if(bankStat.State != STATE_READY)
	{
		if(verbose < 0)
		{
			printf("-\n");
		}
		else
		{
			fprintf(stderr, "Bank %c not ready\n", 'A'+bank);
		}
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
	strncpy(oldLabel, label, XLR_LABEL_LENGTH+1);

#ifdef XLR_MAX_SMARTVALUES
	if(getSMART)
	{
		const int SMARTFileLength = 48;
		char SMARTfile[SMARTFileLength];

		if(strlen(newVSN) == 8)
		{
			v = snprintf(SMARTfile, SMARTFileLength, "%s.smart", newVSN);
		}
		else
		{
			char tmp[10];
			strncpy(tmp, label, 8);
			tmp[8] = 0;
			v = snprintf(SMARTfile, SMARTFileLength, "%s.smart", tmp);
		}
		if(v >= SMARTFileLength)
		{
			fprintf(stderr, "Developer error: setvsn: SMARTFileLength is too small (%d < %d)\n", SMARTFileLength, v+1);
		}
		out = fopen(SMARTfile, "w");
		if(!out)
		{
			fprintf(stderr, "\nCannot open %s for write.\n\n", SMARTfile);
			fprintf(stderr, "SMART data not being written to file!\n\n");
		}
		else
		{
			printf("Writing S.M.A.R.T. data to file %s\n\n", SMARTfile);
		}
	}
#endif

	if(verbose > 0)
	{
		printf("Raw label = %s\n", label);
	}

	WATCHDOG( dirLength = XLRGetUserDirLength(xlrDevice) );
	dirData = (unsigned char *)calloc(dirLength, 1);
	WATCHDOG( xlrRC = XLRGetUserDir(xlrDevice, dirLength, 0, dirData) );
	v = getMark5DirectoryInfo(&dirInfo, dirData, dirLength);
	free(dirData);
	if(v != Mark5DirectoryInfoSuccess)
	{
		fprintf(stderr, "Directory parse error %d encountered: %s\n", v, Mark5DirectoryInfoStatusStrings[v]);
	}

	switch(dirInfo.dirClass)
	{
	case Mark5DirClassNone:
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
		break;
	case Mark5DirClassLegacy:
	case Mark5DirClassNeoLegacy:
		parseModuleLabel(label, 0, 0, 0, &moduleStatus);
		dirVersion = 0;
		break;
	case Mark5DirClassMark5C:
		v = getModuleDirectoryVersion(xlrDevice, &dirVersion, 0, &moduleStatus);
		if(v < 0)
		{
			if(out)
			{
				fclose(out);
				out = 0;
			}

			return v;
		}
		break;
	case Mark5DirClassIllegal:
		fprintf(stderr, "Warning: the module directory is corrupt.\n");
		break;
	}

	if(out)
	{
		fprintMark5DirectoryInfo(out, &dirInfo);
	}

	if(isLegalModuleLabel(oldLabel))
	{
		char vsn[10];

		parseModuleLabel(label, vsn, &capacity, &rate, 0);

		if(verbose < 0)
		{
			printf("%s\n", vsn);
		}
		else
		{
			printf("\nCurrent extended VSN is %s/%d/%d\n", vsn, capacity, rate);
			printf("Current disk module status is %s\n", moduleStatusName(moduleStatus) );
			fprintMark5DirectoryInfo(stdout, &dirInfo);
			printf("This module contains %lld bytes of recorded data and is %4.2f%% full.\n", dir.Length,
				100.0*roundModuleSize(dir.Length)/capacity);
			printf("Write protection is %s.", bankStat.WriteProtected ? "ON" : "OFF");
		}
		if(out)
		{
			fprintf(out, "VSN: %s/%d/%d\n\n", vsn, capacity, rate);
		}
	}
	else
	{
		if(verbose < 0)
		{
			printf("-\n");
		}
		else
		{
			printf("\nNo VSN currently set on module\n");
		}
		if(out)
		{
			fprintf(out, "VSN: <unset>\n\n");
		}
	}

	if(verbose < 0)
	{
		WATCHDOG( XLRClose(xlrDevice) );

		if(out)
		{
			fclose(out);
			out = 0;
		}

		return 0;
	}

	printf("\n");

	nDrive = getDriveInformation(xlrDevice, drive, &capacity);

	printf("This module consists of %d drives totalling about %d GB:\n", nDrive, capacity);
	if(out)
	{
		fprintf(out, "This module consists of %d drives totalling about %d GB:\n\n\n", nDrive, capacity);
	}
	for(d = 0; d < 8; ++d)
	{
		const char *badDriveMessage = "";

		if(drive[d].model[0] == 0)
		{
			continue;
		}

		if(isDriveBad(drive[d].serial))
		{
			badDriveMessage = "* THIS DRIVE IS ON THE BAD DRIVE LIST";
		}
		else if(inBadDriveRange(drive[d].serial))
		{
			badDriveMessage = "* In bad drive serial number range";
		}

		printf("Disk %d info : %s : %s : %s : %d : %s\n",
			d, drive[d].model, drive[d].serial, drive[d].rev,
			roundModuleSize(drive[d].capacity),
			drive[d].failed ? "FAILED" : "OK");
		if(badDriveMessage[0])
		{
			printf("  %s\n", badDriveMessage);
		}
		if(out)
		{
			fprintf(out, "Disk %d info : %s : %s : %s : %d : %s\n\n",
				d, drive[d].model, drive[d].serial, drive[d].rev,
				roundModuleSize(drive[d].capacity),
				drive[d].failed ? "FAILED" : "OK");
			if(badDriveMessage[0])
			{
				fprintf(out, "  %s\n", badDriveMessage);
			}
		}

#ifdef XLR_MAX_SMARTVALUES
		if(drive[d].smartCapable)
		{
			WATCHDOG( xlrRC = XLRReadSmartValues(xlrDevice, &smartVersion, smartValues, d/2, d%2) );
			if(xlrRC != XLR_SUCCESS)
			{
				if(out)
				{
					fprintf(out, "SMART values not available for disk %d\n\n", d);
				}
			}
			else
			{
				if(out)
				{
					fprintf(out, "SMART values version %d\n", smartVersion);
				}
				for(v = 0; v < XLR_MAX_SMARTVALUES; ++v)
				{
					if(smartValues[v].ID > 0)
					{
						char smartDescription[MaxSmartDescriptionLen];
						long long V;
						int isCritical;
						
						V = interpretSMART(smartDescription, MaxSmartDescriptionLen, smartValues+v, &isCritical);
						if(isCritical && V > 0LL)
						{
							printf("  * %s\n", smartDescription);
						}
						if(out)
						{
							fprintf(out, "%3d %5d %3d %3d [%3d %3d %3d %3d %3d %3d]  %s  %s\n", 
								smartValues[v].ID,
								smartValues[v].Status,
								smartValues[v].Current,
								smartValues[v].Worst,
								smartValues[v].raw[0],
								smartValues[v].raw[1],
								smartValues[v].raw[2],
								smartValues[v].raw[3],
								smartValues[v].raw[4],
								smartValues[v].raw[5],
								isCritical ? "**" : "  ",
								smartDescription);
						}
					}
				}
			}
		}
		else if(out)
		{
			fprintf(out, "Drive not SMART capable\n");
		}
		if(out)
		{
			fprintf(out, "\n\n");
		}
#endif
	}
#ifdef XLR_MAX_SMARTVALUES
	if(out)
	{
		fprintf(out, "\nThe columns of the SMART data are:\n");
		fprintf(out, "1. The SMART value ID\n");
		fprintf(out, "2. The status of this SMART value\n");
		fprintf(out, "3. The current value\n");
		fprintf(out, "4. The worst value recorded\n");
		fprintf(out, "[5-10]. Raw data associated with this value\n");
		fprintf(out, "... Comments and interpretation;  ** this field is a potential indicator of failure.\n");
		fprintf(out, "\nInfo from: http://en.wikipedia.org/wiki/S.M.A.R.T.\n");

		fclose(out);
	}
#endif

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
			if(rv == 0)
			{
				/* must be ^D or similar */

				strcpy(resp, "n");
			}
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
			if(!rv)
			{
				printf("Odd... stdin EOF encountered.  Quitting.\n");

				return 0;
			}
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
	int getSMART = 0;
	int lockWait = MARK5_LOCK_DONT_WAIT;
	enum WriteProtectAction wpa = WPA_NONE;
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

					return EXIT_FAILURE;
				}
				newStatus = MODULE_STATUS_ERASED;
			}
			else if(strcmp(argv[a], "-p") == 0 ||
			   strcmp(argv[a], "--played") == 0)
			{
				if(newStatus != 0 && newStatus != MODULE_STATUS_PLAYED)
				{
					fprintf(stderr, "Multiple new states provided!\n");

					return EXIT_FAILURE;
				}
				newStatus = MODULE_STATUS_PLAYED;
			}
			else if(strcmp(argv[a], "-r") == 0 ||
			   strcmp(argv[a], "--recorded") == 0)
			{
				if(newStatus != 0 && newStatus != MODULE_STATUS_RECORDED)
				{
					fprintf(stderr, "Multiple new states provided!\n");

					return EXIT_FAILURE;
				}
				newStatus = MODULE_STATUS_RECORDED;
			}
			else if(strcmp(argv[a], "-w") == 0 ||
			   strcmp(argv[a], "--writeprotect") == 0)
			{
				if(wpa == WPA_CLEAR)
				{
					fprintf(stderr, "Conflicting requests for write protect and unprotect!\n");

					return EXIT_FAILURE;
				}
				wpa = WPA_SET;
			}
			else if(strcmp(argv[a], "-u") == 0 ||
			   strcmp(argv[a], "--unwriteprotect") == 0)
			{
				if(wpa == WPA_SET)
				{
					fprintf(stderr, "Conflicting requests for write protect and unprotect!\n");

					return EXIT_FAILURE;
				}
				wpa = WPA_CLEAR;
			}
			else if(strcmp(argv[a], "-s") == 0 ||
			   strcmp(argv[a], "--smart") == 0)
			{
#ifdef XLR_MAX_SMARTVALUES
				getSMART = 1;
#else
				fprintf(stderr, "SMART is not enabled with this SDK version\n");

				return EXIT_FAILURE;
#endif
			}
			else
			{
				fprintf(stderr, "Unknown option: %s\n", argv[a]);
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
			if(strlen(argv[a]) != 8)
			{
				fprintf(stderr, "Error: VSN length must be 8 characters\n");
				fprintf(stderr, "Run with -h for help info\n");

				return EXIT_FAILURE;
			}
			strncpy(newVSN, argv[a], 8);
			newVSN[8] = 0;
			for(i = 0; i < 8; ++i)
			{
				newVSN[i] = toupper(newVSN[i]);
			}
		}
	}

	if(bank < 0)
	{
		fprintf(stderr, "Error: no bank specified\n");
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

	v = lockMark5(lockWait);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = setvsn(bank, newVSN, newStatus, wpa, force, verbose, getSMART);
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
