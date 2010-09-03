/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
#include <signal.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "config.h"
#include "watchdog.h"
#include "mark5dir.h"
#include "../config.h"

const char program[] = "testmod";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20100903";

const int defaultBlockSize = 10000000;
const int defaultNBlock = 50;
const int defaultNRep = 2;
const int statsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };

int die = 0;
typedef void (*sighandler_t)(int);
sighandler_t oldsiginthand;

void siginthand(int j)
{
	fprintf(stderr, "<Being killed>");
	die = 1;
	signal(SIGHUP, oldsiginthand);
}

int usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to test Mark5 modules\n\n");
	printf("Usage: %s <options> <bank>\n\n", pgm);
	printf("Where <options> can include:\n\n");
	printf("  --verbose\n");
	printf("  -v         Increase the verbosity\n\n");
	printf("  --force\n");
	printf("  -f         Don't ask before proceeding\n\n");
	printf("  --help\n");
	printf("  -h         Print help info and quit\n\n");
	printf("  --readonly\n");
	printf("  -r         Perform read-only test\n\n");
	printf("  --realtime\n");
	printf("  -R         Enable real-time mode (sometimes needed for bad packs)\n\n");
	printf("  --skipdircheck\n");
	printf("  -d         Disable directory checking (sometimes needed for bad packs)\n\n");
	printf("  --nrep <n>\n");
	printf("  -n <n>     Perform the test <n> times (default=%d)\n\n", defaultNRep);
	printf("  --blocksize <s>\n");
	printf("  -s <s>     Use a read/write block of <s> MB (default=%d)\n\n", defaultBlockSize/1000000);
	printf("  --nblock <b>\n");
	printf("  -b <b>     Read/write <b> blocks per test (default=%d)\n\n", defaultNBlock);
	printf("  --dirfile <f>\n");
	printf("  -o <f>     Write the entire module directory to file <f>\n\n");
	printf("  --position <p>\n");
	printf("  -p <p>     Start read tests from pointer position <p>\n\n");
	printf("and <bank> should be either A or B\n\n");
	printf("This program appears to be compiled for SDK version %d\n\n", SDKVERSION);

	return 0;
}

void setbuffer(int num, char *buffer, int size)
{
	int i;

	for(i = 0; i < size; i++) buffer[i] = (num+i) & 0xFF;
}

int comparebuffers(const char *buf1, const char *buf2, int size)
{
	int n = 0;
	int i;

	for(i = 0; i < size; i++)
	{
		if(buf1[i] != buf2[i])
		{
			n++;
		}
	}

	return n;
}

int writeblock(SSHANDLE xlrDevice, int num, char *buffer, int size, int nRep, double *timeAccumulator)
{
	int r;
	struct timeval tv;
	double startTime;
	double endTime;

	if(num == 0)
	{
		WATCHDOGTEST( XLRRecord(xlrDevice, 0, 1) );
	}
	else
	{
		WATCHDOGTEST( XLRAppend(xlrDevice) );
	}

	gettimeofday(&tv, 0);
	startTime = tv.tv_sec + tv.tv_usec*1.0e-6;

	printf("Writing ");
	for(r = 0; r < nRep; r++)
	{
		WATCHDOGTEST( XLRWriteData(xlrDevice, buffer, size) );
		printf("."); fflush(stdout);
		if(die)
		{
			break;
		}
	}

	WATCHDOGTEST( XLRStop(xlrDevice) );

	gettimeofday(&tv, 0);
	endTime = tv.tv_sec + tv.tv_usec*1.0e-6;

	if(timeAccumulator)
	{
		*timeAccumulator += (endTime - startTime);
	}

	printf(" [%7.5f s]\n", endTime - startTime);

	return 0;
}

static long long readblock(SSHANDLE xlrDevice, int num, char *buffer1, char *buffer2, int size, int nRep, long long ptr, double *timeAccumulator)
{
	int r, v;
	long long pos, L = 0;
	unsigned int a, b;
	struct timeval tv;
	double startTime;
	double endTime;

	gettimeofday(&tv, 0);
	startTime = tv.tv_sec + tv.tv_usec*1.0e-6;

	printf("Reading ");
	for(r = 0; r < nRep; r++)
	{
		pos = (long long)size*(num*nRep + r) + ptr;
		a = pos>>32;
		b = pos & 0xFFFFFFFF;
		WATCHDOGTEST( XLRReadData(xlrDevice, (streamstordatatype *)buffer2, a, b, size) );
		v = comparebuffers(buffer1, buffer2, size);
		L += v;
		printf("."); fflush(stdout);
		if(die)
		{
			break;
		}
	}

	gettimeofday(&tv, 0);
	endTime = tv.tv_sec + tv.tv_usec*1.0e-6;

	if(timeAccumulator)
	{
		*timeAccumulator += (endTime - startTime);
	}

	printf(" [%7.5f s]\n", endTime - startTime);

	return L;
}

int getLabels(SSHANDLE xlrDevice, DifxMessageMk5Status *mk5status)
{
	S_BANKSTATUS bankStat;

	mk5status->activeBank = ' ';

	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, BANK_A, &bankStat) );
	if(isLegalModuleLabel(bankStat.Label))
	{
		strncpy(mk5status->vsnA, bankStat.Label, 8);
		mk5status->vsnA[8] = 0;
	}
	else
	{
		mk5status->vsnA[0] = 0;
	}
	if(bankStat.Selected)
	{
		mk5status->activeBank = 'A';
	}
	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, BANK_B, &bankStat) );
	if(isLegalModuleLabel(bankStat.Label))
	{
		strncpy(mk5status->vsnB, bankStat.Label, 8);
		mk5status->vsnB[8] = 0;
	}
	else
	{
		mk5status->vsnB[0] = 0;
	}
	if(bankStat.Selected)
	{
		mk5status->activeBank = 'B';
	}

	return 0;
}

int testModule(int bank, int readOnly, int nWrite, int bufferSize, int nRep, int options, int force, const char *dirFile, long long ptr)
{
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	S_BANKSTATUS bankStat;
	S_DIR dir;
	char label[XLR_LABEL_LENGTH+1];
	char oldLabel[XLR_LABEL_LENGTH+1];
	int labelLength = 0;
	int badLabel = 0;
	int dirLen;
	int nDrive;
	int n, d, v, i;
	long long L, totalError=0, totalBytes=0;
	char *buffer1, *buffer2;
	int dirVersion = 0;
	DifxMessageMk5Status mk5status;
	char vsn[10], message[DIFX_MESSAGE_LENGTH];
	char resp[8] = "Y";
	char *rv;
	char *buffer;
	FILE *out;
	int capacity = 0;
	int rate = 0;
	struct DriveInformation drive[8];
	int moduleStatus = 0;
	double writeTime = 0.0;
	double readTime = 0.0;
	double readRate, writeRate;

	buffer1 = (char *)malloc(bufferSize);
	buffer2 = (char *)malloc(bufferSize);
	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( xlrRC = XLROpen(1, &xlrDevice) );
	if(xlrRC != XLR_SUCCESS)
	{
		fprintf(stderr, "Error: Cannot open streamstor device\n");
		return -1;
	}
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRSetOption(xlrDevice, options) );

	for(int b = 0; b < XLR_MAXBINS; b++)
	{
		driveStats[b].range = statsRange[b];
		driveStats[b].count = 0;
	}
	WATCHDOGTEST( XLRSetDriveStats(xlrDevice, driveStats) );

	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bankStat) );
	if(bankStat.State != STATE_READY)
	{
		fprintf(stderr, "Bank %c not ready\n", 'A'+bank);
		WATCHDOG( XLRClose(xlrDevice) );
		return -1;
	}
	if(!bankStat.Selected)
	{
		WATCHDOGTEST( xlrRC = XLRSelectBank(xlrDevice, bank) );
	}

	/* the following line is essential to work around an apparent streamstor bug */
	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );

	for(labelLength = 0; labelLength < XLR_LABEL_LENGTH; labelLength++)
	{
		if(!label[labelLength])
		{
			break;
		}
	}

	if(labelLength >= XLR_LABEL_LENGTH)
	{
		fprintf(stderr, "Warning: module label is not terminated!");
		label[XLR_LABEL_LENGTH-1] = 0;
		labelLength = XLR_LABEL_LENGTH-1;
		badLabel = 1;
	}

	strcpy(oldLabel, label);
	v = parseModuleLabel(label, vsn, &capacity, &rate, &moduleStatus);
	if(v < 0)
	{
		fprintf(stderr, "Warning: this module has an illegal label: %s\n", label);
	}

	printf("\nCurrent extended VSN is %s/%d/%d\n", vsn, capacity, rate);
	if(moduleStatus > 0)
	{
		printf("Current module status is %s\n", moduleStatusName(moduleStatus));
	}

	v = getModuleDirectoryVersion(&xlrDevice, &dirVersion, 0, 
		(moduleStatus ? 0 : &moduleStatus) );
	if(v < 0)
	{
		return v;
	}

	if(dirVersion < 0)
	{
		fprintf(stderr, "Warning: cannot determine module directory version\n");
	}
	else
	{
		printf("Module directory version appears to be %d\n", dirVersion);
		printf("Module status appears to be %s\n", moduleStatusName(moduleStatus) );
	}

	printf("\n");

	nDrive = getDriveInformation(&xlrDevice, drive, &capacity);

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

	if(!readOnly)
	{
		if(!force && (dir.Length > 0 || 
			      moduleStatus == MODULE_STATUS_PLAYED ||
			      moduleStatus == MODULE_STATUS_RECORDED) )
		{
			printf("\nAbout to perform destructive write/read test.\n");
			printf("This module contains %lld bytes of recorded data\n", dir.Length);
			printf("This test will erase all data on this module!\n");
			printf("Do you wish to continue? [y|n]\n");
			rv = fgets(resp, 8, stdin);
		}
		else
		{
			printf("This module appears empty.\n");
		}
		if(force || resp[0] == 'Y' || resp[0] == 'y')
		{
			WATCHDOGTEST( XLRClearWriteProtect(xlrDevice) );

			WATCHDOG( xlrRC = XLRErase(xlrDevice, SS_OVERWRITE_NONE) );
			if(xlrRC != XLR_SUCCESS)
			{
				fprintf(stderr, "XLRErase Failed\n");
				return -1;
			}

			WATCHDOG( dirLen = XLRGetUserDirLength(xlrDevice) );
			if(dirLen > 8)
			{
				buffer = (char *)malloc(dirLen);
				WATCHDOG( xlrRC = XLRGetUserDir(xlrDevice, dirLen, 0, buffer) );
				if(xlrRC != XLR_SUCCESS)
				{
					fprintf(stderr, "XLRGetUserDir Failed\n");
					free(buffer);
					return -1;
				}

				if(dirFile)
				{
					n = 0;
					out = fopen(dirFile, "w");
					if(out)
					{
						n = fwrite(buffer, dirLen, 1, out);
						fclose(out);
					}
					if(n != 1)
					{
						fprintf(stderr, "Cannot write module directory to file %s\n", dirFile);
					}
				}

				for(i = 0; i < 4; i++) buffer[i] = 0;

				WATCHDOG( xlrRC = XLRSetUserDir(xlrDevice, buffer, dirLen) );
				if(xlrRC != XLR_SUCCESS)
				{
					fprintf(stderr, "XLRSetUserDir Failed\n");
					free(buffer);
					return -1;
				}

				free(buffer);
			}

			printf("\n");

			WATCHDOGTEST( XLRSetLabel(xlrDevice, label, strlen(label)) );
			printf("New disk module state will be %s\n", 
				moduleStatusName(MODULE_STATUS_ERASED) );

			for(n = 0; n < nWrite; n++)
			{
				mk5status.position = (long long)bufferSize*nRep*n;
				mk5status.scanNumber = n+1;

				if(getLabels(xlrDevice, &mk5status) < 0)
				{
					fprintf(stderr, "Error getting bank status\n");
					return -1;
				}

				mk5status.state = MARK5_STATE_TESTWRITE;
				difxMessageSendMark5Status(&mk5status);

				printf("\nTest %d/%d\n", n+1, nWrite);
				setbuffer(n, buffer1, bufferSize);

				v = writeblock(xlrDevice, n, buffer1, bufferSize, nRep, &writeTime);
				if(v < 0)
				{
					return -1;
				}
				
				mk5status.state = MARK5_STATE_TESTREAD;
				difxMessageSendMark5Status(&mk5status);

				L = readblock(xlrDevice, n, buffer1, buffer2, bufferSize, nRep, 0, &readTime);
				if(L < 0)
				{
					return -1;
				}

				printf("%Ld/%Ld differ\n", L, (long long)bufferSize*nRep);

				totalError += L;
				totalBytes += (long long)bufferSize*nRep;
				if(die)
				{
					break;
				}
			}

			printf("\n");

			writeRate = totalBytes/(1.25e5*writeTime);
			readRate = totalBytes/(1.25e5*readTime);

			printf("Total write time = %7.5f sec -> %5.3f Mbps\n", writeTime, writeRate);
			printf("Total read time  = %7.5f sec -> %5.3f Mbps\n", readTime, readRate);
			printf("\n");

			for(d = 0; d < 8; d++)
			{
				WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
				if(xlrRC != XLR_SUCCESS)
				{
					fprintf(stderr, "XLRGetDriveStats failed for drive %d\n", d);
					continue;
				}
				printf("Stats[%d] = %u %u %u %u %u %u %u %u\n", d,
					(unsigned int)(driveStats[0].count), 
					(unsigned int)(driveStats[1].count),
					(unsigned int)(driveStats[2].count), 
					(unsigned int)(driveStats[3].count),
					(unsigned int)(driveStats[4].count), 
					(unsigned int)(driveStats[5].count),
					(unsigned int)(driveStats[6].count), 
					(unsigned int)(driveStats[7].count));
			}

			if(die)
			{
				printf("User interrupt: Stopping test early!\n");
			}
			printf("\nTotal: %Ld/%Ld bytes differ\n", totalError, totalBytes);

			v = snprintf(message, DIFX_MESSAGE_LENGTH, "%8s test complete: %Ld/%Ld bytes read incorrectly", vsn, totalError, totalBytes);
			if(v >= DIFX_MESSAGE_LENGTH)
			{
				fprintf(stderr, "Error: completion message too long\n");
			}
			else
			{
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			}

			WATCHDOGTEST( XLRErase(xlrDevice, SS_OVERWRITE_NONE) );

			v = setModuleLabel(&xlrDevice, vsn, MODULE_STATUS_ERASED, dirVersion, capacity, rate);
			if(v < 0)
			{
				return -1;
			}

			v = resetModuleDirectory(&xlrDevice, vsn, MODULE_STATUS_ERASED, dirVersion, capacity, rate);
			if(v < 0)
			{
				return -1;
			}

			mk5status.state = MARK5_STATE_CLOSE;
			mk5status.activeBank = ' ';
			mk5status.position = 0;
			mk5status.scanNumber = 0;
			difxMessageSendMark5Status(&mk5status);
		}
	}
	else /* Read-only test here */
	{
		WATCHDOG( dirLen = XLRGetUserDirLength(xlrDevice) );
		if(dirLen > 8)
		{
			buffer = (char *)malloc(dirLen);
			WATCHDOG( xlrRC = XLRGetUserDir(xlrDevice, dirLen, 0, buffer) );
			if(xlrRC != XLR_SUCCESS)
			{
				fprintf(stderr, "XLRGetUserDir Failed\n");
				free(buffer);
				return -1;
			}

			if(dirFile)
			{
				n = 0;
				out = fopen(dirFile, "w");
				if(out)
				{
					n = fwrite(buffer, dirLen, 1, out);
					fclose(out);
				}
				if(n != 1)
				{
					fprintf(stderr, "Cannot write module directory to file %s\n", dirFile);
				}
			}
		}
		for(n = 0; n < nWrite; n++)
		{
			mk5status.position = (long long)bufferSize*nRep*n + ptr;
			mk5status.scanNumber = n+1;

			if(getLabels(xlrDevice, &mk5status) < 0)
			{
				fprintf(stderr, "Error getting bank status\n");
				return -1;
			}


			printf("\nTest %d/%d\n", n+1, nWrite);
			setbuffer(n, buffer1, bufferSize);

			
			mk5status.state = MARK5_STATE_TESTREAD;
			difxMessageSendMark5Status(&mk5status);

			L = readblock(xlrDevice, n, buffer1, buffer2, bufferSize, nRep, ptr, &readTime);
			if(L < 0)
			{
				return -1;
			}

			totalError += L;
			totalBytes += (long long)bufferSize*nRep;
			if(die)
			{
				break;
			}
		}

		printf("\n");
		readRate = totalBytes/(1.25e5*readTime);
		printf("Total read time = %7.5f sec -> %5.3f Mbps\n", readTime, readRate);
		printf("\n");

		for(d = 0; d < 8; d++)
		{
			WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
			if(xlrRC != XLR_SUCCESS)
			{
				fprintf(stderr, "XLRGetDriveStats failed for drive %d\n", d);
				continue;
			}
			printf("Stats[%d] = %u %u %u %u %u %u %u %u\n", d,
				(unsigned int)(driveStats[0].count), 
				(unsigned int)(driveStats[1].count),
				(unsigned int)(driveStats[2].count), 
				(unsigned int)(driveStats[3].count),
				(unsigned int)(driveStats[4].count), 
				(unsigned int)(driveStats[5].count),
				(unsigned int)(driveStats[6].count), 
				(unsigned int)(driveStats[7].count));
		}

		if(die)
		{
			printf("User interrupt: Stopping test early!\n");
		}

		strncpy(vsn, label, 8);
		vsn[8] = 0;
		sprintf(message, "%8s read test complete", vsn);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

		mk5status.state = MARK5_STATE_CLOSE;
		mk5status.activeBank = ' ';
		mk5status.position = 0;
		mk5status.scanNumber = 0;
		difxMessageSendMark5Status(&mk5status);
	}

	WATCHDOG( XLRClose(xlrDevice) );

	free(buffer1);
	free(buffer2);

	return 0;
}

int main(int argc, char **argv)
{
	int a, v;
	int bank = -1;
	int nRep = defaultNRep;
	int blockSize = defaultBlockSize;
	int nBlock = defaultNBlock;
	int readOnly = 0;
	int verbose = 0;
	int force = 0;
	int options = SS_OPT_DRVSTATS;
	char *dirFile = 0;
	long long ptr = 0;

	for(a = 1; a < argc; a++)
	{
		if(argv[a][0] == 'A' || argv[a][0] == 'a')
		{
			bank = BANK_A;
		}
		else if(argv[a][0] == 'B' || argv[a][0] == 'b')
		{
			bank = BANK_B;
		}
		else if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[a], "-f") == 0 ||
			   strcmp(argv[a], "--force") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				return usage(argv[0]);
			}
			else if(strcmp(argv[a], "-r") == 0 ||
			        strcmp(argv[a], "--readonly") == 0)
			{
				readOnly = 1;
			}
			else if(strcmp(argv[a], "-R") == 0 ||
			        strcmp(argv[a], "--realtime") == 0)
			{
				options |= SS_OPT_REALTIMEPLAYBACK;
			}
			else if(strcmp(argv[a], "-d") == 0 ||
			        strcmp(argv[a], "--skipdircheck") == 0)
			{
				options |= SS_OPT_SKIPCHECKDIR;
			}
			else if(a+1 < argc)
			{
				if(strcmp(argv[a], "-n") == 0 ||
				        strcmp(argv[a], "--nrep") == 0)
				{
					nRep = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-b") == 0 ||
				        strcmp(argv[a], "--nblock") == 0)
				{
					nBlock = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-s") == 0 ||
				        strcmp(argv[a], "--blocksize") == 0)
				{
					blockSize = 1000000*atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-o") == 0 ||
					strcmp(argv[a], "--dirfile") == 0)
				{
					dirFile = argv[a+1];
				}
				else if(strcmp(argv[a], "-p") == 0 ||
					strcmp(argv[a], "--pointer") == 0)
				{
					ptr = atoll(argv[a+1]);
				}
				else
				{
					fprintf(stderr, "Unknown option %s\n", argv[a]);
					fprintf(stderr, "Run with -h for help info\n");
					return -1;
				}
				a++;
			}
		}
		else
		{
			fprintf(stderr, "Unexpected parameter: %s\n", argv[a]);
			fprintf(stderr, "Run with -h for help info\n");
			return -1;
		}
	}

	if(bank < 0)
	{
		fprintf(stderr, "No bank specified\n");
		fprintf(stderr, "Run with -h for help info\n");
		return -1;
	}

	oldsiginthand = signal(SIGINT, siginthand);

	setWatchdogVerbosity(verbose);

	v = initWatchdog();
	if(v < 0)
	{
		return 0;
	}

	/* 40 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(40);

	difxMessageInit(-1, program);

	/* *********** */

	v = lockMark5(MARK5_LOCK_DONT_WAIT);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = testModule(bank, readOnly, nRep, blockSize, nBlock, options, force, dirFile, ptr);
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
