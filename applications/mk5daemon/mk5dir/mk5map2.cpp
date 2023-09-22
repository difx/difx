/***************************************************************************
 *   Copyright (C) 2018 by Walter Brisken                                  *
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
 * $Id: mk5dir.cpp 3216 2011-04-11 14:55:49Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/mk5dir/mk5dir.cpp $
 * $LastChangedRevision: 3216 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2011-04-11 08:55:49 -0600 (Mon, 11 Apr 2011) $
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <list>
#include <signal.h>
#include <unistd.h>
#include <stdint.h>
#include <inttypes.h>
#include <time.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include <mark5access.h>
#include "mark5dir.h"
#include "watchdog.h"
#include "config.h"

const char program[] = "mk5map2";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20180919";

const int BufferLength = 100<<20;	// bytes; 100 MiB
const int MaxPacketSize = 12000;	// bytes
const int JumpSec = 5;
const int JumpReadSize = 1<<20;		// bytes; 1 MiB

int verbose = 1;
volatile int die = 0;


typedef struct vdif_edv3_header {       /* VLBA extensions: see http://www.vlbi.org/vdif/docs/vlbaupgradememo42.pdf */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;

   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;

   uint32_t framelength8 : 24;  // Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;

   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t samprate : 23;      // in samprateunits; note this is internal complex sample rate; double for real value output
   uint32_t samprateunits : 1;  // 0 = kHz, 1 = MHz
   uint32_t eversion : 8;       // set to 3

   uint32_t syncword;           // 0xACABFEED

   uint32_t tuning;             // In DDS units of 1/2^24 MHz

   uint32_t personalitytype : 8;
   uint32_t minorrev : 4;       // minor part of rev number
   uint32_t majorrev : 4;       // major part of revision number
   uint32_t sideband : 1;       // 1 = upper, 0 = lower
   uint32_t subband : 3;        // subband selector (RDBE specific)
   uint32_t ifnumber : 4;       // which input IF
   uint32_t dbeunit : 4;        // which unit produced this data
   uint32_t unassigned2 : 4;
 } vdif_edv3_header;



/* Note: must use the less appropriate signal() rather than sigaction() call 
 * because streamstor library seems to use signal() and mixing the two
 * is bad. */
sighandler_t oldsiginthand;
sighandler_t oldsigtermhand;

static void printVersion()
{
	printf("%s ver. %s   %s %s\n", program, version, author, verdate);
}

static void usage(const char *pgm)
{
	printf("\n");
	printVersion();
	printf("\n");
	printf("A program to extract Mark5 module directory information via XLR calls (Mark5B and VDIF)\n");
	printf("\nUsage : %s [<options>] { <bank> | <vsn> }\n\n", pgm);
	printf("<options> can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --version      Print version number and quit\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("  --quiet\n");
	printf("  -q             Be more verbose\n\n");
	printf("  --realtime     Use real-time mode when reading\n\n");
	printf("  --begin <B>\n");
	printf("  -b <B>         Begin search at byte position <B> [0]\n\n");
	printf("  --end <E>\n");
	printf("  -e <E>         End search at byte position <E> [full data length]\n\n");
}

static void siginthand(int j)
{
	if(verbose)
	{
		fprintf(stderr, "Being killed (INT)\n");
	}
	die = 1;
}

static void sigtermhand(int j)
{
	if(verbose)
	{
		fprintf(stderr, "Being killed (TERM)\n");
	}
	die = 1;
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

static int mk5map2(char *vsn, uint64_t begin, uint64_t end, enum Mark5ReadMode readMode)
{
	SSHANDLE xlrDevice;
	S_DIR dir;
	char label[XLR_LABEL_LENGTH+1];
	DifxMessageMk5Status mk5status;
	int v;
	int bank;
	unsigned int signature;
	int len;
	char *dirData;
	unsigned int a, b;
	uint32_t *b32;
	const vdif_edv3_header *vh;

	unsigned char *buffer;
	long long toRead;
	long long readPtr;

	long long lastBegin;	// byte of last scan start
	long long lastEnd;	// byte of last scan end
	int firstSecond; // UTC or VDIF Epoch second of first frame of scan
	int lastSecond;	// UTC or VDIF Epoch second of last frame found
	int lastFormat;	// -1 unknown, 0 mark5b, 1 vdif
	long long leftover = 0;

	int nScan = 0;
	int maxFrame = 0;
	int maxThread = 0;
	int okToJump = 0;
	int frameSize = 0;
	int nextJumpSec = 0;

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	if(readMode == MARK5_READ_MODE_RT)
	{
		fprintf(stderr, "Setting real-time playback mode\n");
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

	bank = Mark5BankSetByVSN(xlrDevice, vsn);

	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );
	label[XLR_LABEL_LENGTH] = 0;

	signature = 1;
	WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );
	fprintf(stderr, "User Dir Length = %d\n", len);
	if(len > 128)
	{
		dirData = new char[len];

		WATCHDOGTEST( XLRGetUserDir(xlrDevice, len, 0, dirData) );

		for(int j = 32; j < len/4; ++j)
		{
			unsigned int x = ((unsigned int *)dirData)[j] + 1;
			signature = signature ^ x;
		}

		/* prevent a zero signature */
		if(signature == 0)
		{
			signature = 0x55555555;
		}

		delete[] dirData;
	}

	fprintf(stderr, "About to map module %s with %Ld recorded bytes\n\n", label, dir.Length);

	if(end == 0 || end > dir.Length)
	{
		end = dir.Length;
	}

	mk5status.state = MARK5_STATE_GETDIR;
	difxMessageSendMark5Status(&mk5status);

	oldsiginthand = signal(SIGINT, siginthand);
	oldsigtermhand = signal(SIGTERM, sigtermhand);

	// The action starts here

	toRead = end-begin;
	readPtr = begin;
	buffer = (unsigned char *)malloc(BufferLength);
	b32 = (uint32_t *)buffer;

	firstSecond = -1;
	lastFormat = -1;
	lastSecond = -1;
	lastBegin = 0;
	lastEnd = 0;

	while(!die)
	{
		long long readSize;
		long long bufferStart;
		long long toProcess;
		long long offset;
		int sec;

		if(toRead < MaxPacketSize)
		{
			if(lastFormat >= 0)
			{
				if(lastEnd - lastBegin > 1000000)
				{
					printf("%d %lld %lld %d %d\n", nScan, lastBegin, lastEnd, lastFormat, frameSize);
					fflush(stdout);
					++nScan;
				}
			}

			break;
		}

		if(toRead+leftover < BufferLength)
		{
			readSize = toRead;
		}
		else
		{
			readSize = BufferLength - leftover;
		}
		if(leftover > 0)
		{
			memmove(buffer, buffer+BufferLength-leftover, leftover);
		}

		//printf("\nTotal bytes left to process: %lld\n", toRead);
		//printf("Bytes to read: %lld\n", readSize);

		a = readPtr >> 32;
		b = readPtr & 0xFFFFFFFFLL;
		WATCHDOGTEST( XLRReadData(xlrDevice, (unsigned int *)(buffer + leftover), a, b, readSize));
		bufferStart = readPtr - leftover;
		readPtr += readSize;
		toRead -= readSize;

		offset = 0;
		toProcess = readSize + leftover - MaxPacketSize;
		while(offset < toProcess)
		{
			vh = (const vdif_edv3_header *)(buffer + offset);
			if(vh->syncword == 0xACABFEED)
			{
				sec = vh->seconds;

				if(lastFormat == 1 && lastSecond != sec)
				{
					fprintf(stderr, "Second: %d -> %d  ", lastSecond, sec);
					fprintf(stderr, "Total bytes left to process: %lld  ", toRead);
					fprintf(stderr, "Bytes to read: %lld  ", readSize);
					fprintf(stderr, "nScan = %d\n", nScan);
				}

				if(lastFormat != 1 || sec > lastSecond+1 || sec < lastSecond-1)
				{
					if(lastFormat >= 0)
					{
						if(lastEnd - lastBegin > 1000000)
						{
							printf("%d %lld %lld %d %d\n", nScan, lastBegin, lastEnd - lastBegin, lastFormat, frameSize);
							fflush(stdout);
							++nScan;
						}
					}

					lastFormat = 1;
					lastBegin = lastEnd = bufferStart + offset;
					firstSecond = sec;
					maxFrame = 0;
					maxThread = 0;
					okToJump = 1;
					nextJumpSec = -100;
				}
				else
				{
					lastEnd = bufferStart + offset;
				}
				if(vh->threadid > maxThread)
				{
					maxThread = vh->threadid;
				}
				if(vh->frame > maxFrame)
				{
					maxFrame = vh->frame;
				}

				lastSecond = sec;
				frameSize = vh->framelength8*8;
				offset += frameSize;
			
				continue;
			}
			else if(*((uint32_t *)(buffer+offset)) == 0xABADDEED)
			{
				sec = buffer[offset+8] + buffer[offset+9]*10 + buffer[offset+10]*100 + buffer[offset+11]*1000 + buffer[offset+12]*10000;

				if(lastFormat == 0 && lastSecond != sec)
				{
					fprintf(stderr, "Second: %d -> %d  ", lastSecond, sec);
					fprintf(stderr, "Total bytes left to process: %lld  ", toRead);
					fprintf(stderr, "Bytes to read: %lld  ", readSize);
					fprintf(stderr, "nScan = %d\n", nScan);
				}

				if(sec < lastSecond - 86300)
				{
					lastSecond -= 86400;	/* day boundary */
					fprintf(stderr, "day jump: %d -> %d\n", lastSecond + 86400, lastSecond);
				}

				if(lastFormat != 0 || sec > lastSecond+1 || sec < lastSecond-1)
				{
					if(lastFormat >= 0)
					{
						if(lastEnd - lastBegin > 1000000)
						{
							printf("%d %lld %lld %d %d\n", nScan, lastBegin, lastEnd - lastBegin, lastFormat, frameSize);
							fflush(stdout);
							++nScan;
						}
					}

					lastFormat = 0;
					lastBegin = lastEnd = bufferStart + offset;
					firstSecond = sec;
					okToJump = 1;
					nextJumpSec = -100;
					frameSize = 10016;
				}
				else
				{
					lastEnd = bufferStart + offset;
				}

				lastSecond = sec;
				offset += 10016;

				continue;
			}
			else
			{
				offset += 4;
			}
		}

		leftover += (readSize - offset);

		if(sec == nextJumpSec && okToJump == 0)
		{
			fprintf(stderr, "Reenable jumping after a series of good seconds\n");
			okToJump = 1;
		}

		while(okToJump && sec > firstSecond+1 && !die)
		{
			long long jump;
			if(lastFormat == 0)	// Mark5B
			{
				jump = JumpSec*25600*10016;
			}
			else if(lastFormat == 1)
			{
				int nThread;
				int fps;

				nThread = maxThread + 1;
				for(fps = 100; fps < maxFrame; fps *= 2) ;
				
				jump = JumpSec*fps*nThread*frameSize;
			}
			else
			{
				break;
			}

			if(readPtr + jump > end)
			{
				okToJump = 0;
				nextJumpSec = lastSecond + 6;
				toRead = end - readPtr;
				bufferStart = readPtr;

				break;
			}

			fprintf(stderr, "Jumping %d seconds = %lld bytes\n", JumpSec, jump);

			readPtr = readPtr - leftover + jump;

			a = readPtr >> 32;
			b = readPtr & 0xFFFFFFFFLL;
			WATCHDOGTEST( XLRReadData(xlrDevice, (unsigned int *)buffer, a, b, JumpReadSize));

			leftover = 0;

			/* conditions for successful jump:
			   1. Jump lands exactly on frame 
			   2. Seconds within +-1 of expected
			 */

			/* get second from data */
			if(lastFormat == 0)
			{
				if(*((uint32_t *)buffer) == 0xABADDEED)
				{
					sec = buffer[8] + buffer[9]*10 + buffer[10]*100 + buffer[11]*1000 + buffer[12]*10000;
				}
				else
				{	
					okToJump = 0;
					nextJumpSec = lastSecond + 6;
				}
			}
			else
			{
				vh = (const vdif_edv3_header *)buffer;
				if(vh->syncword == 0xACABFEED)
				{
					sec = vh->seconds;
				}
				else
				{
					okToJump = 0;
					nextJumpSec = lastSecond + 6;
				}
			}

			if(okToJump)
			{
				if(sec > lastSecond+JumpSec-2 && sec < lastSecond+JumpSec+2)
				{
					lastSecond = sec;
				}
				else
				{
					okToJump = 0;
					nextJumpSec = lastSecond + 6;
				}
			}

			if(!okToJump)
			{
				/* here simply go back to a regular read */
				readPtr -= jump;
				toRead = end - readPtr;
				bufferStart = readPtr;
			}
		}
	}

	// Shutdown here

	free(buffer);

	WATCHDOG( XLRClose(xlrDevice) );

	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = 0;
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

int main(int argc, char **argv)
{
	enum Mark5ReadMode readMode = MARK5_READ_MODE_NORMAL;
	char vsn[16] = "";
	int v;
	uint64_t begin = 0LL;
	uint64_t end = 0LL;
	int retval = EXIT_SUCCESS;

	difxMessageInit(-1, program);

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
		else if(strcmp(argv[a], "--version") == 0)
		{
			printVersion();
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
		else if(strcmp(argv[a], "--realtime") == 0)
		{
			readMode = MARK5_READ_MODE_RT;
		}
		else if(argv[a][0] == '-' && a+1 < argc)
		{
			if(strcmp(argv[a], "-b") == 0 ||
			        strcmp(argv[a], "--begin") == 0)
			{
				++a;
				begin = atoll(argv[a]);
			}
			else if(strcmp(argv[a], "-e") == 0 ||
			        strcmp(argv[a], "--end") == 0)
			{
				++a;
				end = atoll(argv[a]);
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

	if(vsn[0] == 0)
	{
		fprintf(stderr, "Error: no module or bank specified\n");

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
		v = mk5map2(vsn, begin, end, readMode);
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
