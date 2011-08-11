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
#include "mark5dir.h"
#if SDKVERSION >= 9
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <signal.h>
#include <mark5ipc.h>
#include <xlrtypes.h>
#include <xlrapi.h>
#include "config.h"
#include "mark5directorystructs.h"
#include "watchdog.h"
#include "../config.h"

const char program[] = "record5c";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20110810";

const int defaultPacketSize = 5008;
const int defaultPayloadOffset = 40;
const int defaultDataFrameOffset = 0;
const int defaultPSNMode = 0;
const int defaultPSNOffset = 0;
const int defaultMACFilterControl = 1;
const unsigned int defaultStreamstorChannel = 28;
const int defaultStatsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };

const int MaxLabelLength = 40;
const int Mark5BFrameSize = 10016;
const UINT32 Mark5BSyncWord = 0xABADDEED;

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

#define N_BANK			2

/* Note that the text going to stdout is interpreted by other code, so change with caution */

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program that records data on a Mark5C unit from the 10G daughter board\n\n");
	printf("Usage: %s [<options>] <bank> <scanname>\n\n", pgm);
	printf("<options> can include:\n\n");
	printf("  --help\n");
	printf("  -h         Print help info and quit\n\n");
	printf("  --verbose\n");
	printf("  -v         Be more verbose in execution\n\n");
	printf("  --packetsize <s>\n");
	printf("  -s <s>     Set packet size to <s> [default %d]\n\n", defaultPacketSize);
	printf("  --dataframeoffset <o>\n");
	printf("  -d <o>     Set data frame offset to <o> [default %d]\n\n", defaultDataFrameOffset);
	printf("  --payloadoffset <o>\n");
	printf("  -p <o>     Set payload offset to <o> [default %d]\n\n", defaultPayloadOffset);
	printf("  --psnoffset <o>\n");
	printf("  -o <o>     Set PSN offset to <o> [default %d]\n\n", defaultPSNOffset);
	printf("  --filtercontrol <f>\n");
	printf("  -f <f>     Set MAC filter control to <f> [default %d]\n\n", defaultMACFilterControl);
	printf("  --bytes <b>\n");
	printf("  -b <b>     Stop recording after <b> bytes written\n\n");
	printf("  --seconds <s>\n");
	printf("  -t <s>     Stop recording after <s> seconds passed\n\n");
	printf("  --statsrange <list>\n");
	printf("  -r <list>  Set Mark5 statistics histogram [default %d,%d,%d,%d,%d,%d,%d]\n\n",
		defaultStatsRange[0], defaultStatsRange[1], defaultStatsRange[2], defaultStatsRange[3],
		defaultStatsRange[4], defaultStatsRange[5], defaultStatsRange[6]);

	printf("Ctrl-C (SIGINT) can be used to cleanly stop recording\n\n");
}


void siginthand(int j)
{
	die = 2;
	signal(SIGINT, oldsiginthand);
}

/* decode5B was taken straight from the DRS source code.  This code is originally (C) 2010 Walter Brisken */
static int decode5B(SSHANDLE xlrDevice, unsigned long long pointer, int framesToRead, unsigned long long *timeBCD, int *firstFrame, int *byteOffset, int *headerMJD, int *headerSeconds)
{
	int bufferSize;
	PUINT32 buffer;
	S_READDESC readdesc;
	int i;
	int rem;
	int returnValue = 0;

	bufferSize = abs(framesToRead)*Mark5BFrameSize;
	buffer = (PUINT32)malloc(bufferSize);

	if(framesToRead < 0)
	{
		pointer -= bufferSize;
		if(pointer < 0)
		{
			pointer = 0;
		}
	}

	rem = pointer % 4;
	if(rem)
	{
		pointer += (4 - rem);
	}

	readdesc.AddrLo = pointer & 0xFFFFFFFF;
	readdesc.AddrHi = pointer >> 32;
	readdesc.XferLength = bufferSize;
	readdesc.BufferAddr = buffer;

	printf("Read %Ld %d\n", pointer, bufferSize);
	fflush(stdout);

	WATCHDOGTEST( XLRRead(xlrDevice, &readdesc) );

	// Mark5B search
	// Look for first sync word and require that at least the next frame also has this
	if(framesToRead > 1)
	{
		const int searchRange=(bufferSize-Mark5BFrameSize)/4 - 8;
		for(i = 0; i < searchRange; i++)
		{
			if(buffer[i] == Mark5BSyncWord)
			{
				printf("Sync 1 1 %d\n", i);
				fflush(stdout);
			}
			if((buffer[i] == Mark5BSyncWord) && (buffer[Mark5BFrameSize/4 + i] == Mark5BSyncWord))
			{
				printf("Sync 1 2 %d\n", Mark5BFrameSize/4 + i);
				fflush(stdout);
				
				break;
			}
		}
		if(i >= searchRange)
		{
			returnValue = -1;
		}
	}
	else
	{
		const int searchRange=(bufferSize-Mark5BFrameSize)/4 - 32;
		for(i = searchRange - 1; i >= 0; i--)
		{
			if(buffer[i] == Mark5BSyncWord)
			{
				printf("Sync 2 1 %d\n", i);
				fflush(stdout);
			}
			if(buffer[i] == Mark5BSyncWord && buffer[Mark5BFrameSize/4 + i] == Mark5BSyncWord)
			{
				printf("Sync 2 2 %d\n", Mark5BFrameSize/4 + i);
				fflush(stdout);

				break;
			}
		}
		if(i < 0)
		{
			returnValue = -1;
		}
		i += Mark5BFrameSize/4;	/* reposition on the last frame */
	}
	if(returnValue == 0)	/* sync words must have been found */
	{
		UINT32 k = buffer[i+2];
		int seconds, mjd;
		int mjdnow;

		// Use system to get first 2 digits of mjd
		mjdnow = (int)(40587.0 + time(0)/86400.0);

		seconds = 0;
		for(int m = 1; m <= 10000; m*=10)
		{
			seconds += (k & 0xF) * m;
			k >>= 4;
		}
		mjd = 0;
		for(int m = 1; m <= 100; m*=10)
		{
			mjd += (k & 0xF) * m;
			k >>= 4;
		}

		// Compute the millenia
		mjd += 1000*( (mjdnow-mjd+500)/1000 );

		if(timeBCD)
		{
			int h, m, s;
			int yr=0, mo=0, da=0, doy;

			mjd2ymd(mjd, &yr, &mo, &da);
			doy = ymd2doy(yr, mo, da);

			s = seconds;
			h = s/3600;
			s -= h*3600;
			m = s/60;
			s -= m*60;
			*timeBCD = 
				((s%10) << 0) +
				((s/10) << 4) +
				((m%10) << 8) +
				((m/10) << 12) + 
				((h%10) << 16) +
				((h/10) << 20);
			for(int k = 0; k < 3; k++)
			{
				*timeBCD += (unsigned long long)(doy % 10) << (24+4*k);
				doy /= 10;
			}
			for(int k = 0; k < 4; k++)
			{
				*timeBCD += (unsigned long long)(yr % 10) << (36+4*k);
				yr /= 10;
			}
		}
		if(firstFrame)
		{
			// second word of frame, 15 least sig bits
			*firstFrame = buffer[i+1] & 0x00007FFF;
		}
		if(byteOffset)
		{
			// convert back to bytes from 32-bit words
			*byteOffset = i*4;
		}
		if(headerMJD)
		{
			*headerMJD = mjd;
		}
		if(headerSeconds)
		{
			*headerSeconds = seconds;
		}
		returnValue = 0;
	}

	free(buffer);

	return returnValue;
}

static void printBankStat(int bank, const S_BANKSTATUS *bankStat)
{
	const char *vsn = bankStat->Label;
	const char noVSN[] = "none";
	int nSlash = 0;

	for(int i = 0; vsn[i]; i++)
	{
		if(vsn[i] == ' ')
		{
			vsn = noVSN;
			break;
		}
		if(vsn[i] == '/')
		{
			nSlash++;
		}
	}
	if(nSlash != 2)
	{
		vsn = noVSN;
	}

	printf("Bank %c %s %d %d %d %d %d %d %d %d %Ld %Ld\n",
		'A' + bank,
		vsn,
		bankStat->State,
		bankStat->Selected,
		bankStat->PowerRequested,
		bankStat->PowerEnabled,
		bankStat->MediaStatus,
		bankStat->WriteProtected,
		bankStat->ErrorCode,
		bankStat->ErrorData,
		bankStat->Length,
#if SDKVERSION >= 9
		bankStat->TotalCapacityBytes
#else
		bankStat->TotalCapacity*512LL
#endif
		);
	fflush(stdout);
}

static int decodeScan(SSHANDLE xlrDevice, long long startByte, long long stopByte,
	struct Mark5DirectoryScanHeaderVer1 *p, struct Mark5DirectoryLegacyBodyVer1 *q)
{
	int frame, byteOffset;
	int mjd1=0, mjd2=0, sec1=0, sec2=0;
	unsigned long long timeBCD;
	int v;

	p->startByte = startByte;
	p->stopByte = stopByte;
	v = decode5B(xlrDevice, startByte, 10, &timeBCD, &frame, &byteOffset, &mjd1, &sec1);
	if(v == 0)
	{
		long long words;
		int samplesPerWord;
		double deltat;
		unsigned long long length = p->stopByte - p->startByte;

		for(int i = 0; i < 8; i++)
		{
			q->timeBCD[i] = ((unsigned char *)(&timeBCD))[i];
		}
		q->firstFrame = frame;
		q->byteOffset = byteOffset;
		q->nTrack = 0xFFFFFFFF;	/* FIXME */
		v = decode5B(xlrDevice, p->stopByte, -10, 0, &frame, 0, &mjd2, &sec2);
		if(v == 0)
		{

			deltat = sec2 - sec1 + 86400*(mjd2 - mjd1) + 1;
			samplesPerWord = 32/upround2(countbits(q->nTrack));
			words = (length/Mark5BFrameSize)*2500;
			/* The Mark5A/B sample rate must be 2^n.  Round up to the nearest */
			for(q->trackRate = 1; q->trackRate < 8192; q->trackRate *= 2)
			{
				if(deltat*q->trackRate*1000000ULL >= words*samplesPerWord)
				{
					break;
				}
			}
			printf("Time %d %d %d  %d %d %d\n", mjd1, sec1, q->firstFrame, mjd2, sec2, frame);
			fflush(stdout);
			printf("TrackRate %d\n", q->trackRate);
			fflush(stdout);
		}
		else
		{
			printf("Decode failure 2\n");
			fflush(stdout);
		}
	}
	else
	{
		printf("Decode failure 1\n");
		fflush(stdout);
	}

	printf("Decode success\n");
	fflush(stdout);

	return 0;
}

static int record(int bank, const char *label, int packetSize, int payloadOffset, int dataFrameOffset, int psnOffset, int psnMode, int macFilterControl, long long maxBytes, double maxSeconds, const int *statsRange, int verbose)
{
	unsigned int channel = defaultStreamstorChannel;
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	S_BANKSTATUS bankStat, stat[N_BANK];
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	S_DIR dir;
	S_DEVSTATUS devStatus;
	int len;
	char *dirData = 0;
	char vsn[XLR_LABEL_LENGTH+1];
	int moduleStatus = MODULE_STATUS_UNKNOWN;
	int v;
	long long ptr;	/* record pointer */
	long long startByte;
	struct Mark5DirectoryHeaderVer1 *dirHeader;
	struct Mark5DirectoryScanHeaderVer1 *p;
	struct Mark5DirectoryLegacyBodyVer1 *q;
	char labelCopy[100];
	char *parts[3];
	int nPart = 0;
	struct timeval tv;
	struct timezone tz;
	double t0, t=0, t_ref, t_next_ref, rate;
	long long p_ref, p_next_ref;

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_DRVSTATS) );
	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bankStat) );
	if(bankStat.State != STATE_READY)
	{
		printf("Error 6 Bank %c not ready\n", 'A'+bank);
		fflush(stdout);
		WATCHDOG( XLRClose(xlrDevice) );
		
		return -1;
	}
	if(!bankStat.Selected)
	{
		WATCHDOGTEST( XLRSelectBank(xlrDevice, bank) );
	}

	/* the following line is essential to work around an apparent streamstor bug */
	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, vsn) );
	vsn[XLR_LABEL_LENGTH] = 0;

	v = parseModuleLabel(vsn, 0, 0, 0, &moduleStatus);
	if(v >= 0)
	{
		printf("VSN %c %s\n", bank+'A', vsn);
		fflush(stdout);
		if(moduleStatus > 0)
		{
			printf("DMS is %s\n", moduleStatusName(moduleStatus));
			fflush(stdout);
		}
	}
	else
	{
		printf("Error 7 No VSN set\n");
		fflush(stdout);
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	for(int b = 0; b < N_BANK; b++)
	{
		WATCHDOGTEST( XLRGetBankStatus(xlrDevice, b, stat+b) );
		printBankStat(b, stat+b);
	}

	startByte = dir.Length;
	printf("Used %Ld %Ld\n", startByte, 0LL);	/* FIXME: 0-> disk size */
	fflush(stdout);

	WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );
	if((len < 128 && len != 0) || len % 128 != 0)
	{
		printf("Error 8 directory format problem\n");
		fflush(stdout);
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	if(len == 0)
	{
		len = 128;
	}

	dirData = (char *)calloc(len+256, 1);	/* make large enough for 2 extra entries */
	WATCHDOGTEST( XLRGetUserDir(xlrDevice, len, 0, dirData) );
	for(int i = 0; i < 256; i++)
	{
		dirData[len+i] = 0;
	}
	dirHeader = (struct Mark5DirectoryHeaderVer1 *)dirData;
	printf("Directory %d %d\n", dirHeader->version, len/128-1);
	fflush(stdout);
	p = (struct Mark5DirectoryScanHeaderVer1 *)(dirData + len);
	q = (struct Mark5DirectoryLegacyBodyVer1 *)(dirData + len + sizeof(struct Mark5DirectoryScanHeaderVer1));

	WATCHDOG( ptr = XLRGetLength(xlrDevice) );

	if(len >= 256)
	{
		long long lastEndByte = ((struct Mark5DirectoryScanHeaderVer1 *)(dirData + len - 128))->stopByte;
		if(ptr - lastEndByte > 1LL<<23)
		{
			/* Here there must have been a problem where the previous scan was not recorded in the dir table. */
			strcpy(p->expName, "UKNOWN");
			strncpy(p->station, ((struct Mark5DirectoryScanHeaderVer1 *)(dirData + len - 128))->station, 2);
			sprintf(p->scanName, "%d", (len/128));
			p->typeNumber = 9 + (len/128)*256;	/* format and scan number */
			p->frameLength = 10016;	/* FIXME */
			decodeScan(xlrDevice, lastEndByte, ptr, p, q);

			len += 128;
			p = (struct Mark5DirectoryScanHeaderVer1 *)(dirData + len);
			q = (struct Mark5DirectoryLegacyBodyVer1 *)(dirData + len + sizeof(struct Mark5DirectoryScanHeaderVer1));
		}
	}

	for(int b = 0; b < XLR_MAXBINS; b++)
	{
		driveStats[b].range = statsRange[b];
		driveStats[b].count = 0;
	}
	WATCHDOGTEST( XLRSetDriveStats(xlrDevice, driveStats) );


	WATCHDOGTEST( XLRSetMode(xlrDevice, SS_MODE_SINGLE_CHANNEL) );
	WATCHDOGTEST( XLRClearChannels(xlrDevice) );
	WATCHDOGTEST( XLRBindInputChannel(xlrDevice, channel) );
	WATCHDOGTEST( XLRSelectChannel(xlrDevice, channel) );

	/* configure 10G input daughter board */
	WATCHDOGTEST( XLRWriteDBReg32(xlrDevice, DATA_PAYLD_OFFSET, payloadOffset) );
	WATCHDOGTEST( XLRWriteDBReg32(xlrDevice, DATA_FRAME_OFFSET, dataFrameOffset) );
	WATCHDOGTEST( XLRWriteDBReg32(xlrDevice, BYTE_LENGTH, packetSize) );
	WATCHDOGTEST( XLRWriteDBReg32(xlrDevice, PSN_OFFSET, psnOffset) );
	WATCHDOGTEST( XLRWriteDBReg32(xlrDevice, MAC_FLTR_CTRL, macFilterControl) );

	printf("Record %s %Ld\n", label, ptr);
	fflush(stdout);
	if(startByte == 0LL)
	{
		WATCHDOGTEST( XLRRecord(xlrDevice, 0, 1) );
	}
	else
	{
		WATCHDOGTEST( XLRAppend(xlrDevice) );
	}

	gettimeofday(&tv, &tz);
	t_ref = t_next_ref = t0 = tv.tv_sec + tv.tv_usec*1.0e-6;
	p_ref = p_next_ref = ptr;
	for(int n = 1; !die; n++)
	{
		usleep(1000);
		
		if(n % 20 == 0)
		{
			gettimeofday(&tv, &tz);
			t = tv.tv_sec + tv.tv_usec*1.0e-6;

			if(t - t0 > maxSeconds)
			{
				printf("Ending time\n");
				fflush(stdout);
				die = 1;
			}
			if(n % 1000 == 0)
			{
				WATCHDOG( ptr = XLRGetLength(xlrDevice) );
				if(ptr - startByte >= maxBytes)
				{
					printf("Ending bytes\n");
					fflush(stdout);
					die = 1;
				}


				WATCHDOG( xlrRC = XLRGetDeviceStatus(xlrDevice, &devStatus) );

				rate = 8.0e-6*(ptr - p_ref) / (t - t_ref);

				printf("Pointer %Ld %4.2f\n", ptr, rate);
				fflush(stdout);

				if(!devStatus.Recording)
				{
					printf("Halted\n");
					fflush(stdout);
					die = 1;
				}
				if(devStatus.DriveFail)
				{
					printf("Drive %d failed\n", devStatus.DriveFailNumber);
					fflush(stdout);
					die = 1;
				}
				if(devStatus.SysError)
				{
					printf("SystemError %d\n", devStatus.SysErrorCode);
					fflush(stdout);
					die = 1;
				}
				if(devStatus.Overflow[0])
				{
					printf("Overflow\n");
					fflush(stdout);
					die = 1;
				}

				if(n % 10000 == 0)
				{
					p_ref = p_next_ref;
					t_ref = t_next_ref;

					p_next_ref = ptr;
					t_next_ref = t;
				}

				/* If there is a change in any other bank, report it */
				for(int b = 0; b < N_BANK; b++)
				{
					if(b == bank)
					{
						continue;
					}
					WATCHDOGTEST( XLRGetBankStatus(xlrDevice, b, &bankStat) );
					if(memcmp(stat+b, &bankStat, sizeof(S_BANKSTATUS)) != 0)
					{
						memcpy(stat+b, &bankStat, sizeof(S_BANKSTATUS));
						printBankStat(b, &bankStat);
					}
				}
			}
		}
	}
	if(die == 2)
	{
		printf("Ending interrupt\n");
		fflush(stdout);
	}

	printf("Stop %s %Ld\n", label, ptr);
	fflush(stdout);
	WATCHDOGTEST( XLRStop(xlrDevice) );

	WATCHDOGTEST( XLRSetMode(xlrDevice, SS_MODE_SINGLE_CHANNEL) );
	WATCHDOGTEST( XLRClearChannels(xlrDevice) );
	WATCHDOGTEST( XLRSelectChannel(xlrDevice, 0) );
	WATCHDOGTEST( XLRBindOutputChannel(xlrDevice, 0) );

	strcpy(labelCopy, label);
	parts[0] = labelCopy;
	nPart = 1;
	for(int i = 0; labelCopy[i] && nPart < 3; i++)
	{
		if(labelCopy[i] == '_' && labelCopy[i+1])
		{
			labelCopy[i] = 0;
			parts[nPart] = labelCopy + i + 1;
			nPart++;
		}
	}

	/* Update directory */
	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	if(dir.Length > startByte)	/* only bother if some data were recorded */
	{
		p->typeNumber = 9 + (len/128)*256;	/* format and scan number */
		p->frameLength = 10016;	/* FIXME */
		switch(nPart)
		{
			case 1:
				strncpy(p->scanName, parts[0], MODULE_SCAN_NAME_LENGTH);
				break;
			case 2:
				strncpy(p->expName, parts[0], 8);
				strncpy(p->scanName, parts[1], MODULE_SCAN_NAME_LENGTH);
				break;
			case 3:
				strncpy(p->expName, parts[0], 8);
				strncpy(p->station, parts[1], 2);
				strncpy(p->scanName, parts[2], MODULE_SCAN_NAME_LENGTH);
				break;
		}

		decodeScan(xlrDevice, startByte, dir.Length, p, q);

		dirHeader->status = MODULE_STATUS_RECORDED;

		WATCHDOGTEST( XLRSetUserDir(xlrDevice, dirData, len+128) );

		for(int d = 0; d < 8; d++)
		{
			WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
			if(xlrRC == XLR_SUCCESS)
			{
				printf("Stats %d %u %u %u %u %u %u %u %u\n", d,
					(unsigned int)(driveStats[0].count),
					(unsigned int)(driveStats[1].count),
					(unsigned int)(driveStats[2].count),
					(unsigned int)(driveStats[3].count),
					(unsigned int)(driveStats[4].count),
					(unsigned int)(driveStats[5].count),
					(unsigned int)(driveStats[6].count),
					(unsigned int)(driveStats[7].count));
				fflush(stdout);
			}
		}
	}

	WATCHDOG( XLRClose(xlrDevice) );

	if(dirData)
	{
		free(dirData);
	}

	return 0;
}

int main(int argc, char **argv)
{
	int a, v, i;
	int bank = -1;
	int verbose = 0;
	int packetSize = defaultPacketSize;
	int dataFrameOffset = defaultDataFrameOffset;
	int payloadOffset = defaultPayloadOffset;
	int psnOffset = defaultPSNOffset;
	int psnMode = defaultPSNMode;
	int macFilterControl = defaultMACFilterControl;
	int retval = EXIT_SUCCESS;
	long long maxBytes = 1LL<<60;
	double maxSeconds = 1.0e12;
	char label[MaxLabelLength] = "";
	int statsRange[XLR_MAXBINS];

	for(int b = 0; b < XLR_MAXBINS; b++)
	{
		statsRange[b] = defaultStatsRange[b];
	}

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
			else if(a+1 < argc)
			{
				if(strcmp(argv[a], "-s") == 0 ||
				   strcmp(argv[a], "--packetsize") == 0)
				{
					packetSize = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-d") == 0 ||
				   strcmp(argv[a], "--dataframeoffset") == 0)
				{
					dataFrameOffset = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-p") == 0 ||
				   strcmp(argv[a], "--payloadoffset") == 0)
				{
					payloadOffset = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-o") == 0 ||
				   strcmp(argv[a], "--psnoffset") == 0)
				{
					psnOffset = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-m") == 0 ||
				   strcmp(argv[a], "--psnmode") == 0)
				{
					psnMode = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-f") == 0 ||
				   strcmp(argv[a], "--filtercontrol") == 0)
				{
					macFilterControl = atoi(argv[a+1]);
				}
				else if(strcmp(argv[a], "-b") == 0 ||
				   strcmp(argv[a], "--bytes") == 0)
				{
					maxBytes = atoll(argv[a+1]);
				}
				else if(strcmp(argv[a], "-t") == 0 ||
				   strcmp(argv[a], "--seconds") == 0)
				{
					maxSeconds = atof(argv[a+1]);
				}
				else if(strcmp(argv[a], "-r") == 0 ||
				   strcmp(argv[a], "--statsrange") == 0)
				{
					int n = sscanf(argv[a+1], "%d,%d,%d,%d,%d,%d,%d",
						statsRange+0, statsRange+1,
						statsRange+2, statsRange+3,
						statsRange+4, statsRange+5,
						statsRange+6);
					if(n != 7)
					{
						printf("The stats range option requires exactly 7 comma separated values\n");

						return EXIT_FAILURE;
					}
				}
				else
				{
					printf("Unknown option: %s\n", argv[a]);

					return EXIT_FAILURE;
				}
				a++;
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
				printf("Error scan name too long (%d > %d)\n", i, MaxLabelLength-1);

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
		printf("Error 3 incomplete command line\n");
		
		return EXIT_FAILURE;
	}

	v = initWatchdog();
	if(v < 0)
	{
		printf("Error 4 initWatchdog() failed.\n");

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
		v = record(bank, label, packetSize, payloadOffset, dataFrameOffset, psnOffset, psnMode, macFilterControl, maxBytes, maxSeconds, statsRange, verbose);
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

#else
/* Just print nocando */

int main()
{
	printf("Sorry, this only works on SDK9+ machines\n");

	return EXIT_FAILURE;
}

#endif
