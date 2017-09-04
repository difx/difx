/***************************************************************************
 *   Copyright (C) 2007-2017 by Walter Brisken                             *
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

#define _GNU_SOURCE

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <signal.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include <mark5access/mark5bfix.h>
#include "config.h"
#include "mark5dir.h"
#include "watchdog.h"

const char program[] = "mk5cp";
const char author[]  = "Walter Brisken";
const char version[] = "0.15";
const char verdate[] = "20170904";

const int defaultChunkSize = 50000000;

int verbose = 0;
volatile int die = 0;

/* Note: must use the less appropriate signal() rather than sigaction() call 
 * because streamstor library seems to use signal() and mixing the two
 * is bad. */
sighandler_t oldsiginthand;
sighandler_t oldsigtermhand;


const int MaxCommandLength = 400;

enum CopyMode
{
	COPY_MODE_NORMAL = 0,
	COPY_MODE_NODIR
};


void siginthand(int j)
{
	if(verbose)
	{
		fprintf(stderr, "Being killed (INT)\n");
	}
	die = 1;
}

void sigtermhand(int j)
{
	if(verbose)
	{
		fprintf(stderr, "Being killed (TERM)\n");
	}
	die = 1;
}

int usage(const char *pgm)
{
	int v;
	int cat = 0;

	v = strlen(pgm);
	if(v >= 6 && strcmp(pgm+v-6, "mk5cat") == 0)
	{
		cat = 1;
	}
	
	fprintf(stderr, "\n%s ver. %s   %s %s\n\n", cat ? "mk5cat" : program, version, author, verdate);
	fprintf(stderr, "A program to copy Mark5 module scans via XLR calls\n");
	fprintf(stderr, "\nUsage : %s [<options>] { <bank> | <vsn> } <scans>%s\n\n", pgm, cat ? "" : " <output path>");
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             Print this help message\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v             Be more verbose\n\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q             Be less verbose\n\n");
	fprintf(stderr, "  --fix5b\n");
	fprintf(stderr, "  -5             Perform packet reordering and filtering for Mark5B format\n");
	fprintf(stderr, "  --force\n");
	fprintf(stderr, "  -f             Continue even if dir is screwy\n\n");
	fprintf(stderr, "  --rt           Read using Real-Time mode\n\n");
	fprintf(stderr, "  --no-dir       Read a byte range without peek at module directory\n\n");
	fprintf(stderr, "  --small        Do read with a small (%d byte) chunk size\n\n", defaultChunkSize/5);
	fprintf(stderr, "  --large        Do read with a large (%d byte) chunk size\n\n", defaultChunkSize*5);
	fprintf(stderr, "  --raw          Read using a single XLRRead call (copy size must fit in RAM)\n\n");
	fprintf(stderr, "<bank> is either A or B or Active\n\n");
	fprintf(stderr, "<vsn> is a valid module VSN (8 characters)\n\n");
	fprintf(stderr, "<scans> is a string containing a list of scans to copy.  No whitespace\n    "
		"is allowed.  Ranges are allowed.  Examples:  1  or  3,5  or  1,3,6-9\n");
	fprintf(stderr, "The <scans> string can also be an MJD range to copy.\n  Example: 54321.112_54321.113\n");
	fprintf(stderr, "The <scans> string can also be a byte range to copy.\n  Example: 38612201536_38619201536\n");
	fprintf(stderr, "The byte range can be expressed as a start and length.\n  Example: 38612201536+7000000\n");
	fprintf(stderr, "The <scans> string can also take the special values 'last' and 'all'\n");
	if(!cat)
	{
		fprintf(stderr, "<output path> is a directory where files will be dumped\n");
		fprintf(stderr, "Environment variable MARK5_DIR_PATH should point to the location of\n");
		fprintf(stderr, "the module directories.  The output filename will be:\n");
		fprintf(stderr, "  $MARK5_DIR_PATH/<vsn>.dir\n");
		fprintf(stderr, "If <output path> is the hyphen (-), then all output goes to stdout\n\n");
	}
	fprintf(stderr, "<output path> can also be of the form [user@]host:directory in which case scp will be used\n\n");

	return 0;
}

int dirCallback(int scan, int nscan, int status, void *data)
{
	DifxMessageMk5Status *mk5status;

	mk5status = (DifxMessageMk5Status *)data;
	mk5status->scanNumber = scan;
	mk5status->position = nscan;
	snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s", Mark5DirDescription[status]);
	difxMessageSendMark5Status(mk5status);

	if(verbose)
	{
		fprintf(stderr, "%d/%d %d\n", scan, nscan, status);
	}

	return die;
}

int parsescp(char *sshLogin, char *sshDest, const char *path)
{
	int colon = -1;

	sshLogin[0] = 0;
	sshDest[0] = 0;

	for(int i = 0; path[i]; ++i)
	{
		if(path[i] == ':')
		{
			if(colon >= 0 || i == 0)
			{
				return -1;
			}
			colon = i;
		}
	}

	if(colon == -1)
	{
		return 0;
	}

	strncpy(sshLogin, path, colon);
	sshLogin[colon] = 0;
	strcpy(sshDest, path+colon+1);

	return 0;
}

int resetDriveStats(SSHANDLE xlrDevice)
{
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	const int defaultStatsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };

	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_DRVSTATS) );
	for(int b = 0; b < XLR_MAXBINS; ++b)
	{
		driveStats[b].range = defaultStatsRange[b];
		driveStats[b].count = 0;
	}
	WATCHDOGTEST( XLRSetDriveStats(xlrDevice, driveStats) );

	return 0;
}

int reportDriveStats(SSHANDLE xlrDevice, const char *vsn)
{
	XLR_RETURN_CODE xlrRC;
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	DifxMessageDriveStats driveStatsMessage;

	snprintf(driveStatsMessage.moduleVSN, DIFX_MESSAGE_MARK5_VSN_LENGTH, "%s", vsn);
	driveStatsMessage.type = DRIVE_STATS_TYPE_READ;

	/* FIXME: for now don't include complete information on drives */
	strcpy(driveStatsMessage.serialNumber, "X");
	strcpy(driveStatsMessage.modelNumber, "Y");
	driveStatsMessage.diskSize = 0;

	for(int d = 0; d < 8; ++d)
	{
		for(int i = 0; i < DIFX_MESSAGE_N_DRIVE_STATS_BINS; ++i)
		{
			driveStatsMessage.bin[i] = -1;
		}
		driveStatsMessage.moduleSlot = d;
		WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
		if(xlrRC == XLR_SUCCESS)
		{
			for(int i = 0; i < XLR_MAXBINS; ++i)
			{
				if(i < DIFX_MESSAGE_N_DRIVE_STATS_BINS)
				{
					driveStatsMessage.bin[i] = driveStats[i].count;
				}
			}
		}
		difxMessageSendDriveStats(&driveStatsMessage);
	}

	resetDriveStats(xlrDevice);

	return 0;
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
		if(bank_stat.Selected)
		{
			mk5status->activeBank = 'A';
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
		if(bank_stat.Selected)
		{
			mk5status->activeBank = 'B';
		}
	}

	return 0;
}

/* if doAppend != 0 and the output file exists, this tries to start from where a previous attempt left off */
static FILE *openOutputFile(const char *filename, int doAppend, long long *byteStart, long long byteStop)
{
	if(doAppend)
	{
		struct stat st;
		int ok;

		ok = stat(filename, &st);
		if(ok == 0)	/* file exists */
		{
			off_t size;

			size = st.st_size;

			if(size >= (byteStop - *byteStart))
			{
				*byteStart += size;

				/* The caller should look at the value of *byteStart after returning.  If *byteStart = byteStop, then this was successful as no effort was needed. */
				
				return 0;
			}
			else
			{
				if(size % 8 != 0)
				{
					/* non multiple of 8 bytes in file.  Must truncate it */
					size -= (size % 8);

					truncate(filename, size);
				}

				*byteStart += size;

				return fopen(filename, "a");
			}
		}
		else
		{
			return fopen(filename, "w");
		}
	}
	else
	{
		return fopen(filename, "w");
	}
}

int copyByteRange(SSHANDLE xlrDevice, const char *outPath, const char *outName, int scanNum, long long byteStart, long long byteStop, DifxMessageMk5Status *mk5status, int chunkSize, long long *nGoodBytes, long long *nReplacedBytes, int doAppend)
{
	FILE *out;
	long long readptr;
	long long togo;
	int len;
	streamstordatatype *data;
	int a, b, v;
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	struct timeval t0, t1, t2;
	double dt;
	double rate;
	char message[DIFX_MESSAGE_LENGTH];
	long long wGood=0, wBad=0;
	int skip = 0;
	char scpLogin[DIFX_MESSAGE_FILENAME_LENGTH];
	char scpDest[DIFX_MESSAGE_FILENAME_LENGTH];
	int nReread = 0;
	long long origByteStart;

	parsescp(scpLogin, scpDest, outPath);

	if(byteStart % 8 != 0)
	{
		skip = byteStart % 8;
		byteStart -= (byteStart % 8);
	}
	if(byteStop % 8 != 0)
	{
		byteStop += (8-byteStart % 8);
	}

	origByteStart = byteStart;

	if(strcmp(outPath, "-") == 0)
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s", outName); 
		out = stdout;
	}
	else if(scpLogin[0])
	{
		char cmd[MaxCommandLength];

		if(scpDest[0])
		{
			snprintf(cmd, MaxCommandLength, "ssh %s 'cat - > %s/%s'", scpLogin, scpDest, outName);
		}
		else
		{
			snprintf(cmd, MaxCommandLength, "ssh %s 'cat - > %s'", scpLogin, outName);
		}
		out = popen(cmd, "w");
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open pipe %s for write.  Check permissions!", cmd);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}
	else
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", outPath, outName); 
		out = openOutputFile(filename, doAppend, &byteStart, byteStop);
		if(!out)
		{
			if(doAppend)
			{
				/* look for some special cases that can come out of append mode */
				if(byteStart == byteStop)  /* This means the existing file was the right size.  Declare success. */
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "File %s exists with the correct size.  Not recopying.", filename);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
					fprintf(stderr, "%s\n", message);

					return 0;	/* success */
				}
				else if(byteStart > byteStop)	/* larger than expected destination file exists.  This is not good. */
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "File %s exists with size larger than expected.  Copying will not proceed.", filename);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "%s\n", message);

					return -1;	/* success */
				}
			}
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open file %s for write.  Check permissions!", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}

	fprintf(stderr, "outName = %s\n", filename);

	if(chunkSize <= 0)
	{
		chunkSize = byteStop - byteStart;
	}

	readptr = byteStart;
	togo = byteStop-byteStart;
	data = (streamstordatatype *)malloc(chunkSize);
	len = chunkSize;

	mk5status->status = MARK5_COPY_SUCCESS;
	mk5status->scanNumber = scanNum+1;

	if(verbose)
	{
		fprintf(stderr, "Writing %s\n", filename);
		fprintf(stderr, "start/length = %Ld/%Ld\n", byteStart, byteStop-byteStart);
	}

	rate = 0.0;
	gettimeofday(&t0, 0);
	gettimeofday(&t1, 0);

	if(byteStart > origByteStart)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Copying portion (%Ld, %Ld) of scan %d to file %s (appending: %Ld bytes previously copied)", byteStart, byteStop, scanNum+1, filename, byteStart - origByteStart);
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Copying portion (%Ld, %Ld) of scan %d to file %s", byteStart, byteStop, scanNum+1, filename);
	}
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

	for(int i = 0; togo > 0; ++i)
	{
		XLR_RETURN_CODE xlrRC;

		if(die)
		{
			difxMessageSendDifxAlert("Data copy aborted due to die signal", DIFX_ALERT_LEVEL_WARNING);
			
			break;
		}
		snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "[%Ld%%]", 100*(readptr-byteStart)/(byteStop-byteStart));
		mk5status->position = readptr;
		mk5status->rate = rate;
		difxMessageSendMark5Status(mk5status);
		if(togo < chunkSize)
		{
			if(togo % 8 != 0)
			{
				togo -= (togo % 8);
			}
			len = togo;
		}
		if(verbose)
		{
			fprintf(stderr, "%Ld = %Ld/%Ld %Ld %d\n", readptr, readptr-byteStart, byteStop-byteStart, togo, len);
		}

		a = readptr >> 32;
		b = readptr % (1LL<<32);

		WATCHDOG( xlrRC = XLRReadData(xlrDevice, data, a, b, len) );
		if(xlrRC != XLR_SUCCESS)
		{

			fprintf(stderr, "Read error: position=%Ld trying one more time:\n", readptr);
			sleep(2);
			++nReread;
			WATCHDOG( xlrRC = XLRReadData(xlrDevice, data, a, b, len) );

			if(xlrRC != XLR_SUCCESS)
			{
				fprintf(stderr, "Read error: position=%Ld, length=%d\n", readptr, len);
				if(out != stdout)
				{
					fclose(out);
				}

				return -1;
			}
		}

		countReplaced2(data, len/4, &wGood, &wBad);

		v = fwrite(((char *)data)+skip, 1, len-skip, out)+skip;

		if(v < len)
		{
			if(out == stdout)
			{
				fprintf(stderr, "mk5cp: Broken pipe.\n");
			}
			else if(scpLogin[0])
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Bad password or broken scp pipe.");
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			else
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Disk full?  path=%s", outPath);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			
			break;
		}
		gettimeofday(&t2, 0);
		dt = (t2.tv_sec - t1.tv_sec) + 1.0e-6*(t2.tv_usec - t1.tv_usec);

		if(dt > 0.0)
		{
			rate = 8.0e-6*chunkSize/dt; /* Mbps */
		}
		t1 = t2;

		dt = t2.tv_sec - t0.tv_sec;

		if(dt >= 10)
		{
			t0 = t2;
			
			v = getBankInfo(xlrDevice, mk5status, mk5status->activeBank == 'B' ? 'A' : 'B');
			if(v < 0)
			{
				return v;
			}
		}

		readptr += chunkSize;
		togo -= len;
		skip = 0;
	}

	if(scpLogin[0])
	{
		pclose(out);
	}
	else if(out != stdout)
	{
		fclose(out);
	}
	free(data);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Copied scan %d. %Ld bytes total, %Ld bytes replaced.", scanNum+1, 4*(wGood+wBad), 4*wBad);
	if((double)wBad/(double)wGood < 1.0e-8)
	{
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		fprintf(stderr, "%s\n", message);
	}
	else
	{
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
		fprintf(stderr, "Warning: %s\n", message);
	}
	if(nReread > 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "%d rereads were done.", nReread);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		fprintf(stderr, "Note: %s\n", message);
	}

	*nGoodBytes += 4*wGood;
	*nReplacedBytes += 4*wBad;

	mk5status->scanNumber = 0;
	mk5status->rate = 0.0;
	mk5status->position = byteStop;
	difxMessageSendMark5Status(mk5status);

	if(togo > 0)
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

int copyScanFix5B(SSHANDLE xlrDevice, const char *vsn, const char *outPath, int scanNum, const Mark5Scan *scan, DifxMessageMk5Status *mk5status, int chunkSize, long long *nGoodBytes, long long *nReplacedBytes)
{
	FILE *out;
	int destSize;
	long long readptr;
	long long togo;
	int len;
	streamstordatatype *data;
	unsigned char *fixed;
	int a, b, v;
	long long wGood=0, wBad=0;
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	struct timeval t0, t1, t2;
	double dt;
	double rate;
	char message[DIFX_MESSAGE_LENGTH];
	char scpLogin[DIFX_MESSAGE_FILENAME_LENGTH];
	char scpDest[DIFX_MESSAGE_FILENAME_LENGTH];
	int startFrame = -1;
	int leftover = 0;
	struct mark5b_fix_statistics stats;

	if(scan->format != 2)
	{
		fprintf(stderr, "Error: Scan %d (%s) is not in Mark5B format!\n", scanNum+1, scan->name.c_str());

		return -1;
	}

	resetmark5bfixstatistics(&stats);
	
	parsescp(scpLogin, scpDest, outPath);

	destSize = chunkSize*2;

	if(strcmp(outPath, "-") == 0)
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%04d_%s", vsn, scanNum+1, scan->name.c_str()); 
		out = stdout;
	}
	else if(scpLogin[0])
	{
		char cmd[MaxCommandLength];

		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%04d_%s", vsn, scanNum+1, scan->name.c_str()); 

		if(scpDest[0])
		{
			snprintf(cmd, MaxCommandLength, "ssh %s 'cat - > %s/%s'", scpLogin, scpDest, filename);
		}
		else
		{
			snprintf(cmd, MaxCommandLength, "ssh %s 'cat - > %s'", scpLogin, filename);
		}
		out = popen(cmd, "w");
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open pipe %s for write.  Check permissions!", cmd);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}
	else
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%8s_%04d_%s", outPath, vsn, scanNum+1, scan->name.c_str()); 
		out = fopen(filename, "w");
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open file %s for write.  Check permissions!", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}

	fprintf(stderr, "outName = %s\n", filename);

	readptr = scan->start;
	togo = scan->length;
	data = (streamstordatatype *)malloc(chunkSize);
	fixed = (unsigned char *)malloc(destSize);
	len = chunkSize;

	mk5status->status = MARK5_COPY_SUCCESS;
	mk5status->scanNumber = scanNum+1;

	if(verbose)
	{
		fprintf(stderr, "Writing %s\n", filename);
		fprintf(stderr, "start/length = %Ld/%Ld\n", scan->start, scan->length);
	}

	rate = 0.0;
	gettimeofday(&t0, 0);
	gettimeofday(&t1, 0);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Copying scan %d to file %s", scanNum+1, filename);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

	for(int i = 0; togo > 0; ++i)
	{
		XLR_RETURN_CODE xlrRC;

		if(die)
		{
			difxMessageSendDifxAlert("Data copy aborted due to die signal", DIFX_ALERT_LEVEL_WARNING);
			
			break;
		}
		snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s[%Ld%%]", scan->name.c_str(), 100*(readptr-scan->start)/scan->length);
		mk5status->position = readptr;
		mk5status->rate = rate;
		difxMessageSendMark5Status(mk5status);
		if(togo < chunkSize-leftover)
		{
			len = togo;
		}
		else
		{
			len = chunkSize-leftover;
		}
		if(verbose)
		{
			fprintf(stderr, "%Ld = %Ld/%Ld %Ld %d\n", readptr, readptr-scan->start, scan->length, togo, len);
		}

		a = readptr >> 32;
		b = readptr % (1LL<<32);

		WATCHDOG( xlrRC = XLRReadData(xlrDevice, data + leftover/sizeof(streamstordatatype), a, b, len) );
		if(xlrRC != XLR_SUCCESS)
		{
			fprintf(stderr, "Read error: position=%Ld, length=%d\n", readptr, len);
			if(out != stdout)
			{
				fclose(out);
			}

			return -1;
		}

		countReplaced2(data, len/4, &wGood, &wBad);

		v = mark5bfix(fixed, destSize, (unsigned char *)data, len + leftover, scan->framespersecond, startFrame, &stats);
		if(v < 0)
		{
			fprintf(stderr, "Error: mark5bfix returned %d\n", v);

			return -1;
		}

		startFrame = stats.startFrameNumber + stats.destUsed/10016;

		v = fwrite(fixed, 1, stats.destUsed, out);
		if(v < stats.destUsed)
		{
			if(out == stdout)
			{
				fprintf(stderr, "mk5cp: Broken pipe.\n");
			}
			else if(scpLogin[0])
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Bad password or broken scp pipe.");
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			else
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Disk full?  path=%s", outPath);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			
			break;
		}

		leftover = stats.srcSize - stats.srcUsed;
		if(leftover > 0)
		{
			memmove((unsigned char *)data, ((unsigned char *)data)+stats.srcSize-leftover, leftover);
		}

		gettimeofday(&t2, 0);
		dt = (t2.tv_sec-t1.tv_sec) + 1.0e-6*(t2.tv_usec-t1.tv_usec);

		if(dt > 0.0)
		{
			rate = 8.0e-6*chunkSize/dt; /* Mbps */
		}
		t1 = t2;

		dt = t2.tv_sec-t0.tv_sec;

		if(dt >= 10)
		{
			t0 = t2;
			
			v = getBankInfo(xlrDevice, mk5status, mk5status->activeBank == 'B' ? 'A' : 'B');
			if(v < 0)
			{
				return v;
			}
		}

		readptr += len;
		togo -= len;
	}

	if(scpLogin[0])
	{
		pclose(out);
	}
	else if(out != stdout)
	{
		fclose(out);
	}
	free(data);
	free(fixed);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Copied scan %d = %s.", scanNum+1, scan->name.c_str());
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	fprintf(stderr, "%s\n", message);
	fprintmark5bfixstatistics(stderr, &stats);
	fprintf(stderr, "\n");

	*nGoodBytes += 4*wGood;
	*nReplacedBytes += 4*wBad;

	mk5status->scanNumber = 0;
	mk5status->rate = 0.0;
	mk5status->position = scan->start + scan->length;
	difxMessageSendMark5Status(mk5status);

	if(togo > 0)
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

int copyScan(SSHANDLE xlrDevice, const char *vsn, const char *outPath, int scanNum, const Mark5Scan *scan, DifxMessageMk5Status *mk5status, int chunkSize, long long *nGoodBytes, long long *nReplacedBytes, int doAppend)
{
	FILE *out;
	long long readptr;
	long long togo;
	int len, skip;
	streamstordatatype *data;
	int a, b, v;
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	struct timeval t0, t1, t2;
	double dt;
	double rate;
	char message[DIFX_MESSAGE_LENGTH];
	long long wGood=0, wBad=0;
	char scpLogin[DIFX_MESSAGE_FILENAME_LENGTH];
	char scpDest[DIFX_MESSAGE_FILENAME_LENGTH];
	long long byteStart;
	long long byteStop;

	parsescp(scpLogin, scpDest, outPath);

	byteStart = scan->start;
	byteStop = scan->start + scan->length;

	if(strcmp(outPath, "-") == 0)
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%04d_%s", vsn, scanNum+1, scan->name.c_str()); 
		out = stdout;
	}
	else if(scpLogin[0])
	{
		char cmd[MaxCommandLength];

		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%04d_%s", vsn, scanNum+1, scan->name.c_str()); 

		if(scpDest[0])
		{
			snprintf(cmd, MaxCommandLength, "ssh %s 'cat - > %s/%s'", scpLogin, scpDest, filename);
		}
		else
		{
			snprintf(cmd, MaxCommandLength, "ssh %s 'cat - > %s'", scpLogin, filename);
		}
		out = popen(cmd, "w");
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open pipe %s for write.  Check permissions!", cmd);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}
	else
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%8s_%04d_%s", outPath, vsn, scanNum+1, scan->name.c_str()); 
		out = openOutputFile(filename, doAppend, &byteStart, byteStop);
		if(!out)
		{
			if(doAppend)
			{
				/* look for some special cases that can come out of append mode */
				if(byteStart == byteStop)  /* This means the existing file was the right size.  Declare success. */
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "File %s exists with the correct size.  Not recopying.", filename);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
					fprintf(stderr, "%s\n", message);

					return 0;	/* success */
				}
				else if(byteStart > byteStop)	/* larger than expected destination file exists.  This is not good. */
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "File %s exists with size larger than expected.  Copying will not proceed.", filename);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "%s\n", message);

					return -1;	/* success */
				}
			}
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open file %s for write.  Check permissions!", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}

	fprintf(stderr, "outName = %s\n", filename);

	readptr = byteStart;
	togo = byteStop - byteStart;
	data = (streamstordatatype *)malloc(chunkSize);
	len = chunkSize;

	mk5status->status = MARK5_COPY_SUCCESS;
	mk5status->scanNumber = scanNum+1;

	if(verbose)
	{
		fprintf(stderr, "Writing %s\n", filename);
		fprintf(stderr, "start/length = %Ld/%Ld\n", readptr, togo);
	}

	rate = 0.0;
	gettimeofday(&t0, 0);
	gettimeofday(&t1, 0);

	if(byteStart > scan->start)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Copying scan %d to file %s (appending: %Ld bytes previously copied)", scanNum+1, filename, byteStart - scan->start);
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Copying scan %d to file %s", scanNum+1, filename);
	}
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

	for(int i = 0; togo > 0; ++i)
	{
		XLR_RETURN_CODE xlrRC;

		if(die)
		{
			difxMessageSendDifxAlert("Data copy aborted due to die signal", DIFX_ALERT_LEVEL_WARNING);
			
			break;
		}
		snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s[%Ld%%]", scan->name.c_str(), 100*(readptr-scan->start)/scan->length);
		mk5status->position = readptr;
		mk5status->rate = rate;
		difxMessageSendMark5Status(mk5status);
		if(togo < chunkSize)
		{
			len = togo;
		}
		if(verbose)
		{
			fprintf(stderr, "%Ld = %Ld/%Ld %Ld %d\n", readptr, readptr-scan->start, scan->length, togo, len);
		}

		a = readptr >> 32;
		b = readptr % (1LL<<32);

		WATCHDOG( xlrRC = XLRReadData(xlrDevice, data, a, b, len) );
		if(xlrRC != XLR_SUCCESS)
		{
			if(die)
			{
				break;
			}
			fprintf(stderr, "Read error: position=%Ld, length=%d\n", readptr, len);
			if(out != stdout)
			{
				fclose(out);
			}

			return -1;
		}

		countReplaced2(data, len/4, &wGood, &wBad);

		if(i == 0)
		{
			skip = scan->frameoffset;
		}
		else
		{
			skip = 0;
		}

		v = fwrite(data+(skip/4), 1, len-skip, out);
		if(v < len-skip)
		{
			if(out == stdout)
			{
				fprintf(stderr, "mk5cp: Broken pipe.\n");
			}
			else if(scpLogin[0])
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Bad password or broken scp pipe.");
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			else
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Disk full?  path=%s", outPath);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			
			break;
		}
		gettimeofday(&t2, 0);
		dt = (t2.tv_sec-t1.tv_sec) + 1.0e-6*(t2.tv_usec-t1.tv_usec);

		if(dt > 0.0)
		{
			rate = 8.0e-6*(chunkSize-skip)/dt; /* Mbps */
		}
		t1 = t2;

		dt = t2.tv_sec-t0.tv_sec;

		if(dt >= 10)
		{
			t0 = t2;
			
			v = getBankInfo(xlrDevice, mk5status, mk5status->activeBank == 'B' ? 'A' : 'B');
			if(v < 0)
			{
				return v;
			}
		}

		readptr += chunkSize;
		togo -= len;
	}

	if(scpLogin[0])
	{
		pclose(out);
	}
	else if(out != stdout)
	{
		fclose(out);
	}
	free(data);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Copied scan %d. %Ld bytes total, %Ld bytes replaced.", scanNum+1, 4*(wGood+wBad), 4*wBad);
	if((double)wBad/(double)wGood < 1.0e-8)
	{
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		fprintf(stderr, "%s\n", message);
	}
	else
	{
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
		fprintf(stderr, "Warning: %s\n", message);
	}

	*nGoodBytes += 4*wGood;
	*nReplacedBytes += 4*wBad;

	mk5status->scanNumber = 0;
	mk5status->rate = 0.0;
	mk5status->position = scan->start + scan->length;
	difxMessageSendMark5Status(mk5status);

	if(togo > 0)
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

/* Return 1 on success */
static int parseMjdRange(double *mjdStart, double *mjdStop, const char *scanList)
{
	if(sscanf(scanList, "%lf_%lf", mjdStart, mjdStop) != 2)
	{
		return 0;
	}

	if(*mjdStart < 10000.0 || *mjdStart > 100000.0)
	{
		return 0;
	}
	if(*mjdStop < 10000.0 || *mjdStop > 100000.0)
	{
		return 0;
	}
	if( (*mjdStart >= *mjdStop) && (*mjdStop - *mjdStart < 3) )
	{
		return 0;
	}

	return 1;
}

/* Return 1 on success */
static int parseByteRange(long long *start, long long *stop, const char *scanList)
{
	if(sscanf(scanList, "%Ld_%Ld", start, stop) == 2)
	{
		if(*start >= 0 && *start < *stop)
		{
			return 1;
		}
		else
		{
			return 0;
		}
	}
	else if(sscanf(scanList, "%Ld+%Ld", start, stop) == 2)
	{
		if(*start >= 0 && *stop > 0)
		{
			*stop += *start;

			return 1;
		}
		else
		{
			return 0;
		}
	}
	else
	{
		return 0;
	}
}

static int mk5cp(char *vsn, const char *scanList, const char *outPath, int force, enum Mark5ReadMode readMode, int chunkSize, int fix5b, int doAppend)
{
	int mjdNow;
	const char *mk5dirpath;
	int v;
	int b, s, nGood, nBad;
	long long nGoodBytes = 0, nReplacedBytes = 0;
	time_t startTime, endTime;
	int bank = -1;
	float replacedFrac;
	int bail = 0;
	double mjdStart, mjdStop;
	long long byteStart, byteStop;
	Mark5Scan *scan;
	char outName[DIFX_MESSAGE_FILENAME_LENGTH];
	char scanrangestr[DIFX_MESSAGE_PARAM_LENGTH];
	char message[DIFX_MESSAGE_LENGTH];
	Mark5Module module;
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	DifxMessageMk5Status mk5status;

	oldsiginthand = signal(SIGINT, siginthand);
	oldsigtermhand = signal(SIGTERM, sigtermhand);

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );

	startTime = time(0);

	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		return v;
	}
	if(mk5status.activeBank == ' ')
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "No module inserted");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);

		return -1;
	}

	if(strcmp(vsn, "Active") == 0)
	{
		vsn[0] = mk5status.activeBank;
		vsn[1] = 0;
	}

	if(strcasecmp(vsn, "A") == 0)
	{
		mk5status.activeBank = 'A';
		strncpy(vsn, mk5status.vsnA, 8);
		vsn[8] = 0;
	}
	else if(strcasecmp(vsn, "B") == 0)
	{
		mk5status.activeBank = 'B';
		strncpy(vsn, mk5status.vsnB, 8);
		vsn[8] = 0;
	}

	bank = Mark5BankSetByVSN(xlrDevice, vsn);

	if(bank < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Bank set error %d\n", bank);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		return -1;
	}

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = ".";
	}
	
	mjdNow = (int)(40587.0 + time(0)/86400.0);

	mk5status.state = MARK5_STATE_GETDIR;
	difxMessageSendMark5Status(&mk5status);

	if(strlen(vsn) != 8)
	{
		fprintf(stderr, "Developer error: vsn length is not 8\n");
		bail = 1;
	}
	else
	{
		v = module.getCachedDirectory(xlrDevice, mjdNow, 
			vsn, mk5dirpath, &dirCallback, &mk5status,
			&replacedFrac, false, 0, 1, -1, -1);
/*
		if(replacedFrac > 0.01)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module %s directory read encountered %4.2f%% data replacement rate",
				vsn, replacedFrac);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "Warning: %s\n", message);
		}
*/
		if(v < 0)
		{
			if(v == DIRECTORY_NOT_CACHED)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH,
					"Directory cache not up to date for module %s.  Please get directory first.",
					vsn);
			}
			else
			{
				snprintf(message, DIFX_MESSAGE_LENGTH,
					"Unsuccessful dir read for module %s.  Return value = %d\n", vsn, v);
			}
			fprintf(stderr, "%s\n", message);
			fprintf(stderr, "\n%s\n", module.error.str().c_str());
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			mk5status.activeBank = ' ';
			bail = 1;
		}
		else if(verbose > 0)
		{
			module.print();
		}

		if(v >= 0)
		{
			v = module.sanityCheck();
			if(v < 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Module %s directory contains undecoded scans!", vsn);
				fprintf(stderr, "%s\n", message);
				if(!force)
				{
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					mk5status.activeBank = ' ';
					bail = 1;
				}
				else
				{
					fprintf(stderr, "Force is set, so continuing anyway\n");
				}
			}
		}

		if(module.mode == MARK5_READ_MODE_RT || readMode == MARK5_READ_MODE_RT)
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
	}

	if(bail)
	{
		fprintf(stderr, "Bailing!\n");

		return -1;
	}

	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
	if(!bank_stat.Selected)
	{
		WATCHDOGTEST( XLRSelectBank(xlrDevice, bank) );
	}

	mk5status.state = MARK5_STATE_COPY;

	nGood = 0;
	nBad = 0;

	/* If "all" is provided as the list of scans, then change scan list to be all scans */
	if(strcasecmp(scanList, "all") == 0)
	{
		snprintf(scanrangestr, DIFX_MESSAGE_PARAM_LENGTH, "1-%d", module.nScans());
		scanList = scanrangestr;
	}
	/* if "last" is provided, then change the scan list to be just that scan */
	else if(strcasecmp(scanList, "last") == 0)
	{
		snprintf(scanrangestr, DIFX_MESSAGE_PARAM_LENGTH, "%d", module.nScans());
		scanList = scanrangestr;
	}

	resetDriveStats(xlrDevice);

	if(mk5status.activeBank > ' ' && bail < 1) 
	{
		/* first look for mjd range */
		if(parseMjdRange(&mjdStart, &mjdStop, scanList))	
		{
			if(fix5b)
			{
				fprintf(stderr, "Error: fixing Mark5B while copying a time range is not supported\n\n");

				return -1;
			}

			for(int scanIndex = 0; scanIndex < module.nScans() && !die; ++scanIndex)
			{
				scan = &module.scans[scanIndex];
				if(!getByteRange(scan, &byteStart, &byteStop, mjdStart, mjdStop))
				{
					continue;
				}
				snprintf(outName, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%s_%d", module.label.c_str(), scanList, nGood+nBad);
				v = copyByteRange(xlrDevice, outPath, outName, scanIndex, byteStart, byteStop, &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes, doAppend);
				if(v == 0)
				{
					++nGood;
				}
				else
				{
					if(watchdogXLRError[0] != 0)
					{
						return v;
					}
					++nBad;
				}
			}
			if(nGood == 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "MJD range %12.6f to %12.6f not in any scan", mjdStart, mjdStop);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
			
		}
		/* next look for byte range */
		else if(parseByteRange(&byteStart, &byteStop, scanList))
		{
			if(fix5b)
			{
				fprintf(stderr, "Error: fixing Mark5B while copying a byte range is not supported\n\n");

				return -1;
			}

			snprintf(outName, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%s", module.label.c_str(), scanList);
			v = copyByteRange(xlrDevice, outPath, outName, -1, byteStart, byteStop, &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes, doAppend);

			if(v == 0)
			{
				++nGood;
			}
			else
			{
				if(watchdogXLRError[0] != 0)
				{
					return v;
				}
				++nBad;
			}
			if(nGood == 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Byte range %Ld to %Ld not in any scan", byteStart, byteStop);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
		}
		/* next look for scan range */
		else if(isdigit(scanList[0])) for(;;)
		{
			int a;

			fprintf(stderr, "scanList = %s\n", scanList);

			v = sscanf(scanList, "%d%n", &a, &s);
			scanList += s;
			if(v < 1)
			{
				break;
			}
			if(scanList[0] == '-')
			{
				++scanList;
				v = sscanf(scanList, "%d%n", &b, &s);
				scanList += s;
				if(v < 1)
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "Bad format for list of scans");
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "Error: %s\n", message);
					break;
				}
			}
			else
			{
				b = a;
			}

			fprintf(stderr, "reading %d to %d\n", a, b);

			if(a >= 0 && b >= a && b < 1000000)
			{
				for(int i = a; i <= b && !die; ++i)
				{
					if(i > 0 && i <= module.nScans())
					{
						int scanIndex = i-1;
						if(fix5b)
						{
							v = copyScanFix5B(xlrDevice, module.label.c_str(), outPath, scanIndex, &module.scans[scanIndex], &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes);
						}
						else
						{
							v = copyScan(xlrDevice, module.label.c_str(), outPath, scanIndex, &module.scans[scanIndex], &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes, doAppend);
						}
						if(v == 0)
						{
							++nGood;
						}
						else
						{
							if(watchdogXLRError[0] != 0)
							{
								return v;
							}
							++nBad;
						}
					}
					else
					{
						snprintf(message, DIFX_MESSAGE_LENGTH, "Scan number %d out of range.  nScan = %d", i, module.nScans());
						difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
						fprintf(stderr, "Warning: %s\n", message);
						++nBad;
					}
				}
			}
			else
			{
				printf("Scan range %d to %d seems suspicious.  Not trying.\n", a, b);
			}

			if(scanList[0] == 0)
			{
				break;
			}
			else
			{
				++scanList;
			}
		}
		/* finally, look for scan name */
		else
		{
			int l = strlen(scanList);
			// FIXME: use iterator
			for(int i = 0; i < module.nScans() && !die; ++i)
			{
				if(strncasecmp(module.scans[i].name.c_str(), scanList, l) == 0)
				{
					int scanIndex = i;

					if(fix5b)
					{
						v = copyScanFix5B(xlrDevice, module.label.c_str(), outPath, scanIndex, &module.scans[scanIndex], &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes);
					}
					else
					{
						v = copyScan(xlrDevice, module.label.c_str(), outPath, scanIndex, &module.scans[scanIndex], &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes, doAppend);
					}
					if(v == 0) 
					{
						++nGood;
					}
					else
					{
						if(watchdogXLRError[0] != 0)
						{
							return v;
						}
						++nBad;
					}
				}
			}
		}

		if(nGood > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "%d scans copied from module %8s to %s", nGood, module.label.c_str(), outPath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			fprintf(stderr, "%s\n", message);
		}
		if(nBad > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "%d scans NOT copied from module %8s to %s", nBad, module.label.c_str(), outPath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "%s\n", message);
		}
		if(nGood == 0 && nBad == 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "No scans match with code %s on module %8s", scanList, module.label.c_str());
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "%s\n", message);
		}

		endTime = time(0);

		if(endTime > startTime)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "A total of %lld bytes were copied in %lld seconds, copy rate was %3.1f Mbps\n", (nGoodBytes + nReplacedBytes), (long long)(endTime-startTime), 8.0e-6*((nGoodBytes + nReplacedBytes)/(endTime-startTime)));
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			fprintf(stderr, "%s\n", message);
			if(nGood > 1 && nReplacedBytes > 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "A total of %lld bytes were replaced by Mark5 fill pattern\n", nReplacedBytes);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
				fprintf(stderr, "%s\n", message);
			}
		}

		if(die == 0)
		{
			reportDriveStats(xlrDevice, vsn);
		}
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot find vsn=%s or get its directory", vsn);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
	}

	WATCHDOG( XLRClose(xlrDevice) );

	/* Send final "IDLE" state message so everyone knows we're done */
	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = module.nScans();
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

static int mk5cp_nodir(char *vsn, const char *scanList, const char *outPath, int force, enum Mark5ReadMode readMode, int chunkSize, int doAppend)
{
	int v;
	int nGood, nBad;
	int bank = -1;
	int bail = 0;
	long long byteStart, byteStop;
	long long nGoodBytes = 0, nReplacedBytes = 0;
	char outName[DIFX_MESSAGE_FILENAME_LENGTH];
	time_t startTime, endTime;
	char message[DIFX_MESSAGE_LENGTH];
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	DifxMessageMk5Status mk5status;

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );

	startTime = time(0);

	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		return v;
	}
	if(mk5status.activeBank == ' ')
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "No module inserted");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);

		return -1;
	}

	if(strcmp(vsn, "Active") == 0)
	{
		vsn[0] = mk5status.activeBank;
		vsn[1] = 0;
	}

	if(strcasecmp(vsn, "A") == 0)
	{
		mk5status.activeBank = 'A';
		strncpy(vsn, mk5status.vsnA, 8);
		vsn[8] = 0;
	}
	else if(strcasecmp(vsn, "B") == 0)
	{
		mk5status.activeBank = 'B';
		strncpy(vsn, mk5status.vsnB, 8);
		vsn[8] = 0;
	}

	bank = Mark5BankSetByVSN(xlrDevice, vsn);

	if(bank < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Bank set error %d\n", bank);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		return -1;
	}

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

	if(bail)
	{
		fprintf(stderr, "Bailing!\n");

		return -1;
	}

	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
	if(!bank_stat.Selected)
	{
		WATCHDOGTEST( XLRSelectBank(xlrDevice, bank) );
	}

	mk5status.state = MARK5_STATE_COPY;

	nGood = 0;
	nBad = 0;

	resetDriveStats(xlrDevice);

	if(mk5status.activeBank > ' ' && bail < 1) 
	{
		if(parseByteRange(&byteStart, &byteStop, scanList))
		{
			snprintf(outName, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%s", vsn, scanList);
			v = copyByteRange(xlrDevice, outPath, outName, -1, byteStart, byteStop, &mk5status, chunkSize, &nGoodBytes, &nReplacedBytes, doAppend);

			if(v == 0)
			{
				++nGood;
			}
			else
			{
				if(watchdogXLRError[0] != 0)
				{
					return v;
				}
				++nBad;
			}
			if(nGood == 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Byte range %Ld to %Ld not in any scan", byteStart, byteStop);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
		}
		else
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Non-directory mode only allows start_stop or start+inc to be specified\n");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			fprintf(stderr, "%s\n", message);
		}

		if(nGood > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "%d scans copied from module %8s to %s", nGood, vsn, outPath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			fprintf(stderr, "%s\n", message);
		}
		if(nBad > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "%d scans NOT copied from module %8s to %s", nBad, vsn, outPath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "%s\n", message);
		}

		endTime = time(0);

		if(endTime > startTime)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "A total of %lld bytes were copied in %lld seconds, copy rate was %3.1f Mbps\n", (nGoodBytes + nReplacedBytes), (long long)(endTime-startTime), 8.0e-6*((nGoodBytes + nReplacedBytes)/(endTime-startTime)));
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			fprintf(stderr, "%s\n", message);
			if(nGood > 1 && nReplacedBytes > 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "A total of %lld bytes were replaced by Mark5 fill pattern\n", nReplacedBytes);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
				fprintf(stderr, "%s\n", message);
			}
		}

		if(die == 0)
		{
			reportDriveStats(xlrDevice, vsn);
		}
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot find vsn=%s", vsn);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
	}

	WATCHDOG( XLRClose(xlrDevice) );

	/* Send final "IDLE" state message so everyone knows we're done */
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
	char vsn[16] = "";
	const char *scanList=0;
	const char *outPath=0;
	int force = 0;
	int fix5b = 0;
	int v;
	int lockWait = 10;	/* [sec] set to MARK5_LOCK_WAIT_FOREVER if immediate quit on lock behavior is wanted */
	int chunkSize = defaultChunkSize;
	enum CopyMode copyMode = COPY_MODE_NORMAL;
	enum Mark5ReadMode readMode = MARK5_READ_MODE_NORMAL;
	int doAppend = 0;

	v = strlen(argv[0]);
	if(v >= 6 && strcmp(argv[0]+v-6, "mk5cat") == 0)
	{
		difxMessageInit(-1, "mk5cat");
		outPath = "-";
	}
	else
	{
		difxMessageInit(-1, program);
	}

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	for(int a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-") == 0)
		{
			if(scanList != 0 && outPath == 0)
			{
				outPath = argv[a];
			}
			else
			{
				return usage(argv[0]);
			}
		}
		else if(strcmp(argv[a], "-h") == 0 ||
			strcmp(argv[a], "--help") == 0)
		{
			return usage(argv[0]);
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
		else if(strcmp(argv[a], "--wait-forever") == 0)
		{
			lockWait = MARK5_LOCK_WAIT_FOREVER;
		}
		else if(strcmp(argv[a], "--rt") == 0)
		{
			readMode = MARK5_READ_MODE_RT;
		}
		else if(strcmp(argv[a], "--raw") == 0)
		{
			copyMode = COPY_MODE_NODIR;
			chunkSize = -1;
		}
		else if(strcmp(argv[a], "-5") == 0 ||
			strcmp(argv[a], "--fix5b") == 0)
		{
			fix5b = 1;
		}
		else if(strcmp(argv[a], "--no-dir") == 0)
		{
			copyMode = COPY_MODE_NODIR;
		}
		else if(strcmp(argv[a], "--small") == 0)
		{
			chunkSize = defaultChunkSize/5;
			printf("Using small chunk size = %d\n", chunkSize);
		}
		else if(strcmp(argv[a], "--large") == 0)
		{
			chunkSize = 5*defaultChunkSize;
			printf("Using large chunk size = %d\n", chunkSize);
		}
		else if(strcmp(argv[a], "-a") == 0 ||
		        strcmp(argv[a], "--append") == 0)
		{
			doAppend = 1;
		}
		else if(vsn[0] == 0)
		{
			strncpy(vsn, argv[a], 8);
			vsn[8] = 0;
		}
		else if(scanList == 0)
		{
			scanList = argv[a];
		}
		else if(outPath == 0)
		{
			outPath = argv[a];
		}
		else
		{
			return usage(argv[0]);
		}
	}

	// If we are at USNO Mark5 unit at MK or PT, force fix5b
	if(copyMode != COPY_MODE_NODIR && !fix5b)
	{
		const int MaxHostnameLength=100;
		char hn[MaxHostnameLength+1];

		gethostname(hn, MaxHostnameLength);
		hn[MaxHostnameLength] = 0;
		if(strcasestr(hn, "pt-mark5c-usno") != 0 || strcasestr(hn, "mk-mark5c-usno") != 0)
		{
			fix5b = 1;

			printf("\n-> Because this is running on %s Mark5B data fixing will be performed\n\n", hn);
		}
	}

	if(fix5b && copyMode == COPY_MODE_NODIR)
	{
		fprintf(stderr, "Error: cannot fix Mark5B and copy without a directory\n");

		return EXIT_FAILURE;
	}

	if(outPath == 0)
	{
		return usage(argv[0]);
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
		switch(copyMode)
		{
			case COPY_MODE_NORMAL:
				v = mk5cp(vsn, scanList, outPath, force, readMode, chunkSize, fix5b, doAppend);
				break;
			case COPY_MODE_NODIR:
				v = mk5cp_nodir(vsn, scanList, outPath, force, readMode, chunkSize, doAppend);
				break;
		}
		if(v < 0)
		{
			if(watchdogXLRError[0] != 0)
			{
				char message[DIFX_MESSAGE_LENGTH];
				snprintf(message, DIFX_MESSAGE_LENGTH, "Streamstor error executing: %s : %s", watchdogStatement, watchdogXLRError);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			}
		}
	}

	unlockMark5();

	/* *********** */

	stopWatchdog();

	return 0;
}
