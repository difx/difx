/***************************************************************************
 *   Copyright (C) 2007-2010 by Walter Brisken                             *
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

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <signal.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "config.h"
#include "mark5dir.h"
#include "watchdog.h"

const char program[] = "mk5cp";
const char author[]  = "Walter Brisken";
const char version[] = "0.7";
const char verdate[] = "20100910";

int verbose = 0;
int die = 0;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	if(verbose)
	{
		fprintf(stderr, "Being killed\n");
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
	fprintf(stderr, "  --force\n");
	fprintf(stderr, "  -f             Continue even if dir is screwy\n\n");
	fprintf(stderr, "<bank> is either A or B or Active\n\n");
	fprintf(stderr, "<vsn> is a valid module VSN (8 characters)\n\n");
	fprintf(stderr, "<scans> is a string containing a list of scans to copy.  No whitespace\n    "
		"is allowed.  Ranges are allowed.  Examples:  1  or  3,5  or  1,3,6-9\n");
	fprintf(stderr, "The <scans> string can also be an MJD range to copy.\n  Example: 54321.112_54321_113\n");
	fprintf(stderr, "The <scans> string can also be a byte range to copy.\n  Example: 38612201536_38619201536\n");
	if(!cat)
	{
		fprintf(stderr, "<output path> is a directory where files will be dumped\n");
		fprintf(stderr, "Environment variable MARK5_DIR_PATH should point to the location of\n");
		fprintf(stderr, "the module directories.  The output filename will be:\n");
		fprintf(stderr, "  $MARK5_DIR_PATH/<vsn>.dir\n");
		fprintf(stderr, "If <output path> is the hyphen (-), then all output goes to stdout\n\n");
	}

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

int copyByteRange(SSHANDLE xlrDevice, const char *outpath, const char *outname, int scanNum, long long byteStart, long long byteStop, DifxMessageMk5Status *mk5status)
{
	FILE *out;
	const int chunksize = 50000000;
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

	if(strcmp(outpath, "-") == 0)
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s", outname); 
		out = stdout;
	}
	else
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", outpath, outname); 
		out = fopen(filename, "w");
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open file %s for write.  Check permissions!", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}

	fprintf(stderr, "outname = %s\n", filename);

	readptr = byteStart;
	togo = byteStop-byteStart;
	data = (streamstordatatype *)malloc(chunksize);
	len = chunksize;

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

	snprintf(message, DIFX_MESSAGE_LENGTH, "Copying portion of scan %d to file %s", scanNum+1, filename);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

	for(int i = 0; togo > 0; i++)
	{
		if(die)
		{
			difxMessageSendDifxAlert("Data copy aborted due to die signal", DIFX_ALERT_LEVEL_WARNING);
			
			break;
		}
		if(verbose)
		{
			fprintf(stderr, "%Ld = %Ld/%Ld\n", readptr, readptr-byteStart, byteStop-byteStart);
		}
		snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "[%Ld%%]", 100*(readptr-byteStart)/(byteStop-byteStart));
		mk5status->position = readptr;
		mk5status->rate = rate;
		difxMessageSendMark5Status(mk5status);
		if(togo < chunksize)
		{
			len = togo;
		}

		a = readptr >> 32;
		b = readptr % (1LL<<32);

		WATCHDOGTEST( XLRReadData(xlrDevice, data, a, b, len) );

		countReplaced(data, len/4, &wGood, &wBad);

		v = fwrite(data, 1, len, out);
		if(v < len)
		{
			if(out == stdout)
			{
				fprintf(stderr, "mk5cp: Broken pipe.\n");
			}
			else
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Disk full?  path=%s", outpath);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			
			break;
		}
		gettimeofday(&t2, 0);
		dt = (t2.tv_sec-t1.tv_sec) + 1.0e-6*(t2.tv_usec-t1.tv_usec);

		if(dt > 0.0)
		{
			rate = 8.0e-6*chunksize/dt; /* Mbps */
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

		readptr += chunksize;
		togo -= len;
	}

	if(out != stdout)
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

int copyScan(SSHANDLE xlrDevice, const char *vsn, const char *outpath, int scanNum, const Mark5Scan *scan, DifxMessageMk5Status *mk5status)
{
	FILE *out;
	const int chunksize = 50000000;
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

	if(strcmp(outpath, "-") == 0)
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%03d_%s", vsn, scanNum+1, scan->name); 
		out = stdout;
	}
	else
	{
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%8s_%03d_%s", outpath, vsn, scanNum+1, scan->name); 
		out = fopen(filename, "w");
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open file %s for write.  Check permissions!", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);

			return -1;
		}
	}

	fprintf(stderr, "outname = %s\n", filename);

	readptr = scan->start;
	togo = scan->length;
	data = (streamstordatatype *)malloc(chunksize);
	len = chunksize;

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

	for(int i = 0; togo > 0; i++)
	{
		if(die)
		{
			difxMessageSendDifxAlert("Data copy aborted due to die signal", DIFX_ALERT_LEVEL_WARNING);
			
			break;
		}
		if(verbose)
		{
			fprintf(stderr, "%Ld = %Ld/%Ld\n", readptr, readptr-scan->start, scan->length);
		}
		snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s[%Ld%%]", scan->name, 100*(readptr-scan->start)/scan->length);
		mk5status->position = readptr;
		mk5status->rate = rate;
		difxMessageSendMark5Status(mk5status);
		if(togo < chunksize)
		{
			len = togo;
		}

		a = readptr >> 32;
		b = readptr % (1LL<<32);

		WATCHDOGTEST( XLRReadData(xlrDevice, data, a, b, len) );

		countReplaced(data, len/4, &wGood, &wBad);

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
			else
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Incomplete write.  Disk full?  path=%s", outpath);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				fprintf(stderr, "Error: %s\n", message);
			}
			
			break;
		}
		gettimeofday(&t2, 0);
		dt = (t2.tv_sec-t1.tv_sec) + 1.0e-6*(t2.tv_usec-t1.tv_usec);

		if(dt > 0.0)
		{
			rate = 8.0e-6*(chunksize-skip)/dt; /* Mbps */
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

		readptr += chunksize;
		togo -= len;
	}

	if(out != stdout)
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

static int parseMjdRange(double *mjdStart, double *mjdStop, const char *scanlist)
{
	if(sscanf(scanlist, "%lf_%lf", mjdStart, mjdStop) != 2)
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

static int parseByteRange(long long *start, long long *stop, const char *scanlist)
{
	if(sscanf(scanlist, "%Ld_%Ld", start, stop) != 2)
	{
		return 0;
	}

	if(*start >= *stop)
	{
		return 0;
	}

	return 1;
}

static int mk5cp(char *vsn, const char *scanlist, const char *outpath, int force)
{
	int mjdnow;
	const char *mk5dirpath;
	int v;
	int b, s, l, nGood, nBad;
	int bank = -1;
	float replacedFrac;
	int bail = 0;
	double mjdStart, mjdStop;
	long long byteStart, byteStop;
	int scanIndex;
	Mark5Scan *scan;
	char outname[DIFX_MESSAGE_FILENAME_LENGTH];

	char message[DIFX_MESSAGE_LENGTH];
	struct Mark5Module module;
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	DifxMessageMk5Status mk5status;

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	WATCHDOGTEST( XLRSetFillData(xlrDevice, MARK5_FILL_PATTERN) );

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

	if(strncasecmp(vsn, mk5status.vsnA, 8) == 0)
	{
		mk5status.activeBank = 'A';
	}
	else if(strncasecmp(vsn, mk5status.vsnB, 8) == 0)
	{
		mk5status.activeBank = 'B';
	}
	else if(strcasecmp(vsn, "A") == 0)
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

	oldsiginthand = signal(SIGINT, siginthand);

	if(strlen(vsn) == 8)
	{
		v = getCachedMark5Module(&module, xlrDevice, mjdnow, 
			vsn, mk5dirpath, &dirCallback, &mk5status,
			&replacedFrac, false, 0, 1);
		if(replacedFrac > 0.01)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module %s directory read encountered %4.2f%% data replacement rate",
				vsn, replacedFrac);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "Warning: %s\n", message);
		}
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
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			mk5status.activeBank = ' ';
			bail = 1;
		}
		else if(verbose > 0)
		{
			printMark5Module(&module);
		}

		if(v >= 0)
		{
			v = sanityCheckModule(&module);
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
	}

	if(bail)
	{
		fprintf(stderr, "Bailing!\n");
		return -1;
	}

	if(strcasecmp(vsn, "A") == 0 && mk5status.vsnA[0] != 0)
	{
		mk5status.activeBank = 'A';
		strcpy(vsn, mk5status.vsnA);
	}
	if(strcasecmp(vsn, "B") == 0 && mk5status.vsnB[0] != 0)
	{
		strcpy(vsn, mk5status.vsnB);
		mk5status.activeBank = 'B';
	}

	if(mk5status.activeBank == 'A')
	{
		bank = BANK_A;
	}
	else if(mk5status.activeBank == 'B')
	{
		bank = BANK_B;
	}

	if(bank < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot figure out which bank to use: '%c' requested.", mk5status.activeBank);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
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
	if(mk5status.activeBank > ' ' && bail < 1) 
	{
		/* first look for mjd range */
		if(parseMjdRange(&mjdStart, &mjdStop, scanlist))	
		{
			for(scanIndex = 0; scanIndex < module.nscans; scanIndex++)
			{
				scan = module.scans+scanIndex;
				if(!getByteRange(scan, &byteStart, &byteStop, mjdStart, mjdStop))
				{
					continue;
				}
				snprintf(outname, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%s_%d", module.label, scanlist, nGood+nBad);
				v = copyByteRange(xlrDevice, outpath, outname, scanIndex, byteStart, byteStop, &mk5status);
				if(v == 0)
				{
					nGood++;
				}
				else
				{
					if(watchdogXLRError[0] != 0)
					{
						return v;
					}
					nBad++;
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
		else if(parseByteRange(&byteStart, &byteStop, scanlist))
		{
			snprintf(outname, DIFX_MESSAGE_FILENAME_LENGTH, "%8s_%s", module.label, scanlist);
			v = copyByteRange(xlrDevice, outpath, outname, -1, byteStart, byteStop, &mk5status);

			if(v == 0)
			{
				nGood++;
			}
			else
			{
				if(watchdogXLRError[0] != 0)
				{
					return v;
				}
				nBad++;
			}
			if(nGood == 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "Byte range %Ld to %Ld not in any scan", byteStart, byteStop);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
				fprintf(stderr, "Warning: %s\n", message);
			}
		}
		/* next look for scan range */
		else if(isdigit(scanlist[0])) for(;;)
		{
			int a;

			fprintf(stderr, "scanlist = %s\n", scanlist);

			v = sscanf(scanlist, "%d%n", &a, &s);
			scanlist += s;
			if(v < 1)
			{
				break;
			}
			if(scanlist[0] == '-')
			{
				scanlist++;
				v = sscanf(scanlist, "%d%n", &b, &s);
				scanlist += s;
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

			for(int i = a; i <= b; i++)
			{
				if(die)
				{
					break;
				}
				if(i > 0 && i <= module.nscans)
				{
					scanIndex = i-1;
					v = copyScan(xlrDevice, module.label, outpath, scanIndex, module.scans+scanIndex, &mk5status);
					if(v == 0)
					{
						nGood++;
					}
					else
					{
						if(watchdogXLRError[0] != 0)
						{
							return v;
						}
						nBad++;
					}
				}
				else
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "Scan number %d out of range.  nScan = %d", i, module.nscans);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
					fprintf(stderr, "Warning: %s\n", message);
					nBad++;
				}
			}

			if(scanlist[0] == 0)
			{
				break;
			}
			else
			{
				scanlist++;
			}
		}
		/* finally, look for scan name */
		else
		{
			l = strlen(scanlist);
			for(int i = 0; i < module.nscans; i++)
			{
				if(strncasecmp(module.scans[i].name, scanlist, l) == 0)
				{
					scanIndex = i;
					v = copyScan(xlrDevice, module.label, outpath, scanIndex, module.scans+scanIndex, &mk5status);
					if(v == 0) 
					{
						nGood++;
					}
					else
					{
						if(watchdogXLRError[0] != 0)
						{
							return v;
						}
						nBad++;
					}
				}
			}
		}

		if(nGood > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "%d scans copied from module %8s to %s", nGood, module.label, outpath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			fprintf(stderr, "%s\n", message);
		}
		if(nBad > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "%d scans NOT copied from module %8s to %s", nBad, module.label, outpath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "%s\n", message);
		}
		if(nGood == 0 && nBad == 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "No scans match with code %s on module %8s", scanlist, module.label);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "%s\n", message);
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
	mk5status.scanNumber = module.nscans;
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

int main(int argc, char **argv)
{
	char vsn[16] = "";
	const char *scanlist=0;
	const char *outpath=0;
	int force = 0;
	int v;
	int lockWait = MARK5_LOCK_DONT_WAIT;

	v = strlen(argv[0]);
	if(v >= 6 && strcmp(argv[0]+v-6, "mk5cat") == 0)
	{
		difxMessageInit(-1, "mk5cat");
		outpath = "-";
	}
	else
	{
		difxMessageInit(-1, program);
	}

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	for(int a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "-") == 0)
		{
			if(scanlist != 0 && outpath == 0)
			{
				outpath = argv[a];
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
			verbose++;
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
		else if(vsn[0] == 0)
		{
			strncpy(vsn, argv[a], 8);
			vsn[8] = 0;
		}
		else if(scanlist == 0)
		{
			scanlist = argv[a];
		}
		else if(outpath == 0)
		{
			outpath = argv[a];
		}
		else
		{
			return usage(argv[0]);
		}
	}

	if(outpath == 0)
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
		v = mk5cp(vsn, scanlist, outpath, force);
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
