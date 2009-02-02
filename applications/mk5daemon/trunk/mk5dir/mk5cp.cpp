/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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
// $Id: $
// $HeadURL: $
// $LastChangedRevision: $
// $Author: $
// $LastChangedDate: $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <difxmessage.h>
#include <signal.h>
#include <ctype.h>
#include <sys/time.h>
#include "config.h"
#include "mark5dir.h"
#include "replaced.h"
#include "../config.h"

const char program[] = "mk5cp";
const char author[]  = "Walter Brisken";
const char version[] = "0.3";
const char verdate[] = "20090202";

int verbose = 0;
int die = 0;

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
	printf("A program to copy Mark5 module scans via XLR calls\n");
	printf("\nUsage : %s [<options>] { <bank> | <vsn> } <scans> <output path>\n\n", pgm);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("<bank> is either A or B\n\n");
	printf("<vsn> is a valid module VSN (8 characters)\n\n");
	printf("<scans> is a string containing a list of scans to copy.  No whitespace\n    "
		"is allowed.  Ranges are allowed.  Examples:  1  or  3,5  or  1,3,6-9\n");
	printf("output path> is a directory where files will be dumped\n");
	printf("Environment variable MARK5_DIR_PATH should point to the location of\n");
	printf("the directory to be written.  The output filename will be:\n");
	printf("  $MARK5_DIR_PATH/<vsn>.dir\n\n");

	return 0;
}

int dirCallback(int scan, int nscan, int status, void *data)
{
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

	return die;
}

int copyScan(SSHANDLE xlrDevice, const char *vsn, const char *outpath, int scanNum, const Mark5Scan *scan, DifxMessageMk5Status *mk5status)
{
	XLR_RETURN_CODE xlrRC;
	FILE *out;
	const int chunksize = 50000000;
	long long readptr;
	long long togo;
	int len, skip;
	unsigned long *data;
	int i, a, b, v;
	char filename[256];
	struct timeval t1, t2;
	double dt;
	double rate;
	char message[1000];
	long long wGood=0, wBad=0;

	sprintf(filename, "%s/%8s_%03d_%s", outpath, vsn, scanNum+1, scan->name); 

	printf("outname = %s\n", filename);

	out = fopen(filename, "w");
	if(!out)
	{
		sprintf(message, "Cannot open file %s for write.  Check permissions!", filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);

		return -1;
	}

	readptr = scan->start;
	togo = scan->length;
	data = (unsigned long *)malloc(chunksize);
	len = chunksize;

	mk5status->status = MARK5_COPY_SUCCESS;
	mk5status->scanNumber = scanNum+1;

	if(verbose)
	{
		printf("Writing %s\n", filename);
		printf("start/length = %Ld/%Ld\n", scan->start, scan->length);
	}

	rate = 0.0;
	gettimeofday(&t1, 0);

	sprintf(message, "Copying scan %d to file %s", scanNum+1, filename);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

	for(i = 0; togo > 0; i++)
	{
		if(die)
		{
			difxMessageSendDifxAlert("Data copy aborted due to die signal", DIFX_ALERT_LEVEL_WARNING);
			
			break;
		}
		if(verbose)
		{
			printf("%Ld = %Ld/%Ld\n", readptr, readptr-scan->start, scan->length);
		}
		sprintf(mk5status->scanName, "%s[%Ld%%]", scan->name, 100*(readptr-scan->start)/scan->length);
		mk5status->position = readptr;
		mk5status->rate = rate;
		difxMessageSendMark5Status(mk5status);
		if(togo < chunksize)
		{
			len = togo;
		}

		a = readptr >> 32;
		b = readptr % (1LL<<32);

		xlrRC = XLRReadData(xlrDevice, data, a, b, len);

		if(xlrRC == XLR_FAIL)
		{
			sprintf(message, "XLR Read failure at %Ld.  Aborting copy.", readptr);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);
			mk5status->status = MARK5_COPY_ERROR;
			
			break;
		}

		countReplaced(data, len/4, &wGood, &wBad);

		if(i == 0)
		{
			skip = scan->frameoffset;
		}
		else
		{
			skip = 0;
		}

		v = fwrite(data+(skip/4), 1, chunksize-skip, out);
		if(v < chunksize-skip)
		{
			sprintf(message, "Incomplete write -- disk full?  path=%s", outpath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "Error: %s\n", message);
			
			break;
		}
		gettimeofday(&t2, 0);
		dt = (t2.tv_sec-t1.tv_sec) + 1.0e-6*(t2.tv_usec-t1.tv_usec);

		if(dt > 0.0)
		{
			rate = 8.0e-6*(chunksize-skip)/dt; /* Mbps */
		}
		t1 = t2;

		readptr += chunksize;
		togo -= len;
	}

	fclose(out);
	free(data);

	sprintf(message, "Copied scan %d. %Ld bytes total, %Ld bytes replaced.", scanNum+1, 4*(wGood+wBad), 4*wBad);
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

int main(int argc, char **argv)
{
	int mjdnow;
	const char *mk5dirpath;
	const char *scanlist=0;
	const char *outpath=0;
	char message[1000];
	struct Mark5Module module;
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	DifxMessageMk5Status mk5status;
	char vsn[16] = "";
	int v;
	int a, b, i, s, l, nGood, nBad;
	int scanIndex;
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

	difxMessageInit(-1, "mk5cp");
	memset(&mk5status, 0, sizeof(mk5status));

	xlrRC = XLROpen(1, &xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot open XLR");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		return -1;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot set SkipCheckDir");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		XLRClose(xlrDevice);
		
		return -1;
	}

	xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot set BankMode");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		XLRClose(xlrDevice);
		
		return -1;
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot get bank A status");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		XLRClose(xlrDevice);
		
		return -1;
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(mk5status.vsnA, bank_stat.Label, 8);
		mk5status.vsnA[8] = 0;
		if(strcasecmp(vsn, "A") == 0)
		{
			mk5status.activeBank = 'A';
			strcpy(vsn, mk5status.vsnA);
		}
	}
	else
	{
		mk5status.vsnA[0] = 0;
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		sprintf(message, "Cannot get bank B status");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
		
		XLRClose(xlrDevice);
		
		return 0;
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(mk5status.vsnB, bank_stat.Label, 8);
		mk5status.vsnB[8] = 0;
		if(strcasecmp(vsn, "B") == 0)
		{
			strcpy(vsn, mk5status.vsnB);
			mk5status.activeBank = 'B';
		}
	}
	else
	{
		mk5status.vsnB[0] = 0;
	}

	if(strncasecmp(vsn, mk5status.vsnA, 8) == 0)
	{
		mk5status.activeBank = 'A';
	}
	else if(strncasecmp(vsn, mk5status.vsnB, 8) == 0)
	{
		mk5status.activeBank = 'B';
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

	if(strlen(vsn) == 8)
	{
		v = getCachedMark5Module(&module, xlrDevice, mjdnow, 
			vsn, mk5dirpath, &dirCallback, &mk5status,
			&replacedFrac);
		if(replacedFrac > 0.01)
		{
			sprintf(message, "Module %s directory read encountered %4.2f%% data replacement rate",
				vsn, replacedFrac);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			fprintf(stderr, "Warning: %s\n", message);
		}
		if(v < 0)
		{
			fprintf(stderr, "Unsuccessful\n");
			mk5status.activeBank = ' ';
		}
		else if(verbose > 0)
		{
			printMark5Module(&module);
		}
	}

	if(mk5status.activeBank == 'A')
	{
		xlrRC = XLRSelectBank(xlrDevice, BANK_A);
	}
	else if(mk5status.activeBank == 'B')
	{
		xlrRC = XLRSelectBank(xlrDevice, BANK_B);
	}

	mk5status.state = MARK5_STATE_COPY;

	nGood = 0;
	nBad = 0;
	if(mk5status.activeBank > ' ') 
	{
		if(isdigit(scanlist[0])) for(;;)
		{

			printf("scanlist = %s\n", scanlist);

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
					sprintf(message, "Bad format for list of scans");
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "Error: %s\n", message);
					break;
				}
			}
			else
			{
				b = a;
			}

			printf("reading %d to %d\n", a, b);

			for(i = a; i <= b; i++)
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
						nBad++;
					}
				}
				else
				{
					sprintf(message, "Scan number %d out of range.  nScan = %d", i, module.nscans);
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
		else
		{
			l = strlen(scanlist);
			for(i = 0; i < module.nscans; i++)
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
						nBad++;
					}
				}
			}
		}

		if(nGood > 0)
		{
			sprintf(message, "%d scans copied from module %8s to %s", nGood, module.label, outpath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			printf("%s\n", message);
		}
		if(nBad > 0)
		{
			sprintf(message, "%d scans NOT copied from module %8s to %s", nBad, module.label, outpath);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			printf("%s\n", message);
		}
		if(nGood == 0 && nBad == 0)
		{
			sprintf(message, "No scans match with code %s on module %8s", scanlist, module.label);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			printf("%s\n", message);
		}
	}
	else
	{
		sprintf(message, "Cannot find vsn=%s or get its directory", vsn);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		fprintf(stderr, "Error: %s\n", message);
	}

	XLRClose(xlrDevice);

	/* Send final "IDLE" state message so everyone knows we're done */
	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = module.nscans;
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}
