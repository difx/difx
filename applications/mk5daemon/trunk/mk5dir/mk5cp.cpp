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

const char program[] = "mk5cp";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";

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
	printf("\n%s ver. %s   %s\n\n", program, version, author);
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

	sprintf(filename, "%s/%8s_%03d_%s", outpath, vsn, scanNum+1, scan->name); 

	printf("outname = %s\n", filename);

	out = fopen(filename, "w");
	if(!out)
	{
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
			difxMessageSendDifxAlert("Data copy aborted", DIFX_ALERT_LEVEL_WARNING);
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
			fprintf(stderr, "Read failure at %Ld\n", readptr);
			mk5status->status = MARK5_COPY_ERROR;
			break;
		}

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
			fprintf(stderr, "Incomplete write, FS full?\n");
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
	int a, b, i, s, l, n;
	int scanIndex;

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
		printf("Cannot open XLR\n");
		return 0;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("Cannot set SkipCheckDir\n");
		XLRClose(xlrDevice);
		return 0;
	}

	xlrRC = XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("Cannot set BankMode\n");
		XLRClose(xlrDevice);
		return 0;
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("Cannot get bank A status\n");
		XLRClose(xlrDevice);
		return 0;
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
		printf("Cannot get bank B status\n");
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
			vsn, mk5dirpath, &dirCallback, &mk5status);
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

	n = 0;
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
					difxMessageSendDifxAlert("Bad format for list of scans", DIFX_ALERT_LEVEL_ERROR);
					fprintf(stderr, "Bad format for list of scans\n");
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
					copyScan(xlrDevice, module.label, outpath, scanIndex, module.scans+scanIndex, &mk5status);
					n++;
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
			if(n == 0)
			{
				sprintf(message, "No scans copied from module %8s to %s", module.label, outpath);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
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
					copyScan(xlrDevice, module.label, outpath, scanIndex, module.scans+scanIndex, &mk5status);
					n++;
				}
			}
			if(n == 0)
			{
				sprintf(message, "No scans match with code %s on module %8s", scanlist, module.label);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			}
		}
	}

	if(n > 0)
	{
		sprintf(message, "%d scans copied from module %8s to %s", n, module.label, outpath);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
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
