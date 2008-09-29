#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <difxmessage.h>
#include "config.h"
#include "mark5dir.h"

const char program[] = "mk5dir";
const char author[]  = "Walter Brisken";
const char version[] = "0.2";

int verbose = 0;

int usage(const char *pgm)
{
	printf("\n%s ver. %s   %s\n\n", program, version, author);
	printf("A program to extract Mark5 module directory information via XLR calls\n");
	printf("\nUsage : %s [<options>] { <bank> | <vsn> }\n\n", pgm);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("<bank> is either A or B\n\n");
	printf("<vsn> is a valid module VSN (8 characters)\n\n");
	printf("Environment variable MARK5_DIR_PATH should point to the location of\n");
	printf("the directory to be written.  The output filename will be:\n");
	printf("  $MARK5_DIR_PATH/<vsn>.dir\n\n");

	return 0;
}

void dirCallback(int scan, int nscan, int status, void *data)
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
}

int main(int argc, char **argv)
{
	int mjdnow;
	const char *mk5dirpath;
	struct Mark5Module module;
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	DifxMessageMk5Status mk5status;
	char vsn[16] = "";
	int v;
	int a;

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
		else
		{
			return usage(argv[0]);
		}
	}

	difxMessageInit(-1, "mk5dir");
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
			mk5status.activeBank = 'B';
			strcpy(vsn, mk5status.vsnB);
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

	if(strcmp(vsn, "AB") == 0)
	{
		if(strlen(mk5status.vsnA) == 8)
		{
			getCachedMark5Module(&module, xlrDevice, mjdnow, 
				mk5status.vsnA, mk5dirpath, &dirCallback, 
				&mk5status);
			if(verbose > 0)
			{
				printMark5Module(&module);
			}
		}

		memset(&module, 0, sizeof(module));
		module.bank = -1;

		if(strlen(mk5status.vsnB) == 8)
		{
			getCachedMark5Module(&module, xlrDevice, mjdnow, 
				mk5status.vsnB, mk5dirpath, &dirCallback, 
				&mk5status);
			if(verbose > 0)
			{
				printMark5Module(&module);
			}
		}
	}
	else
	{
		if(strlen(vsn) == 8)
		{
			v = getCachedMark5Module(&module, xlrDevice, mjdnow, 
				vsn, mk5dirpath, &dirCallback, &mk5status);
			if(v < 0)
			{
				printf("Module not found : %s\n", vsn);
			}
			else if(verbose > 0)
			{
				printMark5Module(&module);
			}
		}
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
