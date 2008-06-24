#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <difxmessage.h>
#include "config.h"
#include "mark5dir.h"

int usage(const char *pgm)
{
	printf("Usage : %s <vsn>\n", pgm);

	return 0;
}

int main(int argc, char **argv)
{
	int mjdnow;
	const char *mk5dirpath;
	struct Mark5Module module;
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	char vsn[16];
	int v;

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	strncpy(vsn, argv[1], 8);
	vsn[8] = 0;

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

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(mk5dirpath == 0)
	{
		mk5dirpath = ".";
	}
	
	mjdnow = (int)(40587.0 + time(0)/86400.0);

	//sendMark5Status(MARK5_STATE_GETDIR, 0, 0, mjdnow, 0.0);

	memset(&module, 0, sizeof(module));
	module.bank = -1;

	v = getCachedMark5Module(&module, xlrDevice, mjdnow, vsn, mk5dirpath);
	if(v < 0)
	{
		printf("Module not found : %s\n", vsn);
	}

	XLRClose(xlrDevice);

	printMark5Module(&module);

	return 0;
}
