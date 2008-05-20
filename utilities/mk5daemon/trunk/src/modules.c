#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <xlrapi.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <difxmessage.h>
#include "mk5daemon.h"

int XLR_get_modules(char *vsna, char *vsnb)
{
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	
	xlrRC = XLROpen(1, &xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("ERROR Cannot open streamstor card\n");
		return 1;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("ERROR Cannot set SkipCheckDir\n");
		return 1;
	}
	
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsna[0] = 0;
	}
	else
	{
		strncpy(vsnb, bank_stat.Label, 16);
		vsnb[15] = 0;
		if(vsnb[8] == '/')
		{
			vsnb[8] = 0;
		}
		else
		{
			vsnb[0] = 0;
		}
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsna[0] = 0;
	}
	else
	{
		strncpy(vsna, bank_stat.Label, 16);
		vsna[15] = 0;
		if(vsna[8] == '/')
		{
			vsna[8] = 0;
		}
		else
		{
			vsna[0] = 0;
		}
	}

	XLRClose(xlrDevice);

	return 0;
}

int Mark5A_get_modules(char *vsna, char *vsnb)
{
	char cmd[] = "echo \"bank_set?\" | tstMark5A";
	char line[512];
	FILE *in;
	int n;

	vsna[0] = vsnb[0] = 0;

	in = popen(cmd, "r");
	if(!in)
	{
		printf("ERROR Cannot run tstMark5A\n");
		return 1;
	}

	n = fread(line, 1, 512, in);
	line[511] = 0;
	fclose(in);

	if(line[48] == 'A' && line[52] != '-')
	{
		strncpy(vsna, line+52, 8);
		vsna[8] = 0;
	}
	if(line[48] == 'B' && line[52] != '-')
	{
		strncpy(vsnb, line+52, 8);
		vsnb[8] = 0;
	}

	if(line[73] == 'A' && line[77] != '-')
	{
		strncpy(vsna, line+77, 8);
		vsna[8] = 0;
	}
	if(line[73] == 'B' && line[77] != '-')
	{
		strncpy(vsnb, line+77, 8);
		vsnb[8] = 0;
	}

	return 0;
}

void Mk5Daemon_getModules(Mk5Daemon *D)
{
	DifxMessageMk5Status dm;
	int n;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	dm.activeBank = ' ';

	/* don't let the process type change while getting vsns */
	pthread_mutex_lock(&D->processLock);

	switch(D->process)
	{
	case PROCESS_NONE:
		n = XLR_get_modules(dm.vsnA, dm.vsnB);
		strncpy(D->vsnA, dm.vsnA, 8);
		strncpy(D->vsnB, dm.vsnB, 8);
		if(n == 0)
		{
			dm.state = MARK5_STATE_IDLE;
		}
		else
		{
			dm.state = MARK5_STATE_ERROR;
		}
		break;
	case PROCESS_MARK5:
		n = Mark5A_get_modules(dm.vsnA, dm.vsnB);
		strncpy(D->vsnA, dm.vsnA, 8);
		strncpy(D->vsnB, dm.vsnB, 8);
		if(n == 0)
		{
			dm.state = MARK5_STATE_BUSY;
		}
		else
		{
			dm.state = MARK5_STATE_ERROR;
		}
		break;
	default:
		dm.state = MARK5_STATE_BUSY;
		break;
	}
	
	pthread_mutex_unlock(&D->processLock);

	difxMessageSendMark5Status(&dm);
}
