#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <xlrapi.h>
#include <unistd.h>
#include <ctype.h>
#include <difxmessage.h>
#include "mk5daemon.h"


/* see if vsn matches <chars> {+|-} <digits> */
int legalVSN(const char *vsn)
{
	int i;
	int state=0;

	for(i = 0; i < 8; i++)
	{
		switch(state)
		{
		case 0:
			if(!isalpha(vsn[i]))
			{
				return 0;
			}
			state = 1;
			break;
		case 1:
			if(i >= 6)
			{
				return 0;
			}
			if(vsn[i] == '-' || vsn[i] == '+')
			{
				state = 2;
			}
			else if(!isalpha(vsn[i]))
			{
				return 0;
			}
			break;
		case 2:
			if(!isdigit(vsn[i]))
			{
				return 0;
			}
			break;
		}
	}

	return 1;
}

static int XLR_get_modules(char *vsna, char *vsnb, Mk5Daemon *D)
{
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	unsigned int xlrError;
	char message[100+(XLR_ERROR_LENGTH)];
	char xlrErrorStr[XLR_ERROR_LENGTH];
	
	xlrRC = XLROpen(1, &xlrDevice);
	D->nXLROpen++;
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_get_modules: "
			"Cannot open streamstor card.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		return 1;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		if(xlrError == 148) /* XLR_ERR_DRIVEMODULE_NOTREADY */
		{
			/* this means no modules loaded */
			vsna[0] = vsnb[0] = 0;
			sprintf(message, "XLR VSNs: <%s> <%s> N=%d\n",
				vsna, vsnb, D->nXLROpen);
		}
		else
		{
			XLRGetErrorMessage(xlrErrorStr, xlrError);
			sprintf(message, "ERROR: XLR_get_modules: "
				"Cannot set SkipCheckDir.  N=%d "
				"Error=%u (%s)\n",
				D->nXLROpen,
				xlrError,
				xlrErrorStr);
		}
		Logger_logData(D->log, message);
		XLRClose(xlrDevice);
		return 0;
	}
	
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsnb[0] = 0;

		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_get_modules: "
			"BANK_B XLRGetBankStatus failed.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(vsnb, bank_stat.Label, 16);
		vsnb[8] = 0;
	}
	else
	{
		vsnb[0] = 0;
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsna[0] = 0;

		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_get_modules: "
			"BANK_A XLRGetBankStatus failed.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(vsna, bank_stat.Label, 16);
		vsna[8] = 0;
	}
	else
	{
		vsna[0] = 0;
	}

	XLRClose(xlrDevice);

	sprintf(message, "XLR VSNs: <%s> <%s> N=%d\n",
		vsna, vsnb, D->nXLROpen);
	Logger_logData(D->log, message);

	return 0;
}

static int Mark5A_get_modules(char *vsna, char *vsnb, Mk5Daemon *D)
{
	char message[512];
	int i, n;
	char *ptr[8];
	int nptr = 0;

	vsna[0] = vsnb[0] = 0;

	n = mark5command("bank_set?\n", message, 511);

	if(n < 0)
	{
		switch(n)
		{
		case -1:
			Logger_logData(D->log, "cannot make socket");	
			break;
		case -2:
			Logger_logData(D->log, "cannot connect to Mark5A");	
			break;
		case -3:
			Logger_logData(D->log, "error sending bank_set? to Mark5A");
			break;
		case -4:
			Logger_logData(D->log, "error recving from Mark5A");
			break;
		}
		return 1;
	}

	for(i = 0; message[i] && nptr < 8; i++)
	{
		if(message[i] == ':')
		{
			ptr[nptr] = message+i+2;
			message[i] = 0;
			nptr++;
		}
	}

	if(nptr >= 2)
	{
		if(ptr[0][0] == 'A' && ptr[0][1] <= ' ' && ptr[1][0] != '-')
		{
			strncpy(vsna, ptr[1], 8);
			vsna[8] = 0;
		}
		if(ptr[0][0] == 'B' && ptr[0][1] <= ' ' && ptr[1][0] != '-')
		{
			strncpy(vsnb, ptr[1], 8);
			vsnb[8] = 0;
		}
	}

	if(nptr >= 4)
	{
		if(ptr[2][0] == 'A' && ptr[2][1] <= ' ' && ptr[3][0] != '-')
		{
			strncpy(vsna, ptr[3], 8);
			vsna[8] = 0;
		}
		if(ptr[2][0] == 'B' && ptr[2][1] <= ' ' && ptr[3][0] != '-')
		{
			strncpy(vsnb, ptr[3], 8);
			vsnb[8] = 0;
		}
	}

	return 0;
}

void Mk5Daemon_getModules(Mk5Daemon *D)
{
	DifxMessageMk5Status dm;
	int n;
	char vsnA[16], vsnB[16];
	char message[1000];

	memset(&dm, 0, sizeof(DifxMessageMk5Status));

	/* don't let the process type change while getting vsns */
	pthread_mutex_lock(&D->processLock);

	vsnA[0] = vsnB[0] = 0;

	switch(D->process)
	{
	case PROCESS_NONE:
		n = XLR_get_modules(vsnA, vsnB, D);
		if(n == 0)
		{
			dm.state = MARK5_STATE_IDLE;
		}
		else
		{
			dm.state = MARK5_STATE_ERROR;
		}
		break;
	case PROCESS_MARK5A:
		n = Mark5A_get_modules(vsnA, vsnB, D);
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

	if(strncmp(D->vsnA, vsnA, 8) != 0)
	{
		if(legalVSN(D->vsnA))
		{
			sprintf(message, "Module %s removed from bank A", D->vsnA);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
		}
		if(vsnA[0] == 0)
		{
			D->vsnA[0] = 0;
		}
		else if(legalVSN(vsnA))
		{
			sprintf(message, "Module %s inserted into bank A", vsnA);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
			strncpy(D->vsnA, vsnA, 8);
		}
		else if(strcmp(D->vsnA, "illegalA") != 0)
		{
			sprintf(message, "Module with illegal VSN inserted into bank A");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			strcpy(D->vsnA, "illegalA");
		}
	}
	if(strncmp(D->vsnB, vsnB, 8) != 0)
	{
		if(legalVSN(D->vsnB))
		{
			sprintf(message, "Module %s removed from bank B", D->vsnB);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
		}
		if(vsnB[0] == 0)
		{
			D->vsnB[0] = 0;
		}
		else if(legalVSN(vsnB))
		{
			sprintf(message, "Module %s inserted into bank B", vsnB);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
			strncpy(D->vsnB, vsnB, 8);
		}
		else if(strcmp(D->vsnB, "illegalB") != 0)
		{
			sprintf(message, "Module with illegal VSN inserted into bank B");
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			strcpy(D->vsnB, "illegalB");
		}
	}
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	
	pthread_mutex_unlock(&D->processLock);

	difxMessageSendMark5Status(&dm);
}

/* on > 0 for mount, 0 for dismount */
static int XLR_disc_power(Mk5Daemon *D, const char *banks, int on)
{
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	unsigned int xlrError;
	char message[100+(XLR_ERROR_LENGTH)];
	char xlrErrorStr[XLR_ERROR_LENGTH];
	int i, bank;
	
	xlrRC = XLROpen(1, &xlrDevice);
	D->nXLROpen++;
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_disc_power: "
			"Cannot open streamstor card.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		return 1;
	}

	for(i = 0; banks[i]; i++)
	{
		if(banks[i] == 'A' || banks[i] == 'a')
		{
			bank = BANK_A;
		}
		else if(banks[i] == 'B' || banks[i] == 'b')
		{
			bank = BANK_B;
		}
		else
		{
			sprintf(message, "XLR_disc_power: illegal args: "
				"bank=%c, on=%d\n", banks[i], on);
			Logger_logData(D->log, message);
			continue;
		}
		if(on)
		{
			xlrRC = XLRMountBank(xlrDevice, bank);
		}
		else
		{
			xlrRC = XLRDismountBank(xlrDevice, bank);
		}
		if(xlrRC != XLR_SUCCESS)
		{
			xlrError = XLRGetLastError();
			XLRGetErrorMessage(xlrErrorStr, xlrError);
			sprintf(message, "XLR_disc_power: error for "
				"bank=%c on=%d Error=%u (%s)\n",
				banks[i], on, xlrError, xlrErrorStr);
			Logger_logData(D->log, message);
		}
	}

	XLRClose(xlrDevice);

	return 0;
}

void Mk5Daemon_diskOn(Mk5Daemon *D, const char *banks)
{
	/* don't let the process type change while getting vsns */
	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		XLR_disc_power(D, banks, 1);
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_diskOff(Mk5Daemon *D, const char *banks)
{
	/* don't let the process type change while getting vsns */
	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		XLR_disc_power(D, banks, 0);
	}

	pthread_mutex_unlock(&D->processLock);
}

