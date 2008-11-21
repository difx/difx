#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <xlrapi.h>
#include <unistd.h>
#include <difxmessage.h>
#include "mk5daemon.h"

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

	strncpy(D->vsnA, vsnA, 8);
	strncpy(D->vsnB, vsnB, 8);
	strncpy(dm.vsnA, vsnA, 8);
	strncpy(dm.vsnB, vsnB, 8);
	
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

