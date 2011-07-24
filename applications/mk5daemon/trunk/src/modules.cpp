/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include <ctype.h>
#include <xlrapi.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "mk5daemon.h"
#include "smart.h"


/* see if vsn matches <chars> {+|-} <digits> */
int legalVSN(const char *vsn)
{
	int state=0;

	for(int i = 0; i < 8; i++)
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
	char message[DIFX_MESSAGE_LENGTH];
	char xlrErrorStr[XLR_ERROR_LENGTH];
	char tempA[SMART_TEMP_STRING_LENGTH]; 
	char tempB[SMART_TEMP_STRING_LENGTH];
	const char id[] = "GetModules";
	int v;

	v = lockStreamstor(D, id, MARK5_LOCK_DONT_WAIT);
	if(v < 0)
	{
		return 0;
	}
	
	xlrRC = XLROpen(1, &xlrDevice);
	D->nXLROpen++;
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"ERROR: XLR_get_modules: "
			"Cannot open streamstor card.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);

		unlockStreamstor(D, id);

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
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"XLR VSNs: <%s> <%s> N=%d\n",
				vsna, vsnb, D->nXLROpen);
		}
		else
		{
			XLRGetErrorMessage(xlrErrorStr, xlrError);
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"ERROR: XLR_get_modules: "
				"Cannot set SkipCheckDir.  N=%d "
				"Error=%u (%s)\n",
				D->nXLROpen,
				xlrError,
				xlrErrorStr);
		}
		Logger_logData(D->log, message);
		XLRClose(xlrDevice);

		unlockStreamstor(D, id);
		
		return 0;
	}
	
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsnb[0] = 0;

		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"ERROR: XLR_get_modules: "
			"BANK_B XLRGetBankStatus failed.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		clearMk5Smart(D, BANK_B);
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(vsnb, bank_stat.Label, 16);
		vsnb[8] = 0;
		getMk5Smart(xlrDevice, D, BANK_B);
	}
	else
	{
		vsnb[0] = 0;
		clearMk5Smart(D, BANK_B);
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsna[0] = 0;

		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"ERROR: XLR_get_modules: "
			"BANK_A XLRGetBankStatus failed.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		clearMk5Smart(D, BANK_A);
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(vsna, bank_stat.Label, 16);
		vsna[8] = 0;
		getMk5Smart(xlrDevice, D, BANK_A);
	}
	else
	{
		vsna[0] = 0;
		clearMk5Smart(D, BANK_A);
	}

	XLRClose(xlrDevice);

	unlockStreamstor(D, id);

	extractSmartTemps(tempA, D, BANK_A);
	extractSmartTemps(tempB, D, BANK_B);

	snprintf(message, DIFX_MESSAGE_LENGTH, "XLR VSNs: <%s> %s  <%s> %s  N=%d\n",
		vsna, tempA, vsnb, tempB, D->nXLROpen);
	Logger_logData(D->log, message);

	return 0;
}

void Mk5Daemon_getModules(Mk5Daemon *D)
{
	DifxMessageMk5Status dm;
	int n, v;
	char vsnA[16], vsnB[16];
	char message[DIFX_MESSAGE_LENGTH];

	if(!D->isMk5)
	{
		return;
	}

	memset(&dm, 0, sizeof(DifxMessageMk5Status));

	/* don't let the process type change while getting vsns */
	pthread_mutex_lock(&D->processLock);

	vsnA[0] = vsnB[0] = 0;

	if(D->process != PROCESS_NONE)
	{
		v = lockStreamstor(D, "getvsn", 0);
		if(v >= 0)
		{
			unlockStreamstor(D, "getvsn");
			D->process = PROCESS_NONE;
		}
	}

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
	default:
		pthread_mutex_unlock(&D->processLock);
		return;
	}

	if(strncmp(D->vsnA, vsnA, 8) != 0)
	{
		if(legalVSN(D->vsnA))
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module %s removed from bank A", D->vsnA);
			difxMessageSendDifxAlert(message, 
				DIFX_ALERT_LEVEL_VERBOSE);
		}
		if(vsnA[0] == 0)
		{
			D->vsnA[0] = 0;
		}
		else if(legalVSN(vsnA))
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module %s inserted into bank A", vsnA);
			difxMessageSendDifxAlert(message, 
				DIFX_ALERT_LEVEL_VERBOSE);
			strncpy(D->vsnA, vsnA, 8);
			logMk5Smart(D, BANK_A);
		}
		else if(strcmp(D->vsnA, "illegalA") != 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module with illegal VSN inserted into bank A");
			difxMessageSendDifxAlert(message, 
				DIFX_ALERT_LEVEL_ERROR);
			strcpy(D->vsnA, "illegalA");
		}
	}
	if(strncmp(D->vsnB, vsnB, 8) != 0)
	{
		if(legalVSN(D->vsnB))
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module %s removed from bank B", D->vsnB);
			difxMessageSendDifxAlert(message, 
				DIFX_ALERT_LEVEL_VERBOSE);
		}
		if(vsnB[0] == 0)
		{
			D->vsnB[0] = 0;
		}
		else if(legalVSN(vsnB))
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module %s inserted into bank B", vsnB);
			difxMessageSendDifxAlert(message, 
				DIFX_ALERT_LEVEL_VERBOSE);
			strncpy(D->vsnB, vsnB, 8);
			logMk5Smart(D, BANK_B);
		}
		else if(strcmp(D->vsnB, "illegalB") != 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Module with illegal VSN inserted into bank B");
			difxMessageSendDifxAlert(message, 
				DIFX_ALERT_LEVEL_ERROR);
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
	char message[DIFX_MESSAGE_LENGTH];
	char xlrErrorStr[XLR_ERROR_LENGTH];
	int bank;
	const char id[] = "BankPower";
	int v;

	v = lockStreamstor(D, id, MARK5_LOCK_DONT_WAIT);
	if(v != 0)
	{
		return 0;
	}
	
	xlrRC = XLROpen(1, &xlrDevice);
	D->nXLROpen++;
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"ERROR: XLR_disc_power: "
			"Cannot open streamstor card.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);

		unlockStreamstor(D, id);

		return 1;
	}

	for(int i = 0; banks[i]; i++)
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
			snprintf(message, DIFX_MESSAGE_LENGTH, 
				"XLR_disc_power: illegal args: "
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
			snprintf(message, DIFX_MESSAGE_LENGTH, 
				"XLR_disc_power: error for "
				"bank=%c on=%d Error=%u (%s)\n",
				banks[i], on, xlrError, xlrErrorStr);
			Logger_logData(D->log, message);
		}
	}

	XLRClose(xlrDevice);

	unlockStreamstor(D, id);

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

