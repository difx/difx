/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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
 * $Id: mk5daemon.h 3555 2011-07-24 00:36:06Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/mk5daemon.h $
 * $LastChangedRevision: 3555 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2011-07-23 18:36:06 -0600 (Sat, 23 Jul 2011) $
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <time.h>
#include "mk5daemon.h"
#include "smart.h"
#include "watchdog.h"
#include "../mk5dir/mark5directorystructs.h"

const SmartDescription smartDescriptions[] =
{
	{ 1,   1, "Read error count"},
	{ 3,   0, "Spin up time (ms)"},
	{ 4,   0, "Start/stop count"},
	{ 5,   1, "Reallocated sector count"},
	{ 7,   0, "Seek error count"},
	{ 8,   0, "Seek time performance"},
	{ 9,   0, "Power on time (hr)"},
	{ 10,  1, "Spin retry count"},
	{ 11,  0, "Recalibration retry count"},
	{ 12,  0, "Power cycle count"},
	{ 187, 0, "Reported Uncorrectable Errors"},
	{ 189, 0, "High Fly Writes"},
	{ 190, 0, "Airflow Temperature"},
	{ 192, 0, "Retract cycle count"},
	{ 193, 0, "Landing zone load count"},
	{ 194, 0, "Temperature (C)"},
	{ 195, 0, "Hardware ECC Recovered"},
	{ 196, 1, "Relocation event count"},
	{ 197, 1, "Questionable sector count"},
	{ 198, 1, "Uncorrectable sector count"},
	{ 199, 0, "DMA CRC error count"},
	{ 200, 0, "Multi-zone error count"},
	{ 201, 1, "Off-track error count"},
	{ 202, 0, "Data Address Mark error count"},
	{ 209, 0, "Offline Seek Performance"},
	{ 212, 0, "Shock During Write"},
	{ -1,  -1, "Unknown SMART id"}
};

static void trim(char *out, const char *in)
{
	int i, s=-1, e=0;

	for(i = 0; in[i]; ++i)
	{
		if(in[i] > ' ')
		{
			if(s == -1) 
			{
				s = e = i;
			}
			else
			{
				e = i;
			}
		}
	}

	if(s == -1)
	{
		out[0] = 0;
	}
	else
	{
		strncpy(out, in+s, e-s+1);
		out[e-s+1] = 0;
	}
}


void clearMk5Smart(Mk5Daemon *D, int bank)
{
	memset(&D->smartData[bank], 0, sizeof(Mk5Smart));
	D->smartData[bank].mjd = 100;
}

void clearMk5DirInfo(Mk5Daemon *D, int bank)
{
	D->nScan[bank] = 0;
	D->bytesUsed[bank] = 0LL;
	D->bytesTotal[bank] = 0LL;
	D->startPointer[bank] = 0LL;
	D->stopPointer[bank] = 0LL;
	D->scanLabel[bank][0] = 0;
	memset(&D->dir_info[bank], 0, sizeof(S_DIR));
}

void clearMk5Stats(Mk5Daemon *D, int bank)
{
	memset(&(D->driveStats[bank]), 0, N_DRIVE*XLR_MAXBINS*sizeof(S_DRIVESTATS));
	D->driveStatsIndex[bank] = 0;
}

const char *getSmartDescription(int smartId)
{
	int i;

	for(i = 0; smartDescriptions[i].id >= 0; ++i)
	{
		if(smartDescriptions[i].id == smartId)
		{
			break;
		}
	}

	return smartDescriptions[i].desc;
}

int isSmartCritical(int smartId)
{
	int i;

	for(i = 0; smartDescriptions[i].id >= 0; ++i)
	{
		if(smartDescriptions[i].id == smartId)
		{
			break;
		}
	}

	return smartDescriptions[i].critical;
}

int setScan(Mk5Daemon *D, int bank, int scanNum)
{
	if(scanNum < 1 || scanNum > D->nScan[bank])
	{
		return -1;
	}
	if(D->dirVersion[bank] >= 1)
	{
		const struct Mark5DirectoryScanHeaderVer1 *scan = (struct Mark5DirectoryScanHeaderVer1 *)(D->dirData[bank] + 128*scanNum);
		char station[4];
		char expName[10];
		char scanName[34];

		strncpy(station, scan->station, 2);
		station[2] = 0;
		strncpy(expName, scan->expName, 8);
		expName[8] = 0;
		strncpy(scanName, scan->scanName, 32);
		scanName[32] = 0;

		D->startPointer[bank] = scan->startByte;
		D->stopPointer[bank] = scan->stopByte;
		snprintf(D->scanLabel[bank], MODULE_LEGACY_SCAN_LENGTH, "%s_%s_%s", expName, station, scanName);
	}
	else if(D->dirVersion[bank] == 0)
	{
		const struct Mark5LegacyDirectory *legacyDir = (struct Mark5LegacyDirectory *)(D->dirData[bank]);

		D->startPointer[bank] = legacyDir->start[scanNum-1];
		D->stopPointer[bank] = D->startPointer[bank] + legacyDir->length[scanNum-1];
		snprintf(D->scanLabel[bank], MODULE_LEGACY_SCAN_LENGTH, "%s", legacyDir->scanName[scanNum-1]);
	}
	else
	{
		return -1;
	}

	return 0;
}

int getDirectoryInfo(SSHANDLE xlrDevice, Mk5Daemon *D, int bank)
{
	XLR_RETURN_CODE xlrRC;
	int len;

	WATCHDOG( XLRGetDirectory(xlrDevice, &(D->dir_info[bank])) );
	
	WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );

	if(len % 128 == 0 && len >= 128)
	{
		if(len != D->dirLength[bank])
		{
			if(D->dirData[bank])
			{
				free(D->dirData[bank]);
			}
			D->dirData[bank] = (char *)malloc(len);
			WATCHDOG( xlrRC = XLRGetUserDir(xlrDevice, len, 0, D->dirData[bank]) );
			if(xlrRC != XLR_SUCCESS)
			{
				return -3;
			}

			const struct Mark5DirectoryHeaderVer1 *dirHeader = (struct Mark5DirectoryHeaderVer1 *)(D->dirData[bank]);
			D->diskModuleState[bank] = dirHeader->status & 0xFF;	/* don't include scan count here */
			D->dirVersion[bank] = dirHeader->version;
			D->dirLength[bank] = len;
			D->nScan[bank] = len/128 - 1;
			setScan(D, bank, D->nScan[bank]);
		}
	}
	else if(len > 80000)	/* probably dir ver 0 */
	{
		if(len != D->dirLength[bank])
		{
			D->dirData[bank] = (char *)malloc(len);
			xlrRC = XLRGetUserDir(xlrDevice, len, 0, D->dirData[bank]);
			if(xlrRC != XLR_SUCCESS)
			{
				return -4;
			}
			D->dirVersion[bank] = 0;
			const struct Mark5LegacyDirectory *legacyDir = (struct Mark5LegacyDirectory *)(D->dirData[bank]);
			D->nScan[bank] = legacyDir->nscans;
			if(D->nScan[bank] < 0 || D->nScan[bank] > MODULE_LEGACY_MAX_SCANS)
			{
				D->nScan[bank] = 0;
			}
			if(D->nScan[bank] > 0)
			{
				setScan(D, bank, D->nScan[bank]);
			}
			D->dirLength[bank] = len;
		}
	}
	else	/* not recognized dir ver */
	{
		D->dirVersion[bank] = -1;
		D->nScan[bank] = 0;
		if(D->dirData)
		{
			free(D->dirData[bank]);
			D->dirData[bank] = 0;
		}
	}

	return 0;
}

/* this is really a lot more than just smart data now! */
int getMk5Smart(SSHANDLE xlrDevice, Mk5Daemon *D, int bank)
{
	XLR_RETURN_CODE xlrRC;
	S_DRIVEINFO driveInfo;
	USHORT smartVersion;
	Mk5Smart *smart;
	int v;

	if(bank < 0 || bank >= N_BANK)
	{
		return -1;
	}

	strncpy(D->smartData[bank].vsn, D->vsns[bank], 8);
	D->smartData[bank].vsn[8] = 0;

	WATCHDOG( xlrRC = XLRSelectBank(xlrDevice, bank) );
	if(xlrRC != XLR_SUCCESS)
	{
		clearModuleInfo(D, bank);
		
		return -2;
	}

	v = getDirectoryInfo(xlrDevice, D, bank);
	if(v < 0)
	{
		clearModuleInfo(D, bank);

		return v;
	}

	D->bytesUsed[bank] = XLRGetLength(xlrDevice);
	D->bytesTotal[bank] = 0LL;

	smart = &(D->smartData[bank]);
	smart->mjd = 40587.0 + time(0)/86400.0;

	for(int d = 0; d < N_SMART_DRIVES; ++d)
	{
		DriveInformation *drive;

		drive = smart->drive + d;

		( xlrRC = XLRGetDriveInfo(xlrDevice, d/2, d%2, &driveInfo) );
		if(xlrRC != XLR_SUCCESS)
		{
			continue;
		}

		trim(drive->model, driveInfo.Model);
		trim(drive->serial, driveInfo.Serial);
		trim(drive->rev, driveInfo.Revision);
		drive->capacity = driveInfo.Capacity * 512LL;
		drive->smartCapable = driveInfo.SMARTCapable;

		D->bytesTotal[bank] += drive->capacity;

		if(!drive->smartCapable)
		{
			continue;
		}

#ifdef HAS_SMART
		WATCHDOG( xlrRC = XLRReadSmartValues(xlrDevice, &smartVersion, smart->smartXLR[d], d/2, d%2) );
		if(xlrRC != XLR_SUCCESS)
		{
			continue;
		}

		for(v = 0; v < XLR_MAX_SMARTVALUES; ++v)
		{
			if(smart->smartXLR[d][v].ID <= 0)
			{
				break;
			}

			smart->id[d][v] = smart->smartXLR[d][v].ID;
			for(int i = 0; i < 6; ++i)
			{
				smart->value[d][v] = (smart->value[d][v] << 8) + smart->smartXLR[d][v].raw[i];
			}
		}

		smart->nValue[d] = v;
#endif
	}

	return 0;
}

int logMk5Smart(const Mk5Daemon *D, int bank)
{
	char message[DIFX_MESSAGE_LENGTH];
	const char *vsn;
	const Mk5Smart *smart = D->smartData + bank;

	if(bank < 0 || bank >= N_BANK)
	{
		return 0;
	}
	
	vsn = D->vsns[bank];

	if(smart->mjd < 50000.0 || vsn[0] == 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Warning: logMk5Smart: smart MJD = %14.8f\n", smart->mjd);

		return 0;
	}

	snprintf(message, DIFX_MESSAGE_LENGTH, "SMART data for module %s\n", vsn);
	Logger_logData(D->log, message);
	for(int d = 0; d < N_SMART_DRIVES; ++d)
	{
		const DriveInformation *drive = smart->drive + d;

		if(drive->capacity <= 0)
		{
			continue;
		}

		snprintf(message, DIFX_MESSAGE_LENGTH, "Drive %d : %s  %s  %s  %Ld bytes\n", d, drive->model, drive->serial, drive->rev, drive->capacity);
		Logger_logData(D->log, message);

		if(!drive->smartCapable)
		{
			continue;
		}
		for(int v = 0; v < smart->nValue[d]; ++v)
		{
			int id = smart->id[d][v];
			const char *desc = getSmartDescription(id);
			int critical = isSmartCritical(id);

			snprintf(message, DIFX_MESSAGE_LENGTH, "  SMART ID %3d : %s = %Ld\n", id, desc, smart->value[d][v]);
			Logger_logData(D->log, message);
			if(critical > 0 && smart->value[d][v] > 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "vsn=%s disk=%d : SMART value (%d) %s = %Ld indicates a potential disk problem", vsn, d, id, desc, smart->value[d][v]);

				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			}
			if(critical < 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "vsn=%s disk=%d : SMART id = %d with value %Ld not known by this software", vsn, d, id, smart->value[d][v]);

				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
			}
		}
	}
	Logger_logData(D->log, "\n");

	return 0;
}

void Mk5Daemon_sendSmartData(Mk5Daemon *D)
{
	for(int bank = 0; bank < N_BANK; ++bank)
	{
		const Mk5Smart *mk5smart = &(D->smartData[bank]);
		const char *vsn = D->vsns[bank];

		for(int d = 0; d < N_SMART_DRIVES; ++d)
		{
			if(mk5smart->nValue[d] > 0)
			{
				difxMessageSendDifxSmart(mk5smart->mjd, vsn, d, mk5smart->nValue[d], mk5smart->id[d], mk5smart->value[d]);
			}
		}
	}
}

#if 0
void Mk5Daemon_getSmart(Mk5Daemon *D)
{
	int n;
	DifxMessageMk5Status dm;

	if(!D->isMk5)
	{
		return;
	}

	memset(&dm, 0, sizeof(DifxMessageMk5Status));

	pthread_mutex_lock(&D->processLock);

	if(D->process != PROCESS_NONE)
	{
		int v = lockStreamstor(D, "getsmart", 0);
		if(v >= 0)
		{
			unlockStreamstor(D, "getsmart");
			D->process = PROCESS_NONE;
		}
	}

	switch(D->process)
	{
	case PROCESS_NONE:
		n = XLR_get_smart(D);
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
}
#endif

int extractSmartTemps(char *tempstr, const Mk5Daemon *D, int bank)
{
	int i, j, t;
	int n = 0;

	if(bank < 0 || bank > 1)
	{
		return -1;
	}

	const Mk5Smart *mk5smart = &(D->smartData[bank]);

	tempstr[0] = 0;

	for(i = 0; i < N_SMART_DRIVES; ++i)
	{
		t = -1;
		for(j = 0; j < mk5smart->nValue[i]; ++j)
		{
			if(mk5smart->id[i][j] == 194)
			{
				t = (int)(mk5smart->value[i][j]);
				break;
			}
		}
	
		n += snprintf(tempstr+n, SMART_TEMP_STRING_LENGTH-n, "%s%d", (i == 0 ? "" : " "), t);
	}

	return 0;
}
