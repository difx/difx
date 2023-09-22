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
#include <xlrapi.h>
#include <unistd.h>
#include <ctype.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include "mk5daemon.h"
#include "watchdog.h"

int Mk5Daemon_getStreamstorVersions(Mk5Daemon *D)
{
	S_XLRSWREV swVersion;
	S_DEVINFO devInfo;
	S_DBINFO dbInfo;
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	unsigned int xlrError;
	char message[DIFX_MESSAGE_LENGTH];
	char xlrErrorStr[XLR_ERROR_LENGTH];
	const char id[]="GetStreamstorVersions";
	int v;

	v = lockStreamstor(D, id, MARK5_LOCK_DONT_WAIT);
	if(v < 0)
	{
		return 0;
	}
	
	WATCHDOG( xlrRC = XLROpen(1, &xlrDevice) );
	++D->nXLROpen;
	if(xlrRC != XLR_SUCCESS)
	{
		WATCHDOG( xlrError = XLRGetLastError() );
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"ERROR: Mk5Daemon_getStreamstorVersions: "
			"Cannot open streamstor card.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		WATCHDOG( XLRClose(xlrDevice) );

		unlockStreamstor(D, id);

		return 1;
	}

	WATCHDOG( xlrRC = XLRGetVersion(xlrDevice, &swVersion) );
	if(xlrRC != XLR_SUCCESS)
	{
		WATCHDOG( xlrError = XLRGetLastError() );
		WATCHDOG( XLRGetErrorMessage(xlrErrorStr, xlrError) );
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"ERROR: Mk5Daemon_getStreamstorVersions: "
			"Cannot get StreamStor versions. "
			"Error=%u (%s)\n",
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		WATCHDOG( XLRClose(xlrDevice) );

		unlockStreamstor(D, id);

		return 2;
	}

	strcpy(D->mk5ver.ApiVersion, swVersion.ApiVersion);
	strcpy(D->mk5ver.ApiDateCode, swVersion.ApiDateCode);
	strcpy(D->mk5ver.FirmwareVersion, swVersion.FirmwareVersion);
	strcpy(D->mk5ver.FirmDateCode, swVersion.FirmDateCode);
	strcpy(D->mk5ver.MonitorVersion, swVersion.MonitorVersion);
	strcpy(D->mk5ver.XbarVersion, swVersion.XbarVersion);
	strcpy(D->mk5ver.AtaVersion, swVersion.AtaVersion);
	strcpy(D->mk5ver.UAtaVersion, swVersion.UAtaVersion);
	strcpy(D->mk5ver.DriverVersion, swVersion.DriverVersion);

	WATCHDOG( xlrRC = XLRGetDeviceInfo(xlrDevice, &devInfo) );
	if(xlrRC != XLR_SUCCESS)
	{
		WATCHDOG( xlrError = XLRGetLastError() );
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"ERROR: Mk5Daemon_getStreamstorVersions: "
			"Cannot get Device information. "
			"Error=%u (%s)\n",
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		WATCHDOG( XLRClose(xlrDevice) );

		unlockStreamstor(D, id);

		return 2;
	}

	strcpy(D->mk5ver.BoardType, devInfo.BoardType);
	D->mk5ver.SerialNum = devInfo.SerialNum;

	if(strncasecmp(D->mk5ver.BoardType, "Amazon", 6) == 0)
	{
		WATCHDOG( xlrRC = XLRGetDBInfo(xlrDevice, &dbInfo) );
		if(xlrRC != XLR_SUCCESS)
		{
			WATCHDOG( xlrError = XLRGetLastError() );
			WATCHDOG( XLRGetErrorMessage(xlrErrorStr, xlrError) );
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"ERROR: Mk5Daemon_getStreamstorVersions: "
				"Cannot get Daughter Board information. "
				"Error=%u (%s)\n",
				xlrError,
				xlrErrorStr);
			Logger_logData(D->log, message);
			WATCHDOG( XLRClose(xlrDevice) );

			unlockStreamstor(D, id);

			return 2;
		}
		D->mk5ver.DB_SerialNum = dbInfo.SerialNum;
		strcpy(D->mk5ver.DB_PCBVersion, dbInfo.PCBVersion);
		strcpy(D->mk5ver.DB_PCBType, dbInfo.PCBType);
		strcpy(D->mk5ver.DB_PCBSubType, dbInfo.PCBSubType);
		strcpy(D->mk5ver.DB_FPGAConfig, dbInfo.FPGAConfig);
		strcpy(D->mk5ver.DB_FPGAConfigVersion, dbInfo.FPGAConfigVersion);
		D->mk5ver.DB_NumChannels = dbInfo.NumChannels;
	}

	WATCHDOG( XLRClose(xlrDevice) );

	unlockStreamstor(D, id);

	return 0;
}

int Mk5Daemon_sendStreamstorVersions(Mk5Daemon *D)
{
	return difxMessageSendMk5Version(&D->mk5ver);
}

int logStreamstorVersions(Mk5Daemon *D)
{
	char message[DIFX_MESSAGE_LENGTH];
	snprintf(message, DIFX_MESSAGE_LENGTH,
		"XLR Versions: Api=%s(%s) Firmware=%s(%s) "
		"Monitor=%s Xbar=%s Ata=%s UAta=%s Driver=%s\n",
		D->mk5ver.ApiVersion,
		D->mk5ver.ApiDateCode,
		D->mk5ver.FirmwareVersion,
		D->mk5ver.FirmDateCode,
		D->mk5ver.MonitorVersion,
		D->mk5ver.XbarVersion,
		D->mk5ver.AtaVersion,
		D->mk5ver.UAtaVersion,
		D->mk5ver.DriverVersion);
	Logger_logData(D->log, message);

	snprintf(message, DIFX_MESSAGE_LENGTH,
		"Streamstor: BoardType=%s SerialNum=%d\n", 
		D->mk5ver.BoardType,
		D->mk5ver.SerialNum);
	Logger_logData(D->log, message);
	
	snprintf(message, DIFX_MESSAGE_LENGTH,
		"DaughterBoard: PCBType=%s PCBSubType=%s PCBVersion=%s "
		"FPGAConfig=%s FPGAConfigVersion=%s\n",
		D->mk5ver.DB_PCBType,
		D->mk5ver.DB_PCBSubType,
		D->mk5ver.DB_PCBVersion,
		D->mk5ver.DB_FPGAConfig,
		D->mk5ver.DB_FPGAConfigVersion);
	Logger_logData(D->log, message);

	return 0;
}
