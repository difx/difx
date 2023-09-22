/***************************************************************************
 *   Copyright (C) 2011-2013 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the Lesser GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   Lesser GNU General Public License for more details.                   *
 *                                                                         *
 *   You should have received a copy of the Lesser GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/vsis.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctype.h>
#include <mark5ipc.h>
#include "config.h"
#include "vsis_commands.h"
#include "../mk5dir/mark5directorystructs.h"

const long long minFreeSpace = 10000000;

int DTS_id_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	v = snprintf(response, maxResponseLength, "!%s? 0 : mk5daemon : %s : %s : 1;", fields[0], VERSION, D->hostName);
#else
	v = snprintf(response, maxResponseLength, "!%s? 0 : mk5daemon-noSS : %s : %s : 1;", fields[0], VERSION, D->hostName);
#endif

	return v;
}

int packet_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : %d : %d : %d : %d : %d;", fields[0],
		D->payloadOffset, D->dataFrameOffset, D->packetSize, D->psnMode, D->psnOffset);
}

int filter_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : %u : %u : %u : %u : %u;", fields[0],
		D->totalPackets, D->ethernetPackets, D->addressRejects, D->lengthRejects, D->fscRejects);
}

int packet_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else
	{
		if(nField > 1 && fields[1][0])
		{
			D->payloadOffset = atoi(fields[1]);
		}
		if(nField > 2 && fields[2][0])
		{
			D->dataFrameOffset = atoi(fields[2]);
		}
		if(nField > 3 && fields[3][0])
		{
			D->packetSize = atoi(fields[3]);
		}
		if(nField > 4 && fields[4][0])
		{
			D->psnMode = atoi(fields[4]);
		}
		if(nField > 5 && fields[5][0])
		{
			D->psnOffset = atoi(fields[5]);
		}

		v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
	}

	return v;
}

int bank_set_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int newBank = -1;
	int v = 0;

	if(nField < 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Bank name must be A, B or INC, case insensitive;", fields[0]);
	}
	else if(nField > 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Only one parameter allowed;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else if(strlen(fields[1]) == 1)
	{
		newBank = toupper(fields[1][0]) - 'A';
		if(newBank < 0 || newBank >= N_BANK)
		{
			newBank = -1;

			v = snprintf(response, maxResponseLength, "!%s = 6 : Bank name must be A, B or INC, case insensitive;", fields[0]);
		}
	}
	else if(strcasecmp(fields[1], "INC") == 0)
	{
		if(D->activeBank < 0)
		{
			newBank = 0;
		}
		else
		{
			newBank = (D->activeBank + 1) % N_BANK;
		}
	}
	if(newBank >= 0)
	{
		if(D->vsns[newBank][0] == 0)
		{
			v =  snprintf(response, maxResponseLength, "!%s = 1 : No disk mounted in bank %c;", fields[0], 'A'+newBank);
		}
		else
		{
			D->activeBank = newBank;

			v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
		}
	}

	return v;
}

int bank_set_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int otherBank;
	int bank;
	int v;

	/* FIXME: report extended VSN? */

	if(D->activeBank >= 0)
	{
		bank = D->activeBank;
	}
	else
	{
		bank = 0;
	}

	otherBank = (bank + 1) % N_BANK;

	v = snprintf(response, maxResponseLength, "!%s? 0 : %c : %s : %c : %s;", fields[0],
		(D->vsns[bank][0]      ? ('A' + bank)       : '-'),
		(D->vsns[bank][0]      ? D->vsns[bank]      : "-"),
		(D->vsns[otherBank][0] ? ('A' + otherBank)  : '-'),
		(D->vsns[otherBank][0] ? D->vsns[otherBank] : "-") );

	return v;
}

int SS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	char dbInfo[DIFX_MESSAGE_LENGTH];

	if(strlen(D->mk5ver.DB_PCBVersion) > 0)
	{
		/* FIXME: get DBSerialNum and DBNumChannels */
		sprintf(dbInfo, " : DBSerialNum %d : DBPCBVersion %s : DBPCBType %s : DBPCBSubType %s : DBFPGConfig %s : DBFPGAConfigVers %s : DBNumChannels %d",
			D->mk5ver.DB_SerialNum,
			D->mk5ver.DB_PCBVersion, 
			D->mk5ver.DB_PCBType,
			D->mk5ver.DB_PCBSubType,
			D->mk5ver.DB_FPGAConfig,
			D->mk5ver.DB_FPGAConfigVersion,
			D->mk5ver.DB_NumChannels);
	}
	else
	{
		dbInfo[0] = 0;
	}

	v = snprintf(response, maxResponseLength, "!%s? 0 : BoardType %s : SerialNum %d : APIVersion %s : APIDateCode %s : FirmwareVersion %s : FirmDateCode %s : MonitorVersion %s : XBarVersion %s : ATAVersion %s : UATAVersion %s : DriverVersion %s%s;", fields[0],
		D->mk5ver.BoardType,
		D->mk5ver.SerialNum,
		D->mk5ver.ApiVersion,
		D->mk5ver.ApiDateCode,
		D->mk5ver.FirmwareVersion,
		D->mk5ver.FirmDateCode,
		D->mk5ver.MonitorVersion,
		D->mk5ver.XbarVersion,
		D->mk5ver.AtaVersion,
		D->mk5ver.UAtaVersion,
		D->mk5ver.DriverVersion,
		dbInfo);
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int OS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	FILE *pin;
	int n, v;
	const char command[] = "uname -rms";
	char A[3][32];

	A[0][0] = A[1][0] = A[2][0] = 0;

	pin = popen(command, "r");
	if(!pin)
	{
		v = snprintf(response, maxResponseLength, "!%s? 1 : Unable to execute : %s;", fields[0], command);
	}
	else
	{
		n = fscanf(pin, "%s%s%s", A[0], A[1], A[2]);
		pclose(pin);

		if(n == 3)
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %s : %s : %s;", fields[0], D->hostName, A[0], A[1], A[2]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 4 : Unable to parse output of %s;", fields[0], command);
		}
	}

	return v;
}

int fill_pattern_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = -1;
	unsigned int p;

	if(nField != 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : One parameter must be supplied;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else
	{
		if(strlen(fields[1]) < 3 || fields[1][0] != '0' || fields[1][1] != 'x')
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Hexadecimal value expected;", fields[0]);
		}
		else
		{
			v = sscanf(fields[1]+2, "%x", &p);
			if(v != 1)
			{
				v = snprintf(response, maxResponseLength, "!%s = 6 : Hexadecimal value expected;", fields[0]);
			}
			else
			{
				v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
			}
		}
	}

	return v;
}

int fill_pattern_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : 0x%08x;", fields[0], D->fillPattern);
}

int protect_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	enum WriteProtectState state;
	char msg[256];

	if(nField != 2 || (strcasecmp(fields[1], "on") != 0 && strcasecmp(fields[1], "off") != 0))
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : on or off expected;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : No module mounted;", fields[0]);
	}
	else
	{
		state = (strcasecmp(fields[1], "on") == 0 ? PROTECT_ON : PROTECT_OFF);
		v = Mk5Daemon_setProtect(D, state, msg);

		if(v < 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Error %d encountered;", fields[0], -v);
		}
		else if(v == 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = %d;", fields[0], v);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int protect_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	int p;

	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		p = D->bank_stat[D->activeBank].WriteProtected;
		v = snprintf(response, maxResponseLength, "!%s? 0 : %s;", fields[0], (p ? "on" : "off") );
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int error_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;
	const int MaxErrorLength=256;
	char msg[MaxErrorLength];
	int n;	/* Number of errors left on stack */

	/* Errors are shown with the following priority 
	 * 1. anything in the D->errors list
	 * 2. Mk5Daemon_error() (Mark5 only)
	 * 3. if #2 returns 5 (system busy), then report a possible openStreamstorError (Mark5 only)
	 * 4. report no error
	 *
	 * A final response field returns the number of unread errors
	 */

	n = Mk5Daemon_popVSIError(D, msg, MaxErrorLength);
	if(n > 0)
	{
		--n;

		v = snprintf(response, maxResponseLength, "!%s? 0 : -1 : %s : %d;", fields[0], msg, n);
	}
#ifdef HAVE_XLRAPI_H
	else
	{
		unsigned int xlrError;
		int p;

		p = Mk5Daemon_error(D, &xlrError , msg);

		if(p != 5)
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %u : %s : 0;", fields[0], xlrError, msg);
		}
		else if(D->openStreamstorError)
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : -1 : Error opening streamstor : 0;", fields[0]);
		}
	}
#endif
	if(v == 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 0 : 0 : : 0;", fields[0]);
	}

	return v;
}

int disk_model_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else 
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;
	
		if(smart->mjd < 50000.0)	/* information not populated */
		{
			v = snprintf(response, maxResponseLength, "!%s? 5;", fields[0]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0", fields[0]);

			for(int d = 0; d < N_SMART_DRIVES; ++d)
			{
				const DriveInformation *drive = smart->drive + d;
				int good = (drive->capacity > 0LL);

				v += snprintf(response+v, maxResponseLength-v, " : %s",
					(good ? drive->model : "") );
			}
			v += snprintf(response+v, maxResponseLength-v, ";");
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif
	
	return v;
}

int disk_model_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else 
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;

		if(smart->mjd < 50000.0)	/* information not populated */
		{
			v = snprintf(response, maxResponseLength, "!%s? 5;", fields[0]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0", fields[0]);

			for(int d = 0; d < N_SMART_DRIVES; ++d)
			{
				const DriveInformation *drive = smart->drive + d;
				int good = (drive->capacity > 0LL);

				v += snprintf(response+v, maxResponseLength-v, " : %s",
					(good ? drive->rev : "") );
			}
			v += snprintf(response+v, maxResponseLength-v, ";");
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif
	
	return v;
}

int disk_serial_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;
		
		if(smart->mjd < 50000.0)	/* information not populated */
		{
			v = snprintf(response, maxResponseLength, "!%s? 5;", fields[0]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0", fields[0]);

			for(int d = 0; d < N_SMART_DRIVES; ++d)
			{
				const DriveInformation *drive = smart->drive + d;
				int good = (drive->capacity > 0LL);

				v += snprintf(response+v, maxResponseLength-v, " : %s",
					(good ? drive->serial : "") );
			}
			v += snprintf(response+v, maxResponseLength-v, ";");
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif
	
	return v;
}

int disk_size_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;
		
		if(smart->mjd < 50000.0)	/* information not populated */
		{
			v = snprintf(response, maxResponseLength, "!%s? 5;", fields[0]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0", fields[0]);

			for(int d = 0; d < N_SMART_DRIVES; ++d)
			{
				const DriveInformation *drive = smart->drive + d;
				int good = (drive->capacity > 0LL);

				if(good)
				{
					v += snprintf(response+v, maxResponseLength-v, " : %Ld", drive->capacity);
				}
				else
				{
					v += snprintf(response+v, maxResponseLength-v, " :");
				}
			}
			v += snprintf(response+v, maxResponseLength-v, ";");
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif
	
	return v;
}

int dir_info_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;
		
		if(smart->mjd < 50000.0)	/* information not populated */
		{
			v = snprintf(response, maxResponseLength, "!%s? 5 : %f : %d;", fields[0], smart->mjd, D->activeBank);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %d : %Ld : %Ld : %d;", fields[0],
				D->nScan[D->activeBank], D->bytesUsed[D->activeBank], D->bytesTotal[D->activeBank], D->dirVersion[D->activeBank]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int pointers_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;
		
		if(smart->mjd < 50000.0)	/* information not populated */
		{
			v = snprintf(response, maxResponseLength, "!%s? 5;", fields[0]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %Ld : %Ld : %Ld;", fields[0],
				D->bytesUsed[D->activeBank], D->startPointer[D->activeBank], D->stopPointer[D->activeBank]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int personality_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : mark5C : bank;", fields[0]);
}

int personality_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	if(nField != 3)
	{
		 v = snprintf(response, maxResponseLength, "!%s = 6 : Two parameters must be supplied;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else
	{
		if(strcasecmp(fields[1], "mark5C") != 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Only mark5C personality is supported;", fields[0]);
		}
		else if(strcasecmp(fields[2], "bank") != 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Only bank-mode is supported;", fields[0]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
		}
	}

	return v;
}

int bank_info_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	int bank;
	char bankStr[N_BANK][60];

	if(D->activeBank >= 0)
	{
		bank = D->activeBank;
	}
	else
	{
		bank = 0;
	}

	for(int b = 0; b < N_BANK; ++b)
	{
		const Mk5Smart *smart = D->smartData + bank;

		if(D->vsns[bank][0])
		{
			if(smart->mjd >= 50000.0)
			{
				long long left = D->bytesTotal[bank] - D->bytesUsed[bank];
				if(left < 0LL)
				{
					left = 0LL;
				}
				sprintf(bankStr[b], "%c : %Ld", 'A'+bank, left);
			}
			else
			{
				sprintf(bankStr[b], "? : 0");
			}
		}
		else
		{
			sprintf(bankStr[b], "- : 0");
		}
	
		bank = (bank + 1) % N_BANK;
	}

	v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %s;", fields[0], bankStr[0], bankStr[1]);
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int net_protocol_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : %s;", fields[0], netProtocolStrings[D->netProtocol]);
}

int net_protocol_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = -1;

	if(nField != 1)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : One parameter must be supplied;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else
	{
		for(int n = 0; n < NUM_NET_PROTOCOLS; ++n)
		{
			if(strcasecmp(fields[1], netProtocolStrings[n]) == 0)
			{
				D->netProtocol = static_cast<NetProtocolType>(n);
				v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);

				break;
			}
		}

		if(v < 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Unrecognized net protocol;" ,fields[0]);
		}
	}

	return v;
}

int disk_state_mask_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : %d : %d : %d;", fields[0], 
		(D->diskStateMask & MODULE_STATUS_ERASED ? 1 : 0), 
		(D->diskStateMask & MODULE_STATUS_PLAYED ? 1 : 0), 
		(D->diskStateMask & MODULE_STATUS_RECORDED ? 1 : 0));
}

int disk_state_mask_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0, d;

	d = D->diskStateMask;

	if(nField > 4)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Up to three parameters allowed;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else
	{
		if(nField > 1 && fields[1][0])
		{
			if(strcmp(fields[1], "1") == 0)
			{
				d = d | MODULE_STATUS_ERASED;
			}
			else if(strcmp(fields[1], "0") == 0)
			{
				d = d & ~MODULE_STATUS_ERASED;
			}
			else
			{
				v = -1;
			}
		}
		if(nField > 2 && fields[2][0])
		{
			if(strcmp(fields[2], "1") == 0)
			{
				d = d | MODULE_STATUS_PLAYED;
			}
			else if(strcmp(fields[2], "0") == 0)
			{
				d = d & ~MODULE_STATUS_PLAYED;
			}
			else
			{
				v = -1;
			}
		}
		if(nField > 3 && fields[3][0])
		{
			if(strcmp(fields[3], "1") == 0)
			{
				d = d | MODULE_STATUS_RECORDED;
			}
			else if(strcmp(fields[3], "0") == 0)
			{
				d = d & ~MODULE_STATUS_RECORDED;
			}
			else
			{
				v = -1;
			}
		}
		if(v == -1)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Parameters must be 0 or 1;", fields[0]);
		}
		else
		{
			D->diskStateMask = d;

			v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
		}
	}

	return v;
}

int get_stats_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const S_DRIVESTATS *stats = D->driveStats[D->activeBank][D->driveStatsIndex[D->activeBank]];
		v = snprintf(response, maxResponseLength, "!%s? 0 : %d : %lu : %lu : %lu : %lu : %lu : %lu : %lu : %lu : %lu;", fields[0],
			D->driveStatsIndex[D->activeBank],
			static_cast<unsigned long int>(stats[0].count),
			static_cast<unsigned long int>(stats[1].count),
			static_cast<unsigned long int>(stats[2].count),
			static_cast<unsigned long int>(stats[3].count),
			static_cast<unsigned long int>(stats[4].count),
			static_cast<unsigned long int>(stats[5].count),
			static_cast<unsigned long int>(stats[6].count),
			static_cast<unsigned long int>(stats[7].count),
			static_cast<unsigned long int>(D->driveStatsReplaced[D->activeBank][D->driveStatsIndex[D->activeBank]]));
		

		D->driveStatsIndex[D->activeBank] = (D->driveStatsIndex[D->activeBank] + 1) % N_DRIVE;
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}


int start_stats_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	const S_DRIVESTATS *stats = D->driveStatsConfig;

	v = snprintf(response, maxResponseLength, "!%s? 0 : %lu : %lu : %lu : %lu : %lu : %lu : %lu;", fields[0],
		static_cast<unsigned long int>(stats[0].range),
		static_cast<unsigned long int>(stats[1].range),
		static_cast<unsigned long int>(stats[2].range),
		static_cast<unsigned long int>(stats[3].range),
		static_cast<unsigned long int>(stats[4].range),
		static_cast<unsigned long int>(stats[5].range),
		static_cast<unsigned long int>(stats[6].range));
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int start_stats_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(nField > XLR_MAXBINS)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Up to %d parameters can be supplied;", fields[0], XLR_MAXBINS-1);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else
	{
		int t[XLR_MAXBINS];
		S_DRIVESTATS *stats = D->driveStatsConfig;

		t[XLR_MAXBINS-1] = -1;

		for(int b = 0; b < XLR_MAXBINS-1; ++b)
		{
			t[b] = stats[b].range;
		}

		for(int f = 1; f < XLR_MAXBINS; ++f)
		{
			t[f-1] = stats[f-1].range;
			if(f >= nField)
			{
				break;
			}
			if(strlen(fields[f]) < 1)
			{
				continue;
			}
			t[f-1] = atoi(fields[f]);
			if(t[f-1] <= 0)
			{
				v = -1;
			}
		}
		if(v == 0)
		{
			for(int b = 1; b < XLR_MAXBINS-1; ++b)
			{
				if(t[b] <= t[b-1])
				{
					v = -1;
				}
			}
			if(v == 0)
			{
				for(int b = 0; b < XLR_MAXBINS-1; ++b)
				{
					stats[b].range = t[b];
				}
				v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
				if(D->activeBank >= 0)
				{
					clearMk5Stats(D, D->activeBank);
				}
			}
			else
			{
				v = snprintf(response, maxResponseLength, "!%s = 6 : Parameters must be monotone increasing;", fields[0]);
			}
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Parameters must be positive integers;", fields[0]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int mode_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	//if(D->format = FORMAT_MARK5C
	{
		v = snprintf(response, maxResponseLength, "!%s? 0 : mark5b : 0x%08x : %d;", fields[0],
			D->bitstreamMask, D->decimationRatio);
	}
	//else
	//{
	//}

	return v;
}

int mode_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	/* FIXME: actually do something! */
	v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);

	return v;
}

int rtime_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;

		if(smart->mjd >= 50000.0)
		{
			double left = D->bytesTotal[D->activeBank] - D->bytesUsed[D->activeBank];
			if(left < 0.0)
			{
				left = 0.0;
			}
			double percent = 100.0*left/D->bytesTotal[D->activeBank];
			double rtime;
			
			if(D->recordRate > 0.0)
			{
				rtime = (left/1.0e6)/(D->recordRate/8.0);
			}
			else
			{
				rtime = 0.0;
			}
		
			v = snprintf(response, maxResponseLength, "!%s? 0 : %3.1f : %4.2f : %5.3f : %s : 0x%08x : %d : %3.1f;", fields[0],
				rtime, left*1.0e-9, percent, D->dataSource, D->bitstreamMask, D->decimationRatio, D->recordRate);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s = 5;", fields[0]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int disk_state_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	if(nField != 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : One parameter expected;", fields[0]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : No module is active;", fields[0]);
	}
	else if(!D->unprotected)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Previous protect off required;", fields[0]);
	}
	else if(strcasecmp(fields[1], "recorded") == 0 || strcasecmp(fields[1], "erased") == 0 || strcasecmp(fields[1], "played") == 0)
	{
		char command[MAX_COMMAND_SIZE];
		char message[DIFX_MESSAGE_LENGTH];

		snprintf(command, MAX_COMMAND_SIZE, "vsn --%s %c", fields[1], 'A'+D->activeBank);
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
		Logger_logData(D->log, message);

		system(command);
		clearModuleInfo(D, D->activeBank);
		Mk5Daemon_getModules(D);

		v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
	}
	else
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Unsupported disk state;", fields[0]);
	}
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

const char *moduleStatusName(int status)
{
	if(status & MODULE_STATUS_RECORDED)
	{
		return "recorded";
	}
	else if(status & MODULE_STATUS_PLAYED)
	{
		return "played";
	}
	else if(status & MODULE_STATUS_ERASED)
	{
		return "erased";
	}

	return "unknown";
}

int disk_state_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

#ifdef HAVE_XLRAPI_H
	int bank;
	char bankStr[N_BANK][60];

	if(D->activeBank >= 0)
	{
		bank = D->activeBank;
	}
	else
	{
		bank = 0;
	}

	for(int b = 0; b < N_BANK; ++b)
	{
		const Mk5Smart *smart = D->smartData + bank;

		if(D->vsns[bank][0])
		{
			if(smart->mjd >= 50000.0)
			{
				sprintf(bankStr[b], "%c : %s", 'A'+bank, moduleStatusName(D->diskModuleState[bank]));
			}
			else
			{
				sprintf(bankStr[b], "%c : unknown", 'A'+bank);
			}
		}
		else
		{
			sprintf(bankStr[b], "- : unknown");
		}
	
		bank = (bank + 1) % N_BANK;
	}

	v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %s ;", fields[0], bankStr[0], bankStr[1]);
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int scan_set_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module mounted;", fields[0]);
	}
	else
	{
		const Mk5Smart *smart = D->smartData + D->activeBank;

		if(smart->mjd >= 50000.0)
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %Ld : %Ld;", fields[0],
				D->scanLabel[D->activeBank], D->startPointer[D->activeBank], D->stopPointer[D->activeBank]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s = 5;", fields[0]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int scan_set_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented yet;", fields[0]);

	return v;
}

int record_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 0 : off : 0 : ;", fields[0]);
	}
	else
	{
		if(D->recordState == RECORD_OFF)
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %d : %s;", fields[0],
				recordStateStrings[D->recordState], D->nScan[D->activeBank], D->scanLabel[D->activeBank]);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %d : %s : %Ld : %4.2f;", fields[0],
				recordStateStrings[D->recordState], D->nScan[D->activeBank], D->scanLabel[D->activeBank], D->bytesUsed[D->activeBank], D->recordRate);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int record_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	char macFilterOptions[1000] = "";
	char packetFilterOptions[1000] = "";
	char command[MAX_COMMAND_SIZE];
	char message[1200];
	int p;

	if(nField < 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : One to four parameters needed;", fields[0]);
	}
	else if(strcasecmp(fields[1], "on") == 0)
	{
		char scanLabel[MODULE_LEGACY_SCAN_LENGTH];

		switch(nField)
		{
			case 1:
			case 2:
				// this error case is captured later
				break;
			case 3:
				snprintf(scanLabel, MODULE_LEGACY_SCAN_LENGTH, "%s", fields[2]);
				break;
			case 4:
				snprintf(scanLabel, MODULE_LEGACY_SCAN_LENGTH, "%s_X_%s", fields[3], fields[2]);
				break;
			default:
				snprintf(scanLabel, MODULE_LEGACY_SCAN_LENGTH, "%s_%s_%s", fields[3], fields[4], fields[2]);
				break;
		}
		if(D->recordState != RECORD_OFF)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Already recording;", fields[0]);
		}
		else if(nField < 3)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Scan name rquired;", fields[0]);
		}
		else if(D->activeBank < 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : No module mounted;", fields[0]);
		}
		else if(D->bank_stat[D->activeBank].WriteProtected)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Disk is write protected;", fields[0]);
		}
		else if(D->dirVersion[D->activeBank] < 1)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Module directory version %d not supported;", fields[0],
				D->dirVersion[D->activeBank]);
		}
		else if(D->bytesTotal[D->activeBank] - D->bytesUsed[D->activeBank] < minFreeSpace)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Insufficient disk free;", fields[0]);
		}
		else if(strlen(scanLabel) > 39)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Scan name %s is too long (39 chars max);", fields[0], scanLabel);
		}
		else
		{

			if(D->packetSize == 0)
			{
				snprintf(packetFilterOptions, 1000, "--psnmode %d --payloadoffset %d", D->psnMode, D->payloadOffset);
			}
			else
			{
				snprintf(packetFilterOptions, 1000, "--psnmode %d --payloadoffset %d --packetsize %d", D->psnMode, D->payloadOffset, D->packetSize);
			}

			p = 0;
			for(std::map<MAC,bool>::const_iterator it = D->macList->begin(); it != D->macList->end(); ++it)
			{
				if(it->second)	/* if MAC is enabled */
				{
					char macStr[100];

					it->first.toString(macStr);
					p += snprintf(macFilterOptions+p, 1000-p, "--mac %s ", macStr);
				}
			}

			if(p == 0 && !D->macList->empty())	/* filtering on, but all disabled */
			{
				v = snprintf(response, maxResponseLength, "!%s = 4 : MAC filtering requested but all addresses are disabled;", fields[0]);
			}
			else
			{
				snprintf(command, MAX_COMMAND_SIZE, "record5c %s %s %c %s", macFilterOptions, packetFilterOptions, 'A'+D->activeBank, scanLabel);
				snprintf(message, 1200, "Executing into pipe: %s\n", command);
				Logger_logData(D->log, message);
				D->recordPipe = popen(command, "r");
				if(!D->recordPipe)
				{
					v = snprintf(response, maxResponseLength, "!%s = 4 : Execution of record5c failed;", fields[0]);
				}
				else
				{
					setlinebuf(D->recordPipe);
					D->recordState = RECORD_ON;
					D->recordT0 = time(0);
					++D->nScan[D->activeBank];
					strcpy(D->scanLabel[D->activeBank], scanLabel);
					v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
				}
			}
		}
	}
	else if(strcasecmp(fields[1], "off") == 0)
	{
		if(D->recordState == RECORD_OFF)
		{
			v = snprintf(response, maxResponseLength, "!%s = 2 : Not recording;", fields[0]);
		}
		else
		{
			int r;

			system("killall -INT record5c");
			r = Mk5Daemon_stopRecord(D);
			if(r == 0)
			{
				v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
			}
			else
			{
				v = snprintf(response, maxResponseLength, "!%s = 4 : Record stop not complete;", fields[0]);
			}
			D->stopRecordRequestTime = time(0);
		}
	}
	else if(strcasecmp(fields[1], "kill") == 0)
	{
		if(D->recordState == RECORD_OFF)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Not recording;", fields[0]);
		}
		else
		{
			system("killall -9 record5c");
			v = snprintf(response, maxResponseLength, "!%s = 1;", fields[0]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int reset_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(nField < 2 || nField > 3)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : One to two parameters needed;", fields[0]);
	}
	if(strcasecmp(fields[1], "mount") == 0 || strcasecmp(fields[1], "dismount") == 0)
	{
		if(nField != 3)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Two parameters needed for %s;", fields[0], fields[1]);
		}
		else if(strlen(fields[2]) != 1)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Second parameter must be A or B;", fields[0]);
		}
		else
		{
			int bank = toupper(fields[2][0]) - 'A';
			
			if(bank < 0 || bank >= N_BANK)
			{
				v = snprintf(response, maxResponseLength, "!%s = 6 : Second parameter must be A or B;", fields[0]);
			}
			else if(bank == D->activeBank && D->recordState != RECORD_OFF)
			{
				v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
			}
			else
			{
				if(strcasecmp(fields[1], "mount") == 0)
				{
					Mk5Daemon_diskOn(D, fields[2]);
				}
				else
				{
					Mk5Daemon_diskOff(D, fields[2]);
				}
				v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
			}
		}
	}
	else if(strcasecmp(fields[1], "erase") == 0)
	{
		if(nField != 2)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : One parameter expected for %s;", fields[0], fields[1]);
		}
		else if(D->recordState != RECORD_OFF)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
		}
		else if(!D->unprotected)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Previous protect off required;", fields[0]);
		}
		else if(D->activeBank < 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : No module is active;", fields[0]);
		}
		else
		{
			char command[MAX_COMMAND_SIZE];
			char message[DIFX_MESSAGE_LENGTH];

			snprintf(command, MAX_COMMAND_SIZE, "mk5erase --force --newdir %s", D->vsns[D->activeBank]);
			snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
			Logger_logData(D->log, message);

			system(command);
			clearModuleInfo(D, D->activeBank);
			Mk5Daemon_getModules(D);

			v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
		}
	}
	else
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : This reset mode not supported;", fields[0]);
	}

#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int recover_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	int mode = 1;

	if(nField > 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Zero or one parameters needed;", fields[0]);
	}
	else 
	{
		if(nField == 2)
		{
			mode = atoi(fields[1]);
		}
		if(mode < 0 || mode > 3)
		{
			v = snprintf(response, maxResponseLength, "!%s = 6 : Recover mode must be 0, 1, or 2;", fields[0]);
		}
		else if(D->recordState != RECORD_OFF)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
		}
		else if(!D->unprotected)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : Previous protect off required;", fields[0]);
		}
		else if(D->activeBank < 0)
		{
			v = snprintf(response, maxResponseLength, "!%s = 4 : No module is active;", fields[0]);
		}
		else
		{
			char command[MAX_COMMAND_SIZE];
			char message[DIFX_MESSAGE_LENGTH];

			snprintf(command, MAX_COMMAND_SIZE, "recover --force %d %c", mode, 'A'+D->activeBank);
			snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
			Logger_logData(D->log, message);

			system(command);
			clearModuleInfo(D, D->activeBank);
			Mk5Daemon_getModules(D);

			v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int status_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;
	unsigned int status = 0;

	/* FIXME: use #defines for these bits */
	if(D->systemReady)
	{
		status |= 0x0001;
	}
	if(!D->errors->empty())
	{
		status |= 0x0002;
	}
#ifdef HAVE_XLRAPI_H
	if(D->openStreamstorError)
	{
		status |= 0x0002;
	}
	if(getMark5LockValue())
	{
		status |= 0x0004;
	}
#endif

	if(D->recordState != RECORD_OFF)
	{
		status |= 0x0040;
	}
	if(D->recordState == RECORD_HALTED)
	{
		status |= 0x0080;
	}

#ifdef HAVE_XLRAPI_H
	if(D->activeBank >= 0 && D->recordState == RECORD_OFF && (status | 0x0002) && D->systemReady)
	{
		if(D->smartData[D->activeBank].mjd > 50000 && D->bank_stat[D->activeBank].WriteProtected == 0)
		{
			status |= 0x40000;
		}
	}

	if(D->activeBank == 0)
	{
		status |= 0x100000;
	}
	if(D->smartData[0].mjd > 50000 && D->vsns[0][0])
	{
		status |= 0x200000;
		if(D->bank_stat[0].MediaStatus == MEDIASTATUS_FULL || D->bank_stat[0].MediaStatus == MEDIASTATUS_FAULTED)
		{
			status |= 0x400000;
		}
		if(D->bank_stat[0].WriteProtected)
		{
			status |= 0x800000;
		}
	}

	if(D->activeBank == 1)
	{
		status |= 0x1000000;
	}
	if(D->smartData[1].mjd > 50000 && D->vsns[1][0])
	{
		status |= 0x2000000;
		if(D->bank_stat[1].MediaStatus == MEDIASTATUS_FULL || D->bank_stat[1].MediaStatus == MEDIASTATUS_FAULTED)
		{
			status |= 0x4000000;
		}
		if(D->bank_stat[1].WriteProtected)
		{
			status |= 0x8000000;
		}
	}
#endif
	v = snprintf(response, maxResponseLength, "!%s? 0 : 0x%08x;", fields[0], status);

	return v;
}

int VSN_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(nField != 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : One parameter expected for %s;", fields[0], fields[1]);
	}
	else if(D->recordState != RECORD_OFF)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Not while recording;", fields[0]);
	}
	else if(!D->unprotected)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : Previous protect off required;", fields[0]);
	}
	else if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s = 4 : No module is active;", fields[0]);
	}
	else
	{
		char command[MAX_COMMAND_SIZE];
		char message[DIFX_MESSAGE_LENGTH];

		snprintf(command, MAX_COMMAND_SIZE, "vsn --force %c %s", 'A'+D->activeBank, fields[1]);
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
		Logger_logData(D->log, message);

		system(command);
		clearModuleInfo(D, D->activeBank);
		Mk5Daemon_getModules(D);

		v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
	}
#else
	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int VSN_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

#ifdef HAVE_XLRAPI_H
	if(D->activeBank < 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 4 : No module is active;", fields[0]);
	}
	else
	{
		char label[XLR_LABEL_LENGTH+1];

		snprintf(label, XLR_LABEL_LENGTH+1, "%s", D->bank_stat[D->activeBank].Label);
		if(D->bank_stat[D->activeBank].ErrorCode)
		{
			/* FIXME: add additional disk info in case of failure */
			v = snprintf(response, maxResponseLength, "!%s? 0 : %s : Fail;", fields[0], label);
		}
		else
		{
			v = snprintf(response, maxResponseLength, "!%s? 0 : %s : OK;", fields[0], label);
		}
	}
#else
	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
#endif

	return v;
}

int MAC_list_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;
	std::map<MAC,bool> origMacList(*(D->macList));

	if(nField < 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : At least one parameter required;", fields[0]);
	}
	else
	{
		int mc;

		for(int p = 1; p < nField; ++p)
		{
			for(mc = 0; mc < NUM_MAC_LIST_COMMANDS; ++mc)
			{
				if(strcasecmp(fields[p], MacListCommandStrings[mc]) == 0)
				{
					break;
				}
			}
			if(mc == NUM_MAC_LIST_COMMANDS)
			{
				v = snprintf(response, maxResponseLength, "!%s = 6 : Illegal MAC_list command '%s';", fields[0], fields[p]);
				break;
			}
			else if(mc == MAC_LIST_FLUSH)
			{
				D->macList->clear();
			}
			else if(p >= nField - 1)
			{
				v = snprintf(response, maxResponseLength, "!%s = 6 : A MAC address is required for command '%s';", fields[0], fields[p]);
				break;
			}
			else
			{
				MAC mac;
				std::map<MAC,bool>::iterator it;

				if(mac.parse(fields[p+1]) < 0)
				{
					v = snprintf(response, maxResponseLength, "!%s = 6 : Malformed MAC address %s;", fields[0], fields[p+1]);
					break;
				}

				it = D->macList->find(mac);
				
				if(mc == MAC_LIST_ADD)
				{
					(*(D->macList))[mac] = true;	/* add and enable */
				}
				else if(mc == MAC_LIST_DELETE)
				{
					if(it != D->macList->end())
					{
						D->macList->erase(it);
					}
					else
					{
						v = snprintf(response, maxResponseLength, "!%s = 6 : MAC %s not in list;", fields[0], fields[p+1]);
						break;
					}
				}
				else if(mc == MAC_LIST_ENABLE)
				{
					if(it != D->macList->end())
					{
						it->second = true;
					}
					else
					{
						v = snprintf(response, maxResponseLength, "!%s = 6 : MAC %s not in list;", fields[0], fields[p+1]);
						break;
					}
				}
				else if(mc == MAC_LIST_DISABLE)
				{
					if(it != D->macList->end())
					{
						it->second = false;
					}
					else
					{
						v = snprintf(response, maxResponseLength, "!%s = 6 : MAC %s not in list;", fields[0], fields[p+1]);
						break;
					}
				}
				++p;	/* skip the MAC address and get to next arg */
			}
		}
	}

	if(D->macList->size() > MAX_MACLIST_LENGTH)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Resultant MAC list too long (%d > %d);", fields[0], static_cast<int>(D->macList->size()), MAX_MACLIST_LENGTH);
	}

	if(v > 0)	/* something went wrong.  Undo all */
	{
		*(D->macList) = origMacList;	
	}
	else
	{
		v = snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
	}

	return v;
}

int MAC_list_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;
	std::map<MAC,bool>::const_iterator it;
	char macStr[200];

	v = snprintf(response, maxResponseLength, "!%s? 0", fields[0]);

	if(!D->macList->empty())
	{
		for(it = D->macList->begin(); it != D->macList->end(); ++it)
		{
			it->first.toString(macStr);
			v += snprintf(response+v, maxResponseLength-v, " : %s : %s", macStr, it->second ? "enabled" : "disabled");
		}
	}

	v += snprintf(response+v, maxResponseLength-v, ";");

	return v;
}
