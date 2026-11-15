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

	v = snprintf(response, maxResponseLength, "!%s? 0 : mk5daemon-noSS : %s : %s : 1;", fields[0], VERSION, D->hostName);

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

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

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

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int protect_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

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
	if(v == 0)
	{
		v = snprintf(response, maxResponseLength, "!%s? 0 : 0 : : 0;", fields[0]);
	}

	return v;
}

int disk_model_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
	
	return v;
}

int disk_model_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
	
	return v;
}

int disk_serial_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
	
	return v;
}

int disk_size_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);
	
	return v;
}

int dir_info_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int pointers_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

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

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

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

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

	return v;
}


int start_stats_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int start_stats_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

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

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int disk_state_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

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

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int scan_set_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

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

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int record_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int reset_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int recover_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

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

	if(D->recordState != RECORD_OFF)
	{
		status |= 0x0040;
	}
	if(D->recordState == RECORD_HALTED)
	{
		status |= 0x0080;
	}

	v = snprintf(response, maxResponseLength, "!%s? 0 : 0x%08x;", fields[0], status);

	return v;
}

int VSN_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s = 2 : Not implemented on this DTS;", fields[0]);

	return v;
}

int VSN_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v = 0;

	v = snprintf(response, maxResponseLength, "!%s? 2 : Not implemented on this DTS;", fields[0]);

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
