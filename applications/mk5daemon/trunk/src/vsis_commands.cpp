#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctype.h>
#include "config.h"
#include "vsis_commands.h"

int DTS_id_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : mk5daemon : %s : %s : 1;", fields[0], VERSION, D->hostName);
}

int packet_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s? 0 : %d : %d : %d : %d : %d;", fields[0],
		D->payloadOffset, D->dataFrameOffset, D->packetSize, D->psnMode, D->psnOffset);
}

int packet_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
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

	return snprintf(response, maxResponseLength, "!%s = 0;", fields[0]);
}

int bank_set_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int newBank = -1;
	int v;

	if(nField < 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Bank name must be A, B or INC, case insensitive;", fields[0]);
	}
	else if(nField > 2)
	{
		v = snprintf(response, maxResponseLength, "!%s = 6 : Only one parameter allowed;", fields[0]);
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
	int v;

	otherBank = (D->activeBank + 1) % N_BANK;

	v = snprintf(response, maxResponseLength, "!%s? 0 : %c : %s : %c : %s;", fields[0],
		(D->vsns[D->activeBank][0] ? ('A' + D->activeBank)  : '-'),
		(D->vsns[D->activeBank][0] ? D->vsns[D->activeBank] : "-"),
		(D->vsns[otherBank][0]     ? ('A' + otherBank)      : '-'),
		(D->vsns[otherBank][0]     ? D->vsns[otherBank]     : "-") );

	return v;
}

int SS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	int v;
	char dbInfo[DIFX_MESSAGE_LENGTH];

	if(strlen(D->mk5ver.DB_PCBVersion) > 0)
	{
		/* FIXME: get DBSerialNum and DBNumChannels */
		sprintf(dbInfo, " : DBPCBVersion %s : DBPCBType %s : DBPCBSubType %s : DBFPGConfig %s : DBFPGAConfigVers %s",
			D->mk5ver.DB_PCBVersion, 
			D->mk5ver.DB_PCBType,
			D->mk5ver.DB_PCBSubType,
			D->mk5ver.DB_FPGAConfig,
			D->mk5ver.DB_FPGAConfigVersion);
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

	return v;
}

int OS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	FILE *pin;
	int v;
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
		fscanf(pin, "%s%s%s", A[0], A[1], A[2]);
		fclose(pin);

		v = snprintf(response, maxResponseLength, "!%s? 0 : %s : %s : %s : %s;", fields[0], D->hostName, A[0], A[1], A[2]);
	}

	return v;
}
