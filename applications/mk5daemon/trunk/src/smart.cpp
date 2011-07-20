#include <string.h>
#include <stdio.h>
#include "mk5daemon.h"
#include "smart.h"

const SmartDescription smartDescriptions[] =
{
	{ 1,   1, "Read error count"},
	{ 3,   0, "Spin up time (ms)"},
	{ 4,   0, "Start/stop count"},
	{ 5,   1, "Reallocated sector count"},
	{ 7,   0, "Seek error count"},
	{ 9,   0, "Power on time (hr)"},
	{ 10,  1, "Spin retry count"},
	{ 11,  0, "Recalibration retry count"},
	{ 12,  0, "Power cycle count"},
	{ 192, 0, "Retract cycle count"},
	{ 193, 0, "Landing zone load count"},
	{ 194, 0, "Temperature (C)"},
	{ 196, 1, "Relocation event count"},
	{ 197, 1, "Questionable sector count"},
	{ 198, 1, "Uncorrectable sector count"},
	{ 199, 0, "DMA CRC error count"},
	{ 200, 0, "Multi-zone error count"},
	{ 201, 1, "Off-track error count"},
	{ 202, 0, "Data Address Mark error count"},
	{ -1,  -1, "Unknown SMART id"}
};



static void trim(char *out, const char *in)
{
	int i, s=-1, e=0;

	for(i = 0; in[i]; i++)
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
	D->smartData[bank].mjd = 0.0;
	D->smartData[bank].vsn[0] = 0;
	
	for(int d = 0; d < N_SMART_DRIVES; d++)
	{
		D->smartData[bank].nValue[d] = 0;
		for(int v = 0; v < XLR_MAX_SMARTVALUES; v++)
		{
			D->smartData[bank].value[d][v] = 0LL;
		}
		
		memset(D->smartData[bank].drive + d, 0, sizeof(DriveInformation));
	}
}

const char *getSmartDescription(int smartId)
{
	int i;

	for(i = 0; smartDescriptions[i].id >= 0; i++)
	{
		if(smartDescriptions[i].id == smartId)
		{
			break;
		}
	}

	return smartDescriptions[i].desc;
}

int getMk5Smart(SSHANDLE xlrDevice, Mk5Daemon *D, int bank)
{
	XLR_RETURN_CODE xlrRC;
	S_DRIVEINFO driveInfo;
	USHORT smartVersion;
	Mk5Smart *smart;
	int d;

	switch(bank)
	{
	case BANK_A:
		strncpy(D->smartData[bank].vsn, D->vsnA, 8);
		D->smartData[bank].vsn[8] = 0;
		break;
	case BANK_B:
		strncpy(D->smartData[bank].vsn, D->vsnB, 8);
		D->smartData[bank].vsn[8] = 0;
		break;
	default:
		return -1;
	}

	xlrRC = XLRSelectBank(xlrDevice, bank);
	if(xlrRC != XLR_SUCCESS)
	{
		clearMk5Smart(D, bank);
		
		return -2;
	}

	smart = &(D->smartData[bank]);
	smart->mjd = 51234;		// FIXME

	for(d = 0; d < N_SMART_DRIVES; d++)
	{
		int v;
		DriveInformation *drive;

		drive = smart->drive + d;

		xlrRC = XLRGetDriveInfo(xlrDevice, d/2, d%2, &driveInfo);
		if(xlrRC != XLR_SUCCESS)
		{
			break;
		}

		trim(drive->model, driveInfo.Model);
		trim(drive->serial, driveInfo.Serial);
		trim(drive->rev, driveInfo.Revision);
		drive->capacity = driveInfo.Capacity * 512LL;
		drive->smartCapable = driveInfo.SMARTCapable;

		if(!drive->smartCapable)
		{
			continue;
		}

		xlrRC = XLRReadSmartValues(xlrDevice, &smartVersion, smart->smartXLR[d], d/2, d%2);
		if(xlrRC != XLR_SUCCESS)
		{
			break;
		}

		for(v = 0; v < XLR_MAX_SMARTVALUES; v++)
		{
			if(smart->smartXLR[d][v].ID <= 0)
			{
				break;
			}

			smart->id[d][v] = smart->smartXLR[d][v].ID;
			for(int i = 0; i < 6; i++)
			{
				smart->value[d][v] = (smart->value[d][v] << 8) + smart->smartXLR[d][v].raw[i];
			}
		}

		smart->nValue[d] = v;
	}

	if(d != N_SMART_DRIVES)
	{
		clearMk5Smart(D, bank);

		return -3;
	}

	return 0;
}

int logMk5Smart(const Mk5Daemon *D, int bank)
{
	char message[DIFX_MESSAGE_LENGTH];
	const char *vsn;
	const Mk5Smart *smart = D->smartData + bank;

	vsn = (bank == BANK_A ? D->vsnA : D->vsnB);

	if(D->smartData[bank].mjd < 50000.0 || vsn[0] == 0)
	{
		return 0;
	}

	snprintf(message, DIFX_MESSAGE_LENGTH, "SMART data for module %s\n", vsn);
	Logger_logData(D->log, message);
	for(int d = 0; d < N_SMART_DRIVES; d++)
	{
		const DriveInformation *drive = smart->drive + d;

		if(drive->capacity <= 0)
		{
			continue;
		}

		snprintf(message, DIFX_MESSAGE_LENGTH, "Drive %d : %s  %s  %s  %Ld bytes\n",
			d, drive->model, drive->serial, drive->rev, drive->capacity);
		Logger_logData(D->log, message);

		if(!drive->smartCapable)
		{
			continue;
		}
		for(int v = 0; v < smart->nValue[d]; v++)
		{
			int id = smart->smartXLR[d][v].ID;
			snprintf(message, DIFX_MESSAGE_LENGTH, "  SMART ID %3d : %s = %Ld\n",
				id, getSmartDescription(id), smart->value[d][v]);
			Logger_logData(D->log, message);
		}
	}
	Logger_logData(D->log, "\n");
}

int XLR_get_smart(Mk5Daemon *D)
{

	return 0;
}

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
