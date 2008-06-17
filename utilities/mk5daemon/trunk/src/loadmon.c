#include <stdio.h>
#include <string.h>
#include <difxmessage.h>
#include "mk5daemon.h"

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd)
{
	static long long lastRX=0, lastTX=0;
	long long curRX=0, curTX=0;
	long long d;
	char message[1024];
	FILE *in;
	float l1, l5, l15;
	char line[100];
	int memused=0, memtot=0;
	char key[100];
	char logMessage[256];
	int val, v;

	/* LOAD */
	in = fopen("/proc/loadavg", "r");
	if(!in)
	{
		return -1;
	}

	fgets(line, 99, in);
	sscanf(line, "%f%f%f", &l1, &l5, &l15);

	fclose(in);

	/* MEMORY USAGE */
	in = fopen("/proc/meminfo", "r");
	if(!in)
	{
		return -1;
	}
	for(;;)
	{
		fgets(line, 99, in);
		if(feof(in))
		{
			break;
		}
		sscanf(line, "%s%d", key, &val);
		if(strcmp(key, "MemTotal:") == 0)
		{
			memtot = val;
			memused += val;
		}
		if(strcmp(key, "MemFree:") == 0 ||
		   strcmp(key, "Buffers:") == 0 ||
		   strcmp(key, "Cached:") == 0)
		{
			memused -= val;
		}
	}
	fclose(in);

	/* NETWORK */
	in = fopen("/proc/net/dev", "r");
	if(!in)
	{
		return -1;
	}
	for(;;)
	{
		fgets(line, 99, in);
		if(feof(in))
		{
			break;
		}
		if(strncmp(line, "  eth0:", 7) == 0)
		{
			curRX = curTX = 0;
			v = sscanf(line+7, "%lld%*d%*d%*d%*d%*d%*d%*d%lld", 
				&curRX, &curTX);
			if(lastRX > 0 && lastTX > 0) 
			{
				d = curRX - lastRX;
				if(d < 0)
				{
					d += 1LL<<32;
				}
				D->load.netRXRate = d/D->loadMonInterval;
				d = curTX - lastTX;
				if(d < 0)
				{
					d += 1LL<<32;
				}
				D->load.netTXRate = d/D->loadMonInterval;
			}
			lastRX = curRX;
			lastTX = curTX;
		}
	}
	fclose(in);
	
	D->load.cpuLoad = l1;
	D->load.totalMemory = memtot;
	D->load.usedMemory = memused;

	sprintf(logMessage, "LOAD: %13.7f %4.2f %d %d %5.3f %5.3f\n", mjd,
		D->load.cpuLoad, D->load.usedMemory, D->load.totalMemory,
		D->load.netRXRate*8.0e-6, D->load.netTXRate*8.0e-6);
	
	Logger_logData(D->log, logMessage);
	
	return difxMessageSendLoad(&D->load);
}
