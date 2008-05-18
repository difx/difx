#include <stdio.h>
#include <string.h>
#include <difxmessage.h>
#include "mk5daemon.h"

int Mk5Daemon_loadMon(Mk5Daemon *D)
{
	char message[1024];
	FILE *in;
	float l1, l5, l15;
	char line[100];
	int memused=0, memtot=0;
	char key[100];
	int val;

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
	
	D->load.cpuLoad = l1;
	D->load.totalMemory = memtot;
	D->load.usedMemory = memused;
	
	return difxMessageSendLoad(&D->load);
}
