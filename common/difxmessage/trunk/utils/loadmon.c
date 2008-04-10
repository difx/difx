#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "difxmessage.h"


int doload(const char *host)
{
	char message[1024];
	FILE *in;
	float l1, l5, l15;
	char line[100];
	DifxMessageLoad load;
	int memused=0, memtot=0;
	char key[64];
	int val;

	/* LOAD */
	in = fopen("/proc/loadavg", "r");
	if(!in)
	{
		sprintf(message, "%s Error -- cannot open /proc/loadavg", host);
		difxMessageSend(message);
		return -1;
	}

	fgets(line, 99, in);
	sscanf(line, "%f%f%f", &l1, &l5, &l15);

	fclose(in);

	/* MEMORY USAGE */
	in = fopen("/proc/meminfo", "r");
	if(!in)
	{
		sprintf(message, "%s Error -- cannot open /proc/meminfo", host);
		difxMessageSend(message);
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
	
	load.cpuLoad = l1;
	load.totalMemory = memtot;
	load.usedMemory = memused;
	
	return difxMessageSendLoad(&load);
}

int main(int argc, char **argv)
{
	int interval = 20;	/* in seconds */
	char host[128];

	difxMessageInit(-1, "loadmon");
	difxMessagePrint();
	
	gethostname(host, 127);
	host[127] = 0;
	
	printf("Load Monitor running on %s\n", host);
	
	for(;;)
	{
		doload(host);
		sleep(interval);
	}

	return 0;
}
