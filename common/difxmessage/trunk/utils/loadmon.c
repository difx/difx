#include <stdio.h>
#include <unistd.h>
#include "difxmessage.h"


int doload(const char *host)
{
	char message[1024];
	FILE *in;
	float l1, l5, l15;
	char line[100];
	DifxMessageLoad load;

	in = fopen("/proc/loadavg", "r");
	if(!in)
	{
		sprintf(message, "%s Error -- cannot open /proc/loadavg", 
			host);
		difxMessageSend(message);
		return -1;
	}

	fgets(line, 99, in);
	sscanf(line, "%f%f%f", &l1, &l5, &l15);

	fclose(in);
	
	load.cpuLoad = l1;
	load.totalMemory = 1;
	load.usedMemory = 0;
	
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
