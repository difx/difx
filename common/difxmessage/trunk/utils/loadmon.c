#include <stdio.h>
#include <unistd.h>
#include "difxmessage.h"


int doload(const char *host)
{
	char message[1024];
	FILE *in;
	float l1, l5, l15;
	char line[100];

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

	sprintf(message, "%s Load1min=%5.3f Load5min=%5.3f Load15min=%5.3f", 
		host, l1, l5, l15);

	return difxMessageSend(message);
}

int main(int argc, char **argv)
{
	int interval = 20;	/* in seconds */
	char host[128];

	difxMessageInit(argv[0]);
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
