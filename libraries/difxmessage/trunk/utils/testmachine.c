#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "testmachine.h"

/* return 0 on success */
int pingtest(const char *hostname)
{
	int MaxCommandLength = 100;
	int MaxMessageLength = 100;
	char cmd[MaxCommandLength];
	char msg[MaxMessageLength];
	FILE *p;

	snprintf(cmd, MaxCommandLength, "ping -c 1 -w 1 -q %s", hostname);

	p = popen(cmd, "r");

	while(!feof(p))
	{
		fgets(msg, MaxMessageLength, p);
	}
	pclose(p);

	if(strncmp(msg, "rtt", 3) == 0)
	{
		return 0;	/* Test passed */
	}

	return 1;
}
