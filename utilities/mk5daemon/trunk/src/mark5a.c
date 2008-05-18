#include <stdio.h>
#include <stdlib.h>
#include "mk5daemon.h"

static void *mark5Arun(void *ptr)
{
	const char command[] = "STREAMSTOR_BIB_PATH=/usr/share/streamstor/bib"
			       " /usr/bin/Mark5A -m 0 2>&1";
	Mk5Daemon *D;
	FILE *pin;
	char str[1000];

	D = (Mk5Daemon *)ptr;

	pin = popen(command, "r");

	for(;;)
	{
		fgets(str, 999, pin);
		str[999] = 0;
		/* FIXME -- look at str for "Ready" and "ERROR" */
		if(feof(pin))
		{
			break;
		}
		Logger_logData(D->log, str);
	}

	return 0;
}

void Mk5Daemon_startMark5A(Mk5Daemon *D)
{
	int v;

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		v = pthread_create(&D->processThread, 0, &mark5Arun, D);
		D->process = PROCESS_MARK5;
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_stopMark5A(Mk5Daemon *D)
{
	const char command[] = "killall -s INT Mark5A";

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_MARK5)
	{
		system(command);
		pthread_join(D->processThread, 0);
		D->process = PROCESS_NONE;
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_resetMark5A(Mk5Daemon *D)
{
	const char command1[] = "STREAMSTOR_BIB_PATH=/usr/share/streamstor/bib"
				" /usr/bin/SSReset";
	const char command2[] = "STREAMSTOR_BIB_PATH=/usr/share/streamstor/bib"
				" /usr/bin/ssopen";

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		D->process = PROCESS_RESET;
		pthread_mutex_unlock(&D->processLock);

		printf("R"); fflush(stdout);
		system(command1);
		printf("R"); fflush(stdout);
		system(command2);
		printf("R"); fflush(stdout);

		pthread_mutex_lock(&D->processLock);
		D->process = PROCESS_NONE;
	}

	pthread_mutex_unlock(&D->processLock);
}
