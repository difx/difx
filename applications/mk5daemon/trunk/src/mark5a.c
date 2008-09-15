#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <xlrapi.h>
#include "mk5daemon.h"

static void *mark5Arun(void *ptr)
{
	const char command[] = "/usr/bin/Mark5A -m 0 2>&1";
	Mk5Daemon *D;
	FILE *pin;
	char str[1000];
	int started = 0;

	D = (Mk5Daemon *)ptr;

	pin = popen(command, "r");

	D->lastMark5AUpdate = time(0);

	for(;;)
	{
		fgets(str, 999, pin);
		str[999] = 0;
		if(feof(pin))
		{
			break;
		}
		if(strstr("ERROR", str) != 0)
		{
			difxMessageSendDifxAlert(str, DIFX_ALERT_LEVEL_ERROR);
		}
		Logger_logData(D->log, str);
		D->lastMark5AUpdate = time(0);
		/* Once ready, get modules */
		if(!started &&
		   strncmp(str, "Mark5A Ready.", 13) == 0)
		{
			D->nXLROpen++;
			Mk5Daemon_getModules(D);
			started=1;
		}
	}
	Logger_logData(D->log, "Mark5A stopped\n");
	D->processDone = 1;

	pthread_exit(0);

	return 0;
}

void Mk5Daemon_startMark5A(Mk5Daemon *D)
{
	int v;

	if(strncmp(D->hostName, "mark5", 5) != 0)
	{
		return;
	}

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		v = pthread_create(&D->processThread, 0, &mark5Arun, D);
		D->process = PROCESS_MARK5A;
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_stopMark5A(Mk5Daemon *D)
{
	const char command[] = "killall -s INT Mark5A";

	/* if Mark5A got usage in last 3 seconds, don't allow */
	if(time(0) - D->lastMark5AUpdate < 3)
	{
		Logger_logData(D->log, "Killing of Mark5A denied\n");
		return;
	}
	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_MARK5A)
	{
		system(command);
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_resetMark5A(Mk5Daemon *D)
{
	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	dm.state = MARK5_STATE_RESETTING;
	difxMessageSendMark5Status(&dm);

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		D->process = PROCESS_RESET;
		pthread_mutex_unlock(&D->processLock);

		XLRCardReset(1);

		pthread_mutex_lock(&D->processLock);
		D->process = PROCESS_NONE;
	}

	pthread_mutex_unlock(&D->processLock);

	Mk5Daemon_getModules(D);
}

void Mk5Daemon_reboot(Mk5Daemon *D)
{
	const char command[] = "/sbin/reboot";

	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	dm.state = MARK5_STATE_REBOOTING;
	difxMessageSendMark5Status(&dm);

	D->dieNow = 1;
	system(command);
}

void Mk5Daemon_poweroff(Mk5Daemon *D)
{
	const char command[] = "/sbin/poweroff";

	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	dm.state = MARK5_STATE_POWEROFF;
	difxMessageSendMark5Status(&dm);

	D->dieNow = 1;
	system(command);
}
