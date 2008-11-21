#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <xlrapi.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "mk5daemon.h"

const int MARK5A_PORT=2620;


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
#if 0
	if(time(0) - D->lastMark5AUpdate < 3)
	{
		Logger_logData(D->log, "Killing of Mark5A denied\n");
		return;
	}
#endif
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

int mark5command(const char *outstr, char *instr, int maxlen)
{
	struct sockaddr_in addr;
	int sock;
	struct timeval tv;
	int status;
	int n;

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if(sock < 0)
	{
		return -1;
	}
	
	tv.tv_sec = 9;	
	tv.tv_usec = 0;
	setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
	setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));

	addr.sin_family = AF_INET;
	addr.sin_port = htons(MARK5A_PORT);
	addr.sin_addr.s_addr = inet_addr("127.0.0.1");

	status = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
	if(status != 0)
	{
		close(sock);
		return -2;
	}

	n = strlen(outstr);

	if(send(sock, outstr, n, 0) < 1)
	{
		close(sock);
		return -3;
	}

	n = recv(sock, instr, maxlen, 0);
	close(sock);

	if(n < 1)
	{
		return -4;
	}
	instr[n] = 0;

	return n;
}

