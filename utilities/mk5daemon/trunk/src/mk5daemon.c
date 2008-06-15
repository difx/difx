#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <difxmessage.h>
#include <expat.h>
#include "mk5daemon.h"
#include "logger.h"

const char program[] = "mk5daemon";
const char version[] = "0.3";
const char verdate[] = "2008 June 15";

const int DefaultDifxMonitorPort = 50200;
const char DefaultDifxGroup[] = "224.2.2.1";
const char headNode[] = "swc000";

int *signalDie = 0;
typedef void (*sighandler_t)(int);
sighandler_t oldsigintHandler;

int usage(int argc, char **argv)
{
	printf("\n%s version %s\n\n", program, version);
	printf("Walter Brisken %s\n\n", verdate);
	printf("logged to /tmp/<year>_<doy>.mark5log\n\n");

	return 0;
}

Mk5Daemon *newMk5Daemon()
{
	Mk5Daemon *D;
	char message[80];

	D = (Mk5Daemon *)calloc(1, sizeof(Mk5Daemon));
	
	D->log = newLogger();
	D->process = PROCESS_NONE;
	D->loadMonInterval = 10;	/* seconds */
	gethostname(D->hostName, 32);
	D->isMk5 = strncasecmp(D->hostName, "mark5", 5) == 0 ? 1 : 0;
	signalDie = &D->dieNow;
	Mk5Daemon_startMonitor(D);
	pthread_mutex_init(&D->processLock, 0);
	sprintf(message, "mk5daemon starting");
	difxMessageSendDifxInfo(message);

	return D;
}

void deleteMk5Daemon(Mk5Daemon *D)
{
	char message[80];

	sprintf(message, "mk5daemon stopping");
	difxMessageSendDifxInfo(message);
	signalDie = 0;
	if(D)
	{
		D->dieNow = 1;
		Mk5Daemon_stopMonitor(D);
		deleteLogger(D->log);
		if(D->process == PROCESS_MARK5)
		{
			Mk5Daemon_stopMark5A(D);
		}
		free(D);
	}
}

int running(const char *name)
{
	FILE *in;
	int n;
	char cmd[256];
	char line[512];

	sprintf(cmd, "ps -e | grep %s", name);

	in = popen(cmd, "r");
	if(!in)
	{
		printf("ERROR Cannot run ps\n");
		return 1;
	}

	n = fread(line, 1, 512, in);
	line[511] = 0;
	fclose(in);

	if(n > 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}


void sigintHandler(int j)
{
	if(signalDie)
	{
		*signalDie = 1;
	}
	signal(SIGINT, oldsigintHandler);
}

int main(int argc, char **argv)
{
	Mk5Daemon *D;
	time_t t, lastTime, firstTime;
	char logMessage[128];
	int startmk5a = 1;
	int i;

	if(argc > 1) for(i = 1; i < argc; i++)
	{
		if(strcmp(argv[i], "-n") == 0)
		{
			startmk5a = 0;
		}
	}

	if(setuid(0) != 0)
	{
		fprintf(stderr, "Need suid root permission.  Bailing.\n");
		return 0;
	}

	setenv("STREAMSTOR_BIB_PATH", "/usr/share/streamstor/bib", 1);

	difxMessageInit(-1, program);

	D = newMk5Daemon();

	sprintf(logMessage, "Starting %s ver. %s  %s\n",
		program, version, verdate);
	Logger_logData(D->log, logMessage);

	oldsigintHandler = signal(SIGINT, sigintHandler);

	firstTime = lastTime = time(0);

	while(!D->dieNow)	/* program event loop */
	{
		t = time(0);
		if(t != lastTime)
		{
			lastTime = t;
			if(lastTime % D->loadMonInterval == 0)
			{
				Mk5Daemon_loadMon(D);
			}
			if(lastTime % 2 == 0 && D->isMk5)
			{
				pthread_mutex_lock(&D->processLock);
				if(running("SSErase"))
				{
					D->process = PROCESS_SSERASE;
				}
				else if(D->process == PROCESS_SSERASE)
				{
					D->process = PROCESS_NONE;
				}
				pthread_mutex_unlock(&D->processLock);
			}
		}

		if(t != 0 && t - D->lastMpifxcorrUpdate > 20 &&
			D->process == PROCESS_DATASTREAM)
		{
			pthread_mutex_lock(&D->processLock);
			if(!running("mpifxcorr"))
			{
				sprintf(logMessage, "Detected premature end of "
					"mpifxorr at %d", t);
				Logger_logData(D->log, logMessage);
				D->process = PROCESS_NONE;
			}
			else
			{
				/* note that it is still alive */
				D->lastMpifxcorrUpdate = t;
			}
			pthread_mutex_unlock(&D->processLock);
		}

		if(t - firstTime > 15 && D->isMk5 &&
			strncasecmp(D->hostName, "mark5", 5) == 0)
		{
			if(startmk5a)
			{
				Mk5Daemon_startMark5A(D);
				startmk5a = 0;
			}
			else
			{
				Mk5Daemon_getModules(D);
			}
		}

		usleep(200000);
	}

	sprintf(logMessage, "Stopping %s ver. %s  %s\n",
		program, version, verdate);
	Logger_logData(D->log, logMessage);

	deleteMk5Daemon(D);

	return 0;
}
