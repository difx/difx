#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <difxmessage.h>
#include <expat.h>
#include "mk5daemon.h"
#include "logger.h"

const char program[] = "mk5daemon";
const char version[] = "0.1";
const char verdate[] = "2008 May 20";

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
	printf("A program to start Mark5A and log its output.\n\n");
	printf("Usage : %s <time> [<command1> [<command2> [ ... ] ]\n\n", 
		argv[0]);
	printf("<time> is a time delay in seconds before starting the "
		"program.\n");
	printf("command(s) are Mark5A commands to be executed 1 minute "
		"after start.\n");
	printf("    (Note -- commands containing spaces need to be "
		"in quotes)\n\n");
	printf("Mark5A is started with the -m 0 options.  Debug data are\n");
	printf("logged to /tmp/<year>_<doy>.mark5log\n\n");

	return 0;
}

Mk5Daemon *newMk5Daemon()
{
	Mk5Daemon *D;

	D = (Mk5Daemon *)calloc(1, sizeof(Mk5Daemon));
	
	D->log = newLogger();
	D->process = PROCESS_NONE;
	D->loadMonInterval = 20;	/* seconds */
	gethostname(D->hostName, 32);
	signalDie = &D->dieNow;
	Mk5Daemon_startMonitor(D);
	pthread_mutex_init(&D->processLock, 0);

	return D;
}

void deleteMk5Daemon(Mk5Daemon *D)
{
	signalDie = 0;
	if(D)
	{
		D->dieNow = 1;
		Mk5Daemon_stopMonitor(D);
		deleteLogger(D->log);
		/* FIXME -- kill running processes */
		free(D);
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
	time_t t, lastTime;
	char logMessage[128];

	//setuid(0);
	difxMessageInit(-1, program);

	D = newMk5Daemon();

	sprintf(logMessage, "Starting %s ver. %s  %s\n",
		program, version, verdate);
	Logger_logData(D->log, logMessage);

	oldsigintHandler = signal(SIGINT, sigintHandler);

	lastTime = time(0);

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
		}

		usleep(200000);
	}

	sprintf(logMessage, "Stopping %s ver. %s  %s\n",
		program, version, verdate);
	Logger_logData(D->log, logMessage);

	deleteMk5Daemon(D);

	return 0;
}
