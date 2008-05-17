#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <difxmessage.h>
#include <expat.h>
#include "mk5daemon.h"
#include "logger.h"

const char program[] = "mk5daemon";
const char version[] = "0.1";
const char verdate[] = "2008 May 17";

const int DefaultDifxMonitorPort = 50200;
const char DefaultDifxGroup[] = "224.2.2.1";


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
	D->dieNow = 0;
	Mk5Daemon_startMonitor(D);

	return D;
}

void deleteMk5Daemon(Mk5Daemon *D)
{
	if(D)
	{
		D->dieNow = 1;
		Mk5Daemon_stopMonitor(D);
		deleteLogger(D->log);
		/* FIXME -- kill running processes */
		free(D);
	}
}

int main(int argc, char **argv)
{
	Mk5Daemon *D;

	setuid(0);
	difxMessageInit(-1, program);

	D = newMk5Daemon();

	for(;;)	/* program event loop */
	{
		sleep(1);
	}

	deleteMk5Daemon(D);

	return 0;
}
