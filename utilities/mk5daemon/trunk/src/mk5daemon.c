#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <difxmessage.h>
#include "logger.h"

const char program[] = "mk5daemon";
const char version[] = "0.1";
const char verdate[] = "2008 May 17";

enum ProcessType
{
	PROCESS_NONE = 0,
	PROCESS_MARK5,
	PROCESS_SSERASE
};

typedef struct
{
	Logger *log;
	enum ProcessType process;
	pthread_t processThread;
} Mk5Daemon;

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
	difxMessageInit(-1, program);

	return D;
}

void deleteMk5Daemon(Mk5Daemon *D)
{
	if(D)
	{
		deleteLogger(D->log);
		/* FIXME -- kill running processes */
		free(D);
	}
}

static void *mark5run(void *ptr)
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

void Mk5Daemon_startMark5(Mk5Daemon *D)
{
	int v;

	if(D->process == PROCESS_NONE)
	{
		v = pthread_create(&D->processThread, 0, &mark5run, D);
		D->process = PROCESS_MARK5;
	}
}

void Mk5Daemon_stopMark5(Mk5Daemon *D)
{
	const char command[] = "killall -s INT Mark5A";

	if(D->process == PROCESS_MARK5)
	{
		system(command);
		pthread_join(D->processThread, 0);
		D->process = PROCESS_NONE;
	}
}

int main(int argc, char **argv)
{
	Mk5Daemon *D;

	setuid(0);

	D = newMk5Daemon();

	deleteMk5Daemon(D);

	return 0;
}
