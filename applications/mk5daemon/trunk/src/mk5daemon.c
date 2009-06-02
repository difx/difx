#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <sys/stat.h>
#include <expat.h>
#include <difxmessage.h>
#include "mk5daemon.h"
#include "../config.h"
#include "logger.h"
#include "proc.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

const int DefaultDifxMonitorPort = 50200;
const char DefaultDifxGroup[] = "224.2.2.1";
const char DefaultLogPath[] = "/tmp";
const char headNode[] = "swc000";
const char difxUser[] = "difx";

const int maxIdle = 25;		/* if streamstor card is idle this long */
				/* set current process to NONE */

int *signalDie = 0;
typedef void (*sighandler_t)(int);
sighandler_t oldsigintHandler;

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n",
		program, version, author);
	fprintf(stderr, "A program to control Mark5A, handle Mark5 allocation "
		"manage VSNs, and\n");
	fprintf(stderr, "log all of the above.  Root permissions required.\n");
	fprintf(stderr, "\nUsage : %s [options]\n\n", pgm);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             Print this help message\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --no-mark5a\n");
	fprintf(stderr, "  -n             Don't automatically start Mark5A\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --headnode\n");
	fprintf(stderr, "  -H             Give head node capabilities\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --condition-watch\n");
	fprintf(stderr, "  -w             Start the condition-watch program\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q             Don't multicast any status\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --log-path <path>\n");
	fprintf(stderr, "  -l <path>      Put log files in <path>\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "Note: This program responds to the following "
			"environment variables:\n");
	fprintf(stderr, "  DIFX_LOG_DIR : change log path from default [%s]\n",
		DefaultLogPath);
	fprintf(stderr, "  DIFX_MESSAGE_GROUP : change multicast group "
		"from default [%s]\n", DefaultDifxGroup);
	fprintf(stderr, "  DIFX_MESSAGE_PORT : change multicast port "
		"from default [%d]\n", DefaultDifxMonitorPort);
	fprintf(stderr, "  STREAMSTOR_BIB_PATH : change streamstor firmware "
		"path from default\n");
	fprintf(stderr, "\n");

	return 0;
}

Mk5Daemon *newMk5Daemon(const char *logPath)
{
	Mk5Daemon *D;
	char message[80];

	D = (Mk5Daemon *)calloc(1, sizeof(Mk5Daemon));
	
	D->log = newLogger(logPath);
	D->process = PROCESS_NONE;
	D->loadMonInterval = 10;	/* seconds */
	gethostname(D->hostName, 32);
	D->isMk5 = strncasecmp(D->hostName, "mark5", 5) == 0 ? 1 : 0;
	printf("isMk5 = %d hostname = %s\n", D->isMk5, D->hostName);
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
		if(D->process == PROCESS_MARK5A)
		{
			Mk5Daemon_stopMark5A(D);
			while(!D->processDone)
			{
				usleep(100000);
			}
			pthread_mutex_lock(&D->processLock);
			pthread_join(D->processThread, 0);
			pthread_mutex_unlock(&D->processLock);
		}
		deleteLogger(D->log);
		free(D);
	}
}

/* FIXME -- move to a /proc query */
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

int checkStreamstor(Mk5Daemon *D, time_t t)
{
	int v, busy;
	char logMessage[128];

	if(!D->isMk5)
	{
		return 0;
	}

	v = procGetStreamstor(&busy);
	if(v < 0 && D->noDriver == 0)
	{
		D->noDriver = t;
		Logger_logData(D->log, 
			"windrvr6 driver went missing!\n");
		difxMessageSendDifxAlert(
			"windrvr6 disappeared!", 
			DIFX_ALERT_LEVEL_ERROR);
	}
	else
	{
		if(D->noDriver != 0)
		{
			D->noDriver = 0;
			Logger_logData(D->log, 
				"windrvr6 driver came back!\n");
			difxMessageSendDifxAlert(
				"windrvr6 reappeared!", 
				DIFX_ALERT_LEVEL_WARNING);
		}
	}

	if(D->noDriver)
	{
		return -1;
	}

	if(busy > 2)
	{
		D->idleCount = 0;
		pthread_mutex_lock(&D->processLock);
		if(D->process == PROCESS_NONE)
		{
			Logger_logData(D->log, 
				"Some process took control of windrvr6\n");
			difxMessageSendDifxAlert(
				"Some process took control of windrvr6", 
				DIFX_ALERT_LEVEL_INFO);
			D->process = PROCESS_UNKNOWN;
		}
		pthread_mutex_unlock(&D->processLock);
	}
	else
	{
		D->idleCount++;
	}

	if(D->idleCount > maxIdle && D->process != PROCESS_NONE & !D->processDone)
	{
		pthread_mutex_lock(&D->processLock);
		Logger_logData(D->log, 
			"Some process gave back control of windrvr6\n");
		difxMessageSendDifxAlert(
			"Some process gave back control of windrvr6", 
			DIFX_ALERT_LEVEL_INFO);
		/* FIXME -- watch for timeout here */
		D->process = PROCESS_NONE;
		pthread_mutex_unlock(&D->processLock);
	}

	if(t - D->lastMpifxcorrUpdate > 20 &&
		D->process == PROCESS_DATASTREAM)
	{
		pthread_mutex_lock(&D->processLock);
		if(!running("mpifxcorr"))
		{
			sprintf(logMessage, 
				"Detected premature end of mpifxcorr at %s\n",
				ctime(&t));
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

	return 0;
}

void sigintHandler(int j)
{
	if(signalDie)
	{
		*signalDie = 1;
	}
	signal(SIGINT, oldsigintHandler);
}

void startConditionWatch(const Mk5Daemon *D)
{
	const char *user;
	char command[1024];

	Logger_logData(D->log, "Starting condition_watch");

	if(fork())
	{
		return;
	}
	
	user = getenv("DIFX_USER_ID");
	if(!user)
	{
		user = difxUser;
	}

	sprintf(command, "ssh -f %s@%s condition_watch", user, D->hostName);

	system(command);

	exit(0);
}

int main(int argc, char **argv)
{
	Mk5Daemon *D;
	time_t t, lastTime, firstTime;
	char logMessage[128], str[16];
	int isHeadNode = 0;
	int i, ok=0;
	int justStarted = 1;
	int startMark5A = 1;
	int halfInterval;
	char logPath[256];
	const char *p;
	double mjd;
	int startCW = 0;

	p = getenv("DIFX_LOG_PATH");
	if(p)
	{
		sprintf(logPath, p);
	}
	else
	{
		strcpy(logPath, DefaultLogPath);
	}

	sprintf(str, "%d", DefaultDifxMonitorPort);
	setenv("DIFX_MESSAGE_PORT", str, 0);
	setenv("DIFX_MESSAGE_GROUP", DefaultDifxGroup, 0);
	setenv("STREAMSTOR_BIB_PATH", "/usr/share/streamstor/bib", 0);

	if(argc > 1) for(i = 1; i < argc; i++)
	{
		if(strcmp(argv[i], "-n") == 0 ||
		   strcmp(argv[i], "--no-mark5a") == 0)
		{
			startMark5A = 0;
		}
		else if(strcmp(argv[i], "-H") == 0 ||
		   strcmp(argv[i], "--headnode") == 0)
		{
			isHeadNode = 1;
		}
		else if(strcmp(argv[i], "-w") == 0 ||
		   strcmp(argv[i], "--condition-watch") == 0)
		{
			startCW = 1;
		}
		else if(strcmp(argv[i], "-h") == 0 ||
		   strcmp(argv[i], "--help") == 0)
		{
			return usage(argv[0]);
		}
		else if(strcmp(argv[i], "-q") == 0 ||
		   strcmp(argv[i], "--quiet") == 0)
		{
			setenv("DIFX_MESSAGE_PORT", "-1", 1);
		}
		else if(i < argc-1)
		{
			if(strcmp(argv[i], "-l") == 0 ||
			   strcmp(argv[i], "--log-path") == 0)
			{
				i++;
				strcpy(logPath, argv[i]);
			}
			else
			{
				return usage(argv[0]);
			}
		}
		else
		{
			return usage(argv[0]);
		}
	}

	if(setuid(0) != 0)
	{
		fprintf(stderr, "Need suid root permission.  Bailing.\n");
		return 0;
	}

	if(fork())
	{
		printf("*** %s ver. %s spawned ***\n", program, version);
		return 0;
	}

	/* the next line is so that later calls to fork() don't end up with defunct
	 * children.  see http://www.ecst.csuchico.edu/~beej/guide/ipc/fork.html
	 */
	signal(SIGCLD, SIG_IGN);

	umask(02);

	difxMessageInit(-1, program);
	difxMessageSendDifxAlert("mk5daemon starting", DIFX_ALERT_LEVEL_INFO);

	difxMessagePrint();

	D = newMk5Daemon(logPath);
	D->isHeadNode = isHeadNode;

	sprintf(logMessage, "Starting %s ver. %s\n", program, version);
	Logger_logData(D->log, logMessage);

	if(startCW && isHeadNode)
	{
		startConditionWatch(D);
	}

	oldsigintHandler = signal(SIGINT, sigintHandler);

	firstTime = lastTime = time(0);

	halfInterval = D->loadMonInterval/2;

	while(!D->dieNow)	/* program event loop */
	{
		t = time(0);
		mjd = 40587.0 + t/86400.0;

		if(t != lastTime)	/* we crossed a 1 second tick */
		{
			lastTime = t;

			ok = (checkStreamstor(D, t) == 0);

			if( (t % D->loadMonInterval) == 0)
			{
				Mk5Daemon_loadMon(D, mjd);
			}

			if(t % D->loadMonInterval == halfInterval)
			{
				Mk5Daemon_getModules(D);
			}

			if(t - firstTime > 15 && D->isMk5 &&
				strncasecmp(D->hostName, "mark5", 5) == 0)
			{
				if(justStarted)
				{
					Mk5Daemon_getStreamstorVersions(D);
					logStreamstorVersions(D);
				}
				if(startMark5A)
				{
					Mk5Daemon_startMark5A(D);
					startMark5A = 0;
					justStarted = 0;
				}
				else if(justStarted)
				{
					Mk5Daemon_getModules(D);
				}
				justStarted = 0;
			}
		}

		usleep(200000);
	}

	sprintf(logMessage, "Stopping %s ver. %s\n", program, version);
	Logger_logData(D->log, logMessage);

	deleteMk5Daemon(D);

	return 0;
}
