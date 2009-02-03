#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <difxmessage.h>
#include "mk5daemon.h"

int messageForMe(const Mk5Daemon *D, const DifxMessageGeneric *G)
{
	int t;

	if(G->nTo < 1)
	{
		return 0;
	}

	for(t = 0; t < G->nTo; t++)
	{
		if(strcasecmp(D->hostName, G->to[t]) == 0)
		{
			return 1;
		}
		if(strcasecmp("all", G->to[t]) == 0)
		{
			return 1;
		}
		if(D->isMk5)
		{
			if(strcasecmp("mark5", G->to[t]) == 0)
			{
				return 1;
			}
		}
		else
		{
			if(strcasecmp("swc", G->to[t]) == 0)
			{
				return 1;
			}
		}
	}

	return 0;
}

static void handleMk5Status(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	/* only care if the message came from another process on same node */
	if(strcmp(D->hostName, G->from) != 0)
	{
		return;
	}

	/* only care if it is a mk5status from a datastream node */
	if(G->mpiId <= 0 || G->type != DIFX_MESSAGE_MARK5STATUS)
	{
		return;
	}

	strncpy(D->vsnA, G->body.mk5status.vsnA, 8);
	D->vsnA[8] = 0;
	strncpy(D->vsnB, G->body.mk5status.vsnB, 8);
	D->vsnB[8] = 0;

	if(G->body.mk5status.state == MARK5_STATE_OPENING ||
	   G->body.mk5status.state == MARK5_STATE_OPEN ||
	   G->body.mk5status.state == MARK5_STATE_PLAY ||
	   G->body.mk5status.state == MARK5_STATE_GETDIR ||
	   G->body.mk5status.state == MARK5_STATE_GOTDIR)
	{
		if(D->process == PROCESS_NONE)
		{
			Logger_logData(D->log, "mpifxcorr started\n");
		}
		D->process = PROCESS_DATASTREAM;

		/* update timestamp of last update */
		D->lastMpifxcorrUpdate = time(0);
	}

	if(G->body.mk5status.state == MARK5_STATE_CLOSE)
	{
		D->process = PROCESS_NONE;
		Logger_logData(D->log, "mpifxcorr finished\n");

		D->lastMpifxcorrUpdate = 0;
	}
}

static void handleCommand(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	const char *cmd;
	char logMessage[1024];
	if(!messageForMe(D, G))
	{
		return;
	}

	cmd = G->body.command.command;

	sprintf(logMessage, "Command: from=%s identifier=%s command=%s\n", 
		G->from, G->identifier, cmd);
	Logger_logData(D->log, logMessage);

	if(strcasecmp(cmd, "GetVSN") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_getModules(D);
		}
	}
	else if(strncasecmp(cmd, "ResetMark5", 10) == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_resetMark5A(D);
		}
	}
	else if(strcasecmp(cmd, "StartMark5A") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMark5A(D);
		}
	}
	else if(strcasecmp(cmd, "StopMark5A") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_stopMark5A(D);
		}
	}
	else if(strcasecmp(cmd, "Reboot") == 0)
	{
		Mk5Daemon_reboot(D);
	}
	else if(strcasecmp(cmd, "Poweroff") == 0)
	{
		Mk5Daemon_poweroff(D);
	}
	else if(strcasecmp(cmd, "Clear") == 0)
	{
		D->process = PROCESS_NONE;
		if(D->isMk5)
		{
			Mk5Daemon_getModules(D);
		}
	}
	else if(strcasecmp(cmd, "stopmk5daemon") == 0)
	{
		D->dieNow = 1;
	}
	else if(strcasecmp(cmd, "getdirA") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMk5Dir(D, "A");
		}
	}
	else if(strcasecmp(cmd, "getdirB") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMk5Dir(D, "B");
		}
	}
	else if(strcasecmp(cmd, "getdir") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMk5Dir(D, "AB");
		}
	}
	else if(strcasecmp(cmd, "stopdir") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_stopMk5Dir(D);
		}
	}
	else if(strcasecmp(cmd, "discon") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOn(D, "AB");
		}
	}
	else if(strcasecmp(cmd, "disconA") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOn(D, "A");
		}
	}
	else if(strcasecmp(cmd, "disconB") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOn(D, "B");
		}
	}
	else if(strcasecmp(cmd, "discoff") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOff(D, "AB");
		}
	}
	else if(strcasecmp(cmd, "discoffA") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOff(D, "A");
		}
	}
	else if(strcasecmp(cmd, "discoffB") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOff(D, "B");
		}
	}
	else if(strncasecmp(cmd, "copy ", 5) == 0)
	{
		/* protect against malicious use */
		if(D->isMk5 && !strstr(cmd, ";") 
		            && !strstr(cmd, "|") 
		            && !strstr(cmd, ">") 
		            && !strstr(cmd, "<") )
		{
			Mk5Daemon_startMk5Copy(D, cmd+5);
		}
	}
	else if(strcasecmp(cmd, "stopcopy") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_stopMk5Copy(D);
		}
	}
	else if(strcasecmp(cmd, "getver") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_sendStreamstorVersions(D);
		}
	}
	else if(strncasecmp(cmd, "Test", 4) == 0)
	{
		printf("[%s]\n", cmd);
	}
	else
	{
		sprintf(logMessage, "Command=%s not recognized!\n", cmd);
		Logger_logData(D->log, logMessage);
	}
}

static void *monitorMultiListen(void *ptr)
{
	Mk5Daemon *D;
	int sock, n, v;
	char message[2000], from[20];
	DifxMessageGeneric G;

	D = (Mk5Daemon *)ptr;

	sock = difxMessageReceiveOpen();
	
	while(!D->dieNow)
	{
		n = difxMessageReceive(sock, message, 1999, from);

		if(n > 0)
		{
			message[n] = 0;
			v = difxMessageParse(&G, message);
			switch(G.type)
			{
			case DIFX_MESSAGE_MARK5STATUS:
				handleMk5Status(D, &G);
				break;
			case DIFX_MESSAGE_COMMAND:
				handleCommand(D, &G);
				break;
			case DIFX_MESSAGE_START:
				Mk5Daemon_startMpifxcorr(D, &G);
				break;
			default:
				break;
			}
		}
		if(D->processDone)
		{
			pthread_mutex_lock(&D->processLock);

			pthread_join(D->processThread, 0);
			D->process = PROCESS_NONE;
			D->processDone = 0;
			
			pthread_mutex_unlock(&D->processLock);
			
			usleep(100000);
			Mk5Daemon_getModules(D);
		}	
	}

	difxMessageReceiveClose(sock);

	return 0;
}

void Mk5Daemon_startMonitor(Mk5Daemon *D)
{
	pthread_create(&D->monitorThread, 0, &monitorMultiListen, D);
}

void Mk5Daemon_stopMonitor(Mk5Daemon *D)
{
	pthread_join(D->monitorThread, 0);
}
