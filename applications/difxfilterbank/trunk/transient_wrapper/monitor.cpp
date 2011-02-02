#include <stdio.h>
#include <string.h>
#include "transient_wrapper_data.h"

static void handleDifxStatus(TransientWrapperData *T, const DifxMessageGeneric *G)
{
	const DifxMessageStatus *status;


	if(strncmp(G->identifier, T->identifier, DIFX_MESSAGE_IDENTIFIER_LENGTH) == 0)
	{
		status = &G->body.status;

		T->difxState = status->state;
		if(T->difxState == DIFX_STATE_ABORTING ||
		   T->difxState == DIFX_STATE_TERMINATED ||
		   T->difxState == DIFX_STATE_DONE)
		{
			T->monitorThreadDie = 1;
		}
	}
}

static void handleTransient(TransientWrapperData *T, const DifxMessageGeneric *G)
{
	char message[DIFX_MESSAGE_LENGTH];
	const DifxMessageTransient *transient;

	transient = &G->body.transient;

	if(strncmp(transient->jobId, T->identifier, DIFX_MESSAGE_IDENTIFIER_LENGTH) == 0)
	{
		if(T->verbose > 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, 
				"Adding transient [%12.6f,%12.6f] with prioritiy %f",
				transient->startMJD, transient->stopMJD,
				transient->priority);
			printf("%s\n", message);
		}
		addEvent(T, transient);
	}
	else
	{
		printf("transient for jobId <%s> ignored (looking for <%s>)\n", transient->jobId, T->identifier);
	}
}

static void *multicastMonitor(void *ptr)
{
	TransientWrapperData *T;
	int sock, n, v;
	char message[DIFX_MESSAGE_LENGTH], from[20];
	DifxMessageGeneric G;

	T = (TransientWrapperData *)ptr;

	sock = difxMessageReceiveOpen();
	if(sock < 0)
	{
		T->monitorThreadDie = -1;
		
		return 0;
	}

	while(!T->monitorThreadDie)
	{
		n = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
		if(n > 0)
		{
			message[n] = 0;
			v = difxMessageParse(&G, message);
			switch(G.type)
			{
			case DIFX_MESSAGE_STATUS:
				handleDifxStatus(T, &G);
				break;
			case DIFX_MESSAGE_TRANSIENT:
				handleTransient(T, &G);
				break;
			default:
				break;
			}
		}
	}

	difxMessageReceiveClose(sock);

	return 0;
}

void startMulticastMonitor(TransientWrapperData *T)
{
	pthread_create(&T->monitorThread, 0, &multicastMonitor, T);
}

void stopMulticastMonitor(TransientWrapperData *T)
{
	T->monitorThreadDie = 1;
	pthread_join(T->monitorThread, 0);
	sortEvents(T);
}

