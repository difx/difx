#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "transient_wrapper_data.h"

#define min(x,y) (((x) < (y)) ? (x) : (y))
#define max(x,y) (((x) > (y)) ? (x) : (y))

TransientWrapperData *newTransientWrapperData()
{
	TransientWrapperData *T;

	T = (TransientWrapperData *)calloc(1, sizeof(TransientWrapperData));

	T->difxState = DIFX_STATE_SPAWNING;
	T->maxCopyOverhead = 0.04;

	return T;
}

void deleteTransientWrapperData(TransientWrapperData *T)
{
	if(T)
	{
		if(T->D)
		{
			deleteDifxInput(T->D);
		}

		if(T->filePrefix)
		{
			free(T->filePrefix);
		}
	}
}

void printTransientWrapperData(const TransientWrapperData *T)
{
	int e;

	printf("TransientWrapperData [%p]\n", T);
	if(T)
	{
		printf("  identifier = %s\n", T->identifier);
		printf("  rank = %d\n", T->rank);
		printf("  DifxState = %s [%d]\n", DifxStateStrings[T->difxState], T->difxState);
		printf("  outputPath = %s\n", T->outputPath);
		printf("  filePrefix = %s\n", T->filePrefix);
		printf("  monitorThreadDie = %d\n", T->monitorThreadDie);
		printf("  verbose = %d\n", T->verbose);
		printf("  doCopy = %d\n", T->doCopy);
		printf("  executeTime = %d\n", T->executeTime);
		printf("  maxCopyOverhead = %f\n", T->maxCopyOverhead);
		printf("  nTransient = %d\n", T->nTransient);
		printf("  nMerged = %d\n", T->nMerged);
		printf("  nEvent = %d\n", T->nEvent);
		for(e = 0; e < T->nEvent; e++)
		{
			printf("    event[%d] = [%12.6f,%12.6f], %f\n", e, 
				T->event[e].startMJD, T->event[e].stopMJD,
				T->event[e].priority);
		}
	}
}

/* Note this sorts on priority only and puts the _highest_ priority events first */
static int eventCompare(const void *p1, const void *p2)
{
	const TransientEvent *e1, *e2;

	e1 = (TransientEvent *)p1;
	e2 = (TransientEvent *)p2;

	if(e1->priority > e2->priority)
	{
		return -1;
	}
	else if(e1->priority == e2->priority)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

void sortEvents(TransientWrapperData *T)
{
	if(T->nEvent > 1)
	{
		qsort(T->event, T->nEvent, sizeof(TransientEvent), eventCompare);

		/* trim down list to maximum allowed if needed */
		if(T->nEvent > MAX_EVENTS)
		{
			T->nEvent = MAX_EVENTS;
		}
	}
}

void addEvent(TransientWrapperData *T, const DifxMessageTransient *transient)
{
	char message[DIFX_MESSAGE_LENGTH];
	int merged = 0;

	T->nTransient++;

	if(transient->startMJD > T->D->job->jobStop || transient->stopMJD < T->D->job->jobStart)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Transient received out of job time range ([%12.6f,%12.6f] not in [%12.6f,%12.6f])",
			transient->startMJD, transient->stopMJD,
			T->D->job->jobStart, T->D->job->jobStop);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
	}
	else
	{
		if(T->nEvent > 0)
		{
			printf("Cool.  Merging two events!\n");
			if(min(T->event[T->nEvent-1].stopMJD,  transient->stopMJD) >= 
			   max(T->event[T->nEvent-1].startMJD, transient->startMJD))
			{
				T->event[T->nEvent-1].startMJD = min(T->event[T->nEvent-1].startMJD, transient->startMJD);
				T->event[T->nEvent-1].stopMJD  = max(T->event[T->nEvent-1].stopMJD,  transient->stopMJD);
				T->event[T->nEvent-1].priority = sqrt(T->event[T->nEvent-1].priority*T->event[T->nEvent-1].priority +
					transient->priority*transient->priority);
			}
			merged = 1;
		}

		if(merged == 0)
		{
			if(T->nEvent >= MAX_EVENTS + EXTRA_EVENTS)
			{
				sortEvents(T);
			}

			T->event[T->nEvent].startMJD = transient->startMJD;
			T->event[T->nEvent].stopMJD  = transient->stopMJD;
			T->event[T->nEvent].priority = transient->priority;
			T->nEvent++;
		}
	}

	T->nMerged += merged;
}

int copyBasebandData(const TransientWrapperData *T)
{
	const unsigned int MaxCommandLength = 1024;
	char command[MaxCommandLength];
	char message[DIFX_MESSAGE_LENGTH];
	time_t t1, t2;
	char outDir[DIFX_MESSAGE_FILENAME_LENGTH];
	int v, e;

	v = snprintf(outDir, DIFX_MESSAGE_FILENAME_LENGTH, 
		"%s/%s%s/%s/%d",
		T->outputPath, T->D->job->obsCode, T->D->job->obsSession, T->identifier, T->rank);
	
	if(v >= DIFX_MESSAGE_FILENAME_LENGTH)
	{
		fprintf(stderr, "Error: pathname is too long (%d vs. %d)\n",
			v, DIFX_MESSAGE_FILENAME_LENGTH);
		return 0;
	}

	snprintf(command, MaxCommandLength, 
		"mkdir -p %s", outDir);
	system(command);

	t1 = t2 = time(0);
	for(e = 0; e < T->nEvent; e++)
	{
		if(t2-t1 > T->executeTime*T->maxCopyOverhead)
		{
			break;
		}

		snprintf(command, MaxCommandLength, 
			"mk5cp Active %12.6f_%12.6f %s", 
			T->event[e].startMJD, T->event[e].stopMJD,
			outDir);

		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s",
			command);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		//system(command);

		t2 = time(0);
	}

	return e;
}
