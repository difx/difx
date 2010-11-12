#ifndef __TRANSIENT_WRAPPER_H__
#define __TRANSIENT_WRAPPER_H__

#include <pthread.h>
#include <difxmessage.h> 
#include <difxio.h>

#define MAX_EVENTS	100
#define EXTRA_EVENTS	20

typedef struct
{
	double startMJD, stopMJD;
	double priority;
} TransientEvent;

typedef struct
{
	DifxInput *D;
	enum DifxState difxState;
	pthread_t monitorThread;
	const char *outputPath;
	char *filePrefix;
	char *identifier;	/* must match that used difxmessage by mpifxcorr */
	int verbose;
	int monitorThreadDie;
	int rank;
	int doCopy;
	double maxCopyOverhead;
	int executeTime;
	int nTransient;		/* count of total number of transient messages received */
	int nEvent;		/* count of number of events in memory (<= nTransient) */
	TransientEvent event[MAX_EVENTS+EXTRA_EVENTS];

} TransientWrapperData;

TransientWrapperData *newTransientWrapperData();

void deleteTransientWrapperData(TransientWrapperData *T);

void printTransientWrapperData(const TransientWrapperData *T);

void startMulticastMonitor(TransientWrapperData *T);

void stopMulticastMonitor(TransientWrapperData *T);

void sortEvents(TransientWrapperData *T);

void addEvent(TransientWrapperData *T, const DifxMessageTransient *transient);

int copyBasebandData(const TransientWrapperData *T);

#endif
