#ifndef __TRANSIENT_WRAPPER_H__
#define __TRANSIENT_WRAPPER_H__

#include <pthread.h>
#include <difxmessage.h> 
#include <difxio.h>

typedef struct
{
	DifxInput *D;
	enum DifxState difxState;
	pthread_t monitorThread;
	char *filePrefix;
	char *identifier;	/* must match that used difxmessage by mpifxcorr */
	int verbose;
	int monitorThreadDie;
	int rank;
	int doCopy;
} TransientWrapperData;

TransientWrapperData *newTransientWrapperData();

void deleteTransientWrapperData(TransientWrapperData *T);

void printTransientWrapperData(const TransientWrapperData *T);

void startMulticastMonitor(TransientWrapperData *T);

void stopMulticastMonitor(TransientWrapperData *T);

#endif
