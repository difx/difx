#ifndef __TRANSIENT_WRAPPER_H__
#define __TRANSIENT_WRAPPER_H__

#include <pthread.h>
#include <difxmessage.h> 
#include <difxio.h>

typedef struct
{
	DifxInput *D;
	int difxState;
	pthread_t monitorThread;
	char *filePrefix;
	int verbose;
	int monitorThreadDie;
} TransientWrapperData;

TransientWrapperData *newTransientWrapperData();

void deleteTransientWrapperData(TransientWrapperData *T);

void startMulticastMonitor(TransientWrapperData *T);

void stopMulticastMonitor(TransientWrapperData *T);

#endif
