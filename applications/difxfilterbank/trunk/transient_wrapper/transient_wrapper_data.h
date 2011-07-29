#ifndef __TRANSIENT_WRAPPER_H__
#define __TRANSIENT_WRAPPER_H__

#include <pthread.h>
#include <difxmessage.h> 
#include <difxio.h>

#define MAX_EVENTS	100
#define EXTRA_EVENTS	20

typedef struct
{
	int enable;
	char vfastrHost[DIFX_MESSAGE_PARAM_LENGTH];

	double maxCopyOverhead;
	double minFreeDiskMB;
	char path[DIFX_MESSAGE_FILENAME_LENGTH];

	double recorr_tInt;
	int recorr_nChan;
	int recorr_specAvg;

	double recorr2_tInt;
	int recorr2_nChan;
	int recorr2_specAvg;
} TransientWrapperConf;

typedef struct
{
	double startMJD, stopMJD;
	double priority;
	double dm;
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
	int executeTime;
	int nMerged;
	int nTransient;		/* count of total number of transient messages received */
	int nEvent;		/* count of number of events in memory (<= nTransient) */
	TransientEvent event[MAX_EVENTS+EXTRA_EVENTS];
	const TransientWrapperConf *conf;

} TransientWrapperData;

TransientWrapperData *newTransientWrapperData(const TransientWrapperConf *conf);

void deleteTransientWrapperData(TransientWrapperData *T);

void printTransientWrapperData(const TransientWrapperData *T);

void startMulticastMonitor(TransientWrapperData *T);

void stopMulticastMonitor(TransientWrapperData *T);

void sortEvents(TransientWrapperData *T);

void addEvent(TransientWrapperData *T, const DifxMessageTransient *transient);

int copyBasebandData(const TransientWrapperData *T);

TransientWrapperConf *newTransientWrapperConf();

void deleteTransientWrapperConf(TransientWrapperConf *conf);

int loadTransientWrapperConf(TransientWrapperConf *conf, const char *filename);

void printTransientWrapperConf(const TransientWrapperConf *conf);

void logExecute(const char *str);

#endif
