#ifndef __MK5DAEMON_H__
#define __MK5DAEMON_H__

#include <time.h>
#include <difxmessage.h>
#include "logger.h"

enum ProcessType
{
	PROCESS_NONE = 0,
	PROCESS_RESET,
	PROCESS_MARK5,
	PROCESS_MARK5_DONE,
	PROCESS_DATASTREAM,
	PROCESS_SSERASE
};

typedef struct
{
	Logger *log;
	DifxMessageLoad load;
	enum ProcessType process;
	pthread_t processThread;
	pthread_t monitorThread;
	pthread_t controlThread;
	pthread_mutex_t processLock;
	int loadMonInterval;		/* seconds */
	int dieNow;
	char vsnA[10], vsnB[10];
	char hostName[32];
	time_t lastMpifxcorrUpdate;
	int isMk5;
} Mk5Daemon;

extern const char headNode[];

int Mk5Daemon_loadMon(Mk5Daemon *D);
void Mk5Daemon_getModules(Mk5Daemon *D);
void Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
void Mk5Daemon_startMark5A(Mk5Daemon *D);
void Mk5Daemon_stopMark5A(Mk5Daemon *D);
void Mk5Daemon_resetMark5A(Mk5Daemon *D);
void Mk5Daemon_reboot(Mk5Daemon *D);
void Mk5Daemon_poweroff(Mk5Daemon *D);

#endif
