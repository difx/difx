#ifndef __MK5DAEMON_H__
#define __MK5DAEMON_H__

#include "logger.h"

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
	pthread_t monitorThread;
	pthread_t controlThread;
	int loadMonInterval;		/* seconds */
	int dieNow;
} Mk5Daemon;

int Mk5Daemon_loadMon();
void Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
void Mk5Daemon_startControl(Mk5Daemon *D);
void Mk5Daemon_stopControl(Mk5Daemon *D);

#endif
