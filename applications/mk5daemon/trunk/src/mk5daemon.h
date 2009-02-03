#ifndef __MK5DAEMON_H__
#define __MK5DAEMON_H__

#include <time.h>
#include <difxmessage.h>
#include "logger.h"

enum ProcessType
{
	PROCESS_NONE = 0,
	PROCESS_RESET,
	PROCESS_SSOPEN,
	PROCESS_MARK5A,
	PROCESS_DATASTREAM,
	PROCESS_MK5DIR,
	PROCESS_MK5COPY,
	PROCESS_SSERASE
};

typedef struct
{
	Logger *log;
	DifxMessageLoad load;
	DifxMessageMk5Version mk5ver;
	enum ProcessType process;
	pthread_t processThread;
	pthread_t monitorThread;
	pthread_t controlThread;
	pthread_mutex_t processLock;
	int processDone;
	int loadMonInterval;		/* seconds */
	int dieNow;
	char vsnA[10], vsnB[10];
	char hostName[32];
	time_t lastMpifxcorrUpdate;
	time_t lastMark5AUpdate;
	int isMk5;
	int isHeadNode;
	long long lastRX, lastTX;
	int idleCount;
	int nXLROpen;
} Mk5Daemon;

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd);
int Mk5Daemon_getStreamstorVersions(Mk5Daemon *D);
int Mk5Daemon_sendStreamstorVersions(Mk5Daemon *D);
int logStreamstorVersions(Mk5Daemon *D);
void Mk5Daemon_getModules(Mk5Daemon *D);
void Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
void Mk5Daemon_startMark5A(Mk5Daemon *D);
void Mk5Daemon_stopMark5A(Mk5Daemon *D);
void Mk5Daemon_resetMark5A(Mk5Daemon *D);
int mark5command(const char *outstr, char *instr, int maxlen);
void Mk5Daemon_reboot(Mk5Daemon *D);
void Mk5Daemon_poweroff(Mk5Daemon *D);
void Mk5Daemon_startMk5Dir(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopMk5Dir(Mk5Daemon *D);
void Mk5Daemon_startMk5Copy(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopMk5Copy(Mk5Daemon *D);
void Mk5Daemon_diskOn(Mk5Daemon *D, const char *banks);
void Mk5Daemon_diskOff(Mk5Daemon *D, const char *banks);
void Mk5Daemon_startMpifxcorr(Mk5Daemon *D, const DifxMessageGeneric *G);

#endif
