/***************************************************************************
 *   Copyright (C) 2008-2018 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#ifndef __MK5DAEMON_H__
#define __MK5DAEMON_H__

#include <map>
#include <list>
#include <string>
#include <time.h>
#include <difxmessage.h>
#include <limits.h>
#include "config.h"
#include "logger.h"
#include "options.h"
#ifdef HAS_MARK6META
#include <mark6meta/Mark6.h>
#endif


extern const char difxUser[];
const int MaxConnections = 8;

#define MAX_COMMAND_SIZE		768
#define MAX_FILENAME_SIZE		PATH_MAX
#define N_BANK				6 /* max number of banks(slots); 2 in Mark5, 4 in Mark6 */
#define N_BANK_MARK6			6
#define N_DRIVE				8 /* max number of drives per slot */
#define MAX_MACLIST_LENGTH		16
#define MAX_USERID_LENGTH		32
#define MK5DAEMON_HOSTNAME_LENGTH	32

enum RecordState
{
	RECORD_OFF = 0,
	RECORD_ON,
	RECORD_HALTED,		/* recording stopped; disk full? */
	RECORD_THROTTLED,	/* no data being written (rate = 0) */
	RECORD_OVERFLOW,
	RECORD_WAITING,
	RECORD_HUNG,		/* the recording process stopped sending status */

	NUM_RECORD_STATES	/* must terminate list */
};

extern const char recordStateStrings[][10];

enum ProcessType
{
	PROCESS_NONE = 0,
	PROCESS_RESET,
	PROCESS_SSOPEN,
	PROCESS_DATASTREAM,
	PROCESS_MK5DIR,
	PROCESS_MK5COPY,
	PROCESS_SSERASE,
	PROCESS_CONDITION,
	PROCESS_FUSEMK5,
	PROCESS_UNKNOWN
};

enum WriteProtectState
{
	PROTECT_OFF = 0,
	PROTECT_ON
};

enum NetProtocolType
{
	NET_PROTOCOL_UDP = 0,
	NET_PROTOCOL_L2,

	NUM_NET_PROTOCOLS	/* must be last entry in this enum */
};

extern const char netProtocolStrings[][10];

enum MacListCommand
{
	MAC_LIST_FLUSH = 0,
	MAC_LIST_ADD,
	MAC_LIST_DELETE,
	MAC_LIST_ENABLE,
	MAC_LIST_DISABLE,

	NUM_MAC_LIST_COMMANDS	/* must terminate list */
};

extern const char MacListCommandStrings[][10];


class MAC
{
public:
	unsigned long long int address;
public:
	int parse(const char *str);
	int toString(char *str) const;	/* returns number of characters written */
	friend bool operator ==(const MAC &mac1, const MAC &mac2);
	friend bool operator <(const MAC &mac1, const MAC &mac2);
};

typedef struct
{
	int isEmbedded;	/* true if commanded with the -e or --embedded options.  For use within a pipe */
	Logger *log;
	DifxMessageLoad load;
	DifxMessageMk5Version mk5ver;
	enum ProcessType process;
	pthread_t processThread;
	pthread_t monitorThread;
	pthread_t vsisThread;
	pthread_mutex_t processLock;
	int processDone;
	int loadMonInterval;		/* seconds */
	int swapMonInterval;		/* seconds */
	volatile int dieNow;
	int activeBank;
	char vsns[N_BANK_MARK6][10];
	char hostName[MK5DAEMON_HOSTNAME_LENGTH];
	time_t lastMpifxcorrUpdate;
	time_t lastMark5AUpdate;
	time_t noDriver;		/* time when driver disappeared */
	int isMk6;
	int isHeadNode;
	long long lastRX, lastTX;
	int idleCount;
	int skipGetModule;
	char userID[MAX_USERID_LENGTH];
	unsigned int fillPattern;

	int difxSock;	/* for difxmessage receives */
	int acceptSock;	/* for VSIS server */
	int clientSocks[MaxConnections];
	
	std::map<MAC,bool> *macList;	/* map from a MAC address to enable flag */
	std::list<std::string> *ipAddresses;	/* possible IP addresses for self */
	int systemReady;
	FILE *recordPipe;
	time_t stopRecordRequestTime;
	enum RecordState recordState;
	std::list<std::string> *errors;
	time_t recordT0;		/* time at which recording started */
	time_t recordLastMessage;	/* time when last message came from recording */
	int driveFail[N_BANK];
	long long bytesUsed[N_BANK];	/* same as record pointer */
	long long bytesTotal[N_BANK];
	long long startPointer[N_BANK];
	long long stopPointer[N_BANK];
	int diskModuleState[N_BANK];
	int nScan[N_BANK];
	int dirLength[N_BANK];
	char *dirData[N_BANK];
	int dirVersion[N_BANK];
	int driveStatsIndex[N_BANK];
	double recordRate;	/* Mbps */
	char dataSource[8];
	int bitstreamMask;
	int decimationRatio;
	int unprotected;	/* set after protect=off */

	int payloadOffset;
	int dataFrameOffset;
	int packetSize;

	unsigned int totalPackets;
	unsigned int ethernetPackets;
	unsigned int addressRejects;
	unsigned int lengthRejects;
	unsigned int fscRejects;
#ifdef HAS_MARK6META
        Mark6 *mark6;
#endif
	char mountedDisk[PATH_MAX+1];
} Mk5Daemon;

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd);
int Mk5Daemon_swapMon(Mk5Daemon *D, double mjd);
int logStreamstorVersions(Mk5Daemon *D);
int Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
void Mk5Daemon_resetStreamstor(Mk5Daemon *D);
int mark5command(const char *outstr, char *instr, int maxlen);
int Mk5Daemon_system(const Mk5Daemon *D, const char *command, int verbose);
FILE* Mk5Daemon_popen( const Mk5Daemon *D, const char *command, int verbose );
void Mk5Daemon_reboot(Mk5Daemon *D);
void Mk5Daemon_poweroff(Mk5Daemon *D);
void Mk5Daemon_killJob(Mk5Daemon *D, const char *jobName);
void Mk5Daemon_startMpifxcorr(Mk5Daemon *D, const DifxMessageGeneric *G, int noSu);
void Mk5Daemon_stopMpifxcorr_USNO( Mk5Daemon *D, const DifxMessageGeneric *G );
void Mk5Daemon_fileTransfer( Mk5Daemon *D, const DifxMessageGeneric *G );
void Mk5Daemon_fileOperation( Mk5Daemon *D, const DifxMessageGeneric *G );
void Mk5Daemon_vex2DifxRun( Mk5Daemon *D, const DifxMessageGeneric *G );
void Mk5Daemon_addVSIError(Mk5Daemon *D, const char *errorMessage);
void Mk5Daemon_delVSIError(Mk5Daemon *D, const char *errorMessage);
int Mk5Daemon_popVSIError(Mk5Daemon *D, char *errorMessage, int maxLength);
bool Mk5Daemon_addrMatches(const Mk5Daemon *D, const char *addrString);

#ifdef HAS_MARK6META
void Mk5Daemon_getMk6FileList(Mk5Daemon *D, int slot);
#endif

void handleMark6Status(Mk5Daemon *D, const DifxMessageGeneric *G);
void handleCommand(Mk5Daemon *D, const DifxMessageGeneric *G);

#endif
