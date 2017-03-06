/***************************************************************************
 *   Copyright (C) 2008-2016 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __MK5DAEMON_H__
#define __MK5DAEMON_H__

#include <map>
#include <list>
#include <string>
#include <time.h>
#include <difxmessage.h>
#include "config.h"
#include "logger.h"
#include "options.h"
#include "../mk5dir/mark5directorystructs.h"
#ifdef HAS_MARK6META
#include <mark6meta/Mark6.h>
#endif
#ifdef HAVE_XLRAPI_H
#include "smart.h"
#endif

#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_PATTERN 0x44332211UL
#else
#define MARK5_FILL_PATTERN 0x11223344UL
#endif


extern const char difxUser[];
const int MaxConnections = 8;

#define MAX_COMMAND_SIZE		768
#define MAX_FILENAME_SIZE		256
#define N_BANK				2
#define N_DRIVE				8
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
	int dieNow;
	int activeBank;
	char vsns[N_BANK][10];
	char hostName[MK5DAEMON_HOSTNAME_LENGTH];
	time_t lastMpifxcorrUpdate;
	time_t lastMark5AUpdate;
	time_t noDriver;		/* time when driver disappeared */
	int isMk5;
	int isMk6;
	int isHeadNode;
	long long lastRX, lastTX;
	int idleCount;
	int nXLROpen;
	int skipGetModule;
	char streamstorLockIdentifer[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	char userID[MAX_USERID_LENGTH];
	unsigned int fillPattern;

	int difxSock;	/* for difxmessage receives */
	int acceptSock;	/* for VSIS server */
	int clientSocks[MaxConnections];
	
	std::map<MAC,bool> *macList;	/* map from a MAC address to enable flag */
	std::list<std::string> *ipAddresses;	/* possible IP addresses for self */
#ifdef HAVE_XLRAPI_H
	/* FIXME: make bank structure??? */
	Mk5Smart smartData[N_BANK];
	S_BANKSTATUS bank_stat[N_BANK];
	S_DIR dir_info[N_BANK];
	S_DRIVESTATS driveStatsConfig[XLR_MAXBINS];
	S_DRIVESTATS driveStats[N_BANK][N_DRIVE][XLR_MAXBINS];
	unsigned int driveStatsReplaced[N_BANK][N_DRIVE];
	bool openStreamstorError;
#endif
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
	char scanLabel[N_BANK][MODULE_LEGACY_SCAN_LENGTH];
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
	int psnMode;
	int psnOffset;
	int macFilterControl;
	enum NetProtocolType netProtocol;
	int diskStateMask;

	unsigned int totalPackets;
	unsigned int ethernetPackets;
	unsigned int addressRejects;
	unsigned int lengthRejects;
	unsigned int fscRejects;
#ifdef HAS_MARK6META
        Mark6 *mark6;
#endif
} Mk5Daemon;

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd);
int Mk5Daemon_swapMon(Mk5Daemon *D, double mjd);
int logStreamstorVersions(Mk5Daemon *D);
int Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
int Mk5Daemon_startVSIS(Mk5Daemon *D);
void Mk5Daemon_stopVSIS(Mk5Daemon *D);
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

#ifdef HAVE_XLRAPI_H
int lockStreamstor(Mk5Daemon *D, const char *identifier, int wait);
int unlockStreamstor(Mk5Daemon *D, const char *identifier);
int Mk5Daemon_getStreamstorVersions(Mk5Daemon *D);
int Mk5Daemon_sendStreamstorVersions(Mk5Daemon *D);
void Mk5Daemon_getModules(Mk5Daemon *D);
void Mk5Daemon_startMk5Dir(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopMk5Dir(Mk5Daemon *D);
void Mk5Daemon_startMk5Copy(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopMk5Copy(Mk5Daemon *D);
void Mk5Daemon_startCondition(Mk5Daemon *D, const char *options);
void Mk5Daemon_stopCondition(Mk5Daemon *D);
int Mk5Daemon_stopRecord(Mk5Daemon *D);
void Mk5Daemon_setBank(Mk5Daemon *D, int bank);
int Mk5Daemon_setProtect(Mk5Daemon *D, enum WriteProtectState state, char *msg);
int Mk5Daemon_error(Mk5Daemon *D, unsigned int *xlrError , char *msg);
void Mk5Daemon_diskOn(Mk5Daemon *D, const char *banks);
void Mk5Daemon_diskOff(Mk5Daemon *D, const char *banks);
void Mk5Daemon_sendSmartData(Mk5Daemon *D);
void Mk5Daemon_startfuseMk5(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopfuseMk5(Mk5Daemon *D);

void clearMk5Smart(Mk5Daemon *D, int bank);
void clearMk5DirInfo(Mk5Daemon *D, int bank);
void clearMk5Stats(Mk5Daemon *D, int bank);
void clearModuleInfo(Mk5Daemon *D, int bank);
int getDirectoryInfo(SSHANDLE xlrDevice, Mk5Daemon *D, int bank);
int setScan(Mk5Daemon *D, int bank, int scanNum);
int logMk5Smart(const Mk5Daemon *D, int bank);
int getMk5Smart(SSHANDLE xlrDevice, Mk5Daemon *D, int bank);
int extractSmartTemps(char *tempstr, const Mk5Daemon *D, int bank);
int handleVSIS(Mk5Daemon *D, int sock);

#endif

int handleVSIS(Mk5Daemon *D, int sock);
void handleMk5Status(Mk5Daemon *D, const DifxMessageGeneric *G);
void handleMark6Status(Mk5Daemon *D, const DifxMessageGeneric *G);
void handleCommand(Mk5Daemon *D, const DifxMessageGeneric *G);
void handleDriveStats(Mk5Daemon *D, const DifxMessageGeneric *G);
void controlVSIS(Mk5Daemon *D, const DifxMessageGeneric *G);

#endif
