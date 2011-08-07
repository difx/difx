/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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

#include <time.h>
#include <difxmessage.h>
#include "config.h"
#include "logger.h"
#include "../mk5dir/mark5directorystructs.h"
#ifdef HAVE_XLRAPI_H
#include "smart.h"
#endif

#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_PATTERN 0x44332211UL
#else
#define MARK5_FILL_PATTERN 0x11223344UL
#endif


extern const char difxUser[];

#define MAX_COMMAND_SIZE	768
#define N_BANK			2
#define N_DRIVE			8

enum RecordState
{
	RECORD_OFF = 0,
	RECORD_ON,
	RECORD_HALTED,
	RECORD_THROTTLED,
	RECORD_OVERFLOW,
	RECORD_WAITING,

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

typedef struct
{
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
	int dieNow;
	int activeBank;
	char vsns[N_BANK][10];
	char hostName[32];
	time_t lastMpifxcorrUpdate;
	time_t lastMark5AUpdate;
	time_t noDriver;		/* time when driver disappeared */
	int isMk5;
	int isHeadNode;
	long long lastRX, lastTX;
	int idleCount;
	int nXLROpen;
	int skipGetModule;
	char streamstorLockIdentifer[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	char userID[256];
	unsigned int fillPattern;

	

#ifdef HAVE_XLRAPI_H
	/* FIXME: make bank structure??? */
	Mk5Smart smartData[N_BANK];
	S_BANKSTATUS bank_stat[N_BANK];
	S_DIR dir_info[N_BANK];
	S_DRIVESTATS driveStatsConfig[XLR_MAXBINS];
	S_DRIVESTATS driveStats[N_BANK][N_DRIVE][XLR_MAXBINS];
#endif
	FILE *recordPipe;
	enum RecordState recordState;
	int errorFlag[N_BANK];
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
	unsigned long bitstreamMask;
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
} Mk5Daemon;

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd);
int logStreamstorVersions(Mk5Daemon *D);
void Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
void Mk5Daemon_startVSIS(Mk5Daemon *D);
void Mk5Daemon_stopVSIS(Mk5Daemon *D);
void Mk5Daemon_resetStreamstor(Mk5Daemon *D);
int mark5command(const char *outstr, char *instr, int maxlen);
int Mk5Daemon_system(const Mk5Daemon *D, const char *command, int verbose);
void Mk5Daemon_reboot(Mk5Daemon *D);
void Mk5Daemon_poweroff(Mk5Daemon *D);
void Mk5Daemon_startMpifxcorr(Mk5Daemon *D, const DifxMessageGeneric *G);
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
void Mk5Daemon_startRecord(Mk5Daemon *D);
void Mk5Daemon_stopRecord(Mk5Daemon *D);
void Mk5Daemon_setBank(Mk5Daemon *D, int bank);
int Mk5Daemon_setProtect(Mk5Daemon *D, enum WriteProtectState state, char *msg);
int Mk5Daemon_error(Mk5Daemon *D, unsigned int *xlrError , char *msg);
void Mk5Daemon_diskOn(Mk5Daemon *D, const char *banks);
void Mk5Daemon_diskOff(Mk5Daemon *D, const char *banks);
void Mk5Daemon_sendSmartData(Mk5Daemon *D);

void clearMk5Smart(Mk5Daemon *D, int bank);
void clearMk5DirInfo(Mk5Daemon *D, int bank);
void clearMk5Stats(Mk5Daemon *D, int bank);
void clearModuleInfo(Mk5Daemon *D, int bank);
int getDirectoryInfo(SSHANDLE xlrDevice, Mk5Daemon *D, int bank);
int setScan(Mk5Daemon *D, int bank, int scanNum);
int logMk5Smart(const Mk5Daemon *D, int bank);
int getMk5Smart(SSHANDLE xlrDevice, Mk5Daemon *D, int bank);
int extractSmartTemps(char *tempstr, const Mk5Daemon *D, int bank);

#endif

#endif
