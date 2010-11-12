/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken                             *
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
#include "logger.h"
#include "transient.h"

extern const char difxUser[];

#define MAX_COMMAND_SIZE	768

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

typedef struct
{
	Logger *log;
	DifxMessageLoad load;
	DifxMessageMk5Version mk5ver;
	enum ProcessType process;
	pthread_t processThread;
	pthread_t monitorThread;
	pthread_mutex_t processLock;
	int processDone;
	int loadMonInterval;		/* seconds */
	int dieNow;
	char vsnA[10], vsnB[10];
	char hostName[32];
	time_t lastMpifxcorrUpdate;
	time_t lastMark5AUpdate;
	time_t noDriver;		/* time when driver disappeared */
	int isMk5;
	int isHeadNode;
	long long lastRX, lastTX;
	int idleCount;
	int nXLROpen;
	EventManager eventManager;	/* for tracking transient events */
	int skipGetModule;
	char streamstorLockIdentifer[DIFX_MESSAGE_IDENTIFIER_LENGTH];
} Mk5Daemon;

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd);
int Mk5Daemon_getStreamstorVersions(Mk5Daemon *D);
int Mk5Daemon_sendStreamstorVersions(Mk5Daemon *D);
int logStreamstorVersions(Mk5Daemon *D);
void Mk5Daemon_getModules(Mk5Daemon *D);
void Mk5Daemon_startMonitor(Mk5Daemon *D);
void Mk5Daemon_stopMonitor(Mk5Daemon *D);
int lockStreamstor(Mk5Daemon *D, const char *identifier, int wait);
int unlockStreamstor(Mk5Daemon *D, const char *identifier);
void Mk5Daemon_resetStreamstor(Mk5Daemon *D);
int mark5command(const char *outstr, char *instr, int maxlen);
int Mk5Daemon_system(const Mk5Daemon *D, const char *command, int verbose);
void Mk5Daemon_reboot(Mk5Daemon *D);
void Mk5Daemon_poweroff(Mk5Daemon *D);
void Mk5Daemon_startMk5Dir(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopMk5Dir(Mk5Daemon *D);
void Mk5Daemon_startMk5Copy(Mk5Daemon *D, const char *bank);
void Mk5Daemon_stopMk5Copy(Mk5Daemon *D);
void Mk5Daemon_startCondition(Mk5Daemon *D, const char *options);
void Mk5Daemon_stopCondition(Mk5Daemon *D);
void Mk5Daemon_diskOn(Mk5Daemon *D, const char *banks);
void Mk5Daemon_diskOff(Mk5Daemon *D, const char *banks);
void Mk5Daemon_startMpifxcorr(Mk5Daemon *D, const DifxMessageGeneric *G);

#endif
