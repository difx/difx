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

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <xlrapi.h>
#include <mark5ipc.h>
#include "mk5daemon.h"


int lockStreamstor(Mk5Daemon *D, const char *identifier, int wait)
{
	int v;
	char message[DIFX_MESSAGE_LENGTH];

	v = lockMark5(wait);
	if(v)
	{
		if(D->streamstorLockIdentifer[0])
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Identifier=%s cannot obtain Streamstor lock.  Identifier=%s maintains control.", identifier, D->streamstorLockIdentifer);
		}
		else
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Identifier=%s cannot obtain Streamstor lock.  PID=%d maintains control.", identifier, getMark5LockPID());
		}
		if(wait > 0)
		{
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
		}
		else
		{
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
		}
	}

	return v;
}

int unlockStreamstor(Mk5Daemon *D, const char *identifier)
{
	char message[DIFX_MESSAGE_LENGTH];
	int v = -1;

	if(strncmp(identifier, D->streamstorLockIdentifer, DIFX_MESSAGE_IDENTIFIER_LENGTH) != 0)
	{
		if(D->streamstorLockIdentifer[0] != 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH,
				"Identifier=%s trying to unlock Streamstor.  Identifier=%s maintains control.", identifier, D->streamstorLockIdentifer);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
		}
	}

	v = unlockMark5();

	D->streamstorLockIdentifer[0] = 0;
	

	return v;
}

void Mk5Daemon_resetStreamstor(Mk5Daemon *D)
{
	DifxMessageMk5Status dm;
	const char id[] = "ResetStreamtor";
	int v;

	/* Wait up to 2 seconds for a lock */
	v = lockStreamstor(D, id, 2);
	if(v)
	{
		return;
	}

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	dm.state = MARK5_STATE_RESETTING;
	difxMessageSendMark5Status(&dm);

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		D->process = PROCESS_RESET;
		pthread_mutex_unlock(&D->processLock);

		XLRCardReset(1);

		pthread_mutex_lock(&D->processLock);
		D->process = PROCESS_NONE;
	}

	pthread_mutex_unlock(&D->processLock);

	unlockStreamstor(D, id);

	Mk5Daemon_getModules(D);
}

void Mk5Daemon_reboot(Mk5Daemon *D)
{
	const char command[] = "/sbin/reboot";

	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	dm.state = MARK5_STATE_REBOOTING;
	difxMessageSendMark5Status(&dm);

	D->dieNow = 1;
	Mk5Daemon_system(D, command, 1);
}

void Mk5Daemon_poweroff(Mk5Daemon *D)
{
	const char command[] = "/sbin/poweroff";

	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsnA, 8);
	strncpy(dm.vsnB, D->vsnB, 8);
	dm.state = MARK5_STATE_POWEROFF;
	difxMessageSendMark5Status(&dm);

	D->dieNow = 1;
	Mk5Daemon_system(D, command, 1);
}

