/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include "mk5daemon.h"

struct conditionParams
{
	Mk5Daemon *D;
	const char *options;
};

static void *conditionRun(void *ptr)
{
	struct conditionParams *params;
	char command[MAX_COMMAND_SIZE];
	char message[DIFX_MESSAGE_LENGTH];
	FILE *pin;
	char *rv;

	params = (struct conditionParams *)ptr;

	if(params->D->activeBank < 0)
	{
		return 0;
	}

	Logger_logData(params->D->log, "mk5erase starting\n");

	snprintf(command, MAX_COMMAND_SIZE, "mk5erase --force %s %s", params->options, params->D->vsns[params->D->activeBank]);

	pin = popen(command, "r");
	for(;;)
	{
		rv = fgets(message, DIFX_MESSAGE_LENGTH, pin);
		if(rv == 0)
		{
			break;
		}
		message[DIFX_MESSAGE_LENGTH-1] = 0;
		if(message[0] == '>')
		{
			Logger_logData(params->D->log, message);
		}
	}
	pclose(pin);

	Logger_logData(params->D->log, "mk5erase done\n");

	params->D->processDone = 1;

	free(params);

	pthread_exit(0);

	return 0;
}

void Mk5Daemon_startCondition(Mk5Daemon *D, const char *options)
{
	struct conditionParams *P;

	if(!D->isMk5)
	{
		return;
	}

	P = (struct conditionParams *)calloc(1, sizeof(struct conditionParams));

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		D->processDone = 0;
		D->process = PROCESS_CONDITION;

		P->D = D;
		P->options = options;
		pthread_create(&D->processThread, 0, &conditionRun, P);
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_stopCondition(Mk5Daemon *D)
{
	char command[MAX_COMMAND_SIZE];
	int v;
	
	snprintf(command, MAX_COMMAND_SIZE, "killall -s SIGINT mk5erase");
	v = system(command);

	if(v == -1)
	{
		char message[DIFX_MESSAGE_LENGTH];

		snprintf(message, DIFX_MESSAGE_LENGTH, "system(%s) returned %d\n", command, v);
		Logger_logData(D->log, message);
	}
}
