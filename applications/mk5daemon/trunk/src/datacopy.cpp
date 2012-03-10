/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken                             *
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

#define OPTIONS_LENGTH	512

struct mk5cpParams
{
	Mk5Daemon *D;
	char options[OPTIONS_LENGTH];
};

static void *mk5cpRun(void *ptr)
{
	struct mk5cpParams *params;
	char command[MAX_COMMAND_SIZE];

	params = (struct mk5cpParams *)ptr;

	Logger_logData(params->D->log, "mk5cp starting\n");

	snprintf(command, MAX_COMMAND_SIZE, "su -l %s -c 'mk5cp %s'", params->D->userID, params->options);
	Mk5Daemon_system(params->D, command, 1);

	Logger_logData(params->D->log, "mk5cp done\n");

	params->D->processDone = 1;

	free(params);

	pthread_exit(0);

	return 0;
}

static void makedir(Mk5Daemon *D, const char *options)
{
	char dir[MAX_FILENAME_SIZE];
	int a=-1;
	int i, l;
	char command[MAX_COMMAND_SIZE];

	/* look for a / character and assume that is the output directory */
	for(i = 0; options[i]; ++i)
	{
		if(a == -1)
		{
			if(options[i] == '/')
			a = i;
		}
		else if(options[i] <= ' ')
		{
			break;
		}
	}

	l = i-a;
	strncpy(dir, options+a, l);
	dir[l] = 0;

	snprintf(command, MAX_COMMAND_SIZE, "mkdir -m 777 -p %s", dir);

	Mk5Daemon_system(D, command, 1);
}

void Mk5Daemon_startMk5Copy(Mk5Daemon *D, const char *options)
{
	struct mk5cpParams *P;

	if(!D->isMk5)
	{
		return;
	}

	/* Make sure output directory exists and has permissions */
	makedir(D, options);

	P = (struct mk5cpParams *)calloc(1, sizeof(struct mk5cpParams));

	pthread_mutex_lock(&D->processLock);

	if(D->process == PROCESS_NONE)
	{
		D->processDone = 0;
		D->process = PROCESS_MK5COPY;

		P->D = D;
		snprintf(P->options, OPTIONS_LENGTH, "%s", options);
		pthread_create(&D->processThread, 0, &mk5cpRun, P);
	}

	pthread_mutex_unlock(&D->processLock);
}

void Mk5Daemon_stopMk5Copy(Mk5Daemon *D)
{
	Mk5Daemon_system(D, "killall -INT mk5cp", 1);
}
