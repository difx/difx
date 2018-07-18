/***************************************************************************
 *   Copyright (C) 2018 by Mark Wainright                                  *
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

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include "mk5daemon.h"

struct mk6dirParams
{
	Mk5Daemon *D;
	int slot;
};

static void *mk6dirRun(void *ptr)
{
	struct mk6dirParams *params;
	char command[MAX_COMMAND_SIZE];

	params = (struct mk6dirParams *)ptr;

	Logger_logData(params->D->log, "mk6dir starting\n");

	// build mk6dir command
        snprintf(command, MAX_COMMAND_SIZE, "su -l %s -c \"rungeneric.JMWDIFX-TRUNK 'mk6dir -c -m %d'\"", params->D->userID, params->slot);

	Mk5Daemon_system(params->D, command, 1);

	Logger_logData(params->D->log, "mk6dir done\n");

	free(params);

	pthread_exit(0);

	return 0;
}

void Mk5Daemon_getMk6FileList(Mk5Daemon *D, int slot)
{
	struct mk6dirParams *P;

	if(!D->isMk6)
	{
		return;
	}

	P = (struct mk6dirParams *)calloc(1, sizeof(struct mk6dirParams));

	pthread_attr_t attr;

	P->D = D;
	P->slot = slot;
	
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
	pthread_create(&D->processThread, &attr, &mk6dirRun, P);
	pthread_attr_destroy(&attr);
}

