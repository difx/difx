/***************************************************************************
 *   Copyright (C) 2010-2013 by Walter Brisken                             *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdlib.h>
#include <string.h>
#include "watchdog.h"

time_t watchdogTime;
int watchdogVerbose = 0;
char watchdogStatement[256];
pthread_mutex_t watchdogLock;
int watchdogTimeout = 20;
pthread_t watchdogThread;
char watchdogXLRError[XLR_ERROR_LENGTH+1];
int watchdogXLRErrorCode;
FILE *watchdogStream = stderr;

void setWatchdogVerbosity(int v)
{
	watchdogVerbose = v;
}

void setWatchdogTimeout(int t)
{
	watchdogTimeout = t;
}

void setWatchdogStream(FILE *stream)
{
	watchdogStream = stream;
}

void *watchdogFunction(void *data)
{
	int deltat;
	int lastdeltat = 0;

	for(;;)
	{
		struct timespec ts;

		ts.tv_sec = 0;
		ts.tv_nsec = 100000000;
		nanosleep(&ts, 0);
		pthread_mutex_lock(&watchdogLock);

		if(strcmp(watchdogStatement, "DIE") == 0)
		{
			pthread_mutex_unlock(&watchdogLock);
			return 0;
		}
		else if(watchdogTime != 0)
		{
			deltat = time(0) - watchdogTime;
			if(deltat > watchdogTimeout)
			{
				fprintf(watchdogStream, "Watchdog caught a hang-up executing: %s\n", watchdogStatement);
				exit(EXIT_FAILURE);
			}
			else if(deltat != lastdeltat)
			{
				if(deltat > (watchdogTimeout/2) && deltat%2 == 0)
				{
					fprintf(watchdogStream, "Waiting %d seconds executing: %s\n", deltat, watchdogStatement);
				}
				lastdeltat = deltat;
			}
		}
		else
		{
			lastdeltat = 0;
		}

		pthread_mutex_unlock(&watchdogLock);
	}
}

int initWatchdog()
{
	int perr;
	pthread_attr_t attr;

	pthread_mutex_init(&watchdogLock, NULL);
	watchdogStatement[0] = 0;
	watchdogXLRError[0] = 0;
	watchdogXLRErrorCode = 0;
	watchdogTime = 0;
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
	perr = pthread_create(&watchdogThread, &attr, watchdogFunction, 0);
	pthread_attr_destroy(&attr);

	if(perr != 0)
	{
		fprintf(watchdogStream, "Error: could not launch watchdog thread!\n");
		return -1;
	}

	return 0;
}

void stopWatchdog()
{
	pthread_mutex_lock(&watchdogLock);
	strcpy(watchdogStatement, "DIE");
	pthread_mutex_unlock(&watchdogLock);
	pthread_join(watchdogThread, NULL);
}
