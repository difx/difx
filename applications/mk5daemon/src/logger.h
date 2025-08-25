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
 * $Id: logger.h 3962 2011-11-23 16:08:27Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/mk5daemon/src/logger.h $
 * $LastChangedRevision: 3962 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2011-11-24 00:08:27 +0800 (四, 2011-11-24) $
 *
 *==========================================================================*/

#ifndef __LOGGER_H__
#define __LOGGER_H__

#include <cstdio>
#include <pthread.h>

#define MK5DAEMON_LOGGER_HOSTNAME_LENGTH	32

typedef struct
{
	FILE *out;
	time_t lastTime;
	pthread_mutex_t lock;
	char logPath[256];
	char hostName[MK5DAEMON_LOGGER_HOSTNAME_LENGTH];
} Logger;

Logger *newLogger(const char *logPath);
void deleteLogger(Logger *log);
int Logger_logData(Logger *log, const char *message);

#endif
