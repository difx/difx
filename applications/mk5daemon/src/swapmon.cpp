/***************************************************************************
 *   Copyright (C) 2016 by Walter Brisken                                  *
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
#include <difxmessage.h>
#include "mk5daemon.h"
#include "swap.h"

int Mk5Daemon_swapMon(Mk5Daemon *D, double mjd)
{
	static int lastUsage = 0;
	int v;
	int usage;
	int rv = 0;

	v = getSwapUsage(&usage);
	if(v != 0)
	{
		return v;
	}

	if(usage > 0)
	{
		char message[DIFX_MESSAGE_LENGTH];
		int severity;

		snprintf(message, DIFX_MESSAGE_LENGTH, "%d kB of operating system swap memory in use.  This could lead to horrible performance!", usage);
		
		Logger_logData(D->log, message);
	
		if(usage > 1000000)
		{
			severity = DIFX_ALERT_LEVEL_SEVERE;
		}
		else if(usage > 10000)
		{
			severity = DIFX_ALERT_LEVEL_WARNING;
		}
		else
		{
			severity = DIFX_ALERT_LEVEL_INFO;
		}

		rv = difxMessageSendDifxAlert(message, severity);
	}
	else if(lastUsage > 0)
	{
		char message[DIFX_MESSAGE_LENGTH];
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Operating system swap no longer in use.");

		Logger_logData(D->log, message);

		rv = difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	}

	lastUsage = usage;

	return rv;
}
