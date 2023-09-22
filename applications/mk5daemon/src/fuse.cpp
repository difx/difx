/***************************************************************************
 *   Copyright (C) 2017 by Mark Wainright                                  *
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
 * $Id: getdir.cpp 6580 2015-03-26 14:04:13Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/getdir.cpp $
 * $LastChangedRevision: 6580 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2015-03-26 08:04:13 -0600 (Thu, 26 Mar 2015) $
 *
 *==========================================================================*/

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include "mk5daemon.h"

void Mk5Daemon_startfuseMk5(Mk5Daemon *D, const char *bank)
{
	char command[MAX_COMMAND_SIZE];

	if(!D->isMk5)
	{
		return;
	}

	// build command to call startfusemk5 with no vsis stop option (-n)
	if(bank[0] == 'A')
	{
		snprintf(command, MAX_COMMAND_SIZE, "su -l %s -c 'startfusemk5 -n A' &", D->userID);
	}
	else
	{
		snprintf(command, MAX_COMMAND_SIZE, "su -l %s -c 'startfusemk5 -n B' &", D->userID);
	}
        
	// stop vsis here
	Mk5Daemon_stopVSIS(D);

	// run startfusemk5
	system(command);
}

void Mk5Daemon_stopfuseMk5(Mk5Daemon *D)
{
	char command[MAX_COMMAND_SIZE];

	if(!D->isMk5)
	{
		return;
	}

	// build command to call stopfusemk5
	snprintf(command, MAX_COMMAND_SIZE, "su -l %s -c 'stopfusemk5 stop'", D->userID);

	// run stopfusemk5
	system(command);
}

