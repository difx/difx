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
#include <cstring>
#include <difxmessage.h>
#include "mk5daemon.h"
#include "proc.h"

int Mk5Daemon_loadMon(Mk5Daemon *D, double mjd)
{
	long long curRX, curTX;
	float l1, l5, l15;
	int memused, memtot;
	char message[DIFX_MESSAGE_LENGTH];
	int v;

	/* LOAD */
	v = procGetCPU(&l1, &l5, &l15);
	if(v < 0)
	{
		return v;
	}

	/* MEMORY USAGE */
	v = procGetMem(&memused, &memtot);
	if(v < 0)
	{
		return v;
	}

	/* NETWORK */
	v = procGetNet(&curRX, &curTX);
	if(v < 0)
	{
		return v;
	}

	/* on 32 bit machines, proc stores only 32 bit values */
	if(D->lastRX > 0 || D->lastTX > 0) 
	{
		long long d;

		d = curRX - D->lastRX;
		D->load.netRXRate = d/D->loadMonInterval;

		d = curTX - D->lastTX;
		D->load.netTXRate = d/D->loadMonInterval;
	}
	else
	{
		D->load.netRXRate = 0;
		D->load.netTXRate = 0;
	}

	D->lastRX = curRX;
	D->lastTX = curTX;
	
	D->load.cpuLoad = l1;
	D->load.totalMemory = memtot;
	D->load.usedMemory = memused;

	snprintf(message, DIFX_MESSAGE_LENGTH,
		"LOAD: %13.7f %4.2f %d %d %5.3f %5.3f  %d  %d\n", mjd,
		D->load.cpuLoad, D->load.usedMemory, D->load.totalMemory,
		D->load.netRXRate*8.0e-6, D->load.netTXRate*8.0e-6, D->process, D->processDone);

	Logger_logData(D->log, message);
	
	return difxMessageSendLoad(&D->load);
}
