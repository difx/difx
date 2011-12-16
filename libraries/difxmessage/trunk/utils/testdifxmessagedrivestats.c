/***************************************************************************
 *   Copyright (C) 2011 by Walter Brisken                                  *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxmessage/trunk/utils/testdifxmessagecondition.c $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <difxmessage.h>

int main(int argc, char **argv)
{
	int i;
	DifxMessageDriveStats stats;

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	strcpy(stats.serialNumber, "123wfb");
	strcpy(stats.modelNumber, "1.0a2.0");
	stats.diskSize = 1212;
	strcpy(stats.moduleVSN, "WFB-1234");
	stats.startMJD = 50000.12;
	stats.stopMJD = 50000.22;
	stats.type = DRIVE_STATS_TYPE_READ;
	stats.startByte = 10000000000LL;

	for(stats.moduleSlot = 0; stats.moduleSlot < 8; stats.moduleSlot++)
	{
		for(i = 0; i < DIFX_MESSAGE_N_DRIVE_STATS_BINS; i++)
		{
			stats.bin[i] = i*i*i + stats.moduleSlot*1000;
		}
		
		difxMessageSendDriveStats(&stats);
	}

	return EXIT_SUCCESS;
}
