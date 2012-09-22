/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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

#include <stdio.h>
#include <stdlib.h>
#include "difxmessage.h"

int main(int argc, char **argv)
{
	DifxMessageDriveStats stats;
	int i;
	int N = 1000000;

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	sprintf(stats.serialNumber, "X1Y2Z3Wxxx001");
	sprintf(stats.modelNumber, "Boomerang9000");
	stats.diskSize = 250;
	sprintf(stats.moduleVSN, "WFB-0123");
	stats.moduleSlot = 4;
	stats.startMJD = 54321.9876;
	stats.stopMJD = 54324.9876;
	for(i = 0; i < DIFX_MESSAGE_N_CONDITION_BINS; i++)
	{
		stats.bin[i] = N;
		N /= 3;
	}
	stats.type = DRIVE_STATS_TYPE_TEST;
	stats.startByte = 0;

	difxMessageSendDriveStats(&stats);

	return EXIT_SUCCESS;
}
