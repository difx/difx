/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
#include "difxmessage.h"

int main(int argc, char **argv)
{
	char message[1500];
	DifxMessageCondition cond;
	int i;
	int N = 1000000;

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	sprintf(cond.serialNumber, "X1Y2Z3Wxxx001");
	sprintf(cond.modelNumber, "Boomerang9000");
	cond.discSize = 250;
	sprintf(cond.moduleVSN, "WFB-0123");
	cond.moduleSlot = 4;
	cond.conditionMJD = 54321.9876;
	for(i = 0; i < DIFX_MESSAGE_N_CONDITION_BINS; i++)
	{
		cond.bin[i] = N;
		N /= 3;
	}

	difxMessageSendCondition(&cond);

	return 0;
}
