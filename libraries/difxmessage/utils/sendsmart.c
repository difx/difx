/***************************************************************************
 *   Copyright (C) 2010-2011 by Walter Brisken                             *
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
	int nSmart;
	char vsn[] = "VSN+1234";
	double mjdData = 56789.345;
	int slot = 3;
	int ids[DIFX_MESSAGE_MAX_SMART_IDS];
	long long values[DIFX_MESSAGE_MAX_SMART_IDS];
	int i;

	if(argc != 2)
	{
		printf("\nUsage: %s <nSmart>\n\n", argv[0]);

		return EXIT_SUCCESS;
	}

	nSmart = atoi(argv[1]);
	if(nSmart < 1 || nSmart > DIFX_MESSAGE_MAX_SMART_IDS)
	{
		fprintf(stderr, "nSmart must be in [1..%d] inclusive.\n", DIFX_MESSAGE_MAX_SMART_IDS);

		return EXIT_FAILURE;
	}

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	for(i = 0; i < nSmart; i++)
	{
		if(i >= 16)
		{
			ids[i] = 174+i;
		}
		else
		{
			ids[i] = 1+i;
		}

		values[i] = ids[i] + 2;
	}


	difxMessageSendDifxSmart(mjdData, vsn, slot, nSmart, ids, values);

	return EXIT_SUCCESS;
}
