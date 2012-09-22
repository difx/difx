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
 * $Id: testdifxmessagereceive.c 1167 2009-05-19 05:04:36Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision: 1167 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2009-05-18 23:04:36 -0600 (Mon, 18 May 2009) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include "difxmessage.h"


int main(int argc, char **argv)
{
	int sock;
	int l;
	char message[DIFX_MESSAGE_LENGTH], from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];
	time_t t;
	char timestr[32];
	DifxMessageGeneric G;

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	sock = difxMessageReceiveOpen();

	for(;;)
	{
		from[0] = 0;
		l = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
		if(l < 0)
		{
			struct timespec ts;
			
			ts.tv_sec = 0;
			ts.tv_nsec = 100000000;
			nanosleep(&ts, 0);

			continue;
		}
		message[l] = 0;
		time(&t);
		strcpy(timestr, ctime(&t));
		timestr[strlen(timestr)-1] = 0;

		if(strncmp(message, "exit", 4) == 0)
		{
			break;
		}

		difxMessageParse(&G, message);
		if(G.type != DIFX_MESSAGE_DRIVE_STATS && G.type != DIFX_MESSAGE_CONDITION)
		{
			continue;
		}

		printf("[%s %s] %s\n", timestr, from, message);
		difxMessageGenericPrint(&G);

		fflush(stdout);
	}

	difxMessageReceiveClose(sock);

	return 0;
}
