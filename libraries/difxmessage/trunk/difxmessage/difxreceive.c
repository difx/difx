/***************************************************************************
 *   Copyright (C) 2007-2014 by Walter Brisken                             *
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
#include <stdio.h>
#include <sys/socket.h>
#include "../difxmessage.h"
#include "difxmessageinternal.h"

int difxMessageReceiveOpen()
{
	int sock;

	if(difxMessagePort < 0)
	{
		return -1;
	}

	sock = openMultiCastSocket(difxMessageGroup, difxMessagePort);

	if(sock > 0)
	{
		const int recBufSize = 768000;

		/* Increase receive buffer size to accommodate bursty traffic */
		setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &recBufSize, sizeof(recBufSize));
	}

	return sock;
}

int difxMessageReceiveClose(int sock)
{
	return closeMultiCastSocket(sock);
}

int difxMessageReceive(int sock, char *message, int maxlen, char *from)
{
	return MultiCastReceive(sock, message, maxlen, from);
}
