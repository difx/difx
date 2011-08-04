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
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/


#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include <difxmessage.h>
#include <ctype.h>
#include <sys/time.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include "config.h"
#include "mk5daemon.h"

const int MaxConnections = 8;
const unsigned short VSIS_PORT = 2650;

static int setnonblocking(int sock)
{
	int opts;

	opts = fcntl(sock,F_GETFL);
	if(opts < 0)
	{
		return -1;
	}

	opts = (opts | O_NONBLOCK);
	if(fcntl(sock,F_SETFL,opts) < 0)
	{
		return -1;
	}

	return 0;
}

static int handleVSIS(Mk5Daemon *D, int sock)
{
}

static void *serveVSIS(void *ptr)
{
	Mk5Daemon *D;
	int acceptSock;
	int clientSocks[MaxConnections];
	struct sockaddr_in server_address;
	struct timeval timeout;
	int n, v;
	int highSock;
	int readSocks;
	const int reuse_addr = 1;
	char message[DIFX_MESSAGE_LENGTH];
	fd_set socks;

	D = (Mk5Daemon *)ptr;

	for(int c = 0; c < MaxConnections; c++)
	{
		clientSocks[c] = 0;
	}

	acceptSock = socket(AF_INET, SOCK_STREAM, 0);

	if(acceptSock < 0)
	{
		Logger_logData(D->log, "Cannot create accept socket for VSI-S\n");

		pthread_exit(0);
	}

	setsockopt(acceptSock, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, sizeof(reuse_addr));

	if(setnonblocking(acceptSock) < 0)
	{
		Logger_logData(D->log, "Cannot non-block accept socket for VSI-S\n");

		pthread_exit(0);
	}

	memset((char *)(&server_address), 0, sizeof(server_address));
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);
	server_address.sin_port = htons(VSIS_PORT);
	if(bind(acceptSock, (struct sockaddr *)(&server_address), sizeof(server_address)) < 0 )
	{
		Logger_logData(D->log, "Cannot bind accept socket for VSI-S\n");

		pthread_exit(0);
	}

	listen(acceptSock, MaxConnections);

	while(!D->dieNow)
	{
		timeout.tv_sec = 1;
		timeout.tv_usec = 0;
		FD_ZERO(&socks);
		FD_SET(acceptSock, &socks);
		highSock = acceptSock;
		for(int c = 0; c < MaxConnections; c++)
		{
			if(clientSocks[c] > 0)
			{
				FD_SET(clientSocks[c], &socks);
				if(clientSocks[c] > highSock)
				{
					highSock = clientSocks[c];
				}
			}
		}

		readSocks = select(highSock+1, &socks, 0, 0, &timeout);

		if(readSocks < 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S select returned %d\n", readSocks);
			Logger_logData(D->log, message);

			pthread_exit(0);
		}

		if(readSocks == 0)
		{
			continue;
		}

		if(FD_ISSET(acceptSock, &socks))
		{
			int newSock = accept(acceptSock, 0, 0);
			if(newSock < 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S accept failure%d\n", newSock);
				Logger_logData(D->log, message);
			}
			else
			{
				/* find a slot for it */
				for(int c = 0; c < MaxConnections; c++)
				{
					if(clientSocks[c] == 0)
					{
						clientSocks[c] = newSock;
						newSock = -1;

						snprintf(message, DIFX_MESSAGE_LENGTH, "New VSI-S connection into slot %d of %d\n", c, MaxConnections);
						Logger_logData(D->log, message);

						break;
					}
				}
				if(newSock != -1)
				{
					Logger_logData(D->log, "No room for new VSI-S connection\n");

					close(newSock);
				}
			}
		}

		for(int c = 0; c < MaxConnections; c++)
		{
			if(FD_ISSET(clientSocks[c], &socks))
			{
				v = handleVSIS(D, clientSocks[c]);
				if(v < 0) /* connection closed? */
				{
					close(clientSocks[c]);
					clientSocks[c] = 0;
					snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S connection on slot %d closed\n", c);
					Logger_logData(D->log, message);
				}
			}
		}
	}

	if(acceptSock > 0)
	{
		close(acceptSock);
		acceptSock = 0;
	}
	for(int c = 0; c < MaxConnections; c++)
	{
		if(clientSocks[c] > 0)
		{
			close(clientSocks[c]);
			clientSocks[c] = 0;
		}
	}

	pthread_exit(0);
}

void Mk5Daemon_startVSIS(Mk5Daemon *D)
{
	pthread_create(&D->vsisThread, 0, &serveVSIS, D);
}

void Mk5Daemon_stopVSIS(Mk5Daemon *D)
{
	pthread_join(D->vsisThread, 0);
}

