/***************************************************************************
 *   Copyright (C) 2007-2012 by Walter Brisken                             *
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
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "../difxmessage.h"

int MulticastSend(const char *group, int port, const char *message, int length)
{
        struct sockaddr_in addr;
        int fd, l;
        unsigned char ttl=3;    /* time-to-live.  Max hops before discard */

        fd = socket(AF_INET, SOCK_DGRAM, 0);
        if(fd < 0)
        {
                return -1;
        }

        setsockopt(fd, IPPROTO_IP, IP_MULTICAST_TTL, &ttl, sizeof(ttl));

        memset(&addr, 0, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = inet_addr(group);
        addr.sin_port = htons(port);
        l = sendto(fd, message, length, 0, (struct sockaddr *)&addr, sizeof(addr));

        close(fd);

        return l;
}


/* below all for receiving */

int openMultiCastSocket(const char *group, int port)
{
	int sock, v;
	unsigned int yes=1;
	struct sockaddr_in addr;
	struct ip_mreq mreq;
	struct timeval tv;

	/* Make UDP socket */
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if(sock < 0) 
	{
		return -1;
	}
	
	/* Set 1 second timeout */
	tv.tv_sec = 1;
	tv.tv_usec = 0;
	setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));

	/* Allow reuse of port */
	v = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes));
	if(v < 0) 
	{
		return -2;
	}
	
	/* set up destination address */
	memset(&addr, 0, sizeof(struct sockaddr_in));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	addr.sin_port = htons(port);

	/* bind to receive address */
	v = bind(sock, (struct sockaddr *)&addr, sizeof(struct sockaddr_in));
	if(v < 0) 
	{
		return -3;
	}
	
	v = inet_aton(group, &mreq.imr_multiaddr);
	if(!v) 
	{
		return -4;
	}
	
	mreq.imr_interface.s_addr = htonl(INADDR_ANY);
	v = setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(struct ip_mreq));
	if(v < 0) 
	{
		printf("Error: Cannot configure multicast.  This is likely due to\n");
		printf("Your network configuration not having a route set for\n");
		printf("The DIFX multicast group: %s .  This can be done\n", group);
		printf("Perhaps at the command line you should try:\n");
		printf("  route add %s <dev>\n", group);
		printf("where <dev> is the network interface appropriate for this\n");
		printf("multicast traffic.\n");

		return -5;
	}
	
	return sock;
}

int closeMultiCastSocket(int sock)
{
	if(sock > 0) 
	{
		close(sock);
	}
	
	return 1;
}

int MultiCastReceive(int sock, char *message, int maxlen, char *from)
{
	struct sockaddr_in addr;
	int nbytes;
	socklen_t addrlen;

	addrlen = sizeof(struct sockaddr_in);

	nbytes = recvfrom(sock, message, maxlen, 0,
		(struct sockaddr *) &addr, &addrlen);

	if(nbytes > 0 && addrlen > 0 && from != 0)
	{
		strncpy(from, inet_ntoa(addr.sin_addr), 16);
	}

	return nbytes;
}

