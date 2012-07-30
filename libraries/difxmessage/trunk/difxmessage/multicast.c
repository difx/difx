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
#include <netdb.h>
#include "../difxmessage.h"

int MulticastSend(const char *ip, int port, const char *message, int length)
{
	static struct addrinfo *servinfo = 0;
	static int lastPort = 0;
        int fd, l;
        unsigned char ttl=3;    /* time-to-live.  Max hops before discard */

	if(port != lastPort)
	{
		if(servinfo != 0)
		{
			freeaddrinfo(servinfo);
			servinfo = 0;
		}

		if(port != 0)
		{
			struct addrinfo hints;
			char portstr[6];

			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			hints.ai_flags = AI_NUMERICHOST;
		
			snprintf(portstr, 6, "%d", port);

			l = getaddrinfo(ip, portstr, &hints, &servinfo);
			if(l != 0)
			{
				printf("Error: getaddrinfo()=%d -> %s\n", l, gai_strerror(l));

				return -1;
			}
		}
		lastPort = port;
	}

	if(port == 0)
	{
		return 0;
	}

        fd = socket(servinfo->ai_family, servinfo->ai_socktype, 0);
        if(fd < 0)
        {
		printf("Error: socket()\n");

                return -1;
        }

	if(servinfo->ai_family == PF_INET6)
	{
        	setsockopt(fd, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, &ttl, sizeof(ttl));
	}
	else
	{
        	setsockopt(fd, IPPROTO_IP, IP_MULTICAST_TTL, &ttl, sizeof(ttl));
	}
        l = sendto(fd, message, length, 0, servinfo->ai_addr, servinfo->ai_addrlen);

        close(fd);

        return l;
}

/* below all for receiving */

int openMultiCastSocket(const char *ip, int port)
{
	int sock, v;
	unsigned int yes=1;
	struct timeval tv;
	struct addrinfo hints;
	static struct addrinfo *servinfo = 0;
	char portstr[6];
	int l;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_DGRAM;
	hints.ai_protocol = IPPROTO_UDP;
	hints.ai_flags = AI_NUMERICHOST;

	snprintf(portstr, 6, "%d", port);

	l = getaddrinfo(ip, portstr, &hints, &servinfo);
	if(l != 0)
	{
		printf("Error: getaddrinfo()=%d -> %s\n", l, gai_strerror(l));

		return -1;
	}
	/* Make UDP socket */
        sock = socket(servinfo->ai_family, servinfo->ai_socktype, servinfo->ai_protocol);
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
		printf("Error: setsockopt() [1] -> %d\n", v);
		freeaddrinfo(servinfo);

		return -2;
	}
	
	/* bind to receive address */
	v = bind(sock, servinfo->ai_addr, servinfo->ai_addrlen);
	if(v) 
	{
		printf("Error: bind() -> %d\n", v);
		freeaddrinfo(servinfo);

		return -4;
	}
	
	if(servinfo->ai_family == AF_INET6)
	{
		struct ipv6_mreq mreq;

		memcpy(&mreq.ipv6mr_multiaddr, &((struct sockaddr_in6 *)servinfo->ai_addr)->sin6_addr, sizeof(struct in6_addr));
		mreq.ipv6mr_interface = 0;
		v = setsockopt(sock, IPPROTO_IPV6, IPV6_JOIN_GROUP, &mreq, sizeof(mreq));
	}
	else
	{
		struct ip_mreq mreq;
		
		mreq.imr_interface.s_addr = htonl(INADDR_ANY);
		inet_aton(ip, &mreq.imr_multiaddr);
		v = setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq));
	}

	freeaddrinfo(servinfo);
	
	if(v < 0) 
	{
		printf("Error: setsockopt() [2] -> %d\n", v);

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
	struct sockaddr_storage addr;
	int nbytes;
	socklen_t addrlen;

	addrlen = sizeof(addr);

	nbytes = recvfrom(sock, message, maxlen, 0, (struct sockaddr *) &addr, &addrlen);

	if(nbytes > 0 && addrlen > 0 && from != 0)
	{
		inet_ntop(addr.ss_family, addr.ss_family == AF_INET ? 
						&(((struct sockaddr_in *)(&addr))->sin_addr) : 
						&(((struct sockaddr_in6 *)(&addr))->sin6_addr), 
			from, DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH);
	}

	return nbytes;
}

