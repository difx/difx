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
		int recBufSize = 768000;

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
