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
#include "../difxmessage.h"
#include "difxmessageinternal.h"

int difxMessageReceiveOpen()
{
	if(difxMessagePort < 0)
	{
		return -1;
	}

	return openMultiCastSocket(difxMessageGroup, difxMessagePort);
}

int difxMessageReceiveClose(int sock)
{
	return closeMultiCastSocket(sock);
}

int difxMessageReceive(int sock, char *message, int maxlen, char *from)
{
	return MultiCastReceive(sock, message, maxlen, from);
}
