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

int difxMessageSendBinary(const char *message)
{
	if(difxBinaryPort < 0)
	{
		return -1;
	}

	return MulticastSend(difxBinaryGroup, difxBinaryPort, message);
}

int difxMessageBinaryOpen()
{
	if(difxBinaryPort < 0)
	{
		return -1;
	}

	return openMultiCastSocket(difxBinaryGroup, difxBinaryPort);
}

int difxMessageBinaryClose(int sock)
{
	return closeMultiCastSocket(sock);
}

int difxMessageReceive(int sock, char *message, int maxlen, char *from)
{
	return MultiCastReceive(sock, message, maxlen, from);
}
