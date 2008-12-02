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

int difxMessageSendBinary(const char *message, int destination, int length)
{
	if(destination == BINARY_STA)
	{
		if(difxBinarySTAPort < 0)
		{
			return -1;
		}

		return MulticastSend(difxBinarySTAGroup, difxBinarySTAPort, message, length);
	}
	else
	{
		if (destination != BINARY_LTA)
		{
			return -1;
		}
                if(difxBinaryLTAPort < 0)
                {
                        return -1;
                }

                return MulticastSend(difxBinaryLTAGroup, difxBinaryLTAPort, message, length);
        }

}

int difxMessageBinaryOpen(int destination)
{
	if(destination != BINARY_STA)
	{
		if(difxBinarySTAPort < 0)
		{
			return -1;
		}

		return openMultiCastSocket(difxBinarySTAGroup, difxBinarySTAPort);
	}
	else
	{
		if (destination != BINARY_LTA)
                {
                        return -1;
                }

		if(difxBinaryLTAPort < 0)
                {
                        return -1;
                }

                return openMultiCastSocket(difxBinaryLTAGroup, difxBinaryLTAPort);
        }

}

int difxMessageBinaryClose(int sock)
{
	return closeMultiCastSocket(sock);
}

int difxMessageBinaryRecv(int sock, char *message, int maxlen, char *from)
{
        return MultiCastReceive(sock, message, maxlen, from);
}

