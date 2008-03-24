#include <stdio.h>
#include "../difxmessage.h"
#include "difxmessageinternal.h"

int difxMessageSend(const char *message)
{
	if(difxMessagePort < 0)
	{
		return -1;
	}

	return MulticastSend(difxMessageGroup, difxMessagePort, message);
}

int difxMessageSendProcessState(const char *state)
{
	char message[200];

	if(difxMessagePort < 0)
	{
		return -1;
	}

	sprintf(message, "%s %s", difxMessageIdentifier, state);
	return difxMessageSend(message);
}
