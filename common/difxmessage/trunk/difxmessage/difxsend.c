#include <stdio.h>
#include "../difxmessage.h"
#include "difxmessageinternal.h"

int difxMessageSend(const char *message)
{
	if(difxMessagePort < 0)
	{
		printf("oops\n");
		return -1;
	}

	return MulticastSend(difxMessageGroup, difxMessagePort, message);
}
