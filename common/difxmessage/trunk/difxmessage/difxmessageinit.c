#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "../difxmessage.h"
#include "difxmessageinternal.h"

char difxMessageGroup[16] = "";
int difxMessagePort = -1; 
char difxMessageIdentifier[128] = "";
char difxMessageHostname[32] = "";

const char difxMessageDefaultGroup[] = "225.0.0.1";
const int difxMessageDefaultPort = 10000;

int difxMessageInit(const char *identifier)
{
	const char *envstr;
	strncpy(difxMessageIdentifier, identifier, MAX_DIFX_MESSAGE_IDENTIFER);
	difxMessageIdentifier[MAX_DIFX_MESSAGE_IDENTIFER-1] = 0;

	strcpy(difxMessageGroup, difxMessageDefaultGroup);
	difxMessagePort = difxMessageDefaultPort;

	gethostname(difxMessageHostname, 32);
	difxMessageHostname[31] = 0;

	envstr = getenv("DIFX_MESSAGE_GROUP");
	if(envstr != 0)
	{
		strncpy(difxMessageGroup, envstr, 16);
		difxMessageGroup[15] = 0;
	}

	envstr = getenv("DIFX_MESSAGE_PORT");
	if(envstr != 0)
	{
		difxMessagePort = atoi(envstr);
	}

	return 0;
}

void difxMessagePrint()
{
	printf("difxMessage: %s\n", difxMessageIdentifier);
	printf("  group/port = %s/%d\n", difxMessageGroup, difxMessagePort);
	printf("  hostname = %s\n", difxMessageHostname);
}
