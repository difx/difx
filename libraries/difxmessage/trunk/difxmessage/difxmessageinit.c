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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "../difxmessage.h"
#include "difxmessageinternal.h"

char difxMessageGroup[16] = "";
int difxMessagePort = -1; 
char difxMessageIdentifier[128] = "";
char difxMessageHostname[32] = "";
int difxMessageMpiProcessId = -1;
char difxMessageXMLFormat[256] = "";
int difxMessageSequenceNumber = 0;
char difxBinaryGroup[16] = "";
int difxBinaryPort = -1;

int difxMessageInit(int mpiId, const char *identifier)
{
	const char *envstr;

	difxMessageSequenceNumber = 0;
	
	strncpy(difxMessageIdentifier, identifier, MAX_DIFX_MESSAGE_IDENTIFER);
	difxMessageIdentifier[MAX_DIFX_MESSAGE_IDENTIFER-1] = 0;

	difxMessageMpiProcessId = mpiId;

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

	sprintf(difxMessageXMLFormat, 
		
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
		"<difxMessage>"
		  "<header>"
		    "<from>%s</from>"
		    "<mpiProcessId>%d</mpiProcessId>"
		    "<identifier>%s</identifier>"
		    "<type>%%s</type>"
		  "</header>"
		  "<body>"
		    "<seqNumber>%%d</seqNumber>"
		    "%%s"
		  "</body>"
		"</difxMessage>\n",
		
		difxMessageHostname, 
		difxMessageMpiProcessId,
		difxMessageIdentifier);

	return 0;
}
int difxMessageInitBinary()
{
	const char *envstr;

	envstr = getenv("DIFX_BINARY_GROUP");
	if(envstr != 0)
	{
		strncpy(difxBinaryGroup, envstr, 16);
		difxBinaryGroup[15] = 0;
	}

	envstr = getenv("DIFX_BINARY_PORT");
	if(envstr != 0)
	{
		difxBinaryPort = atoi(envstr);
	}

	return 0;
}

void difxMessagePrint()
{
	printf("difxMessage: %s\n", difxMessageIdentifier);
	printf("  group/port = %s/%d\n", difxMessageGroup, difxMessagePort);
	printf("  hostname = %s\n", difxMessageHostname);
	printf("  identifier = %s / %d\n", difxMessageIdentifier, 
		difxMessageMpiProcessId);
}

void difxMessageGetMulticastGroupPort(char *group, int *port)
{
	strcpy(group, difxMessageGroup);
	*port = difxMessagePort;
}
