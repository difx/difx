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
char difxMessageIdentifier[DIFX_MESSAGE_IDENTIFER_LENGTH] = "";
char difxMessageHostname[DIFX_MESSAGE_PARAM_LENGTH] = "";
int difxMessageMpiProcessId = -1;
char difxMessageXMLFormat[256] = "";
int difxMessageSequenceNumber = 0;
char difxBinarySTAGroup[16] = "";
int difxBinarySTAPort = -1;
char difxBinaryLTAGroup[16] = "";
int difxBinaryLTAPort = -1;
int difxMessageInUse = 0;

const char *getDifxMessageIdentifier()
{
	return difxMessageIdentifier;
}

int difxMessageInit(int mpiId, const char *identifier)
{
	const char *envstr;

	difxMessageInUse = 1;
	difxMessageSequenceNumber = 0;
	
	strncpy(difxMessageIdentifier, identifier, DIFX_MESSAGE_IDENTIFER_LENGTH);
	difxMessageIdentifier[DIFX_MESSAGE_IDENTIFER_LENGTH-1] = 0;

	difxMessageMpiProcessId = mpiId;

	gethostname(difxMessageHostname, DIFX_MESSAGE_PARAM_LENGTH);
	difxMessageHostname[DIFX_MESSAGE_PARAM_LENGTH-1] = 0;

	envstr = getenv("DIFX_MESSAGE_GROUP");
	if(envstr != 0)
	{
		strncpy(difxMessageGroup, envstr, 16);
		difxMessageGroup[15] = 0;
	}
	else
	{
		difxMessageInUse = 0;
	}

	envstr = getenv("DIFX_MESSAGE_PORT");
	if(envstr != 0)
	{
		difxMessagePort = atoi(envstr);
	}
	else
	{
		difxMessageInUse = 0;
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
		"</difxMessage>",
		
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
		strncpy(difxBinarySTAGroup, envstr, 16);
		difxBinarySTAGroup[15] = 0;
	}

	envstr = getenv("DIFX_BINARY_PORT");
	if(envstr != 0)
	{
		difxBinarySTAPort = atoi(envstr);
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
