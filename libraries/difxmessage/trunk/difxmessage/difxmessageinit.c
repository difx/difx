/***************************************************************************
 *   Copyright (C) 2007-2014 by Walter Brisken                             *
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <netinet/in.h>

#include "../difxmessage.h"
#include "difxmessageinternal.h"

#define MAX_GROUP_SIZE			16

char difxMessageGroup[MAX_GROUP_SIZE] = "";
int difxMessagePort = -1; 
char difxMessageIdentifier[DIFX_MESSAGE_IDENTIFIER_LENGTH] = "";
char difxMessageHostname[DIFX_MESSAGE_PARAM_LENGTH] = "";
char difxMessageInputFilenameTag[DIFX_MESSAGE_FILENAME_TAG_LENGTH] = "";
int difxMessageMpiProcessId = -1;
char difxMessageXMLFormat[DIFX_MESSAGE_FORMAT_LENGTH] = "";
char difxMessageToXMLFormat[DIFX_MESSAGE_FORMAT_LENGTH] = "";
int difxMessageSequenceNumber = 0;
char difxBinarySTAGroup[MAX_GROUP_SIZE] = "";
int difxBinarySTAPort = -1;
char difxBinaryLTAGroup[MAX_GROUP_SIZE] = "";
int difxBinaryLTAPort = -1;
int difxMessageInUse = 0;
int difxMessageUnicast = 0;

const char *getDifxMessageIdentifier()
{
	return difxMessageIdentifier;
}

/* if called, the <inputFile> tag will be added within DifxMessageAlert and DifxMessageStatus <body> */
int difxMessageSetInputFilename(const char *inputFilename)
{
	int v;

	v = snprintf(difxMessageInputFilenameTag, DIFX_MESSAGE_FILENAME_TAG_LENGTH, "<input>%s</input>", inputFilename);

	if(v >= DIFX_MESSAGE_FILENAME_TAG_LENGTH)
	{
		/* name was too long */

		return -1;
	}

	return 0;
}

int difxMessageInitFull(int mpiId, const char *identifier, const char *publishedHostname)
{
	const char *envstr;
	int size;

	difxMessageSequenceNumber = 0;
	difxMessageInUse = 1;
	difxMessageInputFilenameTag[0] = 0;
	
	if(identifier != difxMessageIdentifier)
	{
		/* only copy if the pointers differ -- there are legitmate reasons why they may be the same */
		snprintf(difxMessageIdentifier, DIFX_MESSAGE_IDENTIFIER_LENGTH, "%s", identifier);
	}

	difxMessageMpiProcessId = mpiId;

	if(publishedHostname)
	{
		snprintf(difxMessageHostname, DIFX_MESSAGE_PARAM_LENGTH, "%s", publishedHostname);
	}
	else
	{
		gethostname(difxMessageHostname, DIFX_MESSAGE_PARAM_LENGTH);
		difxMessageHostname[DIFX_MESSAGE_PARAM_LENGTH-1] = 0;
	}

	envstr = getenv("DIFX_MESSAGE_GROUP");
	if(envstr != 0)
	{
		size = snprintf(difxMessageGroup, MAX_GROUP_SIZE, "%s", envstr);
		if(size >= MAX_GROUP_SIZE)
		{
			fprintf(stderr, "Error: difxMessageInit: env var DIFX_MESSAGE_GROUP too long\n");
			difxMessageInUse = 0;
			return -1;
		}
		// Is it unicast?

		struct in_addr addr;
		int status = inet_aton(difxMessageGroup, &addr);
		if(status==0) {
		  fprintf(stderr, "Error: difxMessageInit. DIFX_MESSAGE_GROUP %s invalid\n", difxMessageGroup);
		  difxMessageInUse = 0;
		  return -1;
		}
		// Is it really multicast
		if (ntohl(addr.s_addr)>>28!=14) { 
		  difxMessageUnicast = 1;
		  if (difxMessageMpiProcessId==0) 
		    printf("Warning: Unicast (%s -> %08X) difxMessage in use. Some functionallity may be reduced\n", difxMessageGroup, ntohl(addr.s_addr));
		} 
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

	size = snprintf(difxMessageXMLFormat, DIFX_MESSAGE_FORMAT_LENGTH,
		
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

	if(size >= DIFX_MESSAGE_FORMAT_LENGTH)
	{
		fprintf(stderr, "Error: difxMessageInit: format string overflow: difxMessageXMLFormat\n");

		return -1;
	}

	size = snprintf(difxMessageToXMLFormat, DIFX_MESSAGE_FORMAT_LENGTH,
		
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
		"<difxMessage>"
		  "<header>"
		    "<from>%s</from>"
		    "<to>%%s</to>"
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

	if(size >= DIFX_MESSAGE_FORMAT_LENGTH)
	{
		fprintf(stderr, "Error: difxMessageInit: format string overflow: difxMessageToXMLFormat\n");

		return -1;
	}

	/* send a message to prime the network for multicast */
	difxMessageSendDifxAlert("Initialized", DIFX_ALERT_LEVEL_VERBOSE);
	
	return 0;
}

int difxMessageInit(int mpiId, const char *identifier)
{
	return difxMessageInitFull(mpiId, identifier, 0);
}

int difxMessageInitBinary()
{
	const char *envstr;
	int v;

	envstr = getenv("DIFX_BINARY_GROUP");
	if(envstr != 0)
	{
		v = snprintf(difxBinarySTAGroup, MAX_GROUP_SIZE, "%s", envstr);
		if(v >= MAX_GROUP_SIZE)
		{
			fprintf(stderr, "Error: difxMessageInitBinary: env var DIFX_BINARY_GROUP too long\n");
			
			return -1;
		}
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
	printf("  identifier = %s / %d\n", difxMessageIdentifier, difxMessageMpiProcessId);
}

void difxMessageGetMulticastGroupPort(char *group, int *port)
{
	strcpy(group, difxMessageGroup);
	*port = difxMessagePort;
}
