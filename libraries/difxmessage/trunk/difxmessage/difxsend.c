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
#include <string.h>
#include <ctype.h>
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

int difxMessageSendLoad(const DifxMessageLoad *load)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxLoad>"
		  "<cpuLoad>%4.2f</cpuLoad>"
		  "<totalMemory>%d</totalMemory>"
		  "<usedMemory>%d</usedMemory>"
		  "<netRXRate>%d</netRXRate>"
		  "<netTXRate>%d</netTXRate>"
		"</difxLoad>",

		load->cpuLoad,
		load->totalMemory,
		load->usedMemory,
		load->netRXRate,
		load->netTXRate);

	sprintf(message, difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_LOAD],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxError(const char *errorMessage, int severity)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxError>"
		  "<errorMessage>%s</errorMessage>"
		  "<severity>%d</severity>"
		"</difxError>",

		errorMessage, 
		severity);

	sprintf(message, difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_ERROR],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendMark5Status(const DifxMessageMk5Status *mk5status)
{
	char message[1000];
	char body[700];
	char vsnA[10], vsnB[10];
	char scanName[64];
	char bank;
	int i;
	

	if(strlen(mk5status->vsnA) != 8)
	{
		strcpy(vsnA, "none");
	}
	else
	{
		for(i = 0; i < 9; i++)
		{
			vsnA[i] = toupper(mk5status->vsnA[i]);
		}
	}
	if(strlen(mk5status->vsnB) != 8)
	{
		strcpy(vsnB, "none");
	}
	else
	{
		for(i = 0; i < 9; i++)
		{
			vsnB[i] = toupper(mk5status->vsnB[i]);
		}
	}
	if(!isalpha(mk5status->activeBank))
	{
		bank = ' ';
	}
	else
	{
		bank = toupper(mk5status->activeBank);
	}
	strncpy(scanName, mk5status->scanName, 63);
	scanName[63] = 0;
	sprintf(body, 
	
		"<mark5Status>"
		  "<bankAVSN>%s</bankAVSN>"
		  "<bankBVSN>%s</bankBVSN>"
		  "<statusWord>0x%08x</statusWord>"
		  "<activeBank>%c</activeBank>"
		  "<state>%s</state>"
		  "<scanNumber>%d</scanNumber>"
		  "<scanName>%s</scanName>"
		  "<position>%lld</position>"
		  "<playRate>%7.3f</playRate>"
		  "<dataMJD>%13.7f</dataMJD>"
		"</mark5Status>",

		vsnA,
		vsnB,
		mk5status->status,
		bank,
		Mk5StateStrings[mk5status->state],
		mk5status->scanNumber,
		scanName,
		mk5status->position,
		mk5status->rate,
		mk5status->dataMJD);

	sprintf(message, difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_MARK5STATUS],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxStatus(enum DifxState state, const char *stateMessage,
	double visMJD, int numdatastreams, float *weight)
{
	char message[1500], weightstr[1200];
	char body[700];
	int i, n;
	
	weightstr[0] = 0;
	n = 0;

	for(i = 0; i < numdatastreams; i++)
	{
		if(weight[i] >= 0)
		{
			n += sprintf(weightstr + n, 
				"<weight ant=\"%d\" wt=\"%5.3f\"/>", 
				i, weight[i]);
		}
	}

	sprintf(body,
		
		"<difxStatus>"
		  "<state>%s</state>"
		  "<message>%s</message>"
		  "<visibilityMJD>%13.7f</visibilityMJD>"
		  "%s"
		"</difxStatus>",

		DifxStateStrings[state],
		stateMessage,
		visMJD,
		weightstr);

	sprintf(message, difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_STATUS],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxInfo(const char *infoMessage)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxInfo>"
		  "<message>%s</message>"
		"</difxInfo>",

		infoMessage);

	sprintf(message, difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_INFO],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxCommand(const char *command)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxCommand>"
		  "<command>%s</command>"
		"</difxCommand>",

		command);

	sprintf(message, difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_COMMAND],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}
