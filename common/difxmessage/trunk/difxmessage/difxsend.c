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

int difxMessageSendLoad(const DifxMessageLoad *load)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxLoad>"
		  "<cpuLoad>%4.2f</cpuLoad>"
		  "<totalMemory>%d</totalMemory>"
		  "<usedMemory>%d</usedMemory>"
		"</difxLoad>",

		load->cpuLoad,
		load->totalMemory,
		load->usedMemory);

	sprintf(message, difxMessageXMLFormat, "DifxLoadMessage", 
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxError(const char *errorString, int severity)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxError>"
		  "<errorMessage>%s</errorMessage>"
		  "<severity>%d</severity>"
		"</difxError>",

		errorString, 
		severity);

	sprintf(message, difxMessageXMLFormat, "DifxErrorMessage", 
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendMark5Status(const DifxMessageMk5Status *mk5status)
{
	char message[1000];
	char body[700];

	sprintf(body, 
	
		"<mark5Status>"
		  "<bankAVSN>%s</bankAVSN>"
		  "<bankBVSN>%s</bankBVSN>"
		  "<statusWord>0x%08ux</statusWord>"
		  "<activeBank>%c</activeBank>"
		  "<state>%s</state>"
		  "<scanNumber>%d</scanNumber>"
		  "<position>%lld</position>"
		  "<playRate>%7.3f</playRate>"
		  "<dataMJD>%13.7f</dataMJD>"
		"</mark5Status>",

		mk5status->vsnA,
		mk5status->vsnB,
		mk5status->status,
		mk5status->activeBank,
		Mk5StateStrings[mk5status->state],
		mk5status->scanNumber,
		mk5status->position,
		mk5status->rate,
		mk5status->dataMJD);

	sprintf(message, difxMessageXMLFormat, "Mark5StatusMessage", 
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxStatus(enum DifxState state, const char *stateMessage,
	double visMJD)
{
	char message[1000];
	char body[700];

	sprintf(body,
		
		"<difxStatus>"
		  "<state>%s</state>"
		  "<message>%s</message>"
		  "<visibilityMJD>%13.7f</visibilityMJD>"
		"</difxStatus>",

		DifxStateStrings[state],
		stateMessage,
		visMJD);

	sprintf(message, difxMessageXMLFormat, "DifxStatusMessage", 
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}
