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

int difxMessageSendMark5State(const DifxMessageMk5Status *mk5state)
{
	char message[1000];
	char body[700];

	sprintf(body, 
	
		"<mark5Status>"
		  "<bankAVSN>%s</bankAVSN>"
		  "<bankBVSN>%s</bankBVSN>"
		  "<statusWord>%08x</statusWord>"
		  "<activeBank>%c</activeBank>"
		  "<moduleState>%s</moduleState>"
		  "<scanNumber>%d</scanNumber>"
		  "<position>%lld</position>"
		  "<playRate>%7.3f</playRate>"
		  "<dataMJD>%13.7f</dataMJD>"
		"</mark5Status>",

		mk5state->vsnA,
		mk5state->vsnB,
		mk5state->status,
		mk5state->activeBank,
		Mk5StatusStrings[mk5state->state],
		mk5state->scanNumber,
		mk5state->position,
		mk5state->rate,
		mk5state->dataMJD);

	sprintf(message, difxMessageXMLFormat, "Mark5StatusMessage", 
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}
