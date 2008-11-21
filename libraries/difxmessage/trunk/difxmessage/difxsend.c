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
	char message[DIFX_MESSAGE_LENGTH];
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

int difxMessageSendDifxAlert(const char *alertMessage, int severity)
{
	char message[1500];
	char body[1200];

	if(difxMessagePort < 0)
	{
		/* send to stderr or stdout if no port is defined */
		if(severity < DIFX_ALERT_LEVEL_WARNING)
		{
			fprintf(stderr, "[%s %d] %7s %s\n", difxMessageHostname, difxMessageMpiProcessId, difxMessageAlertString[severity], alertMessage);
		}
		else
		{
			printf("[%s %d] %7s %s\n", difxMessageHostname, difxMessageMpiProcessId, difxMessageAlertString[severity], alertMessage);
		}
	}
	else
	{
		sprintf(body,
			
			"<difxAlert>"
			  "<alertMessage>%s</alertMessage>"
			  "<severity>%d</severity>"
			"</difxAlert>",

			alertMessage, 
			severity);

		sprintf(message, difxMessageXMLFormat, 
			DifxMessageTypeStrings[DIFX_MESSAGE_ALERT],
			difxMessageSequenceNumber++, body);
		
		difxMessageSend(message);

		/* Make sure all fatal errors go to the console */
		if(severity == DIFX_ALERT_LEVEL_FATAL)
		{
			fprintf(stderr, "[%s %d] %7s %s\n", difxMessageHostname, difxMessageMpiProcessId, difxMessageAlertString[severity], alertMessage);
		}
	}
	
	return 0;
}

int difxMessageSendMark5Status(const DifxMessageMk5Status *mk5status)
{
	char message[DIFX_MESSAGE_LENGTH];
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
	char message[DIFX_MESSAGE_LENGTH];
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
	char message[DIFX_MESSAGE_LENGTH];
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

/* mpiDestination: 
	>= 0 implies mpiId, 
	  -1 implies ALL, 
	  -2 implies all Datastrems, 
	  -3 implies all Cores
*/
int difxMessageSendDifxParameter(const char *name, 
	const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[700];

	sprintf(body,
		
		"<difxParameter>"
		  "<name>%s</name>"
		  "<value>%s</value>"
		"</difxParameter>",

		name,
		value);

	sprintf(message, difxMessageXMLParamFormat,
		mpiDestination,
		DifxMessageTypeStrings[DIFX_MESSAGE_COMMAND],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxParameter1(const char *name, int index1,
	const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[700];

	sprintf(body,
		
		"<difxParameter>"
		  "<name>%s</name>"
		  "<index1>%d</index1>"
		  "<value>%s</value>"
		"</difxParameter>",

		name,
		index1,
		value);

	sprintf(message, difxMessageXMLParamFormat,
		mpiDestination,
		DifxMessageTypeStrings[DIFX_MESSAGE_COMMAND],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxParameter2(const char *name, int index1, int index2,
	const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[700];

	sprintf(body,
		
		"<difxParameter>"
		  "<name>%s</name>"
		  "<index1>%d</index1>"
		  "<index2>%d</index1>"
		  "<value>%s</value>"
		"</difxParameter>",

		name,
		index1,
		index2,
		value);

	sprintf(message, difxMessageXMLParamFormat,
		mpiDestination,
		DifxMessageTypeStrings[DIFX_MESSAGE_COMMAND],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}

int difxMessageSendDifxParameterGeneral(const char *name, int nIndex, const int *index,
	const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[700];
	char indices[200];
	int i;
	int p=0;

	for(i = 0; i < nIndex; i++)
	{
		p += sprintf(indices + p, "<index%d>%d</index%d>", i+1, index[i], i+1);
	}

	sprintf(body,
		
		"<difxParameter>"
		  "<name>%s</name>"
		  "%s"
		  "<value>%s</value>"
		"</difxParameter>",

		name,
		indices,
		value);

	sprintf(message, difxMessageXMLParamFormat,
		mpiDestination,
		DifxMessageTypeStrings[DIFX_MESSAGE_COMMAND],
		difxMessageSequenceNumber++, body);
	
	return difxMessageSend(message);
}
