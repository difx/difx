
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
#include <string.h>
#include <expat.h>
#include "../difxmessage.h"

static void XMLCALL startElement(void *userData, const char *name, 
	const char **atts)
{
	DifxMessageGeneric *G;

	G = (DifxMessageGeneric *)userData;
	G->_xml_level++;
	strcpy(G->_xml_element[G->_xml_level], name);
}

static void XMLCALL endElement(void *userData, const char *name)
{
	DifxMessageGeneric *G;

	G = (DifxMessageGeneric *)userData;
	G->_xml_level--;
}

static void XMLCALL charHandler(void *userData, const XML_Char *str, int len)
{
	DifxMessageGeneric *G;
	const char *elem;
	enum DifxMessageType t;
	enum Mk5State m;
	enum DifxState d;
	char s[1024];

	if(len > 1023)
	{
		len = 1023;
	}
	strncpy(s, str, len);
	s[len] = 0;

	G = (DifxMessageGeneric *)userData;
	elem = G->_xml_element[G->_xml_level];

	if(strcmp(G->_xml_element[0], "difxMessage") == 0)
	{
		if(strcmp(G->_xml_element[1], "header") == 0)
		{
			if(strcmp(elem, "from") == 0)
			{
				strncpy(G->from, s, 31);
				G->from[31] = 0;
			}
			else if(strcmp(elem, "to") == 0)
			{
				strncpy(G->to[G->nTo], s, 31);
				G->to[G->nTo][31] = 0;
				G->nTo++;
			}
			else if(strcmp(elem, "mpiProcessId") == 0)
			{
				G->mpiId = atoi(s);
			}
			else if(strcmp(elem, "identifier") == 0)
			{
				strncpy(G->identifier, s, 31);
				G->identifier[31] = 0;
			}
			else if(strcmp(elem, "type") == 0)
			{
				for(t = 0; t < NUM_DIFX_MESSAGE_TYPES; t++)
				{
					if(!strcmp(DifxMessageTypeStrings[t],s))
					{
						G->type = t;
					}
				}
			}
		}
		else if(strcmp(G->_xml_element[1], "body") == 0)
		{
			if(strcmp(elem, "seqNumber") == 0)
			{
				G->seqNumber = atoi(s);
			}
			switch(G->type)
			{
			case DIFX_MESSAGE_LOAD:
				if(strcmp(elem, "cpuLoad") == 0)
				{
					G->body.load.cpuLoad = atof(s);
				}
				else if(strcmp(elem, "totalMemory") == 0)
				{
					G->body.load.totalMemory = atof(s);
				}
				else if(strcmp(elem, "usedMemory") == 0)
				{
					G->body.load.usedMemory = atof(s);
				}
				else if(strcmp(elem, "netRXRate") == 0)
				{
					G->body.load.netRXRate = atof(s);
				}
				else if(strcmp(elem, "netTXRate") == 0)
				{
					G->body.load.netTXRate = atof(s);
				}
				break;
			case DIFX_MESSAGE_ERROR:
				if(strcmp(elem, "errorMessage") == 0)
				{
					strncpy(G->body.error.message, s, 999);
					G->body.error.message[999] = 0;
				}
				else if(strcmp(elem, "severity") == 0)
				{
					G->body.error.severity = atoi(s);
				}
				break;
			case DIFX_MESSAGE_MARK5STATUS:
				if(strcmp(elem, "bankAVSN") == 0)
				{
					strncpy(G->body.mk5status.vsnA, s, 8);
					G->body.mk5status.vsnA[8] = 0;
				}
				else if(strcmp(elem, "bankBVSN") == 0)
				{
					strncpy(G->body.mk5status.vsnB, s, 8);
					G->body.mk5status.vsnB[8] = 0;
				}
				else if(strcmp(elem, "statusWord") == 0)
				{
					sscanf(s, "%x", 
						&G->body.mk5status.status);
				}
				else if(strcmp(elem, "activeBank") == 0)
				{
					G->body.mk5status.activeBank = s[0];
				}
				else if(strcmp(elem, "state") == 0)
				{
					G->body.mk5status.state = 0;
					for(m = 0; m < NUM_MARK5_STATES; m++)
					{
						if(strcmp(Mk5StateStrings[m], s) == 0)
						{
							G->body.mk5status.state = m;
						}
					}
				}
				else if(strcmp(elem, "scanNumber") == 0)
				{
					G->body.mk5status.scanNumber = atoi(s);
				}
				else if(strcmp(elem, "scanName") == 0)
				{
					strncpy(G->body.mk5status.scanName, s,63);
					G->body.mk5status.scanName[63] = 0;
				}
				else if(strcmp(elem, "position") == 0)
				{
					G->body.mk5status.position = atoll(s);
				}
				else if(strcmp(elem, "playRate") == 0)
				{
					G->body.mk5status.rate = atof(s);
				}
				else if(strcmp(elem, "dataMJD") == 0)
				{
					G->body.mk5status.dataMJD = atof(s);
				}
				break;
			case DIFX_MESSAGE_STATUS:
				if(strcmp(elem, "message") == 0)
				{
					strncpy(G->body.status.message, s, 999);
					G->body.status.message[999] = 0;
				}
				else if(strcmp(elem, "state") == 0)
				{
					for(d = 0; d < NUM_DIFX_STATES; d++)
					{
						if(strcmp(DifxStateStrings[d], s) == 0)
						{
							G->body.status.state = d;
						}
					}
				}
				break;
			case DIFX_MESSAGE_INFO:
				if(strcmp(elem, "message") == 0)
				{
					strncpy(G->body.info.message, s, 999);
					G->body.info.message[999] = 0;
				}
				break;
			case DIFX_MESSAGE_COMMAND:
				if(strcmp(elem, "command") == 0)
				{
					strncpy(G->body.command.command, s, 999);
					G->body.command.command[999] = 0;
				}
				break;
			default:
				break;
			}
		}
	}
}

int difxMessageParse(DifxMessageGeneric *G, const char *message)
{
	XML_Parser parser;
	
	memset(G, 0, sizeof(DifxMessageGeneric));
	G->_xml_level = -1;
	parser = XML_ParserCreate(0);
	XML_ParserReset(parser, 0);
	XML_SetElementHandler(parser, startElement, endElement);
	XML_SetCharacterDataHandler(parser, charHandler);
	XML_SetUserData(parser, G);
	XML_Parse(parser, message, strlen(message), 0);
	XML_ParserFree(parser);

	return G->_xml_error_count;
}

void difxMessageGenericPrint(const DifxMessageGeneric *G)
{
	int i;

	printf("Generic Message [%p]\n", G);
	printf("  from = %s\n", G->from);
	printf("  to =");
	for(i = 0; i < G->nTo; i++)
	{
		printf(" %s", G->to[i]);
	}
	printf("\n");
	printf("  identifier = %s\n", G->identifier);
	printf("  mpi id = %d\n", G->mpiId);
	printf("  type = %s %d\n", DifxMessageTypeStrings[G->type], G->type);
	switch(G->type)
	{
	case DIFX_MESSAGE_LOAD:
		printf("    cpu load = %f\n", G->body.load.cpuLoad);
		printf("    total memory = %d kiB\n", G->body.load.totalMemory);
		printf("    used memory = %d kiB\n", G->body.load.usedMemory);
		printf("    network Receive Rate = %d B/s\n", 
			G->body.load.netRXRate);
		printf("    network Transmit Rate = %d B/s\n", 
			G->body.load.netTXRate);
		break;
	case DIFX_MESSAGE_ERROR:
		printf("    severity = %d\n", G->body.error.severity);
		printf("    message = %s\n", G->body.error.message);
		break;
	case DIFX_MESSAGE_MARK5STATUS:
		printf("    state = %s %d\n", 
			Mk5StateStrings[G->body.mk5status.state],
			G->body.mk5status.state);
		printf("    VSN A = %s\n", G->body.mk5status.vsnA);
		printf("    VSN B = %s\n", G->body.mk5status.vsnB);
		printf("    status word = 0x%x\n", G->body.mk5status.status);
		printf("    activeBank = %c\n", G->body.mk5status.activeBank);
		printf("    scanNumber = %d\n", G->body.mk5status.scanNumber);
		printf("    scanName = %s\n", G->body.mk5status.scanName);
		printf("    position = %lld\n", G->body.mk5status.position);
		printf("    rate = %7.3f Mbps\n", G->body.mk5status.rate);
		printf("    data MJD = %12.6f\n", G->body.mk5status.dataMJD);
		break;
	case DIFX_MESSAGE_STATUS:
		printf("    state = %s %d\n", 
			DifxStateStrings[G->body.status.state],
			G->body.status.state);
		printf("    message = %s\n", G->body.status.message);
		break;
	case DIFX_MESSAGE_INFO:
		printf("    message = %s\n", G->body.info.message);
		break;
	case DIFX_MESSAGE_COMMAND:
		printf("    command = %s\n", G->body.command.command);
		break;
	default:
		break;
	}
	printf("  xml errors = %d\n", G->_xml_error_count);
}
