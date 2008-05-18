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

static void XMLCALL charHandler(void *userData, const XML_Char *s, int len)
{
	DifxMessageGeneric *G;
	const char *elem;
	enum DifxMessageType t;

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
				break;
			case DIFX_MESSAGE_ERROR:
				break;
			case DIFX_MESSAGE_MARK5STATUS:
				break;
			case DIFX_MESSAGE_STATUS:
				break;
			case DIFX_MESSAGE_INFO:
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
	
	G->_xml_level = -1;
	G->_xml_error_count = 0;
	G->nTo = 0;
	parser = XML_ParserCreate(0);
	XML_ParserReset(parser, 0);
	XML_SetElementHandler(parser, startElement, endElement);
	XML_SetCharacterDataHandler(parser, charHandler);
	XML_SetUserData(parser, G);
	XML_Parse(parser, message, strlen(message), 0);
	XML_ParserFree(parser);

	return G->_xml_error_count;
}
