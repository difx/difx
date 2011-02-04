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
#include "../difxmessage.h"
#include "difxmessageinternal.h"

int difxMessageSendBinary(const char *message, int destination, int length)
{
	if(destination == BINARY_STA)
	{
		if(difxBinarySTAPort < 0)
		{
			return -1;
		}

		return MulticastSend(difxBinarySTAGroup, difxBinarySTAPort, message, length);
	}
	else
	{
		if (destination != BINARY_LTA)
		{
			return -1;
		}
                if(difxBinaryLTAPort < 0)
                {
                        return -1;
                }

                return MulticastSend(difxBinaryLTAGroup, difxBinaryLTAPort, message, length);
        }

}

int difxMessageBinaryOpen(int destination)
{
	if(destination == BINARY_STA)
	{
		if(difxBinarySTAPort < 0)
		{
			return -1;
		}

		return openMultiCastSocket(difxBinarySTAGroup, difxBinarySTAPort);
	}
	else
	{
		if (destination != BINARY_LTA)
                {
                        return -1;
                }

		if(difxBinaryLTAPort < 0)
                {
                        return -1;
                }

                return openMultiCastSocket(difxBinaryLTAGroup, difxBinaryLTAPort);
        }

}

int difxMessageBinaryClose(int sock)
{
	return closeMultiCastSocket(sock);
}

int difxMessageBinaryRecv(int sock, char *message, int maxlen, char *from)
{
        return MultiCastReceive(sock, message, maxlen, from);
}

void fprintDifxMessageSTARecord(FILE *out, const DifxMessageSTARecord *record, int printData)
{
	int i;
	fprintf(out, "DifxMessageSTARecord [%p]\n", record);
	if(!record)
	{
		return;
	}
	fprintf(out, "  messageType = %d\n", record->messageType);
	fprintf(out, "  scan = %d\n", record->scan);
	fprintf(out, "  sec = %d\n", record->sec);
	fprintf(out, "  ns = %d\n", record->ns);
	fprintf(out, "  nswidth = %d\n", record->nswidth);
	fprintf(out, "  dsindex = %d\n", record->dsindex);
	fprintf(out, "  coreindex = %d\n", record->coreindex);
	fprintf(out, "  threadindex = %d\n", record->threadindex);
	fprintf(out, "  bandindex = %d\n", record->bandindex);
	fprintf(out, "  nChan = %d\n", record->nChan);
	fprintf(out, "  identifier = %s\n", record->identifier);
	fprintf(out, "  data =\n");
	if(printData)
	{
		for(i = 0; i < record->nChan; i++)
		{
			fprintf(out, "    %f\n", record->data[i]);
		}
	}
}

void printDifxMessageSTARecord(const DifxMessageSTARecord *record, int printData)
{
	fprintDifxMessageSTARecord(stdout, record, printData);
}

