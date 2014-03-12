/***************************************************************************
 *   Copyright (C) 2010-2014 by Walter Brisken and Adam Deller             *
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
#include <sys/socket.h>
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
	const int recBufSize = 1000000;
	int sock = -1;

	if(destination == BINARY_STA)
	{
		if(difxBinarySTAPort < 0)
		{
			return -1;
		}

		sock = openMultiCastSocket(difxBinarySTAGroup, difxBinarySTAPort);
	}
	else
	{
		if(destination != BINARY_LTA)
                {
                        return -1;
                }

		if(difxBinaryLTAPort < 0)
                {
                        return -1;
                }

                sock = openMultiCastSocket(difxBinaryLTAGroup, difxBinaryLTAPort);
        }

	/* Increase receive buffer size to accommodate bursty traffic */
	if(sock >= 0)
	{
		setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &recBufSize, sizeof(recBufSize));
	}

	return sock;
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

