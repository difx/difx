/***************************************************************************
 *   Copyright (C) 2007-2016 by Walter Brisken                             *
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
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <time.h>
#include "../difxmessage.h"
#include "difxmessageinternal.h"

const int MIN_SEND_GAP=20;

/* Function that replaces illegal XML string characters with the
 * official "entity" replacements.  
 *
 * Returns:
 *   Increse of string size on success, or -1 on error.
 */
static int expandEntityReferences(char *dest, const char *src, int maxLength)
{
	int i, j;

	for(i = j = 0; src[i]; ++i)
	{
		if(j >= maxLength-7)
		{
			return -1;
		}

		if(src[i] == '>')
		{
			strcpy(dest+j, "&gt;");
			j += 4;
		}
		else if(src[i] == '<')
		{
			strcpy(dest+j, "&lt;");
			j += 4;
		}
		else if(src[i] == '&')
		{
			strcpy(dest+j, "&amp;");
			j += 5;
		}
		else if(src[i] == '"')
		{
			strcpy(dest+j, "&quot;");
			j += 6;
		}
		else if(src[i] == '\'')
		{
			strcpy(dest+j, "&apos;");
			j += 6;
		}
		else if(src[i] < 32)	/* ascii chars < 32 are not allowed */
		{
			sprintf(dest+j, "[[%3d]]", src[i]);
			j += 7;
		}
		else
		{
			dest[j] = src[i];
			++j;
		}
	}

	dest[j] = 0;

	return j - i;
}

int difxMessageSend2(const char *message, int size)
{
	static int first = 1;
	static struct timeval tv0;

	struct timeval tv;

	if(difxMessagePort < 0)
	{
		return -1;
	}

	if(first)
	{
		first = 0;
		gettimeofday(&tv0, 0);
	}
	else
	{
		int dt;

		gettimeofday(&tv, 0);
		dt = 1000000*(tv.tv_sec - tv0.tv_sec) + (tv.tv_usec - tv0.tv_usec);
		if(dt < MIN_SEND_GAP && dt > 0)
		{
			struct timespec ts;

			ts.tv_sec = 0;
			ts.tv_nsec = 1000*(MIN_SEND_GAP-dt);

			/* The minimum gap prevents two messages from being sent too soon 
			 * after each other, a condition that apparently can lead to lost
			 * messages 
			 */
			nanosleep(&ts, 0);
		}
		tv0 = tv;
	}

	return MulticastSend(difxMessageGroup, difxMessagePort, message, size);
}

int difxMessageSend(const char *message)
{
	return difxMessageSend2(message, strlen(message));
}

int difxMessageSendProcessState(const char *state)
{
	char message[DIFX_MESSAGE_LENGTH];
	int size;

	if(difxMessagePort < 0)
	{
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH, "%s %s", difxMessageIdentifier, state);

	return difxMessageSend2(message, size);
}

int difxMessageSendLoad(const DifxMessageLoad *load)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxLoad>"
		  "<cpuLoad>%4.2f</cpuLoad>"
		  "<totalMemory>%d</totalMemory>"
		  "<usedMemory>%d</usedMemory>"
		  "<netRXRate>%d</netRXRate>"
		  "<netTXRate>%d</netTXRate>"
		  "<nCore>%d</nCore>"
		"</difxLoad>",

		load->cpuLoad,
		load->totalMemory,
		load->usedMemory,
		load->netRXRate,
		load->netTXRate,
		load->nCore);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendLoad: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_LOAD],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendLoad: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

int difxMessageSendDifxAlert(const char *alertMessage, int severity)
{
	char message[DIFX_MESSAGE_LENGTH];
	char alertMessageExpanded[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

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
		size = expandEntityReferences(alertMessageExpanded, alertMessage, DIFX_MESSAGE_LENGTH);

		if(size < 0)
		{
			fprintf(stderr, "difxMessageSendDifxAlert: message body overflow in entity replacement (>= %d)\n", DIFX_MESSAGE_LENGTH);

			return -1;
		}

		size = snprintf(body, DIFX_MESSAGE_LENGTH,
			
			"<difxAlert>"
			  "%s"
			  "<alertMessage>%s</alertMessage>"
			  "<severity>%d</severity>"
			"</difxAlert>",

			difxMessageInputFilenameTag,
			alertMessageExpanded,
			severity);
		
		if(size >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendDifxAlert: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
			
			return -1;
		}

		size = snprintf(message, DIFX_MESSAGE_LENGTH,
			difxMessageXMLFormat, 
			DifxMessageTypeStrings[DIFX_MESSAGE_ALERT],
			difxMessageSequenceNumber++, body);
		
		if(size >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendDifxAlert: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

			return -1;
		}

		difxMessageSend2(message, size);

		/* Make sure all fatal errors go to the console */
		if(severity == DIFX_ALERT_LEVEL_FATAL)
		{
			fprintf(stderr, "[%s %d] %7s %s\n", difxMessageHostname, difxMessageMpiProcessId, difxMessageAlertString[severity], alertMessage);
		}
	}
	
	return 0;
}

int difxMessageSendStop(const char *inputFilename, const char *mpiWrapper, const char *difxVersion, const char *difxProgram)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	if(difxMessagePort <= 0)
	{
		fprintf(stderr, "Error: could not deliver stop message for input file %s because difxMessagePort is not set.\n", inputFilename);

		return -1;
	}

	if(inputFilename == 0)
	{
		return -2;
	}

	if(difxProgram == 0)
	{
		difxProgram = "mpifxcorr";
	}
	if(difxVersion == 0)
	{
		difxVersion = "";
	}
	if(mpiWrapper == 0)
	{
		mpiWrapper = "";
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxStop>"
		  "<input>%s</input>"
		  "<mpiWrapper>%s</mpiWrapper>"
		  "<difxVersion>%s</difxVersion>"
		  "<difxProgram>%s</difxProgram>"
		"</difxStop>",

		inputFilename,
		mpiWrapper,
		difxVersion,
		difxProgram);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendStop: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_STOP],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendStop: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	difxMessageSend2(message, size);

	return 0;
}

int difxMessageSendDriveStats(const DifxMessageDriveStats *driveStats)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	char bins[DIFX_MESSAGE_LENGTH];
	char startByteString[48];
	int b = 0;

	if(driveStats->type >= NUM_DRIVE_STATS_TYPES)
	{
		fprintf(stderr, "difxMessageSendDriveStats: illegal type number=%d; needs to be in [0, %d)\n", driveStats->type, NUM_DRIVE_STATS_TYPES);

		return -1;
	}

	bins[0] = 0;

	if(difxMessagePort < 0)
	{
		int i;
		
		for(i = 0; i < DIFX_MESSAGE_N_CONDITION_BINS; ++i)
		{
			b += snprintf(bins+b, DIFX_MESSAGE_LENGTH-b, " %d", driveStats->bin[i]);
			
			if(b >= DIFX_MESSAGE_LENGTH)
			{
				fprintf(stderr, "difxMessageSendDriveStats: message overflow (%d >= %d)\n", b, DIFX_MESSAGE_LENGTH);

				return -1;
			}
		}
		printf("%s[%d] = %s %s\n", 
			driveStats->moduleVSN, 
			driveStats->moduleSlot,
			driveStats->serialNumber,
			bins);
	}
	else
	{
		int i;
		int size;
		
		for(i = 0; i < DIFX_MESSAGE_N_CONDITION_BINS; ++i)
		{
			b += snprintf(bins+b, DIFX_MESSAGE_LENGTH-b, "<bin%d>%d</bin%d>", i, driveStats->bin[i], i);
			if(b >= DIFX_MESSAGE_LENGTH)
			{
				fprintf(stderr, "difxMessageSendDriveStats: message overflow (%d >= %d)\n", b, DIFX_MESSAGE_LENGTH);

				return -1;
			}
		}

		if(driveStats->startByte != 0LL)
		{
			sprintf(startByteString, "<startByte>%lld</startByte>", driveStats->startByte);
		}
		else
		{
			startByteString[0] = 0;
		}

		size = snprintf(body, DIFX_MESSAGE_LENGTH,
			
			"<difxDriveStats>"
			  "<serialNumber>%s</serialNumber>"
			  "<modelNumber>%s</modelNumber>"
			  "<size>%d</size>"
			  "<moduleVSN>%s</moduleVSN>"
			  "<moduleSlot>%d</moduleSlot>"
			  "<startMJD>%7.5f</startMJD>"
			  "<stopMJD>%7.5f</stopMJD>"
			  "<type>%s</type>"
			  "%s"
			  "%s"
			"</difxDriveStats>",

			driveStats->serialNumber,
			driveStats->modelNumber,
			driveStats->diskSize,
			driveStats->moduleVSN, 
			driveStats->moduleSlot,
			driveStats->startMJD,
			driveStats->stopMJD,
			DriveStatsTypeStrings[driveStats->type],
			startByteString,
			bins);

		if(size >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendDriveStats: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

			return -1;
		}

		size = snprintf(message, DIFX_MESSAGE_LENGTH,
			difxMessageXMLFormat, 
			DifxMessageTypeStrings[DIFX_MESSAGE_DRIVE_STATS],
			difxMessageSequenceNumber++, body);
		
		if(size >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendDriveStats: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

			return -1;
		}

		difxMessageSend2(message, size);
	}

	return 0;
}

/**
 * Sends out a message containing the Mark6 status information. 
 * @return 0 in case of success; -1 otherwise
 */
int difxMessageSendMark6Status(const DifxMessageMark6Status *mark6status)
{
	int i;
	int size = 0;
	char scanName[DIFX_MESSAGE_MAX_SCANNAME_LEN];
	char body[DIFX_MESSAGE_LENGTH];
	char message[DIFX_MESSAGE_LENGTH];

	char msn1[DIFX_MESSAGE_MARK6_MSN_LENGTH+2]; 
	char msn2[DIFX_MESSAGE_MARK6_MSN_LENGTH+2]; 
	char msn3[DIFX_MESSAGE_MARK6_MSN_LENGTH+2]; 
	char msn4[DIFX_MESSAGE_MARK6_MSN_LENGTH+2]; 
	
	// validate msn1 and covert to upper case
	if(strlen(mark6status->msn1) != 8)
        {
                strcpy(msn1, "none");
        }
        else
        {
                for(i = 0; i < 9; ++i)
                {
                        msn1[i] = toupper(mark6status->msn1[i]);
                }
        }
	// validate msn2 and covert to upper case
	if(strlen(mark6status->msn2) != 8)
        {
                strcpy(msn2, "none");
        }
        else
        {
                for(i = 0; i < 9; ++i)
                {
                        msn2[i] = toupper(mark6status->msn2[i]);
                }
        }
	// validate msn3 and covert to upper case
	if(strlen(mark6status->msn3) != 8)
        {
                strcpy(msn3, "none");
        }
        else
        {
                for(i = 0; i < 9; ++i)
                {
                        msn3[i] = toupper(mark6status->msn3[i]);
                }
        }
	// validate msn4 and covert to upper case
	if(strlen(mark6status->msn4) != 8)
        {
                strcpy(msn4, "none");
        }
        else
        {
                for(i = 0; i < 9; ++i)
                {
                        msn4[i] = toupper(mark6status->msn4[i]);
                }
        }

	size = snprintf(scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s", mark6status->scanName);

        if(size >= DIFX_MESSAGE_MAX_SCANNAME_LEN)
        {
                fprintf(stderr, "difxMessageSendMark6Status: scanName is too long (%d >= %d) and has been truncated.\n", size, DIFX_MESSAGE_MAX_SCANNAME_LEN);
        }

        size = snprintf(body, DIFX_MESSAGE_LENGTH,

                "<mark6Status>"
                  "%s"
                  "<slot1MSN>%s</slot1MSN>"
                  "<slot2MSN>%s</slot2MSN>"
                  "<slot3MSN>%s</slot3MSN>"
                  "<slot4MSN>%s</slot4MSN>"
                  "<slot1Group>%s</slot1Group>"
                  "<slot2Group>%s</slot2Group>"
                  "<slot3Group>%s</slot3Group>"
                  "<slot4Group>%s</slot4Group>"
                  "<slot1Disks>%d</slot1Disks>"
                  "<slot2Disks>%d</slot2Disks>"
                  "<slot3Disks>%d</slot3Disks>"
                  "<slot4Disks>%d</slot4Disks>"
                  "<slot1MissingDisks>%d</slot1MissingDisks>"
                  "<slot2MissingDisks>%d</slot2MissingDisks>"
                  "<slot3MissingDisks>%d</slot3MissingDisks>"
                  "<slot4MissingDisks>%d</slot4MissingDisks>"
                  "<state>%s</state>"
                  "<scanNumber>%d</scanNumber>"
                  "<scanName>%s</scanName>"
                  "<position>%lld</position>"
                  "<playRate>%5.3f</playRate>"
                  "<dataMJD>%9.7f</dataMJD>"
                "</mark6Status>",

                difxMessageInputFilenameTag,
                msn1,
                msn2,
                msn3,
                msn4,
		mark6status->group1,	
		mark6status->group2,	
		mark6status->group3,	
		mark6status->group4,	
		mark6status->bank1Disks,	
		mark6status->bank2Disks,	
		mark6status->bank3Disks,	
		mark6status->bank4Disks,	
		mark6status->bank1MissingDisks,	
		mark6status->bank2MissingDisks,	
		mark6status->bank3MissingDisks,	
		mark6status->bank4MissingDisks,	
                Mark6StateStrings[mark6status->state],
                mark6status->scanNumber,
                scanName,
                mark6status->position,
                mark6status->rate,
                mark6status->dataMJD);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendMark6Status: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_MARK6STATUS],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendMark6Status: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		return -1;
	}
	
	return difxMessageSend2(message, size);


	return(0);
}

int difxMessageSendMark5Status(const DifxMessageMk5Status *mk5status)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	char scanName[DIFX_MESSAGE_MAX_SCANNAME_LEN];
	char vsnA[10], vsnB[10];
	char bank;
	int size;

	if(strlen(mk5status->vsnA) != 8)
	{
		strcpy(vsnA, "none");
	}
	else
	{
		int i;

		for(i = 0; i < 9; ++i)
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
		int i;

		for(i = 0; i < 9; ++i)
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

	size = snprintf(scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN,
		"%s", mk5status->scanName);
	if(size >= DIFX_MESSAGE_MAX_SCANNAME_LEN)
	{
		fprintf(stderr, "difxMessageSendMark5Status: scanName too long (%d >= %d)\n", size, DIFX_MESSAGE_MAX_SCANNAME_LEN);

		return -1;
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
	
		"<mark5Status>"
		  "%s"
		  "<bankAVSN>%s</bankAVSN>"
		  "<bankBVSN>%s</bankBVSN>"
		  "<statusWord>0x%08x</statusWord>"
		  "<activeBank>%c</activeBank>"
		  "<state>%s</state>"
		  "<scanNumber>%d</scanNumber>"
		  "<scanName>%s</scanName>"
		  "<position>%lld</position>"
		  "<playRate>%5.3f</playRate>"
		  "<dataMJD>%9.7f</dataMJD>"
		"</mark5Status>",

		difxMessageInputFilenameTag,
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

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendMark5Status: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat, 
		DifxMessageTypeStrings[DIFX_MESSAGE_MARK5STATUS],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendMark5Status: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendMk5Version(const DifxMessageMk5Version *mk5version)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	char dbInfo[DIFX_MESSAGE_LENGTH] = "";
	int size;

	if(mk5version->DB_PCBVersion[0] != 0)
	{
		size = snprintf(dbInfo, DIFX_MESSAGE_LENGTH,

		  "<DaughterBoard>"
		    "<PCBVer>%s</PCBVer>"
		    "<PCBType>%s</PCBType>"
		    "<PCBSubType>%s</PCBSubType>"
		    "<FPGAConfig>%s</FPGAConfig>"
		    "<FPGAConfigVer>%s</FPGAConfigVer>"
		    "<SerialNum>%d</SerialNum>"
		    "<NumChannels>%d</NumChannels>"
		  "</DaughterBoard>",

		mk5version->DB_PCBVersion,
		mk5version->DB_PCBType,
		mk5version->DB_PCBSubType,
		mk5version->DB_FPGAConfig,
		mk5version->DB_FPGAConfigVersion,
		mk5version->DB_SerialNum,
		mk5version->DB_NumChannels);

		if(size >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendMk5Version: message dbinfo overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

			return -1;
		}
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
	
		"<mark5Version>"
		  "<ApiVer>%s</ApiVer>"
		  "<ApiDate>%s</ApiDate>"
		  "<FirmVer>%s</FirmVer>"
		  "<FirmDate>%s</FirmDate>"
		  "<MonVer>%s</MonVer>"
		  "<XbarVer>%s</XbarVer>"
		  "<AtaVer>%s</AtaVer>"
		  "<UAtaVer>%s</UAtaVer>"
		  "<DriverVer>%s</DriverVer>"
		  "<BoardType>%s</BoardType>"
		  "<SerialNum>%d</SerialNum>"
		  "%s"
		"</mark5Version>",

		mk5version->ApiVersion,
		mk5version->ApiDateCode,
		mk5version->FirmwareVersion,
		mk5version->FirmDateCode,
		mk5version->MonitorVersion,
		mk5version->XbarVersion,
		mk5version->AtaVersion,
		mk5version->UAtaVersion,
		mk5version->DriverVersion,
		mk5version->BoardType,
		mk5version->SerialNum,
		dbInfo);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendMk5Version: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_MARK5STATUS],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendMk5Version: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxStatus(enum DifxState state, const char *stateMessage, double visMJD, int numdatastreams, float *weight)
{
	char message[DIFX_MESSAGE_LENGTH];
	char weightstr[DIFX_MESSAGE_LENGTH];
	char stateMessageExpanded[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int i, n;
	int size;
	
	size = expandEntityReferences(stateMessageExpanded, stateMessage, DIFX_MESSAGE_LENGTH);

	if(size < 0)
	{
		fprintf(stderr, "difxMessageSendDifxStatus: message body overflow in entity replacement (>= %d)\n", DIFX_MESSAGE_LENGTH);

		return -1;
	}

	weightstr[0] = 0;
	n = 0;

	for(i = 0; i < numdatastreams; ++i)
	{
		if(weight[i] >= 0)
		{
			n += snprintf(weightstr+n, DIFX_MESSAGE_LENGTH-n,
				"<weight ant=\"%d\" wt=\"%5.3f\"/>", 
				i, weight[i]);
			if(n >= DIFX_MESSAGE_LENGTH)
			{
				fprintf(stderr, "difxMessageSendDifxStatus: message weightstr overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

				return -1;
			}
		}
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxStatus>"
		  "%s"
		  "<state>%s</state>"
		  "<message>%s</message>"
		  "<visibilityMJD>%9.7f</visibilityMJD>"
		  "%s"
		"</difxStatus>",

		difxMessageInputFilenameTag,
		DifxStateStrings[state],
		stateMessageExpanded,
		visMJD,
		weightstr);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxStatus: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_STATUS],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxStatus: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxStatus2(const char *jobName, enum DifxState state, const char *stateMessage)
{
	char message[DIFX_MESSAGE_LENGTH];
	char stateMessageExpanded[DIFX_MESSAGE_LENGTH];
	int size;

	size = expandEntityReferences(stateMessageExpanded, stateMessage, DIFX_MESSAGE_LENGTH);

	if(size < 0)
	{
		fprintf(stderr, "difxMessageSendDifxStatus2: message body overflow in entity replacement (>= %d)\n", DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,

		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
		"<difxMessage>"
		  "<header>"
		    "<from>%s</from>"
		    "<mpiProcessId>-1</mpiProcessId>"
		    "<identifier>%s</identifier>"
		    "<type>DifxStatusMessage</type>"
		  "</header>"
		  "<body>"
		    "<seqNumber>%d</seqNumber>"
		    "<difxStatus>"
		      "%s"
		      "<state>%s</state>"
		      "<message>%s</message>"
		      "<visibilityMJD>0</visibilityMJD>"
		    "</difxStatus>"
		  "</body>"
		"</difxMessage>\n",
		
		difxMessageHostname,
		jobName,
		difxMessageSequenceNumber++,
		difxMessageInputFilenameTag,
		DifxStateStrings[state],
		stateMessageExpanded);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxStatus2: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

int difxMessageSendDifxStatus3(enum DifxState state, const char *stateMessage,
	double visMJD, int numdatastreams, float *weight, double mjdStart, double mjdStop)
{
	char message[DIFX_MESSAGE_LENGTH];
	char weightstr[DIFX_MESSAGE_LENGTH];
	char stateMessageExpanded[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int i, n;
	int size;
	
	size = expandEntityReferences(stateMessageExpanded, stateMessage, DIFX_MESSAGE_LENGTH);

	if(size < 0)
	{
		fprintf(stderr, "difxMessageSendDifxStatus: message body overflow in entity replacement (>= %d)\n", DIFX_MESSAGE_LENGTH);

		return -1;
	}

	weightstr[0] = 0;
	n = 0;

	for(i = 0; i < numdatastreams; ++i)
	{
		if(weight[i] >= 0)
		{
			n += snprintf(weightstr+n, DIFX_MESSAGE_LENGTH-n,
				"<weight ant=\"%d\" wt=\"%5.3f\"/>", 
				i, weight[i]);
			if(n >= DIFX_MESSAGE_LENGTH)
			{
				fprintf(stderr, "difxMessageSendDifxStatus: message weightstr overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

				return -1;
			}
		}
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxStatus>"
		  "%s"
		  "<state>%s</state>"
		  "<message>%s</message>"
		  "<visibilityMJD>%9.7f</visibilityMJD>"
		  "<jobstartMJD>%9.7f</jobstartMJD>"
		  "<jobstopMJD>%9.7f</jobstopMJD>"
		  "%s"
		"</difxStatus>",

		difxMessageInputFilenameTag,
		DifxStateStrings[state],
		stateMessageExpanded,
		visMJD, mjdStart, mjdStop,
		weightstr);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxStatus: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_STATUS],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxStatus: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxInfo(const char *infoMessage)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	char infoMessageExpanded[DIFX_MESSAGE_LENGTH];
	int size;

	size = expandEntityReferences(infoMessageExpanded, infoMessage, DIFX_MESSAGE_LENGTH);

	if(size < 0)
	{
		fprintf(stderr, "difxMessageSendDifxInfo: message body overflow in entity replacement (>= %d)\n", DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxInfo>"
		  "<message>%s</message>"
		"</difxInfo>",

		infoMessageExpanded);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxInfo: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_INFO],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxInfo: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

int difxMessageSendDifxCommand(const char *command)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	char commandExpanded[DIFX_MESSAGE_LENGTH];
	int size;

	size = expandEntityReferences(commandExpanded, command, DIFX_MESSAGE_LENGTH);
	
	if(size < 0)
	{
		fprintf(stderr, "difxMessageSendDifxCommand: message body overflow in entity replacement (>= %d)\n", DIFX_MESSAGE_LENGTH);
		
		return -1;
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxCommand>"
		  "<command>%s</command>"
		"</difxCommand>",

		commandExpanded);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxCommand: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_COMMAND],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxCommand: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

/* mpiDestination: 
	>= 0 implies mpiId, 
	  -1 implies ALL, 
	  -2 implies all Datastrems, 
	  -3 implies all Cores
*/
int difxMessageSendDifxParameter(const char *name, const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxParameter>"
		  "<targetMpiId>%d</targetMpiId>"
		  "<name>%s</name>"
		  "<value>%s</value>"
		"</difxParameter>",

		mpiDestination,
		name,
		value);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_PARAMETER],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

int difxMessageSendDifxParameterTo(const char *name, const char *value, const char *to)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxParameter>"
		  "<targetMpiId>%d</targetMpiId>"
		  "<name>%s</name>"
		  "<value>%s</value>"
		"</difxParameter>",

		-10,
		name,
		value);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageToXMLFormat,
		to,
		DifxMessageTypeStrings[DIFX_MESSAGE_PARAMETER],
		difxMessageSequenceNumber++, 
		body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);

}

int difxMessageSendDifxParameter1(const char *name, int index1, const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxParameter>"
		  "<targetMpiId>%d</targetMpiId>"
		  "<name>%s</name>"
		  "<index1>%d</index1>"
		  "<value>%s</value>"
		"</difxParameter>",

		mpiDestination,
		name,
		index1,
		value);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter1: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_PARAMETER],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter1: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxParameter2(const char *name, int index1, int index2, const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxParameter>"
		  "<targetMpiId>%d</targetMpiId>"
		  "<name>%s</name>"
		  "<index1>%d</index1>"
		  "<index2>%d</index1>"
		  "<value>%s</value>"
		"</difxParameter>",

		mpiDestination,
		name,
		index1,
		index2,
		value);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter2: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_PARAMETER],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameter2: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxParameterGeneral(const char *name, int nIndex, const int *index, const char *value, int mpiDestination)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	char indices[DIFX_MESSAGE_LENGTH];
	int i;
	int p=0;
	int size;

	for(i = 0; i < nIndex; ++i)
	{
		p += snprintf(indices + p, DIFX_MESSAGE_LENGTH-p,
			"<index%d>%d</index%d>", i+1, index[i], i+1);
		if(p >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendDifxParameterGeneral: message indicies overflow (%d >= %d)\n", p, DIFX_MESSAGE_LENGTH);

			return -1;
		}
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,
		
		"<difxParameter>"
		  "<targetMpiId>%d</targetMpiId>"
		  "<name>%s</name>"
		  "%s"
		  "<value>%s</value>"
		"</difxParameter>",

		mpiDestination,
		name,
		indices,
		value);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameterGeneral: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_PARAMETER],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxParameterGeneral: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxDiagnosticBufferStatus(int threadid, int numelements, int startelement, int numactiveelements)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxDiagnostic>"
		  "<diagnosticType>%s</diagnosticType>"
		  "<threadId>%d</threadId>"
		  "<numBufElements>%d</numBufElements>"
		  "<startBufElement>%d</startBufElement>"
		  "<activeBufElements>%d</activeBufElements>"
		"</difxDiagnostic>",
		DifxDiagnosticStrings[DIFX_DIAGNOSTIC_BUFFERSTATUS],
		threadid,
		numelements,
		startelement,
		numactiveelements);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_DIAGNOSTIC],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxDiagnosticNumSubintsLost(int numsubintslost)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxDiagnostic>"
		  "<diagnosticType>%s</diagnosticType>"
		  "<numSubintsLost>%d</numSubintsLost>"
		"</difxDiagnostic>",
		DifxDiagnosticStrings[DIFX_DIAGNOSTIC_NUMSUBINTSLOST],
		numsubintslost);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_DIAGNOSTIC],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxDiagnosticInputDatarate(double bytespersec)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxDiagnostic>"
		  "<diagnosticType>%s</diagnosticType>"
		  "<bytespersec>%.3f</bytespersec>"
		"</difxDiagnostic>",
		DifxDiagnosticStrings[DIFX_DIAGNOSTIC_INPUTDATARATE],
		bytespersec);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_DIAGNOSTIC],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxDiagnosticDataConsumed(long long bytes)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxDiagnostic>"
		  "<diagnosticType>%s</diagnosticType>"
		  "<bytes>%lld</bytes>"
		"</difxDiagnostic>",
		DifxDiagnosticStrings[DIFX_DIAGNOSTIC_DATACONSUMED],
		bytes);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_DIAGNOSTIC],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxDiagnosticMemoryUsage(long long membytes)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxDiagnostic>"
		  "<diagnosticType>%s</diagnosticType>"
		  "<bytes>%lld</bytes>"
		"</difxDiagnostic>",
		DifxDiagnosticStrings[DIFX_DIAGNOSTIC_MEMORYUSAGE],
		membytes);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_DIAGNOSTIC],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxDiagnosticProcessingTime(int threadid, double durationMicrosec)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxDiagnostic>"
		  "<diagnosticType>%s</diagnosticType>"
		  "<threadId>%d</threadId>"
		  "<microsec>%f</microsec>"
		"</difxDiagnostic>",
		DifxDiagnosticStrings[DIFX_DIAGNOSTIC_PROCESSINGTIME],
		threadid,
		durationMicrosec);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_DIAGNOSTIC],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxDiagnostic: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
	
		return -1;
	}
	
	return difxMessageSend2(message, size);
}

int difxMessageSendDifxTransient(const DifxMessageTransient *transient)
{
	char message[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int size;

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxTransient>"
		  "<jobId>%s</jobId>"
		  "<startMJD>%10.8f</startMJD>"
		  "<stopMJD>%10.8f</stopMJD>"
		  "<priority>%f</priority>"
		  "<destDir>%s</destDir>"
		  "<comment>%s</comment>"
		  "<dm>%8.6f</dm>"
		"</difxTransient>",

		transient->jobId,
		transient->startMJD,
		transient->stopMJD,
		transient->priority,
		transient->destDir,
		transient->comment,
		transient->dm);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxTransient: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);
		
		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_TRANSIENT],
		difxMessageSequenceNumber++, body);
	
	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxTransient: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

int difxMessageSendDifxSmart(double mjdData, const char *vsn, int slot, int nValue, const int *ids, const long long *values)
{
	char message[DIFX_MESSAGE_LENGTH];
	char smartstr[DIFX_MESSAGE_LENGTH];
	char body[DIFX_MESSAGE_LENGTH];
	int i, n;
	int size;

	smartstr[0] = 0;
	n = 0;

	for(i = 0; i < nValue; ++i)
	{
		n += snprintf(smartstr+n, DIFX_MESSAGE_LENGTH-n,
			"<smart id=\"%d\" value=\"%lld\"/>",
			ids[i], values[i]);
		if(n >= DIFX_MESSAGE_LENGTH)
		{
			fprintf(stderr, "difxMessageSendDifxSmart: message smartstr overflow (%d >= %d)\n", n, DIFX_MESSAGE_LENGTH);
		
			return -1;
		}
	}

	size = snprintf(body, DIFX_MESSAGE_LENGTH,

		"<difxSmart>"
		  "<mjd>%9.7f</mjd>"
		  "<vsn>%s</vsn>"
		  "<slot>%d</slot>"
		  "%s"
		"</difxSmart>",

		mjdData,
		vsn,
		slot,
		smartstr);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxSmart: message body overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	size = snprintf(message, DIFX_MESSAGE_LENGTH,
		difxMessageXMLFormat,
		DifxMessageTypeStrings[DIFX_MESSAGE_SMART],
		difxMessageSequenceNumber++, body);

	if(size >= DIFX_MESSAGE_LENGTH)
	{
		fprintf(stderr, "difxMessageSendDifxSmart: message overflow (%d >= %d)\n", size, DIFX_MESSAGE_LENGTH);

		return -1;
	}

	return difxMessageSend2(message, size);
}

