/***************************************************************************
 *   Copyright (C) 2007-2012 by Walter Brisken                             *
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
#include <string.h>
#include <expat.h>
#include "../difxmessage.h"

/* allow space or comma separated list of nodes */
int addNodes(char nodes[][DIFX_MESSAGE_HOSTNAME_LENGTH], int maxNodes, int *n, const char *nodeList)
{
	int i;
	int s=-1;
	int nNew = 0;

	for(i = 0;; ++i)
	{
		char c = nodeList[i];

		if(*n >= maxNodes)
		{
			break;
		}

		if(c <= ' ' || c == ',' || 
		   c == '(' || c == ')' ||
		   c == '{' || c == '}' ||
		   c == '[' || c == ']')
		{
			if(s >= 0)
			{
				int l = i-s;
				if(l >= DIFX_MESSAGE_HOSTNAME_LENGTH)
				{
					fprintf(stderr, "Developer error: addNodes: DIFX_MESSAGE_HOSTNAME_LENGTH=%d is too small.  Wants to be %d\n", DIFX_MESSAGE_HOSTNAME_LENGTH, l+1);
					l = DIFX_MESSAGE_HOSTNAME_LENGTH-1;
				}
				memcpy(nodes[*n], nodeList+s, l);
				nodes[*n][l] = 0;
				++(*n);
				++nNew;
				s = -1;
			}
		}
		else if(s < 0)
		{	
			s = i;
		}

		if(!c)
		{
			break;
		}
	}

	return nNew;
}

static void XMLCALL startElement(void *userData, const char *name, 
	const char **atts)
{
	DifxMessageGeneric *G;
	int i, j, n;

	G = (DifxMessageGeneric *)userData;

	G->_xml_string[0] = 0;
	
	if(G->type == DIFX_MESSAGE_START)
	{
		DifxMessageStart *S;
		int nThread = 1;
		
		S = &G->body.start;
		for(i = 0; atts[i]; i+=2)
		{
			if(strcmp(atts[i], "threads") == 0)
			{
				nThread = atoi(atts[i+1]);
			}
		}
		if(strcmp(name, "manager") == 0)
		{
			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "node") == 0)
				{
					strncpy(S->headNode, atts[i+1], DIFX_MESSAGE_HOSTNAME_LENGTH-1);
					S->headNode[DIFX_MESSAGE_HOSTNAME_LENGTH-1] = 0;
				}
			}
		}
		else if(strcmp(name, "datastream") == 0)
		{
			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "nodes") == 0)
				{
					addNodes(S->datastreamNode, DIFX_MESSAGE_MAX_DATASTREAMS, &S->nDatastream, atts[i+1]);
				}
			}
		}
		else if(strcmp(name, "process") == 0)
		{
			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "nodes") == 0)
				{
					n = addNodes(S->processNode, DIFX_MESSAGE_MAX_CORES, &S->nProcess, atts[i+1]);
					for(j = S->nProcess-n; j < S->nProcess; j++)
					{
						S->nThread[j] = nThread;
					}
				}
			}
		}
	}
	else if(G->type == DIFX_MESSAGE_MACHINESDEFINITION)
	{
		DifxMessageMachinesDefinition *S;
		int nThread = 1;
		
		S = &G->body.machinesDefinition;
		for(i = 0; atts[i]; i+=2)
		{
			if(strcmp(atts[i], "threads") == 0)
			{
				nThread = atoi(atts[i+1]);
			}
		}
		if(strcmp(name, "manager") == 0)
		{
			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "node") == 0)
				{
					strncpy(S->headNode, atts[i+1], DIFX_MESSAGE_HOSTNAME_LENGTH-1);
					S->headNode[DIFX_MESSAGE_HOSTNAME_LENGTH-1] = 0;
				}
			}
		}
		else if(strcmp(name, "datastream") == 0)
		{
			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "nodes") == 0)
				{
					addNodes(S->datastreamNode, DIFX_MESSAGE_MAX_DATASTREAMS, &S->nDatastream, atts[i+1]);
				}
			}
		}
		else if(strcmp(name, "process") == 0)
		{
			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "nodes") == 0)
				{
					n = addNodes(S->processNode, DIFX_MESSAGE_MAX_CORES, &S->nProcess, atts[i+1]);
					for(j = S->nProcess-n; j < S->nProcess; j++)
					{
						S->nThread[j] = nThread;
					}
				}
			}
		}
	}
	else if(G->type == DIFX_MESSAGE_STATUS)
	{
		DifxMessageStatus *S;

		S = &G->body.status;
		if(strcmp(name, "weight") == 0)
		{
			int ds = -1;

			for(i = 0; atts[i]; i+=2)
			{
				if(strcmp(atts[i], "ant") == 0)
				{
					ds = atoi(atts[i+1]);
				}
				else if(strcmp(atts[i], "wt") == 0)
				{
					if(ds >= 0 && ds < DIFX_MESSAGE_MAX_DATASTREAMS)
					{
						if(ds > S->maxDS)
						{
							S->maxDS = ds;
						}
						S->weight[ds] = atof(atts[i+1]);
					}
				}
			}
		}
	}
	else if(G->type == DIFX_MESSAGE_SMART)
	{
		DifxMessageSmart *S;

		S = &G->body.smart;
		if(strcmp(name, "smart") == 0)
		{
			if(S->nValue < DIFX_MESSAGE_MAX_SMART_IDS)
			{
				for(i = 0; atts[i]; i+=2)
				{
					if(strcmp(atts[i], "id") == 0)
					{
						S->id[S->nValue] = atoi(atts[i+1]);
					}
					if(strcmp(atts[i], "value") == 0)
					{
						S->value[S->nValue] = atoll(atts[i+1]);
					}
				}
				S->nValue++;
			}
		}
	}
	else if(G->type == DIFX_MESSAGE_MARK5VERSION)
	{
		if(strcmp(name, "DaughterBoard") == 0)
		{
			G->_inDB = 1;
		}
	}

	G->_xml_level++;
	strcpy(G->_xml_element[G->_xml_level], name);
}

static void XMLCALL endElement(void *userData, const char *name)
{
	DifxMessageGeneric *G;
	const char *elem;
	const char *s;

	G = (DifxMessageGeneric *)userData;
	elem = G->_xml_element[G->_xml_level];
	s = G->_xml_string;

	if(G->_xml_string[0] != 0)
	{
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
					enum DifxMessageType t;

					for(t = 0; t < NUM_DIFX_MESSAGE_TYPES; t++)
					{
						if(!strcmp(DifxMessageTypeStrings[t],s))
						{
							G->type = t;
							if(G->type == DIFX_MESSAGE_STATUS)
							{
								int i;

								for(i = 0; i < DIFX_MESSAGE_MAX_DATASTREAMS; i++)
								{
									G->body.status.weight[i] = -1;
								}
								G->body.status.maxDS = -1;
							}
							else if(G->type == DIFX_MESSAGE_SMART)
							{
								int i;

								for(i = 0; i < DIFX_MESSAGE_MAX_SMART_IDS; i++)
								{
									G->body.smart.id[i] = 0;
									G->body.smart.value[i] = 0LL;
								}
								G->body.smart.nValue = 0;
							}
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
					else if(strcmp(elem, "nCore") == 0)
					{
						G->body.load.nCore = atoi(s);
					}
					break;
				case DIFX_MESSAGE_ALERT:
					if(strcmp(elem, "alertMessage") == 0)
					{
						strncpy(G->body.alert.message, s, DIFX_MESSAGE_LENGTH-1);
						G->body.alert.message[DIFX_MESSAGE_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "severity") == 0)
					{
						G->body.alert.severity = atoi(s);
					}
					break;
				case DIFX_MESSAGE_MARK5STATUS:
					if(strcmp(elem, "bankAVSN") == 0)
					{
						strncpy(G->body.mk5status.vsnA, s, DIFX_MESSAGE_MARK5_VSN_LENGTH);
						G->body.mk5status.vsnA[DIFX_MESSAGE_MARK5_VSN_LENGTH] = 0;
					}
					else if(strcmp(elem, "bankBVSN") == 0)
					{
						strncpy(G->body.mk5status.vsnB, s, DIFX_MESSAGE_MARK5_VSN_LENGTH);
						G->body.mk5status.vsnB[DIFX_MESSAGE_MARK5_VSN_LENGTH] = 0;
					}
					else if(strcmp(elem, "statusWord") == 0)
					{
						sscanf(s, "%16x", &G->body.mk5status.status);
					}
					else if(strcmp(elem, "activeBank") == 0)
					{
						G->body.mk5status.activeBank = s[0];
					}
					else if(strcmp(elem, "state") == 0)
					{
						enum Mk5State m;

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
						strncpy(G->body.mk5status.scanName, s, 63);
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
				case DIFX_MESSAGE_CONDITION:
				case DIFX_MESSAGE_DRIVE_STATS:
					if(strncmp(elem, "bin", 3) == 0)
					{
						int i;

						i = atoi(elem+3);
						if(i >= 0 && i < DIFX_MESSAGE_N_DRIVE_STATS_BINS)
						{
							G->body.driveStats.bin[i] = atoi(s);
						}
					}
					else if(strcmp(elem, "serialNumber") == 0)
					{
						strncpy(G->body.driveStats.serialNumber, s, DIFX_MESSAGE_DISC_SERIAL_LENGTH);
						G->body.driveStats.serialNumber[DIFX_MESSAGE_DISC_SERIAL_LENGTH] = 0;
					}
					else if(strcmp(elem, "modelNumber") == 0)
					{
						strncpy(G->body.driveStats.modelNumber, s, DIFX_MESSAGE_DISC_MODEL_LENGTH);
						G->body.driveStats.modelNumber[DIFX_MESSAGE_DISC_MODEL_LENGTH] = 0;
					}
					else if(strcmp(elem, "size") == 0)
					{
						G->body.driveStats.diskSize = atoi(s);
					}
					else if(strcmp(elem, "moduleVSN") == 0)
					{
						strncpy(G->body.driveStats.moduleVSN, s, DIFX_MESSAGE_MARK5_VSN_LENGTH);
						G->body.driveStats.moduleVSN[DIFX_MESSAGE_MARK5_VSN_LENGTH] = 0;
					}
					else if(strcmp(elem, "moduleSlot") == 0)
					{
						G->body.driveStats.moduleSlot = atoi(s);
					}
					else if(strcmp(elem, "startMJD") == 0)
					{
						G->body.driveStats.startMJD = atof(s);
					}
					else if(strcmp(elem, "stopMJD") == 0)
					{
						G->body.driveStats.stopMJD = atof(s);
					}
					else if(strcmp(elem, "type") == 0)
					{
						G->body.driveStats.type = stringToDriveStatsType(s);
					}
					else if(strcmp(elem, "startByte") == 0)
					{
						G->body.driveStats.startByte = atoll(s);
					}
					break;
				case DIFX_MESSAGE_MARK5VERSION:
					if(strcmp(elem, "DaughterBoard") == 0)
					{
						G->_inDB = 0;
					}
					else if(G->_inDB == 0)
					{
						if(strcmp(elem, "ApiVer") == 0)
						{
							strncpy(G->body.mk5version.ApiVersion, s, 7);
							G->body.mk5version.ApiVersion[7] = 0;
						}
						else if(strcmp(elem, "ApiDate") == 0)
						{
							strncpy(G->body.mk5version.ApiDateCode, s, 11);
							G->body.mk5version.ApiDateCode[11] = 0;
						}
						else if(strcmp(elem, "FirmVer") == 0)
						{
							strncpy(G->body.mk5version.FirmwareVersion, s, 7);
							G->body.mk5version.FirmwareVersion[7] = 0;
						}
						else if(strcmp(elem, "FirmDate") == 0)
						{
							strncpy(G->body.mk5version.FirmDateCode, s, 11);
							G->body.mk5version.FirmDateCode[11] = 0;
						}
						else if(strcmp(elem, "MonVer") == 0)
						{
							strncpy(G->body.mk5version.MonitorVersion, s, 7);
							G->body.mk5version.MonitorVersion[7] = 0;
						}
						else if(strcmp(elem, "XbarVer") == 0)
						{
							strncpy(G->body.mk5version.XbarVersion, s, 7);
							G->body.mk5version.XbarVersion[7] = 0;
						}
						else if(strcmp(elem, "AtaVer") == 0)
						{
							strncpy(G->body.mk5version.AtaVersion, s, 7);
							G->body.mk5version.AtaVersion[7] = 0;
						}
						else if(strcmp(elem, "UAtaVer") == 0)
						{
							strncpy(G->body.mk5version.UAtaVersion, s, 7);
							G->body.mk5version.UAtaVersion[7] = 0;
						}
						else if(strcmp(elem, "DriverVer") == 0)
						{
							strncpy(G->body.mk5version.DriverVersion, s, 7);
							G->body.mk5version.DriverVersion[7] = 0;
						}
						else if(strcmp(elem, "BoardType") == 0)
						{
							strncpy(G->body.mk5version.BoardType, s, 12);
							G->body.mk5version.BoardType[11] = 0;
						}
						else if(strcmp(elem, "SerialNum") == 0)
						{
							G->body.mk5version.SerialNum = atoi(s);
						}
					}
					else
					{
						if(strcmp(elem, "PCBType") == 0)
						{
							strncpy(G->body.mk5version.DB_PCBType, s, 12);
							G->body.mk5version.DB_PCBType[11] = 0;
						}
						else if(strcmp(elem, "PCBSubType") == 0)
						{
							strncpy(G->body.mk5version.DB_PCBSubType, s, 12);
							G->body.mk5version.DB_PCBSubType[11] = 0;
						}
						else if(strcmp(elem, "PCBVer") == 0)
						{
							strncpy(G->body.mk5version.DB_PCBVersion, s, 8);
							G->body.mk5version.DB_PCBVersion[7] = 0;
						}
						else if(strcmp(elem, "FPGAConfig") == 0)
						{
							strncpy(G->body.mk5version.DB_FPGAConfig, s, 12);
							G->body.mk5version.DB_FPGAConfig[11] = 0;
						}
						else if(strcmp(elem, "FPGAConfigVer") == 0)
						{
							strncpy(G->body.mk5version.DB_FPGAConfigVersion, s, 8);
							G->body.mk5version.DB_FPGAConfigVersion[7] = 0;
						}
						else if(strcmp(elem, "SerialNum") == 0)
						{
							G->body.mk5version.DB_SerialNum = atoi(s);
						}
						else if(strcmp(elem, "NumChannels") == 0)
						{
							G->body.mk5version.DB_NumChannels = atoi(s);
						}
					}
					break;
				case DIFX_MESSAGE_STATUS:
					if(strcmp(elem, "message") == 0)
					{
						strncpy(G->body.status.message, s, DIFX_MESSAGE_LENGTH-1);
						G->body.status.message[DIFX_MESSAGE_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "state") == 0)
					{
						enum DifxState d;

						for(d = 0; d < NUM_DIFX_STATES; d++)
						{
							if(strcmp(DifxStateStrings[d], s) == 0)
							{
								G->body.status.state = d;
							}
						}
					}
					else if(strcmp(elem, "visibilityMJD") == 0)
					{
						G->body.status.mjd = atof(s);
					}
					else if(strcmp(elem, "jobstartMJD") == 0)
					{
						G->body.status.jobStartMJD = atof(s);
					}
					else if(strcmp(elem, "jobstopMJD") == 0)
					{
						G->body.status.jobStopMJD = atof(s);
					}
					break;
				case DIFX_MESSAGE_INFO:
					if(strcmp(elem, "message") == 0)
					{
						strncpy(G->body.info.message, s, DIFX_MESSAGE_LENGTH-1);
						G->body.info.message[DIFX_MESSAGE_LENGTH-1] = 0;
					}
					break;
				case DIFX_MESSAGE_COMMAND:
					if(strcmp(elem, "command") == 0)
					{
						strncpy(G->body.command.command, s, DIFX_MESSAGE_LENGTH-1);
					}
					break;
				case DIFX_MESSAGE_PARAMETER:
					if(strcmp(elem, "targetMpiId") == 0)
					{
						G->body.param.targetMpiId = atoi(s);
					}
					else if(strncmp(elem, "index", 5) == 0)
					{
						int p;

						p = atoi(elem+5);
						if(p > 1 && p < DIFX_MESSAGE_MAX_INDEX)
						{
							int i;
							
							i = atoi(s);
							if(p > G->body.param.nIndex)
							{
								G->body.param.nIndex = p;
							}
							G->body.param.paramIndex[p-1] = i;
						}
					}
					else if(strcmp(elem, "name") == 0)
					{
						strncpy(G->body.param.paramName, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "value") == 0)
					{
						strncpy(G->body.param.paramValue, s, DIFX_MESSAGE_LENGTH-1);
					}
					break;
				case DIFX_MESSAGE_START:
					if(strcmp(elem, "input") == 0)
					{
						strncpy(G->body.start.inputFilename, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "env") == 0)
					{
						if(G->body.start.nEnv < DIFX_MESSAGE_MAX_ENV)
						{
							strncpy(G->body.start.envVar[G->body.start.nEnv], s, DIFX_MESSAGE_FILENAME_LENGTH-1);
							G->body.start.nEnv++;
						}
					}
					else if(strcmp(elem, "mpiWrapper") == 0)
					{
						strncpy(G->body.start.mpiWrapper, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.start.mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "mpiOptions") == 0)
					{
						strncpy(G->body.start.mpiOptions, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.start.mpiOptions[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "difxProgram") == 0)
					{
						strncpy(G->body.start.difxProgram, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.start.difxProgram[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "force") == 0)
					{
						if(s[0] == '0' || s[0] == 'n' || s[0] == 'N' || s[0] == 'f' || s[0] == 'F')
						{
							G->body.start.force = 0;
						}
						else
						{
							G->body.start.force = 1;
						}
					}
					else if(strcmp(elem, "difxVersion") == 0)
					{
						strncpy(G->body.start.difxVersion, s, DIFX_MESSAGE_VERSION_LENGTH-1);
						G->body.start.difxVersion[DIFX_MESSAGE_VERSION_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "restartSeconds") == 0)
					{
						G->body.start.restartSeconds = atoi(s);
					}
					else if(strcmp(elem, "function") == 0)
					{
						enum DifxStartFunction f;

						for(f = 0; f < NUM_DIFX_START_FUNCTION_TYPES; ++f) 
						{
							if(strcmp(DifxStartFunctionString[f], s) == 0)
							{
								G->body.start.function = f;
							}
						}
					}
					else if(strcmp(elem, "address") == 0 )
					{
						strncpy(G->body.start.address, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "port") == 0 )
					{
						G->body.start.port = atoi( s );
					}
					break;
				case DIFX_MESSAGE_MACHINESDEFINITION:
					if(strcmp(elem, "input") == 0)
					{
						strncpy(G->body.machinesDefinition.inputFilename, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "mpiWrapper") == 0)
					{
						strncpy(G->body.machinesDefinition.mpiWrapper, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.machinesDefinition.mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "mpiOptions") == 0)
					{
						strncpy(G->body.machinesDefinition.mpiOptions, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.machinesDefinition.mpiOptions[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "difxVersion") == 0)
					{
						strncpy(G->body.machinesDefinition.difxVersion, s, DIFX_MESSAGE_VERSION_LENGTH-1);
						G->body.machinesDefinition.difxVersion[DIFX_MESSAGE_VERSION_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "testProcessors") == 0 )
					{
					    if ( !strncmp( s, "true", 4 ) )
						    G->body.machinesDefinition.testProcessors = 1;
						else
						    G->body.machinesDefinition.testProcessors = 0;						
					}
					else if(strcmp(elem, "machinesFile") == 0)
					{
						strncpy(G->body.machinesDefinition.machinesFilename, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "threadsFile") == 0)
					{
						strncpy(G->body.machinesDefinition.threadsFilename, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "address") == 0 )
					{
						strncpy(G->body.machinesDefinition.address, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "port") == 0 )
					{
						G->body.machinesDefinition.port = atoi( s );
					}
					break;
				case DIFX_MESSAGE_STOP:
					if(strcmp(elem, "input") == 0)
					{
						strncpy( G->body.stop.inputFilename, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "difxVersion") == 0)
					{
						strncpy(G->body.stop.difxVersion, s, DIFX_MESSAGE_VERSION_LENGTH-1);
						G->body.stop.difxVersion[DIFX_MESSAGE_VERSION_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "difxProgram") == 0)
					{
						strncpy(G->body.stop.difxProgram, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.stop.difxProgram[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "mpiWrapper") == 0)
					{
						strncpy(G->body.stop.mpiWrapper, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.stop.mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					break;
				case DIFX_MESSAGE_DIAGNOSTIC:
					if(strcmp(elem, "diagnosticType") == 0)
					{
						enum DifxDiagnosticType t;

						for(t = 0; t < NUM_DIFX_DIAGNOSTIC_TYPES; t++)
						{
							if(strcmp(DifxDiagnosticStrings[t],s) == 0)
							{
								G->body.diagnostic.diagnosticType = t;
							}
						}
					}
					else if(strcmp(elem, "threadId") == 0)
					{
						G->body.diagnostic.threadid = atoi(s);
					}
					else if(strcmp(elem, "numBufElements") == 0)
					{
						G->body.diagnostic.bufferstatus[0] = atoi(s);
					}
					else if(strcmp(elem, "startBufElement") == 0)
					{
						G->body.diagnostic.bufferstatus[1] = atoi(s);
					}
					else if(strcmp(elem, "activeBufElements") == 0)
					{
						G->body.diagnostic.bufferstatus[2] = atoi(s);
					}
					else if(strcmp(elem, "numSubintsLost") == 0)
					{
						G->body.diagnostic.counter = atoi(s);
					}
					else if(strcmp(elem, "bytespersec") == 0)
					{
						G->body.diagnostic.rateMbps = ((double)atol(s))/1e6;
					}
					else if(strcmp(elem, "bytes") == 0)
					{
						G->body.diagnostic.bytes = atol(s);
					}
					else if(strcmp(elem, "microsec") == 0)
					{
						G->body.diagnostic.microsec = atof(s);
					}
					break;
				case DIFX_MESSAGE_FILETRANSFER:
					if(strcmp(elem, "origin") == 0 )
					{
						strncpy(G->body.fileTransfer.origin, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "destination") == 0 )
					{
						strncpy(G->body.fileTransfer.destination, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "dataNode") == 0 )
					{
						strncpy(G->body.fileTransfer.dataNode, s, DIFX_MESSAGE_HOSTNAME_LENGTH-1);
					}
					else if(strcmp(elem, "direction") == 0 )
					{
						strncpy(G->body.fileTransfer.direction, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "address") == 0 )
					{
						strncpy(G->body.fileTransfer.address, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "port") == 0 )
					{
						G->body.fileTransfer.port = atoi( s );
					}
					break;
				case DIFX_MESSAGE_FILEOPERATION:
					if(strcmp(elem, "path") == 0 )
					{
						strncpy(G->body.fileOperation.path, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "operation") == 0 )
					{
						strncpy(G->body.fileOperation.operation, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "arg") == 0 )
					{
						strncpy(G->body.fileOperation.arg, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "dataNode") == 0 )
					{
						strncpy(G->body.fileOperation.dataNode, s, DIFX_MESSAGE_HOSTNAME_LENGTH-1);
					}
					else if(strcmp(elem, "address") == 0 )
					{
						strncpy(G->body.fileOperation.address, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "port") == 0 )
					{
						G->body.fileOperation.port = atoi( s );
					}
					break;
				case DIFX_MESSAGE_GETDIRECTORY:
					if(strcmp(elem, "mark5") == 0 )
					{
						strncpy(G->body.getDirectory.mark5, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "difxVersion") == 0)
					{
						strncpy(G->body.getDirectory.difxVersion, s, DIFX_MESSAGE_VERSION_LENGTH-1);
						G->body.getDirectory.difxVersion[DIFX_MESSAGE_VERSION_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "mpiWrapper") == 0)
					{
						strncpy(G->body.getDirectory.mpiWrapper, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
						G->body.getDirectory.mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH-1] = 0;
					}
					else if(strcmp(elem, "vsn") == 0 )
					{
						strncpy(G->body.getDirectory.vsn, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "address") == 0 )
					{
						strncpy(G->body.getDirectory.address, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "port") == 0 )
					{
						G->body.getDirectory.port = atoi( s );
					}
					else if(strcmp(elem, "generateNew") == 0 )
					{
						G->body.getDirectory.generateNew = atoi( s );
					}
					break;
				case DIFX_MESSAGE_VEX2DIFXRUN:
					if(strcmp(elem, "user") == 0 )
					{
						strncpy(G->body.vex2DifxRun.user, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "node") == 0 )
					{
						strncpy(G->body.vex2DifxRun.headNode, s, DIFX_MESSAGE_HOSTNAME_LENGTH-1);
					}
					else if(strcmp(elem, "difxVersion") == 0 )
					{
						strncpy(G->body.vex2DifxRun.difxVersion, s, DIFX_MESSAGE_VERSION_LENGTH-1);
					}
					else if(strcmp(elem, "passPath") == 0 )
					{
						strncpy(G->body.vex2DifxRun.passPath, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "file") == 0 )
					{
						strncpy(G->body.vex2DifxRun.v2dFile, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "address") == 0 )
					{
						strncpy(G->body.vex2DifxRun.address, s, DIFX_MESSAGE_PARAM_LENGTH-1);
					}
					else if(strcmp(elem, "port") == 0 )
					{
						G->body.vex2DifxRun.port = atoi( s );
					}
					else if(strcmp(elem, "calcifOnly") == 0 )
					{
						G->body.vex2DifxRun.calcifOnly = atoi( s );
					}
					break;
				case DIFX_MESSAGE_TRANSIENT:
					if(strcmp(elem, "jobId") == 0)
					{
						strncpy(G->body.transient.jobId, s, DIFX_MESSAGE_IDENTIFIER_LENGTH-1);
					}
					else if(strcmp(elem, "startMJD") == 0)
					{
						G->body.transient.startMJD = atof(s);
					}
					else if(strcmp(elem, "stopMJD") == 0)
					{
						G->body.transient.stopMJD = atof(s);
					}
					else if(strcmp(elem, "priority") == 0)
					{
						G->body.transient.priority = atof(s);
					}
					else if(strcmp(elem, "destDir") == 0)
					{
						strncpy(G->body.transient.destDir, s, DIFX_MESSAGE_FILENAME_LENGTH-1);
					}
					else if(strcmp(elem, "comment") == 0)
					{
						strncpy(G->body.transient.comment, s, DIFX_MESSAGE_COMMENT_LENGTH-1);
					}
					else if(strcmp(elem, "dm") == 0)
					{
						G->body.transient.dm = atof(s);
					}
				case DIFX_MESSAGE_SMART:
					if(strcmp(elem, "mjd") == 0)
					{
						G->body.smart.mjdData = atof(s);
					}
					else if(strcmp(elem, "vsn") == 0)
					{
						strncpy(G->body.smart.moduleVSN, s, DIFX_MESSAGE_MARK5_VSN_LENGTH);
						G->body.smart.moduleVSN[DIFX_MESSAGE_MARK5_VSN_LENGTH] = 0;
					}
					else if(strcmp(elem, "slot") == 0)
					{
						G->body.smart.moduleSlot = atoi(s);
					}
				default:
					break;
				}
			}
		}
	}

	G->_xml_level--;
}

static void XMLCALL charHandler(void *userData, const XML_Char *str, int len)
{
	DifxMessageGeneric *G;
	int l;

	G = (DifxMessageGeneric *)userData;

	l = strlen(G->_xml_string);

	if(len + l > 1023)
	{
		len = 1023 - l;
		if(len <= 0)
		{
			return;
		}
	}
	strncpy(G->_xml_string+l, str, len);
	G->_xml_string[len+l] = 0;
}

int difxMessageParse(DifxMessageGeneric *G, const char *message)
{
	XML_Parser parser;
	
	memset(G, 0, sizeof(DifxMessageGeneric));
	G->_xml_level = -1;
	G->type = DIFX_MESSAGE_UNKNOWN;
	parser = XML_ParserCreate(0);
	XML_ParserReset(parser, 0);
	XML_SetElementHandler(parser, startElement, endElement);
	XML_SetCharacterDataHandler(parser, charHandler);
	XML_SetUserData(parser, G);
	XML_Parse(parser, message, strlen(message), 0);
	XML_ParserFree(parser);
	
	/* promote condition type ti disk stat type */
	if(G->type == DIFX_MESSAGE_CONDITION)
	{
		fprintf(stderr, "Note: condition message received.  This is being replaced with a disk stat message.  The sender should be upgraded to use the new type when convenient.\n");
		G->type = DIFX_MESSAGE_DRIVE_STATS;
	}

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
		printf("    network Receive Rate = %d B/s\n", G->body.load.netRXRate);
		printf("    network Transmit Rate = %d B/s\n", G->body.load.netTXRate);
		printf("    nCore = %d\n", G->body.load.nCore);
		break;
	case DIFX_MESSAGE_ALERT:
		printf("    severity = %d\n", G->body.alert.severity);
		printf("    message = %s\n", G->body.alert.message);
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
	case DIFX_MESSAGE_MARK5VERSION:
		printf("    Api Version = %s  Date = %s\n", 
			G->body.mk5version.ApiVersion,
			G->body.mk5version.ApiDateCode);
		printf("    Firmware Version = %s  Date = %s\n", 
			G->body.mk5version.FirmwareVersion,
			G->body.mk5version.FirmDateCode);
		printf("    Monitor Version = %s\n", 
			G->body.mk5version.MonitorVersion);
		printf("    Xbar Version = %s\n", 
			G->body.mk5version.XbarVersion);
		printf("    Ata Version = %s\n", 
			G->body.mk5version.AtaVersion);
		printf("    UAta Version = %s\n", 
			G->body.mk5version.UAtaVersion);
		printf("    Driver Version = %s\n", 
			G->body.mk5version.DriverVersion);
		printf("    Board Type = %s  Serial Num. = %d\n",
			G->body.mk5version.BoardType,
			G->body.mk5version.SerialNum);
		if(G->body.mk5version.DB_PCBVersion[0] != 0)
		{
			printf("    Daughter Board:  Serial number = %d  num channels = %d\n",
				G->body.mk5version.DB_SerialNum,
				G->body.mk5version.DB_NumChannels);
			printf("    Daughter Board:  Type = %s  Subtype = %s  Version = %s\n",
				G->body.mk5version.DB_PCBType,
				G->body.mk5version.DB_PCBSubType,
				G->body.mk5version.DB_PCBVersion);
			printf("    Daughter Board:  Configuration = %s  Version = %s\n",
				G->body.mk5version.DB_FPGAConfig,
				G->body.mk5version.DB_FPGAConfigVersion);
		}
		break;
	case DIFX_MESSAGE_CONDITION:	/* Should not be exercised; falls through to disk_stat */
	case DIFX_MESSAGE_DRIVE_STATS:
		printf("    serial number = %s\n", G->body.driveStats.serialNumber);
		printf("    model number = %s\n", G->body.driveStats.modelNumber);
		printf("    disk size = %d GB\n", G->body.driveStats.diskSize);
		printf("    disk stat type = %s\n", DriveStatsTypeStrings[G->body.driveStats.type]);
		printf("    startbyte = %Ld\n", G->body.driveStats.startByte);
		printf("    module VSN = %s\n", G->body.driveStats.moduleVSN);
		printf("    drive slot = %d\n", G->body.driveStats.moduleSlot);
		printf("    MJD = %7.5f to %7.5f\n", G->body.driveStats.startMJD, G->body.driveStats.stopMJD);
		printf("    stats =");
		for(i = 0; i < DIFX_MESSAGE_N_DRIVE_STATS_BINS; i++)
		{
			printf(" %d", G->body.driveStats.bin[i]);
		}
		printf("\n");
		break;
	case DIFX_MESSAGE_STATUS:
		printf("    state = %s %d\n", DifxStateStrings[G->body.status.state], G->body.status.state);
		printf("    message = %s\n", G->body.status.message);
		break;
	case DIFX_MESSAGE_INFO:
		printf("    message = %s\n", G->body.info.message);
		break;
	case DIFX_MESSAGE_COMMAND:
		printf("    command = %s\n", G->body.command.command);
		break;
	case DIFX_MESSAGE_PARAMETER:
		printf("    targetMpiId = %d\n", G->body.param.targetMpiId);
		printf("    name = %s", G->body.param.paramName);
		for(i = 0; i < G->body.param.nIndex; i++)
		{
			printf("[%d]", G->body.param.paramIndex[i]);
		}
		printf("\n");
		printf("    value = %s\n", G->body.param.paramValue);
		break;
	case DIFX_MESSAGE_DIAGNOSTIC:
		printf("    threadid = %d\n", G->body.diagnostic.threadid);
		printf("    bytes = %lld\n", G->body.diagnostic.bytes);
		printf("    counter = %lld\n", G->body.diagnostic.counter);
		printf("    microsec = %.3f\n", G->body.diagnostic.microsec);
		printf("    rateMbps = %.3f\n", G->body.diagnostic.rateMbps);
		printf("    numBufElements = %d\n", G->body.diagnostic.bufferstatus[0]);
		printf("    startBufElement = %d\n", G->body.diagnostic.bufferstatus[1]);
		printf("    activeBufElements = %d\n", G->body.diagnostic.bufferstatus[2]);
		break;
	case DIFX_MESSAGE_START:
		printf("    MPI wrapper = %s\n", G->body.start.mpiWrapper);
		printf("    MPI options = %s\n", G->body.start.mpiOptions);
		printf("    program = %s\n", G->body.start.difxProgram);
		printf("    input file = %s\n", G->body.start.inputFilename);
		printf("    nEnv = %d\n", G->body.start.nEnv);
		for(i = 0; i < G->body.start.nEnv; i++)
		{
			printf("      %s\n", G->body.start.envVar[i]);
		}
		printf("    headNode = %s\n", G->body.start.headNode);
		printf("    restartSeconds = %f\n", G->body.start.restartSeconds);
		printf("    nDatastream = %d\n", G->body.start.nDatastream);
		for(i = 0; i < G->body.start.nDatastream; i++)
		{
			printf("      %s\n", G->body.start.datastreamNode[i]);
		}
		printf("    nDatastream = %d\n", G->body.start.nDatastream);
		for(i = 0; i < G->body.start.nProcess; i++)
		{
			printf("      %s %d\n", G->body.start.processNode[i], G->body.start.nThread[i]);
		}
		printf("    force = %d\n", G->body.start.force);
		printf("    difxVersion = %s\n", G->body.start.difxVersion);
		break;
	case DIFX_MESSAGE_STOP:
		printf("    MPI wrapper = %s\n", G->body.stop.mpiWrapper);
		printf("    program = %s\n", G->body.stop.difxProgram);
		printf("    difxVersion = %s\n", G->body.stop.difxVersion);
		printf("    input file = %s\n", G->body.start.inputFilename);
		break;
	case DIFX_MESSAGE_FILETRANSFER:
		printf("    origin = %s\n", G->body.fileTransfer.origin);
		printf("    destination = %s\n", G->body.fileTransfer.destination);
		printf("    direction = %s\n", G->body.fileTransfer.direction);
		printf("    address = %s\n", G->body.fileTransfer.address);
		printf("    port = %d\n", G->body.fileTransfer.port);
		break;
	case DIFX_MESSAGE_FILEOPERATION:
		printf("    path = %s\n", G->body.fileOperation.path);
		printf("    operation = %s\n", G->body.fileOperation.operation);
		printf("    arg = %s\n", G->body.fileOperation.arg);
		printf("    dataNode = %s\n", G->body.fileOperation.dataNode);
		break;
	case DIFX_MESSAGE_TRANSIENT:
		printf("    jobId = %s\n", G->body.transient.jobId);
		printf("    startMJD = %14.8f\n", G->body.transient.startMJD);
		printf("    stopMJD = %14.8f\n", G->body.transient.stopMJD);
		printf("    priority = %f\n", G->body.transient.priority);
		printf("    destDir = %s\n", G->body.transient.destDir);
		printf("    comment = %s\n", G->body.transient.comment);
		printf("    DM = %8.6f\n", G->body.transient.dm);
		break;
	case DIFX_MESSAGE_SMART:
		printf("    data MJD = %12.6f", G->body.smart.mjdData);
		printf("    module VSN = %s\n", G->body.smart.moduleVSN);
		printf("    module slot = %d\n", G->body.smart.moduleSlot);
		for(i = 0; i < G->body.smart.nValue; i++)
		{
			printf("      SMART id = %d  value = %Ld\n", G->body.smart.id[i], G->body.smart.value[i]);
		}
		break;
	default:
		break;
	}
	if(G->_xml_error_count > 0)
	{
		printf("  xml errors = %d\n", G->_xml_error_count);
	}
}
