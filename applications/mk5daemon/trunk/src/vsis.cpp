/***************************************************************************
 *   Copyright (C) 2011 by Walter Brisken                                  *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/


#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <unistd.h>
#include <difxmessage.h>
#include <ctype.h>
#include <sys/time.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include "vsis_commands.h"
#include "config.h"
#include "mk5daemon.h"

const int MaxConnections = 8;
const int MaxFields = 24;
const unsigned short VSIS_PORT = 2620;

typedef struct
{
	const char *name;
	int(*query)(Mk5Daemon *, int, char **, char *, int);
	int(*command)(Mk5Daemon *, int, char **, char *, int);
} Command;

int defaultCommand(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s = 7 : No such keyword;", fields[0]);
}

int noCommand(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
        return snprintf(response, maxResponseLength, "!%s = 7 : This query has no corresponding command;", fields[0]);
}

int defaultQuery(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s ? 7 : No such keyword;", fields[0]);
}

int noQuery(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s ? 7 : This command has no corresponding query;", fields[0]);
}

const Command commandSet[] =
{
	/* General commands */
	{ "DTS_id", 		DTS_id_Query, 		noCommand		},
	{ "packet",		packet_Query,		packet_Command		},
	{ "bank_set",		bank_set_Query,		bank_set_Command	},
	{ "SS_rev",		SS_rev_Query,		noCommand		},
	{ "OS_rev",		OS_rev_Query,		noCommand		},
	{ "fill_pattern",	fill_pattern_Query,	fill_pattern_Command	},
	{ "protect",		protect_Query,		protect_Command		},
	{ "error",		error_Query,		noCommand		},
	{ "disk_model",		disk_model_Query,	noCommand		},
	{ "disk_model_rev",	disk_model_rev_Query,	noCommand		},
	{ "disk_serial",	disk_serial_Query,	noCommand		},
	{ "disk_size",		disk_size_Query,	noCommand		},
	{ "dir_info",		dir_info_Query,		noCommand		},
	{ "pointers",		pointers_Query,		noCommand		},
	{ "personality",	personality_Query,	personality_Command	},
	{ "bank_info",		bank_info_Query,	noCommand		},
	{ "net_protocol",	net_protocol_Query,	net_protocol_Command	},
	{ "disk_state_mask",	disk_state_mask_Query,	disk_state_mask_Command	},
	{ "get_stats",	 	get_stats_Query,	noCommand		},
	{ "start_stats",	start_stats_Query,	start_stats_Command	},
	{ "mode",		mode_Query,		mode_Command		},
	{ "rtime",		rtime_Query,		noCommand		},
	{ "disk_state",		disk_state_Query,	disk_state_Command	},
	{ "scan_set",		scan_set_Query,		scan_set_Command	},
	{ "record",		record_Query,		record_Command		},

	{ "recover",		defaultQuery,		defaultCommand		},
	{ "reset",		noQuery,		defaultCommand		},

	{ "",			0,			0			}	/* list terminator */
};

static int setNonBlocking(int sock)
{
	int opts;

	opts = fcntl(sock, F_GETFL);
	if(opts < 0)
	{
		return -1;
	}

	opts = (opts | O_NONBLOCK);
	if(fcntl(sock, F_SETFL, opts) < 0)
	{
		return -1;
	}

	return 0;
}

static int parseVSIS(char *message, char **fields, int *nField, int *isQuery)
{
	int startgood = -1;
	int stopgood = -1;
	int done = 0;

	*nField = 0;
	*isQuery = 0;

	for(int i = 0; !done; i++)
	{
		if(message[i] == 0)
		{
			done = 1;
		}
		else if(message[i] <= ' ')	/* ignore all white space */
		{
			continue;
		}
		if(*isQuery && !done)
		{
			return -4;	/* only whitespace can follow ? */
		}
		if(message[i] == '?')
		{
			*isQuery = 1;
		}
		else if(message[i] == ':' || message[i] == '=' || message[i] == 0)
		{
			if(*nField >= MaxFields)
			{
				return -1;
			}
			if(message[i] == '=' && *nField > 0)
			{
				return -2;
			}
			if(message[i] == ':' && *nField == 0)
			{
				return -3;
			}
			if(startgood >= 0)
			{
				message[stopgood + 1] = 0;
				fields[*nField] = message+startgood;
			}
			else
			{
				message[i] = 0;
				fields[*nField] = message+i;
			}

			startgood = stopgood = -1;
			(*nField)++;
		}
		else
		{
			if(startgood < 0)
			{
				startgood = i;
			}
			stopgood = i;
		}

	}

	return 0;
}

/* This function can mangle the original string */
static int processVSIS(Mk5Daemon *D, char *message, char *response, int maxResponseLength)
{
	char *fields[MaxFields];
	int nField;
	int isQuery;
	int v;

	const char ParseError[][32] =
	{
		"No error",
		"Too many fields (:'s)",
		"Unexpected equal sign",
		"Unexpected colon",
		"Text after question mark"
	};

	if(maxResponseLength < 2)
	{
		response[0] = 0;

		return -1;
	}

	if(strlen(message) == 0)
	{
		return snprintf(response, maxResponseLength, ";");
	}

	v = parseVSIS(message, fields, &nField, &isQuery);

	if(v < 0)
	{
		return snprintf(response, maxResponseLength, "!%s : Parse error: %s;", (nField > 0 ? fields[0] : "?"), ParseError[-v]);
	}
	
	for(int i = 0; commandSet[i].name[0] != 0; i++)
	{
		if(strcmp(commandSet[i].name, fields[0]) == 0)
		{
			if(isQuery)
			{
				return commandSet[i].query(D, nField, fields, response, maxResponseLength);
			}
			else
			{
				return commandSet[i].command(D, nField, fields, response, maxResponseLength);
			}
		}
	}

	/* not in command set... */
	if(isQuery)
	{
		return defaultQuery(D, nField, fields, response, maxResponseLength);
	}
	else
	{
		return defaultCommand(D, nField, fields, response, maxResponseLength);
	}
}

static int handleVSIS(Mk5Daemon *D, int sock)
{
	int v;
	char message[DIFX_MESSAGE_LENGTH];
	char response[DIFX_MESSAGE_LENGTH];
	int r = 0;
	int start = 0;

	v = recv(sock, message, DIFX_MESSAGE_LENGTH-1, 0);
	if(v <= 0)
	{
		return -1;
	}
	message[v] = 0;

	for(int i = 0; message[i]; i++)
	{
		if(message[i] == ';' || message[i] == 0)
		{
			message[i] = 0;

			v = processVSIS(D, message+start, response+r, DIFX_MESSAGE_LENGTH-2-r);	/* leave room for \n */
			start = i+1;

			if(v < 0)
			{
				strcpy(response, "Response too long.;");

				return 0;
			}
			else
			{
				r += v;
			}
		}
	}

	r += snprintf(response+r, DIFX_MESSAGE_LENGTH-r, "\n");

	v = send(sock, response, r, 0);
	if(v < 1)
	{
		return -1;
	}

	return 0;
}

static void *serveVSIS(void *ptr)
{
	Mk5Daemon *D;
	int acceptSock;
	int clientSocks[MaxConnections];
	struct sockaddr_in server_address;
	struct timeval timeout;
	int n, v;
	int highSock;
	int readSocks;
	const int reuse_addr = 1;
	char message[DIFX_MESSAGE_LENGTH];
	fd_set socks;
	int recordFD;

	D = (Mk5Daemon *)ptr;

	for(int c = 0; c < MaxConnections; c++)
	{
		clientSocks[c] = 0;
	}

	acceptSock = socket(AF_INET, SOCK_STREAM, 0);

	if(acceptSock < 0)
	{
		Logger_logData(D->log, "Cannot create accept socket for VSI-S\n");

		pthread_exit(0);
	}

	setsockopt(acceptSock, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, sizeof(reuse_addr));

	if(setNonBlocking(acceptSock) < 0)
	{
		Logger_logData(D->log, "Cannot non-block accept socket for VSI-S\n");

		pthread_exit(0);
	}

	memset((char *)(&server_address), 0, sizeof(server_address));
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);
	server_address.sin_port = htons(VSIS_PORT);
	if(bind(acceptSock, (struct sockaddr *)(&server_address), sizeof(server_address)) < 0 )
	{
		Logger_logData(D->log, "Cannot bind accept socket for VSI-S\n");

		pthread_exit(0);
	}

	listen(acceptSock, MaxConnections);

	while(!D->dieNow)
	{
		timeout.tv_sec = 1;
		timeout.tv_usec = 0;
		FD_ZERO(&socks);
		FD_SET(acceptSock, &socks);
		highSock = acceptSock;
		for(int c = 0; c < MaxConnections; c++)
		{
			if(clientSocks[c] > 0)
			{
				FD_SET(clientSocks[c], &socks);
				if(clientSocks[c] > highSock)
				{
					highSock = clientSocks[c];
				}
			}
		}
		if(D->recordPipe)
		{
			recordFD = fileno(D->recordPipe);
			FD_SET(recordFD, &socks);
			if(recordFD > highSock)
			{
				highSock = recordFD;
			}
		}
		else
		{
			recordFD = -2;
		}

		readSocks = select(highSock+1, &socks, 0, 0, &timeout);

		if(readSocks < 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S select returned %d\n", readSocks);
			Logger_logData(D->log, message);

			pthread_exit(0);
		}

		if(readSocks == 0)
		{
			continue;
		}

		if(FD_ISSET(acceptSock, &socks))
		{
			int newSock = accept(acceptSock, 0, 0);
			if(newSock < 0)
			{
				snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S accept failure%d\n", newSock);
				Logger_logData(D->log, message);
			}
			else
			{
				/* find a slot for it */
				for(int c = 0; c < MaxConnections; c++)
				{
					if(clientSocks[c] == 0)
					{
						clientSocks[c] = newSock;
						newSock = -1;

						snprintf(message, DIFX_MESSAGE_LENGTH, "New VSI-S connection into slot %d of %d\n", c, MaxConnections);
						Logger_logData(D->log, message);

						break;
					}
				}
				if(newSock != -1)
				{
					Logger_logData(D->log, "No room for new VSI-S connection\n");

					close(newSock);
				}
			}
		}

		for(int c = 0; c < MaxConnections; c++)
		{
			if(FD_ISSET(clientSocks[c], &socks))
			{
				v = handleVSIS(D, clientSocks[c]);
				if(v < 0) /* connection closed? */
				{
					close(clientSocks[c]);
					clientSocks[c] = 0;
					snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S connection on slot %d closed\n", c);
					Logger_logData(D->log, message);
				}
			}
		}

		if(recordFD >= 0 && FD_ISSET(recordFD, &socks))
		{
			char *r = fgets(message, DIFX_MESSAGE_LENGTH-1, D->recordPipe);
			if(r)
			{
				char A[12][40];
				int n = sscanf(message, "%s%s%s%s%s%s%s%s%s%s%s%s", 
					A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7], A[8], A[9], A[10], A[11]);
				if(strcmp(A[0], "Pointer") == 0 && n > 2)
				{
					D->bytesUsed[D->activeBank] = atoll(A[1]);
					D->recordRate = atof(A[2]);
				}
				else if(strcmp(A[0], "Stats") == 0 && n >= 10)
				{
					int drive = atoi(A[1]);
					for(int b = 0; b < 8; b++)
					{
						D->driveStats[D->activeBank][drive][b].count += atoi(A[2+b]);
					}
				}
				else if(strcmp(A[0], "Error") == 0)
				{
					D->errorFlag[D->activeBank] = 1;
				}
			}
			if(feof(D->recordPipe))
			{
				pclose(D->recordPipe);
				D->recordPipe = 0;
				D->recordState = RECORD_OFF;

				clearModuleInfo(D, D->activeBank);
				Mk5Daemon_getModules(D);
			}
		}
	}

	if(D->recordPipe > 0)
	{
		pclose(D->recordPipe);
		D->recordPipe = 0;
		D->recordState = RECORD_OFF;
	}

	if(acceptSock > 0)
	{
		close(acceptSock);
		acceptSock = 0;
	}
	for(int c = 0; c < MaxConnections; c++)
	{
		if(clientSocks[c] > 0)
		{
			close(clientSocks[c]);
			clientSocks[c] = 0;
		}
	}

	pthread_exit(0);
}

void Mk5Daemon_startVSIS(Mk5Daemon *D)
{
	pthread_create(&D->vsisThread, 0, &serveVSIS, D);
}

void Mk5Daemon_stopVSIS(Mk5Daemon *D)
{
	pthread_join(D->vsisThread, 0);
}

