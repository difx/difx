/***************************************************************************
 *   Copyright (C) 2011-2012 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the Lesser GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   Lesser GNU General Public License for more details.                   *
 *                                                                         *
 *   You should have received a copy of the Lesser GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/vsis.cpp $
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
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include "vsis_commands.h"
#include "config.h"
#include "mk5daemon.h"

const int MaxFields = 24;
const unsigned short VSIS_PORT = 2620;

typedef struct
{
	const char *name;
	int(*query)(Mk5Daemon *, int, char **, char *, int);
	int(*command)(Mk5Daemon *, int, char **, char *, int);
} Command;

static int defaultCommand(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s = 7 : No such keyword;", fields[0]);
}

static int noCommand(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
        return snprintf(response, maxResponseLength, "!%s = 7 : This query has no corresponding command;", fields[0]);
}

static int defaultQuery(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s ? 7 : No such keyword;", fields[0]);
}

static int noQuery(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!%s ? 7 : This command has no corresponding query;", fields[0]);
}

const Command commandSet[] =
{
	/* Categorization based on 3 March 2011 Mark5C Command Set */

	/* General commands */
	{ "DTS_id", 		DTS_id_Query, 		noCommand		},
	{ "OS_rev",		OS_rev_Query,		noCommand		},
	{ "protect",		protect_Query,		protect_Command		},
	{ "recover",		noQuery,		recover_Command		},
	{ "reset",		noQuery,		reset_Command		},
	{ "SS_rev",		SS_rev_Query,		noCommand		},

	/* System setup and monitoring */
	{ "error",		error_Query,		noCommand		},
	{ "mode",		mode_Query,		mode_Command		},
	{ "status",		status_Query,		noCommand		},
	{ "personality",	personality_Query,	personality_Command	},
	{ "MAC_list",		MAC_list_Query,		MAC_list_Command	},

	/* Data checking */
	{ "scan_set",		scan_set_Query,		scan_set_Command	},

	/* Data transfer */
	{ "net_protocol",	net_protocol_Query,	net_protocol_Command	},
	{ "record",		record_Query,		record_Command		},
	{ "fill_pattern",	fill_pattern_Query,	fill_pattern_Command	},
	{ "packet",		packet_Query,		packet_Command		},
	{ "filter",		filter_Query,		noCommand		},

	/* Bank management */
	{ "bank_info",		bank_info_Query,	noCommand		},
	{ "bank_set",		bank_set_Query,		bank_set_Command	},

	/* Disk info */
	{ "dir_info",		dir_info_Query,		noCommand		},
	{ "disk_model",		disk_model_Query,	noCommand		},
	{ "disk_model_rev",	disk_model_rev_Query,	noCommand		},
	{ "disk_serial",	disk_serial_Query,	noCommand		},
	{ "disk_size",		disk_size_Query,	noCommand		},
	{ "disk_state",		disk_state_Query,	disk_state_Command	},
	{ "disk_state_mask",	disk_state_mask_Query,	disk_state_mask_Command	},
	{ "get_stats",	 	get_stats_Query,	noCommand		},
	{ "pointers",		pointers_Query,		noCommand		},
	{ "rtime",		rtime_Query,		noCommand		},
	{ "start_stats",	start_stats_Query,	start_stats_Command	},
	{ "VSN",		VSN_Query,		VSN_Command		},

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

	for(int i = 0; !done; ++i)
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
			++(*nField);
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
	int nField=0;
	int isQuery;
	int v = 0;

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
		v = snprintf(response, maxResponseLength, "!%s : Parse error: %s;", (nField > 0 ? fields[0] : "?"), ParseError[-v]);
	}
	else
	{
		for(int i = 0; commandSet[i].name[0] != 0; ++i)
		{
			if(strcasecmp(commandSet[i].name, fields[0]) == 0)
			{
				if(isQuery)
				{
					v = commandSet[i].query(D, nField, fields, response, maxResponseLength);
				}
				else
				{
					v = commandSet[i].command(D, nField, fields, response, maxResponseLength);
				}
			}
			if(v != 0)
			{
				break;
			}
		}

		/* not in command set... */
		if(v == 0)
		{
			if(isQuery)
			{
				v = defaultQuery(D, nField, fields, response, maxResponseLength);
			}
			else
			{
				v = defaultCommand(D, nField, fields, response, maxResponseLength);
			}
		}
	}

	if(nField >= 2 && strcasecmp(fields[0], "protect") == 0 && strcasecmp(fields[1], "off") == 0)
	{
		D->unprotected = 1;
	}
	else
	{
		D->unprotected = 0;
	}

	return v;
}

int handleVSIS(Mk5Daemon *D, int sock)
{
	int v;
	char message[DIFX_MESSAGE_LENGTH];
	char logMessage[DIFX_MESSAGE_LENGTH];
	char response[DIFX_MESSAGE_LENGTH];
	int r = 0;
	int start = 0;

	v = recv(sock, message, DIFX_MESSAGE_LENGTH-1, 0);
	if(v <= 0)
	{
		return -1;
	}
	message[v] = 0;

	snprintf(logMessage, DIFX_MESSAGE_LENGTH, "VSI-S received: %s\n", message);
	Logger_logData(D->log, logMessage);

	for(int i = 0; message[i]; ++i)
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

	snprintf(logMessage, DIFX_MESSAGE_LENGTH, "VSI-S responding: %s\n", response);
	Logger_logData(D->log, logMessage);

	r += snprintf(response+r, DIFX_MESSAGE_LENGTH-r, "\n");

	v = send(sock, response, r, 0);
	if(v < 1)
	{
		return -1;
	}

	return 0;
}

int Mk5Daemon_startVSIS(Mk5Daemon *D)
{
	const int reuse_addr = 1;
	struct addrinfo hints, *res;
	char portstr[6];
	int v;

	Mk5Daemon_stopVSIS(D);

	snprintf(portstr, 6, "%u", VSIS_PORT);

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;  // use IPv4 or IPv6, whichever
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;     // fill in my IP for me

	v = getaddrinfo(0, portstr, &hints, &res);
	if(v != 0)
	{
		Logger_logData(D->log, "getaddrinfo(0, VSIS_PORT) failed:");
		Logger_logData(D->log, gai_strerror(v));
		Logger_logData(D->log, "\n");

		return -1;
	}

	D->acceptSock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

	if(D->acceptSock < 0)
	{
		Logger_logData(D->log, "Cannot create accept socket for VSI-S\n");
		freeaddrinfo(res);

		return -1;
	}

	setsockopt(D->acceptSock, SOL_SOCKET, SO_REUSEADDR, &reuse_addr, sizeof(reuse_addr));

	if(setNonBlocking(D->acceptSock) < 0)
	{
		Logger_logData(D->log, "Cannot non-block accept socket for VSI-S\n");
		freeaddrinfo(res);

		return -1;
	}

	v = bind(D->acceptSock, res->ai_addr, res->ai_addrlen);
	freeaddrinfo(res);
	if(v < 0)
	{
		Logger_logData(D->log, "Cannot bind accept socket for VSI-S\n");

		return -1;
	}

	listen(D->acceptSock, MaxConnections);

	return 0;
}

void Mk5Daemon_stopVSIS(Mk5Daemon *D)
{
	if(D->acceptSock > 0)
	{
		close(D->acceptSock);
		D->acceptSock = 0;
	}
	for(int c = 0; c < MaxConnections; ++c)
	{
		if(D->clientSocks[c] > 0)
		{
			close(D->clientSocks[c]);
			D->clientSocks[c] = 0;
		}
	}
}

