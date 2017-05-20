/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <string>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <sys/stat.h>
#include <expat.h>
#include <difxmessage.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <net/if.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "mk5daemon.h"
#include "../config.h"
#include "logger.h"
#include "proc.h"
#ifdef HAS_MARK6META
#include <mark6meta/Mark6.h>
#include <mark6meta/Mark6Meta.h>
#endif
#ifdef HAVE_XLRAPI_H
#include "watchdog.h"
#endif

using namespace std;

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

const int DefaultDifxMonitorPort = 50200;
const char DefaultDifxGroup[] = "224.2.2.1";
const char DefaultLogPath[] = "/tmp";
const char DefaultDifxUser[] = "difx";

const int maxIdle = 25;		/* if streamstor card is idle this long */
				/* set current process to NONE */

const int defaultPacketSize = 0;	/* 0 means don't care */
const int defaultPayloadOffset = 36;	/* works for RDBE DDC and PFB */
const int defaultDataFrameOffset = 0;
const int defaultPSNMode = 0;
const int defaultPSNOffset = 0;
const int defaultMACFilterControl = 1;

// For Mark5 module performance
const int defaultStatsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };

volatile int *signalDie = 0;

struct sigaction old_sigint_action;
struct sigaction old_sigterm_action;


const char recordStateStrings[][10] =
{
	"off",
	"on",
	"halted",
	"throttled",
	"overflow",
	"waiting",
	"hung",

	"error"	/* last entry */
};

const char netProtocolStrings[][10] =
{
	"UDP",
	"L2",

	"Error"	/* last entry */
};

const char MacListCommandStrings[][10] =
{
	"flush",
	"add",
	"delete",
	"enable",
	"disable",

	"illegal" /* last entry */
};

static void usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n",
		program, version, author);
	fprintf(stderr, "A program to control Mark5A, handle Mark5 allocation "
		"manage VSNs, and\n");
	fprintf(stderr, "log all of the above.  Root permissions required.\n");
	fprintf(stderr, "\nUsage : %s [options]\n\n", pgm);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             Print this help message\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --headnode\n");
	fprintf(stderr, "  -H             Give head node capabilities\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q             Don't multicast any status\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --log-path <path>\n");
	fprintf(stderr, "  -l <path>      Put log files in <path>\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --user <user>\n");
	fprintf(stderr, "  -u <user>      Use <user> when executing remote commands (default is 'difx')\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --hostname <name>\n");
	fprintf(stderr, "  -N <name>      Set hostname to <name> in messages (default is canonical hostname)\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --nosu \n");
	fprintf(stderr, "  -n             Don't use su when executing su commands\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --isMk5 \n");
	fprintf(stderr, "  -m             Force mk5daemon on this host to act as Mark5 regardless of hostname\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --isMk6 \n");
	fprintf(stderr, "  -6             Force mk5daemon on this host to act as Mark6 regardless of hostname\n"); 
	fprintf(stderr, "\n");
	fprintf(stderr, "  --embedded\n");
	fprintf(stderr, "  -e             Configure for running within a pipe and with messages to stdout\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "Note: This program responds to the following "
			"environment variables:\n");
	fprintf(stderr, "  DIFX_LOG_DIR : change log path from default [%s]\n",
		DefaultLogPath);
	fprintf(stderr, "  DIFX_MESSAGE_GROUP : change multicast group "
		"from default [%s]\n", DefaultDifxGroup);
	fprintf(stderr, "  DIFX_MESSAGE_PORT : change multicast port "
		"from default [%d]\n", DefaultDifxMonitorPort);
	fprintf(stderr, "  STREAMSTOR_BIB_PATH : change streamstor firmware "
		"path from default\n");
	fprintf(stderr, "  DIFX_USER_ID : change user account for executing remote commands from default [%s]\n", DefaultDifxUser);
	fprintf(stderr, "\n");
	fprintf(stderr, "IPv6 compliance: VSIS TCP port: likely\n");
	fprintf(stderr, "                 DiFX multicast: yes, vis difxmessage\n");
}


/* MAC address stuff (should move to new file) */

bool operator ==(const MAC &mac1, const MAC &mac2)
{
	return mac1.address == mac2.address;
}
bool operator <(const MAC &mac1, const MAC &mac2) 
{
	return mac1.address < mac2.address;
}

int MAC::parse(const char *str)
{
	unsigned int n, p;
	long long int a, b, c, d, e, f;

	n = sscanf(str, "%Lx.%Lx.%Lx.%Lx.%Lx.%Lx%n", &a, &b, &c, &d, &e, &f, &p);

	if(strlen(str) != p)
	{
		return -1;
	}

	if(n != 6)
	{
		return -1;
	}

	if(a > 255 || b > 255 || c > 255 || d > 255 || e > 255 || f > 255)
	{
		return -1;
	}

	address = (a << 40LL) | (b << 32LL) | (c << 24LL) | (d << 16LL) | (e << 8LL) | (f << 0LL);

	return 0;
}

int MAC::toString(char *str) const
{
	int n;

	n = sprintf(str, "%02Lx.%02Lx.%02Lx.%02Lx.%02Lx.%02Lx", 
		(address >> 40) & 0xFF,
		(address >> 32) & 0xFF,
		(address >> 24) & 0xFF,
		(address >> 16) & 0xFF,
		(address >>  8) & 0xFF,
		(address >>  0) & 0xFF);
	
	return n;
}

int checkRunning(const char *hostname)
{
	const int MaxMessageLength = 256;
	int status;
	int sock;
	struct addrinfo hints;
	struct addrinfo *servinfo;
	const char drsQuery[] = "DTS_id?;";
	int txBytes, rxBytes;
	char message[MaxMessageLength];
	struct timeval tv;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;		/* don't care IPv4 or IPv6 */
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;

	status = getaddrinfo(hostname, "2620", &hints, &servinfo);
	if(status != 0)
	{
		return -1;
	}

	sock = socket(servinfo->ai_family, servinfo->ai_socktype, servinfo->ai_protocol);
	if(sock <= 0)
	{
		freeaddrinfo(servinfo);

		return -2;
	}

	tv.tv_sec = 0;
	tv.tv_usec = 100000;

	setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
	setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));

	status = connect(sock, servinfo->ai_addr, servinfo->ai_addrlen); 
	if(status < 0)
	{
		close(sock);
		freeaddrinfo(servinfo);

		return -3;
	}

	txBytes = send(sock, drsQuery, strlen(drsQuery) , 0);
	if(txBytes != strlen(drsQuery))
	{
		close(sock);
		freeaddrinfo(servinfo);

		return -4;
	}

	rxBytes = recv(sock, message, MaxMessageLength-1, 0);
	if(rxBytes <= 0)
	{
		close(sock);
		freeaddrinfo(servinfo);

		return -5;
	}

	close(sock);
	freeaddrinfo(servinfo);

	return 0;
}

void getLocalAddresses(std::list<std::string> *addrList)
{
	struct ifaddrs *localAddrs, *addr;
	int rv;

	addrList->clear();

	rv = getifaddrs(&localAddrs);
	if(rv != 0)
	{
		printf("Cannot get local addresses with getifaddrs\n");

		return;
	}
	for(addr = localAddrs; addr != 0; addr = addr->ifa_next)
	{
		if(addr->ifa_addr && addr->ifa_flags & IFF_UP)
		{
			char addrString[INET_ADDRSTRLEN];

			if(addr->ifa_addr->sa_family == AF_INET)
			{
				struct sockaddr_in *s4;

				s4 = (struct sockaddr_in *)(addr->ifa_addr);
				if(inet_ntop(addr->ifa_addr->sa_family, (void *)&(s4->sin_addr), addrString, INET_ADDRSTRLEN))
				{
					if(strcmp(addrString, "127.0.0.1") != 0)
					{
						addrList->push_back(std::string(addrString));
					}
				}
			}
			else if(addr->ifa_addr->sa_family == AF_INET6)
			{
				struct sockaddr_in6 *s6;

				s6 = (struct sockaddr_in6 *)(addr->ifa_addr);
				if(inet_ntop(addr->ifa_addr->sa_family, (void *)&(s6->sin6_addr), addrString, INET_ADDRSTRLEN))
				{
					if(strcmp(addrString, "::1") != 0)
					{
						addrList->push_back(std::string(addrString));
					}
				}
			}
		}
	}

	freeifaddrs(localAddrs);
}

Mk5Daemon *newMk5Daemon(Options options)
{
	Mk5Daemon *D;
	int n;

	D = (Mk5Daemon *)calloc(1, sizeof(Mk5Daemon));
	
	D->log = newLogger(options.logPath);
	D->process = PROCESS_NONE;
	D->loadMonInterval = 10;	/* seconds */
	D->swapMonInterval = 30;	/* seconds */
	D->macList = new std::map<MAC,bool>;
	D->errors = new std::list<std::string>;
	if (procGetCores(&D->load.nCore) == -2)
        {
                procGetCoresFromCpuInfo(&D->load.nCore);
        }
	// hostname 
	gethostname(D->hostName, MK5DAEMON_HOSTNAME_LENGTH);
	D->hostName[MK5DAEMON_HOSTNAME_LENGTH-1] = 0;

	// isMk5
	D->isMk5 = strcasestr(D->hostName, "mark5") == 0 ? 0 : 1;
	if(options.isMk5)
	{
		D->isMk5 = 1;
	}
	// isMk6
	D->isMk6 = strcasestr(D->hostName, "mark6") == 0 ? 0 : 1;
	if(options.isMk6)
	{
		D->isMk6 = 1;
	}
	// userid
	n = snprintf(D->userID, MAX_USERID_LENGTH, "%s", options.userID);
	if(n >= MAX_USERID_LENGTH)
	{
		fprintf(stderr, "Error: userID = %s is too long.  Won't use it!\n", options.userID);

		snprintf(D->userID, MAX_USERID_LENGTH, "%s", DefaultDifxUser);
	}
	else
	{
		snprintf(D->userID, MAX_USERID_LENGTH, "%s", options.userID);
	}
	D->isHeadNode = options.isHeadNode;
	D->isEmbedded = options.isEmbedded;

	printf("isMk5 = %d hostname = %s\n", D->isMk5, D->hostName);
	printf("isMk6 = %d hostname = %s\n", D->isMk6, D->hostName);
	printf("spawned process userid = %s\n", D->userID);
	signalDie = &D->dieNow;
	Mk5Daemon_startMonitor(D);
	Mk5Daemon_startVSIS(D);
	pthread_mutex_init(&D->processLock, 0);
	difxMessageSendDifxInfo("mk5daemon starting");
	D->activeBank = -1;

	D->packetSize = defaultPacketSize;
	D->dataFrameOffset = defaultDataFrameOffset;
	D->payloadOffset = defaultPayloadOffset;
	D->psnOffset = defaultPSNOffset;
	D->psnMode = defaultPSNMode;
	D->macFilterControl = defaultMACFilterControl;
	D->fillPattern = MARK5_FILL_PATTERN;
	D->netProtocol = NET_PROTOCOL_UDP;
	strcpy(D->dataSource, "ext");
	D->bitstreamMask = 0xFFFFFFFF;
	D->decimationRatio = 1;

	D->recordRate = 2048;	/* just a reference value to start with */

#ifdef HAVE_XLRAPI_H
	for(int b = 0; b < XLR_MAXBINS; ++b)
	{
		D->driveStatsConfig[b].range = defaultStatsRange[b];
	}
#endif
	for(int b = 0; b < N_BANK; ++b)
	{
		D->driveFail[b] = -1;	/* no failure */
	}

	const char *dmsMask = getenv("DEFAULT_DMS_MASK");
	if(dmsMask)
	{
		D->diskStateMask = atoi(dmsMask);
	}

	/* find all local IP addresses */
	D->ipAddresses = new std::list<std::string>;
	getLocalAddresses(D->ipAddresses);

	return D;
}

int Mk5Daemon_system(const Mk5Daemon *D, const char *command, int verbose)
{
	int v;
	char message[DIFX_MESSAGE_LENGTH];

	if(verbose)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
	
		Logger_logData(D->log, message);
	}

	v = system(command);

	// For some reason system returns -1 even if successful
	// possibly because of the fork()ed, threaded, ... situation?
#if 0
	if(v == -1)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"system() failed running: %s\n", command);

		Logger_logData(D->log, message);
	}
#endif

	return v;
}

FILE* Mk5Daemon_popen(const Mk5Daemon *D, const char *command, int verbose)
{
	char message[DIFX_MESSAGE_LENGTH];

	if(verbose)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
		Logger_logData(D->log, message);
	}

	return popen(command, "r");
}

void deleteMk5Daemon(Mk5Daemon *D)
{
	difxMessageSendDifxInfo("mk5daemon stopping");
	signalDie = 0;
	if(D)
	{
		D->dieNow = 1;
		Mk5Daemon_stopMonitor(D);
		Mk5Daemon_stopVSIS(D);
		deleteLogger(D->log);
		delete D->macList;
		delete D->errors;
		delete D->ipAddresses;
		free(D);
	}
}

#warning FIXME: move this to a proc query
int running(const char *name)
{
	const int MaxLineLength = 512;
	FILE *pin;
	int n;
	char command[MAX_COMMAND_SIZE];
	char line[MaxLineLength];

	snprintf(command, MAX_COMMAND_SIZE,  "ps -e | grep %s", name);

	pin = popen(command, "r");
	if(!pin)
	{
		printf("ERROR Cannot run ps\n");

		return 1;
	}

	n = fread(line, 1, MaxLineLength, pin);
	line[MaxLineLength-1] = 0;
	pclose(pin);

	if(n > 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

int checkStreamstor(Mk5Daemon *D, time_t t)
{
	int v, busy;
	char message[DIFX_MESSAGE_LENGTH];

	if(!D->isMk5)
	{
		return 0;
	}

	v = procGetStreamstor(&busy);
	if(v < 0 && D->noDriver == 0)
	{
		D->noDriver = t;
		Logger_logData(D->log, "windrvr6 driver went missing!\n");
		difxMessageSendDifxAlert("windrvr6 disappeared!", DIFX_ALERT_LEVEL_ERROR);
	}
	else
	{
		if(D->noDriver != 0)
		{
			D->noDriver = 0;
			Logger_logData(D->log, "windrvr6 driver came back!\n");
			difxMessageSendDifxAlert("windrvr6 reappeared!", DIFX_ALERT_LEVEL_WARNING);
		}
	}

	if(D->noDriver)
	{
		return -1;
	}

	if(busy > 2)
	{
		D->idleCount = 0;
		pthread_mutex_lock(&D->processLock);
		if(D->process == PROCESS_NONE)
		{
			Logger_logData(D->log, "Some process took control of windrvr6\n");
			difxMessageSendDifxAlert("Some process took control of windrvr6", DIFX_ALERT_LEVEL_INFO);
			D->process = PROCESS_UNKNOWN;
		}
		pthread_mutex_unlock(&D->processLock);
	}
	else
	{
		++D->idleCount;
	}

	if(D->idleCount > maxIdle && D->process != PROCESS_NONE && !D->processDone)
	{
		pthread_mutex_lock(&D->processLock);
		Logger_logData(D->log, "Some process gave back control of windrvr6\n");
		difxMessageSendDifxAlert("Some process gave back control of windrvr6", DIFX_ALERT_LEVEL_INFO);

#warning FIXME: watch for a timeout here

		D->process = PROCESS_NONE;
		pthread_mutex_unlock(&D->processLock);
	}

	if(t - D->lastMpifxcorrUpdate > 20 && D->process == PROCESS_DATASTREAM)
	{
		pthread_mutex_lock(&D->processLock);
		if(!running("mpifxcorr"))
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Detected premature end of mpifxcorr at %s\n", ctime(&t));
			Logger_logData(D->log, message);

			D->process = PROCESS_NONE;
		}
		else
		{
			/* note that it is still alive */
			D->lastMpifxcorrUpdate = t;
		}
		pthread_mutex_unlock(&D->processLock);
	}

	return 0;
}

void sigintHandler(int j)
{
	if(signalDie)
	{
		*signalDie = 1;
	}
	sigaction(SIGINT, &old_sigint_action, 0);
}

void sigtermHandler(int j)
{
	if(signalDie)
	{
		*signalDie = 1;
	}
	sigaction(SIGTERM, &old_sigint_action, 0);
}

void Mk5Daemon_addVSIError(Mk5Daemon *D, const char *errorMessage)
{
	char message[DIFX_MESSAGE_LENGTH];
	int i;

	/* First replace special characters */
	for(i = 0; i < DIFX_MESSAGE_LENGTH-1 && errorMessage[i]; ++i)
	{
		if(errorMessage[i] < ' ')
		{
			message[i] = ' ';
		}
		else if(errorMessage[i] == ':')
		{
			message[i] = '|';
		}
		else if(errorMessage[i] == ';' || errorMessage[i] == '=' || errorMessage[i] == '?' || errorMessage[i] == '!')
		{
			message[i] = '~';
		}
		else
		{
			message[i] = errorMessage[i];
		}
	}
	message[i] = 0;

	std::string errString(message);

	/* Don't add a duplicate message to list */
	if(find(D->errors->begin(), D->errors->end(), message) == D->errors->end())
	{
		D->errors->push_back(errString);
	}
}

void Mk5Daemon_delVSIError(Mk5Daemon *D, const char *errorMessage)
{
	char message[DIFX_MESSAGE_LENGTH];
	int i;

	/* First replace special characters */
	for(i = 0; i < DIFX_MESSAGE_LENGTH-1 && errorMessage[i]; ++i)
	{
		if(errorMessage[i] < ' ')
		{
			message[i] = ' ';
		}
		else if(errorMessage[i] == ':')
		{
			message[i] = '|';
		}
		else if(errorMessage[i] == ';' || errorMessage[i] == '=' || errorMessage[i] == '?' || errorMessage[i] == '!')
		{
			message[i] = '~';
		}
		else
		{
			message[i] = errorMessage[i];
		}
	}
	message[i] = 0;

	std::string errString(message);

	D->errors->remove(errString);
}

int Mk5Daemon_popVSIError(Mk5Daemon *D, char *errorMessage, int maxLength)
{
	int n;

	n = D->errors->size();

	if(n > 0)
	{
		snprintf(errorMessage, maxLength, "%s", D->errors->front().c_str());
		D->errors->pop_front();
	}

	return n;
}

#ifdef HAVE_XLRAPI_H
void handleRecordMessage(Mk5Daemon *D, time_t t)
{
	char message[DIFX_MESSAGE_LENGTH];
	char *r = fgets(message, DIFX_MESSAGE_LENGTH-1, D->recordPipe);

	D->recordLastMessage = t;

	if(r)
	{
		char errorMessage[DIFX_MESSAGE_LENGTH];
		char A[15][40];
		int n = sscanf(message, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s", 
			A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7], A[8], A[9], A[10], A[11], A[12], A[13], A[14]);
		if(n > 0)
		{
			if(strcmp(A[0], "Pointer") == 0 && n > 2)
			{
				D->bytesUsed[D->activeBank] = atoll(A[1]);
				D->recordRate = atof(A[2]);
			}
			else if(strcmp(A[0], "Dir") == 0 && n > 6)
			{
				char logMessage[DIFX_MESSAGE_LENGTH];
				snprintf(logMessage, DIFX_MESSAGE_LENGTH, "Directory entry: %s\n", message+4);
				Logger_logData(D->log, logMessage);
			}
			else if(strcmp(A[0], "Filter") == 0 && n > 5)
			{
				D->totalPackets = atoi(A[1]);
				D->ethernetPackets = atoi(A[2]);
				D->addressRejects = atoi(A[3]);
				D->lengthRejects = atoi(A[4]);
				D->fscRejects = atoi(A[5]);
			}
			else if(strcmp(A[0], "Stats") == 0 && n >= 10)
			{
				int drive = atoi(A[1]);
				for(int b = 0; b < 8; ++b)
				{
					D->driveStats[D->activeBank][drive][b].count += atoi(A[2+b]);
				}
				if(n >= 11)
				{
					D->driveStatsReplaced[D->activeBank][drive] = atoi(A[10]);
				}
				else
				{
					D->driveStatsReplaced[D->activeBank][drive] = 0;
				}
			}
			else if(strcmp(A[0], "Error") == 0)
			{
				snprintf(errorMessage, DIFX_MESSAGE_LENGTH, "Record %s", message);
				Mk5Daemon_addVSIError(D, errorMessage);
			}
			else if(strcmp(A[0], "SystemError") == 0)
			{
				snprintf(errorMessage, DIFX_MESSAGE_LENGTH, "Streamstor System Error %s during record", A[1]);
				Mk5Daemon_addVSIError(D, errorMessage);
			}
			else if(strncmp(A[0], "Warning", 7) == 0)
			{
				Logger_logData(D->log, message);
			}
			else if(n > 2 && strcmp(A[0], "Watchdog") == 0 && strcmp(A[1], "caught") == 0)
			{
				Logger_logData(D->log, message);
			}
			else if(strcmp(A[0], "Halted") == 0)
			{
				D->recordState = RECORD_HALTED;
			}
			else if(strcmp(A[0], "Overflow") == 0)
			{
				D->recordState = RECORD_OVERFLOW;
			}
			else if(strcmp(A[0], "Drive") == 0 && n == 3)
			{
				if(strcmp(A[2], "failed") == 0)
				{
					D->driveFail[D->activeBank] = atoi(A[1]);
					snprintf(errorMessage, DIFX_MESSAGE_LENGTH, "Drive %s failed during record", A[1]);
					Mk5Daemon_addVSIError(D, errorMessage);
				}
			}
			else if(strcmp(A[0], "Bank") == 0 && n == 13)
			{
				/* new module inserted */
				int bank = A[1][0]-'A';
				if(bank >= 0 && bank < N_BANK && bank != D->activeBank)
				{
					clearModuleInfo(D, bank);
					clearMk5Stats(D, bank);
					if(strcmp(A[2], "none") == 0)
					{
						D->vsns[bank][0] = 0;
					}
					else
					{
						strncpy(D->vsns[bank], A[2], 8);
						D->vsns[bank][8] = 0;
					}
				}
			}
		}
	}
	if(feof(D->recordPipe))
	{
		pclose(D->recordPipe);
		D->recordPipe = 0;
		D->recordT0 = D->recordLastMessage = 0;
		D->recordState = RECORD_OFF;

		clearModuleInfo(D, D->activeBank);
		Mk5Daemon_getModules(D);
	}
}
#endif

void handleDifxMessage(Mk5Daemon *D, int noSu)
{
	char message[DIFX_MESSAGE_LENGTH];
	int n;
	DifxMessageGeneric G;
	char from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];

	n = difxMessageReceive(D->difxSock, message, DIFX_MESSAGE_LENGTH-1, from);
	
	if(n > 0)
	{
		int v;

		message[n] = 0;
		v = difxMessageParse(&G, message);
		if(v == 0)
		{
			switch(G.type)
			{
                        case DIFX_MESSAGE_MARK6STATUS:
				handleMark6Status(D, &G);
				break;
			case DIFX_MESSAGE_MARK5STATUS:
				handleMk5Status(D, &G);
				break;
			case DIFX_MESSAGE_COMMAND:
				handleCommand(D, &G);
				break;
			case DIFX_MESSAGE_START:
				Mk5Daemon_startMpifxcorr(D, &G, noSu);
				break;
			case DIFX_MESSAGE_STOP:
				Mk5Daemon_stopMpifxcorr_USNO(D, &G);
				break;
			case DIFX_MESSAGE_DRIVE_STATS:
				handleDriveStats(D, &G);
				break;
			case DIFX_MESSAGE_FILETRANSFER:
				Mk5Daemon_fileTransfer(D, &G);
				break;
			case DIFX_MESSAGE_FILEOPERATION:
				Mk5Daemon_fileOperation(D, &G);
				break;
			case DIFX_MESSAGE_VEX2DIFXRUN:
				Mk5Daemon_vex2DifxRun(D, &G);
				break;
			case DIFX_MESSAGE_VSIS:
				controlVSIS(D, &G);
				break;
			default:
				break;
			}
		}
		else
		{
			char message2[DIFX_MESSAGE_LENGTH+100];

			snprintf(message2, DIFX_MESSAGE_LENGTH+100, "Error: Unparsable message received: %s\n", message);
			Logger_logData(D->log, message2);
		}
	}
}

void handleAcceptMessage(Mk5Daemon *D)
{
	char message[DIFX_MESSAGE_LENGTH];
	int newSock = accept(D->acceptSock, 0, 0);
	
	if(newSock < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S accept failure: return=%d D->acceptSock=%d errno=%d error=%s\n", newSock, D->acceptSock, errno, strerror(errno));
		Logger_logData(D->log, message);
		sleep(2);
	}
	else
	{
		/* find a slot for it */
		for(int c = 0; c < MaxConnections; ++c)
		{
			if(D->clientSocks[c] == 0)
			{
				D->clientSocks[c] = newSock;
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

#ifdef HAVE_XLRAPI_H
int Mk5Daemon_stopRecord(Mk5Daemon *D)
{
	int recordFD;
	time_t t0, t;
	fd_set socks;
	struct timeval timeout;

	t0 = t = time(0);

	for(;;)
	{
		if(D->recordPipe == 0)
		{
			break;
		}
		if(t - t0 >= 3)
		{
			Mk5Daemon_addVSIError(D, "Recording hung during stop request");
			D->recordState = RECORD_HUNG;

			return -1;	/* Error: recording didn't stop in 3 seconds */
		}
		recordFD = fileno(D->recordPipe);
		FD_ZERO(&socks);
		FD_SET(recordFD, &socks);
		timeout.tv_sec = 1;
		timeout.tv_usec = 0;
		select(recordFD + 1, &socks, 0, 0, &timeout);
		t = time(0);
		if(FD_ISSET(recordFD, &socks))
		{
			handleRecordMessage(D, t);
		}
	}

	return 0;
}
#endif

bool Mk5Daemon_addrMatches(const Mk5Daemon *D, const char *addrString)
{
	std::list<std::string>::const_iterator it;

	for(it = D->ipAddresses->begin(); it != D->ipAddresses->end(); ++it)
	{
		if(*it == addrString)
		{
			return true;
		}
	}

	return false;
}

/**
 * parse the command line
 **/
int parseCmd (int argc, char **argv, Options &options)
{
	for(int i = 1; i < argc; ++i)
	{
		if(strcmp(argv[i], "-H") == 0 || strcmp(argv[i], "--headnode") == 0)
		{
			options.isHeadNode = 1;
		}
		else if(strcmp(argv[i], "-e") == 0 || strcmp(argv[i], "--embedded") == 0)
		{
			options.isEmbedded = 1;
			options.logPath = "";
		}
		else if(strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
		{
			usage(argv[0]);
			return 0;
		}
		else if(strcmp(argv[i], "-q") == 0 || strcmp(argv[i], "--quiet") == 0)
		{
			setenv("DIFX_MESSAGE_PORT", "-1", 1);
		}
		else if(strcmp(argv[i], "-n") == 0 || strcmp(argv[i], "--nosu") == 0)
		{
			options.noSu = 1;
                }
		else if(strcmp(argv[i], "-m") == 0 || strcmp(argv[i], "--isMk5") == 0)
		{
			options.isMk5 = 1;
                }
		else if(strcmp(argv[i], "-6") == 0 || strcmp(argv[i], "--isMk6") == 0)
		{
			options.isMk6 = 1;
                }
		else if(i < argc-1)
		{
			if(strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--log-path") == 0)
			{
				++i;
			/*	if(strlen(argv[i]) >= MAX_FILENAME_SIZE)
				{
					fprintf(stderr, "Error: logpath is longer than %d chars.\n", MAX_FILENAME_SIZE);

					return -1;
				}*/
				options.logPath = argv[i];
			}
			else if(strcmp(argv[i], "-u") == 0 || strcmp(argv[i], "--user") == 0)
			{
				++i;
/*
				if(strlen(argv[i]) >= MAX_USERID_LENGTH)
				{
					fprintf(stderr, "Error: userID is longer than %d chars.\n", MAX_USERID_LENGTH);

					return -1;
				}*/
				options.userID = argv[i];
			}
			else if(strcmp(argv[i], "-N") == 0 || strcmp(argv[i], "--hostname") == 0)
			{
				++i;
				options.providedHostname = argv[i];
			}
			else
			{
				usage(argv[0]);

				return -1;
			}
		}
		else
		{
			usage(argv[0]);

			return -1;
		}
	}

	return 0;
}

int main(int argc, char **argv)
{
    Options options;

    // FIXME: fixed length string arrays should be revisited
    Mk5Daemon *D;
    time_t t, lastTime;
    char message[DIFX_MESSAGE_LENGTH];
    char str[16];
    const char *p, *u;
    double mjd;
    fd_set socks;
    struct timeval timeout;
    int readSocks;
    int highSock;
    int v;
    int halfSwapMonInterval;
    int halfLoadMonInterval;
    int pid;
    int status;
    struct sigaction new_sigint_action;
    struct sigaction new_sigterm_action;

#ifdef HAVE_XLRAPI_H
    time_t firstTime;
    int ok = 0;	/* FIXME: combine with D->ready? */
    int justStarted = 1;
    int recordFD;
    int isMk5 = 1;

    if(XLRDeviceFind() < 1)
    {
            isMk5 = 0;
    }
#else
    int isMk5 = 0;
#endif

    try
    {
	p = getenv("DIFX_LOG_PATH");
	if(p)
	{
		options.logPath = p;
	}
	else
	{
		options.logPath = DefaultLogPath;
	}

	u = getenv("DIFX_USER_ID");
	if(u)
	{
		options.userID = u;
	}
	else
	{
                options.userID = DefaultDifxUser;
	}

	sprintf(str, "%d", DefaultDifxMonitorPort);
	setenv("DIFX_MESSAGE_PORT", str, 0);
	setenv("DIFX_MESSAGE_GROUP", DefaultDifxGroup, 0);
	setenv("STREAMSTOR_BIB_PATH", "/usr/share/streamstor/bib", 0);

	// parse the command line options
	if (parseCmd(argc, argv, options) == -1)
		return(EXIT_FAILURE);
	if (options.validate() < 0)
		return(EXIT_FAILURE);

	v = checkRunning("localhost");
	if(v == 0)
	{
		fprintf(stderr, "Error: another instance of %s is already running\n", program);

		exit(EXIT_FAILURE);
	}
	else if(v > -3)
	{
		fprintf(stderr, "Error: some network problem in resolving localhost: %d\n", v);

		exit(EXIT_FAILURE);
	}
	else if(v < -3)
	{
		fprintf(stderr, "Error: some other instance of %s may be running; if so, it is unhappy: %d\n", program, v);

		exit(EXIT_FAILURE);
	}

	if(!options.isEmbedded)
	{
		pid = fork();
		if(pid < 0)
		{
			fprintf(stderr, "!!! %s ver. %s spawn failure !!!\n", program, version);

			return EXIT_FAILURE;
		}
		if(pid > 0)
		{
			printf("*** %s ver. %s spawned ***\n", program, version);

			return EXIT_SUCCESS;
		}
	}
	else
	{
		printf("*** %s ver. %s starting ***\n", program, version);
	}

	umask(02);

	difxMessageInitFull(-1, program, options.providedHostname);
	difxMessageSendDifxAlert("mk5daemon starting", DIFX_ALERT_LEVEL_INFO);

	difxMessagePrint();

	D = newMk5Daemon(options);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Starting %s ver. %s\n", program, version);
	Logger_logData(D->log, message);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Number of CPU cores found = %d\n", D->load.nCore);
	Logger_logData(D->log, message);


	new_sigint_action.sa_handler = sigintHandler;
	sigemptyset(&new_sigint_action.sa_mask);
	new_sigint_action.sa_flags = 0;
	sigaction(SIGINT, &new_sigint_action, &old_sigint_action);
	
	new_sigterm_action.sa_handler = sigtermHandler;
	sigemptyset(&new_sigterm_action.sa_mask);
	new_sigterm_action.sa_flags = 0;
	sigaction(SIGTERM, &new_sigterm_action, &old_sigterm_action);

	lastTime = time(0);

	halfSwapMonInterval = D->swapMonInterval/2 + 3;
	halfLoadMonInterval = D->loadMonInterval/2;

#ifdef HAVE_XLRAPI_H
	firstTime = lastTime;

	v = initWatchdog();
	if(v < 0)
	{
		Logger_logData(D->log, "Error starting Streamstor Watchdog");
	}
	else
	{
		setWatchdogTimeout(60);
		if(options.isEmbedded)
		{
			setWatchdogStream(stdout);
		}
	}
#endif

	ofstream out("my_log");
	clog.rdbuf(out.rdbuf());
        if(options.isMk6)
        {

#ifdef HAS_MARK6META
		clog << "test" << endl;
		D->mark6 = new Mark6();
#else
		fprintf(stderr, "Error: mark6 option provided but Mark6 support is not compiled in.\n");

		exit(EXIT_FAILURE);
#endif
	}
        
	while(!D->dieNow)
	{
		t = time(0);
		mjd = 40587.0 + t/86400.0;

		if(t != lastTime)	/* we crossed a 1 second tick */
		{
			lastTime = t;

#ifdef HAVE_XLRAPI_H
			ok = (checkStreamstor(D, t) == 0);
#endif

			// check every 10 seconds
			if( (t % D->loadMonInterval) == 0)
			{
				if(D->isHeadNode)
				{
					int nKill = killSuProcesses(1);
					if(nKill > 0)
					{
						snprintf(message, DIFX_MESSAGE_LENGTH, "Killed %d rogue su processes\n", nKill);
						Logger_logData(D->log, message);
					}
				}

				Mk5Daemon_loadMon(D, mjd);

				// also, every 10 seconds, look for and clean up any defunct children
				waitpid(-1, &status, WNOHANG);
			}

			// check every 30 seconds
			if( (t % D->swapMonInterval) == halfSwapMonInterval)
			{
				Mk5Daemon_swapMon(D, mjd);
			}

#ifdef HAS_MARK6META
			// check every 5 seconds
			if( (t % D->loadMonInterval) == halfLoadMonInterval)
			{
                                // check for new modules on a mark6
                                if(options.isMk6)
                                {
clog << "pre poll" << endl;
                                    D->mark6->pollDevices();
                                    D->mark6->sendStatusMessage();
                                }
                                    //D->mark6.pollDevices();
			}
#endif

#ifdef HAVE_XLRAPI_H
			// check every 5 seconds
			if( (t % D->loadMonInterval) == halfLoadMonInterval)
			{
				if(D->skipGetModule)
				{
					D->skipGetModule = 0;
				}
				else
				{
					Mk5Daemon_getModules(D);
				}
			}

			// determine streamstor version for mk5s once at startup
			if(t - firstTime > 15 && D->isMk5 && isMk5)
			{
				if(justStarted)
				{
					Mk5Daemon_getStreamstorVersions(D);
					logStreamstorVersions(D);
					Mk5Daemon_getModules(D);
					logMk5Smart(D, BANK_A);
					logMk5Smart(D, BANK_B);
					D->systemReady = 1;	/* initial check-out done */
				}
				justStarted = 0;
			}

			/* if record=off was requested but never completed after 5 seconds, raise an error */
			if(D->recordPipe && D->stopRecordRequestTime > 0 && time(0) - D->stopRecordRequestTime > 5)
			{
				Mk5Daemon_addVSIError(D, "Recording hung during stop request");
				D->recordState = RECORD_HUNG;
			}
#endif
		}

		timeout.tv_sec = 1;
		timeout.tv_usec = 0;

		highSock = 0;
		FD_ZERO(&socks);

		if(D->acceptSock)
		{
			FD_SET(D->acceptSock, &socks);
			if(D->acceptSock > highSock)
			{
				highSock = D->acceptSock;
			}
			for(int c = 0; c < MaxConnections; ++c)
			{
				if(D->clientSocks[c] > 0)
				{
					FD_SET(D->clientSocks[c], &socks);
					if(D->clientSocks[c] > highSock)
					{
						highSock = D->clientSocks[c];
					}
				}
			}
		}
#ifdef HAVE_XLRAPI_H
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
			D->stopRecordRequestTime = 0;
			recordFD = -2;
		}
#endif
		if(D->difxSock)
		{
			FD_SET(D->difxSock, &socks);
			if(D->difxSock > highSock)
			{
				highSock = D->difxSock;
			}
		}

		readSocks = select(highSock+1, &socks, 0, 0, &timeout);

		if(readSocks < 0 || *signalDie)
		{
			D->dieNow = 1;

			break;
		}

		if(readSocks == 0)
		{
			continue;
		}

#ifdef HAVE_XLRAPI_H
		if(recordFD >= 0 && FD_ISSET(recordFD, &socks))
		{
			handleRecordMessage(D, t);
		}

		if(D->recordState == RECORD_ON && D->recordRate < 0.01 && t - D->recordT0 > 10)
		{
			D->recordState = RECORD_WAITING;
		}
		else if(D->recordState == RECORD_ON && t - D->recordLastMessage > 5)
		{
			D->recordState = RECORD_HUNG;
			Mk5Daemon_addVSIError(D, "Recording program has hung");
		}
		else if(D->recordState == RECORD_WAITING && D->recordRate > 1.0)
		{
			D->recordState = RECORD_ON;
		}
		else if(D->recordState == RECORD_HUNG && t - D->recordLastMessage < 5)
		{
			D->recordState = RECORD_ON;
			Mk5Daemon_delVSIError(D, "Recording program has hung");
		}
#endif

		if(D->acceptSock)
		{
			if(FD_ISSET(D->acceptSock, &socks))
			{
				handleAcceptMessage(D);
			}

			for(int c = 0; c < MaxConnections; ++c)
			{
				if(FD_ISSET(D->clientSocks[c], &socks))
				{
					int v = handleVSIS(D, D->clientSocks[c]);
					
					if(v < 0) /* connection closed? */
					{
						close(D->clientSocks[c]);
						D->clientSocks[c] = 0;
						snprintf(message, DIFX_MESSAGE_LENGTH, "VSI-S connection on slot %d closed\n", c);
						Logger_logData(D->log, message);
					}
				}
			}
		}
		if(D->difxSock && FD_ISSET(D->difxSock, &socks))
		{
			handleDifxMessage(D, options.noSu);
		}
		if(D->processDone)
		{
			pthread_mutex_lock(&D->processLock);

			pthread_join(D->processThread, 0);
			D->process = PROCESS_NONE;
			D->processDone = 0;
			
			pthread_mutex_unlock(&D->processLock);
		}	
	}

#ifdef HAVE_XLRAPI_H
	if(D->recordPipe > 0)
	{
		pclose(D->recordPipe);
		D->recordPipe = 0;
		D->recordState = RECORD_OFF;
	}

	stopWatchdog();
#endif

	Mk5Daemon_stopMonitor(D);
	Mk5Daemon_stopVSIS(D);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Stopping %s ver. %s\n", program, version);
	Logger_logData(D->log, message);

	deleteMk5Daemon(D);

	return EXIT_SUCCESS;

   }
#ifdef HAS_MARK6META
   catch(Mark6Exception& ex)
   {
    
        cerr << "The following error has occured: " << ex.what() << endl;
        cerr << "This might be caused by insufficient permissions. On a Mark6 machine mk5daemon must be started as root!" << endl;
        cerr << "Aborting" << endl;
        exit(EXIT_FAILURE);
   }
   catch(Mark6MountException& ex)
   {
        cerr << "The following error has occured: " << ex.what() << endl;
        cerr << "This might be caused by insufficient permissions. On a Mark6 machine mk5daemon must be started as root!" << endl;
        cerr << "Aborting" << endl;
        exit(EXIT_FAILURE);
       
   }    
   catch(Mark6InvalidMetadata& ex)
   {
        cerr << "The following error has occured: " << ex.what() << endl;
        cerr << "Aborting" << endl;
        exit(EXIT_FAILURE);
   }
#endif
   catch(...)
   {
	cerr << "An unexpected error has occured. Aborting" << endl;
	exit(EXIT_FAILURE);
   }

   return 0;
}
