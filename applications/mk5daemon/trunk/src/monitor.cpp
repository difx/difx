/***************************************************************************
 *   Copyright (C) 2008-2015 by Walter Brisken                             *
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
#include <signal.h>
#include <difxmessage.h>
#include <net/if.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include "config.h"
#include "mk5daemon.h"

int messageForMe(const Mk5Daemon *D, const DifxMessageGeneric *G)
{
	struct addrinfo hints;

	memset(&hints, 0, sizeof hints);
	hints.ai_family   = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags    = AI_PASSIVE;

	if(G->nTo < 1)
	{
		return 0;
	}

	for(int t = 0; t < G->nTo; ++t)
	{
		int rv;
		struct addrinfo *results;

		if(strcasecmp("all", G->to[t]) == 0)
		{
			return 1;
		}
		if(strcasecmp(D->hostName, G->to[t]) == 0)
		{
			return 1;
		}
		if(D->isMk5)
		{
			if(strcasecmp("mark5", G->to[t]) == 0)
			{
				return 1;
			}
		}
		else
		{
			if(strcasecmp("swc", G->to[t]) == 0)
			{
				return 1;
			}
		}
		/* finally look closer at the network address */
		/* do a name look-up on the to[] field */
		rv = getaddrinfo(G->to[t], NULL, &hints, &results);
		if(rv == 0)
		{
			/* No error, so continue */

			struct addrinfo *result;

			for(result = results; result != 0; result = result->ai_next)
			{
				struct in_addr *addr;
				char addrString[INET_ADDRSTRLEN];
				
				if(result->ai_family == AF_INET)
				{
					struct sockaddr_in *ipv = (struct sockaddr_in *)result->ai_addr;
					addr = &(ipv->sin_addr);
				}
				else if(result->ai_family == AF_INET6)
				{
					struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *)result->ai_addr;
					addr = (struct in_addr *)&(ipv6->sin6_addr);
				}
				else
				{
					continue;
				}
				inet_ntop(result->ai_family, addr, addrString, INET_ADDRSTRLEN);

				if(Mk5Daemon_addrMatches(D, addrString))
				{
					freeaddrinfo(results);

					return 1;
				}
			}

			freeaddrinfo(results);
		}
	}

	return 0;
}

void handleMk5Status(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	/* only care if the message came from another process on same node */
	if(strcmp(D->hostName, G->from) != 0)
	{
		return;
	}

	/* only care if it is a mk5status from a datastream node */
	if(G->mpiId <= 0 || G->type != DIFX_MESSAGE_MARK5STATUS)
	{
		return;
	}

	strncpy(D->vsns[0], G->body.mk5status.vsnA, 8);
	D->vsns[0][8] = 0;
	strncpy(D->vsns[1], G->body.mk5status.vsnB, 8);
	D->vsns[1][8] = 0;

	if(G->body.mk5status.state == MARK5_STATE_OPENING ||
	   G->body.mk5status.state == MARK5_STATE_OPEN ||
	   G->body.mk5status.state == MARK5_STATE_PLAY ||
	   G->body.mk5status.state == MARK5_STATE_GETDIR ||
	   G->body.mk5status.state == MARK5_STATE_GOTDIR)
	{
		if(D->process == PROCESS_NONE)
		{
			Logger_logData(D->log, "mpifxcorr started\n");
		}
		D->process = PROCESS_DATASTREAM;

		/* update timestamp of last update */
		D->lastMpifxcorrUpdate = time(0);
	}

	if(G->body.mk5status.state == MARK5_STATE_CLOSE)
	{
		D->process = PROCESS_NONE;
		Logger_logData(D->log, "mpifxcorr finished\n");

		D->lastMpifxcorrUpdate = 0;
	}
}
		
static void mountdisk(Mk5Daemon *D, const char *diskdev)
{
	char dev[64];
	char command[MAX_COMMAND_SIZE];
	char message[DIFX_MESSAGE_LENGTH];
	char rv[256] = "hidden message";
	char *c;
	FILE *pin;

	if(strlen(diskdev) > 10)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Mount: device name is bogus: /dev/sd%s", diskdev);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
	
		return;
	}
	
	sprintf(dev, "/dev/sd%s", diskdev);
	
	snprintf(command, MAX_COMMAND_SIZE, "/bin/mount -t auto %s /mnt/usb 2>&1", dev);

	snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
	Logger_logData(D->log, message);

	pin = popen(command, "r");
	c = fgets(rv, 255, pin);
	pclose(pin);

	if(c)
	{
		int l;
		
		/* strip endline */
		l = strlen(rv);
		rv[l-1] = 0;

		snprintf(message, DIFX_MESSAGE_LENGTH, "Mount %s attempt : %s", dev, rv);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Mount %s attempt : Success", dev);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	}
}

static void reown_vfastr(Mk5Daemon *D, const char *path)
{
	char command[MAX_COMMAND_SIZE];
	char message[DIFX_MESSAGE_LENGTH];
	int v;

	if(strcmp(D->hostName, "boom") != 0)
	{
		return;
	}

	while(path[0] <= ' ')
	{
		if(path[0] == 0)
		{
			Logger_logData(D->log, "Null path given to reown_vfastr\n");

			return;
		}
		++path;
	}

	if(strncmp(path, "/home/boom/", 11) != 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "reown_vfastr: path=%s does not start with /home/boom/\n", path);
		Logger_logData(D->log, message);

		return;
	}

	snprintf(command, MAX_COMMAND_SIZE, "/bin/chgrp --recursive vlba_transient %s", path);
	snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
	Logger_logData(D->log, message);
	v = system(command);
	if(v < 0)
	{
		Logger_logData(D->log, "Execution failed.\n");
	}

	snprintf(command, MAX_COMMAND_SIZE, "/bin/chmod --recursive g+rw %s", path);
	snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
	Logger_logData(D->log, message);
	v = system(command);
	if(v < 0)
	{
		Logger_logData(D->log, "Execution failed.\n");
	}
}

static void umountdisk(Mk5Daemon *D)
{
	const char *command;
	char message[DIFX_MESSAGE_LENGTH];
	char rv[256] = "I like chinchillas";
	char *c;
	FILE *pin;

	command = "/bin/umount /mnt/usb 2>&1";
	
	snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s\n", command);
	Logger_logData(D->log, message);

	pin = popen(command, "r");
	c = fgets(rv, 255, pin);
	pclose(pin);

	if(c)
	{
		int l;
		
		/* strip endline */
		l = strlen(rv);
		rv[l-1] = 0;

		snprintf(message, DIFX_MESSAGE_LENGTH, "Unmount /mnt/usb attempt : %s", rv);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Unmount /mnt/usb attempt : Success");
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	}
}

void handleCommand(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	const char *cmd;
	char message[DIFX_MESSAGE_LENGTH];

	if(!messageForMe(D, G))
	{
		return;
	}

	cmd = G->body.command.command;

	snprintf(message, DIFX_MESSAGE_LENGTH,
		"Command: from=%s identifier=%s command=%s\n", 
		G->from, G->identifier, cmd);
	Logger_logData(D->log, message);

	if(strcasecmp(cmd, "Reboot") == 0)
	{
		Mk5Daemon_reboot(D);
	}
	else if(strcasecmp(cmd, "Poweroff") == 0)
	{
		Mk5Daemon_poweroff(D);
	}
	else if(strcasecmp(cmd, "stopmk5daemon") == 0)
	{
		D->dieNow = 1;
	}
	else if(strcasecmp(cmd, "killmpifxcorr") == 0)
	{
		Mk5Daemon_system(D, "killall -9 mpifxcorr", 1);
		Mk5Daemon_system(D, "killall -9 mpirun", 1);
	}
	else if(strncasecmp(cmd, "killmpifxcorr ", 14) == 0)
	{
		Mk5Daemon_killJob(D, cmd + 14);
	}
#ifdef HAVE_XLRAPI_H
	else if(strcasecmp(cmd, "Clear") == 0)
	{
		D->process = PROCESS_NONE;
		if(D->isMk5)
		{
			Mk5Daemon_getModules(D);
		}
	}
	else if(strcasecmp(cmd, "GetVSN") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_getModules(D);
		}
	}
	else if(strncasecmp(cmd, "ResetMark5", 10) == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_resetStreamstor(D);
		}
	}
	else if(strcasecmp(cmd, "getdirA") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMk5Dir(D, "A");
		}
	}
	else if(strcasecmp(cmd, "getdirB") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMk5Dir(D, "B");
		}
	}
	else if(strcasecmp(cmd, "getdir") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startMk5Dir(D, "AB");
		}
	}
	else if(strcasecmp(cmd, "stopdir") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_stopMk5Dir(D);
		}
	}
	else if(strcasecmp(cmd, "erase") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startCondition(D, "");
		}
	}
	else if(strcasecmp(cmd, "condition") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startCondition(D, "-c");
		}
	}
	else if(strcasecmp(cmd, "conditionR") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startCondition(D, "-c");
		}
	}
	else if(strcasecmp(cmd, "conditionW") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_startCondition(D, "-c");
		}
	}
	else if(strcasecmp(cmd, "stopcondition") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_stopCondition(D);
		}
	}
	else if(strcasecmp(cmd, "discon") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOn(D, "AB");
		}
	}
	else if(strcasecmp(cmd, "disconA") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOn(D, "A");
		}
	}
	else if(strcasecmp(cmd, "disconB") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOn(D, "B");
		}
	}
	else if(strcasecmp(cmd, "discoff") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOff(D, "AB");
		}
	}
	else if(strcasecmp(cmd, "discoffA") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOff(D, "A");
		}
	}
	else if(strcasecmp(cmd, "discoffB") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_diskOff(D, "B");
		}
	}
	else if(strncasecmp(cmd, "copy ", 5) == 0)
	{
		/* protect against malicious use */
		if(D->isMk5 && !strstr(cmd, ";") 
		            && !strstr(cmd, "|") 
		            && !strstr(cmd, ">") 
		            && !strstr(cmd, "<") )
		{
			Mk5Daemon_startMk5Copy(D, cmd+5);
		}
	}
	else if(strcasecmp(cmd, "stopcopy") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_stopMk5Copy(D);
		}
	}
	else if(strcasecmp(cmd, "getver") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_sendStreamstorVersions(D);
		}
	}
	else if(strcmp(cmd, "getsmart") == 0)
	{
		if(D->isMk5)
		{
			Mk5Daemon_sendSmartData(D);
		}
	}
#endif
	else if(strncmp(cmd, "mount", 5) == 0 && strlen(cmd) > 5)
	{
		mountdisk(D, cmd+5);
	}
	else if(strcmp(cmd, "umount") == 0)
	{
		umountdisk(D);
	}
	else if(strncmp(cmd, "reown_vfastr", 12) == 0 && strlen(cmd) > 12)
	{
		reown_vfastr(D, cmd+12);
	}
	else if(strncasecmp(cmd, "Test", 4) == 0)
	{
		printf("[%s]\n", cmd);
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Command=%s not recognized!\n", cmd);
		Logger_logData(D->log, message);
	}
}

void handleDriveStats(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	char message[DIFX_MESSAGE_LENGTH];
	const DifxMessageDriveStats *c;

	if(!messageForMe(D, G))
	{
		return;
	}

	c = &G->body.driveStats;

	snprintf(message, DIFX_MESSAGE_LENGTH, 
		"Drive statistivs report: from=%s identifier=%s disk=%s[%d]=%s\n", 
		G->from, G->identifier, c->moduleVSN, c->moduleSlot, c->serialNumber);
	Logger_logData(D->log, message);
}

int Mk5Daemon_startMonitor(Mk5Daemon *D)
{
	Mk5Daemon_stopMonitor(D);

	D->difxSock = difxMessageReceiveOpen();
	
	if(D->difxSock < 0)
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

void Mk5Daemon_stopMonitor(Mk5Daemon *D)
{
	difxMessageReceiveClose(D->difxSock);
	D->difxSock = 0;
}

void Mk5Daemon_reboot(Mk5Daemon *D)
{
	const char command[] = "/sbin/reboot";

	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsns[0], 8);
	strncpy(dm.vsnB, D->vsns[1], 8);
	dm.state = MARK5_STATE_REBOOTING;
	difxMessageSendMark5Status(&dm);

	D->dieNow = 1;
	Mk5Daemon_system(D, command, 1);
}

void Mk5Daemon_poweroff(Mk5Daemon *D)
{
	const char command[] = "/sbin/poweroff";

	DifxMessageMk5Status dm;

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	strncpy(dm.vsnA, D->vsns[0], 8);
	strncpy(dm.vsnB, D->vsns[1], 8);
	dm.state = MARK5_STATE_POWEROFF;
	difxMessageSendMark5Status(&dm);

	Logger_logData(D->log, "PowerOff message received.");
	deleteLogger(D->log);

	sleep(1);
	Mk5Daemon_system(D, command, 1);
}

void Mk5Daemon_killJob(Mk5Daemon *D, const char *jobName)
{
	const int CommandLength = 128;
	const int LineLength = 512;
	char message[DIFX_MESSAGE_LENGTH];
	char cmd[CommandLength];
	char line[LineLength];
	FILE *pin;

	snprintf(cmd, CommandLength, "ps aux | grep \" mpifxcorr \" | grep %s.input", jobName);
	pin = popen(cmd, "r");
	if(!pin)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_killJob: popen failed for jobName=%s", jobName);
		Logger_logData(D->log, message);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

		return;
	}

	for(;;)
	{
		int pid;

		if(fgets(line, LineLength, pin) == 0)
		{
			break;
		}
		if(sscanf(line, "%*s%d", &pid) == 1)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_killJob: executing kill(%d, SIGKILL)\n", pid);
			Logger_logData(D->log, message);
			snprintf(message, CommandLength, "Killing mpifxcorr for job %s with process ID %d", jobName, pid);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			kill(pid, SIGKILL);
		}
	}
	pclose(pin);

	/* Now go back and make sure they are gone */
	sleep(1);

	snprintf(cmd, CommandLength, "ps aux | grep \" mpifxcorr \" | grep %s.input", jobName);
	pin = popen(cmd, "r");
	if(!pin)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_killJob: popen failed for jobName=%s (round 2)", jobName);
		Logger_logData(D->log, message);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

		return;
	}

	for(;;)
	{
		int pid;

		if(fgets(line, LineLength, pin) == 0)
		{
			break;
		}
		if(sscanf(line, "%*s%d", &pid) == 1)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_killJob: Weird: pid %d remains after kill-9", pid);
			Logger_logData(D->log, message);
			snprintf(message, CommandLength, "Killing of job %s not successful.  Try again.", jobName);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		}
	}
	pclose(pin);
}
