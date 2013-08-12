/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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
#include <ctype.h>
#include <regex.h>
#include "proc.h"
#include <iostream>
#include <fstream>

/* routines to get useful information from /proc */

/**
 * get number of CPU cores by parsing /proc/cpuinfo. This routine
 * is meant to superseed procGetCores, which does not work on
 * all linux OS versions.
 * @param nCore the number of cores determined
 * @return -1 in case reading /proc/cpuinfo has failed, 0 otherwise
 * @author Helge Rottmann
 */
int procGetCoresFromCpuInfo(int *nCore)
{
        const int MaxLineLength=256;
        FILE *in;
        char line[MaxLineLength+1];
        char *c;
        int val;


        in = popen("cat /proc/cpuinfo | grep siblings | uniq", "r");
        if(!in)
        {
                return -1;
        }

        for(;;)
        {
                c = fgets(line, MaxLineLength, in);
                if(!c)
                {
                        break;
                }
                sscanf(line, "%*s : %d", &val);
                *nCore = val;
        }

        pclose(in);

        return 0;
}

int procGetCores(int *nCore)
{
	const int MaxLineLength=256;
	FILE *pin;
	char line[MaxLineLength+1];
	regex_t filter;
	regmatch_t match[4];
	int v;

	*nCore = 0;
        std::ifstream testExist;

        // test if file exists
        testExist.open("/sys/devices/system/cpu/cpu0/topology/core_siblings_list");
        if (!testExist.is_open())
      		 return -2;
        
	
	pin = popen("cat /sys/devices/system/cpu/cpu*/topology/core_siblings_list | sort | uniq", "r");
	if(!pin)
	{
		return -1;
	}
	
	/* look for strings of form # or #-# */
	v = regcomp(&filter, "([0-9]+)(-([0-9]+))?", REG_EXTENDED);

	if(v != 0)
	{
		fprintf(stderr, "Error: procGetCores: compiling regex\n");
	}

	for(;;)
	{
		int i;
		char *c;

		c = fgets(line, MaxLineLength, pin);
		if(!c)
		{
			break;
		}
		line[MaxLineLength-1] = 0;

		for(i = 0;;)
		{
			int b, e;

			if(regexec(&filter, line+i, 4, match, 0) != 0)
			{
				break;
			}

			line[i+match[1].rm_eo] = 0;
			b = atoi(line+i+match[1].rm_so);
			if(match[3].rm_so > match[1].rm_eo)
			{
				line[i+match[3].rm_eo] = 0;
				e = atoi(line+i+match[3].rm_so);
			}
			else
			{
				e = b;
			}

			i += match[0].rm_eo + 1;

			*nCore += (e-b+1);
		}
	}

	regfree(&filter);

	pclose(pin);

	return 0;
}

int procGetMem(int *memused, int *memtot)
{
	const int MaxLineLength=256;
	FILE *in;

	*memused = 0;
	*memtot = 0;
	
	in = fopen("/proc/meminfo", "r");
	if(!in)
	{
		return -1;
	}
	
	for(;;)
	{
		char *c;
		char line[MaxLineLength+1];
		char key[MaxLineLength+1];
		int val;
		
		c = fgets(line, MaxLineLength, in);
		if(!c)
		{
			break;
		}
		sscanf(line, "%s%d", key, &val);
		if(strcmp(key, "MemTotal:") == 0)
		{
			*memtot = val;
			*memused += val;
		}
		if(strcmp(key, "MemFree:") == 0 ||
		   strcmp(key, "Buffers:") == 0 ||
		   strcmp(key, "Cached:") == 0)
		{
			*memused -= val;
		}
	}

	fclose(in);

	return 0;
}

int procGetNet(long long *rx, long long *tx)
{
	const int MaxInterfaces=10;
	const int MaxLineLength=256;
	static long long lastrx[MaxInterfaces] = {0};
	static long long lasttx[MaxInterfaces] = {0};
	FILE *in;

	*rx = 0LL;
	*tx = 0LL;

	in = fopen("/proc/net/dev", "r");
	if(!in)
	{
		return -1;
	}

	for(int i = 0; i < MaxInterfaces; ++i)
	{
		char line[MaxLineLength+1];
		char *c;
		
		c = fgets(line, MaxLineLength, in);
		if(!c)
		{
			break;
		}
		if(strncmp(line, "    lo", 6) != 0 && line[6] == ':')	/* count all interfaces except lo */
		{
			long long a, b;
			int v;
			
			v = sscanf(line+7, "%lld%*d%*d%*d%*d%*d%*d%*d%lld", &a, &b);
			if(v >= 2)
			{
				/* take into account 32 bit counters on 32-bit machines */
				if(a < lastrx[i])
				{
					a = (a & 0xFFFFFFFFLL) | (lastrx[i] & 0xFFFFFFFF00000000LL);
					if(a < lastrx[i])
					{
						a += 0x100000000LL;
					}
				}
				if(b < lasttx[i])
				{
					b = (b & 0xFFFFFFFFLL) | (lasttx[i] & 0xFFFFFFFF00000000LL);
					if(b < lasttx[i])
					{
						b += 0x100000000LL;
					}
				}
				*rx += a;
				*tx += b;

				lastrx[i] = a;
				lasttx[i] = b;
			}
		}
	}

	fclose(in);

	return 0;
}

int procGetCPU(float *l1, float *l5, float *l15)
{
	const int MaxLineLength=256;
	FILE *in;
	char line[MaxLineLength+1];
	char *c;

	in = fopen("/proc/loadavg", "r");
	if(!in)
	{
		return -1;
	}

	c = fgets(line, MaxLineLength, in);
	if(c)
	{
		sscanf(line, "%f%f%f", l1, l5, l15);
	}
	else
	{
		*l1 = *l5 = *l15 = 0;
	}

	fclose(in);

	return 0;
}

int procGetStreamstor(int *busy)
{
	const int MaxLineLength=256;
	FILE *in;

	*busy = 0;

	in = fopen("/proc/modules", "r");
	if(!in)
	{
		return -1;
	}

	for(;;)
	{
		char line[MaxLineLength+1];
		char *c;

		c = fgets(line, MaxLineLength, in);
		if(!c)
		{
			break;
		}
		if(strncmp(line, "windrvr6 ", 9) == 0)
		{
			sscanf(line+9, "%*d %d", busy);

			break;
		}
	}

	fclose(in);

	return 0;
}

/**
* Kill rogue su processes. Processes will be killed if they
* either have been running for more than 1000 seconds, or 
* have been running for more than 100 seconds but take 
* up more than 90% of cpu resources.
* @param verbose verbose output will be printed if > 0
* @return returns number of processes killed 
* @author Walter Brisken
*/
int killSuProcesses(int verbose)
{
	const int MaxStrLen=256;
	const int MaxCmdLen=64;
	FILE *p;
	char cmd[MaxCmdLen];
	int nKill=0;
	int n;

	n = snprintf(cmd, MaxCmdLen, "ps -C su -o user,pid,pcpu,time");
	if(n >= MaxCmdLen)
	{
		fprintf(stderr, "Warning: MaxCmdLen too small = %d vs. %d\n", MaxCmdLen, n);

		return -4;
	}

	p = popen(cmd, "r");
	if(!p)
	{
		fprintf(stderr, "Error: popen(%s) failed\n", cmd);

		return -3;
	}
	
	for(;;)
	{
		int t, v;
		char *r;
		int pid;
		float pcpu;
		char owner[16], timestr[16];
		char str[MaxStrLen];

		r = fgets(str, MaxStrLen-1, p);
		str[MaxStrLen-1] = 0;
		if(!r)
		{
			break;
		}
		n = sscanf(str, "%s %d %f %s", owner, &pid, &pcpu, timestr);

		if(n < 4)
		{
			if(n >= 1 && strcmp(owner, "USER") == 0)
			{
				continue;
			}

			nKill = -2;

			fprintf(stderr, "Parse error: n = %d str='%s'\n", n, str);

			break;
		}
		if(strcmp(owner, "root") != 0)
		{
			continue;
		}
		if(strlen(timestr) != 8 || timestr[2] != ':' || timestr[5] != ':')
		{
			continue;
		}
		timestr[2] = timestr[5] = 0;
		t = 3600*atoi(timestr) + 60*atoi(timestr+3) + atoi(timestr+6);
		if((t > 100 && pcpu >= 90) || t > 1000)
		{
			n = snprintf(cmd, MaxCmdLen, "kill -9 %d", pid);
			if(n >= MaxCmdLen)
			{
				fprintf(stderr, "Warning: MaxCmdLen too small = %d vs. %d\n", MaxCmdLen, n);

				nKill = -1;

				break;
			}
			if(verbose > 1)
			{
				printf("Executing: %s\n", cmd);
			}
			v = system(cmd);
			if(verbose > 1)
			{
				printf("Return value = %d\n", v);
			}
			if(v != -1 && v != 256)
			{
				++nKill;
			}
		}
	}

	pclose(p);

	return nKill;
}
