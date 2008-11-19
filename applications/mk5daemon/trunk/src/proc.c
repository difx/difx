#include <stdio.h>
#include <string.h>
#include "proc.h"

/* routines to get useful information from /proc */

int procGetMem(int *memused, int *memtot)
{
	FILE *in;
	char line[100];
	char key[100];
	int val;

	*memused = 0;
	*memtot = 0;
	
	in = fopen("/proc/meminfo", "r");
	if(!in)
	{
		return -1;
	}
	
	for(;;)
	{
		fgets(line, 99, in);
		if(feof(in))
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
	FILE *in;
	char line[100];
	long long a, b;
	int v;

	*rx = 0LL;
	*tx = 0LL;

	in = fopen("/proc/net/dev", "r");
	if(!in)
	{
		return -1;
	}

	for(;;)
	{
		fgets(line, 99, in);
		if(feof(in))
		{
			break;
		}
		if(strncmp(line, "  eth", 5) == 0)
		{
			v = sscanf(line+7, "%lld%*d%*d%*d%*d%*d%*d%*d%lld", 
				&a, &b);
			if(v >= 2)
			{
				*rx += a;
				*tx += b;
			}
		}
	}

	fclose(in);

	return 0;
}

int procGetCPU(float *l1, float *l5, float *l15)
{
	FILE *in;
	char line[100];

	in = fopen("/proc/loadavg", "r");
	if(!in)
	{
		return -1;
	}

	fgets(line, 99, in);
	sscanf(line, "%f%f%f", l1, l5, l15);

	fclose(in);

	return 0;
}

int procGetStreamstor(int *busy)
{
	FILE *in;
	char line[100];

	*busy = 0;

	in = fopen("/proc/modules", "r");
	if(!in)
	{
		return -1;
	}

	for(;;)
	{
		fgets(line, 99, in);
		if(feof(in))
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
