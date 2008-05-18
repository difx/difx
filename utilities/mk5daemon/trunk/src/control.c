#include <string.h>
#include <difxmessage.h>
#include "mk5daemon.h"

static void *controlMultiListen(void *ptr)
{
	Mk5Daemon *D;
	int sock, port, n;
	char group[16];
	char message[2000], from[20];

	D = (Mk5Daemon *)ptr;

	difxMessageGetMulticastGroupPort(group, &port);
	port++;
	sock = openMultiCastSocket(group, port);
	
	while(!D->dieNow)
	{
		n = MultiCastReceive(sock, message, 1999, from);

		if(n > 0)
		{
			if(strncmp(message, "VSN?", 4) == 0)
			{
				Mk5Daemon_getModules(D);
			}
			else if(strncmp(message, "QUIT", 4) == 0)
			{
				D->dieNow = 1;
			}
			else if(strncmp(message, "LOAD", 4) == 0)
			{
				Mk5Daemon_loadMon(D);
			}
			else if(strncmp(message, "RESET", 5) == 0)
			{
				Mk5Daemon_resetMark5A(D);
			}
			else if(strncmp(message, "StartM5", 7) == 0)
			{
				Mk5Daemon_startMark5A(D);
				printf("5");
			}
			else if(strncmp(message, "StopM5", 6) == 0)
			{
				Mk5Daemon_stopMark5A(D);
				printf("6");
			}
		}

		printf("c");fflush(stdout);
	}

	closeMultiCastSocket(sock);

	return 0;
}

void Mk5Daemon_startControl(Mk5Daemon *D)
{
	pthread_create(&D->controlThread, 0, &controlMultiListen, D);
}

void Mk5Daemon_stopControl(Mk5Daemon *D)
{
	pthread_join(D->controlThread, 0);
}
