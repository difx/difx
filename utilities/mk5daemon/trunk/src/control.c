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
	
	for(;;)
	{
		n = MultiCastReceive(sock, message, 1999, from);
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
