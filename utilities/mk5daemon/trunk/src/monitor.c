#include <difxmessage.h>
#include "mk5daemon.h"

static void *monitorMultiListen(void *ptr)
{
	Mk5Daemon *D;
	int sock, port, n;
	char group[16];
	char message[2000], from[20];

	D = (Mk5Daemon *)ptr;

	difxMessageGetMulticastGroupPort(group, &port);
	sock = openMultiCastSocket(group, port);
	
	for(;;)
	{
		n = MultiCastReceive(sock, message, 1999, from);
	}

	closeMultiCastSocket(sock);

	return 0;
}

void Mk5Daemon_startMonitor(Mk5Daemon *D)
{
	pthread_create(&D->monitorThread, 0, &monitorMultiListen, D);
}

void Mk5Daemon_stopMonitor(Mk5Daemon *D)
{
	pthread_join(D->monitorThread, 0);
}
