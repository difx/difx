#ifndef __MULTICAST_H__
#define __MULTICAST_H__

int openMultiCastSocket(const char *group, int port);
int closeMultiCastSocket(int sock);

int MultiCastReceive(int sock, char *message, int maxlen, char *from);

#endif
