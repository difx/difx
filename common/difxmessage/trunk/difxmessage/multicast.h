#ifndef __MULTICAST_H__
#define __MULTICAST_H__

/* single function for send.  returns message length on success, or -1 */

int MulticastSend(const char *group, int port, const char *message);


/* functions for receive */

int openMultiCastSocket(const char *group, int port);
int closeMultiCastSocket(int sock);
int MultiCastReceive(int sock, char *message, int maxlen, char *from);

#endif
