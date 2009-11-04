#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>

#include "monserver.h"

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL SO_NOSIGPIPE 
#endif

int readnetwork(int sock, char* ptr, int bytestoread) {
  int nr;

  while (bytestoread>0)
  {
    nr = recv(sock,ptr,bytestoread,0);

    if (nr==-1) { // Error reading socket
      if (errno == EINTR) continue;
      fprintf(stderr, "Error reading socket\n");
      return(-1);
    } else if (nr==0) {  // Socket closed remotely
      fprintf(stderr, "Socket closed remotely\n");
      return(-1);
    } else {
      ptr += nr;
      bytestoread -= nr;
    }
  }
  return(0);
}

int writenetwork(int sock, char* ptr, int bytestowrite) {
  int nwrote;

  while (bytestowrite>0) {

    nwrote = send(sock, ptr, bytestowrite, MSG_NOSIGNAL|MSG_DONTWAIT);
    if (nwrote==-1) {
      if (errno == EINTR) continue;
      perror("Error writing to network");

      return(1);
    } else if (nwrote==0) {
      fprintf(stderr, "Warning: Did not write any bytes!\n");
      return(1);
    } else {
      bytestowrite -= nwrote;
      ptr += nwrote;
    }
  }
  return(0);
}

void sendint(int sock, int32_t val, int *status) {
  if (*status) return;
  *status = writenetwork(sock, (char*)&val, sizeof(int32_t)); 
  return;
}

int monserver_connect(struct monclient *monclient, char *monhostname, int window_size) {
  int sock, status;
  int32_t status32;
  unsigned long ip_addr;
  struct hostent     *hostptr;
  struct sockaddr_in server;

  memset(monclient, 0, sizeof(struct monclient));

  // Setup network structures etc
  hostptr = gethostbyname(monhostname);
  if (hostptr==NULL) {
    fprintf(stderr,"Failed to look up hostname %s\n", monhostname);
    return(1);
  }

  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons(MONITOR_PORT); /* Which port number to use */
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  server.sin_addr.s_addr = ip_addr;
  
  printf("Connecting to %s\n",inet_ntoa(server.sin_addr));

  /* Create the initial socket */
  sock = socket(AF_INET,SOCK_STREAM,0); 
  if (sock==-1) {
    perror("Error creating socket");
    return(1);
  }

  if (window_size>0) {
    status = setsockopt(sock, SOL_SOCKET, SO_SNDBUF,
			(char *) &window_size, sizeof(window_size));
    if (status!=0) {
      perror("Error setting socket send buffer");
      close(sock);
      return(1);
    } 

    status = setsockopt(sock, SOL_SOCKET, SO_RCVBUF,
			(char *) &window_size, sizeof(window_size));
    
    if (status!=0) {
      perror("Error setting socket receive buffer");
      close(sock);
      return(1);
    }
  }

  // Actually connect to remote host

  status = connect(sock, (struct sockaddr *) &server, sizeof(server));
  if (status!=0) {
    perror("Failed to connect to server");
    close(sock);
    return(1);
  }

  // Receive a status message from the server - 4 byte code

  status32 = 0;
  status = readnetwork(sock, (char*)&status32, sizeof(status32));
  if (status) return(status);

  if (status32!=DIFXMON_NOERROR) {
    fprintf(stderr, "Monitor server refused connection with Error %d\n", status32);
    close(sock);
    return(1);
  }

  monclient->fd = sock;

  return(0);
}

int monserver_close(struct monclient client) {
  if (client.nvis>0) free(client.vis); // Probably should zero, but that means passing a pointer
  if (client.nretvis>0) free(client.visbuf);
  return(close(client.fd));
}

int monserver_sendstatus(int sock, int32_t status32) {
  return(writenetwork(sock, (char*)&status32, sizeof(status32)));
}

int monserver_requestproduct(struct monclient client, unsigned int product) {
  return monserver_requestproducts(client, &product, 1);
}

int monserver_requestproducts(struct monclient client, unsigned int product[], int nprod) {
  int status, i;
  int32_t  status32;
  
  status = DIFXMON_NOERROR;

  sendint(client.fd, nprod, &status);

  for (i=0; i<nprod; i++) 
    sendint(client.fd, product[i], &status);
  if (status) return(status);

  status32 = 0;
  status = readnetwork(client.fd, (char*)&status32, sizeof(status32));
  if (status) return(status);

  if (status32!=DIFXMON_NOERROR) {
    fprintf(stderr, "Error %d requesting products from monitor\n", status32);
    return(1);
  }

  return(0);
}

int monserver_requestall(struct monclient client) {
  unsigned int product=-1;
  return monserver_requestproducts(client, &product, 1);
}

int monserver_readvis(struct monclient *client) {
  int status;
  int32_t headbuf[4], bufsize;

  status = readnetwork(client->fd, (char*)&headbuf, sizeof(headbuf)); 
  if (status) return(status);

  client->timestamp = headbuf[0];
  client->numchannels = headbuf[1];
  client->ivis = 0;
  bufsize = headbuf[3];
  if (client->nretvis != headbuf[2]) {
    if (client->nretvis!=0) {
      free(client->visbuf);
    }
    client->nretvis = headbuf[2];
    client->visbuf = (char*)malloc(bufsize);
    if (client->visbuf==NULL) {
      fprintf(stderr, "Error: Could not allocate memory for visibility buffer\n");
      return(1);
    }
  }
  status = readnetwork(client->fd, (char*)client->visbuf, bufsize);

  return(status);
}

int monserver_nextvis(struct monclient *client, int *product, Ipp32fc **vis) {
  int ivis = client->ivis;

  if (ivis >= client->nretvis)  return(1);

  *product = *(int32_t*)(client->visbuf+ivis*(sizeof(int32_t) 
   	                         + client->numchannels*sizeof(Ipp32fc)));

  *vis = (Ipp32fc*)(client->visbuf+sizeof(int32_t) + ivis*(sizeof(int32_t) 
   	                         + client->numchannels*sizeof(Ipp32fc)));

  client->ivis++;
  return(0);
}

void monserver_resetvis(struct monclient *client) {
  client->ivis=0;
}
