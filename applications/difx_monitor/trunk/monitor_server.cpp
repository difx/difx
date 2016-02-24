/***************************************************************************
 *   Copyright (C) 2009-2016 by Chris Phillips                             *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 ***************************************************************************/

#include <iostream>

#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <poll.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <sstream>
#include <stack>
#include "architecture.h"
#include  "monserver.h"
#include "ipps.h"

using namespace std;

#define MAXCLIENTS  20

#define DEBUG 1

struct datadescrstruct {
  int32_t timestampsec;      // Current correlator time
  cf32 *buffer;
};

void plot_results();
void change_config();
int openstream(int portnumber, int tcpwindowsizebytes, int backlog=1);
void closestream(int sock);
int waitforconnection (int serversock);
int pollfd_add(struct pollfd *fds, nfds_t *nfds, int fd, short events);
int pollfd_remove(struct pollfd *fds, nfds_t *nfds, int fd);
int monclient_add(struct monclient *clients,  int *nclient, int maxclient, int fd);
int monclient_remove(struct monclient *clients, int *nclient, int fd);
struct monclient* monclient_find(struct monclient *clients, int nclient, int fd);
void monclient_addproduct(struct monclient *client, int nproduct, struct product_offset offsets[]);
int monclient_sendvisdata(struct monclient client, int32_t thisbuffersize, 
			  struct datadescrstruct *datadescr);

int main(int argc, const char * argv[]) {
  int status, i, j;
  char *tcpwindow;
  int32_t thisbuffersize;
  //cf32 *resultbuffer;
  int port;                  // TCP port to listen on
  int serversocket=0;        // Socket to listen for connections on
  int difxsocket=0;          // Socket to receive data from mpifxcorr
  int monitorsocket=0;       // Socket for monitor clients to connect to
  int tcp_windowsize = -1;   // TCP windowsize for socket connection to mpifxcorr in kbytes
  int32_t buffersize = 0;    // current size of buffer to receive data blob from mpifxcorr
  int nclient=0;             // Number of currently conncted clients
  struct datadescrstruct datadescr;
  struct monclient clients[MAXCLIENTS]; // Details on client connections
  struct pollfd pollfds[MAXCLIENTS+3];  // Sockets to monitor for messages
  nfds_t nfds;               // Number of sockets being monitored by poll
  stack <int> removestack;

  if(argc != 2)  {
    cerr << "Error - invoke with difx_monitor <port>" << endl;
    return EXIT_FAILURE;
  }
  port = atoi(argv[1]);

  tcpwindow = getenv("DIFX_TCP_MONITOR_WINDOWSIZE");
  if (tcpwindow != NULL) {
    tcp_windowsize = atoi(tcpwindow)*1024;
  }

  //open up the socket for mpifxcorr to connect to
  serversocket = openstream(port, tcp_windowsize);
  if (serversocket <=0) {
    cerr << "Error opening connection for mpifxcorr. Aborting" << endl;
    exit(1);
  }

  //open up the socket for the monitor clients to connect to
  monitorsocket = openstream(MONITOR_PORT, 0);
  if (monitorsocket <=0) {
    cerr << "Error opening connection for monitor clients. Aborting" << endl;
    close(serversocket);
    exit(1);
  }

  // Register these two ports

  memset((char *) &pollfds, 0, sizeof(pollfds));

  nfds = 0;
  if (DEBUG) printf("Add serversocket (for difx)\n");
  pollfd_add(pollfds, &nfds, serversocket, POLLIN);
  if (DEBUG) printf("Add monitorsocket (for client)\n");
  pollfd_add(pollfds, &nfds, monitorsocket, POLLIN);

  while (1) {
    int nfd;
    // Wait till some incoming traffic happens

    cout << "Waiting for message or connection" << endl;
    nfd = poll(pollfds, nfds, -1);

    if (nfd<0) {
      perror("Waiting for incoming traffic\n");
      exit(1);
    } else if (nfd==0) continue;

    if (DEBUG) printf(" Got %d descriptors waiting\n", nfd);

    // Look at all available events
    for (i=0; i<(int)nfds; i++) {
      int fd = pollfds[i].fd;
      short revents = pollfds[i].revents;
      if (DEBUG) printf("  %d: fd %d   %d\n", i, fd, revents);
      if (revents==0) continue;
      
      if (fd == serversocket && revents) {
	printf("Event on serversocket %d (%d)\n", fd, revents);
	
	if (revents==POLLIN) { // Connection request
	  printf("Try and connect\n");
	  
	  int asocket = waitforconnection(serversocket);
	  
	  if (asocket<0) {
	    cerr << "Error accepting connection from mpifxcorr\n";
	  } else if (difxsocket>0) {
	    // Disconnect immediately
	    close(asocket);
	    cout << "Ignoring multiple connection attempts" << endl;
	  } else {
	    difxsocket = asocket;
	    pollfd_add(pollfds, &nfds, difxsocket, POLLIN);
	  }
	} else {
	  cerr << "Warning: serversocket received poll error message: " << revents << endl;
	}
	//break;
      } else if (fd == monitorsocket && revents) {
	int newsock;

	if (DEBUG) printf("Got connection on monitorsocket\n");

	if (revents==POLLIN) { // Connection request
	  if (DEBUG) printf("Got connection request\n");
	
	  newsock = waitforconnection(monitorsocket);
	  if (newsock>0) {

	    status = pollfd_add(pollfds, &nfds, newsock, POLLIN);
	    if (status) {
	      cerr << "Too many clients connected, ignoring connection" << endl;
	      status = monserver_sendstatus(newsock, DIFXMON_TOOMANYCLIENTS);
	      if (status) cerr << "Error sending status message to new client" << endl;
	      close(newsock);
	    }  else {

	      status = monclient_add(clients, &nclient, MAXCLIENTS, newsock);
	      if (status) {
		cerr << "Too many clients connected, ignoring connection" << endl;
		pollfd_remove(pollfds, &nfds, newsock);
		status = monserver_sendstatus(newsock, DIFXMON_TOOMANYCLIENTS);
		if (status) cerr << "Error sending status message to new client" << endl;
		close(newsock);
	      }

	      status = monserver_sendstatus(newsock, DIFXMON_NOERROR);
	      if (status) {
		cerr << "Error sending status message to new client" << endl;
		pollfd_remove(pollfds, &nfds, newsock);
		monclient_remove(clients, &nclient, newsock);
		close(newsock);
	      }
	    }
	  }
#if DEBUG
	} else {
	  printf(" revents==%d.  Ignored\n", revents);
#endif
	}
	//break;
      } else if (fd == difxsocket && revents) {
	
	if (revents & POLLHUP) { // Connection went away
	  cout << "Connection to mpifxcorr dropped" << endl;
	  close(difxsocket);
	  //pollfd_remove(pollfds, &nfds, difxsocket);
	  removestack.push(difxsocket);
	  difxsocket = 0;

	} else {
	  cout << "About to get a visibility" << endl;

	  status = DIFXMON_NOERROR;

	  // Receive the timestamp
	  readint(difxsocket, &datadescr.timestampsec, &status);

	  cout << "Got " << datadescr.timestampsec << endl;

	  if (status) { // Error reading socket
	    close(difxsocket);
	    removestack.push(difxsocket);
	    //pollfd_remove(pollfds, &nfds, difxsocket);
	    difxsocket = 0;
	  }

	  //if not skipping this vis
	  if(!(datadescr.timestampsec < 0))  { 
	    cout << "Got visibility # " << datadescr.timestampsec << endl;
      
	    // Get buffersize to follow
	    readint(difxsocket, &thisbuffersize, &status);
	    if (status) break; // Error reading socket

	    if (thisbuffersize>buffersize) {
	      if (buffersize>0) 
		ippsFree(datadescr.buffer);
	      
	      datadescr.buffer = vectorAlloc_cf32(thisbuffersize);
	      buffersize = thisbuffersize;
	    }

	    //receive the results into a buffer
	    status = readnetwork(difxsocket, (char*)datadescr.buffer, thisbuffersize*sizeof(cf32));
	    if (status) { // Error reading socket
	      break;
	    }

	    for (j=0; j<nclient; j++) {
	      cout << "Sending vis data to fd " << clients[j].fd << " " << flush;
	      status = monclient_sendvisdata(clients[j], thisbuffersize, &datadescr);
	      if (status) {
		removestack.push(clients[j].fd);
		//pollfd_remove(pollfds, &nfds, clients[j].fd);
		monclient_remove(clients, &nclient, clients[j].fd);
		monserver_close(&clients[j]);
		j--;  //Not sure this will work....

	      } else {
		cout << "  sent" << endl;
	      }
	    }
	  }
	}
	//break;
      } else if (revents) {
	if (revents & POLLHUP) { // Connection closed
	  if (DEBUG) printf("Connection %d closed\n", fd);
	  removestack.push(fd);
	  //pollfd_remove(pollfds, &nfds, fd);
	  monclient_remove(clients, &nclient, fd);
	  close(fd);
	  //break;
	} else if (revents==POLLIN) { // Message from client requesting products
	  int32_t nproduct;
	  struct monclient *thisclient ;

	  if (DEBUG) printf("Request from client on fd %d\n", fd);
	  // Find the appropriate monclient
	  thisclient = monclient_find(clients, nclient, fd);
	  if (thisclient==NULL) {
	    cerr << "Internal error: Could not find fd " << fd << endl;
	    removestack.push(fd);
	    //pollfd_remove(pollfds, &nfds, fd);
	    monclient_remove(clients, &nclient, fd);
	    close(fd);
	    //break;
	  }

	  readint(fd, &nproduct, &status);
	  if (status) { // Error reading socket
	    cerr << "Problem reading fd " << fd << " closing" << endl;
	    removestack.push(fd);
	    //pollfd_remove(pollfds, &nfds, fd);
	    monclient_remove(clients, &nclient, fd);
	    close(fd);
	  } else {
	    if (DEBUG) printf("Requested %d products\n", nproduct);
	    if (nproduct<0) {
	      status = monserver_sendstatus(fd, DIFXMON_BADPRODUCTS);
	      if (status) {
		removestack.push(fd);
		//pollfd_remove(pollfds, &nfds, fd);
		monclient_remove(clients, &nclient, fd);
		close(fd);
		//break;
	      }
	    } else if (nproduct>0) {
	      struct product_offset *buf = new struct product_offset [nproduct];
	      status = DIFXMON_NOERROR;
	      for (j=0; j<nproduct; j++) {
		readint(fd, &buf[j].offset, &status);
		readint(fd, &buf[j].npoints, &status);
		readint(fd, &buf[j].product, &status);
	      }
	      if (status) {
		removestack.push(fd);		
		//pollfd_remove(pollfds, &nfds, fd);
		monclient_remove(clients, &nclient, fd);
		close(fd);
		continue;
	      }
	      monclient_addproduct(thisclient, nproduct, buf);

	      status = monserver_sendstatus(fd, DIFXMON_NOERROR);
	      if (status) {
		removestack.push(fd);
		//pollfd_remove(pollfds, &nfds, fd);
		monclient_remove(clients, &nclient, fd);
		close(fd);
	      }
	    
	      delete [] buf;
	      //break;
	    }
	  }
	}
      } else {
	printf("Warning %d on descriptor %d ignored\n", revents, fd);
      }
    }

    while (!removestack.empty()) {
      pollfd_remove(pollfds, &nfds, removestack.top());
      removestack.pop();
    }
  }
}

int openstream(int portnumber, int tcpwindowsizebytes, int backlog) {
  int serversock, status;
  struct sockaddr_in server;    /* Socket address */

  /* Open a server connection for reading */

  /* Initialise server's address */
  memset((char *)&server,0,sizeof(server));
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = htonl(INADDR_ANY); /* Anyone can connect */
  server.sin_port = htons((unsigned short)portnumber); /* Which port number to use */

  /* Create a server to listen with */
  serversock = socket(AF_INET,SOCK_STREAM,0); 
  if (serversock==-1) {
    cerr << "Error creating socket" << endl;
    return(-1);
  }

  if (tcpwindowsizebytes > 0) {
    /* Set the TCP window size */
    status = setsockopt(serversock, SOL_SOCKET, SO_SNDBUF,
	       (char *) &tcpwindowsizebytes, sizeof(tcpwindowsizebytes));
    if (status!=0) {
      cerr << "Error setting socket options" << endl;
      close(serversock);
      return(0);
    } 

    status = setsockopt(serversock, SOL_SOCKET, SO_RCVBUF,
	       (char *) &tcpwindowsizebytes, sizeof(tcpwindowsizebytes));

    if (status!=0) {
      cerr << "Error setting socket options" << endl;
      close(serversock);
      return(0);
    } 
  }

  status = bind(serversock, (struct sockaddr *)&server, sizeof(server));
  if (status!=0) {
    cerr << "Error binding socket" << endl;
    close(serversock);
    return(0);
  } 

  /* We are willing to receive conections, using the maximum
     back log of 1 */
  status = listen(serversock,backlog);
  if (status!=0) {
    cerr << "Error binding socket" << endl;
    close(serversock);
    return(0);
  }

  return serversock;
}

int waitforconnection (int serversock) {
  int sock;
  socklen_t client_len;
  struct sockaddr_in client;    /* Socket address */

  /* Accept connection */
  client_len = sizeof(client);
  sock = accept(serversock, (struct sockaddr *)&client, &client_len);
  if (sock == -1) {
    cerr << "Error connecting to client" << endl;
    close(serversock);
    return(-1);
  }

  cout << "Got a connection from " << inet_ntoa(client.sin_addr) << endl;
  return(sock);
}

void closestream(int sock)
{
  //closes the socket
  int status;

  status = close(sock);
  if (status!=0) 
    cerr << "Error closing socket" << endl;
}

// Add a pollfd struct to the end of a pollfd array. Return 1 if not enough space
int pollfd_add(struct pollfd *fds, nfds_t *nfds, int fd, short events) {
  if (DEBUG) printf("pollfd_add\n");

  if (*nfds>=MAXCLIENTS+3) {
    printf("Warning: Too many clients connected. Increase MAXCLIENTS and recompile\n");
    return(1); // No space left
  }

  fds[*nfds].fd = fd;
  fds[*nfds].events = events;
  (*nfds)++;

#if DEBUG
  nfds_t i;
  for (i=0; i<*nfds; i++) {
    printf("%3d    %d\n", fds[i].fd, fds[i].events);
  }
#endif

  return(0);
}

// Find fd in the polfd array and remove it (compressing the array). Return 1 if fd not found
int pollfd_remove(struct pollfd *fds, nfds_t *nfds, int fd) {
  int i, n;
#if DEBUG 
  nfds_t j;
  printf("pollfd_remove\n");
  printf("Initially:\n");
  for (j=0; j<*nfds; j++) {
    printf("%3d    %d\n", fds[j].fd, fds[j].events);
  }
#endif

  n = -1;
  for (i=0; i<(int)*nfds; i++) {
    if (fds[i].fd==fd) {
      n = i;
      break;
    }
  }
  if (n==-1) return(1);

  (*nfds)--;
  for (i=n; i<(int)*nfds; i++) {
    fds[i].fd = fds[i+1].fd;
    fds[i].events = fds[i+1].events;
  }
  fds[*nfds].revents=0;

#if DEBUG 
  printf("Now:\n");
  for (j=0; j<*nfds; j++) {
    printf("%3d    %d\n", fds[j].fd, fds[j].events);
  }
#endif
  
  return(0);
}


// Add a monclient struct to the end of a monclient array. Return 1 if not enough space
int monclient_add(struct monclient *clients,  int *nclient, int maxclient, int fd) {
  if (*nclient>=maxclient) return(1); // No space left

  clients[*nclient].fd = fd;
  clients[*nclient].nvis = 0;
  clients[*nclient].bufsize = 0;
  clients[*nclient].vis = NULL;
  clients[*nclient].visbuf = NULL;
  (*nclient)++;
  
  return(0);
}

// Find fd in the polfd array and remove it (compressing the array). Return 1 if fd not found
int monclient_remove(struct monclient *clients, int *nclient, int fd) {
  int i, n;

  n = -1;
  for (i=0; i<(int)*nclient; i++) {
    if (clients[i].fd==fd) {
      n = i;
      break;
    }
  }
  if (n==-1) return(1);

  delete [] clients[n].vis;

  (*nclient)--;
  for (i=n; i<(int)*nclient; i++) {
    monserver_copyclient(clients[i+1], &clients[i]);
  }

  return(0);
}

// Find fd in the monclient array and return a pointer to the monclient
struct monclient* monclient_find(struct monclient *clients, int nclient, int fd) {
  int i;

  for (i=0; i<nclient; i++) {
    if (clients[i].fd==fd) {
      return &clients[i];
    }
  }
  return(NULL);
}

// Set the product return adday for a monclient
void monclient_addproduct(struct monclient *client, int nproduct, struct product_offset offsets[]){
  int i;

  if (client->nvis>0) delete [] client->vis;
  
  if (nproduct>0) {
    client->nvis = nproduct;
    client->vis = new struct product_offset [nproduct];
    for (i=0; i<nproduct; i++) {
      client->vis[i].offset = offsets[i].offset;
      client->vis[i].npoints = offsets[i].npoints;
      client->vis[i].product = offsets[i].product;
    }
  } else {
    client->nvis=0;
    client->vis = NULL;
  }
  return;
}

int monclient_sendvisdata(struct monclient client, int32_t thisbuffersize, struct datadescrstruct *datadescr) {
  int i, status;
  int32_t buffersize;

  if (client.nvis==0) return(0);

  //  Send 
  //     int32_t     timestampsec
  //     int32_t     nvis
  //     int32_t     buffersize (following)
  // For each visibility:
  //     int32_t     numchannels
  //     int32_t     product #
  //     cf32[numchannels]   

  status = 0;
  sendint(client.fd, datadescr->timestampsec, &status);
  sendint(client.fd, client.nvis, &status);

  buffersize = 0;
  for (i=0; i<client.nvis; i++) 
    buffersize += client.vis[i].npoints*sizeof(cf32) + sizeof(int32_t)*2;
  
  sendint(client.fd, buffersize, &status);

  for (i=0; i<client.nvis; i++) {
    //      if (client.vis[i] < maxvis) {
    sendint(client.fd, client.vis[i].npoints, &status);
    sendint(client.fd, client.vis[i].product, &status);
    if (status) return(status);
    status = writenetwork(client.fd, 
			  (char*)(datadescr->buffer+client.vis[i].offset), 
			  client.vis[i].npoints*sizeof(cf32));
    //      }
    
  }
  return(status);
}
