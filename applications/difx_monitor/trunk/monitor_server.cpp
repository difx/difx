/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <cpgplot.h>
#include <string.h>
#include <sstream>
#include "architecture.h"
#include "configuration.h"

using namespace std;

//prototypes
void plot_results();
void change_config();
int openstream(int portnumber, int tcpwindowsizebytes);
void closestream(int sock);
int readnetwork(int sock, char* ptr, int bytestoread);

int maxresultlength, buffersize, atseconds, bufferindex;
int resultlength, numchannels;
double intseconds = 1;
f32  *phase, *amplitude;
f32 *lags;
f32 *xval;

int main(int argc, const char * argv[])
{
  int readbytes;
  char *tcpwindow;
  int32_t thisbuffersize;

  cf32 *resultbuffer;
  int status = 1;
  int port;                  // TCP port to listen on
  int socketnumber;          // Socket to receive data from mpifxcorr
  int tcp_windowsize = -1;   // TCP windowsize for socket connection to mpifxcorr in kbytes
  int32_t buffersize = 0;    // current size of buffer to receive data blob from mpifxcorr
  int32_t numchannels;       // Number of spectral points per product
  int32_t timestampsec;      // Current correlator time

  if(argc != 2)
  {
    cerr << "Error - invoke with difx_monitor <port>" << endl;
    return EXIT_FAILURE;
  }
  port = atoi(argv[1]);

  tcpwindow = getenv("DIFX_TCP_MONITOR_WINDOWSIZE");
  if (tcpwindow != NULL) {
    tcp_windowsize = atoi(tcpwindow)*1024;
  }

  //open up the socket
  socketnumber = openstream(port, tcp_windowsize);
  if (socketnumber <=0) {
    cerr << "Error opening connection for mpifxcorr. Aborting" << endl;
    exit(1);
  }


  while(status > 0)
  {
    //receive the timestamp
    cout << "About to get a visibility" << endl;
    
    // Get atseconds
    status = readnetwork(socketnumber, (char*)(&timestampsec), sizeof(int32_t));
    if (status!=1) { // Error reading socket
      break;
    }

    //if not skipping this vis
    if(!(timestampsec < 0))
    {
      cout << "Got visibility # " << timestampsec << endl;
      
      // Get buffersize to follow
      status = readnetwork(socketnumber, (char*)(&thisbuffersize), sizeof(int32_t));
      if (status!=1) { // Error reading socket
	break;
      }

      // Get number of channels
      status = readnetwork(socketnumber, (char*)(&thisbuffersize), sizeof(int32_t));
      if (status!=1) { // Error reading socket
	break;
      }

      if (thisbuffersize>buffersize) {
	if (buffersize>0) 
	  delete resultbuffer;
	
	resultbuffer = vectorAlloc_cf32(thisbuffersize);
	buffersize = thisbuffersize;
      }

      //receive the results into a buffer
      status = readnetwork(socketnumber, (char*)resultbuffer, thisbuffersize*sizeof(cf32));
      if (status!=1) { // Error reading socket
	break;
      }
    }
  }

  //close the socket
  closestream(socketnumber);
}

int openstream(int portnumber, int tcpwindowsizebytes)
{
  int serversock, status, socketnumber;
  socklen_t client_len;
  struct sockaddr_in server, client;    /* Socket address */

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
    setsockopt(serversock, SOL_SOCKET, SO_SNDBUF,
	       (char *) &tcpwindowsizebytes, sizeof(tcpwindowsizebytes));
    if (status!=0) {
      cerr << "Error setting socket options" << endl;
      close(serversock);
      return(0);
    } 

    setsockopt(serversock, SOL_SOCKET, SO_RCVBUF,
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
  status = listen(serversock,1);
  if (status!=0) {
    cerr << "Error binding socket" << endl;
    close(serversock);
    return(0);
  }

  cout << "Waiting for connection" << endl;

  /* Accept connection */
  client_len = sizeof(client);
  socketnumber = accept(serversock, (struct sockaddr *)&client, &client_len);
  if (socketnumber == -1) {
    cerr << "Error connecting to client" << endl;
    close(serversock);
    return(-1);
  }

  cout << "Got a connection from " << inet_ntoa(client.sin_addr) << endl;
  return(socketnumber);
}

int readnetwork(int sock, char* ptr, int bytestoread)
{
  int nr;

  while (bytestoread>0)
  {
    nr = recv(sock,ptr,bytestoread,0);

    if (nr==-1) { // Error reading socket
      if (errno == EINTR) continue;
      cerr << "Error reading socket" << endl;
      return(-1);
    } else if (nr==0) {  // Socket closed remotely
      cerr << "Socket closed remotely" << endl;
      return(-1);
    } else {
      ptr += nr;
      bytestoread -= nr;
    }
  }
  return(1);
}

void closestream(int sock)
{
  //closes the socket
  int status;

  status = close(sock);
  if (status!=0) 
    cerr << "Error closing socket" << endl;
}


