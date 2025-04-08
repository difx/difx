/***************************************************************************
 *  Copyright (C) 2018-2022 by Chris Phillips                              *
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

/**********************************************************************************************

  codif_write

  Read UDP packets from a CODIF stream and write resulting data to local file

 **********************************************************************************************/


#ifdef __APPLE__
#define OSX
#define OPENOPTIONS O_WRONLY|O_CREAT|O_TRUNC
#ifdef SWAPENDIAN
#include <libkern/OSByteOrder.h>
#define byteswap_16(x) OSSwapInt16(x)
#define byteswap_32(x) OSSwapInt32(x)
#define byteswap_64(x) OSSwapInt64(x)
#else
#define byteswap_16(x) x
#define byteswap_32(x) x
#define byteswap_64(x) x
#endif
#else
#define LINUX
#define _LARGEFILE_SOURCE 
#define _LARGEFILE64_SOURCE
#define OPENOPTIONS O_WRONLY|O_CREAT|O_TRUNC|O_LARGEFILE|O_EXCL
#ifdef SWAPENDIAN
#include <byteswap.h>
#define byteswap_16(x) bswap_16(x)
#define byteswap_32(x) bswap_32(x)
#define byteswap_64(x) bswap_64(x)
#else
#define byteswap_16(x) x
#define byteswap_32(x) x
#define byteswap_64(x) x
#endif
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/types.h>  
#include <sys/socket.h>  
#include <sys/stat.h>  
#include <sys/time.h>  
#include <string.h>
#include <signal.h>
#include <getopt.h>
#include <time.h>
#include <netdb.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <fcntl.h>
#include <stdbool.h>

#include <codifio.h>

#define MAXSTR              200 
#define BUFHEADERS          20000
#define DEFAULT_PORT        52100
#define DEFAULT_TIME        60
#define DEFAULT_FILESIZE    60
#define DEFAULT_UPDATETIME  1.0
#define UPDATE              20

#define DEBUG(x) 

double tim(void);
void alarm_signal (int sig);

volatile int time_to_quit = 0;
volatile int sig_received = 0;
unsigned int total_packets = 0;

int setup_net(unsigned short port, const char *ip, const char *group, int *sock, float bufsize);

/* Globals needed by alarm handler */

//unsigned long minpkt_size, maxpkt_size, pkt_drop, pkt_oo;
//uint64_t sum_pkts, pkt_head, npacket, skipped;
double t0, t1, t2;

int main (int argc, char * const argv[]) {
  char *buf, *ptr, timestr[MAXSTR];
  int threadIndex, fileIndex, tmp, opt, status, sock, skip, i, nfile;
  int valid, period, thisthread = -1, thisgroup = -1;
  ssize_t nread, nwrote, nwrite;
  char msg[MAXSTR];
  float updatetime, ftmp;
  struct itimerval timeval;
  sigset_t set;
  time_t filetime=0;
  codif_header *cheader=NULL;
  int16_t *s16, *cdata;
  int64_t *i64, *o64;
  int8_t *s8;
  uint32_t lastseconds, lastframe, thisframe, thisseconds, framesperperiod;

  unsigned int port = DEFAULT_PORT;
  char *ip = NULL;
  char *multicastGroup = NULL;
  char *filename = NULL;
  int time = DEFAULT_TIME;
  int verbose = 0;
  int bufsize = 100;
  
  struct option options[] = {
    {"port", 1, 0, 'p'},
    {"ip", 1, 0, 'i'},
    {"multicast", 1, 0, 'm'},
    {"outfile", 1, 0, 'o'},
    {"time", 1, 0, 't'},
    {"updatetime", 1, 0, 'u'},
    {"verbose", 0, 0, 'V'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  updatetime = DEFAULT_UPDATETIME;

  while (1) {
    opt = getopt_long_only(argc, argv, "i:T:P:p:t:hwg:GH", options, NULL);
    if (opt==EOF) break;

    switch (opt) {
     
     case 'p':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1 || tmp<=0 || tmp>USHRT_MAX)
	fprintf(stderr, "Bad port option %s\n", optarg);
      else 
	port = tmp;
     break;

    case 't':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1 || tmp<=0)
	fprintf(stderr, "Bad time option %s\n", optarg);
      else 
	time = tmp;
     break;

    case 'u':
      status = sscanf(optarg, "%f", &ftmp);
      if (status!=1 || ftmp<=0.0)
	fprintf(stderr, "Bad updatetime option %s\n", optarg);
      else 
	updatetime = ftmp;
     break;

    case 'i':
      ip = strdup(optarg);
      break;

    case 'm':
      multicastGroup = strdup(optarg);
      break;
      
    case 'o':
      filename = strdup(optarg);
      break;

    case 'V':
      verbose = 1;
      break;
      
    case 'h':
      printf("Usage: udp_write [options]\n");
      printf("  -p/-port <PORT>        Port to use\n");
      printf("  -P/-padding <N>        Discard N bytes at end of packet\n");
      printf("  -i/-ip <IP>            Receive data from host IP\n");
      printf("  -m/-multicast <G.G.G.G>  Receive data oin muticast group G.G.G.G\n");
      printf("  -o/-outfile <NAME>     output filename\n");
      printf("  -t/-time <N>           Record for N seconds\n");
      printf("  -V/-verbose            Verbose output/n");
      printf("  -h/-help               This list\n");
      return(1);
    break;
    
    case '?':
    default:
      break;
    }
  }

  if (!filename) filename=strdup("test.chead");

  buf = malloc(BUFHEADERS*CODIF_HEADER_BYTES);
  if (buf==NULL) {
    sprintf(msg, "Trying to allocate %d bytes", BUFHEADERS*CODIF_HEADER_BYTES);
    perror(msg);
    return(1);
  }
  ptr = buf;
  
  status = setup_net(port, ip, multicastGroup, &sock, bufsize);
    
  if (status) exit(1);

  int ofile = open(filename, OPENOPTIONS,S_IRWXU|S_IRWXG|S_IRWXO); 
  if (ofile==-1) {
    sprintf(msg, "Failed to open output file (%s)", filename);
    perror(msg);
  }
  
  /* Install an alarm catcher */
  signal(SIGALRM, alarm_signal);
  sigemptyset(&set);
  sigaddset(&set, SIGALRM);

  timeval.it_interval.tv_sec = updatetime; 
  timeval.it_interval.tv_usec = 0;
  timeval.it_value.tv_sec = updatetime;
  timeval.it_value.tv_usec = 0;
  t0 = t1 = t2 = tim();
  setitimer(ITIMER_REAL, &timeval, NULL);

  int first = true;
  int npacket = 0;
  while (t2-t0<time) {
    nread = recvfrom(sock, ptr, CODIF_HEADER_BYTES, MSG_WAITALL, 0, 0);
    if (nread==-1) {
      perror("Receiving packet");
      exit(1);
    } else if (nread==0) {
      fprintf(stderr, "Error: Did not read any bytes!\n");
      exit(1);
    } else if (nread!=CODIF_HEADER_BYTES) {
      fprintf(stderr, "Error: Packetsize larger than expected!\n");
      exit(1);
    }

    // Block alarm signal while we are updating these values
    status = sigprocmask(SIG_BLOCK, &set, NULL);
    if (status) {
      perror(": Trying to block SIGALRM\n");
      exit(1);
    }
    total_packets++;
    npacket++;
    ptr += CODIF_HEADER_BYTES;
    
    // Unblock the signal again
    status = sigprocmask(SIG_UNBLOCK, &set, NULL);
    if (status) {
      perror(": Trying to block SIGALRM\n");
      exit(1);
    }

    if (npacket>=BUFHEADERS) {
      nwrite = npacket*CODIF_HEADER_BYTES;
      nwrote = write(ofile, buf, nwrite);
      if (nwrote==-1) {
	perror("Error writing outfile");
	for (i=0;i<nfile;i++) close(ofile);
	exit(1);
      } else if (nwrote!=nwrite) {
	fprintf(stderr, "Warning: Did not write all bytes! (%zd/%zd)\n", nwrote, nwrite);
      }
      npacket = 0;
      ptr = buf;
    }
  }
  
  if (npacket>1) {
    nwrite = npacket*CODIF_HEADER_BYTES;
    nwrote = write(ofile, buf, nwrite);
    if (nwrote==-1) {
      perror("Error writing outfile");
      for (i=0;i<nfile;i++) close(ofile);
      exit(1);
    } else if (nwrote!=nwrite) {
      fprintf(stderr, "Warning: Did not write all bytes! (%zd/%zd)\n", nwrote, nwrite);
    }
  }  

  free(buf);
  close(ofile);

  return(0);
}
  
int setup_net(unsigned short port, const char *ip, const char *group, int *sock, float bufsize) {
  int status;
  struct sockaddr_in server; 

  /* Initialise server's address */
  memset((char *)&server,0,sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons(port);

  if (ip==NULL || group!=NULL) 
    server.sin_addr.s_addr = htonl(INADDR_ANY); /* Anyone can connect */
  else {
    server.sin_addr.s_addr = inet_addr(ip);
    if (server.sin_addr.s_addr==INADDR_NONE) {
      fprintf(stderr, "Error parsing IP address %s. Exitting\n", ip);
      return(1);
    }
  }

  /* Create socket */
  *sock = socket(AF_INET,SOCK_DGRAM, IPPROTO_UDP); 
  if (*sock==-1) {
    perror("Error creating socket");
    return(1);
  }

  if (bufsize<=0) bufsize = 10;
  int udpbufbytes = bufsize*1024*1024;
  status = setsockopt(*sock, SOL_SOCKET, SO_RCVBUF, (char *) &udpbufbytes, sizeof(udpbufbytes));
  if (status!=0) {
    fprintf(stderr, "Warning: Could not set socket RCVBUF\n");
  }

  status = bind(*sock, (struct sockaddr *)&server, sizeof(server));
  if (status!=0) {
    perror("Error binding socket");
    close(*sock);
    return(1);
  }

  if (group!=NULL) {
    struct ip_mreqn mreq;
    mreq.imr_multiaddr.s_addr = inet_addr(group);

    if (ip==NULL) {
      mreq.imr_address.s_addr = htonl(INADDR_ANY);
    } else {
      printf("Using %s\n", ip);
      mreq.imr_address.s_addr = inet_addr(ip);
    }
    mreq.imr_ifindex = 0;
    status = setsockopt(*sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq));
    if (status <0) {
      printf("%d\n", errno);
      perror("setsockopt mreq");
      return(1);
    }
  }
  
  return(0);
}
  
double tim(void) {
  struct timeval tv;
  double t;

  gettimeofday(&tv, NULL);
  t = (double)tv.tv_sec + (double)tv.tv_usec/1000000.0;

  return t;
}

void alarm_signal (int sig) {
  t2 = tim();

  printf("Read %d packets\n", total_packets);
  total_packets = 0;

  t1 = t2;
  return;
}  
