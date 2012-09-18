/***************************************************************************
 *   Copyright (C) 2010 by Adam Deller                                     *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL:  $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <pthread.h>
#include <string.h>
#include "vdifio.h"

const char program[] = "captureUDPVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20100319";

#define BUFFERSEGBYTES 1000000
#define NUMSEGMENTS    4

typedef struct {
  int keepwriting, ready, fillsegment;
  int udpframesize, framespersegment;
  char * filename;
  pthread_cond_t writeinitcond;
  char * receivebuffers[NUMSEGMENTS];
  pthread_mutex_t locks[NUMSEGMENTS];
} writethreaddata;

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to capture VDIF frames encapsulated in UDP frames from a network stream\n");
  fprintf(stderr, "A pure VDIF stream of packets is dumped to disk - optionally data is sniffed and written also.\n");
  fprintf(stderr, "\nUsage: %s <VDIF input port> <VDIF output file> [skipbytesfront] [skipbytesback]\n", program);
  fprintf(stderr, "\n<VDIF input port> is the port on which the frames will be coming in over (use 12002 for EVLA)\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to write\n");
  fprintf(stderr, "\n[skipbytesfront=0] is the number of bytes to skip over before each frame\n");
  fprintf(stderr, "\n[skipbytesback=0] is the number of bytes to skip over after each frame\n");
  fprintf(stderr, "\nNot sure if by default the skipbytesfront/back should be zeroed?\n");
}

void * launchNewWriteThread(void * w)
{
  FILE * output;
  writethreaddata * wtd = (writethreaddata*)w;
  int perr, writesegment, wrotebytes;

  //open the output file
  output = fopen(wtd->filename, "w");
  if(output == NULL)
  {
    fprintf(stderr, "Cannot open output file %s\n", wtd->filename);
    exit(EXIT_FAILURE);
  }

  //lock what we need
  perr = pthread_mutex_lock(&(wtd->locks[NUMSEGMENTS-1]));
  if(perr != 0) {
    fprintf(stderr, "Error doing initial mutex locking - aborting!\n");
    exit(EXIT_FAILURE);
  }
  wtd->ready = 1;
  perr = pthread_cond_signal(&(wtd->writeinitcond));
  if (perr != 0)
    fprintf(stderr, "Error trying to signal main thread to wake up!!!");
  writesegment = 0;  

  //loop through writing as it becomes available
  while(wtd->keepwriting || writesegment != wtd->fillsegment) {
    perr = pthread_mutex_lock(&(wtd->locks[writesegment]));
    if(perr != 0) {
      fprintf(stderr, "Write thread error locking segment %d - aborting!\n", writesegment);
      exit(EXIT_FAILURE);
    }
    perr = pthread_mutex_unlock(&(wtd->locks[(writesegment+NUMSEGMENTS-1)%NUMSEGMENTS]));
    if(perr != 0) {
      fprintf(stderr, "Write thread error unlocking segment %d - aborting!\n", 
              (writesegment+NUMSEGMENTS-1)%NUMSEGMENTS);
      exit(EXIT_FAILURE);
    }
    wrotebytes = fwrite(wtd->receivebuffers[writesegment], 1, wtd->udpframesize*wtd->framespersegment, output);
    if(wrotebytes < wtd->udpframesize*wtd->framespersegment) {
      fprintf(stderr, "Problem writing %dth segment - only wrote %d bytes\n", writesegment, wrotebytes);
      break;
    }
    writesegment = (writesegment+1)%NUMSEGMENTS;
  }

  //release last lock and close the output file
  fclose(output);

  return 0;
}

int main(int argc, char **argv)
{
  //UDP socket related variables
  int serversock, status, socketnumber, receivedbytes, udpbufbytes, udpport;
  int currentframes;
  int done;
  vdif_header *header;
  struct sockaddr_in server;    /* Socket address */
  struct msghdr msg;
  struct iovec iov[1];

  //non-UDP socket related variables
  int skipbytesfront, skipbytesback;
  int perr, i;
  writethreaddata wtd;
  pthread_t writethread;
  long long framesread;

  //check the command line arguments
  if(argc < 3 || argc > 5)
  {
    usage();

    return EXIT_FAILURE;
  }

  //store some variables, allocate some arrays
  skipbytesfront   = 0;
  skipbytesback    = 0;
  udpport          = atoi(argv[1]);
  wtd.filename     = argv[2];
  wtd.udpframesize = -1;
  pthread_cond_init(&(wtd.writeinitcond), NULL);
  for(i=0;i<NUMSEGMENTS;++i) {
    wtd.receivebuffers[i] = (char*)malloc(BUFFERSEGBYTES);
    pthread_mutex_init(&(wtd.locks[i]), NULL);
  }
  if(argc > 3)
    skipbytesfront = atoi(argv[3]);
  if(argc > 4)
    skipbytesback = atoi(argv[4]);

  //lock the first segment, launch the writer thread on the last segment
  perr = pthread_mutex_lock(&(wtd.locks[0]));
  if(perr != 0) {
    fprintf(stderr, "Error doing initial mutex locking - aborting!\n");
    exit(EXIT_FAILURE);
  }
  perr = pthread_mutex_lock(&(wtd.locks[1]));
  if(perr != 0) {
    fprintf(stderr, "Error doing initial mutex locking - aborting!\n");
    exit(EXIT_FAILURE);
  }
  wtd.keepwriting = 1;
  wtd.ready       = 0;
  wtd.filename    = argv[2];
  //launch the writer thread
  perr = pthread_create(&writethread, NULL, launchNewWriteThread, (void *)(&wtd));
  if(perr != 0) {
    fprintf(stderr, "Error in launching writethread!!!");
    exit(EXIT_FAILURE);
  }
  while(!wtd.ready) {
    perr = pthread_cond_wait(&(wtd.writeinitcond), &(wtd.locks[1]));
    if (perr != 0)
      fprintf(stderr, "Error waiting on writethreadstarted condition!!!!");
  }
  perr = pthread_mutex_unlock(&(wtd.locks[1]));
  if(perr != 0) {
    fprintf(stderr, "Error doing initial mutex unlocking - aborting!\n");
    exit(EXIT_FAILURE);
  }

  //open the UDP socket
  udpbufbytes = 32*1024*1024;
  memset((char *)&server,0,sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)udpport); /* Which port number to use */  
  serversock = socket(AF_INET,SOCK_DGRAM, IPPROTO_UDP); 
  if (serversock==-1) {
    fprintf(stderr, "Cannot create UDP socket to read VDIF packets!\n");
    exit(EXIT_FAILURE);
  }
  status = setsockopt(serversock, SOL_SOCKET, SO_RCVBUF, (char *) &udpbufbytes, sizeof(udpbufbytes));
  if (status!=0) {
    fprintf(stderr, "Cannot setsocket SO_RCVBUF socket\n");
    close(serversock);
    exit(EXIT_FAILURE);
  } 
  status = bind(serversock, (struct sockaddr *)&server, sizeof(server));
  if (status!=0) {
    fprintf(stderr, "Cannot bind UDP socket\n");
    close(serversock);
    exit(EXIT_FAILURE);
  } 
  socketnumber = serversock;
  done = 0;
  wtd.fillsegment = 0;
  currentframes = 0;
  wtd.udpframesize = 0;

  //loop through reading frames
  printf("Finished initialising socket etc - about to start reading data\n");
  framesread = 0;
  while(!done) {
    memset(&msg, 0, sizeof(msg));
    msg.msg_iov     = &iov[0];
    msg.msg_iovlen  = 1;
    iov[0].iov_base = wtd.receivebuffers[wtd.fillsegment] + currentframes*wtd.udpframesize;
    iov[0].iov_len  = BUFFERSEGBYTES - currentframes*wtd.udpframesize;

    receivedbytes = recvmsg(socketnumber,&msg,MSG_WAITALL);
    if (receivedbytes==-1) { // Error reading socket
      if (errno == EINTR)
        continue;
      fprintf(stderr, "Some problem reading socket - aborting\n");
      done = 1;
      continue;
    } else if (receivedbytes==0) {  // Socket closed remotely
      fprintf(stderr, "Remote socket closed - aborting\n");
      done=1;
      continue;
    } else if (receivedbytes!=wtd.udpframesize) {
      if(wtd.udpframesize != 0) { //not the first packet - something bad happened
        fprintf(stderr, "Received %d bytes vs framesize %d - aborting\n", receivedbytes, wtd.udpframesize);
        done = 1;
        continue;
      }
      else { //was the first time - set framebytes
        wtd.udpframesize = receivedbytes;
        wtd.framespersegment = BUFFERSEGBYTES/wtd.udpframesize;
	header = (vdif_header*)(wtd.receivebuffers[wtd.fillsegment] + wtd.udpframesize*currentframes);
        printf("Setting framebytes to %d, framespersegment to %d\n", wtd.udpframesize, wtd.framespersegment);
        printf("Starting from front, framebytes is %d, MJD is %d, seconds is %d, num channels is %d\n", 
               getVDIFFrameBytes(header),
               getVDIFFrameMJD(header),
               getVDIFFrameSecond(header),
               getVDIFNumChannels(header));
	header = (vdif_header*)((char*)header+8);
        printf("Starting from 8 bytes in, framebytes is %d, MJD is %d, seconds is %d, num channels is %d\n",
               getVDIFFrameBytes(header),
               getVDIFFrameMJD(header),
               getVDIFFrameSecond(header),
               getVDIFNumChannels(header));
	printf("The hex value of that first byte is %llx\n", 
               *((unsigned long long*)(wtd.receivebuffers[wtd.fillsegment] + wtd.udpframesize*currentframes)));
      }
    }
    //if we get here its ok
    if(skipbytesfront != 0) //move the stuff in memory if needed
      memmove(wtd.receivebuffers[wtd.fillsegment] + wtd.udpframesize*currentframes, 
              wtd.receivebuffers[wtd.fillsegment] + wtd.udpframesize*currentframes + skipbytesfront,
              wtd.udpframesize - skipbytesfront - skipbytesback);
    ++currentframes;
    ++framesread;
    if(currentframes == wtd.framespersegment) {
      perr = pthread_mutex_lock(&(wtd.locks[(wtd.fillsegment+1)%NUMSEGMENTS]));
      if(perr != 0) {
        fprintf(stderr, "Main thread error locking segment %d - aborting!\n", (wtd.fillsegment+1)%NUMSEGMENTS);
        wtd.keepwriting = 0;
        done = 1;
      }
      perr = pthread_mutex_unlock(&(wtd.locks[wtd.fillsegment]));
      if(perr != 0) {
        fprintf(stderr, "Main thread error unlocking segment %d - aborting!\n", wtd.fillsegment);
        wtd.keepwriting = 0;
        done = 1;
      }
      wtd.fillsegment = (wtd.fillsegment+1)%NUMSEGMENTS;
      currentframes = 0;
    }
  }

  pthread_mutex_unlock(&(wtd.locks[wtd.fillsegment]));
  printf("Read and wrote %lld frames\n", framesread);
  perr = pthread_join(writethread, NULL);
  if(perr != 0)
    fprintf(stderr, "Error in joining writethread!!!");

    return EXIT_SUCCESS;
}
