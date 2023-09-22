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
#include <string.h>
#include <stdlib.h>
#include "vdifio.h"

const char program[] = "extractVDIFThreads";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20130515";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to get a subset of the threads in a VDIF file\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> <Mbps> <threadids> [-v]\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read and clean\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to write\n");
  fprintf(stderr, "\n<Mbps> is the data rate in Mbps expected for this file\n");
  fprintf(stderr, "\n<threadids> is a comma separated list of thread ids to copy\n");
  fprintf(stderr, "\n[-v] verbose mode on\n");
  fprintf(stderr, "The input file must at least start with one valid packet\n");
}

int main(int argc, char **argv)
{
  char buffer[MAX_VDIF_FRAME_BYTES*2];
  char *writebuffer;
  char * pch;
  int threadids[VDIF_MAX_THREAD_ID+1];
  int writefull[VDIF_MAX_THREAD_ID+1];
  FILE * input;
  FILE * output;
  int readbytes, framebytes, datambps, framespersecond;
  int bufferoffset, wrotebytes, wholemissedpackets, extrareadbytes;
  int i, j, verbose, numthreads, tempframenum, tempframesec, currentframenum, currentframesec;
  long long framesread, invalidpackets, invalidbytes;
  vdif_header *header;

  if(argc != 5 && argc != 6)
  {
    usage();
    return EXIT_FAILURE;
  }

  if(argc == 5)
    verbose = 1;
  else
    verbose = 0;


  writebuffer = malloc(MAX_VDIF_FRAME_BYTES*(VDIF_MAX_THREAD_ID+1));
  if (writebuffer==NULL) {
    fprintf(stderr, "Could not allocate %d kBytes\n", MAX_VDIF_FRAME_BYTES*(VDIF_MAX_THREAD_ID+1)/1024);
    exit(EXIT_FAILURE);
  }

  input = fopen(argv[1], "r");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open input file %s\n", argv[1]);
    exit(EXIT_FAILURE);
  }
  output = fopen(argv[2], "w");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open output file %s\n", argv[2]);
    exit(EXIT_FAILURE);
  }

  datambps = atoi(argv[3]);
  strcpy(buffer, argv[4]);
  printf("Thread ID string: %s\n", argv[4]);
  pch = strtok (buffer,",");
  numthreads = 0;
  while (pch != NULL)
  {
    writefull[numthreads] = 0;
    threadids[numthreads++] = atoi(pch);
    pch = strtok (NULL, ",");
  }
  printf("You will be extracting %d threads\n", numthreads);
  invalidpackets = 0;
  invalidbytes = 0;
  readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
  header = (vdif_header*)buffer;
  framebytes = getVDIFFrameBytes(header);
  if(framebytes > MAX_VDIF_FRAME_BYTES) {
    fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", framebytes, MAX_VDIF_FRAME_BYTES);
    exit(EXIT_FAILURE);
  }
  framespersecond = (int)((((long long)datambps)*1000000LL)/(8LL*(framebytes-VDIF_HEADER_BYTES)));
  printf("Frames per second is %d\n", framespersecond);
  printf("datambps=%d framebytes=%d\n", datambps, framebytes);
  currentframenum = getVDIFFrameNumber(header);
  currentframesec = getVDIFFrameSecond(header);
  rewind(input); //go back to the start

  framesread = 0;
  while(!feof(input)) {
    bufferoffset = 0;
    wholemissedpackets = 0;
    extrareadbytes = 0;
    readbytes = fread(buffer, 1, framebytes, input); //read the whole VDIF packet
    if (readbytes < framebytes) {
      fprintf(stderr, "Header read failed - probably at end of file.\n");
      break;
    }

    header = (vdif_header*)(buffer+bufferoffset);
    while(getVDIFFrameBytes(header) != framebytes) {
      //almost certainly some bogus packet.  Try looking ahead
      if(verbose)
        fprintf(stderr, "Lost the VDIF stream - skipping ahead to look for next packet\n");
      bufferoffset += 8;
      if(bufferoffset == framebytes) {
        for(i=0;i<extrareadbytes;i++)
          buffer[i] = buffer[framebytes+i];
        readbytes = fread(buffer, 1, framebytes-extrareadbytes, input); //read the whole VDIF packet
        if (readbytes < framebytes-extrareadbytes) {
          fprintf(stderr, "Header read failed - probably at end of file.\n");
          invalidpackets++;
          invalidbytes += bufferoffset + wholemissedpackets*framebytes;
          break;
        }
        bufferoffset = 0;
        extrareadbytes = 0;
        wholemissedpackets++;
      }
      if(framebytes - bufferoffset < VDIF_HEADER_BYTES) {
        readbytes = fread(buffer + VDIF_HEADER_BYTES + bufferoffset, 1, 8, input);
        extrareadbytes += 8;
      }
    }
    if(bufferoffset > 0 || wholemissedpackets > 0) {
      invalidpackets++;
      invalidbytes += bufferoffset + wholemissedpackets*framebytes;
    }
    if(bufferoffset > 0) {
      readbytes = fread(buffer+framebytes+extrareadbytes, 1, bufferoffset-extrareadbytes, input);
      if(readbytes < bufferoffset-extrareadbytes) {
        fprintf(stderr, "Header read failed - probably at end of file.\n");
        break;
      }
    }
    header = (vdif_header*)(buffer+bufferoffset);
    if(getVDIFFrameBytes(header) != framebytes) {
      fprintf(stderr, "Framebytes has changed (%d)! Can't deal with this, aborting\n", getVDIFFrameBytes(header));
      fprintf(stderr, "Bufferoffset was %d, wholemissedpackets was %d\n", bufferoffset, wholemissedpackets);
      break;
    }
    //check that it matches the expected time
    tempframenum = getVDIFFrameNumber(header);
    tempframesec = getVDIFFrameSecond(header);
    while(tempframenum != currentframenum || tempframesec != currentframesec){
      for(j=0;j<numthreads;j++) {
        setVDIFFrameInvalid((vdif_header*)(writebuffer + j*framebytes), 1);
	setVDIFThreadID((vdif_header*)(writebuffer + j*framebytes), threadids[j]);
	setVDIFFrameNumber((vdif_header*)(writebuffer + j*framebytes), currentframenum);
	setVDIFFrameSecond((vdif_header*)(writebuffer + j*framebytes), currentframesec);
      }
      fprintf(stderr, "Printing an invalid frame for time %d, %d (looking at a frame from %d, %d)\n", currentframesec, currentframenum, tempframesec, tempframenum);
      wrotebytes = fwrite(writebuffer, 1, numthreads*framebytes, output);
      if(wrotebytes != framebytes*numthreads)
        fprintf(stderr, "Write failed!\n");
      for(j=0;j<numthreads;j++)
        writefull[j] = 0;
      currentframenum += 1;
      if(currentframenum == framespersecond) {
        currentframenum = 0;
	currentframesec += 1;
        if(currentframesec == 86400)
	  currentframesec = 0;
      }
    }
    framesread++;
    //try to copy to writebuffer
    for(i=0;i<numthreads;i++) {
      if(threadids[i] == getVDIFThreadID(header)) {
        if(writefull[i]) {
	  fprintf(stderr, "Buffer for this thread already full!\n");
	  fprintf(stderr, "Thread id was %d, time is %d\n", threadids[i], getVDIFFrameNumber(header));
	  //before we go further, write out invalid tag data for all threads for the current time
	  tempframenum = getVDIFFrameNumber((vdif_header*)(writebuffer + i*framebytes));
	  tempframesec = getVDIFFrameSecond((vdif_header*)(writebuffer + i*framebytes));
          for(j=0;j<numthreads;j++) {
	    setVDIFFrameInvalid((vdif_header*)(writebuffer + j*framebytes), 1);
	    setVDIFThreadID((vdif_header*)(writebuffer + j*framebytes), threadids[j]);
	    setVDIFFrameNumber((vdif_header*)(writebuffer + j*framebytes), tempframenum);
	    setVDIFFrameSecond((vdif_header*)(writebuffer + j*framebytes), tempframesec);
	  }
	  wrotebytes = fwrite(writebuffer, 1, numthreads*framebytes, output);
	  if(wrotebytes != framebytes*numthreads)
	    fprintf(stderr, "Write failed!\n");
          for(j=0;j<numthreads;j++)
	    writefull[j] = 0;
	}
	memcpy(writebuffer + i*framebytes, buffer+bufferoffset, framebytes);
	writefull[i] = 1;
      }
    }
    //check if writebuffer full
    int full = 1;
    for(i=0;i<numthreads;i++) {
      if(!writefull[i])
        full = 0;
    }
    if(full) {
      wrotebytes = fwrite(writebuffer, 1, numthreads*framebytes, output);
      if(wrotebytes != framebytes*numthreads)
        fprintf(stderr, "Write failed!\n");
      for(i=0;i<numthreads;i++)
	writefull[i] = 0;
      currentframenum += 1;
      if(currentframenum == framespersecond) {
        currentframenum = 0;
	currentframesec += 1;
	if(currentframesec == 86400)
	  currentframesec = 0;
      }
    }
  }

  printf("Read %lld frames, skipped over %lld dodgy packets containing %lld dodgy bytes\n", framesread, invalidpackets, invalidbytes);
  fclose(input);
  fclose(output);

  return EXIT_SUCCESS;
}

