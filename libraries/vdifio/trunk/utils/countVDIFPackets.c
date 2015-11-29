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
#include "vdifio.h"

const char program[] = "countVDIFpackets";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20151122";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
  fprintf(stderr, "A program to count the number of missing packets for a given thread\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <Mbps> <theadId>\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<Mbps> is the data rate in Mbps expected for this file\n");
  fprintf(stderr, "\n<threadId> is the threadId to check for\n");
}

int main(int argc, char **argv)
{
  int VERBOSE = 0;
  char buffer[MAX_VDIF_FRAME_BYTES];
  FILE * input;
  int readbytes, framebytes, framemjd, framesecond, framenumber, datambps, framespersecond, targetThreadId;
  int nextmjd, nextsecond, nextnumber;
//  Following not seemingly used...  -WFB
//  int refmjd, refsecond, refnumber;
  long long framesread, framesmissed;
  vdif_header *header;

  if(argc != 4)
  {
    usage();

    return EXIT_FAILURE;
  }

  datambps = atoi(argv[2]);
  targetThreadId = atoi(argv[3]);
  
  input = fopen(argv[1], "r");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open input file %s\n", argv[1]);
    exit(EXIT_FAILURE);
  }

  readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
  if(readbytes < VDIF_HEADER_BYTES) {
    fprintf(stderr, "Header read failed: even first frame came up short.\n");
    fclose(input);
    exit(EXIT_FAILURE);
  }
  header = (vdif_header*)buffer;
  framebytes = getVDIFFrameBytes(header);
  if(framebytes > MAX_VDIF_FRAME_BYTES) {
    fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", framebytes, MAX_VDIF_FRAME_BYTES);
    fclose(input);
    exit(EXIT_FAILURE);
  }
  framespersecond = (int)((((long long)datambps)*1000000)/(8*(framebytes-VDIF_HEADER_BYTES)));
  printf("Frames per second is %d\n", framespersecond);
 
  rewind(input); //go back to the start

  framesread = 0;
  framesmissed = 0;
  while(!feof(input) && getVDIFThreadID(header) != targetThreadId) {
    readbytes = fread(buffer, 1, framebytes, input); //read the whole VDIF packet
    if (readbytes < framebytes) {
      fprintf(stderr, "Header read failed - probably at end of file.\n");
      break;
    }
  }
  
  header = (vdif_header*)buffer;
// Following not seemingly used... -WFB
//  refmjd = getVDIFFrameMJD(header);
//  refsecond = getVDIFFrameSecond(header);
//  refnumber = getVDIFFrameNumber(header);
  nextmjd = getVDIFFrameMJD(header);
  nextsecond = getVDIFFrameSecond(header);
  nextnumber = getVDIFFrameNumber(header);

  fseek(input, 0, SEEK_SET); //go back to the start again

  while(!feof(input)) {
    readbytes = fread(buffer, 1, framebytes, input); //read the whole VDIF packet
    if (readbytes < framebytes) {
      fprintf(stderr, "Header read failed - probably at end of file.\n");
      break;
    }
    header = (vdif_header*)buffer;
    if(getVDIFFrameBytes(header) != framebytes) {
      fprintf(stderr, "Framebytes has changed! Can't deal with this, aborting\n");
      break;
    }
    if(getVDIFThreadID(header) == targetThreadId) {
      framesread++;
      framemjd = getVDIFFrameMJD(header);
      framesecond = getVDIFFrameSecond(header);
      framenumber = getVDIFFrameNumber(header);
      //check for missing frames
      while(framemjd != nextmjd || framesecond != nextsecond || framenumber != nextnumber) {
        if(VERBOSE)
          fprintf(stderr, "Missed a packet! We were expecting MJD %d, sec %d, frame %d, and the next one came in MJD %d, sec %d, frame %d\n",
                  nextmjd, nextsecond, nextnumber, framemjd, framesecond, framenumber);
        framesmissed++;
        nextnumber++;
        if(nextnumber == framespersecond) {
          nextnumber = 0;
          nextsecond++;
          if(nextsecond == 86400) {
            nextsecond = 0;
            nextmjd++;
          }
        }
      }
      nextnumber++;
      if(nextnumber == framespersecond) {
        nextnumber = 0;
        nextsecond++;
	printf("For thread %d, at second %d, read %lld frames, spotted %lld missing frames\n", targetThreadId, nextsecond, framesread, framesmissed);
        if(nextsecond == 86400) {
          nextsecond = 0;
          nextmjd++;
        }
      }
    }
  }

  printf("\n** For thread %d, read %lld frames, spotted %lld missing frames\n", targetThreadId, framesread, framesmissed);
  fclose(input);

  return EXIT_SUCCESS;
}
