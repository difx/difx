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

const char program[] = "printVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20100217";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to dump some basic info about VDIF packets to the screen\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <Mbps>\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<Mbps> is the data rate in Mbps expected for this file\n");
}

int main(int argc, char **argv)
{
  const int MaxFrameSize = 16*MAX_VDIF_FRAME_BYTES;
  char buffer[MaxFrameSize];
  FILE * input;
  int readbytes, framebytes, framemjd, framesecond, framenumber, frameinvalid, datambps, framespersecond;
  long long framesread;
  vdif_header *header;

  if(argc != 3)
  {
    usage();

    return EXIT_FAILURE;
  }
  
  input = fopen(argv[1], "r");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open input file %s\n", argv[1]);
    exit(EXIT_FAILURE);
  }

  datambps = atoi(argv[2]);
  readbytes = fread(buffer, 1, MaxFrameSize, input); //read the VDIF header
  header = (vdif_header*)buffer;
  framebytes = getVDIFFrameBytes(header);
  if(framebytes > MaxFrameSize) {
    fprintf(stderr, "Cannot read frame with %d bytes > max (%d), formal max %d\n", framebytes, MaxFrameSize, MAX_VDIF_FRAME_BYTES);
    exit(EXIT_FAILURE);
  }
  framemjd = getVDIFFrameMJD(header);
  framesecond = getVDIFFrameSecond(header);
  framenumber = getVDIFFrameNumber(header);
  frameinvalid = getVDIFFrameInvalid(header);
  framespersecond = (int)((((long long)datambps)*1000000)/(8*(framebytes-VDIF_HEADER_BYTES)));
  printf("Frames per second is %d\n", framespersecond);
 
  rewind(input); //go back to the start

  framesread = 0;
  while(!feof(input)) {
    readbytes = fread(buffer, 1, framebytes, input); //read the whole VDIF packet
    if (readbytes < framebytes) {
      fprintf(stderr, "Header read failed - probably at end of file.\n");
      break;
    }
    header = (vdif_header*)buffer;
    framemjd = getVDIFFrameMJD(header);
    framesecond = getVDIFFrameSecond(header);
    framenumber = getVDIFFrameNumber(header);
    frameinvalid = getVDIFFrameInvalid(header);
    printf("MJD is %d, second is %d, framenumber is %d, frameinvalid is %d\n", framemjd, framesecond, framenumber, frameinvalid);
    printf("Threadid is %d, stationid is %d, numbits is %d\n", getVDIFThreadID(header), getVDIFStationID(header), getVDIFBitsPerSample(header));
    if(getVDIFFrameBytes(header) != framebytes) { 
      fprintf(stderr, "Framebytes has changed! Can't deal with this, aborting\n");
      break;
    }
    framesread++;
  }

  printf("Read %lld frames\n", framesread);
  fclose(input);

  return EXIT_SUCCESS;
}
