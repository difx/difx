/***************************************************************************
 *   Copyright (C) 2024 by Adam Deller                                     *
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


#include <stdio.h>
#include <stdlib.h>
#include "vdifio.h"

const char program[] = "invalidateVDIFPackets";
const char author[]  = "Adam Deller <adeller@swin.edu.au>";
const char version[] = "0.1";
const char verdate[] = "20240118";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to set some fraction of VDIF packets to invalid\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> <Mbps> <invalidfraction>\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to write\n");
  fprintf(stderr, "\n<Mbps> is the data rate in Mbps expected for this file\n");
  fprintf(stderr, "\n<invalidfraction> is the fraction of the packets to set to invalid\n");
}

int main(int argc, char **argv)
{
  int VERBOSE = 0;
  int MODULATION_PERIOD = 100;
  char buffer[MAX_VDIF_FRAME_BYTES];
  FILE * input;
  FILE * output;
  int readbytes, framebytes, framemjd, framesecond, framenumber, frameinvalid, datambps, framespersecond;
  int nextmjd, nextsecond, nextnumber;
  int numinvalidframes, readvalidframes, readinvalidframes, wrotevalidframes, wroteinvalidframes;
  float invalidfraction = 0.0;
  long long framesread, frameswrote;
  vdif_header *header;

  if(argc != 5)
  {
    usage();

    return EXIT_FAILURE;
  }

  invalidfraction = atof(argv[4]);
  numinvalidframes = (int)(invalidfraction*MODULATION_PERIOD);
  readvalidframes = 0;
  readinvalidframes = 0;
  wrotevalidframes = 0;
  wroteinvalidframes = 0;
  
  input = fopen(argv[1], "r");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open input file %s\n", argv[1]);
    exit(EXIT_FAILURE);
  }

  output = fopen(argv[2], "w");
  if(output == NULL)
  {
    fprintf(stderr, "Cannot open output file %s\n", argv[2]);
    exit(EXIT_FAILURE);
  }

  datambps = atoi(argv[3]);
  readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
  header = (vdif_header*)buffer;
  framebytes = getVDIFFrameBytes(header);
  if(framebytes > MAX_VDIF_FRAME_BYTES) {
    fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", framebytes, MAX_VDIF_FRAME_BYTES);
    exit(EXIT_FAILURE);
  }
  nextmjd = getVDIFFrameMJD(header);
  nextsecond = getVDIFFrameSecond(header);
  nextnumber = getVDIFFrameNumber(header);
  framespersecond = (int)((((long long)datambps)*1000000)/(8*(framebytes-VDIF_HEADER_BYTES)));
  printf("Frames per second is %d\n", framespersecond);
  if(nextnumber >= framespersecond) {
    fprintf(stderr, "Encountered illegal frame number %d, exceeds fps-1=%d\n", nextnumber, framespersecond-1);
    exit(EXIT_FAILURE);
  }
 
  rewind(input); //go back to the start

  framesread = 0;
  frameswrote = 0;
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
    if(frameinvalid)
      readinvalidframes += 1;
    else
      readvalidframes += 1;
    if(framenumber%MODULATION_PERIOD < numinvalidframes) {
      frameinvalid = 1;
    }
    else {
      frameinvalid = 0;
    }
    if(getVDIFFrameBytes(header) != framebytes) { 
      fprintf(stderr, "Framebytes has changed! Can't deal with this, aborting\n");
      break;
    }
    if (framenumber >= framespersecond) {
      fprintf(stderr, "Encountered illegal frame number %d, exceeds fps-1=%d, will ignore and try to continue.\n", framenumber, framespersecond-1);
      continue;
    }
    framesread++;
    if(frameinvalid)
      wroteinvalidframes += 1;
    else
      wrotevalidframes += 1;
    setVDIFFrameInvalid(header, frameinvalid);
    readbytes = fwrite(buffer, 1, framebytes, output); //write out the VDIF packet
    if(readbytes < framebytes) {
      fprintf(stderr, "Problem writing %lldth frame - only wrote %d bytes\n", framesread, readbytes);
      break;
    }
    frameswrote++;
  }

  printf("Read %lld and wrote %lld frames\n", framesread, frameswrote);
  printf("In the input, there were %lld valid and %lld invalid frames\n", readvalidframes, readinvalidframes);
  printf("In the output, there were %lld valid and %lld invalid frames\n", wrotevalidframes, wroteinvalidframes);
  fclose(input);
  fclose(output);

  return EXIT_SUCCESS;
}
