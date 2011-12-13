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

const char program[] = "stripVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20100217";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to strip network headers from a VDIF format basebad data\n");
  fprintf(stderr, "file (e.g. captured from wireshark) and dump a pure VDIF stream.\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> [skipbytesfront] [skipbytesback] [skipbytesinitial]\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n[skipbytesfront=54] is the number of bytes to skip over before each frame\n");
  fprintf(stderr, "\n[skipbytesback=4] is the number of bytes to skip over after each frame\n");
  fprintf(stderr, "\n[skipbytesinitial=28] is the number of bytes to skip over only once after opening the file\n");
}

int main(int argc, char **argv)
{
  char buffer[MAX_VDIF_FRAME_BYTES];
  FILE * input;
  FILE * output;
  int skipbytesinitial, skipbytesfront, skipbytesback;
  int readbytes, framebytes, stationid, numchannels, bitspersample;
  long long framesread;
  vdif_header *header;

  if(argc < 3 || argc > 6)
  {
    usage();

    return EXIT_FAILURE;
  }

  skipbytesfront   = 54;
  skipbytesback    = 4;
  skipbytesinitial = 28;
  if(argc > 3)
    skipbytesfront = atoi(argv[3]);
  if(argc > 4)
    skipbytesback = atoi(argv[4]);
  if(argc > 5)
    skipbytesinitial = atoi(argv[5]);
  
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

  header = (vdif_header*)&buffer;
  framesread = 0;
  if(skipbytesinitial > 0)
    readbytes = fread(buffer, 1, skipbytesinitial, input); //skip some initial wireshark bytes
  while(!feof(input)) {
    if(skipbytesfront > 0)
      readbytes = fread(buffer, 1, skipbytesfront, input); //skip the leading network frame header
    readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
    if (readbytes < VDIF_HEADER_BYTES)
      break;
    stationid = getVDIFStationID(header);
    bitspersample = getVDIFBitsPerSample(header);
    numchannels = getVDIFNumChannels(header);
    framebytes = getVDIFFrameBytes(header);
    fprintf(stdout, "Station ID is %d, bitspersample is %d, numchannels is %d, framebytes is %d\n", stationid, bitspersample, numchannels, framebytes);
    if(framebytes > MAX_VDIF_FRAME_BYTES) {
      fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", framebytes, MAX_VDIF_FRAME_BYTES);
      break;
    }
    readbytes = fwrite(buffer, 1, VDIF_HEADER_BYTES, output); //write out the VDIF header
    if(readbytes != VDIF_HEADER_BYTES) {
      fprintf(stderr, "Problem writing %lldth VDIF header - only wrote %d / %d bytes\n", framesread, readbytes, VDIF_HEADER_BYTES);
      break;
    }
    readbytes = fread(buffer, 1, framebytes-VDIF_HEADER_BYTES, input); //read the VDIF data segment
    if(readbytes < framebytes-VDIF_HEADER_BYTES) {
      fprintf(stderr, "Problem reading %lldth frame - only got %d / %d bytes\n", framesread, readbytes, framebytes-VDIF_HEADER_BYTES);
      break;
    }
    readbytes = fwrite(buffer, 1, framebytes-VDIF_HEADER_BYTES, output); //write out the VDIF data segment
    if(readbytes < framebytes-VDIF_HEADER_BYTES) {
      fprintf(stderr, "Problem writing %lldth frame - only wrote %d / %d bytes\n", framesread, readbytes, framebytes-VDIF_HEADER_BYTES);
      break;
    }
    framesread++;
    if(skipbytesback > 0)
      readbytes = fread(buffer, 1, skipbytesback, input); //skip the trailing network frame header
  }

  printf("Read and wrote %lld frames\n", framesread);
  fclose(input);
  fclose(output);

  return EXIT_SUCCESS;
}
