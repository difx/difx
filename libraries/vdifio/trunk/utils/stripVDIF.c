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

int usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to strip network headers from a VDIF format basebad data\n");
  fprintf(stderr, "file (e.g. captured from wireshark) and dump a pure VDIF stream.\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> [skipbytes]\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n[skipbytes=54] is the number of bytes to skip over before each header\n");

  return 0;
}

int main(int argc, char **argv)
{
  char buffer[MAX_VDIF_FRAME_BYTES];
  FILE * input;
  FILE * output;
  int skipbytes, readbytes, framebytes;
  long long framesread;

  if(argc < 3 || argc > 4)
    return usage();

  skipbytes = 54;
  if(argc == 4)
    skipbytes = atoi(argv[3]);
  
  input = fopen(argv[1], "r");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open input file %s\n", argv[1]);
    exit(1);
  }

  output = fopen(argv[2], "w");
  if(output == NULL)
  {
    fprintf(stderr, "Cannot open output file %s\n", argv[2]);
    exit(1);
  }

  framesread = 0;
  while(!feof(input)) {
    readbytes = fread(buffer, 1, skipbytes, input); //skip the network frame header
    readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
    if (readbytes < VDIF_HEADER_BYTES)
      break;
    framebytes = getVDIFFrameBytes(buffer);
    if(framebytes > MAX_VDIF_FRAME_BYTES) {
      fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", framebytes, MAX_VDIF_FRAME_BYTES);
      break;
    }
    fwrite(buffer, 1, VDIF_HEADER_BYTES, output); //write out the VDIF header
    readbytes = fread(buffer, 1, framebytes, input); //read the VDIF data segment
    if(readbytes < framebytes) {
      fprintf(stderr, "Problem reading %dth frame\n", framesread);
      break;
    }
    readbytes = fwrite(buffer, 1, framebytes, output); //write out the VDIF data segment
    if(readbytes < framebytes) {
      fprintf(stderr, "Problem reading %dth frame\n", framesread);
      break;
    }
    framesread++;
  }

  printf("Read and wrote %lld frames\n", framesread);
  fclose(input);
  fclose(output);
}
