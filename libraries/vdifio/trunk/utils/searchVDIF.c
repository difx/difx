/***************************************************************************
 *   Copyright (C) 2010-2013 by Adam Deller                                *
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
 * $Id: stripVDIF.c 2006 2010-03-04 16:43:04Z AdamDeller $
 * $HeadURL:  $
 * $LastChangedRevision: 2006 $
 * $Author: AdamDeller $
 * $LastChangedDate: 2010-03-04 09:43:04 -0700 (Thu, 04 Mar 2010) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include "vdifio.h"

const char program[] = "searchVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20120103";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
  fprintf(stderr, "A program to dump some basic info about VDIF packets to the screen\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <bitshift> [expected framebytes]\n", program);
  fprintf(stderr, "\n  <VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n  <byteshift> is number of bytes to move along at a time while searching\n");
  fprintf(stderr, "\n  [expected framebytes] is the expected header-inclusive frame sizen\n\n");
}

int main(int argc, char **argv)
{
  char buffer[MAX_VDIF_FRAME_BYTES];
  FILE * input;
  int readbytes, framebytes, framemjd, framesecond, framenumber, frameinvalid, datambps, framespersecond;
  int packetdropped, expectedframebytes;
  off_t byteshift;
  long long bytecount, lastbytecount;

  if(argc != 3 && argc != 4)
  {
    usage();

    return EXIT_FAILURE;
  }

  expectedframebytes = -1;
  if(argc == 4)
  {
    expectedframebytes = atoi(argv[3]);
  }
  
  input = fopen(argv[1], "r");
  if(input == NULL)
  {
    fprintf(stderr, "Cannot open input file %s\n", argv[1]);
    exit(EXIT_FAILURE);
  }

  byteshift = atoi(argv[2]);
  if(byteshift < 32)
  {
    fprintf(stderr, "Error: byteshift must be larger than 32\n");
    exit(EXIT_FAILURE);
  }
  bytecount = 0;
  lastbytecount = 0;
  while(!feof(input)) {
    readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
    framebytes = getVDIFFrameBytes((vdif_header *)buffer);
    framemjd = getVDIFFrameMJD((vdif_header *)buffer);
    framesecond = getVDIFFrameSecond((vdif_header *)buffer);
    framenumber = getVDIFFrameNumber((vdif_header *)buffer);
    frameinvalid = getVDIFFrameInvalid((vdif_header *)buffer);
    if(expectedframebytes < 0 || framebytes == expectedframebytes)
    {
      printf("MJD is %d, second is %d, framenumber is %d, frameinvalid is %d\n", framemjd, framesecond, framenumber, frameinvalid);
    }
    if(framebytes == expectedframebytes)
    {
      printf("%d bytes since last good frame", (int)(bytecount-lastbytecount));
      lastbytecount = bytecount;
    }
    bytecount += byteshift;
    fseeko(input, byteshift-32, SEEK_CUR); //go back to the start
  }
  printf("Read %ll bytes\n", bytecount);
  fclose(input);

  return EXIT_SUCCESS;
}
