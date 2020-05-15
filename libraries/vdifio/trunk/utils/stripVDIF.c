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
#include <getopt.h>
#include "vdifio.h"

const char program[] = "stripVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20200514";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to strip network headers from a VDIF format basebad data\n");
  fprintf(stderr, "file (e.g. captured from wireshark) and dump a pure VDIF stream.\n");
  fprintf(stderr, "Optional remove VDIF headers also\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> [<skipbytesfront> [<skipbytesback> [<skipbytesinitial>]]]\n\n", program);
  fprintf(stderr, "  <VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "  <VDIF output file> is the name of the VDIF file to read\n");
  fprintf(stderr, "  <VDIF output file> is the name of the VDIF file to read\n");
  fprintf(stderr, "  <skipbytesfront> is the number of bytes to skip over before each frame (optional, default 0)\n");
  fprintf(stderr, "  <skipbytesback> is the number of bytes to skip over after each frame (optional, default 0)\n");
  fprintf(stderr, "  <skipbytesinitial> is the number of bytes to skip over only once after opening the file (optional, default 0)\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "   -dropheader        Also remove VDIF header\n");
  fprintf(stderr, "   -front <N>         Alternate to set -skipbytesfront\n");
  fprintf(stderr, "   -back <N>          Alternate to set -skipbytesback\n");
  fprintf(stderr, "   -initial <N>       Alternate to set -skipbytesinitial\n");
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

  skipbytesfront   = 0;
  skipbytesback    = 0;
  skipbytesinitial = 0;
  
  int opt;
  struct option options[] = {
    {"dropheader", 0, 0, 'd'},  // Drop VDIF headers
    {"front", 1, 0, 'f'},       // Drop VDIF headers
    {"back", 1, 0, 'b'},        // Drop VDIF headers
    {"initial", 1, 0, 'i'},     // Drop VDIF headers
    {0, 0, 0, 0}
  };
  int dropHeaders = 0;
  
  while((opt = getopt_long_only(argc, argv, "hdf:b:i:", options, NULL)) != EOF) {
    switch (opt) {
    case 'd': // Strip VDIF headers entirely
      dropHeaders = 1;
      break;

    case 'f': // -front
      skipbytesfront = atoi(optarg);
      break;

    case 'b': // -back
      skipbytesback = atoi(optarg);
      break;

    case 'i': // -initial
      skipbytesinitial = atoi(optarg);
      break;
      
    case 'h': // help
      usage();
      return EXIT_SUCCESS;
    }
  }

  int narg = argc-optind;

  if(narg < 2 || narg > 5)
  {
    usage();
    return EXIT_FAILURE;
  }

  if(narg > 2)
    skipbytesfront = atoi(argv[optind + 2]);
  if(narg > 4)
    skipbytesback = atoi(argv[optind + 3]);
  if(narg > 5)
    skipbytesinitial = atoi(argv[optind + 4]);
  
  input = fopen(argv[optind], "r");
  if (input == NULL) {
    fprintf(stderr, "Cannot open input file %s\n", argv[optind]);
    exit(EXIT_FAILURE);
  }

  output = fopen(argv[optind+1], "w");
  if (output == NULL) {
    fprintf(stderr, "Cannot open output file %s\n", argv[optind+1]);
    exit(EXIT_FAILURE);
  }

  header = (vdif_header*)&buffer;
  framesread = 0;
  if(skipbytesinitial > 0)
    fseek(input, skipbytesinitial, SEEK_SET);
  //readbytes = fread(buffer, 1, skipbytesinitial, input); //skip some initial wireshark bytes

  int first = 1;
  while(!feof(input)) {
    if(skipbytesfront > 0) {
      readbytes = fseek(input, skipbytesfront, SEEK_CUR);
      //readbytes = fread(buffer, 1, skipbytesfront, input); //skip the leading network frame header
      if (readbytes!=0) {
	perror("Skipping bytes before frame\n");
	exit(EXIT_FAILURE);
      }
    }
    
    readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
    if (readbytes < VDIF_HEADER_BYTES)
      break;
    if (first) {
      first = 0;
      stationid = getVDIFStationID(header);
      bitspersample = getVDIFBitsPerSample(header);
      numchannels = getVDIFNumChannels(header);
      framebytes = getVDIFFrameBytes(header);
      fprintf(stdout, "Station ID is %d, bitspersample is %d, numchannels is %d, framebytes is %d\n", stationid, bitspersample, numchannels, framebytes);
    }

    if (framebytes > MAX_VDIF_FRAME_BYTES) {
      fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", framebytes, MAX_VDIF_FRAME_BYTES);
      break;
    }
    if (! dropHeaders) {
      readbytes = fwrite(buffer, 1, VDIF_HEADER_BYTES, output); //write out the VDIF header
      if (readbytes != VDIF_HEADER_BYTES) {
	fprintf(stderr, "Problem writing %lldth VDIF header - only wrote %d / %d bytes\n", framesread, readbytes, VDIF_HEADER_BYTES);
	break;
      }
    }

    readbytes = fread(buffer, 1, framebytes-VDIF_HEADER_BYTES, input); //read the VDIF data segment
    if (readbytes < framebytes-VDIF_HEADER_BYTES) {
      fprintf(stderr, "Problem reading %lldth frame - only got %d / %d bytes\n", framesread, readbytes, framebytes-VDIF_HEADER_BYTES);
	break;
    }
    readbytes = fwrite(buffer, 1, framebytes-VDIF_HEADER_BYTES, output); //write out the VDIF data segment
    if (readbytes < framebytes-VDIF_HEADER_BYTES) {
      fprintf(stderr, "Problem writing %lldth frame - only wrote %d / %d bytes\n", framesread, readbytes, framebytes-VDIF_HEADER_BYTES);
      break;
    }
    framesread++;

    if (skipbytesback > 0) {
      readbytes = fseek(input, skipbytesback, SEEK_CUR);
      if (readbytes!=0) {
	perror("Skipping bytes after frame\n");
	exit(EXIT_FAILURE);
      }
      //readbytes = fread(buffer, 1, skipbytesback, input); //skip the trailing network frame header
    }
  }
  printf("Read and wrote %lld frames\n", framesread);
  fclose(input);
  fclose(output);

  return EXIT_SUCCESS;
}
