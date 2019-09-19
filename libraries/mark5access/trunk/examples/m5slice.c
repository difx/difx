/***************************************************************************
 *   Copyright (C) 2012-2015 by Chris Phillips                             *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL:  $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifdef __APPLE__

#define OSX

#define OPENOPTIONS O_RDONLY

#else

#define LINUX
#define _LARGEFILE_SOURCE 
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#define OPENOPTIONS O_RDONLY|O_LARGEFILE

#endif

#define BUFSIZE 1 // Mbytes

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <time.h>
#include <math.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5slice";
const char author[]  = "Chris Phillips";
const char version[] = "0.2";
const char verdate[] = "20150312";

double printMJD(struct mark5_stream *ms);


static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 slicer.  Can slice VLBA, Mark3/4, Mark5B and VDIF"
		"formats using the mark5access library.\n\n");
	printf("Usage : %s <file> <dataformat> <offset> <length>\n\n", pgm);
	printf("  <file> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  alternatively for VDIF and CODIF, Mbps can be replaced by <FramesPerPeriod>m<AlignmentSeconds>, e.g.\n");
	printf("    VDIF_1000-64000m1-1-2 (8000 frames per 1 second, x1000 bytes x 8 bits= 64 Mbps)\n");
	printf("    CODIFC_5000-51200m27-8-1 (51200 frames every 27 seconds, x5000 bytes x 8 bits / 27  ~= 76 Mbps\n");
	printf("    This allows you to specify rates that are not an integer Mbps value, such as 32/27 CODIF oversampling\n\n");
	printf("  <offset> is the offset into the file in seconds\n\n");
	printf("  <length> size (in seconds) of the slice to make\n\n");
}

int main(int argc, char **argv) {
  int infile, outfile, bufsize;
  size_t readbytes, thisread, nread, nwrote;
  off_t offsetbytes;
  off_t sook;
  double offset, length, framens;
  char msg[512], *buf, *bptr, *outname, *inname, *dotptr, *baseptr;
  struct mark5_stream *ms;

  outname = NULL;

  // m5slice test.m5b Mark5B-512-8-2 <offset> <length>

  if(argc !=5 )	{
    usage(argv[0]);
    return EXIT_FAILURE;
  }

  inname = argv[1];

  offset = atof(argv[3]); // Seconds
  length = atof(argv[4]); // Seconds

  ms = new_mark5_stream_absorb(new_mark5_stream_file(inname, 0),
			       new_mark5_format_generic_from_string(argv[2]));
		
  if(!ms) {
    fprintf(stderr, "Error: problem opening or decoding %s\n", inname);
    return EXIT_FAILURE;
  }

  framens = ms->framens;

  offsetbytes = (off_t)(ms->frameoffset + offset/framens*1e9*ms->framebytes);

  readbytes = length/framens*1e9*ms->framebytes;

  delete_mark5_stream(ms);

  infile = open(inname, OPENOPTIONS);
  if (infile==-1) {
    sprintf(msg, "Failed to open input file (%s) [%d]", 
	    inname, errno);
    perror(msg);
    return EXIT_FAILURE;
  }

  if (offsetbytes>0) {
    sook = lseek(infile, offsetbytes, SEEK_SET);
    if (sook==-1 || sook!=offsetbytes) {
      if (sook==-1)
	perror("Skiping to offset\n");
      else
	fprintf(stderr, "Could not offset to right place in file\n");
      close(infile);
      free(outname);
      return EXIT_FAILURE;
    }
  }
  
  // Create output file name if not set
  if (outname==NULL) {

    baseptr = strrchr(inname, '/');
    if (baseptr==NULL) {
      baseptr = inname;
    } else {
      baseptr++; // Move past "/"
    }

    // File name contained in buffer
    dotptr = strrchr(baseptr, '.');

    outname = malloc(strlen(baseptr)+strlen("-slice")+1);
    if (outname==NULL) {
      close(infile);
      free(outname);
      return EXIT_FAILURE;
    }

    if (dotptr==NULL) {
      sprintf(outname, "%s-slice", baseptr);
    } else {
      *dotptr = 0;
      ++dotptr;
      sprintf(outname, "%s-slice.%s", baseptr, dotptr);
    }
  }

  outfile = creat(outname, S_IRUSR|S_IWUSR|S_IRGRP);
  if (outfile == -1) {
    fprintf(stderr, "Error creating output file \"%s\"\n", outname);
    perror(NULL);
    close(infile);
    return(0);
  }

  bufsize = BUFSIZE*1024;
  buf = malloc(bufsize);
  
  while (readbytes>0) {
    if (readbytes>bufsize) 
      thisread = bufsize;
    else
      thisread = readbytes;

    nread = read(infile, buf, thisread);

    if (nread==0) {
      printf("EOF detected before full slice read\n");
      close(infile);
      close(outfile);
      free(buf);
      free(outname);
      return EXIT_SUCCESS;

    } else if (nread==-1) {
      perror("Error reading file");
      close(outfile);
      close(infile);
      free(outname);
      free(buf);
      return EXIT_FAILURE;
    }
    readbytes -= nread;

    bptr = buf;
    while (nread>0) {
      nwrote = write(outfile, bptr, nread);
      if (nwrote==-1 || nwrote==0) {
	if (nwrote==-1)
	  perror("Error writing outfile");
	else 
	  fprintf(stderr, "Error writing outfile");
	close(outfile);
	close(infile);
	free(buf);
	free(outname);
	return EXIT_FAILURE;
      }
      nread -= nwrote;
    }
  }

  close(infile);
  close(outfile);
  free(outname);

  return EXIT_SUCCESS;
}









