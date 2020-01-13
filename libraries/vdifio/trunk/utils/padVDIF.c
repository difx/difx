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

const char program[] = "padVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20100217";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to insert dummy packets for any missing VDIF packets\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> <Mbps> [new start MJD]\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the VDIF file to write\n");
  fprintf(stderr, "\n<Mbps> is the data rate in Mbps expected for this file\n");
  fprintf(stderr, "\n[new start MJD] is the MJD (with fractional component) to overwrite the times with\n");
}

int main(int argc, char **argv)
{
  int VERBOSE = 0;
  int FORCE_VALID = 1;
  char buffer[MAX_VDIF_FRAME_BYTES];
  FILE * input;
  FILE * output;
  int readbytes, framebytes, framemjd, framesecond, framenumber, frameinvalid, datambps, framespersecond;
  int nextmjd, nextsecond, nextnumber;
  int overwritemjd, overwritesecond, overwritenumber;
  int offsetmjd=0, offsetsecond=0, offsetnumber=0;
  long long framesread, frameswrote;
  vdif_header *header;

  if(argc != 4 && argc != 5)
  {
    usage();

    return EXIT_FAILURE;
  }

  overwritemjd = 0;
  overwritesecond = 0;
  overwritenumber = 0;
  if(argc == 5) {
    overwritemjd = atoi(argv[4]);
    overwritesecond = (int)(86400.0*(atof(argv[4]) - overwritemjd));
  }
  
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

  if(FORCE_VALID) {
    printf("Forcing all read frames to be VALID! (Temp workaround for invalid tagged EVLA VDIF data)\n");
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
  if(overwritemjd > 0) {
    offsetmjd = overwritemjd - nextmjd;
    offsetsecond = overwritesecond - nextsecond;
    offsetnumber = overwritenumber - nextnumber;
  }
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
    if(getVDIFFrameBytes(header) != framebytes) { 
      fprintf(stderr, "Framebytes has changed! Can't deal with this, aborting\n");
      break;
    }
    if (framenumber >= framespersecond) {
      fprintf(stderr, "Encountered illegal frame number %d, exceeds fps-1=%d, will ignore and try to continue.\n", framenumber, framespersecond-1);
      continue;
    }
    framesread++;
    //check for missing frames
    while(framemjd != nextmjd || framesecond != nextsecond || framenumber != nextnumber) {
      if(VERBOSE)
        fprintf(stderr, "Missed a packet! We were expecting MJD %d, sec %d, frame %d, and the next one came in MJD %d, sec %d, frame %d\n",
                nextmjd, nextsecond, nextnumber, framemjd, framesecond, framenumber);
      //write out an invalid frame for the next frame
      overwritenumber = nextnumber + offsetnumber;
      overwritesecond = nextsecond + offsetsecond;
      overwritemjd    = nextmjd    + offsetmjd;
      while (overwritenumber < 0) {
        overwritenumber += framespersecond;
        overwritesecond++;
      }
      while (overwritenumber >= framespersecond) {
        overwritenumber -= framespersecond;
        overwritesecond--;
      }
      while (overwritesecond < 0) {
        overwritesecond += 86400;
        overwritemjd++;
      }
      while (overwritesecond >= 86400) {
        overwritesecond -= 86400;
        overwritemjd--;
      }
      setVDIFFrameMJD(header, overwritemjd);
      setVDIFFrameSecond(header, overwritesecond);
      setVDIFFrameNumber(header, overwritenumber);
      setVDIFFrameInvalid(header, 1);
      readbytes = fwrite(buffer, 1, framebytes, output); //write out the VDIF packet
      if(readbytes < framebytes) {
        fprintf(stderr, "Problem writing %lldth frame - only wrote %d bytes\n", framesread, readbytes);
        break;
      }
      frameswrote++;
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
    overwritenumber = framenumber + offsetnumber;
    overwritesecond = framesecond + offsetsecond;
    overwritemjd    = framemjd    + offsetmjd;
    while (overwritenumber < 0) {
      overwritenumber += framespersecond;
      overwritesecond++;
    }
    while (overwritenumber >= framespersecond) {
      overwritenumber -= framespersecond;
      overwritesecond--;
    }
    while (overwritesecond < 0) {
      overwritesecond += 86400;
      overwritemjd++;
    }
    while (overwritesecond >= 86400) {
      overwritesecond -= 86400;
      overwritemjd--;
    }
    header = (vdif_header*)buffer;
    setVDIFFrameMJD(header, overwritemjd);
    setVDIFFrameSecond(header, overwritesecond);
    setVDIFFrameNumber(header, overwritenumber);
    if(FORCE_VALID)
      frameinvalid = 0;
    setVDIFFrameInvalid(header, frameinvalid);
    readbytes = fwrite(buffer, 1, framebytes, output); //write out the VDIF packet
    if(readbytes < framebytes) {
      fprintf(stderr, "Problem writing %lldth frame - only wrote %d bytes\n", framesread, readbytes);
      break;
    }
    frameswrote++;
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

  printf("Read %lld and wrote %lld frames\n", framesread, frameswrote);
  fclose(input);
  fclose(output);

  return EXIT_SUCCESS;
}
