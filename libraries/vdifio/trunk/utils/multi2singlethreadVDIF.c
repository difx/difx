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
 * $Id: stripVDIF.c 2006 2010-03-04 16:43:04Z AdamDeller $
 * $HeadURL:  $
 * $LastChangedRevision: 2006 $
 * $Author: AdamDeller $
 * $LastChangedDate: 2010-03-04 09:43:04 -0700 (Thu, 04 Mar 2010) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "vdifio.h"

const char program[] = "multi2singlethreadVDIF";
const char author[]  = "Adam Deller <adeller@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20120102";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version,
          author, verdate);
  fprintf(stderr, "A program to translate multiple thread VDIF format to single thread\n");
  fprintf(stderr, "Must be one datastream in and one datastream out\n");
  fprintf(stderr, "\nUsage: %s <VDIF input file> <VDIF output file> <Num input threads> <Num output threads> ", program);
  fprintf(stderr, "<input Mbps/thread> <threadId0> <threadId1> ... <threadIdN> [-v]\n");
  fprintf(stderr, "\n<VDIF input file> is the name of the multiple thread VDIF file to read\n");
  fprintf(stderr, "\n<VDIF output file> is the name of the single thread VDIF file to write\n");
  fprintf(stderr, "\n<Num input threads> is the number of threads to start with (must be a power of 2)\n");
  fprintf(stderr, "\n<Number output threads> Number of threads in the output multichannel VDIF file (must be power of 2)\n"); 
  fprintf(stderr, "\n<input Mbps/thread> is the data rate in Mbps expected per input thread\n");
  fprintf(stderr, "\n<threadIdN> is the threadId to put in the Nth output channel\n");
  fprintf(stderr, "\n[-v] verbose mode on\n");
  fprintf(stderr, "The input file must at least start with one valid packet\n");
}

// functions
int validData(int ** bufferframefull, int bufferframe, int numthreads, int verbose) {
  int i;

  if(verbose) {
    printf("Looking at bufferframe %d\n", bufferframe);
  }
  for(i = 0;i < numthreads; ++i) {
    if(!bufferframefull[i][bufferframe])
      return 0;
  }
  return 1;
}

int numMissingFrames(int ** bufferframefull, int bufferframe, int numthreads, int verbose) {
  int i, toreturn = 0;
  
  for(i = 0;i < numthreads; ++i) {
    if(!bufferframefull[i][bufferframe])
      ++toreturn;
  }
  return toreturn;
}

int readdata(int inputframebytes, char * inputbuffer, FILE * input, int numbufferframes, int numthreadbufframes, int framespersecond, int * threadindexmap, char ** threadbuffers, int ** bufferframefull, int processframenumber, int numthreads, int refframemjd, int refframesecond, int refframenumber, int verbose)
{
  int i, j, framestoread, storedframes;
  int inputframecount, readbytes, frameoffset, frameindex;
  int framebytes, framemjd, framesecond, framenumber, framethread, threadindex;
  long long currentframenumber;
  vdif_header *header;

  //read from the file
  framestoread = numbufferframes;
  storedframes = 0;
  while(framestoread > 0 && !feof(input)) {
    inputframecount = framestoread;
    readbytes = fread(inputbuffer, 1, inputframebytes*framestoread, input);
    if (readbytes < framestoread*inputframebytes) {
      fprintf(stderr, "Read failed with only %d/%d bytes - probably at end of file.\n", readbytes, framestoread*inputframebytes);
      inputframecount = readbytes/inputframebytes;
    }
    framestoread -= inputframecount;

    //distribute packets
    for(i=0;i<inputframecount;++i) {
      header = (vdif_header*)(inputbuffer+i*inputframebytes);
      framethread = getVDIFThreadID(header);
      framebytes = getVDIFFrameBytes(header);
      framemjd = getVDIFFrameMJD(header);
      framesecond = getVDIFFrameSecond(header);
      framenumber = getVDIFFrameNumber(header);
      if(framebytes != inputframebytes) {
        fprintf(stderr, "Framebytes has changed, from %d to %d - aborting!\n", inputframebytes, framebytes);
        exit(EXIT_FAILURE);
      }
      //check that this thread is wanted
      threadindex = -1;
      for(j=0;j<numthreads;++j) {
        if(threadindexmap[j] == framethread) {
          threadindex = j;
          break;
        }
      }
      if(threadindex < 0) {
        if(verbose) {
          fprintf(stderr, "Skipping packet from threadId %d\n", framethread);
        }
        continue;
      }
      storedframes++;
  
      //put this frame where it belongs
      currentframenumber = ((long long)((framemjd-refframemjd)*86400 + framesecond - refframesecond))*framespersecond + framenumber - refframenumber;
      if (currentframenumber < 0) {
        fprintf(stderr, "Discarding a frame from thread %d which is timestamped %lld frames earlier than the first frame in the file\n", framethread, currentframenumber);
        continue;
      }
      frameoffset  = (int) (currentframenumber - processframenumber);
      if(frameoffset < 0) {
        fprintf(stderr, "Discarding a frame from thread %d which is timestamped %d frames earlier than the current frame being processed\n", framethread, -frameoffset);
        continue;
      }
      frameindex = (int)(currentframenumber % numthreadbufframes);
      if (bufferframefull[threadindex][frameindex]) {
        fprintf(stderr, "Frame at index %d, which was count %lld was already full - aborting!\n", frameindex, currentframenumber);
        exit(EXIT_FAILURE);
      }
      if(verbose) {
        fprintf(stdout, "Putting a frame from thread %d into slot %d, the frame count is %d\n", threadindex, frameindex, i);
      }
      memcpy(threadbuffers[threadindex] + frameindex*framebytes, inputbuffer + i*framebytes, framebytes);
      bufferframefull[threadindex][frameindex] = 1;
    }
  }
  return storedframes;
}

// main method
int main(int argc, char **argv)
{
  char tempbuffer[MAX_VDIF_FRAME_BYTES*2];
  unsigned int bitmask[9] = {0, 1, 3, 7, 15, 31, 63, 127, 255};
  FILE * input;
  FILE * output;
  int inputthreadmbps, numthreads, readbytes, wrotebytes, numbufferframes, threadbufmultiplier, numthreadbufframes;
  int bitspersample, samplesperframe, framespersecond;
  int inputframebytes, outputframebytes, refframeepoch, refframemjd, refframesecond, refframenumber;
  int outputframecount;
  char * inputbuffer; // [framebytes * numbufferframes]
  char * outputbuffer; // [framebytes * numbufferframes]
  char ** threadbuffers; // [numthreads][framebytes * numbufferframes]
  unsigned int * threadwords; // [numthreads]
  unsigned int * outputword;
  int ** bufferframefull; //[numthreads][numbufferframes]
  //long long ** bufferframenumber; //[numthreads][numbufferframes]
  //long long * currentthreadwriteframe; //[numthreads]
  //long long * currentthreadreadframe; //[numthreads]
  //int * threadlastbufferindex; // [numthreads]
  int * threadindexmap; // [numthreads]
  int f, i, j, k, l, verbose, processindex;
  unsigned int copyword, activemask;
  int wordsperinputframe, samplesperinputword, samplesperoutputword, numinputthreads, framesinbuffer, nmissing, totalmissing = 0;
  long long processframenumber;
  vdif_header *header;

  //check the command line arguments, store thread mapping etc
  if(argc < 8)
  {
    usage();

    return EXIT_FAILURE;
  }
  numinputthreads = atoi(argv[3]);
  numthreads = atoi(argv[4]);
  if(argc != 6+numthreads && argc != 7+numthreads)
  {
    usage();

    return EXIT_FAILURE;
  }
  if(numthreads < 2) {
    fprintf(stderr, "This is multi2single - you must have a minimum of 2 threads!\n");
    return EXIT_FAILURE;
  }

  if(argc == 7+numthreads)
    verbose = 1;
  else
    verbose = 0;

  inputthreadmbps = atoi(argv[5]);
  threadindexmap = (int *) malloc(numthreads * sizeof(int));
  for(i=0;i<numthreads;++i)
    threadindexmap[i] = atoi(argv[6+i]);

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
    fclose(input);
    exit(EXIT_FAILURE);
  }

  printf("About to peek in file\n");
  //peek at the start of the file, work out framebytes and reference time
  readbytes = fread(tempbuffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
  if (readbytes<VDIF_HEADER_BYTES) {
    if (feof(input)) {
      fprintf(stderr, "Hit EOF reading first header from %s\n", argv[2]);
    } else if (ferror(input)) {
      fprintf(stderr, "Error reading first header from %s\n", argv[2]);
     } else {
      fprintf(stderr, "Did not read enough bytes on first header from %s\n", argv[2]);
    }
    fclose(input);
    fclose(output);
    exit(EXIT_FAILURE);
  }
  
  header = (vdif_header*)tempbuffer;
  inputframebytes = getVDIFFrameBytes(header);
  if(inputframebytes > MAX_VDIF_FRAME_BYTES) {
    fprintf(stderr, "Cannot read frame with %d bytes > max (%d)\n", inputframebytes, MAX_VDIF_FRAME_BYTES);
    fclose(input);
    fclose(output);
    exit(EXIT_FAILURE);
  }
  outputframebytes = (inputframebytes-VDIF_HEADER_BYTES)*numthreads + VDIF_HEADER_BYTES;
  bitspersample = getVDIFBitsPerSample(header);
  activemask = bitmask[bitspersample];
  refframeepoch = header->epoch;
  refframemjd = getVDIFFrameMJD(header);
  refframesecond = getVDIFFrameSecond(header);
  refframenumber = getVDIFFrameNumber(header);
  framespersecond = (int)((((long long)inputthreadmbps)*1000000)/(8*(inputframebytes-VDIF_HEADER_BYTES)));
  samplesperframe = ((inputframebytes-VDIF_HEADER_BYTES)*8)/bitspersample;
  wordsperinputframe = (inputframebytes-VDIF_HEADER_BYTES)/4;
  samplesperinputword = samplesperframe/wordsperinputframe;
  samplesperoutputword = samplesperinputword/numthreads;
  printf("Frames per second is %d\n", framespersecond);
  rewind(input); //go back to the start
  if(samplesperoutputword == 0) {
    fprintf(stderr, "Too many threads/too high bit resolution - can't fit one complete timestep in a 32 bit word! Aborting\n");
    fclose(input);
    fclose(output);
    exit(EXIT_FAILURE);
  }

  //set up the buffers
  threadbufmultiplier = 4/numthreads;
  if(threadbufmultiplier == 0)
    threadbufmultiplier = 1;
  numbufferframes = 1024;
  numthreadbufframes = numbufferframes*threadbufmultiplier;
  inputbuffer = (char *)malloc(inputframebytes*numbufferframes);
  outputbuffer = (char *)malloc(outputframebytes*numbufferframes/numthreads);
  threadbuffers = (char **) malloc(numthreads * sizeof(char *));
  bufferframefull = (int **) malloc(numthreads * sizeof(int *));
  threadwords = (unsigned int *) malloc(numthreads * sizeof(unsigned int));
  //bufferframenumber = (long long **) malloc(numthreads * sizeof(long long *));
  //threadlastbufferindex = (int *) malloc(numthreads * sizeof(int));
  //currentthreadwriteframe = (long long *) malloc(numthreads * sizeof(long long));
  //currentthreadreadframe = (long long *) malloc(numthreads * sizeof(long long));
  for(i=0;i<numthreads;++i) {
    threadbuffers[i] = malloc(inputframebytes*numthreadbufframes);
    bufferframefull[i] = malloc(numthreadbufframes*sizeof(int));
    for(j=0;j<numthreadbufframes;++j) {
      bufferframefull[i][j] = 0;
    }
  }

  //initialise the read buffer, do some checking
  int storedframes;
  outputframecount = 0;
  processframenumber = 0;
  framesinbuffer = 0;
  for(i=0;i<threadbufmultiplier*numthreads/2;++i) {
    if(!feof(input)) {
      storedframes = readdata(inputframebytes, inputbuffer, input, numbufferframes, numthreadbufframes, framespersecond, threadindexmap, threadbuffers, bufferframefull, processframenumber, numthreads, refframemjd, refframesecond, refframenumber, verbose);
      framesinbuffer += storedframes;
    }
  }

  //loop through until no more data
  while(!feof(input) || ((totalmissing < (numthreads*numbufferframes)/numinputthreads) && validData(bufferframefull, processframenumber % numthreadbufframes, numthreads, verbose))) {
    if(verbose) {
      printf("Looping through %d frames...\n", numbufferframes);
    }
    //read data, if we still can
    if(!feof(input) && ((totalmissing == (numthreads*numbufferframes)/numinputthreads) || (framesinbuffer < 2.5*(numthreads*numbufferframes)/numinputthreads))) {
      storedframes = readdata(inputframebytes, inputbuffer, input, numbufferframes, numthreadbufframes, framespersecond, threadindexmap, threadbuffers, bufferframefull, processframenumber, numthreads, refframemjd, refframesecond, refframenumber, verbose);
      framesinbuffer += storedframes;
    }

    //loop over the equivalent amount of data we just read in
    //for(f=0;f<numbufferframes/numthreads;++f) {
    totalmissing = 0;
    for(f=0;f<numbufferframes/numinputthreads;++f) {
      //rearrange one frame
      processindex = processframenumber % numthreadbufframes;
      nmissing = numMissingFrames(bufferframefull, processindex, numthreads, verbose);
      totalmissing += nmissing;
      if(validData(bufferframefull, processindex, numthreads, verbose)) {
        //copy in and tweak up the VDIF header
        memcpy(outputbuffer + outputframecount*outputframebytes, threadbuffers[0] + processindex*inputframebytes, VDIF_HEADER_BYTES);
	header = (vdif_header*)(outputbuffer + outputframecount*outputframebytes);
        setVDIFFrameInvalid(header, 0);
        setVDIFNumChannels(header, numthreads);
        setVDIFFrameBytes(header, outputframebytes);
        setVDIFThreadID(header, 0);
	
        //loop over all the samples and copy them in
        copyword = 0;
        for(i=0;i<wordsperinputframe;++i) {
          for(j=0;j<numthreads;++j)
            threadwords[j] = *(unsigned int *)(&(threadbuffers[j][processindex*inputframebytes + VDIF_HEADER_BYTES + i*4]));
          for(j=0;j<numthreads;++j) {
            outputword = (unsigned int *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES + (i*numthreads + j)*4]);
            copyword = 0;
            for(k=0;k<samplesperoutputword;++k) {
              for(l=0;l<numthreads;++l) {
                copyword |= ((threadwords[l] >> ((j*samplesperoutputword + k)*bitspersample)) & (activemask)) << (k*numthreads + l)*bitspersample;
              }
            }
            *outputword = copyword;
          }
        }

        //clear the data we just used
        for(i=0;i<numthreads;++i)
          bufferframefull[i][processframenumber % numthreadbufframes] = 0;
        ++outputframecount;
      }
      else{
        if(verbose) {
          fprintf(stderr, "Not all threads had valid data for frame %lld\n", processframenumber);
  	  if(feof(input))
 	    fprintf(stderr, "EOF has been reached\n");
	  else
	    fprintf(stderr, "EOF has been *not* reached\n");
	}
	header = (vdif_header*)(outputbuffer + outputframecount*outputframebytes);
	setVDIFNumChannels(header, numthreads);
	setVDIFFrameNumber(header, (processframenumber+refframenumber)%framespersecond);
	setVDIFFrameBytes(header, outputframebytes);
	setVDIFBitsPerSample(header, bitspersample);
	setVDIFThreadID(header, 0);
        header->epoch = refframeepoch; // required for setVDIFFrameMJD to work correctly
        setVDIFFrameMJD(header, refframemjd);
        setVDIFFrameSecond(header, refframesecond + (processframenumber+refframenumber)/framespersecond);
	setVDIFFrameInvalid(header, 1);
        //clear the data we just used
	for(i=0;i<numthreads;++i)
	  bufferframefull[i][processframenumber % numthreadbufframes] = 0;
	++outputframecount;
      }  
      framesinbuffer -= (numthreads - nmissing);
      if(f==0 && verbose)
        printf("Framesinbuffer is now %d, numMissingFrames is %d\n", framesinbuffer, nmissing);
      ++processframenumber;

      //if output buffer is full, dump it to output file
      if(outputframecount == numbufferframes/numthreads) {
        //dump the frames
        wrotebytes = fwrite(outputbuffer, 1, outputframecount*outputframebytes, output);
        if(wrotebytes != outputframecount*outputframebytes)
          fprintf(stderr, "Write failed! Only \n");
        outputframecount = 0;
      }
    }
  }

  if (outputframecount != 0) {
    wrotebytes = fwrite(outputbuffer, 1, outputframecount*outputframebytes, output);
    if(wrotebytes != outputframecount*outputframebytes)
      fprintf(stderr, "Write failed!\n");
  }

  fclose(input);
  fclose(output);

  free(inputbuffer);
  free(outputbuffer);
  free(threadwords);
  free(threadindexmap);
  for(i=0;i<numthreads;++i) {
    free(threadbuffers[i]);
  }
  free(threadbuffers);

  return EXIT_SUCCESS;
}

