/***************************************************************************
 *   Copyright (C) 2011 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au/~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: datamuxer.cpp 2273 2010-07-01 00:22:30Z AdamDeller $
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/datamuxer.cpp $
// $LastChangedRevision: 2273 $
// $Author: AdamDeller $
// $LastChangedDate: 2010-06-30 18:22:30 -0600 (Wed, 30 Jun 2010) $
//
//============================================================================
#include "datamuxer.h"
#include "vdifio.h"
#include "alert.h"
#include <stdio.h>
#include <string.h>

DataMuxer::DataMuxer(const Configuration * conf, int dsindex, int id, int nthreads, int sbytes)
  : config(conf), datastreamindex(dsindex), mpiid(id), numthreads(nthreads), segmentbytes(sbytes)
{
  readcount = 0;
  muxcount  = 0;
  deinterlacecount = 0;
  estimatedbytes = 0;
  demuxbuffer = vectorAlloc_u8(segmentbytes*DEMUX_BUFFER_FACTOR);
  estimatedbytes += segmentbytes*DEMUX_BUFFER_FACTOR;
  threadbuffers = new u8*[numthreads];
  for(int i=0;i<numthreads;i++)
  {
    threadbuffers[i] = vectorAlloc_u8(segmentbytes*DEMUX_BUFFER_FACTOR/numthreads);
    estimatedbytes += segmentbytes*DEMUX_BUFFER_FACTOR/numthreads;
  }
}

DataMuxer::~DataMuxer()
{
  for(int i=0;i<numthreads;i++)
  {
    vectorFree(threadbuffers[i]);
  }
  delete [] threadbuffers;
  vectorFree(demuxbuffer);
}

const unsigned int VDIFMuxer::bitmask[9] = {0, 1, 3, 7, 15, 31, 63, 127, 255};

VDIFMuxer::VDIFMuxer(const Configuration * conf, int dsindex, int id, int nthreads, int iframebytes, int rframes, int fpersec, int bitspersamp, int * tmap)
  : DataMuxer(conf, dsindex, id, nthreads, iframebytes*rframes), inputframebytes(iframebytes), readframes(rframes), framespersecond(fpersec), bitspersample(bitspersamp)
{
  cverbose << startl << "VDIFMuxer: framespersecond is " << framespersecond << ", iframebytes is " << inputframebytes << endl;
  outputframebytes = (inputframebytes-VDIF_HEADER_BYTES)*numthreads + VDIF_HEADER_BYTES;
  processframenumber = 0;
  numthreadbufframes = readframes * DEMUX_BUFFER_FACTOR / numthreads;
  activemask = bitmask[bitspersample];
  threadindexmap = new int[numthreads];
  bufferframefull = new bool*[numthreads];
  threadwords = new unsigned int[numthreads];
  for(int i=0;i<numthreads;i++) {
    threadindexmap[i] = tmap[i];
    bufferframefull[i] = new bool[numthreadbufframes];
    for(int j=0;j<numthreadbufframes;j++)
      bufferframefull[i][j] = false;
  }  
}

VDIFMuxer::~VDIFMuxer()
{
  for(int i=0;i<numthreads;i++) {
    delete [] bufferframefull[i];
  }
  delete [] bufferframefull;
  delete [] threadwords;
  delete [] threadindexmap;
}

bool VDIFMuxer::initialise()
{
  //check the size of an int
  if (sizeof(int) != 4) {
    cfatal << startl << "Int size is " << sizeof(int) << " bytes - VDIFMuxer assumes 4 byte ints - I must abort!" << endl;
    return false;
  }

  //get a reference time, calculate number of words and samples per input/output frame
  //assumes there is data at the start of the demuxbuffer already
  vdif_header *header = (vdif_header*)demuxbuffer;
  refframemjd = getVDIFFrameMJD(header);
  refframesecond = getVDIFFrameSecond(header);
  refframenumber = getVDIFFrameNumber(header);
  samplesperframe = ((inputframebytes-VDIF_HEADER_BYTES)*8)/bitspersample;
  wordsperinputframe = (inputframebytes-VDIF_HEADER_BYTES)/4;
  wordsperoutputframe = wordsperinputframe*numthreads;
  samplesperinputword = samplesperframe/wordsperinputframe;
  samplesperoutputword = samplesperinputword/numthreads;
  if(samplesperoutputword == 0) {
    cfatal << startl << "Too many threads/too high bit resolution - can't fit one complete timestep in a 32 bit word! Aborting." << endl;
    return false;
  }

  return true;
}

int VDIFMuxer::datacheck(u8 * checkbuffer, int bytestocheck, int startfrom)
{
  int consumedbytes, byteoffset, bytestoread;
  char * currentptr;
  char * lastptr;

  //loop over one read's worth of data, shifting any time we find a bad packet
  consumedbytes = (startfrom/inputframebytes)*inputframebytes;
  currentptr = (char*)checkbuffer + consumedbytes;
  bytestoread = 0;
  while(consumedbytes < bytestocheck - (inputframebytes-1)) {
    byteoffset = 0;
    if(getVDIFFrameBytes((vdif_header*)currentptr) != inputframebytes) {
      cwarn << startl << "Bad packet detected in VDIF datastream after " << consumedbytes << " bytes" << endl;
      lastptr = currentptr;
      byteoffset += 4;
      currentptr += 4;
      while(getVDIFFrameBytes((vdif_header*)currentptr) != inputframebytes && consumedbytes + byteoffset < bytestocheck) {
        byteoffset += 4;
        currentptr += 4;
      }
      if(consumedbytes + byteoffset < bytestocheck) {
        cwarn << startl << "Length of interloper packet was " << byteoffset << " bytes" << endl;
      }
      else {
        cwarn << startl << "Reached end of checkbuffer before end of interloper packet - " << byteoffset << " bytes read" << endl;
      }
      bytestoread += byteoffset;
      memmove(lastptr, currentptr, bytestocheck - (consumedbytes + byteoffset));
      consumedbytes += byteoffset;
      currentptr -= byteoffset; //go back to where we should be, after having moved the memory...
    }
    consumedbytes += inputframebytes;
    currentptr += inputframebytes;
  }
  return bytestoread; 
} 

bool VDIFMuxer::validData(int bufferframe) const
{
  for(int i = 0;i < numthreads; i++) {
    if(!bufferframefull[i][bufferframe])
      return false;
  }
  return true;
}

int VDIFMuxer::multiplex(u8 * outputbuffer)
{
  vdif_header * header;
  vdif_header * copyheader;
  unsigned int * outputwordptr;
  unsigned int copyword;
  int outputframecount, processindex;
  bool foundframe;

  outputframecount = 0;

  //loop over one read's worth of data
  for(int f=0;f<readframes/numthreads;f++) {
    //rearrange one frame
    processindex = processframenumber % (readframes*DEMUX_BUFFER_FACTOR/numthreads);
    if(validData(processindex)) {
      //copy in and tweak up the VDIF header
      header = (vdif_header *)(outputbuffer + outputframecount*outputframebytes);
      memcpy(header, (char *)(threadbuffers[0] + processindex*inputframebytes), VDIF_HEADER_BYTES);
      setVDIFFrameInvalid(header, 0);
      setVDIFNumChannels(header, numthreads);
      setVDIFFrameBytes(header, outputframebytes);
      setVDIFThreadID(header, 0);

      //loop over all the samples and copy them in
      copyword = 0;
      for(int i=0;i<wordsperinputframe;i++) {
        for(int j=0;j<numthreads;j++)
          threadwords[j] = *(unsigned int *)(&(threadbuffers[j][processindex*inputframebytes + VDIF_HEADER_BYTES + i*4]));
        for(int j=0;j<numthreads;j++) {
          outputwordptr = (unsigned int *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES + (i*numthreads + j)*4]);
          copyword = 0;
          for(int k=0;k<samplesperoutputword;k++) {
            for(int l=0;l<numthreads;l++) {
              copyword |= ((threadwords[l] >> ((j*samplesperoutputword + k)*bitspersample)) & (activemask)) << (k*numthreads + l)*bitspersample;
            }
          }
          *outputwordptr = copyword;
        }
      }
      outputframecount++;
    }
    else {
      cdebug << startl << "Not all threads had valid data for frame " << processframenumber << endl;
      //just copy in a header, tweak it up for the right time/threadid and set it to invalid
      foundframe = false;
      for(int i = 0;i < numthreads; i++) {
        if(bufferframefull[i][processindex]) {
          header = (vdif_header *)(outputbuffer + outputframecount*outputframebytes); 
          memcpy(header, (char *)(threadbuffers[i] + processindex*inputframebytes), VDIF_HEADER_BYTES);
          setVDIFFrameInvalid(header, 1);
          setVDIFFrameBytes(header, outputframebytes);
          setVDIFThreadID(header, 0);
          foundframe = true;
          break;
        }
      }
      if(foundframe) {
        outputframecount++;
      }
      else {
        if(outputframecount>0) { //take the preceding output frame instead
          header = (vdif_header *)(outputbuffer + outputframecount*outputframebytes);
          copyheader = (vdif_header *)(outputbuffer + (outputframecount-1)*outputframebytes);
          memcpy(header, copyheader, VDIF_HEADER_BYTES);
          setVDIFFrameInvalid(header, 1);
          setVDIFFrameNumber(header, getVDIFFrameNumber(header)+1);
          if(getVDIFFrameNumber(header) == framespersecond) {
            setVDIFFrameNumber(header, 0);
            setVDIFFrameSecond(header, getVDIFFrameSecond(header)+1);
          }
          outputframecount++;
        }
        else {
          //if NONE of the input thread frames are valid, *and* this is the first output frame, we have to make sure this is ignored
          cwarn << startl << "No valid input frames for frame " << processframenumber << "; the rest of this data segment will be lost" << endl;
          //don't increment outputframecount
        }
      }
    }

    //clear the data we just used
    for(int i=0;i<numthreads;i++)
      bufferframefull[i][processindex] = false;
    processframenumber++;
  }

  return outputframecount*outputframebytes;
}

bool VDIFMuxer::deinterlace(int validbytes)
{
  int framethread, framebytes, framemjd, framesecond, framenumber;
  int frameoffset, frameindex, threadindex;
  long long currentframenumber;
  bool found;
  vdif_header * inputptr;

  //cout << "Deinterlacing: deinterlacecount is " << deinterlacecount << endl;
  //cout << "Will start from " << (deinterlacecount%DEMUX_BUFFER_FACTOR)*readframes << " frames in" << endl;
  for(int i=0;i<validbytes/inputframebytes;i++) {
    inputptr = (vdif_header*)(demuxbuffer + i*inputframebytes + (deinterlacecount%DEMUX_BUFFER_FACTOR)*readframes*inputframebytes);
    framethread = getVDIFThreadID(inputptr);
    framebytes = getVDIFFrameBytes(inputptr);
    framemjd = getVDIFFrameMJD(inputptr);
    framesecond = getVDIFFrameSecond(inputptr);
    framenumber = getVDIFFrameNumber(inputptr);
    if(framebytes != inputframebytes) {
      cfatal << startl << "Framebytes has changed, from " << inputframebytes << " to " << framebytes << " - aborting!" << endl;
      return false;
    }
    //check that this thread is wanted
    found = false;
    for(int j=0;j<numthreads;j++) {
      if(threadindexmap[j] == framethread) {
        found = true;
        threadindex = j;
        break;
      }
    }
    if(!found) {
      cdebug << startl << "Skipping packet from threadId " << framethread << ", numthreads is " << numthreads << ", mapping is " << endl;
      for(int j=0;j<numthreads;j++)
        cdebug << startl << threadindexmap[j] << endl;
      continue;
    }

    //put this frame where it belongs
    currentframenumber = ((long long)((framemjd-refframemjd)*86400 + framesecond - refframesecond))*framespersecond + framenumber - refframenumber;
    if (currentframenumber < 0) {
      cdebug << startl << "Discarding a frame from thread " << framethread << " which is timestamped " << -currentframenumber << " frames earlier than the first frame in the file" << endl;
      cdebug << startl << "Currentframenumber is " << currentframenumber << ", processframenumber is " << processframenumber << ", framesecond is " << framesecond << ", refframesecond is " << refframesecond << ", framenumber is " << framenumber << ", refframenumber is " << refframenumber << ", framespersecond is " << framespersecond << endl;
      continue;
    }
    frameoffset  = (int) (currentframenumber - processframenumber);
    if(frameoffset < 0) {
      cwarn << startl << "Discarding a frame from thread " << framethread << " which is timestamped " << -frameoffset << " frames earlier than the current frame being processed" << endl;
      cdebug << startl << "Currentframenumber is " << currentframenumber << ", processframenumber is " << processframenumber << ", framesecond is " << framesecond << ", refframesecond is " << refframesecond << ", framenumber is " << framenumber << ", refframenumber is " << refframenumber << ", framespersecond is " << framespersecond << endl;
      continue;
    }
    if(frameoffset > readframes*DEMUX_BUFFER_FACTOR/numthreads) {
      cfatal << startl << "Discarding a frame from thread " << framethread << " which is timestamped " << frameoffset << " frames after the current frame being processed - must be a large offset in the file, which is not supported" << endl;
      cfatal << startl << "Currentframenumber is " << currentframenumber << ", processframenumber is " << processframenumber << ", framesecond is " << framesecond << ", refframesecond is " << refframesecond << ", framenumber is " << framenumber << ", refframenumber is " << refframenumber << ", framespersecond is " << framespersecond << endl;
      return false;
    }
    frameindex = (int)(currentframenumber % numthreadbufframes);
    if (bufferframefull[threadindex][frameindex]) {
      cwarn << startl << "Frame at index " << frameindex << " (which was count " << currentframenumber << ") was already full for thread " << threadindex << " - probably a major time gap in the file, which is not supported" << endl;
    }
    memcpy(threadbuffers[threadindex] + frameindex*framebytes, inputptr, framebytes);
    bufferframefull[threadindex][frameindex] = true;
  }
  deinterlacecount++;
  return true;
}

// vim: shiftwidth=2:softtabstop=2:expandtab
