/***************************************************************************
 *   Copyright (C) 2011-2013 by Adam Deller & Walter Brisken               *
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
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/datamuxer.cpp $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <cstdio>
#include <cstring>
#ifdef HAVE_OPENMP
#include <omp.h>
#endif
#include "datamuxer.h"
#include "vdifio.h"
#include "alert.h"
#include "config.h"

#ifdef _OPENMP                                                                             
#define PRAGMA_OMP(args) __pragma(omp args)                                              
#else                                                                                      
#define PRAGMA_OMP(args) /**/                                                            
#endif                      

DataMuxer::DataMuxer(const Configuration * conf, int dsindex, int id, int nthreads, int sbytes)
  : config(conf), datastreamindex(dsindex), mpiid(id), numthreads(nthreads), segmentbytes(sbytes)
{
  demuxbuffer = vectorAlloc_u8(segmentbytes*DEMUX_BUFFER_FACTOR);
  estimatedbytes += segmentbytes*DEMUX_BUFFER_FACTOR;
  threadbuffers = new u8*[numthreads];
  threadbufferfree = new double[numthreads];
  for(int i=0;i<numthreads;i++)
  {
    threadbufferfree[i] = 1.0;
    threadbuffers[i] = vectorAlloc_u8(segmentbytes*DEMUX_BUFFER_FACTOR/numthreads);
    estimatedbytes += segmentbytes*DEMUX_BUFFER_FACTOR/numthreads;
  }
  resetcounters();
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

void DataMuxer::resetcounters()
{
  readcount = 0;
  muxcount  = 0;
  deinterlacecount = 0;
  estimatedbytes = 0;
  skipframes = 0;
  lastskipframes = 0;
}

double DataMuxer::getMinThreadBufferFree()
{
  double minfree = 1.0;
  for(int i=0;i<numthreads;i++)
  {
    if(threadbufferfree[i] < minfree)
      minfree = threadbufferfree[i];
  }
  return minfree;
}

double DataMuxer::getMaxThreadBufferFree()
{
  double maxfree = 0.0;
  for(int i=0;i<numthreads;i++)
  {
    if(threadbufferfree[i] > maxfree)
      maxfree = threadbufferfree[i];
  }
  return maxfree;
}

VDIFMuxer::VDIFMuxer(const Configuration * conf, int dsindex, int id, int nthreads, int iframebytes, int rframes, int fpersec, int bitspersamp, int * tmap)
  : DataMuxer(conf, dsindex, id, nthreads, iframebytes*rframes), inputframebytes(iframebytes), readframes(rframes), framespersecond(fpersec), bitspersample(bitspersamp)
{
  cinfo << startl << "VDIFMuxer: framespersecond is " << framespersecond << ", iframebytes is " << inputframebytes << endl;
  cinfo << startl << "VDIFMuxer: readframes is " << readframes << endl;
  outputframebytes = (inputframebytes-VDIF_HEADER_BYTES)*numthreads + VDIF_HEADER_BYTES;
  processframenumber = 0;
  numthreadbufframes = readframes * DEMUX_BUFFER_FACTOR / numthreads;
  activemask = (1<<bitspersample) - 1;
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

void VDIFMuxer::resetcounters()
{
  DataMuxer::resetcounters();
  processframenumber = 0;
  for(int i=0;i<numthreads;i++) {
    for(int j=0;j<numthreadbufframes;j++)
      bufferframefull[i][j] = false;
  }
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
#ifdef WORDS_BIGENDIAN
  // For big endian (non-intel), different, yet to be implemented, corner turners are needed
  // It is not even clear this generic one works for big endian...
  cornerturn = &VDIFMuxer::cornerturn_generic;
#else
  if (numthreads == 1) {
    cinfo << startl << "Using optimized VDIF corner turner: cornerturn_1thread" << endl;
    cornerturn = &VDIFMuxer::cornerturn_1thread;
  }
  else if (numthreads == 2 && bitspersample == 2) {
    cinfo << startl << "Using optimized VDIF corner turner: cornerturn_2thread_2bit" << endl;
    cornerturn = &VDIFMuxer::cornerturn_2thread_2bit;
  }
  else if (numthreads == 4 && bitspersample == 2) {
    cinfo << startl << "Using optimized VDIF corner turner: cornerturn_4thread_2bit" << endl;
    cornerturn = &VDIFMuxer::cornerturn_4thread_2bit;
  }
  else if (numthreads == 8 && bitspersample == 2) {
    cinfo << startl << "Using optimized VDIF corner turner: cornerturn_8thread_2bit" << endl;
    cornerturn = &VDIFMuxer::cornerturn_8thread_2bit;
  }
  else if (numthreads == 16 && bitspersample == 2) {
    cinfo << startl << "Using optimized VDIF corner turner: cornerturn_16thread_2bit" << endl;
    cornerturn = &VDIFMuxer::cornerturn_16thread_2bit;
  }
  else {
    cwarn << startl << "Using generic VDIF corner turner; performance may suffer" << endl;
    cornerturn = &VDIFMuxer::cornerturn_generic;
  }
#endif

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
        cinfo << startl << "Newly found input frame details are bytes = " << getVDIFFrameBytes((vdif_header*)currentptr) << ", MJD = " << getVDIFFrameMJD((vdif_header*)currentptr) << ", framesecond = " << getVDIFFrameSecond((vdif_header*)currentptr) << ", framenumber = " << getVDIFFrameNumber((vdif_header*)currentptr) << endl;
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


void VDIFMuxer::cornerturn_generic(u8 * outputbuffer, int processindex, int outputframecount)
{
  unsigned int copyword;
  unsigned int * outputwordptr;
  
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
}

  

void VDIFMuxer::cornerturn_1thread(u8 * outputbuffer, int processindex, int outputframecount)
{
  // Trivial case of 1 thread: just a copy

  const void * t = (const void *)(&(threadbuffers[0][processindex*inputframebytes + VDIF_HEADER_BYTES]));
  void * outputwordptr = (void *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

  memcpy(outputwordptr, t, wordsperinputframe*4);
}


void VDIFMuxer::cornerturn_2thread_2bit(u8 * outputbuffer, int processindex, int outputframecount)
{
  // Efficiently handle the special case of 2 threads of 2-bit data.
  //
  // Thread: ------1-------   ------0-------   ------1-------   ------0-------
  // Byte:   ------1-------   ------1-------   ------0-------   ------0-------
  // Input:  b7  b6  b5  b4   a7  a6  a5  a4   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:   0  -1  -2  -3   +3  +2  +1   0    0  -1  -2  -3   +3  +2  +1   0
  //
  // Output: b7  a7  b6  a6   b5  a5  b4  a4   b3  a3  b2  a2   b1  a1  b0  a0
  // Byte:   ------3-------   ------2-------   ------1-------   ------0-------

  const unsigned int M0 = 0xC003C003;
  const unsigned int M1 = 0x30003000;
  const unsigned int M2 = 0x000C000C;
  const unsigned int M3 = 0x0C000C00;
  const unsigned int M4 = 0x00300030;
  const unsigned int M5 = 0x03000300;
  const unsigned int M6 = 0x00C000C0;

  const u8 *t0 = threadbuffers[0] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1 = threadbuffers[1] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  unsigned int *outputwordptr = (unsigned int *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

  unsigned int x;
#ifdef HAVE_OPENMP
  unsigned int chunk = 125;
#endif
  int i, n;
  n = wordsperoutputframe;

PRAGMA_OMP(parallel private(i,x) shared(chunk,outputwordptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t1[2*i+1] << 24) | (t0[2*i+1] << 16) | (t1[2*i] << 8) | t0[2*i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 2) | ((x & M2) << 2) | ((x & M3) >> 4) | ((x & M4) << 4) | ((x & M5) >> 6) | ((x & M6) << 6);
    }
  }
}


void VDIFMuxer::cornerturn_4thread_2bit(u8 * outputbuffer, int processindex, int outputframecount)
{
  // Efficiently handle the special case of 4 threads of 2-bit data.  because nthread = samples/byte
  // this is effectively a matrix transpose.  With this comes some symmetries that make this case
  // unexpectedly simple.
  //
  // The trick is to first assemble a 32-bit word containing one 8 bit chunk of each thread
  // and then to reorder the bits using masking and shifts.  Only 7 unique shifts are needed.
  // Note: can be extended to do twice as many samples in a 64 bit word with about the same
  // number of instructions.  This results in a speed-down on 32-bit machines!
  //
  // This algorithm is approximately 9 times faster than the generic cornerturner for this case
  // and about 9 times harder to understand!  The table below (and others) indicates the sample motion
  //
  // Thread: ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:   0  -3  -6  -9   +3   0  -3  -6   +6  +3   0  -3   +9  +6  +3   0
  //
  // Output: d3  c3  b3  a3   d2  c2  b2  a2   d1  c1  b1  a1   d0  c0  b0  a0
  // Byte:   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // -WFB

  const unsigned int M0 = 0xC0300C03;
  const unsigned int M1 = 0x300C0300;
  const unsigned int M2 = 0x00C0300C;
  const unsigned int M3 = 0x0C030000;
  const unsigned int M4 = 0x0000C030;
  const unsigned int M5 = 0x03000000;
  const unsigned int M6 = 0x000000C0;

  const u8 *t0 = threadbuffers[0] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1 = threadbuffers[1] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t2 = threadbuffers[2] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t3 = threadbuffers[3] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  unsigned int *outputwordptr = (unsigned int *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

  unsigned int x;
#ifdef HAVE_OPENMP
  unsigned int chunk = 125;
#endif
  int i, n;
  n = wordsperoutputframe;

PRAGMA_OMP(parallel private(i,x) shared(chunk,outputwordptr,t0,t1,t2,t3,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 6) | ((x & M2) << 6) | ((x & M3) >> 12) | ((x & M4) << 12) | ((x & M5) >> 18) | ((x & M6) << 18);
    }
  }
}

void VDIFMuxer::cornerturn_8thread_2bit(u8 * outputbuffer, int processindex, int outputframecount)
{
  // Efficiently handle the special case of 8 threads of 2-bit data.
  //
  // Thread: ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:   0  -7 -14 -21   +3  -4 -11 -18   +6  -1  -8 -15   +9  +2  -5 -12  +12  +5  -2  -9  +15  +8  +1  -6  +18 +11  +4  -3  +21 +14  +7   0
  //
  // Output: h3  g3  f3  e3   d3  c3  b3  a3   h2  g2  f2  e2   d2  c2  b2  a2   h1  g1  f1  e1   d1  c1  b1  a1   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A resonable way to proceed seems to be to perform two separate 4-thread corner turns and then 
  // do a final suffle of byte sized chunks.  There may be a better way...
  //
  // FIXME: This is thought to work but has yet to be fully verified.

  const unsigned int M0 = 0xC0300C03;
  const unsigned int M1 = 0x300C0300;
  const unsigned int M2 = 0x00C0300C;
  const unsigned int M3 = 0x0C030000;
  const unsigned int M4 = 0x0000C030;
  const unsigned int M5 = 0x03000000;
  const unsigned int M6 = 0x000000C0;

  const u8 *t0 = threadbuffers[0] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1 = threadbuffers[1] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t2 = threadbuffers[2] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t3 = threadbuffers[3] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t4 = threadbuffers[4] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t5 = threadbuffers[5] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t6 = threadbuffers[6] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t7 = threadbuffers[7] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  unsigned int *outputwordptr = (unsigned int *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);
  unsigned int x1, x2;
#ifdef HAVE_OPENMP
  unsigned int chunk = 125;
#endif
  int i, n;
  n = wordsperoutputframe/2;
  union { unsigned int y1; u8 b1[4]; };
  union { unsigned int y2; u8 b2[4]; };

PRAGMA_OMP(parallel private(i,x1,x2,y1,y2,b1,b2) shared(chunk,outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 = (t7[i] << 24) | (t6[i] << 16) | (t5[i] << 8) | t4[i];

      // mask and shift 32-bit chunks
      y1 = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      y2 = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[2*i]   = (b2[1] << 24) | (b1[1] << 16) | (b2[0] << 8) | b1[0];
      outputwordptr[2*i+1] = (b2[3] << 24) | (b1[3] << 16) | (b2[2] << 8) | b1[2];
    }
  }
}

void VDIFMuxer::cornerturn_16thread_2bit(u8 * outputbuffer, int processindex, int outputframecount)
{
  // Efficiently handle the special case of 8 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  p3  p2  p1  p0   o3  o2  o1  o0   n3  n2  n1  n0   m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:  0  -15 -30 -45  +3  -12 -27 -42  +6  -9  -24 -39  +9  -6  -21 -36  +12 -3  -18 -33  +15  0  -15 -30  +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output: p3  o3  n3  m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3   p2  o2  n2  m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2   p1  o1  n1  m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1   p0  o0  n0  m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A resonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final suffle of byte sized chunks.  There may be a better way...
  //
  // FIXME: This is thought to work but has yet to be fully verified.

  const unsigned int M0 = 0xC0300C03;
  const unsigned int M1 = 0x300C0300;
  const unsigned int M2 = 0x00C0300C;
  const unsigned int M3 = 0x0C030000;
  const unsigned int M4 = 0x0000C030;
  const unsigned int M5 = 0x03000000;
  const unsigned int M6 = 0x000000C0;

  const u8 *t0  = threadbuffers[0]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1  = threadbuffers[1]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t2  = threadbuffers[2]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t3  = threadbuffers[3]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t4  = threadbuffers[4]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t5  = threadbuffers[5]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t6  = threadbuffers[6]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t7  = threadbuffers[7]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t8  = threadbuffers[8]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t9  = threadbuffers[9]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t10 = threadbuffers[10] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t11 = threadbuffers[11] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t12 = threadbuffers[12] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t13 = threadbuffers[13] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t14 = threadbuffers[14] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t15 = threadbuffers[15] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  unsigned int *outputwordptr = (unsigned int *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);
  unsigned int x1, x2, x3, x4;
#ifdef HAVE_OPENMP
  unsigned int chunk = 125;
#endif
  int i, n;
  n = wordsperoutputframe/4;
  union { unsigned int y1; u8 b1[4]; };
  union { unsigned int y2; u8 b2[4]; };
  union { unsigned int y3; u8 b3[4]; };
  union { unsigned int y4; u8 b4[4]; };

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,y1,y2,y3,y4,b1,b2,b3,b4) shared(chunk,outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];

      // mask and shift 32-bit chunks
      y1 = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      y2 = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      y3 = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      y4 = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6) | ((x4 & M3) >> 12) | ((x4 & M4) << 12) | ((x4 & M5) >> 18) | ((x4 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[4*i]   = (b4[0] << 24) | (b3[0] << 16) | (b2[0] << 8) | b1[0];
      outputwordptr[4*i+1] = (b4[1] << 24) | (b3[1] << 16) | (b2[1] << 8) | b1[1];
      outputwordptr[4*i+2] = (b4[2] << 24) | (b3[2] << 16) | (b2[2] << 8) | b1[2];
      outputwordptr[4*i+3] = (b4[3] << 24) | (b3[3] << 16) | (b2[3] << 8) | b1[3];
    }
  }
}

/* These experimental merge "cornerturners" will just merge the threads into a single new stream without 
 * intra-byte shuffling.  This is much faster and simpler but yields a non-standard data format.
 * A special decoder downstream will be needed to make use of this.
 */
void VDIFMuxer::cornerturn_2thread_merge(u8 * outputbuffer, int processindex, int outputframecount)
{
  const u8 *t0 = threadbuffers[0] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1 = threadbuffers[1] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  u8 *outputbyteptr = (u8 *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

#ifdef HAVE_OPENMP
  unsigned int chunk = 250;
#endif
  int i, n;
  n = wordsperoutputframe*2;

PRAGMA_OMP(parallel private(i) shared(chunk,outputbyteptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      outputbyteptr[2*i]   = t0[i];
      outputbyteptr[2*i+1] = t1[i];
    }
  }
}

void VDIFMuxer::cornerturn_4thread_merge(u8 * outputbuffer, int processindex, int outputframecount)
{
  const u8 *t0 = threadbuffers[0] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1 = threadbuffers[1] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t2 = threadbuffers[2] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t3 = threadbuffers[3] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  u8 *outputbyteptr = (u8 *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

#ifdef HAVE_OPENMP
  unsigned int chunk = 250;
#endif
  int i, n;
  n = wordsperoutputframe;

PRAGMA_OMP(parallel private(i) shared(chunk,outputbyteptr,t0,t1,t2,t3,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      outputbyteptr[4*i]   = t0[i];
      outputbyteptr[4*i+1] = t1[i];
      outputbyteptr[4*i+2] = t2[i];
      outputbyteptr[4*i+3] = t3[i];
    }
  }
}

void VDIFMuxer::cornerturn_8thread_merge(u8 * outputbuffer, int processindex, int outputframecount)
{
  const u8 *t0 = threadbuffers[0] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1 = threadbuffers[1] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t2 = threadbuffers[2] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t3 = threadbuffers[3] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t4 = threadbuffers[4] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t5 = threadbuffers[5] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t6 = threadbuffers[6] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t7 = threadbuffers[7] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  u8 *outputbyteptr = (u8 *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

#ifdef HAVE_OPENMP
  unsigned int chunk = 250;
#endif
  int i, n;
  n = wordsperoutputframe/2;

PRAGMA_OMP(parallel private(i) shared(chunk,outputbyteptr,t0,t1,t2,t3,t4,t5,t6,t7,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      outputbyteptr[8*i]   = t0[i];
      outputbyteptr[8*i+1] = t1[i];
      outputbyteptr[8*i+2] = t2[i];
      outputbyteptr[8*i+3] = t3[i];
      outputbyteptr[8*i+4] = t4[i];
      outputbyteptr[8*i+5] = t5[i];
      outputbyteptr[8*i+6] = t6[i];
      outputbyteptr[8*i+7] = t7[i];
    }
  }
}

void VDIFMuxer::cornerturn_16thread_merge(u8 * outputbuffer, int processindex, int outputframecount)
{
  const u8 *t0  = threadbuffers[0]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t1  = threadbuffers[1]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t2  = threadbuffers[2]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t3  = threadbuffers[3]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t4  = threadbuffers[4]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t5  = threadbuffers[5]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t6  = threadbuffers[6]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t7  = threadbuffers[7]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t8  = threadbuffers[8]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t9  = threadbuffers[9]  + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t10 = threadbuffers[10] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t11 = threadbuffers[11] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t12 = threadbuffers[12] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t13 = threadbuffers[13] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t14 = threadbuffers[14] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  const u8 *t15 = threadbuffers[15] + processindex*inputframebytes + VDIF_HEADER_BYTES;
  u8 *outputbyteptr = (u8 *)&(outputbuffer[outputframecount*outputframebytes + VDIF_HEADER_BYTES]);

#ifdef HAVE_OPENMP
  unsigned int chunk = 250;
#endif
  int i, n;
  n = wordsperoutputframe/4;

PRAGMA_OMP(parallel private(i) shared(chunk,outputbyteptr,t0,t1,t2,t3,t4,t5,t6,t7,n))
  {
PRAGMA_OMP(for schedule(dynamic,chunk) nowait)
    for(i = 0; i < n; ++i)
    {
      outputbyteptr[16*i]   = t0[i];
      outputbyteptr[16*i+1] = t1[i];
      outputbyteptr[16*i+2] = t2[i];
      outputbyteptr[16*i+3] = t3[i];
      outputbyteptr[16*i+4] = t4[i];
      outputbyteptr[16*i+5] = t5[i];
      outputbyteptr[16*i+6] = t6[i];
      outputbyteptr[16*i+7] = t7[i];
      outputbyteptr[16*i+8] = t8[i];
      outputbyteptr[16*i+9]  = t9[i];
      outputbyteptr[16*i+10] = t10[i];
      outputbyteptr[16*i+11] = t11[i];
      outputbyteptr[16*i+12] = t12[i];
      outputbyteptr[16*i+13] = t13[i];
      outputbyteptr[16*i+14] = t14[i];
      outputbyteptr[16*i+15] = t15[i];
    }
  }
}

int VDIFMuxer::multiplex(u8 * outputbuffer)
{
  vdif_header * header;
  vdif_header * copyheader;
  int outputframecount, processindex;
  bool foundframe;

  outputframecount = 0;
  lastskipframes = skipframes;

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

      // call the corner turning function.  gotta love this syntax!
      (this->*cornerturn)(outputbuffer, processindex, outputframecount);

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
          static unsigned int nNoValidFrames = 0;
          ++nNoValidFrames;
          //if NONE of the input thread frames are valid, *and* this is the first output frame, we have to make sure this is ignored
          
          // use Brian Kernighan's bit counting trick to see if nNoValidFrames is a power of 2 
          if(nNoValidFrames < 16 || (nNoValidFrames & (nNoValidFrames-1)) == 0)
          {
            cwarn << startl << "No valid input frames for frame " << processframenumber << "; the rest of this data segment will be lost (n=" << nNoValidFrames << ")" << endl;
          }
          //don't increment outputframecount
        }
      }
    }

    //clear the data we just used and adjust the threadbufferfree values
    for(int i=0;i<numthreads;i++) {
      bufferframefull[i][processindex] = false;
      threadbufferfree[i] += (1.0/((double)numthreadbufframes));
    }
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
  double bufferratio;
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

//EFFICIENCY: make map from threadId [0 to 1023] to local threadindex [0 to nChan-1]

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
      cinfo << startl << "Discarding a frame from thread " << framethread << " which is timestamped " << -currentframenumber << " frames earlier than the first frame in the file" << endl;
      cinfo << startl << "Currentframenumber is " << currentframenumber << ", processframenumber is " << processframenumber << ", framesecond is " << framesecond << ", refframesecond is " << refframesecond << ", framenumber is " << framenumber << ", refframenumber is " << refframenumber << ", framespersecond is " << framespersecond << endl;
      continue;
    }
    frameoffset  = (int) (currentframenumber - (processframenumber+skipframes));
    if(frameoffset < 0) {
      cwarn << startl << "Discarding a frame from thread " << framethread << " which is timestamped " << -frameoffset << " frames earlier than the current frame being processed" << endl;
      cwarn << startl << "Currentframenumber is " << currentframenumber << ", processframenumber is " << processframenumber << ", framesecond is " << framesecond << ", refframesecond is " << refframesecond << ", framenumber is " << framenumber << ", refframenumber is " << refframenumber << ", framespersecond is " << framespersecond << endl;
      continue;
    }
    if(frameoffset >= readframes*DEMUX_BUFFER_FACTOR/numthreads) {
      cfatal << startl << "Discarding a frame from thread " << framethread << " which is timestamped " << frameoffset << " frames after the current frame being processed - must be a large offset in the file, which is not supported" << endl;
      cfatal << startl << "Currentframenumber is " << currentframenumber << ", processframenumber is " << processframenumber << ", framesecond is " << framesecond << ", refframesecond is " << refframesecond << ", framenumber is " << framenumber << ", refframenumber is " << refframenumber << ", framespersecond is " << framespersecond << ", i=" << i << ", skipframes is " << skipframes << ", lastskipframes is " << lastskipframes << endl;
      cfatal << startl << "inputptr-demuxbuffer = " << ((char *)inputptr - (char *)demuxbuffer) << " bytes; allocated space = " << (segmentbytes*DEMUX_BUFFER_FACTOR) << " bytes; validbytes = " << validbytes << " bytes" << endl; 
      cfatal << startl << "This is loop " << i+1 << "/" << validbytes/inputframebytes << endl;
      cfatal << startl << "The preceding frame had frame number = " << getVDIFFrameNumber(inputptr-inputframebytes) << ", framesecond = " << getVDIFFrameSecond(inputptr+inputframebytes) << endl;
      cfatal << startl << "The following frame would have had information: frame number = " << getVDIFFrameNumber(inputptr+inputframebytes) << ", framesecond = " << getVDIFFrameSecond(inputptr+inputframebytes) << endl;
      cfatal << startl << "The following+1 frame would have had information: frame number = " << getVDIFFrameNumber(inputptr+2*inputframebytes) << ", framesecond = " << getVDIFFrameSecond(inputptr+2*inputframebytes) << endl;
      cfatal << startl << "The following+2 frame would have had information: frame number = " << getVDIFFrameNumber(inputptr+3*inputframebytes) << ", framesecond = " << getVDIFFrameSecond(inputptr+3*inputframebytes) << endl;
      cfatal << startl << "The following+3 frame would have had information: frame number = " << getVDIFFrameNumber(inputptr+4*inputframebytes) << ", framesecond = " << getVDIFFrameSecond(inputptr+4*inputframebytes) << endl;
      cfatal << startl << "The following+4 frame would have had information: frame number = " << getVDIFFrameNumber(inputptr+5*inputframebytes) << ", framesecond = " << getVDIFFrameSecond(inputptr+5*inputframebytes) << endl;
      return false;
    }
    bufferratio = ((double)frameoffset)/((double)numthreadbufframes);
    threadbufferfree[threadindex] = 1.0 - bufferratio;
    frameindex = (int)(currentframenumber % numthreadbufframes);
    if (bufferframefull[threadindex][frameindex]) {
      cwarn << startl << "Frame at index " << frameindex << " (which was count " << currentframenumber << ") was already full for thread " << threadindex << " - probably a major time gap in the file, which is not supported.  Numthreadbufframes is " << numthreadbufframes << endl;
    }
    memcpy(threadbuffers[threadindex] + frameindex*framebytes, inputptr, framebytes);
    bufferframefull[threadindex][frameindex] = true;
  }
  deinterlacecount++;

  return true;
}

// vim: shiftwidth=2:softtabstop=2:expandtab
