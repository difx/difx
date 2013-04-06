/***************************************************************************
 *   Copyright (C) 2013 Walter Brisken                                     *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: vdifio.h 4140 2011-12-13 04:23:35Z ChrisPhillips $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision: 4140 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2011-12-12 21:23:35 -0700 (Mon, 12 Dec 2011) $
//
//============================================================================


/* TODO list

2. Constrain output lengths to frame granularity
3. Avoid first memory copy by using a matrix of threadbuffer pointers
4. Implement m5xxx tools for vdif

*/



#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <vdifio.h>
#include "config.h"

#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif


/* greatest common divisor, from wikipedia */
static unsigned int gcd(unsigned int u, unsigned int v)
{
  // simple cases (termination)
  if (u == v)
    return u;
  if (u == 0)
    return v;
  if (v == 0)
    return u;
 
  // look for factors of 2
  if ((~u) & 1) // u is even
  {
    if (v & 1) // v is odd
    {
      return gcd(u >> 1, v);
    }
    else // both u and v are even
    {
      return gcd(u >> 1, v >> 1) << 1;
    }
  }
  if ((~v) & 1) // u is odd, v is even
  {  
    return gcd(u, v >> 1);
  }
  // reduce larger argument
  if (u > v)
  {  
    return gcd((u - v) >> 1, v);
  }
  return gcd((v - u) >> 1, u);
}



static void cornerturn_1thread(unsigned char *outputBuffer, const unsigned char **threadBuffers, int outputDataSize)
{
  // Trivial case of 1 thread: just a copy

  memcpy(outputBuffer, threadBuffers[0], outputDataSize);
}


static void cornerturn_2thread_2bit(unsigned char *outputBuffer, const unsigned char **threadBuffers, int outputDataSize)
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

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;

  unsigned int x, chunk = 125;
  int i, n;
  n = outputDataSize/4;

#pragma omp parallel private(i,x) shared(chunk,outputwordptr,t0,t1,n)
  {
#pragma omp for schedule(dynamic,chunk) nowait
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t1[2*i+1] << 24) | (t0[2*i+1] << 16) | (t1[2*i] << 8) | t0[2*i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 2) | ((x & M2) << 2) | ((x & M3) >> 4) | ((x & M4) << 4) | ((x & M5) >> 6) | ((x & M6) << 6);
    }
  }
}


static void cornerturn_4thread_2bit(unsigned char *outputBuffer, const unsigned char **threadBuffers, int outputDataSize)
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

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  const unsigned char *t2 = threadBuffers[2];
  const unsigned char *t3 = threadBuffers[3];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;

  unsigned int x, chunk = 125;
  int i, n;
  n = outputDataSize/4;

#pragma omp parallel private(i,x) shared(chunk,outputwordptr,t0,t1,t2,t3,n)
  {
#pragma omp for schedule(dynamic,chunk) nowait
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 6) | ((x & M2) << 6) | ((x & M3) >> 12) | ((x & M4) << 12) | ((x & M5) >> 18) | ((x & M6) << 18);
    }
  }
}

static void cornerturn_8thread_2bit(unsigned char *outputBuffer, const unsigned char **threadBuffers, int outputDataSize)
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

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  const unsigned char *t2 = threadBuffers[2];
  const unsigned char *t3 = threadBuffers[3];
  const unsigned char *t4 = threadBuffers[4];
  const unsigned char *t5 = threadBuffers[5];
  const unsigned char *t6 = threadBuffers[6];
  const unsigned char *t7 = threadBuffers[7];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2, chunk=125;
  int i, n;
  n = outputDataSize/8;
  union { unsigned int y; unsigned char b[4]; } u1, u2;

#pragma omp parallel private(i,x1,x2,u1,u2) shared(chunk,outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,n)
  {
#pragma omp for schedule(dynamic,chunk) nowait
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 = (t7[i] << 24) | (t6[i] << 16) | (t5[i] << 8) | t4[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[2*i]   = (u2.b[1] << 24) | (u1.b[1] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[2*i+1] = (u2.b[3] << 24) | (u1.b[3] << 16) | (u2.b[2] << 8) | u1.b[2];
    }
  }
}

static void cornerturn_16thread_2bit(unsigned char *outputBuffer, const unsigned char **threadBuffers, int outputDataSize)
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

  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];
  const unsigned char *t6  = threadBuffers[6];
  const unsigned char *t7  = threadBuffers[7];
  const unsigned char *t8  = threadBuffers[8];
  const unsigned char *t9  = threadBuffers[9];
  const unsigned char *t10 = threadBuffers[10];
  const unsigned char *t11 = threadBuffers[11];
  const unsigned char *t12 = threadBuffers[12];
  const unsigned char *t13 = threadBuffers[13];
  const unsigned char *t14 = threadBuffers[14];
  const unsigned char *t15 = threadBuffers[15];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2, x3, x4, chunk=125;
  int i, n;
  n = outputDataSize/16;
  union { unsigned int y; unsigned char b[4]; } u1, u2, u3, u4;

#pragma omp parallel private(i,x1,x2,x3,x4,u1,u2,u3,u4) shared(chunk,outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,n)
  {
#pragma omp for schedule(dynamic,chunk) nowait
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u1.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u1.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      u1.y = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6) | ((x4 & M3) >> 12) | ((x4 & M4) << 12) | ((x4 & M5) >> 18) | ((x4 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[4*i]   = (u4.b[0] << 24) | (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[4*i+1] = (u4.b[1] << 24) | (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[4*i+2] = (u4.b[2] << 24) | (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[4*i+3] = (u4.b[3] << 24) | (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
    }
  }
}


/* Params are:
 *
 * dest:
 *	pointer to output (multiplexed, single-thread) VDIF data.
 * destSize:
 *	the size of the output (dest) array.
 * src:
 *	pointer to input (multi-thread) VDIF data.
 * srcSize:
 *	length of the input (src) array.
 * inputFrameSize:
 *	length of single-thread VDIF data packet
 * inputFramesPerSecond:
 *	Number of frames per second per thread of input to expect
 * nBit:
 *	number of bits per sample
 * nThread:
 *	number of input threads.
 * threadIds:
 *	list of thread ids.
 * nSort:
 *	maximum out-of-orderness to allow.
 * nGap:
 *	maximum gap in frame number to allow before returning early.
 * startOutputFrameNumber:
 *      if >= 0, fix the beginning of the dest buffer grid to be this.
 *      Set to -1 unless you really know what frame number to expect.
 * stats:
 *	statistics and information about the processing
 *
 * Will stop when one of three conditions occurs:
 * 1. dest array size is exceeded
 * 2. src array is exhausted
 * 3. a gap longer than nGap frames is encountered
 *
 * Returns:
 *  < 0 on error
 *  Number of processed bytes from source on success
 *
 * The output data is to be stored in dest.  Statistics and some details of the produced data are stored in the stats.
 * Output data is uniform in time.  All initially missing data is replaced with valid VDIF packets with the invalid bit set.
 */

int vdifmux(unsigned char *dest, int destSize, const unsigned char *src, int srcSize, int inputFrameSize, int inputFramesPerSecond, int nBit, int nThread, const int *threadIds, int nSort, int nGap, long long startOutputFrameNumber, struct vdif_mux_statistics *stats)
{
	const int maxThreads = 1024;
	unsigned char chanIndex[maxThreads];	/* map from threadId to channel number (0 to nThread-1) */
	int threadId;
	int nValidFrame = 0;			/* counts number of valid input frames found so far */
	int nSkip = 0;				/* counts number of bytes skipped (not what we are looking for) */
	int nFill = 0;				/* counts number of bytes skipped that were fill pattern */
	int nInvalidFrame = 0;			/* counts number of VDIF frames skipped because of invalid bits */
	long long startFrameNumber;		/* = seconds*inputFramesPerSecond + frameNumber */
	int frameGranularity;			/* number of frames required to make an integer number of nanoseconds */

	int i;					/* index into src */
	int f;
	int N = srcSize - inputFrameSize;	/* max value to allow i to be */
	int highestDestIndex = 0;
	int maxDestIndex;
	int inputDataSize;
	int outputFrameSize;
	int outputDataSize;
	int nOutputChan;			/* nThread rounded up to nearest power of 2 */
	uint32_t goodMask;			/* mask value that represents all channels are present */
	int bytesProcessed = 0;			/* the program return value */
	int nEnd = 0;				/* number of frames processed after the end of the buffer first reached */
	int nGoodOutput = 0;
	int nBadOutput = 0;
	int nWrongThread = 0;
	vdif_header outputHeader;
	int seconds, frameNum;
	int firstValid;

	void (*cornerTurner)(unsigned char *, const unsigned char **, int);

	/* input checks and initialization */
	if(nBit != 1 && nBit != 2 && nBit != 4 && nBit != 8)
	{
		return -1;
	}
	if(nThread < 1 || nThread*nBit > 32)
	{
		return -2;
	}
	if(nGap < nSort)
	{
		nGap = nSort;
	}

	for(nOutputChan = 1; nOutputChan < nThread; nOutputChan *= 2) ;

	/* Choose a corner turning algorithm.  Currently the selection is limited to 2-bit cases or
	 * anything with 1 thread.  Others can be implemented as needed.
	 */
	if(nOutputChan == 1)
	{
		cornerTurner = cornerturn_1thread;
	}
	else if(nBit == 2)
	{
		if(nOutputChan == 2)
		{
			cornerTurner = cornerturn_2thread_2bit;
		}
		else if(nOutputChan == 4)
		{
			cornerTurner = cornerturn_4thread_2bit;
		}
		else if(nOutputChan == 8)
		{
			cornerTurner = cornerturn_8thread_2bit;
		}
		else if(nOutputChan == 16)
		{
			cornerTurner = cornerturn_16thread_2bit;
		}
		else
		{
			return -3;
		}
	}
	else
	{
		return -3;
	}

	memset(chanIndex, 255, maxThreads);
	for(threadId = 0; threadId < nThread; ++threadId)
	{
		if(threadIds[threadId] < 0 || threadIds[threadId] >= maxThreads)
		{
			return -4;
		}
		chanIndex[threadIds[threadId]] = threadId;
	}

	inputDataSize = inputFrameSize - VDIF_HEADER_BYTES;
	outputDataSize = inputDataSize*nOutputChan;
	outputFrameSize = outputDataSize + VDIF_HEADER_BYTES;
	frameGranularity = inputFramesPerSecond/gcd(inputFramesPerSecond, 1000000000);
	maxDestIndex = destSize/outputFrameSize - 1;
	goodMask = (1 << nThread) - 1;	/* nThread 1s as LSBs and 0s above that */

	startFrameNumber = startOutputFrameNumber;

	/* clear mask of presense */
	for(i = 0; i < maxDestIndex; ++i)
	{
		uint32_t *p = (uint32_t *)(dest + outputFrameSize*i);
		p[7] = 0;
	}

	
	/* Stage 1: find good data and put in output array. */
	for(i = 0; i <= N;)
	{
		const unsigned char *cur = src + i;
		const vdif_header *vh = (vdif_header *)cur;
		long long frameNumber;
		int destIndex;		/* frame index into destination array */
		int chanId;

/* This is disabled for now; the VLA produces only "invalid" data
		if(getVDIFFrameInvalid(vh) > 0)
		{
			i += inputFrameSize;
			++nInvalidFrame;

			continue;
		}
*/
		if(*((uint32_t *)(cur+inputFrameSize-4)) == FILL_PATTERN)
		{
			/* Fill pattern at end of frame or invalid bit is set */
			i += inputFrameSize;
			nFill += inputFrameSize;

			continue;
		}
		if(*((uint32_t *)cur) == FILL_PATTERN)
		{
			/* Fill pattern at beginning of frame */
			i += 8;
			nFill += 8;

			continue;
		}
		if(getVDIFFrameBytes(vh) != inputFrameSize ||
		   getVDIFNumChannels(vh) != 1 ||
		   getVDIFBitsPerSample(vh) != nBit)
		{
			i += 4;
			nSkip += 4;

			continue;
		}

		/* If we are here, it looks like we have a VDIF frame to work with */
		threadId = getVDIFThreadID(vh);
		chanId = chanIndex[threadId];
		if(chanId > 32)
		{
			/* Not one of the threads we are looking for */
			i += inputFrameSize;
			++nWrongThread;

			continue;
		}

		frameNumber = (long long)(getVDIFFullSecond(vh)) * inputFramesPerSecond + getVDIFFrameNumber(vh);
		
		if(startFrameNumber < 0)	/* we haven't seen data yet */
		{
			startFrameNumber = frameNumber - nSort;
			startFrameNumber -= (startFrameNumber % frameGranularity);	/* to ensure first frame starts on integer ns */
		
			/* also use this first good frame to generate the prototype VDIF header for the output */
			memcpy((char *)&outputHeader, cur, VDIF_HEADER_BYTES);
			setVDIFNumChannels(&outputHeader, nOutputChan);
			setVDIFThreadID(&outputHeader, 0);
			setVDIFFrameBytes(&outputHeader, outputFrameSize);
		}
	
		/* add 1 to reserve the first slot for later semi-in-place corner turning */
		destIndex = frameNumber - startFrameNumber + 1;

		if(destIndex < 1)
		{
			/* no choice but to discard this data */
			i += inputFrameSize;
			nSkip += inputFrameSize;

			continue;
		}
		if(destIndex > maxDestIndex)
		{
			/* start the shut-down procedure */
			if(bytesProcessed == 0)
			{
				bytesProcessed = i;
			}
			i += inputFrameSize;
			++nEnd;
			if(nEnd >= nSort)
			{
				break;
			}
		}
		else 
		{
			uint32_t *p = (uint32_t *)(dest + outputFrameSize*destIndex);
			
			if(destIndex > highestDestIndex + nGap)
			{
				if(nValidFrame > nSort || startOutputFrameNumber >= 0)
				{
					/* if we are out of the probationary nSort period, start the shut-down procedure */
					if(bytesProcessed == 0)
					{
						bytesProcessed = i;
					}
					++nEnd;
					if(nEnd >= nSort)
					{
						break;
					}
				}
				else
				{
					/* otherwise we take this opportunity to reset the startFrameNumber and clear data moved up to now */
					
					startFrameNumber = frameNumber - nSort;
					startFrameNumber -= (startFrameNumber % frameGranularity);	/* to ensure first frame starts on integer ns */

					/* clear mask of presense */
					for(destIndex = 0; destIndex < highestDestIndex; ++destIndex)
					{
						p = (uint32_t *)(dest + outputFrameSize*destIndex);
						p[7] = 0;
					}
					highestDestIndex = 0;

					/* at this point we're starting over, so there are no valid frames. */
					nSkip += nValidFrame*inputFrameSize;
					nValidFrame = 0;	

					destIndex = frameNumber - startFrameNumber + 1;
					p = (uint32_t *)(dest + outputFrameSize*destIndex);
				}
			}

			/* Finally, here we are at a point where we can copy data */
			
#if 0
			if(p[7] == 0)
			{
				/* frame header not yet copied.  Copy only first 16 bytes */
				memcpy(dest + outputFrameSize*i, src + i, 16);
			}
#endif

			/* set mask indicating valid data in place */
			p[7] |= (1 << chanId);
			memcpy(dest + outputFrameSize*destIndex + inputDataSize*chanId + VDIF_HEADER_BYTES, cur + VDIF_HEADER_BYTES, inputDataSize);
			++nValidFrame;
			i += inputFrameSize;

			if(destIndex > highestDestIndex)
			{
				highestDestIndex = destIndex;
			}
		}
	}

	if(bytesProcessed == 0)
	{
		bytesProcessed = i;
	}


	/* Stage 2: do the corner turning and header population */

	seconds = startFrameNumber/inputFramesPerSecond;
	frameNum = startFrameNumber%inputFramesPerSecond;

	firstValid = -1;

	for(f = 1; f <= highestDestIndex; ++f)
	{
		const unsigned char *cur = dest + outputFrameSize*f;	/* points to block of data needing rearrangement */
		unsigned char *frame;					/* points to new single-thread-VDIF frame */
		uint32_t *p = (uint32_t *)cur;
		const unsigned char *threadBuffers[32];

		if(firstValid < 0 && p[7] == goodMask)
		{
			firstValid = f;
			startFrameNumber = (long long)seconds*inputFramesPerSecond + frameNum;	/* report the actual first frame written */
		}

		if(firstValid >= 0)
		{
			frame = dest + outputFrameSize*(f-firstValid);

			/* generate header for output frame */
			memcpy(frame, (const char *)&outputHeader, VDIF_HEADER_BYTES);
			setVDIFFrameSecond((vdif_header *)frame, seconds);
			setVDIFFrameNumber((vdif_header *)frame, frameNum);

			if(p[7] == goodMask)
			{
				int j;

				for(j = 0; j < nOutputChan; ++j)
				{
					threadBuffers[j] = cur + VDIF_HEADER_BYTES + inputDataSize*j;
				}
				cornerTurner(frame + VDIF_HEADER_BYTES, threadBuffers, outputDataSize);

				++nGoodOutput;
			}
			else
			{
				/* Set invalid bit */
				setVDIFFrameInvalid((vdif_header *)frame, 1);

				++nBadOutput;
			}
		}

		++frameNum;
		if(frameNum >= inputFramesPerSecond)
		{
			++seconds;
			frameNum -= inputFramesPerSecond;
		}
	}

	if(stats)
	{
		stats->nValidFrame += nValidFrame;
		stats->nInvalidFrame += nInvalidFrame;
		stats->nDiscardedFrame += (nValidFrame - nThread*nGoodOutput);
		stats->nWrongThread += nWrongThread;
		stats->nSkippedByte += nSkip;
		stats->nFillByte += nFill;
		stats->bytesProcessed += bytesProcessed;
		stats->nGoodFrame += nGoodOutput;

		stats->srcSize = srcSize;
		stats->srcUsed = bytesProcessed;
		stats->destSize = destSize;
		stats->destUsed = (nGoodOutput + nBadOutput)*outputFrameSize;
		stats->inputFrameSize = inputFrameSize;
		stats->outputFrameSize = outputFrameSize;
		stats->outputFrameGranularity = frameGranularity;
		stats->outputFramesPerSecond = inputFramesPerSecond;
		stats->nOutputFrame = nGoodOutput + nBadOutput;
		stats->startFrameNumber = startFrameNumber;
		
		++stats->nCall;
	}


	return bytesProcessed;
}

void printvdifmuxstatistics(const struct vdif_mux_statistics *stats)
{
	if(stats)
	{
		printf("VDIF multiplexer statistics:\n");
		printf("  Number of calls to vdifmux         = %d\n", stats->nCall);
		printf("  Number of valid input frames       = %Ld\n", stats->nValidFrame);
		printf("  Number of invalid input frames     = %Ld\n", stats->nInvalidFrame);
		printf("  Number of discarded frames         = %Ld\n", stats->nDiscardedFrame);
		printf("  Number of wrong-thread frames      = %Ld\n", stats->nWrongThread);
		printf("  Number of skipped interloper bytes = %Ld\n", stats->nSkippedByte);
		printf("  Number of fill pattern bytes       = %Ld\n", stats->nFillByte);
		printf("  Total number of bytes processed    = %Ld\n", stats->bytesProcessed);
		printf("  Total number of good output frames = %Ld\n", stats->nGoodFrame);
		printf("Properties of output data from recent call:\n");
		printf("  Input frame size                   = %d\n", stats->inputFrameSize);
		printf("  Output frame size                  = %d\n", stats->outputFrameSize);
		printf("  Number of output frames            = %d\n", stats->nOutputFrame);
		printf("  Start output frame number          = %Ld\n", stats->startFrameNumber);
		printf("  Output frame granularity           = %d\n", stats->outputFrameGranularity);
		printf("  Output frames per second           = %d\n", stats->outputFramesPerSecond);
		printf("  %d/%d src bytes consumed\n", stats->srcUsed, stats->srcSize);
		printf("  %d/%d dest bytes generated\n", stats->destUsed, stats->destSize);
	}
	else
	{
		fprintf(stderr, "Weird: printvdifmuxstatistics called with null pointer\n");
	}
}

void resetvdifmuxstatistics(struct vdif_mux_statistics *stats)
{
	if(stats)
	{
		memset(stats, 0, sizeof(struct vdif_mux_statistics));
	}
}
