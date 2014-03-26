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
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================


/* TODO list

 * Constrain output lengths to frame granularity ???

*/



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <vdifio.h>
#include "config.h"

#ifdef HAVE_OPENMP
#include <omp.h>
#endif

#ifdef _OPENMP
#if defined(__GNUC__)
#define STRINGIFY(a) #a
#define PRAGMA_OMP(args) _Pragma(STRINGIFY(omp args))
#else
#define PRAGMA_OMP(args) __pragma(omp args)
#endif
#else
#define PRAGMA_OMP(args) /**/
#endif


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



static void cornerturn_1thread(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Trivial case of 1 thread: just a copy

  memcpy(outputBuffer, threadBuffers[0], outputDataSize);
}

static void cornerturn_2thread_1bit_slow(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 2 threads of 1-bit data.
  //
  // Thread: ---------------1---------------   ---------------0---------------
  // Byte:   ---------------0---------------   ---------------0---------------
  // Input:  b7  b6  b5  b4  b3  b2  b1  b0    a7  a6  a5  a4  a3  a2  a1  a0
  //
  // Shift:   0  -1  -2  -3  -4  -5  -6  -7    +7  +6  +5  +4  +3  +2  +1   0
  //
  // Input:  b7  a7  b6  a6  b5  a5  b4  a4    b3  a3  b2  a2  b1  a1  b0  a0
  // Byte:   ---------------0---------------   ---------------0---------------
  //
  //
  // Take algorithm from http://graphics.stanford.edu/~seander/bithacks.html#Interleave64bitOps

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  uint16_t *outputwordptr = (uint16_t *)outputBuffer;
  int i, n;

  n = outputDataSize/2;

PRAGMA_OMP(parallel private(i) shared(outputwordptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      outputwordptr[i] = (((t0[i] * 0x0101010101010101ULL & 0x8040201008040201ULL) * 0x0102040810204081ULL >> 49) & 0x5555) |
                         (((t1[i] * 0x0101010101010101ULL & 0x8040201008040201ULL) * 0x0102040810204081ULL >> 48) & 0xAAAA);
    }
  }
}

static void cornerturn_2thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 2 threads of 1-bit data.
  //
  // Thread: ---------------1---------------   ---------------0---------------
  // Byte:   ---------------0---------------   ---------------0---------------
  // Input:  b7  b6  b5  b4  b3  b2  b1  b0    a7  a6  a5  a4  a3  a2  a1  a0
  //
  // Shift:   0  -1  -2  -3  -4  -5  -6  -7    +7  +6  +5  +4  +3  +2  +1   0
  //
  // Input:  b7  a7  b6  a6  b5  a5  b4  a4    b3  a3  b2  a2  b1  a1  b0  a0
  // Byte:   ---------------0---------------   ---------------0---------------
  //
  //
  // Take algorithm from http://graphics.stanford.edu/~seander/bithacks.html#Interleave64bitOps

  const uint16_t *t0 = (uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (uint16_t *)(threadBuffers[1]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  int i, n;
  uint32_t x, y;

  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i, x, y) shared(outputwordptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x = t0[i];
      x = (x | (x << 8)) & 0x00FF00FF;
      x = (x | (x << 4)) & 0x0F0F0F0F;
      x = (x | (x << 2)) & 0x33333333;
      x = (x | (x << 1)) & 0x55555555;

      y = t1[i];
      y = (y | (y << 8)) & 0x00FF00FF;
      y = (y | (y << 4)) & 0x0F0F0F0F;
      y = (y | (y << 2)) & 0x33333333;
      y = (y | (y << 1)) & 0x55555555;

      outputwordptr[i] = x | (y << 1);
    }
  }
}

static void cornerturn_3thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0 = (uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (uint8_t *)(threadBuffers[2]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  int i, n;
  uint32_t x, y, z;

  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i, x, y, z) shared(outputwordptr,t0,t1,t2,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x = t0[i];
      x = (x | (x << 12));
      x = (x | (x << 6)) & 0x03030303;
      x = (x | (x << 3)) & 0x11111111;

      y = t1[i];
      y = (y | (y << 12));
      y = (y | (y << 6)) & 0x03030303;
      y = (y | (y << 3)) & 0x11111111;

      z = t2[i];
      z = (z | (z << 12));
      z = (z | (z << 6)) & 0x03030303;
      z = (z | (z << 3)) & 0x11111111;

      outputwordptr[i] = x | (y << 1) | (z << 2);
    }
  }
}

static void cornerturn_4thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0 = (uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (uint8_t *)(threadBuffers[3]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  int i, n;
  uint32_t x, y, z, w;

  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i, x, y, z, w) shared(outputwordptr,t0,t1,t2,t3,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x = t0[i];
      x = (x | (x << 12));
      x = (x | (x << 6)) & 0x03030303;
      x = (x | (x << 3)) & 0x11111111;

      y = t1[i];
      y = (y | (y << 12));
      y = (y | (y << 6)) & 0x03030303;
      y = (y | (y << 3)) & 0x11111111;

      z = t2[i];
      z = (z | (z << 12));
      z = (z | (z << 6)) & 0x03030303;
      z = (z | (z << 3)) & 0x11111111;

      w = t3[i];
      w = (w | (w << 12));
      w = (w | (w << 6)) & 0x03030303;
      w = (w | (w << 3)) & 0x11111111;

      outputwordptr[i] = x | (y << 1) | (z << 2) | (w << 3);
    }
  }
}

static void cornerturn_5thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0 = (uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (uint8_t *)(threadBuffers[4]);
  uint64_t *outputwordptr = (uint64_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,a,b,c,d,e) shared(outputwordptr,t0,t1,t2,t3,t4,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      a = t0[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t1[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t2[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t3[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t4[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      outputwordptr[i] = a | (b << 1) | (c << 2) | (d << 3) | (e << 4);
    }
  }
}

static void cornerturn_6thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0 = (uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (uint8_t *)(threadBuffers[5]);
  uint64_t *outputwordptr = (uint64_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f) shared(outputwordptr,t0,t1,t2,t3,t4,t5,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      a = t0[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t1[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t2[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t3[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t4[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t5[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      outputwordptr[i] = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5);
    }
  }
}

static void cornerturn_7thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0 = (uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (uint8_t *)(threadBuffers[6]);
  uint64_t *outputwordptr = (uint64_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      a = t0[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t1[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t2[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t3[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t4[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t5[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t6[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      outputwordptr[i] = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6);
    }
  }
}

static void cornerturn_8thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0 = (uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (uint8_t *)(threadBuffers[7]);
  uint64_t *outputwordptr = (uint64_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      a = t0[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t1[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t2[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t3[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t4[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t5[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t6[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t7[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      outputwordptr[i] = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7);
    }
  }
}

/* For 2-bit 2-thread corner turning there are two implementions here.  One optimized for 64-bit and the other for 32-bit.
   At compile time one is named cornerturn_2thread_2bit and the other cornerturn_2thread_2bitSlow.  Which is which depends
   on sizeof(size_t)
 */

#if SIZEOF_SIZE_T == 8
// 64-bit optimized version
static void cornerturn_2thread_2bit_slow(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
#else
static void cornerturn_2thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
#endif
{
  // Efficiently handle the special case of 2 threads of 2-bit data.
  // This algorithm is optimized for 64-bit computers
  //
  // Thread: ------1-------   ------0-------   ------1-------   ------0-------   ------1-------   ------0-------   ------1-------   ------0-------
  // Byte:   ------3-------   ------3-------   ------2-------   ------2-------   ------1-------   ------1-------   ------0-------   ------0-------
  // Input:  b15 b14 b13 b12  a15 a14 a13 a12  b11 b10 b9  b8   a11 a10 a9  a8   b7  b6  b5  b4   a7  a6  a5  a4   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                              
  // Shift:   0  -1  -2  -3   +3  +2  +1   0    0  -1  -2  -3   +3  +2  +1   0    0  -1  -2  -3   +3  +2  +1   0    0  -1  -2  -3   +3  +2  +1   0
  //                                                                                                                                            
  // Output: b15 a15 b14 a14  b13 a13 b12 a12  b11 a11 b10 a10  b9  a9  b8  a8   b7  a7  b6  a6   b5  a5  b4  a4   b3  a3  b2  a2   b1  a1  b0  a0
  // Byte:   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------

  const uint64_t M0 = 0xC003C003C003C003;
  const uint64_t M1 = 0x3000300030003000;
  const uint64_t M2 = 0x000C000C000C000C;
  const uint64_t M3 = 0x0C000C000C000C00;
  const uint64_t M4 = 0x0030003000300030;
  const uint64_t M5 = 0x0300030003000300;
  const uint64_t M6 = 0x00C000C000C000C0;

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint64_t x;
  int i, n;
  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t1[4*i+3] * 0x100000000000000LL) | (t0[4*i+3] * 0x001000000000000LL) | 
          (t1[4*i+2] * 0x000010000000000LL) | (t0[4*i+2] * 0x000000100000000LL) | 
	  (t1[4*i+1] * 0x000000001000000LL) | (t0[4*i+1] * 0x000000000010000LL) | 
	  (t1[4*i]   * 0x000000000000100LL) |  t0[4*i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 2) | ((x & M2) << 2) | ((x & M3) >> 4) | ((x & M4) << 4) | ((x & M5) >> 6) | ((x & M6) << 6);
    }
  }
}
#if SIZEOF_SIZE_T == 8
static void cornerturn_2thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
#else
static void cornerturn_2thread_2bit_slow(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
#endif
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

  unsigned int x;
  int i, n;
  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t1[2*i+1] << 24) | (t0[2*i+1] << 16) | (t1[2*i] << 8) | t0[2*i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 2) | ((x & M2) << 2) | ((x & M3) >> 4) | ((x & M4) << 4) | ((x & M5) >> 6) | ((x & M6) << 6);
    }
  }
}

static void cornerturn_3thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 3 threads of 2-bit data.  
  //
  // This is based on cornerturn_4thread_2bit and results in a 4 channel output with 1 channel of all zeros.
  //
  // Thread: ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:                   +3   0  -3  -6   +6  +3   0  -3   +9  +6  +3   0
  //
  // Output:     c3  b3  a3       c2  b2  a2       c1  b1  a1       c0  b0  a0
  // Byte:   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // -WFB

  const unsigned int M0 = 0x00300C03;
  const unsigned int M1 = 0x000C0300;
  const unsigned int M2 = 0x00C0300C;
  const unsigned int M3 = 0x00030000;
  const unsigned int M4 = 0x0000C030;
  const unsigned int M6 = 0x000000C0;

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  const unsigned char *t2 = threadBuffers[2];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;

  unsigned int x;
  int i, n;
  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,t2,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t2[i] << 16) | (t1[i] << 8) | t0[i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 6) | ((x & M2) << 6) | ((x & M3) >> 12) | ((x & M4) << 12) | ((x & M6) << 18);
    }
  }
}

static void cornerturn_4thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
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

  unsigned int x;
  int i, n;
  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,t2,t3,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 6) | ((x & M2) << 6) | ((x & M3) >> 12) | ((x & M4) << 12) | ((x & M5) >> 18) | ((x & M6) << 18);
    }
  }
}

static void cornerturn_5thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 5 threads of 2-bit data.
  // This will be packed into an 8-channel output stream where the 3 unused channels will be all zeros
  //
  // Thread: ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                     e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:                                                     +9  +2  -5 -12  +12  +5  -2  -9  +15  +8  +1  -6  +18 +11  +4  -3  +21 +14  +7   0
  //
  // Output:             e3   d3  c3  b3  a3               e2   d2  c2  b2  a2               e1   d1  c1  b1  a1               e0   d0  c0  b0  a0
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
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { unsigned int y; unsigned char b[4]; } u1, u2;

PRAGMA_OMP(parallel private(i,x1,x2,u1,u2) shared(outputwordptr,t0,t1,t2,t3,t4,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 =                                                t4[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0)                    | ((x2 & M2) << 6)                     | ((x2 & M4) << 12)                     | ((x2 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[2*i]   = (u2.b[1] << 24) | (u1.b[1] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[2*i+1] = (u2.b[3] << 24) | (u1.b[3] << 16) | (u2.b[2] << 8) | u1.b[2];
    }
  }
}

static void cornerturn_6thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 6 threads of 2-bit data.
  //
  // Thread: ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                    f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:                                    +6  -1  -8 -15   +9  +2  -5 -12  +12  +5  -2  -9  +15  +8  +1  -6  +18 +11  +4  -3  +21 +14  +7   0
  //
  // Output:         f3  e3   d3  c3  b3  a3           f2  e2   d2  c2  b2  a2           f1  e1   d1  c1  b1  a1           f0  e0   d0  c0  b0  a0
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
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { unsigned int y; unsigned char b[4]; } u1, u2;

PRAGMA_OMP(parallel private(i,x1,x2,u1,u2) shared(outputwordptr,t0,t1,t2,t3,t4,t5,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 =                                 (t5[i] << 8) | t4[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6)                     | ((x2 & M4) << 12)                     | ((x2 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[2*i]   = (u2.b[1] << 24) | (u1.b[1] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[2*i+1] = (u2.b[3] << 24) | (u1.b[3] << 16) | (u2.b[2] << 8) | u1.b[2];
    }
  }
}

static void cornerturn_7thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 7 threads of 2-bit data.
  //
  // Thread: ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //
  // Shift:                   +3  -4 -11 -18   +6  -1  -8 -15   +9  +2  -5 -12  +12  +5  -2  -9  +15  +8  +1  -6  +18 +11  +4  -3  +21 +14  +7   0
  //
  // Output:     g3  f3  e3   d3  c3  b3  a3       g2  f2  e2   d2  c2  b2  a2       g1  f1  e1   d1  c1  b1  a1       g0  f0  e0   d0  c0  b0  a0
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
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { unsigned int y; unsigned char b[4]; } u1, u2;

PRAGMA_OMP(parallel private(i,x1,x2,u1,u2) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 =                 (t6[i] << 16) | (t5[i] << 8) | t4[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12)                     | ((x2 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[2*i]   = (u2.b[1] << 24) | (u1.b[1] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[2*i+1] = (u2.b[3] << 24) | (u1.b[3] << 16) | (u2.b[2] << 8) | u1.b[2];
    }
  }
}

static void cornerturn_8thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
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
  unsigned int x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { unsigned int y; unsigned char b[4]; } u1, u2;

PRAGMA_OMP(parallel private(i,x1,x2,u1,u2) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
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

static void cornerturn_10thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 10 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                                                                        j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:                                                                                                       +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:                          j3  i3   h3  g3  f3  e3   d3  c3  b3  a3                            j2  i2   h2  g2  f2  e2   d2  c2  b2  a2                            j1  i1   h1  g1  f1  e1   d1  c1  b1  a1                            j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A resonable way to proceed seems to be to perform three separate 4-thread corner turns and then 
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
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2, x3;
  int i, n;
  n = outputDataSize/16;
  union { unsigned int y; unsigned char b[4]; } u1, u2, u3;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 = (t7[i] << 24) | (t6[i] << 16) | (t5[i] << 8) | t4[i];
      x3 =                                 (t9[i] << 8) | t8[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6)                     | ((x3 & M4) << 12)                     | ((x3 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[4*i]   = (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[4*i+1] = (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[4*i+2] = (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[4*i+3] = (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
    }
  }
}

static void cornerturn_12thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 12 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                                      l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:                                                                     +12 -3  -18 -33  +15  0  -15 -30  +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:                  l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3                    l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2                    l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1                    l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A resonable way to proceed seems to be to perform three separate 4-thread corner turns and then 
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
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2, x3;
  int i, n;
  n = outputDataSize/16;
  union { unsigned int y; unsigned char b[4]; } u1, u2, u3;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i] << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i] << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i] << 8) | t8[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[4*i]   = (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[4*i+1] = (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[4*i+2] = (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[4*i+3] = (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
    }
  }
}

static void cornerturn_14thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 14 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                    n3  n2  n1  n0   m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:                                   +6  -9  -24 -39  +9  -6  -21 -36  +12 -3  -18 -33  +15  0  -15 -30  +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:         n3  m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3           n2  m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2           n1  m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1           n0  m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
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
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;
  unsigned int x1, x2, x3, x4;
  int i, n;
  n = outputDataSize/16;
  union { unsigned int y; unsigned char b[4]; } u1, u2, u3, u4;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3,u4) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 =                                   (t13[i] << 8) | t12[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      u4.y = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6)                     | ((x4 & M4) << 12)                     | ((x4 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[4*i]   = (u4.b[0] << 24) | (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[4*i+1] = (u4.b[1] << 24) | (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[4*i+2] = (u4.b[2] << 24) | (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[4*i+3] = (u4.b[3] << 24) | (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
    }
  }
}

static void cornerturn_16thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 16 threads of 2-bit data.
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
  unsigned int x1, x2, x3, x4;
  int i, n;
  n = outputDataSize/16;
  union { unsigned int y; unsigned char b[4]; } u1, u2, u3, u4;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3,u4) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      u4.y = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6) | ((x4 & M3) >> 12) | ((x4 & M4) << 12) | ((x4 & M5) >> 18) | ((x4 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[4*i]   = (u4.b[0] << 24) | (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[4*i+1] = (u4.b[1] << 24) | (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[4*i+2] = (u4.b[2] << 24) | (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[4*i+3] = (u4.b[3] << 24) | (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
    }
  }
}

static void cornerturn_2thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 2 threads of 4-bit data.
  //
  // Thread: --1---   --0---   --1---   --0---
  // Byte:   --1---   --1---   --0---   --0---
  // Input:  b3  b2   a3  a2   b1  b0   a1  a0
  //
  // Shift:   0  -1   +1   0    0  -1   +1   0
  //
  // Output: b3  a3   b2  a2   b1  a1   b0  a0
  // Byte:   --3---   --2---   --1---   --0---

  const unsigned int M0 = 0xF00FF00F;
  const unsigned int M1 = 0x0F000F00;
  const unsigned int M2 = 0x00F000F0;

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;

  unsigned int x;
  int i, n;
  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t1[2*i+1] << 24) | (t0[2*i+1] << 16) | (t1[2*i] << 8) | t0[2*i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 4) | ((x & M2) << 4);
    }
  }
}

static void cornerturn_3thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 3 threads of 4-bit data.
  //
  // Thread: --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---
  // Input:  d1  d0   c1  c0   b1  b0   a1  a0
  //
  // Shift:   0  -3   +1  -2   +2  -1   +3   0
  //
  // Output: d1  c1   b1  a1   d0  c0   b0  a0
  // Byte:   --3---   --2---   --1---   --0---

  const unsigned int M0 = 0x0000000F;
  const unsigned int M2 = 0x00F00000;
  const unsigned int M3 = 0x000F0000;
  const unsigned int M4 = 0x0000F000;
  const unsigned int M5 = 0x00000F00;
  const unsigned int M6 = 0x000000F0;

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  const unsigned char *t2 = threadBuffers[2];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;

  unsigned int x;
  int i, n;
  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,t2,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t2[i] << 16) | (t1[i] << 8) | t0[i];

      // mask and shift
      outputwordptr[i] = (x & M0)                    | ((x & M2) << 4) | ((x & M3) >> 8) | ((x & M4) << 8) | ((x & M5) >> 4) | ((x & M6) << 12);
    }
  }
}

static void cornerturn_4thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 4 threads of 4-bit data.
  //
  // Thread: --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---
  // Input:  d1  d0   c1  c0   b1  b0   a1  a0
  //
  // Shift:   0  -3   +1  -2   +2  -1   +3   0
  //
  // Output: d1  c1   b1  a1   d0  c0   b0  a0
  // Byte:   --3---   --2---   --1---   --0---

  const unsigned int M0 = 0xF000000F;
  const unsigned int M1 = 0x0F000000;
  const unsigned int M2 = 0x00F00000;
  const unsigned int M3 = 0x000F0000;
  const unsigned int M4 = 0x0000F000;
  const unsigned int M5 = 0x00000F00;
  const unsigned int M6 = 0x000000F0;

  const unsigned char *t0 = threadBuffers[0];
  const unsigned char *t1 = threadBuffers[1];
  const unsigned char *t2 = threadBuffers[2];
  const unsigned char *t3 = threadBuffers[3];
  unsigned int *outputwordptr = (unsigned int *)outputBuffer;

  unsigned int x;
  int i, n;
  n = outputDataSize/4;

PRAGMA_OMP(parallel private(i,x) shared(outputwordptr,t0,t1,t2,t3,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble
      x = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];

      // mask and shift
      outputwordptr[i] = (x & M0) | ((x & M1) >> 12) | ((x & M2) << 4) | ((x & M3) >> 8) | ((x & M4) << 8) | ((x & M5) >> 4) | ((x & M6) << 12);
    }
  }
}

static void cornerturn_5thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 5 threads of 4-bit data.
  //
  // Thread: --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                             e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //
  // Order:                               -2         +2       -1      +3       0
  //
  // Thread: --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                    d1  d0   b1  b0            e1  e0   c1  c0   a1  a0
  //
  //         ----------------xo---------------   ----------------xe---------------
  //
  // Shift:                     0  -7    0  -7            +7   0   +7   0   +7   0
  //                                          
  // Output:              e1   d1  c1   b1  a1                e0   d0  c0   b0  a0
  // Byte:   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
 
  const unsigned int M0 = 0x00F0F0F0;
  const unsigned int M1 = 0x000F0F0F;

  unsigned long long int *outputwordptr = (unsigned long long int *)outputBuffer;

  unsigned int xe, xo;
  int i, n;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,xo,xe) shared(outputwordptr,t0,t1,t2,t3,t4,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      xo =                 (t3[i] << 8) | t1[i];
      xe = (t4[i] << 16) | (t2[i] << 8) | t0[i];

      outputwordptr[i] = ((xo & M0) * 0x100000000LL) | ((xo & M1) * 0x10LL) | ((xe & M0) * 0x10000000LL) | (xe & M1);
    }
  }
}

static void cornerturn_6thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 6 threads of 4-bit data.
  //
  // Thread: --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                    f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //
  // Order:                       +1      -2         +2       -1      +3       0
  //
  // Thread: --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:           f1  f0   d1  d0   b1  b0            e1  e0   c1  c0   a1  a0
  //
  //         ----------------xo---------------   ----------------xe---------------
  //
  // Shift:            0  -7    0  -7    0  -7            +7   0   +7   0   +7   0
  //                                          
  // Output:          f1  e1   d1  c1   b1  a1            f0  e0   d0  c0   b0  a0
  // Byte:   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];
 
  const unsigned int M0 = 0x00F0F0F0;
  const unsigned int M1 = 0x000F0F0F;

  unsigned long long int *outputwordptr = (unsigned long long int *)outputBuffer;

  unsigned int xe, xo;
  int i, n;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,xo,xe) shared(outputwordptr,t0,t1,t2,t3,t4,t5,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      xo = (t5[i] << 16) | (t3[i] << 8) | t1[i];
      xe = (t4[i] << 16) | (t2[i] << 8) | t0[i];

      outputwordptr[i] = ((xo & M0) * 0x100000000LL) | ((xo & M1) * 0x10LL) | ((xe & M0) * 0x10000000LL) | (xe & M1);
    }
  }
}

static void cornerturn_7thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 7 threads of 4-bit data.
  //
  // Thread: --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:           g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //
  // Order:              -3       +1      -2         +2       -1      +3       0
  //
  // Thread: --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:           f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //
  //         ----------------xo---------------   ----------------xe---------------
  //
  // Shift:            0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                          
  // Output:     g1   f1  e1   d1  c1   b1  a1       g0   f0  e0   d0  c0   b0  a0
  // Byte:   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];
  const unsigned char *t6  = threadBuffers[6];
 
  const unsigned int M0 = 0xF0F0F0F0;
  const unsigned int M1 = 0x0F0F0F0F;

  unsigned long long int *outputwordptr = (unsigned long long int *)outputBuffer;

  unsigned int xe, xo;
  int i, n;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,xo,xe) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      xo =                 (t5[i] << 16) | (t3[i] << 8) | t1[i];
      xe = (t6[i] << 24) | (t4[i] << 16) | (t2[i] << 8) | t0[i];

      outputwordptr[i] = ((xo & M0) * 0x100000000LL) | ((xo & M1) * 0x10LL) | ((xe & M0) * 0x10000000LL) | (xe & M1);
    }
  }
}

static void cornerturn_8thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 8 threads of 4-bit data.
  //
  // Thread: --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:  h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //
  // Order:    0         -3       +1      -2         +2       -1      +3       0
  //
  // Thread: --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:  h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //
  //         ----------------xo---------------   ----------------xe---------------
  //
  // Shift:   0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                          
  // Output: h1  g1   f1  e1   d1  c1   b1  a1   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];
  const unsigned char *t6  = threadBuffers[6];
  const unsigned char *t7  = threadBuffers[7];
 
  const unsigned int M0 = 0xF0F0F0F0;
  const unsigned int M1 = 0x0F0F0F0F;

  unsigned long long int *outputwordptr = (unsigned long long int *)outputBuffer;

  unsigned int xe, xo;
  int i, n;

  n = outputDataSize/8;

PRAGMA_OMP(parallel private(i,xo,xe) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      xo = (t7[i] << 24) | (t5[i] << 16) | (t3[i] << 8) | t1[i];
      xe = (t6[i] << 24) | (t4[i] << 16) | (t2[i] << 8) | t0[i];

      outputwordptr[i] = ((xo & M0) * 0x100000000LL) | ((xo & M1) * 0x10LL) | ((xe & M0) * 0x10000000LL) | (xe & M1);
    }
  }
}

static void cornerturn_2thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];

  n = outputDataSize/2;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[2*i]   = t0[i];
    outputBuffer[2*i+1] = t1[i];
  }
}

static void cornerturn_3thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];

  n = outputDataSize/4;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[4*i]   = t0[i];
    outputBuffer[4*i+1] = t1[i];
    outputBuffer[4*i+2] = t2[i];
  }
}

static void cornerturn_4thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];

  n = outputDataSize/4;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[4*i]   = t0[i];
    outputBuffer[4*i+1] = t1[i];
    outputBuffer[4*i+2] = t2[i];
    outputBuffer[4*i+3] = t3[i];
  }
}

static void cornerturn_5thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[8*i]   = t0[i];
    outputBuffer[8*i+1] = t1[i];
    outputBuffer[8*i+2] = t2[i];
    outputBuffer[8*i+3] = t3[i];
    outputBuffer[8*i+4] = t4[i];
  }
}

static void cornerturn_6thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[8*i]   = t0[i];
    outputBuffer[8*i+1] = t1[i];
    outputBuffer[8*i+2] = t2[i];
    outputBuffer[8*i+3] = t3[i];
    outputBuffer[8*i+4] = t4[i];
    outputBuffer[8*i+5] = t5[i];
  }
}

static void cornerturn_7thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];
  const unsigned char *t6  = threadBuffers[6];

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[8*i]   = t0[i];
    outputBuffer[8*i+1] = t1[i];
    outputBuffer[8*i+2] = t2[i];
    outputBuffer[8*i+3] = t3[i];
    outputBuffer[8*i+4] = t4[i];
    outputBuffer[8*i+5] = t5[i];
    outputBuffer[8*i+6] = t6[i];
  }
}

static void cornerturn_8thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const unsigned char *t0  = threadBuffers[0];
  const unsigned char *t1  = threadBuffers[1];
  const unsigned char *t2  = threadBuffers[2];
  const unsigned char *t3  = threadBuffers[3];
  const unsigned char *t4  = threadBuffers[4];
  const unsigned char *t5  = threadBuffers[5];
  const unsigned char *t6  = threadBuffers[6];
  const unsigned char *t7  = threadBuffers[7];

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[8*i]   = t0[i];
    outputBuffer[8*i+1] = t1[i];
    outputBuffer[8*i+2] = t2[i];
    outputBuffer[8*i+3] = t3[i];
    outputBuffer[8*i+4] = t4[i];
    outputBuffer[8*i+5] = t5[i];
    outputBuffer[8*i+6] = t6[i];
    outputBuffer[8*i+7] = t7[i];
  }
}

static void cornerturn_10thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
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

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[16*i]    = t0[i];
    outputBuffer[16*i+1]  = t1[i];
    outputBuffer[16*i+2]  = t2[i];
    outputBuffer[16*i+3]  = t3[i];
    outputBuffer[16*i+4]  = t4[i];
    outputBuffer[16*i+5]  = t5[i];
    outputBuffer[16*i+6]  = t6[i];
    outputBuffer[16*i+7]  = t7[i];
    outputBuffer[16*i+8]  = t8[i];
    outputBuffer[16*i+9]  = t9[i];
  }
}

static void cornerturn_12thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
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

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[16*i]    = t0[i];
    outputBuffer[16*i+1]  = t1[i];
    outputBuffer[16*i+2]  = t2[i];
    outputBuffer[16*i+3]  = t3[i];
    outputBuffer[16*i+4]  = t4[i];
    outputBuffer[16*i+5]  = t5[i];
    outputBuffer[16*i+6]  = t6[i];
    outputBuffer[16*i+7]  = t7[i];
    outputBuffer[16*i+8]  = t8[i];
    outputBuffer[16*i+9]  = t9[i];
    outputBuffer[16*i+10] = t10[i];
    outputBuffer[16*i+11] = t11[i];
  }
}

static void cornerturn_14thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
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

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[16*i]    = t0[i];
    outputBuffer[16*i+1]  = t1[i];
    outputBuffer[16*i+2]  = t2[i];
    outputBuffer[16*i+3]  = t3[i];
    outputBuffer[16*i+4]  = t4[i];
    outputBuffer[16*i+5]  = t5[i];
    outputBuffer[16*i+6]  = t6[i];
    outputBuffer[16*i+7]  = t7[i];
    outputBuffer[16*i+8]  = t8[i];
    outputBuffer[16*i+9]  = t9[i];
    outputBuffer[16*i+10] = t10[i];
    outputBuffer[16*i+11] = t11[i];
    outputBuffer[16*i+12] = t12[i];
    outputBuffer[16*i+13] = t13[i];
  }
}

static void cornerturn_16thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
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

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[16*i]    = t0[i];
    outputBuffer[16*i+1]  = t1[i];
    outputBuffer[16*i+2]  = t2[i];
    outputBuffer[16*i+3]  = t3[i];
    outputBuffer[16*i+4]  = t4[i];
    outputBuffer[16*i+5]  = t5[i];
    outputBuffer[16*i+6]  = t6[i];
    outputBuffer[16*i+7]  = t7[i];
    outputBuffer[16*i+8]  = t8[i];
    outputBuffer[16*i+9]  = t9[i];
    outputBuffer[16*i+10] = t10[i];
    outputBuffer[16*i+11] = t11[i];
    outputBuffer[16*i+12] = t12[i];
    outputBuffer[16*i+13] = t13[i];
    outputBuffer[16*i+14] = t14[i];
    outputBuffer[16*i+15] = t15[i];
  }
}


static void (*getCornerTurner(int nThread, int nBit))(unsigned char *, const unsigned char * const *, int)
{
	if(nThread == 1)
	{
		return cornerturn_1thread;
	}
	else if(nBit == 1)
	{
		switch(nThread)
		{
		/* First list the most common ones: powers of two */
		case 2:
			return cornerturn_2thread_1bit;
		case 4:
			return cornerturn_4thread_1bit;
		case 8:
			return cornerturn_8thread_1bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_1bit;
		case 5:
			return cornerturn_5thread_1bit;
		case 6:
			return cornerturn_6thread_1bit;
		case 7:
			return cornerturn_7thread_1bit;
		/* Alternate versions of some corner turners can be specified with negative values */
		case -2:
			return cornerturn_2thread_1bit_slow;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 2)
	{
		switch(nThread)
		{
		/* First list the most common ones: powers of two */
		case 2:
			return cornerturn_2thread_2bit;
		case 4:
			return cornerturn_4thread_2bit;
		case 8:
			return cornerturn_8thread_2bit;
		case 16:
			return cornerturn_16thread_2bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_2bit;
		case 5:
			return cornerturn_5thread_2bit;
		case 6:
			return cornerturn_6thread_2bit;
		case 7:
			return cornerturn_7thread_2bit;
		case 10:
			return cornerturn_10thread_2bit;
		case 12:
			return cornerturn_12thread_2bit;
		case 14:
			return cornerturn_14thread_2bit;
		/* Alternate versions of some corner turners can be specified with negative values */
		case -2:
			return cornerturn_2thread_2bit_slow;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 4)
	{
		switch(nThread)
		{
		/* First list the most common ones: powers of two */
		case 2:
			return cornerturn_2thread_4bit;
		case 4:
			return cornerturn_4thread_4bit;
		case 8:
			return cornerturn_8thread_4bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_4bit;
		case 5:
			return cornerturn_5thread_4bit;
		case 6:
			return cornerturn_6thread_4bit;
		case 7:
			return cornerturn_7thread_4bit;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 8)
	{
		switch(nThread)
		{
		/* First list the most common ones: powers of two */
		case 2:
			return cornerturn_2thread_8bit;
		case 4:
			return cornerturn_4thread_8bit;
		case 8:
			return cornerturn_8thread_8bit;
		case 16:
			return cornerturn_16thread_8bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_8bit;
		case 5:
			return cornerturn_5thread_8bit;
		case 6:
			return cornerturn_6thread_8bit;
		case 7:
			return cornerturn_7thread_8bit;
		case 10:
			return cornerturn_10thread_8bit;
		case 12:
			return cornerturn_12thread_8bit;
		case 14:
			return cornerturn_14thread_8bit;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else
	{
		return 0;
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
 *
 * see ../utils/vmux.c for example usage of this function
 */

int vdifmux(unsigned char *dest, int destSize, const unsigned char *src, int srcSize, int inputFrameSize, int inputFramesPerSecond, int nBit, int nThread, const int *threadIds, int nSort, int nGap, long long startOutputFrameNumber, struct vdif_mux_statistics *stats)
{
	const int maxThreads = 1024;
	unsigned char chanIndex[maxThreads];	/* map from threadId to channel number (0 to nThread-1) */
	int threadId;
	int nValidFrame = 0;			/* counts number of valid input frames found so far */
	int nSkip = 0;				/* counts number of bytes skipped (not what we are looking for) */
	int nFill = 0;				/* counts number of bytes skipped that were fill pattern */
	int nDup = 0;				/* number of duplicate frames found */
	int nInvalidFrame = 0;			/* counts number of VDIF frames skipped because of invalid bits */
	long long startFrameNumber;		/* = seconds*inputFramesPerSecond + frameNumber */
	int frameGranularity;			/* number of frames required to make an integer number of nanoseconds */

	int i;					/* index into src */
	int f;
	int N = srcSize - inputFrameSize;	/* max value to allow i to be */
	int highestDestIndex = 0;
	int maxDestIndex;
	int maxSrcIndex;
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
	int seconds, frameNum;
	vdif_header outputHeader;
	int epoch = -1;
	int highestSortedDestIndex = -1;
	int vhUnset = 1;

	void (*cornerTurner)(unsigned char *, const unsigned char * const *, int);

	cornerTurner = getCornerTurner(nThread, nBit);
	if(cornerTurner == 0)
	{
		fprintf(stderr, "No corner turner implemented for %d threads and %d bits\n", nThread, nBit);

		return -3;
	}
	nThread = abs(nThread);	/* in case a special corner turner was requested */

	if(nSort <= 0)
	{
		maxSrcIndex = srcSize - inputFrameSize;
		nSort = -nSort;
	}
	else
	{
		maxSrcIndex = srcSize - nSort*inputFrameSize;
	}

	if(nGap < nSort)
	{
		nGap = nSort;
	}

	/* Choose a corner turning algorithm.  Currently the selection is limited to 2-bit cases or
	 * anything with 1 thread.  Others can be implemented as needed.
	 */

	for(nOutputChan = 1; nOutputChan < nThread; nOutputChan *= 2) ;

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

	/* clear mask of presence */
	for(i = 0; i <= maxDestIndex; ++i)
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
		}

		if(vhUnset)
		{
			memcpy(&outputHeader, vh, 16);
			memset(((char *)&outputHeader) + 16, 0, 16);

			/* use this first good frame to generate the prototype VDIF header for the output */
			setVDIFNumChannels(&outputHeader, nOutputChan);
			setVDIFThreadID(&outputHeader, 0);
			setVDIFFrameBytes(&outputHeader, outputFrameSize);
			setVDIFFrameInvalid(&outputHeader, 0);
			epoch = getVDIFEpoch(&outputHeader);

			vhUnset = 0;
		}

		destIndex = frameNumber - startFrameNumber;

		if(destIndex < 0)
		{
			/* no choice but to discard this data */
			i += inputFrameSize;
			nSkip += inputFrameSize;

			continue;
		}
		if(i > maxSrcIndex)
		{
			if(highestSortedDestIndex < 0)
			{
				highestSortedDestIndex = highestDestIndex;
			}
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
		else /* here we have a usable packet */
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

					/* clear mask of presence */
					for(destIndex = 0; destIndex < highestDestIndex; ++destIndex)
					{
						p = (uint32_t *)(dest + outputFrameSize*destIndex);
						p[7] = 0;
					}
					highestDestIndex = 0;

					/* at this point we're starting over, so there are no valid frames. */
					nSkip += nValidFrame*inputFrameSize;
					nValidFrame = 0;	

					destIndex = frameNumber - startFrameNumber;
					p = (uint32_t *)(dest + outputFrameSize*destIndex);
				}
			}

			/* set mask indicating valid data in place */
			if(p[7] & (1 << chanId))
			{
				++nDup;
			}
			else
			{
				/* NOTE!  We're using just a bit of the dest buffer to store pointers to the original data payloads */
				const unsigned char **threadBuffers = (const unsigned char **)(dest + outputFrameSize*destIndex + VDIF_HEADER_BYTES);

				p[7] |= (1 << chanId);
				threadBuffers[chanId] = cur + VDIF_HEADER_BYTES;	/* store pointer to data for later corner turning */
				
				++nValidFrame;

				if(destIndex > highestDestIndex)
				{
					highestDestIndex = destIndex;
				}
				
				/* Once we have nSort good frames, we know the earliest frame acceptable, so now scrunch forward... */
				if(nValidFrame == nSort && startOutputFrameNumber < 0)
				{
					int firstUsed;

					for(firstUsed = 0; firstUsed <= highestDestIndex; ++firstUsed)
					{
						p = (uint32_t *)(dest + outputFrameSize*firstUsed);

						if(p[7] > 0)
						{
							break;
						}
					}

					if(firstUsed > 0 && firstUsed <= highestDestIndex)
					{
						/* Ensure frame granularity conditions remain met */
						firstUsed -= (firstUsed % frameGranularity);

						/* slide data forward */
						for(f = firstUsed; f <= highestDestIndex; ++f)
						{
							int e = f-firstUsed;
							uint32_t *q;

							p = (uint32_t *)(dest + outputFrameSize*e);
							q = (uint32_t *)(dest + outputFrameSize*f);
							p[7] = q[7];

							if(p[7] != 0)
							{
								const unsigned char * const *threadBuffers2;

								threadBuffers  = (const unsigned char **)(dest + outputFrameSize*e + VDIF_HEADER_BYTES);
								threadBuffers2 = (const unsigned char **)(dest + outputFrameSize*f + VDIF_HEADER_BYTES);
								memcpy(threadBuffers, threadBuffers2, nThread*sizeof(const unsigned char *));
							}
						}

						/* zero any remaining masks */
						for(f = highestDestIndex+1-firstUsed; f <= highestDestIndex ; ++f)
						{
							uint32_t *q;

							q = (uint32_t *)(dest + outputFrameSize*f);
							q[7] = 0;
						}

						/* change a few other indexes */
						highestDestIndex -= firstUsed;
						highestSortedDestIndex -= firstUsed;
						startFrameNumber += firstUsed;
					}
				}
			}
			i += inputFrameSize;
		}
	}

	if(bytesProcessed == 0)
	{
		bytesProcessed = i;
	}

	/* If there were fewer than nSort frames read and start output frame number not specified, scrunch output to front */
	if(nValidFrame < nSort && startOutputFrameNumber < 0)
	{
		int firstUsed;
		uint32_t *p;

		for(firstUsed = 0; firstUsed <= highestDestIndex; ++firstUsed)
		{
			p = (uint32_t *)(dest + outputFrameSize*firstUsed);

			if(p[7] == goodMask)
			{
				break;
			}
		}
		if(firstUsed > 0 && firstUsed <= highestDestIndex)
		{
			/* Ensure frame granularity conditions remain met */
			firstUsed -= (firstUsed % frameGranularity);

			/* slide data forward */
			for(f = firstUsed; f <= highestDestIndex; ++f)
			{
				int e = f-firstUsed;
				uint32_t *q;

				p = (uint32_t *)(dest + outputFrameSize*e);
				q = (uint32_t *)(dest + outputFrameSize*f);
				p[7] = q[7];

				if(p[7] != 0)
				{
					const unsigned char **threadBuffers;
					const unsigned char **threadBuffers2;

					threadBuffers  = (const unsigned char **)(dest + outputFrameSize*e + VDIF_HEADER_BYTES);
					threadBuffers2 = (const unsigned char **)(dest + outputFrameSize*f + VDIF_HEADER_BYTES);
					memcpy(threadBuffers, threadBuffers2, nThread*sizeof(const unsigned char *));
				}
			}

			/* zero any remaining masks */
			for(f = highestDestIndex+1-firstUsed; f <= highestDestIndex ; ++f)
			{
				uint32_t *q;

				q = (uint32_t *)(dest + outputFrameSize*f);
				q[7] = 0;
			}

			/* change a few other indexes */
			highestDestIndex -= firstUsed;
			highestSortedDestIndex -= firstUsed;
			startFrameNumber += firstUsed;
		}
	}

	/* Here the source dried up, but we want to be able to reconstruct a complete stream later, so back up, a bit to look for most recent incomplete frame consistent with nSort */
	if(highestSortedDestIndex >= 0)
	{
		for(f = highestDestIndex; f > highestSortedDestIndex; --f)
		{
			const uint32_t *p = (const uint32_t *)(dest + outputFrameSize*f);
			uint32_t mask = p[7];

			if(mask != goodMask)
			{
				const unsigned char * const *threadBuffers = (const unsigned char **)(dest + outputFrameSize*f + VDIF_HEADER_BYTES);
				int t;
				
				highestDestIndex = f-1;
				for(t = 0; t < nThread; ++t)
				{
					if(mask & (1<<t))
					{
						int d;

						d = threadBuffers[t] - src - VDIF_HEADER_BYTES;	/* this is number of bytes into input stream */
						if(d < bytesProcessed)
						{
							bytesProcessed = d;
						}
						--nValidFrame;
					}
				}
			}
		}
	}
	else
	{
		int minDestIndex = highestDestIndex - nSort/nThread;

		if(minDestIndex >= 0) for(f = highestDestIndex; f >= minDestIndex; --f)
		{
			const uint32_t *p = (const uint32_t *)(dest + outputFrameSize*f);
			uint32_t mask = p[7];

			if(mask != goodMask)
			{
				const unsigned char * const *threadBuffers = (const unsigned char **)(dest + outputFrameSize*f + VDIF_HEADER_BYTES);
				int t;
				
				highestDestIndex = f-1;
				for(t = 0; t < nThread; ++t)
				{
					if(mask & (1<<t))
					{
						int d;

						d = threadBuffers[t] - src - VDIF_HEADER_BYTES;	/* this is number of bytes into input stream */
						if(d < bytesProcessed)
						{
							bytesProcessed = d;
						}
						--nValidFrame;
					}
				}
			}
		}
	}

	/* If source did not dry up and dest got within nGap of filling, set highestDestIndex to produce (invalid) data through the end of the dest buffer for continuity */
	if(i < N && highestDestIndex < maxDestIndex && highestDestIndex > maxDestIndex - nGap)
	{
		highestDestIndex = maxDestIndex;
	}

	/* Stage 2: do the corner turning and header population */

	seconds = startFrameNumber/inputFramesPerSecond;
	frameNum = startFrameNumber%inputFramesPerSecond;

	for(f = 0; f <= highestDestIndex; ++f)
	{
		unsigned char *frame = dest + outputFrameSize*f;	/* points to rearrangement destination */
		const uint32_t *p = (const uint32_t *)frame;
		uint32_t mask;

		mask = p[7];

		/* generate header for output frame */
		memcpy(frame, (const char *)&outputHeader, VDIF_HEADER_BYTES);
		setVDIFFrameSecond((vdif_header *)frame, seconds);
		setVDIFFrameNumber((vdif_header *)frame, frameNum);

		if(mask == goodMask)
		{
			const unsigned char * const *threadBuffers = (const unsigned char * const *)(frame + VDIF_HEADER_BYTES);

			/* Note: The following function only works because all of the corner turners make a copy of the
			 * thread pointers before beginning */
			cornerTurner(frame + VDIF_HEADER_BYTES, threadBuffers, outputDataSize);

			++nGoodOutput;
		}
		else
		{
			/* Set invalid bit */
			setVDIFFrameInvalid((vdif_header *)frame, 1);

			++nBadOutput;
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
		stats->nDuplicateFrame += nDup;
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
		stats->epoch = epoch;
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
		printf("  Number of duplicate frames         = %Ld\n", stats->nDuplicateFrame);
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
		printf("  Epoch                              = %d\n", stats->epoch);
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

static int testCornerTurn(const unsigned char *outputBuffer, const unsigned char * const *threadData, int outputBytes, int nt, int b)
{
	int nError = 0;
	int nSample = 8*outputBytes/b;	/* total samples in output stream */
	int s;
	int ut;	/* lowest power of 2 >= nt */
	unsigned int bitmask;

	for(ut = 1; ut < nt; ut *= 2);

	bitmask = (1 << b) - 1;

	for(s = 0; s < nSample; ++s)
	{
		int t;	/* thread */
		unsigned int outputvalue, inputvalue;
		int outputsample, outputshift, inputsample, inputshift;

		t = s % ut;
		if(t >= nt)
		{
			/* ignore padded data */
			continue;
		}

		outputsample = s*b / 8;
		outputshift = (s*b) % 8;
		outputvalue = (outputBuffer[outputsample] >> outputshift) & bitmask;

		inputsample = s*b/ut / 8;
		inputshift = (s*b/ut) % 8;
		inputshift /= b;
		inputshift *= b;
		inputvalue = (threadData[t][inputsample] >> inputshift) & bitmask;

		if(outputvalue != inputvalue)
		{
			++nError;
		}
	}

	return nError;
}

void testvdifcornerturners(int outputBytes, int nTest)
{
	const char devRandom[] = "/dev/urandom";
	const int bits[] = {1, 2, 4, 8, 0};
	const int maxThreads = 16;
	int bi;
	int t;
	unsigned char *threadData[maxThreads];
	unsigned char *outputBuffer;

	for(t = 0; t < maxThreads; ++t)
	{
		FILE *in;
		int nRead;
		threadData[t] = (unsigned char *)malloc(outputBytes);
		
		/* fill with random values */
		in = fopen(devRandom, "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open %s\n", devRandom);

			return;
		}

		nRead = fread(threadData[t], 1, outputBytes, in);
		if(nRead <= 0)
		{
			fprintf(stderr, "Error: cannot read from %s\n", devRandom);

			return;
		}

		fclose(in);
	}
	outputBuffer = (unsigned char *)malloc(outputBytes);

	for(bi = 0; bits[bi]; ++bi)
	{
		int b = bits[bi];
		int nt;
		for(nt = -maxThreads; nt <= maxThreads; ++nt)
		{
			int i;
			void (*cornerTurner)(unsigned char *, const unsigned char * const *, int);
			clock_t t0, t1;
			int nError;
			
			cornerTurner = getCornerTurner(nt, b);

			if(!cornerTurner)
			{
				continue;
			}
			printf("%d bits  %d threads...  ", b, nt);
			fflush(stdout);

			t0 = clock();
			for(i = 0; i < nTest; ++i)
			{
				cornerTurner(outputBuffer, (const unsigned char * const*)threadData, outputBytes);
			}
			t1 = clock();
			if(t1 > t0)
			{
				printf("Took %d microseconds -> %0.0f Mbps", (int)(t1-t0), (8.0*nTest*outputBytes/(t1-t0)));
			}
			else
			{
				printf("Weird; took 0 time.");
			}

			nError = testCornerTurn(outputBuffer, (const unsigned char * const*)threadData, outputBytes, abs(nt), b);
			printf("   %d samples of %d were wrong.\n", nError, 8*outputBytes/b);
		}
	}

	free(outputBuffer);
	for(t = 0; t < maxThreads; ++t)
	{
		free(threadData[t]);
	}
}
