/***************************************************************************
 *   Copyright (C) 2013-2024 Walter Brisken                                *
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

#include <string.h>
#include <stdlib.h>

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

#include "vdifio.h"

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

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
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

  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
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

static void cornerturn_9thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      B.u64 = a; 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_10thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_11thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_12thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_13thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_14thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_15thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t14[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_16thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t14[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t15[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = A.u8[1];
      outputwordptr[3]  = B.u8[1];
      outputwordptr[4]  = A.u8[2];
      outputwordptr[5]  = B.u8[2];
      outputwordptr[6]  = A.u8[3];
      outputwordptr[7]  = B.u8[3];
      outputwordptr[8]  = A.u8[4];
      outputwordptr[9]  = B.u8[4];
      outputwordptr[10] = A.u8[5];
      outputwordptr[11] = B.u8[5];
      outputwordptr[12] = A.u8[6];
      outputwordptr[13] = B.u8[6];
      outputwordptr[14] = A.u8[7];
      outputwordptr[15] = B.u8[7];
      outputwordptr += 16;
    }
  }
}

static void cornerturn_20thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B, C;

  n = outputDataSize/32;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B,C) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t14[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t15[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t16[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t17[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t18[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t19[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      C.u64 = a | (b << 1) | (c << 2) | (d << 3); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = C.u8[0];
      outputwordptr[4]  = A.u8[1];
      outputwordptr[5]  = B.u8[1];
      outputwordptr[6]  = C.u8[1];
      outputwordptr[8]  = A.u8[2];
      outputwordptr[9]  = B.u8[2];
      outputwordptr[10] = C.u8[2];
      outputwordptr[12] = A.u8[3];
      outputwordptr[13] = B.u8[3];
      outputwordptr[14] = C.u8[3];
      outputwordptr[16] = A.u8[4];
      outputwordptr[17] = B.u8[4];
      outputwordptr[18] = C.u8[4];
      outputwordptr[20] = A.u8[5];
      outputwordptr[21] = B.u8[5];
      outputwordptr[22] = C.u8[5];
      outputwordptr[24] = A.u8[6];
      outputwordptr[25] = B.u8[6];
      outputwordptr[26] = C.u8[6];
      outputwordptr[28] = A.u8[7];
      outputwordptr[29] = B.u8[7];
      outputwordptr[30] = C.u8[7];
      outputwordptr += 32;
    }
  }
}

static void cornerturn_24thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B, C;

  n = outputDataSize/32;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B,C) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t14[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t15[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t16[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t17[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t18[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t19[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t20[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t21[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t22[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t23[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      C.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = C.u8[0];
      outputwordptr[4]  = A.u8[1];
      outputwordptr[5]  = B.u8[1];
      outputwordptr[6]  = C.u8[1];
      outputwordptr[8]  = A.u8[2];
      outputwordptr[9]  = B.u8[2];
      outputwordptr[10] = C.u8[2];
      outputwordptr[12] = A.u8[3];
      outputwordptr[13] = B.u8[3];
      outputwordptr[14] = C.u8[3];
      outputwordptr[16] = A.u8[4];
      outputwordptr[17] = B.u8[4];
      outputwordptr[18] = C.u8[4];
      outputwordptr[20] = A.u8[5];
      outputwordptr[21] = B.u8[5];
      outputwordptr[22] = C.u8[5];
      outputwordptr[24] = A.u8[6];
      outputwordptr[25] = B.u8[6];
      outputwordptr[26] = C.u8[6];
      outputwordptr[28] = A.u8[7];
      outputwordptr[29] = B.u8[7];
      outputwordptr[30] = C.u8[7];
      outputwordptr += 32;
    }
  }
}

static void cornerturn_32thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B, C, D;

  n = outputDataSize/32;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B,C,D) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t14[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t15[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t16[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t17[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t18[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t19[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t20[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t21[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t22[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t23[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      C.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t24[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t25[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t26[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t27[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t28[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t29[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t30[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t31[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      D.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = C.u8[0];
      outputwordptr[3]  = D.u8[0];
      outputwordptr[4]  = A.u8[1];
      outputwordptr[5]  = B.u8[1];
      outputwordptr[6]  = C.u8[1];
      outputwordptr[7]  = D.u8[1];
      outputwordptr[8]  = A.u8[2];
      outputwordptr[9]  = B.u8[2];
      outputwordptr[10] = C.u8[2];
      outputwordptr[11] = D.u8[2];
      outputwordptr[12] = A.u8[3];
      outputwordptr[13] = B.u8[3];
      outputwordptr[14] = C.u8[3];
      outputwordptr[15] = D.u8[3];
      outputwordptr[16] = A.u8[4];
      outputwordptr[17] = B.u8[4];
      outputwordptr[18] = C.u8[4];
      outputwordptr[19] = D.u8[4];
      outputwordptr[20] = A.u8[5];
      outputwordptr[21] = B.u8[5];
      outputwordptr[22] = C.u8[5];
      outputwordptr[23] = D.u8[5];
      outputwordptr[24] = A.u8[6];
      outputwordptr[25] = B.u8[6];
      outputwordptr[26] = C.u8[6];
      outputwordptr[27] = D.u8[6];
      outputwordptr[28] = A.u8[7];
      outputwordptr[29] = B.u8[7];
      outputwordptr[30] = C.u8[7];
      outputwordptr[31] = D.u8[7];
      outputwordptr += 32;
    }
  }
}

static void cornerturn_64thread_1bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
  const uint8_t *t32 = (const uint8_t *)(threadBuffers[32]);
  const uint8_t *t33 = (const uint8_t *)(threadBuffers[33]);
  const uint8_t *t34 = (const uint8_t *)(threadBuffers[34]);
  const uint8_t *t35 = (const uint8_t *)(threadBuffers[35]);
  const uint8_t *t36 = (const uint8_t *)(threadBuffers[36]);
  const uint8_t *t37 = (const uint8_t *)(threadBuffers[37]);
  const uint8_t *t38 = (const uint8_t *)(threadBuffers[38]);
  const uint8_t *t39 = (const uint8_t *)(threadBuffers[39]);
  const uint8_t *t40 = (const uint8_t *)(threadBuffers[40]);
  const uint8_t *t41 = (const uint8_t *)(threadBuffers[41]);
  const uint8_t *t42 = (const uint8_t *)(threadBuffers[42]);
  const uint8_t *t43 = (const uint8_t *)(threadBuffers[43]);
  const uint8_t *t44 = (const uint8_t *)(threadBuffers[44]);
  const uint8_t *t45 = (const uint8_t *)(threadBuffers[45]);
  const uint8_t *t46 = (const uint8_t *)(threadBuffers[46]);
  const uint8_t *t47 = (const uint8_t *)(threadBuffers[47]);
  const uint8_t *t48 = (const uint8_t *)(threadBuffers[48]);
  const uint8_t *t49 = (const uint8_t *)(threadBuffers[49]);
  const uint8_t *t50 = (const uint8_t *)(threadBuffers[50]);
  const uint8_t *t51 = (const uint8_t *)(threadBuffers[51]);
  const uint8_t *t52 = (const uint8_t *)(threadBuffers[52]);
  const uint8_t *t53 = (const uint8_t *)(threadBuffers[53]);
  const uint8_t *t54 = (const uint8_t *)(threadBuffers[54]);
  const uint8_t *t55 = (const uint8_t *)(threadBuffers[55]);
  const uint8_t *t56 = (const uint8_t *)(threadBuffers[56]);
  const uint8_t *t57 = (const uint8_t *)(threadBuffers[57]);
  const uint8_t *t58 = (const uint8_t *)(threadBuffers[58]);
  const uint8_t *t59 = (const uint8_t *)(threadBuffers[59]);
  const uint8_t *t60 = (const uint8_t *)(threadBuffers[60]);
  const uint8_t *t61 = (const uint8_t *)(threadBuffers[61]);
  const uint8_t *t62 = (const uint8_t *)(threadBuffers[62]);
  const uint8_t *t63 = (const uint8_t *)(threadBuffers[63]);
  uint8_t *outputwordptr = (uint8_t *)outputBuffer;
  int i, n;
  uint64_t a, b, c, d, e, f, g, h;
  union { uint64_t u64; uint8_t u8[8]; } A, B, C, D, E, F, G, H;

  n = outputDataSize/64;

PRAGMA_OMP(parallel private(i,a,b,c,d,e,f,g,h,A,B,C,D,E,F,G,H) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,n))
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

      A.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t8[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t9[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t10[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t11[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t12[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t13[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t14[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t15[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      B.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t16[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t17[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t18[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t19[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t20[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t21[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t22[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t23[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      C.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t24[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t25[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t26[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t27[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t28[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t29[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t30[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t31[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      D.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t32[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t33[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t34[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t35[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t36[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t37[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t38[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t39[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      E.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t40[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t41[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t42[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t43[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t44[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t45[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t46[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t47[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      F.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t48[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t49[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t50[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t51[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t52[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t53[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t54[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t55[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      G.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 

      a = t56[i];
      a = (a | (a << 28));
      a = (a | (a << 14));
      a = (a | (a << 7)) & 0x0101010101010101LL;

      b = t57[i];
      b = (b | (b << 28));
      b = (b | (b << 14));
      b = (b | (b << 7)) & 0x0101010101010101LL;

      c = t58[i];
      c = (c | (c << 28));
      c = (c | (c << 14));
      c = (c | (c << 7)) & 0x0101010101010101LL;

      d = t59[i];
      d = (d | (d << 28));
      d = (d | (d << 14));
      d = (d | (d << 7)) & 0x0101010101010101LL;

      e = t60[i];
      e = (e | (e << 28));
      e = (e | (e << 14));
      e = (e | (e << 7)) & 0x0101010101010101LL;

      f = t61[i];
      f = (f | (f << 28));
      f = (f | (f << 14));
      f = (f | (f << 7)) & 0x0101010101010101LL;

      g = t62[i];
      g = (g | (g << 28));
      g = (g | (g << 14));
      g = (g | (g << 7)) & 0x0101010101010101LL;

      h = t63[i];
      h = (h | (h << 28));
      h = (h | (h << 14));
      h = (h | (h << 7)) & 0x0101010101010101LL;

      H.u64 = a | (b << 1) | (c << 2) | (d << 3) | (e << 4) | (f << 5) | (g << 6) | (h << 7); 


      outputwordptr[0]  = A.u8[0];
      outputwordptr[1]  = B.u8[0];
      outputwordptr[2]  = C.u8[0];
      outputwordptr[3]  = D.u8[0];
      outputwordptr[4]  = E.u8[0];
      outputwordptr[5]  = F.u8[0];
      outputwordptr[6]  = G.u8[0];
      outputwordptr[7]  = H.u8[0];
      outputwordptr[8]  = A.u8[1];
      outputwordptr[9]  = B.u8[1];
      outputwordptr[10] = C.u8[1];
      outputwordptr[11] = D.u8[1];
      outputwordptr[12] = E.u8[1];
      outputwordptr[13] = F.u8[1];
      outputwordptr[14] = G.u8[1];
      outputwordptr[15] = H.u8[1];
      outputwordptr[16] = A.u8[2];
      outputwordptr[17] = B.u8[2];
      outputwordptr[18] = C.u8[2];
      outputwordptr[19] = D.u8[2];
      outputwordptr[20] = E.u8[2];
      outputwordptr[21] = F.u8[2];
      outputwordptr[22] = G.u8[2];
      outputwordptr[23] = H.u8[2];
      outputwordptr[24] = A.u8[3];
      outputwordptr[25] = B.u8[3];
      outputwordptr[26] = C.u8[3];
      outputwordptr[27] = D.u8[3];
      outputwordptr[28] = E.u8[3];
      outputwordptr[29] = F.u8[3];
      outputwordptr[30] = G.u8[3];
      outputwordptr[31] = H.u8[3];
      outputwordptr[32] = A.u8[4];
      outputwordptr[33] = B.u8[4];
      outputwordptr[34] = C.u8[4];
      outputwordptr[35] = D.u8[4];
      outputwordptr[36] = E.u8[4];
      outputwordptr[37] = F.u8[4];
      outputwordptr[38] = G.u8[4];
      outputwordptr[39] = H.u8[4];
      outputwordptr[40] = A.u8[5];
      outputwordptr[41] = B.u8[5];
      outputwordptr[42] = C.u8[5];
      outputwordptr[43] = D.u8[5];
      outputwordptr[44] = E.u8[5];
      outputwordptr[45] = F.u8[5];
      outputwordptr[46] = G.u8[5];
      outputwordptr[47] = H.u8[5];
      outputwordptr[48] = A.u8[6];
      outputwordptr[49] = B.u8[6];
      outputwordptr[50] = C.u8[6];
      outputwordptr[51] = D.u8[6];
      outputwordptr[52] = E.u8[6];
      outputwordptr[53] = F.u8[6];
      outputwordptr[54] = G.u8[6];
      outputwordptr[55] = H.u8[6];
      outputwordptr[56] = A.u8[7];
      outputwordptr[57] = B.u8[7];
      outputwordptr[58] = C.u8[7];
      outputwordptr[59] = D.u8[7];
      outputwordptr[60] = E.u8[7];
      outputwordptr[61] = F.u8[7];
      outputwordptr[62] = G.u8[7];
      outputwordptr[63] = H.u8[7];
      outputwordptr += 64;
    }
  }
}


/* For 2-bit 2-thread corner turning there are two implementations here.  One optimized for 64-bit and the other for 32-bit.
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

  const uint64_t M0 = 0xC003C003C003C003ULL;
  const uint64_t M1 = 0x3000300030003000ULL;
  const uint64_t M2 = 0x000C000C000C000CULL;
  const uint64_t M3 = 0x0C000C000C000C00ULL;
  const uint64_t M4 = 0x0030003000300030ULL;
  const uint64_t M5 = 0x0300030003000300ULL;
  const uint64_t M6 = 0x00C000C000C000C0ULL;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
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

  const uint32_t M0 = 0xC003C003;
  const uint32_t M1 = 0x30003000;
  const uint32_t M2 = 0x000C000C;
  const uint32_t M3 = 0x0C000C00;
  const uint32_t M4 = 0x00300030;
  const uint32_t M5 = 0x03000300;
  const uint32_t M6 = 0x00C000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;

  uint32_t x;
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

  const uint32_t M0 = 0x00300C03;
  const uint32_t M1 = 0x000C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x00030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;

  uint32_t x;
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
  // Efficiently handle the special case of 4 threads of 2-bit data.  Because nthread = samples/byte
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

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;

  uint32_t x;
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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform two separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { uint32_t y; uint8_t b[4]; } u1, u2;

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform two separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { uint32_t y; uint8_t b[4]; } u1, u2;

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform two separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { uint32_t y; uint8_t b[4]; } u1, u2;

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform two separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2;
  int i, n;
  n = outputDataSize/8;
  union { uint32_t y; uint8_t b[4]; } u1, u2;

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

static void cornerturn_9thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 9 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                                                                                         i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:                                                                                                                        +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:                              i3   h3  g3  f3  e3   d3  c3  b3  a3                                i2   h2  g2  f2  e2   d2  c2  b2  a2                                i1   h1  g1  f1  e1   d1  c1  b1  a1                                i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform three separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8 = (const uint8_t *)(threadBuffers[8]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i] << 24) | (t2[i] << 16) | (t1[i] << 8) | t0[i];
      x2 = (t7[i] << 24) | (t6[i] << 16) | (t5[i] << 8) | t4[i];
      x3 =                                                t8[i];

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform three separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8 = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9 = (const uint8_t *)(threadBuffers[9]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3;

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

static void cornerturn_11thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 11 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                                                       k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:                                                                                      +15  0  -15 -30  +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:                      k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3                        k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2                        k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1                        k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform three separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i] << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i] << 8) | t4[i];
      x3 =                  (t10[i] << 16) | (t9[i] << 8) | t8[i];

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform three separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3;

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

static void cornerturn_13thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 13 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                     m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:                                                    +9  -6  -21 -36  +12 -3  -18 -33  +15  0  -15 -30  +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:             m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3               m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2               m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1               m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3,u4) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 =                                                   t12[i];

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4;

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

static void cornerturn_15thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 15 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                   o3  o2  o1  o0   n3  n2  n1  n0   m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  // 
  // Shift:                  +3  -12 -27 -42  +6  -9  -24 -39  +9  -6  -21 -36  +12 -3  -18 -33  +15  0  -15 -30  +18 +3  -12 -27  +21 +6  -9  -24  +24 +9  -6  -21  +27 +12 -3  -18  +30 +15  0  -15  +33 +18 +3  -12  +36 +21 +6  -9   +39 +24 +9  -6   +42 +27 +12 -3   +45 +30 +15  0
  //                                                                                                                                                
  // Output:     o3  n3  m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3       o2  n2  m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2       o1  n1  m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1       o0  n0  m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,u1,u2,u3,u4) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 =                  (t14[i] << 16) | (t13[i] << 8) | t12[i];

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
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4;
  int i, n;
  n = outputDataSize/16;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4;

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

static void cornerturn_20thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 32 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  R3  p2  p1  p0   o3  o2  o1  o0   n3  n2  n1  n0   m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:  -48 -17 +14 +45  -51 -20 +11 +42  -54 -23 +8  +39  -57 -26 +5  +36  -60 -29 +2  +33  -63 -32 -1  +30  -66 -35 -4  +27  -69 -38 -7  +24  -72 -41 -10 +21  -75 -44 -13 +18  -78 -47 -16 +15  -81 -50 -19 +12  -84 -53 -22 +9   -87 -56 -25 +6   -90 -59 -28 +3   -93 -62 -31 +0 
  //                                                                                                                                                
  // Output:                                                    D1  C1  B1  A1   p1  o1  n1  m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1                                                      D0  C0  B0  A0   p0  o0  n0  m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // Thread: ------31------   ------30------   ------29------   ------28------   ------27------   ------26------   ------25------   ------24------   ------23------   ------22------   ------21------   ------20------   ------19------   ------18------   ------17------   ------16------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                                                                                                                                                                              D3  D2  D1  D0   C3  C2  C1  C0   B3  B2  B1  B0   A3  A2  A1  A0
  //                                                                                                                                                
  // Shift:                                                                                                                                                                                                             -36 -5  +26 +57  -39 -8  +23 +54  -42 -11 +20 +51  -45 -14 +17 +48
  //                                                                                                                                                
  // Output:                                                    D3  C3  B3  A3   p3  o3  n3  m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3                                                      D2  C2  B2  A2   p2  o2  n2  m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2
  // Byte:   ------31------   ------30------   ------29------   ------28------   ------27------   ------26------   ------25------   ------24------   ------23------   ------22------   ------21------   ------20------   ------19------   ------18------   ------17------   ------16------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4, x5;
  int i, n;
  n = outputDataSize/32;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4, u5;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,x5,u1,u2,u3,u4,u5) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];
      x5 = (t19[i] << 24) | (t18[i] << 16) | (t17[i] << 8) | t16[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      u4.y = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6) | ((x4 & M3) >> 12) | ((x4 & M4) << 12) | ((x4 & M5) >> 18) | ((x4 & M6) << 18);
      u5.y = (x5 & M0) | ((x5 & M1) >> 6) | ((x5 & M2) << 6) | ((x5 & M3) >> 12) | ((x5 & M4) << 12) | ((x5 & M5) >> 18) | ((x5 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[8*i]   = (u4.b[0] << 24) | (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[8*i+1] =                                                      u5.b[0];
      outputwordptr[8*i+2] = (u4.b[1] << 24) | (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[8*i+3] =                                                      u5.b[1];
      outputwordptr[8*i+4] = (u4.b[2] << 24) | (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[8*i+5] =                                                      u5.b[2];
      outputwordptr[8*i+6] = (u4.b[3] << 24) | (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
      outputwordptr[8*i+7] =                                                      u5.b[3];
    }
  }
}

static void cornerturn_24thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 32 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  R3  p2  p1  p0   o3  o2  o1  o0   n3  n2  n1  n0   m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:  -48 -17 +14 +45  -51 -20 +11 +42  -54 -23 +8  +39  -57 -26 +5  +36  -60 -29 +2  +33  -63 -32 -1  +30  -66 -35 -4  +27  -69 -38 -7  +24  -72 -41 -10 +21  -75 -44 -13 +18  -78 -47 -16 +15  -81 -50 -19 +12  -84 -53 -22 +9   -87 -56 -25 +6   -90 -59 -28 +3   -93 -62 -31 +0 
  //                                                                                                                                                
  // Output:                                   H1  G1  F1  E1   D1  C1  B1  A1   p1  o1  n1  m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1                                     H0  G0  F0  E0   D0  C0  B0  A0   p0  o0  n0  m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // Thread: ------31------   ------30------   ------29------   ------28------   ------27------   ------26------   ------25------   ------24------   ------23------   ------22------   ------21------   ------20------   ------19------   ------18------   ------17------   ------16------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:                                                                                                                                          H3  H2  H1  H0   G3  G2  G1  G0   F3  F2  F1  F0   E3  E2  E1  E0   D3  D2  D1  D0   C3  C2  C1  C0   B3  B2  B1  B0   A3  A2  A1  A0
  //                                                                                                                                                
  // Shift:                                                                                                                                         -24 +7  +38 +69  -27 +4  +35 +66  -30 +1  +32 +63  -33 -2  +29 +60  -36 -5  +26 +57  -39 -8  +23 +54  -42 -11 +20 +51  -45 -14 +17 +48
  //                                                                                                                                                
  // Output:                                   H3  G3  F3  E3   D3  C3  B3  A3   p3  o3  n3  m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3                                     H2  G2  F2  E2   D2  C2  B2  A2   p2  o2  n2  m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2
  // Byte:   ------31------   ------30------   ------29------   ------28------   ------27------   ------26------   ------25------   ------24------   ------23------   ------22------   ------21------   ------20------   ------19------   ------18------   ------17------   ------16------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4, x5, x6;
  int i, n;
  n = outputDataSize/32;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4, u5, u6;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,x5,x6,u1,u2,u3,u4,u5,u6) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];
      x5 = (t19[i] << 24) | (t18[i] << 16) | (t17[i] << 8) | t16[i];
      x6 = (t23[i] << 24) | (t22[i] << 16) | (t21[i] << 8) | t20[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      u4.y = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6) | ((x4 & M3) >> 12) | ((x4 & M4) << 12) | ((x4 & M5) >> 18) | ((x4 & M6) << 18);
      u5.y = (x5 & M0) | ((x5 & M1) >> 6) | ((x5 & M2) << 6) | ((x5 & M3) >> 12) | ((x5 & M4) << 12) | ((x5 & M5) >> 18) | ((x5 & M6) << 18);
      u6.y = (x6 & M0) | ((x6 & M1) >> 6) | ((x6 & M2) << 6) | ((x6 & M3) >> 12) | ((x6 & M4) << 12) | ((x6 & M5) >> 18) | ((x6 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[8*i]   = (u4.b[0] << 24) | (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[8*i+1] =                                     (u6.b[0] << 8) | u5.b[0];
      outputwordptr[8*i+2] = (u4.b[1] << 24) | (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[8*i+3] =                                     (u6.b[1] << 8) | u5.b[1];
      outputwordptr[8*i+4] = (u4.b[2] << 24) | (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[8*i+5] =                                     (u6.b[2] << 8) | u5.b[2];
      outputwordptr[8*i+6] = (u4.b[3] << 24) | (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
      outputwordptr[8*i+7] =                                     (u6.b[3] << 8) | u5.b[3];
    }
  }
}

static void cornerturn_32thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 32 threads of 2-bit data.
  //
  // Thread: ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  p3  p2  p1  p0   o3  o2  o1  o0   n3  n2  n1  n0   m3  m2  m1  m0   l3  l2  l1  l0   k3  k2  k1  k0   j3  j2  j1  j0   i3  i2  i1  i0   h3  h2  h1  h0   g3  g2  g1  g0   f3  f2  f1  f0   e3  e2  e1  e0   d3  d2  d1  d0   c3  c2  c1  c0   b3  b2  b1  b0   a3  a2  a1  a0
  //                                                                                                                                                
  // Shift:  -48 -17 +14 +45  -51 -20 +11 +42  -54 -23 +8  +39  -57 -26 +5  +36  -60 -29 +2  +33  -63 -32 -1  +30  -66 -35 -4  +27  -69 -38 -7  +24  -72 -41 -10 +21  -75 -44 -13 +18  -78 -47 -16 +15  -81 -50 -19 +12  -84 -53 -22 +9   -87 -56 -25 +6   -90 -59 -28 +3   -93 -62 -31 +0 
  //                                                                                                                                                
  // Output: P1  O1  N1  M1   L1  K1  J1  I1   H1  G1  F1  E1   D1  C1  B1  A1   p1  o1  n1  m1   l1  k1  j1  i1   h1  g1  f1  e1   d1  c1  b1  a1   P0  O0  N0  M0   L0  K0  J0  I0   H0  G0  F0  E0   D0  C0  B0  A0   p0  o0  n0  m0   l0  k0  j0  i0   h0  g0  f0  e0   d0  c0  b0  a0
  // Byte:   ------15------   ------14------   ------13------   ------12------   ------11------   ------10------   ------9-------   ------8-------   ------7-------   ------6-------   ------5-------   ------4-------   ------3-------   ------2-------   ------1-------   ------0-------
  //
  // Thread: ------31------   ------30------   ------29------   ------28------   ------27------   ------26------   ------25------   ------24------   ------23------   ------22------   ------21------   ------20------   ------19------   ------18------   ------17------   ------16------
  // Byte:   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------   ------0-------
  // Input:  P3  P2  P1  P0   O3  O2  O1  O0   N3  N2  N1  N0   M3  M2  M1  M0   L3  L2  L1  L0   K3  K2  K1  K0   J3  J2  J1  J0   I3  I2  I1  I0   H3  H2  H1  H0   G3  G2  G1  G0   F3  F2  F1  F0   E3  E2  E1  E0   D3  D2  D1  D0   C3  C2  C1  C0   B3  B2  B1  B0   A3  A2  A1  A0
  //                                                                                                                                                
  // Shift:  0  +31 +62 +93  -3  +28 +59 +90  -6  +25 +56 +87  -9  +22 +53 +84  -12 +19 +50 +81  -15 +16 +47 +78  -18 +13 +44 +75  -21 +10 +41 +72  -24 +7  +38 +69  -27 +4  +35 +66  -30 +1  +32 +63  -33 -2  +29 +60  -36 -5  +26 +57  -39 -8  +23 +54  -42 -11 +20 +51  -45 -14 +17 +48
  //                                                                                                                                                
  // Output: P3  O3  N3  M3   L3  K3  J3  I3   H3  G3  F3  E3   D3  C3  B3  A3   p3  o3  n3  m3   l3  k3  j3  i3   h3  g3  f3  e3   d3  c3  b3  a3   P2  O2  N2  M2   L2  K2  J2  I2   H2  G2  F2  E2   D2  C2  B2  A2   p2  o2  n2  m2   l2  k2  j2  i2   h2  g2  f2  e2   d2  c2  b2  a2
  // Byte:   ------31------   ------30------   ------29------   ------28------   ------27------   ------26------   ------25------   ------24------   ------23------   ------22------   ------21------   ------20------   ------19------   ------18------   ------17------   ------16------
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform four separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4, x5, x6, x7, x8;
  int i, n;
  n = outputDataSize/32;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4, u5, u6, u7, u8;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,x5,x6,x7,x8,u1,u2,u3,u4,u5,u6,u7,u8) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1 = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2 = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3 = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4 = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];
      x5 = (t19[i] << 24) | (t18[i] << 16) | (t17[i] << 8) | t16[i];
      x6 = (t23[i] << 24) | (t22[i] << 16) | (t21[i] << 8) | t20[i];
      x7 = (t27[i] << 24) | (t26[i] << 16) | (t25[i] << 8) | t24[i];
      x8 = (t31[i] << 24) | (t30[i] << 16) | (t29[i] << 8) | t28[i];

      // mask and shift 32-bit chunks
      u1.y = (x1 & M0) | ((x1 & M1) >> 6) | ((x1 & M2) << 6) | ((x1 & M3) >> 12) | ((x1 & M4) << 12) | ((x1 & M5) >> 18) | ((x1 & M6) << 18);
      u2.y = (x2 & M0) | ((x2 & M1) >> 6) | ((x2 & M2) << 6) | ((x2 & M3) >> 12) | ((x2 & M4) << 12) | ((x2 & M5) >> 18) | ((x2 & M6) << 18);
      u3.y = (x3 & M0) | ((x3 & M1) >> 6) | ((x3 & M2) << 6) | ((x3 & M3) >> 12) | ((x3 & M4) << 12) | ((x3 & M5) >> 18) | ((x3 & M6) << 18);
      u4.y = (x4 & M0) | ((x4 & M1) >> 6) | ((x4 & M2) << 6) | ((x4 & M3) >> 12) | ((x4 & M4) << 12) | ((x4 & M5) >> 18) | ((x4 & M6) << 18);
      u5.y = (x5 & M0) | ((x5 & M1) >> 6) | ((x5 & M2) << 6) | ((x5 & M3) >> 12) | ((x5 & M4) << 12) | ((x5 & M5) >> 18) | ((x5 & M6) << 18);
      u6.y = (x6 & M0) | ((x6 & M1) >> 6) | ((x6 & M2) << 6) | ((x6 & M3) >> 12) | ((x6 & M4) << 12) | ((x6 & M5) >> 18) | ((x6 & M6) << 18);
      u7.y = (x7 & M0) | ((x7 & M1) >> 6) | ((x7 & M2) << 6) | ((x7 & M3) >> 12) | ((x7 & M4) << 12) | ((x7 & M5) >> 18) | ((x7 & M6) << 18);
      u8.y = (x8 & M0) | ((x8 & M1) >> 6) | ((x8 & M2) << 6) | ((x8 & M3) >> 12) | ((x8 & M4) << 12) | ((x8 & M5) >> 18) | ((x8 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[8*i]   = (u4.b[0] << 24) | (u3.b[0] << 16) | (u2.b[0] << 8) | u1.b[0];
      outputwordptr[8*i+1] = (u8.b[0] << 24) | (u7.b[0] << 16) | (u6.b[0] << 8) | u5.b[0];
      outputwordptr[8*i+2] = (u4.b[1] << 24) | (u3.b[1] << 16) | (u2.b[1] << 8) | u1.b[1];
      outputwordptr[8*i+3] = (u8.b[1] << 24) | (u7.b[1] << 16) | (u6.b[1] << 8) | u5.b[1];
      outputwordptr[8*i+4] = (u4.b[2] << 24) | (u3.b[2] << 16) | (u2.b[2] << 8) | u1.b[2];
      outputwordptr[8*i+5] = (u8.b[2] << 24) | (u7.b[2] << 16) | (u6.b[2] << 8) | u5.b[2];
      outputwordptr[8*i+6] = (u4.b[3] << 24) | (u3.b[3] << 16) | (u2.b[3] << 8) | u1.b[3];
      outputwordptr[8*i+7] = (u8.b[3] << 24) | (u7.b[3] << 16) | (u6.b[3] << 8) | u5.b[3];
    }
  }
}

static void cornerturn_64thread_2bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 32 threads of 2-bit data.
  //
  // Logical extension of 16 and 32 thread cases
  //
  // This one is a bit complicated.  A reasonable way to proceed seems to be to perform eight separate 4-thread corner turns and then 
  // do a final shuffle of byte sized chunks.  There may be a better way...

  const uint32_t M0 = 0xC0300C03;
  const uint32_t M1 = 0x300C0300;
  const uint32_t M2 = 0x00C0300C;
  const uint32_t M3 = 0x0C030000;
  const uint32_t M4 = 0x0000C030;
  const uint32_t M5 = 0x03000000;
  const uint32_t M6 = 0x000000C0;

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
  const uint8_t *t32 = (const uint8_t *)(threadBuffers[32]);
  const uint8_t *t33 = (const uint8_t *)(threadBuffers[33]);
  const uint8_t *t34 = (const uint8_t *)(threadBuffers[34]);
  const uint8_t *t35 = (const uint8_t *)(threadBuffers[35]);
  const uint8_t *t36 = (const uint8_t *)(threadBuffers[36]);
  const uint8_t *t37 = (const uint8_t *)(threadBuffers[37]);
  const uint8_t *t38 = (const uint8_t *)(threadBuffers[38]);
  const uint8_t *t39 = (const uint8_t *)(threadBuffers[39]);
  const uint8_t *t40 = (const uint8_t *)(threadBuffers[40]);
  const uint8_t *t41 = (const uint8_t *)(threadBuffers[41]);
  const uint8_t *t42 = (const uint8_t *)(threadBuffers[42]);
  const uint8_t *t43 = (const uint8_t *)(threadBuffers[43]);
  const uint8_t *t44 = (const uint8_t *)(threadBuffers[44]);
  const uint8_t *t45 = (const uint8_t *)(threadBuffers[45]);
  const uint8_t *t46 = (const uint8_t *)(threadBuffers[46]);
  const uint8_t *t47 = (const uint8_t *)(threadBuffers[47]);
  const uint8_t *t48 = (const uint8_t *)(threadBuffers[48]);
  const uint8_t *t49 = (const uint8_t *)(threadBuffers[49]);
  const uint8_t *t50 = (const uint8_t *)(threadBuffers[50]);
  const uint8_t *t51 = (const uint8_t *)(threadBuffers[51]);
  const uint8_t *t52 = (const uint8_t *)(threadBuffers[52]);
  const uint8_t *t53 = (const uint8_t *)(threadBuffers[53]);
  const uint8_t *t54 = (const uint8_t *)(threadBuffers[54]);
  const uint8_t *t55 = (const uint8_t *)(threadBuffers[55]);
  const uint8_t *t56 = (const uint8_t *)(threadBuffers[56]);
  const uint8_t *t57 = (const uint8_t *)(threadBuffers[57]);
  const uint8_t *t58 = (const uint8_t *)(threadBuffers[58]);
  const uint8_t *t59 = (const uint8_t *)(threadBuffers[59]);
  const uint8_t *t60 = (const uint8_t *)(threadBuffers[60]);
  const uint8_t *t61 = (const uint8_t *)(threadBuffers[61]);
  const uint8_t *t62 = (const uint8_t *)(threadBuffers[62]);
  const uint8_t *t63 = (const uint8_t *)(threadBuffers[63]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;
  uint32_t x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16;
  int i, n;
  n = outputDataSize/64;
  union { uint32_t y; uint8_t b[4]; } u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16;

PRAGMA_OMP(parallel private(i,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      // assemble 32-bit chunks
      x1  = (t3[i]  << 24) | (t2[i]  << 16) | (t1[i]  << 8) | t0[i];
      x2  = (t7[i]  << 24) | (t6[i]  << 16) | (t5[i]  << 8) | t4[i];
      x3  = (t11[i] << 24) | (t10[i] << 16) | (t9[i]  << 8) | t8[i];
      x4  = (t15[i] << 24) | (t14[i] << 16) | (t13[i] << 8) | t12[i];
      x5  = (t19[i] << 24) | (t18[i] << 16) | (t17[i] << 8) | t16[i];
      x6  = (t23[i] << 24) | (t22[i] << 16) | (t21[i] << 8) | t20[i];
      x7  = (t27[i] << 24) | (t26[i] << 16) | (t25[i] << 8) | t24[i];
      x8  = (t31[i] << 24) | (t30[i] << 16) | (t29[i] << 8) | t28[i];
      x9  = (t35[i] << 24) | (t34[i] << 16) | (t33[i] << 8) | t32[i];
      x10 = (t39[i] << 24) | (t38[i] << 16) | (t37[i] << 8) | t36[i];
      x11 = (t43[i] << 24) | (t42[i] << 16) | (t41[i] << 8) | t40[i];
      x12 = (t47[i] << 24) | (t46[i] << 16) | (t45[i] << 8) | t44[i];
      x13 = (t51[i] << 24) | (t50[i] << 16) | (t49[i] << 8) | t48[i];
      x14 = (t55[i] << 24) | (t54[i] << 16) | (t53[i] << 8) | t52[i];
      x15 = (t59[i] << 24) | (t58[i] << 16) | (t57[i] << 8) | t56[i];
      x16 = (t63[i] << 24) | (t62[i] << 16) | (t61[i] << 8) | t60[i];

      // mask and shift 32-bit chunks
      u1.y  = (x1  & M0) | ((x1  & M1) >> 6) | ((x1  & M2) << 6) | ((x1  & M3) >> 12) | ((x1  & M4) << 12) | ((x1  & M5) >> 18) | ((x1  & M6) << 18);
      u2.y  = (x2  & M0) | ((x2  & M1) >> 6) | ((x2  & M2) << 6) | ((x2  & M3) >> 12) | ((x2  & M4) << 12) | ((x2  & M5) >> 18) | ((x2  & M6) << 18);
      u3.y  = (x3  & M0) | ((x3  & M1) >> 6) | ((x3  & M2) << 6) | ((x3  & M3) >> 12) | ((x3  & M4) << 12) | ((x3  & M5) >> 18) | ((x3  & M6) << 18);
      u4.y  = (x4  & M0) | ((x4  & M1) >> 6) | ((x4  & M2) << 6) | ((x4  & M3) >> 12) | ((x4  & M4) << 12) | ((x4  & M5) >> 18) | ((x4  & M6) << 18);
      u5.y  = (x5  & M0) | ((x5  & M1) >> 6) | ((x5  & M2) << 6) | ((x5  & M3) >> 12) | ((x5  & M4) << 12) | ((x5  & M5) >> 18) | ((x5  & M6) << 18);
      u6.y  = (x6  & M0) | ((x6  & M1) >> 6) | ((x6  & M2) << 6) | ((x6  & M3) >> 12) | ((x6  & M4) << 12) | ((x6  & M5) >> 18) | ((x6  & M6) << 18);
      u7.y  = (x7  & M0) | ((x7  & M1) >> 6) | ((x7  & M2) << 6) | ((x7  & M3) >> 12) | ((x7  & M4) << 12) | ((x7  & M5) >> 18) | ((x7  & M6) << 18);
      u8.y  = (x8  & M0) | ((x8  & M1) >> 6) | ((x8  & M2) << 6) | ((x8  & M3) >> 12) | ((x8  & M4) << 12) | ((x8  & M5) >> 18) | ((x8  & M6) << 18);
      u9.y  = (x9  & M0) | ((x9  & M1) >> 6) | ((x9  & M2) << 6) | ((x9  & M3) >> 12) | ((x9  & M4) << 12) | ((x9  & M5) >> 18) | ((x9  & M6) << 18);
      u10.y = (x10 & M0) | ((x10 & M1) >> 6) | ((x10 & M2) << 6) | ((x10 & M3) >> 12) | ((x10 & M4) << 12) | ((x10 & M5) >> 18) | ((x10 & M6) << 18);
      u11.y = (x11 & M0) | ((x11 & M1) >> 6) | ((x11 & M2) << 6) | ((x11 & M3) >> 12) | ((x11 & M4) << 12) | ((x11 & M5) >> 18) | ((x11 & M6) << 18);
      u12.y = (x12 & M0) | ((x12 & M1) >> 6) | ((x12 & M2) << 6) | ((x12 & M3) >> 12) | ((x12 & M4) << 12) | ((x12 & M5) >> 18) | ((x12 & M6) << 18);
      u13.y = (x13 & M0) | ((x13 & M1) >> 6) | ((x13 & M2) << 6) | ((x13 & M3) >> 12) | ((x13 & M4) << 12) | ((x13 & M5) >> 18) | ((x13 & M6) << 18);
      u14.y = (x14 & M0) | ((x14 & M1) >> 6) | ((x14 & M2) << 6) | ((x14 & M3) >> 12) | ((x14 & M4) << 12) | ((x14 & M5) >> 18) | ((x14 & M6) << 18);
      u15.y = (x15 & M0) | ((x15 & M1) >> 6) | ((x15 & M2) << 6) | ((x15 & M3) >> 12) | ((x15 & M4) << 12) | ((x15 & M5) >> 18) | ((x15 & M6) << 18);
      u16.y = (x16 & M0) | ((x16 & M1) >> 6) | ((x16 & M2) << 6) | ((x16 & M3) >> 12) | ((x16 & M4) << 12) | ((x16 & M5) >> 18) | ((x16 & M6) << 18);

      // shuffle 8-bit chunks
      outputwordptr[16*i]    = (u4.b[0]  << 24) | (u3.b[0]  << 16) | (u2.b[0]  << 8) | u1.b[0];
      outputwordptr[16*i+1]  = (u8.b[0]  << 24) | (u7.b[0]  << 16) | (u6.b[0]  << 8) | u5.b[0];
      outputwordptr[16*i+2]  = (u12.b[0] << 24) | (u11.b[0] << 16) | (u10.b[0] << 8) | u9.b[0];
      outputwordptr[16*i+3]  = (u16.b[0] << 24) | (u15.b[0] << 16) | (u14.b[0] << 8) | u13.b[0];
      outputwordptr[16*i+4]  = (u4.b[1]  << 24) | (u3.b[1]  << 16) | (u2.b[1]  << 8) | u1.b[1];
      outputwordptr[16*i+5]  = (u8.b[1]  << 24) | (u7.b[1]  << 16) | (u6.b[1]  << 8) | u5.b[1];
      outputwordptr[16*i+6]  = (u12.b[1] << 24) | (u11.b[1] << 16) | (u10.b[1] << 8) | u9.b[1];
      outputwordptr[16*i+7]  = (u16.b[1] << 24) | (u15.b[1] << 16) | (u14.b[1] << 8) | u13.b[1];
      outputwordptr[16*i+8]  = (u4.b[2]  << 24) | (u3.b[2]  << 16) | (u2.b[2]  << 8) | u1.b[2];
      outputwordptr[16*i+9]  = (u8.b[2]  << 24) | (u7.b[2]  << 16) | (u6.b[2]  << 8) | u5.b[2];
      outputwordptr[16*i+10] = (u12.b[2] << 24) | (u11.b[2] << 16) | (u10.b[2] << 8) | u9.b[2];
      outputwordptr[16*i+11] = (u16.b[2] << 24) | (u15.b[2] << 16) | (u14.b[2] << 8) | u13.b[2];
      outputwordptr[16*i+12] = (u4.b[3]  << 24) | (u3.b[3]  << 16) | (u2.b[3]  << 8) | u1.b[3];
      outputwordptr[16*i+13] = (u8.b[3]  << 24) | (u7.b[3]  << 16) | (u6.b[3]  << 8) | u5.b[3];
      outputwordptr[16*i+14] = (u12.b[3] << 24) | (u11.b[3] << 16) | (u10.b[3] << 8) | u9.b[3];
      outputwordptr[16*i+15] = (u16.b[3] << 24) | (u15.b[3] << 16) | (u14.b[3] << 8) | u13.b[3];
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

  const uint32_t M0 = 0xF00FF00F;
  const uint32_t M1 = 0x0F000F00;
  const uint32_t M2 = 0x00F000F0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;

  uint32_t x;
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

  const uint32_t M0 = 0x0000000F;
  const uint32_t M2 = 0x00F00000;
  const uint32_t M3 = 0x000F0000;
  const uint32_t M4 = 0x0000F000;
  const uint32_t M5 = 0x00000F00;
  const uint32_t M6 = 0x000000F0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;

  uint32_t x;
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

  const uint32_t M0 = 0xF000000F;
  const uint32_t M1 = 0x0F000000;
  const uint32_t M2 = 0x00F00000;
  const uint32_t M3 = 0x000F0000;
  const uint32_t M4 = 0x0000F000;
  const uint32_t M5 = 0x00000F00;
  const uint32_t M6 = 0x000000F0;

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  uint32_t *outputwordptr = (uint32_t *)outputBuffer;

  uint32_t x;
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

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
 
  const uint32_t M0 = 0x00F0F0F0;
  const uint32_t M1 = 0x000F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t xe, xo;
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

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
 
  const uint32_t M0 = 0x00F0F0F0;
  const uint32_t M1 = 0x000F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t xe, xo;
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

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t xe, xo;
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

  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t xe, xo;
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

static void cornerturn_9thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 9 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                                                                 i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:                                                                    0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                                                                 i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ---------------------------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:                                                                 +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:                                i1   h1  g1   f1  e1   d1  c1   b1  a1                                  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x2 =                                                t8[i];
      x1 = (t7[i] << 24) | (t5[i] << 16) | (t3[i] << 8) | t1[i];
      x0 = (t6[i] << 24) | (t4[i] << 16) | (t2[i] << 8) | t0[i];

      *outputwordptr = ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_10thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 10 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                                                        j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:                                                           +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                             j1  j0                              i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:                              0  -7                              +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:                            j1  i1   h1  g1   f1  e1   d1  c1   b1  a1                              j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 =                                                t9[i];
      x2 =                                                t8[i];
      x1 = (t7[i] << 24) | (t5[i] << 16) | (t3[i] << 8) | t1[i];
      x0 = (t6[i] << 24) | (t4[i] << 16) | (t2[i] << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_11thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 11 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                                               k1  k0   j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:                                                   -1      +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                             j1  j0                     k1  k0   i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:                              0  -7                     +7   0   +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:                       k1   j1  i1   h1  g1   f1  e1   d1  c1   b1  a1                         k0   j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 =                                                 t9[i];
      x2 =                                 (t10[i] << 8) | t8[i];
      x1 = (t7[i] << 24) | (t5[i] << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i] << 24) | (t4[i] << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_12thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 12 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                                      l1  l0   k1  k0   j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:                                          +2       -1      +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                    l1  l0   j1  j0                     k1  k0   i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:                     0  -7    0  -7                     +7   0   +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:                   l1  k1   j1  i1   h1  g1   f1  e1   d1  c1   b1  a1                     l0  k0   j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 =                                 (t11[i] << 8) | t9[i];
      x2 =                                 (t10[i] << 8) | t8[i];
      x1 = (t7[i] << 24) | (t5[i] << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i] << 24) | (t4[i] << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_13thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 13 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                             m1  m0   l1  l0   k1  k0   j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:                               -2         +2       -1      +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                    l1  l0   j1  j0            m1  m0   k1  k0   i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:                     0  -7    0  -7            +7   0   +7   0   +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:              m1   l1  k1   j1  i1   h1  g1   f1  e1   d1  c1   b1  a1                m0   l0  k0   j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 =                                  (t11[i] << 8) | t9[i];
      x2 =                 (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i] << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i] << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_14thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 14 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:                    n1  n0   m1  m0   l1  l0   k1  k0   j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:                       +1      -2         +2       -1      +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:           n1  n0   l1  l0   j1  j0            m1  m0   k1  k0   i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:            0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0             0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:          n1  m1   l1  k1   j1  i1   h1  g1   f1  e1   d1  c1   b1  a1            n0  m0   l0  k0   j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 =                 (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2 =                 (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i] << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i] << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_15thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 15 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:           o1  o0   n1  n0   m1  m0   l1  l0   k1  k0   j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:              -3       +1      -2         +2       -1      +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:           n1  n0   l1  l0   j1  j0   o1  o0   m1  m0   k1  k0   i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:            0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output:     o1   n1  m1   l1  k1   j1  i1   h1  g1   f1  e1   d1  c1   b1  a1       o0   n0  m0   l0  k0   j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 =                  (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2 = (t14[i] << 24) | (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i]  << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i]  << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_16thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 16 threads of 4-bit data.
  //
  // Thread: --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:  p1  p0   o1  o0   n1  n0   m1  m0   l1  l0   k1  k0   j1  j0   i1  i0   h1  h0   g1  g0   f1  f0   e1  e0   d1  d0   c1  c0   b1  b0   a1  a0
  //                                                                                                                                                      
  // Order:    0         -3       +1      -2         +2       -1      +3       0       0         -3       +1      -2         +2       -1      +3       0
  //                                                                                                                                                      
  // Thread: --15--   --13--   --11--   --9---   --14--   --12--   --10--   --8---   --7---   --5---   --3---   --1---   --6---   --4---   --2---   --0---
  // Byte:   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---   --0---
  // Input:  p1  p0   n1  n0   l1  l0   j1  j0   o1  o0   m1  m0   k1  k0   i1  i0   h1  h0   f1  f0   d1  d0   b1  b0   g1  g0   e1  e0   c1  c0   a1  a0
  //                                                                                                                                                      
  //         ----------------x3---------------   ----------------x2---------------   ----------------x1---------------   ----------------x0---------------
  //                                                                                                                                                      
  // Shift:   0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0    0  -7    0  -7    0  -7    0  -7   +7   0   +7   0   +7   0   +7   0
  //                                                                                                                  
  // Output: p1  o1   n1  m1   l1  k1   j1  i1   h1  g1   f1  e1   d1  c1   b1  a1   p0  o0   n0  m0   l0  k0   j0  i0   h0  g0   f0  e0   d0  c0   b0  a0
  // Byte:   --15--   --14--   --13--   --12--   --11--   --10--   --9---   --8---   --7---   --6---   --5---   --4---   --3---   --2---   --1---   --0---

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3;
  int i, n;

  n = outputDataSize/16;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x3 = (t15[i] << 24) | (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2 = (t14[i] << 24) | (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i]  << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i]  << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_20thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 20 threads of 4-bit data.
  //
  // Follow from the logic of 32 thread version

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3, x4, x5;
  int i, n;

  n = outputDataSize/32;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3,x4,x5) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x5 =                                   (t19[i] << 8) | t17[i];
      x4 =                                   (t18[i] << 8) | t16[i];
      x3 = (t15[i] << 24) | (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2 = (t14[i] << 24) | (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i]  << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i]  << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr =                                                              ((x5 & M1) * 0x10LL) | (x4 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
      *outputwordptr =                                                              (x5 & M0) | ((x4 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_24thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 24 threads of 4-bit data.
  //
  // Follow from the logic of 32 thread version

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3, x4, x5;
  int i, n;

  n = outputDataSize/32;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3,x4,x5) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x5 = (t23[i] << 24) | (t21[i] << 16) | (t19[i] << 8) | t17[i];
      x4 = (t22[i] << 24) | (t20[i] << 16) | (t18[i] << 8) | t16[i];
      x3 = (t15[i] << 24) | (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2 = (t14[i] << 24) | (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i]  << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i]  << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr =                                                              ((x5 & M1) * 0x10LL) | (x4 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
      *outputwordptr =                                                              (x5 & M0) | ((x4 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_32thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 32 threads of 4-bit data.
  //
  // Follow from the logic of 16 thread version

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3, x4, x5, x6, x7;
  int i, n;

  n = outputDataSize/32;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3,x4,x5,x6,x7) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x7 = (t31[i] << 24) | (t29[i] << 16) | (t27[i] << 8) | t25[i];
      x6 = (t30[i] << 24) | (t28[i] << 16) | (t26[i] << 8) | t24[i];
      x5 = (t23[i] << 24) | (t21[i] << 16) | (t19[i] << 8) | t17[i];
      x4 = (t22[i] << 24) | (t20[i] << 16) | (t18[i] << 8) | t16[i];
      x3 = (t15[i] << 24) | (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2 = (t14[i] << 24) | (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1 = (t7[i]  << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0 = (t6[i]  << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3 & M1) * 0x1000000000LL) | ((x2 & M1) * 0x100000000LL) | ((x1 & M1) * 0x10LL) | (x0 & M1);
      ++outputwordptr;
      *outputwordptr = ((x7 & M1) * 0x1000000000LL) | ((x6 & M1) * 0x100000000LL) | ((x5 & M1) * 0x10LL) | (x4 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3 & M0) * 0x100000000LL)  | ((x2 & M0) * 0x10000000LL)  | (x1 & M0) | ((x0 & M0) >> 4);
      ++outputwordptr;
      *outputwordptr = ((x7 & M0) * 0x100000000LL)  | ((x6 & M0) * 0x10000000LL)  | (x5 & M0) | ((x4 & M0) >> 4);
      ++outputwordptr;
    }
  }
}

static void cornerturn_64thread_4bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // Efficiently handle the special case of 64 threads of 4-bit data.
  //
  // Follow from the logic of 16 thread version

  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
  const uint8_t *t32 = (const uint8_t *)(threadBuffers[32]);
  const uint8_t *t33 = (const uint8_t *)(threadBuffers[33]);
  const uint8_t *t34 = (const uint8_t *)(threadBuffers[34]);
  const uint8_t *t35 = (const uint8_t *)(threadBuffers[35]);
  const uint8_t *t36 = (const uint8_t *)(threadBuffers[36]);
  const uint8_t *t37 = (const uint8_t *)(threadBuffers[37]);
  const uint8_t *t38 = (const uint8_t *)(threadBuffers[38]);
  const uint8_t *t39 = (const uint8_t *)(threadBuffers[39]);
  const uint8_t *t40 = (const uint8_t *)(threadBuffers[40]);
  const uint8_t *t41 = (const uint8_t *)(threadBuffers[41]);
  const uint8_t *t42 = (const uint8_t *)(threadBuffers[42]);
  const uint8_t *t43 = (const uint8_t *)(threadBuffers[43]);
  const uint8_t *t44 = (const uint8_t *)(threadBuffers[44]);
  const uint8_t *t45 = (const uint8_t *)(threadBuffers[45]);
  const uint8_t *t46 = (const uint8_t *)(threadBuffers[46]);
  const uint8_t *t47 = (const uint8_t *)(threadBuffers[47]);
  const uint8_t *t48 = (const uint8_t *)(threadBuffers[48]);
  const uint8_t *t49 = (const uint8_t *)(threadBuffers[49]);
  const uint8_t *t50 = (const uint8_t *)(threadBuffers[50]);
  const uint8_t *t51 = (const uint8_t *)(threadBuffers[51]);
  const uint8_t *t52 = (const uint8_t *)(threadBuffers[52]);
  const uint8_t *t53 = (const uint8_t *)(threadBuffers[53]);
  const uint8_t *t54 = (const uint8_t *)(threadBuffers[54]);
  const uint8_t *t55 = (const uint8_t *)(threadBuffers[55]);
  const uint8_t *t56 = (const uint8_t *)(threadBuffers[56]);
  const uint8_t *t57 = (const uint8_t *)(threadBuffers[57]);
  const uint8_t *t58 = (const uint8_t *)(threadBuffers[58]);
  const uint8_t *t59 = (const uint8_t *)(threadBuffers[59]);
  const uint8_t *t60 = (const uint8_t *)(threadBuffers[60]);
  const uint8_t *t61 = (const uint8_t *)(threadBuffers[61]);
  const uint8_t *t62 = (const uint8_t *)(threadBuffers[62]);
  const uint8_t *t63 = (const uint8_t *)(threadBuffers[63]);
 
  const uint32_t M0 = 0xF0F0F0F0;
  const uint32_t M1 = 0x0F0F0F0F;

  uint64_t *outputwordptr = (uint64_t *)outputBuffer;

  uint32_t x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15;
  int i, n;

  n = outputDataSize/64;

PRAGMA_OMP(parallel private(i,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) shared(outputwordptr,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,n))
  {
PRAGMA_OMP(for schedule(dynamic,125) nowait)
    for(i = 0; i < n; ++i)
    {
      x15 = (t63[i] << 24) | (t61[i] << 16) | (t59[i] << 8) | t57[i];
      x14 = (t62[i] << 24) | (t60[i] << 16) | (t58[i] << 8) | t56[i];
      x13 = (t55[i] << 24) | (t53[i] << 16) | (t51[i] << 8) | t49[i];
      x12 = (t54[i] << 24) | (t52[i] << 16) | (t50[i] << 8) | t48[i];
      x11 = (t47[i] << 24) | (t45[i] << 16) | (t43[i] << 8) | t41[i];
      x10 = (t46[i] << 24) | (t44[i] << 16) | (t42[i] << 8) | t40[i];
      x9  = (t39[i] << 24) | (t37[i] << 16) | (t35[i] << 8) | t33[i];
      x8  = (t38[i] << 24) | (t36[i] << 16) | (t34[i] << 8) | t32[i];
      x7  = (t31[i] << 24) | (t29[i] << 16) | (t27[i] << 8) | t25[i];
      x6  = (t30[i] << 24) | (t28[i] << 16) | (t26[i] << 8) | t24[i];
      x5  = (t23[i] << 24) | (t21[i] << 16) | (t19[i] << 8) | t17[i];
      x4  = (t22[i] << 24) | (t20[i] << 16) | (t18[i] << 8) | t16[i];
      x3  = (t15[i] << 24) | (t13[i] << 16) | (t11[i] << 8) | t9[i];
      x2  = (t14[i] << 24) | (t12[i] << 16) | (t10[i] << 8) | t8[i];
      x1  = (t7[i]  << 24) | (t5[i]  << 16) | (t3[i]  << 8) | t1[i];
      x0  = (t6[i]  << 24) | (t4[i]  << 16) | (t2[i]  << 8) | t0[i];

      *outputwordptr = ((x3  & M1) * 0x1000000000LL) | ((x2  & M1) * 0x100000000LL) | ((x1  & M1) * 0x10LL) | (x0  & M1);
      ++outputwordptr;
      *outputwordptr = ((x7  & M1) * 0x1000000000LL) | ((x6  & M1) * 0x100000000LL) | ((x5  & M1) * 0x10LL) | (x4  & M1);
      ++outputwordptr;
      *outputwordptr = ((x11 & M1) * 0x1000000000LL) | ((x10 & M1) * 0x100000000LL) | ((x9  & M1) * 0x10LL) | (x8  & M1);
      ++outputwordptr;
      *outputwordptr = ((x15 & M1) * 0x1000000000LL) | ((x14 & M1) * 0x100000000LL) | ((x13 & M1) * 0x10LL) | (x12 & M1);
      ++outputwordptr;
      *outputwordptr = ((x3  & M0) * 0x100000000LL)  | ((x2  & M0) * 0x10000000LL)  | (x1  & M0) | ((x0  & M0) >> 4);
      ++outputwordptr;
      *outputwordptr = ((x7  & M0) * 0x100000000LL)  | ((x6  & M0) * 0x10000000LL)  | (x5  & M0) | ((x4  & M0) >> 4);
      ++outputwordptr;
      *outputwordptr = ((x11 & M0) * 0x100000000LL)  | ((x10 & M0) * 0x10000000LL)  | (x9  & M0) | ((x8  & M0) >> 4);
      ++outputwordptr;
      *outputwordptr = ((x15 & M0) * 0x100000000LL)  | ((x14 & M0) * 0x10000000LL)  | (x13 & M0) | ((x12 & M0) >> 4);
      ++outputwordptr;
    }
  }
}


static void cornerturn_2thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);

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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);

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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);

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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);

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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);

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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);

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
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);

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

static void cornerturn_9thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8 = (const uint8_t *)(threadBuffers[8]);

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
  }
}

static void cornerturn_10thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0 = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1 = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2 = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3 = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4 = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5 = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6 = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7 = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8 = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9 = (const uint8_t *)(threadBuffers[9]);

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

static void cornerturn_11thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);

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
  }
}

static void cornerturn_12thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);

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

static void cornerturn_13thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);

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
  }
}

static void cornerturn_14thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);

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

static void cornerturn_15thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);

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
  }
}

static void cornerturn_16thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);

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

static void cornerturn_20thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[32*i]    = t0[i];
    outputBuffer[32*i+1]  = t1[i];
    outputBuffer[32*i+2]  = t2[i];
    outputBuffer[32*i+3]  = t3[i];
    outputBuffer[32*i+4]  = t4[i];
    outputBuffer[32*i+5]  = t5[i];
    outputBuffer[32*i+6]  = t6[i];
    outputBuffer[32*i+7]  = t7[i];
    outputBuffer[32*i+8]  = t8[i];
    outputBuffer[32*i+9]  = t9[i];
    outputBuffer[32*i+10] = t10[i];
    outputBuffer[32*i+11] = t11[i];
    outputBuffer[32*i+12] = t12[i];
    outputBuffer[32*i+13] = t13[i];
    outputBuffer[32*i+14] = t14[i];
    outputBuffer[32*i+15] = t15[i];
    outputBuffer[32*i+16] = t16[i];
    outputBuffer[32*i+17] = t17[i];
    outputBuffer[32*i+18] = t18[i];
    outputBuffer[32*i+19] = t19[i];
  }
}

static void cornerturn_24thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[32*i]    = t0[i];
    outputBuffer[32*i+1]  = t1[i];
    outputBuffer[32*i+2]  = t2[i];
    outputBuffer[32*i+3]  = t3[i];
    outputBuffer[32*i+4]  = t4[i];
    outputBuffer[32*i+5]  = t5[i];
    outputBuffer[32*i+6]  = t6[i];
    outputBuffer[32*i+7]  = t7[i];
    outputBuffer[32*i+8]  = t8[i];
    outputBuffer[32*i+9]  = t9[i];
    outputBuffer[32*i+10] = t10[i];
    outputBuffer[32*i+11] = t11[i];
    outputBuffer[32*i+12] = t12[i];
    outputBuffer[32*i+13] = t13[i];
    outputBuffer[32*i+14] = t14[i];
    outputBuffer[32*i+15] = t15[i];
    outputBuffer[32*i+16] = t16[i];
    outputBuffer[32*i+17] = t17[i];
    outputBuffer[32*i+18] = t18[i];
    outputBuffer[32*i+19] = t19[i];
    outputBuffer[32*i+20] = t20[i];
    outputBuffer[32*i+21] = t21[i];
    outputBuffer[32*i+22] = t22[i];
    outputBuffer[32*i+23] = t23[i];
  }
}

static void cornerturn_32thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[32*i]    = t0[i];
    outputBuffer[32*i+1]  = t1[i];
    outputBuffer[32*i+2]  = t2[i];
    outputBuffer[32*i+3]  = t3[i];
    outputBuffer[32*i+4]  = t4[i];
    outputBuffer[32*i+5]  = t5[i];
    outputBuffer[32*i+6]  = t6[i];
    outputBuffer[32*i+7]  = t7[i];
    outputBuffer[32*i+8]  = t8[i];
    outputBuffer[32*i+9]  = t9[i];
    outputBuffer[32*i+10] = t10[i];
    outputBuffer[32*i+11] = t11[i];
    outputBuffer[32*i+12] = t12[i];
    outputBuffer[32*i+13] = t13[i];
    outputBuffer[32*i+14] = t14[i];
    outputBuffer[32*i+15] = t15[i];
    outputBuffer[32*i+16] = t16[i];
    outputBuffer[32*i+17] = t17[i];
    outputBuffer[32*i+18] = t18[i];
    outputBuffer[32*i+19] = t19[i];
    outputBuffer[32*i+20] = t20[i];
    outputBuffer[32*i+21] = t21[i];
    outputBuffer[32*i+22] = t22[i];
    outputBuffer[32*i+23] = t23[i];
    outputBuffer[32*i+24] = t24[i];
    outputBuffer[32*i+25] = t25[i];
    outputBuffer[32*i+26] = t26[i];
    outputBuffer[32*i+27] = t27[i];
    outputBuffer[32*i+28] = t28[i];
    outputBuffer[32*i+29] = t29[i];
    outputBuffer[32*i+30] = t30[i];
    outputBuffer[32*i+31] = t31[i];
  }
}

static void cornerturn_64thread_8bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint8_t *t0  = (const uint8_t *)(threadBuffers[0]);
  const uint8_t *t1  = (const uint8_t *)(threadBuffers[1]);
  const uint8_t *t2  = (const uint8_t *)(threadBuffers[2]);
  const uint8_t *t3  = (const uint8_t *)(threadBuffers[3]);
  const uint8_t *t4  = (const uint8_t *)(threadBuffers[4]);
  const uint8_t *t5  = (const uint8_t *)(threadBuffers[5]);
  const uint8_t *t6  = (const uint8_t *)(threadBuffers[6]);
  const uint8_t *t7  = (const uint8_t *)(threadBuffers[7]);
  const uint8_t *t8  = (const uint8_t *)(threadBuffers[8]);
  const uint8_t *t9  = (const uint8_t *)(threadBuffers[9]);
  const uint8_t *t10 = (const uint8_t *)(threadBuffers[10]);
  const uint8_t *t11 = (const uint8_t *)(threadBuffers[11]);
  const uint8_t *t12 = (const uint8_t *)(threadBuffers[12]);
  const uint8_t *t13 = (const uint8_t *)(threadBuffers[13]);
  const uint8_t *t14 = (const uint8_t *)(threadBuffers[14]);
  const uint8_t *t15 = (const uint8_t *)(threadBuffers[15]);
  const uint8_t *t16 = (const uint8_t *)(threadBuffers[16]);
  const uint8_t *t17 = (const uint8_t *)(threadBuffers[17]);
  const uint8_t *t18 = (const uint8_t *)(threadBuffers[18]);
  const uint8_t *t19 = (const uint8_t *)(threadBuffers[19]);
  const uint8_t *t20 = (const uint8_t *)(threadBuffers[20]);
  const uint8_t *t21 = (const uint8_t *)(threadBuffers[21]);
  const uint8_t *t22 = (const uint8_t *)(threadBuffers[22]);
  const uint8_t *t23 = (const uint8_t *)(threadBuffers[23]);
  const uint8_t *t24 = (const uint8_t *)(threadBuffers[24]);
  const uint8_t *t25 = (const uint8_t *)(threadBuffers[25]);
  const uint8_t *t26 = (const uint8_t *)(threadBuffers[26]);
  const uint8_t *t27 = (const uint8_t *)(threadBuffers[27]);
  const uint8_t *t28 = (const uint8_t *)(threadBuffers[28]);
  const uint8_t *t29 = (const uint8_t *)(threadBuffers[29]);
  const uint8_t *t30 = (const uint8_t *)(threadBuffers[30]);
  const uint8_t *t31 = (const uint8_t *)(threadBuffers[31]);
  const uint8_t *t32 = (const uint8_t *)(threadBuffers[32]);
  const uint8_t *t33 = (const uint8_t *)(threadBuffers[33]);
  const uint8_t *t34 = (const uint8_t *)(threadBuffers[34]);
  const uint8_t *t35 = (const uint8_t *)(threadBuffers[35]);
  const uint8_t *t36 = (const uint8_t *)(threadBuffers[36]);
  const uint8_t *t37 = (const uint8_t *)(threadBuffers[37]);
  const uint8_t *t38 = (const uint8_t *)(threadBuffers[38]);
  const uint8_t *t39 = (const uint8_t *)(threadBuffers[39]);
  const uint8_t *t40 = (const uint8_t *)(threadBuffers[40]);
  const uint8_t *t41 = (const uint8_t *)(threadBuffers[41]);
  const uint8_t *t42 = (const uint8_t *)(threadBuffers[42]);
  const uint8_t *t43 = (const uint8_t *)(threadBuffers[43]);
  const uint8_t *t44 = (const uint8_t *)(threadBuffers[44]);
  const uint8_t *t45 = (const uint8_t *)(threadBuffers[45]);
  const uint8_t *t46 = (const uint8_t *)(threadBuffers[46]);
  const uint8_t *t47 = (const uint8_t *)(threadBuffers[47]);
  const uint8_t *t48 = (const uint8_t *)(threadBuffers[48]);
  const uint8_t *t49 = (const uint8_t *)(threadBuffers[49]);
  const uint8_t *t50 = (const uint8_t *)(threadBuffers[50]);
  const uint8_t *t51 = (const uint8_t *)(threadBuffers[51]);
  const uint8_t *t52 = (const uint8_t *)(threadBuffers[52]);
  const uint8_t *t53 = (const uint8_t *)(threadBuffers[53]);
  const uint8_t *t54 = (const uint8_t *)(threadBuffers[54]);
  const uint8_t *t55 = (const uint8_t *)(threadBuffers[55]);
  const uint8_t *t56 = (const uint8_t *)(threadBuffers[56]);
  const uint8_t *t57 = (const uint8_t *)(threadBuffers[57]);
  const uint8_t *t58 = (const uint8_t *)(threadBuffers[58]);
  const uint8_t *t59 = (const uint8_t *)(threadBuffers[59]);
  const uint8_t *t60 = (const uint8_t *)(threadBuffers[60]);
  const uint8_t *t61 = (const uint8_t *)(threadBuffers[61]);
  const uint8_t *t62 = (const uint8_t *)(threadBuffers[62]);
  const uint8_t *t63 = (const uint8_t *)(threadBuffers[63]);

  n = outputDataSize/64;

  for(i = 0; i < n; ++i)
  {
    outputBuffer[64*i]    = t0[i];
    outputBuffer[64*i+1]  = t1[i];
    outputBuffer[64*i+2]  = t2[i];
    outputBuffer[64*i+3]  = t3[i];
    outputBuffer[64*i+4]  = t4[i];
    outputBuffer[64*i+5]  = t5[i];
    outputBuffer[64*i+6]  = t6[i];
    outputBuffer[64*i+7]  = t7[i];
    outputBuffer[64*i+8]  = t8[i];
    outputBuffer[64*i+9]  = t9[i];
    outputBuffer[64*i+10] = t10[i];
    outputBuffer[64*i+11] = t11[i];
    outputBuffer[64*i+12] = t12[i];
    outputBuffer[64*i+13] = t13[i];
    outputBuffer[64*i+14] = t14[i];
    outputBuffer[64*i+15] = t15[i];
    outputBuffer[64*i+16] = t16[i];
    outputBuffer[64*i+17] = t17[i];
    outputBuffer[64*i+18] = t18[i];
    outputBuffer[64*i+19] = t19[i];
    outputBuffer[64*i+20] = t20[i];
    outputBuffer[64*i+21] = t21[i];
    outputBuffer[64*i+22] = t22[i];
    outputBuffer[64*i+23] = t23[i];
    outputBuffer[64*i+24] = t24[i];
    outputBuffer[64*i+25] = t25[i];
    outputBuffer[64*i+26] = t26[i];
    outputBuffer[64*i+27] = t27[i];
    outputBuffer[64*i+28] = t28[i];
    outputBuffer[64*i+29] = t29[i];
    outputBuffer[64*i+30] = t30[i];
    outputBuffer[64*i+31] = t31[i];
    outputBuffer[64*i+32] = t32[i];
    outputBuffer[64*i+33] = t33[i];
    outputBuffer[64*i+34] = t34[i];
    outputBuffer[64*i+35] = t35[i];
    outputBuffer[64*i+36] = t36[i];
    outputBuffer[64*i+37] = t37[i];
    outputBuffer[64*i+38] = t38[i];
    outputBuffer[64*i+39] = t39[i];
    outputBuffer[64*i+40] = t40[i];
    outputBuffer[64*i+41] = t41[i];
    outputBuffer[64*i+42] = t42[i];
    outputBuffer[64*i+43] = t43[i];
    outputBuffer[64*i+44] = t44[i];
    outputBuffer[64*i+45] = t45[i];
    outputBuffer[64*i+46] = t46[i];
    outputBuffer[64*i+47] = t47[i];
    outputBuffer[64*i+48] = t48[i];
    outputBuffer[64*i+49] = t49[i];
    outputBuffer[64*i+50] = t50[i];
    outputBuffer[64*i+51] = t51[i];
    outputBuffer[64*i+52] = t52[i];
    outputBuffer[64*i+53] = t53[i];
    outputBuffer[64*i+54] = t54[i];
    outputBuffer[64*i+55] = t55[i];
    outputBuffer[64*i+56] = t56[i];
    outputBuffer[64*i+57] = t57[i];
    outputBuffer[64*i+58] = t58[i];
    outputBuffer[64*i+59] = t59[i];
    outputBuffer[64*i+60] = t60[i];
    outputBuffer[64*i+61] = t61[i];
    outputBuffer[64*i+62] = t62[i];
    outputBuffer[64*i+63] = t63[i];
  }
}


static void cornerturn_2thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/4;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
  }
}

static void cornerturn_3thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2 = (const uint16_t *)(threadBuffers[2]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    out += 2;
  }
}

static void cornerturn_4thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2 = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3 = (const uint16_t *)(threadBuffers[3]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
  }
}

static void cornerturn_5thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2 = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3 = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4 = (const uint16_t *)(threadBuffers[4]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    out += 4;
  }
}

static void cornerturn_6thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2 = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3 = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4 = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5 = (const uint16_t *)(threadBuffers[5]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    out += 3;
  }
}

static void cornerturn_7thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2 = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3 = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4 = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5 = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6 = (const uint16_t *)(threadBuffers[6]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    out += 2;
  }
}

static void cornerturn_8thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0 = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1 = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2 = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3 = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4 = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5 = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6 = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7 = (const uint16_t *)(threadBuffers[7]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
  }
}

static void cornerturn_9thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    out += 8;
  }
}

static void cornerturn_10thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    out += 7;
  }
}

static void cornerturn_11thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    out += 6;
  }
}

static void cornerturn_12thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    out += 5;
  }
}

static void cornerturn_13thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    out += 4;
  }
}

static void cornerturn_14thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    out += 3;
  }
}

static void cornerturn_15thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  const uint16_t *t14 = (const uint16_t *)(threadBuffers[14]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    out += 2;
  }
}

static void cornerturn_16thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  const uint16_t *t14 = (const uint16_t *)(threadBuffers[14]);
  const uint16_t *t15 = (const uint16_t *)(threadBuffers[15]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
  }
}

static void cornerturn_20thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  const uint16_t *t14 = (const uint16_t *)(threadBuffers[14]);
  const uint16_t *t15 = (const uint16_t *)(threadBuffers[15]);
  const uint16_t *t16 = (const uint16_t *)(threadBuffers[16]);
  const uint16_t *t17 = (const uint16_t *)(threadBuffers[17]);
  const uint16_t *t18 = (const uint16_t *)(threadBuffers[18]);
  const uint16_t *t19 = (const uint16_t *)(threadBuffers[19]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/64;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    out += 13;
  }
}

static void cornerturn_24thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  const uint16_t *t14 = (const uint16_t *)(threadBuffers[14]);
  const uint16_t *t15 = (const uint16_t *)(threadBuffers[15]);
  const uint16_t *t16 = (const uint16_t *)(threadBuffers[16]);
  const uint16_t *t17 = (const uint16_t *)(threadBuffers[17]);
  const uint16_t *t18 = (const uint16_t *)(threadBuffers[18]);
  const uint16_t *t19 = (const uint16_t *)(threadBuffers[19]);
  const uint16_t *t20 = (const uint16_t *)(threadBuffers[20]);
  const uint16_t *t21 = (const uint16_t *)(threadBuffers[21]);
  const uint16_t *t22 = (const uint16_t *)(threadBuffers[22]);
  const uint16_t *t23 = (const uint16_t *)(threadBuffers[23]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/64;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    out += 9;
  }
}

static void cornerturn_32thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  const uint16_t *t14 = (const uint16_t *)(threadBuffers[14]);
  const uint16_t *t15 = (const uint16_t *)(threadBuffers[15]);
  const uint16_t *t16 = (const uint16_t *)(threadBuffers[16]);
  const uint16_t *t17 = (const uint16_t *)(threadBuffers[17]);
  const uint16_t *t18 = (const uint16_t *)(threadBuffers[18]);
  const uint16_t *t19 = (const uint16_t *)(threadBuffers[19]);
  const uint16_t *t20 = (const uint16_t *)(threadBuffers[20]);
  const uint16_t *t21 = (const uint16_t *)(threadBuffers[21]);
  const uint16_t *t22 = (const uint16_t *)(threadBuffers[22]);
  const uint16_t *t23 = (const uint16_t *)(threadBuffers[23]);
  const uint16_t *t24 = (const uint16_t *)(threadBuffers[24]);
  const uint16_t *t25 = (const uint16_t *)(threadBuffers[25]);
  const uint16_t *t26 = (const uint16_t *)(threadBuffers[26]);
  const uint16_t *t27 = (const uint16_t *)(threadBuffers[27]);
  const uint16_t *t28 = (const uint16_t *)(threadBuffers[28]);
  const uint16_t *t29 = (const uint16_t *)(threadBuffers[29]);
  const uint16_t *t30 = (const uint16_t *)(threadBuffers[30]);
  const uint16_t *t31 = (const uint16_t *)(threadBuffers[31]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/64;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    ++out;
    *out = t24[i];
    ++out;
    *out = t25[i];
    ++out;
    *out = t26[i];
    ++out;
    *out = t27[i];
    ++out;
    *out = t28[i];
    ++out;
    *out = t29[i];
    ++out;
    *out = t30[i];
    ++out;
    *out = t31[i];
    ++out;
  }
}

static void cornerturn_64thread_16bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint16_t *t0  = (const uint16_t *)(threadBuffers[0]);
  const uint16_t *t1  = (const uint16_t *)(threadBuffers[1]);
  const uint16_t *t2  = (const uint16_t *)(threadBuffers[2]);
  const uint16_t *t3  = (const uint16_t *)(threadBuffers[3]);
  const uint16_t *t4  = (const uint16_t *)(threadBuffers[4]);
  const uint16_t *t5  = (const uint16_t *)(threadBuffers[5]);
  const uint16_t *t6  = (const uint16_t *)(threadBuffers[6]);
  const uint16_t *t7  = (const uint16_t *)(threadBuffers[7]);
  const uint16_t *t8  = (const uint16_t *)(threadBuffers[8]);
  const uint16_t *t9  = (const uint16_t *)(threadBuffers[9]);
  const uint16_t *t10 = (const uint16_t *)(threadBuffers[10]);
  const uint16_t *t11 = (const uint16_t *)(threadBuffers[11]);
  const uint16_t *t12 = (const uint16_t *)(threadBuffers[12]);
  const uint16_t *t13 = (const uint16_t *)(threadBuffers[13]);
  const uint16_t *t14 = (const uint16_t *)(threadBuffers[14]);
  const uint16_t *t15 = (const uint16_t *)(threadBuffers[15]);
  const uint16_t *t16 = (const uint16_t *)(threadBuffers[16]);
  const uint16_t *t17 = (const uint16_t *)(threadBuffers[17]);
  const uint16_t *t18 = (const uint16_t *)(threadBuffers[18]);
  const uint16_t *t19 = (const uint16_t *)(threadBuffers[19]);
  const uint16_t *t20 = (const uint16_t *)(threadBuffers[20]);
  const uint16_t *t21 = (const uint16_t *)(threadBuffers[21]);
  const uint16_t *t22 = (const uint16_t *)(threadBuffers[22]);
  const uint16_t *t23 = (const uint16_t *)(threadBuffers[23]);
  const uint16_t *t24 = (const uint16_t *)(threadBuffers[24]);
  const uint16_t *t25 = (const uint16_t *)(threadBuffers[25]);
  const uint16_t *t26 = (const uint16_t *)(threadBuffers[26]);
  const uint16_t *t27 = (const uint16_t *)(threadBuffers[27]);
  const uint16_t *t28 = (const uint16_t *)(threadBuffers[28]);
  const uint16_t *t29 = (const uint16_t *)(threadBuffers[29]);
  const uint16_t *t30 = (const uint16_t *)(threadBuffers[30]);
  const uint16_t *t31 = (const uint16_t *)(threadBuffers[31]);
  const uint16_t *t32 = (const uint16_t *)(threadBuffers[32]);
  const uint16_t *t33 = (const uint16_t *)(threadBuffers[33]);
  const uint16_t *t34 = (const uint16_t *)(threadBuffers[34]);
  const uint16_t *t35 = (const uint16_t *)(threadBuffers[35]);
  const uint16_t *t36 = (const uint16_t *)(threadBuffers[36]);
  const uint16_t *t37 = (const uint16_t *)(threadBuffers[37]);
  const uint16_t *t38 = (const uint16_t *)(threadBuffers[38]);
  const uint16_t *t39 = (const uint16_t *)(threadBuffers[39]);
  const uint16_t *t40 = (const uint16_t *)(threadBuffers[40]);
  const uint16_t *t41 = (const uint16_t *)(threadBuffers[41]);
  const uint16_t *t42 = (const uint16_t *)(threadBuffers[42]);
  const uint16_t *t43 = (const uint16_t *)(threadBuffers[43]);
  const uint16_t *t44 = (const uint16_t *)(threadBuffers[44]);
  const uint16_t *t45 = (const uint16_t *)(threadBuffers[45]);
  const uint16_t *t46 = (const uint16_t *)(threadBuffers[46]);
  const uint16_t *t47 = (const uint16_t *)(threadBuffers[47]);
  const uint16_t *t48 = (const uint16_t *)(threadBuffers[48]);
  const uint16_t *t49 = (const uint16_t *)(threadBuffers[49]);
  const uint16_t *t50 = (const uint16_t *)(threadBuffers[50]);
  const uint16_t *t51 = (const uint16_t *)(threadBuffers[51]);
  const uint16_t *t52 = (const uint16_t *)(threadBuffers[52]);
  const uint16_t *t53 = (const uint16_t *)(threadBuffers[53]);
  const uint16_t *t54 = (const uint16_t *)(threadBuffers[54]);
  const uint16_t *t55 = (const uint16_t *)(threadBuffers[55]);
  const uint16_t *t56 = (const uint16_t *)(threadBuffers[56]);
  const uint16_t *t57 = (const uint16_t *)(threadBuffers[57]);
  const uint16_t *t58 = (const uint16_t *)(threadBuffers[58]);
  const uint16_t *t59 = (const uint16_t *)(threadBuffers[59]);
  const uint16_t *t60 = (const uint16_t *)(threadBuffers[60]);
  const uint16_t *t61 = (const uint16_t *)(threadBuffers[61]);
  const uint16_t *t62 = (const uint16_t *)(threadBuffers[62]);
  const uint16_t *t63 = (const uint16_t *)(threadBuffers[63]);
  uint16_t *out = (uint16_t *)(outputBuffer);

  n = outputDataSize/128;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    ++out;
    *out = t24[i];
    ++out;
    *out = t25[i];
    ++out;
    *out = t26[i];
    ++out;
    *out = t27[i];
    ++out;
    *out = t28[i];
    ++out;
    *out = t29[i];
    ++out;
    *out = t30[i];
    ++out;
    *out = t31[i];
    ++out;
    *out = t32[i];
    ++out;
    *out = t33[i];
    ++out;
    *out = t34[i];
    ++out;
    *out = t35[i];
    ++out;
    *out = t36[i];
    ++out;
    *out = t37[i];
    ++out;
    *out = t38[i];
    ++out;
    *out = t39[i];
    ++out;
    *out = t40[i];
    ++out;
    *out = t41[i];
    ++out;
    *out = t42[i];
    ++out;
    *out = t43[i];
    ++out;
    *out = t44[i];
    ++out;
    *out = t45[i];
    ++out;
    *out = t46[i];
    ++out;
    *out = t47[i];
    ++out;
    *out = t48[i];
    ++out;
    *out = t49[i];
    ++out;
    *out = t50[i];
    ++out;
    *out = t51[i];
    ++out;
    *out = t52[i];
    ++out;
    *out = t53[i];
    ++out;
    *out = t54[i];
    ++out;
    *out = t55[i];
    ++out;
    *out = t56[i];
    ++out;
    *out = t57[i];
    ++out;
    *out = t58[i];
    ++out;
    *out = t59[i];
    ++out;
    *out = t60[i];
    ++out;
    *out = t61[i];
    ++out;
    *out = t62[i];
    ++out;
    *out = t63[i];
    ++out;
  }
}


static void cornerturn_2thread_32bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint32_t *t0 = (const uint32_t *)(threadBuffers[0]);
  const uint32_t *t1 = (const uint32_t *)(threadBuffers[1]);
  uint32_t *out = (uint32_t *)(outputBuffer);

  n = outputDataSize/8;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
  }
}

static void cornerturn_4thread_32bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint32_t *t0 = (const uint32_t *)(threadBuffers[0]);
  const uint32_t *t1 = (const uint32_t *)(threadBuffers[1]);
  const uint32_t *t2 = (const uint32_t *)(threadBuffers[2]);
  const uint32_t *t3 = (const uint32_t *)(threadBuffers[3]);
  uint32_t *out = (uint32_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
  }
}

static void cornerturn_8thread_32bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint32_t *t0 = (const uint32_t *)(threadBuffers[0]);
  const uint32_t *t1 = (const uint32_t *)(threadBuffers[1]);
  const uint32_t *t2 = (const uint32_t *)(threadBuffers[2]);
  const uint32_t *t3 = (const uint32_t *)(threadBuffers[3]);
  const uint32_t *t4 = (const uint32_t *)(threadBuffers[4]);
  const uint32_t *t5 = (const uint32_t *)(threadBuffers[5]);
  const uint32_t *t6 = (const uint32_t *)(threadBuffers[6]);
  const uint32_t *t7 = (const uint32_t *)(threadBuffers[7]);
  uint32_t *out = (uint32_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
  }
}

static void cornerturn_16thread_32bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint32_t *t0  = (const uint32_t *)(threadBuffers[0]);
  const uint32_t *t1  = (const uint32_t *)(threadBuffers[1]);
  const uint32_t *t2  = (const uint32_t *)(threadBuffers[2]);
  const uint32_t *t3  = (const uint32_t *)(threadBuffers[3]);
  const uint32_t *t4  = (const uint32_t *)(threadBuffers[4]);
  const uint32_t *t5  = (const uint32_t *)(threadBuffers[5]);
  const uint32_t *t6  = (const uint32_t *)(threadBuffers[6]);
  const uint32_t *t7  = (const uint32_t *)(threadBuffers[7]);
  const uint32_t *t8  = (const uint32_t *)(threadBuffers[8]);
  const uint32_t *t9  = (const uint32_t *)(threadBuffers[9]);
  const uint32_t *t10 = (const uint32_t *)(threadBuffers[10]);
  const uint32_t *t11 = (const uint32_t *)(threadBuffers[11]);
  const uint32_t *t12 = (const uint32_t *)(threadBuffers[12]);
  const uint32_t *t13 = (const uint32_t *)(threadBuffers[13]);
  const uint32_t *t14 = (const uint32_t *)(threadBuffers[14]);
  const uint32_t *t15 = (const uint32_t *)(threadBuffers[15]);
  uint32_t *out = (uint32_t *)(outputBuffer);

  n = outputDataSize/64;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
  }
}

static void cornerturn_32thread_32bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint32_t *t0  = (const uint32_t *)(threadBuffers[0]);
  const uint32_t *t1  = (const uint32_t *)(threadBuffers[1]);
  const uint32_t *t2  = (const uint32_t *)(threadBuffers[2]);
  const uint32_t *t3  = (const uint32_t *)(threadBuffers[3]);
  const uint32_t *t4  = (const uint32_t *)(threadBuffers[4]);
  const uint32_t *t5  = (const uint32_t *)(threadBuffers[5]);
  const uint32_t *t6  = (const uint32_t *)(threadBuffers[6]);
  const uint32_t *t7  = (const uint32_t *)(threadBuffers[7]);
  const uint32_t *t8  = (const uint32_t *)(threadBuffers[8]);
  const uint32_t *t9  = (const uint32_t *)(threadBuffers[9]);
  const uint32_t *t10 = (const uint32_t *)(threadBuffers[10]);
  const uint32_t *t11 = (const uint32_t *)(threadBuffers[11]);
  const uint32_t *t12 = (const uint32_t *)(threadBuffers[12]);
  const uint32_t *t13 = (const uint32_t *)(threadBuffers[13]);
  const uint32_t *t14 = (const uint32_t *)(threadBuffers[14]);
  const uint32_t *t15 = (const uint32_t *)(threadBuffers[15]);
  const uint32_t *t16 = (const uint32_t *)(threadBuffers[16]);
  const uint32_t *t17 = (const uint32_t *)(threadBuffers[17]);
  const uint32_t *t18 = (const uint32_t *)(threadBuffers[18]);
  const uint32_t *t19 = (const uint32_t *)(threadBuffers[19]);
  const uint32_t *t20 = (const uint32_t *)(threadBuffers[20]);
  const uint32_t *t21 = (const uint32_t *)(threadBuffers[21]);
  const uint32_t *t22 = (const uint32_t *)(threadBuffers[22]);
  const uint32_t *t23 = (const uint32_t *)(threadBuffers[23]);
  const uint32_t *t24 = (const uint32_t *)(threadBuffers[24]);
  const uint32_t *t25 = (const uint32_t *)(threadBuffers[25]);
  const uint32_t *t26 = (const uint32_t *)(threadBuffers[26]);
  const uint32_t *t27 = (const uint32_t *)(threadBuffers[27]);
  const uint32_t *t28 = (const uint32_t *)(threadBuffers[28]);
  const uint32_t *t29 = (const uint32_t *)(threadBuffers[29]);
  const uint32_t *t30 = (const uint32_t *)(threadBuffers[30]);
  const uint32_t *t31 = (const uint32_t *)(threadBuffers[31]);
  uint32_t *out = (uint32_t *)(outputBuffer);

  n = outputDataSize/128;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    ++out;
    *out = t24[i];
    ++out;
    *out = t25[i];
    ++out;
    *out = t26[i];
    ++out;
    *out = t27[i];
    ++out;
    *out = t28[i];
    ++out;
    *out = t29[i];
    ++out;
    *out = t30[i];
    ++out;
    *out = t31[i];
    ++out;
  }
}

static void cornerturn_64thread_32bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint32_t *t0  = (const uint32_t *)(threadBuffers[0]);
  const uint32_t *t1  = (const uint32_t *)(threadBuffers[1]);
  const uint32_t *t2  = (const uint32_t *)(threadBuffers[2]);
  const uint32_t *t3  = (const uint32_t *)(threadBuffers[3]);
  const uint32_t *t4  = (const uint32_t *)(threadBuffers[4]);
  const uint32_t *t5  = (const uint32_t *)(threadBuffers[5]);
  const uint32_t *t6  = (const uint32_t *)(threadBuffers[6]);
  const uint32_t *t7  = (const uint32_t *)(threadBuffers[7]);
  const uint32_t *t8  = (const uint32_t *)(threadBuffers[8]);
  const uint32_t *t9  = (const uint32_t *)(threadBuffers[9]);
  const uint32_t *t10 = (const uint32_t *)(threadBuffers[10]);
  const uint32_t *t11 = (const uint32_t *)(threadBuffers[11]);
  const uint32_t *t12 = (const uint32_t *)(threadBuffers[12]);
  const uint32_t *t13 = (const uint32_t *)(threadBuffers[13]);
  const uint32_t *t14 = (const uint32_t *)(threadBuffers[14]);
  const uint32_t *t15 = (const uint32_t *)(threadBuffers[15]);
  const uint32_t *t16 = (const uint32_t *)(threadBuffers[16]);
  const uint32_t *t17 = (const uint32_t *)(threadBuffers[17]);
  const uint32_t *t18 = (const uint32_t *)(threadBuffers[18]);
  const uint32_t *t19 = (const uint32_t *)(threadBuffers[19]);
  const uint32_t *t20 = (const uint32_t *)(threadBuffers[20]);
  const uint32_t *t21 = (const uint32_t *)(threadBuffers[21]);
  const uint32_t *t22 = (const uint32_t *)(threadBuffers[22]);
  const uint32_t *t23 = (const uint32_t *)(threadBuffers[23]);
  const uint32_t *t24 = (const uint32_t *)(threadBuffers[24]);
  const uint32_t *t25 = (const uint32_t *)(threadBuffers[25]);
  const uint32_t *t26 = (const uint32_t *)(threadBuffers[26]);
  const uint32_t *t27 = (const uint32_t *)(threadBuffers[27]);
  const uint32_t *t28 = (const uint32_t *)(threadBuffers[28]);
  const uint32_t *t29 = (const uint32_t *)(threadBuffers[29]);
  const uint32_t *t30 = (const uint32_t *)(threadBuffers[30]);
  const uint32_t *t31 = (const uint32_t *)(threadBuffers[31]);
  const uint32_t *t32 = (const uint32_t *)(threadBuffers[32]);
  const uint32_t *t33 = (const uint32_t *)(threadBuffers[33]);
  const uint32_t *t34 = (const uint32_t *)(threadBuffers[34]);
  const uint32_t *t35 = (const uint32_t *)(threadBuffers[35]);
  const uint32_t *t36 = (const uint32_t *)(threadBuffers[36]);
  const uint32_t *t37 = (const uint32_t *)(threadBuffers[37]);
  const uint32_t *t38 = (const uint32_t *)(threadBuffers[38]);
  const uint32_t *t39 = (const uint32_t *)(threadBuffers[39]);
  const uint32_t *t40 = (const uint32_t *)(threadBuffers[40]);
  const uint32_t *t41 = (const uint32_t *)(threadBuffers[41]);
  const uint32_t *t42 = (const uint32_t *)(threadBuffers[42]);
  const uint32_t *t43 = (const uint32_t *)(threadBuffers[43]);
  const uint32_t *t44 = (const uint32_t *)(threadBuffers[44]);
  const uint32_t *t45 = (const uint32_t *)(threadBuffers[45]);
  const uint32_t *t46 = (const uint32_t *)(threadBuffers[46]);
  const uint32_t *t47 = (const uint32_t *)(threadBuffers[47]);
  const uint32_t *t48 = (const uint32_t *)(threadBuffers[48]);
  const uint32_t *t49 = (const uint32_t *)(threadBuffers[49]);
  const uint32_t *t50 = (const uint32_t *)(threadBuffers[50]);
  const uint32_t *t51 = (const uint32_t *)(threadBuffers[51]);
  const uint32_t *t52 = (const uint32_t *)(threadBuffers[52]);
  const uint32_t *t53 = (const uint32_t *)(threadBuffers[53]);
  const uint32_t *t54 = (const uint32_t *)(threadBuffers[54]);
  const uint32_t *t55 = (const uint32_t *)(threadBuffers[55]);
  const uint32_t *t56 = (const uint32_t *)(threadBuffers[56]);
  const uint32_t *t57 = (const uint32_t *)(threadBuffers[57]);
  const uint32_t *t58 = (const uint32_t *)(threadBuffers[58]);
  const uint32_t *t59 = (const uint32_t *)(threadBuffers[59]);
  const uint32_t *t60 = (const uint32_t *)(threadBuffers[60]);
  const uint32_t *t61 = (const uint32_t *)(threadBuffers[61]);
  const uint32_t *t62 = (const uint32_t *)(threadBuffers[62]);
  const uint32_t *t63 = (const uint32_t *)(threadBuffers[63]);
  uint32_t *out = (uint32_t *)(outputBuffer);

  n = outputDataSize/256;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    ++out;
    *out = t24[i];
    ++out;
    *out = t25[i];
    ++out;
    *out = t26[i];
    ++out;
    *out = t27[i];
    ++out;
    *out = t28[i];
    ++out;
    *out = t29[i];
    ++out;
    *out = t30[i];
    ++out;
    *out = t31[i];
    ++out;
    *out = t32[i];
    ++out;
    *out = t33[i];
    ++out;
    *out = t34[i];
    ++out;
    *out = t35[i];
    ++out;
    *out = t36[i];
    ++out;
    *out = t37[i];
    ++out;
    *out = t38[i];
    ++out;
    *out = t39[i];
    ++out;
    *out = t40[i];
    ++out;
    *out = t41[i];
    ++out;
    *out = t42[i];
    ++out;
    *out = t43[i];
    ++out;
    *out = t44[i];
    ++out;
    *out = t45[i];
    ++out;
    *out = t46[i];
    ++out;
    *out = t47[i];
    ++out;
    *out = t48[i];
    ++out;
    *out = t49[i];
    ++out;
    *out = t50[i];
    ++out;
    *out = t51[i];
    ++out;
    *out = t52[i];
    ++out;
    *out = t53[i];
    ++out;
    *out = t54[i];
    ++out;
    *out = t55[i];
    ++out;
    *out = t56[i];
    ++out;
    *out = t57[i];
    ++out;
    *out = t58[i];
    ++out;
    *out = t59[i];
    ++out;
    *out = t60[i];
    ++out;
    *out = t61[i];
    ++out;
    *out = t62[i];
    ++out;
    *out = t63[i];
    ++out;
  }
}


static void cornerturn_2thread_64bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0 = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1 = (const uint64_t *)(threadBuffers[1]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
  }
}

static void cornerturn_4thread_64bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0 = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1 = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2 = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3 = (const uint64_t *)(threadBuffers[3]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
  }
}

static void cornerturn_8thread_64bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0 = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1 = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2 = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3 = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4 = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5 = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6 = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7 = (const uint64_t *)(threadBuffers[7]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/64;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
  }
}

static void cornerturn_16thread_64bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0  = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1  = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2  = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3  = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4  = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5  = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6  = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7  = (const uint64_t *)(threadBuffers[7]);
  const uint64_t *t8  = (const uint64_t *)(threadBuffers[8]);
  const uint64_t *t9  = (const uint64_t *)(threadBuffers[9]);
  const uint64_t *t10 = (const uint64_t *)(threadBuffers[10]);
  const uint64_t *t11 = (const uint64_t *)(threadBuffers[11]);
  const uint64_t *t12 = (const uint64_t *)(threadBuffers[12]);
  const uint64_t *t13 = (const uint64_t *)(threadBuffers[13]);
  const uint64_t *t14 = (const uint64_t *)(threadBuffers[14]);
  const uint64_t *t15 = (const uint64_t *)(threadBuffers[15]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/128;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
  }
}

static void cornerturn_32thread_64bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0  = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1  = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2  = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3  = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4  = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5  = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6  = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7  = (const uint64_t *)(threadBuffers[7]);
  const uint64_t *t8  = (const uint64_t *)(threadBuffers[8]);
  const uint64_t *t9  = (const uint64_t *)(threadBuffers[9]);
  const uint64_t *t10 = (const uint64_t *)(threadBuffers[10]);
  const uint64_t *t11 = (const uint64_t *)(threadBuffers[11]);
  const uint64_t *t12 = (const uint64_t *)(threadBuffers[12]);
  const uint64_t *t13 = (const uint64_t *)(threadBuffers[13]);
  const uint64_t *t14 = (const uint64_t *)(threadBuffers[14]);
  const uint64_t *t15 = (const uint64_t *)(threadBuffers[15]);
  const uint64_t *t16 = (const uint64_t *)(threadBuffers[16]);
  const uint64_t *t17 = (const uint64_t *)(threadBuffers[17]);
  const uint64_t *t18 = (const uint64_t *)(threadBuffers[18]);
  const uint64_t *t19 = (const uint64_t *)(threadBuffers[19]);
  const uint64_t *t20 = (const uint64_t *)(threadBuffers[20]);
  const uint64_t *t21 = (const uint64_t *)(threadBuffers[21]);
  const uint64_t *t22 = (const uint64_t *)(threadBuffers[22]);
  const uint64_t *t23 = (const uint64_t *)(threadBuffers[23]);
  const uint64_t *t24 = (const uint64_t *)(threadBuffers[24]);
  const uint64_t *t25 = (const uint64_t *)(threadBuffers[25]);
  const uint64_t *t26 = (const uint64_t *)(threadBuffers[26]);
  const uint64_t *t27 = (const uint64_t *)(threadBuffers[27]);
  const uint64_t *t28 = (const uint64_t *)(threadBuffers[28]);
  const uint64_t *t29 = (const uint64_t *)(threadBuffers[29]);
  const uint64_t *t30 = (const uint64_t *)(threadBuffers[30]);
  const uint64_t *t31 = (const uint64_t *)(threadBuffers[31]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/256;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    ++out;
    *out = t24[i];
    ++out;
    *out = t25[i];
    ++out;
    *out = t26[i];
    ++out;
    *out = t27[i];
    ++out;
    *out = t28[i];
    ++out;
    *out = t29[i];
    ++out;
    *out = t30[i];
    ++out;
    *out = t31[i];
    ++out;
  }
}

static void cornerturn_64thread_64bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0  = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1  = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2  = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3  = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4  = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5  = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6  = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7  = (const uint64_t *)(threadBuffers[7]);
  const uint64_t *t8  = (const uint64_t *)(threadBuffers[8]);
  const uint64_t *t9  = (const uint64_t *)(threadBuffers[9]);
  const uint64_t *t10 = (const uint64_t *)(threadBuffers[10]);
  const uint64_t *t11 = (const uint64_t *)(threadBuffers[11]);
  const uint64_t *t12 = (const uint64_t *)(threadBuffers[12]);
  const uint64_t *t13 = (const uint64_t *)(threadBuffers[13]);
  const uint64_t *t14 = (const uint64_t *)(threadBuffers[14]);
  const uint64_t *t15 = (const uint64_t *)(threadBuffers[15]);
  const uint64_t *t16 = (const uint64_t *)(threadBuffers[16]);
  const uint64_t *t17 = (const uint64_t *)(threadBuffers[17]);
  const uint64_t *t18 = (const uint64_t *)(threadBuffers[18]);
  const uint64_t *t19 = (const uint64_t *)(threadBuffers[19]);
  const uint64_t *t20 = (const uint64_t *)(threadBuffers[20]);
  const uint64_t *t21 = (const uint64_t *)(threadBuffers[21]);
  const uint64_t *t22 = (const uint64_t *)(threadBuffers[22]);
  const uint64_t *t23 = (const uint64_t *)(threadBuffers[23]);
  const uint64_t *t24 = (const uint64_t *)(threadBuffers[24]);
  const uint64_t *t25 = (const uint64_t *)(threadBuffers[25]);
  const uint64_t *t26 = (const uint64_t *)(threadBuffers[26]);
  const uint64_t *t27 = (const uint64_t *)(threadBuffers[27]);
  const uint64_t *t28 = (const uint64_t *)(threadBuffers[28]);
  const uint64_t *t29 = (const uint64_t *)(threadBuffers[29]);
  const uint64_t *t30 = (const uint64_t *)(threadBuffers[30]);
  const uint64_t *t31 = (const uint64_t *)(threadBuffers[31]);
  const uint64_t *t32 = (const uint64_t *)(threadBuffers[32]);
  const uint64_t *t33 = (const uint64_t *)(threadBuffers[33]);
  const uint64_t *t34 = (const uint64_t *)(threadBuffers[34]);
  const uint64_t *t35 = (const uint64_t *)(threadBuffers[35]);
  const uint64_t *t36 = (const uint64_t *)(threadBuffers[36]);
  const uint64_t *t37 = (const uint64_t *)(threadBuffers[37]);
  const uint64_t *t38 = (const uint64_t *)(threadBuffers[38]);
  const uint64_t *t39 = (const uint64_t *)(threadBuffers[39]);
  const uint64_t *t40 = (const uint64_t *)(threadBuffers[40]);
  const uint64_t *t41 = (const uint64_t *)(threadBuffers[41]);
  const uint64_t *t42 = (const uint64_t *)(threadBuffers[42]);
  const uint64_t *t43 = (const uint64_t *)(threadBuffers[43]);
  const uint64_t *t44 = (const uint64_t *)(threadBuffers[44]);
  const uint64_t *t45 = (const uint64_t *)(threadBuffers[45]);
  const uint64_t *t46 = (const uint64_t *)(threadBuffers[46]);
  const uint64_t *t47 = (const uint64_t *)(threadBuffers[47]);
  const uint64_t *t48 = (const uint64_t *)(threadBuffers[48]);
  const uint64_t *t49 = (const uint64_t *)(threadBuffers[49]);
  const uint64_t *t50 = (const uint64_t *)(threadBuffers[50]);
  const uint64_t *t51 = (const uint64_t *)(threadBuffers[51]);
  const uint64_t *t52 = (const uint64_t *)(threadBuffers[52]);
  const uint64_t *t53 = (const uint64_t *)(threadBuffers[53]);
  const uint64_t *t54 = (const uint64_t *)(threadBuffers[54]);
  const uint64_t *t55 = (const uint64_t *)(threadBuffers[55]);
  const uint64_t *t56 = (const uint64_t *)(threadBuffers[56]);
  const uint64_t *t57 = (const uint64_t *)(threadBuffers[57]);
  const uint64_t *t58 = (const uint64_t *)(threadBuffers[58]);
  const uint64_t *t59 = (const uint64_t *)(threadBuffers[59]);
  const uint64_t *t60 = (const uint64_t *)(threadBuffers[60]);
  const uint64_t *t61 = (const uint64_t *)(threadBuffers[61]);
  const uint64_t *t62 = (const uint64_t *)(threadBuffers[62]);
  const uint64_t *t63 = (const uint64_t *)(threadBuffers[63]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/512;

  for(i = 0; i < n; ++i)
  {
    *out = t0[i];
    ++out;
    *out = t1[i];
    ++out;
    *out = t2[i];
    ++out;
    *out = t3[i];
    ++out;
    *out = t4[i];
    ++out;
    *out = t5[i];
    ++out;
    *out = t6[i];
    ++out;
    *out = t7[i];
    ++out;
    *out = t8[i];
    ++out;
    *out = t9[i];
    ++out;
    *out = t10[i];
    ++out;
    *out = t11[i];
    ++out;
    *out = t12[i];
    ++out;
    *out = t13[i];
    ++out;
    *out = t14[i];
    ++out;
    *out = t15[i];
    ++out;
    *out = t16[i];
    ++out;
    *out = t17[i];
    ++out;
    *out = t18[i];
    ++out;
    *out = t19[i];
    ++out;
    *out = t20[i];
    ++out;
    *out = t21[i];
    ++out;
    *out = t22[i];
    ++out;
    *out = t23[i];
    ++out;
    *out = t24[i];
    ++out;
    *out = t25[i];
    ++out;
    *out = t26[i];
    ++out;
    *out = t27[i];
    ++out;
    *out = t28[i];
    ++out;
    *out = t29[i];
    ++out;
    *out = t30[i];
    ++out;
    *out = t31[i];
    ++out;
    *out = t32[i];
    ++out;
    *out = t33[i];
    ++out;
    *out = t34[i];
    ++out;
    *out = t35[i];
    ++out;
    *out = t36[i];
    ++out;
    *out = t37[i];
    ++out;
    *out = t38[i];
    ++out;
    *out = t39[i];
    ++out;
    *out = t40[i];
    ++out;
    *out = t41[i];
    ++out;
    *out = t42[i];
    ++out;
    *out = t43[i];
    ++out;
    *out = t44[i];
    ++out;
    *out = t45[i];
    ++out;
    *out = t46[i];
    ++out;
    *out = t47[i];
    ++out;
    *out = t48[i];
    ++out;
    *out = t49[i];
    ++out;
    *out = t50[i];
    ++out;
    *out = t51[i];
    ++out;
    *out = t52[i];
    ++out;
    *out = t53[i];
    ++out;
    *out = t54[i];
    ++out;
    *out = t55[i];
    ++out;
    *out = t56[i];
    ++out;
    *out = t57[i];
    ++out;
    *out = t58[i];
    ++out;
    *out = t59[i];
    ++out;
    *out = t60[i];
    ++out;
    *out = t61[i];
    ++out;
    *out = t62[i];
    ++out;
    *out = t63[i];
    ++out;
  }
}


static void cornerturn_2thread_128bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0 = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1 = (const uint64_t *)(threadBuffers[1]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/16;

  for(i = 0; i < n; i += 2)
  {
    *out = t0[i];
    ++out;
    *out = t0[i+1];
    ++out;
    *out = t1[i];
    ++out;
    *out = t1[i+1];
    ++out;
  }
}

static void cornerturn_4thread_128bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0 = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1 = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2 = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3 = (const uint64_t *)(threadBuffers[3]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/32;

  for(i = 0; i < n; i += 2)
  {
    *out = t0[i];
    ++out;
    *out = t0[i+1];
    ++out;
    *out = t1[i];
    ++out;
    *out = t1[i+1];
    ++out;
    *out = t2[i];
    ++out;
    *out = t2[i+1];
    ++out;
    *out = t3[i];
    ++out;
    *out = t3[i+1];
    ++out;
  }
}

static void cornerturn_8thread_128bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0 = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1 = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2 = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3 = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4 = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5 = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6 = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7 = (const uint64_t *)(threadBuffers[7]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/64;

  for(i = 0; i < n; i += 2)
  {
    *out = t0[i];
    ++out;
    *out = t0[i+1];
    ++out;
    *out = t1[i];
    ++out;
    *out = t1[i+1];
    ++out;
    *out = t2[i];
    ++out;
    *out = t2[i+1];
    ++out;
    *out = t3[i];
    ++out;
    *out = t3[i+1];
    ++out;
    *out = t4[i];
    ++out;
    *out = t4[i+1];
    ++out;
    *out = t5[i];
    ++out;
    *out = t5[i+1];
    ++out;
    *out = t6[i];
    ++out;
    *out = t6[i+1];
    ++out;
    *out = t7[i];
    ++out;
    *out = t7[i+1];
    ++out;
  }
}

static void cornerturn_16thread_128bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0  = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1  = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2  = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3  = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4  = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5  = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6  = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7  = (const uint64_t *)(threadBuffers[7]);
  const uint64_t *t8  = (const uint64_t *)(threadBuffers[8]);
  const uint64_t *t9  = (const uint64_t *)(threadBuffers[9]);
  const uint64_t *t10 = (const uint64_t *)(threadBuffers[10]);
  const uint64_t *t11 = (const uint64_t *)(threadBuffers[11]);
  const uint64_t *t12 = (const uint64_t *)(threadBuffers[12]);
  const uint64_t *t13 = (const uint64_t *)(threadBuffers[13]);
  const uint64_t *t14 = (const uint64_t *)(threadBuffers[14]);
  const uint64_t *t15 = (const uint64_t *)(threadBuffers[15]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/128;

  for(i = 0; i < n; i+=2)
  {
    *out = t0[i];
    ++out;
    *out = t0[i+1];
    ++out;
    *out = t1[i];
    ++out;
    *out = t1[i+1];
    ++out;
    *out = t2[i];
    ++out;
    *out = t2[i+1];
    ++out;
    *out = t3[i];
    ++out;
    *out = t3[i+1];
    ++out;
    *out = t4[i];
    ++out;
    *out = t4[i+1];
    ++out;
    *out = t5[i];
    ++out;
    *out = t5[i+1];
    ++out;
    *out = t6[i];
    ++out;
    *out = t6[i+1];
    ++out;
    *out = t7[i];
    ++out;
    *out = t7[i+1];
    ++out;
    *out = t8[i];
    ++out;
    *out = t8[i+1];
    ++out;
    *out = t9[i];
    ++out;
    *out = t9[i+1];
    ++out;
    *out = t10[i];
    ++out;
    *out = t10[i+1];
    ++out;
    *out = t11[i];
    ++out;
    *out = t11[i+1];
    ++out;
    *out = t12[i];
    ++out;
    *out = t12[i+1];
    ++out;
    *out = t13[i];
    ++out;
    *out = t13[i+1];
    ++out;
    *out = t14[i];
    ++out;
    *out = t14[i+1];
    ++out;
    *out = t15[i];
    ++out;
    *out = t15[i+1];
    ++out;
  }
}

static void cornerturn_32thread_128bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0  = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1  = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2  = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3  = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4  = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5  = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6  = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7  = (const uint64_t *)(threadBuffers[7]);
  const uint64_t *t8  = (const uint64_t *)(threadBuffers[8]);
  const uint64_t *t9  = (const uint64_t *)(threadBuffers[9]);
  const uint64_t *t10 = (const uint64_t *)(threadBuffers[10]);
  const uint64_t *t11 = (const uint64_t *)(threadBuffers[11]);
  const uint64_t *t12 = (const uint64_t *)(threadBuffers[12]);
  const uint64_t *t13 = (const uint64_t *)(threadBuffers[13]);
  const uint64_t *t14 = (const uint64_t *)(threadBuffers[14]);
  const uint64_t *t15 = (const uint64_t *)(threadBuffers[15]);
  const uint64_t *t16 = (const uint64_t *)(threadBuffers[16]);
  const uint64_t *t17 = (const uint64_t *)(threadBuffers[17]);
  const uint64_t *t18 = (const uint64_t *)(threadBuffers[18]);
  const uint64_t *t19 = (const uint64_t *)(threadBuffers[19]);
  const uint64_t *t20 = (const uint64_t *)(threadBuffers[20]);
  const uint64_t *t21 = (const uint64_t *)(threadBuffers[21]);
  const uint64_t *t22 = (const uint64_t *)(threadBuffers[22]);
  const uint64_t *t23 = (const uint64_t *)(threadBuffers[23]);
  const uint64_t *t24 = (const uint64_t *)(threadBuffers[24]);
  const uint64_t *t25 = (const uint64_t *)(threadBuffers[25]);
  const uint64_t *t26 = (const uint64_t *)(threadBuffers[26]);
  const uint64_t *t27 = (const uint64_t *)(threadBuffers[27]);
  const uint64_t *t28 = (const uint64_t *)(threadBuffers[28]);
  const uint64_t *t29 = (const uint64_t *)(threadBuffers[29]);
  const uint64_t *t30 = (const uint64_t *)(threadBuffers[30]);
  const uint64_t *t31 = (const uint64_t *)(threadBuffers[31]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/256;

  for(i = 0; i < n; i+=2)
  {
    *out = t0[i];  ++out; *out = t0[i+1];  ++out;
    *out = t1[i];  ++out; *out = t1[i+1];  ++out;
    *out = t2[i];  ++out; *out = t2[i+1];  ++out;
    *out = t3[i];  ++out; *out = t3[i+1];  ++out;
    *out = t4[i];  ++out; *out = t4[i+1];  ++out;
    *out = t5[i];  ++out; *out = t5[i+1];  ++out;
    *out = t6[i];  ++out; *out = t6[i+1];  ++out;
    *out = t7[i];  ++out; *out = t7[i+1];  ++out;
    *out = t8[i];  ++out; *out = t8[i+1];  ++out;
    *out = t9[i];  ++out; *out = t9[i+1];  ++out;
    *out = t10[i]; ++out; *out = t10[i+1]; ++out;
    *out = t11[i]; ++out; *out = t11[i+1]; ++out;
    *out = t12[i]; ++out; *out = t12[i+1]; ++out;
    *out = t13[i]; ++out; *out = t13[i+1]; ++out;
    *out = t14[i]; ++out; *out = t14[i+1]; ++out;
    *out = t15[i]; ++out; *out = t15[i+1]; ++out;
    *out = t16[i]; ++out; *out = t16[i+1]; ++out;
    *out = t17[i]; ++out; *out = t17[i+1]; ++out;
    *out = t18[i]; ++out; *out = t18[i+1]; ++out;
    *out = t19[i]; ++out; *out = t19[i+1]; ++out;
    *out = t20[i]; ++out; *out = t20[i+1]; ++out;
    *out = t21[i]; ++out; *out = t21[i+1]; ++out;
    *out = t22[i]; ++out; *out = t22[i+1]; ++out;
    *out = t23[i]; ++out; *out = t23[i+1]; ++out;
    *out = t24[i]; ++out; *out = t24[i+1]; ++out;
    *out = t25[i]; ++out; *out = t25[i+1]; ++out;
    *out = t26[i]; ++out; *out = t26[i+1]; ++out;
    *out = t27[i]; ++out; *out = t27[i+1]; ++out;
    *out = t28[i]; ++out; *out = t28[i+1]; ++out;
    *out = t29[i]; ++out; *out = t29[i+1]; ++out;
    *out = t30[i]; ++out; *out = t30[i+1]; ++out;
    *out = t31[i]; ++out; *out = t31[i+1]; ++out;
  }
}

static void cornerturn_64thread_128bit(unsigned char *outputBuffer, const unsigned char * const *threadBuffers, int outputDataSize)
{
  // interleave bytes
  int i, n;
  const uint64_t *t0  = (const uint64_t *)(threadBuffers[0]);
  const uint64_t *t1  = (const uint64_t *)(threadBuffers[1]);
  const uint64_t *t2  = (const uint64_t *)(threadBuffers[2]);
  const uint64_t *t3  = (const uint64_t *)(threadBuffers[3]);
  const uint64_t *t4  = (const uint64_t *)(threadBuffers[4]);
  const uint64_t *t5  = (const uint64_t *)(threadBuffers[5]);
  const uint64_t *t6  = (const uint64_t *)(threadBuffers[6]);
  const uint64_t *t7  = (const uint64_t *)(threadBuffers[7]);
  const uint64_t *t8  = (const uint64_t *)(threadBuffers[8]);
  const uint64_t *t9  = (const uint64_t *)(threadBuffers[9]);
  const uint64_t *t10 = (const uint64_t *)(threadBuffers[10]);
  const uint64_t *t11 = (const uint64_t *)(threadBuffers[11]);
  const uint64_t *t12 = (const uint64_t *)(threadBuffers[12]);
  const uint64_t *t13 = (const uint64_t *)(threadBuffers[13]);
  const uint64_t *t14 = (const uint64_t *)(threadBuffers[14]);
  const uint64_t *t15 = (const uint64_t *)(threadBuffers[15]);
  const uint64_t *t16 = (const uint64_t *)(threadBuffers[16]);
  const uint64_t *t17 = (const uint64_t *)(threadBuffers[17]);
  const uint64_t *t18 = (const uint64_t *)(threadBuffers[18]);
  const uint64_t *t19 = (const uint64_t *)(threadBuffers[19]);
  const uint64_t *t20 = (const uint64_t *)(threadBuffers[20]);
  const uint64_t *t21 = (const uint64_t *)(threadBuffers[21]);
  const uint64_t *t22 = (const uint64_t *)(threadBuffers[22]);
  const uint64_t *t23 = (const uint64_t *)(threadBuffers[23]);
  const uint64_t *t24 = (const uint64_t *)(threadBuffers[24]);
  const uint64_t *t25 = (const uint64_t *)(threadBuffers[25]);
  const uint64_t *t26 = (const uint64_t *)(threadBuffers[26]);
  const uint64_t *t27 = (const uint64_t *)(threadBuffers[27]);
  const uint64_t *t28 = (const uint64_t *)(threadBuffers[28]);
  const uint64_t *t29 = (const uint64_t *)(threadBuffers[29]);
  const uint64_t *t30 = (const uint64_t *)(threadBuffers[30]);
  const uint64_t *t31 = (const uint64_t *)(threadBuffers[31]);
  const uint64_t *t32 = (const uint64_t *)(threadBuffers[32]);
  const uint64_t *t33 = (const uint64_t *)(threadBuffers[33]);
  const uint64_t *t34 = (const uint64_t *)(threadBuffers[34]);
  const uint64_t *t35 = (const uint64_t *)(threadBuffers[35]);
  const uint64_t *t36 = (const uint64_t *)(threadBuffers[36]);
  const uint64_t *t37 = (const uint64_t *)(threadBuffers[37]);
  const uint64_t *t38 = (const uint64_t *)(threadBuffers[38]);
  const uint64_t *t39 = (const uint64_t *)(threadBuffers[39]);
  const uint64_t *t40 = (const uint64_t *)(threadBuffers[40]);
  const uint64_t *t41 = (const uint64_t *)(threadBuffers[41]);
  const uint64_t *t42 = (const uint64_t *)(threadBuffers[42]);
  const uint64_t *t43 = (const uint64_t *)(threadBuffers[43]);
  const uint64_t *t44 = (const uint64_t *)(threadBuffers[44]);
  const uint64_t *t45 = (const uint64_t *)(threadBuffers[45]);
  const uint64_t *t46 = (const uint64_t *)(threadBuffers[46]);
  const uint64_t *t47 = (const uint64_t *)(threadBuffers[47]);
  const uint64_t *t48 = (const uint64_t *)(threadBuffers[48]);
  const uint64_t *t49 = (const uint64_t *)(threadBuffers[49]);
  const uint64_t *t50 = (const uint64_t *)(threadBuffers[50]);
  const uint64_t *t51 = (const uint64_t *)(threadBuffers[51]);
  const uint64_t *t52 = (const uint64_t *)(threadBuffers[52]);
  const uint64_t *t53 = (const uint64_t *)(threadBuffers[53]);
  const uint64_t *t54 = (const uint64_t *)(threadBuffers[54]);
  const uint64_t *t55 = (const uint64_t *)(threadBuffers[55]);
  const uint64_t *t56 = (const uint64_t *)(threadBuffers[56]);
  const uint64_t *t57 = (const uint64_t *)(threadBuffers[57]);
  const uint64_t *t58 = (const uint64_t *)(threadBuffers[58]);
  const uint64_t *t59 = (const uint64_t *)(threadBuffers[59]);
  const uint64_t *t60 = (const uint64_t *)(threadBuffers[60]);
  const uint64_t *t61 = (const uint64_t *)(threadBuffers[61]);
  const uint64_t *t62 = (const uint64_t *)(threadBuffers[62]);
  const uint64_t *t63 = (const uint64_t *)(threadBuffers[63]);
  uint64_t *out = (uint64_t *)(outputBuffer);

  n = outputDataSize/512;

  for(i = 0; i < n; i+=2)
  {
    *out = t0[i];  ++out; *out = t0[i+1];  ++out;
    *out = t1[i];  ++out; *out = t1[i+1];  ++out;
    *out = t2[i];  ++out; *out = t2[i+1];  ++out;
    *out = t3[i];  ++out; *out = t3[i+1];  ++out;
    *out = t4[i];  ++out; *out = t4[i+1];  ++out;
    *out = t5[i];  ++out; *out = t5[i+1];  ++out;
    *out = t6[i];  ++out; *out = t6[i+1];  ++out;
    *out = t7[i];  ++out; *out = t7[i+1];  ++out;
    *out = t8[i];  ++out; *out = t8[i+1];  ++out;
    *out = t9[i];  ++out; *out = t9[i+1];  ++out;
    *out = t10[i]; ++out; *out = t10[i+1]; ++out;
    *out = t11[i]; ++out; *out = t11[i+1]; ++out;
    *out = t12[i]; ++out; *out = t12[i+1]; ++out;
    *out = t13[i]; ++out; *out = t13[i+1]; ++out;
    *out = t14[i]; ++out; *out = t14[i+1]; ++out;
    *out = t15[i]; ++out; *out = t15[i+1]; ++out;
    *out = t16[i]; ++out; *out = t16[i+1]; ++out;
    *out = t17[i]; ++out; *out = t17[i+1]; ++out;
    *out = t18[i]; ++out; *out = t18[i+1]; ++out;
    *out = t19[i]; ++out; *out = t19[i+1]; ++out;
    *out = t20[i]; ++out; *out = t20[i+1]; ++out;
    *out = t21[i]; ++out; *out = t21[i+1]; ++out;
    *out = t22[i]; ++out; *out = t22[i+1]; ++out;
    *out = t23[i]; ++out; *out = t23[i+1]; ++out;
    *out = t24[i]; ++out; *out = t24[i+1]; ++out;
    *out = t25[i]; ++out; *out = t25[i+1]; ++out;
    *out = t26[i]; ++out; *out = t26[i+1]; ++out;
    *out = t27[i]; ++out; *out = t27[i+1]; ++out;
    *out = t28[i]; ++out; *out = t28[i+1]; ++out;
    *out = t29[i]; ++out; *out = t29[i+1]; ++out;
    *out = t30[i]; ++out; *out = t30[i+1]; ++out;
    *out = t31[i]; ++out; *out = t31[i+1]; ++out;
    *out = t32[i]; ++out; *out = t32[i+1]; ++out;
    *out = t33[i]; ++out; *out = t33[i+1]; ++out;
    *out = t34[i]; ++out; *out = t34[i+1]; ++out;
    *out = t35[i]; ++out; *out = t35[i+1]; ++out;
    *out = t36[i]; ++out; *out = t36[i+1]; ++out;
    *out = t37[i]; ++out; *out = t37[i+1]; ++out;
    *out = t38[i]; ++out; *out = t38[i+1]; ++out;
    *out = t39[i]; ++out; *out = t39[i+1]; ++out;
    *out = t40[i]; ++out; *out = t40[i+1]; ++out;
    *out = t41[i]; ++out; *out = t41[i+1]; ++out;
    *out = t42[i]; ++out; *out = t42[i+1]; ++out;
    *out = t43[i]; ++out; *out = t43[i+1]; ++out;
    *out = t44[i]; ++out; *out = t44[i+1]; ++out;
    *out = t45[i]; ++out; *out = t45[i+1]; ++out;
    *out = t46[i]; ++out; *out = t46[i+1]; ++out;
    *out = t47[i]; ++out; *out = t47[i+1]; ++out;
    *out = t48[i]; ++out; *out = t48[i+1]; ++out;
    *out = t49[i]; ++out; *out = t49[i+1]; ++out;
    *out = t50[i]; ++out; *out = t50[i+1]; ++out;
    *out = t51[i]; ++out; *out = t51[i+1]; ++out;
    *out = t52[i]; ++out; *out = t52[i+1]; ++out;
    *out = t53[i]; ++out; *out = t53[i+1]; ++out;
    *out = t54[i]; ++out; *out = t54[i+1]; ++out;
    *out = t55[i]; ++out; *out = t55[i+1]; ++out;
    *out = t56[i]; ++out; *out = t56[i+1]; ++out;
    *out = t57[i]; ++out; *out = t57[i+1]; ++out;
    *out = t58[i]; ++out; *out = t58[i+1]; ++out;
    *out = t59[i]; ++out; *out = t59[i+1]; ++out;
    *out = t60[i]; ++out; *out = t60[i+1]; ++out;
    *out = t61[i]; ++out; *out = t61[i+1]; ++out;
    *out = t62[i]; ++out; *out = t62[i+1]; ++out;
    *out = t63[i]; ++out; *out = t63[i+1]; ++out;
  }
}


void (*getCornerTurner(int nThread, int nBit))(unsigned char *, const unsigned char * const *, int)
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
		case 16:
			return cornerturn_16thread_1bit;
		case 32:
			return cornerturn_32thread_1bit;
		case 64:
			return cornerturn_64thread_1bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_1bit;
		case 5:
			return cornerturn_5thread_1bit;
		case 6:
			return cornerturn_6thread_1bit;
		case 7:
			return cornerturn_7thread_1bit;
		case 9:
			return cornerturn_9thread_1bit;
		case 10:
			return cornerturn_10thread_1bit;
		case 11:
			return cornerturn_11thread_1bit;
		case 12:
			return cornerturn_12thread_1bit;
		case 13:
			return cornerturn_13thread_1bit;
		case 14:
			return cornerturn_14thread_1bit;
		case 15:
			return cornerturn_15thread_1bit;
		case 20:
			return cornerturn_20thread_1bit;
		case 24:
			return cornerturn_24thread_1bit;
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
		case 32:
			return cornerturn_32thread_2bit;
		case 64:
			return cornerturn_64thread_2bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_2bit;
		case 5:
			return cornerturn_5thread_2bit;
		case 6:
			return cornerturn_6thread_2bit;
		case 7:
			return cornerturn_7thread_2bit;
		case 9:
			return cornerturn_9thread_2bit;
		case 10:
			return cornerturn_10thread_2bit;
		case 11:
			return cornerturn_11thread_2bit;
		case 12:
			return cornerturn_12thread_2bit;
		case 13:
			return cornerturn_13thread_2bit;
		case 14:
			return cornerturn_14thread_2bit;
		case 15:
			return cornerturn_15thread_2bit;
		case 20:
			return cornerturn_20thread_2bit;
		case 24:
			return cornerturn_24thread_2bit;
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
		case 16:
			return cornerturn_16thread_4bit;
		case 32:
			return cornerturn_32thread_4bit;
		case 64:
			return cornerturn_64thread_4bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_4bit;
		case 5:
			return cornerturn_5thread_4bit;
		case 6:
			return cornerturn_6thread_4bit;
		case 7:
			return cornerturn_7thread_4bit;
		case 9:
			return cornerturn_9thread_4bit;
		case 10:
			return cornerturn_10thread_4bit;
		case 11:
			return cornerturn_11thread_4bit;
		case 12:
			return cornerturn_12thread_4bit;
		case 13:
			return cornerturn_13thread_4bit;
		case 14:
			return cornerturn_14thread_4bit;
		case 15:
			return cornerturn_15thread_4bit;
		case 20:
			return cornerturn_20thread_4bit;
		case 24:
			return cornerturn_24thread_4bit;
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
		case 32:
			return cornerturn_32thread_8bit;
		case 64:
			return cornerturn_64thread_8bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_8bit;
		case 5:
			return cornerturn_5thread_8bit;
		case 6:
			return cornerturn_6thread_8bit;
		case 7:
			return cornerturn_7thread_8bit;
		case 9:
			return cornerturn_9thread_8bit;
		case 10:
			return cornerturn_10thread_8bit;
		case 11:
			return cornerturn_11thread_8bit;
		case 12:
			return cornerturn_12thread_8bit;
		case 13:
			return cornerturn_13thread_8bit;
		case 14:
			return cornerturn_14thread_8bit;
		case 15:
			return cornerturn_15thread_8bit;
		case 20:
			return cornerturn_20thread_8bit;
		case 24:
			return cornerturn_24thread_8bit;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 16)
	{
		switch(nThread)
		{
		/* First list the most common ones: powers of two */
		case 2:
			return cornerturn_2thread_16bit;
		case 4:
			return cornerturn_4thread_16bit;
		case 8:
			return cornerturn_8thread_16bit;
		case 16:
			return cornerturn_16thread_16bit;
		case 32:
			return cornerturn_32thread_16bit;
		case 64:
			return cornerturn_64thread_16bit;
		/* Then the non-powers-of-two */
		case 3:
			return cornerturn_3thread_16bit;
		case 5:
			return cornerturn_5thread_16bit;
		case 6:
			return cornerturn_6thread_16bit;
		case 7:
			return cornerturn_7thread_16bit;
		case 9:
			return cornerturn_9thread_16bit;
		case 10:
			return cornerturn_10thread_16bit;
		case 11:
			return cornerturn_11thread_16bit;
		case 12:
			return cornerturn_12thread_16bit;
		case 13:
			return cornerturn_13thread_16bit;
		case 14:
			return cornerturn_14thread_16bit;
		case 15:
			return cornerturn_15thread_16bit;
		case 20:
			return cornerturn_20thread_16bit;
		case 24:
			return cornerturn_24thread_16bit;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 32)
	{
		switch(nThread)
		{
		case 2:
			return cornerturn_2thread_32bit;
		case 4:
			return cornerturn_4thread_32bit;
		case 8:
			return cornerturn_8thread_32bit;
		case 16:
			return cornerturn_16thread_32bit;
		case 32:
			return cornerturn_32thread_32bit;
		case 64:
			return cornerturn_64thread_32bit;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 64)
	{
		switch(nThread)
		{
		case 2:
			return cornerturn_2thread_64bit;
		case 4:
			return cornerturn_4thread_64bit;
		case 8:
			return cornerturn_8thread_64bit;
		case 16:
			return cornerturn_16thread_64bit;
		case 32:
			return cornerturn_32thread_64bit;
		case 64:
			return cornerturn_64thread_64bit;
		/* unsupported cases */
		default:
			return 0;
		}
	}
	else if(nBit == 128)
	{
		switch(nThread)
		{
		case 2:
			return cornerturn_2thread_128bit;
		case 4:
			return cornerturn_4thread_128bit;
		case 8:
			return cornerturn_8thread_128bit;
		case 16:
			return cornerturn_16thread_128bit;
		case 32:
			return cornerturn_32thread_128bit;
		case 64:
			return cornerturn_64thread_128bit;
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

static int testCornerTurn(const unsigned char *outputBuffer, const unsigned char * const *threadData, int outputBytes, int nt, int b)
{
	int nError = 0;
	int ut;	/* lowest power of 2 >= nt */

	for(ut = 1; ut < nt; ut *= 2);

	if(b <= 8)
	{
		const int nSample = 8*outputBytes/b;	/* total samples in output stream */
		int s;
		int bitmask;

		bitmask = (1LL << b) - 1LL;

		for(s = 0; s < nSample; ++s)
		{
			int t;	/* thread */
			int outputvalue, inputvalue;
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
	}
	else
	{
		const int B = b/8;	/* bytes per sample */
		int i;
		int lastBad = -1;	/* don't double count errors.  Want number of samples with an error */

		for(i = 0; i < outputBytes; ++i)
		{
			int outputvalue, inputvalue;
			int ib; /* input byte number of current sample */
			int ob;	/* output byte number of current sample */
			int is;	/* input sample number */
			int os; /* output sample number */
			int t;	/* thread id o f current sample */

			t = (i/B) % ut;
			if(t >= nt)
			{
				/* ignore padded data */
				continue;
			}

			ib = i % B;
			ob = i % B;
			is = i/(B*ut);
			os = i/B;

			inputvalue = threadData[t][B*is+ib];
			outputvalue = outputBuffer[B*os+ob];
			
			if(outputvalue != inputvalue)
			{
				if(lastBad != os)
				{
					++nError;
					lastBad = os;
				}
			}
		}
	}

	return nError;
}

void testvdifcornerturners(int outputBytes, int nTest)
{
	const char devRandom[] = "/dev/urandom";
	const int bits[] = { 1, 2, 4, 8, 16, 32, 64, 128, 0 };
	const int maxThreads = 64;
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
			fclose(in);

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
