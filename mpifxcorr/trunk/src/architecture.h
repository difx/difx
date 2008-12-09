/***************************************************************************
 *   Copyright (C) 2005 by Adam Deller                                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

/** \file architecture.h
 *  \brief File contains mapping for vector functions to specific architectures
 */
 
#ifndef ARCHITECTURE_H
#define ARCHITECTURE_H

#define INTEL   1
#define AMD     2
#define GENERIC 3

//define the MPI tags
#define CR_TERMINATE      0
#define CR_VALIDVIS       1
#define CR_RECEIVETIME    2
#define CR_PROCESSDATA    3
#define CR_PROCESSCONTROL 4
#define DS_TERMINATE      5
#define DS_PROCESS        6

//define the architecture to be compiled for here
#define ARCH INTEL

//if no architecture is selected, default to generic 
#ifndef ARCH
#define ARCH GENERIC
#endif

//set up the function mapping for the intel architecture
#if(ARCH == INTEL)
#include <ipps.h>
#include <ippvm.h>
#include <ippcore.h>

//start with the data types
#define u8                       Ipp8u
#define u16                      Ipp16u
#define s16                      Ipp16s
#define cs16                     Ipp16sc
#define s32                      Ipp32s
#define u32                      Ipp32u
#define f32                      Ipp32f
#define cf32                     Ipp32fc
#define f64                      Ipp64f
#define s64                      Ipp64s
#define u64                      Ipp64u

//and the constant values
#define vecNoErr                 ippStsNoErr
#define vecFFTSpecR_f32          IppsFFTSpec_R_32f
#define vecFFTSpecC_f32          IppsFFTSpec_C_32f
#define vecFFTSpec_s16           IppsFFTSpec_R_16s
#define vecHintAlg               IppHintAlgorithm
#define vecRndZero               ippRndZero
#define vecRndNear               ippRndNear
#define vecHamming               ippWinHamming
#define vecTrue                  ippTrue
#define MAX_S32                  IPP_MAX_32S
#define MAX_U32                  IPP_MAX_32U
#define MAX_S16                  IPP_MAX_16S
#define MAX_U16                  IPP_MAX_16U
#define vecFFT_NoReNorm          IPP_FFT_NODIV_BY_ANY
#define vecAlgHintFast           ippAlgHintFast
#define TWO_PI                   IPP_2PI

//now the vector functions themselves
#define vectorAlloc_u8(length)   ippsMalloc_8u(length)
#define vectorAlloc_s16(length)  ippsMalloc_16s(length)
#define vectorAlloc_cs16(length) ippsMalloc_16sc(length)
#define vectorAlloc_s32(length)  ippsMalloc_32s(length)
#define vectorAlloc_f32(length)  ippsMalloc_32f(length)
#define vectorAlloc_cf32(length) ippsMalloc_32fc(length)
#define vectorAlloc_f64(length)  ippsMalloc_64f(length)

#define vectorFree(memptr)       ippsFree(memptr)

#define vectorAdd_f32_I(src, srcdest, length)                               ippsAdd_32f_I(src, srcdest, length)
#define vectorAdd_s16_I(src, srcdest, length)                               ippsAdd_16s_I(src, srcdest, length)
#define vectorAdd_s32_I(src, srcdest, length)                               ippsAdd_32s_ISfs(src, srcdest, length, 0)
#define vectorAdd_cf32_I(src, srcdest, length)                              ippsAdd_32fc_I(src, srcdest, length)
#define vectorAddC_f64(src, val, dest, length)                              ippsAddC_64f(src, val, dest, length)
#define vectorAddC_f32(src, val, dest, length)                              ippsAddC_32f(src, val, dest, length)
#define vectorAddC_f32_I(val, srcdest, length)                              ippsAddC_32f_I(val, srcdest, length)
#define vectorAddC_s16_I(val, srcdest, length)                              ippsAddC_16s_I(val, srcdest, length)
#define vectorAddC_f64_I(val, srcdest, length)                              ippsAddC_64f_I(val, srcdest, length)

#define vectorAddProduct_cf32(src1, src2, accumulator, length)              ippsAddProduct_32fc(src1, src2, accumulator, length)

#define vectorConj_cf32(src, dest, length)                                  ippsConj_32fc(src, dest, length)
#define vectorConjFlip_cf32(src, dest, length)                              ippsConjFlip_32fc(src, dest, length)

#define vectorCopy_u8(src, dest, length)                                    ippsCopy_8u(src, dest, length)
#define vectorCopy_s16(src, dest, length)                                   ippsCopy_16s(src, dest, length)
#define vectorCopy_s32(src, dest, length)                                   ippsCopy_32f((f32*)src, (f32*)dest, length)
#define vectorCopy_f32(src, dest, length)                                   ippsCopy_32f(src, dest, length)
#define vectorCopy_cf32(src, dest, length)                                  ippsCopy_32fc(src, dest, length)
#define vectorCopy_f64(src, dest, length)                                   ippsCopy_64f(src, dest, length)

#define vectorCos_f32(src, dest, length)                                    ippsCos_32f_A11(src, dest, length)

#define vectorConvertScaled_s16f32(src, dest, length, scalefactor)          ippsConvert_16s32f_Sfs(src, dest, length, scalefactor)
#define vectorConvertScaled_f32s16(src, dest, length, rndmode, scalefactor) ippsConvert_32f16s_Sfs(src, dest, length, rndmode, scalefactor)
#define vectorConvertScaled_f32u8(src, dest, length, rndmode, scalefactor)  ippsConvert_32f8u_Sfs(src, dest, length, rndmode, scalefactor)
#define vectorConvert_f32s32(src, dest, length, rndmode)                    ippsConvert_32f32s_Sfs(src, dest, length, rndmode, 0)
#define vectorConvert_s16f32(src, dest, length)                             ippsConvert_16s32f(src, dest, length)
#define vectorConvert_s32f32(src, dest, length)                             ippsConvert_32s32f(src, dest, length)
#define vectorConvert_f64f32(src, dest, length)                             ippsConvert_64f32f(src, dest, length)

#define vectorDotProduct_f64(src1, src2, length, output)                    ippsDotProd_64f(src1, src2, length, output);

#define vectorInitFFTR_f32(fftspec, order, flag, hint)                      ippsFFTInitAlloc_R_32f(fftspec, order, flag, hint)
#define vectorInitFFTC_f32(fftspec, order, flag, hint)                      ippsFFTInitAlloc_C_32f(fftspec, order, flag, hint)
//#define vectorInitFFT_f32(fftspec, order, flag, hint)                       ippsDFTInitAlloc_R_32f(fftspec, order, flag, hint)
#define vectorInitFFT_s16(fftspec, order, flag, hint)                       ippsFFTInitAlloc_R_16s(fftspec, order, flag, hint)
#define vectorGetFFTBufSizeR_f32(fftspec, buffersize)                       ippsFFTGetBufSize_R_32f(fftspec, buffersize)
#define vectorGetFFTBufSizeC_f32(fftspec, buffersize)                       ippsFFTGetBufSize_C_32f(fftspec, buffersize)
//#define vectorGetFFTBufSize_f32(fftspec, buffersize)                        ippsDFTGetBufSize_R_32f(fftspec, buffersize)
#define vectorGetFFTBufSize_s16(fftspec, buffersize)                        ippsFFTGetBufSize_R_16s(fftspec, buffersize)
#define vectorFreeFFTR_f32(fftspec)                                         ippsFFTFree_R_32f(fftspec)
#define vectorFreeFFTC_f32(fftspec)                                         ippsFFTFree_C_32f(fftspec)
//#define vectorFreeFFT_f32(fftspec)                                          ippsDFTFree_R_32f(pFFTSpec)
#define vectorFFT_RtoC_f32(src, dest, fftspec, fftbuffer)                   ippsFFTFwd_RToCCS_32f(src, dest, fftspec, fftbuffer)
#define vectorFFT_CtoC_f32(srcre, srcim, destre, destim, fftspec, fftbuff)  ippsFFTFwd_CToC_32f(srcre, srcim, destre, destim, fftspec, fftbuff)
//#define vectorFFT_RtoC_f32(src, dest, fftspec, fftbuffer)                   ippsDFTFwd_RToCCS_32f(src, dest, fftspec, fftbuffer)
#define vectorScaledFFT_RtoC_s16(src, dest, fftspec, fftbuffer, scale)      ippsFFTFwd_RToCCS_16s_Sfs(src, dest, fftspec, fftbuffer, scale)

#define vectorFlip_f64_I(srcdest, length)                                   ippsFlip_64f_I(srcdest, length)

#define vectorMagnitude_cf32(src, dest, length)                             ippsMagnitude_32fc(src, dest, length)

#define vectorMean_cf32(src, length, mean, hint)                            ippsMean_32fc(src, length, mean, hint)

#define vectorMul_f32(src1, src2, dest, length)                             ippsMul_32f(src1, src2, dest, length)
#define vectorMul_f32_I(src, srcdest, length)                               ippsMul_32f_I(src, srcdest, length)
#define vectorMul_cf32_I(src, srcdest, length)                              ippsMul_32fc_I(src, srcdest, length)
#define vectorMul_cf32(src1, src2, dest, length)                            ippsMul_32fc(src1, src2, dest, length)
#define vectorMulC_f32(src, val, dest, length)                              ippsMulC_32f(src, val, dest, length)
#define vectorMulC_cs16_I(val, srcdest, length)                             ippsMulC_16sc_ISfs(val, srcdest, length, 0)
#define vectorMulC_f32_I(val, srcdest, length)                              ippsMulC_32f_I(val, srcdest, length)
#define vectorMulC_cf32_I(val, srcdest, length)                             ippsMulC_32fc_I(val, srcdest, length)
#define vectorMulC_f64_I(val, srcdest, length)                              ippsMulC_64f_I(val, srcdest, length)
#define vectorMulC_f64(src, val, dest, length)                              ippsMulC_64f(src, val, dest, length)

#define vectorPhase_cf32(src, dest, length)                                 ippsPhase_32fc(src, dest, length)

#define vectorRealToComplex_f32(real, imag, complex, length)                ippsRealToCplx_32f(real, imag, complex, length)

#define vectorSet_f32(val, dest, length)                                    ippsSet_32f(val, dest, length)

#define vectorSin_f32(src, dest, length)                                    ippsSin_32f_A11(src, dest, length)

#define vectorSinCos_f32(src, sin, cos, length)                             ippsSinCos_32f_A11(src, sin, cos, length)

#define vectorSplitScaled_s16f32(src, dest, numchannels, chanlen)           ippsSplitScaled_16s32f_D2L(src, dest, numchannels, chanlen)

#define vectorSquare_f64_I(srcdest, length)                                 ippsSqr_64f_I(srcdest, length)

#define vectorSub_f32_I(src, srcdest, length)                               ippsSub_32f_I(src, srcdest, length)
#define vectorSub_s32(src1, src2, dest, length)                             ippsSub_32s_Sfs(src1, src2, dest, length, 0)
#define vectorSub_cf32_I(src, srcdest, length)                              ippsSub_32fc_I(src, srcdest, length)

#define vectorGenerateFIRLowpass_f64(freq, taps, length, window, normalise) ippsFIRGenLowpass_64f(freq, taps, length, window, normalise)

#define vectorZero_cf32(dest, length)                                       ippsZero_32fc(dest, length)
#define vectorZero_f32(dest, length)                                        ippsZero_32f(dest, length)
#define vectorZero_s16(dest, length)                                        ippsZero_16s(dest, length)
#define vectorZero_s32(dest, length)                                        genericZero_32s(dest, length)

inline int genericZero_32s(s32 * dest, int length)
{ for(int i=0;i<length;i++) dest[i] = 0; return vecNoErr; }

#endif /*Architecture == Intel */


#if (ARCH == GENERIC)

//start with the types
typedef struct {
  short re;
  short im;
} sc16;

typedef struct {
  float re;
  float im;
} fc32;

#define u8                       unsigned char
#define u16                      unsigned short
#define s16                      short
#define cs16                     sc16
#define s32                      int
#define f32                      float
#define cf32                     fc32
#define f64                      double

//then the constant values
#define vecNoErr                 0
#define vecTrue                  1
#define TWO_PI                   6.28318530716
#define MAX_S16                  ((1<<15) - 1)
#define MAX_U16                  ((1<<16) - 1)

//then the generic functions
inline int genericCopy_f32(f32 * src, f32 * dest, int length)
{ for(int i=0;i<length;i++) dest[i] = src[i]; return vecNoErr; }
inline int genericCopy_u8(u8 * src, u8 * dest, int length)
{ for(int i=0;i<length;i++) dest[i] = src[i]; return vecNoErr; }

//vector allocation and deletion routines
#define vectorAlloc_u8(length)   new u8[length]
#define vectorAlloc_s16(length)  new s16[length]
#define vectorAlloc_cs16(length) new cs16[length
#define vectorAlloc_s32(length)  new s32[length]
#define vectorAlloc_f32(length)  new f32[length]
#define vectorAlloc_cf32(length) new cf32[length]
#define vectorAlloc_f64(length)  new f64[length]

#define vectorFree(memptr)       delete [] memptr

//and finally other vector routines
#define vectorCopy_f32(src, dest, length)       genericCopy_f32(src, dest, length)
#define vectorCopy_u8(src, dest, length)        genericCopy_u8(src, dest, length)

#endif /* Generic Architecture */

#endif /* Defined architecture header */
