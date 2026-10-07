/***************************************************************************
 *   Wrappers for the generic (FFTW + C++) section of architecture.h       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* This file may be compiled several times with different flags, each copy
 * being a separate implementation (see Makefile): ARCH_IMPL_NAME and
 * ARCH_IMPL_ORDER name it and place it in reports.  The functions of
 * architecture.h are inline, so the linker would keep only one copy of each
 * across the implementations; they are made static here, so that each copy
 * keeps its own code.  System headers are included first so that this does
 * not reach them. */
#ifndef ARCH_IMPL_NAME
#define ARCH_IMPL_NAME "generic"
#endif
#ifndef ARCH_IMPL_ORDER
#define ARCH_IMPL_ORDER 1
#endif

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <pthread.h>
#include <fftw3.h>
#if defined(__x86_64__) || defined(__i386__)
#include <immintrin.h>
#endif
#include "arch_api.h"

/* defined in mode.cpp in mpifxcorr; one shared definition here */
inline pthread_mutex_t FFTinitMutex = PTHREAD_MUTEX_INITIALIZER;

#define inline static inline
#include "arch_generic.h"	/* ../src/architecture.h.in with @ipp_enabled@ = 0 */
#undef inline

#if ARCH != GENERIC
#error "arch_generic.h does not select the generic architecture"
#endif

/* Routines whose vector* macro does not compile in the generic section (see
 * README) */
#include "arch_generic_skips.h"

static ArchImpl genericImpl;
static ArchImplRegister registerImpl(genericImpl, ARCH_IMPL_NAME, ARCH_IMPL_ORDER);

/* the generic section has no rounding mode constants and ignores the mode */
#define ARCH_RNDMODE(r)	(r)
#define ARCH_FFTFLAG(f)	((f) ? vecFFT_ReNorm : vecFFT_NoReNorm)

#define WRAP(name, params, call) \
	static int w_##name params { return (int)(call); } \
	static ArchRegister reg_##name(genericImpl, #name, (arch_fn)w_##name);
#define WRAP_BODY(name, params, ...) \
	static int w_##name params __VA_ARGS__ \
	static ArchRegister reg_##name(genericImpl, #name, (arch_fn)w_##name);
#define SKIP(name) \
	static ArchSkip skip_##name(genericImpl, #name, ARCH_SKIP_##name);

#include "arch_wrappers.inc"
