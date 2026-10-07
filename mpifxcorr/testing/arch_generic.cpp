/***************************************************************************
 *   Wrappers for the generic (FFTW + C++) section of architecture.h       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

#include <string.h>
#include <pthread.h>
#include "arch_api.h"
#include "arch_generic.h"	/* ../src/architecture.h.in with @ipp_enabled@ = 0 */

#if ARCH != GENERIC
#error "arch_generic.h does not select the generic architecture"
#endif

/* defined in mode.cpp in mpifxcorr */
pthread_mutex_t FFTinitMutex = PTHREAD_MUTEX_INITIALIZER;

/* Routines whose vector* macro does not compile in the generic section (see
 * README).  `make check-compile` tries each of them again. */
#include "arch_generic_skips.h"

static ArchImpl genericImpl;

ArchImpl &arch_generic_impl()
{
	genericImpl.name = "generic";

	return genericImpl;
}

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
