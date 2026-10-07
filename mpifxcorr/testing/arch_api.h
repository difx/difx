/***************************************************************************
 *   Common interface to the architecture.h implementations under test     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* architecture.h defines the vector* API either with Intel IPP or with the
 * generic (FFTW + plain C++) code.  The two cannot be compiled into one
 * translation unit (same names, different types), so each is compiled into
 * its own file of thin wrappers (arch_generic.cpp, arch_ipp.cpp) using only
 * the neutral types below, and registers the wrappers by name.  The test and
 * benchmark programs look routines up by name in each implementation that
 * was built. */

#ifndef ARCH_API_H
#define ARCH_API_H

#include <stdint.h>
#include <map>
#include <string>
#include <vector>

/* layout compatible with cf32/cf64/cs16 of both implementations */
struct acf32 { float re, im; };
struct acf64 { double re, im; };
struct acs16 { int16_t re, im; };

typedef void (*arch_fn)(void);

struct ArchImpl
{
	std::string name;
	std::map<std::string, arch_fn> fns;
	std::map<std::string, int> skipped;	/* routines left out: ARCH_SKIP_BROKEN or ARCH_SKIP_ABSENT */

	arch_fn get(const std::string &n) const
	{
		std::map<std::string, arch_fn>::const_iterator it = fns.find(n);

		return it == fns.end() ? 0 : it->second;
	}
};

struct ArchRegister
{
	ArchRegister(ArchImpl &impl, const char *name, arch_fn fn) { impl.fns[name] = fn; }
};

/* why a routine is left out of an implementation (values set by find_skips.sh) */
enum { ARCH_SKIP_BROKEN = 1, ARCH_SKIP_ABSENT = 2 };

struct ArchSkip
{
	ArchSkip(ArchImpl &impl, const char *name, int why) { impl.skipped[name] = why; }
};

/* Rounding modes and FFT normalisation as passed to the wrappers; each
 * implementation maps them to its own constants */
enum { ARCH_RND_ZERO = 0, ARCH_RND_NEAR = 1 };
enum { ARCH_FFT_NORENORM = 0, ARCH_FFT_RENORM = 1 };

/* Sentinel written to FFT work buffer outputs before init, to detect an init
 * that does not set them */
#define ARCH_UNSET_PTR ((unsigned char *)0x5A5A5A5A5A5A5A5AULL)
#define ARCH_UNSET_INT (-123456789)

ArchImpl &arch_generic_impl();
#ifdef HAVE_IPP
ArchImpl &arch_ipp_impl();
#endif

#endif
