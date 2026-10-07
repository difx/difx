/***************************************************************************
 *   Registry of the architecture.h implementations linked in              *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* Kept out of the wrapper files, which may be compiled with flags for a
 * newer CPU than the one running the program */

#include <algorithm>
#include "arch_api.h"

std::vector<const ArchImpl *> &arch_impls()
{
	static std::vector<const ArchImpl *> impls;

	return impls;
}

std::vector<const ArchImpl *> arch_impls_sorted()
{
	std::vector<const ArchImpl *> v = arch_impls();

	std::sort(v.begin(), v.end(), [](const ArchImpl *a, const ArchImpl *b) { return a->order < b->order; });

	return v;
}
