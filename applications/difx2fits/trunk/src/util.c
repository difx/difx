/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/applications/difx2fits/trunk/src/fitsTS.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <glob.h>
#include "util.h"

int glob2(const char *label, const char *pattern, int flags, int errfunc(const char *epath, int eerrno), glob_t *pglob)
{
	int v;

	v = glob(pattern, flags, errfunc, pglob);
	if(v != 0)
	{
		if(v == GLOB_NOSPACE)
		{
			fprintf(stderr, "Error: %s: No space!\n", label);

			exit(EXIT_FAILURE);
		}
		else if(v == GLOB_ABORTED)
		{
			fprintf(stderr, "Error: %s: Read error!\n", label);

			exit(EXIT_FAILURE);
		}
		else if(v == GLOB_NOMATCH)
		{
			/* no matching files, so quietly leave function */
		}
		else
		{
			fprintf(stderr, "Error: %s: unknown glob() failure! %d\n", label, v);

			exit(EXIT_FAILURE);
		}
	}

	return v;
}
