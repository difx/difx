/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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
#include <string.h>
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

/* look for files that match given filename modulo case.  Put result in
 * filename that was passed to this function.
 *
 * If multiple matches are found, use one at random :( and squawk
 *
 * Return: number of files that matched.
 */
int globcase(const char *label, const char *match, char *fileName)
{
	int i;
	int n = 0;
	glob_t globbuf;

	glob2(label, match, 0, 0, &globbuf);

	if(globbuf.gl_pathc == 0)
	{
		globfree(&globbuf);

		return 0;
	}
	for(i = 0; i < globbuf.gl_pathc; ++i)
	{
		if(strcasecmp(fileName, globbuf.gl_pathv[i]) == 0)
		{
			if(n == 0)
			{
				strcpy(fileName, globbuf.gl_pathv[i]);
			}
			else if(n == 1)
			{
				fprintf(stderr, "\nError: multiple filenames matching %s differing only in case were found.\n", fileName);

				exit(EXIT_FAILURE);
			}
			++n;
		}
	}

	globfree(&globbuf);

	return n;
}
