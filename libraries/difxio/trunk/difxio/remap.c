/***************************************************************************
 *   Copyright (C) 2011 by Walter Brisken                                  *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include "difx_input.h"

void fprintRemap(FILE *out, const char *name, const int *Remap)
{
	int i;
	fprintf(out, "  %s Remap =", name);
	if(Remap == 0 || Remap[0] < 0)
	{
		fprintf(out, " None\n");
	}
	else
	{
		for(i = 0; Remap[i] >= 0; i++)
		{
			fprintf(out, " %d", Remap[i]);
		}
		fprintf(out, "\n");
	}
}

void printRemap(const char *name, const int *Remap)
{
	fprintRemap(stdout, name, Remap);
}

int *newRemap(int nItem)
{
	int *Remap;

	Remap = (int *)calloc(nItem+1, sizeof(int));
	Remap[nItem] = -1;

	return Remap;
}

void deleteRemap(int *Remap)
{
	if(Remap)
	{
		free(Remap);
	}
}

int *dupRemap(const int *Remap)
{
	int i, n;
	int *r2;

	if(!Remap)
	{
		return 0;
	}

	/* count size of Remap */
	for(n = 0; Remap[n] >= 0; n++) {}

	r2 = newRemap(n);
	for(i = 0; i < n; i++)
	{
		r2[i] = Remap[i];
	}

	return r2;
}

int sizeofRemap(const int *Remap)
{
	int n;

	if(Remap)
	{
		for(n = 0; Remap[n] >= 0; n++) {}
	}
	else
	{
		n = 0;
	}

	return n;
}

int reverseRemap(const int *Remap, int y)      /* find index corresponding to y */
{
	int i;

	if(!Remap)
	{
		/* if no Remap array, assume the identity transform */

		return y;
	}
	else
	{
		for(i = 0; Remap[i] >= 0; i++)
		{
			if(Remap[i] == y)
			{
				return y;
			}
		}
	}

	return -1;
}
