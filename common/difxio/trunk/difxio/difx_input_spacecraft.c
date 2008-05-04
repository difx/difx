/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"


DifxSpacecraft *newDifxSpacecraftArray(int nSpacecraft)
{
	DifxSpacecraft *ds;
	
	if(nSpacecraft == 0)
	{
		return 0;
	}
	
	ds = (DifxSpacecraft *)calloc(nSpacecraft, sizeof(DifxSpacecraft));

	return ds;
}

DifxSpacecraft *dupDifxSpacecraftArray(const DifxSpacecraft *src, int n)
{
	DifxSpacecraft *dest;
	int s;

	dest = newDifxSpacecraftArray(n);

	for(s = 0; s < n; s++)
	{
		strcpy(dest[s].name, src[s].name);
		dest[s].nPoint = src[s].nPoint;
		dest[s].pos = (sixVector *)calloc(dest[s].nPoint,
			sizeof(sixVector));
		memcpy(dest[s].pos, src[s].pos, 
			dest[s].nPoint*sizeof(sixVector));
	}

	return dest;
}

void deleteDifxSpacecraft(DifxSpacecraft *ds, int nSpacecraft)
{
	int s;

	if(ds)
	{
		for(s = 0; s < nSpacecraft; s++)
		{
			if(ds[s].pos)
			{
				free(ds[s].pos);
			}
		}
		free(ds);
	}
}

void printDifxSpacecraft(const DifxSpacecraft *ds)
{
	printf("  DifxSpacecraft : %p\n", ds);
	if(!ds)
	{
		return;
	}
	printf("    Name = %s\n", ds->name);
	printf("    Num points = %d\n", ds->nPoint);
}

static void copySpacecraft(DifxSpacecraft *dest, const DifxSpacecraft *src)
{
	strcpy(dest->name, src->name);
	dest->nPoint = src->nPoint;
	dest->pos = (sixVector *)calloc(dest->nPoint, sizeof(sixVector));
	memcpy(dest->pos, src->pos, dest->nPoint*sizeof(sixVector));
}

static void mergeSpacecraft(DifxSpacecraft *dest, const DifxSpacecraft *src1,
	const DifxSpacecraft *src2)
{
	strcpy(dest->name, src1->name);
	
	/* FIXME -- write me! for now just copy the first one found */
	copySpacecraft(dest, src1);
}

/* note: returns number of spacecraft on call stack */
DifxSpacecraft *mergeDifxSpacecraft(const DifxSpacecraft *ds1, int nds1,
	const DifxSpacecraft *ds2, int nds2, int *spacecraftIdRemap, int *nds)
{
	DifxSpacecraft *ds;
	int i, j;

	if(nds1 <= 0 && nds2 <= 0)
	{
		*nds = 0;
		return 0;
	}

	if(nds2 <= 0)
	{
		*nds = nds1;
		return dupDifxSpacecraftArray(ds1, nds1);
	}

	if(nds1 <= 0)
	{
		*nds = nds2;
		for(i = 0; i < nds2; i++)
		{
			spacecraftIdRemap[i] = i;
		}
		return dupDifxSpacecraftArray(ds2, nds2);
	}

	/* both have spacecraft tables, so do the merge */

	*nds = nds1;

	/* first identify entries that differ and assign new spacecraftIds */
	for(j = 0; j < nds2; j++)
	{
		for(i = 0; i < nds1; i++)
		{
			if(strcmp(ds1[i].name, ds2[j].name) == 0)
			{
				spacecraftIdRemap[j] = i;
				break;
			}
		}
		if(i == nds1)
		{
			spacecraftIdRemap[j] = *nds;
			(*nds)++;
		}
	}

	ds = newDifxSpacecraftArray(*nds);

	for(i = 0; i < nds1; i++)
	{
		/* see if the spacecraft is common to both input tables */
		for(j = 0; j < nds2; j++)
		{
			if(spacecraftIdRemap[j] == i)
			{
				break;
			}
		}
		if(j < nds2)	/* yes -- both have it! */
		{
			mergeSpacecraft(ds + i, ds1 + i, ds2 + j);
		}
		else		/* no -- just in first table */
		{
			copySpacecraft(ds + i, ds1 + i);
		}
	}

	/* finally go through input table 2 and copy unique ones */
	for(j = 0; j < nds2; j++)
	{
		i = spacecraftIdRemap[j];
		if(i >= nds1) /* it is unique to second input */
		{
			copySpacecraft(ds + i, ds2 + j);
		}
	}

	return ds;
}
