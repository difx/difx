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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_write.h"


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

void fprintDifxSpacecraft(FILE *fp, const DifxSpacecraft *ds)
{
	fprintf(fp, "  DifxSpacecraft : %p\n", ds);
	if(!ds)
	{
		return;
	}
	fprintf(fp, "    Name = %s\n", ds->name);
	fprintf(fp, "    Num points = %d\n", ds->nPoint);
}

void printDifxSpacecraft(const DifxSpacecraft *ds)
{
	fprintDifxSpacecraft(stdout, ds);
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


static void evalPoly(long double poly[4], long double t, long double *V)
{
	*V = poly[0] + t*(poly[1] + t*(poly[2] + t*poly[3]));
}

int evaluateDifxSpacecraft(const DifxSpacecraft *sc, int mjd, double fracMjd,
	sixVector *interpolatedPosition)
{
	int nRow;
	const sixVector *pos;
	long double t0, t1, tMod, t, deltat;
	long double xPoly[4], yPoly[4], zPoly[4];
	int r, r0, r1;
	long double X, Y, Z, dX, dY, dZ;
	
	nRow = sc->nPoint;
	pos = sc->pos;
	
	tMod = mjd + fracMjd;
	
	/* first find interpolation points */
	t0 = 0.0;
	t1 = pos[0].mjd + pos[0].fracDay;
	for(r = 1; r < nRow; r++)
	{
		t0 = t1;
		t1 = pos[r].mjd + pos[r].fracDay;
		if(t0 <= tMod && tMod <= t1)
		{
			break;
		}
	}
	if(r == nRow)
	{
		return -1;
	}

	/* calculate polynomial for X, Y, Z */
	r0 = r-1;
	r1 = r;
	deltat = t1 - t0;
	t = (tMod - t0)/deltat; /* time, fraction of interval, between 0 and 1 */

	xPoly[0] = pos[r0].X;
	xPoly[1] = pos[r0].dX*deltat;
	xPoly[2] = -3.0L*(pos[r0].X-pos[r1].X) - (2.0L*pos[r0].dX+pos[r1].dX)*deltat;
	xPoly[3] =  2.0L*(pos[r0].X-pos[r1].X) + (    pos[r0].dX+pos[r1].dX)*deltat;
	yPoly[0] = pos[r0].Y;
	yPoly[1] = pos[r0].dY*deltat;
	yPoly[2] = -3.0L*(pos[r0].Y-pos[r1].Y) - (2.0L*pos[r0].dY+pos[r1].dY)*deltat;
	yPoly[3] =  2.0L*(pos[r0].Y-pos[r1].Y) + (    pos[r0].dY+pos[r1].dY)*deltat;
	zPoly[0] = pos[r0].Z;
	zPoly[1] = pos[r0].dZ*deltat;
	zPoly[2] = -3.0L*(pos[r0].Z-pos[r1].Z) - (2.0L*pos[r0].dZ+pos[r1].dZ)*deltat;
	zPoly[3] =  2.0L*(pos[r0].Z-pos[r1].Z) + (    pos[r0].dZ+pos[r1].dZ)*deltat;

	evalPoly(xPoly, t, &X);
	evalPoly(yPoly, t, &Y);
	evalPoly(zPoly, t, &Z);

	/* linear interpolation of velocity gives smoother results than
	 * evaluating derivative polynomial.  Why??? 
	 */
	dX = pos[r0].dX + t*(pos[r1].dX - pos[r0].dX);
	dY = pos[r0].dY + t*(pos[r1].dY - pos[r0].dY);
	dZ = pos[r0].dZ + t*(pos[r1].dZ - pos[r0].dZ);

	interpolatedPosition->mjd = mjd;
	interpolatedPosition->fracDay = fracMjd;
	interpolatedPosition->X = X;
	interpolatedPosition->Y = Y;
	interpolatedPosition->Z = Z;
	interpolatedPosition->dX = dX;
	interpolatedPosition->dY = dY;
	interpolatedPosition->dZ = dZ;

	return r;
}

int writeDifxSpacecraftArray(FILE *out, int nSpacecraft, DifxSpacecraft *ds)
{
	int n;
	int i, j;
	char value[256];
	const sixVector *V;

	writeDifxLineInt(out, "NUM SPACECRAFT", nSpacecraft);
	n = 1;
	if(nSpacecraft > 0) for(i = 0; i < nSpacecraft; i++)
	{
		writeDifxLine1(out, "SPACECRAFT %d NAME", i, ds[i].name);
		writeDifxLineInt1(out, "SPACECRAFT %d ROWS", i, ds[i].nPoint);
		for(j = 0; j < ds[i].nPoint; j++)
		{
			V = ds[i].pos + j;
			sprintf(value, "%13.8f %18.14Le %18.14Le %18.14Le "
					"%18.14Le %18.14Le %18.14Le", 
				V->mjd + V->fracDay, 
				V->X, V->Y, V->Z,
				V->dX, V->dY, V->dZ);
			writeDifxLine2(out, "SPACECRAFT %d ROW %d", 
				i, j, value);
		}
		n += (ds[i].nPoint + 2);

		return -1;
	}

	return n;
}
