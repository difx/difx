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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "config.h"
#include "difxio/difx_write.h"

/* include spice files for spacecraft navigation if libraries are present */
#if HAVE_SPICE
#include "SpiceCK.h"
#include "SpiceZpr.h"
#include "SpiceZfc.h"
#endif


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

	for(s = 0; s < n; ++s)
	{
		snprintf(dest[s].name, DIFXIO_NAME_LENGTH, "%s", src[s].name);
		dest[s].nPoint = src[s].nPoint;
		dest[s].pos = (sixVector *)calloc(dest[s].nPoint,
			sizeof(sixVector));
		memcpy(dest[s].pos, src[s].pos, 
			dest[s].nPoint*sizeof(sixVector));
	}

	return dest;
}

void deleteDifxSpacecraftInternals(DifxSpacecraft *ds)
{
	if(ds->pos)
	{
		free(ds->pos);
		ds->pos = 0;
	}
	
}

void deleteDifxSpacecraftArray(DifxSpacecraft *ds, int nSpacecraft)
{
	int s;

	if(ds)
	{
		for(s = 0; s < nSpacecraft; ++s)
		{
			deleteDifxSpacecraftInternals(ds + s);
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

static int computeDifxSpacecraftEphemeris_bsp(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, const char *objectName, const char *naifFile, const char *ephemFile, double ephemStellarAber, double ephemClockError)
{
#if HAVE_SPICE
	int spiceHandle;
	int p;

	ldpool_c(naifFile);
	spklef_c(ephemFile, &spiceHandle);

	p = snprintf(ds->name, DIFXIO_NAME_LENGTH, "%s", objectName);
	if(p >= DIFXIO_NAME_LENGTH)
	{
		fprintf(stderr, "Warning: computeDifxSpacecraftEphemeris_bsp: spacecraft name %s is too long %d > %d\n", objectName, p, DIFXIO_NAME_LENGTH-1);
	}
	ds->nPoint = nPoint;
	ds->pos = (sixVector *)calloc(nPoint, sizeof(sixVector));
	for(p = 0; p < nPoint; ++p)
	{
		double state[6], range;
		long double mjd, jd;
		char jdstr[24];
		double et;
		
		mjd = mjd0 + p*deltat;
		jd = mjd + 2400000.5 + ephemClockError/86400.0;
		sprintf(jdstr, "JD %18.12Lf", jd);
		str2et_c(jdstr, &et);
		if(ephemStellarAber == 0.0)
		{
			spkezr_c(objectName, et, "J2000", "LT", "399", state, &range);	/* 399 is the earth geocenter */
		}
		else if(ephemStellarAber == 1.0)
		{
			spkezr_c(objectName, et, "J2000", "LT+S", "399", state, &range);
		}
		else
		{
			double state2[6];
			int q;

			spkezr_c(objectName, et, "J2000", "LT+S", "399", state2, &range);
			spkezr_c(objectName, et, "J2000", "LT", "399", state, &range);

			for(q = 0; q < 6; ++q)
			{
				state[q] += ephemStellarAber*(state2[q] - state[q]);
			}
		}

		ds->pos[p].mjd = mjd;
		ds->pos[p].fracDay = mjd - ds->pos[p].mjd;
		ds->pos[p].X = state[0]*1000.0;	/* Convert to m and m/s from km and km/s */
		ds->pos[p].Y = state[1]*1000.0;
		ds->pos[p].Z = state[2]*1000.0;
		ds->pos[p].dX = state[3]*1000.0;
		ds->pos[p].dY = state[4]*1000.0;
		ds->pos[p].dZ = state[5]*1000.0;
	}

	spkuef_c(spiceHandle);
	clpool_c();

	return 0;
#else
	fprintf(stderr, "Error: computeDifxSpacecraftEphemeris_bsp: spice not compiled into difxio.\n");
	
	return -1;
#endif
}

static int findBestSet(double e, SpiceDouble *epochs, int nEpoch, double *f)
{
	int i;

	if(e <= epochs[0])
	{
		*f = 1.0;

		return 0;
	}
	if(e >= epochs[nEpoch - 1])
	{
		*f = 1.0;

		return nEpoch - 1;
	}

	for(i = 1; i < nEpoch; ++i)
	{
		if(epochs[i] >= e)
		{
			*f = (epochs[i]-e)/(epochs[i]-epochs[i-1]);

			return i - 1;
		}
	}

	return -1;
}

void evaluateTLE(doublereal et, doublereal *elems, doublereal *state)
{
	double R;
	doublereal geophysConsts[] = 	/* values from http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/ev2lin.html */
	{
		1.082616e-3,		/* J2 gravitational harmonic for earth */
		-2.53881e-6,		/* J3 gravitational harmonic for earth */
		-1.65597e-6,		/* J4 gravitational harmonic for earth */
		7.43669161e-2,		/* KE: Square root of the GM for earth where GM is expressed in earth radii cubed per minutes squared. */
		120.0,			/* QO: Low altitude bound for atmospheric model in km. */
		78.0,			/* SO: High altitude bound for atmospheric model in km. */
		6378.135,		/* RE: Equatorial radius of the earth in km. */
		1.0			/* AE: Distance units/earth radius (normally 1) */
	};

	ev2lin_(&et, geophysConsts, elems, state);

	R = sqrt(state[0]*state[0] + state[1]*state[1] + state[2]*state[2]);

	/* Adjust for light travel time from Earth Center to object */
	/* R comes out in km, et is in seconds */
	et -= R/299792.458;

	ev2lin_(&et, geophysConsts, elems, state);
}

static int computeDifxSpacecraftEphemeris_tle(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, const char *objectName, const char *naifFile, const char *ephemFile, double ephemStellarAber, double ephemClockError)
{
#if HAVE_SPICE
	const int NElement = 10;
	const int MaxEphemElementSets = 30;
	const SpiceInt MaxLineLength = 512;
	const SpiceInt firstYear = 2000;
	int p;
	doublereal elems[MaxEphemElementSets][NElement];
	SpiceDouble epochs[MaxEphemElementSets];
	int nSet = 0;
	FILE *in;
	char inLine[MaxLineLength];
	SpiceChar lines[2][MaxLineLength];	/* To store the Two Line Element (TLE) */

	in = fopen(ephemFile, "r");
	if(!in)
	{
		fprintf(stderr, "Error opening file %s\n", ephemFile);

		return -1;
	}

	ldpool_c(naifFile);

	for(;;)
	{
		char *rv;
		int l, i;

		l = strlen(objectName);

		rv = fgets(inLine, MaxLineLength-1, in);
		if(!rv)
		{
			break;
		}
		inLine[MaxLineLength-1] = 0;
		for(i = 0; inLine[i]; ++i)
		{
			if(inLine[i] < ' ')
			{
				inLine[i] = 0;
				break;
			}
		}

		if(inLine[0] == '1')
		{
			strcpy(lines[0], inLine);
		}
		else if(inLine[0] == '2')
		{
			strcpy(lines[1], inLine);

			if(strncmp(lines[0]+2, objectName, l) == 0 && strncmp(lines[1]+2, objectName, l) == 0)
			{
				SpiceDouble elemsTemp[NElement];	/* ensure proper type safety */
				int k;

				getelm_c(firstYear, MaxLineLength, lines, epochs + nSet, elemsTemp);
				for(k = 0; k < NElement; ++k)
				{
					elems[nSet][k] = elemsTemp[k];
				}

				++nSet;
			}
		}
	}

	fclose(in);

	if(nSet == 0)
	{
		fprintf(stderr, "Error: no TLEs were properly parsed from file %s\n", ephemFile);

		return -1;
	}

	p = snprintf(ds->name, DIFXIO_NAME_LENGTH, "%s", objectName);
	if(p >= DIFXIO_NAME_LENGTH)
	{
		fprintf(stderr, "Warning: computeDifxSpacecraftEphemeris_tle: spacecraft name %s is too long %d > %d\n", objectName, p, DIFXIO_NAME_LENGTH-1);
	}
	ds->nPoint = nPoint;
	ds->pos = (sixVector *)calloc(nPoint, sizeof(sixVector));

	for(p = 0; p < nPoint; ++p)
	{
		long double mjd, jd;
		char jdstr[24];
		doublereal et;
		int set;
		doublereal state[6];
		double f = -1.0;

		mjd = mjd0 + p*deltat;
		jd = mjd + 2400000.5 + ephemClockError/86400.0;
		sprintf(jdstr, "JD %18.12Lf", jd);
		str2et_c(jdstr, &et);

		set = findBestSet(et, epochs, nSet, &f);

		if(f < 0.0)
		{
			fprintf(stderr, "Developer error: computeDifxSpacecraftEphemeris_tle: f < 0 (= %f)\n", f);

			exit(EXIT_FAILURE);
		}

		evaluateTLE(et, elems[set], state);

		if(f < 1.0)
		{
			/* linear interpolation between two TLE values */

			doublereal state2[6];
			int j;

			evaluateTLE(et, elems[set+1], state2);
			for(j = 0; j < 6; ++j)
			{
				state[j] = f*state[j] + (1.0-f)*state2[j];
			}
		}

		ds->pos[p].mjd = mjd;
		ds->pos[p].fracDay = mjd - ds->pos[p].mjd;
		ds->pos[p].X = state[0]*1000.0;	/* Convert to m and m/s from km and km/s */
		ds->pos[p].Y = state[1]*1000.0;
		ds->pos[p].Z = state[2]*1000.0;
		ds->pos[p].dX = state[3]*1000.0;
		ds->pos[p].dY = state[4]*1000.0;
		ds->pos[p].dZ = state[5]*1000.0;
	}

	clpool_c();

	return 0;
#else
	fprintf(stderr, "Error: computeDifxSpacecraftEphemeris_tle: spice not compiled into difxio.\n");
	
	return -1;
#endif
}

int computeDifxSpacecraftEphemeris(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, const char *objectName, const char *naifFile, const char *ephemFile, double ephemStellarAber, double ephemClockError)
{
	int l;

	/* TODO: add mechanism to just load in state vectors */

	l = strlen(ephemFile);
	if(l > 4 && strcmp(ephemFile+l-4, ".bsp") == 0)
	{
		return computeDifxSpacecraftEphemeris_bsp(ds, mjd0, deltat, nPoint, objectName, naifFile, ephemFile, ephemStellarAber, ephemClockError);
	}
	else if(l > 4 && strcmp(ephemFile+l-4, ".tle") == 0)
	{
		return computeDifxSpacecraftEphemeris_tle(ds, mjd0, deltat, nPoint, objectName, naifFile, ephemFile, ephemStellarAber, ephemClockError);
	}
	else
	{
		fprintf(stderr, "Error: ephemFile (%s) is not of a recognized ephemeris type.  The file should end in .tle or .bsp .\n", ephemFile);
		
		return -1;
	}
}

static void copySpacecraft(DifxSpacecraft *dest, const DifxSpacecraft *src)
{
	snprintf(dest->name, DIFXIO_NAME_LENGTH, "%s", src->name);
	dest->nPoint = src->nPoint;
	dest->pos = (sixVector *)calloc(dest->nPoint, sizeof(sixVector));
	memcpy(dest->pos, src->pos, dest->nPoint*sizeof(sixVector));
}

static void mergeSpacecraft(DifxSpacecraft *dest, const DifxSpacecraft *src1, const DifxSpacecraft *src2)
{
	snprintf(dest->name, DIFXIO_NAME_LENGTH, "%s", src1->name);
	
#warning "FIXME: write me! for now just copy the first one found"
	copySpacecraft(dest, src1);
}

/* note: returns number of spacecraft on call stack */
DifxSpacecraft *mergeDifxSpacecraft(const DifxSpacecraft *ds1, int nds1, const DifxSpacecraft *ds2, int nds2, int *spacecraftIdRemap, int *nds)
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
		for(i = 0; i < nds2; ++i)
		{
			spacecraftIdRemap[i] = i;
		}
		return dupDifxSpacecraftArray(ds2, nds2);
	}

	/* both have spacecraft tables, so do the merge */

	*nds = nds1;

	/* first identify entries that differ and assign new spacecraftIds */
	for(j = 0; j < nds2; ++j)
	{
		for(i = 0; i < nds1; ++i)
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
			++(*nds);
		}
	}

	ds = newDifxSpacecraftArray(*nds);

	for(i = 0; i < nds1; ++i)
	{
		/* see if the spacecraft is common to both input tables */
		for(j = 0; j < nds2; ++j)
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
	for(j = 0; j < nds2; ++j)
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

int evaluateDifxSpacecraft(const DifxSpacecraft *sc, int mjd, double fracMjd, sixVector *interpolatedPosition)
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
	for(r = 1; r < nRow; ++r)
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

	if(fabs(t) > 0.01 && fabs(t-1) > 0.01)
	{
		fprintf(stderr, "WARNING: potentially unhealthy interpolation of state vector occurring\n");
	}

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

#if 0
	/* linear interpolation of velocity gives smoother results than
	 * evaluating derivative polynomial.  Why??? 
	 */
	dX = pos[r0].dX + t*(pos[r1].dX - pos[r0].dX);
	dY = pos[r0].dY + t*(pos[r1].dY - pos[r0].dY);
	dZ = pos[r0].dZ + t*(pos[r1].dZ - pos[r0].dZ);
#endif

	/* override interpolation with linear... */
	X  = pos[r0].X*(1.0-t) + pos[r1].X*t;
	Y  = pos[r0].Y*(1.0-t) + pos[r1].Y*t;
	Z  = pos[r0].Z*(1.0-t) + pos[r1].Z*t;
	dX = pos[r0].dX*(1.0-t) + pos[r1].dX*t;
	dY = pos[r0].dY*(1.0-t) + pos[r1].dY*t;
	dZ = pos[r0].dZ*(1.0-t) + pos[r1].dZ*t;

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
	const int MaxLineLength = 256;
	int n;
	int i, j, v;
	char value[MaxLineLength];
	const sixVector *V;
	long double mjd;

	writeDifxLineInt(out, "NUM SPACECRAFT", nSpacecraft);
	n = 1;
	for(i = 0; i < nSpacecraft; ++i)
	{
		writeDifxLine1(out, "SPACECRAFT %d NAME", i, ds[i].name);
		writeDifxLineInt1(out, "SPACECRAFT %d ROWS", i, ds[i].nPoint);
		for(j = 0; j < ds[i].nPoint; ++j)
		{
			V = ds[i].pos + j;
			mjd = V->mjd + V->fracDay;
			v = snprintf(value, MaxLineLength,
				"%17.12Lf "
				"%18.14Le %18.14Le %18.14Le "
				"%18.14Le %18.14Le %18.14Le", 
				mjd, 
				V->X, V->Y, V->Z,
				V->dX, V->dY, V->dZ);
			if(v >= MaxLineLength)
			{
				fprintf(stderr, "Error: Spacecraft %d row %d is too long!\n", i, j);

				return -1;
			}
			writeDifxLine2(out, "SPACECRAFT %d ROW %d", i, j, value);
		}
		n += (ds[i].nPoint + 2);
	}

	return n;
}
