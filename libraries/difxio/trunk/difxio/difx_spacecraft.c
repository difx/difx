/***************************************************************************
 *   Copyright (C) 2008-2015 by Walter Brisken                             *
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
#include "SpiceUsr.h"
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
		dest[s].pos = (sixVector *)calloc(dest[s].nPoint, sizeof(sixVector));
		memcpy(dest[s].pos, src[s].pos, dest[s].nPoint*sizeof(sixVector));
		if(src[s].timeFrameOffset)
		{
			dest[s].timeFrameOffset = (RadioastronTimeFrameOffset *)calloc(dest[s].nPoint, sizeof(RadioastronTimeFrameOffset));
			memcpy(dest[s].timeFrameOffset, src[s].timeFrameOffset, dest[s].nPoint*sizeof(RadioastronTimeFrameOffset));
		}
		if(src[s].axisVectors)
		{
			dest[s].axisVectors = (RadioastronAxisVectors *)calloc(dest[s].nPoint, sizeof(RadioastronAxisVectors));
			memcpy(dest[s].axisVectors, src[s].axisVectors, dest[s].nPoint*sizeof(RadioastronAxisVectors));
		}
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
	if(ds->timeFrameOffset)
	{
		free(ds->timeFrameOffset);
		ds->timeFrameOffset = 0;
	}
	if(ds->axisVectors)
	{
		free(ds->axisVectors);
		ds->axisVectors = 0;
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
	if(ds)
	{
		fprintf(fp, "    Name = %s\n", ds->name);
		fprintf(fp, "    Num points = %d\n", ds->nPoint);
	}
}

void printDifxSpacecraft(const DifxSpacecraft *ds)
{
	fprintDifxSpacecraft(stdout, ds);
}

#if HAVE_SPICE
static void TEME2J2000(double et, double state[6])
{
	double precm[36];
	double invprecm[36];
	double tmpstate[6];
	int six = 6;
	int i;

	/* Rotate from TEME to J2000 frame */
	/* Get rotation matrix from TEME @ET (sec past J2000 epoch) to J2000 */
	/* PRECM is 6x6, goes from J2000 -> TEME */
	zzteme_(&et, precm);
	/* Invert state transformation matrix to go from TEME -> J2000 */
	invstm_(precm, invprecm);
	/* Do transformation of state from EV2LIN's TEME to J2000 */
	mxvg_(invprecm, state, &six, &six, tmpstate);
	for(i = 0; i < 6; ++i)
	{
		state[i] = tmpstate[i];
	}
}
#endif

int computeDifxSpacecraftEphemerisFromXYZ(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, double X, double Y, double Z, const char *naifFile, double ephemClockError)
{
#if HAVE_SPICE
	double state[6];
	int p;

	if(!ds->pos || ds->nPoint == 0)	/* state vector array needs allocating and intializing */
	{
		ds->nPoint = nPoint;
		ds->pos = (sixVector *)calloc(nPoint, sizeof(sixVector));
		
		for(p = 0; p < ds->nPoint; ++p)
		{
			double m = mjd0 + p*deltat;

			ds->pos[p].mjd = m;
			ds->pos[p].fracDay = m - ds->pos[p].mjd;
		}
	}

	state[0] = X/1000.0;
	state[1] = Y/1000.0;
	state[2] = Z/1000.0;
	state[3] = state[4] = state[5] = 0.0;

	if(naifFile)
	{
		ldpool_c(naifFile);
	}

	for(p = 0; p < nPoint; ++p)
	{
		long double jd;
		char jdstr[24];
		double et;

		jd = 2400000.5 + ds->pos[p].mjd + ds->pos[p].fracDay + ephemClockError/86400.0;
		sprintf(jdstr, "JD %18.12Lf", jd);
		str2et_c(jdstr, &et);

		TEME2J2000(et, state);

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
	fprintf(stderr, "Error: computeDifxSpacecraftEphemerisFromXYZ: spice not compiled into difxio.\n");
	
	return -1;
#endif
}

static int computeDifxSpacecraftEphemeris_bsp(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, const char *objectName, const char *naifFile, const char *ephemFile, double ephemStellarAber, double ephemClockError)
{
#if HAVE_SPICE
	int spiceHandle;
	int p;

	if(!ds->pos || ds->nPoint == 0)	/* state vector array needs allocating and intializing */
	{
		ds->nPoint = nPoint;
		ds->pos = (sixVector *)calloc(nPoint, sizeof(sixVector));
		
		for(p = 0; p < ds->nPoint; ++p)
		{
			double m = mjd0 + p*deltat;

			ds->pos[p].mjd = m;
			ds->pos[p].fracDay = m - ds->pos[p].mjd;
		}
	}

	if(naifFile)
	{
		ldpool_c(naifFile);
	}

	spklef_c(ephemFile, &spiceHandle);

	p = snprintf(ds->name, DIFXIO_NAME_LENGTH, "%s", objectName);
	if(p >= DIFXIO_NAME_LENGTH)
	{
		fprintf(stderr, "Warning: computeDifxSpacecraftEphemeris_bsp: spacecraft name %s is too long %d > %d\n", objectName, p, DIFXIO_NAME_LENGTH-1);
	}
	for(p = 0; p < ds->nPoint; ++p)
	{
		double state[6], range;
		long double jd;
		char jdstr[24];
		double et;
		
		/* time to evaluate ephemeris */
		jd = 2400000.5 + ds->pos[p].mjd + ds->pos[p].fracDay + ephemClockError/86400.0;
		sprintf(jdstr, "JD %18.12Lf", jd);
		str2et_c(jdstr, &et);

		/* 399 is the earth geocenter */
		if(ephemStellarAber < -0.5)
		{
			spkezr_c(objectName, et, "J2000", "NONE", "399", state, &range);
		}
		else if(ephemStellarAber == 0.0)
		{
			spkezr_c(objectName, et, "J2000", "LT", "399", state, &range);
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
		
		/* Convert to m and m/s from km and km/s */
		ds->pos[p].X = state[0]*1000.0;
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

#if HAVE_SPICE
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

void evaluateTLE(double et, double *elems, double *state)
{
	double R;
	double geophysConsts[] = 	/* values from http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/ev2lin.html */
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

	if(elems[8] >= 2.0*M_PI/225.0)
	{
		ev2lin_(&et, geophysConsts, elems, state);

		R = sqrt(state[0]*state[0] + state[1]*state[1] + state[2]*state[2]);

		/* Adjust for light travel time from Earth Center to object */
		/* R comes out in km, et is in seconds */
		et -= R/299792.458;

		ev2lin_(&et, geophysConsts, elems, state);
	}
	else
	{
		dpspce_(&et, geophysConsts, elems, state);

		R = sqrt(state[0]*state[0] + state[1]*state[1] + state[2]*state[2]);

		/* Adjust for light travel time from Earth Center to object */
		/* R comes out in km, et is in seconds */
		et -= R/299792.458;

		dpspce_(&et, geophysConsts, elems, state);
	}

}
#endif

static int computeDifxSpacecraftEphemeris_tle(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, const char *objectName, const char *naifFile, const char *ephemFile, double ephemStellarAber, double ephemClockError)
{
#if HAVE_SPICE
	const int NElement = 10;
	const int MaxEphemElementSets = 30;
	const SpiceInt MaxLineLength = 512;
	const SpiceInt firstYear = 2000;
	int p;
	double elems[MaxEphemElementSets][NElement];
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

	if(naifFile)
	{
		ldpool_c(naifFile);
	}

	if(!ds->pos || ds->nPoint == 0)	/* state vector array needs allocating and intializing */
	{
		ds->nPoint = nPoint;
		ds->pos = (sixVector *)calloc(nPoint, sizeof(sixVector));
		
		for(p = 0; p < ds->nPoint; ++p)
		{
			double m = mjd0 + p*deltat;

			ds->pos[p].mjd = m;
			ds->pos[p].fracDay = m - ds->pos[p].mjd;
		}
	}

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

	for(p = 0; p < nPoint; ++p)
	{
		long double jd;
		char jdstr[24];
		double et;
		int set;
		double state[6];
		double f = -1.0;

		/* time to evaluate ephemeris */
		jd = 2400000.5 + ds->pos[p].mjd + ds->pos[p].fracDay + ephemClockError/86400.0;
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

			double state2[6];
			int j;

			evaluateTLE(et, elems[set+1], state2);
			for(j = 0; j < 6; ++j)
			{
				state[j] = f*state[j] + (1.0-f)*state2[j];
			}
		}

		TEME2J2000(et, state);

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
	double end1;
	int j;
	snprintf(dest->name, DIFXIO_NAME_LENGTH, "%s", src1->name);
	
	/* put in time order */
	if(src1->pos->mjd + src1->pos->fracDay > src2->pos->mjd + src2->pos->fracDay)
	{
		const DifxSpacecraft *tmp;

		tmp = src1;
		src1 = src2;
		src2 = tmp;
	}

	end1 = src1->pos[src1->nPoint-1].mjd + src1->pos[src1->nPoint-1].fracDay;
	for(j = 0; j < src2->nPoint; ++j)
	{
		double end;

		end = src2->pos[j].mjd + src2->pos[j].fracDay;

		if(end > end1)
		{
			break;
		}
	}

	dest->nPoint = src1->nPoint + src2->nPoint - j;
	dest->pos = (sixVector *)calloc(dest->nPoint, sizeof(sixVector));
	memcpy(dest->pos, src1->pos, src1->nPoint*sizeof(sixVector));
	if(j < src2->nPoint)
	{
		memcpy(dest->pos + src1->nPoint, src2->pos + j, (src2->nPoint - j)*sizeof(sixVector));
	}
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

	if(t < -0.01 || t > 1.01)
	{
		fprintf(stderr, "WARNING: potentially unhealthy interpolation of state vector occurring mjd=%14.8Lf t0=%14.8Lf t1=%14.8Lf\n", tMod, t0, t1);
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

/* used to generate an MJD with more precision than a single double */
static int snprintIntDouble(char *s, int maxLength, int i, double d)
{
	const int maxDoubleLength=30;
	char ds[maxDoubleLength];

	if(d < 0.0 || d >= 1.0)
	{
		fprintf(stderr, "Warning: preciseprint(%d, %20.18f): precision being lost because double portion is outside [0,1)\n", i, d);
		if(d < 0.0)
		{
			d += (int)d + 1;
		}
		i += (int)d;
		d -= (int)d;
	}
	snprintf(ds, maxDoubleLength, "%20.18f", d);

	return snprintf(s, maxLength, "%d%s", i, ds+1);
}

int writeDifxSpacecraftArray(FILE *out, int nSpacecraft, DifxSpacecraft *ds)
{
	const int MaxLineLength = 256;
	const int MaxMJDStringLength = 40;
	int n;
	int i, j, v;
	char value[MaxLineLength];
	const sixVector *V;
	const RadioastronTimeFrameOffset *TFO;
	const RadioastronAxisVectors *AV;
	char mjdString[MaxMJDStringLength];

	writeDifxLineInt(out, "NUM SPACECRAFT", nSpacecraft);
	n = 1;
	for(i = 0; i < nSpacecraft; ++i)
	{
		if(ds[i].frame[0])
		{
			writeDifxLine(out, "FRAME", ds[i].frame);
		}
		writeDifxLine1(out, "SPACECRAFT %d NAME", i, ds[i].name);
		writeDifxLineInt1(out, "SPACECRAFT %d ROWS", i, ds[i].nPoint);
		if(ds[i].axisVectors == 0 || ds[i].timeFrameOffset == 0)
		{
			for(j = 0; j < ds[i].nPoint; ++j)
			{
				V = ds[i].pos + j;
				snprintIntDouble(mjdString, MaxMJDStringLength, V->mjd, V->fracDay);
				v = snprintf(value, MaxLineLength,
					"%s "
					"%18.14Le %18.14Le %18.14Le "
					"%18.14Le %18.14Le %18.14Le", 
					mjdString, 
					V->X, V->Y, V->Z,
					V->dX, V->dY, V->dZ);
				if(v >= MaxLineLength)
				{
					fprintf(stderr, "Error: Spacecraft %d row %d is too long!\n", i, j);

					return -1;
				}
				writeDifxLine2(out, "SPACECRAFT %d ROW %d", i, j, value);
			}
		}
		else if(ds[i].axisVectors == 0 && ds[i].timeFrameOffset == 0)
		{
			for(j = 0; j < ds[i].nPoint; ++j)
			{
				V = ds[i].pos + j;
				TFO = ds[i].timeFrameOffset + j;
				AV = ds[i].axisVectors + j;
				snprintIntDouble(mjdString, MaxMJDStringLength, V->mjd, V->fracDay);
				v = snprintf(value, MaxLineLength,
					 "%s "
					 "%27.19Le %27.19Le %27.19Le "
					 "%27.19Le %27.19Le %27.19Le "
					 "%24.16e %24.16e "
					 "%24.16e %24.16e %24.16e "
					 "%24.16e %24.16e %24.16e "
					 "%24.16e %24.16e %24.16e", 
					 mjdString,
					 V->X, V->Y, V->Z,
					 V->dX, V->dY, V->dZ,
					 TFO->Delta_t, TFO->dtdtau,
					 AV->X[0], AV->X[1], AV->X[2],
					 AV->Y[0], AV->Y[1], AV->Y[2],
					 AV->Z[0], AV->Z[1], AV->Z[2]);
				if(v >= MaxLineLength)
				{
					fprintf(stderr, "Error: Spacecraft %d row %d is too long!\n", i, j);

					return -1;
				}
				writeDifxLine2(out, "SPACECRAFT %d ROW %d", i, j, value);
			}
		}
		else
		{
			fprintf(stderr, "Error: spacecraft number=%d: axisVectors=%p timeFrameOffset=%p but both must be null or non-null\n", i, ds[i].axisVectors, ds[i].timeFrameOffset);

			exit(EXIT_FAILURE);
		}
		n += (ds[i].nPoint + 2);
	}

	return n;
}

/* properly handles seconds outside range [0,86400) and retains precision */
void sixVectorSetTime(sixVector *v, int mjd, double sec)
{
	if(sec < 0)
	{
		int N = (int)((-sec)/86400.0) + 1;

		sec += N*86400.0;
		mjd -= N;
	}
	if(sec >= 86400.0)
	{
		int N = (int)(sec/86400.0);

		sec -= N*86400.0;
		mjd += N;
	}

	v->mjd = mjd;
	v->fracDay = sec/86400.0;
}

/* puts some parameters into the spice pool.  Call this function instead of
 * supplying a leapsecond kernel file.
 */
int populateSpiceLeapSecondsFromEOP(const DifxEOP *eop, int nEOP)
{
	const double naif_deltet_delta_t_a = 32.184;
	const double naif_deltet_k = 1.657e-3;
	const double naif_deltet_eb = 1.671e-2; 
	const double naif_deltet_m[2] = { 6.23999600, 1.99096871e-7 };
	double *naif_taiutc;
	int e;
	int nLeapSec = 0;
	double lastLeapSec = -1000.0;

#if HAVE_SPICE
	if(nEOP < 1)
	{
		fprintf(stderr, "Error: populateSpiceLeapSecondsFromEOP: too few values supplied (nEOP = %d)\n", nEOP);

		return -2;
	}

	pdpool_c("DELTET/DELTA_T_A", 1, &naif_deltet_delta_t_a);
	pdpool_c("DELTET/K",         1, &naif_deltet_k);
	pdpool_c("DELTET/EB",        1, &naif_deltet_eb);
	pdpool_c("DELTET/M",         2, naif_deltet_m);

	naif_taiutc = (double *)malloc(2*nEOP*sizeof(double));

	for(e = 0; e < nEOP; ++e)
	{
		if(eop[e].tai_utc != lastLeapSec)
		{
			naif_taiutc[2*nLeapSec] = eop[e].tai_utc;
			naif_taiutc[2*nLeapSec+1] = (eop[e].mjd - 51544.5)*86400.0;
			
			lastLeapSec = naif_taiutc[2*nLeapSec];

			++nLeapSec;
		}
	}

	pdpool_c("DELTET/DELTA_AT",  2*nLeapSec, naif_taiutc);

	free(naif_taiutc);

	return 0;
#else
	fprintf(stderr, "Error: populateSpiceLeapSecondsFromEOP: spice not compiled into difxio.\n");
	
	return -1;
#endif
}
