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
#include "difxio/difx_input.h"
#include "difxio/difx_write.h"

const char clockMergeModeNames[][MAX_CLOCK_MERGE_MODE_STRING_LENGTH] =
{
        "strict",
        "loose",
        "illegal"
};

/* These names must match what calcserver expects */
const char antennaMountTypeNames[][MAX_ANTENNA_MOUNT_NAME_LENGTH] =
{
	"AZEL",
	"EQUA",
	"SPACE",	/* spacecraft */
	"XYEW",
	"NASR",		/* note: this will correctly fall back to AZEL in calcserver */
	"NASL",		/* note: this will correctly fall back to AZEL in calcserver */
	"XYNS",		/* note: no FITS-IDI support */
	"OTHER"		/* don't expect the right parallactic angle or delay model! */
};

/* These names must match what VEX expects */
const char antennaSiteTypeNames[][MAX_ANTENNA_SITE_NAME_LENGTH] =
{
	"fixed",
	"earth_orbit",
	"OTHER"
};

enum ClockMergeMode stringToClockMergeMode(const char *str)
{
	enum ClockMergeMode e;

	for(e = 0; e < NumClockMergeModes; ++e)
	{
		if(strcasecmp(str, clockMergeModeNames[e]) == 0)
		{
        		break;
		}
	}
	return e;
}

enum AntennaMountType stringToMountType(const char *str)
{
	if(strcasecmp(str, "AZEL") == 0 ||
	   strcasecmp(str, "ALTZ") == 0 ||
	   strcasecmp(str, "ALTAZ") == 0)
	{
		return AntennaMountAltAz;
	}
	if(strcasecmp(str, "EQUA") == 0 ||
	   strcasecmp(str, "EQUAT") == 0 ||
	   strcasecmp(str, "EQUATORIAL") == 0 ||
	   strcasecmp(str, "HADEC") == 0)
	{
		return AntennaMountEquatorial;
	}
	if(strcasecmp(str, "SPACE") == 0 ||
	   strcasecmp(str, "SPAC") == 0 ||
	   strcasecmp(str, "ORBIT") == 0 ||
	   strcasecmp(str, "ORBI") == 0 ||
	   strcasecmp(str, "ORBITING") == 0)
	{
		return AntennaMountOrbiting;
	}
	if(strcasecmp(str, "XYEW") == 0)
	{
		return AntennaMountXYEW;
	}
	if(strcasecmp(str, "NASMYTH-R") == 0 ||
	   strcasecmp(str, "NASMYTHR") == 0 ||
	   strcasecmp(str, "NASR") == 0)
	{
		return AntennaMountNasmythR;
	}
	if(strcasecmp(str, "NASMYTH-L") == 0 ||
	   strcasecmp(str, "NASMYTHL") == 0 ||
	   strcasecmp(str, "NASL") == 0)
	{
		return AntennaMountNasmythL;
	}
	if(strcasecmp(str, "XYNS") == 0)
	{
		return AntennaMountXYNS;
	}
	
	return AntennaMountOther;
	
}

enum AntennaSiteType stringToSiteType(const char *str)
{
	enum AntennaSiteType t;

	for(t = 0; t < NumAntennaSiteTypes; ++t)
	{
		if(strcasecmp(str, antennaSiteTypeNames[t]) == 0)
		{
			break;
		}
	}

	return t;
}

DifxAntenna *newDifxAntennaArray(int nAntenna)
{
	DifxAntenna* da;
	int a, i;

	da = (DifxAntenna *)calloc(nAntenna, sizeof(DifxAntenna));
	for(a = 0; a < nAntenna; ++a)
	{
		da[a].spacecraftId = -1;
		for(i = 0;  i <MAX_MODEL_ORDER; ++i)
		{
			da[a].clockcoeff[i] = 0.0;
		}
	}
	
	return da;
}

void deleteDifxAntennaArray(DifxAntenna *da, int nAntenna)
{
	if(nAntenna > 0 && da != 0)
	{
		free(da);
	}
}

void fprintDifxAntenna(FILE *fp, const DifxAntenna *da)
{
	int i;

	fprintf(fp, "  DifxAntenna [%s] : %p\n", da->name, da);
	fprintf(fp, "    Clock reference MJD = %f\n", da->clockrefmjd);
	for(i = 0; i <= da->clockorder; ++i)
	{
		fprintf(fp, "    Clock coeff[%d] = %e us/s^%d\n", i, da->clockcoeff[i], i);
	}
	fprintf(fp, "    Mount = %d = %s\n", da->mount, antennaMountTypeNames[da->mount]);
        fprintf(fp, "    Site Type = %d = %s\n", da->siteType, antennaSiteTypeNames[da->siteType]);
	fprintf(fp, "    Offset = %f, %f, %f m\n", da->offset[0], da->offset[1], da->offset[2]);
	fprintf(fp, "    X, Y, Z = %f, %f, %f m\n", da->X, da->Y, da->Z);
	fprintf(fp, "    SpacecraftId = %d\n", da->spacecraftId);
}

void printDifxAntenna(const DifxAntenna *da)
{
	fprintDifxAntenna(stdout, da);
}

void fprintDifxAntennaSummary(FILE *fp, const DifxAntenna *da)
{
	fprintf(fp, "  %s\n", da->name);
	fprintf(fp, "    Clock: Ref time %f, Order = %d, linear approx %e us + %e us/s\n", 
		da->clockrefmjd, da->clockorder, da->clockcoeff[0], da->clockcoeff[1]);
	fprintf(fp, "    Mount = %s\n", antennaMountTypeNames[da->mount]);
        fprintf(fp, "    Site Type = %d = %s\n", da->siteType, antennaSiteTypeNames[da->siteType]);
	fprintf(fp, "    Offset = %f, %f, %f m\n", 
		da->offset[0], da->offset[1], da->offset[2]);
	fprintf(fp, "    X, Y, Z = %f, %f, %f m\n", da->X, da->Y, da->Z);
	if(da->spacecraftId >= 0)
	{
		fprintf(fp, "    SpacecraftId = %d\n", da->spacecraftId);
	}
}

void printDifxAntennaSummary(const DifxAntenna *da)
{
	fprintDifxAntennaSummary(stdout, da);
}

int isSameDifxAntenna(const DifxAntenna *da1, const DifxAntenna *da2)
{
	if((da1->spacecraftId == da2->spacecraftId) &&
	   strcmp(da1->name, da2->name) == 0 &&
	   fabs(da1->X - da2->X) < 1.0 &&
	   fabs(da1->Y - da2->Y) < 1.0 &&
	   fabs(da1->Z - da2->Z) < 1.0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/* FIXME: I think the logic here may be a bit corrupt.  Low danger as truely identical clocks will still pass. */
int isSameDifxAntennaClock(const DifxAntenna *da1, const DifxAntenna *da2)
{
	int i;
	double deltad, epochdiff, dt;

	if(da1->clockorder != da2->clockorder)
	{
		return 0;
	}
	for(i = 1; i <= da1->clockorder; ++i)
	{
		if(da1->clockcoeff[i] != da2->clockcoeff[i])
		{
			return 0;
		}
	}

	epochdiff = (da2->clockrefmjd - da1->clockrefmjd)*86400.0;
	dt = 1.0;
	deltad = 0.0;
	for(i = 0; i <= da1->clockorder; ++i)
	{
		deltad += (da1->clockcoeff[i] - da2->clockcoeff[i])*dt;
		dt *= epochdiff;
	}
	
	if(fabs(deltad) > 1.0e-10)	/* give a 10^-16 sec tolerance. */
	{
		return 0;
	}

	return 1;
}

void copyDifxAntenna(DifxAntenna *dest, const DifxAntenna *src)
{
	if(dest != src)
	{
		if(dest == 0 || src == 0)
		{
			fprintf(stderr, "Developer error: copyDifxAntenna: dest=%p src=%p.  Both must be non-null\n", dest, src);
		}
		else
		{
			/* Warning: if dynamic objects are added to DifxAntenna then this needs to be changed */
			*dest = *src;
		}
	}
	else
	{
		fprintf(stderr, "Developer error: copyDifxAntenna: src = dest.  Bad things will be coming...\n");
	}
}

/* dt is in seconds */
/* returns number of coefficients copied = order+1, or < 0 on error */
int getDifxAntennaShiftedClock(const DifxAntenna *da, double dt, int outputClockSize, double *clockOut)
{
	int i;
	double a[MAX_MODEL_ORDER+1];
	double t2, t3, t4, t5;	/* units: sec^n, n = 2, 3, 4, 5 */

	if(!da)
	{
		return -1;
	}
    
	if(outputClockSize < da->clockorder+1)
	{
		return -2;
	}

	for(i = 0; i < MAX_MODEL_ORDER+1; ++i)             // pad out input array to full order with 0's
	{
		a[i] = (i <= da->clockorder) ? da->clockcoeff[i] : 0.0;
	}

	t2 = dt * dt;
	t3 = t2 * dt;
	t4 = t2 * t2;
	t5 = t3 * t2;

	switch(da->clockorder)
	{
		case 5: clockOut[5] = a[5];
		case 4: clockOut[4] = a[4] + 5 * a[5] * dt;
		case 3: clockOut[3] = a[3] + 4 * a[4] * dt + 10 * a[5] * t2;
		case 2: clockOut[2] = a[2] + 3 * a[3] * dt +  6 * a[4] * t2 + 10 * a[5] * t3;
		case 1: clockOut[1] = a[1] + 2 * a[2] * dt +  3 * a[3] * t2 +  4 * a[4] * t3 + 5 * a[5] * t4;
		case 0: clockOut[0] = a[0] +     a[1] * dt +      a[2] * t2 +      a[3] * t3 +     a[4] * t4 + a[5] * t5; 
	}

	return da->clockorder + 1;
}

/* returns antenna's clock model in microseconds */
double evaluateDifxAntennaClock(const DifxAntenna *da, double mjd)
{
	double dt;	/* [sec] time since the clock epoch */
	double C;
	double dtn;
	int i;

	if(!da)
	{
		fprintf(stderr, "Error: evaluateDifxAntennaClock called with null DifxAntenna pointer\n");

		exit(EXIT_FAILURE);
	}

	dt = (mjd - da->clockrefmjd)*86400.0;

	dtn = 1.0;
	C = 0.0;
	for(i = 0; i <= da->clockorder; ++i)
	{
		C += da->clockcoeff[i]*dtn;
		dtn *= dt;
	}

	return C;
}

/* return antenna's clock rate, in microsec per sec */
double evaluateDifxAntennaClockRate(const DifxAntenna *da, double mjd)
{
	double dt;	/* [sec] time since the clock epoch */
	double C;
	double dtn;
	int i;

	if(!da)
	{
		fprintf(stderr, "Error: evaluateDifxAntennaClockRate called with null DifxAntenna pointer\n");

		exit(EXIT_FAILURE);
	}

	dt = (mjd - da->clockrefmjd)*86400.0;

	dtn = 1.0;
	C = 0.0;
	for(i = 1; i <= da->clockorder; ++i)
	{
		C += i*da->clockcoeff[i]*dtn;
		dtn *= dt;
	}

	return C;
}

DifxAntenna *mergeDifxAntennaArrays(const DifxAntenna *da1, int nda1, const DifxAntenna *da2, int nda2, int *antennaIdRemap, int *nda)
{
	int i, j;
	DifxAntenna *da;

	*nda = nda1;

	/* first identify entries that differ and assign new antennaIds */
	for(j = 0; j < nda2; ++j)
	{
		for(i = 0; i < nda1; ++i)
		{
			if(isSameDifxAntenna(da1 + i, da2 + j))
			{
				antennaIdRemap[j] = i;
				break;
			}
		}
		if(i == nda1)
		{
			antennaIdRemap[j] = *nda;
			(*nda)++;
		}
	}

	da = newDifxAntennaArray(*nda);
	
	/* now copy da1 */
	for(i = 0; i < nda1; ++i)
	{
		copyDifxAntenna(da + i, da1 + i);
	}

	/* now copy unique members of da2 */
	for(j = 0; j < nda2; ++j)
	{
		if(antennaIdRemap[j] >= nda1)
		{
			copyDifxAntenna(da + antennaIdRemap[j], da2 + j);
		}
	}

	return da;
}

int writeDifxAntennaArray(FILE *out, int nAntenna, const DifxAntenna *da, int doMount, int doOffset, int doCoords, int doClock, int doShelf, int doSpacecraft)
{
	int n;	/* number of lines written */
	int i, j;

	if(doClock)
	{
		writeDifxLineInt(out, "TELESCOPE ENTRIES", nAntenna);
	}
	else
	{
		writeDifxLineInt(out, "NUM TELESCOPES", nAntenna);
	}
	n = 1;

	for(i = 0; i < nAntenna; ++i)
	{
		if(doClock)
		{
			writeDifxLine1(out, "TELESCOPE NAME %d", i, da[i].name);
		}
		else
		{
			writeDifxLine1(out, "TELESCOPE %d NAME", i, da[i].name);
		}
		++n;
		if(doMount)
		{
			writeDifxLine1(out, "TELESCOPE %d MOUNT", i, antennaMountTypeNames[da[i].mount]);
			++n;
		}
		if(doOffset)
		{
			writeDifxLineDouble1(out, "TELESCOPE %d OFFSET (m)", i, "%8.6f", da[i].offset[0]);
			++n;
		}
		if(doCoords)
		{
			writeDifxLineDouble1(out, "TELESCOPE %d X (m)", i, "%8.6f", da[i].X);
			writeDifxLineDouble1(out, "TELESCOPE %d Y (m)", i, "%8.6f", da[i].Y);
			writeDifxLineDouble1(out, "TELESCOPE %d Z (m)", i, "%8.6f", da[i].Z);
			n += 3;
		}
		if(doClock)
		{
			writeDifxLineDouble1(out, "CLOCK REF MJD %d", i, "%15.10f", da[i].clockrefmjd);
			writeDifxLineInt1(out, "CLOCK POLY ORDER %d", i, da[i].clockorder);
			writeDifxLine(out, "@ ***** Clock poly coeff N", " has units microsec / sec^N ***** @");
			for(j = 0; j <= da[i].clockorder; ++j)
			{
				writeDifxLineDouble2(out, "CLOCK COEFF %d/%d", i, j, "%17.15e", da[i].clockcoeff[j]);
			}
			n += 3+da[i].clockorder+1;
		}
		if(doShelf)
		{
			writeDifxLine1(out, "TELESCOPE %d SHELF", i, da[i].shelf);
			++n;
		}
#ifdef RA_MERGED
		if(doSpacecraft)
		{
			writeDifxLineInt1(out, "TELESCOPE %d S/CRAFT ID", i, da[i].spacecraftId);
			++n;
		}
#endif
	}

	return n;
}
