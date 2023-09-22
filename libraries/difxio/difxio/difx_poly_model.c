/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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
#include "difxio/difx_input.h"


DifxPolyModel ***newDifxPolyModelArray(int nAntenna, int nSrcs, int nPoly)
{
	DifxPolyModel ***dpm;
	int a;

	dpm = (DifxPolyModel ***)calloc(nAntenna, sizeof(DifxPolyModel **));
	for(a = 0; a < nAntenna; ++a)
	{
		int s;

		dpm[a] = (DifxPolyModel **)calloc(nSrcs, sizeof(DifxPolyModel *));
		for(s = 0; s < nSrcs; ++s)
		{
			dpm[a][s] = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));
		}
	}

	return dpm;
}

DifxPolyModel *dupDifxPolyModelColumn(const DifxPolyModel *src, int nPoly)
{
	DifxPolyModel *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));

	memcpy(dest, src, nPoly*sizeof(DifxPolyModel));

	return dest;
}

void deleteDifxPolyModelArray(DifxPolyModel ***dpm, int nAntenna, int nSrcs)
{
	if(dpm)
	{
		int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(dpm[a])
			{
				int s;

				for(s = 0; s < nSrcs; ++s)
				{
					free(dpm[a][s]);
				}
				free(dpm[a]);
			}
		}
		free(dpm);
	}
}

void fprintDifxPolyModel(FILE *fp, const DifxPolyModel *dpm, int antennaId, int sourceId, int polyId)
{
	fprintf(fp, "    DifxPolyModel [antennaId = %2d sourceId = %2d polyId = %2d]: %p\n", antennaId, sourceId, polyId, dpm);
	if(dpm)
	{
		fprintf(fp, "        mjd, sec = %d, %d\n", dpm->mjd, dpm->sec);
		fprintf(fp, "        delay = %24.16e %24.16e %24.16e\n", dpm->delay[0], dpm->delay[1], dpm->delay[2]);
	}
}

void printDifxPolyModel(const DifxPolyModel *dpm, int antennaId, int sourceId, int polyId)
{
	fprintDifxPolyModel(stdout, dpm, antennaId, sourceId, polyId);
}


DifxPolyModelLMExtension ***newDifxPolyModelLMExtensionArray(int nAntenna, int nSrcs, int nPoly)
{
	DifxPolyModelLMExtension ***lme;
	int a;

	lme = (DifxPolyModelLMExtension ***)calloc(nAntenna, sizeof(DifxPolyModelLMExtension **));
	for(a = 0; a < nAntenna; ++a)
	{
		int s;

		lme[a] = (DifxPolyModelLMExtension **)calloc(nSrcs, sizeof(DifxPolyModelLMExtension *));
		for(s = 0; s < nSrcs; ++s)
		{
			lme[a][s] = (DifxPolyModelLMExtension *)calloc(nPoly, sizeof(DifxPolyModelLMExtension));
		}
	}

	return lme;
}

DifxPolyModelLMExtension *dupDifxPolyModelLMExtensionColumn(const DifxPolyModelLMExtension *src, int nPoly)
{
	DifxPolyModelLMExtension *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModelLMExtension *)calloc(nPoly, sizeof(DifxPolyModelLMExtension));

	memcpy(dest, src, nPoly*sizeof(DifxPolyModelLMExtension));

	return dest;
}

void deleteDifxPolyModelLMExtensionArray(DifxPolyModelLMExtension ***lme, int nAntenna, int nSrcs)
{
	if(lme)
	{
		int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(lme[a])
			{
				int s;

				for(s = 0; s < nSrcs; ++s)
				{
					free(lme[a][s]);
				}
				free(lme[a]);
			}
		}
		free(lme);
	}
}



DifxPolyModelXYZExtension ***newDifxPolyModelXYZExtensionArray(int nAntenna, int nSrcs, int nPoly)
{
	DifxPolyModelXYZExtension ***xyze;
	int a;

	xyze = (DifxPolyModelXYZExtension ***)calloc(nAntenna, sizeof(DifxPolyModelXYZExtension **));
	for(a = 0; a < nAntenna; ++a)
	{
		int s;

		xyze[a] = (DifxPolyModelXYZExtension **)calloc(nSrcs, sizeof(DifxPolyModelXYZExtension *));
		for(s = 0; s < nSrcs; ++s)
		{
			xyze[a][s] = (DifxPolyModelXYZExtension *)calloc(nPoly, sizeof(DifxPolyModelXYZExtension));
		}
	}

	return xyze;
}

DifxPolyModelXYZExtension *dupDifxPolyModelXYZExtensionColumn(const DifxPolyModelXYZExtension *src, int nPoly)
{
	DifxPolyModelXYZExtension *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModelXYZExtension *)calloc(nPoly, sizeof(DifxPolyModelXYZExtension));

	memcpy(dest, src, nPoly*sizeof(DifxPolyModelXYZExtension));

	return dest;
}

void deleteDifxPolyModelXYZExtensionArray(DifxPolyModelXYZExtension ***xyze, int nAntenna, int nSrcs)
{
	if(xyze)
	{
		int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(xyze[a])
			{
				int s;

				for(s = 0; s < nSrcs; ++s)
				{
					free(xyze[a][s]);
				}
				free(xyze[a]);
			}
		}
		free(xyze);
	}
}

/* Use Cramer's rule to evaluate polynomial */
static long double evaluatePoly(const double *p, int n, double x)
{
	long double y;
	int i;

	if(n == 1)
	{
		return p[0];
	}

	y = p[n-1];

	for(i = n-2; i >= 0; --i)
	{
		y = x*y + p[i];
	}

	return y;
}

static long double evaluatePolyDeriv(const double *p, int n, double x)
{
	long double y;
	int i;

	if(n == 1)
	{
		return 0;
	}

	if(n == 2)
	{
		return p[1];
	}

	y = (n-1)*p[n-1];

	for(i = n-2; i >= 1; --i)
	{
		y = x*y + i*p[i];
	}

	return y;
}

/* outputs delay in us and rate in us/s */
/* set sourceId = -1 to not select on source */
/* return value = 0 on success, < 0 on error */
int evaluateDifxInputDelayRate(long double *delay, long double *rate, const DifxInput *D, int intmjd, double sec, int antennaId, int sourceId)
{
	int scanId;
	double mjd;

	mjd = intmjd + sec/86400.0;	/* low precision time for finding scan */

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		const DifxScan *scan;
		int p;	/* polynomial index in scan */

		scan = D->scan + scanId;

		if(scan->mjdStart > mjd || scan->mjdEnd < mjd)
		{
			continue;
		}

		if(sourceId >= 0 && sourceId != scan->pointingCentreSrc)
		{
			continue;
		}

		if(antennaId >= scan->nAntenna || scan->im[antennaId] == 0)
		{
			continue;
		}

		for(p = 0; p < scan->nPoly; ++p)
		{
			const DifxPolyModel *P;
			double mjdStart, mjdEnd;
			double deltaT;

			P = scan->im[antennaId][0] + p;
			mjdStart = P->mjd + P->sec/86400.0;
			mjdEnd = mjdStart + P->validDuration/86400.0;

			if(mjd < mjdStart || mjd > mjdEnd)
			{
				continue;
			}

			deltaT = 86400*(intmjd - P->mjd) + (sec - P->sec);

			if(delay)
			{
				*delay = evaluatePoly(P->delay, P->order+1, deltaT);
			}
			if(rate)
			{
				*rate = evaluatePolyDeriv(P->delay, P->order+1, deltaT);
			}

			return 0;
		}
	}

	return -1;
}

/* outputs UVW coords in meters */
/* set sourceId = -1 to not select on source */
/* return value = 0 on success, < 0 on error */
int evaluateDifxInputUVW(double uvw[3], const DifxInput *D, int intmjd, double sec, int antennaId, int sourceId)
{
	int scanId;
	double mjd;

	mjd = intmjd + sec/86400.0;	/* low precision time for finding scan */

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		const DifxScan *scan;
		int p;	/* polynomial index in scan */

		scan = D->scan + scanId;

		if(scan->mjdStart > mjd || scan->mjdEnd < mjd)
		{
			continue;
		}

		if(sourceId >= 0 && sourceId != scan->pointingCentreSrc)
		{
			continue;
		}

		if(antennaId >= scan->nAntenna || scan->im[antennaId] == 0)
		{
			continue;
		}

		for(p = 0; p < scan->nPoly; ++p)
		{
			const DifxPolyModel *P;
			double mjdStart, mjdEnd;
			double deltaT;

			P = scan->im[antennaId][0] + p;
			mjdStart = P->mjd + P->sec/86400.0;
			mjdEnd = mjdStart + P->validDuration/86400.0;

			if(mjd < mjdStart || mjd > mjdEnd)
			{
				continue;
			}

			deltaT = 86400*(intmjd - P->mjd) + (sec - P->sec);

			uvw[0] = evaluatePoly(P->u, P->order+1, deltaT);
			uvw[1] = evaluatePoly(P->v, P->order+1, deltaT);
			uvw[2] = evaluatePoly(P->w, P->order+1, deltaT);

			return 0;
		}
	}

	return -1;
}
