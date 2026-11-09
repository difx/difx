/***************************************************************************
 *   Copyright (C) 2008-2026 by Walter Brisken                             *
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
#include <unistd.h>
#include "difxio/difx_input.h"


DifxPolyModel ***newDifxPolyModelArray(unsigned int nAntenna, unsigned int nSrcs, unsigned int nPoly)
{
	DifxPolyModel ***dpm;
	unsigned int a;

	dpm = (DifxPolyModel ***)calloc(nAntenna, sizeof(DifxPolyModel **));
	if(!dpm)
	{
		fprintf(stderr, "Error: newDifxPolyModelArray(): cannot allocate %lu bytes for dpm.\n", nAntenna*sizeof(DifxPolyModel **));

		exit(0);
	}
	for(a = 0; a < nAntenna; ++a)
	{
		unsigned int s;

		dpm[a] = (DifxPolyModel **)calloc(nSrcs, sizeof(DifxPolyModel *));
		if(!dpm[a])
		{
			fprintf(stderr, "Error: newDifxPolyModelArray(): cannot allocate %lu bytes for dpm[%u].\n", nSrcs*sizeof(DifxPolyModel *), a);

			exit(0);
		}
		for(s = 0; s < nSrcs; ++s)
		{
			dpm[a][s] = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));
			if(!dpm[a][s])
			{
				fprintf(stderr, "Error: newDifxPolyModelArray(): cannot allocate %lu bytes for dpm[%u][%u].\n", nSrcs*sizeof(DifxPolyModel), a, s);

				exit(0);
			}
		}
	}

	return dpm;
}

DifxPolyModel *dupDifxPolyModelColumn(const DifxPolyModel *src, unsigned int nPoly)
{
	DifxPolyModel *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));
	if(!dest)
	{
		fprintf(stderr, "Error: dupDifxPolyModelColumn(): cannot allocate %lu bytes.\n", nPoly*sizeof(DifxPolyModel));

		exit(0);
	}

	memcpy(dest, src, nPoly*sizeof(DifxPolyModel));

	return dest;
}

void deleteDifxPolyModelArray(DifxPolyModel ***dpm, unsigned int nAntenna, unsigned int nSrcs)
{
	if(dpm)
	{
		unsigned int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(dpm[a])
			{
				unsigned int s;

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

void fprintDifxPolyModel(FILE *fp, const DifxPolyModel *dpm, unsigned int antennaId, unsigned int sourceId, unsigned int polyId)
{
	fprintf(fp, "    DifxPolyModel [antennaId = %2d sourceId = %2d polyId = %2d]: %p\n", antennaId, sourceId, polyId, dpm);
	if(dpm)
	{
		fprintf(fp, "        mjd, sec = %d, %d\n", dpm->mjd, dpm->sec);
		fprintf(fp, "        delay = %24.16e %24.16e %24.16e\n", dpm->delay[0], dpm->delay[1], dpm->delay[2]);
	}
}

void printDifxPolyModel(const DifxPolyModel *dpm, unsigned int antennaId, unsigned int sourceId, unsigned int polyId)
{
	fprintDifxPolyModel(stdout, dpm, antennaId, sourceId, polyId);
}


DifxPolyModelLMExtension ***newDifxPolyModelLMExtensionArray(unsigned int nAntenna, unsigned int nSrcs, unsigned int nPoly)
{
	DifxPolyModelLMExtension ***lme;
	unsigned int a;

	lme = (DifxPolyModelLMExtension ***)calloc(nAntenna, sizeof(DifxPolyModelLMExtension **));
	if(!lme)
	{
		fprintf(stderr, "Error: newDifxPolyModelLMExtensionArray(): cannot allocate %lu bytes for lme.\n", nAntenna*sizeof(DifxPolyModelLMExtension **));

		exit(0);
	}
	for(a = 0; a < nAntenna; ++a)
	{
		unsigned int s;

		lme[a] = (DifxPolyModelLMExtension **)calloc(nSrcs, sizeof(DifxPolyModelLMExtension *));
		if(!lme[a])
		{
			fprintf(stderr, "Error: newDifxPolyModelLMExtensionArray(): cannot allocate %lu bytes for lme[%u].\n", nAntenna*sizeof(DifxPolyModelLMExtension *), a);

			exit(0);
		}
		for(s = 0; s < nSrcs; ++s)
		{
			lme[a][s] = (DifxPolyModelLMExtension *)calloc(nPoly, sizeof(DifxPolyModelLMExtension));
			if(!lme[a][s])
			{
				fprintf(stderr, "Error: newDifxPolyModelLMExtensionArray(): cannot allocate %lu bytes for lme[%u][%u].\n", nAntenna*sizeof(DifxPolyModelLMExtension), a, s);

				exit(0);
			}
		}
	}

	return lme;
}

DifxPolyModelLMExtension *dupDifxPolyModelLMExtensionColumn(const DifxPolyModelLMExtension *src, unsigned int nPoly)
{
	DifxPolyModelLMExtension *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModelLMExtension *)calloc(nPoly, sizeof(DifxPolyModelLMExtension));
	if(!dest)
	{
		fprintf(stderr, "Error: dupDifxPolyModelLMExtensionColumn(): cannot allocate %lu bytes.\n", nPoly*sizeof(DifxPolyModelLMExtension));

		exit(0);
	}

	memcpy(dest, src, nPoly*sizeof(DifxPolyModelLMExtension));

	return dest;
}

void deleteDifxPolyModelLMExtensionArray(DifxPolyModelLMExtension ***lme, unsigned int nAntenna, unsigned int nSrcs)
{
	if(lme)
	{
		unsigned int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(lme[a])
			{
				unsigned int s;

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



DifxPolyModelXYZExtension ***newDifxPolyModelXYZExtensionArray(unsigned int nAntenna, unsigned int nSrcs, unsigned int nPoly)
{
	DifxPolyModelXYZExtension ***xyze;
	unsigned int a;

	xyze = (DifxPolyModelXYZExtension ***)calloc(nAntenna, sizeof(DifxPolyModelXYZExtension **));
	if(!xyze)
	{
		fprintf(stderr, "Error: newDifxPolyModelXYZExtensionArray(): cannot allocate %lu bytes for xyze.\n", nAntenna*sizeof(DifxPolyModelXYZExtension **));

		exit(0);
	}
	for(a = 0; a < nAntenna; ++a)
	{
		unsigned int s;

		xyze[a] = (DifxPolyModelXYZExtension **)calloc(nSrcs, sizeof(DifxPolyModelXYZExtension *));
		if(!xyze[a])
		{
			fprintf(stderr, "Error: newDifxPolyModelXYZExtensionArray(): cannot allocate %lu bytes for xyze[%u].\n", nAntenna*sizeof(DifxPolyModelXYZExtension *), a);

			exit(0);
		}
		for(s = 0; s < nSrcs; ++s)
		{
			xyze[a][s] = (DifxPolyModelXYZExtension *)calloc(nPoly, sizeof(DifxPolyModelXYZExtension));
			if(!xyze[a][s])
			{
				fprintf(stderr, "Error: newDifxPolyModelXYZExtensionArray(): cannot allocate %lu bytes for xyze[%u][%u].\n", nAntenna*sizeof(DifxPolyModelXYZExtension), a, s);

				exit(0);
			}
		}
	}

	return xyze;
}

DifxPolyModelXYZExtension *dupDifxPolyModelXYZExtensionColumn(const DifxPolyModelXYZExtension *src, unsigned int nPoly)
{
	DifxPolyModelXYZExtension *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModelXYZExtension *)calloc(nPoly, sizeof(DifxPolyModelXYZExtension));
	if(!dest)
	{
		fprintf(stderr, "Error: dupDifxPolyModelXYZExtensionColumn(): cannot allocate %lu bytes.\n", nPoly*sizeof(DifxPolyModelXYZExtension));

		exit(0);
	}

	memcpy(dest, src, nPoly*sizeof(DifxPolyModelXYZExtension));

	return dest;
}

void deleteDifxPolyModelXYZExtensionArray(DifxPolyModelXYZExtension ***xyze, unsigned int nAntenna, unsigned int nSrcs)
{
	if(xyze)
	{
		unsigned int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(xyze[a])
			{
				unsigned int s;

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
static long double evaluatePoly(const double *p, unsigned int n, double x)
{
	long double y;
	unsigned int i;

	if(n == 0)
	{
		return 0;
	}
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

static long double evaluatePolyDeriv(const double *p, unsigned int n, double x)
{
	long double y;
	unsigned int i;

	if(n <= 1)
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
int evaluateDifxInputDelayRate(long double *delay, long double *rate, const DifxInput *D, int intmjd, double sec, unsigned int antennaId, unsigned int sourceId)
{
	unsigned int scanId;
	double mjd;

	mjd = intmjd + sec/86400.0;	/* low precision time for finding scan */

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		const DifxScan *scan;
		unsigned int p;	/* polynomial index in scan */

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
int evaluateDifxInputUVW(double uvw[3], const DifxInput *D, int intmjd, double sec, unsigned int antennaId, unsigned int sourceId)
{
	unsigned int scanId;
	double mjd;

	mjd = intmjd + sec/86400.0;	/* low precision time for finding scan */

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		const DifxScan *scan;
		unsigned int p;	/* polynomial index in scan */

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
