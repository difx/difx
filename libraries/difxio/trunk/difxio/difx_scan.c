/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & Adam Deller               *
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
#include "difxio/difx_write.h"


DifxScan *newDifxScanArray(int nScan)
{
	DifxScan *ds;
	int s;

	ds = (DifxScan *)calloc(nScan, sizeof(DifxScan));
	for(s = 0; s < nScan; s++)
	{
		ds[s].configId = -1;
		ds[s].pointingCentreSrc = -1;
		ds[s].nPhaseCentres = 0;
		ds[s].maxNSBetweenUVShifts = 2000000000;
		ds[s].maxNSBetweenACAvg = 2000000000;
	}
	
	return ds;
}

void deleteDifxScanInternals(DifxScan *ds)
{
	if(ds->im)
	{
		deleteDifxPolyModelArray(ds->im, ds->nAntenna, ds->nPhaseCentres+1);
		ds->im = 0;
	}
}

void deleteDifxScanArray(DifxScan *ds, int nScan)
{
	int s;

	if(ds)
	{
		for(s = 0; s < nScan; s++)
		{
			deleteDifxScanInternals(ds + s);
		}
		free(ds);
	}
}

void fprintDifxScan(FILE *fp, const DifxScan *ds)
{
	int i, j, nModel;

	fprintf(fp, "  DifxScan [%s] : %p\n", ds->identifier, ds);
	fprintf(fp, "    Start = MJD %12.6f\n", ds->mjdStart);
	fprintf(fp, "    End   = MJD %12.6f\n", ds->mjdEnd);
	fprintf(fp, "    Observing mode = %s\n", ds->obsModeName);
	fprintf(fp, "    Max NS between UV shifts = %d\n", ds->maxNSBetweenUVShifts);
	fprintf(fp, "    Max NS between AC averages = %d\n", ds->maxNSBetweenACAvg);
	fprintf(fp, "    Pointing centre source index = %d\n", ds->pointingCentreSrc);
        fprintf(fp, "    Number of phase centres = %d\n", ds->nPhaseCentres);
	for(i = 0; i < ds->nPhaseCentres; i++) 
	{
	        fprintf(fp, "    Phase centre %d source index = %d\n", 
			i, ds->phsCentreSrcs[i]);
		fprintf(fp, "    Original job phase centre %d source index = %d\n",
			i, ds->orgjobPhsCentreSrcs[i]);
	}
	fprintf(fp, "    nAntenna %d\n", ds->nAntenna);

	fprintf(fp, "    ConfigId = %d\n", ds->configId);

	if(ds->nPhaseCentres < 1 || ds->pointingCentreSrc == ds->phsCentreSrcs[0])
	{
		nModel = ds->nPhaseCentres;
	}
	else
	{
		nModel = ds->nPhaseCentres + 1;
	}

	if(ds->nPoly > 0 && ds->nAntenna > 1)
	{
		if(ds->im)
		{
			for(i = 0; i < ds->nAntenna; i++)
			{
				if(ds->im[i])
				{
					for(j = 0; j < nModel; j++)
					{
						fprintDifxPolyModel(fp, ds->im[i][j]);
					}
				}
				else
				{
					fprintf(fp, "    No polymodel[%d]\n", i);
				}
			}
		}
		else
		{
			fprintf(fp, "    No polynomial model available\n");
		}
	}
}

void printDifxScan(const DifxScan *ds)
{
	fprintDifxScan(stdout, ds);
}

void fprintDifxScanSummary(FILE *fp, const DifxScan *ds)
{
	fprintf(fp, "  Start=%12.6f end=%12.6f identifier=%s\n", 
		ds->mjdStart, ds->mjdEnd, ds->identifier);
	fprintf(fp, "    PointingSourceIndex = %d\n", ds->pointingCentreSrc);
	fprintf(fp, "    ConfigId = %d\n", ds->configId);
}

void printDifxScanSummary(const DifxScan *ds)
{
	fprintDifxScanSummary(stdout, ds);
}

void copyDifxScan(DifxScan *dest, const DifxScan *src,
	const int *sourceIdRemap, const int *jobIdRemap, 
	const int *configIdRemap, const int *antennaIdRemap)
{
	int i, j, srcAntenna, destAntenna;

	dest->mjdStart     = src->mjdStart;
	dest->mjdEnd       = src->mjdEnd;
	dest->startSeconds = src->startSeconds;
        dest->durSeconds   = src->durSeconds;
	dest->maxNSBetweenUVShifts = src->maxNSBetweenUVShifts;
	dest->maxNSBetweenACAvg = src->maxNSBetweenACAvg;
	snprintf(dest->identifier, DIFXIO_NAME_LENGTH, "%s", src->identifier);
	snprintf(dest->obsModeName, DIFXIO_NAME_LENGTH, "%s", src->obsModeName);
	dest->nPhaseCentres = src->nPhaseCentres;
	if(sourceIdRemap)
	{
//		printf("About to do the source re-mapping\n");
//		printf("Source pointing centre was %d\n", src->pointingCentreSrc);
//		printf("This will be re-mapped to %d\n", sourceIdRemap[src->pointingCentreSrc]);
		dest->pointingCentreSrc = sourceIdRemap[src->pointingCentreSrc];
		for(i=0;i<src->nPhaseCentres;i++)
		{
			dest->phsCentreSrcs[i] = sourceIdRemap[src->phsCentreSrcs[i]];
			dest->orgjobPhsCentreSrcs[i] = src->orgjobPhsCentreSrcs[i];
		}
//		printf("Done with source re-mapping\n");
	}
	else
	{
		dest->pointingCentreSrc = src->pointingCentreSrc;
		for(i = 0; i < src->nPhaseCentres; i++)
		{
			dest->phsCentreSrcs[i] = src->phsCentreSrcs[i];
			dest->orgjobPhsCentreSrcs[i] = src->orgjobPhsCentreSrcs[i];
		}
	}
	if(jobIdRemap)
	{
		dest->jobId = jobIdRemap[src->jobId];
	}
	else
	{
		dest->jobId = src->jobId;
	}
	if(configIdRemap && src->configId >= 0)
	{
		dest->configId = configIdRemap[src->configId];
	}
	else
	{
		dest->configId = src->configId;
	}
	dest->startSeconds = src->startSeconds;
	dest->durSeconds   = src->durSeconds;
	dest->nPoly        = src->nPoly;

	/* figure out how many antennas needed in this scan */
	dest->nAntenna = src->nAntenna;
	if(antennaIdRemap)
	{
		for(i = 0; i < src->nAntenna; i++)
		{
			destAntenna = antennaIdRemap[i];
			if(destAntenna >= dest->nAntenna)
			{
				dest->nAntenna = destAntenna+1;
			}
		}
	}

	if(src->im)
	{
		dest->im = (DifxPolyModel ***)calloc(dest->nAntenna, 
			sizeof(DifxPolyModel **));
		for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
		{
			if(antennaIdRemap)
			{
				destAntenna = antennaIdRemap[srcAntenna];
			}
			else
			{
				destAntenna = srcAntenna;
			}
			dest->im[destAntenna] = (DifxPolyModel **)calloc(dest->nPhaseCentres+1,
							sizeof(DifxPolyModel *));
			if(dest->im[destAntenna] == 0)
			{
				fprintf(stderr, "Error allocating space for IM table! Aborting");

				exit(1);
			}
			if(src->im[srcAntenna] == 0)
			{
				dest->im[destAntenna] = 0;
				continue; //must have had a change of num antennas at some stage
				//fprintf(stderr, "Missing info for antenna %d\n", srcAntenna);
				//exit(1);
			}
			for(j = 0; j < src->nPhaseCentres+1; j++)
			{
				dest->im[destAntenna][j] = dupDifxPolyModelColumn(
						src->im[srcAntenna][j], dest->nPoly);
				if(dest->im[destAntenna][j] == 0)
				{
					fprintf(stderr, "Error allocating space for IM table! Aborting");

					exit(1);
				}
			}
		}
	}
	else
	{
		dest->im = 0;
	}
}

/* Merge sort the two lists of scans.  This is intended to allow merging of
 * more than two DifxInputs in any order.
 */
DifxScan *mergeDifxScanArrays(const DifxScan *ds1, int nds1,
	const DifxScan *ds2, int nds2, const int *sourceIdRemap, 
	const int *jobIdRemap, const int *configIdRemap, 
	const int *antennaIdRemap, int *nds)
{
	DifxScan *ds;
	int i=0, i1=0, i2=0, src;

	*nds = nds1 + nds2;
	ds = newDifxScanArray(*nds);

	for(;;)
	{
		if(i1 >= nds1)
		{
			i1 = -1;
		}
		if(i2 >= nds2)
		{
			i2 = -1;
		}
		if(i1 < 0 && i2 < 0)
		{
			break;
		}

		/* determine which ScanArray to take from */
		if(i1 == -1)
		{
			src = 2;
		}
		else if(i2 == -1)
		{
			src = 1;
		}
		else if(ds1[i1].mjdStart <= ds2[i2].mjdStart)
		{
			src = 1;
		}
		else
		{
			src = 2;
		}

		/* do the copy and increments */
		if(src == 1)
		{
			if(ds1[i1].configId >= 0)
			{
				copyDifxScan(ds + i, ds1 + i1, 0, 0, 0, 0);
				i++;
			}
			i1++;
		}
		else
		{
			if(ds2[i2].configId >= 0)
			{
				copyDifxScan(ds + i, ds2 + i2, sourceIdRemap,
					jobIdRemap, configIdRemap, antennaIdRemap);
				i++;
			}
			i2++;
		}
	}

	*nds = i;

/*
	printf("After merging two inputs, which had nScan %d and %d, the result has nScan %d\n", nds1, nds2, *nds);
*/

	return ds;
}

/* dt in seconds; mjd and iat both in days */
int getDifxScanIMIndex(const DifxScan *ds, double mjd, double iat, double *dt)
{
	int i;
	double m1, m2;
	DifxPolyModel *dp;
	DifxPolyModel **im=0;

	if(!ds)
	{
		return -1;
	}
	if(!ds->im || ds->nPoly < 1)
	{
		return -1;
	}

	/* be sure to find an antenna with im model data */
	for(i = 0; i < ds->nAntenna; i++)
	{
		im = ds->im[i];
		if(im)
		{
			break;
		}
	}

	if(!im)
	{
		return -1;
	}

	for(i = 0; i < ds->nPoly; i++)
	{
		dp = im[0] + i;
		m1 = (dp->mjd - mjd) + dp->sec/86400.0;
		m2 = m1 + dp->validDuration/86400.0;
		if(m1 <= iat && iat <= m2)
		{
			if(dt)
			{
				*dt = (iat - m1)*86400.0;
			}
			return i;
		}
	}

	/* outside range */
	return -1;
}

/* dc must point to the base of a configuration array */
int writeDifxScan(FILE *out, const DifxScan *ds, int scanId, const DifxConfig *dc)
{
	int i;

	ds += scanId;

        writeDifxLine1(out, "SCAN %d IDENTIFIER", scanId, ds->identifier);
        writeDifxLineInt1(out, "SCAN %d START (S)", scanId, ds->startSeconds);
	writeDifxLineInt1(out, "SCAN %d DUR (S)", scanId, ds->durSeconds);
        writeDifxLine1(out, "SCAN %d OBS MODE NAME", scanId, ds->obsModeName);
	writeDifxLineInt1(out, "SCAN %d UVSHIFT INTERVAL (NS)", scanId, ds->maxNSBetweenUVShifts);
	writeDifxLineInt1(out, "SCAN %d AC AVG INTERVAL (NS)", scanId, ds->maxNSBetweenACAvg);
	writeDifxLineInt1(out, "SCAN %d POINTING SRC", scanId, ds->pointingCentreSrc);
        writeDifxLineInt1(out, "SCAN %d NUM PHS CTRS", scanId, ds->nPhaseCentres);
	for(i = 0; i < ds->nPhaseCentres; i++)
	{
		writeDifxLineInt2(out, "SCAN %d PHS CTR %d", scanId, i, ds->phsCentreSrcs[i]);
	}

	return 7+ds->nPhaseCentres;
}

int writeDifxScanArray(FILE *out, int nScan, const DifxScan *ds, const DifxConfig *dc)
{
	int i, n;

	writeDifxLineInt(out, "NUM SCANS", nScan);
	n = 1;

	for(i = 0; i < nScan; i++)
	{
		n += writeDifxScan(out, ds, i, dc);
	}

	return n;
}
