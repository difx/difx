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
		ds[s].sourceId = -1;
	}
	
	return ds;
}

void deleteDifxScanInternals(DifxScan *ds)
{
	if(ds->model)
	{
		deleteDifxModelArray(ds->model, ds->nAntenna);
		ds->model = 0;
	}
	if(ds->im)
	{
		deleteDifxPolyModelArray(ds->im, ds->nAntenna);
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
	fprintf(fp, "  DifxScan [%s] : %p\n", ds->name, ds);
	fprintf(fp, "    Start = MJD %12.6f\n", ds->mjdStart);
	fprintf(fp, "    End   = MJD %12.6f\n", ds->mjdEnd);
	fprintf(fp, "    Calcode = %s\n", ds->calCode);
	fprintf(fp, "    Qualifier = %d\n", ds->qual);
	fprintf(fp, "    nPoint = %d\n", ds->nPoint);
	fprintf(fp, "    startPoint = %d\n", ds->startPoint);
	fprintf(fp, "    nAntenna %d\n", ds->nAntenna);
	fprintf(fp, "    SourceId = %d\n", ds->sourceId);
	fprintf(fp, "    ConfigId = %d\n", ds->configId);
	if(ds->nPoint > 1 && ds->nAntenna > 1)
	{
		if(ds->model[0])
		{
			fprintDifxModel(fp, ds->model[0] - 1);
			fprintDifxModel(fp, ds->model[0]);
		}
		if(ds->model[1])
		{
			fprintDifxModel(fp, ds->model[1] - 1);
			fprintDifxModel(fp, ds->model[1]);
		}
	}
}

void printDifxScan(const DifxScan *ds)
{
	fprintDifxScan(stdout, ds);
}

void fprintDifxScanSummary(FILE *fp, const DifxScan *ds)
{
	fprintf(fp, "  Start=%12.6f end=%12.6f source=%s\n", 
		ds->mjdStart, ds->mjdEnd, ds->name);
	fprintf(fp, "    SourceId = %d\n", ds->sourceId);
	fprintf(fp, "    ConfigId = %d\n", ds->configId);
}

void printDifxScanSummary(const DifxScan *ds)
{
	fprintDifxScanSummary(stdout, ds);
}

void copyDifxScan(DifxScan *dest, const DifxScan *src,
	const int *jobIdRemap, const int *configIdRemap)
{
	int i, srcAntenna;

	dest->mjdStart = src->mjdStart;
	dest->mjdEnd   = src->mjdEnd;
	dest->ra       = src->ra;
	dest->dec      = src->dec;
	dest->qual     = src->qual;
	strcpy(dest->name, src->name);
	for(i = 0; i < 4; i++)
	{
		dest->calCode[i] = src->calCode[i];
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
	dest->startPoint = src->startPoint;
	dest->nPoint     = src->nPoint;
	dest->nPoly      = src->nPoly;

	/* figure out how many antennas needed in this scan */
	dest->nAntenna = src->nAntenna;

	/* allocate space for model info and copy from original. */
	if(src->model)
	{
		dest->model = (DifxModel **)calloc(dest->nAntenna, 
			sizeof(DifxModel *));
		for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
		{
			dest->model[srcAntenna] = dupDifxModelColumn(
				src->model[srcAntenna], dest->nPoint);
		}
	}
	else
	{
		dest->model = 0;
	}

	if(src->im)
	{
		dest->im = (DifxPolyModel **)calloc(dest->nAntenna, 
			sizeof(DifxPolyModel *));
		for(srcAntenna = 0; srcAntenna < src->nAntenna; srcAntenna++)
		{
			dest->im[srcAntenna] = dupDifxPolyModelColumn(
				src->im[srcAntenna], dest->nPoly);
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
	const DifxScan *ds2, int nds2, const int *jobIdRemap, 
	const int *configIdRemap, int *nds)
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
				copyDifxScan(ds + i, ds1 + i1, 0, 0);
				i++;
			}
			i1++;
		}
		else
		{
			if(ds2[i2].configId >= 0)
			{
				copyDifxScan(ds + i, ds2 + i2, jobIdRemap, 
					configIdRemap);
				i++;
			}
			i2++;
		}
	}

	*nds = i;

	return ds;
}

/* dt in seconds */
int getDifxScanIMIndex(const DifxScan *ds, double mjd, double *dt)
{
	int i;
	double m1, m2;
	const DifxPolyModel *dp, *im=0;

	if(!ds)
	{
		return -1;
	}
	if(!ds->im || ds->nPoly < 1)
	{
		return -1;
	}

	/* be sure to find an antenna with model data */
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
		dp = im + i;
		m1 = dp->mjd + dp->sec/86400.0;
		m2 = m1 + dp->validDuration/86400.0;
		if(mjd >= m1 && mjd <= m2)
		{
			if(dt)
			{
				*dt = (mjd - m1)*86400.0;
			}
			return i;
		}
	}

	/* outside range */
	return -1;
}

/* dc must point to the base of a configuration array */
int writeDifxScan(FILE *out, const DifxScan *ds, int scanId, 
	const DifxConfig *dc, int doRealName, int doCoords, int doExtra)
{
	int n;
	const char *name;

	ds += scanId;

	if(ds->configId == -1)
	{
		name = "SCAN_GAP";
	}
	else if(ds->configId < 0 || ds->configId > 1<<12)
	{
		fprintf(stderr, "Error!   writeDifxScan:  scanId=%d implies configId=%d\n", scanId, ds->configId);
		return -1;
	}
	else
	{
		const DifxConfig *config;
		
		config = dc + ds->configId;
		name = config->name;
	}

	writeDifxLineInt1(out, "SCAN %d POINTS", scanId, ds->nPoint);
	writeDifxLineInt1(out, "SCAN %d START PT", scanId, ds->startPoint);
	writeDifxLine1(out, "SCAN %d SRC NAME", scanId, name);
	n = 3;
	
	if(doRealName)
	{
		writeDifxLine1(out, "SCAN %d REAL NAME", scanId, ds->name);
		n++;
	}
	
	if(doCoords)
	{
		writeDifxLineDouble1(out, "SCAN %d SRC RA", scanId,
			"%17.15f", ds->ra);
		writeDifxLineDouble1(out, "SCAN %d SRC DEC", scanId,
			"%17.15f", ds->dec);
		n += 2;
	}

	if(doExtra)
	{
		writeDifxLine1(out, "SCAN %d CALCODE", scanId,
			ds->calCode);
		writeDifxLineInt1(out, "SCAN %d QUAL", scanId,
			ds->qual);
		n += 2;
	}

	return n;
}

int writeDifxScanArray(FILE *out, int nScan, const DifxScan *ds, 
	const DifxConfig *dc,  int doRealName, int doCoords, int doExtra)
{
	int i;
	int n;

	writeDifxLineInt(out, "NUM SCANS", nScan);
	n = 1;

	for(i = 0; i < nScan; i++)
	{
		n += writeDifxScan(out, ds, i, dc, 
			doRealName, doCoords, doExtra);
	}

	return 0;
}

/* insert dummy scans to make the series of scans contiguous */
int padDifxScans(DifxInput *D)
{
	int nPad = 0;
	int s, nScan;
	DifxScan *newScans;
	DifxScan *s0, *s1, *sNew;
	int deltat;

	nScan = D->nScan;
	if(nScan <= 0 || D->scan == 0)
	{
		fprintf(stderr, "Error: padDifxScanArrays: nScan = %d / scans = %p\n", nScan, D->scan);
		return -1;
	}

	/* Do nothing if only one scan */
	if(nScan == 1)
	{
		return 0;
	}

	/* first go through and count number of new scans needed */
	for(s = 1; s < nScan; s++)
	{
		s1 = D->scan + s;
		s0 = s1 - 1;

		deltat = s1->startPoint - (s0->startPoint + s0->nPoint);

		/* oops -- scans overlap! */
		if(deltat < 0)
		{
			fprintf(stderr, "Warning: padDifxScanArrays: scans %d and %d overlap!\n", s-1, s);
		}
		else if(deltat > 0)
		{
			nPad++;
		}
	}

	if(nPad == 0)
	{
		return 0;
	}

	newScans = newDifxScanArray(nScan + nPad);

	/* now go through again, inserting spacer scans as needed */
	sNew = newScans;
	copyDifxScan(sNew, D->scan + 0, 0, 0);	/* copy first scan */
	sNew++;
	for(s = 1; s < nScan; s++)
	{
		s1 = D->scan + s;
		s0 = s1 - 1;

		deltat = s1->startPoint - (s0->startPoint + s0->nPoint);
		if(deltat > 0)
		{
			sNew->mjdStart = s0->mjdEnd;
			sNew->mjdEnd = s1->mjdStart;
			sNew->ra = 0.0;
			sNew->dec = 1.57;	/* near north pole */
			strcpy(sNew->name, "SCAN_GAP");
			strcpy(sNew->calCode, "-");
			sNew->sourceId = -1;
			sNew->configId = -1;
			sNew->jobId = s0->jobId;
			sNew->nPoint = deltat;
			sNew->startPoint = s0->startPoint + s0->nPoint;
			/* don't populsate model or im */
			
			sNew++;
		}

		copyDifxScan(sNew, s1, 0, 0);
		sNew++;
	}

	deleteDifxScanArray(D->scan, nScan);
	D->scan = newScans;
	D->nScan += nPad;

	return nPad;
}
