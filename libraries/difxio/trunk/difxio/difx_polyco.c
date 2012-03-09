/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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


DifxPolyco *newDifxPolycoArray(int nPolyco)
{
	DifxPolyco *dp;

	dp = (DifxPolyco *)calloc(nPolyco, sizeof(DifxPolyco));

	return dp;
}

void deleteDifxPolycoInternals(DifxPolyco *dp)
{
	if(dp->coef)
	{
		free(dp->coef);
		dp->coef = 0;
	}
}

void deleteDifxPolycoArray(DifxPolyco *dp, int nPolyco)
{
	if(dp)
	{
		int p;

		for(p = 0; p < nPolyco; ++p)
		{
			deleteDifxPolycoInternals(dp + p);
		}
		free(dp);
	}
}

void fprintDifxPolycoArray(FILE *fp, const DifxPolyco *dp, int nPolyco)
{
	int i;
	
	if(!dp)
	{
		return;
	}
	
	for(i = 0; i < nPolyco; ++i)
	{
		const DifxPolyco *p;
		int c;

		p = dp + i;
		fprintf(fp, "      Polyco file = %s\n", p->fileName);
		fprintf(fp, "      D.M. = %f\n", p->dm);
		fprintf(fp, "      refFreq = %f MHz\n", p->refFreq);
		fprintf(fp, "      mjd mid = %f\n", p->mjd);
		fprintf(fp, "      nBlk = %d\n", p->nBlk);
		fprintf(fp, "      P0 = %f turns\n", p->p0);
		fprintf(fp, "      F0 = %f Hz\n", p->f0);
		fprintf(fp, "      nCoef = %d\n", p->nCoef);
		for(c = 0; c < p->nCoef; ++c)
		{
			fprintf(fp, "        %22.16e\n", p->coef[c]);
		}
	}
}

void printDifxPolycoArray(const DifxPolyco *dp, int nPolyco)
{
	fprintDifxPolycoArray(stdout, dp, nPolyco);
}

int DifxPolycoArrayGetMaxPolyOrder(const DifxPolyco *dp, int nPolyco)
{
	int max = 0;
	int p;

	for(p = 0; p < nPolyco; ++p)
	{
		if(dp[p].nCoef > max)
		{
			max = dp[p].nCoef;
		}
	}

	return max;
}

void copyDifxPolyco(DifxPolyco *dest, const DifxPolyco *src)
{
	snprintf(dest->fileName, DIFXIO_FILENAME_LENGTH, "%s", src->fileName);
	if(dest->coef)
	{
		free(dest->coef);
		dest->coef = 0;
		dest->nCoef = 0;
	}
	if(src->coef)
	{
		int c;

		dest->nCoef = src->nCoef;
		dest->coef = (double *)malloc(dest->nCoef*sizeof(double));
		for(c = 0; c < dest->nCoef; ++c)
		{
			dest->coef[c] = src->coef[c];
		}
	}
	dest->dm = src->dm;
	dest->refFreq = src->refFreq;
	dest->mjd = src->mjd;
	dest->nBlk = src->nBlk;
	dest->p0 = src->p0;
	dest->f0 = src->f0;
}

DifxPolyco *dupDifxPolycoArray(const DifxPolyco *src, int nPolyco)
{
	DifxPolyco *dp;
	int p;

	dp = newDifxPolycoArray(nPolyco);
	for(p = 0; p < nPolyco; ++p)
	{
		copyDifxPolyco(dp + p, src + p);
	}

	return dp;
}

int loadPulsarPolycoFile(DifxPolyco **dpArray, int *nPolyco, const char *filename)
{
	const int BufferSize=160;
	FILE *in;
	
	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open %s for read\n", filename);

		return -1;
	}

	for(;;)
	{	
		DifxPolyco *dp;
		char buffer[BufferSize];
		char *ptr;
		int n, r, c, len;

		ptr = fgets(buffer, BufferSize-1, in);
		if(ptr == 0)
		{
			if(*nPolyco < 1)
			{
				fprintf(stderr, "Early EOF in %s\n", filename);
			}
			fclose(in);
			
			return *nPolyco;
		}

		/* append another entry onto the dpArray */
		*dpArray = (DifxPolyco *)realloc(*dpArray, ((*nPolyco)+1)*sizeof(DifxPolyco));
		dp = (*dpArray) + (*nPolyco);
		dp->nCoef = 0;
		dp->coef = 0;
		snprintf(dp->fileName, DIFXIO_FILENAME_LENGTH, "%s", filename);

		r = sscanf(buffer, "%*s%*s%*f%lf%lf", &dp->mjd, &dp->dm);
		if(r != 2)
		{
			fprintf(stderr, "Error: loadPulsarPolycoFile: cannot parse [%s] from %s\n", buffer, filename);
			fclose(in);
			
			return -1;
		}

		ptr = fgets(buffer, BufferSize-1, in);
		if(ptr == 0)
		{
			fprintf(stderr, "Error: loadPulsarPolycoFile: early end of file in %s\n", filename);
			fclose(in);
			
			return -1;
		}
		r = sscanf(buffer, "%lf%lf%*d%d%d%lf", &dp->p0, &dp->f0, &dp->nBlk, &dp->nCoef, &dp->refFreq);
		if(r != 5)
		{
			fprintf(stderr, "Error: loadPulsarPolycoFile: cannot parse [%s] from %s\n", buffer, filename);
			fclose(in);
			
			return -1;
		}

		if(dp->nCoef < 1 || dp->nCoef > 24)
		{
			fprintf(stderr, "Error: loadPulsarPolycoFile: too many coefs(%d) in file %s\n", dp->nCoef, filename);
			fclose(in);

			return -1;
		}

		dp->coef = (double *)calloc(dp->nCoef, sizeof(double));

		for(c = 0; c < dp->nCoef; ++c)
		{
			n = fscanf(in, "%s", buffer);
			if(n == 0)
			{
				fprintf(stderr, "Error: loadPulsarPolycoFile: early end of file in %s\n", filename);
				fclose(in);
				
				return -1;
			}
			buffer[BufferSize-1] = 0;	/* just to be sure */
			len = strlen(buffer);
			if(buffer[len-4] == 'D')
			{
				buffer[len-4] = 'e';
			}
			dp->coef[c] = atof(buffer);
		}

		/* get the end of line character out of the file */
		ptr = fgets(buffer, BufferSize-1, in);	

		// Correct for "FUT time" 
		if(dp->mjd < 20000.0)
		{
			dp->mjd += 39126;
		}
		++(*nPolyco);
	}
	
	fclose(in);

	return *nPolyco;
}
