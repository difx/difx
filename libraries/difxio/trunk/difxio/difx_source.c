/***************************************************************************
 *   Copyright (C) 2007-2011 by Walter Brisken & Adam Deller               *
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
#include "difxio/parsedifx.h"
#include "difxio/difx_write.h"

DifxSource *newDifxSourceArray(int nSource)
{
	DifxSource *ds;
	int s;

	ds = (DifxSource *)calloc(nSource, sizeof(DifxSource));
	for(s = 0; s < nSource; s++)
	{
		ds[s].spacecraftId = -1;
		ds[s].numFitsSourceIds = 0;
		ds[s].fitsSourceIds = 0;
		ds[s].pmRA = 0.0;
		ds[s].pmDec = 0.0;
		ds[s].parallax = 0.0;
		ds[s].pmEpoch = 0.0;
		ds[s].qual = 0;
		ds[s].calCode[0] = 0;
		ds[s].name[0] = 0;
	}
	
	return ds;
}

void deleteDifxSourceArray(DifxSource *ds, int nSource)
{
	int i;
	if(ds)
	{
		for(i=0;i<nSource;i++)
		{
			if(ds[i].numFitsSourceIds > 0)
			{
				free(ds[i].fitsSourceIds);
			}
		}
		free(ds);
	}
}

void fprintDifxSource(FILE *fp, const DifxSource *ds)
{
	int i;
	fprintf(fp, "  DifxSource [%s] : %p\n", ds->name, ds);
	fprintf(fp, "    RA  =  %10.7f\n", ds->ra);
	fprintf(fp, "    Dec = %+11.7f\n", ds->dec);
	fprintf(fp, "    Calcode = %s\n", ds->calCode);
	fprintf(fp, "    Qualifier = %d\n", ds->qual);
	fprintf(fp, "    SpacecraftId = %d\n", ds->spacecraftId);
	fprintf(fp, "    Num FITS SourceIds = %d\n", ds->numFitsSourceIds);
	for(i=0;i<ds->numFitsSourceIds;i++)
	{
		fprintf(fp, "    FITS SourceId[%d] = %d\n", i, ds->fitsSourceIds[i]);
	}
}

void printDifxSource(const DifxSource *ds)
{
	fprintDifxSource(stdout, ds);
}

void fprintDifxSourceSummary(FILE *fp, const DifxSource *ds)
{
	fprintf(fp, "  %s\n", ds->name);
	fprintf(fp, "    RA  =  %10.7f\n", ds->ra);
	fprintf(fp, "    Dec = %+11.7f\n", ds->dec);
	fprintf(fp, "    Calcode = %s\n", ds->calCode);
	fprintf(fp, "    Qualifier = %d\n", ds->qual);
	if(ds->spacecraftId >= 0)
	{
		fprintf(fp, "    SpacecraftId = %d\n", ds->spacecraftId);
	}
}

void printDifxSourceSummary(const DifxSource *ds)
{
	fprintDifxSourceSummary(stdout, ds);
}

int isSameDifxSource(const DifxSource *ds1, const DifxSource *ds2)
{
	int i;
	//printf("About to do the source compare on two sources with values %p, %p\n", ds1, ds2);
	//printf("Comparing source called %s with %s\n", ds1->name, ds2->name);
        if(strcmp(ds1->name, ds2->name) == 0          &&
           ds1->ra               == ds2->ra           &&
           ds1->dec              == ds2->dec          &&
           strcmp(ds1->calCode,ds2->calCode) == 0     &&
           ds1->qual             == ds2->qual         &&
           ds1->spacecraftId     == ds2->spacecraftId &&
           ds1->numFitsSourceIds == ds2->numFitsSourceIds &&
	   ds1->pmRA             == ds2->pmRA         &&
	   ds1->pmDec            == ds2->pmDec        &&
	   ds1->parallax         == ds2->parallax     &&
	   ds1->pmEpoch          == ds2->pmEpoch)
        {
		for(i=0;i<ds1->numFitsSourceIds;i++)
		{
			if(ds1->fitsSourceIds[i] != ds2->fitsSourceIds[i])
			{
				return 0;
			}
		}
                return 1;
        }
        else
        {
                return 0;
        }
}

void copyDifxSource(DifxSource *dest, const DifxSource *src)
{
	int i;
        dest->ra           = src->ra;
        dest->dec          = src->dec;
        dest->qual         = src->qual;
        dest->spacecraftId = src->spacecraftId;
        dest->numFitsSourceIds = src->numFitsSourceIds;
        dest->pmRA         = src->pmRA;
	dest->pmDec        = src->pmDec;
	dest->parallax     = src->parallax;
	dest->pmEpoch      = src->pmEpoch;
	if(src->numFitsSourceIds > 0)
	{
		dest->fitsSourceIds = (int*)malloc(src->numFitsSourceIds * sizeof(int));
		for(i=0;i<src->numFitsSourceIds;i++)
		{
			dest->fitsSourceIds[i] = src->fitsSourceIds[i];
		}
	}
	snprintf(dest->calCode, DIFXIO_CALCODE_LENGTH, "%s", src->calCode);
	snprintf(dest->name, DIFXIO_NAME_LENGTH, "%s", src->name);
}

/* merge two DifxSource tables into an new one.  sourceIdRemap will contain the
 * mapping from ds2's old source entries to that of the merged set
 */
DifxSource *mergeDifxSourceArrays(const DifxSource *ds1, int nds1,
        const DifxSource *ds2, int nds2, int *sourceIdRemap,
        int *nds)
{
        DifxSource *ds;
        int i, j;

        *nds = nds1;

        /* first identify entries that differ and assign new sourceIds to them */
        for(j = 0; j < nds2; j++)
        {
                for(i = 0; i < nds1; i++)
                {
                        if(isSameDifxSource(ds1 + i, ds2 + j))
                        {
                                sourceIdRemap[j] = i;
                                break;
                        }
                }
                if(i == nds1)
                {
                        sourceIdRemap[j] = *nds;
                        (*nds)++;
                }
        }

        /* Allocate and copy */
        ds = newDifxSourceArray(*nds);
        for(i = 0; i < nds1; i++)
        {
                copyDifxSource(ds + i, ds1 + i);
        }
        for(j = 0; j < nds2; j++)
        {
                i = sourceIdRemap[j];
                if(i >= nds1)
                {
                        copyDifxSource(ds + i, ds2 + j);
                }
        }

        return ds;
}


int writeDifxSourceArray(FILE *out, int nSource, const DifxSource *ds,
        int doCalcode, int doQual, int doSpacecraftID)
{
        int n;  /* number of lines written */
        int i;

        writeDifxLineInt(out, "NUM SOURCES", nSource);
        n = 1;

        for(i = 0; i < nSource; i++)
        {
                writeDifxLine1(out, "SOURCE %d NAME", i, ds[i].name);
                n++;
		writeDifxLineDouble1(out, "SOURCE %d RA", i, "%17.15f", ds[i].ra);
                n++;
                writeDifxLineDouble1(out, "SOURCE %d DEC", i, "%17.15f",  ds[i].dec);
                n++;
		if(doCalcode) 
		{
			writeDifxLine1(out, "SOURCE %d CALCODE", i, ds[i].calCode);
			n++;
		}
		if(doQual)
		{
			writeDifxLineInt1(out, "SOURCE %d QUAL", i, ds[i].qual);
	                n++;
		}
                if(doSpacecraftID)
		{
                        writeDifxLineInt1(out, "SOURCE %d S/CRAFT ID", i, ds[i].spacecraftId);
                        n++;
                }
        }

        return n;
}
