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


DifxBaseline *newDifxBaselineArray(int nBaseline)
{
	DifxBaseline *db;

	db = (DifxBaseline *)calloc(nBaseline, sizeof(DifxBaseline));

	return db;
}

void deleteDifxBaselineArray(DifxBaseline *db, int nBaseline)
{
	int b, f;
	
	if(db)
	{
		for(b = 0; b < nBaseline; b++)
		{
			if(db[b].nPolProd)
			{
				free(db[b].nPolProd);
			}
			if(db[b].recChanA)
			{
				for(f = 0; f < db[b].nFreq; f++)
				{
					if(db[b].recChanA[f])
					{
						free(db[b].recChanA[f]);
					}
				}
				free(db[b].recChanA);
			}
			if(db[b].recChanB)
			{
				for(f = 0; f < db[b].nFreq; f++)
				{
					if(db[b].recChanB[f])
					{
						free(db[b].recChanB[f]);
					}
				}
				free(db[b].recChanB);
			}
		}
		free(db);
	}
}

void fprintDifxBaseline(FILE *fp, const DifxBaseline *db)
{
	int f;
	
	fprintf(fp, "  Difx Baseline : %p\n", db);
	fprintf(fp, "    datastream indices = %d %d\n", db->dsA, db->dsB);
	fprintf(fp, "    nFreq = %d\n", db->nFreq);
	if(db->nPolProd)
	{
		fprintf(fp, "    nPolProd[freq] =");
		for(f = 0; f < db->nFreq; f++)
		{
			fprintf(fp, " %d", db->nPolProd[f]);
		}
		fprintf(fp, "\n");
	}
}

void printDifxBaseline(const DifxBaseline *db)
{
	fprintDifxBaseline(stdout, db);
}

int isSameDifxBaseline(const DifxBaseline *db1, const DifxBaseline *db2,
	const int *datastreamIdRemap)
{
	int f, p;
	int ds2A, ds2B;

	if(datastreamIdRemap)
	{
		ds2A = datastreamIdRemap[db2->dsA];
		ds2B = datastreamIdRemap[db2->dsB];
	}
	else
	{
		ds2A = db2->dsA;
		ds2B = db2->dsB;
	}

	if(db1->dsA   != ds2A || 
	   db1->dsB   != ds2B ||
	   db1->nFreq != db2->nFreq)
	{
		return 0;
	}
	for(f = 0; f < db1->nFreq; f++)
	{
		if(db1->nPolProd[f] != db2->nPolProd[f])
		{
			return 0;
		}
		for(p = 0; p < db1->nPolProd[f]; p++)
		{
			if(db1->recChanA[f][p] != db2->recChanA[f][p] ||
			   db1->recChanB[f][p] != db2->recChanB[f][p])
			{
				return 0;
			}
		}
	}
	
	return 1;
}

void copyDifxBaseline(DifxBaseline *dest, const DifxBaseline *src,
	const int *datastreamIdRemap)
{
	int f, p, nProd;

	if(datastreamIdRemap)
	{
		dest->dsA   = datastreamIdRemap[src->dsA];
		dest->dsB   = datastreamIdRemap[src->dsB];
	}
	else
	{
		dest->dsA = src->dsA;
		dest->dsB = src->dsB;
	}
	dest->nFreq = src->nFreq;
	dest->nPolProd = (int *)calloc(dest->nFreq, sizeof(int));
	dest->recChanA = (int **)calloc(dest->nFreq, sizeof(int *));
	dest->recChanB = (int **)calloc(dest->nFreq, sizeof(int *));
	for(f = 0; f < dest->nFreq; f++)
	{
		dest->nPolProd[f] = src->nPolProd[f];
		nProd = dest->nPolProd[f];
		dest->recChanA[f] = (int *)calloc(nProd, sizeof(int));
		dest->recChanB[f] = (int *)calloc(nProd, sizeof(int));
		for(p = 0; p < nProd; p++)
		{
			dest->recChanA[f][p] = src->recChanA[f][p];
			dest->recChanB[f][p] = src->recChanB[f][p];
		}
	}
}

DifxBaseline *mergeDifxBaselineArrays(const DifxBaseline *db1, int ndb1,
	const DifxBaseline *db2, int ndb2, int *baselineIdRemap,
	const int *datastreamIdRemap, int *ndb)
{
	int i, j;
	DifxBaseline *db;

	*ndb = ndb1;

	/* first identify entries that differ and assign new baselineIds */
	for(j = 0; j < ndb2; j++)
	{
		for(i = 0; i < ndb1; i++)
		{
			if(isSameDifxBaseline(db1 + i, db2 + j,
				datastreamIdRemap))
			{
				baselineIdRemap[j] = i;
				break;
			}
		}
		if(i == ndb1)
		{
			baselineIdRemap[j] = *ndb;
			(*ndb)++;
		}
	}

	db = newDifxBaselineArray(*ndb);
	
	/* now copy db1 */
	for(i = 0; i < ndb1; i++)
	{
		copyDifxBaseline(db + i, db1 + i, 0);
	}

	/* now copy unique members of db2 */
	for(j = 0; j < ndb2; j++)
	{
		if(baselineIdRemap[j] >= ndb1)
		{
			copyDifxBaseline(db + baselineIdRemap[j], db2 + j,
				datastreamIdRemap);
		}
	}

	return db;
}

int writeDifxBaselineArray(FILE *out, int nBaseline, const DifxBaseline *db)
{
	int i, f, p;
	int n;
	const DifxBaseline *b;

	writeDifxLineInt(out, "BASELINE ENTRIES", nBaseline);
	n = 1;

	for(i = 0; i < nBaseline; i++)
	{
		b = db + i;
		writeDifxLineInt1(out, "D/STREAM A INDEX %d", i, b->dsA);
		writeDifxLineInt1(out, "D/STREAM B INDEX %d", i, b->dsB);
		writeDifxLineInt1(out, "NUM FREQS %d", i, b->nFreq);
		n += 3;

		for(f = 0; f < b->nFreq; f++)
		{
			writeDifxLineInt2(out, "POL PRODUCTS %d/%d", i, f,
				b->nPolProd[f]);
			n++;
			for(p = 0; p < b->nPolProd[f]; p++)
			{
				writeDifxLineInt1(out, "D/STREAM A BAND %d", p,
					b->recChanA[f][p]);
				writeDifxLineInt1(out, "D/STREAM B BAND %d", p,
					b->recChanB[f][p]);
				n += 2;
			}
		}
	}

	return n;
}
