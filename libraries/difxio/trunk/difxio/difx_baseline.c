/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken                             *
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

void DifxBaselineAllocFreqs(DifxBaseline *b, int nFreq)
{
	int i;

	if(!b)
	{
		fprintf(stderr, "Error: DifxBaselineAllocFreqs: b = 0\n");
		return;
	}
	if(b->nPolProd)
	{
		free(b->nPolProd);
	}
	if(b->bandA)
	{
		for(i = 0; i < b->nFreq; i++)
		{
			if(b->bandA[i])
			{
				free(b->bandA[i]);
			}
		}
		free(b->bandA);
	}
	if(b->bandB)
	{
		for(i = 0; i < b->nFreq; i++)
		{
			if(b->bandB[i])
			{
				free(b->bandB[i]);
			}
		}
		free(b->bandB);
	}

	b->nFreq = nFreq;
	b->nPolProd = (int *)calloc(nFreq, sizeof(int));
	b->bandA = (int **)calloc(nFreq, sizeof(int *));
	b->bandB = (int **)calloc(nFreq, sizeof(int *));
}

void DifxBaselineAllocPolProds(DifxBaseline *b, int freq, int nPol)
{
	if(!b)
	{
		fprintf(stderr, "Error: DifxBaselineAllocPolProds: b = 0\n");
		return;
	}
	if(freq < 0 || freq >= b->nFreq)
	{
		fprintf(stderr, "Error: DifxBaselineAllocPolProds: "
			"freq=%d outside range=%d\n", freq, b->nFreq);
		return;
	}

	if(!b->bandA || !b->bandB || !b->nPolProd)
	{
		fprintf(stderr, "Error: DifxBaselineAllocPolProds: "
			"bandA or bandB or nPolProd is zero\n");
		return;
	}

	if(b->bandA[freq])
	{
		free(b->bandA[freq]);
	}
	if(b->bandB[freq])
	{
		free(b->bandB[freq]);
	}

	b->nPolProd[freq] = nPol;
	if(nPol > 0)
	{
		b->bandA[freq] = (int *)calloc(nPol, sizeof(int));
		b->bandB[freq] = (int *)calloc(nPol, sizeof(int));
	}
	else
	{
		b->bandA[freq] = 0;
		b->bandB[freq] = 0;
	}
}

void deleteDifxBaselineInternals(DifxBaseline *db)
{
	int f;

	if(db->nPolProd)
	{
		free(db->nPolProd);
		db->nPolProd = 0;
	}
	if(db->bandA)
	{
		for(f = 0; f < db->nFreq; f++)
		{
			if(db->bandA[f])
			{
				free(db->bandA[f]);
			}
		}
		free(db->bandA);
		db->bandA = 0;
	}
	if(db->bandB)
	{
		for(f = 0; f < db->nFreq; f++)
		{
			if(db->bandB[f])
			{
				free(db->bandB[f]);
			}
		}
		free(db->bandB);
		db->bandB = 0;
	}
}

void deleteDifxBaselineArray(DifxBaseline *db, int nBaseline)
{
	if(db)
	{
		int b;

		for(b = 0; b < nBaseline; ++b)
		{
			deleteDifxBaselineInternals(db + b);
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
			if(db1->bandA[f][p] != db2->bandA[f][p] ||
			   db1->bandB[f][p] != db2->bandB[f][p])
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
	int f, p;

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

	DifxBaselineAllocFreqs(dest, src->nFreq);
	for(f = 0; f < dest->nFreq; f++)
	{
		DifxBaselineAllocPolProds(dest, f, src->nPolProd[f]);
		for(p = 0; p < dest->nPolProd[f]; p++)
		{
			dest->bandA[f][p] = src->bandA[f][p];
			dest->bandB[f][p] = src->bandB[f][p];
		}
	}
}

void moveDifxBaseline(DifxBaseline *dest, DifxBaseline *src)
{
	dest->dsA = src->dsA;
	dest->dsB = src->dsB;
	dest->nPolProd = src->nPolProd;
	dest->bandA = src->bandA;
	dest->bandB = src->bandB;

	/* "unlink" internal structures */
	src->nPolProd = 0;
	src->bandA = 0;
	src->bandB = 0;
}

int simplifyDifxBaselines(DifxInput *D)
{
	int n0;
	int b, b1;
	int b0;
	int c, cb;

	n0 = D->nBaseline;
	if(n0 < 2)
	{
		return 0;
	}

	for(b = 1;;)
	{
		if(b >= D->nBaseline)
		{
			break;
		}

		for(b1 = 0; b1 < b; b1++)
		{
			if(isSameDifxBaseline(D->baseline+b, D->baseline+b1, 0))
			{
				break;
			}
		}
		if(b == b1)	/* no match found */
		{
			b++;	/* advance to next baseline */
		}
		else		/* found match */
		{
			/* 1. Renumber this and all higher baselines */
			for(c = 0; c < D->nConfig; c++)
			{
				for(cb = 0; cb < D->config[c].nBaseline; cb++)
				{
					b0 = D->config[c].baselineId[cb];
					if(b0 == b)
					{
						b0 = b1;
					}
					else if(b0 > b)
					{
						b0--;
					}
					D->config[c].baselineId[cb] = b0;
				}
			}

			/* 2. reduce number of baselines */
			D->nBaseline--;

			/* 3. Delete this baseline and bump up higher ones */
			deleteDifxBaselineInternals(D->baseline+b);
			for(b1 = b; b1 < D->nBaseline; b1++)
			{
				moveDifxBaseline(D->baseline+b1, D->baseline+b1+1);
			}
		}
	}

	return n0 - D->nBaseline;
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
					b->bandA[f][p]);
				writeDifxLineInt1(out, "D/STREAM B BAND %d", p,
					b->bandB[f][p]);
				n += 2;
			}
		}
	}

	return n;
}
