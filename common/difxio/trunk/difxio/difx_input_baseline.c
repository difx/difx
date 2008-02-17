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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"


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

void printDifxBaseline(const DifxBaseline *db)
{
	int f;
	
	printf("  Difx Baseline : %p\n", db);
	printf("    datastream indices = %d %d\n", db->dsA, db->dsB);
	printf("    nFreq = %d\n", db->nFreq);
	if(db->nPolProd)
	{
		printf("    nPolProd[freq] =");
		for(f = 0; f < db->nFreq; f++)
		{
			printf(" %d", db->nPolProd[f]);
		}
		printf("\n");
	}
}

int isSameDifxBaseline(const DifxBaseline *db1, const DifxBaseline *db2)
{
	int f, p;

	if(db1->dsA   != db2->dsA || 
	   db1->dsB   != db2->dsB ||
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

void copyDifxBaseline(DifxBaseline *dest, const DifxBaseline *src)
{
	int f, p, nProd;

	dest->dsA   = src->dsA;
	dest->dsB   = src->dsB;
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
	const DifxBaseline *db2, int ndb2, int *baselineIdRemap)
{
	int ndb;
	int i, j;
	DifxBaseline *db;

	ndb = ndb1;

	/* first identify entries that differ and assign new baselineIds */
	for(j = 0; j < ndb2; j++)
	{
		for(i = 0; i < ndb1; i++)
		{
			if(isSameDifxBaseline(db1 + i, db2 + j))
			{
				baselineIdRemap[j] = i;
				break;
			}
		}
		if(i == ndb1)
		{
			baselineIdRemap[j] = ndb;
			ndb++;
		}
	}

	db = newDifxBaselineArray(ndb);
	
	/* now copy db1 */
	for(i = 0; i < ndb1; i++)
	{
		copyDifxBaseline(db + i, db1 + i);
	}

	/* now copy unique members of db2 */
	for(j = 0; j < ndb2; j++)
	{
		if(baselineIdRemap[j] >= ndb1)
		{
			copyDifxBaseline(db + baselineIdRemap[j], db2 + j);
		}
	}

	return db;
}
