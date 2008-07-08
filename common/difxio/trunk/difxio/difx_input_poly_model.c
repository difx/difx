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


DifxPolyModel **newDifxPolyModelArray(int nAntenna, int nPoly)
{
	DifxPolyModel **dpm;
	int a;

	dpm = (DifxPolyModel **)calloc(nAntenna, sizeof(DifxPolyModel *));

	for(a = 0; a < nAntenna; a++)
	{
		dpm[a] = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));
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

void deleteDifxPolyModelArray(DifxPolyModel **dpm, int nAntenna)
{
	int a;

	if(dpm)
	{
		for(a = 0; a < nAntenna; a++)
		{
			if(dpm[a])
			{
				free(dpm[a]);
			}
		}
		free(dpm);
	}
}

void printDifxPolyModel(const DifxPolyModel *dpm)
{
	printf("    DifxPolyModel : %p\n", dpm);
	if(dpm)
	{
		printf("        mjd, sec = %d, %d\n", dpm->mjd, dpm->sec);
		printf("        delay = %22.15e", dpm->delay[0]);
	}
}
