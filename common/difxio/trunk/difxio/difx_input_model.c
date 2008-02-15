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


DifxModel **newDifxModelArray(int nAntenna, int nPoint)
{
	DifxModel **dm;
	int a, N;

	dm = (DifxModel **)calloc(nAntenna, sizeof(DifxModel *));

	N = nPoint + 3;

	for(a = 0; a < nAntenna; a++)
	{
		dm[a] = (DifxModel *)calloc(N, sizeof(DifxModel));

		/* offset the array so we can index -1 */
		dm[a]++;
	}

	return dm;
}

void deleteDifxModelArray(DifxModel **dm, int nAntenna)
{
	int a;
	
	if(dm)
	{
		for(a = 0; a < nAntenna; a++)
		{
			/* free memory from actual start of array */
			dm[a]--;

			free(dm[a]);
		}
		free(dm);
	}
}

void printDifxModel(const DifxModel *dm)
{
	printf("    DifxModel : %p\n", dm);
	printf("      U, V, W = %f %f %f m\n", dm->u, dm->v, dm->w);
	printf("      Delay   = %f us\n", dm->t);
}

