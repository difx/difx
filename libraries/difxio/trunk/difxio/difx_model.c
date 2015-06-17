/***************************************************************************
 *   Copyright (C) 2008-2015 by Walter Brisken                             *
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


DifxModel **newDifxModelArray(int nAntenna, int nPoint)
{
	DifxModel **dm;
	int a, N;

	dm = (DifxModel **)calloc(nAntenna, sizeof(DifxModel *));

	N = nPoint + 3;

	for(a = 0; a < nAntenna; ++a)
	{
		dm[a] = (DifxModel *)calloc(N, sizeof(DifxModel));

		/* offset the array so we can index -1 */
		++dm[a];
	}

	return dm;
}

/* duplicate list of DifxModels */
DifxModel *dupDifxModelColumn(const DifxModel *src, int nPoint)
{
	DifxModel *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxModel *)calloc(nPoint+3, sizeof(DifxModel));
	
	/* offset array as per above */
	++dest;

	memcpy(dest-1, src-1, (nPoint+3)*sizeof(DifxModel));

	return dest;
}

void deleteDifxModelArray(DifxModel **dm, int nAntenna)
{
	if(dm)
	{
		int a;
	
		for(a = 0; a < nAntenna; ++a)
		{
			if(dm[a])
			{
				/* free memory from actual start of array */
				--dm[a];
				free(dm[a]);
			}
		}
		free(dm);
	}
}

void fprintDifxModel(FILE *fp, const DifxModel *dm)
{
	fprintf(fp, "    DifxModel : %p\n", dm);
	if(dm)
	{
		fprintf(fp, "      U, V, W = %f %f %f m\n", dm->u, dm->v, dm->w);
		fprintf(fp, "      Delay   = %f us\n", dm->t);
	}
}

void printDifxModel(const DifxModel *dm)
{
	fprintDifxModel(stdout, dm);
}
