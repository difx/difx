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


DifxSpacecraft *newDifxSpacecraftArray(int nSpacecraft)
{
	DifxSpacecraft *ds;
	
	if(nSpacecraft == 0)
	{
		return 0;
	}
	
	ds = (DifxSpacecraft *)calloc(nSpacecraft, sizeof(DifxSpacecraft));

	return ds;
}

void deleteDifxSpacecraft(DifxSpacecraft *ds, int nSpacecraft)
{
	int s;

	if(ds)
	{
		for(s = 0; s < nSpacecraft; s++)
		{
			if(ds[s].pos)
			{
				free(ds[s].pos);
			}
		}
		free(ds);
	}
}

void printDifxSpacecraft(const DifxSpacecraft *ds)
{
	printf("  DifxSpacecraft : %p\n", ds);
	if(!ds)
	{
		return;
	}
	printf("    Name = %s\n", ds->name);
	printf("    Num points = %d\n", ds->nPoints);
}

