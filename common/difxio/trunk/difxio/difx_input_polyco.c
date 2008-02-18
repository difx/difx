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


DifxPolyco *newDifxPolycoArray(int nPolyco)
{
	DifxPolyco *dp;

	dp = (DifxPolyco *)calloc(nPolyco, sizeof(DifxPolyco));

	return dp;
}

void deleteDifxPolycoArray(DifxPolyco *dp, int nPolyco)
{
	/* FIXME -- implement fully! */
	free(dp);
}

void copyDifxPolyco(DifxPolyco *dest, const DifxPolyco *src)
{
	/* FIXME -- implement fully! */
	strcpy(dest->fileName, src->fileName);
}

DifxPolyco *dupDifxPolycoArray(const DifxPolyco *src, int nPolyco)
{
	DifxPolyco *dp;
	int p;

	dp = newDifxPolycoArray(nPolyco);
	for(p = 0; p < nPolyco; p++)
	{
		copyDifxPolyco(dp + p, src + p);
	}

	return dp;
}
