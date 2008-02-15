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


DifxEOP *newDifxEOPArray(int nEOP)
{
	DifxEOP *de;

	if(nEOP == 0)
	{
		return 0;
	}

	de = (DifxEOP *)calloc(nEOP, sizeof(DifxEOP));

	return de;
}

void deleteDifxEOPArray(DifxEOP *de)
{
	if(de)
	{
		free(de);
	}
}

void printDifxEOP(const DifxEOP *de)
{
	printf("  DifxEOP [%d] : %p\n", (int)(de->mjd + 0.5), de);
	if(!de)
	{
		return;
	}
	printf("    TAI - UTC = %d sec\n", de->tai_utc);
	printf("    UT1 - UTC = %9.6f sec\n", de->ut1_utc);
	printf("    Pole X, Y = %8.6f, %8.6f arcsec\n", de->xPole, de->yPole);
}

