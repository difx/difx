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

void copyDifxEOP(DifxEOP *dest, const DifxEOP *src)
{
	memcpy(dest, src, sizeof(DifxEOP));
}

/* Note this function returns the number of merged EOP entries on the call 
 * stack : nde */
DifxEOP *mergeDifxEOPArrays(const DifxEOP *de1, int nde1, 
	const DifxEOP *de2, int nde2, int *nde)
{
	DifxEOP *de;
	int mjdMin=-1, mjdMax=-1;
	int i, i1=0, i2=0;
	int src;

	if(nde1 == 0 && nde2 == 0)
	{
		*nde = 0;
		return 0;
	}

	/* First determine how many output eop rows.  It cannot be more than
	 * the sum of lengths of the two lists, but is likely shorter.  It
	 * also cannot be longer than mjdMax - mjdMin + 1 */

	if(nde1 > 0) for(i = 0; i < nde1; i++)
	{
		if(de1[i].mjd < mjdMin || mjdMin == -1)
		{
			mjdMin = de1[i].mjd;
		}
		if(de1[i].mjd > mjdMax || mjdMax == -1)
		{
			mjdMax = de1[i].mjd;
		}
	}
	if(nde2 > 0) for(i = 0; i < nde2; i++)
	{
		if(de2[i].mjd < mjdMin || mjdMin == -1)
		{
			mjdMin = de2[i].mjd;
		}
		if(de2[i].mjd > mjdMax || mjdMax == -1)
		{
			mjdMax = de2[i].mjd;
		}
	}

	if(mjdMax - mjdMin + 1 < nde1 + nde2)
	{
		*nde = mjdMax - mjdMin + 1;
	}
	else
	{
		*nde = nde1 + nde2;
	}

	de = newDifxEOPArray(*nde);

	/* now merge sort the EOPs, omitting duplicates */

	i = 0;
	for(;;)
	{
		if(i1 >= nde1)
		{
			i1 = -1;
		}
		if(i2 >= nde2)
		{
			i2 = -1;
		}
		if(i1 < 0 && i2 < 0)
		{
			break;
		}

		/* determine which EOPArray to take from */
		if(i1 == -1)
		{
			src = 2;
		}
		else if(i2 == -1)
		{
			src = 1;
		}
		else if(de1[i1].mjd <= de2[i2].mjd)
		{
			src = 1;
			/* for two eops from same day, take the first */
			if(de1[i1].mjd == de2[i2].mjd)
			{
				i2++;
			}
		}
		else
		{
			src = 2;
		}

		/* do the copy and increments */
		if(src == 1)
		{
			copyDifxEOP(de + i, de1 + i1);
			i++;
			i1++;
		}
		else
		{
			copyDifxEOP(de + i, de2 + i2);
			i++;
			i2++;
		}
	}

	/* in case the output count was an overestimate: */
	*nde = i;

	return de;
}
