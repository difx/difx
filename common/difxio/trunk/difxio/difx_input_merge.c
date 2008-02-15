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


DifxInput *mergeDifxInputs(const DifxInput *D1, const DifxInput *D2)
{
	DifxInput *D;
	int i;
	
	int *freqIdRemap;

	if(!D1 || !D2)
	{
		return 0;
	}

	/* allocate some scratch space */
	freqIdRemap = (int *)calloc(D2->nFreq, sizeof(int));

	/* merge DifxFreq table */
	D->freq = mergeDifxFreqArrays(D1->freq, D1->nFreq,
		D2->freq, D2->nFreq, freqIdRemap);
	D->nFreq = D1->nFreq;
	for(i = 0; i < D2->nFreq; i++)
	{
		if(freqIdRemap[i]+1 > D->nFreq)
		{
			D->nFreq = freqIdRemap[i]+1;
		}
	}



	/* clean up */

	free(freqIdRemap);

	return D;
}
