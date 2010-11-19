/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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

#include <stdlib.h>
#include "difx_calculator.h"


DifxCalculator *newDifxCalculator()
{
	DifxCalculator *C;

	C = (DifxCalculator *)calloc(1, sizeof(DifxCalculator));
	
	if(C)
	{
		C->speedUp = 1.0;
	}

	return C;
}

void deleteDifxCalculator(DifxCalculator *C)
{
	if(C)
	{
		free(C);
	}
}

int populateDifxCalculator(DifxCalculator *C, const DifxInput *D)
{
	if(!D || !C)
	{
		return -1;
	}

	C->nConfig = D->nConfig;

	return 0;
}

void printDifxCalculator(const DifxCalculator *C)
{

	printf("Number of Configurations = %d\n", C->nConfig);
}
