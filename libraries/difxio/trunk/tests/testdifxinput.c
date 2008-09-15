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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include "difx_input.h"

int main(int argc, char **argv)
{
	DifxInput *D;
	
	if(argc < 2)
	{
		printf("Usage : %s <inputfilebase>\n", argv[0]);
		return 0;
	}
	
	D = loadDifxInput(argv[1]);
	if(!D)
	{
		fprintf(stderr, "D == 0.  quitting\n");
		return 0;
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "update failed: D == 0.  quitting\n");
		return 0;
	}

	printDifxInput(D);

	writeDifxCalc(D, "calc.out");
	writeDifxInput(D, "input.out");

	deleteDifxInput(D);

	return 0;
}
