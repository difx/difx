/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/parsevis.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include "difxio/difx_tcal.h"

int main()
{
	DifxTcal *T;
	int v;
	float t;

	T = newDifxTcal();

	printf("Initial: ");
	summarizeDifxTcal(T);

	v = setDifxTcalVLBA(T, "/home/jansky3/vlbaops/TCAL");
	printf("\nv = %d\n", v);

	printf("VLBA: ");
	summarizeDifxTcal(T);

	t = getDifxTcal(T, 0, "pt", "6cm", 'R', 5000.0);
	printf("\n5000@pt -> %f\n", t);

	printf("pt: ");
	summarizeDifxTcal(T);

	for(v = 1; v < 100; v+=10)
	{
		t = getDifxTcal(T, 0, "pt", "", 'R', 1000.0*v + 0.5);
		printf("%f@pt -> %f\n", 1000.0*v + 0.5, t);
	}

	printf("allpt: ");
	summarizeDifxTcal(T);

	for(v = 1; v < 100; v+=1)
	{
		t = getDifxTcal(T, 0, "pt", "", 'R', 1000.0*v + 0.5);
		printf("%f@pt -> %f\n", 1000.0*v + 0.5, t);
	}


	deleteDifxTcal(T);

	return 0;
}
