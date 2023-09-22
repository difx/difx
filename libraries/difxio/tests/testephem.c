/***************************************************************************
 *   Copyright (C) 2013 by Walter Brisken                                  *
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
 * $HeadURL: $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include "difxio/difx_input.h"

const char program[] = "testephem";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20130416";

int main(int argc, char **argv)
{
	DifxSpacecraft *ds;

	if(argc != 7)
	{
		printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
		printf("Usage: %s <mjd0> <deltat> <n> <obj> <naif file> <ephem file>\n", argv[0]);

		return 0;
	}

	ds = newDifxSpacecraftArray(1);

	computeDifxSpacecraftEphemeris(ds, 
		atof(argv[1]),			/* mjd0 [days] */
		atof(argv[2])/86400.0,		/* deltat [sec] */
		atoi(argv[3]),			/* nPoint */
		argv[4],			/* obj name */
		argv[5],			/* naifFile */
		argv[6],			/* ephemFile */
		0.0, 0.0);

	writeDifxSpacecraftArray(stdout, 1, ds);

	deleteDifxSpacecraftArray(ds, 1);

	return 0;
}
