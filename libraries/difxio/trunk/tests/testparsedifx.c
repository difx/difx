/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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
#include <stdlib.h>
#include "difxio/parsedifx.h"

int main(int argc, char **argv)
{
	DifxParameters *dp;
	int i;

	if(argc < 2)
	{
		fprintf(stderr, "Usage : %s <input file>\n", argv[0]);
		
		return EXIT_SUCCESS;
	}
	
	/*** First demonstrate parsing on an input file ***/

	printf("Loading DiFX Parameters from file\n");
	
	/* Load file */
	dp = newDifxParametersfromfile(argv[1]);

	/* Print to stdout */
	printDifxParameters(dp);

	/* Look for simple key, print if found */
	i = DifxParametersfind(dp, 0, "OUTPUT FORMAT");
	printf("i = %d\n", i); 
	if(i >= 0)
	{
		printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	}
	else
	{
		printf("key not found\n");
	}
	
	/* Look for key with one index, print if found */
	i = DifxParametersfind1(dp, 0, "DATASTREAM %d INDEX", 3);
	printf("i = %d\n", i); 
	if(i >= 0)
	{
		printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	}
	else
	{
		printf("key not found\n");
	}
	
	/* free from memory */
	deleteDifxParameters(dp);

	/*** Next, demonstrate buiding DifxParameters object from a set of
	     strings as you might do if constructing from stdin ***/
	
	printf("Making new DifxParameters structure by hand\n");

	/* first create empty parameter structure */
	dp = newDifxParameters();

	/* then start adding some rows */
	DifxParametersaddrow(dp, "MJD:                54140");
	DifxParametersaddrow(dp, "U (METRES):         123456.89");

	/* then demonstrate finding a row */
	i = DifxParametersfind(dp, 0, "MJD");
	printf("i = %d\n", i);
	if(i >= 0)
	{
		printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	}
	else
	{
		printf("key not found\n");
	}

	/* searching for an non-existent parameter */
	i = DifxParametersfind(dp, 0, "NOT HERE");
	if(i >= 0)
	{
		printf("%s = %s\n", dp->rows[i].key, DifxParametersvalue(dp, i));
	}
	else
	{
		printf("key not found\n");
	}
	/* free from memory */
	deleteDifxParameters(dp);
	
	return EXIT_SUCCESS;
}
