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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"


DifxAntennaFlag *newDifxAntennaFlagArray(int nFlag)
{
	DifxAntennaFlag *df;

	if(nFlag == 0)
	{
		return 0;
	}

	df = (DifxAntennaFlag *)calloc(nFlag, sizeof(DifxAntennaFlag));

	return df;
}

void deleteDifxAntennaFlagArray(DifxAntennaFlag *df)
{
	if(df)
	{
		free(df);
	}
}

void fprintDifxAntennaFlagArray(FILE *fp, const DifxAntennaFlag *df, int nf)
{
	int f;

	fprintf(fp, "  Difx Antenna Flag : %p\n", df);
	for(f = 0; f < nf; f++)
	{
		fprintf(fp, "    Flag: AntId=%d  times %12.6f to %12.6f\n", 
			df[f].antennaId,
			df[f].mjd1, df[f].mjd2);
	}
}

void printDifxAntennaFlagArray(const DifxAntennaFlag *df, int nf)
{
	fprintDifxAntennaFlagArray(stdout, df, nf);
}

void copyDifxAntennaFlag(DifxAntennaFlag *dest, const DifxAntennaFlag *src,
	const int *antennaIdRemap)
{
	memcpy(dest, src, sizeof(DifxAntennaFlag));
	if(antennaIdRemap)
	{
		dest->antennaId = antennaIdRemap[dest->antennaId];
	}
}

DifxAntennaFlag *mergeDifxAntennaFlagArrays(const DifxAntennaFlag *df1, 
	int ndf1, const DifxAntennaFlag *df2, int ndf2, 
	const int *antennaIdRemap, int *ndf)
{
	DifxAntennaFlag *df;
	int i;

	*ndf = ndf1 + ndf2;
	if(*ndf <= 0)
	{
		return 0;
	}

	df = newDifxAntennaFlagArray(*ndf);

	if(ndf1 > 0) for(i = 0; i < ndf1; i++)
	{
		copyDifxAntennaFlag(df + i, df1 + i, 0);
	}
	if(ndf2 > 0) for(i = 0; i < ndf2; i++)
	{
		copyDifxAntennaFlag(df + ndf1 + i, df2 + i,
			antennaIdRemap);
	}

	return df;
}
