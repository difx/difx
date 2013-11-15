/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsSO(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	struct fitsBinTableColumn columns[] =
	{
		{"SPACECR", "16A", "spacecraft name", 0},
		{"TIME",    "1D",  "UT time", "DAYS"},
		{"ORBXYZ",  "3D",  "geocentric coordinates", "METERS"},
		{"VELXYZ",  "3D",  "velcity vector", "METERS/SEC"}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf;
	char *p_fitsbuf;
	int s;
	int nRec = 0;
	
	if(D == 0)
	{
		return 0;
	}

	if(D->nSpacecraft == 0 || !D->spacecraft)
	{
		return D;
	}

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
	
	for(s = 0; s < D->nSpacecraft; ++s)
	{
		int p;
		char name[16];

		strcpypad(name, D->spacecraft[s].name, 16);

		for(p = 0; p < D->spacecraft[s].nPoint; ++p)
		{
			double xyz[3], vel[3];
			const sixVector *pos;
			double time;

			pos = D->spacecraft[s].pos + p;

			time = pos->mjd + pos->fracDay;
			xyz[0] = pos->X;
			xyz[1] = pos->Y;
			xyz[2] = pos->Z;
			vel[0] = pos->dX;
			vel[1] = pos->dY;
			vel[2] = pos->dZ;

			p_fitsbuf = fitsbuf;

			if(nRec == 0)
			{
				fitsWriteBinTable(out, nColumn, columns, nRowBytes, "SPACECRAFT_ORBIT");
				arrayWriteKeys(p_fits_keys, out);
				fitsWriteInteger(out, "TABREV", 1, "");
				fitsWriteEnd(out);
			}

			FITS_WRITE_ITEM (name, p_fitsbuf);
			FITS_WRITE_ITEM (time, p_fitsbuf);
			FITS_WRITE_ITEM (xyz, p_fitsbuf);
			FITS_WRITE_ITEM (vel, p_fitsbuf);

			testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "SO");

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);

			++nRec;
		}
	}

	free(fitsbuf);

	return D;
}	
