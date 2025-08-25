/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken & Adam Deller               *
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
// $Id: fitsAG.c 10024 2021-05-25 14:53:00Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/difx2fits/src/fitsAG.c $
// $LastChangedRevision: 10024 $
// $Author: WalterBrisken $
// $LastChangedDate: 2021-05-25 22:53:00 +0800 (二, 2021-05-25) $
//
//============================================================================
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include <difxio/antenna_db.h>
#include "config.h"
#include "difx2fits.h"

static double arrayGMST(int mjd)
{
	double mjd2000 = 51545.0;
	double gstmn;
	double convhs = 7.2722052166430399e-5;
	double cent;
	double daysj;
	double gmstc[4];
	int igstmn;

	gmstc[0] = 24110.548410;
	gmstc[1] = 8640184.8128660;
	gmstc[2] = 0.0931040;
	gmstc[3] = -6.2e-6;

	daysj = mjd - mjd2000 + 0.5;

	cent = daysj / 36525;

	gstmn = (gmstc[0] + gmstc[1]*cent + gmstc[2]*cent*cent + gmstc[3]*cent*cent*cent)*convhs;

	igstmn = gstmn / (2.0*M_PI);
	gstmn = gstmn - (double)igstmn * (2.0*M_PI);
	if(gstmn < 0.0)
	{
		gstmn += (2.0*M_PI);
	}
  
	return gstmn / (2.0*M_PI);
}

struct __attribute__((packed)) AGrow
{
	char name[8];
	double x, y, z; /* [m] */
	float dx, dy, dz; /* [m/s] */
	int32_t antId1;
	int32_t mountType;
	float offset[3];
	float diameter; /* [m] */
};

const DifxInput *DifxInput2FitsAG(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/* define the antenna geometry FITS table columns */
	static struct fitsBinTableColumn columns[] =
	{
		{"ANNAME", "8A", "station name", 0},
		{"STABXYZ", "3D", "station offset from array origin", "METERS"},
		{"DERXYZ", "3E", "first order derivs of STABXYZ", "M/SEC"},
		{"ORBPARM", "0D", "orbital parameters", 0},
		{"NOSTA", "1J", "station id number", 0},
		{"MNTSTA", "1J", "antenna mount type", 0},
		{"STAXOF", "3E", "axis offset, x, y, z", "METERS"},
		{"DIAMETER", "1E", "antenna diameter", "METERS"}
	};

	char ref_date[12];
	int nRowBytes;
	int a, mjd;

	if(D == 0)
	{
		return 0;
	}

	nRowBytes = FitsBinTableSize(columns, NELEMENTS(columns));

	/* A warning for developers */
	if(nRowBytes != sizeof(struct AGrow))
	{
		fprintf(stderr, "AG table : nRowBytes != sizeof(row) : %d != %u\n", nRowBytes, (unsigned int)(sizeof(struct AGrow)));

		exit(EXIT_FAILURE);
	}

	fitsWriteBinTable(out, NELEMENTS(columns), columns, nRowBytes, "ARRAY_GEOMETRY");

	mjd = (int)(D->mjdStart);
	mjd2fits(mjd, ref_date);

	fitsWriteFloat(out, "ARRAYX", 0.0, "");
	fitsWriteFloat(out, "ARRAYY", 0.0, "");
	fitsWriteFloat(out, "ARRAYZ", 0.0, "");
	fitsWriteString(out, "ARRNAM", "VLBA", "");
	fitsWriteInteger(out, "NUMORB", 0, "");
	fitsWriteFloat(out, "FREQ", p_fits_keys->ref_freq, "");
	fitsWriteString(out, "FRAME", "GEOCENTRIC", "");
	fitsWriteString(out, "TIMSYS", "UTC", "");
	fitsWriteString(out, "TIMESYS", "UTC", "");
	fitsWriteFloat(out, "GSTIA0", 360.0*arrayGMST(mjd), "");
	fitsWriteFloat(out, "DEGPDY", 360.9856449733, "");
	
	if(D->nEOP > 0)
	{
		int e;

		for(e = 0; e < D->nEOP; ++e)
		{
			if(D->eop[e].mjd == mjd)
			{
				break;
			}
		}

		if(e >= D->nEOP)
		{
			fprintf(stderr, "EOP entry not found for mjd=%d\n", mjd);
			
			return 0;
		}

		fitsWriteFloat(out, "POLARX", D->eop[e].xPole, "");
		fitsWriteFloat(out, "POLARY", D->eop[e].yPole, "");
		fitsWriteFloat(out, "UT1UTC", D->eop[e].ut1_utc, "");
		fitsWriteFloat(out, "IATUTC", (double)(D->eop[e].tai_utc), "");
	}
	else
	{
		printf("\n\nWarning: IATUTC is not provided.  Assuming %3.1f seconds.\n\n", (double)(DEFAULT_IAT_UTC) );
		fitsWriteFloat(out, "IATUTC", (double)(DEFAULT_IAT_UTC), "");
	}
	
  	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	for(a = 0; a < D->nAntenna; ++a)
	{
		const DifxAntenna *antenna;
		struct AGrow row;
		int i;
		const AntennaDBEntry *ae;

		antenna = D->antenna + a;

		ae = antennaDBGetByXYZ(antenna->X, antenna->Y, antenna->Z);

		strcpypad(row.name, antenna->name, 8);
		row.x = antenna->X;
		row.y = antenna->Y;
		row.z = antenna->Z;
		row.dx = antenna->dX;
		row.dy = antenna->dY;
		row.dz = antenna->dZ;
		row.antId1 = a+1;
		if(ae != 0 && ae->mountType[0])
		{
			int mt;

			mt = stringToMountType(ae->mountType);

			if(mt != antenna->mount)
			{
				printf("\nNote: changing mount type of antenna %s from %d = %s to %d = %s.  I hope this is OK.\n", antenna->name, antenna->mount, antennaMountTypeNames[antenna->mount], mt, ae->mountType);
			}

			row.mountType = mt;
		}
		else
		{
			row.mountType = antenna->mount;
		}
		if(antenna->mount == AntennaMountXYNS)
		{
			printf("\n\nWarning: mount type XYNS is not handled in AIPS so is being set to XYEW.\n");
			printf("Expect parallactic angles to be calculated incorrectly.\n\n");
		}

		for(i = 0; i < 3; ++i)
		{
			row.offset[i] = antenna->offset[i];
		}
		if(ae)
		{
			row.diameter = ae->diameter;
		}
		else
		{
			row.diameter = 0.0;
		}
#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, NELEMENTS(columns), &row);
#endif
		fitsWriteBinRow(out, (char *)&row);
	}
	
	return D;
}
