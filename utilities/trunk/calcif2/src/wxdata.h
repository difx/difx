/***************************************************************************
 *   Copyright (C) 2019 by Walter Brisken                                  *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __WXDATA_H__
#define __WXDATA_H__

#include <difxio/difx_input.h>

typedef struct
{
	double mjd;
	double temp;		/* [C] */
	double pressure;	/* [mBar] */
	double dewpoint;	/* [C] */
	double windspeed;	/* [m/s] */
	double winddir;		/* [deg] */
	double rain;		/* [cm] */
	double windgust;	/* [m/s] */
} WXDataRow;

typedef struct
{
	int nRow;
	WXDataRow *row;
} WXData;

int loadWeatherForAntenna(WXData *wxData, const char *filename);

WXData *loadWeatherForProject(DifxInput *D);

void deleteWXDataArray(WXData *wxData, int n);

int interpolateWXData(WXDataRow *output, const WXData *wxData, double mjd);

#endif
