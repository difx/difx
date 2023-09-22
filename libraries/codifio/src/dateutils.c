/***************************************************************************
 *   Copyright (C) 2009-2015 by Adam Deller / Walter Brisken               *
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
// $Id: dateutils.c 6957 2015-08-31 15:34:32Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision: 6957 $
// $Author: WalterBrisken $
// $LastChangedDate: 2015-09-01 01:34:32 +1000 (Tue, 01 Sep 2015) $
//
//============================================================================

#include "dateutils.h"

void mjd2ymd(int mjd, int *year, int *month, int *day)
{
	int jd, temp1, temp2;

	jd = mjd + 2400001;

	// Do some rather cryptic calculations

	temp1 = 4*(jd+((6*(((4*jd-17918)/146097)))/4+1)/2-37);
	temp2 = 10*(((temp1-237)%1461)/4)+5;

	*year = temp1/1461-4712;
	*month =((temp2/306+2)%12)+1;
	*day = (temp2%306)/10+1;
}

int ymd2doy(int yr, int mo, int day)
{
        int monstart1[] = {0,31,59,90,120,151,181,212,243,273,304,334};
        int monstart2[] = {0,31,60,91,121,152,182,213,244,274,305,335};
        int L2;

        L2 = yr/4-(yr+7)/4-yr/100+(yr+99)/100+yr/400-(yr+399)/400;
        if(L2 == -1)
        {
                return day + monstart2[mo-1];
        }
        else
        {
                return day + monstart1[mo-1];
        }
}

int ymd2mjd(int yr, int mo, int day)
{
        int doy;
        int yr1 = yr - 1;

        doy = ymd2doy(yr, mo, day);

        return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
}
