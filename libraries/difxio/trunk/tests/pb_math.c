/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & John Morgan               *
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

#include <math.h>

// Not yet in use!! (functions below duplicated in pbgen)
// NB Unit test below!!
const char program[] = "pbgen";
const char author[]  = "John Morgan <john.morgan@icrar.org>";
const char version[] = "0.1";
const char verdate[] = "20120927";

double haversine(double az1, double el1, double az2, double el2)
{
	/*  
	 * Determine the great circle angular distance between two points
	 * in spherical polar coordinates
	 *
	 * This should give the distance from the pointing centre (az1, el1) of a 2nd point (az2, el2)
	 * all must be in radians
	 */
	double a;
	a = pow(sin((el1 - el2)/2), 2) + (pow(sin((az1 - az2)/2), 2) * pow(cos(el1), 2));
	return 2 * atan2(sqrt(a), sqrt(1-a));
}

double azbearing(double az1, double el1, double az2, double el2)
{
	/*  Initial bearing from 'North' of point 1 from point 2
	 *  For an az/el mounted telescope, 'North' is always 'up' so this should
	 *  give the bearing within the primary beam of (az2, el2) from a 
	 *  pointing centre (az1, el1).
	 * 
	 *  The exact form is such that if the two points share the same RA, and the decl(2) > decl(1),
	 *  The angle returned is the Parallactic Angle
	 */
	double se1, ce1, se2, ce2, sa21, ca21;
	sincos(el1, &se1, &ce1);
	sincos(el2, &se2, &ce2);
	sincos(az2-az1, &sa21, &ca21);

	return -atan2(sa21*ce1, (ce2*se1) - (se2*ce1*ca21));
}


/**********************************************************************
 * UNIT TEST
 *********************************************************************/
#ifdef UNIT_TEST

/* Example:
gcc -DUNIT_TEST pb_math.c -o test
./test
*/
double degrees(double r)
{
	return r*180.0/M_PI;
}

double radians(double d)
{
	return d*M_PI/180.0;
}

int main()
{
	int i;
	double result, expect;
	i=1;
	result = fabs(degrees(azbearing(radians(90), radians(45), radians(270), radians(45))));
	expect = 0.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=2;
	result = degrees(azbearing(radians(90), radians(0), radians(80), radians(0)));
	expect = 90.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=3;
	result = fabs(degrees(azbearing(radians(180), radians(0), radians(180), radians(10))));
	expect = 180.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=4;
	result = degrees(azbearing(radians(90), radians(0), radians(80), radians(0)));
	expect = 90.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=5;
	result = degrees(azbearing(radians(270), radians(0), radians(280), radians(0)));
	expect = -90.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=6;
	result = degrees(haversine(0, radians(90), 0, radians(80)));
	expect = 10.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=7;
	result = degrees(haversine(radians(50), 0, radians(60), 0));
	expect = 10.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=8;
	result = degrees(haversine(radians(90), radians(89), radians(270), radians(89)));
	expect = 2.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=9;
	result = degrees(haversine(radians(90), radians(80), radians(-90), radians(80)));
	expect = 20.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=10;
	result = degrees(haversine(radians(0), radians(0), radians(180), radians(0)));
	expect = 180.000000000000;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	i=11;
	result = degrees(haversine(radians(90), radians(30), radians(100), radians(40)));
	expect = 12.908258700131;
	printf("%03d %+.12e %+.12e\n", i, result, expect);
	return 0;
}
#endif
