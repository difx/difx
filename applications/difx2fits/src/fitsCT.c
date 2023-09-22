/***************************************************************************
 *   Copyright (C) 2008-2009 by Walter Brisken                             *
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


/*
* IAU (1976) System of Astronomical Constants
* SOURCE:  USNO Circular # 163 (1981dec10)
* ALL ITEMS ARE DEFINED IN THE SI (MKS) SYSTEM OF UNITS  
*
* WARNING: These constants are used by the CALC model program. Please
* do not change them.
*/
#define GAUSS_GRAV     0.01720209895   /* Gaussian gravitational constant */
#define C_LIGHT        299792458.      /* Speed of light; m/s */
#define TAU_A          499.004782      /* Light time for one a.u.; sec */
#define E_EQ_RADIUS    6378137.        /* Earth's Equatorial Radius, meters
                                          (IUGG value) */
#define E_FORM_FCTR    0.00108263      /* Earth's dynamical form factor */
#define GRAV_GEO       3.986005e14     /* Geocentric gravitational constant;
                                          (m^3)(s^-2) */
#define GRAV_CONST     6.672e-11       /* Constant of gravitation;
                                          (m^3)(kg^-1)(s^-2) */
#define LMASS_RATIO    0.01230002      /* Ratio of mass of Moon to mass of
                                          Earth */
#define PRECESS        5029.0966       /* General precession in longitude;
                                          arcsec per Julian century
                                          at standard epoch J2000 */
#define OBLIQUITY      84381.448       /* Obliquity of the ecliptic at
                                          epoch J2000; arcsec */
#define NUTATE         9.2025          /* Constant of nutation at
                                          epoch J2000; arcsec */
#define ASTR_UNIT      1.49597870e11   /* Astronomical unit; meters */
#define SOL_PRLX       8.794148        /* Solar parallax; arcsec */
#define ABERRATE       20.49552        /* Constant of aberration at
                                          epoch J2000; arcsec */
#define E_FLAT_FCTR    0.00335281      /* Earth's flattening factor */
#define GRAV_HELIO     1.32712438e20   /* Heliocentric gravitational constant
                                          (m^3)(s^-2) */
#define S_E_RATIO      332946.0        /* Ratio of mass of Sun to mass of 
                                          Earth */
#define S_EMOON_RATIO  328900.5        /* Ratio of mass of sun to
                                          mass of Earth plus Moon */
#define SOLAR_MASS     1.9891e30       /* Mass of Sun; kg */
#define JD_J2000       2451545.0       /* Julian Day Number of 2000jan1.5 */
#define BES_YEAR       365.242198781   /* Length of Besselian Year in days
                                          at B1900.0 (JD 2415020.31352) */
#define SOLAR_SID   0.997269566329084  /* Ratio of Solar time interval to
                                          Sidereal time interval at J2000 */
#define SID_SOLAR   1.002737909350795  /* Ratio of Sidereal time interval
                                          to Solar time interval at J2000 */
#define ACCEL_GRV      9.78031846      /* acceleration of gravity at the
                                        * earth's surface (m)(s^-2) */
#define GRAV_MOON      4.90279750e12   /* Lunar-centric gravitational constant
                                          (m^3)(s^-2) */
#define ETIDE_LAG      0.0             /* Earth tides: lag angle (radians) */
#define LOVE_H         0.60967         /* Earth tides: global Love Number H,
                                          IERS value (unitless) */
#define LOVE_L         0.0852          /* Earth tides: global Love Number L,
                                          IERS value (unitless) */

struct __attribute__((packed)) CTrow
{
	double time, ut1_utc, iat_utc, a1_iat;
	char ut1Type;
	double wobX, wobY;
	char wobType;
	double dPsi, ddPsi, dEps, ddEps;
};

const DifxInput *DifxInput2FitsCT(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	struct CTrow row;

	static struct fitsBinTableColumn columns[] =
	{
		{"TIME",    "1D",  "Time of center of interval", "DAYS"},
		{"UT1-UTC", "1D", "Difference between UT1 and UTC","SECONDS"},
		{"IAT-UTC", "1D", "Difference between IAT and UTC","SECONDS"},
		{"A1-IAT",  "1D", "Difference between A1 and IAT","SECONDS"},
		{"UT1 TYPE","1A", "E=extrapol., P=prelim., F+final",""},
		{"WOBXY",   "2D", "x, y polar offsets","arcsec"},
		{"WOB TYPE","1A", "E=extrapol., P=prelim., F+final",""},
		{"DPSI",    "1D", "Nutation in longitude","rad"},
		{"DDPSI",   "1D", "CT derivative of DPSI","rad/sec"},
		{"DEPS",    "1D", "Nutation in obliquity","rad"},
		{"DDEPS",   "1D", "CT derivative of DEPS","rad/sec"}
	};
	
	int nRowBytes;
	int nColumn;
	int e;
	const DifxEOP *eop;

	if(D == 0)
	{
		return 0;
	}
	
	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "CALC");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteString (out, "C_SRVR", D->job->calcServer, "");
	fitsWriteString (out, "C_VERSN", "9.1", "");
	fitsWriteString (out, "A_VERSN", "2.2", "");
	fitsWriteString (out, "I_VERSN", "0.0", "");
	fitsWriteString (out, "E_VERSN", "9.1", "");

	fitsWriteFloat(out, "ACCELGRV", ACCEL_GRV, "");
	fitsWriteFloat(out, "E-FLAT", E_FLAT_FCTR, "");
	fitsWriteFloat(out, "EARTHRAD", E_EQ_RADIUS, "");
	fitsWriteFloat(out, "MMSEMS", LMASS_RATIO, "");
	fitsWriteInteger(out, "EPHEPOC", 2000, "");

	fitsWriteFloat(out, "ETIDELAG", ETIDE_LAG, "");
	fitsWriteFloat(out, "GAUSS", GAUSS_GRAV, "");
	fitsWriteFloat(out, "GMMOON", GRAV_MOON, "");
	fitsWriteFloat(out, "GMSUN", GRAV_HELIO, "");
	fitsWriteFloat(out, "LOVE_H", LOVE_H, "");
	fitsWriteFloat(out, "LOVE_L", LOVE_L, "");
	fitsWriteFloat(out, "PRE_DATA", PRECESS, "");
	fitsWriteFloat(out, "REL_DATA", 1.0, "");
	fitsWriteInteger(out, "TIDALUT1", 0, "");

	fitsWriteFloat(out, "TSECAU", TAU_A, "");
	fitsWriteFloat(out, "U-GRV-CN", GRAV_CONST, "");
	fitsWriteFloat(out, "VLIGHT", C_LIGHT, "");

	fitsWriteEnd(out);

	if(D->nEOP > 0)
	{
		for(e = 0; e < D->nEOP; e++)
		{
			eop = D->eop + e;

			row.time    = eop->mjd - (int)D->mjdStart;
			row.ut1_utc = eop->ut1_utc;
			row.iat_utc = eop->tai_utc;
			row.a1_iat  = 0.0;
			row.ut1Type = 'X';
			row.wobX    = eop->xPole;
			row.wobY    = eop->yPole;
			row.wobType = 'X';
			row.dPsi    = 0.0;
			row.ddPsi   = 0.0;
			row.dEps    = 0.0;
			row.ddEps   = 0.0;

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, NELEMENTS(columns), &row);
#endif
			fitsWriteBinRow(out, (char *)&row);
		}
	}
	else
	{
		printf("\n    Warning: setting EOPs to zero\n");
		printf("                            ");
		for(e = 0; e < 5; e++)
		{
			row.time    = e-2;
			row.ut1_utc = 0.0;
			row.iat_utc = DEFAULT_IAT_UTC;
			row.a1_iat  = 0.0;
			row.ut1Type = 'X';
			row.wobX    = 0.0;
			row.wobY    = 0.0;
			row.wobType = 'X';
			row.dPsi    = 0.0;
			row.ddPsi   = 0.0;
			row.dEps    = 0.0;
			row.ddEps   = 0.0;

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, &row);
#endif
			fitsWriteBinRow(out, (char *)&row);

		}
	}

	return D;
}

