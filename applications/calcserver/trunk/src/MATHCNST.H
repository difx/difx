/*  @(#)MATHCNST.H  version 1.7  created 95/03/01 14:01:39
                fetched from SCCS 02/01/29 07:49:47
%% mathematical constants
LANGUAGE: C
ENVIRONMENT: Any
:: header include constant math IAU
-*/
#ifndef __MATHCNST_H__
#define __MATHCNST_H__

#define PI         3.1415926535897932385
#define TWOPI      6.2831853071795864769    /* value from 'CALC' code */
#define PIOVR2     1.5707963267948966192    /* PI/2 */
#define DEGREE     1.7453292519943296E-2    /* radians per degree */
#define ARCMIN     2.9088820866572159615E-4 /* radians per arc-minute */
#define ARCSEC     4.8481368110953599359E-6 /* radians per arc-second */
#define DEG_RAD    5.729577951308232E1      /* degrees per radian */
#define HOUR_RAD   3.819718634205488        /* time hours per radian */
#define SEC_RAD    (3600.0*HOUR_RAD)        /* time seconds per radian */
#define RAD_DEG    DEGREE
#define RAD_AMIN   ARCMIN
#define RAD_ASEC   ARCSEC
 
#define E          2.7182818284590452353    /* base of natural logs */
#define LOG_E_10   2.3025850929940456840    /* log (base e) of 10 */
#define LOG_E_2    0.6931471805599453       /* log (base e) of 2 */
#define LOG_10_E   4.3429448190325182765E-1 /* log (base 10) of e */

#define JUL_YEAR   365.25              /* days per Julian year */
 
#define SEC_DAY    86400               /* seconds per day */
#define JUL_CENT   36525               /* days per Julian century */

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
/* NOTE-  If the time interval between two events is t_sid sidereal
*         time units, then that same interval is t_sol = (t_sid * SOLAR_SID)
*         solar time units.  Similarly,  t_sid = (t_sol * SID_SOLAR). */

#endif
