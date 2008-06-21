/*
 * cmxst.h: A c++ header file correspong to cmxst.i, for use in c/c++ programs
 *          Defines a global struct equivalent to the Fortran common block in cmxst.i
 */
 #ifdef _cplusplus
extern"C" {
#endif

#define max_stat short(100)

struct {
    double CFRAD[max_stat], PLAT[max_stat][3], PLON[max_stat][3], SITAXO[max_stat], SITOAM[max_stat][11], SITOPH[max_stat][11],
           SITXYZ[max_stat][3], SNRM[max_stat][3], SITZEN[max_stat], TCROT[max_stat][3][3], XLAT[max_stat], XLON[max_stat], 
           SITHOA[max_stat][2][11], SITHOP[max_stat][2][11], HEIGHT[max_stat], RTROT[max_stat][3][3], GLAT[max_stat];
    short  KTYPE[max_stat], NLAST[2], NUMSIT, LNSITE[max_stat][4], i3dum;
    int    Zero_site;
} sitcm_;

double sitere[max_stat][3];

#ifdef _cplusplus
}
#endif  

/* comments from cmxst.i
C       1.  CFRAD(Max_Stat)      -  THE SITE SPHERICAL EARTH RADII.  (M)
C       2.  PLAT(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
C                                   FIXED VECTOR COMPONENTS WITH RESPECT TO THE
C                                   GEODETIC LATITUDES. (M/RAD)
C       3.  PLON(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
C                                   FIXED VECTOR COMPONENTS WITH RESPECT TO THE
C                                   EAST LONGITUDES. (M/RAD)
C       4.  SITAXO(Max_Stat)     -  THE SITE ANTENNA AXIS OFFSETS. (M)
C       5.  SITOAM(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING AMPLITUDES.
C                                   (M)
C       6.  SITOPH(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING PHASES.
C                                   (RAD)
C       7.  SITXYZ(3,Max_Stat)   -  THE SITE CRUST FIXED X, Y, & Z
C                                   COORDINATES. (M, M, M )
C       8.  SNRM(3,Max_Stat)     -  THE X, Y, AND Z COMPONENTS OF THE SITE
C                                   NORMAL UNIT VECTORS. (UNITLESS)
C       9.  SITZEN(Max_Stat)     -  THE ZENITH ELECTRICAL PATH LENGTH AT EACH
C                                   OBSERVATION SITE. (SEC)
C      10.  TCROT(3,3,Max_Stat)  -  THE ROTATION MATRICES WHICH ROTATE THE
C                                   TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST 
C                                   FIXED REFERENCE SYSTEM FOR EACH SITE. 
C                                   (UNITLESS)
C      11.  XLAT(Max_Stat)       -  THE SITE GEODETIC LATITUDES. (RAD)
C      12.  XLON(Max_Stat)       -  THE SITE EAST LONGITUDES. (RAD)
C      13.  KTYPE(Max_Stat)      -  THE SITE ANTENNA AXIS TYPES. (UNITLESS)
C      14.  NLAST(2)             -  THE INTEGER VARIABLE WHICH DETERMINES IF 
C                                   THE BASELINE ID HAS CHANGED FROM ONE 
C                                   OBSERVATION TO THE NEXT.
C                                   (NOTE: THE SITE GEOMETRY NEED NOT BE 
C                                   RELOADED FOR EACH OBSERVATION IF THE 
C                                   BASELINE ID DOES NOT CHANGE. NLAST MUST BE
C                                   INITIALIZED TO ZERO IN THE INITIALIZATION
C                                   SECTION AND PASSED TO THE GEOMETRY SECTION
C                                   SO THAT IT WILL HAVE ZERO VALUES UNTIL
C                                   AFTER THE FIRST OBSERVATION IS PROCESSED.)
C      15.  NUMSIT               -  THE NUMBER OF SITES IN THE SITE CATALOG.
C      16.  LNSITE(4,Max_Stat)   -  THE EIGHT CHARACTER SITE NAMES OF THE
C                                   SITES IN THE SITE CATALOG. (ALPHAMERIC)
C      17.  SITHOA(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING
C                                   AMPLITUDES. (M)
C      18.  SITHOP(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING PHASES.
C                                   (RAD)
C      19.  HEIGHT(Max_Stat)     -  Height above the geoid. (meters)
C      20.  RTROT(3,3,Max_Stat)  -  The rotation matrices which rotate the 
C                                   'radial-transverse' reference system to the
C                                   crust fixed reference system for each site.
C                                   (Unitless). The 'radial-transverse' ref.
C                                   system is nearly identical to the
C                                   topocentric system. 'Up' is in the radial
C                                   direction from the center of the Earth; 
C                                   'East' is East; and 'North' is perpendicular
C                                   to the radial in the north direction.
C      21.  GLAT(Max_Stat)       -  The geocentric latitude at each site. (rad)
C      22.  Zero_site            -  The site number of the site at the
C                                   geocenter, if there is one in this data set.C                                   For correlator usage. 
*/
