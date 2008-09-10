C   cmxst.i
C
C***The maximum number of stations can be changed in the following parameter
C***statement and is the only change necessary:
C
      Integer*2 Max_Stat      ! Maximum number of stations in the database
      Parameter(Max_Stat = 24)
C
      Real*8           CFRAD(Max_Stat), PLAT(3,Max_Stat), 
     1                 PLON(3,Max_Stat), SITAXO(Max_Stat), 
     2                 SITOAM(11,Max_Stat), SITOPH(11,Max_Stat),
     3                 SITXYZ(3,Max_Stat), SNRM(3,Max_Stat),
     4                 SITZEN(Max_Stat), TCROT(3,3,Max_Stat),
     5                 XLAT(Max_Stat), XLON(Max_Stat), 
     7                 SITHOA(11,2,Max_Stat), SITHOP(11,2,Max_Stat),
     8                 HEIGHT(Max_Stat), RTROT(3,3,Max_Stat),
     9                 GLAT(Max_Stat)
      Integer*2 KTYPE(Max_Stat), NLAST(2), NUMSIT, LNSITE(4,Max_Stat),
     *          i3dum
      Integer*4 Zero_site 
C
      COMMON / SITCM / CFRAD, PLAT, PLON, SITAXO, SITOAM, SITOPH,
     .                 SITXYZ, SNRM, SITZEN, TCROT, XLAT, XLON, SITHOA,
     .                 SITHOP, HEIGHT, RTROT, GLAT, Zero_site, 
     .                 KTYPE, NLAST, LNSITE, NUMSIT, i3dum
C
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
