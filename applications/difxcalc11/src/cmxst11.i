!   cmxst.i
!
!***The maximum number of stations can be changed in the following parameter
!***statement and is the only change necessary:
!
      Integer*4 Max_Stat      ! Maximum number of stations in the database
      Parameter(Max_Stat = 41)
!
      Real*8           CFRAD(Max_Stat), PLAT(3,Max_Stat), &
     &                 PLON(3,Max_Stat), SITAXO(Max_Stat), &
     &                 SITOAM(11,Max_Stat), SITOPH(11,Max_Stat), &
     &                 SITXYZ(3,Max_Stat), SNRM(3,Max_Stat), &
     &                 SITZEN(Max_Stat), TCROT(3,3,Max_Stat), &
     &                 XLAT(Max_Stat), XLON(Max_Stat), &
     &                 SITHOA(11,2,Max_Stat), SITHOP(11,2,Max_Stat), &
     &                 HEIGHT(Max_Stat), RTROT(3,3,Max_Stat), &
     &                 GLAT(Max_Stat), Dbtilt(2,Max_Stat), &
     &                 Rotilt(3,3,Max_Stat), OPTL6(6,Max_Stat)
      Integer*2 KTYPE(Max_Stat), NLAST(2), NUMSIT, LNSITE(4,Max_Stat),  &
     &          i3dum
      Integer*4 Zero_site
!
      COMMON / SITCM / CFRAD, PLAT, PLON, SITAXO, SITOAM, SITOPH,       &
     &                 SITXYZ, SNRM, SITZEN, TCROT, XLAT, XLON, SITHOA, &
     &                 SITHOP, HEIGHT, RTROT, GLAT, Dbtilt, Rotilt,     &
     &                 OPTL6, Zero_site, KTYPE, NLAST, LNSITE, NUMSIT
!
!       1. CFRAD(Max_Stat)      -  THE SITE SPHERICAL EARTH RADII.  (M)
!       2. PLAT(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
!                                  FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!                                  GEODETIC LATITUDES. (M/RAD)
!       3. PLON(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
!                                  FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!                                  EAST LONGITUDES. (M/RAD)
!       4. SITAXO(Max_Stat)     -  THE SITE ANTENNA AXIS OFFSETS. (M)
!       5. SITOAM(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING AMPLITUDES.
!                                  (M)
!       6. SITOPH(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING PHASES.
!                                  (RAD)
!       7. SITXYZ(3,Max_Stat)   -  THE SITE CRUST FIXED X, Y, & Z
!                                  COORDINATES. (M, M, M )
!       8. SNRM(3,Max_Stat)     -  THE X, Y, AND Z COMPONENTS OF THE SITE
!                                  NORMAL UNIT VECTORS. (UNITLESS)
!       9. SITZEN(Max_Stat)     -  THE ZENITH ELECTRICAL PATH LENGTH AT EACH
!                                  OBSERVATION SITE. (SEC)
!      10. TCROT(3,3,Max_Stat)  -  THE ROTATION MATRICES WHICH ROTATE THE
!                                  TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST
!                                  FIXED REFERENCE SYSTEM FOR EACH SITE.
!                                  (UNITLESS)
!      11. XLAT(Max_Stat)       -  THE SITE GEODETIC LATITUDES. (RAD)
!      12. XLON(Max_Stat)       -  THE SITE EAST LONGITUDES. (RAD)
!      13. KTYPE(Max_Stat)      -  THE SITE ANTENNA AXIS TYPES. (UNITLESS)
!      14. NLAST(2)             -  THE INTEGER VARIABLE WHICH DETERMINES IF
!                                  THE BASELINE ID HAS CHANGED FROM ONE
!                                  OBSERVATION TO THE NEXT.
!                                  (NOTE: THE SITE GEOMETRY NEED NOT BE
!                                  RELOADED FOR EACH OBSERVATION IF THE
!                                  BASELINE ID DOES NOT CHANGE. NLAST MUST BE
!                                  INITIALIZED TO ZERO IN THE INITIALIZATION
!                                  SECTION AND PASSED TO THE GEOMETRY SECTION
!                                  SO THAT IT WILL HAVE ZERO VALUES UNTIL
!                                  AFTER THE FIRST OBSERVATION IS PROCESSED.)
!      15. NUMSIT               -  THE NUMBER OF SITES IN THE SITE CATALOG.
!      16. LNSITE(4,Max_Stat)   -  THE EIGHT CHARACTER SITE NAMES OF THE
!                                  SITES IN THE SITE CATALOG. (ALPHAMERIC)
!      17. SITHOA(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING
!                                  AMPLITUDES. (M)
!      18. SITHOP(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING PHASES.
!                                  (RAD)
!      19. HEIGHT(Max_Stat)     -  Height above the geoid. (meters)
!      20. RTROT(3,3,Max_Stat)  -  The rotation matrices which rotate the
!                                  'radial-transverse' reference system to the
!                                  crust fixed reference system for each site.
!                                  (Unitless). The 'radial-transverse' ref.
!                                  system is nearly identical to the
!                                  topocentric system. 'Up' is in the radial
!                                  direction from the center of the Earth;
!                                  'East' is East; and 'North' is perpendicular
!                                  to the radial in the north direction.
!      21. GLAT(Max_Stat)       -  The geocentric latitude at each site. (rad)
!      22. Zero_site            -  The site number of the site at the
!                                  geocenter, if there is one in this data
!                                  set. For correlator usage.
!      23. Dbtilt(Max_Stat,2)   -  Antenna fixed axis tilts, in arc-minutes.
!                                  For alt-az mounts, 1 => East tilt,
!                                  2 => North tilt.
!      24. Rotilt(3,3,Max_Stat) -  Rotation matrices representing the antenna
!                                  fixed axis tilts, in the local topocentric
!                                  station frame. X = Up, Y = East, Z = North.
!      25. OPTL6(6,Max_stat)    -  The site ocean pole tide loading
!                                  coefficients, interpolated from the Desai
!                                  lat/lon table.
