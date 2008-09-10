      SUBROUTINE DRIVR
      IMPLICIT None

c - Modifications to extract Site UVWs
c - WEW  27 Jan 1998
c
      include 'situvw.i'
c - END of Modifications


C
C 1.    DRIVR
C
C 1.1   DRIVR PROGRAM SPECIFICATION
C
C 1.1.1 DRIVR is the main calculation subroutine. It calculates the theoretical
C       delays and delay rates, the contributions of each model module to the
C       delays and delay rates, the partials of the delays and delay rates with
C       respect to model module parameters, and the coordinate time at site #1.
C
C 1.1.2 RESTRICTIONS - NONE
C
C 1.1.3 REFERENCES - PEP MANUAL, GREENBOOK, D. ROBERTSON'S THESIS,
C                    P. McCLURES X-DOCUMENT.
C
C 1.2   DRIVR PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - CALL DRIVR
C
C       INPUT VARIABLES  -NONE
C       OUTPUT VARAIBLES - NONE
C
C 1.2.2 COMMON BLOCKS USED - NONE
C
C 1.2.3 PROGRAM SPECIFICATIONS -
C
      REAL*8  epsmd, omega, gastd, fa(5), fad(5), cent, dut1p(2,2)
      Real*8     AXOFF(2), CFBASE(3), CFLAT(3,2), CFLON(3,2), PANGL(2),
     .           CFSITE(3,2), CFSITN(3,2), DEPS(2), DPSI(2), DIONC(2),
     .           DPSID(2), DEPSD(2), EARTH(3,3), EPBASE(3,2), SUN(3,2),
     .           EPLATP(3,2), EPLATV(3,2), EPLONP(3,2), EPLONV(3,2),
     .           EPS(2), EPSITN(3,2), GAST(2), OCEAMP(11,3,2),
     .           OCEPHS(11,3,2), R2000(3,3,3), RDNP(3,3), RN(3,3,2),
     .           RP(3,3,2), RS(3,3,3), RW(3,3), SITEA(3,2), SITEP(3,2),
     .           SITEV(3,2), SITLAT(2), SITLON(2), SITRAD(2), STAR(3),
     .           SUNCU(3), TCTOCF(3,3,2), DATMC(2,2), ZPATH(2),
     .           TIDEP(3,2), TIDEV(3,2), USITEP(3,2), USITEV(3,2),
     .           XLOADP(3,2), XLOADV(3,2), XMOON(3,2), DAXOC(2,2),
     .           POLTDP(3,2), POLTDV(3,2), AZ(2,2), ELEV(2,2),
     .           SITHEIGHT(2), DSTRP(2,2)
       Real*8 AXIS2000(3,2), DAXIS2000(3,2), STAR_ABERRATED(3,2),
     .        dATMCdh(2,2)
       Real*8 UTC, XJD, AT, DUTCAT, CT, DATDCT, DLPGR, DUT1AT, UT1,
     .        EPSMNR, DIURNV, GMST, WOBXR, WOBYR, CD, CRA, SD, SRA
       Integer*2 KAXIS(2)
C
C 1.2.3.1   SAVE BLOCK -
C
       SAVE GAST, R2000, RDNP, RN, RP, RS, RW, SITEA, SITEP,
     .      SITEV, SITLAT, STAR, SUNCU, TCTOCF, TIDEP, SITRAD,
     .      TIDEV, XLOADP, XLOADV, ZPATH, DEPS, DPSI, EPS, DSTRP,
     .      POLTDP, POLTDV, SUN, AXOFF, CFBASE, DIURNV, DLPGR, EPBASE,
     .      CFSITE, CFSITN, CFLON, CFLAT, SITLON, OCEAMP, OCEPHS,
     .      PANGL, AZ, ELEV, KAXIS, EARTH, EPSMNR, STAR_ABERRATED,
     .      epsmd, omega, gastd, fa, fad, dut1p, XMOON, SITHEIGHT
C
C 1.2.4 DATA BASE ACCESS - NONE
C
C 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
C
C 1.2.6 SUBROUTINE INTERFACE -
C
C             CALLER SUBROUTINES: MAIN
C
C             CALLED SUBROUTINES: INITL, OBSNT, START, TOCUP, WRIDR,
C                                 ATIME, ATMG, AXOG, CTIMG, RMPAR,
C                                 DIURNL, ETDG, M2000, NUTG, OCEG, PANG,
C                                 PEP, PREG, PTDG, RELG, ROSITE, SITG,
C                                 SITCOR, STRCOR, STRG, SUNCOR, UT1G,
C                                 UTCTME, WOBG, PLXG, ATMP, AXOP, ETDP,
C                                 NUTP, OCEP, PREP, RELP, SITP, STRP,
C                                 UT1P, WOBP, PLXP, PTDP, ATMC, AXOC,
C                                 ETDC, OCEC, PTDC, RELC, CSTAR, WOBC
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - NONE
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES -
C
C             1. AXOFF(2)      - THE ANTENNA AXIS OFFSETS AT EACH OBSERVATION 
C                                SITE.  (M)
C
C             2. CFBASE(3)     - THE GEOCENTRIC CRUST FIXED BASELINE VECTOR. (M)
C
C             3. DATMC(2,2)    - THE CONTRIBUTIONS TO THE DELAY AND DELAY 
C                                RATE DUE TO TROPOSPHERIC REFRACTION AT EACH
C                                OBSERVATION SITE. (SEC, SEC/SEC)
C
C             4. DAXOC(2,2)    - THE CONTRIBUTIONS TO THE DELAY AND DELAY 
C                                RATE DUE TO THE ANTENNA AXIS OFFSETS AT EACH
C                                OBSERVATION SITE. (SEC, SEC/SEC)
C
C             5. DEPS(2)       - THE NUTATION IN OBLIQUITY AND ITS CT TIME
C                                DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM
C                                THE DATA BASE. (RAD, RAD/SEC)
C
C             6. DIONC(2)      - THE CONTRIBUTIONS TO THE DELAY AND DELAY
C                                RATE DUE TO IONOSPHERE EFFECTS. (SEC, SEC/SEC)
C
C             7. DIURNV        - THE DIURNAL ANGULAR VELOCITY OF THE EARTH.
C                                (RAD/SEC)
C
C             8. DLPGR         - THE CT TIME DERIVATIVE OF THE LONG PERIOD 
C                                TERMS IN THE 'AT MINUS CT' OFFSET. (SEC/SEC)
C
C             9. DPSI(2)       - THE NUTATION IN LONGITUDE AND ITS CT TIME 
C                                DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM THE
C                                DATA BASE. (RAD, RAD/SEC)
C
C            10. EARTH(3,3)    - THE SOLAR SYSTEM BARYCENTRIC EARTH POSITION, 
C                                VELOCITY, AND ACCELERATION VECTORS. 
C                                (M, M/SEC, M/SEC**2)
C
C            11. EPBASE(3,2)   - THE 2000.0 GEOCENTRIC BASELINE POSITION AND
C                                VELOCITY VECTORS. (M, M/SEC)
C
C            12. EPS(2)        - THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS CT
C                                TIME DERIVATIVE. (RAD, RAD/SEC)
C
C            13. EPSMNR        - MEAN OBLIQUITY AT EPOCH J2000.0. (RAD)
C
C            14. epsmd         - mean obliquity of date (radians)
C
C            15. fa(5)         - fundamental arguments (see NUTFA)
C
C            16. GAST(2)       - THE GREENWICH APPARENT SIDEREAL TIME AND ITS CT
C                                TIME DERIVATIVE. (RAD, RAD/SEC)
C
C            17. KEND          - THE 'END OF DATA' FLAG. KEND = 0 IF THERE IS
C                                MORE DATA TO BE PROCESSED. KEND = 1 IF THE END
C                                OF THE DATA HAS BEEN REACHED.
C
C            18. KOUNT         - THE FLAG WHICH INITIALIZES THE COUNTING OF THE
C                                OBSERVATION ITEMS.
C
C            19. PANGL(2)      - THE PARALLACTIC ANGLE DUE TO FEED BOX ROTATION
C                                AT EACH OBSERVATION SITE. (RAD)
C
C            20. POLTDP(3,2)   - GEOCENTRIC J2000.0 SITE POSITION CORRECTION FOR
C                                THE EFFECTS OF THE POLE TIDE. (M)
C
C            21. POLTDV(3,2)   - GEOCENTRIC J2000.0 SITE VELOCITY CORRECTION FOR
C                                THE EFFECTS OF THE POLE TIDE. (M/SEC)
C
C            22. R2000(3,3,3)  - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION 
C                                MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
C                                (UNITLESS, 1/SEC, 1/SEC**2)
C
C            23. RDNP(3,3)     - THE DIURNAL POLAR MOTION PORTION OF THE 
C                                COMPLETE CRUST FIXED TO J2000.0 ROTATION
C                                MATRIX. (UNITLESS)
C
C            24. RN(3,3,2)     - THE NUTATION PORTION OF THE COMPLETE CRUST 
C                                FIXED TO J2000.0 ROTATION MATRIX
C                                AND THE CT TIME DERIVATIVE OF THAT
C                                MATRIX. (UNITLESS, 1/SEC)
C
C            25. RP(3,3,2)     - THE PRECESSION PORTION OF THE COMPLETE CRUST
C                                FIXED TO J2000.0 ROTATION MATRIX AND THE CT 
C                                TIME DERIVATIVE OF THAT MATRIX. 
C                                (UNITLESS, 1/SEC)
C
C            26. RS(3,3,3)     - THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
C                                FIXED TO J2000.0 ROTATION MATRIX AND THE FIRST
C                                TWO CT TIME DERIVATIVES OF THAT MATRIX.
C                                (UNITLESS, 1/SEC, 1/SEC**2)
C
C            27. RW(3,3)       - THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED
C                                TO J2000.0 ROTATION MATRIX. (UNITLESS)
C
C            28. SITEA(3,2)    - THE J2000.0 GEOCENTRIC ACCELERATION VECTORS OF
C                                EACH OBSERVATION SITE. (M/SEC**2)
C
C            29. SITEP(3,2)    - THE J2000.0 GEOCENTRIC POSITION VECTORS OF EACH
C                                OBSERVATION SITE. (M)
C
C            30. SITEV(3,2)    - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF EACH
C                                OBSERVATION SITE. (M/SEC)
C
C            31. SITLAT(2)     - THE SITE GEODETIC LATITUDES. (RAD)
C            32. SITLON(2)     - The site East longitudes. (rad)
C            33. SITLAT(2)     - The site heights above the geoid. (m)
C
C            34. STAR(3)       - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C
C            35. SUN(3,2)      - THE J2000.0 GEOCENTRIC SUN POSITION AND
C                                VELOCITY VECTORS. (M, M/SEC)
C
C            36. TCTOCF(3,3,2) - THE ROTATION MATRIX WHICH ROTATES THE 
C                                TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST FIXED
C                                REFERENCE SYSTEM AT EACH OBSERVATION SITE.
C
C            37. TIDEP(3,2)    - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                POSITION VECTORS DUE TO EARTH TIDE EFFECTS. (M)
C
C            38. TIDEV(3,2)    - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                VELOCITY VECTORS DUE TO EARTH TIDES. (M/SEC)
C
C            39. WOBX          - THE LONG PERIOD WOBBLE X-OFFSET. (RAD)
C
C            40. WOBY          - THE LONG PERIOD WOBBLE Y-OFFSET. (RAD)
C                                (NOTE: WOBY IS LEFT HANDED.)
C
C            41. XLOADP(3,2)   - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                POSITION VECTORS DUE TO OCEAN LOADING. (M)
C
C            42. XLOADV(3,2)   - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                VELOCTY VECTORS DUE TO OCEAN LOADING. (M/SEC)
C
C            43. ZPATH(2)      - THE ZENITH ELECTRICAL PATH LENGTH AT EACH
C                                OBSERVATION SITE. (SEC)
C
C            44. STAR_ABERRATED(3,2) - THE J2000.0 SOURCE UNIT VECTOR AT EACH
C                                SITE CORRECTED FOR ABERRATION. (UNITLESS)
C
C            45. axis2000(3,2) -  Vector axis offset of antenna in the J2000.0
C                                 frame (effect on baseline). First index is
C                                 X,Y,Z (meters), second runs over sites.
C
C            46. daxis2000(3,2) - Time derivative of axis2000, rate of change
C                                 of vector axis offset of antenna in the
C                                 J2000.0 frame (effect on baseline). First
C                                 index is velocity, second runs over sites.
C
C            47. ELEV(2,2)      - The elevation angle of the source corrrected
C                                 for aberration and its CT time derivative at
C                                 each site (rad,rad/sec)
C
C            48. AZ(2,2)        - The azimuth angle of the source corrrected
C                                 for aberration and its CT time derivative
C                                 at each site (rad,rad/sec)
C
C            49. DSTRP(2,2)     - Partial derivatives of the delay and delay
C                                 rate with respect to source RA and Dec. First
C                                 runs over RA and Dec, second runs over delay
C                                 and delay rate. (sec/rad, sec/sec-rad
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/12/77
C                    DALE MARKHAM  02/16/77
C                    KATHY WATTS   03/28/77
C                    PETER DENATALE 07/07/77
C                    BRUCE SCHUPLER 05/11/78
C                    BRUCE SCHUPLER 12/05/78
C                    BRUCE SCHUPLER 02/01/79
C                    BRUCE SCHUPLER 01/07/80
C                    BRUCE SCHUPLER 08/26/80
C                    CHOPO MA 08/03/81
C                    HAROLD M. SCHUH 10/08/83
C                    GEORGE KAPLAN   ????????
C                    CHOPO MA / DAVID GORDON 04/09/84
C                    DAVID GORDON 05/15/84
C                    DAVID GORDON 06/11/84
C                    JIM RYAN  06/20/84     (OCEAN LOADING)
C                    DAVID GORDON 07/12/84  (POLE TIDE)
C                    DAVID GORDON 07/18/84  (K1 DISPLACEMENT TIDE)
C                    DAVID GORDON 08/30/84  (CHANGED CALL TO OCEG)
C                    DAVID GORDON 01/03/85  (REMOVED POLTDP & POLTDV
C                                            FROM CALL PTDC)
C                    DAVID GORDON 01/08/85  (ADDED IDISC)
C                    SAVITA GOEL  06/04/87  (CDS FOR A900)
C                    GREGG COOKE  12/21/88  (CONSOLIDATED DRIVERS)
C                    LOTHAR MOHLMANN 03/23/89 (CHANGED SITG, OCEG)
C                    GREGG COOKE  05/22/89    (ADDED PANC)
C                    89.07.25 Jim Ryan Documentation simplified
C                    89.10.08 All code relating to computing a perturbed
C                             source positon deleted.  Logic changed for
C                             Shapiro (89) algorithm.
c                    91.10.05 Jim Ryan Arrary EARTH passed to ATMG for
c                             aberration computation.
c                    91.11.25: jwr  The array EARTH added to the call to
c                             SITP, STRP, UT1P, and WOBP.
C                    93MAY    CONSEN added, AXOG & ATMG calls modified, etc.
C                    93.10.07: NZ/DG, added call to DIRNC, new equation of 
C                              equinox's contribution (IERS note 13)
C                    94.01.07  D. Gordon XMOON added to SAVE block, needed 
C                              (along with EARTH and SUN) in PEP to allow 
C                              reusing solar system info if obs. time doesn't
C                              change.
C                    94.04.13  D. Gordon Converted to Implicit None.
C                    94.06.08  D. Gordon Removed unused variable 'TRHOHF' from
C                              Save block
C                    94.09.21  D. Gordon Added SITHEIGHT(2) to AXOG argument
C                              list.
C                    94.10.05  D. Gordon Removed unused arguments from call to
C                              THERY
C                    94.10.24  D. Gordon Removed unused arguments from calls to
C                              ATMG and AXOG.
C                    95.05.02  D. Gordon  DSTRP(2,2) added, put in SAVE block;
C                              added SUN and DSTRP to subroutine PLXP argument
C                              list; added DSTRP to subroutine STRP argument
C                              list.
C
C     PROGRAM STRUCTURE
C
C     Perform the geometry and time calculations.
C     The basic coordinate system is referenced to the Epoch of 2000.0
C     and is a right-handed Cartesian system oriented to the mean
C     celestial pole and mean equator of that epoch.  The nominal
C     origin is the solar system barycenter.  There is also an earth
C     fixed coordinate system which is a right-handed Cartesian system
C     oriented to the mean geographic pole of 1900-1906 and the Green-
C     wich Meridian.  The nominal origin is the Earth's center of
C     mass.  The basic unit of time is the coordinate second as used
C     by the PEP Tape.  UTC, AT, AND UT1 are also used.
C     The geometry of the observation is calculated with an accuracy
C     goal of 0.1 picoseconds of delay.  In doing the calculations
C     for the geometry, much of the work neccesary for the computation
C     of model contributions to delay and delay rate and partials of
C     delay and delay rate with respect to model parameters is also
C     done.  Matrices which represent coordinate rotations
C     ( precession, nutation, diurnal spin, diurnal polar motion,
C     and wobble ) and their CT time derivatives are stored as
C     (3,3,N) arrays, where N indixes the N-1'th time derivative.
C     The subroutines suffixed G are sections of model modules.
C     The other subroutines my be considered utilities and either
C     superseed or incorporate many present PEP routines.
C
C     Call SITG for the geographical site data.  SITG provides the
C     following geocentric information for each observing site:
C     the antenna axis offsets (AXOFF), the antenna types (KAXIS),
C     the crust fixed site vectors (CFSITE), the crust fixed
C     baseline vector (CFBASE), the crust fixed site normal unit
C     vectors (CFSITN), the geodetic latitudes (SITLAT), the
C     site east longitudes (SITLON), the spherical earth radii,
C     the partial derivatives of the crust fixed site vector
C     components with respect to the geodetic latitudes (CFLAT),
C     and the east longitudes (CFLON), the rotation matrices which
C     rotate the topocentric site reference system to the geocentric
C     system at each site (TCTOCF), and the zenith tropospheric path
C     delays at each observing site. SITG is the only routine which
C     'knows' which two sites are involved in the observation.
C     All other routines mearly work wiht site#1 and site#2.
C
      CALL SITG  (AXOFF, CFBASE, CFLAT, CFLON, CFSITE, CFSITN,
     .            KAXIS, OCEAMP, OCEPHS, SITLAT, SITLON, SITRAD,
     .            TCTOCF, ZPATH, SITHEIGHT)
C
C     Call STRG for the 2000.0 unit vector in the direction of the
C     radio source. (STAR)
      CALL STRG  (STAR)
C
C     Call UTCTM for the UTC time fraction of the UTC day (UTC) and for 
C     the Julian Date at zero hours UTC of the date in question (XJD).
      CALL UTCTM ( UTC, XJD )
C
C     Call ATIME for the atomic time fraction of the atomic time
C     day (AT) and for the partial derivative of the UTC time with
C     respect to the atomic time (DUTCAT).
      CALL ATIME ( UTC, XJD, AT, DUTCAT )
C
C     Call CTIMG for the coordinate time fraction of the coordinate
C     time day AT site#1 (CT), the partial derivative of the
C     atomic time with respect to the coordinate time (DATDCT),
C     and the partial derivative of the long period terms in the
C     'AT minus CT' offset with respect to the coordinate time (DLPGR).
      CALL CTIMG (AT,CFSITE,SITLON,UTC,XJD,CT,DATDCT,DLPGR)
C
C     Call PEP for the J2000.0 geocentric Sun (SUN) and Moon (XMOON) position
C     and velocity vectors; the J2000.0 solar system barycentric Earth 
C     position, velocity, and acceleration vectors (EARTH); the other planets'
C     (except Pluto) barycentric and geocentric positions and velocities; the
C     (Woolard) nutation in longitude (DPSID) and obliquity (DEPSD). The solar
C     system info comes from the DE/LE2000 JPL Ephemeris by default. The
C     nutation stuff comes from the database. Note that for the default case
C     the Wahr, rather than these Woolard nutations are used.
      CALL PEP   (XJD, CT, DEPSD,DPSID,EARTH,SUN,XMOON)
C
C     Call NUTFA before NUTG and before UT1G to get epoch in centuries
C     and fundamental arguments for nutation series
      CALL NUTFA (xjd, ct, cent, fa,fad)
C
C     Call UT1G for the UT1 fraction of the UT1 day (UT1) and for the partial
C     derivative of the UT1 time with respect to the atomic time (DUT1AT).
      CALL UT1G (AT,DUTCAT,UTC,XJD,CT,fa,fad,cent,DUT1AT,UT1)
C
C     Call NUTG for the nutation portion of the component crust fixed
C     to J2000.0 rotation matrix and its CT time derivative (RN)
C     and the true obliquity of the ecliptic and its CT time
C     derivative (EPS) and the mean obliquity at J2000.0 (EPSMNR).
      CALL NUTG  (DEPSD,DPSID,cent,fa,fad,    ! input
     .            DEPS,DPSI,EPS,EPSMNR,RN)    ! output

C     compute mean obliquity of date, req. for EQE update in DIRNL
C**   epsmd = eps (1) - deps (1)

C     Call PREG for the precession portion of the complete crust fixed
C     to 2000.0 rotation matrix and its CT time derivative (RP).
      CALL PREG  ( CT, EPSMNR, XJD, RP )
C
C     Call DIRNL for the diurnal spin portion of the complete crust fixed
C     to J2000.0 rotation matrix and its first two CT time derivatives (RS),
C     the Greenwich apparent siderial time and it CT time derivative (GAST),
C     the Greenwich mean siderial time (GMST), and the diurnal rotation
C     velocity of the Earth (DIURNV), and the difference in GAST according 
C     to two versions of the equation of the equinoxes (gastd).
      CALL DIRNL (DATDCT,DPSI,DUT1AT,EPS,DEPS,FA,UT1,XJD, ! input
     .            DIURNV,GAST,GMST,gastd,RS)                  ! output
C
C     Call WOBG for the wobble portion of the complete crust fixed
C     to 2000.0 rotation matrix (RW), and the long period wobble
C     X and Y OFFSETS. (NOTE: Right-handed coordinate system.)
      CALL WOBG  (UTC, XJD, RW, WOBXR, WOBYR )
C
C     Set the diurnal polar motion matrix to unity. (This effect is now
C     obsolete.)
      CALL ROTAT ( 0.D0, 3, RDNP)
C
C     Call M2000 to compute the complete curst fixed to 2000.0
C     rotation matrix and its first two CT time derivatives.
      CALL M2000 ( RDNP, RN, RP, RS, RW, R2000 )
C
C     Call ROSIT to rotate the crust fixed site data into
C     the 2000.0 inertial reference system.  The following
C     variables are output for each observing site in 2000.0
C     coordinates: the site position vectors (USITEP) and velocity
C     vectors (USITEV) uncorrected for earth tidal and ocean loading
C     effects, the site accelerations (SITEA), the site normal unit
C     vectors (EPSITN), the partial derivatives of the site position
C     and velocity vector components with respect to the site
C     geodetic latitudes ( (EPLATP), (EPLATV) ) and with respect
C     to the site east longitudes'' (EPLONP), (EPLONV) ).
      CALL ROSIT (CFLAT,CFLON,CFSITE,CFSITN,R2000,
     .              EPLATP, EPLATV, EPLONP, EPLONV, EPSITN,
     .              SITEA, USITEP, USITEV )
C
C     Call ETDG for the corrections to the J2000 site position vectors (TIDEP)
C     and velocity vectors (TIDEV) due to Earth tide effects.
      CALL ETDG  (EPLATP,EPLATV,EPLONP,EPLONV,R2000,
     .     SITLAT,SITLON,SUN,TCTOCF,USITEP,USITEV,XMOON,
     .     EARTH,GAST,STAR,fa,fad,cent,TIDEP,TIDEV)
C
C     Call 'PTDG' for the corrections to the J2000.0 site position and site
C     velocity vector due to the solid Earth pole tide.
      CALL PTDG  (SITLAT, SITLON, SITRAD, WOBXR, WOBYR, DIURNV,
     .            TCTOCF, R2000, POLTDP, POLTDV )
C
C     Call OCEG for the corrections to the J2000.0 site position vectors
C     (XLOADP) and velocity vectors (XLOADV) due to ocean loading effects.
      CALL OCEG  (CFSITE,UT1,OCEAMP,OCEPHS,R2000,XJD,TCTOCF,
     .            XLOADP,XLOADV)
C
C     Call SITCR to apply the Earth tide, ocean loading, and pole tide
C     corrections to the J2000.0 site position vectors (SITEP), site velocity
C     vectors (SITEV), and the J2000.0 baseline position and velocity vectors
C     (EPBASE).
      CALL SITCR (TIDEP,TIDEV,USITEP,USITEV,XLOADP,
     .              XLOADV,EPBASE,SITEP,SITEV,POLTDP,POLTDV)
C
C     Call ATMG for the aberrated elevation and azimuth angles of the source and
C     their CT time derivatives, and the aberrated source unit vector.
      CALL ATMG  (R2000, STAR, EARTH, TCTOCF, SITEV, AZ, ELEV,
     .            STAR_ABERRATED )
C
C     Call AXOG for the J2000.0 vector axis offsets of the antennas and their
C     time derivatives at each site.
      CALL AXOG  (KAXIS, R2000, SITLAT, STAR, TCTOCF, SITEV, AXOFF,
     1            EARTH, AZ, ELEV, STAR_ABERRATED,
     2            SITHEIGHT, AXIS2000, DAXIS2000 )
C
C     Call PANG to compute the parallactic angle (feed-box rotation angle)
C     for each site.
      CALL PANG (AZ, ELEV, STAR, SITLAT, KAXIS, PANGL )
C
C     Call PLXG tp compute the parallax goemetry.
      CALL PLXG
C
C     Perform the partial derivatives calculations. The partials are calculated
C     using the exact geometry wherever practical. Each section Put's its 
C     partials into the observation item using the database handler. The calling
C     sequence for each module has the form: Call <MOD>P(...) where ... are the
C     variables passed from the geometry section of DRIVR needed to calculate
C     the partials. All of the subroutines are parts of model modules. Note that
C     the relativity partials are now in the THERY subroutine.
C
C     Compute the atmosphere partials.
      CALL ATMP (ELEV, SITLAT, SITHEIGHT, XJD, CT, dATMCdh)
C
C     Compute the axis offset partials.
      CALL AXOP (AXOFF, dATMCdh )
C
C     Compute the Earth tide partials.
      CALL ETDP  (R2000, SITLAT, STAR, TCTOCF)
C
C     Compute the pole tide partials (STUB ONLY)
      CALL PTDP
C
C     Compute the nutation partials.
      CALL NUTP (CFBASE,DEPS,DPSI,EPS,GAST,RDNP,RN,RP,RS,RW,STAR)
C
C     Compute the ocean loading partials.
      CALL OCEP
C
C     Compute the precession partials.
      CALL PREP (CFBASE, EPSMNR, RDNP, RN, RS, RW, STAR )
C
C     Compute the site partials.
      CALL SITP  (R2000, STAR ,EARTH)
C
C     Compute the star partials.
      CALL STRP  (EPBASE, STAR, EARTH, DSTRP, CD, CRA, SD, SRA )
C
C     Compute the UT1 partials.
      CALL UT1P  (CFBASE,DIURNV,GAST,RDNP,RN,RP,RW,STAR,EARTH, ! input
     .            DUT1P)                                       ! out
C
C     Compute the wobble partials.
      CALL WOBP  (CFBASE, RDNP, RN, RP, RS, STAR ,EARTH)
C
C     Compute the parallax partials.
      CALL PLXP  (SUN, DSTRP, CD, CRA, SD, SRA)
C
C     Perform the contributions calculations.
C
C     The individual module contributions are calcualted so that they
C     may be removed in the program 'SOLVE' if desired. Some of the
C     routines are essentially dummies returning values passed from
C     the observation item. However, they are included to retain the
C     capability of using other models. In several cases the
C     contributions can be calculated to first order with sufficient
C     accuracy using the partial derivatives of the dalays and rates
C     with respect to the model module parameters. Each section will Put
C     its contributions into the observation item using the database handler.
C     Note that the relativity contributions are now in subroutine THERY.
C
C     Compute the atmosphere contributions.
      CALL ATMC  (ZPATH, DATMC )
C
C     Compute the axis offset contributions.
      CALL AXOC  (AXOFF, DAXOC )
C
C     Compute the earth tide contributions.
      CALL ETDC  (TIDEP, TIDEV, STAR )
C
C     Compute the pole tide contributions.
      CALL PTDC  ( STAR )
C
C     Zero out the ionosphere contribution.
      DIONC(1) = 0.D0
      DIONC(2) = 0.D0
C
C     Compute the ocean loading contributions.
      CALL OCEC  ( STAR )
C
C     Compute the wobble contributions.
      CALL WOBC
C
C     Compute contribution for new equation of equinox which needs to be added
C     in case the new version will be selected in SOLVE (flybyb-option)
C     CALL EQEC (GASTD, DUT1P)
      CALL DIRNC (GASTD, DUT1P)

C     Perform the calculation for the complete theoretical delay and rate.
C     Also do all the work of all elements of the Relativity Module,
C     including the contributions and partials. This includes the Shapiro, 
C     the Hellings, and the Consensus relativity models computations:
      CALL THERY (DATMC,DAXOC,DIONC,DLPGR,EARTH,EPBASE,
     .            SITEP,SITEV,SITEA,SUN,STAR,XMOON,AT )

c - Modifications to extract Site UVWs and UVWrates
c - WEW  27 Jan 1998
c
      uvwp(1) =    -sra*sitep(1,2) +    cra*sitep(2,2)
      uvwp(2) = -cra*sd*sitep(1,2) - sra*sd*sitep(2,2) + cd*sitep(3,2)
      uvwp(3) =  cra*cd*sitep(1,2) + sra*cd*sitep(2,2) + sd*sitep(3,2)

      uvwv(1) =    -sra*sitev(1,2) +    cra*sitev(2,2)
      uvwv(2) = -cra*sd*sitev(1,2) - sra*sd*sitev(2,2) + cd*sitev(3,2)
      uvwv(3) =  cra*cd*sitev(1,2) + sra*cd*sitev(2,2) + sd*sitev(3,2)

c - END of Modifications


C
C     Go back to the main.
      RETURN
      END
