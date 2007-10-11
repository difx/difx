      SUBROUTINE DRIVR(BSLN,SRC,EARTHX,SRCELEV)
      IMPLICIT None
C
C-VLBA  DRIVR has been modified to support the VLBA Correlator.
C-VLBA  Added arguments BSLN, SRC, EARTHX, SRCELEv
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
      REAL*8 epsmd, omega, gastd, fa(5), fad(5), cent, dut1p(2,2)
      Real*8 AXOFF(2), CFBASE(3), CFLAT(3,2), CFLON(3,2), PANGL(2),
     .       CFSITE(3,2), CFSITN(3,2), DEPS(2), DPSI(2), DIONC(2),
     .       DPSID(2), DEPSD(2), EARTH(3,3), EPBASE(3,2), SUN(3,2),
     .       EPLATP(3,2), EPLATV(3,2), EPLONP(3,2), EPLONV(3,2),
     .       EPS(2), EPSITN(3,2), GAST(2), OCEAMP(11,3,2),
     .       OCEPHS(11,3,2), R2000(3,3,3), RDNP(3,3), RN(3,3,2),
     .       RP(3,3,2), RS(3,3,3), RW(3,3,2),SITEA(3,2),SITEP(3,2),
     .       SITEV(3,2), SITLAT(2), SITLON(2), SITRAD(2), STAR(3),
     .       SUNCU(3), TCTOCF(3,3,2), DATMC(2,2), ZPATH(2),
     .       TIDEP(3,2), TIDEV(3,2), USITEP(3,2), USITEV(3,2),
     .       XLOADP(3,2), XLOADV(3,2), XMOON(3,2), DAXOC(2,2),
     .       POLTDP(3,2), POLTDV(3,2), AZ(2,2), ELEV(2,2),
     .       SITHEIGHT(2), DSTRP(2,2), RTTOCF(3,3,2), GEOLAT(2)
      Real*8 AXIS2000(3,2), DAXIS2000(3,2), STAR_ABERRATED(3,2),
     .        dATMCdh(2,2)
C-VLBA
      Real*8 BSLN(3,2), SRC(3), EARTHX(3,3), SRCELEV(2,2)
      Integer*2 I, J
C-VLBA
      Real*8 UTC, XJD, AT, DUTCAT, CT, DATDCT, DLPGR, DUT1AT, UT1,
     .       EPSMNR, DIURNV, GMST, WOBXR, WOBYR, CD, CRA, SD, SRA,
     .       NUTDIF(2,2), DNUTP(2,2), SJD, TJD, OBSDIF, FUKU(2)
      Integer*2 KAXIS(2)
      Integer*4 TSKIP 
C
      DATA SJD /-999.D6/
C
C 1.2.3.1   SAVE BLOCK - 
       SAVE GAST, R2000, RDNP, RN, RP, RS, RW, SITEA, SITEP, 
     .      SITEV, SITLAT, STAR, SUNCU, TCTOCF, TIDEP, SITRAD, 
     .      TIDEV, XLOADP, XLOADV, ZPATH, DEPS, DPSI, EPS, DSTRP, 
     .      POLTDP, POLTDV, SUN, AXOFF, CFBASE, DIURNV, DLPGR, EPBASE,
     .      CFSITE, CFSITN, CFLON, CFLAT, SITLON, OCEAMP, OCEPHS,
     .      PANGL, AZ, ELEV, KAXIS, EARTH, EPSMNR, STAR_ABERRATED,
     .      EPSMD, OMEGA, GASTD, FA, FAD, DUT1P, XMOON, SITHEIGHT,
     .      NUTDIF, XJD, CT, SJD, TJD, OBSDIF, CENT, UT1, DUT1AT,
     .      RTTOCF, GEOLAT,    WOBXR, WOBYR, UTC, AT, DUTCAT, DATDCT,
     .      GMST, FUKU
C
C 1.2.4 DATA BASE ACCESS - NONE
C
C 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
C
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: MAIN
C             CALLED SUBROUTINES: INITL, OBSNT, START, TOCUP, WRIDR,
C                                 ATIME, ATMG, AXOG, CTIMG, RMPAR,
C                                 DIURNL, ETDG, M2000, NUTG, OCEG, 
C                                 PEP, PREG, PTDG, RELG, ROSITE, SITG,
C                                 SITCOR, STRCOR, STRG, SUNCOR, UT1G,
C                                 UTCTME, WOBG, PLXG, ATMP, AXOP, ETDP,
C                                 NUTP, OCEP, PREP, RELP, SITP, STRP,
C                                 UT1P, WOBP, PLXP, PTDP, ATMC, AXOC,
C                                 ETDC, OCEC, PTDC, RELC, CSTAR, WOBC
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES -
C             1. AXOFF(2)      - THE ANTENNA AXIS OFFSETS AT EACH OBSERVATION 
C                                SITE.  (M)
C             2. CFBASE(3)     - THE GEOCENTRIC CRUST FIXED BASELINE VECTOR. (M)
C             3. DATMC(2,2)    - THE CONTRIBUTIONS TO THE DELAY AND DELAY 
C                                RATE DUE TO TROPOSPHERIC REFRACTION AT EACH
C                                OBSERVATION SITE. (SEC, SEC/SEC)
C             4. DAXOC(2,2)    - THE CONTRIBUTIONS TO THE DELAY AND DELAY 
C                                RATE DUE TO THE ANTENNA AXIS OFFSETS AT EACH
C                                OBSERVATION SITE. (SEC, SEC/SEC)
C             5. DEPS(2)       - THE NUTATION IN OBLIQUITY AND ITS CT TIME
C                                DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM
C                                THE DATA BASE. (RAD, RAD/SEC)
C             6. DIONC(2)      - THE CONTRIBUTIONS TO THE DELAY AND DELAY
C                                RATE DUE TO IONOSPHERE EFFECTS. (SEC, SEC/SEC)
C             7. DIURNV        - THE DIURNAL ANGULAR VELOCITY OF THE EARTH.
C                                (RAD/SEC)
C             8. DLPGR         - THE CT TIME DERIVATIVE OF THE LONG PERIOD 
C                                TERMS IN THE 'AT MINUS CT' OFFSET. (SEC/SEC)
C             9. DPSI(2)       - THE NUTATION IN LONGITUDE AND ITS CT TIME 
C                                DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM THE
C                                DATA BASE. (RAD, RAD/SEC)
C            10. EARTH(3,3)    - THE SOLAR SYSTEM BARYCENTRIC EARTH POSITION, 
C                                VELOCITY, AND ACCELERATION VECTORS. 
C                                (M, M/SEC, M/SEC**2)
C            11. EPBASE(3,2)   - THE J2000.0 GEOCENTRIC BASELINE POSITION AND
C                                VELOCITY VECTORS. (M, M/SEC)
C            12. EPS(2)        - THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS CT
C                                TIME DERIVATIVE. (RAD, RAD/SEC)
C            13. EPSMNR        - MEAN OBLIQUITY AT EPOCH J2000.0. (RAD)
C            14. epsmd         - Mean obliquity of date (radians)
C            15. fa(5)         - Fundamental arguments (see NUTFA)
C            16. fad(5)        - Time derivative of the fundamental arguments
C                                (see NUTFA)
C            17. cent          - Number of Julian centuries elapsed since the
C                                epoch January 1.5, 2000. (centuries)
C            18. GAST(2)       - THE GREENWICH APPARENT SIDEREAL TIME AND ITS CT
C                                TIME DERIVATIVE. (RAD, RAD/SEC)
C            19. KEND          - THE 'END OF DATA' FLAG. KEND = 0 IF THERE IS
C                                MORE DATA TO BE PROCESSED. KEND = 1 IF THE END
C                                OF THE DATA HAS BEEN REACHED.
C            20. KOUNT         - THE FLAG WHICH INITIALIZES THE COUNTING OF THE
C                                OBSERVATION ITEMS.
C            21. PANGL(2)      - THE PARALLACTIC ANGLE DUE TO FEED BOX ROTATION
C                                AT EACH OBSERVATION SITE. (RAD)
C            22. POLTDP(3,2)   - GEOCENTRIC J2000.0 SITE POSITION CORRECTION FOR
C                                THE EFFECTS OF THE POLE TIDE. (M)
C            23. POLTDV(3,2)   - GEOCENTRIC J2000.0 SITE VELOCITY CORRECTION FOR
C                                THE EFFECTS OF THE POLE TIDE. (M/SEC)
C            24. R2000(3,3,3)  - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION 
C                                MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
C                                (UNITLESS, 1/SEC, 1/SEC**2)
C            25. RDNP(3,3)     - THE DIURNAL POLAR MOTION PORTION OF THE 
C                                COMPLETE CRUST FIXED TO J2000.0 ROTATION
C                                MATRIX. (UNITLESS)
C            26. RN(3,3,2)     - THE NUTATION PORTION OF THE COMPLETE CRUST 
C                                FIXED TO J2000.0 ROTATION MATRIX AND ITS CT
C                                TIME DERIVATIVE. (UNITLESS, 1/SEC)
C            27. RP(3,3,2)     - THE PRECESSION PORTION OF THE COMPLETE CRUST
C                                FIXED TO J2000.0 ROTATION MATRIX AND ITS CT 
C                                TIME DERIVATIVE. (UNITLESS, 1/SEC)
C            28. RS(3,3,3)     - THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
C                                FIXED TO J2000.0 ROTATION MATRIX AND ITS FIRST
C                                TWO CT TIME DERIVATIVES. (UNITLESS, 1/SEC, 
C                                1/SEC**2)
C            29. RW(3,3,2)     - THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED
C                                TO J2000.0 ROTATION MATRIX and its time
C                                derivative. (unitless, 1/sec)
C            30. SITEA(3,2)    - THE J2000.0 GEOCENTRIC ACCELERATION VECTORS OF
C                                EACH OBSERVATION SITE. (M/SEC**2)
C            31. SITEP(3,2)    - THE J2000.0 GEOCENTRIC POSITION VECTORS OF EACH
C                                OBSERVATION SITE. (M)
C            32. SITEV(3,2)    - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF EACH
C                                OBSERVATION SITE. (M/SEC)
C            33. SITLAT(2)     - THE SITE GEODETIC LATITUDES. (RAD)
C            34. SITLON(2)     - The site East longitudes. (rad)
C            35. SITHEIGHT(2)  - The site heights above the geoid. (m)
C            36. STAR(3)       - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C            37. SUN(3,2)      - THE J2000.0 GEOCENTRIC SUN POSITION AND
C                                VELOCITY VECTORS. (M, M/SEC)
C            38. TCTOCF(3,3,2) - THE ROTATION MATRIX WHICH ROTATES THE 
C                                TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST FIXED
C                                REFERENCE SYSTEM AT EACH OBSERVATION SITE.
C            39. TIDEP(3,2)    - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                POSITION VECTORS DUE TO EARTH TIDE EFFECTS. (M)
C            40. TIDEV(3,2)    - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                VELOCITY VECTORS DUE TO EARTH TIDES. (M/SEC)
C            41. WOBX          - THE LONG PERIOD WOBBLE X-OFFSET. (RAD)
C            42. WOBY          - THE LONG PERIOD WOBBLE Y-OFFSET. (RAD)
C                                (NOTE: WOBY IS LEFT HANDED.)
C            43. XLOADP(3,2)   - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                POSITION VECTORS DUE TO OCEAN LOADING. (M)
C            44. XLOADV(3,2)   - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                                VELOCTY VECTORS DUE TO OCEAN LOADING. (M/SEC)
C            45. ZPATH(2)      - THE ZENITH ELECTRICAL PATH LENGTH AT EACH
C                                OBSERVATION SITE. (SEC)
C            46. STAR_ABERRATED(3,2) - THE J2000.0 SOURCE UNIT VECTOR AT EACH
C                                SITE CORRECTED FOR ABERRATION. (UNITLESS)
C            47. axis2000(3,2) -  Vector axis offset of antenna in the J2000.0
C                                 frame (effect on baseline). First index is
C                                 X,Y,Z (meters), second runs over sites.
C            48. daxis2000(3,2) - Time derivative of axis2000, rate of change
C                                 of vector axis offset of antenna in the
C                                 J2000.0 frame (effect on baseline). First
C                                 index is velocity, second runs over sites.
C            49. ELEV(2,2)      - The elevation angle of the source corrrected
C                                 for aberration and its CT time derivative at
C                                 each site (rad,rad/sec)
C            50. AZ(2,2)        - The azimuth angle of the source corrrected
C                                 for aberration and its CT time derivative
C                                 at each site (rad,rad/sec)
C            51. DSTRP(2,2)     - Partial derivatives of the delay and delay
C                                 rate with respect to source RA and Dec. First
C                                 runs over RA and Dec, second runs over delay
C                                 and delay rate. (sec/rad, sec/sec-rad
C            52. NUTDIF(2,2)    - Nutation difference: IAU1980 minus IERS1996.
C                                 First index over psi and epsilon; second
C                                 index over difference and derivative of 
C                                 difference. (radians, radians/sec)
C            53. DNUTP(2,2)     - PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
C                                 RATE W.R.T DPSI AND DEPS. (SEC/RAD, 
C                                 SEC/SEC/RAD)
C            54. RTTOCF(3,3,2)  - The rotation matrix which rotates the
C                                 'radial-transverse' reference system to the
C                                 crust fixed reference system at each site.
C            55. GEOLAT(2)      - The geocentric latitude at each site. (rad)
C            56. SJD            - Time of the previous observation.
C            57. XJD            - 
C            58. UTC            - 
C            59. FUKU(2)        - Correction in longitude for the effect of
C                                 geodesic nutation, and its time derivative,
C                                 according to Fukushima. (radians, radians/sec)
C
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
C                    89.07.25  Jim Ryan Documentation simplified
C                    89.10.08  All code relating to computing a perturbed
C                              source positon deleted.  Logic changed for
C                              Shapiro (89) algorithm.
c                    91.10.05  Jim Ryan Arrary EARTH passed to ATMG for
c                              aberration computation.
c                    91.11.25  jwr  The array EARTH added to the call to
c                              SITP, STRP, UT1P, and WOBP.
C                    93MAY     CONSEN added, AXOG & ATMG calls modified, etc.
C                    93.10.07  NZ/DG, added call to DIRNC, new equation of 
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
C                    95.12.04  D. Gordon: Variable CT passed to WOBG for X,Y
C                              interpolation.
C                    95.12.11  D. Gordon  Changing RW(3,3) to RW(3,3,2). Time
C                              derivative of wobble rotation matrix computed 
C                              in WOBG.
C                    98.02.04  D. Gordon: Added DNUTP to subroutine NUTP call.
C                              Added call to subroutine NUTC for Wahr 
C                              contribution. Removed DEPSD and DPSID (database
C                              nutation values) from call to PEP. Logic added
C                              to allow skipping repeat time-dependent 
C                              computations.
C                    98.06.26  D. Gordon: Removed obsolete arguments from call
C                              to ETDG and added rotation matrix RTTOCF(3,3,2)
C                    98.09.08  D. Gordon: Added SITEV to subroutine STRP 
C                              argument list.
C                    98.09.10  D. Gordon: Move 'CALL UTCTM' ahead of 'CALL
C                              STRG' so that the time can be used for proper
C                              motion corrections (optional) in the Star
C                              geometry computations. Add XJD and UTC to
C                              STRG and STRP argument list. 
C                    98.10.15  D. Gordon: Added SITEV to subroutine SITP 
C                              argument list.
C                    98.10.16  D. Gordon: Added 'CALL PLXC' for optional
C                              computation of parallax contributions.
C                    98.11.05  D. Gordon: Added WOBXR, WOBYR, UTC, AT, DUTCAT,
C                              DATDCT, GMST to SAVE block.
C                    98.11.12  D. Gordon: Removed PANG subroutine call. The
C                              feedbox rotation (parallactic angle) module
C                              has been removed and its functions have been
C                              merged into the axis offset module. 
C                              Added variable FUKU(2) to NUTG and NUTC 
C                              argument lists. Used to compute effect of 
C                              geodesic nutation.
C                    98.11.19  D. Gordon: Added EL(2,2) to ATMP argument
C                              list to calculate the Niell atmosphere gadient
C                              partials.
C                    98.11.24  D. Gordon: Added STAR to PTDP argument list to
C                              calculate pole tide partials w.r.t. X and Y.
C                    98.12.17  D. Gordon: Added CENT to PTDP argument list to
C                              compute and remove a secular mean value for
C                              X-pole and Y-pole.
C                    99.01.14  D. Gordon: Put TSKIP in Subroutine PEP 
C                              argument list to check new/repeat time in PEP.
C                              PEP now does PUT's of Earth, Moon, and Sun 
C                              coordinates.
C
C PROGRAM STRUCTURE
C
C  Perform the geometry and time calculations.
C     The basic coordinate system is referenced to the Epoch of 2000.0 and is a
C     right-handed Cartesian system oriented to the mean celestial pole and mean
C     equator of that epoch. The nominal origin is the solar system barycenter.
C     There is also an Earth fixed coordinate system which is a right-handed
C     Cartesian system oriented to the mean geographic pole of 1900-1906 and the
C     Greenwich Meridian. The nominal origin is the Earth's center of mass. The
C     basic unit of time is the coordinate second as used by the PEP Tape. UTC,
C     AT, AND UT1 are also used. The geometry of the observation is calculated
C     with an accuracy goal of 0.1 picoseconds of delay. In doing the
C     calculations for the geometry, much of the work neccesary for the
C     computation of model contributions to delay and delay rate and partials of
C     delay and delay rate with respect to model parameters is also done.
C     Matrices which represent coordinate rotations (precession, nutation,
C     diurnal spin, diurnal polar motion, and wobble) and their CT time
C     derivatives are stored as (3,3,N) arrays, where N indixes the N-1'th time
C     derivative. The subroutines suffixed G are sections of model modules. The
C     other subroutines may be considered utilities and either superseed or
C     incorporate many present PEP routines.
C
C     Call SITG for the geographical site data. SITG provides the following
C     geocentric information for each observing site: the antenna axis offsets
C     (AXOFF), the antenna types (KAXIS), the crust fixed site vectors (CFSITE),
C     the crust fixed baseline vector (CFBASE), the crust fixed site normal unit
C     vectors (CFSITN), the geodetic latitudes (SITLAT), the site east
C     longitudes (SITLON), the spherical Earth radii, the partial derivatives of
C     the crust fixed site vector components with respect to the geodetic
C     latitudes (CFLAT) and east longitudes (CFLON), the rotation matrices which
C     rotate the topocentric site reference system to the geocentric system at
C     each site (TCTOCF), and the zenith tropospheric path delays at each
C     observing site. SITG is the only routine which 'knows' which two sites are
C     involved in the observation. All other routines merely work with site #1
C     and site #2.
C
      CALL SITG (AXOFF, CFBASE, CFLAT, CFLON, CFSITE, CFSITN, KAXIS,
     *     OCEAMP, OCEPHS, SITLAT, SITLON, SITRAD, TCTOCF, RTTOCF,
     *     ZPATH, SITHEIGHT, GEOLAT)
C
C     Call UTCTM for the UTC time fraction of the UTC day (UTC) and for 
C     the Julian Date at zero hours UTC of the date in question (XJD).
      CALL UTCTM (UTC, XJD)
C
C     Call STRG for the J2000.0 unit vector in the direction of the
C     radio source. (STAR)
      CALL STRG (XJD, UTC, STAR)
C
C     Call ATIME for the atomic time fraction of the atomic time day (AT) and
C     for the partial derivative of the UTC time with respect to the atomic
C     time (DUTCAT).
      CALL ATIME (UTC, XJD, AT, DUTCAT)
C
C     Call CTIMG for the coordinate time fraction of the coordinate time day at
C     site #1 (CT), the partial derivative of the atomic time with respect to
C     the coordinate time (DATDCT), and the partial derivative of the long
C     period terms in the 'AT minus CT' offset with respect to the coordinate
C     time (DLPGR).
      CALL CTIMG (AT, CFSITE, SITLON, UTC, XJD, CT, DATDCT, DLPGR)
C
C     Compute epoch and compare with previous observation. If same, set 
C     TSKIP=1, otherwise TSKIP=0. If TSKIP=1, then we can skip many steps in
C     the geometry subroutines.
       TJD = XJD + CT
       OBSDIF = DABS(TJD - SJD) 
       IF (OBSDIF .lt. 2.D-10) THEN
          TSKIP = 1
       ELSE
          TSKIP = 0
          SJD = TJD
       ENDIF
C
C     Call PEP for the J2000.0 geocentric Sun (SUN) and Moon (XMOON) position
C     and velocity vectors; the J2000.0 solar system barycentric Earth 
C     position, velocity, and acceleration vectors (EARTH); the other planets'
C     (except Pluto) barycentric and geocentric positions and velocities.
C     The solar system info comes from the DE/LE403 JPL Ephemeris by default.
      CALL PEP (XJD, CT, TSKIP, EARTH, SUN, XMOON)
C
C     Call NUTFA before NUTG and before UT1G to get epoch in centuries and 
C     the fundamental arguments for the nutation series.
      IF (TSKIP .NE. 1) 
     *    CALL NUTFA (xjd, ct, cent, fa, fad)
C
C     Call UT1G for the UT1 fraction of the UT1 day (UT1) and for the partial
C     derivative of the UT1 time with respect to the atomic time (DUT1AT).
      CALL UT1G (AT, DUTCAT, UTC, XJD, CT, fa, fad, cent, TSKIP,
     *     DUT1AT, UT1)
C
C     Call NUTG for the nutation portion of the complete crust fixed to J2000.0
C     rotation matrix and its CT time derivative (RN), the true obliquity of
C     the ecliptic and its CT time derivative (EPS), and the mean obliquity at
C     J2000.0 (EPSMNR).
      CALL NUTG (cent, fa, fad, TSKIP, DEPS, DPSI, EPS, EPSMNR, RN, 
     *            NUTDIF, FUKU) 

C     Call PREG for the precession portion of the complete crust fixed to
C     J2000.0 rotation matrix and its CT time derivative (RP).
      IF (TSKIP .NE. 1) CALL PREG (CT, EPSMNR, XJD, RP)
C
C     Call DIRNL for the diurnal spin portion of the complete crust fixed to 
C     J2000.0 rotation matrix and its first two CT time derivatives (RS), the
C     Greenwich apparent siderial time and its CT time derivative (GAST), the
C     Greenwich mean siderial time (GMST), the diurnal rotational velocity of
C     the Earth (DIURNV), and the difference in GAST according to two versions
C     of the equation of the equinoxes (gastd).
C ???????
      IF (TSKIP .NE. 1) 
     *  CALL DIRNL (DATDCT, DPSI, DUT1AT, EPS, DEPS, FA, UT1, XJD, 
     *     DIURNV, GAST, GMST, gastd, RS)  
C
C     Call WOBG for the wobble portion of the complete crust fixed to J2000.0
C     rotation matrix and its first time derivative (RW), and the long period
C     wobble X and Y OFFSETS. (NOTE: Right-handed coordinate system.)
      CALL WOBG (CT, UTC, XJD, TSKIP, RW, WOBXR, WOBYR)
C
C     Set the diurnal polar motion matrix to unity. (This effect now obsolete.)
      CALL ROTAT (0.D0, 3, RDNP)
C
C     Call M2000 to compute the complete crust fixed to J2000.0 rotation matrix
C     and its first two CT time derivatives.
      CALL M2000 (RDNP, RN, RP, RS, RW, TSKIP, R2000)
C
C     Call ROSIT to rotate the crust fixed site data into the J2000.0 inertial
C     reference system. The following variables are output for each observing
C     site in J2000.0 coordinates: the site position vectors (USITEP) and
C     velocity vectors (USITEV) uncorrected for Earth tidal and ocean loading
C     effects; the site accelerations (SITEA); the site normal unit vectors
C     (EPSITN); and the partial derivatives of the site position and velocity
C     vector components with respect to the site geodetic latitudes (EPLATP and
C     EPLATV) and east longitudes (EPLONP and EPLONV).
      CALL ROSIT (CFLAT, CFLON, CFSITE, CFSITN, R2000, EPLATP, EPLATV,
     .     EPLONP, EPLONV, EPSITN, SITEA, USITEP, USITEV)
C
C      IF (TSKIP .NE. 1) THEN
C     Call ETDG for the corrections to the J2000 site position vectors (TIDEP)
C     and velocity vectors (TIDEV) due to Earth tide effects.
      CALL ETDG ( R2000, SITLAT, SITLON, SUN, TCTOCF, RTTOCF, 
     *            USITEP, USITEV, XMOON, EARTH, GAST, STAR, FA, 
     *            FAD, CENT, GEOLAT, TIDEP, TIDEV)
C
C     Call 'PTDG' for the corrections to the J2000.0 site positions and site
C     velocity vectors due to the solid Earth pole tide.
      CALL PTDG (SITLAT, SITLON, SITRAD, WOBXR, WOBYR, DIURNV,
     .     TCTOCF, R2000, CENT, POLTDP, POLTDV)
C
C     Call OCEG for the corrections to the J2000.0 site position vectors
C     (XLOADP) and velocity vectors (XLOADV) due to ocean loading effects.
      CALL OCEG (CFSITE, UT1, OCEAMP, OCEPHS, R2000, XJD, TCTOCF, TSKIP,
     .     XLOADP, XLOADV)
C
C      END IF
C
C     Call SITCR to apply the Earth tide, ocean loading, and pole tide
C     corrections to the J2000.0 site position vectors (SITEP), site velocity
C     vectors (SITEV), and the J2000.0 baseline position and velocity vectors
C     (EPBASE).
      CALL SITCR (TIDEP, TIDEV, USITEP, USITEV, XLOADP,
     .     XLOADV, EPBASE, SITEP, SITEV, POLTDP, POLTDV)
C
C     Call UVG to compute the (U,V) coordinates of the baseline, depending
C      on the value of KASTC.
      CALL UVG ( STAR, EPBASE )
C
C     Call ATMG for the aberrated elevation and azimuth angles of the source and
C     their CT time derivatives, and the aberrated source unit vector.
      CALL ATMG (R2000, STAR, EARTH, TCTOCF, SITEV, AZ, ELEV,
     .     STAR_ABERRATED )
C
C     Call AXOG for the J2000.0 vector axis offsets of the antennas and their
C     time derivatives at each site.
      CALL AXOG (KAXIS, R2000, SITLAT, STAR, TCTOCF, SITEV, AXOFF,
     .     EARTH, AZ, ELEV, STAR_ABERRATED, SITHEIGHT, AXIS2000,
     .     DAXIS2000)
C
C     Call PLXG to compute the parallax goemetry.
      CALL PLXG
C
C   Perform the partial derivatives calculations. 
C     The partials are calculated using exact geometry wherever practical. Each
C     section PUT's its partials into the observation item using the database
C     handler. The calling sequence for each module has the form: Call
C     <MOD>P(...) where ... are the variables passed from the geometry section
C     of DRIVR needed to calculate the partials. All of the subroutines are 
C     parts of model modules. Note that the relativity partials are now in the
C     THERY subroutine.
C
C     Compute the atmosphere partials.
      CALL ATMP (ELEV, AZ, SITLAT, SITHEIGHT, XJD, CT, dATMCdh)
C
C
C     Compute the axis offset partials.
      CALL AXOP (AXOFF, dATMCdh)
C
C     Compute the Earth tide partials.
      CALL ETDP (R2000, SITLAT, STAR, TCTOCF)
C
C     Compute the pole tide partials.
      CALL PTDP (STAR)
C
C     Compute the nutation partials.
      CALL NUTP (CFBASE, DEPS, DPSI, EPS, GAST, RDNP, RN, RP, RS,
     .     RW, STAR, DNUTP)
C
C     Compute the ocean loading partials.
      CALL OCEP
C
C     Compute the precession partials.
      CALL PREP (CFBASE, EPSMNR, RDNP, RN, RS, RW, STAR)
C
C     Compute the site partials.
      CALL SITP (R2000, STAR, EARTH, SITEV)
C
C     Compute the star partials.
      CALL STRP (EPBASE, STAR, EARTH, SITEV, DSTRP, CD, CRA, SD, SRA)
C
C     Compute the UT1 partials.
      CALL UT1P (CFBASE,DIURNV,GAST,RDNP,RN,RP,RW,STAR,EARTH,      ! input
     .     DUT1P)                                                  ! output
C
C     Compute the wobble partials.
      CALL WOBP (CFBASE, RDNP, RN, RP, RS, STAR, EARTH)
C
C     Compute the parallax partials.
      CALL PLXP (SUN, DSTRP, CD, CRA, SD, SRA, EARTH, STAR, EPBASE, 
     *           SITEV)
C
C  Perform the contributions calculations.
C     The individual module contributions are calcualted so that they may be
C     removed from the theoretical in the program 'SOLVE' if desired. Some of
C     the routines are essentially dummies returning values passed from the
C     observation item. However, they are included to retain the capability of
C     using other models. In several cases the contributions can be calculated
C     to first order with sufficient accuracy using the partial derivatives of
C     the dalays and rates with respect to the model module parameters. Each
C     section will PUT its contributions into the observation item using the
C     database handler. Note that the relativity contributions are now in 
C     subroutine THERY.
C
C     Compute the atmosphere contributions.
      CALL ATMC (ZPATH, DATMC)
C
C     Compute the axis offset contributions.
      CALL AXOC (AXOFF, DAXOC)
C
C     Compute the earth tide contributions.
      CALL ETDC (TIDEP, TIDEV, STAR)
C
C     Compute the pole tide contributions.
      CALL PTDC (STAR)
C
C     Zero out the ionosphere contribution.
      DIONC(1) = 0.D0
      DIONC(2) = 0.D0
C
C     Compute the ocean loading contributions.
      CALL OCEC (STAR)
C
C     Compute the Wahr nutation contribution
      CALL NUTC (NUTDIF, DNUTP, FUKU)
C
C     Compute proper motion contributions.
      CALL STRC(DSTRP)
C
C     Compute the wobble contributions.
      CALL WOBC
C
C     Compute the parallax contributions.
      CALL PLXC
C
C     Compute contributions for the new definition of the equation of the 
C     equinox.
      CALL DIRNC (GASTD, DUT1P)

C     Perform the calculation for the complete theoretical delay and rate.
C     Also do all the work of all elements of the Relativity Module,
C     including the contributions and partials. This now includes only
C     the Consensus relativity model computations:
      CALL THERY (DATMC, DAXOC, DIONC, DLPGR, EARTH, EPBASE,
     .     SITEP, SITEV, SITEA, SUN, STAR, XMOON, AT)
C
C-VLBA
      DO 10 I = 1, 3
         BSLN(I,1) = EPBASE(I,1)
         BSLN(I,2) = EPBASE(I,2)
         SRC(I)  = STAR(I)
         DO 20 J = 1, 3
            EARTHX(I,J) = EARTH(I,J)
 20         CONTINUE
 10      CONTINUE
      SRCELEV(1,1) = ELEV(1,1)
      SRCELEV(1,2) = ELEV(1,2)
      SRCELEV(2,1) = ELEV(2,1)
      SRCELEV(2,2) = ELEV(2,2)
C
C     Go back to the main.
      RETURN
      END
