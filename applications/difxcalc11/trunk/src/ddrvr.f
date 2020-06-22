      SUBROUTINE dDRIVR(Iscan,J2m)
      IMPLICIT None
!
! 1.    DRIVR
!
! 1.1   DRIVR PROGRAM SPECIFICATION
!
! 1.1.1 DRIVR is the main calculation subroutine. It calculates the theoretical
!       delays and delay rates, the contributions of each model module to the
!       delays and delay rates, the partials of the delays and delay rates with
!       respect to model module parameters, and the coordinate time at site #1.
!
! 1.1.2 RESTRICTIONS - NONE
!
! 1.1.3 REFERENCES - PEP MANUAL, GREENBOOK, D. ROBERTSON'S THESIS,
!                    P. McCLURES X-DOCUMENT.
!
! 1.2   DRIVR PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE - CALL DRIVR
!       INPUT VARIABLES  -NONE
!       OUTPUT VARIABLES - NONE
!
! 1.2.2 COMMON BLOCKS USED 
!
      INCLUDE 'd_input.i'
!       Variables from:
!        1. NumEpochs  - Number of epochs for this calc run. Each will
!                        be d_interval seconds apart.
!        2. d_interval - Interval between Calc epochs (seconds). 
!                        Nominally Calc is run every 24 seconds for the
!                        difx correlator, and then a fifth degree 
!                        polynomial is fit to the delays for each 
!                        minute interval to obtain the correlator
!                        model. 
!        3. Base_mode  - If 'geocenter ' mode, first station is always
!                        the geocenter. If 'baseline  ' mode, will do
!                        all baselines (site2->site3, ..., site2->siten,
!                        site3->site4, ..., siten-1->siten). 
!                        If limited baseline mode, will do site2->site3,
!                        site2->site4, ..., site2->siten only.
!        4. Near_Far   - Character*10 variable specifying whether to 
!                        use the 'Far-field ' or the 'Near-field model.
!
      INCLUDE 'get2s.i'
!       Variables from:
!         1. LNBASE(4,2) - THE EIGHT CHARACTER SITE NAMES OF THE BASELINE
!                          OF THE CURRENT OBSERVATION. (ALPHAMERIC)
!
      INCLUDE 'put2s.i'
!       Variables from:
!        1. ELEV(2,2) - The elevation angle of the source corrrected for
!                       aberration and its CT time derivative at each
!                       site. (rad,rad/sec)
!        2. AZ(2,2)   - The azimuth angle of the source corrrected for
!                       aberration and its CT time derivative at each
!                       site. (rad,rad/sec)
!
       INCLUDE 'cmxut11.i'
!            Variables 'to':
!              1. Xintv(2)    - First and last Julian Date of data in the
!                               current data base.
!              2. Intrvl(5,2) - First and last time tag of data in the current
!                               data base. (First index: year, month, day,
!                               hour, minute. Second index: first, last.)
!
      INCLUDE 'cmxst11.i'
!            Variables 'to':
!              1. NUMSIT - The total number of sites in the data base.
!
       INCLUDE 'cuser11.i'
!       Variables from:
!            1. Calc_user   - Calc user type. 'A' for Calc/SOLVE analysis.
!                             'C' for VLBI correlator.
!            2. Apply_ocean - Switch to apply ocean loading to theoreticals
!                             for correlator usage. 'Y' to apply (recommended),
!                             'N' for do not apply.
!
       INCLUDE 'cphys11.i'
!          Variables 'from':
!            1. VLIGHT  - The velocity of light in vacuum.  (m/sec)
!
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!           Variables 'from' :
!             1. CONVD -
!
       INCLUDE 'c2poly.i'
!
!
! 1.2.3 PROGRAM SPECIFICATIONS -
      Character*50 Buf1, Buf2
      REAL*8 EPSMD, CENT, JDY2K
      Real*8 AXOFF(2), CFBASE(3), CFLAT(3,2), CFLON(3,2),               &
     &       CFSITE(3,2), CFSITN(3,2), DIONC(2),                        &
     &       EARTH(3,3), EPBASE(3,2), SUN(3,2),                         &
     &       EPLATP(3,2), EPLATV(3,2), EPLONP(3,2), EPLONV(3,2),        &
     &       EPS(2), EPSITN(3,2), OCEAMP(11,3,2),                       &
     &       OCEPHS(11,3,2), SITEA(3,2),SITEP(3,2),                     &
     &       SITEV(3,2), SITLAT(2), SITLON(2), SITRAD(2), STAR(3),      &
     &       SUNCU(3), TCTOCF(3,3,2), DATMC(2,2), ZPATH(2),             &
     &       TIDEP(3,2), TIDEV(3,2), USITEP(3,2), USITEV(3,2),          &
     &       XLOADP(3,2), XLOADV(3,2), XMOON(3,2), STARdt(3),           &
     &       POLTDP(3,2), POLTDV(3,2), SITEP1(3,2), SITEV1(3,2),        &
     &       SITHEIGHT(2), RTTOCF(3,3,2), GEOLAT(2), STAR12dt(3,2),     &
     &       AXTILT(2,2), ROTAXIS(3,3,2), OPTLcoef(6,2), STAR12(3,2),   &
     &       WOBXds, WOBYds, OPTLOADP(3,2), OPTLOADV(3,2)
      Real*8 R1(3), R2(3), R1dt(3), R2dt(3), R1mag, R2mag, R1magdt,     &
     &       R2magdt, T0_T1, STARff(3), RIGHT_ASC, DECLINATION,         &
     &       R1_TDB(3), R2_TDB(3), R1mag_TDB, R2mag_TDB, Site2_TDB(3)
      Real*8 AXIS2000(3,2), DAXIS2000(3,2), STAR_ABERRATED(3,2),        &
     &       dATMCdh(2,2), STAR_ABERRATEDdt(3,2)
      Real*8 UTC, XJD, AT, DUTCAT, CT, DATDCT, DLPGR, DUT1AT, UT1,      &
     &       EPSMNR, DIURNV, WOBXR, WOBYR, CD, CRA, SD, SRA,            &
     &       NUTDIF(2,2), SJD, TJD, OBSDIF
      Real*8 WOBXD,WOBYD,THETA,RPOM(3,3),RBPN(3,3),RT2C(3,3),           &
     &       RBPNC(3,3),RT2CC(3,3)
      Real*8 DAS2R, APC2R, RPNKK(3,3,2), DR1(3,3), DR2(3,3)
      Real*8 RPN2K(3,3,2),X,Y,   ERA2K,DERA2K,RS2K(3,3,3),              &
     &       SP,DSP,RW2K(3,3,2),R2K(3,3,3), RPC2K(3,3,2), FA2K(14),     &
     &       FAD2K(14), GAST2K(2), RSC2K(3,3,3), RNC2K(3,3,2),          &
     &       RC2K (3,3,3),                                GMST2K(2),    &
     &       RFR2K(3,3), pERA2K, DNUpe(2,2), Xn(2), Yn(2), Sn(2)
      Real*8 RPN2K6(3,3,2), X06(2), Y06(2), S06(2), RS2K6(3,3,3),       &
     &       R2K6(3,3,3), R2K6m1(3,3,3), R2K6p1(3,3,3)
      Real*8 Xti, Yti, UT1ti, dXti, dYti, dUT1ti, Xli, Yli,             &
     &                 dXli, dYli, UT1li, dUT1li, TAG_SEC
      Real*8 RS2Km1(3,3,3), RS2Kp1(3,3,3),                              &
     &       RPNm1(3,3,2), RWm1(3,3,2), RPNp1(3,3,2), RWp1(3,3,2)
      Real*8 tg2_tg1, dtg2_tg1, delta_t_grav, d_delta_t_grav,           &
     &       delta_t_grav_Sun, d_delta_t_grav_Sun
      Real*8 Xscale, Dt, Rt, DOTP
!     Real*8 UT1td
!     Real*8 R2Kdif(3,3)
      Real*8 TDBminusTT
      Real*8 TT,TDBmTT,TDB,TDBg,Elong,Udist,Vdist
      Real*8 delay6(6), poly6(6)
      Real*8 Ph_Dec, Ph_RA, CD1, SD1, CRA1, SRA1
      Real*8 tr2_tr1, dtr2_tr1, td2_td1, dtd2_td1
      Real*8 C2000STR(3)
      Real*8 K_EWNS(3,4), K_EWNS_ab(3,4), dKew, dKns, AZ_ab(4),         &
     &       EL_ab(4), gmfh(2), gmfw(2), Datmc_h_EWNS(4),               &
     &       Datmc_w_EWNS(4), STAR2(3,4), STAR2_ab(3,4), tg2_tg1ewns(4) 
!
      Character*8 Baseline(2), Sourc8
      Character*20 Sourc20
      Equivalence (LNBASE(1,1), Baseline(1))
!
      Integer*4 TSKIP, I, J, ierc2, c_out2, c_out, c2_out, get4unit,    &
     &          LC, ios, IDEC, IRA, I_ph, I_mid, Iscan, J2m, IS1, IS2
      Integer*4 IYY, IM, ID, JTAG(5), K, L, I11, I12, I21, I22, K1, K2 
      Integer*4 Itime, Istation1, Istation2, Isource, Isrc, IndexB
      Integer*2 KAXIS(2)
!
      DATA SJD /-999.D6/
!
! 1.2.3.1   SAVE BLOCK -
       SAVE SITEA, SITEP,                                              &
     &      SITEV, SITLAT, STAR, SUNCU, TCTOCF, TIDEP, SITRAD,         &
     &      TIDEV, XLOADP, XLOADV, ZPATH, EPS,                         &
     &      POLTDP, POLTDV, SUN, AXOFF, CFBASE, DIURNV, DLPGR, EPBASE, &
     &      CFSITE, CFSITN, CFLON, CFLAT, SITLON, OCEAMP, OCEPHS,      &
     &                       KAXIS, EARTH, EPSMNR, STAR_ABERRATED,     &
     &      EPSMD,        XMOON, SITHEIGHT, FA2K, FAD2K,               &
     &      NUTDIF, XJD, CT, SJD, TJD, OBSDIF, CENT, UT1, DUT1AT,      &
     &      RTTOCF, GEOLAT, WOBXR, WOBYR, UTC, AT, DUTCAT, DATDCT,     &
     &      RPN2K, Xn, Yn, Sn, ERA2K, RS2K, SP, RW2K, R2K,             &
     &      WOBXD, WOBYD, DERA2K, RPC2K, GAST2K, RSC2K,                &
     &      RNC2K, RC2K, RFR2K,                 GMST2K, pERA2K,        &
     &      AXTILT, ROTAXIS, DNUpe, Xti, Yti, UT1ti, dXti, dYti,       &
     &      dUT1ti, TT, RPN2K6, X06, Y06, S06, R2K6, TDB, TDBg,        &
     &      Xli, Yli, dXli, dYli, UT1li, dUT1li, OPTLcoef, WOBXds,     &
     &      WOBYds, OPTLOADP, OPTLOADV, STARdt, STAR12, STAR12dt,      &
     &      STAR_ABERRATEDdt, RS2Km1, RS2Kp1, R2K6m1, R2K6p1, STARff,  &
     &      Sourc20, Sourc8
!
! 1.2.4 DATA BASE ACCESS - NONE
!
! 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: MAIN
!             CALLED SUBROUTINES: INITL, OBSNT, START, TOCUP, WRIDR,
!                                 ATIME, ATMG, AXOG, CTIMG, RMPAR,
!                                 DIURNL, ETDG, M2000, NUTG, OCEG,
!                                 PEP, PREG, PTDG, RELG, ROSIT, SITG,
!                                 SITCOR, STRCOR, STRG, SUNCOR, UT1G,
!                                 UTCTME, WOBG, PLXG, ATMP, AXOP, ETDP,
!                                 NUTP, OCEP, PREP, RELP, SITP, STRP,
!                                 UT1P, WOBP, PLXP, PTDP, ATMC, AXOC,
!                                 ETDC, OCEC, PTDC, RELC, CSTAR, WOBC
!
! 1.2.7 CONSTANTS USED - NONE
!
! 1.2.8 PROGRAM VARIABLES -
!             1. AXOFF(2)      - THE ANTENNA AXIS OFFSETS AT EACH OBSERVATION
!                                SITE.  (M)
!             2. CFBASE(3)     - THE GEOCENTRIC CRUST FIXED BASELINE VECTOR. (M)
!             3. DATMC(2,2)    - THE CONTRIBUTIONS TO THE DELAY AND DELAY
!                                RATE DUE TO TROPOSPHERIC REFRACTION AT EACH
!                                OBSERVATION SITE. (SEC, SEC/SEC)
!             4. DAXOC(2,2)    - THE CONTRIBUTIONS TO THE DELAY AND DELAY
!                                RATE DUE TO THE ANTENNA AXIS OFFSETS AT EACH
!                                OBSERVATION SITE. (SEC, SEC/SEC)
!             7. DIONC(2)      - THE CONTRIBUTIONS TO THE DELAY AND DELAY
!                                RATE DUE TO IONOSPHERE EFFECTS. (SEC, SEC/SEC)
!                                Dummy variables!
!             8. DIURNV        - THE DIURNAL ANGULAR VELOCITY OF THE EARTH.
!                                (RAD/SEC)
!                                --- No longer computed !!!!
!             9. DLPGR         - THE CT TIME DERIVATIVE OF THE LONG PERIOD
!                                TERMS IN THE 'AT MINUS CT' OFFSET. (SEC/SEC)
!            10. EARTH(3,3)    - THE SOLAR SYSTEM BARYCENTRIC EARTH POSITION,
!                                VELOCITY, AND ACCELERATION VECTORS.
!                                (M, M/SEC, M/SEC**2)
!            11. EPBASE(3,2)   - THE J2000.0 GEOCENTRIC BASELINE POSITION AND
!                                VELOCITY VECTORS. (M, M/SEC)
!            12. EPS(2)        - THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS CT
!                                TIME DERIVATIVE. (RAD, RAD/SEC)
!            13. EPSMNR        - MEAN OBLIQUITY AT EPOCH J2000.0. (RAD)
!            14. EPSMD         - Mean obliquity of date (radians)
!            15. FA2K(14)      - The Fundamental arguments (5 Luni-solar,
!                                8 planetary, accumulated precession)
!                                (Radians)
!            16. FAD2K(14)     - Time derivative of the fundamental arguments
!                                (Radians/second)
!            17. CENT          - Number of Julian centuries elapsed since the
!                                epoch January 1.5, 2000. (centuries)
!            18. KEND          - THE 'END OF DATA' FLAG. KEND = 0 IF THERE IS
!                                MORE DATA TO BE PROCESSED. KEND = 1 IF THE END
!                                OF THE DATA HAS BEEN REACHED.
!            19. KOUNT         - THE FLAG WHICH INITIALIZES THE COUNTING OF THE
!                                OBSERVATION ITEMS.
!            20. PANGL(2)      - THE PARALLACTIC ANGLE DUE TO FEED BOX ROTATION
!                                AT EACH OBSERVATION SITE. (RAD)
!            21. POLTDP(3,2)   - GEOCENTRIC J2000.0 SITE POSITION CORRECTION FOR
!                                THE EFFECTS OF THE POLE TIDE. (M)
!            22. POLTDV(3,2)   - GEOCENTRIC J2000.0 SITE VELOCITY CORRECTION FOR
!                                THE EFFECTS OF THE POLE TIDE. (M/SEC)
!            23. RPC2K(3,3,2)  - THE PRECESSION PORTION OF THE COMPLETE CRUST
!                                FIXED TO J2000.0 ROTATION MATRIX AND ITS CT
!                                TIME DERIVATIVE, consistent with the IERS
!                                Conventions (2003). (UNITLESS, 1/SEC)
!            24. RS2K(3,3,3)   - THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
!                                FIXED TO J2000.0 ROTATION MATRIX AND ITS FIRST
!                                TWO CT TIME DERIVATIVES, consistent with the
!                                IERS Conventions (2003) - CEO based version.
!                                (UNITLESS, 1/SEC, 1/SEC**2)
!            25. RSC2K(3,3,3)  - THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
!                                FIXED TO J2000.0 ROTATION MATRIX AND ITS FIRST
!                                TWO CT TIME DERIVATIVES, consistent with the
!                                IERS Conventions (2003) - Classical version.
!                                (UNITLESS, 1/SEC, 1/SEC**2)
!            26. RW2K(3,3,2)   - THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED
!                                TO J2000.0 ROTATION MATRIX and its time
!                                derivative, consistent with the IERS
!                                Conventions (2003). (unitless, 1/sec)
!            27. SITEA(3,2)    - THE J2000.0 GEOCENTRIC ACCELERATION VECTORS OF
!                                EACH OBSERVATION SITE. (M/SEC**2)
!            28. SITEP(3,2)    - THE J2000.0 GEOCENTRIC POSITION VECTORS OF EACH
!                                OBSERVATION SITE. (M)
!            29. SITEV(3,2)    - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF EACH
!                                OBSERVATION SITE. (M/SEC)
!            30. SITLAT(2)     - THE SITE GEODETIC LATITUDES. (RAD)
!            31. SITLON(2)     - The site East longitudes. (rad)
!            32. SITHEIGHT(2)  - The site heights above the geoid. (m)
!            33. STAR(3)       - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!            33.5 STAR12(3,2)  - The J2000.0 source unit vectors from stations
!                                1 and 2. (unitless). For Far-field sources,
!                                these are the same as STAR(3). 
!            34. SUN(3,2)      - THE J2000.0 GEOCENTRIC SUN POSITION AND
!                                VELOCITY VECTORS. (M, M/SEC)
!            35. TCTOCF(3,3,2) - THE ROTATION MATRIX WHICH ROTATES THE
!                                TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST FIXED
!                                REFERENCE SYSTEM AT EACH OBSERVATION SITE.
!            36. TIDEP(3,2)    - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                                POSITION VECTORS DUE TO EARTH TIDE EFFECTS. (M)
!            37. TIDEV(3,2)    - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                                VELOCITY VECTORS DUE TO EARTH TIDES. (M/SEC)
!            38. WOBX          - THE LONG PERIOD WOBBLE X-OFFSET. (RAD)
!            39. WOBY          - THE LONG PERIOD WOBBLE Y-OFFSET. (RAD)
!                                (NOTE: WOBY IS LEFT HANDED.)
!            40. XLOADP(3,2)   - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                                POSITION VECTORS DUE TO OCEAN LOADING. (M)
!            41. XLOADV(3,2)   - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                                VELOCTY VECTORS DUE TO OCEAN LOADING. (M/SEC)
!            42. ZPATH(2)      - THE ZENITH ELECTRICAL PATH LENGTH AT EACH
!                                OBSERVATION SITE. (SEC)
!            43. STAR_ABERRATED(3,2) - THE J2000.0 SOURCE UNIT VECTOR AT EACH
!                                SITE CORRECTED FOR ABERRATION. (UNITLESS)
!            44. axis2000(3,2) - Vector axis offset of antenna in the J2000.0
!                                frame (effect on baseline). First index is
!                                X,Y,Z (meters), second runs over sites.
!            45. daxis2000(3,2) -Time derivative of axis2000, rate of change
!                                of vector axis offset of antenna in the
!                                J2000.0 frame (effect on baseline). First
!                                index is velocity, second runs over sites.
!            46. ELEV(2,2)     - The elevation angle of the source corrrected
!                                for aberration and its CT time derivative at
!                                each site (rad,rad/sec)
!            47. AZ(2,2)       - The azimuth angle of the source corrrected
!                                for aberration and its CT time derivative
!                                at each site (rad,rad/sec)
!****        48. DSTRP(2,2)    - Partial derivatives of the delay and delay
!****                            rate with respect to source RA and Dec. First
!****                            runs over RA and Dec, second runs over delay
!****                            and delay rate. (sec/rad, sec/sec-rad
!                                difference. (radians, radians/sec)
!            51. RTTOCF(3,3,2) - The rotation matrix which rotates the
!                                'radial-transverse' reference system to the
!                                crust fixed reference system at each site.
!            52. GEOLAT(2)     - The geocentric latitude at each site. (rad)
!            53. SJD           - Time of the previous observation.
!            54. XJD           - The Julian Date at zero hours UTC of the
!                                observation.
!            55. UTC           - UTC time fraction of the UTC day.
!            56. RPN2K(3,3,2)  - The Bias Precession Nutation portion of
!                                the complete Fixed to J2000.0 rotation
!                                matrix and its CT time derivative,
!                                consistent with the IERS Conventions
!                                (2003). (unitless, 1/sec)
!           56.5 RPN2K6(3,3,2) - The Bias Precession Nutation portion of
!                                the complete Fixed to J2000.0 rotation
!                                matrix and its CT time derivative,
!                                consistent with the IERS Conventions
!                                (2010). (unitless, 1/sec)
!            57. RNC2K(3,3,2)  - The IAU200A Nutation portion of
!                                the complete Fixed to J2000.0 rotation
!                                matrix and its CT time derivative,
!                                consistent with the IERS Conventions
!                                (2003). (unitless, 1/sec)
!            58. Xn(2)         - X-component of the CIP (Celestial
!                                Intermediate Pole) in the GCRS (Geocentric
!                                Celestial Reference System), and its time
!                                derivative. (Radians, Radians/sec)
!            59. Yn(2)         - Y-component of the CIP (Celestial
!                                Intermediate Pole) in the GCRS (Geocentric
!                                Celestial Reference System), and its time
!                                derivative. (Radians, Radians/sec)
!            60. Sn(2)         - Position of the CEO (Celestial Ephemeris
!                                Origin) on the equator of the CIP, and its
!                                time derivative. (Radians, Radians/sec)
!            61. ERA2K         - The Earth Rotation Angle, angle between
!                                the CEO Celestial Ephemeris Origin) and
!                                the TEO (Terrestrial Ephemeris Origin)
!                                on the equator of the the CIP at the
!                                observation epoch. (Radians)
!            62. DERA2K        - Time derivative of ERA2K (Radians/sec)
!            63. SP            - S-prime, position of the TEO (Terrestrial
!                                Ephemeris Origin) on the equator of the
!                                CIP. (Radians)
!            64. DSP           - Time derivative of SP. (Radians/sec)
!            65. R2K(3,3,3)    - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                                MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                                CEO-based version. (UNITLESS, 1/SEC, 1/SEC**2)
!           65.5 R2K6(3,3,3)   - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                                MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                                CEO-based version. (UNITLESS, 1/SEC, 1/SEC**2)
!                                Updated for IERS Conventions (2010).
!            66. RC2K(3,3,3)   - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                                MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                                Classical version. (UNITLESS, 1/SEC, 1/SEC**2)
!            67. RSC2K(3,3,3)  - THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
!                                FIXED TO J2000.0 ROTATION MATRIX AND ITS FIRST
!                                TWO CT TIME DERIVATIVES. (UNITLESS, 1/SEC,
!                                1/SEC**2)
!            68. GAST2K(2)     - THE GREENWICH APPARENT SIDEREAL TIME AND
!                                ITS CT TIME DERIVATIVE. (RAD, RAD/SEC)
!            69. AXTILT(2,2)   - Antenna fixed axis tilts (arc-seconds).
!                                First index runs over the two orthogonal
!                                tilt directions (Alt-Az: 1 => East,
!                                2 => North; (X-Y (N-S or E-W fixed) and
!                                Equatorial: 1 => Az error, 2 => Elev error).
!                                Second index runs over the two stations.
!            70. ROTAXIS(3,3,2)- Topocentric rotation matrices representing
!                                the fixed axis tilts for station 1 and
!                                station 2 of the current observation.
!            71. RFR2K(3,3)   -  The frame bias rotation matrix
!            72  OPTLcoef(6,2) - The 6 coefficients for computing the ocean 
!                                pole tide loading offsets at each site, and
!                                in the following order: Up-real, Up-imaginary,
!                                North-real, North-imaginary, East-real, and
!                                East-imaginary.
!            73. C2000STR(3)   - Aberrated/refracted source unit vector in the
!                                J2000.0 frame.
!                               
!
! 1.2.9 PROGRAMMER - David Gordon Jan. 2013
!        January 2015  D. Gordon  Difx version: updated for multi-phase
!                                 centers and multiple scans.             
!
! PROGRAM STRUCTURE
!
!  Perform the geometry and time calculations.
!     The basic coordinate system is referenced to the Epoch of 2000.0 and is a
!     right-handed Cartesian system oriented to the mean celestial pole and mean
!     equator of that epoch. The nominal origin is the solar system barycenter.
!     There is also an Earth fixed coordinate system which is a right-handed
!     Cartesian system oriented to the mean geographic pole of 1900-1906 and the
!     Greenwich Meridian. The nominal origin is the Earth's center of mass. The
!     basic unit of time is the coordinate second as used by the PEP Tape. UTC,
!     AT, AND UT1 are also used. The geometry of the observation is calculated
!     with an accuracy goal of 0.1 picoseconds of delay. In doing the
!     calculations for the geometry, much of the work neccesary for the
!     computation of model contributions to delay and delay rate and partials of
!     delay and delay rate with respect to model parameters is also done.
!     Matrices which represent coordinate rotations (precession, nutation,
!     diurnal spin, diurnal polar motion, and wobble) and their CT time
!     derivatives are stored as (3,3,N) arrays, where N indixes the N-1'th time
!     derivative. The subroutines suffixed G are sections of model modules. The
!     other subroutines may be considered utilities and either superseed or
!     incorporate many present PEP routines.
!
!      write(6,*) ' !!!! ddrvr/UVW = ', UVW
!  Pass # of sites to c2poly.i. 
      Numsite = Numsit
!  Open the output file if requested
!     If (I_out .eq. 1) Then 
!      LC = get4unit()
!      Open(LC, file=calc_out_file, status='new', iostat=ios )
!      If(ios.ne.0) Then
!        Write(6,'("File ",A40,"already exists. Stopping.")')           &
!    &            calc_out_file
!        Stop
!      Endif 
!      If (Atmdr .eq. 'Add-dry   ')                                     &
!    &     Buf1 = 'Dry atmosphere contributions added to delays.     '
!      If (Atmdr .eq. 'no-Add-dry')                                     &
!    &     Buf1 = 'Dry atmosphere contributions NOT added to delays. '
!      If (Atmwt .eq. 'Add-wet   ')                                     &
!    &     Buf2 = 'Wet atmosphere contributions added to delays.     '
!      If (Atmwt .eq. 'no-Add-wet')                                     &
!    &     Buf2 = 'Wet atmosphere contributions NOT added to delays. '
!      Write(LC,'("Calc 11 output. ")')
!      IF (NumSpace .le. 0) Write (LC,'("Using far-field model.")')
!      If (NumSpace .ge. 1) Write (LC,'("Using Sekido & Fukushima",     &
!    &                      " near-field model.")') 
!      If (NumSpace .ge. 1 .and. L_time .eq. 'solve     ')              &
!    &   Write (LC,'("Solving for light travel time. ")')
!      Write(LC,'(A50,/,A50)') Buf1, Buf2
!      Write(LC,'("Calc delays every ",F4.1," seconds.",/)') d_interval
!     Endif
!
!  UTC epoch at start of current 2-minute interval
       JTAG(1) = Intrvl(1,1)    ! year
       JTAG(2) = Intrvl(2,1)    ! month
       JTAG(3) = Intrvl(3,1)    ! day 
       JTAG(4) = Intrvl(4,1)    ! hour
       JTAG(5) = Intrvl(5,1) + (J2m-1)*2   ! minute
       TAG_SEC = 0.D0
         IF (JTAG(5) .ge. 60) Call FixEpoch2(JTAG, TAG_SEC)
!
! Start the loop over time. We do all observations at each epoch before 
!  moving to the next epoch because it is the most efficient in terms of
!  CPU time.
!
      DO Itime = 1, Epoch2m                     ! Start of epoch loop
!
!  Define UTC for this epoch
        If (Itime .gt. 1) TAG_SEC = TAG_SEC +  d_interval     ! seconds
       IF (TAG_SEC .ge. 59.999999999D0) Call FixEpoch2(JTAG, TAG_SEC)
        TAGSEC = TAG_SEC
!
!  Compute the Julian date at 0 hours UTC for the year, month, day.
!  Use function JDY2K to convert year, month, day to Julian date.
!  Year can be either 2-digit or 4-digit.
      IYY = JTAG(1)
      IM  = JTAG(2)
      ID  = JTAG(3)
      XJD = JDY2K(IYY,IM,ID)
!     write(6,*) '   '
!     write(6,'("ddrvr: JTAG,TAGSEC,XJD = ",5I5,F5.1,F12.2)')         &
!    &      JTAG,TAGSEC,XJD
!     write(6,*) '   '
!  Fill output time array
       Iymdhms_f(Itime,1) = JTAG(1)
       Iymdhms_f(Itime,2) = JTAG(2)
       Iymdhms_f(Itime,3) = JTAG(3)
       Iymdhms_f(Itime,4) = JTAG(4)
       Iymdhms_f(Itime,5) = JTAG(5)
       Iymdhms_f(Itime,6) = TAGSEC 
!
!     Compute the UTC time as a fraction of the UTC day.
      UTC = ( DFLOAT ( JTAG(4) ) * 3600.D0                              &
     &      + DFLOAT ( JTAG(5) ) * 60.D0                                &
     &      + TAGSEC ) / 86400.D0
!
!     write(6,'("ddrvr: UTC ",F15.10)')  UTC
!
!     Call ATIME for the atomic time fraction of the atomic time day (AT) and
!     for the partial derivative of the UTC time with respect to the atomic
!     time (DUTCAT).
      CALL ATIME (UTC, XJD, AT, DUTCAT, TT)
!     write(6,*) ' ATIME: UTC,XJD,AT,DUTCAT,TT ', UTC,XJD,AT,DUTCAT,TT
!
!     Call CTIMG for the coordinate time fraction of the coordinate time day at
!     site #1 (CT), the partial derivative of the atomic time with respect to
!     the coordinate time (DATDCT), and the partial derivative of the long
!     period terms in the 'AT minus CT' offset with respect to the coordinate
!     time (DLPGR).
      CALL CTIMG (AT, TT, CFSITE, SITLON, UTC, XJD, CT, DATDCT, DLPGR,  &
     &     TDB, TDBg )
!     write(6,*) ' CTIME: CFSITE,SITLON,CT,DATDCT,DLPGR,TDB,TDBg ',     &
!    &            CFSITE,SITLON,CT,DATDCT,DLPGR,TDB,TDBg
!
!     Compute epoch and compare with previous observation. If same, set
!     TSKIP=1, otherwise TSKIP=0. If TSKIP=1, then we can skip many steps in
!     the geometry subroutines.
       TJD = XJD + TT
       OBSDIF = DABS(TJD - SJD)
       IF (OBSDIF .lt. 1.D-16) THEN
          TSKIP = 1
       ELSE
          TSKIP = 0
          SJD = TJD
       ENDIF
!
!     Call PEP for the J2000.0 geocentric Sun (SUN) and Moon (XMOON) position
!     and velocity vectors; the J2000.0 solar system barycentric Earth
!     position, velocity, and acceleration vectors (EARTH); the other planets'
!     (except Pluto) barycentric and geocentric positions and velocities.
!     The solar system info comes from the DE421 JPL Ephemeris.
      CALL PEP (XJD, TDBg, TSKIP, EARTH, SUN, XMOON)
!     write(6,*) ' PEP: EARTH,SUN,XMOON ', EARTH,SUN,XMOON
!
!     Call NUTFA before NUTG and before UT1G to get epoch in centuries and
!     the fundamental arguments for the nutation series.
          CALL NUTFA (XJD, TT, CT, CENT, FA2K, FAD2K)
!     write(6,*) ' NUTFA: FA2K, FAD2K ', FA2K, FAD2K
!
!     Call UT1G for the UT1 fraction of the UT1 day (UT1) and for the partial
!     derivative of the UT1 time with respect to the atomic time (DUT1AT).
      CALL UT1G (AT, DUTCAT, UTC, XJD, CT, TT, FA2K, FAD2K,             &
     &     CENT, TSKIP, DUT1AT, UT1, Xti, Yti, UT1ti,                   &
     &     dXti, dYti, dUT1ti)
!     write(6,*) ' UT1G: DUT1AT, UT1 ', DUT1AT, UT1
!     write(6,*) ' UT1G: Xti,Yti,dXti,dYti(mas) ', Xti,Yti,dXti,dYti 
!     write(6,*) ' UT1G: UT1ti,dUT1ti(msec) ',  UT1ti,dUT1ti 
!
!     Call NUTG for the nutation portion of the complete crust fixed to
!     J2000.0 rotation matrices and their CT time derivatives (RPN2K6),
!     the true obliquity of the ecliptic and its CT time derivative,
!     (EPS), the mean obliquity at J2000.0 (EPSMNR), and the CEO-based
!     nutation offsets (X06, Y06, S06).
      CALL NUTG (CENT, FA2K, FAD2K, XJD, TT, TSKIP, EPS,                &
     &          EPSMNR, RPN2K6, X06, Y06, S06)
!     write(6,*) ' NUTG: X06, Y06, S06 ', X06, Y06, S06 
!     write(6,*) ' NUTG: RPN2K6 ', RPN2K6 
!
!     Call DIRNL for the diurnal spin portion of the complete crust fixed to
!     J2000.0 rotation matrices and their first two CT time derivatives (RS2K
!     and RSC2K), the Earth rotation angle and its CT time derivative
!     (ERA2K and DERA2K), the Greenwich apparent siderial time and its CT
!     time derivative (GAST2K), the Greenwich mean siderial time (GMST2K),
!     and the diurnal rotational velocity of the Earth (DIURNV).
!            ---> DIURNV removed, not used
!
       CALL DIRNL (DATDCT, DUT1AT, EPS, FA2K, FAD2K, UT1,               &
     &       XJD, CT, DUTCAT, CENT,                                     &
     &       RPN2K6, S06,                                               &
     &       ERA2K, DERA2K, pERA2K, RS2K,  RS2Km1, RS2Kp1,              &
     &       GAST2K, GMST2K, RSC2K)
!!!!!! GMST2K updated, GAST2K not updated !!!!!!!!!
!     write(6,*) ' DIRNL: ERA2K, DERA2K ', ERA2K, DERA2K 
!
!     Call WOBG for the wobble portion of the complete crust fixed to J2000.0
!     rotation matrix and its first time derivative (RW2K), and the long period
!     wobble X and Y OFFSETS. (NOTE: Right-handed coordinate system.)
      CALL WOBG (CENT, TT, UTC, XJD, GMST2K, TSKIP, FA2K, FAD2K,        &
     &           UT1, DUT1AT, Xti, Yti, dXti, dYti, Xli, Yli,           &
     &           dXli, dYli, UT1li, dUT1li, UT1ti, dUT1ti,              &
     &           WOBXR, WOBYR, WOBXD, WOBYD, SP, DSP, RW2K)
!     write(6,*) ' WOBG: WOBXR,WOBYR,WOBXD,WOBYD ', WOBXR,WOBYR,WOBXD,WOBYD
!
!     Call M2K to complete the IERS 2010 CEO-based TRF ==> CRF
!      tranformation matrix and its first two time derivatives.
      CALL M2K (RPN2K6, RS2K, RW2K, TSKIP, R2K6 )

!!    IF (NumSpace .ge. 1) THEN
      IF (Near_Far .eq. 'Near-field') THEN
       TSKIP = 0
!      Rotation matrix at -1 second.
         CALL MSUB2 (RPN2K6(1,1,1), RPN2K6(1,1,2), RPNm1(1,1,1))
         CALL MATEQ (RPN2K6(1,1,2), RPNm1(1,1,2))
         CALL MSUB2 (RW2K  (1,1,1), RW2K  (1,1,2), RWm1(1,1,1))
         CALL MATEQ (RW2K  (1,1,2), RWm1(1,1,2))
        CALL M2K (RPNm1, RS2Km1, RWm1, TSKIP, R2K6m1)
!
!      Rotation matrix at +1 second.
         CALL MADD2 (RPN2K6(1,1,1), RPN2K6(1,1,2), RPNp1(1,1,1))
         CALL MATEQ (RPN2K6(1,1,2), RPNp1(1,1,2))
         CALL MADD2 (RW2K  (1,1,1), RW2K  (1,1,2), RWp1(1,1,1))
         CALL MATEQ (RW2K  (1,1,2), RWp1(1,1,2))
        CALL M2K (RPNp1, RS2Kp1, RWp1, TSKIP, R2K6p1)
!      Write(6,1037) R2K6
 1037  Format(1x,'DRIVR/R2K6 : ',(9(/,3E25.15)))
!      Write(6,1047) R2K6m1
 1047  Format(1x,'DRIVR/R2K6m1: ',(9(/,3E25.15)))
!      Write(6,1057) R2K6p1
 1057  Format(1x,'DRIVR/R2K6p1: ',(9(/,3E25.15)))
      ENDIF
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Set up baseline definitions
      If (Base_mode .eq. 'geocenter ') Then 
!    Station 1 is always the geocenter
       I11 = 1 
       I12 = 1 
!    Station 2 runs over all antennas 
       I21 = 2 
       I22 = Numsite
      Endif
!
      If (Base_mode .eq. 'master-stn') Then 
!    Station 1 is always the first antenna 
       I11 = 2 
       I12 = 2 
!    Station 2 runs over all other antennas 
       I21 = 3 
       I22 = Numsite
      Endif
!
      If (Base_mode .eq. 'baseline  ') Then 
!    Station 1 runs from first to next-to-last antenna 
       I11 = 2 
       I12 = Numsite - 1
      Endif
!
!    Assign a unique baseline index number
       IndexB = 0
!
!  Begin Station Loops. Process all the baselines for the current epoch.
!
      DO Istation1 = I11, I12                     ! Start of station 1 loop
!
        If (Base_mode .eq. 'baseline  ') Then 
!      Station 2 runs from next antenna to the last antenna
         I21 = Istation1 + 1
         I22 = Numsite 
        Endif
!
      DO Istation2 = I21, I22                     ! Start of station 2 loop
!
!  Define baseline.
       Baseline(1) = Sites(Istation1)
       Baseline(2) = Sites(Istation2)
!    Assign a unique baseline index number
       IndexB = IndexB + 1
       Site1(IndexB,Itime) = Baseline(1)
       Site2(IndexB,Itime) = Baseline(2)
        Numbaseline = IndexB
!
!     write(6,*) '   '
!     write(6,'(5I5,F5.1,2X,A8,1X,A8)') JTAG, TAGSEC, Baseline(1),      &
!    &      Baseline(2)
!
!     Call SITG for the geographical site data. SITG provides the following
!     geocentric information for each observing site: the antenna axis offsets
!     (AXOFF), the antenna types (KAXIS), the crust fixed site vectors (CFSITE),
!     the crust fixed baseline vector (CFBASE), the crust fixed site normal unit
!     vectors (CFSITN), the geodetic latitudes (SITLAT), the site east
!     longitudes (SITLON), the spherical Earth radii, the partial derivatives of
!     the crust fixed site vector components with respect to the geodetic
!     latitudes (CFLAT) and east longitudes (CFLON), the rotation matrices which
!     rotate the topocentric site reference system to the geocentric system at
!     each site (TCTOCF), and the zenith tropospheric path delays at each
!     observing site. SITG is the only routine which 'knows' which two sites are
!     involved in the observation. All other routines merely work with site #1
!     and site #2.
      CALL SITG (AXOFF, CFBASE, CFLAT, CFLON, CFSITE, CFSITN, KAXIS,    &
     &     OCEAMP, OCEPHS, SITLAT, SITLON, SITRAD, TCTOCF, RTTOCF,      &
     &     ZPATH, SITHEIGHT, GEOLAT, AXTILT, ROTAXIS, OPTLcoef )
!
!     Call ROT2K to rotate the crust fixed site data into the J2000.0 inertial
!     reference system. The following variables are output for each observing
!     site in J2000.0 coordinates: the site position vectors (USITEP) and
!     velocity vectors (USITEV) uncorrected for Earth tidal and ocean loading
!     effects; the site accelerations (SITEA); the site normal unit vectors
!     (EPSITN); and the partial derivatives of the site position and velocity
!     vector components with respect to the site geodetic latitudes (EPLATP
!     and EPLATV) and east longitudes (EPLONP and EPLONV).
!  IERS 2010 version:
      CALL ROT2K (CFLAT, CFLON, CFSITE, CFSITN, R2K6, EPLATP, EPLATV,   &
     &     EPLONP, EPLONV, EPSITN, SITEA, USITEP, USITEV)
!     write(6,237) USITEP, USITEV, SITEA
 237  format(' USITEP, USITEV, SITEA',/,6(3D30.16,/))
!
!     Call ETDG for the corrections to the J2000 site position vectors (TIDEP)
!     and velocity vectors (TIDEV) due to Earth tide effects.
      CALL ETDG ( R2K6, SITLAT, SITLON, SUN, TCTOCF, RTTOCF,            &
     &            USITEP, USITEV, XMOON, EARTH, GAST2K,                 &
     &                FA2K, FAD2K, CENT, GEOLAT, TIDEP, TIDEV)
!
!     Call 'PTDG' for the corrections to the J2000.0 site positions
!      and site velocity vectors due to the solid Earth pole tide.
      CALL PTDG (SITLAT, SITLON, SITRAD, WOBXR, WOBYR, &
     &     TCTOCF, R2K6, CENT, POLTDP, POLTDV, WOBXds, WOBYds)
!
!     Call OCEG for the corrections to the J2000.0 site position vectors
!      (XLOADP) and velocity vectors (XLOADV) due to ocean loading effects.
      CALL OCEG (CFSITE, UT1, OCEAMP, OCEPHS, R2K6, XJD, TCTOCF, TSKIP, &
     &     FA2K, CENT, UTC, XLOADP, XLOADV)
!
!     Call OCPTG for the corrections to the J2000.0 site position vectors
!      (XLOADP) and velocity vectors (XLOADV) due to ocean pole tide 
!      loading effects.
      CALL OPTLG (WOBXds, WOBYds, OPTLcoef, R2K6, TCTOCF, TSKIP,        &
     &           OPTLOADP, OPTLOADV)
!
!     Call dSITCR to apply the Earth tide, ocean loading, and pole tide
!      corrections to the J2000.0 site position vectors (SITEP), site
!      velocity vectors (SITEV), and the J2000.0 baseline position and
!      velocity vectors (EPBASE).
      CALL dSITCR (TIDEP, TIDEV, USITEP, USITEV, XLOADP, XLOADV,        &
     &     EPBASE, SITEP, SITEV, POLTDP, POLTDV, OPTLOADP, OPTLOADV)
!     write(6,238) SITEP
 238  format(' Site positions:',/,3D26.16,/,3D26.16)
!     write(6,239) SITEV
 239  format(' Site velocities:',/,3D26.16,/,3D26.16)
!
!
      DO Isrc = 1, (NumPhCntr+1)           ! Start of source/phase center loop
!
       If (Isrc .eq. 1) Isource = PointingSrc
       If (Isrc .gt. 1) Isource = PhCntr(Isrc-1) 
!       write(6,*) 'ddrvr: Isrc, Isource ', Isrc, Isource

!
!     Call STRG for the J2000.0 unit vector in the direction of the
!     radio source. (STAR)
!        Far-field:
      IF (Near_Far .eq. 'Far-field ') Then 
          CALL STRG (XJD, UTC, Isource,                                 &
     &           STAR, STAR12, RIGHT_ASC, DECLINATION, Sourc20)
!
       If (UVW .eq. 'exact ' .or. UVW .eq. 'noatmo')  Then
!          write (6,*) 'Itime, ANT, Isrc: ', Itime, (Istation2-2), Isrc 
          Call STAR_NSEW (STAR, K_EWNS, RIGHT_ASC,DECLINATION, STAR12,  &
     &                    dKew,dKns)
       Endif
      Endif
!
!        Near-field:
      If (Near_Far .eq. 'Near-field') Then
        CALL NFSTRG (XJD, UTC, Isource, EARTH,  &
     &    SITEP, SITEV, SUN, XMOON,R2K6,STAR, STARdt, STAR12, STAR12dt, &
     &    T0_T1, R1, R1dt, R1mag, R1magdt, R2, R2dt, R2mag,             &
     &    R2magdt, STARff, R1_TDB, R2_TDB, R1mag_TDB, R2mag_TDB,        &
     &    Site2_TDB, Sourc20)
!       write(6,*) 'ddrvr1/R1mag,R1magdt ',  R1mag,R1magdt
!       write(6,*) 'ddrvr1/R2mag,R2magdt ',  R2mag,R2magdt
!       write(6,'(" R1mag: ",F20.7," AU")') R2mag/1.4959787D11
!       write(6,*) 'STARff: ',  STARff
       If (UVW .eq. 'exact ' .or. UVW .eq. 'noatmo')  Then
        Call NFSTewns( R1, R1dt, R1mag, R1magdt, R2, R2dt, R2mag,       &
     &       R2magdt, STAR, STARdt, STAR12, STAR12dt, SITEP, SITEV,     &
     &       R1_TDB, R2_TDB, R1mag_TDB, R2mag_TDB, Site2_TDB, K_EWNS,   &
     &       STAR2           )
!       Write (6,*) '*** ddrvr *** '
!       Write (6,*) 'STAR2(n,1): ', STAR2(1,1), STAR2(2,1), STAR2(3,1)
!       Write (6,*) 'STAR2(n,2): ', STAR2(1,2), STAR2(2,2), STAR2(3,2)
!       Write (6,*) 'STAR2(n,3): ', STAR2(1,3), STAR2(2,3), STAR2(3,3)
!       Write (6,*) 'STAR2(n,4): ', STAR2(1,4), STAR2(2,4), STAR2(3,4)
!       Write (6,*) '************* '
       Endif
      Endif
!
!       xSource = Sourc8
        xSource = Sourc20
!       write(6,*) 'ddrvr: Sourc20,xSource,RIGHT_ASC, DECLINATION',     &
!    &    Sourc20,xSource,RIGHT_ASC, DECLINATION
!
!
!     Call ATMG for the aberrated elevation and azimuth angles of the
!      source and their CT time derivatives, and the aberrated source
!      unit vector.
!!    IF (NumSpace .eq. 0) Then
      IF (Near_Far .eq. 'Far-field ') Then
         CALL ATMG (R2K6, STAR, STAR12, EARTH, TCTOCF, SITEV,           &
     &     STAR_ABERRATED)
       DO I = 1,3
        DO J = 1,2
          STAR_ABERRATEDdt(I,J) = 0.D0
        ENDDO
       ENDDO 
        If (UVW .eq. 'exact ')  Then
          CALL ATMGuv (R2K6, K_EWNS, EARTH, TCTOCF, SITEV, K_EWNS_ab,   &
     &         AZ_ab, EL_ab)
        Endif

      ENDIF
!
!!    IF (NumSpace .ge. 1)                                              &
      IF (Near_Far .eq. 'Near-field') Then  
        CALL NFATM (R2K6, STAR, STAR12, STAR12dt, EARTH, TCTOCF, SITEV, &
     &       SITEA, R2K6m1, R2K6p1, R1mag, R2mag,                       &
     &       STAR_ABERRATED, STAR_ABERRATEDdt)
!       write(6,*) 'ddrvr2/R1mag,R1magdt ',  R1mag,R1magdt
!       write(6,*) 'ddrvr2/R2mag,R2magdt ',  R2mag,R2magdt
!
       If (UVW .eq. 'exact ' .or. UVW .eq. 'noatmo')  Then
        Call NFATMuv (R2K6, STAR2, TCTOCF, SITEV, STAR12, STAR2_ab,      &
     &         AZ_ab, EL_ab)
       Endif
      ENDIF
!
!     Call AXOG for the J2000.0 vector axis offsets of the antennas and
!      their time derivatives at each site.
      CALL AXOG (KAXIS, R2K6, SITLAT, STAR, TCTOCF, SITEV, AXOFF,       &
     &     EARTH, STAR_ABERRATED, STAR_ABERRATEDdt, SITHEIGHT, AXTILT,  &
     &     ROTAXIS, AXIS2000, DAXIS2000, C2000STR)
!  C2000STR(3) = Aberrated/refracted source unit vector in the J2000.0 frame.
!
!    Add axis offset to the baseline instead of computing a contribution 
      Do J=1,2
       Do I=1,3
        SITEP1(I,J) = SITEP(I,J) +  AXIS2000(I,J)
        SITEV1(I,J) = SITEV(I,J) + DAXIS2000(I,J)
       Enddo
      Enddo
       Do I=1,3
        EPBASE(I,1) = SITEP1(I,1) - SITEP1(I,2)
        EPBASE(I,2) = SITEV1(I,1) - SITEV1(I,2)
       Enddo
!
!    Call UVG to compute the (U,V) coordinates of the baseline.
      If (UVW .eq. 'uncorr') Then 
       CALL UVG_un (STAR, EPBASE) ! [ABERRATION CORR = UNCORRECTED]
!      Write (6,*) 'UVG_un: U,V,W   ', U_V, Wb
      Endif
!
!    Call UVG_ab to compute the (U,V) coordinates of the baseline,
!    using the aberrated source unit vector. 
      If (UVW .eq. 'approx') Then 
       CALL UVG_ab (STAR_ABERRATED, EPBASE) ! [ABERRATION CORR = APPROXIMATE]
!      Write (6,*) 'UVG_ab: U,V,W   ', U_V, Wb
      Endif
!
!    Call UVG_no to compute the (U,V) coordinates of the baseline,
!    using the partial derivative technique without atmosphere 
!#    CALL UVG_plus (EPBASE, STAR, STAR_ABERRATED, EARTH, SITEV1) ! [ABERRATION CORR = NO ATMOS]
!!!!  CALL UVG_plus (EPBASE, STAR, STAR          , EARTH, SITEV1) ! [ABERRATION CORR = NO ATMOS??]
!!!!!! Write (6,*) 'UVG_noatmo: U,V ', U_V
!
!    Call UVG_exact to compute the (U,V) coordinates of the baseline,
!    using the partial derivative technique with atmosphere 
!#    CALL UVG_plus (EPBASE, STAR, C2000STR, EARTH, SITEV1) ! [ABERRATION CORR = EXACT ]
!#     Write (6,*) 'UVG_exact: U,V  ', U_V
!
!  Load U,V,W coordinates:
!       Ubase_f(Itime,Istation1,(Istation2-1),Isrc) = U_V(1)
!       Vbase_f(Itime,Istation1,(Istation2-1),Isrc) = U_V(2)
!       Wbase_f(Itime,Istation1,(Istation2-1),Isrc) = Wb 
!
!
!   Perform the partial derivatives calculations.
!    For difx, only the atmosphere partials are needed.
!    The others are left for future use but commented out.
!
!     Compute the atmosphere partials.
      CALL ATMP (SITLAT, SITLON, SITHEIGHT, XJD, CT, dATMCdh,           &
     &     gmfh, gmfw)
!
!     Compute the axis offset partials.
!**   CALL AXOP (AXOFF, STAR12, EARTH, SITEV1)
!
!     Compute the Earth tide partials.
!**   CALL ETDP (R2K6, SITLAT, STAR, TCTOCF)
!
!     Compute the pole tide partials.
!**   CALL PTDP (STAR)
!
!     Compute the nutation partials. (IERS 2010)
!**   CALL NUTP (CFBASE,      X06,Y06,S06,                              &
!    &      STAR, RPN2K6, RS2K, RW2K, TSKIP)
!
!     Compute the ocean loading partials.
!**   CALL OCEP()
!
!     Compute the site partials.
!**   CALL SITP (R2K6, STAR, STAR12, EARTH, SITEV1)
!
!     Compute the star partials.
      CALL STRP (EPBASE, STAR, EARTH, SITEV1,        CD, CRA, SD, SRA)
!*    Write(6,'(" U,V,W: ",7x,3D22.14)')  U_V(1), U_V(2), Wb
!     WRITE(6,'(" DSTRP*Vlight: ",4D22.14)') DSTRP(1,1)*Vlight/CD,      &
!    &  DSTRP(2,1)*Vlight, DSTRP(1,2)*Vlight/CD, DSTRP(2,2)*Vlight

!
!     Compute the UT1 partials.
!**   CALL UT1P (CFBASE, STAR,EARTH, RPN2K6, RW2K, ERA2K, dERA2K,        &
!**  &           pERA2K, SITEV       )
!
!     Compute the wobble partials.
!**   CALL WOBP (CFBASE, STAR, EARTH, RPN2K6, RS2K, SITEV1)
!
!     Compute the parallax partials.
!**   CALL PLXP (SUN, CD, CRA, SD, SRA, EARTH, STAR, EPBASE, SITEV1)
!
!
!  Perform the contributions calculations.
!    For difx, only the atmosphere contributions are needed.
!    The others are left for future use but commented out.
!
!     Compute the atmosphere contributions.
      CALL ATMC (ZPATH, DATMC)
       If (UVW .eq. 'exact ')  Then
        Call EWNS_atmC (gmfh,gmfw, EL_ab,Datmc_h_EWNS,Datmc_w_EWNS)
       Endif
!
      If (Base_mode .eq. 'geocenter ') IS2 = 1
      If (Base_mode .ne. 'geocenter ') Then
       IS1 = 1
       IS2 = 2
      Endif
!      
!    Station 2:
       ATMdryd_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = DATMC(2,1)
       ATMdryr_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = DATMC(2,2)
       ATMwetd_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = Datmc_wmf(2,1)
       ATMwetr_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = Datmc_wmf(2,2)
       El_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = ELEV(2,1) * 57.295779512D0
       Az_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = AZ(2,1) * 57.295779512D0
       StaX_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = -EPBASE(1,1)
       StaY_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = -EPBASE(2,1)
       StaZ_f(IS2,Itime,Istation1,(Istation2-1),Isrc) = -EPBASE(3,1)
!    Station 1 (usually the geocenter):
!     If (Base_mode .ne. 'geocenter ') Then
!      ATMdryd_f(IS1,Itime,Istation1,(Istation2-1),Isrc) = DATMC(1,1)
!      ATMdryr_f(IS1,Itime,Istation1,(Istation2-1),Isrc) = DATMC(1,2)
!      ATMwetd_f(IS1,Itime,Istation1,(Istation2-1),Isrc) = Datmc_wmf(1,1)
!      ATMwetr_f(IS1,Itime,Istation1,(Istation2-1),Isrc) = Datmc_wmf(1,2)
!     Endif
!
!     Compute the axis offset contributions.
!**   CALL AXOC (AXOFF)
!
!     Compute the Earth tide contributions.
!**   CALL ETDC (TIDEP1, TIDEV1, STAR)
!
!     Compute the pole tide contributions.
!**   CALL PTDC (STAR)
!
!     Compute the ocean loading contributions.
!**   CALL OCEC (STAR)
!
!     Compute the ocean pole tide loading contributions.
!**   CALL OPTLC(OPTLOADP, OPTLOADV, STAR)
!
!     Compute the UT1 contributions.
!**   CALL UT1C (       UT1ti, dUT1ti, UT1li, dUT1li)
!
!     Compute the wobble contributions.
!**   CALL WOBC(Xti,Yti,dXti,dYti)
!
!     Compute the parallax contributions.
!**   CALL PLXC()
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Perform the calculation for the complete theoretical delay and rate.
!     Also do all the work of all elements of the Relativity Module,
!     including the contributions and partials. This now includes only
!     the Consensus relativity model computations:
!!!   IF (NumSpace .le. 0)                                              &
      IF (Near_Far .eq. 'Far-field ')                                   &
     & Call CONSEN ( DATMC, EARTH, EPBASE, SITEP1, SITEV1,              &
     &      SITEA, SUN, XMOON, STAR, K_EWNS, Datmc_h_EWNS,Datmc_w_EWNS, &
     &      tg2_tg1, dtg2_tg1,                                          &
     &      delta_t_grav, d_delta_t_grav, delta_t_grav_Sun,             &
     &      d_delta_t_grav_Sun )    
!
!       write(6,*) 'ddrvr3/R1mag,R1magdt ',  R1mag,R1magdt
!       write(6,*) 'ddrvr3/R2mag,R2magdt ',  R2mag,R2magdt
!
!     IF (NumSpace .ge. 1) THEN 
      IF (Near_Far .eq. 'Near-field') THEN
!
       If (NF_model .eq. 'Sekido  ') Then
        Call SEKIDO ( DATMC, EARTH, EPBASE, SITEP1, SITEV1, SITEA, SUN, &
     &      XMOON, STAR, STARdt, T0_T1, R1, R1dt, R1mag, R1magdt,       &
     &      R2, R2dt, R2mag, R2magdt, STAR12, STAR12dt,                 &
     &      tg2_tg1, dtg2_tg1) 
!      write(6,*) 'tg2_tg1,dtg2_tg1 ', tg2_tg1,dtg2_tg1
        If (UVW .eq. 'exact ' .or. UVW .eq. 'noatmo')  Then
!         write (6,*) ' tg2_tg1      ', tg2_tg1
         Call  SEKIDOewns ( Datmc_h_EWNS, Datmc_w_EWNS, EARTH, EPBASE,  &
     &         SITEP, SITEV,  SITEA, SUN, XMOON, T0_T1 )
        Endif
       Endif
!
       If (NF_model .eq. 'Ranging ') Then
        Call RANGE (DATMC, SITEP, SITEV, SITEA, SUN,                    &
     &      T0_T1, R1, R1dt, R1mag, R1magdt,                            &
     &      R2, R2dt, R2mag, R2magdt,                                   &
     &      tr2_tr1, dtr2_tr1)
         tg2_tg1 = tr2_tr1
         dtg2_tg1 = 0.D0
        If (UVW .eq. 'exact ' .or. UVW .eq. 'noatmo')  Then
!         write(6,*) 'tr2_tr1 ', tr2_tr1
        Call  RANGEewns (Datmc_h_EWNS, Datmc_w_EWNS, SITEP, SITEV,      &
     &      SITEA, SUN, T0_T1,                                          &
     &      R1, R1dt, R1mag, R1magdt, R2, R2dt, R2mag, R2magdt)
        Endif
       Endif
!
       If (NF_model .eq. 'Duev    ') Then
        CALL DUEV (DATMC, SITEP, SITEV, SITEA, SUN, T0_T1,              &
     &     EARTH, XMOON, UTC, TT, TDB, TDBg, td2_td1, dtd2_td1)
         tg2_tg1 = td2_td1
         dtg2_tg1 = 0.D0
!       write(6,*) 'td2_td1 ', td2_td1
        If (UVW .eq. 'exact ' .or. UVW .eq. 'noatmo')  Then
         Call DUEVewns (Datmc_h_EWNS, Datmc_w_EWNS, SITEP, SITEV,       &
     &      SITEA, SUN, T0_T1, EARTH, XMOON, UTC, TT, TDB, TDBg)
        Endif
       Endif
!
!      write(6,*) 'tg2_tg1,tr2_tr1,td2_td1 ', tg2_tg1,tr2_tr1,td2_td1
!      write(6,*) 'diffs: ', tg2_tg1-tr2_tr1, tg2_tg1-td2_td1, tr2_tr1-td2_td1 
!      write(6,*) '       ' 
!
      ENDIF
!
!
!   Load delay and rate arrays
        Delay_f(Itime,Istation1,(Istation2-1),Isrc) = tg2_tg1 
         Rate_f(Itime,Istation1,(Istation2-1),Isrc) = dtg2_tg1
!        write(6,*) 'ddrvr: ', Iscan,J2m,Itime,Istation1,(Istation2-1), &
!    &     Isrc, Delay_f(Itime,Istation1,(Istation2-1),Isrc),DATMC(1,1), DATMC(2,1) 
!
       If (Atmdr .eq. 'Add-dry   ')  Then
        Delay_f(Itime,Istation1,(Istation2-1),Isrc)  =                  &
     &     Delay_f(Itime,Istation1,(Istation2-1),Isrc) +                &
     &      DATMC(1,1) + DATMC(2,1)
         Rate_f(Itime,Istation1,(Istation2-1),Isrc)  =                  &
     &     Rate_f(Itime,Istation1,(Istation2-1),Isrc)   +               &
     &     DATMC(1,2) + DATMC(2,2)
       Endif
!
       If (Atmwt .eq. 'Add-wet   ')  Then
        Delay_f(Itime,Istation1,(Istation2-1),Isrc)  =                  &
     &     Delay_f(Itime,Istation1,(Istation2-1),Isrc) +                &
     &     Datmc_wmf(1,1) + Datmc_wmf(2,1)
         Rate_f(Itime,Istation1,(Istation2-1),Isrc)  =                  &
     &     Rate_f(Itime,Istation1,(Istation2-1),Isrc) +                 &
     &     Datmc_wmf(1,2) + Datmc_wmf(2,2)
       Endif
!
! W coordinate of UVW:
      If (UVW .eq. 'noatmo')  Then
       Wb = tg2_tg1 * VLIGHT
!      Write (6,*) 'noatmo: U,V,W   ', U_V, Wb
      Endif
!
      If (UVW .eq. 'exact ') Then 
        Wb = (tg2_tg1 + DATMC(1,1) + DATMC(2,1) +                       &
     &       Datmc_wmf(1,1) + Datmc_wmf(2,1)) * VLIGHT
!      Write (6,*) 'exact: U,V,W   ', U_V, Wb
      Endif
!
!  Load U,V,W coordinates:
        Ubase_f(Itime,Istation1,(Istation2-1),Isrc) = U_V(1)
        Vbase_f(Itime,Istation1,(Istation2-1),Isrc) = U_V(2)
        Wbase_f(Itime,Istation1,(Istation2-1),Isrc) = Wb 
!         
!     If (I_out .eq. 1) Then 
!      L = Itime
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      write (LC,1016) RIGHT_ASC, DECLINATION, IRA,IDec, Ph_RA, Ph_Dec  
!1016  Format('  RA/Dec: ',2F20.15,'  Phase Center(',I3,',',I3,'):',2F20.15)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      If (Base_mode .eq. 'geocenter ') Then 
!       write (LC,1011) IndexB, Iymdhms_f(L,1), Iymdhms_f(L,2),          &
!    &        Iymdhms_f(L,3),                                           &
!    &        Iymdhms_f(L,4), Iymdhms_f(L,5), Iymdhms_f(L,6),           &
!    &        Site1(IndexB,L), Site2(IndexB,L), xSource,                &
!    &        Delay_f(IndexB,L)*1.D6, Rate_f(IndexB,L)*1.D6,            &
!    &        Atmdryd_f(IndexB,2,L)*1.D6, Atmdryr_f(IndexB,2,L)*1.D6,   &
!    &        Atmwetd_f(IndexB,2,L)*1.D6, Atmwetr_f(IndexB,2,L)*1.D6,   &
!             Ubase_f(IndexB,L), Vbase_f(IndexB,L), Wbase_f(IndexB,L)
!1011 Format(I3,I7,5I3,3X,A8,2X,A8,3X,A8,/,'Delay(us):   ',2E25.16,/,   &
 1011 Format(I3,I7,5I3,3X,A8,2X,A8,3X,A20,/,'Delay(us):   ',2E25.16,/,   &
     &       'Atm-dry(us): ', 2E25.16,/, 'Atm-wet(us): ', 2E25.16,/,    &
     &       'U,V,W(m):    ', 3E25.16,/) 
!      Endif
!
!      If (Base_mode .eq. 'master-stn' .or. Base_mode .eq. 'baseline  ') Then 
!!       Write(LC,'("KdotB/C(us): ",2E25.16)') (DOTP(STAR,EPBASE(1,1)))/VLIGHT*1.D6,   &
!!   &        (Delay_f(IndexB,L) - (DOTP(STAR,EPBASE(1,1)))/VLIGHT)*1.D6
!!       Write(LC,'("R2-R1/C(us): ",2E25.16)') (R2mag - R1mag)/VLIGHT*1.D6,    &
!!   &        (Delay_f(IndexB,L) - (R2mag - R1mag)/VLIGHT)*1.D6
!      write (LC,1012) IndexB, Iymdhms_f(L,1), Iymdhms_f(L,2),          &
!    &        Iymdhms_f(L,3),                                           &
!    &        Iymdhms_f(L,4), Iymdhms_f(L,5), Iymdhms_f(L,6),           &
!    &        Site1(IndexB,L), Site2(IndexB,L), xSource,                &
!    &        Delay_f(IndexB,L)*1.D6, Rate_f(IndexB,L)*1.D6,            &
!    &        DATMC(1,1)*1.D6, DATMC(1,2)*1.D6,                         &
!    &        Datmc_wmf(1,1)*1.D6, Datmc_wmf(1,2)*1.D6,                 &
!    &        DATMC(2,1)*1.D6, DATMC(2,2)*1.D6,                         &
!    &        Datmc_wmf(2,1)*1.D6, Datmc_wmf(2,2)*1.D6,                 &
!             Ubase_f(IndexB,L), Vbase_f(IndexB,L), Wbase_f(IndexB,L)
!1012 Format(I3,I7,5I3,3X,A8,2X,A8,3X,A8,/,'Delay(us):   ',2E25.16,/,   &
 1012 Format(I3,I7,5I3,3X,A8,2X,A8,3X,A20,/,'Delay(us):   ',2E25.16,/,   &
     &       'Atm-dry(us): ', 2E25.16,/, 'Atm-wet(us): ', 2E25.16,/,    &
     &       'Atm-dry(us): ', 2E25.16,/, 'Atm-wet(us): ', 2E25.16,/,    &
     &       'U,V,W(m):    ', 3E25.16,/)
!      Endif
!
!     Endif
!
!
!      write (6,*) '               '
!     Write(6,'("NF: Elev:",4F15.10)') Elev(1,1)/CONVD,Elev(2,1)/CONVD, &
!    &         Elev(1,2)/CONVD,Elev(2,2)/CONVD
!     Write(6,'("NF: AZ:  ",4F15.10)') AZ(1,1)/CONVD, AZ(2,1)/CONVD,    &
!    &         AZ(1,2)/CONVD, AZ(2,2)/CONVD
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Compare near-field to far-field:
!     IF (NumSpace .ge. 1) THEN
!       write(6,*) '                     '
!       write(6,*) ' Switching to far-field model '
!       write(6,*) '                     '
!      CALL STRG (XJD, UTC, Isource, STAR, STAR12, Sourc8)
!       write(6,*) 'STAR: ',  STAR
!       write(6,*) 'STAR12: ',  STAR12
!      CALL ATMG (R2K6, STAR, STAR12, EARTH, TCTOCF, SITEV,           &
!    &     STAR_ABERRATED)
!      write (6,*) '               '
!     Write(6,'("FF: Elev:",4F15.10)') Elev(1,1)/CONVD,Elev(2,1)/CONVD, &
!    &         Elev(1,2)/CONVD,Elev(2,2)/CONVD
!     Write(6,'("FF: AZ:  ",4F15.10)') AZ(1,1)/CONVD, AZ(2,1)/CONVD,    &
!    &         AZ(1,2)/CONVD, AZ(2,2)/CONVD
!      DO I = 1,3
!       DO J = 1,2
!         STAR_ABERRATEDdt(I,J) = 0.D0
!       ENDDO
!      ENDDO 
!      CALL AXOG (KAXIS, R2K6, SITLAT, STAR, TCTOCF, SITEV, AXOFF,       &
!    &     EARTH, STAR_ABERRATED, STAR_ABERRATEDdt, SITHEIGHT, AXTILT,  &
!    &     ROTAXIS, AXIS2000, DAXIS2000)
!      CALL ATMP (          SITLAT, SITHEIGHT, XJD, CT, dATMCdh)
!      CALL AXOP (AXOFF, STAR12, EARTH, SITEV)
!      CALL ATMC (ZPATH, DATMC)
!      CALL AXOC (AXOFF       )
!      CALL UVG_ab (STAR_ABERRATED, EPBASE)
!      CALL THERY (DATMC, DIONC, DLPGR, EARTH, EPBASE, SITEP, SITEV,    &
!    &            SITEA, SUN, STAR, XMOON, AT)
!     Call  CONSEN ( DATMC, EARTH, EPBASE, SITEP, SITEV,                &
!    &      SITEA, SUN, XMOON, STAR, tg2_tg1, dtg2_tg1,                 &
!    &      delta_t_grav, d_delta_t_grav, delta_t_grav_Sun,             &
!    &      d_delta_t_grav_Sun )    
!
!      Dt = tg2_tg1 + DATMC(1,1) + DATMC(2,1) + Datmc_wmf(1,1) +        &
!    &                Datmc_wmf(2,1)
!      Rt = dtg2_tg1 + DATMC(1,2) + DATMC(2,2) + Datmc_wmf(1,2) +       &
!    &                 Datmc_wmf(2,2)
!
!     Write(6,1021) Dt*1.D6, Rt*1.D6, DATMC(2,1)*1.D6, DATMC(2,2)*1.D6, &
!    &    Datmc_wmf(2,1)*1.D6, Datmc_wmf(2,2)*1.D6, U_V(1), U_V(2), Wb
!      Write(6,1022)  (Dt - Delay_f(IndexB,L))*1.D12,                   &
!    &  (Rt - Rate_f(IndexB,L))*1.D12,                                  &
!    &  (DATMC(2,1) - Atmdryd_f(IndexB,2,L))*1.D12,                     &
!    &  (DATMC(2,2) - Atmdryr_f(IndexB,2,L))*1.D12,                     &
!    &  (Datmc_wmf(2,1) - Atmwetd_f(IndexB,2,L))*1.D12,                 &
!    &  (Datmc_wmf(2,2) - Atmwetr_f(IndexB,2,L))*1.D12,                 &
!    &  U_V(1) - Ubase_f(IndexB,L),                                     &
!    &  U_V(2) - Vbase_f(IndexB,L),                                     &
!    &  Wb - Wbase_f(IndexB,L)
!1021  Format(6E25.16,/,3E25.16)
!1022  Format(2F12.4,1X,2F12.4,1X,2F12.4,1X,3F10.4)
!     ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      ENDDO                        ! End of source/phase center loop
!
      ENDDO                        ! End of station2 loop
      ENDDO                        ! End of station1 loop
      ENDDO                        ! End of epoch loop
!
!     If (I_out .eq. 1) Close (LC)
!
!      write (6,*) '               '
!      write (6,*) ' Numbaseline = ', Numbaseline
!      write (6,*) '               '
!
!
!      write (6,*) '               '
!        Call c_out2(delay_f(1,1))
!           delay6(1) = Delay_f(1,1)
!           delay6(2) = Delay_f(1,2)
!        ierc2 = c_out(delay6, poly6)
!      write (6,*) ' poly6 ', poly6
!!!       call  f2c1(delay6)
!      write (6,*) '               '
!        ierc2 = c2_out( %REF(delay_f(I11,I21,1)))
!        ierc2 = c_out2( )
!      write (6,*) '               '
!
!
!     Go back to the main.
      RETURN
      END
