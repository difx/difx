      SUBROUTINE ETDA
      IMPLICIT None 
C
C     ETDA adds entries to the table of contents for the Earth tide
C     module text message, the partial derivatives, and the contributions.
C
C     ETDA Program Interface:
C
C     Common blocks used:
        INCLUDE 'ccon.i'
C
      INCLUDE 'cuser.i'
C           1. Calc_user - Analysis center or Correlator user
C
C
C     DATABASE ACCESS -
C            ACCESS CODES:
C              1. 'ETD MESS' - The database access code for the Earth Tide
C                              Module Text Message.
C              2. 'ETD CFLG' - The database access code for the Earth Tide
C                              Module flow control message.
C              3. 'ETD CONT' - The database access code for the Earth Tide
C                              Module contributions array.
C              4. 'ETJMGPAR' - The database access code for the Earth Tide
C                              Module partial derivatives array (1996 IERS
C                              model) as defined in John Gipson's 10 Aug 1998
C                              memo.
C              5. 'C82ETCON' - The database access code for the delay and
C                              rate contribution to get the previous Calc 
C                              (versions 8.x) Earth tide model with third 
C                              order term included.
C              6. 'ELASTCON' - The data base access code for the correction
C                              to change from the anelastic to the elastic 
C                              case.
C              7. 'PERMDEF ' - The database access code for the delay and
C                              rate contribution to restore the effects of
C                              the permanent deformation.
C
C    Subroutine interface:
C           Caller subroutines: TOCUP
C           Called subroutines: ADDA, ADDR
C
C    PROGRAMMER    - DALE MARKHAM  01/13/77
C                    PETER DENATALE 06/29/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 10/21/77
C                    SAVITA GOEL    06/03/87 (CDS FOR A900)
C                    Jim Ryan 89.06.29 Character strings and clean up.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                 implimented.
C                    David Gordon 94.01.21 Access code 'ETD3 CONT' added,
C                                 correction to remove third order tide.
C                    David Gordon 94.04.06 Changed to 'Implicit None'
C                    David Gordon 94.04.07 'ETD3CONT' changed to 'ETD IERS'.
C                                 Correction to use in SOLVE to get strict
C                                 equivalent of IERS Standards (1992) Solid
C                                 Earth Tides.  
C                    David Gordon 94.04.27 'ETD3CONT', 'ETDKCONT, and 'ETD2CONT'
C                                 Lcodes added.
C                    David Gordon 94.07.12 Changed 'ETHLCONT' to 'ETD2CONT'.
C                    David Gordon 94.08.23 Removed 'ETD IERS' Lcode. Changed
C                                 default back to the IERS Standards. Updated
C                                 documentation.
C                    David Gordon 98.06.24 Update Lcodes for Calc 9.0.
C                                 'ETD PART' changed to 'ET96PART'. Deletes 
C                                 added for old calc 8.x contributions. 
C                    David Gordon 98.08.27 'ET96PART' changed to 'ETJMGPAR',
C                                 in case we need to blame John Gipson.
C                    David Gordon 99.01.06 Added Lcodes 'C82ETCON' and 
C                                 'ELASTCON'. Mods for correlator use. 
C                    David Gordon 99.10.15 Added Lcode 'PERMDEF ' to hold
C                                 contribution to restore the permanent 
C                                 deformation effect. 
C
C 1.3   ETDA PROGRAM STRUCTURE
C
C   ADD for Earth Tide module text message.
      CALL ADDA (1,'ETD MESS','Earth Tide message definition   ',
     *     40, 1, 1 )
C
C   ADD for Earth Tide module flow control message.
      CALL ADDA (1,'ETD CFLG','Earth Tide flow control mess def',
     *     40,1,1)
C
C   ADD for Earth Tide contributions.
      CALL ADDR (2,'ETD CONT','Earth tide contributions def.   ',
     *     2, 1, 1 )
C
      IF (Calc_user .eq. 'A') THEN
C   ADD for Earth Tide partials.
       CALL ADDR (2,'ETJMGPAR','Gipson Earth Tide Partials      ',
     *     2, 4, 6 )
C
       CALL ADDR (2,'C82ETCON','Calc8.2 Earth Tide Corrections  ',
     *     2, 1, 1 )
C
       CALL ADDR (2,'PERMDEF ','Permanent Deformation Contrib.  ',
     *     2, 1, 1 )
C
       CALL ADDR (2,'ELASTCON','Elastic Earth Tide Corrections  ',
     *     2, 1, 1 )
C
C   Deletes for obsolete Calc 8.x contributions
        CALL DELR (2,'ETD PART')
        CALL DELR (2,'ETD3CONT')
        CALL DELR (2,'ETDKCONT')
        CALL DELR (2,'ETD2CONT')
C
      ENDIF
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE ETDI
      IMPLICIT None
C
C     ETDI is the Earth Tide Module input and initialization section.
C
C     ETDI Program Interface:
C
C     Common blocks used:
C
      INCLUDE 'ccon.i'
C         Variables 'from':
C           1. KETDC  -  The module flow control flag.
C                        = 0 => Default, IERS 1996 Earth Tide Model  
C                        = 1 => Earth tide model OFF, no corrections computed 
C             2. KETDD - Module debug output control flag.
C
C   Program specifications:
c     INTEGER*2 NDO(3), KERR
      INTEGER*2      LETDM(40),  New_ON(40),  LOFF(40)
      CHARACTER*40 C_LETDM(2), C_New_ON(2), C_LOFF(2) 
      EQUIVALENCE (C_LETDM,LETDM), (C_New_ON,New_ON), (C_LOFF,LOFF)
C
      DATA C_LETDM 
     *   /'Earth Tide Module - IERS 1996 model, las',
     *    't modified 99OCT18, David Gordon, GSFC  '/ 
      DATA C_New_ON                                    ! KETDC=0
     *   /'IERS 1996 Earth Tide Model              ',
     *    '                                        '/
      DATA C_LOFF                                      ! KETDC=1
     *   /'Earth tide module is turned off.        ',
     *    '                                        '/
C
C     Database access:
C          'PUT' variables:
C             1. LETDM(40)   - The Earth Tide Module text message.
C             2. New_ON(40)  - The 1996 IERS Earth tide model.
C             3. LOFF(40)    - The Earth tides completely turned OFF message.
C          Access codes:
C             1. 'ETD MESS' - The database access code for the Earth Tide
C                             Module text message.
C             2. 'ETD DATA' - The database access code for the Earth Tide data.
C             3. 'ETD CFLG' - The database access code for the Earth Tide 
C                             Module flow control message.
C
C     Subroutine Interface:
C          Caller subroutines: INITL
C          Called subroutines: GET4, KILL, PUTA
C
C     Program variables:
C           1. KERR   - The database error return flag.
C           2. NDO(3) - The database return error indices.
C
C     Programmer   - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 03/24/78
C                    BRUCE SCHUPLER 11/02/78
C                    DAVID GORDON   07/31/84
C                    Jim Ryan 89.06.29 Character strings and clean up comments.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    David Gordon 93.11.19 New database access messages for IERS
C                                 Earth tide computation.
C                    David Gordon 94.01.24 Database access codes modified for
C                                 third order Earth tide terms.
C                    David Gordon 94.04.06 Changed to 'Implicit None'
C                    David Gordon 94.04.27 New common block 'tide_stuff'; 
C                                 XLOVEH_IERS and XLOVEL_IERS defined as h2 and
C                                 l2 from database (which will be the IERS 
C                                 Standards in Calc 8.0).
C                    David Gordon 94.07.12 Database header messsages revised,
C                                 message variable names changed. 
C                    David Gordon 94.08.08 Love numbers changed, according to
C                                 July 28, 1994 letter from Sonny Mathews to
C                                 John Gipson.
C                    David Gordon 94.08.09 Love numbers changed again per Sonny
C                                 Mathews communication to John Gipson.
C                    David Gordon 94.08.24 Changing back to IERS model as the
C                                 default. Messages updated. XLOVEH and XLOVEL
C                                 reset to IERS values. XLOVEH_alt and
C                                 XLOVEL_alt for Mathews Love numbers. 
C                    David Gordon 96.02.07 Changing XLOVEH_alt and XLOVEL_alt
C                                 to 0.6081 and .0845 per Feb. 6, 1996 John
C                                 Gipson memo.
C                    David Gordon 98.06.24 Extensive mods in Calc 9.0 for 
C                                 IERS 1996 Earth tide model. Redefined KETDC
C                                 options.
C                    David Gordon 98.10.01 Remove common block ETDCM. Remove 
C                                 GET of 'ETD DATA' - no longer needed. 
C
C  ETDI Program Structure.
C
C   PUT Earth tide module text message into database.
      CALL PUTA ('ETD MESS      ', LETDM, 40, 1, 1 )
C
C   PUT the flow control text message depending on KETDC.
      IF (KETDC .EQ. 0) CALL PUTA('ETD CFLG      ',New_ON,40,1,1)
      IF (KETDC .EQ. 1) CALL PUTA('ETD CFLG      ',LOFF,40,1,1)
C
C   Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE ETDG ( R2000, SITLAT, SITLON, SUN, TCTOCF, RTTOCF, 
     *                   USITEP, USITEV, XMOON, EARTH, GAST, STAR, fa, 
     *                   fad, cent, GEOLAT, TIDEP, TIDEV )
      IMPLICIT None 
C
C     ETDG is the Solid Earth Tide geometry section. It calculates the 
C     Earth surface crustal displacements and velocities due to Earth 
C     tidal effects at each site using the IERS 1996 Earth tide model.
C
C     References:  1. IERS Technical Note 21, IERS Conventions (1996); 
C                     Dennis D. McCarthy (ed.); pages 56-65.
C                  2. Memo: "Explicit Form of Partials", John M. Gipson,
C                     8 September 1998.
C
C     ETDG Program Interface:
C
C       Calling sequence:
C           INPUT VARIABLES:
C             1. R2000(3,3,3) - The complete crust fixed to J2000.0 rotation
C                               matrix and its first two CT time derivatives.
C                               (unitless, 1/sec, 1/sec**2)
C             2. SITLAT(2)    - The geodetic latitude of each site. (RAD)
C             3. SITLON(2)    - The longitude of each site. (RAD)
C             4. SUN(3,2)     - The J2000.0 geocentric Sun position and
C                               velocity vectors. (M, M/SEC)
C             5. TCTOCF(3,3,2)- The rotation matrix which rotates the
C                               topocentric reference system to the crust
C                               fixed reference system at each site. (unitless)
C             6. RTTOCF(3,3,2)- The rotation matrix which rotates the
C                               'radial-transverse' reference system to the
C                               crust fixed reference system at each site.
C             7. USITEP(3,2)  - The J2000.0 geocentric site position vectors
C                               uncorrected for Earth tidal effects. (M)
C             8. USITEV(3,2)  - The J2000.0 geocentric site velocity vectors
C                               uncorrected for Earth tidal effects. (M/SEC)
C             9. XMOON(3,2)   - The J2000.0 geocentric Moon position and
C                               velocity vectors. (M, M/SEC)
C            10. EARTH(3,3)   - The solar system barycentric Earth position,
C                               velocity, and acceleration vectors. The first
C                               index runs over the vector components and the
C                               second runs over the time derivatives.
C                               (m, m/sec, m/sec**2)
C            11. GAST(2)      - The Greenwich Apparent Siderial time and its
C                               CT time derivative. (RAD, RAD/SEC)
C            12. STAR(3)      - The J2000.0 source unit vector.
C            13. fa(5)        - The fundamental arguments (arcsec)
C            14. fad(5)       - The CT time derivatives of the fundamental 
C                               arguments. (arcsec/century)
C            15. cent         - The number of Julian centuries elapsed since the
C                               epoch January 1.5, 2000. (centuries)
C            16. GEOLAT(2)    - The geocentric latitude at each site. (rad)
C
C           OUTPUT VARIABLES:
C             1. TIDEP(3,2)   - The corrections to the J2000.0 geocentric site
C                               position vectors due to Earth tidal effects at
C                               each site. (M)
C             2. TIDEV(3,2)   - The corrections to the J2000.0 geocentric site
C                               velocity vectors due to Earth tidal effects at
C                               each site. (M/SEC)
C
C     Common blocks used:
C
      INCLUDE 'ccon.i'
C          VARIABLES 'FROM':
C           1. KETDC  -  The module flow control flag.
C                        = 0 => Default, IERS 1996 Earth Tide Model plus
C                               contributions for optional use (in SOLVE)
C                               of the IERS 1992 Earth tide + third order
C                               term. 
C                        = 1 => Earth tide model OFF, no corrections computed.
C           2. KETDD  -  The debug output flag.
C
      INCLUDE 'cphys.i'
C            Variables 'from':
C              2. GMMOON  - The mass of the Moon multiplied by the Universal
C                           gravitational constant. (M**3/SEC**2)
C              3. GMSUN   - The mass of the Sun multiplied by the Universal
C                           Gravitational constant. (M**3/SEC**2)
C              4. GMEARTH - The mass of the Earth multiplied by the Universal
C                           Gravitational constant. (M**3/SEC**2)
C              5. REARTH  - The equatorial radius of the Earth. (meters)
C
      Real*8 DETDP9(2,4,6)
      COMMON / ETDCM / DETDP9
C     Variables 'to':
C      1. DETDP9(2,4,6) - The partial derivatives of the delay and rate
C                         with respect to the site dependent Earth tide
C                         parameters, as defined in John Gipson's Sept. 8, 
C                         1998 memo.
C                         The first index runs over the observing sites.
C                         The second index runs over the two Love numbers AND
C                          their real and imaginary parts. 
C                           1 => h2, Real
C                           2 => h2, Imaginary
C                           3 => l2, Real
C                           4 => h2, Imaginary
C                         The third index runs over the 3 principal bands, 
C                          AND the delay and rate.
C                           1 => semi-diurnal, Delay
C                           2 => diurnal,      Delay
C                           3 => long period,  Delay
C                           4 => semi-diurnal, Rate 
C                           5 => diurnal,      Rate 
C                           6 => long period,  Rate 
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.
C
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C 
C          Variables "From": 
C            1. PI     - THE MATHEMATICAL CONSTANT PI (UNITLESS) 
C            2. TWOPI  - PI * 2.0D0 (UNITLESS) 
C            3. HALFPI - PI / 2.0D0 (UNITLESS) 
C            4. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
C            5. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                        (RAD/ARCSECOND) 
C            6. CONVHS - THE CONVERSION FACTOR FROM TIME SECONDS TO RADIANS
C                        (RADIANS / TIME SECOND) 
C            7. SECDAY - THE NUMBER OF TIME SECONDS PER DAY (SECONDS / DAY)
C
      INCLUDE 'cuser.i'
C           1. Calc_user - Analysis center (A) or Correlator (C) user
C           2. Use_tide  - For correlators only: 
C                          = 'C8' => use IERS 1992 tide model + third order 
C                                    term (tide free crust, pemanent tide
C                                    not removed).
C                          = 'C9' => use IERS 1996 tide model without 
C                                    removing the permanent tide (as in 
C                                    Calc 9.0)
C                          = 'RC' => use IERS 1996 tide model with 
C                            permanent tide removed. (real crust frame)
C
C   Program specifications:
C
      Real*8 delta_r(3,2), ddelta_r(3,2), delta_r2(3,2), 
     *       ddelta_r2(3,2), delta_r3(3,2), ddelta_r3(3,2),
     *       delta_r2_dh2(3,2), ddelta_r2_dh2(3,2),
     *       delta_r2_dl2(3,2), ddelta_r2_dl2(3,2)
      Real*8 R2000(3,3,3), SITLAT(2), SUN(3,2), TCTOCF(3,3,2), 
     *       TIDEP(3,2), TIDEV(3,2), USITEP(3,2), USITEV(3,2), 
     *       XMOON(3,2), SITLON(2), GAST(2), STAR(3), EARTH(3,3),
     *       fa(5), fad(5), cent, RTTOCF(3,3,2), GEOLAT(2)
      Real*8 SunLat, SunLon, MoonLat, MoonLon, h2_R_s(3,2), 
     *       h2_I_s(3,2), h2_R_d(3,2), h2_I_d(3,2), h2_R_l(3,2), 
     *       h2_I_l(3,2), l2_R_s(3,2), l2_I_s(3,2), l2_R_d(3,2), 
     *       l2_I_d(3,2), l2_R_l(3,2), l2_I_l(3,2), xsign, 
     *       l2_R_s2000(3,2), l2_I_s2000(3,2), l2_R_d2000(3,2), 
     *       l2_I_d2000(3,2), l2_R_l2000(3,2), l2_I_l2000(3,2), CFtmp(3)
      Real*8 dSunLat, dSunLon, dMoonLat, dMoonLon, dh2_R_s(3,2), 
     *       dh2_I_s(3,2), dh2_R_d(3,2), dh2_I_d(3,2), dh2_R_l(3,2), 
     *       dh2_I_l(3,2), dl2_R_s(3,2), dl2_I_s(3,2), dl2_R_d(3,2), 
     *       dl2_I_d(3,2), dl2_R_l(3,2), dl2_I_l(3,2),
     *       dl2_R_s2000(3,2), dl2_I_s2000(3,2), dl2_R_d2000(3,2), 
     *       dl2_I_d2000(3,2), dl2_R_l2000(3,2), dl2_I_l2000(3,2),
     *       vect1(3), vect2(3), vect3(3)
      Real*8 Vecmg, Dotp
      Real*8 r_vec(3),dr_vec(3), r_mag,dr_mag, r_hat(3),dr_hat(3),
     *       Rmoon(3),dRmoon(3), Rmoon_mag, dRmoon_mag, 
     *       Rmoon_hat(3),dRmoon_hat(3), Rmoon_dot_r,dRmoon_dot_r,
     *       Rsun(3),dRsun(3), Rsun_mag,dRsun_mag,
     *       Rsun_hat(3),dRsun_hat(3), Rsun_dot_r,dRsun_dot_r,
     *       term1M,dterm1M, term2,dterm2, term3M,dterm3M,
     *       term4M,dterm4M, term1M_2,dterm1M_2, term1S,dterm1S,
     *       term3S,dterm3S, term4S,dterm4S, term1S_2,dterm1S_2
      Real*8 term5M, dterm5M, term5S, dterm5S
      Integer*4 c1, c2
      Real*8 GSL2(2), Phi(2), h_sub_2, l_sub_2, h_sub_3, l_sub_3,
     *       term01M, dterm01M, term02, dterm02, term03M, dterm03M, 
     *       term04M, dterm04M, term05M, dterm05M, 
     *       PHImoon, dPHImoon, Theta_moon, dTheta_moon,
     *       RA_moon, dRA_moon, Lamda_moon, dLamda_moon,
     *       P12_moon, P12_moon_a, P22_moon, P22_moon_a,
     *       dP12_moon, dP12_moon_a, dP22_moon, dP22_moon_a,
     *       PHIsun, dPHIsun, Theta_sun, dTheta_sun,
     *       RA_sun, dRA_sun, Lamda_sun, dLamda_sun,
     *       P12_sun, P12_sun_a, P22_sun, P22_sun_a, 
     *       dP12_sun, dP12_sun_a, dP22_sun, dP22_sun_a,
     *       TR2000(3,3,2), CFRmoon(3), CFRsun(3), dCFRmoon(3), 
     *       dCFRsun(3), CFM1(3), CFM2(3), CFRmoon_mag, dCFRmoon_mag, 
     *       CFRsun_mag, dCFRsun_mag
      Real*8 topo11(3,2), dtopo11(3,2), CF11(3,2), dCF11(3,2),
     *       topo12(3,2), dtopo12(3,2), CF12(3,2), dCF12(3,2),
     *       topo13(3,2), dtopo13(3,2), CF13(3,2), dCF13(3,2),
     *       topo14(3,2), dtopo14(3,2), CF14(3,2), dCF14(3,2),
     *       topo15(3,2), dtopo15(3,2), CF15(3,2), dCF15(3,2),
     *       topo16(3,2), dtopo16(3,2), CF16(3,2), dCF16(3,2),
     *       topo17(3,2), dtopo17(3,2), CF17(3,2), dCF17(3,2),
     *       topo17el(3,2), CF17el(3,2), del17el(3,2),
     *       vec1(3), vec2(3), T17A, T17B
      Real*8 delta11(3,2), ddelta11(3,2), delta12(3,2), ddelta12(3,2),
     *       delta13(3,2), ddelta13(3,2), delta14(3,2), ddelta14(3,2), 
     *       delta15(3,2), ddelta15(3,2), delta16(3,2), ddelta16(3,2), 
     *       delta17(3,2), ddelta17(3,2),  
     *       l1_11, l1_12, h_I13, l_I13, h_I14, l_I14,
     *       theta15(11), dtheta15(11), theta16(5), dtheta16(5)
      Real*8 CPhi, SPhi, C2Phi, S2Phi, CsqPhi, SsqPhi, CLamMoon, 
     *       SLamMoon, CLLMoon, SLLMoon, C2LLMoon, S2LLMoon, dCLamMoon,
     *       dSLamMoon, dCLLMoon, dSLLMoon, dC2LLMoon, dS2LLMoon,  
     *       CPhiMoon, SPhiMoon, CsqPhiMoon, SsqPhiMoon, C2PhiMoon, 
     *       S2PhiMoon, dCPhiMoon, dSPhiMoon, dCsqPhiMoon, dSsqPhiMoon,
     *       dC2PhiMoon, dS2PhiMoon, CLamSun, SLamSun, CLLSun, SLLSun,
     *       C2LLSun, S2LLSun, dCLamSun, dSLamSun, dCLLSun, dSLLSun,
     *       dC2LLSun, dS2LLSun, CPhiSun, SPhiSun, CsqPhiSun, 
     *       SsqPhiSun, C2PhiSun, S2PhiSun, dCPhiSun, dSPhiSun, 
     *       dCsqPhiSun, dSsqPhiSun, dC2PhiSun, dS2PhiSun,
     *       C2LamMoon, S2LamMoon, C2LamSun, S2LamSun,
     *       dC2LamMoon, dS2LamMoon, dC2LamSun, dS2LamSun
      Real*8 TD82P(3,2), TD82V(3,2), ELASP(3,2), ELASV(3,2), 
     *       delperm(3,2), ddelperm(3,2)
      Common /TIDE8/ TD82P, TD82V, ELASP, ELASV, delperm, ddelperm
C
      Data h_sub_3  /0.292D0/
      Data l_sub_3  /0.015D0/
C
C     Real*4 Dood_arg(10,11), Table73a(10,11), Table73b(12,5)
      Real*4                  Table73a(10,11), Table73b(12,5)
      Integer*4 itide, ll, kk, k, k8, L, J, I, jf
C
      Real*8 fund_arg(6), arg, dfund_arg(6), d_arg
      Real*8 geo_lat(2)
C
      Data Table73a / 
     *  1.,  0.,  2.,  0.,  2., -1., -0.11, -0.01, -0.09,  0.00,
     *  0.,  0.,  2.,  0.,  1., -1., -0.12, -0.01, -0.10,  0.00, 
     *  0.,  0.,  2.,  0.,  2., -1., -0.63, -0.04, -0.53,  0.02,
     *  1.,  0.,  0.,  0.,  0., -1.,  0.07,  0.00,  0.06,  0.00, 
     *  0.,  1.,  2., -2.,  2., -1., -0.06,  0.00, -0.05,  0.00,
     *  0.,  0.,  2., -2.,  2., -1., -1.29,  0.05, -1.23,  0.07,
     *  0.,  0.,  0.,  0., -1., -1., -0.23,  0.01, -0.22,  0.01, 
     *  0.,  0.,  0.,  0.,  0., -1., 12.25, -0.65, 12.04, -0.72,
     *  0.,  0.,  0.,  0.,  1., -1.,  1.77, -0.09,  1.74, -0.10, 
     *  0., -1.,  0.,  0.,  0., -1., -0.51,  0.03, -0.50,  0.03,
     *  0.,  0., -2.,  2., -2., -1., -0.11,  0.01, -0.11,  0.01 /
C
      Data Table73b / 
     *  0.,  0.,  0.,  0.,  1.,  0., -0.05,  0.00,  0.47,  0.23,
     *                                              0.16,  0.07,
     *  0.,  0., -2.,  2., -2.,  0.,  0.05,  0.00, -0.20, -0.12, 
     *                                             -0.11, -0.05,
     * -1.,  0.,  0.,  0.,  0.,  0.,  0.06,  0.00, -0.11, -0.08,
     *                                             -0.09, -0.04,
     *  0.,  0., -2.,  0., -2.,  0.,  0.12,  0.00, -0.13, -0.11, 
     *                                             -0.15, -0.07,
     *  0.,  0., -2.,  0., -1.,  0.,  0.05,  0.00, -0.05, -0.05,
     *                                             -0.06, -0.03 /
C
C     External input/output - Possible debug output.
C
C     Subroutine Interface:
C           Caller subroutines: DRIVG
C           Called subroutines: DCOS, DOTP, ROTATE, VECAD, VECRT, VECMG, VECSB
C
C     Constants used:  GMMOON, GMSUN, GMEARTH, REARTH
C
C    Program variables:
C
C       1. Rmoon(3), Rsun(3), dRmoon(3), dRsun(3) - Geocentric position and
C            velocity vectors of the Moon and the Sun. (m, m, m/s, m/s)
C       2. Rmoon_mag, dRmoon_mag, Rsun_mag, dRsun_mag - Magnitude of geocentric
C             vectors and their derivatives. (m, m, m/s, m/s)
C       3. Rmoon_hat(3), Rsun_hat(3), dRmoon_hat(3), dRsun_hat(3) - Geocentric
C             unit vectors and their derivatives. (unitless)
C       4. r_vec(3), dr_vec(3) - Geocentric position and velocity of current
C             site. (m, m/s)
C       5. r_mag, dr_mag - Magnitude of geocentric site vector and its
C             derivative. (m, m/s)
C       6. r_hat(3), dr_hat(3) -  Geocentric unit vector of current site and
C             its derivative. (unitless)
C       7. Rmoon_dot_r, dRmoon_dot_r, Rsun_dot_r, dRsun_dot_r - Dot products of
C             Moon/Sun unit vectors with site unit vector and their derivatives.
C       8. term---, dterm---  - terms used for the second and third order tide
C             and partials computations, too numerous to mention individually.
C       9. delta_r2(3,2)-The second order tidal displacement at each site. First
C             index is X,Y,Z. Second index runs over sites. (m)
C      10. ddelta_r2(3,2)-The second order tidal velocity at each site. First
C             index is X,Y,Z. Second index runs over sites. (m/s)
C      11. fund_arg(6) - The five fundamental arguments (fa(5))
C             converted to radians and the Greenwich apparent siderial time
C             plus PI. (radians)
C      12. dfund_arg(6) - Time derivatives of above. (radians/sec)
C      13. geo_lat(2) - The geocentric latitude of each site. (RAD)
C      14. SunLat  - The J2000 geocentric latitude (declination) of the Sun.
C      15. MoonLat - The J2000 geocentric latitude (declination) of the Moon.
C      16. SunLon  - The J2000 geocentric longitude (RA) of the Sun.
C      17. MoonLon - The J2000 geocentric longitude (RA) of the Moon.
C
C      1. delta_r(3,2) - Sum of 
C             First index runs over X,Y,Z; second runs over sites. (meters)
C      2. ddelta_r(3,2) - Time derivative of above, i.e., IERS tidal velocities.
C             First index runs over X,Y,Z; second runs over sites. (m/sec) 
C      5. delta_r3(3,2) - The third order tidal displacement at each site,
C             from the IERS 1996 model. First index runs over X,Y,Z;
C             second runs over sites. (meters)
C      6. ddelta_r3(3,2) - Time derivative of above, third order tidal velocity.
C             First index runs over X,Y,Z; second runs over sites. (m/sec)
C
C 4.2.9 PROGRAMMER - 
C           DALE MARKHAM  01/13/77
C           PETER DENATALE 07/12/77
C           BRUCE SCHUPLER 03/24/78
C           BRUCE SCHUPLER 11/02/78
C           BRUCE SCHUPLER 01/14/80
C           DAVID GORDON   07/20/84
C           Jim Ryan 89.06.29 Character strings and clean up.
C           Jim Ryan 89:10:05 CPHYS common made an include file.
C           David Gordon 93Oct/Nov. Added IERS tide computation,
C                        restructured subroutine, new KETDC options.
C           David Gordon 94.01.24 Third order tide components added.
C           David Gordon 94.01.27 Partials with respect to lag angle
C                        added.
C           David Gordon 94.04.05 Coding added to compute the six
C                        frequency dependent Love number correction
C                        vertical terms (K1, O1, P1, Psi, and 2 
C                        unnamed terms). New terms will replace old
C                        K1 term in IERS tide computation (default).
C                        K1 left in old tide option. 
C           David Gordon 94.04.06 Changed to 'Implicit None'
C           David Gordon 94.04.27 New common block 'tide_stuff'. 
C           David Gordon 94.04.28 Term for change in Love/Shida #'s
C                        (r2_IERS) added. Separate terms kept for
C                        all non "pure" IERS contributions.
C           David Gordon 94.05.23 Removed second dimensioning of XK1V.
C           David Gordon 94.07.11 Changed absolute value of geocentric
C                        site radius to Earth's equatorial radius
C                        (REARTH) in second and third order tide 
C                        computation. Added change to second order
C                        term to IERS-restorer variable. Reference
C                        is Sonny Mathews via John Gipson. 
C           David Gordon 94.08.09 Changed Love numbers for 
C                        Gipson/Mathews tides. Changed amplitudes for
C                        O1, P1, K1, Psi, ... terms. John Gipson
C                        Aug. 9, 1994 memo (reference #7 above).
C           David Gordon 94.08.25 Changed default back to IERS
C                        Standards. Three correction terms kept to
C                        convert to Gipson/Mathews Earth tide model.
C           David Gordon 96.01.10 Changing geodetic latitude to 
C                        geocentric latitude for computation of K1
C                        and K1-plus tide corrections.
C           David Gordon 96.02.07 Changing 2 arguments (signs
C                        reversed) in Tide_arg and 3 amplitudes in
C                        Tide_val. See 96Feb06 J. Gipson memo.   
C           David Gordon 98.05.06 Old (Calc 5/6/7) computations removed. 
C                        Calc 8 computations moved into subroutine ETDC8.
C                        Calc 9 (1996 IERS Conventions) computations coded
C                        and put into subroutine ETDC9.
C           David Gordon 98.06.24 Subroutine ETDC8 deleted. Subroutine ETDG
C                        reduced to a driver for ETDC9.
C           David Gordon 98.07.29 Added 'Include cobsn.i' with
C                        variable Nzero, and code to skip topocentric
C                        computations for a station at the geocenter.
C           David Gordon 98.07.31 Subroutine ETDC9 folded back in to ETDG.
C                        Mods to set tides to zero for a geocenter station.
C           David Gordon 98.09.30 Finished coding Earth tide partials based
C                        on John Gipson's memo of 8 Sept. 1998. Restructured
C                        and simplifed earlier tide components and time 
C                        derivatives. Removed all but DETDP9 from common 
C                        block ETDCM. Removed the block data subprogram.
C           David Gordon 98.12.29 Corrected errors in Table73b, found by 
C                        John Gipson.
C           David Gordon 99.01.06 Added call to subroutine ETDC8 to compute
C                        earlier Calc 8 Earth tide. Skipping of partials
C                        for correlator usage.
C           David Gordon 99.10.01 Adding 'Use_Tide' parameter to allow 
C                        using the Calc 8 tide model at the correlators.
C           David Gordon 99.10.13 Added computation of permanent tide vectors.
C           David Gordon 99.10.18 Redefined 'Use_Tide' parameter to allow 
C                        using the Calc 8 tide model, the Calc 9.0 model,
C                        or the real crust model at the correlators.
C                                 
C****************************************************************************
C     ETDG PROGRAM STRUCTURE
C****************************************************************************
C
C  Check KETDC to determine if the Earth tide module is to be turned OFF.
      IF (KETDC .eq. 1)  Then
        DO 220  L = 1,2
          DO 210  I = 1,3
            TIDEP(I,L) = 0.D0
            TIDEV(I,L) = 0.D0
           delta_r(I,L) = TIDEP(I,L)
          ddelta_r(I,L) = TIDEV(I,L) 
  210     CONTINUE
  220   CONTINUE
       RETURN   
      ENDIF
C
C  Check for which tide model to use at the correlators:
       If (Calc_user .eq. 'C' .and. Use_tide .eq. 'C8') Then
C        Use Calc 8.x tide model
         Go to 1500
       Endif
C
C****************************************************************************
C     The Earth tide geometry for site #1 and site #2 are calculated
C     separately by running twice through a loop. 
C     The program uses the IERS Conventions (1996) model. 
C
C
C Preliminary Sun and Moon (station independent) quantities:
C
C   Vectors from the geocenter to the Moon and the Sun and their derivatives
       Do kk=1,3
         Rmoon(kk)  = XMOON(kk,1)
         dRmoon(kk) = XMOON(kk,2)
         Rsun(kk)  = SUN(kk,1)
         dRsun(kk) = SUN(kk,2)
       Enddo
C
C   Magnitude of vector from geocenter to the Moon and its derivative
       Rmoon_mag = vecmg(Rmoon)
       dRmoon_mag = Dotp(Rmoon,dRmoon)/Rmoon_mag
C
C   Magnitude of vector from geocenter to the Sun and its derivative
       Rsun_mag = vecmg(Rsun)
       dRsun_mag = Dotp(Rsun,dRsun)/Rsun_mag
C
C   Unit vectors from geocenter to the Moon and Sun and their derivatives
       call vunit(Rmoon,Rmoon_hat)
       call vunit(Rsun,Rsun_hat)
       do kk=1,3
        dRmoon_hat(kk) = dRmoon(kk)/Rmoon_mag - Rmoon(kk)*dRmoon_mag /
     *                   Rmoon_mag**2
        dRsun_hat(kk) = dRsun(kk)/Rsun_mag - Rsun(kk)*dRsun_mag /
     *                   Rsun_mag**2
       enddo
C
C  Get Crust-fixed latitude and longitude of the Sun and the Moon.
C  Rotate Moon and Sun vectors from the J2000 coordinate system to the
C  geocentric crust-fixed system.
C    First compute the rotation matrix which rotates from the J2000.0
C    reference system to the geocentric crust fixed system, and its
C    time derivative.
           CALL MTRAN ( R2000(1,1,1), TR2000(1,1,1) )
           CALL MTRAN ( R2000(1,1,2), TR2000(1,1,2) )
C    Then rotate the Moon and Sun unit vectors to the crust fixed system.
           CALL VECRT ( TR2000(1,1,1),Rmoon(1),CFRmoon(1))
           CALL VECRT ( TR2000(1,1,1),Rsun(1),CFRsun(1))
C    And find their derivatives in the crust fixed system
           CALL VECRT ( TR2000(1,1,1),dRmoon,CFM1)
           CALL VECRT ( TR2000(1,1,2), Rmoon,CFM2)
           CALL VECAD ( CFM1, CFM2, dCFRmoon )
           CALL VECRT ( TR2000(1,1,1),dRsun,CFM1)
           CALL VECRT ( TR2000(1,1,2), Rsun,CFM2)
           CALL VECAD ( CFM1, CFM2, dCFRsun )
C
      CFRmoon_mag = VECMG( CFRmoon )
      dCFRmoon_mag = DOTP( CFRmoon, dCFRmoon ) / CFRmoon_mag 
C
      CFRsun_mag = VECMG( CFRsun )
      dCFRsun_mag = DOTP( CFRsun, dCFRsun ) / CFRsun_mag 
C
C  Find body-fixed geocentric latitude of Sun/moon 
      PHImoon  = DASIN( CFRmoon(3)/CFRmoon_mag )
      dPHImoon = ( dCFRmoon(3)/CFRmoon_mag - 
     *             CFRmoon(3)*dCFRmoon_mag/CFRmoon_mag**2 ) / 
     *             DSQRT( 1.D0 - (CFRmoon(3)/CFRmoon_mag)**2 )
      PHIsun   = DASIN( CFRsun(3)/CFRsun_mag )
      dPHIsun  = ( dCFRsun(3)/CFRsun_mag - 
     *             CFRsun(3)*dCFRsun_mag/CFRsun_mag**2 ) / 
     *             DSQRT( 1.D0 - (CFRsun(3)/CFRsun_mag)**2 )
C
C  Find body-fixed East longitude (from Greenwich) of moon 
      Lamda_moon = DATAN2( CFRmoon(2),CFRmoon(1) )
      dLamda_moon =  1.D0 / ( 1.D0 + (CFRmoon(2)/CFRmoon(1))**2 )
     *      *  ( dCFRmoon(2)/CFRmoon(1) - 
     *        CFRmoon(2)*dCFRmoon(1)/CFRmoon(1)**2 )
C
C  Find body-fixed East longitude (from Greenwich) of Sun 
      Lamda_sun = DATAN2( CFRsun(2),CFRsun(1) )
      dLamda_sun =  1.D0 / ( 1.D0 + (CFRsun(2)/CFRsun(1))**2 )
     *      * ( dCFRsun(2)/CFRsun(1) - 
     *        CFRsun(2)*dCFRsun(1)/CFRsun(1)**2 )
C
C  Compute heavily used sine and cosine terms and derivatives of Sun and
C       Moon crust-fixed latitude and longitude 
C
       CPhiMoon = DCOS(PhiMoon)
       SPhiMoon = DSIN(PhiMoon)
      dCPhiMoon = -SPhiMoon * dPhiMoon
      dSPhiMoon =  CPhiMoon * dPhiMoon
C    
       CsqPhiMoon = CPhiMoon * CPhiMoon
       SsqPhiMoon = SPhiMoon * SPhiMoon
      dCsqPhiMoon = 2.D0 * CPhiMoon * dCPhiMoon
      dSsqPhiMoon = 2.D0 * SPhiMoon * dSPhiMoon
C
       C2PhiMoon = DCOS(2.D0*PhiMoon)
       S2PhiMoon = DSIN(2.D0*PhiMoon)
      dC2PhiMoon = -2.D0 * S2PhiMoon * dPhiMoon
      dS2PhiMoon =  2.D0 * C2PhiMoon * dPhiMoon
C
       CLamMoon = DCOS(Lamda_Moon)
       SLamMoon = DSIN(Lamda_Moon)
      dCLamMoon = -SLamMoon * dLamda_Moon
      dSLamMoon =  CLamMoon * dLamda_Moon
C
       C2LamMoon = DCOS(2.D0 * Lamda_Moon)
       S2LamMoon = DSIN(2.D0 * Lamda_Moon)
      dC2LamMoon = -S2LamMoon * 2.D0 * dLamda_Moon
      dS2LamMoon =  C2LamMoon * 2.D0 * dLamda_Moon
C
       CPhiSun  = DCOS(PhiSun )
       SPhiSun  = DSIN(PhiSun )
      dCPhiSun  = -SPhiSun  * dPhiSun 
      dSPhiSun  =  CPhiSun  * dPhiSun 
C    
       CsqPhiSun  = CPhiSun  * CPhiSun 
       SsqPhiSun  = SPhiSun  * SPhiSun 
      dCsqPhiSun  = 2.D0 * CPhiSun  * dCPhiSun 
      dSsqPhiSun  = 2.D0 * SPhiSun  * dSPhiSun 
C
       C2PhiSun  = DCOS(2.D0*PhiSun )
       S2PhiSun  = DSIN(2.D0*PhiSun )
      dC2PhiSun  = -2.D0 * S2PhiSun  * dPhiSun 
      dS2PhiSun  =  2.D0 * C2PhiSun  * dPhiSun 
C
       CLamSun  = DCOS(Lamda_Sun )
       SLamSun  = DSIN(Lamda_Sun )
      dCLamSun  = -SLamSun  * dLamda_Sun 
      dSLamSun  =  CLamSun  * dLamda_Sun 
C
       C2LamSun = DCOS(2.D0 * Lamda_Sun)
       S2LamSun = DSIN(2.D0 * Lamda_Sun)
      dC2LamSun = -S2LamSun * 2.D0 * dLamda_Sun 
      dS2LamSun =  C2LamSun * 2.D0 * dLamda_Sun 
C
      If(KETDD .ne. 0) Then
       write(6,'(/,10x,"Debug from Subroutine ETDC9: KETDC =",I2)')KETDC
       write(6,8)' Rmoon        ', Rmoon
       write(6,8)' dRmoon       ', dRmoon
       write(6,8)' Rmoon_mag, dRmoon_mag ', Rmoon_mag, dRmoon_mag
       write(6,8)' Rmoon_hat    ', Rmoon_hat
       write(6,8)' dRmoon_hat   ', dRmoon_hat
       write(6,8)' Rsun         ', Rsun
       write(6,8)' dRsun        ', dRsun
       write(6,8)' Rsun_mag, dRsun_mag ', Rsun_mag, dRsun_mag
       write(6,8)' Rsun_hat     ', Rsun_hat
       write(6,8)' dRsun_hat    ', dRsun_hat
       write(6,8)' CFRmoon  ', CFRmoon
       write(6,8)' CFRsun  ', CFRsun 
       write(6,8)' CFM1moon ', CFM1   
       write(6,8)' CFM2moon ', CFM2   
       write(6,8)' dCFRmoon ', dCFRmoon
       write(6,8)' CFM1sun ', CFM1   
       write(6,8)' CFM2sun ', CFM2   
       write(6,8)' dCFRsun ', dCFRsun 
       write(6,8)' CFRmoon_mag, dCFRmoon_mag  ',CFRmoon_mag,dCFRmoon_mag
       write(6,8)' CFRsun_mag, dCFRsun_mag  ',CFRsun_mag,dCFRsun_mag
       write(6,8)' PHImoon, dPHImoon  ', PHImoon/CONVD, dPHImoon/CONVD 
       write(6,8)' Lamda_moon, dLamda_moon ', Lamda_moon/CONVD, 
     *                                        dLamda_moon/CONVD
       write(6,8)' PHIsun, dPHIsun  ', PHIsun/CONVD, dPHIsun/CONVD 
       write(6,8)' Lamda_sun, dLamda_sun ', Lamda_sun/CONVD, 
     *                                      dLamda_sun/CONVD
       write(6,8)' CPhiMoon, SPhiMoon  ', CPhiMoon, dCPhiMoon, 
     *                                    SPhiMoon, dSPhiMoon 
       write(6,8)' CsqPhiMoon, SsqPhiMoon  ', CsqPhiMoon, dCsqPhiMoon, 
     *                                        SsqPhiMoon, dSsqPhiMoon 
       write(6,8)' C2PhiMoon, S2PhiMoon  ', C2PhiMoon, dC2PhiMoon, 
     *                                      S2PhiMoon, dS2PhiMoon 
       write(6,8)' CLamMoon, SLamMoon  ', CLamMoon, dCLamMoon, 
     *                                    SLamMoon, dSLamMoon 
       write(6,8)' C2LamMoon, S2LamMoon  ', C2LamMoon, dC2LamMoon, 
     *                                    S2LamMoon, dS2LamMoon 
       write(6,8)' CPhiSun, SPhiSun  ', CPhiSun, dCPhiSun,  
     *                                   SPhiSun, dSPhiSun 
       write(6,8)' CsqPhiSun, SsqPhiSun  ', CsqPhiSun, dCsqPhiSun,  
     *                                      SsqPhiSun, dSsqPhiSun
       write(6,8)' C2PhiSun, S2PhiSun  ', C2PhiSun, dC2PhiSun,  
     *                                    S2PhiSun, dS2PhiSun 
       write(6,8)' CLamSun, SLamSun  ', CLamSun, dCLamSun,  
     *                                  SLamSun, dSLamSun 
       write(6,8)' C2LamSun, S2LamSun  ', C2LamSun, dC2LamSun, 
     *                                    S2LamSun, dS2LamSun  
       write(6,8)' GMMOON  ',GMMOON
       write(6,8)' GMSUN   ',GMSUN
       write(6,8)' GMEARTH ',GMEARTH
       write(6,8)' REARTH ',REARTH
       write(6,8)'  fund_arg ',  fund_arg
       write(6,8)' dfund_arg ', dfund_arg
      Endif
C
C******************************************************************
C   Loop through twice for sites 1 and 2
C
      DO k=1,2                                  !Loop over sites
C
C First check for geocenter station. There should be no tidal effects at the 
C  geocenter.
      IF (Nzero .eq. k) THEN
c      print *,'ETDG: Zero site found '
         Do kk=1,3
          TIDEP(kk,k) = 0.D0 
          TIDEV(kk,k) = 0.D0 
         Enddo
           do kk=1,4
            do k8 = 1,6
             DETDP9(k,kk,k8) = 0.D0
            enddo
           enddo
       GO TO 100
      ENDIF
C
C   Vector from geocenter to site #k and its derivative
       do kk = 1,3
        r_vec(kk)  = USITEP(kk,k)
        dr_vec(kk) = USITEV(kk,k)
       enddo
C   Magnitude of vector from geocenter to site #k (r_vec) and its derivative
C     (Derivative should be zero)
       r_mag = vecmg(r_vec)
C         dr_mag = Dotp(r_vec,dr_vec)/r_mag
       dr_mag = 0.D0 
C   Unit vector from geocenter to site # k and its derivative
       call vunit(r_vec,r_hat)
       do kk=1,3
        dr_hat(kk) = dr_vec(kk)/r_mag - r_vec(kk)*dr_mag/r_mag**2
       enddo
C
C   Dot product of unit Moon vector with unit site vector and its derivative
       Rmoon_dot_r  = Dotp(Rmoon_hat,r_hat)
       dRmoon_dot_r = Dotp(Rmoon_hat,dr_hat) +
     *                Dotp(dRmoon_hat,r_hat)
C
C   Dot product of unit Sun vector with unit site vector and its derivative
       Rsun_dot_r = Dotp(Rsun_hat,r_hat)
       dRsun_dot_r = Dotp(Rsun_hat,dr_hat) +
     *                Dotp(dRsun_hat,r_hat)
C 
C   Geocentric latitude:
        Phi(k) = GEOLAT(k)
C
C--------------------------------------------------------------------------
C  Compute heavily used sine and cosine terms and derivatives 
C
       CLLMoon = DCOS(SITLON(k) - Lamda_moon)
       SLLMoon = DSIN(SITLON(k) - Lamda_moon)
      dCLLMoon = -SLLMoon * -dLamda_moon
      dSLLMoon =  CLLMoon * -dLamda_moon
C
       C2LLMoon = DCOS(2.D0 * (SITLON(k) - Lamda_moon))
       S2LLMoon = DSIN(2.D0 * (SITLON(k) - Lamda_moon))
      dC2LLMoon = -S2LLMoon * 2.D0 * -dLamda_moon
      dS2LLMoon =  C2LLMoon * 2.D0 * -dLamda_moon
C
       CLLSun  = DCOS(SITLON(k) - Lamda_sun )
       SLLSun  = DSIN(SITLON(k) - Lamda_sun )
      dCLLSun  = -SLLSun  * -dLamda_sun 
      dSLLSun  =  CLLSun  * -dLamda_sun 
C
       C2LLSun  = DCOS(2.D0 * (SITLON(k) - Lamda_sun ))
       S2LLSun  = DSIN(2.D0 * (SITLON(k) - Lamda_sun ))
      dC2LLSun  = -S2LLSun  * 2.D0 * -dLamda_sun 
      dS2LLSun  =  C2LLSun  * 2.D0 * -dLamda_sun 
C
C  Constant (nearly) quantities
       CPhi   = DCOS(Phi(k))
       SPhi   = DSIN(Phi(k))
       C2Phi  = DCOS(2.D0*Phi(k))
       S2Phi  = DSIN(2.D0*Phi(k))
       CsqPhi = CPhi * CPhi 
       SsqPhi = SPhi * SPhi 
C
C--------------------------------------------------------------------------
C   Elastic case with latitude dependence:
C      h_sub_2 = .6026D0 - .0006D0 * (1.5*SsqPhi - .5D0) 
C      l_sub_2 = .0831D0 + .0002D0 * (1.5*SsqPhi - .5D0) 
C   Anelastic case with latitude dependence:
       h_sub_2 = .6078D0 - .0006D0 * (1.5D0*SsqPhi - .5D0) 
       l_sub_2 = .0847D0 + .0002D0 * (1.5D0*SsqPhi - .5D0) 
C
       term1M = GMMOON / Rmoon_mag**3
       dterm1M = -3.D0 * GMMOON * dRmoon_mag / Rmoon_mag**4
       term1S = GMSUN / Rsun_mag**3
       dterm1S = -3.D0 * GMSUN * dRsun_mag / Rsun_mag**4
C
       term2  =  REARTH**4 / GMEARTH
       dterm2  =  0.D0
C
       term3M  = h_sub_2 * (1.5D0 * (Rmoon_dot_r)**2 - .5D0)
       dterm3M = h_sub_2 * 3.D0 * (Rmoon_dot_r) * dRmoon_dot_r 
       term3S  = h_sub_2 * (1.5D0 * (Rsun_dot_r)**2 - .5D0)
       dterm3S = h_sub_2 * 3.D0 * (Rsun_dot_r) * dRsun_dot_r 
C
       term4M  =  3.D0 * l_sub_2 * Rmoon_dot_r 
       dterm4M =  3.D0 * l_sub_2 * dRmoon_dot_r 
       term4S  =  3.D0 * l_sub_2 * Rsun_dot_r 
       dterm4S =  3.D0 * l_sub_2 * dRsun_dot_r 
C
       term5M  =  -3.D0 * l_sub_2 * Rmoon_dot_r**2 
       dterm5M =  -6.D0 * l_sub_2 * Rmoon_dot_r * dRmoon_dot_r 
       term5S  =  -3.D0 * l_sub_2 * Rsun_dot_r**2 
       dterm5S =  -6.D0 * l_sub_2 * Rsun_dot_r * dRsun_dot_r 
C
       term1M_2 =  term1M * term2
       dterm1M_2 = dterm1M*term2 + term1M*dterm2
C
       term1S_2 = term1S * term2
       dterm1S_2 = dterm1S*term2 + term1S*dterm2
C
C  Step 1A: Degree 2 tides, using Equation 8, p. 61, IERS Technical Note 21, 
C   IERS Conventions (1996).
       do kk = 1,3
         delta_r2(kk,k) = term1M_2 * ( term3M * r_hat(kk)
     *                               + term4M * Rmoon_hat(kk) 
     *                               + term5M * r_hat(kk) )
     *                  + term1S_2 * ( term3S * r_hat(kk)
     *                               + term4S * Rsun_hat(kk)
     *                               + term5S * r_hat(kk) )
C
        ddelta_r2(kk,k) = dterm1M_2 * ( term3M * r_hat(kk)
     *                               + term4M * Rmoon_hat(kk) 
     *                               + term5M * r_hat(kk) )
     *       + term1M_2 * ( dterm3M * r_hat(kk) + term3M * dr_hat(kk) 
     *            + dterm4M * Rmoon_hat(kk) + term4M * dRmoon_hat(kk) 
     *                    + dterm5M * r_hat(kk) + term5M * dr_hat(kk) )
     *                 + dterm1S_2 * ( term3S * r_hat(kk)
     *                               + term4S * Rsun_hat(kk)
     *                               + term5S * r_hat(kk) )
     *       + term1S_2 * ( dterm3S * r_hat(kk) + term3S * dr_hat(kk)
     *            + dterm4S * Rsun_hat(kk) + term4S * dRsun_hat(kk) 
     *                    + dterm5S * r_hat(kk) + term5S * dr_hat(kk) )
C
       enddo
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  Partials of second order tide with respect to h2 and l2:
       do kk = 1,3
         delta_r2_dh2(kk,k) = term1M_2 * term3M/h_sub_2 * r_hat(kk)
     *                      + term1S_2 * term3S/h_sub_2 * r_hat(kk)
         delta_r2_dl2(kk,k) = term1M_2 * (term4M/l_sub_2 * Rmoon_hat(kk)
     *                                 +  term5M/l_sub_2 * r_hat(kk) )
     *                      + term1S_2 * (term4S/l_sub_2 * Rsun_hat(kk)
     *                                 +  term5S/l_sub_2 * r_hat(kk) )
C
        ddelta_r2_dh2(kk,k) = dterm1M_2 * term3M/h_sub_2 * r_hat(kk)
     *       + term1M_2 * (dterm3M/h_sub_2 * r_hat(kk) + term3M/h_sub_2
     *             * dr_hat(kk))
     *       + dterm1S_2 * term3S/h_sub_2 * r_hat(kk)
     *       + term1S_2 * (dterm3S/h_sub_2 * r_hat(kk) + term3S/h_sub_2
     *             * dr_hat(kk))
C
        ddelta_r2_dl2(kk,k) = dterm1M_2 * (term4M/l_sub_2 *Rmoon_hat(kk)
     *                          + term5M/l_sub_2 * r_hat(kk) )
     *     + term1M_2 * ( dterm4M/l_sub_2 * Rmoon_hat(kk) +
     *                      term4M/l_sub_2 * dRmoon_hat(kk)
     *     + dterm5M/l_sub_2 * r_hat(kk) + term5M/l_sub_2 * dr_hat(kk))
     *
     *     + dterm1S_2 * (term4S/l_sub_2 * Rsun_hat(kk)
     *         + term5S/l_sub_2 * r_hat(kk) )
     *       + term1S_2 * (dterm4S/l_sub_2 * Rsun_hat(kk)
     *         + term4S/l_sub_2 * dRsun_hat(kk)
     *         + dterm5S/l_sub_2 * r_hat(kk) + term5S/l_sub_2
     *         * dr_hat(kk) )
       enddo
C
C  Tide difference if elastic case used instead of anelastic case
       Do kk = 1,3
        ELASP(kk,k) = -.0052* delta_r2_dh2(kk,k) +
     *                -.0016* delta_r2_dl2(kk,k) 
        ELASV(kk,k) = -.0052*ddelta_r2_dh2(kk,k) +
     *                -.0016*ddelta_r2_dl2(kk,k) 
       Enddo
C
       term01M = GMMOON / Rmoon_mag**4
       dterm01M = -4.D0 * GMMOON * dRmoon_mag / Rmoon_mag**5
C
       term02  =  REARTH**5 / GMEARTH
       dterm02  =  0.D0
C
       term03M  = h_sub_3 * (2.5D0*Rmoon_dot_r**3 - 1.5D0*Rmoon_dot_r)
       dterm03M = h_sub_3 * (7.5D0*Rmoon_dot_r**2*dRmoon_dot_r -
     *                       1.5D0*dRmoon_dot_r)
       term04M  = l_sub_3 * (7.5D0*Rmoon_dot_r**2 - 1.5D0)
       dterm04M = l_sub_3 * (15.D0*Rmoon_dot_r*dRmoon_dot_r)
C
       term05M  = -l_sub_3 * (7.5D0*Rmoon_dot_r**3 - 1.5D0*Rmoon_dot_r)
       dterm05M = -l_sub_3 * (22.5D0*Rmoon_dot_r**2*dRmoon_dot_r 
     *                     - 1.5D0*dRmoon_dot_r)
C
C  Step 1B: Degree 3 tides, using Equation 9, p. 61, IERS Technical Note 21, 
C  IERS Conventions (1996).
       Do kk = 1,3
         delta_r3(kk,k) = term01M*term02 * 
     *       ( term03M*r_hat(kk) + 
     *         term04M*Rmoon_hat(kk) +
     *         term05M*r_hat(kk) )
C
         ddelta_r3(kk,k) = dterm01M*term02 * ( term03M*r_hat(kk) + 
     *         term04M*Rmoon_hat(kk) +  term05M*r_hat(kk) )
     *    + term01M*term02 * ( dterm03M*r_hat(kk) + term03M*dr_hat(kk) 
     *    +    dterm04M*Rmoon_hat(kk) + term04M*dRmoon_hat(kk) 
     *    +    dterm05M*r_hat(kk) +  term05M*dr_hat(kk) )
       Enddo
C
C Step 1D, Contributions from latitude dependence
C    Equations 11 and 12, page 62
C
C   Legendre polynomials - Moon
       P12_moon = 3.D0 * CFRmoon(1) * CFRmoon(3) / CFRmoon_mag**2 /
     *             CLamMoon
       dP12_moon =
     *   3.D0*dCFRmoon(1)*CFRmoon(3)/CFRmoon_mag**2/CLamMoon
     * + 3.D0*CFRmoon(1)*dCFRmoon(3)/CFRmoon_mag**2/CLamMoon
     * - 6.D0*CFRmoon(1)*CFRmoon(3) * dCFRmoon_mag / CFRmoon_mag**3
     *        /CLamMoon
     * + 3.D0*CFRmoon(1)*CFRmoon(3)/CFRmoon_mag**2 * 
     *        -dCLamMoon  / CLamMoon**2
C
       P22_moon = 
     *     (3.D0 / CFRmoon_mag**2) * (CFRmoon(1)**2 - CFRmoon(2)**2) /
     *         C2LamMoon
       dP22_moon = 
     *   (3.D0 / CFRmoon_mag**2) * ( 2.D0*CFRmoon(1)*dCFRmoon(1) -
     *     2.D0*CFRmoon(2)*dCFRmoon(2)  ) / C2LamMoon              
     * - (6.D0*dCFRmoon_mag/CFRmoon_mag**3) * (CFRmoon(1)**2 - 
     *     CFRmoon(2)**2) / C2LamMoon              
     * + (3.D0 / CFRmoon_mag**2) * (CFRmoon(1)**2 - CFRmoon(2)**2) *
     *    -dC2LamMoon / C2LamMoon**2 
C
C   Legendre polynomials - Sun 
       P12_sun = 3.D0 * CFRsun(1) * CFRsun(3) / CFRsun_mag**2 /
     *            CLamSun
       dP12_sun =
     *   3.D0*dCFRsun(1)*CFRsun(3)/CFRsun_mag**2/CLamSun
     * + 3.D0*CFRsun(1)*dCFRsun(3)/CFRsun_mag**2/CLamSun
     * - 6.D0*CFRsun(1)*CFRsun(3) * dCFRsun_mag / CFRsun_mag**3
     *        /CLamSun
     * + 3.D0*CFRsun(1)*CFRsun(3)/CFRsun_mag**2 * 
     *        -dCLamSun / CLamSun**2
C
       P22_sun  = 
     *     (3.D0 / CFRsun_mag**2) * (CFRsun(1)**2 - CFRsun(2)**2) /
     *         C2LamSun              
       dP22_sun  = 
     *   (3.D0 / CFRsun_mag**2) * ( 2.D0*CFRsun(1)*dCFRsun(1) -
     *     2.D0*CFRsun(2)*dCFRsun(2) ) / C2LamSun              
     * - (6.D0*dCFRsun_mag/CFRsun_mag**3) * (CFRsun(1)**2 - 
     *     CFRsun(2)**2) / C2LamSun              
     * + (3.D0 / CFRsun_mag**2) * (CFRsun(1)**2 - CFRsun(2)**2) *
     *    -dC2LamSun / C2LamSun**2 
C
C Step 1D1: Equation 11, page 62, topocentric contribution from the diurnal 
C   band (with l_1 = 0.0012)
        l1_11 = 0.0012D0
C  Up
        topo11(1,k) = 0.D0
       dtopo11(1,k) = 0.D0
C  East
       topo11(2,k) =  l1_11 * SPhi * term1M_2 * P12_moon * C2Phi*SLLMoon 
     *              + l1_11 * SPhi * term1S_2 * P12_sun  * C2Phi *SLLSun 
      dtopo11(2,k) =  
     *        l1_11 * SPhi * dterm1M_2 * P12_moon * C2Phi * SLLMoon 
     *     +  l1_11 * SPhi * term1M_2 * dP12_moon * C2Phi * SLLMoon 
     *     +  l1_11 * SPhi * term1M_2 * P12_moon * C2Phi  * dSLLMoon 
     *     +  l1_11 * SPhi * dterm1S_2 * P12_sun * C2Phi * SLLSun 
     *     +  l1_11 * SPhi * term1S_2 * dP12_sun * C2Phi * SLLSun 
     *     +  l1_11 * SPhi * term1S_2 * P12_sun  * C2Phi * dSLLSun 
C
C  North
       topo11(3,k) = -l1_11 * SPhi**2 * term1M_2 * P12_moon * CLLMoon 
     *              - l1_11 * SPhi**2 * term1S_2 * P12_sun  * CLLSun 
      dtopo11(3,k) = -l1_11 * SPhi**2 * dterm1M_2 * P12_moon * CLLMoon 
     *         -  l1_11 * SPhi**2 * term1M_2 * dP12_moon * CLLMoon
     *         -  l1_11 * SPhi**2 * term1M_2 * P12_moon  * dCLLMoon 
     *         -  l1_11 * SPhi**2 * dterm1S_2 * P12_sun * CLLSun 
     *         -  l1_11 * SPhi**2 * term1S_2 * dP12_sun * CLLSun 
     *         -  l1_11 * SPhi**2 * term1S_2 * P12_sun * dCLLSun 
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo11(1,k), CF11(1,k) )
       CALL VECRT( R2000(1,1,1), CF11(1,k), delta11(1,k) )
C
       CALL VECRT( RTTOCF(1,1,k), dtopo11(1,k), dCF11(1,k) )
       CALL VECRT( R2000(1,1,1), dCF11(1,k), vec1 )
       CALL VECRT( R2000(1,1,2),  CF11(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta11(1,k) )
C
C Step 1D2: Equation 12, page 62, topocentric contribution from the 
C   semidiurnal band (with l_1 = 0.0024)
        l1_12 = 0.0024D0
C  Up
       topo12(1,k) = 0.D0
      dtopo12(1,k) = 0.D0
C  East 
       topo12(2,k) = -0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon
     *                      * SPhi * S2LLMoon 
     *    -           0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun 
     *                      * SPhi * S2LLSun 
      dtopo12(2,k) =
     *    -0.5D0 * l1_12 * SPhi * CPhi * dterm1M_2 * P22_moon
     *         * SPhi * S2LLMoon 
     *  -  0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * dP22_moon
     *         * SPhi * S2LLMoon 
     *  -  0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon
     *         * SPhi * dS2LLMoon 
     * 
     *  -  0.5D0 * l1_12 * SPhi * CPhi * dterm1S_2 * P22_sun 
     *         * SPhi * S2LLSun 
     *  -  0.5D0 * l1_12 * SPhi * CPhi  * term1S_2 * dP22_sun 
     *         * SPhi * S2LLSun 
     *  -  0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun 
     *         * SPhi * dS2LLSun 
C  North
       topo12(3,k) = 
     *   -0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon * C2LLMoon 
     *  - 0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun  * C2LLSun 
C
      dtopo12(3,k) = 
     *   -0.5D0 * l1_12 * SPhi * CPhi * dterm1M_2 * P22_moon * C2LLMoon 
     *  - 0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * dP22_moon * C2LLMoon 
     *  - 0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon * dC2LLMoon
     *  - 0.5D0 * l1_12 * SPhi * CPhi * dterm1S_2 * P22_sun * C2LLSun 
     *  - 0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * dP22_sun * C2LLSun 
     *  - 0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun * dC2LLSun 
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo12(1,k), CF12(1,k) )
       CALL VECRT( R2000(1,1,1), CF12(1,k), delta12(1,k) )
C
       CALL VECRT( RTTOCF(1,1,k), dtopo12(1,k), dCF12(1,k) )
       CALL VECRT( R2000(1,1,1), dCF12(1,k), vec1 )
       CALL VECRT( R2000(1,1,2),  CF12(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta12(1,k) )
C
C
C  Step 1C1. Equation 13, Out-of-Phase for degree 2 only, diurnal components 
       h_I13 = -0.0025D0
       l_I13 = -0.0007D0
C
C   Equation 13A: Up component
       topo13(1,k) = 
     *  -.75D0 * h_I13 * term1M_2 * S2PhiMoon * S2Phi * SLLMoon 
     *  -.75D0 * h_I13 * term1S_2 * S2PhiSun  * S2Phi * SLLSun 
       dtopo13(1,k) = 
     *  -.75D0 * h_I13 * dterm1M_2 * S2PhiMoon * S2Phi * SLLMoon 
     *  -.75D0 * h_I13 * term1M_2 * dS2PhiMoon * S2Phi * SLLMoon 
     *  -.75D0 * h_I13 * term1M_2 * S2PhiMoon  * S2Phi * dSLLMoon
     *  -.75D0 * h_I13 * dterm1S_2 * S2PhiSun * S2Phi * SLLSun 
     *  -.75D0 * h_I13 * term1S_2 * dS2PhiSun * S2Phi * SLLSun 
     *  -.75D0 * h_I13 * term1S_2 * S2PhiSun  * S2Phi * dSLLSun 
C
C    Equation 13B: East component
       topo13(2,k) = 
     *  -1.5D0 * l_I13 * term1M_2 * S2PhiMoon * SPhi * CLLMoon 
     *  -1.5D0 * l_I13 * term1S_2 * S2PhiSun  * SPhi * CLLSun 
       dtopo13(2,k) = 
     *  -1.5D0 * l_I13 * dterm1M_2 * S2PhiMoon  * SPhi * CLLMoon
     *  -1.5D0 * l_I13 * term1M_2  * dS2PhiMoon * SPhi * CLLMoon 
     *  -1.5D0 * l_I13 * term1M_2  * S2PhiMoon  * SPhi * dCLLMoon
     *  -1.5D0 * l_I13 * dterm1S_2 * S2PhiSun  * SPhi * CLLSun 
     *  -1.5D0 * l_I13 * term1S_2  * dS2PhiSun * SPhi * CLLSun
     *  -1.5D0 * l_I13 * term1S_2  * S2PhiSun  * SPhi * dCLLSun 
C
C    Equation 13B: North component
       topo13(3,k) = 
     *  -1.5D0 * l_I13 * term1M_2 * S2PhiMoon * C2Phi * SLLMoon 
     *  -1.5D0 * l_I13 * term1S_2 * S2PhiSun  * C2Phi * SLLSun 
       dtopo13(3,k) = 
     *  -1.5D0 * l_I13 * dterm1M_2 * S2PhiMoon * C2Phi * SLLMoon 
     *  -1.5D0 * l_I13 * term1M_2 * dS2PhiMoon * C2Phi * SLLMoon 
     *  -1.5D0 * l_I13 * term1M_2 * S2PhiMoon * C2Phi * dSLLMoon 
     * 
     *  -1.5D0 * l_I13 * dterm1S_2 * S2PhiSun * C2Phi * SLLSun 
     *  -1.5D0 * l_I13 * term1S_2 * dS2PhiSun * C2Phi * SLLSun 
     *  -1.5D0 * l_I13 * term1S_2 * S2PhiSun * C2Phi * dSLLSun 
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo13(1,k), CF13(1,k) )
       CALL VECRT( R2000(1,1,1), CF13(1,k), delta13(1,k) )
C
       CALL VECRT( RTTOCF(1,1,k), dtopo13(1,k), dCF13(1,k) )
       CALL VECRT( R2000(1,1,1), dCF13(1,k), vec1 )
       CALL VECRT( R2000(1,1,2),  CF13(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta13(1,k) )
C
C  Step 1C2. Equation 14, Out-of-Phase for degree 2 only, semidiurnal 
C   components 
       h_I14 = -0.0022D0
       l_I14 = -0.0007D0
C
C    Equation 14A: Up component
       topo14(1,k) = 
     *  -.75D0 * h_I14 * term1M_2 * CsqPhiMoon * Cphi**2 * S2LLMoon 
     *  -.75D0 * h_I14 * term1S_2 * CsqPhiSun  * Cphi**2 * S2LLSun 
       dtopo14(1,k) = 
     *  -.75D0 * h_I14 * dterm1M_2 * CsqPhiMoon * Cphi**2 * S2LLMoon 
     *  -.75D0 * h_I14 * term1M_2 * dCsqPhiMoon * Cphi**2 * S2LLMoon 
     *  -.75D0 * h_I14 * term1M_2 * CsqPhiMoon * Cphi**2 * dS2LLMoon 
     *  
     *  -.75D0 * h_I14 * dterm1S_2 * CsqPhiSun * Cphi**2 * S2LLSun 
     *  -.75D0 * h_I14 * term1S_2 * dCsqPhiSun * Cphi**2 * S2LLSun 
     *  -.75D0 * h_I14 * term1S_2 * CsqPhiSun * Cphi**2 * dS2LLSun
C
C    Equation 14B: East component
       topo14(2,k) = 
     *   .75D0 * l_I14 * term1M_2 * CsqPhiMoon * -2.D0*CPhi * C2LLMoon 
     * + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * -2.D0*CPhi * C2LLSun 
       dtopo14(2,k) = 
     *   .75D0 * l_I14 * dterm1M_2 * CsqPhiMoon * -2.D0*CPhi * C2LLMoon 
     * + .75D0 * l_I14 * term1M_2 * dCsqPhiMoon * -2.D0*CPhi * C2LLMoon 
     * + .75D0 * l_I14 * term1M_2 * CsqPhiMoon * -2.D0*CPhi * dC2LLMoon 
     * + .75D0 * l_I14 * dterm1S_2 * CsqPhiSun * -2.D0*CPhi * C2LLSun 
     * + .75D0 * l_I14 * term1S_2 * dCsqPhiSun * -2.D0*CPhi * C2LLSun 
     * + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * -2.D0*CPhi * dC2LLSun
C
C    Equation 14B: North component
       topo14(3,k) = 
     *   .75D0 * l_I14 * term1M_2 * CsqPhiMoon * S2Phi * S2LLMoon  
     * + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * S2Phi * S2LLSun 
       dtopo14(3,k) = 
     *   .75D0 * l_I14 * dterm1M_2 * CsqPhiMoon * S2Phi * S2LLMoon  
     * + .75D0 * l_I14 * term1M_2 * dCsqPhiMoon * S2Phi * S2LLMoon  
     * + .75D0 * l_I14 * term1M_2 * CsqPhiMoon * S2Phi * dS2LLMoon 
     * 
     * + .75D0 * l_I14 * dterm1S_2 * CsqPhiSun * S2Phi * S2LLSun 
     * + .75D0 * l_I14 * term1S_2 * dCsqPhiSun * S2Phi * S2LLSun 
     * + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * S2Phi * dS2LLSun  
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo14(1,k), CF14(1,k) )
       CALL VECRT( R2000(1,1,1), CF14(1,k), delta14(1,k) )
C
       CALL VECRT( RTTOCF(1,1,k), dtopo14(1,k), dCF14(1,k) )
       CALL VECRT( R2000(1,1,1), dCF14(1,k), vec1 )
       CALL VECRT( R2000(1,1,2),  CF14(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta14(1,k) )
C
C  Step 2A, Equation 15, Frequency domain corrections. In phase for degree
C    2, diurnal tides. Computed using the Delauney fundamental arguments
C
      do ll=1,5
         fund_arg(ll) = fa(ll) * CONVDS
        dfund_arg(ll) = fad(ll) * CONVDS / 3.155734D9     ! time derivative
      enddo 
         fund_arg(6) = GAST(1) + PI
        dfund_arg(6) = GAST(2)
C
      Do jf =1, 11 
          arg = 0.D0
        d_arg = 0.D0
        Do ll=1,6
           arg = arg   - Table73a(ll,jf)* fund_arg(ll)
         d_arg = d_arg - Table73a(ll,jf)*dfund_arg(ll)
        Enddo
          theta15(jf) = DMOD(arg,TWOPI)       
c        dtheta15(jf) = DMOD(d_arg,TWOPI)   
         dtheta15(jf) = d_arg   
      Enddo
C
       Do I=1,3
         topo15(I,k) = 0.0D0
        dtopo15(I,k) = 0.0D0
       Enddo
C
      Do jf=1,11
C 
C  Up component (meters)
       topo15(1,k) =  topo15(1,k) + Table73a(9,jf) * S2Phi                
     *           * DSIN(theta15(jf) + SITLON(k)) * 1.D-3
      dtopo15(1,k) = dtopo15(1,k) + Table73a(9,jf) * S2Phi                
     *           * DCOS(theta15(jf) + SITLON(k)) * dtheta15(jf) * 1.D-3
C
C  East component (meters)
        topo15(2,k) = topo15(2,k) + Table73a(10,jf) * SPhi             
     *           * DCOS(theta15(jf) + SITLON(k)) * 1.D-3
       dtopo15(2,k) = dtopo15(2,k) + Table73a(10,jf) * SPhi             
     *           * -DSIN(theta15(jf) + SITLON(k)) * dtheta15(jf)*1.D-3
C
C  North component (meters)
       topo15(3,k) = topo15(3,k) + Table73a(10,jf) * C2Phi
     *           * DSIN(theta15(jf) + SITLON(k)) * 1.D-3
       dtopo15(3,k) = dtopo15(3,k) + Table73a(10,jf) * C2Phi
     *           * DCOS(theta15(jf) + SITLON(k)) * dtheta15(jf) * 1.D-3
C
      Enddo
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo15(1,k), CF15(1,k) )
       CALL VECRT( R2000(1,1,1), CF15(1,k), delta15(1,k) )
C
       CALL VECRT( RTTOCF(1,1,k), dtopo15(1,k), dCF15(1,k) )
       CALL VECRT( R2000(1,1,1), dCF15(1,k), vec1 )
       CALL VECRT( R2000(1,1,2),  CF15(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta15(1,k) )
C
C  Step 2B, Equation 16, Frequency domain corrections, in phase and out of
C    phase for degree 2, long period tides. Computed using the Delauney 
C    fundamental arguments
C
      Do jf =1, 5
          arg = 0.D0
        d_arg = 0.D0
        Do ll=1,6
           arg = arg   - Table73b(ll,jf)* fund_arg(ll)
         d_arg = d_arg - Table73b(ll,jf)*dfund_arg(ll)
        Enddo
          theta16(jf) = DMOD(arg,TWOPI)       
c        dtheta16(jf) = DMOD(d_arg,TWOPI)   
         dtheta16(jf) = d_arg   
      Enddo
C
       Do I=1,3
         topo16(I,k) = 0.0D0
        dtopo16(I,k) = 0.0D0
       Enddo
C
      Do jf=1,5
C  Up component (meters)
        topo16(1,k) = topo16(1,k) + (1.5D0*SPhi**2 - .5D0)
     *             * ( Table73b(9,jf)*DCOS(theta16(jf)) + 
     *                 Table73b(11,jf)*DSIN(theta16(jf)) ) * 1.D-3
       dtopo16(1,k) = dtopo16(1,k) + (1.5D0*SPhi**2 -.5D0)
     *             * (-Table73b(9,jf)*DSIN(theta16(jf))*dtheta16(jf) + 
     *                 Table73b(11,jf)*DCOS(theta16(jf))*dtheta16(jf) )
     *             * 1.D-3
C
C  East component (meters) ==> ZERO
c       topo16(2,k) = 0.0D0
c      dtopo16(2,k) = 0.0D0
C
C  North component (meters)
        topo16(3,k) = topo16(3,k) 
     *    + S2Phi *  ( Table73b(10,jf)*DCOS(theta16(jf)) +
     *         Table73b(12,jf)*DSIN(theta16(jf)) ) * 1.D-3
       dtopo16(3,k) = dtopo16(3,k) 
     *    + S2Phi *  (-Table73b(10,jf)*DSIN(theta16(jf))*dtheta16(jf) +
     *         Table73b(12,jf)*DCOS(theta16(jf))*dtheta16(jf) ) * 1.D-3
C
      Enddo
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo16(1,k), CF16(1,k) )
       CALL VECRT( R2000(1,1,1), CF16(1,k), delta16(1,k) )
C
       CALL VECRT( RTTOCF(1,1,k), dtopo16(1,k), dCF16(1,k) )
       CALL VECRT( R2000(1,1,1), dCF16(1,k), vec1 )
       CALL VECRT( R2000(1,1,2),  CF16(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta16(1,k) )
C
C
C  Compute the 'Permanent Deformation' component of the solid Earth tide:
C
C  Up component
       T17A = DSQRT(5.D0/(4.D0*PI)) * -.31460D0
        topo17(1,k) = T17A * h_sub_2 * (1.5D0*SsqPhi  - .5D0)
       dtopo17(1,k) = 0.D0
C  East component
        topo17(2,k) = 0.D0
       dtopo17(2,k) = 0.D0
C  North component
       T17B = T17A * 1.5D0
        topo17(3,k) = T17B * l_sub_2 * S2Phi             
       dtopo17(3,k) = 0.D0
C
C  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
C   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo17(1,k), CF17(1,k) )
       CALL VECRT( R2000(1,1,1), CF17(1,k),  delta17(1,k) )
       CALL VECRT( R2000(1,1,2), CF17(1,k), ddelta17(1,k) )
C
C  Save permanent deformation vectors to construct a contribution later.
        do kk=1,3
          delperm(kk,k) =  delta17(kk,k)
         ddelperm(kk,k) = ddelta17(kk,k)
        enddo
C
C  Permanent tide difference if elastic case used:
C      topo17el(1,k) = -.0052D0*topo17(1,k)/h_sub_2
C      topo17el(2,k) =   0.D0
C      topo17el(3,k) = -.0016D0*topo17(3,k)/l_sub_2
C      CALL VECRT( RTTOCF(1,1,k),topo17el(1,k),CF17el(1,k) )
C      CALL VECRT( R2000(1,1,1), CF17el(1,k), del17el(1,k) )
C   Combine with previous elastic/anelastic difference
C      Do kk = 1,3
C       ELASP(kk,k) = ELASP(kk,k) - del17el(kk,k)
C      Enddo
C
C  Finished computing all the pieces, now add them up
        Do kk = 1,3
          TIDEP(kk,k) = delta_r2(kk,k) + delta_r3(kk,k)
     *          + delta11(kk,k)  + delta12(kk,k) + delta13(kk,k)
     *          + delta14(kk,k)  + delta15(kk,k) + delta16(kk,k)
C Don't remove permanent deformation; make it a contribution later 
C    *          - delta17(kk,k) 
C
          TIDEV(kk,k) = ddelta_r2(kk,k) + ddelta_r3(kk,k)
     *          + ddelta11(kk,k) + ddelta12(kk,k) + ddelta13(kk,k)
     *          + ddelta14(kk,k) + ddelta15(kk,k) + ddelta16(kk,k)
C Don't remove permanent deformation; make it a contribution later 
C    *          - ddelta17(kk,k) 
C
        Enddo
C
C  Remove permanent deformation at the correlators if real crust specified:
       If (Calc_user .eq. 'C' .and. Use_tide .eq. 'RC') Then
        Do kk = 1,3
          TIDEP(kk,k) = TIDEP(kk,k) -  delta17(kk,k) 
          TIDEV(kk,k) = TIDEV(kk,k) - ddelta17(kk,k) 
        Enddo
       Endif
C
C****************************************************************************
C   Compute partials from John Gipson's memo
C    Skip at Correlators
        IF (Calc_user .eq. 'C') GO TO 1212
C
C  h2 partials, will be in J2000 frame
      DO kk=1,3
C
C  Partial of station position w.r.t. the semi-diurnal real h2:
       h2_R_s(kk,k) = 0.75D0*r_hat(kk) * 
     *    ( (term1M_2 * CPhi**2 * CsqPhiMoon * C2LLMoon)
     *    + (term1S_2 * CPhi**2 * CsqPhiSun  * C2LLSun )  )
C   Derivative
       dh2_R_s(kk,k) = 0.75D0*dr_hat(kk) * 
     *      ( (term1M_2 * CPhi**2 * CsqPhiMoon * C2LLMoon)
     *      + (term1S_2 * CPhi**2 * CsqPhiSun  * C2LLSun )  )
     *  + 0.75D0*r_hat(kk) * 
     *      ( (dterm1M_2 * CPhi**2 * CsqPhiMoon * C2LLMoon)
     *      + (dterm1S_2 * CPhi**2 * CsqPhiSun  * C2LLSun )  )
     *  + 0.75D0*r_hat(kk) * 
     *      ( (term1M_2 * CPhi**2 * dCsqPhiMoon * C2LLMoon)
     *      + (term1S_2 * CPhi**2 * dCsqPhiSun  * C2LLSun )  )
     *  + 0.75D0*r_hat(kk) * 
     *      ( (term1M_2 * CPhi**2 * CsqPhiMoon * dC2LLMoon) 
     *      + (term1S_2 * CPhi**2 * CsqPhiSun  * dC2LLSun  ) )
C
C  Partial of station position w.r.t. the semi-diurnal imaginary h2:
       h2_I_s(kk,k) =  -0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * CPhi**2 * CsqPhiMoon * S2LLMoon)
     *  +    (term1S_2 * CPhi**2 * CsqPhiSun  * S2LLSun )  )
C  Derivative
       dh2_I_s(kk,k) =  -0.75D0*dr_hat(kk) * 
     *    (  (term1M_2 * CPhi**2 * CsqPhiMoon * S2LLMoon)
     *     + (term1S_2 * CPhi**2 * CsqPhiSun  * S2LLSun )  )
     * +  -0.75D0*r_hat(kk) * 
     *    (  (dterm1M_2 * CPhi**2 * CsqPhiMoon * S2LLMoon)
     *     + (dterm1S_2 * CPhi**2 * CsqPhiSun  * S2LLSun )  )
     * +  -0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * CPhi**2 * dCsqPhiMoon * S2LLMoon)
     *     + (term1S_2 * CPhi**2 * dCsqPhiSun  * S2LLSun )  )
     * +  -0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * CPhi**2 * CsqPhiMoon * dS2LLMoon)
     *     + (term1S_2 * CPhi**2 * CsqPhiSun  * dS2LLSun )  )
C
C  Partial of station position w.r.t. the diurnal real h2:
       h2_R_d(kk,k) = 0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * S2Phi * S2PhiMoon * CLLMoon)
     *   +   (term1S_2 * S2Phi * S2PhiSun  * CLLSun )  )
C  Derivative
       dh2_R_d(kk,k) = 0.75D0*dr_hat(kk) * 
     *    (  (term1M_2 * S2Phi * S2PhiMoon * CLLMoon)
     *     + (term1S_2 * S2Phi * S2PhiSun  * CLLSun )  )
     *  + 0.75D0*r_hat(kk) * 
     *    (  (dterm1M_2 * S2Phi * S2PhiMoon * CLLMoon)
     *     + (dterm1S_2 * S2Phi * S2PhiSun  * CLLSun )  )
     *  + 0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * S2Phi * dS2PhiMoon * CLLMoon)
     *     + (term1S_2 * S2Phi * dS2PhiSun  * CLLSun )  )
     *  + 0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * S2Phi * S2PhiMoon * dCLLMoon)
     *     + (term1S_2 * S2Phi * S2PhiSun  * dCLLSun )  )
C
C  Partial of station position w.r.t. the diurnal imaginary h2:
       h2_I_d(kk,k) = -0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * S2Phi * S2PhiMoon * SLLMoon)
     *  +    (term1S_2 * S2Phi * S2PhiSun  * SLLSun )  )
C  Derivative
       dh2_I_d(kk,k) = -0.75D0*dr_hat(kk) * 
     *    (  (term1M_2 * S2Phi * S2PhiMoon * SLLMoon)
     *     + (term1S_2 * S2Phi * S2PhiSun  * SLLSun )  )
     *  -  0.75D0*r_hat(kk) * 
     *    (  (dterm1M_2 * S2Phi * S2PhiMoon * SLLMoon)
     *     + (dterm1S_2 * S2Phi * S2PhiSun  * SLLSun )  )
     *  -  0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * S2Phi * dS2PhiMoon *  SLLMoon)
     *     + (term1S_2 * S2Phi * dS2PhiSun  *  SLLSun )  )
     *  -  0.75D0*r_hat(kk) * 
     *    (  (term1M_2 * S2Phi * S2PhiMoon * dSLLMoon)
     *     + (term1S_2 * S2Phi * S2PhiSun  * dSLLSun )  )
C
C  Partial of station position w.r.t. the long period real h2:
       h2_R_l(kk,k) = .25D0 * r_hat(kk) * (1.D0 - 3.D0*SPhi**2) * 
     *   ( ( term1M_2 * (1.D0 - 3.D0*SsqPhiMoon) )
     *  +  ( term1S_2 * (1.D0 - 3.D0*SsqPhiSun ) )  )
C
C  Derivative
       dh2_R_l(kk,k) = .25D0 * dr_hat(kk) * (1.D0 - 3.D0*SPhi**2) *
     *   ( ( term1M_2 * (1.D0 - 3.D0*SsqPhiMoon) )
     *   + ( term1S_2 * (1.D0 - 3.D0*SsqPhiSun)  ) )
     * +  .25D0 * r_hat(kk) * (1.D0 - 3.D0*SPhi**2) * 
     *   ( (dterm1M_2 * (1.D0 - 3.D0*SsqPhiMoon) )
     *   + (dterm1S_2 * (1.D0 - 3.D0*SsqPhiSun)  )  )
     * +  .25D0 * r_hat(kk) * (1.D0 - 3.D0*SPhi**2) * 
     *   ( (term1M_2 * -3.D0 * dSsqPhiMoon)
     *   + (term1S_2 * -3.D0 * dSsqPhiSun ) )
C
C  Partial of station position w.r.t. the long period imaginary h2:
       h2_I_l(kk,k) = 0.D0
C  Derivative
       dh2_I_l(kk,k) = 0.D0
C
      ENDDO
C
C   Compute East and North components of l2 partials. We will then rotate 
C    them to the J2000 frame.
C
C  Partial of station position w.r.t. the semi-diurnal real l2:
       l2_R_s(1,k) = 0.D0
       l2_R_s(2,k) =  -term1M_2 * 1.5D0 * CPhi * CsqPhiMoon * S2LLMoon 
     *               - term1S_2 * 1.5D0 * CPhi * CsqPhiSun  * S2LLSun 
       l2_R_s(3,k) = -term1M_2 * .75D0 * S2Phi * CsqPhiMoon * C2LLMoon 
     *              - term1S_2 * .75D0 * S2Phi * CsqPhiSun  * C2LLSun 
C
C   Derivative
       dl2_R_s(1,k) = 0.D0
       dl2_R_s(2,k) =
     *         -dterm1M_2 * 1.5D0 * CPhi * CsqPhiMoon * S2LLMoon 
     *        - dterm1S_2 * 1.5D0 * CPhi * CsqPhiSun  * S2LLSun  
     *       -  term1M_2 * 1.5D0 * CPhi * dCsqPhiMoon * S2LLMoon 
     *        - term1S_2 * 1.5D0 * CPhi * dCsqPhiSun  * S2LLSun 
     *       -  term1M_2 * 1.5D0 * CPhi * CsqPhiMoon * dS2LLMoon 
     *        - term1S_2 * 1.5D0 * CPhi * CsqPhiSun  * dS2LLSun
C
       dl2_R_s(3,k) = 
     *    -dterm1M_2 * .75D0 * S2Phi * CsqPhiMoon * C2LLMoon 
     *   - dterm1S_2 * .75D0 * S2Phi * CsqPhiSun  * C2LLSun 
     *   -  term1M_2 * .75D0 * S2Phi * dCsqPhiMoon * C2LLMoon 
     *   -  term1S_2 * .75D0 * S2Phi * dCsqPhiSun  * C2LLSun
     *   -  term1M_2 * .75D0 * S2Phi * CsqPhiMoon * dC2LLMoon 
     *   -  term1S_2 * .75D0 * S2Phi * CsqPhiSun  * dC2LLSun 
C
C Rotate to J2000
       CALL VECRT( RTTOCF(1,1,k), l2_R_s(1,k), CFtmp )
       CALL VECRT( R2000(1,1,1), CFtmp, l2_R_s2000(1,k) )
C    Rotate derivative to crust fixed frame
       call vecrt( RTTOCF(1,1,k),dl2_R_s(1,k),vect1)
C    Then rotate to J2000 frame:
        call vecrt(R2000(1,1,1), vect1, vect2)
        call vecrt(R2000(1,1,2), CFtmp      ,vect3)
        call vecad(vect2, vect3, dl2_R_s2000(1,k) )
C
C  Partial of station position w.r.t. the semi-diurnal imaginary l2:
       l2_I_s(1,k) = 0.D0
       l2_I_s(2,k) = -term1M_2 * 1.5D0 * CPhi * CsqPhiMoon * C2LLMoon 
     *             -  term1S_2 * 1.5D0 * CPhi * CsqPhiSun  * C2LLSun 
       l2_I_s(3,k) = term1M_2 * .75D0 * CsqPhiMoon * S2Phi * S2LLMoon
     *            +  term1S_2 * .75D0 * CsqPhiSun  * S2Phi * S2LLSun 
C
C   Derivative
       dl2_I_s(1,k) = 0.D0
       dl2_I_s(2,k) = 
     *     -dterm1M_2 * 1.5D0 * CPhi * CsqPhiMoon * C2LLMoon 
     *   -  dterm1S_2 * 1.5D0 * CPhi * CsqPhiSun  * C2LLSun 
     *   -   term1M_2 * 1.5D0 * CPhi * dCsqPhiMoon * C2LLMoon 
     *   -   term1S_2 * 1.5D0 * CPhi * dCsqPhiSun  * C2LLSun 
     *   -   term1M_2 * 1.5D0 * CPhi * CsqPhiMoon * dC2LLMoon 
     *   -   term1S_2 * 1.5D0 * CPhi * CsqPhiSun  * dC2LLSun 
       dl2_I_s(3,k) =
     *      dterm1M_2 * .75D0 *  CsqPhiMoon * S2Phi * S2LLMoon 
     *    + dterm1S_2 * .75D0 *  CsqPhiSun  * S2Phi * S2LLSun 
     *    +  term1M_2 * .75D0 * dCsqPhiMoon * S2Phi * S2LLMoon 
     *    +  term1S_2 * .75D0 * dCsqPhiSun  * S2Phi * S2LLSun 
     *    +  term1M_2 * .75D0 *  CsqPhiMoon * S2Phi * dS2LLMoon 
     *    +  term1S_2 * .75D0 *  CsqPhiSun  * S2Phi * dS2LLSun 
C
C Rotate to J2000
       CALL VECRT( RTTOCF(1,1,k), l2_I_s(1,k), CFtmp )
       CALL VECRT( R2000(1,1,1), CFtmp, l2_I_s2000(1,k) )
C    Rotate derivative to crust fixed frame
       call vecrt( RTTOCF(1,1,k),dl2_I_s(1,k),vect1)
C    Then rotate to J2000 frame:
        call vecrt(R2000(1,1,1), vect1, vect2)
        call vecrt(R2000(1,1,2), CFtmp      ,vect3)
        call vecad(vect2, vect3, dl2_I_s2000(1,k) )
C
C  Partial of station position w.r.t. the diurnal real l2:
       l2_R_d(1,k) = 0.D0
       l2_R_d(2,k) = -term1M_2 * 1.5D0 * SPhi * S2PhiMoon * SLLMoon 
     *             -  term1S_2 * 1.5D0 * SPhi * S2PhiSun  * SLLSun
       l2_R_d(3,k) = term1M_2 * 1.5D0 * C2Phi * S2PhiMoon * CLLMoon 
     *            +  term1S_2 * 1.5D0 * C2Phi * S2PhiSun  * CLLSun
C
C   Derivative
       dl2_R_d(1,k) = 0.D0
       dl2_R_d(2,k) = -dterm1M_2 * 1.5D0 * SPhi * S2PhiMoon * SLLMoon 
     *              -  dterm1S_2 * 1.5D0 * SPhi * S2PhiSun  * SLLSun
     *              -  term1M_2  * 1.5D0 * SPhi * dS2PhiMoon * SLLMoon 
     *              -  term1S_2  * 1.5D0 * SPhi * dS2PhiSun  * SLLSun
     *              -  term1M_2  * 1.5D0 * SPhi * S2PhiMoon * dSLLMoon 
     *              -  term1S_2  * 1.5D0 * SPhi * S2PhiSun  * dSLLSun
       dl2_R_d(3,k) = dterm1M_2 * 1.5D0 * C2Phi * S2PhiMoon * CLLMoon 
     *             +  dterm1S_2 * 1.5D0 * C2Phi * S2PhiSun  * CLLSun
     *             +   term1M_2 * 1.5D0 * C2Phi * dS2PhiMoon * CLLMoon 
     *             +   term1S_2 * 1.5D0 * C2Phi * dS2PhiSun  * CLLSun
     *             +   term1M_2 * 1.5D0 * C2Phi * S2PhiMoon * dCLLMoon 
     *             +   term1S_2 * 1.5D0 * C2Phi * S2PhiSun  * dCLLSun
C
C Rotate to J2000
       CALL VECRT( RTTOCF(1,1,k), l2_R_d(1,k), CFtmp )
       CALL VECRT( R2000(1,1,1), CFtmp, l2_R_d2000(1,k) )
C    Rotate derivative to crust fixed frame
       call vecrt( RTTOCF(1,1,k),dl2_R_d(1,k),vect1)
C    Then rotate to J2000 frame:
        call vecrt(R2000(1,1,1), vect1, vect2)
        call vecrt(R2000(1,1,2), CFtmp      ,vect3)
        call vecad(vect2, vect3, dl2_R_d2000(1,k) )
C
C  Partial of station position w.r.t. the diurnal imaginary l2:
       l2_I_d(1,k) = 0.D0
       l2_I_d(2,k) = -term1M_2 * 1.5D0 * SPhi * S2PhiMoon * CLLMoon  
     *              - term1S_2 * 1.5D0 * SPhi * S2PhiSun  * CLLSun
       l2_I_d(3,k) = -term1M_2 * 1.5D0 * C2Phi * S2PhiMoon * SLLMoon 
     *              - term1S_2 * 1.5D0 * C2Phi * S2PhiSun  * SLLSun
C
C   Derivative
       dl2_I_d(1,k) = 0.D0
       dl2_I_d(2,k) = -dterm1M_2 * 1.5D0 * SPhi * S2PhiMoon * CLLMoon  
     *               - dterm1S_2 * 1.5D0 * SPhi * S2PhiSun  * CLLSun
     *               -  term1M_2 * 1.5D0 * SPhi * dS2PhiMoon * CLLMoon  
     *               -  term1S_2 * 1.5D0 * SPhi * dS2PhiSun  * CLLSun
     *               -  term1M_2 * 1.5D0 * SPhi * S2PhiMoon * dCLLMoon  
     *               -  term1S_2 * 1.5D0 * SPhi * S2PhiSun  * dCLLSun
       dl2_I_d(3,k) = -dterm1M_2 * 1.5D0 * C2Phi * S2PhiMoon * SLLMoon 
     *               - dterm1S_2 * 1.5D0 * C2Phi * S2PhiSun  * SLLSun
     *               -  term1M_2 * 1.5D0 * C2Phi * dS2PhiMoon * SLLMoon 
     *               -  term1S_2 * 1.5D0 * C2Phi * dS2PhiSun  * SLLSun
     *               -  term1M_2 * 1.5D0 * C2Phi * S2PhiMoon * dSLLMoon 
     *               -  term1S_2 * 1.5D0 * C2Phi * S2PhiSun  * dSLLSun
C
C Rotate to J2000
       CALL VECRT( RTTOCF(1,1,k), l2_I_d(1,k), CFtmp )
       CALL VECRT( R2000(1,1,1), CFtmp, l2_I_d2000(1,k) )
C    Rotate derivative to crust fixed frame
       call vecrt( RTTOCF(1,1,k),dl2_I_d(1,k),vect1)
C    Then rotate to J2000 frame:
        call vecrt(R2000(1,1,1), vect1, vect2)
        call vecrt(R2000(1,1,2), CFtmp      ,vect3)
        call vecad(vect2, vect3, dl2_I_d2000(1,k) )
C
C  Partial of station position w.r.t. the long period real l2:
       l2_R_l(1,k) = 0.D0
       l2_R_l(2,k) = 0.D0
       l2_R_l(3,k) = 
     *    term1M_2 * .75D0 * S2Phi * (3.D0*SsqPhiMoon - 1.D0)
     *  + term1S_2 * .75D0 * S2Phi * (3.D0*SsqPhiSun  - 1.D0)
C
C   Derivative
       dl2_R_l(1,k) = 0.D0
       dl2_R_l(2,k) = 0.D0
       dl2_R_l(3,k) =  
     *    dterm1M_2 * .75D0 * S2Phi * (3.D0*SsqPhiMoon - 1.D0)
     *  + dterm1S_2 * .75D0 * S2Phi * (3.D0*SsqPhiSun  - 1.D0)
     *  +  term1M_2 * .75D0 * S2Phi * 3.D0 * dSsqPhiMoon 
     *  +  term1S_2 * .75D0 * S2Phi * 3.D0 * dSsqPhiSun 
C
C Rotate to J2000
       CALL VECRT( RTTOCF(1,1,k), l2_R_l(1,k), CFtmp )
       CALL VECRT( R2000(1,1,1), CFtmp, l2_R_l2000(1,k) )
C    Rotate derivative to crust fixed frame
       call vecrt( RTTOCF(1,1,k),dl2_R_l(1,k),vect1)
C    Then rotate to J2000 frame:
        call vecrt(R2000(1,1,1), vect1, vect2)
        call vecrt(R2000(1,1,2), CFtmp      ,vect3)
        call vecad(vect2, vect3, dl2_R_l2000(1,k) )
C
C  Partial of station position w.r.t. the long period imaginary l2:
       l2_I_l(1,k) = 0.D0
       l2_I_l(2,k) = 0.D0
       l2_I_l(3,k) = 0.D0
C
C   Derivative
       dl2_I_l(1,k) = 0.D0
       dl2_I_l(2,k) = 0.D0
       dl2_I_l(3,k) = 0.D0
C
C Rotate to J2000
       l2_I_l2000(1,k) = 0.D0
       l2_I_l2000(2,k) = 0.D0
       l2_I_l2000(3,k) = 0.D0
       dl2_I_l2000(1,k) = 0.D0
       dl2_I_l2000(2,k) = 0.D0
       dl2_I_l2000(3,k) = 0.D0
C
C
C  Compute delay and rate partials w.r.t. the various parameters 
C
        If (k .eq. 1) xsign =  1.D0
        If (k .eq. 2) xsign = -1.D0
C
C  h2 delay terms
       DETDP9(k,1,1) = xsign * DOTP(h2_R_s(1,k),STAR)/VLIGHT
       DETDP9(k,2,1) = xsign * DOTP(h2_I_s(1,k),STAR)/VLIGHT
       DETDP9(k,1,2) = xsign * DOTP(h2_R_d(1,k),STAR)/VLIGHT
       DETDP9(k,2,2) = xsign * DOTP(h2_I_d(1,k),STAR)/VLIGHT
       DETDP9(k,1,3) = xsign * DOTP(h2_R_l(1,k),STAR)/VLIGHT
       DETDP9(k,2,3) = 0.D0 
C
C  l2 delay terms
       DETDP9(k,3,1) = xsign * DOTP(l2_R_s2000(1,k),STAR)/VLIGHT
       DETDP9(k,4,1) = xsign * DOTP(l2_I_s2000(1,k),STAR)/VLIGHT
       DETDP9(k,3,2) = xsign * DOTP(l2_R_d2000(1,k),STAR)/VLIGHT
       DETDP9(k,4,2) = xsign * DOTP(l2_I_d2000(1,k),STAR)/VLIGHT
       DETDP9(k,3,3) = xsign * DOTP(l2_R_l2000(1,k),STAR)/VLIGHT
       DETDP9(k,4,3) = 0.D0 
C
C  h2 rate terms
       DETDP9(k,1,4) = xsign * DOTP(dh2_R_s(1,k),STAR)/VLIGHT
       DETDP9(k,2,4) = xsign * DOTP(dh2_I_s(1,k),STAR)/VLIGHT
       DETDP9(k,1,5) = xsign * DOTP(dh2_R_d(1,k),STAR)/VLIGHT
       DETDP9(k,2,5) = xsign * DOTP(dh2_I_d(1,k),STAR)/VLIGHT
       DETDP9(k,1,6) = xsign * DOTP(dh2_R_l(1,k),STAR)/VLIGHT
       DETDP9(k,2,6) = 0.D0 
C
C  l2 rate terms
       DETDP9(k,3,4) = xsign * DOTP(dl2_R_s2000(1,k),STAR)/VLIGHT
       DETDP9(k,4,4) = xsign * DOTP(dl2_I_s2000(1,k),STAR)/VLIGHT
       DETDP9(k,3,5) = xsign * DOTP(dl2_R_d2000(1,k),STAR)/VLIGHT
       DETDP9(k,4,5) = xsign * DOTP(dl2_I_d2000(1,k),STAR)/VLIGHT
       DETDP9(k,3,6) = xsign * DOTP(dl2_R_l2000(1,k),STAR)/VLIGHT
       DETDP9(k,4,6) = 0.D0 
C
 1212   CONTINUE
C
C***********************************************************************
C   Check for debug output
      If (KETDD .ne. 0) Then
       write(6,'(/," Dump for site #",i1)') K
       write(6,8)' SITLAT(k), SITLON(k) ', SITLAT(k), SITLON(k)
       write(6,8)' GEO_LAT(k)           ', GEO_LAT(k)
       write(6,8)' r_vec        ', r_vec
       write(6,8)' dr_vec       ', dr_vec
       write(6,8)' r_mag, dr_mag', r_mag, dr_mag
       write(6,8)' r_hat        ', r_hat
       write(6,8)' dr_hat       ', dr_hat
       write(6,8)' Rmoon_dot_r, dRmoon_dot_r ',Rmoon_dot_r,dRmoon_dot_r
       write(6,8)' Rsun_dot_r, dRsun_dot_r ',Rsun_dot_r,dRsun_dot_r
       write(6,8)' k, Phi(k): ', k, Phi(k)
       write(6,8)' CLLMoon, SLLMoon  ',CLLMoon,dCLLMoon,SLLMoon,dSLLMoon
       write(6,8)' C2LLMoon, S2LLMoon ', C2LLMoon, dC2LLMoon, 
     *                                    S2LLMoon, dS2LLMoon 
       write(6,8)' CLLSun, SLLSun  ', CLLSun, dCLLSun, SLLSun, dSLLSun
       write(6,8)' C2LLSun, S2LLSun  ',C2LLSun,dC2LLSun,S2LLSun,dS2LLSun 
       write(6,8)' CPhi, SPhi     ', CPhi, SPhi   
       write(6,8)' C2Phi, S2Phi   ', C2Phi, S2Phi   
       write(6,8)' CsqPhi, SsqPhi ', CsqPhi, SsqPhi   
       write(6,8)' term1M, dterm1M ', term1M, dterm1M
       write(6,8)' term2, dterm2   ', term2, dterm2
       write(6,8)' term3M, dterm3M ', term3M, dterm3M
       write(6,8)' term4M, dterm4M ', term4M, dterm4M
       write(6,8)' term5M, dterm5M  ', term5M, dterm5M
       write(6,8)' term1M_2, dterm1M_2 ', term1M_2, dterm1M_2
       write(6,8)' term1S, dterm1S ', term1S, dterm1S
       write(6,8)' term3S, dterm3S ', term3S, dterm3S
       write(6,8)' term4S, dterm4S ', term4S, dterm4S
       write(6,8)' term5S, dterm5S  ', term5S, dterm5S
       write(6,8)' term1S_2, dterm1S_2 ', term1S_2, dterm1S_2
       write(6,8)' delta_r2    ', delta_r2(1,k), delta_r2(2,k),
     *                            delta_r2(3,k)
       write(6,8)' ddelta_r2   ', ddelta_r2(1,k),ddelta_r2(2,k),
     *                            ddelta_r2(3,k)
       write(6,8)' term01M, dterm01M ', term01M, dterm01M
       write(6,8)' term02,  dterm02  ', term02, dterm02
       write(6,8)' term03M, dterm03M ', term03M, dterm03M
       write(6,8)' term04M, dterm04M ', term04M, dterm04M
       write(6,8)' term05M, dterm05M ', term05M, dterm05M
       write(6,8)' delta_r3  ', delta_r3(1,k),delta_r3(2,k),
     *                            delta_r3(3,k)
       write(6,8)' ddelta_r3 ', ddelta_r3(1,k),ddelta_r3(2,k),
     *                            ddelta_r3(3,k)
       write(6,8)' P12_moon, dP12_moon  ', P12_moon, dP12_moon
       write(6,8)' P22_moon, dP22_moon  ', P22_moon, dP22_moon
       write(6,8)' P12_sun, dP12_sun  ', P12_sun, dP12_sun 
       write(6,8)' P22_sun, dP22_sun  ', P22_sun, dP22_sun 
       write(6,8)' topo11   ', topo11(1,k),topo11(2,k),topo11(3,k) 
       write(6,8)' dtopo11  ', dtopo11(1,k),dtopo11(2,k),dtopo11(3,k)
       write(6,8)' delta11  ', delta11(1,k),delta11(2,k),delta11(3,k) 
       write(6,8)' ddelta11 ',ddelta11(1,k),ddelta11(2,k),ddelta11(3,k)
       write(6,8)' topo12   ', topo12(1,k), topo12(2,k),topo12(3,k)
       write(6,8)' dtopo12  ', dtopo12(1,k),dtopo12(2,k),dtopo12(3,k) 
       write(6,8)' delta12  ', delta12(1,k),delta12(2,k),delta12(3,k) 
       write(6,8)' ddelta12 ',ddelta12(1,k),ddelta12(2,k),ddelta12(3,k)
       write(6,8)' topo13  ',topo13(1,k),topo13(2,k),topo13(3,k)
       write(6,8)' dtopo13 ', dtopo13(1,k),dtopo13(2,k),dtopo13(3,k) 
       write(6,8)' delta13  ', delta13(1,k),delta13(2,k),delta13(3,k) 
       write(6,8)' ddelta13 ',ddelta13(1,k),ddelta13(2,k),ddelta13(3,k)
       write(6,8)' topo14  ', topo14(1,k), topo14(2,k), topo14(3,k)
       write(6,8)' dtopo14 ', dtopo14(1,k),dtopo14(2,k),dtopo14(3,k)
       write(6,8)' delta14  ', delta14(1,k),delta14(2,k),delta14(3,k) 
       write(6,8)' ddelta14 ',ddelta14(1,k),ddelta14(2,k),ddelta14(3,k)
       write(6,'(" theta15:  ",7f12.8,/,10x,4f12.8)') theta15
       write(6,'(" dtheta15: ",7f12.9,/,10x,4f12.9)') dtheta15
       write(6,8)' topo15  ', topo15(1,k),topo15(2,k),topo15(3,k)
       write(6,8)' dtopo15 ', dtopo15(1,k),dtopo15(2,k),dtopo15(3,k)
       write(6,8)' delta15  ', delta15(1,k),delta15(2,k),delta15(3,k) 
       write(6,8)' ddelta15 ',ddelta15(1,k),ddelta15(2,k),ddelta15(3,k)
       write(6,'(" theta16:  ",5f12.8)') theta16
       write(6,'(" dtheta16: ",5f12.9)') dtheta16
       write(6,8)' topo16  ', topo16(1,k),topo16(2,k),topo16(3,k)
       write(6,8)' dtopo16 ', dtopo16(1,k),dtopo16(2,k),dtopo16(3,k)
       write(6,8)' delta16  ', delta16(1,k),delta16(2,k),delta16(3,k) 
       write(6,8)' ddelta16 ',ddelta16(1,k),ddelta16(2,k),ddelta16(3,k)
       print *, 'T17A,h_sub_2,T17B,l_sub_2 ',T17A,h_sub_2,T17B,l_sub_2 
       write(6,8)' topo17  ', topo17(1,k),topo17(2,k),topo17(3,k)
       write(6,8)' delta17 ', delta17(1,k),delta17(2,k),delta17(3,k)
       write(6,8)'ddelta17 ', ddelta17(1,k),ddelta17(2,k),ddelta17(3,k)
       Write(6,8) '  h2_R_s ', ( h2_R_s(kk,k), kk=1,3)
       Write(6,8) ' dh2_R_s ', (dh2_R_s(kk,k), kk=1,3)
       Write(6,8) '  h2_I_s ', ( h2_I_s(kk,k), kk=1,3)
       Write(6,8) ' dh2_I_s ', (dh2_I_s(kk,k), kk=1,3)
       Write(6,8) '  h2_R_d ', ( h2_R_d(kk,k), kk=1,3)
       Write(6,8) ' dh2_R_d ', (dh2_R_d(kk,k), kk=1,3)
       Write(6,8) '  h2_I_d ', ( h2_I_d(kk,k), kk=1,3)
       Write(6,8) ' dh2_I_d ', (dh2_I_d(kk,k), kk=1,3)
       Write(6,8) '  h2_R_l ', ( h2_R_l(kk,k), kk=1,3)
       Write(6,8) ' dh2_R_l ', (dh2_R_l(kk,k), kk=1,3)
       Write(6,8) '  l2_R_s ', ( l2_R_s(kk,k), kk=1,3)
       Write(6,8) ' dl2_R_s ', (dl2_R_s(kk,k), kk=1,3)
       Write(6,8) '  l2_I_s ', ( l2_I_s(kk,k), kk=1,3)
       Write(6,8) ' dl2_I_s ', (dl2_I_s(kk,k), kk=1,3)
       Write(6,8) '  l2_R_d ', ( l2_R_d(kk,k), kk=1,3)
       Write(6,8) ' dl2_R_d ', (dl2_R_d(kk,k), kk=1,3)
       Write(6,8) '  l2_I_d ', ( l2_I_d(kk,k), kk=1,3)
       Write(6,8) ' dl2_I_d ', (dl2_I_d(kk,k), kk=1,3)
       Write(6,8) '  l2_R_l ', ( l2_R_l(kk,k), kk=1,3)
       Write(6,8) ' dl2_R_l ', (dl2_R_l(kk,k), kk=1,3)
       Write(6,8) ' h2_R_s, dh2_R_s ', DETDP9(k,1,1),DETDP9(k,1,4)
       Write(6,8) ' h2_I_s, dh2_I_s ', DETDP9(k,2,1),DETDP9(k,2,4)
       Write(6,8) ' h2_R_d, dh2_R_d ', DETDP9(k,1,2),DETDP9(k,1,5)
       Write(6,8) ' h2_I_d, dh2_I_d ', DETDP9(k,2,2),DETDP9(k,2,5)
       Write(6,8) ' h2_R_l, dh2_R_l ', DETDP9(k,1,3),DETDP9(k,1,6)
       Write(6,8) ' h2_I_l, dh2_I_l ', DETDP9(k,2,3),DETDP9(k,2,6)
       Write(6,8) ' l2_R_s, dl2_R_s ', DETDP9(k,3,1),DETDP9(k,3,4)
       Write(6,8) ' l2_I_s, dl2_I_s ', DETDP9(k,4,1),DETDP9(k,4,4)
       Write(6,8) ' l2_R_d, dl2_R_d ', DETDP9(k,3,2),DETDP9(k,3,5)
       Write(6,8) ' l2_I_d, dl2_I_d ', DETDP9(k,4,2),DETDP9(k,4,5)
       Write(6,8) ' l2_R_l, dl2_R_l ', DETDP9(k,3,3),DETDP9(k,3,6)
       Write(6,8) ' l2_I_l, dl2_I_l ', DETDP9(k,4,3),DETDP9(k,4,6)
C
      Endif
C
C   Close loop over sites
 100  Continue
      Enddo                                  !Loop over sites
C  
      If (KETDD .ne. 0) Then
      write (6, 9201)  TIDEP, TIDEV
 9201 FORMAT (1X, "TIDEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "TIDEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
      Endif
C
    8 FORMAT(A,4D25.16/(7X,5D25.16))
C
 1500  Continue
C
C  Compute the old Calc 8.2 Earth tide values
       IF (Calc_user .eq. 'A' .or. Use_tide .eq. 'C8') THEN
        CALL ETDC8 (R2000, SITLAT, SITLON, SUN, TCTOCF, USITEP,
     *              USITEV, XMOON, EARTH, GAST, STAR, fa, fad, cent,
     *              TD82P, TD82V)
C
         If (Calc_user .eq. 'C') Then
           Do k=1,2
            Do kk=1,3
             TIDEP(kk,k) = TD82P(kk,k)
             TIDEV(kk,k) = TD82V(kk,k)
            Enddo
           Enddo
         Endif
C
       ENDIF
C
C     Normal program conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE ETDP ( R2000, SITLAT, STAR, TCTOCF )
      IMPLICIT None
C
C     ETDP is the Earth Tide Module partial derivatives section. It calculates
C     the partial derivatives of the delay and rate with respect to site
C     dependent Earth tide parameters.
C
C     References: Markham's X-document
C
C     ETDP Program Interface:
C
C       Calling sequence -
C
C           Input variables:
C             1. R2000(3,3,3)  - The complete crust fixed to J2000 rotation
C                                matrix and its first two CT time derivatives.
C                                (unitless, 1/sec, 1/sec**2)
C             2. SITLAT(2)     - The site geodetic latitudes. (RAD)
C             3. STAR(3)       - The J2000.0 source unit vector. (unitless)
C             4. TCTOCF(3,3,2) - The rotation matrix which rotates the 
C                                topocentric reference system to the crust
C                                fixed reference system at each site.
C
C     Common blocks used:
C
      INCLUDE 'cphys.i'
C            Variables 'from':
C              1. VLIGHT  -  The vacuum velocity of light.  (M/SEC)
C
      Real*8 DETDP9(2,4,6)
      COMMON / ETDCM / DETDP9
C      1. DETDP9(2,4,6) - The partial derivatives of the delay and rate
C                         with respect to the site dependent Earth tide
C                         parameters, as defined in John Gipson's Sept. 8, 
C                         1998 memo.
C                         The first index runs over the observing sites.
C                         The second index runs over the two Love numbers AND
C                          their real and imaginary parts. 
C                           1 => h2, Real
C                           2 => h2, Imaginary
C                           3 => l2, Real
C                           4 => h2, Imaginary
C                         The third index runs over the 3 principal bands, 
C                          AND the delay and rate.
C                           1 => semi-diurnal, Delay
C                           2 => diurnal,      Delay
C                           3 => long period,  Delay
C                           4 => semi-diurnal, Rate 
C                           5 => diurnal,      Rate 
C
      INCLUDE 'ccon.i'
C         Variables 'from':
C           1. KETDC  -  The module flow control flag.
C                        = 0 => Default, IERS 1996 Earth Tide Model  
C                        = 1 => Earth tide model OFF, no corrections computed 
C           2. KETDD  -  The debug output flag.
C
      INCLUDE 'cuser.i'
C           1. Calc_user - Analysis center or Correlator user
C
C   Program specifications -
      Real*8   R2000(3,3,3), SITLAT(2), STAR(3), TCTOCF(3,3,2)
      Integer*4 I, J, K, L
C
C     Database access:
C     'PUT' Variables:
C       1. DETDP9(2,4,6) - The partial derivatives of the delay and rate
C                          with respect to the site dependent Earth tide
C                          parameters. See above description. 
C       Access codes:
C        1. 'ETJMGPAR' - The database access code for the 1996 IERS
C                        Earth tide partial derivatives array.
C
C     Subroutine Interface:
C            Caller subroutines: DRIVP
C            Called subroutines: PUT4
C
C 5.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 03/24/78
C                    BRUCE SCHUPLER 11/02/78
C                    Jim Ryan 89.06.29 Character strings and clean up.
C                    Jim Ryan 89:10:05 CPHYS common made an include file.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    Jim Ryan 90.01.20 Documentation of the partials array
C                                      fixed.
C                    David Gordon 93NOV23 Modified for IERS Earth tides.
C                    David Gordon 94.04.06 Changed to 'Implicit None'
C                    David Gordon 94.04.27 New common block 'tide_stuff'. 
C                    David Gordon 98.06.24 Obsolete code removed. New Puts
C                                 for 1996 IERS model.
C                    David Gordon 98.09.30 Increased dimensions on PUT4 of
C                                 partial derivatives (ETJMGPAR).
C                    David Gordon 98.01.06 Return immediately if Correlator.
C
C  ETDP Program Structure
C
C     Skip if correlator
        IF (Calc_user .eq. 'C') Return
C
C  Use IERS 1996 model 
      IF (KETDC.eq.0) THEN
C   Partials already computed in ETDG, PUT them into the database.
       CALL PUT4 ('ETJMGPAR      ', DETDP9, 2, 4, 6 )
      ENDIF                                 
C
C   Check KETDC to determine if the Earth tide module is to be off.
      IF (KETDC .eq. 1)  Then
       DO i = 1,2
        DO j = 1,4
         DO k = 1,6
          DETDP9(i,j,k) = 0.D0
         enddo
        enddo
       enddo
C  PUT the partials into the database.
        CALL PUT4 ('ETJMGPAR      ', DETDP9, 2, 4, 6 )
      Endif
C 
    8 FORMAT(A,4D25.16/(7X,5D25.16))
 9100  FORMAT (1X, "Debug output for subroutine ETDP " )
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE ETDC ( TIDEP, TIDEV, STAR )
      IMPLICIT None
C
C     ETDC is the Earth Tide Module contribution section. It computes the
C     contributions to the delay and rate due to Earth tide effects.
C
C     References:  Markham's X-document.
C
C     ETDC Program Interface:
C
C       Calling sequence -
C           Input variables:
C             1. TIDEP(3,2) - The corrections to the J2000 geocentric site
C                             position due to Earth tidal effects at each site.
C                             (M)
C             2. TIDEV(3,2) - The corrections to the J2000.0 geocentric site
C                             velocity vectors due to Earth tidal effects at
C                             each site. (M/SEC)
C             3. STAR(3)    - The J2000.0 source unit vector. (unitless)
C
C     Common blocks used:
C
      INCLUDE 'cphys.i'
C            Variables 'from':
C              1. VLIGHT - The vacuum velocity of light. (m/sec)
C
      INCLUDE 'ccon.i'
C         Variables 'from':
C           1. KETDC  -  The module flow control flag.
C                        = 0 => Default, IERS 1996 Earth Tide Model  
C                        = 1 => Earth tide model OFF, no corrections computed 
C           2. KETDD  -  The debug output flag.
C
      INCLUDE 'cuser.i'
C           1. Calc_user - Analysis center or Correlator user
C
      Real*8 TD82P(3,2), TD82V(3,2), ELASP(3,2), ELASV(3,2), 
     *       delperm(3,2), ddelperm(3,2)
      Common /TIDE8/ TD82P, TD82V, ELASP, ELASV, delperm, ddelperm
C
C   Program specifications:
      Real*8 DOTP, BASCOR(3,2), TIDEP(3,2), TIDEV(3,2), DETDC(2),STAR(3)
      Real*8 C8CORP(3,2), C8CORV(3,2), C8BCOR(3,2), D8ETDC(2), 
     *       ELASCOR(3,2), ELASETC(2), dperm(3,2), PRTIDE(2)
      Integer*4 K
C
C     Database access:
C           'PUT' Variables:
C             1. DETDC(2)  - The total Earth tide contribution to the delay and
C                            rate. Value depends on the value of KETDC. Default
C                            is the IERS model (second order and K1 terms). 
C             2. D8ETDC(2) - The delay and rate contributions to get the Calc
C                            8.x (IERS 1992) solid Earth tide. (Third order 
C                            term also included.) 
C             3. PRTIDE(2) - The delay and rate contributions for the 
C                            permanent tide components. Changes the effective
C                            reference frame to the old 'tide free' crust.
C             4. ELASETC(2)- The delay and rate contributions that change
C                            the Earth tide from the anelastic to the elastic
C                            case.
C           Access codes:
C             1. 'ETD CONT' - Database access code for the total Earth tide
C                             contributions.
C             2. 'C82ETCON' - The database access code for the delay and
C                             rate contribution to get the previous Calc 
C                             (versions 8.x) Earth tide model with third 
C                             order term included.
C             3. 'PERMDEF ' - The database access code for the delay and
C                             rate contribution to restore the effects of
C                             the permanent deformation.
C             4. 'ELASTCON' - The database access code for the delay and
C                             rate contribution to change from the anelastic
C                             to the elastic case.
C
C     External input/output - Possible debug output.
C
C     Subroutine Interface:
C             Caller subroutines: DRIVC
C             Called subroutines: DOTP, PUT4, VECSB
C
C     Constants used: VLIGHT
C
C     Program variables:
C         1. BASCOR(3,2) - The correction to the J2000 baseline position and
C                          velocity vectors due to Earth tidal effects.
C                          (M, M/SEC)
C
C 6.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/12/77
C                    Jim Ryan 89.06.29 Character strings and clean up.
C                    Jim Ryan 89:10:05 CPHYS common made an include file.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    David Gordon 94.01.24 Third order tide remover 
C                                 contribution added.
C                    David Gordon 94.04.06 Changed to 'Implicit None'. Vertical
C                                 Love # dependent terms added to IERS 'excess'
C                                 correction term and that Lcode renamed 
C                                 'ETD IERS'.
C                    David Gordon 94.04.27 New common block 'tide_stuff'. 
C                    David Gordon 94.04.28 Change due to Love # difference added
C                                 to 'ETD IERS'. Computations and PUT4's for 
C                                 'ETD3CONT', 'ETDKCONT', and 'ETD2CONT' added. 
C                    David Gordon 94.08.25 IERS made default. delta_IERS,
C                                 dtide_IERS, and other unused variables
C                                 removed. Documentation expanded and corrected.
C                    David Gordon 98.06.24 Obsolete code for Calc 8.x 
C                                 contributions removed.
C                    David Gordon 98.01.06 Added PUT's of corrections to use
C                                 old Calc 8.x Earth tide ('C82ETCON') and
C                                 correction to change anelastic case to 
C                                 elastic case ('ELASTCON'); skipped if
C                                 Correlator.
C                    David Gordon 99.10.15 Added computation and PUT of
C                                 permanent deformation delay and rate 
C                                 contributions, access code 'PERMDEF '.
C
C  ETDC Program Structure.
C
C Compute the contributions.
C
C  Compute the corrections to the J2000.0 baseline position and
C  velocity vectors due to Earth tidal effects.
      CALL VECSB ( TIDEP(1,1), TIDEP(1,2), BASCOR(1,1) )
      CALL VECSB ( TIDEV(1,1), TIDEV(1,2), BASCOR(1,2) )
C
C  Complete the calculation of the contributions.
      DO K = 1,2
        DETDC(K) = DOTP ( BASCOR(1,K), STAR ) / VLIGHT
      ENDDO   
C
C  PUT the Earth tide contributions.
      CALL PUT4 ('ETD CONT      ', DETDC, 2, 1, 1 )
C
C  Skip rest if Correlator
      IF (Calc_user .eq. 'C') RETURN
C
C Compute correction to get the old Calc 8.2 Earth tides:
      CALL VECSB ( TD82P(1,1), TIDEP(1,1), C8CORP(1,1) )
      CALL VECSB ( TD82P(1,2), TIDEP(1,2), C8CORP(1,2) )
      CALL VECSB ( TD82V(1,1), TIDEV(1,1), C8CORV(1,1) )
      CALL VECSB ( TD82V(1,2), TIDEV(1,2), C8CORV(1,2) )
      CALL VECSB ( C8CORP(1,1),C8CORP(1,2),C8BCOR(1,1) )
      CALL VECSB ( C8CORV(1,1),C8CORV(1,2),C8BCOR(1,2) )
      DO K = 1,2
        D8ETDC(K) = DOTP ( C8BCOR(1,K), STAR ) / VLIGHT
      ENDDO   
C
C  PUT the Calc 8.2 Earth tide difference contributions.
      CALL PUT4 ('C82ETCON      ', D8ETDC, 2, 1, 1 )
C
C Compute contribution to restore the permanent tide components:
      CALL VECSB ( delperm(1,1), delperm(1,2),dperm(1,1))
      CALL VECSB (ddelperm(1,1),ddelperm(1,2),dperm(1,2))
      DO k = 1,2
        PRTIDE(k) = -DOTP ( dperm(1,k), STAR ) / VLIGHT
      ENDDO   
C  PUT the permanent tide contribution into the data base.
      CALL PUT4 ('PERMDEF       ', PRTIDE, 2, 1, 1 )
C
C   Corrections to convert from anelastic to alastic case
      CALL VECSB ( ELASP(1,1),ELASP(1,2),ELASCOR(1,1) )
      CALL VECSB ( ELASV(1,1),ELASV(1,2),ELASCOR(1,2) )
      DO K = 1,2
        ELASETC(K) = DOTP (ELASCOR(1,K), STAR ) / VLIGHT
      ENDDO   
C
C  PUT the Elastic Earth tide difference contributions.
      CALL PUT4 ('ELASTCON      ',ELASETC, 2, 1, 1 )
C
C  Check KETDD for debug output.
      IF ( KETDD .ne. 0 ) Then        ! Debug output
       WRITE ( 6, 9100 )
 9100  FORMAT (1X, "Debug output for subroutine ETDC." )
    8  FORMAT(A,4D25.16/(7X,5D25.16))
       WRITE(6,8)' BASCOR',BASCOR
       WRITE(6,8)' DETDC ',DETDC
       WRITE(6,8)' VLIGHT',VLIGHT
       WRITE ( 6, 9200 )  TIDEP, TIDEV, STAR
 9200  FORMAT (1X, "TIDEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1             "TIDEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     2             "STAR   = ",     3 ( D30.16, 10X ) )
      ENDIF                    ! Debug output
C
C     Normal conclusion.
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE ETDC8 ( R2000, SITLAT,
     1           SITLON, SUN, TCTOCF, USITEP, USITEV, XMOON, EARTH,
     2           GAST, STAR, fa, fad, cent, TD82P, TD82V )
      IMPLICIT None 
C
C     ETDC8 calculates the Earth tide effects using the older Calc 8.2 model.
C      It includes the second order tide, the K1 tide, and the third order
C      tide components. A correction Lcode ('C82ETDCON') is written out in 
C      ETDC that, when added to the theoretical, will efectively convert it
C      to the Calc 8.x equivalent.
C
C          Reference: IERS Technical Note 13, IERS Standards (1992); D.D.
C                     McCarthy, Editor; page 57, equations 6 and 7.
C
C     ETDC8 Program Interface:
C
C       Calling sequence:
C
C           INPUT VARIABLES:
C             1. R2000(3,3,3) - The complete crust fixed to J2000.0 rotation
C                               matrix and its first two CT time derivatives.
C                               (unitless, 1/sec, 1/sec**2)
C             2. SITLAT(2)    - The geodetic latitude of each site. (RAD)
C             3. SITLON(2)    - The longitude of each site. (RAD)
C             4. SUN(3,2)     - The J2000.0 geocentric Sun position and
C                               velocity vectors. (M, M/SEC)
C             5. TCTOCF(3,3,2)- The rotation matrix which rotates the
C                               topocentric reference system to the crust
C                               fixed reference system at each site. (unitless)
C             6. USITEP(3,2)  - The J2000.0 geocentric site position vectors
C                               uncorrected for Earth tidal effects. (M)
C             7. USITEV(3,2)  - The J2000.0 geocentric site velocity vectors
C                               uncorrected for Earth tidal effects. (M/SEC)
C             8. XMOON(3,2)   - The J2000.0 geocentric Moon position and
C                               velocity vectors. (M, M/SEC)
C             9. EARTH(3,3)   - The solar system barycentric Earth position,
C                               velocity, and acceleration vectors. The first
C                               index runs over the vector components and the
C                               second runs over the time derivatives.
C                               (m, m/sec, m/sec**2)
C            10. GAST(2)      - The Greenwich Apparent Siderial time and its
C                               CT time derivative. (RAD, RAD/SEC)
C            11. STAR(3)      - The J2000.0 source unit vector.
C            12. fa(5)        - The fundamental arguments (arcsec)
C            13. fad(5)       - The CT time derivatives of the fundamental 
C                               arguments. (arcsec/century)
C            14. cent         - The number of Julian centuries elapsed since the
C                               epoch January 1.5, 2000. (centuries)
C
C           OUTPUT VARIABLES:
C             1. TD82P(3,2)   - The corrections to the J2000.0 geocentric site
C                               position vectors due to Earth tidal effects at
C                               each site. (M)
C             2. TD82V(3,2)   - The corrections to the J2000.0 geocentric site
C                               velocity vectors due to Earth tidal effects at
C                               each site. (M/SEC)
C
C     Common blocks used:
C
      INCLUDE 'cphys.i'
C            Variables 'from':
C              2. GMMOON  - The mass of the Moon multiplied by the Universal
C                           gravitational constant. (M**3/SEC**2)
C              3. GMSUN   - The mass of the Sun multiplied by the Universal
C                           Gravitational constant. (M**3/SEC**2)
C              4. GMEARTH - The mass of the Earth multiplied by the Universal
C                           Gravitational constant. (M**3/SEC**2)
C              5. REARTH  - The equatorial radius of the Earth. (meters)
C
      Real*8 delta_r2(3,2), ddelta_r2(3,2), delta_r3(3,2),
     .       ddelta_r3(3,2), XK1P(3,2), XK1V(3,2)
C      1. delta_r2(3,2) - The second order tidal displacement at each site,
C             computed using the IERS Standards (1992) formula. First index 
C             runs over X,Y,Z; second runs over sites. (meters) 
C      2. ddelta_r2(3,2) - Time derivative of above, second order tidal velocity
C             at each site. First index runs over X,Y,Z; second runs over sites.
C             (m/sec) 
C      3. delta_r3(3,2) - The third order tidal displacement at each site,
C             from the new Gipson formulation. First index runs over X,Y,Z;
C             second runs over sites. (meters)
C      4. ddelta_r3(3,2) - Time derivative of above, third order tidal velocity.
C             First index runs over X,Y,Z; second runs over sites. (m/sec)
C      5. XK1P(3,2) - The IERS vertical K1 term rotated to J2000. First index
C             runs over X,Y,Z; second runs over sites. (meters)
C      6. XK1V(3,2) - Time derivative of above. (meters/sec)
C
      Real*8 XLOVEH, XLOVEL 
C              2. XLOVEH - The global Love Number H (IERS value). (unitless)
C              3. XLOVEL - The global Love Number L (IERS value). (unitless)
C
      INCLUDE 'ccon.i'
C          VARIABLES 'FROM':
C           2. KETDD  -  The debug output flag.
C
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C          Variables "From": 
C            1. PI     - THE MATHEMATICAL CONSTANT PI (UNITLESS) 
C            2. TWOPI  - PI * 2.0D0 (UNITLESS) 
C            3. HALFPI - PI / 2.0D0 (UNITLESS) 
C            4. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
C            5. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                        (RAD/ARCSECOND) 
C            6. CONVHS - THE CONVERSION FACTOR FROM TIME SECONDS TO RADIANS
C                        (RADIANS / TIME SECOND) 
C            7. SECDAY - THE NUMBER OF TIME SECONDS PER DAY (SECONDS / DAY)
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.
C
C     Program specifications:
C
      Real*8 CFDIS(3,2), CFVEL(3,2), R2000(3,3,3), SITLAT(2), SUN(3,2), 
     6       TCTOCF(3,3,2), TD82P(3,2), TD82V(3,2),
     7       USITEP(3,2), USITEV(3,2), XMOON(3,2),
     8       SITLON(2), GAST(2), TCK1(3,2), TCK1V(3,2), STAR(3),
     9       EARTH(3,3), fa(5), fad(5), cent
      Real*8 Vecmg, Dotp,  TCK
      Real*8 r_vec(3),dr_vec(3), r_mag,dr_mag, r_hat(3),dr_hat(3),
     1       Rmoon(3),dRmoon(3), Rmoon_mag, dRmoon_mag, 
     2       Rmoon_hat(3),dRmoon_hat(3), Rmoon_dot_r,dRmoon_dot_r,
     3       Rsun(3),dRsun(3), Rsun_mag,dRsun_mag,
     4       Rsun_hat(3),dRsun_hat(3), Rsun_dot_r,dRsun_dot_r,
     5       term1M,dterm1M, term2,dterm2, term3M,dterm3M,
     6       term4M,dterm4M, term1M_2,dterm1M_2, term1S,dterm1S,
     7       term3S,dterm3S, term4S,dterm4S, term1S_2,dterm1S_2
      Real*8 CFK1P(3,2), CFK1V(3,2), XK1V1(3,2), XK1V2(3,2)
      Real*8 term5M, dterm5M, term6, dterm6, term7aM, dterm7aM,
     .       term7bM, dterm7bM, term7M, dterm7M, term8M, dterm8M,
     .       term9M, dterm9M, term789M(3), dterm789M(3) 
      Real*8 term5S, dterm5S, term7aS, dterm7aS,
     .       term7bS, dterm7bS, term7S, dterm7S, term8S, dterm8S,
     .       term9S, dterm9S, term789S(3), dterm789S(3)
      Real*8 geo_lat(2)
      Real*8 l3, h3
      Save l3, h3, XLOVEH, XLOVEL
      Data h3 /.2900D0/
      Data l3 /.0152D0/
      Data XLOVEH /.6090D0/
      Data XLOVEL /.0852D0/
C
      Integer*4  ll, kk, k, L, J, I
C
C     External input/output - Possible debug output.
C
C     Subroutine Interface:
C           Caller subroutines: DRIVG
C           Called subroutines: DCOS, DOTP, ROTATE, VECAD, VECRT, VECMG, VECSB
C
C     Constants used:  ACCGRV, GMASS(2), XLAG, XLOVEH, XLOVEL, ESQUAR, GRVLAT,
C                      GMMOON, GMSUN, GMEARTH, REARTH
C
C    Program variables:
C       1. Rmoon(3), Rsun(3), dRmoon(3), dRsun(3) - Geocentric position and
C            velocity vectors of the Moon and the Sun. (m, m, m/s, m/s)
C       2. Rmoon_mag, dRmoon_mag, Rsun_mag, dRsun_mag - Magnitude of geocentric
C             vectors and their derivatives. (m, m, m/s, m/s)
C       3. Rmoon_hat(3), Rsun_hat(3), dRmoon_hat(3), dRsun_hat(3) - Geocentric
C             unit vectors and their derivatives. (unitless)
C       4. r_vec(3), dr_vec(3) - Geocentric position and velocity of current
C             site. (m, m/s)
C       5. r_mag, dr_mag - Magnitude of geocentric site vector and its
C             derivative. (m, m/s)
C       6. r_hat(3), dr_hat(3) -  Geocentric unit vector of current site and
C             its derivative. (unitless)
C       7. Rmoon_dot_r, dRmoon_dot_r, Rsun_dot_r, dRsun_dot_r - Dot products of
C             Moon/Sun unit vectors with site unit vector and their derivatives.
C       8. term---, dterm---  - terms used for the second and third order tide
C             and partials computations, too numerous to mention individually.
C       9. delta_r2(3,2)-The second order tidal displacement at each site. First
C             index is X,Y,Z. Second index runs over sites. (m)
C      10. ddelta_r2(3,2)-The second order tidal velocity at each site. First
C             index is X,Y,Z. Second index runs over sites. (m/s)
C      11. pRm_hat(3), pRs_hat(3), dpRm_hat(3), dpRs_hat(3) - Partials of
C             Rmoon_hat and Rsun_hat with respect to lag angle and derivatives.
C      12. P_moon, P_sun, dP_moon, dP_sun - Partials of Rmoon_dot_r and
C             Rsun_dot_r with respect to lag angle, and their derivatives.
C      13. delta_r_dL(3,2), ddelta_r_dL(3,2) - Partial derivatives of tidal
C             displacement with respect to XLOVEL (l2).
C      14. delta_r_dH(3,2), ddelta_r_dH(3,2) - Partial derivatives of tidal
C             displacement with respect to XLOVEH (h2).
C      15. delta_r_dLag(3,2), ddelta_r_dLag(3,2) - Partial derivatives of tidal
C             displacement with respect to a lag angle.
C      16. delta_r(3,2)-Sum of the second and third order tidal displacements.
C             First index is X,Y,Z. Second index runs over sites.
C      17. ddelta_r(3,2)-Sum of the second and third order tidal velocities.
C             First index is X,Y,Z. Second index runs over sites.
C      25. TCK1(3,2) - The topocentric Earth crustal displacements at each site
C             due to the IERS K1 displacement term. (m)
C      26. TCK1V(3,2) - The topocentric Earth crustal velocities at each site
C             due to the IERS K1 displacement term. (m/sec)
C      29. TCK1_alt(3,2) - The total topocentric Earth crustal displacements at
C             each site due to the six frequency dependent Love number
C             correction terms of the Gipson/Mathews model. (m)
C      30. TCK1V_alt(3,2) - The total topocentric Earth crustal velocities at
C             each site due to the six frequency dependent Love number 
C             correction terms of the Gipson/Mathews model. Derivative of above.
C             (m/sec)
C      31. geo_lat(2) - The geocentric latitude of each site. (RAD)
C
C 4.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 03/24/78
C                    BRUCE SCHUPLER 11/02/78
C                    BRUCE SCHUPLER 01/14/80
C                    DAVID GORDON   07/20/84
C                    Jim Ryan 89.06.29 Character strings and clean up.
C                    Jim Ryan 89:10:05 CPHYS common made an include file.
C                    David Gordon 93Oct/Nov. Added IERS tide computation,
C                                 restructured subroutine, new KETDC options.
C                    David Gordon 94.01.24 Third order tide components added.
C                    David Gordon 94.01.27 Partials with respect to lag angle
C                                 added.
C                    David Gordon 94.04.05 Coding added to compute the six
C                                 frequency dependent Love number correction
C                                 vertical terms (K1, O1, P1, Psi, and 2 
C                                 unnamed terms). New terms will replace old
C                                 K1 term in IERS tide computation (default).
C                                 K1 left in old tide option. 
C                    David Gordon 94.04.06 Changed to 'Implicit None'
C                    David Gordon 94.04.27 New common block 'tide_stuff'. 
C                    David Gordon 94.04.28 Term for change in Love/Shida #'s
C                                 (r2_IERS) added. Separate terms kept for
C                                 all non "pure" IERS contributions.
C                    David Gordon 94.05.23 Removed second dimensioning of XK1V.
C                    David Gordon 94.07.11 Changed absolute value of geocentric
C                                 site radius to Earth's equatorial radius
C                                 (REARTH) in second and third order tide 
C                                 computation. Added change to second order
C                                 term to IERS-restorer variable. Reference
C                                 is Sonny Mathews via John Gipson. 
C                    David Gordon 94.08.09 Changed Love numbers for 
C                                 Gipson/Mathews tides. Changed amplitudes for
C                                 O1, P1, K1, Psi, ... terms. John Gipson
C                                 Aug. 9, 1994 memo (reference #7 above).
C                    David Gordon 94.08.25 Changed default back to IERS
C                                 Standards. Three correction terms kept to
C                                 convert to Gipson/Mathews Earth tide model.
C                    David Gordon 96.01.10 Changing geodetic latitude to 
C                                 geocentric latitude for computation of K1
C                                 and K1-plus tide corrections.
C                    David Gordon 96.02.07 Changing 2 arguments (signs
C                                 reversed) in Tide_arg and 3 amplitudes in
C                                 Tide_val. See 96Feb06 J. Gipson memo.   
C                    David Gordon June 1998 - Made into separate subroutine.
C                                 Removed obsolete variables, etc.
C                    David Gordon 98.01.06 Put back into Calc 9, but removed 
C                                 all but the second order, third order, and 
C                                 K1 computations.
C                    David Gordon 98.01.25 A few corrections found by John
C                                 Benson, NRAO/VLBA. 
C****************************************************************************
C     ETDC8 PROGRAM STRUCTURE
C
C     The Earth tide geometry for site #1 and site #2 are calculated
C     separately by running twice through a loop. There are two formulations
C     for computing the Earth tides. The first uses the IERS Standards (1992)
C     model. Corrections are computed to convert the IERS tide model to the
C     Gipson/Mathews model, which uses a) different second order Love numbers
C     and the Earth's equatorial radius in the second order expansion; b) has a
C     third order term (see first John Gipson memo); and c) uses 6 frequency
C     dependent vertical terms (O1, P1, K1, etc) in place of a single K1 term
C     (see second John Gipson memo). The second Earth tide formulation uses the
C     old Calc (versions 7.6 and earlier) computation (with the IERS K1 term).
C     The default is to use the new IERS formula.
C
C  **************************  STEP 1  **************************
C
C   IERS second order Earth tide computation, equation (6), page 57, IERS 
C    Technical Note 13, IERS Standards (1992).
C
C   Vectors from the geocenter to the Moon and the Sun and their derivatives
       Do kk=1,3
         Rmoon(kk)  = XMOON(kk,1)
         dRmoon(kk) = XMOON(kk,2)
         Rsun(kk)  = SUN(kk,1)
         dRsun(kk) = SUN(kk,2)
       Enddo
C
C   Magnitude of vector from geocenter to the Moon and its derivative
       Rmoon_mag = vecmg(Rmoon)
       dRmoon_mag = Dotp(Rmoon,dRmoon)/Rmoon_mag
C
C   Magnitude of vector from geocenter to the Sun and its derivative
       Rsun_mag = vecmg(Rsun)
       dRsun_mag = Dotp(Rsun,dRsun)/Rsun_mag
C
C   Unit vectors from geocenter to the Moon and Sun and their derivatives
       call vunit(Rmoon,Rmoon_hat)
       call vunit(Rsun,Rsun_hat)
       do kk=1,3
        dRmoon_hat(kk) = dRmoon(kk)/Rmoon_mag - Rmoon(kk)*dRmoon_mag /
     1                   Rmoon_mag**2
        dRsun_hat(kk) = dRsun(kk)/Rsun_mag - Rsun(kk)*dRsun_mag /
     1                   Rsun_mag**2
       enddo
C
      If(KETDD .ne. 0) Then
       write(6,'(/,10x,"Debug from Subroutine ETDG: KETDC =",I2)')KETDC
       write(6,8)' Rmoon        ', Rmoon
       write(6,8)' dRmoon       ', dRmoon
       write(6,8)' Rmoon_mag, dRmoon_mag ', Rmoon_mag, dRmoon_mag
       write(6,8)' Rmoon_hat    ', Rmoon_hat
       write(6,8)' dRmoon_hat   ', dRmoon_hat
       write(6,8)' Rsun         ', Rsun
       write(6,8)' dRsun        ', dRsun
       write(6,8)' Rsun_mag, dRsun_mag ', Rsun_mag, dRsun_mag
       write(6,8)' Rsun_hat     ', Rsun_hat
       write(6,8)' dRsun_hat    ', dRsun_hat
       write(6,'(" XLOVEH, XLOVEL ", 2f10.5)') XLOVEH, XLOVEL
       write(6,8)' GMMOON  ',GMMOON
       write(6,8)' GMSUN   ',GMSUN
       write(6,8)' GMEARTH ',GMEARTH
       write(6,8)' REARTH ',REARTH
      Endif
C
C******************************************************************
C   Loop through twice for sites 1 and 2
C
      Do k=1,2
C
C First check for geocenter station. There should be no tidal effects at the
C  geocenter.
      IF (Nzero .eq. k) THEN
c      print *,'ETDG: Zero site found '
         Do kk=1,3
          TD82P(kk,k) = 0.D0
          TD82V(kk,k) = 0.D0
         Enddo
       GO TO 100
      ENDIF
C
C   Vector from geocenter to site #k and its derivative
       do kk = 1,3
        r_vec(kk)  = USITEP(kk,k)
        dr_vec(kk) = USITEV(kk,k)
       enddo
C   Magnitude of vector from geocenter to site #k (r_vec) and its derivative
C     (Derivative should be zero)
       r_mag = vecmg(r_vec)
C      dr_mag = Dotp(r_vec,dr_vec)/r_mag
       dr_mag = 0.D0 
C   Unit vector from geocenter to site # k and its derivative
       call vunit(r_vec,r_hat)
       do kk=1,3
        dr_hat(kk) = dr_vec(kk)/r_mag - r_vec(kk)*dr_mag/r_mag**2
       enddo
C
C   Dot product of unit Moon vector with unit site vector and its derivative
       Rmoon_dot_r  = Dotp(Rmoon_hat,r_hat)
       dRmoon_dot_r = Dotp(Rmoon_hat,dr_hat) +
     1                Dotp(dRmoon_hat,r_hat)
C
C   Dot product of unit Sun vector with unit site vector and its derivative
       Rsun_dot_r = Dotp(Rsun_hat,r_hat)
       dRsun_dot_r = Dotp(Rsun_hat,dr_hat) +
     1                Dotp(dRsun_hat,r_hat)
C
C   Simplify and evaluate equation 6
C
       term1M = GMMOON / Rmoon_mag**3
       dterm1M = -3.D0 * GMMOON * dRmoon_mag / Rmoon_mag**4
C
       term2  =  r_mag**4 / GMEARTH
       dterm2  =  4.D0 * r_mag**3 * dr_mag / GMEARTH
C
       term3M =  3.0D0 * XLOVEL * Rmoon_dot_r
       dterm3M = 3.0D0 * XLOVEL * dRmoon_dot_r
C
       term4M = ( 3.D0 * (XLOVEH/2.D0 - XLOVEL)*Rmoon_dot_r**2) 
     .          - XLOVEH/2.D0
       dterm4M = ( 6.D0 * (XLOVEH/2.D0 - XLOVEL) * Rmoon_dot_r 
     .             * dRmoon_dot_r) 
C
       term1M_2 =  term1M * term2
       dterm1M_2 = dterm1M*term2 + term1M*dterm2
C
       term1S = GMSUN / Rsun_mag**3
       dterm1S = -3.D0 * GMSUN * dRsun_mag / Rsun_mag**4
C
       term3S =  3.0D0 * XLOVEL * Rsun_dot_r
       dterm3S = 3.0D0 * XLOVEL * dRsun_dot_r
C
       term4S = ( 3.D0 * (XLOVEH/2.D0 - XLOVEL)*Rsun_dot_r**2) 
     .          - XLOVEH/2.D0
       dterm4S = ( 6.D0 * (XLOVEH/2.D0 - XLOVEL) * Rsun_dot_r
     .             * dRsun_dot_r) 
C
       term1S_2 = term1S * term2
       dterm1S_2 = dterm1S*term2 + term1S*dterm2
C
C  Compute second order tidal terms
       do kk = 1,3
         delta_r2(kk,k) = term1M_2 * ( term3M * Rmoon_hat(kk)
     .                      + term4M * r_hat(kk) )
     .                      + term1S_2 * ( term3S * Rsun_hat(kk) 
     .                      + term4S * r_hat(kk) )
C
         ddelta_r2(kk,k) =  dterm1M_2 * ( term3M * Rmoon_hat(kk)
     1                      + term4M * r_hat(kk) )
     2         + term1M_2 * ( dterm3M * Rmoon_hat(kk)
     3                      +  term3M * dRmoon_hat(kk)
     4                      + dterm4M * r_hat(kk) + term4M * dr_hat(kk))
     5         + dterm1S_2 * ( term3S * Rsun_hat(kk)
     6                      +  term4S * r_hat(kk) )
     7         + term1S_2 * ( dterm3S * Rsun_hat(kk)
     8                      +  term3S * dRsun_hat(kk)
     9                      + dterm4S * r_hat(kk) + term4S * dr_hat(kk))
       enddo
C
C**************************************************
C   Compute third order tidal terms:
C
       term5M = GMMOON / Rmoon_mag**4
       dterm5M = -4.D0 * GMMOON * dRmoon_mag / Rmoon_mag**5
C
C      term6  =  r_mag**5 / GMEARTH
C      dterm6  =  5.D0 * r_mag**4 * dr_mag / GMEARTH
C   Change to equatorial radius for Gipson/Mathews model, 94JUL08 -DG-
       term6  =  REARTH**5 / GMEARTH
       dterm6  =  0.D0
C
       term7aM = 2.5D0 * Rmoon_dot_r**3
       dterm7aM = 7.5D0 * Rmoon_dot_r**2 * dRmoon_dot_r
       term7bM = -1.5D0 * Rmoon_dot_r
       dterm7bM = -1.5D0 * dRmoon_dot_r
       term7M  = term7aM + term7bM
       dterm7M  = dterm7aM + dterm7bM
C
       term8M = 1.5D0 * (5.D0 * Rmoon_dot_r**2 - 1.D0)
       dterm8M = 1.5D0 * (10.D0 * Rmoon_dot_r * dRmoon_dot_r)
C
       term9M = -term8M * Rmoon_dot_r 
       dterm9M = -dterm8M * Rmoon_dot_r - term8M * dRmoon_dot_r 
C
C      ******************************************
       term5S = GMSUN / Rsun_mag**4
       dterm5S = -4.D0 * GMSUN * dRsun_mag / Rsun_mag**5
C
       term7aS = 2.5D0 * Rsun_dot_r**3
       dterm7aS = 7.5D0 * Rsun_dot_r**2 * dRsun_dot_r
       term7bS = -1.5D0 * Rsun_dot_r
       dterm7bS = -1.5D0 * dRsun_dot_r
       term7S  = term7aS + term7bS
       dterm7S  = dterm7aS + dterm7bS
C
       term8S = 1.5D0 * (5.D0 * Rsun_dot_r**2 - 1.D0)
       dterm8S = 1.5D0 * (10.D0 * Rsun_dot_r * dRsun_dot_r)
C
       term9S = -term8S * Rsun_dot_r 
       dterm9S = -dterm8S * Rsun_dot_r - term8S * dRsun_dot_r 
C      ******************************************
C
       Do kk = 1,3
C
          term789M(kk) =  (h3*term7M + l3*term9M)* r_hat(kk)
     .         +  l3 * term8M * Rmoon_hat(kk)
          dterm789M(kk) =  (h3*dterm7M + l3*dterm9M)* r_hat(kk)
     .         +  l3 * dterm8M * Rmoon_hat(kk)
     .         +  (h3*term7M + l3*term9M)* dr_hat(kk)
     .         +  l3 * term8M * dRmoon_hat(kk)
C
          term789S(kk) =  (h3*term7S + l3*term9S)* r_hat(kk)
     .         +  l3 * term8S * Rsun_hat(kk)
          dterm789S(kk) =  (h3*dterm7S + l3*dterm9S)* r_hat(kk)
     .         +  l3 * dterm8S * Rsun_hat(kk)
     .         +  (h3*term7S + l3*term9S)* dr_hat(kk)
     .         +  l3 * term8S * dRsun_hat(kk)
C
C   Third order terms:
         delta_r3(kk,k) =
     .     term5M*term6*term789M(kk) + term5S*term6*term789S(kk) 
C
         ddelta_r3(kk,k) =
     .     dterm5M*term6*term789M(kk) + term5M*dterm6*term789M(kk) +
     .     term5M*term6*dterm789M(kk) +
     .     dterm5S*term6*term789S(kk) + term5S*dterm6*term789S(kk) +
     .     term5S*term6*dterm789S(kk) 
C
       Enddo
C
C**********************************************************************
C                              STEP 2                              
C
C   Compute the IERS K1 topocentric tide displacement and velocity
C     (IERS Standards, 1992, page 57, equation 7).
          geo_lat(k) = DASIN( (usitep(3,k)) / DSQRT(usitep(1,k)**2 +
     .                 usitep(2,k)**2 + usitep(3,k)**2) ) 
          TCK = -.0253D0 * DSIN(geo_lat(k)) * DCOS(geo_lat(k))
          TCK1(1,k)  = TCK * DSIN(GAST(1) + SITLON(k))
          TCK1(2,k) = 0.D0
          TCK1(3,k) = 0.D0
C
          TCK1V(1,k) = TCK * GAST(2) * DCOS(GAST(1) + SITLON(k))
          TCK1V(2,k) = 0.D0
          TCK1V(3,k) = 0.D0
C
C   Rotate the K1 vertical displacement and velocity to the crust fixed
C     geocentric coordinate system.
          CALL VECRT ( TCTOCF(1,1,k), TCK1(1,k), CFK1P(1,k) )
          CALL VECRT ( TCTOCF(1,1,k), TCK1V(1,k), CFK1V(1,k) )
C
C   Next rotate the crust fixed geocentric displacement to J2000.0.
          CALL VECRT ( R2000(1,1,1), CFK1P(1,k), XK1P(1,k) )
C   And rotate the crust fixed geocentric velocity to the J2000.0 system.
C     There are two contributions. The first is due to the rotation of the
C     Earth, and the second is due to the effect of the Earth tides.
          CALL VECRT ( R2000(1,1,2), CFK1P(1,k), XK1V1(1,k) )
          CALL VECRT ( R2000(1,1,1), CFK1V(1,k), XK1V2(1,k) )
C     Add the two contributions.
          CALL VECAD ( XK1V1(1,k), XK1V2(1,k), XK1V(1,k) )
C
C   Add the IERS second order term, the third order term and the IERS K1 
C     term to get the total tide
       Do kk=1,3
        TD82P(kk,k) =  delta_r2(kk,k) +  delta_r3(kk,k) + XK1P(kk,k)
        TD82V(kk,k) = ddelta_r2(kk,k) + ddelta_r3(kk,k) + XK1V(kk,k)
       Enddo
C
C*****************************
C   Check for debug output
      If (KETDD .ne. 0) Then
       write(6,'(/," Dump for site #",i1)') K
       write(6,8)' r_vec        ', r_vec
       write(6,8)' dr_vec       ', dr_vec
       write(6,8)' r_mag, dr_mag', r_mag, dr_mag
       write(6,8)' r_hat        ', r_hat
       write(6,8)' dr_hat       ', dr_hat
       write(6,8)' Rmoon_dot_r  ', Rmoon_dot_r
       write(6,8)' dRmoon_dot_r ', dRmoon_dot_r
       write(6,8)' Rsun_dot_r   ', Rsun_dot_r
       write(6,8)' dRsun_dot_r  ', dRsun_dot_r
       write(6,8)' term1M, dterm1M ', term1M, dterm1M
       write(6,8)' term2, dterm2   ', term2, dterm2
       write(6,8)' term3M, dterm3M ', term3M, dterm3M
       write(6,8)' term4M, dterm4M ', term4M, dterm4M
       write(6,8)' term1M_2, dterm1M_2 ', term1M_2, dterm1M_2
       write(6,8)' term1S, dterm1S ', term1S, dterm1S
       write(6,8)' term3S, dterm3S ', term3S, dterm3S
       write(6,8)' term4S, dterm4S ', term4S, dterm4S
       write(6,8)' term1S_2, dterm1S_2 ', term1S_2, dterm1S_2
       write(6,8)' delta_r2    ', delta_r2(1,k),delta_r2(2,k),
     .                            delta_r2(3,k)
       write(6,8)' ddelta_r2   ', ddelta_r2(1,k),ddelta_r2(2,k),
     .                        ddelta_r2(3,k)
       write(6,8)' term5M, dterm5M  ', term5M, dterm5M
       write(6,'(" term6, dterm6 ",D30.21,4x, D25.16)') term6 , dterm6
       write(6,8)' r_mag, dr_mag', r_mag, dr_mag
       write(6,8)' term7aM, dterm7aM ', term7aM, dterm7aM
       write(6,8)' term7bM, dterm7bM ', term7bM, dterm7bM
       write(6,8)' term7M , dterm7M  ', term7M, dterm7M
       write(6,8)' term8M , dterm8M  ', term8M, dterm8M
       write(6,8)' term9M , dterm9M  ', term9M, dterm9M
       write(6,8)' term789M          ', term789M
       write(6,8)' dterm789M         ', term789M
       write(6,8)' term5S , dterm5S  ', term5S, dterm5S
       write(6,8)' term7aS, dterm7aS ', term7aS, dterm7aS
       write(6,8)' term7bS, dterm7bS ', term7bS, dterm7bS
       write(6,8)' term7S , dterm7S  ', term7S, dterm7S
       write(6,8)' term8S , dterm8S  ', term8S, dterm8S
       write(6,8)' term9S , dterm9S  ', term9S, dterm9S
       write(6,8)' term789S          ', term789S
       write(6,8)' dterm789S         ', dterm789S
       write(6,8)' delta_r3  ', delta_r3(1,k),delta_r3(2,k),
     .                            delta_r3(3,k)
       write(6,8)' ddelta_r3 ', ddelta_r3(1,k),ddelta_r3(2,k),
     .                            ddelta_r3(3,k)
       write(6,8)'SITLAT(k), SITLON(k) ', SITLAT(k), SITLON(k)
       write(6,8)'geo_lat(k)           ', geo_lat(k)
       write(6,8)'TCK, TCK1, TCK1V ',TCK, TCK1(1,k),TCK1V(1,k)
       write(6,8)' XK1P     ', XK1P(1,k),XK1P(2,k),XK1P(3,k)
       write(6,8)' XK1V     ', XK1V(1,k),XK1V(2,k),XK1V(3,k)
C
      Endif
C
C   Close loop over sites
 100  CONTINUE
      Enddo
C  
    8 FORMAT(A,4D25.16/(7X,5D25.16))
C     Normal program conclusion.
      RETURN
      END
