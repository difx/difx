      SUBROUTINE ETDG ( R2K, SITLAT, SITLON, SUN, TCTOCF, RTTOCF,       &
     &         USITEP, USITEV, XMOON, EARTH, GAST2K,                    &
     &         FA2K, FAD2K, CENT, GEOLAT, TIDEP, TIDEV )
      IMPLICIT None
!
!     ETDG is the Solid Earth Tide geometry section. It calculates the
!     Earth surface crustal displacements and velocities due to Earth
!     tidal effects at each site using the IERS 1996 Earth tide model.
!
!     References:  1. IERS Technical Note 21, IERS Conventions (1996);
!                     Dennis D. McCarthy (ed.); pages 56-65.
!                  2. IERS Technical Note 32, IERS Conventions (2003),
!                     Dennis D. McCarthy (ed.), chapter 7.
!
!     ETDG Program Interface:
!
!       Calling sequence:
!           INPUT VARIABLES:
!             1. R2K(3,3,3)   - The complete crust fixed to J2000.0 rotation
!                               matrix and its first two CT time derivatives.
!                               (unitless, 1/sec, 1/sec**2)
!             2. SITLAT(2)    - The geodetic latitude of each site. (RAD)
!             3. SITLON(2)    - The longitude of each site. (RAD)
!             4. SUN(3,2)     - The J2000.0 geocentric Sun position and
!                               velocity vectors. (M, M/SEC)
!             5. TCTOCF(3,3,2)- The rotation matrix which rotates the
!                               topocentric reference system to the crust
!                               fixed reference system at each site. (unitless)
!             6. RTTOCF(3,3,2)- The rotation matrix which rotates the
!                               'radial-transverse' reference system to the
!                               crust fixed reference system at each site.
!             7. USITEP(3,2)  - The J2000.0 geocentric site position vectors
!                               uncorrected for Earth tidal effects. (M)
!             8. USITEV(3,2)  - The J2000.0 geocentric site velocity vectors
!                               uncorrected for Earth tidal effects. (M/SEC)
!             9. XMOON(3,2)   - The J2000.0 geocentric Moon position and
!                               velocity vectors. (M, M/SEC)
!            10. EARTH(3,3)   - The solar system barycentric Earth position,
!                               velocity, and acceleration vectors. The first
!                               index runs over the vector components and the
!                               second runs over the time derivatives.
!                               (m, m/sec, m/sec**2)
!            11. GAST(2)      - The Greenwich Apparent Siderial time and its
!                               CT time derivative. (RAD, RAD/SEC)
!            12. FA2K(14)     - The fundamental arguments (arcsec)
!            13. FAD2K(14)    - The CT time derivatives of the fundamental
!                               arguments. (arcsec/century)
!            14. CENT         - The number of Julian centuries elapsed since the
!                               epoch January 1.5, 2000. (centuries)
!            15. GEOLAT(2)    - The geocentric latitude at each site. (rad)
!
!           OUTPUT VARIABLES:
!             1. TIDEP(3,2)   - The corrections to the J2000.0 geocentric site
!                               position vectors due to Earth tidal effects at
!                               each site. (M)
!             2. TIDEV(3,2)   - The corrections to the J2000.0 geocentric site
!                               velocity vectors due to Earth tidal effects at
!                               each site. (M/SEC)
!
!     Common blocks used:
!
      INCLUDE 'ccon.i'
!          VARIABLES 'FROM':
!           1. KETDC  -  The module flow control flag.
!                        = 0 => Default, IERS 2003 Earth Tide Model.
!                        = 1 => Earth tide model OFF, no corrections computed.
!           2. KETDD  -  The debug output flag.
!
      INCLUDE 'cphys11.i'
!            Variables 'from':
!              2. GMMOON  - The mass of the Moon multiplied by the Universal
!                           gravitational constant. (M**3/SEC**2)
!              3. GMSUN   - The mass of the Sun multiplied by the Universal
!                           Gravitational constant. (M**3/SEC**2)
!              4. GMEARTH - The mass of the Earth multiplied by the Universal
!                           Gravitational constant. (M**3/SEC**2)
!              5. REARTH  - The equatorial radius of the Earth. (meters)
!
      INCLUDE 'cobsn11.i'
!          Variables from:
!            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
!                         otherwise equals zero. For correlator usage.
!
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!
!          Variables "From":
!            1. PI     - THE MATHEMATICAL CONSTANT PI (UNITLESS)
!            2. TWOPI  - PI * 2.0D0 (UNITLESS)
!            3. HALFPI - PI / 2.0D0 (UNITLESS)
!            4. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
!            5. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                        (RAD/ARCSECOND)
!            6. CONVHS - THE CONVERSION FACTOR FROM TIME SECONDS TO RADIANS
!                        (RADIANS / TIME SECOND)
!            7. SECDAY - THE NUMBER OF TIME SECONDS PER DAY (SECONDS / DAY)
!
!   Program specifications:
!
      Real*8 delta_r2(3,2),                                             &
     &       ddelta_r2(3,2), delta_r3(3,2), ddelta_r3(3,2),             &
     &       delta_r2_dh2(3,2), ddelta_r2_dh2(3,2),                     &
     &       delta_r2_dl2(3,2), ddelta_r2_dl2(3,2)
      Real*8 R2K  (3,3,3), SITLAT(2), SUN(3,2), TCTOCF(3,3,2),          &
     &       TIDEP(3,2), TIDEV(3,2), USITEP(3,2), USITEV(3,2),          &
     &       XMOON(3,2), SITLON(2),          EARTH(3,3), CENT,          &
     &       RTTOCF(3,3,2), GEOLAT(2), GAST2K(2), FA2K(14), FAD2K(14)
      Real*8 SunLat, SunLon, MoonLat, MoonLon, dSunLat, dSunLon,        &
     &       dMoonLat, dMoonLon
      Real*8 Vecmg, Dotp
      Real*8 r_vec(3),dr_vec(3), r_mag,dr_mag, r_hat(3),dr_hat(3),      &
     &       Rmoon(3),dRmoon(3), Rmoon_mag, dRmoon_mag,                 &
     &       Rmoon_hat(3),dRmoon_hat(3), Rmoon_dot_r,dRmoon_dot_r,      &
     &       Rsun(3),dRsun(3), Rsun_mag,dRsun_mag,                      &
     &       Rsun_hat(3),dRsun_hat(3), Rsun_dot_r,dRsun_dot_r,          &
     &       term1M,dterm1M, term2,dterm2, term3M,dterm3M,              &
     &       term4M,dterm4M, term1M_2,dterm1M_2, term1S,dterm1S,        &
     &       term3S,dterm3S, term4S,dterm4S, term1S_2,dterm1S_2
      Real*8 term5M, dterm5M, term5S, dterm5S
      Real*8 GSL2(2), Phi(2), h_sub_2, l_sub_2, h_sub_3, l_sub_3,       &
     &       term01M, dterm01M, term02, dterm02, term03M, dterm03M,     &
     &       term04M, dterm04M, term05M, dterm05M,                      &
     &       PHImoon, dPHImoon, Theta_moon, dTheta_moon,                &
     &       RA_moon, dRA_moon, Lamda_moon, dLamda_moon,                &
     &       P12_moon, P12_moon_a, P22_moon, P22_moon_a,                &
     &       dP12_moon, dP12_moon_a, dP22_moon, dP22_moon_a,            &
     &       PHIsun, dPHIsun, Theta_sun, dTheta_sun,                    &
     &       RA_sun, dRA_sun, Lamda_sun, dLamda_sun,                    &
     &       P12_sun, P12_sun_a, P22_sun, P22_sun_a,                    &
     &       dP12_sun, dP12_sun_a, dP22_sun, dP22_sun_a,                &
     &       TR2K  (3,3,2), CFRmoon(3), CFRsun(3), dCFRmoon(3),         &
     &       dCFRsun(3), CFM1(3), CFM2(3), CFRmoon_mag, dCFRmoon_mag,   &
     &       CFRsun_mag, dCFRsun_mag
      Real*8 topo11(3,2), dtopo11(3,2), CF11(3,2), dCF11(3,2),          &
     &       topo12(3,2), dtopo12(3,2), CF12(3,2), dCF12(3,2),          &
     &       topo13(3,2), dtopo13(3,2), CF13(3,2), dCF13(3,2),          &
     &       topo14(3,2), dtopo14(3,2), CF14(3,2), dCF14(3,2),          &
     &       topo15(3,2), dtopo15(3,2), CF15(3,2), dCF15(3,2),          &
     &       topo16(3,2), dtopo16(3,2), CF16(3,2), dCF16(3,2),          &
     &       vec1(3), vec2(3), T17A, T17B
      Real*8 delta11(3,2), ddelta11(3,2), delta12(3,2), ddelta12(3,2),  &
     &       delta13(3,2), ddelta13(3,2), delta14(3,2), ddelta14(3,2),  &
     &       delta15(3,2), ddelta15(3,2), delta16(3,2), ddelta16(3,2),  &
     &       l1_11, l1_12, h_I13, l_I13, h_I14, l_I14,                  &
     &       theta15(11), dtheta15(11), theta16(5), dtheta16(5)
      Real*8 CPhi, SPhi, C2Phi, S2Phi, CsqPhi, SsqPhi, CLamMoon,        &
     &       SLamMoon, CLLMoon, SLLMoon, C2LLMoon, S2LLMoon, dCLamMoon, &
     &       dSLamMoon, dCLLMoon, dSLLMoon, dC2LLMoon, dS2LLMoon,       &
     &       CPhiMoon, SPhiMoon, CsqPhiMoon, SsqPhiMoon, C2PhiMoon,     &
     &       S2PhiMoon, dCPhiMoon, dSPhiMoon, dCsqPhiMoon, dSsqPhiMoon, &
     &       dC2PhiMoon, dS2PhiMoon, CLamSun, SLamSun, CLLSun, SLLSun,  &
     &       C2LLSun, S2LLSun, dCLamSun, dSLamSun, dCLLSun, dSLLSun,    &
     &       dC2LLSun, dS2LLSun, CPhiSun, SPhiSun, CsqPhiSun,           &
     &       SsqPhiSun, C2PhiSun, S2PhiSun, dCPhiSun, dSPhiSun,         &
     &       dCsqPhiSun, dSsqPhiSun, dC2PhiSun, dS2PhiSun,              &
     &       C2LamMoon, S2LamMoon, C2LamSun, S2LamSun,                  &
     &       dC2LamMoon, dS2LamMoon, dC2LamSun, dS2LamSun
      Integer*4 C1, C2
!
      Data h_sub_3  /0.292D0/
      Data l_sub_3  /0.015D0/
!
      Real*4 Table75a(10,11), Table75b(10,5)
      Integer*4 itide, ll, kk, k, k8, L, J, I, jf
      Real*8 fund_arg(6), arg, dfund_arg(6), d_arg
      Real*8 geo_lat(2)
!
!  Table 7.5a, IERS Conventions (2003)
      Data Table75a / &
     &  1.,  0.,  2.,  0.,  2., -1., -0.08,  0.00, -0.01,  0.01,        &
     &  0.,  0.,  2.,  0.,  1., -1., -0.10,  0.00,  0.00,  0.00,        &
     &  0.,  0.,  2.,  0.,  2., -1., -0.51,  0.00, -0.02,  0.03,        &
     &  1.,  0.,  0.,  0.,  0., -1.,  0.06,  0.00,  0.00,  0.00,        &
     &  0.,  1.,  2., -2.,  2., -1., -0.06,  0.00,  0.00,  0.00,        &
     &  0.,  0.,  2., -2.,  2., -1., -1.23, -0.07,  0.06,  0.01,        &
     &  0.,  0.,  0.,  0., -1., -1., -0.22,  0.01,  0.01,  0.00,        &
     &  0.,  0.,  0.,  0.,  0., -1., 12.00, -0.78, -0.67, -0.03,        &
     &  0.,  0.,  0.,  0.,  1., -1.,  1.73, -0.12, -0.10,  0.00,        &
     &  0., -1.,  0.,  0.,  0., -1., -0.50, -0.01,  0.03,  0.00,        &
     &  0.,  0., -2.,  2., -2., -1., -0.11,  0.01,  0.01,  0.00 /
!
!  Table 7.5b, IERS Conventions (2003)
      Data Table75b / &
     &  0.,  0.,  0.,  0.,  1.,  0.,  0.47,  0.16,  0.23,  0.07,        &
     &  0.,  0., -2.,  2., -2.,  0., -0.20, -0.11, -0.12, -0.05,        &
     & -1.,  0.,  0.,  0.,  0.,  0., -0.11, -0.09, -0.08, -0.04,        &
     &  0.,  0., -2.,  0., -2.,  0., -0.13, -0.15, -0.11, -0.07,        &
     &  0.,  0., -2.,  0., -1.,  0., -0.05, -0.06, -0.05, -0.03 /
!
!     External input/output - Possible debug output.
!
!     Subroutine Interface:
!           Caller subroutines: DRIVG
!           Called subroutines: DCOS, DOTP, ROTATE, VECAD, VECRT, VECMG, VECSB
!
!     Constants used:  GMMOON, GMSUN, GMEARTH, REARTH
!
!    Program variables:
!
!       1. Rmoon(3), Rsun(3), dRmoon(3), dRsun(3) - Geocentric position and
!            velocity vectors of the Moon and the Sun. (m, m, m/s, m/s)
!       2. Rmoon_mag, dRmoon_mag, Rsun_mag, dRsun_mag - Magnitude of geocentric
!             vectors and their derivatives. (m, m, m/s, m/s)
!       3. Rmoon_hat(3), Rsun_hat(3), dRmoon_hat(3), dRsun_hat(3) - Geocentric
!             unit vectors and their derivatives. (unitless)
!       4. r_vec(3), dr_vec(3) - Geocentric position and velocity of current
!             site. (m, m/s)
!       5. r_mag, dr_mag - Magnitude of geocentric site vector and its
!             derivative. (m, m/s)
!       6. r_hat(3), dr_hat(3) -  Geocentric unit vector of current site and
!             its derivative. (unitless)
!       7. Rmoon_dot_r, dRmoon_dot_r, Rsun_dot_r, dRsun_dot_r - Dot products of
!             Moon/Sun unit vectors with site unit vector and their derivatives.
!       8. term---, dterm---  - terms used for the second and third order tide
!             and partials computations, too numerous to mention individually.
!       9. delta_r2(3,2)-The second order tidal displacement at each site. First
!             index is X,Y,Z. Second index runs over sites. (m)
!      10. ddelta_r2(3,2)-The second order tidal velocity at each site. First
!             index is X,Y,Z. Second index runs over sites. (m/s)
!      11. fund_arg(6) - The five fundamental arguments (fa(5))
!             converted to radians and the Greenwich apparent siderial time
!             plus PI. (radians)
!      12. dfund_arg(6) - Time derivatives of above. (radians/sec)
!      13. geo_lat(2) - The geocentric latitude of each site. (RAD)
!      14. SunLat  - The J2000 geocentric latitude (declination) of the Sun.
!      15. MoonLat - The J2000 geocentric latitude (declination) of the Moon.
!      16. SunLon  - The J2000 geocentric longitude (RA) of the Sun.
!      17. MoonLon - The J2000 geocentric longitude (RA) of the Moon.
!      18. delta_r3(3,2) - The third order tidal displacement at each site,
!             from the IERS 1996 model. First index runs over X,Y,Z;
!             second runs over sites. (meters)
!      19. ddelta_r3(3,2) - Time derivative of above, third order tidal
!             velocity. First index runs over X,Y,Z; second runs over sites.
!             (m/sec)
!
! 4.2.9 PROGRAMMER -
!           DALE MARKHAM  01/13/77
!           PETER DENATALE 07/12/77
!           BRUCE SCHUPLER 03/24/78
!           BRUCE SCHUPLER 11/02/78
!           BRUCE SCHUPLER 01/14/80
!           DAVID GORDON   07/20/84
!           Jim Ryan 89.06.29 Character strings and clean up.
!           Jim Ryan 89:10:05 CPHYS common made an include file.
!           David Gordon 93Oct/Nov. Added IERS tide computation,
!                        restructured subroutine, new KETDC options.
!           David Gordon 94.01.24 Third order tide components added.
!           David Gordon 94.01.27 Partials with respect to lag angle
!                        added.
!           David Gordon 94.04.05 Coding added to compute the six
!                        frequency dependent Love number correction
!                        vertical terms (K1, O1, P1, Psi, and 2
!                        unnamed terms). New terms will replace old
!                        K1 term in IERS tide computation (default).
!                        K1 left in old tide option.
!           David Gordon 94.04.06 Changed to 'Implicit None'
!           David Gordon 94.04.27 New common block 'tide_stuff'.
!           David Gordon 94.04.28 Term for change in Love/Shida #'s
!                        (r2_IERS) added. Separate terms kept for
!                        all non "pure" IERS contributions.
!           David Gordon 94.05.23 Removed second dimensioning of XK1V.
!           David Gordon 94.07.11 Changed absolute value of geocentric
!                        site radius to Earth's equatorial radius
!                        (REARTH) in second and third order tide
!                        computation. Added change to second order
!                        term to IERS-restorer variable. Reference
!                        is Sonny Mathews via John Gipson.
!           David Gordon 94.08.09 Changed Love numbers for
!                        Gipson/Mathews tides. Changed amplitudes for
!                        O1, P1, K1, Psi, ... terms. John Gipson
!                        Aug. 9, 1994 memo (reference #7 above).
!           David Gordon 94.08.25 Changed default back to IERS
!                        Standards. Three correction terms kept to
!                        convert to Gipson/Mathews Earth tide model.
!           David Gordon 96.01.10 Changing geodetic latitude to
!                        geocentric latitude for computation of K1
!                        and K1-plus tide corrections.
!           David Gordon 96.02.07 Changing 2 arguments (signs
!                        reversed) in Tide_arg and 3 amplitudes in
!                        Tide_val. See 96Feb06 J. Gipson memo.
!           David Gordon 98.05.06 Old (Calc 5/6/7) computations removed.
!                        Calc 8 computations moved into subroutine ETDC8.
!                        Calc 9 (1996 IERS Conventions) computations coded
!                        and put into subroutine ETDC9.
!           David Gordon 98.06.24 Subroutine ETDC8 deleted. Subroutine ETDG
!                        reduced to a driver for ETDC9.
!           David Gordon 98.07.29 Added 'Include cobsn.i' with
!                        variable Nzero, and code to skip topocentric
!                        computations for a station at the geocenter.
!           David Gordon 98.07.31 Subroutine ETDC9 folded back in to ETDG.
!                        Mods to set tides to zero for a geocenter station.
!           David Gordon 98.09.30 Finished coding Earth tide partials based
!                        on John Gipson's memo of 8 Sept. 1998. Restructured
!                        and simplifed earlier tide components and time
!                        derivatives. Removed all but DETDP9 from common
!                        block ETDCM. Removed the block data subprogram.
!           David Gordon 98.12.29 Corrected errors in Table73b, found by
!                        John Gipson.
!           David Gordon 99.01.06 Added call to subroutine ETDC8 to compute
!                        earlier Calc 8 Earth tide. Skipping of partials
!                        for correlator usage.
!           David Gordon 99.10.01 Adding 'Use_Tide' parameter to allow
!                        using the Calc 8 tide model at the correlators.
!           David Gordon 99.10.13 Added computation of permanent tide vectors.
!           David Gordon 99.10.18 Redefined 'Use_Tide' parameter to allow
!                        using the Calc 8 tide model, the Calc 9.0 model,
!                        or the real crust model at the correlators.
!           Jim Ryan     2002.09 Integer*4 conversion
!           David Gordon 2004 Updated Step 2 for IERS Conventions (2003).
!                        Removed Calc 8 tide call and Calc 9 partials.
!           David Gordon 2007.01.10. Corrected geocenter bug.
!
!****************************************************************************
!     ETDG PROGRAM STRUCTURE
!****************************************************************************
!
!  Check KETDC to determine if the Earth tide module is to be turned OFF.
      IF (KETDC .eq. 1)  Then
        DO 220  L = 1,2
          DO 210  I = 1,3
            TIDEP(I,L) = 0.D0
            TIDEV(I,L) = 0.D0
  210     CONTINUE
  220   CONTINUE
       RETURN
      ENDIF
!
!****************************************************************************
!     The Earth tide geometry for site #1 and site #2 are calculated
!     separately by running twice through a loop.
!     The program uses the IERS Conventions (2003) model.
!
!
! Preliminary Sun and Moon (station independent) quantities:
!
!   Vectors from the geocenter to the Moon and the Sun and their derivatives
       Do kk=1,3
         Rmoon(kk)  = XMOON(kk,1)
         dRmoon(kk) = XMOON(kk,2)
         Rsun(kk)  = SUN(kk,1)
         dRsun(kk) = SUN(kk,2)
       Enddo
!
!   Magnitude of vector from geocenter to the Moon and its derivative
       Rmoon_mag = vecmg(Rmoon)
       dRmoon_mag = Dotp(Rmoon,dRmoon)/Rmoon_mag
!
!   Magnitude of vector from geocenter to the Sun and its derivative
       Rsun_mag = vecmg(Rsun)
       dRsun_mag = Dotp(Rsun,dRsun)/Rsun_mag
!
!   Unit vectors from geocenter to the Moon and Sun and their derivatives
       call vunit(Rmoon,Rmoon_hat)
       call vunit(Rsun,Rsun_hat)
       do kk=1,3
        dRmoon_hat(kk) = dRmoon(kk)/Rmoon_mag - Rmoon(kk)*dRmoon_mag /  &
     &                   Rmoon_mag**2
        dRsun_hat(kk) = dRsun(kk)/Rsun_mag - Rsun(kk)*dRsun_mag /       &
     &                   Rsun_mag**2
       enddo
!
!  Get Crust-fixed latitude and longitude of the Sun and the Moon.
!  Rotate Moon and Sun vectors from the J2000 coordinate system to the
!  geocentric crust-fixed system.
!    First compute the rotation matrix which rotates from the J2000.0
!    reference system to the geocentric crust fixed system, and its
!    time derivative.
           CALL MTRAN ( R2K  (1,1,1), TR2K  (1,1,1) )
           CALL MTRAN ( R2K  (1,1,2), TR2K  (1,1,2) )
!    Then rotate the Moon and Sun unit vectors to the crust fixed system.
           CALL VECRT ( TR2K  (1,1,1),Rmoon(1),CFRmoon(1))
           CALL VECRT ( TR2K  (1,1,1),Rsun(1),CFRsun(1))
!    And find their derivatives in the crust fixed system
           CALL VECRT ( TR2K  (1,1,1),dRmoon,CFM1)
           CALL VECRT ( TR2K  (1,1,2), Rmoon,CFM2)
           CALL VECAD ( CFM1, CFM2, dCFRmoon )
           CALL VECRT ( TR2K  (1,1,1),dRsun,CFM1)
           CALL VECRT ( TR2K  (1,1,2), Rsun,CFM2)
           CALL VECAD ( CFM1, CFM2, dCFRsun )
!
      CFRmoon_mag = VECMG( CFRmoon )
      dCFRmoon_mag = DOTP( CFRmoon, dCFRmoon ) / CFRmoon_mag
!
      CFRsun_mag = VECMG( CFRsun )
      dCFRsun_mag = DOTP( CFRsun, dCFRsun ) / CFRsun_mag
!
!  Find body-fixed geocentric latitude of Sun/moon
      PHImoon  = DASIN( CFRmoon(3)/CFRmoon_mag )
      dPHImoon = ( dCFRmoon(3)/CFRmoon_mag - &
     &             CFRmoon(3)*dCFRmoon_mag/CFRmoon_mag**2 ) /           &
     &             DSQRT( 1.D0 - (CFRmoon(3)/CFRmoon_mag)**2 )
      PHIsun   = DASIN( CFRsun(3)/CFRsun_mag )
      dPHIsun  = ( dCFRsun(3)/CFRsun_mag -                              &
     &             CFRsun(3)*dCFRsun_mag/CFRsun_mag**2 ) /              &
     &             DSQRT( 1.D0 - (CFRsun(3)/CFRsun_mag)**2 )
!
!  Find body-fixed East longitude (from Greenwich) of moon
      Lamda_moon = DATAN2( CFRmoon(2),CFRmoon(1) )
      dLamda_moon =  1.D0 / ( 1.D0 + (CFRmoon(2)/CFRmoon(1))**2 )       &
     &      *  ( dCFRmoon(2)/CFRmoon(1) -                               &
     &        CFRmoon(2)*dCFRmoon(1)/CFRmoon(1)**2 )
!
!  Find body-fixed East longitude (from Greenwich) of Sun
      Lamda_sun = DATAN2( CFRsun(2),CFRsun(1) )
      dLamda_sun =  1.D0 / ( 1.D0 + (CFRsun(2)/CFRsun(1))**2 )          &
     &      * ( dCFRsun(2)/CFRsun(1) -                                  &
     &        CFRsun(2)*dCFRsun(1)/CFRsun(1)**2 )
!
!  Compute heavily used sine and cosine terms and derivatives of Sun and
!       Moon crust-fixed latitude and longitude
!
       CPhiMoon = DCOS(PhiMoon)
       SPhiMoon = DSIN(PhiMoon)
      dCPhiMoon = -SPhiMoon * dPhiMoon
      dSPhiMoon =  CPhiMoon * dPhiMoon
!
       CsqPhiMoon = CPhiMoon * CPhiMoon
       SsqPhiMoon = SPhiMoon * SPhiMoon
      dCsqPhiMoon = 2.D0 * CPhiMoon * dCPhiMoon
      dSsqPhiMoon = 2.D0 * SPhiMoon * dSPhiMoon
!
       C2PhiMoon = DCOS(2.D0*PhiMoon)
       S2PhiMoon = DSIN(2.D0*PhiMoon)
      dC2PhiMoon = -2.D0 * S2PhiMoon * dPhiMoon
      dS2PhiMoon =  2.D0 * C2PhiMoon * dPhiMoon
!
       CLamMoon = DCOS(Lamda_Moon)
       SLamMoon = DSIN(Lamda_Moon)
      dCLamMoon = -SLamMoon * dLamda_Moon
      dSLamMoon =  CLamMoon * dLamda_Moon
!
       C2LamMoon = DCOS(2.D0 * Lamda_Moon)
       S2LamMoon = DSIN(2.D0 * Lamda_Moon)
      dC2LamMoon = -S2LamMoon * 2.D0 * dLamda_Moon
      dS2LamMoon =  C2LamMoon * 2.D0 * dLamda_Moon
!
       CPhiSun  = DCOS(PhiSun )
       SPhiSun  = DSIN(PhiSun )
      dCPhiSun  = -SPhiSun  * dPhiSun
      dSPhiSun  =  CPhiSun  * dPhiSun
!
       CsqPhiSun  = CPhiSun  * CPhiSun
       SsqPhiSun  = SPhiSun  * SPhiSun
      dCsqPhiSun  = 2.D0 * CPhiSun  * dCPhiSun
      dSsqPhiSun  = 2.D0 * SPhiSun  * dSPhiSun
!
       C2PhiSun  = DCOS(2.D0*PhiSun )
       S2PhiSun  = DSIN(2.D0*PhiSun )
      dC2PhiSun  = -2.D0 * S2PhiSun  * dPhiSun
      dS2PhiSun  =  2.D0 * C2PhiSun  * dPhiSun
!
       CLamSun  = DCOS(Lamda_Sun )
       SLamSun  = DSIN(Lamda_Sun )
      dCLamSun  = -SLamSun  * dLamda_Sun
      dSLamSun  =  CLamSun  * dLamda_Sun
!
       C2LamSun = DCOS(2.D0 * Lamda_Sun)
       S2LamSun = DSIN(2.D0 * Lamda_Sun)
      dC2LamSun = -S2LamSun * 2.D0 * dLamda_Sun
      dS2LamSun =  C2LamSun * 2.D0 * dLamda_Sun
!
!    Delauney fundamental arguments, for Steps 2A and 2B:
!     Moved 2007.01.11
       DO LL=1,5
         FUND_ARG(LL) = FA2K(LL)
        DFUND_ARG(LL) = FAD2K(LL)      ! time derivative
       ENDDO
         FUND_ARG(6) = GAST2K(1) + PI
        DFUND_ARG(6) = GAST2K(2)
!        print *,' ETDG/GAST2K: ', GAST2K
!        print *,' ETDG/FUND_ARG (new): ', FUND_ARG
!
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
       write(6,8)' Lamda_moon, dLamda_moon ', Lamda_moon/CONVD,         &
     &                                        dLamda_moon/CONVD
       write(6,8)' PHIsun, dPHIsun  ', PHIsun/CONVD, dPHIsun/CONVD
       write(6,8)' Lamda_sun, dLamda_sun ', Lamda_sun/CONVD,            &
     &                                      dLamda_sun/CONVD
       write(6,8)' CPhiMoon, SPhiMoon  ', CPhiMoon, dCPhiMoon,          &
     &                                    SPhiMoon, dSPhiMoon
       write(6,8)' CsqPhiMoon, SsqPhiMoon  ', CsqPhiMoon, dCsqPhiMoon,  &
     &                                        SsqPhiMoon, dSsqPhiMoon
       write(6,8)' C2PhiMoon, S2PhiMoon  ', C2PhiMoon, dC2PhiMoon,      &
     &                                      S2PhiMoon, dS2PhiMoon
       write(6,8)' CLamMoon, SLamMoon  ', CLamMoon, dCLamMoon,          &
     &                                    SLamMoon, dSLamMoon
       write(6,8)' C2LamMoon, S2LamMoon  ', C2LamMoon, dC2LamMoon,      &
     &                                    S2LamMoon, dS2LamMoon
       write(6,8)' CPhiSun, SPhiSun  ', CPhiSun, dCPhiSun,              &
     &                                   SPhiSun, dSPhiSun
       write(6,8)' CsqPhiSun, SsqPhiSun  ', CsqPhiSun, dCsqPhiSun,      &
     &                                      SsqPhiSun, dSsqPhiSun
       write(6,8)' C2PhiSun, S2PhiSun  ', C2PhiSun, dC2PhiSun,          &
     &                                    S2PhiSun, dS2PhiSun
       write(6,8)' CLamSun, SLamSun  ', CLamSun, dCLamSun,              &
     &                                  SLamSun, dSLamSun
       write(6,8)' C2LamSun, S2LamSun  ', C2LamSun, dC2LamSun,          &
     &                                    S2LamSun, dS2LamSun
       write(6,8)' GMMOON  ',GMMOON
       write(6,8)' GMSUN   ',GMSUN
       write(6,8)' GMEARTH ',GMEARTH
       write(6,8)' REARTH ',REARTH
       write(6,8)'  fund_arg ',  fund_arg
       write(6,8)' dfund_arg ', dfund_arg
      Endif
!
!******************************************************************
!   Loop through twice for sites 1 and 2
!
      DO k=1,2                                  !Loop over sites
!
! First check for geocenter station. There should be no tidal effects at the
!  geocenter.
      IF (Nzero .eq. k) THEN
!      print *,'ETDG: Zero site found '
         Do kk=1,3
          TIDEP(kk,k) = 0.D0
          TIDEV(kk,k) = 0.D0
         Enddo
       GO TO 100
      ENDIF
!
!   Vector from geocenter to site #k and its derivative
       do kk = 1,3
        r_vec(kk)  = USITEP(kk,k)
        dr_vec(kk) = USITEV(kk,k)
       enddo
!   Magnitude of vector from geocenter to site #k (r_vec) and its derivative
!     (Derivative should be zero)
       r_mag = vecmg(r_vec)
!         dr_mag = Dotp(r_vec,dr_vec)/r_mag
       dr_mag = 0.D0
!   Unit vector from geocenter to site # k and its derivative
!         write(6,*) 'k, r_vec: ', k, r_vec
       call vunit(r_vec,r_hat)
       do kk=1,3
        dr_hat(kk) = dr_vec(kk)/r_mag - r_vec(kk)*dr_mag/r_mag**2
       enddo
!
!   Dot product of unit Moon vector with unit site vector and its derivative
       Rmoon_dot_r  = Dotp(Rmoon_hat,r_hat)
       dRmoon_dot_r = Dotp(Rmoon_hat,dr_hat) +                          &
     &                Dotp(dRmoon_hat,r_hat)
!
!   Dot product of unit Sun vector with unit site vector and its derivative
       Rsun_dot_r = Dotp(Rsun_hat,r_hat)
       dRsun_dot_r = Dotp(Rsun_hat,dr_hat) +                            &
     &                Dotp(dRsun_hat,r_hat)
!
!   Geocentric latitude:
        Phi(k) = GEOLAT(k)
!
!--------------------------------------------------------------------------
!  Compute heavily used sine and cosine terms and derivatives
!
       CLLMoon = DCOS(SITLON(k) - Lamda_moon)
       SLLMoon = DSIN(SITLON(k) - Lamda_moon)
      dCLLMoon = -SLLMoon * (-dLamda_moon)
      dSLLMoon =  CLLMoon * (-dLamda_moon)
!
       C2LLMoon = DCOS(2.D0 * (SITLON(k) - Lamda_moon))
       S2LLMoon = DSIN(2.D0 * (SITLON(k) - Lamda_moon))
      dC2LLMoon = -S2LLMoon * 2.D0 * (-dLamda_moon)
      dS2LLMoon =  C2LLMoon * 2.D0 * (-dLamda_moon)
!
       CLLSun  = DCOS(SITLON(k) - Lamda_sun )
       SLLSun  = DSIN(SITLON(k) - Lamda_sun )
      dCLLSun  = -SLLSun  * (-dLamda_sun)
      dSLLSun  =  CLLSun  * (-dLamda_sun)
!
       C2LLSun  = DCOS(2.D0 * (SITLON(k) - Lamda_sun ))
       S2LLSun  = DSIN(2.D0 * (SITLON(k) - Lamda_sun ))
      dC2LLSun  = -S2LLSun  * 2.D0 * (-dLamda_sun)
      dS2LLSun  =  C2LLSun  * 2.D0 * (-dLamda_sun)
!
!  Constant (nearly) quantities
       CPhi   = DCOS(Phi(k))
       SPhi   = DSIN(Phi(k))
       C2Phi  = DCOS(2.D0*Phi(k))
       S2Phi  = DSIN(2.D0*Phi(k))
       CsqPhi = CPhi * CPhi
       SsqPhi = SPhi * SPhi
!
!--------------------------------------------------------------------------
!   Elastic case with latitude dependence:
!      h_sub_2 = .6026D0 - .0006D0 * (1.5*SsqPhi - .5D0)
!      l_sub_2 = .0831D0 + .0002D0 * (1.5*SsqPhi - .5D0)
!   Anelastic case with latitude dependence:
       h_sub_2 = .6078D0 - .0006D0 * (1.5D0*SsqPhi - .5D0)
       l_sub_2 = .0847D0 + .0002D0 * (1.5D0*SsqPhi - .5D0)
!
       term1M = GMMOON / Rmoon_mag**3
       dterm1M = -3.D0 * GMMOON * dRmoon_mag / Rmoon_mag**4
       term1S = GMSUN / Rsun_mag**3
       dterm1S = -3.D0 * GMSUN * dRsun_mag / Rsun_mag**4
!
       term2  =  REARTH**4 / GMEARTH
       dterm2  =  0.D0
!
       term3M  = h_sub_2 * (1.5D0 * (Rmoon_dot_r)**2 - .5D0)
       dterm3M = h_sub_2 * 3.D0 * (Rmoon_dot_r) * dRmoon_dot_r
       term3S  = h_sub_2 * (1.5D0 * (Rsun_dot_r)**2 - .5D0)
       dterm3S = h_sub_2 * 3.D0 * (Rsun_dot_r) * dRsun_dot_r
!
       term4M  =  3.D0 * l_sub_2 * Rmoon_dot_r
       dterm4M =  3.D0 * l_sub_2 * dRmoon_dot_r
       term4S  =  3.D0 * l_sub_2 * Rsun_dot_r
       dterm4S =  3.D0 * l_sub_2 * dRsun_dot_r
!
       term5M  =  -3.D0 * l_sub_2 * Rmoon_dot_r**2
       dterm5M =  -6.D0 * l_sub_2 * Rmoon_dot_r * dRmoon_dot_r
       term5S  =  -3.D0 * l_sub_2 * Rsun_dot_r**2
       dterm5S =  -6.D0 * l_sub_2 * Rsun_dot_r * dRsun_dot_r
!
       term1M_2 =  term1M * term2
       dterm1M_2 = dterm1M*term2 + term1M*dterm2
!
       term1S_2 = term1S * term2
       dterm1S_2 = dterm1S*term2 + term1S*dterm2
!
!  Step 1A: Degree 2 tides, using Equation 8, p. 61, IERS Technical Note 21,
!   IERS Conventions (1996).
       do kk = 1,3
         delta_r2(kk,k) = term1M_2 * ( term3M * r_hat(kk)               &
     &                               + term4M * Rmoon_hat(kk)           &
     &                               + term5M * r_hat(kk) )             &
     &                  + term1S_2 * ( term3S * r_hat(kk)               &
     &                               + term4S * Rsun_hat(kk)            &
     &                               + term5S * r_hat(kk) )
!
!       ddelta_r2(kk,k) = dterm1M_2 * ( term3M * r_hat(kk) &
!    &                               + term4M * Rmoon_hat(kk) &
!    &                               + term5M * r_hat(kk) ) &
!    &       + term1M_2 * ( dterm3M * r_hat(kk) + term3M * dr_hat(kk) &
!    &            + dterm4M * Rmoon_hat(kk) + term4M * dRmoon_hat(kk) &
!    &                    + dterm5M * r_hat(kk) + term5M * dr_hat(kk) ) &
!    &                 + dterm1S_2 * ( term3S * r_hat(kk) &
!    &                               + term4S * Rsun_hat(kk) &
!    &                               + term5S * r_hat(kk) ) &
!    &       + term1S_2 * ( dterm3S * r_hat(kk) + term3S * dr_hat(kk) &
!    &            + dterm4S * Rsun_hat(kk) + term4S * dRsun_hat(kk) &
!    &                    + dterm5S * r_hat(kk) + term5S * dr_hat(kk) )
!
        ddelta_r2(kk,k) = dterm1M_2 * ( term3M * r_hat(kk) +            &
     &              term4M * Rmoon_hat(kk) + term5M * r_hat(kk) ) +     &
     &         term1M_2 * ( dterm3M * r_hat(kk) + term3M * dr_hat(kk) + &
     &              dterm4M * Rmoon_hat(kk) + term4M * dRmoon_hat(kk) + &
     &                   dterm5M * r_hat(kk) + term5M * dr_hat(kk) ) +  &
     &                  dterm1S_2 * ( term3S * r_hat(kk) + &
     &                  term4S * Rsun_hat(kk) + term5S * r_hat(kk) ) +  &
     &         term1S_2 * ( dterm3S * r_hat(kk) + term3S * dr_hat(kk) + &
     &              dterm4S * Rsun_hat(kk) + term4S * dRsun_hat(kk) +   &
     &                      dterm5S * r_hat(kk) + term5S * dr_hat(kk) )
!
       enddo
!
!  Partials of second order tide with respect to h2 and l2:
       do kk = 1,3
         delta_r2_dh2(kk,k) = term1M_2 * term3M/h_sub_2 * r_hat(kk)     &
     &                      + term1S_2 * term3S/h_sub_2 * r_hat(kk)
         delta_r2_dl2(kk,k) = term1M_2 * (term4M/l_sub_2 * Rmoon_hat(kk) &
     &                                 +  term5M/l_sub_2 * r_hat(kk) )  &
     &                      + term1S_2 * (term4S/l_sub_2 * Rsun_hat(kk) &
     &                                 +  term5S/l_sub_2 * r_hat(kk) )
!
        ddelta_r2_dh2(kk,k) = dterm1M_2 * term3M/h_sub_2 * r_hat(kk)    &
     &       + term1M_2 * (dterm3M/h_sub_2 * r_hat(kk) + term3M/h_sub_2 &
     &             * dr_hat(kk)) &
     &       + dterm1S_2 * term3S/h_sub_2 * r_hat(kk) &
     &       + term1S_2 * (dterm3S/h_sub_2 * r_hat(kk) + term3S/h_sub_2 &
     &             * dr_hat(kk))
!
!       ddelta_r2_dl2(kk,k) = dterm1M_2 * (term4M/l_sub_2 *Rmoon_hat(kk) &
!    &                          + term5M/l_sub_2 * r_hat(kk) ) &
!    &     + term1M_2 * ( dterm4M/l_sub_2 * Rmoon_hat(kk) + &
!    &                      term4M/l_sub_2 * dRmoon_hat(kk) &
!    &     + dterm5M/l_sub_2 * r_hat(kk) + term5M/l_sub_2 * dr_hat(kk))
!    &     + dterm1S_2 * (term4S/l_sub_2 * Rsun_hat(kk) &
!    &         + term5S/l_sub_2 * r_hat(kk) ) &
!    &       + term1S_2 * (dterm4S/l_sub_2 * Rsun_hat(kk) &
!    &         + term4S/l_sub_2 * dRsun_hat(kk) &
!    &         + dterm5S/l_sub_2 * r_hat(kk) + term5S/l_sub_2 &
!    &         * dr_hat(kk) )
!
        ddelta_r2_dl2(kk,k) = dterm1M_2 * (term4M/l_sub_2               &
     &      * Rmoon_hat(kk) + term5M/l_sub_2 * r_hat(kk) ) +            &
     &       term1M_2 * ( dterm4M/l_sub_2 * Rmoon_hat(kk) +             &
     &       term4M/l_sub_2 * dRmoon_hat(kk) + &
     &       dterm5M/l_sub_2 * r_hat(kk) + term5M/l_sub_2 * dr_hat(kk)) &
     &     + dterm1S_2 * (term4S/l_sub_2 * Rsun_hat(kk) +               &
     &       term5S/l_sub_2 * r_hat(kk) ) +                             &
     &       term1S_2 * (dterm4S/l_sub_2 * Rsun_hat(kk) +               &
     &       term4S/l_sub_2 * dRsun_hat(kk) +                           &
     &       dterm5S/l_sub_2 * r_hat(kk) + term5S/l_sub_2 * dr_hat(kk) )
       enddo
!
!  Tide difference if elastic case used instead of anelastic case
!      Do kk = 1,3
!       ELASP(kk,k) = -.0052* delta_r2_dh2(kk,k) +
!    *                -.0016* delta_r2_dl2(kk,k)
!       ELASV(kk,k) = -.0052*ddelta_r2_dh2(kk,k) +
!    *                -.0016*ddelta_r2_dl2(kk,k)
!      Enddo
!
       term01M = GMMOON / Rmoon_mag**4
       dterm01M = -4.D0 * GMMOON * dRmoon_mag / Rmoon_mag**5
!
       term02  =  REARTH**5 / GMEARTH
       dterm02  =  0.D0
!
       term03M  = h_sub_3 * (2.5D0*Rmoon_dot_r**3 - 1.5D0*Rmoon_dot_r)
       dterm03M = h_sub_3 * (7.5D0*Rmoon_dot_r**2*dRmoon_dot_r -        &
     &                       1.5D0*dRmoon_dot_r)
       term04M  = l_sub_3 * (7.5D0*Rmoon_dot_r**2 - 1.5D0)
       dterm04M = l_sub_3 * (15.D0*Rmoon_dot_r*dRmoon_dot_r)
!
       term05M  = -l_sub_3 * (7.5D0*Rmoon_dot_r**3 - 1.5D0*Rmoon_dot_r)
       dterm05M = -l_sub_3 * (22.5D0*Rmoon_dot_r**2*dRmoon_dot_r        &
     &                     - 1.5D0*dRmoon_dot_r)
!
!  Step 1B: Degree 3 tides, using Equation 9, p. 61, IERS Technical Note 21,
!  IERS Conventions (1996).
       Do kk = 1,3
         delta_r3(kk,k) = term01M*term02 *                              &
     &       ( term03M*r_hat(kk) +                                      &
     &         term04M*Rmoon_hat(kk) +                                  &
     &         term05M*r_hat(kk) )
!
         ddelta_r3(kk,k) = dterm01M*term02 * ( term03M*r_hat(kk) +      &
     &         term04M*Rmoon_hat(kk) +  term05M*r_hat(kk) )             &
     &    + term01M*term02 * ( dterm03M*r_hat(kk) + term03M*dr_hat(kk)  &
     &    +    dterm04M*Rmoon_hat(kk) + term04M*dRmoon_hat(kk)          &
     &    +    dterm05M*r_hat(kk) +  term05M*dr_hat(kk) )
       Enddo
!
! Step 1D, Contributions from latitude dependence
!    Equations 11 and 12, page 62
!
!   Legendre polynomials - Moon
       P12_moon = 3.D0 * CFRmoon(1) * CFRmoon(3) / CFRmoon_mag**2 /     &
     &             CLamMoon
       dP12_moon =                                                      &
     &   3.D0*dCFRmoon(1)*CFRmoon(3)/CFRmoon_mag**2/CLamMoon            &
     & + 3.D0*CFRmoon(1)*dCFRmoon(3)/CFRmoon_mag**2/CLamMoon            &
     & - 6.D0*CFRmoon(1)*CFRmoon(3) * dCFRmoon_mag / CFRmoon_mag**3     &
     &        /CLamMoon                                                 &
     & + 3.D0*CFRmoon(1)*CFRmoon(3)/CFRmoon_mag**2 *                    &
     &        (-dCLamMoon)  / CLamMoon**2
!
       P22_moon =                                                       &
     &     (3.D0 / CFRmoon_mag**2) * (CFRmoon(1)**2 - CFRmoon(2)**2) /  &
     &         C2LamMoon
       dP22_moon =                                                      &
     &   (3.D0 / CFRmoon_mag**2) * ( 2.D0*CFRmoon(1)*dCFRmoon(1) -      &
     &     2.D0*CFRmoon(2)*dCFRmoon(2)  ) / C2LamMoon                   &
     & - (6.D0*dCFRmoon_mag/CFRmoon_mag**3) * (CFRmoon(1)**2 -          &
     &     CFRmoon(2)**2) / C2LamMoon                                   &
     & + (3.D0 / CFRmoon_mag**2) * (CFRmoon(1)**2 - CFRmoon(2)**2) *    &
     &    (-dC2LamMoon) / C2LamMoon**2
!
!   Legendre polynomials - Sun
       P12_sun = 3.D0 * CFRsun(1) * CFRsun(3) / CFRsun_mag**2 /         &
     &            CLamSun
       dP12_sun = &
     &   3.D0*dCFRsun(1)*CFRsun(3)/CFRsun_mag**2/CLamSun                &
     & + 3.D0*CFRsun(1)*dCFRsun(3)/CFRsun_mag**2/CLamSun                &
     & - 6.D0*CFRsun(1)*CFRsun(3) * dCFRsun_mag / CFRsun_mag**3         &
     &        /CLamSun                                                  &
     & + 3.D0*CFRsun(1)*CFRsun(3)/CFRsun_mag**2 *                       &
     &        (-dCLamSun) / CLamSun**2
!
       P22_sun  = &
     &     (3.D0 / CFRsun_mag**2) * (CFRsun(1)**2 - CFRsun(2)**2) /     &
     &         C2LamSun
       dP22_sun  =                                                      &
     &   (3.D0 / CFRsun_mag**2) * ( 2.D0*CFRsun(1)*dCFRsun(1) -         &
     &     2.D0*CFRsun(2)*dCFRsun(2) ) / C2LamSun                       &
     & - (6.D0*dCFRsun_mag/CFRsun_mag**3) * (CFRsun(1)**2 -             &
     &     CFRsun(2)**2) / C2LamSun                                     &
     & + (3.D0 / CFRsun_mag**2) * (CFRsun(1)**2 - CFRsun(2)**2) *       &
     &    (-dC2LamSun) / C2LamSun**2
!
! Step 1D1: Equation 11, page 62, topocentric contribution from the diurnal
!   band (with l_1 = 0.0012)
        l1_11 = 0.0012D0
!  Up
        topo11(1,k) = 0.D0
       dtopo11(1,k) = 0.D0
!  East
       topo11(2,k) =  l1_11 * SPhi * term1M_2 * P12_moon * C2Phi*SLLMoon &
     &              + l1_11 * SPhi * term1S_2 * P12_sun  * C2Phi *SLLSun
      dtopo11(2,k) = &
     &        l1_11 * SPhi * dterm1M_2 * P12_moon * C2Phi * SLLMoon     &
     &     +  l1_11 * SPhi * term1M_2 * dP12_moon * C2Phi * SLLMoon     &
     &     +  l1_11 * SPhi * term1M_2 * P12_moon * C2Phi  * dSLLMoon    &
     &     +  l1_11 * SPhi * dterm1S_2 * P12_sun * C2Phi * SLLSun       &
     &     +  l1_11 * SPhi * term1S_2 * dP12_sun * C2Phi * SLLSun       &
     &     +  l1_11 * SPhi * term1S_2 * P12_sun  * C2Phi * dSLLSun
!
!  North
       topo11(3,k) = -l1_11 * SPhi**2 * term1M_2 * P12_moon * CLLMoon   &
     &              - l1_11 * SPhi**2 * term1S_2 * P12_sun  * CLLSun
      dtopo11(3,k) = -l1_11 * SPhi**2 * dterm1M_2 * P12_moon * CLLMoon  &
     &         -  l1_11 * SPhi**2 * term1M_2 * dP12_moon * CLLMoon      &
     &         -  l1_11 * SPhi**2 * term1M_2 * P12_moon  * dCLLMoon     &
     &         -  l1_11 * SPhi**2 * dterm1S_2 * P12_sun * CLLSun        &
     &         -  l1_11 * SPhi**2 * term1S_2 * dP12_sun * CLLSun        &
     &         -  l1_11 * SPhi**2 * term1S_2 * P12_sun * dCLLSun
!
!  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
!   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo11(1,k), CF11(1,k) )
       CALL VECRT( R2K  (1,1,1), CF11(1,k), delta11(1,k) )
!
       CALL VECRT( RTTOCF(1,1,k), dtopo11(1,k), dCF11(1,k) )
       CALL VECRT( R2K  (1,1,1), dCF11(1,k), vec1 )
       CALL VECRT( R2K  (1,1,2),  CF11(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta11(1,k) )
!
! Step 1D2: Equation 12, page 62, topocentric contribution from the
!   semidiurnal band (with l_1 = 0.0024)
        l1_12 = 0.0024D0
!  Up
       topo12(1,k) = 0.D0
      dtopo12(1,k) = 0.D0
!  East
       topo12(2,k) = -0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon &
     &                      * SPhi * S2LLMoon &
     &    -           0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun  &
     &                      * SPhi * S2LLSun
      dtopo12(2,k) = &
     &    -0.5D0 * l1_12 * SPhi * CPhi * dterm1M_2 * P22_moon           &
     &         * SPhi * S2LLMoon - &
     &     0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * dP22_moon           &
     &         * SPhi * S2LLMoon - &
     &     0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon            &
     &         * SPhi * dS2LLMoon - &
     &     0.5D0 * l1_12 * SPhi * CPhi * dterm1S_2 * P22_sun            &
     &         * SPhi * S2LLSun - &
     &     0.5D0 * l1_12 * SPhi * CPhi  * term1S_2 * dP22_sun           &
     &         * SPhi * S2LLSun - &
     &     0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun             &
     &         * SPhi * dS2LLSun
!  North
       topo12(3,k) =                                                    &
     &   -0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon * C2LLMoon  &
     &  - 0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun  * C2LLSun
!
      dtopo12(3,k) =                                                    &
     &   -0.5D0 * l1_12 * SPhi * CPhi * dterm1M_2 * P22_moon * C2LLMoon &
     &  - 0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * dP22_moon * C2LLMoon &
     &  - 0.5D0 * l1_12 * SPhi * CPhi * term1M_2 * P22_moon * dC2LLMoon &
     &  - 0.5D0 * l1_12 * SPhi * CPhi * dterm1S_2 * P22_sun * C2LLSun   &
     &  - 0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * dP22_sun * C2LLSun   &
     &  - 0.5D0 * l1_12 * SPhi * CPhi * term1S_2 * P22_sun * dC2LLSun
!
!  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
!   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo12(1,k), CF12(1,k) )
       CALL VECRT( R2K  (1,1,1), CF12(1,k), delta12(1,k) )
!
       CALL VECRT( RTTOCF(1,1,k), dtopo12(1,k), dCF12(1,k) )
       CALL VECRT( R2K  (1,1,1), dCF12(1,k), vec1 )
       CALL VECRT( R2K  (1,1,2),  CF12(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta12(1,k) )
!
!
!  Step 1C1. Equation 13, Out-of-Phase for degree 2 only, diurnal components
       h_I13 = -0.0025D0
       l_I13 = -0.0007D0
!
!   Equation 13A: Up component
       topo13(1,k) =                                                    &
     &  -.75D0 * h_I13 * term1M_2 * S2PhiMoon * S2Phi * SLLMoon         &
     &  -.75D0 * h_I13 * term1S_2 * S2PhiSun  * S2Phi * SLLSun
       dtopo13(1,k) =                                                   &
     &  -.75D0 * h_I13 * dterm1M_2 * S2PhiMoon * S2Phi * SLLMoon        &
     &  -.75D0 * h_I13 * term1M_2 * dS2PhiMoon * S2Phi * SLLMoon        &
     &  -.75D0 * h_I13 * term1M_2 * S2PhiMoon  * S2Phi * dSLLMoon       &
     &  -.75D0 * h_I13 * dterm1S_2 * S2PhiSun * S2Phi * SLLSun          &
     &  -.75D0 * h_I13 * term1S_2 * dS2PhiSun * S2Phi * SLLSun          &
     &  -.75D0 * h_I13 * term1S_2 * S2PhiSun  * S2Phi * dSLLSun
!
!    Equation 13B: East component
       topo13(2,k) =                                                    &
     &  -1.5D0 * l_I13 * term1M_2 * S2PhiMoon * SPhi * CLLMoon          &
     &  -1.5D0 * l_I13 * term1S_2 * S2PhiSun  * SPhi * CLLSun
       dtopo13(2,k) =                                                   &
     &  -1.5D0 * l_I13 * dterm1M_2 * S2PhiMoon  * SPhi * CLLMoon        &
     &  -1.5D0 * l_I13 * term1M_2  * dS2PhiMoon * SPhi * CLLMoon        &
     &  -1.5D0 * l_I13 * term1M_2  * S2PhiMoon  * SPhi * dCLLMoon       &
     &  -1.5D0 * l_I13 * dterm1S_2 * S2PhiSun  * SPhi * CLLSun          &
     &  -1.5D0 * l_I13 * term1S_2  * dS2PhiSun * SPhi * CLLSun          &
     &  -1.5D0 * l_I13 * term1S_2  * S2PhiSun  * SPhi * dCLLSun
!
!    Equation 13B: North component
       topo13(3,k) =                                                    &
     &  -1.5D0 * l_I13 * term1M_2 * S2PhiMoon * C2Phi * SLLMoon         &
     &  -1.5D0 * l_I13 * term1S_2 * S2PhiSun  * C2Phi * SLLSun
       dtopo13(3,k) = &
     &  -1.5D0 * l_I13 * dterm1M_2 * S2PhiMoon * C2Phi * SLLMoon        &
     &  -1.5D0 * l_I13 * term1M_2 * dS2PhiMoon * C2Phi * SLLMoon        &
     &  -1.5D0 * l_I13 * term1M_2 * S2PhiMoon * C2Phi * dSLLMoon        &
     &  -1.5D0 * l_I13 * dterm1S_2 * S2PhiSun * C2Phi * SLLSun          &
     &  -1.5D0 * l_I13 * term1S_2 * dS2PhiSun * C2Phi * SLLSun          &
     &  -1.5D0 * l_I13 * term1S_2 * S2PhiSun * C2Phi * dSLLSun
!
!  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
!   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo13(1,k), CF13(1,k) )
       CALL VECRT( R2K  (1,1,1), CF13(1,k), delta13(1,k) )
!
       CALL VECRT( RTTOCF(1,1,k), dtopo13(1,k), dCF13(1,k) )
       CALL VECRT( R2K  (1,1,1), dCF13(1,k), vec1 )
       CALL VECRT( R2K  (1,1,2),  CF13(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta13(1,k) )
!
!  Step 1C2. Equation 14, Out-of-Phase for degree 2 only, semidiurnal
!   components
       h_I14 = -0.0022D0
       l_I14 = -0.0007D0
!
!    Equation 14A: Up component
       topo14(1,k) =                                                    &
     &  -.75D0 * h_I14 * term1M_2 * CsqPhiMoon * Cphi**2 * S2LLMoon     &
     &  -.75D0 * h_I14 * term1S_2 * CsqPhiSun  * Cphi**2 * S2LLSun
       dtopo14(1,k) =                                                   &
     &  -.75D0 * h_I14 * dterm1M_2 * CsqPhiMoon * Cphi**2 * S2LLMoon    &
     &  -.75D0 * h_I14 * term1M_2 * dCsqPhiMoon * Cphi**2 * S2LLMoon    &
     &  -.75D0 * h_I14 * term1M_2 * CsqPhiMoon * Cphi**2 * dS2LLMoon    &
     &  -.75D0 * h_I14 * dterm1S_2 * CsqPhiSun * Cphi**2 * S2LLSun      &
     &  -.75D0 * h_I14 * term1S_2 * dCsqPhiSun * Cphi**2 * S2LLSun      &
     &  -.75D0 * h_I14 * term1S_2 * CsqPhiSun * Cphi**2 * dS2LLSun
!
!    Equation 14B: East component
       topo14(2,k) =                                                    &
     &   .75D0 * l_I14 * term1M_2 * CsqPhiMoon * (-2.D0)*CPhi * C2LLMoon  &
     & + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * (-2.D0)*CPhi * C2LLSun
       dtopo14(2,k) =                                                   &
     &   .75D0 * l_I14 * dterm1M_2 * CsqPhiMoon * (-2.D0)*CPhi * C2LLMoon &
     & + .75D0 * l_I14 * term1M_2 * dCsqPhiMoon * (-2.D0)*CPhi * C2LLMoon &
     & + .75D0 * l_I14 * term1M_2 * CsqPhiMoon * (-2.D0)*CPhi * dC2LLMoon &
     & + .75D0 * l_I14 * dterm1S_2 * CsqPhiSun * (-2.D0)*CPhi * C2LLSun   &
     & + .75D0 * l_I14 * term1S_2 * dCsqPhiSun * (-2.D0)*CPhi * C2LLSun   &
     & + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * (-2.D0)*CPhi * dC2LLSun
!
!    Equation 14B: North component
       topo14(3,k) =                                                    &
     &   .75D0 * l_I14 * term1M_2 * CsqPhiMoon * S2Phi * S2LLMoon       &
     & + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * S2Phi * S2LLSun
       dtopo14(3,k) =                                                   &
     &   .75D0 * l_I14 * dterm1M_2 * CsqPhiMoon * S2Phi * S2LLMoon      &
     & + .75D0 * l_I14 * term1M_2 * dCsqPhiMoon * S2Phi * S2LLMoon      &
     & + .75D0 * l_I14 * term1M_2 * CsqPhiMoon * S2Phi * dS2LLMoon      &
     & + .75D0 * l_I14 * dterm1S_2 * CsqPhiSun * S2Phi * S2LLSun        &
     & + .75D0 * l_I14 * term1S_2 * dCsqPhiSun * S2Phi * S2LLSun        &
     & + .75D0 * l_I14 * term1S_2 * CsqPhiSun  * S2Phi * dS2LLSun
!
!  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
!   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo14(1,k), CF14(1,k) )
       CALL VECRT( R2K  (1,1,1), CF14(1,k), delta14(1,k) )
!
       CALL VECRT( RTTOCF(1,1,k), dtopo14(1,k), dCF14(1,k) )
       CALL VECRT( R2K  (1,1,1), dCF14(1,k), vec1 )
       CALL VECRT( R2K  (1,1,2),  CF14(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta14(1,k) )
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  !!!!!!!!!!!! UPDATED FOR IERS CONVENTIONS (2003) !!!!!!!!!!!!!!
!  Step 2A, Equation 16, [IERS Conventions (2003), Chapter 7, page 12]
!     Frequency domain corrections. In phase for degree 2, diurnal
!     tides. Computed using the Delauney fundamental arguments
!
!!!! Bug here if station 1 is at the geocenter, remove IF/THEN/ENDIF,
!!!!  2007.01.10
!**   IF (K .EQ. 1) THEN
!      DO LL=1,5
!        FUND_ARG(LL) = FA2K(LL)
!       DFUND_ARG(LL) = FAD2K(LL)      ! time derivative
!      ENDDO
!        FUND_ARG(6) = GAST2K(1) + PI
!       DFUND_ARG(6) = GAST2K(2)
!        print *,' ETDG/FUND_ARG (new): ', FUND_ARG
!**   ENDIF
!
      Do jf =1, 11
          arg = 0.D0
        d_arg = 0.D0
        Do ll=1,6
           arg = arg   - Table75a(ll,jf)* fund_arg(ll)
         d_arg = d_arg - Table75a(ll,jf)*dfund_arg(ll)
        Enddo
          theta15(jf) = DMOD(arg,TWOPI)
         dtheta15(jf) = d_arg
      Enddo
!
       Do I=1,3
         topo15(I,k) = 0.0D0
        dtopo15(I,k) = 0.0D0
       Enddo
!
      Do jf=1,11
!
!  Up component (meters)
       topo15(1,k) =  topo15(1,k) + Table75a(7,jf) * S2Phi              &
     &           * DSIN(theta15(jf) + SITLON(k)) * 1.D-3                &
     &          +  Table75a(8,jf) * S2Phi                               &
     &           * DCOS(theta15(jf) + SITLON(k)) * 1.D-3
      dtopo15(1,k) = dtopo15(1,k) + Table75a(7,jf) * S2Phi              &
     &           * DCOS(theta15(jf) + SITLON(k)) * dtheta15(jf) * 1.D-3 &
     &          -  Table75a(8,jf) * S2Phi                               &
     &           * DSIN(theta15(jf) + SITLON(k)) * dtheta15(jf) * 1.D-3
!
!  East component (meters)
        topo15(2,k) = topo15(2,k) + Table75a( 9,jf) * SPhi              &
     &           * DCOS(theta15(jf)+SITLON(k)) * 1.D-3                  &
     &           - Table75a(10,jf) * SPhi                               &
     &           * DSIN(theta15(jf)+SITLON(k)) * 1.D-3
       dtopo15(2,k) = dtopo15(2,k) + Table75a( 9,jf) * SPhi             &
     &           * (-DSIN(theta15(jf)+SITLON(k))) * dtheta15(jf)*1.D-3    &
     &           - Table75a(10,jf) * SPhi                               &
     &           * DSIN(theta15(jf)+SITLON(k)) * dtheta15(jf)* 1.D-3
!
!  North component (meters)
       topo15(3,k) = topo15(3,k) + Table75a( 9,jf) * C2Phi              &
     &           * DSIN(theta15(jf)+SITLON(k)) * 1.D-3                  &
     &           + Table75a(10,jf) * C2Phi                              &
     &           * DCOS(theta15(jf)+SITLON(k)) * 1.D-3
       dtopo15(3,k) = dtopo15(3,k) + Table75a( 9,jf) * C2Phi            &
     &           * DCOS(theta15(jf)+SITLON(k)) * dtheta15(jf) * 1.D-3   &
     &           + Table75a(10,jf) * C2Phi                              &
     &           * (-DSIN(theta15(jf)+SITLON(k))) * dtheta15(jf) * 1.D-3
!  --- need to check time derivatives, etc !!!!!
!
      Enddo
!
!  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
!   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo15(1,k), CF15(1,k) )
       CALL VECRT( R2K  (1,1,1), CF15(1,k), delta15(1,k) )
!
       CALL VECRT( RTTOCF(1,1,k), dtopo15(1,k), dCF15(1,k) )
       CALL VECRT( R2K  (1,1,1), dCF15(1,k), vec1 )
       CALL VECRT( R2K  (1,1,2),  CF15(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta15(1,k) )
!
!  Step 2B, Equation 17, [IERS Conventions (2003), Chapter 5, page 13]
!     Frequency domain corrections, in phase and out of
!    phase for degree 2, long period tides. Computed using the Delauney
!    fundamental arguments
!
      Do jf =1, 5
          arg = 0.D0
        d_arg = 0.D0
        Do ll=1,6
           arg = arg   - Table75b(ll,jf)* fund_arg(ll)
         d_arg = d_arg - Table75b(ll,jf)*dfund_arg(ll)
        Enddo
          theta16(jf) = DMOD(arg,TWOPI)
         dtheta16(jf) = d_arg
!       IF(K .EQ. 1) THEN
!        if (theta16(jf) .gt. PI) theta16(jf) = theta16(jf) - TWOPI
!        if (theta16(jf) .le. -PI) theta16(jf) = theta16(jf) + TWOPI
!        print *,' theta16(degrees) ', jf,theta16(jf)/CONVD
!       ENDIF
      Enddo
!
       Do I=1,3
         topo16(I,k) = 0.0D0
        dtopo16(I,k) = 0.0D0
       Enddo
!
      Do jf=1,5
!  Up component (meters)
        topo16(1,k) = topo16(1,k) + (1.5D0*SPhi**2 - .5D0)              &
     &             * ( Table75b(7,jf)*DCOS(theta16(jf)) +               &
     &                 Table75b( 8,jf)*DSIN(theta16(jf)) ) * 1.D-3
       dtopo16(1,k) = dtopo16(1,k) + (1.5D0*SPhi**2 -.5D0)              &
     &             * (-Table75b(7,jf)*DSIN(theta16(jf))*dtheta16(jf) +  &
     &                 Table75b( 8,jf)*DCOS(theta16(jf))*dtheta16(jf) ) &
     &             * 1.D-3
!
!  East component (meters) ==> ZERO
!       topo16(2,k) = 0.0D0
!      dtopo16(2,k) = 0.0D0
!
!  North component (meters)
        topo16(3,k) = topo16(3,k)                                       &
     &    + S2Phi *  ( Table75b( 9,jf)*DCOS(theta16(jf)) +              &
     &         Table75b(10,jf)*DSIN(theta16(jf)) ) * 1.D-3
       dtopo16(3,k) = dtopo16(3,k)                                      &
     &    + S2Phi *  (-Table75b( 9,jf)*DSIN(theta16(jf))*dtheta16(jf) + &
     &         Table75b(10,jf)*DCOS(theta16(jf))*dtheta16(jf) ) * 1.D-3
!
      Enddo
!
!  Rotate vectors from topocentric to crust-fixed and then from crust-fixed
!   to J2000.
       CALL VECRT( RTTOCF(1,1,k), topo16(1,k), CF16(1,k) )
       CALL VECRT( R2K  (1,1,1), CF16(1,k), delta16(1,k) )
!
       CALL VECRT( RTTOCF(1,1,k), dtopo16(1,k), dCF16(1,k) )
       CALL VECRT( R2K  (1,1,1), dCF16(1,k), vec1 )
       CALL VECRT( R2K  (1,1,2),  CF16(1,k), vec2 )
       CALL VECAD( vec1, vec2, ddelta16(1,k) )
!
!************************************************************************
!
!  Finished computing all the pieces, now add them up
        Do kk = 1,3
          TIDEP(kk,k) = delta_r2(kk,k) + delta_r3(kk,k)                 &
     &          + delta11(kk,k)  + delta12(kk,k) + delta13(kk,k)        &
     &          + delta14(kk,k)  + delta15(kk,k) + delta16(kk,k)
!
          TIDEV(kk,k) = ddelta_r2(kk,k) + ddelta_r3(kk,k)               &
     &          + ddelta11(kk,k) + ddelta12(kk,k) + ddelta13(kk,k)      &
     &          + ddelta14(kk,k) + ddelta15(kk,k) + ddelta16(kk,k)
!
        Enddo
!
!****************************************************************************
!   Check for debug output
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
       write(6,8)' C2LLMoon, S2LLMoon ', C2LLMoon, dC2LLMoon,           &
     &                                    S2LLMoon, dS2LLMoon
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
       write(6,8)' delta_r2    ', delta_r2(1,k), delta_r2(2,k),         &
     &                            delta_r2(3,k)
       write(6,8)' ddelta_r2   ', ddelta_r2(1,k),ddelta_r2(2,k),        &
     &                            ddelta_r2(3,k)
       write(6,8)' term01M, dterm01M ', term01M, dterm01M
       write(6,8)' term02,  dterm02  ', term02, dterm02
       write(6,8)' term03M, dterm03M ', term03M, dterm03M
       write(6,8)' term04M, dterm04M ', term04M, dterm04M
       write(6,8)' term05M, dterm05M ', term05M, dterm05M
       write(6,8)' delta_r3  ', delta_r3(1,k),delta_r3(2,k),            &
     &                            delta_r3(3,k)
       write(6,8)' ddelta_r3 ', ddelta_r3(1,k),ddelta_r3(2,k),          &
     &                            ddelta_r3(3,k)
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
!
      Endif
!
 100  Continue
!   Close loop over sites
      Enddo                                  !Loop over sites
!
      If (KETDD .ne. 0) Then
      write (6, 9201)  TIDEP, TIDEV
 9201 FORMAT (1X, "TIDEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "TIDEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
      Endif
!
    8 FORMAT(A,4D25.16/(7X,5D25.16))
!
!     Normal program conclusion.
      RETURN
      END
!**********************************************************************
      SUBROUTINE ETDP ( R2K, SITLAT, STAR, TCTOCF )
      IMPLICIT None
!
!     ETDP is the Earth Tide Module partial derivatives section. It calculates
!     the partial derivatives of the delay and rate with respect to site
!     dependent Earth tide parameters.
!
!     ETDP Program Interface:
!
!       Calling sequence -
!           Input variables:
!             1. R2K  (3,3,3)  - The complete crust fixed to J2000 rotation
!                                matrix and its first two CT time derivatives.
!                                (unitless, 1/sec, 1/sec**2)
!             2. SITLAT(2)     - The site geodetic latitudes. (RAD)
!             3. STAR(3)       - The J2000.0 source unit vector. (unitless)
!             4. TCTOCF(3,3,2) - The rotation matrix which rotates the
!                                topocentric reference system to the crust
!                                fixed reference system at each site.
!
!     Common blocks used:
!
      INCLUDE 'cphys11.i'
!            Variables 'from':
!              1. VLIGHT  -  The vacuum velocity of light.  (M/SEC)
!
      INCLUDE 'ccon.i'
!         Variables 'from':
!           1. KETDC  -  The module flow control flag.
!                        = 0 => Default, IERS 1996 Earth Tide Model
!                        = 1 => Earth tide model OFF, no corrections computed
!           2. KETDD  -  The debug output flag.
!
!   Program specifications -
      Real*8   R2K(3,3,3), SITLAT(2), STAR(3), TCTOCF(3,3,2)
      Integer*4 I, J, K, L
!
!     Database access:
!       'PUT' Variables: None
!          Access codes: None
!
!     Subroutine Interface:
!            Caller subroutines: DRIVP
!            Called subroutines: PUT4
!
! 5.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
!                    PETER DENATALE 07/12/77
!                    BRUCE SCHUPLER 03/24/78
!                    BRUCE SCHUPLER 11/02/78
!                    Jim Ryan 89.06.29 Character strings and clean up.
!                    Jim Ryan 89:10:05 CPHYS common made an include file.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                    implimented.
!                    Jim Ryan 90.01.20 Documentation of the partials array
!                                      fixed.
!                    David Gordon 93NOV23 Modified for IERS Earth tides.
!                    David Gordon 94.04.06 Changed to 'Implicit None'
!                    David Gordon 94.04.27 New common block 'tide_stuff'.
!                    David Gordon 98.06.24 Obsolete code removed. New Puts
!                                 for 1996 IERS model.
!                    David Gordon 98.09.30 Increased dimensions on PUT4 of
!                                 partial derivatives (ETJMGPAR).
!                    David Gordon 98.01.06 Return immediately if Correlator.
!                    Jim Ryan     2002.09 Integer*4 conversion
!                    David Gordon 2004.08.05 PUT's removed.
!
!  ETDP Program Structure
!
!  There are no partials! Keep subroutine for future use.
!
!     Normal conclusion.
      RETURN
      END
!************************************************************************
      SUBROUTINE ETDC ( TIDEP, TIDEV, STAR )
      IMPLICIT None
!
!     ETDC is the Earth Tide Module contribution section. It computes the
!     contributions to the delay and rate due to Earth tide effects.
!
!     ETDC Program Interface:
!
!       Calling sequence -
!           Input variables:
!             1. TIDEP(3,2) - The corrections to the J2000 geocentric site
!                             position due to Earth tidal effects at each site.
!                             (M)
!             2. TIDEV(3,2) - The corrections to the J2000.0 geocentric site
!                             velocity vectors due to Earth tidal effects at
!                             each site. (M/SEC)
!             3. STAR(3)    - The J2000.0 source unit vector. (unitless)
!
!     Common blocks used:
!
      INCLUDE 'cphys11.i'
!            Variables 'from':
!              1. VLIGHT - The vacuum velocity of light. (m/sec)
!
      INCLUDE 'ccon.i'
!         Variables 'from':
!           1. KETDC  -  The module flow control flag.
!                        = 0 => Default, IERS 1996 Earth Tide Model
!                        = 1 => Earth tide model OFF, no corrections computed
!           2. KETDD  -  The debug output flag.
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. DETDC(2) - The total Earth tide contribution to the delay and
!                        rate. 
!
!   Program specifications:
      Real*8 DOTP, BASCOR(3,2), TIDEP(3,2), TIDEV(3,2),          STAR(3)
      Integer*4 K
!
!     Database access: => Moved to PUT_C.
!
!     Subroutine Interface:
!             Caller subroutines: DRIVC
!             Called subroutines: DOTP, VECSB
!
!     Constants used: VLIGHT
!
!     Program variables:
!         1. BASCOR(3,2) - The correction to the J2000 baseline position and
!                          velocity vectors due to Earth tidal effects.
!                          (M, M/SEC)
!
! 6.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
!                    PETER DENATALE 07/12/77
!                    Jim Ryan 89.06.29 Character strings and clean up.
!                    Jim Ryan 89:10:05 CPHYS common made an include file.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                    implimented.
!                    David Gordon 94.01.24 Third order tide remover
!                                 contribution added.
!                    David Gordon 94.04.06 Changed to 'Implicit None'. Vertical
!                                 Love # dependent terms added to IERS 'excess'
!                                 correction term and that Lcode renamed
!                                 'ETD IERS'.
!                    David Gordon 94.04.27 New common block 'tide_stuff'.
!                    David Gordon 94.04.28 Change due to Love # difference added
!                                 to 'ETD IERS'. Computations and PUT4's for
!                                 'ETD3CONT', 'ETDKCONT', and 'ETD2CONT' added.
!                    David Gordon 94.08.25 IERS made default. delta_IERS,
!                                 dtide_IERS, and other unused variables
!                                 removed. Documentation expanded and corrected.
!                    David Gordon 98.06.24 Obsolete code for Calc 8.x
!                                 contributions removed.
!                    David Gordon 98.01.06 Added PUT's of corrections to use
!                                 old Calc 8.x Earth tide ('C82ETCON') and
!                                 correction to change anelastic case to
!                                 elastic case ('ELASTCON'); skipped if
!                                 Correlator.
!                    David Gordon 99.10.15 Added computation and PUT of
!                                 permanent deformation delay and rate
!                                 contributions, access code 'PERMDEF '.
!                    Jim Ryan     2002.09 Integer*4 conversion
!                    David Gordon 2004 Removed Calc 8 tide, permanent tide,
!                                 and elasticity contributions.
!                    David Gordon Jan. 2013  Moved PUT to subroutine PUT_C.
!
!  ETDC Program Structure.
!
! Compute the contributions.
!
!  Compute the corrections to the J2000.0 baseline position and
!  velocity vectors due to Earth tidal effects.
      CALL VECSB ( TIDEP(1,1), TIDEP(1,2), BASCOR(1,1) )
      CALL VECSB ( TIDEV(1,1), TIDEV(1,2), BASCOR(1,2) )
!
!  Complete the calculation of the contributions.
      DO K = 1,2
        DETDC(K) = DOTP ( BASCOR(1,K), STAR ) / VLIGHT
      ENDDO
!
!  Check KETDD for debug output.
      IF ( KETDD .ne. 0 ) Then        ! Debug output
       WRITE ( 6, 9100 )
 9100  FORMAT (1X, "Debug output for subroutine ETDC." )
    8  FORMAT(A,4D25.16/(7X,5D25.16))
       WRITE(6,8)' BASCOR',BASCOR
       WRITE(6,8)' DETDC ',DETDC
       WRITE(6,8)' VLIGHT',VLIGHT
       WRITE ( 6, 9200 )  TIDEP, TIDEV, STAR
 9200  FORMAT (1X, "TIDEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &             "TIDEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &             "STAR   = ",     3 ( D30.16, 10X ) )
      ENDIF                    ! Debug output
!
!     Normal conclusion.
      RETURN
      END
