      SUBROUTINE NUTG (CENT, FA2K, FAD2K, XJD, TT, TSKIP, EPS,         &
     &           EPSMNR, RPN2K6, X06, Y06, S06) 
!
      IMPLICIT None
!
! 4.    NUTG
!
! 4.1   NUTG PROGRAM SPECIFICATION
!
! 4.1.1 NUTG IS THE NUTATION MODULE GEOMETRY SECTION. IT CALCULATES THE 
!       NUTATION PORTION OF THE COMPLETE CRUST FIXED TO J2000.0 ROTATION 
!       MATRIX AND ITS CT TIME DERIVATIVE. 
!
! 4.1.3 REFERENCES -
!       1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN EPHEMERIS AND
!          NAUTICAL ALMANAC", P. 41-45, 98.
!       2) 'SPHERICAL AND PRACTICAL ASTRONOMY AS APPLIED TO GEODESY', I.
!          MUELLER, 1969, P. 68-75. (NOTE: THE REFERENCE IN MUELLER REFERS
!          TO THE COMPUTATION OF THE NUTATION PORTION OF THE COMPLETE
!          J2000.0 TO CRUST FIXED ROTATION MATRIX. HOWEVER, CALC REQUIRES
!          THE TRANSPOSE OF THIS MATRIX. CARE MUST BE TAKEN WHEN COMPARING
!          THIS REFERENCE TO THE FOLLOWING PROGRAM.)
!       3) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE PRECESSIONAL QUANTITIES
!          BASED ON THE IAU (1976) SYSTEM OF ASTRONOMICAL CONSTANTS, ASTRON.
!          ASTROPHYS. 58, 1-16, 1977.
!       4) IERS Technical Note 32, IERS Conventions (2003).
!       5) IERS Technical Note 36, IERS Conventions (2010).
!
! 4.2   NUTG PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!              1. CENT      -  The number of Julian centuries elapsed since the
!                              epoch January 1.5, 2000. (centuries)
!                              Now uses TT.
!              2. FA2K(14)  -  The fundamental arguments. (arcsec)
!              3. FAD2K(14) -  The CT time derivatives of the fundamental
!                              arguments. (arcsec/century)
!              4. XJD       -  The Julian Date at zero hours UTC of the
!                              observation.
!              6. TSKIP     -  Skip nutation recomputation if TSKIP=1.
!              7. TT        -  Terrestrial time fraction of the day.
!           OUTPUT VARIABLES:
!              1. X06(2)    -  X-component of the CIP (Celestial
!                              Intermediate Pole) in the GCRS (Geocentric
!                              Celestial Reference System), and its time
!                              derivative. (Radians, Radians/sec)
!              2. Y06(2)    -  Y-component of the CIP (Celestial
!                              Intermediate Pole) in the GCRS (Geocentric
!                              Celestial Reference System), and its time
!                              derivative. (Radians, Radians/sec)
!              3. S06(2)    -  Position of the CEO (Celestial Ephemeris
!                              Origin) on the equator of the CIP, and its
!                              time derivative. (Radians, Radians/sec)
!                              TIME DERIVATIVE. (RAD, RAD/SEC)
!              4. RPN2K6(3,3,2)-The Bias Precession Nutation portion of
!                              the complete Fixed to J2000.0 rotation
!                              matrix and its CT time derivative,
!                              consistent with the IERS Conventions
!                              (2003). (unitless, 1/sec)
!
!      Program Variables:
!              1. DEPS2K6(2) - The nutation in obliquity and its TT time
!                              derivative computed from the IAU2006/2000A 
!                              Precession/Nutation model. (rad, rad/sec) 
!              2. DPSI2K6(2) - The nutation in longitude and its TT time
!                              derivative computed from the IAU2006/2000A
!                              Precession/Nutation model. (rad, rad/sec) 
!              3. DEPS2K(2)  - The nutation in obliquity and its TT time
!                              derivative computed from the IAU2000/2000A 
!                              Precession/Nutation model. (rad, rad/sec) 
!              4. DPSI2K(2)  - The nutation in longitude and its TT time
!                              derivative computed from the IAU2000/2000A
!                              Precession/Nutation model. (rad, rad/sec) 
!              5. DPSI(2) -    The nutation in longitude and its TT time
!                              derivative as computed using the IAU1976/1980
!                              Precession/Nutation models (Wahr nutation).
!                              (arcsec, arcsec/sec)
!              6. DEPS(2) -    The nutation in obliquity and its TT time
!                              derivative as computed using the IAU1976/1980
!                              Precession/Nutation models (Wahr nutation).
!                              (arcsec, arcsec/sec)
!
! 4.2.2 COMMON BLOCKS USED -
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!           VARIABLES 'FROM':
!              1. CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER ARCSECOND.
!                            (RAD/ARCSEC)
!              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
!                            COORDINATE TIME DAY.  (SEC/DAY)
!
      Real*8 CENTJ, DJ2000
      COMMON / NUTCM / CENTJ, DJ2000
!            VARIABLES 'FROM':
!              1. CENTJ  -  THE NUMBER OF COORDINATE TIME DAYS PER JULIAN
!                           CENTURY. (DAYS/CENTURY)
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KNUTC - THE NUTATION MODULE FLOW CONTROL FLAG.
!                         KNUTC = 0 TO TURN ON THE IERS 1996 NUTATION MODEL
!                         KNUTC = 1 TO TURN OFF THE NUTATION MODULE
!              2. KNUTD - THE NUTATION MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'put2s.i'
!       Variables to:
!        1. NUT6XYS(3,2) - Variable used to hold the X, Y, and S nutation
!                          values computed using the IAU2000/2006 
!                          Nutation/Precession models, and 'PUT' into the
!                          'NUT06XYS' database access code. 
!        2. NUT2006(2,2) - Variable used to hold the IAU2006/2000A 
!                          classical nutation values (DPSI and DEPS) and 
!                          their time derivatives. (rad, rad/sec)
!        3. NUTWAHR(2,2) - Variable used to hold the Wahr DPSI and DEPS and
!                          their time derivatives. (rad, rad/sec)
!
      INCLUDE 'cuser11.i'
!       Variables from:
!        1. C_mode - Character*6 variable that determines the type of
!                    usage. 
!                     'mark3 ' for regular calc (geodetic databases).
!                     'difx  ' for difx correlator usage (dcalc).
!                     'nusolv' for future nuSolve usage.
!     
!************************************************************************
! 4.2.3 PROGRAM SPECIFICATIONS -
      REAL*8 CENT,xFA(5),xFAD(5),xFUNDARG(28), FA2K(14), FAD2K(14)
      Real*8 EPS(2), EPSMN(2), EPSMNR, DCENT            
      Real*8 XJD, CT, EPSA(2), DPSI(2), DEPS(2)
      Real*8 TT, X06(2), Y06(2), S06(2)
      Real*8 RBPN6(3,3), dRBPN6(3,3), RPN2K6(3,3,2)
      Real*8 DPSI2K(2), DEPS2K(2), DPSI2K6(2), DEPS2K6(2), FJ2, dFJ2
      Integer*4 TSKIP, M, N, II, JJ
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! 4.2.4 DATA BASE ACCESS - None, moved to subroutine PUT_G.
!
! 4.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVR
!             CALLED SUBROUTINES: DROTT, MADD3, MMUL3, ROTAT, NUTW
!
! 4.2.7 CONSTANTS USED - CONVDS, CENTJ, DJ2000, SECDAY
!
! 4.2.8 PROGRAM VARIABLES -
!         1. DCENT     -  THE CT TIME DERIVATIVE OF THE VARIABLE CENT.
!                         (CENTURIES/SEC)
!
! 4.2.9 PROGRAMMER
!        77.01.13  DALE MARKHAM
!        77.07.12  PETER DENATALE
!        78.03.15  BRUCE SCHUPLER
!        79.02.01  BRUCE SCHUPLER
!        79.06.06  BRUCE SCHUPLER
!        81.08.04  CHOPO MA
!        81.08.24  JIM RYAN
!        89.06.30  Jim Ryan: Documetation clean up.
!        89.12.12  Jim Ryan: UNIX-like database interface implemented
!        93.09.01  Norbert Zacharias: put parts of NUTG,NUTW into NUTFA
!                  for equation of equinox update EQE
!        94.04.15  David Gordon: Converted to Implicit None.
!        98.02.03  D. Gordon: Default changed to IERS 1996 Nutation. Call to
!                  Subroutine KSV_1996 (from Tom Herring, modified at USNO)
!                  added. Added PUT for 'NUT 1996'. Other minor mods.
!        98.11.12  D. Gordon: Added code to compute the correction in longitude
!                  for the effect of geodesic nutation, according to Fukushima
!                  (IERS Conventions (1996), page 37).
!       2002.09    J. Ryan: Integer*4 conversion.
!       2003/2004  David Gordon: Mods for IERS Conventions 2003.
!       Aug. 2012  David Gordon: Mods for IERS Conventions 2010.
!       Dec. 2012  David Gordon: Removed IAU2000 Nutation/Precession.
!                  Moved 'PUT' to PUT_G. 
!       2014.01.09 David Gordon: Put back the classical nutation
!                  (psi and epsilon) computation (from Calc10). Added
!                  code to convert it to the IAU2006 Precession 
!                  model values. Also added back the Wahr nutation
!                  computation. These are needed in Solve to get 
!                  classical nutation adjustments with respect to the
!                  old Wahr model. 
!
!  NUTG PROGRAM STRUCTURE
!
!  Compute the CT time derivative of CENT.
      DCENT = 1.D0 / ( CENTJ * SECDAY )
!
!********************************************************************
!  Obtain the CEO-based bias-precession-nutation matrix.
!
      IF(TSKIP.EQ.1) GO TO 665
!
!  Compute the X,Y,S nutation/precession values using the IAU2000A/2006
!   Nutation/Precession models.
      CALL  XY2K6 (XJD,TT,CENT,DCENT,FA2K,FAD2K,X06,Y06)
      CALL  S02K6 (XJD,TT,CENT,DCENT,FA2K,FAD2K,X06,Y06,S06)
!
        NUT6XYS(1,1) = X06(1)
        NUT6XYS(1,2) = X06(2)
        NUT6XYS(2,1) = Y06(1)
        NUT6XYS(2,2) = Y06(2)
        NUT6XYS(3,1) = S06(1)
        NUT6XYS(3,2) = S06(2)
!   Compute the intermediate-to-GCRS rotation matrix
      CALL BPN2K(X06, Y06, S06, RBPN6, dRBPN6 )
      Do II = 1,3
       Do JJ = 1,3
        RPN2K6(II,JJ,1) =  RBPN6(II,JJ)
        RPN2K6(II,JJ,2) = dRBPN6(II,JJ)
       Enddo
      Enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  In order to continue generating the eops file, we need to compute some
!   obsolete nutation quantities. These are needed for geodetic analysis
!   only. Skip for correlator version.
       IF (C_mode .eq. 'difx  ') Go to 665 
!
!   Compute the classical Wahr nutation model values. 
        CALL NUTW (CENT, DPSI, DEPS)
         DPSI(1) = DPSI(1) * CONVDS
         DEPS(1) = DEPS(1) * CONVDS
         DPSI(2) = DPSI(2) * CONVDS
         DEPS(2) = DEPS(2) * CONVDS
        NUTWAHR(1,1) = DPSI(1)
        NUTWAHR(1,2) = DPSI(2)
        NUTWAHR(2,1) = DEPS(1)
        NUTWAHR(2,2) = DEPS(2)
!
! Compute the classical nutation with respect to IAU2006/2000 P/N.
!   First compute IAU2000A nutation:
        CALL NU2KA (CENT,DCENT,XJD,CT,FA2K,FAD2K,DPSI2K,DEPS2K)
!   Then compute corrections for 2006 Precession
!      Factor for secular variation of J2:
        FJ2  = -2.7774D-6 * CENT
        dFJ2 = -2.7774D-6 * DCENT
!      Apply P03 adjustments:
        DPSI2K6(1) = DPSI2K(1) + DPSI2K(1)*(.4697D-6 + FJ2)
        DPSI2K6(2) = DPSI2K(2) + DPSI2K(1)*dFJ2 + DPSI2K(2)*(.4697D-6+FJ2)
        DEPS2K6(1) = DEPS2K(1) + DEPS2K(1)*FJ2
        DEPS2K6(2) = DEPS2K(2) + DEPS2K(1)*dFJ2 + DEPS2K(2)*FJ2
!
        NUT2006(1,1) = DPSI2K6(1)
        NUT2006(1,2) = DPSI2K6(2)
        NUT2006(2,1) = DEPS2K6(1)
        NUT2006(2,2) = DEPS2K6(2)
!
!     WRITE(6,8)'            '                     
!     WRITE(6,8)' CENT,DCENT ', CENT,DCENT
!     WRITE(6,8)' FJ2, dFJ2  ', FJ2, dFJ2 
!     WRITE(6,8)' DPSI(1),DEPS(1) (mas) ', DPSI(1)*1.D3/CONVDS, DEPS(1)*1.D3/CONVDS 
!     WRITE(6,8)' DPSI2K(1),DEPS2K(1)   ', DPSI2K(1)*1.D3/CONVDS, DEPS2K(1)*1.D3/CONVDS 
!     WRITE(6,8)' DPSI2K6(1),DEPS2K6(1) ', DPSI2K6(1)*1.D3/CONVDS,DEPS2K6(1)*1.D3/CONVDS 
!     WRITE(6,8)'            '                     
!     WRITE(6,8)' DPSI(2),DEPS(2) (mas) ', DPSI(2)*1.D3/CONVDS, DEPS(2)*1.D3/CONVDS 
!     WRITE(6,8)' DPSI2K(2),DEPS2K(2)   ', DPSI2K(2)*1.D3/CONVDS, DEPS2K(2)*1.D3/CONVDS 
!     WRITE(6,8)' DPSI2K6(2),DEPS2K6(2) ', DPSI2K6(2)*1.D3/CONVDS,DEPS2K6(2)*1.D3/CONVDS 
!     WRITE(6,8)'            '                     
!     WRITE(6,8)' DPSI(1),DEPS(1) (rad)', DPSI(1), DEPS(1)
!     WRITE(6,8)' DPSI2K(1),DEPS2K(1)  ', DPSI2K(1), DEPS2K(1)
!     WRITE(6,8)'            '                     
!     WRITE(6,8)' DPSI(2),DEPS(2) (rad)', DPSI(2), DEPS(2)
!     WRITE(6,8)' DPSI2K(2),DEPS2K(2)  ', DPSI2K(2), DEPS2K(2)
!
 665   Continue
!
!  Nutation 'PUT' moved to PUT_G.
!
!
!  Check KNUTC to determine if the nutation module is to be turned off.
      IF ( KNUTC .EQ. 1 )  THEN
!   Set the position portion of the nutation matrix to the identity matrix.
       CALL ROTAT ( 0.D0, int2(3), RPN2K6(1,1,1))
!   Set the velocity portion of the nutation matrix to 0.0D0.
       DO 600 N=1,3
         DO 600 M=1,3
          RPN2K6(M,N,2) = 0.0D0
  600  CONTINUE
      ENDIF
!
!   Check KNUTD for debug output.
  700 IF (KNUTD .EQ. 0) GO TO 800
      WRITE (6,9100)
 9100 FORMAT (1X, "Debug output for subroutine NUTG." )
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CENT    ',CENT
      WRITE(6,8)' DCENT   ',DCENT
      WRITE(6,8)' CENTJ   ',CENTJ
      WRITE(6,8)' SECDAY  ',SECDAY
      WRITE(6,8)' CONVDS  ',CONVDS
      WRITE(6,8)' NUT6XYS ',NUT6XYS
      Write(6,1019) RPN2K6
 1019 Format(1x,'RPN2K6, CEO-based Nutation Matrix:',(6(/,3E25.15)))
!
  800 RETURN
      END
!********************************************************************
        SUBROUTINE NUTP (CFBASE, Xn, Yn, Sn, STAR, RPN2K6, RS2K, RW2K,  &
     &                   TSKIP)
        IMPLICIT None
!
! 5.    NUTP
!
! 5.1   NUTP PROGRAM SPECIFICATION
!
! 5.1.1 NUTP IS THE NUTATION MODULE PARTIAL DERIVATIVES SECTION. NUTP COMPUTES
!       THE PARTIAL DERIVATIVES OF THE DELAY AND OF THE DELAY RATE W.R.T. the
!       X and Y precession/nutation at the epoch of the observation.
!
! 5.1.2 RESTRICTIONS - NONE
!
! 5.1.3 REFERENCES - 1) IERS Conventions 2010
!                    2) SOFA library.
!
! 5.2   NUTP PROGRAM INTERFACE
!
! 5.2.1 CALLING SEQUENCE -
!
!          INPUT VARIABLES:
!             1. CFBASE(3) -  THE CRUST FIXED BASELINE VECTOR. (M)
!             3. Xn(2)     -  X-component of the CIP (Celestial
!                             Intermediate Pole) in the GCRS (Geocentric
!                             Celestial Reference System), and its time
!                             derivative. (Radians, Radians/sec)
!             4. Yn(2)     -  Y-component of the CIP (Celestial
!                             Intermediate Pole) in the GCRS (Geocentric
!                             Celestial Reference System), and its time
!                             derivative. (Radians, Radians/sec)
!             5. Sn(2)     -  Position of the CEO (Celestial Ephemeris
!                             Origin) on the equator of the CIP, and its
!                             time derivative. (Radians, Radians/sec)
!            10. STAR(3)   -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!            11. RPN2K6(3,3,2)-THE PRECESSION-NUTATION PORTION OF THE CRUST
!                             FIXED TO J2000.0 ROTATION MATRIX and it first
!                             time derivative. (unitless, 1/sec)
!                             Updated to use 2000/2006 Nut/Prec models.
!            12. RS2K(3,3,3)- THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
!                             FIXED TO J2000.0 ROTATION MATRIX AND ITS FIRST
!                             two CT TIME DERIVATIVEs. (UNITLESS, 1/SEC)
!            13. RW2K(3,3,2)- THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED
!                             TO J2000.0 ROTATION MATRIX and its time
!                             derivative. (unitless, 1/sec)
!            18. TSKIP      - Skip recomputations if TSKIP=1.
!
!          OUTPUT VARIABLES:
!             1. DN   (2,2) - PARTIAL DERIVATIVES OF THE DELAY AND RATE
!                             W.R.T  (SEC/RAD, SEC/SEC/RAD)
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!              1. KNUTC - THE NUTATION MODULE FLOW CONTROL FLAG.
!              2. KNUTD - THE NUTATION MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'cphys11.i'
!           VARIABLES 'FROM':
!              1.  VLIGHT - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
!
      INCLUDE 'put2s.i'
!       Variables to:
!         1. DNUXY(2,2) - Partial derivatives of the delay and rate
!                             w.r.t. X and Y CEO-based nutation.
!
! 5.2.3 PROGRAM SPECIFICATIONS -
      Real*8 CFBASE(3),         RDNP(3,3), STAR(3),                     &
     &       RPN2K6(3,3,2),RS2K(3,3,3),RW2K(3,3,2),Xn(2),Yn(2),Sn(2)
      Real*8 DOTP
      REAL*8 RBPNX(3,3,2),RBPNY(3,3,2)
      REAL*8 R2KX(3,3,3), R2KY(3,3,3),                                  &
     &       pBL_X(3,2),pBL_Y(3,2)
      REAL*8 dRX1(3,3), dRX2(3,3), dRX3(3,3), ddRX1(3,3), ddRX2(3,3),   &
     &       ddRX3(3,3), ddRX4(3,3), ddRX11(3,3), ddRX12(3,3),          &
     &       ddRX13(3,3), dRY1(3,3), dRY2(3,3), dRY3(3,3), ddRY1(3,3),  &
     &       ddRY2(3,3), ddRY3(3,3), ddRY4(3,3), ddRY11(3,3),           &
     &       ddRY12(3,3), ddRY13(3,3)
      Integer*4 TSKIP, N
       SAVE  R2KX, R2KY
!
! 5.2.4 DATA BASE ACCESS => Moved to PUT_P.
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVR
!             CALLED SUBROUTINES: MMUL3, MADD3, VECRT
!
! 5.2.7 CONSTANTS USED - VLIGHT
!
! 5.2.8 PROGRAM VARIABLES -
!          1. RBPNX(3,3,2)  - Partial derivative of RPN2K6 w.r.t. X nutation.
!          2. RBPNY(3,3,2)  - Partial derivative of RPN2K6 w.r.t. Y nutation.
!          3. R2KX(3,3,3)   - Partial derivative of R2K w.r.t. X nutation.
!          4. R2KY(3,3,3)   - Partial derivative of R2K w.r.t. Y nutation.
!         10. pBL_X(3,2)    - Partial derivatives of the baseline vector
!                             w.r.t. X nutation.
!         11. pBL_Y(3,2)    - Partial derivatives of the baseline vector
!                             w.r.t. Y nutation.
!
! 5.2.9 PROGRAMMER - DOUG ROBERTSON 09/07/82
!                    GEORGE KAPLAN 04/24/84
!                    DAVID GORDON 11/13/84 REDIMENSIONED NUT# PART AND DNUTPT
!                    Jim Ryan     89:10:05 CPHYS common made an include file
!                    Jim Ryan     89.12.12 UNIX-like database interface
!                                 implimented.
!                    David Gordon 94.04.15 Converted to Implicit None.
!                    David Gordon 95.12.11 Changed RW(3,3) to RW(3,3,2).
!                    David Gordon 95.12.12 RS improperly dimensioned as
!                                 (3,3,2). Changed to (3,3,3). RP and RN
!                                 improperly dimensioned as (3,3). Changed to
!                                 (3,3,2).
!                    David Gordon 98.02.03  Removed DNUTPT and computation
!                                 and PUT's of 'NT1 PART', 'NT2 PART', etc.
!                    Jim Ryan 2002.09 Integer*4 conversion.
!                    David Gordon 2003-2004 Mods for 2003 IERS Conventions.
!                    David Gordon Aug. 2012 Mods for 2010 IERS Conventions.
!                    David Gordon Jan. 2013 Moved PUT into subroutine PUT_P.
!
! 5.3   NUTP PROGRAM STRUCTURE
!
!  Start New Code
!
!    Skip site independent computations if same time as previous obs.
       IF(TSKIP.EQ.1) GO TO 150
!
!    Compute partial derivatives of the IERS2000/2006 CEO Bias/Prec/Nut
!     rotation matrix (RPN2K6) w.r.t. X and Y.
      CALL BPN2KP(Xn,Yn,Sn,RBPNX,RBPNY)
!
!    Compute partial derivatives of the IERS2000 CEO-based transformation
!     matrix (R2K) w.r.t. the X and Y nutation quantities.
!
!   Partials of R2K6(1,1,1).
      CALL MMUL3 (RBPNX(1,1,1), RS2K(1,1,1), RW2K(1,1,1), R2KX(1,1,1))
      CALL MMUL3 (RBPNY(1,1,1), RS2K(1,1,1), RW2K(1,1,1), R2KY(1,1,1))
!
!   Partials of first time derivative, R2K6(1,1,2).
      CALL MMUL3 ( RBPNX(1,1,2), RS2K(1,1,1), RW2K(1,1,1), dRX1 )
      CALL MMUL3 ( RBPNX(1,1,1), RS2K(1,1,2), RW2K(1,1,1), dRX2 )
      CALL MMUL3 ( RBPNX(1,1,1), RS2K(1,1,1), RW2K(1,1,2), dRX3 )
      CALL MADD3 ( dRX1, dRX2, dRX3, R2KX(1,1,2 ) )
!
      CALL MMUL3 ( RBPNY(1,1,2), RS2K(1,1,1), RW2K(1,1,1), dRY1 )
      CALL MMUL3 ( RBPNY(1,1,1), RS2K(1,1,2), RW2K(1,1,1), dRY2 )
      CALL MMUL3 ( RBPNY(1,1,1), RS2K(1,1,1), RW2K(1,1,2), dRY3 )
      CALL MADD3 ( dRY1, dRY2, dRY3, R2KY(1,1,2 ) )
!
!!!!!!!!!!!!!!!! Second derivatives not needed !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Partials of second time derivative, R2K6(1,1,3).
!
      CALL MMUL3 ( RBPNX(1,1,2), RS2K(1,1,2), RW2K(1,1,1), ddRX1 )
      CALL MMUL3 ( RBPNX(1,1,2), RS2K(1,1,1), RW2K(1,1,2), ddRX2 )
       CALL MADD2 (ddRX1, ddRX2, ddRX11)
!     CALL MMUL3 ( RBPNX(1,1,2), RS2K(1,1,2), RW2K(1,1,1), ddRX1 )
      CALL MMUL3 ( RBPNX(1,1,1), RS2K(1,1,3), RW2K(1,1,1), ddRX3 )
      CALL MMUL3 ( RBPNX(1,1,1), RS2K(1,1,2), RW2K(1,1,2), ddRX4 )
       CALL MADD3 (ddRX1, ddRX3, ddRX4, ddRX12)
!     CALL MMUL3 ( RBPNX(1,1,2), RS2K(1,1,1), RW2K(1,1,2), ddRX2 )
!     CALL MMUL3 ( RBPNX(1,1,1), RS2K(1,1,2), RW2K(1,1,2), ddRX4 )
       CALL MADD2 (ddRX2, ddRX4, ddRX13)
!     Complete the second derivative
      CALL MADD3 ( ddRX11, ddRX12, ddRX13, R2KX(1,1,3) )
!
      CALL MMUL3 ( RBPNY(1,1,2), RS2K(1,1,2), RW2K(1,1,1), ddRY1 )
      CALL MMUL3 ( RBPNY(1,1,2), RS2K(1,1,1), RW2K(1,1,2), ddRY2 )
       CALL MADD2 (ddRY1, ddRY2, ddRY11)
!     CALL MMUL3 ( RBPNY(1,1,2), RS2K(1,1,2), RW2K(1,1,1), ddRY1 )
      CALL MMUL3 ( RBPNY(1,1,1), RS2K(1,1,3), RW2K(1,1,1), ddRY3 )
      CALL MMUL3 ( RBPNY(1,1,1), RS2K(1,1,2), RW2K(1,1,2), ddRY4 )
       CALL MADD3 (ddRY1, ddRY3, ddRY4, ddRY12)
!     CALL MMUL3 ( RBPNY(1,1,2), RS2K(1,1,1), RW2K(1,1,2), ddRY2 )
!     CALL MMUL3 ( RBPNY(1,1,1), RS2K(1,1,2), RW2K(1,1,2), ddRY4 )
       CALL MADD2 (ddRY2, ddRY4, ddRY13)
!     Complete the second derivative
      CALL MADD3 ( ddRY11, ddRY12, ddRY13, R2KY(1,1,3) )
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 150   Continue
!
!   Compute partials of the baseline vector and its first time
!    derivative with respect to X and Y.
          CALL VECRT(R2KX(1,1,1),CFBASE,pBL_X(1,1))
          CALL VECRT(R2KX(1,1,2),CFBASE,pBL_X(1,2))
          CALL VECRT(R2KY(1,1,1),CFBASE,pBL_Y(1,1))
          CALL VECRT(R2KY(1,1,2),CFBASE,pBL_Y(1,2))
!
!
!   Compute the partials of the delay and delay rate with respect to
!    X and Y.
!     X partials
          DNUXY(1,1) = DOTP(pBL_X(1,1),STAR) / VLIGHT
          DNUXY(1,2) = DOTP(pBL_X(1,2),STAR) / VLIGHT
!     Y partials
          DNUXY(2,1) = DOTP(pBL_Y(1,1),STAR) / VLIGHT
          DNUXY(2,2) = DOTP(pBL_Y(1,2),STAR) / VLIGHT
!
!   End New Code
!
 800      Continue
!
!     CHECK KNUTD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
      If(KNUTD.EQ.0) GO TO 900
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine NUTP." )
       Write(6,1012) RBPNX
 1012 Format(' NUTP/RBPNX, Partials CEO Bias/Prec/Nut:',(6(/,3E25.15)))
       Write(6,1013) RBPNY
 1013 Format(' NUTP/RBPNY, Partials CEO Bias/Prec/Nut:',(6(/,3E25.15)))
       Write(6,1015) R2KX
 1015 Format(' NUTP/R2KX, X Partials R2K:',(6(/,3E25.15)))
       Write(6,1016) R2KY
 1016 Format(' NUTP/R2KY, Y Partials R2K:',(6(/,3E25.15)))
       Write(6,1035) pBL_X
 1035  Format(1x,' NUTP/pBL_X: ',(2(/,3E25.15)))
       Write(6,1036) pBL_Y
 1036  Format(1x,' NUTP/pBL_Y: ',(2(/,3E25.15)))
       Write(6,1039) DNUXY
 1039  Format(1x,' NUTP/DNUXY: ',(2(/,2E25.15)))
!
!   9.    NORMAL PROGRAM CONCLUSION.
!
  900 RETURN
      END
!***********************************************************************
      SUBROUTINE NUTC ()
      IMPLICIT None
!
! 1.    NUTC
!
! 1.1   NUTC PROGRAM SPECIFICATION
!
! 1.1.1 NUTC computes the contributions to convert from the IERS 1996 nutation
!       model back to the old IAU 1980 (Wahr) model, and places them in the
!       data base.
!
! 1.2   NUTC PROGRAM INTERFACE
!
!         Input variables:
!            1. NUTDIF(2,2) - Nutation difference: IAU1980 minus IAU2000A.
!                             First index over psi and epsilon; second
!                             index over difference and derivative of
!                             difference. (radians, radians/sec)
!            2. DNUpe(2,2)  - PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
!                             RATE W.R.T DPSI AND DEPS. (SEC/RAD,
!                             SEC/SEC/RAD)
!
        Real*8 NUTDIF(2,2), DNUpe(2,2)
!
! 1.2.2 COMMON BLOCKS USED
      INCLUDE 'ccon.i'
!
! 1.2.3 PROGRAM SPECIFICATIONS
!
! 1.2.4 DATA BASE ACCESS - NONE
!
! 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
!
! 1.2.6 SUBROUTINE INTERFACE - NONE
!        Caller subroutine: DRIVR
!
! 1.2.7 CONSTANTS USED - NONE
!
! 1.2.8 PROGRAM VARIABLES
!
! 1.2.9 PROGRAMMER -
!         BRUCE SCHUPLER 11/2/77
!         D. Gordon 94.04.15 Converted to Implicit None.
!         D. Gordon 98.02.03  Documentation and code added to compute Wahr
!                   nutation contribution and place it in the data base.
!         D. Gordon 98.11.12  Computation and PUT of the delay and rate
!                   contributions for the Fukushima geodesic nutation in
!                   longitude correction, access code 'GDNUTCON'.
!         Jim Ryan 2002.09 Integer*4 conversion.
!         David Gordon 2003/2004 Mods for IERS Conventions 2003.
!         David Gordon Aug 2012 - Stub - no longer does anything.
!
! 1.3   NUTC PROGRAM STRUCTURE
!
!
      RETURN
      END
!***********************************************************************
      BLOCK DATA NUTCMM
      IMPLICIT None
!
! 7.    NUTBD
!
! 7.1   NUTBD PROGRAM SPECIFICATION
!
! 7.1.1 NUTBD IS THE NUTATION MODULE BLOCK DATA INITIALIZATION SECTION.
!       THE NUTATION SERIES IS ESTABLISHED HERE. THIS VERSION CONTAINS
!       THE 1980 IAU THEORY OF NUTATION, FROM THE WORK OF J. M. WAHR,
!       SPECIFICALLY, THE WAHR NUTATION SERIES FOR AXIS B OF GILBERT &
!       DZIEWONSKI EARTH MODEL 1066A.
!
! 7.1.3 REFERENCES - 1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN
!                    EPHEMERIS AND NAUTICAL ALMANAC", P. 41-45, 98
!
!                    2) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE
!                    PRECESSIONAL QUANTITIES BASED ON THE IAU (1976)
!                    SYSTEM OF ASTRONOMICAL CONSTANTS,
!                    ASTRON. ASTROPHYS. 58, 1-16, 1977.
!
!                    3) SEIDELMANN, P. K., 1980 IAU THEORY OF NUTATION:
!                    THE FINAL REPORT OF THE IAU WORKING GROUP ON
!                    NUTATION, CELEST. MECH. 27, PP. 79-106 (1982).
!
!                    4) WAHR, J. M., THE FORCED NUTATIONS OF ... EARTH,
!                    GEOPHYS. J. ROY. ASTR. SOC. 64, PP. 705-727 (1981).
!
! 7.2   NUTBD PROGRAM INTERFACE
!
! 7.2.1 CALLING SEQUENCE - NONE
!
! 7.2.2 COMMON BLOCK -
!
!**   Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6)
      Real*8 CENTJ, DJ2000
!**   Integer*4 NOT, NOP, IDP(6)
!**   COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
      COMMON / NUTCM / CENTJ, DJ2000
!           VARIABLES 'TO':
!              1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN
!                           CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
!              2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000.
!                           (DAYS) (DJ2000 = 2451545.D0)
!              3. EC(4)   - THE CONSTANTS APPEARING IN TERMS 1-4 IN THE
!                           CALCULATION OF THE MEAN OBLIQUITY OF THE ECLIPTIC.
!                           (ARCSEC) (SEE REFERENCES)
!                           ( EC(1) = +8.4381448D4, EC(2) = -46.815D0,
!                             EC(3) = -5.9D-4, EC(4) = +1.813D-3 )
!              4. NOT     - THE NUMBER OF TERMS IN THE NUTATION SERIES.
!                           (NOT = 106)
!              5. X(9,120)- THE ARRAY CONTAINING THE NUTATION SERIES.
!                           (X = 1980 IAU THEORY OF NUTATION)
!              6. NOP     - THE NUMBER OF NUTATION TERMS DESIGNATED FOR WHICH
!                           PARTIALS ARE TO BE COMPUTED. (NOP = 6)
!                                  (Obsolete?)
!              7. IDP(6)  - IDENTIFICATION NUMBERS (TERM NUMBERS) OF DESIGNATED
!                           NUTATION TERMS FOR WHICH PARTIALS ARE TO BE COMPUTED
!                           ( IDP(1) =  1, IDP(2) =  2, IDP(3) =  3,
!                             IDP(4) =  4, IDP(5) =  5, IDP(6) =  7 )
!              8. ARGP(2,6)-ARGUMENTS (COMBINATIONS OF FUNDAMENTAL ARGUMENTS)
!                           AND THEIR DERIVATIVES OF DESIGNATED NUTATION TERMS
!                           FOR WHICH PARTIALS ARE TO BE COMPUTED.
!                           (COMPUTED IN NUTW. SET TO 0.0D0 HERE)
!                                  (Obsolete?)
!
! 7.2.3 PROGRAM SPECIFICATIONS -
!
      DATA  CENTJ  / 36525.D0 /, &
     &      DJ2000 / 2451545.D0 /
!**  &      EC     / 8.4381448D4, -46.8150D0, -5.9D-4, 1.813D-3 /, &
!**  &      NOT    / 106 /, &
!**  &      NOP    / 6 /, &
!**  &      IDP    / 1, 2, 3, 4, 5, 7 /, &
!**  &      ARGP   / 12 * 0.0D0 /
!***********************************************************************
!
! 7.2.4 CONSTANTS USED - CENTJ, DJ2000, EC(4), NOT, X(9,120),
!                    NOP, IDP(6), ARGP(2,6)
!
! 7.2.5 PROGRAMMER - DALE MARKHAM   01/13/77
!                    PETER DENATALE 07/12/77
!                    BRUCE SCHUPLER 12/22/77
!                    CHOPO MA       08/04/81
!                    GEORGE KAPLAN  04/24/84
!                    David Gordon   94.04.15 Converted to Implicit None.
!                    David Gordon   95.09.27 X(9,120) table changed from Real*4
!                                   to Real*8
!                    David Gordon   98.02.03 Removed X(9,120) from COMMON
!                                   /NUTCM/ and put it into COMMON /XWAHR/,
!                                   and removed it from most subroutines.
!                   David Gordon 2012.April/Oct. Removed Wahr nutation stuff.
!
! 7.3   NUTBD PROGRAM STRUCTURE - NONE
!
      END
!
!***********************************************************************
      SUBROUTINE NUTFA (XJD, TT, CT, CENT, FA2K, FAD2K)
      IMPLICIT NONE
!
!  NUTFA computes the number of Julian centuries since J2000 and the fundamental
!  arguments and derivatives to be used in the nutation series.
!
!  References: D.McCarthy, IERS Technical Note 32, IERS Conventions (2003).
!
!  Calling sequence:
!    input:
!           1. XJD  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
!                      QUESTION. (DAYS)
!           2. CT   -  THE COORDINATE TIME FRACTION OF THE COORDINATE TIME DAY.
!                      (DAYS)
!           3. TT   -  Terrestrial Time (days).
!    output:
!           1. CENT -  The number of Julian centuries elapsed since the epoch
!                      January 1.5 2000.(centuries)
!           4. FA2K(14)- The fundamental arguments for the nutation theory,
!                      updated for the IERS Conventions 2003.  (radians)
!               1 = mean anomaly of the moon
!                 = mean longitude of the moon minus the
!                   mean longitude of the moon's perigee     (l)
!               2 = mean anomaly of the sun
!                 = mean longitude of the sun minus the
!                   mean longitude of the sun's perigee      (l')
!               3 = mean longitude of the moon minus omega   (F)
!               4 = mean elongation of the moon from the sun (D)
!               5 = longitude of the asc.node of the moon's
!                   mean orbit on the ecliptic,
!                   measured from the mean equinox of date   (omega)
!               6-13 = Planetary longitudes, Mercury through Neptune.
!               14  = General accumulated precession in longitude.
!           3. FAD2K(14)- The CT time derivatives of the fundamental arguments.
!                      (radians/second)
!
      REAL*8 XJD, TT, CT, CENT, EL, ELP, F, D, OM, SEC360, CENT2,CENT3, &
     &       CENT4, DAYSJ, ELC2(5), ELPC2(5), FC2(5),                   &
     &       DC2(5), OMC2(5), dTdt
      REAL*8 FA2K(14), FAD2K(14)
!
!**   Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6)
      Real*8 CENTJ, DJ2000
!**   Integer*4 NOT, NOP, IDP(6)
!**   COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
      COMMON / NUTCM / CENTJ, DJ2000
!     Variables from:
!        1. CENTJ   -  The number of coordinate time days per Julian century.
!                      (days/century)
!        2. DJ2000  -  The Julian date of the epoch January 1.5, 2000. (days)
!
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!           VARIABLES 'FROM':
!              1. TWOPI   -  TWOPI
!              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
!                            COORDINATE TIME DAY. (SEC/DAY)
!
      INCLUDE 'ccon.i'
!     Variables from:
!        1. KNUTD  -  nutation module debug flag
!
!     Subroutine interface:
!       Caller subroutines: DRIVR, UT1I
!       Called subroutines: DMOD
!
!     Constants used -
!       ELC(5)   - COEFFICIENTS USED IN THE CALCULATION OF EL
!       ELPC(5)  - COEFFICIENTS USED IN THE CALCULATION OF ELP
!       FC(5)    - COEFFICIENTS USED IN THE CALCULATION OF F
!       DC(5)    - COEFFICIENTS USED IN THE CALCULATION OF D
!       OMC(5)   - COEFFICIENTS USED IN THE CALCULATION OF OM
!
      DATA SEC360 / 1296000.0D0 /       ! arcseconds in one turn
!
!     DATA statements for the revised fundamental arguments.
!     Simons et al., 1994 values
!      -Conform to IERS Conventions (2003)-
      DATA ELC2   / -0.00024470d0,       0.051635d0,  31.8792d0, &
     &          1717915923.2178d0,  485868.249036d0/
      DATA ELPC2  / -0.00001149d0,      +0.000136d0,  -0.5532d0, &
     &           129596581.0481d0,  1287104.793048d0/
      DATA FC2    /  0.00000417d0,      -0.001037d0, -12.7512d0, &
     &          1739527262.8478d0,  335779.526232d0/
      DATA DC2    / -0.00003169d0,       0.006593d0,  -6.3706d0, &
     &          1602961601.2090d0,  1072260.703692d0/
      DATA OMC2   /-0.00005939d0,        0.007702d0,   7.4722d0, &
     &           -6962890.5431d0,   450160.398036d0/
!
!  Programmer:
!    93.09.01  Norbert Zacharias - Fundamental arguments computation put into
!              separate subroutine, taken from old NUTG subroutine.
!    98.01.28  David Gordon - Coefficients and computations modified to conform
!              to IERS Conventions (1996).
!                    Jim Ryan 2002.09 Integer*4 conversion.
!    2003.     David Gordon - Revised for IERS Conventions (2003).
!    2012.     David Gordon - Revised for IERS Conventions (2010). CENT now
!                             computed using TT, terrestrial time.
!
!-------------------------------------------------------------------------------
!  Compute the number of Julian days elapsed since the epoch January 1.5, 2000.
!     DAYSJ = XJD + CT - DJ2000
      DAYSJ = XJD + TT - DJ2000
!  Derivative of CENT w.r.t. time (centuries/sec)
      dTdt = 1.D0/(CENTJ*86400.D0)
!
!  Compute the number of Julian centuries elapsed since the epoch January 1.5,
!   2000.
!     CENT  = ((XJD - DJ2000) + CT) / CENTJ
      CENT  = ((XJD - DJ2000) + TT) / CENTJ
      CENT2 = CENT * CENT
      CENT3 = CENT * CENT2
      CENT4 = CENT2 * CENT2
!
!-----------------------------------------------------------
!
!  Revised computation of the fundamental arguments and derivatives.
!   IERS Conventions (2003)
!
      EL = ELC2(1)*CENT4 + ELC2(2)*CENT3 + ELC2(3)*CENT2                &
     &   + ELC2(4)*CENT  + ELC2(5)
      FA2K(1)  = DMOD( EL, SEC360 )
      FAD2K(1) = 4.D0*ELC2(1)*CENT3 + 3.D0*ELC2(2)*CENT2                &
     &       + 2.D0*ELC2(3)*CENT  +      ELC2(4)
!
      ELP = ELPC2(1)*CENT4 + ELPC2(2)*CENT3 + ELPC2(3)*CENT2            &
     &    + ELPC2(4)*CENT  + ELPC2(5)
      FA2K (2) = DMOD( ELP, SEC360 )
      FAD2K(2) = 4.D0*ELPC2(1)*CENT3 + 3.D0*ELPC2(2)*CENT2              &
     &       + 2.D0*ELPC2(3)*CENT  +      ELPC2(4)
!
      F = FC2(1)*CENT4 + FC2(2)*CENT3 + FC2(3)*CENT2                    &
     &  + FC2(4)*CENT  + FC2(5)
      FA2K (3) = DMOD( F, SEC360 )
      FAD2K(3) = 4.D0*FC2(1)*CENT3 + 3.D0*FC2(2)*CENT2                  &
     &       + 2.D0*FC2(3)*CENT  +      FC2(4)
!
      D = DC2(1)*CENT4 + DC2(2)*CENT3 + DC2(3)*CENT2                    &
     &  + DC2(4)*CENT  + DC2(5)
      FA2K (4) = DMOD( D, SEC360 )
      FAD2K(4) = 4.D0*DC2(1)*CENT3 + 3.D0*DC2(2)*CENT2                  &
     &       + 2.D0*DC2(3)*CENT  +      DC2(4)
!
      OM = OMC2(1)*CENT4 + OMC2(2)*CENT3 + OMC2(3)*CENT2                &
     &   + OMC2(4)*CENT  + OMC2(5)
      FA2K (5) = DMOD( OM, SEC360 )
      FAD2K(5) = 4.D0*OMC2(1)*CENT3 + 3.D0*OMC2(2)*CENT2                &
     &       + 2.D0*OMC2(3)*CENT  +      OMC2(4)
!  Convert to radians and radians/sec:
      FA2K(1)  =  FA2K(1) * CONVDS
      FA2K(2)  =  FA2K(2) * CONVDS
      FA2K(3)  =  FA2K(3) * CONVDS
      FA2K(4)  =  FA2K(4) * CONVDS
      FA2K(5)  =  FA2K(5) * CONVDS
      FAD2K(1)  =  FAD2K(1) * CONVDS/(SECDAY*CENTJ)
      FAD2K(2)  =  FAD2K(2) * CONVDS/(SECDAY*CENTJ)
      FAD2K(3)  =  FAD2K(3) * CONVDS/(SECDAY*CENTJ)
      FAD2K(4)  =  FAD2K(4) * CONVDS/(SECDAY*CENTJ)
      FAD2K(5)  =  FAD2K(5) * CONVDS/(SECDAY*CENTJ)
!
!  Planetary longitudes, Mercury through Neptune (Souchay et al. 1999).
      FA2K( 6) = ( 4.402608842D0 + 2608.7903141574D0 * CENT )
        FA2K(6) = DMOD(FA2K(6),TWOPI)
      FA2K( 7) = ( 3.176146697D0 + 1021.3285546211D0 * CENT )
        FA2K(7) = DMOD(FA2K(7),TWOPI)
      FA2K( 8) = ( 1.753470314D0 +  628.3075849991D0 * CENT )
        FA2K(8) = DMOD(FA2K(8),TWOPI)
      FA2K( 9) = ( 6.203480913D0 +  334.0612426700D0 * CENT )
        FA2K(9) = DMOD(FA2K(9),TWOPI)
      FA2K(10) = ( 0.599546497D0 +   52.9690962641D0 * CENT )
        FA2K(10) = DMOD(FA2K(10),TWOPI)
      FA2K(11) = ( 0.874016757D0 +   21.3299104960D0 * CENT )
        FA2K(11) = DMOD(FA2K(11),TWOPI)
      FA2K(12) = ( 5.481293872D0 +    7.4781598567D0 * CENT )
        FA2K(12) = DMOD(FA2K(12),TWOPI)
      FA2K(13) = ( 5.311886287D0 +    3.8133035638D0 * CENT )
        FA2K(13) = DMOD(FA2K(13),TWOPI)
!  General accumulated precession in longitude.
      FA2K(14) = ( 0.024381750D0*CENT + 0.00000538691D0*CENT2)
!
      FAD2K( 6) = 2608.7903141574D0 * dTdt
      FAD2K( 7) = 1021.3285546211D0 * dTdt
      FAD2K( 8) = 628.3075849991D0 * dTdt
      FAD2K( 9) = 334.0612426700D0 * dTdt
      FAD2K(10) = 52.9690962641D0 * dTdt
      FAD2K(11) = 21.3299104960D0 * dTdt
      FAD2K(12) = 7.4781598567D0 * dTdt
      FAD2K(13) = 3.8133035638D0 * dTdt
      FAD2K(14) = (0.024381750D0 + 2.D0*0.00000538691D0*CENT) * dTdt
!
!  Debug output
      IF (knutd.NE.0) THEN
        WRITE (6,'(1x,a)') 'Debug output for subroutine NUTFA'
       WRITE (6,8) ' CENT,  ', cent
    8         FORMAT(A,4D25.16/(7X,5D25.16))
      Write (6,787) FA2K
 787  Format(' FA2K:',/,3(5E22.15,/))
      Write (6,788) FAD2K
 788  Format(' FAD2K:',/,3(5E22.15,/))
!
!     Write (6,778) FAD(1)*CONVDS/(SECDAY*CENTJ),
!    *   FAD(2)*CONVDS/(SECDAY*CENTJ),FAD(3)*CONVDS/(SECDAY*CENTJ),
!    *   FAD(4)*CONVDS/(SECDAY*CENTJ),FAD(5)*CONVDS/(SECDAY*CENTJ)
!778  Format(' NUTFA: FAD (rad/sec):',/, (5E22.15,/))
      ENDIF
!
      RETURN
      END
!*************************************************************************
      SUBROUTINE BPN2K ( X, Y, S, RBPN, dRBPN )
!
!  CEO-based bias-precession-nutation matrix.
!  Annexe to IERS Conventions 2000, Chapter 5
!
!  Given:
!     X(2),Y(2)        d      CIP coordinates and time derivatives
!     S(2)             d      the quantity s and time derivatives
!                             (radians)
!  Returned:
!     RBPN        d(3,3)   intermediate-to-celestial matrix ("Q")
!
!  Modified SOFA subroutine for use in Calc. Added time derivative
!  of bias-precession-nutation matrix.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION X(2), Y(2), S(2), RBPN(3,3)
      DOUBLE PRECISION X2, Y2, R2, R, Z, A, AXY, RR(3,3), RL(3,3)
      DOUBLE PRECISION dX2, dY2, dR2, dZ, dA, dAXY,dRR(3,3),dRL(3,3), &
     &                 RR1(3,3), RR2(3,3), dRBPN(3,3)
      Integer*2 Int1,Int2,Int3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         Int1 = 1
         Int2 = 2
         Int3 = 3
!
!  Prepare to evaluate expression (10).
      X2 = X(1)*X(1)
       dX2 = 2.D0*X(1)*X(2)
!       print *,' X2,dX2: ', X2,dX2
      Y2 = Y(1)*Y(1)
       dY2 = 2.D0*Y(1)*Y(2)
!       print *,' Y2,dY2: ', Y2,dY2
      R2 = X2 + Y2
       dR2 = dX2 + dY2
!       print *,' R2,dR2: ', R2,dR2
      R = DSQRT (R2)
      Z = DSQRT (1.D0 - R2)
       dZ = -dR2/(2.D0*DSQRT(1D0 - R2))
!       print *,'  Z, dZ: ',  Z, dZ
      A = 1D0 / ( 1.D0 + Z )
       dA = -dZ/(1.D0 + Z)**2
!       print *,'  A, dA: ',  A, dA
      AXY = A*X(1)*Y(1)
       dAXY = dA*X(1)*Y(1) + A*X(2)*Y(1) + A*X(1)*Y(2)
!       print *,' AXY,dAXY: ',  AXY,dAXY
!
!  Right-hand matrix.
      CALL ROTAT(S(1),Int3,RR)
      CALL DROTT(S(1),S(2),Int3,dRR)
!
!  Left-hand matrix.
      RL(1,1) = 1.D0-A*X2
      RL(1,2) = -AXY
      RL(1,3) = X(1)
      RL(2,1) = -AXY
      RL(2,2) = 1.D0-A*Y2
      RL(2,3) = Y(1)
      RL(3,1) = -X(1)
      RL(3,2) = -Y(1)
      RL(3,3) = 1.D0-A*R2
!
      dRL(1,1) = -dA*X2 - A*dX2
      dRL(1,2) = -dAXY
      dRL(1,3) = X(2)
      dRL(2,1) = -dAXY
      dRL(2,2) = -dA*Y2 - A*dY2
      dRL(2,3) = Y(2)
      dRL(3,1) = -X(2)
      dRL(3,2) = -Y(2)
      dRL(3,3) = -dA*R2 - A*dR2
!
!  The result is the product of the two matrices.
      CALL MMUL2 (RL, RR, RBPN)
!  Derivative of RBPN:
      CALL MMUL2 (dRL, RR, RR1)
      CALL MMUL2 (RL, dRR, RR2)
      CALL MADD2 (RR1, RR2, dRBPN)
!      Write(6,1012) RBPN,dRBPN
 1012  Format(1x,'BPN2K/RBPN,dRBPN:',(6(/,3E25.15)))
!
      RETURN
      END
!***************************************************************************
      SUBROUTINE BPN2KP ( X, Y, S, RBPNX, RBPNY )
!
!  CEO-based bias-precession-nutation matrix.
!  Given:
!     X(2),Y(2)        d      CIP coordinates
!     S(2)             d      the quantity s (radians)
!  Returned:
!     RBPN        d(3,3)   intermediate-to-celestial matrix ("Q")
!
!
!  Modified SOFA subroutine for use in Calc. Computes partial derivatives
!  of the bias-precession-nutation matrix and its time derivative w.r.t.
!  X and Y nutation.
!-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION X(2), Y(2), S(2), RBPNX(3,3,2), RBPNY(3,3,2)
      DOUBLE PRECISION X2, Y2, R2, R, Z, A, AXY, RR(3,3), RL(3,3)
      DOUBLE PRECISION dX2, dY2, dR2, dZ, dA, dAXY,dRR(3,3),dRL(3,3), &
     &                 RR1(3,3), RR2(3,3), dRBPN(3,3)
      Double PRECISION p_X2_x, p_dX2_x,p_Y2_y,p_dY2_y,p_R2_x, &
     &       p_R2_y,p_dR2_x,p_dR2_y,p_Z_x,p_Z_y,p_dZ_x,p_dZ_y, &
     &       p_A_x,p_A_y,p_dA_x,p_dA_y,p_AXY_x,p_AXY_y,p_dAXY_x, &
     &       p_dAXY_y, RLX(3,3), RLY(3,3), &
     &       dRLX(3,3), dRLY(3,3)
      Integer*2 Int1,Int2,Int3
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         Int1 = 1
         Int2 = 2
         Int3 = 3
!       print *,' X, Y, S: ',  X, Y, S
!
!  Prepare to evaluate expression (10).
      X2 = X(1)*X(1)
      dX2 = 2.D0*X(1)*X(2)
       p_X2_x = 2.D0*X(1)
       p_dX2_x = 2.D0*X(2)
!       print *,'X2,dX2: ', X2,dX2
!       print *,'p_X2_x,p_dX2_x: ', p_X2_x,p_dX2_x
!
      Y2 = Y(1)*Y(1)
      dY2 = 2.D0*Y(1)*Y(2)
       p_Y2_y = 2.D0*Y(1)
       p_dY2_y = 2.D0*Y(2)
!       print *,'Y2,dY2: ', Y2,dY2
!       print *,'p_Y2_y,p_dY2_y: ', p_Y2_y,p_dY2_y
!
      R2 = X2 + Y2
      dR2 = dX2 + dY2
       p_R2_x = p_X2_x
       p_R2_y = p_Y2_y
       p_dR2_x = p_dX2_x
       p_dR2_y = p_dY2_y
!       print *,'R2,dR2: ', R2,dR2
!       print *,'p_R2_x,p_dR2_x: ', p_R2_x,p_dR2_x
!       print *,'p_R2_y,p_dR2_y: ', p_R2_y,p_dR2_y
!
!     R = DSQRT (R2)
!
      Z = DSQRT (1.D0 - R2)
      dZ = -dR2/(2.D0*DSQRT(1D0 - R2))
       p_Z_x = -X(1)/Z
       p_Z_y = -Y(1)/Z
       p_dZ_x = -p_dX2_x/(2.D0*Z) + dR2*p_Z_x/(2.D0*(Z**2))
       p_dZ_y = -p_dY2_y/(2.D0*Z) + dR2*p_Z_y/(2.D0*(Z**2))
!       print *,'Z,dZ: ', Z,dZ
!       print *,'p_Z_x,p_dZ_x: ', p_Z_x,p_dZ_x
!       print *,'p_Z_y,p_dZ_y: ', p_Z_y,p_dZ_y
!
      A = 1D0 / ( 1.D0 + Z )
      dA = -dZ/(1.D0 + Z)**2
       p_A_x = -p_Z_x/(1.D0 + Z)**2
       p_A_y = -p_Z_y/(1.D0 + Z)**2
       p_dA_x = -p_dZ_x/(1.D0+Z)**2 + 2.D0*dZ*p_Z_x/(1.D0+Z)**3
       p_dA_y = -p_dZ_y/(1.D0+Z)**2 + 2.D0*dZ*p_Z_y/(1.D0+Z)**3
!       print *,'A,dA: ', A,dA
!       print *,'p_A_x,p_dA_x: ', p_A_x,p_dA_x
!       print *,'p_A_y,p_dA_y: ', p_A_y,p_dA_y
!
      AXY = A*X(1)*Y(1)
      dAXY = dA*X(1)*Y(1) + A*X(2)*Y(1) + A*X(1)*Y(2)
       p_AXY_x = p_A_x*X(1)*Y(1) + A*Y(1)
       p_AXY_y = p_A_y*X(1)*Y(1) + A*X(1)
       p_dAXY_x = p_dA_x*X(1)*Y(1) + dA*Y(1) + p_A_x*X(2)*Y(1) + &
     &            p_A_x*X(1)*Y(2) + A*Y(2)
       p_dAXY_y = p_dA_y*X(1)*Y(1) + dA*X(1) + p_A_y*X(2)*Y(1) + &
     &            A*X(2) + p_A_y*X(1)*Y(2)
!       print *,'AXY,dAXY: ', AXY,dAXY
!       print *,'p_AXY_x,p_dAXY_x: ', p_AXY_x,p_dAXY_x
!       print *,'p_AXY_y,p_dAXY_y: ', p_AXY_y,p_dAXY_y
!
!
!  Right-hand matrix.
      CALL ROTAT(S(1),Int3,RR)
      CALL DROTT(S(1),S(2),Int3,dRR)
!
!  Left-hand matrix w.r.t. X.
      RLX(1,1) = -p_A_x*X2 - A*p_X2_x
      RLX(1,2) = -p_AXY_x
      RLX(1,3) = 1.D0
      RLX(2,1) = -p_AXY_x
      RLX(2,2) = -p_A_x*Y2
      RLX(2,3) = 0.D0
      RLX(3,1) = -1.D0
      RLX(3,2) = 0.D0
      RLX(3,3) = -p_A_x*R2 - A*p_R2_x
!
      RLY(1,1) = -p_A_y*X2
      RLY(1,2) = -p_AXY_y
      RLY(1,3) = 0.D0
      RLY(2,1) = -p_AXY_y
      RLY(2,2) = -p_A_y*Y2 - A*p_Y2_y
      RLY(2,3) = 1.D0
      RLY(3,1) =  0.D0
      RLY(3,2) = -1.D0
      RLY(3,3) = -p_A_y*R2 - A*p_R2_y
!
!      dRLX(1,1) = -dA*X2 - A*dX2
      dRLX(1,1) = -p_dA_x*X2 - dA*p_X2_x - p_A_x*dX2 - A*p_dX2_x
      dRLY(1,1) = -p_dA_y*X2             - p_A_y*dX2
!      dRLX(1,2) = -dAXY
      dRLX(1,2) = -p_dAXY_x
      dRLY(1,2) = -p_dAXY_y
!      dRLX(1,3) =  X(2)
      dRLX(1,3) =  0.D0
      dRLY(1,3) =  0.D0
!      dRLX(2,1) = -dAXY
      dRLX(2,1) = -p_dAXY_x
      dRLY(2,1) = -p_dAXY_y
!     dRLX(2,2) = -dA*Y2 - A*dY2
      dRLX(2,2) = -p_dA_x*Y2 - p_A_x*dY2
      dRLY(2,2) = -p_dA_y*Y2 -dA*p_Y2_y - p_A_y*dY2 - A*p_dY2_y
!      dRLX(2,3) =  Y(2)
      dRLX(2,3) =  0.D0
      dRLY(2,3) =  0.D0
!      dRLX(3,1) = -X(2)
      dRLX(3,1) =  0.D0
      dRLY(3,1) =  0.D0
!      dRLX(3,2) = -Y(2)
      dRLX(3,2) =  0.D0
      dRLY(3,2) =  0.D0
!      dRLX(3,3) = -dA*R2 - A*dR2
      dRLX(3,3) = -p_dA_x*R2 - dA*p_R2_x - p_A_x*dR2 - A*p_dR2_x
      dRLY(3,3) = -p_dA_y*R2 - dA*p_R2_y - p_A_y*dR2 - A*p_dR2_y
!
!  The partials are the two matrices.
      CALL MMUL2 (RLX, RR, RBPNX(1,1,1))
      CALL MMUL2 (RLY, RR, RBPNY(1,1,1))
!  The partials of the derivative matrix w.r.t. X
      CALL MMUL2 (dRLX, RR, RR1)
      CALL MMUL2 (RLX, dRR, RR2)
      CALL MADD2 (RR1, RR2, RBPNX(1,1,2))
!  The partials of the derivative matrix w.r.t. Y
      CALL MMUL2 (dRLY, RR, RR1)
      CALL MMUL2 (RLY, dRR, RR2)
      CALL MADD2 (RR1, RR2, RBPNY(1,1,2))
!      Write(6,1012) RBPNdX
!1012  Format(1x,'RBPNdX:',(6(/,3E25.15)))
!      Write(6,1014) RBPNdY
!1014  Format(1x,'RBPNdY:',(6(/,3E25.15)))
      RETURN
      END
