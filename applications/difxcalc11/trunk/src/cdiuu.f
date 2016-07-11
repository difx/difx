      SUBROUTINE DIRNL ( DATDCT, DUT1AT, EPS, FA2K, FAD2K, UT1,         &
     &           XJD, CT, DUTCAT, CENT,                                 &
     &           RPN2K6, S06,                                           &
     &           ERA2K, DERA2K, pERA2K, RS2K, RS2Km1, RS2Kp1,           &
     &           GAST2K, GMST2K, RSC2K)
      IMPLICIT None
!
! 1.    DIRNL
!
! 1.1   DIRNL PROGRAM SPECIFICATION
!
! 1.1.1 DIRNL is the utility routine which computes the diurnal spin portion of
!       the complete crust fixed to J2000.0 rotation matrix and its first two CT
!       time derivatives. DIRNL also computes the diurnal angular velocity of
!       the Earth, the Greenwich Mean Sidereal Time, and the Greenwich Apparent
!       Siderial Time (GAST) and its CT time derivative.
!
! 1.1.2 RESTRICTIONS - NONE
!
! 1.1.3 REFERENCES - 1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN
!                       EPHEMERIS AND NAUTICAL ALMANAC", P.72-76,
!                    2) AOKI, S. ET AL., "THE NEW DEFINITION OF UNIVERSAL
!                       TIME", ASTRON. ASTROPHYS., ????,1980.
!                    3) McCarthy, D., IERS Technical Note 13, Paris 1992
!                    4) McCarthy, D., IERS Technical Note 32, IERS
!                       Conventions (2003).
!
! 1.2   DIRNL PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!        INPUT VARIABLES:
!             1. DATDCT   -  THE PARTIAL DERIVATIVE OF ATOMIC TIME WITH
!                            RESPECT TO COORDINATE TIME. (SEC/SEC)
!             2. DUT1AT   -  THE PARTIAL DERIVATIVE OF UT1 TIME WITH
!                            RESPECT TO ATOMIC TIME. (SEC/SEC)
! Not Used??  3. EPS(2)   -  THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS
!                            CT TIME DERIVATIVE. (RAD, RAD/SEC)
!             4. FA2K(5)  -  The fundamental nutation arguments (see NUTFA)
!             5. FAD2K(5) -  The time derivative of th fundamental
!                            nutation arguments (see NUTFA)
!             6. UT1      -  THE UT1 TIME OF THE DAY. (SEC)
!             7. XJD      -  THE JULIAN DATE AT ZERO HOURS UTC OF THE
!                            DATE IN QUESTION. (DAYS)
!             8. CT       -  Coordinate time, fraction of the day. (days)
!             9. DUTCAT    - Partial derivative of the UTC time with
!                            respect to the atomic time.
!            10. CENT      - Number of Julian centuries elapsed since the
!                            epoch January 1.5, 2000. (centuries)
!            13. RPN2K6(3,3,2)-The Bias Precession Nutation portion of
!                            the complete Fixed to J2000.0 rotation
!                            matrix and its CT time derivative,
!                            consistent with the IERS Conventions
!                            (2010). (unitless, 1/sec)
!            14. S06(2)    - Position of the CEO (Celestial Ephemeris
!                            Origin) on the equator of the CIP, and its
!                            time derivative. (Radians, Radians/sec)
!
!        OUTPUT VARIABLES:
!             3. ERA2K     -  Earth rotation angle (Radians).
!             4. DERA2K    -  Time derivative of Earth rotation angle.
!             5. pERA2K    -  Partial derivative of the Earth rotation
!                             angle (ERA2K) w.r.t. UT1.
!             5. RS2K(3,3,3)- 'New paradigm' diurnal spin rotation
!                             matrix, and its first two CT time
!                             derivatives. (unitless, 1/sec, 1/sec**2)
!             7. GAST2K(2) -  IERS2000 Greenwich apparent sidereal time
!                             and its CT (?) time derivative. (rad, rad/sec)
!             8. GMST2K     - THE GREENWICH MEAN SIDEREAL TIME. (RAD)
!             9. RSC2K(3,3,3)-'Classical' diurnal spin matrix using the
!                             IERS2000 Greenwich apparent sidereal time,
!                             and its first two CT time derivatives.
!                             (unitless, 1/sec, 1/sec**2)
!
      Real*8 DATDCT, DUT1AT, EPS(2), UT1, XJD, CT, DUTCAT, GMST2K(2),   &
     &       ERA2K, DERA2K, RS2K(3,3,3), GAST2K(2), RSC2K(3,3,3),       &
     &       CENT, FA2K(14), FAD2K(14), RPN2K6(3,3,2), S06(2) 
!
! 1.2.2 COMMON BLOCKS USED -
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!            VARIABLES 'FROM':
!              1.  CONVHS  -  THE CONVERSION FACTOR OF RADIANS PER TIME-SECOND.
!                             (RAD/TIME-SEC)
           INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1.  KDIUC  -  THE DIURNAL SPIN UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KDIUD  -  THE DIURNAL SPIN UTILITY ROUTINE DEBUG OUTPUT FLAG.
!              3.  KNUTC  -  THE NUTATION MODULE FLOW CONTROL FLAG. USED TO
!                            DETERMINE WHETHER OR NOT DPSI SHOULD BE ZEROED.
!
      Real*8 DJ2000, DAYSJ, CENJ
      Real*8 UT1f, F, T, ERA, GST2000, EE2000, XJD2, T2, dT2, GST,      &
     &       dGST, EECT2000,               EE2K, dEE2K, EECT2K,         &
     &       dEECT2K, pERA2K, dERA2K1, EQ_ORS, EO, dEO
      Real*8 ERA2Km1, ERA2Kp1, RS2Km1(3,3,3), RS2Kp1(3,3,3)
      Integer*4 I
!
      DATA  DJ2000 /  2451545.D0 /
!
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVR
!             CALLED SUBROUTINES: DDROT, DROTT, ROTAT
!
! 1.2.7 CONSTANTS USED - CONVHS, DJ2000
!
! 1.2.7.1 CONSTANTS INITIALIZED IN THIS UTILITY ROUTINE -
!           2. DJ2000    -  THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000.
!                           (DJ2000 = 2451545.D0 DAYS)
! 1.2.8 PROGRAM VARIABLES -
!
! 1.2.9 PROGRAMMER
!         770207  DALE MARKHAM
!         770718  PETER DENATALE
!         771128  BRUCE SCHUPLER
!         790201  BRUCE SCHUPLER
!         810804  CHOPO MA
!         870604  SAVITA GOEL
!         890726  Jim Ryan:          Documentation simplified.
!         930901  Norbert Zacharias: Equation of equinoxes GAST update
!        2002.09  Jim Ryan  Integer*4 conversion.
!        2003-2004 D.Gordon Updated for IERS Conventions (2003).
!        Oct.2012 D.Gordon  New code for GMST2K.
!        Sept2013 D.Gordon  New code for GAST2K
!
!  DIRNL Program Structure
!
!   Compute the Earth rotation angle, THETA, for use in the new IERS2000
!   tranformations
       DAYSJ = XJD  -  DJ2000
       UT1f = UT1/86400.D0
       T = UT1f + DAYSJ
       F = DMOD(UT1f,1.D0) + DMOD (XJD,1.D0)
       ERA   =  TWOPI * ( F + 0.7790572732640D0                         &
     &          + 0.00273781191135448D0 * T )
       ERA2K = DMOD (ERA,TWOPI)
       IF (ERA2K .lt. 0.D0) ERA2K = ERA2K + TWOPI
       DERA2K = TWOPI*(DUT1AT + 0.00273781191135448D0*DUTCAT)/86400.D0
!
!  Debug
!     Write(6,340) ERA2K, DERA2K
!
!   Construct the Earth rotation angle matrix and its first two CT
!     time derivatives.
!   Construct the Earth rotation matrix.
      CALL ROTAT ( -ERA2K, int2(3), RS2K(1,1,1))
!   Construct the first CT time derivative of the diurnal spin matrix
      CALL DROTT ( -ERA2K, -DERA2K, int2(3), RS2K(1,1,2))
!   Construct the second CT time derivative of the diurnal spin matrix.
      CALL DDROT ( -ERA2K, DERA2K**2, int2(3), RS2K(1,1,3))
!
!   Compute the partials of ERA2K and DERA2K w.r.t. UT1
       pERA2K = TWOPI * 1.00273781191135448D0/86400.D0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   New code for near-field time derivatives:
!    Compute rotation matrices for -1 and +1 second from epoch
!      DAYSJ = XJD  -  DJ2000
!
       UT1f = (UT1-1.D0)/86400.D0
       T = UT1f + DAYSJ
       F = DMOD(UT1f,1.D0) + DMOD (XJD,1.D0)
       ERA = TWOPI * ( F + 0.7790572732640D0                         &
     &     + 0.00273781191135448D0 * T )
       ERA2Km1 = DMOD (ERA,TWOPI)
       IF (ERA2Km1 .lt. 0.D0) ERA2Km1 = ERA2Km1 + TWOPI
      CALL ROTAT ( -ERA2Km1, int2(3), RS2Km1(1,1,1))
      CALL DROTT ( -ERA2Km1, -DERA2K, int2(3), RS2Km1(1,1,2))
      CALL DDROT ( -ERA2Km1, DERA2K**2, int2(3), RS2Km1(1,1,3))
!
       UT1f = (UT1+1.D0)/86400.D0
       T = UT1f + DAYSJ
       F = DMOD(UT1f,1.D0) + DMOD (XJD,1.D0)
       ERA = TWOPI * ( F + 0.7790572732640D0                         &
     &     + 0.00273781191135448D0 * T )
       ERA2Kp1 = DMOD (ERA,TWOPI)
       IF (ERA2Kp1 .lt. 0.D0) ERA2Kp1 = ERA2Kp1 + TWOPI
      CALL ROTAT ( -ERA2Kp1, int2(3), RS2Kp1(1,1,1))
      CALL DROTT ( -ERA2Kp1, -DERA2K, int2(3), RS2Kp1(1,1,2))
      CALL DDROT ( -ERA2Kp1, DERA2K**2, int2(3), RS2Kp1(1,1,3))
!************************************************************************
!
!   Compute the Greenwich Apparent Siderial Time consistent with the
!   IERS 2003 classical transformations.
        T2 = CENT
        dT2 = 1.D0/(36525.D0*86400.D0)
!
!  Greenwich mean siderial time: New code, October 2012.
       GMST2K(1) = ERA2K + ( 0.014506D0 + 4612.15653400D0*T2 +          &
     &        1.39158170D0*T2**2 - 0.00000044D0*T2**3 -                 &
     &        0.000029956D0*T2**4 - .0000000368D0*T2**5 ) * CONVDS 
       GMST2K(2) = dERA2K + ( 4612.15653400D0 +                         &
     &        2.D0*1.39158170D0*T2 - 3.D0*0.00000044D0*T2**2 -          &
     &        4.D0*0.000029956D0*T2**3 - 5.D0*.0000000368D0*T2**4 ) *   &
     &         CONVDS*dT2 
!      WRITE (6,*) ' DIRNL/new GMST2K:  ', GMST2K
!
!  Greenwich apparent siderial time: New code, Sept. 2013.
!    Equation of the origins, from modified SOFA function.
        EO = EQ_ORS( RPN2K6, S06 )
        GAST2K(1) = ERA2K - EO
         dEO = 0.D0  ! need to add this
        GAST2K(2) = dERA2K - dEO
!
!   Construct the IERS2000 classical diurnal spin matrix and its first
!    two CT time derivatives.
!   Construct the diurnal spin matrix.
!!      CALL ROTAT ( -GAST2K(1), int2(3), RSC2K(1,1,1))
!   Construct the first CT time derivative of the diurnal spin matrix
!!      CALL DROTT ( -GAST2K(1), -GAST2K(2), int2(3), RSC2K(1,1,2))
!   Construct the second CT time derivative of the diurnal spin matrix.
!!      CALL DDROT ( -GAST2K(1), GAST2K(2)**2, int2(3), RSC2K(1,1,3))
!
!************************************************************************
!   Check KDIUD for debug output.
      IF ( KDIUD .NE. 0 )  THEN
        WRITE ( 6, 9)
    9   FORMAT (1X, "Debug output for utillity DIRNL." )
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' DJ2000  ',DJ2000
        WRITE(6,8)' CONVHS  ',CONVHS
       write(6,233)  UT1f,F,T
 233   format (' DIURNL/UT1f,F,T: ', 3E30.20)
       write(6,340)  ERA2K,DERA2K
 340   format(' DIRNL/ERA2K,DERA2K: ',F22.18,E30.20)
       WRITE ( 6, * ) 'DIURNL/EE_PRC: ', GAST2K(1) - ERA2K - EE2K
       Write(6,1024) RS2K
 1024  Format(1x,'DIRNL/RS2K  ',(9(/,3E25.15)))
       write(6,341)  EE2K, dEE2K
 341   format(' DIRNL/EE2K,dEE2K: ',E30.20)
       write(6,342)  GAST2K
 342   format(' DIRNL/GAST2K: ',F22.18,E30.20)
       WRITE ( 6, * ) ' DIRNL/GMST2K:  ', GMST2K
       Write(6,1026) RSC2K
 1026  Format(1x,'DIRNL/RSC2K: ',(9(/,3E25.15)))
       WRITE ( 6, * ) 'DIURNL/pERA2K: ', pERA2K
       WRITE ( 6, * ) 'DIURNL/T2: ', T2
       WRITE ( 6, * ) 'DIURNL/DAYSJ: ', DAYSJ
!
        WRITE ( 6, 9200 )  DATDCT,         DUT1AT,       UT1, XJD,      &
     &                     GAST2K, GMST2K, RS2K
 9200   FORMAT (1X, "DATDCT = ", D30.16, /, 1X,                         &
     &            "DUT1AT = ", D30.16, /, 1X,                           &
     &            "UT1    = ", D30.16, /, 1X,                           &
     &            "XJD    = ", D30.16, /, 1X,                           &
     &            "GAST2K = ", 2 ( D30.16, 10X ), /, 1X,                &
     &            "GMST2K = ", D30.16, /, 1X,                           &
     &            "RS2K   = ", 9 ( 3 ( D30.16, 10X ), /, 1X ) )
!
      ENDIF   ! debug output flag
!
      Return
      END
!*************************************************************************
      SUBROUTINE EECT (T, FA2K, FAD2K, EECT2K, dEECT2K)
      IMPLICIT NONE
!
!  Equation of the equinoxes complementary terms, consistent with
!  IAU 2000 resolutions.
!  Modified form of SOFA subroutine EECT2000.
!
!  Given:
!     XJD, CT       d    TT date (JD = DATE1+DATE2)
!
!  Returned:
!     EECT2K        d    Complementary terms (radians)
!     dEECT2K       d    Time derivative of complementary terms
!                         (radians/sec)
!
!-----------------------------------------------------------------------
!
      REAL*8 EECT2K, dEECT2K
!  2Pi
      REAL*8           D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )
!  Arcseconds to radians
      REAL*8           DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )
!  Reference epoch (J2000), JD
      REAL*8           DJ0
      PARAMETER ( DJ0 = 2451545D0 )
!  Days per Julian century
      REAL*8           DJC
      PARAMETER ( DJC = 36525D0 )
!  Time since J2000, in Julian centuries
      REAL*8           T
!  Miscellaneous
      INTEGER I, J
      REAL*8           A, S0, S1, dA, dS0, dS1
      REAL*8           ANMP
!
!  Fundamental arguments
!     DOUBLE PRECISION FA(14)
      REAL*8 FA2K(14), FAD2K(14)
!     COMMON / NFA2K / FA2K, FAD2K
!
!  -----------------------------------------
!  The series for the EE complementary terms
!  -----------------------------------------
!
!  Number of terms in the series
      INTEGER NE0, NE1
      PARAMETER ( NE0=  33, NE1=  1 )
!
!  Coefficients of l,l',F,D,Om,LMe,LVe,LE,LMa,LJu,LSa,LU,LN,pA
      INTEGER KE0 ( 14, NE0 ),                                          &
     &        KE1 ( 14, NE1 )
!
!  Sine and cosine coefficients
      REAL*8           SE0 ( 2, NE0 ),                                  &
     &                 SE1 ( 2, NE1 )
!
!  Argument coefficients for t^0
      DATA ( ( KE0(I,J), I=1,14), J =    1,   10 ) /                    &
     &  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,14), J =   11,   20 ) / &
     &  1,  0,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  4, -4,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  1, -1,  1,  0, -8, 12,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,14), J =   21,   30 ) / &
     &  0,  0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1, -2,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1, -2,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  0,  0,  0,  0,  8,-13,  0,  0,  0,  0,  0, -1,          &
     &  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  2,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0,  0, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  1,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0,  0, -2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  0,  0,  4, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,14), J =   31,  NE0 ) / &
     &  0,  0,  2, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0, -2,  0, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0,          &
     &  1,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
!
!  Argument coefficients for t^1
      DATA ( ( KE1(I,J), I=1,14), J =    1,  NE1 ) /                    &
     &  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
!
!  Sine and cosine coefficients for t^0
      DATA ( ( SE0(I,J), I=1,2), J =    1,   10 ) /                     &
     &            +2640.96D-6,          -0.39D-6,                       &
     &              +63.52D-6,          -0.02D-6,                       &
     &              +11.75D-6,          +0.01D-6,                       &
     &              +11.21D-6,          +0.01D-6,                       &
     &               -4.55D-6,          +0.00D-6,                       &
     &               +2.02D-6,          +0.00D-6,                       &
     &               +1.98D-6,          +0.00D-6,                       &
     &               -1.72D-6,          +0.00D-6,                       &
     &               -1.41D-6,          -0.01D-6,                       &
     &               -1.26D-6,          -0.01D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   11,   20 ) /                     &
     &               -0.63D-6,          +0.00D-6,                       &
     &               -0.63D-6,          +0.00D-6,                       &
     &               +0.46D-6,          +0.00D-6,                       &
     &               +0.45D-6,          +0.00D-6,                       &
     &               +0.36D-6,          +0.00D-6,                       &
     &               -0.24D-6,          -0.12D-6,                       &
     &               +0.32D-6,          +0.00D-6,                       &
     &               +0.28D-6,          +0.00D-6,                       &
     &               +0.27D-6,          +0.00D-6,                       &
     &               +0.26D-6,          +0.00D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   21,   30 ) /                     &
     &               -0.21D-6,          +0.00D-6,                       &
     &               +0.19D-6,          +0.00D-6,                       &
     &               +0.18D-6,          +0.00D-6,                       &
     &               -0.10D-6,          +0.05D-6,                       &
     &               +0.15D-6,          +0.00D-6,                       &
     &               -0.14D-6,          +0.00D-6,                       &
     &               +0.14D-6,          +0.00D-6,                       &
     &               -0.14D-6,          +0.00D-6,                       &
     &               +0.14D-6,          +0.00D-6,                       &
     &               +0.13D-6,          +0.00D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   31,  NE0 ) /                     &
     &               -0.11D-6,          +0.00D-6,                       &
     &               +0.11D-6,          +0.00D-6,                       &
     &               +0.11D-6,          +0.00D-6 /
!
!  Sine and cosine coefficients for t^1
      DATA ( ( SE1(I,J), I=1,2), J =    1,  NE1 ) /                     &
     &               -0.87D-6,          +0.00D-6 /
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Interval between fundamental epoch J2000.0 and current date (JC).
!     T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC
!         print *,' EECT2K/T= ', T
!
!
!  Evaluate the EE complementary terms.
      S0 = 0D0
      S1 = 0D0
      dS0 = 0D0
      dS1 = 0D0
!
      DO I = NE0,1,-1
        A = 0.D0
        dA = 0.D0
         DO J=1,14
            A = A + DBLE(KE0(J,I))*FA2K(J)
            dA = dA + DBLE(KE0(J,I))*FAD2K(J)
         END DO
        S0 = S0 + ( SE0(1,I)*SIN(A) + SE0(2,I)*COS(A) )
        dS0 = dS0 + ( SE0(1,I)*COS(A)*dA - SE0(2,I)*SIN(A)*dA )
      END DO
!      print *,' EECT/S0,dS0 ', S0,dS0
!
      DO I = NE1,1,-1
         A = 0.D0
         dA = 0.D0
         DO J=1,14
            A = A + DBLE(KE1(J,I))*FA2K(J)
            dA = dA + DBLE(KE1(J,I))*FAD2K(J)
         END DO
         S1 = S1 + ( SE1(1,I)*SIN(A) + SE1(2,I)*COS(A) )
         dS1 = dS1 + ( SE1(1,I)*COS(A)*dA - SE1(2,I)*SIN(A)*dA )
      END DO
!      print *,' EECT/S1,dS1 ', S1,dS1
!
      EECT2K = ( S0 + S1 * T ) * DAS2R
      dEECT2K = ( dS0 + dS1*T/(36525.D0*86400D0) ) * DAS2R
!      print *,' EECT/EECT2K,dEECT2K ', EECT2K,dEECT2K
!
      RETURN
      END
!**********************************************************************
      DOUBLE PRECISION FUNCTION EQ_ORS ( RNPB, S )
!+
!  - - - - - - - - -
!   Modified version of 'iau_EORS' from SOFA library:
!       -Converted to Fortran 90.
!       -Modifed for Calc 11 inputs.
!       -Renamed Function EQ_ORS.
!  - - - - - - - - -
!
!  Equation of the origins, given the classical NPB matrix and the
!  quantity s.
!
!  This routine is part of the International Astronomical Union's
!  SOFA (Standards of Fundamental Astronomy) software collection.
!
!  Status:  support routine.
!
!  Given:
!     RNPB    d(3,3,2)   classical nutation x precession x bias matrix
!     S       d(2)       the quantity s (the CIO locator)
!
!  Returned:
!     EQ_ORS  d          the equation of the origins in radians.
!
!  Notes:
!
!  1)  The equation of the origins is the distance between the true
!      equinox and the celestial intermediate origin and, equivalently,
!      the difference between Earth rotation angle and Greenwich
!      apparent sidereal time (ERA-GST).  It comprises the precession
!      (since J2000.0) in right ascension plus the equation of the
!      equinoxes (including the small correction terms).
!
!  2)  The algorithm is from Wallace & Capitaine (2006).
!
!  References:
!
!     Capitaine, N. & Wallace, P.T., 2006, Astron.Astrophys. 450, 855
!
!     Wallace, P. & Capitaine, N., 2006, Astron.Astrophys. 459, 981
!
!  This revision:  2008 February 24
!
!  SOFA release 2012-03-01
!
!  Copyright (C) 2012 IAU SOFA Board.  See notes at end.
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
      DOUBLE PRECISION RNPB(3,3,2), S(2)
!
      DOUBLE PRECISION X, AX, XS, YS, ZS, P, Q
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!  Evaluate Wallace & Capitaine (2006) expression (16).
      X = RNPB(3,1,1)
      AX = X / ( 1D0 + RNPB(3,3,1) )
      XS = 1D0 - AX*X
      YS = -AX*RNPB(3,2,1)
      ZS = -X
      P = RNPB(1,1,1)*XS + RNPB(1,2,1)*YS + RNPB(1,3,1)*ZS
      Q = RNPB(2,1,1)*XS + RNPB(2,2,1)*YS + RNPB(2,3,1)*ZS
      IF ( P.NE.0D0 .OR. Q.NE.0D0 ) THEN
         EQ_ORS = S(1) - ATAN2 ( Q, P )
      ELSE
         EQ_ORS = S(1)
      END IF
!
!  Finished.
!
!+----------------------------------------------------------------------
!
!  Copyright (C) 2012
!  Standards Of Fundamental Astronomy Board
!  of the International Astronomical Union.
!
!  =====================
!  SOFA Software License
!  =====================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
!  CONDITIONS WHICH APPLY TO ITS USE.
!
!  1. The Software is owned by the IAU SOFA Board ("SOFA").
!
!  2. Permission is granted to anyone to use the SOFA software for any
!     purpose, including commercial applications, free of charge and
!     without payment of royalties, subject to the conditions and
!     restrictions listed below.
!
!  3. You (the user) may copy and distribute SOFA source code to others,
!     and use and adapt its code and algorithms in your own software,
!     on a world-wide, royalty-free basis.  That portion of your
!     distribution that does not consist of intact and unchanged copies
!     of SOFA source code files is a "derived work" that must comply
!     with the following requirements:
!
!     a) Your work shall be marked or carry a statement that it
!        (i) uses routines and computations derived by you from
!        software provided by SOFA under license to you; and
!        (ii) does not itself constitute software provided by and/or
!        endorsed by SOFA.
!
!     b) The source code of your derived work must contain descriptions
!        of how the derived work is based upon, contains and/or differs
!        from the original SOFA software.
!
!     c) The names of all routines in your derived work shall not
!        include the prefix "iau" or "sofa" or trivial modifications
!        thereof such as changes of case.
!
!     d) The origin of the SOFA components of your derived work must
!        not be misrepresented;  you must not claim that you wrote the
!        original software, nor file a patent application for SOFA
!        software or algorithms embedded in the SOFA software.
!
!     e) These requirements must be reproduced intact in any source
!        distribution and shall apply to anyone to whom you have
!        granted a further right to modify the source code of your
!        derived work.
!
!     Note that, as originally distributed, the SOFA software is
!     intended to be a definitive implementation of the IAU standards,
!     and consequently third-party modifications are discouraged.  All
!     variations, no matter how minor, must be explicitly marked as
!     such, as explained above.
!
!  4. You shall not cause the SOFA software to be brought into
!     disrepute, either by misuse, or use for inappropriate tasks, or
!     by inappropriate modification.
!
!  5. The SOFA software is provided "as is" and SOFA makes no warranty
!     as to its use or performance.   SOFA does not and cannot warrant
!     the performance or results which the user may obtain by using the
!     SOFA software.  SOFA makes no warranties, express or implied, as
!     to non-infringement of third party rights, merchantability, or
!     fitness for any particular purpose.  In no event will SOFA be
!     liable to the user for any consequential, incidental, or special
!     damages, including any lost profits or lost savings, even if a
!     SOFA representative has been advised of such damages, or for any
!     claim by any third party.
!
!  6. The provision of any version of the SOFA software under the terms
!     and conditions specified herein does not imply that future
!     versions will also be made available under the same terms and
!     conditions.
!
!  In any published work or commercial product which uses the SOFA
!  software directly, acknowledgement (see www.iausofa.org) is
!  appreciated.
!
!  Correspondence concerning SOFA software should be addressed as
!  follows:
!
!      By email:  sofa@ukho.gov.uk
!      By post:   IAU SOFA Center
!                 HM Nautical Almanac Office
!                 UK Hydrographic Office
!                 Admiralty Way, Taunton
!                 Somerset, TA1 2DN
!                 United Kingdom
!
!-----------------------------------------------------------------------
!
      END
