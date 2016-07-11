      SUBROUTINE M2K (RPN2K, RS2K, RW2K, TSKIP, R2K)
      IMPLICIT None
!
! 1.1.1 M2K calculates the complete crust fixed to J2000.0 rotation matrix and
!       its first two CT time derivatives using the new IERS2000 CEO-based
!       transformations.
!
! 1.1.3 REFERENCES - IERS Conventions (2003)
!
! 1.2   M2K PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             2. RPN2K(3,3,2)- The Precession/NUTATION PORTION OF
!                              The COMPLETE CRUST FIXED
!                              TO J2000.0 ROTATION MATRIX AND THE CT TIME
!                              DERIVATIVE OF THAT MATRIX. (UNITLESS, 1/SEC)
!             4. RS2K(3,3,3) - The Earth rotation PORTION OF THE COMPLETE CRUST
!                              FIXED TO J2000.0 ROTATION MATRIX AND THE FIRST
!                              TWO CT TIME DERIVATIVES OF THAT MATRIX.
!                              (UNITLESS, 1/SEC, 1/SEC**2)
!             5. RW2K(3,3,2) - THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED TO
!                              J2000.0 ROTATION MATRIX and its first time
!                              derivative. (unitless, 1/sec)
!             6. TSKIP       - Repeat time indicator. If TSKIP = 1, skip
!                              computations becasue they will be identical to
!                              the previous observation (same time).
!
!           OUTPUT VARIABLES:
!             1. R2K(3,3,3)  -  THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                               MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                               (UNITLESS, 1/SEC, 1/SEC**2)
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KM20C  -  THE M2000 UTILITY ROUTINE FLOW CONTROL FLAG.
!              2. KM20D  -  THE M2000 UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
      INCLUDE 'put2s.i'
!       Variables from:
!        1. R2Kput(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                            MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                            (UNITLESS, 1/SEC, 1/SEC**2)
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 R2K(3,3,3), RPN2K(3,3,2), RS2K(3,3,3), RW2K(3,3,2)
      Real*8 dR2K1(3,3), dR2K2(3,3), dR2K3(3,3), dR2K4(3,3),            &
     &       ddR2K1(3,3), ddR2K2(3,3), ddR2K3(3,3), ddR2K4(3,3),        &
     &       ddR2K11(3,3), ddR2K12(3,3), ddR2K13(3,3)
      Integer*4 TSKIP, I, J, K
!
! 1.2.4 DATA BASE ACCESS -
!         'PUT Variables:
!           1. R2K(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                             MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                             (UNITLESS, 1/SEC, 1/SEC**2)
!          ACCESS CODES:
!            2. 'CF2J2K'  -  The data base access code for the complete crust
!                              fixed to J2000 rotation matrix and its first two
!                              time derivatives.
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: MADD3, MADD4, MADD5, MMUL5, PUT4
!
! 1.2.7 CONSTANTS USED - NONE
!
! 1.2.8 PROGRAM VARIABLES -
!          1. R20001(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (dP/dt) (N) (S) (DNP) (W).  (1/SEC)
!          2. R20002(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (dN/dt) (S) (DNP) (W).  (1/SEC)
!          3. R20003(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (dS/dt) (DNP) (W).  (1/SEC)
!          4. R20004(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (S) (DNP) (dW/dt).  (1/SEC)
!          5. R20005(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (dP/dt) (N) (dS/dt) (DNP) (W). (1/SEC**2)
!          6. R20006(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (dN/dt) (dS/dt) (DNP) (W). (1/SEC**2)
!          7. R20007(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (d2S/dt2) (DNP) (W).  (1/SEC**2)
!          8. R20008(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (dS/dt) (DNP) (dW/dt).  (1/SEC**2)
!
! 1.2.9 PROGRAMMER -
!             David Gordon 2003.05.14
!             David Gordon Dec. 2012 PUT's moved to PUT_G
!
      IF (TSKIP .eq. 1) Go To 250
!
!   Compute the complete crust fixed to CEO based J2000.0 rotation matrix.
      CALL MMUL3 (RPN2K(1,1,1), RS2K(1,1,1), RW2K(1,1,1), R2K(1,1,1))
!
!   Compute the first CT time derivative of the complete crust fixed to J2000.0
!   rotation matrix.
!
!   Compute the three terms necessary for the calculation.
      CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,1), RW2K(1,1,1), dR2K1 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,2), RW2K(1,1,1), dR2K2 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,1), RW2K(1,1,2), dR2K3 )
!   Add the three terms to complete the calculation.
      CALL MADD3 ( dR2K1, dR2K2, dR2K3, R2K(1,1,2 ) )
!
!   Compute the second CT time derivative of the complete crust fixed to J2000.0
!   rotation matrix.
!
!   Compute the four terms; three are used twice.
!    (NOTE: Only the second derivative of RS2K is needed, the other
!     second derivatives are insignificant.)
      CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,2), RW2K(1,1,1), ddR2K1 )
      CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,1), RW2K(1,1,2), ddR2K2 )
       CALL MADD2 (ddR2K1, ddR2K2, ddR2K11)
!
!     CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,2), RW2K(1,1,1), ddR2K1 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,3), RW2K(1,1,1), ddR2K3 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,2), RW2K(1,1,2), ddR2K4 )
       CALL MADD3 (ddR2K1, ddR2K3, ddR2K4, ddR2K12)
!
!     CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,1), RW2K(1,1,2), ddR2K2 )
!     CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,2), RW2K(1,1,2), ddR2K4 )
       CALL MADD2 (ddR2K2, ddR2K4, ddR2K13)
!
!     Complete the second derivative
      CALL MADD3 ( ddR2K11, ddR2K12, ddR2K13, R2K(1,1,3) )
!
 250   CONTINUE
!
      Do I = 1,3
       Do J = 1,3
        Do K = 1,3
         R2Kput(I,J,K) = R2K(I,J,K)
        Enddo
       Enddo
      Enddo
      
!    PUT the Crust-fixed-to-J2000 rotation matrix into the data base. We do
!     it here for convenience, there really isn't a correct place to do it.
!     96APR02  -DG-
!     CALL PUT4 ('CF2J2K        ', R2K, int2(3), int2(3), int2(3))
!
!   Check KM20D for debug output.
      IF ( KM20D .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine M2K. " )
      WRITE(6,8)' dR2K1    ', dR2K1
      WRITE(6,8)' dR2K2    ', dR2K2
      WRITE(6,8)' dR2K3    ', dR2K3
      WRITE(6,8)' ddR2K1   ', ddR2K1
      WRITE(6,8)' ddR2K2   ', ddR2K2
      WRITE(6,8)' ddR2K3   ', ddR2K3
      WRITE(6,8)' ddR2K4   ', ddR2K4
      WRITE(6,8)' ddR2K11  ', ddR2K11
      WRITE(6,8)' ddR2K12  ', ddR2K12
      WRITE(6,8)' ddR2K13  ', ddR2K13
    8 FORMAT(A,3D25.16/(9X,3D25.16))
!
      WRITE ( 6, 9200 )  RPN2K, RS2K, RW2K, R2K
 9200 FORMAT (1X, "RPN2K  = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RS2K   = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RW2K   = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "R2K    = ", 9 ( 3 ( D30.16, 10X ), /, 1X ) )
!
!     Normal conclusion.
  500 RETURN
      END
!*********************************************************************
!
      SUBROUTINE MC2K (RNC2K, RPC2K, RSC2K, RW2K, RFR2K, TSKIP, RC2K)
      IMPLICIT None
!
! 1.1.1 MC2K calculates the classical crust fixed to J2000.0 rotation
!       matrix and its first two CT time derivatives using the IERS2003
!       models.
!
! 1.1.3 REFERENCES - IERS Conventions (2003)
!
! 1.2   MC2K  PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. RNC2K(3,3,2)- THE NUTATION PORTION OF THE COMPLETE CRUST FIXED
!                              TO J2000.0 ROTATION MATRIX AND THE CT TIME
!                              DERIVATIVE OF THAT MATRIX. (UNITLESS, 1/SEC)
!             2. RPC2K(3,3,2)- THE PRECESSION PORTION OF THE COMPLETE CRUST
!                              FIXED TO J2000.0 ROTATION MATRIX AND THE CT TIME
!                              DERIVATIVE OF THAT MATRIX. (UNITLESS, 1/SEC)
!             3. RSC2K(3,3,3)- THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
!                              FIXED TO J2000.0 ROTATION MATRIX AND THE FIRST
!                              TWO CT TIME DERIVATIVES OF THAT MATRIX.
!                              (UNITLESS, 1/SEC, 1/SEC**2)
!             4. RW2K(3,3,2)-  THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED TO
!                              J2000.0 ROTATION MATRIX and its first time
!                              derivative. (unitless, 1/sec)
!             5. TSKIP      -  Repeat time indicator. If TSKIP = 1, skip
!                              computations becasue they will be identical to
!                              the previous observation (same time).
!
!           OUTPUT VARIABLES:
!             1. RFR2K(3,3)   -  The frame bias rotation matrix
!             2. RC2K(3,3,3)  -  THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                                 MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                                 (UNITLESS, 1/SEC, 1/SEC**2)
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KM20C  -  THE M2000 UTILITY ROUTINE FLOW CONTROL FLAG.
!              2. KM20D  -  THE M2000 UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!     VARIABLES 'FROM':
!             1. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 RC2K (3,3,3), R2001(3,3), R2002(3,3), R2003(3,3), &
     &       R2004(3,3), R2005(3,3), R2006(3,3),  RFR2K(3,3), &
     &       RNC2K(3,3,2), RPC2K(3,3,2), RSC2K(3,3,3), RW2K(3,3,2), &
     &       R2007(3,3), R2008(3,3),s2001(3,3), RN1(3,3), RN2(3,3), &
     &       RN3(3,3)
      Integer*4 TSKIP
!
      REAL*8 DEPSBI, DPSIBI, EPS0, DRA0
       REAL*8      DAS2R
       PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )
       PARAMETER ( DEPSBI = -0.0068192D0 * DAS2R  )
       PARAMETER ( DPSIBI = -0.041775D0 * DAS2R  )
       PARAMETER ( EPS0 = 84381.448D0 * DAS2R  )
       PARAMETER ( DRA0   = -0.0146D0 * DAS2R  )
!     PARAMETER ( DEPSBI = -0.0068192D0 * CONVDS )
!     PARAMETER ( DPSIBI = -0.041775D0 * CONVDS )
!     PARAMETER ( EPS0 = 84381.448D0 * CONVDS )
!     PARAMETER ( DRA0   = -0.0146D0 * CONVDS )
      INTEGER*4 IFRAME
       DATA IFRAME /0/
       SAVE IFRAME
!
! 1.2.4 DATA BASE ACCESS - None
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: MADD3, MADD4, MADD5, MMUL5, PUT4
!
! 1.2.7 CONSTANTS USED - DEPSBI, DPSIBI, EPS0, DRA0
!
! 1.2.8 PROGRAM VARIABLES -
!          1. R2001(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (dP/dt) (N) (S) (DNP) (W).  (1/SEC)
!          2. R2002(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (dN/dt) (S) (DNP) (W).  (1/SEC)
!          3. R2003(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (dS/dt) (DNP) (W).  (1/SEC)
!          4. R2004(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (S) (DNP) (dW/dt).  (1/SEC)
!          5. R2005(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (dP/dt) (N) (dS/dt) (DNP) (W). (1/SEC**2)
!          6. R2006(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (dN/dt) (dS/dt) (DNP) (W). (1/SEC**2)
!          7. R2007(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (d2S/dt2) (DNP) (W).  (1/SEC**2)
!          8. R2008(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES
!                             (P) (N) (dS/dt) (DNP) (dW/dt).  (1/SEC**2)
!
! 1.2.9 PROGRAMMER -
!             David Gordon 2003.07.30 Subroutine created
!
!
        IFRAME = IFRAME + 1
       IF (IFRAME .GT. 1) Go to 10
!   Construct the frame bias rotation matrix.
!    (Needs to be done only once per database and saved.)
        CALL ROTAT (DEPSBI, int2(1), RN1)
        CALL ROTAT (-DPSIBI*DSIN(EPS0), int2(2), RN2)
        CALL ROTAT (-DRA0, int2(3), RN3)
        CALL MMUL3 ( RN3, RN2, RN1, RFR2K )
!        Write(6,1031) RFR2K
!1031  Format(1x,' MC2K/Frame Bias Matrix RFR2K:',(3(/,3E25.15)))
!
  10  Continue
!
      IF (TSKIP .eq. 1) Go To 250
!
!   Compute the complete crust fixed to J2000.0 rotation matrix.
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,1), RSC2K(1,1,1),     &
     &             RW2K(1,1,1), RC2K(1,1,1) )
!
!   Compute the first CT time derivative of the complete crust fixed to J2000.0
!   rotation matrix.
!
!   Compute the four terms necessary for the calculation.
      CALL MMUL5 ( RFR2K, RPC2K(1,1,2), RNC2K(1,1,1), RSC2K(1,1,1),     &
     &             RW2K(1,1,1), R2001 )
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,2), RSC2K(1,1,1),     &
     &             RW2K(1,1,1), R2002 )
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,1), RSC2K(1,1,2),     &
     &             RW2K(1,1,1), R2003 )
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,1), RSC2K(1,1,1),     &
     &             RW2K(1,1,2), R2004 )
!   Add the four terms to complete the calculation.
      CALL MADD4 ( R2001, R2002, R2003, R2004, RC2K (1,1,2 ) )
!
!   Compute the second CT time derivative of the complete crust fixed to J2000.0
!   rotation matrix.
!
!   Compute the four largest terms.
!    (NOTE: Other terms are needed for the complete calculation but those terms
!     are insignificant for the purposes of CALC.
!
      CALL MMUL5 ( RFR2K, RPC2K(1,1,2), RNC2K(1,1,1), RSC2K(1,1,2),     &
     &             RW2K(1,1,1), R2005 )
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,2), RSC2K(1,1,2),     &
     &             RW2K(1,1,1), R2006 )
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,1), RSC2K(1,1,3),     &
     &             RW2K(1,1,1), R2007 )
      CALL MMUL5 ( RFR2K, RPC2K(1,1,1), RNC2K(1,1,1), RSC2K(1,1,2),     &
     &             RW2K(1,1,2), R2008 )
!
!   Add the terms in the correct way to complete the calculation.
!    (NOTE: The matrices R2005, R2006, and R2008 need to be added twice in
!    the  calculation.)
!
      CALL MADD5 ( R2005, R2005, R2006, R2006, R2007, s2001)
      CALL MADD3 (S2001, R2008, R2008, RC2K (1,1,3))
!
 250   CONTINUE
!
!  The classical rotation matrix is not currently saved.
!     CALL PUT4 ( '              ', RC2K , int2(3), int2(3), int2(3))
!
!   Check KM20D for debug output.
      IF ( KM20D .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine MC2K. " )
      WRITE(6,8)' R2001  ',R2001
      WRITE(6,8)' R2002  ',R2002
      WRITE(6,8)' R2003  ',R2003
      WRITE(6,8)' R2004  ',R2004
      WRITE(6,8)' R2005  ',R2005
      WRITE(6,8)' R2006  ',R2006
      WRITE(6,8)' R2007  ',R2007
      WRITE(6,8)' R2008  ',R2008
      WRITE(6,8)' S2001  ',S2001
    8 FORMAT(A,3D25.16/(9X,3D25.16))
!
      WRITE ( 6, 9200 )  RFR2K, RPC2K, RNC2K, RSC2K, RW2K, RC2K
 9200 FORMAT (1X, "RFR2K  = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RPC2K  = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RNC2K  = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RSC2K  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RW2K   = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "RC2K   = ", 9 ( 3 ( D30.16, 10X ), /, 1X ) )
!
!     Normal conclusion.
  500 RETURN
      END
