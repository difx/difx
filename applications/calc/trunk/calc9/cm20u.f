      SUBROUTINE M2000A
      IMPLICIT None
C
C 1.    M2000A
C
C 1.1   M2000A PROGRAM SPECIFICATION
C
C 1.1.1 M2000A ADDs an entry to the Table of Contents for the
C       Crust fixed to J2000 rotation Matrix array.
C
C 1.2   M2000A PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - NONE
C
C 1.2.2 COMMON BLOCKS USED - NONE
C 
C 1.2.3 PROGRAM SPECIFICATIONS - NONE 
C 
C 1.2.4 DATA BASE ACCESS -
C           ACCESS CODES:
C             1. 'CF2J2000' - The data base access code for the complete crust
C                             fixed to J2000 rotation matrix and its first two 
C                             time derivatives.
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT -  None
C 
C 1.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: TOCUP 
C           CALLED SUBROUTINES: ADDR
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - NONE
C
C 1.2.9 PROGRAMMER - 
C             David Gordon 96.04.02 Created to ADD Lcode 'CF2J2000' to hold the
C                          Crust-fixed-to-J2000 rotation matrix and it first and
C                          second derivatives.
C
C     ADD for Crust-fixed-to-J2000 rotation matrix.
      CALL ADDR (2,'CF2J2000','Crust-fixed-to-J2000 Rot. Matrix',
     1     3, 3, 3 )
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE M2000 ( RDNP, RN, RP, RS, RW, TSKIP, R2000 )
      IMPLICIT None
C
C 1.    M2000
C
C 1.1   M2000 PROGRAM SPECIFICATION
C
C 1.1.1 M2000 calculates the complete crust fixed to J2000.0 rotation matrix and
C       its first two CT time derivatives.
C
C 1.1.2 RESTRICTIONS - NONE
C
C 1.1.3 REFERENCES - ????
C
C 1.2   M2000 PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. RDNP(3,3)  -  THE DIURNAL POLAR MOTION PORTION OF THE COMPLETE
C                              CRUST FIXED TO J2000.0 ROTATION MATRIX.(UNITLESS)
C                               (Unity matrix)
C             2. RN(3,3,2)  -  THE NUTATION PORTION OF THE COMPLETE CRUST FIXED
C                              TO J2000.0 ROTATION MATRIX AND THE CT TIME 
C                              DERIVATIVE OF THAT MATRIX. (UNITLESS, 1/SEC)
C             3. RP(3,3,2)  -  THE PRECESSION PORTION OF THE COMPLETE CRUST 
C                              FIXED TO J2000.0 ROTATION MATRIX AND THE CT TIME
C                              DERIVATIVE OF THAT MATRIX. (UNITLESS, 1/SEC)
C             4. RS(3,3,3)  -  THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
C                              FIXED TO J2000.0 ROTATION MATRIX AND THE FIRST
C                              TWO CT TIME DERIVATIVES OF THAT MATRIX. 
C                              (UNITLESS, 1/SEC, 1/SEC**2) 
C             5. RW(3,3,2)  -  THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED TO
C                              J2000.0 ROTATION MATRIX and its first time
C                              derivative. (unitless, 1/sec)
C             6. TSKIP      -  Repeat time indicator. If TSKIP = 1, skip 
C                              computations becasue they will be identical to 
C                              the previous observation (same time).
C 
C           OUTPUT VARIABLES: 
C             1. R2000(3,3,3)  -  THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
C                                 MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES. 
C                                 (UNITLESS, 1/SEC, 1/SEC**2)
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KM20C  -  THE M2000 UTILITY ROUTINE FLOW CONTROL FLAG.
C              2. KM20D  -  THE M2000 UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C 1.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 R2000(3,3,3), R20001(3,3), R20002(3,3), R20003(3,3), 
     1       R20004(3,3), R20005(3,3), R20006(3,3), RDNP(3,3),
     2       RN(3,3,2), RP(3,3,2), RS(3,3,3), RW(3,3,2), R20007(3,3),
     3       R20008(3,3),s20001(3,3)
      Integer*4 TSKIP
C 
C 1.2.4 DATA BASE ACCESS - 
C         'PUT Variables:
C           1. R2000(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
C                             MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES. 
C                             (UNITLESS, 1/SEC, 1/SEC**2)
C          ACCESS CODES:
C            2. 'CF2J2000'  -  The data base access code for the complete crust
C                              fixed to J2000 rotation matrix and its first two
C                              time derivatives.
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG 
C             CALLED SUBROUTINES: MADD3, MADD4, MADD5, MMUL5, PUT4 
C 
C 1.2.7 CONSTANTS USED - NONE 
C 
C 1.2.8 PROGRAM VARIABLES - 
C          1. R20001(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (dP/dt) (N) (S) (DNP) (W).  (1/SEC)
C          2. R20002(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (P) (dN/dt) (S) (DNP) (W).  (1/SEC)
C          3. R20003(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (P) (N) (dS/dt) (DNP) (W).  (1/SEC)
C          4. R20004(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (P) (N) (S) (DNP) (dW/dt).  (1/SEC)
C          5. R20005(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (dP/dt) (N) (dS/dt) (DNP) (W). (1/SEC**2)
C          6. R20006(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (P) (dN/dt) (dS/dt) (DNP) (W). (1/SEC**2)
C          7. R20007(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (P) (N) (d2S/dt2) (DNP) (W).  (1/SEC**2) 
C          8. R20008(3,3)  -  THE PRODUCT OF THE ROTATION MATRICES 
C                             (P) (N) (dS/dt) (DNP) (dW/dt).  (1/SEC**2) 
C 
C 1.2.9 PROGRAMMER - DALE MARKHAM   01/17/77
C                    PETER DENATALE 07/18/77
C                    SAVITA GOEL    06/04/87 (CDS FOR A900)
C                    Jim Ryan     89.07.25 Documentation simplified.
C                    David Gordon 94.04.14 Converted to Implicit None
C                    David Gordon 95.12.11 Adding in terms for the effect of 
C                                 the time derivative of the wobble rotation
C                                 matrix. RW(3,3) changed to RW(3,3,2). 
C                    David Gordon 96.04.02 Adding PUT4 for 'CF2J2000' L-code,
C                                 the complete crust fixed to J2000 rotation
C                                 matrix and its first two derivatives.
C                    David Gordon 98.07.30 TSKIP added along with logic to skip
C                                 matrix computations if time is same as
C                                 previous observation. 
C
C     M2000 Program Structure
C
      IF (TSKIP .eq. 1) Go To 250
C
C   Compute the complete crust fixed to J2000.0 rotation matrix.
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,1), RDNP, RW(1,1,1),
     1              R2000(1,1,1) )
C
C   Compute the first CT time derivative of the complete crust fixed to J2000.0
C   rotation matrix.
C
C   Compute the four terms necessary for the calculation.
      CALL MMUL5 ( RP(1,1,2), RN(1,1,1), RS(1,1,1), RDNP, RW(1,1,1),
     .           R20001 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,2), RS(1,1,1), RDNP, RW(1,1,1),
     .           R20002 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,2), RDNP, RW(1,1,1),
     .           R20003 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,1), RDNP, RW(1,1,2),
     .           R20004 )
C
C   Add the four terms to complete the calculation.
      CALL MADD4 ( R20001, R20002, R20003, R20004, R2000(1,1,2 ) )
C
C   Compute the second CT time derivative of the complete crust fixed to J2000.0
C   rotation matrix.
C
C   Compute the four largest terms.
C    (NOTE: Other terms are needed for the complete calculation but those terms
C     are insignificant for the purposes of CALC.
      CALL MMUL5 ( RP(1,1,2), RN(1,1,1), RS(1,1,2), RDNP, RW(1,1,1),
     .           R20005 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,2), RS(1,1,2), RDNP, RW(1,1,1),
     .           R20006 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,3), RDNP, RW(1,1,1),
     .           R20007 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,2), RDNP, RW(1,1,2),
     .           R20008 )
C
C   Add the terms in the correct way to complete the calculation.
C    (NOTE: The matrices R20005, R20006, and R20008 need to be added twice in
C    the  calculation.)
      CALL MADD5 ( R20005, R20005, R20006, R20006, R20007, s20001)
      call madd3 (s20001, r20008, r20008, R2000(1,1,3))
C
 250   CONTINUE
C
C    PUT the Crust-fixed-to-J2000 rotation matrix into the data base. We do
C     it here for convenience, there really isn't a correct place to do it.
C     96APR02  -DG-
      CALL PUT4 ('CF2J2000      ',R2000, 3, 3, 3 )
C
C   Check KM20D for debug output.
      IF ( KM20D .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine M2000." )
      WRITE(6,8)' R20001  ',R20001
    8 FORMAT(A,3D25.16/(9X,3D25.16))
      WRITE(6,8)' R20002  ',R20002
      WRITE(6,8)' R20003  ',R20003
      WRITE(6,8)' R20004  ',R20004
      WRITE(6,8)' R20005  ',R20005
      WRITE(6,8)' R20006  ',R20006
      WRITE(6,8)' R20007  ',R20007
      WRITE(6,8)' R20008  ',R20008
      WRITE(6,8)' S20001  ',S20001
C
      WRITE ( 6, 9200 )  RDNP, RN, RP, RS, RW, R2000
 9200 FORMAT (1X, "RDNP   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "RN     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     2            "RP     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     3            "RS     = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "RW     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     5            "R2000  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C     Normal conclusion.
  500 RETURN
      END
