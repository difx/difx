      SUBROUTINE ROTAT ( THETA, I, R )
      IMPLICIT None
!
! 1.    ROTAT
!
! 1.1   ROTAT PROGRAM SPECIFICATION
!
! 1.1.1 ROTAT is the matrix utility routine which creates a matrix 'R' which
!       performs a coordinate system rotation by an angle 'THETA' about
!       coordinate axis 'I' in the counterclockwise direction looking along the
!       axis towards the origin.
!
! 1.1.2 RESTRICTIONS - NONE
!
! 1.1.3 REFERENCES - NONE
!
! 1.2   ROTAT PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. I      -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
!                          (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
!                          THE X, Y, AND Z AXES.)
!             2. THETA  -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM
!                          ROTATION IS PERFORMED. (RAD)
!
!           OUTPUT VARIABLES:
!             1. R(3,3)  -  THE 3x3 ROTATION MATRIX WHICH PERFORMS THE
!                            COORDINATE SYSTEM ROTATION. (UNITLESS)
!
! 1.2.2 COMMON BLOCKS USED -
!
       INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 R(3,3), THETA, C, S
      Integer*2 I
!
! 1.2.4 DATA BASE ACCESS - NONE
!
! 1.2.5 EXTERNAL INPUT/OUTPUT -  NONE
!
!           OUTPUT VARIABLES:
!             1.  POSSIBLE DEBUG OUTPUT
!
! 1.2.6 SUBROUTINE INTERFACE -
!
!             CALLER SUBROUTINES: DIRNL, ETDG, NUTG, PREG, SITI, WOBG
!             CALLED SUBROUTINES: DCOS, DSIN
!
! 1.2.7 CONSTANTS USED - NONE
!
! 1.2.8 PROGRAM VARIABLES -
!             1.  C  -  THE COSINE OF THE ROTATION ANGLE THETA. (UNITLESS)
!             2.  S  -  THE SINE OF THE ROTATION ANGLE THETA. (UNITLESS)
!
! 1.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    SAVITA GOEL    06/04/87 (CDS FOR A900)
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!     ROTAT Program structure
!
!   Compute the cosine and sine of the rotation angle THETA.
      C = DCOS ( THETA )
      S = DSIN ( THETA )
!
!   Go to the right logic depending to the axix of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
!
!   Rotation about the X-axis.
!
!          ( 1  0  0 )
!   R(X) = ( 0  C  S )
!          ( 0 -S  C )
!
  300 R(1,1) = 1.D0
      R(2,1) = 0.D0
      R(3,1) = 0.D0
      R(1,2) = 0.D0
      R(2,2) = +C
      R(3,2) = -S
      R(1,3) = 0.D0
      R(2,3) = +S
      R(3,3) = +C
      GO TO 600
!
!   Rotation about the Y-axis.
!
!          ( C  0 -S )
!   R(Y) = ( 0  1  0 )
!          ( S  0  C )
!
  400 R(1,1) = +C
      R(2,1) = 0.D0
      R(3,1) = +S
      R(1,2) = 0.D0
      R(2,2) = 1.D0
      R(3,2) = 0.D0
      R(1,3) = -S
      R(2,3) = 0.D0
      R(3,3) = +C
      GO TO 600
!
!   Rotation about the Z-axis.
!
!          ( C  S  0 )
!   R(Z) = (-S  C  0 )
!          ( 0  0  1 )
!
  500 R(1,1) = +C
      R(2,1) = -S
      R(3,1) = 0.D0
      R(1,2) = +S
      R(2,2) = +C
      R(3,2) = 0.D0
      R(1,3) = 0.D0
      R(2,3) = 0.D0
      R(3,3) = 1.D0
      GO TO 600
!
!   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility ROTAT." )
      WRITE ( 6, 9200 )  C, S, I, THETA, R
 9200 FORMAT (1X, "C     = ", D30.16, /, 1X, &
     &            'S     = ', D30.16, /, 1X, &
     &            'I     = ', I2, /, 1X, &
     &            'THETA = ', D30.16, /, 1X, &
     &            'R     = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
!    Normal conclusion.
  700 RETURN
      END
!
!******************************************************************************
      SUBROUTINE DROTT ( THETA, DTHETA, I, DR )
      IMPLICIT None
!
! 2.    DROTT
!
! 2.1   DROTT PROGRAM SPECIFICATION
!
! 2.1.1 DROTT is the matrix utility routine which creates the partial derivative
!       of a rotation matrix with respect to a variable on which the rotation
!       matrix is functionally dependent.
!
! 2.1.2 RESTRICTIONS - NONE
!
! 2.1.3 REFERENCES - NONE
!
! 2.2   DROTT PROGRAM INTERFACE
!
! 2.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. DTHETA  -  THE PARTIAL DERIVATIVE OF THE ROTATION ANGLE THETA
!                           WITH RESPECT TO THE VARIABLE IN QUESTION.
!             2. I       -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
!                           (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
!                           THE X, Y, AND Z AXES.)
!             3. THETA   -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM
!                           ROTATION IS PERFORMED. (RAD)
!
!           OUTPUT VARIABLES:
!             1. DR(3,3)  -  THE PARTIAL DERIVATIVE OF THE ROTATION MATRIX IN
!                            QUESTION WITH RESPECT TO THE VARIABLE IN QUESTION.
!
! 2.2.2 COMMON BLOCKS USED -
!
       INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1.  KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 2.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 DR(3,3), THETA, DTHETA, DC, DS
      Integer*2 I
!
! 2.2.4 DATA BASE ACCESS - NONE
!
! 2.2.5 EXTERNAL INPUT/OUTPUT -
!             1.  POSSIBLE DEBUG OUTPUT
!
! 2.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DIRNL, NUTG, PREG, PREP, UT1P, WOBP
!             CALLED SUBROUTINES: DCOS, DSIN
!
! 2.2.7 CONSTANTS USED - NONE
!
! 2.2.8 PROGRAM VARIABLES -
!
!             1. DC  -  THE COSINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY
!                       THE PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
!                       RESPECT TO THE VARIABLE IN QUESTION (DTHETA).
!             2. DS  -  THE SINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY THE
!                       PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH RESPECT TO
!                       THE VARIABLE IN QUESTION (DTHETA).
!
! 2.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!     DROTT Program Structure
!
!   Compute the program variables DC and DS.
      DC = DCOS ( THETA ) * DTHETA
      DS = DSIN ( THETA ) * DTHETA
!
!   Go to the correct logic depending on the axis of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
!
!   Rotation about the X-axis.
!
!          (  0   0   0  )
!  DR(X) = (  0 -DS  DC  )
!          (  0 -DC -DS  )
!
  300 DR(1,1) = 0.D0
      DR(2,1) = 0.D0
      DR(3,1) = 0.D0
      DR(1,2) = 0.D0
      DR(2,2) = -DS
      DR(3,2) = -DC
      DR(1,3) = 0.D0
      DR(2,3) = +DC
      DR(3,3) = -DS
      GO TO 600
!
!   Rotation about the Y-axis.
!
!          (-DS   0 -DC  )
!  DR(Y) = (  0   0   0  )
!          ( DC   0 -DS  )
!
  400 DR(1,1) = -DS
      DR(2,1) = 0.D0
      DR(3,1) = +DC
      DR(1,2) = 0.D0
      DR(2,2) = 0.D0
      DR(3,2) = 0.D0
      DR(1,3) = -DC
      DR(2,3) = 0.D0
      DR(3,3) = -DS
      GO TO 600
!
!   Rotation about the Z-axis.
!
!          (-DS  DC   0  )
!  DR(Z) = (-DC -DS   0  )
!          (  0   0   0  )
!
  500 DR(1,1) = -DS
      DR(2,1) = -DC
      DR(3,1) = 0.D0
      DR(1,2) = +DC
      DR(2,2) = -DS
      DR(3,2) = 0.D0
      DR(1,3) = 0.D0
      DR(2,3) = 0.D0
      DR(3,3) = 0.D0
      GO TO 600
!
!   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility DROTT." )
      WRITE ( 6, 9200 )  DC, DS, DTHETA, I, THETA, DR
 9200 FORMAT (1X, "DC     = ", D30.16, /, 1X, &
     &            'DS     = ', D30.16, /, 1X, &
     &            'DTHETA = ', D30.16, /, 1X, &
     &            'I      = ', I2, /, 1X, &
     &            'THETA  = ', D30.16, /, 1X, &
     &            'DR     = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
!   7.    NORMAL PROGRAM CONCLUSION.
!
  700 RETURN
      END
!
!******************************************************************************
      SUBROUTINE DDROT ( THETA, DDTHTA, I, DDR )
      IMPLICIT None
!
! 3.    DDROT
!
! 3.1   DDROT PROGRAM SPECIFICATION
!
! 3.1.1 DDROT is the matrix utility routine which creates the second
!       partial derivative of a rotation matrix with respect to a
!       variable on which the rotation matrix is functionally dependent.
!
! 3.1.2 RESTRICTIONS - THIS UTILITY ROUTINE ASSUMES THAT THE SECOND PARTIAL
!                      DERIVATIVE OF THE FUNCTIONALLY DEPENDENT VARIABLE WITH
!                      RESPECT TO THE PARAMETER IN QUESTION IS EQUAL TO ZERO.
!                      THIS IS A VERY GOOD ASSUMPTION IN ALL CASES WHERE
!                      SUBROUTINE DDROT IS USED IN THIS PROGRAM.
!
! 3.1.3 REFERENCES - NONE
!
! 3.2   DDROT PROGRAM INTERFACE
!
! 3.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. DDTHTA  -  THE SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE
!                           THETA WITH RESPECT TO THE VARIABLE IN QUESTION.
!             2. I       -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
!                           (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
!                           THE X, Y, AND Z AXES. )
!             3. THETA   -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM
!                           ROTATION IS PERFORMED.  (RAD)
!
!           OUTPUT VARIABLES:
!             1. DDR(3,3)  -  THE SECOND PARTIAL DERIVATIVE OF THE ROTATION
!                             MATRIX IN QUESTION WITH RESPECT TO THE VARIABLE IN
!                             QUESTION.
!
! 3.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 3.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  DDR(3,3), THETA, DDTHTA, DDC, DDS
      Integer*2 I
!
! 3.2.4 DATA BASE ACCESS - NONE
!
! 3.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 3.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DIRNL, UT1P
!             CALLED SUBROUTINES: DCOS, DSIN
!
! 3.2.7 CONSTANTS USED - NONE
!
! 3.2.8 PROGRAM VARIABLES -
!             1. DDC  -  THE COSINE OF THE ROTATION ANGLE MULTIPLIED BY THE
!                        SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
!                        RESPECT TO THE VARIABLE IN QUESTION.
!             2. DDS  -  THE SINE OF THE ROTATION ANGLE MULTIPLIED BY THE SECOND
!                        PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH RESPECT
!                        TO THE VARIABLE IN QUESTION.
!
! 3.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   DDROT Program Structure
!
!   Compute the program variables DDC and DDS.
      DDC = DCOS ( THETA ) * DDTHTA
      DDS = DSIN ( THETA ) * DDTHTA
!
!   Go to the correct logic depending of the axis of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
!
!   Rotation ablut the X-axis.
!
!           (   0    0    0   )
!  DDR(X) = (   0 -DDC -DDS   )
!           (   0  DDS -DDC   )
!
  300 DDR(1,1) = 0.D0
      DDR(2,1) = 0.D0
      DDR(3,1) = 0.D0
      DDR(1,2) = 0.D0
      DDR(2,2) = -DDC
      DDR(3,2) = +DDS
      DDR(1,3) = 0.D0
      DDR(2,3) = -DDS
      DDR(3,3) = -DDC
      GO TO 600
!
!   Rotation about the Y-axis.
!
!           (-DDC    0  DDS   )
!  DDR(Y) = (   0    0    0   )
!           (-DDS    0 -DDC   )
!
  400 DDR(1,1) = -DDC
      DDR(2,1) = 0.D0
      DDR(3,1) = -DDS
      DDR(1,2) = 0.D0
      DDR(2,2) = 0.D0
      DDR(3,2) = 0.D0
      DDR(1,3) = +DDS
      DDR(2,3) = 0.D0
      DDR(3,3) = -DDC
      GO TO 600
!
!   Rotation about the Z-axis.
!
!           (-DDC -DDS    0   )
!  DDR(Z) = ( DDS -DDC    0   )
!           (   0    0    0   )
!
  500 DDR(1,1) = -DDC
      DDR(2,1) = +DDS
      DDR(3,1) = 0.D0
      DDR(1,2) = -DDS
      DDR(2,2) = -DDC
      DDR(3,2) = 0.D0
      DDR(1,3) = 0.D0
      DDR(2,3) = 0.D0
      DDR(3,3) = 0.D0
      GO TO 600
!
!   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine DDROT." )
      WRITE ( 6, 9200 )  THETA, I, DDTHTA, DDR
 9200 FORMAT (1X, "THETA  = ", D30.16, /, 1X, &
     &            'I      = ', I2, /, 1X, &
     &            'DDTHTA = ', D30.16, /, 1X, &
     &            'DDR    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
!     Normal Conclusion.
  700 RETURN
      END
!
!*******************************************************************************
      SUBROUTINE D3ROT ( THETA, D3THET, I, D3R )
      IMPLICIT None
!
! 2.    D3ROT
!
! 2.1   D3ROT PROGRAM SPECIFICATION
!
! 2.1.1 D3ROT is the matrix utility routine which creates the third partial
!       derivative of a rotation matrix with respect to a variable on which
!       the rotation matrix is functionally dependent.
!
! 2.1.2 RESTRICTIONS - NONE
!
! 2.1.3 REFERENCES - NONE
!
! 2.2   D3ROT PROGRAM INTERFACE
!
! 2.2.1 CALLING SEQUENCE - CALL D3ROT(THETA,D3THET,I,D3R)
!
!           INPUT VARIABLES:
!             1. D3THET  -  THE THIRD PARTIAL DERIVATIVE OF THE ROTATION ANGLE
!                           THETA WITH RESPECT TO THE VARIABLE IN QUESTION.
!             2. I       -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
!                           (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
!                           THE X, Y, AND Z AXES.)
!             3. THETA   -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM
!                           ROTATION IS PERFORMED. (RAD)
!
!           OUTPUT VARIABLES:
!             1. D3R(3,3) -  THE THIRD PARTIAL DERIVATIVE OF THE ROTATION MATRIX
!                            IN QUESTION WITH RESPECT TO THE VARIABLE IN
!                            QUESTION.
!
! 2.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES "FROM":
!             1.  KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 2.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  D3R(3,3), THETA, D3THET, D3C, D3S
      Integer*2 I
!
! 2.2.4 DATA BASE ACCESS - NONE
!
! 2.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 2.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DIRNL, NUTG, PREG, PREP, UT1P, WOBP
!             CALLED SUBROUTINES: DCOS, DSIN
!
! 2.2.7 CONSTANTS USED - NONE
!
! 2.2.8 PROGRAM VARIABLES -
!             1. D3C -  THE COSINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY
!                       THE SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
!                       RESPECT TO THE VARIABLE IN QUESTION (DTHETA).
!             2. D3S -  THE SINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY THE
!                       SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
!                       RESPECT TO THE VARIABLE IN QUESTION (DTHETA).
!
! 2.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    BRUCE SCHUPLER 03/27/78
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   D3ROT Program Structure
!
!   Compute the program variables D3C and D3S.
      D3C = DCOS ( THETA ) * D3THET
      D3S = DSIN ( THETA ) * D3THET
!
!   Go to the correct logic depending on the axis of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
!
!   Rotation about the X-axis.
!
!          (  0   0   0  )
!  D3R(X) = (  0 +D3S -D3C  )
!          (  0 +D3C +D3S  )
!
  300 D3R(1,1) = 0.D0
      D3R(2,1) = 0.D0
      D3R(3,1) = 0.D0
      D3R(1,2) = 0.D0
      D3R(2,2) = +D3S
      D3R(3,2) = +D3C
      D3R(1,3) = 0.D0
      D3R(2,3) = -D3C
      D3R(3,3) = +D3S
      GO TO 600
!
!   Rotation about the y-axis.
!
!          (+D3S   0 +D3C  )
!  D3R(Y) = (  0   0   0  )
!          (-D3C   0 +D3S  )
!
  400 D3R(1,1) = +D3S
      D3R(2,1) = 0.D0
      D3R(3,1) = -D3C
      D3R(1,2) = 0.D0
      D3R(2,2) = 0.D0
      D3R(3,2) = 0.D0
      D3R(1,3) = +D3C
      D3R(2,3) = 0.D0
      D3R(3,3) = +D3S
      GO TO 600
!
!   Rotation about the Z-axis.
!
!          (+D3S -D3C   0  )
!  D3R(Z) = (+D3C +D3S   0  )
!          (  0   0   0  )
!
  500 D3R(1,1) = +D3S
      D3R(2,1) = +D3C
      D3R(3,1) = 0.D0
      D3R(1,2) = -D3C
      D3R(2,2) = +D3S
      D3R(3,2) = 0.D0
      D3R(1,3) = 0.D0
      D3R(2,3) = 0.D0
      D3R(3,3) = 0.D0
      GO TO 600
!
!   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility D3ROT." )
      WRITE ( 6, 9200 )  D3C, D3S, D3THET, I, THETA, D3R
 9200 FORMAT (1X, "D3C     = ", D30.16, /, 1X, &
     &            'D3S     = ', D30.16, /, 1X, &
     &            'D3THET = ', D30.16, /, 1X, &
     &            'I      = ', I2, /, 1X, &
     &            'THETA  = ', D30.16, /, 1X, &
     &            'D3R     = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
!     Normal conclusion.
  700 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MMUL2 ( A, B, C )
      IMPLICIT None
!
! 4.    MMUL2
!
! 4.1   MMUL2 PROGRAM SPECIFICATION
!
! 4.1.1 MMUL2 is the matrix utility which multiplies two rotation matrices
!       producing a product rotation matrix.
!
! 4.1.2 RESTRICTIONS - NONE
!
! 4.1.3 REFERENCES - NONE
!
! 4.2   MMUL2 PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. C(3,3)  -  THE PRODUCT OF ROTATION MATRICES A AND B.
!
! 4.2.2 COMMON BLOCKS USED -
!
       INCLUDE 'ccon.i'
!
!            VARIABLES 'FROM':
!              1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!              2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES 'TO': NONE
!
! 4.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  A(3,3), B(3,3), C(3,3)
      Integer*4 I, J
!
! 4.2.4 DATA BASE ACCESS - NONE
!
! 4.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 4.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: MMUL3, SITI, WOBG, WOBP
!             CALLED SUBROUTINES: NONE
!
! 4.2.7 CONSTANTS USED - NONE
!
! 4.2.8 PROGRAM VARIABLES - NONE
!
! 4.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   MMUL2 Program Structure
!
!   Perform the multiplication.
      DO 120  J = 1,3
        DO 110  I = 1,3
          C(I,J) =   A(I,1) * B(1,J) &
     &             + A(I,2) * B(2,J) &
     &             + A(I,3) * B(3,J)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility MMUL2." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MMUL3 ( A, B, C, D )
      IMPLICIT None
!
! 5.    MMUL3
!
! 5.1   MMUL3 PROGRAM SPECIFICATION
!
! 5.1.1 MMUL3 is the matrix utility routine which multiplies together
!       three rotation matrices producing a product rotation matrix.
!
! 5.1.2 RESTRICTIONS - NONE
!
! 5.1.3 REFERENCES - NONE
!
! 5.2   MMUL3 PROGRAM INTERFACE
!
! 5.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. D(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, AND C.
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES "FROM":
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 5.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  A(3,3), AB(3,3), B(3,3), C(3,3), D(3,3)
!
! 5.2.4 DATA BASE ACCESS - NONE
!
! 5.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: MMUL5, NUTG, PREG, PREP
!             CALLED SUBROUTINES: MMUL2
!
! 5.2.7 CONSTANTS USED - NONE
!
! 5.2.8 PROGRAM VARIABLES -
!             1. AB(3,3)  -  THE PRODUCT OF ROTATION MATRICES A AND B.
!
! 5.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   MMUL3 Program Structure
!
!   Perform the multiplications.
      CALL MMUL2 ( A, B, AB )
      CALL MMUL2 ( AB, C, D )
!
!   Check KMATD to for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility MMUL3." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!******************************************************************************
      SUBROUTINE MMUL4 ( A, B, C, D, E )
      IMPLICIT None
!
! 6.1.1 MMUL4 is the matrix utility routine which multiplies together
!       four rotation matrices producing a product rotation matrix.
!
! 6.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
!             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
!           OUTPUT VARIABLES:
!             1. E(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, C, AND D.
!
! 6.2.2 COMMON BLOCKS USED -
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 6.2.3 PROGRAM SPECIFICATIONS -
      Real*8 A(3,3), ABC(3,3), B(3,3), C(3,3), D(3,3), E(3,3)
!
! 6.2.8 PROGRAM VARIABLES -
!             1. ABC(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, AND C.
!
! 6.2.9 PROGRAMMER - D. Gordon 2003.12.30
!
!   Perform the multiplications
      CALL MMUL3 ( A, B, C, ABC )
      CALL MMUL2 ( ABC, D, E )
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utililty MMUL4." )
      WRITE ( 6, 9200 )  A, ABC, B, C, D, E
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'ABC  = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'D    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'E    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MMUL5 ( A, B, C, D, E, F )
      IMPLICIT None
!
! 6.    MMUL5
!
! 6.1   MMUL5 PROGRAM SPECIFICATION
!
! 6.1.1 MMUL5 is the matrix utility routine which multiplies together
!       five rotation matrices producing a product rotation matrix.
!
! 6.1.2 RESTRICTIONS - NONE
!
! 6.1.3 REFERENCES - NONE
!
! 6.2   MMUL5 PROGRAM INTERFACE
!
! 6.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
!             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
!             5. E(3,3)  -  THE FIFTH ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. F(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, C, D, AND E.
!
! 6.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 6.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3,3), ABC(3,3), B(3,3), C(3,3), D(3,3), E(3,3), F(3,3)
!
! 6.2.4 DATA BASE ACCESS - NONE
!
! 6.2.5 EXTERNAL INPUT/OUTPUT -
!             1.  POSSIBLE DEBUG OUTPUT
!
! 6.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: M1950, PREP, UT1P, WOBP
!             CALLED SUBROUTINES: MMUL3
!
! 6.2.7 CONSTANTS USED - NONE
!
! 6.2.8 PROGRAM VARIABLES -
!             1. ABC(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, AND C.
!
! 6.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!     MMUL5 Program Structure
!
!   Perform the multiplications
      CALL MMUL3 ( A, B, C, ABC )
      CALL MMUL3 ( ABC, D, E, F )
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utililty MMUL5." )
      WRITE ( 6, 9200 )  A, ABC, B, C, D, E, F
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'ABC  = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'D    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'E    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'F    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MADD2 ( A, B, C )
      IMPLICIT None
!
! 7.    MADD2
!
! 7.1   MADD2 PROGRAM SPECIFICATION
!
! 7.1.1 MADD2 is the matrix utility which adds togeter two matrices.
!
! 7.1.2 RESTRICTIONS - NONE
!
! 7.1.3 REFERENCES - NONE
!
! 7.2   MADD2 PROGRAM INTERFACE
!
! 7.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. C(3,3)  -  THE SUM OF ROTATION MATRICES A AND B.
!
! 7.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1.  KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 7.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  A(3,3), B(3,3), C(3,3)
      Integer*4 I, J
!
! 7.2.4 DATA BASE ACCESS - NONE
!
! 7.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 7.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: NONE
!             CALLED SUBROUTINES: NONE
!
! 7.2.7 CONSTANTS USED - NONE
!
! 7.2.8 PROGRAM VARIABLES - NONE
!
! 7.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   MADD2 Program Structure
!
!   Do the addition.
      DO 120  J = 1,3
        DO 110  I = 1,3
          C(I,J) = A(I,J) + B(I,J)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD2." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MADD3 ( A, B, C, D )
      IMPLICIT None
!
! 8.    MADD3
!
! 8.1   MADD3 PROGRAM SPECIFICATION
!
! 8.1.1 MADD3 adds together three matrices.
!
! 8.1.2 RESTRICTIONS - NONE
!
! 8.1.3 REFERENCES - NONE
!
! 8.2   MADD3 PROGRAM INTERFACE
!
! 8.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. D(3,3)  -  THE SUM OF ROTATION MATRICES A, B, AND C.
!
! 8.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 8.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3,3), B(3,3), C(3,3), D(3,3)
      Integer*4 I, J
!
! 8.2.4 DATA BASE ACCESS - NONE
!
! 8.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 8.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: M1950, NUTG, PREG, PREP
!             CALLED SUBROUTINES: NONE
!
! 8.2.7 CONSTANTS USED - NONE
!
! 8.2.8 PROGRAM VARIABLES - NONE
!
! 8.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!     MADD3 PROGRAM STRUCTURE
!
!   Do the additions.
      DO 120  J = 1,3
        DO 110  I = 1,3
          D(I,J) = A(I,J) + B(I,J) + C(I,J)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD3." )
      WRITE ( 6, 9200 )  A, B, C, D
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'D   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MADD4 ( A, B, C, D, E )
      IMPLICIT None
!
! 8.    MADD4
!
! 8.1   MADD4 PROGRAM SPECIFICATION
!
! 8.1.1 MADD4 adds together four matrices.
!
! 8.1.2 RESTRICTIONS - NONE
!
! 8.1.3 REFERENCES - NONE
!
! 8.2   MADD4 PROGRAM INTERFACE
!
! 8.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
!             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. E(3,3)  -  THE SUM OF ROTATION MATRICES A, B, C, AND D.
!
! 8.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 8.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3,3), B(3,3), C(3,3), D(3,3), E(3,3)
      Integer*4 I, J
!
! 8.2.4 DATA BASE ACCESS - NONE
!
! 8.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
!
! 8.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: M2000, ?
!             CALLED SUBROUTINES: NONE
!
! 8.2.7 CONSTANTS USED - NONE
!
! 8.2.8 PROGRAM VARIABLES - NONE
!
! 8.2.9 PROGRAMMER - 95.12.11 David Gordon Created from MADD3
!
!     MADD4 PROGRAM STRUCTURE
!
!   Do the additions.
      DO 120  J = 1,3
        DO 110  I = 1,3
          E(I,J) = A(I,J) + B(I,J) + C(I,J) + D(I,J)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD4." )
      WRITE ( 6, 9200 )  A, B, C, D, E
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'D   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'E   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MADD5 ( A, B, C, D, E, F )
      IMPLICIT None
!
! 9.    MADD5
!
! 9.1   MADD5 PROGRAM SPECIFICATION
!
! 9.1.1 MADD5 adds together five matrices.
!
! 9.1.2 RESTRICTIONS - NONE
!
! 9.1.3 REFERENCES - NONE
!
! 9.2   MADD5 PROGRAM INTERFACE
!
! 9.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
!             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
!             5. E(3,3)  -  THE FIFTH ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. F(3,3)  -  THE SUM OF ROTATION MATRICES A, B, C, D, AND E.
!
! 9.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
! 9.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3,3), B(3,3), C(3,3), D(3,3), E(3,3), F(3,3)
      Integer*4 I,J
!
! 9.2.4 DATA BASE ACCESS - NONE
!
! 9.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
! 9.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: M1950
!             CALLED SUBROUTINES: NONE
!
! 9.2.7 CONSTANTS USED - NONE
!
! 9.2.8 PROGRAM VARIABLES - NONE
!
! 9.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   MADD5 Program Structure
!
!     Do the additons.
      DO 120  J = 1,3
        DO 110  I = 1,3
          F(I,J) = A(I,J) + B(I,J) + C(I,J) + D(I,J) + E(I,J)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD5." )
      WRITE ( 6, 9200 )  A, B, C, D, E, F
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'D    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'E    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'F    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MTRAN ( A, B )
      IMPLICIT None
!
!10.    MTRAN
!
!10.1   MTRAN PROGRAM SPECIFICATION
!
!10.1.1 MTRAN constructs the transpose of a matrix.
!
!10.1.2 RESTRICTIONS - NONE
!
!10.1.3 REFERENCES - NONE
!
!10.2   MTRAN PROGRAM INTERFACE
!
!10.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE ROTATION MATRIX TO BE TRANSPOSED.
!
!           OUTPUT VARIABLES:
!             1. B(3,3)  -  THE TRANSPOSE OF THE ROTATION MATRIX A.
!
!10.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
!10.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3,3), B(3,3)
      Integer*4 I, J
!
!10.2.4 DATA BASE ACCESS - NONE
!
!10.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
!10.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: ATMG, AXOG
!             CALLED SUBROUTINES: NONE
!
!10.2.7 CONSTANTS USED - NONE
!
!10.2.8 PROGRAM VARIABLES - NONE
!
!10.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   MTRAN Program Structure
!
!     Do the transpostion.
      DO 120  J = 1,3
        DO 110  I = 1,3
          B(I,J) = A(J,I)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for utility MTRAN." )
      WRITE ( 6, 9200 )  A, B
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!*****************************************************************************
      SUBROUTINE VECRT ( A, V, RV )
      IMPLICIT None
!
!11.    VECRT
!
!11.1   VECRT PROGRAM SPECIFICATION
!
!11.1.1 VECRT is the matrix utility which multiplies a vector by a rotation
!       matrix.
!
!11.1.2 RESTRICTIONS - NONE
!
!11.1.3 REFERENCES - NONE
!
!11.2   VECRT PROGRAM INTERFACE
!
!11.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE ROTATION MATRIX PERFORMING THE COORDINATE SYSTEM
!                           ROTATION.
!             2. V(3)    -  THE VECTOR BEING ROTATED BY THE ROTATION MATRIX.
!
!           OUTPUT VARIABLES:
!             1. RV(3)  -  THE ROTATED VECTOR.
!
!11.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!           VARIABLES 'FROM':
!             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!           VARIABLES 'TO': NONE
!
!11.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  A(3,3), V(3), RV(3)
      Integer*4 I
!
!11.2.4 DATA BASE ACCESS - NONE
!
!11.2.5 EXTERNAL INPUT/OUTPUT -
!             1. POSSIBLE DEBUG OUTPUT
!
!11.2.6 SUBROUTINE INTERFACE -
!           CALLER SUBROUTINES: ATMG, AXOG, ETDG, ETDP, PREP, ROSIT, UT1P, WOBP
!           CALLED SUBROUTINES: NONE
!
!11.2.7 CONSTANTS USED - NONE
!
!11.2.8 PROGRAM VARIABLES - NONE
!
!11.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.15 Converted to Implicit None.
!
!   VECRT PROGRAM STRUCTURE
!
!   Perform the rotation.
      DO 100  I = 1,3
        RV(I) =   A(I,1) * V(1) &
     &          + A(I,2) * V(2) &
     &          + A(I,3) * V(3)
  100 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility VECRT." )
      WRITE ( 6, 9200 )  A, V, RV
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'V   = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'RV  = ', 3 ( D30.16, 10X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MSUB2 ( A, B, C )
      IMPLICIT None
!
! 7.    MSUB2
!
! 7.1   MSUB2 PROGRAM SPECIFICATION
!
! 7.1.1 MSUB2 is the matrix utility which subtracts one matrix from another.
!
! 7.2   MSUB2 PROGRAM INTERFACE
!
! 7.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
!             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
!           OUTPUT VARIABLES:
!             1. C(3,3)  -  THE Difference OF ROTATION MATRICES A AND B.
!
! 7.2.2 COMMON BLOCKS USED -
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 7.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  A(3,3), B(3,3), C(3,3)
      Integer*4 I, J
!
! 7.2.9 PROGRAMMER - David Gordon 02/22/2013. Modified MADD2.
!
!   MSUB2 Program Structure
!
!   Do the subtraction.
      DO 120  J = 1,3
        DO 110  I = 1,3
          C(I,J) = A(I,J) - B(I,J)
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MSUB2." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
      SUBROUTINE MATEQ ( A, C )
      IMPLICIT None
!
! 7.    MATEQ
!
! 7.1   MATEQ PROGRAM SPECIFICATION
!
! 7.1.1 MATEQ is the matrix utility which sets one matrix equal to another.
!
! 7.2   MATEQ PROGRAM INTERFACE
!
! 7.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. A(3,3)  -  THE Input ROTATION MATRIX.
!           OUTPUT VARIABLES:
!             1. C(3,3)  -  THE output ROTATION MATRIX.
!
! 7.2.2 COMMON BLOCKS USED -
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 7.2.3 PROGRAM SPECIFICATIONS -
      Real*8  A(3,3), C(3,3)
      Integer*4 I, J
!
! 7.2.9 PROGRAMMER - David Gordon 02/22/2013. Modified MADD2.
!
!   MATEQ Program Structure
!
!   Set matrix C equal to matrix A.
      DO 120  J = 1,3
        DO 110  I = 1,3
          C(I,J) = A(I,J) 
  110   CONTINUE
  120 CONTINUE
!
!   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MATEQ." )
      WRITE ( 6, 9200 )  A, C
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!
!******************************************************************************
