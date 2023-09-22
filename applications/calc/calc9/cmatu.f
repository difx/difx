      SUBROUTINE ROTAT ( THETA, I, R )
      IMPLICIT None
C
C 1.    ROTAT
C
C 1.1   ROTAT PROGRAM SPECIFICATION
C
C 1.1.1 ROTAT is the matrix utility routine which creates a matrix 'R' which
C       performs a coordinate system rotation by an angle 'THETA' about 
C       coordinate axis 'I' in the counterclockwise direction looking along the
C       axis towards the origin.
C
C 1.1.2 RESTRICTIONS - NONE
C
C 1.1.3 REFERENCES - NONE
C
C 1.2   ROTAT PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. I      -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
C                          (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
C                          THE X, Y, AND Z AXES.) 
C             2. THETA  -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM 
C                          ROTATION IS PERFORMED. (RAD)
C 
C           OUTPUT VARIABLES: 
C             1. R(3,3)  -  THE 3x3 ROTATION MATRIX WHICH PERFORMS THE 
C                            COORDINATE SYSTEM ROTATION. (UNITLESS)
C 
C 1.2.2 COMMON BLOCKS USED -
C 
       INCLUDE 'ccon.i'               
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 1.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 R(3,3), THETA, C, S 
      Integer*2 I
C 
C 1.2.4 DATA BASE ACCESS - NONE 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT -  NONE
C 
C           OUTPUT VARIABLES:
C             1.  POSSIBLE DEBUG OUTPUT
C 
C 1.2.6 SUBROUTINE INTERFACE -
C 
C             CALLER SUBROUTINES: DIRNL, ETDG, NUTG, PREG, SITI, WOBG 
C             CALLED SUBROUTINES: DCOS, DSIN
C 
C 1.2.7 CONSTANTS USED - NONE 
C 
C 1.2.8 PROGRAM VARIABLES - 
C             1.  C  -  THE COSINE OF THE ROTATION ANGLE THETA. (UNITLESS)
C             2.  S  -  THE SINE OF THE ROTATION ANGLE THETA. (UNITLESS)
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    SAVITA GOEL    06/04/87 (CDS FOR A900)
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C     ROTAT Program structure
C
C   Compute the cosine and sine of the rotation angle THETA.
      C = DCOS ( THETA )
      S = DSIN ( THETA )
C
C   Go to the right logic depending to the axix of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
C
C   Rotation about the X-axis.
C
C          ( 1  0  0 )
C   R(X) = ( 0  C  S )
C          ( 0 -S  C )
C
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
C
C   Rotation about the Y-axis.
C
C          ( C  0 -S )
C   R(Y) = ( 0  1  0 )
C          ( S  0  C )
C
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
C
C   Rotation about the Z-axis.
C
C          ( C  S  0 )
C   R(Z) = (-S  C  0 )
C          ( 0  0  1 )
C
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
C
C   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility ROTAT." )
      WRITE ( 6, 9200 )  C, S, I, THETA, R
 9200 FORMAT (1X, "C     = ", D30.16, /, 1X,
     1            'S     = ', D30.16, /, 1X,
     2            'I     = ', I2, /, 1X,
     3            'THETA = ', D30.16, /, 1X,
     4            'R     = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C    Normal conclusion.
  700 RETURN
      END
C
C******************************************************************************
      SUBROUTINE DROTT ( THETA, DTHETA, I, DR )
      IMPLICIT None
C
C 2.    DROTT
C
C 2.1   DROTT PROGRAM SPECIFICATION
C
C 2.1.1 DROTT is the matrix utility routine which creates the partial derivative
C       of a rotation matrix with respect to a variable on which the rotation
C       matrix is functionally dependent.
C
C 2.1.2 RESTRICTIONS - NONE
C
C 2.1.3 REFERENCES - NONE
C
C 2.2   DROTT PROGRAM INTERFACE
C
C 2.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. DTHETA  -  THE PARTIAL DERIVATIVE OF THE ROTATION ANGLE THETA
C                           WITH RESPECT TO THE VARIABLE IN QUESTION.
C             2. I       -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
C                           (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
C                           THE X, Y, AND Z AXES.)
C             3. THETA   -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM 
C                           ROTATION IS PERFORMED. (RAD) 
C 
C           OUTPUT VARIABLES: 
C             1. DR(3,3)  -  THE PARTIAL DERIVATIVE OF THE ROTATION MATRIX IN
C                            QUESTION WITH RESPECT TO THE VARIABLE IN QUESTION. 
C 
C 2.2.2 COMMON BLOCKS USED -
C 
       INCLUDE 'ccon.i'             
C 
C           VARIABLES 'FROM':
C             1.  KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 2.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 DR(3,3), THETA, DTHETA, DC, DS
      Integer*2 I
C 
C 2.2.4 DATA BASE ACCESS - NONE 
C 
C 2.2.5 EXTERNAL INPUT/OUTPUT - 
C             1.  POSSIBLE DEBUG OUTPUT
C 
C 2.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DIRNL, NUTG, PREG, PREP, UT1P, WOBP 
C             CALLED SUBROUTINES: DCOS, DSIN
C 
C 2.2.7 CONSTANTS USED - NONE 
C 
C 2.2.8 PROGRAM VARIABLES - 
C 
C             1. DC  -  THE COSINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY
C                       THE PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
C                       RESPECT TO THE VARIABLE IN QUESTION (DTHETA).
C             2. DS  -  THE SINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY THE
C                       PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH RESPECT TO
C                       THE VARIABLE IN QUESTION (DTHETA).
C
C 2.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C     DROTT Program Structure
C
C   Compute the program variables DC and DS.
      DC = DCOS ( THETA ) * DTHETA
      DS = DSIN ( THETA ) * DTHETA
C
C   Go to the correct logic depending on the axis of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
C
C   Rotation about the X-axis.
C
C          (  0   0   0  )
C  DR(X) = (  0 -DS  DC  )
C          (  0 -DC -DS  )
C
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
C
C   Rotation about the Y-axis.
C
C          (-DS   0 -DC  )
C  DR(Y) = (  0   0   0  )
C          ( DC   0 -DS  )
C
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
C
C   Rotation about the Z-axis.
C
C          (-DS  DC   0  )
C  DR(Z) = (-DC -DS   0  )
C          (  0   0   0  )
C
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
C
C   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility DROTT." )
      WRITE ( 6, 9200 )  DC, DS, DTHETA, I, THETA, DR
 9200 FORMAT (1X, "DC     = ", D30.16, /, 1X,
     1            'DS     = ', D30.16, /, 1X,
     2            'DTHETA = ', D30.16, /, 1X,
     3            'I      = ', I2, /, 1X,
     4            'THETA  = ', D30.16, /, 1X,
     5            'DR     = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C   7.    NORMAL PROGRAM CONCLUSION.
C
  700 RETURN
      END
C
C******************************************************************************
      SUBROUTINE DDROT ( THETA, DDTHTA, I, DDR )
      IMPLICIT None
C
C 3.    DDROT
C
C 3.1   DDROT PROGRAM SPECIFICATION
C
C 3.1.1 DDROT is the matrix utility routine which creates the second
C       partial derivative of a rotation matrix with respect to a
C       variable on which the rotation matrix is functionally dependent.
C
C 3.1.2 RESTRICTIONS - THIS UTILITY ROUTINE ASSUMES THAT THE SECOND PARTIAL
C                      DERIVATIVE OF THE FUNCTIONALLY DEPENDENT VARIABLE WITH
C                      RESPECT TO THE PARAMETER IN QUESTION IS EQUAL TO ZERO.
C                      THIS IS A VERY GOOD ASSUMPTION IN ALL CASES WHERE
C                      SUBROUTINE DDROT IS USED IN THIS PROGRAM.
C
C 3.1.3 REFERENCES - NONE
C
C 3.2   DDROT PROGRAM INTERFACE
C
C 3.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. DDTHTA  -  THE SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE
C                           THETA WITH RESPECT TO THE VARIABLE IN QUESTION.
C             2. I       -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
C                           (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
C                           THE X, Y, AND Z AXES. )
C             3. THETA   -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM 
C                           ROTATION IS PERFORMED.  (RAD) 
C 
C           OUTPUT VARIABLES: 
C             1. DDR(3,3)  -  THE SECOND PARTIAL DERIVATIVE OF THE ROTATION 
C                             MATRIX IN QUESTION WITH RESPECT TO THE VARIABLE IN
C                             QUESTION. 
C 
C 3.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'                 
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 3.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  DDR(3,3), THETA, DDTHTA, DDC, DDS
      Integer*2 I 
C 
C 3.2.4 DATA BASE ACCESS - NONE 
C 
C 3.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C 
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DIRNL, UT1P 
C             CALLED SUBROUTINES: DCOS, DSIN
C 
C 3.2.7 CONSTANTS USED - NONE 
C 
C 3.2.8 PROGRAM VARIABLES -
C             1. DDC  -  THE COSINE OF THE ROTATION ANGLE MULTIPLIED BY THE
C                        SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
C                        RESPECT TO THE VARIABLE IN QUESTION.
C             2. DDS  -  THE SINE OF THE ROTATION ANGLE MULTIPLIED BY THE SECOND
C                        PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH RESPECT
C                        TO THE VARIABLE IN QUESTION.
C
C 3.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   DDROT Program Structure
C
C   Compute the program variables DDC and DDS.
      DDC = DCOS ( THETA ) * DDTHTA
      DDS = DSIN ( THETA ) * DDTHTA
C
C   Go to the correct logic depending of the axis of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
C
C   Rotation ablut the X-axis.
C
C           (   0    0    0   )
C  DDR(X) = (   0 -DDC -DDS   )
C           (   0  DDS -DDC   )
C
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
C
C   Rotation about the Y-axis.
C
C           (-DDC    0  DDS   )
C  DDR(Y) = (   0    0    0   )
C           (-DDS    0 -DDC   )
C
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
C
C   Rotation about the Z-axis.
C
C           (-DDC -DDS    0   )
C  DDR(Z) = ( DDS -DDC    0   )
C           (   0    0    0   )
C
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
C
C   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine DDROT." )
      WRITE ( 6, 9200 )  THETA, I, DDTHTA, DDR
 9200 FORMAT (1X, "THETA  = ", D30.16, /, 1X,
     1            'I      = ', I2, /, 1X,
     2            'DDTHTA = ', D30.16, /, 1X,
     3            'DDR    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C     Normal Conclusion.
  700 RETURN
      END
C
C*******************************************************************************
      SUBROUTINE D3ROT ( THETA, D3THET, I, D3R )
      IMPLICIT None
C
C 2.    D3ROT
C
C 2.1   D3ROT PROGRAM SPECIFICATION
C
C 2.1.1 D3ROT is the matrix utility routine which creates the third partial
C       derivative of a rotation matrix with respect to a variable on which
C       the rotation matrix is functionally dependent.
C
C 2.1.2 RESTRICTIONS - NONE
C
C 2.1.3 REFERENCES - NONE
C
C 2.2   D3ROT PROGRAM INTERFACE
C
C 2.2.1 CALLING SEQUENCE - CALL D3ROT(THETA,D3THET,I,D3R)
C
C           INPUT VARIABLES:
C             1. D3THET  -  THE THIRD PARTIAL DERIVATIVE OF THE ROTATION ANGLE
C                           THETA WITH RESPECT TO THE VARIABLE IN QUESTION. 
C             2. I       -  THE INTEGER FLAG WHICH DETERMINES THE ROTATION AXIS.
C                           (I = 1, 2, 3 REFERS RESPECTIVELY TO ROTATIONS ABOUT
C                           THE X, Y, AND Z AXES.)
C             3. THETA   -  THE ANGLE THROUGH WHICH THE COORDINATE SYSTEM
C                           ROTATION IS PERFORMED. (RAD) 
C 
C           OUTPUT VARIABLES: 
C             1. D3R(3,3) -  THE THIRD PARTIAL DERIVATIVE OF THE ROTATION MATRIX
C                            IN QUESTION WITH RESPECT TO THE VARIABLE IN 
C                            QUESTION. 
C 
C 2.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C 
C           VARIABLES "FROM":
C             1.  KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 2.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  D3R(3,3), THETA, D3THET, D3C, D3S
      Integer*2 I
C 
C 2.2.4 DATA BASE ACCESS - NONE 
C 
C 2.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C 
C 2.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DIRNL, NUTG, PREG, PREP, UT1P, WOBP 
C             CALLED SUBROUTINES: DCOS, DSIN
C 
C 2.2.7 CONSTANTS USED - NONE 
C
C 2.2.8 PROGRAM VARIABLES -
C             1. D3C -  THE COSINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY 
C                       THE SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
C                       RESPECT TO THE VARIABLE IN QUESTION (DTHETA).
C             2. D3S -  THE SINE OF THE ROTATION ANGLE (THETA) MULTIPLIED BY THE
C                       SECOND PARTIAL DERIVATIVE OF THE ROTATION ANGLE WITH
C                       RESPECT TO THE VARIABLE IN QUESTION (DTHETA).
C
C 2.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    BRUCE SCHUPLER 03/27/78
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   D3ROT Program Structure
C
C   Compute the program variables D3C and D3S.
      D3C = DCOS ( THETA ) * D3THET
      D3S = DSIN ( THETA ) * D3THET
C
C   Go to the correct logic depending on the axis of rotation.
      IF ( I .EQ. 1 )  GO TO 300
      IF ( I .EQ. 2 )  GO TO 400
      IF ( I .EQ. 3 )  GO TO 500
C
C   Rotation about the X-axis.
C
C          (  0   0   0  )
C  D3R(X) = (  0 +D3S -D3C  )
C          (  0 +D3C +D3S  )
C
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
C
C   Rotation about the y-axis.
C
C          (+D3S   0 +D3C  )
C  D3R(Y) = (  0   0   0  )
C          (-D3C   0 +D3S  )
C
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
C
C   Rotation about the Z-axis.
C
C          (+D3S -D3C   0  )
C  D3R(Z) = (+D3C +D3S   0  )
C          (  0   0   0  )
C
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
C
C   Check KMATD for debug output.
  600 IF ( KMATD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility D3ROT." )
      WRITE ( 6, 9200 )  D3C, D3S, D3THET, I, THETA, D3R
 9200 FORMAT (1X, "D3C     = ", D30.16, /, 1X,
     1            'D3S     = ', D30.16, /, 1X,
     2            'D3THET = ', D30.16, /, 1X,
     3            'I      = ', I2, /, 1X,
     4            'THETA  = ', D30.16, /, 1X,
     5            'D3R     = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C     Normal conclusion.
  700 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MMUL2 ( A, B, C )
      IMPLICIT None
C
C 4.    MMUL2
C
C 4.1   MMUL2 PROGRAM SPECIFICATION
C
C 4.1.1 MMUL2 is the matrix utility which multiplies two rotation matrices
C       producing a product rotation matrix.
C
C 4.1.2 RESTRICTIONS - NONE
C
C 4.1.3 REFERENCES - NONE
C 
C 4.2   MMUL2 PROGRAM INTERFACE 
C 
C 4.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX. 
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C 
C           OUTPUT VARIABLES: 
C             1. C(3,3)  -  THE PRODUCT OF ROTATION MATRICES A AND B.
C 
C 4.2.2 COMMON BLOCKS USED -
C 
       INCLUDE 'ccon.i'                     
C 
C            VARIABLES 'FROM':
C              1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C              2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C            VARIABLES 'TO': NONE 
C 
C 4.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  A(3,3), B(3,3), C(3,3) 
      Integer*2 I, J
C 
C 4.2.4 DATA BASE ACCESS - NONE 
C 
C 4.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: MMUL3, SITI, WOBG, WOBP
C             CALLED SUBROUTINES: NONE
C
C 4.2.7 CONSTANTS USED - NONE
C
C 4.2.8 PROGRAM VARIABLES - NONE
C
C 4.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   MMUL2 Program Structure
C
C   Perform the multiplication.
      DO 120  J = 1,3
        DO 110  I = 1,3
          C(I,J) =   A(I,1) * B(1,J)
     1             + A(I,2) * B(2,J)
     2             + A(I,3) * B(3,J)
  110   CONTINUE
  120 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD. EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility MMUL2." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'C   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MMUL3 ( A, B, C, D )
      IMPLICIT None
C
C 5.    MMUL3
C
C 5.1   MMUL3 PROGRAM SPECIFICATION
C
C 5.1.1 MMUL3 is the matrix utility routine which multiplies together
C       three rotation matrices producing a product rotation matrix.
C
C 5.1.2 RESTRICTIONS - NONE
C 
C 5.1.3 REFERENCES - NONE 
C 
C 5.2   MMUL3 PROGRAM INTERFACE 
C 
C 5.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX. 
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C             3. C(3,3)  -  THE THIRD ROTATION MATRIX. 
C 
C           OUTPUT VARIABLES: 
C             1. D(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, AND C. 
C 
C 5.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'                 
C 
C           VARIABLES "FROM":
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 5.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  A(3,3), AB(3,3), B(3,3), C(3,3), D(3,3)
C 
C 5.2.4 DATA BASE ACCESS - NONE 
C 
C 5.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: MMUL5, NUTG, PREG, PREP 
C             CALLED SUBROUTINES: MMUL2 
C 
C 5.2.7 CONSTANTS USED - NONE 
C
C 5.2.8 PROGRAM VARIABLES -
C             1. AB(3,3)  -  THE PRODUCT OF ROTATION MATRICES A AND B.
C
C 5.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   MMUL3 Program Structure
C
C   Perform the multiplications.
      CALL MMUL2 ( A, B, AB )
      CALL MMUL2 ( AB, C, D )
C
C   Check KMATD to for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility MMUL3." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MMUL5 ( A, B, C, D, E, F )
      IMPLICIT None
C
C 6.    MMUL5
C
C 6.1   MMUL5 PROGRAM SPECIFICATION
C
C 6.1.1 MMUL5 is the matrix utility routine which multiplies together
C       five rotation matrices producing a product rotation matrix.
C 
C 6.1.2 RESTRICTIONS - NONE 
C 
C 6.1.3 REFERENCES - NONE 
C 
C 6.2   MMUL5 PROGRAM INTERFACE 
C 
C 6.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX. 
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C             3. C(3,3)  -  THE THIRD ROTATION MATRIX. 
C             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
C             5. E(3,3)  -  THE FIFTH ROTATION MATRIX. 
C 
C           OUTPUT VARIABLES: 
C             1. F(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, C, D, AND E.
C 
C 6.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'                 
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 6.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 A(3,3), ABC(3,3), B(3,3), C(3,3), D(3,3), E(3,3), F(3,3) 
C 
C 6.2.4 DATA BASE ACCESS - NONE 
C 
C 6.2.5 EXTERNAL INPUT/OUTPUT - 
C             1.  POSSIBLE DEBUG OUTPUT
C 
C 6.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: M1950, PREP, UT1P, WOBP
C             CALLED SUBROUTINES: MMUL3
C
C 6.2.7 CONSTANTS USED - NONE
C
C 6.2.8 PROGRAM VARIABLES -
C             1. ABC(3,3)  -  THE PRODUCT OF ROTATION MATRICES A, B, AND C.
C
C 6.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C     MMUL5 Program Structure
C
C   Perform the multiplications
      CALL MMUL3 ( A, B, C, ABC )
      CALL MMUL3 ( ABC, D, E, F )
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utililty MMUL5." )
      WRITE ( 6, 9200 )  A, ABC, B, C, D, E, F
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'ABC  = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     3            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     4            'D    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     5            'E    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     6            'F    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MADD2 ( A, B, C )
      IMPLICIT None
C
C 7.    MADD2
C
C 7.1   MADD2 PROGRAM SPECIFICATION
C
C 7.1.1 MADD2 is the matrix utility which adds togeter two matrices.
C
C 7.1.2 RESTRICTIONS - NONE
C
C 7.1.3 REFERENCES - NONE
C
C 7.2   MADD2 PROGRAM INTERFACE
C
C 7.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX. 
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C 
C           OUTPUT VARIABLES: 
C             1. C(3,3)  -  THE SUM OF ROTATION MATRICES A AND B.
C 
C 7.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C 
C           VARIABLES 'FROM':
C             1.  KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2.  KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 7.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  A(3,3), B(3,3), C(3,3) 
      Integer*2 I, J
C 
C 7.2.4 DATA BASE ACCESS - NONE 
C 
C 7.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C
C 7.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: NONE
C             CALLED SUBROUTINES: NONE
C
C 7.2.7 CONSTANTS USED - NONE
C
C 7.2.8 PROGRAM VARIABLES - NONE
C
C 7.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   MADD2 Program Structure
C
C   Do the addition.
      DO 120  J = 1,3
        DO 110  I = 1,3
          C(I,J) = A(I,J) + B(I,J)
  110   CONTINUE
  120 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD2." )
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MADD3 ( A, B, C, D )
      IMPLICIT None
C
C 8.    MADD3
C
C 8.1   MADD3 PROGRAM SPECIFICATION
C
C 8.1.1 MADD3 adds together three matrices.
C
C 8.1.2 RESTRICTIONS - NONE
C
C 8.1.3 REFERENCES - NONE
C
C 8.2   MADD3 PROGRAM INTERFACE
C
C 8.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
C 
C           OUTPUT VARIABLES: 
C             1. D(3,3)  -  THE SUM OF ROTATION MATRICES A, B, AND C. 
C 
C 8.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 8.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 A(3,3), B(3,3), C(3,3), D(3,3) 
      Integer*2 I, J
C 
C 8.2.4 DATA BASE ACCESS - NONE 
C 
C 8.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C 
C 8.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: M1950, NUTG, PREG, PREP
C             CALLED SUBROUTINES: NONE
C
C 8.2.7 CONSTANTS USED - NONE
C
C 8.2.8 PROGRAM VARIABLES - NONE
C
C 8.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C     MADD3 PROGRAM STRUCTURE
C
C   Do the additions.
      DO 120  J = 1,3
        DO 110  I = 1,3
          D(I,J) = A(I,J) + B(I,J) + C(I,J)
  110   CONTINUE
  120 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD3." )
      WRITE ( 6, 9200 )  A, B, C, D
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'C   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     3            'D   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MADD4 ( A, B, C, D, E )
      IMPLICIT None
C
C 8.    MADD4
C
C 8.1   MADD4 PROGRAM SPECIFICATION
C
C 8.1.1 MADD4 adds together four matrices.
C
C 8.1.2 RESTRICTIONS - NONE
C
C 8.1.3 REFERENCES - NONE
C
C 8.2   MADD4 PROGRAM INTERFACE
C
C 8.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C             3. C(3,3)  -  THE THIRD ROTATION MATRIX.
C             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
C 
C           OUTPUT VARIABLES: 
C             1. E(3,3)  -  THE SUM OF ROTATION MATRICES A, B, C, AND D. 
C 
C 8.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 8.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 A(3,3), B(3,3), C(3,3), D(3,3), E(3,3) 
      Integer*2 I, J
C 
C 8.2.4 DATA BASE ACCESS - NONE 
C 
C 8.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
C 
C 8.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: M2000, ?
C             CALLED SUBROUTINES: NONE
C
C 8.2.7 CONSTANTS USED - NONE
C
C 8.2.8 PROGRAM VARIABLES - NONE
C
C 8.2.9 PROGRAMMER - 95.12.11 David Gordon Created from MADD3
C
C     MADD4 PROGRAM STRUCTURE
C
C   Do the additions.
      DO 120  J = 1,3
        DO 110  I = 1,3
          E(I,J) = A(I,J) + B(I,J) + C(I,J) + D(I,J)
  110   CONTINUE
  120 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD4." )
      WRITE ( 6, 9200 )  A, B, C, D, E
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'C   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     3            'D   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     4            'E   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MADD5 ( A, B, C, D, E, F )
      IMPLICIT None
C
C 9.    MADD5
C
C 9.1   MADD5 PROGRAM SPECIFICATION
C
C 9.1.1 MADD5 adds together five matrices.
C
C 9.1.2 RESTRICTIONS - NONE
C
C 9.1.3 REFERENCES - NONE
C
C 9.2   MADD5 PROGRAM INTERFACE
C
C 9.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE FIRST ROTATION MATRIX.
C             2. B(3,3)  -  THE SECOND ROTATION MATRIX.
C             3. C(3,3)  -  THE THIRD ROTATION MATRIX. 
C             4. D(3,3)  -  THE FOURTH ROTATION MATRIX.
C             5. E(3,3)  -  THE FIFTH ROTATION MATRIX. 
C 
C           OUTPUT VARIABLES: 
C             1. F(3,3)  -  THE SUM OF ROTATION MATRICES A, B, C, D, AND E. 
C 
C 9.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C 9.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 A(3,3), B(3,3), C(3,3), D(3,3), E(3,3), F(3,3) 
      Integer*2 I,J
C 
C 9.2.4 DATA BASE ACCESS - NONE 
C 
C 9.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C
C 9.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: M1950
C             CALLED SUBROUTINES: NONE
C
C 9.2.7 CONSTANTS USED - NONE
C
C 9.2.8 PROGRAM VARIABLES - NONE
C
C 9.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   MADD5 Program Structure
C
C     Do the additons.
      DO 120  J = 1,3
        DO 110  I = 1,3
          F(I,J) = A(I,J) + B(I,J) + C(I,J) + D(I,J) + E(I,J)
  110   CONTINUE
  120 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE MADD5." )
      WRITE ( 6, 9200 )  A, B, C, D, E, F
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'C    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     3            'D    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     4            'E    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     5            'F    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C******************************************************************************
      SUBROUTINE MTRAN ( A, B )
      IMPLICIT None
C
C10.    MTRAN
C
C10.1   MTRAN PROGRAM SPECIFICATION
C
C10.1.1 MTRAN constructs the transpose of a matrix.
C
C10.1.2 RESTRICTIONS - NONE
C 
C10.1.3 REFERENCES - NONE 
C 
C10.2   MTRAN PROGRAM INTERFACE 
C 
C10.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE ROTATION MATRIX TO BE TRANSPOSED.
C 
C           OUTPUT VARIABLES: 
C             1. B(3,3)  -  THE TRANSPOSE OF THE ROTATION MATRIX A.
C 
C10.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'                 
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C10.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 A(3,3), B(3,3) 
      Integer*2 I, J
C 
C10.2.4 DATA BASE ACCESS - NONE 
C 
C10.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C
C10.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: ATMG, AXOG
C             CALLED SUBROUTINES: NONE
C
C10.2.7 CONSTANTS USED - NONE
C
C10.2.8 PROGRAM VARIABLES - NONE
C
C10.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   MTRAN Program Structure
C
C     Do the transpostion.
      DO 120  J = 1,3
        DO 110  I = 1,3
          B(I,J) = A(J,I)
  110   CONTINUE
  120 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for utility MTRAN." )
      WRITE ( 6, 9200 )  A, B
 9200 FORMAT (1X, "A    = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'B    = ', 3 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
C
C*****************************************************************************
      SUBROUTINE VECRT ( A, V, RV )
      IMPLICIT None 
C
C11.    VECRT
C
C11.1   VECRT PROGRAM SPECIFICATION
C
C11.1.1 VECRT is the matrix utility which multiplies a vector by a rotation
C       matrix.
C
C11.1.2 RESTRICTIONS - NONE
C
C11.1.3 REFERENCES - NONE
C
C11.2   VECRT PROGRAM INTERFACE
C
C11.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. A(3,3)  -  THE ROTATION MATRIX PERFORMING THE COORDINATE SYSTEM
C                           ROTATION.
C             2. V(3)    -  THE VECTOR BEING ROTATED BY THE ROTATION MATRIX. 
C 
C           OUTPUT VARIABLES: 
C             1. RV(3)  -  THE ROTATED VECTOR.
C 
C11.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'                 
C 
C           VARIABLES 'FROM':
C             1. KMATC  -  THE MATRIX UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KMATD  -  THE MATRIX UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C           VARIABLES 'TO': NONE 
C 
C11.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  A(3,3), V(3), RV(3)
      Integer*2 I
C 
C11.2.4 DATA BASE ACCESS - NONE 
C 
C11.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C 
C11.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: ATMG, AXOG, ETDG, ETDP, PREP, ROSIT, UT1P, WOBP
C           CALLED SUBROUTINES: NONE
C
C11.2.7 CONSTANTS USED - NONE
C
C11.2.8 PROGRAM VARIABLES - NONE
C
C11.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
C                    PETER DENATALE 07/18/77
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.15 Converted to Implicit None.
C
C   VECRT PROGRAM STRUCTURE
C
C   Perform the rotation.
      DO 100  I = 1,3
        RV(I) =   A(I,1) * V(1)
     1          + A(I,2) * V(2)
     2          + A(I,3) * V(3)
  100 CONTINUE
C
C   Check KMATD for debug output.
      IF ( KMATD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility VECRT." )
      WRITE ( 6, 9200 )  A, V, RV
 9200 FORMAT (1X, "A   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'V   = ', 3 ( D30.16, 10X ), /, 1X,
     2            'RV  = ', 3 ( D30.16, 10X ) )
C
  300 RETURN
      END
