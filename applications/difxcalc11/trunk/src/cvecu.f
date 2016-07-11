      SUBROUTINE VECAD ( A, B, C )
      IMPLICIT None
!
! 1.    VECAD
!
! 1.1   VECAD PROGRAM SPECIFICATION
!
! 1.1.1 VECAD IS THE VECTOR UTILITY ROUTINE WHICH ADDS TOGETHER
!       TWO VECTORS PRODUCING A SUM VECTOR.
!
! 1.1.2 RESTRICTIONS - NONE
!
! 1.1.3 REFERENCES - NONE
!
! 1.2   VECAD PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1.  A(3,3)  -  THE FIRST VECTOR.
!             2.  B(3,3)  -  THE SECOND VECTOR.
!
!           OUTPUT VARIABLES:
!             1.  C(3,3)  -  THE SUM OF VECTORS A AND B.
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES "TO": NONE
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), B(3), C(3)
      Integer*4 I
!
! 1.2.4 DATA BASE ACCESS - NONE
!
! 1.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: ETDG, ETDP, SITCR, STRCR
!             CALLED SUBROUTINES: NONE
!
! 1.2.7 CONSTANTS USED - NONE
!
! 1.2.8 PROGRAM VARIABLES - NONE
!
! 1.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    SAVITA GOEL    06/04/97 (CDS FOR A900)
!                    David Gordon 94.04.18 Converted to Implicit None.
!
! 1.3   VECAD PROGRAM STRUCTURE
!
!   1.    PERFORM THE ADDITION.
!
      DO 100  I = 1,3
           C(I) = A(I) + B(I)
  100 CONTINUE
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE VECAD." )
!
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A   = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'B   = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'C   = ', 3 ( D30.16, 10X ) )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!*****************************************************************************
      SUBROUTINE VECSB ( A, B, C )
      IMPLICIT None
!
! 2.    VECSB
!
! 2.1   VECSB PROGRAM SPECIFICATION
!
! 2.1.1 VECSB IS THE VECTOR UTILITY ROUTINE WHICH SUBTRACTS
!       TWO VECTORS PRODUCING A DIFFERENCE VECTOR.
!
! 2.1.2 RESTRICTIONS - NONE
!
! 2.1.3 REFERENCES - NONE
!
! 2.2   VECSB PROGRAM INTERFACE
!
! 2.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1.  A(3,3)  -  THE FIRST VECTOR.
!             2.  B(3,3)  -  THE SECOND VECTOR.
!
!           OUTPUT VARIABLES:
!             1.  C(3,3)  -  THE DIFFERENCE OF VECTORS A AND B.
!
! 2.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES "TO": NONE
!
! 2.2.2 PROGRAM SPECIFICATIONS -
!
      Real*8  A(3), B(3), C(3)
      Integer*4 I
!
! 2.2.4 DATA BASE ACCESS - NONE
!
! 2.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
!
! 2.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: ETDC, ETDG, SITCR, SITG
!             CALLED SUBROUTINES: NONE
!
! 2.2.7 CONSTANTS USED - NONE
!
! 2.2.8 PROGRAM VARIABLES - NONE
!
! 2.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    David Gordon 94.04.18 Converted to Implicit None.
!
! 2.3   VECSB PROGRAM STRUCTURE
!
!   1.    PERFORM THE SUBTRACTION.
!
      DO 100  I = 1,3
           C(I) = A(I) - B(I)
  100 CONTINUE
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE VECSB." )
!
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'B    = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'C    = ', 3 ( D30.16, 10X ) )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!******************************************************************************
      Real*8 FUNCTION DOTP ( A, B )
      IMPLICIT None
!
! 3.    DOTP
!
! 3.1   DOTP PROGRAM SPECIFICATION
!
! 3.1.1 DOTP IS THE VECTOR UTILITY ROUTINE WHICH COMPUTES THE DOT
!       PRODUCT BETWEEN TWO VECTORS.
!
! 3.1.2 RESTRICTIONS - NONE
!
! 3.1.3 REFERENCES - NONE
!
! 3.2   DOTP PROGRAM INTERFACE
!
! 3.2.1 CALLING SEQUENCE - NONE
!
!           INPUT VARIABLES:
!             1.  A(3)  -  THE FIRST VECTOR.
!             2.  B(3)  -  THE SECOND VECTOR.
!
!           OUTPUT VARIABLES:
!             1.  DOTP  -  THE DOT PRODUCT BETWEEN VECTORS A AND B.
!
! 3.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES "TO": NONE
!
! 3.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), B(3)
!
! 3.2.4 DATA BASE ACCESS - NONE
!
! 3.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
!
! 3.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: AXOG, ETDC, ETDG, ETDP, PREP, RELP,
!                                 SITP, SUNCR, UT1P, WOBP, etc., etc.
!             CALLED SUBROUTINES: NONE
!
! 3.2.7 CONSTANTS USED - NONE
!
! 3.2.8 PROGRAM VARIABLES - NONE
!
! 3.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    David Gordon 94.04.18 Converted to Implicit None.
!
! 3.3   DOTP PROGRAM STRUCTURE
!
!   1.    COMPUTE THE DOT PRODUCT.
!
      DOTP = A(1) * B(1) &
     &     + A(2) * B(2) &
     &     + A(3) * B(3)
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR FUNCTION DOTP." )
!
      WRITE ( 6, 9200 )  A, B, DOTP
 9200 FORMAT (1X, "A   = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'B   = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'DOTP   = ', D30.16 )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!******************************************************************************
      Real*8 FUNCTION VECMG ( A )
      IMPLICIT None
!
! 4.    VECMG
!
! 4.1   VECMG PROGRAM SPECIFICATION
!
! 4.1.1 VECMG IS THE VECTOR UTILITY ROUTINE WHICH COMPUTES THE
!       MAGNITUDE OF A VECTOR.
!
! 4.1.2 RESTRICTIONS - NONE
!
! 4.1.3 REFERENCES - NONE
!
! 4.2   VECMG PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE - NONE
!
!           INPUT VARIABLES:
!             1.  A(3)  -  THE VECTOR IN QUESTION.
!
!           OUTPUT VARIABLES:
!             1.  VECMG  -  THE MAGNITUDE OF THE VECTOR A.
!
! 4.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES "TO": NONE
!
! 4.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), DOTP
!
! 4.2.4 DATA BASE ACCESS - NONE
!
! 4.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
!
! 4.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: ETDG, SUNCR, et., etc.
!             CALLED SUBROUTINES: DOTP, DSQRT
!
! 4.2.7 CONSTANTS USED - NONE
!
! 4.2.8 PROGRAM VARIABLES - NONE
!
! 4.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    David Gordon 94.04.18 Converted to Implicit None.
!
! 4.3   VECMG PROGRAM STRUCTURE
!
!   1.    PERFORM THE COMPUTATION.
!
      VECMG = DSQRT ( DOTP ( A, A ) )
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR FUNCTION VECMG." )
!
      WRITE ( 6, 9200 )  A, VECMG
 9200 FORMAT (1X, "A    = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'VECMG = ', D30.16 )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!******************************************************************************
      SUBROUTINE VUNIT ( A, B )
      IMPLICIT None
!
! 5.    VUNIT
!
! 5.1   VUNIT PROGRAM SPECIFICATION
!
! 5.1.1 VUNIT IS THE VECTOR UTILITY ROUTINE WHICH NORMALIZES A
!       VECTOR TO PRODUCE A UNIT VECTOR.
!
! 5.1.2 RESTRICTIONS - NONE
!
! 5.1.3 REFERENCES - NONE
!
! 5.2   VUNIT PROGRAM INTERFACE
!
! 5.2.1 CALLING SEQUENCE - NONE
!
!           INPUT VARIABLES:
!             1.  A(3)  -  THE UNNORMALIZED VECTOR.
!
!           OUTPUT VARIABLES:
!             1.  B(3)  -  THE NORMALIZED UNIT VECTOR.
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES "TO": NONE
!
! 5.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), B(3), Amag, Vecmg
      Integer*4 I
!
! 5.2.4 DATA BASE ACCESS - NONE
!
! 5.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: STRCR, SUNCR, THERR
!             CALLED SUBROUTINES: VECMG
!
! 5.2.7 CONSTANTS USED - NONE
!
! 5.2.8 PROGRAM VARIABLES -
!           1.  AMAG  -  THE MAGNITUDE OF THE VECTOR A.
!
! 5.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    David Gordon 94.04.18 Converted to Implicit None.
!
! 5.3   VUNIT PROGRAM STRUCTURE
!
!   1.    PERFORM THE NORMALIZATION.
!
      AMAG = VECMG ( A )
      DO 100  I = 1,3
           B(I) = A(I) / AMAG
  100 CONTINUE
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE VUNIT." )
!
      WRITE ( 6, 9200 )  A, B, AMAG
 9200 FORMAT (1X, "A    = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'B    = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'AMAG = ', D30.16 )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!******************************************************************************
      SUBROUTINE CROSP ( A, B, C )
      IMPLICIT None
!
! 6.    CROSP
!
! 6.1   CROSP PROGRAM SPECIFICATION
!
! 6.1.1 CROSP IS THE VECTOR UTILITY ROUTINE WHICH COMPUTES THE
!       CROSS PRODUCT OF TWO VECTORS.
!
! 6.1.2 RESTRICTIONS - NONE
!
! 6.1.3 REFERENCES - NONE
!
! 6.2   CROSP PROGRAM INTERFACE
!
! 6.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1.  A(3)  -  THE FIRST VECTOR.
!             2.  B(3)  -  THE SECOND VECTOR.
!
!           OUTPUT VARIABLES:
!             3.  C(3)  -  THE VECTOR REPRESENTING THE CROSS PRODUCT OF A x B.
!
! 6.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
!            VARIABLES "TO": NONE
!
! 6.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), B(3), C(3)
!
! 6.2.4 DATA BASE ACCESS - NONE
!
! 6.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
!
! 6.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: SUNCR
!             CALLED SUBROUTINES: NONE
!
! 6.2.7 CONSTANTS USED - NONE
!
! 6.2.8 PROGRAM VARIABLES - NONE
!
! 6.2.9 PROGRAMMER - DALE MARKHAM  01/19/77
!                    PETER DENATALE 07/18/77
!                    David Gordon 94.04.18 Converted to Implicit None.
!
! 6.3   CROSP PROGRAM STRUCTURE
!
!   1.    PERFORM THE CROSS PRODUCT.
!
      C(1) = A(2) * B(3)  -  B(2) * A(3)
      C(2) = A(3) * B(1)  -  B(3) * A(1)
      C(3) = A(1) * B(2)  -  B(1) * A(2)
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE CROSP." )
!
      WRITE ( 6, 9200 )  A, B, C
 9200 FORMAT (1X, "A    = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'B    = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'C    = ', 3 ( D30.16, 10X ) )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!**********************************************************************
      SUBROUTINE VECEQ ( A, C )
      IMPLICIT None
!
! 1.    VECEQ
!
! 1.1   VECEQ PROGRAM SPECIFICATION
!
! 1.1.1 VECEQ IS THE VECTOR UTILITY ROUTINE WHICH SETS AN INPUT VECTOR
!       EQUAL TO AN OUTPUT VECTOR.
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3) - INPUT VECTOR.
!           OUTPUT VARIABLES:
!             1. C(3,3) - OUTPUT VECTOR.
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), C(3)
      Integer*4 I
!
! 1.2.9 PROGRAMMER - David Gordon  Feb. 2013 
!
! 1.3   VECEQ PROGRAM STRUCTURE
!
!   1. PERFORM THE EQUALIZATION.
!
      DO 100  I = 1,3
           C(I) = A(I)
  100 CONTINUE
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE VECAD." )
!
      WRITE ( 6, 9200 )  A, C
 9200 FORMAT (1X, "A   = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'C   = ', 3 ( D30.16, 10X ) )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!
!*****************************************************************************
      SUBROUTINE VECDV ( A, D, C )
      IMPLICIT None
!
! 1.    VECDV
!
! 1.1   VECDV PROGRAM SPECIFICATION
!
! 1.1.1 VECDV IS THE VECTOR UTILITY ROUTINE WHICH DIVIDES A VECTOR
!       BY A CONSTANT.
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3) - INPUT VECTOR.
!             2. D      - Constanat to divide by.
!           OUTPUT VARIABLES:
!             1. C(3,3) - OUTPUT VECTOR.
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), C(3), D
      Integer*4 I
!
! 1.2.9 PROGRAMMER - David Gordon  Feb. 2013 
!
! 1.3   VECDV PROGRAM STRUCTURE
!
!   1. PERFORM THE DIVISION.
!
      DO 100  I = 1,3
           C(I) = A(I)/D
  100 CONTINUE
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE VECDV." )
!
      WRITE ( 6, 9200 )  A, D, C
 9200 FORMAT (1X, "A   = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'D   = ', ( D30.16, 10X ), /, 1X, &
     &            'C   = ', 3 ( D30.16, 10X ) )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!
!*****************************************************************************
      SUBROUTINE VECMU ( A, M, C )
      IMPLICIT None
!
! 1.    VECMU
!
! 1.1   VECMU PROGRAM SPECIFICATION
!
! 1.1.1 VECMU IS THE VECTOR UTILITY ROUTINE WHICH MULTIPLIES A VECTOR
!       BY A CONSTANT.
!
! 1.2.1 CALLING SEQUENCE -
!
!           INPUT VARIABLES:
!             1. A(3,3) - INPUT VECTOR.
!             2. M      - Constanat to multiply by.
!           OUTPUT VARIABLES:
!             1. C(3,3) - OUTPUT VECTOR.
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!
!            VARIABLES "FROM":
!              1.  KVECC  -  THE VECTOR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2.  KVECD  -  THE VECTOR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 A(3), C(3), M
      Integer*4 I
!
! 1.2.9 PROGRAMMER - David Gordon  Feb. 2013 
!
! 1.3   VECMU PROGRAM STRUCTURE
!
!   1. PERFORM THE MULTIPLICATION
!
      DO 100  I = 1,3
           C(I) = A(I) * M
  100 CONTINUE
!
!   2.    CHECK KVECD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
!
      IF ( KVECD .EQ. 0 )  GO TO 300
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE VECMU." )
!
      WRITE ( 6, 9200 )  A, M, C
 9200 FORMAT (1X, "A   = ", 3 ( D30.16, 10X ), /, 1X, &
     &            'M   = ', ( D30.16, 10X ), /, 1X, &
     &            'C   = ', 3 ( D30.16, 10X ) )
!
!   3.    NORMAL PROGRAM CONCLUSION.
!
  300 RETURN
      END
!*****************************************************************************
