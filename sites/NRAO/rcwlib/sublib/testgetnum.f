      PROGRAM TESTGETNUM
C
C     Test the subroutine GETNUM.  Also time it.
C
      CHARACTER          STRING*40
      DOUBLE PRECISION   GETNUM, VAR, GETNUM2
      INTEGER            IVAR, I,J
C -------------------------------------------------------------
C
      STRING = '   -123754   '      
      VAR = GETNUM2( STRING, 1, 12 )
      IVAR = VAR
      WRITE(*,*) STRING
      WRITE(*,*) VAR, IVAR
C
      STRING = '-12.37b54'      
      VAR = GETNUM2( STRING, 1, 9 )
      WRITE(*,*) STRING
      WRITE(*,*) VAR
C
      STRING = 'NaN'      
      VAR = GETNUM2( STRING, 1, 18 )
      WRITE(*,*) STRING
      WRITE(*,'(F25.20)') VAR
C
      STRING = '***'      
      VAR = GETNUM2( STRING, 1, 20 )
      WRITE(*,*) STRING
      WRITE(*,'(F25.20)') VAR
C
      STRING = '-158794.987D-32'
      VAR = GETNUM2( STRING, 1, 22 )
      WRITE(*,*) STRING
      WRITE(*,*) VAR
C
      STRING = '-158794.987E-32'
      VAR = GETNUM2( STRING, 1, 22 )
      WRITE(*,*) STRING
      WRITE(*,*) VAR
C
      DO J = 1, 10
         WRITE(*,*) ' GETNUM '
C
         STRING = '-158794.9876543E-32'
         DO I = 1, 20000
            VAR = GETNUM( STRING, 1, 25 )
         END DO
C
         WRITE(*,*) ' GETNUM2 '
C
         STRING = '-158794.9876543E-32'
         DO I = 1, 20000
            VAR = GETNUM2( STRING, 1, 25 )
         END DO
      END DO
C
      STOP
      END

