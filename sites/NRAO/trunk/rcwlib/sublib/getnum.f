      DOUBLE PRECISION FUNCTION GETNUM( STRING, CHAR1, CHARN )
C
C     Function to decode a number in STRING.
C     R. C. Walker
C     Complete rewrite 5jan98.  Added exponential forms and
C     better bad data tests.
C
      CHARACTER*(*)  STRING
      INTEGER        CHAR1, CHARN, IC, DIG
      DOUBLE PRECISION  INTG, FRAC, EXPON, ISIGN, IESIGN, DIV
      LOGICAL        NEG, PER, EXP, RI, RF, RE, ERR, BLANK
C ----------------------------------------------------------------------
      GETNUM = 0.D0
      NEG = .FALSE.
      RI = .TRUE.
      RF = .FALSE.
      RE = .FALSE.
      INTG = 0.D0
      FRAC = 0.D0
      EXPON = 0.D0
      DIV = 1.D0
      ISIGN = 1.0
      IESIGN = 1.0
      ERR = .FALSE.
C
C     Protect against absurd values.
C
      IF( CHAR1 .LT. 1 .OR. CHARN .LT. CHAR1 .OR.
     1    CHARN .GT. CHAR1 + 25 ) THEN
         WRITE( *, '( A, 2I10 )' )
     1     ' GETNUM: Unreasonable character range: ', CHAR1, CHARN,
     2     ' - set value to 0. '
         ERR = .TRUE.
         GETNUM = 0.D0
      ELSE IF( INDEX( STRING(CHAR1:CHARN), 'NaN' ) .NE. 0 ) THEN
         WRITE( *, '( A, A )' ) ' GETNUM: NaN found.  Return 0.'
         ERR = .TRUE.
         GETNUM = 0.D0
      ELSE IF( INDEX( STRING(CHAR1:CHARN), '*' ) .NE. 0 ) THEN
         WRITE( *, '( A, A )' ) ' GETNUM: * found.  Return 0.'
         ERR = .TRUE.
         GETNUM = 0.D0
      ELSE
         DO IC = CHAR1, CHARN
C
C           Decode the next character.
C
            IF( .NOT. ERR )
     1         CALL GETDIG( STRING(IC:IC), DIG, NEG, PER, EXP, 
     2                      BLANK, ERR )
C
C           Deal with case where it has been the integer part so far.
C
            IF( .NOT. ERR .AND. .NOT. BLANK) THEN
               IF( RI ) THEN
                  IF( NEG ) THEN
                     IF( ISIGN .GT. 0.D0 ) THEN
                        ISIGN = -1.D0
                     ELSE
                        ERR = .TRUE.
                     END IF
                  ELSE IF( PER ) THEN
                     RI = .FALSE.
                     RF = .TRUE.
                  ELSE IF( EXP ) THEN
                     RI = .FALSE.
                     RE = .TRUE.
                  ELSE
                     INTG = INTG * 10.D0 + DBLE( DIG )
                  END IF
               ELSE IF( RF ) THEN
                  IF( NEG .OR. PER ) THEN
                     ERR = .TRUE.
                  ELSE IF( EXP ) THEN
                     RF = .FALSE.
                     RE = .TRUE.
                  ELSE
                     DIV = DIV * 10.D0
                     FRAC = FRAC + DBLE( DIG ) / DIV
                  END IF
               ELSE IF( RE ) THEN
                  IF( NEG ) THEN
                     IF( IESIGN .GT. 0.D0 ) THEN
                        IESIGN = -1.D0
                     ELSE
                        ERR = .TRUE.
                     END IF
                  ELSE IF( PER .OR. EXP ) THEN
                     ERR = .TRUE.
                  ELSE
                     EXPON = EXPON * 10.D0 + DBLE( DIG )
                  END IF
               END IF
            END IF
         END DO
C
C        Deal with an error or put the number together.
C
         IF( ERR ) THEN
            WRITE(*,*) 'GETNUM: Bad number, use 0: ', 
     1                 STRING(CHAR1:CHARN)
            GETNUM = 0.D0
         ELSE
            GETNUM = ISIGN * ( INTG + FRAC ) * 10.D0**(IESIGN*EXPON)
         END IF
C
      END IF
C
      RETURN
      END
      SUBROUTINE GETDIG( STRING, DIG, NEG, PER, EXP, BLANK, ERR )
C
C     Routine to decode the single digit in STRING.
C
      CHARACTER*1 STRING
      INTEGER     DIG
      CHARACTER*1 NUMS(16)
      INTEGER     I
      LOGICAL     NEG, PER, EXP, ERR, BLANK
      DATA  NUMS / '1', '2', '3', '4', '5', '6', '7', '8', '9',
     1             '0', ' ', '-', '+', '.', 'E', 'D' /
C ---------------------------------------------------------------------
C     See if the character is recognized.
C
      DIG = -99
      NEG = .FALSE.
      PER = .FALSE.
      EXP = .FALSE.
      BLANK = .FALSE.
      DO I = 1, 16
         IF( STRING .EQ. NUMS(I) ) THEN
            DIG = I
            IF( I .EQ. 10 ) THEN
               DIG = 0
            ELSE IF( I .EQ. 11 ) THEN
               DIG = 0
               BLANK = .TRUE.
            ELSE IF( I .EQ. 12 ) THEN
               DIG = 0
               NEG = .TRUE.
            ELSE IF( I .EQ. 13 ) THEN
               DIG = 0
            ELSE IF( I .EQ. 14 ) THEN
               DIG = 0
               PER = .TRUE.
            ELSE IF( I .EQ. 15 .OR. I .EQ. 16 ) THEN
               DIG = 0
               EXP = .TRUE.
            END IF
            GO TO 100
         END IF
      END DO
  100 CONTINUE
C
C     Got unrecognized character.
C
      IF( DIG .EQ. -99 ) THEN
         ERR = .TRUE.
         DIG = 0
      END IF
C
      RETURN
      END
