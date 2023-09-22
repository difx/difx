      INTEGER FUNCTION DAYNUM( STRING )
C
C     Given day as yyyymmmdd, what is day number?
C
      CHARACTER      STRING*(*) 
      CHARACTER*3    MONTH(12)
      INTEGER        I, NMONTH, ADD(12), NDAY, NYEAR, I1, LSTR
      REAL*8         GETNUM
      DATA  MONTH / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL',
     1              'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /
      DATA  ADD  / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 
     1             304, 334 /
C
      DO 10 I = 1, 12
         IF( STRING(5:7) .EQ. MONTH(I) ) GO TO 20
   10 CONTINUE
      WRITE(*,*) ' Something wrong with month '
      STOP
C   
   20 CONTINUE
      NMONTH = I
C
      CALL NXTWRD( STRING, 1, I1, LSTR )
      NDAY = IDNINT( GETNUM( STRING, 8, LSTR ) )
      NYEAR = IDNINT( GETNUM( STRING, 3, 4 ) )
      DAYNUM = ADD(NMONTH) + NDAY
      IF( MOD( NYEAR, 4 ) .EQ. 0 .AND. NMONTH .GT. 2 ) 
     1   DAYNUM = DAYNUM + 1
C
      RETURN
      END
