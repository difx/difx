C*JULDA -- find Julian Date and GMST at midnight for a given day
C+
      SUBROUTINE JULDA (YEAR, MONTH, DAY, DATE, GMST)
      INTEGER YEAR
      INTEGER MONTH
      INTEGER DAY
      DOUBLE PRECISION DATE
      DOUBLE PRECISION GMST
C
C Returns DATE = Julian Date and GMST = Greenwich Mean sidereal time
C at 0 hrs U.T. on day DAY/MONTH/YEAR. Accuracy not tested; GMST is
C correct at least to nearest second
C
C History:
C  1977 Aug  5 - TJP.
C  1991 May 18 - TJP.
C-----------------------------------------------------------------------
      INTEGER MOFF(12), IC, NYRM1, JD, NDAYS
      DOUBLE PRECISION DU,SMD,T
      DATA MOFF/0,31,59,90,120,151,181,212,243,273,304,334/
C
C JD number at 12 hrs UT on Jan 0 of year YEAR (Gregorian Calendar).
C
      NYRM1 = YEAR-1
      IC = NYRM1/100
      JD = 1721425 + 365*NYRM1 + NYRM1/4 - IC + IC/4
C
C Number of days from Standard Epoch 1900 Jan 0.5 (JD 2415020.0) to
C Jan 0.0 of YEAR.
C
      DU = DBLE(JD-2415020) - 0.5D0
C
C Day number; is it a leap year?
C
      NDAYS = MOFF(MONTH) + DAY
      IF (MONTH.GT.2 .AND. ( 
     1     ( MOD(YEAR,4).EQ.0 .AND. MOD(YEAR,100).NE.0 ) .OR.
     2     MOD(YEAR,400).EQ.0 ) ) NDAYS = NDAYS+1
C
C UT from Epoch to today (days), (centuries).
C
      SMD = DU+DBLE(NDAYS)
      T = SMD/36525.D0
C
C Greenwich Mean Sidereal Time.
C
      GMST = DBLE(6*3600 + 38*60) +45.836D0
     1     + 8 640 184.542D0*T  +  0.0929D0*T**2
      GMST = MOD(GMST,86400D0)
C
C Julian Date.
C
      DATE = 2415020.D0+SMD
C
      END
