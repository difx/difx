      SUBROUTINE MJD2DAY (MJD,YEAR,MONTH,DAY)
C
C     Convert modified julian day number to year, month number, and
C     day of month. Use the algorithm in the mjd to string function
C     mjd2date.c
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 MJD4, AD, ICEN, ICEN4, IYR4, IYR, IDAY, LEAPDY
      INTEGER*4 MONLEN(12), IMON, MJD
      INTEGER*4 YEAR,MONTH,DAY
C
      DATA MONLEN/31,28,31,30,31,30,31,31,30,31,30,31/
C
      AD = 678576
C
      MJD4 = MJD + AD -1
C
      ICEN4 = MJD4 / 146097
C
      MJD4 = MJD4 - ICEN4 * 146097
C
      ICEN = MJD4 / 36524
      IF (ICEN.EQ.4) ICEN = 3
C
      MJD4 = MJD4 - ICEN * 36524
      IYR4 = MJD4 / 1461
C
      MJD4 = MJD4 - IYR4 * 1461
      IYR  = MJD4 / 365
      IF (IYR.EQ.4) IYR = 3
C
      IDAY = MJD4 - IYR * 365
C
      LEAPDY = 0
      IF (IYR.EQ.3) LEAPDY = 1
      DO IMON = 1, 12
         IDAY = IDAY - MONLEN(IMON)
         IF (IMON.EQ.2) IDAY = IDAY - LEAPDY
         IF (IDAY.LT.0) THEN
            IDAY = IDAY + MONLEN(IMON)
            IF (IMON.EQ.2) IDAY = IDAY + LEAPDY
            MONTH = IMON
            DAY   = IDAY + 1
            GO TO 100
         END IF
      END DO
C
 100  CONTINUE
      YEAR = ICEN4 * 400 + ICEN * 100 + IYR4 * 4 + IYR + 1
C
      RETURN
      END
