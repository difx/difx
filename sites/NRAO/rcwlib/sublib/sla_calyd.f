      SUBROUTINE sla_CALYD (IY,IM,ID,NY,ND,J)

*+
*
*     - - - - - -
*      C A L Y D
*     - - - - - -
*
*
*  Calendar to year and day in year
*
*
*  Given:
*     IY,IM,ID   int    year, month, day in Gregorian calendar
*                       (year may optionally omit the century)
*  Returned:
*     NY         int    year AD
*     ND         int    day in year (1 = January 1st)
*     J          int    status:
*                        -1 = OK, but outside range 1900/3/1-2100/2/28
*                         0 = OK, and within above range
*                         2 = bad month  (day not computed)
*                         3 = bad day    (year,day both computed)
*
*  J=0 means that the date is within the range where the Gregorian
*  rule concerning century leap years can be neglected.
*
*
*  P.T.Wallace   Starlink   April 1985
*
*+

      IMPLICIT NONE

      INTEGER IY,IM,ID,NY,ND,J

      INTEGER I

*  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB/31,28,31,30,31,30,31,31,30,31,30,31/



*  Default century if appropriate
      IF (IY.GE.0.AND.IY.LE.49) THEN
         NY=IY+2000
      ELSE IF (IY.GE.50.AND.IY.LE.99) THEN
         NY=IY+1900
      ELSE
         NY=IY
      END IF

*  Preset status
      J=0

*  Validate month
      IF (IM.GE.1.AND.IM.LE.12) THEN

*     Allow for leap year
         IF (MOD(NY,4).EQ.0) THEN
            MTAB(2)=29
         ELSE
            MTAB(2)=28
         END IF
         IF (MOD(IY,100).EQ.0.AND.MOD(IY,400).NE.0)
     :      MTAB(2)=28

*     Validate day
         IF (ID.LT.1.OR.ID.GT.MTAB(IM)) J=3

*     Day in year
         ND=ID
         DO I=1,IM-1
            ND=ND+MTAB(I)
         END DO

*     Bad month
      ELSE
         J=2
      END IF

*  Flag dates outside the range 1900 March 1 to 2100 February 28
      IF (J.EQ.0.AND.
     :   (NY.LT.1900.OR.
     :    NY.GT.2100.OR.
     :   (NY.EQ.1900.AND.ND.LT.60).OR.
     :   (NY.EQ.2100.AND.ND.GT.59))) J=-1

      END
