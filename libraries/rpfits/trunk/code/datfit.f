      SUBROUTINE DATFIT (OLDDAT, NEWDAT, IERR)
*-----------------------------------------------------------------------
* DATFIT translates a date from the old form, DD/MM/YY (or rarely
* DD/MM/YYYY), to the new form, YYYY-MM-DD, and also fixes some bad
* dates written at Mopra in 2000 with the year as '**'.
*
* Returns the current UTC date if the input date is blank (as used by
* RPFITSOUT).
*
*   Given:
*      OLDDAT   C**      Date in DD/MM/YY format.  If blank the current
*                        UTC date is supplied.
*
*   Returned:
*      NEWDAT   C*12     Date in YYYY-MM-DD form.
*
*      IERR     I        Error status:
*                           0: Success.
*                           1: Illegal OLDDAT.
*
*   Notes:
*      1) A date of the form DD/MM/YYYY is known to have been written
*         at least once at the ATCA on 18/11/1998 when the date format
*         was changed from DD/MM/YY to YYYY-MM-DD.
*
* $Id: datfit.f,v 1.10 2006/10/25 01:48:04 cal103 Exp $
*-----------------------------------------------------------------------
      INTEGER   IMON, IDAY, IYEAR, IERR
      CHARACTER INDATE*8, NEWDAT*12, OLDDAT*(*)
*-----------------------------------------------------------------------
      IF (LEN(OLDDAT).GE.8 .AND. OLDDAT.NE.' ') THEN
         IF (OLDDAT(3:3).NE.'/') THEN
*           New date format.
            NEWDAT = OLDDAT
            IERR = 0
            RETURN
         END IF

         IF (LEN(OLDDAT).GE.10) THEN
            IF (OLDDAT(7:8).EQ.'19' .AND. OLDDAT(9:10).NE.' ') THEN
*              Convert DD/MM/YYYY (rare) to DD/MM/YY.
               OLDDAT(7:) = OLDDAT(9:10)
            END IF
         END IF

*        Rescue bad dates written at Mopra in 2000.
         INDATE = OLDDAT
         IF (INDATE(7:8).EQ.'**') INDATE(7:8) = '00'

         READ (INDATE, '(I2,1X,I2,1X,I2)', IOSTAT=IERR) IDAY, IMON,
     :      IYEAR
         IF (IERR.NE.0) THEN
*           Bad date string.
            IERR = 1
            RETURN
         END IF

         IF (IMON.LT.1 .OR. IMON.GT.12 .OR.
     :       IDAY.LT.1 .OR. IDAY.GT.31) THEN
*           Invalid date fields.
            IERR = 1
            RETURN
         END IF

*        Years written at Mopra past 1999.
         IF (IYEAR.LT.70) IYEAR = IYEAR + 100

         IYEAR = 1900 + IYEAR

      ELSE
*        Get the current UTC date.
         CALL UTDATE (IYEAR, IMON, IDAY)
      END IF

      WRITE (NEWDAT, '(I4.4,2(A,I2.2))') IYEAR, '-', IMON, '-', IDAY

      RETURN
      END
