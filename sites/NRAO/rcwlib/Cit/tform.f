      CHARACTER*(*) FUNCTION TFORM ( RAD, TYPE, NSIGN, NDEG, NSEC, 
     +       DELIM )
C
C    A function to return time or angle data provided in radians
C    in a character form suitable for printing.  Several formats are
C    provided.  Output is rounded and leading zeros are included. 
C
C    RCW  16 Feb. 1988
C
C    INPUTS:
C        RAD      R*8     The time or angle in radians.
C        TYPE     C*1     'T' => time,  anything else, angle.
C        NSIGN    I       0 => Do not output, or leave room for, sign.
C                            eg. for use with RA. 
C                         > 0 => First character is sign.  Blank for positive.
C                         < 0 => First character is sign.  + for positive.
C        NDEG     I       2 => only two digits for hour or degrees.  
C                         Anything else => implies 3 digits.
C        NSEC     I       Number of seconds digits.  This will be reduced if
C                         necessary so as not to overflow TFORM whose size is
C                         specified in the calling routine.  Absolute upper
C                         limit is 19. 
C                         If 3 or more, includes one for decimal.
C                         1 => 2.  Otherwise there can be confusion.
C                         Can be zero for output of deg and min only.
C        DELIM    C*3     1 character each to go after degrees (or hours),
C                         minutes, and seconds.  @ means do not leave space.
C                         Some common cases:
C                             '  @'  spaces between elements, nothing at end.
C                             'hms'  a VLBA time-like number.
C                             ':: '  typical use of colons.
C                             'd''"' a VLBA angle.
C                             '@@@'  A SNAP time.
C  ---------------------------------------------------------------------
      INTEGER    NSIGN, MSIGN, NDEG, MDEG, NSEC , DEG, MINU, TSEC
      INTEGER    II, I, IDIG, IMOD
      INTEGER    SDIG,  CHLEN, MAXSEC
      DOUBLE PRECISION  RAD, SECRAD, SECIN, SEC, PI, TEST, TMOD, DELT
      LOGICAL    PLUS
      CHARACTER  TYPE*1, DELIM*3, SIGN*1, TEMP*25, OUTCH*28
      PARAMETER  (PI=3.1415926535897932D0)
      PARAMETER  (SECRAD=180.D0*3600.D0/PI)
C ---------------------------------------------------------------------
C     Check for allowed values.
C
      TFORM = ' '
      MSIGN = NSIGN
      IF( MSIGN.NE.0 ) MSIGN = 1
      PLUS = .FALSE.
      IF( NSIGN.LT.0 ) PLUS = .TRUE.
      MDEG = NDEG
      IF( MDEG.NE.2 ) MDEG = 3
C
C     Don't allow too long character strings for the 
C     temporary variables.
C
      CHLEN = MIN( LEN(TFORM), 25 )
      MAXSEC = CHLEN - MSIGN - MDEG - 2 - 3
      IF( DELIM(1:1).EQ.'@') MAXSEC = MAXSEC + 1
      IF( DELIM(2:2).EQ.'@') MAXSEC = MAXSEC + 1
      IF( DELIM(3:3).EQ.'@') MAXSEC = MAXSEC + 1
      MAXSEC = MIN( MAXSEC, 19 )
      SDIG = MIN( NSEC, MAXSEC )
      IF( SDIG.EQ.1 ) SDIG = 2
C
C     Convert from radians to seconds.
C
      SECIN = RAD * SECRAD
      IF( TYPE .EQ. 'T' ) SECIN = SECIN / 15.D0
C
C     Set sign for output and convert SECIN to positive.
C
      SIGN = ' '
      IF( SECIN .LT. 0.D0 ) SIGN = '-'
      IF( SECIN .GE. 0.D0 .AND. PLUS ) SIGN = '+'
      SECIN = ABS( SECIN )
C
C     Deal with the rounding.  Need to do before breaking out degrees
C     and minutes. 
C
C     Note that MOD(SECIN, 10**(SDIG-3)) can overflow an integer.  Hence
C     the somewhat strange scheme used below.  Also, the 0.01*TEST is 
C     added to prevent, for example, 5.0 from being expressed as 4.99999..
C
      IF( SDIG .GE. 3 ) THEN
         TEST = 10.D0 ** (3.D0-SDIG)
         TMOD = MOD( SECIN, TEST )
         IMOD = TMOD / TEST  + 0.5D0
         SECIN = SECIN - TMOD  +  (IMOD+0.01) * TEST 
      ELSE IF( SDIG .EQ. 2 .OR. SDIG .EQ. 3 ) THEN 
         II = SECIN + 0.5D0
         SECIN = II
      ELSE IF( SDIG .EQ. 1) THEN
         II = (SECIN/10.D0) + 0.5D0
         SECIN = II * 10.D0
      ELSE IF( SDIG .EQ. 0 ) THEN
         II = ( SECIN + 30.D0 ) / 60.D0
         SECIN = II * 60
      END IF
C
C     Add a small number so the integerization will go well.
C     Use something appropriate for the number of output digits.
C
      IF( SDIG .LT. 5 ) THEN
         DELT = 1.D-5
      ELSE
         DELT = 10.D0 ** ( -SDIG )
      END IF
      SECIN = SECIN + DELT
C
C     Determine degrees (hours), minutes, and seconds.
C     TSEC is needed to get leading zeros on seconds.
C     Help the integerization with a very small addition.
C
      DEG = INT( SECIN / 3600.D0 )
      MINU = INT( (SECIN - DEG*3600.D0) / 60.D0 )
      SEC = SECIN - DEG*3600.D0 - MINU*60.D0 
      TSEC = INT( SEC / 10.0D0 )
      SEC = SEC - TSEC*10.D0
C
C     Write the temporary character variable.
C     Format will be sdddmmss.ssssssssssss
C
      WRITE( TEMP, '(A1,I3.3,I2.2,I1,F18.16)')
     +        SIGN, DEG, MINU, TSEC, SEC
      IF( TEMP(8:8).EQ.' ' ) TEMP(8:8) = '0'
C
C     Now construct the output form, OUTCH.
C
      OUTCH = ' '
      IDIG = 1
      IF( MSIGN.NE.0 ) THEN
         OUTCH(IDIG:IDIG) = TEMP(1:1)
         IDIG = IDIG + 1
      END IF
      IF( MDEG .EQ.2 ) THEN
         OUTCH(IDIG:IDIG+1) = TEMP(3:4)
         IDIG = IDIG + 2
      ELSE
         OUTCH(IDIG:IDIG+2) = TEMP(2:4)
         IDIG = IDIG + 3
      END IF
      IF( DELIM(1:1).NE.'@' ) THEN
         OUTCH(IDIG:IDIG) = DELIM(1:1)
         IDIG = IDIG + 1
      END IF
C                 minutes
      OUTCH(IDIG:IDIG+1) = TEMP(5:6)
      IDIG = IDIG + 2
      IF( DELIM(2:2).NE.'@' ) THEN
         OUTCH(IDIG:IDIG) = DELIM(2:2)
         IDIG = IDIG + 1
      END IF
C                 seconds
      IF( SDIG.GE.1 ) THEN
         DO 200 I = 1,SDIG
            OUTCH(IDIG+I-1:IDIG+I-1) = TEMP(6+I:6+I)
200      CONTINUE
      END IF
      IDIG = IDIG + SDIG
C
      IF( DELIM(3:3).NE.'@' .AND. SDIG.NE.0 ) THEN
         OUTCH(IDIG:IDIG) = DELIM(3:3)
         IDIG = IDIG + 1
      END IF
C
C     Transfer OUTCH to TFORM.  Doing now rather than earlier
C     protects against cases when TFORM is to short for the number
C     of characters that were set.
C
      TFORM = OUTCH
C
      RETURN
      END
