      SUBROUTINE TCALS( FREQ, BAND, POL, STCODE, TCAL, TOUT, IYR,
     1                  IMONTH, IDAY, IHR, IMIN, IER )
C
C     Gets Tcals from /vlbacc/home/vlbaops/TCAL/4cm.pt etc. files. 
C     For FITMON and TSYSMON.  Reads from unit 40.
C     Only setting up in FITMON for now.  
C         We use B Greschke's TSYSMON now.
C     SOMEDAY - check date of TCAL file to see if valid.
C
C     Major modifications to read vlbacc on 1991apr01.  RCW
C
C     Call variables:
C       FREQ       R4   input   Frequency in MHz.
C       BAND       C*   input   Band name (eg 4cm)
C       POL        C*   input   Polarization ('RCP' or 'LCP')
C       STCODE     C*   input   Station code.
C       TCAL       R4   output  TCAL.
C       TOUT       I4   input   Output unit for interactive text.
C       IER        I4   output  Error flag: 0=>ok, 1=>freq not found.
C
C       The routine gets both polarizations, then picks the desired one
C       at the end.  This allows it to be changed easily to get both
C       at once as it used to.
C
      INTEGER    CIN, TOUT, NWORDS, WLEN(100), IER, IOERR
      INTEGER    IYR, IMONTH, IDAY, IHR, IMIN
      PARAMETER  (CIN=40)
      INTEGER    VLBOPE, NTC, LEN1
      REAL       FREQ, TCALR, TCALL, TCF, TCRCP, TCLCP, TCAL
      REAL       FRLO, TCLOR, TCLOL, FRHI, TCHIR, TCHIL, DFREQ
      REAL       DFRHI, DFRLO
      REAL*8     GETNUM, TCALTIME, DATATIME
      LOGICAL    FOPEN, TOONEW, GOTDAT
      CHARACTER  CALFILE*80, STCODE*(*), BAND*(*), POL*(*)
      CHARACTER*25  WORD(100), RESULT*255, LBAND*8, LSTCODE*2
      DATA       FOPEN / .FALSE. /
      DATA       CALFILE / '  ' /
      DATA       LBAND, LSTCODE / ' ', ' ' /
C
C     Cal temperatures file.
C
      IF( BAND(1:LEN1(BAND)) .EQ. LBAND(1:LEN1(LBAND)) .AND. 
     1    STCODE(1:LEN1(STCODE)) .EQ. LSTCODE(1:LEN1(LSTCODE)) ) THEN
         REWIND( UNIT=CIN )
      ELSE
         IF( FOPEN ) CLOSE( UNIT=CIN )
         CALFILE = BAND(1:LEN1(BAND))//'.'//STCODE(1:LEN1(STCODE))
         CALL DWCASE( CALFILE )
         CALFILE = '/vlbacc/home/vlbaops/TCAL/' // 
     1         CALFILE(1:LEN1(CALFILE))
         IOERR = VLBOPE( CIN, CALFILE, 'TEXT', 'OLD', RESULT )
         IF( IOERR .EQ. 0 ) THEN
            WRITE( TOUT, '(A)' ) RESULT(1:130)
            WRITE( TOUT, '(A)' ) ' Setting fake TCAL to 0.1 K '
            TCAL = 0.1
            IER = 1
            GO TO 999
         END IF
         LBAND = BAND
         LSTCODE = STCODE
         WRITE( TOUT, '(A,A)' ) ' Opened TCAL file: ', CALFILE
         FOPEN = .TRUE.
      END IF
C
C     Initializations.
C
      FRLO = -1.0E10
      FRHI =  1.0E10
      IER = 0
      TOONEW = .FALSE.
      GOTDAT = .FALSE.
      NTC = 0
C
C     Get data time for comparison with times in file.
C
      DATATIME = IYR*1.D4 + IMONTH*1.D2 + IDAY + (IHR/1.D2) +
     1           (IMIN/1.D4)
C
C     Read lines of the Tcal file.
C
14    CONTINUE
         NWORDS = 100
         CALL RDLINE( WORD, WLEN, NWORDS, CIN, TOUT )
         IF( NWORDS .EQ. -1 ) GO TO 16
         NTC = NTC + 1
C
C        Read the data appropriate for the time of the monitor data.
C
         IF( WORD(1) .EQ. 'RECEIVER' ) THEN
            TCALTIME = GETNUM( WORD(3), 1, WLEN(3) )
            IF( TCALTIME .GT. DATATIME ) THEN
               TOONEW = .TRUE.
            ELSE
               TOONEW = .FALSE.
               IF( GOTDAT ) GO TO 16
            END IF
         END IF
         IF( NWORDS .GE. 3 .AND. WORD(1)(1:1) .NE. '!' .AND.
     1       WORD(1)(1:8) .NE. 'RECEIVER' .AND. .NOT. TOONEW  ) THEN
            GOTDAT = .TRUE.
            TCF   = GETNUM( WORD(1), 1, WLEN(1) )
            TCLCP = GETNUM( WORD(2), 1, WLEN(2) )
            TCRCP = GETNUM( WORD(3), 1, WLEN(3) )
            DFREQ = ABS( TCF  - FREQ )
            DFRHI = ABS( FRHI - FREQ )
            DFRLO = ABS( FRLO - FREQ )
            IF( TCF .GT. FREQ .AND. DFREQ .LT. DFRHI ) THEN
               FRHI = TCF
               TCHIR = TCRCP
               TCHIL = TCLCP
            END IF
            IF( TCF .LE. FREQ .AND. DFREQ .LT. DFRLO ) THEN
               FRLO = TCF 
               TCLOR = TCRCP
               TCLOL = TCLCP
            END IF
         ELSE IF( NWORDS .NE. 0 .AND. WORD(1)(1:1) .NE. '!' 
     1      .AND. WORD(1)(1:8) .NE. 'RECEIVER' .AND. 
     2      .NOT. TOONEW ) THEN
            WRITE( TOUT, '(A, I4, A )' )  ' Line ', NTC, 
     1           ' is strange - check the tcal file. '
         END IF
         GO TO 14
C
C     Now calculate Tcals.  Use end values for frequencies out of range.
C
   16 CONTINUE
      IF( FRLO.EQ.-1.0E10 .AND. FRHI.EQ.1.0E10 ) THEN
         WRITE( TOUT, '(A, F9.2, A )' ) ' Did not find Tcal for: ',
     1           FREQ, ' MHz.  Set to 0.1 '
         TCALR = 0.1
         TCALL = 0.1 
         IER = 1
      ELSE IF( FRLO .EQ. -1.0E10 .OR. FRHI .EQ. FRLO ) THEN
         TCALR = TCHIR
         TCALL = TCHIL
      ELSE IF( FRHI .EQ. 1.0E10 ) THEN
         TCALR = TCLOR
         TCALL = TCLOL 
      ELSE
         TCALR = (TCHIR * (FREQ - FRLO) + TCLOR * (FRHI - FREQ) )
     2                    / ( FRHI - FRLO )
         TCALL = (TCHIL * (FREQ - FRLO) + TCLOL * (FRHI - FREQ) )
     2                    / ( FRHI - FRLO )
      END IF
C
C     Select the desired polarization.
C
      IF( POL .EQ. 'RCP' ) THEN
         TCAL = TCALR
      ELSE IF( POL .EQ. 'LCP' ) THEN
         TCAL = TCALL
      ELSE
         WRITE( TOUT, '(3A)' ) ' TCALS: Specified polarization (',
     1      POL, ') bad.'
         IER = 1
      END IF
C
C     Protect against dummy files with 0 tcal set.
C
      IF( TCAL .LT. 0.1 ) THEN
         WRITE( TOUT, '( A, F10.3, A )' ) 
     1     ' TCALS: TCAL too low: ', TCAL, '  Will use 0.1 '
         TCAL = 0.1
      END IF
C
C     Write result.
C
      IF( IER .EQ. 0 ) THEN
         WRITE( TOUT, '(A, F9.2, 3A, F6.2, A )' ) 
     1          ' Tcal for ', FREQ, ' MHz, ', POL, ' = ', TCAL,
     2          ' K' 
      END IF
C
  999 CONTINUE
      RETURN
      END


