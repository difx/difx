      SUBROUTINE UT1A
      IMPLICIT None 
C
C     UT1A adds entries to the table of contents.
C
C     Common blocks used:
C
      INCLUDE 'cmxut.i'
C       Variables 'from':
C         1. Ndays - number of days in the ROTEPH array.
C       Variables 'to':
C         1. IEPOCH - The number of epochs at which TAI - UT1 is desired.
C         2. ASKKER - The database return code from ASK for 'UT1EPOCH'
C
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_EOP   - T/F logical flag telling whether to use
C                               external EOP file input
C
C    Program specifications:
C
      Integer*2 LTEXT(16)
      CHARACTER*32 LTEXT_chr
      EQUIVALENCE (LTEXT,LTEXT_chr)
      Integer*2 ND1, ND3, NVER, KTYPE, Nut1, idm8
C
C     Database access -
C          Access codes:
C            1.  'UT1 MESS'  -  Module text message access code.
C            2.  'UT1 PART'  -  The partial derivatives access code.
C            3.  'UT1 CFLG' -   Module control flag access code.
C            4.  'UT1 -TAI' -   Observation dependent UT1 access code.
C            5.  'UT1EPOCH' -   TAI-UT1 array access code.
C            6.  'ROTEPOCH' -   Access code for the array containing
C                               the epoch at which TAI-UT1 is desired.
C            7.  'UT1INTRP' -   Access code for the type of interpolation
C                               used in UT1MU.
C     Subroutine interface -
C            Caller subroutines: TOCUP
C            Called subroutines: ADDA, ADDR, ASK
C
C     Program variables -
C            1. LTEXT(16) - A dummy array used to hold the message from ASK
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  02/14/77
C      77.07.14 Peter Denatale
C      78.03.21 Bruce Schupler
C      87.06.03 Savita Goel    CDS for A900.
C      88.12.21 Gregg Cooke    Mods for CALC 7.0
C      89.07.11 Jim Ryan       Documentation simplified.
C      89.12.12 Jim Ryan       UNIX-like database interface implimented.
C      90.01.02 Jim Ryan       UT1-TAI stored rather than the reverse.
C      90.11.14 Jim Ryan       Bug in short period control logic fixed.
C      90.11.30 Jim Ryan       Bug in short period control logic fixed
C                              again. Ugh!
C      91.05.15 Brent Archinal Comments fixed, esp. for short period
C                              control logic. Dummy UT1C routine dropped.
C      91.05.30 Jim Ryan       Documentation and code simplified.
C      93.03.17 David Gordon   UT1S implimented as default, new control flag
C                              scheme.
C      94.04.06 David Gordon   Changed to 'Implicit None'. 
C      94.06.09 David Gordon   Shuffled Common UT1CM (R*8, I*4, I*2, L*2) 
C      98.04.13 David Gordon   Common /UT1CM/ moved to 'cmxut.i' include file. 
C      98.05.11 David Gordon   ADD's for optional EOP input via external file.
C                              DEL's for previous EOP Lcodes if external file
C                              input.
C      2000.12.29 David Gordon ADD for 'EOPSCALE' added, time scale of EOP
C                              table. 
C
C     UT1A Program Structure
C
C     ADD for the UT1 module text message.
      CALL ADDA (1,'UT1 MESS','UT1 Module message definition   ',
     1     40, 1, 1 )
C
C     ADD for the UT1 partial derivatives.
      CALL ADDR (2,'UT1 PART','UT1 partial derivatives def.    ',
     1     2, 2, 1 )
C
C     ADD for the UT1 module flow control message.
      CALL ADDA (1,'UT1 CFLG','UT1 control flag message def.   ',
     1    40,1,1)
C
C     ADD for storing with the observations the UT1 values actually used.
      CALL ADDR (2,'UT1 -TAI','UT1 time of day for this obsvr. ',
     .      1,1,1)
C
C     ASK in order to obtain the number of rotation epochs for UT1.
      CALL ASK
     . ('ROTEPOCH',1,ND1,IEPOCH,ND3,NVER,LTEXT_chr,KTYPE,ASKKER)
C
C     If there already is an array of epochs in the database, call 'ADDR' so
C     that we may place the values in later. Otherwise skip this step.
      IF (ASKKER .NE. 0) GO TO 100
      CALL ADDR (1,'UT1EPOCH','TAI - UT1 epoch value definition',
     1          2,IEPOCH,1)
  100 CONTINUE
C
      Call ADDA (1,'UT1INTRP','Message for UT1 interp. scheme  ',
     1          30,1,1)
C
C------------------------------------------------------------------
C  ADD's for optional EOP input via external file
      If (Input_EOP) Then
C
C ADD's for 'TIDALUT1', 'FUT1 INF', 'FUT1 PTS', and 'EOPSCALE'.
C DEL's for 'PUT1 INF', 'EUT1 INF', 'PUT1 PTS', and 'EUT1 PTS'.
C
      CALL ADDI(1,'TIDALUT1','Flag for tidal terms in UT1 sers',1,1,1)
C
      CALL ADDR(1,'FUT1 INF','Final Value TAI-UT1 array descrp',4,1,1)
       Nut1 = UT1IF(3) + .01
      CALL ADDR(1,'FUT1 PTS','Final Value TAI-UT1 data points.',
     *          Nut1,1,1) 
C
      CALL ADDR(1,'A1 - UTC','FJD, A1-UTC (sec),rate off.(sec)',3,1,1)
C
C   'TAI- UTC' used in catiu.f 
      CALL ADDR(1,'TAI- UTC','FJD,TAI-UTC (sec),rate off.(sec)',3,1,1)
C
C   'AI - TAI' used in cctiu.f 
      CALL ADDR(1,'A1 - TAI','TAI USNO- TAI BIH = 0.03439 sec.',3,1,1)
C      Ndays from Subroutine START and cmxut.i 
      CALL ADDR(1,'ROTEPOCH','UT1,pole interp.reference epochs',
     *            2,ndays,1)
      CALL ADDR (1,'UT1EPOCH','TAI - UT1 epoch value definition',
     *           2,ndays,1)
C
      CALL ADDA(1,'FUT1TEXT','Final Value TAI-UT1 origin text.',
     *              40,1,1)
      CALL ADDA(1,'EOPSCALE','Time scale of EOP table epochs. ',
     *               4,1,1)
C
      CALL DELR (1,'PUT1 PTS')
      CALL DELR (1,'PUT1 INF')
      CALL DELA (1,'PUT1TEXT')
      CALL DELR (1,'EUT1 PTS')
      CALL DELR (1,'EUT1 INF')
      CALL DELA (1,'EUT1TEXT')
C
      Endif
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE UT1I
      IMPLICIT None
C
C     UT1I is the UT1 module input and initialization section.
C
C     Common blocks used -
C
      INCLUDE 'cmxut.i'
C            Variables 'from':
C              1. IEPOCH - The number of epochs at which TAI - UT1 is desired.
C              2. ASKKER - The database error return code from ASK.
C              3. Leap_fix   - Used in external input mode. .True. means
C                              correct the input EOP series for accumluated
C                              leap seconds. .False. means do not correct.
C              4. UT1type    - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '.
C                              For 'UT1-UTC ', leap second corrections
C                              must be made.
C              5. EOP_time_scale - EOP table time scale, allowed values:
C                              'TAI     ', 'TCG     ', 'TDB     ',
C                              'TDT     ', 'UTC     ', 'UNDEF   '.
C
C            Variables 'to':
C              1. UT1IF(4)  - The final UT1 information array. This array
C                             contains respectively: 1) The Julian date of the
C                             first tabular point, 2) The increment in days of
C                             the tabular points, 3) The number of tabular
C                             points, 4) The units of the UT1 tabular array per
C                             second. (days, days, unitless, sec/table unit)
C              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
C                             (table units)
C              3. UT1RS(20) - The table of 'TAI-UT1S' or 'TAI-UT1R' values,
C                             as controlled by KUT1C. (seconds) 
C                             !!! TAI-UT1R IS NO LONGER SUPPORTED!!!!!!
C              4. ISHRTFL   - The short period tidal terms flag, (unitless).
C                             = 1 --> UT1 table coming from input database is
C                             true UT1, (that is, fortnightly tidal terms have
C                             not been removed, as in the IRIS or IERS series).
C                             = -1 --> UT1 table coming from input database is
C                             UT1R, (that is, the Yoder fortnightly tidal terms
C                             HAVE been removed as in Bulletin B). 
C                             !!! UT1R IS NO LONGER SUPPORTED!!!!!!
C                             = -2 --> UT1 table coming from input database is
C                             UT1S, (the S tidal terms HAVE been removed).
C             5. Usecubic   - Set to true if cubic interpolation to be used.
C             6. Uselinear  - Set to true if linear interpolation to be used.
C             7. Usespline  - Set to true if spline interpolation to be used.
C
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C             1. KUT1C  -  UT1 module flow control flag, controls the 
C                          temporary removal of periodic terms (UT1S or UT1R)
C                          and the type of interpolation (spline, cubic, or
C                          linear) in the UT1 tables.
C                          = 0. Convert table to UT1S (logic controled by 
C                             ISHRTFL); do spline interpolation for a 1-day
C                             series, or cubic interpolation for a 5-day series;
C                             then restore to true UT1 using the new UT1S model.
C                          = 1. Module completely off, that is, UT1 set equal
C                             to AT.
C                          = 2. Use UT1S; use cubic interpolation for a 1-day
C                             series, or spline interpolation for a 5-day
C                             series. 
C                          = 3. Use UT1S, use linear interpolation for both
C                             1-day and 5-day series.
C             2. KUT1D  -  The UT1 module debug output flag.
C
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_EOP - T/F logical flag telling whether to use external
C                             EOP file input
C              2. Ex_EOP    - File name for EOP external file input. If 'NONE'
C                             or blank, external EOP input will not be done.
C
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
C           VARIABLES 'TO':
C            1. ATMUTC(3)   - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
C                             CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
C                             RATE OF CHANGE OF 'TAI MINUS UTC'. Used in the 
C                             atomic time module. (DAYS, SEC, SEC/SEC)
C            2. ROTEPH(2,20)- The array which contains the epochs at which
C                             TAI - UT1 is desired. The entries are:
C                             1) JD at 0:00 hours UTC,
C                             2) The fraction of a UTC day from 0:00 hours
C                                to the desired epoch.
C            3. A1UTC(3)    -
C            4. A1DIFF(3)   - THE ARRAY CONTAINING THE EPOCH, OFFSET AND RATE
C                             OF CHANGE OF THE OFFSET BETWEEN A1 AND TAI
C                             (DAYS, SECONDS, SECONDS/DAY)
C
C     Program Specifications -
C
      INTEGER*4 get_leapsec, ierr, ierr4
      INTEGER*2  KERR(8), NDO(3), Tab_len 
      INTEGER*4  Increment, max_ut1_pts
      REAL*8     UT1TAB(2,20)
      REAL*8     xjd, ct, fa(5), fad(5), tc2000           ! for NUTFA
      Real*8     yp1, ypn, xleap(5), tol
      Integer*4  N, Itab, II, I
      Real*8     Dut, Dlod, Domega, Dutr, Dlodr, Domegar, Duts, Dlods,
     .           Domegas, Atmut1, Shortp, Divutc, tab_time
      INTEGER*2  LUT1M(40), LOFF(40), LUT1S(40), LYODR(40)
      CHARACTER*40 C_LOFF(2), C_LUT1S(2), C_LYODR(2)
      CHARACTER*40 C_LUT1M(2)
      Equivalence (LUT1S,C_LUT1S), (C_LUT1M,LUT1M),
     .            (LYODR,C_LYODR), (LOFF ,C_LOFF )
C
      Character*20 C_LFI, C_LPR, C_LEX
      Integer*2 LFI(10), LPR(10), LEX(10)
      Equivalence (C_LFI,LFI), (C_LPR,LPR), (C_LEX,LEX)
C
      Integer*2   lutext(40)
      Character*80 utext
      Equivalence (utext,lutext)
C
      Integer*2    mess_linear(30), mess_cubic(30), mess_spline(3)
      Character*60 mess_linear_c , mess_cubic_c, mess_spline_c
      Equivalence(mess_linear,mess_linear_c),(mess_cubic,mess_cubic_c),
     .           (mess_spline,mess_spline_c)
      Character*1 type_found, cdum1(3)
C
      Integer*2   L_EOPSCALE(4)
      Character*8 C_EOPSCALE
      Equivalence( L_EOPSCALE, C_EOPSCALE)
C
      Integer*2 Mxepch, MEPOCH
      DATA MXEPCH /20/
      DATA C_LUT1M /
     .'UT1 Module - Version 2000.12.29, last mod ',
     .'by D. Gordon, GSFC -                  '/
C
      DATA C_LUT1S  /
     .'UT1 Module on. Tidal terms restored usin',
     .'g the IERS (UT1S) model.                '/
C
c     DATA C_LYODR  /
c    .'UT1 Module on. Tidal terms restored usin',
c    .'g the Yoder (UT1R) model.               '/
C
      DATA C_LOFF /
     .'UT1 Module is turned off.               ',
     .'                                        '/
C
      DATA  C_LFI /' Final Values       '/
      DATA  C_LPR /' Preliminary Values '/
      DATA  C_LEX /' Extrapolated Values'/
C
      Data mess_cubic_c  /
     .'UT1 table interpolated with 4 point, 3rd order polynomial.  '/
C
      Data mess_linear_c /
     .'UT1 table interpolated with 2 point, linear interpolation.  '/
C
      Data mess_spline_c /
     .'UT1 table interpolated with 5 point cubic spline.           '/
C
      Data max_ut1_pts /20/
C
C 2.2.4 DATA BASE ACCESS -
C
C            'GET' VARIABLES:
C              1. UT1IF(4)   -   The UT1 information array. (See above.)
C              2. UT1PT(20)  -   The tabular values of 'TAI minus UT1'.
C                                (table units)
C              3. ROTEPH(2,20) - The array which contains the epochs at which
C                                TAI - UT1 is desired. The entries are:
C                                1) JD at 0:00 hours UTC,
C                                2) The fraction of a UTC day from 0:00 hours
C                                to the desired epoch.
C              4. ISHRTFL    -   The short period tidal terms flag. (See above.)
C
C            'PUT' Variables:
C              1.  LUT1M(40)    - The UT1 module text message.
C              2.  LON(40)      - The UT1 module 'turned on' message.
C              3.  LOFF(40)     - The UT1 module 'turned off' message.
C              4.  UT1TAB(2,20) - The array which contains the TAI - UT1 values
C                                 (complete) and the fortnightly corrections to
C                                 TAI - UT1 (s,s).
C
C            Access codes:
C              1.  'UT1 MESS'  -  UT1 module text message code.
C              2.  'FUT1 INF'  -  The access code for the Final UT1 information
C                                 array.
C              3.  'FUT1 PTS'  -  Final UT1 points access code.
C              4.  'PUT1 INF'  -  Like above but preliminary.
C              5.  'PUT1 PTS'  -  Like above but preliminary.
C              6.  'UT1 CFLG'  -  Module control flag access code.
C              7.  'EUT1 INF'  -  Like above but for extropolated data.
C              8.  'EUT1 PTS'  -  Like above but for extropolated data.
C              9.  'UT1EPOCH'  -  Access code for the UT1 epochs array.
C             10.  'ROTEPOCH'  -  Access code for the array of epochs at which
C                                 TAI - UT1 is desired.
C             11.  'TIDALUT1'  -  The access code for the flag indicating
C                                 whether the fortightly terms in UT1 are in
C                                 the input UT1 series. (See below.)
C
C     Subroutine interface -
C             Caller subroutines: INITL
C             Called subroutines: GETI, GET4, KILL, PUTA, PUT4, NUTFA, UT1RZT, 
C                                 UT1SZT, spline
C
C     Program variables:
C           1.  Tab_len   -  The number of tabular points in the UT1
C                            information array in the database
C           2.  KERR(8)   -  The database error return flags.
C           3.  LUT1F(4)  -  The message inserted into the UT1 module text
C                            message if the final UT1 information are used.
C           4.  LUT1P(4)  -  The message if preliminary data is used.
C           5.  NDO(3)    -  Database return array indices.
C           6.  LUT1E(3)  -  The message if extropolated data is used.
C           7.  UT1TAB(2,20)-THe array which contains the values of TAI - UT1
C                            and the short period corrections. (s,s).
C           8.  MEPOCH    -  This is IEPOCH checked to be certain that it is no
C                            greater than MXEPCH.
C           9.  MXEPCH    -  The maximum number of TAI - UT1 EPOCHS allowed.
C          10.  XJD       -  The Julian date at 0 hours UTC of the epoch in 
C                            question (days).
C          11.  UTC       -  The UTC fraction of day of the observation. (days)
C          12.  ATMUT1    -  The complete computed value of TAI - UT1 (s)
C          13.  SHORTP    -  'UT1S - UT1' or 'UT1R - UT1', depending on KUT1C.
C                            (s). Notice the sense!
C          14.  Increment -  The increment of the UT1 table in the database
C                            (days).
C          15.  max_ut1_pts  The maximum size of the UT1 table from the
C                            database.
C          16.  type_found   Tracks the type of UT1 table in the database.
C
C     Programmer - Dale Markham   02/14/77
C      77.07.14  Peter Denatale
C      78.03.28  Bruch Schupler
C      78.07.03  Bruce Schupler
C      84.06.05  David Gordon
C      87.06.03  Savita Goel   CDS FOR A900
C      88.12.21  Gregg Cooke   CALC 7.0 mods.
C      89.06.08  Jim Ryan      Short period logic modified.
C      89.07.11  Jim Ryan      Documentation simplified.
C      89.12.12  Jim Ryan      UNIX-like database interface implimented.
C      90.11.30  Jim Ryan      Bug in short period UT1 control logic fixed
C                              again.
C      91.05.30  Jim Ryan      Documentation furthur simplilfied.
C      91.06.05  Jim Ryan      Mods to support linear interpolation.
C      91.06.19  Jim Ryan      Code added to construct UT1R table from UT1
C                              table. Needed for linear interpolation. For CALC
C                              7.4+ the scaling law for the UT1 table MUST be
C                              1, because SOLVE doesn't know about that law.
C      93.03.17  David Gordon  Code added to use UT1S, new control flag scheme.
C      93.09.07  Norbert Zacharias  Moved calculation of fundamental argumuments
C                              to subroutine NUTFA.
C      93 Dec.   David Gordon  Cubic spline interpolation added, modified
C                              control flag scheme for type of interpolation.
C      93.12.30  David Gordon  Cleaned up 'UT1 MESS'.
C      94.04.06  David Gordon  Changed to 'Implicit None'. 
C      94.09.26  David Gordon  Added some debug printout, changed some I*2's to
C                              I*4's, documentation corrections, cosmetic mods. 
C      94.09.27  David Gordon  Removed unused 'XLOVEK' Love number.
C      95.06.08  David Gordon  Corrected debug printout, wrong format statement
C                              used. 
C      98.01.27  David Gordon  Removed Yoder (UT1R) model interpolation option. 
C                              KUT1C values redefined. Kill if no TIDALUT1
C                              Lcode (old database, UT1R assumed) or if ISHRTFL
C                              equals -1.  
C      98.04.l3  David Gordon  Common /UT1CM/ moved to 'cmxut.i' include file. 
C      98.05.01  David Gordon  Mods for external file input of EOP info.  
C     2001.01.02 David Gordon  CT replaced with tab_time in UT1MU argument. 
C                              Code added to PUT 'EOPSCALE'.
C                            
C     UT1I Program Structure
C
C    Verify that the UT1 module flow control flag is valid
      If (KUT1C.lt.0 .or. KUT1C.gt.3) Then
        Write(6,'( "In UT1I, UT1 module control flag is invalid. ",/,
     .  "  KUT1C =",I5,/)') KUT1C
        CALL CKILL(6HUT1I   ,0,0)
      Endif
C
C----------------------------------------------------------------------------
C  If using data base input, get the UT1 values from the data base now.
C   If using external EOP input, then we already have them.
C
      IF (.not. Input_EOP) THEN      !Data Base or External Input?
C
C     Call 'geti' to get the short period tidal terms existance flag from the
C     header and place it in ISHRTFL for other programs to use. If a database
C     interface error is detected, CALL CKILL to terminate the program.
C
      CALL GETI('TIDALUT1      ', ISHRTFL, 1, 1, 1, NDO, KERR(1))
C
      IF (KERR(1).NE.0) Then
        IF (KERR(1).EQ.1) WRITE(6,'("No UT1 type Lcode (TIDALUT1)!",
     .   " Update with true UT1 and try again. ")')
        CALL CKILL(6HUT1I  ,1,KERR(1))
      Endif
C
      If(ISHRTFL.ne.1 .and. ISHRTFL.ne.-2) Then
        Write(6,'(
     .  "In subroutine UT1I: The flag ISHRTFL from the database",/,
     .  "must have a value of 1 or -2. Its value is",I5)') ISHRTFL
        CALL CKILL(6HUT1I   ,0,0)
      Endif
C
C     Call 'Get4' to obtain the final UT1 information from the database. If
C     final information is unavailable, call 'get4' to obtain preliminary UT1
C     information. If preliminary information is unavailable, call 'get4' to
C     obtain predicted UT1 information. If database interface error, terminate.
C
      table_found = .false.
      If(.not.table_found) then
        Call GET4 ('FUT1 INF      ', UT1IF, 4, 1, 1, NDO, KERR(1) )
        If(KERR(1).eq.0)
     .  then
          type_found = 'F'
C         C_LUT1M(31:40) = ' Final Values       '
          do n = 1,10
           LUT1M(30+n) = LFI(n)
          enddo
          table_found = .true.
        Endif
      Endif
C
      If(.not.table_found) then
        CALL GET4 ('PUT1 INF      ', UT1IF, 4, 1, 1, NDO, KERR(1) )
        If(KERR(1).eq.0)
     .  then
          type_found = 'P'
C         C_LUT1M(31:40) = ' Preliminary Values '
          do n = 1,10
           LUT1M(30+n) = LPR(n)
          enddo
          table_found = .true.
        Endif
      Endif
C
      If(.not.table_found) then
        CALL GET4('EUT1 INF      ',UT1IF,4,1,1,NDO,KERR(1))
        If(KERR(1).eq.0)
     .  then
          type_found = 'X'
C         C_LUT1M(31:40) = ' Extrapolated Values'
          do n = 1,10
           LUT1M(30+n) = LEX(n)
          enddo
          table_found = .true.
        Endif
      Endif
C
      If(KERR(1).eq.2) then
       Write(6,'("In UT1I: UT1 information array has wrong size!")')
       CALL CKILL(6hUT1I  ,KERR(1),1)
      Endif
C
      IF (.not.table_found)
     .then
        Write(6,'(
     .  "In UT1I. This database contains NO UT1 information.",/,
     .  " Quitting!")')
        CALL CKILL(6HUT1I  ,0,0)
      Endif
C
      If(abs(UT1IF(4)-1.D0) .gt.  .00001) then
        Write(6,'(
     .  "In UT1I: The scaling law for UT1 table must be 1.0! ",/,
     .  "It is not.  Quitting!")')
        CALL CKILL(6hUT1I  ,0,0)
      Endif
C
      Increment = UT1IF(2) + .01
      Tab_len   = UT1IF(3) + .01
C
      If(Tab_len .gt. max_ut1_pts) then
        Write(6,'(
     .  "The maximum allowable UT1 table is ",i5," points.",/,
     .  "The table in the database contains ",i5," points.",
     .  "Quitting.")') max_ut1_pts, Tab_len
        CALL CKILL(6hUT1I  ,0,0)
      Endif
C
C  Get the EOP timescale definition. If not present, we call it undefined,
C   but assume the EOP table epochs to be in TDB (= CT).
      CALL GETA('EOPSCALE      ',L_EOPSCALE,4,1,1,NDO,KERR(1))
        If(KERR(1).ne.0)  C_EOPSCALE = 'UNDEF   '
       EOP_time_scale = C_EOPSCALE
C
C
      ELSE                           !Data base or External Input? 
C
      CALL PUTI('TIDALUT1      ',ISHRTFL,1,1,1)
      CALL PUT4('ROTEPOCH      ',ROTEPH,2,ndays,1)
        C_EOPSCALE = EOP_time_scale 
      CALL PUTA('EOPSCALE      ',L_EOPSCALE,4,1,1)
C
      Increment = UT1IF(2) + .01
      Tab_len   = UT1IF(3) + .01
C
C   Get leap seconds, then load TAI - UTC array.
       ierr = get_leapsec(xintv(1),xleap)
C      If (ierr .ne. 0) go to ????
       ATMUTC(1) = xleap(1)
       ATMUTC(2) = xleap(2)
       ATMUTC(3) = xleap(4)
       tol = 1.D-8
       If (Dabs(ATMUTC(3)).gt.tol) ATMUTC(1) = xleap(3) + 2400000.5D0
       ATMUTC(3) = ATMUTC(3) / 8.64D4
      CALL PUT4('TAI- UTC      ',ATMUTC,3,1,1)
C
C  Remove leap seconds if input table was UT1-UTC
C WEW - Leap_fix is defined (in cmxut.i) as Logical*4, so replace 
C       following line
C      If (Leap_fix .eq. 'True') Then
       If (Leap_fix) Then
        Do I = 1,Tab_len
         UT1PT(I) = UT1PT(I) + ATMUTC(2)
        Enddo
       Endif
C
       Do I = 1,3
        a1utc(i) = ATMUTC(i)
       enddo
        a1utc(2) = a1utc(2) + 0.03439D0
      CALL PUT4('A1 - UTC      ',a1utc,3,1,1)
C
       A1DIFF(1) = ATMUTC(1) 
       A1DIFF(2) =  0.03439D0
       A1DIFF(3) =  0.0D0
      CALL PUT4('A1 - TAI      ',A1DIFF,3,1,1)
C
      ENDIF                          !Data Base or External Input? 
C
C----------------------------------------------------------------------------
C
C  Determine interpolation method. Defaults are cubic spline for a 1-day series
C    and cubic polynomial for a 5-day (or any other) series. 
C
            Usespline = .false.
            Usecubic  = .false.
            Uselinear = .false.
C
      If (KUT1C.eq.0) Then
         if (Increment.eq.1) then 
            Usespline = .true. 
            Call PUTA('UT1INTRP      ',mess_spline,30,1,1)
         else
            Usecubic = .true. 
            Call PUTA('UT1INTRP      ',mess_cubic,30,1,1)
         endif
      Endif
C
      If (KUT1C.eq.3) Then
            Uselinear = .true. 
            Call PUTA('UT1INTRP      ',mess_linear,30,1,1)
      Endif
C
      If (KUT1C.eq.2) Then
         if (Increment.eq.1) then 
            Usecubic = .true. 
            Call PUTA('UT1INTRP      ',mess_cubic,30,1,1)
         else
            Usespline = .true. 
            Call PUTA('UT1INTRP      ',mess_spline,30,1,1)
         endif
      Endif
C
C----------------------------------------------------------------------------
      IF (.not. Input_EOP) THEN      !Data Base or External Input? 
C
       If(type_found.eq.'F')
     .   CALL GET4('FUT1 PTS      ',UT1PT,Tab_len,1,1,NDO,KERR(7))
       If(type_found.eq.'P')
     .   CALL GET4('PUT1 PTS      ',UT1PT,Tab_len,1,1,NDO,KERR(7))
       If(type_found.eq.'X')
     .   CALL GET4('EUT1 PTS      ',UT1PT,Tab_len,1,1,NDO,KERR(7))
       IF (KERR(7) .ne. 0) CALL CKILL(6hWOBI  ,7,KERR(7))
C
      ELSE                           !Data Base or External Input? 
C
       CALL PUT4('FUT1 INF      ',UT1IF,4,1,1)
       CALL PUT4('FUT1 PTS      ',UT1PT,Tab_len,1,1)
        If(ISHRTFL .eq.  1) utext(1:10) = 'UT1  from '
        If(ISHRTFL .eq. -2) utext(1:10) = 'UT1S from '
        utext(11:80) = Ex_EOP(1:70) 
       CALL PUTA('FUT1TEXT      ',lutext,40,1,1)
C
      ENDIF                          !Data Base or External Input? 
C----------------------------------------------------------------------------
C
C     PUT the UT1 type message into the database.
      CALL PUTA ('UT1 MESS      ', LUT1M, 40, 1, 1 )
C
C  Construct the table of TAI-UT1S or TAI-UT1R (note the sense), depending
C    on the value of KUT1C selected by the user.
C
C  Logic for TAI-UT1S tables (default)
      If(KUT1C.eq.0 .or. KUT1C.eq.3 .or. KUT1C.eq.2) then
C
       Do Itab = 1, Tab_len
        If(ISHRTFL .eq. -2) Then       !Already UT1S
          UT1RS(Itab) = UT1PT(Itab)
        Else
          ct  = 0.0d0     ! fraction of day
          xjd = UT1IF(1) + (Itab-1)*UT1IF(2)
	  CALL NUTFA  (xjd, ct, tc2000, fa, fad)
          CALL UT1SZT (fa, fad, DUT, DLOD, DOMEGA)
C
          if(ISHRTFL.eq.1) then       !starting with true UT1
            UT1RS(Itab) = UT1PT(Itab) + DUT
          endif
        Endif
       Enddo
C
C     Place the UT1 module flow control message into the database.
C     UT1 module on, Tidal terms from UT1S model
          apply_tidal_correction = .true.
          Call PUTA('UT1 CFLG      ',LUT1S,40,1,1)
C
      Endif
C
C ***************
C   Code for spline interpolation initialization, 93DEC08  -DG-
      If (Usespline) Then       ! Initialize spline routine
       Nspline = tab_len
C
       do ii=1,Nspline
        ya(ii) = UT1RS(ii)
       enddo
C
       do ii = 1, Nspline
        XT(ii) =  UT1IF(1) + (ii-1)*UT1IF(2)
       enddo
C
C   If interval (UT1IF(2)) not 1.0 days, then divide by interval ?????
       if ( Abs(UT1IF(2) - 1.D0) .gt. 1.D-10) then
         do ii=1,Nspline
          XT(ii) = XT(ii) / UT1IF(2)
         enddo
       endif
C
C   Take first derivatives at endpoints
       yp1 = (ya(2)-ya(1)) / UT1IF(2)     
       ypn = (ya(Nspline)-ya(Nspline-1))/ UT1IF(2)
C
C  call spline initialization subroutine
       call spline(XT,ya,Nspline,yp1,ypn,y2s,ierr4)
C
      Endif                      ! Initialize spline routine
C ***************
C
C   If UT1 module off:
      IF (KUT1C .EQ. 1) then
        CALL PUTA('UT1 CFLG      ',LOFF,40,1,1)
        apply_tidal_correction = .false.
      Endif
C
C     If there is a ROTEPH array already in the database, get it.
C     Otherwise bypass this step. Also, protect yourself in case
C     there are more than MXEPCH (currently 20) entries in ROTEPH.
C
C    ?????
        MEPOCH = ndays  
C    ?????
      IF (.not. Input_EOP) THEN   ! Already have ROTEPH?
        IF (ASKKER .NE. 0) GO TO 400
        MEPOCH = IEPOCH
        IF (MEPOCH .GT. MXEPCH) MEPOCH = MXEPCH
        CALL GET4('ROTEPOCH      ',ROTEPH,2,MEPOCH,1,NDO,KERR(8))
        IF (KERR(8) .NE. 0) CALL CKILL(6HUT1I  ,8,KERR(8))
      ENDIF                       ! Already have ROTEPH?
C
C     Compute TAI - UT1 and the short period tidal correction for the
C     desired epochs and place them in UT1TAB.
      DO  N=1,MEPOCH
        XJD = ROTEPH(1,N)
        CT  = ROTEPH(2,N)
        tab_time  = ROTEPH(2,N)
C
        if(KUT1C.ne.1) then
          CALL NUTFA (xjd, ct, tc2000, fa,fad)
C         CALL UT1MU (XJD,CT,fa,fad,tc2000,ATMUT1,SHORTP,DIVUTC)
          CALL UT1MU (XJD,tab_time,fa,fad,tc2000,ATMUT1,SHORTP,DIVUTC)
        else
          ATMUT1 = 0.D0
          SHORTP = 0.D0
        endif
C
        UT1TAB(1,N) = ATMUT1
        UT1TAB(2,N) = SHORTP
      ENDDO
C
C     PUT the UT1TAB array into the database.
      CALL PUT4('UT1EPOCH      ',UT1TAB,2,MEPOCH,1)
C
C     Go here if we are bypassing the updating of UT1TAB.
  400 CONTINUE
C
C     Check KUT1D for debug output.
    9 FORMAT (1X, "Debug output for subroutine UT1I.")
    7 FORMAT(A,15I8,/,(9X,15I8))
    8 FORMAT(A,4D25.16,/,(9X,4D25.16))
      IF ( KUT1D .ne. 0 ) Then
      WRITE ( 6, 9)
      WRITE(6,8)' UT1IF   ',UT1IF
      WRITE(6,8)' UT1PT   ',UT1PT
      WRITE(6,8)' UT1RS   ',UT1RS
      If (Usespline) write(6,8)' XT      ', XT
      If (Usespline) write(6,8)' ya      ', ya
      If (Usespline) write(6,8)' y2s     ', y2s
      If (Usespline) write(6,8)' yp1, ypn', yp1, ypn
      If (Usespline) write(6,7)' Nspline, ierr4 ', Nspline, ierr4 
      WRITE(6,8)' UT1TAB  ',UT1TAB
      WRITE(6,8)' tab_len ',tab_len
      WRITE(6,7)' IEPOCH  ',IEPOCH
      WRITE(6,7)' MEPOCH  ',MEPOCH
      WRITE(6,7)' MXEPCH  ',MXEPCH
      WRITE(6,7)' ASKKER  ',ASKKER
C
      ENDIF
C
C     Normal conclusion.
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE UT1G ( AT, DUTCAT, UTC, XJD, CT, fa, fad, tc2000,
     .           TSKIP, DUT1AT, UT1 )
      IMPLICIT None
C
C     UT1G is the UT1 module geometry section. It computes the instantaneous
C     offset between A1 and UT1 and the partial derivative of UT1 time with
C     respect to atomic time.
C
C     References - Ash,  M.E., 'Determination of Earth Satellite Orbits',
C     Lincoln Laboratory Technical Report 1972-5, 04/19/72, P. 42, 229-230.
C
C     Calling sequence -
C           Input variables:
C             1. AT     - The TAI fraction of the atomic time day. (days)
C             2. DUTCAT - The partial derivative of the UTC time with
C                         respect to the atomic time. (s/s)
C             3. UTC    - The UTC time fraction of the UTC day. (days)
C             4. XJD    - The Julian date at zero hours UTC of the date in
C                         question. (days)
C             5. CT     - The coordinate time fraction of the coordinate time
C                         day (days). [==> TDB]    
C             6. fa(5)  - The fundamental arguments (used in UT1MU). (arcsec)
C             7. fad(5) - The time derivatives of the fundamental arguments.
C                         (arcsec/century) 
C             8. tc2000 - Time in Julian centuries since Jan. 1.5, 2000.
C                         (centuries)
C             9. TSKIP  - If 1, skip recomputations.
C
C           Output variables:
C             1. UT1    - The UT1 time of the day. (s)
C             2. DUT1AT - The partial derivative of the UT1 time with
C                         respect to atomic time. (s/s)
        Real*8 XJD, CT, fa(5), fad(5), tc2000, ATMUT1,SHORTP,DIVUTC
        Real*8 AT, DUTCAT, UTC, DUT1AT, UT1, tab_time 
        Integer*4 TSKIP
C
C    Common blocks used -
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C            Variables 'from':
C              1. SECDAY     -  The number of seconds in a day. (s/day)
C
      INCLUDE 'cmxut.i'
C            Variables 'from':
C             1. UT1IF(4)   -  The UT1 information array. This array
C                              contains respectively: 1) The Julian
C                              date of the first tabular point,
C                              2) The increment in days of the tabular
C                              points, 3) The number of tabular points,
C                              4) The units of the UT1 tabular array per 
C                              second. (days, days, unitless, s/table unit)
C             2. UT1PT(20)  -  The tabular values of the 'TAI - UT1' offset.
C             3. Y1(2)      -  An array used in the calculation of DIVUTC.
C             4. Y2(1)      -  An array used in the calculation of DIVUTC.
C             5. T          -  A time used in the calculation of DIVUTC.
C             6. S          -  Used in the calculation of DIVUTC.
C             7. UT1RS      -  Table of UT1S or UT1R (depending on KUT1C
C                              flag) values corresponding to UT1PT.
C             8. EOP_time_scale - EOP table time scale, allowed values:
C                              'TAI     ', 'TCG     ', 'TDB     ',
C                              'TDT     ', 'UTC     ', 'UNDEF   '.
C
      INCLUDE 'ccon.i'
C           Variables 'from':
C             1. KUT1C - UT1 module flow control flag, controls the 
C                        temporary removal of periodic terms (UT1S or UT1R)
C                        and the type of interpolation (spline, cubic, or
C                        linear) in the UT1 tables.
C                         = 0. Convert table to UT1S (logic controled by 
C                            ISHRTFL); do spline interpolation for a 1-day
C                            series, or cubic interpolation for a 5-day series;
C                            then restore to true UT1 using the new UT1S model.
C                         = 1. Module completely off, that is, UT1 set equal
C                            to AT.
C                         = 2. Use UT1S; use cubic interpolation for a 1-day
C                            series, or spline interpolation for a 5-day
C                            series. 
C                         = 3. Use UT1S, use linear interpolation for both
C                            1-day and 5-day series.
C             2. KUT1D - The module debug output flag.
C
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
C           VARIABLES 'TO':
C            1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
C                           CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
C                           RATE OF CHANGE OF 'TAI MINUS UTC'. Used in the 
C                           atomic time module. (DAYS, SEC, SEC/SEC)
C
C     Database access -
C           Variables 'PUT':
C              1. UT1 - The UT1 time of the day. (s)
C
C     Subroutine interface -
C             Caller subroutines: DRIVG
C             Called subroutines: UT1MU, PUT4
C
C     Program variables -
C           1. ATMUT1 -  The interpolated value of 'TAI - UT1'. (s)
C           2. DIVUTC -  The partial derivative of 'TAI MINUS UT1'
C                        with respect to UTC.  (SEC/SEC)
C           3. SHORTP -  UT1S-UT1 or UT1R-UT1 (s).  Notice the sense.
C                        The short period correction to ATMUT1. (sec)
C
       SAVE  ATMUT1
C
C     Programmer - Dale Markham   02/14/77
C      77.07.14: Peter Denatale
C      78.02.21: Bruce Schupler
C      88.12.21: Gregg Cooke   Initial CALC 7.4 mods.
C      89.07.11: Jim Ryan      Documentation simplified.
C      89.12.12: Jim Ryan      UNIX-like database interface implimented.
C      90.01.02: Jim Ryan      UT1-TAI stored rather than reverse.
C      90.06.19: Jim Ryan      Logic to always interpolate with UT1R added.
C      93:09:07: Norbert Zacharias Use fundam. arg. from DRIVR to UT1G to UT1MU.
C      93:03:17: David Gordon  UT1S added as default, new control flag scheme.
C      94.04.06: David Gordon  Changed to 'Implicit None'. 
C      94.09.27: David Gordon  Removed unused 'XLOVEK' Love number.
C      98.01.27: David Gordon  Removed UT1R references. 
C      98.02.09: David Gordon  Code to skip repeat computations.
C      98.04.13: David Gordon  Common /UT1CM/ moved to 'cmxut.i' include file. 
C      98.11.05: David Gordon  SAVE block added to save ATMUT1. 
C     2001.01.02 David Gordon  Modified to get table scale from EOP_time_scale.
C                              Tab_time used in place of CT in UT1MU argument.
C
C     UT1G Program structure
C
C     Obtain ATMUT1.
C
      IF (TSKIP .EQ. 1) GO TO 101
C
C  Determine time scale of EOP table epochs. CT (=TDB) is the default.
         tab_time = CT 
       If (EOP_time_scale .eq. 'UTC     ') tab_time = UTC
       If (EOP_time_scale .eq. 'TAI     ') tab_time = UTC + 
     *       ATMUTC(2)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
       If (EOP_time_scale .eq. 'TDT     ') tab_time = UTC + 
     *      (ATMUTC(2) + 32.184D0)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
C
      If(KUT1C.ne.1) then
C       CALL UT1MU (XJD, CT, fa, fad, tc2000, ATMUT1,SHORTP,DIVUTC)
        CALL UT1MU (XJD,tab_time,fa,fad,tc2000,ATMUT1,SHORTP,DIVUTC)
C  Compute the UT1 time of the day.
        UT1 =  AT * SECDAY - ATMUT1
C  Compute the partial derivative of UT1 time with respect to atomic time.
        DUT1AT = 1.D0  -  DIVUTC * DUTCAT
      else
        UT1    = AT * SECDAY
        DUT1AT = 1.D0
        ATMUT1 = 0.D0
      endif
C
 101  CONTINUE
C     PUT the UT1 value into the database for this observation.
C     NOTE: This is UT1-TAI, not the opposite sense.
      CALL PUT4( 'UT1 -TAI      ', -ATMUT1, 1, 1, 1 )
C
C     Check KUT1D for debug output.
      IF ( KUT1D .ne. 0 )  Then
      WRITE ( 6, 9)
    9 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE UT1G." )
      WRITE(6,8)' ATMUT1  ',ATMUT1
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DIVUTC  ',DIVUTC
    7 FORMAT(A,15I8/(9X,15I8))
      WRITE(6,8)' SECDAY  ',SECDAY
C
      WRITE ( 6, 9200 )  AT, DUTCAT, UTC, XJD, UT1, DUT1AT
 9200 FORMAT (1X, "AT     = ", D30.16, /, 1X,
     1            "DUTCAT = ", D30.16, /, 1X,
     2            "UTC    = ", D30.16, /, 1X,
     3            "XJD    = ", D30.16, /, 1X,
     4            "UT1    = ", D30.16, /, 1X,
     5            "DUT1AT = ", D30.16, /, 1X)
      Endif
C
C   7.    NORMAL PROGRAM CONCLUSION.
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE UT1P ( CFBASE, DIURNV, GAST, RDNP, RN, RP, RW, STAR,
     .                  EARTH, DUT1P)
      IMPLICIT None
C
C     UT1P is the UT1 module partial derivatives section. It computes the
C     partial derivatives of the delay and the delay rate with respect to the
C     instantaneous 'TAI - UT1'.
C
C     Calling sequence
C           Input variables:
C             1. CFBASE(3)  -  The crust fixed baseline vector.  (m)
C             2. DIURNV     -  The diurnal angular velocity of the Earth.
C                              (radians/s)
C             3. GAST(2)    -  The Greenwich apparent sidereal time and its
C                              CT time derivative. (rad, rad/s)
C             4. RDNP(3,3)  -  The diurnal polar motion portion of the complete
C                              crust fixed to J2000.0 rotation matrix.
C             5. RN(3,3,2)  -  The nutation portion of the complete crust fixed
C                              to J2000.0 rotation matrix and the CT time
C                              derivative of that matrix. (unitless, 1/s)
C             6. RP(3,3,2)  -  The precession portion of the complete crust
C                              fixed to J2000.0 rotation matrix and its CT time
C                              derivative (unitless,1/s).
C             7. RW(3,3,2)  -  The wobble portion of the complete crust fixed
C                              to J2000.0 rotation matrix and its first time
C                              derivative. (unitless, 1/sec)
C             8. STAR(3)    -  The J2000.0 source unit vector.
C             9. EARTH(3,3) -  The SSBC position, velocity, and acceleration of
C                              the Earth. (m, m/s, m/s**2)
C
C           Output variables:
C             1. DUT1P(2,2) -  See below, PUT to database, also required in
C                              calling program DRIVR to pass on to EQEC routine.
C
      Real*8 CFBASE(3), DIURNV, GAST(2), RDNP(3,3), RN(3,3,2),
     1       RP(3,3,2), RW(3,3,2), STAR(3), EARTH(3,3), DUT1P(2,2)
C
C     Common blocks used -
C
      INCLUDE 'cphys.i'
C            Variables 'from':
C              1. VLIGHT  -  The defined speed of light. (m/s)
C
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C             1. KUT1C  -  UT1 module flow control flag, controls the 
C                          temporary removal of periodic terms (UT1S or UT1R)
C                          and the type of interpolation (spline, cubic, or
C                          linear) in the UT1 tables.
C                          = 0. Convert table to UT1S (logic controled by 
C                             ISHRTFL); do spline interpolation for a 1-day
C                             series, or cubic interpolation for a 5-day series;
C                             then restore to true UT1 using the new UT1S model.
C                          = 1. Module completely off, that is, UT1 set equal
C                             to AT.
C                          = 2. Use UT1S; use cubic interpolation for a 1-day
C                             series, or spline interpolation for a 5-day
C                             series. 
C                          = 3. Use UT1S, use linear interpolation for both
C                             1-day and 5-day series.
C              2. KUT1D  -  The UT1 module debug output flag.
C
C     Program specifications -
C
      Real*8 DSDUT1(3,3,4), SBASE(3,2,2), SR2000(3,3,4), c1, c2, tt,
     *       vg(3), Dotp
      Integer*4 I, J, K
C
C    Database access  -
C           'PUT' variables:
C              1. DUT1P(2,2)  -  The partial derivatives of the delay and rate
C                                with respect to the instantaneous value of
C                                'A1-UT1'. (s/s, s/s**2) The second index runs
C                                over the first and second derivatives.
C
C     Subroutine interface -
C             Caller subroutines: DRIVP
C             Called subroutines: DDROT, DOTP, DROTT, MMUL5, PUT4, VECRT
C
C     Program variables -
C          1. DSDUT1(3,3,4)  -  The partial derivatives of the diurnal spin
C                               matrix and its time derivative with respect to
C                               the value of 'A1-UT1'. (1/s, 1/s**2 )
C                               A final index value of 1 or 2 applies to the
C                               first partial. A final index of 3 or 4 applies
C                               to the second derivative. In CALC on the 360
C                               this array was 4 dimensional. Unfortunatley, we
C                               could not have 4 dimensional arrays on the
C                               HP000 (okay in HP-UX?).
C          2. SBASE(3,2,2)   -  The partial derivatives of the J2000.0 baseline
C                               position and velocity vectors with respect to 
C                               the instantaneous value of 'A1-UT1'.
C                               (m/s,m/s**2). The final index counts the first
C                               and second partials.
C          3. SR2000(3,3,4)  -  The partial derivative of the complete crust
C                               fixed to J2000.0 rotation matrix and its
C                               derivative with respect to the instantaneous
C                               value of 'A1-UT1'. (1/s, 1/s**2)
C                               A final index value of 1 or 2 applies to the 
C                               first derivative. A final index value of 3 or 4
C                               applies to the second derivative.
C          4. c1, c2, tt,vg  -  Dummy variables used in the computation of the
C                               partials.
C
C     Programmer - Dale Markham   02/14/77
C      77.07.14  Peter Denatale
C      78.03.28  Bruce Schupler
C      80.07.11  Bruce Schupler
C      89.07.11  Jim Ryan      Documentation simplified.
C      89.10.05  Jim Ryan      CPHYS common made an include file.
C      89.12.12  Jim Ryan      UNIX-like database interface implemented.
C      91.05.30  Jim Ryan      Documentation simplified.
C      91.11.25  Jim Ryan      Second term in the Shapiro model added
C                              to the partials.
C      93.08.17  Norbert Zacharias  Output variable DUT1P added to arg.list
C      94.04.06  David Gordon  Changed to 'Implicit None'. 
C      95.12.11  David Gordon  Changed RW(3,3) to RW(3,3,2).
C      98.01.27  David Gordon  Removed UT1R references. 
C
C     UT1P Program Structure
C
C     Construct the partial derivatives of the diurnal spin matrix
C     with respect to the instantaneous offset between A1 and UT1.
C
      CALL DROTT ( -GAST(1), DIURNV, 3, DSDUT1(1,1,1) )
      CALL DDROT(-GAST(1),+DIURNV*DIURNV,3,DSDUT1(1,1,3))
C
C     Construct the partial derivatives of the first CT time derivative of the
C     diurnal spin matrix with respect to the instantaneous offset between A1
C     and UT1. (NOTE: To a very good approximation the first partial is just the
C     second CT time derivative of the diurnal spin matrix.)
C
      CALL DDROT ( -GAST(1), -DIURNV*GAST(2), 3, DSDUT1(1,1,2) )
      CALL D3ROT(-GAST(1),-DIURNV*DIURNV*GAST(2),3,DSDUT1(1,1,4))
C
C     Compute the partial derivatives of the complete crust fixed to J2000.0
C     rotation matrix and of its first CT time derivative with respect to the 
C     instantaneous offset between A1 and UT1. (NOTE: Of the three terms which
C     are used to compute the CT time derivative of the complete crust fixed to
C     J2000.0 rotation matrix, only the term which contains the CT time
C     derivative of the diurnal spin matrix is considered significant enough to
C     include in this partial derivatives section.)
C
      DO 310 J=1,2
        DO 300  K = 1,2
          CALL MMUL5(RP(1,1,1),RN(1,1,1),DSDUT1(1,1,(K+2*(J-1))),RDNP,
     1         RW(1,1,1),SR2000(1,1,(K+2*(J-1))))
  300   CONTINUE
  310 CONTINUE
C
C   Compute the partial derivatives of the J2000 baseline position and velocity 
C   vectors with respect to the instantaneous offset between A1 and UT1.
C
      DO 410 J=1,2
        DO 400  K = 1,2
          CALL VECRT ( SR2000(1,1,(K+2*(J-1))), CFBASE, SBASE(1,K,J) )
  400   CONTINUE
  410 CONTINUE
C
C   Compute the partial derivatives of the delay and the delay rate with
C   respect to the instantaneous offset between A1 and UT1. Only the first
C   two terms in the Shapiro model are included.
C
      Do i = 1,3
        vg(i) = EARTH(i,2)
      Enddo
      c1 = 1.d0/VLIGHT
      c2 = c1**2
      tt = 1.d0 - c1*Dotp(STAR,vg)
      DUT1P(1,1)=c1*DOTP(SBASE(1,1,1),STAR)*tt+c2*Dotp(SBASE(1,1,1),vg)
      DUT1P(1,2)=c1*DOTP(SBASE(1,1,2),STAR)*tt+c2*Dotp(SBASE(1,1,2),vg)
      DUT1P(2,1)=c1*DOTP(SBASE(1,2,1),STAR)*tt+c2*Dotp(SBASE(1,2,1),vg)
      DUT1P(2,2)=c1*DOTP(SBASE(1,2,2),STAR)*tt+c2*Dotp(SBASE(1,2,2),vg)
C
C   PUT the derivatives in the database.
      CALL PUT4 ('UT1 PART      ', DUT1P, 2, 2, 1 )
C
C   Check KUT1D for debug output.
      IF ( KUT1D .ne. 0 )  Then
      WRITE ( 6, 9)
    9 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE UT1P." )
      WRITE(6,8)' DSDUT1  ',DSDUT1
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DUT1P   ',DUT1P
      WRITE(6,8)' SBASE   ',SBASE
      WRITE(6,8)' SR2000  ',SR2000
      WRITE(6,8)' VLIGHT  ',VLIGHT
      WRITE(6,8)' c1      ',c1
      WRITE(6,8)' c2      ',c2
      WRITE(6,8)' tt      ',tt
      WRITE(6,8)' vg      ',vg
C
      WRITE ( 6, 9200 )CFBASE,DIURNV,GAST,RDNP,RN,RP,RW,STAR
 9200 FORMAT (1X, "CFBASE = ", 3 ( D30.16, 10X ), /, 1X,
     1            "DIURNV = ", D30.16, /, 1X,
     2            "GAST   = ", 2 ( D30.16, 10X ), /, 1X,
     3            "RDNP   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "RN     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     5            "RP     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     6            "RW     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     7            "STAR   = ", 3 ( D30.16, 10X ) )
      Endif
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
C     SUBROUTINE UT1MU (XJD,CT,fa, fad, tc2000, ATMUT1,SHORTP,DIVUTC)
      SUBROUTINE UT1MU (XJD,tab_time,fa,fad,tc2000,ATMUT1,SHORTP,DIVUTC)
      IMPLICIT None
C
C     UT1MU is the UT1 module utility. It calculates the difference between AT
C     and UT1 to the best precision that our model allows. It also returns
C     separately the tidal correction to the difference.
C
C     References -
C           ASH, M.E., "Determination of Earth Satellite Orbits", Lincoln
C           Laboratory Technical Report 1972-5, 04/19/1972, P. 42, 229-230.
C
C     Real*8 XJD, CT, fa(5), fad(5), tc2000, ATMUT1, SHORTP, DIVUTC
      Real*8 XJD,     fa(5), fad(5), tc2000, ATMUT1, SHORTP, DIVUTC,
     *       tab_time
C
C        Input variables:
C         1. XJD   - The Julian date at zero hours UTC of the day in question.
C         2. tab_time - Fraction of the day in the time scale of the input 
C                    EOP table (CT, UTC, or ???)
C        [2. CT    - The fraction of the CT day since zero hours CT to the time
C                    in question (days).]
C         3. fa(5) - The fundamental arguments (as of NUTFA from file cnutm.f).
C                    (arcseconds)
C         4. fad(5)- The time derivatives of the fundamental arguments fa(5).
C                    (arcseconds/century) 
C         5. TC2000- Time in Julian centuries since J2000.
C
C        Output variables:
C         1. ATMUT1 - The interpolated value of 'TAI - UT1' (s).
C         2. SHORTP - 'UT1S-UT1' or 'UT1R-UT1' depending on KUT1C. Note sense.
C         3. DIVUTC - The partial derivative of 'TAI MINUS UT1' with respect
C                     to UTC. (SEC/SEC)
C
C     Common block used  -
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C          Variables 'from':
C            1.  SECDAY  -  The number of seconds in a day. (s/day)
C            2.  TWOPI   -  PI times 2.0D0
C
      Integer*4 ierr4, NN
      Real*8 x_spline, y_spline, ydot, ydot2, ydot3
C
      INCLUDE 'cmxut.i'
C      Variables 'from':
C        1. UT1IF -  The UT1 infomation array. (See above.)
C        2. UT1PT -  The tabular values of 'TAI - UT1'. (See above)
C        3. CENTJ -  The number of Julian days per Julian century. (days)
C        4. DJ1900 - The Julian date of January 1.5, 1900 (days).
C        5. DJ2000 - The Julian date of January 1.5, 2000 (days).
C        6. SERSHO(4) - The coefficients appearing in the calculation of SHORTP
C                    (See ref 2) (s).
C        7. ISHRTFL  - The short period tidal terms flag, (unitless).
C                    = 0 --> UT1 table coming from input db is true UT1, 
C                    (fortnightly tidal terms have not been removed).
C                    = -1 --> UT1 table coming from input db is UT1R, (Yoder
C                    fortnightly tidal terms HAVE been removed as in Bull. B).
C                    = -2 --> UT1 table coming from input db is UT1S, (that is,
C                    the S tidal terms HAVE been removed)
C        8. Usecubic - Set to true if cubic interpolation is to be used. False
C                    otherwise.
C        9. Uselinear - Set to true if linear interpolation is to be used.
C                    False otherwise.
C       10. Usespline - Set to true if spline interpolation is to be used.
C                    False otherwise.
C       11. UT1RS  - The table of UT1S or UT1R values for interpolation. (sec)
C
C      Variables 'to':
C        1. Y1(2) - A variable used in interpolation.
C        2. Y2(2) - Same.
C        3. T     - Same.
C        4. S     - Same.
C
      INCLUDE 'ccon.i'
C       Variables 'from':
C             1. KUT1C  -  UT1 module flow control flag, controls the 
C                          temporary removal of periodic terms (UT1S or UT1R)
C                          and the type of interpolation (spline, cubic, or
C                          linear) in the UT1 tables.
C                          = 0. Convert table to UT1S (logic controled by 
C                             ISHRTFL); do spline interpolation for a 1-day
C                             series, or cubic interpolation for a 5-day series;
C                             then restore to true UT1 using the new UT1S model.
C                          = 1. Module completely off, that is, UT1 set equal
C                             to AT.
C                          = 2. Use UT1S; use cubic interpolation for a 1-day
C                             series, or spline interpolation for a 5-day
C                             series. 
C                          = 3. Use UT1S, use linear interpolation for both
C                             1-day and 5-day series.
C             2. KUT1D - The UT1 module debug control flag.
C
C     Program specifications
C
      Real*8 XINT(4), Dut, Dlod, Domega, Shortp_dot, F2
      Real*8 DUT_DOT, DUT1
C WEW - NN has already been defined, so replace following line
C     Integer*4 INT, ILAST, N, NN, NR
      Integer*4 INT, ILAST, N, NR
C
C     Subroutine interface
C        Caller subroutines: UT1G
C        Called subroutines: DFLOTJ,IDINT,UT1RZT,splint4
C
C     Program variables:
C        1. XINT(4) - An array used in the interpolation procedure.
C
C     Programmer - Bruce Schupler 02/21/78
C      78.07.03  Bruce Schupler
C      85.04.13  David Gordon
C      89.07.11  Jim Ryan  Documentation simplified.
C      91.05.30  Jim Ryan  Documentation simplified furthur.
C      91.06.05  Jim Ryan  Interpolation modified to use linear interpolation.
C                          if the series is a 'one day' series.
C      91.06.19  Jim Ryan  Logic modified to always interpolate in the UT1R
C                          values. Important! Logic for computing the Ut1 time
C                          derivative moved here from UT1G (where it did not
C                          belong), and the effect of tidal terms on that
C                          derivative added.
C      91.11.05  Jim Ryan  SHORTP_DOT added to the computation of UT1 rate for
C                          linear interpolation. It was left out when linear
C                          was added.
C      93.03.17  D. Gordon Interpolation in UT1S added, new control flag scheme
C      93.09.07  Norbert Zacharias  Handle fundamental arguments, fa.
C      93 Dec.   D. Gordon Cubic spline interpolation added, modified control
C                          flag scheme for type of interpolation.
C      94.04.06  D. Gordon Changed to 'Implicit None'. 
C      94.04.13  D. Gordon DFLOT changed to Fortran 77 DFLOTJ.
C      94.09.26  D. Gordon Added some debug printout, documentation 
C                          corrections, cosmetic mods. 
C      94.09.27  D. Gordon Removed unused 'XLOVEK' Love number.
C      98.01.21  D. Gordon Adding fad(5) to UT1RZT and UT1SZT. 
C      98.01.27  D. Gordon Removing call to UT1RZT, and UT1R option. 
C      98.04.13  D. Gordon Common /UT1CM/ moved to 'cmxut.i' include file. 
C     20001.01.02 D.Gordon CT replaced with tab_time as input argument.
C
C     UT1MU Program Structure
C
C     Convert tab_time and XJD to the units of the interval of the 
C     table relative to
C     the first point of the table. INT is defined so that INT+2 is the number
C     of the point in the UT1 table immediately before (or at) the time of the
C     observation. (INT+2 is before and INT+3 is after the observation.)
C
      T = XJD  -  UT1IF(1)
C     T = ( T + CT ) / UT1IF(2)
      T = ( T + tab_time ) / UT1IF(2)
      INT = T
C WEW -  DFLOTJ not available in G77 - try
C     T = T - DFLOTJ ( INT )
      T = T - INT
      INT = INT - 1
C
        CALL UT1SZT (fa, fad, DUT, DLOD, DOMEGA)
c        write(6,1023) DUT,DLOD,DOMEGA
c1023    format('UT1SZT:   DUT,DLOD,DOMEGA = ', 3D25.16)
C
        SHORTP = -DUT
C    Convert rate of change from rad/sec to sec/sec
        SHORTP_DOT = -(DOMEGA/TWOPI)*SECDAY
C
C  Begin interpolation:
C
C***************************************
      IF (Usespline) then                      !Cubic spline interpolation
C   Compute time of obs. and divide by interval 
C       x_spline = (xjd + ct) / UT1IF(2)
        x_spline = (xjd + tab_time) / UT1IF(2)
C
C   Do the spline interpolation
        call splint4(XT,ya,y2s,Nspline,x_spline,y_spline,ydot,ydot2,
     *               ydot3,ierr4)
C
        ATMUT1 = y_spline + SHORTP
        DIVUTC = ydot / (UT1IF(2) * SECDAY) + SHORTP_DOT
C
      Endif                                     !Cubic spline interpolation
C
C***************************************
      IF (Usecubic) then                    !Four point cubic interpolation
C       Select the required four tabular points and verify that the
C       interpolation is not outside the range of the table.
C       (NOTE: If the interpolation is outside the range of the table,
C       a message is written and the program terminates.)
C
        ILAST = IDINT ( UT1IF(3) )
        DO  N = 1,4
          NN = INT  +  N
          IF ( ( NN .LT. 1 ) .OR. ( NN .GT. ILAST ) )  GO TO 1000
          XINT(N) = UT1RS(NN)
        ENDDO
C
C   Interpolate to determine the value of the 'TAI minus UT1' offset.
        DO  N = 1,2
          NR = N  +  1
          F2 = ( XINT (NR+1)  +  XINT (NR-1) ) / 6.D0
          Y1(N) = + ( 4.D0 / 3.D0 ) * XINT (NR)  -  F2
          Y2(N) = - ( 1.D0 / 3.D0 ) * XINT (NR)  +  F2
        ENDDO
C
        S = 1.D0  -  T
        ATMUT1 =
     .     (  T * ( Y1(2)  +  T**2 * Y2(2) )
     .      + S * ( Y1(1)  +  S**2 * Y2(1) ) )
     .   + SHORTP
C
C   Compute the partial derivative of the 'IAT MINUS UT1' offset with respect
C    to UTC.
        DIVUTC =
     .  ((  Y1(2)+3.d0*T**2*Y2(2)
     .     -Y1(1)-3.d0*S**2*Y2(1) )/
     .    ( UT1IF(2) * SECDAY )     )
     .  + SHORTP_DOT
C
      ENDIF                                  !Four point cubic interpolation
C
C********************************************
      IF (Uselinear) then                    !Two point linear interpolation.
C
        If( INT+2 .lt. 1     .or.
     .      INT+3 .gt. DINT(UT1IF(3)+.0001)) then
          Write(6,'(
     .    "Error in UT1MU! Attemped to interpolate outside UT1 table.",/
     .    ,"INT =",I5," Table length =",i5)') INT, UT1IF(3)
          CALL CKILL(6hUT1MU ,0,0)
        Endif
C
        ATMUT1 =
     .  ( (UT1RS(INT+3)-UT1RS(INT+2))*T+UT1RS(INT+2) ) +
     .  SHORTP
C
        DIVUTC = (UT1RS(INT+3)-UT1RS(INT+2))/(UT1IF(2)*SECDAY)
     .  +SHORTP_DOT
C
      ENDIF                                    !Two point linear interpolation.
C
C******************************************************************************
C     Check for debug output.
      IF (KUT1D .ne. 0) THEN
        WRITE(6,'(" Debug output for subroutine UT1MU")')
          WRITE(6,88)' SHORTP, SHORTP_DOT ', SHORTP, SHORTP_DOT
 88       FORMAT(A,4D25.16/(9X,4D25.16))
 89       FORMAT(A,4I16/(9X,4I16))
          if(Usespline) write(6,88)' XT      ', XT
          if(Usespline) write(6,88)' ya      ', ya
          if(Usespline) write(6,88)' y2s     ', y2s
          if(Usespline) write(6,88)' x_spline,y_spline', x_spline,
     .                              y_spline
          if(Usespline) write(6,88)' ydot,ydot2,ydot3 ', ydot,ydot2,
     .                              ydot3
          if(Usespline) write(6,88)' Nspline, ierr4 ',Nspline,ierr4
          WRITE(6,89)' INT     ',INT
          WRITE(6,88)' T       ',T
          WRITE(6,88)' ATMUT1  ',ATMUT1
          WRITE(6,88)' DIVUTC  ',DIVUTC
          WRITE(6,88)' UT1IF   ',UT1IF
          WRITE(6,88)' UT1RS   ',UT1RS
C
C       WRITE(6,9200) XJD,CT,ATMUT1,SHORTP,DJ1900,CENTJ,DJ2000,TC2000
        WRITE(6,9200) XJD,tab_time,ATMUT1,SHORTP,DJ1900,CENTJ,
     *                DJ2000,TC2000
 9200   FORMAT(1X,"XJD = ",D30.16,/,1X,"tab_time = ",D30.16,/,
     .       1X,"ATMUT1 = ",D30.16,/,1X,"SHORTP = ",D30.16,/,
     .       1X,"DJ1900 = ",D30.16,/,1X,"CENTJ = ",D30.16,/,
     .       1X,"DJ2000 = ",D30.16,/,1X,"TC2000 = ",D30.16)
        ENDIF
C
C     Normal conclusion.
      RETURN
C
C     Abnormal termination.
 1000 CONTINUE
      WRITE(6,'(" CALC has terminated in subroutine UT1MU.",
     .       /" The interpolation is outside the range of the UT1",
     .       " table.  NN = ",I2," ILAST = ",I2," .")') NN, ILAST
      CALL CKILL (6HUT1MU ,0,0)
      END
C
C******************************************************************************
      BLOCK DATA UT1CMB
      IMPLICIT None
C
C     UT1BD is the UT1 module block data input and initialization section.
C
C     References 1) Ash, M.E., 'Determination of Earth Satellite Orbits",
C                   Lincoln Laboratory Technical Report 1972-5, 04/19/72, P. 42.
C                2  American Ephemeris and Nautical Almanac.
C                3) Melchior,"The Earth Tides".
C
      INCLUDE 'cmxut.i'
C            Variables 'to':
C              1. CENTJ   -   The number of Julian days per Julian century.
C              2. DJ1900  -   The Julian date of Jan 1.5, 1900 (days).
C              3. DJ2000  -   The Julian date of Jan 1.5, 2000 (days).
C              4. SERSHO(4) - The coefficients appearing in the series for the
C                             short period corrections to UT1 (See ref 4) (s).
C
C            Variables 'passed' from other module sections:
C              1. UT1IF(4)  -  The UT1 information array. (See above.)
C              2. UT1PT(20) -  The tabular values of 'IAT MINUS UT1'.
C
      DATA DJ1900, CENTJ / 2415020.0D0,36525.0D0/
      DATA DJ2000/2451545.0D0/
      DATA SERSHO /0.00247D0,0.00102D0,0.00263D0,0.00058D0/
C
C    Constants used - DJ1900, CENTJ
C
C    Programmer - Dale Markham  02/14/77
C     77.07.14  Peter Denatale
C     78.02.21  Bruce Schuler
C     84.04.13  David Gordon
C     84.06.06  David Gordon
C     84.06.03  Savita Goel   CDS for A900.
C     94.04.06  David Gordon  Changed to 'Implicit None'. 
C     94.09.27  David Gordon  Removed unused 'XLOVEK' Love number.
C     98.01.27  David Gordon  Removed Yoder coefficients.
C     98.04.13  David Gordon  Common /UT1CM/ moved to 'cmxut.i' include file. 
C
      END
C
C*****************************************************************************
      SUBROUTINE UT1SZT (fa, fad, DUT, DLOD, DOMEGA)
      IMPLICIT None 
C
C     Purpose: This subroutine evaluates the effects of zonal Earth tides on
C     the rotation of the Earth. The model used is from Yoder, Williams, and
C     Park (1981) and modified by the ocean effects as given in Dickman (1991)
C     as recommended by the IERS Standards, p. 117, 119-120 (1992).
C
C     Special Note: Under the UT1S definition, and as done by this routine,
C     zonal tides of _all_ periods are evaluated, including even those of
C     18.6 year period. Results will be substantially different (tenths of
C     seconds) from those evaluated with the "UT1R" algorithm, which only
C     includes the 41 terms for periods under 35 days. If you wish to determine
C     the effects from only those periods, please use the original Luzum
C     "zontids" routine, with N set to 41.  (B.A.)
C
C     Input Variables:
C          fa(5)  = Fundamental arguments from subroutine NUTFA (arcseconds)
C          fad(5) = Time derivatives of fundamental arguments (arcsec/century)
C     Output Variables:
C          DUT    - 'UT1 minus UT1S'. Effect on UT (Subtract from observation,
C                   add to IERS UT1). (sec)
C          DLOD   = Effect on length of day. (seconds).
C          DOMEGA = Effect on rotational speed (radians/second).
C
C     Written by:
C       Brian J. Luzum     92.08.11
C     Modifications:
C       Brent A. Archinal  92.08.27  Name changed from zontids to
C                                    ut1szt, N dropped from argument
C                                    list, comments improved.
C       "     "  "         92.10.27  Special note added above.
C       "     "  "         92.12.17  All real variables set to double
C                                    precision, at Jim Ray's suggestion
C                                    (email of 92.11.20).
C       David Gordon       93.03.17  Array X changed to XS, debug printout
C                                    added for calc 8.0.
C       Norbert Zacharias  93.09.16  Take fundam. arg. from subr. NUTFA
C       David Gordon       94.04.06  Changed to 'Implicit None'. 
C       David Gordon       98.01.21  Extensive mods from John Gipson to use
C                                    the series expansion of DUT to calculate
C                                    DUT, DLOD, and DOMEGA instead of seperate
C                                    series expansions. fad(5) added to 
C                                    subroutine call. DBLE's removed. CMATH
C                                    common block added. Variable SECCON 
C                                    removed and replaced with 1/CONVDS. Sign
C                                    of X(7,62) corrected. 
C
      REAL*8 T, DUT, DLOD, DOMEGA, F, D, OM, ARG
      REAL*8 L, LP, fa(5), fad(5), ARG_DOT
      Integer*4 N, I
      REAL*8 XS(11,62), X1(220), X2(220), X3(220), X4(22)
      EQUIVALENCE(XS(1,  1),X1(1))
      EQUIVALENCE(XS(1, 21),X2(1))
      EQUIVALENCE(XS(1, 41),X3(1))
      EQUIVALENCE(XS(1, 61),X4(1))
C
      INCLUDE 'ccon.i'
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C          Variables 'from':
C            1. SECDAY -  The number of seconds in a day. (s/day)
C            2. TWOPI  -  PI times 2.0D0
C            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                 (RAD/ARCSECOND)
C
C  N=Number of tidal terms to be used (62 for full set).
C  (This was an argument in the original zontids routine.)
C
      DATA  N/62/
C
C***********************************************************************
C     Table of multiples of arguments and coefficients
C      (DLOD and DOMEGA tables no longer used, 98JAN21 -DG-)
C 
C                  Multiple of            DUT        DLOD      DOMEGA
C             l   l'  F   D OMEGA     sin   cos    cos  sin   cos   sin
      DATA X1/1., 0., 2., 2., 2.,    -0.02, 0.00,  0.3, 0.0, -0.2,  0.0,
     /        2., 0., 2., 0., 1.,    -0.04, 0.00,  0.4, 0.0, -0.3,  0.0,
     /        2., 0., 2., 0., 2.,    -0.10, 0.00,  0.9, 0.0, -0.8,  0.0,
     /        0., 0., 2., 2., 1.,    -0.05, 0.00,  0.4, 0.0, -0.4,  0.0,
     /        0., 0., 2., 2., 2.,    -0.12, 0.00,  1.1, 0.0, -0.9,  0.0,
     /        1., 0., 2., 0., 0.,    -0.04, 0.00,  0.3, 0.0, -0.2,  0.0,
     /        1., 0., 2., 0., 1.,    -0.40, 0.01,  2.7, 0.1, -2.3, -0.1,
     /        1., 0., 2., 0., 2.,    -0.98, 0.03,  6.7, 0.2, -5.7, -0.2,
     /        3., 0., 0., 0., 0.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0,
     /       -1., 0., 2., 2., 1.,    -0.08, 0.00,  0.5, 0.0, -0.5,  0.0,
     /       -1., 0., 2., 2., 2.,    -0.20, 0.00,  1.3, 0.0, -1.1,  0.0,
     /        1., 0., 0., 2., 0.,    -0.08, 0.00,  0.5, 0.0, -0.4,  0.0,
     /        2., 0., 2.,-2., 2.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 1., 2., 0., 2.,     0.03, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 0., 2., 0., 0.,    -0.30, 0.00,  1.4, 0.0, -1.2,  0.0,
     /        0., 0., 2., 0., 1.,    -3.20, 0.09, 14.7, 0.4,-12.4, -0.4,
     /        0., 0., 2., 0., 2.,    -7.73, 0.21, 35.6, 1.0,-30.0, -0.8,
     /        2., 0., 0., 0.,-1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        2., 0., 0., 0., 0.,    -0.34, 0.00,  1.5, 0.0, -1.3,  0.0,
     /        2., 0., 0., 0., 1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0/
      DATA X2/0.,-1., 2., 0., 2.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0,
     /        0., 0., 0., 2.,-1.,     0.05, 0.00, -0.2, 0.0,  0.2,  0.0,
     /        0., 0., 0., 2., 0.,    -0.72, 0.02,  3.1, 0.1, -2.6, -0.1,
     /        0., 0., 0., 2., 1.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0,
     /        0.,-1., 0., 2., 0.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0,
     /        1., 0., 2.,-2., 1.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        1., 0., 2.,-2., 2.,     0.10, 0.00, -0.3, 0.0,  0.2,  0.0,
     /        1., 1., 0., 0., 0.,     0.04, 0.00, -0.1, 0.0,  0.1,  0.0,
     /       -1., 0., 2., 0., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0,
     /       -1., 0., 2., 0., 1.,     0.18, 0.00, -0.4, 0.0,  0.3,  0.0,
     /       -1., 0., 2., 0., 2.,     0.44, 0.00, -1.0, 0.0,  0.9,  0.0,
     /        1., 0., 0., 0.,-1.,     0.53, 0.00, -1.2, 0.0,  1.0,  0.0,
     /        1., 0., 0., 0., 0.,    -8.33, 0.12, 19.0, 0.3,-16.0, -0.2,
     /        1., 0., 0., 0., 1.,     0.54, 0.00, -1.2, 0.0,  1.0,  0.0,
     /        0., 0., 0., 1., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        1.,-1., 0., 0., 0.,    -0.06, 0.00,  0.1, 0.0, -0.1,  0.0,
     /       -1., 0., 0., 2.,-1.,     0.12, 0.00, -0.2, 0.0,  0.2,  0.0,
     /       -1., 0., 0., 2., 0.,    -1.84, 0.02,  3.6, 0.0, -3.0,  0.0,
     /       -1., 0., 0., 2., 1.,     0.13, 0.00, -0.3, 0.0,  0.2,  0.0,
     /        1., 0.,-2., 2.,-1.,     0.02, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X3/-1.,-1.,0., 2., 0.,    -0.09, 0.00,  0.2, 0.0, -0.1,  0.0,
     /        0., 2., 2.,-2., 2.,    -0.06, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0., 1., 2.,-2., 1.,     0.03, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0., 1., 2.,-2., 2.,    -1.88, 0.00,  1.0, 0.0, -0.8,  0.0,
     /        0., 0., 2.,-2., 0.,     0.25, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 0., 2.,-2., 1.,     1.17, 0.00, -0.4, 0.0,  0.3,  0.0,
     /        0., 0., 2.,-2., 2.,   -48.84, 0.11, 16.8, 0.0,-14.2,  0.0,
     /        0., 2., 0., 0., 0.,    -0.19, 0.00,  0.1, 0.0, -0.1,  0.0,
     /        2., 0., 0.,-2.,-1.,     0.05, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        2., 0., 0.,-2., 0.,    -0.55, 0.00,  0.2, 0.0, -0.1,  0.0,
     /        2., 0., 0.,-2., 1.,     0.04, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0.,-1., 2.,-2., 1.,    -0.05, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0., 1., 0., 0.,-1.,     0.09, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        0.,-1., 2.,-2., 2.,     0.83, 0.00, -0.1, 0.0,  0.1,  0.0,
     /        0., 1., 0., 0., 0.,   -15.55, 0.02,  2.6, 0.0, -2.2,  0.0,
     /        0., 1., 0., 0., 1.,    -0.14, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        1., 0., 0.,-1., 0.,     0.03, 0.00,  0.0, 0.0,  0.0,  0.0,
     /        2., 0.,-2., 0., 0.,    -0.14, 0.00,  0.0, 0.0,  0.0,  0.0,
     /       -2., 0., 2., 0., 1.,     0.42, 0.00,  0.0, 0.0,  0.0,  0.0,
     /       -1., 1., 0., 1., 0.,     0.04, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X4/0., 0., 0., 0., 2.,     7.90, 0.00,  0.1, 0.0, -0.1,  0.0,
     /        0., 0., 0., 0., 1., -1637.68,-0.10,-10.4, 0.0,  8.8,  0.0/
C       X4(18) corrected, old value +0.10, new value -0.10  
C    /        0., 0., 0., 0., 1., -1637.68, 0.10,-10.4, 0.0,  8.8,  0.0/
C
C***********************************************************************
C     Fundamental arguments from subroutine NUTFA in file cnutm.f:
C
c     L = fa (1)
c     LP= fa (2)
c     F = fa (3)
c     D = fa (4)
c     OM= fa (5)
C
      DUT    = 0.0D+0
      DLOD   = 0.0D+0
      DOMEGA = 0.0D+0
C
C     Sum zonal tide terms
C
      DO 10 I=1,N
C   Formation of multiples of arguments
      ARG = XS(1,I)*fa(1) + XS(2,I)*fa(2) + XS(3,I)*fa(3)
     *    + XS(4,I)*fa(4) + XS(5,I)*fa(5)
      ARG = DMOD(ARG,1296000.0D0) * CONVDS
C   First derivative
      ARG_DOT = XS(1,I)*fad(1)  + XS(2,I)*fad(2) + XS(3,I)*fad(3)
     *    + XS(4,I)*fad(4) + XS(5,I)*fad(5)
C     Evaluate zonal tidal terms
      DUT    = XS(6,I)*DSIN(ARG) + XS(7,I)*DCOS(ARG) + DUT
      DLOD   = ( XS(6,I)*DCOS(ARG) - XS(7,I)*DSIN(ARG) )*ARG_DOT + DLOD
   10 CONTINUE
      DUT    = DUT    * 1.0D-4
      DLOD   = -DLOD * 1.0D-4 / (3.6525D+4 / CONVDS)
      DOMEGA = -DLOD * TWOPI / SECDAY**2 
C
C     Check for debug output.
      IF (KUT1D .ne. 1) GO TO 600
      WRITE(6,9)
    9 FORMAT(1X,"Debug output for subroutine UT1SZT")
      WRITE(6,9200) L,LP,F,D,OM,ARG,DUT,DLOD,DOMEGA
 9200 FORMAT(1X,"L = ",D30.16,/,1X,"LP = ",D30.16,/,
     1       1X,"F = ",D30.16,/,1X,"D = ",D30.16,/,
     2       1X,"OM = ",D30.16,/,1X,"ARG = ",D30.16,/,
     2       1X,"DUT = ",D30.16,/,1X,"DLOD = ",D30.16,/,
     2       1X,"DOMEGA = ",D30.16)
  600 CONTINUE
C
      RETURN
      END
C
C*****************************************************************************
C
      INTEGER*4 function get_leapsec(xjd,xleap)
C
C     Routine to retrieve "leapsecond" (UTC step adjustment)
C     information for given date from default "leapsecond" file.
C     Passes back a five-element array of "leapsecond" information for
C     the given "xjd" (full) Julian date:
C
C       Position   Description
C       ---------------------------
C          1       Julian date just following step adjustment (always
C                  midnight).
C          2       The "leapsecond", i.e. TAI-UTC (seconds).
C          3       Epoch of rate of change (MJD days).
C          4       Rate of change of leapsecond (seconds/day).
C          5       Julian date just following next step adjustment.
C
C     Programmer:
C       Gregg Cooke      90.02.12  Creation.
C     Modifcations:
C       Gregg Cooke      90.03.30  Returns date following next 
C                                  "leapsecond" as new fifth element
C                                  of xleap.  Also removed lpnam.
C       Kaybee Wilcox    92.04.23  Parameters obtained from param.i.
C       Brent Archinal   92.12.22  "getunit" properly used.  Many
C                                  other fixes made.  EOF Error return
C                                  fixed to only occur on first read
C                                  (wasn't working at all).  EOF on
C                                  later read okay, but properly handled.
C       D. Gordon        98.05.01  Added to Calc UT1 module. 
C
C     Error  Returns:
C       >0   FORTRAN file I/O error.
C     -1201  Requested data is before first "leapsecond" in file.
C     -1301  No data found in "leapsecond" file (EOF on first read).
C            
      INCLUDE 'param.i'
C
C     Specifications:
C
C     LPDCB   --  Unit number of "leapsecond" file.
C     XLEAPR  --  Current "leapsecond" entry read from file.
C     BIGDT   --  Arbitrarily large Julian Date.
C
      integer*2 getunit
      integer*4 lpdcb, ierr
      real*8 xjd, xleap(5), xleapr(4), bigdt
      save lpdcb,bigdt
      data lpdcb / 0 /, bigdt/1.D+99/
C
C     Program Structure:
C
C     Initialize the routine.  Get a unit number from getunit. Then
C     open the "leapsecond" file.
C
      k = 0
      ierr = 0
      do i = 1,4
        xleapr(i) = 0.D0
        enddo
      xleap(5) = 0.D0
      get_leapsec = 0
      if(lpdcb.le.0) lpdcb = getunit()
C WEW STATUS='O' not valid - replace
C     open(UNIT=lpdcb,FILE=DFLEAP,STATUS='O',IOSTAT=ierr,ERR=911)
      open(UNIT=lpdcb,FILE=DFLEAP,STATUS='OLD',IOSTAT=ierr,ERR=911)
C
C     Read through the file until the target date is passed.  Then 
C     return with date just following the "leapsecond and the
C     "leapsecond" information.  Remember to save each record -- the
C     "leapsecond" record information BEFORE the target
C     date is really the one we want.
C
      do while(xjd.ge.xleapr(1) .and. ierr.eq.0)
	k = k + 1
	do i = 1, 4
          xleap(i) = xleapr(i)
          enddo
        read(lpdcb,'(17X,F9.1,12X,F10.7,12X,F6.0,4X,F9.7,1X)',
     .         IOSTAT=ierr,ERR=911,END=800) xleapr
        enddo
C
C     EOF and Error returns come here...
C     Set error if the target date was before the bounds of the 
C     "leapsecond" file.  Also, send back the date of the next
C     "leapsecond".
C
  800 continue
      if(ierr.eq.-1) then
        xleapr(1) = bigdt
        if(k.ne.1) ierr = 0
        endif
  911 continue
      close(lpdcb)
      get_leapsec = ierr
      if (xjd.lt.xleapr(1) .and. k.eq.1) get_leapsec = -1201
      if (ierr.eq.-1) get_leapsec = -1301
      if (get_leapsec.eq.0) xleap(5) = xleapr(1)
      return
      end
C
C*****************************************************************************
      SUBROUTINE spline(xa,ya,n,yp1,ypn,y2,ierr4)
      Implicit none
C
C     Subroutine spline is the initialization section of a two subroutine
C     module used to do cubic spline interpolations. Spline is called each
C     time you change to a new set of tabular points. Subroutine splint4
C     is the second half and does the actual interpolation.
C
C     Given arrays xa(i), and ya(i) for i=1 to n containing a tabulated
C     function ya(i) = f(xa(i)) with xa(1) < xa(2) <..< xa(n) and values
C     yp1 and ypn whichc are the first derivatives of the function f( ) at
C     points 1 and n, respectively, this routine returns an array 
C     y2(i) for i=1 to n which contains the second derivatives of the
C     function f( ) at the tabulated points xa(i). If yp1 and/or ypn are
C     greater than or equal to 1.d30, the routine is signaled to set the 
C     corresponding boundary condition for a natural spline, with zero
C     second derivatives on the boundary.
C
C     The cubic spline has the following properties.
C     1) It is continuous and exactly fits at the tabular values.
C     2) The first and second derivatives are everywhere continuous, 
C        even at the tabular points when points are added to and
C        deleted from the set of five points used to interpolate.
C     3) The third derivative is not continuous, but rather a series
C        of disconnected constant values.
C     4) The fourth derivative is zero.
C     5) It requires a minimum of five points and values of the first
C        derivative at the two endpoints.  
C
C     In order to guarantee that this routine is only used in the modes in
C     which it has been tested, we require that the values of xa be exactly 
C     one unit apart.
C
C  References:  'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
C
C  Calling sequence -
C
C     Input Variables:
C       1. xa(n) - Array of tabular (time) values corresponding to the ya
C                  array. The xa's must be evenly spaced.
C       2. ya(n) - Array of EOP values at the times corresponding to those
C                  in the xa array. i.e. ya(i) = f(xa(i)).
C       3. n     - Number of points in the xa and ya arrays.
C       4. yp1   - First derivative of the EOP function at the point i=1.
C       5. ypn   - First derivative of the EOP function at the point i=n.
C
C     Output variables:
C       1. y2(n) - Array containing the second derivatives of the
C                   interpolating function at the tabular points xa(i).
C       2. ierr4 - Error return code (0=good, 1=bad)
C
      INTEGER*4 n,NMAX,ierr4
      REAL*8 yp1,ypn,xa(20),ya(20),y2(20)
      PARAMETER (NMAX=25)
      INTEGER*4 i,k
      REAL*8 p,qn,sig,un,u(NMAX)
C
C  Program variables:
C       1. NMAX    - The largest anticipated value of n.
C       2. p       -
C       3. qn      -
C       4. sig     -
C       5. un      -
C       6. u(NMAX) - 
C
C  Programmer:
C     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
C     93.12.07  David Gordon - Make variables same as in subroutine splint4, 
C                              Calc-like documentation inserted.
C
C  SPLINE program structure:
C
C  Verify that the values of xa are one unit apart.
      do i = 2,n
        If(abs(xa(i)-xa(i-1)-1.d0) .gt. 1.d-8) then
          pause 'spline: independent variable NOT one unit apart!'
          write(6,'("spline: i, xa(i), xa(i-1), diff",i5,3d20.15)')
     .    i, xa(i), xa(i-1),(xa(i)-xa(i-1))
          ierr4 = 1
          return
        else
          ierr4 = 0
        endif
      enddo
C
C  Set lower boundary condition to be "natural" or else to have a specified
C  first derivative.
      if (yp1 .gt. .99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(xa(2)-xa(1)))*((ya(2)-ya(1))/(xa(2)-xa(1))-yp1)
      endif
C
C  Decomposition loop of the tridiagonal algorithm. y2 and u used for temporary
C  storage of the decomposed factors.
      do 11 i=2,n-1
        sig=(xa(i)-xa(i-1))/(xa(i+1)-xa(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((ya(i+1)-ya(i))/(xa(i+1)-xa(i))-
     *       (ya(i)-ya(i-1))/(xa(i)-xa(i-1)))/(xa(i+1)-xa(i-1))-
     *       sig*u(i-1))/p
  11  continue
C
C  Set upper boundary condition to be "natural" or else to have a specified
C  first derivative.
      if (ypn .gt. .99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(xa(n)-xa(n-1)))*(ypn-(ya(n)-ya(n-1))/(xa(n)-xa(n-1)))
      endif
C
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
C
C  Backsubstitution loop of the tridiagonal algorithm.
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
  12  continue
C
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *1(a)031..
C
C*******************************************************************************
      SUBROUTINE splint4(xa,ya,y2,n,x,y,ydot,ydot2,ydot3,ierr4)
      Implicit none
C
C     Given the arrays xa(i) and ya(i) for i = 1 to n, which tabulate      
C     a function with the xa(i)'s in time order, and given the array
C     y2(i), which is the previous output of subroutine spline, and  
C     given a value of x, this routine returns the cubic spline
C     interpolated value of y and its first three derivatives. 
C
C  References: 'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
C
C  Calling sequence -
C
C     Input Variables:
C       1. xa(n) - Array of tabular (time) values corresponding to the ya
C                  array. The xa's must be evenly spaced.
C       2. ya(n) - Array of EOP values at the times corresponding to those
C                  in the xa array. i.e. ya(i) = f(xa(i)).
C       3. y2(n) - Array containing the second derivatives of the
C                  interpolating function at the tabular points xa(i).
C       4. n     - Number of points in the xa and ya arrays.
C       5. x     - Input time for which the corresponding interpolated value 
C                  of y (= f(x)) is to be determined.
C
C     Output variables:
C       1. y     - The interpolated value.  
C       2. ydot  - The 1st derivative of the interpolated value.  
C       3. ydot2 - The 2nd derivative of the interpolated value.
C       4. ydot3 - The 3rd derivative of the interpolated value.
C       5. ierr4 - Error return code (0=good, 1=bad)
C
      INTEGER*4 n, ierr4
      REAL*8 x,y,xa(20),y2(20),ya(20),ydot,ydot2,ydot3 
      INTEGER*4 k,khi,klo
      REAL*8 a,b,h,adot,bdot
C
C  Program variables - klo, khi, k, h, a, b, adot, bdot
C
C  Programmer:
C     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
C                              First, second, and third derivatives added.
C     93.12.07  David Gordon - Make variables same as in subroutine spline, 
C                              Calc-like documentation inserted.
C
C  SPLINT4 program structure:
C
C  Find right place in table by means of bisection. Optimized for sequential
C   calls being at random values of x. 
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k) .gt. x)then
          khi=k
        else
          klo=k
        endif
      go to 1
      endif
C
C  Make sure the xa's aren't the same values.
      h=xa(khi)-xa(klo)
      if (h.eq.0.) then                           
        write(6,'(" Bad xa input in splint4 ")')
        ierr4 = 1
      else
        ierr4 = 0
      endif
C
C  Evaluate cubic spline polynomial
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2(klo)+(b**3-b)*y2(khi))*
     *  (h**2)/6.
C
C  First derivative
      adot = -1.d0/h
      bdot =  1.d0/h
      ydot = adot*ya(klo)+bdot*ya(khi)
     .       +( (3.d0*a**2-1.d0)*adot*y2(klo)
     .       +  (3.d0*b**2-1.d0)*bdot*y2(khi))*(h**2/6.d0)
C
C  Second derivative.      
      ydot2 = (a*(adot**2)*y2(klo)+b*(bdot**2)*y2(khi))*(h**2)
C
C  Third derivative.
      ydot3 = ((adot**3)*y2(klo)+(bdot**3)*y2(khi))*(h**2)
C
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *1(a)031..
