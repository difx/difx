      SUBROUTINE WOBA
      Implicit None
C
C     WOBA adds entries to the table of contents for the Wobble module text
C     message, and the partial derivatives and contributions arrays. It also
C     adds entries to the table of contents for the flow control and
C     interpolation messages.
C
C     Common blocks used -
C
      INCLUDE 'cmwob.i'
C         Variables 'to':
C           1) KERASK - The database error return code from the 'ASK'
C                       for the rotation epoch array.
C           2) NEPOCH - The number of epochs at which the interpolated
C                       polar motions are desired.
C         Variables 'from':
C             1. WOBIF(3)  -  The wobble information array. Contains
C                              respectively: 1) The Julian date of the first
C                              tabular point, 2) The increment in days of the
C                              tabular points, 3) The number of tabular points.
C                              (days, days, unitless)
C
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_EOP - T/F logical flag telling whether to use external
C                             EOP file input
C
C    Program Specifications -
      Integer*2 LTEXT(16), nd1, nd3, nver, ktype, nut1
      CHARACTER*32 LTEXT_chr
      EQUIVALENCE (LTEXT,LTEXT_chr)
C
C     Database access -
C            Access codes:
C              1.  'WOB MESS'  -  Wobble module message code.
C              2.  'WOB PART'  -  Wobble partials array code.
C              3.  'WOBXCONT'  -  X-Wobble contributions array code.
C              4.  'WOBYCONT'  -  Y-Wobble contributions array code.
C              5.  'WOB CFLG'  -  Wobble flow control message code.
C              6.  'POLAR XY'  -  Observation wobble values code.
C              7.  'ROTEPOCH'  -  Rotation epochs array code.
C              8.  'WOBEPOCH'  -  Interpolated wobble array code.
C              9.  'WOBINTRP'  -  Access code for polar motion interpolation
C                                 message.
C
C     Subroutine interface -
C             Caller subroutines: TOCUP
C             Called subroutines: ADDA, ADDR, ASK
C
C     Program Variables -
C       1) LTEXT(16) - A dummy array used to hold the message from 'ASK'.
C
C     Programmer - Dale Markham  02/17/77
C     77.07.14 Peter Denatale
C     77.01.11 Bruce Schupler
C     87.06.04 Savita Goel     CDS for A900.
C     88.12.21 Gregg Cooke
C     89.07.26 Jim Ryan  Documentation simplifed and strings.
C     89.08.14 Jim Ryan  Wobble table shortened to 10 and LEN_WOB_TABLE added
C     89.12.12 Jim Ryan  UNIX-like database interface implimented.
C     90.06.95 Jim Ryan  Mods for linear interpolation.
C     93.12.17 D.Gordon  Mods for cubic spline interpolation.
C     94.04.15 D.Gordon  Converted to Implicit None.
C     94.05.23 D.Gordon  Fixed bug - use_cubic, use_linear, use_spline were
C                        being dimensioned both Logical*2 and Real*8.
C     94.06.09 D.Gordon  Shuffled Common block WOBCM (R*8, I*4, I*2, L*2)
C     94.06.29 D.Gordon  Split wobble contribution (WOB CONT) into separate X
C                        and Y contributions (WOBXCONT and WOBYCONT). Code
C                        added to delete 'WOB CONT' from old databases.
C     98.05.01 D.Gordon  Put Common /WOBCM/ into include file 'cmwob.i'.
C                        Added include file 'inputs.i'. ADD's added for
C                        'FWOB INF', 'FWOBX&YT', and 'FWOBTEXT'. DEL's for
C                        'PWOB INF', 'PWOBX&YT', 'PWOBTEXT', 'EWOB INF', 
C                        'EWOBX&YT', and 'EWOBTEXT' for the case of 
C                        external EOP input. 
C     99.10.27 D.Gordon  Corrected error in ADD/PUT of 'WOBEPOCH' when
C                        external input being used. 
C
C     WOBA Program Structure
C
C     ADD for wobble module text message.
      CALL ADDA (1,'WOB MESS','Wobble message definition.      ',
     1     40, 1, 1 )
C
C     Add for wobble interpolation scheme message.
      Call ADDA( 1,'WOBINTRP','Interp. scheme for polar motion.',
     .     30,1,1)
C
C     ADD for wobble module flow control message.
      CALL ADDA (1,'WOB CFLG','Wobble flow control mess def.   ',
     1     40,1,1)
C 
C     ADD for wobble partials.
      CALL ADDR (2,'WOB PART','Wobble partial derivatives def. ',
     1     2, 2, 1 )
C
C     ADD's for wobble contributions.
C   Now expanded into two separate L-codes for X & Y. 94JUN29
      CALL ADDR (2,'WOBXCONT','X Wobble contribution definition',
     1     2, 1, 1 )
      CALL ADDR (2,'WOBYCONT','Y Wobble contribution definition',
     1     2, 1, 1 )
C
C   Delete old X and Y contributions from pre-Calc 8.0 databases.
      CALL DELR(2,'WOB CONT')
C
C     ADD for X and Y wobble values.
      CALL ADDR (2,'POLAR XY','Polar motion X & Y for obs (rad)',
     .     2, 1, 1)
C
C**   IF (.not. Input_EOP) THEN       ! Skip check if external EOP input
C       Check to see if there is a rotation epoch array in the database.
        CALL ASK ('ROTEPOCH',1,ND1,NEPOCH,ND3,NVER,LTEXT_chr,KTYPE,
     .      KERASK)
C**   ELSE
C**      KERASK = 0
C**   ENDIF                           ! Skip check if external EOP input
C
C     If there is no rotation epoch array in the database,
C     skip on.  Otherwise ADD for interpolated wobble values.
      IF (KERASK .NE. 0) GO TO 100
      CALL ADDR (1,'WOBEPOCH','Interpolated wobble array def   ',
     1          2,NEPOCH,1)
  100 CONTINUE
C
C  ADD's for optional EOP input via external file
      IF (Input_EOP) THEN
        nut1 = WOBIF(3) + 0.01
       CALL ADDR(1,'FWOB INF','Final Value wobble array descr. ',
     .              3,1,1)
       CALL ADDR(1,'FWOBX&YT','Final wobble X,Y component value',
     .              2,nut1,1)
       CALL ADDA(1,'FWOBTEXT','Final Value wobble origin text. ',
     .              40,1,1)
C
       CALL DELR(1,'PWOB INF')
       CALL DELR(1,'PWOBX&YT')
       CALL DELA(1,'PWOBTEXT')
       CALL DELR(1,'EWOB INF')
       CALL DELR(1,'EWOBX&YT')
       CALL DELA(1,'EWOBTEXT')
      ENDIF
C
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE WOBI
      Implicit None
C
C     WOBI is the wobble module input and initialization section.
C
C     Common blocks used -
C
      INCLUDE 'cmwob.i'
C            Variables 'from':
C              1) KERASK - The database return code from the 'ASK' for
C                          the rotation epochs array.
C              2) NEPOCH - The number of epochs in the rotation epochs array.
C            Variables 'to':
C              1. WOBIF(3)  -  The wobble information array. Contains 
C                              respectively: 1) The Julian date of the first
C                              tabular point, 2) The increment in days of the
C                              tabular points, 3) The number of tabular points.
C                              (days, days, unitless)
C              2. XYWOB(2,20)- The wobble tabular points for the polar motion 
C                              (wobble) X & Y offsets. (milliarcsec)
C                              (Note: Based on old BIH conventions, offsets
C                              are assumed to be left-handed.)
C
      INCLUDE 'cmxut.i'
C            Variables 'from':
C              1. Ndays - number of days in the ROTEPH array.
C
      INCLUDE 'ccon.i'
C            Variables 'from':
C              1.  KWOBC  -  The Wobble Module flow control flag.
C                            0 --> Default, module on; spline interpolation
C                                  for 1-day series, cubic for 5-day series.
C                            1 --> Module off. No polar motion applied.
C                            2 --> Module on; linear interpolation for any
C                                  series. 
C                            3 --> Module on; cubic interpolation for 1-day
C                                  series, spline for 5-day series. 
C              2.  KWOBD  -  The Wobble Module debug output flag.
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
C            2. ROTEPH(2,20)- The array which contains the epochs at which
C                             TAI - UT1 is desired. The entries are:
C                             1) JD at 0:00 hours UTC,
C                             2) The fraction of a UTC day from 0:00 hours
C                                to the desired epoch.
C
C   Program specifications -
C
      Real*8 yp1x,ypnx,yp1y,ypny
      Integer*4 ierr4
      INTEGER*2  KERR(7), NDO(3), Increment, Tab_len
      INTEGER*2      LWOBM(40)
      CHARACTER*40 C_LWOBM(2)
      EQUIVALENCE (C_LWOBM,LWOBM)
      INTEGER*2      LON(40) ,   LOFF(40)
      CHARACTER*40 C_LON(2)  , C_LOFF(2)
      EQUIVALENCE(C_LON,LON),(C_LOFF,LOFF)
      REAL*8 WOBTAB(2,20), XJD, UTC, WOBXL, WOBYL, tab_time,
     *       DWOBXL, DWOBYL
      Integer*2    mess_linear(30), mess_cubic(30), mess_spline(30)
      Character*60 mess_linear_c , mess_cubic_c, mess_spline_c
      Equivalence(mess_linear,mess_linear_c),(mess_cubic,mess_cubic_c),
     *           (mess_spline,mess_spline_c)
C     logical*4 table_found
      Character*1 type_found, cdumm(3)
C
      Character*20 C_LFI, C_LPR, C_LEX
      Integer*2 LFI(10), LPR(10), LEX(10)
      Equivalence (C_LFI,LFI), (C_LPR,LPR), (C_LEX,LEX)
      Integer*2 MEPOCH, mdum1
      Integer*4 MXEPCH, N, II
C
      Integer*2 lwtext(40)
      Character wtext*80
      Equivalence  (wtext,lwtext)
C
      DATA LEN_WOB_TABLE /20/
      data roteph /40*0.0d0/
      data wobtab /40*0.0d0/
C
      DATA C_LWOBM /
     .'Wobble Module - Version 98.05.04, last m',
     .'od D. Gordon, GSFC -                    '/
C
      DATA  C_LFI /' Final Values       '/
      DATA  C_LPR /' Preliminary Values '/
      DATA  C_LEX /' Extrapolated Values'/
C
      DATA C_LON   /
     .'Wobble Module is turned on.             ',
     .'                                        '/
C
      DATA C_LOFF  /
     .'Wobble Module is turned off.            ',
     .'                                        '/
C
      Data mess_cubic_c  /
     .'Polar motion table interp. w. 4 pt, 3rd order polynomial.   '/
c
      Data mess_linear_c/
     .'Polar motion table interp. w. 2 pt, linear interpolator.    '/
C
      Data mess_spline_c /
     .'Polar motion table interpolated with 5 point cubic spline.  '/
C
      DATA MXEPCH /20/
C
C    Database access  -
C           'GET' Variables:
C             1. WOBIF(3)   -  The wobble information array. Contains
C                              respectively: 1) The Julian date of the first
C                              tabular point, 2) The increment in days of the
C                              tabular points, 3) The number of tabular points.
C                              (days, days, unitless)
C             2. XYWOB(2,20) - The tabular points for the long period
C                              wobble X & Y-offsets. (milli-arc-sec)
C                              (NOTE: in old BIH convention UT1, X-pole, Y-pole
C                              system is left handed.)
C             3. ROTEPH(2,20)- The epochs at which the interpolated WOBBLE are
C                              desired. The first entry is the Julian date at
C                              0:00 hours UTC of the day desired. The second
C                              entry is the UTC fraction of the UTC day of the
C                              desired epoch (days,days)
C           'PUT' Variables:
C             1. LWOBM(40)    - The Wobble Module text message.
C             2. LON(40)      - The Wobble Module 'TURNED ON' message.
C             3. LOFF(40)     - The Wobble Module 'TURNED OFF' message.
C             4. WOBTAB(2,20) - The array which contains the interpolated
C                               wobble in a left-handed system. The first entry
C                               is the X wobble. The second entry is the Y
C                               wobble. (milliarcsec, milliarcsec)
C           ACCESS CODES:
C             1. 'WOB MESS'  -  The database access code for the Wobble Module
C                               text message.
C             2. 'FWOB INF'  -  The database access code for the final wobble
C                               information array.
C             3. 'FWOBX&YT'  -  The database access code for the final wobble
C                               X & Y offsets.
C             4. 'PWOB INF'  -  The database access code for the preliminary
C                               wobble information array.
C             6. 'PWOBX&YT'  -  The database access code for the preliminary
C                               wobble X & Y offsets.
C             7. 'WOB CFLG'  -  The database access code for the Wobble Module
C                               flow control message.
C             8. 'ROTEPOCH'  -  The database access code for the epochs at
C                               which the interpolated values are computed.
C             9. 'WOBEPOCH'  -  The database access code for the interpolated
C                               wobble array.
C            10. 'EWOB INF'  -  The database access code for the extrapolated
C                               wobble information array.
C            11. 'EWOBX&YT'  -  The database access code for the extrapolated
C                               wobble X & Y offsets.
C            12. 'WOBINTRP'  -  The database access code for the polar motion
C                               interpolation message.
C
C     Subroutine interface -
C             Caller subroutines: INITL
C             Called subroutines: GET4, KILL, PUTA, PUT4, WOBMU
C
C     Program Variables -
C           1. Increment -  The increment of the wobble table. (days)
C           2. KERR(7)   -  Database error return flags.
C           3. NDO(3)    -  Database return array indices.
C           4. MXEPCH    -  The maximum number of interpolated wobble epochs.
C           5. WOBXL     -  The interpolated X-wobble (left-handed).
C                           (milliarcsec)
C           6. WOBYL     -  The interpolated Y-wobble (left-handed).
C                           (milliarcsec)
C           7. DWOBXL    -  The time derivative of the interpolated X-wobble.
C                           (left-handed) (milliarcsec/sec)
C           8. DWOBYL    -  The time derivative of the interpolated Y-wobble.
C                           (left-handed) (milliarcsec/sec)
C           9. MEPOCH    -  The number of epochs at which the wobble
C                           is interpolated.
C          10. XJD       -  The Julian date at 0:00 hours UTC of the
C                           interpolation epoch. (days)
C          11. UTC       -  The UTC fraction of the UTC day of the
C                           interpolation epoch. (days)
C          12. Tab_len   -  The length of the Wobble table in the database.
C          13. type_found-  Tracks the type of wobble table in database.
C          14. tab_time  -  The fraction of the day of the interpolation epoch
C                           in WOBMU. (days)
C
C 3.2.9 PROGRAMMER - DALE MARKHAM  02/17/77
C     77.07.14 Peter Denatale
C     78.01.11 Bruce Schupler
C     89.07.26 Jim Ryan  Documentation simplifed and strings.
C     89.08.16 Jim Ryan  Use of LEN_WOB_TALBE implimented.
C     89.12.12 Jim Ryan  UNIX-like database interface implimented.
C     90.06.05 Jim Ryan  Mods for linear interpolation and more clean up.
C     92.07.17 Jim Ryan  Roteph and Wobtab initialized to avoid debug problem.
C     93.12.17 D. Gordon Spline interpolation added, new flow control logic.
C     93.12.30 D. Gordon Cleaned up 'WOB MESS'.
C     94.04.15 D. Gordon Converted to Implicit None.
C     94.05.23 D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
C                        being dimensioned both Logical*2 and Real*8.
C     95.10.05 D. Gordon Skip interpolation if module OFF (KWOBC=1); set
C                        WOBXL and WOBYL equal to zero (previously undefined).
C     95.12.04 D. Gordon Interpolation epoch variable changed from UTC to
C                        tab_time.
C     95.12.11 D. Gordon Adding DWOBXL and DWOBYL, derivatives of X-wobble and
C                        Y-wobble, to WOBMU argument list. Not used here. 
C     98.05.01 D.Gordon  Put Common /WOBCM/ into include file 'cmwob.i'.
C                        Added include files 'inputs.i' and 'cmxut.i', and 
C                        common block EOPCM. Extensive mods for external 
C                        EOP input.
C     99.10.27 D.Gordon  Corrected error in ADD/PUT of 'WOBEPOCH' when
C                        external input being used. 
C
C     WOBI Program Structure
C
C   Verify that the wobble control flag is okay.
      If(KWOBC.lt.0 .or. KWOBC.gt.3) Then
        Write(6,'(
     *  "In WOBI, Wobble module control flag has invalid value.",/,
     *  "KWOBC =",i5)') KWOBC
        CALL CKILL(6hWOBI  ,0,0)
      Endif
C ---------------------------------------------------------------------------
C  If using data base EOP input, get the X and Y wobble values from the data 
C   base now. If using external EOP input instead, then we already have them.
C ---------------------------------------------------------------------------
      IF (.not. Input_EOP) THEN      !Data Base or External Input?
C
C  'GET' the final wobble information from the database. If final information
C  is not available, then 'GET' the preliminary or extrapolated information.
C  If a database access error is detected, then KILL CALC.
C
      table_found = .false.
      If(.not.table_found) Then     ! Get final values
        CALL GET4 ('FWOB INF      ',WOBIF,3,1,1,NDO,KERR(1) )
        IF ( KERR(1) .eq. 0 ) Then
          table_found = .true.
          type_found  = 'F'
           do n = 1,10 
             LWOBM(30+n) = LFI(n)
           enddo
        Endif
      Endif
C
      If(.not.table_found) Then     ! Get preliminary values
        CALL GET4 ('PWOB INF      ',WOBIF,3,1,1,NDO,KERR(1))
        IF (KERR(1) .eq. 0) Then
          table_found = .true.
          type_found  = 'P'
           do n = 1,10 
             LWOBM(30+n) = LPR(n)
           enddo
        Endif
      Endif
C
      If(.not.table_found) Then     ! Get extrapolated values
        CALL GET4 ('EWOB INF      ',WOBIF,3,1,1,NDO,KERR(1))
        If(KERR(1).eq.0) Then
          table_found = .true.
          type_found  = 'X'
           do n = 1,10 
             LWOBM(30+n) = LEX(n)
           enddo
        Endif
      Endif
C
      If(KERR(1).eq.2) Then
        Write(6,'(
     .  "In WOBI: The wobble table in database has wrong size.")')
        CALL CKILL(6hWOBI  ,KERR(1),1)
      Endif
C
      If (.not.table_found) Then
        Write(6,'(
     .  "This database contains NO polar motion data. Quitting!")')
        CALL CKILL(6HWOBI  ,5,KERR(5))
      Endif
C
      Increment = WOBIF(2) + 0.01
      Tab_len   = WOBIF(3) + 0.01
C
      If(Tab_len .gt. LEN_WOB_TABLE) Then
        WRITE(6,'(
     .  "The length of the wobble table from the database is",I5,/,
     .  "CALC Wobble module currently supports only",I5)') Tab_len,
     .  LEN_WOB_TABLE
        CALL CKILL(6hWOBI,0,0)
      Endif
C
      ELSE                           !Data Base or External Input?
C
C      Do PUT's for the case of EOP external file input
C
         Increment = WOBIF(2) + 0.01
         Tab_len   = WOBIF(3) + 0.01
        call put4('FWOB INF      ',WOBIF,3,1,1)
        call put4('FWOBX&YT      ',XYWOB,2,Tab_len,1)
C   lwtext = ??????
        wtext(1:16) = 'X/Y wobble from ' 
        wtext(17:80) =  Ex_EOP(1:64) 
        call puta('FWOBTEXT      ',lwtext,40,1,1)
C
      Increment = WOBIF(2) + 0.01
      Tab_len   = WOBIF(3) + 0.01
C
      ENDIF                          !Data Base or External Input?
C ---------------------------------------------------------------------------
C
C  Determine interpolation method. Defaults are cubic spline for a 1-day series
C    and cubic polynomial for a 5-day (or any other) series. 
C
        use_spline = .false.
        use_linear = .false.
        use_cubic  = .false.
C
      If(KWOBC.eq.0) Then       !Default interpolation
        If(Increment.eq.1) Then   
          use_spline = .true.
          Call PUTA('WOBINTRP      ',mess_spline,30,1,1)
        Else
          use_cubic = .true.
          Call PUTA('WOBINTRP      ',mess_cubic,30,1,1)
        Endif
      Endif 
C
      If(KWOBC.eq.2) Then       !Force linear interpolation
          use_linear = .true.
          Call PUTA('WOBINTRP      ',mess_linear,30,1,1)
      Endif 
C
      If(KWOBC.eq.3) Then       !Reverse default interpolation
        If(Increment.eq.1) Then   
          use_cubic = .true.
          Call PUTA('WOBINTRP      ',mess_cubic,30,1,1)
        Else
          use_spline = .true.
          Call PUTA('WOBINTRP      ',mess_spline,30,1,1)
        Endif
      Endif 
C
      IF (.not. Input_EOP) THEN 
C        'GET' the tabular values, if not external input.
        If(type_found.eq.'F')
     .   CALL GET4 ('FWOBX&YT      ',XYWOB,2,Tab_len,1,NDO,KERR(6))
        If(type_found.eq.'P')
     .   CALL GET4 ('PWOBX&YT      ',XYWOB,2,Tab_len,1,NDO,KERR(6))
        If(type_found.eq.'X')
     .   CALL GET4 ('EWOBX&YT      ',XYWOB,2,Tab_len,1,NDO,KERR(6))
        If(KERR(6).ne.0) CALL CKILL(6hWOBI  ,6,kerr(6))
      ENDIF
C
C  'PUT' the module text message.
      CALL PUTA ('WOB MESS      ', LWOBM, 40, 1, 1 )
C
C   'PUT' the Wobble module flow control message depending on KWOBC.
C    See above string definitions for meanings.
C
      If (KWOBC .ne. 1) Then
        CALL PUTA ('WOB CFLG      ',LON ,40,1,1)
      Else
        CALL PUTA ('WOB CFLG      ',LOFF,40,1,1)
      Endif
C
C   Code for spline interpolation initialization, 93DEC17  -DG-
C
      If (use_spline) Then       ! Initialize spline routine
C
      n_spline = tab_len
      do ii=1,n_spline
       yax(ii) = XYWOB(1,ii)
       yay(ii) = XYWOB(2,ii)
      enddo
C
      xa(1) = WOBIF(1)
      do ii=2,n_spline
       xa(ii) = xa(ii-1) + WOBIF(2)
      enddo
C
C   If interval (WOBIF(2)) not 1.0 days, then divide by interval
      if ( Abs(WOBIF(2) - 1.D0) .gt. 1.D-10) then
        do ii=1,n_spline
         xa(ii) = xa(ii) / WOBIF(2)
        enddo
      endif
C
C   Take first derivatives at endpoints for X-wobble
      yp1x = (yax(2)-yax(1)) / (xa(2)-xa(1))
      ypnx = (yax(n_spline)-yax(n_spline-1))/
     .       (xa(n_spline)-xa(n_spline-1))
C  call spline initialization subroutine for X-wobble
      call spline(xa,yax,n_spline,yp1x,ypnx,y2sx,ierr4)
C
C   Take first derivatives at endpoints for Y-wobble
      yp1y = (yay(2)-yay(1)) / (xa(2)-xa(1))
      ypny = (yay(n_spline)-yay(n_spline-1))/
     .       (xa(n_spline)-xa(n_spline-1))
C  call spline initialization subroutine for Y-wobble
      call spline(xa,yay,n_spline,yp1y,ypny,y2sy,ierr4)
C
      Endif                      ! Initialize spline routine
C
C    If there is a ROTEPH array in the database, GET it. Otherwise, bypass this 
C    step. Also, check in case there are more than MXEPCH entries in ROTEPH.
C
C**   IF (.not. Input_EOP) THEN 
        IF (KERASK .NE. 0) GO TO 400
        MEPOCH = NEPOCH
        IF (MEPOCH .GT. MXEPCH) MEPOCH = MXEPCH
        CALL GET4('ROTEPOCH      ',ROTEPH,2,MEPOCH,1,NDO,KERR(7))
        IF (KERR(7) .NE. 0) CALL CKILL(6HWOBI  ,7,KERR(7))
C**   ELSE
C**     MEPOCH = ndays
C**   ENDIF
C
C    Compute the interpolated values of the wobble with a call to WOBMU.
      DO N=1,MEPOCH
        XJD = ROTEPH(1,N)
        tab_time = ROTEPH(2,N)
         If (KWOBC.ne.1) then             ! Module ON
          CALL WOBMU(XJD,tab_time,WOBXL,WOBYL,DWOBXL,DWOBYL)
         Else                             ! Module OFF
          WOBXL = 0.D0
          WOBYL = 0.D0
         Endif
        WOBTAB(1,N) = WOBXL
        WOBTAB(2,N) = WOBYL
      Enddo
C
C    'PUT' the WOBTAB array into the database.
      CALL PUT4 ('WOBEPOCH      ',WOBTAB,2,MEPOCH,1)
C
C    Go here if bypassing the interpolation step.
  400 CONTINUE
C
C    Check KWOBD for debug output.
      IF ( KWOBD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, 'Debug output for subroutine WOBI.' )
    7 FORMAT(A,15I8/(9X,15I8))
      WRITE(6,8)' WOBIF ',WOBIF
    8 FORMAT(A,4D25.16/(8X,5D25.16))
      WRITE(6,8)' XYWOB   ',XYWOB
      WRITE(6,8)' ROTEPH  ',ROTEPH
      WRITE(6,8)' WOBTAB  ',WOBTAB
C
C     Normal conclusion.
  500 RETURN
      END
C
C******************************************************************************
      SUBROUTINE WOBG ( CT, UTC, XJD, TSKIP, RW, WOBXR, WOBYR )
      IMPLICIT None
C
C    WOBG is the geometry section of the Wobble Module. It computes the wobble
C    portion of the complete crust fixed to J2000.0 rotation matrix. Although
C    this subroutine uses the BIH sign convention for a left-handed coordinate
C    system, the values for WOBX and WOBY leave this routine as WOBXR and WOBYR,
C    values in a right-handed system. WOBXR and WOBYR are passed to all routines
C    outside the Wobble Module. (Used in Pole Tide module.)
C
C    References - Ash, M.E., 'Determination of Earth Satellite Orbits", Lincoln
C                 Laboratory Technical Report 1972-75, 04/19/72, p. 229-230.
C
C                 Mueller, I.P., "Spherical and Practical Astronomy as Applied
C                 to Geodesy", 1969, p. 80-85. (NOTE: The reference in Mueller
C                 refers to the calculation of the wobble portion of the
C                 complete J2000.0 to crust fixed rotation matrix. Program CALC
C                 however, needs the transpose of this matrix, so be cautious
C                 if comparing the reference to the following program.)
C
C     Calling sequence -
C           Input Variables:
C             1. CT       -  Coordinate time (ephemeris time). 
C             2. UTC      -  The UTC fraction of the UTC day (days).
C             3. XJD      -  The Julian date at zero hours UTC of the date in
C                            question. (days)
C             4. TSKIP    -  Skip time-dependent computations if TSKIP=1 
C                            (same time as previous observation).
C           Output Variables:
C             1. RW(3,3,2) - The wobble portion of the complete crust fixed to
C                            J2000.0 rotation matrix and its first time'
C                            derivative. (unitless, 1/sec)
C             2. WOBXR     - The long period wobble X-offset. (RAD)
C                            This variable, along with WOBYR, are for use in a
C                            right-handed coordinate system. WOBXR and WOBYR
C                            are the variables passed to all routines external
C                            to the Wobble Module.
C             3. WOBYR     - The long period wobble Y-offset. (RAD)
C                            To be used in a right-handed system (see WOBXR).
C
C     Common blocks used -
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C            Variables 'FROM':
C              1. CONVDS  -  The conversion factor of radians per arcsecond. 
C                            (radians/arcsecond)
C
      INCLUDE 'cmwob.i'
C            Variables 'TO':
C              1. RWOBX(3,3) - The rotation matrix which rotates about the crust
C                              fixed Y-axis by an angle equal to WOBX.
C              2. RWOBY(3,3) - The rotation matrix which rotates about the new 
C                              X-axis by an angle equal to WOBY. (unitless)
C              3. WOBX       - The left-handed X-wobble. (rad)
C              4. DWOBX      - Time derivative of WOBX. (rad/sec)
C              5. WOBY       - The left-handed Y-wobble. (rad)
C              6. DWOBY      - Time derivative of WOBY. (rad/sec)
C              7. rwx(3,3)   - Rotation matrix used in computation of time 
C                              derivative of wobble rotation matrix.
C              8. rwy(3,3)   - Rotation matrix used in computation of time 
C                              derivative of wobble rotation matrix.
C
      INCLUDE 'cmxut.i'
C            Variables 'FROM':
C              1. EOP_time_scale - EOP table time scale, allowed values:
C                              'TAI     ', 'TCG     ', 'TDB     ',
C                              'TDT     ', 'UTC     ', 'UNDEF   '.
C
      INCLUDE 'ccon.i'
C            Variables 'FROM':
C              1.  KWOBC  -  The Wobble Module flow control flag.
C              2.  KWOBD  -  The Wobble Module debug output flag.
C
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
C           VARIABLES 'TO':
C            1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
C                           CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
C                           RATE OF CHANGE OF 'TAI MINUS UTC'. Used in the
C                           atomic time module. (DAYS, SEC, SEC/SEC)
C
C   Program Specifications -
      Real*8 RW(3,3,2), WOBIN(2), UTC, XJD, WOBXR, WOBYR, WOBXL,
     *       WOBYL, DWOBXL, DWOBYL, CT, tab_time,rw1(3,3), rw2(3,3) 
      Integer*4 TSKIP
C
C     Database access  -
C             Variables 'PUT' :
C               1. WOBIN - The X and Y polar motion values interpolated to
C                          the time of this observation in the conventional
C                          left-handed system. (rad)
C
C     Subroutine interface -
C             Caller subroutines: DRIVG
C             Called subroutines: WOBMU, MMUL2, ROTAT, DROTT, MADD2
C
C     Program Variables -
C               1. WOBXL  - The left-handed X-Wobble. (milliarcsec)
C               2. WOBYL  - The left-handed Y-Wobble. (milliarcsec)
C               3. DWOBXL - Time derivative of WOBXL. (masec/sec)
C               4. DWOBYL - Time derivative of WOBYL. (masec/sec)
C               5. WOBIN  - Both wobbles, to be put into the database.
C               6. rw1(3,3)-Rotation matrix used in computation of time 
C                           derivative of wobble rotation matrix.
C               7. rw2(3,3)-Rotation matrix used in computation of time 
C                           derivative of wobble rotation matrix.
C
C     Programmer - Dale Markham  02/17/77
C      77.07.14 Peter Denatale
C      77.03.31 Kathy Watts
C      78.01.09 Bruce Schupler
C      88.12.21 Gregg Cooke
C      89.07.26 Jim Ryan  Documentation simplifed and strings.
C      89.12.12 Jim Ryan  UNIX-like database interface implimented.
C      90.01.02 Jim Ryan  WOBIN converted to left-handed.
C      91.06.06 Jim Ryan  Clean up documentation.
C      94.04.15 D. Gordon Converted to Implicit None.
C      94.05.23 D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
C                         being dimensioned both Logical*2 and Real*8.
C      95.10.05 D. Gordon Skip interpolation if Module OFF (KWOBC=1); set
C                         WOBXL and WOBYL equal to zero (previously undefined).
C      95.12.04 D. Gordon Changing interpolation epoch variable to tab_time
C                         (previously was UTC). Tab_time set to CT for 
C                         consistency with UT1 interpolation which is in CT. 
C      95.12.11 D. Gordon Adding DWOBXL and DWOBYL, derivatives of X-wobble and
C                         Y-wobble, to WOBMU argument list. Variables DWOBX,
C                         DWOBY, rwx, rwy, rw1, and rw2 added. Code added to
C                         compute the time derivative of the wobble rotation
C                         matrix. RW(3,3) changed to RW(3,3,2).
C      96.02.09 D. Gordon Added dwobx, dwoby, rwx, and rwy to Common/WOBCM/.
C    2001.01.02 D. Gordon Code added to determine time scale of EOP table
C                         (CT/TDB, UTC, TDT or TAI).
C
C    WOBG Program Structure.
C
      IF (TSKIP .eq. 1) Go to 250
C
C     Call 'WOBMU' to obtain WOBXL and WOBYL
      If (KWOBC.ne.1) then             ! Module ON
C
C  Determine time scale of EOP table epochs. CT (=TDB) is the default.
         tab_time = CT
       If (EOP_time_scale .eq. 'UTC     ') tab_time = UTC
       If (EOP_time_scale .eq. 'TAI     ') tab_time = UTC +
     *       ATMUTC(2)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
       If (EOP_time_scale .eq. 'TDT     ') tab_time = UTC +
     *      (ATMUTC(2) + 32.184D0)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
C
       CALL WOBMU(XJD,tab_time,WOBXL,WOBYL,DWOBXL,DWOBYL)
      Else                             ! Module OFF
       WOBXL  = 0.D0
       WOBYL  = 0.D0
       DWOBXL = 0.D0
       DWOBYL = 0.D0
      Endif
C
C    Convert the variables WOBXL and WOBYL from units of milliarcseconds
C    to units of radians.
      WOBX = WOBXL * CONVDS * 1.0D-3
      WOBY = WOBYL * CONVDS * 1.0D-3
      DWOBX = DWOBXL * CONVDS * 1.0D-3
      DWOBY = DWOBYL * CONVDS * 1.0D-3
C
C    Create variables WOBXR and WOBYR, for external use, in subroutines
C    using a right-handed coordinate system.
      WOBXR = WOBX
      WOBYR = -WOBY
C
C    Compute the wobble portion of the complete crust fixed to J2000.0
C    rotation matrix. (NOTE: WOBY is still left-handed, however,
C    the wobble matrix is constructed to take this into account.
C    Thus the final wobble matrix is right-handed.)
C
C    Construct the rotation matrix which rotates about the crust fixed
C    Y-axis by an angle equal to +WOBX.
      CALL ROTAT ( +WOBX, 2, RWOBX )
C
C    Construct the rotation matrix which rotates about the X-axis by
C    an angle equal to +WOBY.
      CALL ROTAT ( +WOBY, 1, RWOBY )
C
C    Complete the construction of the wobble matrix.
      CALL MMUL2 ( RWOBY, RWOBX, RW(1,1,1) )
C
C  Compute the time derivative of the wobble rotation matrix.
C   Compute the X derivative component
       call drott(wobx,dwobx,2,rwx)
C   Compute the Y derivative component
       call drott(woby,dwoby,1,rwy)
C   Compute the two components of the derivative
       call mmul2 ( rwy, RWOBX, rw1 )
       call mmul2 ( RWOBY, rwx, rw2 )
C   Multiply together to get the total time derivative of the wobble rotation
C   matrix
       call madd2( rw1, rw2, RW(1,1,2) ) 
C
C    Check KWOBC to determine if the wobble module is to be turned off.
      IF ( KWOBC .EQ. 1 )  THEN
        CALL ROTAT ( 0.D0, 3, RW(1,1,1) )
        CALL ROTAT ( 0.D0, 3, RW(1,1,2) )
        WOBXR = 0.0D0
        WOBYR = 0.0D0
      ENDIF
C
 250   CONTINUE
C
C    'PUT' the left-handed wobble values (radians) into the database.
      WOBIN(1) = WOBX
      WOBIN(2) = WOBY
      CALL PUT4 ( 'POLAR XY      ', WOBIN, 2, 1, 1)
C
C    Check KWOBD for debug output.
      If (KWOBD .ne. 0) then 
      WRITE (6,9)
    9 FORMAT (1X, 'Debug output for subroutine WOBG.' )
      WRITE(6,8)' CONVDS  ',CONVDS
    8 FORMAT(A,4D25.16/(7X,5D25.16))
   18 FORMAT(A,3D30.20/(10X,3D30.20))
      WRITE(6,8)' WOBXL,  WOBYL  = ',WOBXL, WOBYL 
      WRITE(6,8)' DWOBXL, DWOBYL = ',DWOBXL, DWOBYL 
      WRITE(6,8)' CT      ',CT   
      WRITE(6,8)' tab_time',tab_time
C
      WRITE ( 6, 9200 )  UTC, XJD, RW
      WRITE(6,8)'  WOBX,  WOBY = ', WOBX,  WOBY 
      WRITE(6,8)' DWOBX, DWOBY = ',DWOBX, DWOBY 
      WRITE(6,18)' RWOBX  = ', RWOBX
      WRITE(6,18)' RWX    = ', RWX
      WRITE(6,18)' RWOBY  = ', RWOBY
      WRITE(6,18)' RWY    = ', RWY
      WRITE(6,18)' rw1    = ', rw1
      WRITE(6,18)' rw2    = ', rw2
C
 9200 FORMAT (1X,' UTC = ',D26.16,9X,'XJD = ',D30.16,/,
     .           ' RW = ',3D30.20,/,5(6x,3d30.20,/) )
C
C     Normal Conclusion.
      endif
      END
C
C*****************************************************************************
      SUBROUTINE WOBP ( CFBASE, RDNP, RN, RP, RS, STAR, EARTH)
      Implicit None
C
C    WOBP is the partial derivatives section of the Wobble module. It computes
C    the partial derivatives of the delay and rate with respect to the polar
C    motion offsets.
C
C     Calling sequence -
C           Input Variables:
C             1. CFBASE(3)  -  The crust fixed baseline vector. (m)
C             2. RDNP(3,3)  -  The diurnal polar motion portion of the complete
C                              crust fixed to J2000.0 rotation matrix.
C             3. RN(3,3,2)  -  The nutation portion of the complete crust fixed
C                              to J2000.0 rotation matrix and its CT time
C                              derivative. (unitless, 1/sec)
C             4. RP(3,3,2)  -  The precession portion of the complete crust
C                              fixed to J2000.0 rotation matrix and its CT time
C                              derivative. (unitless, 1/sec)
C             5. RS(3,3,3)  -  The diurnal spin portion of the complete crust
C                              fixed to J2000.0 rotation matrix and its first
C                              two CT time derivatives. (unitless, 1/s, 1/s**2)
C             6. STAR(3)    -  The J2000.0 source unit vector.
C             7. EARTH(3,3) -  The SSBC position, velocity, and acceleration
C                              vectors of the Earth. (m, m/s, m/s**2)
C
C     Common blocks used -
C
      INCLUDE 'cphys.i'
C            Variables 'FROM':
C              1. VLIGHT    -  The velocity of light in a vacuum. (m/s)
C              2. VLIGHT2   -  The velocity of light squared. (m**2/s**2)
C
      INCLUDE 'cmwob.i'
C
C            Variables 'FROM':
C              1. RWOBX(3,3) - The rotation matrix which rotates about the crust
C                              fixed Y-axis by an angle equal to WOBX. 
C              2. RWOBY(3,3) - The rotation matrix which rotates about the new
C                              X-axis by an angle equal to WOBY. (unitless)
C              3. WOBX       - The left-handed X-wobble. (rad)
C              4. DWOBX      - Time derivative of WOBX. (rad/sec)
C              5. WOBY       - The left-handed Y-wobble. (rad)
C              6. DWOBY      - Time derivative of WOBY. (rad/sec)
C              7. rwx(3,3)   - Rotation matrix used in computation of time 
C                              derivative of wobble rotation matrix.
C              8. rwy(3,3)   - Rotation matrix used in computation of time 
C                              derivative of wobble rotation matrix.
C            Variables 'TO':
C              1. DWOBP(2,2) - The partial derivatives of the delay and rate
C                              with respect to the X and Y polar motion. The
C                              first index runs over the X and Y offsets
C                              and the second runs over delay and rate.
C                              (s/radian, (s/s)/radian)
C
      INCLUDE 'ccon.i'
C            Variables 'FROM':
C              1. KWOBC  -  The wobble module flow control flag.
C              2. KWOBD  -  The wobble module debug output flag.
C
C     Program specifications -
C
      Real*8  CFBASE(3), DWOBDX(3,3), DWOBDY(3,3), RDNP(3,3),
     1        RN(3,3,2), RP(3,3,2), RS(3,3,3), STAR(3), WOBXDX(3,3),
     2        WOBYDY(3,3), XBASE(3,2), XR2000(3,3,2), YBASE(3,2),
     3        YR2000(3,3,2),EARTH(3,3), VG(3), c1, c2, tt, DOTP
      Real*8  pRWdx(3,3), pRWdy(3,3), prwxdx(3,3), prwydy(3,3),
     .        pRWx1(3,3), pRWx2(3,3), pRWy1(3,3), pRWy2(3,3),
     .        pdRWdtdx(3,3), pdRWdtdy(3,3), XRW2000(3,3,2), 
     .        YRW2000(3,3,2), rx1(3,3), rx2(3,3), rx3(3,3), rx4(3,3), 
     .        ry1(3,3), ry2(3,3), ry3(3,3), ry4(3,3)
      Integer*4 K, I
C
C     Database access:
C            'PUT' Variables:
C              1. DWOBP(2,2)  -  The polar motion partials. (See above.)
C
C    Subroutine Interface -
C             Caller subroutines: DRIVP
C             Called subroutines: DOTP, DROTT, MMUL2, MMUL5, PUT4, VECRT
C
C    Constants used - VLIGHT, VLIGHT2
C
C    Program Variables -
C           1. pRWdx(3,3)    - The partial derivative of the wobble matrix with
C                              respect to the x-offset. (1/radian)
C           2. pRWdy(3,3)    - The partial derivative of the wobble matrix with
C                              respect to the y-offset. (1/radian)
C           3. WOBXDX(3,3)   - The partial derivative of the rotation matrix
C                              RWOBX with respect to the X offset. (1/radian)
C           4. WOBYDY(3,3)   - The partial derivative of the rotation matrix
C                              RWOBY with respect to the Y offset. (1/radian)
C           5. XBASE(3,2)    - The partial derivatives of the J2000.0 baseline
C                              position and velocity vectors with respect to
C                              the X offset. (m/radian, (m/s)/radian)
C           6. YBASE(3,2)    - The partial derivatives of the J2000.0 baseline
C                              position and velocity vectors with respect to
C                              the Y offset. (m/radian, (m/s)/radian)
C           7. XR2000(3,3,2) - The partial derivative of the complete crust
C                              fixed to J2000.0 rotation matrix and its first
C                              CT time derivative with respect to the X offset.
C                              (1/radian,(1/x)/radian)
C           8. YR2000(3,3,2) - The partial derivative of the complete crust 
C                              fixed to J2000.0 rotation matrix and its first
C                              CT time derivative with respect to the Y offset.
C                              (1/radian, (1/s)/radian)
C           9. vg(3)         - A local copy of the SSBC velocity vector of the
C                              Earth. See EARTH.
C          10. c1, c2, tt    - Temporary variables used to simplify the 
C                              expression for computing the partials.
C          11. prwxdx        -
C          12. prwxdy        -
C
C     Programmer - Dale Markham 02/17/77
C      77.07.14  Peter Denatale
C      77.03.28  Kathy Watts
C      77.12.23  Bruce Schupler
C      89.07.26  Jim Ryan  Documentation simplifed and strings.
C      89.10.05  Jim Ryan  CPHYS common made an include file.
C      89.12.12  Jim Ryan  UNIX-like database interface implimented.
C      91.06.06  Jim Ryan  Documentation cleanup.
C      91.11.25  Jim Ryan  Second term in Shapiro's model added to
C                          the computation of the partials.
C      94.04.15  D. Gordon Converted to Implicit None.
C      94.05.23  D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
C                          being dimensioned both Logical*2 and Real*8.
C      95.11.27  D. Gordon Corrected error in second part of DWOBP(2,1).
C      96.02.09  D. Gordon Added dwobx, dwoby, rwx, and rwy to Common/WOBCM/.
C                          Put in all 4 terms for computation of the partials
C                          of the first derivative of the J2000.0 rotation
C                          matrix w.r.t X-pole and Y-pole. Changed DWOBDX to
C                          pRWdx and DWOBDY to pRWdx and reversed order of
C                          multiplication to compute them.  
C      98.05.01  D. Gordon Common /WOBCM/ moved into include file cmwob.i.
C
C    WOBP Program Structure
C
C    Compute the partial derivatives of the rotation matrices RWOBX and RWOBY
C    with respect to the long period X and Y offsets, respectively.
      CALL DROTT ( WOBX, 1.D0, 2, WOBXDX )
      CALL DROTT ( WOBY, 1.D0, 1, WOBYDY )
C
C   Compute partials of the wobble rotation matrix, RW(1,1,1), with respect to
C    X-pole and Y-pole.
        call mmul2 (RWOBY, WOBXDX, pRWdx )
        call mmul2 (WOBYDY, RWOBX, pRWdy )
C
C   Compute the partial derivatives of rwx w.r.t. X-pole and of rwy w.r.t.
C    Y-pole.
        call ddrot ( wobx, dwobx, 2, prwxdx )      ! ?????????
        call ddrot ( woby, dwoby, 1, prwydy )      ! ?????????
C
C   Compute partials of the time derivative of the wobble rotation matrix,
C    RW(1,1,2), with respect to  X-pole and Y-pole.
        call mmul2 ( rwy,   WOBXDX, pRWx1   )
        call mmul2 ( RWOBY, prwxdx, pRWx2   )
        call madd2 ( pRWx1, pRWx2,  pdRWdtdx )
C
        call mmul2 ( prwydy, RWOBX, pRWy1   )
        call mmul2 ( WOBYDY, rwx,   pRWy2   )
        call madd2 ( pRWy1,  pRWy2, pdRWdtdy )
C
C    Compute the partial derivative of the complete crust fixed to J2000.0
C    rotation matrix and its first CT time derivative with respect to the
C    long period wobble X and Y offsets. 
C          Partials of R2000(1,1,1) w.r.t. X-pole and Y-pole
        CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,1), RDNP, pRWdx,
     1              XR2000(1,1,1) )
        CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,1), RDNP, pRWdy,
     1              YR2000(1,1,1) )
C   
C          Partial of R2000(1,1,2) w.r.t. X-pole
C   Compute the four terms necessary for the calculation.
      CALL MMUL5 ( RP(1,1,2), RN(1,1,1), RS(1,1,1), RDNP, pRWdx, rx1 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,2), RS(1,1,1), RDNP, pRWdx, rx2 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,2), RDNP, pRWdx, rx3 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,1), RDNP, pdRWdtdx, rx4) 
C   Add the four terms to complete the calculation.
      CALL MADD4 ( rx1, rx2, rx3, rx4, XR2000(1,1,2) )
C
C          Partial of R2000(1,1,2) w.r.t. Y-pole
C   Compute the four terms necessary for the calculation.
      CALL MMUL5 ( RP(1,1,2), RN(1,1,1), RS(1,1,1), RDNP, pRWdy, ry1 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,2), RS(1,1,1), RDNP, pRWdy, ry2 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,2), RDNP, pRWdy, ry3 )
      CALL MMUL5 ( RP(1,1,1), RN(1,1,1), RS(1,1,1), RDNP, pdRWdtdy, ry4) 
C   Add the four terms to complete the calculation.
      CALL MADD4 ( ry1, ry2, ry3, ry4, YR2000(1,1,2) )
C.........................................................................
C
C    Compute the partial derivatives of the J2000.0 baseline positon and
C    velocity vectors with respect to the long period wobble X and Y offsets.
C
      DO 300  K = 1,2
        CALL VECRT ( XR2000(1,1,K), CFBASE, XBASE(1,K) )
        CALL VECRT ( YR2000(1,1,K), CFBASE, YBASE(1,K) )
  300 CONTINUE
C
C    Compute the partial derivatives of the delay and of the delay rate with
C    respect to the long period wobble X and Y offsets. First two terms in
C    Shapiro's model included.
C
      Do i = 1,3
        vg(i) = EARTH(i,2)
      Enddo
      c1 = 1.d0/VLIGHT
      c2 = 1.d0/VLIGHT2
      tt = 1.d0-c1*Dotp(Star,VG)
C
      DWOBP(1,1)=c1*Dotp(XBASE(1,1),STAR )*tt+c2*Dotp(Xbase(1,1),vg)
      DWOBP(1,2)=c1*Dotp(XBASE(1,2),STAR )*tt+c2*Dotp(Xbase(1,2),vg)
      DWOBP(2,1)=c1*Dotp(YBASE(1,1),STAR )*tt+c2*Dotp(Ybase(1,1),vg)
      DWOBP(2,2)=c1*Dotp(YBASE(1,2),STAR )*tt+c2*Dotp(Ybase(1,2),vg)
C
C    'PUT' the wobble partial derivatives.
      CALL PUT4 ('WOB PART      ', DWOBP, 2, 2, 1 )
C
C    Check KWOBD for debug.
      If (KWOBD .ne. 0) then
      WRITE (6,9)
    7 FORMAT(A,3D25.16,5(/,9X,3D25.16))
    8 FORMAT(A,4D25.16/(7X,5D25.16))
    9 FORMAT (1X, 'DEBUG OUTPUT FOR SUBROUTINE WOBP.' )
      WRITE(6,7)' WOBX, DWOBX ', WOBX, DWOBX  
      WRITE(6,7)' WOBY, DWOBY ', WOBY, DWOBY  
      WRITE(6,7)' WOBXDX  ', WOBXDX
      WRITE(6,7)' WOBYDY  ', WOBYDY
      WRITE(6,7)' RWOBX   ',RWOBX
      WRITE(6,7)' RWOBY   ',RWOBY
      WRITE(6,7)' pRWdx   ', pRWdx   
      WRITE(6,7)' pRWdy   ', pRWdy   
      WRITE(6,7)' rwx     ', rwx     
      WRITE(6,7)' prwxdx  ', prwxdx  
      WRITE(6,7)' rwy     ', rwy     
      WRITE(6,7)' prwydy  ', prwydy  
      WRITE(6,7)' pRWx1   ', pRWx1   
      WRITE(6,7)' pRWx2   ', pRWx2   
      WRITE(6,7)' pdRWdtdx', pdRWdtdx
      WRITE(6,7)' pRWy1   ', pRWy1   
      WRITE(6,7)' pRWy2   ', pRWy2   
      WRITE(6,7)' pdRWdtdy', pdRWdtdy
      WRITE(6,7)' XR2000  ', XR2000  
      WRITE(6,7)' rx1     ', rx1     
      WRITE(6,7)' rx2     ', rx2     
      WRITE(6,7)' rx3     ', rx3     
      WRITE(6,7)' rx4     ', rx4     
      WRITE(6,7)' YR2000  ', YR2000  
      WRITE(6,7)' ry1     ', ry1     
      WRITE(6,7)' ry2     ', ry2     
      WRITE(6,7)' ry3     ', ry3     
      WRITE(6,7)' ry4     ', ry4     
      WRITE(6,8)' DWOBP   ',DWOBP
      WRITE(6,8)' VLIGHT, VLIGHT2 ', VLIGHT, VLIGHT2
      WRITE(6,8)' XBASE   ',XBASE
      WRITE(6,8)' YBASE   ',YBASE
      WRITE(6,8)' c1, c2, tt ', c1, c2, tt
      WRITE(6,8)' vg      ',vg
      WRITE ( 6, 9200 )  CFBASE, RDNP, RN, RP, RS, STAR
 9200 FORMAT (1X, 'CFBASE = ', 3 ( D30.16, 10X ), /, 1X,
     1            'RDNP   = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     2            'RN     = ', 6 ( 3 ( D30.16, 10X ), /, 1X ),
     3            'RP     = ', 6 ( 3 ( D30.16, 10X ), /, 1X ),
     4            'RS     = ', 9 ( 3 ( D30.16, 10X ), /, 1X ),
     5            'STAR   = ', 3 ( D30.16))
C
C     Normal conclustion.
      endif
      END
C
C******************************************************************************
      SUBROUTINE WOBC
      Implicit None
C
C    WOBC is the contributions section of the wobble module. It computes the 
C    contributions to the delay and rate due to polar motion X and Y offsets.
C
C     Common blocks used -
C
      INCLUDE 'cmwob.i'
C            Variables 'FROM':
C              1. DWOBP(2,2)  -  The polar motion partials. (See above.)
C
      INCLUDE 'ccon.i'
C            Variables 'FROM':
C              1. KWOBC  -  The wobble module flow control flag.
C              2. KWOBD  -  The wobble module debug output flag.
C
C    Program specifications -
      Real*8  dwobxc(2), dwobyc(2)
      Integer*4 K, J
C
C     Database access -
C            'PUT' Variables:
C              1. DWOBXC(2) - The contributions to the delay and rate due to 
C                             the X-pole offset. (s, s/s)
C              2. DWOBYC(2) - The contributions to the delay and rate due to 
C                             the Y-pole offset. (s, s/s)
C
C     External I/O - Possible debug output.
C
C     Subroutine interface -
C             Caller subroutines: DRIVC
C             Called subroutines: PUT4
C
C     Program variables - None
C
C     Programmer - Dale Marknam  02/17/77
C      77.07.14  Peter Denatale
C      77.03.28  Kathy Watts
C      77.12.23  Bruce Schupler
C      89.07.26  Jim Ryan  Documentation simplifed and strings.
C      89.12.12  Jim Ryan  UNIX-like database interface implimented.
C      91.06.06  Jim Ryan  Documentation cleaned up.
C      91.12.17  D. Gordon Bug fixed. For KWOBC = 2 or 3, contributions were
C                          incorrectly being set to zero. (Calc 7.n versions)
c      94.04.15  D.Gordon  Converted to Implicit None.
C      94.05.23  D.Gordon  Fixed bug - use_cubic, use_linear, use_spline were
C                          being dimensioned both Logical*2 and Real*8.
C      94.06.29  D.Gordon  Wobble contribution (DWOBC(2,2)) split into X and Y
C                          components (dwobxc(2) and dwobyc(2)) for 
C                          compatability with SOLVE. 
C      95.10.05  D.Gordon  Corrected wobble contributions when KWOBC=1 (Wobble
C                          module turned OFF). DO loops restructured.
C      95.11.27  D.Gordon  Added DWOBXC and DWOBYC to debug printout, removed
C                          DWOBC which is no longer used. Documentation
C                          corrected.
C      98.05.01  D. Gordon Common /WOBCM/ moved into include file cmwob.i.
C
C  WOBC Program Structure
C
C   Compute the contributions.
      do k=1,2
        dwobxc(k) = DWOBP(1,K) * WOBX
        dwobyc(k) = DWOBP(2,K) * WOBY
      enddo
C
C   Check KWOBC to see if wobble is to be turned off.
      IF (KWOBC .EQ. 1)  Then
        do k=1,2
            dwobxc(k) = 0.d0
            dwobyc(k) = 0.d0
        enddo
      ENDIF
C
C   PUT the wobble contributions.
      CALL PUT4 ('WOBXCONT      ', dwobxc, 2, 1, 1 )
      CALL PUT4 ('WOBYCONT      ', dwobyc, 2, 1, 1 )
  300 continue
C
C   Check KWOBD for debug output.
      IF ( KWOBD .EQ. 0 )  GO TO 600
      WRITE ( 6, 9 )
    9 FORMAT (1X, 'Debug output for subroutine WOBC.' )
      WRITE(6,8)' DWOBXC  ',DWOBXC
      WRITE(6,8)' DWOBYC  ',DWOBYC
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DWOBP   ',DWOBP
      WRITE(6,8)' WOBX, WOBY ', WOBX, WOBY
C
C     Normal conclusion.
  600 RETURN
      END
C
C******************************************************************************
      SUBROUTINE WOBMU(XJD,tab_time,WOBXL,WOBYL,DWOBXL,DWOBYL)
      Implicit None
C
C    WOBMU is the Wobble module utility. It provides interpolated values of the
C    X and Y wobble at the epochs specified in the calling sequence. If the
C    specified epoch is outside the range of the interpolation table, the
C    program issues a message and stops.
C
C     Calling sequence - CALL WOBMU(XJD,UTC,WOBXL,WOBYL)
C
C      Input Variables:
C        1) XJD     -  The Julian date at 0:00 hours UTC of the epoch for
C                      interpolation. (days)
C        2) tab_time - The fraction of the day in the timescale of the
C                      input EOP series (CT, UTC, or ???).
C
C      Output variables:
C        1) WOBXL  -  The interpolated value of the X wobble.
C                     (left-handed, milliarcsec)
C        2) WOBYL  -  The interpolated value of the Y wobble.
C                     (left-handed, milliarcsec)
C
C     Common blocks used -
C
      INCLUDE 'cmwob.i'
C        Variables 'FROM':
C          1) WOBIF(3)    - The wobble information array. (See above.)
C          2) XYWOB(2,10) - The tabular points for the X & Y offsets.
C                           (milliarcsec)
C
      INCLUDE 'ccon.i'
C     Variables 'FROM':
C        1) KWOBC - The Wobble module flow control flag.
C                   0 --> Default, module on; spline interpolation for 1-day
C                         series, cubic for 5-day series.
C                   1 --> Module off. No polar motion applied.
C                   2 --> Module on; linear interpolation for any series. 
C                   3 --> Module on; cubic interpolation for 1-day series, 
C                         spline for 5-day series. 
C        2) KWOBD - The wobble module debug control flag.
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C            Variables 'FROM':
C              1. SECDAY - The number of seconds in a day (s/day). 
C
C     Program specifications -
C
      Real*8 XJD, UTC, WOBXL, WOBYL, T, DWOBXL, DWOBYL, S, tab_time
      Real*8 XINT(4,2), F2(2), Y1(2,2), Y2(2,2)
      Real*8 x_spline, yx_spline, yy_spline, yxdot, yydot, yxdot2,
     *       yydot2, yxdot3, yydot3
      Integer*4 INT, I, N, NN, NR, ILAST, ierr4
C
C     Subroutine interface:
C         Caller subroutines: WOBI, WOBG
C         Called subroutines: DFLOTJ, IDINT
C
C     Programmer - Bruce Schupler 12/23/77
C      89.07.26  Jim Ryan  Documentation simplifed and strings.
C      90.05.06  Jim Ryan  Mods for linear interpolation.
C      91.06.06  Jim Ryan  Linear interpolation mods and clean up.
C      93.12.17  D. Gordon Spline interpolation mods.
C      94.04.13  D. Gordon DFLOT changed to Fortran 77 DFLOTJ.
C      94.04.15  D. Gordon Converted to Implicit None.
C      94.05.23  D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
C                          being dimensioned both Logical*2 and Real*8.
C      95.06.09  D. Gordon Cleaned up debug printout.
C      95.12.11  D. Gordon Adding computation of time derivatives of X- and 
C                          Y-wobble for linear interpolation.
C      95.12.28  D. Gordon Adding computation of time derivatives of X- and 
C                          Y-wobble for cubic polynomial interpolation. Debug
C                          printout mods and additions.
C      98.05.01  D. Gordon Common /WOBCM/ moved into include file cmwob.i.
C
C     WOBMU Program Structure
C
C    Convert UTC and XJD to the units of the interval of the table relative to
C    the first point of the table. INT is computed so that INT+2 is the number
C    of the tabular point just before (or at) the time of the observation and
C    INT+3 is the number of the point after the time of the observation.
C
      T =  (XJD  -  WOBIF(1))
      T = (T + tab_time) / WOBIF(2)
      INT = T
C WEW - DFLOTJ not available in G77 - hence
C     T =  T - DFLOTJ (INT)
      T =  T - INT
      INT = INT - 1
C
C**************************************
C   Code for cubic spline interpolation
      IF (use_spline) then     ! Spline interpolation
C
C   Compute time of obs. and divide by interval
        x_spline = (xjd + tab_time)/ WOBIF(2)
C
C   In case the interval (WOBIF(2)) is not 1.0 days, divide by interval
C       x_spline = x_spline / WOBIF(2)
C
C  Perform cubic spline for X-wobble interpolation
        call splint4(xa,yax,y2sx,n_spline,x_spline,yx_spline,yxdot,
     *               yxdot2,yxdot3,ierr4)
        WOBXL = yx_spline
        DWOBXL = yxdot / (WOBIF(2) * SECDAY) 
C
C  Perform cubic spline for Y-wobble interpolation
        call splint4(xa,yay,y2sy,n_spline,x_spline,yy_spline,yydot,
     *               yydot2,yydot3,ierr4)
        WOBYL = yy_spline
        DWOBYL = yydot / (WOBIF(2) * SECDAY) 
C
      Endif               ! Spline interpolation
C
C**************************************
C   Code for cubic polynomial interpolation
      If(use_cubic) Then            !3rd order polynomial interpolation
C
C   Select the four tabular points from each of the XWOB and YWOB arrays and
C   verify that the interpolation is not outside the range of the table.
C   (NOTE: If the interpolation is outside the range of the table, a message is
C    written and the program terminates.)
C
        ILAST = IDINT ( WOBIF(3) )
        Do N = 1,4
          NN = INT  +  N
          IF ( ( NN .LT. 1 ) .OR. ( NN .GT. ILAST ) )  GO TO 900
          XINT(N,1) = XYWOB(1,NN)
          XINT(N,2) = XYWOB(2,NN)
        Enddo
C
C    Do the cubic interpolation.
        DO I = 1,2
          DO N = 1,2
            NR = N + 1
            F2(I) = ( XINT ( NR+1, I ) + XINT ( NR-1, I ) ) / 6.D0
            Y1(N,I) = + ( 4.D0 / 3.D0 ) * XINT ( NR, I ) - F2(I)
            Y2(N,I) = - ( 1.D0 / 3.D0 ) * XINT ( NR, I ) + F2(I)
          enddo    
        enddo    
C
       S = 1.D0 - T
       WOBXL = T * ( Y1(2,1)  +  T**2 * Y2(2,1) )
     .       + S * ( Y1(1,1)  +  S**2 * Y2(1,1) )
       WOBYL = T * ( Y1(2,2)  +  T**2 * Y2(2,2) )
     .       + S * ( Y1(1,2)  +  S**2 * Y2(1,2) )
C
       DWOBXL = ( Y1(2,1) + (3.D0*T**2*Y2(2,1)) 
     .          - Y1(1,1) - (3.D0*S**2*Y2(1,1)) ) / (WOBIF(2)*SECDAY)
       DWOBYL = ( Y1(2,2) + (3.D0*T**2*Y2(2,2)) 
     .          - Y1(1,2) - (3.D0*S**2*Y2(1,2)) ) / (WOBIF(2)*SECDAY)
C
      Endif                         !3rd order polynomial interpolation
C
C**************************************
C   Code for linear interpolation
      If(use_linear) Then           !2 point linear interpolation
        If(INT+1 .lt. 1  .or.
     .     INT+1 .gt. DINT(WOBIF(3)+.001)) Then
          Write(6,'(
     .    "Error in WOBMU! Attempted to interpolate polar motion",/,
     .    "outside of PM table. INT =",i5)') INT
       CALL CKILL(6hWOBMU ,0,0)
        Endif
C
C   Interpolate linearly. Don't change the units.
        WOBXL = (XYWOB(1,INT+3)-XYWOB(1,INT+2))*T + XYWOB(1,INT+2)
        WOBYL = (XYWOB(2,INT+3)-XYWOB(2,INT+2))*T + XYWOB(2,INT+2)
C   Compute time derivative.
        DWOBXL =  (XYWOB(1,INT+3)-XYWOB(1,INT+2)) / (WOBIF(2) * SECDAY) 
        DWOBYL =  (XYWOB(2,INT+3)-XYWOB(2,INT+2)) / (WOBIF(2) * SECDAY) 
C
      Endif                         !2 point linear interpolation
C
C**************************************
C    Check to see if debug output is requested. 
      IF (KWOBD .NE. 1) GO TO 400
      WRITE(6,9)
      If(use_spline) write(6,'(" Spline interpolation used.")')
      If(use_cubic) write(6,'(" Cubic polynomial interpolation used.")')
      If(use_linear) write(6,'(" Linear interpolation used.")')
      WRITE(6,8)' T       ',T
      WRITE(6,7)' INT     ',INT
      WRITE(6,8)' XYWOB   ',XYWOB
      WRITE(6,9120) XJD, tab_time, WOBXL, WOBYL, DWOBXL, DWOBYL  
C
      if (use_spline) then
        WRITE(6,8)' x_spline ', x_spline
        write (6,'("  xa: ",5d20.10,3(/,6x,5d20.10))') xa
        write (6,'("n_spline",i5,"x_spline ",d25.16)') n_spline,x_spline
        write (6,'(" yax: ",5d20.10,3(/,6x,5d20.10))') yax
        write (6,'("y2sx: ",5d20.10,3(/,6x,5d20.10))') y2sx
        write (6,8)' yx_spline ', yx_spline
        write (6,8)' yxdot, yxdot2, yxdot3 ', yxdot, yxdot2, yxdot
        write (6,'(" yay: ",5d20.10,3(/,6x,5d20.10))') yay
        write (6,'("y2sy: ",5d20.10,3(/,6x,5d20.10))') y2sy
        write (6,8)' yy_spline ', yy_spline
        write (6,8)' yydot, yydot2, yydot3 ', yydot, yydot2, yydot
      endif 
C
      if (use_cubic) then 
        WRITE(6,7)' ILAST ', ILAST
        WRITE(6,8)' XINT  ', XINT
        WRITE(6,8)' F2    ', F2
        WRITE(6,8)' Y1    ', Y1
        WRITE(6,8)' Y2    ', Y2
        WRITE(6,8)' S     ',  S
      endif 
C
    7 FORMAT(A,15I8/(9X,15I8))
    8 FORMAT(A,4D25.16/(7X,5D25.16))
    9 FORMAT(1X,'Debug output for subroutine WOBMU')
 9120 FORMAT(1X,'   XJD = ',D30.16,3X,'tab_time = ',D30.16,/,
     .       1X,' WOBXL = ',D30.16,3X,'   WOBYL = ',D30.16,/,
     .       1X,'DWOBXL = ',D30.16,3X,'  DWOBYL = ',D30.16)
C
C     Normal Conclusion.
  400 RETURN
C
C     Abnormal conclusion.
  900 WRITE ( 6, 9300 )  NN, ILAST
 9300 FORMAT (1X, ' CALC has terminated in subroutine WOBMU.',/,
     . ' The interpolation is outside the range of the wobble table.',
     . /,'  NN = ', I2, ' ILAST = ', I2, '.' )
      CALL CKILL (6HWOBMU , 0, 0)
      END
