      SUBROUTINE dSTART (Num_Scans, Kjob)
      IMPLICIT None
!
!           Input variables:
!             1) calc_file_name - Job file '.calc' file name.
!
!     CHARACTER*128 calc_file_name, Finite_name
      Logical*4     Finite_model
!
!      Common blocks used -
!
      INCLUDE 'ccon.i'
!       Variables 'to':
!         1. ILUOUT - A flag controlling output.
!
      INCLUDE 'cmxst11.i'
!       Variables 'to':
!         1. NUMSIT    - The total number of sites in the data base.
!         2. Zero_site - The site number of the site at the geocenter.
!
      INCLUDE 'cmxsr11.i'
!       Variables 'to':
!         1. NUMSTR - The total number of stars (radio sources) in the
!                     data base.
!
      INCLUDE 'cmxut11.i'
!       Variables 'to':
!         1. Xintv(2)    - First and last Julian Date of data in the
!                          current data base.
!         2. Intrvl(5,2) - First and last time tag of data in the current
!                          data base. (First index: year, month, day,
!                          hour, minute. Second index: first, last.)
!
      INCLUDE 'param11.i'
!       Variables from:
!         1. A_tilts    - Antenna tilts file name.
!         2. OC_file
!         3. DFLEAP
!         4. OPTL_file
!         5. JPL_DE421
!
      INCLUDE 'd_input.i'
!
      INCLUDE 'c2poly.i'
!
!
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
!       VARIABLES 'TO':
!         1. ROTEPH(2,20)- The array which contains the epochs at which
!                          TAI - UT1 is desired. The entries are:
!                          1) JD at 0:00 hours UTC,
!                          2) The fraction of a UTC day from 0:00 hours
!                          to the desired epoch.
!
      Real*8    XCALC, FJLDY
      Integer*2 NFLAG,NFLAGC,loadm(8),LFILE(3)
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
!
!     Integer*4 gethostname, ierr4
      Integer*4 imode, ILU, iout, idd1, Kjob
      Character*64 Computing_center
      Character*6 C_LFILE
      Character*128 CALCON_NAME
      Character*75 Lhist1
      Equivalence (C_LFILE,LFILE)
!
!       Variables 'from':
!         1. loadm(8)  - The load module compilation date message.
!         2. NFLAG     - The total number of CALC flow control and debug flags.
!         3. LFILE(3)  - The name of the CALC control file.
!         4. XCALC     - The CALC version number.
!         5. Kjob      - Incrmental job number. If .gt. 1, do not call get4unit.
!
!   Program specifications -
!
      Real*8    JDY2K
      Real*8    FJD2, Fday, Fhr, Fmin
      Real*8    JD1, JD2, Xmin, StrtUTCmin, StopUTCmin, ProcMin 
      Integer*4 Min2, MinStrt, MinStop, StopSec, StopMin, Num2Min,     &
     &          StopHr, StopDay, StopMo, StopYr
!
!     Integer*4 Iyear, Imonth, Iday
      Integer*4  IFLAG(62)
      Integer*2  LCALC(40), LFCIO(40), LHIST(66), LNAME(5), LNAMO(5), &
     &           IBUF(40),      trimlen, host_len, hist_len
      Integer*4 Ipid, Getpid, jj, IOS
      Real*8 xleap(5), tol
      CHARACTER LNAME_chr*10, LNAMO_chr*10, LFCIO_chr*80, LHIST_chr*132
      Character*8  Ich8
      Character*80 Ich80
      CHARACTER*80 CBUF,CTIME*12,CDATE*16
      EQUIVALENCE  ( IFLAG(1),  KATMC ),  ( LCALC(1), LHIST(1) ), &
     &             ( IBUF, CBUF), (LNAME,LNAME_chr), &
     &             ( LNAMO,LNAMO_chr), (LFCIO,LFCIO_chr), &
     &             ( LHIST,LHIST_chr)
      Integer*4 get4unit
!     Integer*2 IPAR(5),          kruc, iveri, lfvo, isame, ivero, &
!    &          kerr, NDO(3), idd2
      Integer*4 I, N, Unit1, Unit2, isz, iup, MXUTPM, Num_Scans
      Integer*4 IMNTHS(12)
      DATA IMNTHS /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      Character*80 xtlt
      Character*24 STR24
!     Save      Unit1, Unit2
!     CHARACTER*10 IPAR_C
!     EQUIVALENCE (IPAR,IPAR_C)
!     DATA  KRUC / 2 /
!     Data  Lhist1 / &
!    &   ' &
!    &             '/
!     Data Unit1 /0/
!
!
!     External I/O
!       Input variables:
!         1. Non-default model module and utility routine flow control and
!                          debug output flags.
!       Output variables:
!         1. All model and module utility routine flow control and debug
!                          output flags.
!
!     Program variables -
!         1. IFLAG(NFLAG) -  The variable used to initialize all flow
!                            control and debug output flags.
!         7. Intrvl(5,2)  -  First and last UTC tag for data in the current
!                            data base. (First index - year(2 digits),
!                            month(1-12), day of month, hours, minutes)
!
!     Programmer - David Gordon Jan-Apr, 2013 
!
!   dSTART Program Structure
!
      ILUOUT = iout
      MXUTPM = 20
!
!   Initialize all flow control and debug output flags to zero.
      DO 400 N = 1, NFLAG
 400    IFLAG(N) = 0
!
!
!   Get the apriori's from the .calc file.
      Call dGet_input(Kjob)
       Num_Scans = NumScans
!
! Set Geocenter station;
      Zero_site = 1
      SITES(1) = 'GEOCENTR'
      Axis(1)  = 'AZEL'
      SITAXO(1) = 0.D0
      SITXYZ(1,1) = 0.D0
      SITXYZ(2,1) = 0.D0
      SITXYZ(3,1) = 0.D0
       NUMSIT = NUMSIT + 1
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     Get the start and stop times, all on the even minute
!       Intrvl(1,1) = StartYr
!       Intrvl(2,1) = StartMo
!       Intrvl(3,1) = StartDay
!       Intrvl(4,1) = StartHr
!       Min2 = StartMin/2
!       MinStrt = Min2*2
!       Intrvl(5,1) = MinStrt 
!       JD1    = JDY2K (StartYr,StartMo,StartDay)
!       Xintv(1) = JD1 + Intrvl(4,1)/24.D0 + Intrvl(5,1)/1440.D0
!!
!! Add 1 second to stop time to get an additional polynomial interval
!!  if stop time is on the even minute.
!       XMin = StartMin + (StartSec+ScanDur+1)/60.D0 + 120.001D0/60.D0
!       Min2 = Xmin/2
!       MinStop = Min2*2
!       StopSec = 0
!       StopMin = MinStop
!        Num2Min = (MinStop - MinStrt)/2
!       StopHr = StartHr
!!        If (StopMin .ge. 60) Then
!         StopHr = StopHr + StopMin/60
!         StopMin = MOD(StopMin,60)
!        Endif
!       StopDay = StartDay
!        If (StopHr .ge. 24) Then
!         StopDay = StopDay + StopDay/24
!         StopHr = MOD(StopHr,24)
!        Endif
!       StopMo = StartMo
!        If (MOD(StartYr,4) .eq. 0) IMNTHS(2) = IMNTHS(2) + 1
!        If (Stopday .gt. IMNTHS(StartMo)) Then
!         StopMo = StopMo + 1
!         StopDay = StopDay - IMNTHS(StartMo)
!        Endif
!! The scan should not be allowed to cross the year boundary, but 
!!   just in case:
!       StopYr = StartYr
!        If (StopMo .ge. 13) Then
!         StopYr = StopYr + 1
!         StopMo = StopMo - 12
!        Endif
!!
!       Intrvl(1,2) = StopYr
!       Intrvl(2,2) = StopMo
!       Intrvl(3,2) = StopDay
!       Intrvl(4,2) = StopHr
!       Intrvl(5,2) = StopMin
!       JD2    = JDY2K (StopYr,StopMo,StopDay)
!       Xintv(2) = JD2 + Intrvl(4,2)/24.D0 + Intrvl(5,2)/1440.D0
!!
!! Start/Stop time in UTC minutes (1 day = 1440 minutes)
!        StrtUTCmin = (Xintv(1) - JD1) * 1440.D0
!        StopUTCmin = (Xintv(2) - JD1) * 1440.D0
!        ProcMin = StopUTCmin - StrtUTCmin
!!       NumEpochs = ((ProcMin/2.D0 + .001) * 5) + 1
!        NumEpochs = ((ProcMin*60. + .001)/d_interval) + 1
!! Check that the arrays are sized large enough
!        If (NumEpochs .gt. Max_Epoch) Then
!          Write(6,'(/," Requesting ",I4," epochs. Max_epoch is ",I4,    &
!     &    " in c2poly.i.",/)') NumEpochs, Max_Epoch
!          Stop
!        Endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Write(6,1078) Intrvl, StrtUTCmin, StopUTCmin, ProcMin, NumEpochs
!1078 Format('dSTART: Start UTC: ',5I6,/,'         Stop UTC: ',5I6,/,   &
!    &  ' StrtUTCmin, StopUTCmin, ProcMin, NumEpochs: ', 3F8.2,I5)
!       Write(6,*) 'd_interval ', d_interval
!
!      Call YMDJL(FJD2, StopYr, StopMo, StopDay)
!
!  See if we are doing a near-field object
!      Finite_model = .False.
!     If (NumSpace .ge. 1) Finite_model = .True.
!
!     Normal conclusion.
      RETURN
!
      END
!
!**********************************************************************
      BLOCK DATA dSTACMB
      IMPLICIT None
!
! 1.     STABD
!
! 1.1.1  STABD IS THE BLOCK DATA INITIALIZATION SECTION FOR THE START MODULE.
!        IT HOLDS THE LOAD MODULE DATE MESSAGE.
!
! 1.2.2  COMMON BLOCKS USED
      Real*8  XCALC
      Integer*2 NFLAG,NFLAGC,loadm(8),LFILE(3)
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
      Character*64 Computing_center
      CHARACTER*16 LOADM_CHR
      EQUIVALENCE (LOADM,LOADM_CHR)
      CHARACTER*6 C_LFILE
      EQUIVALENCE (C_LFILE,LFILE)
!
!       VARIABLES 'TO':
!         1. LOADM - THE LOAD MODULE COMPILATION DATE MESSAGE.
!         2. NFLAG - THE TOTAL NUMBER OF CALC FLOW CONTROL AND DEBUG FLAGS.
!         3. LFILE(3) - THE NAME OF THE CALC CONTROL FILE.
!         4. XCALC - THE CALC PROGRAM VERSION NUMBER.
!
!     DATA LOADM_CHR /'Ver. 2011.07.19 '/
      DATA NFLAG /62/
!     DATA C_LFILE /'CALCON'/
!     DATA XCALC/10.99D0/
!
! 1.2.9  PROGRAMMER - BRUCE SCHUPLER 05/12/78
!                     BRUCE SCHUPLER 06/05/78
!                     BRUCE SCHUPLER 09/14/78
!                     BRUCE SCHUPLER 12/06/78
!                     BRUCE SCHUPLER 06/06/79
!                     BRUCE SCHUPLER 08/26/80
!                     DAVID GORDON   06/19/84
!                     DAVID GORDON   01/08/85 (REMOVED IDISC=59)
!                     SAVITA GOEL    06/02/87 (CDS FOR A900)
!                     Jim Ryan 89.07.25 Documentation simplified.
!                     Jim Ryan 89.12.12 UNIX-like database interface
!                                    implimented.
!                     David Gordon 94.04.15 Converted to Implicit None
!                     David Gordon 98.07.23 Removed ISECU, IDISC, and IOPEN
!                                    from Common /STACM/.
      END
!**********************************************************************
      SUBROUTINE dGETEOP()
      Implicit None
!
!     Get Earth orientation table from an external file.
!      This routine reads a SOLVE-format EOP Mod file.
!
!     Common blocks used -
!
      INCLUDE 'input11.i'
!            Variables 'from':
!              1. Ex_EOP - File name of the external EOP input file
!
      INCLUDE 'cmxut11.i'
!            Variables 'from':
!              1. Xintv(2)  - First and last Julian Date in the data base
!            Variables 'to':
!              1. UT1IF(4)  - The final UT1 information array. This array
!                             contains respectively: 1) The Julian date of the
!                             first tabular point, 2) The increment in days of
!                             the tabular points, 3) The number of tabular
!                             points, 4) The units of the UT1 tabular array per
!                             second. (days, days, unitless, sec/table unit)
!              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
!                             (table units)
!              3. ISHRTFL   - The short period tidal terms flag, (unitless).
!                             = 1 --> UT1 table coming from input database is
!                             true UT1, (that is, fortnightly tidal terms have
!                             not been removed, as in the IRIS or IERS series).
!                             = -1 --> UT1 table coming from input database is
!                             UT1R, (that is, the Yoder fortnightly tidal terms
!                             HAVE been removed as in Bulletin B).
!                             = -2 --> UT1 table coming from input database is
!                             UT1S, (the S tidal terms HAVE been removed).
!              4. Leap_fix   - Used in external input mode. .True. means
!                              correct the input EOP series for accumluated
!                              leap seconds. .False. means do not correct.
!              5. UT1type    - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '.
!                              For ''UT1-UTC ', leap second corrections
!                              must be made.
!              6. EOP_time_scale - EOP table time scale, allowed values:
!                              'TAI     ', 'TCG     ', 'TDB     ',
!                              'TDT     ', 'UTC     ', 'UNDEF   '.
!                              Assumed default if not present => TDB
!
      INCLUDE 'cmwob11.i'
!            Variables 'to':
!              1. WOBIF(3)  -  The wobble information array. Contains
!                              respectively: 1) The Julian date of the first
!                              tabular point, 2) The increment in days of the
!                              tabular points, 3) The number of tabular points.
!                              (days, days, unitless)
!              2. XYWOB(2,20)- The wobble tabular points for the polar motion
!                              (wobble) X & Y offsets. (milliarcsec)
!                              (Note: Based on old BIH conventions, offsets
!                              are assumed to be left-handed.)
!
!   Program Specifications -
      Real*8      JD1, Uintv, Xmjdm, Mjdm, Mjds, Xmjds, XJDT, X10, Y10, &
     &            UT1T, Xmjdl, JD2
      Integer*4   Get4unit, Iunit, Npts, Nerp, I, IOS
      Character*4 Utflag
      Integer*2   KERR, idd3
      Character   EOP_ID*15, dummy1*1
!
!   Program variables:
!          JD1    - First (full) Julian Day in EOP file
!          JD2    - Last (full) Julian Day in EOP file
!          Uintv  - EOP interval (usually 1.0 days)
!          Npts   - Number of points in the input EOP file
!          Nerp   - Number of trabular points to use in the UT1 and polar
!                   motion interpolation tables
!          Utflag - Type of UT1 data (UT1, UT1R, UT1S)
!          Xmjdm  - Experiment midpoint (FJD)
!          Mjdm   - Experiment midpoint (MJD)
!          Mjds   - Time of first tabular point (MJD)
!          Xmjds  - Time of first tabular point (FJD)
!          Xmjdl  - Time of last tabular point (FJD)
!          Kjob   - Job number. If .gt. 1, do not call get4unit again.
!
!     Programmer:
!      98.04.27  David Gordon - Original program written
!      99.11.19  David Gordon - Bug fix, CLOSE(Iunit) statements added.
!      99.11.23  David Gordon - Setting number of EOP points to 15 always,
!                               for compatability with Dbedit/Apriori and
!                               SOLVE.
!    2000.12.11  David Gordon - Modify to skip comment statements in EOP
!                               mod file.
!    2000.12.29  David Gordon - Modify to read/interpret new EOP mod file
!                               table header record.
!-----------------------------------------------------------------------
!
         Leap_fix = .False.
!
!  Open the EOP file
!
       Iunit = Get4unit()
       OPEN (unit=Iunit, file=Ex_EOP, status='old', Action='READ',      &
     &       err=99, Iostat=Ios)
       Go to 101
  99   Continue
       Write(6,'(" Error opening external EOP file. Quitting! ")')
       Call TERMINATE_CALC ( 'GETEOP', int2(1), int2(IOS))
 101   Continue
!
!  Read new EOP mod file format. 2000.12.29
       Read(Iunit,1016,err=88) EOP_ID, JD1, Uintv, Npts, UT1type, &
     &                  EOP_time_scale
 1016  Format (A15,2X,F9.1,2X,F4.1,2X,I5,2X,A8,2X,A8)
!
       If (UT1type .eq. 'UT1-TAI ') Then               ! Normal case
         Utflag = 'UT1 '
         ISHRTFL =  1
         Leap_fix = .False.
         Go to 102
       Endif                                           ! Normal case
!
       If (UT1type .eq. 'UT1-UTC ') Then               ! Abnormal case
         Utflag = 'UT1 '
         ISHRTFL =  1
!          Will need to subtract leap seconds later (in UT1I)
         Leap_fix = .True.
         Go to 102
       Endif                                           ! Abnormal case
!
!  If here, UT1type not properly defined. ?????
       Write(6,'(" Illegal UT1type in EOP mod file! Quitting! ")')
       Call TERMINATE_CALC ( 'GETEOP', int2(1), int2(IOS))
!
  88   Continue
!  Old format, just in case
       Backspace (Iunit)
       Read(Iunit,1018, err=89) JD1, Uintv, Npts, Utflag
 1018  Format (F9.1, F4.0, I4, 1X, A4)
       Write(6,'(/,"  !!! Using old EOP mod file format !!!",/)')
!
       Leap_fix = .False.
       UT1type = 'UT1-TAI '
       EOP_time_scale = 'UNDEF   '
!   Determine what the short period tidal term flag should be
       IF(Utflag .eq. 'UT1 ') ISHRTFL =  1
       IF(Utflag .eq. 'UT1S') ISHRTFL = -2
       IF(Utflag .eq. 'UT1R') Then
         ISHRTFL = -1
         Write(6,'(" Cannot use UT1R data! Quitting! ")')
         Call TERMINATE_CALC ('GETEOP', int2(1), int2(Ios))
       ENDIF
!
!**   Write(6,'(" ISHRTFL = ",I3)') ISHRTFL
       Go to 102
  89   Continue
       Write(6,'(" Cannot read EOP file! Quitting! ")')
       Call TERMINATE_CALC ('GETEOP', int2(1), int2(Ios))
!
 102  Continue
!
!  Number of points in table. Set to 15 when data interval less than 2.0 days.
!   Increase by 1 for each additional day, up to 20 points.
       Nerp = 15
!   Set to 15 in all cases! 99.11.23 -DG-
!*     Nerp = 14 + (xintv(2)-xintv(1))
!*     If (Nerp .lt. 15) Nerp = 15
!*     If (Nerp .gt. 20) Nerp = 20
!
!  Midpoint of experiment
       Xmjdm = (Xintv(1) + Xintv(2)) / 2.D0
!  First tabular point at midnight prior to (Nerp*Uintv)/2 days before midpoint
       Mjdm = Xmjdm - 2400000.5D0
       Mjds = Dint (Mjdm - (Nerp*Uintv)/2.D0)
!   First point at time Xmjds:
       Xmjds = Mjds + 2400000.5D0
!
!  Check if EOP file does not start early enough:
! !! Require 15 points in all cases!!! 99.11.23 -DG-
       If (Xmjds .lt. JD1) Then
!*       Need at least one point before first observation time in data base
!*       If ( (Xintv(1) - Uintv) .lt. JD1 ) Then
           Write(6,'("GETEOP: Not enough EOP points before database")')
            KERR = 0
            Close (Iunit)
           CALL TERMINATE_CALC ('GETEOP', int2(1), KERR)
!*       Else
!*         Recompute Nerp and reset first tabular point to JD1
!*         Nerp = Nerp - (JD1-Xmjds+.01)/Uintv
!*         Xmjds = JD1
!*       Endif
       Endif
!
!  Check if EOP file ends too early:
! !! Require 15 points in all cases!!! 99.11.23 -DG-
       Xmjdl = Xmjds + (Nerp-1)*Uintv
       JD2 = JD1 + (Npts-1)*Uintv
       If (Xmjdl .gt. JD2) Then
!*       Need at least one point after last observation time in data base
!*       If ( (Xintv(2) + Uintv) .gt. JD2 ) Then
           Write(6,'("GETEOP: Not enough EOP points after database")')
            KERR = 0
            Close (Iunit)
           CALL TERMINATE_CALC ('GETEOP', int2(1), KERR)
!*       Else
!          Recompute Nerp
!*         Nerp = Nerp - ( Xmjdl-JD2+.01)/Uintv
!*       Endif
       Endif
!
!     Write(6,1021) Xintv, Xmjdm, Mjdm, Mjds, Xmjds
!1021 Format ('Xintv(2), Xmjdm, Mjdm, Mjds, Xmjds: ',2F20.8,/,5X,
!    *        4F19.8)
!
!  Fill the UT1 and Wobble information arrays
       UT1IF(1) = Xmjds
       UT1IF(2) = Uintv
       UT1IF(3) = Nerp
       UT1IF(4) = 1.0D0
!**   Write(6,'(" UT1IF(4) ",4F15.6)') UT1IF
!
       WOBIF(1) = Xmjds
       WOBIF(2) = Uintv
       WOBIF(3) = Nerp
!**   Write(6,'(" WOBIF(3) ",3F15.6)') WOBIF
!
!  Get the EOP points:
!   Read till first point found
  50   Continue
       Read (Iunit,*,err=50) XJDT, X10, Y10, UT1T
!      Read (Iunit,1019) XJDT, X10, Y10, UT1T
 1019  Format (F9.1, 2F8.4, I10)
!
       If (DABS(XJDT - XMJDS) .le. 1.D-8) Then
         Backspace (Iunit)
         Go to 70
       Else
         Go to 50
       Endif
!
  70   Continue
       Do I = 1,Nerp
!      Read (Iunit,1019) XJDT, X10, Y10, UT1T
       Read (Iunit,*) XJDT, X10, Y10, UT1T
!  Input units are : X/Y => 0.1 arc-sec; UT1 => microseconds
!  Convert to milli-arc-seconds and time seconds, and change
!   UT1-TAI to TAI-UT1 (or UT1-UTC to UTC-UT1)
       UT1PT(I) = -UT1T/1.D6
       XYWOB(1,I) = X10/10.D0 * 1.D3
       XYWOB(2,I) = Y10/10.D0 * 1.D3
!
       Enddo
!      Write (6,1023)   (XYWOB(1,I), XYWOB(2,I), UT1PT(I), I=1,Nerp)
!1023  Format (' External EOPs: ',20(/,3F20.10))
!
       Close(Iunit)
!
       Return
       End
!**********************************************************************
      REAL*8 FUNCTION JDY2K (IYEAR, IMONTH, IDAY)
      Implicit None
!
!     Function JDY2K: Function to convert year, month, day to full Julian
!     day. The year can be either a four-digit year or a two-digit year.
!
!     If a 4-digit year, this function is good from 1 March 1900 to 31
!     December 2099.
!
!     If a 2-digit year, this function is good from 1 January 1970 to
!     31 December 2069. If year is 70 - 99, 1900 is added. If year is
!     00 - 69, 2000 is added.
!
!     Programmer:
!      98.07.23  D. Gordon  Function written from code in cutcu.f
!
      Integer*4 IYEAR, IMONTH, IDAY, IY, IM, ID
!
       IY = IYEAR
       IM = IMONTH
       ID = IDAY
!
       If (IY .ge. 70 .and. IY .le. 99) Then
        IY = IY + 1900
        Go To 100
       Endif
!
       If (IY .ge. 0 .and. IY .le. 69) Then
        IY = IY + 2000
        Go To 100
       Endif
!
       If (IY .gt.1900 .and. IY .le. 2099) Then
        Go To 100
       Endif
!
!     Year out of range if we get here
       Print *, ' JDY2K, Year out of Range, Stopping! ', IY
       Stop
!
 100   Continue
!
        JDY2K = 367.D0*IY - (7 * ( IY + (IM+9)/12) )/4 + &
     &          (275*IM)/9 + ID + 1721013.5D0
!
!      Write(6,1000) IYEAR, IMONTH, IDAY, IY, IM, ID, JDY2k
 1000  Format(/,'Function JDY2K: ',/,' Input, Modified Y,M,D: ', &
     &        2x,3I5,5x,3I5,/,' JDY2K ', F15.2)
      Return
      End
!
!**********************************************************************
      SUBROUTINE YMDJL(FJD, IYEAR, IMONTH, IDAY)
      Implicit none
!
!     Convert Julian date at midnight to month, day and year.
!     Copied from PEP plus an additional .5 day to convert
!     from Julian date at midnight to the PEP Julian day number.
!
!     INPUT Variables:
      REAL*8 FJD
!
! FJD = Julian date at midnight
!
!     OUTPUT Variables:
      INTEGER*4 IMONTH,IDAY,IYEAR,ITIME
!
! IDAY = Day of the month
! IMONTH = Month of the year (1-12)
! IYEAR = Year of the century (0-99)
!       => Changed to full 4-digit year
! ITIME - Number of centuries since 1900
!
!     LOCAL VARIABLES
      INTEGER*2 MDN(13),nyr,ic,inyr,i
      Real*8 XJD
      DATA MDN/0,31,59,90,120,151,181,212,243,273,304,334,365/
!
! IC - Number of centuries sice 0 January 1600
! INYR - Leap year days since 0 January 1600
! MDN - Array containing day of year at end of each month
! NYR - Number of years since 0 January 1600
! XJD - Days since 0 January 1600
!
!     HISTORY
!   WHO   WHEN     WHAT
!   DSR  7608??    Created
!   DG  2012.04.26 Integer*4 and 4-digit year mods.
!
!     YMDJL PROGRAM STRUCTURE
!
      XJD = FJD - 2305447.0D0 + 0.5D0
      NYR = XJD/365.
   16 IC = NYR/100
!     DAYS DUE TO LEAP YEARS
      INYR = XJD - NYR*365.0
      IDAY = INYR - (NYR-1)/4 + (NYR + 99)/100 - (NYR + 399)/400 - 1
      IF(IC .NE.0) GO TO 20
      IF(NYR.NE.0) GO TO 20
      IDAY = IDAY + 1
   20 IF(IDAY .GT. 0) GO TO 23
      NYR = NYR - 1
      GO TO 16
!**** IYEAR (O THRU 99) YEAR OF THE CENTURY
   23 IYEAR = NYR - IC * 100
      ITIME = IC - 3
      NYR = IYEAR
      IF(NYR .NE. 0) GO TO 27
      IF(MOD(IC,INT2(4)) .NE. 0) GO TO 34
      GO TO 30
   27 IF(MOD(NYR,INT2(4)) .NE. 0) GO TO 34
!
! Replacing ancient code:
!  30 IF(IDAY - 60) 34,39,32
   30 Continue
      If((IDAY-60) .lt. 0) Go to 34
      If((IDAY-60) .eq. 0) Go to 39
      If((IDAY-60) .gt. 0) Go to 32
!  
   32 IDAY = IDAY - 1
   34 DO 36 I=2,13
          IF(IDAY .LE. MDN(I)) GO TO 40
   36 CONTINUE
   39 IMONTH = 2
      IDAY = 29
      GO TO 45
   40 IMONTH = I - 1
      IDAY = IDAY - MDN(IMONTH)
   45 CONTINUE
! Modified - 4 digit year
       IYEAR = IYEAR + 1900 + ITIME*100
!
      RETURN
      END
!
!**********************************************************************
      SUBROUTINE dGet_input(Kjob)
      IMPLICIT None
!
!
! 3.2.2 COMMON BLOCKS USED -
!
!!!!  INCLUDE 'cuser11.i'
!       Variables from:
!         1. Calc_user  - Calc user type. 'A' for Calc/SOLVE analysis.
!                         'C' for VLBI correlator.
      INCLUDE 'cmxut11.i'
!            Variables 'to':
!              1. Xintv(2)  - First and last Julian Date in the data base
!              1. UT1IF(4)  - The final UT1 information array. This array
!                             contains respectively: 1) The Julian date of the
!                             first tabular point, 2) The increment in days of
!                             the tabular points, 3) The number of tabular
!                             points, 4) The units of the UT1 tabular array per
!                             second. (days, days, unitless, sec/table unit)
!              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
!                             (table units)
!              3. ISHRTFL   - The short period tidal terms flag, (unitless).
!                             = 1 --> UT1 table coming from input database is
!                             true UT1, (that is, fortnightly tidal terms have
!                             not been removed, as in the IRIS or IERS series).
!                             = -1 --> UT1 table coming from input database is
!                             UT1R, (that is, the Yoder fortnightly tidal terms
!                             HAVE been removed as in Bulletin B).
!                             = -2 --> UT1 table coming from input database is
!                             UT1S, (the S tidal terms HAVE been removed).
!              4. Leap_fix   - Used in external input mode. .True. means
!                              correct the input EOP series for accumluated
!                              leap seconds. .False. means do not correct.
!              5. UT1type    - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '.
!                              For ''UT1-UTC ', leap second corrections
!                              must be made.
!              6. EOP_time_scale - EOP table time scale, allowed values:
!                              'TAI     ', 'TCG     ', 'TDB     ',
!                              'TDT     ', 'UTC     ', 'UNDEF   '.
!                              Assumed default if not present => TDB
!
      INCLUDE 'cmwob11.i'
!            Variables 'to':
!              1. WOBIF(3)  -  The wobble information array. Contains
!                              respectively: 1) The Julian date of the first
!                              tabular point, 2) The increment in days of the
!                              tabular points, 3) The number of tabular points.
!                              (days, days, unitless)
!              2. XYWOB(2,20)- The wobble tabular points for the polar motion
!                              (wobble) X & Y offsets. (milliarcsec)
!                              (Note: Based on old BIH conventions, offsets
!                              are assumed to be left-handed.)
!
      INCLUDE 'cmxst11.i'
!      Variables from:
!       1. Max_Stat             -  Maximum number of stations that can be 
!                                  processed.
!      Variables to:
!*      1. CFRAD(Max_Stat)      -  THE SITE SPHERICAL EARTH RADII.  (M)
!*      2. PLAT(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
!*                                 FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!*                                 GEODETIC LATITUDES. (M/RAD)
!*      3. PLON(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
!*                                 FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!*                                 EAST LONGITUDES. (M/RAD)
!       4. SITAXO(Max_Stat)     -  THE SITE ANTENNA AXIS OFFSETS. (M)
!       5. SITOAM(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING AMPLITUDES.
!                                  (M)
!       6. SITOPH(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING PHASES.
!                                  (RAD)
!       7. SITXYZ(3,Max_Stat)   -  THE SITE CRUST FIXED X, Y, & Z
!                                  COORDINATES. (M, M, M )
!*      8. SNRM(3,Max_Stat)     -  THE X, Y, AND Z COMPONENTS OF THE SITE
!*                                 NORMAL UNIT VECTORS. (UNITLESS)
!*      9. SITZEN(Max_Stat)     -  THE ZENITH ELECTRICAL PATH LENGTH AT EACH
!*                                 OBSERVATION SITE. (SEC)
!*     10. TCROT(3,3,Max_Stat)  -  THE ROTATION MATRICES WHICH ROTATE THE
!*                                 TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST
!*                                 FIXED REFERENCE SYSTEM FOR EACH SITE.
!*                                 (UNITLESS)
!*     11. XLAT(Max_Stat)       -  THE SITE GEODETIC LATITUDES. (RAD)
!*     12. XLON(Max_Stat)       -  THE SITE EAST LONGITUDES. (RAD)
!      13. KTYPE(Max_Stat)      -  THE SITE ANTENNA AXIS TYPES. (UNITLESS)
!*     14. NLAST(2)             -  THE INTEGER VARIABLE WHICH DETERMINES IF
!*                                 THE BASELINE ID HAS CHANGED FROM ONE
!*                                 OBSERVATION TO THE NEXT.
!*                                 (NOTE: THE SITE GEOMETRY NEED NOT BE
!*                                 RELOADED FOR EACH OBSERVATION IF THE
!*                                 BASELINE ID DOES NOT CHANGE. NLAST MUST BE
!*                                 INITIALIZED TO ZERO IN THE INITIALIZATION
!*                                 SECTION AND PASSED TO THE GEOMETRY SECTION
!*                                 SO THAT IT WILL HAVE ZERO VALUES UNTIL
!*                                 AFTER THE FIRST OBSERVATION IS PROCESSED.)
!*     15. NUMSIT               -  THE NUMBER OF SITES IN THE SITE CATALOG.
!      16. LNSITE(4,Max_Stat)   -  THE EIGHT CHARACTER SITE NAMES OF THE
!                                  SITES IN THE SITE CATALOG. (ALPHAMERIC)
!      17. SITHOA(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING
!                                  AMPLITUDES. (M)
!      18. SITHOP(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING PHASES.
!                                  (RAD)
!*     19. HEIGHT(Max_Stat)     -  Height above the geoid. (meters)
!*     20. RTROT(3,3,Max_Stat)  -  The rotation matrices which rotate the
!*                                 'radial-transverse' reference system to the
!*                                 crust fixed reference system for each site.
!*                                 (Unitless). The 'radial-transverse' ref.
!*                                 system is nearly identical to the
!*                                 topocentric system. 'Up' is in the radial
!*                                 direction from the center of the Earth;
!*                                 'East' is East; and 'North' is perpendicular
!*                                 to the radial in the north direction.
!*     21. GLAT(Max_Stat)       -  The geocentric latitude at each site. (rad)
!      22. Zero_site            -  The site number of the site at the
!                                  geocenter, if there is one in this data
!                                  set. For correlator usage.
!      23. Dbtilt(Max_Stat,2)   -  Antenna fixed axis tilts, in arc-minutes.
!                                  For alt-az mounts, 1 => East tilt,
!                                  2 => North tilt.
!*     24. Rotilt(3,3,Max_Stat) -  Rotation matrices representing the antenna
!*                                 fixed axis tilts, in the local topocentric
!*                                 station frame. X = Up, Y = East, Z = North.
!      25. OPTL6(6,Max_stat)    -  The site ocean pole tide loading 
!                                  coefficients, interpolated from the Desai
!                                  lat/lon table.
!
      INCLUDE 'cmxsr11.i'
!       VARIABLES 'TO':
!         1. LNSTAR(10,MAX_ARC_SRC)- THE ALPHANUMERIC CHARACTER NAMES
!                                    OF THE STARS IN THE STAR CATALOG.
!                                    Now up to 20 characters in difx mode.
!         2. NUMSTR                - THE NUMBER OF STARS IN THE STAR
!                                    CATALOG.
!         3. RADEC(2,MAX_ARC_SRC)  - THE RIGHT ASCENSIONS AND DECLINATIONS
!                                    OF THE STARS IN THE STAR CATALOG.
!                                    (RAD, RAD)
!         4. P_motion(3,Max_arc_src)-The RA and Dec proper motions and
!                                    appropriate epoch for stars in the
!                                    star catalog. (arc-sec/yr, arc-sec/yr,
!                                    year (i.e. 1995.5, etc.))
!         5. D_psec(Max_arc_src)   - Distances, if known, for stars in the
!                                    star catalog. Zero => unknown.
!                                    (parsecs)
!         6. PRcorr(2,Max_arc_src) - Proper motion corrections in RA and
!                                    Declination for each star. (Radians)
!
!
      Character*200 Buf1
!     CHARACTER*128 calc_file_name
      CHARACTER*26 Sou26, Sp26
      CHARACTER*12 Site12, Scan12
      Real*8    JStart, JStop, StartMJD, EopTag(20), TAIUTC(20),        &
     &          UT1UTC(20), ADJUSTL, X_Sp
      Integer*4 AntNum, SrcNum,          NumEOP, EopNum, SpNum
      Integer*4 I, J, Unit1, IOS, IX, IK, KJ, I_Sp, L, Kjob
      Integer*4 get4unit
      Character*6 Mount6 
      Save      Unit1
!
      INCLUDE 'd_input.i'
!     COMMON/Calc_input/ SrcName, ScanID, Sites, Axis, JobID, NumScans, &
!    &       StartYr, StartMo, StartDay, StartHr, StartMin, StartSec,   &
!    &       ScanStrt, ScanDur
!     Character*10 SrcName(100), ScanID
!     Character*8 Sites(Max_stat)
!     Integer*4 JobID, NumScans
!     Integer*4 StartYr, StartMo, StartDay, StartHr, StartMin, StartSec
!     Integer*4 ScanStrt, ScanDur
!     Character*4 Axis(Max_Stat) 
!
!     COMMON/NFO/ SpTag, SpPos, SpVel, NumSpace, NumRows, SpName 
!     Character*10 SpName(10)
!     Real*8 SpTag(100,10), SpPos(100,3,10), SpVel(100,3,10)
!     Integer*4 NumSpace, Numrows(10)
!
!!    Character*8 SrcName(10)
      Character*20 SrcName(MAX_ARC_SRC)
      Equivalence (LNSTAR(1,1), SrcName(1))
!
!  Need to initialize stuff:
!
       NumSit = 0
       NumStr = 0
       NumEOP = 0
       NumSpace = 0
       SpFrame = 'ECJ2'
       Do IK=1, Max_Stat
         SITAXO(IK) = 0.D0
         Do KJ=1,3
          SITXYZ(KJ,IK) = 0.D0
         Enddo
       Enddo
!
       If (Kjob .eq. 1) Unit1 = get4unit()
       OPEN(Unit1,FILE=calc_file_name ,STATUS='OLD', IOSTAT= IOS)
       IF(IOS.ne.0) Write(6,'("Open Error for file: ",A128)') calc_file_name
!
 100  Continue
!
      Read(Unit1,'(A200)',end=200) Buf1
!
      If (Buf1(1:7) .eq.'JOB ID:') Read(Buf1(20:40),*) JobID 
      If (Buf1(1:15).eq.'JOB START TIME:') Read(Buf1(16:40),*) JStart 
      If (Buf1(1:14).eq.'JOB STOP TIME:') Read(Buf1(16:40),*) JStop 
      If (Buf1(1:10).eq.'START MJD:') Read(Buf1(20:40),*) StartMJD
      If (Buf1(1:11).eq.'START YEAR:') Read(Buf1(20:40),*) StartYr
      If (Buf1(1:12).eq.'START MONTH:') Read(Buf1(20:40),*) StartMo
      If (Buf1(1:10).eq.'START DAY:') Read(Buf1(20:40),*) StartDay
      If (Buf1(1:11).eq.'START HOUR:') Read(Buf1(20:40),*) StartHr
      If (Buf1(1:13).eq.'START MINUTE:') Read(Buf1(20:40),*) StartMin
      If (Buf1(1:13).eq.'START SECOND:') Read(Buf1(20:40),*) StartSec
! 
      If (Buf1(1:15).eq.'NUM TELESCOPES:') Read(Buf1(20:40),*) NUMSIT
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
      If (Buf1(1:9).eq.'TELESCOPE') Then
!  Don't fill site #1. That will be the Geocenter.

       IX = INDEX(Buf1,'NAME:')
       If (IX .gt. 0) Then
        Read(Buf1(10:IX-1),*) AntNum
        Site12 = Buf1((IX+5):(IX+16)) 
        Site12 = ADJUSTL(Site12)
        Sites(AntNum+2) = Site12(1:8)
       Endif 
!
       IX = INDEX(Buf1,'MOUNT:')
       If (IX .gt. 0) Then
        Read(Buf1(10:IX-1),*) AntNum
        Mount6 = Buf1((IX+6):(IX+11)) 
        Mount6 = ADJUSTL(Mount6)
        Axis(AntNum+2) = Mount6(1:4)
       Endif 
!
       IX = INDEX(Buf1,'OFFSET (m):')
       If (IX .gt. 0) Then
        Read(Buf1(10:IX-1),*) AntNum
        Read(Buf1((IX+11):(IX+22)),*) SITAXO(AntNum+2) 
       Endif 
!
       IX = INDEX(Buf1,'X (m):')
       If (IX .gt. 0) Then
        Read(Buf1(10:IX-1),*) AntNum
        Read(Buf1((IX+7):(IX+26)),*) SITXYZ(1,AntNum+2) 
       Endif 
!
       IX = INDEX(Buf1,'Y (m):')
       If (IX .gt. 0) Then
        Read(Buf1(10:IX-1),*) AntNum
        Read(Buf1((IX+7):(IX+26)),*) SITXYZ(2,AntNum+2) 
       Endif 
!
       IX = INDEX(Buf1,'Z (m):')
       If (IX .gt. 0) Then
        Read(Buf1(10:IX-1),*) AntNum
        Read(Buf1((IX+7):(IX+26)),*) SITXYZ(3,AntNum+2) 
       Endif 
!
      Endif    
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
!
      If (Buf1(1:12).eq.'NUM SOURCES:') Read(Buf1(13:30),*)  NUMSTR 
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
      If (Buf1(1:6).eq.'SOURCE') Then
       IX = INDEX(Buf1,'NAME:')
       If (IX .gt. 0) Then
        Read(Buf1(8:IX-1),*) SrcNum
        Sou26 = Buf1((IX+5):(IX+30)) 
        Sou26 = ADJUSTL(Sou26)
        SrcName(SrcNum+1) = Sou26(1:20)
       Endif 
       IX = INDEX(Buf1,'RA:')
       If (IX .gt. 0) Then
        Read(Buf1(7:IX-1),*) SrcNum
        Read(Buf1((IX+3):(IX+33)),*) RADEC(1,SrcNum+1) 
       Endif 
       IX = INDEX(Buf1,'DEC:')
       If (IX .gt. 0) Then
        Read(Buf1(7:IX-1),*) SrcNum
        Read(Buf1((IX+4):(IX+33)),*) RADEC(2,SrcNum+1) 
       Endif 
      Endif 
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
!
      If (Buf1(1:10).eq.'NUM SCANS:') Read(Buf1(11:30),*) NumScans
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Scan information now is read in by subroutine dScan.
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
!     If (Buf1(1:4).eq.'SCAN') Then
!      IX = INDEX(Buf1,'IDENTIFIER:')
!      If (IX .gt. 0) Then
!       Read(Buf1(5:IX-1),*) ScanNum
!       Scan12 = Buf1((IX+11):(IX+22)) 
!       Scan12 = ADJUSTL(Scan12)
!       ScanID = Scan12(1:10)
!      Endif 
!      IX = INDEX(Buf1,'START (S):')
!      If (IX .gt. 0) Then
!       Read(Buf1(5:IX-1),*) ScanNum
!       Read(Buf1((IX+10):(IX+17)),*) ScanStrt
!      Endif 
!      IX = INDEX(Buf1,'DUR (S):')
!      If (IX .gt. 0) Then
!       Read(Buf1(5:IX-1),*) ScanNum
!       Read(Buf1((IX+8):(IX+17)),*) ScanDur
!      Endif 
!      IX = INDEX(Buf1,'NUM PHS CTRS:')
!      If (IX .gt. 0) Then
!       Read(Buf1(5:IX-1),*) ScanNum
!       Read(Buf1((IX+13):(IX+16)),*) NumPhCntr
!      Endif 
!     Endif 
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
!
      If (Buf1(1:9).eq.'NUM EOPS:') Read(Buf1(10:30),*) NumEOP
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
      If (Buf1(1:3).eq.'EOP') Then
       IX = INDEX(Buf1,'TIME (mjd):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) EopNum
        Read(Buf1((IX+11):(IX+20)),*) EopTag(EopNum+1) 
       Endif 
       IX = INDEX(Buf1,'TAI_UTC (sec):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) EopNum
        Read(Buf1((IX+14):(IX+20)),*) TAIUTC(EopNum+1) 
       Endif 
       IX = INDEX(Buf1,'UT1_UTC (sec):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) EopNum
        Read(Buf1((IX+14):(IX+25)),*) UT1UTC(EopNum+1) 
       Endif 
       IX = INDEX(Buf1,'XPOLE (arcsec):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) EopNum
        Read(Buf1((IX+15):(IX+25)),*) XYWOB(1,EopNum+1) 
       Endif 
       IX = INDEX(Buf1,'YPOLE (arcsec):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) EopNum
        Read(Buf1((IX+15):(IX+25)),*) XYWOB(2,EopNum+1) 
       Endif 
      Endif 
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
!
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
      If (Buf1(1:15).eq.'NUM SPACECRAFT:') Read(Buf1(16:30),*) NumSpace
       If (Buf1(1:6).eq.'FRAME:') Read(Buf1(21:24),*) SpFrame 
      If (Buf1(1:10).eq.'SPACECRAFT') Then
       IX = INDEX(Buf1,'NAME:')
       If (IX .gt. 0) Then
        Read(Buf1(11:IX-1),*) SpNum
!       Read(Buf1((IX+5):(IX+30)),*) SpName(SpNum+1) 
        Sp26 = Buf1((IX+5):(IX+30)) 
        Sp26 = ADJUSTL(Sp26)
        SpName(SpNum+1) = Sp26(1:20)
!     write(6,*) 'NumSpace,SpNum,SpName(SpNum+1))', NumSpace,SpNum,SpName(SpNum+1)
       Endif 
       IX = INDEX(Buf1,'ROWS:')
       If (IX .gt. 0) Then
        Read(Buf1(11:IX-1),*) SpNum 
        Read(Buf1((IX+5):(IX+14)),*) Numrows(SpNum+1) 
!  Check for too many spacecraft positions.
           If (Numrows(SpNum+1) .gt. NF_row) Then
            Write(6,1031) Numrows(SpNum+1), NF_row
 1031       Format(/,' Number of spacecraft position rows = ',I4,/,     &
     &            ' The current limit is ',I4,/,' Increase the value ', &
     &            'of NF_row in d_input.i ',/)
            Stop 
           Endif
          Do I = 1, Numrows(SpNum+1)
            Read(Unit1,'(A200)',end=200) Buf1
             If (Buf1(1:6).eq.'FRAME:') Then
               Read(Buf1(21:24),*) SpFrame
               Go to 190
             Endif
            IX = INDEX(Buf1,':')
            Read(Buf1(IX+1:200),*) SpTag(I,SpNum+1),SpPos(I,1,SpNum+1), &
     &       SpPos(I,2,SpNum+1),SpPos(I,3,SpNum+1), SpVel(I,1,SpNum+1), &
     &       SpVel(I,2,SpNum+1),SpVel(I,3,SpNum+1)
!             Shift time tags by t_offset if requested.
            If (SpOffset .eq. 'Offset  ')                               &
     &             SpTag(I,SpNum+1) = SpTag(I,SpNum+1) + t_offset
 190         Continue
          Enddo
       Endif 
      Endif 
!
!
       Go to 100
!
  200 Continue
!
      Close (Unit1)
!
! Setup WOBIF and UT1IF arrays:
      WOBIF(1) = EOPTag(1) + 2400000.5D0
!     WOBIF(2) = 1.0D0 
      WOBIF(2) = EOPTag(2) - EOPTag(1)
      WOBIF(3) = NumEop
!
      UT1IF(1) = EOPTag(1) + 2400000.5D0
!     UT1IF(2) = 1.0D0
      UT1IF(2) = EOPTag(2) - EOPTag(1)
      UT1IF(3) = NumEop
      UT1IF(4) = 1.0D0 
!
       Xintv(1) = JStart + 2400000.5D0
       Xintv(2) = JStop  + 2400000.5D0
!
!  Save leap seconds
       Xleap_sec = TAIUTC(1)
!
!  If multiple scans, get the total duration time.
!!!!   If(NumScans .gt. 1) ScanDur = ScanStrt + ScanDur
!
! Change XY-wobble to milli-arc-seconds
      Do J = 1, NumEOP
       UT1PT(J) = TAIUTC(J) - UT1UTC(J)
       XYWOB(1,J) = XYWOB(1,J) * 1.D3
       XYWOB(2,J) = XYWOB(2,J) * 1.D3
      Enddo
!
!  If Spacecraft mode, create SpcIF array
!     If (NumSpace .ge. 1) Then
!       SpcIF(1) = SpTag(1,1) + 2400000.5D0 
!       SpcIF(1) = SpTag(1,1) 
!         X_Sp = (SpTag(2,1) - SpTag(1,1))*86400.D0 + .01
!         I_Sp = X_Sp 
!       SpcIF(2) = I_Sp/86400.D0
!       SpcIF(3) = Numrows(1)
!      write(6,*) 'dGet_input: SpcIF,Numrows: ', SpcIF, Numrows(1)
! Set near field flag to geocentric coordinates. 
!  [Expand later to handle SSBC coordinates.] 
!       NF_flag = 'GC'
!     Endif 
!
      Numsrc = NUMSTR
!
!**   IF (Debug .eq. 'd') Then 
!!    Write(6,*) StartMJD, StartYr, StartMo, StartDay, StartHr,         &
!!   &           StartMin, StartSec
!!     Write(6,*) '      '
!     Write(6,*) ScanDur 
!      Write(6,*) '      '
!     Write(6,*) Numsit, (Sites(I), I=2, Numsit+1)
!      Write(6,*) '      '
!     Do I = 2, Numsit+1
!!     Write(6,*) I, Sites(I), Axis(I), SITAXO(I), SitXYZ(1,I),         &
!!   &            SitXYZ(2,I), SitXYZ(3,I)
!     Enddo
!
!     Write(6,*) 'Number of Sources: ', NUMSTR
!     Do I=1,NUMSTR
!      Write(6,*) I, SrcName(I), RADEC(1,I), RADEC(2,I)
!     Enddo
!!     Write(6,*) 'WOBIF ', WOBIF
!!     Write(6,*) 'UT1IF ', UT1IF
!     Do I = 1, NumEOP
!!     Write(6,*) I, UT1PT(I), XYWOB(1,I), XYWOB(2,I)
!     Enddo
!      Write(6,*) '      '
!!
!     If (NumSpace .ge. 1) Then
!      Write(6,*) 'SpcIF ', SpcIF
!      Write(6,*) 'X_Sp, I_Sp ', X_Sp, I_Sp
!      Write(6,*) '      '
!      Write(6,*) SpNum, SpName(1), Numrows(1)
!       Do I = 1, Numrows(1)
!        Write(6,*) SpTag(I,1), SpPos(I,1,1), SpPos(I,2,1),             &
!    &     SpPos(I,3,1), SpVel(I,1,1), SpVel(I,2,1),SpVel(I,3,1)
!       Enddo
!     Endif
!      Write(6,*) '      '
!**   Endif 
!
      Return
      End
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE GETCL(Calcfiles, IMfiles)
      IMPLICIT None
!
      INCLUDE 'd_input.i'
!       To variables:
!        3) Numjobs        - Number of .calc files that will be processed.
!!!!!    4) Base_mode      - Baseline mode: 'geocenter' or 'baseline'.
!!!!!                        Default is 'geocenter'  => not currently used.
!        5) L_time         - 'dont-solve' or 'solve' for light travel
!                            time from spacecraft. Default is 'dont-solve'. 
!        6) Atmdr          - 'Add-dry   ' or 'no-Add-dry' atmosphere. 
!        7) Atmwt          - 'Add-wet   ' or 'no-Add-wet' atmosphere. 
!
!   Local variables
!        1) Calcfiles(n)   - List of input .calc files
!        2) IMfiles(n)     - List of output .im files
!        3) calc_file - Input '----.calc' file name.
!        4) IM_file   - Output '----.im' file name.
!
!     Programmer - David Gordon Jan-Apr, 2013 
!                  David Gordon March 2015
!                  David Gordon June 2015
!
!     CHARACTER*40  calc_file_name, Jobname, IM_file_name
      CHARACTER*128 calc_file, IM_file
      CHARACTER*128 Calcfiles(2000), IMfiles(2000)
      CHARACTER*128 ch_cl
!     CHARACTER*10  Base_mode, L_time
      Real*8  t_offsec
      Integer*4 Iar, I, Ic, Index, isec, Int_poly, i1, Ix, Iskip
      Integer*4 ierr, Icm1, l_scr, get4unit, iv, Kjob
!
      Do I = 1,128
       calc_file(I:I) = ' '
       Jobname(I:I) =   ' '
       IM_file(I:I) =   ' '
      Enddo
!     calc_out_file =  'Null                                    '
      I_out = 0
      IM_out = 1
      SpOffset = 'NoOffset'
!  Defaults:
      Base_mode = 'geocenter ' 
      L_time = 'dont-solve'
      Atmdr  = 'Add-dry   '
      Atmwt  = 'Add-wet   '
      Verbose = 0
      t_offset = 0.D0
!     NF_model = 'Sekido  '
      NF_model = 'Duev    '
      DoStnPos = 0
      overwrite = 'no  '
      UVW = 'exact '
!   Interval between Calc runs (default is every 24 seconds 
!      for VLBA DiFX correlator).
      d_interval = 24.D0
      int_poly = 120
!   # of Calc epochs in each 2-minute interval
      epoch2m = (120.0001/d_interval) + 1
!     write (6,*) ' d_interval, epoch2m: ', d_interval, epoch2m 
!   # of input calc jobs
      Numjobs = 0
!
!   Find number of parameters on the command line
       Iar = IARGC()
!      write(6,*) '  '
!      write(6,*) ' GETCL/Iar =  ', Iar
! If no arguments, print out the help menu and terminate 
       If (Iar .eq. 0) Call USAGE()
        Iskip = 0
!
       Do I = 1, Iar
          If (Iskip .eq. 1) Then   ! Next argument already read
           Iskip = 0
           Go to 100
          Endif
        Call GETARG(I,ch_cl)
!
         Ic = Index(ch_cl,'.calc')
         If (Ic .ge. 2) Then
          Numjobs = Numjobs + 1
          Ijob = Ic-1
          Jobname(1:Ijob) = ch_cl(1:Ijob)
          Icalc = Ijob+5
          calc_file(1:Icalc) = ch_cl(1:Icalc) 
            Do Ix = Icalc+1, 128
             calc_file(Ix:Ix) = ' '
            Enddo
          IM_file(1:Ijob) = Jobname(1:Ijob)
!         IM_file(Ijob+1:Ijob+4) =  '.im'//CHAR(0)
          IM_file(Ijob+1:Ijob+3) =  '.im'
            Do Ix = Ijob+4, 128
             IM_file(Ix:Ix) = ' '
            Enddo
          Calcfiles(Numjobs) = calc_file
          IMfiles(Numjobs) = IM_file
          Go to 100
         Endif
!
         If (ch_cl(1:2) .eq. '-h') Call USAGE() 
         If (ch_cl(1:6) .eq. '--help') Call USAGE()
!        If (ch_cl(1:2) .eq. '-v') Verbose = 1
!        If (ch_cl(1:9) .eq. '--verbose') Verbose = 1
!        If (ch_cl(1:2) .eq. '-q') Verbose = -1
!        If (ch_cl(1:7) .eq. '--quiet') Verbose = -1
!        If (ch_cl(1:2) .eq. '-b') Base_mode = 'baseline  '
!        If (ch_cl(1:2) .eq. '-m') Base_mode = 'master-stn'
         If (ch_cl(1:3) .eq. '-lt') Then
            L_time = 'solve     '
            Go to 100
         Endif
         If (ch_cl(1:4) .eq. '-dry') Then
            Atmdr  = 'no-Add-dry'
            Go to 100
         Endif
         If (ch_cl(1:4) .eq. '-wet') Then
            Atmwt  = 'no-Add-wet'
            Go to 100
         Endif
         If (ch_cl(1:2) .eq. '-S' .or. ch_cl(1:8) .eq. '--Sekido') Then
            NF_model = 'Sekido  '
            Go to 100
         Endif
         If (ch_cl(1:2) .eq. '-R' .or. ch_cl(1:9) .eq. '--Ranging') Then
            NF_model = 'Ranging '
            Go to 100
         Endif
         If (ch_cl(1:2) .eq. '-D' .or. ch_cl(1:6) .eq. '--Duev') Then
            NF_model = 'Duev    '
            Go to 100
         Endif
         If (ch_cl(1:2) .eq. '-s' .or. ch_cl(1:6) .eq. '--stnpos') Then
            DoStnPos = 1
            Go to 100
         Endif
         If (ch_cl(1:2) .eq. '-f') Then    ! force execution
            overwrite = 'yes '
!             write(6,*) ' Overwriting existing .im files! '
            Go to 100
         Endif
         If (ch_cl(1:11) .eq. '-uncorr') Then   ! U,V,W: non-relativistic geometry 
            UVW = 'uncorr'
            Go to 100
         Endif
         If (ch_cl(1:11) .eq. '-approx') Then   ! U,V,W: geometry with aberration 
            UVW = 'approx' 
            Go to 100
         Endif
         If (ch_cl(1:11) .eq. '-exact ') Then   ! U,V,W: partial derivatives of delay
            UVW = 'exact '
            Go to 100
         Endif
         If (ch_cl(1:11) .eq. '-noatmo') Then   ! U,V,W: 'exact' but no atmosphere 
            UVW = 'noatmo'
            Go to 100
         Endif
!
!        If (ch_cl(1:2) .eq. '-o') I_out = 1
!        If (ch_cl(1:3) .eq. '-im') IM_out = 0
!        If (ch_cl(1:2) .eq. '-e') Then     
!          Call GETARG(I+1,ch_cl)
!          Read(ch_cl,*,err=22) isec
!          If (isec .ge. 1 .and. isec .le. 60) Then
!37         continue
!           i1 =  int_poly/isec
!           if(i1*isec .ne. int_poly) then 
!            isec = isec-1
!            go to 37
!           endif
!             # of seconds between Calc epochs
!           d_interval = isec
!             # of Calc epochs in each 2-minute interval
!           epoch2m = (120.0001/d_interval) + 1
!            write (6,*) ' d_interval, epoch2m: ', d_interval, epoch2m 
!          Else
!            Write (6,*) 'Calc epoch interval set to default value.'
!          Endif
!        Endif
!
         If (ch_cl(1:2) .eq. '-v')  Then
           Call GETARG(I+1,ch_cl)
           Read(ch_cl,*,err=20) iv
           Verbose = iv
           Iskip = 1 
           Go to 100
  20        Continue
           Verbose = 1
           Go to 100
         Endif
!
         If (ch_cl(1:2) .eq. '-t') Then     
           Call GETARG(I+1,ch_cl)
           Read(ch_cl,*,err=22) t_offsec
           t_offset = t_offsec/86400.D0
           SpOffset = 'Offset  '
!          write(6,*) 't_offset ', SpOffset, t_offset
           Iskip = 1 
           Go to 100
         Endif
! Process all .calc files in the current directory
         If (ch_cl(1:3) .eq. 'all' .or. ch_cl(1:5) .eq. '--all' .or.    &
     &       ch_cl(1:1) .eq. '*') Then     
            If (I .ne. Iar) Then
             Write(6,*) ' *, all, or --all must be the last command line argument, Quitting!'
             Stop
            Endif
            If (Numjobs .ne. 0) Then
             Write(6,*) ' Illegal combination of jobs, Quitting! '
             Stop
            Endif
            l_scr = get4unit()
            Open(unit=l_scr,file='scr_file',status='unknown')
            Close(unit=l_scr,status='Delete')
           ierr = SYSTEM('ls *.calc > scr_file')
            Open(unit=l_scr,file='scr_file',status='old')
 60          Continue
              Read(l_scr,'(A128)',end=70) calc_file 
               Numjobs = Numjobs + 1 
               Calcfiles(Numjobs) = calc_file
               IM_file = calc_file
                Icm1 = Index(calc_file,'.calc')
               IM_file(Icm1:Icm1+5) = '.im   '
               IMfiles(Numjobs) = IM_file
               Go to 60
 70           Continue
!           Close(unit=l_scr,status='Delete')
            Close(unit=l_scr)
           Go to 110
         Endif
        
! If we are here, then there is an unrecognized argument.
  80    Continue
        Write (6,*) '  '
        Write (6,*) ' Unrecognized command line argument: ', ch_cl
        Write (6,*) ' Quitting!  '
        Write (6,*) '  '
        Stop
!       Call USAGE()
!
 100    Continue
       Enddo
 110   Continue
!       Write (6,*) '  '
!       Write (6,*) '  '
!         Do I = 1, Numjobs
!            Write (6,*) I,Calcfiles(I),IMfiles(I)
!         Enddo
!       Write (6,*) '  '
!       Write (6,*) '  '
!
! Check for no jobs
        If (Numjobs .eq. 0) Then
         Write (6,*) '  '
         Write (6,*) ' No jobs to process, Quitting! '
         Write (6,*) '  '
         Stop
        Endif
!
!       If (I_out .eq. 1) Then
!         calc_out_file(1:Ijob) = Jobname(1:Ijob)
!         calc_out_file(Ijob+1:Ijob+9) =  '.calc.out'
!         write (6,*) ' calc_out_file ', calc_out_file
!       Endif
!      write(6,*) ' Verbose ', Verbose
!
      Return
 22   Continue
       Write (6,*) 'Error reading time offset. Stopping. '
!      Write (6,*) 'Illegal epoch interval. Stopping. '
       Stop
      End
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE FixEpoch(JTAG, TAG_SEC)
      IMPLICIT None
!
      Real*8    TAG_SEC
      Integer*4 JTAG(5), Imin
      Integer IMNTHS(12)
      DATA IMNTHS /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
!      Write (6,*) 'FixEpoch: ', JTAG, TAG_SEC
!
! Check for leap year
      IF(MOD(JTAG(1),4) .EQ. 0) IMNTHS(2) = 29
!
      IF (TAG_SEC .ge. 59.9999999) Then  ! Increment minutes
       Imin = TAG_SEC/60.D0 + .00001
       TAG_SEC = TAG_SEC - Imin*60.0D0
       JTAG(5) = JTAG(5) + Imin  ! minutes
        If (JTAG(5) .ge. 60) Then     ! Increment hours
         JTAG(5) = JTAG(5) - 60
         JTAG(4) = JTAG(4) + 1  ! hours
          If (JTAG(4) .ge. 24) Then     ! Increment days
           JTAG(4) = JTAG(4) - 24
           JTAG(3) = JTAG(3) + 1   ! days
            If (JTAG(3) .gt. IMNTHS(JTAG(2))) Then ! Increment month
             JTAG(3) = JTAG(3) - IMNTHS(JTAG(2))    ! days, should be 1
             JTAG(2) = JTAG(2) + 1   ! month
             If (JTAG(2) .gt. 12) Then               ! Increment year
              JTAG(2) = 1
              JTAG(1) = JTAG(1) + 1
             Else
              Return
             Endif                                   ! Increment year
            Else
             Return
            Endif                                  ! Increment month
          Else
           Return
          Endif                         ! Increment days
        Else
         Return
        Endif                         ! Increment hours
      ENDIF                              ! Increment minutes
!      Write (6,*) 'FixEpoch: ', JTAG, TAG_SEC
!
      Return
      End
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE FixEpoch2(JTAG, TAG_SEC)
      IMPLICIT None
!
      Real*8    TAG_SEC, Xmin, Xhr
      Integer*4 JTAG(5), Imin, Ihr
      Integer IMNTHS(12)
      DATA IMNTHS /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
!      Write (6,*) 'FixEpoch2/Before: ', JTAG, TAG_SEC
!
! Check for leap year
      IF(MOD(JTAG(1),4) .EQ. 0) IMNTHS(2) = 29
!
      IF (TAG_SEC .ge. 59.9999999) Then  ! Reset seconds and increment minutes
       Xmin = TAG_SEC/60.D0 + .00001
       Imin = Xmin
       TAG_SEC = TAG_SEC - Imin*60.0D0
       JTAG(5) = JTAG(5) + Imin  ! minutes
      ENDIF
!
       If (JTAG(5) .ge. 60) Then     ! Reset minutes ana increment hours
        Xmin = JTAG(5)
        Xhr  = Xmin/60.D0
        Ihr  = Xhr
        JTAG(5) = JTAG(5) - Ihr * 60  ! minutes
        JTAG(4) = JTAG(4) + Ihr 
       Endif
!
          If (JTAG(4) .ge. 24) Then     ! Reset hours and increment days
           JTAG(4) = JTAG(4) - 24
           JTAG(3) = JTAG(3) + 1   ! days
            If (JTAG(3) .gt. IMNTHS(JTAG(2))) Then ! Increment month
             JTAG(3) = JTAG(3) - IMNTHS(JTAG(2))    ! days, should be 1
             JTAG(2) = JTAG(2) + 1   ! month
             If (JTAG(2) .gt. 12) Then               ! Increment year
              JTAG(2) = 1
              JTAG(1) = JTAG(1) + 1
             Else
              Return
             Endif                                   ! Increment year
            Else
             Return
            Endif                                  ! Increment month
          Else
           Return
          Endif                         ! Increment days
!
!      Write (6,*) 'FixEpoch2/After:  ', JTAG, TAG_SEC
!
      Return
      End
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE USAGE()
      IMPLICIT None
!
      Write(6,1001)
 1001 Format(/,' Program difxcalc: Calc 11 for the difx correlator.',/, &
     &  ' Send comments, suggestions, requests, etc to',                &
     &  ' David.Gordon-1@nasa.gov.  ',/ ,                               &
     &  ' ************** 2016 July 07 Version *************     ',//,   &
     &  ' Usage: difxcalc [options] <file1>  ',/,                       &
     &  ' or:    difxcalc [options] <file1> <file2> <file3> ... ',/,    &
     &  ' or:    difxcalc [options]  --all                      ',/,    &
     &  ' or:    difxcalc [options]  all                      ',/,      &
!    &  ' or:    difxcalc [options]  *                      ',//,       &
     &  ' <file1> <file2>, etc. should be .calc files.      ',//,       &
!    &  ' "*", "all" or "--all" processes all .calc files in the working',   &
     &  ' all or --all processes all .calc files in the working',       &
     &  ' directory (2000 max).                                 ',//,   &
     &  ' If the .calc file contains a spacecraft ephemeris, then',     &
     &  ' difxcalc will',/,'  switch to the near-field model.',//,      &
!    &  '  Uses the Sekido and Fukushima (2006) model for near-',       &
!    &  'field delays.  ',//,                                           &
     &  ' Options can include: ',/,                                     &
     &  '  --help ',/                                                   &
     &  '  -h                 Print this help and quit.    ',//,        &
!    &  '  --verbose ',/,                                               &
     &  '  -v                 Verbose: Small printout.          ',//,   &
!    &  '  -v 1               Verbose: Small printout.          ',//,   &
!    &  '  -v 2               Verbose: More printout.           ',/,    &
!    &  '  -v 3               Verbose: Much more printout.     ',//,    &
!    &  '  --quiet ',/,                                                 &
!    &  '  -q                 Be less verbose in operation.    ',//,    &
     &  '  -s                 Write station J2000 positions.    ',//,   &
     &  '  -dry               DO NOT ADD dry atm delays.      '/,       &
     &  '                     (Default is to ADD dry atm.)     ',//,    &
     &  '  -wet               DO NOT ADD wet atm delays.      '/,       &
     &  '                     (Default is to ADD wet atm.)     ',//,    &
!    &  '  -b                 Baseline mode. Do ALL baselines.  ',/,    &
!    &  '                     (Default is geocenter mode.)     ',//,    &
!    &  '  -m                 Master station mode. First       ',/,     &
!    &  '                     antenna to all others.           ',//,    &
!    &  '  -e <n>             Epoch interval. Default: 24 seconds. ',/, &
!    &  '                     Permitted values of n (seconds): ',/,     &
!    &  '                     1,2,3,4,5,6,8,10,12,15,20,24,30,60. ',//, &
!    &  '  -im                Turn OFF the .im file output.     ',/,    &
!    &  '                     (Default is to write a .im file.) ',//,   &
!    &  '  -o                 Write calc output to a           ',/,     &
!    &  '                     *.calc.out file.            ',//,         &
     &  '  -f                 Force execution, overwrite existing .im files.',//, &
     &  '  -uncorr            U,V,W: non-relativistic geometry.',//,    &
     &  '  -approx            U,V,W: n-r geometry with aberration.',//,  &
     &  '  -exact             U,V,W: partial derivatives (default).',//, &
     &  '  -noatmo            U,V,W: exact but no atmosphere.      ',//, &
     &  '  -S                 Use modified Sekido near-field model.',/, &
!    &  '  --Sekido           Use modified Sekido near-field model.',/, &
     &  '  -D                 Use Duev near-field model. (default)',/,  &
!    &  '  --Duev             Use Duev near-field model. (default)',/, &
     &  '  -R                 Use satellite ranging near-field model.',//, &
!    &  '  --Ranging          Use satellite ranging near-field model.',//, &
     &  '  -lt                Solve for light travel time.    ',/,      &
     &  '                     (Near-field mode only)      ',//,         &
     &  '  -t <offset>        Near-field ephemeris epoch offset.   ',/, &
     &  '                     (in seconds, Real or Integer)       ',//, &
     &  '                                                   ')
!
      Stop
      End
