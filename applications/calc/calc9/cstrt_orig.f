      SUBROUTINE START (ILU, CALCON_NAME, Iout)
      IMPLICIT None
C
C     START initializes CALC. It reads the run title, the database experiment
C     file to be processed, and the control namelist which contains non-default
C     model module and utility routine flow control and debug output flags. It
C     obtains the date and time of the CALC run from subroutine FDATE. It writes
C     the CALC run title, the database experiment file to be processed, the run
C     date and time, the load module message, and the values of all flow control
C     and debug output flags in the program. The database is initialized for 
C     update, and the CALC run title, run date, run time, and the load module
C     message are placed in the database through the call to PHIST. All flow
C     control and debug output flags are loaded into the global common area CON
C     for distribution to the routines called by DRIVR. START is called only 
C     once per database. START is the routine which stops CALC when the end of
C     the control data set is reached.
C
C     Calling sequence -  CALL START(ILU, CALCON_NAME, iout)
C
C           Input variables:
C             1) ILU         - The output message logical unit.
C             2) CALCON_NAME -  Full name with path for CALC control file.
C
C      Common blocks used -
C
      INCLUDE 'ccon.i'
C            Variables 'to':
C              1.  KMODC  -  The model module flow control flags.
C              2.  KMODD  -  The model module debug output flags.
C              3.  KUTLC  -  The utility routine flow contol flags.
C              4.  KUTLD  -  The utility routine debug output flags.
C              5.  ILUOUT -  A flag controlling output.
C
      INCLUDE 'cmxst.i'
C            Variables 'to':
C              1. NUMSIT - The total number of sites in the data base.
C
      INCLUDE 'cmxsr.i'
C            Variables 'to':
C              1. NUMSTR - The total number of stars (radio sources) in the 
C                          data base.
C
      INCLUDE 'cmxut.i'
C            Variables 'to':
C              1. Xintv(2)    - First and last Julian Date of data in the  
C                               current data base.
C              2. Intrvl(5,2) - First and last time tag of data in the current
C                               data base. (First index: year, month, day,
C                               hour, minute. Second index: first, last.)
C
      INCLUDE 'inputs.i'
C            Variables 'from':
C              1. External_inputs   - Character*80 string containing the name
C                                     of the file which contains the external
C                                     file inputs (source position, site
C                                     position, etc. files)
C              2. External_aprioris - Logical variable controlling whether
C                                     external a priori inputs will be looked
C                                     for. If .FALSE., will not look for them.
C                                     If .TRUE., will look for them.
C            Variables 'to':
C              1. Ex_sites     - File name for site info
C              2. Ex_stars     - File name for stars (radio sources) info
C              3. Ex_ocean     - File name for ocean loading info
C              4. Ex_EOP       - File name for Earth orientation parameters
C              5. Input_sites  - T/F flag for using external site inputs
C              6. Input_stars  - T/F flag for using external source inputs
C              7. Input_ocean  - T/F flag for using external ocean loading 
C              8. Input_EOP    - T/F flag for using external EOP inputs
C
      Data  Ex_sites     /'                                                     
     *                                   '/
      Data  Ex_stars     /'                                                     
     *                                   '/
      Data  Ex_ocean     /'                                                     
     *                                   '/
      Data  Ex_EOP       /'                                                     
     *                                   '/
      Data Input_sites   /.False./
      Data Input_stars   /.False./
      Data Input_ocean   /.False./
      Data Input_EOP     /.False./
C
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
C           VARIABLES 'TO':
C            1. ROTEPH(2,20)- The array which contains the epochs at which
C                             TAI - UT1 is desired. The entries are:
C                             1) JD at 0:00 hours UTC,
C                             2) The fraction of a UTC day from 0:00 hours
C                                to the desired epoch.
C
      Real*8    XCALC, FJLDY
      Integer*2 NFLAG,NFLAGC,loadm(8),LFILE(3)
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
C
      Integer*4 gethostname, ierr4
      Integer*2 imode, ILU, iout, idd1
      Character*64 Computing_center
      Character*6 C_LFILE
      Character*128 CALCON_NAME
      Character*75 Lhist1
      Equivalence (C_LFILE,LFILE)
C
C       Variables 'from':
C         1. loadm(8)  - The load module compilation date message.
C         2. NFLAG     - The total number of CALC flow control and debug flags.
C         3. LFILE(3)  - The name of the CALC control file.
C         7. XCALC     - The CALC version number.
C
C   Program specifications -
C
      Real*8 JDY2K, JD1
      Integer*4 Iyear, Imonth, Iday 
      Integer*4  IFLAG(62)
      Integer*2  LCALC(40), LFCIO(40), LHIST(66), LNAME(5), LNAMO(5),
     *           IBUF(40), IOS, trimlen, host_len, hist_len
      Integer*4 Ipid, Getpid, jj 
      Real*8 xleap(5), tol
      CHARACTER LNAME_chr*10, LNAMO_chr*10, LFCIO_chr*80, LHIST_chr*132
      Character*8  Ich8
      Character*80 Ich80
      CHARACTER*80 CBUF,CTIME*12,CDATE*16
      EQUIVALENCE  ( IFLAG(1),  KATMC ),  ( LCALC(1), LHIST(1) ), 
     *             ( IBUF, CBUF), (LNAME,LNAME_chr),
     *             ( LNAMO,LNAMO_chr), (LFCIO,LFCIO_chr),
     *             ( LHIST,LHIST_chr)
      Integer*2 IPAR(5), getunit, kruc, iveri, lfvo, isame, ivero, 
     *          kerr, NDO(3), idd2
      Integer*4 I, N, Unit1, Unit2, isz, iup, MXUTPM 
      Character*24 STR24
      save      Unit1, Unit2
      CHARACTER*10 IPAR_C
      EQUIVALENCE (IPAR,IPAR_C)
      DATA  KRUC / 2 /
      Data  Lhist1 /
     .   '
     .             '/
      Data Unit1 /0/
C
C     Database access - The database is initialized by a call to subroutine
C                       KAI. Inserted into the database via subroutine PHIST is
C                       the CALC history text which consists of the CALC run
C                       title, the CALC run date, the CALC run time and the 
C                       load module message.
C
C     External I/O
C            Input variables:
C              1. Non-default model module and utility routine flow control and
C                               debug output flags.
C              2. LCALC(40)  -  The CALC run title.
C              3. LNAME(5)   -  The database experiment file to be processed.
C              4. LNAMO(5)   -  The name of the output database.
C            Output variables:
C              1. All model and module utility routine flow control and debug
C                               output flags.
C              2. LCALC(40)  -  The CALC run title.
C              3. STR24(24)  -  The date and time of the CALC run.
C              4. LDATI(5)   -  The creation date of the input database.
C              5. LFCIO(40)  -  The text descriptor from the input database.
C              6. LNAME(3)   -  The database experiment file to be processed.
C              7. LNAMO(5)   -  The name of the output database.
C
C     Subroutine interface -
C             Caller subroutines: MAIN
C             Called subroutines: QUIT_CALC, UPPER, gethostname, trimlen,
C                                 KAI, MVREC, GETI, PHIST, FDATE, OPEN, CLOSE, 
C                                 JDY2K, GETEOP
C                                 
C     Program variables -
C           1. IFLAG(NFLAG) -  The variable used to initialize all flow 
C                              control and debug output flags.
C           2. LHIST1(66)   -  The CALC history text. Included in the text
C                              are the CALC run title, the run date, the run
C                              time, and the load module message.
C           3. KERR         -  The database error return flag.
C           4. IVERI        -  The version number of the input database.
C           5. LFVO         -  The version number of the output database.
C           6. IBUF(40)     -  A buffer area for READF
C           7. Intrvl(5,2)  -  First and last UTC tag for data in the current
C                              data base. (First index - year(2 digits), 
C                              month(1-12), day of month, hours, minutes)
C
C     Programmer - Dale Markham   01/12/77
C     77.07.07 Peter Denatale
C     78.05.11 Bruce Schupler
C     78.09.14 Bruce Schupler
C     78.05.12 Bruce Schupler
C     78.06.12 Bruce Schupler
C     80.01.07 Bruce Schupler
C     80.08.26 Bruce Schupler
C     84.07.12 David Gordon    Pole Tide
C     85.01.08 David Gordon    IDISC from RMPAR(3))
C     85.02.17 David Gordon    Fixed bug in purge statement.
C     87.06.02 Savita Goel     CDS for A900.
C     89.05.22 Gregg Cooke     CALC 7.0 MODS.
C     87.09.25 Jim Ryan        Documentation simplified.
C     89.12.12 Jim Ryan        UNIX-like database interface implimented.
C     90.02.03 Jim Ryan        Removed from call to KAI.
C     90.02.09 Jim Ryan        CALCON file logic modified to use CI and fmgr
c     90.11.26 Jim Ryan        Upgraded to CALC 7.3. Improved history
c                              message added. OLD calcon_name logic for
c                              A900 lu's striped out.
C     91.05.10 Brent Archinal  Upgraded to CALC 7.4Beta with change to
C                              ctheu.f to fix 'HELL EMS'.
C     91.05.28 Jim Ryan        Documentation cleaned up furthur and output
c                              suppression fixed.
c     91.06.23 JWR             History message improved and made machine
C                              independent.
c     92.11.13 JWR             Getunit introduced to get a unique unit number
C     93.03.09 Brent Archinal  Number of flags correctly set to 62.
C     94.04.15 David Gordon    Converted to Implicit None.
C     94.06.08 David Gordon    Corrected format statements, single and double
C                              quotes reversed.
C     94.06.27 David Gordon    Changed to Calc 8.1.
C     95.11.13 B. Archinal     Fixed spelling of "pleasant"!  Also now properly 
C                              handling error from gethostname.
C     98.03.13 D. Gordon       Added include file inputs.i; mods for external
C                              file input of a priori's.
C     98.04.13 D. Gordon       Added include files cmxst.i, cmxsr.i, and 
C                              'cmxut.i'. Added read-only data base open and 
C                              GET's for number of sites, number of sources, 
C                              and data interval. Other mods for external
C                              site and source a priori's input. 
C     98.05.01 D. Gordon       Added code to load rotation epoch array. Added
C                              subroutine GETEOP and other mods for external
C                              EOP input.
C     98.07.23 D. Gordon       Replacing FJLDY calls with JDY2k calls for Y2k
C                              compliance. Will handle both 2-digit and 4-digit
C                              years. Removed ISECU, IDISC, and IOPEN from 
C                              Common /STACM/.
C     98.10.13 D. Gordon       Code cleanup. ZTIME calls replaced with FDATE.
C                              CDATE and CTIME replaced with STR24.
C     98.11.04 D. Gordon       Put in code to use proposed new Lcode 
C                              'INTRVAL4', the start/stop interval 
C                              (yr/month/day/hr/min) using a 4-digit year. If
C                              not there will use 'INTERVAL' (2-digit year).
C     99.10.01 D.Gordon        Changed to version 9.1.
C     99.10.27 D.Gordon        Extraneous printouts removed.
C
C   START Program Structure
C
      ILUOUT = iout
      MXUTPM = 20
C
C   Open the CALC control file first time through (first database).
      If(Unit1 .eq. 0) Then           ! First data base
C
       Unit1 = getunit()
       OPEN(Unit1,FILE=CALCON_NAME,STATUS='OLD', IOSTAT= IOS)
       IF(IOS.ne.0) then
        WRITE(6,'(
     .  " Expected to find CALCON file as ",A,". Not there.",//,
     .  " The run string for CALC 9 is 0,IC,calcon,inputs  where:",/,
     .  " 1) 0 is a literal zero ",/,
     .  " 2) IC is normally 0, but -1 to suppress screen output,",/,
     .  " 3) calcon is the name with path of the calcon file,",/,
     .  " 4) inputs is the optional name of the inputs file.",//,
     .  " Quitting ")') CALCON_NAME
        IPAR_C = 'CALC Fail '
        CALL QUIT_CALC(IPAR)
       Endif
C
C  Open external input file if present
       If (External_aprioris) Then    ! External aprioris
C**       print *, ' Using external a prioris  '
         Unit2 = getunit()
        Open(Unit2, File=External_inputs, Status='Old', Iostat=Ios) 
         If(Ios .ne. 0) Then
          print *, 'START/OPEN, IOS= ', Ios
         Endif
C
 22     Continue
        Read(unit2,1530,end=23) Ich8, Ich80
 1530   Format(a,a)
C  Remove leading blanks and make all upper case
          isz = 8
          iup = 1
         Call Upper(isz, iup, Ich8)
C
          isz = 80
          iup = 0
         Call Upper(isz, iup, Ich80)
C      print *,'Ich8, Ich80', Ich8, Ich80
        If (Ich8(1:5) .eq. 'SITES')   Ex_sites = Ich80
        If (Ich8(1:7) .eq. 'SOURCES') Ex_stars = Ich80
        If (Ich8(1:5) .eq. 'OCEAN')   Ex_ocean = Ich80
        If (Ich8(1:3) .eq. 'EOP')     Ex_EOP   = Ich80
        Go to 22
 23     Continue
        Close(Unit2)
        If (Ex_sites(1:20)   .eq. '                    ') 
     *                     Ex_sites(1:4) = 'None'
        If (Ex_stars(1:20) .eq. '                    ') 
     *                     Ex_stars(1:4) = 'None'
        If (Ex_ocean(1:20)   .eq. '                    ') 
     *                     Ex_ocean(1:4) = 'None'
        If (Ex_EOP(1:20)     .eq. '                    ') 
     *                     Ex_EOP(1:4)   = 'None'
       write(6,'("Ex_sites ",1x,A80)') Ex_sites  
       write(6,'("Ex_stars ",1x,A80)') Ex_stars  
       write(6,'("Ex_ocean ",1x,A80)') Ex_ocean  
       write(6,'("Ex_EOP   ",1x,A80)') Ex_EOP    
C
        If (Ex_sites(1:4) .ne. 'None') Input_sites = .True.
        If (Ex_stars(1:4) .ne. 'None') Input_stars = .True.
        If (Ex_ocean(1:4) .ne. 'None') Input_ocean = .True.
        If (Ex_EOP(1:4)   .ne. 'None') Input_EOP   = .True.
C
C      print *, ' START/Inputs: ', Input_sites, Input_stars,
C    *                           Input_ocean, Input_EOP    
C
      Endif           ! External aprioris
C 
      Endif           ! First data base
C
C   Input the CALC run title and the database experiment file to be processed,
C   and the name of the output database. Obtain the date and time of the CALC
C   run. If there is no more information in the control data set, skip on.
C
      READ(Unit1,'(40A2)',END=10000) LCALC
      READ(Unit1,'(5A2,I10,5X,5A2,I10)',END=10000)
     .      LNAME, IVERI, LNAMO, LFVO
C
      IF(LNAMO_chr .eq. '          ') then !Defaulting the output name
        LNAMO_chr = LNAME_chr
        LFVO = 0
      Endif
C
       CALL FDATE (STR24)
C          FDATE returns a 24 character string containing the time and date 
C          in the following form: 'Tue Oct 13 14:08:43 1998'. Previously this
C          info was obtained with subroutine ZTIME. You may need to use the
C          +U77 compile line option when compliling and linking. 
C
C   Write the CALC run title, the database experiment file to be processed, the
C   date of the CALC run, the time of the CALC run, and the load module message.
      Ipid = Getpid()
      ierr4 = gethostname(computing_center)
      if(ierr4.ne.0) then
        write(6,"('ERROR reading host computer name')")
        call perror()
      endif
      host_len = trimlen(Computing_center)
       WRITE (6,9301) LCALC, LNAME, IVERI, LNAMO, LFVO, STR24, 
     *                LOADM,xcalc,computing_center(1:host_len)
 9301 FORMAT (/,1X,40A2,/,' Database experiment file: ',5A2,2X,I4,
     *        /,' Name of output file: ',5X,5A2,2X,I4,/,
     *        ' Date and time of CALC run: ', A24,
     *         /,1X,8A2,' CALC version ',f5.2,1x,a15)
C
C   Initialize all flow control and debug output flags to zero.
      DO 400 N = 1, NFLAG
 400    IFLAG(N) = 0
C
C   Read the non-default model module and utility routine flow control and 
C   debug flags.
      READ(Unit1,'(80I1)',END=10000) IFLAG
C
C   Write the entire array of model module and utility routine flow control and
C   debug flags.
      If(ILUOUT.ne.-1) WRITE ( 6,9400 )  IFLAG
C
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  If doing external a priori inputs, we must open the database in read-only
C   mode, get the number of sites and sources and the data base interval,
C   and then close the database.
      IF ( Input_sites .or. Input_stars .or. Input_ocean .or.
     *     Input_EOP ) Then
        Call KAI(1,0,0,1,LNAME_chr,iveri,'SAME','SAME',IVERO,
     *           LFCIO_chr,KERR)
C  Get the header record
        Call MVREC(1,1,1,kerr)
         If (kerr .ne. 0) Then 
          write(6,'("START/MVREC: Kerr = ",i5)') Kerr
          CALL CKILL ( 6HSTART , 1, KERR )
         Endif
C  Get number of sites, number of sources, Interval, EOP stuff
       If (Input_sites .or. Input_ocean)
     *   CALL GETI ('# SITES       ',NUMSIT, 1, 1, 1, NDO, KERR )
       If (Input_stars)
     *   CALL GETI ('# STARS       ',NUMSTR, 1, 1, 1, NDO, KERR)
C
       If (Input_EOP) Then
C             Get new 4-digit year access code
         CALL GETI ('INTRVAL4      ',Intrvl, 5, 2, 1, NDO, KERR)
C             If not there, then get old 2-digit year access code
         If (KERR.ne.0) 
     *     CALL GETI ('INTERVAL      ',Intrvl, 5, 2, 1, NDO, KERR)
         If (kerr.eq.0) then
C New code, for Y2K compliance. Takes 2 or 4 digit years.
          Iyear  = intrvl(1,1) 
          Imonth = intrvl(2,1)
          Iday   = intrvl(3,1)
          JD1    = JDY2K (Iyear, Imonth, Iday) 
          xintv(1) = JD1 +
     *           intrvl(4,1)/24.D0 + intrvl(5,1)/1440.D0
C
          Iyear  = intrvl(1,2)
          Imonth = intrvl(2,2)
          Iday   = intrvl(3,2)
          xintv(2) = JDY2K(Iyear, Imonth, Iday) +
     *           intrvl(4,2)/24.D0 + intrvl(5,2)/1440.D0
C
C       Get the UT1 and polar motion external input values
           Call GETEOP
C
C     Load the rotation epoch array.
       ndays = 2
       ROTEPH(1,1) = JD1
C
       if((intrvl(4,1).eq.0) .and. (intrvl(5,1).eq.0))
     .               ROTEPH(1,1) = ROTEPH(1,1) - 1.D0
       ROTEPH(2,1) = xintv(1) - ROTEPH(1,1) - 1.D0/1440.D0
       do while (ndays.le.MXUTPM .and.
     .         (ROTEPH(1,ndays-1)+1.D0).le.xintv(2))
         ROTEPH(1,ndays) = ROTEPH(1,ndays-1) + 1
         ROTEPH(2,ndays) = 0.D0
         ndays = ndays + 1
         enddo
       ndays = ndays - 1
C
         Else
           xintv(1) = 9.9D99
           xintv(2) = -9.9D99
         Endif
       Endif
C
C  Close the database
        Call FINIS(0)
      ENDIF
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C   Initialize the database in update mode.
C   In order to get around a quirk in the database catalog, we need to test to
C   see if the input and output keys are the same.
      ISAME = 0
      IF(LNAME_chr .eq. LNAMO_chr)  ISAME = 1
      IF(ISAME .EQ. 0) CALL KAI( KRUC, 0, 0, 1, LNAME_chr, IVERI,
     1             LNAMO_chr, 'SAME', IVERO ,LFCIO_chr, KERR )
C
      IF(ISAME .EQ. 1) CALL KAI(KRUC,0,0,1,LNAME_chr,IVERI,
     1             'SAME','SAME',IVERO,LFCIO_chr,KERR)
C
C   Check for interfacing errors with the database. If an error, then KILL CALC
C   and terminiate.
      IF ( KERR .NE. 0 ) CALL CKILL ( 6HSTART , 1, KERR )
C
C   Insert two history records.
      Write(Lhist1,'( "CALC",f5.2,1x,8a2,1x,a24,a10)')
     * xcalc,(loadm(jj),jj=1,8),STR24, computing_center(1:host_len)
      hist_len = trimlen(lhist1)
      CALL PHIST (hist_len, Lhist1)
      CALL PHIST (132, LHIST_chr )
C
C     Check for debug printout.
      IF(KSTAD .NE. 0) THEN
        WRITE(6,'("Debug output from subroutine START")')
        WRITE(6,'("LFILE = ",3A2)') LFILE
       ENDIF
C
C     Normal conclusion.
      RETURN
C
 9400 FORMAT (1X,          'KATMC =', I2, 3X, 'KATMD =', I2, 3X,
     1  'KAXOC =', I2, 3X, 'KAXOD =', I2, 3X, 'KPTDC =', I2, 3X,
     2  'KPTDD =', I2, /, ' KDNPC =', I2, 3X, 'KDNPD =', I2, 3X,
     3  'KETDC =', I2, 3X, 'KETDD =', I2, 3X, 'KIONC =', I2, 3X,
     4  'KIOND =', I2, /, ' KNUTC =', I2, 3X, 'KNUTD =', I2, 3X,
     5  'KPREC =', I2, 3X, 'KPRED =', I2, 3X, 'KRELC =', I2, 3X,
     6  'KRELD =', I2, /, ' KSITC =', I2, 3X, 'KSITD =', I2, 3X,
     7  'KSTRC =', I2, 3X, 'KSTRD =', I2, 3X, 'KUT1C =', I2, 3X,
     8  'KUT1D =', I2, /, ' KWOBC =', I2, 3X, 'KWOBD =', I2, /,
     9 ' KUTCC =', I2, 3X, 'KUTCD =', I2, 3X, 'KATIC =', I2, 3X,
     A  'KATID =', I2, 3X, 'KCTIC =', I2, 3X, 'KCTID =', I2,  /,
     1 ' KPEPC =', I2, 3X, 'KPEPD =', I2, 3X, 'KDIUC =', I2, 3X,
     2  'KDIUD =', I2, 3X, 'KM20C =', I2, 3X, 'KM20D =', I2,  /,
     3 ' KROSC =', I2, 3X, 'KROSD =', I2, 3X ,'KSTEC =', I2, 3X,
     4  'KSTED =', I2, 3X, 'KSUNC =', I2, 3X, 'KSUND =', I2,  /,
     5 ' KSARC =', I2, 3X, 'KSARD =', I2, 3X, 'KTHEC =', I2, 3X,
     6  'KTHED =', I2, 3X, 'KMATC =', I2, 3X, 'KMATD =', I2,  /,
     7 ' KVECC =', I2, 3X, 'KVECD =', I2, 3X, 'KOCEC =', I2, 3X,
     8  'KOCED =', I2, 3X, 'KASTC =', I2, 3X, 'KASTD =', I2, /,
     9 ' KSTAC =', I2, 3X, 'KSTAD =', I2, 3X, 'KPLXC =', I2, 3X,
     A  'KPLXD =', I2, 3X, 'KPANC =', I2, 3X, 'KPAND =', I2)
C
C   Here we tell the user that all requests are done. Also send a parameters
C   array back to the scheduling program saying that CALC terminated normally.
C
10000 CONTINUE
      CLOSE(Unit1)
10110 CONTINUE
      IF (ILUOUT.NE.-1) WRITE(6,'(/,
     . " All requested databases have been processed.",/,
     . " CALC",f5.2," thanks you and hopes you have a pleasant day.")')
     . xcalc
      IPAR_C = 'CALC 9 OK '
      CALL QUIT_CALC(IPAR)
      END
C
C******************************************************************************
      BLOCK DATA STACMB
      IMPLICIT None 
C
C 1.     STABD
C
C 1.1.1  STABD IS THE BLOCK DATA INITIALIZATION SECTION FOR THE START MODULE.
C        IT HOLDS THE LOAD MODULE DATE MESSAGE.
C
C 1.2.2  COMMON BLOCKS USED
      Real*8  XCALC
      Integer*2 NFLAG,NFLAGC,loadm(8),LFILE(3)
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
      Character*64 Computing_center
      CHARACTER*16 LOADM_CHR
      EQUIVALENCE (LOADM,LOADM_CHR)
      CHARACTER*6 C_LFILE, xdum1
      EQUIVALENCE (C_LFILE,LFILE)
C
C       VARIABLES 'TO':
C         1. LOADM - THE LOAD MODULE COMPILATION DATE MESSAGE.
C         2. NFLAG - THE TOTAL NUMBER OF CALC FLOW CONTROL AND DEBUG FLAGS.
C         3. LFILE(3) - THE NAME OF THE CALC CONTROL FILE.
C         4. XCALC - THE CALC PROGRAM VERSION NUMBER.
C
C     DATA LOADM_CHR /'Load 2000JUN02'/
      DATA LOADM_CHR /'Ver. 2001.01.12 '/
      DATA NFLAG /62/
      DATA C_LFILE /'CALCON'/
      DATA XCALC/9.12D0/
C
C 1.2.9  PROGRAMMER - BRUCE SCHUPLER 05/12/78
C                     BRUCE SCHUPLER 06/05/78
C                     BRUCE SCHUPLER 09/14/78
C                     BRUCE SCHUPLER 12/06/78
C                     BRUCE SCHUPLER 06/06/79
C                     BRUCE SCHUPLER 08/26/80
C                     DAVID GORDON   06/19/84
C                     DAVID GORDON   01/08/85 (REMOVED IDISC=59)
C                     SAVITA GOEL    06/02/87 (CDS FOR A900)
C                     Jim Ryan 89.07.25 Documentation simplified.
C                     Jim Ryan 89.12.12 UNIX-like database interface
C                                    implimented.
C                     David Gordon 94.04.15 Converted to Implicit None
C                     David Gordon 98.07.23 Removed ISECU, IDISC, and IOPEN
C                                    from Common /STACM/.
      END
C
C*****************************************************************************
      SUBROUTINE STAA
      IMPLICIT None
C
C 1.    STAA
C
C 1.1   STAA PROGRAM SPECIFICATION
C
C 1.1.1 STAA will do the ADDS to the database to provide for PUT'ing the CALC
C       flow control flag names and values into the database. STAA also places
C       the current version number of CALC into the database.
C
C 1.2   STAA PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - CALL STAA
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      Real*8    XCALC
      Integer*2 NFLAG,NFLAGC,loadm(8),LFILE(3)
      Character*64 Computing_center
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
C 
C     VARIABLES 'FROM' -
C       1) NFLAG - THE TOTAL NUMBER OF CALC FLOW CONTROL AND DEBUG FLAGS.
C     VARIABLES 'TO' -
C       1) NFLAGC - THE NUMBER OF CALC FLOW CONTROL FLAGS (= NFLAG / 2). 
C 
C 1.2.3 PROGRAM SPECIFICATIONS - NONE 
C 
C 1.2.4 DATA BASE ACCESS -
C     ACCESS CODES -
C       1) 'CALCFLGN' - THE DATA BASE ACCESS CODE FOR THE ARRAY OF THE CALC FLOW
C                       CONTROL FLAG NAMES.
C       2) 'CALCFLGV' - THE DATA BASE ACCESS CODE FOR THE ARRAY OF CALC FLOW
C                       CONTROL FLAG VALUES. 
C       3) 'CALC VER' - THE DATA BASE ACCESS CODE FOR THE CURRENT VERSION NUMBER
C                       OF PROGRAM CALC. 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 
C 1.2.6 SUBROUTINE INTERFACE -
C       CALLER SUBROUTINES - TOCUP
C       CALLED SUBROUTINES - ADDA,ADDI,ADDR 
C 
C 1.2.9 PROGRAMMER - BRUCE SCHUPLER 09/14/78
C                    DAVID GORDON   06/19/84
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                   implimented.
C                    David Gordon 94.04.15 Converted to Implicit None
C                    David Gordon 98.07.23 Removed ISECU, IDISC, and IOPEN
C                                   from Common /STACM/.
C
C   STAA Program Structure
C
C     Compute NFLAGC
      NFLAGC = NFLAG / 2
C
C   Do the ADD for the flag names
      CALL ADDA (1,'CALCFLGN','CALC flow control flags name def',
     1          2,NFLAGC,1)
C
C   Do the ADD for the flag values
      CALL ADDI (1,'CALCFLGV','CALC flow control flags valu def',
     1          NFLAGC,1,1)
C
C   Do the ADD for the CALC version number
      CALL ADDR (1,'CALC VER','CALC version number             ',
     1          1,1,1)
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE STAI
      IMPLICIT None
C
C 1.    STAI
C
C 1.1   STAI PROGRAM SPECIFICATION
C
C 1.1.1 STAI is the routine which will PUT the CALC control flag names and
C       values and the CALC version number into the database.
C
C 1.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'ccon.i'
C   VARIABLES 'FROM' -
C     1) IFLAG(62) - THE ARRAY OF CALC DEBUG AND CONTROL FLAGS.
C
      Real*8    XCALC
      Integer*2 NFLAG,NFLAGC,loadm(8),LFILE(3)
      Character*64 Computing_center
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
C   VARIABLES 'FROM' -
C     1) NFLAG  - THE TOTAL NUMBER OF CALC DEBUG AND CONTROL FLAGS.
C     2) NFLAGC - THE NUMBER OF CALC CONTROL FLAGS.
C
C 1.2.3 PROGRAM SPECIFICATIONS
C
      INTEGER*4   IFLAG(62), I, J
      INTEGER*2   LFLAGC(2,31), IFLAGC(31), idm7
      CHARACTER*4 C_LFLAGC(31)
      EQUIVALENCE (IFLAG(1),KATMC), (C_LFLAGC,LFLAGC)
C
      DATA C_LFLAGC /
     .  'ATMC','AXOC','PTDC','DNPC','ETDC','IONC',
     .  'NUTC','PREC','RELC','SITC','STRC','UT1C',
     .  'WOBC','UTCC','ATIC','CTIC','PEPC','DIUC',
     .  'M19C','ROSC','STEC','SUNC','SARC','THEC',
     .  'MATC','VECC','OCEC','ASTC','STAC','PLXC',
     .  'PANC'/
C
C 1.2.4 DATA BASE ACCESS
C     ACCESS CODES -
C       1) 'CALCFLGN      ' - THE DATA BASE ACCESS CODE FOR THE ARRAY
C                             OF CALC CONTROL FLAG NAMES.
C       2) 'CALCFLGV      ' - THE DATA BASE ACCESS CODE FOR THE ARRAY OF
C                             CALC CONTROL FLAG VALUES.
C       3) 'CALC VER      ' - THE DATA BASE ACCESS CODE FOR THE CURRENT
C                             CALC VERSION NUMBER.
C     'PUT' VARIABLES - 
C       1) LFLAGC(2,31) - THE ARRAY OF CALC CONTROL FLAG NAMES. 
C       2) IFLAGC(31)   - THE ARRAY OF CALC CONTROL FLAG VALUES.
C       3) XCALC        - THE CALC VERSION NUMBER (REAL NUMBER).
C 
C 1.2.6 SUBROUTINE INTERFACE
C       CALLER SUBROUTINES - INITAL 
C       CALLED SUBROUTINES - PUTA,PUTI,PUT4 
C
C 1.2.9 PROGRAMMER - BRUCE SCHUPLER 09/18/78
C                    BRUCE SCHUPLER 01/09/80
C                    BRUCE SCHUPLER 08/26/80
C                    DAVID GORDON   06/19/84  (VERSION #)
C                    DAVID GORDON   07/12/84  (POLE TIDE)
C                    DAVID GORDON   01/14/86  (IFLAG & OTHER DIMENSIONS FIXED)
C                    SAVITA GOEL    06/02/87  (CDS FOR A900)
C                    Jim Ryan 89.07.26 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.15 Converted to Implicit None
C                    David Gordon 98.07.23 Removed ISECU, IDISC, and IOPEN
C                                   from Common /STACM/.
C
C   STAI Program Structure
C
C   Copy the control flag from IFLAG to IFLAGC
      J = 0
      DO 100 I=1,NFLAG,2
        J = J + 1
        IFLAGC(J) = IFLAG(I)
100   CONTINUE
C
C   PUT the flag values.
      CALL PUTI ('CALCFLGV      ',IFLAGC,NFLAGC,1,1)
C
C   PUT the flag names.
      CALL PUTA ('CALCFLGN      ',LFLAGC,2,NFLAGC,1)
C
C   PUT the CALC version number.
      CALL PUT4 ('CALC VER      ',XCALC,1,1,1)
C
C   Check for debug output.
      IF(KSTAD .NE. 1) GO TO 1000
      WRITE(6,200)
200   FORMAT(1X,'Debug output from subroutine STAI')
      WRITE(6,220) NFLAG,NFLAGC
220   FORMAT(1X,'NFLAG = ',I5,5X,'NFLAGC = ',I5)
      WRITE(6,230) IFLAG
230   FORMAT(1X,'IFLAG = ',31I2,/,9X,31I2)
      WRITE(6,240) IFLAGC
240   FORMAT(1X,'IFLAGC =' ,10I5,/, 9X,10I5,/, 9X,10I5)
      WRITE(6,250) LFLAGC
250   FORMAT(1X,'LFLAGC = ',10(2A2,1X),/,10X,10(2A2,1X),
     *        /,10X,10(2A2,1X))
C
1000  CONTINUE
      RETURN
      END
C
C******************************************************************************
      Subroutine Upper(Isize, Iup, String)
      Implicit None
C
C  Subroutine to remove leading blanks in a character string and convert all
C   letters to upper case.
C
C    Input:
C     Isize = Number of characters
C     Iup = 0 ==> Don't convert to upper case
C     Iup = 1 ==> Do convert to upper case
C    Input/Output:
C     String = The character string
C
C     Programmer:
C      98.03.13  David Gordon  Original program written
C
      Integer*4 Isize, I, J, Icnt, Iup
      Character*(*) String
      Character*26 Upper_case, Lower_case
      Data Upper_case /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      Data Lower_case /'abcdefghijklmnopqrstuvwxyz'/
C
      Icnt = 0
C
C**    write(6,'(i4,2x,a)') Isize, String
C
C  Get rid of leading blanks
  20  Continue
      Icnt = Icnt + 1
      If (Icnt .ge. Isize) Return
      If (String(1:1) .eq. ' ') Then
        Do J=2,Isize
          String(J-1:J-1) = String(J:J)
        Enddo
        String(Isize:Isize) = ' '
        Go to 20
      Endif
C
C  Special case, 'None', 'NONE', 'none', etc.
       If (String(1:4) .eq. 'NONE') String(1:4) = 'None'
       If (String(1:4) .eq. 'none') String(1:4) = 'None'
C
       If (Iup .ne. 1) Return
C  Make all letters upper case
      Do J=1,Isize
        Do I=1,26
         If (String(J:J) .eq. Lower_case(I:I)) Then
          String(J:J) = Upper_case(I:I)
          Go to 30
         Endif
        Enddo
  30    Continue
      Enddo
C
      Return
      End
C******************************************************************************
C
      SUBROUTINE GETEOP
      Implicit None
C
C     Get Earth orientation table from an external file.
C      This routine reads a SOLVE-format EOP Mod file.
C
C     Common blocks used -
C
      INCLUDE 'inputs.i'
C            Variables 'from':
C              1. Ex_EOP - File name of the external EOP input file
C
      INCLUDE 'cmxut.i'
C            Variables 'from':
C              1. Xintv(2)  - First and last Julian Date in the data base
C            Variables 'to':
C              1. UT1IF(4)  - The final UT1 information array. This array
C                             contains respectively: 1) The Julian date of the
C                             first tabular point, 2) The increment in days of
C                             the tabular points, 3) The number of tabular
C                             points, 4) The units of the UT1 tabular array per
C                             second. (days, days, unitless, sec/table unit)
C              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
C                             (table units)
C              3. ISHRTFL   - The short period tidal terms flag, (unitless).
C                             = 1 --> UT1 table coming from input database is
C                             true UT1, (that is, fortnightly tidal terms have
C                             not been removed, as in the IRIS or IERS series).
C                             = -1 --> UT1 table coming from input database is
C                             UT1R, (that is, the Yoder fortnightly tidal terms
C                             HAVE been removed as in Bulletin B).
C                             = -2 --> UT1 table coming from input database is
C                             UT1S, (the S tidal terms HAVE been removed).
C              4. Leap_fix   - Used in external input mode. .True. means 
C                              correct the input EOP series for accumluated
C                              leap seconds. .False. means do not correct. 
C              5. UT1type    - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '. 
C                              For ''UT1-UTC ', leap second corrections 
C                              must be made.
C              6. EOP_time_scale - EOP table time scale, allowed values:
C                              'TAI     ', 'TCG     ', 'TDB     ',   
C                              'TDT     ', 'UTC     ', 'UNDEF   '.   
C                              Assumed default if not present => TDB
C
      INCLUDE 'cmwob.i'
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
C   Program Specifications -
      Integer*2   Getunit, Ios, KERR, idd3
      Integer*4   Iunit, Npts, Nerp, I
      Character*4 Utflag
      Character*4 Utflag
      Character   EOP_ID*15, dummy1*1
      Real*8      JD1, Uintv, Xmjdm, Mjdm, Mjds, Xmjds, XJDT, X10, Y10, 
     *            UT1T, Xmjdl, JD2
C
C   Program variables:
C          JD1    - First (full) Julian Day in EOP file
C          JD2    - Last (full) Julian Day in EOP file
C          Uintv  - EOP interval (usually 1.0 days)
C          Npts   - Number of points in the input EOP file
C          Nerp   - Number of trabular points to use in the UT1 and polar
C                   motion interpolation tables 
C          Utflag - Type of UT1 data (UT1, UT1R, UT1S)
C          Xmjdm  - Experiment midpoint (FJD)
C          Mjdm   - Experiment midpoint (MJD)
C          Mjds   - Time of first tabular point (MJD) 
C          Xmjds  - Time of first tabular point (FJD) 
C          Xmjdl  - Time of last tabular point (FJD) 
C
C     Programmer:
C      98.04.27  David Gordon - Original program written
C      99.11.19  David Gordon - Bug fix, CLOSE(Iunit) statements added.
C      99.11.23  David Gordon - Setting number of EOP points to 15 always,
C                               for compatability with Dbedit/Apriori and
C                               SOLVE. 
C    2000.12.11  David Gordon - Modify to skip comment statements in EOP
C                               mod file. 
C    2000.12.29  David Gordon - Modify to read/interpret new EOP mod file
C                               table header record.
C-----------------------------------------------------------------------
C
         Leap_fix = .False.
C
C  Open the EOP file 
C
       Iunit = Getunit()
       OPEN (unit=Iunit, file=Ex_EOP, status='old', Readonly, err=99,
     *       Iostat=Ios)
       Go to 101
  99   Continue
       Write(6,'(" Error opening external EOP file. Quitting! ")')
       CALL CKILL (6HGETEOP,1,Ios)
 101   Continue
C
C  Read new EOP mod file format. 2000.12.29
       Read(Iunit,1016, err=88) EOP_ID, JD1, Uintv, Npts, UT1type, 
     *                  EOP_time_scale
 1016  Format (A15,2X,F9.1,2X,F4.1,2X,I5,2X,A8,2X,A8)
C
       If (UT1type .eq. 'UT1-TAI ') Then               ! Normal case
         Utflag = 'UT1 '
         ISHRTFL =  1
         Leap_fix = .False.
         Go to 102
       Endif                                           ! Normal case
C
       If (UT1type .eq. 'UT1-UTC ') Then               ! Abnormal case
         Utflag = 'UT1 '
         ISHRTFL =  1
C          Will need to subtract leap seconds later (in UT1I)
         Leap_fix = .True.
         Go to 102 
       Endif                                           ! Abnormal case
C
C  If here, UT1type not properly defined. ?????
       Write(6,'(" Illegal UT1type in EOP mod file! Quitting! ")')
       CALL CKILL (6HGETEOP,1,Ios)
C
  88   Continue
C  Old format, just in case
       Backspace (Iunit)
       Read(Iunit,1018, err=89) JD1, Uintv, Npts, Utflag
 1018  Format (F9.1, F4.0, I4, 1X, A4)
       Write(6,'(/,"  !!! Using old EOP mod file format !!!",/)')
C
       Leap_fix = .False.
       UT1type = 'UT1-TAI '
       EOP_time_scale = 'UNDEF   '
C   Determine what the short period tidal term flag should be  
       IF(Utflag .eq. 'UT1 ') ISHRTFL =  1
       IF(Utflag .eq. 'UT1S') ISHRTFL = -2
       IF(Utflag .eq. 'UT1R') Then
         ISHRTFL = -1
         Write(6,'(" Cannot use UT1R data! Quitting! ")')
         CALL CKILL (6HGETEOP,1,Ios)
       ENDIF
 
C**   Write(6,'(" ISHRTFL = ",I3)') ISHRTFL
       Go to 102
  89   Continue
       Write(6,'(" Cannot read EOP file! Quitting! ")')
       CALL CKILL (6HGETEOP,1,Ios)
C
 102  Continue
C
C  Number of points in table. Set to 15 when data interval less than 2.0 days.
C   Increase by 1 for each additional day, up to 20 points.
       Nerp = 15 
C   Set to 15 in all cases! 99.11.23 -DG-
C*     Nerp = 14 + (xintv(2)-xintv(1)) 
C*     If (Nerp .lt. 15) Nerp = 15
C*     If (Nerp .gt. 20) Nerp = 20
C
C  Midpoint of experiment
       Xmjdm = (Xintv(1) + Xintv(2)) / 2.D0
C  First tabular point at midnight prior to (Nerp*Uintv)/2 days before midpoint
       Mjdm = Xmjdm - 2400000.5D0
       Mjds = Dint (Mjdm - (Nerp*Uintv)/2.D0)
C   First point at time Xmjds:
       Xmjds = Mjds + 2400000.5D0
C
C  Check if EOP file does not start early enough: 
C !! Require 15 points in all cases!!! 99.11.23 -DG-
       If (Xmjds .lt. JD1) Then
C*       Need at least one point before first observation time in data base
C*       If ( (Xintv(1) - Uintv) .lt. JD1 ) Then
           Write(6,'("GETEOP: Not enough EOP points before database")')
            KERR = 0
            Close (Iunit)
           CALL CKILL ( 6HGETEOP, 1, KERR )
C*       Else
C*         Recompute Nerp and reset first tabular point to JD1
C*         Nerp = Nerp - (JD1-Xmjds+.01)/Uintv
C*         Xmjds = JD1
C*       Endif
       Endif
C
C  Check if EOP file ends too early: 
C !! Require 15 points in all cases!!! 99.11.23 -DG-
       Xmjdl = Xmjds + (Nerp-1)*Uintv
       JD2 = JD1 + (Npts-1)*Uintv 
       If (Xmjdl .gt. JD2) Then
C*       Need at least one point after last observation time in data base
C*       If ( (Xintv(2) + Uintv) .gt. JD2 ) Then
           Write(6,'("GETEOP: Not enough EOP points after database")')
            KERR = 0
            Close (Iunit)
           CALL CKILL ( 6HGETEOP, 1, KERR )
C*       Else
C          Recompute Nerp 
C*         Nerp = Nerp - ( Xmjdl-JD2+.01)/Uintv
C*       Endif
       Endif
C
C     Write(6,1021) Xintv, Xmjdm, Mjdm, Mjds, Xmjds 
C1021 Format ('Xintv(2), Xmjdm, Mjdm, Mjds, Xmjds: ',2F20.8,/,5X,
C    *        4F19.8)  
C
C  Fill the UT1 and Wobble information arrays
       UT1IF(1) = Xmjds
       UT1IF(2) = Uintv
       UT1IF(3) = Nerp
       UT1IF(4) = 1.0D0
C**   Write(6,'(" UT1IF(4) ",4F15.6)') UT1IF
C
       WOBIF(1) = Xmjds
       WOBIF(2) = Uintv
       WOBIF(3) = Nerp
C**   Write(6,'(" WOBIF(3) ",3F15.6)') WOBIF
C
C  Get the EOP points:
C   Read till first point found
  50   Continue
       Read (Iunit,*,err=50) XJDT, X10, Y10, UT1T
C      Read (Iunit,1019) XJDT, X10, Y10, UT1T
 1019  Format (F9.1, 2F8.4, I10)
C
       If (DABS(XJDT - XMJDS) .le. 1.D-8) Then
         Backspace (Iunit)
         Go to 70
       Else
         Go to 50
       Endif
C
  70   Continue
       Do I = 1,Nerp
C      Read (Iunit,1019) XJDT, X10, Y10, UT1T
       Read (Iunit,*) XJDT, X10, Y10, UT1T
C  Input units are : X/Y => 0.1 arc-sec; UT1 => microseconds
C  Convert to milli-arc-seconds and time seconds, and change 
C   UT1-TAI to TAI-UT1 (or UT1-UTC to UTC-UT1)
       UT1PT(I) = -UT1T/1.D6
       XYWOB(1,I) = X10/10.D0 * 1.D3
       XYWOB(2,I) = Y10/10.D0 * 1.D3
C
       Enddo
C      Write (6,1023)   (XYWOB(1,I), XYWOB(2,I), UT1PT(I), I=1,Nerp)
C1023  Format (' External EOPs: ',20(/,3F20.10))
C
       Close(Iunit)
C
       Return
       End
C*****************************************************************************
C
      REAL*8 FUNCTION JDY2K (IYEAR, IMONTH, IDAY) 
      Implicit None 
C 
C     Fucntion JDY2K: Function to convert year, month, day to full Julian 
C     day. The year can be either a four-digit year or a two-digit year.  
C 
C     If a 4-digit year, this function is good from 1 March 1900 to 31
C     December 2099.
C
C     If a 2-digit year, this function is good from 1 January 1970 to 
C     31 December 2069. If year is 70 - 99, 1900 is added. If year is 
C     00 - 69, 2000 is added. 
C
C     Programmer:
C      98.07.23  D. Gordon  Function written from code in cutcu.f
C
      Integer*4 IYEAR, IMONTH, IDAY, IY, IM, ID 
C
       IY = IYEAR
       IM = IMONTH
       ID = IDAY
C
       If (IY .ge. 70 .and. IY .le. 99) Then
        IY = IY + 1900
        Go To 100
       Endif
C
       If (IY .ge. 0 .and. IY .le. 69) Then
        IY = IY + 2000
        Go To 100
       Endif
C
       If (IY .gt.1900 .and. IY .le. 2099) Then
        Go To 100
       Endif
C
C     Year out of range if we get here
       Print *, ' JDY2K, Year out of Range, Stopping! ', IY
       Stop 
C
 100   Continue
C
        JDY2K = 367.D0*IY - (7 * ( IY + (IM+9)/12) )/4 + 
     .          (275*IM)/9 + ID + 1721013.5D0
C
C      Write(6,1000) IYEAR, IMONTH, IDAY, IY, IM, ID, JDY2k
 1000  Format(/,'Function JDY2K: ',/,' Input, Modified Y,M,D: ',
     .        2x,3I5,5x,3I5,/,' JDY2K ', F15.2)
      Return
      End
