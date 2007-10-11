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
      Integer*2 NFLAG,NFLAGC,loadm(7),LFILE(3)
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
C         1. loadm(7)  - The load module compilation date message.
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
C
C   START Program Structure
C
      ILUOUT = iout
      MXUTPM = 20
C
C   Open the CALC control file first time through (first database).
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
      Integer*2 NFLAG,NFLAGC,loadm(7),LFILE(3)
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
      Character*64 Computing_center
      CHARACTER*14 LOADM_CHR
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
      DATA LOADM_CHR /'Load 99OCT04  '/
      DATA NFLAG /62/
      DATA C_LFILE /'CALCON'/
      DATA XCALC/9.10D0/
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
      Integer*2 NFLAG,NFLAGC,loadm(7),LFILE(3)
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
      Integer*2 NFLAG,NFLAGC,loadm(7),LFILE(3)
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
       write(6,'(i4,2x,a)') Isize, String
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
