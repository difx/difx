      SUBROUTINE STRA
      IMPLICIT None
C
C 1.    STRA
C
C 1.1   STRA PROGRAM SPECIFICATION
C
C 1.1.1 STRA adds entries to the table of contents for the
C       STAR Module text message and the partial derivative array.
C
C 1.2   STRA PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - NONE
C
C 1.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'             
C            VARIABLES 'FROM':
C             1. KSTRC  -  THE STAR MODULE FLOW CONTROL FLAG.
C                          0 => Do not add proper motion constants to data
C                               base when in external file mode (Lcode 
C                               'PRMOTION'). Do not compute any proper
C                               motion contributions. Remove any previous
C                               proper motion contribution Lcodes.
C                          1 => Add proper motion constants ('PRMOTION') if
C                               in external file input mode. Compute proper
C                               motion contributions if proper motion 
C                               constants are available and insert in data
C                               base under Lcode 'PMOTNCON'. Do NOT add 
C                               these contributions to the theoretical.
C                               [They can be ADDED in SOLVE to get proper
C                               motion corrected delays and rates.]
C                          2 => Add proper motion constants ('PRMOTION') if
C                               in external file input mode. Also, ADD the
C                               proper motions to source vector and use the
C                               proper motion corrected source vector 
C                               throughout all computations. Compute a 
C                               contribution (ADD to theoretical) that will
C                               (approximately) return the delays and rates
C                               to their non-proper motion corrected values, 
C                               and put in Lcode 'PMOT2CON'. For cases where
C                               there is a large accumulated proper motion
C                               (greater than ~1 arcsec). Intended for 
C                               correlator useage only. USE WITH CAUTION!! 
C             2. KSTRD  -  THE STAR MODULE DEBUG OUTPUT FLAG.
C             3. KPLXC  -  THE PARALLAX MODULE FLOW CONTROL FLAG.
C                          0 => Do not insert source distances into data 
C                               base when in external file input mode, even 
C                               if distances given in external source file. 
C                          1 => Insert source distances into data base in 
C                               external file input mode, even if all zeros,
C                               using Lcode 'DISTPSEC'. 
C
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_stars - T/F logical flag telling whether to use
C                               external star (radio sources) a priori input.
C 
      INCLUDE 'cmxsr.i'
C           VARIABLES 'TO':
C             1. NUMSTR - The number of stars (radio sources) in the Star 
C                         catalog.
C
C 1.2.3 PROGRAM SPECIFICATIONS - NONE 
C 
C 1.2.4 DATA BASE ACCESS: 
C            ACCESS CODES:
C              1. 'STR MESS' - THE DATA BASE ACCESS CODE FOR THE 
C                              STAR MODULE TEXT MESSAGE. 
C              2. 'STR CFLG' - THE DATA BASE ACCESS CODE FOR THE
C                              STAR MODULE FLOW CONTROL MESSAGE.
C              3. 'STR PART' - THE DATA BASE ACCESS CODE FOR THE 
C                              STAR MODULE PARTIAL DERIVATIVES ARRAY.
C              4. 'PRMOTION' - The data base access code for the proper
C                              motion array. First variable runs over 
C                              RA velocity (arc-sec/year), Declination
C                              velocity (arc-sec/year), and epoch for 
C                              which the J2000 coordinates in 'STAR2000'
C                              precessed to date are correct (epoch for
C                              which corrections should be zero). Second 
C                              index runs over the sources, same order as
C                              in 'STRNAMES'. Zeros imply unknown.
C              5. 'DISTPSEC' - The data base access code for the source 
C                              distance array. Units are parsecs; zero
C                              implies unknown.
C              6. 'PMOTNCON' - Proper motion contributions (sec, sec/sec).
C                              Add to normal delays and rates to correct for
C                              proper motions to date of observation. Exists
C                              only if KSTRC=1.
C              7. 'PMOT2CON' - Proper motion contributions (sec, sec/sec)
C                              to return to non-proper motion delays and 
C                              rates. Exists only if KSTRC=2. Add to proper
C                              motion corrected delays and rates to return
C                              to the uncorrected values.
C              8. 'RADECADD' - Proper motion offsets (if proper motion turned
C                              on) in RA and Dec. Add to RA and Dec to get 
C                              corrected values. Used if KSTRC = 1.
C              9. "STARPRMO' - RA and Declinations after correcting for proper
C                              motion. Used if KSTRC = 2.
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP 
C             CALLED SUBROUTINES: ADDA, ADDR
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/14/77
C                    SAVITA GOEL    06/03/87 (CDS FOR A900)
C                    Jim Ryan 89.07.09 Documentation simplied.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                        implimented.
C                    David Gordon 94.04.15 Converted to Implicit None
C                    David Gordon 98.03.19 Modified to do ADDR for source
C                        positions in the case of external source a priori 
C                        input.
C                    David Gordon 98.09.08 Adds for 'PRMOTION' and 'DISTPSEC'
C                        for the case of external source a priori input.
C                    David Gordon 98.09.14 Adds and Deletes for 'PMOTNCON' 
C                        and 'PMOT2CON', and various mods for proper motion
C                        contibutions.
C                    David Gordon 98.11.25 Adds/Deletes for 'RADECADD' and
C                        'STARPRMO', depending on value of KSTRC.
C
C     STRA PROGRAM STRUCTURE
C
C     ADD for the Star Module text message.
      CALL ADDA (1,'STR MESS','Star module message definition  ',
     1     40, 1, 1 )
C
C   ADD for the Star Module flow control message.
      CALL ADDA (1,'STR CFLG','Parallax flow control mess def  ',
     1     40,1,1)
C
C     ADD for the STAR Module partial derivatives.
      CALL ADDR (2,'STR PART','Star partial derivatives def.   ',
     1     2, 2, 1 )
C
C   Do ADDR to replace the source positions in the data base in the case of 
C    external file input
      IF (Input_stars) THEN
       CALL ADDR(1,'STAR2000','J2000 source RAs, decs (rd,rd). ',
     *          2, NUMSTR, 1)
       CALL ADDA(1,'STAR REF','Source of coordinate values.    ',
     *          10, NUMSTR, 1)
C
       IF (KSTRC.eq.1 .or. KSTRC.eq.2) 
     *  CALL ADDR(1,'PRMOTION','Proper motions/Epoch (a-sec, yr)',
     *           3, NUMSTR, 1)
C
       IF (KPLXC.eq.1) 
     *  CALL ADDR(1,'DISTPSEC','Source Distances (parsecs)      ',
     *           NUMSTR, 1, 1)
      ENDIF
C
       IF (KSTRC.eq.0) Then
        CALL DELR(2,'PMOTNCON')
        CALL DELR(2,'PMOT2CON')
       ENDIF
C
       If (KSTRC.eq.1) Then
        CALL ADDR(1,'RADECADD','Add to RA/Dec for proper motion ',
     *           2,Numstr,1)
        CALL ADDR(2,'PMOTNCON','Proper Motion contribs, sec, s/s',
     *           2, 1 1)
        CALL DELR(1,'STARPRMO')
        CALL DELR(2,'PMOT2CON')
       Endif
C
       If (KSTRC.eq.2) Then
        CALL ADDR(1,'STARPRMO','Proper motion corrected RA/Dec  ',
     *           2,Numstr,1)
        CALL ADDR(2,'PMOT2CON','Proper Motion Remover,  sec, s/s',
     *           2, 1 1)
        CALL DELR(1,'RADECADD')
        CALL DELR(2,'PMOTNCON')
       Endif
C
C     Normal conclusion.
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE STRI
      IMPLICIT None
C
C 3.    STRI
C
C 3.1   STRI PROGRAM SPECIFICATION
C
C 3.1.1 STRI is the STAR Module input and initialization section.
C
C 3.2   STRI PROGRAM INTERFACE
C 
C 3.2.1 CALLING SEQUENCE - NONE 
C 
C 3.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'cmxsr.i'
C           VARIABLES 'TO':
C             1. LNSTAR(4,MAX_ARC_SRC) - THE EIGHT ALPHANUMERIC CHARACTER NAMES
C                                        OF THE STARS IN THE STAR CATALOG.
C             2. NUMSTR                - THE NUMBER OF STARS IN THE STAR
C                                        CATALOG.
C             3. RADEC(2,MAX_ARC_SRC)  - THE RIGHT ASCENSIONS AND DECLINATIONS 
C                                        OF THE STARS IN THE STAR CATALOG.
C                                        (RAD, RAD)
C             4. P_motion(3,Max_arc_src)-The RA and Dec proper motions and 
C                                        appropriate epoch for stars in the 
C                                        star catalog. (arc-sec/yr, arc-sec/yr,
C                                        year (i.e. 1995.5, etc.))
C             5. D_psec(Max_arc_src)   - Distances, if known, for stars in the 
C                                        star catalog. Zero => unknown.
C                                        (parsecs)
C             6. PRcorr(2,Max_arc_src) - Proper motion corrections in RA and
C                                        Declination for each star. (Radians)
C 
      INCLUDE 'ccon.i'             
C            VARIABLES 'FROM':
C             1. KSTRC  -  THE STAR MODULE FLOW CONTROL FLAG.
C                          0 => Do not add proper motion constants to data
C                               base when in external file mode (Lcode 
C                               'PRMOTION'). Do not compute any proper
C                               motion contributions. Remove any previous
C                               proper motion contribution Lcodes.
C                          1 => Add proper motion constants ('PRMOTION') if
C                               in external file input mode. Compute proper
C                               motion contributions if proper motion 
C                               constants are available and insert in data
C                               base under Lcode 'PMOTNCON'. Do NOT add 
C                               these contributions to the theoretical.
C                               [They can be ADDED in SOLVE to get proper
C                               motion corrected delays and rates.]
C                               The proper motion RA and Delination offsets,
C                               in radians, will be stored for each source in
C                               the type 1 Lcode 'RADECADD'. 
C                          2 => Add proper motion constants ('PRMOTION') if
C                               in external file input mode. Also, ADD the
C                               proper motions to source vector and use the
C                               proper motion corrected source vector 
C                               throughout all computations. Compute a 
C                               contribution (ADD to theoretical) that will
C                               (approximately) return the delays and rates
C                               to their non-proper motion corrected values, 
C                               and put in Lcode 'PMOT2CON'. For cases where
C                               there is a large accumulated proper motion
C                               (greater than ~1 arcsec). Intended for 
C                               correlator useage only. USE WITH CAUTION!! 
C                               The RA's and Dec's for each star, AFTER
C                               correcting for proper motion offsets, are
C                               output in the type 1 Lcode 'STARPRMO'. These
C                               should be captured and used in any post
C                               processing adjustments, etc. 
C             2. KSTRD  -  THE STAR MODULE DEBUG OUTPUT FLAG.
C             3. KPLXC  -  THE PARALLAX MODULE FLOW CONTROL FLAG.
C                          0 => Do not insert source distances into data 
C                               base when in external file input mode, even 
C                               if distances given in external source file. 
C                          1 => Insert source distances into data base in 
C                               external file input mode, even if all zeros,
C                               using Lcode 'DISTPSEC'. 
C
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C         CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                 (RAD/ARCSECOND)
C
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_stars - T/F logical flag telling whether to use star
C                               a priori external input
C              2. Ex_stars    - File name for stars external file input.
C                               If 'NONE' or blank, then no external inputs
C
C 3.2.3 PROGRAM SPECIFICATIONS -
C
      Real*8    Xdoy1, Xepoch, X_frac
      Integer*4 I, J, N, NN, Imdoy(12)
      INTEGER*2 KERR(5), LSTRM(40), LOFF(4), LON1(40), LON2(40), NDO(3),
     *          Intrvl(5,2), KERX, idum2
      CHARACTER*40 C_LSTRM(2), C_OFF(2), C_ON1(2), C_ON2(2) 
      EQUIVALENCE (C_LSTRM,LSTRM), (C_OFF,LOFF), (C_ON1,LON1), 
     *            (C_ON2,LON2)
C
      Data Imdoy /0,31,59,90,120,151,181,212,243,273,304,334/
C
      DATA C_LSTRM / 'Star Module - Last modification 98.09.15',
     *               ', D. Gordon, GSFC                       '/
C
      DATA C_OFF / 'Proper Motion Corrections OFF.          ',
     *             '                                        '/
C
      DATA C_ON1 / 'Proper Motion Corrections ON, but NOT in',
     *             'cluded in theoreticals.                 '/
C
      DATA C_ON2 / 'Proper Motion Corrections ON, AND includ',
     *             'ed in theoreticals.                     '/
C
C 3.2.4 DATA BASE ACCESS -
C          'GET' VARIABLES:
C             1. LNSTAR(4,MAX_ARC_SRC) - THE EIGHT ALPHANUMMERIC CHARACTER
C                                        NAMES OF THE STARS IN THE STAR CATALOG
C                                        (ALPHANUMERIC). 
C             2. NUMSTR                - THE NUMBER OF STARS IN THE STAR
C                                        CATALOG.
C             3. RADEC(2,MAX_ARC_SRC)  - THE RIGHT ASCENSIONS AND DECLINATIONS
C                                        OF THE STARS IN THE STAR CATALOG
C                                        (RAD, RAD).
C           'PUT' VARIABLES: 
C             1. LSTRM(40)  -  THE STAR MODULE TEXT MESSAGE.
C 
C           ACCESS CODES:
C             1. 'STR MESS' - THE DATA BASE ACCESS CODE FOR THE STAR MODULE
C                             TEXT MESSAGE. 
C             2. 'STAR2000' - THE DATA BASE ACCESS CODE FOR THE ARRAY OF STAR 
C                             RIGHT ASCENSIONS AND DECLINATIONS IN J2000.0 
C                             COORDINATES.
C             3. 'STRNAMES' - THE DATA BASE ACCESS CODE FOR THE ARRAY OF STAR
C                             NAMES.
C             4. '# STARS ' - DATA BASE ACCESS CODE FOR THE NUMBER OF STARS.
C             5. 'PRMOTION' - The data base access code for the proper
C                             motion array. First variable runs over 
C                             RA velocity (arc-sec/year), Declination
C                             velocity (arc-sec/year), and epoch for 
C                             which the J2000 coordinates in 'STAR2000'
C                             precessed to date are correct (epoch for
C                             which corrections should be zero). Second 
C                             index runs over the sources, same order as
C                             in 'STRNAMES'. Zeros imply unknown.
C             6. 'DISTPSEC' - The data base access code for the source 
C                             distance array. Units are parsecs; zero
C                             implies unknown.
C             7. 'RADECADD' - Proper motion offsets (if proper motion turned
C                             on) in RA and Dec. Add to RA and Dec to get 
C                             corrected values. Used if KSTRC = 1.
C             8. "STARPRMO' - RA and Declinations after correcting for proper
C                             motion. Used if KSTRC = 2.
C 
C 3.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG/ERROR OUTPUT
C
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL
C             CALLED SUBROUTINES: GETA, GETI, GET4, KILL, PUTA, STRIN
C
C 3.2.7 CONSTANTS USED - NONE
C
C 3.2.8 PROGRAM VARIABLES -
C             1.  KERR(3) - THE DATA BASE ERROR RETURN FLAGS.
C             2.  NDO(3)  - THE DATA BASE RETURN ARRAY INDICES.
C
C 3.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/14/77
C                    CHOPO MA       08/05/81
C                    Jim Ryan 89.07.09 Documentation simplied.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                        implimented.
C                    David Gordon 94.04.15 Converted to Implicit None
C                    B. Archinal  95.11.13 Max # sources set in cmxsr.i.
C                    David Gordon 98.03.19 Mods for external file input of 
C                        source coordinates
C                    David Gordon 98.09.15 Mods for proper motions and
C                        distances.
C                    David Gordon 98.11.25 Moved proper motion source offset
C                        computation here so that the RA/Dec offsets or 
C                        corrected RA/Dec's could be output for capture by
C                        correlator users. Added Lcodes 'RADECADD' and 
C                        'STARPRMO'.
C
C     STRI PROGRAM STRUCTURE
C
C   PUT the Star Module text message.
      CALL PUTA ('STR MESS      ', LSTRM, 40, 1, 1 )
C   PUT the module flow control message.
      IF (KSTRC .EQ. 0) CALL PUTA('STR CFLG      ',LOFF ,40,1,1)
      IF (KSTRC .EQ. 1) CALL PUTA('STR CFLG      ',LON1 ,40,1,1)
      IF (KSTRC .EQ. 2) CALL PUTA('STR CFLG      ',LON2 ,40,1,1)
C
C   GET the star catalog from the input database.
      CALL GETI('# STARS       ',NUMSTR,1,       1,1,NDO,KERR(1))
      CALL GETA('STRNAMES      ',LNSTAR,4,(NUMSTR),1,NDO,KERR(2))
C
       Pmotion = 0
       Dpsec   = 0
C  Get source positions either from the data base or from an external file
      IF (Input_stars) Then                         !Get Star Info
        Call STRIN(Kerr)
      ELSE                                          !Get Star Info
        CALL GET4('STAR2000      ',RADEC ,2,NUMSTR,1,NDO,KERR(3))
C
C  Also get proper motions, if they are in the data base
       IF(KSTRC.eq.1 .or. KSTRC.eq.2 .or. KSTRC.eq.3) Then  !Get proper motions
        CALL GET4('PRMOTION      ',P_motion ,3,NUMSTR,1,NDO,KERR(4))
        If (Kerr(4) .eq. 0) Then
C   Lcode exists, but check that values are not all zero
         Do N=1,Numstr
          If((P_motion(3,N).ge.1900.D0) .AND. 
     *       (DABS(P_motion(1,N)) .gt. 1.D-12  .or. 
     *        DABS(P_motion(2,N)) .gt. 1.D-12) )  Pmotion = Pmotion + 1
          Enddo
        Else
C  No Lcode, therefore no proper motions, zero out the array. 
         Do N=1,Numstr
          P_motion(1,N) = 0.D0
          P_motion(2,N) = 0.D0
          P_motion(3,N) = 0.D0
         Enddo
          Pmotion = 0
        Endif
       ENDIF                                        !Get proper motions
C
C  Also get distances, if they are in the data base
       IF(KPLXC.eq.1) Then                          !Get distances
        CALL GET4('DISTPSEC      ',D_Psec,NUMSTR,1,1,NDO,KERR(5))
        If (Kerr(5) .eq. 0) Then
C   Check that values not all zero
         Do N=1,Numstr
          If( D_psec(N).ge.1.D0 ) Dpsec = Dpsec + 1
         Enddo
        Else
C  No distances, zero out the array. 
         Do N=1,Numstr
          D_psec(N) = 0.D0
         Enddo
          Dpsec = 0
        Endif
       ENDIF                                        !Get distances
C
      ENDIF                                         !Get Star Info
C     print *,' STRI: Pmotion, Dpsec = ', Pmotion, Dpsec
C
C  Proper Motions: If proper motions applied, do the work here. RA and Dec
C   offsets will be computed once for each source, and will remain constant
C   throughout this Calc run. If proper motions are added to the source
C   coordinates (KSTRC = 2), those new coordinates will be output in the 
C   type 1 Lcode 'STARPRMO' for capture and later use. If proper motions 
C   are used only to compute a proper motion contribution but not added 
C   to the source coordinates (KSTRC = 1), then those offsets will be 
C   output in the type 1 Lcode 'RADECADD'.
C
       IF (KSTRC.eq.1 .or. KSTRC.eq.2) THEN           !Do proper motions
C
        IF (Pmotion .eq. 0) Then
         Do N = 1, Numstr
          PRcorr(1,N) = 0.D0
          PRcorr(2,N) = 0.D0
         Enddo
         Go to 250
        ENDIF
C
C           Look for 4-digit year interval
          CALL GETI ('INTRVAL4      ',Intrvl, 5, 2, 1, NDO, KERX )
C           If no 4-digit year interval, get 2-digit year interval
         If (KERX.ne.0) then
          CALL GETI ('INTERVAL      ',Intrvl, 5, 2, 1, NDO, KERX )
C            Convert 2-digit year to 4-digit year
          If (Intrvl(1,1) .ge. 70 .and. Intrvl(1,1) .le. 99)
     *        Intrvl(1,1) = Intrvl(1,1)+1900
          If (Intrvl(1,1) .ge.  0 .and. Intrvl(1,1) .le. 69)
     *        Intrvl(1,1) = Intrvl(1,1)+2000
         Endif
C
C  Convert start time to year and fraction, not worrying about leap years:
           xdoy1 = imdoy(Intrvl(2,1)) + Intrvl(3,1) + Intrvl(4,1)/24.d0
     *             + Intrvl(5,1)/1440.d0
           Xepoch = Intrvl(1,1) + xdoy1/365.D0
              write (6,*) " Xepoch  = ", Xepoch
              write (6,*) " P_epoch = ", P_motion(3,N)
             Do N = 1, Numstr
              X_frac = Xepoch - P_motion(3,N)
               If (P_motion(3,N).eq.0.D0) X_frac = 0.D0
              PRcorr(1,N) = P_motion(1,N)*X_frac*CONVDS/DCOS(RADEC(2,N))
              PRcorr(2,N) = P_motion(2,N)*X_frac*CONVDS
                If (KSTRC.eq.2) Then
                 RADEC(1,N) = RADEC(1,N) + PRcorr(1,N)
                 RADEC(2,N) = RADEC(2,N) + PRcorr(2,N)
                 PRcorr(1,N) = -PRcorr(1,N) 
                 PRcorr(1,N) = -PRcorr(1,N) 
                Endif
             Enddo
 250    Continue
        IF (KSTRC.eq.1) CALL PUT4 ('RADECADD      ',PRcorr,2,Numstr,1)
        IF (KSTRC.eq.2) CALL PUT4 ('STARPRMO      ',RADEC ,2,Numstr,1)
       ENDIF                                          !Do proper motions
C
      DO 300  N = 1,3
        NN = N
        IF ( KERR(N) .EQ. 0 ) GO TO 300
           CALL CKILL (6HSTRI  , NN, KERR(NN) )
  300 CONTINUE
C
C   Check KSTRD for debug output.
      IF ( KSTRD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine STRI." )
      WRITE(6,8)' RADEC   ',RADEC
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,7)' NUMSTR  ',NUMSTR
    7 FORMAT(A6,15I8/(9X,15I8))
      WRITE ( 6, 9200 )  LNSTAR
 9200 FORMAT (1X, "LNSTAR = ", /, 1X, 10 ( 10 ( 4A2, 2X ), /, 1X ) )
      If (KSTRC.eq.1 .or .KSTRC.eq.1)  WRITE(6,8)' P_motion ', 
     *       ((P_motion(I,J), I=1,3), J=1,Numstr)
      If (KPLXC.eq.1)  WRITE(6,8)' D_Psec ', (D_Psec(J), J=1,Numstr)
C
C     Normal conclusion.
  500 RETURN
      END
C
C******************************************************************************
      SUBROUTINE STRG (XJD, UTC, STAR)
      IMPLICIT None
C
C 4.    STRG
C
C 4.1   STRG PROGRAM SPECIFICATION
C
C 4.1.1 STRG is the geometry section of the STAR Module. STRG computes the 
C       J2000.0 unit vector in the direction of the source. Now will compute
C       proper motions in special situations.
C
C 4.2   STRG PROGRAM INTERFACE
C 
C 4.2.1 CALLING SEQUENCE -
C           OUTPUT VARIABLES: 
C             1. STAR(3)  -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C 
C 4.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'cmxsr.i'
C           VARIABLES 'FROM':
C             1. LNSTAR(4,MAX_ARC_SRC) - THE EIGHT ALPHANUMERIC CHARACTER NAMES
C                                        OF THE STARS IN THE STAR CATALOG.
C             2. NUMSTR                - THE NUMBER OF STARS IN THE STAR 
C                                        CATALOG.
C             3. RADEC(2,MAX_ARC_SRC)  - THE RIGHT ASCENSIONS AND DECLINATIONS
C                                        OF THE STARS IN THE STAR CATALOG.
C                                        (RAD, RAD)
C           VARIABLES 'TO':
C             1. CD   -  THE COSINE OF THE DECLINATION OF THE STAR BEING USED IN
C                        THE CURRENT OBSERVATION. (UNITLESS) 
C             2. CRA  -  THE COSINE OF THE RIGHT ASCENSION OF THE STAR BEING
C                        USED IN THE CURRENT OBSERVATION. (UNITLESS) 
C             3. SD   -  THE SINE OF THE DECLINATION OF THE STAR BEING USED IN
C                        THE CURRENT OBSERVATION. (UNITLESS) 
C             4. SRA  -  THE SINE OF THE RIGHT ASCENSION OF THE STAR BEING USED
C                        IN THE CURRENT OBSERVATION. (UNITLESS) 
C 
      INCLUDE 'ccon.i'               
C           VARIABLES 'FROM':
C             1. KSTRC  -  THE STAR MODULE FLOW CONTROL FLAG. 
C             2. KSTRD  -  THE STAR MODULE DEBUG OUTPUT FLAG. 
C
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C         CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                 (RAD/ARCSECOND)
C
      Real*8 PR_RA, PR_DEC, PM_RA, PM_DEC
      Common /Pmotn/ PR_RA, PR_DEC, PM_RA, PM_DEC
C
      Real*8 Dparsec, t_prlx(2)
      Common /PRLX/ Dparsec, t_prlx
C 
C 4.2.3 PROGRAM SPECIFICATIONS -
      Real*8  STAR(3), XJD, UTC 
C 
C 4.2.4 DATA BASE ACCESS -
C           'GET' VARIABLES: 
C             1. LSTRNM(4)  -  THE EIGHT ALPHAMERIC CHARACTER STAR NAME FOR THE
C                               CURRENT OBSERVATION. (ALPHAMERIC) 
C            ACCESS CODES:
C             1. 'STAR ID '  -  THE DATA BASE ACCESS CODE FOR THE STAR NAME OF
C                               THE CURRENT OBSERVATION. 
C 
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG
C             CALLED SUBROUTINES: DCOS, DSIN, GETA, KILL, JDY2K
C
C 4.2.7 CONSTANTS USED - NONE
C
C 4.2.8 PROGRAM VARIABLES -
      Integer*2 LSTRNM(4), NDO(3), KERR
      Integer*4 N, NN, IM, ID, Ieph 
      REAL*8    RIGHT_ASC, DECLINATION, Xepoch, XJAN1, Xdays, Difyrs,
     *          JDepoch, JDY2K
C
C             1. KERR        - THE DATA BASE ERROR RETURN FLAG.
C             2. NDO(3)      - THE DATA BASE RETURN ARRAY INDICES.
C             3. RIGHT_ASC   - LOCAL VARIABLE FOR HOLDING THE RA.
C             4. DECLINATION - LOCAL VARIABLE FOR HOLDING THE DEC.
  
C 4.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/14/77
C                    JIM RYAN      88.01.07
C                    Jim Ryan 89.07.09 Documentation simplied.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                          implimented.
C                    David Gordon 94.04.15 Converted to Implicit None
C                    B. Archinal  95.11.13 Max # sources set in cmxsr.i.
C                    David Gordon 98.09.08 Add /CMATH/ Common block.
C                    David Gordon 98.09.11 Add new /Pmotn/ and /PRLX/ common
C                          blocks to hold proper motion offsets and distance.
C                          Code added to compute and handle proper motions, 
C                          if that option is turned on.
C                    David Gordon 98.11.25 Removed proper motion computations
C                          and moved them to the initialization section. 
C
C     STRG PROGRAM STRUCTURE
C
C     GET the star name of the current observation.
C-VLBA Modified for the VLBA Correlator. Source index number is supplied
C     to Calc by the modlTask. Change GETA to GETI.
C      CALL GETA ('STAR ID       ', LSTRNM, 4, 1, 1, NDO, KERR )
      CALL GETI  ('STAR ID       ', LSTRNM, 1, 1, 1, NDO, KERR)
      N = LSTRNM(1)
      IF ( KERR .EQ. 0 )  GO TO 320
      CALL CKILL (6HSTRG  , 1, KERR )
C
C     Construct the arrays which will hold the information for the
C     source being used in the current observation in order to pass
C     this information to the remainder of CALC.
C
C     Match the current star name against the names in the star catalog.
C     If no match, send a message and quit.
C  300 DO 310  NN = 1, NUMSTR
C           N = NN
C           IF  ( ( LNSTAR(1,N) .EQ. LSTRNM(1) )
C     1     .AND. ( LNSTAR(2,N) .EQ. LSTRNM(2) )
C     2     .AND. ( LNSTAR(3,N) .EQ. LSTRNM(3) )
C     3     .AND. ( LNSTAR(4,N) .EQ. LSTRNM(4) ) )  GO TO 320
C  310 CONTINUE
C      GO TO 600
C
C     Construct the arrays to hold the sine and cosine of the star
C     declination and right ascention.
  320 CONTINUE
      RIGHT_ASC   = RADEC(1,N)
      DECLINATION = RADEC(2,N)
C
C***********************************************************************
C  Check for proper motion computations
      IF (KSTRC.eq.3) THEN
C  Compute proper motion offsets in RA and Dec
C   Proper motion epoch
        JDepoch = P_motion(3,N) + 2400000.5
C   Truncate to integer year
c       Ieph = Xepoch
C   Day of year
c       If (JMOD(Ieph,4) .eq. 0) Then
c        Xdays = (Xepoch - Ieph) / 366.D0
c       Else
c        Xdays = (Xepoch - Ieph) / 365.D0
c       Endif
C   Julian day an Jan. 1
c        IM = 1
c        ID = 1
c       XJAN1 = JDY2K(Ieph,IM,ID)
C   Julian day at proper motion epoch
c       JDepoch = XJAN1 - 1 + Xdays
C   Difference: (Observation time) - (Proper motion epoch), yrs
        Difyrs = ((XJD+UTC) - JDepoch) / 365.25D0
C   Proper motion in RA (convert arc-seconds to time units in radians)
        PR_RA = P_motion(1,N) / DCOS(DECLINATION) * CONVDS * Difyrs
C   Proper motion in Dec (convert arc-seconds to radians)
        PR_DEC = P_motion(2,N) * CONVDS * Difyrs
C
        PM_RA  = P_motion(1,N) * CONVDS / 3.155760D+7
        PM_DEC = P_motion(2,N) * CONVDS / 3.155760D+7
C
C   Determine if proper motions should be added to the source vector. If so
C    the corrections should also be reversed for later use in STRP.
        IF (KSTRC.eq.3) THEN
          RIGHT_ASC   = RIGHT_ASC   + PR_RA
          DECLINATION = DECLINATION + PR_DEC
          PR_RA  = -PR_RA 
          PR_DEC = -PR_DEC
        ENDIF
C      WRITE(6,8) ' Xepoch, Xdays, XJAN1, JDepoch, Difyrs ', 
C    *              Xepoch, Xdays, XJAN1, JDepoch, Difyrs 
C
      ENDIF
C***********************************************************************
C
      SD = DSIN ( DECLINATION)
      CD = DCOS ( DECLINATION)
      SRA = DSIN ( RIGHT_ASC )
      CRA = DCOS ( RIGHT_ASC )
C
C     Compute the star position unit vector.
      STAR(1) = CD * CRA
      STAR(2) = CD * SRA
      STAR(3) = SD
C
      IF (KSTRC.eq.1 .or. KSTRC.eq.2) THEN
        PR_RA  = PRcorr(1,N) 
        PR_DEC = PRcorr(2,N) 
      ENDIF
C
C  Match source with distance, if parallax to be computed
      IF (KPLXC .eq. 1) Then
        Dparsec = D_psec(N)
        WRITE (6,8) ' Dparsec ', Dparsec
      ENDIF
C
C     Check KSTRD for debug output.
      IF ( KSTRD .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine STRG." )
C
      WRITE(6,8)' CD      ',CD
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CRA     ',CRA
      WRITE(6,8)' RADEC   ',RADEC
      WRITE(6,7)' NUMSTR  ',NUMSTR
    7 FORMAT(A,15I8/(9X,15I8))
      WRITE(6,8)' SD      ',SD
      WRITE(6,8)' SRA     ',SRA
C
      WRITE ( 6, 9200 )  STAR, LSTRNM, LNSTAR
 9200 FORMAT (1X, "STAR   = ", 3 ( D30.16, 10X ), /, 1X,
     1            "LSTRNM = ", 4A2, /, 1X,
     2            "LNSTAR = ", 10 ( 10 ( 4A2, 2X ), /, 1X ) )
C
      IF (KSTRC.eq.1 .or. KSTRC.eq.1) THEN
       WRITE(6,8) ' Xepoch, Xdays, XJAN1, JDepoch, Difyrs ', 
     *              Xepoch, Xdays, XJAN1, JDepoch, Difyrs 
       WRITE(6,8) ' PR_RA, PR_DEC ', PR_RA, PR_DEC 
      ENDIF
      IF (KPLXC .eq. 1) WRITE (6,8) ' Dparsec ', Dparsec
C
C   5.    NORMAL PROGRAM CONCLUSION.
C
  500 RETURN
C
C   6.    ABNORMAL PROGRAM TERMINATION.
C
  600 WRITE ( 6, 9300 )
 9300 FORMAT (" CALC has terminated in subroutine STRG.  ",
     1        ' The source identification was not successful. ' )
C
      CALL CKILL (6HSTRG  , 0, 0)
      END
C
C*****************************************************************************
      SUBROUTINE STRP (EPBASE,STAR,EARTH,SITEV,DSTRP,CDX,CRAX,SDX,SRAX)
      IMPLICIT None
C
C 5.    STRP
C
C 5.1   STRP PROGRAM SPECIFICATION
C
C 5.1.1 STRP is the partial derivatives section of the STAR module. It computes
C       the partial derivatives of the delay and rate with respect to the source
C       declination and right ascension.
C
C 5.2.1 CALLING SEQUENCE -
C           INPUT VARIABLES:
C             1. EPBASE(3,2)  -  THE J2000.0 GEOCENTRIC BASELINE VECTOR
C                                AND ITS CT TIME DERIVATIVE. (M, M/SEC)
C             2. STAR(3)      -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C             3. EARTH(3,3)   =  The ssbc position, velocity, and acceleration
C                                of the Earth. (m, m/s, m/s**2)
C           OUTPUT VARIABLES:
C             1. CDX -  THE COSINE OF THE DECLINATION OF THE SOURCE
C             2. CRAX - THE COSINE OF THE RIGHT ASCENSION OF THE SOURCE
C             3. SDX -  THE SINE OF THE DECLINATION OF THE SOURCE
C             4. SRAX - THE SINE OF THE RIGHT ASCENSION OF THE SOURCE
C             5. DSTRP(2,2)-THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
C                       RATE WITH RESPECT TO THE SOURCE RIGHT ASCENSION AND
C                       DECLINATION. (sec/rad, sec/sec-rad) THE FIRST INDEX
C                       RUNS OVER RA AND DEC, THE SECOND RUNS OVER DELAY AND
C                       DELAY RATE. 
C
C 5.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C           VARIABLES 'FROM':
C             1. VLIGHT  -  THE VELOCITY OF LIGHT IN VACUUM.  (M/SEC)
C             2. VLIGHT2 -  THE VELOCITY OF LIGHT IN VACUUM SQUARED.
C                           (M**2/SEC**2)
C 
      INCLUDE 'cmxsr.i'
C           VARIABLES 'FROM':
C             1. CD   -  THE COSINE OF THE DECLINATION OF THE STAR BEING
C                        USED IN THE CURRENT OBSERVATION.
C             2. CRA  -  THE COSINE OF THE RIGHT ASCENSION OF THE STAR BEING
C                        USED IN THE CURRENT OBSERVATION.
C             3. SD   -  THE SINE OF THE DECLINATION OF THE STAR BEING USED
C                        IN THE CURRENT OBSERVATION.
C             4. SRA  -  THE SINE OF THE RIGHT ASCENSION OF THE STAR BEING 
C                        USED IN THE CURRENT OBSERVATION.
C 
      INCLUDE 'ccon.i'               
C           VARIABLES 'FROM':
C             1.  KSTRC  -  THE STAR MODULE FLOW CONTROL FLAG.
C             2.  KSTRD  -  THE STAR MODULE DEBUG OUTPUT FLAG.
C
      Real*8 PR_RA, PR_DEC, PM_RA, PM_DEC
      Common /Pmotn/ PR_RA, PR_DEC, PM_RA, PM_DEC
C
C 5.2.3 PROGRAM SPECIFICATIONS -
C
       Real*8 DDEC(3), DRA(3), DSTRP(2,2), EPBASE(3,2), STAR(3), CDX,
     *        CRAX, SDX, SRAX, EARTH(3,3), SITEV(3,2), c1, c2, tt, 
     *        vg(3), bp(3), bv(3), PMCONT(2), DOTP
       Integer*4 I
C
C 5.2.4 DATA BASE ACCESS -
C           'PUT' VARIABLES:
C             1. DSTRP(2,2) - THE PARTIAL DERIVATIVES OF THE DELAY AND OF THE
C                             DELAY RATE WITH RESPECT TO THE SOURCE RIGHT
C                             ASCENSION AND DECLINATION. (sec/rad, sec/sec-rad)
C                             THE FIRST INDEX RUNS OVER RIGHT ASCENSION AND 
C                             DECLINATION, THE SECOND INDEX RUNS OVER THE DELAY
C                             AND THE DELAY RATE. 
C           ACCESS CODES:
C             1. 'STR PART' - THE DATA BASE ACCESS CODE FOR THE STAR MODULE
C                             PARTIAL DERIVATIVES ARRAY.
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVP 
C             CALLED SUBROUTINES: PUT4
C 
C 5.2.7 CONSTANTS USED - VLIGHT 
C
C 5.2.8 PROGRAM VARIABLES -
C             1. DDEC(3)  -  THE PARTIAL DERIVATIVE OF THE J2000.0 SOURCE UNIT
C                            VECTOR WITH RESPECT TO SOURCE DECLINATION. (1/RAD)
C             2. DRA(3)   -  THE PARTIAL DERIVATIVE OF THE J2000.0 SOURCE UNIT
C                            VECTOR WITH RESPECT TO THE SOURCE RIGHT ASCENSION.
C                            (1/RAD)
C             3. c1, c2, tt, vg(3), b(3) -  Dummy variables used in computation
C                            of the partials.
C
C 5.2.9 PROGRAMMER - 77.01.13 Dale Markham
C                    77.07.14 Peter Denatale
C                    88.11.10 Bruce Schupler
C                    88.01.07 Jim Ryan
C                    89.07.09 Jim Ryan Documentation simplied.
C                    89.10.05 Jim Ryan CPHYS common made an include file
C                    89.12.12 Jim Ryan UNIX-like database interface
C                             implimented.
C                    01:11:25 Jim Ryan Term 2 of Shapiro's model added
C                             to the partials.
C                    David Gordon 94.04.15 Converted to Implicit None
C                    David Gordon 95.05.02 DSTRP passed back to DRIVR for use
C                             in PLXP.
C                    B. Archinal  95.11.13 Max # sources set in cmxsr.i.
C                    David Gordon 98.09.08 Changed partials computation to 
C                             use CONSENSUS model (Step 10B). Makes no 
C                             significant difference. 
C
C     STRP Program Structure
C
C  Compute the partial derivatives of the J2000.0 source unit vector with
C   respect to the source declination and with respect to the source R.A.
      DDEC(1) = - SD * CRA
      DDEC(2) = - SD * SRA
      DDEC(3) = + CD
C
      DRA(1) = - CD * SRA
      DRA(2) = + CD * CRA
      DRA(3) = 0.D0
C******************************************************************************
C   Complete the calculation of the partial derivatives.
c     c1 = 1.d0/VLIGHT
c     c2 = c1**2
c     Do i=1,3
c       vg(i) =  EARTH(I,2)
c       bp(i) = -EPBASE(I,1)
c       bv(i) = -EPBASE(I,2)
c     Enddo
c     tt = 1.d0 - c1*Dotp(STAR,vg)
c
c     DSTRP(1,1)=-c1*Dotp(bp,DRA )*tt+c2*Dotp(STAR,bp)*Dotp(vg,DRA)
c     DSTRP(1,2)=-c1*Dotp(bv,DRA )*tt+c2*Dotp(STAR,bv)*Dotp(vg,DRA)
c     DSTRP(2,1)=-c1*Dotp(bp,DDEC)*tt+c2*Dotp(STAR,bp)*Dotp(vg,DDEC)
c     DSTRP(2,2)=-c1*Dotp(bv,DDEC)*tt+c2*Dotp(STAR,bv)*Dotp(vg,DDEC)
c     WRITE(6,'(" Old DSTRP: ",4D22.14)') DSTRP
C******************************************************************************
C
C   Complete the calculation of the partial derivatives.
      c1 = 1.d0/VLIGHT
      Do I=1,3
        vg(I) =  EARTH(I,2) + SITEV(I,2)
        bp(I) = -EPBASE(I,1)
        bv(I) = -EPBASE(I,2)
      Enddo
      tt = 1.d0 + c1*Dotp(STAR,vg)
C   Changed to Consensus model formula 
      DSTRP(1,1) = -Dotp(bp,DRA )/(Vlight*tt) +
     *              Dotp(STAR,bp)*Dotp(vg,DRA)/Vlight2
      DSTRP(1,2) = -Dotp(bv,DRA )/(Vlight*tt) +
     *              Dotp(STAR,bv)*Dotp(vg,DRA)/Vlight2
      DSTRP(2,1) = -Dotp(bp,DDEC)/(Vlight*tt) +
     *              Dotp(STAR,bp)*Dotp(vg,DDEC)/Vlight2
      DSTRP(2,2) = -Dotp(bv,DDEC)/(Vlight*tt) +
     *              Dotp(STAR,bv)*Dotp(vg,DDEC)/Vlight2
c     WRITE(6,'(" New DSTRP: ",4D22.14)') DSTRP
C
C   PUT the partials into the database.
      CALL PUT4 ('STR PART      ', DSTRP, 2, 2, 1 )
c     WRITE(6,'(" PUT DSTRP: ",4D22.14)') DSTRP
C
C   Copy some values from STRCM into dummy variables for use elsewhere
      CDX = CD
      CRAX = CRA
      SDX = SD
      SRAX = SRA
C
C   Check KSTRD for debug output.
      IF ( KSTRD .EQ. 0 )  GO TO 600
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine STRP." )
      WRITE(6,8)' CD     ',CD
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CRA    ',CRA
      WRITE(6,8)' DDEC   ',DDEC
      WRITE(6,8)' DRA    ',DRA
      WRITE(6,8)' DSTRP  ',DSTRP
      WRITE(6,8)' SD     ',SD
      WRITE(6,8)' SRA    ',SRA
      WRITE(6,8)' VLIGHT ',VLIGHT
      WRITE(6,8)' c1     ',c1
      WRITE(6,8)' c2     ',c2
      WRITE(6,8)' tt     ',tt
      WRITE(6,8)' vg     ',vg
      WRITE(6,8)' bp     ',bp
      WRITE(6,8)' bv     ',bv
C
      WRITE ( 6, 9200 )  EPBASE, STAR,CDX,SDX,CRAX,SRAX
 9200 FORMAT (1X, "EPBASE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "STAR   = ", 3 ( D30.16, 10X ),/,1X,
     2            "CDX    = ", D30.16,1X,
     3            "SDX    = ", D30.16,1X,
     4            "CRAX   = ", D30.16,/,1X,
     5            "SRAX   = ", D30.16)
C
C NORMAL PROGRAM CONCLUSION.
C
  600 RETURN
      END
C*****************************************************************************
      SUBROUTINE STRC (DSTRP)
      IMPLICIT None
C
C 5.    STRC
C
C 5.1   STRC PROGRAM SPECIFICATION
C
C 5.1.1 STRC is the contributions section of the STAR module. It computes
C       contributions to the delay and rate due to proper motions. Used 
C       only when KSTRC = 1 or 2.
C
C 5.2.1 CALLING SEQUENCE -
C           INPUT VARIABLES:
C             1. DSTRP(2,2)-THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
C                       RATE WITH RESPECT TO THE SOURCE RIGHT ASCENSION AND
C                       DECLINATION. (sec/rad, sec/sec-rad) THE FIRST INDEX
C                       RUNS OVER RA AND DEC, THE SECOND RUNS OVER DELAY AND
C                       DELAY RATE. 
C
C 5.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'ccon.i'               
C           VARIABLES 'FROM':
C             1.  KSTRC  -  THE STAR MODULE FLOW CONTROL FLAG.
C             2.  KSTRD  -  THE STAR MODULE DEBUG OUTPUT FLAG.
C
      Real*8 PR_RA, PR_DEC, PM_RA, PM_DEC
      Common /Pmotn/ PR_RA, PR_DEC, PM_RA, PM_DEC
C
C 5.2.3 PROGRAM SPECIFICATIONS -
       Real*8 DSTRP(2,2), PMCONT(2), PMTRACK
C
C 5.2.4 DATA BASE ACCESS -
C          'PUT' VARIABLES:
C            1. PMCONT(2) - If KSTRC = 1, these are the contributions to
C                           the delay and rate to correct for the effect 
C                           of proper motion; add to theoreticals.
C                           If KSTRC = 2, these are the contributions to
C                           return the delay and rate to their non-proper
C                           motion values; add to theoreticals. (sec, sec/sec).
C          ACCESS CODES:
C            1. 'PMOTNCON' - The data base access code for the proper motion
C                            contribution. 
C            2. 'PMOT2CON' - The data base access code for the proper motion
C                            removal contribution.
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVP 
C             CALLED SUBROUTINES: PUT4
C 
C 5.2.9 PROGRAMMER - 98.09.15 D. Gordon - subroutine created
C
C     STRC Program Structure
C
C  Check for proper motion computations
      IF (KSTRC.eq.1 .or. KSTRC.eq.2 .or. KSTRC.eq.3) THEN
        PMCONT(1) = PR_RA*DSTRP(1,1) + PR_DEC*DSTRP(2,1)
        PMCONT(2) = PR_RA*DSTRP(1,2) + PR_DEC*DSTRP(2,2)
        PMTRACK   = PM_RA*DSTRP(1,1) + PM_DEC*DSTRP(2,1)
        IF (KSTRC.eq.1) CALL PUT4 ('PMOTNCON      ',PMCONT,2,1,1)
        IF (KSTRC.eq.2) CALL PUT4 ('PMOT2CON      ',PMCONT,2,1,1)
        IF (KSTRC.eq.3) CALL PUT4 ('PMOT3CON      ',PMCONT,2,1,1)
        IF (KSTRC.eq.3) CALL PUT4 ('PMTRACK       ',PMTRACK,1,1,1)
      ELSE
       Return
      ENDIF
C
C** DEBUG *************************************************************
c      WRITE(6,8)'STRP: DSTRP  ', DSTRP
c      WRITE(6,8)'STRP: PMCONT ', PMCONT
C
C   Check KSTRD for debug output.
      IF (KSTRD .EQ. 10)  THEN       
       WRITE ( 6, 9)
    9  FORMAT (1X, "Debug output for subroutine STRC." )
    8  FORMAT(A,4D25.16/(7X,5D25.16))
       WRITE(6,8)' DSTRP  ',DSTRP
       WRITE(6,8)'STRP: PMCONT ', PMCONT
      ENDIF
C
C NORMAL PROGRAM CONCLUSION.
C
  600 RETURN
      END
C*****************************************************************************
       SUBROUTINE STRIN(Kerr)
       Implicit None
C
C    Subroutine to open external source catalog and get source a priori's.
C
C     98.03.19 D. Gordon/GSFC - Subroutine created
C     98.09.11 D. Gordon/GSFC - Expanded to read a non-blokq.dat file which
C                               may optionally contain proper motion rates,
C                               proper motion epochs, and source distances.
C
      INCLUDE 'cmxsr.i'
      INCLUDE 'inputs.i'
      INCLUDE 'ccon.i'             
C            VARIABLES 'FROM':
C             1. KSTRC  -  THE STAR MODULE FLOW CONTROL FLAG.
C                          0 => Do not add proper motion constants to data
C                               base when in external file mode  (Lcode 
C                               'PRMOTION'). Do not compute any proper
C                               motion contributions. Remove any previous
C                               proper motion contribution Lcodes.
C                          1 => Add proper motion constants ('PRMOTION') if
C                               in external file input mode. Compute proper
C                               motion contributions if proper motion 
C                               constants are available and insert in data
C                               base under Lcode 'PMOTNCON'. Do NOT add 
C                               these contributions to the theoretical.
C                               [They can be ADDED in SOLVE to get proper
C                               motion corrected delays and rates.]
C                          2 => Add proper motion constants ('PRMOTION') if
C                               in external file input mode. Also, ADD the
C                               proper motions to source vector and use the
C                               proper motion corrected source vector 
C                               throughout all computations. Compute a 
C                               contribution (ADD to theoretical) that will
C                               (approximately) return the delays and rates
C                               to their non-proper motion corrected values, 
C                               and put in Lcode 'PMOT2CON'. For cases where
C                               there is a large accumulated proper motion
C                               (greater than ~1 arcsec). Intended for 
C                               correlator useage only. USE WITH CAUTION!! 
C             2. KSTRD  -  THE STAR MODULE DEBUG OUTPUT FLAG.
C             3. KPLXC  -  THE PARALLAX MODULE FLOW CONTROL FLAG.
C                          0 => Do not insert source distances into data 
C                               base when in external file input mode, even 
C                               if distances given in external source file. 
C                          1 => Insert source distances into data base in 
C                               external file input mode, even if all zeros,
C                               using Lcode 'DISTPSEC'. 
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      ExTERNAL CMATHB
C
      Integer*2 Getunit, Kerr(3), Lstref(10,Max_arc_src)
      Integer*4 I, II, I2, Jstar(Max_arc_src), Iunit, ios,
     *          Iquit, Index, K, RA_hr, RA_min, Dec_deg, Dec_min,
     *          Itype
      Real*8    RA, Dec, RA_sec, Dec_sec, Dsign, PM_RA, PM_Dec, PM_ep,
     *       Psecs 
      Character*1 Csign, dummy(3)
      Character*20 Cstref(Max_arc_src), Staref
      Equivalence ( Lstref, Cstref )
C
      Character*120 Inbuf
      Character*8  Dbstars(Max_arc_src), Star
      Equivalence (LNSTAR(1,1), Dbstars(1))
C
C   Initialize star counter
       Do I = 1, Max_arc_src
         Jstar(i) = 0
       Enddo
C
        Itype = 1
        Dpsec = 0
        Pmotion = 0
C
C  Open the Star catalog data file
       Iunit = getunit()
c       Open (Unit=Iunit, File=Ex_stars, Status='old', Readonly,
c     *       Err=240, Iostat=ios)
C
      If (Index(Ex_stars,'blokq') .gt. 0) Then 
C  Blokq.dat file, find the star catalog
        Itype = 2
        I2 = 0
  50   Continue
       Read(iunit,'(A120)') Inbuf
       If (Inbuf(1:2) .eq. '//') Then
        I2 = I2 + 1
        If (I2 .eq. 2) Go to 100
       Endif
       Go to 50
 100   Continue
      Endif
C
 130   Continue
       Read(iunit,'(A120)',end=200) Inbuf
C   Skip comments and illegal lines
       If (Inbuf(1:2) .eq. '//')   Go to 200 
       If (Inbuf(1:2) .eq. '$$'  ) Go to 130
       If (Inbuf(1:4) .ne. '    ') Go to 130
c      If (Inbuf(13:13) .ne. ' ' ) Go to 130
C
       IF (Itype .eq. 2) THEN
C  blokq.dat file
        Read(Inbuf,1010,err=200,end=200) Star, RA_hr, RA_min, RA_sec, 
     *       Csign, Dec_deg, Dec_min, Dec_sec, Staref 
 1010   Format(4X,A8,2X,2(I2,1X),F13.10,1X,A1,2(I2,1X),F13.10,3X,A20)
C
       ELSE 
C  Non-blokq.dat file
        Read(Inbuf,1015,err=200,end=200) Star, RA_hr, RA_min, RA_sec, 
     *       Csign, Dec_deg, Dec_min, Dec_sec, PM_RA, PM_Dec, PM_ep,
     *       Psecs, Staref 
 1015   Format(4X,A8,2X,2(I2,1X),F13.10,1X,A1,2(I2,1X),F13.10,3X,
     *         F8.7,2X,F8.7,2X,F8.3,2X,F8.1,3X,A20)
       ENDIF
C
C See if this station is in the database list
       Do I = 1, NUMSTR
         If (Star .eq. Dbstars(I)) Then
C          print *, 'Star matched: ', Dbstars(I)
           II = I
           Jstar(II) = II
C
           Dsign = 0.D0
           If(Csign .eq. '-') Dsign = -1.D0
           If(Csign .eq. '+') Dsign =  1.D0
           If(Csign .eq. ' ') Dsign =  1.D0
           If(Dsign .eq. 0.D0) Then
            print *, '!!! No plus/minus sign for Declination !!! '
           Endif
C
C  [We use 2.D0*PI below instead of TWOPI for consistency with 
C   Dbedit/Apriori/Skeleton programs. Difference is: TWOPI - 2.D0*PI = +1.D-16.
C   -Also, don't change or re-order these equations, or they may no longer
C    give identical results to Dbedit/Apriori/Skeleton.] 
           RADEC(1,II) = (RA_hr/24.D0 + RA_min/1440.D0 + RA_sec/8.64D4) 
     .           * 2.D0*PI
           RADEC(2,II) = ( Dec_deg + (Dec_min + Dec_sec/60.D0)/60.D0 )
     .           * Dsign * 2.D0*PI / 360.D0
C
           Cstref(II) = Staref
C           print *,'RA/Dec/ref ', RADEC(1,II), RADEC(2,II), Cstref(II)
C
           IF (Itype .eq. 1) THEN
            P_motion(1,II) = PM_RA
            P_motion(2,II) = PM_Dec
            P_motion(3,II) = PM_ep
            D_psec(II)     = Psecs
            If((P_motion(3,II).ge.1900.D0) .AND. 
     *         (DABS(P_motion(1,II)) .gt. 1.D-12  .or. 
     *          DABS(P_motion(2,II)) .gt. 1.D-12) )  
     *                Pmotion = Pmotion + 1
            If(D_psec(II).ge.1.D0) Dpsec = Dpsec + 1
           ENDIF
C
         Endif
       Enddo
C
       Go to 130
C
 200   Continue
       print *,' STRIN: Pmotion, Dpsec = ', Pmotion, Dpsec
C
       Close(Iunit)
C
C   Verify that we have a priori's for all stars
        Iquit = 0
C
      DO I = 1, NUMSTR
        If (Jstar(i) .eq. 0) Then
            Write(6,'(" STRIN: No match for star #",i3)') I
            Iquit = Iquit + 1
        Endif 
      ENDDO
C
       If (Iquit.eq.0) Then
        Kerr(3) = 0
       Else
        Call CKill(6HSTRIN ,0,0)
       Endif
C
C  Now we must replace the star a priori's in the data base
      CALL PUTR('STAR2000      ', RADEC , 2, NUMSTR, 1)
      CALL PUTA('STAR REF      ', LSTREF,10, NUMSTR, 1)
      IF (KSTRC.eq.1 .or. KSTRC.eq.2)
     *  CALL PUTR('PRMOTION      ', P_motion, 3, NUMSTR, 1)
      IF (KPLXC.eq.1)
     *  CALL PUTR('DISTPSEC      ', D_psec, NUMSTR, 1, 1)
C
       Go to 270
C
C   Error on Read
 180  Continue
      print *, 'STRIN: Error on read of star catalog '
        Call CKill(6HSTRIN ,0,0)
C
C   Error on OPEN
 240  Continue
      print *, 'STRIN: Error on OPEN of star catalog '
        Call CKill(6HSTRIN ,0,0)
C
 270  Continue
      Return
      End


