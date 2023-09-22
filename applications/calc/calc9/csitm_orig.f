      SUBROUTINE SITA
      IMPLICIT None
C
C 1.    SITA
C
C 1.1   SITA PROGRAM SPECIFICATION
C
C 1.1.1 SITA ADDs entries to the Table of Contents for the
C       Site Module text message and the partial derivatives array.
C
C 1.2   SITA PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - NONE
C
C 1.2.2 COMMON BLOCKS USED - 
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_sites - T/F logical flag telling whether to use
C                               external site a priori input.
      INCLUDE 'cmxst.i'
C            Variables from:
C              1. NUMSIT   - Number of sites in the data base.
C              2. Max_Stat - Maximun number of stations allowed.
C
      INCLUDE 'cuser.i'
C       Variables from:
C         1. Calc_user  - Calc user type. 'A' for Mark III/SOLVE analysis.
C                         'C' for VLBI correlator.
C
C 1.2.3 PROGRAM SPECIFICATIONS - NONE 
C 
C 1.2.4 DATA BASE ACCESS -
C            ACCESS CODES ADDED:
C              1.  'SIT MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 SITE MODULE TEXT MESSAGE. 
C              2.  'SIT PART'  -  THE DATA BASE ACCESS CODE FOR THE SITE
C                                 MODULE TEXT MESSAGE.
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT -  None
C 
C 1.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: TOCUP 
C           CALLED SUBROUTINES: ADDA, ADDR, ADDI
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - NONE
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/13/77
C                    SAVITA GOEL    06/03/87 (CDS FOR A900)
C                    89.07.20 Jim Ryan Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                        implimented.
C                    David Gordon 94.04.16 Converted to Implicit None.
C                    David Gordon 94.06.08 Corrected format statements all
C                         subroutines, where single and double quotes reversed.
C                    David Gordon 98.03.17 Mods and ADD's for source a priori's
C                         and ocean loading in the case of external site inputs
C                    David Gordon 98.11.05 Mods/ADD for 'SITEXYZS', velocity
C                         corrected site coordinates. For correlator use only.
C                    David Gordon 99.10.27 Extraneous printout removed.
C                    David Gordon 2001.01.05 Code to add Lcodes 'OCE STAT'
C                         and 'TECTPLNM'.
C
C     SITA Program Structure
C
C     ADD for module text message.
      CALL ADDA (1,'SIT MESS','Site Module Message Definition  ',
     1     40, 1, 1 )
C
C     ADD for Site module partials.
      CALL ADDR (2,'SIT PART','Site partial derivative def.    ',
     1     3, 2, 2 )
C
C   Do adds to replace the site a priori's in the data base in the case of 
C    external file inputs
      If (Input_sites) Then
C**     print *, 'ADDs for external site catalog'
       CALL ADDR(1,'SITERECS','Site cartesian coords (m).      ',
     *          3,Numsit,1)
       CALL ADDR(1,'SITEZENS','Site zenith path delays (nsec). ',
     *          Numsit,1,1)
       CALL ADDA(1,'TECTPLNM','4-char tectonic plate names.    ',
     *          2,Numsit,1)
       CALL ADDI(1,'AXISTYPS','Axis type (1-eq,2-xy,3-azel,4,5)',
     *          Numsit,1,1)
       CALL ADDR(1,'AXISOFFS','Axis offsets (m).               ',
     *          Numsit,1,1)
      Endif
C
C   Do adds to replace the ocean loading coefficients in the data base in 
C    the case of external file inputs
      If (Input_ocean) Then
C**     print *, 'ADDs for external ocean loading catalog'
       CALL ADDR(1,'SITOCAMP','Vert ocean loading ampltudes (m)',
     *          11,Numsit,1)
       CALL ADDR(1,'SITOCPHS','Vert ocean loading phases (rad).',
     *          11,Numsit,1)
       CALL ADDR(1,'SITHOCAM','Horz ocean loading ampltudes (m)',
     *          11,2,Numsit)
       CALL ADDR(1,'SITHOCPH','Horz ocean loading phases (rad).',
     *          11,2,Numsit)
       CALL ADDA(1,'OCE STAT','Ocean loading station status.   ',
     *           2,Numsit,1)
      Endif
C
C Do add for modified/unmodified site positions for correlator usage. 
C  Correlator users must capture this access code for downstream analysis
C  (in AIPS, HOPS, etc.) (in place of 'SITERECS') if they use the 'SITERECV'
C  access code for site input. The number of stations will probably not be
C  known at this time. To avoid unnecessary complications, we allow for the
C  maximum number of stations, as defined in 'cmxst.i'.
       If (calc_user. eq. 'C') Then
        CALL ADDR(1,'SITEXYZS','Site Coords To Date (meters)    ',
     *          3, Max_Stat, 1)
       Endif
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE SITI
      IMPLICIT None
C
C 3.    SITI
C
C 3.1   SITI PROGRAM SPECIFICATION
C
C 3.1.1 SITI is the Site Module input and initialization section.
C
C 3.1.2 RESTRICTIONS - NONE
C
C 3.1.3 REFERENCES - SMART, W.M., 'TEXTBOOK ON SPHERICAL ASTRONOMY',
C                    1965, P. 195-198
C                    MARKHAM'S X-DOCUMENT
C
C 3.2   SITI PROGRAM INTERFACE
C
C 3.2.1 CALLING SEQUENCE - NONE
C
C 3.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C            VARIABLES 'FROM':
C              1. EFLAT  -  THE FLATTENNING OF THE ELLIPSOID APPROXIMATING
C                           THE SHAPE OF THE EARTH.  (UNITLESS)
C              2. REARTH -  THE EQUATORIAL RADIUS OF THE EARTH. (M)
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C
      INCLUDE 'cmxst.i'
C
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C              1. KSITC  -  THE SITE MODULE FLOW CONTROL FLAG.
C              2. KSITD  -  THE SITE MODULE DEBUG OUTPUT FLAG.
C
      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_sites - T/F logical flag telling whether to use
C                               external site a priori input
C              2. Input_ocean - T/F logical flag telling whether to
C                               use external ocean loading a priori input
C              3. Ex_sites    - File name for sites external file input.
C                               If 'NONE' or blank, external site input
C                               will not be done.
C              4. Ex_ocean    - File name for ocean loading external file.
C                               If 'NONE' or blank, external ocean loading 
C                               input will not be done.
C
      INCLUDE 'cuser.i'
C       Variables from:
C         1. Calc_user  - Calc user type. 'A' for Mark III/SOLVE analysis.
C                         'C' for VLBI correlator.
C
C 3.2.3 PROGRAM SPECIFICATIONS -
C
      Integer*2 KERR(10), LSITM(40), NDO(3), NDI(3), NN, numsit_local,
     *          Intrvl(5,2), KERX(2)
      Integer*4 N, I, J, K, L, ik, kj, imdoy(12), Intmov
      REAL*8 XLAT_DUMMY,XLON_DUMMY, RY(3,3), RZ(3,3), TCROT_DUMMY(3,3),
     *       RGY(3,3), GLAT_DUMMY, RTROT_DUMMY(3,3), Sitxyzv(7,Max_stat)
      Real*8 xlatlonht(3),pie,a,fl,xyz(3), Xdoy1, Xepoch, X_frac
      CHARACTER*40 C_LSITM(2)
      EQUIVALENCE (LSITM,C_LSITM)
      CHARACTER*1 ITEST, idum(3)
C     Logical*4 Input_sites, Input_ocean 
C
      DATA C_LSITM /
     .'SITE MODULE - Last modified 98.10.26, D.',
     .' Gordon, GSFC                           '/
C
      Data Imdoy /0,31,59,90,120,151,181,212,243,273,304,334/
C
C 3.2.4 DATA BASE ACCESS -
C
C    'GET' VARIABLES:
C      1. KTYPE(Max_Stat)       -  THE ANTENNA AXIS TYPES.  (UNITLESS)
C      2. LNSITE(4,Max_Stat)    -  THE EIGHT ALPHAMERIC CHARACTER SITE NAMES
C                                  OF THE SITES IN THE SITE CATALOG.
C      3. NUMSIT                -  THE NUMBER OF SITES IN THE SITE CATALOG.
C      4. SITAXO(Max_Stat)      -  THE SITE ANTENNA AXIS OFFSETS. (M)
C      5. SITOAM(11,Max_Stat)   -  THE SITE VERTICAL OCEAN LOADING
C                                  AMPLITUDES. (M)
C      6. SITOPH(11,Max_Stat)   -  THE SITE VERTICAL OCEAN LOADING PHASES.
C                                  (RAD)
C      7. SITHOA(11,2,Max_Stat) -  THE SITE HORIZONTAL OCEAN LOADING
C                                  AMPLITUDES. (M)
C      8. SITHOP(11,2,Max_Stat) -  THE SITE HORIZONTAL OCEAN LOADING PHASES.
C                                  (RAD)
C      9. SITXYZ(3,Max_Stat)    -  THE SITE CRUST FIXED X, Y, & Z
C                                  COORDINATES. (M, M, M )
C     10. SITZEN(Max_Stat)      -  THE ZENITH ELECTRICAL PATH LENGTH
C                                  AT EACH OBSERVATION SITE. (SEC)
C     11. Sitxyzv(7,Max_Stat)   -  Optional correlator input via access code
C                                  'SITERECV' in place of SITXYZ (via 
C                                  'SITERECS'). First index runs over site crust
C                                  fixed coordinates (X, Y, Z), epoch for
C                                  those coordinates (4-digit and fractional
C                                  year, such as 1998.5), and site velocities
C                                  (X-velocity, Y-velocity, Z-velocity). 
C                                  Second index runs over the sites. For
C                                  geocenter station make sure X-velocity =
C                                  Y-velocity = Z-velocity = 0.D0.
C                                  (m, m, m, yrs, m/sec, m/sec, m/sec) 
C
C    'PUT' VARIABLES:
C      1. LSITM(40)  -  THE SITE MODULE TEXT MESSAGE.
C      2. SITXYZ(3,Max_Stat) - Correlator output of site crust fixed 
C                       coordinates, modified if access code 'SITERECV' 
C                       supplied. 
C
C    ACCESS CODES:
C      1. 'SIT MESS'  -  THE DATA BASE ACCESS CODE FOR THE SITE MODULE TEXT
C                        MESSAGE.
C      2. 'AXISTYPS'  -  THE DATA BASE ACCESS CODE FOR THE ARRAY OF SITE 
C                        ANTENNA TYPES.
C      3. 'SITNAMES'  -  THE DATA BASE ACCESS CODE FOR THE ARRAY OF SITE NAMES.
C      4. '# SITES '  -  THE DATA BASE ACCESS CODE FOR THE NUMBER OF 
C                        OBSERVATION SITES.
C      5. 'AXISOFFS'  -  THE DATA BASE ACCESS CODE FOR THE ARRAY OF SITE
C                        ANTENNA AXIS OFFSETS. 
C      6. 'SITERECS'  -  THE DATA BASE ACCESS CODE FOR THE  ARRAY OF SITE
C                        X,Y,Z COORDINATES.
C      7. 'SITEZENS'  -  THE DATA BASE ACCESS CODE FOR THE SITE ZENITH 
C                        ELECTRICAL PATH LENGTH. 
C      8. 'SITOCAMP'  -  THE DATA BASE ACCESS CODE FOR THE VERTICAL OCEAN
C                        LOADING AMPLITUDES.
C      9. 'SITOCPHS'  -  THE DATA BASE ACCESS CODE FOR THE VERTICAL OCEAN
C                        LOADING PHASES.
C     10. 'SITHOCAM'  -  THE DATA BASE ACCESS CODE FOR THE HORIZONTAL OCEAN
C                        LOADING AMPLITUDES.
C     11. 'SITHOCPH'  -  THE DATA BASE ACCESS CODE FOR THE HORIZONTAL OCEAN
C                        LOADING PHASES.
C
C 3.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: INITL
C           CALLED SUBROUTINES: DCOS, DSIN, DSQRT, GETA, GETI,
C                    GET4, KILL, MMUL2, PUTA, ROTATE, bkplh 
C
C 3.2.7 CONSTANTS USED - EFLAT, REARTH
C
C 3.2.8 PROGRAM VARIABLES -
C           1. KERR(10) - THE DATA BASE ERROR RETURN FLAGS.
C           2. NDO(3)   - THE DATA BASE RETURN ARRAY INDICES.
C           3. RY(3,3)  - THE ROTATION MATRIX WHICH PERFORMS A COORDINATE 
C                         SYSTEM ROTATION ABOUT THE TOPOCENTRIC Y-AXIS (EAST)
C                         THROUGH AN ANGLE EQUAL TO THE GEODETIC LATITUDE OF
C                         THE CURRENT SITE. (UNITLESS)
C           4. RZ(3,3) -  THE ROTATION MATRIX WHICH PERFORMS A COORDINATE
C                         SYSTEM ROTATION ABOUT THE TOPOCENTRIC Z-AXIS (NORTH)
C                         THROUGH AN ANGLE EQUAL TO THE NEGATIVE EAST LONGITUDE
C                         OF THE CURRENT SITE. (UNITLESS)
C
C 3.2.9 PROGRAMMER - DALE MARKHAM    01/13/77
C                    PETER DENATALE  07/13/77
C                    BRUCE SCHUPLER  03/08/78
C                    CHOPO MA        08/06/81
C                    HAROLD M. SCHUH 10/08/83
C                    SAVITA GOEL 06/03/87 (CDS FOR A900)
C                    LOTHAR MOHLMANN 03/23/89
C                    89.07.20 Jim Ryan Documentation simplified.
C                    Jim Ryan 89:10:05 CPHYS common made an include file.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                      implimented.
C                    MSW 93.03.30 SITCM common and maximum number of station 
C                                 variable put into newly created include file
C                                 "cmxst.i".
C                    David Gordon 93.10.12 Call to subroutine bkplh added for
C                                 geodetic latitude, longitude, and height;
C                                 Fixed debug printout errors.
C                    David Gordon 94.02.04 HEIGHT(Max_Stat) addded and put into
C                                 cmxst.i; heights above geoid (meters).
C                    David Gordon 94.04.16 Converted to Implicit None.
C                    David Gordon 98.01.22 Modified for station at or near the
C                         geocenter, set topocentric stuff to -999 or so.
C                    David Gordon 98.03.13 Mods for external site input via
C                         an Ascii file.
C                    David Gordon 98.03.17 Mods for source a priori's and ocean
C                         loading coefficients in the case of external file 
C                         inputs.
C                    David Gordon 98.06.26 Add code to compute the 'radial-
C                         transverse rotation matrices at each site, for use 
C                         in the Earth tide module.
C                    David Gordon 98.10.26 Add code for optional correlator
C                         usage of site velocities. Need input Lcodes 
C                         'INTERVAL' and 'SITERECV'. New output Lcode is
C                         'SITERECS' - modified or not.
C                    David Gordon 98.11.05 Put in code to use proposed new 
C                         Lcode 'INTRVAL4', the start/stop interval
C                         (yr/month/day/hr/min) using a 4-digit year. If
C                         not there will use 'INTERVAL' (2-digit year).
C                         Correlator output site position Lcode changed to
C                         'SITEXYZS', dimensions (3,Max_stat). 10 year limit
C                         set on velocity interpolation. 
C
C
C 3.3   SITI PROGRAM STRUCTURE
C
C   Initialize site positions for correlator usage
       If (calc_user. eq. 'C') Then
         do ik=1,Max_Stat
          do kj=1,3
           SITXYZ(kj,ik) = 0.D0
          enddo
         enddo
       Endif
C
C     PUT the module text message.
      CALL PUTA ('SIT MESS      ', LSITM, 40, 1, 1 )
C
C     Use GETA, GETI, & GET4 to obtain the site information from the
C       database. All accessed site info goes into COMMON/SITCM/ in cmxst.i.
C
      CALL GETI ('# SITES       ',NUMSIT, 1, 1, 1, NDO, KERR(1) )
      NUMSIT_LOCAL = NUMSIT
      CALL GETA ('SITNAMES      ',LNSITE, 4, NUMSIT_LOCAL, 1, NDO,
     *            KERR(2) )
C
C-----------------------------------------------------------------------------
C  Data base input or external file input for sites? 
      IF (Input_sites) THEN                    ! Get site info
C
       CALL SITBLK(Kerr)
C
      ELSE                                     ! Get site info
C
       CALL GETI ('AXISTYPS      ',KTYPE, NUMSIT_LOCAL, 1, 1, NDO,
     *            KERR(3) )
       CALL GET4 ('AXISOFFS      ',SITAXO,NUMSIT_LOCAL, 1, 1, NDO,
     *            KERR(4) )
       CALL GET4 ('SITEZENS      ',SITZEN, NUMSIT_LOCAL, 1, 1, NDO,
     *            KERR(6) )
C
C***********************************************
C  Expanded for optional correlator usage of site velocities 
       If (calc_user. eq. 'C') Then
C         'SITERECV' should contain X, Y, Z, epoch, X-dot, Y-dot, Z-dot
C           for each station, all Real*8. Epoch must be a 4-digit year and 
C           fraction  thereof (1998.0, 2001.5, etc.) If Epoch is zero for a
C           site, then velocity corrections will be turned off for that site.
C           If X=Y=Z=0.D0 (geocenter), then velocity corrections will 
C           automatically be turned off.
        CALL GET4 ('SITERECV      ',Sitxyzv, 7, NUMSIT_LOCAL, 1, NDO,
     *            KERX(1) )
C
C !  Apply site velocities from new L-code 'SITERECV'
        If (KERX(1) .eq. 0)  Then
C           Look for 4-digit year interval
          CALL GETI ('INTRVAL4      ',Intrvl, 5, 2, 1, NDO, KERX(2) )
C           If no 4-digit year interval, get 2-digit year interval
         If (KERX(2).ne.0) then
          CALL GETI ('INTERVAL      ',Intrvl, 5, 2, 1, NDO, KERX(2) )
C            Convert 2-digit year to 4-digit year
          If (Intrvl(1,1) .ge. 70 .and. Intrvl(1,1) .le. 99) 
     *        Intrvl(1,1) = Intrvl(1,1)+1900
          If (Intrvl(1,1) .ge.  0 .and. Intrvl(1,1) .le. 69) 
     *        Intrvl(1,1) = Intrvl(1,1)+2000
         Endif
C  Convert start time to year and fraction, not worrying about leap years:
           xdoy1 = imdoy(Intrvl(2,1)) + Intrvl(3,1) + Intrvl(4,1)/24.d0 
     *             + Intrvl(5,1)/1440.d0 
           Xepoch = Intrvl(1,1) + xdoy1/365.D0
             Do ik = 1, Numsit_local
              X_frac = Xepoch - Sitxyzv(4,ik)
C              Turn off corrections if no position epoch or at the geocenter
                If (Sitxyzv(4,ik) .eq. 0.D0) X_frac = 0.D0
                If ( (DABS(Sitxyzv(1,ik)) .le. 1.D-6) .and.
     *               (DABS(Sitxyzv(2,ik)) .le. 1.D-6) .and.
     *               (DABS(Sitxyzv(3,ik)) .le. 1.D-6) )  X_frac = 0.D0
C               Make sure interpolation is over no more than 10 years!!
                 If (DABS(X_frac) .gt. 10.D0) Then
                  Write(6,147) ik, Xepoch, Sitxyzv(4,ik)    
 147              Format(' Problem in SITI for Site #',I2,
     *             ', Data epoch = ',F10.2, ', Site epoch = ',F10.2,/,
     *             ' Maximum difference allowed is 10 years!! Check',
     *             ' input codes SITERECV and INTRVAL4/INTERVAL. ') 
                  CALL CKILL (6HSITI  , 0, 0 )
                  STOP
                 Endif
C 
               Do kj = 1, 3
C                   Add motion rounded to nearest 0.1 mm
                 Intmov = Sitxyzv(kj+4,ik)*X_frac*1.D4 + .49D0
                SITXYZ(kj,ik) = Sitxyzv(kj,ik) + Intmov/1.D4
               Enddo
             Enddo 
C !  No site velocities, use old L-code
        Else
          CALL GET4 ('SITERECS      ',SITXYZ, 3, NUMSIT_LOCAL, 1, NDO,
     *            KERR(5) )
        Endif
C
C   Write out new/old site coordinates for Correlator capture
        CALL PUTR('SITEXYZS      ', SITXYZ, 3, Max_Stat, 1)
C
       Else
        CALL GET4 ('SITERECS      ',SITXYZ, 3, NUMSIT_LOCAL, 1, NDO,
     *            KERR(5) )
       Endif
C***********************************************
C
      ENDIF                                    ! Get site info
C
C  Check for a geocenter station and set flag if so
        Zero_site = 0
      Do I = 1, Numsit
        If ( (DABS(SITXYZ(1,I)) .le. 1.D-6) .and.
     *       (DABS(SITXYZ(2,I)) .le. 1.D-6) .and.
     *       (DABS(SITXYZ(3,I)) .le. 1.D-6) )  Then
          If (Zero_site .eq. 0) Then
           Zero_site = I
           Write(6,'("SITI: Goecenter site = site # ",I3)') Zero_site
          Else      ! More than one geocenter site! Not allowed!
           Write(6,'("SITBLK: More than 1 geocenter site! Quitting!")')
           CALL CKILL (6HSITI  , 0, 0 )
          Endif
        Endif
      Enddo
C
C-----------------------------------------------------------------------------
C
C  Data base input or external file input for ocean loading?
      IF (Input_OCEAN) THEN        ! Database/external file ocean loading?
C
       CALL OCNIN(Kerr)
C
      ELSE                         ! Database/external file ocean loading?
C
       CALL GET4 ('SITOCAMP      ',SITOAM, 11, NUMSIT_LOCAL, 1,NDI,
     *            KERR(7) )
       CALL GET4 ('SITOCPHS      ',SITOPH, 11, NUMSIT_LOCAL, 1,NDI,
     *            KERR(8) )
       CALL GET4 ('SITHOCAM      ',SITHOA, 11,2, NUMSIT_LOCAL, NDI,
     *            KERR(9) )
       CALL GET4 ('SITHOCPH      ',SITHOP, 11,2, NUMSIT_LOCAL, NDI,
     *            KERR(10) )
       IF(KERR(9).NE.0  .OR. KERR(10).NE.0) THEN
         DO I = 1,11
           DO J = 1,2
             DO K = 1,NUMSIT_LOCAL
               SITHOA(I,J,K) = 0.D0
               SITHOP(I,J,K) = 0.D0
             ENDDO
           ENDDO
         ENDDO
        KERR(9)  = 0
        KERR(10) = 0
C
        If(KOCEC.ne.3) Then       !See if the user wants to go on.
          WRITE(6,'(///,
     .    "WARNING: This database does not contain horizontal ocean loa"
     .    ,"ding amplitudes and",/,
     .    "phases.  Arrays for all sites zeroed out.",//,
     .    "Continue (y/(n)) ?",$)')
          ITEST = 'N'
          READ(5,'(A)') ITEST
          IF(ITEST.ne.'Y' .and. ITEST.ne.'y') THEN
            KERR(9)  = 1
            KERR(10) = 1
          ENDIF
C
        Else         !If the ocean loading module control flag is 3 proceed!
          Write(6,'(
     .    " Proceeding with null horizontal ocean loading catalog!")')
        Endif
       ENDIF
C
      ENDIF                        ! Database/external file ocean loading?
C-----------------------------------------------------------------------------
C
      IF (.Not. Input_sites) THEN 
C
C     If only one site zenith path delay, copy for all stations.
       IF( NDO(1) .NE. NUMSIT ) THEN
         DO 210 N = 2,NUMSIT
 210       SITZEN(N) = SITZEN(1)
       ENDIF
C
C     Check for database interface errors. If an error is found, KILL.
       DO N = 1,5
           NN = N
           IF ( KERR(NN) .NE. 0 ) CALL CKILL (6HSITI  , NN, KERR(NN) )
       ENDDO
C
C     There may be only one site zenith atmosphere delay.
       IF( KERR(6) .NE. 0 .AND. KERR(6) .NE. 2 )
     .     CALL CKILL (6HSITI  , 6, KERR(6) )
C
      ENDIF
C
C     Check for database interface errors _ ocean loading.
C
      DO 302 N = 7,10
        NN = N
        IF( KERR(N) .EQ. 0) GO TO 302
          CALL CKILL (6HSITI  ,NN, KERR(NN))
 302  CONTINUE
C
C     Calculate the neccesary site geometry.
C      Mod added 98JAN22: Dummy out topocentric type variables for station at
C      or near the geocenter, for correlator usage. 
C
C     Loop once for each station in the site catalog.
      DO 490  N = 1,NUMSIT
C
C       Compute the site spherical radii.
        CFRAD(N) = DSQRT ( SITXYZ(1,N)**2  +  SITXYZ(2,N)**2  +
     1                     SITXYZ(3,N)**2  )
C
C   Check for geocenter
         If (Zero_site .eq. N) Go to 491
C
C   Compute geocentric latitudes
         GLAT(N) = DASIN( SITXYZ(3,N) / CFRAD(N) )
C
C   93OCT12. Call subroutine bkplh to compute 
C    geodetic latitude, geodetic longitude, and height. DG 
         pie = pi
         fl = eflat
         a = rearth
         do i=1,3
          xyz(i)=SITXYZ(i,N)
         enddo
        call bkplh(xyz,xlatlonht,pie,a,fl) 
C keep longitudes between -PI and +PI
         if (xlatlonht(2) .gt. pi)
     .       xlatlonht(2) = xlatlonht(2) - 2.D0*pi
         XLAT(N)  =  xlatlonht(1) 
         XLON(N)  =  xlatlonht(2) 
         Height(N) = xlatlonht(3)     ! height in meters
C
C       Compute the site normal unit vectors.
        SNRM(1,N) = DCOS ( XLAT(N) ) * DCOS ( XLON(N) )
        SNRM(2,N) = DCOS ( XLAT(N) ) * DSIN ( XLON(N) )
        SNRM(3,N) = DSIN ( XLAT(N) )
C
C       Compute the partial derivatives of the crust fixed site
C       coordinates with respect to the East longitudes.
        PLON(1,N) = - SITXYZ(2,N)
        PLON(2,N) =   SITXYZ(1,N)
        PLON(3,N) =   0.D0
C
C       Compute the partial derivatives of the crust fixed site
C       coordinates with respect to the geodetic latitudes.
C       (NOTE: The following equations are actually for the geocentric partial
C       derivatives, however, these partials are sufficiently close to the
C       geodetic partials for the purposes of CALC use.)
        PLAT(1,N) = - SITXYZ(3,N) * DCOS (XLON(N) )
        PLAT(2,N) = - SITXYZ(3,N) * DSIN (XLON(N) )
        PLAT(3,N) = + CFRAD(N) * DCOS (XLAT(N) )
C
C     Compute the topocentric-to-crust-fixed rotation matrices by rotating 
C     about the geodetic latitude and the longitude. Also now compute a 
C     "radial-transverse" rotation matrix by rotating about the geocentric
C     latitude and the longitude. 
C
        XLAT_DUMMY = XLAT(N)
        CALL ROTAT ( XLAT_DUMMY, 2, RY )
c      write(6,8) ' XLAT? ',  xlat(n)*57.29578 
C
        XLON_DUMMY = XLON(N)
        CALL ROTAT ( -XLON_DUMMY, 3, RZ )
c      write(6,8) ' XLON? ',  xlon(n)*57.29578 
C
        GLAT_DUMMY = GLAT(N)
        CALL ROTAT ( GLAT_DUMMY, 2, RGY )
c      write(6,8) ' GLAT? ',  glat(n)*57.29578 
C
c       DO I=1,3
c         DO J=1,3
c           TCROT_DUMMY(I,J) = TCROT(I,J,N)
c           RTROT_DUMMY(I,J) = RTROT(I,J,N)
c         ENDDO
c       ENDDO
        CALL MMUL2 ( RZ, RY, TCROT_DUMMY(1,1) )
        CALL MMUL2 ( RZ, RGY,RTROT_DUMMY(1,1) )
        DO I=1,3
          DO J=1,3
            TCROT(I,J,N) = TCROT_DUMMY(I,J)
            RTROT(I,J,N) = RTROT_DUMMY(I,J)
          ENDDO
        ENDDO
c      write(6,8) ' TCROT ',  TCROT_DUMMY 
c      write(6,8) ' RTROT ',  RTROT_DUMMY 
C
      IF (KSITD .ne. 0) Then  !Station debug printout
       if (N.eq.1) Then
        WRITE ( 6, 1)
        WRITE(6,8)' EFLAT   ',EFLAT
        WRITE(6,8)' REARTH  ',REARTH
        WRITE(6,7)' NUMSIT  ',NUMSIT
       endif
    1  FORMAT (1X, 'Debug output for subroutine SITI.' )
       write(6,'(" For site #",i2)') N
       WRITE(6,4)' RY   ',((RY(J,K),J=1,3),K=1,3)
       WRITE(6,4)' RZ   ',((RZ(J,K),J=1,3),K=1,3)
       WRITE(6,4)' RGY  ',((RGY(J,K),J=1,3),K=1,3)
       WRITE (6,8)' Geoid Height  ',  xlatlonht(3) 
      Endif          !Station debug printout
      GO TO 490
C
  491 CONTINUE
C    Dummy out the above topocentric quantities, they have no meaning at the
C     geocenter
        XLAT(N)   = -999.D0
        XLON(N)   = -999.D0
        Height(N) = -999.D0
        SNRM(1,N) = 0.D0
        SNRM(2,N) = 0.D0
        SNRM(3,N) = 0.D0
        GLAT(N)   = -999.D0
        PLON(1,N) = 0.D0
        PLON(2,N) = 0.D0
        PLON(3,N) = 0.D0
        PLAT(1,N) = 0.D0
        PLAT(2,N) = 0.D0
        PLAT(3,N) = 0.D0
        SITAXO(N) = 0.D0
        KTYPE(N)  = 0
        SITZEN(N) = 0.D0
        DO I=1,3
         DO J=1,3
          TCROT(I,J,N) = 0.D0
          RTROT(I,J,N) = 0.D0
         ENDDO
        ENDDO
        DO I=1,11
          SITOAM(I,N) = 0.D0
          SITHOA(I,1,N) = 0.D0
          SITHOA(I,2,N) = 0.D0
          SITOPH(I,N) = 0.D0
          SITHOP(I,1,N) = 0.D0
          SITHOP(I,2,N) = 0.D0
        ENDDO
C
C     Close the loop which runs over the sites in the catalog.
  490 CONTINUE
C
C     Initialize the integer variable NLAST to zero.
      NLAST(1) = 0
      NLAST(2) = 0
C
C     Check KSITD for debug output.
      IF ( KSITD .ne. 0 ) Then  !Debug printout
C     WRITE ( 6, 11)
   11 FORMAT (1X, 'Station Debug for subroutine SITI.' )
      WRITE(6,8)' CFRAD   ',(CFRAD(J),J=1,NUMSIT)
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,7)' KTYPE   ',(KTYPE(J),J=1,NUMSIT)
    7 FORMAT(/,A,15I8/(7X,15I8))
      WRITE(6,7)' NLAST   ',NLAST
      WRITE(6,4)' PLAT    ',(( PLAT(J,K),J=1,3),K=1,NUMSIT)
    4 FORMAT(/,A,3D25.16/(9X,3D25.16))
      WRITE(6,4)' PLON    ',(( PLON(J,K),J=1,3),K=1,NUMSIT)
      WRITE(6,8)' SITAXO  ',( SITAXO(J),J=1,NUMSIT)
      WRITE(6,9)' SITOAM, ',((SITOAM(J,K),J=1,11),K=1,NUMSIT)
      WRITE(6,9)' SITOPH, ',((SITOPH(J,K),J=1,11),K=1,NUMSIT)
      WRITE(6,9)' SITHOA, ',(((SITHOA(J,L,K),J=1,11),L=1,2),K=1,
     .NUMSIT)
      WRITE(6,9)' SITHOP, ',(((SITHOP(J,L,K),J=1,11),L=1,2),K=1,
     .NUMSIT)
    9 FORMAT(/,A,11F9.4,/,(9X,11F9.4))
      WRITE(6,6)' SITXYZ  ',((SITXYZ(J,K),J=1,3),K=1,NUMSIT)
    6 FORMAT(/,A,3F20.4,/,(9X,3F20.4))
      WRITE(6,8)' SITZEN  ',(SITZEN(K),K=1,NUMSIT)
      WRITE(6,4)' SNRM    ',((SNRM(I,J),I=1,3),J=1,NUMSIT)
      WRITE(6,4)' TCROT   ',(((TCROT(I,J,K),I=1,3),J=1,3),K=1,
     .NUMSIT)
    5 FORMAT(/,A,/,3(3F20.4,/)/)
      WRITE(6,8)' XLAT    ',(XLAT(J),J=1,NUMSIT)
      WRITE(6,8)' XLON    ',(XLON(J),J=1,NUMSIT)
      WRITE(6,8)' HEIGHT    ',(HEIGHT(J),J=1,NUMSIT)
      WRITE(6,3)' LNSITE  ',((LNSITE(J,K),J=1,4),K=1,NUMSIT)
    3 FORMAT (/,A,4A2,/, 9X,4A2/)
C
      Endif          !Debug printout
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE SITG ( AXOFF, CFBASE, CFLAT, CFLON, CFSITE, CFSITN,
     1                  KAXIS, OCEAMP, OCEPHS,  SITLAT, SITLON, SITRAD,
     2                  TCTOCF, RTTOCF, ZPATH, SITHEIGHT, GEOLAT)
      IMPLICIT None
C
C 4.    SITG
C
C 4.1   SITG PROGRAM SPECIFICATION
C
C 4.1.1 SITG is the Site Module geometry section. SITG calculates the site
C       geometry for the stations participating in the current observation.
C
C 4.1.2 RESTRICTIONS - NONE
C
C 4.1.3 REFERENCES - MARKHAM'S X-DOCUMENT
C
C 4.2   SITG PROGRAM INTERFACE
C
C 4.2.1 CALLING SEQUENCE -
C 
C         OUTPUT VARIABLES: 
C           1. AXOFF(2)      - THE ANTENNA AXIS OFFSETS AT EACH SITE. (M)
C           2. CFBASE(3)     - THE CRUST FIXED BASELINE VECTOR. (M) 
C           3. CFLAT(3,2)    - THE PARTIAL DERIVATIVES OF THE SITE CRUST FIXED
C                              VECTOR COMPONENTS WITH RESPECT TO THE GEODETIC
C                              LATITUDES AT EACH OBSERVATION SITE. (M/RAD) 
C           4. CFLON(3,2)    - THE PARTIAL DERIVATIVES OF THE SITE CRUST FIXED
C                              VECTOR COMPONENTS WITH RESPECT TO THE EAST
C                              LONGITUDES AT EACH OBSERVATION SITE.  (M/RAD)
C           5. CFSITE(3,2)   - THE CRUST FIXED SITE VECTORS AT EACH SITE. (M)
C           6. CFSITN(3,2)   - THE CRUST FIXED SITE NORMAL UNIT VECTORS AT
C                              EACH OBSERVATION SITE. (UNITLESS)
C           7. KAXIS(2)      - THE ANTENNA AXIS TYPES FOR EACH SITE. (UNITLESS)
C           8. OCEAMP(11,3,2)- THE TOPOCENTRIC OCEAN LOADING AMPLITUDES FOR
C                    ( J,K,L)  THE 11 MAIN TIDES (J=1,11),
C                                      K=1 : VERTICAL,
C                                      K=2 : EAST-WEST, AND
C                                      K=3 : NORTH-SOUTH DIRECTION
C                               FOR EACH OBSERVATION SITE (L=1,2). (M)
C           9. OCEPHS(11,3,2)- THE OCEAN LOADING PHASES AT EACH SITE. (RAD)
C          10. SITLAT(2)     - THE GEODETIC LATITUDE AT EACH SITE. (RAD)
C          11. SITLON(2)     - THE EAST LONGITUDE AT EACH SITE. (RAD)
C          12. SITRAD(2)     - THE SPHERICAL EARTH RADIUS OF EACH SITE. (M)
C          13. TCTOCF(3,3,2) - THE ROTATION MATRIX WHICH ROTATES THE
C                              TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST FIXED
C                              REFERENCE SYSTEM AT EACH SITE. (UNITLESS)
C          14. RTTOCF(3,3,2) - The rotation matrix which rotates the 
C                              'radial-transverse' reference system to the 
C                              crust fixed reference system at each site.
C          15. ZPATH(2)      - THE ZENITH ELECTRICAL PATH LENGTH AT EACH 
C                              OBSERVATION SITE.  (SEC)
C          16. SITHEIGHT(2)  - The height above the geoid at each site. (m)
C          17. GEOLAT(2)     - The geocentric latitude at each site. (rad)
C
      INCLUDE 'cmxst.i'
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         then used downstream. Otherwise equals zero. For 
C                         correlator usage.
C
      INCLUDE 'ccon.i'
C          VARIABLES 'FROM':
C            1.  KSITC  -  THE SITE MODULE FLOW CONTROL FLAG.
C            2.  KSITD  -  THE SITE MODULE DEBUG OUTPUT FLAG.
C
C 4.2.3 PROGRAM SPECIFICATIONS -
C
      Real*8  AXOFF(2),CFBASE(3),CFLAT(3,2),CFLON(3,2),CFSITE(3,2),
     1        CFSITN(3,2),OCEAMP(11,3,2),OCEPHS(11,3,2),SITLAT(2),
     2        SITLON(2),SITRAD(2),TCTOCF(3,3,2),ZPATH(2),SITHEIGHT(2),
     3        RTTOCF(3,3,2), GEOLAT(2)
      Integer*2  KAXIS(2), LNBASE(4,2), NDO(3), KERR
      Integer*4  I, J, K, L, N, NN
C
C 4.2.4 DATA BASE ACCESS -
C          'GET' VARIABLES:
C            1. LNBASE(4,2) - THE EIGHT CHARACTER SITE NAMES OF THE BASELINE
C                             OF THE CURRENT OBSERVATION. (ALPHAMERIC) 
C          ACCESS CODES:
C            1. 'BASELINE' - THE DATA BASE ACCESS CODE FOR THE BASELINE
C                            IDENTIFICATION OF THE CURRENT OBSERVATION.
C
C 4.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: DRIVG
C           CALLED SUBROUTINES: GETA, KILL, VECSB
C
C 4.2.7 CONSTANTS USED - NONE
C
C 4.2.8 PROGRAM VARIABLES -
C            1.  KERR     -  THE DATA BASE ERROR RETURN FLAG.
C            2.  NDO(3)   -  THE DATA BASE RETURN ARRAY INDICES.
C            3.  LOC...(4) - CONTAINS THE NAMES OF THE STATIONS WITH KNOWN
C                            PARAMETERS OF HORIZONTAL DISPLACEMENT DUE TO 
C                            OCEAN LOADING.
C
C 4.2.9 PROGRAMMER - DALE MARKHAM    01/13/77
C                    PETER DENATALE  07/13/77
C                    CHOPO MA        08/06/81
C                    HAROLD M. SCHUH 10/08/83
C                    SAVITA GOEL 06/03/87 (CDS FOR A900)
C                    LOTHAR MOHLMANN 03/23/89
C                    89.07.20 Jim Ryan Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                      implimented.
C                    93.03.30 MSW SITCM common and maximum number of station
C                             variable put into newly created include file
C                             "cmxst.i".
C                    94.02.04 David Gordon SITHEIGHT(2) added, station heights 
C                             above geoid (meters).
C                    David Gordon 94.04.16 Converted to Implicit None.
C                    David Gordon 98.06.26 Adding rotation matrix RTTOCF(3,3,2)
C                             and geocentric latitudes GEOLAT(2), for use later
C                             in the solid Earth tide module.
C                    David Gordon 98.07.29 Added 'Include cobsn.i' with 
C                             variable Nzero, and code to determine when a 
C                             station is at the geocenter.
C
C     SITG program structure.
C
C     GET the baseline name. Check for errors.
      CALL GETA ('BASELINE      ', LNBASE, 4, 2, 1, NDO, KERR )
      IF ( KERR .EQ. 0 ) GO TO 310
      CALL CKILL (6HSITG  , 1, KERR )
C
C   Set geocenter indicator to zero
      Nzero = 0
C
C     Construct the arrays to hold the geometry of the stations participating 
C     in the current observation so that this information can be passed to the
C     rest of the program.
C
C     Loop for sites 1 and 2.
  310 DO 3130  L = 1,2
C
C       Determine the identification of the stations participating in
C       the current observation. If the baseline identification is not
C       successful, a message is written and the program terminates.
C
        DO 320  NN = 1, NUMSIT
          N = NN
          IF   ( ( LNSITE(1,NN) .EQ. LNBASE(1,L) )
     1    .AND.  ( LNSITE(2,NN) .EQ. LNBASE(2,L) )
     2    .AND.  ( LNSITE(3,NN) .EQ. LNBASE(3,L) )
     3    .AND. ( LNSITE(4,NN) .EQ. LNBASE(4,L) ) )  GO TO 330
  320   CONTINUE
C
        GO TO 700
C
C       Check to see if the ID of the baseline has changed from that of the
C       previous observation. If not, then retain the current site geometry.
C
  330   IF ( NLAST(L) .EQ. N )  GO TO 3130
        NLAST(L) = N
C
C       Construct the array to hold the crust fixed site vectors.
        CFSITE(1,L) = SITXYZ(1,N)
        CFSITE(2,L) = SITXYZ(2,N)
        CFSITE(3,L) = SITXYZ(3,N)
C
C       Construct the array to hold the site spherical radii.
        SITRAD(L) = CFRAD(N)
C
C       Construct the array to hold the site normal unit vectors.
        CFSITN(1,L) = SNRM(1,N)
        CFSITN(2,L) = SNRM(2,N)
        CFSITN(3,L) = SNRM(3,N)
C
C       Construct the arrays to hold the geodetic latitudes and the East
C       longitudes. Also now an array for geocentric latitude.
        SITLAT(L) = XLAT(N)
        SITLON(L) = XLON(N)
        SITHEIGHT(L) = HEIGHT(N)
        GEOLAT(L) = GLAT(N)
C
C       Construct arrays to hold the partial derivatives of the crust fixed site
C       coordinates with respect to the longitudes and the geodetic latitudes.
C
        CFLON(1,L) = PLON(1,N)
        CFLON(2,L) = PLON(2,N)
        CFLON(3,L) = PLON(3,N)
C
        CFLAT(1,L) = PLAT(1,N)
        CFLAT(2,L) = PLAT(2,N)
        CFLAT(3,L) = PLAT(3,N)
C
C       Construct the array to hold the site antenna axis offsets.
        AXOFF(L) = SITAXO(N)
C
C       Construct the array to hold the site antenna types.
        KAXIS(L) = KTYPE(N)
C
C       Construct the array to hold the topocentric to crust fixed
C       rotation matrices. Now also an array for the radial-transverse 
C       rotation matrices.
        DO 3112  J = 1,3
          DO 3111  I = 1,3
            TCTOCF(I,J,L) = TCROT(I,J,N)
            RTTOCF(I,J,L) = RTROT(I,J,N)
 3111     CONTINUE
 3112   CONTINUE
C
C       Construct the array to hold the zenith electrical path lengths
        ZPATH(L) = SITZEN(N)
C
C       Construct the arrays to hold the site ocean loading amplitudes
C       and phases.
        DO J = 1, 11
          OCEAMP(J,1,L) = SITOAM(J,N)
          OCEAMP(J,2,L) = SITHOA(J,1,N)
          OCEAMP(J,3,L) = SITHOA(J,2,N)
C
          OCEPHS(J,1,L) = SITOPH(J,N)
          OCEPHS(J,2,L) = SITHOP(J,1,N)
          OCEPHS(J,3,L) = SITHOP(J,2,N)
        ENDDO
C
C  Check for geocenter station
       If(Zero_site .ne. 0 .and. N .eq. Zero_site) Then
        Nzero = N
        print *, ' SITG: Geocenter Site Found, Nzero =  ', Nzero
       Endif
C
C     Close the loop which runs over the sites.
 3130 CONTINUE
C
C     Construct the array to hold the crust fixed baseline vector.
      CALL VECSB ( CFSITE(1,1), CFSITE(1,2), CFBASE )
C
C     Check KSITD for debug output.
      IF ( KSITD .EQ. 0 )  GO TO 600
      WRITE ( 6, 1)
    1 FORMAT (1X, 'Debug output for subroutine SITG.' )
      WRITE(6,8)' CFRAD   ',(CFRAD(J),J=1,NUMSIT)
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,7)' KTYPE   ',(KTYPE(J),J=1,NUMSIT)
    7 FORMAT(/,A,15I8/(7X,15I8))
      WRITE(6,7)' NLAST   ',NLAST
      WRITE(6,7)' NUMSIT  ',NUMSIT
      WRITE(6,4)' PLAT    ',(( PLAT(J,K),J=1,3),K=1,NUMSIT)
    4 FORMAT(/,A,3D25.16/(9X,3D25.16))
      WRITE(6,4)' PLON    ',(( PLON(J,K),J=1,3),K=1,NUMSIT)
      WRITE(6,8)' SITAXO  ',( SITAXO(J),J=1,NUMSIT)
      WRITE(6,9)' SITOAM, ',((SITOAM(J,K),J=1,11),K=1,NUMSIT)
      WRITE(6,9)' SITOPH, ',((SITOPH(J,K),J=1,11),K=1,NUMSIT)
      WRITE(6,9)' SITHOA, ',(((SITHOA(J,L,K),J=1,11),L=1,2),K=1,
     .NUMSIT)
      WRITE(6,9)' SITHOP, ',(((SITHOP(J,L,K),J=1,11),L=1,2),K=1,
     .NUMSIT)
    9 FORMAT(/,A,11F7.4,/,(9X,11F7.4))
      WRITE(6,6)' SITXYZ  ',((SITXYZ(J,K),J=1,3),K=1,NUMSIT)
    6 FORMAT(/,A,3F20.4,/,(9X,3F20.4))
      WRITE(6,8)' SITZEN  ',(SITZEN(K),K=1,NUMSIT)
      WRITE(6,4)' SNRM    ',((SNRM(I,J),I=1,3),J=1,NUMSIT)
      WRITE(6,4)' TCROT   ',(((TCROT(I,J,K),I=1,3),J=1,3),K=1,
     .NUMSIT)
    5 FORMAT(/,A,/,3(3F20.4,/)/)
      WRITE(6,8)' XLAT    ',(XLAT(J),J=1,NUMSIT)
      WRITE(6,8)' XLON    ',(XLON(J),J=1,NUMSIT)
      WRITE(6,8)' GLAT    ',(GLAT(J),J=1,NUMSIT)
      WRITE(6,8)' SITHEIGHT ',(SITHEIGHT(J),J=1,NUMSIT)
C
      WRITE ( 6, 9200 )  AXOFF, CFBASE, CFLAT, CFLON, CFSITE,
     1           CFSITN, KAXIS, OCEAMP, OCEPHS, SITLAT, SITLON,
     2           SITRAD, TCTOCF, ZPATH,  LNBASE
 9200 FORMAT (1X, 'AXOFF  = ', 2 ( D30.16, 10X ), /, 1X,
     .            'CFBASE = ', 3 ( D30.16, 10X ), /, 1X,
     .            'CFLAT  = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),
     .            'CFLON  = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),
     .            'CFSITE = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),
     .            'CFSITN = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),
     .            'KAXIS  = ',/, 2 ( I2, 10X ), /, 1X,
     .            'OCEAMP = ',/,2( 3( 11F10.4,/ ),/),/,1X,
     .            'OCEPHS = ',/,2( 3( 11F10.4,/ ),/,1X ),/,1X,
     .            'SITLAT = ', 2 ( D30.16, 10X ), /, 1X,
     .            'SITLON = ', 2 ( D30.16, 10X ), /, 1X,
     .            'SITRAD = ', 2 ( D30.16, 10X ), /, 1X,
     .            'TCTOCF = ',/,6( 3 ( D30.16, 10X ), /, 1X ),
     .            'ZPATH  = ', 2 ( D30.16, 10X ), /, 1X,
     .            'LNBASE = ', 4A2,1X,4A2)
C
C     Normal conclusiton.
  600 RETURN
C
C     Abnormal conclusion.       .
  700 WRITE ( 6, 9300 )
 9300 FORMAT (1X, 'CALC has been terminated in subroutine SITG.  ',
     1            'The baseline identification was not successful.' )
      CALL CKILL (6HSITG  , 0, 0)
      END
C
C******************************************************************************
      SUBROUTINE SITP ( R2000, STAR , EARTH, SITEV)
      IMPLICIT None
C
C 5.    SITP
C
C 5.1   SITP PROGRAM SPECIFICATION
C
C 5.1.1 SITP is the Site Module partial derivatives section. SITP
C       computes the partial derivatives of the delay and the rate with
C       respect to the site crust fixed vector components at each site.
C
C 5.1.2 RESTRICTIONS - NONE
C
C 5.1.3 REFERENCES - MARKHAM'S X-DOCUMENT
C
C 5.2   SITP PROGRAM INTERFACE
C
C 5.2.1 CALLING SEQUENCE -
C
C         INPUT VARIABLES:
C           1.  R2000(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION 
C                              MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
C                              (UNITLESS, 1/SEC, 1/SEC**2)
C           2.  STAR(3)      - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C           3.  EARTH(3,3)   - The position, velocity, and acceleration of the 
C                              Earth relative to the SSBC. (m, m/s, m/s**2)
C           4. SITEV(3,2)    - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF EACH
C                              OBSERVATION SITE. (M/SEC)
C
C         OUTPUT VARIABLES: NONE
C
C 5.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C          VARIABLES 'FROM':
C            1.  VLIGHT  -  THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C            2.  VLIGHT2 -  THE VELOCITY OF LIGHT SQUARED.
C
      INCLUDE 'ccon.i'
C          VARIABLES 'FROM':
C            1.  KSITC  -  THE SITE MODULE FLOW CONTROL FLAG.
C            2.  KSITD  -  THE SITE MODULE DEBUG OUTPUT FLAG.
C
C 5.2.3 PROGRAM SPECIFICATIONS -
C
      Real*8 R2000(3,3,3), STAR(3), EARTH(3,3), SITEV(3,2), DBDX1(3,2),
     *       DBDX2(3,2), DBDY1(3,2), DBDY2(3,2), DBDZ1(3,2), 
     *       DBDZ2(3,2), DSITP(3,2,2), VG(3), VE(3), c1, c2, tt, DOTP
      Integer*4 I, K
C
C 5.2.4 DATA BASE ACCESS -
C
C          'GET' VARIABLES: NONE
C
C          'PUT' VARIABLES:
C            1. DSITP(3,2,2) - THE PARTIAL DERIVATIVES OF THE DELAY AND THE
C                              DELAY RATE WITH RESPECT TO THE CRUST FIXED SITE
C                              COORDINATES AT EACH OBSERVATION SITE. THE FIRST
C                              INDEX RUNS OVER THE SITE COORDINATES, THE SECOND
C                              INDEX RUNS OVER THE SITES, AND THE THIRD RUNS
C                              OVER THE DELAY AND THE DELAY RATE. 
C                              (SEC/M, SEC/SEC-M)
C 
C          ACCESS CODES:
C            1. 'SIT PART'  -  THE DATA BASE ACCESS CODE FOR THE SITE MODULE
C                              PARTIAL DERIVATIVES ARRAY.
C 
C 5.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT 
C 
C 5.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: DRIVP 
C           CALLED SUBROUTINES: DOTP, PUT4
C 
C 5.2.7 CONSTANTS USED - VLIGHT 
C 
C 5.2.8 PROGRAM VARIABLES - 
C       1.  DBDX1(3,2)  -  THE PARTIAL DERIVATIVES OF THE J2000.0 BASELINE
C                          POSITION AND VELOCITY VECTORS WITH RESPECT TO THE
C                          X-COMPONENT OF THE CRUST FIXED SITE VECTOR AT
C                          OBSERVATION SITE #1. (M/M, M/M-SEC)
C       2.  DBDX2(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
C                          X-COMPONENT AT SITE #2.  (M/M, M/M-SEC)
C       3.  DBDY1(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
C                          Y-COMPONENT AT SITE #1.  (M/M, M/M-SEC)
C       4.  DBDY2(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
C                          Y-COMPONENT AT SITE #2.  (M/M, M/M-SEC)
C       5.  DBDZ1(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
C                          Z-COMPONENT AT SITE #1.  (M/M, M/M-SEC)
C       6.  DBDZ2(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
C                          Z-COMPONENT AT SITE #2.  (M/M, M/M-SEC)
C       7.  VE(3)       -  A local copy of the velocity of the Earth
C                          relative to the SSBC. 
C       9.  ci          -  1.d0/VLIGHT
C      10.  tt          -  A term common to all partials.
C
C 5.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/13/77
C                    CHOPO MA       08/06/81
C                    89.07.20 Jim Ryan Documentation simplified.
C                    Jim Ryan 89:10:05 CPHYS common made an include file.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                   implimented.
C                    Jim Ryan 91.11.22 Next term from the delay and rate
C                                   theoreticals added to partials computation.
C                    David Gordon 94.04.16 Converted to Implicit None.
C                    David Gordon 98.10.15 Added SITEV to input arguments. 
C                                   Changed site delay and rate partials 
C                                   computations to use the Consensus formula. 
C                                   Differences are very small and probably
C                                   not noticeable.
C
C     SITP program structure
C
C     Loop twice for delay and rate partials for both sites.
C      [Index K runs over the delays and rates.]
      DO 300  K = 1,2
C
C   Loop three times for the calculation of the partials with respect to the
C    crust fixed vector components.
        DO 140  I = 1,3
C
C    Compute the partial derivatives of the J2000.0 baseline position and
C    velocity vectors with respect to the crust fixed vector coordinates 
C    at site #2.
          DBDX2(I,K) = R2000(I,1,K)
          DBDY2(I,K) = R2000(I,2,K)
          DBDZ2(I,K) = R2000(I,3,K)
C
C    Compute the partial derivatives of the J2000.0 baseline position and
C    velocity vectors with respect to the crust fixed site coordinates 
C    at site #1.
          DBDX1(I,K) = - R2000(I,1,K)
          DBDY1(I,K) = - R2000(I,2,K)
          DBDZ1(I,K) = - R2000(I,3,K)
C
C    Close the loop running over the vector components.
  140   CONTINUE
C
C    Complete the calculation of the partial derivatives of the delay and the
C    rate with respect to the crust fixed site vector components at each site.
c
c    First make a local copy of the velocity of the Earth.
C       DO I =1,3
C         VG(I) = EARTH(I,2)
C       Enddo
C       c1 = 1.d0/VLIGHT
C       c2 = 1.d0/VLIGHT2
C       tt = 1.d0 - c1*DOTP(star,vg)
C
C       DSITP(1,1,K)=-c1*DOTP(DBDX1(1,K),STAR)*tt-c2*DOTP(DBDX1(1,K),VG)
C       DSITP(2,1,K)=-c1*DOTP(DBDY1(1,K),STAR)*tt-c2*DOTP(DBDY1(1,K),VG)
C       DSITP(3,1,K)=-c1*DOTP(DBDZ1(1,K),STAR)*tt-c2*DOTP(DBDZ1(1,K),VG)
C       DSITP(1,2,K)=-c1*DOTP(DBDX2(1,K),STAR)*tt-c2*DOTP(DBDX2(1,K),VG)
C       DSITP(2,2,K)=-c1*DOTP(DBDY2(1,K),STAR)*tt-c2*DOTP(DBDY2(1,K),VG)
C       DSITP(3,2,K)=-c1*DOTP(DBDZ2(1,K),STAR)*tt-c2*DOTP(DBDZ2(1,K),VG)
C     WRITE(6,8)' Old DSITP ', DSITP(1,1,K), DSITP(2,1,K),DSITP(3,1,K)
C     WRITE(6,8)' Old DSITP ', DSITP(1,2,K), DSITP(2,2,K),DSITP(3,2,K)
C
C Change to use the Consensus model definition
        DO I =1,3
          VG(I) = EARTH(I,2) + SITEV(I,2)
          VE(I) = EARTH(I,2)
        Enddo
         tt = 1.d0 + DOTP(STAR,VG)/VLIGHT
c
        DSITP(1,1,K) = -DOTP(DBDX1(1,K),STAR)/VLIGHT/tt
     *                 - DOTP(DBDX1(1,K),VE)/VLIGHT2
        DSITP(2,1,K) = -DOTP(DBDY1(1,K),STAR)/VLIGHT/tt
     *                 - DOTP(DBDY1(1,K),VE)/VLIGHT2
        DSITP(3,1,K) = -DOTP(DBDZ1(1,K),STAR)/VLIGHT/tt
     *                 - DOTP(DBDZ1(1,K),VE)/VLIGHT2
        DSITP(1,2,K) = -DOTP(DBDX2(1,K),STAR)/VLIGHT/tt
     *                 - DOTP(DBDX2(1,K),VE)/VLIGHT2
        DSITP(2,2,K) = -DOTP(DBDY2(1,K),STAR)/VLIGHT/tt
     *                 - DOTP(DBDY2(1,K),VE)/VLIGHT2
        DSITP(3,2,K) = -DOTP(DBDZ2(1,K),STAR)/VLIGHT/tt
     *                 - DOTP(DBDZ2(1,K),VE)/VLIGHT2
C     WRITE(6,8)' New DSITP ', DSITP(1,1,K), DSITP(2,1,K),DSITP(3,1,K)
C     WRITE(6,8)' New DSITP ', DSITP(1,2,K), DSITP(2,2,K),DSITP(3,2,K)
C
C    Close the loop which runs over the partials of the delay and rate.
  300 CONTINUE
C
C    PUT the site module partials.
      CALL PUT4 ('SIT PART      ',DSITP, 3, 2, 2 )
C
C    Check KSITD for debug output.
      IF ( KSITD .EQ. 0 )  GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, 'Debug output for subroutine SITP.' )
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DBDX1   ',DBDX1
      WRITE(6,8)' DBDX2   ',DBDX2
      WRITE(6,8)' DBDY1   ',DBDY1
      WRITE(6,8)' DBDY2   ',DBDY2
      WRITE(6,8)' DBDZ1   ',DBDZ1
      WRITE(6,8)' DBDZ2   ',DBDZ2
      WRITE(6,8)' DSITP   ',DSITP
      WRITE(6,8)' VLIGHT  ',VLIGHT
      WRITE(6,8)' c1      ',c1
      WRITE(6,8)' c2      ',c2
      WRITE(6,8)' tt      ',tt
      WRITE(6,8)' vg      ',vg
      WRITE ( 6, 9200 )  R2000, STAR
 9200 FORMAT (1X, 'R2000 = ', 9 ( 3 ( D30.16, 10X ), /, 1X ),
     1            'STAR  = ',     3 ( D30.16, 10X ) )
C
C     Normal conclusion.
  700 RETURN
      END
C
C*******************************************************************
      SUBROUTINE bkplh(XYZ,PLH,PI,A,FL)                                 
      IMPLICIT NONE
C                                                                       
C NAME             bkplh.f                                    
C                                                                       
C VERSION          93.01.27                                             
C                                                                       
C WRITTEN          B. Archinal, USNO, July 20-23, 1990.
C                  Name changed from "borkow" to "bkplh", and arguments
C                  adapted for efficient use by Calc (csitm), Dbcal,
C                  and Solve.  BA, 93.01.27.                 
C                                                                       
C PURPOSE          Converts XYZ coordinates to Phi, Lambda, H           
C                  ellipsoidal coordinates.                             
C                                                                       
C References       Borkowski, K. M. (1989).  "Accurate Algorithms to    
C                  transform geocentric to geodetic coordinates"        
C                  *Bulletin Geodesique*, v. 63, pp. 50-56.  Also see   
C                  Borkowski, K. M. (1987).  "Transformation of         
C                  Geocentric to Geodetic Coordinates without           
C                  Approximations", *Astrophysics and Space Science*,   
C                  v. 139, n. 1, pp. 1-4.  Correction in (1988), v. 146,
C                  n. 1, p. 201.                                        
C                                                                       
C Note             Will not work for points on the Z axis, i.e. if      
C                  if X=Y=0 (Phi = +/- 90 degrees).                     
C                                                                       
C Calling sequence CALL bkplh ( XYZ, PLH, PI, A, FL )                    
C                                                                       
C ARGUMENT LIST                                                         
C                                                                       
C  PARM       TYPE DESCRIPTION                                          
C                                                                       
C  XYZ(3)     D    INPUT - XYZ Cartesian coordinates of point.          
C                  XYZ(1) and XYZ(2) must not both be zero.  Units are  
C                  those of A below.                                    
C  PLH(3)     D    OUTPUT - Ellipsoidal coordinates of point, in        
C                  geodetic latitude, longitude, and height.  Units     
C                  for latitude and longitude are in radians, units     
C                  of height are those of A below.
C  PI         D    INPUT - Ratio of circumference to diameter of circle.
C                  Unitless.
C  A          D    INPUT - Semi-major axis of ellipsoid.  Units are     
C                  of distance (meters, kilometers, miles, etc.).       
C  FL         D    INPUT - Flattening of ellipsoid.  Unitless.
C                 
C                                                                       
C SUBPROGRAMS USED                                                      
C  Fortran         DABS      DACOS     DATAN     DATAN2   DCOS          
C                  DSIN      DSQRT                                      
C                                                                       
C COMMON BLOCKS    None.                                                
C                                                                       
C INPUT            None.                                                
C                                                                       
C OUTPUT           None, unless diagnostic printout uncommented.                                                
C                                                                       
C LANGUAGE         Fortran 77.                
C                                                                       
C===================================================================    
C
      Real*8 A,B,D,DABS,DACOS,DATAN,DATAN2,DCOS,DSIN,
     .       DSQRT,E,F,FL,G,P,PI,Q,R,T,V,X,Y,Z,ZLONG
      Real*8 XYZ(3),PLH(3)                                           
C     INTEGER IOUT
C
C--- XYZ.                                                               
      X=XYZ(1)                                                          
      Y=XYZ(2)                                                          
      Z=XYZ(3)                                                          
C--- Semi-minor axis.                                                   
      B=A*(1.D0-FL)                                                     
C--- Set sign of B to that of Z in order to get sign of Phi correct.    
      IF(Z.LT.0.D0) B=-B                                                
C--- Intermediate Values for Latitude.                                  
      R=DSQRT(X*X+Y*Y)                                                  
      E=(B*Z-(A*A-B*B))/(A*R)                                           
      F=(B*Z+(A*A-B*B))/(A*R)                                           
      P=4.D0/3.D0 * (E*F+1)                                             
      Q=2.D0 * (E*E - F*F)                                              
      D=P*P*P+Q*Q                                                       
      IF(D.GE.0.D0) then                                                     
        V=(DSQRT(D)-Q)**(1.D0/3.D0) - (DSQRT(D)+Q)**(1.D0/3.D0)           
        else                                                     
        V=2.D0 * DSQRT(-P) * DCOS (1.D0/3.D0 *                            
     .  DACOS(Q/(P * DSQRT(-P))))
        endif                                 
C   (Improve V - not really necessary except near axes.)                
      IF(V*V.LT.DABS(P)) V=-(V*V*V + 2.D0*Q)/(3.D0*P)                   
      G=(DSQRT(E*E+V)+E)/2.D0                                           
      T=DSQRT( G*G  + (F-V*G)/(2.D0*G-E) ) - G                          
      PLH(1)=DATAN( (A*(1.D0-T*T))/(2.D0*B*T) )                         
C--- HEIGHT.                                                            
      PLH(3)=(R-A*T)*DCOS(PLH(1)) + (Z-B)*DSIN(PLH(1))                  
C--- LONGITUDE.                                                         
      ZLONG=DATAN2(Y,X)                                                 
      IF(ZLONG.LT.0.D0) ZLONG=ZLONG+2.D0*PI                             
      PLH(2)=ZLONG                                                      
C                                                                       
C   Diagnostic output.                                                  
C                                                                       
C     IOUT=11                                                           
C     WRITE(IOUT,901) A,F,B                                          
C 901 FORMAT(' A,F,B:',3D25.16)                                       
C     WRITE(IOUT,902) X, Y, Z                                           
C 902 FORMAT(' X, Y, Z:',3D25.16)                                       
C     WRITE(IOUT,903) R,E,F                                             
C 903 FORMAT(' R, E, F:',3D25.16)                                       
C     WRITE(IOUT,904) P,Q,D                                             
C 904 FORMAT(' P, Q, D:',3D25.16)                                       
C     WRITE(IOUT,905) V,G,T                                             
C 905 FORMAT(' V, G, T:',3D25.16)                                       
C--- Check.                                                             
C     CHK1=T*T*T*T + 2.D0 * E *T*T*T + 2.D0 * F *T - 1.D0               
C     CHK2=V*V*V + 3.D0*P*V + 2.D0*Q                                    
C     WRITE(IOUT,906) CHK1,CHK2                                         
C 906 FORMAT('Check values (=0):',2D25.16)                              
      RETURN                                                            
      END                                                               
C******************************************************************************
C
       SUBROUTINE SITBLK(Kerr)
       Implicit None
C
      INCLUDE 'cmxst.i'
      INCLUDE 'inputs.i'
C
      Integer*2 Getunit, Kerr(10)
      Integer*4 I, II, Jsite(Max_stat), Iunit, An_typ, ios,
     *          Iquit, Index
      Real*8 Sit_X, Sit_Y, Sit_Z, Ax_off, S_zen
C
      Character*80 Inbuf
      Character*8  Dbsites(Max_stat)
      Equivalence (LNSITE(1,1), Dbsites(1))
C
      Character*4 T_plate(Max_stat), plate
      Integer*2   L_plate(2,Max_stat) 
      Equivalence (T_plate,L_plate)
C
C  Programmer/History:
C       David Gordon 1998.03.17 Program created.
C       David Gordon 2001.01.05 Code to PUT Lcode TECTPLNM. 
C
C   Initialize station counter
       Do I = 1, Max_stat
         Jsite(i) = 0
       Enddo
C
C  Open the input sites data file
       Iunit = getunit()
       Open (Unit=Iunit, File=Ex_sites, Status='old', Readonly,
     *       Err=240, Iostat=ios)
C
      If ( Index(Ex_sites,'blokq') .gt. 0) Then 
C  If it is a blokq.dat file, find the site catalog section
  50   Continue
       Read(iunit,'(A80)') Inbuf
C      If (Inbuf(1:18) .eq. '$$ STATION CATALOG') Go to 100
       If (Inbuf(1:4) .eq. '$$//') Go to 100
       Go to 50
 100   Continue
      Endif
C
 110   Continue
       Read(iunit,'(A80)',end=200) Inbuf
C   Skip comments and illegal lines
c      If (Inbuf(1:2) .eq. '$$'  ) Go to 110
       If (Inbuf(1:4) .ne. '    ') Go to 110
       If (Inbuf(13:13) .ne. ' ' ) Go to 110
C
C   Finished site catalog
       If (Inbuf(1:2) .eq. '//') Go to 200
C
       Do I = 1, Numsit
         If (Inbuf(5:12) .eq. Dbsites(I)) Then
C         print *, 'Site matched: ', Dbsites(I)
C  Matched I'th station in data base station list, save station particulars
           II = I
           Read(Inbuf(14:80),*) Sit_X, Sit_Y, Sit_Z, An_typ, Ax_off, 
     *                          S_zen, plate
C
           SITXYZ(1,II) = Sit_X
           SITXYZ(2,II) = Sit_Y
           SITXYZ(3,II) = Sit_Z
           SITAXO(II)   = Ax_off
           KTYPE(II)    = An_typ
           SITZEN(II)   = S_zen
           T_plate(II)  = plate
C
           Jsite(II)    = II
C
         Endif
C
       Enddo
C
       Go to 110
C
 200   Continue
C
       Close(Iunit)
C
C   Verify that we have site a priori's for all stations. If not, we must
C    quit here and tell the user to fix the problem.
C
        Iquit = 0
      DO I = 1, Numsit
        If (Jsite(i) .eq. 0) Then
          Write(6,'(" SITBLK: No match for station #",i3)') I
          Iquit = Iquit + 1
        Endif 
      ENDDO
C
       If (Iquit.eq.0) Then
        Kerr(3) = 0
        Kerr(4) = 0
        Kerr(5) = 0
        Kerr(6) = 0
       Else
        CALL CKILL(6HSITBLK,0,0)
       Endif
C
C       print *, ' SITXYZ ', SITXYZ
C       print *, ' SITAXO ', SITAXO
C       print *, ' KTYPE  ', KTYPE 
C       print *, ' SITZEN ', SITZEN
C
C  Now we must replace the site a priori's in the data base
      CALL PUTR('SITERECS      ', SITXYZ, 3, Numsit, 1)
      CALL PUTR('SITEZENS      ', SITZEN, Numsit, 1, 1)
      CALL PUTI('AXISTYPS      ', KTYPE , Numsit, 1, 1)
      CALL PUTR('AXISOFFS      ', SITAXO, Numsit, 1, 1)
      CALL PUTA('TECTPLNM      ', L_plate, 2,Numsit,1 )
C
       Go to 270
C
C   error on OPEN
 240  Continue
      print *, 'Error on OPEN of blokq file '
C
 270  Continue
      Return
      End
C******************************************************************************
C
       SUBROUTINE OCNIN(Kerr)
       Implicit None
C
      INCLUDE 'cmxst.i'
      INCLUDE 'inputs.i'
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
C         VARIABLES 'FROM':
C            1. CONVD - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
C
      Integer*2 Getunit, Kerr(10)
      Integer*4 I, II, Jsite(Max_stat), Iunit, ios, Iquit, Index, K
      Real*8    OC11(11) 
C
      Character*80 Inbuf
      Character*8  Dbsites(Max_stat)
      Equivalence (LNSITE(1,1), Dbsites(1))
C
      Character*4 Ocestat(Max_stat)
      Integer*2 L_ocestat(2,Max_stat)
      Equivalence (Ocestat,L_ocestat)
C
C  Programmer/History:
C       David Gordon 98.03.17 - Program created.
C       David Gordon 99.11.19 - Bug fix - corrected ocean loading external
C                               file site name.
C       David Gordon 2000.01.05 - Determine ocean loading statuses (must be
C                               'YES' for all stations) and PUT them into
C                               the type 1 records, Lcode 'OCE STAT'.
C
C   Initialize station counter
       Do I = 1, Max_stat
         Jsite(i) = 0
       Enddo
C
C  Open the Ocean loading data file
       Iunit = getunit()
       Open (Unit=Iunit, File=Ex_ocean, Status='old', Readonly,
     *       Err=240, Iostat=ios)
C
      If ( Index(Ex_ocean,'blokq') .gt. 0) Then 
C  Blokq.dat file, find the site catalog
  50   Continue
       Read(iunit,'(A80)') Inbuf
       If (Inbuf(1:2) .eq. '//') Go to 100
       Go to 50
 100   Continue
      Endif
C
 110   Continue
       Read(iunit,'(A80)',end=200) Inbuf
C   Skip comments and illegal lines
       If (Inbuf(1:2) .eq. '$$') Go to 110
       If (Inbuf(1:2) .ne. '  ') Go to 110
c      If (Inbuf(11:11) .ne. ' ' ) Go to 110
C
C   Finished site catalog
       If (Inbuf(1:2) .eq. '//') Go to 200
C
C See if this station is in the database list
       Do I = 1, Numsit
         If (Inbuf(3:10) .eq. Dbsites(I)) Then
C         print *, 'Site matched: ', Dbsites(I)
C  Matched I'th station in data base station list, save station particulars
           II = I
C
C  Skip comments
 170    Continue
        Read(iunit,'(A80)',end=200) Inbuf
        If (Inbuf(1:2) .eq. '$$') Go to 170
C
C   Read 6 data lines
        Read(Inbuf,*,err=180) OC11 
          Do k=1,11
           SITOAM(k,II) = OC11(k)
          Enddo
C
        Read(Iunit,*,err=180) OC11 
          Do k=1,11
           SITHOA(k,1,II) = OC11(k)
          Enddo
        Read(Iunit,*,err=180) OC11 
          Do k=1,11
           SITHOA(k,2,II) = OC11(k)
          Enddo
C
        Read(Iunit,*,err=180) OC11 
          Do k=1,11
           SITOPH(k,II) = OC11(k) * CONVD
          Enddo
        Read(Iunit,*,err=180) OC11 
          Do k=1,11
           SITHOP(k,1,II) = OC11(k) * CONVD
          Enddo
        Read(Iunit,*,err=180) OC11 
          Do k=1,11
           SITHOP(k,2,II) = OC11(k) * CONVD
          Enddo
C
           Jsite(II)    = II
C
         Endif
       Enddo
C
       Go to 110
C
 200   Continue
C
       Close(Iunit)
C
C   Verify that we have ocean loading a priori's for all stations, except any
C    site at the geocenter. If not, we must quit here and tell the user to fix 
C    the problem.
C
        Iquit = 0
C
      DO I = 1, Numsit
        If (Jsite(i) .eq. 0) Then
           If (I .ne. Zero_site) Then
            Write(6,'(" SITBLK: No match for station #",i3)') I
            Iquit = Iquit + 1
           Endif 
        Endif 
      ENDDO
C
       If (Iquit.eq.0) Then
        Kerr(7)  = 0
        Kerr(8)  = 0
        Kerr(9)  = 0
        Kerr(10) = 0
         Do I = 1, Numsit
          Ocestat(I) = 'YES '
         Enddo
       Else
        CALL CKILL(6HOCNIN ,0,0)
       Endif
C
C       print *, ' SITOAM ', SITOAM
C       print *, ' SITOPH ', SITOPH
C       print *, ' SITHOA ', SITHOA
C       print *, ' SITHOP ', SITHOP
C
C  Now we must replace the ocean loading a priori's in the data base
      CALL PUTR('SITOCAMP      ', SITOAM, 11, Numsit, 1 )
      CALL PUTR('SITOCPHS      ', SITOPH, 11, Numsit, 1 )
      CALL PUTR('SITHOCAM      ', SITHOA, 11, 2, Numsit )
      CALL PUTR('SITHOCPH      ', SITHOP, 11, 2, Numsit )
      CALL PUTA('OCE STAT      ', L_ocestat, 2,Numsit,1 )
C
       Go to 270
C
C   error on Read
 180  Continue
      print *, 'OCNIN: Error on read of ocean loading file '
        CALL CKILL(6HOCNIN ,0,0)
C
C   error on OPEN
 240  Continue
      print *, 'OCNIN: Error on OPEN of ocean loading file '
        CALL CKILL(6HOCNIN ,0,0)
C
 270  Continue
      Return
      End
