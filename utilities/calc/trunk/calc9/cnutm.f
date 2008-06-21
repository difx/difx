      SUBROUTINE NUTA
      IMPLICIT None 
C
C 1.    NUTA
C
C 1.1   NUTA PROGRAM SPECIFICATION
C
C 1.1.1 NUTA ADDS ENTRIES TO THE TABLE OF CONTENTS FOR THE NUTATION
C       MODULE TEXT MESSAGE, THE NUTATION MODULE FLOW CONTROL MESSAGE,
C       THE IERS 1996 (NEW) NUTATION VALUES, THE WAHR (OLD) NUTATION 
C       VALUES, THE WAHR DIFFERENCE CONTRIBUTIONS, THE NUTATION PARTIAL 
C       DERIVATIVES, AND THE FUNDAMENTAL ARGUMENTS. ALSO REMOVES OLD 
C       LCODES.  
C 
C 1.2   NUTA PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - NONE 
C 
C 1.2.2 COMMON BLOCKS USED
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KNUTC - THE NUTATION MODULE CONTROL FLAG. 
C                 0 - 1996 IERS Nutation (Herring) 
C                 1 - NO NUTATION
C                 2 - 1980 IAU Nutation (Wahr) 
C 
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C 
C 1.2.3 PROGRAM SPECIFICATIONS - NONE 
C 
C 1.2.4 DATA BASE ACCESS -
C            ACCESS CODES:
C              1.  'NUT MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 NUTATION MODULE TEXT MESSAGE. 
C              2.  'NUT CFLG'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 NUTATION MODULE FLOW CONTROL MESSAGE. 
C              3.  'NUT 1996'  -  THE DATA BASE ACCESS CODE FOR THE IERS
C                                 1996 NUTATION VALUES. 
C              4.  'NUT WAHR'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 WAHR NUTATION VALUES. 
C              5.  'NUT PART'  -  THE DATA BASE ACCESS CODE FOR THE NUTATION
C                                 MODULE PARTIAL DERIVATIVE ARRAY 
C              6.  'WAHRCONT'  -  The data base access code for the delay and
C                                 rate contribution to convert from IERS 1996
C                                 nutation to IAU 1980 (Wahr) nutation.
C              7.  'FUNDARGS'  -  The data base access code for the fundamental
C                                 arguments and their time derivatives. 
C              8.  'GDNUTCON'  -  The data base access code for the Fukushima
C                                 geodesic nutation in longitude contributions
C                                 to the delay and rate. 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP 
C             CALLED SUBROUTINES: ADDA, ADDR
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - NONE
C
C 1.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 12/16/77
C                    CHOPO MA      08/04/81
C                    JIM RYAN      08/26/81
C                    GEORGE KAPLAN 04/24/84
C                    DAVID GORDON  11/13/84  (REDIMENSIONED NT1 AMPS &
C                                             NT1 PART, ETC)
C                    SAVITA GOEL  06/03/87  (CDS FOR A900)
C                    Jim Ryan     89.06.30 Documetation clean up.
C                    Jim Ryan     89.12.12 UNIX-like database interface
C                                 implimented.
C                    David Gordon 94.04.15 Converted to Implicit None.
C                    David Gordon 94.06.09 Rearranged COMMON /NUTCM/  (Real*8,
C                             Real*4, Integer*4, Integer*2). Changed a few  
C                             Integer*2's to Integer*4.
C                    David Gordon 98.02.03 Default changed to 1996 IERS 
C                             nutation model. 'NT1 AMPS', 'NT1 PART', etc.
C                             code removed, and DELR's added to remove old
C                             Lcodes. New Lcodes 'NUT 1996', 'WAHRCONT',
C                             and 'FUNDARGS' added. Nutation from database 
C                             option removed.
C                    David Gordon 98.11.12 Add for 'GDNUTCON', geodesic 
C                             nutation contribution.
C
C 1.3   NUTA PROGRAM STRUCTURE
C
C   ADD for the module text message.
      CALL ADDA (1,'NUT MESS','Nutation message definition     ',
     1     40, 1, 1 )
C
C   ADD for  module flow control message.
      CALL ADDA (1,'NUT CFLG','Nutation flow control mess def. ',
     1     40,1,1)
C
C   The Lcodes 'NT1 AMPS' and NT1 PART, etc. no longer used. Delete old Lcodes.
      CALL DELR (1,'NT1 AMPS')
      CALL DELR (1,'NT2 AMPS')
      CALL DELR (1,'NT3 AMPS')
      CALL DELR (1,'NT4 AMPS')
      CALL DELR (1,'NT5 AMPS')
      CALL DELR (1,'NT6 AMPS')
C
      CALL DELR (2,'NT1 PART')
      CALL DELR (2,'NT2 PART')
      CALL DELR (2,'NT3 PART')
      CALL DELR (2,'NT4 PART')
      CALL DELR (2,'NT5 PART')
      CALL DELR (2,'NT6 PART')
C
C   Nutation from data base no longer used, remove old Lcodes
      CALL DELR (2,'DPSI    ')
      CALL DELR (2,'DEPS    ')
C
C  Lcodes for the nutation values
C
C   If using IERS 1996 Nutation:
      IF ( KNUTC .eq. 0) then 
       CALL ADDR (2,'NUT 1996','IERS 1996 Nut. - Dpsi,Deps&rates',
     *            2, 2, 1 )
       CALL ADDR (2,'NUT WAHR','Wahr nut vals  - Dpsi,Deps&rates',
     *            2, 2, 1 )
       CALL ADDR (2,'WAHRCONT','1996 Nut to Wahr Nut Contributn ',
     *            2, 1, 1 )
       CALL ADDR (2,'FUNDARGS','Fundamental args and derivatives',
     *            5, 2, 1 )
      Endif
C
C   If using 1980 (Wahr) Nutation:
      IF ( KNUTC .eq. 2) then 
       CALL ADDR (2,'NUT WAHR','Wahr nut vals  - Dpsi,Deps&rates',
     *            2, 2, 1 )
       CALL ADDR (2,'FUNDARGS','Fundamental args and derivatives',
     *            5, 2, 1 )
C     And remove misleading Lcodes
       CALL DELR (2,'NUT 1996')
       CALL DELR (2,'WAHRCONT')
      Endif
C
C   ADD for nutation partials.
      CALL ADDR (2,'NUT PART','Nutation partial derive. def.   ',
     1     2, 2, 1 )
C
C   ADD for geodesic nutation contribution 
      CALL ADDR (2,'GDNUTCON','Geodesic Nutation contribution  ',
     1     2, 1, 1 )
C
      RETURN
      END  
C
C****************************************************************************
      SUBROUTINE NUTI
      IMPLICIT None
C
C 3.    NUTI
C
C 3.1   NUTI PROGRAM SPECIFICATION
C
C 3.1.1 NUTI IS THE NUTATION MODULE INPUT AND INITIALIZATION SECTION.
C       MESSAGES ARE PUT IN THE HEADER TO INDICATE PROGRAM FLOW.
C       KNUTC = 0 - IERS 1996 NUTATION TO BE USED (DEFAULT)
C               1 - NO NUTATION TO BE USED
C               2 - IAU 1980 (WAHR) NUTATION TO BE USED
C 
C 3.2   NUTI PROGRAM INTERFACE
C 
C 3.2.1 CALLING SEQUENCE - NONE 
C 
C 3.2.2 COMMON BLOCKS USED -
      INCLUDE 'ccon.i'               
C      VARIABLES 'FROM': 
C       1. KNUTC - THE NUTATION MODULE FLOW CONTROL FLAG
C       2. KNUTD - THE NUTATION MODULE DEBUG CONTROL FLAG 
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C     VARIABLES 'FROM': 
C       1.  CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER 
C                      ARCSECOND.  (RAD/ARCSEC) 
C 
C 3.2.3 PROGRAM SPECIFICATIONS -
      Real*8  APAMPS(2,2)
      Integer*4 N, I 
      INTEGER*2      LNUTM(40),  LON(40),  LOFF(40),  LWAHR(40)
      CHARACTER*40  C_LNUTM(2), C_LON(2), C_LOFF(2),  C_WAHR(2)
      EQUIVALENCE (LNUTM,C_LNUTM), (LON,C_LON), (LOFF,C_LOFF),
     .            (LWAHR,C_WAHR )
C
      DATA C_LNUTM /
     . 'NUTATION MODULE - Version # 6, last modi',
     . 'fication - 98NOV12, D. Gordon, GSFC     '/
C
      DATA C_LON  /
     . 'Nutation module is turned on. IERS 1996 ',
     . 'model used.                             '/
C
      DATA C_WAHR/
     . 'Nutation module is turned on. Wahr model',
     . ' used.                                  '/
C
      DATA C_LOFF /
     . 'Nutation module is turned off. This turn',
     . 's turns off the nut in obliq in DIURNAL.'/
C
C 3.2.4 DATA BASE ACCESS -
C           'PUT' VARIABLES:
C              1.  LNUTM(40)   - THE NUTATION MODULE TEXT MESSAGE.
C              2.  LON(40)     - THE NUTATION MODULE TURNED ON MESSAGE.
C              3.  LOFF(40)    - THE NUTATION MODULE TURNED OFF MESSAGE.
C              4.  LNDB(40)    - THE NUTATION FROM DATA BASE MESSAGE.
C            ACCESS CODES:
C              1.  'NUT MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 NUTATION MODULE TEXT MESSAGE. 
C              2.  'NUT CFLG'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 NUTATION MODULE FLOW CONTROL MESSAGE. 
C 
C 3.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL 
C             CALLED SUBROUTINES: PUTA
C 
C 3.2.7 CONSTANTS USED - NONE 
C 
C 3.2.8 PROGRAM VARIABLES - NONE
C 
C 3.2.9 PROGRAMMER - DALE MARKHAM   01/13/77  
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 03/15/78
C                    BRUCE SCHUPLER 02/01/79
C                    BRUCE SCHUPLER 06/06/79
C                    CHOPO MA       08/04/81
C                    GEORGE KAPLAN  04/24/84
C                    DAVID GORDON   11/13/84  (REDIMENSIONED NT# AMPS)
C                    Jim Ryan 89.06.30 Documetation clean up.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.15 Converted to Implicit None.
C                    David Gordon 95.09.27 Conversion of X(9,120) nutation table
C                                 coefficients to double precision removed - no
C                                 longer needed.
C                    David Gordon 98.02.03 Changed KNUTC definitions. Default 
C                                 is now IERS 1996 Nutation. Removed 
C                                 computation and PUT's of 'NT1 AMPS', etc.
C
C     NUTI PROGRAM STRUCTURE
C
C     PUT the nutation module text message.
      CALL PUTA ('NUT MESS      ',LNUTM, 40, 1, 1 )
C
C     PUT the nutation module flow control messaged depending on KNUTC.
      IF (KNUTC .EQ. 0) CALL PUTA ('NUT CFLG      ',LON,40,1,1)
      IF (KNUTC .EQ. 2) CALL PUTA ('NUT CFLG      ',LWAHR,40,1,1)
      IF (KNUTC .EQ. 1) CALL PUTA ('NUT CFLG      ',LOFF,40,1,1)
C
C     Normal conclusion.
      RETURN
      END 
C*****************************************************************************
C
      SUBROUTINE NUTG (cent, fa, fad, TSKIP, DEPS, DPSI, EPS, EPSMNR,
     *                 RN, NUTDIF, FUKU) 
      IMPLICIT None
C 
C 4.    NUTG
C 
C 4.1   NUTG PROGRAM SPECIFICATION
C 
C 4.1.1 NUTG IS THE NUTATION MODULE GEOMETRY SECTION. IT CALCULATES THE NUTATION
C       PORTION OF THE COMPLETE CRUST FIXED TO J2000.0 ROTATION MATRIX AND ITS
C       CT TIME DERIVATIVE. THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS CT TIME
C       DERIVATIVE ARE ALSO CALCULATED. 
C 
C 4.1.2 RESTRICTIONS - NONE 
C 
C 4.1.3 REFERENCES - 
C       1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN EPHEMERIS AND 
C          NAUTICAL ALMANAC", P. 41-45, 98. 
C       2) 'SPHERICAL AND PRACTICAL ASTRONOMY AS APPLIED TO GEODESY', I. 
C          MUELLER, 1969, P. 68-75. (NOTE: THE REFERENCE IN MUELLER REFERS
C          TO THE COMPUTATION OF THE NUTATION PORTION OF THE COMPLETE 
C          J2000.0 TO CRUST FIXED ROTATION MATRIX. HOWEVER, CALC REQUIRES
C          THE TRANSPOSE OF THIS MATRIX. CARE MUST BE TAKEN WHEN COMPARING
C          THIS REFERENCE TO THE FOLLOWING PROGRAM.) 
C       3) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE PRECESSIONAL QUANTITIES
C          BASED ON THE IAU (1976) SYSTEM OF ASTRONOMICAL CONSTANTS, ASTRON.
C          ASTROPHYS. 58, 1-16, 1977. 
C       4) IERS Technical Note 21, IERS Conventions (1996), page 37. 
C 
C 4.2   NUTG PROGRAM INTERFACE
C 
C 4.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C              1. cent      -  The number of Julian centuries elapsed since the
C                              epoch January 1.5, 2000. (centuries)
C              2. fa (5)    -  The fundamental arguments (arcsec)
C              3. fad(5)    -  The CT time derivatives of the fundamental 
C                              arguments. (arcsec/century)
C              4. TSKIP     -  Skip nutation recomputation if TSKIP=1.
C
C           OUTPUT VARIABLES: 
C              1. DEPS(2)   -  THE NUTATION IN OBLIQUITY AND ITS CT TIME 
C                              DERIVATIVE COMPUTED FROM THE IERS 1996 NUTATION 
C                              MODEL OR FROM THE IAU 1980 (WAHR) NUTATION
C                              MODEL. (RAD, RAD/SEC)
C              2. DPSI(2)   -  THE NUTATION IN LONGITUDE AND ITS CT TIME
C                              DERIVATIVE COMPUTED FROM THE IERS 1996 NUTATION 
C                              MODEL OR FROM THE IAU 1980 (WAHR) NUTATION
C                              MODEL. (RAD, RAD/SEC)
C              3. EPS(2)    -  THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS CT
C                              TIME DERIVATIVE. (RAD, RAD/SEC) 
C              4. EPSMNR    -  MEAN OBLIQUITY AT REFERENCE EPOCH J2000. (RAD) 
C              5. RN(3,3,2) -  THE NUTATION PORTION OF THE COMPLETE CRUST FIXED
C                              TO J2000.0 ROTATION MATRIX AND ITS CT TIME
C                              DERIVATIVE. (UNITLESS, 1/SEC) 
C              6. NUTDIF(2,2)- Differences between 1996 and 1980 nutation 
C                              values. First variable runs over Dpsi and Deps,
C                              second runs over delay and rate. (rad, rad/sec)
C              7. FUKU(2)    - Correction in longitude for the effect of 
C                              geodesic nutation, and its time derivative,
C                              using Fukushima equation from p. 37 of the 
C                              IERS Conventions (1996). (radians, radians/sec)
C 
C 4.2.2 COMMON BLOCKS USED -
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C           VARIABLES 'FROM':
C              1. CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER ARCSECOND.
C                            (RAD/ARCSEC)
C              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
C                            COORDINATE TIME DAY.  (SEC/DAY) 
C 
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C            VARIABLES 'FROM':
C              1. CENTJ  -  THE NUMBER OF COORDINATE TIME DAYS PER JULIAN
C                           CENTURY. (DAYS/CENTURY) 
C              2. EC(4)  -  THE CONSTANTS APPEARING IN TERMS 1-4 IN THE
C                           CALCULATION OF THE MEAN OBLIQUITY OF THE ECLIPTIC.
C                           (ARCSEC) (SEE REFERENCES)
C 
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KNUTC - THE NUTATION MODULE FLOW CONTROL FLAG. 
C                         KNUTC = 0 TO TURN ON THE IERS 1996 NUTATION MODEL 
C                         KNUTC = 1 TO TURN OFF THE NUTATION MODULE
C                         KNUTC = 2 TO TURN ON THE IAU 1980 (WAHR) NUTATION
C                                   MODEL
C              2. KNUTD - THE NUTATION MODULE DEBUG OUTPUT FLAG. 
C 
C************************************************************************
C 4.2.3 PROGRAM SPECIFICATIONS -
      REAL*8 cent, fa(5), fad(5), fundarg(10)
      Real*8 DEPS(2), DPSI(2), EPS(2), EPSMN(2), R1N(3,3),
     *       R2N(3,3), R3N(3,3), RN(3,3,2), RN1(3,3), RN2(3,3), 
     *       RN3(3,3), RN4(3,3), RN5(3,3), RN6(3,3), NUTDIF(2,2),
     *       DTEMP(4), EPSMNR, DCENT, NUTWAHR(4), NUT1996(4), FUKU(2)
      Real*8 dpsi_ls(2), deps_ls(2), dpsi_plan(2), deps_plan(2),
     *       dpsi_fcn(2) , deps_fcn(2) , dpsi_prec(2), deps_prec(2),
     *       dpsi_tot(2) , deps_tot(2), Lprime, dLprime 
      Integer*4 TSKIP, M, N
C 
C 4.2.4 DATA BASE ACCESS -
C       'PUT' VARIABLES - 
C          1. NUTWAHR(4) - Variable used to hold the Wahr DPSI and DEPS and
C                          their time derivatives. (rad, rad/sec)
C          2. NUT1996(4) - Variable used to hold the IERS 1996 DPSI and DEPS 
C                          and their time derivatives. (rad, rad/sec)
C          3. fundarg(10)- Variable used to hold the fundamental arguments and
C                          their time derivatives. (arc-sec, arc-sec/century)
C
      SAVE  NUTWAHR, NUT1996, fundarg, EPSMN
C 
C 4.2.5 EXTERNAL INPUT/OUTPUT - 
C           1. POSSIBLE DEBUG OUTPUT 
C 
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVR 
C             CALLED SUBROUTINES: DROTT, MADD3, MMUL3, ROTAT, PUT4, NUTW
C 
C 4.2.7 CONSTANTS USED - CONVDS, CENTJ, DJ2000, EC(4), SECDAY 
C 
C 4.2.8 PROGRAM VARIABLES - 
C         1. DCENT     -  THE CT TIME DERIVATIVE OF THE VARIABLE CENT. 
C                         (CENTURIES/SEC)
C         2. R1N(3,3)  -  THE FIRST TERM OF THE CT TIME DERIVATIVE OF THE
C                         NUTATION PORTION OF THE COMPLETE CRUST FIXED TO 
C                         J2000.0 ROTATION MATRIX. (1/SEC)
C         3. R2N(3,3)  -  THE SECOND TERM OF THE CT TIME DERIVATIVE OF THE
C                         NUTATION PORTION OF THE COMPLETE CRUST FIXED TO
C                         J2000.0 ROTATION MATRIX. (1/SEC)
C         4. R3N(3,3)  -  THE THIRD TERM OF THE CT TIME DERIVATIVE OF THE
C                         NUTATION PORTION OF THE COMPLETE CRUST FIXED TO 
C                         J2000.0 ROTATION MATRIX. (1/SEC)
C         5. RN1(3,3)  -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                         THE TRUE EQUINOCTIAL LINE BY AN ANGLE EQUAL TO
C                         EPSMN(1)+DEPS(1). (UNITLESS) 
C         6. RN2(3,3)  -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                         THE MEAN ECLIPTIC POLE BY AN ANGLE EQUAL TO DPSI(1).
C                         (UNITLESS) 
C         7. RN3(3,3)  -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                         THE MEAN EQUINOCTIAL LINE BY AN ANGLE EQUAL TO
C                         -EPSMN(1). (UNITLESS) 
C         8. RN4(3,3)  -  THE CT TIME DERIVATIVE OF THE ROTATION MATRIX RN1. 
C                         (1/SEC) 
C         9. RN5(3,3)  -  THE CT TIME DERIVATIVE OF THE ROTATION MATRIX RN2. 
C                         (1/SEC) 
C        10. RN6(3,3)  -  THE CT TIME DERIVATIVE OF THE ROTATION  MATRIX RN3.
C                         (1/SEC) 
C        11. EPSMN(2)  -  MEAN OBLIQUITY OF DATE AND CT TIME DERIVATIVE.
C                         (RAD, RAD/SEC)
C        12. NUTWAHR(4)-  Array to hold the IAU 1980 (Wahr) Dpsi, Deps, and
C                         their time derivatives (rad, rad/sec)
C        13. NUT1996(4)-  Array to hold the IERS 1996 Dpsi, Deps, and
C                         their time derivatives (rad, rad/sec)
C        14. fundarg(10)- Variable used to hold the fundamental arguments and
C                         their time derivatives. (arc-sec, arc-sec/century)
C
C 4.2.9 PROGRAMMER
C        77.01.13  DALE MARKHAM
C        77.07.12  PETER DENATALE 
C        78.03.15  BRUCE SCHUPLER
C        79.02.01  BRUCE SCHUPLER
C        79.06.06  BRUCE SCHUPLER 
C        81.08.04  CHOPO MA 
C        81.08.24  JIM RYAN 
C        89.06.30  Jim Ryan: Documetation clean up.
C        89.12.12  Jim Ryan: UNIX-like database interface implemented
C        93.09.01  Norbert Zacharias: put parts of NUTG,NUTW into NUTFA
C                  for equation of equinox update EQE 
C        94.04.15  David Gordon: Converted to Implicit None.
C        98.02.03  D. Gordon: Default changed to IERS 1996 Nutation. Call to
C                  Subroutine KSV_1996 (from Tom Herring, modified at USNO) 
C                  added. Added PUT for 'NUT 1996'. Other minor mods.
C        98.11.12  D. Gordon: Added code to compute the correction in longitude
C                  for the effect of geodesic nutation, according to Fukushima
C                  (IERS Conventions (1996), page 37).
C
C  NUTG PROGRAM STRUCTURE
C
C  Compute the CT time derivative of CENT.
      DCENT = 1.D0 / ( CENTJ * SECDAY )
C
C  Compute the nutation in obliquity and longitude.
C
      IF (KNUTC.eq.0 .or. KNUTC.eq.2) THEN  
C   Compute Wahr nutation (skip if same time as previous observation)
       IF(TSKIP.EQ.1) GO TO 150
        CALL NUTW (CENT, fa, fad, DPSI, DEPS)
         DEPS (1) = DEPS(1) * CONVDS
         DEPS (2) = DEPS(2) * CONVDS
         DPSI (1) = DPSI(1) * CONVDS
         DPSI (2) = DPSI(2) * CONVDS
        NUTWAHR(1) = DPSI(1)
        NUTWAHR(2) = DEPS(1)
        NUTWAHR(3) = DPSI(2)
        NUTWAHR(4) = DEPS(2)
  150  CONTINUE
        CALL PUT4 ('NUT WAHR      ',NUTWAHR,2,2,1)
C
C  Put fundamental arguments in data base
        Do 101 m=1,5
         fundarg(m)   = fa(m)
         fundarg(m+5) = fad(m)
 101    Continue
        CALL PUT4 ('FUNDARGS      ',fundarg,5,2,1)
C
      ENDIF
C
C  Default - compute and use IERS 1996 Nutations (skip if same time)
      IF (KNUTC.eq.0) THEN  
       IF(TSKIP.EQ.1) GO TO 151
        CALL KSV_1996_3(CENT, fa, fad, dpsi_ls, deps_ls, dpsi_plan, 
     *       deps_plan, dpsi_fcn , deps_fcn , dpsi_prec, deps_prec,
     *       dpsi_tot , deps_tot )
C          dpsi_ls(1)   = dpsi_ls(1)  / 1000.D0 * CONVDS
C          dpsi_ls(2)   = dpsi_ls(2)  / 1000.D0 * CONVDS
C          deps_ls(1)   = deps_ls(1)   / 1000.D0 * CONVDS
C          deps_ls(2)   = deps_ls(2)   / 1000.D0 * CONVDS
C          dpsi_plan(1) = dpsi_plan(1)  / 1000.D0 * CONVDS
C          dpsi_plan(2) = dpsi_plan(2)  / 1000.D0 * CONVDS
C          deps_plan(1) = deps_plan(1)  / 1000.D0 * CONVDS
C          deps_plan(2) = deps_plan(2)  / 1000.D0 * CONVDS
C          dpsi_fcn(1)  = dpsi_fcn(1)  / 1000.D0 * CONVDS
C          dpsi_fcn(2)  = dpsi_fcn(2)  / 1000.D0 * CONVDS
C          deps_fcn(1)  = deps_fcn(1)  / 1000.D0 * CONVDS
C          deps_fcn(2)  = deps_fcn(2)  / 1000.D0 * CONVDS
C          dpsi_prec(1) = dpsi_prec(1)  / 1000.D0 * CONVDS
C          dpsi_prec(2) = dpsi_prec(2)  / 1000.D0 * CONVDS
C          deps_prec(1) = deps_prec(1)  / 1000.D0 * CONVDS
C          deps_prec(2) = deps_prec(2)  / 1000.D0 * CONVDS
          dpsi_tot(1)  = dpsi_tot(1)  / 1000.D0 * CONVDS
          dpsi_tot(2)  = dpsi_tot(2)  / 1000.D0 * CONVDS
          deps_tot(1)  = deps_tot(1)  / 1000.D0 * CONVDS
          deps_tot(2)  = deps_tot(2)  / 1000.D0 * CONVDS
        DEPS (1) = deps_tot(1) 
        DEPS (2) = deps_tot(2) 
        DPSI (1) = dpsi_tot(1) 
        DPSI (2) = dpsi_tot(2) 
        NUT1996(1) = DPSI(1)
        NUT1996(2) = DEPS(1)
        NUT1996(3) = DPSI(2)
        NUT1996(4) = DEPS(2)
c       print *,'1996 DPSI, DEPS ', DPSI, DEPS
  151      CONTINUE
        CALL PUT4 ('NUT 1996      ',NUT1996,2,2,1)
C
C Compute differences between 1996 and 1980 nutation values
        NUTDIF(1,1) = NUTWAHR(1) - NUT1996(1)
        NUTDIF(2,1) = NUTWAHR(2) - NUT1996(2)
        NUTDIF(1,2) = NUTWAHR(3) - NUT1996(3)
        NUTDIF(2,2) = NUTWAHR(4) - NUT1996(4)
C
      ENDIF
C
C-----------------------------------------------------------------------
C  New feature: Compute the effect of geodesic nutation. From Fukushima 
C   via the IERS Conventions (1996), page 37.
C    Mean anomaly of the sun and its time derivative in radians and rad/sec.
      IF(TSKIP.EQ.1) GO TO 165
        Lprime = FA(2)*CONVDS
       dLprime = FAD(2)*CONVDS/CENTJ/SECDAY
C  Longitude correction
       Fuku(1) = ( -0.000153D0 * DSIN(Lprime) - 
     *              0.000002D0 * DSIN(2.D0*Lprime) ) * CONVDS
       Fuku(2) = ( -0.000153D0 * DCOS(Lprime) * dLprime -
     *              0.000002D0 * 2.D0*DCOS(2.D0*Lprime) * dLprime ) 
     *            * CONVDS
 165  Continue
C-----------------------------------------------------------------------
C
      IF (KNUTC .EQ. 1) THEN 
C   Option to turn off nutation.
        DEPS(1) = 0.0D0
        DEPS(2) = 0.0D0
        DPSI(1) = 0.0D0
        DPSI(2) = 0.0D0
      ENDIF 
C
C   Calculate the mean and true obliquities of the ecliptic and their CT time
C   derivatives in units of radians and radians per second.
C
        IF(TSKIP.EQ.1) GO TO 625
C   Compute the mean obliquity and its CT time derivative.
      EPSMN(1) = ( EC(1)
     1           + EC(2) * CENT
     2           + EC(3) * CENT**2
     3           + EC(4) * CENT**3 ) * CONVDS
      EPSMN(2) = ( EC(2)
     1           + EC(3) * ( 2.D0 * CENT )
     2           + EC(4) * ( 3.D0 * CENT**2 ) )
     3         * DCENT * CONVDS
C
C   Compute the true obliquity and its CT time derivative.
      EPS(1) = EPSMN(1)  +  DEPS(1)
      EPS(2) = EPSMN(2)  +  DEPS(2)
C
C   Pass the mean obliquity of the reference epoch J2000.0.
      EPSMNR = EC(1) * CONVDS
C
C   Construct the nutation portion of the complete crust fixed to J2000.0
C   rotation matrix if nutation is turned on.
      IF ( KNUTC .NE. 1 ) GO TO 400
      GO TO 599
  400 CONTINUE
C
C   Construct the rotation matrix which rotates about the true equinoctial line
C   by an angle equal to [EPSMN(1)+DEPS(1)].
      CALL ROTAT ( EPSMN(1)+DEPS(1), 1, RN1 )
C
C   Construct the rotation matrix which rotates about the mean ecliptic pole by
C   an angle equal to DPSI(1).
      CALL ROTAT ( DPSI(1), 3, RN2 )
C
C   Construct the rotation matrix which rotates about the mean equinoctial line
C   by an angle equal to -EPSMN(1).
      CALL ROTAT ( -EPSMN(1), 1, RN3 )
C
C   Complete the construction of the nutation matrix.
      CALL MMUL3 ( RN3, RN2, RN1, RN(1,1,1) )
C
C   Compute the CT time derivative of the nutation portion of the complete 
C   crust fixed to J2000.0 rotation matrix.
C
C   Construct the CT time derivative of the rotation matrix RN1.
      CALL DROTT ( EPSMN(1)+DEPS(1), EPSMN(2)+DEPS(2), 1, RN4 )
C
C   Construct the CT time derivative of the rotation matrix RN2.
      CALL DROTT ( DPSI(1), DPSI(2), 3, RN5 )
C
C   Construct the CT time derivative of the rotation matrix RN3.
      CALL DROTT ( -EPSMN(1), -EPSMN(2), 1, RN6 )
C
C   Complete the construction of the CT time derivative of the nutation matrix.
C
C   Compute the three terms necessary for the calculation.
      CALL MMUL3 ( RN6, RN2, RN1, R1N )
      CALL MMUL3 ( RN3, RN5, RN1, R2N )
      CALL MMUL3 ( RN3, RN2, RN4, R3N )
C
C  Add the three terms to complete the calculation.
      CALL MADD3 ( R1N, R2N, R3N, RN(1,1,2) )
C
  599 CONTINUE
C
C  Check KNUTC to determine if the nutation module is to be turned off.
C     IMPORTANT NOTE ----- Turning off the nutation module will also turn off
C     the nutation in obliquity portion of subroutine DIURNAL.  BEWARE!
C
      IF ( KNUTC .EQ. 1 )  THEN     
C   Set the position portion of the nutation matrix to the identity matrix.
       CALL ROTAT ( 0.D0, 3, RN(1,1,1) )
C   Set the velocity portion of the nutation matrix to 0.0D0.
       DO 600 N=1,3
         DO 600 M=1,3
          RN(M,N,2) = 0.0D0
  600  CONTINUE
      ENDIF
C
  625  CONTINUE
C
C   Check KNUTD for debug output.
  700 IF ( KNUTD .EQ. 0 ) GO TO 800
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for subroutine NUTG." )
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CENT    ',CENT
      WRITE(6,8)' CENTJ   ',CENTJ
      WRITE(6,8)' CONVDS  ',CONVDS
      WRITE(6,8)' DCENT   ',DCENT
      WRITE(6,8)' EC      ',EC
      WRITE(6,8)' SECDAY  ',SECDAY
      WRITE(6,8)' fundarg ',fundarg
      WRITE(6,8)' NUTWAHR ',NUTWAHR
      WRITE(6,8)' NUT1996 ',NUT1996
      WRITE(6,8)' NUTDIF  ',NUTDIF 
      WRITE(6,8)' dpsi_ls,   deps_ls   ', dpsi_ls,   deps_ls  
      WRITE(6,8)' dpsi_plan, deps_plan ', dpsi_plan, deps_plan
      WRITE(6,8)' dpsi_prec, deps_prec ', dpsi_prec, deps_prec
      WRITE(6,8)' dpsi_tot,  deps_tot  ', dpsi_tot,  deps_tot
      WRITE(6,8)' FUKU    ',FUKU   
      WRITE(6,8)' Lprime, dLprime ', Lprime, dLprime   
       IF(TSKIP.EQ.1) GO TO 725
      WRITE(6,8)' R1N     ',R1N
      WRITE(6,8)' R2N     ',R2N
      WRITE(6,8)' R3N     ',R3N
      WRITE(6,8)' RN1     ',RN1
      WRITE(6,8)' RN2     ',RN2
      WRITE(6,8)' RN3     ',RN3
      WRITE(6,8)' RN4     ',RN4
      WRITE(6,8)' RN5     ',RN5
      WRITE(6,8)' RN6     ',RN6
  725  CONTINUE
C
      WRITE ( 6, 9200 )   DEPS, DPSI, EPS, EPSMN, RN, 
     1                    EPSMNR
 9200 FORMAT (1X, 
     1            "DEPS  = ", 2 ( D30.16, 10X ), /, 1X,
     2            "DPSI  = ", 2 ( D30.16, 10X ), /, 1X,
     3            "EPS   = ", 2 ( D30.16, 10X ), /, 1X,
     4            "EPSMN = ", 2 ( D30.16, 10X ), /, 1X,
     5            "RN    = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     9            "EPSMNR= ", D30.16, /, 1X)
C
C   8.    NORMAL PROGRAM CONCLUSION.
C
  800 RETURN
      END 
C
C****************************************************************************
        SUBROUTINE NUTP (CFBASE, DEPS, DPSI, EPS, GAST, RDNP,
     *                   RN, RP, RS, RW, STAR, DNUTP)
        IMPLICIT None
C
C 5.    NUTP
C 
C 5.1   NUTP PROGRAM SPECIFICATION
C 
C 5.1.1 NUTP IS THE NUTATION MODULE PARTIAL DERIVATIVES SECTION. NUTP COMPUTES
C       THE PARTIAL DERIVATIVES OF THE DELAY AND OF THE DELAY RATE W.R.T. PSI
C       AND EPSILON AT THE EPOCH OF THE OBSERVATION.
C 
C 5.1.2 RESTRICTIONS - NONE 
C 
C 5.1.3 REFERENCES - 1) ROBERTSON, GEODETIC AND ASTROMETRIC MEASUREMENTS
C                       WITH VLBI, NASA X-DOCUMENT X-922-77-228. 
C                    2) KAPLAN, USNO NOTES. 
C 
C 5.2   NUTP PROGRAM INTERFACE
C 
C 5.2.1 CALLING SEQUENCE -
C 
C          INPUT VARIABLES:
C             1. CFBASE(3) -  THE CRUST FIXED BASELINE VECTOR. (M) 
C             2. DEPS(2)   -  THE NUTATION IN OBLIQUITY AND ITS CT TIME
C                             DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM THE
C                             DATA BASE. (RAD, RAD/SEC)
C             3. DPSI(2)   -  THE NUTATION IN LONGITUDE AND ITS CT TIME 
C                             DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM THE
C                             DATA BASE. (RAD, RAD/SEC)
C             4. EPS(2)    -  THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS CT 
C                             TIME DERIVATIVE. (RAD, RAD/SEC) 
C             5. GAST(2)   -  THE GREENWICH APPARENT SIDEREAL TIME AND ITS CT
C                             TIME DERIVATIVE. (RAD, RAD/SEC) 
C             6. RDNP(3,3) -  THE DIURNAL POLAR MOTION PORTION OF THE COMPLETE
C                             CRUST FIXED TO J2000.0 ROTATION MATRIX. (UNITLESS)
C             7. RN(3,3,2) -  THE NUTATION PORTION OF THE COMPLETE CRUST FIXED
C                             TO J2000.0 ROTATION MATRIX and it first time
C                             derivative. (unitless, 1/sec)
C             8. RP(3,3,2) -  THE PRECESSION PORTION OF THE COMPLETE CRUST 
C                             FIXED TO J2000.0 ROTATION MATRIX and it first time
C                             derivative. (unitless, 1/sec)
C             9. RS(3,3,3) -  THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST 
C                             FIXED TO J2000.0 ROTATION MATRIX AND ITS FIRST
C                             two CT TIME DERIVATIVEs. (UNITLESS, 1/SEC) 
C            10. RW(3,3,2) -  THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED
C                             TO J2000.0 ROTATION MATRIX and its time 
C                             derivative. (unitless, 1/sec)
C            11. STAR(3)   -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C 
C          OUTPUT VARIABLES: 
C             1. DNUTP(2,2) - PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
C                             RATE W.R.T DPSI AND DEPS. (SEC/RAD, SEC/SEC/RAD)
C 
C 5.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C           VARIABLES 'FROM':
C              1. KNUTC  -  THE NUTATION MODULE FLOW CONTROL FLAG.
C              2. KNUTD  -  THE NUTATION MODULE DEBUG OUTPUT FLAG.
C
      INCLUDE 'cphys.i'
C           VARIABLES 'FROM':
C              1.  VLIGHT    -  THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C
C**   Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
C**   Integer*4 NOT, NOP, IDP(6)
C**   COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C              VARIABLES 'FROM':
C 
C 5.2.3 PROGRAM SPECIFICATIONS -
      Real*8 CFBASE(3), DEPS(2), DPSI(2), EPS(2), GAST(2), RDNP(3,3),
     .       RN(3,3,2), RP(3,3,2), RS(3,3,3), RW(3,3,2), STAR(3)
      Real*8 DNUTP(2,2), DRNDPS(3,3), DRNDEP(3,3), 
     1       DRSDPS(3,3), DRSDEP(3,3), DRS1DP(3,3), DRS1DE(3,3),
     2       DBLDEP(3,2), DBLDPS(3,2), TMTRX(3,3,3) 
      Real*8 C, S, CT, ST, SINARG, COSARG, DOTP
      Integer*4 N
C 
C 5.2.4 DATA BASE ACCESS -
C        'PUT' VARIABLES:  
C           1. DNUTP(2,2)   -  PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
C                              RATE W.R.T DPSI AND DEPS. (SEC/RAD, SEC/SEC/RAD)
C        ACCESS CODES: 
C           1. 'NUT PART'   -  THE DATA BASE ACCESS CODE FOR THE NUTATION MODULE
C                              PARTIAL DERIVATIVES ARRAY DNUTP.
C 
C 5.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE ERROR OUTPUT 
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVR 
C             CALLED SUBROUTINES: DCOS, DSIN, MMUL5, MADD2, PUT4, VECRT 
C 
C 5.2.7 CONSTANTS USED - VLIGHT 
C 
C 5.2.8 PROGRAM VARIABLES - 
C           1. DRNDPS(3,3) - THE PARTIAL DERIVATIVE OF THE NUTATION
C                            MATRIX RN WITH RESPECT TO PSI 
C           2. DRNDEP(3,3) - THE PARTIAL DERIVATIVE OF THE NUTATION
C                            MATRIX RN WITH RESPECT TO EPSILON 
C           3. DRSDPS(3,3) - THE PARTIAL DERIVATIVE OF THE DIURNAL 
C                            MATRIX RS WITH RESPECT TO PSI 
C           4. DRSDEP(3,3) - THE PARTIAL DERIVATIVE OF THE DIURNAL 
C                            MATRIX RS WITH RESPECT TO EPSILON 
C           5. DRS1DP(3,3) - THE PARTIAL DERIVATIVE OF THE DIURNAL 
C                            MATRIX DERIVATIVE RS WITH RESPECT TO P
C           6. DRS1DE(3,3) - THE PARTIAL DERIVATIVE OF THE DIURNAL
C                            MATRIX DERIVATIVE RS WITH RESPECT TO E
C           7. DBLDPS(3,2) - THE PARTIAL DERIVATIVE OF THE BASELINE VECTOR AND
C                            ITS TIME DERIVATIVE WITH RESPECT TO PSI
C           8. DBLDEP(3,2) - THE PARTIAL DERIVATIVE OF THE BASELINE VECTOR AND
C                            ITS TIME DERIVATIVE WITH RESPECT TO EPSILON
C           9. TMTRX(3,3,3)- TEMPORARY STORAGE FOR MATRIX PARTIALS
C
C 5.2.9 PROGRAMMER - DOUG ROBERTSON 09/07/82
C                    GEORGE KAPLAN 04/24/84
C                    DAVID GORDON 11/13/84 REDIMENSIONED NUT# PART AND DNUTPT
C                    Jim Ryan     89:10:05 CPHYS common made an include file
C                    Jim Ryan     89.12.12 UNIX-like database interface
C                                 implimented.
C                    David Gordon 94.04.15 Converted to Implicit None.
C                    David Gordon 95.12.11 Changed RW(3,3) to RW(3,3,2).
C                    David Gordon 95.12.12 RS improperly dimensioned as 
C                                 (3,3,2). Changed to (3,3,3). RP and RN
C                                 improperly dimensioned as (3,3). Changed to
C                                 (3,3,2). 
C                    David Gordon 98.02.03  Removed DNUTPT and computation 
C                                 and PUT's of 'NT1 PART', 'NT2 PART', etc.
C
C 5.3   NUTP PROGRAM STRUCTURE
C
C   1. CONSTRUCT THE PARTIAL DERIVATIVES OF THE NUTATION MATRIX WITH RESPECT TO
C      PSI AND EPSILON.
C
          C = DCOS(EPS(1))
          S = DSIN(EPS(1))
          CT = DCOS(GAST(1))
          ST = DSIN(GAST(1))
C
          DRNDPS(1,1) =  0.0D0
          DRNDPS(2,1) = -C
          DRNDPS(3,1) = -S
          DRNDPS(1,2) =  C
          DRNDPS(2,2) =  0.0D0
          DRNDPS(3,2) =  0.0D0
          DRNDPS(1,3) =  S
          DRNDPS(2,3) =  0.0D0
          DRNDPS(3,3) =  0.0D0
C 
          DRNDEP(1,1) =  0.0D0
          DRNDEP(2,1) =  DPSI(1)*S
          DRNDEP(3,1) = -DPSI(1)*C
          DRNDEP(1,2) = -DPSI(1)*S
          DRNDEP(2,2) =  0.0D0
          DRNDEP(3,2) = -1.0D0
          DRNDEP(1,3) =  DPSI(1)*C
          DRNDEP(2,3) =  1.0D0
          DRNDEP(3,3) =  0.0D0
C 
C   2. CONSTRUCT THE PARTIAL DERIVATIVES OF THE DIURNAL SPIN MATRIX WITH
C      RESPECT TO PSI AND EPSILON. 
C 
          DRSDPS(1,1) = -ST*C 
          DRSDPS(2,1) =  CT*C 
          DRSDPS(3,1) =  0.0D0
          DRSDPS(1,2) = -CT*C 
          DRSDPS(2,2) = -ST*C 
          DRSDPS(3,2) =  0.0D0
          DRSDPS(1,3) =  0.0D0
          DRSDPS(2,3) =  0.0D0
          DRSDPS(3,3) =  0.0D0
C 
          DRSDEP(1,1) =  ST*DPSI(1)*S 
          DRSDEP(2,1) = -CT*DPSI(1)*S 
          DRSDEP(3,1) =  0.0D0
          DRSDEP(1,2) =  CT*DPSI(1)*S 
          DRSDEP(2,2) =  ST*DPSI(1)*S 
          DRSDEP(3,2) =  0.0D0
          DRSDEP(1,3) =  0.0D0
          DRSDEP(2,3) =  0.0D0
          DRSDEP(3,3) =  0.0D0
C 
C   3. CONSTRUCT THE PARTIAL DERIVATIVES OF THE TIME DERIVATIVE OF THE DIURNAL
C      SPIN MATRIX WITH RESPECT TO PSI AND EPSILON. 
C 
          DRS1DP(1,1) = -CT*GAST(2)*C 
          DRS1DP(2,1) = -ST*GAST(2)*C 
          DRS1DP(3,1) =  0.0D0
          DRS1DP(1,2) =  ST*GAST(2)*C 
          DRS1DP(2,2) = -CT*GAST(2)*C 
          DRS1DP(3,2) =  0.0D0
          DRS1DP(1,3) =  0.0D0
          DRS1DP(2,3) =  0.0D0
          DRS1DP(3,3) =  0.0D0
C 
          DRS1DE(1,1) =  CT*DPSI(1)*GAST(2)*S 
          DRS1DE(2,1) =  ST*DPSI(1)*GAST(2)*S 
          DRS1DE(3,1) =  0.0D0
          DRS1DE(1,2) = -ST*DPSI(1)*GAST(2)*S 
          DRS1DE(2,2) =  CT*DPSI(1)*GAST(2)*S 
          DRS1DE(3,2) =  0.0D0
          DRS1DE(1,3) =  0.0D0
          DRS1DE(2,3) =  0.0D0
          DRS1DE(3,3) =  0.0D0
C 
C   4. COMPUTE THE TIME DERIVATIVES OF THE BASELINE VECTOR WITH RESPECT TO PSI
C      AND EPSILON.
C 
          CALL MMUL5(RP(1,1,1),DRNDPS,RS(1,1,1),RDNP,RW(1,1,1),
     .         TMTRX(1,1,1)) 
          CALL MMUL5(RP(1,1,1),RN(1,1,1),DRSDPS,RDNP,RW(1,1,1),
     .         TMTRX(1,1,2)) 
          CALL MADD2(TMTRX(1,1,1),TMTRX(1,1,2),TMTRX(1,1,3))
          CALL VECRT(TMTRX(1,1,3),CFBASE,DBLDPS(1,1)) 
C 
          CALL MMUL5(RP(1,1,1),DRNDEP,RS(1,1,1),RDNP,RW(1,1,1),
     .         TMTRX(1,1,1)) 
          CALL MMUL5(RP(1,1,1),RN(1,1,1),DRSDEP,RDNP,RW(1,1,1),
     .         TMTRX(1,1,2)) 
          CALL MADD2(TMTRX(1,1,1),TMTRX(1,1,2),TMTRX(1,1,3))
          CALL VECRT(TMTRX(1,1,3),CFBASE,DBLDEP(1,1)) 
C 
C   5. NOW THE CORRESPONDING RATE DERIVATIVES.
C 
          CALL MMUL5(RP(1,1,1),DRNDPS,RS(1,1,2),RDNP,RW(1,1,1),
     .         TMTRX(1,1,1))
          CALL MMUL5(RP(1,1,1),RN(1,1,1),DRS1DP,RDNP,RW(1,1,1),
     .         TMTRX(1,1,2)) 
          CALL MADD2(TMTRX(1,1,1),TMTRX(1,1,2),TMTRX(1,1,3))
          CALL VECRT(TMTRX(1,1,3),CFBASE,DBLDPS(1,2)) 
C 
          CALL MMUL5(RP(1,1,1),DRNDEP,RS(1,1,2),RDNP,RW(1,1,1),
     .         TMTRX(1,1,1))
          CALL MMUL5(RP(1,1,1),RN(1,1,1),DRS1DE,RDNP,RW(1,1,1),
     .         TMTRX(1,1,2)) 
          CALL MADD2(TMTRX(1,1,1),TMTRX(1,1,2),TMTRX(1,1,3))
          CALL VECRT(TMTRX(1,1,3),CFBASE,DBLDEP(1,2)) 
C 
C   6. COMPUTE THE DERIVATIVE OF THE DELAY AND THE DELAY RATE WITH
C      RESPECT TO PSI AND EPSILON AND PLACE IN DATA BASE.
C
C   6.1 COMPUTE THE DERIVATIVE OF THE DELAY W.R.T. PSI AND EPSILON.
C
          DNUTP(1,1) = DOTP(DBLDPS(1,1),STAR) / VLIGHT
          DNUTP(2,1) = DOTP(DBLDEP(1,1),STAR) / VLIGHT
C
C   6.2 COMPUTE THE DERIVATIVE OF THE DELAY RATE W.R.T. PSI AND EPSILON.
C
          DNUTP(1,2) = DOTP(DBLDPS(1,2),STAR) / VLIGHT
          DNUTP(2,2) = DOTP(DBLDEP(1,2),STAR) / VLIGHT
C
C   6.3 CALL 'PUT4' TO PLACE THE NUTATION MODULE PARTIAL DERIVATIVES ARRAY INTO
C       THE DATA BASE.
C
          CALL PUT4 ('NUT PART      ', DNUTP , 2, 2, 1 )
C
 800      continue
C
C     CHECK KNUTD TO DETERMINE IF DEBUG OUTPUT IS NECESSARY.
      If(KNUTD.EQ.0) GO TO 900
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine NUTP." )
C     FIRST PRINT THE INPUT VARIABLES
      WRITE ( 6, 9200 )  CFBASE, DEPS, DPSI, EPS, GAST,
     *                        RDNP, RN, RP, RS, RW, STAR
 9200 FORMAT (1X, "CFBASE = ", 3 ( D30.16, 10X ), /, 1X,
     1            "DEPS   = ", 2 ( D30.16, 10X ), /, 1X,
     2            "DPSI   = ", 2 ( D30.16, 10X ), /, 1X,
     3            "EPS    = ", 2 ( D30.16, 10X ), /, 1X,
     4            "GAST   = ", 2 ( D30.16, 10X ), /, 1X,
     5            "RDNP   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     6            "RN     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     7            "RP     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     8            "RS     = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),
     9            "RW     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     *            "STAR   = ", 3 ( D30.16, 10X ) )
C
C     NOW THE LOCAL AND OUTPUT VARIABLES
C
      WRITE (6, 9300) DNUTP, DRNDPS, DRNDEP, DRSDPS, DRSDEP,
     *                DRS1DP, DRS1DE, DBLDEP, DBLDPS, TMTRX
 9300 FORMAT (1X, "DNUTP  = ", 2 ( 2 ( D25.16,  5X ), /, 1X ),
     3            "DRNDPS = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "DRNDEP = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     5            "DRSDPS = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     6            "DRSDEP = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     7            "DRS1DP = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     8            "DRS1DE = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     9            "DBLDEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     *            "DBLDPS = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     *            "TMTRX  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C   9.    NORMAL PROGRAM CONCLUSION.
C
  900 RETURN
      END 
C
C*****************************************************************************
      SUBROUTINE NUTC (NUTDIF, DNUTP, FUKU)
      IMPLICIT None
C
C 1.    NUTC
C 
C 1.1   NUTC PROGRAM SPECIFICATION
C 
C 1.1.1 NUTC computes the contributions to convert from the IERS 1996 nutation 
C       model back to the old IAU 1980 (Wahr) model, and places them in the 
C       data base. 
C 
C 1.2   NUTC PROGRAM INTERFACE
C 1.2.1 CALLING SEQUENCE - NONE 
C         Input variables:
C            1. NUTDIF(2,2) - Nutation difference: IAU1980 minus IERS1996.
C                             First index over psi and epsilon; second
C                             index over difference and derivative of
C                             difference. (radians, radians/sec)
C            2. DNUTP(2,2)  - PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
C                             RATE W.R.T DPSI AND DEPS. (SEC/RAD,
C                             SEC/SEC/RAD)
C            3. FUKU(2)     - Correction in longitude for the effect of
C                             geodesic nutation, and its time derivative,
C                             according to Fukushima. (radians, radians/sec)
C
        Real*8 NUTDIF(2,2), DNUTP(2,2), FUKU(2), Geodesic(2)
C
C 1.2.2 COMMON BLOCKS USED
C
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KNUTC - THE NUTATION MODULE CONTROL FLAG. 
C                 0 - 1996 IERS Nutation (Herring) 
C                 1 - NO NUTATION
C                 2 - 1980 IAU Nutation (Wahr) 
C 
C 1.2.3 PROGRAM SPECIFICATIONS
C
        Real*8 Wahr_cont(2)
C 
C 1.2.4 DATA BASE ACCESS 
C         'PUT' Variables:
C            1. Wahr_cont(2) - Delay and rate contributions to convert back to
C                              the IAU 1980 Nutation (Wahr) model.
C            2. Geodesic(2)  - Delay and rate contributions to correct for the
C                              effect of geodesic nutation in longitude,
C                              according to Fukushima.
C          Access Codes:
C            1. 'WAHRCONT'   - Data base access code for the contribution to
C                              convert to the IAU 1980 Nutation (Wahr) model.
C            2. 'GDNUTCON'   - Data base access code for the contibutions to
C                              correct for geodesic nutation in longitude. 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 
C 1.2.6 SUBROUTINE INTERFACE - NONE 
C        Caller subroutine: DRIVR
C 
C 1.2.7 CONSTANTS USED - NONE 
C 
C 1.2.8 PROGRAM VARIABLES 
C         1. Wahr_cont(2) - Delay and rate contributions to convert back to
C                           the IAU 1980 Nutation (Wahr) model.
C         2. Geodesic(2)  - Delay and rate contributions to correct for the
C                           effect of geodesic nutation in longitude,
C                           according to Fukushima.
C
C 1.2.9 PROGRAMMER - 
C         BRUCE SCHUPLER 11/2/77
C         D. Gordon 94.04.15 Converted to Implicit None.
C         D. Gordon 98.02.03  Documentation and code added to compute Wahr
C                   nutation contribution and place it in the data base.
C         D. Gordon 98.11.12  Computation and PUT of the delay and rate
C                   contributions for the Fukushima geodesic nutation in 
C                   longitude correction, access code 'GDNUTCON'.
C
C 1.3   NUTC PROGRAM STRUCTURE 
C
C      Skip if not using 1996 IERS Nutation
         IF (KNUTC .ne. 0) Go to 999
C
C    Wahr delay contribution
         Wahr_cont(1) = Nutdif(1,1) * Dnutp(1,1) + 
     *                  Nutdif(2,1) * Dnutp(2,1) 
C    Wahr rate contribution
       Wahr_cont(2) = Nutdif(1,1)*Dnutp(1,2) + Nutdif(1,2)*Dnutp(1,1) 
     *              + Nutdif(2,1)*Dnutp(2,2) + Nutdif(2,2)*Dnutp(2,1) 
C  Call PUT4 to place the Wahr contribution in the data base
         CALL PUT4 ('WAHRCONT      ', Wahr_cont, 2, 1, 1 )
C
C  New feature: Compute the effect of geodesic nutation. From FUkishima 
C   via the IERS Conventions (1996), page 37.
        Geodesic(1) = Fuku(1)*DNUTP(1,1)
        Geodesic(2) = Fuku(2)*DNUTP(1,1) + Fuku(1)*DNUTP(1,2)
        CALL PUT4 ('GDNUTCON      ', Geodesic, 2, 1, 1 )
C     print *, 'GDNUTCON ', Geodesic
C
 999  Continue
      RETURN
      END
C
C******************************************************************************
      BLOCK DATA NUTCMB
      IMPLICIT None
C
C 7.    NUTBD
C
C 7.1   NUTBD PROGRAM SPECIFICATION
C
C 7.1.1 NUTBD IS THE NUTATION MODULE BLOCK DATA INITIALIZATION SECTION.
C       THE NUTATION SERIES IS ESTABLISHED HERE. THIS VERSION CONTAINS
C       THE 1980 IAU THEORY OF NUTATION, FROM THE WORK OF J. M. WAHR,
C       SPECIFICALLY, THE WAHR NUTATION SERIES FOR AXIS B OF GILBERT &
C       DZIEWONSKI EARTH MODEL 1066A.
C
C 7.1.3 REFERENCES - 1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN
C                    EPHEMERIS AND NAUTICAL ALMANAC", P. 41-45, 98
C
C                    2) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE
C                    PRECESSIONAL QUANTITIES BASED ON THE IAU (1976)
C                    SYSTEM OF ASTRONOMICAL CONSTANTS,
C                    ASTRON. ASTROPHYS. 58, 1-16, 1977.
C
C                    3) SEIDELMANN, P. K., 1980 IAU THEORY OF NUTATION:
C                    THE FINAL REPORT OF THE IAU WORKING GROUP ON
C                    NUTATION, CELEST. MECH. 27, PP. 79-106 (1982).
C
C                    4) WAHR, J. M., THE FORCED NUTATIONS OF ... EARTH,
C                    GEOPHYS. J. ROY. ASTR. SOC. 64, PP. 705-727 (1981).
C
C 7.2   NUTBD PROGRAM INTERFACE
C
C 7.2.1 CALLING SEQUENCE - NONE
C
C 7.2.2 COMMON BLOCK -
C 
      REAL*8 X(9,120)
      COMMON / XWAHR / X
C
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C           VARIABLES 'TO':
C              1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                           CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
C              2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000. 
C                           (DAYS) (DJ2000 = 2451545.D0) 
C              3. EC(4)   - THE CONSTANTS APPEARING IN TERMS 1-4 IN THE 
C                           CALCULATION OF THE MEAN OBLIQUITY OF THE ECLIPTIC.
C                           (ARCSEC) (SEE REFERENCES)
C                           ( EC(1) = +8.4381448D4, EC(2) = -46.815D0,
C                             EC(3) = -5.9D-4, EC(4) = +1.813D-3 )
C              4. NOT     - THE NUMBER OF TERMS IN THE NUTATION SERIES.
C                           (NOT = 106) 
C              5. X(9,120)- THE ARRAY CONTAINING THE NUTATION SERIES.
C                           (X = 1980 IAU THEORY OF NUTATION) 
C              6. NOP     - THE NUMBER OF NUTATION TERMS DESIGNATED FOR WHICH
C                           PARTIALS ARE TO BE COMPUTED. (NOP = 6) 
C                                  (Obsolete?)
C              7. IDP(6)  - IDENTIFICATION NUMBERS (TERM NUMBERS) OF DESIGNATED
C                           NUTATION TERMS FOR WHICH PARTIALS ARE TO BE COMPUTED
C                           ( IDP(1) =  1, IDP(2) =  2, IDP(3) =  3,
C                             IDP(4) =  4, IDP(5) =  5, IDP(6) =  7 ) 
C              8. ARGP(2,6)-ARGUMENTS (COMBINATIONS OF FUNDAMENTAL ARGUMENTS) 
C                           AND THEIR DERIVATIVES OF DESIGNATED NUTATION TERMS
C                           FOR WHICH PARTIALS ARE TO BE COMPUTED.
C                           (COMPUTED IN NUTW. SET TO 0.0D0 HERE)
C                                  (Obsolete?)
C
C 7.2.3 PROGRAM SPECIFICATIONS -
      Real*8  X1(180), X2(180), X3(180), X4(180), X5(180), X6(180)
      EQUIVALENCE (X(1,  1),X1(1))
      EQUIVALENCE (X(1, 21),X2(1))
      EQUIVALENCE (X(1, 41),X3(1))
      EQUIVALENCE (X(1, 61),X4(1))
      EQUIVALENCE (X(1, 81),X5(1))
      EQUIVALENCE (X(1,101),X6(1))
C
      DATA  CENTJ  / 36525.D0 /,
     1      DJ2000 / 2451545.D0 /,
     2      EC     / 8.4381448D4, -46.8150D0, -5.9D-4, 1.813D-3 /,
     3      NOT    / 106 /,
     4      NOP    / 6 /,
     5      IDP    / 1, 2, 3, 4, 5, 7 /,
     6      ARGP   / 12 * 0.0D0 /
C***********************************************************************
C
C               1980 IAU THEORY OF NUTATION (WAHR THEORY)
C           TABLE OF MULTIPLES OF ARGUMENTS AND COEFFICIENTS
C
C                   MULTIPLE OF            LONGITUDE        OBLIQUITY
C              L    L'   F    D  OMEGA   COEFF. OF SIN    COEFF. OF COS
      DATA X1/ 0.,  0.,  0.,  0.,  1., -171996., -174.2,  92025.,  8.9,
     /         0.,  0.,  2., -2.,  2.,  -13187.,   -1.6,   5736., -3.1,
     /         0.,  0.,  2.,  0.,  2.,   -2274.,   -0.2,    977., -0.5,
     /         0.,  0.,  0.,  0.,  2.,    2062.,    0.2,   -895.,  0.5,
     /         0.,  1.,  0.,  0.,  0.,    1426.,   -3.4,     54., -0.1,
     /         1.,  0.,  0.,  0.,  0.,     712.,    0.1,     -7.,  0.0,
     /         0.,  1.,  2., -2.,  2.,    -517.,    1.2,    224., -0.6,
     /         0.,  0.,  2.,  0.,  1.,    -386.,   -0.4,    200.,  0.0,
     /         1.,  0.,  2.,  0.,  2.,    -301.,    0.0,    129., -0.1,
     /         0., -1.,  2., -2.,  2.,     217.,   -0.5,    -95.,  0.3,
     /         1.,  0.,  0., -2.,  0.,    -158.,    0.0,     -1.,  0.0,
     /         0.,  0.,  2., -2.,  1.,     129.,    0.1,    -70.,  0.0,
     /        -1.,  0.,  2.,  0.,  2.,     123.,    0.0,    -53.,  0.0,
     /         1.,  0.,  0.,  0.,  1.,      63.,    0.1,    -33.,  0.0,
     /         0.,  0.,  0.,  2.,  0.,      63.,    0.0,     -2.,  0.0,
     /        -1.,  0.,  2.,  2.,  2.,     -59.,    0.0,     26.,  0.0,
     /        -1.,  0.,  0.,  0.,  1.,     -58.,   -0.1,     32.,  0.0,
     /         1.,  0.,  2.,  0.,  1.,     -51.,    0.0,     27.,  0.0,
     /         2.,  0.,  0., -2.,  0.,      48.,    0.0,      1.,  0.0,
     /        -2.,  0.,  2.,  0.,  1.,      46.,    0.0,    -24.,  0.0/
      DATA X2/ 0.,  0.,  2.,  2.,  2.,     -38.,    0.0,     16.,  0.0,
     /         2.,  0.,  2.,  0.,  2.,     -31.,    0.0,     13.,  0.0,
     /         2.,  0.,  0.,  0.,  0.,      29.,    0.0,     -1.,  0.0,
     /         1.,  0.,  2., -2.,  2.,      29.,    0.0,    -12.,  0.0,
     /         0.,  0.,  2.,  0.,  0.,      26.,    0.0,     -1.,  0.0,
     /         0.,  0.,  2., -2.,  0.,     -22.,    0.0,      0.,  0.0,
     /        -1.,  0.,  2.,  0.,  1.,      21.,    0.0,    -10.,  0.0,
     /         0.,  2.,  0.,  0.,  0.,      17.,   -0.1,      0.,  0.0,
     /         0.,  2.,  2., -2.,  2.,     -16.,    0.1,      7.,  0.0,
     /        -1.,  0.,  0.,  2.,  1.,      16.,    0.0,     -8.,  0.0,
     /         0.,  1.,  0.,  0.,  1.,     -15.,    0.0,      9.,  0.0,
     /         1.,  0.,  0., -2.,  1.,     -13.,    0.0,      7.,  0.0,
     /         0., -1.,  0.,  0.,  1.,     -12.,    0.0,      6.,  0.0,
     /         2.,  0., -2.,  0.,  0.,      11.,    0.0,      0.,  0.0,
     /        -1.,  0.,  2.,  2.,  1.,     -10.,    0.0,      5.,  0.0,
     /         1.,  0.,  2.,  2.,  2.,      -8.,    0.0,      3.,  0.0,
     /         0., -1.,  2.,  0.,  2.,      -7.,    0.0,      3.,  0.0,
     /         0.,  0.,  2.,  2.,  1.,      -7.,    0.0,      3.,  0.0,
     /         1.,  1.,  0., -2.,  0.,      -7.,    0.0,      0.,  0.0,
     /         0.,  1.,  2.,  0.,  2.,       7.,    0.0,     -3.,  0.0/
      DATA X3/-2.,  0.,  0.,  2.,  1.,      -6.,    0.0,      3.,  0.0,
     /         0.,  0.,  0.,  2.,  1.,      -6.,    0.0,      3.,  0.0,
     /         2.,  0.,  2., -2.,  2.,       6.,    0.0,     -3.,  0.0,
     /         1.,  0.,  0.,  2.,  0.,       6.,    0.0,      0.,  0.0,
     /         1.,  0.,  2., -2.,  1.,       6.,    0.0,     -3.,  0.0,
     /         0.,  0.,  0., -2.,  1.,      -5.,    0.0,      3.,  0.0,
     /         0., -1.,  2., -2.,  1.,      -5.,    0.0,      3.,  0.0,
     /         2.,  0.,  2.,  0.,  1.,      -5.,    0.0,      3.,  0.0,
     /         1., -1.,  0.,  0.,  0.,       5.,    0.0,      0.,  0.0,
     /         1.,  0.,  0., -1.,  0.,      -4.,    0.0,      0.,  0.0,
     /         0.,  0.,  0.,  1.,  0.,      -4.,    0.0,      0.,  0.0,
     /         0.,  1.,  0., -2.,  0.,      -4.,    0.0,      0.,  0.0,
     /         1.,  0., -2.,  0.,  0.,       4.,    0.0,      0.,  0.0,
     /         2.,  0.,  0., -2.,  1.,       4.,    0.0,     -2.,  0.0,
     /         0.,  1.,  2., -2.,  1.,       4.,    0.0,     -2.,  0.0,
     /         1.,  1.,  0.,  0.,  0.,      -3.,    0.0,      0.,  0.0,
     /         1., -1.,  0., -1.,  0.,      -3.,    0.0,      0.,  0.0,
     /        -1., -1.,  2.,  2.,  2.,      -3.,    0.0,      1.,  0.0,
     /         0., -1.,  2.,  2.,  2.,      -3.,    0.0,      1.,  0.0,
     /         1., -1.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0/
      DATA X4/ 3.,  0.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     /        -2.,  0.,  2.,  0.,  2.,      -3.,    0.0,      1.,  0.0,
     /         1.,  0.,  2.,  0.,  0.,       3.,    0.0,      0.,  0.0,
     /        -1.,  0.,  2.,  4.,  2.,      -2.,    0.0,      1.,  0.0,
     /         1.,  0.,  0.,  0.,  2.,      -2.,    0.0,      1.,  0.0,
     /        -1.,  0.,  2., -2.,  1.,      -2.,    0.0,      1.,  0.0,
     /         0., -2.,  2., -2.,  1.,      -2.,    0.0,      1.,  0.0,
     /        -2.,  0.,  0.,  0.,  1.,      -2.,    0.0,      1.,  0.0,
     /         2.,  0.,  0.,  0.,  1.,       2.,    0.0,     -1.,  0.0,
     /         3.,  0.,  0.,  0.,  0.,       2.,    0.0,      0.,  0.0,
     /         1.,  1.,  2.,  0.,  2.,       2.,    0.0,     -1.,  0.0,
     /         0.,  0.,  2.,  1.,  2.,       2.,    0.0,     -1.,  0.0,
     /         1.,  0.,  0.,  2.,  1.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0.,  2.,  2.,  1.,      -1.,    0.0,      1.,  0.0,
     /         1.,  1.,  0., -2.,  1.,      -1.,    0.0,      0.,  0.0,
     /         0.,  1.,  0.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         0.,  1.,  2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         0.,  1., -2.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0., -2.,  2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0., -2., -2.,  0.,      -1.,    0.0,      0.,  0.0/
      DATA X5/ 1.,  0.,  2., -2.,  0.,      -1.,    0.0,      0.,  0.0,
     /         1.,  0.,  0., -4.,  0.,      -1.,    0.0,      0.,  0.0,
     /         2.,  0.,  0., -4.,  0.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0.,  2.,  4.,  2.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0.,  2., -1.,  2.,      -1.,    0.0,      0.,  0.0,
     /        -2.,  0.,  2.,  4.,  2.,      -1.,    0.0,      1.,  0.0,
     /         2.,  0.,  2.,  2.,  2.,      -1.,    0.0,      0.,  0.0,
     /         0., -1.,  2.,  0.,  1.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0., -2.,  0.,  1.,      -1.,    0.0,      0.,  0.0,
     /         0.,  0.,  4., -2.,  2.,       1.,    0.0,      0.,  0.0,
     /         0.,  1.,  0.,  0.,  2.,       1.,    0.0,      0.,  0.0,
     /         1.,  1.,  2., -2.,  2.,       1.,    0.0,     -1.,  0.0,
     /         3.,  0.,  2., -2.,  2.,       1.,    0.0,      0.,  0.0,
     /        -2.,  0.,  2.,  2.,  2.,       1.,    0.0,     -1.,  0.0,
     /        -1.,  0.,  0.,  0.,  2.,       1.,    0.0,     -1.,  0.0,
     /         0.,  0., -2.,  2.,  1.,       1.,    0.0,      0.,  0.0,
     /         0.,  1.,  2.,  0.,  1.,       1.,    0.0,      0.,  0.0,
     /        -1.,  0.,  4.,  0.,  2.,       1.,    0.0,      0.,  0.0,
     /         2.,  1.,  0., -2.,  0.,       1.,    0.0,      0.,  0.0,
     /         2.,  0.,  0.,  2.,  0.,       1.,    0.0,      0.,  0.0/
      DATA X6/ 2.,  0.,  2., -2.,  1.,       1.,    0.0,     -1.,  0.0,
     /         2.,  0., -2.,  0.,  1.,       1.,    0.0,      0.,  0.0,
     /         1., -1.,  0., -2.,  0.,       1.,    0.0,      0.,  0.0,
     /        -1.,  0.,  0.,  1.,  1.,       1.,    0.0,      0.,  0.0,
     /        -1., -1.,  0.,  2.,  1.,       1.,    0.0,      0.,  0.0,
     /         0.,  1.,  0.,  1.,  0.,       1.,    0.0,      0.,  0.0,
     /                      126 *  0./
C
C***********************************************************************
C
C 7.2.4 CONSTANTS USED - CENTJ, DJ2000, EC(4), NOT, X(9,120),
C                    NOP, IDP(6), ARGP(2,6)
C
C 7.2.5 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 12/22/77
C                    CHOPO MA       08/04/81
C                    GEORGE KAPLAN  04/24/84
C                    David Gordon   94.04.15 Converted to Implicit None.
C                    David Gordon   95.09.27 X(9,120) table changed from Real*4
C                                   to Real*8
C                    David Gordon   98.02.03 Removed X(9,120) from COMMON 
C                                   /NUTCM/ and put it into COMMON /XWAHR/,
C                                   and removed it from most subroutines.
C
C 7.3   NUTBD PROGRAM STRUCTURE - NONE
C
      END 
C
C*****************************************************************************
      SUBROUTINE NUTW (CENT, fa, fad, DPSI, DEPS)
      IMPLICIT None
C
C 1.    NUTW
C 
C 1.1   NUTW PROGRAM SPECIFICATION
C 
C 1.1.1 THIS SUBROUTINE EVALUATES THE NUTATION SERIES AND RETURNS THE 
C       VALUES FOR NUTATION IN LONGITUDE AND NUTATION IN OBLIQUITY. 
C       The fundamental arguments and derivatives are calculated 
C       in NUTFA to be called prior to NUTW.
C 
C 1.1.2 RESTRICTIONS - NONE 
C 
C 1.1.3 REFERENCES - 1) KAPLAN, G. H. (ED.), THE IAU RESOLUTIONS ... ,
C                       USNO CIRCULAR NO. 163, U. S. NAVAL OBSERV'Y (1981).
C 
C 1.2   NUTW PROGRAM INTERFACE
C 
C 1.2.1 CALLING SEQUENCE - CALL NUTW(CENT, fa, fad, DPSI, DEPS)
C    INPUT VARIABLES:
C      1. cent   - The number of Julian centuries between the observation
C                  epoch and January 1.5, 2000. (centuries)
C      2. fa (5) - Fundamental arguments. (arcseconds) (see NUTFA)
C      3. fad(5) - The CT derivatives of the fundamental arguments. (see NUTFA)
C    OUTPUT VARIABLES: 
C      1. DPSI(2) - THE NUTATION IN LONGITUDE AND ITS CT TIME DERIVATIVE
C                   (ARCSEC,ARCSEC/SEC) 
C      2. DEPS(2) - THE NUTATION IN OBLIQUITY AND ITS CT TIME DERIVATIVE
C                   (ARCSEC,ARCSEC/SEC) 
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C     VARIABLES 'FROM': 
C       1. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS.
C       2. SECDAY - THE NUMBER OF COORDINATE TIME SECONDS PER COORDINATE
C                   TIME DAY. (SECONDS/DAY)
C 
      INCLUDE 'ccon.i'                 
C     VARIABLES 'FROM': 
C       1. KNUTD - THE NUTATION MODULE DEBUG CONTROL FLAG 
C
      REAL*8 fa(5), fad(5)
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
      REAL*8 X(9,120)
      COMMON / XWAHR / X
C 
C     VARIABLES 'FROM': 
C        1. CENTJ - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN CENTURY.
C                   (DAYS/CENTURY) 
C        2. NOT   - THE NUMBER OF TERMS IN THE NUTATION SERIES. 
C        3. X(9,120)-ARRAY CONTAINING THE NUTATION SERIES.
C        4. NOP   - THE NUMBER OF NUTATION TERMS DESIGNATED FOR WHICH
C                   PARTIALS ARE TO BE COMPUTED. (NOP .LE. 6) 
C                       (obsolete?)
C        5. IDP(6)- IDENTIFICATION NUMBERS (TERM NUMBERS) OF DESIGNATED 
C                   NUTATION TERMS FOR WHICH PARTIALS ARE TO BE COMPUTED.
C 
C     VARIABLES 'TO': 
C        1. ARGP(2,6) - ARGUMENTS (COMBINATIONS OF FUNDAMENTAL ARGUMENTS)
C                       AND THEIR DERIVATIVES OF DESIGNATED NUTATION TERMS
C                       FOR WHICH PARTIALS ARE TO BE COMPUTED.
C                           (obsolete?)
C 
C 1.2.3 PROGRAM SPECIFICATIONS
C 
      Real*8 DPSI(2),DEPS(2), CENT, SEC360, ARG, ARGDOT
      Integer*4 I, J, N 
C 
C 1.2.4 DATA BASE ACCESS - NONE 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG
C 
C 1.2.6 SUBROUTINE INTERFACE
C       CALLER SUBROUTINES - NUTG 
C       CALLED SUBROUTINES - DSIN, DCOS, DMOD
C 
C  constants used: 
C       SEC360   - ARCSECONDS IN ONE TURN
      DATA SEC360 / 1296000.D0 /
C 
C 1.2.8 PROGRAM VARIABLES 
C      1. ARG    - THE COMBINATION OF FUNDAMENTAL ARGUMENTS USED 
C                  TO COMPUTE THE NUTATION (RADIANS) 
C      2. ARGDOT - THE DERIVATIVE OF ARG (RADIANS/CENTURY) 
C 
C 1.2.9 PROGRAMMER:
C          810804  CHOPO MA 
C          840000  GEORGE KAPLAN  
C          930901  Norbert Zacharias: put fundamental arguments computation
C                  into subroutine NUTFA.
C          David Gordon 94.04.15 Converted to Implicit None.
C          David Gordon 95.09.27 Conversion of nutation series coefficients to
C                       double precision removed - no longer necessary. 
C
C 2.    INITIALIZE OUTPUT 
C 
      DPSI(1) = 0.0D0
      DPSI(2) = 0.0D0
      DEPS(1) = 0.0D0
      DEPS(2) = 0.0D0
C
C 3.    SUM NUTATION SERIES TERMS, FROM SMALLEST TO LARGEST 
C 
      N = NOP 
C 
      DO J=1,NOT
        I = NOT + 1 - J 
C 
C 3.1   FORMATION OF MULTIPLES OF ARGUMENTS 
C 
        ARG = X(1,I) * fa(1)  ! EL    arcseconds
     /      + X(2,I) * fa(2)  ! ELP
     /      + X(3,I) * fa(3)  ! F
     /      + X(4,I) * fa(4)  ! D
     /      + X(5,I) * fa(5)  ! OM 
        ARG = DMOD(ARG,SEC360) * CONVDS     ! radian
C 
C 3.2   FORMATION OF MULTIPLES FOR DERIVATIVES
C 
        ARGDOT = X(1,I) * fad(1)  ! ELD 
     /         + X(2,I) * fad(2)  ! ELPD 
     /         + X(3,I) * fad(3)  ! FD 
     /         + X(4,I) * fad(4)  ! DD 
     /         + X(5,I) * fad(5)  ! OMD
        ARGDOT = ARGDOT * CONVDS
C 
C 3.3   STORE VALUES OF ARGUMENTS AND DERIVATIVES OF SPECIFIC TERMS 
C 
        IF (N.GT.0) THEN 
          IF (IDP(N).EQ.I) THEN 
            ARGP(1,N) = ARG 
            ARGP(2,N) = ARGDOT
            N = N - 1 
          ENDIF 
        ENDIF
C 
C 3.4   EVALUATE NUTATION AND DERIVATIVES OF NUTATION 
C 
        DPSI(1) = (X(6,I) + X(7,I)*CENT) * DSIN(ARG) 
     .          + DPSI(1)
        DPSI(2) = DPSI(2) + X(7,I) * DSIN(ARG) + (X(6,I)
     .          + X(7,I) * CENT) * ARGDOT * DCOS(ARG) 
        DEPS(1) = (X(8,I) + X(9,I)*CENT) * DCOS(ARG) 
     .          + DEPS(1)
        DEPS(2) = DEPS(2) + X(9,I) * DCOS(ARG) - (X(8,I)
     .          + X(9,I) * CENT) * ARGDOT * DSIN(ARG)
C
      ENDDO  ! j=1,not
C
C 4.    CONVERT TO PROPER UNITS
C
      DPSI(1) = DPSI(1) * 1.0D-4
      DPSI(2) = DPSI(2) * 1.0D-4 / (CENTJ * SECDAY)
      DEPS(1) = DEPS(1) * 1.0D-4
      DEPS(2) = DEPS(2) * 1.0D-4 / (CENTJ * SECDAY)
C
C SEE IF WE NEED DEBUG OUTPUT
      IF (KNUTD .NE. 0) THEN 
        WRITE (6,9)
    9   FORMAT (1X,'Debug output for subroutine NUTW.')
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' SEC360  ',SEC360
        WRITE(6,8)' ARG     ',ARG
        WRITE(6,8)' ARGDOT  ',ARGDOT
        WRITE (6,9200) CONVDS, CENTJ, SECDAY, CENT, DEPS, DPSI
9200    FORMAT (1X,'CONVDS = ',D25.16,/,1X,'CENTJ = ',D25.16,/,1X,
     1       'SECDAY = ',D25.16,/,1X,'CENT = ',D25.16,/,1X,
     2       'DEPS = ',2(D25.16,2X),/,1X,'DPSI = ',2(D25.16,2X), /)
      ENDIF 
C
      END 
C
C*****************************************************************************
      SUBROUTINE NUTFA (xjd,ct,cent,fa,fad)
      IMPLICIT NONE
C 
C  NUTFA computes the number of Julian centuries since J2000 and the fundamental
C  arguments and derivatives to be used in the nutation series.
C
C  References: D.McCarthy, IERS Technical Note 13, 'IERS Conventions (1992)',
C                          Paris 1992
C              T.C. van Flandern, Lunar Occult. Work (fundam.argum.)
C              D.McCarthy, IERS Technical Note 21, 'IERS Conventions (1996)',
C                          Paris 1996
C 
C  Calling sequence:
C    input:  
C           1. xjd  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN 
C                      QUESTION. (DAYS) 
C           2. ct   -  THE COORDINATE TIME FRACTION OF THE COORDINATE TIME DAY.
C                      (DAYS)
C    output:
C           1. cent -  The number of Julian centuries elapsed since the epoch
C                      January 1.5 2000.(centuries)
C           2. fa(5)-  The fundamental arguments for the nutation theory.
C                      (arcseconds)
C               1 = mean anomaly of the moon
C                 = mean longitude of the moon minus the 
C                   mean longitude of the moon's perigee     (l)
C               2 = mean anomaly of the sun 
C                 = mean longitude of the sun minus the
C                   mean longitude of the sun's perigee      (l')
C               3 = mean longitude of the moon minus omega   (F)
C               4 = mean elongation of the moon from the sun (D)
C               5 = longitude of the asc.node of the moon's 
C                   mean orbit on the ecliptic, 
C                   measured from the mean equinox of date   (omega)
C           3. fad(5)- The CT time derivatives of the fundamental arguments.
C                      (arcsec/century)
C
      REAL*8 xjd,ct,cent,fa(5), fad(5), el, elp, f, d, om, sec360,
     .       elc(5), elpc(5), fc(5), dc(5), omc(5), cent2, cent3, cent4,
     .       daysj
C
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C     Variables from:
C        1. centj   -  the number of coordinate time days per Julian century.
C                      (days/century)
C        2. dj2000  -  the Julian date of the epoch January 1.5, 2000. (days)
C
      INCLUDE 'ccon.i'
C     Variables from:
C        1. KNUTD  -  nutation module debug flag   
C
C     Subroutine interface:
C       Caller subroutines: DRIVR, UT1I
C       Called subroutines: DMOD  
C
C     Constants used -
C       ELC(5)   - COEFFICIENTS USED IN THE CALCULATION OF EL
C       ELPC(5)  - COEFFICIENTS USED IN THE CALCULATION OF ELP 
C       FC(5)    - COEFFICIENTS USED IN THE CALCULATION OF F 
C       DC(5)    - COEFFICIENTS USED IN THE CALCULATION OF D 
C       OMC(5)   - COEFFICIENTS USED IN THE CALCULATION OF OM
C 
C     DATA statements for the fundamental arguments.
C     Simons et al., 1994 values
C      -Conform to IERS Conventions (1996)-
      data elc    / -0.00024470d0,       0.051635d0,  31.8792d0,    
     .          1717915923.2178d0,  485868.249036d0/
      data elpc   / -0.00001149d0,      -0.000136d0,  -0.5532d0,   
     .           129596581.0481d0,  1287104.79305d0/
      data fc     /  0.00000417d0,      -0.001037d0, -12.7512d0,
     .          1739527262.8478d0,  335779.526232d0/
      data dc     / -0.00003169d0,       0.006593d0,  -6.3706d0,   
     .          1602961601.2090d0,  1072260.70369d0/
      data omc    /-0.00005939d0,        0.007702d0,   7.4722d0, 
     .           -6962890.2665d0,   450160.398036d0/
C
      DATA SEC360 / 1296000.0D0 /       ! arcseconds in one turn
C
C  Programmer:
C    93.09.01  Norbert Zacharias - Fundamental arguments computation put into
C              separate subroutine, taken from old NUTG subroutine. 
C    98.01.28  David Gordon - Coefficients and computations modified to conform
C              to IERS Conventions (1996).
C
C-------------------------------------------------------------------------------
C  Compute the number of Julian days elapsed since the epoch January 1.5, 2000.
      daysj = xjd + ct - dj2000
C
C  Compute the number of Julian centuries elapsed since the epoch January 1.5,
C   2000.
      CENT  = DAYSJ / CENTJ 
      CENT2 = CENT * CENT
      CENT3 = CENT * CENT2
      CENT4 = CENT2 * CENT2
C
C  Computation of the fundamental arguments and derivatives
C 
      EL = ELC(1)*CENT4 + ELC(2)*CENT3 + ELC(3)*CENT2
     .   + ELC(4)*CENT  + ELC(5) 
      fa (1) = DMOD( EL, SEC360 ) 
      fad(1) = 4.D0*ELC(1)*CENT3 + 3.D0*ELC(2)*CENT2  
     .       + 2.D0*ELC(3)*CENT  +      ELC(4) 
C 
      ELP = ELPC(1)*CENT4 + ELPC(2)*CENT3 + ELPC(3)*CENT2
     .    + ELPC(4)*CENT  + ELPC(5)
      fa (2) = DMOD( ELP, SEC360 ) 
      fad(2) = 4.D0*ELPC(1)*CENT3 + 3.D0*ELPC(2)*CENT2 
     .       + 2.D0*ELPC(3)*CENT  +      ELPC(4) 
C 
      F = FC(1)*CENT4 + FC(2)*CENT3 + FC(3)*CENT2
     .  + FC(4)*CENT  + FC(5)
      fa (3) = DMOD( F, SEC360 ) 
      fad(3) = 4.D0*FC(1)*CENT3 + 3.D0*FC(2)*CENT2
     .       + 2.D0*FC(3)*CENT  +      FC(4) 
C 
      D = DC(1)*CENT4 + DC(2)*CENT3 + DC(3)*CENT2
     .  + DC(4)*CENT  + DC(5)
      fa (4) = DMOD( D, SEC360 ) 
      fad(4) = 4.D0*DC(1)*CENT3 + 3.D0*DC(2)*CENT2 
     .       + 2.D0*DC(3)*CENT  +      DC(4) 
C 
      OM = OMC(1)*CENT4 + OMC(2)*CENT3 + OMC(3)*CENT2
     .   + OMC(4)*CENT  + OMC(5)
      fa (5) = DMOD( OM, SEC360 ) 
      fad(5) = 4.D0*OMC(1)*CENT3 + 3.D0*OMC(2)*CENT2 
     .       + 2.D0*OMC(3)*CENT  +      OMC(4)
C-------------------------------------------------------------------------------
C  Debug output
      IF (knutd.NE.0) THEN
	WRITE (6,'(1x,a)') 'Debug output for subroutine NUTFA'
	WRITE (6,'(1x,a,d25.16)') 'cent  = ', cent
    8         FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' ELC     ',ELC
        WRITE(6,8)' EL      ',fa (1)
        WRITE(6,8)' ELD     ',fad(1)
        WRITE(6,8)' ELPC    ',ELPC  
        WRITE(6,8)' ELP     ',fa (2)
        WRITE(6,8)' ELPD    ',fad(2)
        WRITE(6,8)' FC      ',FC
        WRITE(6,8)' F       ',fa (3)
        WRITE(6,8)' FD      ',fad(3)
        WRITE(6,8)' DC      ',DC
        WRITE(6,8)' D       ',fa (4)
        WRITE(6,8)' DD      ',fad(4)
        WRITE(6,8)' OMC     ',OMC
        WRITE(6,8)' OM      ',fa (5)
        WRITE(6,8)' OMD     ',fad(5)
      ENDIF
C
      END
C******************************************************************************
C
      SUBROUTINE KSV_1996_3( CENT, fa, fad, dpsi_ls, deps_ls,
     .           dpsi_plan, deps_plan, dpsi_fcn , deps_fcn ,
     .           dpsi_prec, deps_prec, dpsi_tot , deps_tot )
      Implicit None
 
C     Subroutine to compute the complete KSV_1996_3 nutation series
C     with the associated corrections for planetary nutations,
C     the freely excited free-core-nutation (valid for 1988-1995),
C     the precession constant change and a rate of change of oblquity.)
C
C     This program has been modifed slightly from Herring's KSV
C     program to account for the bias in celestial pole offsets
C     (-43.1 in dpsi and -5.1 in depsilon).  These bias offsets
C     are added to the results in the calculation of dpsi_tot
C     and deps_tot.  These biases are chosen to best fit the
C     model to the VLBI observations.
C
C INPUT Values
C    1. Cent   -  Time in Julian centuries since January 1.5, 2000. (centuries)
C    2. fa (5) -  The fundamental arguments (arcsec)
C    3. fad(5) -  The CT time derivatives of the fundamental arguments.
C                 (arcsec/century)
C
C OUTPUT Values
C     dpsi_ls and deps_ls - luni-solar nutation in longitude and oblquity
C             and their time derivatives. (mas, mas/sec)
C     dpsi_plan and deps_plan  - contributions to the nutations in longitude
C             and obliquity due to direct planetary nutations and the 
C             perturbations of the lunar and terrestrial orbits, and time
C             derivatives.  (mas, mas/sec)
C     dpsi_fcn and deps_fcn  - contributions to the nutations in longitude
C             and obliquity due the free- excitation of the Free-core-nutation
C             and time derivatives.  (mas, mas/sec). Valid for 1988-1994 only.  
C     dpsi_prec and deps_prec  - contributions to the nutations in longitude
C             and obliquity due to changes in the precession constant and the
C             rate of change of obliquity, and time derivatives. 
C             (mas, mas/sec)
C     dpsi_tot and deps_tot  - total nutations in longitude and obliquity 
C             including the correction for the precession constant (when 
C             precession is computed using the IAU 1976 precession constant).
C             Obtained by summing all of the above corrections (except the
C             fcn terms, which are not in the IERS Conventions(1996)). 
C             (mas, mas/sec)
C
      Real*8 jd, dpsi_ls(2), deps_ls(2), dpsi_plan(2), deps_plan(2),
     .    dpsi_fcn(2) ,  deps_fcn(2), dpsi_prec(2), deps_prec(2),
     .    dpsi_tot(2), deps_tot(2), CENT, fa(5), fad(5)
C
C   Programmers
C       Tom Herring  ???????? - Original program 
C       USNO         ???????? - Unknown mods
C       D. Gordon    98.02.03 - Numerous mods for Calc 9.0 usage. Modified to
C                               use CENT (centuries) instead of Julian day. 
C                               Added time derivatives of all nutation terms.
C
C---------------------------------------------------------------
C     Call each of the routines needed for each contribution.
C
C    Luni-solar nutation
      Call ls_nut( CENT, fa, fad, dpsi_ls, deps_ls )
C
C    Planetary nutation
      Call plan_nut ( CENT, dpsi_plan, deps_plan )
C
C    Freely excited FCN (NOTE: No warning message is printed
C    if the JD is out of the range of 1988-1994)
C!!   Call fcn_nut ( CENT, dpsi_fcn , deps_fcn )
C   Free core nutation not in IERS Conventions and fcn_nut no longer valid
      dpsi_fcn(1) = 0.D0 
      deps_fcn(1) = 0.D0 
      dpsi_fcn(2) = 0.D0 
      deps_fcn(2) = 0.D0 
C
C    Precession and obliquity rate contributions (NOTE: IAU-1976 precession
C    constant assumed to be used in the basic calculation of precession).
      call prec_nut( CENT, dpsi_prec, deps_prec )
C
C     Now add up all of the terms to get the total nutation angles
C     and add on the bias offsets.
C      -FCN terms not used, Not in IERS Conventions (1996)
C**   dpsi_tot(1) = dpsi_ls(1) + dpsi_plan(1) + dpsi_fcn(1) + 
      dpsi_tot(1) = dpsi_ls(1) + dpsi_plan(1) +
     *              dpsi_prec(1) - 43.1d0
C**   deps_tot(1) = deps_ls(1) + deps_plan(1) + deps_fcn(1) + 
      deps_tot(1) = deps_ls(1) + deps_plan(1) +
     *              deps_prec(1) - 5.1d0
C
C   Time derivatives
      dpsi_tot(2) = dpsi_ls(2) + dpsi_plan(2) + dpsi_prec(2)
      deps_tot(2) = deps_ls(2) + deps_plan(2) + deps_prec(2) 
C
      return
      end
C*******************************************************************************
C
      SUBROUTINE ls_nut( CENT, fa, fad, dpsi_ls, deps_ls )
      Implicit None
C
C     Routine to compute the KSV_1996_3 luni-solar contributions the 
C     nutations in longitude and obliquity. The KSV_1996_3 is based on:
C
C     (1) The Souchay and Kinoshita Rigid Earth nutation series KSRE95. The
C     seven terms with duplicate arguments on the KS series have been 
C     combined into single terms.
C     The arguments were:
C      l   lp  F   D  Om  Period (days)
C     -1   0   0   1   0   411.78
C      0   1   0   0   0   365.26
C      0  -1   2  -2   2   365.22
C      0   2   0   0   0   182.63
C      0   0   2  -2   2   182.62
C      0  -1  -2   2  -2   121.75
C      0   0   0   1   0    29.53
C     The series has also been sorted in increasing order of amplitude of the
C     nutation. This is to minimize rounding error as the series is summed 
C     in reverse order.
C
C     (2) Estimates of the Retrograde FCN resonance factors from the Mathews
C     et al., nutation formulation (full complex estimates, and scaling
C     parameter R from the same theory. (The definition of complex values 
C     here is Rr + iRi; in earlier versions the definition was Rr -iRi.
C     This change does not effect the nutation series coefficients). The
C     nutation amplitudes are still defined as ar - ai).
C
C   The resonance factors used are:
C   Type             Amplitude                    Frequency (cpsd)
C                Real            Imag.         Real            Imag.
C   RFCN     -.00011489752   .00000214130     -1.00231888314  -.00002920327 
C   CW       -.00057992000   .00000000000       .00253170000   .00000000000  
C   R and R' 1.04901828112  -.00150732471      -.25517427386   .03965769073
C
C     (3) The effects of annual modulation of geodetic precession.
C     The correction applied is
C      0  1  0  0  0 -0.150 (correction to in-phase nutation in longitude).
C
C     (4) A prograde annual nutation has been estimated along with the
C      resonance coefficients. This probably reflects the influence of
C      S1 atmospheric tide.
C
C     (5) The free RFCN mode was estimated once every two years for the
C      data after 1984. (See values commented in eval_ls_nut. For the
C      last 6 years the values seem to be resonably stable.  
C
C     (6) The new Simons et al., fundamental arguments are used in this
C      version. (The largest change from KSV_1995_1 was 0.007 mas for
C      semiannual nutation. All other changes, including the 18.6 year
C      nutation were 0.001-0.002 mas.)
C
C     REFERENCES:
C NEW Version based on: Corrections and new developments in rigid Earth
C     nutation theory: Lunisolar influence including indirect planetary
C     effects, J. Souchay and H. Kinioshita, Astron. and Astrophys., 1995.
C (Version here based on data files: KSRE95_FIG_PSI.DAT KSRE95_FIG_EPS.DAT
C  and generated with ks_plan.f)
C     Kinoshita, H., and J. Souchay, The theory of the nutations for
C         the rigid Earth at the second order, Celes. Mech. and Dynam.
C         Astron., 48, 187--266, 1990.
C     Mathews, P. M., B. A. Buffett, T. A. Herring, and I. I. Shapiro,
C         Forced nutations of the Earth: Influence of the inner core
C         dynamics, 1, Theory. J. Geophs. Res., 96, 8219--8242, 1991.
C     Simon, J. L., Bretagnon, P., Chapront, J., Chapront-Touze, M.,
C         Francou, G., Laskar, J., 1994, "Numerical Expressions for 
C         Precession Formulae and Mean Elements for the Moon and
C         Planets," Astron. Astrophys., 282, pp. 663-683.
C
C INPUT Values
C    1. Cent   - The number of Julian centuries elapsed since the
C                epoch January 1.5, 2000. (centuries)
C    2. fa (5) - The fundamental arguments (arcsec)
C    3. fad(5) - The CT time derivatives of the fundamental 
C                arguments. (arcsec/century)
C
C OUTPUT Values
C    1. dpsi_ls  - The nutation in longitude (mas).
C    2. deps_ls  - The nutation in obliquity (mas).
C
      Real*8 Cent, fa(5), fad(5), dpsi_ls(2), deps_ls(2)
C
C  Common Blocks:
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C     VARIABLES 'FROM': 
C       1.  CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER 
C                      ARCSECOND.  (RAD/ARCSEC) 
C
C LOCAL VARIABLES
C   ls_arg(5)   -  The arguments for the Luni-solar nutations.
C                  (l, l', F, D and Omega). All in Radians.
C   ls_argdot(5) - The derivatives of the fundamental arguments for the
C                  Luni-solar nutations.
C                  (l, l', F, D and Omega). All in Radians.
C
      Real*8 ls_arg(5), ls_argdot(5)
C
C   Get the fundamental arguments at this epoch
C    --- Turned off, all fundamental arguments computed in NUTFA
C     call ls_angles( CENT, ls_arg, ls_argdot)
C Convert fundamental arguments (from NUTFA) to radians 
      ls_arg(1) = fa(1) * CONVDS              
      ls_arg(2) = fa(2) * CONVDS                
      ls_arg(3) = fa(3) * CONVDS                
      ls_arg(4) = fa(4) * CONVDS              
      ls_arg(5) = fa(5) * CONVDS              
      ls_argdot(1) = fad(1) * CONVDS              
      ls_argdot(2) = fad(2) * CONVDS              
      ls_argdot(3) = fad(3) * CONVDS              
      ls_argdot(4) = fad(4) * CONVDS              
      ls_argdot(5) = fad(5) * CONVDS              
C
C  Compute the luni-solar nutations by summing over all terms in the series.
      call eval_ls_nut(CENT, ls_arg, ls_argdot, dpsi_ls, deps_ls)
C
      return
      end
C*******************************************************************************
C 
      SUBROUTINE ls_angles( CENT, ls_arg, ls_argdot )
      Implicit None
C
C     Routine to compute the value of the fundamental argument for Brown's
C     arguments. Arguments based on the IERS standards.
C
C MOD TAH 960206: Changed arguments to use Simons et al., 1994 values:
C     Simon, J. L., Bretagnon, P., Chapront, J., Chapront-Touze, M.,
C          Francou, G., Laskar, J., 1994, "Numerical Expressions for 
C          Precession Formulae and Mean Elements for the Moon and
C          Planets," Astron. Astrophys., 282, pp. 663-683.
C-------------------------------------------------------------------
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C     VARIABLES 'FROM': 
C       1.  CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER 
C                      ARCSECOND.  (RAD/ARCSEC) 
C       2.  PI      -  PI
C 
C PHYSICAL CONSTANTS
C
C   rad_to_deg  - Conversion from radians to degs.
C   DJ2000      - Julian date of J2000
C   sec360      - number of seconds in 360 degreees.
C
c     real*8 pie,rad_to_deg, DJ2000, sec360
c     real*8 rad_to_deg, DJ2000, sec360
      real*8 DJ2000, sec360
      parameter ( DJ2000        = 2451545.d0           )
      parameter ( sec360        = 1296000.d0           )
C     Computed quantities
c     parameter ( rad_to_deg    = 180.d0   /pi         )
C
C PASSED VARIABLES
C
C INPUT
C   Cent   - The number of Julian centuries elapsed since the
C                epoch January 1.5, 2000. (centuries)
C
C OUTPUT
C ls_arg(5) -  Brown's arguments (radians, REAL*8)
C
      Real*8 Cent, ls_arg(5), ls_argdot(5)
C
C LOCAL VARIABLES
C      el,eld           - Mean longitude of moon minus mean
C                       - longitude of moon's perigee (arcsec)
C      elc(5)           - Coefficients for computing el
C      elp,elpd         - Mean longitude of the sun minus mean
C                       - longitude of sun perigee (arcsec)
C      elpc(5)          - Coeffiecents for computing elp
C      f,fd             - Moon's mean longitude minus omega (sec)
C      fc(5)            - Coefficients for computing f
C      d,dd             - Mean elongation of the moon from the
C                       - sun (arcsec)
C      dc(5)            - coefficients for computing d
C      om,omd           - longitude of the ascending node of the
C                       - moon's mean orbit on the elliptic
C                       - measured from the mean equinox of date
C      omc(5)           - Coefficients for computing om.
C
      Real*8 el,eld, elc(5), elp, elpd, elpc(5),
     .    f,fd, fc(5), d,dd, dc(5), om,omd, omc(5)
C
C***  DATA statements for the fundamental arguments.
C     Simons et al., 1994 values
C
      data elc    /    -0.00024470d0,    0.051635d0,   31.8792d0,    
     .         1717915923.2178d0,   485868.249036d0/
      data elpc   /    -0.00001149d0,    -0.000136d0,  -0.5532d0,   
     .          129596581.0481d0,   1287104.79305d0/
      data fc     /     0.00000417d0,    -0.001037d0,  -12.7512d0,
     .         1739527262.8478d0,    335779.526232d0/
      data dc     /    -0.00003169d0,     0.006593d0,   -6.3706d0,   
     .         1602961601.2090d0,   1072260.70369d0/
C MOD TAH KSV_1996_3: 960606: Replaced <Om> with expression from b.3 of 
C     Simon et al., 1994 since b.3 is computed with new precession constant
C     (Only the rate changes).   
      data omc    /    -0.00005939,       0.007702d0,    7.4722d0, 
     .           -6962890.5431d0,     450160.398036d0/
c    .           -6962890.2665d0,     450160.398036d0/
C
C
C***  Compute angular arguments and their time derivatives
C New formulas adding in the higher order term.
C
      el = elc(1) * cent**4 + elc(2) * cent**3 + elc(3) * cent**2
     .      + elc(4) * cent + elc(5)
      el = mod( el, sec360 )
      eld = 4.d0 * elc(1) * cent**3 + 3.d0 * elc(2) * cent**2 + 
     .      2.d0 * elc(3) * cent    +        elc(4) 
C
      elp = elpc(1) * cent**4 + elpc(2) * cent**3 + elpc(3) * cent**2
     .     + elpc(4) * cent + elpc(5)
      elp = mod( elp, sec360 )
      elpd = 4.d0 * elpc(1) * cent**3 + 3.d0 * elpc(2) * cent**2 + 
     .       2.d0 * elpc(3) * cent    +        elpc(4) 
C
      f = fc(1) * cent**4 + fc(2) * cent**3 + fc(3) * cent**2
     .     + fc(4) * cent + fc(5)
      f = mod( f, sec360 )
      fd = 4.d0 * fc(1) * cent**3 + 3.d0 * fc(2) * cent**2 + 
     .     2.d0 * fc(3) * cent    +        fc(4) 
C
      d = dc(1) * cent**4 + dc(2) * cent**3 + dc(3) * cent**2
     .   + dc(4) * cent + dc(5)
      d = mod( d, sec360 )
      dd = 4.d0 * dc(1) * cent**3 + 3.d0 * dc(2) * cent**2 + 
     .     2.d0 * dc(3) * cent    +        dc(4) 
C
      om = omc(1) * cent**4 + omc(2) * cent**3 + omc(3) * cent**2
     .     + omc(4) * cent + omc(5)
      om = mod( om, sec360 )
      omd = 4.d0 * omc(1) * cent**3 + 3.d0 * omc(2) * cent**2 + 
     .      2.d0 * omc(3) * cent    +        omc(4) 
C
c       print *, 'el, elp, f, d, om  ', el, elp, f, d, om 
c       print *, 'eld,elpd,fd,dd,omd ', eld, elpd, fd, dd, omd
C
C***  Now save the values.  Convert values from arcseconds to radians
C
      ls_arg(1) = el * CONVDS              
      ls_arg(2) = elp* CONVDS                
      ls_arg(3) = f  * CONVDS                
      ls_arg(4) = d  * CONVDS              
      ls_arg(5) = om * CONVDS              
C
      ls_argdot(1) = eld * CONVDS              
      ls_argdot(2) = elpd* CONVDS              
      ls_argdot(3) = fd  * CONVDS              
      ls_argdot(4) = dd  * CONVDS              
      ls_argdot(5) = omd * CONVDS              
C
      return
      end
C******************************************************************************
C
      SUBROUTINE eval_ls_nut(CENT, ls_arg, ls_argdot, dpsi_ls, deps_ls)
      Implicit None
C
C     Routine to compute the nutations in longitude and obliquity
C     by summing over all terms in the nutation series.
C
C PARAMETERS:
C
C  num_ls  - Number of terms in the nutations series
      integer*4 num_ls
      parameter (num_ls = 263)
C
C   DJ2000      - Julian date of J2000
c     real*8 DJ2000
c     parameter ( DJ2000        = 2451545.d0           )
C
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C         1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                      CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
C         2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000. 
C                      (DAYS) (DJ2000 = 2451545.D0) 
C
C PASSED PARAMETERS:
C
C INPUT:
C  1. Cent   - The number of Julian centuries elapsed since the
C              epoch January 1.5, 2000. (centuries)
C  2. ls_arg(5)  - Five fundamental arguments for the nutations 
C              (l,l',F,D and Om) at the epoch that the nutations need to be
C              evaluated (rad) 
C  3. ls_argdot(5) - Time derivatives of the fundamental arguments above.
C
C OUTPUT:
C dpsi_ls, deps_ls   - nutations in longitude and obliquity (mas)
c     Real*8 epoch, ls_arg(5), ls_argdot(5), dpsi_ls(2), deps_ls(2)
      Real*8 Cent, ls_arg(5), ls_argdot(5), dpsi_ls(2), deps_ls(2)
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C           VARIABLES 'FROM':
C              1. PI      -  PI
C              1. CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER ARCSECOND.
C                            (RAD/ARCSEC)
C              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
C                            COORDINATE TIME DAY.  (SEC/DAY) 
C 
C LOCAL VARIABLES:
C  i and j - Counters to loop over the coeffients and the argumemts
      integer*4 i,j
C
C  arg       - Final summed argumemt for the nutations contributions. (rads)
C  argdot    - Time derivative of arg. (rads/???)
C  dpsi_lsu and deps_lsu - Nutations in longitude and oblquity in micro-arc-sec
C              (units that the data statements are in).
C
      Real*8 arg, argdot, dpsi_lsu(2), deps_lsu(2)
C
C     Fortran data statements for the nutation series
C     Series based on the following model:
C     Type                Amplitude           Frequency
C       1     -.00011489814   .00000214165     -1.00231887299  -.00002920212
C       2      .00000000000   .00000000000      -.99780880000   .00000000000
C       3     -.00057992000   .00000000000       .00253170000   .00000000000
C       4      .00000000000   .00000000000       .00041390000   .00000000000
C       5     1.04901875266  -.00150730877      -.25518016085   .03966200327
C       6      .00000000000   .00000000000       .00000000000   .00000000000
C    
C     RFCN Freq. -1.00231887299 cyc per sidreal day, Period 430.0665 solar days
C  IX01-IX27(11,10)  -- Individual declarations of the coefficients of the 
C                  nutation series so no data statement has more than 10 lines.
C                          The first 5 values are the arguments for l lp F D Om
C                          The remaining elements are:
C                           6 - Nutation in longitude psi (sin, uas)
C                           7 - dpsi/dt (uasec/cent)
C                           8 - Nutation in oblquity eps (cos, uas)
C                           9 - deps/dt (uas/cent)
C                          10 - Out-of-phase longitude (cos, uas)
C                          11 - Out-of-phase obliquity (sin, uas)
C
      Integer*4 IX01(11,10), IX02(11,10), IX03(11,10), IX04(11,10), 
     .          IX05(11,10), IX06(11,10), IX07(11,10), IX08(11,10), 
     .          IX09(11,10), IX10(11,10), IX11(11,10), IX12(11,10), 
     .          IX13(11,10), IX14(11,10), IX15(11,10), IX16(11,10), 
     .          IX17(11,10), IX18(11,10), IX19(11,10), IX20(11,10), 
     .          IX21(11,10), IX22(11,10), IX23(11,10), IX24(11,10), 
     .          IX25(11,10), IX26(11,10), IX27(11, 3)

      integer*4 nutc_int(11,263)
      equivalence (nutc_int(1,  1),IX01(1,1))
      equivalence (nutc_int(1, 11),IX02(1,1))
      equivalence (nutc_int(1, 21),IX03(1,1))
      equivalence (nutc_int(1, 31),IX04(1,1))
      equivalence (nutc_int(1, 41),IX05(1,1))
      equivalence (nutc_int(1, 51),IX06(1,1))
      equivalence (nutc_int(1, 61),IX07(1,1))
      equivalence (nutc_int(1, 71),IX08(1,1))
      equivalence (nutc_int(1, 81),IX09(1,1))
      equivalence (nutc_int(1, 91),IX10(1,1))
      equivalence (nutc_int(1,101),IX11(1,1))
      equivalence (nutc_int(1,111),IX12(1,1))
      equivalence (nutc_int(1,121),IX13(1,1))
      equivalence (nutc_int(1,131),IX14(1,1))
      equivalence (nutc_int(1,141),IX15(1,1))
      equivalence (nutc_int(1,151),IX16(1,1))
      equivalence (nutc_int(1,161),IX17(1,1))
      equivalence (nutc_int(1,171),IX18(1,1))
      equivalence (nutc_int(1,181),IX19(1,1))
      equivalence (nutc_int(1,191),IX20(1,1))
      equivalence (nutc_int(1,201),IX21(1,1))
      equivalence (nutc_int(1,211),IX22(1,1))
      equivalence (nutc_int(1,221),IX23(1,1))
      equivalence (nutc_int(1,231),IX24(1,1))
      equivalence (nutc_int(1,241),IX25(1,1))
      equivalence (nutc_int(1,251),IX26(1,1))
      equivalence (nutc_int(1,261),IX27(1,1))
C
      data IX01/
     .  0,  0,  0,  0,  1,-17206262, -17419, 9205348,  886, 3608, 1543,
     .  0,  0,  2, -2,  2, -1317015,   -156,  573059, -306,-1398, -463,
     .  0,  0,  2,  0,  2,  -227718,    -23,   97864,  -48,  288,  145,
     .  0,  0,  0,  0,  2,   207428,     21,  -89746,   47,  -71,  -29,
     .  0,  1,  0,  0,  0,   147545,   -364,    7390,  -19, 1121, -198,
     .  0,  1,  2, -2,  2,   -51687,    123,   22440,  -68,  -54,  -18,
     .  1,  0,  0,  0,  0,    71118,      7,    -687,    0,  -98,   39,
     .  0,  0,  2,  0,  1,   -38752,    -37,   20076,    2,   37,   34,
     .  1,  0,  2,  0,  2,   -30136,     -4,   12896,   -6,   81,   37,
     .  0, -1,  2, -2,  2,    21583,    -49,   -9591,   30,    6,   12/
      data IX02/ 
     .  0,  0,  2, -2,  1,    12820,     14,   -6897,   -1,   18,    4,
     . -1,  0,  2,  0,  2,    12353,      1,   -5333,    3,    1,   -1,
     . -1,  0,  0,  2,  0,    15700,      1,    -127,    0,  -19,    9,
     .  1,  0,  0,  0,  1,     6314,      6,   -3323,    0,    3,   -1,
     . -1,  0,  0,  0,  1,    -5797,     -6,    3141,    0,  -19,   -8,
     . -1,  0,  2,  2,  2,    -5965,     -1,    2554,   -1,   15,    7,
     .  1,  0,  2,  0,  1,    -5163,     -4,    2635,    0,   12,    8,
     . -2,  0,  2,  0,  1,     4590,      5,   -2424,   -1,    1,    1,
     .  0,  0,  0,  2,  0,     6336,      1,    -125,    0,  -16,    3,
     .  0,  0,  2,  2,  2,    -3854,      0,    1643,    0,   15,    7/
      data IX03/ 
     . -2,  0,  0,  2,  0,    -4774,      0,      48,    0,   -2,   -3,
     .  2,  0,  2,  0,  2,    -3102,      0,    1322,   -1,   13,    6,
     .  1,  0,  2, -2,  2,     2863,      0,   -1234,    1,    0,    0,
     . -1,  0,  2,  0,  1,     2044,      2,   -1076,    0,    1,    0,
     .  2,  0,  0,  0,  0,     2923,      0,     -62,    0,   -8,    1,
     .  0,  0,  2,  0,  0,     2585,      0,     -56,    0,   -7,    1,
     .  0,  1,  0,  0,  1,    -1406,     -3,     857,    0,    8,   -4,
     . -1,  0,  0,  2,  1,     1517,      1,    -801,    0,    1,    0,
     .  0,  2,  2, -2,  2,    -1578,      7,     685,   -4,   -2,   -1,
     .  0,  0, -2,  2,  0,     2178,      0,     -15,    0,    1,    1/
      data IX04/
     .  1,  0,  0, -2,  1,    -1286,     -1,     694,    0,   -4,   -2,
     .  0, -1,  0,  0,  1,    -1269,      1,     642,    1,    6,    2,
     . -1,  0,  2,  2,  1,    -1022,     -1,     522,    0,    2,    2,
     .  0,  2,  0,  0,  0,     1671,     -8,      14,    0,   -1,    1,
     .  1,  0,  2,  2,  2,     -768,      0,     325,    0,    4,    2,
     . -2,  0,  2,  0,  0,    -1102,      0,      10,    0,   -1,    0,
     .  0,  1,  2,  0,  2,      757,     -2,    -326,   -2,   -1,   -1,
     .  0,  0,  2,  2,  1,     -664,     -1,     335,   -1,    2,    1,
     .  0, -1,  2,  0,  2,     -714,      2,     307,    2,    1,    0,
     .  0,  0,  0,  2,  1,     -631,     -1,     327,    0,    0,    0/
      data IX05/ 
     .  1,  0,  2, -2,  1,      580,      1,    -307,    0,    0,    0,
     .  2,  0,  2, -2,  2,      643,      0,    -277,    0,   -1,    0,
     . -2,  0,  0,  2,  1,     -579,     -1,     304,    0,   -1,    0,
     .  2,  0,  2,  0,  1,     -533,      0,     269,    0,    2,    1,
     .  0, -1,  2, -2,  1,     -477,     -1,     271,   -1,    0,    0,
     .  0,  0,  0, -2,  1,     -493,     -1,     272,    0,   -2,   -1,
     . -1, -1,  0,  2,  0,      735,      0,      -5,    0,   -1,    0,
     .  2,  0,  0, -2,  1,      405,      0,    -220,    0,    1,    0,
     .  1,  0,  0,  2,  0,      657,      0,     -20,    0,   -2,    0,
     .  0,  1,  2, -2,  1,      361,      0,    -194,    0,    1,    0/
      data IX06/
     .  1, -1,  0,  0,  0,      471,      0,      -4,    0,   -1,    0,
     . -2,  0,  2,  0,  2,     -311,      0,     131,    0,    0,    0,
     .  3,  0,  2,  0,  2,     -289,      0,     124,    0,    2,    1,
     .  0, -1,  0,  2,  0,      435,      0,      -9,    0,   -1,    0,
     .  1, -1,  2,  0,  2,     -287,      0,     123,    0,    1,    0,
     . -1, -1,  2,  2,  2,     -282,      0,     122,    0,    1,    0,
     .  0,  0,  0,  1,  0,     -422,      0,       3,    0,    1,    0,
     . -1,  0,  2,  0,  0,     -404,      0,       4,    0,    1,    0,
     .  0, -1,  2,  2,  2,     -264,      0,     114,    0,    1,    0,
     . -2,  0,  0,  0,  1,     -228,      0,     126,    0,   -1,    0/
      data IX07/ 
     .  1,  1,  2,  0,  2,      246,      0,    -106,    0,   -1,    0,
     .  2,  0,  0,  0,  1,      218,      0,    -114,    0,    0,    0,
     . -1,  1,  0,  1,  0,      327,      0,      -1,    0,    0,    0,
     .  1,  1,  0,  0,  0,     -338,      0,       4,    0,    0,    0,
     .  1,  0,  2,  0,  0,      334,      0,     -11,    0,   -1,    0,
     . -1,  0,  2, -2,  1,     -199,      0,     107,    0,   -1,    0,
     .  1,  0,  0,  0,  2,     -197,      0,      85,    0,    0,    0,
     . -1,  0,  0,  1,  0,      405,      0,     -55,    0,  -35,  -14,
     .  0,  0,  2,  1,  2,      165,      0,     -72,    0,    0,    0,
     . -1,  0,  2,  4,  2,     -151,      0,      66,    0,    1,    0/
      data IX08/ 
     .  0, -2,  2, -2,  1,     -130,      0,      69,    0,    0,    0,
     . -1,  1,  0,  1,  1,      132,      0,     -68,    0,    0,    0,
     .  1,  0,  2,  2,  1,     -133,      0,      66,    0,    1,    0,
     . -2,  0,  2,  2,  2,      139,      0,     -60,    0,    0,    0,
     . -1,  0,  0,  0,  2,      139,      0,     -60,    0,    0,    0,
     .  1,  1,  2, -2,  2,      128,      0,     -55,    0,    0,    0,
     . -2,  0,  2,  4,  2,     -121,      0,      52,    0,    0,    0,
     . -1,  0,  4,  0,  2,      115,      0,     -49,    0,    0,    0,
     .  2,  0,  2, -2,  1,      101,      0,     -54,    0,    0,    0,
     .  2,  0,  2,  2,  2,     -108,      0,      47,    0,    1,    0/
      data IX09/ 
     .  1,  0,  0,  2,  1,      -95,      0,      49,    0,    0,    0,
     .  3,  0,  0,  0,  0,      157,      0,      -5,    0,   -1,    0,
     .  3,  0,  2, -2,  2,       94,      0,     -40,    0,    0,    0,
     .  0,  0,  4, -2,  2,       91,      0,     -39,    0,    0,    0,
     .  0,  0, -2,  2,  1,       87,      0,     -44,    0,    0,    0,
     .  0,  1,  2,  0,  1,       81,      0,     -42,    0,    0,    0,
     .  0,  0,  2, -2,  3,      123,      0,     -20,    0,    0,    0,
     . -1,  0,  0,  4,  0,      133,      0,      -4,    0,    0,    0,
     .  2,  0, -2,  0,  1,       71,      0,     -38,    0,    0,    0,
     . -2,  0,  0,  4,  0,      128,      0,       1,    0,    0,    0/
      data IX10/
     . -1, -1,  0,  2,  1,       75,      0,     -39,    0,    0,    0,
     . -2, -1,  0,  2,  0,     -115,      0,       1,    0,    0,    0,
     .  0, -1,  2,  0,  1,      -66,      0,      35,    0,    0,    0,
     . -1,  0,  0,  1,  1,      101,      0,     -49,    0,   -3,   -1,
     .  0,  0, -2,  0,  1,      -68,      0,      36,    0,    0,    0,
     .  0,  1,  0,  0,  2,       69,      0,     -33,    0,   -1,    0,
     .  0,  0,  2, -1,  2,      -74,      0,      31,    0,    0,    0,
     .  0,  0,  2,  4,  2,      -69,      0,      29,    0,    0,    0,
     .  1,  1,  0, -2,  1,      -61,      0,      32,    0,    0,    0,
     . -1,  1,  0,  2,  0,      -94,      0,       0,    0,    0,    0/
      data IX11/ 
     .  1, -1,  2,  2,  2,      -59,      0,      25,    0,    0,    0,
     .  1, -1,  0,  0,  1,       51,      0,     -27,    0,    0,    0,
     .  0,  1, -2,  2,  0,      -90,      0,       3,    0,    0,    0,
     .  3,  0,  2,  0,  1,      -50,      0,      25,    0,    0,    0,
     . -1,  1,  2,  2,  2,       56,      0,     -24,    0,    0,    0,
     .  0,  1,  2,  2,  2,       54,      0,     -22,    0,    0,    0,
     . -1,  0,  0, -2,  1,      -50,      0,      27,    0,    0,    0,
     . -1,  1,  0,  1,  2,      -52,      0,      23,    0,    0,    0,
     .  0, -1,  2,  2,  1,      -44,      0,      24,    0,    0,    0,
     .  1,  0,  2, -4,  1,      -47,      0,      24,    0,    0,    0/
      data IX12/
     . -1,  0, -2,  2,  0,       77,      0,       0,    0,    0,    0,
     . -1, -1,  2,  2,  1,      -46,      0,      24,    0,    0,    0,
     .  0, -1,  0,  0,  2,       59,      0,     -25,    0,    0,    0,
     .  2, -1,  2,  0,  2,      -48,      0,      21,    0,    0,    0,
     .  1, -1,  2,  0,  1,      -42,      0,      22,    0,    0,    0,
     .  0,  0,  0,  2,  2,      -46,      0,      20,    0,    0,    0,
     .  0,  1,  0,  2,  0,      -67,      0,       0,    0,    0,    0,
     . -1,  1,  2,  0,  2,       47,      0,     -20,    0,    0,    0,
     .  0,  3,  2, -2,  2,      -44,      0,      19,    0,    0,    0,
     .  0, -1, -2,  2,  0,       66,      0,       0,    0,    0,    0/
      data IX13/ 
     .  0,  0,  0,  1,  1,      -37,      0,      20,    0,    0,    0,
     . -1,  0,  2,  2,  0,       64,      0,       1,    0,    0,    0,
     .  1,  1,  2,  0,  1,       36,      0,     -18,    0,    0,    0,
     .  2,  1,  2,  0,  2,       40,      0,     -17,    0,    0,    0,
     .  0,  1,  0,  1,  0,       57,      0,       0,    0,    0,    0,
     .  1,  0, -2,  2,  0,      -58,      0,       0,    0,    0,    0,
     .  1,  1,  0,  0,  1,      -34,      0,      19,    0,    0,    0,
     .  2,  0,  0,  2,  0,       59,      0,       1,    0,    0,    0,
     . -1,  0,  0,  2,  2,      -38,      0,      17,    0,    0,    0,
     .  0,  0,  0, -1,  1,       33,      0,     -18,    0,    0,    0/
      data IX14/ 
     .  0,  1,  0, -2,  1,      -33,      0,      18,    0,    0,    0,
     . -1,  0,  2, -2,  2,       36,      0,     -16,    0,    0,    0,
     . -1,  1,  0,  0,  1,      -31,      0,      17,    0,    0,    0,
     .  1,  0,  2,  1,  2,       33,      0,     -14,    0,    0,    0,
     .  0,  0,  0,  4,  0,       48,      0,       1,    0,    0,    0,
     .  0,  0,  2,  1,  1,       27,      0,     -14,    0,    0,    0,
     .  1,  0,  0, -2,  2,       32,      0,     -14,    0,    0,    0,
     .  1,  0,  2, -1,  2,      -33,      0,      13,    0,    0,    0,
     .  1, -1,  0,  2,  0,       48,      0,       0,    0,    0,    0,
     . -1,  0,  2,  4,  1,      -26,      0,      13,    0,    0,    0/
      data IX15/ 
     .  0,  0,  2,  2,  0,       41,      0,       1,    0,    0,    0,
     .  1,  0, -2,  0,  1,       27,      0,     -14,    0,    0,    0,
     . -1,  0,  2, -1,  1,      -23,      0,      14,    0,    0,    0,
     .  1,  1,  2, -2,  1,       23,      0,     -12,    0,    0,    0,
     .  4,  0,  2,  0,  2,      -26,      0,      11,    0,    0,    0,
     .  0,  1,  2,  1,  2,      -24,      0,      10,    0,    0,    0,
     .  2,  0,  2,  0,  0,       36,      0,       1,    0,    0,    0,
     .  2,  1,  2, -2,  2,       25,      0,     -10,    0,    0,    0,
     .  2, -1,  0,  0,  0,       38,      0,       0,    0,    0,    0,
     . -1, -1,  0,  0,  1,       21,      0,     -12,    0,    0,    0/
      data IX16/
     . -2,  0,  2,  2,  1,       22,      0,     -11,    0,    0,    0,
     .  0,  0,  0,  0,  3,      -22,      0,      10,    0,    0,    0,
     .  1,  0,  4, -2,  2,       23,      0,      -9,    0,    0,    0,
     .  2,  0,  2,  2,  1,      -19,      0,      10,    0,    0,    0,
     . -2,  0,  2,  4,  1,      -20,      0,      10,    0,    0,    0,
     .  0,  1,  0,  2,  1,       18,      0,      -9,    0,    0,    0,
     .  1,  0,  0,  1,  0,      -33,      0,       0,    0,    0,    0,
     . -1,  0,  0,  4,  1,      -18,      0,       9,    0,    0,    0,
     . -1,  0,  4,  0,  1,       19,      0,      -9,    0,    0,    0,
     .  0,  0,  2, -3,  2,      -20,      0,       8,    0,    0,    0/
      data IX17/ 
     .  0,  0,  4,  0,  2,       19,      0,      -8,    0,    0,    0,
     .  2,  1,  0,  0,  0,      -28,      0,       0,    0,    0,    0,
     .  0,  0,  2, -4,  1,      -16,      0,       9,    0,    0,    0,
     . -1, -1,  2,  4,  2,      -17,      0,       7,    0,    0,    0,
     . -1, -2,  0,  2,  0,       27,      0,       0,    0,    0,    0,
     .  0,  0,  0,  4,  1,      -16,      0,       7,    0,    0,    0,
     .  0, -1,  0,  2,  1,      -14,      0,       7,    0,    0,    0,
     .  1,  0,  2,  4,  2,      -16,      0,       7,    0,    0,    0,
     . -2,  0,  0,  2,  2,       18,      0,      -8,    0,    0,    0,
     . -2,  2,  0,  2,  0,      -22,      0,       0,    0,    0,    0/
      data IX18/
     . -2, -1,  2,  0,  1,        9,      0,      -5,    0,    0,    0,
     . -3,  0,  0,  0,  1,      -14,      0,       7,    0,    0,    0,
     .  0,  0,  2,  0,  3,       20,      0,       0,    0,    0,    0,
     .  0,  0,  2,  4,  1,      -12,      0,       6,    0,    0,    0,
     .  0,  0,  4, -2,  1,       12,      0,      -7,    0,    0,    0,
     .  0, -2,  0,  2,  0,       21,      0,       0,    0,    0,    0,
     .  1,  0,  0, -1,  1,       17,      0,      -5,    0,   -3,    1,
     .  1,  1,  2,  2,  2,       15,      0,      -6,    0,    0,    0,
     .  3,  0,  2, -2,  1,       12,      0,      -7,    0,    0,    0,
     . -1, -1,  2,  0,  2,      -16,      0,       6,    0,    0,    0/
      data IX19/
     . -2, -1,  0,  2,  1,      -13,      0,       7,    0,    0,    0,
     .  0,  0,  0, -2,  2,       13,      0,      -5,    0,    0,    0,
     .  0, -2,  2,  2,  2,      -13,      0,       5,    0,    0,    0,
     .  1,  0,  0, -4,  1,      -12,      0,       6,    0,    0,    0,
     . -1,  1,  0,  2,  1,      -10,      0,       6,    0,    0,    0,
     . -2,  0,  0,  4,  1,       11,      0,      -6,    0,    0,    0,
     .  0,  0,  2, -1,  1,      -10,      0,       5,    0,    0,    0,
     .  0,  2,  0,  0,  1,       -9,      0,       5,    0,    0,    0,
     .  0,  2,  2, -2,  1,        8,      0,      -5,    0,    0,    0,
     .  2,  0,  0,  2,  1,       -9,      0,       5,    0,    0,    0/
      data IX20/ 
     .  2,  0,  0, -4,  1,      -11,      0,       5,    0,    0,    0,
     .  2,  0,  2, -4,  1,       10,      0,      -5,    0,    0,    0,
     . -1,  0, -2,  0,  1,      -10,      0,       5,    0,    0,    0,
     . -1,  1,  2,  0,  1,        9,      0,      -5,    0,    0,    0,
     . -1,  1,  2, -2,  1,      -11,      0,       5,    0,    0,    0,
     . -1, -1,  0,  4,  0,       15,      0,       0,    0,    0,    0,
     . -3,  0,  0,  4,  0,       16,      0,       0,    0,    0,    0,
     .  3,  0,  2,  2,  2,      -14,      0,       0,    0,    0,    0,
     . -2,  1,  0,  2,  0,        9,      0,       1,    0,   -1,    0,
     .  0,  2, -2,  2,  0,       -9,      0,       0,    0,    0,    0/
      data IX21/ 
     .  0, -1,  2,  4,  2,       -9,      0,       0,    0,    0,    0,
     .  0, -1,  2, -1,  2,        9,      0,       0,    0,    0,    0,
     .  1,  1,  0,  2,  0,      -10,      0,       0,    0,    0,    0,
     .  2,  0,  0, -2,  2,      -11,      0,       0,    0,    0,    0,
     .  2, -1,  2,  2,  2,       -9,      0,       0,    0,    0,    0,
     .  4,  0,  0,  0,  0,        9,      0,       0,    0,    0,    0,
     .  4,  0,  2, -2,  2,       12,      0,       0,    0,    0,    0,
     . -1,  0,  0,  3,  0,      -10,      0,       0,    0,    0,    0,
     . -1,  0,  4, -2,  2,       -9,      0,       0,    0,    0,    0,
     . -1, -2,  2,  2,  2,       -9,      0,       0,    0,    0,    0/
      data IX22/
     . -2, -1,  0,  4,  0,       12,      0,       0,    0,    0,    0,
     . -2, -1,  2,  4,  2,      -12,      0,       0,    0,    0,    0,
     .  0,  1,  2,  2,  1,        7,      0,       0,    0,    0,    0,
     .  0,  2,  2,  0,  2,        7,      0,       0,    0,    0,    0,
     .  0, -2,  2,  0,  2,       -8,      0,       0,    0,    0,    0,
     .  1,  0,  0,  4,  0,        8,      0,       0,    0,    0,    0,
     .  1,  0,  2,  2,  0,        8,      0,       0,    0,    0,    0,
     .  1,  0,  2, -4,  2,        7,      0,       0,    0,    0,    0,
     .  1, -1,  2,  2,  1,       -8,      0,       0,    0,    0,    0,
     .  1, -1,  2, -2,  2,       -7,      0,       0,    0,    0,    0/
      data IX23/
     .  1, -2,  0,  0,  0,        8,      0,       0,    0,    0,    0,
     .  2,  0,  0,  0,  2,       -8,      0,       0,    0,    0,    0,
     .  2,  1,  0, -2,  1,        8,      0,       0,    0,    0,    0,
     .  3,  0,  0,  0,  1,        7,      0,       0,    0,    0,    0,
     . -1,  0,  2,  1,  2,        8,      0,       0,    0,    0,    0,
     . -1,  0,  2,  3,  2,        8,      0,       0,    0,    0,    0,
     . -1,  0, -2,  4,  0,       -7,      0,       0,    0,    0,    0,
     . -1,  1,  2,  2,  1,        7,      0,       0,    0,    0,    0,
     . -1,  2,  0,  2,  0,       -8,      0,       0,    0,    0,    0,
     . -1, -1,  2, -1,  1,        7,      0,       0,    0,    0,    0/
      data IX24/
     . -2,  0,  2, -2,  1,       -8,      0,       0,    0,    0,    0,
     . -2,  0,  4,  0,  2,       -7,      0,       0,    0,    0,    0,
     . -2,  0, -2,  2,  0,        8,      0,       0,    0,    0,    0,
     . -2,  1,  2,  0,  1,        9,      0,       0,    0,    0,    0,
     . -3,  0,  2,  0,  1,       -8,      0,       0,    0,    0,    0,
     .  0,  1,  0,  1,  1,        5,      0,       0,    0,    0,    0,
     .  0, -1,  0,  4,  0,        6,      0,       0,    0,    0,    0,
     .  0, -1,  0, -2,  1,        5,      0,       0,    0,    0,    0,
     .  0, -2,  0,  0,  1,       -6,      0,       0,    0,    0,    0,
     .  1,  0,  2,  1,  1,        5,      0,       0,    0,    0,    0/
      data IX25/ 
     .  1,  0,  2, -3,  2,       -6,      0,       0,    0,    0,    0,
     .  1,  0, -2,  1,  0,       -7,      0,       0,    0,    0,    0,
     .  1,  1,  0,  1,  0,        5,      0,       0,    0,    0,    0,
     .  1, -1,  0, -2,  1,        6,      0,       0,    0,    0,    0,
     .  2,  0,  2, -1,  2,       -6,      0,       0,    0,    0,    0,
     .  2,  1,  2,  0,  1,        5,      0,       0,    0,    0,    0,
     .  2, -1,  2,  0,  1,       -6,      0,       0,    0,    0,    0,
     .  2, -1,  2, -2,  2,        5,      0,       0,    0,    0,    0,
     .  3,  0,  0,  2,  0,        5,      0,       0,    0,    0,    0,
     .  3, -1,  2,  0,  2,       -5,      0,       0,    0,    0,    0/
      data IX26/
     . -1, -1,  2,  0,  1,       -6,      0,       0,    0,    0,    0,
     . -2,  0,  0,  0,  2,        6,      0,       0,    0,    0,    0,
     . -2,  0,  0,  3,  0,       -5,      0,       0,    0,    0,    0,
     . -2,  0,  0, -2,  1,       -5,      0,       0,    0,    0,    0,
     . -2,  0,  2,  2,  0,       -6,      0,       0,    0,    0,    0,
     . -2, -1,  2,  0,  0,       -5,      0,       0,    0,    0,    0,
     . -2, -1,  2,  2,  2,        6,      0,       0,    0,    0,    0,
     .  0,  0,  1,  0,  0,        0,      0,       0,    0,    8,    0,
     .  0,  0,  1,  0,  1,        0,      0,       0,    0,  -16,  -14,
     . -1,  0,  1,  0,  0,        0,      0,       0,    0,   33,    0/
      data IX27/
     . -1,  0,  1,  0,  1,        0,      0,       0,    0, -105,  -89,
     . -1,  0,  1,  0,  2,        0,      0,       0,    0,   36,   18,
     . -1,  0,  1,  0,  3,        0,      0,       0,    0,   -6,    0/
C    
C           RFCN Mode   430.07 d 1979/ 1/ 1-1984/ 1/ 1
C Amplitudes:         .232     -.215      .000      .000mas
C Coefficients:      -.584     -.542     -.232      .215 mas
C           RFCN Mode   430.07 d 1984/ 1/ 1-1986/ 1/ 1
C Amplitudes:         .090     -.245      .000      .000mas
C Coefficients:      -.226     -.617     -.090      .245 mas
C           RFCN Mode   430.07 d 1986/ 1/ 1-1988/ 1/ 1
C Amplitudes:         .189     -.217      .000      .000mas
C Coefficients:      -.475     -.546     -.189      .217 mas
C           RFCN Mode   430.07 d 1988/ 1/ 1-1990/ 1/ 1
C Amplitudes:         .101     -.173      .000      .000mas
C Coefficients:      -.254     -.436     -.101      .173 mas
C           RFCN Mode   430.07 d 1990/ 1/ 1-1992/ 1/ 1
C Amplitudes:        -.025     -.170      .000      .000mas
C Coefficients:       .063     -.426      .025      .170 mas
C           RFCN Mode   430.07 d 1992/ 1/ 1-1994/ 1/ 1
C Amplitudes:        -.048     -.118      .000      .000mas
C Coefficients:       .120     -.296      .048      .118 mas
C           RFCN Mode   430.07 d 1994/ 1/ 1-1996/ 1/ 1
C Amplitudes:        -.004     -.076      .000      .000mas
C Coefficients:       .010     -.191      .004      .076 mas
C    
C** Initialize the values and sum over the series
C
      dpsi_lsu(1) = 0.0d0
      dpsi_lsu(2) = 0.0d0
      deps_lsu(1) = 0.0d0
      deps_lsu(2) = 0.0d0
C
      do 200 i = num_ls, 1, -1
C
C   Sum the mulitpliers by the arguments to the argument of nutation
          arg = 0.d0
          argdot = 0.d0
C
          do 150 j = 1,5
C
C   Sum into the argument for nutation.
             arg = arg + nutc_int(j,i)*ls_arg(j)
             argdot = argdot + nutc_int(j,i)*ls_argdot(j) 
 150      continue
C
          arg = mod(arg, 2.d0*pi)
C
C   Now add contributions to dpsi and deps
          dpsi_lsu(1) = dpsi_lsu(1)  
     .             + (nutc_int( 6,i)+ nutc_int(7,i)*cent)*dsin(arg)  
     .             +  nutc_int(10,i)*dcos(arg)
          deps_lsu(1) = deps_lsu(1)  
     .             + (nutc_int( 8,i)+ nutc_int(9,i)*cent)*dcos(arg)  
     .             +  nutc_int(11,i)*dsin(arg)
C
          dpsi_lsu(2) = dpsi_lsu(2) 
     .          +  nutc_int(7,i)*dsin(arg) 
     .          + (nutc_int( 6,i)+ nutc_int(7,i)*cent)*dcos(arg)*argdot 
     .          -  nutc_int(10,i)*dsin(arg)*argdot
          deps_lsu(2) = deps_lsu(2) 
     .          +  nutc_int(9,i)*dcos(arg) 
     .          - (nutc_int( 8,i)+ nutc_int(9,i)*cent)*dsin(arg)*argdot 
     .          +  nutc_int(11,i)*dcos(arg)*argdot
C
 200  continue
C
C     Convert values from micro-arc-sec to milli-arc-second
      dpsi_ls(1) = dpsi_lsu(1) * 1.d-3
      deps_ls(1) = deps_lsu(1) * 1.d-3
C     Convert from micro-arc-sec/century to milli-arc-second/sec
      dpsi_ls(2) = dpsi_lsu(2) * 1.d-3 / (SECDAY*36525.D0)
      deps_ls(2) = deps_lsu(2) * 1.d-3 / (SECDAY*36525.D0)
C 
      return
      end
C******************************************************************************
C
      SUBROUTINE plan_nut( CENT, dpsi, deps )   
      Implicit None
C
C     Routine to compute the planetary contribution to the nutations.
C     Coefficents from Tables XIV to XIX of Kinoshita, H. and J. Souchay,
C     Nutations for the rigid Earth, Celes. Mech. and Dynam. Astron,
C
C NEW Version based on: Corrections and new developments in rigid Earth
C     nutation theory: Lunisolar influence including indirect planetary
C     effects, J. Souchay and H. Kinioshita, Astron. and Astrophys., 1995.
C (Version here based on data files: KSRE95_FIG_PSI.DAT KSRE95_FIG_EPS.DAT
C  and generated with ks_plan.f)
C MOD for KSV_1996_2: Corrected rate of change argument for lmc in 
C     plan_angles subroutine (see comments in routine) 
C MOD for KSV_1996_3: Replaced all planetary arguments with values from 
C     Simon et al., 1994.   
C
C APPROXIMATIONS: The Oppolzer terms have not been added (should be < 0.005 
C                mas), and Contributions from a non-rigid Earth have not been
C                computed. For many of these terms the contribution arises
C                from the perturbation of the Earth's orbit and therefore 
C                there will be no deformation effects.
C
C PASSED VARIABLES
C
C INPUT Values
C   Cent   - The number of Julian centuries elapsed since the
C                epoch January 1.5, 2000. (centuries)
C
C OUTPUT Values
C   dpsi   - Contribution to the nutation in longitude and their time 
C            derivatives (mas, mas/sec). Should be added to the standard 
C            nutation in longitude values.
C   deps   - Contribution to the nutation in obliquity and their time 
C            derivatives. (mas, mas/sec). Should be added to the standard 
C            nutation in obliquity values.
C
      Real*8 dpsi(2), deps(2), CENT     
C 
C LOCAL VARIABLES
C     plan_arg(10) - Values of the planetary arguments (Lve, Le, Lma,  LJ, 
C                    Lsa, pa, D, F, lm, Om). (rads) (same order as KS1990)
C     plan_rat(10) - Rates of changes of the planetary arguments. (rad/year)
C                    Used to get periods for the terms.
C
      Real*8 plan_arg(10), plan_rat(10)
C
C** Get the fundamental arguments at this epoch
      call plan_angles( CENT, plan_arg, plan_rat )
C
C   Now compute the contributions of the planetery nutations by 
C     summing over the series.
      call eval_plan_nut( plan_arg, plan_rat, dpsi, deps )
C
      return
      end
C
C*******************************************************************************
      SUBROUTINE eval_plan_nut( plan_arg, plan_rat, dpsi, deps ) 
      Implicit None
C
C     Routine to compute the planetary nutations by summing over the KS1990
C     coefficients. The coefficients and their arguments saved here as 
C     integers are in micro-arc-seconds.  
C
C NOTE: plan_angles must be called before routine.
C
C PARAMETERS:
C
C num_plan  - Number of contributions to the planetary nutations
      integer*4 num_plan 
      parameter (num_plan = 112)
C
C PASSED PARAMETERS:
C
C INPUT
C   plan_arg(10)  - Ten planetary arguments including pa as given (KS1990.)
C                   (rad)
C   plan_rat(10)  - Rates of change of the arguments (used to get periods for
C                   the planetary nutations). (rad/yr)
C
C OUTPUT:
C   dpsi, deps   - Contributions to nutations in longitude and obliquity, 
C                  and their derivatives (mas, mas/sec).
C
      Real*8 plan_arg(10), plan_rat(10), dpsi(2), deps(2)
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C           VARIABLES 'FROM':
C              1. PI      -  PI
C              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
C                            COORDINATE TIME DAY.  (SEC/DAY) 
C 
C LOCAL VARIABLES:
C
C IX01-IX12(14,10) - Integer values of the planetary arguments and 
C                    values (micro-arc-seconds for values)
      Integer*4 IX01(14,10), IX02(14,10), IX03(14,10),
     .          IX04(14,10), IX05(14,10), IX06(14,10),
     .          IX07(14,10), IX08(14,10), IX09(14,10),
     .          IX10(14,10), IX11(14,10), IX12(14, 2)
C
      Integer*4 Plan_int(14,num_plan)
C
C  i and j   - Counters to loop over the coeffients and the argumemts
      Integer*4 i,j
C
C  arg       - Final summed argumemt for the nutations contributions (rads)
C  dargdt    - Rate of change of the argument (rads/yr)
C  period    - Period of the nutation in days.
C  amp       - Total Amplitude of the planetary nutation. (To be used for
C              sorting size)
C
      Real*8 arg, dargdt, period, amp
C
      equivalence (Plan_int(1,  1),IX01)
      equivalence (Plan_int(1, 11),IX02)
      equivalence (Plan_int(1, 21),IX03)
      equivalence (Plan_int(1, 31),IX04)
      equivalence (Plan_int(1, 41),IX05)
      equivalence (Plan_int(1, 51),IX06)
      equivalence (Plan_int(1, 61),IX07)
      equivalence (Plan_int(1, 71),IX08)
      equivalence (Plan_int(1, 81),IX09)
      equivalence (Plan_int(1, 91),IX10)
      equivalence (Plan_int(1,101),IX11)
      equivalence (Plan_int(1,111),IX12)
C
      data IX01/   
     .   0,   2,  0, -2,  0,  0,  2,  0, -2,  1,   28,   0,    0,  -15,
     .  18, -16,  0,  0,  0,  0,  0,  0, -1,  0,   23,  10,    0,    0,
     .  -8,  12,  0,  0,  0,  0, -1,  1,  0,  1,  120,  60,   32,  -64,
     .   0,   0,  2,  0,  0,  0,  1, -1,  0,  0,   27,  -8,    0,    0,
     .   0,  -1,  2,  0,  0,  0,  0,  0,  0,  1,  -46, -43,  -23,   25,
     .   3,  -4,  0,  0,  0,  0,  1,  0, -1,  0,    0,  13,    0,    0,
     .   5,  -6,  0,  0,  0,  0,  2, -2,  0,  0,    0,  23,    0,    0,
     .   6,  -8,  0,  0,  0,  0,  2,  0, -2,  0,    8,  -2,    0,    0,
     .   0,   8,-15,  0,  0,  0,  0,  0,  0,  0,    5,  -2,    0,    0,
     .   0,  -2,  0,  3,  0,  0, -2,  0,  2,  1,    9,  -2,    0,   -5/
      data IX02/ 
     .   0,   2,  0, -3,  0,  0,  2,  0, -2,  0,  -35,  -6,    0,    0,
     .   0,   1,  0, -1,  0,  0,  1,  0, -1,  0,   -5,   0,    0,    0,
     .   0,   1,  0,  1,  0,  0,  1, -1,  0,  0,   -2,  -8,    0,    0,
     .   0,   0,  0,  1,  0,  0,  0,  0,  0,  1,    2,  -7,    0,    0,
     .   0,  -1,  0,  0, -1,  0, -1,  1,  0,  1,   17,   8,    4,   -9,
     .   0,   0,  0,  0,  1,  0,  0,  0,  0,  0,    1,   6,    0,    0,
     .   0,   0,  0,  0,  1,  1,  0,  0,  0,  0,    5,   0,    0,    0,
     .   0,  -1,  0,  0,  1,  0, -1,  1,  0,  1,   -7,  -1,    0,    0,
     .   8, -13,  0,  0,  0,  0,  0,  0,  0,  1,    5,   7,    0,    0,
     .  18, -16,  0,  0,  0,  0,  0,  0, -1,  1,   -7,  -3,    0,    0/
      data IX03/  
     .   0,   0,  0, -2,  5,  0,  0,  0,  0,  1,    8,   2,    0,    0,
     .   0,  -4,  8, -3,  0,  0,  0,  0,  0,  1,    8,  30,   16,   -4,
     .   0,   4, -8,  3,  0,  0,  0,  0,  0,  1,   -8,  29,   16,    4,
     .   0,   0,  0,  2, -5,  0,  0,  0,  0,  1,   -7,   2,    0,    0,
     .   0,   2,  0, -2,  0,  0,  2,  0, -2,  0,  -44,   0,    0,    0,
     . -18,  16,  0,  0,  0,  0,  0,  0,  1,  1,    6,  -3,    0,    0,
     .  -8,  13,  0,  0,  0,  0,  0,  0,  0,  1,   -4,   6,    0,    0,
     .   0,   0, -2,  0,  0,  0, -1,  1,  0,  1,   27,   8,    4,  -15,
     .   0,   1, -2,  0,  0,  0,  0,  0,  0,  0,  -46,  44,    0,    0,
     .   0,  -2,  2,  0,  0,  0, -1,  1,  0,  1,    0,  -5,    0,    0/
      data IX04/ 
     .   0,   0,  0,  0,  2,  1,  0,  0,  0,  0,    5,  10,    6,   -3,
     .   0,  -1,  0,  0,  2,  0, -1,  1,  0,  1,   -5, -11,   -6,    3,
     .   0,   0,  0,  0,  2,  2,  0,  0,  0,  0,  -12,   0,    0,    5,
     .  -5,   6,  0,  0,  0,  0, -2,  2,  0,  1,   -2, -44,  -23,    0,
     .   0,   2,  0, -3,  0,  0,  2,  0, -2,  1,   -5,   0,    0,    0,
     .   0,  -1,  0, -1,  0,  0, -1,  1,  0,  1,   -5,  19,   10,    3,
     .   0,   0,  0,  1,  0, -1,  0,  0,  0,  0,    2,   6,    0,    0,
     .   0,   0,  0,  1,  0,  0,  0,  0,  0,  0,   -8,  25,    0,    0,
     .   0,   0,  0,  1,  0,  1,  0,  0,  0,  0,    0,   5,    0,    0,
     .   0,  -1,  0,  1,  0,  0, -1,  1,  0,  1,    0,  -5,    0,    0/
      data IX05/ 
     .   3,  -3,  0,  0,  0,  0,  2,  0, -2,  0,  -14,   0,    0,    0,
     .   0,  -2,  0,  2,  0,  0, -2,  0,  2,  1,    5,   0,    0,    0,
     .   3,  -5,  0,  0,  0,  0,  0,  0,  0,  0,  -22,   7,    0,    0,
     .   3,  -5,  0,  0,  0, -1,  0,  0,  0,  0,   -1,  -7,    3,   -1,
     .   3,  -5,  0,  0,  0, -2,  0,  0,  0,  0,  211,   0,    0,   96,
     .   0,   2, -4,  0,  0,  0,  0,  0,  0,  0,   -8,  14,    0,    0,
     .   0,   2, -4,  0,  0, -2,  0,  0,  0,  0,    5,   0,    0,    0,
     .   5,  -8,  0,  0,  0, -2,  0,  0,  0,  0,    0, -26,   13,    0,
     .  -5,   7,  0,  0,  0,  0, -1,  1,  0,  1,   14,   3,    1,   -7,
     .   0,   0,  0,  2,  0,  1,  0,  0,  0,  0,    4,  27,   12,   -2/
      data IX06/ 
     .   0,  -1,  0,  2,  0,  0, -1,  1,  0,  1,   -3, -14,   -8,    1,
     .   0,   0,  0,  2,  0,  2,  0,  0,  0,  0, -116,   0,    0,   51,
     .  -3,   3,  0,  0,  0,  0, -2,  2,  0,  1,   12,   0,    0,   -6,
     .   0,  -2,  0,  2,  0,  0, -2,  2,  0,  1,    5,   0,    0,    0,
     .   2,  -3,  0,  0,  0,  0,  0,  0,  0,  0,    0,  63,    0,    0,
     .   0,   0,  0,  3,  0,  2,  0,  0,  0,  0,  -12,   0,    0,    5,
     .   1,  -2,  0,  0,  0,  0,  0,  0,  0,  0,    0,  -9,    0,    0,
     .   0,   2, -3,  0,  0,  0,  0,  0,  0,  0,   -8,   5,    0,    0,
     .   0,   1, -1,  0,  0,  0,  0,  0,  0,  0,   -6,   0,    0,    0,
     .   4,  -7,  0,  0,  0, -2,  0,  0,  0,  0,    0,   6,    0,    0/
      data IX07/
     .   4,  -6,  0,  0,  0, -2,  0,  0,  0,  0,  -52,   0,    0,  -23,
     .   4,  -6,  0,  0,  0, -1,  0,  0,  0,  0,    0,   9,   -5,    1,
     .   1,  -1,  0,  0,  0,  0,  0,  0,  0,  0,  153,   0,    0,    0,
     .   1,  -1,  0,  0,  0,  1,  0,  0,  0,  0,    0,  -6,   -5,   -1,
     .   0,   1,  0, -3,  0, -2,  0,  0,  0,  0,  -11,   0,    0,   -5,
     .   2,  -4,  0,  0,  0, -1,  0,  0,  0,  0,    0,  -8,    0,    0,
     .   2,  -4,  0,  0,  0, -2,  0,  0,  0,  0,   47,   0,    0,   20,
     .   0,   1,  0, -2,  0,  0,  0,  0,  0,  0,  -18,  27,    0,    0,
     .   0,   3, -4,  0,  0,  0,  0,  0,  0,  0,   -8,   5,    0,    0,
     .   3,  -4,  0,  0,  0,  0,  0,  0,  0,  0,    0,  29,    0,    0/
      data IX08/ 
     .   0,   1,  0, -1,  0,  0,  0,  0,  0,  0, -123,  -3,    0,    0,
     .   0,   2, -2,  0,  0,  0,  0,  0,  0,  0,  -38,   0,    0,    0,
     .   0,   1,  0,  0, -1,  0,  0,  0,  0,  0,   -8,   0,    0,    0,
     .   0,   0,  2,  0,  0,  2,  0,  0,  0,  0,   -7,   0,    0,    0,
     .   0,   1,  0,  1,  0,  2,  0,  0,  0,  0,  -25,   0,    0,   11,
     .   3,  -6,  0,  0,  0, -2,  0,  0,  0,  0,    0,  -5,    0,    0,
     .   5,  -7,  0,  0,  0, -2,  0,  0,  0,  0,  -21,   0,    0,   -9,
     .   0,   1,  0,  2,  0,  2,  0,  0,  0,  0,   -3,  -5,    0,    0,
     .   2,  -2,  0,  0,  0, -1,  0,  0,  0,  0,    0,   5,    0,    0,
     .   2,  -2,  0,  0,  0,  0,  0,  0,  0,  0,  -60,   0,    0,    0/
      data IX09/ 
     .   1,  -3,  0,  0,  0, -2,  0,  0,  0,  0,  -11,   0,    0,   -5,
     .   1,  -3,  0,  0,  0, -1,  0,  0,  0,  0,    0,  -5,    0,    0,
     .   0,   2,  0, -3,  0,  0,  0,  0,  0,  0,    8,   2,    0,    0,
     .   2,  -5,  0,  0,  0, -2,  0,  0,  0,  0,    0, -13,    6,    0,
     .   6,  -8,  0,  0,  0, -2,  0,  0,  0,  0,  -12,   0,    0,   -5,
     .   0,   2,  0, -2,  0,  0,  0,  0,  0,  0,   39,   0,    0,    0,
     .   3,  -3,  0,  0,  0,  0,  0,  0,  0,  0,   10,   0,    0,    0,
     .   3,  -3,  0,  0,  0,  2,  0,  0,  0,  0,    5,  -2,    0,    0,
     .   0,   2,  0, -1,  0,  2,  0,  0,  0,  0,  -15,  -3,   -1,    7,
     .   0,   3, -2,  0,  0,  2,  0,  0,  0,  0,    8,  -7,    0,    0/
      data IX10/ 
     .   8, -15,  0,  0,  0, -2,  0,  0,  0,  0,   -6, -10,    4,   -3,
     .   0,   6, -8,  3,  0,  2,  0,  0,  0,  0,   12, -42,  -18,   -5,
     .   0,   2,  0,  0,  0,  2,  0,  0,  0,  0,   -9,   0,    0,    0,
     .   0,   2, -8,  3,  0, -2,  0,  0,  0,  0,   12, -42,   18,    5,
     .   8, -11,  0,  0,  0,  2,  0,  0,  0,  0,   -6, -10,   -4,   13,
     .   0,   1,  2,  0,  0,  2,  0,  0,  0,  0,   -8,  -7,    0,    0,
     .   0,   2,  0,  1,  0,  2,  0,  0,  0,  0,   17,  -1,    0,   -7,
     .   3,  -7,  0,  0,  0, -2,  0,  0,  0,  0,    7,  -2,    0,    0,
     .   2,  -1,  0,  0,  0,  2,  0,  0,  0,  0,    0, -17,   -8,    0,
     .   7,  -9,  0,  0,  0, -2,  0,  0,  0,  0,   -7,   0,    0,    0/
      data IX11/ 
     .   4,  -4,  0,  0,  0,  0,  0,  0,  0,  0,   11,   0,    0,    0,
     .   1,   1,  0,  0,  0,  2,  0,  0,  0,  0,  -30,   0,    0,   13,
     .   0,   3,  0, -2,  0,  2,  0,  0,  0,  0,    7,  -9,   -4,   -3,
     .   3,  -2,  0,  0,  0,  2,  0,  0,  0,  0,    0, -11,   -5,    0,
     .   0,   3,  0, -1,  0,  2,  0,  0,  0,  0,   52,   2,    0,  -22,
     .   0,   4, -2,  0,  0,  2,  0,  0,  0,  0,   14,   0,    0,   -6,
     .   8, -10,  0,  0,  0, -2,  0,  0,  0,  0,   -5,   0,    0,    0,
     .   5,  -5,  0,  0,  0,  0,  0,  0,  0,  0,    7,   0,    0,    0,
     .   2,   0,  0,  0,  0,  2,  0,  0,  0,  0,   39,   0,    0,  -17,
     .   0,   4,  0, -2,  0,  2,  0,  0,  0,  0,  -18,   0,    0,    8/
      data IX12/ 
     .  18, -16,  0,  0,  0,  0,  0,  2, -1,  2,  -13,  -6,   -3,    5,
     . -18,  16,  0,  0,  0,  0,  0,  2,  1,  2,   13,  -6,   -3,   -5/
C   Last line has only 2 values
C
C** Initialize the values and sum over the series
      dpsi(1) = 0.0d0
      dpsi(2) = 0.0d0
      deps(1) = 0.0d0
      deps(2) = 0.0d0
C
      do 200 i = num_plan, 1, -1
C         
C  Sum the mulitpliers by the arguments to the argument of nutation
          arg = 0.d0
          dargdt = 0.d0
          do 150 j = 1,10
C
C  Planetary values
             arg = arg + Plan_int(j,i)*plan_arg(j)
             dargdt = dargdt + Plan_int(j,i)*plan_rat(j)
 150      continue
C
          arg = mod(arg, 2.d0*pi)
          period = (2*pi/dargdt)*365.25d0
C
C** Now add contributions to dpsi and deps
          dpsi(1) = dpsi(1) + (Plan_int(11,i)*dsin(arg) +
     .                   Plan_int(12,i)*dcos(arg)) * 1.d-3
          deps(1) = deps(1) + (Plan_int(13,i)*dsin(arg) +
     .                   Plan_int(14,i)*dcos(arg)) * 1.d-3
C
          dpsi(2) = dpsi(2) + (Plan_int(11,i)*dcos(arg)*dargdt 
     .                 - Plan_int(12,i)*dsin(arg)*dargdt) * 1.d-3
          deps(2) = deps(2) + (Plan_int(13,i)*dcos(arg)*dargdt 
     .                 - Plan_int(14,i)*dsin(arg)*dargdt) * 1.d-3
C
 200  continue
C      Convert rate terms from mas/year to mas/sec
          dpsi(2) = dpsi(2) / (365.25D0 * SECDAY) 
          deps(2) = deps(2) / (365.25D0 * SECDAY) 
C
      return
      end
C*******************************************************************************
C
      SUBROUTINE plan_angles( CENT, plan_arg, plan_rat )
      Implicit None
C
C     Routine to compute planetary arguments for planetary nutation. 
C     Longitudes of the major planets are computed from Simon et al., 1994.
C     Units are kept the same as Simon et al., and converted during 
C     calculations.
C Reference:
C     Simon, J. L., Bretagnon, P., Chapront, J., Chapront-Touze, M.,
C         Francou, G., Laskar, J., 1994, "Numerical Expressions for 
C         Precession Formulae and Mean Elements for the Moon and
C         Planets," Astron. Astrophys., 282, pp. 663-683.
C
C PHYSICAL CONSTANTS NEEDED FOR SD_COMP
C   sec360      - number of arcseconds in 360 degreees.
      Real*8  sec360
      parameter ( sec360        = 1296000.d0           )
C
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C           VARIABLES 'TO':
C              1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                           CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
C              2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000. 
C                           (DAYS) (DJ2000 = 2451545.D0) 
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C           VARIABLES 'FROM':
C              1. PI      -  PI
C              2. SECDAY  -  THE NUMBER OF COORDINATE TIME SECONDS PER
C                            COORDINATE TIME DAY. (SEC/DAY) 
C
C PASSED VARIABLES
C
C INPUT
C   Cent - The number of Julian centuries since January 1.5, 2000. (centuries)
C
C OUTPUT
C   plan_arg(10)  - Planetary arguments for longitudes Venus, Earth, Mars,
C           Jupiter, Saturn, pa, D, F, lm and Om in order given (rads)
C   plan_rat(10)  - Rates of change of the arguments (used to get periods 
C           for the planetary nutations). (rad/yr)
      Real*8 Cent, plan_arg(10), plan_rat(10)
C
C LOCAL VARIABLES 
C
C See comments in data statements about units for each type (from
C Simon et al., units) Final angles are returned in rads and rads/yr
C      vl           - Venus longitude (rads)
C      vlc(2)       - coefficients for computing vl
C      tl           - Earth longitude (rads)
C      tlc(2)       - coefficients for computing tl
C      ml           - Mars longitude (rads)
C      mlc(2)       - coefficients for computing ml
C      jl           - Jupliter longitude (rads)
C      jlc(2)       - coefficients for computing jl
C      sl           - Saturn longitude (rads)
C      slc(2)       - coefficients for computing sl
C      pa           - pa (rads)
C      pac(2)       - Coefficients for computing pa from KS1990.
C                     (Values converted from rates and acceleration
C                     in 1000's year to centuries).
C      Dr           - Mean elongation of the Moon from the Sun (rads)
C      drc(2)       - Coefficients for computing dc
C      Fr           - Moon's mean longitude minus Om (rad)
C      Frc(2)       - Coefficients for computing Fr
C      lm           - Mean longitude of moon minus mean longitude
C                     of perigee (rads)
C      lmc(2)       - Coefficients for computing lm
C      Om           - Longitude of the ascending node of the moon's
C                     mean orbit on the elliptic (rad)
C      Omc          - Coefficients for computing Om
C
      Real*8 vl, vlc(2), tl, tlc(2), ml, mlc(2), jl, jlc(2), sl,
     .    slc(2), pa, pac(2), Dr, drc(2), Fr, frc(2), 
     .    lm, lmc(2), Om, Omc(2)
C    
C MOD TAH 960625: Changed all the arguments to values from Simon et al. 1994.
C   The Mean elements referred to mean dynamical ecliptic and equinox of 
C   data are from Table 5.9 of Simon et al. We use the same units as in paper.
C
C                   (degrees)         ("/thousand years)
      data vlc  / 181.979 800 85d0, 2 106 691 666.319 89d0   /
      data tlc  / 100.466 456 83d0, 1 296 027 711.034 29d0   /
      data mlc  / 355.432 999 58d0,   689 101 069.330 69d0   /
      data jlc  /  34.351 518 74d0,   109 306 899.894 53d0   /
      data slc  /  50.077 444 30d0,    44 046 398.470 38d0   /
C
C     Pa is from Equation (6).  (IAU76 Masses)  
C                  "/thousand yrs  "/(thousand yrs)**2
      data pac  /    50 288.200d0, 111.202 2d0 /
C
C     Delaunay variables from Equation 3.5.b (values same as 
C     ls_angles).  NOTES: As in Simon et al., 1994 the angle
C     rates here are in Centuries not thousand years. 
C                        "                     "/Julian Cent.
      data drc  /  1 072 260.703 690d0,  1 602 961 601.2090d0 /
      data frc  /    335 779.526 232d0,  1 739 527 262.8478d0 /
      data lmc  /    485 868.249 036d0,  1 717 915 923.2178d0 /
      data omc  /    450 160.398 036d0,     -6 962 890.5431d0 /
C
C     Compute arguments 
C     For longitudes (degree-> seconds and time in thousand yrs)
C     Final result in radians.
      vl  = ((vlc(1)*3600.d0 + vlc(2)*cent/10.d0)/sec360)*2*pi
      tl  = ((tlc(1)*3600.d0 + tlc(2)*cent/10.d0)/sec360)*2*pi
      ml  = ((mlc(1)*3600.d0 + mlc(2)*cent/10.d0)/sec360)*2*pi
      jl  = ((jlc(1)*3600.d0 + jlc(2)*cent/10.d0)/sec360)*2*pi
      sl  = ((slc(1)*3600.d0 + slc(2)*cent/10.d0)/sec360)*2*pi
      pa  = ((pac(1)*cent/10 + pac(2)*(cent/10)**2)/sec360)*2*pi
      dr  = ((drc(1) + drc(2)*cent)/sec360)*2*pi
      fr  = ((frc(1) + frc(2)*cent)/sec360)*2*pi
      lm  = ((lmc(1) + lmc(2)*cent)/sec360)*2*pi
      om  = ((omc(1) + omc(2)*cent)/sec360)*2*pi
C
C** Now save the values
      plan_arg( 1) = vl
      plan_arg( 2) = tl 
      plan_arg( 3) = ml 
      plan_arg( 4) = jl 
      plan_arg( 5) = sl 
      plan_arg( 6) = pa 
      plan_arg( 7) = dr 
      plan_arg( 8) = fr 
      plan_arg( 9) = lm 
      plan_arg(10) = om 
C     
C** Save the rates of change.
      plan_rat( 1) = ((vlc(2)/sec360)*2*pi)/1000.d0
      plan_rat( 2) = ((tlc(2)/sec360)*2*pi)/1000.d0
      plan_rat( 3) = ((mlc(2)/sec360)*2*pi)/1000.d0
      plan_rat( 4) = ((jlc(2)/sec360)*2*pi)/1000.d0
      plan_rat( 5) = ((slc(2)/sec360)*2*pi)/1000.d0
      plan_rat( 6) = (((pac(1) + 2*pac(2)*cent/10)/sec360)
     .                               *2*pi)/1000.d0
      plan_rat( 7) = (drc(2)/100.d0/sec360)*2*pi
      plan_rat( 8) = (frc(2)/100.d0/sec360)*2*pi
      plan_rat( 9) = (lmc(2)/100.d0/sec360)*2*pi
      plan_rat(10) = (omc(2)/100.d0/sec360)*2*pi
C
      return
      end
C*******************************************************************************
C
      SUBROUTINE fcn_nut ( CENT, dpsi_fcn, deps_fcn )
      Implicit None
C
C     Routine to compute the contributions of the freely excited
C     FCN mode to the nutations in longitude and obliquity.
C
C RESTRICTIONS: This term represents as free excitation mode and
C               therefore will change with time (in much the same
C               way that the Chandler Wobble changes). The
C               frequency of the FCN used here is accurate, but
C               coefficients used will depend on time.  The values
C               are interpolated over the 1979-1995 interval with
C               the first or last values being used outside of these times.
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C     VARIABLES 'FROM': 
C       1.  PI      -  PI
C 
C PARAMETERS:
C   DJ2000      - Julian date of J2000
C   solar_to_sidereal   - Conversion from solar days to sidereal days.
C   num_fcn     - Number of FCN amplitudes (linear interpolation between
C                 values).
C
c     real*8 DJ2000, solar_to_sidereal
      real*8 solar_to_sidereal
      integer*4 num_fcn
c     parameter ( DJ2000        = 2451545.d0           )
      parameter ( solar_to_sidereal = 1.002737909d0 )
      parameter ( num_fcn       = 7 )
C
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C           VARIABLES 'TO':
C              1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                           CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
C              2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000. 
C                           (DAYS) (DJ2000 = 2451545.D0) 
C
C INPUT Values
C   Cent - The time in Julian centuries since January 1.5, 2000. (centuries)
C
C OUTPUT Values
C   dpsi_fcn  - Contribution to the nutation in longitude and its time 
C          derivative. (mas, mas/sec). Add to standard nutation.
C   deps_fcn  - Contribution to the nutation in obliquity and its time 
C          derivative. (mas, mas/sec). Add to standard nutation.
C
      Real*8 dpsi_fcn(2), deps_fcn(2), CENT
C
C LOCAL VARIABLES
C   epoch       - Julian date (Computed from Cent).
C   fcn_freq    - Freqency of the FCN mode (cycles per sidreal day).
C   fcn_arg     - Argument for the fcn mode computed from J2000 (rad).
C   fcn_tabl(2, num_fcn) - Amplitude for the fcn free exciation. These
C                 are converted to nutation in longitude and obliquity. (mas)
C   fcn_ampl(2) - Interpolated FCN amplitude. (mas)
C   fcn_jd(num_fcn) - Starting epochs for the fcn amplitudes. (JD) 
C   sine        - Sine of the mean obliquity of the ecliptic. (A constant
C                 value can be used here since the changes are small for
C                 this constribution i.e., between 1980 and 2000 the error
C                 in the nutation in longitude is only 0.05 micro-arc-sec
C   dt          - Time difference between epoch and tabular interval. (days)
C   dt_tab      - Time difference in table values.
C   dfcn_amp(2) - Change in FCN amplitude between tabular points. (mas)
C
      Real*8 epoch, fcn_freq, fcn_arg, fcn_ampl(2), 
     .       fcn_tabl(2,num_fcn), sine, fcn_jd(num_fcn),
     .       dt, dt_tab, dfcn_amp(2), fcn_argdot
C
C   i  - A counter used in do loop to find the correct pair of amplitudes to use
      integer*4 i
C
      data  fcn_freq  /   -1.00231887299d0 /
C
C     Time dependent values 
      data  fcn_jd   / 2443874.50d0,  2445700.50d0, 2446431.50d0,
     .                 2447161.50d0,  2447892.50d0, 2448622.50d0,
     .                 2449353.50d0  / 
C
      data  fcn_tabl /  .232d0,  -.215d0,     .090d0,  -.245d0,
     .                  .189d0,  -.217d0,     .101d0,  -.173d0,
     .                 -.025d0,  -.170d0,    -.048d0,  -.118d0,
     .                 -.004d0,  -.076d0 / 
C
      data  sine      /   0.3977771203d0 /
C
         epoch = CENT*CENTJ + DJ2000
C
C** Find out which table values we should use.
      if( epoch.le.fcn_jd(1) ) then
          fcn_ampl(1) = fcn_tabl(1,1)
          fcn_ampl(2) = fcn_tabl(2,1)
      else if( epoch.ge. fcn_jd(num_fcn) ) then
          fcn_ampl(1) = fcn_tabl(1,num_fcn)
          fcn_ampl(2) = fcn_tabl(2,num_fcn)
      else
          do 200 i = 1, num_fcn-1
             if( epoch.ge.fcn_jd(i) .and. epoch.lt.fcn_jd(i+1) ) then
                 dt = epoch - fcn_jd(i)
                 dt_tab = fcn_jd(i+1) - fcn_jd(i)
                 dfcn_amp(1) = fcn_tabl(1,i+1) - fcn_tabl(1,i)
                 dfcn_amp(2) = fcn_tabl(2,i+1) - fcn_tabl(2,i)
                 fcn_ampl(1) = fcn_tabl(1,i) + (dfcn_amp(1)/dt_tab)*dt
                 fcn_ampl(2) = fcn_tabl(2,i) + (dfcn_amp(2)/dt_tab)*dt
             end if
 200      continue
      end if
C
C** Get the argument for the FCN mode at this times
C
      fcn_arg = -2*pi*(1.d0+fcn_freq)*solar_to_sidereal*(epoch-DJ2000)
      fcn_argdot = -2*pi*(1.d0+fcn_freq)*solar_to_sidereal
C
      dpsi_fcn(1) = (-fcn_ampl(1)*sin(fcn_arg) + 
     .             fcn_ampl(2)*cos(fcn_arg))/sine
      deps_fcn(1) =  -fcn_ampl(1)*cos(fcn_arg) - 
     .             fcn_ampl(2)*sin(fcn_arg) 
C
      dpsi_fcn(2) = (-fcn_ampl(1)*cos(fcn_arg)*fcn_argdot - 
     .             fcn_ampl(2)*sin(fcn_arg)*fcn_argdot)/sine
      deps_fcn(2) =   fcn_ampl(1)*sin(fcn_arg)*fcn_argdot - 
     .             fcn_ampl(2)*cos(fcn_arg)*fcn_argdot 
C  Convert rates from mas/day to mas/sec
      dpsi_fcn(2) = dpsi_fcn(2)/86400.D0
      deps_fcn(2) = deps_fcn(2)/86400.D0
C
      return
      end
C
C*******************************************************************************
      SUBROUTINE prec_nut( CENT, dpsi_prec, deps_prec )
      Implicit None
C
C     Routine to evaluate the corrections to the nutations in longitude
C     and obliquity due to the corrections to the IAU-1976 Luni-solar
C     precession constant and the secular rate of change of the obliquity
C     of the ecliptic.
C
C PARAMETERS:
C
c     Real*8 DJ2000
c     Parameter ( DJ2000        = 2451545.d0           )
C
      Real*8 CENTJ, DJ2000, EC(4), ARGP(2,6) 
      Integer*4 NOT, NOP, IDP(6)
      COMMON / NUTCM / CENTJ, DJ2000, EC, ARGP, NOT, NOP, IDP
C           VARIABLES 'TO':
C              1. CENTJ   - THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                           CENTURY. (DAYS/CENTURY) (CENTJ = 36525.D0)
C              2. DJ2000  - THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000. 
C                           (DAYS) (DJ2000 = 2451545.D0) 
C
C PASSED VARIABLES
C
C INPUT Values
C   Cent   - Time in Julian centuries since January 1.5, 2000. (centuries)
C
C OUTPUT Values
C dpsi_prec - Contribution to the nutation in longitude and its time
C             derivative. (mas, mas/sec). Should be added to standard 
C             nutation in longitude values. Valid only when the IAU-1976
C             precession constant used to compute the transformation to 
C             mean system.
C deps_prec - Contribution to the nutation in obliquity and its time
C             derivative. (mas, mas/sec). Should be added to standard 
C             nutation in obliquity values. Valid only when the IAU-1976
C             precession constant used to compute the transformation to 
C             mean system.
C
      Real*8 Cent, dpsi_prec(2), deps_prec(2)
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C     VARIABLES 'FROM': 
C       1.  SECDAY  -  Seconds per day.
C 
C LOCAL VARIABLES
C   DpsiDt - Correction to precession constant as a linear rate of 
C            change of nutation in longitude. (arc-second/century)
C   DepsDt - Correction to rate of change of oblquity
C            (arc-second/century)
      Real*8 DpsiDt,  DepsDt
      data  DpsiDt  /  -0.2957d0  /
      data  DepsDt  /  -0.0227d0  /
C
      dpsi_prec(1) = DpsiDt*cent*1000.d0
      deps_prec(1) = DepsDt*cent*1000.d0
C
      dpsi_prec(2) = DpsiDt*1000.d0
      deps_prec(2) = DepsDt*1000.d0
C Convert rates to mas/sec
      dpsi_prec(2) = dpsi_prec(2) / (SECDAY*36525.D0)
      deps_prec(2) = deps_prec(2) / (SECDAY*36525.D0) 
C 
      return
      end
