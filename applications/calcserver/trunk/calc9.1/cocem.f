      SUBROUTINE OCEA
      IMPLICIT None
C
C     OCEA adds entries to the table of contents for the ocean
C     loading module text message and for the contributions arrays.
C
C     OCEA Program Interface:
C
C       Common blocks used - NONE
C
C       Database access -
C              ACCESS CODES:
C                1. 'OCE MESS'  -  THE DATA BASE ACCESS CODE FOR THE OCEAN
C                                  LOADING MODULE TEXT MESSAGE.
C                2. 'OCE CONT'  -  THE DATA BASE ACCESS CODE FOR THE OCEAN
C                                  LOADING CONTRIBUTIONS ARRAY.
C                3. 'OCE CFLG'  -  THE DATA BASE ACCESS CODE FOR THE OCEAN 
C                                  LOADING MODULE FLOW CONTROL MESSAGE.
C                4. 'OCE DELD'  -  THE DATA BASE ACCESS CODE FOR THE SITE
C                                  DEPENDENT OCEAN LOADING DISPLACEMENTS AND
C                                  VELOCITIES.
C                5. 'OCE HORZ   -  THE DATABASE ACCESS CODE FOR THE SITE 
C                                  DEPENDENT CONTRIBUTIONS TO THE DELAY AND 
C                                  RATE FOR THE HORIZONTAL CORRECTIONS.
C                6. 'OCE VERT   -  THE DATABASE ACCESS CODE FOR THE SITE
C                                  DEPENDENT CONTRIBUTIONS TO THE DELAY AND
C                                  RATE FOR THE VERTICAL CORRECTIONS.
C
C 1.2.5   EXTERNAL INPUT/OUTPUT - NONE
C
C 1.2.6   SUBROUTINES INTERFACE  -
C                CALLER SUBROUTINES: TOCUP
C                CALLED SUBROUTINES: ADDA,ADDR
C
C 1.2.7   CONSTANTS USED - NONE
C
C 1.2.8   PROGRAM VARIABLES - NONE
C
C 1.2.9   PROGRAMMER - HAROLD M. SCHUH 10/08/83
C                      SAVITA GOEL 06/03/87 (CDS FOR A900)
C                      Jim Ryan 89.06.22 (Horizontal mods)
C                      Jim Ryan 89.12.12 UNIX-like database interface
C                               implimented.
C                      David Gordon 94.04.18 Converted to Implicit None.
C                      David Gordon 94.06.08 Some format statements fixed,
C                               single and double quotes reversed.
C
C 1.3   OCEA PROGRAM STRUCTURE
C
C   Add for ocean loading module text message.
      CALL ADDA (1,'OCE MESS','Ocean loading message definition',
     1     40, 1, 1 )
C
C   Add for module flow control message.
      CALL ADDA (1,'OCE CFLG','Ocean load flow control mess def',
     1     40, 1, 1 )
C
C   Add for module contributions array. This is the old CALC-6 contribution,
C   that is, one site-independent delay and rate for all ocean loading effects.
      CALL ADDR (2,'OCE CONT','Ocean loading contributions def.',
     1     2, 1, 1 )
C
C   Add for site depending displacement and velocity array.
      CALL ADDR (2,'OCE DELD','Ocean load site depndnt displace',
     .     3, 2, 2 )
C
C   Add for site dependent contribution to the delay and rate for the horizontal
C   corrections.
      CALL ADDR (2,'OCE HORZ','Site-dep ocean cont - horizontal',
     .     2, 2, 1 )
C
C   Add for site dependent contribution to the delay and rate for the vertical
C   corrections.
      CALL ADDR (2,'OCE VERT','Site-dep ocean cont - vertical  ',
     .     2, 2, 1 )
C
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE OCEI
      IMPLICIT None
C
C     OCEI is the ocean loading module input and initialization section.
C
C     OCEI Program Interface
C
C       Calling Sequence - NONE
C
C       Common Blocks used -
C
      INCLUDE 'ccon.i'
C       VARIABLES 'FROM':
C         1. KOCEC - THE OCEAN LOADING MODULE FLOW CONTROL FLAG.
C
      INCLUDE 'cuser.i'
C       Variables from:
C         1. Calc_user   - Calc user type. 'A' for Mark III/SOLVE analysis.
C                          'C' for VLBI correlator.
C         2. Apply_ocean - Switch to apply ocean loading to theoreticals
C                          for correlator usage. 'Y' to apply (recommended),
C                          'N' for do not apply.
C
C 3.2.3 PROGRAM SPECIFICATIONS -
C
      INTEGER*2      LOCEM(40),  LON(40),  LOFF(40),  LOX(40),  LHOR(40)
      CHARACTER*40 C_LOCEM(2), C_LON(2), C_LOFF(2), C_LOX(2), C_LHOR(2)
      EQUIVALENCE (LOCEM,C_LOCEM),(LON,C_LON),(LOFF,C_LOFF),
     .            (LOX,C_LOX), (LHOR,C_LHOR)
C
C 3.2.4 DATA BASE ACCESS -
C
C            'PUT' VARIABLES:
C              1. LOCEM(40)  -  THE OCEAN LOADING MODULE TEXT MESSAGE.
C              2. LON(40)    -  THE OCEAN LOADING MODULE TURNED ON MESSAGE.
C              3. LOFF(40)   -  THE OCEAN LOADING MODULE TURNED OFF MESSAGE.
C              4. LOX(40)    -  THE OCEAN LOADING SPECIAL SETUP MESSAGE.
C              5. LHOR(40)   -  "PROCEED EVEN WITHOUT HORIZONTAL CAT" MESSAGE.
C
C            ACCESS CODES:
C              1. 'OCE MESS' -  THE DATA BASE ACCESS CODE FOR THE OCEAN LOADING
C                               MODULE TEXT MESSAGE.
C              2. 'OCE CFLG' -  THE DATA BASE ACCESS CODE FOR THE OCEAN LOADING
C                               MODULE FLOW CONTROL MESSAGE.
C
C 3.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 
C 3.2.6 SUBROUTINE INTERFACE -
C              CALLER SUBROUTINE: INITL
C              CALLED SUBROUTINE: PUTA
C
C 3.2.7 CONSTANTS USED - NONE
C
C 3.2.8 PROGRAM VARIABLES - NONE
C
C 3.2.9 PROGRAMMER - HAROLD SCHUH 10/08/83
C                    JIM RYAN      6/20/84 (MODIFIED TEXT MESSAGE)
C                    Greg Cook     6/29/89 Inserted horizontal code
C                    JIM RYAN      6/29/89 (MODIFIED TEXT MESSAGE)
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                  implimented.
C                    David Gordon 94.04.18 Converted to Implicit None.
C                    David Gordon 98.10.16 Added 'cuser.i' include file
C                                 and code to use ocean loading included
C                                 message for correlator users. 
C
      DATA C_LOCEM  /
     .'Ocean loading module - Version 2. Last m',
     .'odified 94.08.22 by D. Gordon, GSFC     '/
C
      DATA C_LON    /
     .'Ocean loading module is turned on - cont',
     .'ributions not applied to theoretical.   '/

      DATA C_LOFF   /
     .'Ocean loading module is turned off.     ',
     .'                                        '/
      DATA C_LOX    /
     .'Ocean loading special setup - contributi',
     .'ons are applied to the theoreticals.    '/
C
      DATA C_LHOR   /
     .'Ocean loading proceeding even if horizon',
     .'tal catalog missing. Contributions only.'/
C
C   Call 'PUTA' to place the ocean loading module text message into db.
      CALL PUTA ('OCE MESS      ',LOCEM, 40, 1, 1 )
C
C   Check that the flow control variable has a legitimate value.
      IF(KOCEC.LT.0 .OR. KOCEC.GT.3) CALL CKILL(6HOCEI  ,1,1)
C
      IF (KOCEC .EQ. 2 .or. (Calc_user.eq.'C' .and. Apply_ocean.eq.'Y')) 
     *   Then
          CALL PUTA ('OCE CFLG      ',LOX , 40, 1, 1)
          Go to 112
      ENDIF
      IF (KOCEC .EQ. 0) CALL PUTA ('OCE CFLG      ',LON , 40, 1, 1)
      IF (KOCEC .EQ. 1) CALL PUTA ('OCE CFLG      ',LOFF, 40, 1, 1)
      IF (KOCEC .EQ. 3) CALL PUTA ('OCE CFLG      ',LHOR, 40, 1, 1)
 112   Continue
C
      RETURN
      END
C
C*****************************************************************************
      SUBROUTINE OCEG ( CFSITE, UT1, OCEAMP, OCEPHS, R2000,
     1                  XJD, TCTOCF, TSKIP, XLOADP, XLOADV)
      IMPLICIT None
C
C     OCEG is the ocean loading geometry section.
C
C     Restrictions - NONE
C
C     References -
C       1. Merit Standards, April 1981, Second draft, Appendices 7 and 11.
C       2. C.C.GOAD, J. GEOPHYS. RES., 85, P.2679-2683, MAY 1980.
C
C     OCEG program interfaces -
C
C       CALLING SEQUENCE -
C
C       INPUT VARIABLES:
C         1. CFSITE(3,2)  - THE CRUST FIXED SITE VECTORS AT EACH OBSERVATION 
C                           SITE. (M)
C         2. UT1          - THE UT1 TIME OF THE DAY. (SEC)
C         3. OCEAMP(11,3,2) - OCEAN LOADING AMPLITUDES FOR 11 TIDES AT EACH
C                             OBSERVATION SITE. (M)
C                  ( J,K,L)      K=1 : VERTICAL
C                                K=2 : EAST-WEST
C                                K=3 : NORTH-SOUTH DIRECTION
C         4. OCEPHS(11,3,2) - OCEAN LOADING PHASES. (RAD)
C         5. R2000(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION MATRIX
C                           AND ITS FIRST TWO CT TIME DERIVATIVES.
C                           (UNITLESS,1/SEC,1,SEC**2)
C         6. XJD          - THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN 
C                           QUESTION. (DAYS)
C         7. TCTOCF(3,3,2)- THE ROTATION MATRIX WHICH ROTATES THE TOPOCENTRIC
C                           REFERENCE SYSTEM TO THE CRUST FIXED REFERENCE SYSTEM
C                           AT EACH OBSERVATION SITE. (UNITLESS)
C
C       OUTPUT VARIABLES:
C         1. XLOADP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                           POSITION VECTORS DUE TO OCEAN LOADING EFFECTS. (M)
C                           FOR THE DEFAULT CALC SETUP (KOCEC=0), THE OCEAN
C                           LOADING CORRECTIONS ARE NOT ADDED TO THE
C                           THEORETICAL OBSERVATIONS, BUT ONLY PASSED OUT AS
C                           CONTRIBUTIONS, SO THIS ARRAY IS NORMALLY ZEROED OUT.
C         2. XLOADV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                           VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS. (M/S)
C                           SEE ABOVE COMMENTS FOR XLOADP.
C
C 4.2.2 COMMON BLOCKS USED -
C
      Real*8 ZLOADP(3,2), ZLOADV(3,2), ZLOADP_HOR(3,2),
     .       ZLOADV_HOR(3,2), ZLOADP_VER(3,2), ZLOADV_VER(3,2)
      COMMON / OCECM / ZLOADP, ZLOADV, ZLOADP_HOR, 
     .       ZLOADV_HOR, ZLOADP_VER, ZLOADV_VER
C       VARIABLES TO:
C         1. ZLOADP(3,2)  - SAME AS XLOADP ABOVE, EXCEPT CONTAINS VALUES UNLESS
C                           THE OCEAN LOADING MODULE IS TURNED OFF (KOCEC=1).
C                           STORED AS A SEPARATE VARIABLE IN COMMON IN ORDER TO
C                           PASS IT TO THE CONTRIBUTIONS SUBROUTINE.
C         2. ZLOADV(3,2)  - SAME AS XLOADV. SEE COMMENTS FOR ZLOADP.
C         3. ZLOADP_HOR(3,2) - SAME AS ZLOADP EXCEPT CONTAINS ONLY THE
C                           HORIZONTAL OCEAN LOADING EFFECTS.
C         4. ZLOADV_HOR(3,2)  - SAME AS ZLOADV EXCEPT CONTAINS ONLY THE
C                           HORIZONTAL OCEAN LOADING EFFECTS.
C         5. ZLOADP_VER(3,2)  - SAME AS ZLOADP EXCEPT CONTAINS ONLY THE VERTICAL
C                           OCEAN LOADING EFFECTS.
C         6. ZLOADV_VER(3,2)  - SAME AS ZLOADV EXCEPT CONTAINS ONLY THE VERTICAL
C                           OCEAN LOADING EFFECTS.
C
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C       VARIABLES 'FROM' -
C         1. CONVD   -  THE CONVERSION FACTOR FROM DEGREES TO radians
C
       INCLUDE 'ccon.i'
C       VARIABLES 'FROM':
C         1. KOCEC   -  THE OCEAN LOADING MODULE FLOW CONTROL FLAG
C         2. KOCED   -  THE OCEAN LOADING MODULE DEBUG OUTPUT FLAG
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.
C
       INCLUDE 'cuser.i'
C       Variables from:
C            1. Calc_user   - Calc user type. 'A' for Calc/SOLVE analysis.
C                             'C' for VLBI correlator.
C            2. Apply_ocean - Switch to apply ocean loading to theoreticals
C                             for correlator usage. 'Y' to apply (recommended),
C                             'N' for do not apply.
C
       Real*8 speed(11)
       Common /tide_speed/ speed
C            SPEED(11)- COEFFICIENTS FOR THE COMPUTATION OF ARGUMENTS IN THE
C                       FOLLOWING ORDER OF TIDES: (RAD/SEC)
C                       M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA.
C                       (These are actually the rates of change of the 11 tide
C                        angles - see subroutine OCARG).
C
C 4.2.3 PROGRAM SPECIFICATIONS -
C
      Integer*4 I, J, K, L
      Real*8 ANGLE(11), CFDIS(3,2), CFSITE(3,2), DELTAO(3,2),
     1       OCEAMP(11,3,2), OCEPHS(11,3,2), R2000(3,3,3),
     2       XLOADP(3,2), XLOADV(3,2), TCDIS(3,2), CFVEL(3,2),
     3       ZLOAV1(3,2), ZLOAV2(3,2), TCVEL(3,2), TCTOCF(3,3,2),
     4       DELTAV(3,2), TCDIS_HOR(3,2), TCDIS_VER(3,2),
     5       TCVEL_HOR(3,2), TCVEL_VER(3,2), CFDIS_HOR(3,2), 
     6       CFDIS_VER(3,2), CFVEL_HOR(3,2), CFVEL_VER(3,2),
     7       ZLOAV1_HOR(3,2),ZLOAV1_VER(3,2), ZLOAV2_HOR(3,2),
     8       ZLOAV2_VER(3,2), XHOLD(3,2,2), UT1, XJD
      Integer*4 TSKIP
C
      Save ANGLE
C
C 4.2.4 DATA BASE ACCESS
C
C       PUT VARIABLES:
C         1. XHOLD(3,2,2) - THE TOTAL OCEAN LOADING DISPLACEMENT VECTORS IN
C                           J2000.0 COORDINATES. THE INDICES RUN OVER (X,Y,Z),
C                          (POSITION, VELOCITY), (SITE 1, SITE 2) (METERS)
C
C 4.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
C
C 4.2.6 SUBROUTINE INTERFACE  -
C       CALLER SUBROUTINES: DRIVG
C       CALLED SUBROUTINES: OCARG, ANG, DATAN2, DCOS, DSIN, VECRT, VECAD 
C 
C 4.2.7 CONSTANTS USED - NONE 
C 
C 4.2.8 PROGRAM VARIABLES - 
C 
C         1. ANGLE(11)  - THE ANGULAR ARGUMENTS FOR SCHWIDERSKI COMPUTATION IN
C                         THE FOLLOWING ORDER:
C                         M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA
C         2. CFDIS(3,2) - THE CRUST FIXED GEOCENTRIC DISPLACEMENTS DUE TO OCEAN
C                         LOADING AT EACH SITE. (M)
C         3. CFVEL(3,2) - THE CRUST FIXED GEOCENTRIC VELOCITIES DUE TO OCEAN
C                         LOADING AT EACH SITE. (M/SEC)
C         4. DELTAO(3,2)- THE UP (I=1), EAST-WEST (I=2), AND NORTH-SOUTH (I=3)
C                  (I,J)  DISPLACEMENTS DUE TO OCEAN LOADING AT EACH SITE. (M)
C         5. ZLOAV1(3,2)- THE CONTRIBUTION TO THE CORRECTION TO THE J2000.0
C                         GEOCENTRIC SITE VELOCITY VECTOR DUE TO THE EFFECT OF
C                         THE CHANGED SITE POSITION. (M/SEC)
C         6. ZLOAV2(3,2)- THE CONTRIBUTION TO THE CORRECTION TO THE J2000.0
C                         GEOCENTRIC SITE VELOCITY VECTOR DUE TO THE EFFECT OF
C                         THE CHANGING OCEAN LOADING DISPLACEMENT. (M/SEC)
C         7. TCDIS(3,2) - THE TOPOCENTRIC EARTH CRUSTAL DISPLACEMENTS DUE TO
C                         OCEAN LOADING AT EACH SITE. (M)
C         8. TCVEL(3,2) - THE TOPOCENTRIC EARTH CRUSTAL VELOCITIES DUE TO OCEAN
C                         LOADING AT EACH SITE. (M/SEC)
C         9. DELTAV(3,2)- THE VELOCITY OF EACH SITE DUE TO OCEAN LOADING. (M/S)
C        10. ZLOAV1_HOR(3,2)- LIKE ZOAV1, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
C        11. ZLOAV2_HOR(3,2)- LIKE ZOAV2, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
C        12. ZLOAV1_VER(3,2)- LIKE ZOAV1, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
C        13. ZLOAV2_VER(3,2)- LIKE ZOAV2, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
C
C 4.2.9 PROGRAMMER - HAROLD M. SCHUH 10/08/83
C                    JIM RYAN        06/20/84  CHANGED TO CONTRIBUTION ONLY
C                    DAVID GORDON    08/30/84  PUT CONVERSION TO J2000.0
C                                    POSITIONS AND VELOCITIES INTO STANDARD FORM
C                    DAVID GORDON    09/05/84  ADDED DANGL AND COMPLETED
C                                    COMPUTATION OF RATES
C                    DAVID GORDON    03/18/85  FIXED BUG FOR KOCEC.NE.0
C                    GREGG COOKE     12/22/88 TOPO OCL DISP. & VEL. PUT INTO 
C                                    DATABASE)
C                    LOTHAR MOHLMANN 03/23/89 HORIZONTAL OCEAN LOADING 
C                                    DISPLACEMENTS)
C                    GREG COOK       06/20/89  Put into CALC 7.0
C                    JIM RYAN        06/23/89  Code added to break out
C                                    horizontal and vertical.
C                    Jim Ryan        89.12.12 UNIX-like database interface
C                                    implimented.
C                    Jim Ryan        90.01.24 Logic for KOCEC=3 added.
C                    DSR&JWR         90.04.24 Changed the signs on the hor.
C                                    displacements and velocities. Code
C                                    orginally supplied by Bonn group was in 
C                                    error due to error in algorithm supplied by
C                                    Sherneck at Uppsala. (Sherneck after
C                                    consulting with Duncan Agnew at Scripps
C                                    discovered the signs on both horizontal 
C                                    components were in error.)
C                    Jim Ryan        90:11:20 Bug in logic for putting local
C                                    displacements in the database fixed.
C                                    Debug statments fixed.
C                    Jim Ryan        91.11.04 Array ANGFAC in 'OCARG' modified
C                                    to make it consistent with IERS stardards.
C                    David Gordon    94.04.18 Converted to Implicit None.
C                    David Gordon    94.08.22 Added common block for SPEED(11)
C                     & John Gipson  from OCARG to replace variable DANGL(11), 
C                                    which was incorrectly being used as the
C                                    derivatives of the tide angles.
C                    David Gordon    98.08.04 Added code for handling Geocenter
C                                    station, zeroes out necessary quantities.
C                    David Gordon    98.10.16 Added 'cuser.i' include file
C                                    and code to add ocean loading to the 
C                                    theoreticals for correlator users. 
C
C     OCEG PROGRAM STRUCTURE
C
C   The ocean loading geometry for sites #1 and #2 are calculated separately by
C   running a loop over the two sites.
C
C   Compute the time dependent angular momentum for the 11 tidal arguments.
      IF (TSKIP.ne.1) 
     *    CALL OCARG ( UT1, XJD, ANGLE )
C
C   Loop twice for the calculation of the height displacement due to ocean
C   loading at sites #1 and 2.
C
      DO  L = 1, 2
C
C Check for Geocenter site
      IF (L .eq. Nzero) Then
       Do K=1,3
         DELTAO(K,L) = 0.D0
         DELTAV(K,L) = 0.D0
          TCDIS(K,L) = 0.D0
          TCVEL(K,L) = 0.D0
         ZLOADP(K,L) = 0.D0
         ZLOADP_HOR(K,L) = 0.D0
         ZLOADP_VER(K,L) = 0.D0
         ZLOADV(K,L) = 0.D0
         ZLOADV_HOR(K,L) = 0.D0
         ZLOADV_VER(K,L) = 0.D0
       Enddo
        Go to 150
      ENDIF
C
C   Compute the topocentric displacement DELTAO and DELTAV for each site due to
C    ocean loading (for the 11 main tides in 3 directions).
C
        DO K = 1, 3
          DELTAO(K,L) = 0.D0
          DELTAV(K,L) = 0.D0
          DO J = 1,11
            DELTAO(K,L) =   OCEAMP(J,K,L)  * DCOS(ANGLE(J)
     .                    - OCEPHS(J,K,L)) + DELTAO(K,L)
            DELTAV(K,L) = - OCEAMP(J,K,L)  * DSIN(ANGLE(J)
     .                    - OCEPHS(J,K,L)) * speed(J)
     .                    + DELTAV(K,L)
          ENDDO
        ENDDO
C
C   Change the signs on the horizontal displacements and velocities.
C     (See explanation in the subroutine history above.)
C
        DELTAO(2,L) = -DELTAO(2,L)
        DELTAV(2,L) = -DELTAV(2,L)
        DELTAO(3,L) = -DELTAO(3,L)
        DELTAV(3,L) = -DELTAV(3,L)
C
C   Load the topocentric ocean loading displacement vector. Also load up vector
C   that contains only the vertical and only the horizontal effects.
        TCDIS(1,L) = DELTAO(1,L)
        TCDIS(2,L) = DELTAO(2,L)
        TCDIS(3,L) = DELTAO(3,L)
C
        TCDIS_HOR(1,L) = 0.D0
        TCDIS_HOR(2,L) = DELTAO(2,L)
        TCDIS_HOR(3,L) = DELTAO(3,L)
C
        TCDIS_VER(1,L) = DELTAO(1,L)
        TCDIS_VER(2,L) = 0.D0
        TCDIS_VER(3,L) = 0.D0
C
C   Load the topocentric ocean loading velocity vector. As above load like
C   quantities for horizontal and vertical.
        TCVEL(1,L) = DELTAV(1,L)
        TCVEL(2,L) = DELTAV(2,L)
        TCVEL(3,L) = DELTAV(3,L)
C
        TCVEL_HOR(1,L) = 0.D0
        TCVEL_HOR(2,L) = DELTAV(2,L)
        TCVEL_HOR(3,L) = DELTAV(3,L)
C
        TCVEL_VER(1,L) = DELTAV(1,L)
        TCVEL_VER(2,L) = 0.D0
        TCVEL_VER(3,L) = 0.D0
C
C   Rotate the displacements and velocities to the crust fixed geocentric
C   coordinate system.
        CALL VECRT(TCTOCF(1,1,L),TCDIS(1,L),CFDIS(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVEL(1,L),CFVEL(1,L))
C
        CALL VECRT(TCTOCF(1,1,L),TCDIS_HOR(1,L),CFDIS_HOR(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVEL_HOR(1,L),CFVEL_HOR(1,L))
C
        CALL VECRT(TCTOCF(1,1,L),TCDIS_VER(1,L),CFDIS_VER(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVEL_VER(1,L),CFVEL_VER(1,L))
C
C   Rotate the crust fixed geocentric displacements to J2000.0.
        CALL VECRT(R2000(1,1,1),CFDIS(1,L)    ,ZLOADP(1,L)    )
        CALL VECRT(R2000(1,1,1),CFDIS_HOR(1,L),ZLOADP_HOR(1,L))
        CALL VECRT(R2000(1,1,1),CFDIS_VER(1,L),ZLOADP_VER(1,L))
C
C   Rotate the crust fixed geocentric velocities to J2000.0.
C
C   Compute the contribution to the J2000.0 velocities due to the effect of
C   changed site positon due to ocean loading.
        CALL VECRT(R2000(1,1,2),CFDIS    (1,L),ZLOAV1    (1,L))
        CALL VECRT(R2000(1,1,2),CFDIS_HOR(1,L),ZLOAV1_HOR(1,L))
        CALL VECRT(R2000(1,1,2),CFDIS_VER(1,L),ZLOAV1_VER(1,L))
C
C   Compute the contribution to the J2000.0 velocities due to the effect of the
C   changing ocean loading displacement.
        CALL VECRT(R2000(1,1,1),CFVEL    (1,L),ZLOAV2    (1,L))
        CALL VECRT(R2000(1,1,1),CFVEL_HOR(1,L),ZLOAV2_HOR(1,L))
        CALL VECRT(R2000(1,1,1),CFVEL_VER(1,L),ZLOAV2_VER(1,L))
C
C   Add the contributions
        CALL VECAD(ZLOAV1    (1,L),ZLOAV2    (1,L),ZLOADV    (1,L))
        CALL VECAD(ZLOAV1_HOR(1,L),ZLOAV2_HOR(1,L),ZLOADV_HOR(1,L))
        CALL VECAD(ZLOAV1_VER(1,L),ZLOAV2_VER(1,L),ZLOADV_VER(1,L))
C
 150   CONTINUE
C
      ENDDO
C
C   Check KOCEC to determine if the ocean loading module is to be turned off.
C
C   Handle the normal case (Contributions only)
      IF (KOCEC .EQ. 0 .or. KOCEC.eq.3) THEN
        DO  L = 1,2
          DO  I = 1,3
            XLOADP(I,L) = 0.D0
            XLOADV(I,L) = 0.D0
          ENDDO
        ENDDO
      ENDIF
C
C   Handle the ocean loading module off case.
      IF (KOCEC .EQ. 1) THEN
        DO  L = 1,2
          DO  I = 1,3
            ZLOADP(I,L) = 0.D0
            ZLOADP_HOR(I,L) = 0.D0
            ZLOADP_VER(I,L) = 0.D0
            XLOADP(I,L) = 0.D0
  
            ZLOADV(I,L) = 0.D0
            ZLOADV_HOR(I,L) = 0.D0
            ZLOADV_VER(I,L) = 0.D0
            XLOADV(I,L) = 0.D0
          ENDDO
        ENDDO
      ENDIF
C
C   Handle the special case (KOCEC=2) where the effect of ocean loading is 
C   added to the theoretical.
      IF (KOCEC.EQ.2 .or. (Calc_user.eq.'C' .and. Apply_ocean.eq.'Y')) 
     *         THEN
C       print *, 'Ocean loading being applied'
        DO  L = 1,2
          DO  I = 1,3
            XLOADP (I,L) = ZLOADP(I,L)
            XLOADV (I,L) = ZLOADV(I,L)
          ENDDO
        ENDDO
      ENDIF
C
C  Put the site dependent topocentric ocean loading displacement and velocity in
C  the database only if the module is turned on.
C   Note the use of the indices: (Up, East, North) by (P,V) by (site1,site2).
      IF (KOCEC .NE. 1) THEN
        DO I = 1,3
          DO L = 1,2
            XHOLD(I,1,L) = TCDIS(I,L)
            XHOLD(I,2,L) = TCVEL(I,L)
          ENDDO
        ENDDO
        CALL PUT4 ('OCE DELD      ',XHOLD, 3, 2, 2 )
      ENDIF
C
C  Check KOCED to determine if debug output is requested.
  400 IF( KOCED .EQ. 0) GO TO 500
      WRITE (6,9100)
 9100 FORMAT (1X, 'Debug output for subroutine OCEG.' )
      WRITE(6,8)' ANGLE     ' ,ANGLE
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CFDIS     ' ,CFDIS
      WRITE(6,8)' DELTAO    ' ,DELTAO
      WRITE(6,8)' DELTAV    ' ,DELTAV
      WRITE(6,8)' TCDIS     ' ,TCDIS
      WRITE(6,8)' TCDIS_HOR ' ,TCDIS_HOR
      WRITE(6,8)' TCDIS_VER ' ,TCDIS_VER
      WRITE(6,8)' TCVEL     ' ,TCVEL
      WRITE(6,8)' TCVEL_HOR ' ,TCVEL_HOR
      WRITE(6,8)' TCVEL_VER ' ,TCVEL_VER
      WRITE(6,8)' ZLOAV1    ' ,ZLOAV1
      WRITE(6,8)' ZLOAV1_HOR' ,ZLOAV1_HOR
      WRITE(6,8)' ZLOAV1_VER' ,ZLOAV1_VER
      WRITE(6,8)' ZLOAV2_HOR' ,ZLOAV2_HOR
      WRITE(6,8)' ZLOAV2_VER' ,ZLOAV2_VER
C      WRITE(6,9200) ZLOADP, ZLOADP_HOR, ZLOADP_VER,
C     *                   ZLOADV, ZLOADV_HOR, ZLOADV_VER,
C     *                   XLOADP, XLOADV, CFSITE, UT1, OCEAMP,
C     *                   ((((OCEPHS(J,K,L)/CONVD),J=1,11),K=1,3),L=1,2),
C     *                   R2000,CFDIS, CFDIS_HOR,CFDIS_VER
C
C 9200 FORMAT (' ZLOADP  = ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' ZLOADP_HOR',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' ZLOADP_VER',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' ZLOADV  = ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' ZLOADV_HOR',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' ZLOADV_VER',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' XLOADP  = ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' XLOADV  = ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' CFSITE  = ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' UT1     = ',/, D25.16, 10X , /, 1X ,
C     *        ' OCEAMP  = ',/,2( 3(11F10.5,/),/),
C     *        ' OCEPHS-deg',/,2( 3(11F10.5,/),/),
C     *        ' R2000   = ',/,3( 3(3D25.16,/),/,),
C     *        ' CFDIS   = ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' CFDIS_HOR ',/,2 ( 3 ( D25.16, 10X ), /),
C     *        ' CFDIS_VER ',/,2 ( 3 ( D25.16, 10X ), /))
      WRITE(6,'(" XJD     = ", F15.2, 1X,"KOCEC   = ",I3)')
     *  XJD,KOCEC
C
C     Normal termination.
 500  RETURN
      END
C
C******************************************************************************
      SUBROUTINE OCEP
      IMPLICIT None
C
C     This is a "STUB" routine which does nothing but return control to DRIVRP.
C     There are no ocean loading partials.
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE OCEC (STAR)
      IMPLICIT None
C
C    OCEC is the contributions section of the ocean loading module. It computes
C    the contributions to the delay and delay rate due to ocean loading effects.
C
C     OCEC program interface -
C      CALLING SEQUENCE -
C       INPUT VARIABLES:
C         1. STAR(3) - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C       OUTPUT VARIABLES: NONE
C
C 6.2.2 COMMON BLOCKS -
C
      Real*8 ZLOADP(3,2), ZLOADV(3,2), ZLOADP_HOR(3,2),
     .       ZLOADV_HOR(3,2), ZLOADP_VER(3,2), ZLOADV_VER(3,2)
      COMMON / OCECM / ZLOADP, ZLOADV, ZLOADP_HOR, 
     .       ZLOADV_HOR, ZLOADP_VER, ZLOADV_VER
C       VARIABLES FROM:
C         1. ZLOADP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                           POSITION VECTORS DUE TO OCEAN LOADING EFFECTS. (M)
C         2. ZLOADV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE 
C                           VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS. (M/S)
C         3. ZLOADP_HOR(3,2)- SAME AS ZLOADP EXCEPT CONTAINS ONLY THE HORIZONTAL
C                           OCEAN LOADING EFFECTS.
C         4. ZLOADV_HOR(3,2)- SAME AS ZLOADV EXCEPT CONTAINS ONLY THE HORIZONTAL
C                           OCEAN LOADING EFFECTS.
C         5. ZLOADP_VER(3,2)- SAME AS ZLOADP EXCEPT CONTAINS ONLY THE VERTICAL
C                           OCEAN LOADING EFFECTS.
C         6. ZLOADV_VER(3,2)- SAME AS ZLOADV EXCEPT CONTAINS ONLY THE VERTICAL
C                           OCEAN LOADING EFFECTS.
C
      INCLUDE 'cphys.i'
C       VARIABLES 'FROM':
C         1. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C
      INCLUDE 'ccon.i'
C       VARIABLES 'FROM' :
C         1. KOCEC  - THE OCEAN LOADING MODULE FLOW CONTROL FLAG
C         2. KOCED  - THE OCEAN LOADING MODULE DEBUG OUTPUT FLAG
C
C 6.2.3  PROGRAM SPECIFICATIONS -
C
      Integer*4 K
      Real*8 BASCOR(3,2), DOCEC(2), STAR(3), CONTRIB_HOR(2,2),
     *       CONTRIB_VER(2,2), DOTP
C
C 6.2.4  DATA BASE ACCESS -
C       'PUT' VARIABLES:
C         1. DOCEC(2)         - THE OCEAN LOADING CONTRIBUTION TO THE DELAY
C                               AND TO THE DELAY RATE. (SEC, SEC/SEC)
C         2. CONTRIB_HOR(2,2) - THE SITE-DEPENDENT CONTRIBUTION TO THE DELAY
C                               AND RATE FROM HORIZONTAL DISPLACEMENTS. THE
C                               first index runs over the sites and the second
C                               over the delay and rate. (sec, sec/sec)
C         3. CONTRIB_VER(2,2) - THE SITE-DEPENDENT CONTRIBUTION TO THE DELAY
C                               AND RATE FROM VERTICAL DISPLACEMENTS. THE 
C                               first index runs over the sites and the second
C                               over the delay and rate. (sec, sec/sec)
C       ACCESS CODES:
C         1. 'OCE CONT' -  THE DATA BASE ACCESS CODE FOR THE OCEAN LOADING
C                          MODULE CONTRIBUTIONS ARRAY.
C         2. 'OCE HORZ' -  THE DATABASE ACCESS CODE FOR THE SITE DEPENDENT
C                          CONTRIBUTIONS TO THE DELAY AND RATE FOR THE
C                          HORIZONTAL CORRECTIONS.
C         3. 'OCE VERT' -  THE DATABASE ACCESS CODE FOR THE SITE DEPENDENT 
C                          CONTRIBUTIONS TO THE DELAY AND RATE FOR THE VERTICAL
C                          CORRECTIONS.
C
C 6.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
C
C 6.2.6 SUBROUTINE INTERFACE -
C         CALLER SUBROUTINES: DRIVC
C         CALLED SUBROUTINES: DOTP, PUT4, VECSB
C
C 6.2.7 CONSTANTS USED - VLIGHT
C
C 6.2.8 PROGRAM VARIABLES -
C         1. BASCOR(3,2)  -  THE CORRECTION TO THE J2000.0 BASELINE POSITION AND
C                            VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS. 
C                            (M, M/SEC)
C
C 6.2.9 PROGRAMMER - HAROLD M. SCHUH 10/08/83
C                    JIM RYAN         6/20/83
C                    JIM RYAN        06/23/89  Code added to break out
C                                      horizontal and vertical.
C                    Jim Ryan 89:10:05 CPHYS common made an include file
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                                      implimented.
C                    David Gordon 94.04.18 Converted to Implicit None.
C                    David Gordon 94.06.27 Reversed indices of CONTRIB_HOR and
C                                      CONTRIB_VER for consistency elsewhere in
C                                      Calc (and SOLVE). First index is now
C                                      sites, second is delay and rate.
C
C     OCEC PROGRAM STRUCTURE
C
C  Compute the contributions.
C
C   Compute the corrections to the J2000.0 baseline position and velocity 
C   vectors due to ocean laoding effects.
C
      CALL VECSB ( ZLOADP(1,1), ZLOADP(1,2), BASCOR(1,1) )
      CALL VECSB ( ZLOADV(1,1), ZLOADV(1,2), BASCOR(1,2) )
C
C   Complete the calculation of the contributions.
      DO K = 1,2
        DOCEC(K) = DOTP(BASCOR(1,K),STAR)/VLIGHT
      Enddo
C
c     CONTRIB_HOR(1,1) =  DOTP( ZLOADP_HOR(1,1), STAR ) / VLIGHT
c     CONTRIB_HOR(2,1) =  DOTP( ZLOADV_HOR(1,1), STAR ) / VLIGHT
c     CONTRIB_HOR(1,2) = -DOTP( ZLOADP_HOR(1,2), STAR ) / VLIGHT
c     CONTRIB_HOR(2,2) = -DOTP( ZLOADV_HOR(1,2), STAR ) / VLIGHT
C
      CONTRIB_HOR(1,1) =  DOTP( ZLOADP_HOR(1,1), STAR ) / VLIGHT
      CONTRIB_HOR(2,1) = -DOTP( ZLOADP_HOR(1,2), STAR ) / VLIGHT
      CONTRIB_HOR(1,2) =  DOTP( ZLOADV_HOR(1,1), STAR ) / VLIGHT
      CONTRIB_HOR(2,2) = -DOTP( ZLOADV_HOR(1,2), STAR ) / VLIGHT
C
c     CONTRIB_VER(1,1) =  DOTP( ZLOADP_VER(1,1), STAR ) / VLIGHT
c     CONTRIB_VER(2,1) =  DOTP( ZLOADV_VER(1,1), STAR ) / VLIGHT
c     CONTRIB_VER(1,2) = -DOTP( ZLOADP_VER(1,2), STAR ) / VLIGHT
c     CONTRIB_VER(2,2) = -DOTP( ZLOADV_VER(1,2), STAR ) / VLIGHT
C
      CONTRIB_VER(1,1) =  DOTP( ZLOADP_VER(1,1), STAR ) / VLIGHT
      CONTRIB_VER(2,1) = -DOTP( ZLOADP_VER(1,2), STAR ) / VLIGHT
      CONTRIB_VER(1,2) =  DOTP( ZLOADV_VER(1,1), STAR ) / VLIGHT
      CONTRIB_VER(2,2) = -DOTP( ZLOADV_VER(1,2), STAR ) / VLIGHT
C
C   'PUT' the ocean loading contributions into the database.
      CALL PUT4 ('OCE CONT      ', DOCEC      , 2, 1, 1 )
      CALL PUT4 ('OCE HORZ      ', CONTRIB_HOR, 2, 2, 1 )
      CALL PUT4 ('OCE VERT      ', CONTRIB_VER, 2, 2, 1 )
C
C   Check KOCED so see if debug is requested.
      IF ( KOCED .EQ. 0 ) GO TO 500
C
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, 'Debug output for subroutine OCEC.' )
      WRITE(6,8)'BASCOR ',BASCOR
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)'VLIGHT ',VLIGHT
      WRITE ( 6, 9200 )  ZLOADP, ZLOADV, STAR, CONTRIB_HOR,
     .                   CONTRIB_VER, DOCEC
 9200 FORMAT (    ' ZLOADP  =    ', 6D16.8, /,
     1            ' ZLOADV  =    ', 6D16.8, /,
     2            ' STAR    =    ', 3D16.8, /,
     3            ' CONTRIB_HOR  ', 4D16.8, /,
     3            ' CONTRIB_VER  ', 4D16.8, /,
     3            ' DOCEC  =     ', 2D16.8 )
C
C     Normal termination.
  500 RETURN
      END
C
C******************************************************************************
      SUBROUTINE OCARG (UT1, XJD, ANGLE)
      IMPLICIT None 
C
C   1.     OCARG
C
C   1.1    OCARG PROGRAM SPECIFICATIONS
C
C   1.1.1  THIS SUBROUTINE COMPUTES THE ANGULAR ARGUMENTS
C          FOR SCHWIDERSKI COMPUTATION OF 11 OCEAN TIDES.
C
C          C A U T I O N
C          = = = = = = =
C          SCHWIDERSKI MODIFIES THE ANGULAR ARGUMENTS OF THE DIURNAL TERMS
C          BY +/- 90 DEGREES. THEREFORE HIS DIURNAL PHASES CANNOT BE USED WITH
C          THE STANDARD DOODSEN OR CARTWRIGHT CONVENTIONS.
C
C          Valid only after 1973!!!!!!
C
C   1.1.2  RESTRICTIONS  -  NONE
C
C   1.1.3  REFERENCES - MERIT STANDARDS, APRIL 1981, SECOND DRAFT, APPENDICES
C                       7 and 11.
C
C   1.2.   OCARG PROGRAM INTERFACE
C
C   1.2.1  CALLING SEQUENCE
C
C          INPUT VARIABLES  -
C            1. UT1  -  THE UT1 TIME OF THE DAY. (SEC)
C            2. XJD  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
C                       QUESTION. (DAYS)
C
C          OUTPUT VARIABLES  -
C            1. ANGLE(11) - THE ANGULAR ARGUMENTS FOR SCHWIDERSKI COMPUTATION
C                           OF THE OCEAN TIDES IN THE ORDER:
C                           M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA
C
C   1.2.2. COMMON BLOCKS USED
C
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C          VARIABLES 'FROM'  -
C            1. TWOPI  -  PI * 2.0D0. (UNITLESS)
C            2. CONVD  -  CONVERSION FACTOR FROM DEGREES TO RADIANS. (RAD/DEG)
C            3. SECDAY -  THE NUMBER OF TIME SECONDS PER DAY. (SEC/DAY)
C
      INCLUDE 'ccon.i'
C          VARIABLES 'FROM'  -
C            1. KOCED  -  THE OCEAN LOADING MODULE DEBUG CONTROL FLAG.
C
       Real*8 speed(11)
       Common /tide_speed/ speed
C            SPEED(11)- COEFFICIENTS FOR THE COMPUTATION OF ARGUMENTS IN THE
C                       FOLLOWING ORDER OF TIDES: (RAD/SEC)
C                       M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA.
C
C  1.2.3   PROGRAM SPECIFICATIONS
      REAL*8 ANGLE(11), UT1, XJD, CENTJ, XJD75, FDAY,
     .       CAPT, H0, S0, P0
      REAL*4 ANGFAC(4,11)
      Integer*4 ICAPD, JCAPD, K
      Save CAPT, H0, S0, P0, JCAPD
C
C  1.2.5   EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG
C
C  1.2.6   SUBROUTINE INTERFACE
C              CALLER SUBROUTINE - OCEG
C              CALLED SUBROUTINE - DMOD
C
C  1.2.7   CONSTANTS USED  -
C            1. CONVD  -  CONVERSION FACTOR FROM DEGREES TO RADIANS. (RAD/DEG)
C            2. TWOPI  -  PI * 2.0D0. (UNITLESS)
C            3. ANGFAC(4,11)-TABLE OF MULTIPLES OF ARGUMENTS. (UNITLESS) 
C            4. CENTJ  -  THE NUMBER OF JULIAN DAYS PER JULIAN CENTURY.
C                         (DAYS/CENT.) (CENTJ=36525.D0)
C            5. SPEED(11)- COEFFICIENTS FOR THE COMPUTATION OF ARGUMENTS IN THE
C                          FOLLOWING ORDER OF TIDES: (RAD/SEC)
C                           M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA.
C            6. XJD75  -  THE JULIAN DATE OF JAN. 0.0 1975. (DAYS)
C
      DATA ANGFAC / 2.D0, -2.D0, 0.D0, 0.D0,
     2              0.D0,  0.D0, 0.D0, 0.D0,
     3              2.D0, -3.D0, 1.D0, 0.D0,
     4              2.D0,  0.D0, 0.D0, 0.D0,
     5              1.D0,  0.D0, 0.D0, 0.25D0,
     6              1.D0, -2.D0, 0.D0,-0.25D0,
     7             -1.D0,  0.D0, 0.D0,-0.25D0,
     8              1.D0, -3.D0, 1.D0,-0.25D0,
     9              0.D0,  2.D0, 0.D0, 0.D0,
     A              0.D0,  1.D0,-1.D0, 0.D0,
     1              2.D0,  0.D0, 0.D0, 0.D0 /
C
      DATA CENTJ / 36525.D0 /
C
      DATA SPEED / 1.40519D-4, 1.45444D-4, 1.37880D-4,
     1             1.45842D-4, 0.72921D-4, 0.67598D-4,
     2             0.72523D-4, 0.64959D-4, 0.053234D-4,
     3             0.026392D-4, 0.003982D-4 /
C 
      DATA XJD75 / 2442412.5D0 /
      data JCAPD / -9999 /   ! initial value, make sure its before 1973 
C 
C  1.2.8   PROGRAM VARIABLES
C            1. CAPT  -  THE NUMBER OF JULIAN CENTURIES BETWEEN JAN 0.5, 1900
C                        AND THE OBSERVATION.
C            2. FDAY  -  FRACTIONAL PART OF UNIVERSAL TIME (UT1) DAY IN
C                        SECONDS. (SEC)
C            3. H0    -  MEAN LONGITUDE OF SUN AT BEGINNING OF DAY. (RAD)
C            4. ICAPD -  JULIAN DAYS SINCE JANUARY 0.0 UT 1975. (DAYS)
C            5. P0    -  MEAN LONGITUDE OF LUNAR PERIGEE AT BEGINNING OF DAY.
C                        (RAD)
C            6. S0    -  MEAN LONGITUDE OF MOON AT BEGINNING OF DAY. (RAD)
C            7. JCAPD -  Previous value of ICAPD. Checked to see if we can
C                        reuse the previous values of CAPT, H0, P0, and S0.
C
C  1.2.9   PROGRAMMER  -  CLYDE GOAD
C          83:10:08 HAROLD M. SCHUH
C          89:10:08 Jim Ryan ANGFAC and SPEED moved to ema.
C          91:11:04 Jim Ryan ANGFAC(8,3) changed from 0.0 to 1.0 to make
C               consistent with IERS standards (Nov. 89)
C          David Gordon 94.04.18 Converted to Implicit None.
C          David Gordon 94.08.22 ICAPD had been incorrectly set to R*8, changed
C                       it to an integer - error had no effect. Put SPEED(11)
C                       into common block for use in geometry section [to 
C                       replace DANGL(11)].
C          David Gordon 94.09.22 Added JCAPD and setting it to the previous 
C                       value of ICAPD. If equal to current ICAPD, we skip
C                       computation of CAPT, H0, S0, and P0. CAPT, H0, S0, P0, 
C                       and JCAPD put in save block.
C
C  1.3     OCARG PROGRAM STRUCTURE
C
C   Compute FDAY  -  fractional part of UT1 day in seconds.
      FDAY = UT1
C
C   Compute ICAPD  -  Days since JAN 0, 0.00 UT, 1975
      ICAPD = XJD - XJD75
C
C  Check to see if we've started a new day. If so, we need to compute new values
C    for CAPT, H0, S0, & P0. If not, then we can reuse the previous values. 
      If (ICAPD .ne. JCAPD) Then  
C
       JCAPD = ICAPD      !Reset JCAPD for next observation
C
C   Compute CAPT  -  Julian centuries since JAN 0.5, 1900.
      CAPT = ( 27392.500528D0 + 1.000000035D0 * ICAPD ) / CENTJ
C
C   Compute mean longitude of sun at beginning of day.
      H0 = (279.69668D0 + (36000.768930485D0 + 3.03D-4 * CAPT) * CAPT)
     1     * CONVD
C
C   Compute mean longitude of moon at beginning of day.
      S0 = ((( 1.9D-6 * CAPT - 0.001133D0 ) * CAPT + 481267.88314137D0)
     1     * CAPT + 270.434358D0 ) * CONVD
C
C   Compute mean longitude of lunar perigee at beginning of day.
      P0 = ((( -1.2D-5 * CAPT - .010325D0 ) * CAPT + 4069.0340329577D0)
     1     * CAPT + 334.329653D0 ) * CONVD
C
      Endif
C
C   Calculate the angular arguments. Run a loop over the 11 main tides.
C
      DO K = 1,11
        ANGLE(K) = SPEED(K)*FDAY + ANGFAC(1,K)*H0 + ANGFAC(2,K)*S0
     1             + ANGFAC(3,K)*P0 + ANGFAC(4,K)*TWOPI
C
        ANGLE(K) = DMOD( ANGLE(K), TWOPI )
        IF( ANGLE(K) .LT. 0.D0 ) ANGLE(K) = ANGLE(K) + TWOPI
      Enddo
C
C   Check KOCED to determine if debug output is requested.
      IF( KOCED .EQ. 0 ) GO TO 800
      H0 = DMOD( H0, TWOPI )
      S0 = DMOD( S0, TWOPI )
      P0 = DMOD( P0, TWOPI )
      WRITE (6,9100)
 9100 FORMAT (1X, 'Debug output for subroutine OCARG.' )
      WRITE(6,8)'ANGLE  ' ,ANGLE
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,9)'ANGFAC ',ANGFAC
    9 FORMAT(/,1X,A,4I4/(8X,4I4))
      WRITE(6,8)'CAPT   ',CAPT
      WRITE(6,8)'FDAY   ',FDAY
      WRITE(6,8)'H0     ',H0
      WRITE(6,7) 'ICAPD ',ICAPD
    7 FORMAT(/,1X,A,15I8/(8X,15I8))
      WRITE(6,8)'P0     ',P0
      WRITE(6,8)'S0     ',S0
      WRITE(6,8)'CENTJ  ',CENTJ
      WRITE(6,8)'CONVD  ',CONVD
      WRITE(6,8)'TWOPI  ',TWOPI
      WRITE(6,8)'XJD75  ',XJD75
      WRITE(6,8)'UT1    ',UT1
      WRITE(6,8)'XJD    ',UT1
C
C     Normal termination.
  800 RETURN
      END
