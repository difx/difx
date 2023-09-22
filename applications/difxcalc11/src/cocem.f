      SUBROUTINE OCEG (CFSITE, UT1, OCEAMP, OCEPHS, R2000, XJD, TCTOCF, &
     &                 TSKIP, FA2K, CENT, UTC, XLOADP, XLOADV)
      IMPLICIT None
!
!     OCEG is the ocean loading geometry section.
!
!     References -
!       1. Merit Standards, April 1981, Second draft, Appendices 7 and 11.
!       2. C.C.GOAD, J. GEOPHYS. RES., 85, P.2679-2683, MAY 1980.
!       3. IERS Conventions (2010).
!
!     OCEG program interfaces -
!
!       CALLING SEQUENCE -
!
!       INPUT VARIABLES:
!         1. CFSITE(3,2)  - THE CRUST FIXED SITE VECTORS AT EACH OBSERVATION
!                           SITE. (M)
!         2. UT1          - THE UT1 TIME OF THE DAY. (SEC)
!         3. OCEAMP(11,3,2) - OCEAN LOADING AMPLITUDES FOR 11 TIDES AT EACH
!                             OBSERVATION SITE. (M)
!                  ( J,K,L)      K=1 : VERTICAL
!                                K=2 : EAST-WEST
!                                K=3 : NORTH-SOUTH DIRECTION
!         4. OCEPHS(11,3,2) - OCEAN LOADING PHASES. (RAD)
!         5. R2000(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION MATRIX
!                           AND ITS FIRST TWO CT TIME DERIVATIVES.
!                           (UNITLESS,1/SEC,1,SEC**2)
!         6. XJD          - THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
!                           QUESTION. (DAYS)
!         7. TCTOCF(3,3,2)- THE ROTATION MATRIX WHICH ROTATES THE TOPOCENTRIC
!                           REFERENCE SYSTEM TO THE CRUST FIXED REFERENCE SYSTEM
!                           AT EACH OBSERVATION SITE. (UNITLESS)
!
!       OUTPUT VARIABLES:
!         1. XLOADP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                           POSITION VECTORS DUE TO OCEAN LOADING EFFECTS. (M)
!                           FOR THE DEFAULT CALC SETUP (KOCEC=0), THE OCEAN
!                           LOADING CORRECTIONS ARE NOT ADDED TO THE
!                           THEORETICAL OBSERVATIONS, BUT ONLY PASSED OUT AS
!                           CONTRIBUTIONS, SO THIS ARRAY IS NORMALLY ZEROED OUT.
!         2. XLOADV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                           VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS. (M/S)
!                           SEE ABOVE COMMENTS FOR XLOADP.
!
! 4.2.2 COMMON BLOCKS USED -
!
      Real*8 ZLOADP(3,2), ZLOADV(3,2), ZLOADP_HOR(3,2),                 &
     &       ZLOADV_HOR(3,2), ZLOADP_VER(3,2), ZLOADV_VER(3,2),         &
     &       ZLOADPold(3,2), ZLOADVold(3,2)
      COMMON / OCECM / ZLOADP, ZLOADV, ZLOADP_HOR,                      &
     &       ZLOADV_HOR, ZLOADP_VER, ZLOADV_VER,                        &
     &       ZLOADPold, ZLOADVold
!       VARIABLES TO:
!         1. ZLOADP(3,2)  - SAME AS XLOADP ABOVE, EXCEPT CONTAINS VALUES UNLESS
!                           THE OCEAN LOADING MODULE IS TURNED OFF (KOCEC=1).
!                           STORED AS A SEPARATE VARIABLE IN COMMON IN ORDER TO
!                           PASS IT TO THE CONTRIBUTIONS SUBROUTINE.
!         2. ZLOADV(3,2)  - SAME AS XLOADV. SEE COMMENTS FOR ZLOADP.
!         3. ZLOADP_HOR(3,2) - SAME AS ZLOADP EXCEPT CONTAINS ONLY THE
!                           HORIZONTAL OCEAN LOADING EFFECTS.
!         4. ZLOADV_HOR(3,2)  - SAME AS ZLOADV EXCEPT CONTAINS ONLY THE
!                           HORIZONTAL OCEAN LOADING EFFECTS.
!         5. ZLOADP_VER(3,2)  - SAME AS ZLOADP EXCEPT CONTAINS ONLY THE VERTICAL
!                           OCEAN LOADING EFFECTS.
!         6. ZLOADV_VER(3,2)  - SAME AS ZLOADV EXCEPT CONTAINS ONLY THE VERTICAL
!                           OCEAN LOADING EFFECTS.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!       VARIABLES 'FROM' -
!         1. CONVD   -  THE CONVERSION FACTOR FROM DEGREES TO radians
!
       INCLUDE 'ccon.i'
!       VARIABLES 'FROM':
!         1. KOCEC   -  THE OCEAN LOADING MODULE FLOW CONTROL FLAG
!         2. KOCED   -  THE OCEAN LOADING MODULE DEBUG OUTPUT FLAG
!
      INCLUDE 'cobsn11.i'
!          Variables from:
!            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
!                         otherwise equals zero. For correlator usage.
!
       INCLUDE 'cuser11.i'
!       Variables from:
!            1. Calc_user   - Calc user type. 'A' for Calc/SOLVE analysis.
!                             'C' for VLBI correlator.
!            2. Apply_ocean - Switch to apply ocean loading to theoreticals
!                             for correlator usage. 'Y' to apply (recommended),
!                             'N' for do not apply.
!
     INCLUDE 'put2s.i'
!       Variables to:
!         1. XHOLD(3,2,2) - THE TOTAL OCEAN LOADING DISPLACEMENT VECTORS IN
!                           J2000.0 COORDINATES. THE INDICES RUN OVER (X,Y,Z),
!                          (POSITION, VELOCITY), (SITE 1, SITE 2) (METERS)
!
       Real*8 speed(11)
       Common /tide_speed/ speed
!            SPEED(11)- COEFFICIENTS FOR THE COMPUTATION OF ARGUMENTS IN THE
!                       FOLLOWING ORDER OF TIDES: (RAD/SEC)
!                       M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA.
!                       (These are actually the rates of change of the 11 tide
!                        angles - see subroutine OCARG).
!
! 4.2.3 PROGRAM SPECIFICATIONS -
!
      Integer*4 I, J, K, L
      Real*8 ANGLE(11), CFDIS(3,2), CFSITE(3,2), DELTAO(3,2),           &
     &       OCEAMP(11,3,2), OCEPHS(11,3,2), R2000(3,3,3),              &
     &       XLOADP(3,2), XLOADV(3,2), TCDIS(3,2), CFVEL(3,2),          &
     &       ZLOAV1(3,2), ZLOAV2(3,2), TCVEL(3,2), TCTOCF(3,3,2),       &
     &       DELTAV(3,2), TCDIS_HOR(3,2), TCDIS_VER(3,2),               &
     &       TCVEL_HOR(3,2), TCVEL_VER(3,2), CFDIS_HOR(3,2),            &
     &       CFDIS_VER(3,2), CFVEL_HOR(3,2), CFVEL_VER(3,2),            &
     &       ZLOAV1_HOR(3,2),ZLOAV1_VER(3,2), ZLOAV2_HOR(3,2),          &
     &       ZLOAV2_VER(3,2), UT1, XJD
      Real*8 TCDISold(3,2),TCVELold(3,2),CFDISold(3,2),CFVELold(3,2),   &
     &       ZLOAV3(3,2), ZLOAV4(3,2)
!
      REAL*8 CTUT1, FA2K(14), CENT, UTC, D_UEN(3,2),DV_UEN(3,2)
      Integer*4 NL
      PARAMETER (NL=600)
      REAL*4 DZ(NL),DS(NL),DW(NL)
!
      Integer*4 TSKIP
!
      Save ANGLE
!
! 4.2.4 DATA BASE ACCESS
!
!       PUT VARIABLES:
!         1. XHOLD(3,2,2) - THE TOTAL OCEAN LOADING DISPLACEMENT VECTORS IN
!                           J2000.0 COORDINATES. THE INDICES RUN OVER (X,Y,Z),
!                          (POSITION, VELOCITY), (SITE 1, SITE 2) (METERS)
!
! 4.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
!
! 4.2.6 SUBROUTINE INTERFACE  -
!       CALLER SUBROUTINES: DRIVG
!       CALLED SUBROUTINES: OCARG, ANG, DATAN2, DCOS, DSIN, VECRT, VECAD
!
! 4.2.7 CONSTANTS USED - NONE
!
! 4.2.8 PROGRAM VARIABLES -
!
!         1. ANGLE(11)  - THE ANGULAR ARGUMENTS FOR SCHWIDERSKI COMPUTATION IN
!                         THE FOLLOWING ORDER:
!                         M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA
!         2. CFDIS(3,2) - THE CRUST FIXED GEOCENTRIC DISPLACEMENTS DUE TO OCEAN
!                         LOADING AT EACH SITE. (M)
!         3. CFVEL(3,2) - THE CRUST FIXED GEOCENTRIC VELOCITIES DUE TO OCEAN
!                         LOADING AT EACH SITE. (M/SEC)
!         4. DELTAO(3,2)- THE UP (I=1), EAST-WEST (I=2), AND NORTH-SOUTH (I=3)
!                  (I,J)  DISPLACEMENTS DUE TO OCEAN LOADING AT EACH SITE. (M)
!         5. ZLOAV1(3,2)- THE CONTRIBUTION TO THE CORRECTION TO THE J2000.0
!                         GEOCENTRIC SITE VELOCITY VECTOR DUE TO THE EFFECT OF
!                         THE CHANGED SITE POSITION. (M/SEC)
!         6. ZLOAV2(3,2)- THE CONTRIBUTION TO THE CORRECTION TO THE J2000.0
!                         GEOCENTRIC SITE VELOCITY VECTOR DUE TO THE EFFECT OF
!                         THE CHANGING OCEAN LOADING DISPLACEMENT. (M/SEC)
!         7. TCDIS(3,2) - THE TOPOCENTRIC EARTH CRUSTAL DISPLACEMENTS DUE TO
!                         OCEAN LOADING AT EACH SITE. (M)
!         8. TCVEL(3,2) - THE TOPOCENTRIC EARTH CRUSTAL VELOCITIES DUE TO OCEAN
!                         LOADING AT EACH SITE. (M/SEC)
!         9. DELTAV(3,2)- THE VELOCITY OF EACH SITE DUE TO OCEAN LOADING. (M/S)
!        10. ZLOAV1_HOR(3,2)- LIKE ZOAV1, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
!        11. ZLOAV2_HOR(3,2)- LIKE ZOAV2, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
!        12. ZLOAV1_VER(3,2)- LIKE ZOAV1, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
!        13. ZLOAV2_VER(3,2)- LIKE ZOAV2, BUT CONTAINS ONLY HORIZONTAL EFFECTS.
!        14. D_UEN(3,2) - THE UP (I=1), EAST-WEST (I=2), AND NORTH-SOUTH (I=3)
!                 (I,J)   DISPLACEMENTS DUE TO OCEAN LOADING AT EACH SITE, as 
!                         computed using the HARDISP model. (meters)
!
! 4.2.9 PROGRAMMER - HAROLD M. SCHUH 10/08/83
!                    JIM RYAN        06/20/84  CHANGED TO CONTRIBUTION ONLY
!                    DAVID GORDON    08/30/84  PUT CONVERSION TO J2000.0
!                                    POSITIONS AND VELOCITIES INTO STANDARD FORM
!                    DAVID GORDON    09/05/84  ADDED DANGL AND COMPLETED
!                                    COMPUTATION OF RATES
!                    DAVID GORDON    03/18/85  FIXED BUG FOR KOCEC.NE.0
!                    GREGG COOKE     12/22/88 TOPO OCL DISP. & VEL. PUT INTO
!                                    DATABASE)
!                    LOTHAR MOHLMANN 03/23/89 HORIZONTAL OCEAN LOADING
!                                    DISPLACEMENTS)
!                    GREG COOK       06/20/89  Put into CALC 7.0
!                    JIM RYAN        06/23/89  Code added to break out
!                                    horizontal and vertical.
!                    Jim Ryan        89.12.12 UNIX-like database interface
!                                    implimented.
!                    Jim Ryan        90.01.24 Logic for KOCEC=3 added.
!                    DSR&JWR         90.04.24 Changed the signs on the hor.
!                                    displacements and velocities. Code
!                                    orginally supplied by Bonn group was in
!                                    error due to error in algorithm supplied by
!                                    Sherneck at Uppsala. (Sherneck after
!                                    consulting with Duncan Agnew at Scripps
!                                    discovered the signs on both horizontal
!                                    components were in error.)
!                    Jim Ryan        90:11:20 Bug in logic for putting local
!                                    displacements in the database fixed.
!                                    Debug statments fixed.
!                    Jim Ryan        91.11.04 Array ANGFAC in 'OCARG' modified
!                                    to make it consistent with IERS stardards.
!                    David Gordon    94.04.18 Converted to Implicit None.
!                    David Gordon    94.08.22 Added common block for SPEED(11)
!                     & John Gipson  from OCARG to replace variable DANGL(11),
!                                    which was incorrectly being used as the
!                                    derivatives of the tide angles.
!                    David Gordon    98.08.04 Added code for handling Geocenter
!                                    station, zeroes out necessary quantities.
!                    David Gordon    98.10.16 Added 'cuser.i' include file
!                                    and code to add ocean loading to the
!                                    theoreticals for correlator users.
!                    Jim Ryan Sept 2002 Integer*2/4 mods.
!                    D. Gordon Nov. 2012 Hardisp model added.
!                    D. Gordon Dec. 2012 PUT's move to PUT_G. 
!                    D. Gordon Jan. 2013 Modified to pass ocean loading into
!                                   subroutines SITCR or dSITCR and decide 
!                                   there whether to apply or not.
!                    D. Gordon Oct. 2013 Modified to save the old ocean 
!                                   loading displacements and velocities. 
!                                   (Calc 10 and earlier versions.)
!
!     OCEG PROGRAM STRUCTURE
!
!   The ocean loading geometry for sites #1 and #2 are calculated separately by
!   running a loop over the two sites.
!
!   Compute the time dependent angular momentum for the 11 tidal arguments.
      IF (TSKIP.ne.1) &
     &    CALL OCARG ( UT1, XJD, ANGLE )
!
!   Loop twice for the calculation of the displacements due to ocean
!   loading at sites #1 and 2.
!
      DO  L = 1, 2
!
! Check for Geocenter site
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
!
!   Compute the topocentric displacement DELTAO and DELTAV for each site due to
!    ocean loading (for the 11 main tides in 3 directions).
!     [These are the old (Calc 10 and earlier) displacements.]
!
        DO K = 1, 3
          DELTAO(K,L) = 0.D0
          DELTAV(K,L) = 0.D0
          DO J = 1,11
            DELTAO(K,L) =   OCEAMP(J,K,L)  * DCOS(ANGLE(J)              &
     &                    - OCEPHS(J,K,L)) + DELTAO(K,L)
            DELTAV(K,L) = - OCEAMP(J,K,L)  * DSIN(ANGLE(J)              &
     &                    - OCEPHS(J,K,L)) * speed(J) &
     &                    + DELTAV(K,L)
          ENDDO
        ENDDO
!
!   Change the signs on the horizontal displacements and velocities.
!     (See explanation in the subroutine history above.)
!
        DELTAO(2,L) = -DELTAO(2,L)
        DELTAV(2,L) = -DELTAV(2,L)
        DELTAO(3,L) = -DELTAO(3,L)
        DELTAV(3,L) = -DELTAV(3,L)
!
!**********************************************************************
!  Now call Hardisp ocean loading model:
       CALL C_HARDISP(CENT,UT1,UTC,OCEAMP,OCEPHS, FA2K, L, DZ, DW, DS)
       D_UEN(1,L) =  DZ(1)
       D_UEN(2,L) = -DW(1)
       D_UEN(3,L) = -DS(1)
! Velocities:
       DV_UEN(1,L) = 0.0D0 
       DV_UEN(2,L) = 0.0D0 
       DV_UEN(3,L) = 0.0D0 
!
!      write(6,1017) L, DELTAO(1,L), DELTAO(2,L), DELTAO(3,L)
!      write(6,1018) L, D_UEN (1,L), D_UEN (2,L), D_UEN (3,L)
 1017  format (/,' OCEG: L, DELTAO(m) ',I2,3F14.6)
 1018  format (/,' OCEG: L, D_UEN(m)  ',I2,3F14.6)
!
!**********************************************************************
!
!   Load the topocentric ocean loading displacement vector. Also load up vector
!   that contains only the vertical and only the horizontal effects.
!!!     TCDIS(1,L) = DELTAO(1,L)
!!!     TCDIS(2,L) = DELTAO(2,L)
!!!     TCDIS(3,L) = DELTAO(3,L)
!
!!!     TCDIS_HOR(1,L) = 0.D0
!!!     TCDIS_HOR(2,L) = DELTAO(2,L)
!!!     TCDIS_HOR(3,L) = DELTAO(3,L)
!
!!!     TCDIS_VER(1,L) = DELTAO(1,L)
!!!     TCDIS_VER(2,L) = 0.D0
!!!     TCDIS_VER(3,L) = 0.D0
!
!   Load the topocentric ocean loading velocity vector. As above, load like
!   quantities for horizontal and vertical.
!!!     TCVEL(1,L) = DELTAV(1,L)
!!!     TCVEL(2,L) = DELTAV(2,L)
!!!     TCVEL(3,L) = DELTAV(3,L)
!
!!!     TCVEL_HOR(1,L) = 0.D0
!!!     TCVEL_HOR(2,L) = DELTAV(2,L)
!!!     TCVEL_HOR(3,L) = DELTAV(3,L)
!
!!!     TCVEL_VER(1,L) = DELTAV(1,L)
!!!     TCVEL_VER(2,L) = 0.D0
!!!     TCVEL_VER(3,L) = 0.D0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Replace with the HARDISP model
!
!   Load the topocentric ocean loading displacement vector. Also load vectors
!   that contains only the vertical and only the horizontal effects.
        TCDIS(1,L) =  D_UEN(1,L)
        TCDIS(2,L) =  D_UEN(2,L)
        TCDIS(3,L) =  D_UEN(3,L)
!
        TCDIS_HOR(1,L) = 0.D0
        TCDIS_HOR(2,L) =  D_UEN(2,L)
        TCDIS_HOR(3,L) =  D_UEN(3,L)
!
        TCDIS_VER(1,L) =  D_UEN(1,L)
        TCDIS_VER(2,L) = 0.D0
        TCDIS_VER(3,L) = 0.D0
!
!   Load the topocentric ocean loading velocity vector. As above, load like
!   quantities for horizontal and vertical.
        TCVEL(1,L) = DV_UEN(1,L)
        TCVEL(2,L) = DV_UEN(2,L)
        TCVEL(3,L) = DV_UEN(3,L)
!
        TCVEL_HOR(1,L) = 0.D0
        TCVEL_HOR(2,L) = DV_UEN(2,L)
        TCVEL_HOR(3,L) = DV_UEN(3,L)
!
        TCVEL_VER(1,L) = DV_UEN(1,L)
        TCVEL_VER(2,L) = 0.D0
        TCVEL_VER(3,L) = 0.D0
!
!   Rotate the displacements and velocities to the crust fixed geocentric
!   coordinate system.
        CALL VECRT(TCTOCF(1,1,L),TCDIS(1,L),CFDIS(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVEL(1,L),CFVEL(1,L))
!
        CALL VECRT(TCTOCF(1,1,L),TCDIS_HOR(1,L),CFDIS_HOR(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVEL_HOR(1,L),CFVEL_HOR(1,L))
!
        CALL VECRT(TCTOCF(1,1,L),TCDIS_VER(1,L),CFDIS_VER(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVEL_VER(1,L),CFVEL_VER(1,L))
!
!   Rotate the crust fixed geocentric displacements to J2000.0.
        CALL VECRT(R2000(1,1,1),CFDIS(1,L)    ,ZLOADP(1,L)    )
        CALL VECRT(R2000(1,1,1),CFDIS_HOR(1,L),ZLOADP_HOR(1,L))
        CALL VECRT(R2000(1,1,1),CFDIS_VER(1,L),ZLOADP_VER(1,L))
!
!   Rotate the crust fixed geocentric velocities to J2000.0.
!
!   Compute the contribution to the J2000.0 velocities due to the effect of
!   changed site positon due to ocean loading.
        CALL VECRT(R2000(1,1,2),CFDIS    (1,L),ZLOAV1    (1,L))
        CALL VECRT(R2000(1,1,2),CFDIS_HOR(1,L),ZLOAV1_HOR(1,L))
        CALL VECRT(R2000(1,1,2),CFDIS_VER(1,L),ZLOAV1_VER(1,L))
!
!   Compute the contribution to the J2000.0 velocities due to the effect of the
!   changing ocean loading displacement.
        CALL VECRT(R2000(1,1,1),CFVEL    (1,L),ZLOAV2    (1,L))
        CALL VECRT(R2000(1,1,1),CFVEL_HOR(1,L),ZLOAV2_HOR(1,L))
        CALL VECRT(R2000(1,1,1),CFVEL_VER(1,L),ZLOAV2_VER(1,L))
!
!   Add the contributions
        CALL VECAD(ZLOAV1    (1,L),ZLOAV2    (1,L),ZLOADV    (1,L))
        CALL VECAD(ZLOAV1_HOR(1,L),ZLOAV2_HOR(1,L),ZLOADV_HOR(1,L))
        CALL VECAD(ZLOAV1_VER(1,L),ZLOAV2_VER(1,L),ZLOADV_VER(1,L))
!
!**********************************************************************
!   Save the 'old' topocentric ocean loading displacement and velocity vectors.
        TCDISold(1,L) = DELTAO(1,L)
        TCDISold(2,L) = DELTAO(2,L)
        TCDISold(3,L) = DELTAO(3,L)
        TCVELold(1,L) = DELTAV(1,L)
        TCVELold(2,L) = DELTAV(2,L)
        TCVELold(3,L) = DELTAV(3,L)
!
!   Rotate to the crust fixed geocentric coordinate system.
        CALL VECRT(TCTOCF(1,1,L),TCDISold(1,L),CFDISold(1,L))
        CALL VECRT(TCTOCF(1,1,L),TCVELold(1,L),CFVELold(1,L))
!
!   Rotate the crust fixed geocentric displacements to J2000.0.
        CALL VECRT(R2000(1,1,1),CFDISold(1,L),ZLOADPold(1,L)    )
!
!   Rotate the crust fixed geocentric velocities to J2000.0.
!
!   Compute the contribution to the J2000.0 velocities due to the effect of
!   changed site positon due to ocean loading.
        CALL VECRT(R2000(1,1,2),CFDISold(1,L),ZLOAV3(1,L))
!   Compute the contribution to the J2000.0 velocities due to the effect of the
!   changing ocean loading displacement.
        CALL VECRT(R2000(1,1,1),CFVELold(1,L),ZLOAV4(1,L))
!   Add the contributions
        CALL VECAD(ZLOAV3(1,L),ZLOAV4(1,L),ZLOADVold    (1,L))
!**********************************************************************
!
 150   CONTINUE
!
      ENDDO
!
!   Check KOCEC to determine if the ocean loading module is to be turned off.
!
!   Handle the normal case (Contributions only)
!**   IF (KOCEC .EQ. 0 .or. KOCEC.eq.3) THEN
        DO  L = 1,2
          DO  I = 1,3
!**         XLOADP(I,L) = 0.D0
!**         XLOADV(I,L) = 0.D0
!  Save Ocean loading. Decide in subroutine SITCR/dSITCR whether
!   to apply it or not. 2013-Jan-29.
            XLOADP(I,L) = ZLOADP(I,L)
            XLOADV(I,L) = ZLOADV(I,L)
          ENDDO
        ENDDO
!**   ENDIF
!
!   Handle the ocean loading module off case.
      IF (KOCEC .EQ. 1) THEN
        DO  L = 1,2
          DO  I = 1,3
            ZLOADP(I,L) = 0.D0
            ZLOADP_HOR(I,L) = 0.D0
            ZLOADP_VER(I,L) = 0.D0
            XLOADP(I,L) = 0.D0
!
!
            ZLOADV(I,L) = 0.D0
            ZLOADV_HOR(I,L) = 0.D0
            ZLOADV_VER(I,L) = 0.D0
            XLOADV(I,L) = 0.D0
          ENDDO
        ENDDO
      ENDIF
!
!   Handle the special case (KOCEC=2) where the effect of ocean loading is
!   added to the theoretical.
!**   IF (KOCEC.eq.2 .or. (Calc_user.eq.'C' .and. Apply_ocean.eq.'Y')   &
!**  &    .or. (Calc_user.eq.'D' .and. Apply_ocean.eq.'Y') ) &
!**  &         THEN
!       print *, 'Ocean loading being applied'
!**     DO  L = 1,2
!**       DO  I = 1,3
!**         XLOADP (I,L) = ZLOADP(I,L)
!**         XLOADV (I,L) = ZLOADV(I,L)
!**       ENDDO
!**     ENDDO
!**   ENDIF
!
!  Put the site dependent topocentric ocean loading displacement and velocity in
!  the database only if the module is turned on. => PUT moved to PUT_G.
!   Note the use of the indices: (Up, East, North) by (P,V) by (site1,site2).
      IF (KOCEC .NE. 1) THEN
        DO I = 1,3
          DO L = 1,2
            XHOLD(I,1,L) = TCDIS(I,L)
            XHOLD(I,2,L) = TCVEL(I,L)
          ENDDO
        ENDDO
      ENDIF
!
!  Check KOCED to determine if debug output is requested.
  400 CONTINUE
!
      IF( KOCED .EQ. 0) GO TO 500
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
      WRITE(6,9200) ZLOADP, ZLOADP_HOR, ZLOADP_VER,                     &
     &              ZLOADV, ZLOADV_HOR, ZLOADV_VER,                     &
     &              XLOADP, XLOADV, CFSITE, UT1, OCEAMP,                &
     &              ((((OCEPHS(J,K,L)/CONVD),J=1,11),K=1,3),L=1,2),     &
     &              R2000,CFDIS, CFDIS_HOR,CFDIS_VER
!
 9200 FORMAT (' ZLOADP  = ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' ZLOADP_HOR',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' ZLOADP_VER',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' ZLOADV  = ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' ZLOADV_HOR',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' ZLOADV_VER',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' XLOADP  = ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' XLOADV  = ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' CFSITE  = ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' UT1     = ',/, D25.16, 10X , /, 1X ,                    &
     &        ' OCEAMP  = ',/,2( 3(11F10.5,/),/),                       &
     &        ' OCEPHS-deg',/,2( 3(11F10.5,/),/),                       &
     &        ' R2000   = ',/,3( 3(3D25.16,/),/),                       &
     &        ' CFDIS   = ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' CFDIS_HOR ',/,2 ( 3 ( D25.16, 10X ), /),                &
     &        ' CFDIS_VER ',/,2 ( 3 ( D25.16, 10X ), /))
      WRITE(6,'(" XJD     = ", F15.2, 1X,"KOCEC   = ",I3)')             &
     &  XJD,KOCEC
!
!     Normal termination.
 500  RETURN
      END
!*********************************************************************
      SUBROUTINE OCEP()
      IMPLICIT None
!
!     This is a "STUB" routine which does nothing but return control to DRIVR.
!     There are no ocean loading partials.
!
      RETURN
      END
!*********************************************************************
      SUBROUTINE OCEC (STAR)
      IMPLICIT None
!
!    OCEC is the contributions section of the ocean loading module. It computes
!    the contributions to the delay and delay rate due to ocean loading effects.
!
!     OCEC program interface -
!      CALLING SEQUENCE -
!       INPUT VARIABLES:
!         1. STAR(3) - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!       OUTPUT VARIABLES: NONE
!
! 6.2.2 COMMON BLOCKS -
!
      Real*8 ZLOADP(3,2), ZLOADV(3,2), ZLOADP_HOR(3,2),                 &
     &       ZLOADV_HOR(3,2), ZLOADP_VER(3,2), ZLOADV_VER(3,2),         &
     &       ZLOADPold(3,2), ZLOADVold(3,2)
      COMMON / OCECM / ZLOADP, ZLOADV, ZLOADP_HOR,                      &
     &       ZLOADV_HOR, ZLOADP_VER, ZLOADV_VER,                        &
     &       ZLOADPold, ZLOADVold
!
!       VARIABLES FROM:
!         1. ZLOADP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                           POSITION VECTORS DUE TO OCEAN LOADING EFFECTS. (M)
!         2. ZLOADV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                           VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS. (M/S)
!         3. ZLOADP_HOR(3,2)- SAME AS ZLOADP EXCEPT CONTAINS ONLY THE HORIZONTAL
!                           OCEAN LOADING EFFECTS.
!         4. ZLOADV_HOR(3,2)- SAME AS ZLOADV EXCEPT CONTAINS ONLY THE HORIZONTAL
!                           OCEAN LOADING EFFECTS.
!         5. ZLOADP_VER(3,2)- SAME AS ZLOADP EXCEPT CONTAINS ONLY THE VERTICAL
!                           OCEAN LOADING EFFECTS.
!         6. ZLOADV_VER(3,2)- SAME AS ZLOADV EXCEPT CONTAINS ONLY THE VERTICAL
!                           OCEAN LOADING EFFECTS.
!
      INCLUDE 'cphys11.i'
!       VARIABLES 'FROM':
!         1. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
!
      INCLUDE 'ccon.i'
!       VARIABLES 'FROM' :
!         1. KOCEC - THE OCEAN LOADING MODULE FLOW CONTROL FLAG
!         2. KOCED - THE OCEAN LOADING MODULE DEBUG OUTPUT FLAG
!
     INCLUDE 'put2s.i'
!       Variables to:
!         1. DOCEC(2)         - THE OCEAN LOADING CONTRIBUTION TO THE DELAY
!                               AND TO THE DELAY RATE. (SEC, SEC/SEC)
!         2. CONTRIB_HOR(2,2) - THE SITE-DEPENDENT CONTRIBUTION TO THE DELAY
!                               AND RATE FROM HORIZONTAL DISPLACEMENTS. THE
!                               first index runs over the sites and the second
!                               over the delay and rate. (sec, sec/sec)
!         3. CONTRIB_VER(2,2) - THE SITE-DEPENDENT CONTRIBUTION TO THE DELAY
!                               AND RATE FROM VERTICAL DISPLACEMENTS. THE
!                               first index runs over the sites and the second
!                               over the delay and rate. (sec, sec/sec)
!
!
! 6.2.3  PROGRAM SPECIFICATIONS -
!
      Integer*4 K
      Real*8 BASCOR(3,2), STAR(3), DOTP, BASCORold(3,2)
!
! 6.2.4  DATA BASE ACCESS => PUT's moved to PUT_C.
!
! 6.2.6 SUBROUTINE INTERFACE -
!         CALLER SUBROUTINES: DRIVC
!         CALLED SUBROUTINES: DOTP, PUT4, VECSB
!
! 6.2.7 CONSTANTS USED - VLIGHT
!
! 6.2.8 PROGRAM VARIABLES -
!         1. BASCOR(3,2)  -  THE CORRECTION TO THE J2000.0 BASELINE POSITION AND
!                            VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS.
!                            (M, M/SEC)
!
! 6.2.9 PROGRAMMER - HAROLD M. SCHUH 10/08/83
!                    JIM RYAN         6/20/83
!                    JIM RYAN        06/23/89  Code added to break out
!                                      horizontal and vertical.
!                    Jim Ryan 89:10:05 CPHYS common made an include file
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                                      implimented.
!                    David Gordon 94.04.18 Converted to Implicit None.
!                    David Gordon 94.06.27 Reversed indices of CONTRIB_HOR and
!                                      CONTRIB_VER for consistency elsewhere in
!                                      Calc (and SOLVE). First index is now
!                                      sites, second is delay and rate.
!                    Jim Ryan Sept 2002 Integer*2/4 mods.
!                    David Gordon Jan. 2013 Moved PUT's into subroutine PUT_C.
!                    David Gordon Oct. 2013 Code added to compute the old ocean 
!                                   loading contribution. 
!
!     OCEC PROGRAM STRUCTURE
!
!  Compute the contributions.
!
!   Compute the corrections to the J2000.0 baseline position and velocity
!   vectors due to ocean loading effects.
!
      CALL VECSB ( ZLOADP(1,1), ZLOADP(1,2), BASCOR(1,1) )
      CALL VECSB ( ZLOADV(1,1), ZLOADV(1,2), BASCOR(1,2) )
!
!   Complete the calculation of the contributions.
      DO K = 1,2
        DOCEC(K) = DOTP(BASCOR(1,K),STAR)/VLIGHT
      Enddo
!
      CONTRIB_HOR(1,1) =  DOTP( ZLOADP_HOR(1,1), STAR ) / VLIGHT
      CONTRIB_HOR(2,1) = -DOTP( ZLOADP_HOR(1,2), STAR ) / VLIGHT
      CONTRIB_HOR(1,2) =  DOTP( ZLOADV_HOR(1,1), STAR ) / VLIGHT
      CONTRIB_HOR(2,2) = -DOTP( ZLOADV_HOR(1,2), STAR ) / VLIGHT
!
      CONTRIB_VER(1,1) =  DOTP( ZLOADP_VER(1,1), STAR ) / VLIGHT
      CONTRIB_VER(2,1) = -DOTP( ZLOADP_VER(1,2), STAR ) / VLIGHT
      CONTRIB_VER(1,2) =  DOTP( ZLOADV_VER(1,1), STAR ) / VLIGHT
      CONTRIB_VER(2,2) = -DOTP( ZLOADV_VER(1,2), STAR ) / VLIGHT
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Compute contributions using the 'old' ocean loading computations
      CALL VECSB ( ZLOADPold(1,1), ZLOADPold(1,2), BASCORold(1,1) )
      CALL VECSB ( ZLOADVold(1,1), ZLOADVold(1,2), BASCORold(1,2) )
      DO K = 1,2
        DOCECold(K) = DOTP(BASCORold(1,K),STAR)/VLIGHT
      Enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   Check KOCED so see if debug is requested.
      IF ( KOCED .EQ. 0 ) GO TO 500
!
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, 'Debug output for subroutine OCEC.' )
      WRITE(6,8)'BASCOR ',BASCOR
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)'VLIGHT ',VLIGHT
      WRITE ( 6, 9200 )  ZLOADP, ZLOADV, STAR, CONTRIB_HOR,             &
     &                   CONTRIB_VER, DOCEC
 9200 FORMAT (    ' ZLOADP  =    ', 6D16.8, /,                          &
     &            ' ZLOADV  =    ', 6D16.8, /,                          &
     &            ' STAR    =    ', 3D16.8, /,                          &
     &            ' CONTRIB_HOR  ', 4D16.8, /,                          &
     &            ' CONTRIB_VER  ', 4D16.8, /,                          &
     &            ' DOCEC  =     ', 2D16.8 )
!
!     Normal termination.
  500 RETURN
      END
!*********************************************************************
      SUBROUTINE OCARG (UT1, XJD, ANGLE)
      IMPLICIT None
!
!   1.1.1  THIS SUBROUTINE COMPUTES THE ANGULAR ARGUMENTS
!          FOR SCHWIDERSKI COMPUTATION OF 11 OCEAN TIDES.
!
!          C A U T I O N
!          = = = = = = =
!          SCHWIDERSKI MODIFIES THE ANGULAR ARGUMENTS OF THE DIURNAL TERMS
!          BY +/- 90 DEGREES. THEREFORE HIS DIURNAL PHASES CANNOT BE USED WITH
!          THE STANDARD DOODSEN OR CARTWRIGHT CONVENTIONS.
!
!          Valid only after 1973!!!!!!
!
!   1.1.2  RESTRICTIONS  -  NONE
!
!   1.1.3  REFERENCES - MERIT STANDARDS, APRIL 1981, SECOND DRAFT, APPENDICES
!                       7 and 11.
!
!   1.2.   OCARG PROGRAM INTERFACE
!
!   1.2.1  CALLING SEQUENCE
!
!          INPUT VARIABLES  -
!            1. UT1  -  THE UT1 TIME OF THE DAY. (SEC)
!            2. XJD  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
!                       QUESTION. (DAYS)
!
!          OUTPUT VARIABLES  -
!            1. ANGLE(11) - THE ANGULAR ARGUMENTS FOR SCHWIDERSKI COMPUTATION
!                           OF THE OCEAN TIDES IN THE ORDER:
!                           M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA
!
!   1.2.2. COMMON BLOCKS USED
!
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!          VARIABLES 'FROM'  -
!            1. TWOPI  -  PI * 2.0D0. (UNITLESS)
!            2. CONVD  -  CONVERSION FACTOR FROM DEGREES TO RADIANS. (RAD/DEG)
!            3. SECDAY -  THE NUMBER OF TIME SECONDS PER DAY. (SEC/DAY)
!
      INCLUDE 'ccon.i'
!          VARIABLES 'FROM'  -
!            1. KOCED  -  THE OCEAN LOADING MODULE DEBUG CONTROL FLAG.
!
       Real*8 speed(11)
       Common /tide_speed/ speed
!            SPEED(11)- COEFFICIENTS FOR THE COMPUTATION OF ARGUMENTS IN THE
!                       FOLLOWING ORDER OF TIDES: (RAD/SEC)
!                       M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA.
!
!  1.2.3   PROGRAM SPECIFICATIONS
      REAL*8 ANGLE(11), UT1, XJD, CENTJ, XJD75, FDAY, &
     &       CAPT, H0, S0, P0
      REAL*4 ANGFAC(4,11)
      Integer*4 ICAPD, JCAPD, K
      Save CAPT, H0, S0, P0, JCAPD
!
!  1.2.5   EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG
!
!  1.2.6   SUBROUTINE INTERFACE
!              CALLER SUBROUTINE - OCEG
!              CALLED SUBROUTINE - DMOD
!
!  1.2.7   CONSTANTS USED  -
!            1. CONVD  -  CONVERSION FACTOR FROM DEGREES TO RADIANS. (RAD/DEG)
!            2. TWOPI  -  PI * 2.0D0. (UNITLESS)
!            3. ANGFAC(4,11)-TABLE OF MULTIPLES OF ARGUMENTS. (UNITLESS)
!            4. CENTJ  -  THE NUMBER OF JULIAN DAYS PER JULIAN CENTURY.
!                         (DAYS/CENT.) (CENTJ=36525.D0)
!            5. SPEED(11)- COEFFICIENTS FOR THE COMPUTATION OF ARGUMENTS IN THE
!                          FOLLOWING ORDER OF TIDES: (RAD/SEC)
!                           M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA.
!            6. XJD75  -  THE JULIAN DATE OF JAN. 0.0 1975. (DAYS)
!
      DATA ANGFAC / 2.D0, -2.D0, 0.D0, 0.D0,            &
     &              0.D0,  0.D0, 0.D0, 0.D0,            &
     &              2.D0, -3.D0, 1.D0, 0.D0,            &
     &              2.D0,  0.D0, 0.D0, 0.D0,            &
     &              1.D0,  0.D0, 0.D0, 0.25D0,          &
     &              1.D0, -2.D0, 0.D0,-0.25D0,          &
     &             -1.D0,  0.D0, 0.D0,-0.25D0,          &
     &              1.D0, -3.D0, 1.D0,-0.25D0,          &
     &              0.D0,  2.D0, 0.D0, 0.D0,            &
     &              0.D0,  1.D0,-1.D0, 0.D0,            &
     &              2.D0,  0.D0, 0.D0, 0.D0 /
!
      DATA CENTJ / 36525.D0 /
!
      DATA SPEED / 1.40519D-4, 1.45444D-4, 1.37880D-4,  &
     &             1.45842D-4, 0.72921D-4, 0.67598D-4,  &
     &             0.72523D-4, 0.64959D-4, 0.053234D-4, &
     &             0.026392D-4, 0.003982D-4 /
!
      DATA XJD75 / 2442412.5D0 /
      data JCAPD / -9999 /   ! initial value, make sure its before 1973
!
!  1.2.8   PROGRAM VARIABLES
!            1. CAPT  -  THE NUMBER OF JULIAN CENTURIES BETWEEN JAN 0.5, 1900
!                        AND THE OBSERVATION.
!            2. FDAY  -  FRACTIONAL PART OF UNIVERSAL TIME (UT1) DAY IN
!                        SECONDS. (SEC)
!            3. H0    -  MEAN LONGITUDE OF SUN AT BEGINNING OF DAY. (RAD)
!            4. ICAPD -  JULIAN DAYS SINCE JANUARY 0.0 UT 1975. (DAYS)
!            5. P0    -  MEAN LONGITUDE OF LUNAR PERIGEE AT BEGINNING OF DAY.
!                        (RAD)
!            6. S0    -  MEAN LONGITUDE OF MOON AT BEGINNING OF DAY. (RAD)
!            7. JCAPD -  Previous value of ICAPD. Checked to see if we can
!                        reuse the previous values of CAPT, H0, P0, and S0.
!
!  1.2.9   PROGRAMMER  -  CLYDE GOAD
!          83:10:08 HAROLD M. SCHUH
!          89:10:08 Jim Ryan ANGFAC and SPEED moved to ema.
!          91:11:04 Jim Ryan ANGFAC(8,3) changed from 0.0 to 1.0 to make
!               consistent with IERS standards (Nov. 89)
!          David Gordon 94.04.18 Converted to Implicit None.
!          David Gordon 94.08.22 ICAPD had been incorrectly set to R*8, changed
!                       it to an integer - error had no effect. Put SPEED(11)
!                       into common block for use in geometry section [to
!                       replace DANGL(11)].
!          David Gordon 94.09.22 Added JCAPD and setting it to the previous
!                       value of ICAPD. If equal to current ICAPD, we skip
!                       computation of CAPT, H0, S0, and P0. CAPT, H0, S0, P0,
!                       and JCAPD put in save block.
!
!  1.3     OCARG PROGRAM STRUCTURE
!
!   Compute FDAY  -  fractional part of UT1 day in seconds.
      FDAY = UT1
!
!   Compute ICAPD  -  Days since JAN 0, 0.00 UT, 1975
      ICAPD = XJD - XJD75
!
!  Check to see if we've started a new day. If so, we need to compute new values
!    for CAPT, H0, S0, & P0. If not, then we can reuse the previous values.
      If (ICAPD .ne. JCAPD) Then
!
       JCAPD = ICAPD      !Reset JCAPD for next observation
!
!   Compute CAPT  -  Julian centuries since JAN 0.5, 1900.
      CAPT = ( 27392.500528D0 + 1.000000035D0 * ICAPD ) / CENTJ
!
!   Compute mean longitude of sun at beginning of day.
      H0 = (279.69668D0 + (36000.768930485D0 + 3.03D-4 * CAPT) * CAPT)  &
     &     * CONVD
!
!   Compute mean longitude of moon at beginning of day.
      S0 = ((( 1.9D-6 * CAPT - 0.001133D0 ) * CAPT + 481267.88314137D0) &
     &     * CAPT + 270.434358D0 ) * CONVD
!
!   Compute mean longitude of lunar perigee at beginning of day.
      P0 = ((( -1.2D-5 * CAPT - .010325D0 ) * CAPT + 4069.0340329577D0) &
     &     * CAPT + 334.329653D0 ) * CONVD
!
      Endif
!
!   Calculate the angular arguments. Run a loop over the 11 main tides.
!
      DO K = 1,11
        ANGLE(K) = SPEED(K)*FDAY + ANGFAC(1,K)*H0 + ANGFAC(2,K)*S0      &
     &             + ANGFAC(3,K)*P0 + ANGFAC(4,K)*TWOPI
!
        ANGLE(K) = DMOD( ANGLE(K), TWOPI )
        IF( ANGLE(K) .LT. 0.D0 ) ANGLE(K) = ANGLE(K) + TWOPI
      Enddo
!
!   Check KOCED to determine if debug output is requested.
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
!
!     Normal termination.
  800 RETURN
      END
