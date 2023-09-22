      SUBROUTINE dSITCR (TIDEP, TIDEV, USITEP, USITEV, XLOADP, XLOADV,  &
     &                   EPBASE, SITEP, SITEV, POLTDP, POLTDV,          &
     &                   OPTLOADP, OPTLOADV)
      IMPLICIT None
!
! 1.    SITCR
!
! 1.1   SITCR PROGRAM SPECIFICATION
!
! 1.1.1 SITCR computes the instantaneous J2000.0 geocentric site position and
!       velocity vectors by adding in the effects of Earth tides, the pole tide,
!       ocean loading and ocean ploe tide loading. The J2000.0 geocentric 
!       baseline position and velocity vectors are similiarly corrected.
!       --- difx version ---
!
! 1.2   SITCR PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!
!          INPUT VARIABLES:
!            1. TIDEP(3,2)   -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
!                               POSITION VECTORS DUE TO EARTH TIDAL EFFECTS. (M)
!            2. TIDEV(3,2)   -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
!                               VELOCITY VECTORS DUE TO EARTH TIDAL EFFECTS.
!                               (M/SEC)
!            3. USITEP(3,2)  -  THE 2000.0 GEOCENTRIC SITE POSITION VECTORS
!                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
!                               EFFECTS. (M)
!            4. USITEV(3,2)  -  THE 2000.0 GEOCENTRIC SITE VELOCITY VECTORS
!                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
!                               EFFECTS. (M/SEC)
!            5. XLOADP(3,2)  -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
!                               POSITION VECTORS DUE TO OCEAN LOADING EFFECTS.
!                               (M)
!            6. XLOADV(3,2)  -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
!                               VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS.
!                              (M/SEC)
!            7. POLTDP(3,2)  -  GEOCENTRIC 2000.0 SITE POSITION CORRECTION FOR
!                               THE EFFECTS OF THE POLE TIDE. (METERS)
!            8. POLTDV(3,2)  -  GEOCENTRIC 2000.0 SITE VELOCITY CORRECTION FOR
!                               THE EFFECTS OF THE POLE TIDE. (M/SEC)
!
!           OUTPUT VARIABLES:
!            1. EPBASE(3,2)  -  THE 2000.0 GEOCENTRIC BASELINE POSITION AND
!                               VELOCITY VECTORS CORRECTED FOR EARTH TIDAL
!                               EFFECTS.(M, M/SEC)
!            2. SITEP(3,2)   -  THE 2000.0 GEOCENTRIC SITE POSITION VECTORS
!                               CORRECTED FOR EARTH TIDAL EFFECTS. (M)
!            3. SITEV(3,2)   -  THE 2000.0 GEOCENTRIC SITE VELOCITY VECTORS
!                               CORRECTED FOR EARTH TIDAL EFFECTS. (M/SEC)
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KSTEC  -  THE SITCR UTILITY ROUTINE FLOW CONTROL FLAG.
!              2. KSTED  -  THE SITCR UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
       INCLUDE 'cuser11.i'
!       Variables from:
!            1. Calc_user   - Calc user type. 'A' for Calc/SOLVE analysis.
!                             'C' for VLBI correlator.
!            2. Apply_ocean - Switch to apply ocean loading to theoreticals
!                             for correlator usage. 'Y' to apply (recommended),
!                             'N' for do not apply.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
      Real*8 CSITEP(3,2), CSITEV(3,2), EPBASE(3,2), SITEP(3,2),         &
     &       SITEV(3,2), TIDEP(3,2), TIDEV(3,2), USITEP(3,2),           &
     &       USITEV(3,2), XLOADP(3,2), XLOADV(3,2), POLTDP(3,2),        &
     &       POLTDV(3,2), TX(3,2), TV(3,2), OPTLOADP(3,2),              &
     &       OPTLOADV(3,2), XOP(3,2), XOV(3,2)
      Integer*4 L
!
! 1.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: VECAD, VECSB
!
! 1.2.7 CONSTANTS USED - NONE
!
! 1.2.8 PROGRAM VARIABLES -
!             1. CSITEP(3,2) - THE INSTANTANEOUS CORRECTIONS TO THE J2000.0
!                              SITE POSITION VECTORS DUE TO EARTH TIDE, POLE
!                              TIDE AND OCEAN LOADING EFFECTS. (M)
!             2. CSITEV(3,2) - THE INSTANTANEOUS CORRECTIONS TO THE J2000.0
!                              SITE VELOCITY VECTORS DUE TO EARTH TIDE, POLE
!                              TIDE AND OCEAN LOADING EFFECTS. (M/SEC)
!             3. TX(3,2)     - TEMPORARY STORAGE VECTOR FOR THE SUM OF THE
!                              EARTH TIDE AND THE OCEAN LOADING SITE POSITION
!                              CORRECTIONS. (M)
!             4. TV(3,2)     - TEMPORARY STORAGE VECTOR FOR THE SUM OF THE
!                              EARTH TIDE AND THE OCEAN LOADING SITE VELOCITY
!                              CORRECTIONS. (M)
!
! 1.2.9 PROGRAMMER - DALE MARKHAM   01/17/77
!                    PETER DENATALE 07/18/77
!                    DAVID GORDON   07/12/84
!                    SAVITA GOEL    06/04/87 (CDS FOR A900)
!                    Jim Ryan 89.07.27 Documetation simplified.
!                    David Gordon 94.04.18 Converted to Implicit None.
!                    David Gordon Jan. 2013. Added ocean pole tide loading.
!                                 Added decision wheter to apply ocean and
!                                 ocean pole tide corrections (formerly in
!                                 OCEG).
!
!     SITCR Program Structure
!
!  Add together ocean loading and ocean pole tide loading.
        DO L = 1,2
         CALL VECAD ( XLOADP(1,L), OPTLOADP(1,L), XOP(1,L)     )
         CALL VECAD ( XLOADV(1,L), OPTLOADV(1,L), XOV(1,L)     )
        ENDDO
!
!   Compute the Earth tide, ocean loading, and pole tide
!   corrections to the J2000.0 geocentric site vectors.
      DO 100 L = 1,2
!       Add tide and ocean/ocean pole tide, then add pole tide. 
!         Positions then velocities.
        CALL VECAD ( TIDEP(1,L), XOP(1,L),    TX(1,L)     )
        CALL VECAD ( TX(1,L)   , POLTDP(1,L), CSITEP(1,L) )
!
        CALL VECAD ( TIDEV(1,L), XOV(1,L),    TV(1,L)     )
        CALL VECAD ( TV(1,L)   , POLTDV(1,L), CSITEV(1,L) )
  100 CONTINUE
!
!   Compute the corrected J2000.0 geocentric site vectors.
      DO 200  L = 1,2
        CALL VECAD ( USITEP(1,L), CSITEP(1,L), SITEP(1,L) )
        CALL VECAD ( USITEV(1,L), CSITEV(1,L), SITEV(1,L) )
  200 CONTINUE
!
!   Compute the corrected J2000.0 geocentric baseline vectors.
      CALL VECSB ( SITEP(1,1), SITEP(1,2), EPBASE(1,1) )
      CALL VECSB ( SITEV(1,1), SITEV(1,2), EPBASE(1,2) )
!
!   Check KSTED for debug output.
      IF ( KSTED .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility SITCR." )
      WRITE ( 6, 9200 )  CSITEP, CSITEV, EPBASE, SITEP, SITEV,          &
     &                   TIDEP, TIDEV, USITEP, USITEV, XLOADP, XLOADV,  &
     &           POLTDP, POLTDV, OPTLOADP, OPTLOADV, XOP, XOV, TX, TV
 9200 FORMAT (1X, "CSITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CSITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPBASE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "SITEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "SITEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "TIDEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "TIDEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "USITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "USITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "XLOADP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "XLOADV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "POLTDP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "POLTDV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &          "OPTLOADP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &          "OPTLOADV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "XOP    = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "XOV    = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "TX     = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "TV     = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  500 RETURN
      END
