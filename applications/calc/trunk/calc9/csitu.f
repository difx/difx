      SUBROUTINE SITCR ( TIDEP, TIDEV, USITEP, USITEV, XLOADP, XLOADV,
     1                    EPBASE, SITEP, SITEV, POLTDP, POLTDV )
      IMPLICIT None
C
C 1.    SITCR
C
C 1.1   SITCR PROGRAM SPECIFICATION
C
C 1.1.1 SITCR computes the instantaneous J2000.0 geocentric site position and
C       velocity vectors by adding in the effects of Earth tides, the pole tide
C       and ocean loading. The J2000.0 geocentric baseline position and velocity
C       vectors are similiarly corrected.
C
C 1.2   SITCR PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE -
C
C          INPUT VARIABLES:
C            1. TIDEP(3,2)   -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
C                               POSITION VECTORS DUE TO EARTH TIDAL EFFECTS. (M)
C            2. TIDEV(3,2)   -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
C                               VELOCITY VECTORS DUE TO EARTH TIDAL EFFECTS.
C                               (M/SEC)
C            3. USITEP(3,2)  -  THE 2000.0 GEOCENTRIC SITE POSITION VECTORS 
C                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
C                               EFFECTS. (M)
C            4. USITEV(3,2)  -  THE 2000.0 GEOCENTRIC SITE VELOCITY VECTORS
C                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
C                               EFFECTS. (M/SEC)
C            5. XLOADP(3,2)  -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
C                               POSITION VECTORS DUE TO OCEAN LOADING EFFECTS.
C                               (M) 
C            6. XLOADV(3,2)  -  THE CORRECTIONS TO THE 2000.0 GEOCENTRIC SITE
C                               VELOCITY VECTORS DUE TO OCEAN LOADING EFFECTS.
C                              (M/SEC) 
C            7. POLTDP(3,2)  -  GEOCENTRIC 2000.0 SITE POSITION CORRECTION FOR
C                               THE EFFECTS OF THE POLE TIDE. (METERS) 
C            8. POLTDV(3,2)  -  GEOCENTRIC 2000.0 SITE VELOCITY CORRECTION FOR 
C                               THE EFFECTS OF THE POLE TIDE. (M/SEC)
C
C           OUTPUT VARIABLES: 
C            1. EPBASE(3,2)  -  THE 2000.0 GEOCENTRIC BASELINE POSITION AND
C                               VELOCITY VECTORS CORRECTED FOR EARTH TIDAL
C                               EFFECTS.(M, M/SEC)
C            2. SITEP(3,2)   -  THE 2000.0 GEOCENTRIC SITE POSITION VECTORS
C                               CORRECTED FOR EARTH TIDAL EFFECTS. (M) 
C            3. SITEV(3,2)   -  THE 2000.0 GEOCENTRIC SITE VELOCITY VECTORS
C                               CORRECTED FOR EARTH TIDAL EFFECTS. (M/SEC) 
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KSTEC  -  THE SITCR UTILITY ROUTINE FLOW CONTROL FLAG.
C              2. KSTED  -  THE SITCR UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C 1.2.3 PROGRAM SPECIFICATIONS -
      Real*8 CSITEP(3,2), CSITEV(3,2), EPBASE(3,2), SITEP(3,2), 
     1       SITEV(3,2), TIDEP(3,2), TIDEV(3,2), USITEP(3,2), 
     2       USITEV(3,2), XLOADP(3,2), XLOADV(3,2), POLTDP(3,2),
     3       POLTDV(3,2), TX(3,2), TV(3,2)
      Integer*4 L
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG 
C             CALLED SUBROUTINES: VECAD, VECSB
C 
C 1.2.7 CONSTANTS USED - NONE 
C 
C 1.2.8 PROGRAM VARIABLES - 
C             1. CSITEP(3,2) - THE INSTANTANEOUS CORRECTIONS TO THE J2000.0
C                              SITE POSITION VECTORS DUE TO EARTH TIDE, POLE
C                              TIDE AND OCEAN LOADING EFFECTS. (M) 
C             2. CSITEV(3,2) - THE INSTANTANEOUS CORRECTIONS TO THE J2000.0
C                              SITE VELOCITY VECTORS DUE TO EARTH TIDE, POLE
C                              TIDE AND OCEAN LOADING EFFECTS. (M/SEC) 
C             3. TX(3,2)     - TEMPORARY STORAGE VECTOR FOR THE SUM OF THE 
C                              EARTH TIDE AND THE OCEAN LOADING SITE POSITION
C                              CORRECTIONS. (M) 
C             4. TV(3,2)     - TEMPORARY STORAGE VECTOR FOR THE SUM OF THE
C                              EARTH TIDE AND THE OCEAN LOADING SITE VELOCITY
C                              CORRECTIONS. (M) 
C
C 1.2.9 PROGRAMMER - DALE MARKHAM   01/17/77
C                    PETER DENATALE 07/18/77
C                    DAVID GORDON   07/12/84
C                    SAVITA GOEL    06/04/87 (CDS FOR A900)
C                    Jim Ryan 89.07.27 Documetation simplified.
C                    David Gordon 94.04.18 Converted to Implicit None.
C
C     SITCR Program Structure
C
C   Compute the Earth tide, ocean loading, and pole tide
C   corrections to the J2000.0 geocentric site vectors.
      DO 100 L = 1,2
C       Add tide and ocean, then add pole tide. Positions then velocities.
        CALL VECAD ( TIDEP(1,L), XLOADP(1,L), TX(1,L)     )
        CALL VECAD ( TX(1,L)   , POLTDP(1,L), CSITEP(1,L) )
        CALL VECAD ( TIDEV(1,L), XLOADV(1,L), TV(1,L)     )
        CALL VECAD ( TV(1,L)   , POLTDV(1,L), CSITEV(1,L) )
  100 CONTINUE
C
C   Compute the corrected J2000.0 geocentric site vectors.
      DO 200  L = 1,2
        CALL VECAD ( USITEP(1,L), CSITEP(1,L), SITEP(1,L) )
        CALL VECAD ( USITEV(1,L), CSITEV(1,L), SITEV(1,L) )
  200 CONTINUE
C
C   Compute the corrected J2000.0 geocentric baseline vectors.
      CALL VECSB ( SITEP(1,1), SITEP(1,2), EPBASE(1,1) )
      CALL VECSB ( SITEV(1,1), SITEV(1,2), EPBASE(1,2) )
C
C   Check KSTED for debug output.
      IF ( KSTED .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility SITCR." )
      WRITE ( 6, 9200 )  CSITEP, CSITEV, EPBASE, SITEP, SITEV,
     1                   TIDEP, TIDEV, USITEP, USITEV, XLOADP, XLOADV,
     2                   POLTDP, POLTDV, TX, TV
 9200 FORMAT (1X, "CSITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "CSITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     2            "EPBASE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     3            "SITEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "SITEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     5            "TIDEP  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     6            "TIDEV  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     7            "USITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     8            "USITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     9            "XLOADP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     A            "XLOADV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     B            "POLTDP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     C            "POLTDV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     D            "TX     = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     E            "TV     = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  500 RETURN
      END
