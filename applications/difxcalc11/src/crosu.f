      SUBROUTINE ROT2K ( CFLAT, CFLON, CFSITE, CFSITN, R2K  , EPLATP, &
     &                    EPLATV, EPLONP, EPLONV, EPSITN, SITEA, USITEP, &
     &                    USITEV )
      IMPLICIT None
!
! 1.    ROT2K
!
! 1.1   ROT2K PROGRAM SPECIFICATION
!
! 1.1.1 ROT2K is the utility which rotates the site geometry from the crust
!       fixed reference system to the J2000.0 reference system using the
!       IERS 2003 CEO-based TRF ==> CRF tranformation matrix.
!
! 1.2   ROT2K PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. CFLAT(3,2)   - THE PARTIAL DERIVATIVES OF THE GEOCENTRIC
!                               CRUST FIXED VECTOR COMPONENTS WITH RESPECT TO
!                               THE GEODETIC LATITUDES AT EACH OBSERVATION SITE.
!                               (M/RAD)
!             2. CFLON(3,2)   - THE PARTIAL DERIVATIVES OF THE GEOCENTRIC CRUST
!                               FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!                               EAST LONGITUDES AT EACH OBSERVATION SITE.
!                               (M/RAD)
!             3. CFSITE(3,2)  - THE GEOCENTRIC CRUST FIXED SITE VECTORS AT EACH
!                               OBSERVATION SITE. (M)
!             4. CFSITN(3,2)  - THE GEOCENTRIC CRUST FIXED SITE NORMAL UNIT
!                               VECTORS AT EACH OBSERVATION SITE. (UNITLESS)
!             5. R2K(3,3,3)   - THE IERS 2003 CEO-based CRUST FIXED TO J2000.0
!                               ROTATION MATRIX AND ITS FIRST TWO CT TIME
!                               DERIVATIVES. (UNITLESS, 1/SEC, 1/SEC**2)
!
!           OUTPUT VARIABLES:
!             1. EPLATP(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE POSITION VECTOR COMPONENTS WITH
!                               RESPECT TO THE GEODETIC LATITUDE AT EACH
!                               OBSERVATION SITE. (M/RAD)
!             2. EPLATV(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE VELOCITY VECTOR COMPONENTS WITH
!                               RESPECT TO THE GEODETIC LATITUDE AT EACH
!                               OBSERVATION SITE. (M/SEC-RAD)
!             3. EPLONP(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE POSITION VECTOR COMPONENTS WITH
!                               RESPECT TO THE EAST LONGITUDE AT EACH
!                               OBSERVATION SITE. (M/RAD)
!             4. EPLONV(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE VELOCITY VECTOR COMPONENTS WITH
!                               RESPECT TO THE EAST LONGITUDE AT EACH
!                               OBSERVATION SITE. (M/RAD-SEC)
!             5. EPSITN(3,2)  - THE J2000.0 GEOCENTRIC SITE NORMAL UNIT VECTORS
!                               AT EACH OBSERVATION SITE. (UNITLESS)
!             6. SITEA(3,2)   - THE J2000.0 GEOCENTRIC SITE ACCELERATION
!                               VECTORS. (M/SEC**2)
!                               (NOTE: IT IS NOT NECESSARY TO CORRECT THE
!                               J2000.0 SITE GEOCENTRIC ACCELERATION VECTORS
!                               FOR EARTH TIDAL AND OCEAN LOADING EFFECTS.)
!             7. USITEP(3,2)  - THE J2000.0 GEOCENTRIC SITE POSITION VECTORS
!                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
!                               EFFECTS. (M)
!             8. USITEV(3,2)  - THE J2000.0 GEOCENTRIC SITE VELOCITY VECTORS
!                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
!                               EFFECTS. (M/SEC)
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1. KROSC - THE ROSIT UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KROSD - THE ROSIT UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
      Real*8     CFLAT(3,2), CFLON(3,2), CFSITE(3,2), CFSITN(3,2), &
     &           EPLATP(3,2), EPLATV(3,2), EPLONP(3,2), EPLONV(3,2), &
     &           EPSITN(3,2), R2K  (3,3,3), SITEA(3,2), USITEP(3,2), &
     &           USITEV(3,2)
      Integer*4  L
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: VECRT
!
! 1.2.9 PROGRAMMER - DALE MARKHAM  01/17/77
!                    PETER DENATALE 07/18/77
!                    SAVITA GOEL    06/04/87 (CDS FOR A900)
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.18 Converted to Implicit None.
!                    David Gordon 2003.11.20 Updated for new solid body
!                                 rotation matrix (IERS 2003 model).
!
!   ROT2k Program Structure
!
!   ROTATE the site geometry from the crust fixed reference system
!     to the J2000.0 system.
!
!   Loop twice for the geometry of the two sites.
      DO 160  L = 1,2
!
!   Rotate the geocentric site vectors.
        CALL VECRT ( R2K(1,1,1), CFSITE(1,L), USITEP(1,L) )
        CALL VECRT ( R2K(1,1,2), CFSITE(1,L), USITEV(1,L) )
        CALL VECRT ( R2K(1,1,3), CFSITE(1,L), SITEA(1,L) )
!
!   Rotate the geocentric site normal unit vectors.
        CALL VECRT ( R2K(1,1,1), CFSITN(1,L), EPSITN(1,L) )
!
!   Rotate the geodetic latitude partial derivatives.
        CALL VECRT ( R2K(1,1,1), CFLAT(1,L), EPLATP(1,L) )
        CALL VECRT ( R2K(1,1,2), CFLAT(1,L), EPLATV(1,L) )
!
!   Rotate the East longitude partial derivatives.
        CALL VECRT ( R2K(1,1,1), CFLON(1,L), EPLONP(1,L) )
        CALL VECRT ( R2K(1,1,2), CFLON(1,L), EPLONV(1,L) )
!
!   Close the loop over the sites.
  160 CONTINUE
!
!   Check KROSD for debug output.
      IF ( KROSD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine ROT2K." )
      WRITE ( 6, 9200 )  CFLAT, CFLON, CFSITE, CFSITN, R2K, &
     &            EPLATP, EPLATV, EPLONP, EPLONV, EPSITN, SITEA, USITEP, &
     &                   USITEV
 9200 FORMAT (1X, "CFLAT  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CFLON  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CFSITE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CFSITN = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "R2K    = ", 9 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLATP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLATV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLONP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLONV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPSITN = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "SITEA  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "USITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "USITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
!********************************************************************
      SUBROUTINE ROTC2K ( CFLAT, CFLON, CFSITE, CFSITN, RC2K , EPLATP, &
     &                    EPLATV, EPLONP, EPLONV, EPSITN, SITEA, USITEP, &
     &                    USITEV )
      IMPLICIT None
!
! 1.    ROTC2K
!
! 1.1   ROTC2K PROGRAM SPECIFICATION
!
! 1.1.1 ROTC2K is the utility which rotates the site geometry from the crust
!       fixed reference system to the J2000.0 reference system using the
!       IERS 2003 classical TRF ==> CRF tranformation matrix.
!
! 1.2   ROTC2K PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. CFLAT(3,2)   - THE PARTIAL DERIVATIVES OF THE GEOCENTRIC
!                               CRUST FIXED VECTOR COMPONENTS WITH RESPECT TO
!                               THE GEODETIC LATITUDES AT EACH OBSERVATION SITE.
!                               (M/RAD)
!             2. CFLON(3,2)   - THE PARTIAL DERIVATIVES OF THE GEOCENTRIC CRUST
!                               FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!                               EAST LONGITUDES AT EACH OBSERVATION SITE.
!                               (M/RAD)
!             3. CFSITE(3,2)  - THE GEOCENTRIC CRUST FIXED SITE VECTORS AT EACH
!                               OBSERVATION SITE. (M)
!             4. CFSITN(3,2)  - THE GEOCENTRIC CRUST FIXED SITE NORMAL UNIT
!                               VECTORS AT EACH OBSERVATION SITE. (UNITLESS)
!             5. RC2K(3,3,3)  - THE IERS 2003 CEO-based CRUST FIXED TO J2000.0
!                               ROTATION MATRIX AND ITS FIRST TWO CT TIME
!                               DERIVATIVES. (UNITLESS, 1/SEC, 1/SEC**2)
!
!           OUTPUT VARIABLES:
!             1. EPLATP(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE POSITION VECTOR COMPONENTS WITH
!                               RESPECT TO THE GEODETIC LATITUDE AT EACH
!                               OBSERVATION SITE. (M/RAD)
!             2. EPLATV(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE VELOCITY VECTOR COMPONENTS WITH
!                               RESPECT TO THE GEODETIC LATITUDE AT EACH
!                               OBSERVATION SITE. (M/SEC-RAD)
!             3. EPLONP(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE POSITION VECTOR COMPONENTS WITH
!                               RESPECT TO THE EAST LONGITUDE AT EACH
!                               OBSERVATION SITE. (M/RAD)
!             4. EPLONV(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0
!                               GEOCENTRIC SITE VELOCITY VECTOR COMPONENTS WITH
!                               RESPECT TO THE EAST LONGITUDE AT EACH
!                               OBSERVATION SITE. (M/RAD-SEC)
!             5. EPSITN(3,2)  - THE J2000.0 GEOCENTRIC SITE NORMAL UNIT VECTORS
!                               AT EACH OBSERVATION SITE. (UNITLESS)
!             6. SITEA(3,2)   - THE J2000.0 GEOCENTRIC SITE ACCELERATION
!                               VECTORS. (M/SEC**2)
!                               (NOTE: IT IS NOT NECESSARY TO CORRECT THE
!                               J2000.0 SITE GEOCENTRIC ACCELERATION VECTORS
!                               FOR EARTH TIDAL AND OCEAN LOADING EFFECTS.)
!             7. USITEP(3,2)  - THE J2000.0 GEOCENTRIC SITE POSITION VECTORS
!                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
!                               EFFECTS. (M)
!             8. USITEV(3,2)  - THE J2000.0 GEOCENTRIC SITE VELOCITY VECTORS
!                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
!                               EFFECTS. (M/SEC)
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1. KROSC - THE ROSIT UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KROSD - THE ROSIT UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 1.2.3 PROGRAM SPECIFICATIONS -
      Real*8     CFLAT(3,2), CFLON(3,2), CFSITE(3,2), CFSITN(3,2), &
     &           EPLATP(3,2), EPLATV(3,2), EPLONP(3,2), EPLONV(3,2), &
     &           EPSITN(3,2), RC2K (3,3,3), SITEA(3,2), USITEP(3,2), &
     &           USITEV(3,2)
      Integer*4  L
!
! 1.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: VECRT
!
! 1.2.9 PROGRAMMER - DALE MARKHAM  01/17/77
!                    PETER DENATALE 07/18/77
!                    SAVITA GOEL    06/04/87 (CDS FOR A900)
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    David Gordon 94.04.18 Converted to Implicit None.
!                    David Gordon 2003.11.20 Updated for new solid body
!                                 rotation matrix (IERS 2003 model).
!
!   ROT2k Program Structure
!
!   ROTATE the site geometry from the crust fixed reference system
!     to the J2000.0 system.
!
!   Loop twice for the geometry of the two sites.
      DO 160  L = 1,2
!
!   Rotate the geocentric site vectors.
        CALL VECRT ( RC2K(1,1,1), CFSITE(1,L), USITEP(1,L) )
        CALL VECRT ( RC2K(1,1,2), CFSITE(1,L), USITEV(1,L) )
        CALL VECRT ( RC2K(1,1,3), CFSITE(1,L), SITEA(1,L) )
!
!   Rotate the geocentric site normal unit vectors.
        CALL VECRT ( RC2K(1,1,1), CFSITN(1,L), EPSITN(1,L) )
!
!   Rotate the geodetic latitude partial derivatives.
        CALL VECRT ( RC2K(1,1,1), CFLAT(1,L), EPLATP(1,L) )
        CALL VECRT ( RC2K(1,1,2), CFLAT(1,L), EPLATV(1,L) )
!
!   Rotate the East longitude partial derivatives.
        CALL VECRT ( RC2K(1,1,1), CFLON(1,L), EPLONP(1,L) )
        CALL VECRT ( RC2K(1,1,2), CFLON(1,L), EPLONV(1,L) )
!
!   Close the loop over the sites.
  160 CONTINUE
!
!   Check KROSD for debug output.
      IF ( KROSD .EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine ROTC2K." )
      WRITE ( 6, 9200 )  CFLAT, CFLON, CFSITE, CFSITN, RC2K, &
     &            EPLATP, EPLATV, EPLONP, EPLONV, EPSITN, SITEA, USITEP, &
     &                   USITEV
 9200 FORMAT (1X, "CFLAT  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CFLON  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CFSITE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "CFSITN = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "RC2K   = ", 9 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLATP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLATV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLONP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPLONV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "EPSITN = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "SITEA  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "USITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "USITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
!
  300 RETURN
      END
