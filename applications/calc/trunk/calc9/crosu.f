      SUBROUTINE ROSIT ( CFLAT, CFLON, CFSITE, CFSITN, R2000, EPLATP,
     1                    EPLATV, EPLONP, EPLONV, EPSITN, SITEA, USITEP,
     2                    USITEV )
      IMPLICIT None
C
C 1.    ROSIT
C
C 1.1   ROSIT PROGRAM SPECIFICATION
C
C 1.1.1 ROSIT is the utility which rotates the site geometry from the crust
C       fixed reference system to the J2000.0 reference system.
C
C 1.1.2 RESTRICTIONS - NONE
C
C 1.1.3 REFERENCES - MARKHAM'S X-DOCUMENT
C
C 1.2   ROSIT PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. CFLAT(3,2)   - THE PARTIAL DERIVATIVES OF THE GEOCENTRIC
C                               CRUST FIXED VECTOR COMPONENTS WITH RESPECT TO 
C                               THE GEODETIC LATITUDES AT EACH OBSERVATION SITE.
C                               (M/RAD)
C             2. CFLON(3,2)   - THE PARTIAL DERIVATIVES OF THE GEOCENTRIC CRUST
C                               FIXED VECTOR COMPONENTS WITH RESPECT TO THE 
C                               EAST LONGITUDES AT EACH OBSERVATION SITE.
C                               (M/RAD) 
C             3. CFSITE(3,2)  - THE GEOCENTRIC CRUST FIXED SITE VECTORS AT EACH
C                               OBSERVATION SITE. (M)
C             4. CFSITN(3,2)  - THE GEOCENTRIC CRUST FIXED SITE NORMAL UNIT
C                               VECTORS AT EACH OBSERVATION SITE. (UNITLESS)
C             5. R2000(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
C                               MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES. 
C                               (UNITLESS, 1/SEC, 1/SEC**2) 
C 
C           OUTPUT VARIABLES: 
C             1. EPLATP(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0 
C                               GEOCENTRIC SITE POSITION VECTOR COMPONENTS WITH
C                               RESPECT TO THE GEODETIC LATITUDE AT EACH 
C                               OBSERVATION SITE. (M/RAD) 
C             2. EPLATV(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0 
C                               GEOCENTRIC SITE VELOCITY VECTOR COMPONENTS WITH
C                               RESPECT TO THE GEODETIC LATITUDE AT EACH 
C                               OBSERVATION SITE. (M/SEC-RAD) 
C             3. EPLONP(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0 
C                               GEOCENTRIC SITE POSITION VECTOR COMPONENTS WITH
C                               RESPECT TO THE EAST LONGITUDE AT EACH 
C                               OBSERVATION SITE. (M/RAD) 
C             4. EPLONV(3,2)  - THE PARTIAL DERIVATIVES OF THE J2000.0 
C                               GEOCENTRIC SITE VELOCITY VECTOR COMPONENTS WITH
C                               RESPECT TO THE EAST LONGITUDE AT EACH
C                               OBSERVATION SITE. (M/RAD-SEC) 
C             5. EPSITN(3,2)  - THE J2000.0 GEOCENTRIC SITE NORMAL UNIT VECTORS
C                               AT EACH OBSERVATION SITE. (UNITLESS)
C             6. SITEA(3,2)   - THE J2000.0 GEOCENTRIC SITE ACCELERATION
C                               VECTORS. (M/SEC**2) 
C                               (NOTE: IT IS NOT NECESSARY TO CORRECT THE
C                               J2000.0 SITE GEOCENTRIC ACCELERATION VECTORS 
C                               FOR EARTH TIDAL AND OCEAN LOADING EFFECTS.)
C             7. USITEP(3,2)  - THE J2000.0 GEOCENTRIC SITE POSITION VECTORS
C                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
C                               EFFECTS. (M) 
C             8. USITEV(3,2)  - THE J2000.0 GEOCENTRIC SITE VELOCITY VECTORS
C                               UNCORRECTED FOR EARTH TIDAL AND OCEAN LOADING
C                               EFFECTS. (M/SEC) 
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'             
C 
C           VARIABLES 'FROM':
C             1. KROSC - THE ROSIT UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KROSD - THE ROSIT UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C            VARIABLES 'TO': NONE 
C 
C 1.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8     CFLAT(3,2), CFLON(3,2), CFSITE(3,2), CFSITN(3,2),
     1           EPLATP(3,2), EPLATV(3,2), EPLONP(3,2), EPLONV(3,2),
     2           EPSITN(3,2), R2000(3,3,3), SITEA(3,2), USITEP(3,2),
     3           USITEV(3,2)
      Integer*2  L, idm6
C 
C 1.2.4 DATA BASE ACCESS - NONE 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG 
C             CALLED SUBROUTINES: VECRT 
C 
C 1.2.7 CONSTANTS USED - NONE 
C 
C 1.2.8 PROGRAM VARIABLES - NONE
C 
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/17/77
C                    PETER DENATALE 07/18/77
C                    SAVITA GOEL    06/04/87 (CDS FOR A900)
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    David Gordon 94.04.18 Converted to Implicit None.
C
C   ROSIT Program Structure
C
C   ROTATE the site geometry from the crust fixed reference system
C     to the J2000.0 system.
C
C   Loop twice for the geometry of the two sites.
      DO 160  L = 1,2
C
C   Rotate the geocentric site vectors.
        CALL VECRT ( R2000(1,1,1), CFSITE(1,L), USITEP(1,L) )
        CALL VECRT ( R2000(1,1,2), CFSITE(1,L), USITEV(1,L) )
        CALL VECRT ( R2000(1,1,3), CFSITE(1,L), SITEA(1,L) )
C
C   Rotate the geocentric site normal unit vectors.
        CALL VECRT ( R2000(1,1,1), CFSITN(1,L), EPSITN(1,L) )
C
C   Rotate the geodetic latitude partial derivatives.
        CALL VECRT ( R2000(1,1,1), CFLAT(1,L), EPLATP(1,L) )
        CALL VECRT ( R2000(1,1,2), CFLAT(1,L), EPLATV(1,L) )
C
C   Rotate the East longitude partial derivatives.
        CALL VECRT ( R2000(1,1,1), CFLON(1,L), EPLONP(1,L) )
        CALL VECRT ( R2000(1,1,2), CFLON(1,L), EPLONV(1,L) )
C
C   Close the loop over the sites.
  160 CONTINUE
C
C   Check KROSD for debug output.
      IF ( KROSD. EQ. 0 )  GO TO 300
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine ROSIT." )
      WRITE ( 6, 9200 )  CFLAT, CFLON, CFSITE, CFSITN, R2000,
     1            EPLATP, EPLATV, EPLONP, EPLONV, EPSITN, SITEA, USITEP,
     2                   USITEV
 9200 FORMAT (1X, "CFLAT  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "CFLON  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     2            "CFSITE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     3            "CFSITN = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "R2000  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),
     5            "EPLATP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     6            "EPLATV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     7            "EPLONP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     8            "EPLONV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     9            "EPSITN = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     A            "SITEA  = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "USITEP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     2            "USITEV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ) )
C
  300 RETURN
      END
