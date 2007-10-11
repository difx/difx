      SUBROUTINE PTDA
      IMPLICIT None
C 
C 1.    PTDA
C 
C 1.1   PTDA PROGRAM SPECIFICATION
C 
C 1.1.1 PTDA ADDS ENTRIES TO THE TABLE OF CONTENTS FOR THE POLE TIDE MODULE 
C       TEXT MESSAGE AND CONTRIBUTIONS ARRAYS. IT ALSO ADDS ENTRIES TO THE
C       TABLE OF CONTENTS FOR THE POLE TIDE MODULE FLOW CONTROL MESSAGE.
C 
C 1.2   PTDA PROGRAM INTERFACE
C 
C 1.2.4 DATA BASE ACCESS -
C          ACCESS CODES:
C            1. 'PTD MESS' - THE DATA BASE ACCESS CODE FOR THE 
C                            POLE TIDE MODULE TEXT MESSAGE.  
C            2. 'PTD CFLG' - THE DATA BASE ACCESS CODE FOR THE POLE 
C                            TIDE MODULE FLOW CONTROL MESSAGE.
C            3. 'PTD CONT' - THE DATA BASE ACCESS CODE FOR THE POLE  
C                            TIDE MODULE CONTRIBUTIONS ARRAY.
C            4. 'PTDXYPAR' - The data base access code for the pole tide 
C                            delay and rate partials w.r.t. X-pole and
C                            Y-pole.
C            5. 'PTOLDCON' - The data base access code for the contribution
C                            to restore the X_mean and Y_mean portion of 
C                            the pole tide.
C
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDA, ADDR
C
C 1.2.9 PROGRAMMER - TOM HERRING   07/01/84
C                    SAVITA GOEL   06/03/87 (CDS FOR A900)
C                    Jim Ryan      89.07.08 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.15 Changed to Implicit None
C                    David Gordon 98.11.24 Added Lcode 'PTDXYPAR', partials
C                             of delay and rate w.r.t. X-pole and Y-pole.
C                    David Gordon 99.01.19 Added Lcode 'PTOLDCON', contribution
C                             to get back to the old pole tide - without mean
C                             values removed.
C
C     PTDA PROGRAM STRUCTURE
C
C     ADD for pole tide module text message.
      CALL ADDA (1,'PTD MESS','Pole tide message definition    ',
     1     40, 1, 1 )
C
C     ADD for pole tide module flow control flag status.
      CALL ADDA (1,'PTD CFLG','Pole tide flow control mess def ',
     1     40,1,1)
C
C     ADD for pole tide module contributions.
      CALL ADDR (2,'PTD CONT','Pole tide contributions def.    ',
     1     2, 1, 1 )
C
C     ADD for pole tide delay and rate partials w.r.t. X-wobble and Y-wobble. 
      CALL ADDR (2,'PTDXYPAR','Pole Tide Partials w.r.t. X & Y ',
     1     2, 2, 1 )
C
C     ADD for old pole tide restorer contributions.
      CALL ADDR (2,'PTOLDCON','Old Pole Tide Restorer Contrib. ',
     1     2, 1, 1 )
C
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE PTDI
      IMPLICIT None
C
C 3.    PTDI
C
C 3.1   PTDI PROGRAM SPECIFICATION
C
C 3.1.1 PTDI IS THE POLE TIDE MODULE INPUT AND INITIALIZATION SECTION.
C
C 3.2.2 COMMON BLOCKS USED -
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C              1.  KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
C              2.  KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
C
C 3.2.3 PROGRAM SPECIFICATIONS -
      INTEGER*2  NDO(3), KERR
      SAVE KERR
      INTEGER*2      LPTDM(40),      LON(40),    LOFF(40),     LOXX(40)
      CHARACTER*40 C_LPTDM(2)     ,C_LON(2)   ,C_LOFF(2)   ,C_ LOXX(2)
      EQUIVALENCE (C_LPTDM,LPTDM),(C_LON,LON),(C_LOFF,LOFF),
     .            (C_LOXX ,LOXX)
C
      DATA C_LPTDM /
     .'Pole Tide Module - Last modification - J',
     .'anuary 19, 1999, D. Gordon, GSFC.       '/
C
      DATA C_LON /
     .'Pole Tide Module is turned on - contribu',
     .'tions applied to the theoreticals.      '/
C
      DATA C_LOFF /
     .'Pole Tide Module is turned off.         ',
     .'                                        '/
C
      DATA C_LOXX /
     .'Pole Tide Module is turned on - contribu',
     .'tions NOT applied to theoreticals.      '/
C
C 3.2.4 DATA BASE ACCESS -
C            'PUT' VARIABLES:
C              1.  LPTDM(40)  -  THE POLE TIDE MODULE TEXT MESSAGE.
C              2.  LON(40)    -  THE POLE TIDE MODULE TURNED ON MESSAGE.
C              3.  LOFF(40)   -  THE POLE TIDE MODULE TURNED OFF MESSAGE.
C            ACCESS CODES:
C              1.  'PTD MESS'  -  THE DATA BASE ACCESS CODE FOR THE
C                                 POLE TIDE MODULE TEXT MESSAGE.  
C              2.  'PTD DATA'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 POLE TIDE DATA. 
C              3.  'PTD CFLG'  -  THE DATA BASE ACCESS CODE FOR THE POLE  
C                                 TIDE MODULE FLOW CONTROL MESSAGE. 
C 
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL 
C             CALLED SUBROUTINES: KILL, PUTA
C
C 3.2.8 PROGRAM VARIABLES -
C           1.  KERR   -  THE DATA BASE ERROR RETURN FLAG.
C           2.  NDO(3) -  THE DATA BASE RETURN ARRAY INDICES.
C
C 3.2.9 PROGRAMMER - TOM HERRING   07/01/84
C                    DAVID GORDON  01/03/85 (ADDED FLAG 2)
C                    Jim Ryan      89.07.08 Documentation simplified.
C                    Jim Ryan      89.12.12 UNIX-like database interface
C                                  implimented.
C                    David Gordon  94.04.15 Changed to Implicit None
C                    David Gordon  98.08.04 Changed data base message for
C                                  Calc 9. 
C
C     PTDI PROGRAM STRUCTURE
C
C     PUT the Pole Tide Module text message.
      CALL PUTA ('PTD MESS      ', LPTDM, 40, 1, 1 )
C
C     PUT the Pole Tide Module flow control message. See message above
C     for meanings.
      IF (KPTDC .EQ. 0) CALL PUTA('PTD CFLG      ',LON,40,1,1)
      IF (KPTDC .EQ. 1) CALL PUTA('PTD CFLG      ',LOFF,40,1,1)
      IF (KPTDC .EQ. 2) CALL PUTA('PTD CFLG      ',LOXX,40,1,1)
      IF ( KERR .EQ. 0 ) GO TO 500
           CALL CKILL (6HPTDI  , 1, KERR )
C
C     Normal conclusion.
  500 CONTINUE
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE PTDG ( SITLAT, SITLON, SITRAD, WOBXR, WOBYR, DIURNV,
     .                  TCTOCF, R2000, CENT, POLTDP, POLTDV )
      IMPLICIT None
C
C 4.    PTDG
C 
C 4.1   PTDG PROGRAM SPECIFICATION
C 
C 4.1.1 PTDG Is the Pole tide module geometry section. It calculates the 
C       surface pole tide crustal displacements due to the oscillation of
C       the Earth's pole about a mean secular pole. The pole tide station 
C       offsets are due to the solid Earth's response to the change in the
C       centrifugal force due to the change in the pole position. There may
C       be additional loading effects due to ocean pole tide effects, but
C       these are not well understood, and not modelled here. 
C
C       Beginning with Calc 9.0, mean secular values for X-pole and Y-pole
C       are removed before computing the pole tide. Thus the long term effect
C       of pole tide, averaged over many Chandler cycles, should be null.
C       Previously, a small velocity, from the cumulative effect of the
C       secular movement of the pole, was imparted to each station. 
C       (maximums of ~.1 mm/year Up, and ~.03 mm/year horizontally.) The
C       secular mean values of X-pole and Y-pole are computed as simple 
C       linear functions of time, using formulae provided by Harald Schuh.
C       These equations are preliminary and may be changed slightly later,
C       with the 2000 IERS Conventions, perhaps. Later adjustments can be
C       mapped in using the partials w.r.t. X and Y. The X-mean and Y-mean
C       values subtractred out are:
C         X-mean(arc-sec) = +0.0039 + 0.00062*T  
C         Y-mean(arc-sec) = +0.1485 + 0.00333*T  
C       where T = number of years since 1945.85. The value of Y-mean used
C       below will be the negative of the above, since the sign of Y-pole
C       is flipped in the wobble module. 98.12.17
C       
C       At the request of Leonid Petrov, an Lcode contribution (PTOLDCON)
C       to undo all the above has been added. It will be computed and 
C       ADD'ed in PTDC. 99.01.19
C 
C 4.1.2 RESTRICTIONS - MUST BE CALLED AFTER SITG, AND DIRNL TO ENSURE
C                      THAT ALL OF THE VARIABLES THAT PTDG USES HAVE THE
C                      CORRECT VALUE. 
C 
C 4.1.3 REFERENCES - IERS Technical Note 21, IERS Conventions (1996), 
C                    page 67, equation 22. 
C                    E-mail from Harald Schuh, 17 Dec. 1998.
C 
C 4.2   PTDG PROGRAM INTERFACE
C 
C 4.2.1 CALLING SEQUENCE -
C         INPUT VARIABLES:
C           1.  SITLAT(2)     -  GEODETIC LATITUDE OF EACH SITE (RAD) 
C           2.  SITLON(2)     -  EAST LONGTIUDE OF EACH SITE (RAD)
C           3.  SITRAD(2)     -  SPHERICAL EARTH RADIUS OF EACH SITE (M) 
C           4.  WOBXR         -  LONG PERIOD WOBBLE OFFSET ALONG GREENWICH 
C                                MERIDIAN (RAD) 
C           5.  WOBYR         -  LONG PERIOD WOBBLE OFFSET ALONG LONGITUDE
C                                90 DEG EAST (RAD)
C           6.  DIURNV        -  DIURNAL SPIN RATE OF THE EARTH (RAD/SEC)
C           7.  TCTOCF(3,3,2) -  THE ROTATION MATRIX WHICH ROTATES THE
C                                TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST
C                                FIXED REFERENCE SYSTEM AT EACH SITE
C           8.  R2000(3,3,3)  -  THE COMPLETE CRUST FIXED TO J2000 ROTATION
C                                MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES
C                                (UNITLESS, 1/SEC, 1/SEC**2)
C           9. CENT           -  Number of Julian centuries elapsed since the
C                                epoch January 1.5, 2000. (centuries)
C         OUTPUT VARIABLES: 
C           1.  POLTDP(3,2)   -  THE CORRECTIONS TO THE J2000.0 GEOCENTRIC 
C                                SITE POSITION VECTORS DUE TO POLE TIDAL 
C                                EFFECTS AT EACH SITE. (M) [IN SOME CASES THIS
C                                IS ZEROED OUT DEPENDING ON THE VALUE OF THE
C                                POLE TIDE CONTROL FLAG KPTDC.]
C           2.  POLTDV(3,2)   -  THE CORRECTIONS TO THE J2000.0 GEOCENTRIC
C                                SITE VELOCITY VECTORS DUE TO POLE TIDAL
C                                EFFECTS AT EACH SITE. (M/SEC) [IN SOME CASES
C                                THIS IS ZEROED OUT DEPENDING ON THE VALUE OF 
C                                THE POLE TIDE CONTROL FLAG KPTDC.]
C 
C 4.2.2 COMMON BLOCKS USED -
C 
      Real*8  ZPLTDP(3,2), ZPLTDV(3,2), ZPLDPX(3,2), ZPLDVX(3,2),
     *        ZPLDPY(3,2), ZPLDVY(3,2), X_mean, Y_mean, DPTDP(2,2) 
      COMMON / PTDCM / ZPLTDP, ZPLTDV, ZPLDPX, ZPLDVX, ZPLDPY, ZPLDVY,
     *                 X_mean, Y_mean, DPTDP
C            VARIABLES FROM:
C             1.  ZPLTDP(3,2) - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                               POSITION VECTORS DUE TO POLE TIDAL EFFECTS AT
C                               EACH SITE. (M) 
C             2.  ZPLTDV(3,2) - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                               VELOCITY VECTORS DUE TO POLE TIDAL EFFECTS AT
C                               EACH SITE. (M/SEC) 
C             3.  ZPLDPX(3,2) - The partial derivative of the ZPLTDP with 
C                               respective to the X-wobble offset. (m/arcsec)
C             4.  ZPLDPY(3,2) - The partial derivative of the ZPLTDP with 
C                               respective to the Y-wobble offset. (m/arcsec)
C             5.  ZPLDVX(3,2) - The partial derivative of the ZPLTDV with 
C                               respective to the X-wobble offset.
C                               (m/sec/arcsec)
C             6.  ZPLDVY(3,2) - The partial derivative of the ZPLTDV with 
C                               respective to the Y-wobble offset.
C                               (m/sec/arcsec)
C             7.  X_mean, Y_mean - The current mean offsets of the pole, using
C                               Harald Schuh's formula.
C 
      Real*8            PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH /  PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C             VARIABLES 'FROM': 
C               1. HALFPI -  PI/2.D0 (UNITLESS) 
C
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1.  KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
C              2.  KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         then used downstream. Otherwise equals zero. For
C                         correlator usage.
C 
C 4.2.3 PROGRAM SPECIFICATIONS -
      Real*8 TCDISP(3,2), CFDISP(3,2), COLAT(2), SITLAT(2), SITLON(2),
     .       SITRAD(2), TCTOCF(3,3,2), R2000(3,3,3), POLTDP(3,2), 
     .       POLTDV(3,2), WOBXR, WOBYR, DIURNV, CENT,
     .       TCDSPX(3,2), TCDSPY(3,2), Tyr, WOBXd, WOBYd 
      Integer*4 L, I
C 
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG 
C             CALLED SUBROUTINES: DCOS, DSIN, VECRT 
C 
C 4.2.7 CONSTANTS USED - XLOVEH, XLOVEL, DIURNV, WOBXR, WOBYR 
C 
C 4.2.8 PROGRAM VARIABLES - 
C       1. TCDISP(3,2) - THE TOPOCENTRIC EARTH CRUSTAL DISPLACEMENTS AT EACH
C                        SITE (M) 
C       2. CFDISP(3,2) - THE CRUST FIXED GEOCENTRIC EARTH CRUSTAL DISPLACEMENTS
C                        AT EACH SITE.  (M) 
C       3. COLAT(2)    - COLATITUDE AT EACH SITE (USED FOR CONSISTENCY WITH REF
C                        1) (RAD)
C 
C 4.2.9 PROGRAMMER - TOM HERRING   07/01/84 
C                    DAVID GORDON  01/03/85 (ADDED FLAG 2 CODING)
C                    Jim Ryan      89.07.08 Documentation simplified.
C                    David Gordon  11/24/93 Local site gravity computation
C                                  copied from subroutine ETDG
C                    David Gordon 94.04.15 Changed to Implicit None
C                    David Gordon 94.04.27 Common block ETDCM changed (from 
C                                 Earth tide module).
C                    David Gordon 98.06.24 Changed common block ETDCM to match
C                                 Earth tide module.
C                    David Gordon 98.08.04 Changed computations to match 
C                                 1996 IERS Conventions; ETDCM not needed.  
C                                 Put in mods for Geocenter site (pole tide
C                                 zeroed out).
C                    David Gordon 98.11.24 Added computations of the partial
C                                 derivatives of the pole tide displacement
C                                 w.r.t. X-pole and Y-pole. Partials added to 
C                                 common block PTDCM. 
C                    David Gordon 98.12.17 Added variable CENT and computations
C                                 for a mean pole X and Y offsets and their
C                                 removal from the pole tide correction. 
C                    David Gordon 99.01.19 Added X_mean, Y_mean, and DPTDP to
C                                 Common /PTDCM/.
C
C     PTDG PROGRAM STRUCTURE
C
C     Compute the secular mean values of the X and Y pole positions
C      Year and fraction thereof from 1945.85
        Tyr = CENT*100.D0 + 2000.001368D0 - 1945.85D0
C     Mean secular offsets (arc-seconds)
          X_mean = ( 0.0039 + 0.00062*Tyr) 
          Y_mean = (-0.1485 - 0.00333*Tyr)
C
C     De-secularized X and Y (radians)
          WOBXd = WOBXR - X_mean*CONVDS
          WOBYd = WOBYR - Y_mean*CONVDS
C
C
C     The pole tide geometry for sites 1 and 2 are calculated 
C      separately by running through a loop twice.
C
      DO 1200  L = 1,2
C
C      Check for Geocenter site:
        IF (L .eq. Nzero) Then
         Do i=1,3 
           TCDISP(i,L) = 0.D0
           ZPLTDP(i,L) = 0.D0
           ZPLTDV(i,L) = 0.D0
           ZPLDPX(i,L) = 0.D0
           ZPLDVX(i,L) = 0.D0
           ZPLDPY(i,L) = 0.D0
           ZPLDVY(i,L) = 0.D0
         Enddo
         Go to 1200
        ENDIF
C
C       Compute the colatitude of the site
        COLAT(L) = HALFPI - SITLAT(L)
C
C   Compute the IERS Conventions (1996) topocentric displacements.
C         Up
C       TCDISP(1,L) = -32.D0 * DSIN( 2.D0*COLAT(L) ) * 1.D-3 *
C    *                ( WOBXR*DCOS(SITLON(L)) + WOBYR*DSIN(SITLON(L)) )
C    *                / CONVDS
C         East
C       TCDISP(2,L) =   9.D0 * DCOS( COLAT(L) ) * 1.D-3 *
C    *                ( WOBXR*DSIN(SITLON(L)) - WOBYR*DCOS(SITLON(L)) )
C    *                / CONVDS
C         North
C       TCDISP(3,L) =  9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *
C    *                ( WOBXR*DCOS(SITLON(L)) + WOBYR*DSIN(SITLON(L)) )
C    *                / CONVDS
C
C
C   Compute the IERS Conventions (1996) topocentric displacements with
C    mean offsets removed.
C         Up
        TCDISP(1,L) = -32.D0 * DSIN( 2.D0*COLAT(L) ) * 1.D-3 *
     *                ( WOBXd*DCOS(SITLON(L)) + WOBYd*DSIN(SITLON(L)) )
     *                / CONVDS
C         East
        TCDISP(2,L) =   9.D0 * DCOS( COLAT(L) ) * 1.D-3 *
     *                ( WOBXd*DSIN(SITLON(L)) - WOBYd*DCOS(SITLON(L)) )
     *                / CONVDS
C         North
        TCDISP(3,L) =  9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *
     *                ( WOBXd*DCOS(SITLON(L)) + WOBYd*DSIN(SITLON(L)) )
     *                / CONVDS
C
C    Rotate the displacements to the crust fixed geocentric system.
        CALL VECRT ( TCTOCF(1,1,L), TCDISP(1,L), CFDISP(1,L) )
C    Rotate the crust fixed geocentric displacements to the J2000.0 system.
        CALL VECRT ( R2000(1,1,1), CFDISP(1,L), ZPLTDP(1,L) )
C    Compute the contribution to the J2000.0 velocities due to the
C    effect of the rotation of the earth.
        CALL VECRT ( R2000(1,1,2), CFDISP(1,L), ZPLTDV(1,L) )
C
C  Compute the partial derivatives of the displacements w.r.t. X and Y 
C   Recall that the sign of Y-pole was reversed a while back. Units will
C   be meters/arc-sec.
C         Up
        TCDSPX(1,L) = -32.D0 * DSIN(2.D0*COLAT(L)) * 1.D-3 *
     *                 DCOS(SITLON(L))
        TCDSPY(1,L) = -32.D0 * DSIN(2.D0*COLAT(L)) * 1.D-3 *
     *                 DSIN(SITLON(L))
C         East
        TCDSPX(2,L) = 9.D0 * DCOS(COLAT(L)) * 1.D-3 * DSIN(SITLON(L)) 
        TCDSPY(2,L) = 9.D0 * DCOS(COLAT(L)) * 1.D-3 * -DCOS(SITLON(L))
C
C         North
        TCDSPX(3,L) = 9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *
     *                 DCOS(SITLON(L)) 
        TCDSPY(3,L) = 9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *
     *                 DSIN(SITLON(L)) 
C
C    Rotate to J2000 position and velocity offsets
        CALL VECRT ( TCTOCF(1,1,L),TCDSPX(1,L), CFDISP(1,L) )
        CALL VECRT ( R2000(1,1,1), CFDISP(1,L), ZPLDPX(1,L) )
        CALL VECRT ( R2000(1,1,2), CFDISP(1,L), ZPLDVX(1,L) )
C
        CALL VECRT ( TCTOCF(1,1,L),TCDSPY(1,L), CFDISP(1,L) )
        CALL VECRT ( R2000(1,1,1), CFDISP(1,L), ZPLDPY(1,L) )
        CALL VECRT ( R2000(1,1,2), CFDISP(1,L), ZPLDVY(1,L) )
C
C     Close the loop over the sites.
C
 1200 CONTINUE
C
C     Check KPTDC to determine if the pole tide module is to be turned off
C     and the contribution zeroed out.
      IF ( KPTDC .NE. 1 )  GO TO 300
      DO 220  L = 1,2
         DO 210  I = 1,3
            POLTDP(I,L) = 0.D0
            POLTDV(I,L) = 0.D0
            ZPLTDP(I,L) = 0.D0
            ZPLTDV(I,L) = 0.D0
            ZPLDPX(I,L) = 0.D0
            ZPLDVX(I,L) = 0.D0
            ZPLDPY(I,L) = 0.D0
            ZPLDVY(I,L) = 0.D0
  210    CONTINUE
  220 CONTINUE
C
  300 CONTINUE
C
C     Handle the normal case where the pole tide effect is to be applied
C     to the theoretical and is stored as a contribution.
      IF ( KPTDC .NE. 0 )  GO TO 400
      DO 320  L = 1,2
        DO 310  I = 1,3
          POLTDP(I,L) = ZPLTDP(I,L)
          POLTDV(I,L) = ZPLTDV(I,L)
  310   CONTINUE
  320 CONTINUE
C
  400 CONTINUE
C
C  Handle the case where the pole tide is stored as a contribution
C   but not applied to the theoretical.
      IF ( KPTDC .NE. 2 )  GO TO 500
      DO 420  L = 1,2
        DO 410  I = 1,3
          POLTDP(I,L) = 0.D0
          POLTDV(I,L) = 0.D0
  410   CONTINUE
  420 CONTINUE
C
C     Check KPTDD for debug output.
  500 IF ( KPTDD .EQ. 0 ) GO TO 600
C
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDG." )
C
      WRITE ( 6, 9200 ) TCDISP, CFDISP, POLTDP, POLTDV, SITLAT,
     1    SITLON,  WOBXR, WOBYR, R2000, TCTOCF
C
 9200 FORMAT (1X, "TCDISP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     1            "CFDISP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     2            "POLTDP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     3            "POLTDV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "SITLAT = ", 2 (  D30.16, 10X ), /, 1X,
     5            "SITLON = ", 2 (  D30.16, 10X ), /, 1X,
     8            "WOBXR  = ", D30.16, /, 1X,
     9            "WOBYR  = ", D30.16, /, 1X,
     B            "R2000  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),
     C            "TCTOCF = ", 6 ( 3 ( D30.16, 10X ), /, 1X ) )
C
       write (6,'(" ZPLTDP ",3f22.10)') ZPLTDP(1,L), ZPLTDP(2,L),
     *                                 ZPLTDP(3,L) 
       write (6,'(" ZPLTDV ",3f22.10)') ZPLTDV(1,L), ZPLTDV(2,L),
     *                                 ZPLTDV(3,L) 
       write (6,'(" ZPLDPX ",3f22.10)') ZPLDPX(1,L), ZPLDPX(2,L),
     *                                 ZPLDPX(3,L) 
       write (6,'(" ZPLDVX ",3f22.10)') ZPLDVX(1,L), ZPLDVX(2,L),
     *                                 ZPLDVX(3,L) 
       write (6,'(" ZPLDPY ",3f22.10)') ZPLDPY(1,L), ZPLDPY(2,L),
     *                                 ZPLDPY(3,L) 
       write (6,'(" ZPLDVY ",3f22.10)') ZPLDVY(1,L), ZPLDVY(2,L),
     *                                 ZPLDVY(3,L) 
       write (6,'(" TCDSPX ",3f22.10)') TCDSPX(1,L), TCDSPX(2,L),
     *                                 TCDSPX(3,L) 
       write (6,'(" TCDSPY ",3f22.10)') TCDSPY(1,L), TCDSPY(2,L),
     *                                 TCDSPY(3,L) 
C     Normal conclusion.
  600 RETURN
      END
C
C*******************************************************************************
      SUBROUTINE PTDP(STAR) 
      IMPLICIT None
C
C 5.    PTDP
C
C 5.1   PTDP PROGRAM SPECIFICATION
C
C 5.1.1 PTDP is the pole tide module partials section. It computes the partial
C       derivatives with respect to the X-wobble and Y-wobble (X, Y in arc
C       seconds)
C
C 5.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C            VARIABLES 'FROM':
C              1. VLIGHT  -  THE VELOCITY OF LIGHT IN VACUUM.  (M/SEC)
C
      Real*8  ZPLTDP(3,2), ZPLTDV(3,2), ZPLDPX(3,2), ZPLDVX(3,2),
     *        ZPLDPY(3,2), ZPLDVY(3,2), X_mean, Y_mean, DPTDP(2,2) 
      COMMON / PTDCM / ZPLTDP, ZPLTDV, ZPLDPX, ZPLDVX, ZPLDPY, ZPLDVY,
     *                 X_mean, Y_mean, DPTDP
C            VARIABLES FROM:
C             1. ZPLTDP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                               POSITION VECTORS DUE TO POLE TIDAL EFFECTS AT
C                               EACH SITE. (M)
C             2. ZPLTDV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                               VELOCITY VECTORS DUE TO POLE TIDAL EFFECTS AT
C                               EACH SITE. (M/SEC)
C             3. ZPLDPX(3,2)  - The partial derivative of the ZPLTDP with 
C                               respective to the X-wobble offset. (m/arcsec)
C             4. ZPLDPY(3,2)  - The partial derivative of the ZPLTDP with 
C                               respective to the Y-wobble offset. (m/arcsec)
C             5. ZPLDVX(3,2)  - The partial derivative of the ZPLTDV with 
C                               respective to the X-wobble offset.
C                               (m/sec/arcsec)
C             6. ZPLDVY(3,2)  - The partial derivative of the ZPLTDV with 
C                               respective to the Y-wobble offset.
C                               (m/sec/arcsec)
C            VARIABLES TO:
C               1. DPTDP(2,2) - See below.
C 
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
C              2. KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
C 
C 5.2.3 PROGRAM SPECIFICATIONS -
      Real*8 STAR(3)
C 
C 5.2.4 DATA BASE ACCESS -
C            'PUT' VARIABLES: 
C               1. DPTDP(2,2) - The pole tide partial derivatives of the delay
C                               the delay rate w.r.t. X-pole and Y-pole. First
C                               index runs over the delay partial w.r.t. X and
C                               Y, second runs over rate partial w.r.t X and Y.
C                               (seconds/arcsecond, seconds/sec/arcsec) 
C
C             ACCESS CODES: 
C               1. 'PTDXYPAR' - The data base access code for the pole tide 
C                               X and Y partials array.
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVR
C             CALLED SUBROUTINES: DOTP, PUT4, VECSB
C
C 5.2.7 CONSTANTS USED - VLIGHT
C
C 5.2.8 PROGRAM VARIABLES -
C          1. BASCRX(3,2) - The partial derivative of the J2000 baseline
C                           position and velocity vectors w.r.t to the X-pole
C                           offset. (m/arcsec, m/sec/arcsec)
C          2. BASCRY(3,2) - The partial derivative of the J2000 baseline
C                           position and velocity vectors w.r.t to the Y-pole
C                           offset. (m/arcsec, m/sec/arcsec)
C
C 5.2.9 PROGRAMMER -
C       David Gordon 11.24.98 Code added to compute delay and rate pole
C                    tide partials w.r.t. X-pole and Y-pole. 
C       David Gordon 99.01.19 Added X_mean, Y_mean, and DPTDP to Common /PTDCM/
C                    for use in contribution section.
C
       Real*8 BASCRX(3,2), BASCRY(3,2), DOTP
C
C  Compute the partials.
C
C  Compute the partials w.r.t. X and Y of the J2000.0 baseline position and
C   velocity vectors due to pole tide effects 
      CALL VECSB ( ZPLDPX(1,1), ZPLDPX(1,2), BASCRX(1,1) )
      CALL VECSB ( ZPLDVX(1,1), ZPLDVX(1,2), BASCRX(1,2) )
      CALL VECSB ( ZPLDPY(1,1), ZPLDPY(1,2), BASCRY(1,1) )
      CALL VECSB ( ZPLDVY(1,1), ZPLDVY(1,2), BASCRY(1,2) )
C
C  Complete the calculation of the partials.
        DPTDP(1,1) =  DOTP ( BASCRX(1,1), STAR ) / VLIGHT
        DPTDP(1,2) =  DOTP ( BASCRX(1,2), STAR ) / VLIGHT
        DPTDP(2,1) = -DOTP ( BASCRY(1,1), STAR ) / VLIGHT
        DPTDP(2,2) = -DOTP ( BASCRY(1,2), STAR ) / VLIGHT
C
C     PUT the pole tide partials.
      CALL PUT4 ('PTDXYPAR      ', DPTDP, 2, 2, 1 )
C
C  Check KPTDD for debug output.
      IF ( KPTDD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDC ",/)
      WRITE(6,9200) ZPLDPX,ZPLDVX,BASCRX,ZPLDPY,ZPLDVY,BASCRY,STAR,DPTDP
 9200 FORMAT (1X, "ZPLDPX = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),
     *            "ZPLDVX = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),
     *            "BASCRX = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),
     *            "ZPLDVY = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),
     *            "ZPLDVY = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),
     *            "BASCRY = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),
     *            "STAR   = ",     3 ( D25.16, 1X ), /, 1X,
     *            "DPTDP  = ",     4 ( D25.16, 1X ) )
C
C  Normal conclusion.
C
  500 RETURN
      END
C
C*******************************************************************************
      SUBROUTINE PTDC ( STAR )
      IMPLICIT None
C
C 6.    PTDC
C
C 6.1   PTDC PROGRAM SPECIFICATION
C
C 6.1.1 PTDC IS THE POLE TIDE MODULE CONTRIBUTION SECTION. IT COMPUTES THE 
C       CONTRIBUTION TO THE DELAY AND THE DELAY RATE DUE TO POLE TIDE EFFECTS.
C
C 6.2.1 CALLING SEQUENCE -
C           INPUT VARIABLES:
C             1. STAR(3)  -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C
C 6.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C            VARIABLES 'FROM':
C              1. VLIGHT - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C
      Real*8  ZPLTDP(3,2), ZPLTDV(3,2), ZPLDPX(3,2), ZPLDVX(3,2),
     *        ZPLDPY(3,2), ZPLDVY(3,2), X_mean, Y_mean, DPTDP(2,2) 
      COMMON / PTDCM / ZPLTDP, ZPLTDV, ZPLDPX, ZPLDVX, ZPLDPY, ZPLDVY,
     *                 X_mean, Y_mean, DPTDP
C            VARIABLES FROM:
C             1. ZPLTDP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                               POSITION VECTORS DUE TO POLE TIDAL EFFECTS AT
C                               EACH SITE. (M)
C             2. ZPLTDV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
C                               VELOCITY VECTORS DUE TO POLE TIDAL EFFECTS AT
C                               EACH SITE. (M/SEC)
C             3. ZPLDPX(3,2)  - The partial derivative of the ZPLTDP with 
C                               respective to the X-wobble offset. (m/arcsec)
C             4. ZPLDPY(3,2)  - The partial derivative of the ZPLTDP with 
C                               respective to the Y-wobble offset. (m/arcsec)
C             5. ZPLDVX(3,2)  - The partial derivative of the ZPLTDV with 
C                               respective to the X-wobble offset.
C                               (m/sec/arcsec)
C             6. ZPLDVY(3,2)  - The partial derivative of the ZPLTDV with 
C                               respective to the Y-wobble offset.
C                               (m/sec/arcsec)
C             7. X_mean, Y_mean-The current mean offsets of the pole, using
C                               Harald Schuh's formula.
C             8. DPTDP(2,2)   - The pole tide partial derivatives of the delay
C                               the delay rate w.r.t. X-pole and Y-pole. First
C                               index runs over the delay partial w.r.t. X and
C                               Y, second runs over rate partial w.r.t. X and Y.
C                               (seconds/arcsecond, seconds/sec/arcsec) 
C 
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
C              2. KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
C 
C 6.2.3 PROGRAM SPECIFICATIONS -
      Real*8 BASCOR(3,2), DPTDC(2), STAR(3), DOTP, PTOLD(2) 
      Integer*4 K
C 
C 6.2.4 DATA BASE ACCESS -
C            'PUT' VARIABLES: 
C               1. DPTDC(2) - THE POLE TIDE CONTRIBUTION TO THE DELAY AND TO
C                             THE DELAY RATE (SEC, SEC/SEC)
C               2. PTOLD(2) - The contribution that will effectively convert
C                             the pole tides to the non-mean-offset-removed 
C                             values, i.e. to the old (Calc 8.2 and earlier
C                             versions) values.
C             ACCESS CODES: 
C               1. 'PTD CONT' - THE DATA BASE ACCESS CODE FOR THE POLE TIDE 
C                               MODULE CONTRIBUTIONS ARRAY. 
C               2. 'PTOLDCON' - The data base access code for the old pole-
C                               tide-restored contribution.
C 
C 6.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVR
C             CALLED SUBROUTINES: DOTP, PUT4, VECSB
C
C 6.2.7 CONSTANTS USED - VLIGHT
C
C 6.2.8 PROGRAM VARIABLES -
C          1. BASCOR(3,2) - THE CORRECTION TO THE J2000.0 BASELINE POSITION AND
C                           VELOCITY VECTORS DUE TO THE POLE TIDE. (M, M/S)
C
C 6.2.9 PROGRAMMER - TOM HERRING   07/01/84
C                    DAVID GORDON  08/24/84  (ADDED SOME DEBUG)
C                    DAVID GORDON  01/03/85  (ADDED ZPLTDP AND ZPLTDV)
C                    Jim Ryan      89.07.08 Documentation simplified.
C                    Jim Ryan      89:10:05 CPHYS common made an include file.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.15 Changed to Implicit None
C                    D. Gordon 99.01.19 Added X_mean, Y_mean, and DPTDP to 
C                             Common /PTDCM/. Added calculation and PUT for
C                             the contribution to put back in the mean pole
C                             tide correction, Lcode 'PTOLDCON'.
C
C     PTDC PROGRAM STRUCTURE
C
C     Compute the contributions.
C
C  Compute the corrections to the J2000.0 baseline position and
C   velocity vectors due to pole tidal effects.
      CALL VECSB ( ZPLTDP(1,1), ZPLTDP(1,2), BASCOR(1,1) )
      CALL VECSB ( ZPLTDV(1,1), ZPLTDV(1,2), BASCOR(1,2) )
C  Complete the calculation of the contributions.
      DO 120  K = 1,2
        DPTDC(K) = DOTP ( BASCOR(1,K), STAR ) / VLIGHT
  120 CONTINUE
C
C  Compute the delay and rate contributions to restore the mean offsets that
C   were removed in the geometry section. Need to reverse the Y_mean sign.
        PTOLD(1) = X_mean*DPTDP(1,1) + -Y_mean*DPTDP(2,1)
        PTOLD(2) = X_mean*DPTDP(1,2) + -Y_mean*DPTDP(2,2)
C
C     PUT the contributions.
      CALL PUT4 ('PTD CONT      ', DPTDC, 2, 1, 1 )
      CALL PUT4 ('PTOLDCON      ', PTOLD, 2, 1, 1 )
C
C  Check KPTDD for debug output.
      IF ( KPTDD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDC ",/)
      WRITE ( 6, 9200 )  ZPLTDP, ZPLTDV, BASCOR, STAR, DPTDC
 9200 FORMAT (1X, "ZPLTDP = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),
     *            "ZPLTDV = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),
     *            "BASCOR = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),
     *            "STAR   = ",     3 ( D25.16, 5X ), /, 1X,
     *            "DPTDC  = ",     2 ( D25.16, 5X ), /, 1X,
     *            "PTOLD  = ",     2 ( D25.16, 5X ) )
C
C  Normal conclusion.
  500 RETURN
      END
