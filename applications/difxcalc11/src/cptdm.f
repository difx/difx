      SUBROUTINE PTDG (SITLAT, SITLON, SITRAD, WOBXR, WOBYR,            &
     &          TCTOCF, R2K, CENT, POLTDP, POLTDV, WOBXds, WOBYds)
      IMPLICIT None
!
! 4.    PTDG
!
! 4.1   PTDG PROGRAM SPECIFICATION
!
! 4.1.1 PTDG Is the Pole tide module geometry section. It calculates the
!       surface pole tide crustal displacements due to the oscillation of
!       the Earth's pole about a mean secular pole. The pole tide station
!       offsets are due to the solid Earth's response to the change in the
!       centrifugal force due to the change in the pole position. There may
!       be additional loading effects due to ocean pole tide effects, but
!       these are not well understood, and not modelled here.
!
!       Beginning with Calc 9.0, mean secular values for X-pole and Y-pole
!       are removed before computing the pole tide. Thus the long term effect
!       of pole tide, averaged over many Chandler cycles, should be null.
!       Previously, a small velocity, from the cumulative effect of the
!       secular movement of the pole, was imparted to each station.
!       (maximums of ~.1 mm/year Up, and ~.03 mm/year horizontally.) The
!       secular mean values of X-pole and Y-pole are computed as simple
!       linear functions of time:
!          NEW VALUES, from IERS Conventions (2003)
!         X-mean(arc-sec) = +0.054  + 0.00083*T
!         Y-mean(arc-sec) = +0.357  + 0.00395*T
!       where T = number of years since 2000. The value of Y-mean used
!       below will be the negative of the above, since the sign of Y-pole
!       is flipped in the wobble module. 2004.03.26
!
!       Update for Calc 11.0 from IERS Conventions (2010).
!       Before 2010, the mean pole is modeled using a third degree polynomial:
!          X_mean(arc-sec) = (55.974D-3 + 1.8243D-3*Tyr + .18413D-3*Tyr**2      
!                            + .007024D-3*Tyr**3)
!          Y_mean(arc-sec) = -(346.346D-3 + 1.7896D-3*Tyr - .10729D-3*Tyr**2 
!                            - .000908D-3*Tyr**3)
!       After 2010, a linear model is used:
!          X_mean(arc-sec) = (23.513D-3 + 7.6141D-3*Tyr)
!          Y_mean(arc-sec) = -(358.891D-3 - 0.6287D-3*Tyr)
!
!       An Lcode contribution ('PTOLDCON') to undo all the above is computed 
!       in PTDC. If both 'PTD CONT' and 'PTOLDCON' are applied in Solve, then
!       the result is a pole tide using the total X and Y polar motion offsets. 
!
! 4.1.2 RESTRICTIONS - MUST BE CALLED AFTER SITG, AND DIRNL TO ENSURE
!                      THAT ALL OF THE VARIABLES THAT PTDG USES HAVE THE
!                      CORRECT VALUE.
!
! 4.1.3 REFERENCES - IERS Technical Note 36, IERS Conventions (2010),
!                    chapter 7, pages 114-116 and Table 7.7.
!
! 4.2   PTDG PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!         INPUT VARIABLES:
!           1. SITLAT(2)     -  GEODETIC LATITUDE OF EACH SITE (RAD)
!           2. SITLON(2)     -  EAST LONGTIUDE OF EACH SITE (RAD)
!           3. SITRAD(2)     -  SPHERICAL EARTH RADIUS OF EACH SITE (M)
!           4. WOBXR         -  LONG PERIOD WOBBLE OFFSET ALONG GREENWICH
!                               MERIDIAN (RAD)
!           5. WOBYR         -  LONG PERIOD WOBBLE OFFSET ALONG LONGITUDE
!                               90 DEG EAST (RAD)
!           7. TCTOCF(3,3,2) -  THE ROTATION MATRIX WHICH ROTATES THE
!                               TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST
!                               FIXED REFERENCE SYSTEM AT EACH SITE
!           8. R2K(3,3,3)    -  THE COMPLETE CRUST FIXED TO J2000 ROTATION
!                               MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES
!                               (UNITLESS, 1/SEC, 1/SEC**2)
!           9. CENT          -  Number of Julian centuries elapsed since the
!                               epoch January 1.5, 2000. (centuries)
!         OUTPUT VARIABLES:
!           1. POLTDP(3,2)   -  THE CORRECTIONS TO THE J2000.0 GEOCENTRIC
!                               SITE POSITION VECTORS DUE TO POLE TIDAL
!                               EFFECTS AT EACH SITE. (M) [IN SOME CASES THIS
!                               IS ZEROED OUT DEPENDING ON THE VALUE OF THE
!                               POLE TIDE CONTROL FLAG KPTDC.]
!           2. POLTDV(3,2)   -  THE CORRECTIONS TO THE J2000.0 GEOCENTRIC
!                               SITE VELOCITY VECTORS DUE TO POLE TIDAL
!                               EFFECTS AT EACH SITE. (M/SEC) [IN SOME CASES
!                               THIS IS ZEROED OUT DEPENDING ON THE VALUE OF
!                               THE POLE TIDE CONTROL FLAG KPTDC.]
!
! 4.2.2 COMMON BLOCKS USED -
!
      Real*8  ZPLTDP(3,2), ZPLTDV(3,2), ZPLDPX(3,2), ZPLDVX(3,2),       &
     &        ZPLDPY(3,2), ZPLDVY(3,2), X_mean, Y_mean, DPTDP(2,2)
      COMMON / PTDCM / ZPLTDP, ZPLTDV, ZPLDPX, ZPLDVX, ZPLDPY, ZPLDVY,  &
     &                 X_mean, Y_mean, DPTDP
!            VARIABLES FROM:
!             1.  ZPLTDP(3,2) - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                               POSITION VECTORS DUE TO POLE TIDAL EFFECTS AT
!                               EACH SITE. (M)
!             2.  ZPLTDV(3,2) - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                               VELOCITY VECTORS DUE TO POLE TIDAL EFFECTS AT
!                               EACH SITE. (M/SEC)
!             3.  ZPLDPX(3,2) - The partial derivative of the ZPLTDP with
!                               respective to the X-wobble offset. (m/arcsec)
!             4.  ZPLDPY(3,2) - The partial derivative of the ZPLTDP with
!                               respective to the Y-wobble offset. (m/arcsec)
!             5.  ZPLDVX(3,2) - The partial derivative of the ZPLTDV with
!                               respective to the X-wobble offset.
!                               (m/sec/arcsec)
!             6.  ZPLDVY(3,2) - The partial derivative of the ZPLTDV with
!                               respective to the Y-wobble offset.
!                               (m/sec/arcsec)
!             7.  X_mean, Y_mean - The current mean secular offsets of the
!                               pole, using linear approximations in IERS
!                               Conventions (2003).
!
      Real*8            PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH /  PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!             VARIABLES 'FROM':
!               1. HALFPI -  PI/2.D0 (UNITLESS)
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1.  KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
!              2.  KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'cobsn11.i'
!          Variables from:
!            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
!                         then used downstream. Otherwise equals zero. For
!                         correlator usage.
!
! 4.2.3 PROGRAM SPECIFICATIONS -
      Real*8 TCDISP(3,2), CFDISP(3,2), COLAT(2), SITLAT(2), SITLON(2),  &
     &       SITRAD(2), TCTOCF(3,3,2), R2K(3,3,3), POLTDP(3,2),         &
     &       POLTDV(3,2), WOBXR, WOBYR, CENT,                           &
     &       TCDSPX(3,2), TCDSPY(3,2), Tyr, WOBXds, WOBYds
      Integer*4 L, I
!
! 4.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: DCOS, DSIN, VECRT
!
! 4.2.7 CONSTANTS USED - XLOVEH, XLOVEL, WOBXR, WOBYR
!
! 4.2.8 PROGRAM VARIABLES -
!       1. TCDISP(3,2) - THE TOPOCENTRIC EARTH CRUSTAL DISPLACEMENTS AT EACH
!                        SITE (M)
!       2. CFDISP(3,2) - THE CRUST FIXED GEOCENTRIC EARTH CRUSTAL DISPLACEMENTS
!                        AT EACH SITE.  (M)
!       3. COLAT(2)    - COLATITUDE AT EACH SITE (RAD)
!
! 4.2.9 PROGRAMMER - TOM HERRING   07/01/84
!                    DAVID GORDON  01/03/85 (ADDED FLAG 2 CODING)
!                    Jim Ryan      89.07.08 Documentation simplified.
!                    David Gordon  11/24/93 Local site gravity computation
!                                  copied from subroutine ETDG
!                    David Gordon 94.04.15 Changed to Implicit None
!                    David Gordon 94.04.27 Common block ETDCM changed (from
!                                 Earth tide module).
!                    David Gordon 98.06.24 Changed common block ETDCM to match
!                                 Earth tide module.
!                    David Gordon 98.08.04 Changed computations to match
!                                 1996 IERS Conventions; ETDCM not needed.
!                                 Put in mods for Geocenter site (pole tide
!                                 zeroed out).
!                    David Gordon 98.11.24 Added computations of the partial
!                                 derivatives of the pole tide displacement
!                                 w.r.t. X-pole and Y-pole. Partials added to
!                                 common block PTDCM.
!                    David Gordon 98.12.17 Added variable CENT and computations
!                                 for a mean pole X and Y offsets and their
!                                 removal from the pole tide correction.
!                    David Gordon 99.01.19 Added X_mean, Y_mean, and DPTDP to
!                                 Common /PTDCM/.
!                    David Gordon 2001.05.02 Corrected debug output code.
!                    Jim Ryan 2002.09 Interger*4 mods.
!                    David Gordon 2004.03.26 Update for IERS Conventions
!                                 (2003). Mean secular pole offsets revised.
!                    David Gordon Nov. 2012. Modified mean offsets and pole
!                                 tide computations for IERS Conventions 2010.
!
!     PTDG PROGRAM STRUCTURE
!
!     Compute the secular mean values of the X and Y pole positions
!      Year and fraction thereof from 2000.0
        Tyr = CENT*100.D0
!     Mean secular offsets (arc-seconds) [2003 Conventions]
!         X_mean = ( 0.054  + 0.00083*Tyr)
!         Y_mean = (-0.357  - 0.00395*Tyr)
!       Write(6,'("Old: Tyr,X_mean,Y_mean ",3F10.5)') Tyr,X_mean,Y_mean
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Mean secular offsets (arc-seconds) [2010 Conventions]
        If (Tyr .le. 10.D0) Then
           X_mean = (55.974D-3 + 1.8243D-3*Tyr + .18413D-3*Tyr**2       &
     &            + .007024D-3*Tyr**3)
           Y_mean = -(346.346D-3 + 1.7896D-3*Tyr - .10729D-3*Tyr**2     &
     &            - .000908D-3*Tyr**3)
        Else
           X_mean = (23.513D-3 + 7.6141D-3*Tyr)
           Y_mean = -(358.891D-3 - 0.6287D-3*Tyr)
        Endif
!       Write(6,'("New: Tyr,X_mean,Y_mean ",3F10.5)') Tyr,X_mean,Y_mean
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     De-secularized X and Y (radians)
          WOBXds = WOBXR - X_mean*CONVDS
          WOBYds = WOBYR - Y_mean*CONVDS
!
!     The pole tide geometry for sites 1 and 2 are calculated
!      separately by running through a loop twice.
      DO 1200  L = 1,2
!
!      Check for Geocenter site:
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
!
!       Compute the colatitude of the site
        COLAT(L) = HALFPI - SITLAT(L)
!
!   Compute the IERS Conventions (2010) topocentric displacements with
!    mean offsets removed.
!         Up
        TCDISP(1,L) = -33.D0 * DSIN( 2.D0*COLAT(L) ) * 1.D-3 *          &
     &              ( WOBXds*DCOS(SITLON(L)) + WOBYds*DSIN(SITLON(L)) ) &
     &              / CONVDS
!         East
        TCDISP(2,L) =   9.D0 * DCOS( COLAT(L) ) * 1.D-3 *               &
     &              ( WOBXds*DSIN(SITLON(L)) - WOBYds*DCOS(SITLON(L)) ) &
     &              / CONVDS
!         North
        TCDISP(3,L) =  9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *           &
     &              ( WOBXds*DCOS(SITLON(L)) + WOBYds*DSIN(SITLON(L)) ) &
     &              / CONVDS
!
!*       write(6,*), ' PTDG: WOBXds, WOBYds ', WOBXds, WOBYds
!*       write(6,*), ' PTDG: Up,East,North, ', TCDISP(1,L),TCDISP(2,L), &
!*   &               TCDISP(3,L)
!
!    Rotate the displacements to the crust fixed geocentric system.
        CALL VECRT ( TCTOCF(1,1,L), TCDISP(1,L), CFDISP(1,L) )
!    Rotate the crust fixed geocentric displacements to the J2000.0 system.
        CALL VECRT ( R2K(1,1,1), CFDISP(1,L), ZPLTDP(1,L) )
!    Compute the contribution to the J2000.0 velocities due to the
!    effect of the rotation of the earth.
        CALL VECRT ( R2K(1,1,2), CFDISP(1,L), ZPLTDV(1,L) )
!
!  Compute the partial derivatives of the displacements w.r.t. X and Y
!   Recall that the sign of Y-pole was reversed a while back. Units will
!   be meters/arc-sec.
!         Up
        TCDSPX(1,L) = -33.D0 * DSIN(2.D0*COLAT(L)) * 1.D-3 *            &
     &                 DCOS(SITLON(L))
        TCDSPY(1,L) = -33.D0 * DSIN(2.D0*COLAT(L)) * 1.D-3 *            &
     &                 DSIN(SITLON(L))
!         East
        TCDSPX(2,L) = 9.D0 * DCOS(COLAT(L)) * 1.D-3 * DSIN(SITLON(L))
        TCDSPY(2,L) = 9.D0 * DCOS(COLAT(L)) * 1.D-3 * (-DCOS(SITLON(L)))
!
!         North
        TCDSPX(3,L) = 9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *            &
     &                 DCOS(SITLON(L))
        TCDSPY(3,L) = 9.D0 * DCOS( 2.D0*COLAT(L) ) * 1.D-3 *            &
     &                 DSIN(SITLON(L))
!
!    Rotate to J2000 position and velocity offsets
        CALL VECRT ( TCTOCF(1,1,L),TCDSPX(1,L), CFDISP(1,L) )
        CALL VECRT ( R2K(1,1,1),   CFDISP(1,L), ZPLDPX(1,L) )
        CALL VECRT ( R2K(1,1,2),   CFDISP(1,L), ZPLDVX(1,L) )
!
        CALL VECRT ( TCTOCF(1,1,L),TCDSPY(1,L), CFDISP(1,L) )
        CALL VECRT ( R2K(1,1,1),   CFDISP(1,L), ZPLDPY(1,L) )
        CALL VECRT ( R2K(1,1,2),   CFDISP(1,L), ZPLDVY(1,L) )
!
!     Close the loop over the sites.
!
 1200 CONTINUE
!
!     Check KPTDC to determine if the pole tide module is to be turned off
!     and the contribution zeroed out.
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
!
  300 CONTINUE
!
!     Handle the normal case where the pole tide effect is to be applied
!     to the theoretical and is stored as a contribution.
      IF ( KPTDC .NE. 0 )  GO TO 400
      DO 320  L = 1,2
        DO 310  I = 1,3
          POLTDP(I,L) = ZPLTDP(I,L)
          POLTDV(I,L) = ZPLTDV(I,L)
  310   CONTINUE
  320 CONTINUE
!
  400 CONTINUE
!
!  Handle the case where the pole tide is stored as a contribution
!   but not applied to the theoretical.
      IF ( KPTDC .NE. 2 )  GO TO 500
      DO 420  L = 1,2
        DO 410  I = 1,3
          POLTDP(I,L) = 0.D0
          POLTDV(I,L) = 0.D0
  410   CONTINUE
  420 CONTINUE
!
!     Check KPTDD for debug output.
  500 Continue
!
      IF ( KPTDD .EQ. 0 ) GO TO 600
!
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDG." )
!
      WRITE ( 6, 9200 ) TCDISP, CFDISP, POLTDP, POLTDV, SITLAT, &
     &    SITLON,  WOBXR, WOBYR, R2K, TCTOCF
!
 9200 FORMAT (1X, "TCDISP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "CFDISP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "POLTDP = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "POLTDV = ", 2 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "SITLAT = ", 2 (  D30.16, 10X ), /, 1X,               &
     &            "SITLON = ", 2 (  D30.16, 10X ), /, 1X,               &
     &            "WOBXR  = ", D30.16, /, 1X,                           &
     &            "WOBYR  = ", D30.16, /, 1X,                           &
     &            "R2K    = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),          &
     &            "TCTOCF = ", 6 ( 3 ( D30.16, 10X ), /, 1X ) )
!
      Do 777 L = 1,2
       write (6,'(" ZPLTDP ",3f22.10)') ZPLTDP(1,L), ZPLTDP(2,L),       &
     &                                 ZPLTDP(3,L)
       write (6,'(" ZPLTDV ",3f22.10)') ZPLTDV(1,L), ZPLTDV(2,L),       &
     &                                 ZPLTDV(3,L)
       write (6,'(" ZPLDPX ",3f22.10)') ZPLDPX(1,L), ZPLDPX(2,L),       &
     &                                 ZPLDPX(3,L)
       write (6,'(" ZPLDVX ",3f22.10)') ZPLDVX(1,L), ZPLDVX(2,L),       &
     &                                 ZPLDVX(3,L)
       write (6,'(" ZPLDPY ",3f22.10)') ZPLDPY(1,L), ZPLDPY(2,L),       &
     &                                 ZPLDPY(3,L)
       write (6,'(" ZPLDVY ",3f22.10)') ZPLDVY(1,L), ZPLDVY(2,L),       &
     &                                 ZPLDVY(3,L)
       write (6,'(" TCDSPX ",3f22.10)') TCDSPX(1,L), TCDSPX(2,L),       &
     &                                 TCDSPX(3,L)
       write (6,'(" TCDSPY ",3f22.10)') TCDSPY(1,L), TCDSPY(2,L),       &
     &                                 TCDSPY(3,L)
 777  Continue
!     Normal conclusion.
  600 RETURN
      END
!*****************************************************************
      SUBROUTINE PTDP(STAR)
      IMPLICIT None
!
! 5.    PTDP
!
! 5.1   PTDP PROGRAM SPECIFICATION
!
! 5.1.1 PTDP is the pole tide module partials section. It computes the partial
!       derivatives with respect to the X-wobble and Y-wobble (X, Y in arc
!       seconds)
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!            VARIABLES 'FROM':
!              1. VLIGHT  -  THE VELOCITY OF LIGHT IN VACUUM.  (M/SEC)
!
      Real*8  ZPLTDP(3,2), ZPLTDV(3,2), ZPLDPX(3,2), ZPLDVX(3,2),       &
     &        ZPLDPY(3,2), ZPLDVY(3,2), X_mean, Y_mean, DPTDP(2,2)
      COMMON / PTDCM / ZPLTDP, ZPLTDV, ZPLDPX, ZPLDVX, ZPLDPY, ZPLDVY,  &
     &                 X_mean, Y_mean, DPTDP
!            VARIABLES FROM:
!             1. ZPLTDP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                               POSITION VECTORS DUE TO POLE TIDAL EFFECTS AT
!                               EACH SITE. (M)
!             2. ZPLTDV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                               VELOCITY VECTORS DUE TO POLE TIDAL EFFECTS AT
!                               EACH SITE. (M/SEC)
!             3. ZPLDPX(3,2)  - The partial derivative of the ZPLTDP with
!                               respective to the X-wobble offset. (m/arcsec)
!             4. ZPLDPY(3,2)  - The partial derivative of the ZPLTDP with
!                               respective to the Y-wobble offset. (m/arcsec)
!             5. ZPLDVX(3,2)  - The partial derivative of the ZPLTDV with
!                               respective to the X-wobble offset.
!                               (m/sec/arcsec)
!             6. ZPLDVY(3,2)  - The partial derivative of the ZPLTDV with
!                               respective to the Y-wobble offset.
!                               (m/sec/arcsec)
!            VARIABLES TO:
!               1. DPTDP(2,2) - The pole tide partial derivatives of the delay
!                               the delay rate w.r.t. X-pole and Y-pole. First
!                               index runs over the delay partial w.r.t. X and
!                               Y, second runs over rate partial w.r.t X and Y.
!                               (seconds/arcsecond, seconds/sec/arcsec)
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
!              2. KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
!
! 5.2.3 PROGRAM SPECIFICATIONS -
      Real*8 STAR(3)
!
! 5.2.4 DATA BASE ACCESS => PUT ('PTDXYPAR') moved to PUT_P.
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVR
!             CALLED SUBROUTINES: DOTP, VECSB
!
! 5.2.7 CONSTANTS USED - VLIGHT
!
! 5.2.8 PROGRAM VARIABLES -
!          1. BASCRX(3,2) - The partial derivative of the J2000 baseline
!                           position and velocity vectors w.r.t to the X-pole
!                           offset. (m/arcsec, m/sec/arcsec)
!          2. BASCRY(3,2) - The partial derivative of the J2000 baseline
!                           position and velocity vectors w.r.t to the Y-pole
!                           offset. (m/arcsec, m/sec/arcsec)
!
! 5.2.9 PROGRAMMER -
!       David Gordon 11.24.98 Code added to compute delay and rate pole
!                    tide partials w.r.t. X-pole and Y-pole.
!       David Gordon 99.01.19 Added X_mean, Y_mean, and DPTDP to Common /PTDCM/
!                    for use in contribution section.
!       Jim Ryan 2002.09 Interger*4 mods.
!       David Gordon Jan. 2013  Moved PUT to subroutine PUT_P.
!
       Real*8 BASCRX(3,2), BASCRY(3,2), DOTP
!
!  Compute the partials.
!
!  Compute the partials w.r.t. X and Y of the J2000.0 baseline position and
!   velocity vectors due to pole tide effects
      CALL VECSB ( ZPLDPX(1,1), ZPLDPX(1,2), BASCRX(1,1) )
      CALL VECSB ( ZPLDVX(1,1), ZPLDVX(1,2), BASCRX(1,2) )
      CALL VECSB ( ZPLDPY(1,1), ZPLDPY(1,2), BASCRY(1,1) )
      CALL VECSB ( ZPLDVY(1,1), ZPLDVY(1,2), BASCRY(1,2) )
!
!  Complete the calculation of the partials.
        DPTDP(1,1) =  DOTP ( BASCRX(1,1), STAR ) / VLIGHT
        DPTDP(1,2) =  DOTP ( BASCRX(1,2), STAR ) / VLIGHT
        DPTDP(2,1) = -DOTP ( BASCRY(1,1), STAR ) / VLIGHT
        DPTDP(2,2) = -DOTP ( BASCRY(1,2), STAR ) / VLIGHT
!
!
!  Check KPTDD for debug output.
      IF ( KPTDD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDC ",/)
      WRITE(6,9200) ZPLDPX,ZPLDVX,BASCRX,ZPLDPY,ZPLDVY,BASCRY,STAR,DPTDP
 9200 FORMAT (1X, "ZPLDPX = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),           &
     &            "ZPLDVX = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),           &
     &            "BASCRX = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),           &
     &            "ZPLDVY = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),           &
     &            "ZPLDVY = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),           &
     &            "BASCRY = ", 2 ( 3 ( D25.16, 1X ), /, 1X ),           &
     &            "STAR   = ",     3 ( D25.16, 1X ), /, 1X,             &
     &            "DPTDP  = ",     4 ( D25.16, 1X ) )
!
!  Normal conclusion.
!
  500 RETURN
      END
!********************************************************************
      SUBROUTINE PTDC ( STAR )
      IMPLICIT None
!
! 6.    PTDC
!
! 6.1   PTDC PROGRAM SPECIFICATION
!
! 6.1.1 PTDC IS THE POLE TIDE MODULE CONTRIBUTION SECTION. IT COMPUTES
!       THE CONTRIBUTION TO THE DELAY AND THE DELAY RATE DUE TO POLE
!       TIDE EFFECTS.
!
! 6.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. STAR(3)  -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!
! 6.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!            VARIABLES 'FROM':
!              1. VLIGHT - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. DPTDC(2)   - THE POLE TIDE CONTRIBUTION TO THE DELAY AND TO
!                          THE DELAY RATE (SEC, SEC/SEC)
!          2. PTOLD(2)   - The contribution that will effectively convert
!                          the pole tides to the non-mean-offset-removed
!                          values, i.e. to the old (Calc 8.2 and earlier
!                          versions) values.
!
      Real*8  ZPLTDP(3,2), ZPLTDV(3,2), ZPLDPX(3,2), ZPLDVX(3,2),       &
     &        ZPLDPY(3,2), ZPLDVY(3,2), X_mean, Y_mean, DPTDP(2,2)
      COMMON / PTDCM / ZPLTDP, ZPLTDV, ZPLDPX, ZPLDVX, ZPLDPY, ZPLDVY,  &
     &                 X_mean, Y_mean, DPTDP
!            VARIABLES FROM:
!             1. ZPLTDP(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                               POSITION VECTORS DUE TO POLE TIDAL EFFECTS AT
!                               EACH SITE. (M)
!             2. ZPLTDV(3,2)  - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                               VELOCITY VECTORS DUE TO POLE TIDAL EFFECTS AT
!                               EACH SITE. (M/SEC)
!             3. ZPLDPX(3,2)  - The partial derivative of the ZPLTDP with
!                               respective to the X-wobble offset. (m/arcsec)
!             4. ZPLDPY(3,2)  - The partial derivative of the ZPLTDP with
!                               respective to the Y-wobble offset. (m/arcsec)
!             5. ZPLDVX(3,2)  - The partial derivative of the ZPLTDV with
!                               respective to the X-wobble offset.
!                               (m/sec/arcsec)
!             6. ZPLDVY(3,2)  - The partial derivative of the ZPLTDV with
!                               respective to the Y-wobble offset.
!                               (m/sec/arcsec)
!             7. X_mean, Y_mean-The current mean offsets of the pole, using
!                               Harald Schuh's formula.
!             8. DPTDP(2,2)   - The pole tide partial derivatives of the delay
!                               the delay rate w.r.t. X-pole and Y-pole. First
!                               index runs over the delay partial w.r.t. X and
!                               Y, second runs over rate partial w.r.t. X and Y.
!                               (seconds/arcsecond, seconds/sec/arcsec)
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
!              2. KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
!
! 6.2.3 PROGRAM SPECIFICATIONS -
!**   Real*8 BASCOR(3,2), DPTDC(2), STAR(3), DOTP, PTOLD(2), PTOFF(2)
      Real*8 BASCOR(3,2),           STAR(3), DOTP,           PTOFF(2)
      Integer*4 K
!
! 6.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVR
!             CALLED SUBROUTINES: DOTP, VECSB
!
! 6.2.7 CONSTANTS USED - VLIGHT
!
! 6.2.8 PROGRAM VARIABLES -
!       1. BASCOR(3,2) - THE CORRECTION TO THE J2000.0 BASELINE POSITION AND
!                        VELOCITY VECTORS DUE TO THE POLE TIDE. (M, M/S)
!
! 6.2.9 PROGRAMMER - TOM HERRING   07/01/84
!                    DAVID GORDON  08/24/84  (ADDED SOME DEBUG)
!                    DAVID GORDON  01/03/85  (ADDED ZPLTDP AND ZPLTDV)
!                    Jim Ryan      89.07.08 Documentation simplified.
!                    Jim Ryan      89:10:05 CPHYS common made an include file.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                             implimented.
!                    David Gordon 94.04.15 Changed to Implicit None
!                    D. Gordon 99.01.19 Added X_mean, Y_mean, and DPTDP to
!                             Common /PTDCM/. Added calculation and PUT for
!                             the contribution to put back in the mean pole
!                             tide correction, Lcode 'PTOLDCON'.
!                    Jim Ryan 2002.09 Interger*4 mods.
!                    David Gordon 2004.08.11 Mean secular offsets removed
!                                 from the default. Contribution to restore
!                                 them computed in PTDC.
!                    David Gordon Jan. 2013  Moved PUT's to subroutine PUT_C.
!
!     PTDC PROGRAM STRUCTURE
!
!     Compute the contributions.
!
!  Compute the corrections to the J2000.0 baseline position and
!   velocity vectors due to pole tidal effects.
      CALL VECSB ( ZPLTDP(1,1), ZPLTDP(1,2), BASCOR(1,1) )
      CALL VECSB ( ZPLTDV(1,1), ZPLTDV(1,2), BASCOR(1,2) )
!  Complete the calculation of the contributions.
      DO 120  K = 1,2
        DPTDC(K) = DOTP ( BASCOR(1,K), STAR ) / VLIGHT
  120 CONTINUE
!
!  Compute the delay and rate contributions to restore the secular
!   mean offsets that were removed in the geometry section. Need to
!   reverse the Y_mean sign.
        PTOLD(1) = X_mean*DPTDP(1,1) -  Y_mean*DPTDP(2,1)
        PTOLD(2) = X_mean*DPTDP(1,2) -  Y_mean*DPTDP(2,2)
!  Compute the delay and rate contributions needed to apply the
!   mean offsets computed in the geometry section.
!   Need to reverse the Y_mean sign.
!xx     PTOFF(1) = -(X_mean*DPTDP(1,1) -  Y_mean*DPTDP(2,1))
!xx     PTOFF(2) = -(X_mean*DPTDP(1,2) -  Y_mean*DPTDP(2,2))
!
!        print *,'PTDC: DPTDC(psec) ', DPTDC(1)*1.D12, DPTDC(2)*1.D12
!        print *,'PTDC: PTOFF(psec) ', PTOFF(1)*1.D12, PTOFF(2)*1.D12
!
!  Check KPTDD for debug output.
      IF ( KPTDD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDC ",/)
      WRITE ( 6, 9200 )  ZPLTDP, ZPLTDV, BASCOR, STAR, DPTDC
 9200 FORMAT (1X, "ZPLTDP = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),           &
     &            "ZPLTDV = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),           &
     &            "BASCOR = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),           &
     &            "STAR   = ",     3 ( D25.16, 5X ), /, 1X,             &
     &            "DPTDC  = ",     2 ( D25.16, 5X ), /, 1X,             &
     &            "PTOLD  = ",     2 ( D25.16, 5X ) )
!
!  Normal conclusion.
  500 RETURN
      END
!
!***************************************************************************
      SUBROUTINE OPTLG(WOBXds, WOBYds, OPTLcoef, R2K6, TCTOCF, TSKIP,   &
     &                OPTLOADP,OPTLOADV)
      Implicit None
!
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!
!          Variables "From":
!            1. PI     - THE MATHEMATICAL CONSTANT PI (UNITLESS)
!            2. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
!            3. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                        (RAD/ARCSECOND)
!
      INCLUDE 'cobsn11.i'
!          Variables from:
!            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
!                         then used downstream. Otherwise equals zero. For
!                         correlator usage.
!
!     Real*8 CT,Xpole,Ypole,Vsite,Vlon,Vlat,u_rR,u_rI,u_nR,        &
!    &       u_nI,u_eR,u_eI,U_up,U_north,U_east
      Real*8 U_up,U_north,U_east
      Real*8 GEE,  a_E, rho_w, Omega, gee_e, GM
      Real*8 H_p, Kay, Gamma2R,  Gamma2I, Xbar_p, Ybar_p, m1, m2, XT
      Real*8 WOBXds, WOBYds, OPTLcoef(6,2), R2K6(3,3,3), TCTOCF(3,3,2), &
     &       OPTLOADP(3,2),OPTLOADV(3,2)
      Real*8 TCoff(3,2), CFDISP(3,2)
      Integer*4 Ifirst, TSKIP, L
!     Save PI, CONVD , GEE,  a_E, rho_w, Omega, gee_e, GM,      &
!    &     H_p, Kay,  Gamma2R, Gamma2I 
      Save  H_p, Kay, Ifirst
!      
!     DATA  PI  /3.1415926535897932D0/,          &
!    &      CONVD  /1.7453292519943296D-02/,     &
!    &      CONVDS /4.8481368110953599D-06/,     &
      DATA  GEE    /6.673D-11/,                  &
     &      a_E    /6378136.6D0/,                &
     &      rho_w  /1025.0D0/,                   &
     &      Omega  /7.292115D-5/,                &
     &      gee_e  /9.7803278/,                  &
     &      GM     /3.986004418D14/,             &
     &      Gamma2R  /0.6870D0/                  &
     &      Gamma2I  /0.0036D0/                  &
     &      Ifirst /0/  
!
!      Modifications:
!       Nov/Dec 2012 D.Gordon 
!
        Ifirst = Ifirst+1 
!
       IF (Ifirst .eq. 1) Then 
        H_p = DSQRT(8.D0*PI/15.D0) * ( (Omega**2 * a_E**4) / GM ) 
        Kay = (4.D0 * PI * GEE * a_E * rho_w * H_p)/(3.D0*gee_e)
!         write(6,*), ' OCPT: H_p, Kay ',  H_p, Kay 
       Endif 

!
!      m1 =  (Xpole - Xbar_p) * CONVDS
!      m2 = -(Ypole - Ybar_p) * CONVDS
!        write(6,*), 'OPTLG: m1, m2 ',  m1, m2 
!
        m1 = WOBXds
        m2 = WOBYds 
!        write(6,*), 'OPTLG: m1, m2 ',  m1, m2 
!
!  Loop over both stations
      DO L = 1,2
!
!      Check for Geocenter site:
        IF (L .eq. Nzero) Then
          TCoff(1,L) = 0.0D0
          TCoff(2,L) = 0.0D0
          TCoff(3,L) = 0.0D0
         Go to 120
        ENDIF
!
!  Ocean pole tide offsets, in meters:
       U_up    = Kay * ( ((m1*Gamma2R + m2*Gamma2I) * OPTLcoef(1,L)) +  &
     &               ((m2*Gamma2R - m1*Gamma2I) * OPTLcoef(2,L))  )
       U_north = Kay * ( ((m1*Gamma2R + m2*Gamma2I) * OPTLcoef(3,L)) +  &
     &               ((m2*Gamma2R - m1*Gamma2I) * OPTLcoef(4,L))  )
       U_east  = Kay * ( ((m1*Gamma2R + m2*Gamma2I) * OPTLcoef(5,L)) +  &
     &               ((m2*Gamma2R - m1*Gamma2I) * OPTLcoef(6,L))  )
! 
!        write(6,*), '                     '
!        write(6,*), 'OPTLG: WOBXds, WOBYds(rad) ', WOBXds, WOBYds
!        write(6,*), 'OPTLG: Up,North,East(m)    ', U_up,U_north,U_east
!
!  Up, East, North
       TCoff(1,L) = U_up
       TCoff(2,L) = U_east
       TCoff(3,L) = U_north
!
 120  Continue
!
!    Rotate the displacements to the crust fixed geocentric system.
        CALL VECRT ( TCTOCF(1,1,L), TCoff (1,L), CFDISP(1,L) )
!    Rotate the crust fixed geocentric displacements to the J2000.0 system.
        CALL VECRT ( R2K6(1,1,1), CFDISP(1,L), OPTLOADP(1,L) )
!    Compute the contribution to the J2000.0 velocities due to the
!    effect of the rotation of the earth.
        CALL VECRT ( R2K6(1,1,2), CFDISP(1,L), OPTLOADV(1,L) )
!        write(6,*), ' OPTLG: OPTLOADP: ', OPTLOADP(1,L), OPTLOADP(2,L), &
!    &                                    OPTLOADP(3,L)
!        write(6,*), ' OPTLG: OPTLOADV: ', OPTLOADV(1,L), OPTLOADV(2,L), &
!    &                                    OPTLOADV(3,L)
!        write(6,*), '                     '
!
      ENDDO
!
      Return
      End
!
!********************************************************************
!
      SUBROUTINE OPTLC (OPTLOADP, OPTLOADV, STAR )
      IMPLICIT None
!
! 6.    OPTLC
!
! 6.1   OPTLC PROGRAM SPECIFICATION
!
! 6.1.1 OPTLC IS THE OCEAN POLE TIDE MODULE CONTRIBUTION SECTION. IT COMPUTES
!       THE CONTRIBUTION TO THE DELAY AND THE DELAY RATE DUE TO OCEAN POLE
!       TIDE EFFECTS.
!
! 6.2.1 CALLING SEQUENCE -
!         INPUT VARIABLES:
!           1. STAR(3)       - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!           2. OPTLOADP(3,2) - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                              POSITION VECTORS DUE TO OCEAN POLE TIDAL EFFECTS
!                              EACH SITE. (M)
!           3. OPTLOADV(3,2) - THE CORRECTIONS TO THE J2000.0 GEOCENTRIC SITE
!                              VELOCITY VECTORS DUE TO OCEAN POLE TIDAL EFFECTS
!                              EACH SITE. (M/sec)
!
! 6.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1.  KPTDC  -  THE POLE TIDE MODULE FLOW CONTROL FLAG.
!              2.  KPTDD  -  THE POLE TIDE MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'cphys11.i'
!            VARIABLES 'FROM':
!           1. VLIGHT - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. DOPTLC(2) - THE OCEAN POLE TIDE CONTRIBUTION TO THE DELAY AND TO
!                         THE DELAY RATE (SEC, SEC/SEC)
!
! 6.2.3 PROGRAM SPECIFICATIONS -
      Real*8 BASCOR(3,2), STAR(3), DOTP, OPTLOADP(3,2), OPTLOADV(3,2)
      Integer*4 L
!
! 6.2.4 DATA BASE ACCESS => Moved to PUT_C ('OPTLCONT'/DOPTLC).  
!
! 6.2.6 SUBROUTINE INTERFACE -
!         CALLER SUBROUTINES: DRIVR
!         CALLED SUBROUTINES: DOTP, VECSB
!
! 6.2.7 CONSTANTS USED - VLIGHT
!
! 6.2.8 PROGRAM VARIABLES -
!          1. BASCOR(3,2) - THE CORRECTION TO THE J2000.0 BASELINE POSITION AND
!                           VELOCITY VECTORS DUE TO THE OCEAN POLE TIDE. (M, M/S)
!
! 6.2.9 PROGRAMMER - David Gordon  December 2012
!
!
!     Compute the contributions.
!
!  Compute the corrections to the J2000.0 baseline position and
!   velocity vectors due to pole tidal effects.
      CALL VECSB ( OPTLOADP(1,1), OPTLOADP(1,2), BASCOR(1,1) )
      CALL VECSB ( OPTLOADV(1,1), OPTLOADV(1,2), BASCOR(1,2) )
!  Complete the calculation of the contributions.
      DO L = 1,2
        DOPTLC(L) = DOTP(BASCOR(1,L),STAR)/VLIGHT
      ENDDO 
!
!       print *,'OPTLC: DOPTLC(psec) ', DOPTLC(1)*1.D12,DOPTLC(2)*1.D12
!
!  Check KPTDD for debug output.
      IF ( KPTDD .EQ. 0 ) GO TO 500
      WRITE ( 6, 9100 )
 9100 FORMAT ( /,1X, "Debug output for subroutine PTDC ",/)
      WRITE ( 6, 9200 ) OPTLOADP,OPTLOADV,BASCOR, STAR, DOPTLC
 9200 FORMAT (1X, "OPTLOADP = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),           &
     &            "OPTLOADV = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),           &
     &            "BASCOR   = ", 2 ( 3 ( D25.16, 5X ), /, 1X ),           &
     &            "STAR     = ",     3 ( D25.16, 5X ), /, 1X,             &
     &            "DOPTLC   = ",     2 ( D25.16, 5X ), /, 1X)
!
!  Normal conclusion.
  500 RETURN
      END
