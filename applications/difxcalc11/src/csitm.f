      SUBROUTINE SITG (AXOFF, CFBASE, CFLAT, CFLON, CFSITE, CFSITN,     &
     &                 KAXIS, OCEAMP, OCEPHS,  SITLAT, SITLON, SITRAD,  &
     &                 TCTOCF, RTTOCF, ZPATH, SITHEIGHT, GEOLAT,        &
     &                 AXTILT, ROTAXIS, OPTLcoef)
      IMPLICIT None
!
! 4.    SITG
!
! 4.1   SITG PROGRAM SPECIFICATION
!
! 4.1.1 SITG is the Site Module geometry section. SITG calculates the site
!       geometry for the stations participating in the current observation.
!
! 4.1.2 RESTRICTIONS - NONE
!
! 4.1.3 REFERENCES - MARKHAM'S X-DOCUMENT
!
! 4.2   SITG PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!
!         OUTPUT VARIABLES:
!           1. AXOFF(2)      - THE ANTENNA AXIS OFFSETS AT EACH SITE. (M)
!           2. CFBASE(3)     - THE CRUST FIXED BASELINE VECTOR. (M)
!           3. CFLAT(3,2)    - THE PARTIAL DERIVATIVES OF THE SITE CRUST FIXED
!                              VECTOR COMPONENTS WITH RESPECT TO THE GEODETIC
!                              LATITUDES AT EACH OBSERVATION SITE. (M/RAD)
!           4. CFLON(3,2)    - THE PARTIAL DERIVATIVES OF THE SITE CRUST FIXED
!                              VECTOR COMPONENTS WITH RESPECT TO THE EAST
!                              LONGITUDES AT EACH OBSERVATION SITE.  (M/RAD)
!           5. CFSITE(3,2)   - THE CRUST FIXED SITE VECTORS AT EACH SITE. (M)
!           6. CFSITN(3,2)   - THE CRUST FIXED SITE NORMAL UNIT VECTORS AT
!                              EACH OBSERVATION SITE. (UNITLESS)
!           7. KAXIS(2)      - THE ANTENNA AXIS TYPES FOR EACH SITE. (UNITLESS)
!           8. OCEAMP(11,3,2)- THE TOPOCENTRIC OCEAN LOADING AMPLITUDES FOR
!                    ( J,K,L)  THE 11 MAIN TIDES (J=1,11),
!                                      K=1 : VERTICAL,
!                                      K=2 : EAST-WEST, AND
!                                      K=3 : NORTH-SOUTH DIRECTION
!                               FOR EACH OBSERVATION SITE (L=1,2). (M)
!           9. OCEPHS(11,3,2)- THE OCEAN LOADING PHASES AT EACH SITE. (RAD)
!          10. SITLAT(2)     - THE GEODETIC LATITUDE AT EACH SITE. (RAD)
!          11. SITLON(2)     - THE EAST LONGITUDE AT EACH SITE. (RAD)
!          12. SITRAD(2)     - THE SPHERICAL EARTH RADIUS OF EACH SITE. (M)
!          13. TCTOCF(3,3,2) - THE ROTATION MATRIX WHICH ROTATES THE
!                              TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST FIXED
!                              REFERENCE SYSTEM AT EACH SITE. (UNITLESS)
!          14. RTTOCF(3,3,2) - The rotation matrix which rotates the
!                              'radial-transverse' reference system to the
!                              crust fixed reference system at each site.
!          15. ZPATH(2)      - THE ZENITH ELECTRICAL PATH LENGTH AT EACH
!                              OBSERVATION SITE.  (SEC)
!          16. SITHEIGHT(2)  - The height above the geoid at each site. (m)
!          17. GEOLAT(2)     - The geocentric latitude at each site. (rad)
!          18. AXTILT(2,2)   - Antenna fixed axis tilts (arc-seconds).
!                              First index runs over the two orthogonal
!                              tilt directions (Alt-Az: 1 => East,
!                              2 => North; (X-Y (N-S or E-W fixed) and
!                              Equatorial: 1 => Az error, 2 => Elev error).
!                              Second index runs over the two stations.
!
      INCLUDE 'cmxst11.i'
!
      INCLUDE 'cobsn11.i'
!          Variables from:
!            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
!                         then used downstream. Otherwise equals zero. For
!                         correlator usage.
!
      INCLUDE 'ccon.i'
!          VARIABLES 'FROM':
!            1.  KSITC  -  THE SITE MODULE FLOW CONTROL FLAG.
!            2.  KSITD  -  THE SITE MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'get2s.i'
!       Variables from:
!          1. LNBASE(4,2) - THE EIGHT CHARACTER SITE NAMES OF THE BASELINE
!                           OF THE CURRENT OBSERVATION. (ALPHAMERIC)
!
! 4.2.3 PROGRAM SPECIFICATIONS -
      Real*8  AXOFF(2),CFBASE(3),CFLAT(3,2),CFLON(3,2),CFSITE(3,2),     &
     &        CFSITN(3,2),OCEAMP(11,3,2),OCEPHS(11,3,2),SITLAT(2),      &
     &        SITLON(2),SITRAD(2),TCTOCF(3,3,2),ZPATH(2),SITHEIGHT(2),  &
     &        RTTOCF(3,3,2), GEOLAT(2), AXTILT(2,2), ROTAXIS(3,3,2),    &
     &        OPTLcoef(6,2)
!     Integer*2  KAXIS(2), LNBASE(4,2), NDO(3), KERR
      Integer*2  KAXIS(2)
      Integer*4  I, J, K, L, N, NN, ix, jx
!
! 4.2.4 DATA BASE ACCESS - Moved to GET_G ('BASELINE').
!
! 4.2.6 SUBROUTINE INTERFACE -
!           CALLER SUBROUTINES: DRIVG
!           CALLED SUBROUTINES: TERMINATE_CALC, VECSB
!
! 4.2.7 CONSTANTS USED - NONE
!
! 4.2.8 PROGRAM VARIABLES -
!            3.  LOC...(4) - CONTAINS THE NAMES OF THE STATIONS WITH KNOWN
!                            PARAMETERS OF HORIZONTAL DISPLACEMENT DUE TO
!                            OCEAN LOADING.
!
! 4.2.9 PROGRAMMER - DALE MARKHAM    01/13/77
!                    PETER DENATALE  07/13/77
!                    CHOPO MA        08/06/81
!                    HAROLD M. SCHUH 10/08/83
!                    SAVITA GOEL 06/03/87 (CDS FOR A900)
!                    LOTHAR MOHLMANN 03/23/89
!                    89.07.20 Jim Ryan Documentation simplified.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                                      implimented.
!                    93.03.30 MSW SITCM common and maximum number of station
!                             variable put into newly created include file
!                             "cmxst.i".
!                    94.02.04 David Gordon SITHEIGHT(2) added, station heights
!                             above geoid (meters).
!                    David Gordon 94.04.16 Converted to Implicit None.
!                    David Gordon 98.06.26 Adding rotation matrix RTTOCF(3,3,2)
!                             and geocentric latitudes GEOLAT(2), for use later
!                             in the solid Earth tide module.
!                    David Gordon 98.07.29 Added 'Include cobsn.i' with
!                             variable Nzero, and code to determine when a
!                             station is at the geocenter.
!                    Jim Ryan 02.Sept Integer*2/4 Updates.
!                    D. Gordon 2004.05.18 Axis tilt code added.
!                    D. Gordon 2007.01.11 Fixed bug in NZERO logic.
!                    D. Gordon 2012 Nov. Added OPTLcoef(6,2), the 6 ocean
!                              pole tide loading coefficients for each site.
!
!     SITG program structure.
!
!     GET the baseline name. (Moved to GET_G, 'BASELINE'/LNBASE).
!
!   Set geocenter indicator to zero
      Nzero = 0
!
!     Construct the arrays to hold the geometry of the stations participating
!     in the current observation so that this information can be passed to the
!     rest of the program.
!
!     Loop for sites 1 and 2.
  310 DO 3130  L = 1,2
!
!       Determine the identification of the stations participating in
!       the current observation. If the baseline identification is not
!       successful, a message is written and the program terminates.
!
        DO 320  NN = 1, NUMSIT
          N = NN
          IF   ( ( LNSITE(1,NN) .EQ. LNBASE(1,L) )                      &
     &    .AND.  ( LNSITE(2,NN) .EQ. LNBASE(2,L) )                      &
     &    .AND.  ( LNSITE(3,NN) .EQ. LNBASE(3,L) )                      &
     &    .AND. ( LNSITE(4,NN) .EQ. LNBASE(4,L) ) )  GO TO 330
  320   CONTINUE
!
        GO TO 700
!
!       Check to see if the ID of the baseline has changed from that of the
!       previous observation. If not, then retain the current site geometry.
!
  330   CONTINUE
!!!!!!!!IF ( NLAST(L) .EQ. N )  GO TO 3130
        NLAST(L) = N
!
!       Construct the array to hold the crust fixed site vectors.
        CFSITE(1,L) = SITXYZ(1,N)
        CFSITE(2,L) = SITXYZ(2,N)
        CFSITE(3,L) = SITXYZ(3,N)
!
!       Construct the array to hold the site spherical radii.
        SITRAD(L) = CFRAD(N)
!
!       Construct the array to hold the site normal unit vectors.
        CFSITN(1,L) = SNRM(1,N)
        CFSITN(2,L) = SNRM(2,N)
        CFSITN(3,L) = SNRM(3,N)
!
!       Construct the arrays to hold the geodetic latitudes and the East
!       longitudes. Also now an array for geocentric latitude.
        SITLAT(L) = XLAT(N)
        SITLON(L) = XLON(N)
        SITHEIGHT(L) = HEIGHT(N)
        GEOLAT(L) = GLAT(N)
!
!       Construct arrays to hold the partial derivatives of the crust fixed site
!       coordinates with respect to the longitudes and the geodetic latitudes.
!
        CFLON(1,L) = PLON(1,N)
        CFLON(2,L) = PLON(2,N)
        CFLON(3,L) = PLON(3,N)
!
        CFLAT(1,L) = PLAT(1,N)
        CFLAT(2,L) = PLAT(2,N)
        CFLAT(3,L) = PLAT(3,N)
!
!       Construct the array to hold the site antenna axis offsets.
        AXOFF(L) = SITAXO(N)
!
!       Construct the array to hold the site antenna types.
        KAXIS(L) = KTYPE(N)
!
!       Construct the array to hold the site antenna axis tilts.
        AXTILT(1,L) = Dbtilt(1,N)
        AXTILT(2,L) = Dbtilt(2,N)
!
!       Construct arrays to hold the fixed axis rotation matrices
        Do ix = 1,3
         Do jx = 1,3
          ROTAXIS(ix,jx,L) = Rotilt(ix,jx,N)
         Enddo
        Enddo
!
!       Construct the array to hold the topocentric to crust fixed
!       rotation matrices. Now also an array for the radial-transverse
!       rotation matrices.
        DO 3112  J = 1,3
          DO 3111  I = 1,3
            TCTOCF(I,J,L) = TCROT(I,J,N)
            RTTOCF(I,J,L) = RTROT(I,J,N)
 3111     CONTINUE
 3112   CONTINUE
!
!       Construct the array to hold the zenith electrical path lengths
        ZPATH(L) = SITZEN(N)
!
!       Construct the arrays to hold the site ocean loading amplitudes
!       and phases.
        DO J = 1, 11
          OCEAMP(J,1,L) = SITOAM(J,N)
          OCEAMP(J,2,L) = SITHOA(J,1,N)
          OCEAMP(J,3,L) = SITHOA(J,2,N)
!
          OCEPHS(J,1,L) = SITOPH(J,N)
          OCEPHS(J,2,L) = SITHOP(J,1,N)
          OCEPHS(J,3,L) = SITHOP(J,2,N)
        ENDDO
!
!       Construct the array to hold the site ocean pole tide 
!        loading coefficients.
        Do J = 1,6
         OPTLcoef(J,L) = OPTL6(J,N)
        Enddo
!
!       WRITE (6,*)  '         ' 
!       WRITE (6,*)  ' SITG: Zero_site, N =  ', Zero_site, N 
!       WRITE (6,*)  '         ' 
!  Check for geocenter station
       If(Zero_site .ne. 0 .and. N .eq. Zero_site) Then
!       Nzero = N
        Nzero = L
!       WRITE (6,*)  ' SITG: Geocenter Site Found, Nzero =  ', Nzero
       Endif
!
!     Close the loop which runs over the sites.
 3130 CONTINUE
!
!     Construct the array to hold the crust fixed baseline vector.
      CALL VECSB ( CFSITE(1,1), CFSITE(1,2), CFBASE )
!
!     Check KSITD for debug output.
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
      WRITE(6,9)' SITHOA, ',(((SITHOA(J,L,K),J=1,11),L=1,2),K=1,        &
     &            NUMSIT)
      WRITE(6,9)' SITHOP, ',(((SITHOP(J,L,K),J=1,11),L=1,2),K=1,        &
     &            NUMSIT)
    9 FORMAT(/,A,11F7.4,/,(9X,11F7.4))
      WRITE(6,6)' SITXYZ  ',((SITXYZ(J,K),J=1,3),K=1,NUMSIT)
    6 FORMAT(/,A,3F20.4,/,(9X,3F20.4))
      WRITE(6,8)' SITZEN  ',(SITZEN(K),K=1,NUMSIT)
      WRITE(6,4)' SNRM    ',((SNRM(I,J),I=1,3),J=1,NUMSIT)
      WRITE(6,4)' TCROT   ',(((TCROT(I,J,K),I=1,3),J=1,3),K=1,          &
     &            NUMSIT)
    5 FORMAT(/,A,/,3(3F20.4,/)/)
      WRITE(6,8)' XLAT    ',(XLAT(J),J=1,NUMSIT)
      WRITE(6,8)' XLON    ',(XLON(J),J=1,NUMSIT)
      WRITE(6,8)' GLAT    ',(GLAT(J),J=1,NUMSIT)
      WRITE(6,8)' HEIGHT  ',(HEIGHT(J),J=1,NUMSIT)
!
      WRITE ( 6, 9200 )  AXOFF, CFBASE, CFLAT, CFLON, CFSITE,           &
     &           CFSITN, KAXIS, OCEAMP, OCEPHS, SITLAT, SITLON,         &
     &           SITRAD, TCTOCF, ZPATH,  LNBASE
 9200 FORMAT (1X, 'AXOFF  = ', 2 ( D30.16, 10X ), /, 1X,                &
     &            'CFBASE = ', 3 ( D30.16, 10X ), /, 1X,                &
     &            'CFLAT  = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),        &
     &            'CFLON  = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),        &
     &            'CFSITE = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),        &
     &            'CFSITN = ',/, 2 ( 3 ( D30.16, 10X ), /, 1X ),        &
     &            'KAXIS  = ',/, 2 ( I2, 10X ), /, 1X,                  &
     &            'OCEAMP = ',/,2( 3( 11F10.4,/ ),/),/,1X,              &
     &            'OCEPHS = ',/,2( 3( 11F10.4,/ ),/,1X ),/,1X,          &
     &            'SITLAT = ', 2 ( D30.16, 10X ), /, 1X,                &
     &            'SITLON = ', 2 ( D30.16, 10X ), /, 1X,                &
     &            'SITRAD = ', 2 ( D30.16, 10X ), /, 1X,                &
     &            'TCTOCF = ',/,6( 3 ( D30.16, 10X ), /, 1X ),          &
     &            'ZPATH  = ', 2 ( D30.16, 10X ), /, 1X,                &
     &            'LNBASE = ', 4A2,1X,4A2)
!
!     Normal conclusiton.
  600 RETURN
!
!     Abnormal conclusion.       .
  700 WRITE ( 6, 9300 )
 9300 FORMAT (1X, 'CALC has been terminated in subroutine SITG.  ',     &
     &            'The baseline identification was not successful.')
      CALL TERMINATE_CALC ( 'SITG  ', int2(0), int2(0))
      END
!*************************************************************************
      SUBROUTINE SITP (R2K, STAR, STAR12, EARTH, SITEV)
      IMPLICIT None
!
! 5.    SITP
!
! 5.1   SITP PROGRAM SPECIFICATION
!
! 5.1.1 SITP is the Site Module partial derivatives section. SITP
!       computes the partial derivatives of the delay and the rate with
!       respect to the site crust fixed vector components at each site.
!
! 5.1.2 RESTRICTIONS - NONE
!
! 5.1.3 REFERENCES - MARKHAM'S X-DOCUMENT
!
! 5.2   SITP PROGRAM INTERFACE
!
! 5.2.1 CALLING SEQUENCE -
!
!         INPUT VARIABLES:
!           1. R2K(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                           MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                           (UNITLESS, 1/SEC, 1/SEC**2)
!           2. STAR(3)    - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!          2.5 STAR12(3,2) - The J2000.0 source unit vectors from stations
!                           1 and 2. (unitless). For Far-field sources,
!                           these are the same as STAR(3)
!           3. EARTH(3,3) - The position, velocity, and acceleration of the
!                           Earth relative to the SSBC. (m, m/s, m/s**2)
!           4. SITEV(3,2) - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF EACH
!                           OBSERVATION SITE. (M/SEC)
!
!         OUTPUT VARIABLES: NONE
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!          VARIABLES 'FROM':
!            1.  VLIGHT  -  THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
!            2.  VLIGHT2 -  THE VELOCITY OF LIGHT SQUARED.
!
      INCLUDE 'ccon.i'
!          VARIABLES 'FROM':
!            1.  KSITC  -  THE SITE MODULE FLOW CONTROL FLAG.
!            2.  KSITD  -  THE SITE MODULE DEBUG OUTPUT FLAG.
!
     INCLUDE 'put2s.i'
!       Variables to:
!          1. DSITP(3,2,2) - THE PARTIAL DERIVATIVES OF THE DELAY AND THE
!                            DELAY RATE WITH RESPECT TO THE CRUST FIXED SITE
!                            COORDINATES AT EACH OBSERVATION SITE. THE FIRST
!                            INDEX RUNS OVER THE SITE COORDINATES, THE SECOND
!                            INDEX RUNS OVER THE SITES, AND THE THIRD RUNS
!                            OVER THE DELAY AND THE DELAY RATE.
!                            (SEC/M, SEC/SEC-M)
!
!
! 5.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8 R2K(3,3,3), STAR(3), EARTH(3,3), SITEV(3,2), DBDX1(3,2),   &
     &       DBDX2(3,2), DBDY1(3,2), DBDY2(3,2), DBDZ1(3,2),            &
     &       DBDZ2(3,2),               VG(3), VE(3), c1, c2, tt, DOTP,  &
     &       STAR12(3,2)
      Integer*4 I, K
!
! 5.2.4 DATA BASE ACCESS => Moved to PUT_P ('SIT PART'/DSITP). 
!
! 5.2.6 SUBROUTINE INTERFACE -
!           CALLER SUBROUTINES: DRIVP
!           CALLED SUBROUTINES: DOTP
!
! 5.2.7 CONSTANTS USED - VLIGHT
!
! 5.2.8 PROGRAM VARIABLES -
!       1.  DBDX1(3,2)  -  THE PARTIAL DERIVATIVES OF THE J2000.0 BASELINE
!                          POSITION AND VELOCITY VECTORS WITH RESPECT TO THE
!                          X-COMPONENT OF THE CRUST FIXED SITE VECTOR AT
!                          OBSERVATION SITE #1. (M/M, M/M-SEC)
!       2.  DBDX2(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
!                          X-COMPONENT AT SITE #2.  (M/M, M/M-SEC)
!       3.  DBDY1(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
!                          Y-COMPONENT AT SITE #1.  (M/M, M/M-SEC)
!       4.  DBDY2(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
!                          Y-COMPONENT AT SITE #2.  (M/M, M/M-SEC)
!       5.  DBDZ1(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
!                          Z-COMPONENT AT SITE #1.  (M/M, M/M-SEC)
!       6.  DBDZ2(3,2)  -  THE PARTIAL DERIVATIVES WITH RESPECT TO THE
!                          Z-COMPONENT AT SITE #2.  (M/M, M/M-SEC)
!       7.  VE(3)       -  A local copy of the velocity of the Earth
!                          relative to the SSBC.
!       8.  ci          -  1.d0/VLIGHT
!       9.  tt          -  A term common to all partials.
!
! 5.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
!                    PETER DENATALE 07/13/77
!                    CHOPO MA       08/06/81
!                    89.07.20 Jim Ryan Documentation simplified.
!                    Jim Ryan 89:10:05 CPHYS common made an include file.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                                   implimented.
!                    Jim Ryan 91.11.22 Next term from the delay and rate
!                                   theoreticals added to partials computation.
!                    David Gordon 94.04.16 Converted to Implicit None.
!                    David Gordon 98.10.15 Added SITEV to input arguments.
!                                   Changed site delay and rate partials
!                                   computations to use the Consensus formula.
!                                   Differences are very small and probably
!                                   not noticeable.
!                    Jim Ryan 02.Sept Integer*2/4 Updates.
!                    David Gordon Jan. 2013  Changed to use STAR12(3,2) for 
!                                   near-field case. Moved PUT into subroutine
!                                   PUT_P.
!
!     SITP program structure
!
!     Loop twice for delay and rate partials for both sites.
!      [Index K runs over the delays and rates.]
      DO 300  K = 1,2
!
!   Loop three times for the calculation of the partials with respect to the
!    crust fixed vector components.
        DO 140  I = 1,3
!
!    Compute the partial derivatives of the J2000.0 baseline position and
!    velocity vectors with respect to the crust fixed vector coordinates
!    at site #2.
          DBDX2(I,K) = R2K(I,1,K)
          DBDY2(I,K) = R2K(I,2,K)
          DBDZ2(I,K) = R2K(I,3,K)
!
!    Compute the partial derivatives of the J2000.0 baseline position and
!    velocity vectors with respect to the crust fixed site coordinates
!    at site #1.
          DBDX1(I,K) = - R2K(I,1,K)
          DBDY1(I,K) = - R2K(I,2,K)
          DBDZ1(I,K) = - R2K(I,3,K)
!
!    Close the loop running over the vector components.
  140   CONTINUE
!
!    Complete the calculation of the partial derivatives of the delay and the
!    rate with respect to the crust fixed site vector components at each site.
!
!    First make a local copy of the velocity of the Earth.
!       DO I =1,3
!         VG(I) = EARTH(I,2)
!       Enddo
!       c1 = 1.d0/VLIGHT
!       c2 = 1.d0/VLIGHT2
!       tt = 1.d0 - c1*DOTP(star,vg)
!
!       DSITP(1,1,K)=-c1*DOTP(DBDX1(1,K),STAR)*tt-c2*DOTP(DBDX1(1,K),VG)
!       DSITP(2,1,K)=-c1*DOTP(DBDY1(1,K),STAR)*tt-c2*DOTP(DBDY1(1,K),VG)
!       DSITP(3,1,K)=-c1*DOTP(DBDZ1(1,K),STAR)*tt-c2*DOTP(DBDZ1(1,K),VG)
!       DSITP(1,2,K)=-c1*DOTP(DBDX2(1,K),STAR)*tt-c2*DOTP(DBDX2(1,K),VG)
!       DSITP(2,2,K)=-c1*DOTP(DBDY2(1,K),STAR)*tt-c2*DOTP(DBDY2(1,K),VG)
!       DSITP(3,2,K)=-c1*DOTP(DBDZ2(1,K),STAR)*tt-c2*DOTP(DBDZ2(1,K),VG)
!     WRITE(6,8)' Old DSITP ', DSITP(1,1,K), DSITP(2,1,K),DSITP(3,1,K)
!     WRITE(6,8)' Old DSITP ', DSITP(1,2,K), DSITP(2,2,K),DSITP(3,2,K)
!
! Change to use the Consensus model definition
        DO I =1,3
          VG(I) = EARTH(I,2) + SITEV(I,2)
          VE(I) = EARTH(I,2)
        Enddo
         tt = 1.d0 + DOTP(STAR,VG)/VLIGHT
!
!*      DSITP(1,1,K) = -DOTP(DBDX1(1,K),STAR)/VLIGHT/tt &
!*   &                 - DOTP(DBDX1(1,K),VE)/VLIGHT2
!*      DSITP(2,1,K) = -DOTP(DBDY1(1,K),STAR)/VLIGHT/tt &
!*   &                 - DOTP(DBDY1(1,K),VE)/VLIGHT2
!*      DSITP(3,1,K) = -DOTP(DBDZ1(1,K),STAR)/VLIGHT/tt &
!*   &                 - DOTP(DBDZ1(1,K),VE)/VLIGHT2
!*      DSITP(1,2,K) = -DOTP(DBDX2(1,K),STAR)/VLIGHT/tt &
!*   &                 - DOTP(DBDX2(1,K),VE)/VLIGHT2
!*      DSITP(2,2,K) = -DOTP(DBDY2(1,K),STAR)/VLIGHT/tt &
!*   &                 - DOTP(DBDY2(1,K),VE)/VLIGHT2
!*      DSITP(3,2,K) = -DOTP(DBDZ2(1,K),STAR)/VLIGHT/tt &
!*   &                 - DOTP(DBDZ2(1,K),VE)/VLIGHT2
!
!! Modified tio use STAR12(3,2) for near-field computations
!!       X,Y,Z for station 1:
        DSITP(1,1,K) = -DOTP(DBDX1(1,K),STAR12(1,1))/VLIGHT/tt          &
     &                 - DOTP(DBDX1(1,K),VE)/VLIGHT2
        DSITP(2,1,K) = -DOTP(DBDY1(1,K),STAR12(1,1))/VLIGHT/tt          &
     &                 - DOTP(DBDY1(1,K),VE)/VLIGHT2
        DSITP(3,1,K) = -DOTP(DBDZ1(1,K),STAR12(1,1))/VLIGHT/tt          &
     &                 - DOTP(DBDZ1(1,K),VE)/VLIGHT2
!!       X,Y,Z for station 2:
        DSITP(1,2,K) = -DOTP(DBDX2(1,K),STAR12(1,2))/VLIGHT/tt          &
     &                 - DOTP(DBDX2(1,K),VE)/VLIGHT2
        DSITP(2,2,K) = -DOTP(DBDY2(1,K),STAR12(1,2))/VLIGHT/tt          &
     &                 - DOTP(DBDY2(1,K),VE)/VLIGHT2
        DSITP(3,2,K) = -DOTP(DBDZ2(1,K),STAR12(1,2))/VLIGHT/tt          &
     &                 - DOTP(DBDZ2(1,K),VE)/VLIGHT2
!
!    Close the loop which runs over the partials of the delay and rate.
  300 CONTINUE
!
!    PUT the site module partials. => Moved to PUT_P.
!
!    Check KSITD for debug output.
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
      WRITE ( 6, 9200 )  R2K, STAR
 9200 FORMAT (1X, 'R2K =  ', 9 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'STAR = ',     3 ( D30.16, 10X ) )
!
!     Normal conclusion.
  700 RETURN
      END
!*************************************************************************
      SUBROUTINE bkplh(XYZ,PLH,PI,A,FL)
      IMPLICIT NONE
!
! NAME             bkplh.f
!
! VERSION          93.01.27
!
! WRITTEN          B. Archinal, USNO, July 20-23, 1990.
!                  Name changed from "borkow" to "bkplh", and arguments
!                  adapted for efficient use by Calc (csitm), Dbcal,
!                  and Solve.  BA, 93.01.27.
!
! PURPOSE          Converts XYZ coordinates to Phi, Lambda, H
!                  ellipsoidal coordinates.
!
! References       Borkowski, K. M. (1989).  "Accurate Algorithms to
!                  transform geocentric to geodetic coordinates"
!                  *Bulletin Geodesique*, v. 63, pp. 50-56.  Also see
!                  Borkowski, K. M. (1987).  "Transformation of
!                  Geocentric to Geodetic Coordinates without
!                  Approximations", *Astrophysics and Space Science*,
!                  v. 139, n. 1, pp. 1-4.  Correction in (1988), v. 146,
!                  n. 1, p. 201.
!
! Note             Will not work for points on the Z axis, i.e. if
!                  if X=Y=0 (Phi = +/- 90 degrees).
!
! Calling sequence CALL bkplh ( XYZ, PLH, PI, A, FL )
!
! ARGUMENT LIST
!
!  PARM       TYPE DESCRIPTION
!
!  XYZ(3)     D    INPUT - XYZ Cartesian coordinates of point.
!                  XYZ(1) and XYZ(2) must not both be zero.  Units are
!                  those of A below.
!  PLH(3)     D    OUTPUT - Ellipsoidal coordinates of point, in
!                  geodetic latitude, longitude, and height.  Units
!                  for latitude and longitude are in radians, units
!                  of height are those of A below.
!  PI         D    INPUT - Ratio of circumference to diameter of circle.
!                  Unitless.
!  A          D    INPUT - Semi-major axis of ellipsoid.  Units are
!                  of distance (meters, kilometers, miles, etc.).
!  FL         D    INPUT - Flattening of ellipsoid.  Unitless.
!
!
! SUBPROGRAMS USED
!  Fortran         DABS      DACOS     DATAN     DATAN2   DCOS
!                  DSIN      DSQRT
!
! COMMON BLOCKS    None.
!
! INPUT            None.
!
! OUTPUT           None, unless diagnostic printout uncommented.
!
! LANGUAGE         Fortran 77. --- Now Fortran 95! 
!
!===================================================================
!
      Real*8 A,B,D,DABS,DACOS,DATAN,DATAN2,DCOS,DSIN, &
     &       DSQRT,E,F,FL,G,P,PI,Q,R,T,V,X,Y,Z,ZLONG
      Real*8 XYZ(3),PLH(3)
!     INTEGER IOUT
!
!--- XYZ.
      X=XYZ(1)
      Y=XYZ(2)
      Z=XYZ(3)
!--- Semi-minor axis.
      B=A*(1.D0-FL)
!--- Set sign of B to that of Z in order to get sign of Phi correct.
      IF(Z.LT.0.D0) B=-B
!--- Intermediate Values for Latitude.
      R=DSQRT(X*X+Y*Y)
      E=(B*Z-(A*A-B*B))/(A*R)
      F=(B*Z+(A*A-B*B))/(A*R)
      P=4.D0/3.D0 * (E*F+1)
      Q=2.D0 * (E*E - F*F)
      D=P*P*P+Q*Q
      IF(D.GE.0.D0) then
        V=(DSQRT(D)-Q)**(1.D0/3.D0) - (DSQRT(D)+Q)**(1.D0/3.D0)
        else
        V=2.D0 * DSQRT(-P) * DCOS (1.D0/3.D0 * &
     &  DACOS(Q/(P * DSQRT(-P))))
        endif
!   (Improve V - not really necessary except near axes.)
      IF(V*V.LT.DABS(P)) V=-(V*V*V + 2.D0*Q)/(3.D0*P)
      G=(DSQRT(E*E+V)+E)/2.D0
      T=DSQRT( G*G  + (F-V*G)/(2.D0*G-E) ) - G
      PLH(1)=DATAN( (A*(1.D0-T*T))/(2.D0*B*T) )
!--- HEIGHT.
      PLH(3)=(R-A*T)*DCOS(PLH(1)) + (Z-B)*DSIN(PLH(1))
!--- LONGITUDE.
      ZLONG=DATAN2(Y,X)
      IF(ZLONG.LT.0.D0) ZLONG=ZLONG+2.D0*PI
      PLH(2)=ZLONG
!
!   Diagnostic output.
!
!     IOUT=11
!     WRITE(IOUT,901) A,F,B
! 901 FORMAT(' A,F,B:',3D25.16)
!     WRITE(IOUT,902) X, Y, Z
! 902 FORMAT(' X, Y, Z:',3D25.16)
!     WRITE(IOUT,903) R,E,F
! 903 FORMAT(' R, E, F:',3D25.16)
!     WRITE(IOUT,904) P,Q,D
! 904 FORMAT(' P, Q, D:',3D25.16)
!     WRITE(IOUT,905) V,G,T
! 905 FORMAT(' V, G, T:',3D25.16)
!--- Check.
!     CHK1=T*T*T*T + 2.D0 * E *T*T*T + 2.D0 * F *T - 1.D0
!     CHK2=V*V*V + 3.D0*P*V + 2.D0*Q
!     WRITE(IOUT,906) CHK1,CHK2
! 906 FORMAT('Check values (=0):',2D25.16)
      RETURN
      END
