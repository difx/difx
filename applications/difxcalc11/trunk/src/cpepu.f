!*************************************************************************
      SUBROUTINE PEP ( XJD, ET, TSKIP, EARTH, SUN, XMOON )
      IMPLICIT None
!
! 4.    PEP
!
! 4.1   PEP PROGRAM SPECIFICATION
!
! 4.1.1 PEP is the utility which obtains the solar system geometry
!       from the JPL DE405/LE405 ephemeris. Positions and velocities
!       are obtained for the Sun, Moon, and all planets except Pluto.
!       Acceleration is also obtained for the Earth.
!
!       Users Note: The JPL DE405 ephemeris used here was created by
!       merging the two Unix binary ephemeris files for 1950-2000 and
!       2000-2050, obtained from the JPL site. We use the standard JPL
!       subroutines
!
!       PLEPH, STATE, INTERP, and SPLIT, with a few modifications, but all
!       unchanged from Calc 9.
!
!       October 2012: Now using the JPL DE421 ephemeris, little Endian
!       version.
!
! 4.1.2 RESTRICTIONS - None
!
! 4.1.3 REFERENCES - JPL anonymous FTP site
!                    ftp://ssd.jpl.nasa.gov/pub/eph/planets/ascii/421
!
! 4.2   PEP PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1.  XJD   - The Julian date at zero hours UTC of the date in
!                         question. (days)
!             2.  CT    - The coordinate time fraction of the coordinate time
!                         day (days).
!             3.  ET    - Ephemeris time (days). Using TDB at the geocenter.
!             4.  TSKIP - Flag telling whether to compute new values or use
!                         previous values. If TSKIP = 0 => new time, compute
!                         new values. If TSKIP = 1 => same time as previous
!                         observation, re-use previous values.
!
!           OUTPUT VARIABLES:
!             1. EARTH(3,3) - THE J2000.0 BARYCENTRIC EARTH POSITION, VELOCITY,
!                             AND ACCELERATION VECTORS. (M, M/SEC, M/SEC**2)
!                             (THE FIRST INDEX RUNS OVER THE VECTOR
!                             COMPONENTS, THE SECOND RUNS OVER THE POSITION,
!                             VELOCITY, AND ACCELERATION RESPECTIVELY.)
!             2. SUN(3,2)   - THE J2000.0 GEOCENTRIC SUN POSITION AND VELOCITY
!                             VECTORS. (M, M/SEC)
!             3. XMOON(3,2) - THE J2000.0 GEOCENTRIC MOON POSITION AND VELOCITY
!                             VECTORS. (M, M/SEC)
!
! 4.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'csolsys11.i'
!         VARIABLES 'TO':
!           1. SPLANET(3,2,7) - The J2000.0 Solar System Barycentric positions
!                               and velocities of all planets except the Earth
!                               and Pluto. (meters, meters/sec) The first index
!                               runs over X, Y, and Z, the second runs over
!                               position and velocity, and the third runs over
!                               the planets, where
!                                      1 = Mercury
!                                      2 = Venus
!                                      3 = Mars
!                                      4 = Jupiter
!                                      5 = Saturn
!                                      6 = Uranus
!                                      7 = Neptune
!           2. GPLANET(3,2,7) - The J2000.0 Geocentric positions and velocities
!                               of all planets except the Earth and Pluto.
!                               (meters, meters/sec) The first index runs over
!                               X, Y, and Z, the second runs over position and
!                               velocity, and the third runs over the planets,
!                               where
!                                      1 = Mercury
!                                      2 = Venus
!                                      3 = Mars
!                                      4 = Jupiter
!                                      5 = Saturn
!                                      6 = Uranus
!                                      7 = Neptune
!          3. SUNb(3,2)       - Barycentric Sun position and velocity.
!          4. MOONb(3,2)      - Barycentric Moon position and velocity.
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KPEPD - THE PEP UTILITY ROUTINE DEBUG OUTPUT FLAG.
!              2. KPEPC - The PEP module flow control flag (No options).
!
      INCLUDE 'put2s.i'
!       Variables to:
!         1. EARTH1(3,3) - THE J2000.0 BARYCENTRIC EARTH POSITION, VELOCITY,
!                          AND ACCELERATION VECTORS. (M, M/SEC, M/SEC**2)
!                          (THE FIRST INDEX RUNS OVER THE VECTOR
!                          COMPONENTS, THE SECOND RUNS OVER THE POSITION,
!                          VELOCITY, AND ACCELERATION RESPECTIVELY.)
!         2. SUN1(3,2)   - THE J2000.0 GEOCENTRIC SUN POSITION AND VELOCITY
!                          VECTORS. (M, M/SEC)
!         3. XMOON1(3,2) - THE J2000.0 GEOCENTRIC MOON POSITION AND VELOCITY
!                          VECTORS. (M, M/SEC)
!
!
! 4.2.3 PROGRAM SPECIFICATIONS -
!
      Real*8  EARTH(3,3),SUN(3,2),XMOON(3,2)
      INTEGER*4 TSKIP, TARG, CENT, IPV, IERR, ITARG, I, K
      Real*8 XJD, TJD, CT, OBSDIF, ET
      Real*8  POS(3), VEL(3), RRD(6)
      Equivalence ( RRD(1), POS(1) )
      Equivalence ( RRD(4), VEL(1) )
      Real*8 tplus1, tminus1, rplus1(6), rminus1(6)
!
!     Real*8 unit_planet(3)
!     Real*8 S_planet, G_planet, x, y, z, DEC, DEC_min, DEC_sec, theta, &
!    &       RA, RA_hrs, RA_min, RA_sec, Vecmg
!     Integer*4 K_deg, K_min, I_hrs, I_min
!
! 4.2.4 DATA BASE ACCESS - None
!
! 4.2.5 EXTERNAL INPUT/OUTPUT - Possible debug and error output
!
! 4.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: PLEPH
!
! 4.2.7 CONSTANTS USED - NONE
!
! 4.2.8 PROGRAM VARIABLES -
!          1. Targ    - Integer number of the 'target' object in subroutine
!                       PLEPH (see PLEPH).
!          2. Cent    - Integer number of the reference point in subroutine
!                       PLEPH (see PLEPH). Here we use cent=12 ==> SSBC.
!          3. Pos(3)  - J2000 position of 'targ' with respect to 'cent', as
!                       returned by subroutine PLEPH. (km)
!          4. Vel(3)  - J2000 velocity of 'targ' with respect to 'cent', as
!                       returned by subroutine PLEPH. (km/sec)
!          5. TJD     - Time of the current observation (Julian days).
!
! 4.2.9 PROGRAMMER - KATHY WATTS    04/25/77
!                    PETER DENATALE 07/19/77
!                    BRUCE SCHUPLER 11/17/77
!                    BRUCE SCHUPLER 01/08/80
!                    CHOPO MA       08/04/81
!                    Jim Ryan 89.07.25 Documentation simplified.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                    implimented.
!                    David Gordon, Brent Archinal  Oct/Nov 1993 - Use of JPL
!                                  DE/LE200 ephemeris added for Sun, Moon, and
!                                  all planets except Pluto. New common block
!                                  /SOLSYS/ added to hold planetary information.
!                    David Gordon 94.01.07 Code added to compare time with
!                                 time of previous scan and skip solar system
!                                 ephemeris calculations if they are the same.
!                    David Gordon 94.04.18 Converted to Implicit None.
!                    David Gordon 94.06.07 Documentation expanded.
!                    David Gordon/Michael Kesteven 94.06.07 Mods for SUN and
!                                 DEC ALPHA compatibility: rrd(12) put in call
!                                 to PLEPH.
!                    David Gordon 98.02.05 DPSI and DEPS (database nutation)
!                                 removed. Computation repeat/no repeat logic
!                                 moved to DRIVG. Options to get Sun, Moon,
!                                 Earth from data base removed.
!                    David Gordon 98.07.13 Converted to read JPL DE/LE403
!                                 Ephemeris for compliance with the IERS 1996
!                                 Conventions. Old code would not work so
!                                 now using the standard JPL ephemeris
!                                 subroutines, with slight modifications.
!                                 Earth's acceleration obtained by doing a
!                                 numerical differentiation of the velocity
!                                 at +1/-1 second from the time tag epoch.
!                    David Gordon 99.01.14 Added PUT's for 'EARTH CE',
!                                 'SUN DATA', and 'MOONDATA' - Earth, Sun,
!                                 and Moon Coordinates. This is to avoid an
!                                 angry lynch mob after Dbedit is updated to
!                                 no longer put these in at data base creation
!                                 time.
!                   Jim Ryan 2002 Sept. Interger*4 updates.
!                   D. Gordon 2004.04.21 Updated to use DE405 ephemeris.
!                   D. Gordon 2012.10.17 Updated to use DE421 ephemeris, 
!                                 and to use TDB at geocenter as time
!                                 argument.
!                   D. Gordon Dec. 2012. EARTH, SUN, MOON moved to 'put2s.i'.
!                                 Moved all PUT's to subroutine PUT_G. 
!                                 
!
!     PEP Program Structure
!
!    Compute new values or use previous values?
       IF (TSKIP .eq. 1) Go to 510
!
!     Get the solar system geometry of the current observation from the
!     DE/LE405 file.
!
!--- CT of observation.
!     TJD = XJD + CT
!    Ephereris time of observation.
      TJD = XJD + ET
!
!   Get SSBC Earth position, velocity, and acceleration vectors
      targ=3       ! Earth
      cent=12      ! Barycentric
!
!  First get Earth position and velocity 1 second before and one second later
      tminus1 = tjd - 1.d0/8.64d4
      tplus1  = tjd + 1.d0/8.64d4
      call pleph(tminus1,targ,cent,rminus1)
      call pleph(tplus1,targ,cent,rplus1)
!
      call pleph(tjd,targ,cent,rrd)
      do i=1,3
        earth(i,1) = pos(i)*1.D3   !convert from km's to meters
        earth(i,2) = vel(i)*1.D3
        earth(i,3) = (rplus1(i+3) - rminus1(i+3))*1.D3 / 2.D0   !Acceleration
         earth1(i,1) = earth(i,1) 
         earth1(i,2) = earth(i,2) 
         earth1(i,3) = earth(i,3) 
      enddo
!
!   Get SSBC Sun position and velocity vectors
      targ=11      ! Sun
      cent=12      ! Barycentric
      call pleph(tjd,targ,cent,rrd)
!    Convert to geocentric coordinates
      do i=1,3
        sun(i,1) = pos(i)*1.D3 - earth(i,1)
        sun(i,2) = vel(i)*1.D3 - earth(i,2)
         sun1(i,1) = sun(i,1)
         sun1(i,2) = sun(i,2)
        SUNb(i,1) = pos(i)*1.D3    ! Barycentric Sun position
        SUNb(i,2) = vel(i)*1.D3    ! Barycentric Sun velocity
      enddo
!
!   Get SSBC Moon position and velocity vectors
      targ=10      ! Moon
      cent=12      ! Barycentric
      call pleph(tjd,targ,cent,rrd)
!    Convert to geocentric coordinates
      do i=1,3
        xmoon(i,1) = pos(i)*1.D3 - earth(i,1)
        xmoon(i,2) = vel(i)*1.D3 - earth(i,2)
         xmoon1(i,1) = xmoon(i,1) 
         xmoon1(i,2) = xmoon(i,2) 
        MOONb(i,1) = pos(i)*1.D3   ! Barycentric Moon position
        MOONb(i,2) = vel(i)*1.D3   ! Barycentric Moon velocity
      enddo
!
!    Get SSBC position and velocity vectors for all other planets except
!     Pluto (1=Mercury, 2=Venus, 3=Mars, 4=Jupiter, 5=Saturn, 6=Uranus,
!     and 7=Neptune)
       cent = 12     ! in SSBC coordinates
      Do itarg=1,7
        if (itarg.le.2)  targ = itarg     ! Mercury and Venus
        if (itarg.gt.2)  targ = itarg+1   ! Mars, Jupiter, etc. (skip Earth)
        call pleph(tjd,targ,cent,rrd)
!
        k = itarg      ! Planet index
        do i=1,3
          SPLANET(i,1,k) = pos(i)*1.D3                    ! SSBC position
          SPLANET(i,2,k) = vel(i)*1.D3                    ! SSBC velocity
          GPLANET(i,1,k) = SPLANET(i,1,k) - Earth(i,1)   ! Geocentric position
          GPLANET(i,2,k) = SPLANET(i,2,k) - Earth(i,2)   ! Geocentric velocity
        enddo
!
      Enddo
!
 510  Continue
!
!   PUT's  for Earth, Sun, and Moon coordinates moved to PUT_G.
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!   Test code to convert to RA and Dec. Keep for future usefulness.
!      do k = 1,7
!        S_planet = Vecmg(SPLANET(1,1,k)) / 1.4959787061D11   ! in AU's
!        G_planet = Vecmg(GPLANET(1,1,k)) / 1.4959787061D11   ! in AU's
!        write(6,'(" k, S_planet, G_planet = ",i3,2f12.7)')
!    .         k, S_planet, G_planet
!
!        call Vunit(GPLANET(1,1,k),unit_planet)
!        x = unit_planet(1)
!        y = unit_planet(2)
!        z = unit_planet(3)
!        write(6,'(" x, y, z = ",3f12.8)') x,y,z
!
!        DEC = DASIND(z)           ! Declination in degrees
!        DEC_min = (DEC - INT(DEC)) * 60.D0
!        DEC_sec = (DEC_min - INT(DEC_min)) * 60.D0
!        K_deg = DEC
!        K_min = DEC_min
!
!        theta = DATAND( DABS (y/x) )
!        if (x.ge.0 .and. y.ge.0) RA =  theta                ! RA in degrees
!        if (x.le.0 .and. y.ge.0) RA = -theta + 180.D0       ! RA in degrees
!        if (x.le.0 .and. y.le.0) RA =  theta + 180.D0       ! RA in degrees
!        if (x.ge.0 .and. y.le.0) RA = -theta + 360.D0       ! RA in degrees
!        RA_hrs = RA/15.D0
!        RA_min = (RA_hrs - INT(RA_hrs)) * 60.D0
!        RA_sec = (RA_min - INT(RA_min)) * 60.D0
!        I_hrs = RA_hrs
!        I_min = RA_min
!        write(6,'("   RA, hr, min, sec = ", F10.5,i5,i3,f6.2 )')
!    .        RA, I_hrs, I_min, RA_sec
!        write(6,'(" DEC, deg, min, sec = ", F10.5,i5,i3,f6.2 )')
!    .        DEC, K_deg, K_min, DEC_sec
!      enddo
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     Check KPEPD for debug output.
      IF ( KPEPD .ne. 0 )  Then
!
        WRITE (6,'(" Debug output for subroutine PEP.")')
!
!       write(6,'(2x," Ephemeris information from JPL DE/LE405", &
        write(6,'(2x," Ephemeris information from JPL DE421", &
     &        " Ephemeris:")')
        write(6,'("XJD, ET, TJD: ",3D25.16)') XJD, ET, TJD
        WRITE ( 6, 9200 )  EARTH, SUN, XMOON
 9200   FORMAT (1X, 'EARTH = ', 3 ( 3 ( D30.16, 10X ), /, 1X ), &
     &              'SUN   = ', 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &              'XMOON = ', 2 ( 3 ( D30.16, 10X ), /, 1X ) )
!
        do k = 1,7
          write(6,'(/)')
          write(6,'("SPLANET(",I1,") = ",3d25.16)') k, &
     &          SPLANET(1,1,k),SPLANET(2,1,k),SPLANET(3,1,k)
          write(6,'("SPLANET(",I1,") = ",3d25.16)') k, &
     &          SPLANET(1,2,k),SPLANET(2,2,k),SPLANET(3,2,k)
          write(6,'("GPLANET(",I1,") = ",3d25.16)') k, &
     &          GPLANET(1,1,k),GPLANET(2,1,k),GPLANET(3,1,k)
          write(6,'("GPLANET(",I1,") = ",3d25.16)') k, &
     &          GPLANET(1,2,k),GPLANET(2,2,k),GPLANET(3,2,k)
        enddo
!
      Endif
!
      RETURN
      END
!
!***************************************************************************
      SUBROUTINE PLEPH ( ET, NTARG, NCENT, RRD )
      Implicit None
!
!  The following code is from JPL with only a few necessary modifications:
!
!
!  NOTE : Over the years, different versions of PLEPH have had a fifth argument:
!  sometimes, an error return statement number; sometimes, a logical denoting
!  whether or not the requested date is covered by the ephemeris.  We apologize
!  for this inconsistency; in this present version, we use only the four
!  necessary arguments and do the testing outside of the subroutine.
!
!     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS AND GIVES THE
!     POSITION AND VELOCITY OF THE POINT 'NTARG' WITH RESPECT TO 'NCENT'.
!
!     CALLING SEQUENCE PARAMETERS:
!
!       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION IS WANTED.
!
!       ** NOTE THE ENTRY DPLEPH FOR A DOUBLY-DIMENSIONED TIME **
!          THE REASON FOR THIS OPTION IS DISCUSSED IN THE SUBROUTINE STATE
!
!     NTARG = INTEGER NUMBER OF 'TARGET' POINT.
!     NCENT = INTEGER NUMBER OF CENTER POINT.
!
!            THE NUMBERING CONVENTION FOR 'NTARG' AND 'NCENT' IS:
!                1 = MERCURY           8 = NEPTUNE
!                2 = VENUS             9 = PLUTO
!                3 = EARTH            10 = MOON
!                4 = MARS             11 = SUN
!                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
!                6 = SATURN           13 = EARTH-MOON BARYCENTER
!                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
!                                     15 = LIBRATIONS, IF ON EPH FILE
!             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
!              SET NTARG = 15. SET NCENT=0.)
!
!      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
!            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
!            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
!            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
!            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
!            RADIANS AND RADIANS/DAY.
!
!            The option is available to have the units in km and km/sec.
!            For this, set km=.true. in the STCOMX common block.
!
      REAL*8  RRD(6),ET2Z(2),ET2(2),PV(6,13), ET, AU, EMRAT
      REAL*8  SS(3),CVAL(400),PVSUN(6)
!  BL1,BL2 added for padding. Common STCOMX rearranged.
      LOGICAL*2 BSAVE,KM,BARY, BL1,BL2
      LOGICAL*2 FIRST
      DATA FIRST/.TRUE./
      INTEGER*4 LIST(12), IPT(39), DENUM, NTARG, NCENT, NCON, I, K
!
      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT
!     COMMON/STCOMX/KM,BARY,PVSUN
      COMMON/STCOMX/PVSUN, KM, BARY, BL1, BL2
!

      ET2(1)=0.D0
      IF(FIRST) CALL STATE(ET2,LIST,PV,RRD)
      FIRST=.FALSE.
!
!  INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
      ET2(1)=ET
      ET2(2)=0.D0
!     GO TO 11
!
!     ENTRY POINT 'DPLEPH' FOR DOUBLY-DIMENSIONED TIME ARGUMENT
!          (SEE THE DISCUSSION IN THE SUBROUTINE STATE)
!     ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)
!     ET2(1)=ET2Z(1)
!     ET2(2)=ET2Z(2)
!
  11  DO I=1,6
      RRD(I)=0.D0
      ENDDO
!
!
  96  IF(NTARG .EQ. NCENT) RETURN
!
      DO I=1,12
      LIST(I)=0
      ENDDO
!
!   CHECK FOR NUTATION CALL
      IF(NTARG.NE.14) GO TO 97
        IF(IPT(35).GT.0) THEN
          LIST(11)=2
          CALL STATE(ET2,LIST,PV,RRD)
          RETURN
        ELSE
          WRITE(6,297)
  297     FORMAT(' *****  NO NUTATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF
!
!   CHECK FOR LIBRATIONS
  97  IF(NTARG.NE.15) GO TO 98
        IF(IPT(38).GT.0) THEN
          LIST(12)=2
          CALL STATE(ET2,LIST,PV,RRD)
          DO I=1,6
          RRD(I)=PV(I,11)
          ENDDO
          RETURN
        ELSE
          WRITE(6,298)
  298     FORMAT(' *****  NO LIBRATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF
!
!   FORCE BARYCENTRIC OUTPUT BY 'STATE'
  98  BSAVE=BARY
      BARY=.TRUE.
!
!   SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL
      DO I=1,2
      K=NTARG
      IF(I .EQ. 2) K=NCENT
      IF(K .LE. 10) LIST(K)=2
      IF(K .EQ. 10) LIST(3)=2
      IF(K .EQ. 3) LIST(10)=2
      IF(K .EQ. 13) LIST(3)=2
      ENDDO
!
!   MAKE CALL TO STATE
      CALL STATE(ET2,LIST,PV,RRD)
!
      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
      DO I=1,6
      PV(I,11)=PVSUN(I)
      ENDDO
      ENDIF
!
      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
      DO I=1,6
      PV(I,12)=0.D0
      ENDDO
      ENDIF
!
      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,13)=PV(I,3)
      ENDDO
      ENDIF
!
      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,3)=0.D0
      ENDDO
      GO TO 99
      ENDIF
!
      IF(LIST(3) .EQ. 2) THEN
      DO I=1,6
      PV(I,3)=PV(I,3)-PV(I,10)/(1.D0+EMRAT)
      ENDDO
      ENDIF
!
      IF(LIST(10) .EQ. 2) THEN
      DO I=1,6
      PV(I,10)=PV(I,3)+PV(I,10)
      ENDDO
      ENDIF
!
  99  DO I=1,6
      RRD(I)=PV(I,NTARG)-PV(I,NCENT)
      ENDDO
!
      BARY=BSAVE
!
      RETURN
      END
!
!*****************************************************************************
      SUBROUTINE INTERP(BUF,T,NCF,NCM,NA,IFL,PV)
      Implicit None
!
!     THIS SUBROUTINE DIFFERENTIATES AND INTERPOLATES A
!     SET OF CHEBYSHEV COEFFICIENTS TO GIVE POSITION AND VELOCITY
!
!     CALLING SEQUENCE PARAMETERS:
!
!       INPUT:
!         BUF   1ST LOCATION OF ARRAY OF D.P. CHEBYSHEV COEFFICIENTS OF POSITION
!           T   T(1) IS DP FRACTIONAL TIME IN INTERVAL COVERED BY
!               COEFFICIENTS AT WHICH INTERPOLATION IS WANTED
!               (0 .LE. T(1) .LE. 1).  T(2) IS DP LENGTH OF WHOLE
!               INTERVAL IN INPUT TIME UNITS.
!         NCF   # OF COEFFICIENTS PER COMPONENT
!         NCM   # OF COMPONENTS PER SET OF COEFFICIENTS
!          NA   # OF SETS OF COEFFICIENTS IN FULL ARRAY
!               (I.E., # OF SUB-INTERVALS IN FULL INTERVAL)
!          IFL  INTEGER FLAG: =1 FOR POSITIONS ONLY
!                             =2 FOR POS AND VEL
!
!       OUTPUT:
!         PV   INTERPOLATED QUANTITIES REQUESTED.  DIMENSION
!               EXPECTED IS PV(NCM,IFL), DP.
!
      SAVE
      REAL*8  BUF(NCF,NCM,*), T(2), PV(NCM,*), PC(18), VC(18), &
     &        DNA, DT1, TEMP, TC, TWOT, VFAC
      INTEGER*4 NCF, NCM, NA, IFL, L, NP, NV, I, J
!
      DATA NP/2/
      DATA NV/3/
      DATA TWOT/0.D0/
      DATA PC(1),PC(2)/1.D0,0.D0/
      DATA VC(2)/1.D0/
!
!       ENTRY POINT. GET CORRECT SUB-INTERVAL NUMBER FOR THIS SET
!       OF COEFFICIENTS AND THEN GET NORMALIZED CHEBYSHEV TIME
!       WITHIN THAT SUBINTERVAL.
!
      DNA=DBLE(NA)
      DT1=DINT(T(1))
      TEMP=DNA*T(1)
      L=IDINT(TEMP-DT1)+1
!
!
!   TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)
      TC=2.D0*(DMOD(TEMP,1.D0)+DT1)-1.D0
!
!       CHECK TO SEE WHETHER CHEBYSHEV TIME HAS CHANGED,
!       AND COMPUTE NEW POLYNOMIAL VALUES IF IT HAS.
!       (THE ELEMENT PC(2) IS THE VALUE OF T1(TC) AND HENCE
!       CONTAINS THE VALUE OF TC ON THE PREVIOUS CALL.)
!
      IF(TC.NE.PC(2)) THEN
        NP=2
        NV=3
        PC(2)=TC
        TWOT=TC+TC
      ENDIF
!
!  BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
!  AND ARE STORED IN THE ARRAY 'PC'.
      IF(NP.LT.NCF) THEN
        DO 1 I=NP+1,NCF
        PC(I)=TWOT*PC(I-1)-PC(I-2)
    1   CONTINUE
        NP=NCF
      ENDIF
!
!   INTERPOLATE TO GET POSITION FOR EACH COMPONENT
      DO 2 I=1,NCM
      PV(I,1)=0.D0
      DO 3 J=NCF,1,-1
      PV(I,1)=PV(I,1)+PC(J)*BUF(J,I,L)
    3 CONTINUE
    2 CONTINUE
      IF(IFL.LE.1) RETURN
!
!  IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
!  DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
      VFAC=(DNA+DNA)/T(2)
      VC(3)=TWOT+TWOT
      IF(NV.LT.NCF) THEN
        DO 4 I=NV+1,NCF
        VC(I)=TWOT*VC(I-1)+PC(I-1)+PC(I-1)-VC(I-2)
    4   CONTINUE
        NV=NCF
      ENDIF
!
!  INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
      DO 5 I=1,NCM
      PV(I,2)=0.D0
      DO 6 J=NCF,2,-1
      PV(I,2)=PV(I,2)+VC(J)*BUF(J,I,L)
    6 CONTINUE
      PV(I,2)=PV(I,2)*VFAC
    5 CONTINUE
!
      RETURN
      END
!
!****************************************************************************
      SUBROUTINE SPLIT(TT,FR)
      Implicit None
!
!   THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
!   AND A D.P. FRACTIONAL PART.
!
!   CALLING SEQUENCE PARAMETERS:
!       TT = D.P. INPUT NUMBER
!       FR = D.P. 2-WORD OUTPUT ARRAY.
!            FR(1) CONTAINS INTEGER PART.
!            FR(2) CONTAINS FRACTIONAL PART.
!            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
!            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
!
!   CALLING SEQUENCE DECLARATIONS
      REAL*8 TT, FR(2)
!
!   MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS
      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)
!
      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN
!
!   MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER
      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0
!
      RETURN
      END
!
!************************************************************************
      SUBROUTINE STATE(ET2,LIST,PV,PNUT)
      Implicit None
!
! THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
!
!     CALLING SEQUENCE PARAMETERS:
!
!     INPUT:
!         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
!               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
!               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
!                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
!                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
!                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
!                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
!                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
!                   ELAPSED BETWEEN ET2(1) AND EPOCH.
!                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
!                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
!                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
!        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
!               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
!                         LIST(I)=0, NO INTERPOLATION FOR BODY I
!                                =1, POSITION ONLY
!                                =2, POSITION AND VELOCITY
!               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
!                         I = 1: MERCURY
!                           = 2: VENUS
!                           = 3: EARTH-MOON BARYCENTER
!                           = 4: MARS
!                           = 5: JUPITER
!                           = 6: SATURN
!                           = 7: URANUS
!                           = 8: NEPTUNE
!                           = 9: PLUTO
!                           =10: GEOCENTRIC MOON
!                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
!                           =12: LUNAR LIBRATIONS (IF ON FILE)
!
!     OUTPUT:
!          PV     DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
!               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
!               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
!               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
!               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
!               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
!               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
!               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
!                 ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
!               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
!               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200.
!                 THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES
!               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC,
!               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
!                 LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
!               LIST(12) IS 1 OR 2.
!         NUT     DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
!               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
!               QUANTITIES IN NUT IS:
!                        D PSI  (NUTATION IN LONGITUDE)
!                        D EPSILON (NUTATION IN OBLIQUITY)
!                        D PSI DOT
!                        D EPSILON DOT
!           *     STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
!               RANGE OR I/O ERRORS.
!
!     COMMON AREA STCOMX:
!          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
!               STATES. KM = .TRUE., KM AND KM/SEC
!                          = .FALSE., AU AND AU/DAY
!               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
!               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
!        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
!               ONLY THE 9 PLANETS ARE AFFECTED.
!                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
!                             = .FALSE. =\ CENTER IS SUN
!               DEFAULT VALUE = .FALSE.
!       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
!               VELOCITY OF THE SUN.
!
      SAVE
      REAL*8    ET2(2),PV(6,12),PNUT(4),T(2),PJD(4),BUF(1500), &
     & SS(3),CVAL(400),PVSUN(6,1), AU, EMRAT, S, AUFAC
!    . SS(3),CVAL(400),PVSUN(3,2), AU, EMRAT, S, AUFAC
      INTEGER*4 LIST(12),IPT(3,13), NUMDE, NCON, NRFILE, &
     &          IRECSZ, NCOEFFS, I, J, K, NRL, NR
      INTEGER*4 I2, I3
      DATA I2 /2/
      DATA I3 /3/
      LOGICAL*2 FIRST
      DATA FIRST/.TRUE./
      CHARACTER*6 TTL(14,3),CNAM(400)
!     CHARACTER*80 NAMFIL
!  BL1,BL2 added for padding. Common STCOMX rearranged.
      LOGICAL*2 KM,BARY, BL1, BL2
!
      INCLUDE 'param11.i'
!       Variables from:
!         1. JPL_405 - Character string giving the complete path name of
!                      the JPL DE405 ephemeris.
!
      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
      COMMON/CHRHDR/CNAM,TTL
!     COMMON/STCOMX/KM,BARY,PVSUN
      COMMON/STCOMX/PVSUN, KM, BARY, BL1, BL2
       DATA KM/.TRUE./
!
!  Program Mods -
!      D.Gordon 2005.02.18 Changed JPL_eph to JPL_405, IRECL to I_RECL,
!                          KSIZE to K_SIZE_JPL, and NRECL to N_RECL_JPL to
!                          avoid conflicts with Calc 9 in param.i
!                          file.
!----------------------------------------------------------------------
!
!   ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
      IF(FIRST) THEN
        FIRST=.FALSE.
!
      NRFILE=12
      I_RECL_JPL = N_RECL_JPL * K_SIZE_JPL
!      print *, ' PEP: I_RECL_JPL, N_RECL_JPL, K_SIZE_JPL ', I_RECL_JPL, N_RECL_JPL, K_SIZE_JPL
!
      NCOEFFS = K_SIZE_JPL/2
!     NCOEFFS = I_RECL_JPL/N_RECL_JPL/2
!
!     OPEN(NRFILE, FILE=JPL_405, ACCESS='DIRECT', FORM='UNFORMATTED', &
!    &     RECL=I_RECL_JPL, STATUS='OLD')
      OPEN(NRFILE, FILE=JPL_DE421, ACCESS='DIRECT', FORM='UNFORMATTED', &
     &     RECL=I_RECL_JPL, STATUS='OLD')
!
      READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT, &
     & ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)
!
      READ(NRFILE,REC=2)CVAL
!
!       write(6,'("JPL_405: ", A)' ) JPL_405
!       write(6,'("I_RECL_JPL: ", I)' ) I_RECL_JPL
!       write(6,'("TTL: ", 3(14(a6)/))') TTL
!       write(6,'("CNAM: ",40(10(a6,1x)/))') CNAM
!       write(6,'("SS: ",3d25.16)') SS
!       write(6,'("NCON, NUMDE: ",2i10)') NCON, NUMDE
!       write(6,'("AU, EMRAT: ",2d25.16)') AU, EMRAT
!       write(6,'("IPT: ",3(13i8,/))') IPT
      NRL=0
!
      ENDIF
!
!       ********** MAIN ENTRY POINT **********
      IF(ET2(1) .EQ. 0.D0) RETURN
!
      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)
!
!       ERROR RETURN FOR EPOCH OUT OF RANGE
      IF(PJD(1)+PJD(4).LT.SS(1) .OR. PJD(1)+PJD(4).GT.SS(2)) GO TO 98
!
!       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL
      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1
      T(1)=((PJD(1)-(DBLE(NR-3)*SS(3)+SS(1)))+PJD(4))/SS(3)
!
!       READ CORRECT RECORD IF NOT IN CORE
      IF(NR.NE.NRL) THEN
        NRL=NR
!      print *, '!!!!!!!!!! STATE: NR = ', NR
        READ(NRFILE,REC=NR,ERR=99)(BUF(K),K=1,NCOEFFS)
      ENDIF
!
      IF(KM) THEN
      T(2)=SS(3)*86400.D0
      AUFAC=1.D0
      ELSE
      T(2)=SS(3)
      AUFAC=1.D0/AU
      ENDIF
!
!   INTERPOLATE SSBARY SUN
!     CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)
      CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),I3,IPT(3,11),I2,PVSUN)
      DO I=1,6
       PVSUN(I,1)=PVSUN(I,1)*AUFAC
      ENDDO
!
!   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED
!
      DO 4 I=1,10
       IF(LIST(I).EQ.0) GO TO 4
!      CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
!    &  LIST(I),PV(1,I))
       CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),I3,IPT(3,I), &
     &  LIST(I),PV(1,I))
!
       DO J=1,6
        IF(I.LE.9 .AND. .NOT.BARY) THEN
        PV(J,I)=PV(J,I)*AUFAC-PVSUN(J,1)
        ELSE
        PV(J,I)=PV(J,I)*AUFAC
        ENDIF
       ENDDO
!
   4  CONTINUE
!
!   DO NUTATIONS IF REQUESTED (AND IF ON FILE)
      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0) THEN
!      CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
!    * LIST(11),PNUT)
       CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),I2,IPT(3,12), &
     & LIST(11),PNUT)
      ENDIF
!
!   GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0) THEN
!      CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
!    * LIST(12),PV(1,11))
       CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),I3,IPT(3,13), &
     & LIST(12),PV(1,11))
      ENDIF
!
      RETURN
!
  98  WRITE(6,198)ET2(1)+ET2(2),SS(1),SS(2)
 198  format(' ***  Requested JED,',f12.2, &
     & ' not within ephemeris limits,',2f12.2,'  ***')
!
      STOP
!
   99 WRITE(6,'(2F12.2,"  ERROR RETURN IN STATE")') ET2
      STOP
      END
