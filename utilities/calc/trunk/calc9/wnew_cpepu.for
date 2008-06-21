      SUBROUTINE PEPA
      IMPLICIT None
C***************************************************************************
C       Calc 9 version of cpepu.f, modified to work in Calc 8.
C        D. Gordon, 2001.05.01
c
c wew 2001.05.02  Designed to read JPL standard ephemeris - DE200
c
C***************************************************************************
C
C 1.    PEPA
C 1.1   PEPA PROGRAM SPECIFICATION
C 1.1.1 PEPA adds entries to the table of contents for the PEP utility.
C 1.2   PEPA PROGRAM INTERFACE
C 1.2.1 CALLING SEQUENCE - CALL PEPA
C
C 1.2.2 COMMON BLOCKS USED
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C              1. KPEPC - The PEP module flow control flag.
C                          = 0 ==> Solar system geometry from JPL DE/LE403
C                                  ephemeris file (default). (Sun, Moon, 
C                                  Earth, and all other planets except Pluto)
C
C 1.2.4 DATA BASE ACCESS -
C            ACCESS CODES ADDED:
C              1. 'PEP MESS'  -  THE DATA BASE ACCESS CODE FOR THE
C                                PEP UTILITY ROUTINE TEXT MESSAGE.
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT 
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDA
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - NONE
C
C 1.2.9 PROGRAMMER - KATHY WATTS    04/25/77
C                    PETER DENATALE 07/19/77
C                    BRUCE SCHUPLER 01/08/80
C                    CHOPO MA       08/04/81
C                    SAVITA GOEL    06/04/87 (CDS FOR A900)
C                    Jim Ryan  89.07.25 Documentation simplified.
C                    Jim Ryan  89.12.12 UNIX-like database interface
C                              implimented.
C                    David Gordon 94.04.18 Converted to Implicit None.
C                    David Gordon 94.06.08 Corrected format statements, single
C                              and double quotes reversed.
C                    David Gordon 98.07.13 Deleted 'PEP 2000' Lcode.
C                    David Gordon 99.01.14 Added access codes 'PEP TAPE',
C                              'EARTH CE', 'SUN DATA', and 'MOONDATA' to
C   
C
C     PEPA Program Structure
C
C     ADD for the PEP utility text message.
      CALL ADDA (1,'PEP MESS','PEP Utility Message Definition  ',
     1     40, 1, 1 )
C
C     ADD for PEP tape update message.
      CALL ADDA (1,'PEP TAPE','Pep utility tape title..........',
     1     64, 1, 1 )
C
C    ADD's to insert/replace Earth, Sun, Moon coordinates
      CALL ADDR (2,'EARTH CE','Earth barycentric coordinates...',
     *     3, 3, 1 )
      CALL ADDR (2,'SUN DATA','Solar geocentric coordinates....',
     *     3, 2, 1 )
      CALL ADDR (2,'MOONDATA','Lunar geocentric coordinates....',
     *     3, 2, 1 )
C
C   Remove obsolete Lcode
       CALL DELA (1,'PEP 2000')
C
  500 RETURN
      END
C
C*******************************************************************************
      SUBROUTINE PEPI
      IMPLICIT None
C
C 3.    PEPI
C
C 3.1   PEPI PROGRAM SPECIFICATION
C
C 3.1.1 PEPI is the PEP utility routine input and initialization section.
C       A message is written into the header giving the PEP module version
C       number. The planetary ephemeris information is now designed to be
C       taken from the JPL DE403/LE403 ephemeris, in compliance with the
C       IERS 1996 Conventions. [The code should also work for other recent 
C       ephemeris's such as DE200 or DE405 if the file is made in the same
C       way as the one used here.] Ehemeris information from the input 
C       database is no longer supported, and those Lcodes are not needed in
C       the data base. The epoch is now J2000 in all cases, so no 1950 =>
C       2000 rotation matrix is needed.
C
C 3.1.2 RESTRICTIONS - None.
C
C 3.2   PEPI PROGRAM INTERFACE
C
C 3.2.1 CALLING SEQUENCE - NONE
C
C 3.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C              1. KPEPD - THE PEP UTILITY ROUTINE DEBUG OUTPUT FLAG.
C              2. KPEPC - The PEP module flow control flag.
C                          = 0 ==> Solar system geometry from JPL DE/LE403
C                             ephemeris file (default). (Sun, Moon, Earth, and
C                             all other planets except Pluto)
C
C 3.2.3 PROGRAM SPECIFICATIONS -
C
      INTEGER*4 N
      INTEGER*2      LPEPU(40),  LPEPT(64)
      CHARACTER*40 C_LPEPU(2)
      CHARACTER*64             C_LPEPT(2)
      EQUIVALENCE (C_LPEPU,LPEPU), (C_LPEPT,LPEPT)
C
      DATA C_LPEPU /
     .'PEP Routine - 98JUL13, D. Gordon/GSFC, J',
     .'2000 Ephemeris from JPL DE403/LE403.    '/
C
      DATA C_LPEPT /
     . 'EARTH, SUN, MOON Coordinates Replaced by Calc 9 Using DE403 '/
c    . '                                                            ',
c    . '       '/
C
C 3.2.4 DATA BASE ACCESS -
C            'PUT' VARIABLES:
C              1. LPEPU(40)  -  THE PEP UTILITY ROUTINE TEXT MESSAGE.
C            ACCESS CODES:
C              1. 'PEP MESS' -  THE DATA BASE ACCESS CODE FOR THE PEP UTILITY
C                               ROUTINE TEXT MESSAGE. 
C 
C 3.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT
C 
C 3.2.6 SUBROUTINE INTERFACE -
C           CALLER SUBROUTINES: INITL 
C           CALLED SUBROUTINES: PUTA, GETI
C
C 3.2.7 CONSTANTS USED - NONE
C
C 3.2.8 PROGRAM VARIABLES -
C
C 3.2.9 PROGRAMMER - KATHY WATTS    05/25/77
C                    PETER DENATALE 07/19/77
C                    BRUCE SCHUPLER 03/09/78
C                    BRUCE SCHUPLER 09/14/78
C                    BRUCE SCHUPLER 09/20/78
C                    BRUCE SCHUPLER 01/08/80
C                    CHOPO MA       08/04/81
C                    SAVITA GOEL 06/04/87 (CDS FOR A900)
C                    Jim Ryan 89.07.25 Documentation simplied and strings.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    D. Gordon 93.11.08 Code for use of JPL DE/LE200 ephemeris. 
C                    D. Gordon 94.04.18 Converted to Implicit None.
C                    D. Gordon 94.05.23 Fixed bug, changed C_LPEPJPL to 
C                              C_LPEPJPL(2).
C                    David Gordon 94.10.05 Minor fixup of 'PEP MESS' text. 
C                    David Gordon 98.07.13 Removed option for getting the
C                              ephemeris values from the data base. Removed
C                              'PEP 2000' Lcode. Modified 'PEP MESS'.
C                    David Gordon 99.01.14 Addition to re-PUT 'PEP TAPE'
C                              access codes to clarify that Sun, Moon, and
C                              Earth coordinates are being inserted by Calc, 
C                              replacing any previous values. 
C
C   PEPI Program Structure
C
C     PUT the PEP utility text messages.
      CALL PUTA ('PEP MESS      ', LPEPU, 40, 1, 1 )
      CALL PUTA ('PEP TAPE      ', LPEPT, 64, 1, 1 )
C
C   See if debug is requested.
      IF(KPEPD .ne. 0) Then
       WRITE(6,9)
       write (6,'("  LPEPU = ",40A2,/)') LPEPU
   9   FORMAT(10X, 'Debug output for subroutine PEPI.')
      Endif
C
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE PEP ( XJD, CT, DEPSD, DPSID, EARTH, SUN, XMOON )
C     SUBROUTINE PEP ( XJD, CT, TSKIP, EARTH, SUN, XMOON )
      IMPLICIT None
C
C***************************************************************************
C      Calc 9 version modified for Calc 8 usage.
C       D. Gordon, 2001.05.01
C***************************************************************************
C
C 4.    PEP
C
C 4.1   PEP PROGRAM SPECIFICATION
C
C 4.1.1 PEP is the utility which obtains the solar system geometry 
C       from the JPL DE403/LE403 ephemeris. Positions and velocities 
C       are obtained for the Sun, Moon, and all planets except Pluto. 
C       Acceleration is also obtained for the Earth. 
C
C       Users Note: The JPL DE403 ephemeris used here was written using
C       the JPL program 'ASC2EPH' for the years 1950 - 2050, with all 
C       integers  defined as INTEGER*4. We use the standard JPL subroutines
C       PLEPH, STATE, INTERP, and SPLIT, with a few modifications. 
C
C 4.1.2 RESTRICTIONS - None
C
C 4.1.3 REFERENCES - JPL anonymous FTP site 'navigator.jpl.nasa.gov'
C
C 4.2   PEP PROGRAM INTERFACE
C
C 4.2.1 CALLING SEQUENCE -
C           INPUT VARIABLES:
C             1.  XJD   - The Julian date at zero hours UTC of the date in 
C                         question. (days)
C             2.  CT    - The coordinate time fraction of the coordinate time
C                         day (days).
C             3.  TSKIP - Flag telling whether to compute new values or use
C                         previous values. If TSKIP = 0 => new time, compute
C                         new values. If TSKIP = 1 => same time as previous
C                         observation, re-use previous values.
C
C           OUTPUT VARIABLES:
C             1. EARTH(3,3) -  THE J2000.0 BARYCENTRIC EARTH POSITION, VELOCITY,
C                              AND ACCELERATION VECTORS. (M, M/SEC, M/SEC**2) 
C                              (THE FIRST INDEX RUNS OVER THE VECTOR
C                              COMPONENTS, THE SECOND RUNS OVER THE POSITION,
C                              VELOCITY, AND ACCELERATION RESPECTIVELY.) 
C             2. SUN(3,2)   -  THE J2000.0 GEOCENTRIC SUN POSITION AND VELOCITY
C                              VECTORS. (M, M/SEC)
C             3. XMOON(3,2) -  THE J2000.0 GEOCENTRIC MOON POSITION AND VELOCITY
C                              VECTORS. (M, M/SEC)
C 
C 4.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'csolsys.i'            
C         VARIABLES 'TO': 
C           1. SPLANET(3,2,7) - The J2000.0 Solar System Barycentric positions
C                               and velocities of all planets except the Earth
C                               and Pluto. (meters, meters/sec) The first index
C                               runs over X, Y, and Z, the second runs over
C                               position and velocity, and the third runs over
C                               the planets, where
C                                      1 = Mercury
C                                      2 = Venus
C                                      3 = Mars
C                                      4 = Jupiter
C                                      5 = Saturn
C                                      6 = Uranus
C                                      7 = Neptune 
C           2. GPLANET(3,2,7) - The J2000.0 Geocentric positions and velocities
C                               of all planets except the Earth and Pluto.
C                               (meters, meters/sec) The first index runs over
C                               X, Y, and Z, the second runs over position and
C                               velocity, and the third runs over the planets,
C                               where
C                                      1 = Mercury
C                                      2 = Venus
C                                      3 = Mars
C                                      4 = Jupiter
C                                      5 = Saturn
C                                      6 = Uranus
C                                      7 = Neptune 
C
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KPEPD - THE PEP UTILITY ROUTINE DEBUG OUTPUT FLAG. 
C              2. KPEPC - The PEP module flow control flag (No options).
C 
C 4.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  EARTH(3,3),SUN(3,2),XMOON(3,2)
          Real*8 DEPSD(2), DPSID(2)
c     Real*8  EARTHD(3,3),SUND(3,2),XMOOND(3,2)
c     Real*8  DEARTH(3,3),DSUN(3,3),DXMOON(3,3)
      INTEGER*4 TSKIP, targ,cent,ipv,ierr, itarg, I, K
      Real*8 xjd,tjd,ct,obsdif
      Real*8  pos(3), vel(3), rrd(6)
      Equivalence ( rrd(1), pos(1) )
      Equivalence ( rrd(4), vel(1) )
      Real*8 tplus1, tminus1, rplus1(6), rminus1(6)
C
C     Real*8 unit_planet(3)
C     Real*8 S_planet, G_planet, x, y, z, DEC, DEC_min, DEC_sec, theta,
C    .       RA, RA_hrs, RA_min, RA_sec, Vecmg
C     Integer*4 K_deg, K_min, I_hrs, I_min
C
C 4.2.4 DATA BASE ACCESS - None
C 
C 4.2.5 EXTERNAL INPUT/OUTPUT - Possible debug and error output 
C 
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG
C             CALLED SUBROUTINES: PLEPH
C
C 4.2.7 CONSTANTS USED - NONE
C
C 4.2.8 PROGRAM VARIABLES -
C          1. targ    - Integer number of the 'target' object in subroutine
C                       PLEPH (see PLEPH).
C          2. cent    - Integer number of the reference point in subroutine
C                       PLEPH (see PLEPH). Here we use cent=12 ==> SSBC.
C          3. pos(3)  - J2000 position of 'targ' with respect to 'cent', as
C                       returned by subroutine PLEPH. (km) 
C          4. vel(3)  - J2000 velocity of 'targ' with respect to 'cent', as
C                       returned by subroutine PLEPH. (km/sec) 
C          5. tjd     - Time of the current observation (Julian days).
C
C 4.2.9 PROGRAMMER - KATHY WATTS    04/25/77
C                    PETER DENATALE 07/19/77
C                    BRUCE SCHUPLER 11/17/77
C                    BRUCE SCHUPLER 01/08/80
C                    CHOPO MA       08/04/81
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    David Gordon, Brent Archinal  Oct/Nov 1993 - Use of JPL
C                                  DE/LE200 ephemeris added for Sun, Moon, and
C                                  all planets except Pluto. New common block
C                                  /SOLSYS/ added to hold planetary information.
C                    David Gordon 94.01.07 Code added to compare time with 
C                                 time of previous scan and skip solar system
C                                 ephemeris calculations if they are the same.
C                    David Gordon 94.04.18 Converted to Implicit None.
C                    David Gordon 94.06.07 Documentation expanded.
C                    David Gordon/Michael Kesteven 94.06.07 Mods for SUN and 
C                                 DEC ALPHA compatibility: rrd(12) put in call
C                                 to PLEPH.
C                    David Gordon 98.02.05 DPSI and DEPS (database nutation)
C                                 removed. Computation repeat/no repeat logic
C                                 moved to DRIVG. Options to get Sun, Moon,
C                                 Earth from data base removed.
C                    David Gordon 98.07.13 Converted to read JPL DE/LE403 
C                                 Ephemeris for compliance with the IERS 1996
C                                 Conventions. Old code would not work so
C                                 now using the standard JPL ephemeris
C                                 subroutines, with slight modifications.
C                                 Earth's acceleration obtained by doing a
C                                 numerical differentiation of the velocity
C                                 at +1/-1 second from the time tag epoch.
C                    David Gordon 99.01.14 Added PUT's for 'EARTH CE',
C                                 'SUN DATA', and 'MOONDATA' - Earth, Sun, 
C                                 and Moon Coordinates. This is to avoid an
C                                 angry lynch mob after Dbedit is updated to
C                                 no longer put these in at data base creation
C                                 time.
C
C     PEP Program Structure
C
C    Compute new values or use previous values?
C!!!   IF (TSKIP .eq. 1) Go to 510
C
C     Get the solar system geometry of the current observation from the 
C     DE/LE403 file.
C
C--- CT of observation.
      tjd = xjd + ct
C
C*******************************************************************
C   Code from Calc 8
C  Check if the time is the same as the previous Observation. If it is, then
C    keep the previous solar system parameters and return. If not, calculate
C    new parameters.
       obsdif = dabs(tjd - sjd)     ! difference in time from previous obs.
       sjd = tjd                    ! save for next observation
       if (obsdif .lt. 1.D-7) then  ! Same time (diff .lt. .000864 seconds)
         Return
       endif
C*******************************************************************
C
C   Get SSBC Earth position, velocity, and acceleration vectors
      targ=3       ! Earth
      cent=12      ! Barycentric
C
C  First get Earth position and velocity 1 second before and one second later
      tminus1 = tjd - 1.d0/8.64d4
      tplus1  = tjd + 1.d0/8.64d4
      call pleph(tminus1,targ,cent,rminus1)
      call pleph(tplus1,targ,cent,rplus1)
C
      call pleph(tjd,targ,cent,rrd)
      do i=1,3
        earth(i,1) = pos(i)*1.D3   !convert from km's to meters
        earth(i,2) = vel(i)*1.D3
        earth(i,3) = (rplus1(i+3) - rminus1(i+3))*1.D3 / 2.D0   !Acceleration
      enddo
C
C   Get SSBC Sun position and velocity vectors
      targ=11      ! Sun
      cent=12      ! Barycentric
      call pleph(tjd,targ,cent,rrd)
C    Convert to geocentric coordinates
      do i=1,3   
        sun(i,1) = pos(i)*1.D3 - earth(i,1)
        sun(i,2) = vel(i)*1.D3 - earth(i,2)
      enddo
C
C   Get SSBC Moon position and velocity vectors
      targ=10      ! Moon
      cent=12      ! Barycentric
      call pleph(tjd,targ,cent,rrd)
C    Convert to geocentric coordinates
      do i=1,3 
        xmoon(i,1) = pos(i)*1.D3 - earth(i,1)
        xmoon(i,2) = vel(i)*1.D3 - earth(i,2)
      enddo
C
C    Get SSBC position and velocity vectors for all other planets except 
C     Pluto (1=Mercury, 2=Venus, 3=Mars, 4=Jupiter, 5=Saturn, 6=Uranus,
C     and 7=Neptune)
       cent = 12     ! in SSBC coordinates
      Do itarg=1,7
        if (itarg.le.2)  targ = itarg     ! Mercury and Venus
        if (itarg.gt.2)  targ = itarg+1   ! Mars, Jupiter, etc. (skip Earth) 
        call pleph(tjd,targ,cent,rrd)
C
        k = itarg      ! Planet index
        do i=1,3
          SPLANET(i,1,k) = pos(i)*1.D3                    ! SSBC position 
          SPLANET(i,2,k) = vel(i)*1.D3                    ! SSBC velocity 
          GPLANET(i,1,k) = SPLANET(i,1,k) - Earth(i,1)   ! Geocentric position
          GPLANET(i,2,k) = SPLANET(i,2,k) - Earth(i,2)   ! Geocentric velocity
        enddo
C
      Enddo
C
 510  Continue
C   Do the PUT's for the Earth, Sun, and Moon coordinates
c wew - following three lines not required - comment out
c      CALL PUT4 ('EARTH CE      ', EARTH, 3, 3, 1 )
c      CALL PUT4 ('SUN DATA      ', SUN  , 3, 2, 1 )
c      CALL PUT4 ('MOONDATA      ', XMOON, 3, 2, 1 )

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   Test code to convert to RA and Dec. Keep for future usefulness.
C      do k = 1,7
C        S_planet = Vecmg(SPLANET(1,1,k)) / 1.4959787061D11   ! in AU's
C        G_planet = Vecmg(GPLANET(1,1,k)) / 1.4959787061D11   ! in AU's
C        write(6,'(" k, S_planet, G_planet = ",i3,2f12.7)') 
C    .         k, S_planet, G_planet 
C
C        call Vunit(GPLANET(1,1,k),unit_planet)
C        x = unit_planet(1)
C        y = unit_planet(2)
C        z = unit_planet(3)
C        write(6,'(" x, y, z = ",3f12.8)') x,y,z 
C
C        DEC = DASIND(z)           ! Declination in degrees
C        DEC_min = (DEC - INT(DEC)) * 60.D0
C        DEC_sec = (DEC_min - INT(DEC_min)) * 60.D0
C        K_deg = DEC
C        K_min = DEC_min
C
C        theta = DATAND( DABS (y/x) )
C        if (x.ge.0 .and. y.ge.0) RA =  theta                ! RA in degrees
C        if (x.le.0 .and. y.ge.0) RA = -theta + 180.D0       ! RA in degrees
C        if (x.le.0 .and. y.le.0) RA =  theta + 180.D0       ! RA in degrees
C        if (x.ge.0 .and. y.le.0) RA = -theta + 360.D0       ! RA in degrees
C        RA_hrs = RA/15.D0
C        RA_min = (RA_hrs - INT(RA_hrs)) * 60.D0
C        RA_sec = (RA_min - INT(RA_min)) * 60.D0
C        I_hrs = RA_hrs
C        I_min = RA_min 
C        write(6,'("   RA, hr, min, sec = ", F10.5,i5,i3,f6.2 )')
C    .        RA, I_hrs, I_min, RA_sec
C        write(6,'(" DEC, deg, min, sec = ", F10.5,i5,i3,f6.2 )')
C    .        DEC, K_deg, K_min, DEC_sec
C      enddo
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Check KPEPD for debug output.
      IF ( KPEPD .ne. 0 )  Then
C
        WRITE (6,'(" Debug output for subroutine PEP.")')
C
       If (KPEPC .eq. 0) Then
        write(6,'(2x," Ephemeris information from JPL DE/LE403",
     .        " Ephemeris:")')
        write(6,'("XJD, CT, TJD: ",3D25.16)') xjd,ct,tjd
        WRITE ( 6, 9200 )  EARTH, SUN, XMOON
 9200   FORMAT (1X, 'EARTH = ', 3 ( 3 ( D30.16, 10X ), /, 1X ),
     .              'SUN   = ', 2 ( 3 ( D30.16, 10X ), /, 1X ),
     .              'XMOON = ', 2 ( 3 ( D30.16, 10X ), /, 1X ) )
C
        do k = 1,7
          write(6,'(/)')
          write(6,'("SPLANET(",I1,") = ",3d25.16)') k,
     .          SPLANET(1,1,k),SPLANET(2,1,k),SPLANET(3,1,k)
          write(6,'("SPLANET(",I1,") = ",3d25.16)') k,
     .          SPLANET(1,2,k),SPLANET(2,2,k),SPLANET(3,2,k)
          write(6,'("GPLANET(",I1,") = ",3d25.16)') k,
     .          GPLANET(1,1,k),GPLANET(2,1,k),GPLANET(3,1,k)
          write(6,'("GPLANET(",I1,") = ",3d25.16)') k, 
     .          GPLANET(1,2,k),GPLANET(2,2,k),GPLANET(3,2,k)
        enddo
C
       Endif
C
      Endif
C
      RETURN
      END
C
C*************************************************************************
      SUBROUTINE PLEPH ( ET, NTARG, NCENT, RRD )
      Implicit None
C  The following code is from JPL with only a few necessary modifications:
C*************************************************************************
C
C  NOTE : Over the years, different versions of PLEPH have had a fifth argument:
C  sometimes, an error return statement number; sometimes, a logical denoting
C  whether or not the requested date is covered by the ephemeris.  We apologize
C  for this inconsistency; in this present version, we use only the four 
C  necessary arguments and do the testing outside of the subroutine.
C
C     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS AND GIVES THE 
C     POSITION AND VELOCITY OF THE POINT 'NTARG' WITH RESPECT TO 'NCENT'.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION IS WANTED.
C
C       ** NOTE THE ENTRY DPLEPH FOR A DOUBLY-DIMENSIONED TIME **
C          THE REASON FOR THIS OPTION IS DISCUSSED IN THE SUBROUTINE STATE
C
C     NTARG = INTEGER NUMBER OF 'TARGET' POINT.
C     NCENT = INTEGER NUMBER OF CENTER POINT.
C
C            THE NUMBERING CONVENTION FOR 'NTARG' AND 'NCENT' IS:
C                1 = MERCURY           8 = NEPTUNE
C                2 = VENUS             9 = PLUTO
C                3 = EARTH            10 = MOON
C                4 = MARS             11 = SUN
C                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
C                6 = SATURN           13 = EARTH-MOON BARYCENTER
C                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
C                                     15 = LIBRATIONS, IF ON EPH FILE
C             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
C              SET NTARG = 15. SET NCENT=0.)
C
C      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
C            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
C            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
C            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
C            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
C            RADIANS AND RADIANS/DAY.
C
C            The option is available to have the units in km and km/sec.
C            For this, set km=.true. in the STCOMX common block.
C
      REAL*8  RRD(6),ET2Z(2),ET2(2),PV(6,13), ET, AU, EMRAT
      REAL*8  SS(3),CVAL(400),PVSUN(6)
      LOGICAL BSAVE,KM,BARY
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER*4 LIST(12), IPT(39), DENUM, NTARG, NCENT, NCON, I, K

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT
      COMMON/STCOMX/KM,BARY,PVSUN

C  INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
      ET2(1)=ET
      ET2(2)=0.D0
C     GO TO 11
C
C     ENTRY POINT 'DPLEPH' FOR DOUBLY-DIMENSIONED TIME ARGUMENT 
C          (SEE THE DISCUSSION IN THE SUBROUTINE STATE)
C     ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)
C     ET2(1)=ET2Z(1)
C     ET2(2)=ET2Z(2)
C
  11  DO I=1,6
      RRD(I)=0.D0
      ENDDO
C
      IF(FIRST) CALL STATE(0.D0,0,0,0)
      FIRST=.FALSE.
C
  96  IF(NTARG .EQ. NCENT) RETURN
C
      DO I=1,12
      LIST(I)=0
      ENDDO
C
C   CHECK FOR NUTATION CALL
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
C
C   CHECK FOR LIBRATIONS
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
C
C   FORCE BARYCENTRIC OUTPUT BY 'STATE'
  98  BSAVE=BARY
      BARY=.TRUE.
C
C   SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL
      DO I=1,2
      K=NTARG
      IF(I .EQ. 2) K=NCENT
      IF(K .LE. 10) LIST(K)=2
      IF(K .EQ. 10) LIST(3)=2
      IF(K .EQ. 3) LIST(10)=2
      IF(K .EQ. 13) LIST(3)=2
      ENDDO
C
C   MAKE CALL TO STATE
      CALL STATE(ET2,LIST,PV,RRD)
C
      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
      DO I=1,6
      PV(I,11)=PVSUN(I)
      ENDDO
      ENDIF
C
      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
      DO I=1,6
      PV(I,12)=0.D0
      ENDDO
      ENDIF
C
      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,13)=PV(I,3)
      ENDDO
      ENDIF
C
      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,3)=0.D0
      ENDDO
      GO TO 99
      ENDIF
C
      IF(LIST(3) .EQ. 2) THEN
      DO I=1,6
      PV(I,3)=PV(I,3)-PV(I,10)/(1.D0+EMRAT)
      ENDDO
      ENDIF
C
      IF(LIST(10) .EQ. 2) THEN
      DO I=1,6
      PV(I,10)=PV(I,3)+PV(I,10)
      ENDDO
      ENDIF
C
  99  DO I=1,6
      RRD(I)=PV(I,NTARG)-PV(I,NCENT)
      ENDDO
C
      BARY=BSAVE
C
      RETURN
      END
C
C**************************************************************************
      SUBROUTINE INTERP(BUF,T,NCF,NCM,NA,IFL,PV)
      Implicit None
C
C     THIS SUBROUTINE DIFFERENTIATES AND INTERPOLATES A
C     SET OF CHEBYSHEV COEFFICIENTS TO GIVE POSITION AND VELOCITY
C
C     CALLING SEQUENCE PARAMETERS:
C
C       INPUT:
C         BUF   1ST LOCATION OF ARRAY OF D.P. CHEBYSHEV COEFFICIENTS OF POSITION
C           T   T(1) IS DP FRACTIONAL TIME IN INTERVAL COVERED BY
C               COEFFICIENTS AT WHICH INTERPOLATION IS WANTED
C               (0 .LE. T(1) .LE. 1).  T(2) IS DP LENGTH OF WHOLE
C               INTERVAL IN INPUT TIME UNITS.
C         NCF   # OF COEFFICIENTS PER COMPONENT
C         NCM   # OF COMPONENTS PER SET OF COEFFICIENTS
C          NA   # OF SETS OF COEFFICIENTS IN FULL ARRAY
C               (I.E., # OF SUB-INTERVALS IN FULL INTERVAL)
C          IFL  INTEGER FLAG: =1 FOR POSITIONS ONLY
C                             =2 FOR POS AND VEL
C
C       OUTPUT:
C         PV   INTERPOLATED QUANTITIES REQUESTED.  DIMENSION
C               EXPECTED IS PV(NCM,IFL), DP.
C
      SAVE

      INTEGER*4 NCF, NCM, NA, IFL, L, NP, NV, I, J
      REAL*8  BUF(NCF,NCM,*), T(2), PV(NCM,*), PC(18), VC(18),
     *        DNA, DT1, TEMP, TC, TWOT, VFAC 
C
      DATA NP/2/
      DATA NV/3/
      DATA TWOT/0.D0/
      DATA PC(1),PC(2)/1.D0,0.D0/
      DATA VC(2)/1.D0/
C
C       ENTRY POINT. GET CORRECT SUB-INTERVAL NUMBER FOR THIS SET
C       OF COEFFICIENTS AND THEN GET NORMALIZED CHEBYSHEV TIME
C       WITHIN THAT SUBINTERVAL.
C
      DNA=DBLE(NA)
      DT1=DINT(T(1))
      TEMP=DNA*T(1)
      L=IDINT(TEMP-DT1)+1

C   TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)
      TC=2.D0*(DMOD(TEMP,1.D0)+DT1)-1.D0
C
C       CHECK TO SEE WHETHER CHEBYSHEV TIME HAS CHANGED,
C       AND COMPUTE NEW POLYNOMIAL VALUES IF IT HAS.
C       (THE ELEMENT PC(2) IS THE VALUE OF T1(TC) AND HENCE
C       CONTAINS THE VALUE OF TC ON THE PREVIOUS CALL.)
C
      IF(TC.NE.PC(2)) THEN
        NP=2
        NV=3
        PC(2)=TC
        TWOT=TC+TC
      ENDIF
C
C  BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
C  AND ARE STORED IN THE ARRAY 'PC'.
      IF(NP.LT.NCF) THEN
        DO 1 I=NP+1,NCF
        PC(I)=TWOT*PC(I-1)-PC(I-2)
    1   CONTINUE
        NP=NCF
      ENDIF
C
C   INTERPOLATE TO GET POSITION FOR EACH COMPONENT
      DO 2 I=1,NCM
      PV(I,1)=0.D0
      DO 3 J=NCF,1,-1
      PV(I,1)=PV(I,1)+PC(J)*BUF(J,I,L)
    3 CONTINUE
    2 CONTINUE
      IF(IFL.LE.1) RETURN
C
C  IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
C  DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
      VFAC=(DNA+DNA)/T(2)
      VC(3)=TWOT+TWOT
      IF(NV.LT.NCF) THEN
        DO 4 I=NV+1,NCF
        VC(I)=TWOT*VC(I-1)+PC(I-1)+PC(I-1)-VC(I-2)
    4   CONTINUE
        NV=NCF
      ENDIF
C
C  INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
      DO 5 I=1,NCM
      PV(I,2)=0.D0
      DO 6 J=NCF,2,-1
      PV(I,2)=PV(I,2)+VC(J)*BUF(J,I,L)
    6 CONTINUE
      PV(I,2)=PV(I,2)*VFAC
    5 CONTINUE
C
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE SPLIT(TT,FR)
      Implicit None
C
C   THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
C   AND A D.P. FRACTIONAL PART.
C
C   CALLING SEQUENCE PARAMETERS:
C       TT = D.P. INPUT NUMBER
C       FR = D.P. 2-WORD OUTPUT ARRAY.
C            FR(1) CONTAINS INTEGER PART.
C            FR(2) CONTAINS FRACTIONAL PART.
C            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
C            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
C
C   CALLING SEQUENCE DECLARATIONS
      REAL*8 TT, FR(2)
C
C   MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS
      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)
C
      IF(TT.GE.0.D0 .OR. FR(2).EQ.0.D0) RETURN
C
C   MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER
      FR(1)=FR(1)-1.D0
      FR(2)=FR(2)+1.D0
C
      RETURN
      END
C
C********************************************************************
      SUBROUTINE STATE(ET2,LIST,PV,PNUT)
      Implicit None
C
C THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
C
C     CALLING SEQUENCE PARAMETERS:
C
C     INPUT:
C         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
C               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
C               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
C                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
C                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
C                   ELAPSED BETWEEN ET2(1) AND EPOCH.
C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
C                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
C                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
C                           =12: LUNAR LIBRATIONS (IF ON FILE)
C
C     OUTPUT:
C          PV     DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
C               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
C               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
C               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
C               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
C               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
C               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C                 ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200. 
C                 THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC, 
C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
C                 LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
C               LIST(12) IS 1 OR 2.
C         NUT     DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C                        D PSI  (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C           *     STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
C               RANGE OR I/O ERRORS.
C
C     COMMON AREA STCOMX:
C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
C               STATES. KM = .TRUE., KM AND KM/SEC
C                          = .FALSE., AU AND AU/DAY
C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
C               ONLY THE 9 PLANETS ARE AFFECTED.
C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
C                             = .FALSE. =\ CENTER IS SUN
C               DEFAULT VALUE = .FALSE.
C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
C               VELOCITY OF THE SUN.
C
      SAVE
      REAL*8    ET2(2),PV(6,12),PNUT(4),T(2),PJD(4),BUF(1500),
     . SS(3),CVAL(400),PVSUN(6,1), AU, EMRAT, S, AUFAC
C    . SS(3),CVAL(400),PVSUN(3,2), AU, EMRAT, S, AUFAC
C     INTEGER*4 LIST(12),IPT(3,13), NUMDE, NCON, NRECL, KSIZE, NRFILE,
      INTEGER*4 LIST(12),IPT(3,13), NUMDE, NCON, NRECL,        NRFILE,
     *          IRECSZ, NCOEFFS, I, J, K, NRL, NR
      INTEGER*4 I2, I3
      DATA I2 /2/
      DATA I3 /3/
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      CHARACTER*6 TTL(14,3),CNAM(400)
C     CHARACTER*80 NAMFIL
      LOGICAL KM,BARY
C

C wew      INCLUDE 'param.i'
      INCLUDE 'params.i'

C       Variables from:
C         1. JPL_eph - Character string giving the complete path name of 
C                      the JPL DE403 ephemeris.
C
      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
      COMMON/CHRHDR/CNAM,TTL
      COMMON/STCOMX/KM,BARY,PVSUN
       DATA KM/.TRUE./
C
C   ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
      IF(FIRST) THEN
        FIRST=.FALSE.
C
      NRECL=4
      NRFILE=12
C     NAMFIL= 'JPL.DE403'
C     KSIZE = 1796
C
C     IRECSZ=NRECL*KSIZE
C     NCOEFFS=KSIZE/2

C wew      NCOEFFS = IRECL/NRECL/2
      NCOEFFS = EPH_RECL/2
C
C     OPEN(NRFILE, FILE=NAMFIL, ACCESS='DIRECT', FORM='UNFORMATTED',
C    *     RECL=IRECSZ, STATUS='OLD')
      OPEN(NRFILE, 
     *FILE='$DISK3:[CORR.EPHEM]JPLEPH_DE200.DAT',
     *   ACCESS='DIRECT', FORM='UNFORMATTED',
C wew     *     RECL=IRECL, STATUS='OLD')
     *     RECL=EPH_RECL, STATUS='OLD')

C
      READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,
     . ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)
C
      READ(NRFILE,REC=2)CVAL
C

c wew: Add
      write(6,'(/" Opened JPL DE200 Ephemeris - MJD range: ",
     .                  2f12.1/)' ) SS(1), SS(2)

C       write(6,'("TTL: ", 3(14(a6)/))') TTL
C       write(6,'("CNAM: ",40(10(a6,1x)/))') CNAM
C       write(6,'("SS: ",3d25.16)') SS  
C       write(6,'("NCON, NUMDE: ",2i10)') NCON, NUMDE  
C       write(6,'("AU, EMRAT: ",2d25.16)') AU, EMRAT    
C       write(6,'("IPT: ",3(13i8,/))') IPT 
      NRL=0
C
      ENDIF
C
C       ********** MAIN ENTRY POINT **********
      IF(ET2(1) .EQ. 0.D0) RETURN
C
      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)
C
C       ERROR RETURN FOR EPOCH OUT OF RANGE
      IF(PJD(1)+PJD(4).LT.SS(1) .OR. PJD(1)+PJD(4).GT.SS(2)) GO TO 98
C
C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL
      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1
      T(1)=((PJD(1)-(DBLE(NR-3)*SS(3)+SS(1)))+PJD(4))/SS(3)
C
C       READ CORRECT RECORD IF NOT IN CORE
      IF(NR.NE.NRL) THEN
        NRL=NR
C      print *, '!!!!!!!!!! STATE: NR = ', NR
        READ(NRFILE,REC=NR,ERR=99)(BUF(K),K=1,NCOEFFS)
      ENDIF
C
      IF(KM) THEN
      T(2)=SS(3)*86400.D0
      AUFAC=1.D0
      ELSE
      T(2)=SS(3)
      AUFAC=1.D0/AU
      ENDIF
C
C   INTERPOLATE SSBARY SUN
C     CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)
      CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),I3,IPT(3,11),I2,PVSUN)
      DO I=1,6
       PVSUN(I,1)=PVSUN(I,1)*AUFAC
      ENDDO
C
C   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED
C
      DO 4 I=1,10
       IF(LIST(I).EQ.0) GO TO 4
C      CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
C    &  LIST(I),PV(1,I))
       CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),I3,IPT(3,I),
     &  LIST(I),PV(1,I))
C
       DO J=1,6
        IF(I.LE.9 .AND. .NOT.BARY) THEN
        PV(J,I)=PV(J,I)*AUFAC-PVSUN(J,1)
        ELSE
        PV(J,I)=PV(J,I)*AUFAC
        ENDIF
       ENDDO
C
   4  CONTINUE
C
C   DO NUTATIONS IF REQUESTED (AND IF ON FILE)
      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0) THEN
C      CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
C    * LIST(11),PNUT)
       CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),I2,IPT(3,12),
     * LIST(11),PNUT)
      ENDIF
C
C   GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0) THEN
C      CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
C    * LIST(12),PV(1,11))
       CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),I3,IPT(3,13),
     * LIST(12),PV(1,11))
      ENDIF
C
      RETURN
C
  98  WRITE(6,198)ET2(1)+ET2(2),SS(1),SS(2)
 198  format(' ***  Requested JED,',f12.2,
     * ' not within ephemeris limits,',2f12.2,'  ***')
C
      STOP
C
   99 WRITE(6,'(2F12.2,"  ERROR RETURN IN STATE")') ET2
      STOP
      END
