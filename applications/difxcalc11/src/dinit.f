      SUBROUTINE dINITL (Kjob)
      IMPLICIT None
!
! 1.    INITL
!
! 1.1   INITL PROGRAM SPECIFICATION
!
! 1.1.1 INITL obtains the mathematical and physical constants from the database
!       and elsewhere and loads them into the common block 'cphys' for their
!       use throughout the program. INITL also calls the input and
!       initialization sections of the model modules and the necessary utility
!       routines. Each section will obtain internally the model module parameter
!       values from the database and initialize all such variables in the local
!       common block. Each section will also put into the header a text message
!       for each model module and necessary utility routine. INITL also
!       initializes a counter which counts the observation number and writes a
!       header text for the observation number, time tag, baseline
!       identification, and source identification.
!       SUBROUTINE INITL IS CALLED ONLY ONCE PER DATA BASE.
!
! 1.2   INITL PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE -
!             OUTPUT VARIABLES:
!               1. KOUNT - THE VARIABLE WHICH INITIALIZES THE COUNTER
!                          OF THE OBSERVATION ITEMS TO ZERO. (UNITLESS)
!
! 1.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!            VARIABLES 'TO':
!              2. EFLAT   - THE FLATTENNING OF THE ELLIPSOID WHICH APPROXIMATES
!                           THE SHAPE OF THE EARTH. (UNITLESS)  (Site module)
!              3. GMMOON  - THE MASS OF THE MOON MULTIPLIED BY THE NEWTONIAN
!                           GRAVITATIONAL CONSTANT. (M**3/SEC**2)
!              4. GMSUN   - THE MASS OF THE SUN MULTIPLIED BY THE NEWTONIAN
!                           GRAVITATIONAL CONSTANT. (M**3/SEC**2)
!              5. GMEARTH - THE MASS OF THE EARTH MULTIPLIED BY THE NEWTONIAN
!                           GRAVITATIONAL CONSTANT. (M**3/SEC**2)
!              6. REARTH  - THE EQUATORIAL RADIUS OF THE EARTH. (M)
!              7. SECPAU  - THE NUMBER OF LIGHT-SECONDS PER ASTRONOMICAL UNIT.
!                           (SEC/A.U.)
!              8. GAMMA   - THE POST NEWTONIAN EXPANSION PARAMETER WHICH AFFECTS
!                           LIGHT BENDING. (1.0 FOR EINSTEIN).
!              9. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
!             10. VLIGHT2 - THE VELOCITY OF LIGHT SQUARED. ((M/SEC)**2)
!             11. VLIGHT3 - THE VELOCITY OF LIGHT CUBED. ((M/SEC)**3)
!             12. GMPLANET(7)-The masses of 7 of the planets multiplied by the
!                           gravitational constant. (1=Mercury, 2=Venus, 3=Mars,
!                           4=Jupiter, 5=Saturn, 6=Uranus, and 7=Neptune)
!             13. AU_meters-The Astronomical unit. (meters)
!
      INCLUDE 'ccon.i'
!       Variables 'from':
!              1. ILUOUT  - Output control flag.
!
!      INCLUDE 'cuser11.i'
!            Variables 'from':
!              1. Calc_user - Character denoting the user type:
!                             'A' = Analysis center, using the old Mark3 
!                                   database handler.
!                             'N' = Future analysis mode, to work with nuSolve
!                                   and NetCDF files. -Not yet implemented-
!                             'C' = Correlator user. Interfaces with some
!                                   type of calc server.
!                             'D' = Difx correlator user. Similar to type C,
!                                   but will get input from a '.calc' 
!                                   correlator file.
!
      INCLUDE 'd_input.i'
!       Variables 'from':
!              1. NumSpace - Number of spacecrafts. If .ge. 1, we switch to 
!                            the near field mode and call subroutine SpaceI
!                            for spline initialization of the spacecraft 
!                            positions and veloxcities. 
!
!
!       PROGRAM SPECIFICATIONS -
      Integer*4  KOUNT, Kjob
!
!       PROGRAM VARIABLES -
!           1. Kjob - .calc job number. Used to conserve LU numbers.
!
!       PROGRAMMER - David Gordon Jan. 2013 
!                    DG, 2016-Jul-06  Added Kjob 
!
!
! Physical constants from IERS Conventions (2003)
!  Revised 2012-NOV-08 FROM IAU 2009 Resolution B2.
          VLIGHT  = 299792458.0D0
          GMSUN   = 1.32712442099D20
          SECPAU  = 499.0047838061D0
          REARTH  = 6378136.6D0
          GMEARTH = 3.986004418D14
          GMMOON  = GMEARTH * .0123000371D0
          EFLAT   = 1.D0/298.25642D0
          GAMMA   = 1.0D0
          AU_meters = 1.49597870700D11
!
! GM's of the planets. From IAU 2009 Resolution B2. 
!  Updated 2012-Nov-08
      GMPLANET(1) = GMSUN / 6.0236D6          ! Mercury
      GMPLANET(2) = GMSUN / 4.08523719D5      ! Venus
      GMPLANET(3) = GMSUN / 3.09870359D6      ! Mars
      GMPLANET(4) = GMSUN / 1.047348644D3     ! Jupiter
      GMPLANET(5) = GMSUN / 3.4979018D3       ! Saturn
      GMPLANET(6) = GMSUN / 2.290298D4        ! Uranus
      GMPLANET(7) = GMSUN / 1.941226D4        ! Neptune
!     GMPLANET( ) = GMSUN / 1.36566D8         ! Pluto   
!
! GM's of the planets. From IERS 2010 and DE421 ephemeris
!     GMPLANET(1) = GMSUN / 6023597.400017D0      ! Mercury
!     GMPLANET(2) = GMSUN / 408523.718655D0       ! Venus
!     GMPLANET(3) = GMSUN / 3098703.590267D0      ! Mars
!     GMPLANET(4) = GMSUN / 1047.348625D0         ! Jupiter
!     GMPLANET(5) = GMSUN / 3497.901768D0         ! Saturn
!     GMPLANET(6) = GMSUN / 22902.981613D0        ! Uranus
!     GMPLANET(7) = GMSUN / 19412.237346D0        ! Neptune
!     GMPLANET(8) = GMSUN / 135836683.767599D0    ! Pluto   
!
!  Compute square and cube of velocity of light. 93MAY06, D. Gordon
      VLIGHT2 = VLIGHT * VLIGHT
      VLIGHT3 = VLIGHT2 * VLIGHT
!
!  Provide for the input and initializations of the model modules and of the
!  necessary utility routines and for the adding to the header of the
!  corresponding text messages.
!
!**    CALL dSTAI()    ! Do we need the Calc control flags?
!
       CALL dSITI(Kjob) ! Get Ocean loading coefficients.
!                       ! Get Ocean pole tide loading coefficients.
!                       ! Get tilt angles. Get Axis types. Compute tilt topo matrices.
!                       ! Compute latitudes and longitudes, and topo matrices. 
!
       CALL dUT1I()    ! Initialize 
!
       CALL dWOBI()    ! Initialize 
!
!   Initialization of spacecraft ephemeris moved to subroutine dScan. 
!!!    If (NumSpace .ge. 1) CALL SPACEI(1)
!
!  Initialize the observation counter to 0.
      KOUNT = 0
!
!
!     Normal conclusion.
      RETURN
      END
!
!*************************************************************************
      SUBROUTINE dSITI(Kjob)
      IMPLICIT None
!
! 3.    SITI
!
! 3.1   SITI PROGRAM SPECIFICATION
!
! 3.1.1 SITI is the Site Module input and initialization section.
!
! 3.1.3 REFERENCES - SMART, W.M., 'TEXTBOOK ON SPHERICAL ASTRONOMY',
!                    1965, P. 195-198
!                    MARKHAM'S X-DOCUMENT
!
! 3.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!            VARIABLES 'FROM':
!              1. EFLAT  - THE FLATTENNING OF THE ELLIPSOID APPROXIMATING
!                          THE SHAPE OF THE EARTH.  (UNITLESS)
!              2. REARTH - THE EQUATORIAL RADIUS OF THE EARTH. (M)
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!
      INCLUDE 'cmxst11.i'
!            Variables to:
!       1. CFRAD(Max_Stat)      -  THE SITE SPHERICAL EARTH RADII.  (M)
!       2. PLAT(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
!                                  FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!                                  GEODETIC LATITUDES. (M/RAD)
!       3. PLON(3,Max_Stat)     -  THE PARTIAL DERIVATIVES OF THE SITE CRUST
!                                  FIXED VECTOR COMPONENTS WITH RESPECT TO THE
!                                  EAST LONGITUDES. (M/RAD)
!       4. SITAXO(Max_Stat)     -  THE SITE ANTENNA AXIS OFFSETS. (M)
!       5. SITOAM(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING AMPLITUDES.
!                                  (M)
!       6. SITOPH(11,Max_Stat)  -  THE SITE VERTICAL OCEAN LOADING PHASES.
!                                  (RAD)
!**     7. SITXYZ(3,Max_Stat)   -  THE SITE CRUST FIXED X, Y, & Z
!**                                COORDINATES. (M, M, M )
!       8. SNRM(3,Max_Stat)     -  THE X, Y, AND Z COMPONENTS OF THE SITE
!                                  NORMAL UNIT VECTORS. (UNITLESS)
!**     9. SITZEN(Max_Stat)     -  THE ZENITH ELECTRICAL PATH LENGTH AT EACH
!**                                OBSERVATION SITE. (SEC)
!      10. TCROT(3,3,Max_Stat)  -  THE ROTATION MATRICES WHICH ROTATE THE
!                                  TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST
!                                  FIXED REFERENCE SYSTEM FOR EACH SITE.
!                                  (UNITLESS)
!      11. XLAT(Max_Stat)       -  THE SITE GEODETIC LATITUDES. (RAD)
!      12. XLON(Max_Stat)       -  THE SITE EAST LONGITUDES. (RAD)
!      13. KTYPE(Max_Stat)      -  THE SITE ANTENNA AXIS TYPES. (UNITLESS)
!      14. NLAST(2)             -  THE INTEGER VARIABLE WHICH DETERMINES IF
!                                  THE BASELINE ID HAS CHANGED FROM ONE
!                                  OBSERVATION TO THE NEXT.
!                                  (NOTE: THE SITE GEOMETRY NEED NOT BE
!                                  RELOADED FOR EACH OBSERVATION IF THE
!                                  BASELINE ID DOES NOT CHANGE. NLAST MUST BE
!                                  INITIALIZED TO ZERO IN THE INITIALIZATION
!                                  SECTION AND PASSED TO THE GEOMETRY SECTION
!                                  SO THAT IT WILL HAVE ZERO VALUES UNTIL
!                                  AFTER THE FIRST OBSERVATION IS PROCESSED.)
!**    15. NUMSIT               -  THE NUMBER OF SITES IN THE SITE CATALOG.
!      16. LNSITE(4,Max_Stat)   -  THE EIGHT CHARACTER SITE NAMES OF THE
!                                  SITES IN THE SITE CATALOG. (ALPHAMERIC)
!      17. SITHOA(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING
!                                  AMPLITUDES. (M)
!      18. SITHOP(11,2,Max_Stat) - THE SITE HORIZONTAL OCEAN LOADING PHASES.
!                                  (RAD)
!      19. HEIGHT(Max_Stat)     -  Height above the geoid. (meters)
!      20. RTROT(3,3,Max_Stat)  -  The rotation matrices which rotate the
!                                  'radial-transverse' reference system to the
!                                  crust fixed reference system for each site.
!                                  (Unitless). The 'radial-transverse' ref.
!                                  system is nearly identical to the
!                                  topocentric system. 'Up' is in the radial
!                                  direction from the center of the Earth;
!                                  'East' is East; and 'North' is perpendicular
!                                  to the radial in the north direction.
!      21. GLAT(Max_Stat)       -  The geocentric latitude at each site. (rad)
!      22. Zero_site            -  The site number of the site at the
!                                  geocenter, if there is one in this data
!                                  set. For correlator usage.
!      23. Dbtilt(Max_Stat,2)   -  Antenna fixed axis tilts, in arc-minutes.
!                                  For alt-az mounts, 1 => East tilt,
!                                  2 => North tilt.
!      24. Rotilt(3,3,Max_Stat) -  Rotation matrices representing the antenna
!                                  fixed axis tilts, in the local topocentric
!                                  station frame. X = Up, Y = East, Z = North.
!      25. OPTL6(6,Max_stat)    -  The site ocean pole tide loading 
!                                  coefficients, interpolated from the Desai
!                                  lat/lon table.
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!              1. KSITC - THE SITE MODULE FLOW CONTROL FLAG.
!              2. KSITD - THE SITE MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'cuser11.i'
!       Variables from:
!         1. Calc_user  - Calc user type. 'A' for Calc/SOLVE analysis.
!                         'C' for VLBI correlator.
!
      INCLUDE 'cmxut11.i'
!        Variables 'to':
!**         1. Intrvl(5,2) - First and last time tag of data in the current
!                            data base. (First index: year, month, day,
!                            hour, minute. Second index: first, last.)
      INCLUDE 'param11.i'
!       Variables from:
!           1. A_tilts   - Antenna tilts file name (default file).
!           2. OPTL_file - Ocean pole tide loading file (default file).
!
      INCLUDE 'd_input.i'
!
!
! 3.2.3 PROGRAM SPECIFICATIONS -
!
      Character*80 xtlt
      CHARACTER*40 C_LSITM(2)
      Character*8 Ch_Sites(Max_Stat)
      REAL*8 XLAT_DUMMY,XLON_DUMMY, RY(3,3), RZ(3,3), TCROT_DUMMY(3,3), &
     &       RGY(3,3), GLAT_DUMMY,RTROT_DUMMY(3,3),Sitxyzv(7,Max_stat), &
     &       T1(3,3), T2(3,3)
      Real*8 xlatlonht(3),Pie,A,Fl,Xyz(3), Xdoy1, Xepoch, X_frac
      Integer*4 N, I, J, K, L, ik, kj, Imdoy(12), Intmov, Krr, Kjob
      Integer*2 KERR(12), LSITM(40), NDO(3), NDI(3), NN, numsit_local,  &
     &                       KERX(2)
      EQUIVALENCE (LSITM,C_LSITM)
      CHARACTER*1 ITEST, idum(3)
      Equivalence (LNSITE(1,1), Ch_Sites(1))
!     Logical*4 Input_sites, Input_ocean
!
      Data Imdoy /0,31,59,90,120,151,181,212,243,273,304,334/
!
! 3.2.6 SUBROUTINE INTERFACE -
!           CALLER SUBROUTINES: INITL
!           CALLED SUBROUTINES: DCOS, DSIN, DSQRT, GETA, GETI,
!                    GET4, TERMINATE_CALC, MMUL2, PUTA, ROTATE, bkplh
!
! 3.2.7 CONSTANTS USED - EFLAT, REARTH
!
! 3.2.8 PROGRAM VARIABLES -
!           1. KERR(12) - THE DATA BASE ERROR RETURN FLAGS.
!           2. NDO(3)   - THE DATA BASE RETURN ARRAY INDICES.
!           3. RY(3,3)  - THE ROTATION MATRIX WHICH PERFORMS A COORDINATE
!                         SYSTEM ROTATION ABOUT THE TOPOCENTRIC Y-AXIS (EAST)
!                         THROUGH AN ANGLE EQUAL TO THE GEODETIC LATITUDE OF
!                         THE CURRENT SITE. (UNITLESS)
!           4. RZ(3,3)  - THE ROTATION MATRIX WHICH PERFORMS A COORDINATE
!                         SYSTEM ROTATION ABOUT THE TOPOCENTRIC Z-AXIS (NORTH)
!                         THROUGH AN ANGLE EQUAL TO THE NEGATIVE EAST LONGITUDE
!                         OF THE CURRENT SITE. (UNITLESS)
!
! 3.2.9 PROGRAMMER - David Gordon    Jan. 2013  
!                    DG, 2016-July-06  Added Kjob
!
! 3.3   dSITI PROGRAM STRUCTURE
!
       Krr = 0
!
!  Fill LNSITE array from SITES array
      Do J = 1, Numsit
       Ch_Sites(J) = Sites(J)
      Enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      Write (6,*) 'dSITI: Sites   ', Sites  
!      Write (6,*) 'dSITI: LNSITE  ', LNSITE 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!***********************************************
!  Set geocenter station flag 
        Zero_site = 1
!***********************************************
!
!  Define axis types:
      Do J = 2, Numsit
       If (AXIS(J) .eq. 'AZEL') KTYPE(J) = 3
       If (AXIS(J) .eq. 'EQUA') KTYPE(J) = 1
       If (AXIS(J) .eq. 'XYNS') KTYPE(J) = 2
       If (AXIS(J) .eq. 'XYEW') KTYPE(J) = 4
       If (AXIS(J) .eq. 'RICH') KTYPE(J) = 5
      Enddo 
! Set GEOCENTR site to avoid problems:
       KTYPE(1) = 3
!
!  External file input for ocean loading
       CALL dOCNIN(Kjob,Krr)
!
!  External file input for antenna tilts
       CALL dANTILT(Kjob,Krr)
!      If (KRR .ne. 0) Then
!       Do I = 1, Max_stat
!        Dbtilt(1,I) = 0.0
!        Dbtilt(2,I) = 0.0
!       Enddo
!      Endif
!
!  External file input for ocean pole tide loading
       CALL dOPTLIN(Kjob,Krr)
!***********************************************
!
!  Compute topocentric rotation matrices for each antenna axis tilt.
       Do I = 1, NUMSIT
!
!           Alt-Az case:
        If (KTYPE(I) .eq. 3) Then
         Call ROTAT(-Dbtilt(1,I)*CONVD/60.D0, int2(3), T1)
         Call ROTAT( Dbtilt(2,I)*CONVD/60.D0, int2(2), T2)
         Call MMUL2(T1, T2, Rotilt(1,1,I))
        Endif
!
!           Equatorial, X/Y N-S, or Richmond case:
        If (KTYPE(I) .eq. 1 .or. KTYPE(I) .eq. 2 .or.     &
     &      KTYPE(I) .eq. 5) Then
         Call ROTAT( Dbtilt(1,I)*CONVD/60.D0, int2(1), T1)
         Call ROTAT(-Dbtilt(2,I)*CONVD/60.D0, int2(2), T2)
         Call MMUL2(T1, T2, Rotilt(1,1,I))
        Endif
!
!           X/Y E-W case:
        If (KTYPE(I) .eq. 4) Then
         Call ROTAT( Dbtilt(1,I)*CONVD/60.D0, int2(1), T1)
         Call ROTAT( Dbtilt(2,I)*CONVD/60.D0, int2(3), T2)
         Call MMUL2(T1, T2, Rotilt(1,1,I))
        Endif
!
       Enddo
!
!-----------------------------------------------------------------------------
!
!
!?    If only one site zenith path delay, copy for all stations.
!?     IF( NDO(1) .NE. NUMSIT ) THEN
!?       DO 210 N = 2,NUMSIT
!?10       SITZEN(N) = SITZEN(1)
!?     ENDIF
!
!
!     Calculate the neccesary site geometry.
!      Mod added 98JAN22: Dummy out topocentric type variables for station at
!      or near the geocenter, for correlator usage.
!
!     Loop once for each station in the site catalog.
      DO 490  N = 1,NUMSIT
!
!   Check for geocenter
         If (Zero_site .eq. N) Go to 491
!
!       Compute the site spherical radii.
        CFRAD(N) = DSQRT ( SITXYZ(1,N)**2  +  SITXYZ(2,N)**2  +         &
     &                     SITXYZ(3,N)**2  )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       If (CFRAD(N) .le. 0.D0) Write(6,1077) N, CFRAD(N), SITXYZ(1,N),  &
     &        SITXYZ(2,N), SITXYZ(3,N)
 1077  Format('SITI: N,CFRAD,SITXYZ(N): 'I2,4F14.4)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   Compute geocentric latitudes
         GLAT(N) = DASIN( SITXYZ(3,N) / CFRAD(N) )
!
!   93OCT12. Call subroutine bkplh to compute
!    geodetic latitude, geodetic longitude, and height. DG
         Pie = PI
         Fl = Eflat
         A = Rearth
         do i=1,3
          XYZ(i)=SITXYZ(i,N)
         enddo
        call bkplh(xyz,xlatlonht,pie,a,fl)
! Keep longitudes between -PI and +PI
         if (xlatlonht(2) .gt. pi) &
     &       xlatlonht(2) = xlatlonht(2) - 2.D0*pi
         XLAT(N)  =  xlatlonht(1)
         XLON(N)  =  xlatlonht(2)
         Height(N) = xlatlonht(3)     ! height in meters
!
!       Compute the site normal unit vectors.
        SNRM(1,N) = DCOS ( XLAT(N) ) * DCOS ( XLON(N) )
        SNRM(2,N) = DCOS ( XLAT(N) ) * DSIN ( XLON(N) )
        SNRM(3,N) = DSIN ( XLAT(N) )
!
!       Compute the partial derivatives of the crust fixed site
!       coordinates with respect to the East longitudes.
        PLON(1,N) = - SITXYZ(2,N)
        PLON(2,N) =   SITXYZ(1,N)
        PLON(3,N) =   0.D0
!
!       Compute the partial derivatives of the crust fixed site
!       coordinates with respect to the geodetic latitudes.
!       (NOTE: The following equations are actually for the geocentric partial
!       derivatives, however, these partials are sufficiently close to the
!       geodetic partials for the purposes of CALC use.)
        PLAT(1,N) = - SITXYZ(3,N) * DCOS (XLON(N) )
        PLAT(2,N) = - SITXYZ(3,N) * DSIN (XLON(N) )
        PLAT(3,N) = + CFRAD(N) * DCOS (XLAT(N) )
!
!     Compute the topocentric-to-crust-fixed rotation matrices by rotating
!     about the geodetic latitude and longitude. Also now compute a
!     "radial-transverse" rotation matrix by rotating about the geocentric
!     latitude and the longitude.
!
        XLAT_DUMMY = XLAT(N)
        CALL ROTAT ( XLAT_DUMMY, int2(2), RY)
!      write(6,8) ' XLAT? ',  xlat(n)*57.29578
!
        XLON_DUMMY = XLON(N)
        CALL ROTAT ( -XLON_DUMMY, int2(3), RZ)
!      write(6,8) ' XLON? ',  xlon(n)*57.29578
!
        GLAT_DUMMY = GLAT(N)
        CALL ROTAT ( GLAT_DUMMY, int2(2), RGY)
!      write(6,8) ' GLAT? ',  glat(n)*57.29578
!
!       DO I=1,3
!         DO J=1,3
!           TCROT_DUMMY(I,J) = TCROT(I,J,N)
!           RTROT_DUMMY(I,J) = RTROT(I,J,N)
!         ENDDO
!       ENDDO
        CALL MMUL2 ( RZ, RY, TCROT_DUMMY(1,1) )
        CALL MMUL2 ( RZ, RGY,RTROT_DUMMY(1,1) )
        DO I=1,3
          DO J=1,3
            TCROT(I,J,N) = TCROT_DUMMY(I,J)
            RTROT(I,J,N) = RTROT_DUMMY(I,J)
          ENDDO
        ENDDO
!      write(6,8) ' TCROT ',  TCROT_DUMMY
!      write(6,8) ' RTROT ',  RTROT_DUMMY
!
      IF (KSITD .ne. 0) Then  !Station debug printout
       if (N.eq.1) Then
        WRITE ( 6, 1)
        WRITE(6,8)' EFLAT   ',EFLAT
        WRITE(6,8)' REARTH  ',REARTH
        WRITE(6,7)' NUMSIT  ',NUMSIT
       endif
    1  FORMAT (1X, 'Debug output for subroutine SITI.' )
       write(6,'(" For site #",i2)') N
       WRITE(6,4)' RY   ',((RY(J,K),J=1,3),K=1,3)
       WRITE(6,4)' RZ   ',((RZ(J,K),J=1,3),K=1,3)
       WRITE(6,4)' RGY  ',((RGY(J,K),J=1,3),K=1,3)
       WRITE (6,8)' Geoid Height  ',  xlatlonht(3)
      Endif          !Station debug printout
      GO TO 490
!
  491 CONTINUE
!    Dummy out the above topocentric quantities, they have no meaning at the
!     geocenter
        XLAT(N)   = -999.D0
        XLON(N)   = -999.D0
        Height(N) = -999.D0
        SNRM(1,N) = 0.D0
        SNRM(2,N) = 0.D0
        SNRM(3,N) = 0.D0
        GLAT(N)   = -999.D0
        PLON(1,N) = 0.D0
        PLON(2,N) = 0.D0
        PLON(3,N) = 0.D0
        PLAT(1,N) = 0.D0
        PLAT(2,N) = 0.D0
        PLAT(3,N) = 0.D0
        SITAXO(N) = 0.D0
        KTYPE(N)  = 0
        SITZEN(N) = 0.D0
        DO I=1,3
         DO J=1,3
          TCROT(I,J,N) = 0.D0
          RTROT(I,J,N) = 0.D0
         ENDDO
        ENDDO
        DO I=1,11
          SITOAM(I,N) = 0.D0
          SITHOA(I,1,N) = 0.D0
          SITHOA(I,2,N) = 0.D0
          SITOPH(I,N) = 0.D0
          SITHOP(I,1,N) = 0.D0
          SITHOP(I,2,N) = 0.D0
        ENDDO
!
!     Close the loop which runs over the sites in the catalog.
  490 CONTINUE
!
!     Initialize the integer variable NLAST to zero.
      NLAST(1) = 0
      NLAST(2) = 0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     WRITE ( 6, 11)
!      Do I = 1, NUMSIT
!        Write(6,1012) I, Dbtilt(1,I), Dbtilt(2,I),                     &
!    &     ((Rotilt(kj,ik,I), ik=1,3), kj=1,3)
!      Enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Check KSITD for debug output.
      IF ( KSITD .ne. 0 ) Then  !Debug printout
      WRITE ( 6, 11)
   11 FORMAT (1X, 'Station Debug for subroutine dSITI.' )
      WRITE(6,8)' CFRAD   ',(CFRAD(J),J=1,NUMSIT)
    8 FORMAT(A,4D25.16/(1X,5D25.16))
      WRITE(6,7)' KTYPE   ',(KTYPE(J),J=1,NUMSIT)
    7 FORMAT(/,A,15I3/(1X,15I3))
      WRITE(6,7)' NLAST   ',NLAST
      WRITE(6,4)' PLAT    ',(( PLAT(J,K),J=1,3),K=1,NUMSIT)
    4 FORMAT(/,A,3D25.16/(9X,3D25.16))
      WRITE(6,4)' PLON    ',(( PLON(J,K),J=1,3),K=1,NUMSIT)
      WRITE(6,8)' SITAXO  ',( SITAXO(J),J=1,NUMSIT)
      WRITE(6,9)' SITOAM, ',((SITOAM(J,K),J=1,11),K=1,NUMSIT)
      WRITE(6,9)' SITOPH, ',((SITOPH(J,K),J=1,11),K=1,NUMSIT)
      WRITE(6,9)' SITHOA, ',(((SITHOA(J,L,K),J=1,11),L=1,2),K=1,        &
     & NUMSIT)
      WRITE(6,9)' SITHOP, ',(((SITHOP(J,L,K),J=1,11),L=1,2),K=1,        &
     & NUMSIT)
    9 FORMAT(/,A,11F9.4,/,(9X,11F9.4))
      WRITE(6,6)' SITXYZ  ',((SITXYZ(J,K),J=1,3),K=1,NUMSIT)
    6 FORMAT(/,A,3F20.4,/,(9X,3F20.4))
      WRITE(6,8)' SITZEN  ',(SITZEN(K),K=1,NUMSIT)
      WRITE(6,4)' SNRM    ',((SNRM(I,J),I=1,3),J=1,NUMSIT)
      WRITE(6,4)' TCROT   ',(((TCROT(I,J,K),I=1,3),J=1,3),K=1,          &
     & NUMSIT)
    5 FORMAT(/,A,/,3(3F20.4,/)/)
      WRITE(6,8)' XLAT    ',(XLAT(J),J=1,NUMSIT)
      WRITE(6,8)' XLON    ',(XLON(J),J=1,NUMSIT)
      WRITE(6,8)' HEIGHT    ',(HEIGHT(J),J=1,NUMSIT)
      WRITE(6,*)' LNSITE: '
      WRITE(6,3)  ((LNSITE(J,K),J=1,4),K=1,NUMSIT)
    3 FORMAT (8(2X,4A2))
       Do I = 1, NUMSIT
         Write(6,1012) I, Dbtilt(1,I), Dbtilt(2,I),                     &
     &     ((Rotilt(kj,ik,I), ik=1,3), kj=1,3)
 1012    Format('Station #',I2,2x,2F10.5,/,'Rotilt: ',3F20.10,          &
     &          /,8X,3F20.10,/,8X,3F20.10)
       Enddo
!
!
      Endif          !Debug printout
!
!     Normal conclusion.
      RETURN
      END
!
!*************************************************************************
      SUBROUTINE dUT1I()
      IMPLICIT None
!
!     UT1I is the UT1 module input and initialization section.
!     The input UT1 table MUST be a 1-day
!     series of 'TAI-UT1'. Tidal terms should not be removed!
!
!     Common blocks used -
!
      INCLUDE 'cmxut11.i'
!            Variables 'from':
!              1. IEPOCH   - The number of epochs at which TAI - UT1 is desired.
!              2. ASKKER   - The database error return code from ASK.
!              3. Leap_fix - Used in external input mode. .True. means
!                            correct the input EOP series for accumluated
!                            leap seconds. .False. means do not correct.
!              4. UT1type  - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '.
!                            For 'UT1-UTC ', leap second corrections
!                            must be made.
!              5. EOP_time_scale - EOP table time scale, allowed values:
!                            'TAI     ', 'TCG     ', 'TDB     ',
!                            'TDT     ', 'UTC     ', 'UNDEF   '.
!
!            Variables 'to':
!              1. UT1IF(4)  - The final UT1 information array. This array
!                             contains respectively: 1) The Julian date of the
!                             first tabular point, 2) The increment in days of
!                             the tabular points, 3) The number of tabular
!                             points, 4) The units of the UT1 tabular array per
!                             second. (days, days, unitless, sec/table unit)
!              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
!                             (table units)
!              3. UT1RS(20) - The table of either 'TAI-UT1' (default 
!                             beginning with Calc 10) or 'TAI-UT1S'
!                             depending on the value of KUT1C. For
!                             'TAI-UT1S', the tidal terms are removed 
!                             using the model of Defrainge and Smits, 1999,
!                             from the IERS Conventions (2003)]. (seconds)
!              4. ISHRTFL   - The short period tidal terms flag, (unitless).
!                             = 1 --> UT1 table coming from input database is
!                             true UT1, (that is, fortnightly tidal terms have
!                             not been removed, as in the IERS series).
!                             [This is the normal case.]
!                             = -2 --> UT1 table coming from input database
!                             is UT1S (tidal terms removed).
!                             This mode no longer supported and should not
!                             be used. It will cause Calc 10 to abort!!!!
!                             = -1 --> UT1R (old Yoder fortnightly terms
!                             removed). This mode no longer supported and
!                             will cause Calc to abort!!!!!
!             5. Usecubic   - Set to true if cubic interpolation to be used.
!             6. Uselinear  - Set to true if linear interpolation to be used.
!             7. Usespline  - Set to true if spline interpolation to be used.
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!             1. KUT1C - UT1 module flow control flag, controls the
!                        temporary removal of periodic terms (UT1S)
!                        and the type of interpolation (spline or cubic)
!                        in the UT1 tables. Revised Sept. 2005.
!                        = 0. Leave table as TAI-UT1. Do spline 
!                           interpolation for a 1-day series. 
!                           5-day series no longer allowed!
!                        = 1. Module completely off, that is, UT1 set equal
!                           to AT.
!                        = 2. Use TAI-UT1; use cubic interpolation for a
!                           1-day series. 5-day series no longer allowed!
!                        = 3. Use TAI-UT1; use linear interpolation for a
!                           1-day series. 5-day series no longer allowed!
!                        = 4. Convert table to TAI-UT1S. Do spline 
!                           interpolation for a 1-day series, then 
!                           restore to true UT1 using the UT1S model of
!                           Defrainge and Smits, 1999. 
!                           5-day series no longer allowed!
!             2. KUT1D - The UT1 module debug output flag.
!
      INCLUDE 'd_input.i'
!           1. Xleap_sec - Number of leap seconds from the .calc file
!                          in difx mode.
!
      INCLUDE 'cuser11.i'
!           1. C_mode - Calc 'mode'. Either 'difx  ' or 'mark3 ' or
!                       'nusolv', etc.
!
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
!           VARIABLES 'TO':
!            1. ATMUTC(3)   - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
!                             CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
!                             RATE OF CHANGE OF 'TAI MINUS UTC'. Used in the
!                             atomic time module. (DAYS, SEC, SEC/SEC)
!            2. ROTEPH(2,20)- The array which contains the epochs at which
!                             TAI - UT1 is desired. The entries are:
!                             1) JD at 0:00 hours UTC,
!                             2) The fraction of a UTC day from 0:00 hours
!                                to the desired epoch.
!            3. A1UTC(3)    - A1 - UTC array ???? 
!            4. A1DIFF(3)   - THE ARRAY CONTAINING THE EPOCH, OFFSET AND RATE
!                             OF CHANGE OF THE OFFSET BETWEEN A1 AND TAI
!                             (DAYS, SECONDS, SECONDS/DAY)
!
!     Program Specifications -
!
      REAL*8     UT1TAB(2,20)
      REAL*8     XJD, CT, TC2000, FA2K(14), FAD2K(14), TT0
      Real*8     yp1, ypn, xleap(5), tol, xntv
      Real*8     Dut, Dlod, Domega, Dutr, Dlodr, Domegar, Duts, Dlods, &
     &           Domegas, Atmut1, Shortp, Divutc, tab_time
      INTEGER*4  get_leapsec, ierr, ierr4
      INTEGER*4  Increment, max_ut1_pts
      Integer*4  N, Itab, II, I
      INTEGER*2  KERR(8), NDO(3), Tab_len
      INTEGER*2  LUT1M(40), LOFF(40), LUT1S(40), LUT1(40)
      CHARACTER*40 C_LOFF(2), C_LUT1S(2), C_LUT1M(2), C_LUT1(2)
      Equivalence (LUT1S,C_LUT1S), (C_LUT1M,LUT1M), (LOFF,C_LOFF ), &
     &            (LUT1,C_LUT1)
!
      Character*20 C_LFI, C_LPR, C_LEX
      Integer*2    LFI(10), LPR(10), LEX(10)
      Equivalence (C_LFI,LFI), (C_LPR,LPR), (C_LEX,LEX)
!
      Integer*2    Lutext(40)
      Character*80 Utext
      Equivalence (Utext,Lutext)
!
      Integer*2    mess_linear(30), mess_cubic(30), mess_spline(3)
      Character*60 mess_linear_c , mess_cubic_c, mess_spline_c
      Equivalence (mess_linear,mess_linear_c),(mess_cubic,mess_cubic_c), &
     &            (mess_spline,mess_spline_c)
      Character*1 type_found, cdum1(3)
!
      Integer*2    L_EOPSCALE(4)
      Character*8  C_EOPSCALE
      Equivalence (L_EOPSCALE, C_EOPSCALE)
!
      Integer*2 Mxepch, MEPOCH
      DATA MXEPCH /20/
!     DATA C_LUT1M / &
!    & 'UT1 Module - Last Modified 2005.09.23, D', &
!    & '. Gordon/GSFC:                          '/
!
!     DATA C_LUT1   / &
!    & 'UT1 Module ON. Interpolation in TAI-UT1.', &
!    & '                                        '/
!
!     DATA C_LUT1S  / &
!    & 'UT1 Module ON. Tidal terms restored usin', &
!    & 'g Defraigne and Smits 1999 (IERS 2003). '/
!
!     DATA C_LOFF / &
!    & 'UT1 Module is turned OFF.               ', &
!    & '                                        '/
!
!     DATA  C_LFI /' Final Values       '/
!     DATA  C_LPR /' Preliminary Values '/
!     DATA  C_LEX /' Extrapolated Values'/
!
!     Data mess_cubic_c  / &
!    & 'UT1 table interpolated with 4 point, 3rd order polynomial.  '/
!
!     Data mess_linear_c / &
!    & 'UT1 table interpolated with 2 point, linear interpolation.  '/
!
!     Data mess_spline_c / &
!    & 'UT1 table interpolated with 15 point cubic spline.          '/
!
      Data Max_ut1_pts /20/
!
!*2.2.4 DATA BASE ACCESS -
!**         'GET' VARIABLES:
!**            1. UT1IF(4)   -   The UT1 information array. (See above.)
!**            2. UT1PT(20)  -   The tabular values of 'TAI minus UT1'.
!**                              (table units)
!**            3. ROTEPH(2,20) - The array which contains the epochs at which
!**                              TAI - UT1 is desired. The entries are:
!**                              1) JD at 0:00 hours UTC,
!**                              2) The fraction of a UTC day from 0:00 hours
!**                              to the desired epoch.
!**            4. ISHRTFL    -   The short period tidal terms flag. (See above.)
!**         'PUT' Variables:
!**            1. LUT1M(40)    - The UT1 module text message.
!**            2. LON(40)      - The UT1 module 'turned on' message.
!**            3. LOFF(40)     - The UT1 module 'turned off' message.
!**            4. UT1TAB(2,20) - The array which contains the TAI - UT1 
!**                              values (complete) and the fortnightly 
!**                              corrections to TAI - UT1 (s,s).
!**         Access codes:
!**            1. 'UT1 MESS'  -  UT1 module text message code.
!**            2. 'FUT1 INF'  -  The access code for the Final UT1 
!**                              information array.
!**            3. 'FUT1 PTS'  -  Final UT1 points access code.
!**            4. 'PUT1 INF'  -  Like above but preliminary.
!**            5. 'PUT1 PTS'  -  Like above but preliminary.
!**            6. 'UT1 CFLG'  -  Module control flag access code.
!**            7. 'EUT1 INF'  -  Like above but for extropolated data.
!**            8. 'EUT1 PTS'  -  Like above but for extropolated data.
!**            9. 'UT1EPOCH'  -  Access code for the UT1 epochs array.
!**           10. 'ROTEPOCH'  -  Access code for the array of epochs at which
!**                              TAI - UT1 is desired.
!**           11. 'TIDALUT1'  -  The access code for the flag indicating
!**                              whether the fortightly terms in UT1 are in
!**                              the input UT1 series. (See below.)
!
!     Subroutine Interface -
!             Caller subroutines: INITL
!             Called subroutines: GETI, GET4, TERMINATE_CALC, PUTA, PUT4,
!                                 NUTFA, UT1RZT, UT1SZT, spline
!
!     Program variables:
!           1. Tab_len     - The number of tabular points in the UT1
!                            information array in the database
!           2. KERR(8)     - The database error return flags.
!           3. LUT1F(4)    - The message inserted into the UT1 module text
!                            message if the final UT1 information are used.
!           4. LUT1P(4)    - The message if preliminary data is used.
!           5. NDO(3)      - Database return array indices.
!           6. LUT1E(3)    - The message if extropolated data is used.
!           7. UT1TAB(2,20)- THe array which contains the values of TAI - UT1
!                            and the short period corrections. (s,s).
!           8. MEPOCH      - This is IEPOCH checked to be certain that it is no
!                            greater than MXEPCH.
!           9. MXEPCH      - The maximum number of TAI - UT1 EPOCHS allowed.
!          10. XJD         - The Julian date at 0 hours UTC of the epoch in
!                            question (days).
!          11. UTC         - The UTC fraction of day of the observation. (days)
!          12. ATMUT1      - The complete computed value of TAI - UT1 (s)
!          13. SHORTP      - 'UT1S - UT1' depending on KUT1C.
!                            (s). Notice the sense!
!          14. Increment   - The increment of the UT1 table in the database
!                            (days).
!          15. max_ut1_pts - The maximum size of the UT1 table from the
!                            database.
!          16. type_found  - Tracks the type of UT1 table in the database.
!
!     Programmer - Dale Markham   02/14/77
!      77.07.14  Peter Denatale
!      78.03.28  Bruch Schupler
!      78.07.03  Bruce Schupler
!      84.06.05  David Gordon
!      87.06.03  Savita Goel   CDS FOR A900
!      88.12.21  Gregg Cooke   CALC 7.0 mods.
!      89.06.08  Jim Ryan      Short period logic modified.
!      89.07.11  Jim Ryan      Documentation simplified.
!      89.12.12  Jim Ryan      UNIX-like database interface implimented.
!      90.11.30  Jim Ryan      Bug in short period UT1 control logic fixed
!                              again.
!      91.05.30  Jim Ryan      Documentation furthur simplilfied.
!      91.06.05  Jim Ryan      Mods to support linear interpolation.
!      91.06.19  Jim Ryan      Code added to construct UT1R table from UT1
!                              table. Needed for linear interpolation. For CALC
!                              7.4+ the scaling law for the UT1 table MUST be
!                              1, because SOLVE doesn't know about that law.
!      93.03.17  David Gordon  Code added to use UT1S, new control flag scheme.
!      93.09.07  Norbert Zacharias  Moved calculation of fundamental argumuments
!                              to subroutine NUTFA.
!      93 Dec.   David Gordon  Cubic spline interpolation added, modified
!                              control flag scheme for type of interpolation.
!      93.12.30  David Gordon  Cleaned up 'UT1 MESS'.
!      94.04.06  David Gordon  Changed to 'Implicit None'.
!      94.09.26  David Gordon  Added some debug printout, changed some I*2's to
!                              I*4's, documentation corrections, cosmetic mods.
!      94.09.27  David Gordon  Removed unused 'XLOVEK' Love number.
!      95.06.08  David Gordon  Corrected debug printout, wrong format statement
!                              used.
!      98.01.27  David Gordon  Removed Yoder (UT1R) model interpolation option.
!                              KUT1C values redefined. TERMINATE_CALC if no
!                              TIDALUT1
!                              Lcode (old database, UT1R assumed) or if ISHRTFL
!                              equals -1.
!      98.04.l3  David Gordon  Common /UT1CM/ moved to 'cmxut.i' include file.
!      98.05.01  David Gordon  Mods for external file input of EOP info.
!     2001.01.02 David Gordon  CT replaced with tab_time in UT1MU argument.
!                              Code added to PUT 'EOPSCALE'.
!      2002.09    Jim Ryan     Integer*2/4 mods.
!     2003-2004  David Gordon  Updated for IERS Conventions (2003).
!      2005.10   David Gordon  Change default to interpolate in 'TAI-UT1',
!                              i.e. Do NOT remove fortnightly, etc terms.
!                              Only for KUTC1=4 are the fortnightly terms
!                              removed/restored.
!
!     UT1I Program Structure
!
!----------------------------------------------------------------------------
!
      If(DABS(UT1IF(4)-1.D0) .gt. .00001D0) Then
        Write(6,'( &
     &  "In UT1I: The scaling law for UT1 table must be 1.0! ",/, &
     &  "It is not.  Quitting!")')
        Call TERMINATE_CALC('UT1I  ', int2(0), int2(0))
      Endif
!
      Increment = UT1IF(2) + .01
      Tab_len   = UT1IF(3) + .01
!
      If(Tab_len .gt. Max_ut1_pts) Then
        Write(6,'( &
     &  "The maximum allowable UT1 table is ",I5," points.",/, &
     &  "The table in the database contains ",I5," points.", &
     &  "Quitting.")') max_ut1_pts, Tab_len
        Call TERMINATE_CALC( 'UT1I  ', int2(0), int2(0))
      Endif
!
!  Get the EOP timescale definition. If not present, we call it undefined,
!   but assume the EOP table epochs to be in TDB (= CT).
        If(KERR(1).ne.0)  C_EOPSCALE = 'UNDEF   '
       EOP_time_scale = 'UNDEF   '
!
!
      Increment = UT1IF(2) + .01
      Tab_len   = UT1IF(3) + .01
!
          xntv = xintv(1)
!       WRITE ( 6, * )  ' UT1I/xintv: ', xintv
!   Get leap seconds, then load TAI - UTC array.
      If (C_mode .eq. 'difx  ') Then 
        ATMUTC(1) = UT1IF(1)
        ATMUTC(2) = Xleap_sec
        ATMUTC(3) = 0.0D0 
!       WRITE (6,*)  ' ATMUTC: ', ATMUTC
      Else
       ierr = get_leapsec(xintv(1),xleap)
!       WRITE ( 6, * )  ' UT1I/xintv: ', xintv
!       WRITE ( 6, * )  ' UT1I/xleap: ', xleap
!      If (ierr .ne. 0) go to ????
       ATMUTC(1) = xleap(1)
       ATMUTC(2) = xleap(2)
       ATMUTC(3) = xleap(4)
       tol = 1.D-8
!??    If (Dabs(ATMUTC(3)).gt.tol) ATMUTC(1) = xleap(3) + 2400000.5D0
       ATMUTC(3) = ATMUTC(3) / 8.64D4
!       WRITE (6,*)  ' ATMUTC: ', ATMUTC
      Endif
!
!  Remove leap seconds if input table was UT1-UTC
!*     If (Leap_fix .eq. 'True') Then
!
!*       WRITE ( 6, * )  '!!! Removing leap seconds !!!'
!
!*      Do I = 1,Tab_len
!*       UT1PT(I) = UT1PT(I) + ATMUTC(2)
!*      Enddo
!*     Endif
!
       Do I = 1,3
        A1utc(i) = ATMUTC(i)
       Enddo
        A1utc(2) = a1utc(2) + 0.03439D0
!
       A1DIFF(1) = ATMUTC(1)
       A1DIFF(2) =  0.03439D0
       A1DIFF(3) =  0.0D0
!
!
!----------------------------------------------------------------------------
!  Mod 2005.09.23. Allow use of 1-day series only. There is no longer
!   any reason to use a 5-day series.
       IF (Increment .NE. 1) THEN
        Write(6,'("UT1I: The UT1 table increment must be 1 day! ",/, &
     &  "Instead it is ",I3," days. Calc is quitting!")') Increment
        Call TERMINATE_CALC('UT1I  ', int2(0), int2(0))
       ENDIF
!
!  Determine interpolation method. Options are cubic spline or 
!    cubic polynomial for a 1-day series.
          Usespline = .true.
!
!----------------------------------------------------------------------------
!
!  Construct the table of TAI-UT1 (note the sense).
!    Default: leave tables as true TAI-UT1 
       Do Itab = 1, Tab_len
          UT1RS(Itab) = UT1PT(Itab) 
       Enddo
!
!---------------------------------------------------------------------
!   Code for spline interpolation initialization, 93DEC08  -DG-
      If (Usespline) Then       ! Initialize spline routine
       Nspline = tab_len
!
       Do ii=1,Nspline
        Ya(ii) = UT1RS(ii)
       Enddo
!
       Do ii = 1, Nspline
        XT(ii) =  UT1IF(1) + (ii-1)*UT1IF(2)
       Enddo
!
!   Take first derivatives at endpoints
       yp1 = (ya(2)-ya(1)) / UT1IF(2)
       ypn = (ya(Nspline)-ya(Nspline-1))/ UT1IF(2)
!
!  Call spline initialization subroutine
       CALL SPLINE(XT,ya,Nspline,yp1,ypn,y2s,ierr4)
!
      Endif                      ! Initialize spline routine
! ***************
!
!
!     If there is a ROTEPH array already in the database, get it.
!     Otherwise bypass this step. Also, protect yourself in case
!     there are more than MXEPCH (currently 20) entries in ROTEPH.
!
!    ?????
        MEPOCH = ndays
!    ?????
!??   IF (.not. Input_EOP) THEN   ! Already have ROTEPH?
!??     IF (ASKKER .NE. 0) GO TO 400
!??     MEPOCH = IEPOCH
!??     IF (MEPOCH .GT. MXEPCH) MEPOCH = MXEPCH
!??     CALL GET4 ('ROTEPOCH      ',ROTEPH,int2(2),MEPOCH,int2(1),NDO, &
!??  &   KERR(8))
!??     IF (KERR(8).NE.0) CALL TERMINATE_CALC('UT1I  ',int2(8),KERR(8))
!??   ENDIF                       ! Already have ROTEPH?
!
!     Compute TAI - UT1 and the short period tidal correction for the
!     desired epochs and place them in UT1TAB.
!??   DO  N=1,MEPOCH
!??     XJD = ROTEPH(1,N)
!??     CT  = ROTEPH(2,N)
!??     TT0 = ROTEPH(2,N)
!??     tab_time  = ROTEPH(2,N)
!
!??     If(KUT1C.ne.1) Then
!??       CALL NUTFA (XJD, TT0, CT, TC2000, FA2K, FAD2K)
!??       CALL UT1MU (XJD,tab_time,FA2K,FAD2K,TC2000,ATMUT1, &
!??  &              SHORTP,DIVUTC)
!??     Else
!??       ATMUT1 = 0.D0
!??       SHORTP = 0.D0
!??     Endif
!
!??     UT1TAB(1,N) = ATMUT1
!??     UT1TAB(2,N) = SHORTP
!??   ENDDO
!
!     PUT the UT1TAB array into the database.
!??   CALL PUT4 ('UT1EPOCH      ', UT1TAB, int2(2), MEPOCH, int2(1))
!
!     Go here if we are bypassing the updating of UT1TAB.
  400 CONTINUE
!
!     Check KUT1D for debug output.
    9 FORMAT (1X, "Debug output for subroutine dUT1I.")
    7 FORMAT(A,15I8,/,(9X,15I8))
    8 FORMAT(A,4D25.16,/,(9X,4D25.16))
      IF ( KUT1D .ne. 0 ) Then
       WRITE ( 6, 9)
       WRITE(6,8)' UT1IF   ', UT1IF
       WRITE(6,8)' UT1PT   ', UT1PT
       WRITE(6,8)' UT1RS   ', UT1RS
       If (Usespline) Write(6,8)' XT      ', XT
       If (Usespline) Write(6,8)' ya      ', ya
       If (Usespline) Write(6,8)' y2s     ', y2s
       If (Usespline) Write(6,8)' yp1, ypn', yp1, ypn
       If (Usespline) Write(6,7)' Nspline, ierr4 ', Nspline, ierr4
       WRITE(6,8)' UT1TAB  ', UT1TAB
       WRITE(6,*)' tab_len ', tab_len
       WRITE(6,7)' IEPOCH  ', IEPOCH
       WRITE(6,7)' MEPOCH  ', MEPOCH
       WRITE(6,7)' MXEPCH  ', MXEPCH
       WRITE(6,7)' ASKKER  ', ASKKER
!
      ENDIF
!
!     Normal conclusion.
      RETURN
      END
!*************************************************************************
      SUBROUTINE dWOBI()
      Implicit None
!
!     WOBI is the wobble module input and initialization section.
!
!     Common blocks used -
!
      INCLUDE 'cmwob11.i'
!            Variables 'from':
!              1) KERASK - The database return code from the 'ASK' for
!                          the rotation epochs array.
!              2) NEPOCH - The number of epochs in the rotation epochs array.
!            Variables 'to':
!              1. WOBIF(3)  -  The wobble information array. Contains
!                              respectively: 1) The Julian date of the first
!                              tabular point, 2) The increment in days of the
!                              tabular points, 3) The number of tabular points.
!                              (days, days, unitless)
!              2. XYWOB(2,20)- The wobble tabular points for the polar motion
!                              (wobble) X & Y offsets. (milliarcsec)
!                              (Note: Based on old BIH conventions, offsets
!                              are assumed to be left-handed.)
!
      INCLUDE 'cmxut11.i'
!            Variables 'from':
!              1. Ndays - number of days in the ROTEPH array.
!
      INCLUDE 'ccon.i'
!            Variables 'from':
!              1.  KWOBC  -  The Wobble Module flow control flag.
!                            0 --> Default, module on; spline interpolation
!                                  for 1-day series, cubic for 5-day series.
!                            1 --> Module off. No polar motion applied.
!                            2 --> Module on; linear interpolation for any
!                                  series.
!                            3 --> Module on; cubic interpolation for 1-day
!                                  series, spline for 5-day series.
!              2.  KWOBD  -  The Wobble Module debug output flag.
!
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
!           VARIABLES 'TO':
!            2. ROTEPH(2,20)- The array which contains the epochs at which
!                             TAI - UT1 is desired. The entries are:
!                             1) JD at 0:00 hours UTC,
!                             2) The fraction of a UTC day from 0:00 hours
!                                to the desired epoch.
!
!   Program specifications -
!
      Real*8 yp1x,ypnx,yp1y,ypny
      Integer*4 ierr4
      INTEGER*2  KERR(7), NDO(3), Increment, Tab_len
      INTEGER*2      LWOBM(40)
      CHARACTER*40 C_LWOBM(2)
      EQUIVALENCE (C_LWOBM,LWOBM)
      INTEGER*2      LON(40) ,   LOFF(40)
      CHARACTER*40 C_LON(2)  , C_LOFF(2)
      EQUIVALENCE(C_LON,LON),(C_LOFF,LOFF)
      REAL*8 WOBTAB(2,20), XJD, UTC, WOBXL, WOBYL, tab_time, &
     &       DWOBXL, DWOBYL
      Integer*2    mess_linear(30), mess_cubic(30), mess_spline(30)
      Character*60 mess_linear_c , mess_cubic_c, mess_spline_c
      Equivalence(mess_linear,mess_linear_c),(mess_cubic,mess_cubic_c), &
     &           (mess_spline,mess_spline_c)
!     logical*4 table_found
      Character*1 type_found, cdumm(3)
!
      Character*20 C_LFI, C_LPR, C_LEX
      Integer*2 LFI(10), LPR(10), LEX(10)
      Equivalence (C_LFI,LFI), (C_LPR,LPR), (C_LEX,LEX)
      Integer*2 MEPOCH, mdum1
      Integer*4 MXEPCH, N, II
!
      Integer*2 lwtext(40)
      Character wtext*80
      Equivalence  (wtext,lwtext)
!
      DATA LEN_WOB_TABLE /20/
      data roteph /40*0.0d0/
      data wobtab /40*0.0d0/
!
!*    DATA C_LWOBM / &
!*   &'Wobble Module - Last Modified 2004.04.21', &
!*   &', D. Gordon/GSFC:                       '/
!
      DATA  C_LFI /' Final Values       '/
      DATA  C_LPR /' Preliminary Values '/
      DATA  C_LEX /' Extrapolated Values'/
!
!*    DATA C_LON   / &
!*   &'Wobble Module is turned on.             ', &
!*   &'                                        '/
!
!*    DATA C_LOFF  / &
!*   &'Wobble Module is turned off.            ', &
!*   &'                                        '/
!
!*    Data mess_cubic_c  / &
!*   &'Polar motion table interp. w. 4 pt, 3rd order polynomial.   '/
!
!*    Data mess_linear_c/ &
!*   &'Polar motion table interp. w. 2 pt, linear interpolator.    '/
!
!*    Data mess_spline_c / &
!*   &'Polar motion table interpolated with 5 point cubic spline.  '/
!
      DATA MXEPCH /20/
!
!*   Database access  -
!*          'GET' Variables:
!*            1. WOBIF(3)   -  The wobble information array. Contains
!*                             respectively: 1) The Julian date of the first
!*                             tabular point, 2) The increment in days of the
!*                             tabular points, 3) The number of tabular points.
!*                             (days, days, unitless)
!*            2. XYWOB(2,20) - The tabular points for the long period
!*                             wobble X & Y-offsets. (milli-arc-sec)
!*                             (NOTE: in old BIH convention UT1, X-pole, Y-pole
!*                             system is left handed.)
!*            3. ROTEPH(2,20)- The epochs at which the interpolated WOBBLE are
!*                             desired. The first entry is the Julian date at
!*                             0:00 hours UTC of the day desired. The second
!*                             entry is the UTC fraction of the UTC day of the
!*                             desired epoch (days,days)
!*          'PUT' Variables:
!*            1. LWOBM(40)    - The Wobble Module text message.
!*            2. LON(40)      - The Wobble Module 'TURNED ON' message.
!*            3. LOFF(40)     - The Wobble Module 'TURNED OFF' message.
!*            4. WOBTAB(2,20) - The array which contains the interpolated
!*                              wobble in a left-handed system. The first entry
!*                              is the X wobble. The second entry is the Y
!*                              wobble. (milliarcsec, milliarcsec)
!*          ACCESS CODES:
!*            1. 'WOB MESS'  -  The database access code for the Wobble Module
!*                              text message.
!*            2. 'FWOB INF'  -  The database access code for the final wobble
!*                              information array.
!*            3. 'FWOBX&YT'  -  The database access code for the final wobble
!*                              X & Y offsets.
!*            4. 'PWOB INF'  -  The database access code for the preliminary
!*                              wobble information array.
!*            6. 'PWOBX&YT'  -  The database access code for the preliminary
!*                              wobble X & Y offsets.
!*            7. 'WOB CFLG'  -  The database access code for the Wobble Module
!*                              flow control message.
!*            8. 'ROTEPOCH'  -  The database access code for the epochs at
!*                              which the interpolated values are computed.
!*            9. 'WOBEPOCH'  -  The database access code for the interpolated
!*                              wobble array.
!*           10. 'EWOB INF'  -  The database access code for the extrapolated
!*                              wobble information array.
!*           11. 'EWOBX&YT'  -  The database access code for the extrapolated
!*                              wobble X & Y offsets.
!*           12. 'WOBINTRP'  -  The database access code for the polar motion
!*                              interpolation message.
!
!     Subroutine interface -
!             Caller subroutines: INITL
!             Called subroutines: GET4, TERMINATE_CALC, PUTA, PUT4, WOBMU
!
!     Program Variables -
!           1. Increment -  The increment of the wobble table. (days)
!           2. KERR(7)   -  Database error return flags.
!           3. NDO(3)    -  Database return array indices.
!           4. MXEPCH    -  The maximum number of interpolated wobble epochs.
!           5. WOBXL     -  The interpolated X-wobble (left-handed).
!                           (milliarcsec)
!           6. WOBYL     -  The interpolated Y-wobble (left-handed).
!                           (milliarcsec)
!           7. DWOBXL    -  The time derivative of the interpolated X-wobble.
!                           (left-handed) (milliarcsec/sec)
!           8. DWOBYL    -  The time derivative of the interpolated Y-wobble.
!                           (left-handed) (milliarcsec/sec)
!           9. MEPOCH    -  The number of epochs at which the wobble
!                           is interpolated.
!          10. XJD       -  The Julian date at 0:00 hours UTC of the
!                           interpolation epoch. (days)
!          11. UTC       -  The UTC fraction of the UTC day of the
!                           interpolation epoch. (days)
!          12. Tab_len   -  The length of the Wobble table in the database.
!          13. type_found-  Tracks the type of wobble table in database.
!          14. tab_time  -  The fraction of the day of the interpolation epoch
!                           in WOBMU. (days)
!
! 3.2.9 PROGRAMMER - DALE MARKHAM  02/17/77
!     77.07.14 Peter Denatale
!     78.01.11 Bruce Schupler
!     89.07.26 Jim Ryan  Documentation simplifed and strings.
!     89.08.16 Jim Ryan  Use of LEN_WOB_TALBE implimented.
!     89.12.12 Jim Ryan  UNIX-like database interface implimented.
!     90.06.05 Jim Ryan  Mods for linear interpolation and more clean up.
!     92.07.17 Jim Ryan  Roteph and Wobtab initialized to avoid debug problem.
!     93.12.17 D. Gordon Spline interpolation added, new flow control logic.
!     93.12.30 D. Gordon Cleaned up 'WOB MESS'.
!     94.04.15 D. Gordon Converted to Implicit None.
!     94.05.23 D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
!                        being dimensioned both Logical*2 and Real*8.
!     95.10.05 D. Gordon Skip interpolation if module OFF (KWOBC=1); set
!                        WOBXL and WOBYL equal to zero (previously undefined).
!     95.12.04 D. Gordon Interpolation epoch variable changed from UTC to
!                        tab_time.
!     95.12.11 D. Gordon Adding DWOBXL and DWOBYL, derivatives of X-wobble and
!                        Y-wobble, to WOBMU argument list. Not used here.
!     98.05.01 D.Gordon  Put Common /WOBCM/ into include file 'cmwob.i'.
!                        Added include files 'inputs.i' and 'cmxut.i', and
!                        common block EOPCM. Extensive mods for external
!                        EOP input.
!     99.10.27 D.Gordon  Corrected error in ADD/PUT of 'WOBEPOCH' when
!                        external input being used.
!     2002 Sept Jim Ryan Integer*2/4 mods.
!
!     WOBI Program Structure
!
!
      Increment = WOBIF(2) + 0.01
      Tab_len   = WOBIF(3) + 0.01
! ---------------------------------------------------------------------------
!
!  Determine interpolation method. Defaults are cubic spline for a 1-day series.
        use_spline = .true.
!
!   Code for spline interpolation initialization, 93DEC17  -DG-
      If (use_spline) Then       ! Initialize spline routine
!
      n_spline = tab_len
      do ii=1,n_spline
       yax(ii) = XYWOB(1,ii)
       yay(ii) = XYWOB(2,ii)
      enddo
!
      xa(1) = WOBIF(1)
      do ii=2,n_spline
       xa(ii) = xa(ii-1) + WOBIF(2)
      enddo
!
!   If interval (WOBIF(2)) not 1.0 days, then divide by interval
      if ( Abs(WOBIF(2) - 1.D0) .gt. 1.D-10) then
        do ii=1,n_spline
         xa(ii) = xa(ii) / WOBIF(2)
        enddo
      endif
!
!   Take first derivatives at endpoints for X-wobble
      yp1x = (yax(2)-yax(1)) / (xa(2)-xa(1))
      ypnx = (yax(n_spline)-yax(n_spline-1))/ &
     &       (xa(n_spline)-xa(n_spline-1))
!  call spline initialization subroutine for X-wobble
      call spline(xa,yax,n_spline,yp1x,ypnx,y2sx,ierr4)
!
!   Take first derivatives at endpoints for Y-wobble
      yp1y = (yay(2)-yay(1)) / (xa(2)-xa(1))
      ypny = (yay(n_spline)-yay(n_spline-1))/ &
     &       (xa(n_spline)-xa(n_spline-1))
!  call spline initialization subroutine for Y-wobble
      call spline(xa,yay,n_spline,yp1y,ypny,y2sy,ierr4)
!
      Endif                      ! Initialize spline routine
!
!    If there is a ROTEPH array in the database, GET it. Otherwise,
!    bypass this step. Also, check in case there are more than MXEPCH
!    entries in ROTEPH.
!
!**   IF (.not. Input_EOP) THEN
        IF (KERASK .NE. 0) GO TO 400
        MEPOCH = NEPOCH
        IF (MEPOCH .GT. MXEPCH) MEPOCH = MXEPCH
!@        mepoch = 2
!??     CALL GET4 ('ROTEPOCH      ', ROTEPH, int2(2), MEPOCH, int2(1), &
!??  &       NDO, KERR(7))
!??     IF (KERR(7) .NE. 0) write ( 6, * ) 'Failure to retreieve lcode '// &
!??  &      ' ROTEPOCH  MEPOCH =', MEPOCH,' NEPOCH =',NEPOCH, ' MXEPCH = ', &
!??  &        MXEPCH, ' kerr(7) = ', kerr(7) ! %%%%%%%%%
!??     IF (KERR(7) .NE. 0) CALL TERMINATE_CALC('WOBI  ', int2(7), &
!??  &       KERR(7))
!**   ELSE
!**     MEPOCH = ndays
!**   ENDIF
!
!   Compute the interpolated values of the wobble with a call to WOBMU.
!??   DO N=1,MEPOCH
!??     XJD = ROTEPH(1,N)
!??     tab_time = ROTEPH(2,N)
!??      If (KWOBC.ne.1) then             ! Module ON
!??       CALL WOBMU (XJD,tab_time,WOBXL,WOBYL,DWOBXL,DWOBYL)
!??      Else                             ! Module OFF
!??       WOBXL = 0.D0
!??       WOBYL = 0.D0
!??      Endif
!??     WOBTAB(1,N) = WOBXL
!??     WOBTAB(2,N) = WOBYL
!??   Enddo
!
!    'PUT' the WOBTAB array into the database.
!**   CALL PUT4 ('WOBEPOCH      ', WOBTAB, int2(2), MEPOCH, int2(1))
!
!    Go here if bypassing the interpolation step.
  400 CONTINUE
!
!    Check KWOBD for debug output.
      IF ( KWOBD .EQ. 0 ) GO TO 500
       WRITE ( 6, 9)
    9  FORMAT (1X, 'Debug output for subroutine dWOBI.' )
    7  FORMAT(A,15I8/(9X,15I8))
       WRITE(6,8)' WOBIF ',WOBIF
    8  FORMAT(A,4D25.16/(8X,5D25.16))
       WRITE(6,8)' XYWOB   ',XYWOB
       WRITE(6,8)' ROTEPH  ',ROTEPH
       WRITE(6,8)' WOBTAB  ',WOBTAB
!
!     Normal conclusion.
  500 RETURN
      END
!
!*************************************************************************
      SUBROUTINE SPACEI(I)
      Implicit None
!
!     SPACEI is the spacecraft module input and initialization section.
!
!     Common blocks used -
!
      INCLUDE 'd_input.i'
!        Variables 'from':
!          1. SpcIF(3)     - The spacecraft information array for the  
!                               current near-field sources. Contains
!                               1) The Julian date of the first tabular 
!                               point, 2) The increment in fractional days
!                               of the tabular points, 3) The number of 
!                               tabular points.  (days, days, unitless)
!          2. SpTag(NF_row,10) - The time tags for the spacecraft positions
!                               and velocities, for up to 10 near-field sources.
!          3. SpPos(NF_row,3,10) - The X,Y,Z tabular points of the spacecraft
!                               geocentric J2000 position (meters).
!          4. SpVel(NF_row,3,10) - The X,Y,Z tabular points of the spacecraft
!                               geocentric J2000 velocity (meters/sec).
!
!   Program specifications -
!
      Real*8  xa(NF_row), xb(NF_row)
      Real*8 yp1x,ypnx,yp1y,ypny
      REAL*8 XJD, UTC, tab_time, X_Sp
      Integer*4 I, Ierr4, Ix, Ia(NF_row)
      INTEGER*4  Increment, Tab_len, I_Sp
!
      Integer*4 MXEPCH, N, II
!
!
!     Program Variables -
!           1. I         -  The spacecraft index number, usually 1.
!           2. Increment -  The increment of the wobble table. (days)
!                           (milliarcsec)
!           3. XJD       -  The Julian date at 0:00 hours UTC of the
!                           interpolation epoch. (days)
!           4. UTC       -  The UTC fraction of the UTC day of the
!                           interpolation epoch. (days)
!           5. Tab_len   -  The length of the Wobble table in the database.
!           6. tab_time  -  The fraction of the day of the interpolation epoch
!                           in WOBMU. (days)
!
! 3.2.9 PROGRAMMER - David Gordon Feb. 2013
!       2015-JUL-16 D.G.  Updated with spacecraft index number for jobs 
!                         with both near- and far-field sources.
!
!
!  Create the SpcIF array
!       SpcIF(1) = SpTag(1,I) + 2400000.5D0   ! Full Julian Date
        SpcIF(1) = SpTag(1,I)                 ! MJD 
          X_Sp = (SpTag(2,I) - SpTag(1,I))*86400.D0 + .01
          I_Sp = X_Sp 
        SpcIF(2) = I_Sp/86400.D0
        SpcIF(3) = Numrows(I)
!      write(6,*) 'SPACEI: I,SpcIF,Numrows: ', I, SpcIF, Numrows(I)
!
      Increment = SpcIF(2) + 0.01
      Tab_len   = SpcIF(3) + 0.01
!
!   Code for spline interpolation initialization.
!
       S_spline = tab_len
      do ii=1, S_spline
       ySPxp(ii) = SpPos(ii,1,I)
       ySPyp(ii) = SpPos(ii,2,I)
       ySPzp(ii) = SpPos(ii,3,I)
       ySPxv(ii) = SpVel(ii,1,I)
       ySPyv(ii) = SpVel(ii,2,I)
       ySPzv(ii) = SpVel(ii,3,I)
      enddo
!
      xa(1) = SpcIF(1)
      do ii=2, S_spline
       xa(ii) = xa(ii-1) + SpcIF(2)
      enddo
!
!   If interval is not 1.0 days, then divide by interval
      If ( Abs(SpcIF(2) - 1.D0) .gt. 1.D-10) then
        do ii=1, S_spline
         xb(ii) = (xa(ii)-SpcIF(1)) / SpcIF(2)  + .001D0
         Ia(ii) = xb(ii)
         xc(ii) = Ia(ii)
        enddo
      endif
!
!   Take first derivatives at endpoints for X-positions
      yp1x = (ySPxp(2)-ySPxp(1)) / (xc(2)-xa(1))
      ypnx = (ySPxp(S_spline)-ySPxp(S_spline-1))/                       &
     &       (xc(S_spline)-xc(S_spline-1))
!   Call spline initialization subroutine for X-positions
      call splyne(xc,ySPxp,S_spline,yp1x,ypnx,y2SPxp,ierr4)
!
!   Take first derivatives at endpoints for Y-positions
      yp1x = (ySPyp(2)-ySPyp(1)) / (xc(2)-xc(1))
      ypnx = (ySPyp(S_spline)-ySPyp(S_spline-1))/                       &
     &       (xc(S_spline)-xc(S_spline-1))
!   Call spline initialization subroutine for Y-positions
      call splyne(xc,ySPyp,S_spline,yp1x,ypnx,y2SPyp,ierr4)
!
!   Take first derivatives at endpoints for Z-positions
      yp1x = (ySPzp(2)-ySPzp(1)) / (xc(2)-xc(1))
      ypnx = (ySPzp(S_spline)-ySPzp(S_spline-1))/                       &
     &       (xc(S_spline)-xc(S_spline-1))
!   Call spline initialization subroutine for Z-positions
      call splyne(xc,ySPzp,S_spline,yp1x,ypnx,y2SPzp,ierr4)
!
!   Take first derivatives at endpoints for X-velocities
      yp1x = (ySPxv(2)-ySPxv(1)) / (xc(2)-xc(1))
      ypnx = (ySPxv(S_spline)-ySPxv(S_spline-1))/                       &
     &       (xc(S_spline)-xc(S_spline-1))
!   Call spline initialization subroutine for X-velocities
      call splyne(xc,ySPxv,S_spline,yp1x,ypnx,y2SPxv,ierr4)
!
!   Take first derivatives at endpoints for Y-velocities
      yp1x = (ySPyv(2)-ySPyv(1)) / (xc(2)-xc(1))
      ypnx = (ySPyv(S_spline)-ySPyv(S_spline-1))/                       &
     &       (xc(S_spline)-xc(S_spline-1))
!   Call spline initialization subroutine for Y-velocities
      call splyne(xc,ySPyv,S_spline,yp1x,ypnx,y2SPyv,ierr4)
!
!   Take first derivatives at endpoints for Z-velocities
      yp1x = (ySPzv(2)-ySPzv(1)) / (xc(2)-xc(1))
      ypnx = (ySPzv(S_spline)-ySPzv(S_spline-1))/                       &
     &       (xc(S_spline)-xc(S_spline-1))
!   Call spline initialization subroutine for Z-velocities
      call splyne(xc,ySPzv,S_spline,yp1x,ypnx,y2SPzv,ierr4)
!
!
!    Check debug output.
!     IF ( KWOBD .EQ. 0 ) GO TO 500
!      WRITE ( 6, 9)
    9  FORMAT (1X, 'Debug output for subroutine SPACEI.' )
    7  FORMAT(A,15I8/(9X,15I8))
    8  FORMAT(A,4D25.16/(8X,5D25.16))
!      WRITE(6,8)' SpcIF  ', SpcIF
!      WRITE(6,8)' SpTag  ', (SpTag(ix,I), ix=1,S_spline)
!      WRITE(6,*) ' S_spline: ', S_spline
!      WRITE(6,8)' xa     ', (xa(ix),ix=1,S_spline)   
!      WRITE(6,8)' xb     ', (xb(ix),ix=1,S_spline)   
!      WRITE(6,8)' xc     ', (xc(ix),ix=1,S_spline)   
!      WRITE(6,*)' Ia     ', (Ia(ix),ix=1,S_spline)   
!      WRITE(6,8)' SpPosX ', (SpPos(ix,1,I), ix=1,S_spline)
!      WRITE(6,8)' SpPosY ', (SpPos(ix,2,I), ix=1,S_spline)
!      WRITE(6,8)' SpPosZ ', (SpPos(ix,3,I), ix=1,S_spline)
!      WRITE(6,8)' SpVelX ', (SpVel(ix,1,I), ix=1,S_spline)
!      WRITE(6,8)' SpVelY ', (SpVel(ix,2,I), ix=1,S_spline)
!      WRITE(6,8)' SpVelZ ', (SpVel(ix,3,I), ix=1,S_spline)
!
!      WRITE(6,8)' ySPxp  ', (ySPxp(ix), ix=1,S_spline) 
!      WRITE(6,8)' ySPyp  ', (ySPyp(ix), ix=1,S_spline) 
!      WRITE(6,8)' ySPzp  ', (ySPzp(ix), ix=1,S_spline) 
!      WRITE(6,8)' ySPxv  ', (ySPxv(ix), ix=1,S_spline) 
!      WRITE(6,8)' ySPyv  ', (ySPyv(ix), ix=1,S_spline) 
!      WRITE(6,8)' ySPzv  ', (ySPzv(ix), ix=1,S_spline) 
!
!      WRITE(6,8)' y2SPxp ', (y2SPxp(ix), ix=1,S_spline) 
!      WRITE(6,8)' y2SPyp ', (y2SPyp(ix), ix=1,S_spline) 
!      WRITE(6,8)' y2SPzp ', (y2SPzp(ix), ix=1,S_spline) 
!      WRITE(6,8)' y2SPxv ', (y2SPxv(ix), ix=1,S_spline) 
!      WRITE(6,8)' y2SPyv ', (y2SPyv(ix), ix=1,S_spline) 
!      WRITE(6,8)' y2SPzv ', (y2SPzv(ix), ix=1,S_spline) 
!
!     Normal conclusion.
  500 RETURN
      END
!
!*************************************************************************
!
      SUBROUTINE dOCNIN(Kjob,Kerr)
      Implicit None
!
      INCLUDE 'cmxst11.i'
!
      INCLUDE 'param11.i'
!       1. OC_file - Name of the ocean loading coefficients file
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!         VARIABLES 'FROM':
!            1. CONVD - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
!
      Real*8    OC11(11)
      Integer*4 I, II, Jsite(Max_stat), Iunit, ios, Iquit, Index, K
      Integer*4 Get4unit, Kjob,  Kerr
!     Integer*2 Kerr(10)
!
      Character*80 Inbuf
      Character*8  Dbsites(Max_stat)
      Equivalence (LNSITE(1,1), Dbsites(1))
!
      Character*4 Ocestat(Max_stat)
      Integer*2 L_ocestat(2,Max_stat)
      Equivalence (Ocestat,L_ocestat)
      Save Iunit
!
!  Programmer/History:
!       David Gordon 98.03.17 - Program created.
!       David Gordon 99.11.19 - Bug fix - corrected ocean loading external
!                               file site name.
!       David Gordon 2000.01.05 - Determine ocean loading statuses (must be
!                               'YES' for all stations) and PUT them into
!                               the type 1 records, Lcode 'OCE STAT'.
!       David Gordon 2006.03.30 Revised missing station message.
!       David Gordon 2006.04.03 Revised missing station message again.
!       David Gordon 2013 Jan/April - Modified for correlator usage.
!       David Gordon 2016 July 06   - Kjob added to conserve LU numbers.
!
!   Initialize station counter
       Do I = 1, Max_stat
         Jsite(i) = 0
       Enddo
!
!  Open the Ocean loading data file
       If (Kjob .eq. 1) Iunit = get4unit()
       Open (Unit=Iunit, File=OC_file, Status='old', Action='READ',     &
     &       Err=240, Iostat=ios)
!
      If ( Index(OC_file,'blokq') .gt. 0) Then
!  Blokq.dat file, find the ocean loading coefficients catalog
  50   Continue
       Read(iunit,'(A80)') Inbuf
       If (Inbuf(1:2) .eq. '//') Go to 100
       Go to 50
 100   Continue
      Endif
!
 110   Continue
       Read(iunit,'(A80)',end=200) Inbuf
!   Skip comments and illegal lines
       If (Inbuf(1:2) .eq. '$$') Go to 110
       If (Inbuf(1:2) .ne. '  ') Go to 110
!      If (Inbuf(11:11) .ne. ' ' ) Go to 110
!
!   Finished site catalog
       If (Inbuf(1:2) .eq. '//') Go to 200
!
! See if this station is in the database list
       Do I = 1, Numsit
         If (Inbuf(3:10) .eq. Dbsites(I) .or.                           &
     &       Inbuf(13:20) .eq. Dbsites(I) .or.                          &
     &       Inbuf(23:30) .eq. Dbsites(I) ) Then
!    &       Inbuf(13:14) .eq. Dbsites(I)(1:2) .or.                     &
!    &       Inbuf(23:24) .eq. Dbsites(I)(1:2) ) Then
!**       print *, 'dOCNIN: Site ', Dbsites(I)
!  Matched I'th station in data base station list, save station particulars
           II = I
!
!  Skip comments
 170    Continue
        Read(iunit,'(A80)',end=200) Inbuf
        If (Inbuf(1:2) .eq. '$$') Go to 170
!
!   Read 6 data lines
        Read(Inbuf,*,err=180) OC11
          Do k=1,11
           SITOAM(k,II) = OC11(k)
          Enddo
!
        Read(Iunit,*,err=180) OC11
          Do k=1,11
           SITHOA(k,1,II) = OC11(k)
          Enddo
        Read(Iunit,*,err=180) OC11
          Do k=1,11
           SITHOA(k,2,II) = OC11(k)
          Enddo
!
        Read(Iunit,*,err=180) OC11
          Do k=1,11
           SITOPH(k,II) = OC11(k) * CONVD
          Enddo
        Read(Iunit,*,err=180) OC11
          Do k=1,11
           SITHOP(k,1,II) = OC11(k) * CONVD
          Enddo
        Read(Iunit,*,err=180) OC11
          Do k=1,11
           SITHOP(k,2,II) = OC11(k) * CONVD
          Enddo
!
           Jsite(II)    = II
!
         Endif
       Enddo
!
       Go to 110
!
 200   Continue
!
       Close(Iunit)
!
!   Verify that we have ocean loading a priori's for all stations, except any
!    site at the geocenter. If not, we must quit here and tell the user to fix
!    the problem.
!
        Iquit = 0
!
      DO I = 1, Numsit
        If (Jsite(i) .eq. 0) Then
           If (I .ne. Zero_site) Then
            If (iquit.eq.0) Write(6,'(/)')
            Write(6,'("Warning - No ocean loading coefficients for ",   &
     &         A8,/, "Will continue but you really should update file ", &
     &         A128,/)')  Dbsites(I), OC_file
            Iquit = Iquit + 1
             Do k = 1, 11
              SITOAM(k,I)   = 0.D0
              SITHOA(k,1,I) = 0.D0
              SITHOA(k,2,I) = 0.D0
              SITOPH(k,I)   = 0.D0
              SITHOP(k,1,I) = 0.D0
              SITHOP(k,2,I) = 0.D0
             Enddo
           Endif
        Endif
!        If (Iquit .gt. 0) Then
!            Write(6,'(/,"!!! Missing ocean loading!!! Update file ",/,   &
!    &       10X, A80,/,3X, " and rerun Calc!!!",/ )') OC_file 
!            Call TERMINATE_CALC( 'dOCNIN ',int2(0), int2(0))
!        Endif
      ENDDO
!
       Go to 270
!
!   Error on Read
 180  Continue
      WRITE (6,*)  'dOCNIN: Error on read of ocean loading file '
        Call TERMINATE_CALC('dOCNIN ', int2(0), int2(0))
!
!   Error on OPEN
 240  Continue
      WRITE ( 6, * )  'OCNIN: Error on OPEN of ocean loading file '
        Call TERMINATE_CALC('OCNIN ', int2(0), int2(0))
!
 270  Continue
!
      Return
      End
!*************************************************************************
      SUBROUTINE dANTILT(Kjob, Krr)
      Implicit None
!
      INCLUDE 'cmxst11.i'
!        Variables 'from':
!           1. LNSITE(4,Max_Stat)  - THE EIGHT CHARACTER SITE NAMES
!                                    IN THE DATABASE. (ALPHAMERIC)
!           2. NUMSIT             -  THE NUMBER OF SITES IN THE DATABASE.
!        Variables 'to':
!           2. Dbtilt(2,Max_Stat)  - Antenna fixed axis tilts, in arc-minutes.
!                                    For alt-az mounts, 1 => East tilt,
!                                    2 => North tilt.
      INCLUDE 'param11.i'
!        Variables 'from':
!           1. A_tilts -  Ascii name of the antenna fixed axis tilts file.
!
      INCLUDE 'cmxut11.i'
!        Variables 'to':
!           1. Intrvl(5,2) - First and last time tag of data in the current
!                            data base. (First index: year, month, day,
!                            hour, minute. Second index: first, last.)
!
      Character*80 Inbuf
      Character*8  Dbsites(Max_stat), Asite, Bsite, slash, Ablnk, &
     &             Bblnk
      Equivalence (LNSITE(1,1), Dbsites(1))
!
      Real*4 Tyear, Tdate, Tilt1, Tilt2, Tilt1a, Tilt2a, Tilt1b, &
     &       Tilt2b, Tdatea, Tdateb
      Real*4 XDOY, XYR
      Integer*4 Iyear, Imonth, Iday, Krr, Nsites, Mstat, Kjob
      Integer*4 I, II, Iunit, Ios, Index, I4, ICAL(12), LCAL(12)
      Integer*4 Get4unit
      Save Iunit
!
      DATA ICAL /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
      DATA LCAL /0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/
!
!  Programmer/History:
!       David Gordon 2004.05.17 Program created.
!       David Gordon 2016.07.06 Kjob added to conserve LU numbers.
!
!   Use Integer*4 quantities
       Nsites = Numsit
       Mstat = Max_stat
!
!   Initialize station tilts
       Do I = 1, Mstat
         Dbtilt(1,I) = 0.0D0
         Dbtilt(2,I) = 0.0D0
       Enddo
!
        Iyear  = Intrvl(1,1)
        Imonth = Intrvl(2,1)
        Iday   = Intrvl(3,1)
         If (Iyear .ge.  0 .and. Iyear .le. 69) Iyear = Iyear+2000
         If (Iyear .ge. 70 .and. Iyear .le. 99) Iyear = Iyear+1900
!         Write (6,*) 'dANTILT: Intrvl ', Intrvl
!         Write (6,*) 'dANTILT: Iyear,Imonth,Iday ', Iyear,Imonth,Iday
!
!   Find epoch, in years at start of session.
!    Get day of year number
        IF (MOD(Iyear,4).NE. 0) Then
          XDOY = ICAL(Imonth) + Iday
          XYR = 365.
        Else
          XDOY = LCAL(Imonth) + Iday
          XYR = 366.
        Endif
         Tyear = Float(Iyear) + XDOY/XYR
!          Write ( 6, * ) ' XDOY,XYR,Tyear ', XDOY,XYR,Tyear
!
!  Open the input file of antenna tilts
       If (Kjob .eq. 1) Iunit = get4unit()
       Open (Unit=Iunit, File=A_tilts, Status='old', Action='READ',     &
     &       Err=240, Iostat=ios)
!
!   Skip first 4 lines
       Do I = 1, 4
        Read (Iunit,'(A80)') Inbuf
       Enddo
!
 110   Continue
       Read(iunit,1001,end=300) Asite, Bsite, Tdate, Tilt1,             &
     &                                  Tilt2, slash
 1001   Format(2X,A8,1X,A8,F11.3,6X,F8.2,7X,F8.2,A8)
!
       IF ( Index(slash, '/') .gt. 0) Then
          Tilt1a = Tilt1
          Tilt2a = Tilt2
          Tdatea = Tdate
          Tilt1b = Tilt1
          Tilt2b = Tilt2
          Tdateb = Tdate
          Go to 200
       Endif
!
!             More lines for this station
          Tilt1a = Tilt1
          Tilt2a = Tilt2
          Tdatea = Tdate
       If (Tyear .le. Tdate) Then
          Tilt1b = Tilt1
          Tilt2b = Tilt2
          Tdateb = Tdate
!           Read till '/' found
  180     Continue
          Read(iunit,1001) Ablnk, Bblnk, Tdate, Tilt1, Tilt2, slash
          IF (Index(slash, '/') .gt. 0) Go to 200
          Go to 180
       Endif
!
  182      Continue
          Read(Iunit,1001) Ablnk, Bblnk, Tdate, Tilt1, Tilt2, slash
          If (Tyear .le. Tdate) Then
          Tilt1b = Tilt1
          Tilt2b = Tilt2
          Tdateb = Tdate
          IF (Index(slash, '/') .gt. 0) Go to 200
  184      Continue
           Read(iunit,1001) Ablnk, Bblnk, Tdate, Tilt1, Tilt2, slash
           IF (Index(slash, '/') .gt. 0) Go to 200
           Go to 184
!
          Else
            IF (Index(slash, '/') .le. 0) Then
             Tilt1a = Tilt1
             Tilt2a = Tilt2
             Tdatea = Tdate
             Go to 182
            Else
             Tilt1b = Tilt1
             Tilt2b = Tilt2
             Tdateb = Tdate
            Endif
!
          Endif
!
 200   Continue
!       Write (6,*) ' Asite, Bsite, Tyear: ', Asite, Bsite, Tyear
!       Write (6,*) ' Tdatea,Tilt1a,Tilt2a: ', Tdatea,Tilt1a,Tilt2a
!       Write (6,*) ' Tdateb,Tilt1b,Tilt2b: ', Tdateb,Tilt1b,Tilt2b
!
!  Match I'th station and find its tilt for this epoch.
      Do I = 1, Nsites
       If (Asite .eq. Dbsites(I) .or. Bsite .eq. Dbsites(I)) Then
         II = I
         Go to 155
       Endif
      Enddo
       Go to 110
!
 155  Continue
        If ( ABS(Tdateb-Tdatea) .lt. .002 ) Then
          Dbtilt(1,II) = Tilt1a
          Dbtilt(2,II) = Tilt2a
        Else
          Dbtilt(1,II) = Tilt1a + (Tyear-Tdatea)*(Tilt1b-Tilt1a)/       &
     &                   (Tdateb-Tdatea)
          Dbtilt(2,II) = Tilt2a + (Tyear-Tdatea)*(Tilt2b-Tilt2a)/       &
     &                   (Tdateb-Tdatea)
        Endif
!       Write (6,*) 'dANTILT: ', II,Dbsites(II),Dbtilt(1,II),Dbtilt(2,II)
!
       Go to 110
!
 300   Continue
       Close(Iunit)
!
       Krr = 0
       Go to 270
!
!   Error on OPEN
 240  Continue
      Krr = -1
      WRITE (6,*)  'Error on OPEN of blokq file '
!
 270  Continue
      Return
      End

!*************************************************************************
      SUBROUTINE dOPTLIN(Kjob, Kerr)
      Implicit None
!
      INCLUDE 'cmxst11.i'
!
      INCLUDE 'param11.i'
!       1. OPTL_file - Name of the ocean pole tide loading coefficients
!                      file.
!
      Character*80 Inbuf
!     Real*8    OPTL6(6,Max_stat)
      Real*8 Vlon, Vlat, XYZ(3), u_rR, u_rI, u_nR, u_nI, u_eR, u_eI
      Character*8  Vsite, Dbsites(Max_stat)
      Equivalence (LNSITE(1,1), Dbsites(1))
      Integer*4 I, II, Jsite(Max_stat), Iunit, Ios, Iquit, Index, J
      Integer*4 Get4unit, Kjob, Kerr
!     Integer*2 Kerr(10)
      Character*3 C3, dum1, dum2, dum3
      Save Iunit
!
!  Programmer/History:
!       David Gordon 2012.11.28 - Program created.
!       David Gordon 2016.07.06 - Kjob added to conserve LU numbers.
!
!   Initialize station counter
       Do I = 1, Max_stat
         Jsite(i) = 0
         Do J = 1,6
          OPTL6(J,I) = 0.0D0
         Enddo
       Enddo
!
!  Open the Ocean pole tide loading data file
       If (Kjob .eq. 1) Iunit = get4unit()
       Open (Unit=Iunit, File=OPTL_file, Status='old', Action='READ',   &
     &       Err=240, Iostat=Ios)
!
  50   Continue
       Read(Iunit,'(A80)',Err=180,End=250) Inbuf
!       Write(6,*) Inbuf
       If (Inbuf(1:36) .eq. 'Ocean Pole Tide Loading Coefficients')     &
     &     Go to 100
       Go to 50
 100   Continue
!  Skip 4 lines
       Do I = 1,4
        Read(iunit,'(A80)') Inbuf
       Enddo
!
 110   Continue
       Read(iunit,1001,err=110,end=200) Vsite, C3, Vlat, Vlon, u_rR,    &
     &        u_rI, u_nR, u_nI, u_eR, u_eI
 1001  Format(1x,A8,1X,A3,1x,F6.2,3X,F8.2,6(2X,F10.6))
!
! see if this station is in the database list
       do I = 1, Numsit
         If (Vsite .eq. Dbsites(I) .or. C3 .eq. Dbsites(I)(1:3)) Then
!         print *, 'OPTLIN: Site matched - ', Dbsites(I)
!  matched I'th station in data base station list, save station particulars
           II = I
          OPTL6(1,II) =  u_rR
          OPTL6(2,II) =  u_rI
          OPTL6(3,II) =  u_nR
          OPTL6(4,II) =  u_nI
          OPTL6(5,II) =  u_eR
          OPTL6(6,II) =  u_eI
           Jsite(II) = II
!
         Endif
       enddo
!
       go to 110
!
 200   continue
!
       close(Iunit)
!
!   Verify that we have ocean pole tide loading coefficients for all stations,
!    except any site at the geocenter. If not, we must quit here and tell the 
!    user to fix the problem.
!
        iquit = 0
!
      do I = 1, Numsit
        if (Jsite(I) .eq. 0 .and. I .ne. Zero_site) Then
          If (iquit.eq.0) Write(6,'(/)')
          Write(6,'("Warning - No ocean pole tide loading coefficients for ",  &
     &         A8,/, "Will continue but you really should update file ", &
     &         A50,/)')  Dbsites(I), OPTL_file
          Iquit = Iquit + 1
        endif
!        If (Iquit .gt. 0) Then
!            Write(6,'(/,"!!! Missing ocean pole tide loading!!! You"   &
!    &       " really should update file ",/, A80, /,                   &
!    &       " and rerun Calc!!!",/ )') OPTL_file 
!!!!!        Call TERMINATE_CALC( 'OPTLIN ',int2(0), int2(0))
!        Endif
      enddo
!
!
!      Write(6,'(" OPTLIN: Ocean pole tide loading coefficients: ")')
!     Do I=1,Numsit
!      Write (6,1073) Dbsites(I), OPTL6(1,I), OPTL6(2,I), OPTL6(3,I),   &
!    &                OPTL6(4,I), OPTL6(5,I), OPTL6(6,I)
!1073  Format(1x,A8,3X,6(2X,F10.6))
!     Enddo
!   
!
       Go To 270
!
!   error on Read
 180  continue
      Write (6,*)  'OPTLIN: Error on read of ocean loading file '
        call TERMINATE_CALC('OPTLIN ', int2(0), int2(0))
!
!   error on OPEN
 240  continue
       close (Iunit)
      wriTE (6,*)  'OPTLIN: Ocean Pole Tide Loading not found '
        call TERMINATE_CALC('OPTLIN ', int2(0), int2(0))
!
 250  continue
       close (Iunit)
      Write(6,*) 'OPTLIN: Ocean Pole Tide Loading section was not found'
        call TERMINATE_CALC('OPTLIN ', int2(0), int2(0))
!
 270  continue
       close (Iunit)
      return
      end
