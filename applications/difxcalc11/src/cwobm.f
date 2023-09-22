      SUBROUTINE WOBG (CENT, TT, UTC, XJD, GMST2K, TSKIP, FA2K, FAD2K,  &
     &                 UT1, DUT1AT, Xti, Yti, dXti, dYti, Xli, Yli,     &
     &                 dXli, dYli, UT1li, dUT1li, UT1ti, dUT1ti,        &
     &                 WOBXR, WOBYR, WOBXD, WOBYD, SP, DSP, RW2K)
      IMPLICIT None
!
!    WOBG is the geometry section of the Wobble Module. It computes the
!    wobble portion of the complete crust fixed to J2000.0 rotation matrix.
!    Although this subroutine uses the BIH sign convention for a left-handed
!    coordinate system, the values for WOBX and WOBY leave this routine as
!    WOBXR and WOBYR, values in a right-handed system. WOBXR and WOBYR are
!    passed to all routines outside the Wobble Module. (Used in Pole Tide
!    module.)
!
!    References - Ash, M.E., 'Determination of Earth Satellite Orbits", Lincoln
!                 Laboratory Technical Report 1972-75, 04/19/72, p. 229-230.
!
!               - Mueller, I.P., "Spherical and Practical Astronomy as Applied
!                 to Geodesy", 1969, p. 80-85. (NOTE: The reference in Mueller
!                 refers to the calculation of the wobble portion of the
!                 complete J2000.0 to crust fixed rotation matrix. Program CALC
!                 however, needs the transpose of this matrix, so be cautious
!                 if comparing the reference to the following program.)
!
!                 IERS Conventions (2003).
!
!     Calling sequence -
!           Input Variables:
!             1. CENT     -
!             1. CT       -  Coordinate time (ephemeris time).
!             1. TT       -  Terrestrial Time fraction of the day.
!             2. UTC      -  The UTC fraction of the UTC day (days).
!             3. XJD      -  The Julian date at zero hours UTC of the date in
!                            question. (days)
!             4. TSKIP    -  Skip time-dependent computations if TSKIP=1
!                            (same time as previous observation).
!             5. Xti      -  Short period X tidal contribution. (mas)
!             6. Yti      -  Short period Y tidal contribution. (mas)
!             7. dXti     -  Time derivative of Xti. (mas/sec)
!             8. dYti     -  Time derivative of Yti. (mas/sec)
!           Output Variables:
!             1. RW2K(3,3,2)-The wobble portion of the complete crust fixed 
!                            to J2000.0 rotation matrix and its first time'
!                            derivative. (unitless, 1/sec)
!             2. WOBXR     - The long period wobble X-offset. (RAD)
!                            This variable, along with WOBYR, are for use in a
!                            right-handed coordinate system. WOBXR and WOBYR
!                            are the variables passed to all routines external
!                            to the Wobble Module.
!             3. WOBYR     - The long period wobble Y-offset. (RAD)
!                            To be used in a right-handed system (see WOBXR).
!             4. SP        - S' angle, positions the Terrestrial Ephemeris 
!                            Origin on the equator of the Celestial 
!                            Intermediate Pole.
!             5. DSP       - dS'/dt, time derivative of SP (above).
!
!     Common blocks used -
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!            Variables 'FROM':
!              1. CONVDS  -  The conversion factor of radians per arcsecond.
!                            (radians/arcsecond)
!
      INCLUDE 'cmwob11.i'
!            Variables 'TO':
!              1. RWOBX(3,3) - The rotation matrix which rotates about the
!                              crust fixed Y-axis by an angle equal to WOBX.
!              2. RWOBY(3,3) - The rotation matrix which rotates about the new
!                              X-axis by an angle equal to WOBY. (unitless)
!              3. RWOBZ(3,3) - The rotation matrix which rotates about the
!                              Z-axis by the angle SP.
!              4. WOBX       - The left-handed X-wobble. (rad)
!              5. DWOBX      - Time derivative of WOBX. (rad/sec)
!              6. WOBY       - The left-handed Y-wobble. (rad)
!              7. DWOBY      - Time derivative of WOBY. (rad/sec)
!              8. RWX(3,3)   - Rotation matrix used in computation of time
!                              derivative of wobble rotation matrix.
!              9. RWY(3,3)   - Rotation matrix used in computation of time
!                              derivative of wobble rotation matrix.
!             10. RWZ(3,3)   - Rotation matrix used in computation of time
!                              derivative of wobble rotation matrix.
!             11. WOBXli     - Short period X libration contribution. (rad)
!             12. WOBYli     - Short period Y libration contribution. (rad)
!             13. dWOBXli    - Time derivative of WOBXn. (rad/sec)
!             14. dWOBYli    - Time derivative of WOBYn. (rad/sec)
!             15. WOBXti     - Short period X tidal contribution. (rad)
!             16. WOBYti     - Short period Y tidal contribution. (rad)
!
      INCLUDE 'cmxut11.i'
!            Variables 'FROM':
!              1. EOP_time_scale - EOP table time scale, allowed values:
!                              'TAI     ', 'TCG     ', 'TDB     ',
!                              'TDT     ', 'UTC     ', 'UNDEF   '.
!
       INCLUDE 'cuser11.i'
!          Variables from:
!            1. Calc_user - Calc user type. 'A' for Calc/SOLVE analysis.
!                           'C' for VLBI correlator.
!            2. Hifreq_tidal_EOP - Correlator option to add the high
!                           frequency (diurnal and sub-diurnal) EOP 
!                           correction for ocean tidal effects from 
!                           subroutine ORTHO_EOP. 
!                           'Y' => Add high frequency EOP corrections.
!                           'N' => Don't add high frequency EOP correction. 
!            3. Hifreq_nut_EOP - Correlator option to add the EOP
!                           correction for high frequency (periods less 
!                           than 2 days) nutation. 
!                           'Y' => Add EOP nutation corrections.
!                           'N' => Don't add EOP nutation corrections. 
!
      INCLUDE 'ccon.i'
!            Variables 'FROM':
!              1.  KWOBC  -  The Wobble Module flow control flag.
!              2.  KWOBD  -  The Wobble Module debug output flag.
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. WOBIN - The X and Y polar motion values interpolated to
!                     the time of this observation in the conventional
!
!
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
!           VARIABLES 'TO':
!            1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
!                           CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
!                           RATE OF CHANGE OF 'TAI MINUS UTC'. Used in the
!                           atomic time module. (DAYS, SEC, SEC/SEC)
!
!   Program Specifications -
      Real*8           UTC, XJD, WOBXR, WOBYR, WOBXL,                   &
     &       WOBYL, DWOBXL, DWOBYL, CT, tab_time,rw1(3,3), rw2(3,3),    &
     &       CENT, GMST2K(2), FA2K(14), FAD2K(14), TT
      Real*8 WOBXD, WOBYD
      REAL*8 SP,DSP,           RW2K(3,3,2),DRWC1(3,3),                  &
     &       DRWC2(3,3), DRWC3(3,3), Xli, Yli, dXli, dYli, UT1ti,       &
     &       dUT1ti, TMJD, Xti, Yti, dXti, dYti, UT1li, dUT1li
         Real*8        UT1, DUT1AT
      Integer*4 TSKIP
!
!     Subroutine interface -
!             Caller subroutines: DRIVG
!             Called subroutines: WOBMU, XYLIB, UT1_LIB, MMUL3,
!                                 ROTAT, DROTT, MADD3
!
!     Program Variables -
!               1. WOBXL  - The left-handed X-Wobble. (milliarcsec)
!               2. WOBYL  - The left-handed Y-Wobble. (milliarcsec)
!               3. DWOBXL - Time derivative of WOBXL. (masec/sec)
!               4. DWOBYL - Time derivative of WOBYL. (masec/sec)
!               5. WOBIN  - Both wobbles, to be put into the database.
!               6. rw1(3,3)-Rotation matrix used in computation of time
!                           derivative of wobble rotation matrix.
!               7. rw2(3,3)-Rotation matrix used in computation of time
!                           derivative of wobble rotation matrix.
!
!     Programmer - Dale Markham  02/17/77
!      77.07.14 Peter Denatale
!      77.03.31 Kathy Watts
!      78.01.09 Bruce Schupler
!      88.12.21 Gregg Cooke
!      89.07.26 Jim Ryan  Documentation simplifed and strings.
!      89.12.12 Jim Ryan  UNIX-like database interface implimented.
!      90.01.02 Jim Ryan  WOBIN converted to left-handed.
!      91.06.06 Jim Ryan  Clean up documentation.
!      94.04.15 D. Gordon Converted to Implicit None.
!      94.05.23 D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
!                         being dimensioned both Logical*2 and Real*8.
!      95.10.05 D. Gordon Skip interpolation if Module OFF (KWOBC=1); set
!                         WOBXL and WOBYL equal to zero (previously undefined).
!      95.12.04 D. Gordon Changing interpolation epoch variable to tab_time
!                         (previously was UTC). Tab_time set to CT for
!                         consistency with UT1 interpolation which is in CT.
!      95.12.11 D. Gordon Adding DWOBXL and DWOBYL, derivatives of X-wobble and
!                         Y-wobble, to WOBMU argument list. Variables DWOBX,
!                         DWOBY, rwx, rwy, rw1, and rw2 added. Code added to
!                         compute the time derivative of the wobble rotation
!                         matrix. RW(3,3) changed to RW(3,3,2).
!      96.02.09 D. Gordon Added dwobx, dwoby, rwx, and rwy to Common/WOBCM/.
!      2001.01.02 D. Gordon Code added to determine time scale of EOP table
!                         (CT/TDB, UTC, TDT or TAI).
!      2002 Sept Jim Ryan Integer*2/4 mods.
!      2003.05.07 D. Gordon Code added to compute S' (SP) and dS'/dt (DSP),
!                         to position the Terrestrial Ephemeris Origin on
!                         the equator of the Celestial Intermediate Pole.
!      2003/2004 D.Gordon Modified for CEO-based rotation computations and
!                         for high frequency (tidal and nutation) polar
!                         motion.
!      2007.12.28 D.Gordon Options to add high frequency (diurnal and 
!                         sub-diurnal) EOP corrections due to ocean tides
!                         and nutation for correlator usage.
!      Nov. 2012 D.Gordon Added new subroutine calls. XYLIB computes 
!                         short period polar motion due to libration 
!                         effects. UT1_LIB computes short period UT1
!                         due to libration effects. UT1_LIB is called
!                         here instead of UT1G because GMST2K is not
!                         defined when UT1G is called. 
!      Dec. 2012 D.Gordon Moved PUT's to PUT_G.
!
!
!    WOBG Program Structure.
!
      IF (TSKIP .eq. 1) Go to 250
!
!     Call 'WOBMU' to obtain WOBXL and WOBYL
      If (KWOBC.ne.1) Then             ! Module ON
!
!  Determine time scale of EOP table epochs. TT is now the default.
!        tab_time = CT
         tab_time = TT
       If (EOP_time_scale .eq. 'UTC     ') tab_time = UTC
       If (EOP_time_scale .eq. 'TAI     ') tab_time = UTC +             &
     &       ATMUTC(2)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
       If (EOP_time_scale .eq. 'TDT     ') tab_time = UTC +             &
     &      (ATMUTC(2) + 32.184D0)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
!
       CALL WOBMU (XJD,tab_time,WOBXL,WOBYL,DWOBXL,DWOBYL)
!
! Get short period libration (formerly called nutation) contribution 
!    to X and Y. Xli and Yli are in milli-arc-seconds
       CALL XYLIB (GMST2K, FA2K, FAD2K, Xli, Yli, dXli, dYli)
!
!      WRITE(6,*) 'WOBG: Xli,dXli(mas) ', Xli, dXli
!      WRITE(6,*) 'WOBG: Yli,dYli(mas) ', Yli, dYli
!
!   --- ORTHO_EOP moved to UT1G.
! Get short period tidal contribution to X and Y and UT1.
!      CALL ORTHO_EOP (TMJD, Xti, Yti, UT1t, dXti, dYti, dUT1t)
!
      Else                             ! Module OFF
       WOBXL  = 0.D0
       WOBYL  = 0.D0
       DWOBXL = 0.D0
       DWOBYL = 0.D0
      Endif
!
!   Compute the short period corrections to UT1 due to libration. (msec)
!!!! We have to do this here because GMST2K is defined after the call to UT1G!!!!!!!
      CALL UT1_LIB (GMST2K, FA2K, FAD2K, UT1li, dUT1li)
!      WRITE(6,*) 'WOBG: UT1li,dUT1li(mas) ', UT1li, dUT1li
!      WRITE(6,*) 'WOBG: UT1, UT1+li ', UT1, UT1 + UT1li*1.D-3   
!
!    Convert the variables WOBXL and WOBYL from units of milliarcseconds
!    to units of radians.
      WOBX =   WOBXL * CONVDS * 1.0D-3
      WOBY =   WOBYL * CONVDS * 1.0D-3
      DWOBX = DWOBXL * CONVDS * 1.0D-3
      DWOBY = DWOBYL * CONVDS * 1.0D-3
!
! Save short period nutation and tidal contributions in radians and
!  short period UT1 tidal contribution in seconds.
         WOBXli   =  Xli * CONVDS * 1.0D-3
         WOBYli   =  Yli * CONVDS * 1.0D-3
         dWOBXli  = dXli * CONVDS * 1.0D-3
         dWOBYli  = dYli * CONVDS * 1.0D-3
         WOBXti   =  Xti * CONVDS * 1.0D-3
         WOBYti   =  Yti * CONVDS * 1.0D-3
!
!     If (Calc_user .eq. 'C' .and. Hifreq_tidal_EOP .eq. 'Y') Then
!       WOBX = WOBX + WOBXti 
!       WOBY = WOBY + WOBYti 
!       DWOBX = DWOBX + (dXti * CONVDS * 1.0D-3)
!       DWOBY = DWOBY + (dYti * CONVDS * 1.0D-3)
!     Endif
!
!     If (Calc_user .eq. 'C' .and. Hifreq_nut_EOP .eq. 'Y') Then
!       WOBX = WOBX + WOBXli
!       WOBY = WOBY + WOBYli
!       DWOBX = DWOBX + dWOBXli
!       DWOBY = DWOBY + dWOBYli
!       UT1 = UT1 + UT1li*1.D-3
!     Endif 
!
!    Create variables WOBXR and WOBYR, for external use, in subroutines
!    using a right-handed coordinate system.
      WOBXR =  WOBX
      WOBYR = -WOBY
      WOBXD = DWOBX
      WOBYD = DWOBY
!
!    Compute the wobble portion of the complete crust fixed to J2000.0
!    rotation matrix. (NOTE: WOBY is still left-handed, however,
!    the wobble matrix is constructed to take this into account.
!    Thus the final wobble matrix is right-handed.)
!
!    Construct the rotation matrix which rotates about the crust fixed
!    Y-axis by an angle equal to +WOBX.
      CALL ROTAT ( +WOBX, int2(2), RWOBX)
!
!    Construct the rotation matrix which rotates about the X-axis by
!    an angle equal to +WOBY.
      CALL ROTAT ( +WOBY, int2(1), RWOBY)
!
!  Compute S' and dS'/dt. (Taken from IERS function SP2000.)
        SP  = -47.D-6 * CENT * CONVDS
        DSP = SP/(CENT*36525.D0*86400.D0)
!
!   Construct the rotation matrix which rotates about the Z-axis by
!    an angle equal to -SP.
      CALL ROTAT ( -SP, int2(3), RWOBZ)
!
!  Compute the new polar motion (CEO-based) rotation matrix
!   below agrees with POM2000 and IERS Conventions (2003)
      CALL MMUL3 ( RWOBZ, RWOBX, RWOBY, RW2K(1,1,1) )
!   below agrees with IERS Conventions (1996)
!     CALL MMUL3 ( RWOBZ, RWOBY, RWOBX, RW2K(1,1,1) )
!
!  Compute the time derivative of the CEO-based wobble rotation matrix.
!   Compute the X derivative component
       CALL DROTT (WOBX, DWOBX, int2(2), RWX)
!   Compute the Y derivative component
       CALL DROTT (WOBY, DWOBY, int2(1), RWY)
!   Compute the Z derivative component
      CALL DROTT (-SP, -DSP, Int2(3), RWZ)
!  Compute the three time derivative components ...
!
! Below agrees with POM2000 and IERS Conventions (2003)
      CALL MMUL3 (   RWZ, RWOBX, RWOBY, DRWC1 )
      CALL MMUL3 ( RWOBZ,   RWX, RWOBY, DRWC2 )
      CALL MMUL3 ( RWOBZ, RWOBX,   RWY, DRWC3 )
!
! Below agrees with IERS Conventions (1996)
!     CALL MMUL3 (   RWZ, RWOBY, RWOBX, DRWC1 )
!     CALL MMUL3 ( RWOBZ,   RWY, RWOBX, DRWC2 )
!     CALL MMUL3 ( RWOBZ, RWOBY,   RWX, DRWC3 )
!
!   and add them together (chain rule).
      CALL MADD3 ( DRWC1, DRWC2, DRWC3, RW2K(1,1,2) )
!
!****************************************************
!    Check KWOBC to determine if the wobble module is to be turned off.
      IF ( KWOBC .EQ. 1 )  THEN
        CALL ROTAT ( 0.D0, int2(3), RW2K(1,1,1))
        CALL ROTAT ( 0.D0, int2(3), RW2K(1,1,2))
        WOBXR = 0.0D0
        WOBYR = 0.0D0
      ENDIF
!
 250   CONTINUE
!
!    'PUT' the left-handed wobble values (radians) into the database.
      WOBIN(1) = WOBX
      WOBIN(2) = WOBY
!  The 'PUT' was moved to Subroutine PUT_G
!
!    Check KWOBD for debug output.
      If (KWOBD .ne. 0) then
      WRITE (6,9)
    9 FORMAT (1X, 'Debug output for subroutine WOBG.' )
      WRITE(6,8)' CONVDS  ',CONVDS
    8 FORMAT(A,4D25.16/(7X,5D25.16))
   18 FORMAT(A,3D30.20/(10X,3D30.20))
      WRITE(6,8)' WOBXL,  WOBYL  = ',WOBXL, WOBYL
      WRITE(6,8)' DWOBXL, DWOBYL = ',DWOBXL, DWOBYL
      WRITE(6,8)' CT      ',CT
      WRITE(6,8)' tab_time',tab_time
      WRITE ( 6, 9200 )  UTC, XJD, RW2K
      WRITE(6,8)'  WOBX,  WOBY = ', WOBX,  WOBY
      WRITE(6,8)' DWOBX, DWOBY = ',DWOBX, DWOBY
      WRITE(6,18)' RWOBX  = ', RWOBX
      WRITE(6,18)' RWOBY  = ', RWOBY
      WRITE(6,18)' RWOBZ  = ', RWOBZ
      WRITE(6,18)' RWX    = ', RWX
      WRITE(6,18)' RWY    = ', RWY
      WRITE(6,18)' RWZ    = ', RWZ
      WRITE ( 6, * ) ' WOBG/ORTHO_EOP: Xti,Yti,UT1t:    ',              &
     &            Xti,Yti,UT1ti
      WRITE ( 6, * ) ' WOBXti, WOBYti (rad) =', WOBXti, WOBYti
      WRITE(6,8)' WOBG/WOBX (radians): ', WOBX,DWOBX
      WRITE(6,8)' WOBG/WOBY (radians): ', WOBY,DWOBY
!      Write(6,1015) RW2K
 1015  Format(1x,'WOBG/RW2K ',(3(/,3E30.20)))
 9200 FORMAT (1X,' UTC = ',D26.16,9X,'XJD = ',D30.16,/,                 &
     &           ' RW2K = ',3D30.20,/,5(6x,3d30.20,/) )
!
!     Normal Conclusion.
      endif
      END
!******************************************************************
      SUBROUTINE WOBP (CFBASE, STAR, EARTH, RPN2K, RS2K, SITEV)
      Implicit None
!
!    WOBP is the partial derivatives section of the Wobble module. It computes
!    the partial derivatives of the delay and rate with respect to the polar
!    motion offsets.
!
!     Calling sequence -
!           Input Variables:
!             1. CFBASE(3)   - The crust fixed baseline vector. (m)
!             3. RPN2K(3,3,2)- The nutation portion of the complete crust fixed
!                              to J2000.0 rotation matrix and its CT time
!                              derivative. (unitless, 1/sec)
!             5. RS2K(3,3,3) - The diurnal spin portion of the complete crust
!                              fixed to J2000.0 rotation matrix and its first
!                              two CT time derivatives. (unitless, 1/s, 1/s**2)
!             6. STAR(3)     - The J2000.0 source unit vector.
!             7. EARTH(3,3)  - The SSBC position, velocity, and acceleration
!                              vectors of the Earth. (m, m/s, m/s**2)
!             8. SITEV(3,2)  - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF
!                              EACH OBSERVATION SITE. (M/SEC)
!
!     Common blocks used -
!
      INCLUDE 'cphys11.i'
!            Variables 'FROM':
!              1. VLIGHT    -  The velocity of light in a vacuum. (m/s)
!              2. VLIGHT2   -  The velocity of light squared. (m**2/s**2)
!
      INCLUDE 'cmwob11.i'
!            Variables 'FROM':
!              1. RWOBX(3,3) - The rotation matrix which rotates about the crust
!                              fixed Y-axis by an angle equal to WOBX.
!              2. RWOBY(3,3) - The rotation matrix which rotates about the new
!                              X-axis by an angle equal to WOBY. (unitless)
!              3. WOBX       - The left-handed X-wobble. (rad)
!              4. DWOBX      - Time derivative of WOBX. (rad/sec)
!              5. WOBY       - The left-handed Y-wobble. (rad)
!              6. DWOBY      - Time derivative of WOBY. (rad/sec)
!              7. RWX(3,3)   - Rotation matrix used in computation of time
!                              derivative of wobble rotation matrix.
!              8. RWY(3,3)   - Rotation matrix used in computation of time
!                              derivative of wobble rotation matrix.
!              9. RWZ(3,3)   - Rotation matrix used in computation of time
!                              derivative of wobble rotation matrix.
!            Variables 'TO':
!              1. DWOBP(2,2) - The partial derivatives of the delay and rate
!                              with respect to the X and Y polar motion. The
!                              first index runs over the X and Y offsets
!                              and the second runs over delay and rate.
!                              (s/radian, (s/s)/radian)
!
      INCLUDE 'ccon.i'
!            Variables 'FROM':
!              1. KWOBC - The wobble module flow control flag.
!              2. KWOBD - The wobble module debug output flag.
!
!     Program specifications -
!
      Real*8  CFBASE(3), DWOBDX(3,3), DWOBDY(3,3), STAR(3),WOBXDX(3,3), &
     &        WOBYDY(3,3), XBASE(3,2), XR2000(3,3,2), YBASE(3,2), &
     &        YR2000(3,3,2),EARTH(3,3), VG(3), c1, c2, tt, DOTP, &
     &        RPN2K(3,3,2), RS2K(3,3,3), SITEV(3,2), VE(3)
      Real*8  pRWdx(3,3), pRWdy(3,3), prwxdx(3,3), prwydy(3,3), &
     &        pRWx1(3,3), pRWx2(3,3), pRWy1(3,3), pRWy2(3,3), &
     &        pdRWdtdx(3,3), pdRWdtdy(3,3), XRW2000(3,3,2), &
     &        YRW2000(3,3,2), rx1(3,3), rx2(3,3), rx3(3,3), rx4(3,3), &
     &        ry1(3,3), ry2(3,3), ry3(3,3), ry4(3,3), pRW2Kx(3,3,2), &
     &        pRW2Ky(3,3,2), pDRWX1(3,3), pDRWX2(3,3),pDRWX3(3,3), &
     &        pDRWY1(3,3), pDRWY2(3,3),pDRWY3(3,3), pR2Kx(3,3,2), &
     &        pR2Ky(3,3,2), dR2Kx1(3,3), dR2Kx2(3,3), dR2Kx3(3,3), &
     &        dR2Ky1(3,3), dR2Ky2(3,3), dR2Ky3(3,3)
      Integer*4 K, I
!
!    Subroutine Interface -
!             Caller subroutines: DRIVP
!             Called subroutines: DOTP, DROTT, MMUL3, MADD3, VECRT
!
!    Constants used - VLIGHT, VLIGHT2
!
!    Program Variables -
!           1. pRWdx(3,3)    - The partial derivative of the wobble matrix with
!                              respect to the x-offset. (1/radian)
!           2. pRWdy(3,3)    - The partial derivative of the wobble matrix with
!                              respect to the y-offset. (1/radian)
!           3. WOBXDX(3,3)   - The partial derivative of the rotation matrix
!                              RWOBX with respect to the X offset. (1/radian)
!           4. WOBYDY(3,3)   - The partial derivative of the rotation matrix
!                              RWOBY with respect to the Y offset. (1/radian)
!           5. XBASE(3,2)    - The partial derivatives of the J2000.0 baseline
!                              position and velocity vectors with respect to
!                              the X offset. (m/radian, (m/s)/radian)
!           6. YBASE(3,2)    - The partial derivatives of the J2000.0 baseline
!                              position and velocity vectors with respect to
!                              the Y offset. (m/radian, (m/s)/radian)
!           7. XR2000(3,3,2) - The partial derivative of the complete crust
!                              fixed to J2000.0 rotation matrix and its first
!                              CT time derivative with respect to the X offset.
!                              (1/radian,(1/x)/radian)
!           8. YR2000(3,3,2) - The partial derivative of the complete crust
!                              fixed to J2000.0 rotation matrix and its first
!                              CT time derivative with respect to the Y offset.
!                              (1/radian, (1/s)/radian)
!           9. vg(3)         - A local copy of the SSBC velocity vector of the
!                              Earth. See EARTH.
!          10. c1, c2, tt    - Temporary variables used to simplify the
!                              expression for computing the partials.
!          11. pRWxdx        - Partial derivative of RWX w.r.t. X-pole.
!          12. pRWydy        - Partial derivative of RWY w.r.t. Y-pole.
!
!     Programmer - Dale Markham 02/17/77
!      77.07.14  Peter Denatale
!      77.03.28  Kathy Watts
!      77.12.23  Bruce Schupler
!      89.07.26  Jim Ryan  Documentation simplifed and strings.
!      89.10.05  Jim Ryan  CPHYS common made an include file.
!      89.12.12  Jim Ryan  UNIX-like database interface implimented.
!      91.06.06  Jim Ryan  Documentation cleanup.
!      91.11.25  Jim Ryan  Second term in Shapiro's model added to
!                          the computation of the partials.
!      94.04.15  D. Gordon Converted to Implicit None.
!      94.05.23  D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
!                          being dimensioned both Logical*2 and Real*8.
!      95.11.27  D. Gordon Corrected error in second part of DWOBP(2,1).
!      96.02.09  D. Gordon Added dwobx, dwoby, rwx, and rwy to Common/WOBCM/.
!                          Put in all 4 terms for computation of the partials
!                          of the first derivative of the J2000.0 rotation
!                          matrix w.r.t X-pole and Y-pole. Changed DWOBDX to
!                          pRWdx and DWOBDY to pRWdx and reversed order of
!                          multiplication to compute them.
!      98.05.01  D. Gordon Common /WOBCM/ moved into include file cmwob.i.
!      2002 Sept  Jim Ryan Integer*2/4 mods.
!      2003/2004  D.Gordon Modified for CEO-based rotation computations.
!      2004.11.09 D.Gordon Modified partial to use Consensus model.
!      Jan. 2013  D.Gordon Moved PUT to subroutine P_PUT.
!
!    WOBP Program Structure
!
!    Compute the partial derivatives of the rotation matrices RWOBX and RWOBY
!    with respect to the long period X and Y offsets, respectively.
      CALL DROTT ( WOBX, 1.D0, int2(2), WOBXDX)
      CALL DROTT ( WOBY, 1.D0, int2(1), WOBYDY)
!
!   Compute the partial derivatives of RWX w.r.t. X-pole and of RWY w.r.t.
!    Y-pole.
        Call DDROT ( WOBX, DWOBX, int2(2), pRWxdx)
        Call DDROT ( WOBY, DWOBY, int2(1), pRWydy)
!
!**************************
!   Compute partials of the new CEO based wobble rotation matrix,
!    RW2K(1,1,1), with respect to X-pole and Y-pole.
!  Below agrees with POM2000 and IERS Conventions (2003)
      CALL MMUL3 ( RWOBZ, WOBXDX, RWOBY, pRW2Kx(1,1,1) )
      CALL MMUL3 ( RWOBZ,  RWOBX, WOBYDY,pRW2Ky(1,1,1) )
!
!   Compute partials of the time derivative of the new CEO based
!    wobble rotation matrix,
!    RW2K(1,1,2), with respect to X-pole and Y-pole.
!
!  Below agrees with POM2000 and IERS Conventions (2003)
      CALL MMUL3 ( RWOBZ, WOBXDX,   RWY, pDRWX1 )
      CALL MMUL3 ( RWOBZ, pRWXDX, RWOBY, pDRWX2 )
      CALL MMUL3 (   RWZ, WOBXDX, RWOBY, pDRWX3 )
      CALL MADD3 ( pDRWX1, pDRWX2, pDRWX3, pRW2Kx(1,1,2) )
!
!  Below agrees with POM2000 and IERS Conventions (2003)
      CALL MMUL3 ( RWOBZ, RWOBX, pRWYDY, pDRWY1 )
      CALL MMUL3 ( RWOBZ,   RWX, WOBYDY, pDRWY2 )
      CALL MMUL3 (   RWZ, RWOBX, WOBYDY, pDRWY3 )
      CALL MADD3 ( pDRWY1, pDRWY2, pDRWY3, pRW2Ky(1,1,2) )
!
!    Compute partials of CEO-based J2000 rotation matrix, R2K(3,3,2),
!       w.r.t. X-pole and Y-pole.
      CALL MMUL3 (RPN2K(1,1,1),RS2K(1,1,1),pRW2Kx(1,1,1),pR2Kx(1,1,1))
      CALL MMUL3 (RPN2K(1,1,1),RS2K(1,1,1),pRW2Ky(1,1,1),pR2Ky(1,1,1))
!
!    Compute partials of the time derivative of R2K(3,3,2) w.r.t. X-pole
!     and Y-pole.
!   Compute the three terms necessary for the X-pole calculation.
      CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,1), pRW2Kx(1,1,1), dR2Kx1 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,2), pRW2Kx(1,1,1), dR2Kx2 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,1), pRW2Kx(1,1,2), dR2Kx3 )
!   Add the three terms to complete the calculation.
      CALL MADD3 ( dR2Kx1, dR2Kx2, dR2Kx3, pR2Kx(1,1,2 ) )
!   Compute the three terms necessary for the Y-pole calculation.
      CALL MMUL3 ( RPN2K(1,1,2), RS2K(1,1,1), pRW2Ky(1,1,1), dR2Ky1 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,2), pRW2Ky(1,1,1), dR2Ky2 )
      CALL MMUL3 ( RPN2K(1,1,1), RS2K(1,1,1), pRW2Ky(1,1,2), dR2Ky3 )
!   Add the three terms to complete the calculation.
      CALL MADD3 ( dR2Ky1, dR2Ky2, dR2Ky3, pR2Ky(1,1,2 ) )
!
!***************************************************
!
!    Compute the partial derivatives of the J2000.0 baseline position and
!    velocity vectors with respect to the long period wobble X and Y offsets.
!
      DO 300  K = 1,2
!        New CEO-based partials
        CALL VECRT ( pR2Kx (1,1,K), CFBASE, XBASE(1,K) )
        CALL VECRT ( pR2Ky (1,1,K), CFBASE, YBASE(1,K) )
  300 CONTINUE
!
!    Compute the partial derivatives of the delay and the delay rate with
!    respect to the long period wobble X and Y offsets. 
!    Use the Consensus model definition.
        DO I =1,3
          VG(I) = EARTH(I,2) + SITEV(I,2)
          VE(I) = EARTH(I,2)
        Enddo
         TT = 1.d0 + DOTP(STAR,VG)/VLIGHT
!
      DWOBP(1,1) = Dotp(XBASE(1,1),STAR )/VLIGHT/TT +                   &
     &             Dotp(Xbase(1,1),VE)/VLIGHT2
      DWOBP(1,2) = Dotp(XBASE(1,2),STAR )/VLIGHT/TT +                   &
     &             Dotp(Xbase(1,2),VE)/VLIGHT2
      DWOBP(2,1) = Dotp(YBASE(1,1),STAR )/VLIGHT/TT +                   &
     &             Dotp(Ybase(1,1),VE)/VLIGHT2
      DWOBP(2,2) = Dotp(YBASE(1,2),STAR )/VLIGHT/TT +                   &
     &             Dotp(Ybase(1,2),VE)/VLIGHT2
!
!    'PUT' of the wobble partial derivatives, DWOBP, moved to sub. PUT_P.
!
!    Check KWOBD for debug.
      If (KWOBD .ne. 0) then
      WRITE (6,9)
    7 FORMAT(A,3D25.16,5(/,9X,3D25.16))
    8 FORMAT(A,4D25.16/(7X,5D25.16))
    9 FORMAT (1X, 'DEBUG OUTPUT FOR SUBROUTINE WOBP.' )
!        Write(6,1020) WOBXDX
!1020    Format(1x,'WOBP/WOBXDX: ',(3(/,3E25.15)))
!        Write(6,1021) WOBYDY
!1021    Format(1x,'WOBP/WOBYDY: ',(3(/,3E25.15)))
!        Write(6,1022) pRWxdx
!1022    Format(1x,'WOBP/pRWxdx ',(3(/,3E25.15)))
!        Write(6,1023) pRWydy
!1023    Format(1x,'WOBP/pRWydy ',(3(/,3E25.15)))
!        Write(6,1025) pRW2Kx
 1025    Format(1x,'WOBP/pRW2Kx: ',(6(/,3E25.15)))
!        Write(6,1026) pRW2Ky
 1026    Format(1x,'WOBP/pRW2Ky: ',(6(/,3E25.15)))
!        Write(6,1028) pR2Kx
 1028    Format(1x,'WOBP/pR2Kx: ',(6(/,3E25.15)))
!        Write(6,1029) pR2Ky
 1029    Format(1x,'WOBP/pR2Ky: ',(6(/,3E25.15)))
      WRITE(6,7)' WOBX, DWOBX ', WOBX, DWOBX
      WRITE(6,7)' WOBY, DWOBY ', WOBY, DWOBY
      WRITE(6,7)' WOBXDX  ', WOBXDX
      WRITE(6,7)' WOBYDY  ', WOBYDY
      WRITE(6,7)' RWOBX   ',RWOBX
      WRITE(6,7)' RWOBY   ',RWOBY
      WRITE(6,7)' RWX     ', RWX
      WRITE(6,7)' pRWxdx  ', pRWxdx
      WRITE(6,7)' RWY     ', RWY
      WRITE(6,7)' pRWydy  ', prwydy
      WRITE(6,7)' pRW2Kx  ', pRW2Kx
      WRITE(6,7)' pRW2Ky  ', pRW2Ky
      WRITE(6,7)' pR2Kx   ', pR2Kx
      WRITE(6,7)' pR2Ky   ', pR2Ky
      WRITE(6,8)' DWOBP   ',DWOBP
      WRITE(6,8)' VLIGHT, VLIGHT2 ', VLIGHT, VLIGHT2
      WRITE(6,8)' XBASE   ',XBASE
      WRITE(6,8)' YBASE   ',YBASE
      WRITE(6,8)' C1, C2, TT ', C1, C2, TT
      WRITE(6,8)' VG      ',VG
      WRITE ( 6, 9200 )  CFBASE, RPN2K, RS2K, STAR
 9200 FORMAT (1X, 'CFBASE = ', 3 ( D30.16, 10X ), /, 1X, &
     &            'RPN2K  = ', 6 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'RS2K   = ', 9 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            'STAR   = ', 3 ( D30.16))
!
!     Normal conclusion.
      endif
      END
!***************************************************************************
      SUBROUTINE WOBC(Xti, Yti, dXti, dYti)
      Implicit None
!
!    WOBC is the contributions section of the wobble module. It computes the
!    contributions to the delay and rate due to polar motion X and Y offsets.
!
!     Common blocks used -
!
      INCLUDE 'cmwob11.i'
!            Variables 'FROM':
!              1. DWOBP(2,2) - The polar motion partials. (See above.)
!              2. WOBX       - The left-handed X-wobble. (rad)
!              3. DWOBX      - Time derivative of WOBX. (rad/sec)
!              4. WOBY       - The left-handed Y-wobble. (rad)
!              5. DWOBY      - Time derivative of WOBY. (rad/sec)
!              6. WOBXn      - Short period X libration contribution. (rad)
!              7. WOBYn      - Short period Y libration contribution. (rad)
!              8. dWOBXn     - Time derivative of WOBXn. (rad/sec)
!              9. dWOBYn     - Time derivative of WOBYn. (rad/sec)
!             10. WOBXli     - Short period X libration contribution. (rad)
!             11. WOBYli     - Short period Y libration contribution. (rad)
!             12. dWOBXli    - Time derivative of WOBXn. (rad/sec)
!             13. dWOBYli    - Time derivative of WOBYn. (rad/sec)
!             14. WOBXti     - Short period X tidal contribution. (rad)
!             15. WOBYti     - Short period Y tidal contribution. (rad)
!
      INCLUDE 'ccon.i'
!            Variables 'FROM':
!              1. KWOBC  -  The wobble module flow control flag.
!              2. KWOBD  -  The wobble module debug output flag.
!
!   Common blocks used:
      INCLUDE 'put2s.i'
!       Variables from:
!         1. DWOBXC(2)  - The contributions to the delay and rate due to
!                         the X-pole offset. (s, s/s)
!         2. DWOBYC(2)  - The contributions to the delay and rate due to
!                         the Y-pole offset. (s, s/s)
!         3. DWOBlib(2) - The contributions to the delay and rate due to
!                         the X and Y short period libration. (s, s/s)
!         4. DWOBtid(2) - The contributions to the delay and rate due to
!                         the X and Y ocean tides. (s, s/s)
!
! 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!            Variables 'FROM':
!              1. CONVDS  -  The conversion factor of radians per arcsecond.
!                            (radians/arcsecond).
!
!    Program specifications -
      Real*8 Xti, Yti, dXti, dYti
      Integer*4 K, J
!
!     Subroutine interface -
!             Caller subroutines: DRIVC
!             Called subroutines: 
!
!     Program variables - None
!
!     Programmer - Dale Marknam  02/17/77
!      77.07.14  Peter Denatale
!      77.03.28  Kathy Watts
!      77.12.23  Bruce Schupler
!      89.07.26  Jim Ryan  Documentation simplifed and strings.
!      89.12.12  Jim Ryan  UNIX-like database interface implimented.
!      91.06.06  Jim Ryan  Documentation cleaned up.
!      91.12.17  D. Gordon Bug fixed. For KWOBC = 2 or 3, contributions were
!                          incorrectly being set to zero. (Calc 7.n versions)
!      94.04.15  D.Gordon  Converted to Implicit None.
!      94.05.23  D.Gordon  Fixed bug - use_cubic, use_linear, use_spline were
!                          being dimensioned both Logical*2 and Real*8.
!      94.06.29  D.Gordon  Wobble contribution (DWOBC(2,2)) split into X and Y
!                          components (dwobxc(2) and dwobyc(2)) for
!                          compatability with SOLVE.
!      95.10.05  D.Gordon  Corrected wobble contributions when KWOBC=1 (Wobble
!                          module turned OFF). DO loops restructured.
!      95.11.27  D.Gordon  Added DWOBXC and DWOBYC to debug printout, removed
!                          DWOBC which is no longer used. Documentation
!                          corrected.
!      98.05.01  D. Gordon Common /WOBCM/ moved into include file cmwob.i.
!      2002 Sept Jim Ryan  Integer*2/4 mods.
!      2004.08.10 D. Gordon Added contributions for short period tidal
!                           and nutation polar motion.
!      2012Oct/Nov D.Gordon Mods for IERS 2010. Short period 'nutation'
!                           changed to 'libration'. 'WOBNUTAT' lcode changed
!                           to 'WOBLIBRA'. 
!      Jan. 2013 D. Gordon  Moved all PUT's to subroutine PUT_C.
!
!  WOBC Program Structure
!
!   Compute the contributions.
      Do K=1,2
        DWOBXC(k) = DWOBP(1,K) * WOBX
        DWOBYC(k) = DWOBP(2,K) * WOBY
      Enddo
!         print *,' DWOBXC (psec): ', DWOBXC(1)*1.D12,DWOBXC(2)*1.D12
!         print *,' DWOBYC (psec): ', DWOBYC(1)*1.D12,DWOBYC(2)*1.D12
!        print *,' DWOBP(1,1)/(1,2):', DWOBP(1,1), DWOBP(1,2)         
!        print *,' DWOBP(2,1)/(2,2):', DWOBP(2,1), DWOBP(2,1)         
!
!        print *,' WOBXti:         ', WOBXti
!        print *,' WOBXli, dWOBXli:', WOBXli, dWOBXli
!
!   Compute the short period (libration/nutation and tidal) contributions.
      DO K=1,2
        DWOBlib(K) = DWOBP(1,K)*WOBXli + DWOBP(2,K)*WOBYli
!
        DWOBorth(K) = DWOBP(1,K)*WOBXti + DWOBP(2,K)*WOBYti            
!       DWOBorth(K) = DWOBP(1,K) * Xti*CONVDS*1.0D-3 + &
!    &                DWOBP(2,K) * Yti*CONVDS*1.0D-3
      ENDDO
!      print *,' DWOBorth (psec): ', DWOBorth(1)*1.D12,DWOBorth(2)*1.D12
!      print *,' DWOBlib  (psec): ', DWOBlib(1)*1.D12, DWOBlib(2)*1.D12
!
!   Check KWOBC to see if wobble is to be turned off.
      IF (KWOBC .EQ. 1)  Then
        Do K=1,2
            DWOBXC(K) = 0.d0
            DWOBYC(K) = 0.d0
        Enddo
      ENDIF
!
!   PUT the wobble contributions => moved to subroutine PUT_C.
!
  300 Continue
!
!   Check KWOBD for debug output.
      IF ( KWOBD .EQ. 0 )  GO TO 600
      WRITE ( 6, 9 )
    9 FORMAT (1X, 'Debug output for subroutine WOBC.' )
      WRITE(6,8)' DWOBXC  ',DWOBXC
      WRITE(6,8)' DWOBYC  ',DWOBYC
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DWOBP   ',DWOBP
      WRITE(6,8)' WOBX, WOBY ', WOBX, WOBY
      WRITE(6,8)' DWOBlib  ', DWOBlib
      WRITE(6,8)' DWOBorth ', DWOBorth
!     WRITE(6,8)' DWOBP   ',DWOBP(1,1),DWOBP(2,1),DWOBP(1,2),DWOBP(2,2)
!     print *,  ' DWOBXC (psec)  ', DWOBXC(1)*1.D12, DWOBXC(2)*1.D12
!     print *,  ' DWOBYC (psec)  ', DWOBYC(1)*1.D12, DWOBYC(2)*1.D12
!     print *,  ' DWOBlib (psec)  ', DWOBlib(1)*1.D12, DWOBlib(2)*1.D12
!     print *,  ' DWOBtid (psec)  ', DWOBtid(1)*1.D12, DWOBtid(2)*1.D12
!
!     Normal conclusion.
  600 RETURN
      END
!*************************************************************************
      SUBROUTINE WOBMU(XJD,tab_time,WOBXL,WOBYL,DWOBXL,DWOBYL)
      Implicit None
!
!    WOBMU is the Wobble module utility. It provides interpolated values of the
!    X and Y wobble at the epochs specified in the calling sequence. If the
!    specified epoch is outside the range of the interpolation table, the
!    program issues a message and stops.
!
!     Calling sequence - CALL WOBMU(XJD,UTC,WOBXL,WOBYL)
!
!      Input Variables:
!        1) XJD     -  The Julian date at 0:00 hours UTC of the epoch for
!                      interpolation. (days)
!        2) tab_time - The fraction of the day in the timescale of the
!                      input EOP series (CT, UTC, or ???).
!
!      Output variables:
!        1) WOBXL  -  The interpolated value of the X wobble.
!                     (left-handed, milliarcsec)
!        2) WOBYL  -  The interpolated value of the Y wobble.
!                     (left-handed, milliarcsec)
!
!     Common blocks used -
!
      INCLUDE 'cmwob11.i'
!        Variables 'FROM':
!          1) WOBIF(3)    - The wobble information array. (See above.)
!          2) XYWOB(2,10) - The tabular points for the X & Y offsets.
!                           (milliarcsec)
!
      INCLUDE 'ccon.i'
!     Variables 'FROM':
!        1) KWOBC - The Wobble module flow control flag.
!                   0 --> Default, module on; spline interpolation for 1-day
!                         series, cubic for 5-day series.
!                   1 --> Module off. No polar motion applied.
!                   2 --> Module on; linear interpolation for any series.
!                   3 --> Module on; cubic interpolation for 1-day series,
!                         spline for 5-day series.
!        2) KWOBD - The wobble module debug control flag.
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!            Variables 'FROM':
!              1. SECDAY - The number of seconds in a day (s/day).
!
!     Program specifications -
!
      Real*8 XJD, UTC, WOBXL, WOBYL, T, DWOBXL, DWOBYL, S, tab_time
      Real*8 XINT(4,2), F2(2), Y1(2,2), Y2(2,2)
      Real*8 x_spline, yx_spline, yy_spline, yxdot, yydot, yxdot2, &
     &       yydot2, yxdot3, yydot3
      Integer*4 INT, I, N, NN, NR, ILAST, ierr4
!
!     Subroutine interface:
!         Caller subroutines: WOBI, WOBG
!         Called subroutines: SPLINT4
!
!     Programmer - Bruce Schupler 12/23/77
!      89.07.26  Jim Ryan  Documentation simplifed and strings.
!      90.05.06  Jim Ryan  Mods for linear interpolation.
!      91.06.06  Jim Ryan  Linear interpolation mods and clean up.
!      93.12.17  D. Gordon Spline interpolation mods.
!      94.04.13  D. Gordon DFLOT changed to Fortran 77 DFLOTJ.
!      94.04.15  D. Gordon Converted to Implicit None.
!      94.05.23  D. Gordon Fixed bug - use_cubic, use_linear, use_spline were
!                          being dimensioned both Logical*2 and Real*8.
!      95.06.09  D. Gordon Cleaned up debug printout.
!      95.12.11  D. Gordon Adding computation of time derivatives of X- and
!                          Y-wobble for linear interpolation.
!      95.12.28  D. Gordon Adding computation of time derivatives of X- and
!                          Y-wobble for cubic polynomial interpolation. Debug
!                          printout mods and additions.
!      98.05.01  D. Gordon Common /WOBCM/ moved into include file cmwob.i.
!      2002 Sept Jim Ryan  Integer*2/4 mods.
!
!     WOBMU Program Structure
!
!    Convert UTC and XJD to the units of the interval of the table relative to
!    the first point of the table. INT is computed so that INT+2 is the number
!    of the tabular point just before (or at) the time of the observation and
!    INT+3 is the number of the point after the time of the observation.
!
      T =  (XJD  -  WOBIF(1))
      T = (T + tab_time) / WOBIF(2)
      INT = T
      T =  T - DFLOAT (INT)
      INT = INT - 1
!
!**************************************
!   Code for cubic spline interpolation
      IF (use_spline) then     ! Spline interpolation
!
!   Compute time of obs. and divide by interval
        x_spline = (xjd + tab_time)/ WOBIF(2)
!
!   In case the interval (WOBIF(2)) is not 1.0 days, divide by interval
!       x_spline = x_spline / WOBIF(2)
!
!  Perform cubic spline for X-wobble interpolation
        call splint4(xa,yax,y2sx,n_spline,x_spline,yx_spline,yxdot, &
     &               yxdot2,yxdot3,ierr4)
        WOBXL = yx_spline
        DWOBXL = yxdot / (WOBIF(2) * SECDAY)
!
!  Perform cubic spline for Y-wobble interpolation
        call splint4(xa,yay,y2sy,n_spline,x_spline,yy_spline,yydot, &
     &               yydot2,yydot3,ierr4)
        WOBYL = yy_spline
        DWOBYL = yydot / (WOBIF(2) * SECDAY)
!
      Endif               ! Spline interpolation
!
!**************************************
!   Code for cubic polynomial interpolation
      If(use_cubic) Then            !3rd order polynomial interpolation
!
!   Select the four tabular points from each of the XWOB and YWOB arrays and
!   verify that the interpolation is not outside the range of the table.
!   (NOTE: If the interpolation is outside the range of the table, a message is
!    written and the program terminates.)
!
        ILAST = IDINT ( WOBIF(3) )
        Do N = 1,4
          NN = INT  +  N
          IF ( ( NN .LT. 1 ) .OR. ( NN .GT. ILAST ) )  GO TO 900
          XINT(N,1) = XYWOB(1,NN)
          XINT(N,2) = XYWOB(2,NN)
        Enddo
!
!    Do the cubic interpolation.
        DO I = 1,2
          DO N = 1,2
            NR = N + 1
            F2(I) = ( XINT ( NR+1, I ) + XINT ( NR-1, I ) ) / 6.D0
            Y1(N,I) = + ( 4.D0 / 3.D0 ) * XINT ( NR, I ) - F2(I)
            Y2(N,I) = - ( 1.D0 / 3.D0 ) * XINT ( NR, I ) + F2(I)
          enddo
        enddo
!
       S = 1.D0 - T
       WOBXL = T * ( Y1(2,1)  +  T**2 * Y2(2,1) ) &
     &       + S * ( Y1(1,1)  +  S**2 * Y2(1,1) )
       WOBYL = T * ( Y1(2,2)  +  T**2 * Y2(2,2) ) &
     &       + S * ( Y1(1,2)  +  S**2 * Y2(1,2) )
!
       DWOBXL = ( Y1(2,1) + (3.D0*T**2*Y2(2,1)) &
     &          - Y1(1,1) - (3.D0*S**2*Y2(1,1)) ) / (WOBIF(2)*SECDAY)
       DWOBYL = ( Y1(2,2) + (3.D0*T**2*Y2(2,2)) &
     &          - Y1(1,2) - (3.D0*S**2*Y2(1,2)) ) / (WOBIF(2)*SECDAY)
!
      Endif                         !3rd order polynomial interpolation
!
!**************************************
!   Code for linear interpolation
      If(use_linear) Then           !2 point linear interpolation
        If(INT+1 .lt. 1  .or. &
     &     INT+1 .gt. DINT(WOBIF(3)+.001)) Then
          Write(6,'( &
     &    "Error in WOBMU! Attempted to interpolate polar motion",/, &
     &    "outside of PM table. INT =",i5)') INT
       Call TERMINATE_CALC( 'WOBMU ', int2(0), int2(0))
        Endif
!
!   Interpolate linearly. Don't change the units.
        WOBXL = (XYWOB(1,INT+3)-XYWOB(1,INT+2))*T + XYWOB(1,INT+2)
        WOBYL = (XYWOB(2,INT+3)-XYWOB(2,INT+2))*T + XYWOB(2,INT+2)
!   Compute time derivative.
        DWOBXL =  (XYWOB(1,INT+3)-XYWOB(1,INT+2)) / (WOBIF(2) * SECDAY)
        DWOBYL =  (XYWOB(2,INT+3)-XYWOB(2,INT+2)) / (WOBIF(2) * SECDAY)
!
      Endif                         !2 point linear interpolation
!
!**************************************
!    Check to see if debug output is requested.
      IF (KWOBD .NE. 1) GO TO 400
      WRITE(6,9)
      If(use_spline) write(6,'(" Spline interpolation used.")')
      If(use_cubic) write(6,'(" Cubic polynomial interpolation used.")')
      If(use_linear) write(6,'(" Linear interpolation used.")')
      WRITE(6,8)' T       ',T
      WRITE(6,7)' INT     ',INT
      WRITE(6,8)' XYWOB   ',XYWOB
      WRITE(6,9120) XJD, tab_time, WOBXL, WOBYL, DWOBXL, DWOBYL
!
      if (use_spline) then
        WRITE(6,8)' x_spline ', x_spline
        write (6,'("  xa: ",5d20.10,3(/,6x,5d20.10))') xa
        write (6,'("n_spline",i5,"x_spline ",d25.16)') n_spline,x_spline
        write (6,'(" yax: ",5d20.10,3(/,6x,5d20.10))') yax
        write (6,'("y2sx: ",5d20.10,3(/,6x,5d20.10))') y2sx
        write (6,8)' yx_spline ', yx_spline
        write (6,8)' yxdot, yxdot2, yxdot3 ', yxdot, yxdot2, yxdot
        write (6,'(" yay: ",5d20.10,3(/,6x,5d20.10))') yay
        write (6,'("y2sy: ",5d20.10,3(/,6x,5d20.10))') y2sy
        write (6,8)' yy_spline ', yy_spline
        write (6,8)' yydot, yydot2, yydot3 ', yydot, yydot2, yydot
      endif
!
      if (use_cubic) then
        WRITE(6,7)' ILAST ', ILAST
        WRITE(6,8)' XINT  ', XINT
        WRITE(6,8)' F2    ', F2
        WRITE(6,8)' Y1    ', Y1
        WRITE(6,8)' Y2    ', Y2
        WRITE(6,8)' S     ',  S
      endif
!
    7 FORMAT(A,15I8/(9X,15I8))
    8 FORMAT(A,4D25.16/(7X,5D25.16))
    9 FORMAT(1X,'Debug output for subroutine WOBMU')
 9120 FORMAT(1X,'   XJD = ',D30.16,3X,'tab_time = ',D30.16,/, &
     &       1X,' WOBXL = ',D30.16,3X,'   WOBYL = ',D30.16,/, &
     &       1X,'DWOBXL = ',D30.16,3X,'  DWOBYL = ',D30.16)
!
!     Normal Conclusion.
  400 RETURN
!
!     Abnormal conclusion.
  900 WRITE ( 6, 9300 )  NN, ILAST
 9300 FORMAT (1X, ' CALC has terminated in subroutine WOBMU.',/, &
     & ' The interpolation is outside the range of the wobble table.', &
     & /,'  NN = ', I2, ' ILAST = ', I2, '.' )
      CALL TERMINATE_CALC ( 'WOBMU ', int2(0), int2(0))
      END
!
!**************************************************************************
      SUBROUTINE XYLIB (GMST2K, FA2K, FAD2K, Xp, Yp, dXp, dYp)
      IMPLICIT None
!
!     Input Variables:
!          GMST2K(2) = Greenwich mean siderial time and its first time
!                      derivative (radians, radians/sec)
!          FA2K(14)  = Fundamental arguments from subroutine NUTFA (radians)
!          FAD2K(14) = Time derivatives of fundamental arguments (rad/sec)
!     Output Variables:
!          Xp  = Short term nutation correction to X-polar motion (mas)
!          Yp  = Short term nutation correction to Y-polar motion (mas)
!          dXp = Time derivative of Short term nutation correction to 
!                X-polar motion (mas/sec)
!          dYp = Time derivative of Short term nutation correction to 
!                Y-polar motion (mas/sec)
!
      REAL*8 GMST2K(2), Xp, Yp, dXp, dYp
      REAL*8 L, LP, fa(5), fad(5), ARG, dARG
      Integer*4 N, I
      REAL*8 XS(10,10), X1(100)
      EQUIVALENCE(XS(1,1),X1(1))
!
      INCLUDE 'ccon.i'
!
      REAL*8 FA2K(14), FAD2K(14)
!     COMMON / NFA2K / FA2K, FAD2K
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!          Variables 'from':
!            1. SECDAY -  The number of seconds in a day. (s/day)
!            2. TWOPI  -  PI times 2.0D0
!            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!  N=Number of tidal terms to be used (10 for full set).
!  (This was an argument in the original zontids routine.)
      DATA  N/10/
!
!***********************************************************************
!     Short period nutation terms
!     From Table 5.1, IERS Conventions (2003)
!
!                  Multiple of                  Xp             Yp
!              l   l'   F    D  OMEGA Chi   sin    cos     sin     cos
!     DATA X1/-1., 0., -2.,  0., -1., 1.,  -0.44,  0.25,  -0.25,  -0.44, &
!    &        -1., 0., -2.,  0., -2., 1.,  -2.31,  1.32,  -1.32,  -2.31, &
!    &         1., 0., -2., -2., -2., 1.,  -0.44,  0.25,  -0.25,  -0.44, &
!    &         0., 0., -2.,  0., -1., 1.,  -2.14,  1.23,  -1.23,  -2.14, &
!    &         0., 0., -2.,  0., -2., 1., -11.36,  6.52,  -6.52, -11.36, &
!    &        -1., 0.,  0.,  0.,  0., 1.,   0.84, -0.48,   0.48,   0.84, &
!    &         0., 0., -2.,  2., -2., 1.,  -4.76,  2.73,  -2.73,  -4.76, &
!    &         0., 0.,  0.,  0.,  0., 1.,  14.27, -8.19,   8.19,  14.27, &
!    &         0., 0.,  0.,  0., -1., 1.,   1.93, -1.11,   1.11,   1.93, &
!    &         1., 0.,  0.,  0.,  0., 1.,   0.76, -0.43,   0.43,   0.76/
!
!***********************************************************************
!     Short period nutation terms
!     From Table 5.1a, IERS Conventions (2010)
!
!                  Multiple of                  Xp             Yp
!              l   l'   F    D  OMEGA Chi   sin    cos     sin     cos
      DATA X1/-1., 0., -2.,  0., -1., 1.,  -0.4 ,  0.3 ,  -0.3 ,  -0.4 , &
     &        -1., 0., -2.,  0., -2., 1.,  -2.3 ,  1.3 ,  -1.3 ,  -2.3 , &
     &         1., 0., -2., -2., -2., 1.,  -0.4 ,  0.3 ,  -0.3 ,  -0.4 , &
     &         0., 0., -2.,  0., -1., 1.,  -2.1 ,  1.2 ,  -1.2 ,  -2.1 , &
     &         0., 0., -2.,  0., -2., 1., -11.4 ,  6.5 ,  -6.5 , -11.4 , &
     &        -1., 0.,  0.,  0.,  0., 1.,   0.8 , -0.5 ,   0.5 ,   0.8 , &
     &         0., 0., -2.,  2., -2., 1.,  -4.8 ,  2.7 ,  -2.7 ,  -4.8 , &
     &         0., 0.,  0.,  0.,  0., 1.,  14.3 , -8.2 ,   8.2 ,  14.3 , &
     &         0., 0.,  0.,  0., -1., 1.,   1.9 , -1.1 ,   1.1 ,   1.9 , &
     &         1., 0.,  0.,  0.,  0., 1.,   0.8 , -0.4 ,   0.4 ,   0.8 /
!***********************************************************************
!
!
      Xp  = 0.0D+0
      Yp  = 0.0D+0
      dXp = 0.0D+0
      dYp = 0.0D+0
!
!     Sum zonal tide terms
!
      DO 10 I=1, N
!   Formation of multiples of arguments
      ARG = XS(1,I)*FA2K(1) + XS(2,I)*FA2K(2) + XS(3,I)*FA2K(3) &
     &    + XS(4,I)*FA2K(4) + XS(5,I)*FA2K(5) + XS(6,I)*(GMST2K(1)+PI)
      ARG = DMOD(ARG,TWOPI)
!     ARG = DMOD(ARG,1296000.0D0) * CONVDS
!   First derivative
      dARG = XS(1,I)*FAD2K(1) + XS(2,I)*FAD2K(2) + XS(3,I)*FAD2K(3) &
     &     + XS(4,I)*FAD2K(4) + XS(5,I)*FAD2K(5) + XS(6,I)*(GMST2K(2))
!
!     Evaluate zonal tidal terms (micro-arcsec)
       Xp  = XS(7,I)*DSIN(ARG) +  XS(8,I)*DCOS(ARG) + Xp
       Yp  = XS(9,I)*DSIN(ARG) + XS(10,I)*DCOS(ARG) + Yp
       dXp = XS(7,I)*DCOS(ARG)*dARG -  XS(8,I)*DSIN(ARG)*dARG + dXp
       dYp = XS(9,I)*DCOS(ARG)*dARG - XS(10,I)*DSIN(ARG)*dARG + dYp
   10 CONTINUE
!
!  Change from micro-arsecs to milli-arcsecs
      Xp  =  Xp*1.0D-3
      Yp  =  Yp*1.0D-3
      dXp = dXp*1.0D-3
      dYp = dYp*1.0D-3
!
!       print *, ' XYNUT: GMST2K:  ', GMST2K
!       print *, ' XYNUT: Xp, dXp: ', Xp, dXp
!       print *, ' XYNUT: Yp, dYp: ', Yp, dYp
!
      RETURN
      END
!
!**************************************************************************
      SUBROUTINE XYTID (GMST2K, FA2K, FAD2K, Xt, Yt, dXt, dYt)
      IMPLICIT None
!
!     Input Variables:
!          GMST2K(2) = Greenwich mean siderial time and its first time
!                      derivative (radians, radians/sec)
!          FA2K(14)  = Fundamental arguments from subroutine NUTFA (radians)
!          FAD2K(14) = Time derivatives of fundamental arguments (rad/sec)
!     Output Variables:
!          Xt =
!          Yt =
!
      REAL*8 GMST2K(2), Xt, Yt, dXt, dYt
      REAL*8 L, LP, fa(5), fad(5), ARG, dARG
      Integer*4 N, I
      REAL*8 XS(11,71), X1(781)
      EQUIVALENCE(XS(1,1),X1(1))
!
      INCLUDE 'ccon.i'
!
      REAL*8 FA2K(14), FAD2K(14)
!     COMMON / NFA2K / FA2K, FAD2K
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!          Variables 'from':
!            1. SECDAY -  The number of seconds in a day. (s/day)
!            2. TWOPI  -  PI times 2.0D0
!            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!  N=Number of tidal terms to be used (71 for full set).
!  (This was an argument in the original zontids routine.)
      DATA  N/71/
!
!***********************************************************************
!     Short period ocean tidal terms
!     From Table 8.2, IERS Conventions (2003)
!                  Multiple of                    Xp            Yp
!        l   l' F   D OMGA Chi   Period       sin    cos    sin    cos
      DATA X1/ &
     & -1., 0.,-2.,-2.,-2., 1., 1.2113611,    0.0,   0.9,   -0.9, -0.1, &
     & -2., 0.,-2., 0.,-1., 1., 1.1671262,    0.1,   0.6,   -0.6,  0.1, &
     & -2., 0.,-2., 0.,-2., 1., 1.1669259,    0.3,   3.4,   -3.4,  0.3, &
     &  0., 0.,-2.,-2.,-1., 1., 1.1605476,    0.1,   0.8,   -0.8,  0.1, &
     &  0., 0.,-2.,-2.,-2., 1., 1.1603495,    0.5,   4.2,   -4.1,  0.5, &
     & -1., 0.,-2., 0.,-1., 1., 1.1196993,    1.2,   5.0,   -5.0,  1.2, &
     & -1., 0.,-2., 0.,-2., 1., 1.1195148,    6.2,  26.3,  -26.3,  6.2, &
     &  1., 0.,-2.,-2.,-1., 1., 1.1136429,    0.2,   0.9,   -0.9,  0.2, &
     &  1., 0.,-2.,-2.,-2., 1., 1.1134606,    1.3,   5.0,   -5.0,  1.3, &
     &  0., 0.,-2., 0., 0., 1., 1.0761465,   -0.3,  -0.8,    0.8, -0.3, &
     &  0., 0.,-2., 0.,-1., 1., 1.0759762,    9.2,  25.1,  -25.1,  9.2, &
     &  0., 0.,-2., 0.,-2., 1., 1.0758059,   48.8, 132.9, -132.9, 48.8, &
     & -2., 0., 0., 0., 0., 1., 1.0750901,   -0.3,  -0.9,    0.9, -0.3, &
     &  0., 0., 0.,-2., 0., 1., 1.0695055,   -0.7,  -1.7,    1.7, -0.7, &
     & -1., 0.,-2., 2.,-2., 1., 1.0406147,   -0.4,  -0.9,    0.9, -0.4, &
     &  1., 0.,-2., 0.,-1., 1., 1.0355395,   -0.3,  -0.6,    0.6, -0.3, &
     &  1., 0.,-2., 0.,-2., 1., 1.0353817,   -1.6,  -3.5,    3.5, -1.6, &
     & -1., 0., 0., 0., 0., 1., 1.0347187,   -4.5,  -9.6,    9.6, -4.5, &
     & -1., 0., 0., 0.,-1., 1., 1.0345612,   -0.9,  -1.9,    1.9, -0.9, &
     &  1., 0., 0.,-2., 0., 1., 1.0295447,   -0.9,  -1.8,    1.8, -0.9, &
     &  0.,-1.,-2., 2.,-2., 1., 1.0055058,    1.5,   3.0,   -3.0,  1.5, &
     &  0., 0.,-2., 2.,-1., 1., 1.0028933,   -0.3,  -0.6,    0.6, -0.3, &
     &  0., 0.,-2., 2.,-2., 1., 1.0027454,   26.1,  51.2,  -51.2, 26.1, &
     &  0., 1.,-2., 2.,-2., 1., 1.0000001,   -0.2,  -0.4,    0.4, -0.2, &
     &  0.,-1., 0., 0., 0., 1., 0.9999999,   -0.6,  -1.2,    1.2, -0.6, &
     &  0., 0., 0., 0., 1., 1., 0.9974159,    1.5,   3.0,   -3.0,  1.5, &
     &  0., 0., 0., 0., 0., 1., 0.9972695,  -77.5,-151.7,  151.7,-77.5, &
     &  0., 0., 0., 0.,-1., 1., 0.9971233,  -10.5, -20.6,   20.6,-10.5, &
     &  0., 0., 0., 0.,-2., 1., 0.9969771,    0.2,   0.4,   -0.4,  0.2, &
     &  0., 1., 0., 0., 0., 1., 0.9945541,   -0.6,  -1.2,    1.2, -0.6, &
     &  0., 0., 2.,-2., 2., 1., 0.9918532,   -1.1,  -2.1,    2.1, -1.1, &
     & -1., 0., 0., 2., 0., 1., 0.9669565,   -0.7,  -1.4,    1.4, -0.7, &
     &  1., 0., 0., 0., 0., 1., 0.9624365,   -3.5,  -7.3,    7.3, -3.5, &
     &  1., 0., 0., 0.,-1., 1., 0.9623003,   -0.7,  -1.4,    1.4, -0.7, &
     &  0., 0., 0., 2., 0., 1., 0.9341741,   -0.4,  -1.1,    1.1, -0.4, &
     &  2., 0., 0., 0., 0., 1., 0.9299547,   -0.2,  -0.5,    0.5, -0.2, &
     &  0., 0., 2., 0., 2., 1., 0.9294198,   -1.1,  -3.4,    3.4, -1.1, &
     &  0., 0., 2., 0., 1., 1., 0.9292927,   -0.7,  -2.2,    2.2, -0.7, &
     &  0., 0., 2., 0., 0., 1., 0.9291657,   -0.1,  -0.5,    0.5, -0.1, &
     &  1., 0., 2., 0., 2., 1., 0.8990932,    0.0,  -0.6,    0.6,  0.0, &
     &  1., 0., 2., 0., 1., 1., 0.8989743,    0.0,  -0.4,    0.4,  0.0, &
     & -3., 0.,-2., 0.,-2., 2., 0.5484264,   -0.5,   0.0,    0.6,  0.2, &
     & -1., 0.,-2.,-2.,-2., 2., 0.5469695,   -1.3,  -0.2,    1.5,  0.7, &
     & -2., 0.,-2., 0.,-2., 2., 0.5377239,   -6.1,  -1.6,    3.1,  3.4, &
     &  0., 0.,-2.,-2.,-2., 2., 0.5363232,   -7.6,  -2.0,    3.4,  4.2, &
     &  0., 1.,-2.,-2.,-2., 2., 0.5355369,   -0.5,  -0.1,    0.2,  0.3, &
     & -1.,-1.,-2., 0.,-2., 2., 0.5281939,    0.5,   0.1,   -0.1, -0.3, &
     & -1., 0.,-2., 0.,-1., 2., 0.5274721,    2.1,   0.5,   -0.4, -1.2, &
     & -1., 0.,-2., 0.,-2., 2., 0.5274312,  -56.9, -12.9,   11.1, 32.9, &
     & -1., 1.,-2., 0.,-2., 2., 0.5266707,   -0.5,  -0.1,    0.1,  0.3, &
     &  1., 0.,-2.,-2.,-2., 2., 0.5260835,  -11.0,  -2.4,    1.9,  6.4, &
     &  1., 1.,-2.,-2.,-2., 2., 0.5253269,   -0.5,  -0.1,    0.1,  0.3, &
     & -2., 0.,-2., 2.,-2., 2., 0.5188292,    1.0,   0.1,   -0.1, -0.6, &
     &  0.,-1.,-2., 0.,-2., 2., 0.5182593,    1.1,   0.1,   -0.1, -0.7, &
     &  0., 0.,-2., 0.,-1., 2., 0.5175645,   12.3,   1.0,   -1.4, -7.3, &
     &  0., 0.,-2., 0.,-2., 2., 0.5175251, -330.2, -27.0,   37.6,195.9, &
     &  0., 1.,-2., 0.,-2., 2., 0.5167928,   -1.0,  -0.1,    0.1,  0.6, &
     & -1., 0.,-2., 2.,-2., 2., 0.5092406,    2.5,  -0.3,   -0.4, -1.5, &
     &  1., 0.,-2., 0.,-2., 2., 0.5079842,    9.4,  -1.4,   -1.9, -5.6, &
     & -1., 0., 0., 0., 0., 2., 0.5078245,   -2.4,   0.4,    0.5,  1.4, &
     & -1., 0., 0., 0.,-1., 2., 0.5077866,   -1.0,   0.2,    0.2,  0.6, &
     &  0.,-1.,-2., 2.,-2., 2., 0.5006854,   -8.5,   3.5,    3.3,  5.1, &
     &  0., 0.,-2., 2.,-2., 2., 0.5000000, -144.1,  63.6,   59.2, 86.6, &
     &  0., 1.,-2., 2.,-2., 2., 0.4993165,    1.2,  -0.6,   -0.5, -0.7, &
     &  0., 0., 0., 0., 1., 2., 0.4986714,    0.5,  -0.2,   -0.2, -0.3, &
     &  0., 0., 0., 0., 0., 2., 0.4986348,  -38.5,  19.1,   17.7, 23.1, &
     &  0., 0., 0., 0.,-1., 2., 0.4985982,  -11.4,   5.8,    5.3,  6.9, &
     &  0., 0., 0., 0.,-2., 2., 0.4985616,   -1.2,   0.6,    0.6,  0.7, &
     &  1., 0., 0., 0., 0., 2., 0.4897717,   -1.8,   1.8,    1.7,  1.0, &
     &  1., 0., 0., 0.,-1., 2., 0.4897365,   -0.8,   0.8,    0.8,  0.5, &
     &  0., 0., 2., 0., 2., 2., 0.4810750,   -0.3,   0.6,    0.7,  0.2/
!
!***********************************************************************
      Xt  = 0.0D+0
      Yt  = 0.0D+0
      dXt = 0.0D+0
      dYt = 0.0D+0
!
!     Sum zonal tide terms
!
      DO 10 I=1, N
!   Formation of multiples of arguments
      ARG = XS(1,I)*FA2K(1) + XS(2,I)*FA2K(2) + XS(3,I)*FA2K(3) &
     &    + XS(4,I)*FA2K(4) + XS(5,I)*FA2K(5) + XS(6,I)*(GMST2K(1)+PI)
      ARG = DMOD(ARG,TWOPI)
!     ARG = DMOD(ARG,1296000.0D0) * CONVDS
!   First derivative
      dARG = XS(1,I)*FAD2K(1) + XS(2,I)*FAD2K(2) + XS(3,I)*FAD2K(3) &
     &     + XS(4,I)*FAD2K(4) + XS(5,I)*FAD2K(5) + XS(6,I)*(GMST2K(2))
!
!     Evaluate zonal tidal terms (micro-arcsec)
       Xt  = XS(8,I)*DSIN(ARG)  + XS(9,I)*DCOS(ARG)  + Xt
       Yt  = XS(10,I)*DSIN(ARG) + XS(11,I)*DCOS(ARG) + Yt
       dXt = XS(8,I)*DCOS(ARG)*dARG  - XS(9,I)*DSIN(ARG)*dARG  + dXt
       dYt = XS(10,I)*DCOS(ARG)*dARG - XS(11,I)*DSIN(ARG)*dARG + dYt
   10 CONTINUE
!
!  Change from micro-arsecs to milli-arcsecs
      Xt  =  Xt*1.0D-3
      Yt  =  Yt*1.0D-3
      dXt = dXt*1.0D-3
      dYt = dYt*1.0D-3
!
!       print *,' XYTID/GMST2K: ', GMST2K
!       print *,' XYTID/Xt,dXt: ', Xt,dXt
!       print *,' XYTID/Yt,dYt: ', Yt,dYt
!
      RETURN
      END
!
!**********************************************************************
      SUBROUTINE ORTHO_EOP (TMJD, Xt, Yt, UT1t, dXt, dYt, dUT1t )
      IMPLICIT None
!
!   Subroutines to compute the diurnal and semidiurnal variations
!   in the earth orientation from the version of Richard Ray's ocean
!   tide model that was listed in IERS Technical Note 21, July 1996.
!   This code includes the variations from 71 diurnal and semidiurnal
!   terms instead of the 8 that are listed in the report.
!
!...Purpose: to compute the diurnal and semidiurnal variations
!            in EOP (x,y,UT1) from ocean tides
!
!...Coded by: Richard Eanes, UT/CSR, Feb 1997
!             2004: D. Gordon, Modified for Calc 10 usage.
!
!...Input: time = Modified Julian Date
!
!...Output: eop = (delta_x, delta_y, delta_UT1)
!                 microarcsec for x and y, microsec for UT1
!
      REAL*8 TMJD, Xt, Yt, UT1t, dXt, dYt, dUT1t
      REAL*8 TIME, eop(3), orthow(12,3), H(12)
      REAL*8 d_H(12), d_eop(3)
      INTEGER*4 K, J
!
!...Diurnal and semidiurnal orthoweights fit to the 8 constituents
!   listed in IERS Technical Note 21, July 1996 which are from the
!   paper "Diurnal and Semidiurnal Variations in the Earth's Rotation
!   Rate Induced by Ocean Tides" by Ray, R.D., Steinberg, D.J.,
!   Chao, B.F., and Cartwright, D.E., Science, 264, pp. 830-832.
!
      Data Orthow / &
     & -6.77832,-14.86323,  0.47884, -1.45303,  0.16406,  0.42030, &
     &  0.09398, 25.73054, -4.77974,  0.28080,  1.94539, -0.73089, &
     & 14.86283, -6.77846,  1.45234,  0.47888, -0.42056,  0.16469, &
     & 15.30276, -4.30615,  0.07564,  2.28321, -0.45717, -1.62010, &
     & -1.76335,  1.03364, -0.27553,  0.34569, -0.12343, -0.10146, &
     & -0.47119,  1.28997, -0.19336,  0.02724,  0.08955,  0.04726/
!
! Initialize:
        Xt    = 0.0D0
        Yt    = 0.0D0
        UT1t  = 0.0D0
        dXt   = 0.0D0
        dYt   = 0.0D0
        dUT1t = 0.0D0
!
!...compute the partials of the tidal variations to the orthoweights
       time = TMJD
      Call CNMTX (Time, H)
!     Call CNMTX (TIME, H, d_H)
!       print *, 'Ortho_eop/H: ', H
!
!...compute eop changes
      do k=1,3
         eop(k) = 0.
!        d_eop(k) = 0.
         do j=1,12
            eop(k) = eop(k) + h(j)*orthow(j,k)
!           d_eop(k) = d_eop(k) + d_h(j)*orthow(j,k)
         enddo
      enddo
!   Change X and Y to milli-arcsec and UT1 to milli-sec
         Xt   = eop(1)*1.D-3
         Yt   = eop(2)*1.D-3
         UT1t = eop(3)*1.D-3
!        dXt   = d_eop(1)*1.D-3
!        dYt   = d_eop(2)*1.D-3
!        dUT1t = d_eop(3)*1.D-3
!
!        print *, ' ORTHO_EOP/Time: ', Time
!        print *, ' ORTHO_EOP/Xt,Yt,UT1t: ', Xt,Yt,UT1t
!        print *, ' ORTHO_EOP/dXt,dYt,dUT1t: ', dXt,dYt,dUT1t
!
      return
      end
!
!**********************************************************************
      SUBROUTINE CNMTX (DMJD, H)
      IMPLICIT None
!
!...Purpose: To compute the time dependent part of the second degree
!            diurnal and semidiurnal tidal potential from the dominant
!            spectral lines in the Cartwright-Tayler-Edden harmonic
!            decomposition.
!
!...Coded by: Richard Eanes, UT/CSR, Feb 1997
!             2004: D. Gordon, Modified for Calc 10 usage.
!
!...Input: DMJD = Modified Julian Date
!
!...Output: H = vector of length 12 with partials of the tidal
!           variation with respect to the orthoweights.
!
      Real*8    DMJD, H(12)
      Real*8    hs(71), phase(71), freq(71), twopi, dt, dt60, d1960, &
     &          pinm, alpha, ap, am, bp, bm, sp(6,2)
      Real*8    anm(2:3,0:3,-1:1), bnm(2:3,0:3,-1:1)
      Real*8    p(0:2,2), q(0:2,2)
      INTEGER*4 NLINES, i, j, k, m, n, nj(71), mj(71), nmax
      Character*7 Numarg(71), ndum
!
!...The orthotide weight factors
      Data ((sp(i,m),i=1,6),m=1,2) / &
     & 0.0298,  0.1408, +0.0805,  0.6002, +0.3025,  0.1517, &
     & 0.0200,  0.0905, +0.0638,  0.3476, +0.1645,  0.0923/
!
      Data twopi /6.2831853071796D0/
      Data dt / 2.D0 /
      Data nmax /2/
      Data nlines /71/
!
!...Tidal potential model for 71 diurnal and semidiurnal lines
!
      Data d1960/37076.5/
      data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=1,15) &
     &/2, 1,  -1.94,  9.0899831,  5.18688050, '117.655', &
     & 2, 1,  -1.25,  8.8234208,  5.38346657, '125.745', &
     & 2, 1,  -6.64, 12.1189598,  5.38439079, '125.755', &
     & 2, 1,  -1.51,  1.4425700,  5.41398343, '127.545', &
     & 2, 1,  -8.02,  4.7381090,  5.41490765, '127.555', &
     & 2, 1,  -9.47,  4.4715466,  5.61149372, '135.645', &
     & 2, 1, -50.20,  7.7670857,  5.61241794, '135.655', &
     & 2, 1,  -1.80, -2.9093042,  5.64201057, '137.445', &
     & 2, 1,  -9.54,  0.3862349,  5.64293479, '137.455', &
     & 2, 1,   1.52, -3.1758666,  5.83859664, '145.535', &
     & 2, 1, -49.45,  0.1196725,  5.83952086, '145.545', &
     & 2, 1,-262.21,  3.4152116,  5.84044508, '145.555', &
     & 2, 1,   1.70, 12.8946194,  5.84433381, '145.755', &
     & 2, 1,   3.43,  5.5137686,  5.87485066, '147.555', &
     & 2, 1,   1.94,  6.4441883,  6.03795537, '153.655'/
      Data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=16,30) &
     &/2, 1,   1.37, -4.2322016,  6.06754801, '155.445', &
     & 2, 1,   7.41, -0.9366625,  6.06847223, '155.455', &
     & 2, 1,  20.62,  8.5427453,  6.07236095, '155.655', &
     & 2, 1,   4.14, 11.8382843,  6.07328517, '155.665', &
     & 2, 1,   3.94,  1.1618945,  6.10287781, '157.455', &
     & 2, 1,  -7.14,  5.9693878,  6.24878055, '162.556', &
     & 2, 1,   1.37, -1.2032249,  6.26505830, '163.545', &
     & 2, 1,-122.03,  2.0923141,  6.26598252, '163.555', &
     & 2, 1,   1.02, -1.7847596,  6.28318449, '164.554', &
     & 2, 1,   2.89,  8.0679449,  6.28318613, '164.556', &
     & 2, 1,  -7.30,  0.8953321,  6.29946388, '165.545', &
     & 2, 1, 368.78,  4.1908712,  6.30038810, '165.555', &
     & 2, 1,  50.01,  7.4864102,  6.30131232, '165.565', &
     & 2, 1,  -1.08, 10.7819493,  6.30223654, '165.575', &
     & 2, 1,   2.93,  0.3137975,  6.31759007, '166.554'/
      Data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=31,45) &
     &/2, 1,   5.25,  6.2894282,  6.33479368, '167.555', &
     & 2, 1,   3.95,  7.2198478,  6.49789839, '173.655', &
     & 2, 1,  20.62, -0.1610030,  6.52841524, '175.455', &
     & 2, 1,   4.09,  3.1345361,  6.52933946, '175.465', &
     & 2, 1,   3.42,  2.8679737,  6.72592553, '183.555', &
     & 2, 1,   1.69, -4.5128771,  6.75644239, '185.355', &
     & 2, 1,  11.29,  4.9665307,  6.76033111, '185.555', &
     & 2, 1,   7.23,  8.2620698,  6.76125533, '185.565', &
     & 2, 1,   1.51, 11.5576089,  6.76217955, '185.575', &
     & 2, 1,   2.16,  0.6146566,  6.98835826, '195.455', &
     & 2, 1,   1.38,  3.9101957,  6.98928248, '195.465', &
     & 2, 2,   1.80, 20.6617051, 11.45675174, '225.855', &
     & 2, 2,   4.67, 13.2808543, 11.48726860, '227.655', &
     & 2, 2,  16.01, 16.3098310, 11.68477889, '235.755', &
     & 2, 2,  19.32,  8.9289802, 11.71529575, '237.555'/
      Data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=46,60) &
     &/2, 2,   1.30,  5.0519065, 11.73249771, '238.554', &
     & 2, 2,  -1.02, 15.8350306, 11.89560406, '244.656', &
     & 2, 2,  -4.51,  8.6624178, 11.91188181, '245.645', &
     & 2, 2, 120.99, 11.9579569, 11.91280603, '245.655', &
     & 2, 2,   1.13,  8.0808832, 11.93000800, '246.654', &
     & 2, 2,  22.98,  4.5771061, 11.94332289, '247.455', &
     & 2, 2,   1.06,  0.7000324, 11.96052486, '248.454', &
     & 2, 2,  -1.90, 14.9869335, 12.11031632, '253.755', &
     & 2, 2,  -2.18, 11.4831564, 12.12363121, '254.556', &
     & 2, 2, -23.58,  4.3105437, 12.13990896, '255.545', &
     & 2, 2, 631.92,  7.6060827, 12.14083318, '255.555', &
     & 2, 2,   1.92,  3.7290090, 12.15803515, '256.554', &
     & 2, 2,  -4.66, 10.6350594, 12.33834347, '263.655', &
     & 2, 2, -17.86,  3.2542086, 12.36886033, '265.455', &
     & 2, 2,   4.47, 12.7336164, 12.37274905, '265.655'/
      Data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=61,71) &
     &/2, 2,   1.97, 16.0291555, 12.37367327, '265.665', &
     & 2, 2,  17.20, 10.1602590, 12.54916865, '272.556', &
     & 2, 2, 294.00,  6.2831853, 12.56637061, '273.555', &
     & 2, 2,  -2.46,  2.4061116, 12.58357258, '274.554', &
     & 2, 2,  -1.02,  5.0862033, 12.59985198, '275.545', &
     & 2, 2,  79.96,  8.3817423, 12.60077620, '275.555', &
     & 2, 2,  23.83, 11.6772814, 12.60170041, '275.565', &
     & 2, 2,   2.59, 14.9728205, 12.60262463, '275.575', &
     & 2, 2,   4.47,  4.0298682, 12.82880334, '285.455', &
     & 2, 2,   1.95,  7.3254073, 12.82972756, '285.465', &
     & 2, 2,   1.17,  9.1574019, 13.06071921, '295.555'/
!
!...Compute the time dependent potential matrix
!
      do n =  2,3
       do m =  0,3
        do k = -1,1
         anm(n,m,k)   = 0.
         bnm(n,m,k)   = 0.
        enddo
       enddo
      enddo
!
      do k=-1,1
         dt60 = (dmjd - k*dt) - d1960
!        anm(2,1:2,k) = 0.
!        bnm(2,1:2,k) = 0.
         do j=1,nlines
            n = nj(j)
            m = mj(j)
            pinm = mod(n+m,2)*twopi/4.D0
            alpha = phase(j) + freq(j)*dt60 - pinm
            alpha = mod(alpha,twopi)
            anm(n,m,k) = anm(n,m,k) + hs(j)*cos(alpha)
            bnm(n,m,k) = bnm(n,m,k) - hs(j)*sin(alpha)
         enddo
      enddo
!
!...Orthogonalize the response terms
!
      do m = 1,2
        ap = anm(2,m,1) + anm(2,m,-1)
        am = anm(2,m,1) - anm(2,m,-1)
        bp = bnm(2,m,1) + bnm(2,m,-1)
        bm = bnm(2,m,1) - bnm(2,m,-1)
        p(0,m) = sp(1,m)*anm(2,m,0)
        p(1,m) = sp(2,m)*anm(2,m,0) - sp(3,m)*ap
        p(2,m) = sp(4,m)*anm(2,m,0) - sp(5,m)*ap + sp(6,m)*bm
        q(0,m) = sp(1,m)*bnm(2,m,0)
        q(1,m) = sp(2,m)*bnm(2,m,0) - sp(3,m)*bp
        q(2,m) = sp(4,m)*bnm(2,m,0) - sp(5,m)*bp - sp(6,m)*am
        anm(2,m,-1) = p(0,m)
        anm(2,m, 0) = p(1,m)
        anm(2,m, 1) = p(2,m)
        bnm(2,m,-1) = q(0,m)
        bnm(2,m, 0) = q(1,m)
        bnm(2,m, 1) = q(2,m)
      enddo
!
!...Fill partials vector
      j = 1
      do n=2,nmax
         do m = 1,n
            do k = -1,1
               h(j)   = anm(n,m,k)
               h(j+1) = bnm(n,m,k)
               j = j + 2
             enddo
         enddo
      enddo
!
      return
      end
!
!**********************************************************************
      SUBROUTINE CNMTXzz (DMJD, H, d_H)
      IMPLICIT None
!
!...Purpose: To compute the time dependent part of the second degree
!            diurnal and semidiurnal tidal potential from the dominant
!            spectral lines in the Cartwright-Tayler-Edden harmonic
!            decomposition
!
!...Coded by: Richard Eanes, UT/CSR, Feb 1997
!             2004: D. Gordon, Modified for Calc 10 usage.
!
!...Input: dmjd = modified julian date
!
!...Output: h = vector of length 12 with partials of the tidal
!           variation with respect to the orthoweights
!
      Real*8    DMJD, H(12), d_H(12)
      INTEGER*4 NLINES, i, j, k, m, n, nj(71), mj(71), nmax
!     Parameter (nlines=71)
      Character*7 numarg(71)
      Real*8                                            dt60
      Real*8    hs(71), phase(71), freq(71), twopi, dt,       d1960, &
     &          pinm, alpha, ap, am, bp, bm, sp(6,2)
      Real*8    d_dt60, d_alpha, d_ap, d_am, d_bp, d_bm
      Real*8    anm(2:3,0:3,-1:1), bnm(2:3,0:3,-1:1)
      Real*8    p(0:2,2), q(0:2,2)
      Real*8    d_anm(2:3,0:3,-1:1), d_bnm(2:3,0:3,-1:1)
      Real*8    d_p(0:2,2), d_q(0:2,2)
!
!...The orthotide weight factors
      data ((sp(i,m),i=1,6),m=1,2) / &
     & 0.0298,  0.1408, +0.0805,  0.6002, +0.3025,  0.1517, &
     & 0.0200,  0.0905, +0.0638,  0.3476, +0.1645,  0.0923/
!
      data twopi /6.2831853071796/
      data dt / 2. /
      data nmax /2/
      data nlines /71/
!
!...Tidal potential model for 71 diurnal and semidiurnal lines
!
      data d1960/37076.5/
      data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=1,15) &
     &/2, 1,  -1.94, 9.0899831, 5.18688050, '117.655', &
     & 2, 1,  -1.25, 8.8234208, 5.38346657, '125.745', &
     & 2, 1,  -6.64,12.1189598, 5.38439079, '125.755', &
     & 2, 1,  -1.51, 1.4425700, 5.41398343, '127.545', &
     & 2, 1,  -8.02, 4.7381090, 5.41490765, '127.555', &
     & 2, 1,  -9.47, 4.4715466, 5.61149372, '135.645', &
     & 2, 1, -50.20, 7.7670857, 5.61241794, '135.655', &
     & 2, 1,  -1.80,-2.9093042, 5.64201057, '137.445', &
     & 2, 1,  -9.54, 0.3862349, 5.64293479, '137.455', &
     & 2, 1,   1.52,-3.1758666, 5.83859664, '145.535', &
     & 2, 1, -49.45, 0.1196725, 5.83952086, '145.545', &
     & 2, 1,-262.21, 3.4152116, 5.84044508, '145.555', &
     & 2, 1,   1.70,12.8946194, 5.84433381, '145.755', &
     & 2, 1,   3.43, 5.5137686, 5.87485066, '147.555', &
     & 2, 1,   1.94, 6.4441883, 6.03795537, '153.655'/
      data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=16,30) &
     &/2, 1,   1.37,-4.2322016, 6.06754801, '155.445', &
     & 2, 1,   7.41,-0.9366625, 6.06847223, '155.455', &
     & 2, 1,  20.62, 8.5427453, 6.07236095, '155.655', &
     & 2, 1,   4.14,11.8382843, 6.07328517, '155.665', &
     & 2, 1,   3.94, 1.1618945, 6.10287781, '157.455', &
     & 2, 1,  -7.14, 5.9693878, 6.24878055, '162.556', &
     & 2, 1,   1.37,-1.2032249, 6.26505830, '163.545', &
     & 2, 1,-122.03, 2.0923141, 6.26598252, '163.555', &
     & 2, 1,   1.02,-1.7847596, 6.28318449, '164.554', &
     & 2, 1,   2.89, 8.0679449, 6.28318613, '164.556', &
     & 2, 1,  -7.30, 0.8953321, 6.29946388, '165.545', &
     & 2, 1, 368.78, 4.1908712, 6.30038810, '165.555', &
     & 2, 1,  50.01, 7.4864102, 6.30131232, '165.565', &
     & 2, 1,  -1.08,10.7819493, 6.30223654, '165.575', &
     & 2, 1,   2.93, 0.3137975, 6.31759007, '166.554'/
      data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=31,45) &
     &/2, 1,   5.25, 6.2894282, 6.33479368, '167.555', &
     & 2, 1,   3.95, 7.2198478, 6.49789839, '173.655', &
     & 2, 1,  20.62,-0.1610030, 6.52841524, '175.455', &
     & 2, 1,   4.09, 3.1345361, 6.52933946, '175.465', &
     & 2, 1,   3.42, 2.8679737, 6.72592553, '183.555', &
     & 2, 1,   1.69,-4.5128771, 6.75644239, '185.355', &
     & 2, 1,  11.29, 4.9665307, 6.76033111, '185.555', &
     & 2, 1,   7.23, 8.2620698, 6.76125533, '185.565', &
     & 2, 1,   1.51,11.5576089, 6.76217955, '185.575', &
     & 2, 1,   2.16, 0.6146566, 6.98835826, '195.455', &
     & 2, 1,   1.38, 3.9101957, 6.98928248, '195.465', &
     & 2, 2,   1.80,20.6617051,11.45675174, '225.855', &
     & 2, 2,   4.67,13.2808543,11.48726860, '227.655', &
     & 2, 2,  16.01,16.3098310,11.68477889, '235.755', &
     & 2, 2,  19.32, 8.9289802,11.71529575, '237.555'/
      data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=46,60) &
     &/2, 2,   1.30, 5.0519065,11.73249771, '238.554', &
     & 2, 2,  -1.02,15.8350306,11.89560406, '244.656', &
     & 2, 2,  -4.51, 8.6624178,11.91188181, '245.645', &
     & 2, 2, 120.99,11.9579569,11.91280603, '245.655', &
     & 2, 2,   1.13, 8.0808832,11.93000800, '246.654', &
     & 2, 2,  22.98, 4.5771061,11.94332289, '247.455', &
     & 2, 2,   1.06, 0.7000324,11.96052486, '248.454', &
     & 2, 2,  -1.90,14.9869335,12.11031632, '253.755', &
     & 2, 2,  -2.18,11.4831564,12.12363121, '254.556', &
     & 2, 2, -23.58, 4.3105437,12.13990896, '255.545', &
     & 2, 2, 631.92, 7.6060827,12.14083318, '255.555', &
     & 2, 2,   1.92, 3.7290090,12.15803515, '256.554', &
     & 2, 2,  -4.66,10.6350594,12.33834347, '263.655', &
     & 2, 2, -17.86, 3.2542086,12.36886033, '265.455', &
     & 2, 2,   4.47,12.7336164,12.37274905, '265.655'/
      data (nj(j),mj(j),hs(j),phase(j),freq(j),numarg(j),j=61,71) &
     &/2, 2,   1.97,16.0291555,12.37367327, '265.665', &
     & 2, 2,  17.20,10.1602590,12.54916865, '272.556', &
     & 2, 2, 294.00, 6.2831853,12.56637061, '273.555', &
     & 2, 2,  -2.46, 2.4061116,12.58357258, '274.554', &
     & 2, 2,  -1.02, 5.0862033,12.59985198, '275.545', &
     & 2, 2,  79.96, 8.3817423,12.60077620, '275.555', &
     & 2, 2,  23.83,11.6772814,12.60170041, '275.565', &
     & 2, 2,   2.59,14.9728205,12.60262463, '275.575', &
     & 2, 2,   4.47, 4.0298682,12.82880334, '285.455', &
     & 2, 2,   1.95, 7.3254073,12.82972756, '285.465', &
     & 2, 2,   1.17, 9.1574019,13.06071921, '295.555'/
!
!...Compute the time dependent potential matrix
!
         d_dt60 = 1./86400.
!
      do n =  2,3
       do m =  0,3
        do k = -1,1
         anm(n,m,k)   = 0.
         bnm(n,m,k)   = 0.
         d_anm(n,m,k) = 0.
         d_bnm(n,m,k) = 0.
        enddo
       enddo
      enddo
!
      do k=-1,1
         dt60 = (dmjd - k*dt) - d1960
!          write(6,1011)  dt60,d_dt60
!1011      format(' dt60,d_dt60 ', F20.10,F20.15)
!        anm(2,1:2,k) = 0.
!        bnm(2,1:2,k) = 0.
!        d_anm(2,1:2,k) = 0.
!        d_bnm(2,1:2,k) = 0.
         do j=1,nlines
            n = nj(j)
            m = mj(j)
            pinm = mod(n+m,2)*twopi/4.
            alpha = phase(j) + freq(j)*dt60 - pinm
            alpha = mod(alpha,twopi)
            d_alpha = freq(j)*d_dt60
!          if(j.le.2)  print *,'j,alpha,d_alpha ', j,alpha,d_alpha
            anm(n,m,k) = anm(n,m,k) + hs(j)*cos(alpha)
            bnm(n,m,k) = bnm(n,m,k) - hs(j)*sin(alpha)
            d_anm(n,m,k) = d_anm(n,m,k) - hs(j)*sin(alpha)*d_alpha
            d_bnm(n,m,k) = d_bnm(n,m,k) - hs(j)*cos(alpha)*d_alpha
         enddo
      enddo
!          print *,'anm   ', anm
!          print *,'d_anm ', d_anm
!          print *,'bnm   ', bnm
!          print *,'d_bnm ',d_bnm
!
!...Orthogonalize the response terms
!
      do m = 1,2
        ap = anm(2,m,1) + anm(2,m,-1)
        am = anm(2,m,1) - anm(2,m,-1)
        bp = bnm(2,m,1) + bnm(2,m,-1)
        bm = bnm(2,m,1) - bnm(2,m,-1)
        p(0,m) = sp(1,m)*anm(2,m,0)
        p(1,m) = sp(2,m)*anm(2,m,0) - sp(3,m)*ap
        p(2,m) = sp(4,m)*anm(2,m,0) - sp(5,m)*ap + sp(6,m)*bm
        q(0,m) = sp(1,m)*bnm(2,m,0)
        q(1,m) = sp(2,m)*bnm(2,m,0) - sp(3,m)*bp
        q(2,m) = sp(4,m)*bnm(2,m,0) - sp(5,m)*bp - sp(6,m)*am
        anm(2,m,-1) = p(0,m)
        anm(2,m, 0) = p(1,m)
        anm(2,m, 1) = p(2,m)
        bnm(2,m,-1) = q(0,m)
        bnm(2,m, 0) = q(1,m)
        bnm(2,m, 1) = q(2,m)
!
        d_ap = d_anm(2,m,1) + d_anm(2,m,-1)
        d_am = d_anm(2,m,1) - d_anm(2,m,-1)
        d_bp = d_bnm(2,m,1) + d_bnm(2,m,-1)
        d_bm = d_bnm(2,m,1) - d_bnm(2,m,-1)
        d_p(0,m) = sp(1,m)*d_anm(2,m,0)
        d_p(1,m) = sp(2,m)*d_anm(2,m,0) - sp(3,m)*d_ap
        d_p(2,m) = sp(4,m)*d_anm(2,m,0) - sp(5,m)*d_ap + sp(6,m)*d_bm
        d_q(0,m) = sp(1,m)*d_bnm(2,m,0)
        d_q(1,m) = sp(2,m)*d_bnm(2,m,0) - sp(3,m)*d_bp
        d_q(2,m) = sp(4,m)*d_bnm(2,m,0) - sp(5,m)*d_bp - sp(6,m)*d_am
        d_anm(2,m,-1) = d_p(0,m)
        d_anm(2,m, 0) = d_p(1,m)
        d_anm(2,m, 1) = d_p(2,m)
        d_bnm(2,m,-1) = d_q(0,m)
        d_bnm(2,m, 0) = d_q(1,m)
        d_bnm(2,m, 1) = d_q(2,m)
!
!        print *,'m ', m
!        print *,'ap,d_ap ', ap,d_ap
!        print *,'am,d_am ', am,d_am
!        print *,'bp,d_bp ', bp,d_bp
!        print *,'bm,d_bm ', bm,d_bm
!        print *,'p(0,m),d_p(0,m)', p(0,m),d_p(0,m)
!        print *,'p(1,m),d_p(1,m)', p(1,m),d_p(1,m)
!        print *,'p(2,m),d_p(2,m)', p(2,m),d_p(2,m)
!        print *,'q(0,m),d_q(0,m)', q(0,m),d_q(0,m)
!        print *,'q(1,m),d_q(1,m)', q(1,m),d_q(1,m)
!        print *,'q(2,m),d_q(2,m)', q(2,m),d_q(2,m)
!        print *,'anm(2,m,-1),d_anm(2,m,-1) ', anm(2,m,-1),d_anm(2,m,-1)
!        print *,'anm(2,m,0),d_anm(2,m,0) ', anm(2,m,0), d_anm(2,m,0)
!        print *,'anm(2,m,1),d_anm(2,m,1) ', anm(2,m,1), d_anm(2,m,1)
!        print *,'bnm(2,m,-1),d_bnm(2,m,-1) ', bnm(2,m,-1),d_bnm(2,m,-1)
!        print *,'bnm(2,m,0),d_bnm(2,m,0) ', bnm(2,m,0), d_bnm(2,m,0)
!        print *,'bnm(2,m,1),d_bnm(2,m,1) ', bnm(2,m,1), d_bnm(2,m,1)
!
      enddo
!
!...Fill partials vector
      j = 1
      do n=2,nmax
         do m = 1,n
            do k = -1,1
               h(j)   = anm(n,m,k)
               h(j+1) = bnm(n,m,k)
               d_h(j)   = d_anm(n,m,k)
               d_h(j+1) = d_bnm(n,m,k)
               j = j + 2
             enddo
         enddo
      enddo
!        print *,'h   ', h
!        print *,'d_h ', d_h
!
      return
      end
