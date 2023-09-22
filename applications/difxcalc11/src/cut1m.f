      SUBROUTINE UT1G (AT, DUTCAT, UTC, XJD, CT, TT, FA2K, FAD2K,       &
     &           TC2000, TSKIP, DUT1AT, UT1,                            &
     &           Xti, Yti, UT1ti, dXti, dYti, dUT1ti)
      IMPLICIT None
!
!     UT1G is the UT1 module geometry section. It computes the instantaneous
!     offset between A1 and UT1 and the partial derivative of UT1 time with
!     respect to atomic time.
!
!     References - Ash,  M.E., 'Determination of Earth Satellite Orbits',
!     Lincoln Laboratory Technical Report 1972-5, 04/19/72, P. 42, 229-230.
!
!     Calling sequence -
!           Input variables:
!             1. AT     - The TAI fraction of the atomic time day. (days)
!             2. DUTCAT - The partial derivative of the UTC time with
!                         respect to the atomic time. (s/s)
!             3. UTC    - The UTC time fraction of the UTC day. (days)
!             4. XJD    - The Julian date at zero hours UTC of the date in
!                         question. (days)
!             5. CT     - The coordinate time fraction of the coordinate time
!                         day (days). [==> TDB]
!             6. TT     - The Terrestrial Time fraction of the day (days).
!             7. FA2K(14)-The fundamental arguments (used in UT1MU). (arcsec)
!             8. FAD2K(14)-The time derivatives of the fundamental arguments.
!                         (arcsec/century)
!             9. TC2000 - Time in Julian centuries since Jan. 1.5, 2000.
!                         (centuries)
!            10. TSKIP  - If 1, skip recomputations.
!           Output variables:
!             1. UT1    - The UT1 time of the day. (s)
!             2. DUT1AT - The partial derivative of the UT1 time with
!                         respect to atomic time. (s/s)
!             3. Xti    - Short period, ocean tidal contribution to
!                         X-polar motion. (milli-arc-seconds)
!             4. Yti    - Short period, ocean tidal contribution to
!                         Y-polar motion. (milli-arc-seconds)
!             5. UT1ti  - Short period, ocean tidal contribution to
!                         UT1 motion. (milli-seconds)
!
       Real*8 XJD, CT, TT, TC2000, ATMUT1,SHORTP,DIVUTC, &
     &        FA2K(14), FAD2K(14)
       Real*8 AT, DUTCAT, UTC, DUT1AT, UT1, tab_time
       Real*8 TMJD, Xti, Yti, UT1ti, dXti, dYti, dUT1ti, UT1li, dUT1li
       Integer*4 TSKIP
!
!    Common blocks used -
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!            Variables 'from':
!              1. SECDAY     -  The number of seconds in a day. (s/day)
!
      INCLUDE 'cmxut11.i'
!            Variables 'from':
!             1. UT1IF(4)   -  The UT1 information array. This array
!                              contains respectively: 1) The Julian
!                              date of the first tabular point,
!                              2) The increment in days of the tabular
!                              points, 3) The number of tabular points,
!                              4) The units of the UT1 tabular array per
!                              second. (days, days, unitless, s/table unit)
!             2. UT1PT(20)  -  The tabular values of the 'TAI - UT1' offset.
!             3. Y1(2)      -  An array used in the calculation of DIVUTC.
!             4. Y2(1)      -  An array used in the calculation of DIVUTC.
!             5. T          -  A time used in the calculation of DIVUTC.
!             6. S          -  Used in the calculation of DIVUTC.
!             7. UT1RS(20)  -  The table of 'TAI-UT1' (default) or 
!                              'TAI-UT1S' ['TAI-UT1' with fortnightly
!                              tidal terms removed using the model of
!                              Defrainge and Smits, 1999, as recommended
!                              by the IERS Conventions (2003)]. (seconds)
!             8. EOP_time_scale - EOP table time scale, allowed values:
!                              'TAI     ', 'TCG     ', 'TDB     ',
!                              'TDT     ', 'UTC     ', 'UNDEF   '.
!
      INCLUDE 'ccon.i'
!           Variables 'from':
!             1. KUT1C - UT1 module flow control flag, controls the
!                        temporary removal of periodic terms (UT1S)
!                        and the type of interpolation (spline or cubic)
!                        in the UT1 tables. Revised Sept/Oct 2005.
!                        = 0. Leave table as TAI-UT1.
!                           Do spline interpolation for a 1-day
!                           series. 5-day series no longer allowed!
!                        = 1. Module completely off, that is, UT1 set 
!                           equal to AT.
!                        = 2. Use TAI-UT1; use cubic interpolation for a
!                           1-day series. 5-day series no longer allowed!
!                        = 3. Use TAI-UT1; use linear interpolation for a
!                           1-day series. 5-day series no longer allowed!
!                        = 4. Convert table to TAI-UT1S. 
!                           Do spline interpolation for a 
!                           1-day series, then restore to true UT1 using
!                           the new UT1S model of Defrainge and Smits, 
!                           1999. 5-day series no longer allowed!
!             2. KUT1D - The module debug output flag.
!
       INCLUDE 'cuser11.i'
!          Variables from:
!            1. Calc_user - Calc user type. 'A' for Calc/SOLVE analysis.
!                           'C' for VLBI correlator.
!            2. Hifreq_tidal_EOP - Correlator option to add the high
!                           frequency (diurnal and sub-diurnal) EOP 
!                           correction for ocean tidal effects from 
!                           subroutine ORTHO_EOP. 
!                           'Y' => Add corrections to UT1, DUT1AT, ATMUT1.
!                           'N' => Don't add high frequency UT1 correction. 
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. ATMUT_1 -  The interpolated value of 'TAI - UT1'. (sec)
!
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
!           VARIABLES 'TO':
!            1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
!                           CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
!                           RATE OF CHANGE OF 'TAI MINUS UTC'. Used in the
!                           atomic time module. (DAYS, SEC, SEC/SEC)
!
!     Database access - PUT ('UT1 -TAI'/-ATMUT1) moved to PUT_G
!
!     Subroutine interface -
!             Caller subroutines: DRIVG
!             Called subroutines: UT1MU, PUT
!
!     Program variables -
!           1. ATMUT1 -  The interpolated value of 'TAI - UT1'. (sec)
!           2. DIVUTC -  The partial derivative of 'TAI MINUS UT1'
!                        with respect to UTC.  (sec/sec)
!           3. SHORTP -  UT1S-UT1 (s). Notice the sense.
!                        The short period correction to ATMUT1. (sec)
!
       SAVE  ATMUT1
!
!     Programmer - Dale Markham   02/14/77
!      77.07.14: Peter Denatale
!      78.02.21: Bruce Schupler
!      88.12.21: Gregg Cooke   Initial CALC 7.4 mods.
!      89.07.11: Jim Ryan      Documentation simplified.
!      89.12.12: Jim Ryan      UNIX-like database interface implimented.
!      90.01.02: Jim Ryan      UT1-TAI stored rather than reverse.
!      90.06.19: Jim Ryan      Logic to always interpolate with UT1R added.
!      93:09:07: Norbert Zacharias Use fundam. arg. from DRIVR to UT1G to UT1MU.
!      93:03:17: David Gordon  UT1S added as default, new control flag scheme.
!      94.04.06: David Gordon  Changed to 'Implicit None'.
!      94.09.27: David Gordon  Removed unused 'XLOVEK' Love number.
!      98.01.27: David Gordon  Removed UT1R references.
!      98.02.09: David Gordon  Code to skip repeat computations.
!      98.04.13: David Gordon  Common /UT1CM/ moved to 'cmxut.i' include file.
!      98.11.05: David Gordon  SAVE block added to save ATMUT1.
!      2001.01.02 David Gordon Modified to get table scale from EOP_time_scale.
!                              Tab_time used in place of CT in UT1MU argument.
!      2002.09   Jim Ryan      Integer*2/4 mods.
!      2003.12.24 D. Gordon    Changed short period computations. Effects
!                              UT1 at the ~2-3 nsec level. .......
!      2003-2004 David Gordon  Updated for IERS Conventions 2003.
!      2007.12.28 D. Gordon    Option to add short period ocean tide 
!                              corrections to UT1 and X/Y polar motion. 
!      2012.04.19 D. Gordon    Added TT, Terrestrial Time. 
!      Dec. 2012  D. Gordon    Added put2s.i and moved PUT's to PUT_G.
!
!     UT1G Program structure
!
!     Obtain ATMUT1 (The interpolated value of 'TAI - UT1').
!
      IF (TSKIP .EQ. 1) GO TO 101
!
!  Determine time scale of EOP table epochs. CT (=TDB) is the default.
!        tab_time = CT
! Change to TT for consistency with Tskip. Differences is ~1 milli-sec.
         tab_time = TT
       If (EOP_time_scale .eq. 'UTC     ') tab_time = UTC
       If (EOP_time_scale .eq. 'TAI     ') tab_time = UTC + &
     &       ATMUTC(2)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
       If (EOP_time_scale .eq. 'TDT     ') tab_time = UTC + &
     &      (ATMUTC(2) + 32.184D0)/SECDAY + ATMUTC(3)*(XJD-ATMUTC(1))
!
      If(KUT1C.ne.1) Then
        CALL UT1MU (XJD,TAB_TIME,FA2K,FAD2K,TC2000,ATMUT1, &
     &              SHORTP,DIVUTC)
!
!  Compute the UT1 time of the day.
        UT1 = AT * SECDAY - ATMUT1
!  Compute the partial derivative of UT1 time with respect to atomic time.
        DUT1AT = 1.D0  -  DIVUTC * DUTCAT
      Else
        UT1    = AT * SECDAY
        DUT1AT = 1.D0
        ATMUT1 = 0.D0
      Endif
!
!   Compute the short period corrections to UT1 and X,Y polar motion
!    due to ocean tides using the Ortho_EOP subroutine. (msec)
!    Polar motion contributions (Xti, Yti) to be saved for subroutine WOBG.
!   (Time scale not defined for ORTHO_EOP, using TT.
         TMJD = (XJD - 2400000.5D0) + TT
       CALL ORTHO_EOP (TMJD, Xti, Yti, UT1ti, dXti, dYti, dUT1ti)
!
!   Compute the short period corrections to UT1 due to libration. (msec)
!      CALL UT1LIB (GMST2K, FA2K, FAD2K, UT1li, dUT1li)
!!!!! Can't do this here, GMST2K not yet defined!!!!!!!!!!!!!!!!!!
!
!     WRITE(6,*) 'UT1G: Xti, dXti (mas) ', Xti, dXti
!     WRITE(6,*) 'UT1G: Yti, dYti (mas) ', Yti, dYti
!     WRITE(6,*) 'UT1G: UT1 (sec) ', UT1 
!     WRITE(6,*) 'UT1G: UT1ti, dUT1ti(ms) ', UT1ti, dUT1ti
!!!   WRITE(6,*) 'UT1G: UT1li,dUT1li  ', UT1li, dUT1li
!     WRITE(6,*) 'UT1G: UT1 (sec)     ', UT1       
!     WRITE(6,*) 'UT1G: UT1 + UT1ti   ', UT1 + UT1ti*1.D-3         
!
!  Add in short period ocean tidal effects for correlator usage.
!     If (Calc_user .eq. 'C' .and. Hifreq_tidal_EOP .eq. 'Y') Then
!       UT1 = UT1 + UT1ti*1.D-3 
!       DUT1AT = DUT1AT + dUT1ti*1.D-3
!       ATMUT1 = ATMUT1 - UT1ti*1.D-3
!     Endif
!
 101  CONTINUE
!
       ATMUT_1 = ATMUT1
!     PUT the UT1 value into the database for this observation.
!     NOTE: This is UT1-TAI, not the opposite sense.
!        Moved to PUT_G.
!
!     Check KUT1D for debug output.
      IF ( KUT1D .ne. 0 )  Then
      WRITE ( 6, 9)
    9 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE UT1G." )
      WRITE(6,8)' ATMUT1  ',ATMUT1
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DIVUTC  ',DIVUTC
    7 FORMAT(A,15I8/(9X,15I8))
      WRITE(6,8)' SECDAY  ',SECDAY
!
      WRITE ( 6, 9200 )  AT, DUTCAT, UTC, XJD, UT1, DUT1AT
 9200 FORMAT (1X, "AT     = ", D30.16, /, 1X, &
     &            "DUTCAT = ", D30.16, /, 1X, &
     &            "UTC    = ", D30.16, /, 1X, &
     &            "XJD    = ", D30.16, /, 1X, &
     &            "UT1    = ", D30.16, /, 1X, &
     &            "DUT1AT = ", D30.16, /, 1X)
!
      WRITE(6,8) ' TMJD        ', TMJD
      WRITE(6,8) ' Xti, dXti   ', Xti, dXti
      WRITE(6,8) ' Yti, dYti   ', Yti, dYti
      WRITE(6,8) ' UT1ti, dUT1ti ', UT1ti, dUT1ti
!
      Endif
!
!   7.    NORMAL PROGRAM CONCLUSION.
!
      RETURN
      END
!*********************************************************************
      SUBROUTINE UT1P (CFBASE, STAR, EARTH, RPN2K, RW2K, ERA2K, &
     &                 dERA2K, pERA2K, SITEV       )
      IMPLICIT None
!
!     UT1P is the UT1 module partial derivatives section. It computes the
!     partial derivatives of the delay and the delay rate with respect to the
!     instantaneous 'TAI - UT1'.
!
!     Calling sequence
!           Input variables:
!             1. CFBASE(3)  -  The crust fixed baseline vector.  (m)
!             2. STAR(3)    -  The J2000.0 source unit vector.
!             3. EARTH(3,3) -  The SSBC position, velocity, and acceleration of
!                              the Earth. (m, m/s, m/s**2)
!             4. RPN2K(3,3,2) - The precession-nutation portion of the
!                              complete crust
!                              fixed to J2000.0 rotation matrix and its CT time
!                              derivative (unitless,1/s).
!             5. RW2K(3,3,2) - The wobble portion of the complete crust fixed
!                              to J2000.0 rotation matrix and its first time
!                              derivative. (unitless, 1/sec)
!             6. ERA2K       - Earth rotation angle (Radians).
!             7. dERA2K      - Time derivative of Earth rotation angle.
!             8. pERA2K      - Partial derivative of the Earth rotation
!                              angle (ERA2K) w.r.t. UT1.
!             9. SITEV(3,2)  - THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF
!                              EACH OBSERVATION SITE. (M/SEC)
!
      Real*8 CFBASE(3), STAR(3), EARTH(3,3),             SITEV(3,2), &
     &       RPN2K(3,3,2), RW2K(3,3,2), pERA2K, ERA2K, dERA2K
!
!     Common blocks used -
!
      INCLUDE 'cphys11.i'
!            Variables 'from':
!              1. VLIGHT  - The defined speed of light. (m/s)
!              2. VLIGHT2 - THE VELOCITY OF LIGHT SQUARED.
!
      INCLUDE 'ccon.i'
!            VARIABLES 'FROM':
!             1. KUT1C  -  UT1 module flow control flag, controls the
!                          temporary removal of periodic terms (UT1S)
!                          and the type of interpolation (spline or cubic)
!                          in the UT1 tables. Revised Sept/Oct 2005.
!                          = 0. Leave table as TAI-UT1. Do spline 
!                             interpolation for a 1-day series. 
!                             No longer allow use of a 5-day series!
!                          = 1. Module completely off, that is, UT1 set equal
!                             to AT.
!                          = 2. Use TAI-UT1; use cubic interpolation for a
!                             1-day series. 5-day series no longer allowed!
!                          = 3. Use TAI-UT1; use linear interpolation for a
!                             1-day series. 5-day series no longer allowed!
!                          = 4. Convert table to TAI-UT1S. Do spline 
!                             interpolation for a 1-day series, then 
!                             restore to true UT1 using the UT1S model of
!                             Defrainge and Smits, 1999. 
!                             No longer allow use of a 5-day series.
!              2. KUT1D  -  The UT1 module debug output flag.
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. DUT1P(2,2) - The partial derivatives of the delay and rate
!                          with respect to the instantaneous value of
!                          'A1-UT1'. (s/s, s/s**2) The second index runs
!                              over the first and second derivatives.
!
!
!     Program specifications -
!
      Real*8  SBASE(3,2,2), TT,                                         &
     &       VG(3), VE(3), DOTP, pRS2K(3,3,2,2), SR2K(3,3,2,2)
      Integer*4 I, J, K
!
!    Database access => Moved to PUT_P ('UT1 PART'/DUT1P).
!
!     Subroutine interface -
!             Caller subroutines: DRIVP
!             Called subroutines: DDROT, DOTP, DROTT, MMUL5, VECRT
!
!     Program variables -
!          1. pRS2K(3,3,2,2) -  The partial derivatives of the diurnal spin
!                               matrix (DSM) and its time derivative w.r.t.
!                               the value of 'A1-UT1'. (1/s,1/s**2)
!                               The 4th index runs over the first and
!                               second partials of the DSM w.r.t. (TAI-UT1).
!                               The third index runs over the first and
!                               second partials of the first time derivative
!                               of the DSM w.r.t. (TAI-UT1).
!          2. SBASE(3,2,2)   -  The partial derivatives of the J2000.0 baseline
!                               position and velocity vectors with respect to
!                               the instantaneous value of 'A1-UT1'.
!                               (m/s,m/s**2). The final index counts the first
!                               and second partials.
!          3. SR2K(3,3,2,2)  -  The partial derivative of the complete crust
!                               fixed to J2000.0 rotation matrix and its
!                               derivative with respect to the instantaneous
!                               value of 'A1-UT1'. (1/s, 1/s**2)
!                               A 4th index value of 1 or 2 applies to the
!                               first derivative. A third index value of 1
!                               or 2 applies to the second derivative.
!          4. TT, VG, VE     -  Dummy variables used in the computation of the
!                               partials.
!
!     Programmer - Dale Markham   02/14/77
!      77.07.14  Peter Denatale
!      78.03.28  Bruce Schupler
!      80.07.11  Bruce Schupler
!      89.07.11  Jim Ryan      Documentation simplified.
!      89.10.05  Jim Ryan      CPHYS common made an include file.
!      89.12.12  Jim Ryan      UNIX-like database interface implemented.
!      91.05.30  Jim Ryan      Documentation simplified.
!      91.11.25  Jim Ryan      Second term in the Shapiro model added
!                              to the partials.
!      93.08.17  Norbert Zacharias  Output variable DUT1P added to arg.list
!      94.04.06  David Gordon  Changed to 'Implicit None'.
!      95.12.11  David Gordon  Changed RW(3,3) to RW(3,3,2).
!      98.01.27  David Gordon  Removed UT1R references.
!      2002.09   Jim Ryan      Integer*2/4 mods.
!      2003-2004 David Gordon  Updated for IERS Conventions 2003.
!      Dec. 2012 D. Gordon     PUT moved to PUT_P.
!
!     UT1P Program Structure
!
!   New code using Earth rotation angle, from IERS200
!
!     Construct the partial derivatives of the diurnal spin matrix
!     with respect to the instantaneous offset between A1 and UT1.
!
!    First partial w.r.t. TAI-UT1
!     CALL DROTT ( -ERA2K,-pERA2K, int2(3), pRS2K(1,1,1,1))
      CALL DROTT ( -ERA2K,+pERA2K, int2(3), pRS2K(1,1,1,1))
!    Second partial w.r.t. TAI-UT1
      CALL DDROT ( -ERA2K, +pERA2K*pERA2K, int2(3), pRS2K(1,1,1,2))
!
!     Construct the partial derivatives of the first CT time derivative of the
!     diurnal spin matrix with respect to the instantaneous offset between A1
!     and UT1. (NOTE: To a very good approximation the first partial is just the
!     second CT time derivative of the diurnal spin matrix.)
!
!    First partial of first CT time derivative w.r.t. TAI-UT1
!     CALL DDROT ( -ERA2K, +pERA2K*DERA2K , int2(3), pRS2K(1,1,2,1))
      CALL DDROT ( -ERA2K, -pERA2K*DERA2K , int2(3), pRS2K(1,1,2,1))
!    Second partial of first CT time derivative w.r.t. TAI-UT1
      CALL D3ROT( -ERA2K,-pERA2K*pERA2K*DERA2K,int2(3),pRS2K(1,1,2,2))
!
!     Compute the partial derivatives of the complete crust fixed to J2000.0
!     rotation matrix and of its first CT time derivative with respect to the
!     instantaneous offset between A1 and UT1. (NOTE: Of the three terms which
!     are used to compute the CT time derivative of the complete crust fixed to
!     J2000.0 rotation matrix, only the term which contains the CT time
!     derivative of the diurnal spin matrix is considered significant enough to
!     include in this partial derivatives section.)
!
!   First partial w.r.t. TAI-UT1
          CALL MMUL3 (RPN2K(1,1,1), pRS2K(1,1,1,1), RW2K(1,1,1), &
     &                SR2K(1,1,1,1))
!   Second partial w.r.t. TAI-UT1
          CALL MMUL3 (RPN2K(1,1,1), pRS2K(1,1,1,2), RW2K(1,1,1), &
     &                SR2K(1,1,1,2))
!
!   First partial of first CT time derivative w.r.t. TAI-UT1
          CALL MMUL3 (RPN2K(1,1,1), pRS2K(1,1,2,1), RW2K(1,1,1), &
     &                SR2K(1,1,2,1))
!   Second partial of first CT time derivative w.r.t. TAI-UT1
          CALL MMUL3 (RPN2K(1,1,1), pRS2K(1,1,2,2), RW2K(1,1,1), &
     &                SR2K(1,1,2,2))
!
!   Compute the partial derivatives of the J2000 baseline position and velocity
!   vectors with respect to the instantaneous offset between A1 and UT1.
!
          CALL VECRT ( SR2K(1,1,1,1), CFBASE, SBASE(1,1,1) )
          CALL VECRT ( SR2K(1,1,1,2), CFBASE, SBASE(1,2,1) )
!
          CALL VECRT ( SR2K(1,1,2,1), CFBASE, SBASE(1,1,2) )
          CALL VECRT ( SR2K(1,1,2,2), CFBASE, SBASE(1,2,2) )
!
!   Compute the partial derivatives of the delay and the delay rate with
!   respect to the instantaneous offset between A1 and UT1.
!   Changed to use the Consensus model definition
      Do I = 1,3
        VE(I) = EARTH(I,2)
        VG(I) = EARTH(I,2) + SITEV(I,2)
      Enddo
      TT = 1.d0 + DOTP(STAR,VG)/VLIGHT
!   First partial of delay w.r.t. TAI-UT1
      DUT1P(1,1) = DOTP(SBASE(1,1,1),STAR)/VLIGHT/TT                    &
     &           + Dotp(SBASE(1,1,1),VE)/VLIGHT2
!   Second partial of delay w.r.t. TAI-UT1
      DUT1P(2,1) = DOTP(SBASE(1,2,1),STAR)/VLIGHT/TT                    &
     &           + Dotp(SBASE(1,2,1),VE)/VLIGHT2
!   First partial of rate w.r.t. TAI-UT1
      DUT1P(1,2) = DOTP(SBASE(1,1,2),STAR)/VLIGHT/TT                    &
     &           + Dotp(SBASE(1,1,2),VE)/VLIGHT2
!   Second partial of rate w.r.t. TAI-UT1
      DUT1P(2,2) = DOTP(SBASE(1,2,2),STAR)/VLIGHT/TT                    &
     &           + Dotp(SBASE(1,2,2),VE)/VLIGHT2
!
!   PUT the UT1 partial derivatives.   Moved to PUT_P.
!
!   Check KUT1D for debug output.
      IF ( KUT1D .ne. 0 )  Then
      WRITE ( 6, 9)
    9 FORMAT (1X, "DEBUG OUTPUT FOR SUBROUTINE UT1P." )
!     WRITE(6,8)' DSDUT1  ',DSDUT1
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DUT1P   ',DUT1P
      WRITE(6,8)' SBASE   ',SBASE
      WRITE(6,8)' VLIGHT  ',VLIGHT
      WRITE(6,8)' TT      ',TT
      WRITE(6,8)' VG      ',VG
      WRITE(6,8)' VE      ',VE
      WRITE(6,8)' ERA2K   ',ERA2K
      WRITE(6,8)' DERA2K  ',DERA2K
      WRITE(6,8)' pERA2K  ',pERA2K
      Write(6,1031) pRS2K
 1031  Format(1x,'UT1P/pRS2K: ',(12(/,3E25.15)))
       Write(6,1032) SR2K
 1032  Format(1x,'UT1P/SR2K: ',(12(/,3E25.15)))
       Write(6,1033) SBASE
 1033  Format(1x,'UT1P/SBASE: ',(4(/,3E25.15)))
!
      WRITE ( 6, 9200 ) CFBASE, RPN2K, RW2K, STAR
 9200 FORMAT (1X, "CFBASE = ", 3 ( D30.16, 10X ), /, 1X, &
     &            "RPN2K  = ", 6 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "RW2K   = ", 6 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "STAR   = ", 3 ( D30.16, 10X ) )
      Endif
!
!     Normal conclusion.
      RETURN
      END
!*********************************************************************
      SUBROUTINE UT1C (UT1ti, dUT1ti, UT1li, dUT1li)
      IMPLICIT None
!
!     UT1C is the UT1 module contributions section. It computes the
!      delay and rate contributions due to the short period ocean
!      tidal contribution to UT1.
!
!     Calling sequence
!       Input variables:
!         1. UT1ti  - Short period, ocean tidal contribution to
!                     UT1 motion. (milli-seconds)
!         2. dUT1ti - Time derivative of the short period, ocean tidal 
!                     contribution to UT1 motion. (milli-sec/sec)
!         3. Ut1li  - Short term libration correction to UT1 (milli-sec)
!         4. dUt1li - Time derivative of short term libration correction to
!                     UT1. (milli-sec/sec)
!
!   Common blocks used:
      INCLUDE 'put2s.i'
!       Variables from:
!         1. DUT1P(2,2) - The partial derivatives of the delay and rate
!                         with respect to the instantaneous value of
!                         'A1-UT1'. (s/s, s/s**2) The second index runs
!                         over the first and second derivatives.
!       Variables to:
!         1. UT1tid(2) - UT1 delay and rate contributions from short
!                        period UT1 corrections (sec, sec/sec).
!         2. UT1lib(2) - UT1 delay and rate contributions from short
!                        period libration corrections (sec, sec/sec).
!
!
!**   Real*8    DUT1P(2,2), UT1tid(2), UT1lib(2), UT1ti, dUT1ti, UT1li, &
      Real*8                                      UT1ti, dUT1ti, UT1li, &
     &          dUT1li
      Integer*4 K
!
!     Database access => Moved to PUT_C ('UT1ORTHO'/UT1tid and 
!                        'UT1LIBRA'/UT1lib).
!
!   Programmer - David Gordon 2004.10.15
!          David Gordon Jan. 2013  Moved all PUT's to subroutine PUT_C.
!
!  UT1C Program Structure
!
!   Compute the contributions.
!    The negative sign is because DUT1P is the partial w.r.t. (TAI-UT1)!
!    The rate contributions are incomplete, but they are negligibly small.
      UT1tid(1) = -DUT1P(1,1) * UT1ti * 1.0D-3
      UT1tid(2) = -DUT1P(1,2) * UT1ti * 1.0D-3
      UT1lib(1) = -DUT1P(1,1) * UT1li * 1.0D-3
      UT1lib(2) = -DUT1P(1,2) * UT1li * 1.0D-3
!
!       print *,' UT1C: DUT1P(1,1)/(1,2) ', DUT1P(1,1), DUT1P(1,2)
!       print *,' UT1C: DUT1P(2,1)/(2,2) ', DUT1P(2,1), DUT1P(2,2)
!       print *,' UT1C: UT1tid =  ', UT1tid 
!       print *,' UT1C: UT1lib =  ', UT1lib 
!
!
!     Normal conclusion.
      RETURN
      END
!
!*********************************************************************
      SUBROUTINE UT1MU (XJD,tab_time,FA2K,FAD2K,tc2000,ATMUT1,          &
     &                  SHORTP,DIVUTC)
      IMPLICIT None
!
!     UT1MU is the UT1 module utility. It calculates the difference between AT
!     and UT1 to the best precision that our model allows. It also returns
!     separately the tidal correction to the difference.
!
!     References -
!           ASH, M.E., "Determination of Earth Satellite Orbits", Lincoln
!           Laboratory Technical Report 1972-5, 04/19/1972, P. 42, 229-230.
!
      Real*8 XJD, tc2000, ATMUT1, SHORTP, DIVUTC, tab_time, FA2K(14),   &
     &       FAD2K(14)
!
!        Input variables:
!         1. XJD   - The Julian date at zero hours UTC of the day in question.
!         2. Tab_time - Fraction of the day in the time scale of the input
!                    EOP table (CT, UTC, or ???)
!         3. FA2K(14)-The fundamental arguments (as of NUTFA from file
!                    cnutm.f). (arcseconds)
!         4. FAD2K(14)-The time derivatives of the fundamental arguments
!                    FA2K(14). (arcseconds/century)
!         5. TC2000- Time in Julian centuries since J2000.
!
!        Output variables:
!         1. ATMUT1 - The interpolated value of 'TAI - UT1' (s).
!         2. SHORTP - 'UT1S-UT1' (seconds). Note sense.
!         3. DIVUTC - The partial derivative of 'TAI MINUS UT1' with respect
!                     to UTC. (SEC/SEC)
!
!     Common block used  -
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!          Variables 'from':
!            1.  SECDAY  -  The number of seconds in a day. (s/day)
!            2.  TWOPI   -  PI times 2.0D0.
!
      INCLUDE 'cmxut11.i'
!      Variables 'from':
!        1. UT1IF -  The UT1 infomation array. (See above.)
!        2. UT1PT -  The tabular values of 'TAI - UT1'. (See above)
!        3. CENTJ -  The number of Julian days per Julian century. (days)
!        4. DJ1900 - The Julian date of January 1.5, 1900 (days).
!        5. DJ2000 - The Julian date of January 1.5, 2000 (days).
!        7. ISHRTFL  - The short period tidal terms flag, (unitless).
!                    = 0 --> UT1 table coming from input db is true UT1,
!                    (fortnightly tidal terms have not been removed).
!                    = -1 --> UT1 table coming from input db is UT1R, (Yoder
!                    fortnightly tidal terms HAVE been removed as in Bull. B).
!                     NOT SUPPORTED!!!!!
!                    = -2 --> UT1 table coming from input db is UT1S, (that is,
!                    the S tidal terms HAVE been removed)
!                     NOT SUPPORTED!!!!!
!        8. Usecubic - Set to true if cubic interpolation is to be used. False
!                    otherwise.
!        9. Uselinear - Set to true if linear interpolation is to be used.
!                    False otherwise.
!       10. Usespline - Set to true if spline interpolation is to be used.
!                    False otherwise.
!       11. UT1RS(20) - The table of 'TAI-UT1' or 'TAI-UT1S' ('TAI-UT1'
!                    with tidal terms removed using the model of
!                    Defrainge and Smits, 1999, as recommended
!                    by the IERS Conventions (2003)]. (seconds)
!
!      Variables 'to':
!        1. Y1(2) - A variable used in interpolation.
!        2. Y2(2) - Same.
!        3. T     - Same.
!        4. S     - Same.
!
      INCLUDE 'ccon.i'
!       Variables 'from':
!             1. KUT1C - UT1 module flow control flag, controls the
!                        temporary removal of periodic terms (UT1S)
!                        and the type of interpolation (spline or cubic)
!                        in the UT1 tables. Revised Sept. 2005.
!                        = 0. Leave table as TAI-UT1.
!                           Do spline interpolation for a 1-day
!                           series. No longer allow use of a 5-day series!
!                        = 1. Module completely off, that is, UT1 set equal
!                           to AT.
!                        = 2. Use TAI-UT1; use cubic interpolation for a
!                           1-day series. 5-day series no longer allowed!
!                        = 3. Use TAI-UT1; use linear interpolation for a
!                           1-day series. 5-day series no longer allowed!
!                        = 4. Convert table to TAI-UT1S.
!                           Do spline interpolation for a 
!                           1-day series, then restore to true UT1 using
!                           the new UT1S model of Defrainge and Smits, 
!                           1999. No longer allow use of a 5-day series.
!             2. KUT1D - The UT1 module debug control flag.
!
!     Program specifications
!
      Real*8 XINT(4), Dut, Dlod, Domega, Shortp_dot, F2
      Real*8 DUT_DOT, DUT1
      Real*8 x_spline, y_spline, ydot, ydot2, ydot3
      Integer*4 INT, ILAST, N, NN, NR, Ierr4
!
!     Subroutine Interface
!        Caller Subroutines: UT1G
!        Called Subroutines: DFLOTJ,IDINT,UT1S2K,splint4
!
!     Program variables:
!        1. XINT(4) - An array used in the interpolation procedure.
!
!     Programmer - Bruce Schupler 02/21/78
!      78.07.03  Bruce Schupler
!      85.04.13  David Gordon
!      89.07.11  Jim Ryan  Documentation simplified.
!      91.05.30  Jim Ryan  Documentation simplified furthur.
!      91.06.05  Jim Ryan  Interpolation modified to use linear interpolation.
!                          if the series is a 'one day' series.
!      91.06.19  Jim Ryan  Logic modified to always interpolate in the UT1R
!                          values. Important! Logic for computing the Ut1 time
!                          derivative moved here from UT1G (where it did not
!                          belong), and the effect of tidal terms on that
!                          derivative added.
!      91.11.05  Jim Ryan  SHORTP_DOT added to the computation of UT1 rate for
!                          linear interpolation. It was left out when linear
!                          was added.
!      93.03.17  D. Gordon Interpolation in UT1S added, new control flag scheme
!      93.09.07  Norbert Zacharias  Handle fundamental arguments, fa.
!      93 Dec.   D. Gordon Cubic spline interpolation added, modified control
!                          flag scheme for type of interpolation.
!      94.04.06  D. Gordon Changed to 'Implicit None'.
!      94.04.13  D. Gordon DFLOT changed to Fortran 77 DFLOTJ.
!      94.09.26  D. Gordon Added some debug printout, documentation
!                          corrections, cosmetic mods.
!      94.09.27  D. Gordon Removed unused 'XLOVEK' Love number.
!      98.01.21  D. Gordon Adding fad(5) to UT1RZT and UT1SZT.
!      98.01.27  D. Gordon Removing call to UT1RZT, and UT1R option.
!      98.04.13  D. Gordon Common /UT1CM/ moved to 'cmxut.i' include file.
!      2001.01.02 D.Gordon CT replaced with tab_time as input argument.
!      2002.09   Jim Ryan  Integer*4 conversion.
!      2003-2004 David Gordon Updated for IERS Conventions 2003.
!
!     UT1MU Program Structure
!
!     Convert tab_time and XJD to the units of the interval of the
!     table relative to
!     the first point of the table. INT is defined so that INT+2 is the number
!     of the point in the UT1 table immediately before (or at) the time of the
!     observation. (INT+2 is before and INT+3 is after the observation.)
!
      T = XJD  -  UT1IF(1)
!     T = ( T + CT ) / UT1IF(2)
      T = ( T + tab_time ) / UT1IF(2)
      INT = T
!     T = T - DFLOTJ ( INT )
      T = T - DFLOAT ( INT )
      INT = INT - 1
!
      IF (KUT1C.eq.4) Then
!       CALL UT1SZT (fa2k, fad2k, DUT, DLOD, DOMEGA)
        CALL UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
!        write(6,1021) DUT,DLOD,DOMEGA
!1021    format('UT1MU/UT1S2K: DUT,DLOD,DOMEGA = ', 3D25.16)
        SHORTP = -DUT
!    Convert rate of change from rad/sec to sec/sec
        SHORTP_DOT = -(DOMEGA/TWOPI)*SECDAY
!
      ELSE
!  Normal case: No UT1 smoothing
        SHORTP = 0.D0
        SHORTP_DOT = 0.D0
      ENDIF
!
!  Begin interpolation:
!
      IF (Usespline) then                      !Cubic spline interpolation
!   Compute time of obs. and divide by interval
!       X_spline = (XJD + Ct) / UT1IF(2)
        X_spline = (XJD + Tab_time) / UT1IF(2)
!
!   Do the spline interpolation
        Call Splint4 (XT,Ya,Y2s,Nspline,X_spline,Y_spline,Ydot,Ydot2, &
     &                Ydot3,Ierr4)
!
        ATMUT1 = Y_spline + SHORTP
        DIVUTC = Ydot / (UT1IF(2) * SECDAY) + SHORTP_DOT
!
      Endif                                     !Cubic spline interpolation
!
!***************************************
      IF (Usecubic) then                    !Four point cubic interpolation
!       Select the required four tabular points and verify that the
!       interpolation is not outside the range of the table.
!       (NOTE: If the interpolation is outside the range of the table,
!       a message is written and the program terminates.)
!
        ILAST = IDINT ( UT1IF(3) )
        DO  N = 1,4
          NN = INT  +  N
          IF ( ( NN .LT. 1 ) .OR. ( NN .GT. ILAST ) )  GO TO 1000
          XINT(N) = UT1RS(NN)
        ENDDO
!
!   Interpolate to determine the value of the 'TAI minus UT1' offset.
        DO  N = 1,2
          NR = N  +  1
          F2 = ( XINT (NR+1)  +  XINT (NR-1) ) / 6.D0
          Y1(N) = + ( 4.D0 / 3.D0 ) * XINT (NR)  -  F2
          Y2(N) = - ( 1.D0 / 3.D0 ) * XINT (NR)  +  F2
        ENDDO
!
        S = 1.D0  -  T
        ATMUT1 = &
     &     (  T * ( Y1(2)  +  T**2 * Y2(2) ) &
     &      + S * ( Y1(1)  +  S**2 * Y2(1) ) ) &
     &   + SHORTP
!
!   Compute the partial derivative of the 'IAT MINUS UT1' offset with respect
!    to UTC.
        DIVUTC = &
     &  ((  Y1(2)+3.d0*T**2*Y2(2) &
     &     -Y1(1)-3.d0*S**2*Y2(1) )/ &
     &    ( UT1IF(2) * SECDAY )     ) &
     &  + SHORTP_DOT
!
      ENDIF                                  !Four point cubic interpolation
!
!********************************************
      IF (Uselinear) Then           !Two point linear interpolation.
!
        If( INT+2 .lt. 1     .or. &
     &      INT+3 .gt. DINT(UT1IF(3)+.0001)) then
          Write(6,'( &
     &    "Error in UT1MU! Attemped to interpolate outside UT1 table.",/ &
     &    ,"INT =",I5," Table length =",i5)') INT, UT1IF(3)
          CALL TERMINATE_CALC( 'UT1MU ', int2(0), int2(0))
        Endif
!
        ATMUT1 = &
     &  ( (UT1RS(INT+3)-UT1RS(INT+2))*T+UT1RS(INT+2) ) + &
     &  SHORTP
!
        DIVUTC = (UT1RS(INT+3)-UT1RS(INT+2))/(UT1IF(2)*SECDAY) &
     &  + SHORTP_DOT
!
      ENDIF                          !Two point linear interpolation.
!
!******************************************************************************
!     Check for debug output.
      IF (KUT1D .ne. 0) THEN
        WRITE(6,'(" Debug output for subroutine UT1MU")')
        WRITE(6,88)' SHORTP, SHORTP_DOT ', SHORTP, SHORTP_DOT
 88     FORMAT(A,4D25.16/(9X,4D25.16))
 89     FORMAT(A,4I16/(9X,4I16))
        If(Usespline) Write(6,88)' XT      ', XT
        If(Usespline) Write(6,88)' Ya      ', Ya
        If(Usespline) Write(6,88)' Y2s     ', Y2s
        If(Usespline) Write(6,88)' X_spline,Y_spline', X_spline, &
     &                             Y_spline
        If(Usespline) Write(6,88)' Ydot,Ydot2,Ydot3 ', Ydot,Ydot2, &
     &                             Ydot3
        if(Usespline) Write(6,88)' Nspline, Ierr4 ',Nspline,Ierr4
        WRITE(6,89)' INT     ',INT
        WRITE(6,88)' T       ',T
        WRITE(6,88)' ATMUT1  ',ATMUT1
        WRITE(6,88)' DIVUTC  ',DIVUTC
        WRITE(6,88)' UT1IF   ',UT1IF
        WRITE(6,88)' UT1RS   ',UT1RS
!
!       WRITE(6,9200) XJD,CT,ATMUT1,SHORTP,DJ1900,CENTJ,DJ2000,TC2000
        WRITE(6,9200) XJD,tab_time,ATMUT1,SHORTP,DJ1900,CENTJ, &
     &                DJ2000,TC2000
 9200   FORMAT(1X,"XJD = ",D30.16,/,1X,"tab_time = ",D30.16,/, &
     &       1X,"ATMUT1 = ",D30.16,/,1X,"SHORTP = ",D30.16,/, &
     &       1X,"DJ1900 = ",D30.16,/,1X,"CENTJ = ",D30.16,/, &
     &       1X,"DJ2000 = ",D30.16,/,1X,"TC2000 = ",D30.16)
      ENDIF
!
!     Normal conclusion.
      RETURN
!
!     Abnormal termination.
 1000 CONTINUE
      WRITE(6,'(" CALC has terminated in subroutine UT1MU.", &
     &       /" The interpolation is outside the range of the UT1", &
     &       " table.  NN = ",I2," ILAST = ",I2," .")') NN, ILAST
      CALL TERMINATE_CALC ( 'UT1MU ', int2(0), int2(0))
      END
!************************************************************************
      BLOCK DATA UT1CMB
      IMPLICIT None
!
!     UT1BD is the UT1 module block data input and initialization section.
!
!     References 1) Ash, M.E., 'Determination of Earth Satellite Orbits",
!                   Lincoln Laboratory Technical Report 1972-5, 04/19/72, P. 42.
!                2  American Ephemeris and Nautical Almanac.
!                3) Melchior,"The Earth Tides".
!
      INCLUDE 'cmxut11.i'
!            Variables 'to':
!              1. CENTJ   -   The number of Julian days per Julian century.
!              2. DJ1900  -   The Julian date of Jan 1.5, 1900 (days).
!              3. DJ2000  -   The Julian date of Jan 1.5, 2000 (days).
!                             short period corrections to UT1 (See ref 4) (s).
!
!            Variables 'passed' from other module sections:
!              1. UT1IF(4)  -  The UT1 information array. (See above.)
!              2. UT1PT(20) -  The tabular values of 'IAT MINUS UT1'.
!
      DATA DJ1900, CENTJ / 2415020.0D0,36525.0D0/
      DATA DJ2000/2451545.0D0/
!
!    Constants used - DJ1900, CENTJ
!
!    Programmer - Dale Markham  02/14/77
!     77.07.14  Peter Denatale
!     78.02.21  Bruce Schuler
!     84.04.13  David Gordon
!     84.06.06  David Gordon
!     84.06.03  Savita Goel   CDS for A900.
!     94.04.06  David Gordon  Changed to 'Implicit None'.
!     94.09.27  David Gordon  Removed unused 'XLOVEK' Love number.
!     98.01.27  David Gordon  Removed Yoder coefficients.
!     98.04.13  David Gordon  Common /UT1CM/ moved to 'cmxut.i' include file.
!     03.02.25  Jim Ryan      Implicit none and type decs added by hand.
!
      END
!
!************************************************************************
      SUBROUTINE UT1SZT (FA2K, FAD2K, DUT, DLOD, DOMEGA)
      IMPLICIT None
!
!     Purpose: This subroutine evaluates the effects of zonal Earth tides on
!     the rotation of the Earth. The model used is from Yoder, Williams, and
!     Park (1981) and modified by the ocean effects as given in Dickman (1991)
!     as recommended by the IERS Standards, p. 117, 119-120 (1992).
!
!     Special Note: Under the UT1S definition, and as done by this routine,
!     zonal tides of _all_ periods are evaluated, including even those of
!     18.6 year period. Results will be substantially different (tenths of
!     seconds) from those evaluated with the "UT1R" algorithm, which only
!     includes the 41 terms for periods under 35 days. If you wish to determine
!     the effects from only those periods, please use the original Luzum
!     "zontids" routine, with N set to 41.  (B.A.)
!
!     Input Variables:
!          fa(5)  = Fundamental arguments from subroutine NUTFA (arcseconds)
!          fad(5) = Time derivatives of fundamental arguments (arcsec/century)
!     Output Variables:
!          DUT    - 'UT1 minus UT1S'. Effect on UT (Subtract from observation,
!                   add to IERS UT1). (sec)
!          DLOD   = Effect on length of day. (seconds).
!          DOMEGA = Effect on rotational speed (radians/second).
!
!     Written by:
!       Brian J. Luzum     92.08.11
!     Modifications:
!       Brent A. Archinal  92.08.27  Name changed from zontids to
!                                    ut1szt, N dropped from argument
!                                    list, comments improved.
!       "     "  "         92.10.27  Special note added above.
!       "     "  "         92.12.17  All real variables set to double
!                                    precision, at Jim Ray's suggestion
!                                    (email of 92.11.20).
!       David Gordon       93.03.17  Array X changed to XS, debug printout
!                                    added for calc 8.0.
!       Norbert Zacharias  93.09.16  Take fundam. arg. from subr. NUTFA
!       David Gordon       94.04.06  Changed to 'Implicit None'.
!       David Gordon       98.01.21  Extensive mods from John Gipson to use
!                                    the series expansion of DUT to calculate
!                                    DUT, DLOD, and DOMEGA instead of seperate
!                                    series expansions. fad(5) added to
!                                    subroutine call. DBLE's removed. CMATH
!                                    common block added. Variable SECCON
!                                    removed and replaced with 1/CONVDS. Sign
!                                    of X(7,62) corrected.
!     2002.09   Jim Ryan      Integer*4 conversion.
!
      REAL*8 T, DUT, DLOD, DOMEGA, F, D, OM, ARG
!     REAL*8 L, LP, fa(5), fad(5), ARG_DOT
      REAL*8 L, LP, FA2K(14), FAD2K(14), ARG_DOT
      Integer*4 N, I
      REAL*8 XS(11,62), X1(220), X2(220), X3(220), X4(22)
      EQUIVALENCE(XS(1,  1),X1(1))
      EQUIVALENCE(XS(1, 21),X2(1))
      EQUIVALENCE(XS(1, 41),X3(1))
      EQUIVALENCE(XS(1, 61),X4(1))
!
      INCLUDE 'ccon.i'
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!          Variables 'from':
!            1. SECDAY -  The number of seconds in a day. (s/day)
!            2. TWOPI  -  PI times 2.0D0
!            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!
!  N=Number of tidal terms to be used (62 for full set).
!  (This was an argument in the original zontids routine.)
!
      DATA  N/62/
!
!***********************************************************************
!     Table of multiples of arguments and coefficients
!      (DLOD and DOMEGA tables no longer used, 98JAN21 -DG-)
!
!                  Multiple of            DUT        DLOD      DOMEGA
!             l   l'  F   D OMEGA     sin   cos    cos  sin   cos   sin
      DATA X1/1., 0., 2., 2., 2.,    -0.02, 0.00,  0.3, 0.0, -0.2,  0.0, &
     &        2., 0., 2., 0., 1.,    -0.04, 0.00,  0.4, 0.0, -0.3,  0.0, &
     &        2., 0., 2., 0., 2.,    -0.10, 0.00,  0.9, 0.0, -0.8,  0.0, &
     &        0., 0., 2., 2., 1.,    -0.05, 0.00,  0.4, 0.0, -0.4,  0.0, &
     &        0., 0., 2., 2., 2.,    -0.12, 0.00,  1.1, 0.0, -0.9,  0.0, &
     &        1., 0., 2., 0., 0.,    -0.04, 0.00,  0.3, 0.0, -0.2,  0.0, &
     &        1., 0., 2., 0., 1.,    -0.40, 0.01,  2.7, 0.1, -2.3, -0.1, &
     &        1., 0., 2., 0., 2.,    -0.98, 0.03,  6.7, 0.2, -5.7, -0.2, &
     &        3., 0., 0., 0., 0.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &       -1., 0., 2., 2., 1.,    -0.08, 0.00,  0.5, 0.0, -0.5,  0.0, &
     &       -1., 0., 2., 2., 2.,    -0.20, 0.00,  1.3, 0.0, -1.1,  0.0, &
     &        1., 0., 0., 2., 0.,    -0.08, 0.00,  0.5, 0.0, -0.4,  0.0, &
     &        2., 0., 2.,-2., 2.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 1., 2., 0., 2.,     0.03, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 0., 2., 0., 0.,    -0.30, 0.00,  1.4, 0.0, -1.2,  0.0, &
     &        0., 0., 2., 0., 1.,    -3.20, 0.09, 14.7, 0.4,-12.4, -0.4, &
     &        0., 0., 2., 0., 2.,    -7.73, 0.21, 35.6, 1.0,-30.0, -0.8, &
     &        2., 0., 0., 0.,-1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        2., 0., 0., 0., 0.,    -0.34, 0.00,  1.5, 0.0, -1.3,  0.0, &
     &        2., 0., 0., 0., 1.,     0.02, 0.00, -0.1, 0.0,  0.1,  0.0/
      DATA X2/0.,-1., 2., 0., 2.,    -0.02, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &        0., 0., 0., 2.,-1.,     0.05, 0.00, -0.2, 0.0,  0.2,  0.0, &
     &        0., 0., 0., 2., 0.,    -0.72, 0.02,  3.1, 0.1, -2.6, -0.1, &
     &        0., 0., 0., 2., 1.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0, &
     &        0.,-1., 0., 2., 0.,    -0.05, 0.00,  0.2, 0.0, -0.2,  0.0, &
     &        1., 0., 2.,-2., 1.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        1., 0., 2.,-2., 2.,     0.10, 0.00, -0.3, 0.0,  0.2,  0.0, &
     &        1., 1., 0., 0., 0.,     0.04, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &       -1., 0., 2., 0., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &       -1., 0., 2., 0., 1.,     0.18, 0.00, -0.4, 0.0,  0.3,  0.0, &
     &       -1., 0., 2., 0., 2.,     0.44, 0.00, -1.0, 0.0,  0.9,  0.0, &
     &        1., 0., 0., 0.,-1.,     0.53, 0.00, -1.2, 0.0,  1.0,  0.0, &
     &        1., 0., 0., 0., 0.,    -8.33, 0.12, 19.0, 0.3,-16.0, -0.2, &
     &        1., 0., 0., 0., 1.,     0.54, 0.00, -1.2, 0.0,  1.0,  0.0, &
     &        0., 0., 0., 1., 0.,     0.05, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        1.,-1., 0., 0., 0.,    -0.06, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &       -1., 0., 0., 2.,-1.,     0.12, 0.00, -0.2, 0.0,  0.2,  0.0, &
     &       -1., 0., 0., 2., 0.,    -1.84, 0.02,  3.6, 0.0, -3.0,  0.0, &
     &       -1., 0., 0., 2., 1.,     0.13, 0.00, -0.3, 0.0,  0.2,  0.0, &
     &        1., 0.,-2., 2.,-1.,     0.02, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X3/-1.,-1.,0., 2., 0.,    -0.09, 0.00,  0.2, 0.0, -0.1,  0.0, &
     &        0., 2., 2.,-2., 2.,    -0.06, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        0., 1., 2.,-2., 1.,     0.03, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        0., 1., 2.,-2., 2.,    -1.88, 0.00,  1.0, 0.0, -0.8,  0.0, &
     &        0., 0., 2.,-2., 0.,     0.25, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 0., 2.,-2., 1.,     1.17, 0.00, -0.4, 0.0,  0.3,  0.0, &
     &        0., 0., 2.,-2., 2.,   -48.84, 0.11, 16.8, 0.0,-14.2,  0.0, &
     &        0., 2., 0., 0., 0.,    -0.19, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &        2., 0., 0.,-2.,-1.,     0.05, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        2., 0., 0.,-2., 0.,    -0.55, 0.00,  0.2, 0.0, -0.1,  0.0, &
     &        2., 0., 0.,-2., 1.,     0.04, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        0.,-1., 2.,-2., 1.,    -0.05, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        0., 1., 0., 0.,-1.,     0.09, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        0.,-1., 2.,-2., 2.,     0.83, 0.00, -0.1, 0.0,  0.1,  0.0, &
     &        0., 1., 0., 0., 0.,   -15.55, 0.02,  2.6, 0.0, -2.2,  0.0, &
     &        0., 1., 0., 0., 1.,    -0.14, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        1., 0., 0.,-1., 0.,     0.03, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &        2., 0.,-2., 0., 0.,    -0.14, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &       -2., 0., 2., 0., 1.,     0.42, 0.00,  0.0, 0.0,  0.0,  0.0, &
     &       -1., 1., 0., 1., 0.,     0.04, 0.00,  0.0, 0.0,  0.0,  0.0/
      DATA X4/0., 0., 0., 0., 2.,     7.90, 0.00,  0.1, 0.0, -0.1,  0.0, &
     &        0., 0., 0., 0., 1., -1637.68,-0.10,-10.4, 0.0,  8.8,  0.0/
!       X4(18) corrected, old value +0.10, new value -0.10
!    /        0., 0., 0., 0., 1., -1637.68, 0.10,-10.4, 0.0,  8.8,  0.0/
!
!***********************************************************************
!
      DUT    = 0.0D+0
      DLOD   = 0.0D+0
      DOMEGA = 0.0D+0
!
!     Sum zonal tide terms
!
      DO 10 I=1,N
!   Formation of multiples of arguments
      ARG = XS(1,I)*fa2k(1) + XS(2,I)*fa2k(2) + XS(3,I)*fa2k(3) &
     &    + XS(4,I)*fa2k(4) + XS(5,I)*fa2k(5)
      ARG = DMOD(ARG,1296000.0D0) * CONVDS
!   First derivative
      ARG_DOT = XS(1,I)*fad2k(1)  + XS(2,I)*fad2k(2) + XS(3,I)*fad2k(3) &
     &    + XS(4,I)*fad2k(4) + XS(5,I)*fad2k(5)
!     Evaluate zonal tidal terms
      DUT    = XS(6,I)*DSIN(ARG) + XS(7,I)*DCOS(ARG) + DUT
      DLOD   = ( XS(6,I)*DCOS(ARG) - XS(7,I)*DSIN(ARG) )*ARG_DOT + DLOD
   10 CONTINUE
      DUT    = DUT    * 1.0D-4
      DLOD   = -DLOD * 1.0D-4 / (3.6525D+4 / CONVDS)
      DOMEGA = -DLOD * TWOPI / SECDAY**2
!
!     Check for debug output.
      IF (KUT1D .ne. 1) GO TO 600
      WRITE(6,9)
    9 FORMAT(1X,"Debug output for subroutine UT1SZT")
!     WRITE(6,9200) L,LP,F,D,OM,ARG,DUT,DLOD,DOMEGA
 9200 FORMAT(1X,"L = ",D30.16,/,1X,"LP = ",D30.16,/, &
     &       1X,"F = ",D30.16,/,1X,"D = ",D30.16,/, &
     &       1X,"OM = ",D30.16,/,1X,"ARG = ",D30.16,/, &
     &       1X,"DUT = ",D30.16,/,1X,"DLOD = ",D30.16,/, &
     &       1X,"DOMEGA = ",D30.16)
  600 CONTINUE
!
      RETURN
      END
!
!*************************************************************************
      SUBROUTINE UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
      IMPLICIT None
!
!     Purpose: This subroutine evaluates the effects of zonal Earth tides on
!     the rotation of the Earth. The model used is from Defraigne and Smits,
!     1999, as recommended by the IERS Conventions (2003).
!
!     Input Variables:
!          fa(5)  = Fundamental arguments from subroutine NUTFA (arcseconds)
!          fad(5) = Time derivatives of fundamental arguments (arcsec/century)
!     Output Variables:
!          DUT    - 'UT1 minus UT1S'. Effect on UT (Subtract from observation,
!                   add to IERS UT1). (sec)
!          DLOD   = Effect on length of day. (seconds).
!          DOMEGA = Effect on rotational speed (radians/second).
!
!     Written by:
!       Brian J. Luzum     92.08.11
!     Modifications:
!       Brent A. Archinal  92.08.27  Name changed from zontids to
!                                    ut1szt, N dropped from argument
!                                    list, comments improved.
!       "     "  "         92.10.27  Special note added above.
!       "     "  "         92.12.17  All real variables set to double
!                                    precision, at Jim Ray's suggestion
!                                    (email of 92.11.20).
!       David Gordon       93.03.17  Array X changed to XS, debug printout
!                                    added for calc 8.0.
!       Norbert Zacharias  93.09.16  Take fundam. arg. from subr. NUTFA
!       David Gordon       94.04.06  Changed to 'Implicit None'.
!       David Gordon       98.01.21  Extensive mods from John Gipson to use
!                                    the series expansion of DUT to calculate
!                                    DUT, DLOD, and DOMEGA instead of seperate
!                                    series expansions. fad(5) added to
!                                    subroutine call. DBLE's removed. CMATH
!                                    common block added. Variable SECCON
!                                    removed and replaced with 1/CONVDS. Sign
!                                    of X(7,62) corrected.
!       David Gordon  2003.11.24     Name changed to UT1S2K, modified for
!                                    Defraigne and Smits (1999) model, as
!                                    recommended in IERS Conventions (2003).
!
      REAL*8 DUT, DLOD, DOMEGA, ARG
      REAL*8 FA2K(14), FAD2K(14), ARG_DOT
      Integer*4 N, I
      REAL*8 XS(11,62), X1(220), X2(220), X3(220), X4(22)
      EQUIVALENCE(XS(1,  1),X1(1))
      EQUIVALENCE(XS(1, 21),X2(1))
      EQUIVALENCE(XS(1, 41),X3(1))
      EQUIVALENCE(XS(1, 61),X4(1))
!
      INCLUDE 'ccon.i'
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!          Variables 'from':
!            1. SECDAY -  The number of seconds in a day. (s/day)
!            2. TWOPI  -  PI times 2.0D0
!            3. CONVDS -  THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!
!  N=Number of tidal terms to be used (62 for full set).
      DATA  N/62/
!
!***********************************************************************
!     Table of multiples of arguments and coefficients
!
!                  Multiple of            DUT        DLOD      DOMEGA
!             l   l'  F   D OMEGA     sin   cos    cos  sin   cos   sin
      DATA X1/1., 0., 2., 2., 2.,   -0.02, 0.00, 0.26, 0.00, -0.22, .00, &
     &        2., 0., 2., 0., 1.,   -0.04, 0.00, 0.38, 0.00, -0.32, .00, &
     &        2., 0., 2., 0., 2.,   -0.10, 0.00, 0.91, 0.00, -0.76, .00, &
     &        0., 0., 2., 2., 1.,   -0.05, 0.00, 0.45, 0.00, -0.38, .00, &
     &        0., 0., 2., 2., 2.,   -0.12, 0.00, 1.09, 0.01, -0.92,-.01, &
!
     &        1., 0., 2., 0., 0.,   -0.04, 0.00, 0.27, 0.00, -0.22, .0 , &
     &        1., 0., 2., 0., 1.,   -0.40, 0.00, 2.84, 0.02, -2.40,-.01, &
     &        1., 0., 2., 0., 2.,   -1.00, 0.01, 6.85, 0.04, -5.78,-.03, &
     &        3., 0., 0., 0., 0.,   -0.02, 0.00, 0.12, 0.00, -0.11, .00, &
     &       -1., 0., 2., 2., 1.,   -0.08, 0.00, 0.54, 0.00, -0.46, .00, &
!
     &       -1., 0., 2., 2., 2.,   -0.20, 0.00, 1.30, 0.01, -1.10,-.01, &
     &        1., 0., 0., 2., 0.,   -0.08, 0.00, 0.50, 0.00, -0.42, .00, &
     &        2., 0., 2.,-2., 2.,    0.02, 0.00,-0.11, 0.00,  0.09, .00, &
     &        0., 1., 2., 0., 2.,    0.03, 0.00,-0.12, 0.00,  0.10, .00, &
     &        0., 0., 2., 0., 0.,   -0.30, 0.00, 1.39, 0.01, -1.17,-.01, &
!
     &        0., 0., 2., 0., 1.,   -3.22, 0.02,14.86, 0.09,-12.54,-.08, &
     &        0., 0., 2., 0., 2.,   -7.79, 0.05,35.84, 0.22,-30.25,-.18, &
     &        2., 0., 0., 0.,-1.,    0.02, 0.00,-0.10, 0.00,  0.08, .00, &
     &        2., 0., 0., 0., 0.,   -0.34, 0.00, 1.55, 0.01, -1.31,-.01, &
     &        2., 0., 0., 0., 1.,    0.02, 0.00,-0.08, 0.00,  0.07, .00/
!
      DATA X2/0.,-1., 2., 0., 2.,   -0.02, 0.00, 0.11, 0.00, -0.09, .00, &
     &        0., 0., 0., 2.,-1.,    0.05, 0.00,-0.20, 0.00,  0.17, .00, &
     &        0., 0., 0., 2., 0.,   -0.74, 0.00, 3.14, 0.02, -2.65,-.02, &
     &        0., 0., 0., 2., 1.,   -0.05, 0.00, 0.22, 0.00, -0.19, .00, &
     &        0.,-1., 0., 2., 0.,   -0.05, 0.00, 0.21, 0.00, -0.17, .00, &
!
     &        1., 0., 2.,-2., 1.,    0.05, 0.00,-0.13, 0.00,  0.11, .00, &
     &        1., 0., 2.,-2., 2.,    0.10, 0.00,-0.26, 0.00,  0.22, .00, &
     &        1., 1., 0., 0., 0.,    0.04, 0.00,-0.10, 0.00,  0.08, .00, &
     &       -1., 0., 2., 0., 0.,    0.05, 0.00,-0.11, 0.00,  0.09, .00, &
     &       -1., 0., 2., 0., 1.,    0.18, 0.00,-0.41, 0.00,  0.35, .00, &
!
     &       -1., 0., 2., 0., 2.,    0.44, 0.00,-1.02,-0.01,  0.86, .01, &
     &        1., 0., 0., 0.,-1.,    0.54, 0.00,-1.23,-0.01,  1.04, .01, &
     &        1., 0., 0., 0., 0.,   -8.33, 0.06,18.99, 0.13,-16.03,-.11, &
     &        1., 0., 0., 0., 1.,    0.55, 0.00,-1.25,-0.01,  1.05, .01, &
     &        0., 0., 0., 1., 0.,    0.05, 0.00,-0.11, 0.00,  0.09, .00, &
!
     &        1.,-1., 0., 0., 0.,   -0.06, 0.00, 0.12, 0.00, -0.10, .00, &
     &       -1., 0., 0., 2.,-1.,    0.12, 0.00,-0.24, 0.00,  0.20, .00, &
     &       -1., 0., 0., 2., 0.,   -1.84, 0.01, 3.63, 0.02, -3.07,-.02, &
     &       -1., 0., 0., 2., 1.,    0.13, 0.00,-0.26, 0.00,  0.22, .00, &
     &        1., 0.,-2., 2.,-1.,    0.02, 0.00,-0.04, 0.00,  0.03, .03/
!
      DATA X3/-1.,-1.,0., 2., 0.,   -0.09, 0.00, 0.16, 0.00, -0.13, .00, &
     &        0., 2., 2.,-2., 2.,   -0.06, 0.00, 0.04, 0.00, -0.03, .00, &
     &        0., 1., 2.,-2., 1.,    0.03, 0.00,-0.02, 0.00,  0.01, .00, &
     &        0., 1., 2.,-2., 2.,   -1.91, 0.02, 0.98, 0.01, -0.83,-.01, &
     &        0., 0., 2.,-2., 0.,    0.26, 0.00,-0.09, 0.00,  0.08, .00, &
!
     &        0., 0., 2.,-2., 1.,    1.18,-0.01,-0.42, 0.00,  0.35, .00, &
     &        0., 0., 2.,-2., 2.,  -49.06, 0.43,16.88, 0.15,-14.25,-.13, &
     &        0., 2., 0., 0., 0.,   -0.20, 0.00, 0.07, 0.00, -0.06, .00, &
     &        2., 0., 0.,-2.,-1.,    0.05, 0.00,-0.02, 0.00,  0.01, .00, &
     &        2., 0., 0.,-2., 0.,   -0.56, 0.01, 0.17, 0.00, -0.14, .00, &
!
     &        2., 0., 0.,-2., 1.,    0.04, 0.00,-0.01, 0.00,  0.01, .00, &
     &        0.,-1., 2.,-2., 1.,   -0.05, 0.00, 0.01, 0.00, -0.01, .00, &
     &        0., 1., 0., 0.,-1.,    0.09, 0.00,-0.02, 0.00,  0.01, .00, &
     &        0.,-1., 2.,-2., 2.,    0.82,-0.01,-0.14, 0.00,  0.12, .00, &
     &        0., 1., 0., 0., 0.,  -15.65, 0.15, 2.69, 0.03, -2.27,-.02, &
!
     &        0., 1., 0., 0., 1.,   -0.14, 0.00, 0.02, 0.00, -0.02, .00, &
     &        1., 0., 0.,-1., 0.,    0.03, 0.00, 0.00, 0.00,  0.0 , .00, &
     &        2., 0.,-2., 0., 0.,   -0.14, 0.00,-0.01, 0.00,  0.01, .00, &
     &       -2., 0., 2., 0., 1.,    0.43,-0.01,-0.02, 0.00,  0.02, .00, &
     &       -1., 1., 0., 1., 0.,   -0.04, 0.00, 0.00, 0.00,  0.00, .00/
!
      DATA X4/0., 0., 0., 0., 2.,    8.20, 0.11, 0.15, 0.00, -0.13, .00, &
     &        0., 0., 0., 0., 1.,-1689.54,-25.04,-15.62,0.23,13.18,-.20/
!
!***********************************************************************
!
      DUT    = 0.0D+0
      DLOD   = 0.0D+0
      DOMEGA = 0.0D+0
!
!     Sum zonal tide terms
!
      DO 10 I=1,N
!   Formation of multiples of arguments
      ARG = XS(1,I)*FA2K(1) + XS(2,I)*FA2K(2) + XS(3,I)*FA2K(3) &
     &    + XS(4,I)*FA2K(4) + XS(5,I)*FA2K(5)
!!!   ARG = DMOD(ARG,1296000.0D0) * CONVDS
!  Already in radians!!!
      ARG = DMOD(ARG,1296000.0D0)
!   First derivative
      ARG_DOT = XS(1,I)*FAD2K(1)  + XS(2,I)*FAD2K(2) + XS(3,I)*FAD2K(3) &
     &    + XS(4,I)*FAD2K(4) + XS(5,I)*FAD2K(5)
! convert from radians/sec to arcsec/century
      ARG_DOT = ARG_DOT*(SECDAY*36525.D0)/CONVDS
!
!     Evaluate zonal tidal terms
      DUT    = XS(6,I)*DSIN(ARG) + XS(7,I)*DCOS(ARG) + DUT
      DLOD   = ( XS(6,I)*DCOS(ARG) - XS(7,I)*DSIN(ARG) )*ARG_DOT + DLOD
   10 CONTINUE
      DUT    = DUT    * 1.0D-4
      DLOD   = -DLOD * 1.0D-4 / (3.6525D+4 / CONVDS)
      DOMEGA = -DLOD * TWOPI / SECDAY**2
!
!     Check for debug output.
      IF (KUT1D .ne. 1) GO TO 600
!     WRITE(6,9)
    9 FORMAT(1X,"Debug output for subroutine UT1S2K")
!     WRITE(6,9200) L,LP,F,D,OM,ARG,DUT,DLOD,DOMEGA
 9200 FORMAT(1X,"L = ",D30.16,/,1X,"LP = ",D30.16,/, &
     &       1X,"F = ",D30.16,/,1X,"D = ",D30.16,/, &
     &       1X,"OM = ",D30.16,/,1X,"ARG = ",D30.16,/, &
     &       1X,"DUT = ",D30.16,/,1X,"DLOD = ",D30.16,/, &
     &       1X,"DOMEGA = ",D30.16)
  600 CONTINUE
!
      RETURN
      END
!*************************************************************************
!
      INTEGER*4 function get_leapsec(xjd,xleap)
      Implicit None
!
!     Routine to retrieve "leapsecond" (UTC step adjustment)
!     information for given date from default "leapsecond" file.
!     Passes back a five-element array of "leapsecond" information for
!     the given "xjd" (full) Julian date:
!
!       Position   Description
!       ---------------------------
!          1       Julian date just following step adjustment (always
!                  midnight).
!          2       The "leapsecond", i.e. TAI-UTC (seconds).
!          3       Epoch of rate of change (MJD days).
!          4       Rate of change of leapsecond (seconds/day).
!          5       Julian date just following next step adjustment.
!
!     Programmer:
!       Gregg Cooke      90.02.12  Creation.
!     Modifcations:
!       Gregg Cooke      90.03.30  Returns date following next
!                                  "leapsecond" as new fifth element
!                                  of xleap.  Also removed lpnam.
!       Kaybee Wilcox    92.04.23  Parameters obtained from param.i.
!       Brent Archinal   92.12.22  "getunit" properly used.  Many
!                                  other fixes made.  EOF Error return
!                                  fixed to only occur on first read
!                                  (wasn't working at all).  EOF on
!                                  later read okay, but properly handled.
!       D. Gordon        98.05.01  Added to Calc UT1 module.
!
!     Error  Returns:
!       >0   FORTRAN file I/O error.
!     -1201  Requested data is before first "leapsecond" in file.
!     -1301  No data found in "leapsecond" file (EOF on first read).
!
      INCLUDE 'param11.i'
!
!     Specifications:
!
!     LPDCB   --  Unit number of "leapsecond" file.
!     XLEAPR  --  Current "leapsecond" entry read from file.
!     BIGDT   --  Arbitrarily large Julian Date.
!
      Integer*4 get4unit
      Integer*4 k, i
      Integer*4 lpdcb, ierr
      Real*8 xjd, xleap(5), xleapr(4), bigdt
      Save lpdcb,bigdt
      Data lpdcb / 0 /, bigdt/1.D+99/
!
!     Program Structure:
!
!     Initialize the routine.  Get a unit number from get4unit. Then
!     open the "leapsecond" file.
!
      k = 0
      ierr = 0
      do i = 1,4
        xleapr(i) = 0.D0
        enddo
      xleap(5) = 0.D0
      get_leapsec = 0
      if(lpdcb.le.0) lpdcb = get4unit()
      open(UNIT=lpdcb,FILE=DFLEAP,STATUS='Old',IOSTAT=ierr)
      IF ( IERR .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'Error in opening leap second file '//DFLEAP
           CALL TERMINATE_CALC ( 'get_leapsec', int2(0), int2(0) )
      END IF
!
!     Read through the file until the target date is passed.  Then
!     return with date just following the "leapsecond and the
!     "leapsecond" information.  Remember to save each record -- the
!     "leapsecond" record information BEFORE the target
!     date is really the one we want.
!
      do while(xjd.ge.xleapr(1) .and. ierr.eq.0)
         k = k + 1
         do i = 1, 4
            xleap(i) = xleapr(i)
         enddo
         read(lpdcb,'(17X,F9.1,12X,F10.7,12X,F6.0,4X,F9.7,1X)', &
     &        IOSTAT=ierr,END=800) xleapr
         IF ( IERR > 0 ) THEN
              WRITE ( 6, '(A)' ) 'Error in reading leap second file '//DFLEAP
              CALL TERMINATE_CALC ( 'get_leapsec', int2(0), int2(0) )
          END IF
      enddo
!
!     EOF and Error returns come here...
!     Set error if the target date was before the bounds of the
!     "leapsecond" file.  Also, send back the date of the next
!     "leapsecond".
!
  800 continue
      if(ierr.eq.-1) then
        xleapr(1) = bigdt
        if(k.ne.1) ierr = 0
        endif
  911 continue
      close(lpdcb)
      get_leapsec = ierr
      if (xjd.lt.xleapr(1) .and. k.eq.1) get_leapsec = -1201
      if (ierr.eq.-1) get_leapsec = -1301
      if (get_leapsec.eq.0) xleap(5) = xleapr(1)
      return
      end
!
!***********************************************************************
      SUBROUTINE spline(xa,ya,n,yp1,ypn,y2,ierr4)
      Implicit none
!
!     Subroutine spline is the initialization section of a two subroutine
!     module used to do cubic spline interpolations. Spline is called each
!     time you change to a new set of tabular points. Subroutine splint4
!     is the second half and does the actual interpolation.
!
!     Given arrays xa(i), and ya(i) for i=1 to n containing a tabulated
!     function ya(i) = f(xa(i)) with xa(1) < xa(2) <..< xa(n) and values
!     yp1 and ypn whichc are the first derivatives of the function f( ) at
!     points 1 and n, respectively, this routine returns an array
!     y2(i) for i=1 to n which contains the second derivatives of the
!     function f( ) at the tabulated points xa(i). If yp1 and/or ypn are
!     greater than or equal to 1.d30, the routine is signaled to set the
!     corresponding boundary condition for a natural spline, with zero
!     second derivatives on the boundary.
!
!     The cubic spline has the following properties.
!     1) It is continuous and exactly fits at the tabular values.
!     2) The first and second derivatives are everywhere continuous,
!        even at the tabular points when points are added to and
!        deleted from the set of five points used to interpolate.
!     3) The third derivative is not continuous, but rather a series
!        of disconnected constant values.
!     4) The fourth derivative is zero.
!     5) It requires a minimum of five points and values of the first
!        derivative at the two endpoints.
!
!     In order to guarantee that this routine is only used in the modes in
!     which it has been tested, we require that the values of xa be exactly
!     one unit apart.
!
!  References:  'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
!
!  Calling sequence -
!
!     Input Variables:
!       1. xa(n) - Array of tabular (time) values corresponding to the ya
!                  array. The xa's must be evenly spaced.
!       2. ya(n) - Array of EOP values at the times corresponding to those
!                  in the xa array. i.e. ya(i) = f(xa(i)).
!       3. n     - Number of points in the xa and ya arrays.
!       4. yp1   - First derivative of the EOP function at the point i=1.
!       5. ypn   - First derivative of the EOP function at the point i=n.
!
!     Output variables:
!       1. y2(n) - Array containing the second derivatives of the
!                   interpolating function at the tabular points xa(i).
!       2. ierr4 - Error return code (0=good, 1=bad)
!
      INTEGER*4 n,NMAX,ierr4
      REAL*8 yp1,ypn,xa(20),ya(20),y2(20)
      PARAMETER (NMAX=25)
      INTEGER*4 i,k
      REAL*8 p,qn,sig,un,u(NMAX)
!
!  Program variables:
!       1. NMAX    - The largest anticipated value of n.
!       2. p       -
!       3. qn      -
!       4. sig     -
!       5. un      -
!       6. u(NMAX) -
!
!  Programmer:
!     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
!     93.12.07  David Gordon - Make variables same as in subroutine splint4,
!                              Calc-like documentation inserted.
!
!  SPLINE program structure:
!
!  Verify that the values of xa are one unit apart.
      do i = 2,n
        If(dabs(xa(i)-xa(i-1)-1.d0) .gt. 1.d-8) then
!        pause 'spline: independent variable NOT one unit apart!'
         write(6,'("spline: independent variable NOT one unit apart!")')
         write(6,'("spline: i, xa(i), xa(i-1), diff",i5,3d20.15)')     &
     &    i, xa(i), xa(i-1),(xa(i)-xa(i-1))
         ierr4 = 1
         Stop
         return
        else
          ierr4 = 0
        endif
      enddo
!
!  Set lower boundary condition to be "natural" or else to have a specified
!  first derivative.
      if (yp1 .gt. .99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(xa(2)-xa(1)))*((ya(2)-ya(1))/(xa(2)-xa(1))-yp1)
      endif
!
!  Decomposition loop of the tridiagonal algorithm. y2 and u used for temporary
!  storage of the decomposed factors.
      do 11 i=2,n-1
        sig=(xa(i)-xa(i-1))/(xa(i+1)-xa(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((ya(i+1)-ya(i))/(xa(i+1)-xa(i))- &
     &       (ya(i)-ya(i-1))/(xa(i)-xa(i-1)))/(xa(i+1)-xa(i-1))- &
     &       sig*u(i-1))/p
  11  continue
!
!  Set upper boundary condition to be "natural" or else to have a specified
!  first derivative.
      if (ypn .gt. .99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(xa(n)-xa(n-1)))*(ypn-(ya(n)-ya(n-1))/(xa(n)-xa(n-1)))
      endif
!
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
!
!  Backsubstitution loop of the tridiagonal algorithm.
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
  12  continue
!
      return
      END
!***********************************************************************
      SUBROUTINE splint4(xa,ya,y2,n,x,y,ydot,ydot2,ydot3,ierr4)
      Implicit none
!
!     Given the arrays xa(i) and ya(i) for i = 1 to n, which tabulate
!     a function with the xa(i)'s in time order, and given the array
!     y2(i), which is the previous output of subroutine spline, and
!     given a value of x, this routine returns the cubic spline
!     interpolated value of y and its first three derivatives.
!
!  References: 'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
!
!  Calling sequence -
!
!     Input Variables:
!       1. xa(n) - Array of tabular (time) values corresponding to the ya
!                  array. The xa's must be evenly spaced.
!       2. ya(n) - Array of EOP values at the times corresponding to those
!                  in the xa array. i.e. ya(i) = f(xa(i)).
!       3. y2(n) - Array containing the second derivatives of the
!                  interpolating function at the tabular points xa(i).
!       4. n     - Number of points in the xa and ya arrays.
!       5. x     - Input time for which the corresponding interpolated value
!                  of y (= f(x)) is to be determined.
!
!     Output variables:
!       1. y     - The interpolated value.
!       2. ydot  - The 1st derivative of the interpolated value.
!       3. ydot2 - The 2nd derivative of the interpolated value.
!       4. ydot3 - The 3rd derivative of the interpolated value.
!       5. ierr4 - Error return code (0=good, 1=bad)
!
      INTEGER*4 n, ierr4
      REAL*8 x,y,xa(20),y2(20),ya(20),ydot,ydot2,ydot3
      INTEGER*4 k,khi,klo
      REAL*8 a,b,h,adot,bdot
!
!  Program variables - klo, khi, k, h, a, b, adot, bdot
!
!  Programmer:
!     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
!                              First, second, and third derivatives added.
!     93.12.07  David Gordon - Make variables same as in subroutine spline,
!                              Calc-like documentation inserted.
!
!  SPLINT4 program structure:
!
!  Find right place in table by means of bisection. Optimized for sequential
!   calls being at random values of x.
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k) .gt. x)then
          khi=k
        else
          klo=k
        endif
      go to 1
      endif
!
!  Make sure the xa's aren't the same values.
      h=xa(khi)-xa(klo)
      if (h.eq.0.) then
        write(6,'(" Bad xa input in splint4 ")')
        ierr4 = 1
      else
        ierr4 = 0
      endif
!
!  Evaluate cubic spline polynomial
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2(klo)+(b**3-b)*y2(khi))* &
     &  (h**2)/6.
!
!  First derivative
      adot = -1.d0/h
      bdot =  1.d0/h
      ydot = adot*ya(klo)+bdot*ya(khi) &
     &       +( (3.d0*a**2-1.d0)*adot*y2(klo) &
     &       +  (3.d0*b**2-1.d0)*bdot*y2(khi))*(h**2/6.d0)
!
!  Second derivative.
      ydot2 = (a*(adot**2)*y2(klo)+b*(bdot**2)*y2(khi))*(h**2)
!
!  Third derivative.
      ydot3 = ((adot**3)*y2(klo)+(bdot**3)*y2(khi))*(h**2)
!
      return
      END
!
!**************************************************************************
      SUBROUTINE UT1_LIB (GMST2K, FA2K, FAD2K, UT1li, dUT1li)
      IMPLICIT None
!
!     Input Variables:
!          GMST2K(2) = Greenwich mean siderial time and its first time
!                      derivative (radians, radians/sec)
!          FA2K(14)  = Fundamental arguments from subroutine NUTFA (radians)
!          FAD2K(14) = Time derivatives of fundamental arguments (rad/sec)
!     Output Variables:
!          Ut1li  = Short term libration correction to UT1 (milli-sec)
!          dUt1li = Time derivative of short term libration correction to 
!                UT1 (milli-sec/sec)
!
      REAL*8 GMST2K(2), UT1li, dUT1li, LODli, dLODli 
      REAL*8 L, LP, fa(5), fad(5), ARG, dARG
      Integer*4 N, I
      REAL*8 XS(10,11), X1(110)
      EQUIVALENCE(XS(1,1),X1(1))
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
      DATA  N/11/
!
!***********************************************************************
!     Short period UT1 libration terms
!     From Table 5.1b, IERS Conventions (2010)
!
!                  Multiple of               UT1           LOD
!          l    l'   F    D   OMEGA Chi   sin    cos    sin    cos
      DATA X1/                                                          &
     &    -2.,  0., -2.,  0., -2.,  2.,  0.05, -0.03,  -0.3,  -0.6,     &
     &     0.,  0., -2., -2., -2.,  2.,  0.06, -0.03,  -0.4,  -0.7,     &
     &    -1.,  0., -2.,  0., -2.,  2.,  0.35, -0.20,  -2.4,  -4.1,     &
     &     1.,  0., -2., -2., -2.,  2.,  0.07, -0.04,  -0.5,  -0.8,     &
     &     0.,  0., -2.,  0., -1.,  2., -0.07,  0.04,   0.5,   0.8,     &
     &     0.,  0., -2.,  0., -2.,  2.,  1.75, -1.01, -12.2, -21.3,     &
     &     1.,  0., -2.,  0., -2.,  2., -0.05,  0.03,   0.3,   0.6,     &
     &     0., -1., -2.,  2., -2.,  2.,  0.04, -0.03,  -0.3,  -0.6,     &
     &     0.,  0., -2.,  2., -2.,  2.,  0.76, -0.44,  -5.5,  -9.6,     &
     &     0.,  0.,  0.,  0.,  0.,  2.,  0.21, -0.12,  -1.5,  -2.6,     &
     &     0.,  0.,  0.,  0., -1.,  2.,  0.06, -0.04,  -0.4,  -0.8/
!
!
      UT1li  = 0.0D+0
      dUT1li = 0.0D+0
      LODli  = 0.0D+0
      dLODli = 0.0D+0
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
!   Evaluate zonal tidal terms (micro-arcsec)
      UT1li  = XS(7,I)*DSIN(ARG) +  XS(8,I)*DCOS(ARG) + UT1li
      LODli  = XS(9,I)*DSIN(ARG) + XS(10,I)*DCOS(ARG) + LODli
      dUT1li = XS(7,I)*DCOS(ARG)*dARG - XS(8,I)*DSIN(ARG)*dARG + dUT1li
      dLODli = XS(9,I)*DCOS(ARG)*dARG - XS(10,I)*DSIN(ARG)*dARG + dLODli
   10 CONTINUE
!
!  Change from micro-secs to milli-secs
      UT1li  =  UT1li*1.0D-3
      LODli  =  LODli*1.0D-3
      dUT1li = dUT1li*1.0D-3
      dLODli = dLODli*1.0D-3
!
!       print *, ' UT1LIB: UT1li =  ', UT1li 
!       print *, ' UT1LIB: dUT1li = ', dUT1li 
!       print *, ' UT1LIB: LODli =  ', LODli 
!       print *, ' UT1LIB: dLODli = ', dLODli 
!
      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE splyne(xa,ya,n,yp1,ypn,y2,ierr4)
      Implicit none
!
!     Subroutine spline is the initialization section of a two subroutine
!     module used to do cubic spline interpolations. Spline is called each
!     time you change to a new set of tabular points. Subroutine splint4
!     is the second half and does the actual interpolation.
!
!     Given arrays xa(i), and ya(i) for i=1 to n containing a tabulated
!     function ya(i) = f(xa(i)) with xa(1) < xa(2) <..< xa(n) and values
!     yp1 and ypn whichc are the first derivatives of the function f( ) at
!     points 1 and n, respectively, this routine returns an array
!     y2(i) for i=1 to n which contains the second derivatives of the
!     function f( ) at the tabulated points xa(i). If yp1 and/or ypn are
!     greater than or equal to 1.d30, the routine is signaled to set the
!     corresponding boundary condition for a natural spline, with zero
!     second derivatives on the boundary.
!
!     The cubic spline has the following properties.
!     1) It is continuous and exactly fits at the tabular values.
!     2) The first and second derivatives are everywhere continuous,
!        even at the tabular points when points are added to and
!        deleted from the set of five points used to interpolate.
!     3) The third derivative is not continuous, but rather a series
!        of disconnected constant values.
!     4) The fourth derivative is zero.
!     5) It requires a minimum of five points and values of the first
!        derivative at the two endpoints.
!
!     In order to guarantee that this routine is only used in the modes in
!     which it has been tested, we require that the values of xa be exactly
!     one unit apart.
!
!  References:  'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
!
!  Calling sequence -
!
!     Input Variables:
!       1. xa(n) - Array of tabular (time) values corresponding to the ya
!                  array. The xa's must be evenly spaced.
!       2. ya(n) - Array of EOP values at the times corresponding to those
!                  in the xa array. i.e. ya(i) = f(xa(i)).
!       3. n     - Number of points in the xa and ya arrays.
!       4. yp1   - First derivative of the EOP function at the point i=1.
!       5. ypn   - First derivative of the EOP function at the point i=n.
!
!     Output variables:
!       1. y2(n) - Array containing the second derivatives of the
!                   interpolating function at the tabular points xa(i).
!       2. ierr4 - Error return code (0=good, 1=bad)
!
!     Common blocks used -
      INCLUDE 'd_input.i'
!            Variables 'from':
!             1) NF_row - Maximum number of rows  
!
!
      INTEGER*4 n,NMAX,ierr4
      REAL*8 yp1,ypn,xa(NF_row),ya(NF_row),y2(NF_row)
      PARAMETER (NMAX=1000)
      INTEGER*4 i,k
      REAL*8 p,qn,sig,un,u(NMAX)
!
!  Program variables:
!       1. NMAX    - The largest anticipated value of n.
!       2. p       -
!       3. qn      -
!       4. sig     -
!       5. un      -
!       6. u(NMAX) -
!
!  Programmer:
!     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
!     93.12.07  David Gordon - Make variables same as in subroutine splint4,
!                              Calc-like documentation inserted.
!     Jan-Apri, 2012 D. Gordon - Modified for near-field objects.
!
!  SPLINE program structure:
!
!
!  Verify that the values of xa are one unit apart.
      do i = 2,n
        If(abs(xa(i)-xa(i-1)-1.d0) .gt. 1.d-8) then
!        pause 'splyne: independent variable NOT one unit apart!'
         write(6,'("splyne: independent variable NOT one unit apart!")')
         write(6,'("splyne: i, xa(i), xa(i-1), diff",i5,3d20.15)') &
     &    i, xa(i), xa(i-1),(xa(i)-xa(i-1))
         ierr4 = 1
         Stop
         return
        else
          ierr4 = 0
        endif
      enddo
!
!  Set lower boundary condition to be "natural" or else to have a specified
!  first derivative.
      if (yp1 .gt. .99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(xa(2)-xa(1)))*((ya(2)-ya(1))/(xa(2)-xa(1))-yp1)
      endif
!
!  Decomposition loop of the tridiagonal algorithm. y2 and u used for temporary
!  storage of the decomposed factors.
      do 11 i=2,n-1
        sig=(xa(i)-xa(i-1))/(xa(i+1)-xa(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((ya(i+1)-ya(i))/(xa(i+1)-xa(i))- &
     &       (ya(i)-ya(i-1))/(xa(i)-xa(i-1)))/(xa(i+1)-xa(i-1))- &
     &       sig*u(i-1))/p
  11  continue
!
!  Set upper boundary condition to be "natural" or else to have a specified
!  first derivative.
      if (ypn .gt. .99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(xa(n)-xa(n-1)))*(ypn-(ya(n)-ya(n-1))/(xa(n)-xa(n-1)))
      endif
!
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
!
!  Backsubstitution loop of the tridiagonal algorithm.
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
  12  continue
!
      return
      END
!***********************************************************************
      SUBROUTINE splynt4(xa,ya,y2,n,x,y,ydot,ydot2,ydot3,ierr4)
      Implicit none
!
!     Given the arrays xa(i) and ya(i) for i = 1 to n, which tabulate
!     a function with the xa(i)'s in time order, and given the array
!     y2(i), which is the previous output of subroutine spline, and
!     given a value of x, this routine returns the cubic spline
!     interpolated value of y and its first three derivatives.
!
!  References: 'Numerical Recipes in FORTRAN, 2nd Edition' page 109-110.
!
!  Calling sequence -
!
!     Input Variables:
!       1. xa(n) - Array of tabular (time) values corresponding to the ya
!                  array. The xa's must be evenly spaced.
!       2. ya(n) - Array of EOP values at the times corresponding to those
!                  in the xa array. i.e. ya(i) = f(xa(i)).
!       3. y2(n) - Array containing the second derivatives of the
!                  interpolating function at the tabular points xa(i).
!       4. n     - Number of points in the xa and ya arrays.
!       5. x     - Input time for which the corresponding interpolated value
!                  of y (= f(x)) is to be determined.
!
!     Output variables:
!       1. y     - The interpolated value.
!       2. ydot  - The 1st derivative of the interpolated value.
!       3. ydot2 - The 2nd derivative of the interpolated value.
!       4. ydot3 - The 3rd derivative of the interpolated value.
!       5. ierr4 - Error return code (0=good, 1=bad)
!
!     Common blocks used -
      INCLUDE 'd_input.i'
!            Variables 'from':
!             1) NF_row - Maximum number of rows  
!
!
      INTEGER*4 n, ierr4
      REAL*8 x,y,xa(NF_row),y2(NF_row),ya(NF_row),ydot,ydot2,ydot3
      INTEGER*4 k,khi,klo
      REAL*8 a,b,h,adot,bdot
!
!  Program variables - klo, khi, k, h, a, b, adot, bdot
!
!  Programmer:
!     93.11.22  Jim Ryan     - Initial coding and modification for Calc.
!                              First, second, and third derivatives added.
!     93.12.07  David Gordon - Make variables same as in subroutine spline,
!                              Calc-like documentation inserted.
!     Jan-Apri, 2012 D. Gordon - Modified for near-field objects.
!
!  SPLINT4 program structure:
!
!  Find right place in table by means of bisection. Optimized for sequential
!   calls being at random values of x.
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k) .gt. x)then
          khi=k
        else
          klo=k
        endif
      go to 1
      endif
!
!  Make sure the xa's aren't the same values.
      h=xa(khi)-xa(klo)
      if (h.eq.0.) then
!       write(6,'(" Bad xa input in splynt4 ")')
        write(6,*) 'Splynt: n,x,xa(1),xa(n): ', n,x,xa(1),xa(n)
        ierr4 = 1
      else
        ierr4 = 0
      endif
!
!  Evaluate cubic spline polynomial
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2(klo)+(b**3-b)*y2(khi))* &
     &  (h**2)/6.
!
!  First derivative
      adot = -1.d0/h
      bdot =  1.d0/h
      ydot = adot*ya(klo)+bdot*ya(khi) &
     &       +( (3.d0*a**2-1.d0)*adot*y2(klo) &
     &       +  (3.d0*b**2-1.d0)*bdot*y2(khi))*(h**2/6.d0)
!
!  Second derivative.
      ydot2 = (a*(adot**2)*y2(klo)+b*(bdot**2)*y2(khi))*(h**2)
!
!  Third derivative.
      ydot3 = ((adot**3)*y2(klo)+(bdot**3)*y2(khi))*(h**2)
!
      return
      END
!
!**************************************************************************
!
      Integer*4 function get4unit()
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*4 iutot
!-----END of imp added lines.
!
!
! Purpose       Returns a unique unit number for each new call.
!
! Note          It is recommended that all code should call get4unit
!               in order to obtain unit numbers.  If this is not
!               desirable, the warning message below should be
!               disabled.
!
! Modified
!   B. Archinal         92.06.19  Rewritten.  Check made if unit
!   A. Myers                      already open.  Possible range
!                                 extended to include 1-4, 8-49 as unit
!                                 numbers.  Check made to see if too
!                                 many units in use.
!   D. Gordon         2012.04.26  Changed to Integer*4, renamed get4unit.
!
      logical*4 lopen
!
!  Ipoint contains the last unit assigned.  If7 is standard error.
!  Iutot is the total number of units used (including std in, out, err).
!  Iumax is maximum number of units allowed open on this system (this
!    can be changed as a system installation parameter under HP-UX
!    8.0 as "maxfiles" in the /etc/master file).
!
      Integer*4 ipoint,if7,iumax
      Save ipoint
!
!  Somewhat arbitrarily we start with 41.
!
      Data ipoint/40/,if7/6/,iutot/3/,iumax/60/
!
  100 ipoint = ipoint+1
      iutot = iutot + 1
!
!  If 100 or 5, 6, or 7, increment appropriately.
!
      if(ipoint.eq.100) ipoint = 1
      if(ipoint.gt.4.and.ipoint.lt.8) ipoint = 8
!
!  Terminate if out of unit numbers.
!
      if(iutot.gt.iumax) then
        write(if7,"(' *** ERROR *** Get4unit: All available unit'      &
     &  ,' numbers have been opened.'/ &
     &              '               Execution terminating.'/)")
        stop
      endif
!
!  Issue warning if unit already open, then increment again.
!
      inquire(unit=ipoint,opened=lopen)
      if(lopen) then
!        write(if7,"(' *** WARNING *** get4unit has determined that unit' &
!     &  ,' number',I3/ '                 has already been opened' &
!     &  ,' elsewhere.'/'                 Proceeding with next unit' &
!     &  ,' number.'/)") ipoint
        go to 100
       endif
!
!
      get4unit = ipoint
      return
      end
