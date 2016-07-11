!  Include file cmxut.i
      Integer*4 Nspline
      Logical*4 Usecubic, Uselinear, Usespline, Leap_fix,               &
     &          apply_tidal_correction, table_found
      Integer*2 ISHRTFL, IEPOCH, ASKKER, Intrvl(5,2), ndays
      Character*8 EOP_time_scale, UT1type
      Real*8      UT1IF(4), UT1PT(20), CENTJ, DJ1900, Y1(2),            &
     &            Y2(2), T, S,            DJ2000, Xintv(2),             &
     &            UT1RS(20), XT(20), YA(20), Y2S(20)
!
      COMMON / UT1CM / UT1IF, UT1PT, CENTJ, DJ1900, Y1, Y2, T, S,       &
     &                 DJ2000, Xintv, UT1RS, XT, YA, Y2S,               &
     &                 Nspline, Usecubic, Uselinear, Usespline,         &
     &                 apply_tidal_correction, table_found, Leap_fix,   &
     &                 Intrvl, ISHRTFL, IEPOCH, ASKKER, ndays,          &
     &                 EOP_time_scale, UT1type
!
!            Variables:
!              1. UT1IF(4)  - The final UT1 information array. This array
!                             contains respectively: 1) The Julian date of the
!                             first tabular point, 2) The increment in days of
!                             the tabular points, 3) The number of tabular
!                             points, 4) The units of the UT1 tabular array per
!                             second. (days, days, unitless, sec/table unit)
!              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
!                             (table units)
!              3. UT1RS(20) - The table of 'TAI-UT1S' or 'TAI-UT1R' values,
!                             as controlled by KUT1C. (seconds)
!              4. ISHRTFL   - The short period tidal terms flag, (unitless).
!                             = 1 --> UT1 table coming from input database is
!                             true UT1, (that is, fortnightly tidal terms have
!                             not been removed, as in the IRIS or IERS series).
!                             = -1 --> UT1 table coming from input database is
!                             UT1R, (that is, the Yoder fortnightly tidal terms
!                             HAVE been removed as in Bulletin B).
!                             = -2 --> UT1 table coming from input database is
!                             UT1S, (the S tidal terms HAVE been removed).
!             5. Usecubic   - Set to true if cubic interpolation to be used.
!             6. Uselinear  - Set to true if linear interpolation to be used.
!             7. Usespline  - Set to true if spline interpolation to be used.
!             8. Leap_fix   - Used in external input mode. .True. means
!                             correct the input EOP series for accumluated
!                             leap seconds. .False. means do not correct.
!             9. UT1type    - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '.
!                             For ''UT1-UTC ', leap second corrections
!                             must be made.
!            10. EOP_time_scale - EOP table time scale, allowed values:
!                             'TAI     ', 'TCG     ', 'TDB     ',
!                             'TDT     ', 'UTC     ', 'UNDEF   '.
!            11. IEPOCH     - The number of epochs at which TAI - UT1 is
!                             desired.
!            12. ASKKER     - The database error return code from ASK.
!
