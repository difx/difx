C  Include file cmxut.i
      Integer*4 Nspline
      Logical*4 Usecubic, Uselinear, Usespline, Leap_fix,
     *          apply_tidal_correction, table_found
      Integer*2 ISHRTFL, IEPOCH, ASKKER, Intrvl(5,2), ndays
      Character*8 EOP_time_scale, UT1type
      Real*8      UT1IF(4), UT1PT(20), CENTJ, DJ1900, Y1(2),
     *            Y2(2), T, S, SERSHO(4), DJ2000, Xintv(2),
     *            UT1RS(20), XT(20), YA(20), Y2S(20)
C
      COMMON / UT1CM / UT1IF, UT1PT, CENTJ, DJ1900, Y1, Y2, T, S, 
     *                 SERSHO, DJ2000, Xintv, UT1RS, XT, YA, Y2S, 
     *                 EOP_time_scale, UT1type,
     *                 Nspline, Usecubic, Uselinear, Usespline,
     *                 apply_tidal_correction, table_found, Leap_fix, 
     *                 Intrvl, ISHRTFL, IEPOCH, ASKKER, ndays
C
C            Variables:
C              1. UT1IF(4)  - The final UT1 information array. This array
C                             contains respectively: 1) The Julian date of the
C                             first tabular point, 2) The increment in days of
C                             the tabular points, 3) The number of tabular
C                             points, 4) The units of the UT1 tabular array per
C                             second. (days, days, unitless, sec/table unit)
C              2. UT1PT(20) - The tabular values of 'TAI minus UT1'.
C                             (table units)
C              3. UT1RS(20) - The table of 'TAI-UT1S' or 'TAI-UT1R' values,
C                             as controlled by KUT1C. (seconds)
C              4. ISHRTFL   - The short period tidal terms flag, (unitless).
C                             = 1 --> UT1 table coming from input database is
C                             true UT1, (that is, fortnightly tidal terms have
C                             not been removed, as in the IRIS or IERS series).
C                             = -1 --> UT1 table coming from input database is
C                             UT1R, (that is, the Yoder fortnightly tidal terms
C                             HAVE been removed as in Bulletin B).
C                             = -2 --> UT1 table coming from input database is
C                             UT1S, (the S tidal terms HAVE been removed).
C             5. Usecubic   - Set to true if cubic interpolation to be used.
C             6. Uselinear  - Set to true if linear interpolation to be used.
C             7. Usespline  - Set to true if spline interpolation to be used.
C             8. Leap_fix   - Used in external input mode. .True. means
C                             correct the input EOP series for accumluated
C                             leap seconds. .False. means do not correct.
C             9. UT1type    - UT1 data type: 'UT1-TAI ' or 'UT1-UTC '.
C                             For ''UT1-UTC ', leap second corrections
C                             must be made.
C            10. EOP_time_scale - EOP table time scale, allowed values:
C                             'TAI     ', 'TCG     ', 'TDB     ',
C                             'TDT     ', 'UTC     ', 'UNDEF   '.
C            11. IEPOCH     - The number of epochs at which TAI - UT1 is 
C                             desired.
C            12. ASKKER     - The database error return code from ASK.
C
