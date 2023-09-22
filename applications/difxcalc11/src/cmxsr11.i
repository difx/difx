!   cmxsr.i
!
!***The maximum number of sources can be changed in the parameter
!***statement here and is the only change necessary.
!
! Written: B. Archinal, 95.11.13.
!    98.09.08 - D. Gordon: Added variables P_motion, D_psec, Pmotion, and
!               Dpaec for proper motion and parallax corrections, mainly
!               for correlator usage; added additional documentation of
!               variables.
!    98.11.25 - D. Gordon: Added variable PRcorr to hold proper motion
!               RA and Dec corrections for each source.
!  2015.01.05 - D. Gordon: changed LNSTAR(4,MAX_ARC_SRC) to
!               LNSTAR(10,MAX_ARC_SRC) to support source names with
!               up to 20 characters in difx mode.
!
!   Maximum number of radio sources per database.
!   (should be equal to or greater than MAX_ARC_SRC in
!   $MK5_ROOT/include/solve.i and MAXSTR in
!   $MK5_ROOT/include/dbedit_sitstr.i). (mark3 mode only)
!
      Integer*4 MAX_ARC_SRC
      Parameter(MAX_ARC_SRC=300)
!
      Real*8     CD, CRA, RADEC(2,MAX_ARC_SRC), SD, SRA,                &
     &           P_motion(3,MAX_ARC_SRC), D_psec(MAX_ARC_SRC),          &
     &           PRcorr(2,MAX_ARC_SRC)
      Integer*4  Pmotion, Dpsec
!!    Integer*2  NUMSTR, LNSTAR(4,MAX_ARC_SRC), i1dum
      Integer*2  NUMSTR, LNSTAR(10,MAX_ARC_SRC), i1dum
      COMMON / STRCM / CD, CRA, RADEC, SD, SRA, P_motion, D_psec,       &
     &                 PRcorr, Pmotion, Dpsec, LNSTAR, NUMSTR, i1dum
!
!   Variables:
!       1. CD                    - Cosine of Declination of source in the
!                                  current observation.
!       2. CRA                   - Cosine of Right Ascension of source in the
!                                  current observation.
!       3. RADEC(2,MAX_ARC_SRC)  - THE RIGHT ASCENSIONS AND DECLINATIONS OF THE
!                                  STARS IN THE STAR CATALOG. (RAD, RAD)
!       4. SD                    - Sine of Declination of source in the
!                                  current observation.
!       5. SRA                   - Sine of Right Ascension of source in the
!                                  current observation.
!       6. P_motion(3,Max_arc_src)-Proper motion array. 1) Proper motion in RA,
!                                  2) Proper motion in Dec, and 3) Epoch for
!                                  which RADEC( ) above is correct, for each
!                                  source. (arc-sec/year, arc-sec/year,
!                                  year (1995.0, etc.))
!       7. D_psec(Max_arc_src)   - The estimated distances to each source.
!                                  (Parsecs)
!       8. PRcorr(2,Max_arc_src) - Proper motion corrections in RA and
!                                  Declination. (radians)
!       9. Pmotion               - Proper motion indicator. If greater than
!                                  zero, compute proper motion contributions.
!      10. Dpsec                 - Parallax indicator. If greater than zero,
!                                  compute parallax contributions.
!      11. NUMSTR                - THE NUMBER OF STARS IN THE STAR CATALOG.
!      12. LNSTAR(10,MAX_ARC_SRC)- THE ALPHANUMERIC CHARACTER NAMES OF
!                                  THE STARS IN THE STAR CATALOG, up to 20 
!                                  characters long.
!
