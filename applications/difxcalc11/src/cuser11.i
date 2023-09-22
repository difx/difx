!  cuser11.i  - Calc user type
!
!  Calc 11 mode = C_mode. 
       Character*6 C_mode
       Common /mode/ C_mode
!  C_mode is set to 'mark3 ' in cmain.f for calc11
!   or:
!  C_mode is set to 'difx  ' in dmain.f for dcalc
!
!       Parameter (C_mode = 'mark3 ')
!        Parameter (C_mode = 'difx  ')
!        Parameter (C_mode = 'nusolv')
!
       Logical*4 Near_field
!  The Near_field flag is either '.TRUE.' or '.FALSE.'. It will
!   be set to .TRUE. if there is a near-field flag in the database 
!   header or if there are spacecraft positions in the .calc file.  
!
       Character*1 Calc_user, Apply_ocean, Compute_partials,            &
     &             Wet_atm, Hifreq_tidal_EOP, Hifreq_nut_EOP,           &
     &             dum11, dum12
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Calc_user flag is obsolete in the difx mode. Leave it set to 'A'.
!      Will be cleaned up later. 2013-APR-29
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Set Calc_user = 'A' for use at Mark III VLBI analysis centers:
       Parameter (Calc_user = 'A')
!
!  Set Calc_user = 'C' for use at VLBI correlators:
!      Parameter (Calc_user = 'C')
!
!  Set Calc_user = 'D' for use with difx correlators:
!      Parameter (Calc_user = 'D')
!
!************************************************************************
!!!!!!!  These are also obsolete in the difx mode.
!      Will be cleaned up later. 2013-APR-29
!
!  The following apply only for correlator users (Calc_user = 'C')
!   Also for difx users (Calc_user = 'D')
!
!   Apply ocean loading to theoreticals (recommended).
      Parameter (Apply_ocean = 'Y')
!   Don't apply ocean loading to theoreticals (not recommended).
!     Parameter (Apply_ocean = 'N')
!
!   Correlator option to combine dry and wet Niell model atmosphere
!    corrections. Set Wet_atm = 'Y' to combine both wet and dry in
!    DATMC. Set Wet_atm = 'N' to use only the dry component.
      Parameter (Wet_atm = 'Y')
!     Parameter (Wet_atm = 'N')
!
!  Correlator option to add the high frequency (diurnal and sub-diurnal)
!   EOP corrections due to ocean tides. 'Y' is recommended.
      Parameter (Hifreq_tidal_EOP = 'Y')
!     Parameter (Hifreq_tidal_EOP = 'N')
!
!  Correlator option to add the high frequency (diurnal and sub-diurnal)
!   EOP corrections due to nutation. 'Y' is recommended.
      Parameter (Hifreq_nut_EOP = 'Y')
!     Parameter (Hifreq_nut_EOP = 'N')
!
!  Switch to turn off unnecessary computations (mostly partials)
!  **** The following not yet implemented ****
!    'N' means don't compute partials (to save computing time)
!     Parameter (Compute_partials = 'N')
!     Parameter (Compute_partials = 'Y')
