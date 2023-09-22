C  !CUSER  - Calc user type
C
       Character*1 Calc_user, Apply_ocean, Compute_partials, 
     *             Wet_atm 
       Character*2 Use_Tide 
C
       Common /cuser/ Calc_user, Wet_atm
C
C  Set Calc_user = 'A' for use at Mark III VLBI analysis centers:
C      Parameter (Calc_user = 'A')
C
C  Set Calc_user = 'C' for use at VLBI correlators:
C  CALC server gets Calc_user and Wet_atm from enviroment variables
C  retrieved in calcinit.f
C      Parameter (Calc_user = 'C')
C
C************************************************************************
C  The following apply only for correlator users (Calc_user = 'C')
C
C   Apply ocean loading to theoreticals (recommended).
      Parameter (Apply_ocean = 'Y')
C   Don't apply ocean loading to theoreticals (not recommended).
C       Parameter (Apply_ocean = 'N')
C
C   Set Use_Tide = 'TF' to apply the IERS 1992 solid Earth 
C    tides (tide free frame). This is strongly recommended at 
C    the current time (~Oct. 1999) for consistency with current 
C    sets of site a priori positions.
      Parameter (Use_Tide = 'TF')
C
C   Set Use_Tide = 'RC' to apply the IERS 1996 solid Earth tides
C    (real crust frame). This is NOT recommended at the current
C    time because most apriori site positions are based on the
C    tide free frame, and up to 11 cm vertical errors can result.  
C     Parameter (Use_Tide = 'RC')
C
C   Correlator option to combine dry and wet Niell model atmosphere
C    corrections. Set Wet_atm = 'Y' to combine and use both wet
C    and dry. Set Wet_atm = 'N' to use only the dry component.
C      Parameter (Wet_atm = 'Y')
C     Parameter (Wet_atm = 'N')
C
C  Switch to turn off unnecessary computations (mostly partials)
C  **** The following not yet implemented ****
C    'N' means don't compute partials (to save computing time)
C     Parameter (Compute_partials = 'N')
C     Parameter (Compute_partials = 'Y')