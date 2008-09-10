      SUBROUTINE DIRNA
      IMPLICIT NONE
C
C  Add to TOC variables of new equation of equinox procedure
C
C       PROGRAMMER:
C         93.09.01 Norbert Zacharias - Equation of the equinoxes GAST update 
C         94.01.04 David Gordon - Cleaned up TOC entries.
C         98.01.20 David Gordon - New TOC entries for New definition of the
C                                 equation of the equinoxes. Removed Lcodes 
C                                 for previous EQE (calc 8.x) contributions.
C 
       INCLUDE 'ccon.i'             
C            VARIABLES 'FROM':
C              1. KDIUC - THE DIURNAL SPIN UTILITY ROUTINE FLOW CONTROL FLAG.
C                         = 0 (default) Use new definition of the equation of
C                             the equinoxes.
C                         = 1 Use old definition of the equation of the 
C                             equinoxes.
C
C  Lcode for difference in GAST (UT1)  
C    [Even though we change the default to use the New definition, we keep this
C     Lcode the same as before so we don't have to change the name.]
       CALL ADDR (2,'EQE DIFF','GAST diff. (New-Old) from EQE   ',
     *           1,1,1)
C
C   Add contribution Lcode for the delay and rate contribution due to the 
C   change in the definition of the equation of the equinox. The default is 
C   now to use the New definition. Also remove previous EQE contribution
C   Lcodes that might be in the database.
        CALL DELR(2,'EQE CONT')
C    For the default (New definition), put in Lcode to convert back to the old
C    definition. 
      If(KDIUC .eq. 0) Then 
       CALL ADDR (2,'OLDEQCON','Old Eqn. of Equinoxes Contrib.  ',
     *           2,1,1)
       CALL DELR(2,'NEWEQCON')
      Endif
C
C    For the Old definition, put in Lcode to convert to the new definition. 
      If(KDIUC .eq. 1)  Then
       CALL ADDR (2,'NEWEQCON','New Eqn. of Equinoxes Contrib.  ',
     *           2,1,1)
        CALL DELR(2,'OLDEQCON')
      Endif
C
      Return
      END 
C
C*************************************************************************** 
      SUBROUTINE DIRNL ( DATDCT, DPSI, DUT1AT, EPS, DEPS, FA, UT1, 
     1                   XJD, DIURNV, GAST, GMST, GASTD, RS )
      IMPLICIT None
C
C 1.    DIRNL
C
C 1.1   DIRNL PROGRAM SPECIFICATION
C
C 1.1.1 DIRNL is the utility routine which computes the diurnal spin portion of
C       the complete crust fixed to J2000.0 rotation matrix and its first two CT
C       time derivatives. DIRNL also computes the diurnal angular velocity of
C       the Earth, the Greenwich Mean Sidereal Time, and the Greenwich Apparent
C       Siderial Time (GAST) and its CT time derivative.
C
C 1.1.2 RESTRICTIONS - NONE
C
C 1.1.3 REFERENCES - 1) 'THE EXPLANATORY SUPPLEMENT TO THE AMERICAN
C                       EPHEMERIS AND NAUTICAL ALMANAC", P.72-76,
C                    2) AOKI, S. ET AL., "THE NEW DEFINITION OF UNIVERSAL
C                       TIME", ASTRON. ASTROPHYS., ????,1980.
C                    3) McCarthy, D., IERS Technical Note 13, Paris 1992
C
C 1.2   DIRNL PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE -
C
C           INPUT VARIABLES:
C             1. DATDCT   -  THE PARTIAL DERIVATIVE OF ATOMIC TIME WITH
C                            RESPECT TO COORDINATE TIME. (SEC/SEC)
C             2. DPSI(2)  -  THE NUTATION IN LONGITUDE AND ITS CT
C                            TIME DERIVATIVE. (RAD, RAD/SEC)
C             3. DUT1AT   -  THE PARTIAL DERIVATIVE OF UT1 TIME WITH
C                            RESPECT TO ATOMIC TIME. (SEC/SEC)
C             4. EPS(2)   -  THE TRUE OBLIQUITY OF THE ECLIPTIC AND ITS
C                            CT TIME DERIVATIVE. (RAD, RAD/SEC)
C             5. DEPS(2)  -  THE NUTATION IN OBLIQUITY AND ITS CT TIME
C                            DERIVATIVE COMPUTED FROM WAHR OR TAKEN FROM
C                            THE DATA BASE. (RAD, RAD/SEC)
C             6. FA(5)    -  The fundamental nutation arguments (see NUTFA)
C             6. OMEGA    -  The mean longitude of the ascending node of the
C                            Moon. (arcsec!)
C             7. UT1      -  THE UT1 TIME OF THE DAY. (SEC) 
C             8. XJD      -  THE JULIAN DATE AT ZERO HOURS UTC OF THE
C                            DATE IN QUESTION. (DAYS) 
C 
C           OUTPUT VARIABLES: 
C             1. DIURNV    -  THE DIURNAL ANGULAR VELOCITY OF THE EARTH.
C                             (RAD/SEC) 
C             2. GAST(2)   -  THE GREENWICH APPARENT SIDEREAL TIME AND
C                             ITS CT TIME DERIVATIVE. (RAD, RAD/SEC) 
C             3. GMST      -  THE GREENWICH MEAN SIDEREAL TIME. (RAD)
C             4. GASTD     -  The difference in GAST (new-old version)
C             5. RS(3,3,3) -  THE DIURNAL SPIN PORTION OF THE COMPLETE
C                             CRUST FIXED TO 2000.0 ROTATION MATRIX 
C                             AND THE FIRST TWO CT TIME DERIVATIVES OF
C                             THAT MATRIX. (UNITLESS, 1/SEC, 1/SEC**2) 
C
      Real*8 DATDCT, DPSI(2), DUT1AT, EPS(2), DEPS(2), FA(5), UT1, 
     .       XJD, DIURNV, GAST(2), GMST, GASTD, RS(3,3,3)
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C 
C            VARIABLES 'FROM':
C              1.  CONVHS  -  THE CONVERSION FACTOR OF RADIANS PER TIME-SECOND.
C                             (RAD/TIME-SEC)  
C 
           INCLUDE 'ccon.i'             
C            VARIABLES 'FROM':
C              1.  KDIUC  -  THE DIURNAL SPIN UTILITY ROUTINE FLOW CONTROL FLAG.
C              2.  KDIUD  -  THE DIURNAL SPIN UTILITY ROUTINE DEBUG OUTPUT FLAG.
C              3.  KNUTC  -  THE NUTATION MODULE FLOW CONTROL FLAG. USED TO 
C                            DETERMINE WHETHER OR NOT DPSI SHOULD BE ZEROED.  
C 
      Real*8 GMSTC(4), SIDVEL(3), DPSIS(2), epsmd, omega, CENTJ, DJ2000,
     .       GMSTMN, DAYSJ, CENT
      Integer*4 I
C 
      DATA  CENTJ  / 36525.D0 /,
     1      DJ2000 /  2451545.D0 /, 
     2      GMSTC  / + 24110.54841D0, + 8640184.812866D0, 
     3               + 0.093104D0, - 6.2D-6 /,
     4      SIDVEL / + 1.002737909350795D0, + 5.9006D-11, - 5.9D-15 / 
C 
C 1.2.4 DATA BASE ACCESS - NONE 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - 
C            INPUT VARIABLES - NONE
C            OUTPUT VARIABLES - POSSIBLE DEBUG OUTPUT 
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVR 
C             CALLED SUBROUTINES: DDROT, DROTT, ROTAT, EQEG 
C 
C 1.2.7 CONSTANTS USED - CENTJ, CONVHS, DJ2000, GMSTC(4), SIDVEL(3) 
C 
C 1.2.7.1 CONSTANTS INITIALIZED IN THIS UTILITY ROUTINE - 
C           1. CENTJ     -  THE NUMBER OF COORDINATE TIME DAYS PER JULIAN
C                           CENTURY (CENTJ = 36525.D0 DAYS/CENTURY) 
C           2. DJ2000    -  THE JULIAN DATE OF THE EPOCH JANUARY 1.5, 2000.
C                           (DJ2000 = 2451545.D0 DAYS) 
C           4. GMSTC(4)  -  THE CONSTANTS APPEARING IN TERMS 1-4 IN THE
C                           CALCULATION OF THE GREENWICH MEAN SIDEREAL TIME AT 
C                           ZERO HOURS UT1 OF THE DATE IN QUESTION. (TIME-SEC) 
C                           (SEE REFERENCES)
C                           ( GMSTC(1) = + 24110.54841D0, 
C                             GMSTC(2) = + 8640184.812866D0,
C                             GMSTC(3) = + 0.093104D0,
C                             GMSTC(4) = - 6.2D-6 ) 
C           4. SIDVEL(3) -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE
C                           CALCULATION OF THE EARTH'S DIURNAL ANGULAR VELOCITY.
C                           (TIME SEC/SEC) (SEE REFERENCES)
C                           ( SIDVEL(1) = + 1.002737909350795D0,
C                             SIDVEL(2) = + 5.9006D-11, 
C                             SIDVEL(3) = - 5.9D-15 ) 
C 
C 1.2.8 PROGRAM VARIABLES - 
C           1.  CENT    -  THE NUMBER OF JULIAN CENTURIES ELAPSED SINCE THE
C                          EPOCH JANUARY 1.5, 2000. (CENTURIES)
C           2.  DAYSJ   -  THE NUMBER OF JULIAN DAYS ELAPSED SINCE THE EPOCH
C                          JANUARY 1.5, 2000. (DAYS)
C           3.  GMSTMN  -  THE GREENWICH MEAN SIDEREAL TIME AT ZERO HOURS UT1
C                          OF THE DATE IN QUESTION. (RAD)
C           4. DPSIS(2) -  THE VALUE OF NUTATION USED IN THE CALCULATION OF
C                          SIDEREAL TIME.
C           5. EPSMD    -  Mean obliquity of the ecliptic at date. (rad)
C           6. OMEGA    -  Mean longitude of the ascending node of the Moon
C                          (arcsec)
C
C 1.2.9 PROGRAMMER 
C         770207  DALE MARKHAM
C         770718  PETER DENATALE 
C         771128  BRUCE SCHUPLER
C         790201  BRUCE SCHUPLER 
C         810804  CHOPO MA   
C         870604  SAVITA GOEL 
C         890726  Jim Ryan:          Documentation simplified.
C         930901  Norbert Zacharias: Equation of equinoxes GAST update 
C
C  DIRNL Program Structure
C
C   Compute mean obliquity of date, req. for EQE update in DIRNL
      epsmd = eps (1) - deps (1)
C   Mean longitude of ascending node of the Moon
      omega = fa (5) 

C   Compute the number of Julian days elapsed since the epoch
C   JANUARY 1.5, 2000 to 0 hours UT1 of the date in question.
      DAYSJ = XJD  -  DJ2000
C
C   Compute the number of Julian centuries elapsed since Epoch January 1.5, 2000
      CENT = DAYSJ / CENTJ
C
C   Compute the diurnal angular velocity of the Earth.
      DIURNV = ( SIDVEL(1) + SIDVEL(2) * CENT + SIDVEL(3) * CENT**2 )
     1       * CONVHS
C
C   Compute the Greenwich Mean Sidereal Time at 0 hours UT1 of the date
C   in question. (radians)
      GMSTMN = ( GMSTC(1) + GMSTC(2) * CENT + GMSTC(3) * CENT**2 +
     .           GMSTC(4) * CENT**3 ) * CONVHS
C
C   Compute the Greenwich Mean Sidereal Time.
      GMST = GMSTMN  +  DIURNV * UT1
      GMST = DMOD(GMST,TWOPI)
C
C   Check to see if the nutation in longitude is to be turned off.
      DO I=1,2
        IF (KNUTC .EQ. 1) DPSIS(I) = 0.0D0
        IF (KNUTC .NE. 1) DPSIS(I) = DPSI(I)
      ENDDO   
C
C  Compute the Greenwich Apparent Sidereal Time by calling the geometry part of
C   the equation of equinox procedure.
      CALL EQEG (gmst, dpsis(1), eps(1), epsmd, omega,
     .           gast, gastd)
C
C  Compute the time derivative of Greenwich Apparent Siderial Time
      GAST(2) = DIURNV * DUT1AT * DATDCT
     1        + DCOS ( EPS(1) ) * DPSIS(2)
     2        - DSIN ( EPS(1) ) * DPSIS(1) * EPS(2)
C
C   Construct the diurnal spin matrix and its first two CT time derivatives.
C
C   Construct the diurnal spin matrix.
      CALL ROTAT ( -GAST(1), 3, RS(1,1,1) )
C
C   Construct the first CT time derivative of the diurnal spin matrix
      CALL DROTT ( -GAST(1), -GAST(2), 3, RS(1,1,2) )
C
C   Construct the second CT time derivative of the diurnal spin matrix.
      CALL DDROT ( -GAST(1), GAST(2)**2, 3, RS(1,1,3) )
C
C****************************************************************************
C   Check KDIUD for debug output.
      IF ( KDIUD .NE. 0 )  THEN       
        WRITE ( 6, 9)
    9   FORMAT (1X, "Debug output for utillity DIRNL." )
        WRITE(6,8)' CENT    ',CENT
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' CENTJ   ',CENTJ
        WRITE(6,8)' DJ2000  ',DJ2000
        WRITE(6,8)' DPSIS   ',DPSIS
        WRITE(6,8)' CONVHS  ',CONVHS
        WRITE(6,8)' DAYSJ   ',DAYSJ
        WRITE(6,8)' GMSTC   ',GMSTC
        WRITE(6,8)' GMSTMN  ',GMSTMN
        WRITE(6,8)' SIDVEL  ',SIDVEL
C
        WRITE ( 6, 9200 )  DATDCT, DPSI, DUT1AT, EPS, UT1, XJD,
     1                     DIURNV, GAST, GMST, RS
 9200   FORMAT (1X, "DATDCT = ", D30.16, /, 1X,
     1            "DPSI   = ", 2 ( D30.16, 10X ), /, 1X,
     2            "DUT1AT = ", D30.16, /, 1X,
     3            "EPS    = ", 2 ( D30.16, 10X ), /, 1X,
     4            "UT1    = ", D30.16, /, 1X,
     5            "XJD    = ", D30.16, /, 1X,
     6            "DIURNV = ", D30.16, /, 1X,
     7            "GAST   = ", 2 ( D30.16, 10X ), /, 1X,
     8            "GMST   = ", D30.16, /, 1X,
     9            "RS     = ", 9 ( 3 ( D30.16, 10X ), /, 1X ) )
C
      ENDIF   ! debug output flag
C
      Return
      END 
C
C******************************************************************************
      SUBROUTINE EQEG (gmst,dpsi,eps,epsmd,omega, gast,gastd)
      IMPLICIT NONE
C
C  This subroutine computes the geometry section of the equation of equinox 
C  procedure.
C
C  References:  D.McCarthy, IERS Technical Note 13, Paris 1992
C
C  Calling sequence:
C    input variables:
C      gmst  = Greenwich mean siderial time           (radian)
C      dpsi  = nutation in longitude                  (radian)
C      eps   = true obliquity of the ecliptic         (radian)
C      epsmd = obliquity of the mean ecliptic of date (radian)
C      omega = mean longitude of the asc. node of the moon (arcsec) 
C
C    output variables:
C      GAST(1)=Greenwich apparent siderial time (New or Old depending on KDIUC)
C               (radians)
C      GASTD = Difference in GAST of two versions (New-Old) (radians)
C
      REAL*8   gmst, dpsi, eps, epsmd, omega, gast(2), gastd
      REAL*8   old_gast, aswas_gast, new_gast,
     .         eqe_const(2), omega_rad
      REAL*8   pi, twopi, halfpi, convd, convds, convhs, secday
      COMMON /CMATH/ pi, twopi, halfpi, convd, convds, convhs, secday
C
C     Variables from:  convds = convert arcsec to radian
C
       INCLUDE 'ccon.i'
C        Variables from:
C           KDIUC - Flow control flag for EQE routines
C                   = 0, GAST defined by new equation of equinoxes definition
C                   = 1, GAST defined by old equation of equinoxes definition
C           KDIUD - Debug flag for EQE routines
C
C  Data base access: None
C
C  Subroutine interface:
C     caller subr.: DIRNL
C     called subr.: None 
C
C  Program variables:
C     eqe_const   = Constants in new version of GAST formula
C                   depending on sine of omega  (arcsec)
C     aswas_gast  = GAST as calculated incorrectly before (CALC 7.6 and 
C                   earlier). (radian)
C     old_gast    = GAST as calculated before (CALC 7.6 and earlier) but 
C                   corrected to use mean obliquity of date, to be used until
C                   1997.02.26. (radians)
C     omega_rad   = Omega (longit. of asc. node of moon) (radians)
C     new_gast    = GAST for use after 1997.02.26 as recommended by the IERS.
C
C  Programmer:
C         930901 Norbert Zacharias: first implementation, equat.of eq. GAST
C                                   update.
C         94.01.04 David Gordon - Clean up documentation, change sign of GAST
C                                 correction.
C         98.01.20 David Gordon - Updated to use new definition EQE
C
      DATA eqe_const / 0.00264D0, 0.000063D0 /  ! in arc sec
C
C  compute old GAST as before, use true obliquity eps as before
      aswas_gast = gmst + dpsi * DCOS (eps)
C
C  compute old GAST (formula in effect until 19970226)
C  but with correct epsilon (mean obliquity of date)
      old_gast = gmst + dpsi * DCOS (epsmd)
C
C  compute new GAST as recomm. in IERS techn.note
      omega_rad = omega * convds
      new_gast  = gmst + dpsi * DCOS (epsmd) 
     .            + eqe_const(1) * convds * DSIN (omega_rad)
     .            + eqe_const(2) * convds * DSIN (2.0d0*omega_rad)
C
C  Calc 9, use new definition
      If (KDIUC.eq.0) gast(1)  = new_gast
C  For diehards, allow using old definition
      If (KDIUC.eq.1) gast(1)  = old_gast
C  Define difference, Gastd, as (New - Old)
      gastd = new_gast - old_gast 
C
C  check debug output flag
      IF (KDIUD.NE.0) THEN
	WRITE (6,'(1x,a)') 'debug output for EQEG'
	WRITE (6,'(1x,a,d25.16)') 'gmst       = ', gmst
	WRITE (6,'(1x,a,d25.16)') 'dpsi       = ', dpsi  
	WRITE (6,'(1x,a,d25.16)') 'eps        = ', eps 
	WRITE (6,'(1x,a,d25.16)') 'epsmd      = ', epsmd   
	WRITE (6,'(1x,a,d25.16)') 'omega      = ', omega   
	WRITE (6,'(1x,a,d25.16)') 'aswas_gast = ', aswas_gast
	WRITE (6,'(1x,a,d25.16)') 'old_gast   = ', old_gast
	WRITE (6,'(1x,a,d25.16)') 'new_gast   = ', new_gast
	WRITE (6,'(1x,a,d25.16)') 'gast(1)    = ', gast(1)
	WRITE (6,'(1x,a,d25.16)') 'aswas-old  = ', aswas_gast-old_gast
	WRITE (6,'(1x,a,d25.16)') 'gastd      = ', gastd   
      ENDIF
C
      Return
      END 
C ****************************************************************************
      SUBROUTINE DIRNC (gastd, dut1p)
      IMPLICIT NONE
C
C  This subr. computes the contributions to the delay and delay rate due to the
C  different versions of the equation of equinox procedure. The New version  
C  (to be used after 26 Feb 1997) becomes the default in Calc 9.0. To get back
C  to the old definition, simply add the contribution Lcode OLDEQCON during
C  SOLVE analysis. 
C
C  Users can still use the old EQE definition by setting KDIUC=1. In that case,
C  a diferent contribution goes into the database, 'NEWEQCON'. When NEWEQCON
C  is added to the delays and rates in this case, they will be converted to 
C  the new definition. Don't count on this being supported in SOLVE though. 
C
C  References: D.McCarthy, IERS Technical Note 13, Paris 1992
C              E-Mail Memo, 'UT1 Errors', D. Gordon, 10 April, 1997
C
C  Calling sequence:
C    Input Variables:
C       GASTD      = Differ. in GAST of the two versions (new-old) (radian) 
C       DUT1P(2,2) = Partial derivatives of delay and rate wrt (A1-UT1) 
C                    (First index - delay and rate, second index - first and 
C                     second derivatives) (s/s, s/s**2)
C         [Note: The partial we want here is actually the delay and rate
C                derivatives wrt GAST. We approximate this by dividing 
C                dut1p by -1.0027379 sidereal days/solar day.] 
C
      REAL*8   gastd, dut1p(2,2), deqec(2)
      REAL*8   pi,twopi, halfpi, convd, convds, convhs, secday
      COMMON /CMATH/ pi,twopi,halfpi, convd, convds,
     .               convhs, secday
C      Variables From: 
C           Convhs = conversion factor (radian/sec.of time)
C
       INCLUDE 'ccon.i'
C      Variables From:
C           KDIUC - Flow control flag for EQE routines
C                   = 0, GAST defined by new equation of equinoxes definition
C                   = 1, GAST defined by old equation of equinoxes definition
C           KDIUD - debug flag for EQE routines
C
C  Data base access:
C     PUT4 GASTD    = differ. in GAST (new - old) (radian)
C     PUT4 DEQEC(2) = contrib. of GASTD to delay and delay rate
C                     (sec, sec/sec)
C  Subroutine interface:
C     Caller Subr.: DRIVR
C     Called Subr.: None
C
C  Programmer:
C    930901 Norbert Zacharias: first implementation
C    94.01.04 David Gordon - Fixed up documentation, sign of contributions 
C                            flipped in EQEG.
C    94.06.02 David Gordon - Changed PUTR to PUT4.
C    95.02.09 David Gordon - Corrected PUT4's of 'EQE DIFF' and 'EQE CONT'.
C                            Only 8 charcters allocated, changed to 14. Bug
C                            found by Bill Petrachenko using a Sun workstation
C                            at the Dominion Radio Astrophysical Obs. (DRAO).
C    98.01.20 David Gordon - Corrected old (Calc 8.x) delay and rate 
C                            contribution (Deqec) by dividing by -1.0027379 
C                            siderial days/solar day. Changed database Lcode to
C                            'NEWEQCON' (from 'EQE CONT') and made it the 
C                            non-default. Added 'OLDEQCON' Lcode contribution
C                            and made it the default.
C
C  Calculate contributions to delay and rate
C
C   Default case, contribution to convert from New to Old EQE definition
      If (KDIUC .eq. 0) then 
       deqec (1) = -gastd * dut1p(1,1) / (-1.0027379 * convhs)
       deqec (2) = -gastd * dut1p(2,1) / (-1.0027379 * convhs)
       CALL PUT4 ('OLDEQCON      ', deqec, 2,1,1)
      Endif
C
C   Non-default case, contribution to convert from Old to New EQE definition
      If (KDIUC .eq. 1) then 
C    Correct Calc 8.2 error by dividing by -1.0027379
       deqec (1) = gastd * dut1p(1,1) / (-1.0027379 * convhs)
       deqec (2) = gastd * dut1p(2,1) / (-1.0027379 * convhs)
       CALL PUT4 ('NEWEQCON      ', deqec, 2,1,1)
      Endif
C
C  Output EQE difference to data base
      CALL PUT4 ('EQE DIFF      ', gastd, 1,1,1)
C
C  check flag for debug output:
      IF (KDIUD.NE.0) THEN
	WRITE (6,'(1x,a)') 'debug output for EQEC'
	WRITE (6,'(1x,a,2d25.16)') 
     .    'dut1p         = ', dut1p(1,1), dut1p(2,1)
	WRITE (6,'(1x,a,2d25.16)')
     .    'gastd, convhs = ', gastd, convhs
	WRITE (6,'(1x,a,2d25.16)') 'deqec         = ', deqec   
      ENDIF
C
      Return
      END 
