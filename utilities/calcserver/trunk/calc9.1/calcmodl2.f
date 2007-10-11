      SUBROUTINE CALCMODL2 (MJD,TIME,SRCID,STNAID,STNBID,DELAY,RATE,
     *                     U,V,W, ATMOS, DATMOS, ACCEL,
     *                     RISETIME, SETTIME, XELEV, RELEV,
     *                     XAZ, RAZ, PARTIALS, IRETURN)
C-------------------------------------------------------------------
C
C     CALCMODL calls the CALC driver subroutine (DRIVR).
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z) 
      REAL*8   D_FTOC                                   
      REAL*8   TIME,UTCSEC,DELAY,RATE,U,V,W, TWOPI, XAZ(2), RAZ(2)
      REAL*8   XDELAY,XRATE,SEQ, BSLN(3,2), SRC(3), EARTH(3,3)
      REAL*8   RISETIME,SETTIME,NEXTELEV,SRCELEV(2,2), CALC_DELAY
      REAL*8   ACCEL,UT1UTC, XPOLE, YPOLE, ATMOS(2,2), DATMOS(2,2)
      REAL*8   XELEV(2), RELEV(2), RC_PLUS_EPS2(3), DLYSAVE(2), EPS2(3)
      REAL*8   CLIGHT, R2_SSB(3), RC_SSB(3), R2_GEO(3), RC_GEO(3)
      REAL*8   BETA(3), BETA2(3), B_SQR, B_SQR2, GAMMA, BDOTR_SSB
      REAL*8   BETADOTSTN, BETADOTRC, PARALLAX, RDIST, BETADOTBETA2
      REAL*8   RSOURCE, ONE_LY, PARTIALS(28)
      REAL*8   EPS_SQR, RCDOTEPS, RCDOTB2, EPS_CURVE(3), DLYCRV
      REAL*8   DEL_CURVE
      REAL*8   CALC_GRAVDLY, GRAV_TERM1, GRAV_TERM2, VECMG, C_TERM
      REAL*8   STN1_SC(3), STN2_SC(3), MP_STN1(3), MP_STN2(3), MP_SC(3)
      REAL*8   MP_SC1(3), MP_SC2(3), R1_R2(3), GEO_SC(3)
      REAL*8   UNIT_R1(3), UNIT_SC(3), UNIT_R1SC(3)
      REAL*8   R1MPT1(3), R2MPT1(3)
      REAL*8   MAG_STN1_SC, MAG_STN2_SC, MAG_MP_STN1, MAG_MP_STN2
      REAL*8   MAG_MP_SC, MAG_MP_SC1, MAG_MP_SC2
      REAL*8   SUNGRAVDLY, MOONGRAVDLY, EARGRAVDLY, PLANGRAVDLY
      INTEGER*4  MJD, SRCID, STNAID, STNBID, YEAR, MONTH, DAY
      INTEGER*4  UTCTAG(5), I, J, ISOL, ITER_SOL, IRETURN
C
      COMMON /CALCM/ LSEG(3,11),NSEG
C
      include 'CALCIO.INC'
      include 'ccon.i'
C
C--------------------------------------------------------------------
C
      IRETURN = 0
      TWOPI = 6.2831853071795864769D0
      CLIGHT = 299792458.0D0
C
C     One light year in meters
      ONE_LY = 9.4605D+15
C
C----------------------------------------------------------------------
C     Normally CALC calls OBSNT to get the observing date and time,
C     stations and source. We to it by hand here.
C----------------------------------------------------------------------
C
      UTCTAG(4) = TIME * 24.0D0 / TWOPI
      UTCTAG(5) = ((TIME * 24.0D0 / TWOPI - DFLOAT(UTCTAG(4)))*60.0D0
     *          + 0.01)
      UTCSEC    = TIME * 86400.D0 / TWOPI - DFLOAT(UTCTAG(4))*3600.D0
     *          - DFLOAT(UTCTAG(5))*60.D0
C     I         = UTCSEC + 0.1
C     UTCSEC    = DFLOAT(I)
C
      CALL MJD2DAY(MJD,YEAR,MONTH,DAY)
      UTCTAG(1) = YEAR 
      UTCTAG(2) = MONTH
      UTCTAG(3) = DAY
      MJDATE    = MJD
C
C     Load date and time into COMMON variable
C
      DO I = 1, 5
         GETTAG(I) = UTCTAG(I)
      END DO
C
      GETSEC = UTCSEC
C
      GETSRC = SRCID + 1
      GETSIT(1) = STNAID + 1
      GETSIT(2) = STNBID + 1
C
C----------------------------------------------------------------------
C     Call the CALC DRIVR subroutine that does it all.
C----------------------------------------------------------------------
C
      CALL DRIVR (BSLN,SRC,EARTH,SRCELEV)
      IF (IRET.EQ.1) THEN
         IRETURN = IRET
         RETURN
      END IF
C
C     Retrieve CALC delay and rate from COMMON variables
      XDELAY = PUTDLY(1) + PUTDLY(2)
      XRATE  = PUTRAT
      DELAY  = XDELAY
      RATE   = XRATE
C
C     Get the Calc atmos delay and rate from common in CALCIO.INC
C     Pass the dry and wet delays and rates. First index is station a
C     or station b, second index is dry atm or wet atm.
C
      ATMOS(1,1)  = ATMDLY(1)
      ATMOS(2,1)  = ATMDLY(2)
C      ATMOS(1,2)  = ATMDLY(3)
C      ATMOS(2,2)  = ATMDLY(4)
C
      DATMOS(1,1) = ATMRATE(1)
      DATMOS(2,1) = ATMRATE(2)
C      DATMOS(1,2) = ATMRATE(3)
C      DATMOS(2,2) = ATMRATE(4)
C
C
C----------------------------------------------------------------------
C     Calculate U,V,W based on source and baseline vectors from CALC.
C     U,V,W are in meters and in the J2000 frame. Diurnal and annual
C     aberration corrections are NOT applied to U,V,W.
C----------------------------------------------------------------------
C
      SEQ = DSQRT(1.0D0 - SRC(3)*SRC(3))
      U = -BSLN(1,1)*SRC(2)/SEQ + BSLN(2,1)*SRC(1)/SEQ
      V = -BSLN(1,1)*SRC(1)*SRC(3)/SEQ - BSLN(2,1)*SRC(2)*SRC(3)/SEQ
     +    +BSLN(3,1)*SEQ
      W =  BSLN(1,1)*SRC(1) + BSLN(2,1)*SRC(2) + BSLN(3,1)*SRC(3)
C
C----------------------------------------------------------------------
C     Calculate the projection of the earth's accelaration (orbital)
C     in the direction of the source
C----------------------------------------------------------------------
C
      ACCEL = SRC(1)*EARTH(1,3)
     *      + SRC(2)*EARTH(2,3)
     *      + SRC(3)*EARTH(3,3)
C
C
C----------------------------------------------------------------------
C     Calculate the source rise and set times. Correlator will flag
C     records when the source is below the horizon.
C----------------------------------------------------------------------
C
C     The VLBA antenna elevation limit is 2.25 degrees
C
      RISETIME = DFLOAT(MJD) + 0.0
      SETTIME  = DFLOAT(MJD) + 1.0
      NEXTELEV = SRCELEV(2,1) + SRCELEV(2,2) * 120.0
      XELEV(1) = PUTEL(1,1)
      XELEV(2) = PUTEL(2,1)
      RELEV(1) = PUTEL(1,2)
      RELEV(2) = PUTEL(2,2)
      XAZ(1)   = PUTAZ(1,1)
      XAZ(2)   = PUTAZ(2,1)
      RAZ(1)   = PUTAZ(1,2)
      RAZ(2)   = PUTAZ(2,2)
C
C     The source is below the elevation limit for the next two mins.
C
      IF (SRCELEV(2,1).LE.3.927D-2 .AND. NEXTELEV.LE.3.927D-2) THEN
         RISETIME = DFLOAT(MJD) + 1.0
         SETTIME  = DFLOAT(MJD) + 2.0
      END IF
C
C     The source rises during the next two minutes.
C
      IF (SRCELEV(2,1).LT.3.927D-2 .AND. NEXTELEV.GE.3.927D-2) THEN
         RISETIME = DFLOAT(MJD) + TIME / TWOPI 
     *            + (3.927D-2 - SRCELEV(2,1)) / (SRCELEV(2,2)*86400.0)
      END IF
C
C     The source sets during the next two minutes.
C
      IF (SRCELEV(2,1).GE.3.927D-2 .AND. NEXTELEV.LT.3.927D-2) THEN
         SETTIME  = DFLOAT(MJD) + TIME / TWOPI 
     *            + (3.927D-2 - SRCELEV(2,1)) / (SRCELEV(2,2)*86400.0)
      END IF
C
C----------------------------------------------------------------------
C     Load the Calc partial derivatives in CALCIO.INC common into the
C     argument variable PARTIALS.
C----------------------------------------------------------------------
C
C      DO I = 1, 2
C         PARTIALS(I)   = DRYATMP(I)
C         PARTIALS(I+2) = WETATMP(I)
C         PARTIALS(I+4) = AXOP(I)
C         PARTIALS(I+6) = SITDLYP(I)
C      END DO
C      DO I = 1, 4
C         PARTIALS(I+8)  = SITDLYP(I+2)
C         PARTIALS(I+12) = SRCDLYP(I)
C         PARTIALS(I+16) = UT1P(I)
C         PARTIALS(I+20) = WOBP(I)
C      END DO
C
C=======================================================================
C     Get the source parallax from the job script structure. If non-zero
C     calculate a near-field delay correction, and recalculate the
C     gravitational bending for the sun, earth, moon, planets..
C-----------------------------------------------------------------------
C
c      write (6,3001) "C_SUN   = ", C_SUN
c      write (6,4002) "R1SUNT1 = ", R1SUNT1(1),R1SUNT1(2),R1SUNT1(3)
c      write (6,4002) "R2SUNT1 = ", R2SUNT1(1),R2SUNT1(2),R2SUNT1(3)
c      write (6,3001) "C_EARTH   = ", C_EARTH
c      write (6,4002) "R1EARTHT1 = ", R1EARTHT1(1),
c     +                R1EARTHT1(2),R1EARTHT1(3)
c      write (6,4002) "R2EARTHT1 = ", R2EARTHT1(1),
c     +                R2EARTHT1(2),R2EARTHT1(3)
c      write (6,3001) "C_MOON   = ", C_MOON
c      write (6,4002) "R1MOONT1 = ", R1MOONT1(1),
c     +                R1MOONT1(2),R1MOONT1(3)
c      write (6,4002) "R2MOONT1 = ", R2MOONT1(1),
c     +                R2MOONT1(2),R2MOONT1(3)
c      DO I = 1, 7
c         write (6,3001) "C_PLANET = ", C_PLAN(I)
c         write (6,4002) "R1PLANT1 = ", R1PLANT1(1,I),
c     +                   R1PLANT1(2,I),R1PLANT1(3,I)
c         write (6,4002) "R2PLANT1 = ", R2PLANT1(1,I),
c     +                   R2PLANT1(2,I),R2PLANT1(3,I)
c      END DO
c      write (6,*) "------------------------------------------------"

      PARALLAX = D_FTOC (JOBNUM, 'SOURCE', SRCID , 'PARALLAX')  
C      write (6,*) "parallax = ", PARALLAX   
C      write (6,*) "gravdly = ", gravdly
      IF (PARALLAX .LE. 0.0)
     +   GO TO 900
C -----------------------------------------------------------------------
C     Calculate the near-field delay correction following the article
C     "Astrometry and Geodesy with Radio Interferomety: Experiments,
C      Models, Results", Sovers, Fanselow, and Jacobs. 1998. 
C     Reviews of Modern Physics, Vol. 70, Oct 1998.
C
C
C     Calculate the distance to the source in meters
C
      RSOURCE = 206265.0D0 * 499.004782D0 * CLIGHT / PARALLAX
c      write (6,*)"rsource = ", RSOURCE
C      write (6,*)"rdist (secs) = ", RSOURCE/CLIGHT
C
C     Don't calculate a near-field correction beyond one light year
C
      IF (RSOURCE .GE. ONE_LY)
     +   GO TO 900
C
C    This is the actual delay from Calc in microsecs.
C
      CALC_DELAY = DELAY
C
C     Calculate BETA (geocentric velocity in solar system barycenter)
C     Calculate BETA2 = BETA + station #2 velocity
C
      B_SQR = 0.0D0
      B_SQR2 = 0.0D0
      DO I = 1, 3
         BETA(I)  = EARTH(I,2) / CLIGHT
         BETA2(I) = BETA(I) - BSLN(I,2) / CLIGHT
         B_SQR    = B_SQR + BETA(I) * BETA(I)
         B_SQR2   = B_SQR2 + BETA2(I) * BETA2(I)
      ENDDO
C
      GAMMA = 1.0D0 / DSQRT (1.0D0 - B_SQR)
C
C     Run the JPL approximation for delta delay due to a curved
C     wavefront
C
      RDIST = RSOURCE
C
C     Load the geocentric frame station and source vectors.
C
      DO I = 1, 3
         R2_GEO(I) = BSLN(I,1)
         RC_GEO(I) = -SRC(I) * RDIST
      ENDDO
C
c      write (6,4002) "R2_GEO = ", R2_GEO(1),R2_GEO(2),R2_GEO(3)
c      write (6,4002) "RC_GEO = ", RC_GEO(1),RC_GEO(2),RC_GEO(3)
c      write (6,4004) "RDIST  = ", RDIST
C
      BETADOTSTN = 0.0D0
      BETADOTRC  = 0.0D0
      BETADOTBETA2 = 0.0D0
      DO I = 1, 3
         BETADOTBETA2 = BETADOTBETA2 + BETA(I) * BETA2(I)
         BETADOTSTN = BETADOTSTN + BETA(I) * R2_GEO(I)
         BETADOTRC  = BETADOTRC  + BETA(I) * RC_GEO(I)
      ENDDO
C
C     Transform the station and source vectors into SSB frame.
C     Eqn. 3.158 in above reference.
C
      BDOTR_SSB = 0.0D0
      RDIST_SSB = 0.0D0
      DO I = 1, 3
         R2_SSB(I) = R2_GEO(I)
     +             + (GAMMA - 1.0D0) * BETADOTSTN * BETA(I) / B_SQR
     +             - GAMMA * BETADOTSTN * BETA2(I)
         RC_SSB(I) = RC_GEO(I)
     +             + (GAMMA - 1.0D0) * BETADOTRC * BETA(I) / B_SQR
     +             - GAMMA * BETADOTRC * BETA2(I)
         RDIST_SSB = RDIST_SSB + RC_SSB(I) * RC_SSB(I)
         BDOTR_SSB = BDOTR_SSB + BETA(I) * R2_SSB(I)
      ENDDO
      RDIST_SSB = DSQRT(RDIST_SSB)
C
c      write (6,4002) "R2_SSB = ", R2_SSB(1),R2_SSB(2),R2_SSB(3)
c      write (6,4002) "RC_SSB = ", RC_SSB(1),RC_SSB(2),RC_SSB(3)
c      write (6,4004) "RDIST_SSB = ", RDIST_SSB 
C
C     Evaluate eqn 3.11 in Rev. Mod. Phys. Vol. 70, No. 4,
C     October 1998. Sovers, Fanselow and Jacobs
C     Eqn. 3.11 is supposed to be accurate to 1 ps at a
C     distance of the lunar orbit...
C
C     Use the Calc delay solution as a starting point
C
      DLYCRV = -CALC_DELAY * 1.0D-6
C      
      RCDOTEPS = 0.0
      RCDOTB2  = 0.0
      EPS_SQR  = 0.0
      DO I = 1, 3
         EPS_CURVE(I) = -(R2_SSB(I) / CLIGHT + DLYCRV * BETA2(I))
     +                  / (RDIST_SSB / CLIGHT)
         RCDOTEPS = RCDOTEPS - RC_GEO(I) * EPS_CURVE(I) / RDIST_SSB
         RCDOTB2  = RCDOTB2  - RC_GEO(I) * BETA2(I)
         EPS_SQR  = EPS_SQR  + EPS_CURVE(I) * EPS_CURVE(I) 
      ENDDO
C
      DEL_CURVE = EPS_SQR 
     +          - (RCDOTEPS*RCDOTEPS)
     +          - (RCDOTEPS*RCDOTEPS*RCDOTEPS)
     +          + (RCDOTEPS*EPS_SQR)                 
C
      DELCRV = ((RDIST_SSB / CLIGHT) * DEL_CURVE)
     +       / 2.0 * (1.0 - RCDOTB2 / RDIST_SSB)
C
C        write(6,*) "del_c (sec) = ", DELCRV
c      write(6,4003) "uncorrected dly    = ", CALC_DELAY*1.0D-6
      DELAY = CALC_DELAY + DELCRV*1.0E6
c      write(6,4003) "corrected dly 3.11 = ", DELAY*1.0D-6
C
C     Use eqn. 3.13 of the above reference for more 
C     accuracy. However it won't work because it calculates
C     a total delay rather than a correction to the Calc plane
C     wavefront delay. The Calc delay contains non-geometric 
C     components, like atm delay. Eqn 3.13 doesn't....
C
      ITER_SOL = 0
      IF (ITER_SOL .EQ. 1) THEN
C
C     Use the Calc delay solution as a starting point
C
         DLYCRV = -CALC_DELAY * 1.0D-6
C         DLYCRV = 0.0
         DO I = 1, 5
            DO J = 1, 3
               EPS2(J) = -(R2_SSB(J) + DLYCRV * BETA2(J) * CLIGHT)
     +                   / RDIST
               RC_PLUS_EPS2(J) = EPS2(J) + RC_SSB(J) / RDIST_SSB
C
            ENDDO
C
C     Calculate the geocentric delay of the curved wavefront.
C     Still in SSB frame.
C         
            DLYCRV = (DSQRT (RC_PLUS_EPS2(1) * RC_PLUS_EPS2(1)
     +                    +  RC_PLUS_EPS2(2) * RC_PLUS_EPS2(2)
     +                    +  RC_PLUS_EPS2(3) * RC_PLUS_EPS2(3))
     +                    - 1.0)
            DLYCRV = DLYCRV * RDIST_SSB / CLIGHT
C       
         ENDDO
C
C     Transform the delay from SSB frame back to geocentric frame.
C
         DELAY = DLYCRV
         DELAY = GAMMA * (1.0D0 - BETADOTBETA2) * DELAY
     +         - GAMMA * BDOTR_SSB / CLIGHT
C
C
c         write(6,4003) "corrected dly 3.13 = ", DELAY

      END IF
C
C----------------------------------------------------------------------------
C
C     Correct the gravitaional bending delay for a spacecraft
C     or asteroid within the solar system.
C
C     Turn the planetary corrections off if the spacecraft is 
C     interior to the planet. Recalculate the solar bending 
C
C     RC_GEO vector to the spacecraft from the geocenter
C     RS vector from gravitational mass to the spacecraft
C     R1, R2 vectors from grav. mass to stn 1 and 2
C     RS1, RS2 vectors from stn 1 and 2 to spacecraft
C     Calculate vectors for eqn. 3.17
C

c      CALC_GRAVDLY = GRAVDLY

C         equation 5b: Vector from the Sun to receiver 1
c       vecmg1 = VECMG(R1Sunt1)
c       term_a = vecmg1 + DOTP(unit_K,R1Sunt1)
c       vecmg2 = VECMG(R2Sunt1)
c       term_b = vecmg2 + DOTP(unit_K,R2Sunt1)
C    Derivatives:
c       dterm_a = Dotp(R1Sunt1,dR1Sunt1)/vecmg1 + DOTP(unit_K,dR1Sunt1)
c       dterm_b = Dotp(R2Sunt1,dR2Sunt1)/vecmg2 + DOTP(unit_K,dR2Sunt1)
C
c       delta_t_grav_Sun = C_Sun * DLOG(term_a / term_b) 
c       d_delta_t_grav_Sun = C_Sun * ( dterm_a/term_a -
c     .                      dterm_b/term_b )                 !derivative
      DO I= 1, 3
         GEO_SC(I)   = RC_GEO(I)
      END DO

C
C    Evaluate eqn. 3.14
C
      DO I = 1, 3
         R1MPT1(I)   = R1SUNT1(I)
         R2MPT1(I)   = R2SUNT1(I)
      END DO
      CALL JPL314 (C_SUN, TERM4,
     +             GEO_SC, R1MPT1, R2MPT1, SUNGRAVDLY)
c      write (6,*) "JPL eqn. 3.14, Sun  = ", sungravdly

      DO I = 1, 3
         R1MPT1(I)   = R1MOONT1(I)
         R2MPT1(I)   = R2MOONT1(I)
      END DO
      CALL JPL314 (C_MOON, TERM4,
     +             GEO_SC, R1MPT1, R2MPT1, MOONGRAVDLY)
c      write (6,*) "JPL eqn. 3.14, Moon = ", moongravdly
c
      eargravdly = earthgrav
c      write (6,*) "Calc9.1 Earth grav   = ", earthgrav

      plangravdly = 0.0
      DO J = 1, 7
         C_TERM = C_PLAN(J)
         DO I = 1, 3
            R1MPT1(I)   = R1PLANT1(I,J)
            R2MPT1(I)   = R2PLANT1(I,J)
         END DO
         CALL JPL314 (C_TERM, TERM4,
     +                GEO_SC, R1MPT1, R2MPT1, GRAVDLY)
          plangravdly = plangravdly + gravdly
c         write (6,*) "JPL eqn. 3.14, Planet = ", gravdly
      END DO
     
c      write (6,*) "Calc sun      = ", SUN_CNTRB(1)
c      write (6,*) "delta_t_grav  = ", DELTGRAV
c      write (6,*) "term4         = ", term4

c      write (6,3001)"uncorrected total delay = ", delay
c
c     Convert delay to seconds and remove 1.0/term4
      DELAY = DELAY * TERM4 * 1.0D-06
c
c     Remove the spacecraft at infinity grav bending corrections for
c     the sun, earth, moon, planets. Leave in the high order solar 
c     bending correction. DELTGRAV is delta_t_grav in Calc 9.1
      DELAY = DELAY - DELTGRAV
c
c     Now add in the gravitation terms calculated here...
      DELAY = DELAY + SUNGRAVDLY + MOONGRAVDLY + EARGRAVDLY
      DELAY = DELAY + PLANGRAVDLY
c
      DELAY = DELAY * 1.0D+06 / TERM4
c      write (6,3001) "corrected total delay   = ", delay
 3001 format(1x,a,d20.14)
 3002 format(1x,a,f14.0, f14.0, f14.0)
 4002 format(1x,a,f18.2, f18.2, f18.2)
 4003 format(1x,a,f16.12)
 4004 format(1x,a,f18.2)
 900  CONTINUE
      RETURN
      END                                                                     
      SUBROUTINE JPL314 (C_TERM, TERM4,
     +                   GEO_SC, R1MPT1, R2MPT1, GRAVDLY)
      Implicit none
      Real*8   C_TERM, TERM4, GRAVDLY, VECMG
      Real*8   MAG_MP_SC, MAG_MP_STN1, MAG_MP_STN2  
      Real*8   MAG_STN1_SC, MAG_STN2_SC
      Real*8   GEO_SC(3), R1MPT1(3), R2MPT1(3)
      Real*8   MP_STN1(3), MP_STN2(3), MP_SC(3)
      Real*8   STN1_SC(3), STN2_SC(3)
      Real*8   GRAV_TERM1, GRAV_TERM2
      Integer*4 I

C
C    Evaluate eqn. 3.14
C
      DO I = 1, 3
         MP_STN1(I)  = R1MPT1(I)
         MP_STN2(I)  = R2MPT1(I)
         MP_SC(I)    = -GEO_SC(I) + R1MPT1(I)
         STN1_SC(I)  = -GEO_SC(I)
         STN2_SC(I)  = -MP_STN2(I) + MP_SC(I)   
      END DO

      MAG_STN1_SC  = VECMG(STN1_SC)
      MAG_STN2_SC  = VECMG(STN2_SC)
      MAG_MP_STN1  = VECMG(MP_STN1)
      MAG_MP_STN2  = VECMG(MP_STN2)
      MAG_MP_SC    = VECMG(MP_SC)

      IF (MAG_MP_STN1 .LT. 100.0) MAG_MP_STN1 = 100.0

      GRAV_TERM1 = (MAG_MP_SC + MAG_MP_STN2 + MAG_STN2_SC)
     +           / (MAG_MP_SC + MAG_MP_STN2 - MAG_STN2_SC)
      GRAV_TERM2 = (MAG_MP_SC + MAG_MP_STN1 + MAG_STN1_SC)
     +           / (MAG_MP_SC + MAG_MP_STN1 - MAG_STN1_SC)

      GRAVDLY = C_TERM * (DLOG(GRAV_TERM1) - DLOG(GRAV_TERM2)) / TERM4

      RETURN
      END
















