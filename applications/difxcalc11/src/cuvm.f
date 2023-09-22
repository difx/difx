      SUBROUTINE UVG_un (STAR, EPBASE )
      IMPLICIT None
!
!       UVG computes the (U,V) coordinates of the baseline.
!       'UNCORRECTED' version.
!
!      CALLING SEQUENCE -
!        INPUT VARIABLES:
!          1. STAR(3)     - The J2000.0 Source unit vector.
!          2. EPBASE(3,2) - The J2000.0 Geocentric baseline position and
!                           velocity vectors. (m, m/sec)
!
!   COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!        VARIABLES 'FROM':
!          1. KASTC - The Dave Shaffer switch to turn on the computation
!                     of (U,V) coordinates. (Logic reversed 2001.01.12)
!                      = 0 ==> Switched ON. (default)
!                      = 1 ==> Switched OFF
!
      INCLUDE 'cphys11.i'
!        VARIABLES 'FROM':
!          1. VLIGHT - The velocity of light in a vacuum (m/sec).
!
      INCLUDE 'cuser11.i'
!        VARIABLES 'FROM'
!          1. Calc_user - 'A' for analysis centers, 'C' for correlators.
!
      INCLUDE 'get2s.i'
!       Variables from:
!          1. REF_FREQ - For databases, should be the correct reference
!                        frequency. For correlator usage, set to 1.0 MHz,
!                        should be multiplied later by the correct
!                        frequency or frequencies.
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. U_V(2) - Baseline coordinates in the (U,V) plane, in
!                      units of fringes per arcsec. [Multiply by
!                      206264.81 to get the more conventional values.]
!                      Scaled by the value of REF_FREQ. 
!
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!        VARIABLES 'FROM':
!          1. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                      (RAD/ARCSECOND)
!
      Real*8 STAR(3), EPBASE(3,2)
      Real*8 DOTP, B(3), NCP(3), vectr(3), Bpr(3), NCPpr(3), W,         &
     &       VECMG
!     Integer*2 KERR, NDO(3)
!
!        Program variables:
!          1. U_V(2) -   Baseline coordinates in the (U,V) plane, in
!                        units of fringes per arcsec. [Multiply by
!                        206264.81 to get the more conventional values.]
!                        Scaled by the value of REF_FREQ.
!          2. REF_FREQ - For databases, should be the correct reference
!                        frequency. For correlator usage, set to 1.0 MHz,
!                        should be multiplied later by the correct
!                        frequency or frequencies.
!
!       PROGRAMMER:
!             David Gordon 1998.11.17 Subroutine created
!             David Gordon 2001.01.12 Logic reversed, default is to
!                          compute and add U/V coordinates, since the
!                          Mark IV correlators are not computing them.
!                          New access code ('UVF/MHz ', U/V coordinates in
!                          fringes per arcsec per MHz) added for Mark IV
!                          correlator usage.
!             Jim Ryan Sept 2002 Integer*2/4 mods.
!             David Gordon Dec. 2012 Moved GET's to GET_G and PUT's to
!                          PUT_G.
!
!       UVG PROGRAM STRUCTURE
!
      IF (KASTC .ne. 0) Go to 800
!
!  Set frequency to 1 MHz for correlators.
      If (Calc_user .eq. 'C')  REF_FREQ = 1.D6
      If (C_mode .eq. 'difx  ') REF_FREQ = 1.D6
!
!   Baseline vector
       B(1) = EPBASE(1,1) * REF_FREQ/VLIGHT*CONVDS
       B(2) = EPBASE(2,1) * REF_FREQ/VLIGHT*CONVDS
       B(3) = EPBASE(3,1) * REF_FREQ/VLIGHT*CONVDS
!
      If (C_mode .eq. 'difx  ') Then
       B(1) = EPBASE(1,1) 
       B(2) = EPBASE(2,1)
       B(3) = EPBASE(3,1)
      Endif
!
!   NCP unit vector
       NCP(1) = 0.D0
       NCP(2) = 0.D0
       NCP(3) = 1.D0
!
! Get component of baseline vector projected into the plane
!   perpendicular to the STAR vector:
        CALL CROSP (STAR,      B, vectr)
        CALL CROSP (vectr,  STAR,   Bpr)
! Get component of NCP vector projected into the plane perpendicular
!   to the STAR vector:
        CALL CROSP (STAR,   NCP, vectr)
        CALL CROSP (vectr, STAR, NCPpr)
! Convert to a unit vector
        CALL VUNIT (NCPpr, NCP)
!
        U_V(2) = DOTP (Bpr, NCP)
        CALL CROSP (Bpr, NCP, vectr)
        U_V(1) = VECMG (vectr)
         If (DOTP (STAR,vectr) .lt. 0.d0) U_V(1) = -U_V(1)
! Add third component
        W = DOTP(B, STAR)
        Wb = W
!
!      Write (6,*) 'UVG:STAR: U,V,W  ', U_V, W
!
!     Normal conclusion.
  800 CONTINUE
      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE UVG_ab (STAR_ab, EPBASE )
      IMPLICIT None
!
!       UVG computes the (U,V) coordinates of the baseline, using
!        the aberrated source unit vector.
!       'APPROXIMATE' version:
!
!      CALLING SEQUENCE -
!        INPUT VARIABLES:
!          1. STAR_ab(3,2) - The J2000.0 aberrated source unit vector.
!          2. EPBASE(3,2)  - The J2000.0 Geocentric baseline position and
!                            velocity vectors. (m, m/sec)
!
!   COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!        VARIABLES 'FROM':
!          1. KASTC - The Dave Shaffer switch to turn on the computation
!                     of (U,V) coordinates. (Logic reversed 2001.01.12)
!                      = 0 ==> Switched ON. (default)
!                      = 1 ==> Switched OFF
!
      INCLUDE 'cphys11.i'
!        VARIABLES 'FROM':
!          1. VLIGHT - The velocity of light in a vacuum (m/sec).
!
      INCLUDE 'cuser11.i'
!        VARIABLES 'FROM'
!          1. Calc_user - 'A' for analysis centers, 'C' for correlators.
!
      INCLUDE 'get2s.i'
!       Variables from:
!          1. REF_FREQ - For databases, should be the correct reference
!                        frequency. For correlator usage, set to 1.0 MHz,
!                        should be multiplied later by the correct
!                        frequency or frequencies.
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. U_V(2) - Baseline coordinates in the (U,V) plane, in
!                      units of fringes per arcsec. [Multiply by
!                      206264.81 to get the more conventional values.]
!                      Scaled by the value of REF_FREQ. 
!
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
!        VARIABLES 'FROM':
!          1. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                      (RAD/ARCSECOND)
!
      Real*8 STAR_ab(3,2), EPBASE(3,2)
      Real*8 DOTP, B(3), NCP(3), vectr(3), Bpr(3), NCPpr(3), W,         &
     &       VECMG, Starxx(3), Starab(3)
!     Integer*2 KERR, NDO(3)
!
!        Program variables:
!          1. U_V(2) -   Baseline coordinates in the (U,V) plane, in
!                        units of fringes per arcsec. [Multiply by
!                        206264.81 to get the more conventional values.]
!                        Scaled by the value of REF_FREQ.
!          2. REF_FREQ - For databases, should be the correct reference
!                        frequency. For correlator usage, set to 1.0 MHz,
!                        should be multiplied later by the correct
!                        frequency or frequencies.
!
!       PROGRAMMER:
!             David Gordon 1998.11.17 Subroutine created
!             David Gordon 2001.01.12 Logic reversed, default is to
!                          compute and add U/V coordinates, since the
!                          Mark IV correlators are not computing them.
!                          New access code ('UVF/MHz ', U/V coordinates in
!                          fringes per arcsec per MHz) added for Mark IV
!                          correlator usage.
!             Jim Ryan Sept 2002 Integer*2/4 mods.
!             David Gordon Dec. 2012 Moved GET's to GET_G and PUT's to
!                          PUT_G.
!
!       UVG PROGRAM STRUCTURE
!
!     IF (KASTC .ne. 0) Go to 800
!
!  Set frequency to 1 MHz for correlators.
      If (Calc_user .eq. 'C') REF_FREQ = 1.D6
      If (C_mode .eq. 'difx  ') REF_FREQ = 1.D6
!
! Take average of the two abberrated star vectors and renormalize
      Call VECAD(Star_ab(1,1), Star_ab(1,2), Starxx)
      Call Vunit(Starxx,Starab)
!
!   Baseline vector
       B(1) = EPBASE(1,1) * REF_FREQ/VLIGHT*CONVDS
       B(2) = EPBASE(2,1) * REF_FREQ/VLIGHT*CONVDS
       B(3) = EPBASE(3,1) * REF_FREQ/VLIGHT*CONVDS
!
      If (C_mode .eq. 'difx  ') Then
       B(1) = EPBASE(1,1) 
       B(2) = EPBASE(2,1)
       B(3) = EPBASE(3,1)
      Endif
!
!   NCP unit vector
       NCP(1) = 0.D0
       NCP(2) = 0.D0
       NCP(3) = 1.D0
!
! Get component of baseline vector projected into the plane
!   perpendicular to the STAR vector:
        CALL CROSP (STARab,      B, vectr)
        CALL CROSP (vectr,  STARab,   Bpr)
! Get component of NCP vector projected into the plane perpendicular
!   to the STAR vector:
        CALL CROSP (STARab,   NCP, vectr)
        CALL CROSP (vectr, STARab, NCPpr)
! Convert to a unit vector
        CALL VUNIT (NCPpr, NCP)
!
        U_V(2) = DOTP (Bpr, NCP)
        CALL CROSP (Bpr, NCP, vectr)
        U_V(1) = VECMG (vectr)
         If (DOTP (STARab,vectr) .lt. 0.d0) U_V(1) = -U_V(1)
! Add third component
        W = DOTP(B, STARab)
        Wb = W
!
!      Write (6,*) 'UVG:STARab: U,V,W  ', U_V, W
!
!     Normal conclusion.
  800 CONTINUE
      RETURN
      END
!
!**********************************************************************
      SUBROUTINE UVG_plus (EPBASE,STAR,STAR_plus,EARTH,SITEV)
      IMPLICIT None
!
! 5.1.1 UVG_plus: Modified version STRP, the partial derivative the delay
!       w.r.t. the STAR coordinates. It computes the partial derivatives
!       of the delay and rate with respect to the source declination and 
!       right ascension and converts them to the U and V analytic 
!       baseline vectors, in meters. Input source vector can be either
!       the source unit vector, the aberrated source unit vector, or  the
!       aberrated/refracted source unit vector.
!       For 'EXACT' version - use STAR_plus = aberrated/refracted source.
!       For 'NO ATMOS' version - use STAR_plus = aberrated source.
!
! 5.2.1 CALLING SEQUENCE -
!           INPUT VARIABLES:
!             1. EPBASE(3,2) - THE J2000.0 GEOCENTRIC BASELINE VECTOR
!                              AND ITS CT TIME DERIVATIVE. (M, M/SEC)
!             2. STAR(3)     - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!             3. EARTH(3,3)  - The ssbc position, velocity, and acceleration
!                               of the Earth. (m, m/s, m/s**2)
!           OUTPUT VARIABLES:
!             1. CDX -  THE COSINE OF THE DECLINATION OF THE SOURCE
!             2. CRAX - THE COSINE OF THE RIGHT ASCENSION OF THE SOURCE
!             3. SDX -  THE SINE OF THE DECLINATION OF THE SOURCE
!             4. SRAX - THE SINE OF THE RIGHT ASCENSION OF THE SOURCE
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!           VARIABLES 'FROM':
!             1. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM.  (M/SEC)
!             2. VLIGHT2 - THE VELOCITY OF LIGHT IN VACUUM SQUARED.
!                          (M**2/SEC**2)
!
      INCLUDE 'cmxsr11.i'
!           VARIABLES 'FROM':
!             1. CD  - THE COSINE OF THE DECLINATION OF THE STAR BEING
!                      USED IN THE CURRENT OBSERVATION.
!             2. CRA - THE COSINE OF THE RIGHT ASCENSION OF THE STAR BEING
!                      USED IN THE CURRENT OBSERVATION.
!             3. SD  - THE SINE OF THE DECLINATION OF THE STAR BEING USED
!                      IN THE CURRENT OBSERVATION.
!             4. SRA - THE SINE OF THE RIGHT ASCENSION OF THE STAR BEING
!                      USED IN THE CURRENT OBSERVATION.
!
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1.  KSTRC - THE STAR MODULE FLOW CONTROL FLAG.
!             2.  KSTRD - THE STAR MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'put2s.i'
!       Variables to:
!          1. U_V(2) - Baseline coordinates in the (U,V) plane, in
!                      meters.
!
!
      Real*8 PR_RA, PR_DEC
      Common /Pmotn/ PR_RA, PR_DEC
!
! 5.2.3 PROGRAM SPECIFICATIONS -
!
       Real*8 DDEC(3), DRA(3),             EPBASE(3,2), STAR(3), CDX,   &
     &        CRAX, SDX, SRAX, EARTH(3,3), SITEV(3,2), c1, c2, tt,      &
     &        vg(3), bp(3), bv(3),  DOTP, XSTRP(2,2), STAR_plus(3)
       Integer*4 I
!
! 5.2.4 DATA BASE ACCESS - Moved to subroutine PUT_P ('STR PART').
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVP
!             CALLED SUBROUTINES: 
!
! 5.2.7 CONSTANTS USED - VLIGHT
!
! 5.2.8 PROGRAM VARIABLES -
!             1. DDEC(3) - THE PARTIAL DERIVATIVE OF THE J2000.0 SOURCE UNIT
!                          VECTOR WITH RESPECT TO SOURCE DECLINATION. (1/RAD)
!             2. DRA(3)  - THE PARTIAL DERIVATIVE OF THE J2000.0 SOURCE UNIT
!                          VECTOR WITH RESPECT TO THE SOURCE RIGHT ASCENSION.
!                          (1/RAD)
!             3. c1, c2, tt, vg(3), b(3) -  Dummy variables used in computation
!                          of the partials.
!
! 5.2.9 PROGRAMMER - 77.01.13 Dale Markham
!                    77.07.14 Peter Denatale
!                    88.11.10 Bruce Schupler
!                    88.01.07 Jim Ryan
!                    89.07.09 Jim Ryan Documentation simplied.
!                    89.10.05 Jim Ryan CPHYS common made an include file
!                    89.12.12 Jim Ryan UNIX-like database interface
!                             implimented.
!                    01:11:25 Jim Ryan Term 2 of Shapiro's model added
!                             to the partials.
!                    David Gordon 94.04.15 Converted to Implicit None
!                    David Gordon 95.05.02 DSTRP passed back to DRIVR for use
!                             in PLXP.
!                    B. Archinal  95.11.13 Max # sources set in cmxsr.i.
!                    David Gordon 98.09.08 Changed partials computation to
!                             use CONSENSUS model (Step 10B). Makes no
!                             significant difference.
!                    David Gordon 2000.05.15 Bug correction, variable c1
!                             redefined (had been commented out).
!                    Jim Ryan Sept 2002 Integer*2/4 mods.
!                    David Gordon Jan. 2013  PUT moved to subroutine PUT_P.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!                                             !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!           NOT FOR NEAR-FIELD USE            !!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!                                             !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     STRP Program Structure
!
!  Get cosines and sines of RA and declnation.
       SDX  = STAR_plus(3)       ! sine of declination
       CDX  = DCOS(DASIN(SDX))   ! cosine of declination
       SRAX = STAR_plus(2)/CDX   ! sine of RA
       CRAX = STAR_plus(1)/CDX   ! cosine of RA
!
!  Compute the partial derivatives of the J2000.0 source unit vector with
!   respect to the source declination and with respect to the source R.A.
      DDEC(1) = - SDX * CRAX
      DDEC(2) = - SDX * SRAX
      DDEC(3) = + CDX
!
      DRA(1) = - CDX * SRAX
      DRA(2) = + CDX * CRAX
      DRA(3) = 0.D0
!
!   Complete the calculation of the partial derivatives.
      Do I=1,3
        vg(I) =  EARTH(I,2) + SITEV(I,2)
        bp(I) = -EPBASE(I,1)
        bv(I) = -EPBASE(I,2)
      Enddo
      c1 = 1.d0/VLIGHT
      tt = 1.d0 + c1*Dotp(STAR,vg)
!   Changed to Consensus model formula
      XSTRP(1,1) = -Dotp(bp,DRA )/(Vlight*tt) +                         &
     &              Dotp(STAR,bp)*Dotp(vg,DRA)/Vlight2
      U_V(1) = XSTRP(1,1)*VLIGHT/CDX
!
!!    XSTRP(1,2) = -Dotp(bv,DRA )/(Vlight*tt) +                         &
!!   &              Dotp(STAR,bv)*Dotp(vg,DRA)/Vlight2
!
      XSTRP(2,1) = -Dotp(bp,DDEC)/(Vlight*tt) +                         &
     &              Dotp(STAR,bp)*Dotp(vg,DDEC)/Vlight2
      U_V(2) = XSTRP(2,1)*VLIGHT
!
!!    XSTRP(2,2) = -Dotp(bv,DDEC)/(Vlight*tt) +                         &
!!   &              Dotp(STAR,bv)*Dotp(vg,DDEC)/Vlight2
!     WRITE(6,'(" New DSTRP: ",4D22.14)') DSTRP
!
!     write(6,*) 'UVG_no: ', XSTRP(1,1), XSTRP(2,1)
!
!   Copy some values from STRCM into dummy variables for use elsewhere
!     CDX = CD
!     CRAX = CRA
!     SDX = SD
!     SRAX = SRA
!
!   Check KSTRD for debug output.
      IF ( KSTRD .EQ. 0 )  GO TO 600
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine STRP." )
      WRITE(6,8)' CD     ',CD
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CRA    ',CRA
      WRITE(6,8)' DDEC   ',DDEC
      WRITE(6,8)' DRA    ',DRA
      WRITE(6,8)' DSTRP  ',DSTRP
      WRITE(6,8)' SD     ',SD
      WRITE(6,8)' SRA    ',SRA
      WRITE(6,8)' VLIGHT ',VLIGHT
      WRITE(6,8)' c1     ',c1
      WRITE(6,8)' c2     ',c2
      WRITE(6,8)' tt     ',tt
      WRITE(6,8)' vg     ',vg
      WRITE(6,8)' bp     ',bp
      WRITE(6,8)' bv     ',bv
!
      WRITE ( 6, 9200 )  EPBASE, STAR,CDX,SDX,CRAX,SRAX
 9200 FORMAT (1X, "EPBASE = ", 2 ( 3 ( D30.16, 10X ), /, 1X ), &
     &            "STAR   = ", 3 ( D30.16, 10X ),/,1X, &
     &            "CDX    = ", D30.16,1X, &
     &            "SDX    = ", D30.16,1X, &
     &            "CRAX   = ", D30.16,/,1X, &
     &            "SRAX   = ", D30.16)
!
! NORMAL PROGRAM CONCLUSION.
!
  600 RETURN
      END
!***********************************************************************
