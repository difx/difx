      SUBROUTINE NFSTRG (XJD, UTC, Isource, EARTH, SITEP, SITEV, SUN,   &
     &           XMOON,R2K6, STAR, STARdt, STAR12, STAR12dt, T0_T1, R1, &
     &           R1dt, R1mag, R1magdt, R2, R2dt, R2mag, R2magdt,STARff, &
     &           R1_TDB, R2_TDB, R1mag_TDB, R2mag_TDB, Site2_TDB,       &
     &           Sourc20)
      IMPLICIT None
!
! 4.1.1 Near-field version of the STAR Module. 
!       Reference: M. Sekido and T. Fukushima, 'A VLBI delay model for
!                  radio sources at a finite distance', J. Geodesy, 
!                  vol. 80, pp. 137-149, 2006.
!
! 4.2   STRG PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!         INPUT:
!          1. Isource - Source # if in 'difx' mode.
!          2. R2K6(3,3,3) - THE COMPLETE CRUST FIXED TO J2000.0 ROTATION
!                           MATRIX AND ITS FIRST TWO CT TIME DERIVATIVES.
!                           CEO-based version. (UNITLESS, 1/SEC, 1/SEC**2)
!         OUTPUT VARIABLES:
!          1. STAR(3)  -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!
! 4.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cmxsr11.i'
!       VARIABLES 'FROM':
!          1. LNSTAR(10,MAX_ARC_SRC)- THE EIGHT ALPHANUMERIC CHARACTER NAMES
!                                     OF THE STARS IN THE STAR CATALOG.
!                                     Up to 20 characters long in difx mode.
!          2. NUMSTR                - THE NUMBER OF STARS IN THE STAR
!                                     CATALOG.
!          3. RADEC(2,MAX_ARC_SRC)  - THE RIGHT ASCENSIONS AND DECLINATIONS
!                                     OF THE STARS IN THE STAR CATALOG.
!                                     (RAD, RAD)
!       VARIABLES 'TO':
!          1. CD  - THE COSINE OF THE DECLINATION OF THE STAR BEING USED IN
!                   THE CURRENT OBSERVATION. (UNITLESS)
!          2. CRA - THE COSINE OF THE RIGHT ASCENSION OF THE STAR BEING
!                   USED IN THE CURRENT OBSERVATION. (UNITLESS)
!          3. SD  - THE SINE OF THE DECLINATION OF THE STAR BEING USED IN
!                   THE CURRENT OBSERVATION. (UNITLESS)
!          4. SRA - THE SINE OF THE RIGHT ASCENSION OF THE STAR BEING USED
!                   IN THE CURRENT OBSERVATION. (UNITLESS)
!
      INCLUDE 'ccon.i'
!       VARIABLES 'FROM':
!          1. KSTRC - THE STAR MODULE FLOW CONTROL FLAG.
!          2. KSTRD - THE STAR MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'cuser11.i'
!       Variables 'from':
!          1. C_mode     - Calc 11 mode. Either 'mark3 ', 'difx  ', or
!                          'nusolve'.
!          2. Near_field - Logical*4 flag for source type. 
!               ====>  Not currently used.
!                          = .False. for far-field sources (normal mode). 
!                          = .True. for near-field sources (spacecraft). 
!
      INCLUDE 'get2s.i'
!       Variables to:
!          1. LSTRNM(4) - THE EIGHT ALPHAMERIC CHARACTER STAR NAME FOR THE
!                         CURRENT OBSERVATION. (ALPHAMERIC)
!
      INCLUDE 'd_input.i'
!       Variables to:
!          1. 
!       Variables from:
!          1. NF_flag - Near field flag giving coordinate definition of 
!                       the near field object. Either 'GC' - geocentric,
!                       or 'BC' - barycentric or ?????.  
!               ====>  Not currently used. Assumed to be geocentric.
!          2. L_time  - Option to solve for light-travel time, if not
!                       built into the near-field object ephemeris.
!
      INCLUDE 'cphys11.i'
!           VARIABLES 'FROM':
!             1. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM.  (M/SEC)
!
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!         CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!
! 4.2.3 PROGRAM SPECIFICATIONS -
      Real*8  STAR(3), XJD, UTC, STAR12(3,2), EARTH(3,3), SITEP(3,2),   &
     &        SITEV(3,2), STARdt(3), STAR12dt(3,2), SUN(3,2),           &
     &        XMOON(3,2), R2K6(3,3,3)
      Real*8 VECMG, DOTP, R1(3), R2(3), R1dt(3), R2dt(3), R1mag, R2mag, &
     &       R1magdt, R2magdt, UTC0, T0_T1, DTgrav, R12(3), Div,        &
     &       STARff(3), STAR12dt0(3,2), R1dt0(3), R2dt0(3)
      Real*8 Sptimem1, Sptimep1, SPpm1(3), SPpp1(3), R1m1(3), R2m1(3),  &
     &       R1p1(3), R2p1(3), STAR12m1(3,2), STAR12p1(3,2),            &
     &       SITEPm1(3,2), SITEPp1(3,2)
!
! 4.2.4 DATA BASE ACCESS -
!
! 4.2.8 PROGRAM VARIABLES -
      REAL*8    RIGHT_ASC, DECLINATION, Xepoch, XJAN1, Xdays, Difyrs,   &
     &          JDepoch, JDY2K
      Real*8  Sptime, xMJD
!
      Real*8 Xp_spline, Yp_spline, Zp_spline, Xv_spline, Yv_spline,     &
     &       Zv_spline, Xpdot, Ypdot, Zpdot, Xvdot, Yvdot, Zvdot
!     Real*8 SPpxyz(3), SPvxyz(3), SPv2xyz(3), SPaxyz(3), yxdot2,yxdot3
      Real*8 yxdot2, yxdot3
      Real*8 ECEFp(3), ECEFv(3), ECEFa(3), SP1(3), SP2(3), SP3(3),      &
     &       SP4(3), SP5(3), SP6(3)
!
      Real*8 Site1_TDB(3), Site2_TDB(3), SPp_TDB(3), U_Sun, U, dU,      &
     &       vecmg1, F1, VdotR1, VdotR2, VdotNFO, R1_TDB(3), R2_TDB(3), &
     &       STAR12_TDB(3,2), R1mag_TDB, R2mag_TDB, STAR_TDB(3),        &
     &       VsubEarth(3), R_Earth_Sun(3), V_Earth_Sun(3)
!
      Real*8 tau, dP2(3), P2(3), V2(3), dV2(3)
      Real*8 Star_ab(3), VS1(3), VS2(3), VS3(3)
      Real*8 STARmag, uSTAR(3), K1(3), K2(3), STARssbc(3)
!
      Integer*4 N, NN, IM, ID, Ieph, I, J, Isource, Ierr4, I4, MJDx
!     Integer*2 NDO(3), KERR
!
      Character*20 SrcName(MAX_ARC_SRC), SName, Sourc20
      Equivalence (LNSTAR(1,1), SrcName(1))
      Equivalence (LSTRNM(1), SName)
!
!
! 4.2.9 PROGRAMMER - David Gordon Feb. 2013 
!
!     Construct the arrays which will hold the information for the
!     source being used in the current observation in order to pass
!     this information to the remainder of CALC.
!
!     Match the current source name against the names in the star catalog.
!     If no match, send a message and quit.
      IF (C_mode .eq. 'mark3 ') Then
  300  DO 310  NN = 1, NUMSTR
           N = NN
           IF  ( ( LNSTAR(1,N) .EQ. LSTRNM(1) ) &
     &     .AND. ( LNSTAR(2,N) .EQ. LSTRNM(2) ) &
     &     .AND. ( LNSTAR(3,N) .EQ. LSTRNM(3) ) &
     &     .AND. ( LNSTAR(4,N) .EQ. LSTRNM(4) ) )  GO TO 320
  310  CONTINUE
!       Write(6,*) 'NUMSTR= ', NUMSTR
!       Write(6,*) 'LSTRNM= ', LSTRNM(1), LSTRNM(2), LSTRNM(3), LSTRNM(4)
!       Write(6,*) 'LNSTAR= ', LNSTAR(1,1),LNSTAR(2,1),LNSTAR(3,1),LNSTAR(4,1)
       GO TO 600
      ENDIF
!
      IF (C_mode .eq. 'difx  ') Then
       N = Isource
       SName = SrcName(N)
       Sourc20 = SName
!       Write(6,*) ' Subroutine NFSTRG: '
!       Write(6,*) 'NUMSTR: ', NUMSTR
!       Write(6,'(" SName, SrcName: ",A20,2X,A20)') SName,SrcName(N)
!       Write(6,'(" STRNM= ",4A2)' ) LSTRNM
      ENDIF
!
  320 CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Do spline interpolations to get spacecraft position and velocity 
!   for the current epoch (at the time of transmission).
!       write (6,*) ' '
         MJDx = XJD - 2400000.5D0  + .00001    ! Integer 
         xMJD = MJDx                           ! Real
!       write (6,*) ' XJD,UTC,MJDx,xMJD  ', XJD,UTC,MJDx,xMJD 
        UTC0 = UTC
!           Correct for light travel time or not?
          I4 = 1
          If (L_time .eq. 'solve     ') I4 = 4 
      DO I = 1,I4       ! Iterate I4 times if solving light time equation
!  Convert UTC time to tabular table time
!       Sptime = ( (XJD+UTC0) - SpcIF(1) ) / SpcIF(2)
        Sptime = ( (xMJD + UTC0) - SpcIF(1) ) / SpcIF(2)
!       write (6,*) 'NFSTRG: '
!       write (6,*) ' UTC, xMJD+UTC0 ', UTC, (xMJD+UTC0)
!       write (6,*) ' SpcIF   ', SpcIF
!       write (6,*) ' I, UTC0    ', I, UTC0 
!       write (6,*) ' I, Sptime: ', I, Sptime
!       write (6,*) ' '
       Call Splynt4(Xc,ySPxp,y2SPxp,S_spline,Sptime,Xp_spline,Xpdot,    &
     &               yxdot2,yxdot3,ierr4)
       SPpxyz(1) = Xp_spline
       SPv2xyz(1) = Xpdot / (SpcIF(2) * 86400.D0) 
       Call Splynt4(Xc,ySPyp,y2SPyp,S_spline,Sptime,Yp_spline,Ypdot,    &
     &               yxdot2,yxdot3,ierr4)
        SPpxyz(2) = Yp_spline
        SPv2xyz(2) = Ypdot / (SpcIF(2) * 86400.D0) 
       Call Splynt4(Xc,ySPzp,y2SPzp,S_spline,Sptime,Zp_spline,Zpdot,    &
     &               yxdot2,yxdot3,ierr4)
        SPpxyz(3) = Zp_spline
        SPv2xyz(3) = Zpdot / (SpcIF(2) * 86400.D0) 
!
        If (I .eq. 4) Go to 77
!      Estimate T0, time when signal left near-field object
        CALL VECSB (SPpxyz, SITEP(1,1), R1) 
        T0_T1 = VECMG(R1)/VLIGHT 
!        write (6,*) 'I, T0_T1: ', I, T0_T1 
        UTC0 = UTC - (T0_T1/86400.D0) 
         Call DELTGRAV (T0_T1, EARTH, SITEP, SUN, XMOON, DTgrav)
        UTC0 = UTC0 - DTgrav/86400.D0
         T0_T1 = T0_T1 + DTgrav
!        write (6,*) 'I, T0_T1+DTgrav: ', I, T0_T1 
!        write (6,*) 'NFSTRG: I, T0_T1, DTgrav: ', I, T0_T1, DTgrav 
!        
 77     Continue
      ENDDO
!
!      write (6,*) 'UTC,UTC0,DTgrav', UTC*86400.D0,UTC0*86400.D0,DTgrav
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Near-field test
!   Multiply satelite positions by 10, 
!    Don't change velocities
!      write (6,*) ' SPACECRAFT POSITIONS TIMES 1.D6. ' 
!       SPpxyz(1) = SPpxyz(1) * 1.D6
!       SPpxyz(2) = SPpxyz(2) * 1.D6
!       SPpxyz(3) = SPpxyz(3) * 1.D6
!        T0_T1    =     T0_T1 * 1.D6    
!        write (6,*) '                 ' 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      Call Splynt4(Xc,ySPxv,y2SPxv,S_spline,Sptime,Xv_spline,Xvdot,     &
     &               yxdot2,yxdot3,ierr4)
       SPvxyz(1) = Xv_spline
       SPaxyz(1) = Xvdot / (SpcIF(2) * 86400.D0) 
      Call Splynt4(Xc,ySPyv,y2SPyv,S_spline,Sptime,Yv_spline,Yvdot,     &
     &               yxdot2,yxdot3,ierr4)
       SPvxyz(2) = Yv_spline
       SPaxyz(2) = Yvdot / (SpcIF(2) * 86400.D0) 
      Call Splynt4(Xc,ySPzv,y2SPzv,S_spline,Sptime,Zv_spline,Zvdot,     &
     &               yxdot2,yxdot3,ierr4)
!       write (6,*) ' Sptime: ', Sptime
       SPvxyz(3) = Zv_spline
       SPaxyz(3) = Zvdot / (SpcIF(2) * 86400.D0) 
!
!     Write (6,*) 'Xp_spline,Yp_spline,Zp_spline: ', Xp_spline,         &
!    &             Yp_spline, Zp_spline
!     Write (6,*) 'Xv_spline, Yv_spline, Zv_spline: ', Xv_spline,       &
!    &             Yv_spline, Zv_spline
!     Write (6,*) 'Xpdot, Ypdot, Zpdot: ', Xpdot, Ypdot, Zpdot 
!     Write (6,*) 'Xvdot, Yvdot, Zvdot: ', Xvdot, Yvdot, Zvdot 
!     Write (6,*) 'SPpxyz', SPpxyz
!     Write (6,*) 'SPvxyz: ', SPvxyz
!     Write (6,*) 'SPv2xyz: ', SPv2xyz
!     Write (6,*) 'SPaxyz: ', SPaxyz
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Near-field test
!   Multiply near-field positions by factors of 10, and set velocities
!    and accelerations to zero. 
!      write (6,*) ' Near-field positions times 1.D6. ' 
!       SPpxyz(1) = SPpxyz(1) * 1.D6
!       SPpxyz(2) = SPpxyz(2) * 1.D6
!       SPpxyz(3) = SPpxyz(3) * 1.D6
!        T0_T1    =     T0_T1 * 1.D6    
!       SPvxyz(1) = 0.D0
!       SPvxyz(2) = 0.D0
!       SPvxyz(3) = 0.D0
!       SPaxyz(1) = 0.D0
!       SPaxyz(2) = 0.D0
!       SPaxyz(3) = 0.D0
!        write (6,*) '                 ' 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      write(6,*) ' SPpxyz ', SPpxyz
!  If frame is geocentric Earth-fixed, convert to geocentric J2000
      If (SpFrame .eq. 'ECEF') Then
       Do I = 1,3
        ECEFp(I) = SPpxyz(I) 
        ECEFv(I) = SPvxyz(I) 
        ECEFa(I) = SPaxyz(I) 
       Enddo
       CALL VECRT ( R2K6(1,1,1), ECEFp, SPpxyz )
!
       CALL VECRT ( R2K6(1,1,2), ECEFp, SP1 )
       CALL VECRT ( R2K6(1,1,1), ECEFv, SP2 )
       CALL VECAD (SP1, SP2, SPvxyz)
!
       CALL VECRT ( R2K6(1,1,3), ECEFp, SP1 )
       CALL VECRT ( R2K6(1,1,2), ECEFv, SP2 )
       CALL VECRT ( R2K6(1,1,2), ECEFv, SP3 )
       CALL VECRT ( R2K6(1,1,1), ECEFa, SP4 )
       CALL VECAD (SP1, SP2, SP5)
       CALL VECAD (SP3, SP4, SP6)
       CALL VECAD (SP5, SP6, SPaxyz )
!
!      write(6,*) ' SpFrame: ', SpFrame
!      write(6,*) ' ECEFp  ', ECEFp 
!      write(6,*) ' SPpxyz ', SPpxyz
!      write(6,*) ' ECEFv  ', ECEFv 
!      write(6,*) ' SPvxyz ', SPvxyz
!      write(6,*) ' ECEFa  ', ECEFa 
!      write(6,*) ' SPaxyz ', SPaxyz
      Endif
!
!  If frame is SSBC J2000, convert to geocentric J2000
      If (SpFrame .eq. 'SSBC') Then
       CALL VECSB (SPpxyz, EARTH(1,1), SPpxyz)
       CALL VECSB (SPvxyz, EARTH(1,2), SPvxyz)
       CALL VECSB (SPaxyz, EARTH(1,3), SPaxyz)
      Endif
!
!  Spacecraft coordinates should now be geocentric J2000.
!
      CALL VECSB (SPpxyz, SITEP(1,1), R1) 
      CALL VUNIT (R1, STAR12(1,1))
      CALL VECSB (SPvxyz, SITEV(1,1), R1dt) 
       R1mag = VECMG(R1)
       R1magdt = (0.5D0/R1mag)*2.D0*DOTP(R1,R1dt)
       Do I = 1, 3
        STAR12dt(I,1) = R1dt(I)/R1mag - R1(I)*R1magdt/R1mag**2 
       Enddo
!  STAR12dt0( ) does NOT contain the effects of Earth rotation:
!     CALL VECEQ (SPvxyz, R1dt0)
!     CALL VECDV(R1dt0, R1mag, STAR12dt0(1,1))
!
      CALL VECSB (SPpxyz, SITEP(1,2), R2) 
      CALL VUNIT (R2, STAR12(1,2))
      CALL VECSB (SPvxyz, SITEV(1,2), R2dt) 
       R2mag = VECMG(R2)
       R2magdt = (0.5D0/R2mag)*2.D0*DOTP(R2,R2dt)
       Do I = 1, 3
        STAR12dt(I,2) = R2dt(I)/R2mag - R2(I)*R2magdt/R2mag**2 
       Enddo
!  STAR12dt0( ) does NOT contain the effects of Earth rotation:
!     CALL VECEQ (SPvxyz, R2dt0)
!     CALL VECDV(R2dt0, R2mag, STAR12dt0(1,2))
!
!  Compute pseudo source vector, STAR(3)
       CALL VECAD (R1, R2, R12)
        Div = R1mag + R2mag
       CALL VECDV (R12, Div, STAR)  
!
!!        write(6,*) 'R2T1: '
!!        write(6,*) 'R2 ',  R2
!!        write(6,*) 'R2mag ',  R2mag
!!        write(6,*) 'STAR12(1,2) ', STAR12(1,2),STAR12(2,2),STAR12(3,2)
!!        write(6,*) 'STAR ', STAR
!
      Do I = 1,3
       STARdt(I) = (R1dt(I)+R2dt(I))/Div - STAR(I)*(R1magdt+R2magdt)/Div
      Enddo
!!!!!! Test: use R2 at time T2
!?         tau = (R1mag - R2mag)/Vlight
!?         Call Vecmu(SITEV(1,2),tau,dP2)
!?         Call Vecsb(SITEP(1,2),dP2,P2)
!?         Call Vecsb(SPpxyz, P2, R2)
!?          R2mag = VECMG(R2)
!!        write(6,*) 'R2T2: '
!!        write(6,*) ' tau ', tau
!!        write(6,*) ' dP2 ', dP2
!!        write(6,*) '  P2 ',  P2
!!        write(6,*) '  R2 ',  R2
!!        write(6,*) '  R2mag ',  R2mag
!!        write(6,*) '   '
!
!?         tau = (R1mag - R2mag)/Vlight
!?         Call Vecmu(SITEV(1,2),tau,dP2)
!?         Call Vecsb(SITEP(1,2),dP2,P2)
!?         Call Vecsb(SPpxyz, P2, R2)
!?          R2mag = VECMG(R2)
!!        write(6,*) ' tau ', tau
!!        write(6,*) ' dP2 ', dP2
!!        write(6,*) '  P2 ',  P2
!!        write(6,*) '  R2 ',  R2
!!        write(6,*) '  R2mag ',  R2mag
!?       CALL VUNIT (R2, STAR12(1,2))
!!        write(6,*) 'STAR12(1,2) ', STAR12(1,2),STAR12(2,2),STAR12(3,2)
!!!      Call Vecmu(SITEA(1,2),tau,dV2)
!!!      Call Vecsb(SITEV(1,2),dV2,V2)
!      CALL VECSB (SPvxyz, V2, R2dt) 
!?       R2mag = VECMG(R2)
!?       R2magdt = (0.5D0/R2mag)*2.D0*DOTP(R2,R2dt)
!?       Do I = 1, 3
!?        STAR12dt(I,2) = R2dt(I)/R2mag - R2(I)*R2magdt/R2mag**2 
!?       Enddo
!  Compute pseudo source vector, STAR(3)
!?       CALL VECAD (R1, R2, R12)
!?        Div = R1mag + R2mag
!?       CALL VECDV (R12, Div, STAR)  
!         write(6,*) 'STAR ', STAR
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Convert spacecraft and site vectors from TT to TDB  
       Do I = 1, 3
        VsubEarth(I) = EARTH(I,2)      !  SSBC Earth Velocity
        R_Earth_Sun(I)  = -SUN(I,1)    !  Vector from Sun to Earth
        V_Earth_Sun(I)  = -SUN(I,2)    !  Velocity of Earth w.r.t. the Sun
       Enddo
! 
!       Sun's potential:
       U_Sun  = GMSUN/VLIGHT2
       vecmg1 = VECMG(R_Earth_Sun)
       U      = U_Sun/vecmg1
!!!      write(6,*) 'GMSUN,VLIGHT2,R_Earth_Sun ',  GMSUN,VLIGHT2,R_Earth_Sun 
!   Derivative:
!??    dU = -U_Sun * Dotp(R_Earth_Sun,V_Earth_Sun) / vecmg1**3
       F1 = 1.D0 - U - 1.48082686741D-8 
!        write(6,*) 'vecmg1, U, F1 ', vecmg1, U, F1 
!
        VdotR1 = DOTP(VsubEarth,SITEP(1,1))
        VdotR2 = DOTP(VsubEarth,SITEP(1,2))
        VdotNFO = DOTP(VsubEarth,SPpxyz)     
! 
        Call Vecad(VsubEarth,SITEV(1,1),VS1)
        Call Vecad(VsubEarth,SITEV(1,2),VS2)
        Call Vecad(VsubEarth,SPvxyz    ,VS3)
!       VdotR1 = DOTP(VS1      ,SITEP(1,1))
!       VdotR2 = DOTP(VS2      ,SITEP(1,2))
!       VdotNFO = DOTP(VS3      ,SPpxyz)     
!
!   TDB vectors:
       Do I = 1,3
!       Site1_TDB(I) = SITEP(I,1)*F1 - 0.5D0*(VdotR1/VLIGHT2)*(VsubEarth(I)+SITEV(I,1))
!       Site2_TDB(I) = SITEP(I,2)*F1 - 0.5D0*(VdotR2/VLIGHT2)*(VsubEarth(I)+SITEV(I,2))
!       SPp_TDB(I) = SPpxyz(I)*F1 - 0.5D0*(VdotNFO/VLIGHT2)*(VsubEarth(I)+SPvxyz(I))
        Site1_TDB(I) = SITEP(I,1)*F1 - 0.5D0*(VdotR1/VLIGHT2)*(VS1(I))
        Site2_TDB(I) = SITEP(I,2)*F1 - 0.5D0*(VdotR2/VLIGHT2)*(VS2(I))
        SPp_TDB(I) = SPpxyz(I)*F1 - 0.5D0*(VdotNFO/VLIGHT2)*(VS3(I))
       Enddo
!      Do I = 1,3
!       R1_TDB(I) = R1(I)*F1 - 0.5D0*((DOTP(VsubEarth,R1))*VsubEarth(I)/VLIGHT2)
!       R2_TDB(I) = R2(I)*F1 - 0.5D0*((DOTP(VsubEarth,R2))*VsubEarth(I)/VLIGHT2)
!      Enddo
!
      CALL VECSB (SPp_TDB, Site1_TDB, R1_TDB) 
      CALL VUNIT (R1_TDB, STAR12_TDB(1,1))
!??   CALL VECSB (SPvxyz, SITEV(1,1), R1dt) 
       R1mag_TDB = VECMG(R1_TDB)
!??    R1magdt = (0.5D0/R1mag)*2.D0*DOTP(R1,R1dt)
!??    Do I = 1, 3
!??     STAR12dt(I,1) = R1dt(I)/R1mag - R1(I)*R1magdt/R1mag**2 
!??    Enddo
!  STAR12dt0( ) does NOT contain the effects of Earth rotation:
!     CALL VECEQ (SPvxyz, R1dt0)
!     CALL VECDV(R1dt0, R1mag, STAR12dt0(1,1))
!
      CALL VECSB (SPp_TDB, Site2_TDB, R2_TDB) 
      CALL VUNIT (R2_TDB, STAR12_TDB(1,2))
!??   CALL VECSB (SPvxyz, SITEV(1,2), R2dt) 
       R2mag_TDB = VECMG(R2_TDB)
!??    R2magdt = (0.5D0/R2mag)*2.D0*DOTP(R2,R2dt)
!??    Do I = 1, 3
!??     STAR12dt(I,2) = R2dt(I)/R2mag - R2(I)*R2magdt/R2mag**2 
!??    Enddo
!  STAR12dt0( ) does NOT contain the effects of Earth rotation:
!     CALL VECEQ (SPvxyz, R2dt0)
!     CALL VECDV(R2dt0, R2mag, STAR12dt0(1,2))
!
!  Compute pseudo source vector, STAR(3)
       CALL VECAD (R1_TDB, R2_TDB, R12)
        Div = R1mag_TDB + R2mag_TDB
       CALL VECDV (R12, Div, STAR_TDB)  
       CALL VECDV (R12, Div, STAR    )      ! Now corrected to TDB!
!       write(6,*) 'R1,   R2   ', R1, R2
!       write(6,*) 'R1TDB,R2TDB', R1_TDB, R2_TDB
!       write(6,*) 'STAR,STAR_TDB', STAR, STAR_TDB
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  For Sekido model, compute apparent source pseudo vector at SSBC
!    due to abberation.
!       STARmag = VECMG(STAR)
!       Call VUNIT(STAR, uSTAR)
!       Do I = 1,3 
!        K1(I) = uSTAR(I) + (-1.D0)*VsubEarth(I)/VLIGHT -          &
!    &    (-1.D0)*uSTAR(I)*(DOTP(uSTAR,VsubEarth))/VLIGHT 
!       Enddo
!       Call VUNIT(K1,K2)
!       Call VECMU(K2,STARmag, STARssbc)
!       write(6,*) 'STAEssbc ', STAEssbc
!       Do I = 1,3   ! Abberation Test
!        STAR(I) = Starssbc(I)
!       Enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!??   Do I = 1,3
!??    STARdt(I) = (R1dt(I)+R2dt(I))/Div - STAR(I)*(R1magdt+R2magdt)/Div
!??   Enddo
!!!!
!!!   Write (6,*) 'SITEP1: ', SITEP(1,1),SITEP(2,1),SITEP(3,1),Site1_TDB
!!!   Write (6,*) 'SITEP2: ', SITEP(1,2),SITEP(2,2),SITEP(3,2),Site2_TDB
!!!   Write (6,*) 'SPp:    ', SPpxyz, SPp_TDB 
!!!   Write (6,*) 'R1:     ', R1, R1_TDB
!!!   Write (6,*) 'R1mag:  ', R1mag, R1mag_TDB
!!!   Write (6,*) 'R2:     ', R2, R2_TDB
!!!   Write (6,*) 'R2mag:  ', R2mag, R2mag_TDB
!!!   Write (6,*) 'STAR12(1): ', STAR12(1,1), STAR12(2,1), STAR12(3,1), &
!!!  &            STAR12_TDB(1,1), STAR12_TDB(2,1), STAR12_TDB(3,1)
!!!   Write (6,*) 'STAR12(2): ', STAR12(1,2), STAR12(2,2), STAR12(3,2), &
!!!  &            STAR12_TDB(1,2), STAR12_TDB(2,2), STAR12_TDB(3,2)
!!!    Write (6,*) 'STAR   ', STAR, STAR_TDB
!!!    Write (6,*) 'STAR-ratio', STAR(1)/STAR_TDB(1),STAR(2)/STAR_TDB(2), &
!!!  &                           STAR(3)/STAR_TDB(3)
!!!    Write (6,*) '       '                 
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      Write (6,*) '          '
!      Write (6,*) 'SPpxyz:   ', SPpxyz
!      Write (6,*) 'SITEP(1): ', SITEP(1,1), SITEP(2,1), SITEP(3,1)
!      Write (6,*) '          '
!      Write (6,*) 'SPvxyz:   ', SPvxyz
!      Write (6,*) 'SPv2xyz:  ', SPv2xyz
!      Write (6,*) 'SITEV(1): ', SITEV(1,1), SITEV(2,1), SITEV(3,1)
!      Write (6,*) '          '
!      Write (6,*) 'SPaxyz:   ', SPaxyz
!      Write (6,*) 'SITEA(1): ', SITEA(1,1), SITEA(2,1), SITEA(3,1)
!      Write (6,*) '          '
!      Write (6,*) 'R1:       ', R1      
!      Write (6,*) 'R1dt:     ', R1dt      
!      Write (6,*) 'R1mag:    ', R1mag
!      Write (6,*) 'R1magdt:  ', R1magdt
!      Write (6,*) '          '
!      Write (6,*) 'STAR12(1):', STAR12(1,1), STAR12(2,1), STAR12(3,1)
!      Write (6,*) 'STAR12dt: ', STAR12dt(1,1), STAR12dt(2,1),          &
!    &              STAR12dt(3,1)
!      Write (6,*) 'STAR12dt0: ', STAR12dt0(1,1), STAR12dt0(2,1),       &
!    &              STAR12dt0(3,1)
!      Write (6,*) '          '
!      Write (6,*) 'SPpxyz:   ', SPpxyz
!      Write (6,*) 'SITEP(2): ', SITEP(1,2), SITEP(2,2), SITEP(3,2)
!      Write (6,*) '          '
!      Write (6,*) 'SPvxyz:   ', SPvxyz
!      Write (6,*) 'SITEV(2): ', SITEV(1,2), SITEV(2,2), SITEV(3,2)
!      Write (6,*) '          '
!      Write (6,*) 'R2:       ', R2
!      Write (6,*) 'R2dt:     ', R2dt
!      Write (6,*) 'R2mag:    ', R2mag
!      Write (6,*) 'R2magdt:  ', R2magdt
!      Write (6,*) '          '
!      Write (6,*) 'STAR12(2):', STAR12(1,2), STAR12(2,2), STAR12(3,2)
!      Write (6,*) 'STAR12dt:  ', STAR12dt(1,2), STAR12dt(2,2),         &
!    &              STAR12dt(3,2)
!      Write (6,*) 'STAR12dt0: ', STAR12dt0(1,2), STAR12dt0(2,2),       &
!    &              STAR12dt0(3,2)
!      Write (6,*) '          '
!      Write (6,*) 'R12       ', R12     
!      Write (6,*) 'Div       ', Div     
!      Write (6,*) 'STAR      ', STAR    
!      Write (6,*) 'STARdt    ', STARdt  
!      Write (6,*) '          '
!
!***********************************************************************
!   Compute a pseudo RA and Declination
      Call VUNIT (STAR, STARff)
       RIGHT_ASC = DATAN2(STARff(2),STARff(1))
       DECLINATION = DASIN (STARff(3))
!
        RADEC(1,Isource) = RIGHT_ASC 
        RADEC(2,Isource) = DECLINATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!*    SD = DSIN ( DECLINATION)
!*    CD = DCOS ( DECLINATION)
!*    SRA = DSIN ( RIGHT_ASC )
!*    CRA = DCOS ( RIGHT_ASC )
!    Compute the equivalent Far-field source position unit vector.
!*    STARff(1) = CD * CRA
!*    STARff(2) = CD * SRA
!*    STARff(3) = SD
!       Write (6,*) '          '
!       Write (6,*) 'STARff    ', STARff  
!       Write (6,*) 'Far-Field RA,DEC: ', RIGHT_ASC, DECLINATION
!       Write (6,*) '          '
!
!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     WRITE ( 6, 9207 )  STAR, LSTRNM
!9207 FORMAT (1X, "STAR   = ", 3 ( D30.16, 10X ), /, 1X, &
!    &            "LSTRNM = ", 4A2, / )
!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Compute Star unit vectors at -1 and +1 seconds
!*      Sptimem1 = Sptime - 1.D0/(86400.D0*SpcIF(2))
!*      Sptimep1 = Sptime + 1.D0/(86400.D0*SpcIF(2))
!*      Write(6,*) 'Sptime, -1, +1', Sptime, Sptimem1, Sptimep1
!
!*     Call Splynt4(Xc,ySPxp,y2SPxp,S_spline,Sptimem1,Xp_spline,Xpdot,  &
!*   &               yxdot2,yxdot3,ierr4)
!*      SPpm1(1) = Xp_spline
!*     Call Splynt4(Xc,ySPyp,y2SPyp,S_spline,Sptimem1,Yp_spline,Ypdot,  &
!*   &               yxdot2,yxdot3,ierr4)
!*      SPpm1(2) = Yp_spline
!*     Call Splynt4(Xc,ySPzp,y2SPzp,S_spline,Sptimem1,Zp_spline,Zpdot,  &
!*   &               yxdot2,yxdot3,ierr4)
!*      SPpm1(3) = Zp_spline
!*     Write (6,*) 'SPpm1: ', SPpm1 
!
!*     Call Splynt4(Xc,ySPxp,y2SPxp,S_spline,Sptimep1,Xp_spline,Xpdot,  &
!*   &               yxdot2,yxdot3,ierr4)
!*      SPpp1(1) = Xp_spline
!*     Call Splynt4(Xc,ySPyp,y2SPyp,S_spline,Sptimep1,Yp_spline,Ypdot,  &
!*   &               yxdot2,yxdot3,ierr4)
!*      SPpp1(2) = Yp_spline
!*     Call Splynt4(Xc,ySPzp,y2SPzp,S_spline,Sptimep1,Zp_spline,Zpdot,  &
!*   &               yxdot2,yxdot3,ierr4)
!*      SPpp1(3) = Zp_spline
!*     Write (6,*) 'SPpp1: ', SPpp1 
!
!*      CALL VECSB(SITEP(1,1),SITEV(1,1), SITEPm1(1,1))
!*    CALL VECSB (SPpm1, SITEPm1(1,1), R1m1)
!*    CALL VUNIT (R1m1, STAR12m1(1,1))
!*      CALL VECSB(SITEP(1,2),SITEV(1,2), SITEPm1(1,2))
!*    CALL VECSB (SPpm1, SITEPm1(1,2), R2m1)
!*    CALL VUNIT (R2m1, STAR12m1(1,2))
!*     Write (6,*) 'STAR12m1(1): ', STAR12m1(1,1),STAR12m1(2,1),STAR12m1(3,1)
!*     Write (6,*) 'STAR12m1(2): ', STAR12m1(1,2),STAR12m1(2,2),STAR12m1(3,2)
!
!*      CALL VECAD(SITEP(1,1),SITEV(1,1), SITEPp1(1,1))
!*    CALL VECSB (SPpp1, SITEPp1(1,1), R1p1)
!*    CALL VUNIT (R1p1, STAR12p1(1,1))
!*      CALL VECAD(SITEP(1,2),SITEV(1,2), SITEPp1(1,2))
!*    CALL VECSB (SPpp1, SITEPp1(1,2), R2p1)
!*    CALL VUNIT (R2p1, STAR12p1(1,2))
!*     Write (6,*) 'STAR12p1(1): ', STAR12p1(1,1),STAR12p1(2,1),STAR12p1(3,1)
!*     Write (6,*) 'STAR12p1(2): ', STAR12p1(1,2),STAR12p1(2,2),STAR12p1(3,2)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     Check KSTRD for debug output.
      IF ( KSTRD .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine STRG." )
!
      WRITE(6,8)' CD      ',CD
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CRA     ',CRA
      WRITE(6,8)' RADEC   ',RADEC
      WRITE(6,7)' NUMSTR  ',NUMSTR
    7 FORMAT(A,15I8/(9X,15I8))
      WRITE(6,8)' SD      ',SD
      WRITE(6,8)' SRA     ',SRA
!
      WRITE ( 6, 9200 )  STAR, LSTRNM, LNSTAR
 9200 FORMAT (1X, "STAR   = ", 3 ( D30.16, 10X ), /, 1X, &
     &            "LSTRNM = ", 4A2, /, 1X, &
     &            "LNSTAR = ", 10 ( 10 ( 4A2, 2X ), /, 1X ) )
!
!   5.    NORMAL PROGRAM CONCLUSION.
!
  500 RETURN
!
!   6.    ABNORMAL PROGRAM TERMINATION.
!
  600 WRITE ( 6, 9300 )
 9300 FORMAT (" CALC has terminated in subroutine STRG.  ", &
     &        ' The source identification was not successful. ' )
!
      CALL TERMINATE_CALC ( 'STRG  ', int2(0), int2(0))
      END
!**********************************************************************
      SUBROUTINE NFSTRP (EPBASE,STAR,EARTH,SITEV, CDX,CRAX,SDX,SRAX)
      IMPLICIT None
!
! 5.1.1 STRP is the partial derivatives section of the STAR module. It computes
!       the partial derivatives of the delay and rate with respect to the source
!       declination and right ascension.
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
!**           5. DSTRP(2,2)-THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
!**                     RATE WITH RESPECT TO THE SOURCE RIGHT ASCENSION AND
!**                     DECLINATION. (sec/rad, sec/sec-rad) THE FIRST INDEX
!**                     RUNS OVER RA AND DEC, THE SECOND RUNS OVER DELAY AND
!**                     DELAY RATE.
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cphys11.i'
!           VARIABLES 'FROM':
!             1. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM.  (M/SEC)
!             2. VLIGHT2 - THE VELOCITY OF LIGHT IN VACUUM SQUARED.
!                          (M**2/SEC**2)
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
!         1. DSTRP(2,2) - THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
!                         RATE WITH RESPECT TO THE SOURCE RIGHT ASCENSION AND
!                         DECLINATION. (sec/rad, sec/sec-rad) THE FIRST INDEX
!                         RUNS OVER RA AND DEC, THE SECOND RUNS OVER DELAY AND
!                         DELAY RATE.
!
!
      Real*8 PR_RA, PR_DEC
      Common /Pmotn/ PR_RA, PR_DEC
!
! 5.2.3 PROGRAM SPECIFICATIONS -
!
       Real*8 DDEC(3), DRA(3),             EPBASE(3,2), STAR(3), CDX,   &
     &        CRAX, SDX, SRAX, EARTH(3,3), SITEV(3,2), c1, c2, tt,      &
     &        vg(3), bp(3), bv(3),            DOTP
!*   &        vg(3), bp(3), bv(3), PMCONT(2), DOTP
       Integer*4 I
!
! 5.2.4 DATA BASE ACCESS -
!**         'PUT' VARIABLES:
!**           1. DSTRP(2,2) - THE PARTIAL DERIVATIVES OF THE DELAY AND OF THE
!**                           DELAY RATE WITH RESPECT TO THE SOURCE RIGHT
!**                           ASCENSION AND DECLINATION. (sec/rad, sec/sec-rad)
!**                           THE FIRST INDEX RUNS OVER RIGHT ASCENSION AND
!**                           DECLINATION, THE SECOND INDEX RUNS OVER THE DELAY
!**                           AND THE DELAY RATE.
!**         ACCESS CODES:
!**           1. 'STR PART' - THE DATA BASE ACCESS CODE FOR THE STAR MODULE
!**                           PARTIAL DERIVATIVES ARRAY.
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVP
!             CALLED SUBROUTINES: PUT4
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
!  Compute the partial derivatives of the J2000.0 source unit vector with
!   respect to the source declination and with respect to the source R.A.
      DDEC(1) = - SD * CRA
      DDEC(2) = - SD * SRA
      DDEC(3) = + CD
!
      DRA(1) = - CD * SRA
      DRA(2) = + CD * CRA
      DRA(3) = 0.D0
!******************************************************************************
!   Complete the calculation of the partial derivatives.
!     c1 = 1.d0/VLIGHT
!     c2 = c1**2
!     Do i=1,3
!       vg(i) =  EARTH(I,2)
!       bp(i) = -EPBASE(I,1)
!       bv(i) = -EPBASE(I,2)
!     Enddo
!     tt = 1.d0 - c1*Dotp(STAR,vg)
!
!     DSTRP(1,1)=-c1*Dotp(bp,DRA )*tt+c2*Dotp(STAR,bp)*Dotp(vg,DRA)
!     DSTRP(1,2)=-c1*Dotp(bv,DRA )*tt+c2*Dotp(STAR,bv)*Dotp(vg,DRA)
!     DSTRP(2,1)=-c1*Dotp(bp,DDEC)*tt+c2*Dotp(STAR,bp)*Dotp(vg,DDEC)
!     DSTRP(2,2)=-c1*Dotp(bv,DDEC)*tt+c2*Dotp(STAR,bv)*Dotp(vg,DDEC)
!     WRITE(6,'(" Old DSTRP: ",4D22.14)') DSTRP
!******************************************************************************
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
      DSTRP(1,1) = -Dotp(bp,DRA )/(Vlight*tt) +                         &
     &              Dotp(STAR,bp)*Dotp(vg,DRA)/Vlight2
      DSTRP(1,2) = -Dotp(bv,DRA )/(Vlight*tt) +                         &
     &              Dotp(STAR,bv)*Dotp(vg,DRA)/Vlight2
      DSTRP(2,1) = -Dotp(bp,DDEC)/(Vlight*tt) +                         &
     &              Dotp(STAR,bp)*Dotp(vg,DDEC)/Vlight2
      DSTRP(2,2) = -Dotp(bv,DDEC)/(Vlight*tt) +                         &
     &              Dotp(STAR,bv)*Dotp(vg,DDEC)/Vlight2
!     WRITE(6,'(" New DSTRP: ",4D22.14)') DSTRP
!
!   PUT the partials into the database.
!**   CALL PUT4 ('STR PART      ', DSTRP, int2(2), int2(2), int2(1))
!
!   Copy some values from STRCM into dummy variables for use elsewhere
      CDX = CD
      CRAX = CRA
      SDX = SD
      SRAX = SRA
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
!**   SUBROUTINE STRC (DSTRP)
      SUBROUTINE NFSTRC ( )
      IMPLICIT None
!
! 5.1.1 STRC is the contributions section of the STAR module. It computes
!       contributions to the delay and rate due to proper motions. Used
!       only when KSTRC = 1 or 2.
!
! 5.2.1 CALLING SEQUENCE -
!**     INPUT VARIABLES:
!**       1. DSTRP(2,2)-THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
!**                     RATE WITH RESPECT TO THE SOURCE RIGHT ASCENSION AND
!**                     DECLINATION. (sec/rad, sec/sec-rad) THE FIRST INDEX
!**                     RUNS OVER RA AND DEC, THE SECOND RUNS OVER DELAY AND
!**                     DELAY RATE.
!
! 5.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1. KSTRC - THE STAR MODULE FLOW CONTROL FLAG.
!             2. KSTRD - THE STAR MODULE DEBUG OUTPUT FLAG.
!
      INCLUDE 'put2s.i'
!       Variables from:
!         1. DSTRP(2,2)-THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY
!                       RATE WITH RESPECT TO THE SOURCE RIGHT ASCENSION AND
!                       DECLINATION. (sec/rad, sec/sec-rad) THE FIRST INDEX
!                       RUNS OVER RA AND DEC, THE SECOND RUNS OVER DELAY AND
!                       DELAY RATE.
!       Variables to:
!         1. PMCONT(2) - If KSTRC = 1, these are the contributions to
!                        the delay and rate to correct for the effect
!                        of proper motion; add to theoreticals.
!                        If KSTRC = 2, these are the contributions to
!                        return the delay and rate to their non-proper
!                        motion values; add to theoreticals. (sec, sec/sec).
!
      Real*8 PR_RA, PR_DEC
      Common /Pmotn/ PR_RA, PR_DEC
!
! 5.2.3 PROGRAM SPECIFICATIONS -
!**    Real*8 DSTRP(2,2), PMCONT(2)
!
! 5.2.4 DATA BASE ACCESS -
!**        'PUT' VARIABLES:
!**          1. PMCONT(2) - If KSTRC = 1, these are the contributions to
!**                         the delay and rate to correct for the effect
!**                         of proper motion; add to theoreticals.
!**                         If KSTRC = 2, these are the contributions to
!**                         return the delay and rate to their non-proper
!**                         motion values; add to theoreticals. (sec, sec/sec).
!**        ACCESS CODES:
!**          1. 'PMOTNCON' - The data base access code for the proper motion
!**                          contribution.
!**          2. 'PMOT2CON' - The data base access code for the proper motion
!                            removal contribution.
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVP
!             CALLED SUBROUTINES: PUT4
!
! 5.2.9 PROGRAMMER - 98.09.15 D. Gordon - subroutine created
!                    Jim Ryan Sept 2002 Integer*2/4 mods.
!
!     STRC Program Structure
!
!  Check for proper motion computations
      IF (KSTRC.eq.1 .or. KSTRC.eq.2) THEN
        PMCONT(1) = PR_RA*DSTRP(1,1) + PR_DEC*DSTRP(2,1)
        PMCONT(2) = PR_RA*DSTRP(1,2) + PR_DEC*DSTRP(2,2)
!**     IF (KSTRC.eq.1) CALL PUT4 ('PMOTNCON      ', PMCONT, int2(2), &
!**  &      int2(1), int2(1))
!**     IF (KSTRC.eq.2) CALL PUT4 ('PMOT2CON      ', PMCONT, int2(2), &
!**  &      int2(1), int2(1))
      ELSE
       Return
      ENDIF
!
!** DEBUG *************************************************************
      WRITE(6,8)'STRP: DSTRP  ', DSTRP
      WRITE(6,8)'STRP: PMCONT ', PMCONT
!
!   Check KSTRD for debug output.
      IF (KSTRD .EQ. 0)  THEN
       WRITE ( 6, 9)
    9  FORMAT (1X, "Debug output for subroutine STRC." )
    8  FORMAT(A,4D25.16/(7X,5D25.16))
       WRITE(6,8)' DSTRP  ',DSTRP
       WRITE(6,8)'STRP: PMCONT ', PMCONT
      ENDIF
!
! NORMAL PROGRAM CONCLUSION.
!
  600 RETURN
      END
!
!**********************************************************************
!
      SUBROUTINE DELTGRAV ( T0_T1, EARTH, SITEP, SUN, XMOON, DTgrav)
      IMPLICIT NONE
!
      REAL*8 T0_T1, EARTH(3,3), SITEP(3,2), SUN(3,2), XMOON(3,2), DTgrav
      Real*8 dSun(3), SunT0(3), R0sun(3), dMoon(3), MoonT0(3),          &
     &       R0moon(3), dPlan(3), PlanT0(3), R0gp(3), DT(9), VECMG,     &
     &       DTsun, DTmoon
      Integer*4 I
!
!     Program Interface
!       Input variables:
!         1. T0_T1      - 
!         8. SITEP(3,2) - The J2000.0 geocentric position vectors of each
!                         observing site. (m)
!        10. SUN(3,2)   - The J2000.0 geocentric Sun position and velocity
!                         vectors. (m, m/sec)
!        11. XMOON(3,2) - The J2000.0 geocentric Moon position and velocity
!                         vectors. (m, m/sec)
!       Output variables:
!         1. DTgrav     - 

!
      INCLUDE 'd_input.i'
!       Variables from:
!          1. SPpxyz(3) - Geocentric satellite position (meters). 
!
!
       INCLUDE 'cphys11.i'
!          Variables 'from':
!            1. VLIGHT  - The velocity of light in vacuum.  (m/sec)
!            2. VLIGHT2 - The velocity of light squared. (m/sec)**2
!            3. VLIGHT3 - THE VELOCITY OF LIGHT IN VACUUM CUBED.
!                         (M**3/SEC**3)
!            4. GMSUN, GMMOON, GMEARTH, GMPLANET(7) - Gravitational constant
!                         times the masses of the Sun, Moon, Earth, and the
!                         other planets except Pluto.

!
      INCLUDE 'csolsys11.i'
!       Variables 'from':
!         2. GPLANET(3,2,7) - The J2000.0 Geocentric positions and velocities
!                             of all planets except the Earth and Pluto.
!                             (meters, meters/sec) The first index runs over
!                             X,Y, and Z, the second runs over position and
!                             velocity, and the third runs over the planets,
!                             where
!                                    1 = Mercury
!                                    2 = Venus
!                                    3 = Mars
!                                    4 = Jupiter
!                                    5 = Saturn
!                                    6 = Uranus
!                                    7 = Neptune
!
!!!!!
       CALL VECMU(SUN(1,2),T0_T1,dSun)
       CALL VECSB (SUN(1,1),dSun, SunT0  )
       CALL VECSB (SunT0   ,SPpxyz, R0sun)
       DTsun = 2.D0 * GMSUN/VLIGHT3 *                                   &
     & DLOG( (VECMG(SunT0) + VECMG(R0sun) + VECMG(SPpxyz    )) /        &
     &       (VECMG(SunT0) + VECMG(R0sun) - VECMG(SPpxyz     )))
!       Write(6,*) '   '
!       Write(6,*) 'DELTGRAV: '
!       Write(6,*) 'T0_T1 ', T0_T1
!       Write(6,*) 'GMSUN ', GMSUN 
!       Write(6,*) 'VLIGHT3 ', VLIGHT3 
!       Write(6,*) 'SUN   ', SUN
!       Write(6,*) 'dSun  ', dSun
!       Write(6,*) 'SunT0 ', SunT0
!       Write(6,*) 'R0sun ', R0sun
!       Write(6,*) 'DTsun(psec) ', DTsun * 1.D12
!
       CALL VECMU (XMOON(1,2),T0_T1, dMoon)
       CALL VECSB (XMOON(1,1),dMoon, MoonT0)
       CALL VECSB (MoonT0,SPpxyz, R0moon)
       DTmoon = 2.D0 * GMMOON/VLIGHT3 *                                 &
     & DLOG( (VECMG(MoonT0) + VECMG(R0moon) + VECMG(SPpxyz    )) /      &
     &       (VECMG(MoonT0) + VECMG(R0moon) - VECMG(SPpxyz     )))
!       Write(6,*) '   '
!       Write(6,*) 'GMMOON ', GMMOON
!       Write(6,*) 'XMOON  ', XMOON
!       Write(6,*) 'dMoon  ', dMoon
!       Write(6,*) 'MoonT0 ', MoonT0
!       Write(6,*) 'R0moon ', R0moon
!       Write(6,*) 'DTmoon(psec) ', DTmoon * 1.D12
!
      DO I = 1,7
        CALL VECMU (GPLANET(1,2,I),T0_T1, dPlan)
        CALL VECSB (GPLANET(1,1,I),dPlan, PlanT0)
        CALL VECSB (PlanT0,SPpxyz, R0gp)
        DT(I) = 2.D0 * GMPLANET(I)/VLIGHT3 *                            &
     &  DLOG( (VECMG(PlanT0) + VECMG(R0gp) + VECMG(SPpxyz    )) /       &
     &      (VECMG(PlanT0) + VECMG(R0gp) - VECMG(SPpxyz    )))
!       Write(6,*) '   '
!       Write(6,*) 'GMPLANET ', GMPLANET(I) 
!       Write(6,*) 'I,GPLANET  ', I, GPLANET(1,1,I),GPLANET(2,1,I),     &
!    &     GPLANET(3,1,I), GPLANET(1,2,I),GPLANET(2,2,I),GPLANET(3,2,I)
!       Write(6,*) 'dPlan  ', dPlan
!       Write(6,*) 'PlanT0 ', PlanT0
!       Write(6,*) 'R0gp   ', R0gp  
!       Write(6,*) 'DT(I)(psec) ', DT(I) * 1.D12 
!       Write(6,*) '   '
      ENDDO
!
      DTgrav = DTsun + DTmoon
      Do I = 1,7
       DTgrav = DTgrav + DT(I)
      Enddo
!       Write(6,*) '   '
!       Write(6,*) 'DTgrav(psec) ', DTgrav * 1.D12 
!       Write(6,*) '   '
!
      Return
      End
!
!**********************************************************************
!
      Subroutine NFSTewns( R1, R1dt, R1mag, R1magdt, R2, R2dt, R2mag,   &
     &           R2magdt, STAR, STARdt, STAR12, STAR12dt, SITEP, SITEV, &
     &           R1_TDB, R2_TDB, R1mag_TDB, R2mag_TDB, Site2_TDB,       &
     &            K_EWNS, STAR2         )
      IMPLICIT None
!
      INCLUDE 'd_input.i'
!       Variables from:
!          1. SPpxyz(3) - Geocentric satellite position (meters). 
!
      Real*8 SpEWNS(3,4), R1EWNS(3,4), R2EWNS(3,4), R1EWNSmag(4),       &
     &       R2EWNSmag(4), STAR12EWNS(3,4,2), STARewns(3,4)
      Common / NFewns/ SpEWNS, R1EWNS, R2EWNS, R1EWNSmag, R2EWNSmag,    &
     &       STAR12EWNS, STARewns
!
      Real*8 STAR(3), STAR12(3,2), STARdt(3), STAR12dt(3,2), R1(3),     &
     &       R2(3), R1dt(3), R2dt(3), R1mag, R2mag, R1magdt, R2magdt,   &
     &       SITEP(3,2), SITEV(3,2), R1_TDB(3), R2_TDB(3), R1mag_TDB,   &
     &       R2mag_TDB, Site2_TDB(3)             
      Real*8 Zee(3), West(3), South(3), UnitW(3), UnitS(3), W1(3),      &
     &       E1(3), StarW(3), StarE(3), S1(3), N1(3), StarS(3),         &
     &       StarN(3), StE(3), StW(3), StN(3), StS(3), R1E(3), R1W(3),  &
     &       R1N(3), R1S(3), div, VECMG, TAN0001
      Real*8 STAR2(3,4), K_EWNS(3,4)
      Real*8 R12(3), K12(3)
      Integer*4 I, K
!
!     write(6,*) '********* NFSTewns ********* '         
      Zee(1) = 0.D0
      Zee(2) = 0.D0
      Zee(3) = 1.D0
      TAN0001 = DTAN(.0001D0)
!
!
! R1 vector goes from the geocenter to the NFO, in J2000 frame.
! The geocenter is assumed to have coordinates (0,0,0).
! Take cross product of the R1 vector and the Z-unit vector. Result 
!   is a vector in the West direction in the plane tangent to the 
!   celestial sphere at the NFO as seen from the geocenter.
      Call Crosp(R1  ,Zee,West)
!       write(6,*) 'R1_TDB  :  ', R1_TDB   
!       write(6,*) 'West:  ', West
      Call VUNIT(West, UnitW)      ! make it a unit vector
!       write(6,*) 'UnitW: ', UnitW
! Compute a .0001 radian rotation of R1 in West and East directions
      Do I = 1,3
!      W1(I) = R1_TDB(I) + TAN0001 * UnitW(I) * R1mag_TDB
!      E1(I) = R1_TDB(I) - TAN0001 * UnitW(I) * R1mag_TDB
       W1(I) = R1    (I) + TAN0001 * UnitW(I) * R1mag    
       E1(I) = R1    (I) - TAN0001 * UnitW(I) * R1mag    
      Enddo
! Convert to unit vectors and then scale by R1mag
      Call VUNIT(W1, StarW)
      Call VUNIT(E1, StarE)
! Scale up to R1 magnitude
      Do I = 1,3
!      StarW(I) = StarW(I) * R1mag_TDB
!      StarE(I) = StarE(I) * R1mag_TDB
       StarW(I) = StarW(I) * R1mag
       StarE(I) = StarE(I) * R1mag
      Enddo
!       write(6,*) '        '
!       write(6,*) 'R2_TDB ', R2_TDB
!       write(6,*) 'SPpxyz ', SPpxyz
!       write(6,*) 'R1_TDB ', R1_TDB
!       write(6,*) 'R1     ', R1    
!       write(6,*) 'StarW: ', StarW
!       write(6,*) 'StarE: ', StarE
!
! Now do same in N-S direction
!     Call Crosp(R1_TDB,UnitW,South)
      Call Crosp(R1    ,UnitW,South)
      Call VUNIT(South,UnitS)
!       write(6,*) 'South: ', South
!       write(6,*) 'UnitS: ', UnitS
! Compute a .0001 radian rotation of R1 in North and South directions
      Do I = 1,3
!      S1(I) = R1_TDB(I) + TAN0001 * UnitS(I) * R1mag_TDB
!      N1(I) = R1_TDB(I) - TAN0001 * UnitS(I) * R1mag_TDB
       S1(I) = R1    (I) + TAN0001 * UnitS(I) * R1mag    
       N1(I) = R1    (I) - TAN0001 * UnitS(I) * R1mag    
      Enddo
! Convert to unit vectors
      Call VUNIT(S1, StarS)
      Call VUNIT(N1, StarN)
! Scale up to R1 magnitude
      Do I = 1,3
!      StarN(I) = StarN(I) * R1mag_TDB
!      StarS(I) = StarS(I) * R1mag_TDB
       StarN(I) = StarN(I) * R1mag
       StarS(I) = StarS(I) * R1mag
      Enddo
      Do I = 1,3     ! For Duev and ranging models
       SpEWNS(I,1) = StarE(I)
       SpEWNS(I,2) = StarW(I)
       SpEWNS(I,3) = StarN(I)
       SpEWNS(I,4) = StarS(I)
      Enddo
!       write(6,*) 'StarS: ', StarS
!       write(6,*) 'StarN: ', StarN
!
! Find R1, R2, Star12, and STAR for each of the 4 offsets
      Do K = 1, 4      ! Run over the 4 offset positions
        CALL VECSB (SpEWNS(1,K), SITEP(1,1), R1EWNS(1,K))
        CALL VUNIT (R1EWNS(1,K), STAR12EWNS(1,K,1))
        R1EWNSmag(K) = VECMG(R1EWNS(1,K))
!
        CALL VECSB (SpEWNS(1,K), SITEP(1,2), R2EWNS(1,K))
        CALL VUNIT (R2EWNS(1,K), STAR12EWNS(1,K,2))
        R2EWNSmag(K) = VECMG(R2EWNS(1,K))
! Pseudo star vectors
        CALL VECAD (R1EWNS(1,K), R2EWNS(1,K), R12)
         Div = R1EWNSmag(K) + R2EWNSmag(K)
        CALL VECDV (R12, Div, STARewns(1,K))
      Enddo
!       write(6,*) 'SPpxyz ', SPpxyz
!       write(6,*) 'SPeast ', SpEWNS(1,1), SpEWNS(2,1), SpEWNS(3,1) 
!       write(6,*) 'SPwest ', SpEWNS(1,2), SpEWNS(2,2), SpEWNS(3,2) 
!       write(6,*) 'SPnorth', SpEWNS(1,3), SpEWNS(2,3), SpEWNS(3,3) 
!       write(6,*) 'SPsouth', SpEWNS(1,4), SpEWNS(2,4), SpEWNS(3,4) 
!       write(6,*) '       '
!       write(6,*) 'SITEP-1', SITEP(1,1), SITEP(2,1), SITEP(3,1)
!       write(6,*) 'R1     ', R1    
!       write(6,*) 'R1east ', R1EWNS(1,1), R1EWNS(2,1), R1EWNS(3,1)
!       write(6,*) 'R1west ', R1EWNS(1,2), R1EWNS(2,2), R1EWNS(3,2)
!       write(6,*) 'R1south', R1EWNS(1,3), R1EWNS(2,3), R1EWNS(3,3)
!       write(6,*) 'R1north', R1EWNS(1,4), R1EWNS(2,4), R1EWNS(3,4)
!       write(6,*) '       '
!       write(6,*) 'SITEP-2', SITEP(1,2), SITEP(2,2), SITEP(3,2)
!       write(6,*) 'R2     ', R2    
!       write(6,*) 'R2east ', R2EWNS(1,1), R2EWNS(2,1), R2EWNS(3,1)
!       write(6,*) 'R2west ', R2EWNS(1,2), R2EWNS(2,2), R2EWNS(3,2)
!       write(6,*) 'R2south', R2EWNS(1,3), R2EWNS(2,3), R2EWNS(3,3)
!       write(6,*) 'R2north', R2EWNS(1,4), R2EWNS(2,4), R2EWNS(3,4)
!       write(6,*) '       '
!       write(6,*) 'STAR12(1)  ', STAR12(1,1), STAR12(2,1), STAR12(3,1)
!       write(6,*) 'STAR12(1)E ', STAR12EWNS(1,1,1), STAR12EWNS(2,1,1), STAR12EWNS(3,1,1)
!       write(6,*) 'STAR12(1)W ', STAR12EWNS(1,2,1), STAR12EWNS(2,2,1), STAR12EWNS(3,2,1)
!       write(6,*) 'STAR12(1)N ', STAR12EWNS(1,3,1), STAR12EWNS(2,3,1), STAR12EWNS(3,3,1)
!       write(6,*) 'STAR12(1)S ', STAR12EWNS(1,4,1), STAR12EWNS(2,4,1), STAR12EWNS(3,4,1)
!       write(6,*) '       '
!       write(6,*) 'STAR12(2)  ', STAR12(1,2), STAR12(2,2), STAR12(3,2)
!       write(6,*) 'STAR12(2)E ', STAR12EWNS(1,1,2), STAR12EWNS(2,1,2), STAR12EWNS(3,1,2)
!       write(6,*) 'STAR12(2)W ', STAR12EWNS(1,2,2), STAR12EWNS(2,2,2), STAR12EWNS(3,2,2)
!       write(6,*) 'STAR12(2)N ', STAR12EWNS(1,3,2), STAR12EWNS(2,3,2), STAR12EWNS(3,3,2)
!       write(6,*) 'STAR12(2)S ', STAR12EWNS(1,4,2), STAR12EWNS(2,4,2), STAR12EWNS(3,4,2)
!       write(6,*) '       '
!       write(6,*) 'STAR      ', STAR  
!       write(6,*) 'STAReast  ', STARewns(1,1), STARewns(2,1), STARewns(3,1)
!       write(6,*) 'STARwest  ', STARewns(1,2), STARewns(2,2), STARewns(3,2)
!       write(6,*) 'STARnorth ', STARewns(1,3), STARewns(2,3), STARewns(3,3)
!       write(6,*) 'STARsouth ', STARewns(1,4), STARewns(2,4), STARewns(3,4)
!       write(6,*) '       '
!
!   STAR2(3,4) = unit vectors from site 2 to the four offset positions (EWNS).
!   K_EWNS(3,4) = pseudo unit vectors from site 2 to the four offset
!      positions (EWNS). Same magnitude as STAR(3) (??). 
        CALL VECSB (StarE, SITEP(1,2), StE)  
        CALL VECSB (StarW, SITEP(1,2), StW)  
        CALL VECSB (StarN, SITEP(1,2), StN)  
        CALL VECSB (StarS, SITEP(1,2), StS)  
!       CALL VECSB (StarE, Site2_TDB,  StE)  
!       CALL VECSB (StarW, Site2_TDB,  StW)  
!       CALL VECSB (StarN, Site2_TDB,  StN)  
!       CALL VECSB (StarS, Site2_TDB,  StS)  
        Call Vunit(StE, STAR2(1,1))
        Call Vunit(StW, STAR2(1,2))
        Call Vunit(StN, STAR2(1,3))
        Call Vunit(StS, STAR2(1,4))
!        write(6,*) ' last week:  '
!        write(6,*) ' STAR12(n,2): ', STAR12(1,2), STAR12(2,2),STAR12(3,2)
!        write(6,*) ' STAR2(n,1):  ', STAR2(1,1), STAR2(2,1), STAR2(3,1)
!        write(6,*) ' STAR2(n,2):  ', STAR2(1,2), STAR2(2,2), STAR2(3,2)
!        write(6,*) ' STAR2(n,3):  ', STAR2(1,3), STAR2(2,3), STAR2(3,3)
!        write(6,*) ' STAR2(n,4):  ', STAR2(1,4), STAR2(2,4), STAR2(3,4)
!
!  Compute pseudo source vectors STARewns(3,4)
!!     CALL VECAD (R1_TDB, R2_TDB, R12)
!!      div = R1mag_TDB + VECMG(R12)
!!      div = R1mag_TDB + R2mag_TDB     
!!      CALL VECDV (R12, div, K12)
!
!      CALL VECAD (R1_TDB, StE, R1E)
!       div = R1mag_TDB + VECMG(StE)
!       CALL VECDV (R1E, div, K_EWNS(1,1)) 
!
!      CALL VECAD (R1_TDB, StW, R1W)
!       div = R1mag_TDB + VECMG(StW)
!       CALL VECDV (R1W, div, K_EWNS(1,2)) 
!
!      CALL VECAD (R1_TDB, StN, R1N)
!       div = R1mag_TDB + VECMG(StN)
!       CALL VECDV (R1N, div, K_EWNS(1,3)) 
!
!      CALL VECAD (R1_TDB, StS, R1S)
!       div = R1mag_TDB + VECMG(StS)
!       CALL VECDV (R1S, div, K_EWNS(1,4)) 
!        write(6,*) '             '
!!       write(6,*) 'K12     :   ', K12(1), K12(2), K12(3)
!        write(6,*) '      STAR: ', STAR(1), STAR(2), STAR(3)
!        write(6,*) 'K_EWNS(,1): ', K_EWNS(1,1), K_EWNS(2,1),K_EWNS(3,1)
!        write(6,*) 'K_EWNS(,2): ', K_EWNS(1,2), K_EWNS(2,2),K_EWNS(3,2)
!        write(6,*) 'K_EWNS(,3): ', K_EWNS(1,3), K_EWNS(2,3),K_EWNS(3,3)
!        write(6,*) 'K_EWNS(,4): ', K_EWNS(1,4), K_EWNS(2,4),K_EWNS(3,4)
!        write(6,*) '            ' 
!
!       STOP
!
      Return
      End
