      SUBROUTINE STRG (XJD, UTC, Isource, STAR, STAR12, RIGHT_ASC,      &
     &                 DECLINATION, Sourc20)
      IMPLICIT None
!
! 4.1.1 STRG is the geometry section of the STAR Module. STRG computes the
!       J2000.0 unit vector in the direction of the source. Now will compute
!       proper motions in special situations.
!
! 4.2   STRG PROGRAM INTERFACE
!
! 4.2.1 CALLING SEQUENCE -
!         INPUT:
!          1. Isource - Source # if in 'difx' mode.
!         OUTPUT VARIABLES:
!          1. STAR(3) - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
!
! 4.2.2 COMMON BLOCKS USED -
!
      INCLUDE 'cmxsr11.i'
!       VARIABLES 'FROM':
!          1. LNSTAR(10,MAX_ARC_SRC)- THE ALPHANUMERIC CHARACTER NAMES
!                                     OF THE STARS IN THE STAR CATALOG.
!                                     Up to 20 characters in difx mode.
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
!          1. C_mode  - Calc 11 mode. Either 'mark3 ', 'difx  ', or
!                       'nusolve'.
!
      INCLUDE 'get2s.i'
!       Variables to:
!          1. LSTRNM(4) - THE EIGHT ALPHAMERIC CHARACTER STAR NAME FOR THE
!                         CURRENT OBSERVATION. (ALPHAMERIC)
!
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!         CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!
      Real*8 PR_RA, PR_DEC
      Common /Pmotn/ PR_RA, PR_DEC
!
      Real*8 Dparsec, t_prlx(2)
      Common /PRLX/ Dparsec, t_prlx
!
! 4.2.3 PROGRAM SPECIFICATIONS -
      Real*8  STAR(3), XJD, UTC, STAR12(3,2)
!
! 4.2.4 DATA BASE ACCESS - None. GET 'STAR ID ' moved to subroutine GET_G.
!
! 4.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: DCOS, DSIN, GETA, TERMINATE_CALC, JDY2K
!
! 4.2.7 CONSTANTS USED - NONE
!
! 4.2.8 PROGRAM VARIABLES -
      REAL*8    RIGHT_ASC, DECLINATION, Xepoch, XJAN1, Xdays, Difyrs,   &
     &          JDepoch, JDY2K
      Integer*4 N, NN, IM, ID, Ieph, I, J, Isource
      Integer*2 NDO(3), KERR
!
      Character*20 SrcName(MAX_ARC_SRC), SName, Sourc20
      Equivalence (LNSTAR(1,1), SrcName(1))
      Equivalence (LSTRNM(1), SName)
!
!       Local variables:
!             1. RIGHT_ASC   - LOCAL VARIABLE FOR HOLDING THE RA.
!             2. DECLINATION - LOCAL VARIABLE FOR HOLDING THE DEC.
!
! 4.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
!                    PETER DENATALE 07/14/77
!                    JIM RYAN      88.01.07
!                    Jim Ryan 89.07.09 Documentation simplied.
!                    Jim Ryan 89.12.12 UNIX-like database interface
!                          implimented.
!                    David Gordon 94.04.15 Converted to Implicit None
!                    B. Archinal  95.11.13 Max # sources set in cmxsr.i.
!                    David Gordon 98.09.08 Add /CMATH/ Common block.
!                    David Gordon 98.09.11 Add new /Pmotn/ and /PRLX/ common
!                          blocks to hold proper motion offsets and distance.
!                          Code added to compute and handle proper motions,
!                          if that option is turned on.
!                    David Gordon 98.11.25 Removed proper motion computations
!                          and moved them to the initialization section.
!                    Jim Ryan Sept 2002 Integer*2/4 mods.
!                    David Gordon 2012.12.20  Database GET's move to 
!                          subroutine GET_G.
!
!     STRG PROGRAM STRUCTURE
!
!   GET of 'STAR ID' moved to subroutine GET_G.
!
!     Construct the arrays which will hold the information for the
!     source being used in the current observation in order to pass
!     this information to the remainder of CALC.
!
!       Write(6,*) 'C_mode: ', C_mode
!     Match the current star name against the names in the star catalog.
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
!       Write(6,*) 'NUMSTR: ', NUMSTR
!       Write(6,'(" SName, SrcName: ",A20,2X,A20)') SName,SrcName(N)
!       Write(6,'(" STRNM= ",4A2)' ) LSTRNM
      ENDIF
!
!     Construct the arrays to hold the sine and cosine of the star
!     declination and right ascention.
  320 CONTINUE
      RIGHT_ASC   = RADEC(1,N)
      DECLINATION = RADEC(2,N)
!      WRITE(6,*) 'STRG: RIGHT_ASC,DECLINATION ', RIGHT_ASC,DECLINATION
!
!***********************************************************************
!  Check for proper motion computations
!     IF (KSTRC.eq.1 .or. KSTRC.eq.2) THEN
!  Compute proper motion offsets in RA and Dec
!   Proper motion epoch
!       Xepoch = P_motion(3,N)
!   Truncate to integer year
!       Ieph = Xepoch
!   Day of year
!       If (JMOD(Ieph,4) .eq. 0) Then
!        Xdays = (Xepoch - Ieph) / 366.D0
!       Else
!        Xdays = (Xepoch - Ieph) / 365.D0
!       Endif
!   Julian day an Jan. 1
!        IM = 1
!        ID = 1
!       XJAN1 = JDY2K(Ieph,IM,ID)
!   Julian day at proper motion epoch
!       JDepoch = XJAN1 - 1 + Xdays
!   Difference: (Observation time) - (Proper motion epoch), yrs
!       Difyrs = ((XJD+UTC) - JDepoch) / 365.25D0
!   Proper motion in RA (convert arc-seconds to time units in radians)
!       PR_RA = P_motion(1,N) / DCOS(DECLINATION) * CONVDS * Difyrs
!   Proper motion in Dec (convert arc-seconds to radians)
!       PR_DEC = P_motion(2,N) * CONVDS * Difyrs
!
!   Determine if proper motions should be added to the source vector. If so
!    the corrections should also be reversed for later use in STRP.
!       IF (KSTRC.eq.2) THEN
!         RIGHT_ASC   = RIGHT_ASC   + PR_RA
!         DECLINATION = DECLINATION + PR_DEC
!         PR_RA  = -PR_RA
!         PR_DEC = -PR_DEC
!       ENDIF
!      WRITE(6,8) ' Xepoch, Xdays, XJAN1, JDepoch, Difyrs ',
!    *              Xepoch, Xdays, XJAN1, JDepoch, Difyrs
!      WRITE(6,8) ' PR_RA, PR_DEC ', PR_RA, PR_DEC
!
!     ENDIF
!***********************************************************************
!
      SD = DSIN ( DECLINATION)
      CD = DCOS ( DECLINATION)
      SRA = DSIN ( RIGHT_ASC )
      CRA = DCOS ( RIGHT_ASC )
!
!     Compute the star position unit vector.
      STAR(1) = CD * CRA
      STAR(2) = CD * SRA
      STAR(3) = SD
!
!  Compute star unit vectors at sites 1 and 2. 
       Do J = 1, 2
        Do I = 1, 3
         STAR12(I,J) = STAR(I)
        Enddo
       Enddo
!
      IF (KSTRC.eq.1 .or. KSTRC.eq.2) THEN
        PR_RA  = PRcorr(1,N)
        PR_DEC = PRcorr(2,N)
      ENDIF
!
!  Match source with distance, if parallax to be computed
      IF (KPLXC .eq. 1) Then
        Dparsec = D_psec(N)
        WRITE (6,8) ' Dparsec ', Dparsec
      ENDIF
!
!
!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     WRITE ( 6, 9207 )  STAR, LSTRNM
!9207 FORMAT (1X, "STAR   = ", 3 ( D30.16, 10X ), /, 1X, &
!    &            "LSTRNM = ", 4A2, / )
!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
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
     &            "LNSTAR = ", 20 ( 5(10A2,2X), /, 1X ) )
!
      IF (KSTRC.eq.1 .or. KSTRC.eq.2) THEN
       WRITE(6,8) ' Xepoch, Xdays, XJAN1, JDepoch, Difyrs ', &
     &              Xepoch, Xdays, XJAN1, JDepoch, Difyrs
       WRITE(6,8) ' PR_RA, PR_DEC ', PR_RA, PR_DEC
      ENDIF
      IF (KPLXC .eq. 1) WRITE (6,8) ' Dparsec ', Dparsec
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
      SUBROUTINE STRP (EPBASE,STAR,EARTH,SITEV,      CDX,CRAX,SDX,SRAX)
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
      SUBROUTINE STRC ( )
      IMPLICIT None
!
! 5.1.1 STRC is the contributions section of the STAR module. It computes
!       contributions to the delay and rate due to proper motions. Used
!       only when KSTRC = 1 or 2.
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
!
! 5.2.4 DATA BASE ACCESS - Moved to subroutine PUT_C ('PMOTNCON', 'PMOT2CON').
!
! 5.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVP
!             CALLED SUBROUTINES: 
!
! 5.2.9 PROGRAMMER - 98.09.15 D. Gordon - subroutine created
!                    Jim Ryan Sept 2002 Integer*2/4 mods.
!                    David Gordon Jan. 2013  PUT's moved to subroutine PUT_C.
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
!***********************************************************************
      SUBROUTINE STAR_NSEW (STAR,K_EWNS, RIGHT_ASC, DECLINATION,        &
     &                      STAR12, dKew, dKns)
      IMPLICIT None
!
      Real*8 STAR(3), K_EWNS(3,4), RIGHT_ASC, DECLINATION, STAR12(3,2)
      Real*8 Zee(3), West(3), South(3), UnitW(3), UnitS(3), W1(3),      &
     &       E1(3), StarW(3), StarE(3), S1(3), N1(3), StarS(3),         &
     &       StarN(3)
      Real*8 delKew(3), delKns(3), dKew, dKns, VECMG
      Real*8 P1(3), P2(3), P3(3), W2(3), E2(3), S2(3), N2(3), sag
      Real*8 RAw, RAe, RAs, RAn, DECw, DECe, DECs, DECn, SD, CD, SRA,CRA
      Real*8 TAN0001
      Integer*4 I
!
!      write(6,*) '********* STAR_NSEW ********* '         
      Zee(1) = 0.D0
      Zee(2) = 0.D0
      Zee(3) = 1.D0
      TAN0001 = DTAN(.0001D0) 
!     TAN0001 = DTAN(.001D0) 
!      write(6,*) 'TAN0001: ', TAN0001
       sag = 1.D0 - DSQRT(1.D0 - TAN0001**2)
!
!
! Take cross product of the STAR vector and the Z-unit vector. Result 
!   is a vector in the West direction.
      Call Crosp(STAR,Zee,West)
!       write(6,*) 'STAR:  ', STAR 
!       write(6,*) 'West:  ', West
      Call VUNIT(West, UnitW)      ! make it a unit vector
!       write(6,*) 'UnitW: ', UnitW
! Compute a .0001 radian rotation of STAR in West and East directions
      Do I = 1,3
       W1(I) = STAR(I) + TAN0001 * UnitW(I)
       E1(I) = STAR(I) - TAN0001 * UnitW(I)
      Enddo
! Convert to unit vectors
      Call VUNIT(W1, StarW)
      Call VUNIT(E1, StarE)
!       write(6,*) 'Plan A: '
!       write(6,*) 'StarW: ', StarW
!       write(6,*) 'StarE: ', StarE
!
! Now do same in N-S direction
!
      Call Crosp(STAR,UnitW,South) 
      Call VUNIT(South,UnitS)
!       write(6,*) 'South: ', South
!       write(6,*) 'UnitS: ', UnitS
! Compute a .0001 radian rotation of STAR in North and South directions
      Do I = 1,3
       S1(I) = STAR(I) + TAN0001 * UnitS(I)
       N1(I) = STAR(I) - TAN0001 * UnitS(I)
      Enddo
! Convert to unit vectors
      Call VUNIT(S1, StarS)
      Call VUNIT(N1, StarN)
!       write(6,*) 'StarS: ', StarS
!       write(6,*) 'StarN: ', StarN
!---------------------------------------------------------------------
! Plan C:
!!     RAw  = RIGHT_ASC - .0001D0/DCOS(DECLINATION)
!!     DECw = DECLINATION
!!     RAe  = RIGHT_ASC + .0001D0/DCOS(DECLINATION)
!!     DECe = DECLINATION
!!
!!    SD = DSIN (DECw)
!!    CD = DCOS (DECw)
!!    SRA = DSIN (RAw)
!!    CRA = DCOS (RAw)
!!    StarW(1) = CD * CRA
!!    StarW(2) = CD * SRA
!!    StarW(3) = SD
!!
!!    SD = DSIN (DECe)
!!    CD = DCOS (DECe)
!!    SRA = DSIN (RAe)
!!    CRA = DCOS (RAe)
!!    StarE(1) = CD * CRA
!!    StarE(2) = CD * SRA
!!    StarE(3) = SD
!
!     RAs  = RIGHT_ASC 
!     DECs = DECLINATION - .0001D0
!     SD = DSIN (DECs)
!     CD = DCOS (DECs)
!     SRA = DSIN (RAs)
!     CRA = DCOS (RAs)
!     StarS(1) = CD * CRA
!     StarS(2) = CD * SRA
!     StarS(3) = SD
!
!     RAn  = RIGHT_ASC 
!     DECn = DECLINATION + .0001D0
!     SD = DSIN (DECn)
!     CD = DCOS (DECn)
!     SRA = DSIN (RAn)
!     CRA = DCOS (RAn)
!     StarN(1) = CD * CRA
!     StarN(2) = CD * SRA
!     StarN(3) = SD
!
!     Call Crosp(STAR,StarN,West ) 
!     Call VUNIT(West, UnitW)      ! make it a unit vector
! Compute a .0001 radian rotation of STAR in West and East directions
!     Do I = 1,3
!      W1(I) = STAR(I) + TAN0001 * UnitW(I)
!      E1(I) = STAR(I) - TAN0001 * UnitW(I)
!     Enddo
! Convert to unit vectors
!     Call VUNIT(W1, StarW)
!     Call VUNIT(E1, StarE)
!
!       write(6,*) 'Plan C: '
!       write(6,*) 'StarW: ', StarW
!       write(6,*) 'StarE: ', StarE
!       write(6,*) 'StarS: ', StarS
!       write(6,*) 'StarN: ', StarN
!---------------------------------------------------------------------
! Plan B:
!   Create vector perpendicular to the plane and pointing back to Earth.
!      Call VECMU(STAR,-sag,P3)
!       write(6,*) 'Plan B: '
!       write(6,*) 'STAR   ', STAR  
!       write(6,*) 'sag,P3 ', sag,P3
!
!      Call VECAD(W1,P3,W2)
!      Call VECAD(E1,P3,E2)
!      Call VECAD(S1,P3,S2)
!      Call VECAD(N1,P3,N2)
!      Call VUNIT(W2,StarW)
!      Call VUNIT(E2,StarE)
!      Call VUNIT(S2,StarS)
!      Call VUNIT(N2,StarN)
!       write(6,*) 'W2:    ', W2 
!       write(6,*) 'StarW: ', StarW
!       write(6,*) 'E2:    ', E2 
!       write(6,*) 'StarE: ', StarE
!       write(6,*) 'S2:    ', S2 
!       write(6,*) 'StarS: ', StarS
!       write(6,*) 'N2:    ', N2 
!       write(6,*) 'StarN: ', StarN
!---------------------------------------------------------------------
!
!  Load K_EWNS array 
      Do I = 1,3
       K_EWNS(I,1) = StarE(I)
       K_EWNS(I,2) = StarW(I)
       K_EWNS(I,3) = StarN(I)
       K_EWNS(I,4) = StarS(I)
        delKew(I) = StarE(I) - StarW(I)
        delKns(I) = StarN(I) - StarS(I)
      Enddo
! Find difference vectors:
       dKew = VECMG(delKew)
       dKns = VECMG(delKns)
!       write(6,*) 'dKew, dKns: ', dKew, dKns 
! One more test:
!       Do I = 1, 3
!        STAR(I) = StarS(I)
!        STAR12(I,1) = StarE(I)
!        STAR12(I,2) = StarE(I)
!       Enddo
!
      Return
      End
