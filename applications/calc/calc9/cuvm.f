      SUBROUTINE UVA
      IMPLICIT None
C
C       UVA adds entries to the table of contents for the U, V 
C       coordinates Lcode. 
C
      INCLUDE 'ccon.i'
C        VARIABLES 'FROM'
C          1. KASTC - The Dave Shaffer switch to turn on the computation 
C                     of (U,V) coordinates. (Logic reversed 2001.01.12)
C                      = 0 ==> Switched ON. (default)
C                      = 1 ==> Switched OFF 
C
      INCLUDE 'cuser.i'
C        VARIABLES 'FROM'
C          1. Calc_user - 'A' for analysis centers, 'C' for correlators. 
C
C       DATA BASE ACCESS -
C         ACCESS CODES ADDED:
C          1. 'UVF/ASEC' -  The data base access code for the 
C                           U, V coordinates.
C          2. 'UVF/MHz ' -  The correlator code for the 
C                           U, V coordinates in fringes per arcsec,
C                           per MHz.
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDR 
C
C       PROGRAMMER:
C             David Gordon 1998.11.17 Subroutine created
C             David Gordon 2001.01.12 Logic reversed, default is to
C                          compute and add U/V coordinates, since the
C                          Mark IV correlators are not computing them. 
C                          New access code ('UVF/MHz ', U/V coordinates in 
C                          fringes per arcsec per MHz) added for Mark IV
C                          correlator usage.
C
C     UVA PROGRAM STRUCTURE
C
C   ADD for U,V coordinates 
      If (KASTC .eq. 0) Then
       If (Calc_user .eq. 'A') 
     *  CALL ADDR (2,'UVF/ASEC','U,V in FR per arcsec, from CALC ',
     *     2, 1, 1 )
       If (Calc_user .eq. 'C') 
     *  CALL ADDR (2,'UVF/MHz ','U,V in FR per arcsec per MHz    ',
     *     2, 1, 1 )
      Endif
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE UVG (STAR, EPBASE )
      IMPLICIT None
C
C       UVG computes the (U,V) coordinates of the baseline.  
C 
C      CALLING SEQUENCE -
C        INPUT VARIABLES:
C          1. STAR(3)     - The J2000.0 Source unit vector.
C          2. EPBASE(3,2) - The J2000.0 Geocentric baseline position and
C                           velocity vectors. (m, m/sec)
C
C   COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C        VARIABLES 'FROM':
C          1. KASTC - The Dave Shaffer switch to turn on the computation 
C                     of (U,V) coordinates. (Logic reversed 2001.01.12)
C                      = 0 ==> Switched ON. (default)
C                      = 1 ==> Switched OFF 
C
      INCLUDE 'cphys.i'
C        VARIABLES 'FROM':
C          1. VLIGHT - The velocity of light in a vacuum (m/sec).
C
      INCLUDE 'cuser.i'
C        VARIABLES 'FROM'
C          1. Calc_user - 'A' for analysis centers, 'C' for correlators. 
C
C       DATA BASE ACCESS -
C         ACCESS CODES ADDED:
C          1. 'UVF/ASEC' -  The data base access code for the 
C                           U, V coordinates in fringes per arcsec.
C          2. 'UVF/MHz ' -  The correlator code for the 
C                           U, V coordinates in fringes per arcsec,
C                           per MHz.
C 
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
C        VARIABLES 'FROM':
C          1. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                      (RAD/ARCSECOND)
C 
      Real*8 STAR(3), EPBASE(3,2) 
      Real*8 DOTP, REF_FREQ, B(3), NCP(3), vectr(3), Bpr(3), NCPpr(3),
     *       U_V(2), VECMG
      Integer*2 KERR, NDO(3)
C
C        Program variables:
C          1. U_V(2) -   Baseline coordinates in the (U,V) plane, in 
C                        units of fringes per arcsec. [Multiply by 
C                        206264.81 to get the more conventional values.]
C                        Scaled by the value of REF_FREQ.  
C          2. REF_FREQ - For databases, should be the correct reference 
C                        frequency. For correlator usage, set to 1.0 MHz,
C                        should be multiplied later by the correct 
C                        frequency or frequencies.  
C 
C       PROGRAMMER:
C             David Gordon 1998.11.17 Subroutine created
C             David Gordon 2001.01.12 Logic reversed, default is to
C                          compute and add U/V coordinates, since the
C                          Mark IV correlators are not computing them. 
C                          New access code ('UVF/MHz ', U/V coordinates in 
C                          fringes per arcsec per MHz) added for Mark IV
C                          correlator usage.
C
C       UVG PROGRAM STRUCTURE
C
      IF (KASTC .ne. 0) Go to 800
C
      If (Calc_user .eq. 'A') Then
C   Get the reference frequency. 
       CALL GET4('REF FREQ      ',REF_FREQ,1,1,1,NDO,KERR)
       IF(KERR.NE.0) then
         write(6,'("UVG: Failure to obtain ref frequency.")')
         CALL CKILL(6HUVG   ,1,KERR)
       Endif
C    Convert from MHz to Hz.
       REF_FREQ = REF_FREQ*1.D6
      Endif
C
C  Set frequency to 1 MHz for correlators.
      If (Calc_user .eq. 'C') Then
       REF_FREQ = 1.D6
      Endif
C
C   Baseline vector
       B(1) =  EPBASE(1,1) * REF_FREQ/VLIGHT*CONVDS 
       B(2) =  EPBASE(2,1) * REF_FREQ/VLIGHT*CONVDS 
       B(3) =  EPBASE(3,1) * REF_FREQ/VLIGHT*CONVDS 
C   NCP unit vector
       NCP(1) = 0.D0
       NCP(2) = 0.D0
       NCP(3) = 1.D0
C
C Get component of baseline vector projected into the plane perpendicular to 
C  the STAR vector: 
        CALL CROSP(STAR,      B, vectr)
        CALL CROSP(vectr,  STAR,   Bpr) 
C Get component of NCP vector projected into the plane perpendicular to 
C  the STAR vector: 
        CALL CROSP(STAR,   NCP, vectr)
        CALL CROSP(vectr, STAR, NCPpr)  
C Convert to a unit vector
        CALL VUNIT(NCPpr, NCP)
C
        U_V(2) = DOTP(Bpr, NCP)
        CALL CROSP(Bpr, NCP, vectr)
        U_V(1) = VECMG(vectr)
         If (DOTP(STAR,vectr) .lt. 0.d0) U_V(1) = -U_V(1)
C 
C  PUT the U,V coordinates in the database.
      If (Calc_user .eq. 'A') 
     *    CALL PUT4 ('UVF/ASEC      ', U_V, 2, 1, 1 )
      If (Calc_user .eq. 'C') 
     *    CALL PUT4 ('UVF/MHz       ', U_V, 2, 1, 1 )
C
C     Normal conclusion.
  800 CONTINUE
      RETURN
      END
