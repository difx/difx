      SUBROUTINE UVA
      IMPLICIT None
C
       INCLUDE 'ccon.i'
C      VARIABLES 'FROM'
C        1. KASTC - The Dave Shaffer switch to turn on the computation 
C                   of (U,V) coordinates.
C                    = 0 ==> Switched OFF (default). 
C                    = 1 ==> Switched ON. 
C
C       UVA adds entries to the table of contents for the U, V 
C       coordinates Lcode. Only done if KASTC = 1. 
C
C       DATA BASE ACCESS -
C           ACCESS CODES ADDED:
C              1.  'UVF/ASEC'  -  The data base access code for the 
C                                 U, V coordinates  ..........
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDR 
C
C                Programmer: 
C                    David Gordon 98.11.17
C
C     UVA PROGRAM STRUCTURE
C
C   ADD for U,V coordinates ???? 
      If (KASTC .eq. 1) 
     *  CALL ADDR (2,'UVF/ASEC','U,V in FR per arcsec, from CALC ',
     *     2, 2, 1 )
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE UVG (STAR, EPBASE )
      IMPLICIT None
C
C       UVG computes the (U,V) coordinates of the baseline, if KASTC=1.  
C 
C 3.2.1 CALLING SEQUENCE -
C          INPUT VARIABLES:
C            1. STAR(3)     - The J2000.0 Source unit vector.
C            2. EPBASE(3,2) - The J2000.0 Geocentric baseline position and
C                             velocity vectors. (m, m/sec)
C
C 3.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'               
C       VARIABLES 'FROM':
C        1. KASTC - The Dave Shaffer switch to turn on the computation 
C                   of (U,V) coordinates.
C                    = 0 ==> Switched OFF (default). 
C                    = 1 ==> Switched ON. 
C
      INCLUDE 'cphys.i'
C           VARIABLES 'FROM':
C              1. VLIGHT  -  The velocity of light in a vacuum (m/sec).
C
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      EXTERNAL CMATHB
C           VARIABLES 'FROM':
C              1. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                 (RAD/ARCSECOND)
C 
      Real*8 STAR(3), EPBASE(3,2) 
      Real*8 DOTP, REF_FREQ, B(3), NCP(3), vectr(3), Bpr(3), NCPpr(3),
     *       U_V(2), VECMG
      Integer*2 KERR, NDO(3)
C 
C       PROGRAMMER - David Gordon  98.11.17 
C
C       UVG PROGRAM STRUCTURE
C
      IF (KASTC .ne. 1) Go to 800
C
C   Get the reference frequency for use in the phase delay rate corrections. 
      CALL GET4('REF FREQ      ',REF_FREQ,1,1,1,NDO,KERR)
C      print *, ' REF_FREQ ', REF_FREQ
C     CALL GET4('UVF/ASEC      ',U_V     ,2,1,1,NDO,KERR)
C      print *, ' Database (U,V) ', U_V 
      IF(KERR.NE.0) then
        write(6,'("UVG: Failure to obtain ref frequency.")')
        CALL CKILL(6HUVG   ,1,KERR)
      Endif
C    Convert from MHz to Hz.
      REF_FREQ = REF_FREQ*1.D6
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
C     print *, ' Computed U_V   ',  U_V     
C  PUT the U,V coordinates in the database.
      CALL PUT4 ('UVF/ASEC      ', U_V, 2, 1, 1 )
C
C     Normal conclusion.
  800 CONTINUE
      RETURN
      END
