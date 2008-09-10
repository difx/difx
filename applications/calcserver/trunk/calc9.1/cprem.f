      SUBROUTINE PREA
      IMPLICIT None
C 
C 1.    PREA
C 
C 1.1   PREA PROGRAM SPECIFICATION
C 
C 1.1.1 PREA ADDS ENTRIES TO THE TABLE OF CONTENTS FOR THE PRECESSION MODULE 
C       TEXT MESSAGE AND PARTIAL DERIVATIVES ARRAY. IT ALSO ADDS ENTRIES TO THE
C       TABLE OF CONTENTS FOR THE PRECESSION MODULE FLOW CONTROL MESSAGE.
C 
C 1.1.2 RESTRICTIONS - NONE 
C 
C 1.1.3 REFERENCES - NONE 
C 
C 1.2   PREA PROGRAM INTERFACE
C 
C 1.2.1 CALLING SEQUENCE - NONE 
C 
C 1.2.2 COMMON BLOCKS USED - NONE 
C 
C 1.2.3 PROGRAM SPECIFICATIONS - NONE 
C 
C 1.2.4 DATA BASE ACCESS:
C 
C           ACCESS CODES:
C             1. 'PRE MESS' - THE DATA BASE ACCESS CODE FOR THE PRECESSION
C                             MODULE TEXT MESSAGE. 
C             2. 'PRE PART' - THE DATA BASE ACCESS CODE FOR THE PRECESSION 
C                             MODULE PARTIAL DERIVATIVES ARRAY. 
C             3. 'PRE CFLG' - THE DATA BASE ACCESS CODE FOR THE PRECESSION 
C                             MODULE FLOW CONTROL MESSAGE. 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - None
C
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDA, ADDR
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - NONE
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 10/21/77
C                    SAVITA GOEL    06/03/87 (CDS FOR A900)
C                    Jim Ryan       89.07.07 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.14 Converted to Implicit None
C
C     PREA PROGRAM STRUCTURE
C
C   ADD for precession module text message.
      CALL ADDA (1,'PRE MESS','Precession message definition   ',
     .     40, 1, 1 )
C
C   ADDA_S for precession module flow control message.
      CALL ADDA (1,'PRE CFLG','Precession flow contril mess def',
     .     40,1,1)
C
C   ADD for precession module partial derivatives.
      CALL ADDR (2,'PRE PART','Precession partial deriv. def. ' ,
     .     2, 1, 1 )
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE PREI
      IMPLICIT None
C
C 3.    PREI
C
C 3.1   PREI PROGRAM SPECIFICATION
C
C 3.1.1 PREI IS THE PRECESSION MODULE INPUT AND INITIALIZATION SECTION.
C
C 3.1.2 RESTRICTIONS - NONE
C
C 3.1.3 REFERENCES - NONE
C
C 3.2   PREI PROGRAM INTERFACE
C
C 3.2.1 CALLING SEQUENCE - NONE
C
C 3.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'ccon.i'
C        VARIABLES 'FROM':
C          1. KPREC - THE PRECESSION MODULE FLOW CONTROL FLAG.
C          2. KPRED - THE PRECESSION MODULE DEBUG OUTPUT FLAG.
C
C
      Real*8 CENTJ, CTHETA(3), CZEE(3), CZETA(3), DJ2000, HEPOCH, 
     1    PRECON, RP1(3,3), RP2(3,3), RP3(3,3), THETA, ZEE, ZETA, CENTR
      COMMON / PRECM / CENTJ, CTHETA, CZEE, CZETA, DJ2000, HEPOCH, 
     1         PRECON, RP1, RP2, RP3, THETA, ZEE, ZETA, CENTR
C        VARIABLES 'TO':
C           1. PRECON - THE PRECESSION CONSTANT. (ARCSEC/JCENTURY)
C
C 3.2.3 PROGRAM SPECIFICATIONS -
C
      INTEGER*2  NDO(3), KERR
      INTEGER*2      LPREM(40),      LON(40),    LOFF(40)
      CHARACTER*40 C_LPREM(2) ,    C_LON(2) ,  C_LOFF(2)
      EQUIVALENCE (C_LPREM,LPREM),(C_LON,LON),(C_LOFF,LOFF)
C
      DATA C_LPREM /
     .'Precession Module - Version # 2, last mo',
     .'dification - 08/04/81, Chopo Ma         '/
C
      DATA C_LON  /
     .'Precession Module is turned on.         ',
     .'                                        '/
C
      DATA C_LOFF /
     .'Precession module is turned off.        ',
     .'                                        '/
C
C 3.2.4 DATA BASE ACCESS -
C
C           'GET' VARIABLES:
C             1. PRECON  -  THE PRECESSION CONSTANT.  (ARCSEC/JCENTURY)
C
C           'PUT' VARIABLES:
C             1. LPREM(40)  -  THE PRECESSION MODULE TEXT MESSAGE.
C             2. LON(40)    -  THE PRECESSION MODULE TURNED ON MESSAGE.
C             3. LOFF(40)   -  THE PRECESSION MODULE TURNED OFF MESSAGE.
C
C           ACCESS CODES:
C             1. 'PRE MESS' -  THE DATABASE ACCESS CODE FOR THE PRECESSION 
C                              MODULE TEXT MESSAGE.
C             2. 'PRE DATA' -  THE DATABASE ACCESS CODE FOR THE PRECESSION DATA.
C             3. 'PRE CFLG' -  THE DATABASE ACCESS CODE FOR THE PRECESSION
C                              MODULE FLOW CONTROL MESSAGE.
C 
C 3.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C             2. POSSIBLE ERROR OUTPUT
C 
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL
C             CALLED SUBROUTINES: GET4, KILL, PUTA
C
C 3.2.7 CONSTANTS USED - NONE
C
C 3.2.8 PROGRAM VARIABLES -
C             1. KERR   -  THE DATA BASE ERROR RETURN FLAG.
C             2. NDO(3) -  THE DATA BASE RETURN ARRAY INDICES.
C
C 3.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 10/21/77
C                    BRUCE SCHUPLER 06/05/78
C                    CHOPO MA       08/04/81
C                    Jim Ryan       89.07.07 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.14 Converted to Implicit None
C
C     PREI PROGRAM STRUCTURE
C
C   PUT the precession module text message.
      CALL PUTA ('PRE MESS      ', LPREM, 40, 1, 1 )
C
C   PUT the Precession Module control flag message.
      IF (KPREC .NE. 1) CALL PUTA('PRE CFLG      ',LON,40,1,1)
      IF (KPREC .EQ. 1) CALL PUTA('PRE CFLG      ',LOFF,40,1,1)
C
C   GET the precession constant from the database
      CALL GET4 ('PRE DATA      ', PRECON, 1, 1, 1, NDO, KERR )
      IF ( KERR .EQ. 0 ) GO TO 300
           CALL CKILL (6HPREI  , 1, KERR )
C
C   Check KPRED for debug output.
  300 IF ( KPRED .EQ. 0 ) GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for Subroutine PREI." )
      WRITE(6,8)' PRECON   ',PRECON
    8 FORMAT(A,4D25.16/(7X,5D25.16))
C
  500 RETURN
      END
C
C******************************************************************************
      SUBROUTINE PREG ( CT, EPSMNR, XJD, RP )
      IMPLICIT None
C
C 4.    PREG
C
C 4.1   PREG PROGRAM SPECIFICATION
C
C 4.1.1 PREG CALCULATES THE PRECESSION PORTION OF THE COMPLETE CRUST FIXED TO
C       J2000.0 ROTATION MATRIX AND THE CT TIME DERIVATIVE OF THAT MATRIX.
C
C 4.1.2 RESTRICTIONS - NONE
C
C 4.1.3 REFERENCES -  1) LIESKE, J.H., PRECESSION MATRIX BASED ON IAU
C                        (1976) SYSTEM OF ASTRONOMICAL CONSTANTS,
C                        ASTRON. ASTROPHYS. 73, 282-284, 1979.
C                     2) ASH, M.E., "DETERMINATION OF EARTH SATELLITE
C                        ORBITS", LINCOLN LABORATORY TECHNICAL REPORT 
C                        1972-5, 04/19/76, P. 57-59,
C                     3) MUELLER, I.V., "SPHERICAL AND PRACTICAL 
C                        ASTRONOMY AS APPLIED TO GEODESY", 1969, P. 62-65.
C                        (NOTE: THE REFERENCE IN MUELLER REFERS TO THE
C                        COMPUTATION OF THE PRECESSION PORTION OF THE 
C                        COMPLETE J2000.0 TO CRUST FIXED ROTATION MATRIX.
C                        HOWEVER, THE CALC PROGRAM REQUIRES THE TRANSPOSE OF
C                        THIS MATRIX. CARE MUST BE TAKEN WHEN COMPARING THE
C                        REFERENCE TO THE FOLLOWING PROGRAM.) 
C 
C 4.2   PREG PROGRAM INTERFACE
C 
C 4.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. CT      -  THE COORDINATE TIME FRACTION OF THE COORDINATE
C                           TIME DAY. (SEC/SEC) 
C             2. EPSMNR  -  MEAN OBLIQUITY OF REFERENCE EPOCH J2000.0. (RAD) 
C             3. XJD     -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN 
C                           QUESTION. (DAYS)
C
C           OUTPUT VARIABLES:
C             1. RP(3,3,2) - THE PRECESSION PORTION OF THE COMPLETE CRUST 
C                            FIXED TO J2000.0 ROTATION MATRIX AND ITS CT TIME
C                            DERIVATIVE. (UNITLESS, 1/SEC)
C
      Real*8 CT, EPSMNR,  XJD, RP(3,3,2)
C
C 4.2.2 COMMON BLOCKS USED -
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C
C           VARIABLES 'FROM':
C             1. CONVDS  -  THE CONVERSION FACTOR OF RADIANS PER ARC SECOND.
C             2. HALFPI  -  THE VALUE OF PI DIVIDED BY TWO. (RAD)
C             3. SECDAY  -  THE CONVERSION FACTOR OF COORDINATE TIME SECONDS PER
C                           COORDINATE TIME DAY. (SEC/DAY) 
C 
      Real*8 CENTJ, CTHETA(3), CZEE(3), CZETA(3), DJ2000, HEPOCH, 
     1    PRECON, RP1(3,3), RP2(3,3), RP3(3,3), THETA, ZEE, ZETA, CENTR
      COMMON / PRECM / CENTJ, CTHETA, CZEE, CZETA, DJ2000, HEPOCH, 
     1         PRECON, RP1, RP2, RP3, THETA, ZEE, ZETA, CENTR
C
C          VARIABLES 'FROM':
C            1. CENTJ     -  THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                            CENTURY. (DAYS/CENTURY) 
C            2. CTHETA(3) -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE 
C                            CALCULATION OF THE PRECESSIONAL ELEMENT THETA.
C                            (ARCSEC)
C            3. CZEE(3)   -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE 
C                            CALCULATION OF THE PRECESSIONAL ELEMENT ZEE.
C                            (ARCSEC)
C            4. CZETA(3)  -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE
C                            CALCULATION OF THE PRECESSIONAL ELEMENT ZETA. 
C                            (ARCSEC) 
C            5. DJ2000    -  THE JULIAN DATE OF THE EPOCH J2000.0. (DAYS)
C            6. HEPOCH    -  THE NOMINAL VALUE OF THE PRECESSION CONSTANT IN
C                            THE EPOCH J2000.0. (ARCSEC/JCENTURY) 
C            7. PRECON    -  THE PRECESSION CONSTANT. (ARCSEC/JCENTURY) 
C
C          VARIABLES 'TO':
C            1. RP1(3,3)  - THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                           THE NORTH CELESTIAL POLE OF THE EPOCH 2000.0 BY THE
C                           ANGLE ZEE+HALFPI. (UNITLESS) 
C            2. RP2(3,3) -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                           THE ASCENDING NODE OF THE EQUATOR OF THE CURRENT
C                           EPOCH ON THE EQUATOR OF THE J2000.0 EPOCH BY THE 
C                           ANGLE -THETA. (UNITLESS)
C            3. RP3(3,3) -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                           THE NORTH CELESTIAL POLE OF THE CURRENT EPOCH BY
C                           THE ANGLE ZETA-HALFPI. (UNITLESS)
C            4. THETA    -  THE PRECESSIONAL ELEMENT THETA. (RAD)
C            5. ZEE      -  THE PRECESSIONAL ELEMENT ZEE. (RAD)
C            6. ZETA     -  THE PRECESSIONAL ELEMENT ZETA. (RAD)
C            7. CENTR    -  THE NUMBER OF JULIAN CENTURIES FROM J2000.0 TO THE
C                           EPOCH OF THE OBSERVATION. (CENTURIES)
C
      INCLUDE 'ccon.i'
C          VARIABLES 'FROM':
C            1. KPREC - THE PRECESSION MODULE FLOW CONTROL FLAG. 
C            2. KPRED - THE PRECESSION MODULE DEBUG OUTPUT FLAG. 
C 
C 4.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8 DRP1DT(3,3), DRP2DT(3,3), DRP3DT(3,3), DRPDT1(3,3),
     .       DRPDT2(3,3), DRPDT3(3,3)
      Real*8 dysj, dcentr, hc, hs, tzeta, tzee, ttheta
      Integer*4 N, M
C 
C 4.2.4 DATA BASE ACCESS - NONE 
C 
C 4.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE DEBUG OUTPUT 
C 
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG 
C             CALLED SUBROUTINES: DABS, DROTT, MADD3, MMUL3, ROTAT  
C 
C 4.2.7 CONSTANTS USED - CENTJ, CONVDS, DJ2000, SECDAY, 
C                        CTHETA(3), CZEE(3), CZETA(3) 
C 
C 4.2.8 PROGRAM VARIABLES - 
C             1. DCENTR       -  THE CT TIME DERIVATIVE OF CENTR. (CENTURIES/S)
C             2. DRP1DT(3,3)  -  THE CT TIME DERIVATIVE OF THE ROTATION MATRIX
C                                RP1. (1/SEC)
C             3. DRP2DT(3,3)  -  THE CT TIME DERIVATIVE OF THE ROTATION MATRIX
C                                RP2. (1/SEC)
C             4. DRP3DT(3,3)  -  THE CT TIME DERIVATIVE OF THE ROTATION MATRIX
C                                RP3. (1/SEC)
C             5. DRPDT1(3,3)  -  THE FIRST TERM OF THE CT TIME DERIVATIVE OF THE
C                                PRECESSION MATRIX. (1/SEC)
C             6. DRPDT2(3,3)  -  THE SECOND TERM OF THE CT TIME DERIVATIVE OF
C                                THE PRECESSION MATRIX. (1/SEC)
C             7. DRPDT3(3,3)  -  THE THIRD TERM OF THE CT TIME DERIVATIVE OF THE
C                                PRECESSION MATRIX. (1/SEC)
C             8. DYSJ         -  THE NUMBER OF JULIAN DAYS ELAPSED SINCE THE 
C                                EPOCH J2000.0. (DAYS)
C             9. HC           -  THE TERM WHICH SHOWS THE DEPENDENCE OF SMALL
C                                CHANGES IN THE PRECESSION CONSTANT FROM ITS
C                                NOMINAL VALUE IN THE EPOCH J2000.0 ON THE 
C                                PRECESSIONAL ELEMENTS  ZEE AND ZETA. (RAD)
C            10. HS           -  THE TERM WHICH SHOWS THE DEPENDENCE OF SMALL
C                                CHANGES IN THE PRECESSION CONSTANT FROM ITS
C                                NOMINAL VALUE IN THE EPOCH J2000.0 ON THE
C                                PRECESSIONAL ELEMENT THETA. (RAD) 
C            11. TTHETA       -  THE CT TIME DERIVATIVE OF THE PRECESSIONAL
C                                ELEMENT THETA. (RAD/SEC)
C            12. TZEE         -  THE CT TIME DERIVATIVE OF THE PRECESSIONAL
C                                ELEMENT ZEE. (RAD/SEC)
C            13. TZETA        -  THE CT TIME DERIVATIVE OF THE PRECESSIONAL
C                                ELEMENT ZETA. (RAD/SEC)
C
C 4.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 11/10/77
C                    BRUCE SCHUPLER 06/06/78
C                    CHOPO MA       08/04/81
C                    Jim Ryan       89.07.07 Documentation simplified.
C                    David Gordon   94.04.14 Converted to Implicit None
C
C     PREG PROGRAM STRUCTURE
C
C   Calculate the relevant time variables.
C
C   Calculate the number of Julian days elapsed since epoch J2000.0.
      DYSJ = XJD + CT - DJ2000
C
C   Calculate the number of Julian centuries elapsed since epoch J2000.0.
      CENTR = DYSJ / CENTJ
C
C   Calculate the CT time derivative of CENTR.
      DCENTR = 1.D0 / ( CENTJ * SECDAY )
C
C   Calculate the precessional elements ZETA, ZEE, and THETA and
C   their CT time derivatives.
C
C   Compute the terms which show the dependence of small changes of the
C   precession constant from its nominal value in the epoch J2000.0 on the
C   precessional elements.
      HC = 0.D0
      HS = 0.D0
      IF ( DABS ( PRECON - HEPOCH ) .LT. 1.D-4 )  GO TO 220
      HC =( PRECON - HEPOCH ) * DCOS ( EPSMNR ) * CONVDS *
     1      CENTR / 2.D0
      HS =( PRECON - HEPOCH ) * DSIN ( EPSMNR ) * CONVDS *
     1      CENTR
C
C   Compute ZETA and its CT time derivative.
  220 ZETA = ( CZETA(1) * CENTR
     1       + CZETA(2) * CENTR**2
     2       + CZETA(3) * CENTR**3 ) * CONVDS
     3       + HC
      TZETA = ( CZETA(1)
     1        + CZETA(2) * ( 2.D0 * CENTR )
     2        + CZETA(3) * ( 3.D0 * CENTR**2 ) ) * CONVDS * DCENTR
     3        + ( HC / CENTR ) * DCENTR
C
C   Compute ZEE and its CT time derivative.
      ZEE = ( CZEE(1) * CENTR
     1      + CZEE(2) * CENTR**2
     2      + CZEE(3) * CENTR**3 ) * CONVDS
     3      + HC
      TZEE = ( CZEE(1)
     1       + CZEE(2) * ( 2.D0 * CENTR )
     2       + CZEE(3) * ( 3.D0 * CENTR**2 ) ) * CONVDS * DCENTR
     3       + ( HC / CENTR ) * DCENTR
C
C   Compute THETA and its CT time derivative.
      THETA = ( CTHETA(1) * CENTR
     1        + CTHETA(2) * CENTR**2
     2        + CTHETA(3) * CENTR**3 ) * CONVDS
     3        + HS
      TTHETA = ( CTHETA(1)
     1         + CTHETA(2) * ( 2.D0 * CENTR )
     2         + CTHETA(3) * ( 3.D0 * CENTR**2 ) ) * CONVDS * DCENTR
     3         + ( HS / CENTR ) * DCENTR
C
C   Construct the precession matrix.
C
C   Construct the rotation matrix which rotates about the current north
C   celestial pole by an angle equal to ZEE+HALFPI.
      CALL ROTAT ( ZEE+HALFPI, 3, RP1 )
C
C   Construct the rotation matrix which rotates about the ascending node
C   of the equator by an angle equal to TO -THETA.
      CALL ROTAT ( -THETA, 1, RP2 )
C
C   Construct the rotation matrix which rotates about the  J2000.0 north
C   celestial pole by an angle equal to ZETA - HALFPI.
      CALL ROTAT ( ZETA-HALFPI, 3, RP3 )
C
C   Complete the construction of the precession matrix.
      CALL MMUL3 ( RP3, RP2, RP1, RP(1,1,1) )
C
C   Construct the CT time derivative of the precession matrix.
C
C   Construct the CT time derivative of the rotation matrix RP1.
      CALL DROTT ( ZEE+HALFPI, TZEE, 3, DRP1DT )
C
C   Construct the CT time derivative of the rotation matrix RP2.
      CALL DROTT ( -THETA, -TTHETA, 1, DRP2DT )
C
C   Construct the CT time derivative of the rotation mariix RP3.
      CALL DROTT ( ZETA-HALFPI, TZETA, 3, DRP3DT )
C
C   Compute the three terms which make up the CT time derivative of the
C   precession matrix.
      CALL MMUL3 ( DRP3DT, RP2, RP1, DRPDT1 )
      CALL MMUL3 ( RP3, DRP2DT, RP1, DRPDT2 )
      CALL MMUL3 ( RP3, RP2, DRP1DT, DRPDT3 )
C
C   Complete the construction of the CT time derivative of the precession
C   matrix.
      CALL MADD3 ( DRPDT1, DRPDT2, DRPDT3, RP(1,1,2) )
C
C   Check KPREC to determine if the precession module is to be turned off.
      IF ( KPREC .NE. 1 )  GO TO 600
C
C   It's turned off, so set the position portion of the precession matrix to 
C   the identity matrix and zero out the velocity matrix.
      CALL ROTAT ( 0.D0, 3, RP(1,1,1) )
      DO 500 N=1,3
        DO 500 M=1,3
          RP(M,N,2) = 0.0D0
  500 CONTINUE
C
C  Check KPRED for debug.
  600 IF ( KPRED .EQ. 0 ) GO TO 700
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine PREG." )
C
      WRITE(6,8)' CENTR    ',CENTR
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' CENTJ    ',CENTJ
      WRITE(6,8)' CONVDS   ',CONVDS
      WRITE(6,8)' CTHETA   ',CTHETA
      WRITE(6,8)' CZEE     ',CZEE
      WRITE(6,8)' CZETA    ',CZETA
      WRITE(6,8)' DCENTR   ',DCENTR
      WRITE(6,8)' DJ2000   ',DJ2000
      WRITE(6,8)' DRP1DT   ',DRP1DT
      WRITE(6,8)' DRP2DT   ',DRP2DT
      WRITE(6,8)' DRP3DT   ',DRP3DT
      WRITE(6,8)' DRPDT1   ',DRPDT1
      WRITE(6,8)' DRPDT2   ',DRPDT2
      WRITE(6,8)' DRPDT3   ',DRPDT3
      WRITE(6,8)' DYSJ     ',DYSJ
      WRITE(6,8)' HALFPI   ',HALFPI
      WRITE(6,8)' HC       ',HC
      WRITE(6,8)' HEPOCH   ',HEPOCH
      WRITE(6,8)' HS       ',HS
      WRITE(6,8)' PRECON   ',PRECON
      WRITE(6,8)' RP1      ',RP1
      WRITE(6,8)' RP2      ',RP2
      WRITE(6,8)' RP3      ',RP3
      WRITE(6,8)' SECDAY   ',SECDAY
      WRITE(6,8)' THETA    ',THETA
      WRITE(6,8)' TTHETA   ',TTHETA
      WRITE(6,8)' TZEE     ',TZEE
      WRITE(6,8)' TZETA    ',TZETA
      WRITE(6,8)' ZEE      ',ZEE
      WRITE(6,8)' ZETA     ',ZETA
C
      WRITE ( 6, 9200 )  CT, EPSMNR, XJD, RP
 9200 FORMAT (1X, "CT    = ", D30.16, /, 1X,
     1            "EPSMNR= ", D30.16, /, 1X,
     2            "XJD   = ", D30.16, /, 1X,
     3            "RP    = ", 6 ( 3 ( D30.16, 10X ), /, 1X ) )
C
C   7.    NORMAL PROGRAM CONCLUSION.
C
  700 RETURN
      END
C
C*****************************************************************************
      SUBROUTINE PREP ( CFBASE, EPSMNR, RDNP, RN, RS, RW, STAR )
      IMPLICIT None
C
C 5.    PREP
C
C 5.1   PREP PROGRAM SPECIFICATION
C
C 5.1.1 PREP IS THE PRECESSION MODULE PARTIAL DERIVATIVES SECTION. PREP COMPUTES
C       THE PARTIAL DERIVATIVES OF THE DELAY AND THE DELAY RATE W.R.T. THE 
C       PRECESSION CONSTANT.
C 
C 5.1.2 RESTRICTIONS - NONE 
C 
C 5.1.3 REFERENCES - 'DETERMINATION OF EARTH SATELLITE ORBITS', M.ASH, M.I.T.
C                     LINCOLN LAB TECHNICAL REPORT, APRIL 19, 1972,  P.57-59.
C 
C 5.2   PREP PROGRAM INTERFACE
C 
C 5.2.1 CALLING SEQUENCE -
C 
C           INPUT VARIABLES:
C             1. CFBASE(3)  -  THE CRUST FIXED BASELINE VECTOR. (M) 
C             2. EPSMNR     -  MEAN OBLIQUITY OF REFERENCE EPOCH J2000.0 (RAD)
C             3. RDNP(3,3)  -  THE DIURNAL POLAR MOTION PORTION OF THE COMPLETE
C                              CRUST FIXED TO 2000.0 ROTATION MATRIX. (UNITLESS)
C             4. RN(3,3,2)  -  THE NUTATION PORTION OF THE COMPLETE CRUST FIXED
C                              TO 2000.0 ROTATION MATRIX  AND ITS CT TIME 
C                              DERIVATIVE. (UNITLESS, 1/SEC) 
C             5. RS(3,3,3)  -  THE DIURNAL SPIN PORTION OF THE COMPLETE CRUST
C                              FIXED TO 2000.0 ROTATION MATRIX AND ITS FIRST TWO
C                              CT TIME DERIVATIVES. (UNITLESS, 1/SEC, 1/SEC**2)
C             6. RW(3,3,2)  -  THE WOBBLE PORTION OF THE COMPLETE CRUST FIXED TO
C                              J2000.0 ROTATION MATRIX and its first time 
C                              derivative. (unitless, 1/sec)
C             7. STAR(3)    -  THE 2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C 
C           OUTPUT VARIABLES: NONE
C 
C 5.2.2 COMMON BLOCKS USED -
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      EXTERNAL CMATHB
C           VARIABLES 'FROM':
C             1. CONVDS - THE CONVERSION FACTOR OF RADIANS PER ARC SECOND.
C                         (RAD/ARCSEC)
C             2. HALFPI - THE VALUE OF PI DIVIDED BY TWO. (RAD)
C
      INCLUDE 'cphys.i'
C           VARIABLES 'FROM':
C             1. VLIGHT - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C
      Real*8 CENTJ, CTHETA(3), CZEE(3), CZETA(3), DJ2000, HEPOCH, 
     1    PRECON, RP1(3,3), RP2(3,3), RP3(3,3), THETA, ZEE, ZETA, CENTR
      COMMON / PRECM / CENTJ, CTHETA, CZEE, CZETA, DJ2000, HEPOCH, 
     1         PRECON, RP1, RP2, RP3, THETA, ZEE, ZETA, CENTR
C
C           VARIABLES 'FROM':
C             1. RP1(3,3) -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                            THE NORTH CELESTIAL POLE OF THE EPOCH 2000.0 BY THE
C                            ANGLE ZEE+HALFPI. (UNITLESS) 
C             2. RP2(3,3) -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                            THE ASCENDING NODE OF THE EQUATOR OF THE CURRENT
C                            EPOCH ON THE EQUATOR OF THE EPOCH 2000.0 BY THE
C                            ANGLE -THETA. (UNITLESS) 
C             3. RP3(3,3) -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION ABOUT
C                            THE NORTH CELESTIAL POLE OF THE CURRENT EPOCH BY
C                            THE ANGLE ZETA-HALFPI. (UNITLESS)
C             4. THETA    -  THE PRECESSIONAL ELEMENT THETA. (RAD)
C             5. ZEE      -  THE PRECESSIONAL ELEMENT ZEE. (RAD)
C             6. ZETA     -  THE PRECESSIONAL ELEMENT ZETA. (RAD)
C             7. CENTR    -  THE NUMBER OF JULIAN CENTURIES FROM J2000.0 TO THE
C                            EPOCH OF THE OBSERVATION. (CENTURIES)
C
      INCLUDE 'ccon.i'
C           VARIABLES 'FROM':
C             1. KPREC - THE PRECESSION MODULE FLOW CONTROL FLAG.
C             2. KPRED - THE PRECESSION MODULE DEBUG OUTPUT FLAG.
C
C 5.2.3 PROGRAM SPECIFICATIONS -
C 
      Real*8  CFBASE(3), DPREP(2), DRP1DC(3,3), DRP2DC(3,3), 
     1        DRP3DC(3,3), DRPDC1(3,3), DRPDC2(3,3), DRPDC3(3,3),
     2        PBASE(3,2), PR2000(3,3,2), RDNP(3,3), RN(3,3,2), 
     3        RPC(3,3), RS(3,3,3), RW(3,3,2), STAR(3)
      Real*8  epsmnr, pzeta, pzee, ptheta, DOTP
      Integer*4 K
C 
C 5.2.4 DATA BASE ACCESS -
C           'GET' VARIABLES: NONE
C           'PUT' VARIABLES: 
C             1. DPREP(2)  -  THE PARTIAL DERIVATIVES OF THE DELAY AND OF THE
C                             DELAY RATE W.R.T. THE PRECESSION CONSTANT.
C                             ((SEC/(RAD/CENTURY), (SEC/SEC)/(RAD/CENTURY))
C           ACCESS CODES:
C             1. 'PRE PART'  -  THE DATA BASE ACCESS CODE FOR THE PRECESSION
C                               MODULE PARTIAL DERIVATIVES ARRAY.
C 
C 5.2.5 EXTERNAL INPUT/OUTPUT - POSSIBLE ERROR OUTPUT 
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVP 
C             CALLED SUBROUTINES: DCOS, DOTP, DROTT, DSIN, MADD3, 
C                                 MMUL3, MMUL5, PUT4_S, VECRT 
C 
C 5.2.7 CONSTANTS USED - VLIGHT 
C 
C 5.2.8 PROGRAM VARIABLES - 
C            1. DRP1DC(3,3)   -  THE PARTIAL DERIVATIVE OF THE ROTATION MATRIX
C                                RP1 WITH RESPECT TO THE PRECESSION CONSTANT.
C                                (1/(RAD/CENTURY))
C            2. DRP2DC(3,3)   -  THE PARTIAL DERIVATIVE OF THE ROTATION MATRIX
C                                RP2 WITH RESPECT TO THE PRECESSION CONSTANT.
C                                (1/(RAD/CENTURY))
C            3. DRP3DC(3,3)   -  THE PARTIAL DERIVATIVE OF THE ROTATION MATRIX
C                                RP3 WITH RESPECT TO THE PRECESSION CONSTANT.
C                                (1/(RAD/CENTURY))
C            4. DRPDC1(3,3)   -  THE FIRST TERM OF THE THREE TERMS NECESSARY 
C                                FOR THE CALCULATION OF THE PARTIAL DERIVATIVE
C                                OF THE PRECESSION MATRIX WITH RESPECT TO THE
C                                PRECESSION  CONSTANT.  (1/(RAD/CENTURY))
C            5. DRPDC2(3,3)   -  THE SECOND TERM OF THE THREE TERMS NECESSARY
C                                FOR THE CALCULATION OF THE PARTIAL DERIVATIVE
C                                OF THE PRECESSION MATRIX WITH RESPECT TO THE
C                                PRECESSION CONSTANT. (1/(RAD/CENTURY))
C            6. DRPDC3(3,3)   -  THE THIRD TERM OF THE THREE TERMS NECESSARY
C                                FOR THE CALCULATION OF THE PARTIAL DERIVATIVE
C                                OF THE PRECESSION MATRIX WITH RESPECT TO THE
C                                PRECESSION CONSTANT. (1/(RAD/CENTURY))
C            7. PBASE(3,2)    -  THE PARTIAL DERIVATIVES OF THE 2000.0 
C                                BASELINE POSITION AND VELOCITY VECTORS WITH
C                                RESPECT TO THE PRECESSION CONSTANT.
C                                (M/(RAD/CENTURY))
C            8. PR2000(3,3,2) -  THE PARTIAL DERIVATIVE OF THE COMPLETE CRUST
C                                FIXED TO 2000.0 ROTATION MATRIX AND OF ITS CT
C                                TIME DERIVATIVE WITH RESPECT TO THE PRECESSION
C                                CONSTANT. (1/(RAD/SEC), 1/(RAD/CENTURY)*SEC))
C            9. PTHETA        -  THE PARTIAL DERIVATIVE OF THE PRECESSIONAL
C                                ELEMENT THETA WITH RESPECT TO THE PRECESSION
C                                CONSTANT. ((1/CENTURY))
C           10. PZEE          -  THE PARTIAL DERIVATIVE OF THE PRECESSIONAL
C                                ELEMENT ZEE WITH RESPECT TO THE PRECESSION
C                                CONSTANT. ((1/CENTURY))
C           11. PZETA         -  THE PARTIAL DERIVATIVE OF THE PRECESSIONAL
C                                ELEMENT ZETA WITH RESPECT TO THE PRECESSION
C                                CONSTANT. ((1/CENTURY))
C           12. RPC(3,3)      -  THE PARTIAL DERIVATIVE OF THE PRECESSION
C                                PORTION OF THE COMPLETE CRUST FIXED TO 2000.0
C                                ROTATION MATRIX WITH RESPECT TO THE PRECESSION
C                                CONSTANT. ((1/(RAD/CENTURY))
C
C 5.2.9 PROGRAMMER - DALE MARKHAM   01/13/77
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 06/05/78
C                    CHOPO MA       08/04/81
C                    JIM RYAN       09/14/81 FIXED DOCUMENTATION ONLY
C                    Jim Ryan       89.07.07 Documentation simplified.
C                    Jim Ryan       89:10:05 CPHYS common made an include file
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 94.04.14 Converted to Implicit None
C                    David Gordon 95.12.11 Changed RW(3,3) to RW(3,3,2).
C
C     PREP PROGRAM STRUCTURE
C
C   Compute the partial derivative of the precession matrix with
C   respect to the precession constrat.
C
C     Compute the partial derivatives of the precessional elements with
C     respect to the precession constant.
      PZETA  = DCOS ( EPSMNR ) * CENTR / 2.D0
      PZEE   = DCOS ( EPSMNR ) * CENTR / 2.D0
      PTHETA = DSIN ( EPSMNR ) * CENTR
C
C   Construct the partial derivative of the rotation matrices
C   RP1, RP2, and RP3 with respect to the precession constant.
      CALL DROTT ( ZEE+HALFPI, PZEE, 3, DRP1DC )
      CALL DROTT ( -THETA, -PTHETA, 1, DRP2DC )
      CALL DROTT ( ZETA-HALFPI, PZETA, 3, DRP3DC )
C
C   Construct the partial derivative of the precession matrix
C   with respect to the precession constant.
C
C   Compute the three terms neccesary for the calculation.
      CALL MMUL3 ( DRP3DC, RP2, RP1, DRPDC1 )
      CALL MMUL3 ( RP3, DRP2DC, RP1, DRPDC2 )
      CALL MMUL3 ( RP3, RP2, DRP1DC, DRPDC3 )
C
C   Complete the construction by ADDI_Sng the three terms.
      CALL MADD3 ( DRPDC1, DRPDC2, DRPDC3, RPC )
C
C   Compute the partial derivative of the complete crust fixed to J2000.0
C   rotation matix and its first CT time derivative with respect to the
C   precession constant.
C    (NOTE: Of the three terms which are used to compute the CT
C    time derivative of the complete curst fixed to J2000.0 rotation
C    matrix only the term which contains the CT time
C    derivative of the diurnal spin matrix is considered significant
C    enough to include in this partial derivatives section.)
      CALL MMUL5 ( RPC, RN(1,1,1), RS(1,1,1), RDNP, RW(1,1,1),
     .           PR2000(1,1,1))
      CALL MMUL5 ( RPC, RN(1,1,1), RS(1,1,2), RDNP, RW(1,1,1),
     .           PR2000(1,1,2))
C
C   Compute the partial derivaiives of the J200.0 baselene position and
C   velocity vectors with respect to the precession constant.
      DO 300  K = 1,2
           CALL VECRT ( PR2000(1,1,K), CFBASE, PBASE(1,K) )
  300 CONTINUE
C
C   Compute the partial derivatives of the delay and rate with respect
C   to the precession constant.
      DO 400  K = 1,2
        DPREP(K) = DOTP ( PBASE(1,K), STAR ) / VLIGHT
  400 CONTINUE
C
C   PUT precession partials.
      CALL PUT4 ('PRE PART      ', DPREP, 2, 1, 1 )
C
C   Check KPRED for debug output.
      IF ( KPRED .EQ. 0 )  GO TO 800
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine PREP." )
C
      WRITE(6,8)' DPREP   ',DPREP
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' DRP1DC  ',DRP1DC
      WRITE(6,8)' DRP2DC  ',DRP2DC
      WRITE(6,8)' DRP3DC  ',DRP3DC
      WRITE(6,8)' DRPDC1  ',DRPDC1
      WRITE(6,8)' DRPDC2  ',DRPDC2
      WRITE(6,8)' DRPDC3  ',DRPDC3
      WRITE(6,8)' HALFPI  ',HALFPI
      WRITE(6,8)' PBASE   ',PBASE
      WRITE(6,8)' PR2000  ',PR2000
      WRITE(6,8)' PTHETA  ',PTHETA
      WRITE(6,8)' PZEE    ',PZEE
      WRITE(6,8)' PZETA   ',PZETA
      WRITE(6,8)' RP1     ',RP1
      WRITE(6,8)' RP2     ',RP2
      WRITE(6,8)' RP3     ',RP3
      WRITE(6,8)' RPC     ',RPC
      WRITE(6,8)' THETA   ',THETA
      WRITE(6,8)' VLIGHT  ',VLIGHT
      WRITE(6,8)' ZEE     ',ZEE
      WRITE(6,8)' ZETA    ',ZETA
      WRITE(6,8)' CENTR   ',CENTR
C
      WRITE ( 6, 9200 )  CFBASE, EPSMNR, RDNP, RN, RS, RW, STAR
 9200 FORMAT (1X, "CFBASE = ", 3 ( D30.16, 10X ), /, 1X,
     1            "EPSMNR = ",  ( D30.16, 10X ), /, 1X,
     2            "RDNP   = ", 3 ( 3 ( D30.16, 10X ), /, 1X ),
     3            "RN     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     4            "RS     = ", 9 ( 3 ( D30.16, 10X ), /, 1X ),
     5            "RW     = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),
     6            "STAR   = ", 3 ( D30.16, 10X ) )
C
C     Normal conclusion.
  800 RETURN
      END
C
C******************************************************************************
      SUBROUTINE PREC
      Implicit None
C
C 1.   PREC
C
C 1.1   PREC PROGRAM SPECIFICATION
C
C 1.1.1 THE PRECESSION MODULE HAS NO CONTRIBUTIONS SECTION.
C
C 1.1.2 RESTRICTIONS - NONE
C
C 1.1.3 REFERENCES - NONE
C
C 1.2   PREC PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE - NONE
C
C 1.2.2 COMMON BLOCKS USED - NONE
C
C 1.2.3 PROGRAM SPECIFICATIONS - NONE
C
C 1.2.4 DATA BASE ACCESS - NONE 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 
C 1.2.6 SUBROUTINE INTERFACE - NONE 
C 
C 1.2.7 CONSTANTS USED - NONE 
C 
C 1.2.8 PROGRAM VARIABLES USED - NONE (I IS INCLUDED TO AVOID COMPILATION 
C                                       ERRORS) 
       Integer*4 I
C 
C 1.2.9 PROGRAMMER - BRUCE SCHUPLER 11/2/77 
C 
C 1.3   PREC PROGRAM STRUCTURE - NONE 
C 
      I = 1
C
      RETURN
      END
C
C******************************************************************************
      BLOCK DATA PRECMB
      IMPLICIT None
C
C 7.    PREBD
C
C 7.1   PREBD PROGRAM SPECIFICATION
C
C 7.1.1 PREBD IS THE PRECESSION MODULE BLOCK DATA INPUT AND INITIALIZATION
C       SECTION.
C
C 7.1.2 RESTRICTIONS - NONE
C
C 7.1.3 REFERENCES - 1) LIESKE, J.H., PRECESSION MATRIX BASED ON IAU
C                       (1976) SYSTEM OF ASTRONOMICAL CONSTANTS,
C                       ASTRON. ASTROPHYS. 73, 282-284, 1979.
C                    2) LIESKE, J.H., ET AL., EXPRESSIONS FOR THE
C                       PRECESSION QUANTITIES BASED UPON THE IAU (1976)
C                       SYSTEM OF ASTRONOMICAL CONSTANTS, ASTRON. ASTROPHYS.
C                       58, 1-16, 1977. 
C                    3) ASH, M.E., "DETERMINATION OF EARTH SATELLITE 
C                       ORBITS", LINCOLN LABORATORY TECHNICAL REPORT 
C                       1972-5, 04/19/72, P. 55-57.
C 
C 7.2   PREBD PROGRAM INTERFACE 
C 
C 7.2.1 CALLING SEQUENCE - NONE 
C 
C 7.2.2 COMMON BLOCK -
C 
      Real*8 CENTJ, CTHETA(3), CZEE(3), CZETA(3), DJ2000, HEPOCH, 
     1    PRECON, RP1(3,3), RP2(3,3), RP3(3,3), THETA, ZEE, ZETA, CENTR
      COMMON / PRECM / CENTJ, CTHETA, CZEE, CZETA, DJ2000, HEPOCH, 
     1         PRECON, RP1, RP2, RP3, THETA, ZEE, ZETA, CENTR
C
C          VARIABLES 'TO':
C             1. CENTJ     -  THE NUMBER OF COORDINATE TIME DAYS PER JULIAN 
C                             CENTURY.  (DAYS/CENTURY)  ( CENTJ = 36525.D0 ) 
C             2. CTHETA(3) -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE
C                             CALCULATION OF THE PRECESSIONAL ELEMENT THETA.
C                             (ARCSEC)  (SEE REFERENCES) 
C                             ( CTHETA(1) = + 2004.3109D0, 
C                               CTHETA(2) = - 0.42665D0, 
C                               CTHETA(3) = - 0.041833D0 ) 
C             3. CZEE(3)   -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE
C                             CALCULATION OF THE PRECESSIONAL ELEMENT ZEE.
C                             (ARCSEC)  (SEE REFERENCES) 
C                             ( CZEE(1) = + 2306.2181D0, 
C                               CZEE(2) = + 1.09468D0, 
C                               CZEE(3) = + 0.018203D0 ) 
C             4. CZETA(3)  -  THE CONSTANTS APPEARING IN TERMS 1-3 IN THE
C                             CALCULATION OF THE PRECESSIONAL ELEMENT ZETA.
C                             (SECOND-SEC)  (SEE REFERENCES)
C                             ( CZETA(1) = + 2306.2181D0,
C                               CZETA(2) = + 0.30188D0,
C                               CZETA(3) = + 0.017998D0 )
C             5. DJ2000    -  THE JULIAN DATE OF THE EPOCH J2000.0. (DAYS) 
C                             (2000 JAN 01 12HR UT)
C                             ( DJ2000 = 2451545.0D0 ) 
C             6. HEPOCH    -  THE NOMINAL VALUE OF THE PRECESSION CONSTANT IN
C                             THE EPOCH J2000.0. (ARCSEC/JCENTURY)
C                             ( HEPOCH = 5029.0966D0 ) 
C 
C          VARIABLES 'PASSED' FROM OTHER MODULE SECTIONS: 
C             1. PRECON    -  THE PRECESSION CONSTANT. (ARCSEC/JCENTURY) 
C             2. RP1(3,3)  -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION
C                             ABOUT THE NORTH CELESTIAL POLE OF THE EPOCH 2000.0
C                             BY THE ANGLE ZEE+HALFPI. (UNITLESS) 
C             3. RP2(3,3)  -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION
C                             ABOUT THE ASCENDING NODE OF THE EQUATOR OF THE 
C                             CURRENT EPOCH ON THE EQUATOR OF THE 2000.0 EPOCH
C                             BY THE ANGLE -THETA. (UNITLESS)
C             4. RP3(3,3)  -  THE ROTATION MATRIX WHICH PERFORMS A ROTATION
C                             ABOUT THE NORTH CELESTIAL POLE OF THE CURRENT
C                             EPOCH BY THE ANGLE ZETA-HALFPI. (UNITLESS)
C             5. THETA     -  THE PRECESSIONAL ELEMENT THETA. (RAD)
C             6. ZEE       -  THE PRECESSIONAL ELEMENT ZEE. (RAD)
C             7. ZETA      -  THE PRECESSIONAL ELEMENT ZETA. (RAD) 
C 
C 7.2.3 PROGRAM SPECIFICATIONS -
C 
      DATA  CENTJ     / 36525.D0 /, 
     1      CTHETA    / +2004.3109D0, -0.42665D0, -0.041833D0 /,
     2      CZEE      / +2306.2181D0, +1.09468D0, +0.018203D0 /,
     3      CZETA     / +2306.2181D0, +0.30188D0, +0.017998D0 /,
     4      DJ2000    / 2451545.D0 /, 
     5      HEPOCH    / 5029.0966D0 / 
C 
C 7.2.4 CONSTANTS USED - CENTJ, CTHETA(3), CZEE(3), CZETA(3), DJ2000, 
C                        HEPOCH 
C 
C 7.2.5 PROGRAMMER - DALE MARKHAM   01/13/77  
C                    PETER DENATALE 07/12/77
C                    BRUCE SCHUPLER 06/05/78
C                    CHOPO MA       08/04/81
C                    David Gordon 94.04.14 Converted to Implicit None
C 
C 7.3   PREBD PROGRAM STRUCTURE - NONE
      END 
