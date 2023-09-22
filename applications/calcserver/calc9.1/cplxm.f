      SUBROUTINE PLXA
      IMPLICIT None
C 
C 1.    PLXA
C 
C 1.1   PLXA PROGRAM SPECIFICATION
C 
C 1.1.1 PLXA ADDS ENTRIES TO THE TABLE OF CONTENTS FOR THE PARALLAX MODULE TEXT
C       MESSAGE, AND PARTIAL DERIVATIVE AND CONTRIBUTION ARRAYS. IT ALSO ADDS
C       ENTRIES TO THE TABLE OF CONTENTS FOR THE FLOW CONTROL MESSAGE.
C 
C 1.2   PLXA PROGRAM INTERFACE
C 
C 1.2.1 CALLING SEQUENCE - NONE 
C 
C 1.2.2 COMMON BLOCKS USED - NONE 
       INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C             1. KPLXC - THE PARALLAX MODULE FLOW CONTROL FLAG.
C                         0 => Do NOT compute parallax delay and rate
C                              contributions.
C                         1 => DO compute parallax delay and rate
C                              contributions. (Lcode 'PRLXCONT'). A distance
C                              in parsecs must be supplied, either via Lcode
C                              'DISTPSEC', or via external source file input,
C                              for this computation to be meaningful.
C
C 1.2.4 DATA BASE ACCESS -
C           ACCESS CODES:
C             1. 'PLX MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                               PARALLAX MODULE TEXT MESSAGE. 
C             2. 'PLX PART'  -  THE DATA BASE ACCESS CODE FOR THE PARALLAX
C                               MODULE PARTIAL DERIVATIVES ARRAY.
C             3. 'PLX CFLG'  -  THE DATA BASE ACCESS CODE FOR THE 
C                               PARALLAX MODULE FLOW CONTROL MESSAGE. 
C             4. 'PLX1PSEC'  -  New with Calc 9.x. Has two possible usages. 
C                               1) Parallax delay and rate contributions for
C                               the source at 1 parsec distance. Divide by
C                               actual distance (parsecs) to get delay and 
C                               rate corrections. 2) Partial derivatives of
C                               delay and rate w.r.t. inverse distance in
C                               parsecs. Use to solve for inverse distance. 
C             5. 'PRLXCONT'  -  Optional/New with Calc 9.x. Delay and rate 
C                               contributions for parallax (sec, sec/sec).
C                               Computed only if KPLXC=1. If distance not
C                               supplied, these will be zeros. Intended for
C                               correlator usage.  
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDA, ADDR
C
C 1.2.9 PROGRAMMER - C. A. KNIGHT  08/12/80
C                    SAVITA GOEL   06/04/87 (CDS FOR A900)
C                    89.08.15 Jim Ryan Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                         implimented.
C                    David Gordon 94.04.14 Converted to Implicit None.
C                    David Gordon 98.09.11 Put in ADD's for 'PLX1PSEC' and
C                         'PRLXCONT', new parallax partials and contributions. 
C
C 1.3   PLXA PROGRAM STRUCTURE
C
C   ADD for the module text message.
      CALL ADDA (1,'PLX MESS','Parallax message definition     ',
     1     40, 1, 1 )
C
C   ADD for module flow control message.
      CALL ADDA (1,'PLX CFLG','Parallax flow control mess def  ',
     1     40,1,1)
C
C   ADD for parallax partial derivatives. (Old partial, for annual parallax
C        of 1 radian.)
      CALL ADDR (2,'PLX PART','Parallax partial deriv. def.    ',
     1     2, 1, 1 )
C
C   New ADD for parallax partials. Use to solve for inverse distance (parsecs)
C     or divide by distance (in parsecs) to get delay and rate corrections.
      CALL ADDR (2,'PLX1PSEC','Parallax partial/contr, 1 parsec',
     1     2, 1, 1 )
C
C   Optional add for total parallax contributions.
      If (KPLXC .eq. 1) 
     *  CALL ADDR (2,'PRLXCONT','Parallax Contributions, sec, s/s',
     *             2, 1, 1 )
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE PLXI
      IMPLICIT None
C
C 3.    PLXI
C
C 3.1   PLXI PROGRAM SPECIFICATION
C
C 3.1.1 PLXI IS THE PARALLAX MODULE INPUT AND INITIALIZATION SECTION.
C
C 3.2   PLXI PROGRAM INTERFACE
C
C 3.2.1 CALLING SEQUENCE - NONE
C 
C 3.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'                   
C           VARIABLES 'FROM':
C             1.  KPLXC  -  THE PARALLAX MODULE FLOW CONTROL FLAG.
C                           0 => Do not compute parallax contributions
C                           1 => DO compute parallax contributionsns
C             2.  KPLXD  -  THE PARALLAX MODULE DEBUG OUTPUT FLAG.
C
C 3.2.3 PROGRAM SPECIFICATIONS -
      INTEGER*2  NDO(3), idm7
      INTEGER*2      LON(40),    LOFF(40),     LPLXM(40)
      CHARACTER*40 C_LON(2) ,  C_LOFF(2) ,   C_LPLXM(2)
      EQUIVALENCE( C_LON,LON),(C_LOFF,LOFF),(C_LPLXM,LPLXM)
C
      DATA C_LPLXM  /
     .'Parallax Module, Last modified 98SEP11, ',
     .'D. Gordon/GSFC                          '/
C
      DATA C_LON  /
     .'Parallax Contributions OFF.             ',
     .'                                        '/
C
      DATA C_LOFF /
     .'Parallax Contributions ON.              ',
     .'                                        '/
C
C 3.2.4 DATA BASE ACCESS -
C           'GET' VARIABLES: none
C           'PUT' VARIABLES:
C             1. LPLXM(40)  -  THE PARALLAX MODULE TEXT MESSAGE.
C             2. LON(40)    -  THE PARALLAX MODULE 'TURNED ON' MESSAGE.
C             3. LOFF(40)   -  THE PARALLAX MODULE 'TURNED OFF' MESSAGE.
C           ACCESS CODES:
C             1. 'PLX MESS' -  THE DATA BASE ACCESS CODE FOR THE PARALLAX
C                              MODULE TEXT MESSAGE.
C             2. 'PLX CFLG' -  THE DATA BASE ACCES CODE FOR THE PARALLAX
C                              MODULE FLOW CONTROL MESSAGE.
C 
C 3.2.5 EXTERNAL INPUT/OUTPUT - None
C
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL
C             CALLED SUBROUTINES: PUTA
C
C 3.2.7 CONSTANTS USED - NONE
C
C 3.2.8 PROGRAM VARIABLES -  NONE
C
C 3.2.9 PROGRAMMER - C. A. KNIGHT  02/26/80
C                    89.08.15 Jim Ryan Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    David Gordon 94.04.14 Converted to Implicit None.
C                    David Gordon 98.09.11 Updated data base text messages.
C
C    PLXI PROGRAM STRUCTURE
C
C   PUT the module text message.
      CALL PUTA ('PLX MESS      ', LPLXM,40,1,1)
C
C   PUT the module flow control message.
      IF (KPLXC .EQ. 0) CALL PUTA('PLX CFLG      ',LON ,40,1,1)
      IF (KPLXC .NE. 0) CALL PUTA('PLX CFLG      ',LOFF,40,1,1)
C
  500 RETURN
      END
C
C******************************************************************************
      SUBROUTINE PLXG 
      IMPLICIT None
C
C 4.    PLXG
C
C 4.1   PLXG PROGRAM SPECIFICATION
C 4.1.1 PLXG is the Parallax Module geometry section. With Calc 9.0, 
C        parallax contributions can be calculated, but the work will be 
C        done in the partials and contributions sections. 
C
C 4.2   PLXG PROGRAM INTERFACE
C 4.2.1 CALLING SEQUENCE - None
C 4.2.2 COMMON BLOCKS USED - NONE 
C 4.2.3 PROGRAM SPECIFICATIONS -
C 4.2.4 DATA BASE ACCESS - NONE 
C 4.2.5 EXTERNAL INPUT/OUTPUT - NONE
C 4.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG
C             CALLED SUBROUTINES: NONE
C 4.2.7 CONSTANTS USED - NONE
C 4.2.8 PROGRAM VARIABLES - NONE
C
C 4.2.9 PROGRAMMER - C. A. KNIGHT  02/26/80
C                    89.08.15 Jim Ryan Documentation simplified.
C
      Integer*4 I
C
C     PLXG PROGRAM STRUCTURE
C
C   Normal program conclusion - a dummy executable statement included to make
C    the compiler happy
      I=1
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE PLXP( SUN, DSTRP, CD, CRA, SD, SRA,
     *           EARTH, STAR, EPBASE, SITEV )
      IMPLICIT None
C
C 5.    PLXP
C
C 5.1   PLXP PROGRAM SPECIFICATION
C
C 5.1.1 PLXP is the Parallax module partial derivatives section. PLXP computes
C       the partial derivatives of the delay and rate with respect to the
C       parallax parameter.
C
C 5.1.2 RESTRICTIONS - MUST BE CALLED AFTER STAR PARTIALS, SUN DATA, AND 
C                      PHYSICAL CONSTANTS ARE AVAILABLE.
C
C 5.1.3 REFERENCES - EXPLANATORY SUPPLEMENT TO THE A.E.N.A. (GREEN BOOK), p. 64.
C 
C 5.2   PLXP PROGRAM INTERFACE
C 
C 5.2.1 CALLING SEQUENCE -
C         INPUT VARIABLES:
C            1. SUN(3,2)  - The J2000 geocentric Sun position and velocity 
C                           vectors. (m, m/sec)
C            2. DSTRP(2,2)- Partial derivatives of the delay and delay rate with
C                           respect to source RA and Dec. First index runs over
C                           RA and Dec, second runs over delay and rate. 
C                           (sec/rad, sec/sec-rad) 
C            3. CD        - COSINE OF DECLINATION OF THE CURRENT SOURCE.
C            4. CRA       - COSINE OF RIGHT ASCENSION OF THE CURRENT SOURCE.
C            5. SD        - SINE OF DECLINATION OF THE CURRENT SOURCE.
C            6. SRA       - SINE OF RIGHT ASCENSION OF THE CURRENT SOURCE.
C            7. EARTH(3,3)- THE SOLAR SYSTEM BARYCENTRIC EARTH POSITION,
C                           VELOCITY, AND ACCELERATION VECTORS.
C                           (M, M/SEC, M/SEC**2)
C            8. STAR(3)   - THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS)
C            9. EPBASE(3,2)-THE J2000.0 GEOCENTRIC BASELINE POSITION AND
C                           VELOCITY VECTORS. (M, M/SEC)
C           10. SITEV(3,2)- THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF EACH
C                           OBSERVATION SITE. (M/SEC)
C 
C 5.2.2 COMMON BLOCKS USED -
C 
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C              1.  KPLXC  -  THE PARALLAX MODULE FLOW CONTROL FLAG.
C              2.  KPLXD  -  THE PARALLAX MODULE DEBUG OUTPUT FLAG.
C
      INCLUDE 'cphys.i'
C            VARIABLES FROM -
C              1. AU_meters - Astronomical Unit. (meters)
C              2. VLIGHT    - The velocity of light in vacuum.  (m/sec)
C
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C            VARIABLES FROM -
C              1.  CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
C                           (RAD/ARCSECOND)
C
      Real*8 Dparsec, t_prlx(2)
      Common /PRLX/ Dparsec, t_prlx
C       Variables From:
C         Dparsec - Distance of the current source in parsecs, either from
C                   Lcode 'DISTPSEC' or from an external source file. But
C                   usually this will be zero.
C       Variables To:
C         t_prlx(2) - Partial derivatives of the delay and rate w.r.t. the 
C                     inverse source distance (in parsecs). 
C
C 5.2.3 PROGRAM SPECIFICATIONS -
      Real*8 CD, CRA, SD, SRA, SUN(3,2)
      Real*8 DSTRP(2,2), SUNVCT(3), DPLXP(2), DRADPI, DDCDPI
      Real*8 EARTH(3,3), STAR(3), EPBASE(3,2), SITEV(3,2), DOTP
      Real*8 E(3), A(3), B(3), K(3), dK(3), B0(3), V0(3), Vpw2(3)
      INTEGER*4 I, J
C
C 5.2.4 DATA BASE ACCESS -
C            'PUT' VARIABLES:
C              1.  DPLXP(2)  - THE PARTIAL DERIVATIVES OF THE DELAY AND RATE
C              2.  t_prlx(2) - The partial derivatives of the delay and rate
C                              with respect to the inverse distance in parsecs.
C            ACCESS CODES:
C              1. 'PLX PART' - THE DATA BASE ACCESS CODE FOR THE PARALLAX 
C                              MODULE PARTIAL DERIVATIVES ARRAY.
C              2. 'PLX1PSEC' - The data base access code for the parallax 
C                              partial with respect to inverse distance in
C                              parsecs.
C 
C 5.2.5 EXTERNAL INPUT/OUTPUT -  POSSIBLE DEBUG OUTPUT 
C 
C 5.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVR
C             CALLED SUBROUTINES: PUT4  
C 
C 5.2.7 CONSTANTS USED - NONE 
C 
C 5.2.8 PROGRAM VARIABLES - 
C             1. DRADPI - THE PARTIAL DERIVATIVE OF SOURCE RIGHT ASCENSION 
C                         WRT THE PARALLAX PARAMETER. (RAD/RAD) 
C             2. DDCDPI - THE PARTIAL DERIVATIVE OF SOURCE DECLINATION WRT
C                         THE PARALLAX PARAMETER. (RAD/RAD)
C
C 5.2.9 PROGRAMMER - C. A. KNIGHT  02/26/80
C                    Jim Ryan      89:10:05 CPHYS common made an include file
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                        implimented.
C                    D. Gordon 94.04.14 Converted to Implicit None.
C                    D. Gordon 95.05.02 Removed database GET4 calls for 
C                        'SUN DATA' and 'STR PART'. Now passing them from the
C                        driver subroutine via variables SUN and DSTRP. 
C                        Replaced (VLIGHT*SECPAU) with AU_meters from cphys.i.
C                        Changed SUNVCT(3,2) to SUNVCT(3).
C                    D. Gordon 96.02.27 Removed second declaration of 
C                        DSTRP(2,2), found by Warwick Wilson, ATNF. Minor
C                        code cleanup.
C                    D. Gordon 98.09.11 Added EARTH, STAR, EPBASE, SITEV.
C                        Corrected computations to use Earth-barycenter
C                        vector instead of Earth-Sun vector. 
C                        Adding partials w.r.t. inverse distance in parsecs
C                        and new Lcode 'PLX1PSEC'.
C
C 5.3   PLXP PROGRAM STRUCTURE
C
C   The rectangular coordinates of the Sun are supplied in meters but are
C   required in A.U. - convert them using the constant AU_meters.
C    98.09.11 - Should be using Barycentric coordinates.
      do i=1,3
C**    SUNVCT(I) = SUN(I,1) / AU_meters
       SUNVCT(I) = -EARTH(I,1) / AU_meters
      enddo 
C
C Compute the partial derivatives by the chain rule.
C
C   Compute derivatives of RA and DEC wrt the parallax
      DRADPI = (SUNVCT(2)*CRA-SUNVCT(1)*SRA)/CD
      DDCDPI = SUNVCT(3)*CD-SUNVCT(1)*CRA*SD-SUNVCT(2)*SRA*SD
C
C   Apply the chain rule.
      DPLXP(1)=DRADPI*DSTRP(1,1)+DDCDPI*DSTRP(2,1)
      DPLXP(2)=DRADPI*DSTRP(1,2)+DDCDPI*DSTRP(2,2)
C
C   PUT the parallax partials.
      CALL PUT4 ('PLX PART      ', DPLXP, 2, 1, 1 )
C
C Test
C     DRADPI = DRADPI * CONVDS
C     DDCDPI = DDCDPI * CONVDS
C     dK(1) = -SD*DDCDPI*CRA - CD*SRA*DRADPI
C     dK(2) = -SD*DDCDPI*SRA + CD*CRA*DRADPI
C     dK(3) =  CD*DDCDPI
C   print dK in arc-seconds
C     WRITE(6,8)'test/dK: ', dK(1)/CONVDS, dK(2)/CONVDS, 
C    *             dK(3)/CONVDS
C
C   Check for debug output.
      IF ( KPLXD .EQ. 0 )  GO TO 500
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for subroutine PLXP." )
      WRITE(6,8)' DSTRP   ',DSTRP
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' SUNVCT  ',SUNVCT
      WRITE(6,8)' VLIGHT  ',VLIGHT
      WRITE(6,8)' SECPAU  ',SECPAU
      WRITE(6,8)' SRA     ',SRA
      WRITE(6,8)' SD      ',SD
      WRITE(6,8)' CRA     ',CRA
      WRITE(6,8)' CD      ',CD
      WRITE(6,8)' DPLXP   ',DPLXP
C
 500  CONTINUE
C
C   New computation of parallax partial w.r.t. inverse distance (parsecs)
      Do J=1,3
       E(J) = EARTH(J,1)
       K(J) = STAR(J)
       B0(J) = EPBASE(J,1)
       V0(J) = EPBASE(J,2)
       Vpw2(J) = EARTH(J,2) + SITEV(J,2)
      Enddo
C   Compute vector to Earth that is perpendicular to star vector  
       Call CROSP(E, K, A)
       Call CROSP(K, A, B)
C   Compute parallax correction to star vector at distance of 1 parsec
C    (Radians)
       dK(1) = B(1) / AU_meters * CONVDS
       dK(2) = B(2) / AU_meters * CONVDS
       dK(3) = B(3) / AU_meters * CONVDS
C   print dK in arc-seconds
C     WRITE(6,8)' dK: ', dK(1)/CONVDS, dK(2)/CONVDS, 
C    *             dK(3)/CONVDS
C
C  Convert to arc-sec - parsec units
C     DPLXP(1) = DPLXP(1) * CONVDS
C     DPLXP(2) = DPLXP(2) * CONVDS
C   PUT the new parallax partials.
C     CALL PUT4 ('PLX1PSEC      ', DPLXP, 2, 1, 1 )

C
C  Compute delay and rate corrections using Consensus model equation (15)
      t_prlx(1) = -(DOTP(dK,B0)/VLIGHT) / (1.D0 + (DOTP(K,Vpw2)/VLIGHT))
     *         + ((DOTP(K,B0)/VLIGHT2) * DOTP(dK,Vpw2))
      t_prlx(2) = -(DOTP(dK,V0)/VLIGHT) / (1.D0 + (DOTP(K,Vpw2)/VLIGHT))
     *         + ((DOTP(K,V0)/VLIGHT2) * DOTP(dK,Vpw2))
      CALL PUT4 ('PLX1PSEC      ', t_prlx, 2, 1, 1 )
C     WRITE(6,8)' t_prlx(parsec): ',  t_prlx
C
C     Normal termination.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE PLXC
      IMPLICIT None
C
C 6.    PLXC
C
C 6.1   PLXC PROGRAM SPECIFICATION
C
C 6.1.1 PLXC is the parallax module contribution section. Contributions will
C       be computed if KPLXC=1 and if the source distance is given. This is
C       intended for special cases at correlators only.
C
C 6.2   PLXC PROGRAM INTERFACE
C 
C 6.2.1 CALLING SEQUENCE - NONE 
C 
C 6.2.2 COMMON BLOCKS USED
C
      Real*8 Dparsec, t_prlx(2)
      Common /PRLX/ Dparsec, t_prlx
C 
      INCLUDE 'ccon.i'
C            VARIABLES 'FROM':
C              1.  KPLXC  -  THE PARALLAX MODULE FLOW CONTROL FLAG.
C              2.  KPLXD  -  THE PARALLAX MODULE DEBUG OUTPUT FLAG.
C
C 6.2.3 PROGRAM SPECIFICATIONS 
C
        Real*8 PLXCON(2)
C 
C 6.2.4 DATA BASE ACCES
C 
C 6.2.5 EXTERNAL INPUT/OUTPUT - None
C
C 6.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVC
C             CALLED SUBROUTINES: NONE
C
C 6.2.7 CONSTANTS USED - NONE
C
C 6.2.8 PROGRAM VARIABLES - NONE
C
C 6.2.9 PROGRAMMER - C. A. KNIGHT  02/26/80
C                    David Gordon 98.09.11 Optional computation of parallax
C                    contributions added. 
C
C 6.3   PLXC PROGRAM STRUCTURE
C
      IF (KPLXC.eq.1) Then
       PLXCON(1) = t_prlx(1) / Dparsec
       PLXCON(2) = t_prlx(2) / Dparsec
C   PUT the parallax contributions.
        CALL PUT4 ('PRLXCONT      ', PLXCON, 2, 1, 1 )
c       WRITE(6,8)' PLXCON  ', PLXCON
      ENDIF
C
    8 FORMAT(A,4D25.16/(7X,5D25.16))
C
      RETURN
      END 
