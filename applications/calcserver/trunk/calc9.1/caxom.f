      SUBROUTINE AXOA
      IMPLICIT None
C
C 1.    AXOA
C
C 1.1   AXOA PROGRAM SPECIFICATION
C
C 1.1.1 AXOA adds entries to the table of contents for the Axis Offset 
C       text message, the axis offset flow control message, the axis
C       offset partial derivatives and contributions, the feedbox 
C       rotation routine test message, the feedbox rotation angles, and
C       the group and phase delay rate corrections. 
C
C 1.2   AXOA PROGRAM INTERFACE
C
C 1.2.4 DATA BASE ACCESS -
C           ACCESS CODES ADDED:
C              1.  'AXO MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 AXIS OFFSET MODULE TEXT MESSAGE.
C              2.  'AXO CFLG'  -  THE DATA BASE ACCESS CODE FOR THE AXIS
C                                 OFFSET CONTROL FLAG MESSAGE.
C              3.  'AXO PART'  -  THE DATA BASE ACCESS CODE FOR THE AXIS 
C                                 OFFSET MODULE PARTIAL DERIVATIVES ARRAY.
C                                 (Using new, simple axis offset model)
C              4.  'AXO CONT'  -  The database access code for the new axis
C                                 offset module contributions array.
C                                 (Using the new, simple axis offset model.)
C              5.  'AXO2CONT'  -  The database access code for the correction
C                                 to convert to the more complicated new axis
C                                 offset model.
C              6.  'AXIS OLD'  -  The database access code for the correction
C                                 to convert to the old (Calc 7.6 and earlier)
C                                 axis offset model.
C              7.  'PAN MESS'  -  The database access code for the
C                                 feedhorn rotation routine text message.
C              8.  'PARANGLE'  -  The database access code for the
C                                 feedhorn rotation angles.
C              9.  'FEED.COR'  -  The database access code for the feedbox 
C                                 rotation corrections for group delay and
C                                 phase delay rate.
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDA, ADDR
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/11/77
C                    BRUCE SCHUPLER 09/16/77
C                    SAVITA GOEL    06/03/87 (CDS FOR A900)
C                    Jim Ryan 86.29 Character stings used.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implemented.
C                    David Gordon 94.03.25 AXO2CONT and AXIS_OLD Lcodes added.
C                    David Gordon 94.07.22 Changed 'AXIS_OLD' to 'AXIS OLD' to 
C                                 keep Chopo happy.
C                    David Gordon 98.11.12 Merged in code from subroutine
C                                 PANA for feedbox rotation stuff. 'AXIS OLD'
C                                 contribution removed. 
C                    David Gordon 98.12.30 FEED.COR changed from (2,2) to
C                                 (2,1) dimensions.
C
C 1.3   AXOA PROGRAM STRUCTURE
C
C   ADD for axis offset module text message.
      CALL ADDA (1,'AXO MESS','Axis Offset Message Definition  ',
     *     40, 1, 1 )
C
C   ADD for axis offset module control flag message.
      CALL ADDA (1,'AXO CFLG','Axis Offset Control flag mes def',
     *     40,1,1)
C
C   ADD for axis offset partial derivatives.
      CALL ADDR (2,'AXO PART','Axis Offset partial deriv. def. ',
     *     2, 2, 1 )
C
C   ADD for new axis offset contributions (one-term formula).
      CALL ADDR (2,'AXO CONT','New Axis Offset Contributions   ',
     *     2, 2, 1 )
C
C   ADD for correction to convert to the alternate new axis offset
C       (three-term formula).
      CALL ADDR (2,'AXO2CONT','Other new axis offset correction',
     *     2, 1, 1 )
C
C   Delete for old axis offset correction.
      CALL DELR (2,'AXIS OLD')
C   ADD for correction to convert back to the old (pre Calc 8.0) axis offset.
c     CALL ADDR (2,'AXIS OLD','Old axis offset correction      ',
c    *     2, 1, 1 )
C
C     ADDA for feedbox rotation (paralactic angle) text message.
      CALL ADDA (1,'PAN MESS','Feedhorn rot. angle mod. ident. ',
     *     40, 1, 1 )
C
C     ADD for feedbox rotation angle (paralactic angle).
      CALL ADDR (2,'PARANGLE','Feedhorn rot. angle, ref and rem',
     *     2, 1, 1 )
C
C     ADD for feedbox corrections for standard observables.
      CALL ADDR (2,'FEED.COR','Feedhorn corr. in CORFIL scheme ',
     *     2, 1, 1 )
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE AXOI
      IMPLICIT None
C
C 2.    AXOI
C
C 2.1   AXOI PROGRAM SPECIFICATION
C
C 2.1.1 AXOI is the Axis Offset and Feedbox Rotation Module input and 
C       initialization section.
C
C 2.2   AXOI PROGRAM INTERFACE
C
C 2.2.2 COMMON BLOCKS USED -
C
       INCLUDE 'ccon.i'
C      VARIABLES 'FROM'
C        1. KAXOC - THE AXIS OFFSET MODULE FLOW CONTROL FLAG
C                   = 0 ==> Axis offset corrections applied to theoreticals.
C                   = 1 ==> Axis offset corrections NOT applied to theoreticals.
C        1.  KPANC  -  The Feedbox Rotation flow control flag.
C
C 2.2.3 PROGRAM SPECIFICATIONS -
      INTEGER*2       LAXOM(40),      LON(40),    LOFF(40)
      CHARACTER*40  C_LAXOM(2),     C_LON(2),   C_LOFF(2)
      EQUIVALENCE(  C_LAXOM,LAXOM),(C_LON,LON),(C_LOFF,LOFF)
      INTEGER*2      LPANG_ON(40),  LPANG_OF(40)
      CHARACTER*40 C_LPANG_ON(2), C_LPANG_OF(2)
      EQUIVALENCE (  LPANG_ON, C_LPANG_ON)
      EQUIVALENCE (  LPANG_OF, C_LPANG_OF)
C
      DATA C_LAXOM /
     .'Axis Offset Module - Last modified 98DEC',
     .'30, D. Gordon/GSFC.                    '/
      DATA C_LON  /
     .'Axis Offset Module is turned ON in CALC.',
     .'                                        '/
      DATA C_LOFF /
     .'Axis Offset Module is turned OFF in CALC',
     .'.                                       '/
C
      DATA C_LPANG_ON  /
     .'Feedbox Rotation Angle Module, Last Modi',
     .'fied 98NOV12, D. Gordon/GSFC.           '/
      DATA C_LPANG_OF  /
     .'Feedbox Rotation angle zeroed out.      ',
     .'                                        '/
C
C 2.2.4 DATA BASE ACCESS -
C           'PUT' VARIABLES:
C              1. LAXOM(40)    - THE AXIS OFFSET MODULE TEXT MESSAGE.
C              2. LON(40)      - THE AXIS OFFSET MODULE TURNED ON MESSAGE
C              3. LOFF(40)     - THE AXIS OFFSET MODULE TURNED OFF MESSAGE.
C              4. LPANG_ON(40) - The feedbox rotation (parallactic) angle
C                                text message.
C              5. LPANG_OF(40) - The feedbox rotation (parallactic) angle
C                                turned OFF message.
C           ACCESS CODES PUT:
C              1. 'AXO MESS'   -  THE DATA BASE ACCESS CODE FOR THE
C                                 AXIS OFFSET MODULE TEXT MESSAGE.
C              2. 'AXO CFLG'   -  THE DATA BASE ACCESS CODE FOR THE AXIS
C                                 OFFSET MODULE FLOW CONTROL MESSAGE.
C              3. 'PAN MESS'   -  The data base access code for the feedbox 
C                                 rotation text message.
C 
C 2.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL
C             CALLED SUBROUTINES: PUTA
C
C 2.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/11/77
C                    BRUCE SCHUPLER 03/07/78
C                    Jim Ryan 89.06.29 Character stings used.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implemented.
C                    D. Gordon 98.11.12 Merged in code from subroutine PANI,
C                             the feedbox rotation (parallactic) angle
C                             initialization routine.
C
C 2.3   AXOI PROGRAM STRUCTURE
C
C     Put Axis Offset Module text message in database
      CALL PUTA ('AXO MESS      ', LAXOM, 40, 1, 1 )
C
C     Put Axis Offset Module flow control message into database header.
      IF (KAXOC .NE. 1) CALL PUTA('AXO CFLG      ',LON,40,1,1)
      IF (KAXOC .EQ. 1) CALL PUTA('AXO CFLG      ',LOFF,40,1,1)
C
C     PUT for the feedbox rotation (paralactic) angle routine text message.
      IF (KPANC .NE. 1) CALL PUTA ('PAN MESS      ', LPANG_ON,40,1,1)
      IF (KPANC .EQ. 1) CALL PUTA ('PAN MESS      ', LPANG_OF,40,1,1)
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE AXOG (KAXIS, R2000, SITLAT, STAR, TCTOCF, SITEV, AXOFF,
     *           EARTH, AZ, ELEV, STAR_ABERRATED,
     *           SITHEIGHT, axis2000, daxis2000 )
      IMPLICIT None
C
C 3.    AXOG
C
C 3.1   AXOG PROGRAM SPECIFICATION
C
C 3.1.1 AXOG is the geometry section of the Axis Offset Module. AXOG computes
C       the 3 dimensional axis offset vector and its time derivative corrected
C       for aberration and atmospheric refraction at each site.
C       Beginning with Calc 9.0, AXOG also computes the feedbox rotation
C       angle and the group delay and phase delay corrections, using a new
C       algorithm that replaces subroutine PANG.
C
C 3.1.2 REFERENCES - NINER, EDGAR P., 'MEMORANDUM FOR RECORD', 25 OCTOBER
C                    1967 (REFRACTION CORRECTION).
C                    SMART, W.M., "TEXTBOOK ON SPHERICAL ASTRONOMY", 1965,
C                    P. 68.
C                    Gordon, David, "Axis offset VLBI delay correction", 
C                    1994 March 7 memo, updated 1994 May 5.
C
C 3.2   AXOG PROGRAM INTERFACE
C 
C 3.2.1 CALLING SEQUENCE -
C          INPUT VARIABLES:
C             1. KAXIS(2)       -  THE ANTENNA AXIS TYPES AT EACH OBSERVATION
C                                  SITE. (UNITLESS)
C             2. R2000(3,3,3)   -  THE ROTATION MATRIX WHICH ROTATES THE CRUST
C                                  FIXED REFERENCE FRAME TO THE J2000.0 FRAME 
C                                  AND ITS FIRST TWO CT TIME DERIVATIVES. 
C                                  (UNITLESS, 1/SEC, 1/SEC**2)
C             3. SITLAT(2)      -  THE GEODETIC LATITUDES AT EACH OBSERVATION
C                                  SITE. (RAD) 
C             4. STAR(3)        -  THE J2000.0 SOURCE UNIT VECTOR. (UNITLESS) 
C             5. TCTOCF(3,3,2)  -  THE ROTATION MATRIX WHICH ROTATES THE
C                                  TOPOCENTRIC REFERENCE SYSTEM TO THE CRUST
C                                  FIXED GEOCENTRIC REFERENCE SYSTEM AT EACH 
C                                  OBSERVATION SITE. (UNITLESS)
C             6. SITEV(3,2)     -  THE J2000.0 GEOCENTRIC VELOCITY VECTORS OF
C                                  EACH OBSERVATION SITE. (M/SEC)
C             7. AXOFF(2)       -  THE ANTENNA AXIS OFFSETS AT EACH OBSERVATION
C                                  SITE.  (M)
C             8. EARTH(3,3)     -  THE SSBC EARTH POSITION, VELOCITY, AND 
C                                  ACCELERATION VECTORS. (M, M/SEC, M/SEC**2)
C             9. ELEV(2,2)      -  The elevation angle of the source corrrected
C                                  for aberration and its CT time derivative at
C                                  each site (rad,rad/sec)
C            10. AZ(2,2)        -  The azimuth angle of the source corrrected
C                                  for aberration and its CT time derivative
C                                  at each site (rad,rad/sec)
C            11. STAR_ABERRATED(3,2) - The J2000 source unit vector corrected
C                                  for annual and diurnal aberration at each 
C                                  site. (unitless)
C            12. SITHEIGHT(2)   -  Station heights above the geoid. (meters)
C          OUTPUT VARIABLES:
C             1. axis2000(3,2) -  Vector axis offset of antenna in the J2000.0
C                                 frame (effect on baseline). First index is
C                                 X,Y,Z (meters), second runs over sites.
C             2. daxis2000(3,2) - Time derivative of axis2000, rate of change
C                                 of vector axis offset of antenna in the
C                                 J2000.0 frame (effect on baseline). First
C                                 index is velocity, second runs over sites.
C
C 3.2.2 COMMON BLOCKS USED -
C 
      Real*8  DAXOP(2,2), DCOMP(2,2), SURREF, RICHM(2), DCOMP_new(2,2),
     .        udel_h(2,2),DAXOP_new(2,2), Daxop_newer(2,2)
      COMMON / AXOCM / DAXOP, DCOMP, SURREF, RICHM, DCOMP_new, 
     .        udel_h, DAXOP_new, Daxop_newer
C         VARIABLES 'TO':
C            1. DCOMP(2,2) - THE PARTIAL DERIVATIVES OF THE COMPONENT OF THE
C                            ANTENNA AXIS OFFSET IN THE APPARENT DIRECTION OF
C                            THE SOURCE AND THE CT TIME DERIVATIVE OF THAT
C                            COMPONENT WITH RESPECT TO THE ANTENNA AXIS
C                            OFFSETS AT EACH OBSERVATION SITE. THE FIRST
C                            INDEX RUNS OVER THE OBSERVATION SITES AND THE
C                            SECOND INDEX RUNS OVER THE DELAY AND THE DELAY
C                            RATE.  (UNITLESS, 1/SEC) 
C            2. SURREF     - THE SURFACE REFRACTIVITY AT RADIO WAVELENGTHS
C                            IN N UNITS (N = (1 - REFRACTIVITY) * 10**6)
C                            (SEE REFERENCES) 
C            3. RICHM(2)   - THE AXES ORIENTATION FOR THE RICHMOND ANTENNA
C                            SYSTEM. IT IS ORIENTED AS FOR AN EQUATORIAL
C                            MOUNT AT LATITUDE+39.06 DEGREES AND ROTATED 0.12
C                            DEGREES WEST OF NORTH.
C            4. udel_h(2,2)- Up component of the topocentric unit vector axis
C                            offset and its derivative. First index runs
C                            over the sites, second runs over height and 
C                            its time derivative. (meters, meters/sec)
C            5. DCOMP_new(2,2)-The first two parts of the partial derivative of
C                            the new axis offset delay and rate with respect
C                            to AXOFF. First index runs over the sites, second
C                            runs over delay and rate. (sec/m, sec/sec/m)
C
      INCLUDE 'cphys.i'               
C           VARIABLES 'FROM':
C              1. VLIGHT  -  The velocity of light in a vacuum (m/sec).
C 
      INCLUDE 'ccon.i'               
C       VARIABLES 'FROM':
C         KAXOC - THE AXIS OFFSET MODULE FLOW CONTROL FLAG.
C                  = 0 ==> Axis offset corrections applied to theoreticals.
C                  = 1 ==> Axis offset corrections NOT applied to theoreticals.
C         KAXOD - THE AXIS OFFSET MODULE DEBUG OUTPUT FLAG.
C         KPANC - The feedhorn rotation module flow control flag.
C         KPAND - The feedhorn rotation module debug flag (not used).
C
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY 
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY 
      EXTERNAL CMATHB
C           VARIABLES 'FROM':
C              1. HALFPI - THE VALUE OF PI/2
C              2. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS
C                          (RAD/DEG)
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.
C       DATA BASE ACCESS -
C            'PUT' VARIABLES:
C               1. PANGL(2) - Feedbox rotation angles at site 1 and 2 in degrees
C                             of phase.
C             ACCESS CODES:
C               1. 'PARANGLE' - The database access code for the feedbox 
C                               rotation angles.
C               2. 'FEED.COR' - The database access code for the feedbox
C                               rotation correction to the group delay and 
C                               the phase delay rate.
C 
C 3.2.3 PROGRAM SPECIFICATIONS -
      DATA  SURREF / 313.0D0/ 
      DATA RICHM /39.06D0, 0.12D0/  
      Integer*2 KAXIS(2), NDO(3), KERR
      Integer*4 L, LL, kx  
      Real*8 CFSTAR(3,2), CFTOTC(3,3), CTCSTR(3), SITEV(3,2), 
     1       R2000(3,3,3), SITLAT(2), STAR(3), TCAXIS(3), 
     2       TCSTAR(3,2), TCTOCF(3,3,2), TR2000(3,3,2),
     3       AZ(2,2), ELEV(2,2), STAR_ABERRATED(3,2), SITHEIGHT(2),
     4       ZC, ZEN, TANEL, AZMUTH, ZENCOR, REF_FREQ
      Real*8 axistc(3,2), daxistc(3,2), axiscf(3,2), daxiscf(3,2), 
     *       AXOFF(2), xnormal, ccfstr(3), dccfstr(3), vectr(3),
     *       daxocnew(2,2), EARTH(3,3), c2000str(3), axis2000(3,2),
     *       daxis2000(3,2), dctcstr(3), vec1(3), dvec1(3), absvec1,
     *       dabsvec1, vec2(3), vec3(3), uaxis2000(3,2),duaxis2000(3,2)
      Real*8 VECMG, DOTP, AMAG
      Real*8 zen_K, tanel_K, azmuth_K, zencor_K
      Real*8 term1, dterm1, sinEZ, dsinEZ, DotDot1, dDot1Dot1,
     *       term2, dterm2
      Real*8 El_Rad, Sithit, Temp_K, X, Press_Hg, Humid_F, sbend
      Real*8 TNCP(3), ZA, DZA, DAZM, RANGL, DRANGL, PANGL(2), 
     *       FEED_COR(2,2), FCONT(2)
C 
C 3.2.6 SUBROUTINE INTERFACE -
C          CALLER SUBROUTINES: DRIVG 
C          CALLED SUBROUTINES: DCOS, DSIN, DTAN, MTRAN, VECRT, VECMG, VECAD,
C                              DOTP, CROSP, VECAD, FBOX
C 
C 3.2.8 PROGRAM VARIABLES - 
C           1. TR2000(3,3,2)  -  THE COMPLETE J2000.0 TO CRUST FIXED
C                                ROTATION MATRIX AND ITS FIRST CT
C                                TIME DERIVATIVE. (UNITLESS, 1/SEC)
C           2. CFTOTC(3,3)    -  THE 3x3 ROTATION MATRIX WHICH ROTATES 
C                                THE GEOCENTRIC CRUST FIXED REFERENCE
C                                SYSTEM TO THE TOPOCENTRIC REFERENCE 
C                                SYSTEM. (UNITLESS) 
C           3. CFSTAR(3,2)    -  THE SOURCE UNIT VECTOR IN THE CRUST FIXED
C                                GEOCENTRIC REFERENCE SYSTEM CORRECTED FOR
C                                ABERRATION AND ITS CT TIME DERIVATIVE.
C                                (UNITLESS, 1/SEC)
C           4. TCSTAR(3,2)    -  THE SOURCE UNIT VECTOR IN THE TOPOCENTRIC
C                                SYSTEM CORRECTED FOR ABERRATION AND ITS CT 
C                                TIME DERIVATIVE. (UNITLESS, 1/SEC)
C           5. CTCSTR(3)      -  THE SOURCE UNIT VECTOR IN THE TOPOCENTRIC
C                                REFERENCE SYSTEM CORRECTED FOR ABERRATION
C                                AND ATMOSPHERIC REFRACTION. (UNITLESS)
C           6. dctcstr(3)     -  Derivative of CTCSTR (above).
C           7. ccfstr(3)      -  Aberrated/refracted source unit vector in the
C                                crust fixed frame.
C           8. dccfstr(3)     -  Time derivative of ccfstr (above).
C           9. c2000str(3)    -  Aberrated/refracted source unit vector in the
C                                J2000.0 frame.
C          10. AZMUTH         -  THE AZIMUTH OF THE TOPOCENTRIC SOURCE UNIT
C                                VECTOR CORRECTED FOR ABERRATION. (RAD) 
C          11. TANEL          -  THE TANGENT OF THE ELEVATION ANGLE,
C                                CORRECTED FOR ABERRATION. (UNITLESS)
C          12. ZEN            -  THE ZENITH ANGLE OF THE SOURCE UNIT VECTOR.
C                                (RADIANS)
C          13. ZENCOR         -  THE CORRECTION ANGLE TO THE SOURCE UNIT VECTOR
C                                DUE TO ATMOSPHERIC REFRACTION. (RADIANS)
C          14. TCAXIS(3)      -  THE UNIT VECTOR REPRESENTING THE ANTENNA FIXED
C                                AXIS IN THE TOPOCENTRIC SYSTEM. (UNITLESS)
C          15. XI             -  THE ANGLE BETWEEN THE FIXED ANTENNA AXIS AND
C                                THE SOURCE UNIT VECTOR CORRECTED FOR 
C                                ABERRATION AND REFRACTION. (RADIANS)
C          16. axistc(3,2)    -  Vector axis offset of the antenna in the
C                                topocentric frame (effect on baseline). First
C                                index is X,Y,Z position (meters); second runs
C                                over the sites.
C          17. daxistc(3,2)   -  Time derivative of axistc, rate of change of
C                                vector axis offset of antenna in the
C                                topocentric frame (effect on baseline). First
C                                index is velocity, second runs over sites.
C          18. axiscf(3,2)    -  Vector axis offset of the antenna in the
C                                geocentric frame (effect on baseline). First
C                                index is X,Y,Z position (meters); second runs
C                                over the sites.
C          19. daxiscf(3,2)   -  Time derivative of axiscf, rate of change of
C                                vector axis offset of the antenna in the
C                                geocentric frame (effect on baseline). First
C                                index is velocity, second runs over sites.
C          20. uaxis2000(3,2) -  Unit vector axis offset of antenna in the 
C                                J2000.0 frame. First index is X,Y,Z, second
C                                runs over sites. (unitless) [See axis2000]
C          21. duaxis2000(3,2) - Time derivative of uaxis2000, rate of change
C                                of unit vector axis offset in the J2000.0
C                                frame. First index is velocity, second runs
C                                over sites. (unitless) [See daxis2000]
C          22. El_Rad          - Source elevation for use by function Sbend to
C                                compute the atmospheric bending. (radians
C          23. Sithit          - Station height above the geoid, for use in 
C                                computing atmospheric bending. (meters) 
C          24. Temp_K          - A priori value for absolute temperature, for
C                                use in computing atmospheric bending. (Kelvins)
C          25. Press_Hg        - A priori value for atmospheric pressure, for 
C                                use in computing atmospheric bending. (mm of 
C                                mercury)
C          26. Humid_F         - A priori value for station humidity (.5).
C          27. X               - Variable related to station height, for use in
C                                computing atmospheric bending.
C          28. PANGL(2)        - The clock-wise feed box rotation angles for
C                                stations 1 and 2. (Degrees) 
C          29. FEED_COR(2,2)   - The corrections to group delay and phase
C                                delay rate due to feedbox rotation. First
C                                index runs over the sites, second index runs
C                                over delay and rate. Sec, Sec/sec)
C
C 3.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/11/77
C                    DOUG ROBERTSON ??/??/84 (RICHMOND AXIS CODING)
C                    DAVID GORDON   08/14/84 (RICHMOND)
C                    Jim Ryan 89.06.29 Character stings used.
C                    David Gordon  June - Aug, 1993 Modified to compute 3-D
C                                  axis offset vector; Aberrated star vector,
C                                  and altitude and azimuth from subroutine 
C                                  ATMG added; new computation of DCOMP.
C                    David Gordon  93.12.23 Corrected division by zero problem
C                                  in DCOMP.
C                    David Gordon  94.03.08 New axis offset delay computation.
C                    David Gordon  94.03.25 Second (simpler) new axis offset 
C                                  delay computation.
C                    David Gordon  94.09.21 Added function Sbend to compute
C                                  atmospheric bending. Sbend comes from the 
C                                  Mark III Field System. Changed computation 
C                                  of second new axis offset contribution to
C                                  combine aberration and bending. SITHEIGHT(2)
C                                  added to subroutine input arguments.
C                    David Gordon  94.10.20 Changed computation of second axis
C                                  offset correction to apply aberration to
C                                  vacuum part, but not to aberrated/refracted 
C                                  part.
C                    David Gordon  94.10.24 Code cleanup.
C                    David Gordon  98.08.05 Mods for geocenter station.
C                    David Gordon  98.11.12 Code added to call the new feedbox
C                                  rotation subroutine, FBOX, and to do PUT's 
C                                  of the rotation angles and the group delay
C                                  and phase delay rate corrections. Code to
C                                  GET Reference frequency added. 
C                    David Gordon  98.12.30 FEED.COR access code changed from
C                                  a (2,2) array to a (2,1) array - sites 
C                                  1 and 2 combined.
C
C 3.3   AXOG PROGRAM STRUCTURE
C
C   Get the reference frequency for use in the phase delay rate corrections. 
C    Convert from MHz to Hz.
      CALL GET4('REF FREQ      ',REF_FREQ,1,1,1,NDO,KERR)
      IF(KERR.NE.0) then
        write(6,'("AXOG: Failure to obtain ref frequency.")')
        CALL CKILL(6HAXOG  ,1,KERR)
      Endif
      REF_FREQ = REF_FREQ*1.D6
C
C
C    Loop through twice for the calculation of the axis offset vectors and
C    their time derivatives and the partial derivatives of the component of
C    the antenna axis offsets in the apparent direction of the source and the
C    time derivative of that component with respect to the antenna axis
C    offsets at each site.
C
      DO 800  L = 1,2
           LL = L
C
C  Check for geocenter station
      IF (L. eq. Nzero) Go to 750
C
C    Identify the unit vector representing the antenna fixed axis in a
C    topocentric reference system. The topocentric reference system sits on the
C    Earth's surface at the observation site with axes pointing 1) radially Up
C    (X-AXIS), 2) East (Y-axis), and 3) North (Z-axis).
C
C  Identify the antenna axis type in order to obtain the correct algorithm:
C
C    Equatorial mount - fixed axis points at the North Celestial Pole.
         If (KAXIS(L) .EQ. 1) Then
           TCAXIS(1) = DSIN ( SITLAT(L) )
           TCAXIS(2) = 0.D0
           TCAXIS(3) = DCOS ( SITLAT(L) )
           GO TO 310
         Endif
C
C    X-Y mount - antenna fixed axis points North-South. (i.e. the antenna fixed
C      axis is in the plane of the horizon pointed North-South.)
         If (KAXIS(L) .EQ. 2) Then
           TCAXIS(1) = 0.D0
           TCAXIS(2) = 0.D0
           TCAXIS(3) = 1.D0
           GO TO 310
         Endif
C
C    Alt-Az mount - the antenna fixed axis points at the zenith.
         If (KAXIS(L) .EQ. 3) Then
           TCAXIS(1) = 1.D0
           TCAXIS(2) = 0.D0
           TCAXIS(3) = 0.D0
           GO TO 310
         Endif
C
C    X-Y mount -  antenna fixed axis pointing East-West. (i.e. The antenna
C      fixed axis is in the plane of the horizon pointed East-West.)
         If (KAXIS(L) .EQ. 4) Then
           TCAXIS(1) = 0.D0
           TCAXIS(2) = 1.D0
           TCAXIS(3) = 0.D0
           GO TO 310
         Endif
C
C    Old Richmond antenna system - axes oriented as for an equatorial mount at
C      latitude +39.06 degrees and rotated 0.12 degrees West of North.
         If (KAXIS(L) .EQ. 5) Then
           ZC        =     DCOS(RICHM(1)*CONVD)
           TCAXIS(1) =     DSIN(RICHM(1)*CONVD)
           TCAXIS(2) = -ZC*DSIN(RICHM(2)*CONVD)
           TCAXIS(3) =  ZC*DCOS(RICHM(2)*CONVD)
           GO TO 310
         Endif
C
C    Antenna axis type invalid, a message is written and the program stops.
         IF ((KAXIS(L) .le. 0 ) .OR. (KAXIS(L) .gt. 5)) Then
           WRITE ( 6, 9300 )  LL, KAXIS(LL)
           CALL CKILL (6HAXOG  , 0, 0)
         Endif
 9300 FORMAT (1X, "THE PROGRAM HAS TERMINATED IN SUBROUTINE AXOG.  ",
     1            'KAXIS (', I2, ') = ', I2, '.' )
C
C******************************************************************************
C    Rotate the J2000.0 source unit vector to the topocentric reference system.
C
C        Compute the rotation matrix which rotates from the geocentric crust
C        fixed reference system to the topocentric reference system.
  310      CALL MTRAN ( TCTOCF(1,1,L), CFTOTC )
C
C        Compute the rotation matrix which rotates from the J2000.0
C        reference system to the geocentric crust fixed system.
           CALL MTRAN ( R2000(1,1,1), TR2000(1,1,1) )
C
C        Rotate the aberrated J2000.0 source unit vector to the crust fixed
C        system and then to the topocentric system.
           CALL VECRT ( TR2000(1,1,1),STAR_ABERRATED(1,L),CFSTAR(1,1))
           CALL VECRT ( CFTOTC, CFSTAR(1,1), TCSTAR(1,1) )
C
C*****************************************************************************
C    Correct the aberrated topocentric source unit vector for atmospheric
C    refraction. 
C
C   Compute the zenith angle of the aberrated source.
        ZEN =  HALFPI - ELEV(L,1) 
C   Compute the azimuth of the aberrated source.
        AZMUTH = AZ(L,1) 
C   Compute values needed for function sbend.
        El_Rad = elev(l,1)
        Sithit = Sitheight(l)
        Temp_K = 293.15 - (6.5D-3)*sithit
        X = 1.D0 - (6.5D-3)*sithit/293.15D0
        Press_Hg = 760.D0 * (X**5.26D0)
        Humid_F = .5D0
C   Compute atmospheric bending.
        zencor = sbend(El_rad,Temp_K,Humid_F,Press_Hg)
C
C *************************************************************************
C    Compute the corrected (aberrated/refracted) topocentric star unit
C    vector. (NOTE: This corrects the vector so that it is pointing nearer 
C    to the zenith).
          CTCSTR(1) = DCOS ( ZEN - ZENCOR )
          CTCSTR(2) = DSIN ( ZEN - ZENCOR ) * DSIN ( AZMUTH )
          CTCSTR(3) = DSIN ( ZEN - ZENCOR ) * DCOS ( AZMUTH )
C       Make sure we still have a unit vector
          amag = vecmg(CTCSTR)
          do kx = 1,3
           CTCSTR(kx) = CTCSTR(kx)/amag
          enddo
C
C     Compute the CT time derivative of the aberrated source unit vector
C      in the topocentric reference system.
            CALL MTRAN ( R2000(1,1,2), TR2000(1,1,2) )
            CALL VECRT ( TR2000(1,1,2),STAR_ABERRATED(1,L),CFSTAR(1,2))
            CALL VECRT ( CFTOTC, CFSTAR(1,2), TCSTAR(1,2) )
C
C .............................................................................
C   New code for computing the three dimensional components of the axis offset
C    vector.  D. Gordon 93MAY13 thru 93JUL27
C
C  Compute topocentric axis offset vector (change in effective site position
C   due to axis offset orientation) and its time derivative:
C
C   DELAY COMPUTATION:
C   ------------------
C    Take triple vector product to get direction of axis offset vector: 
        call crosp(TCAXIS,CTCSTR,vectr)
        call crosp(vectr,TCAXIS,vec1)
C    Compute magnitude of vector so we can normalize it
       absvec1 = vecmg(vec1)
C    Normalize to get topocentric unit axis offset vector
       do kx=1,3
        axistc(kx,L) = vec1(kx) / absvec1
       enddo
C
C    Rotate axis offset vector to the crust fixed frame
        call vecrt(TCTOCF(1,1,L),axistc(1,L),axiscf(1,L))
C    Then rotate axis offset vector to the J2000 frame
        call vecrt(R2000(1,1,1),axiscf(1,L),uaxis2000(1,L))
C
C   Derivative of CTCSTR: (aberrated/refracted topocentric source unit vector)
C   --------------------
C     Rotate atmosphere corrected aberrated source unit vector to the
C     crust fixed frame:
        call vecrt(TCTOCF(1,1,L),CTCSTR,ccfstr)
C    Then rotate to the J2000 frame:
        call vecrt(R2000(1,1,1),ccfstr,c2000str)
C
C    Take time derivative and rotate back to crust fixed frame
        call vecrt(TR2000(1,1,2),c2000str,dccfstr)
C
C    Rotate source vector time derivative back to topocentric frame
        call vecrt(CFTOTC,dccfstr,dctcstr)
C
C   RATE COMPUTATION:
C   -----------------
C    Take derivative of triple vector product (TCAXIS is a constant)
        call crosp(TCAXIS,dctcstr,vectr)
        call crosp(vectr,TCAXIS,dvec1)
C    Derivative of magnitude of vec1
        dabsvec1 = Dotp(vec1,dvec1) / absvec1
C    Compute derivative of unit axis offset vector in topocentric frame
       do kx=1,3
        daxistc(kx,L) = dvec1(kx)/absvec1 -
     .   vec1(kx) * dabsvec1 / absvec1**2 
       enddo
C
C    Rotate axis offset time derivative to crust fixed frame
        call vecrt(TCTOCF(1,1,L),daxistc(1,L),daxiscf(1,L))
C
C    Then rotate to J2000 frame:
        call vecrt(R2000(1,1,1),daxiscf(1,L),vec2)
        call vecrt(R2000(1,1,2), axiscf(1,L),vec3)
        call vecad(vec2,vec3,duaxis2000(1,L))
C
C    Compute partial derivative of the components of the antenna axis offset
C     in the apparent direction of the source with respect to the antenna axis
C     offset.
        DCOMP(L,1) = Dotp(c2000str,uaxis2000(1,L))
        DCOMP(L,2) = Dotp(c2000str,duaxis2000(1,L))
C
C   Convert unit axis offset vector and its derivative to actual axis offset
C     vector and its derivative.
       do kx=1,3
         axis2000(kx,L) =  uaxis2000(kx,L) * AXOFF(L)
        daxis2000(kx,L) = duaxis2000(kx,L) * AXOFF(L)
       enddo
C
C   For use later in AXOP, save the topocentric UP (change in height) component
C    of the unit axis offset vector and its time derivative
       udel_h(L,1) =  axistc(1,L)    ! unit UP component
       udel_h(L,2) = daxistc(1,L)    ! unit UP time derivative
C
C .............................................................................
C  Now compute new axis offset partial with respect to AXOFF (3-part formula,
C    see D. Gordon reference)
C
        sinEZ = DSIN(ELEV(L,1)+zencor)
       dsinEZ = DCOS(ELEV(L,1)+zencor) * ELEV(L,2)
C
C   Vacuum part 1:
C!      term1 = -Dotp( uaxis2000(1,L),Star)
C!     dterm1 = -Dotp(duaxis2000(1,L),Star)
C   Vacuum part 1 with aberration correction:
        term1 = -Dotp( uaxis2000(1,L),Star_aberrated(1,L))
       dterm1 = -Dotp(duaxis2000(1,L),Star_aberrated(1,L))
C
C   Vacuum part 2 with aberration correction and refraction/geometric part:
c       DotDot1 = Dotp(Star,c2000str) - 1.D0
C      dDotDot1 = 0.D0  ! approximation
        DotDot1 = Dotp(Star_aberrated(1,L),c2000str) - 1.D0
        term2 =  axistc(L,1) * DotDot1 / sinEZ
       dterm2 = daxistc(L,1) * DotDot1 / sinEZ - 
     *           axistc(L,1) * DotDot1 * dsinEZ / sinEZ**2
C
C   Now combine everything to get the first two parts of the new axis 
C    offset delay and rate partials with respect to AXOFF. (The last part, the
C    additional atmosphere correction to the reference height is done in AXOP.)
       DCOMP_new(L,1) = (term1 + term2) / VLIGHT 
       DCOMP_new(L,2) = (dterm1 + dterm2) / VLIGHT
C******************************************************************************
C
C          Now do feedbox rotation computations
C
        IF(KPANC.EQ.1) THEN
          PANGL(L)      = 0.0D0      
          FEED_COR(L,1) = 0.0D0
          FEED_COR(L,2) = 0.0D0 
          GO TO 156
         ENDIF  
C
C  Define vector TNCP(3), a unit vector in the direction of the North 
C   Celestial pole.
           TNCP(1) = DSIN(SITLAT(L))
           TNCP(2) = 0.D0
           TNCP(3) = DCOS(SITLAT(L))
C
        ZA = ZEN - ZENCOR
        DZA = -ELEV(L,2)
        DAZM = AZ(l,2)
       If (KAXIS(L) .NE. 1) Then
        CALL FBOX (TNCP, TCAXIS, CTCSTR, AZMUTH, DAZM, ZA, DZA, 
     *            Rangl, dRangl)
       Else
C           Equatorial Mount, no feedbox rotation
        Rangl = 0.0D0
        dRangl = 0.0D0
       Endif
C
C  Convert the feedbox rotation angle to degrees for this site. 
       PANGL(L) = RANGL/CONVD
C  Compute the feedbox rotation corrections for group delay and phase delay
C   rate for this site. The units are seconds and sec/sec.
       FEED_COR(L,1) = 0.D0
       FEED_COR(L,2) = DRANGL/TWOPI/REF_FREQ 
C
C    To correct the phases, either ADD the quantity [PANGL(2) - PANGL(1)]
C     to the total or residual observed phases, or SUBTRACT it from the 
C     theoretical (model) phases.
C    Note that the group delay corrections are always zero.
C    To correct the phase delay rates, either ADD the quantity 
C     [FEED_COR(2,2) - FEEDCOR(1,2)] to the observed rates, or SUBTRACT
C     it from the theoretical (model) rates.
C
 156   CONTINUE
C******************************************************************************
C     See if debug is needed.
      IF ( KAXOD .ne. 0 )  Then
C
      If (L.eq.1) then 
        WRITE ( 6, 9100 )
      endif
C
      If (L.eq.2) then 
        WRITE ( 6, 9101 )
        WRITE ( 6, 9200 )  KAXIS, R2000, SITLAT, STAR, TCTOCF
        WRITE(6,8)' TR2000  ',TR2000
        WRITE(6,8)' STAR_ABERRATED ',STAR_ABERRATED
        WRITE(6,8)' AXOFF   ', AXOFF
        WRITE(6,8)' PANGL ', PANGL
        WRITE(6,8)' FEED_COR  ', FEED_COR
      endif
C
 9100 FORMAT (20X, "Debug output for subroutine AXOG." )
 9200   FORMAT (1X, "KAXIS  = ", 2 ( I2, 10X ), /, 1X,
     1   "R2000  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ), /, 1X,
     2   "SITLAT = ", 2 ( D30.16, 10X ), /, 1X,
     3   "STAR   = ", 3 ( D30.16, 4X ), /, 1X,
     4   "TCTOCF = ", 6 ( 3 ( D30.16, 10X ), /, 1X ) )
 9101 FORMAT (1X, "Dump for both stations:          " )
    8 FORMAT(A,3D25.16,/,5(7X,3D25.16))
C
      write(6,'(/,10x," ******** DUMP FOR SITE",i2," ******** ")') L
      WRITE(6,8)' TCAXIS  ',TCAXIS
      WRITE(6,8)' CFTOTC  ',CFTOTC
      WRITE(6,8)' CFSTAR  ',CFSTAR
      WRITE(6,8)' TCSTAR  ',TCSTAR
      WRITE(6,8)' SURREF  ',SURREF
      WRITE(6,8)' ZEN     ',ZEN
      WRITE(6,8)' TANEL   ',TANEL
      WRITE(6,8)' AZMUTH  ',AZMUTH
      WRITE(6,8)' ZENCOR  ',ZENCOR
      write(6,'("vec1:  ",3D23.14)') vec1
      write(6,'("dvec1: ",3D23.14)') dvec1
      write(6,'("absvec1: ",3D23.14)') absvec1
      write(6,'("dabsvec1: ",3D23.14)') dabsvec1
      write(6,'("axistc:  ",3D23.14)') axistc(1,L),axistc(2,L)
     *                                ,axistc(3,L)
      write(6,'("daxistc: ",3D23.14)') daxistc(1,L), daxistc(2,L), 
     *                                 daxistc(3,L) 
      write(6,'("axiscf:  ",3D23.14)') axiscf(1,L),axiscf(2,L)
     *                                ,axiscf(3,L)
      write(6,'("daxiscf: ",3D23.14)') daxiscf(1,L), daxiscf(2,L)
     *                                ,daxiscf(3,L)
      write(6,'("c2000str:  ",3D23.14)') c2000str
      write(6,'("ccfstr:  ",3D23.14)') ccfstr
      write(6,'("dccfstr:  ",3D23.14)') dccfstr
      write(6,'("CTCSTR:  ",3D23.14)') CTCSTR
      write(6,'("dctcstr:  ",3D23.14)') dctcstr
      write(6,'("uaxis2000:",3D23.14)') uaxis2000(1,L),uaxis2000(2,L)
     *                                ,uaxis2000(3,L) 
      write(6,'("duaxis2000:",3D23.14)') duaxis2000(1,L),duaxis2000(2,L)
     *                                 ,duaxis2000(3,L)
      write(6,'("axis2000:",3D23.14)') axis2000(1,L),axis2000(2,L)
     *                                ,axis2000(3,L) 
      write(6,'("daxis2000:",3D23.14)') daxis2000(1,L),daxis2000(2,L)
     *                                 ,daxis2000(3,L)
      write(6,'("DCOMP:   ",2D23.14)') DCOMP(L,1),DCOMP(L,2)
      write(6,'("DCOMP_new",2D23.14)') DCOMP_new(L,1),DCOMP_new(L,2)
      WRITE(6,8)' SinEZ   ', SinEZ
      WRITE(6,8)' term1, dterm1 ', term1, dterm1 
      WRITE(6,8)' DotDot1 ', DotDot1
      WRITE(6,8)' term2, dterm2 ', term2, dterm2 
C
      ENDIF
       Go to 800
C
  750 CONTINUE
C   Handling of geocenter station, set axis offset quantities to zero.
       do kx=1,3
         axis2000(kx,L) = 0.0D0
        daxis2000(kx,L) = 0.0D0
       enddo
        DCOMP(L,1) = 0.0D0 
        DCOMP(L,2) = 0.0D0 
        DCOMP_new(L,1) = 0.0D0
        DCOMP_new(L,2) = 0.0D0 
        udel_h(L,1) = 0.0D0 
        udel_h(L,2) = 0.0D0 
C    Ditto for the feedbox rotation quantities.
       PANGL(L)      = 0.0D0      
       FEED_COR(L,1) = 0.0D0
       FEED_COR(L,2) = 0.0D0 
C
C     Close loop running over the observing sites.
  800 CONTINUE
C
C  PUT the feedbox rotation angles and delay/rate corrections in the database.
C       FCONT(1) = FEED_COR(1,1) - FEEDCOR(2,1)
        FCONT(1) = 0.D0 
        FCONT(2) = FEED_COR(1,2) - FEED_COR(2,2)
      CALL PUT4 ('PARANGLE      ', PANGL, 2, 1, 1 )
C     CALL PUT4 ('FEED.COR      ', FEED_COR,2,2,1)
      CALL PUT4 ('FEED.COR      ', FCONT,2,1,1)
C
      IF ( KAXOD .ne. 0 )  Then
       WRITE(6,'(" KAXIS, PANGL ",2I3,2F10.2)')  KAXIS, PANGL
       WRITE(6,'(" FEED_COR ",4D20.10)')  FEED_COR
       WRITE(6,'(" FCONT    ",2D20.10)')  FEED_COR
      ENDIF
C
C     Normal conclusion.
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE AXOP ( AXOFF, dATMCdh )
      IMPLICIT None 
C
C 5.    AXOP
C
C 5.1   AXOP PROGRAM SPECIFICATION
C
C 5.1.1 AXOP is the partial derivatives section of the axis offset
C       module. AXOP computes the partial derivatives of the delay
C       and rate with respect to the antenna axis offsets at the sites.
C
C 5.2   AXOP PROGRAM INTERFACE
C
C 5.2.1 CALLING SEQUENCE -
C       INPUT VARIABLES:
C          1. AXOFF(2)     - THE ANTENNA AXIS OFFSETS AT EACH SITE. (M)
C          2. dATMCdh(2,2) - Partial derivatives of the Niell dry 
C                            atmosphere contribution with respect to
C                            station height. First index runs over the
C                            sites and the second over delay and rate.
C                            (sec/meter, sec/sec/meter)
C
      Real*8  AXOFF(2), dATMCdh(2,2)
C
C 5.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C         VARIABLES 'FROM':
C            1.  VLIGHT - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C
      Real*8  DAXOP(2,2), DCOMP(2,2), SURREF, RICHM(2), DCOMP_new(2,2),
     .        udel_h(2,2),DAXOP_new(2,2), Daxop_newer(2,2)
      COMMON / AXOCM / DAXOP, DCOMP, SURREF, RICHM, DCOMP_new, 
     .        udel_h, DAXOP_new, Daxop_newer
C         VARIABLES 'FROM': 
C            1. DCOMP(2,2)    -  THE PARTIAL DERIVATIVES OF THE COMPONENT OF
C                                THE ANTENNA AXIS OFFSET IN THE APPARENT
C                                DIRECTION OF THE SOURCE AND THE CT TIME
C                                DERIVATIVE OF THAT COMPONENT WITH RESPECT TO
C                                THE ANTENNA AXIS OFFSETS AT EACH SITE. THE
C                                FIRST INDEX RUNS OVER THE OBSERVATION SITES
C                                AND THE SECOND INDEX RUNS OVER THE DELAY AND
C                                THE DELAY RATE. (UNITLESS, 1/SEC) 
C            2. DCOMP_new(2,2) - The first two parts of the partial derivative 
C                                of the new axis offset delay and rate with 
C                                respect to AXOFF. First index runs over the
C                                sites, second runs over delay and rate.
C                                (sec/m, sec/sec/m)
C            3. udel_h(2,2)    - Up component of the topocentric unit vector 
C                                axis offset and its derivative. First index 
C                                runs over the sites, second runs over height 
C                                and its time derivative. (meters, meters/sec)
C         VARIABLES 'TO': 
C            1. DAXOP(2,2)     - THE PARTIAL DERIVATIVES OF THE DELAY AND THE
C                                DELAY RATE WITH RESPECT TO THE ANTENNA AXIS 
C                                OFFSETS AT EACH SITE (old model). FIRST INDEX
C                                RUNS OVER THE OBSERVATION SITES AND THE
C                                SECOND RUNS OVER THE DELAY AND DELAY RATE.
C                                (SEC/M, SEC/SEC-M) 
C            2. DAXOP_new(2,2) - The partial derivatives of the delay and delay
C                                rate with respect to the antenna axis offsets
C                                at each site using the new 3-term model. First
C                                index runs over sites, second over delay and 
C                                rate. (sec/m, sec/sec/m)
C            3. DAXOP_newer(2,2)-The partial derivatives of the delay and delay
C                                rate with respect to the antenna axis offsets
C                                at each site using the new 1-term model. First
C                                index runs over sites, second over delay and 
C                                rate. (sec/m, sec/sec/m)
C
      INCLUDE 'ccon.i'             
C         VARIABLES 'FROM': 
C            1. KAXOC  -  THE AXIS OFFSET MODULE FLOW CONTROL FLAG. 
C            2. KAXOD  -  THE AXIS OFFSET MODULE DEBUG OUTPUT FLAG. 
C
      Common /OBS/N_air, DAXOC_new, DAXOC_newer
      Real*8 N_air(2), DAXOC_new(2,2), DAXOC_newer(2,2)
C 
C 5.2.3 PROGRAM SPECIFICATIONS -
C 
C 5.2.4 DATA BASE ACCESS -
C       'PUT' VARIABLES:
C          1. Daxop_newer(2,2)-THE PARTIAL DERIVATIVES OF THE DELAY AND THE
C                              DELAY RATE WITH RESPECT TO THE ANTENNA AXIS
C                              OFFSETS AT EACH SITE (modified for new model).
C                              THE FIRST INDEX RUNS OVER THE OBSERVATION SITES
C                              AND THE SECOND RUNS OVER THE DELAY AND DELAY
C                              RATE. (SEC/M, SEC/SEC-M) 
C       ACCESS CODES: 
C          1.  'AXO PART'  -  THE DATA BASE ACCESS CODE FOR THE AXIS OFFSET
C                             MODULE PARTIAL DERIVATIVES ARRAY. 
C 
C 5.2.6 SUBROUTINE INTERFACE -
C          CALLER SUBROUTINES: DRIVP
C          CALLED SUBROUTINES: PUT4
C
C 5.2.7 CONSTANTS USED - VLIGHT
C
C 5.2.8 PROGRAM VARIABLES 
      Real*8 uatm1, duatm1, uatm2, duatm2 
      Integer*4 K
C
C 5.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/11/77
C                    BRUCE SCHUPLER 03/07/78
C                    Jim Ryan 89.06.29 Character stings used.
C                    Jim Ryan 89.10.05 CPHYS common now an include file.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    D. Gordon 94.03.25 Old partials multiplied by index of
C                              refraction and PUT into database (new, simple 
C                              axis offset model). Added complete partials for
C                              more complex (3 terms) axis offset model. 
C                    D. Gordon 94.08.03 Corrected old axis multiplication by
C                              N_air(site #). Was incorrrectly multiplying both
C                              delays by site 1 and both rates by site 2 
C                              indices of refraction.
C
C 5.3   AXOP PROGRAM STRUCTURE
C
C     Compute the partial derivatives of the delay and rate with
C     respect to the antenna axis offsets at each site. Old compuation now
C     modified by index of refraction in air to correct for actual 
C     propagation speed. 94MAR25 -DG- 
C     (NOTE: The minus sign appears due to the definition of the
C     baseline vector being the site#1 vector minus the site#2 vector.)
C
C   Old axis offset partial compution
      DO   K = 1,2                       ! looping over delay and rate
           DAXOP(1,K) = + DCOMP(1,K) / VLIGHT      ! site 1
           DAXOP(2,K) = - DCOMP(2,K) / VLIGHT      ! site 2
      ENDDO
C
C   New axis offset partial (modified to use velocity of light in air)
      DO   K = 1,2                       ! looping over delay and rate
           Daxop_newer(1,K) = DAXOP(1,K) * N_air(1)      ! site 1
           Daxop_newer(2,K) = DAXOP(2,K) * N_air(2)      ! site 2
      ENDDO
C
C     PUT the new (simple formula) axis offset partial derivatives array.
      CALL PUT4 ('AXO PART      ', Daxop_newer, 2, 2, 1 )
C
C Compute axis offset delay and rate new partials using the alternate new
C  model (D. Gordon 3-term formula)
C   Station 1:
       uatm1  =  dATMCdh(1,1)*udel_h(1,1)
      duatm1  =  dATMCdh(1,1)*udel_h(1,2) + dATMCdh(1,2)*udel_h(1,1)
      DAXOP_new(1,1) = - DCOMP_new(1,1) + uatm1
      DAXOP_new(1,2) = - DCOMP_new(1,2) + duatm1
C
C   Station 2:
       uatm2 =  dATMCdh(2,1)*udel_h(2,1) 
      duatm2 =  dATMCdh(2,1)*udel_h(2,2) + dATMCdh(2,2)*udel_h(2,1)
      DAXOP_new(2,1) = + DCOMP_new(2,1) + uatm2
      DAXOP_new(2,2) = + DCOMP_new(2,2) + duatm2
C  The partials are so similar we only use the first one (Daxop_newer)
C
C   See if debug is neccessary.
      IF (KAXOD .ne. 0) Then
       WRITE ( 6, 9100 )
 9100  FORMAT (1X, "Debug output for subroutine AXOP." )
   8   FORMAT(A,5D25.16/(9X,5D25.16))
       WRITE(6,8)' AXOFF  ',AXOFF 
       WRITE(6,8)' DCOMP      ',DCOMP
       WRITE(6,8)' DCOMP_new  ',DCOMP_new
       WRITE(6,8)' DAXOP      ',DAXOP
       WRITE(6,8)' Daxop_newer',Daxop_newer
       WRITE(6,8)' Daxop_new  ',Daxop_new
       WRITE(6,8)' VLIGHT ',VLIGHT
       WRITE(6,'(" N_air: ",2f12.8)')  N_air 
       write(6,8)' dATMCdh ', dATMCdh 
       write(6,8)' udel_h  ', udel_h
       write(6,8)' uatm1, duatm1 ', uatm1,duatm1
       write(6,8)' uatm2, duatm2 ', uatm2,duatm2
      Endif
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE AXOC ( AXOFF, DAXOC )
      IMPLICIT None
C
C 6.    AXOC
C
C 6.1   AXOC PROGRAM SPECIFICATION
C
C 6.1.1 AXOC is the contributions section of the axis offset module. It
C       computes the contributions to the delay and rate due to the antenna
C       axis offsets at each site. We now compute three sets of contributions:
C       1) The old model contributions, 2) Contributions from a new 3-term 
C       model, and 3) Contributions from a new but simpler model which
C       multiplies the old contributions by the indexes of refraction at each 
C       site. Models 2 and 3 ahould give the same answer but they don't quite
C       because of different assumptions in estimating a priori the indexes of 
C       refraction versus estimating the a priori Niell mapping function. The
C       contribution that will be added to the theoretical delays and rates
C       is from the simple new formula. Corrections are computed to convert
C       to the more complicated new formula or to the old formula in program
C       SOLVE 'on the fly'.
C
C 6.2   AXOC PROGRAM INTERFACE
C
C 6.2.1 CALLING SEQUENCE -
C          INPUT VARIABLES:
C             1. AXOFF(2)  -  THE ANTENNA AXIS OFFSETS AT EACH SITE. (M) 
C          OUTPUT VARIABLES: 
C             1. DAXOC(2,2) - THE CONTRIBUTIONS TO THE DELAY AND TO THE DELAY
C                             RATE DUE TO ANTENNA AXIS OFFSETS AT EACH SITE.
C                             -Using the new 1-part axis offset delay formula.
C                             THE FIRST INDEX RUNS OVER THE OBSERVATION SITES
C                             AND THE SECOND RUNS OVER THE DELAY AND THE DELAY
C                             RATE. (SEC, SEC/SEC) 
C 
C 6.2.2 COMMON BLOCKS USED -
C 
      Real*8  DAXOP(2,2), DCOMP(2,2), SURREF, RICHM(2), DCOMP_new(2,2),
     .        udel_h(2,2),DAXOP_new(2,2), Daxop_newer(2,2)
      COMMON / AXOCM / DAXOP, DCOMP, SURREF, RICHM, DCOMP_new, 
     .        udel_h, DAXOP_new, Daxop_newer
C          VARIABLES 'FROM':
C            1. DAXOP(2,2)    -  THE PARTIAL DERIVATIVES OF THE DELAY AND THE
C                                DELAY RATE WITH RESPECT TO THE ANTENNA AXIS
C                                OFFSETS AT EACH SITE. THE FIRST INDEX RUNS
C                                OVER THE OBSERVATION SITES AND THE SECOND RUNS
C                                OVER THE DELAY AND THE DELAY RATE. (Old model)
C                                (SEC/M, SEC/SEC-M) 
C            2. DAXOP_new(2,2) - The partial derivatives of the delay and delay
C                                rate with respect to the antenna axis offsets
C                                at each site using the new 3-term model. First
C                                index runs over sites, second over delay and 
C                                rate. (sec/m, sec/sec/m)
C            3. DAXOP_newer(2,2)-The partial derivatives of the delay and delay
C                                rate with respect to the antenna axis offsets
C                                at each site using the new 1-term model. First
C                                index runs over sites, second over delay and 
C                                rate. (sec/m, sec/sec/m)
C 
       INCLUDE 'ccon.i'
C         VARIABLES 'FROM':
C            1. KAXOC - THE AXIS OFFSET MODULE FLOW CONTROL FLAG.
C            2. KAXOD - THE AXIS OFFSET MODULE DEBUG OUTPUT FLAG.
C 
C     Common /OBS/N_air, DAXOC_new, DAXOC_newer
C     Real*8 N_air(2), DAXOC_new(2,2), DAXOC_newer(2,2)
C
C 6.2.3 PROGRAM SPECIFICATIONS -
      Real*8  AXOFF(2), DAXOC(2,2)
      Real*8  del_axis_new(2), del_axis_old(2)
      Real*8  DAXOC_new(2,2), DAXOC_newer(2,2), DAXOC_old(2,2)
      Integer*4 K, L
C 
C 6.2.4 DATA BASE ACCESS -
C         'PUT' VARIABLES: 
C            1. DAXOC_newer(2,2)-THE CONTRIBUTIONS TO THE DELAY AND THE DELAY
C                              RATE DUE TO ANTENNA AXIS OFFSETS AT EACH SITE,
C                              using the new, simple axis offset model.
C                              THE FIRST INDEX RUNS OVER THE OBSERVATION
C                              SITES AND THE SECOND RUNS OVER THE DELAY AND
C                              THE DELAY RATE. (SEC, SEC/SEC)
C         ACCESS CODES:
C            1. 'AXO CONT' -  The database access code for the new, simple
C                             version, axis offset contributions array.
C            2. 'AXO2CONT' -  The database access code for the correction to
C                             use 'on-the-fly' in SOLVE to convert to the new,
C                             more complex version of the axis offset model.
C            3. 'AXIS OLD' -  The database access code for the correction to
C                             use 'on-the-fly' in SOLVE to convert to the old 
C                             (pre-Calc8.0) axis offset model.
C 
C 6.2.6 SUBROUTINE INTERFACE -
C          CALLER SUBROUTINES: DRIVC
C          CALLED SUBROUTINES: PUT4
C
C 6.2.7 CONSTANTS USED - NONE
C
C 6.2.8 PROGRAM VARIABLES -
C            1. DAXOC_new(2,2)   - The contributions to the delay and rate due
C                                  to the antenna axis offset using the new,
C                                  complex model. First index runs over sites,
C                                  second runs over delay and rate.
C            2. DAXOC_newer(2,2) - The contributions to the delay and rate due
C                                  to the antenna axis offset using the new,
C                                  simple model. First index runs over sites,
C                                  second runs over delay and rate.
C            3. DAXOC_old(2,2)   - The contributions to the delay and rate due
C                                  to the antenna axis offset using the old axis
C                                  offset model. First index runs over sites,
C                                  second runs over delay and rate.
C            4. del_axis_new(2)  - Delay and rate differences between the new
C                                  simple and the new complex axis offset
C                                  models. Add to theoreticals in SOLVE. 
C                                  (sec, sec/sec)
C            5. del_axis_old(2)  - Delay and rate differences between the new
C                                  simple and the old axis offset models. Add to
C                                  theoreticals in SOLVE. (sec, sec/sec)
C
C 6.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C                    PETER DENATALE 07/11/77
C                    BRUCE SCHUPLER 03/07/78
C                    Jim Ryan 89.06.29 Character stings used.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    D. Gordon 94.03.25 Added computations for both new versions
C                              of the axis offset contributions. Made the
C                              simpler version the default (will be added to 
C                              theoreticals in THERY). Computing corrections
C                              to convert to alternate new model and to old
C                              model ('on-the-fly' in SOLVE), and PUTting them
C                              into the database. 
C                    D. Gordon 94.05.23 Changed $Include to Include.
C                    D. Gordon 94.08.04 Error corrected. Old axis offset was
C                              still the default. Corrected to make new/simple
C                              offset the default. Added variable DAXOC_old.
C                    D. Gordon 98.11.12 Removed PUT of old axis offset 
C                              differential contribution.
C
C 6.3   AXOC PROGRAM STRUCTURE
C
C     Compute the three (old, new complex, and new simple) contributions.
      DO 120  K = 1,2
           DO 110  L = 1,2
C            DAXOC_old(L,K)   = DAXOP(L,K)       * AXOFF(L)
             DAXOC_new(L,K)   = DAXOP_new(L,K)   * AXOFF(L)
             DAXOC_newer(L,K) = DAXOP_newer(L,K) * AXOFF(L)
  110      CONTINUE
  120 CONTINUE
C
C   Set default axis offset to the new simple formula
       do  k = 1,2
           do  l = 1,2
                DAXOC(l,k) = DAXOC_newer(l,k)
           enddo   
       enddo   
C
C     Check to determine if the axis offset module is to be turned off.
      IF (KAXOC .eq. 1) Then
       DO 320  K = 1,2
           DO 310  L = 1,2
                DAXOC(L,K) = 0.D0
  310      CONTINUE
  320  CONTINUE
      Endif
C
C     PUT  the new/simple axis offset contributions.
  400 CALL PUT4 ('AXO CONT      ', DAXOC_newer, 2, 2, 1 )
C
C  Compute difference corrections to get the equivalent of the other new
C   formula and the old formula.
       do 340 k=1,2
            del_axis_new(k) = (DAXOC_new(1,k) + DAXOC_new(2,k)) -
     .                  (DAXOC_newer(1,k) + DAXOC_newer(2,k))
C           del_axis_old(k) = (DAXOC_old(1,k) + DAXOC_old(2,k)) -
C    .                  (DAXOC_newer(1,k) + DAXOC_newer(2,k))
  340  continue
C
      CALL PUT4 ('AXO2CONT      ', del_axis_new, 2, 1, 1 )
C     CALL PUT4 ('AXIS OLD      ', del_axis_old, 2, 1, 1 )
C
C     Check for debug output.
      IF (KAXOD .ne. 0)  Then
       WRITE ( 6, 9100 )
 9100  FORMAT (1X, "Debug output for subroutine AXOC." )
    8  FORMAT(A,5D25.16/(9X,5D25.16))
       WRITE(6,8)' AXOFF ',AXOFF
       WRITE(6,8)' DAXOP       ',DAXOP
       WRITE(6,8)' DAXOP_new   ',DAXOP_new
       WRITE(6,8)' DAXOP_newer ',DAXOP_newer
       WRITE(6,8)' DAXOC       ',DAXOC
C      WRITE(6,8)' DAXOC_old   ',DAXOC_old
       WRITE(6,8)' DAXOC_new   ',DAXOC_new
       WRITE(6,8)' DAXOC_newer ',DAXOC_newer
       WRITE(6,8)'del_axis_new  ',del_axis_new
C      WRITE(6,8)'del_axis_old',del_axis_old
      Endif
C
  600 RETURN
      END
C
C******************************************************************************
      double precision function sbend(El_rad,Temp_K,Humid_F,Press_Hg)
      IMPLICIT None
C
C input:
C   El_rad   -- elevation angle in radians 
C   Press_Hg -- Pressure in mm of Mercury (Hg)
C   Temp_K   -- Temperature in Kelvins
C   Humid_F  -- relative humidity (percent)
C
C output   --
C   Sbend  -- bending angle in radians.
C
      Real*8 El_rad, Temp_K, Humid_F, Press_Hg  
      Real*8 e(12),wp1(4),d3
      Real*8 fp,ft,fw,u,x,ad1,ad2,bd1,bd2,zd2,r,delta
      Real*8 a1,a2,b1,b2,c1,c2,e1,e2,e3,e4,e5,e6,e7,e8,e9
      Real*8 e10,e11,e12,p1,p2,t1,t2,z1,z2,w0,w1,w2,w3
      Real*8 conv
      Integer*4 I
C
      equivalence (e( 1), e1),(e( 2), e2),(e( 3), e3),(e( 4), e4),
     &            (e( 5), e5),(e( 6), e6),(e( 7), e7),(e( 8), e8),
     &            (e( 9), e9),(e(10),e10),(e(11),e11),(e(12),e12)
      equivalence (wp1(1),w0),(wp1(2),w1),(wp1(3),w2),(wp1(4),w3)
C 
      data a1, a2 /     0.40816d0, 112.30d0  / 
      data b1, b2 /     0.12820d0, 142.88d0  / 
      data c1, c2 /     0.80000d0,  99.344d0 / 
      data e   /    46.625d0  ,  45.375d0 ,     4.1572d0,  1.4468d0  ,
     &               0.25391d0,   2.2716d0,    -1.3465d0, -4.3877d0  ,
     &               3.1484d0 ,   4.5201d0,    -1.8982d0,  0.89000d0 /
      data p1 /   760.0d0 / 
      data t1 /   273.0d0 / 
      data wp1 / 22000.0d0    ,  17.149d0 ,  4684.1d0,    38.450d0   /
      data z1 /  91.870d0 / 
      data conv/57.295779512d0/ 
C 
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY 
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY 
      EXTERNAL CMATHB
C           VARIABLES 'FROM':
C              1. HALFPI - THE VALUE OF PI/2
C              2. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS
C                          (RAD/DEG)
C 
C STATEMENT FUCNTION
      delta(ad1,ad2,bd1,bd2,zd2)=(ad2-ad1)*dexp(bd1*(zd2-bd2))
C 
C CONVERT UNITS 
C  Zenith angle in degrees
      z2 = 90.0d0 - El_Rad/CONVD
C  Temperature in Kelvins
      t2 = Temp_K
C  Fractional humidity (0.0 -> 1.0)
      r = Humid_F 
C  Pressure in mm of Hg
      p2 = Press_Hg
C
C      WRITE(6,9956) R,T2,P2,Z2                                          DEBUG
C9956  FORMAT(" R,T2,P2,Z2",4F10.4)                                      DEBUG
C 
C CALCULATE CORRECTIONS FOR PRES, TEMP, AND WETNESS 
C      WRITE(6,9980) Z1,Z2,C1,C2                                         DEBUG
C9980  FORMAT(" Z1,Z2,C1,C2",4D15.6)                                     DEBUG
      d3=1.0d0+delta(z1,z2,c1,c2,z2)
      fp=(p2/p1)*(1.0d0-delta(p1,p2,a1,a2,z2)/d3) 
      ft=(t1/t2)*(1.0d0-delta(t1,t2,b1,b2,z2)/d3) 
      fw=1.0d0+(w0*r*dexp((w1*t2-w2)/(t2-w3))/(t2*p2))
C      WRITE(6,9957) D3,FP,FT,FW                                         DEBUG
C9957  FORMAT(" D3,FP,FT,FW ",4D12.6)                                    DEBUG
C 
C  CALCULATE OPTICAL REFRACTION 
      u=(z2-e1)/e2
      x=e11 
      do 10 i=1,8 
        x=e(11-i)+u*x 
10    continue
C 
C  COMBINE FACTORS AND FINISH OPTICAL FACTOR
      sbend=ft*fp*fw*(dexp(x/d3)-e12) 
C      WRITE(6,9958) SBEND,X,U                                           DEBUG
C9958  FORMAT(" SBEND,X,U ",3D20.10)                                     DEBUG
C 
C BACK TO RADIANS FROM ARC SECONDS
      sbend=(sbend/3600.0d0)*CONVD
C      WRITE(6,9959) SBEND                                               DEBUG
 9959  FORMAT(" SBEND (RADIANS) ",D20.10)                                DEBUG
      return
      end 
C
C-----------------------------------------------------------------------------
      SUBROUTINE FBOX ( TNCP, TCAXIS, CTCSTR, AZ, DAZ, ZA, DZA, 
     *            RANGL, DRANGL )
C
C     FBOX computes the feedbox rotation angle, relative to the source. 
C     It computes the angle, looking up at the sky and measured at the 
C     source, between the North Celestial Pole and the antenna fixed axis. 
C     An increase in the feedbox rotation angle means a clockwise 
C     rotation of the feedbox, for an observer looking up at the sky. 
C     The angle itself is arbitrary. The important quantity is the 
C     difference in the feedbox rotations at the two antennas, and how it
C     changes from one observation to the next.
C     
C     It's not clear what to do with these angle in all case. For RCP data
C     in geodesy, we correct the phase by adding the remote station 
C     correction and subtracting the reference station correction. 
C
C     The algorithm makes two coordinate rotations to put the star vector
C     along the X-axis. The azimuths of the NCP and the fixed axis in the
C     transformed Y-Z plane are then easily found and differenced.  
C
C     This subroutine replaces subroutine PANG in Calc 9.0, and will handle
C     all azis types. 
C
C     Input Variables:
C         1) CTCSTR(3)  - Topocentric unit vector in the direction of the 
C                         source. [X = Up = COS(Zenith Angle). Y = East =
C                         SIN(Zenith Angle) * SIN(Azimuth). Z = North =
C                         SIN(Zenith Angle) * COS(Azimuth). For greatest
C                         accuracy, the effects of atmospheric refraction 
C                         and aberration should be included.]
C         2) TCAXIS(3)  - Topocentric unit vector in the direction of the
C                         antenna's fixed axis. [Altazimuth: X = 1.0,
C                         Y = 0.0, Z = 0.0.  X/Y North (Gilcreek): 
C                         X = 0., Y = 0., Z = 1.0.  X/Y East (HARTRAO):
C                         X = 0., Y = 1.0., Z = 0.] 
C         3) TNCP(3)    - Topocentric unit vector in the direction of the 
C                         North Celestial Pole. [X = SIN(Geodetic Latitude),
C                         Y = 0.0, Z = COS(Geodetic Latitude)] 
C         4) AZ         - Azimuth angle of source, in radians. 
C         5) DAZ        - Time derivative of Azimuth angle. (radians/sec)
C                         Zeros can be used here if the user does not care
C                         about the rate of change of the rotation angle, or
C                         about the correction to the phase delay rate.
C         6) ZA         - Zenith angle of source, in radians. Include
C                         refraction and aberration for greatest accuracy.
C         7) DZA        - Time derivative of zenith angle. (radians/sec)
C                         Zeros can be used here if the user does not care
C                         about the rate of change of the rotation angle, or
C                         about the correction to the phase delay rate.
C     Output Variables:
C         1) RANGL      - The clockwise rotation angle of the feedbox. (rad)
C         2) DRANGL     - The time derivative of RANGL. (rad/sec)
C
C 
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY 
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY 
      EXTERNAL CMATHB
C           VARIABLES 'FROM':
C              1. HALFPI - THE VALUE OF PI/2
C              2. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS
C                          (RAD/DEG)
C
C    Program History:
C       D. Gordon 98.11.10 Subroutine written at GSFC. Replaces subroutine 
C                 PANG. 
C
C
      Real*8 TNCP(3), TCAXIS(3), CTCSTR(3), AZ, DAZ, 
     *       ZA, DZA, Rangl, dRangl
      REAL*8 P1AZ(3,3), P2ZA(3,3), P12(3,3), DP1AZ(3,3), DP2ZA(3,3), 
     *       DP12(3,3), TT1(3,3), TT2(3,3)
      REAL*8 NCP1(3), AXIS1(3), STAR1(3), DNCP1(3), DAXIS1(3)
      REAL*8 AZNCP, AZAXIS, DAZNCP, DAZAXIS
C
C  Construct rotation matrix to rotate about the X-axis by the angle -AZ
C    and its derivative.
      CALL ROTAT (-AZ, 1, P1AZ)
       CALL DROTT (-AZ, -DAZ, 1, DP1AZ)
C  Construct rotation matrix to rotate about the Y-axis by the angle -ZA
C    and its derivative.
      CALL ROTAT (-ZA, 2, P2ZA)
       CALL DROTT (-ZA, -DZA, 2, DP2ZA)
C  Combine the above rotation matrices and the derivative.
      CALL MMUL2 (P2ZA, P1AZ, P12)
       CALL MMUL2 (DP2ZA, P1AZ,  TT1)
       CALL MMUL2 (P2ZA, DP1AZ,  TT2)
       CALL MADD2 (TT1,    TT2, DP12)
C  Rotate the three topocentric vectors and take derivatives of first two. 
      CALL VECRT (P12,   TNCP,  NCP1)
      CALL VECRT (P12, TCAXIS, AXIS1)
      CALL VECRT (P12, CTCSTR, STAR1)
       CALL VECRT (DP12,   TNCP,  DNCP1)
       CALL VECRT (DP12, TCAXIS, DAXIS1)
C  The STAR1 vector should now be along the X-axis.
C       print *,' STAR1  ', STAR1
C
C  Find the azimuth angle of the NCP and its time derivative in the new frame
       AZNCP = -ATAN2(NCP1(3),NCP1(2)) + HALFPI
       IF (AZNCP .LT. 0.D0) AZNCP = AZNCP + TWOPI
        IF(DABS(NCP1(2)) .GT. 1.D-16) THEN
          dAZNCP = -1.D0/(1.D0+((NCP1(3)/NCP1(2))**2)) * 
     *      ( 1.D0/NCP1(2) * DNCP1(3) - NCP1(3)/NCP1(2)**2 * DNCP1(2) ) 
        ELSE
          dAZNCP = 0.D0 
        ENDIF 
C
C  Find the azimuth angle of the fixed axis and its time derivative 
       AZAXIS = -ATAN2(AXIS1(3),AXIS1(2)) + HALFPI
       IF (AZAXIS .LT. 0.D0) AZAXIS = AZAXIS + TWOPI
        IF(DABS(AXIS1(2)) .GT. 1.D-16) THEN
          dAZAXIS = -1.D0/(1.D0+((AXIS1(3)/AXIS1(2))**2)) *
     *      ( 1.D0/AXIS1(2) * DAXIS1(3) - AXIS1(3)/AXIS1(2)**2 *
     *                                           DAXIS1(2) ) 
        ELSE
          dAZAXIS = 0.D0 
        ENDIF 
C  Take difference (NCP to AXIS) and express as a clock-wise rotation, as
C   seen from the ground looking up
       RANGL = AZNCP - AZAXIS 
        DRANGL = DAZNCP - DAZAXIS 
        IF (RANGL .LT. -PI) RANGL = RANGL + PI
        IF (RANGL .GT.  PI) RANGL = RANGL - PI
C
C  That's all!
      RETURN
      END
