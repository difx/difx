      SUBROUTINE ATMA
      Implicit none
C
C     ATMA adds entries to the table of contents for the atmosphere module text
C     message, the flow control message, the partial derivatives, and the
C     contributions arrays.
C
C     ATMI Interface
C
C     Common blocks used - none
C
C            Data base access codes:
C              1. 'ATM MESS' - Access code for the atmosphere module message.
C              2. 'ATM CFLG' - Access code for ATM module control flag message.
C              3. 'EL-THEO ' - Access code for source elevation array.
C              4. 'AZ-THEO ' - Access code for the source azimuth array.
C              5. 'NDRYPART' - Access code for the NHMF2 (Niell) dry partial.
C              6. 'NWETPART' - Access code for the WHMF2 (Niell) wet partial.
C              7. 'NDRYCONT' - Access code for the NHMF2 (Niell) dry 
C                              contributions.
C              8. 'NWETCONT' - Access code for the WHMF2 (Niell) wet 
C                              contributions.
C              9. 'NGRADPAR' - Access code for the gradient partials using
C                              Niell dry scaling.  
C            Old Chau access codes deleted:
C              1. 'ATM PART' - Access code for the Chao dry partial.
C                              (In CALC since first version.)
C              2. 'WET PART' - Access code for the Chao wet partial.
C                              (Added in CALC 7.4.)
C              3. 'ATM CONT' - Access code for Chau dry contributions.
C
C       External I/O - none
C
C       Subroutine Interface -
C             Caller subroutines: TOCUP
C             Called subroutines: ADDA, ADDR
C
C     Programmer - Dale Markham  01/13/77
C      77.07.07  Peter Denatale
C      77.03.06  Bruce Schupler
C      87.03.06  Savita Goel   CDS for A900.
C      89.07.27  Jim Ryan      Documentation simplified.
C      89.12.12  Jim Ryan      UNIX-like database interface implimented.
C      91.05.24  Jim Ryan      Wet partial added and documatation simplified.
C      94.02.03  David Gordon  Access codes for nhmf and whmf partials and
C                              contributions added.
C      94.03.04  David Gordon  Changed Niell Lcodes from: NHMFCONT to NDRYCONT,
C                              NHMFPART to NDRYPART, WHMFCONT to NWETCONT, and
C                              WHMFPART to NWETPART.
C      98.11.19  David Gordon  ADDR for Niell dry atmosphere gradient partial. 
C                              Removing all old Chau access codes.
C
C     ADD for Atmosphere module test message.
      CALL ADDA (1,'ATM MESS','Atmosphere message definition   ',
     1     40, 1, 1 )
C
C     ADD for Atmosphere module flow control message.
      CALL ADDA(1,'ATM CFLG','Atmosphere control flag mess def',
     .     40,1,1)
C
C     ADD for Niell Dry (NHMF2) partial derivatives.
      CALL ADDR (2,'NDRYPART','Nhmf2 dry partial deriv. def.   ',
     1     2, 2, 1 )
C
C     ADD for Niell Wet (WHMF2) partial derivatives.
      CALL ADDR (2,'NWETPART','Whmf2 wet partial deriv. def.   ',
     1     2, 2, 1 )
C
C     ADD for Niell Dry (NHMF2) contributions.
      CALL ADDR (2,'NDRYCONT','Nhmf (dry) atm. contribution    ',
     1     2, 2, 1 )
C
C     ADD for Niell Wet (WHMF2) contributions.
      CALL ADDR (2,'NWETCONT','Whmf (wet) atm. contribution    ',
     1     2, 2, 1 )
C
C     ADD for Niell Dry atmosphere gradient partials. 
      CALL ADDR (2,'NGRADPAR','Niell dry atm. gradient partials',
     1     2, 2, 2 )
C
C     ADD for source elevation array.
      CALL ADDR(2,'EL-THEO ','Elevation array definition      ',2,2,1)
C
C     ADD for source azimuth array.
      CALL ADDR(2,'AZ-THEO ','Azimuth array definition        ',2,2,1)
C
C  Deletes for old Chau Lcodes
      CALL DELR (2,'ATM PART')
      Call DELR (2,'WET PART')
      CALL DELR (2,'ATM CONT')
C
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE ATMI
      Implicit none
C
C     ATMI is the atmosphere module input and initialization section.
C
      INCLUDE 'ccon.i'
C       Variables 'from':
C          1. KATMC - Atmosphere module flow control flag.
C                     = 0 (default), compute Niell dry and
C                       wet partials and contributions, but don't apply them to
C                       the theoreticals.
C                     = 1, as above but DO apply Niell dry to the theoreticals.
C
C     Program specifications -
      INTEGER*2      LATMM(40),      LON(40),    LOFF(40)
      CHARACTER*40 C_LATMM(2) ,    C_LON(2) ,  C_LOFF(2)
      EQUIVALENCE (C_LATMM,LATMM),(C_LON,LON),(C_LOFF,LOFF)
C
      DATA C_LATMM /
     .'Atmosphere Module - Last modification 99',
     .'OCT05, D. Gordon, GSFC.                 '/
C
      DATA C_LON /
     .'Atmosphere Module turned on, Niell dry c',
     .'ontributions applied to theoreticals.   '/
C
      DATA C_LOFF /
     .'Atmosphere Module is turned on - Contrib',
     .'utions NOT applied to theoreticals.     '/
C
C     Database access -
C            'Put' variables:
C              1.  LATMM(40)  -  The atmosphere module text message.
C              2.  LON(40)    -  Module flow control 'ON' message.
C              3.  LOFF(40)   -  Module flow control 'OFF' message.
C            Access codes:
C              1.  'ATM MESS'  -  Access code for module text message.
C              2.  'ATM CFLG'  -  Access code for module status message.
C
C      Subroutine Interface -
C         Caller subroutines: INITL
C         Called subroutines: PUTA
C
C      Programmer - Dale Markham  01/13/77
C       77.07.07  Peter Denatale
C       78.01.03  Bruce Schupler
C       85.01.03  David Grodon  Changed FLAG=1 definition.
C       87.03.06  Savita Goel   CDS for A900.
C       89.06.29  Jim Ryan      Character strings used.
C       89.12.12  Jim Ryan      UNIX-like database interface implimented.
C       91.05.24  Jim Ryan      Documentation simplified.
C       94.02.03  David Gordon  Mods for Nhmf2 and Whmf2 computations.
C
C     PUT Atmosphere module text message.
      CALL PUTA ('ATM MESS      ', LATMM, 40, 1, 1 )
C
C     PUT module control flag status message according to KATMC.
      IF (KATMC .EQ. 1) CALL PUTA('ATM CFLG      ',LON,40,1,1)
      IF (KATMC .NE. 1) CALL PUTA('ATM CFLG      ',LOFF,40,1,1)
C
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE ATMG ( R2000, STAR, EARTH, TCTOCF, SITEV, AZ, ELEV,
     .                  STAR_ABERRATED )
      Implicit none
C
C       ATMG
C
C       ATMG is the geometry section of the Atmosphere module. It computes the
C       aberrated source vector and the elevation of the aberrated source and 
C       its CT time derivative at each site.
C
C       References - Smart, W.M., 'Textbook on Spherical Astronomy', 1965, P. 68
C
C       ATMG program interface
C
C       Calling sequence -
C           Input variables:
C             1. R2000(3,3,3)  - The complete crust fixed to J2000.0 rotation
C                                matrix and its first two CT time derivatives.
C                                (unitless, 1/sec, 1/sec**2)
C             2. STAR(3)       - The J2000.0 source unit vector. (unitless)
C             3. EARTH(3,3)    - The SSBC position, velocity and acceleration of
C                                the Earth. (m,m/s,m/s**2)
C             4. TCTOCF(3,3,2) - The rotation matrix which rotates the
C                                topocentric reference system to the crust fixed
C                                geocentric reference system at each observation
C                                site. (unitless)
C             5. SITEV(3,2)    - The J2000.0 geocentric velocity of each site.
C                                (m/sec)
C           Output variables:
C             1. STAR_ABERRATED(3,2) - The J2000 source unit vector with
C                            aberration applied at each observing site.
C             2. AZ(2,2)   - The azimuth angle of the source corrrected for 
C                            aberration and its CT time derivative at each
C                            site (rad, rad/sec)
C             3. ELEV(2,2) - The elevation angle of the source corrected for 
C                            aberration and its CT time derivative at each 
C                            observing site (rad,rad/sec)
C
C     Common blocks used -
C
      Real*8 RF(2), RTRACE(2), wetcon(2), Datmp_hmf(2,2), 
     .       Datmp_wmf(2,2), Zen_dry(2,2), Zen_wet(2,2)
      COMMON /ATMCM/ RF, RTRACE, wetcon, Datmp_hmf, Datmp_wmf, Zen_dry,
     .               Zen_wet
C           Variables 'from':
C             1. RF(2) - The constants appearing in terms 1 and 2 in the
C                        calculation of the correction angle to be added to
C                        the source zenith angle to find the apparent source
C                        direction due to tropospheric refraction. - See 
C                        references. (Radians)
C
      INCLUDE 'ccon.i'
C           Variables 'from':
C             1. KATMC  -  The atmosphere module flow control flag.
C             2. KATMD  -  The atmosphere module debug output flag.
C
      Include 'cphys.i'
C           Variables from:
C             1. VLIGHT  -  The speed of light. (m/s)
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.

      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
C           Variables 'from' :
C             1. TWOPI  -  The mathematical constant 2 * PI
C
C     Program specifications -
      Real*8     CFSTAR(3,2), CFTOTC(3,3), R2000(3,3,3), SITLAT(2),
     1           STAR(3), TCSTAR(3,2), TCTOCF(3,3,2), TR2000(3,3,2),
     2           AZ(2,2),TAZ(2,2),ELEV(2,2), STAR_ABERRATED(3,2)
      Real*8 DOTP, EARTH(3,3), VR, SITEV(3,2), Earthplus(3)
      Real*8 Vecmg, starcheck, K_Unit_Aberr(3)
      Real*8 AZQUAD
      Integer*4 I, L
C
C      Database access  -
C      PUT variables:
C        1. ELEV(2,2) - The elevation angle of the source corrrected for 
C                       aberration and its CT time derivative at each
C                       site. (rad,rad/sec)
C        2. AZ(2,2)   - The azimuth angle of the source corrrected for 
C                       aberration and its CT time derivative at each
C                       site. (rad,rad/sec)
C
C      External I/O - possible debug output.
C
C      Subroutine interface -
C             Caller subroutines: DRIVG
C             Called subroutines: DASIN, DCOS, MTRAN, VECRT, DATAN2, PUT4
C
C      Constants used - none
C
C      Program variables -
C           1. CFSTAR(3,2)   - The aberratted source unit vector in the crust 
C                              fixed geocentric reference system and its CT time
C                              derivative. (unitless, 1/sec)
C           2. CFTOTC(3,3)   - The 3x3 rotation matrix which rotates the 
C                              geocentric crust fixed reference system to the
C                              topocentric reference system. (unitless)
C           3. TCSTAR(3,2)   - The aberratted source unit vector in the 
C                              topocentric reference system and its CT time 
C                              derivative.
C                              (unitless, 1/sec)
C           4. TR2000(3,3,2) - The complete J2000.0 to crust fixed rotation
C                              matrix and its first CT time derivative. 
C                              (unitless, 1/sec)
C           5. TAZ(2,2)      - A temporary array used in the computation of AZ
C                              (rad,rad/sec)
C           6. AZQUAD        - A variable used to force the source azimuth into
C                              the range 0 to TWOPI rather than -PI to PI.
C           7. VR            - Barycentric speed of the Earth in the direction
C                              of the source.
C
C       Programmer - Dale Markham  01/13/77
C        77.11.07  Peter Denatale
C        78.02.13  Bruce Schupler
C        87.06.03  Savita Goel   CDS for A900)
C        89.05.22  Gregg Cooke
C        89.07.27  Jim Ryan      Documentation simpfilied.
C        89.12.12  Jim Ryan      UNIX-like database interface implimented.
C        89.05.25  Jim Ryan      Documentation simplified again.
C        91:11:05  Jim Ryan      Logic to account for source position
C                                aberration added to the elevation and
C                                azimuth computation.
C        91:12:30  Jim Ryan      Two equations involving VR have had the sign
C                                flipped to make to use of VR consistent with
C                                it definition.
C        93.05.19  David Gordon  STAR_ABERRATED spelling corrected, changed to
C                                a (3,2) variable, diurnal aberration added,
C                                and renormalized to a unit vector. Replaced 
C                                derivative of STAR with STAR_ABERRATED for
C                                derivatives of azimuth and elevation 
C                                computation.
C        94.03.03  David Gordon  Added computation for unmodified (unaberrated)
C                                source elevations and azimuths.
C        94.10.24  David Gordon  Removed computation of unmodified (unaberrated)
C                                source elevations and azimuths.
C        95.12.18  David Gordon  SITEV added to debug printout.
C        98.08.05  David Gordon  Mods for geocenter station.
C
C  ATMG program structure
C
C   Loop twice for the calculation of the elevation and azimuth angles
C   and their CT time derivatives at each site.
C
      DO 500  L = 1,2
C
C  Check for geocenter station
       IF (L .eq. Nzero) Go to 450
C
C  Rotate the J2000.0 source unit vector to the topocentric system.
C   (NOTE: The topocentric system sits at the observation site with the axes
C    pointing Up, East and North.
C
C  Compute the rotation matrix which rotates from the geocentric crust fixed
C    system to the topocentric system.
            CALL MTRAN ( TCTOCF(1,1,L), CFTOTC )
C
C  Compute the rotation matrix which rotates from the J2000.0 system to the
C    geocentric crust fixed system.
            CALL MTRAN ( R2000(1,1,1), TR2000(1,1,1) )
C
C  Apply aberration to the J2000 unit vector. We now add in the diurnal
C    component to each site's velocity, whereas previously only an annual
C    term was used.
            call vecad(EARTH(1,2),SITEV(1,L),Earthplus)
            VR = DOTP(STAR,Earthplus)
           Do I = 1,3
             STAR_ABERRATED(I,L) = STAR(I)
     *       + (Earthplus(i) - VR*STAR(I)) / VLIGHT
           Enddo
C  Normalize for further use here, but keep initial vector for later use
C    in the axis offset module
             Call VUNIT(Star_Aberrated(1,L), K_Unit_Aberr)
C
C
C  Rotate to the crust fixed system:
C           CALL VECRT (TR2000(1,1,1),STAR_ABERRATED(1,L),CFSTAR(1,1))
            CALL VECRT (TR2000(1,1,1),K_Unit_Aberr       ,CFSTAR(1,1))
C  Rotate to the topocentric system:
            CALL VECRT (CFTOTC, CFSTAR(1,1), TCSTAR(1,1))
C
C  Compute the elevation angle of the aberrated source.
            ELEV(L,1) = DASIN ( TCSTAR(1,1) )
C
C     Compute the azimuth angle of the aberrated source.
            TAZ(L,1) = DATAN2(TCSTAR(2,1),TCSTAR(3,1))
            AZQUAD = 0.0D0
            IF (TAZ(L,1) .LT. 0.0D0) AZQUAD = TWOPI
            AZ(L,1) = TAZ(L,1) + AZQUAD
C
C     Compute the CT time derivative of the aberrated source unit vector
C      in the topocentric system.
            CALL MTRAN ( R2000(1,1,2), TR2000(1,1,2) )
C           CALL VECRT ( TR2000(1,1,2),STAR_ABERRATED(1,L),CFSTAR(1,2))
            CALL VECRT ( TR2000(1,1,2),K_Unit_Aberr       ,CFSTAR(1,2))
            CALL VECRT ( CFTOTC, CFSTAR(1,2), TCSTAR(1,2) )
C
C     Compute time derivatives of elevation and azimuth
            ELEV(L,2) = TCSTAR(1,2) / DCOS ( ELEV(L,1) )
C
            AZ(L,2) = ( (TCSTAR(2,2) / TCSTAR(3,1)) -
     1                (TCSTAR(2,1) * TCSTAR(3,2) / TCSTAR(3,1)**2) ) /
     2                (1.0D0 + (TCSTAR(2,1) / TCSTAR(3,1))**2)
C
       GO TO 500
C
  450 CONTINUE
C      Geocenter station special handling
c           print *,'ATMG/Nzero: SITEV = ', SITEV
            call vecad(EARTH(1,2),SITEV(1,L),Earthplus)
            VR = DOTP(STAR,Earthplus)
           Do I = 1,3
             STAR_ABERRATED(I,L) = STAR(I)
     .       + (Earthplus(i) - VR*STAR(I)) / VLIGHT
           Enddo
            ELEV(L,1) = HALFPI
            AZ(L,1)   = 0.0D0 
            ELEV(L,2) = 0.0D0
            AZ(L,2)   = 0.0D0
C
C     Close the loop which runs over the sites.
  500 CONTINUE
C
C   PUT the elevation and its time derivative into the data base.
      CALL PUT4('EL-THEO      ',ELEV,2,2,1)
C
C   PUT the azimuth and its time derivative into the data base.
      CALL PUT4('AZ-THEO      ',AZ,2,2,1)
C
C   Check for debug output.
      IF ( KATMD .ne. 0 )  Then
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for subroutine ATMG." )
    8 FORMAT(A,3D25.16/(7X,3D25.16))
      WRITE(6,8)' CFSTAR  ',CFSTAR
      WRITE(6,8)' CFTOTC  ',CFTOTC
      WRITE(6,8)' TCSTAR  ',TCSTAR
      WRITE(6,8)' TR2000  ',TR2000
      write(6,8) ' SITEV  ', SITEV
      write(6,8) ' EARTH  ', EARTH
      write(6,8) ' Earthplus ', Earthplus
      write(6,8) ' VR        ', VR
      write(6,8) ' STAR           ', STAR
      write(6,8) ' STAR_ABERRATED ', STAR_ABERRATED
      WRITE(6,8)' ELEV    ',ELEV
      WRITE(6,8)' AZ      ',AZ
      WRITE(6,8)' TAZ     ',TAZ
      WRITE(6,8)' AZQUAD  ',AZQUAD
      WRITE ( 6, 9200 )  R2000, STAR, TCTOCF,TWOPI
 9200 FORMAT (1X, "R2000  = ", 9 ( 3 ( D30.16, 10X ), /, 1X ), /, 1X,
     1            "STAR   = ", 3 ( D30.16, 4X ), /, 1X,
     2            "TCTOCF = ", 6 ( 3 ( D30.16, 10X ), /, 1X ),1X,
     3            "TWOPI  = ",D30.16)
C
      Endif
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE ATMP (ELEV, AZ, SITLAT, SITHEIGHT, XJD, CT, dATMCdh)
      Implicit none
C
C     ATMP is the atmosphere module partial derivatives section. ATMP computes
C     the partial derivatives of the delay and rate with respect to the zenith
C     electrical path delay at each observing site. Separate computations are
C     made for the Chau and the Niell (Nhmf and Whmf) models.
C
C     Calling sequence:
C       'Input' variables:
C          1. ELEV(2,2) - The elevation angle of the source and its CT time
C                         derivative, corrected for aberration, at each 
C                         observation site (rad,rad/sec)
C          2. AZ(2,2)   - The azimuth angle of the source and its CT time
C                         derivative, corrected for aberration, at each 
C                         observation site (rad,rad/sec)
C          2. SITLAT(2) - The geodetic site latitudes. (rad)
C          3. SITHEIGHT(2)-The site heights above the geoid. (meters)
C          4. XJD       - Julian date at zero hours UTC of the date in question.
C          5. CT        - The coordinate time fraction of the coordinate time
C                         day.
C       'Output' variables:
C          1. dATMCdh(2,2)-Derivative of the Niell dry atmosphere contribution
C                          with respect to station height. First index runs over
C                          the sites and the second over the delay and rate.
C                          (sec/meter, sec/sec/meter) 
C 
      Real*8 ELEV(2,2), AZ(2,2), SITLAT(2), SITHEIGHT(2), XJD, CT, 
     *       dATMCdh(2,2) 
C
C   Common blocks used:
C
      Real*8 N_air(2),DAXOC_new(2,2), Daxoc_newer(2,2)
      Common /OBS/N_air,DAXOC_new, Daxoc_newer
C
      Real*8 RF(2), RTRACE(2), wetcon(2), Datmp_hmf(2,2), 
     .       Datmp_wmf(2,2), Zen_dry(2,2), Zen_wet(2,2)
      COMMON /ATMCM/ RF, RTRACE, wetcon, Datmp_hmf, Datmp_wmf, Zen_dry,
     .               Zen_wet
C        Variables 'from':
C          1. RTRACE(2) - The constants appearing in the calculation of the
C                         partial derivatives of the delay and the delay
C                         rate with respect to the zenith path delay at each
C                         site. These constants were determined from the
C                         best fit of the ray tracing algorithm for
C                         atmospheric thickness from the zenith to the 
C                         horizon. (unitless)  (See references.)
C          2. wetcon(2) - The same as RTRACE, except that RTRACE is
C                         appropriate for the dry atmosphere and wetcon is
C                         appropriate for the wet.
C        Variables 'to':
C          1. Datmp_hmf(2,2) - The Niell (Nhmf) dry atmosphere partial 
C                          derivatives of the delay and rate with respect to
C                          the zenith path delays at each site. The first
C                          index runs over the sites and the second runs
C                          over the delay and rate. (sec,sec/sec)
C          2. Datmp_wmf(2,2) - Same as Datmp_hmf, except for the wet Niell 
C                          (Whmf) atmosphere partials.
C          3. Zen_dry(2,2)- Dry (hydrostatic) zenith delay (meters), and its 
C                          rate of change (m/sec) from Niell. First index runs
C                          over sites, second runs over delay and rate.
C          4. Zen_wet(2,2)- Wet zenith delay (meters), and its rate of change
C                          (m/sec) from Niell. First index runs over sites, 
C                          second runs over delay and rate.
C
      INCLUDE 'ccon.i'
C        Variables 'from':
C          1. KATMC  -  The atmosphere module flow control flag.
C          2. KATMD  -  The atmosphere module debug output flag.
C
      INCLUDE 'cphys.i'
C        Variables 'from':
C          1. VLIGHT - Velocity of light in a vacuum. (m/sec)
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.
C
      INCLUDE 'cuser.i'
C          Variables from:
C            1. Calc_user - Calc user type. 'A' for Calc/SOLVE analysis.
C                           'C' for VLBI correlator.
C
C     Program specifications -
C
C     Database access -
C        'PUT' variables:
C          1. Datmp_hmf(2,2)     - See above.
C          2. Datmp_wmf(2,2)     - See above. 
C          3. Ngrad(2,2,2)       - Atmosphere gradient partials, scaled using 
C                                  the Niell dry mapping function. First 
C                                  index runs over sites, second over North 
C                                  and East components, third over delay and
C                                  rate.  
C        Access codes:
C          1. 'NDRYPART' - Access code for dry Niell (Nhmf) partials.
C          2. 'NWETPART' - Access code for wet Niell (Whmf) partials.
C          3. 'NGRADPAR' - Access code for dry Niell atmosphere gradient
C                          partials. 
C
C    Subroutine interface -
C        Caller subroutines: DRIVP
C        Called subroutines: DCOS, DSIN, DTAN, PUT4
C
C    Program variables:
      Real*8  hmf(2), wmf(2), hmf2(2,2), wmf2(2,2), epoch, el
      Real*8  Temp, Tdot, Press, Pdot, Relhum, Rdot, X, ZD, ZDdot,
     *        ZW, ZWdot, sithit, Rlat, dXdh, dPdh, dZDdh,
     *        SurPR(2,2), SurTP(2,2), SurHM(2,2), Ngrad(2,2,2)
      Integer*4 N, metPR, metTP, metHM
      Integer*2 ND0(3), Kerr 
C
C          1. Press  - Atmospheric presure. (mbar)
C          2. Pdot   - Rate of change of pressure (set to zero).
C          3. Relhum - Relative humidity. (fraction)
C          4. Temp   - Temperature. (Celsius)
C          5. ZD,ZDdot-Dry (hydrostatic) zenith delay, and rate of change,
C                      from Niell model. (meters, meters/sec)
C          6. ZW,ZWdot-Wet zenith delay, and rate of change, from Niell model.
C                      (meters, meters/sec)
C          7. dPdh   - Derivative of pressure with respect to station height.
C          8. dZDdh  - Derivative of zenith delay with respect to station 
C                      height.
C          9. hmf2(2,2)-Hydrstatic (dry) mapping function and its derivative
C                      with respect to source elevation. First index runs over
C                      sites, second over the function and the derivative.
C          10. wmf2(2,2)-Wet delay mapping function and its derivative with
C                      respect to source elevation. First index runs over sites,
C                      second over the function and the derivative.
C
C 5.2.9 PROGRAMMER - DALE MARKHAM  01/13/77
C    77.07.11  Peter Denatale
C    89.07.27  Gregg Cooke
C    89.07.27  Jim Ryan  Documentation simpfilied.
C    89.12.13  Jim Ryan  UNIX-like database interface implemented.
C    89.05.24  Jim Ryan  Wet partial introduced and dry partial recode
C                        to simplify. Documetation simplified.
C    94.02.03  David Gordon - Put in ADDR's for 'NDRYPART', 'NWETPART',
C              access codes for Arthur Niell's NHMF2 and WHMF2 dry and wet 
C              mapping function partials. Added SITLAT, SITHEIGHT, XJD, and CT
C              to ATMP calling sequence. DATMP changed to Datmp_Chau. Wetp 
C              changed to Datmp_wetChau. Added Datmp_hmf(2,2) and Datmp_wmf(2,2)
C              to Common block ATMCM; these are the Niell model dry and wet 
C              atmosphere partials w.r.t. the zenith path delay. Calls to 
C              subroutines Nhmf2 and Nwmf2 added to get Niell dry and wet 
C              mapping functions. PUT4's added for NDRYPART and NWETPART.
C    94.03.23  David Gordon - Moved calls to SASTD and SASTW to compute zenith
C              delays here instead of in ATMC. Added dATMCdh to calling sequence
C              and moved computation to here instead of ATMC.
C    94.03.25  David Gordon - N_air(2) computation added, a priori index of 
C              refraction at each site.
C    95.12.18  David Gordon - Added about 30 variables to debug printout.
C    98.08.05  David Gordon - Mods for geocenter station.
C    98.10.01  David Gordon - New include file 'cuser.i' to tell if user is 
C              analysis center or correlator. If correlator, allowed to use
C              measured pressures, temperatures, and humidities in 
C              computation of Saastomoinen zenith dry and/or wet delays. 
C    98.11.19  David Gordon - Adding computation and put for atmosphere
C              gradient partials, using the Niell dry mapping function. 
C              Removed all old Chau computations and PUT's.
C
C 5.3   ATMP PROGRAM STRUCTURE
C
C  Check to see if we are to use surface met data:
      metPR = 0 
      metTP = 0 
      metHM = 0 
       IF (Calc_user .eq. 'C') Then
         CALL GET4 ('ATM PRES      ', SurPR, 2, 2, 1, ND0, Kerr) 
           If (Kerr .eq. 0) metPR = 1
         CALL GET4 ('TEMP C        ', SurTP, 2, 2, 1, ND0, Kerr) 
           If (Kerr .eq. 0) metTP = 1
         CALL GET4 ('REL.HUM.      ', SurHM, 2, 2, 1, ND0, Kerr) 
           If (Kerr .eq. 0) metHM = 1
       ENDIF
C
C  Now do Arthur Niell's mapping functions:
       epoch = XJD + CT
       Do N=1,2                                      ! Loop over sites
C
C   First check for geocenter station
        IF (N .eq. Nzero) Go to 550 
C
        Rlat = SITLAT(N)
        sithit = SITHEIGHT(N)
        el = elev(N,1)
C
        call NHMF2(epoch, rlat, sithit, el, hmf)
        call NWMF2(rlat, el, wmf)
C
C   Partials with respect to zenith path delays:
      if(N.eq.1) then             ! reverse sign for station 1)
        Datmp_hmf(N,1) = -hmf(1)            ! hydrostatic mapping function 
        Datmp_hmf(N,2) = -hmf(2)*ELEV(N,2)  ! derivative of above w.r.t. elev. 
        Datmp_wmf(N,1) = -wmf(1)            ! wet mapping function       
        Datmp_wmf(N,2) = -wmf(2)*ELEV(N,2)  ! derivative of above w.r.t. elev. 
      else
        Datmp_hmf(N,1) = hmf(1)             ! hydrostatic mapping function 
        Datmp_hmf(N,2) = hmf(2)*ELEV(N,2)   ! derivative of above w.r.t. elev. 
        Datmp_wmf(N,1) = wmf(1)             ! wet mapping function       
        Datmp_wmf(N,2) = wmf(2)*ELEV(N,2)   ! derivative of above w.r.t. elev. 
      endif
C
C    Atmosphere gradient partials using Niell mapping function
C        Delay terms
       Ngrad(N,1,1) = DCOS(AZ(N,1)) / DTAN(ELEV(N,1)) * hmf(1)          !North
       Ngrad(N,2,1) = DSIN(AZ(N,1)) / DTAN(ELEV(N,1)) * hmf(1)          !East 
C        Rate terms
       Ngrad(N,1,2)= -DSIN(AZ(N,1))/DTAN(ELEV(N,1))*hmf(1)*AZ(N,2)
     *              - DCOS(AZ(N,1))/DSIN(ELEV(N,1))**2*hmf(1)*ELEV(N,2)
     *              + DCOS(AZ(N,1))/DTAN(ELEV(N,1))*hmf(2) * ELEV(N,2) 
       Ngrad(N,2,2) = DCOS(AZ(N,1))/DTAN(ELEV(N,1))*hmf(1)*AZ(N,2) 
     *              - DSIN(AZ(N,1))/DSIN(ELEV(N,1))**2*hmf(1)*ELEV(N,2)
     *              + DSIN(AZ(N,1))/DTAN(ELEV(N,1))*hmf(2) * ELEV(N,2) 
C     print *,' Ngrad  ', Ngrad(N,1,1),Ngrad(N,2,1),
C    *                    Ngrad(N,1,2),Ngrad(N,2,2)
C
C   Now do the Niell dry and wet (Nhmf and Whmf) contributions:
C
C     Met values, either measured or a priori computed:
        If (metTP .eq. 1 .and. SurTP(N,1).ne.-999.D0) Then
          Temp = SurTP(N,1)
          Tdot = SurTP(N,2)
        Else
          Temp = 293.15D0 - (6.5D-3)*Sithit - 273.16D0
          Tdot = 0.D0
        Endif
C
        If (metPR .eq. 1 .and. SurPR(N,1).ne.-99900.D0) Then
          Press = SurPR(N,1)
          Pdot  = SurPR(N,2)
        Else
          X = 1.D0 - (6.5D-3)*Sithit  / 293.15D0
          Press = 1013.25D0 * (X**5.26D0)
           Pdot = 0.D0
        Endif
C
        If (metHM .eq. 1 .and. SurHM(N,1).ne.-999.D0) Then
          Relhum = SurHM(N,1)
          Rdot   = SurHM(N,2)
        Else
          Relhum = .5D0
           Rdot  = 0.D0
        Endif
C
C    Compute index of refraction in air at each site
        N_air(N) = 77.6D-6*Press/(Temp+273.16D0) + 1.D0
C
C   94Feb15 - Take partials with respect to height
         dXdh = - 6.5D-3 / 293.15D0
         dPdh = 1013.25D0 * 5.26 * (X**4.26D0) * dXdh
C
C  Compute zenith dry and wet delays
      call SASTD(Press,Pdot,Rlat,Sithit,dPdh,ZD,ZDDOT,dZDdh)
      call SASTW(Relhum,Temp,Rdot,Tdot,ZW,ZWDOT)
C
      Zen_dry(N,1) = ZD / VLIGHT
      Zen_dry(N,2) = ZDDOT / VLIGHT
      Zen_wet(N,1) = ZW / VLIGHT
      Zen_wet(N,2) = ZWDOT / VLIGHT
C
C  Find atmosphere contribution partial with respect to height (sec/meter) and
C    its time derivative
        dATMCdh(N,1) = Datmp_hmf(N,1) * dZDdh / VLIGHT
        dATMCdh(N,2) = Datmp_hmf(N,2) * dZDdh / VLIGHT
       Go to 600
C
 550   Continue
C   Geocenter handling:
        Datmp_hmf(N,1) = 0.0D0
        Datmp_hmf(N,2) = 0.0D0
        Datmp_wmf(N,1) = 0.0D0
        Datmp_wmf(N,2) = 0.0D0
        Zen_dry(N,1)   = 0.0D0 
        Zen_dry(N,2)   = 0.0D0 
        Zen_wet(N,1)   = 0.0D0 
        Zen_wet(N,2)   = 0.0D0 
        dATMCdh(N,1)   = 0.0D0
        dATMCdh(N,2)   = 0.0D0
        Ngrad(N,1,1)   = 0.0D0
        Ngrad(N,2,1)   = 0.0D0
        Ngrad(N,1,2)   = 0.0D0
        Ngrad(N,2,2)   = 0.0D0 
C
 600   Continue
C
      Enddo                                          ! Loop over sites
C
C   'PUT' the Niell partials.
      CALL PUT4 ('NDRYPART      ', Datmp_hmf, 2, 2, 1 )
      CALL PUT4 ('NWETPART      ', Datmp_wmf, 2, 2, 1 )
      CALL PUT4 ('NGRADPAR      ', Ngrad    , 2, 2, 2 )
C************
C    Check for debug output.
      IF ( KATMD .ne. 0 )  Then 
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for subroutine ATMP." )
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' ELEV       ',ELEV
      write(6,8)' SITLAT     ',SITLAT
      write(6,8)' SITHEIGHT  ',SITHEIGHT
      write(6,8)' XJD, CT    ',XJD, CT  
      write(6,8)' epoch      ',epoch 
      write(6,8)' RLAT, Sithit, el ',RLAT, Sithit, el   
      write(6,8)' hmf        ',hmf   
      write(6,8)' wmf        ',wmf   
      write(6,8)' Temp, Press, Relhum ',Temp, Press, Relhum 
      write(6,8)' X          ',X     
      write(6,8)' N_air      ',N_air 
      write(6,8)' dXdh, dPdh ',dXdh, dPdh 
      write(6,8)' ZD, ZDDOT, dZDdh ',ZD, ZDDOT, dZDdh
      write(6,8)' ZW, ZWDOT        ',ZW, ZWDOT           
      WRITE(6,8)' RTRACE     ',RTRACE
      write(6,8)' wetcon     ',wetcon
      WRITE(6,8)' Datmp_hmf  ',Datmp_hmf 
      WRITE(6,8)' Datmp_wmf  ',Datmp_wmf 
      WRITE(6,8)' Zen_dry    ', Zen_dry
      WRITE(6,8)' Zen_wet    ', Zen_wet
      WRITE(6,8)' dATMCdh    ', dAtmcdh
      WRITE(6,8)' Ngrad      ', Ngrad 
      Endif
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE ATMC ( ZPATH, DATMC )
      Implicit none
C
C     ATMC is the atmosphere module contributions section. ATMC computes the
C     contributions to the delay and rate due to tropospheric refraction at
C     each observation site.
C
      Real*8 ZPATH(2), DATMC(2,2)
C
C     Calling sequence -
C          Input variables:
C            1. ZPATH(2)  - The nominal zenith atmosphere path delay at each 
C                           site. (sec) 
C          Output variables: 
C            1. DATMC(2,2) - The contributions to the delay and rate due to 
C                            tropospheric refraction at each site. The first 
C                            index runs over the sites and the second over the 
C                            delay and rate. (sec, sec/sec) 
C 
C     Common blocks used - 
C 
      Real*8 RF(2), RTRACE(2), wetcon(2), Datmp_hmf(2,2), 
     .       Datmp_wmf(2,2), Zen_dry(2,2), Zen_wet(2,2)
      COMMON /ATMCM/ RF, RTRACE, wetcon, Datmp_hmf, Datmp_wmf, Zen_dry,
     .               Zen_wet
C          Variables 'from': 
C            2. Datmp_hmf(2,2) - The Niell (Nhmf) dry atmosphere partial 
C                            derivatives of the delay and rate with respect to
C                            the zenith path delays at each site. The first
C                            index runs over the sites and the second runs
C                            over the delay and rate. (sec,sec/sec)
C            3. Datmp_wmf(2,2) - Same as Datmp_hmf, except for the wet Niell 
C                            (Whmf) atmosphere partials.
C            4. Zen_dry(2,2)- Dry (hydrostatic) zenith delay (meters), and its 
C                            rate of change (m/sec) from Niell. First index runs
C                            over sites, second runs over delay and rate.
C            5. Zen_wet(2,2)- Wet zenith delay (meters), and its rate of change
C                            (m/sec) from Niell. First index runs over sites, 
C                            second runs over delay and rate.
C
      INCLUDE 'ccon.i'
C            Variables 'from':
C              1. KATMC  -  The atmosphere module flow control flag.
C              2. KATMD  -  the atmosphere module debug output flag.
C
C     INCLUDE 'cphys.i'
C            Variables 'from':
C              1. VLIGHT - Velocity of light in a vacuum. (m/sec)
C
       INCLUDE 'cuser.i'
C          Variables from:
C            1. Calc_user - Calc user type. 'A' for Calc/SOLVE analysis.
C                           'C' for VLBI correlator.
C            2. Wet_atm   - Correlator option to combine dry and wet 
C                           atmosphere corrections. 
C                           = 'Y' => combine and output in 'NDRYCONT' access
C                             code. Set 'NWETCONT' access code to zero.
C                           = 'N' => don't combine.
C
C    Program specifications -
C    Local variables:
      Integer*4 L,K,N
      Real*8  Datmc_hmf(2,2), Datmc_wmf(2,2)
C
C     Database access - 
C         'PUT' variables:
C           1. Datmc_hmf(2,2) - The contributions to the delay and rate due 
C                           the Niell (Nhmf) dry tropospheric refraction at 
C                           each site. The  first index runs over the sites 
C                           and the second over the delay and rate. 
C                           (sec, sec/sec)
C           2. Datmc_hmf(2,2) - The contributions to the delay and rate due 
C                           the Niell (Whmf) wet tropospheric refraction at 
C                           each site. The  first index runs over the sites 
C                           and the second over the delay and rate. 
C                           (sec, sec/sec)
C         Access codes:
C           2. 'NDRYCONT' - Access code for the NHMF2 (Niell) dry contributions.
C           3. 'NWETCONT' - Access code for the WHMF2 (Niell) wet contributions.
C
C     Subroutine interface -
C             Caller subroutines: DRIVC
C             Called subroutines: PUT4
C
C     PROGRAMMER - Dale Markham  01/13/77
C      77.07.07  Peter Denatale
C      87.06.03  David Gordon Changed flag definition.
C      87.06.03  Savita Goel  CDS for A900)
C      89.05.22  Gregg Cooke
C      89.07.27  Jim Ryan     Documentation simpfilied.
C      89.12.12  Jim Ryan     UNIX-like database interface implimented.
C      91.05.24  Jim Ryan     Documentation further simplified.
C      94.02.04  David Gordon Added SITLAT and SITHEIGHT to calling sequence.
C                             Added subroutines Sastd and Sastw and calls to
C                             to get Niell model dry and wet Zenith
C                             contributions. PUT4's added for NDRYCONT and
C                             NWETCONT. DATMC set equal to Datmc_hmf for use
C                             elsewhere in Calc.
C      94.03.04  David Gordon Changed dATMCdh to a (2,2) array and added 
C                             computation for time derivative of Niell dry
C                             contribution. 
C      94.03.23  David Gordon Moved Niell zenith delay and dATMCdh computations
C                             to ATMP.
C      98.11.19  David Gordon Removed all old Chau computations and PUT's. 
C      99.10.05  David Gordon Added correlator option to use dry and 
C                             wet atmosphere corrections in theory module.
C
C     ATMC Program Structure
C
C   Do the Niell dry and wet (Nhmf and Whmf) contributions:
      Do N=1,2 
       Datmc_hmf(N,1) = Datmp_hmf(N,1) * Zen_dry(N,1)
       Datmc_hmf(N,2) = Datmp_hmf(N,2) * Zen_dry(N,1)
       Datmc_wmf(N,1) = Datmp_wmf(N,1) * Zen_wet(N,1)
       Datmc_wmf(N,2) = Datmp_wmf(N,2) * Zen_wet(N,1)
C
C  Store the Niell dry atmosphere delays for use elsewhere:
       DATMC(N,1) = Datmc_hmf(N,1)
       DATMC(N,2) = Datmc_hmf(N,2)
      Enddo 
C 
C  Mods for correlator usage. Add wet component to DATMC for use elsewhere
      If (Calc_user .eq. 'C' .and. Wet_atm .eq. 'Y') Then
       Do N=1,2 
        DATMC(N,1) = DATMC(N,1) + Datmc_wmf(N,1)
        DATMC(N,2) = DATMC(N,2) + Datmc_wmf(N,2)
       Enddo
      Endif
C
C  'PUT' the Niell dry and wet contributions.
      CALL PUT4 ('NDRYCONT      ', Datmc_hmf, 2, 2, 1 )
      CALL PUT4 ('NWETCONT      ', Datmc_wmf, 2, 2, 1 )
C
C     Check for debug output.
      IF ( KATMD .ne. 0 )  Then
      WRITE ( 6, 9100 )
 9100 FORMAT (1X, "Debug output for subroutine  ATMC." )
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' ZPATH        ',ZPATH     
      WRITE(6,8)' Zen_dry   ', Zen_dry
      WRITE(6,8)' Zen_wet   ', Zen_wet
      WRITE(6,8)' Datmc_hmf ',Datmc_hmf
      WRITE(6,8)' Datmc_wmf ',Datmc_wmf
      WRITE(6,8)' DATMC        ',DATMC     
      Endif
C
      RETURN
      END
C
C******************************************************************************
      Subroutine NHMF2(epoch,latitude,height,elev,hmf)
      Implicit none
C
C     Routine to compute the hydrostatic mapping function nhmf2 which depends on
C     the day of the year (DOY), station latitude, and height above the geoid.
C
      integer*4 i
C
C   a,b,c       - the a,b,and c coeffiecents in the continued fraction
C                 form of Marini
C   beta        - intermediate term in calculation
C   gamma       - intermediate term in calculation
C   sine        - sine of elevation angle
C   cose        - cos of elevation angle
C   hmf(1)      - delay mapping function
C   hmf(2)      - d_mapping_function/d_elevation (dhmf2/d_el)
C   topcon      - constant of top of mapping function to ensure
C                 that value is 1.0000 at zenith 
C
      Real*8 a,b,c, beta, cose, hmf(2), gamma, sine, topcon
C
C   height     - Height of site above geoid (meters)
C   hs_km      - Height of site in kms
C   latitude   - Latitude (radians)
C   latituded  - Latitude (deg)
C   l          - Absolute value of latitude
C   dl         - Incremental latitude from last lat_hmf
C   elev       - Elevation (radians)
C   epoch      - Julian date of observation, used to get day of year
C   doy        - Days since Dec 31 
C   doy_atm    - Doy for atmosphere relative to Jan 28
C   doyr_atm   - Doy_atm in radians;
C   cost       - Cosine(day of year)
C   doy2rad    - Convert doy to radians
C
      Real*8 epoch, doy, latitude,latituded, height, elev
      Real*8 hs_km, l, dl, doy_atm, doyr_atm, cost
C
C   lat_hmf(5)  - Latitudes at which coefficients are defined
C   abc_avg     - Continued fraction coefficients at latitudes lat_hmf
C   abc_amp     - Amplitude of annual variation of abc_avg
C   daavg, daamp, etc - Incremental values for interpolation
C   aavg,  aamp,  etc - Average and amplitude at latitude
C
      Real*8 lat_hmf(5), abc_avg(5,3), abc_amp(5,3), a_ht, b_ht, c_ht
      Common / hmf2_coef/ lat_hmf, abc_avg, abc_amp, a_ht, b_ht, c_ht 
C
      Real*8 daavg, daamp, dbavg, dbamp, dcavg, dcamp
C
C   a_ht, b_ht, c_ht - Parameters for continued fraction for height corr'n.
C
C   Define parameters used for calculating coefficients.
C
C     Data lat_hmf / 15, 30, 45, 60, 75/
C
C     Data abc_avg / 
C    .1.2769934D-3,1.2683230D-3,1.2465397D-3,1.2196049D-3,1.2045996D-3,
C    .2.9153695D-3,2.9152299D-3,2.9288445D-3,2.9022565D-3,2.9024912D-3,
C    .62.610505D-3,62.837393D-3,63.721774D-3,63.824265D-3,64.258455D-3/
C
C     Data abc_amp / 
C    .  0.0D0, 1.2709626D-5, 2.6523662D-5, 3.4000452D-5, 4.1202191D-5,
C    .  0.0D0, 2.1414979D-5, 3.0160779D-5, 7.2562722D-5, 11.723375D-5,
C    .  0.0D0, 9.0128400D-5, 4.3497037D-5, 84.795348D-5, 170.37206D-5/
C
C     Data a_ht / 2.53D-5/
C    .     b_ht / 5.49D-3/
C    .     c_ht / 1.14D-3/
C
      Real*8 aavg,  aamp,  bavg,  bamp,  cavg,  camp
      Real*8 ht_corr_coef, ht_corr
      real*8 dhmf_ht_del, dht_corr_coef_del, dht_corr_del               ! CSJ
C
C   Common Blocks:
C
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
C
      INCLUDE 'ccon.i'
C            Variables 'from':
C              1. KATMC  -  The atmosphere module flow control flag.
C              2. KATMD  -  the atmosphere module debug output flag.
C
C   Programmers:
C    00.00.00  Arthur Niell  Subroutine created.
C    93.05.17  Arthur Niell  Use phase of 28 days (winter extremum corresponds
C                            to Jan 28) based on least-square fit to raytrace
C                            of radiosonde data for DRT, ELP, ALB, CHH, FAI,
C                            MUN, and LIH.
C    94.01.27  Dan MacMillan Changed input latitude and elevation to radians,
C                            changed input time to Julian Day.  
C    95.12.15  David Gordon  Installed mods by Chris Jacobs (JPL) to compute 
C                            height correction for hmf(2). Cleanup/additions to
C                            documentation and debug printout.
C
C   Convert height in meters to kilometers
      hs_km  = height/1000.d0
C
C   If Julian date is used for epoch, then calculate day of year; 
C      use 1980 Jan 0 as reference epoch.
      doy = epoch - 2444238.5
C
C   To account for the six month difference in seasons between hemispheres,
C   add 365.25/2 days to doy if station is in the southern hemisphere.
      latituded = latitude/CONVD                 ! convert to degrees
      l = abs(latituded)
      if (latituded .lt. 0) doy = doy + 365.25/2
C
C mod aen 930517 Use phase of 28 days (winter extremum corresponds to Jan 28)
C                based on least-square fit to raytrace of radiosonde data for
C                DRT, ELP, ALB, CHH, FAI, MUN, and LIH.
      doy_atm  = doy - 28.
C  convert to radians
      doyr_atm = doy_atm * TWOPI/365.25
C
      cost = cos(doyr_atm)
C
C   Coefficients for the continued fraction expansion for each latitude.
C
C   For latitudes less than 15 degrees:
      if (l .le. lat_hmf(1)) then
         a = abc_avg(1,1)
         b = abc_avg(1,2)
         c = abc_avg(1,3)
      endif
C
C   For latitudes between 15 and 75  degrees:
      do i = 1,4
          if (l .gt. lat_hmf(i) .and. l .le. lat_hmf(i+1)) then
             dl = (l-lat_hmf(i))/(lat_hmf(i+1)-lat_hmf(i))
             daavg =   abc_avg(i+1,1)-abc_avg(i,1)
             daamp =   abc_amp(i+1,1)-abc_amp(i,1)
             aavg  =   abc_avg(i,1) + dl*daavg
             aamp  =   abc_amp(i,1) + dl*daamp
             a     = aavg - aamp*cost
C
             dbavg =   abc_avg(i+1,2)-abc_avg(i,2)
             dbamp =   abc_amp(i+1,2)-abc_amp(i,2)
             bavg  =   abc_avg(i,2) + dl*dbavg
             bamp  =   abc_amp(i,2) + dl*dbamp
             b     = bavg - bamp*cost
C
             dcavg =   abc_avg(i+1,3)-abc_avg(i,3)
             dcamp =   abc_amp(i+1,3)-abc_amp(i,3)
             cavg  =   abc_avg(i,3) + dl*dcavg
             camp  =   abc_amp(i,3) + dl*dcamp 
             c     = cavg - camp*cost
C
          endif
      end do
C
C   for latitudes greater than 75 degrees:
      if (l .ge. lat_hmf(5)) then
         a = abc_avg(5,1)
         b = abc_avg(5,2)
         c = abc_avg(5,3)
      endif
C
C   Now the coefficients exist; calculate the mapping function, hmf(1), and the
C    change of mapping function with elevation, dhmf/d_el = hmf(2).
C   To get delay-rate correction d_tau/dt:
C      d_tau/dt = d_tau-zen/dt*hmf(1) + tau-zen*dhmf/d_el*d_el/dt
C
      sine   = sin(elev )
      cose   = cos(elev )
      beta   = b/(sine + c )
      gamma  = a/(sine + beta)
      topcon = (1.d0 + a/(1.d0 + b/(1.d0 + c)))
C
      hmf(1) = topcon / ( sine + gamma )
C
      hmf(2) = -topcon / ( sine + gamma )**2 *
     .            ( cose - a/( sine + beta)**2 * cose *
     .            ( 1.d0 - b/( sine + c )**2 ) )
C
C   Apply height correction to mapping function only 
C         (not to dmf/d_el since this is a small correction): 
C      1) height correction coefficient is 
C         1/sine(elev) - continued fraction(a_ht,b_ht,c_ht).
C      2) height correction is ht_corr_coef times height in km.
C
      beta   = b_ht/( sine + c_ht )
      gamma  = a_ht/( sine + beta)
      topcon = (1.d0 + a_ht/(1.d0 + b_ht/(1.d0 + c_ht)))
      ht_corr_coef = 1/sine - topcon/(sine + gamma)
      ht_corr      = ht_corr_coef * hs_km
      hmf(1)       = hmf(1) + ht_corr
C
C************************************************
C   CSJ additions December 1995:
C     Calculate the component of d hmf/ d el    due to ht_corr.
C     This will be needed for the second term in delay rate equation,
C      d_tau/dt = d_tau-zen/dt*hmf(1) + tau-zen * dhmf/d_el * d_el/dt
C
C     Assume 
C      1) hs_km is constant.
C      2) d el /dt, hs_km, tau-zen are supplied outside this routine
C      3) For convenience of notation define  eldot = d el /dt
C
C     d ht_corr / dt   = d ht_corr      / d el         * eldot
C     d ht_corr / dt   = d ht_corr_coef / d el * hs_km * eldot
C
C     Thus we need d ht_corr_coef / d el
C     Define hmf_ht = topcon/(sinel+gamma)
C
C     d ht_corr_coef / d el  = d csc(el) / d el  - d hmf_ht /d el
C
C     d csc(el) / d el  =   - ( cosel / sinel**2 )
C
C     d hmf_ht / d el = (-topcon*cosel/( sinel + gamma)**2) *
C   (1.d0 -  (a_ht/( sinel + betm )**2) * (1.d0 -  (b_ht/( sinel + c_ht )**2)))
C   1        2     3              3   2   2        3     4              4   321
C   The numbers above show levels of parenthesis
C
      dhmf_ht_del = (-topcon*cose/( sine + gamma)**2) 
     *            * (1.d0 -  (a_ht/( sine + beta )**2) 
     *            * (1.d0 -  (b_ht/( sine + c_ht )**2) ) )      ! CSJ
C
      dht_corr_coef_del = -cose/sine**2 - dhmf_ht_del           ! CSJ
      dht_corr_del      = dht_corr_coef_del * hs_km             ! CSJ
      hmf(2)            = hmf(2) + dht_corr_del                 ! CSJ      
C
C************************************************
C     Check for debug output.
      IF ( KATMD .ne. 0 )  Then
        WRITE ( 6, 9100 )
 9100   FORMAT (1X, "Debug output for subroutine NHMF2." )
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        write(*,'("doy, doy_atm, doyr_atm = ", 3f20.9)') doy, doy_atm,
     .       doyr_atm
        WRITE(6,8)' epoch, elev    ', epoch, elev   
        WRITE(6,8)' Height, hs_km  ', Height, hs_km 
        WRITE(6,8)' Latitude, Latituded ', Latitude, Latituded 
        WRITE(6,8)' a, b, c ', a, b, c 
        write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
     .             sine, cose, beta, gamma, topcon
        write(6,8)' ht_corr_coef, ht_corr ', ht_corr_coef, ht_corr
        WRITE(6,8)' dhmf_ht_del       ', dhmf_ht_del   
        WRITE(6,8)' dht_corr_coef_del ', dht_corr_coef_del
        WRITE(6,8)' dht_corr_del      ', dht_corr_del
        write(6,8)' hmf(1), hmf(2)    ', hmf
      Endif
C
      Return
      End
C 
C******************************************************************************
      Subroutine NWMF2(latitude, elev, wmf)
      Implicit none
C
C    NWMF2: Routine to compute the new WMF2.0 mapping function which depends
C    only on latitude and elevation.
C
C   Input variables:
C       1. latitude - Latitude (radians)
C       2. elev     - Source selevation. (radians)
C
C   Output variables:
C       1. wmf(1) - Wet delay mapping function.
C       2. wmf(2) - Derivative of the wet delay mapping function (wmf(1) with
C                   respect to elevation.
C
C   Program variables:
C       1. a,b,c      - The a,b,and c coefficients in the continued fraction
C                       form of Marini
C       2. beta       - Intermediate term in calculation
C       3. gamma      - Intermediate term in calculation
C       4. sine       - Sine of elevation angle
C       5. cose       - Cos of elevation angle
C       6. topcon     - Constant of top of mapping fuinction to ensure
C                       value is 1.0000 at zenith 
C       7. latituded  - latitude (degrees)
C       8. l          - absolute latitude
C       9. dl         - incremental latitude from last lat_wmf
C      10. dl,da,db,dc- used for interpolation
C
      Real*8 a,b,c, beta, cose, wmf(2), gamma, sine, topcon
      Real*8 lat_wmf(5), abc_w2p0(5,3)
      Real*8 dl, da, db, dc
      Real*8 latitude, latituded, l, elev
      Integer*4 I
C
C   Define parameters used for calculating coefficients.
      Data lat_wmf / 15.D0, 30.D0, 45.D0, 60.D0, 75.D0/
C
C   Coefficients are from fits to raytraces of the standard atmospheres
C   for July for latitudes 15, 45, 60, and 75 degrees latitude and for 
C   January for 30 degrees latitude (930517).
      Data abc_w2p0 / 
     . 5.8021897D-4,5.6794847D-4,5.8118019D-4,5.9727542D-4,6.1641693D-4,
     . 1.4275268D-3,1.5138625D-3,1.4572752D-3,1.5007428D-3,1.7599082D-3,
     . 4.3472961D-2,4.6729510D-2,4.3908931D-2,4.4626982D-2,5.4736038D-2/
C
C   Common Blocks:
C
      INCLUDE 'ccon.i'
C
      COMMON /CMATH/ PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
      Real*8         PI,TWOPI,HALFPI,CONVD,CONVDS,CONVHS,SECDAY
C
C   Programmers:
C     93.05.17  Arthur Niell  Subroutine created.
C     94.01.27  Dan MacMillan Changed input latitude and elevation to radians.
C     95.12.18  David Gordon  Documentation made 'Calc'-like, debug section
C                             added. 
C
      latituded = latitude/CONVD             ! convert to degrees
      l = abs(latituded)
C
C   Coefficients for the continued fraction expansion for each latitude.
C
C   for latitudes less than 15 degrees:
      if (l .le. lat_wmf(1)) then
         a = abc_w2p0(1,1)
         b = abc_w2p0(1,2)
         c = abc_w2p0(1,3)
      endif
C
C   for latitudes between 15 and 75  degrees:
      do i = 1,4
          if (l .gt. lat_wmf(i) .and. l .le. lat_wmf(i+1)) then
             dl = (l-lat_wmf(i))/(lat_wmf(i+1)-lat_wmf(i))
             da  =   abc_w2p0(i+1,1)-abc_w2p0(i,1)
             a   =   abc_w2p0(i,1) + dl*da
C
             db  =   abc_w2p0(i+1,2)-abc_w2p0(i,2)
             b   =   abc_w2p0(i,2) + dl*db 
C
             dc  =   abc_w2p0(i+1,3)-abc_w2p0(i,3)
             c   =   abc_w2p0(i,3) + dl*dc 
C
          endif
      end do
C
C   for latitudes greater than 75 degrees:
      if (l .ge. lat_wmf(5)) then
         a = abc_w2p0(5,1)
         b = abc_w2p0(5,2)
         c = abc_w2p0(5,3)
      endif
C
C   Now the coefficients exist; calculate the mapping function, wmf(1), and the
C    change of mapping function with elevation, dwmf/d_el =wmf(2).
C   To calculate the delay-rate correction, d_tau/dt:
C       d_tau/dt = d_tau_zen/dt * wmf(1) + tau_zen * dwmf/d_el * d_el/dt 
C
      sine  = sin( elev )
      cose  = cos( elev )
      beta  = b/( sine + c )
      gamma = a/( sine + beta)
      topcon = (1.d0 + a/(1.d0 + b/(1.d0 + c)))
C
      wmf(1) = topcon / ( sine + gamma )
C
      wmf(2) = -topcon / ( sine + gamma )**2 *
     .         ( cose - a/( sine + beta)**2 * cose *
     .         ( 1.d0 - b/( sine + c )**2 ) )
C
C     Check for debug output.
      IF ( KATMD .ne. 0 )  Then
        WRITE ( 6, 9100 )
 9100   FORMAT (1X, "Debug output for subroutine NWMF2." )
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' latitude, elev ', latitude, elev
        WRITE(6,8)' a, b, c        ', a, b, c 
        WRITE(6,8)' dl, da, db, dc ', dl, da, db, dc
        write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
     .           sine, cose, beta, gamma, topcon
         write(6,8)' wmf(1), wmf(2) ', wmf
      Endif 
C
      return
      end
C
C******************************************************************************
      SUBROUTINE SASTD(P,PDOT,RLAT,SITEHT,dPdh,ZD,ZDDOT,dZDdh)
      IMPLICIT NONE
C
C     SASTD PROGRAM SPECIFICATION
C
C     Calculate zenith delay for "hydrostatic" component of the atmosphere using
C     Saastamoinen formulation and constants from Davis et al.
C
C     REFERENCES:
C       1) Saastamoinen, J. "The Use of Artificial Satellites for
C          Geodesy", Geophys. Monograph Series, vol. 15, ed. by
C          S.W.Henriksen et al, AGU, Wash., D.C., pp 247-251, 1972.
C       2) Davis, J.L., et al., "Geodesy by Radio Interferometry:
C          Effects of Atmospheric Modeling Errors On Estimates Of
C          Baseline Length", Radio Science, 20, 1593-1607, 1985.
C
      REAL*8 P, PDOT, RLAT, SITEHT, dPdh, ZD, ZDDOT, dZDdh

C  Calling Sequence:
C
C    Input Variables:
C      1. P      - Pressure. (mbar)
C      2. PDOT   - Rate of change of pressure, set to 0.0. (mbar/sec)
C      3. RLAT   - Latitude. (radians)
C      4. SITEHT - Height of station above geoid. (meters)
C      5. dPdh   - Derivative of pressure with respect to station height.
C
C    Output Variables:
C      1. ZD    - Zenith delay of "hydrostatic" component. (meters)
C      2. ZDDOT - Rate of change of zenith delay. (meters/second)
C      3. dZDdh - Derivative of ZD with respect to station height.
C
C     SUBROUTINE INTERFACE
C	CALLING SUBROUTINES - ATMP
C       CALLED SUBROUTINES  - None
C
      INCLUDE 'ccon.i'
C
      REAL*8 F, dFdh

C     Program Variables 
C       1. F    - Intermediate result in zenith delay calculation
C       2. dFdh - Derivative of F with respect to height.
C
C     Programmers:
C      ??.??.??  JMG,AEN??  Created?
C      94.02.15  D. Gordon  Added partials of ZD with respect to height.
C      95.12.18  D. Gordon  Documentation made 'Calc'-like, debug printout
C                           section added.
C
C     SASTD PROGRAM STRUCTURE
C
C-----Calculate variation of gravity with station position.
C
      F = 1.0D0-0.00266D0*COS(2.0D0*RLAT) - 0.00028D0*SITEHT/1000.0D0
      ZD = 0.0022768D0*P/F
      ZDDOT = ZD*PDOT/P
C
C 94FEB15 - take partials with respect to height -DG-
      dFdh = -2.8D-7
      dZDdh = 0.0022768D0 * (dPdh  - P*dFdh/F) / F
C  
      If (KATMD .ne. 0)  Then
        WRITE ( 6, 9100 )
 9100   FORMAT (1X, "Debug output for subroutine SASTD." )
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' P, PDOT, dPdh ', P, PDOT, dPdh   
        WRITE(6,8)' RLAT, SITEHT  ', RLAT, SITEHT
        WRITE(6,8)' ZD, ZDDOT, dZDdh ', ZD, ZDDOT, dZDdh  
        WRITE(6,8)' F, dFdh   ', F, dFdh   
      Endif
C
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE SASTW(RH,TC,RHDOT,TCDOT,ZW,ZWDOT)
      IMPLICIT NONE
C
C     SASTW PROGRAM SPECIFICATION
C
C     Calculate zenith delay due to the 'wet' (non-hydrostatic) component of
C     the atmosphere using Saastamoinen formula (19a) (Ref. 1).  The saturation
C     vapor pressure is calculated using the formula found in MET03,  which is
C     much simpler than that found in "Methods of Experimental Physics B"
C    (1976) p. 187, but agrees to 0.3% over the range 0 - 40 deg. C.
C
C     REFERENCES:
C       1) Saastamoinen, J. "The Use of Artificial Satellites for
C          Geodesy", Geophys. Monograph Series, vol. 15, ed. by
C          S.W.Henriksen et al, AGU, Wash, D.C., pp 247-251, 1972.
C       2) Davis, J.L., et al., "Geodesy by Radio Interferometry:
C          Effects of Atmospheric Modeling Errors on Estimates of
C          Baseline Length", Radio Science, 20, 1593-1607, 1985.
C
C     SASTW INTERFACE
C
      REAL*8 RH, TC, RHDOT, TCDOT, ZW, ZWDOT
C
C     Input Variables:
C       1. RH - Relative humidity (set equal to .5).  (0 <= RH <= 1.0)
C       2. TC - Temperature (a function of height). (Celsius)
C       3. RHDOT - Time derivative of RH (set to 0.0).
C       4. TCDOT - Time derivative of TC (set to 0.0).
C
C     Output Variables:
C       1. ZW    - Zenith path delay of wet component. (meters)
C       2. ZWDOT - Time derivative of zenith delay. (meters/sec)
C
C     SUBROUTINE INTERFACE
C	CALLING SUBROUTINES - ATMP
C       CALLED SUBROUTINES  - None
C
      INCLUDE 'ccon.i'
C
      REAL*8 ESAT, TEMP, ESATDOT
C
C     Program Variables:
C       1. ESAT    - Saturation vapor pressure.
C       2. ESATDOT - Time derivative of saturation pressure.
C       3. TEMP    - Used for intermediate values in calculations.
C
C     Programmers:
C      ??.??.??  ?????????  Created?
C      95.12.18  D. Gordon  Documentation made 'Calc'-like, debug printout
C                           section added.
C
C    SASTW PROGRAM STRUCTURE
C
C---Calculate zenith delay due to'wet' (non-hydrostatic) component
C     of atmosphere using Saastamoinen formula 19a. (Ref 1).
C
C     The saturation vapor pressure is calculated using the formula
C     found in MET03 which is much simpler than that found in Methods
C     of Experimental Physics B (1976) p. 187, but agrees to 0.3%
C     over the range 0 - 40 deg. C.
C
C---Calculate saturation vapor pressure, and time derivative.
C     ESAT = 6.11 * 10**(7.5*TC/(TC+237.3))
      TEMP = TC+237.3D0
      ESAT = 6.11D0 * EXP(17.269D0*TC/TEMP)
      ESATDOT = ESAT * (17.269D0/TEMP - 17.269D0*TC/(TEMP*TEMP))*TCDOT
C
C---Calculate zenith path delay
      ZW = 0.002277D0 * (1255.D0/(TC+273.16D0) + 0.05D0) * RH * ESAT
C  
C   ... AND RATE OF CHANGE OF ZENITH PATH DELAY
C       CHANGED 255. TO 1255. IN TCDOT CONTRIBUTION       12-28-87 WEH
C       CHANGED RHDOT CONTRIBUTION FROM ZW*RHDOT/RH       12-28-87  WEH
      TEMP = 273.16D0+TC
      ZWDOT = - 0.002277D0 * (TCDOT*1255.D0/(TEMP*TEMP))*RH*ESAT
     >  + 0.002277D0 * (1255.D0/(TC+273.16D0) + 0.05D0) * RHDOT * ESAT
     >  + ZW * ESATDOT/ESAT
C  
      If (KATMD .ne. 0)  Then
        WRITE ( 6, 9100 )
 9100   FORMAT (1X, "Debug output for subroutine SASTW." )
    8   FORMAT(A,4D25.16/(7X,5D25.16))
        WRITE(6,8)' RH, RHDOT ', RH, RHDOT
        WRITE(6,8)' TC, TCDOT ', TC, TCDOT
        WRITE(6,8)' ZW, ZWDOT ', ZW, ZWDOT
        WRITE(6,8)' ESAT, TEMP, ESATDOT ', ESAT, TEMP, ESATDOT
      Endif
C
      RETURN
      END
C
C******************************************************************************
      BLOCK DATA ATMCMB
      Implicit none
C
C     ATMBD is the atmosphere module block data input and initialization section
C
C     References - Hopfield, H.S., 'Radio Science', 6, 357, 1971,
C                                    'JSR', 74, 4487, 1969.
C                - Chau, C.C., 'JPL Technical Manual', #391-129, 1970.
C                - SMART, W.M., 'Textbook on Spherical Astronomy', 1965, P. 68.
C                - Chau, C.C., 'The Tropospheric Calibration Model for Mariner
C                    Mars 1971", JPL Technical Report 32-1587, (NASA Sci & Tech
C                    Info Facility N&$-16983) p. 75, eq. 19 - wet formulation.
C
C     Common block -
C
      Real*8 RF(2), RTRACE(2), wetcon(2), Datmp_hmf(2,2), 
     .       Datmp_wmf(2,2), Zen_dry(2,2), Zen_wet(2,2)
      COMMON /ATMCM/ RF, RTRACE, wetcon, Datmp_hmf, Datmp_wmf, Zen_dry,
     .               Zen_wet
C
C        Variables 'to':
C          1. RF(2)      - The constants appearing in terms 1 and 2 of the
C                          calculation of the correction angle to be added to
C                          the source elevation angle to find the apparent
C                          source direction due to tropospheric refraction. 
C                           RF(1) = 2.826172873D-4 radians
C                           RF(2) = -3.23855539D-7 radians
C           2. RTRACE(2) - The constants appearing in the calculation of the
C                          partial derivatives of the delay and rate with
C                          respect to the zenith path delay at each observing
C                          site. These constants were determined from the
C                          best fit of the ray tracing algorithm for
C                          atmospheric thickness from the zenith to the 
C                          horizon. (unitless)  (See references)
C                           RTRACE(1) = 1.43D-3
C                           RTRACE(2) = 4.45D-2 
C           3. wetcon(2) - The same as RTRACE, except that RTRACE is
C                          appropriate for the dry atmosphere and wetcon is
C                          appropriate for the wet.
C           4. lat_hmf(5)- Latitudes at which coefficients are defined for the
C                          Niell mapping function.
C           5. abc_avg(5,3) - Continued fraction coefficients at latitudes
C                          lat_hmf
C           6. abc_amp(5,3) - Amplitude of annual variation of abc_avg.
C           7. a_ht, b_ht, c_ht - Parameters for continued fraction for height
C                          correction.
C
      Real*8 lat_hmf(5), abc_avg(5,3), abc_amp(5,3), a_ht, b_ht, c_ht
      Common / hmf2_coef/ lat_hmf, abc_avg, abc_amp, a_ht, b_ht, c_ht 
C
C     Program specifications -
C
C   Define parameters used for calculating coefficients.
C
      Data lat_hmf / 15.D0, 30.D0, 45.D0, 60.D0, 75.D0/
C
      Data abc_avg / 
     .1.2769934D-3,1.2683230D-3,1.2465397D-3,1.2196049D-3,1.2045996D-3,
     .2.9153695D-3,2.9152299D-3,2.9288445D-3,2.9022565D-3,2.9024912D-3,
     .62.610505D-3,62.837393D-3,63.721774D-3,63.824265D-3,64.258455D-3/
C
      Data abc_amp / 
     .  0.0,   1.2709626D-5, 2.6523662D-5, 3.4000452D-5, 4.1202191D-5,
     .  0.0,   2.1414979D-5, 3.0160779D-5, 7.2562722D-5, 11.723375D-5,
     .  0.0,   9.0128400D-5, 4.3497037D-5, 84.795348D-5, 170.37206D-5/
C
      Data a_ht / 2.53D-5/
      Data b_ht / 5.49D-3/
      Data c_ht / 1.14D-3/
C
      Data  RF     / 2.826172873D-4, -3.23855539D-7 /
      Data  RTRACE / 1.43D-3, 4.45D-2 /
      Data  wetcon / 0.35d-3, 1.7 d-2 /
C
C    Constants used - RF, RTRACE
C
C    Programmer - Dale Markham  01/13/77
C     77.07.11  Peter Denatale
C     89.05.24  Jim Ryan - Wet partials info added.
C
      END
