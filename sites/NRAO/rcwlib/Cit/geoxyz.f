      SUBROUTINE GEOXYZ( MODE, LONG, LAT, HT, X, Y, Z, IER )
C
C     Routine to make conversions between geodetic and geocentric
C     coordinates.  There are two modes:
C       MODE=0:   Input data are longitude, latitude and height,
C                 either goecentric or geodetic (height < 1E6)
C                 Output data are geodetic long, lat, and ht and
C                 geocentric X, Y, and Z.
C       MODE=1:   Input data are geocentric X, Y, and Z.
C                 Output data are geodetic long, lat, and height
C                 above the geoid.
C
C       IER is a return code.  0 => ok.  1 => bad mode.
C       
C     All input variables are double precision.  Longitude and
C     latitude are in radians.  HT, X, Y, and Z are in meters.
C
C     Algorithms from the 1992 Astronomical Almanac Explanatory
C     Supplement, Chapter 4, are used.  The IERS 1989 ellipsoid
C     is used (Semi-major axis and inverse flattening).
C
C     Note that the sign of longitude is backwards from a right
C     handed coordinate system.
C
C     On 29 Jan 2002, I tried some tests against Web conversion sites.
C     The equations here clearly apply to WGS84
C     There is an example at http://www.anzlic.org.au/icsm/gdatm/xyzcd.htm
C       An australian site.
C     For 
C     station=TEST     elev=737.574 lat=-37:39:10.1598 long=143:55:35.3730 
C     Sched gets with      SMAXIX=6378136.D0 and FLATEN=1.D0/298.257D0
C       SCHED:         -4087094.75 -2977467.03 -3875456.72 
C     The web site uses XMAXIX=6378137.D0 and FLATTEN=1.D0/298.25722210
C       Web example    -4087095.384 2977467.494 -3875457.340
C     Change this code to  SMAXIS=6378137.D0 FLATEN=1.D0/298.257D0
C       SCHED:         -4087095.39 -2977467.50 -3875457.32 
C     This now agrees at the 1cm level.  Try for other opinions on A and 1/f
C     At a laser site at gsfc, 6378136.3 and 298.257
C     At a USNO site, 6378137  298.257222101
C     In the WGSMAN24.PDF document that I have in my geodesy area, 
C     the values are 6378137  298.257223563.  This is a European document.
C     Modify 1/f to the USNO site value.
C     SCHED (6378137  298.257222101)  -4087095.39 -2977467.49 -3875457.34  
C     This agrees with the Ausi site to under a cm.
C     From NIMA, which Bryan Butler figures is the best reference, the
C     values to use are  6378137  298.257223563 - same as the European site.
C     So now install those values.
C
C     R. C. Walker
C
      INTEGER           MODE, IER
      DOUBLE PRECISION  LONG, LAT, HT, X, Y, Z
      DOUBLE PRECISION  SMAXIS, FLATEN, C, S
      PARAMETER         (SMAXIS=6378137.D0)
      PARAMETER         (FLATEN=1.D0/298.257223563D0)
      DOUBLE PRECISION  SLAT, CLAT, SLONG, CLONG
      DOUBLE PRECISION  B, R, E, F, P, Q, D, NU, G, T, PI
      LOGICAL           MAKEGD
C ----------------------------------------------------------------
      PI = DATAN( 1.0D0 ) * 4.D0
      MAKEGD = MODE .EQ. 1
      IER = 1
      IF( MODE .EQ. 0 ) THEN
C
C        Input is long, lat, and ht of some sort.
C
         IF( HT .LT. 1.D6 ) THEN
C
C           Input coordinates are geodetic long, lat, ht.
C
            SLAT = DSIN( LAT )
            CLAT = DCOS( LAT )
            SLONG = DSIN( -LONG )
            CLONG = DCOS( -LONG )
            C = CLAT**2 + ( 1.D0 - FLATEN )**2 * SLAT**2
            C = 1.D0 / SQRT( C )
            S = ( 1.D0 - FLATEN )**2 * C
            X = ( SMAXIS * C + HT ) * CLAT * CLONG
            Y = ( SMAXIS * C + HT ) * CLAT * SLONG
            Z = ( SMAXIS * S + HT ) * SLAT
            IER = 0

         ELSE
C
C           Input coordinates are geocentric long, lat, ht.
C           Derive X, Y, and Z.  Then convert angles to 
C           geodetic in section for data starting with X, Y, Z.
C
            X = HT * COS( LAT ) * COS( -LONG )
            Y = HT * COS( LAT ) * SIN( -LONG )
            Z = HT * SIN( LAT )
            MAKEGD = .TRUE.
         END IF
      END IF
C
C     Now do any required geocentric to geodetic conversion.
C
      IF( MAKEGD ) THEN
         R = SQRT( X**2 + Y**2 )
C
C        Deal with singular cases.
C
         IF( Z .EQ. 0.D0 .AND. R .NE. 0.D0 ) THEN
C
C           On equitorial plane.
C
            LAT = 0.D0
            LONG = -DATAN2( Y, X )
            HT = SQRT( X**2 + Y**2 ) - SMAXIS
C
         ELSE IF( R .EQ. 0.D0 .AND. Z .NE. 0.D0 ) THEN
C
C           On Z axis.
C
            LAT = DSIGN( PI / 2.D0, Z )
            LONG = 0.D0
            HT = Z - SMAXIS * ( 1.D0 - FLATEN )
C
         ELSE IF( R .EQ. 0.D0 .AND. Z .EQ. 0.D0 ) THEN
C
C           Center of earth.
C
            LAT = 0.D0
            LONG = 0.D0
            HT = -SMAXIS
         ELSE
C
C           Reasonable range.
C
            B = DSIGN( SMAXIS * ( 1.D0 - FLATEN ), Z )
            E = ( B * Z - ( SMAXIS**2 - B**2 ) ) / ( SMAXIS * R )
            F = ( B * Z + ( SMAXIS**2 - B**2 ) ) / ( SMAXIS * R )
            P = ( 4.0D0 / 3.0D0 ) * ( E * F + 1.D0 )
            Q = 2.D0 * ( E**2 - F**2 )
            D = P**3 + Q**2
            IF( D .LT. 0.D0 ) THEN
               NU = 2.D0 * SQRT( -P ) * COS( (1.D0/3.D0) *
     1              ACOS( Q / ( P * SQRT( -P ) ) ) )
            ELSE
               NU = ( SQRT( D ) - Q )**(1.D0/3.D0) - 
     1              ( SQRT( D ) + Q )**(1.D0/3.D0)
            END IF
C
C           Deal with cases near singularities as per Almanac.
C           The criteria are a wild guess.
C
            IF( Z .LT. SMAXIS / 1.D5 .OR. R .LT. SMAXIS / 1.D5 ) THEN
               NU = (-1.D0) * ( NU**3 + 2.D0 * Q ) / ( 3.D0 * P )
            END IF
            G = 0.5D0 * ( SQRT( E**2 + NU ) + E )
            T = SQRT( G**2 + ( F - NU * G ) / ( 2.D0 * G - E ) ) - G
            LAT = DATAN( SMAXIS * ( 1.D0 - T**2 ) / ( 2.D0 * B * T ) )
            HT = ( R - SMAXIS * T ) * COS( LAT ) + 
     1           ( Z - B ) * SIN( LAT )
            LONG = -DATAN2( Y, X )
         END IF
         IER = 0
      END IF
C
      RETURN
      END
