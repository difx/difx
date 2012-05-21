      INTEGER FUNCTION RDSTA( IUNIT, OPENIT, FILE )
C
C    RDSTA is a routine to read an entry of the station catalog.
C    The catalog is assumed to be in a KEYIN readable format.
C
C    Input parameters:
      INTEGER        IUNIT    ! Unit number read from.
      LOGICAL        OPENIT   ! Open file on first call.  See below.
      CHARACTER*(*)  FILE     ! File name of catalog.
C
C    The value of RDSTA on return is:
C         0  if a line was successfully read.
C         1  if an end-of-file or an "ENDCAT" is encountered.  All 
C            catalog entries should terminate with a '/' so RDSTA=1, 
C            assume that no new data has been read.
C
C    Data from the catalog is returned via COMMON's specified in the 
C    include file, rdcat.inc.  The calling program should use that 
C    file.  If new parameters are added to the catalog, it should 
C    only be necessary to relink old programs that don't use them.
C
C    OPENIT controls whether or not the catalog file is opened on
C    the first read and closed when an EOF or parameter ENDCAT is 
C    reached.
C    Set OPENIT=.TRUE. for normal reading of external catalog files.
C    Set OPENIT=.FALSE. if no open or close is desired - eg. if the 
C    catalog information is imbedded in the calling program's main 
C    input.
C    Use ENDCAT to terminate catalog input in such cases.
C
C    R. C. Walker, Dec. 19, 1988.
C
C    Changed default NDRIVES to 1, July 22, 2010.  Then changed
C    it back to 2 to keep various wideband code happy.  RCW.
C
C    Include file. 
C
      INCLUDE 'rdcat.inc'
C
C     Constants.
C
      REAL*8         PI, RADDEG, UNSET
      PARAMETER      (PI=3.141592653589793238D0)
      PARAMETER      (RADDEG=PI/180.D0)
      PARAMETER      (UNSET=-9999.D0)
C
C     Internal variables.
C
      INTEGER        MODE, IER, VLBOPE, LEN1, IDB
      LOGICAL        FOPEN, NOLOC, LOCWARN, LOCNONE, GOTLOC
      REAL*8         NONAME, XX, NONE, ALTAZ, CVLBA, BLANK, CCONT
      CHARACTER      CNAME*50, LOCTST*80
      CHARACTER      RESULT*255
      SAVE           FOPEN, NONAME, XX, NONE, ALTAZ, CVLBA, BLANK
      SAVE           NOLOC, LOCWARN, CCONT
C
C     Catalog input parameters.
C
      INTEGER           MSP, I, I1, I2
      PARAMETER         (MSP = 500)
      INTEGER           KI(MSP), KEYPTR
      CHARACTER         KC(MSP)*8, KCHAR*256
      DOUBLE PRECISION  KD(MSP*2), ENDMRK
      LOGICAL           GOTKEYS, GOTXYZ, GOTLLH, WARNXYZ
      SAVE              KI, KC, KD, GOTKEYS, ENDMRK, WARNXYZ, LOCTST
      SAVE              GOTLOC
C
      DATA   FOPEN             / .FALSE. /
      DATA   GOTKEYS           / .FALSE. /
      DATA   (KI(I),I=1,3)     / MSP, 0, 3 /
C --------------------------------------------------------------------
C     Establish the key names if required.
C
      IF( .NOT. GOTKEYS ) THEN
         CALL KPACK( '/       ', ENDMRK )
         CALL KPACK( 'NONAME  ', NONAME )
         CALL KPACK( 'XX      ', XX )
         CALL KPACK( 'NONE    ', NONE )
         CALL KPACK( 'ALTAZ   ', ALTAZ )
         CALL KPACK( 'VLBA    ', CVLBA )
         CALL KPACK( 'CONT    ', CCONT )
         CALL KPACK( '        ', BLANK )
         CALL KEYCHR( 'VERSION', ' ', 20, KD, KC, KI )
         CALL KEYCHR( 'STAtion', ' ', 8, KD, KC, KI )
         CALL KEYCHR( 'STCode', ' ', 3, KD, KC, KI )
         CALL KEYCHR( 'DBNAME', ' ', 10, KD, KC, KI )
         CALL KEYCHR( 'DBCODE', ' ', 4, KD, KC, KI )
         CALL KEYCHR( 'FRAME', ' ', 80, KD, KC, KI )
         CALL KEYADD( 'ELev', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'LAT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'LONG', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'ZALim', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'X', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'Y', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'Z', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DXDT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DYDT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DZDT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'EPOCH', 0.D0, 1, KD, KC, KI )
         CALL KEYCHR( 'DEScrip', ' ', 80, KD, KC, KI )
         CALL KEYCHR( 'CONtrol', ' ', 5, KD, KC, KI )
         CALL KEYCHR( 'DAR', ' ', 5, KD, KC, KI )
         CALL KEYCHR( 'RECORDER', ' ', 6, KD, KC, KI )
         CALL KEYADD( 'NDRIVEs', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'NHEADs', 0.D0, 1, KD, KC, KI )
         CALL KEYCHR( 'DISK', ' ', 6, KD, KC, KI )
         CALL KEYCHR( 'DISC', ' ', 6, KD, KC, KI )
         CALL KEYCHR( 'MEDIADEF', ' ', 6, KD, KC, KI )
         CALL KEYADD( 'NBBC', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'HOR_AZ', 0.D0, MHOR, KD, KC, KI )
         CALL KEYADD( 'HOR_EL', 0.D0, MHOR, KD, KC, KI )
         CALL KEYADD( 'AX1LIM', 0.D0, 6, KD, KC, KI )
         CALL KEYADD( 'AX2LIM', 0.D0, 6, KD, KC, KI )
         CALL KEYADD( 'AX1RATE', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'AX2RATE', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'AX1ACC', 0.D0, 2, KD, KC, KI )
         CALL KEYADD( 'AX2ACC', 0.D0, 2, KD, KC, KI )
         CALL KEYADD( 'MOUNT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'AXISTYPE', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'AXISOFF', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'TSETTLE', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'MINSETUP', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'MAXSRCHR', 1.D6, 1, KD, KC, KI )
         CALL KEYCHR( 'TSCAL', 'CONT', 4, KD, KC, KI )
         CALL KEYADD( 'ENDCAT', 0.D0, 1, KD, KC, KI )
         GOTKEYS = .TRUE.
         WARNXYZ = .TRUE.
         GOTLOC  = .FALSE.
      END IF
C
C     Try to get the locations file.  This file may
C     or may not exist and may or may not be needed.  Therefore
C     RDLOC will return gracefully if the file cannot be opened
C     and we can deal with the situation here.
C
C     RDLOC fills the "DB" arrays with antenna positions etc.
C     It can use the same unit number because, when RDSTA gets
C     here, the stations file is not open and RDLOC will close
C     the locations file when it is done.
C
C     Note GOTLOC just means that we have successfully read a
C     locations file.  NOLOC means we don't have one either because
C     of a failed read or because none was requested.
C
      IF( .NOT. GOTLOC ) THEN
         LOCTST = LOCFILE
         CALL UPCASE( LOCTST )
         LOCNONE = LOCTST .EQ. 'NONE' .OR. LOCTST .EQ. 'NOLOC'
         IF( .NOT. LOCNONE ) THEN
            CALL RDLOC( IER )
            NOLOC = IER .NE. 0
            IF( IER .EQ. 0 ) GOTLOC = .TRUE.
         ELSE
            NOLOC = .TRUE.
         END IF
         LOCWARN = .TRUE.
      END IF
C
C     Open the file if requested.
C
      IF( OPENIT .AND. .NOT. FOPEN ) THEN
         IER = VLBOPE( IUNIT, FILE, 'TEXT', 'OLD', RESULT )
         IF( IER .EQ. 0 ) THEN
            CALL PUTOUT( RESULT )
            CNAME = FILE
            CALL ERROR( 'Station catalog: '//CNAME//' not opened' )
         END IF
         FOPEN = .TRUE. 
      END IF
C
C     Read the catalog line with KEYIN.  
C
C     Reset all parameters before start (except version).
C     Use defaults that are ok for the VLBA to 
C     smooth the transition to new catalogs.
C
      MODE = 0
      KD( KEYPTR( 'STAtion', KC, KI ) ) = NONAME
      KD( KEYPTR( 'STCode', KC, KI ) ) = XX
      KD( KEYPTR( 'DBNAME', KC, KI ) ) = BLANK
      KD( 1 + KEYPTR( 'DBNAME', KC, KI ) ) = BLANK
      KD( KEYPTR( 'DBCODE', KC, KI ) ) = BLANK
      KD( KEYPTR( 'FRAME', KC, KI ) ) = XX
      KD( KEYPTR( 'CONtrol', KC, KI ) ) = BLANK
      KD( KEYPTR( 'DAR', KC, KI ) ) = BLANK
      KD( KEYPTR( 'RECORDER', KC, KI ) ) = BLANK
      KD( KEYPTR( 'DISK', KC, KI ) ) = BLANK
      KD( KEYPTR( 'DISC', KC, KI ) ) = BLANK
      KD( KEYPTR( 'MEDIADEF', KC, KI ) ) = BLANK
      KD( KEYPTR( 'ELev', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'LAT', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'LONG', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'X', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'Y', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'Z', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'DXDT', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'DYDT', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'DZDT', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'EPOCH', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'NDRIVEs', KC, KI ) ) = 2.0D0
      KD( KEYPTR( 'NHEADs', KC, KI ) ) = 1.0D0
      KD( KEYPTR( 'NBBC', KC, KI ) ) = 8.0D0
      KD( KEYPTR( 'AXISTYPE', KC, KI ) ) = BLANK
      KD( KEYPTR( 'AXISOFF', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'TSETTLE', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'MINSETUP', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'MAXSRCHR', KC, KI ) ) = 1.0D6
      KD( KEYPTR( 'TSCAL', KC, KI ) ) = CCONT
      KD( KEYPTR( 'ENDCAT', KC, KI ) ) = UNSET
C
C     Reset the horizon mask.
C
      I1 = KEYPTR( 'HOR_AZ', KC, KI ) - 1
      I2 = KEYPTR( 'HOR_EL', KC, KI ) - 1
      DO I = 1, MHOR
         KD(I1+I) = UNSET
         KD(I2+I) = 0.D0
      END DO
C
C     Reset the slew and limit information.  Defaults are for an 
C     extremely fast antenna with VLBA constraints.
C
      KD( KEYPTR( 'ZALim', KC, KI ) ) = UNSET
      I1 = KEYPTR( 'AX1LIM', KC, KI ) - 1
      I2 = KEYPTR( 'AX2LIM', KC, KI ) - 1
      KD(I1+1) = -90.D0
      KD(I1+2) = 450.D0
      KD(I2+1) = 2.D0
      KD(I2+2) = 90.D0
      DO I = 3, 6
         KD(I1+I) = UNSET
         KD(I2+I) = UNSET
      END DO
      KD( KEYPTR( 'AX1RATE', KC, KI ) ) = 1000.0D0
      KD( KEYPTR( 'AX2RATE', KC, KI ) ) = 1000.0D0
      KD( KEYPTR( 'AX1ACC', KC, KI ) ) = 1000.0D0
      KD( KEYPTR( 'AX2ACC', KC, KI ) ) = 1000.0D0
      KD( 1 + KEYPTR( 'AX1ACC', KC, KI ) ) = 0.0D0
      KD( 1 + KEYPTR( 'AX2ACC', KC, KI ) ) = 0.0D0
      KD( KEYPTR( 'MOUNT', KC, KI ) ) = ALTAZ
C
C     Get the input data for the next line.
C
      CALL KEYIN( KD(MSP+1), KD, KI(2), ENDMRK, MODE, IUNIT, 6 )
C
      IF( MODE .EQ. 1 .OR. 
     1    KD( KEYPTR( 'ENDCAT', KC, KI ) ) .EQ. 0.D0 ) THEN
         IF( OPENIT .AND. FOPEN ) CLOSE( UNIT=IUNIT )
         FOPEN = .FALSE.
         RDSTA = 1
         GO TO 990
      END IF
C
C     Decode input
C
      RDSTA = 0
      STAVER = KCHAR( 'VERSION', 20, .FALSE., KD, KC, KI )
      STANAM = KCHAR( 'STAtion', 8, .FALSE., KD, KC, KI )
      STACOD = KCHAR( 'STCode', 3, .FALSE., KD, KC, KI )
      STADBN = KCHAR( 'DBNAME', 10, .FALSE., KD, KC, KI )
      STADBC = KCHAR( 'DBCODE', 4, .FALSE., KD, KC, KI )
      STAFRM = KCHAR( 'FRAME', 80, .FALSE., KD, KC, KI )
      STACTL = KCHAR( 'CONtrol', 5, .TRUE., KD, KC, KI )
      STADAR = KCHAR( 'DAR', 5, .TRUE., KD, KC, KI )
      STAREC = KCHAR( 'RECORDER', 6, .TRUE., KD, KC, KI )
      STADSK = KCHAR( 'DISK', 6, .TRUE., KD, KC, KI )
      IF( STADSK .EQ. ' ' ) THEN
         STADSK = KCHAR( 'DISC', 6, .TRUE., KD, KC, KI )
      END IF
      STAMD = KCHAR( 'MEDIADEF', 6, .TRUE., KD, KC, KI )
      IF( STAMD .EQ. 'DISC' ) STAMD = 'DISK'
      STAEL  = KD( KEYPTR( 'ELev', KC, KI ) )
      STALAT = KD( KEYPTR( 'LAT', KC, KI ) ) * RADDEG / 3600.D0
      STALON = KD( KEYPTR( 'LONG', KC, KI ) ) * RADDEG / 3600.D0
      STAX   = KD( KEYPTR( 'X', KC, KI ) )
      STAY   = KD( KEYPTR( 'Y', KC, KI ) )
      STAZ   = KD( KEYPTR( 'Z', KC, KI ) )
      STADX  = KD( KEYPTR( 'DXDT', KC, KI ) )
      STADY  = KD( KEYPTR( 'DYDT', KC, KI ) )
      STADZ  = KD( KEYPTR( 'DZDT', KC, KI ) )
      STAEPO = KD( KEYPTR( 'EPOCH', KC, KI ) )
      STANDR = KD( KEYPTR( 'NDRIVEs', KC, KI ) )
      STANHD = KD( KEYPTR( 'NHEADs', KC, KI ) )
      STANBC = KD( KEYPTR( 'NBBC', KC, KI ) )
      STADBA = KCHAR( 'AXISTYPE', 5, .FALSE., KD, KC, KI )
      STAOFF = KD( KEYPTR( 'AXISOFF', KC, KI ) )
C
C     Note that STADBA and STAOFF will be changed if we have to get
C     positions from the locations catalog.
C
C     Fill in any missing coordinates.  Also ensure that any 
C     spherical geocentric coordinates are converted to geodetic.
C
      GOTXYZ = STAX .NE. 0.D0 .OR. STAY .NE. 0.D0 .OR. STAZ .NE. 0.D0
      GOTLLH = STAEL.NE.0.D0 .OR. STALAT.NE.0.D0 .OR. STALON.NE.0.D0
C
C     Issue a warning, once, if XYZ and Lat Long in same stations.
C
      IF( GOTXYZ .AND. GOTLLH .AND. WARNXYZ ) THEN
         CALL PUTOUT( 'RDSTA: Both XYZ and Lat/Long coordinates '//
     1          'given for some stations.' )
         CALL PUTOUT( '       There is nothing to insure they agree.' )
         WARNXYZ = .FALSE.
      END IF      
C
C     Get any missing coordinates from what is present.
C
      IF( GOTXYZ .AND. ( .NOT. GOTLLH .OR. STAEL .GE. 1.E6 ) ) THEN
C
         CALL GEOXYZ( 1, STALON, STALAT, STAEL, STAX, STAY, STAZ, IER )
         IF( IER .NE. 0 ) CALL PUTOUT( 
     1     'RDSTA: Problem with coordinate conversions for '// STANAM )
C
      ELSE IF( GOTLLH .AND. .NOT. GOTXYZ ) THEN
C
         CALL GEOXYZ( 0, STALON, STALAT, STAEL, STAX, STAY, STAZ, IER )
         IF( IER .NE. 0 ) CALL PUTOUT( 
     1     'RDSTA: Problem with coordinate conversions for '// STANAM )
C
C     If coordinates missing, try to get them from the locations
C     file and complain if you can't.  The LOCTST test is probably
C     SCHED specific, but should not hurt elsewhere.
C
      ELSE IF( .NOT. GOTLLH .AND. .NOT. GOTXYZ ) THEN
         IF( NOLOC .OR. NDB .EQ. 0 ) THEN
            IF( LOCWARN ) THEN
               IF( LOCTST .EQ. 'NOLOC' ) THEN
                  CALL PUTOUT( 'RDSTA: **Cannot use locations catalog '
     1               // 'with in-line stations catalog.' )
               ELSE IF( .NOT. LOCNONE ) THEN
                  CALL PUTOUT( 
     1               'RDSTA:  Could not open locations catalog: '//
     2               LOCFILE(1:LEN1(LOCFILE)) )
               END IF
            END IF
            LOCWARN = .FALSE.
            CALL PUTOUT( 'RDSTA:   No coordinates for '//STANAM )
         ELSE
            DO IDB = 1, NDB
C
C              Get data from the stations catalog.  
C              Note that the mount type is given in both the 
C              stations catalog and the locations catalog.  For now,
C              ignore the one in the locations catalog.  Someday,
C              perhaps check for consistency.
C              Also note that the locations catalog, and the VLBA
C              data base, has 4 character station codes.
C
               IF( STADBN .EQ. DBNAME(IDB) ) THEN
                  STADBC = DBCODE(IDB)
                  STAX = DBX(IDB)
                  STAY = DBY(IDB)
                  STAZ = DBZ(IDB)
                  STADX = DBDX(IDB)
                  STADY = DBDY(IDB)
                  STADZ = DBDZ(IDB)
                  STAEPO = DBEPO(IDB)
                  STAFRM = DBFRM(IDB)
                  STAOFF = DBOFF(IDB)
                  STADBA = DBDBA(IDB)
C
                  GOTXYZ = .TRUE.
                  GO TO 300
               END IF
            END DO
  300       CONTINUE
C
C           Get lat, long, el if have coordinate or complain if don't
C
            IF( GOTXYZ ) THEN
               CALL GEOXYZ( 1, STALON, STALAT, STAEL, 
     1                      STAX, STAY, STAZ, IER )
               IF( IER .NE. 0 ) CALL PUTOUT( 
     1             'RDSTA: Problem with locations file coordinate '//
     2             'conversions for '// STANAM )
            ELSE
               CALL PUTOUT( 'RDSTA: No coordinates in stations or' //
     1                ' locations file for '//STANAM )
            END IF
         END IF
      END IF
C
C     Slew and limit information.
C     Assume that, if the deceleration (element 2 of STAAC1 and STAAC2)
C     is not set that it is the same as the acceleration.
C
      STAZAL = KD( KEYPTR( 'ZALim', KC, KI ) )
      STARA1 = KD( KEYPTR( 'AX1RATE', KC, KI ) )
      STARA2 = KD( KEYPTR( 'AX2RATE', KC, KI ) )
      STAAC1(1) = KD( KEYPTR( 'AX1ACC', KC, KI ) )
      STAAC1(2) = KD( 1 + KEYPTR( 'AX1ACC', KC, KI ) )
      IF( STAAC1(2) .EQ. 0.0 ) STAAC1(2) = STAAC1(1)
      STAAC2(1) = KD( KEYPTR( 'AX2ACC', KC, KI ) )
      STAAC2(2) = KD( 1 + KEYPTR( 'AX2ACC', KC, KI ) )
      IF( STAAC2(2) .EQ. 0.0 ) STAAC2(2) = STAAC2(1)
      STAMNT = KCHAR( 'MOUNT', 5, .FALSE., KD, KC, KI )
C
      STANAX = 3
      I1 = KEYPTR( 'AX1LIM', KC, KI ) - 1
      I2 = KEYPTR( 'AX2LIM', KC, KI ) - 1
      DO I = 6, 1, -1
         STAAX1(I) = KD(I1+I)
         STAAX2(I) = KD(I2+I)
         IF( STAAX1(I) .EQ. UNSET .OR. STAAX2(I) .EQ. UNSET ) THEN
            STANAX = ( I - 1 ) / 2 
         END IF
      END DO
C
C     Settling time, minimum setup time, and max sources per hour.
C     STASTL is the number of seconds to be added to the time for a 
C     slew.  STAMSU is the minimum time for a scan change.
C     STAMSH is the maximum number of sources (slews) per hour.  That
C     is something needed for Jodrell.
C
      STASTL = KD( KEYPTR( 'TSETTLE', KC, KI ) )
      STAMSU = KD( KEYPTR( 'MINSETUP', KC, KI ) )
      STAMSH = KD( KEYPTR( 'MAXSRCHR', KC, KI ) )
C
C     An indicator of how the Tsys measrurements are done.
C
      STATSC = KCHAR( 'TSCAL', 4, .TRUE., KD, KC, KI )
C
C     For backward compatibility, extract an hour angle limit from
C     the more general limits above.  This is not used in SCHED.
C     Also make sure STAZAL is reasonable.
C
      IF( STAMNT .EQ. 'EQUAT' ) THEN
         STAHAL = 0.0
         DO I = 1, 2*STANAX
            STAHAL = MAX( STAHAL, ABS( STAAX1(I) ) )
         END DO
      ELSE
         STAHAL = 12.0
      END IF
C
      IF( KD( KEYPTR( 'ZALim', KC, KI ) ) .EQ. UNSET ) THEN
         IF( STAMNT .EQ. 'ALTAZ' ) THEN
            STAZAL = 90.0 - STAAX2(1)
         ELSE 
            STAZAL = 90.0
         END IF
      END IF
C
C     Decode horizon information, including counting good points.
C
      STANHO = 0
      I1 = KEYPTR( 'HOR_AZ', KC, KI ) - 1
      I2 = KEYPTR( 'HOR_EL', KC, KI ) - 1
      DO I = 1, 200
         IF( KD(I1+I) .NE. UNSET ) THEN
            STANHO = I
            STAHAZ(I) = KD(I1+I)
            STAHEL(I) = KD(I2+I)
         ELSE
            GO TO 900
         END IF
      END DO
  900 CONTINUE
C
  990 RETURN
      END


