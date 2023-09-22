      SUBROUTINE RDLOC( JERR )
C
C     Routine used by RDSTN to read a separate station locations file if
C     one is to be used.  The locations file only contains station 
C     position information (in X, Y, Z), rates, and some history.  In 
C     the SCHED implementation, it is derived from the VLBA correlator 
C     data base and is managed separately from the stations file.  The 
C     stations file can contain all position information too and any 
C     position information there will override what is in the locations
C     file.  If the open fails, control will be returned to the calling
C     routine and IER is set to 1.  The calling routine can decide
C     whether or not this was bad.  This routine is only expected
C     to be called once.  It will not attempt to append to the
C     results from a previous read.  The only parameters actually
C     saved are STNAME and the X, Y, Z coordinates.

C
C     Most of the input variables are the duplicates of ones in 
C     the stations catalog.
C
C     The file name is kept in the include file.
C
C     Note that unit 24 is used so the calling routine should avoid that
C     unit number.
C
      INCLUDE    'rdcat.inc'
C
      INTEGER    IUNIT, JERR, IER
      PARAMETER  (IUNIT=24)
C
C     Catalog input parameters.
C
      INTEGER           MLO, I, IPTR, MODE
      PARAMETER         (MLO = 25)
      INTEGER           KI(MLO), KEYPTR, VLBOPE
      CHARACTER         KC(MLO)*8, KCHAR*256, TEXT*80, RESULT*256
      DOUBLE PRECISION  KD(MLO*2), ENDMRK, BLANK
      LOGICAL           EXISTS
C for commented out statement      INTEGER           LEN1
C
      DATA   (KI(I),I=1,3)     / MLO, 0, 3 /
C ----------------------------------------------------------------------
C     First attempt to open the locations catalog.
C
      JERR = 0
      INQUIRE( FILE=LOCFILE, EXIST=EXISTS )
      IF( .NOT. EXISTS ) THEN
         JERR = 1
      ELSE
         IER = VLBOPE( IUNIT, LOCFILE, 'TEXT', 'OLD', RESULT )
         IF( IER .EQ. 0 ) THEN
            JERR = 1
         END IF
      END IF
      IF( JERR .EQ. 0 ) THEN
C         Leave this announcement for the calling programs.
C         Otherwise, I can't get it to the SCHED logfile.
C         CALL PUTOUT( 'RDLOC:   Reading locations file '//
C     1        LOCFILE(1:LEN1(LOCFILE)) )
         JERR = 0
         CALL KPACK( '/       ', ENDMRK )
         CALL KPACK( '        ', BLANK )
C
C        Set up the variables.
C
         CALL KEYCHR( 'VERSION', 'Not known', 20, KD, KC, KI )
         CALL KEYCHR( 'DBNAME', ' ', 10, KD, KC, KI )
         CALL KEYCHR( 'DBCODE', ' ', 4, KD, KC, KI )
         CALL KEYCHR( 'FRAME', ' ', 80, KD, KC, KI )
         CALL KEYADD( 'AXISTYPE', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'AXISOFF', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'X', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'Y', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'Z', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DXDT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DYDT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DZDT', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'EPOCH', 0.D0, 1, KD, KC, KI )
C
C        The reading loop.
C 
         NDB = 0
100      CONTINUE
C
C           Reset the defaults.
C
            IPTR = KEYPTR( 'DBNAME', KC, KI )
            KD(IPTR) = BLANK
            KD(IPTR+1) = BLANK
            KD( KEYPTR( 'DBCODE', KC, KI ) ) = BLANK
            IPTR = KEYPTR( 'FRAME', KC, KI ) - 1
            DO I = 1, 10
               KD(IPTR+I) = BLANK
            END DO
            KD( KEYPTR( 'X', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'Y', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'Z', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'DXDT', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'DYDT', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'DZDT', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'EPOCH', KC, KI ) ) = 0.0D0
            KD( KEYPTR( 'AXISTYPE', KC, KI ) ) = BLANK
            KD( KEYPTR( 'AXISOFF', KC, KI ) ) = 0.0D0
C
C           Call KEYIN to get the data record.
C
            MODE = 0
            CALL KEYIN( KD(MLO+1), KD, KI(2), ENDMRK, MODE, IUNIT, 6 )
            IF( MODE .EQ. 1 ) THEN
               GO TO 990
            ELSE
               NDB = NDB + 1
               IF( NDB .GT. MDB ) THEN
                  WRITE( TEXT, '( A, I5 )' ) 
     1              'RDLOC: Too many stations in location file. Max:',
     2              MDB
                  CALL ERROR( TEXT )
               END IF
C
C              Extract the data.
C
               DBVER = KCHAR( 'VERSION', 20, .FALSE., KD, KC, KI )
               DBNAME(NDB) = KCHAR( 'DBNAME', 10, .FALSE., KD, KC, KI )
               DBCODE(NDB) = KCHAR( 'DBCODE', 4, .FALSE., KD, KC, KI )
               DBFRM(NDB) = KCHAR( 'FRAME', 80, .FALSE., KD, KC, KI )
               DBX(NDB)   = KD( KEYPTR( 'X', KC, KI ) )    
               DBY(NDB)   = KD( KEYPTR( 'Y', KC, KI ) )    
               DBZ(NDB)   = KD( KEYPTR( 'Z', KC, KI ) )    
               DBDX(NDB)  = KD( KEYPTR( 'DXDT', KC, KI ) ) 
               DBDY(NDB)  = KD( KEYPTR( 'DYDT', KC, KI ) ) 
               DBDZ(NDB)  = KD( KEYPTR( 'DZDT', KC, KI ) ) 
               DBEPO(NDB) = KD( KEYPTR( 'EPOCH', KC, KI ) )
               DBDBA(NDB) = KCHAR( 'AXISTYPE', 5, .TRUE., KD, KC, KI )
               DBOFF(NDB) = KD( KEYPTR( 'AXISOFF', KC, KI ) )
C
C              Adjust for various varients on the axis type spec,
C              especially for altaz mounts.
C              Note also that some mount types in locations.dat of
C              mid 2010 are simply wrong - like Hobart.
C              As far as I know, DBDBA is not used anywhere.  The
C              axis type is also in stations.dat and that is the one
C              that is used.
C
               IF( DBDBA(NDB) .EQ. 'ALTZ' .OR. 
     1             DBDBA(NDB) .EQ. 'ALTA' ) THEN
                  DBDBA(NDB) = 'ALTAZ'
               END IF
C
C              Go back for next record.
C
               GO TO 100
            END IF
990      CONTINUE
C
C        Close the file.
C
         CLOSE( UNIT=IUNIT )
C
      END IF
C
      RETURN
      END
