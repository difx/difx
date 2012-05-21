      INTEGER FUNCTION RDSRC( IUNIT, OPENIT, FILE )
C
C    RDSRC is a routine to read an entry of the source catalog.
C    The catalog is assumed to be in a KEYIN readable format.
C
C    Input parameters:
      INTEGER        IUNIT    ! Unit number read from.
      LOGICAL        OPENIT   ! Open file on first call.  See below.
      CHARACTER*(*)  FILE     ! File name of catalog.
C
C    The value of RDSRC on return is:
C         0  if a line was successfully read.
C         1  if an end-of-file or an "ENDCAT" is encountered.  All 
C            catalogentries should terminate with a '/' so if RDSRC=1,
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
C    The number of variables should be kept small for efficient reading
C    of large catalogs.
C
C    R. C. Walker, Dec. 19, 1988.
C    12 character source names, Nov. 10, 1994
C
C
C    Include file.
C
      INCLUDE 'rdcat.inc'
C
C     Constants:
C
      REAL*8         PI, RADDS, RADHS, UNSET
      PARAMETER      (PI=3.141592653589793238D0)
      PARAMETER      (RADDS=PI/180.D0/3600.D0)
      PARAMETER      (RADHS=PI/12.D0/3600.D0)
      PARAMETER      (UNSET=-9999.D0)
C
C     Internal variables.
C
      INTEGER        IER, VLBOPE, I, I1
      REAL           CONT
      REAL*8         BLANK1, BLANK8, NONAME
      LOGICAL        FOPEN
      CHARACTER*50   CNAME
      CHARACTER*255  RESULT
      SAVE           BLANK1, BLANK8, NONAME, FOPEN, CONT
C
C     Catalog input parameters.  Catalog is read with KEYIN.
C     The items starting with PT are pointers.  They are used rather
C     than calls to KEYPTR every time for efficiency (source catalogs
C     are big and take a while to read).
C
      INTEGER          MSRP, MODE, KEYPTR
      PARAMETER        (MSRP=130)
      INTEGER          KI(MSRP)
      CHARACTER        KC(MSRP)*8, KCHAR*256, LASTFILE*80
      DOUBLE PRECISION KD(MSRP*2), ENDMRK
      LOGICAL          GOTKEYS
      SAVE             KI, KC, KD, ENDMRK, GOTKEYS, LASTFILE
C
C     Pointers (sort of - array indices anyway).
C
      INTEGER          PTSO, PTRA, PTDEC, PTRAE, PTDECE
      INTEGER          PTC, PTV, PTVR, PTVD
      INTEGER          PTEN, PTPM, PTDRA, PTDDEC, PTPRA, PTPDEC 
      INTEGER          PTPAR, PTPARL, PTRE, PTVER, PTFLX, PTFREF
      SAVE             PTSO, PTRA, PTDEC, PTRAE, PTDECE
      SAVE             PTC, PTV, PTVR, PTVD
      SAVE             PTEN, PTPM, PTDRA, PTDDEC, PTPRA, PTPDEC
      SAVE             PTPAR, PTPARL, PTRE, PTVER, PTFLX, PTFREF
C
      DATA   FOPEN         / .FALSE. /
      DATA   GOTKEYS       / .FALSE. /
      DATA   LASTFILE      / ' ' /
      DATA   (KI(I),I=1,3) / MSRP, 0, 3 /
C ----------------------------------------------------------------------
C     Set up the key words if needed.
C
      IF( .NOT. GOTKEYS ) THEN
         CALL KPACK( '/       ', ENDMRK )
         CALL KPACK( 'NONAME  ', NONAME )
         CALL KPACK( '        ', BLANK8 )
         CALL KPACK( ' ', BLANK1 )
         CALL KPACK( 'CONT', CONT )
         DO I = 1, 10
            CALL KEYCHR( 'SOURCE', ' ', 16, KD, KC, KI )
         END DO
         CALL KEYADD( 'RA', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DEc', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'RAERR', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DECERR', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'EPoch', 0.D0, 1, KD, KC, KI )
         CALL KEYCHR( 'EQuinox', ' ', 8, KD, KC, KI )
         CALL KEYCHR( 'Calcode', ' ', 1, KD, KC, KI )
         CALL KEYADD( 'VElocity', 0.D0, MVEL, KD, KC, KI )
         CALL KEYCHR( 'VDef', 'R', 1, KD, KC, KI )
         CALL KEYCHR( 'VRef', 'L', 1, KD, KC, KI )
         CALL KEYADD( 'ENdcat', UNSET, 1, KD, KC, KI )
         CALL KEYADD( 'PMEPOCH', 0.D0, 4, KD, KC, KI )
         CALL KEYADD( 'DRA', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'DDEC', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'PMRA', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'PMDEC', 0.D0, 1, KD, KC, KI )
C
C              Oops, had a spelling problem and don't want to 
C              kill old files while correcting it.
C
         CALL KEYADD( 'PARALAX', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'PARALLAX', 0.D0, 1, KD, KC, KI )
         CALL KEYCHR( 'REmarks', ' ', 80, KD, KC, KI )
         CALL KEYCHR( 'VERSION', 'Not given', 20, KD, KC, KI )
         CALL KEYADD( 'FLUX', 0.D0, 30, KD, KC, KI )
         CALL KEYCHR( 'FLUXREF', ' ', 15, KD, KC, KI )
         GOTKEYS = .TRUE.
C
C        Get the pointers.
C
         PTSO   = KEYPTR( 'SOURCE', KC, KI )
         PTRA   = KEYPTR( 'RA', KC, KI )
         PTDEC  = KEYPTR( 'DEc', KC, KI )
         PTRAE  = KEYPTR( 'RAERR', KC, KI )
         PTDECE = KEYPTR( 'DECERR', KC, KI )
         PTC    = KEYPTR( 'Calcode', KC, KI )
         PTV    = KEYPTR( 'VElocity', KC, KI )
         PTVR   = KEYPTR( 'VRef', KC, KI )
         PTVD   = KEYPTR( 'VDef', KC, KI )
         PTEN   = KEYPTR( 'ENdcat', KC, KI )
         PTPM   = KEYPTR( 'PMEPOCH', KC, KI )
         PTDRA  = KEYPTR( 'DRA', KC, KI )
         PTDDEC = KEYPTR( 'DDEC', KC, KI )
         PTPRA  = KEYPTR( 'PMRA', KC, KI )
         PTPDEC = KEYPTR( 'PMDEC', KC, KI )
         PTPAR  = KEYPTR( 'PARALAX', KC, KI )
         PTPARL = KEYPTR( 'PARALLAX', KC, KI )
         PTRE   = KEYPTR( 'REmarks', KC, KI )
         PTVER  = KEYPTR( 'VERSION', KC, KI )
         PTFLX  = KEYPTR( 'FLUX', KC, KI )
         PTFREF = KEYPTR( 'FLUXREF', KC, KI )
      END IF
C
C     Open the file if requested.
C
      IF( OPENIT .AND. .NOT. FOPEN ) THEN
         IER = VLBOPE( IUNIT, FILE, 'TEXT', 'OLD', RESULT )
         IF( IER .EQ. 0 ) THEN
            CALL PUTOUT( RESULT )
            CNAME = FILE
            CALL ERROR( 'Source catalog: '//CNAME//' not opened' )
         END IF
         FOPEN = .TRUE. 
C         
      END IF
C
C     Set parameters defaults before reading new line.
C
      MODE = 0
C
C     Source.  There are two cells in KD for each source.
C
      KD(PTSO) = NONAME
      DO I = 2, 2*MAL
        KD(PTSO-1+I) = BLANK8
      END DO
C
      KD(PTRA)   = 0.D0
      KD(PTDEC)  = -324000.D0      
      KD(PTRAE)  = 0.D0
      KD(PTDECE) = 0.D0
      KD(PTC)    = BLANK1
C
      DO I = 1, MVEL
         KD(PTV-1+I) = UNSET
      END DO
      KD(PTEN) = UNSET
C
      DO I = 1, 4
         KD(PTPM-1+I) = 0.D0
      END DO
      KD(PTDRA)  = 0.D0
      KD(PTDDEC) = 0.D0
      KD(PTPRA)  = 0.D0
      KD(PTPDEC) = 0.D0
      KD(PTPAR)  = 0.D0
      KD(PTPARL) = 0.D0
C
      DO I = 1, 10
        KD(PTRE-1+I) = BLANK8
      END DO
C
C     EPOCH and EQUINOX default to the previous value so they need 
C     not be given for every source, although that is safer in 
C     case a source of different equinox is inserted into the catalog.
C     But reset EPOCH and EQUINOX if starting a new file.
C     Also reset VERSION.
C
      IF( FILE .NE. LASTFILE ) THEN    
         KD( KEYPTR( 'EPoch', KC, KI ) ) = 0.D0
         KD( KEYPTR( 'EQuinox', KC, KI ) ) = BLANK8
         LASTFILE = FILE
         CALL KPACK( 'Not give', KD(PTVER) )
         CALL KPACK( 'n       ', KD(PTVER+1) )
         CALL KPACK( '        ', KD(PTVER+2) )
      END IF
C
      DO I = 1, 30
         KD(PTFLX-1+I) = 0.D0
      END DO
      DO I = 1, 2
         KD(PTFREF-1+I) = BLANK8
      END DO      
C
C
C
C     Read the catalog line with KEYIN.  
C
C
C
      CALL KEYIN( KD(MSRP+1), KD, KI(2), ENDMRK, MODE, IUNIT, 6 )
C
C
C
      IF( MODE .EQ. 1 .OR. KD(PTEN) .EQ. 0.D0 ) THEN
         IF( OPENIT .AND. FOPEN ) CLOSE( UNIT=IUNIT )
         FOPEN = .FALSE.
         RDSRC = 1
         GO TO 990
      END IF
C
C     Get the catalog version.
C
      SRCVER = KCHAR( 'VERSION', 20, .FALSE., KD, KC, KI )
C
C     Get the source names.
C
      RDSRC = 0
      DO I = 1, MAL
         I1 = PTSO + 2 * ( I - 1 )
         WRITE( SRCNAM(I), '(2A8)' ) KD(I1), KD(I1+1)
      END DO
C
C     Get the source velocities
C
      DO I = 1, MVEL
         SRCVEL(I) = KD(PTV-1+I)
         IF( KD(PTV-1+I) .EQ. UNSET ) SRCVEL(I) = CONT
      END DO
      WRITE( SRCVREF, '(A1)' ) KD(PTVR)
      CALL UPCASE( SRCVREF )
      WRITE( SRCVDEF, '(A1)' ) KD(PTVD)
      CALL UPCASE( SRCVDEF )
C
C     Get the proper and planetary motion information.
C     Assume that the proper motion and paralax come in as mas
C     but SCHED wants arcseconds.
C 
      SRCPMY   = KD(PTPM)
      SRCPMM   = KD(PTPM+1)
      SRCPMD   = KD(PTPM+2)
      SRCPMT   = KD(PTPM+3) * RADHS
      SRCDRA   = KD(PTDRA)
      SRCDDEC  = KD(PTDDEC)
      SRCPMRA  = KD(PTPRA) / 1000.D0
      SRCPMDEC = KD(PTPDEC)/ 1000.D0
C         Deal with that spelling problem.  Give the correct spelling
C         priority, but take the other if that is used.
      SRCPARA  = KD(PTPARL) / 1000.D0 
      IF( KD(PTPARL) .EQ. 0.D0 .AND. KD(PTPAR) .NE. 0.D0 ) THEN
         SRCPARA = KD(PTPAR) / 1000.D0
      END IF
C
C     Comment.
C
      SRCRMK   = KCHAR( 'REmarks', 80, .FALSE., KD, KC, KI )
C
C     Get the source position.  Errors are assumed to be in mas.
C
      SRCRA   = KD(PTRA) * RADHS
      SRCDEC  = KD(PTDEC) * RADDS
      SRCRAE  = KD(PTRAE)
      SRCDECE = KD(PTDECE)
C
C     Get the epoch and equinox.  Just return these to the 
C     calling routines.  I could fix things here if EPOCH 
C     is used instead of equinox, but the important case
C     is SCHED and I want access to it's logging utilities for
C     the warnings etc.
C
      SRCEQ = KCHAR( 'EQuinox', 8, .TRUE., KD, KC, KI )
      SRCEPOT = KD( KEYPTR( 'EPoch', KC, KI ) )
C
C     Cal code.
C
      WRITE( SRCCAL, '(A1)' ) KD(PTC)
      CALL UPCASE( SRCCAL )
C
C     Fluxes
C
      DO I = 1, 30
         SRCFLUX(I) = KD(PTFLX-1+I)
      END DO
      SRCFREF = KCHAR( 'FLUXREF', 15, .FALSE., KD, KC, KI )
C
  990 RETURN
      END
