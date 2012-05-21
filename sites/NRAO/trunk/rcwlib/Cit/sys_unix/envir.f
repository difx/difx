      SUBROUTINE ENVIR( INFILE )
C
C     Subroutine for unix systems that looks for an environment variable
C     at the front of file name FILE (ie, it starts with $).  If one
C     is there, it expands it and returns FILE fully expanded.  
C     Otherwise it returns with FILE unmodified.
C
C     This uses the Fortran Library routine GETENV.
C
      CHARACTER    INFILE*(*), FILE*80, ENVNAM*80, ENVVAL*80
      INTEGER      ICHN, NCHF, LEN1, NECHF
C ---------------------------------------------------------------------
C     Avoid problems with systems (Linux) that don't like concatenation
C     with a subroutine argument declared *(*).
C
      FILE = INFILE
C
C     Check for $ as first character.
C
      IF( FILE(1:1) .EQ. '$' ) THEN
C
C        Get full length of the file name.
C
         NCHF = LEN1( FILE )
C
C        Get the length of the environment variable.  Assume it contains
C        no "/"s.  Anything after the "/" is to be added to the expanded
C        environment variable.  If there are no "/"s at all, assume that
C        the environment variable fully specifies the file.
C
         ICHN = INDEX( FILE, '/' ) - 1
         IF( ICHN .EQ. -1 ) ICHN = NCHF
C
C        Strip off the "$".
C
         ENVNAM = FILE(2:ICHN)
C
C        Expand the environment variable.
C
         CALL GETENV( ENVNAM, ENVVAL )
C
C        Reconstruct the file name.  Abort if the environment 
C        variable does not expand.
C
         IF( ENVVAL .NE. ' ' ) THEN
            IF( NCHF .GT. ICHN ) THEN
C
C              File is environment plus extra.  Be careful not to 
C              end up with "//".
C
               NECHF = LEN1(ENVVAL)
               IF( ENVVAL(NECHF:NECHF) .EQ. '/' ) NECHF = NECHF - 1
               INFILE = ENVVAL(1:NECHF)//FILE(ICHN+1:NCHF)
            ELSE
C
C              File is fully specified by the environment variable.
C
               INFILE = ENVVAL
            END IF
         ELSE
C
C           Abort with error.
C
            CALL ERROR( 'ENVIR: Cannot expand environment variable in '
     1           // FILE(1:LEN1(FILE)) )
         END IF
      END IF
C
      RETURN
      END

