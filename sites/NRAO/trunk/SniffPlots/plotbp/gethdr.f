      SUBROUTINE GETHDR( IER )
C     
C     Get scan info from bandpass files.
C     IER = 0 => ok
C     IER = 1 => Out of data.
C
      INTEGER  IER, IIF
      DOUBLE PRECISION GETNUM
      LOGICAL  WARNMAX
      INCLUDE 'plotbp.inc'
      SAVE     WARNMAX
      DATA     WARNMAX / .TRUE. /
C -----------------------------------------------
      IER = 0
C
C     Parse first line - it was just read.
C     Protect against NCHIF*NIF>MCHAN later, after the channel header
C     lines have been read.
C
      CTIME(1) = WORD(3)
      CTIME(2) = WORD(5)
      EXPNAM   = WORD(7)
      JDAY1 = GETNUM( WORD(2), 1, WLEN(2) )
      NCHIF = GETNUM( WORD(9), 1, WLEN(9) )
      NIF   = GETNUM( WORD(11), 1, WLEN(11) )
C
C     Read second line.
C
      NWORDS = MWORDS
      CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
      IF( NWORDS .EQ. -1 ) THEN
         WRITE(*,*) ' No data '
         IER = 1
         GO TO 999
      END IF
      NEWFMT = NWORDS .NE. 10
      IF( .NOT. NEWFMT ) THEN
         SOURCE = WORD(2)
         STOKES(1) = WORD(10)
         FREQ(1) = GETNUM( WORD(4), 1, WLEN(4) ) * 1000.0
         BW = GETNUM( WORD(7), 1, WLEN(7) )
         SBD(1) = ' '
      ELSE
         SOURCE = WORD(2)
         BW = GETNUM( WORD(4), 1, WLEN(4) )
      END IF
C
C     Read the channel lines.
C
      IF( NEWFMT ) THEN
         DO IIF = 1, NIF
            NWORDS = MWORDS
            CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )            
            IF( NWORDS .EQ. -1 ) THEN
               WRITE(*,*) ' GETHDR: Missing channel info. '
               IER = 1
               GO TO 999
            END IF
            FREQ(IIF) = GETNUM( WORD(2), 1, WLEN(2) ) * 1000.0
            STOKES(IIF) = WORD(5)
            SBD(IIF) = WORD(7)
         END DO
      END IF
C
C     Now protect against too-large spectra.
C
      IF( NCHIF * NIF .GT. MCHAN ) THEN
         NIF = MCHAN / NCHIF
         IF( NIF .LE. 0 ) THEN
            NIF = 1
            NCHIF = MCHAN
         END IF
         IF( WARNMAX ) THEN
            WRITE(*,*) ' *****  WARNING  ******'
            WRITE(*,*) '     Spectra with product of channels * IFs '//
     1            'too large found.'
            WRITE(*,*) '     Maximum is ', MCHAN
            WRITE(*,*) '     Will process ', NIF, ' IFs and ', NCHIF, 
     1            ' channels per if.'
            WARNMAX = .FALSE.
         END IF
      END IF
C
  999 CONTINUE
      RETURN
      END


