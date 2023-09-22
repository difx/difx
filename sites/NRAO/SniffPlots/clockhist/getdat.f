      SUBROUTINE GETDAT
C
C     Routine for reading the data for CLOCKDIST.
C     It first gets all data in ACCFILE
C     Then it reads the FILELIST and goes and gets data from any
C     files not in ACCFILE.
C     It adds that data to ACCFILE
C
      INCLUDE 'clock.inc'
C
      INTEGER    JB, VLBOPE, IER, IEXP, NFILES, WHICH, LEN1, L1, L2
      CHARACTER  INFILE*132, CBUFF*256, INLINE*256
      DOUBLE PRECISION  GETNUM
C -------------------------------------------------------------------
C
C     Get the file names for the FILELIST and the ACCFILE from the user.
C
      WRITE(*,*) ' File that lists the desired datafile.lis files:'
      READ(*,'(A)') FILELIST
      WRITE(*,*) ' File that constains previously accumulated data:'
      READ(*,'(A)') ACCFILE
C
C     First open the accum file and get the data from it.  Open it
C     with unknown status so that it can be read and written.
C
      WRITE(*,*) ' Opening accumulated data file: ', 
     1      ACCFILE(1:LEN1(ACCFILE))
      OPEN( UNIT=10, FILE=ACCFILE, STATUS='UNKNOWN', FORM='FORMATTED',
     1      IOSTAT=IER, ACCESS='SEQUENTIAL' )
      IF( IER .NE. 0 ) THEN
         WRITE(*,*) ' Can''t open Accum file'
         STOP
      END IF
C
C     Now get the data out of the file.
C
      NEXP = 0
  100 CONTINUE
C      
C        Read the next line of data.
C
         NWORDS = MWORDS
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 10, 6 )
         IF( NWORDS .EQ. -1 ) GO TO 200
C
C        Look for a file name.
C
         IF( WORD(1) .EQ. 'File:' ) THEN
            NEXP = NEXP + 1
            DATAFILE(NEXP) = WORD(2)
            NBAS(NEXP) = 0
         ELSE IF( NWORDS .EQ. 7 ) THEN
            NBAS(NEXP) = NBAS(NEXP) + 1
            JB = NBAS(NEXP)
            SOURCE(JB,NEXP) = WORD(1)(1:WLEN(1))
            ISTA1(JB,NEXP) = WHICH( WORD(2)(1:WLEN(2)), NAMES, MAXSTA )
            ISTA2(JB,NEXP) = WHICH( WORD(3)(1:WLEN(2)), NAMES, MAXSTA )
            TIME(JB,NEXP)   = GETNUM( WORD(4), 1, WLEN(4) )
            MAXAMP(JB,NEXP) = GETNUM( WORD(5), 1, WLEN(5) )
            DELAY(JB,NEXP)  = GETNUM( WORD(6), 1, WLEN(6) )
            RATE(JB,NEXP)   = GETNUM( WORD(7), 1, WLEN(7) )
         ELSE
            WRITE(*,*) ' There is a bad format line in ', ACCFILE
            WRITE(*,*) INLINE(1:LEN1(INLINE))
         END IF
C
         GO TO 100
C
C     Out of data in the ACCFILE
C
  200 CONTINUE
      NACC = NEXP
C
C     Now get any new data not in the ACCFILE.
C
C     First open the FILELIST
C
      WRITE(*,*) ' Getting file list from: ', 
     1      FILELIST(1:LEN1(FILELIST))
      IER = VLBOPE( 8, FILELIST, 'TEXT', 'OLD', CBUFF )
      IF( IER .NE. 1 ) THEN
         WRITE(*,*) ' Can''t open datafiles file: ', FILELIST
         STOP
      END IF
C
      NFILES = 0
  300 CONTINUE
C
C        Read the next file name.  The /nfs can cause trouble - I
C        think it keeps the automounter from working.
C
         READ( 8, '(A)', END=400 ) INFILE
         IF( INFILE(1:4) .EQ. '/nfs' ) INFILE = INFILE(5:LEN1(INFILE))
C
C        Now check if we alread have this file.
C
         DO IEXP = 1, NEXP
            IF( INFILE .EQ. DATAFILE(IEXP) ) GO TO 300
         END DO
C
C        Have a new one.  Open it.
C
         NEXP = NEXP + 1
         IF( NEXP .GT. MAXEXP ) THEN
            WRITE(*,*) ' Too many experiments '
            NEXP = NEXP - 1
            GO TO 400
         END IF
         DATAFILE(NEXP) = INFILE
C
         WRITE(*,*) NEXP, ' ', DATAFILE(NEXP)(1:LEN1(DATAFILE(NEXP)))
         IER = VLBOPE( 9, DATAFILE(NEXP), 'TEXT', 'OLD', CBUFF )
         IF( IER .NE. 1 ) THEN 
            WRITE(*,*) ' Could not open file: ', DATAFILE(NEXP)
            NEXP = NEXP - 1
            GO TO 300
         END IF
C
C        Get the data from this file.
C
         CALL GETCLK
C
C        Close this file and return for the next.
C
         CLOSE( UNIT = 9 )
         GO TO 300
  400 CONTINUE
C
C     Now add the new data to the ACCFILE.
C
      IF( NEXP .GT. NACC ) THEN
         DO IEXP = NACC + 1, NEXP
            WRITE( 10, '( A, A )' ) 'File: ', 
     1          DATAFILE(IEXP)(1:LEN1(DATAFILE(IEXP)))
            IF( NBAS(IEXP) .GT. 0 ) THEN
               DO JB = 1, NBAS(IEXP)
                  IF( MAXAMP(JB,IEXP) .GT. 0.0 ) THEN
                     L1 = LEN1( NAMES(ISTA1(JB,IEXP)) )
                     L2 = LEN1( NAMES(ISTA2(JB,IEXP)) )
                     WRITE( 10, '(A, T13, A, T16, A, T20, F12.5, 
     1                  F10.5, F9.1, F9.1)' )
     2                  SOURCE(JB,IEXP)(1:LEN1(SOURCE(JB,IEXP))),
     3                  NAMES(ISTA1(JB,IEXP))(1:L1),
     4                  NAMES(ISTA2(JB,IEXP))(1:L2),
     5                  TIME(JB,IEXP), MAXAMP(JB,IEXP),
     6                  DELAY(JB,IEXP), RATE(JB,IEXP)
                  END IF
               END DO
            END IF
         END DO
         WRITE(*,*) ' Done adding data to accum file. ', NEXP
      ELSE
         WRITE(*,*) ' All requested data was in accum file. ', NEXP
      END IF
C
      CLOSE( UNIT=10 )
C
      RETURN
      END
