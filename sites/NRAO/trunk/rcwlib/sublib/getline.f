      SUBROUTINE GETLINE( WORD, WLEN, NWORDS, INLINE, FIN, TOUT )
C
C     Routine to read a line and break it into words.  Merged versions
C     from FITMON and TSYSMON.  
C     Modified from RDLINE
C     Input:
C        NWORDS  I              Size of the WORD and WLEN arrays.
C        FIN     I              Unit number to read from.
C        TOUT    I              Unit number for error messages.
C     Output:
C        WORD    C*(*)  Array   Array of words from next line.
C        WLEN    I      Array   Array of lengths of words from next line.
C        NWORDS  I              Number of words found.
C        INLINE  C*(*)          The input line.
C     
C     1991Apr12  No longer treats :- as word boundary since FITMON
C                does not want it and TSYSMON is no longer in use.
C
C     1992Mar13  This version is for FITMON.  A different version
C                is required for SKEDCONV (in PARSE.FOR on PC).
C                The other has a longer word length and does not have
C                the Apr 12. mod above.  Merge them some day.
C
C     1992Aug14  For AZEL, don't complain about long words if they
C                are strings such as '---------', '+++++++++++' etc.
C                These are cosmetic delimiters.
C
C     1993Feb2   Try to generalize the size of WORD character strings.
C                
C     1995Jan7   INLINE lengthened from 256 to 384
C
C     1995Feb15  Ignore *************
C
C     1996Nov19  Make GETLINE from RDLINE.  Adding INLINE in call.
C
      INTEGER        MAXCH
      CHARACTER      INLINE*(*)
      CHARACTER      WORD(*)*(*)
      INTEGER        WLEN(*), NWORDS, MWORDS, CHAR1, CHARN
      INTEGER        START, I, FIN, TOUT, NERR, NCALL, LEN1
C            INTEGER  ITWO, IW
      DATA           NCALL / 0 /
      SAVE           NCALL
C ----------------------------------------------------------------------
      NCALL = NCALL + 1
      MAXCH = LEN( WORD(1) )
      MWORDS = NWORDS
C
      INLINE = ' '
      DO 100 I = 1, MWORDS
         WORD(I) = ' '
         WLEN(I) = 0
  100 CONTINUE
C
  105 CONTINUE
      READ( FIN, '(A)', END=200, ERR=110 ) INLINE
      GO TO 115
C
C     Deal with a line that causes error in read.
C
  110 CONTINUE
      NERR = NERR + 1
      IF( NERR .LT. 15 ) THEN
         WRITE( TOUT, '(A)' ) ' Problem reading record, will try again'
         GO TO 105
      ELSE
         WRITE( TOUT, '(A)' ) ' Cannot read more of file, STOP '
         GO TO 200
      END IF
C
C     Break line into words.  Treat the combination :- as a word
C     boundary.  This is mainly for TSYSMON
C
  115 CONTINUE
      START = 1
      NWORDS = 0
  130 CONTINUE
         CALL NXTWRD( INLINE, START, CHAR1, CHARN )
         IF( CHAR1 .EQ. 0 ) GO TO 140
         NWORDS = NWORDS + 1
C
C        Protect against words that are too long.
C
         IF( CHARN - CHAR1 .GT. MAXCH - 1 ) THEN
            CHARN = CHAR1 + MAXCH - 1
            IF(  INLINE(CHAR1:CHAR1+9) .NE. '----------' .AND. 
     1           INLINE(CHAR1:CHAR1+9) .NE. '==========' .AND. 
     2           INLINE(CHAR1:CHAR1+9) .NE. '**********' .AND. 
     3           INLINE(CHAR1:CHAR1+9) .NE. '++++++++++' ) THEN 
              WRITE(*,*) ' GETLINE: A word exceeds the maximum '//
     1                 'length.  Truncated.'
              WRITE(*,*) '          Happened on call ', NCALL, 
     2                 ' to GETLINE.  Input line:'
              WRITE(*,*) INLINE(1:LEN1(INLINE))
            END IF
         END IF
C
         IF( NWORDS .GT. MWORDS ) THEN
            WRITE(*,*) ' GETLINE: Too many words found in line.  '//
     1                 'Some ignored. '
            NWORDS = NWORDS - 1
            GO TO 140
         ELSE
C                          ITWO = INDEX( INLINE(CHAR1:CHARN), ':-' )
C                          IF( ITWO.EQ. 0 ) THEN
            WORD(NWORDS) = INLINE(CHAR1:CHARN)
            WLEN(NWORDS) = CHARN - CHAR1 + 1
C                          ELSE
C                             WORD(NWORDS) = INLINE(CHAR1:CHAR1+ITWO-1)
C                             WLEN(NWORDS) = ITWO
C                             NWORDS = NWORDS + 1
C                             WORD(NWORDS) = INLINE(CHAR1+ITWO:CHARN)
C                             WLEN(NWORDS) = CHARN - CHAR1 + 1 - ITWO
C                          END IF
            START = CHARN + 1
         END IF
      GO TO 130
C
  140 CONTINUE
      GO TO 999
C
  200 NWORDS = -1
C
  999 CONTINUE
      RETURN
      END



