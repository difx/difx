C*KEYMAP -- build keyword table (KEYIN)
C+
      SUBROUTINE KEYMAP(KEYS, VALUES, N, ENDMRK, MAXK, NKEYS, KEYWD, 
     1                  KEYPT)
      INTEGER N, MAXK, NKEYS
      DOUBLE PRECISION KEYS(N), VALUES(N), ENDMRK
      CHARACTER*8 KEYWD(MAXK)
      INTEGER     KEYPT(MAXK)
C
C Build a data structure for keyword lookup.
C------------------------------------------------------------------------
      INTEGER     I
      DOUBLE PRECISION BLANK8, LAST
      CALL KPACK( '        ', BLANK8 )
C
C Insert the end marker with pointer -1.
C
      WRITE (KEYWD(1), '(A8)') ENDMRK
      KEYPT(1) = -1
C
C Insert the special keywords with negative pointers.
C
      KEYWD(2) = 'SHOW'
      KEYPT(2) = -2
      KEYWD(3) = 'HELP'
      KEYPT(3) = -3
      KEYWD(4) = 'SAVE'
      KEYPT(4) = -4
      NKEYS = 4
C
C Insert the supplied keywords with positive pointers.
C
      I = 0
      LAST = BLANK8
      DO 10 I=1,N
          IF (KEYS(I).NE.BLANK8 .AND. KEYS(I).NE.LAST) THEN
              NKEYS = NKEYS+1
              IF (NKEYS.GT.MAXK) CALL
     1          ERROR('Too many KEYIN parameters')
              WRITE (KEYWD(NKEYS), '(A8)') KEYS(I)
              KEYPT(NKEYS) = I
              LAST = KEYS(I)
          END IF
   10 CONTINUE
C
      END      
