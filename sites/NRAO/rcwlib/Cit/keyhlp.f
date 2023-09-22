C*KEYHLP -- help routine for KEYIN
C+
      SUBROUTINE KEYHLP(KEYS,VALUES,N,P,PROG)
      INTEGER N, P
      DOUBLE PRECISION KEYS(N),VALUES(N)
      CHARACTER*(*) PROG
C
C HELP routine for KEYIN: lists parameter names.
C 1991 May 17 - TJP.
C Get rid of dp/char DATA statements.  1996Jun26 RCW
C-----------------------------------------------------------------------
      DOUBLE PRECISION BLANK8
      CHARACTER*12 WORD,DUMMYC
      INTEGER I, J, LEN1, LB
      INTEGER VMSHLP
      DATA LB/0/
      CALL KPACK( '        ', BLANK8 )
C
C First try system help library.
C
      IF (VMSHLP(PROG).EQ.1) RETURN
C
C Otherwise, list parameter names.
C
      CALL KEYPUT(PROG(1:LEN1(PROG)),P,LB)
      CALL KEYPUT(' parameters: ',P,LB)
      CALL KEYPUT('***END',P,LB)
      I = 0
    5 IF (I.LT.N) THEN
            I = I+1
            IF (KEYS(I).NE.BLANK8) THEN
                J = 1
   10           IF (I.LT.N .AND. KEYS(I+1).EQ.KEYS(I)) THEN
                    I = I+1
                    J = J+1
                    GO TO 10
                ENDIF
                WRITE (WORD,'(A8)') KEYS(I)
C
C               The following required by a bug in the Ridge 32 compiler
C
                DUMMYC = WORD
                IF (J.LE.1 .AND. WORD.NE.' ') THEN
                  WORD = DUMMYC(1:LEN1(DUMMYC))//','
                ELSEIF (J.LE.1) THEN
                  WORD = ','
                ENDIF
                CALL KEYPUT(' '//WORD(1:LEN1(WORD)),P,LB)
                IF (J.GT.1) THEN
                    WRITE (WORD,'(I3)') J
                    CALL KEYPUT(' ('//WORD(1:3)//' values),',P,LB)
                END IF
            END IF
            GOTO 5
      ENDIF
      CALL KEYPUT('***END',P,LB)
      RETURN
      END
