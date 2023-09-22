C*KEYIN  -- free-format parameter input by keyword
C+
      SUBROUTINE KEYIN (KEYS, VALUES, N, ENDMRK, MODE, IN, PR)
      INTEGER N, MODE, IN, PR
      DOUBLE PRECISION  KEYS(N),VALUES(N),ENDMRK
C
C Free-format keyed input.
C
C Author:
C  T. J. Pearson, Caltech.
C
C History:
C  Original version: 1978 May 31
C  Rewritten:        1991 Jan  9
C  Get rid of DATA statements putting characters into numeric constants
C      because g77 calls them an error.  Walker  June 96
C  Use CHAR to set TAB.  It looks like the linux compiler expands
C      tabs in the code before parsing it.  Walker Jan 2000.
C
C Arguments:
C  KEYS: array of parameter names
C  VALUES: array to receive values or defaults
C  N: number of parameters (dimension of keys and values)
C  ENDMRK: special keyword to indicate end of input
C  MODE: integer variable:
C        (input) 1 = turn on reflection, 0 = turn off
C                2 = interactive mode (prompts for input,
C                          no reflection, no limit on errors)
C        (output) 1 = eof found, 0 = endmrk found
C  IN: input unit number
C  PR: printer unit number for reflection and messages
C
C This routine stores character strings in DOUBLE PRECISION
C variables, which is not compatible with standard portable Fortran.
C The input array KEYS contains character strings, not numeric values.
C The elements of the array VALUES may contain either character strings
C or numeric values on input or output.
C-----------------------------------------------------------------------
C     -- maximum number of distinct keywords
      INTEGER MAXK
      PARAMETER (MAXK=250)
C     -- characters per REAL*8 word
      INTEGER    CHPWRD
      PARAMETER  (CHPWRD=8)
C     -- default extension for parameter files
      CHARACTER*(*) EXTENS
      PARAMETER  (EXTENS='.par')
C     -- character constants
      CHARACTER BLANK,EQUALS,COMMA,ARROW,BRA,
     1          KET,SQUOTE,DQUOTE,QUEST,TAB,EXCL,ATSIGN,
     2          SLASH
      PARAMETER (BLANK =' ')
      PARAMETER (EQUALS='=')
      PARAMETER (COMMA =',')
      PARAMETER (ARROW ='>')
      PARAMETER (BRA   ='(')
      PARAMETER (KET   =')')
      PARAMETER (SQUOTE='''')
      PARAMETER (DQUOTE='"')
      PARAMETER (QUEST ='?')
C      PARAMETER (TAB   ='	')  Doesn't work in Linux
      PARAMETER (EXCL  ='!')
      PARAMETER (ATSIGN='@')
      PARAMETER (SLASH ='/')
C
C Local variables
      CHARACTER*13  NUMER
      CHARACTER*255 REC
      CHARACTER*1   DELIM(3)
      CHARACTER*8   WORD, KEYWD(MAXK)
      CHARACTER*64  FILENM, PROG
      INTEGER    ERRCNT, P, LWORD, UNIT, I, LPROG, MAXREC,
     2           IER, NEW, NK, NK2, LEN1, LUNSAV, II, NW, I1,
     3           NLETS, NKEYWD, NK0, NBAD
      INTEGER    KEYPT(MAXK)
      LOGICAL    REFLEC,DIALOG,SAVERF,SAVEDL,EQPRES
      DOUBLE PRECISION BLANK8,UNSET, RESULT, EOL
C
C External functions
      INTEGER KCTOI
      DOUBLE  PRECISION KCTOR, KEXPRE
      LOGICAL TSTTTY
C
C Data statements.
      DATA NUMER  /'-+.0123456789'/
C
C Format statements.
  410 FORMAT (1X,'*',1X,A)
  440 FORMAT (' +++ERROR+++ ',A,A)
  450 FORMAT (' Saved: ',A)
  460 FORMAT (' Reading: ',A)
C
C-----------------------------------------------------------------------
      TAB = CHAR( 9 )
      CALL KPACK( '        ', BLANK8 )
      CALL KPACK( '***EOL  ', EOL )
      CALL KPACK( '***UNSET', UNSET )
C
C Find name of executing program:
      CALL PROGNM(PROG)
      LPROG = LEN1(PROG)
C Output unit number (for prompting and error messages):
      P = PR
      IF (P.LT.0 .OR.  P.GT.99) P = 6
C Input unit number (an illegal unit is treated as EOF). This unit
C number is changed when nested input is read.
      UNIT = IN
C Should input be echoed to output?
      REFLEC = MODE.EQ.1
C Is prompting required?
      DIALOG = (MODE.EQ.2) .OR. TSTTTY(IN)
C Error count is initially zero.
      ERRCNT = 0
      MODE = 0
C
C Build an internal data structure for keyword lookup.
C
      CALL KEYMAP (KEYS, VALUES, N, ENDMRK, MAXK, NKEYWD, KEYWD, KEYPT)
C
C Read next record. If the special ENDMRK '***EOL' is requested,
C force an end-of-file after the first read.
C
      GOTO 102
  100 IF (ENDMRK.EQ.EOL) GOTO 350
  102 CALL KRDLIN(UNIT, REC, MAXREC, P, DIALOG, REFLEC, IER)
      IF (IER.NE.0) GOTO 340
      I = 1
C
C Look for next input word: it is either a comment, an
C @-command, or a keyword.
C
  130 CALL KSKIPB(REC,I)
      IF (I.GT.MAXREC) GOTO 100
C
C     Comment: skip rest of line.
C
      IF (REC(I:I) .EQ. EXCL) GOTO 100
C
C     @-command: include file. The file name is
C     in the character variable FILENM, and the unit is 91 (only
C     one level of nesting is allowed). Default file extension 
C     is EXTENS.
C
      IF (REC(I:I) .EQ. ATSIGN) THEN
          IER = 6
C         ! signal file-name error
          FILENM = ' '
          I = I+1
          CALL KSKIPB(REC,I)
          IF (I.GT.MAXREC) THEN
              FILENM = PROG(1:LPROG)//EXTENS
          ELSE
              FILENM = REC(I:MAXREC)
          END IF
          IF (INDEX(FILENM,'.').LE.0)
     +        FILENM = FILENM(1:(LEN1(FILENM)))//EXTENS
          NEW = 91
          OPEN (UNIT=NEW, FILE=FILENM, FORM='FORMATTED',
     1            STATUS='OLD', ERR=330)
          INQUIRE (UNIT=NEW,NAME=FILENM)
          WRITE (P,460) FILENM(1:LEN1(FILENM))
          UNIT = NEW
          SAVEDL = DIALOG
          SAVERF = REFLEC
          REFLEC = REFLEC.OR.DIALOG
          DIALOG = .FALSE.
          GOTO 100
      END IF
C
C     Keyword: read into WORD and look up in list of known keywords.
C
      CALL KWORD(REC, I, WORD, LWORD)
      IER = 17
      IF (LWORD.LT.1) GOTO 330
      CALL KEYFND(WORD, KEYWD, NKEYWD, NK, IER)
      IF (IER.NE.0) GOTO 330
      NK = KEYPT(NK)
C
C     Is it the ENDMRK?
C
      IF (NK.EQ.-1) THEN
            GOTO 350
C
C     Or a SHOW command?
C
      ELSE IF (NK.EQ.-2) THEN
          CALL KSKIPB(REC,I)
          IF (I.GT.MAXREC) THEN
C             -- SHOW by itself shows all parameters
              CALL KEYOUT(KEYS, VALUES, N, P)
          ELSE
C             -- SHOW KEYWORD shows selected parameter
              CALL KWORD(REC, I, WORD, LWORD)
              IER = 17
              IF (LWORD.LT.1) GOTO 330
              CALL KEYFND(WORD, KEYWD, NKEYWD, NK, IER)
              IF (IER.NE.0) GOTO 330
              NK = KEYPT(NK)
              NK2 =1
  147         IF (KEYS(NK+NK2).EQ.KEYS(NK) .OR.
     1            KEYS(NK+NK2).EQ.BLANK8) THEN
                    NK2 = NK2+1
                    GOTO 147
              END IF
              CALL KEYOUT(KEYS(NK),VALUES(NK),NK2,P)
              GOTO 130
          END IF
          GOTO 100
C
C     Or a HELP command?
C
      ELSE IF (NK.EQ.-3) THEN
          CALL KEYHLP(KEYS,VALUES,N,P,PROG)
          GOTO 100
C
C     Or a SAVE command?
C
      ELSE IF (NK.EQ.-4) THEN
          IER = 6
          FILENM = ' '
          I = I+1
          CALL KSKIPB(REC,I)
          IF (I.GT.MAXREC) THEN
              FILENM = PROG(1:LPROG)//EXTENS
          ELSE
              FILENM = REC(I:MAXREC)
          END IF
          IF (INDEX(FILENM,'.').LE.0)
     +      FILENM = FILENM(1:(LEN1(FILENM)))//EXTENS
          LUNSAV = 92
          OPEN (UNIT=LUNSAV, FILE=FILENM, FORM='FORMATTED',
     1          STATUS='UNKNOWN', ERR=330)
          CALL KEYOUT(KEYS,VALUES,N,LUNSAV)
          INQUIRE (UNIT=LUNSAV,NAME=FILENM)
          CLOSE (UNIT=LUNSAV)
          WRITE (P,450) FILENM(1:LEN1(FILENM))
          GOTO 100
      END IF
C
C     Otherwise it is supplied keyword number NK.
C     Look for a (subscript).
C
      EQPRES = .FALSE.
      NK0 = NK
      IF (REC(I:I).NE.BRA) GOTO 190
          I=I+1
          NK=NK-1+KCTOI(REC,I)
          IER=1
C               ! subscript error?
          IF(REC(I:I).NE.KET) GOTO 330
          I=I+1
  180     IF (NK.LT.1) GOTO 330
          IER = 2
C               ! too many values?
          IF (NK.GT.N) GOTO 330
          IF (KEYS(NK).NE.KEYS(NK0)) GOTO 330
C
C Look for value after optional '='. If no value
C and no "=", assign value 0.
C
  190 CALL KSKIPB(REC,I)
      IF (I.GT.MAXREC) THEN
            VALUES(NK) = 0D0
            GOTO 100
      END IF
      IF (REC(I:I).EQ.EQUALS) THEN
            I=I+1
            EQPRES = .TRUE.
            CALL KSKIPB(REC,I)
            IF (I.GT.MAXREC) THEN
                CALL KRDLIN(UNIT, REC, MAXREC, P, DIALOG, REFLEC, IER)
                IF (IER.NE.0) GOTO 340
                I = 1
                CALL KSKIPB(REC,I)
            END IF
      END IF
C
C               If a bracket is found, read an expression.
C
      IF (REC(I:I).EQ.BRA) GOTO 205
C
C               If an explicit quotation mark is found, read
C               a quoted string.
C
      DELIM(1) = REC(I:I)
      DELIM(2) = REC(I:I)
      DELIM(3) = REC(I:I)
      IF (REC(I:I).EQ.SQUOTE .OR. REC(I:I).EQ.DQUOTE) GOTO 210
C
C               If the parameter requires a character value,
C               read an unquoted (blank-terminated) string.
C
      IF (NK.LT.N) THEN
         IF(KEYS(NK+1).EQ.BLANK8) GOTO 202
      END IF
C
C               If '+', '-', '.', or a digit is found, read a
C               numerical value.
C
      DO 199 II=1,LEN(NUMER)
            IF (REC(I:I).EQ.NUMER(II:II)) GOTO 201
  199 CONTINUE
C
C               Otherwise, read a string terminated by blank
C               or comma, but only if an equals sign was found.
C
      IF (.NOT.EQPRES) THEN
            VALUES(NK) = 0D0
            GOTO 260
      END IF
  202 I = I-1
      DELIM(1) = BLANK
      DELIM(2) = COMMA
      DELIM(3) = TAB
      GOTO 210
C
C               Numeric value.
C
  201 RESULT=KCTOR(REC,I)
      IF (REC(I:I).EQ.BLANK .OR. REC(I:I).EQ.COMMA .OR. 
     1    REC(I:I).EQ.EXCL  .OR. REC(I:I).EQ.TAB   .OR.
     2    REC(I:I).EQ.SLASH) THEN
            VALUES(NK) = RESULT
            GOTO 260
      ELSE
            IER = 8
C                               ! bad numeric value
            GOTO 330
      END IF
C
C               Arithmetic expression
C
  205 RESULT = KEXPRE(REC,I,IER)
      IF (IER.NE.0) GOTO 330
      VALUES(NK) = RESULT
      GOTO 260
C
C               Textual value - 'NW' words of 'CHPWRD' characters
C
  210 NW=1
      VALUES(NK)=BLANK8
  220 IF(NK+NW.GT.N) GOTO 230
      IF(KEYS(NK+NW).NE.BLANK8) GOTO 230
            VALUES(NK+NW)=BLANK8
            NW=NW+1
            GOTO 220
  230 I1=I+1
      NLETS=0
  240 I=I+1
      IF(I.GT.MAXREC) GOTO 250
      IF (REC(I:I).EQ.DELIM(1) .OR. REC(I:I).EQ.DELIM(2) .OR.
     1    REC(I:I).EQ.DELIM(3)) GOTO 250
            NLETS=NLETS+1
            GOTO 240
  250 NLETS = MIN(NW*CHPWRD, NLETS)
      CALL KPACK(REC(I1:I1+NLETS-1),VALUES(NK))
      IF (REC(I:I).NE.COMMA) I=I+1
      NK=NK+NW-1
C
C               Look for another value, if allowed
C
  260 CALL KSKIPB(REC,I)
      IF (I.GT.MAXREC) GOTO 100
      IF (REC(I:I).NE.COMMA) GOTO 130
      I = I+1
      CALL KSKIPB(REC,I)
      IF (I.GT.MAXREC) THEN
          CALL KRDLIN(UNIT, REC, MAXREC, P, DIALOG, REFLEC, IER)
          IF (IER.NE.0) GOTO 340
          I = 1
          CALL KSKIPB(REC,I)
      END IF
      NK = NK+1
      GOTO 180
C
C----------------------------------------------------------------------
C               Error report
C----------------------------------------------------------------------
C
  330 IF(.NOT.(REFLEC.OR.DIALOG)) WRITE(P,410) REC(1:MAXREC)
C     WRITE(P,410) BLANK,(ARROW,J=1,I)
      IF(IER.EQ.1)  THEN
          WRITE(P,440) 'in subscript of ',WORD
      ELSE IF (IER.EQ.2)  THEN
          WRITE(P,440) 'Too many values for ',WORD
      ELSE IF (IER.EQ.4)  THEN
          WRITE(P,440) 'Unknown parameter: ',WORD
      ELSE IF (IER.EQ.6)  THEN
          WRITE(P,440) 'Bad file name: ', FILENM(1:LEN1(FILENM))
      ELSE IF (IER.EQ.8)  THEN
          WRITE(P,440) 'Not a valid number'
      ELSE IF (IER.EQ.11) THEN
          WRITE(P,440) 'Too many parentheses'
      ELSE IF (IER.EQ.12) THEN
          WRITE(P,440) 'Expecting ")"'
      ELSE IF (IER.EQ.13) THEN
          WRITE(P,440) 'Value expected after operator'
      ELSE IF (IER.EQ.14) THEN
          WRITE(P,440) 'Attempt to divide by zero'
      ELSE IF (IER.EQ.17) THEN
          WRITE(P,440) 'Syntax error - offending line:' 
          WRITE(P,'( A )' ) REC(1:LEN1(REC))
      ELSE IF (IER.EQ.20) THEN
          WRITE(P,440) 'Unknown parameter: ',WORD
      ELSE IF (IER.EQ.21) THEN
          WRITE(P,440) 'Ambiguous keyword: ',WORD
      ELSE
          WRITE (P,*) 'KEYIN error number',IER
      END IF
      IF(DIALOG) GOTO 102
      ERRCNT=ERRCNT+1
      IF(ERRCNT.LT.10) GOTO 102
C
C----------------------------------------------------------------------
C               EOF or 'ENDMRK' found
C----------------------------------------------------------------------
C If this is the end of a file included with an @-command, revert to
C original file; otherwise check that values have been assigned to all
C compulsory parameters (default value '***UNSET'). If any have not,
C give the user another chance if we found the end-marker, but not if
C we found EOF.
C
  340 CONTINUE
C     -- end of file on primary or secondary input
      IF (UNIT.NE.IN) THEN
C           -- revert to original file
            CLOSE (UNIT=UNIT)
            UNIT = IN
            REFLEC = SAVERF
            DIALOG = SAVEDL
            ERRCNT = 0
            GOTO 100
      ELSE
            MODE = 1
            GOTO 350
      END IF
C
  350 CONTINUE
C     -- end of file on primary input (MODE=1) or ENDMRK found (MODE=0)
C     -- report missing parameters
      NBAD = 0
      DO 390 NK=1,N
          IF (VALUES(NK).EQ.UNSET) THEN
              WRITE (P,440) 'Value needed for ',KEYS(NK)
              NBAD = NBAD+1
          END IF
  390 CONTINUE
C     -- give user another chance if we have reached EOF
      IF (MODE.NE.1 .AND. NBAD.GT.0) GOTO 102
C     -- otherwise quit if there are errors
      IF (NBAD.GT.0) CALL ERROR('in input parameters.')
C     -- otherwise retrun normally
      RETURN
      END
