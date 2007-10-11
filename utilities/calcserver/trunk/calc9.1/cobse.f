      SUBROUTINE OBSNT ( KOUNT, KEND)
      IMPLICIT None
C
C 1.    OBSNT
C
C 1.1   OBSNT PROGRAM SPECIFICATION
C
C 1.1.1 OBSNT calls the database handler to fetch an observation. If the 
C       end-of-data is detected, a flag is set and CALC finishes processing the
C       experiment. Before each new observation is processed, OBSNT writes the
C       observation number, time tag, the baseline identification, and the 
C       source name. OBSNT is called once for each observation.
C
C 1.2   OBSNT PROGRAM INTERFACE 
C 
C 1.2.1 CALLING SEQUENCE -
C            INPUT VARIABLES: 
C              1.  KOUNT  -  THE VARIABLE WHICH INITIALIZES THE COUNTER OF THE
C                            OBSERVATION ITEMS TO ZERO IN SUBROUTINE INITL.
C            OUTPUT VARIABLES:
C              1.  KEND   -  THE VARIABLE WHICH FLAGS WHEN AN END OF FILE HAS 
C                            BEEN REACHED. (NOTE: KEND = 0 MEANS PROCESS
C                            ANOTHER OBSERVATION ITEM, KEND = 1 MEANS THAT THE
C                            END OF FILE HAS BEEN REACHED.)
C 
C 1.2.2 COMMON BLOCKS USED -
      INCLUDE 'ccon.i'             
C 
C 1.2.3 PROGRAM SPECIFICATIONS -
      Integer*2 ITAG(5), KERR(5), LNBASE(4,2), LSTRNM(4), NDO(3), NN
      Integer*4 KOUNT, KEND, N 
      Real*8  TAGSEC
C 
C 1.2.4 DATA BASE ACCESS - CALL SUBROUTINE MVREC TO OBTAIN THE NEXT OBSERVATION
C                          FROM THE DATA BASE. CALL SUBROUTINE FINIS TO CLOSE
C                          THE DATA BASE.
C            'GET' VARIABLES: 
C              1.  ITAG(5)     -  AN ARRAY USED TO STORE THE YEAR, MONTH, DAY,
C                                 HOUR, AND MINUTE OF THE OBSERVATION TIME TAG.
C              2.  LNBASE(4,2) -  AN ARRAY USED TO STORE THE EIGHT CHARACTER 
C                                 BASELINE IDENTIFICATIONS OF THE CURRENT 
C                                 OBSERVATION. (ALPHAMERIC) 
C              3.  LSTRNM(4)   -  AN ARRAY USED TO STORE THE EIGHT CHARACTER
C                                 STAR NAME OF THE CURRENT OBSERVATION.  
C                                 (ALPHAMERIC)
C              4.  TAGSEC      -  AN ARRAY USED TO STORE THE SECONDS PORTION OF
C                                 THE OBSERVATION TIME TAG. (SEC) 
C            ACCESS CODES:
C              1.  'UTC TAG ' -  THE DATA BASE ACCESS CODE FOR THE YEAR, MONTH,
C                                DAY, HOUR, AND MINUTE PORTION OF THE UTC 
C                                OBSERVATION TIME TAG ARRAY. (Year is 2-digit
C                                in the Mark III analysis system.)
C              l.5 'UTC TAG4' -  THE DATA BASE ACCESS CODE FOR THE YEAR, MONTH,
C                                DAY, HOUR, AND MINUTE PORTION OF THE UTC 
C                                OBSERVATION TIME TAG ARRAY. (New proposed 
C                                L-code. Year will be 2-digits in the Mark 
C                                III analysis system.)
C              2.  'BASELINE' -  THE DATA BASE ACCESS CODE FOR THE BASELINE
C                                IDENTIFICATION ARRAY.
C              3.  'STAR ID ' -  THE DATA BASE ACCESS CODE FOR THE SOURCE 
C                                IDENTIFICATION ARRAY.
C              4.  'SEC TAG ' -  THE DATA BASE ACCESS CODE FOR THE SECONDS
C                                PORTION OF THE UTC OBSERVATION TIME TAG. 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - 
C            OUTPUT VARIABLES:
C              1. THE OBSERVATION NUMBER, TIME TAG, BASELINE IDENTIFICATION,
C                 AND SOURCE IDENTIFICATION. (NOTE: THESE VARIABLES ARE
C                 WRITTEN OUT ONCE FOR EACH OBSERVATION ITEM PROCESSED.)
C              2. POSSIBLE ERROR OUTPUT
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: MAIN
C             CALLED SUBROUTINES: GETA, GETI, GET4, KILL, MVREC, FINIS
C 
C 1.2.7 PROGRAM VARIABLES -
C           1.  KEND    - THE VARIABLE WHICH FLAGS WHEN AN END OF FILE HAS BEEN
C                         REACHED.  (NOTE: KEND = 0 MEANS PROCESS ANOTHER
C                         OBSERVATION ITEM, KEND = 1 MEANS THAT THE END OF FILE
C                         HAS BEEN REACHED.)
C           2.  KERR(5) - THE DATA BASE ERROR RETURN FLAGS.
C           3.  NDO(3)  -
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/12/77
C                    PETER DENATALE 07/06/77
C                    BRUCE SCHUPLER 05/11/78
C                    BRUCE SCHUPLER 12/05/78
C                    SAVITA GOEL    06/03/87 (CDS FOR A900)
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    D. Gordon 94.05.23 Changed $Include to Include.
C                    D. Gordon 94.06.08 Corrected format statement.
C                    D. Gordon 98.10.13 Changed year field to I4 in output
C                       format for 4-digit years.
C                    D. Gordon 98.11.04 Added GETI of 'UTC TAG4' (4-digit
C                       year). If not there (will not be there in Mark III
C                       system for a while) will get 'UTC TAG ' (2-digit
C                       year) as before.  
C
C   OBSNT Program Structure
C
C   Try to move to the next observation.
      CALL MVREC ( 2, 1, 1, KERR(1) )
C
C   Check the error return to determine course of action.
C   KERR = 0 says observation found. KERR = 1 says end-of-file.
C   First check for database failure.
      IF ( KERR(1) .NE. 0 ) THEN
        IF ( KERR(1) .EQ. 1 )  GO TO 810
          CALL CKILL (6HOBSNT , 1, KERR(1) )
      ENDIF
C
C   GET the time tag, the baseline id, and source name.
C    First try to get new time tag with 4-digit year. If not there, get old
C     time tag with 2-digit year. Doesn't really matter though. 
      CALL GETI ('UTC TAG4      ', ITAG,   5, 1, 1, NDO, KERR(2) )
       If (KERR(2) .ne. 0) 
     *  CALL GETI ('UTC TAG       ', ITAG,   5, 1, 1, NDO, KERR(2) )
      CALL GET4 ('SEC TAG       ', TAGSEC, 1, 1, 1, NDO, KERR(3) )
      CALL GETA ('BASELINE      ', LNBASE, 4, 2, 1, NDO, KERR(4) )
      CALL GETA ('STAR ID       ', LSTRNM, 4, 1, 1, NDO, KERR(5) )
C
C   Check for database errors and KILL if found.
      DO  N = 1,5
        NN = N
        IF ( KERR(N) .NE. 0 )  CALL CKILL (6HOBSNT ,NN,KERR(NN))
      ENDDO
C
C   Kick the counter for another observation and set end-of-data flag to zero.
      KEND = 0
      KOUNT = KOUNT + 1
C
C   Write the observation line to the screen.
      IF (ILUOUT .NE. -1)
     .WRITE ( 6, 9000 )  KOUNT, ITAG, TAGSEC, LNBASE, LSTRNM
 9000 FORMAT (1X,I6,1X,I4,'/',I2,'/',I2,1X,I2,':',I2,F6.2,
     1 1X,4A2,'-',4A2, 1X, 4A2 )
      GO TO 820
C
C  End-of-file encountered, so set KEND and close the database.
  810 KEND = 1
      CALL FINIS(0)
C
  820 RETURN
      END
