      SUBROUTINE UTCTM ( UTC, XJD )
      IMPLICIT None
C
C 1.    UTCTM
C
C 1.1   UTCTM PROGRAM SPECIFICATION
C
C 1.1.1 UTCTM is the utility which fetches the UTC time tag of the observation
C       and computes the Julian date at 0 hours UTC of the date of the obs.
C
C 1.1.2 RESTRICTIONS - Computation of Julian date correct for 1901 to 2099 only.
C
C 1.1.3 REFERENCES - ALMANAC FOR COMPUTERS - 1981, Nautical Almanac Office,
C                    United States Naval Observatory, Washington, D.C., 20390
C
C 1.2   UTCTM PROGRAM INTERFACE
C 
C 1.2.1 CALLING SEQUENCE -
C           INPUT VARIABLES: None 
C           OUTPUT VARIABLES: 
C             1. UTC  -  THE UTC TIME AS A FRACTION OF THE UTC DAY. (SEC/SEC) 
C             2. XJD  -  THE JULIAN DATE AT ZERO HOURS UTC OF THE DATE IN
C                        QUESTION. (DAYS)
C 
C 1.2.2 COMMON BLOCKS USED -
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      EXTERNAL CMATHB
C           VARIABLES 'FROM':
C             1. SECDAY  -  THE CONVERSION FACTOR OF COORDINATE TIME SECONDS 
C                           PER COORDINATE TIME DAY. (SEC/DAY)
C           VARIABLES 'TO': NONE 
C 
      INCLUDE 'ccon.i'             
C           VARIABLES 'FROM':
C             1. KUTCC  -  THE UTCTM UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KUTCD  -  THE UTCTM UTILITY ROUTINE FLOW CONTROL FLAG.
C           VARIABLES 'TO': NONE 
C 
C 1.2.3 PROGRAM SPECIFICATIONS -
      Integer*2 ITAG(5), KERR(3), NDO(3), ndum 
      Real*8    TAGSEC, UTC, XJD, JDY2K
      Integer*4 N, NN, IYY, IM, ID
C 
C 1.2.4 DATA BASE ACCESS -
C          'GET' VARIABLES: 
C             1. ITAG(5)  -  THE ARRAY CONTAINING THE YEAR/MONTH/DAY/HOUR/MINUTE
C                            PORTION OF THE OBSERVATION TIME TAG. 
C             2. TAGSEC   -  THE SECONDS PORTION OF THE OBSERVATION TIME TAG.
C          'PUT' VARIABLES: NONE
C          ACCESS CODES:
C             1. 'UTC TAG '  -  THE DATA BASE ACCESS CODE FOR THE 
C                               YEAR/MONTH/DAY/HOUR/MINUTE PORTION OF THE 
C                               OBSERVATION TIME TAG ARRAY. (Year is 2-digit
C                               in the Mark III analysis system.)
C             l.5 'UTC TAG4' -  THE DATA BASE ACCESS CODE FOR THE YEAR, MONTH,
C                               DAY, HOUR, AND MINUTE PORTION OF THE UTC
C                               OBSERVATION TIME TAG ARRAY. (New proposed
C                               L-code. Year will be 2-digits in the Mark
C                               III analysis system.)
C             2. 'SEC TAG '  -  THE DATA BASE ACCESS CODE FOR THE SECONDS 
C                               PORTION OF THE OBSERVATION TIME TAG. 
C 
C 1.2.5 EXTERNAL INPUT/OUTPUT - 
C             1. POSSIBLE DEBUG OUTPUT
C             2. POSSIBLE ERROR OUTPUT
C 
C 1.2.6 SUBROUTINE INTERFACE -
C          CALLER SUBROUTINES: DRIVG 
C          CALLED SUBROUTINES: DFLOTI, GETI, GET4, KILL, JDY2K
C 
C 1.2.7 CONSTANTS USED - SECDAY 
C 
C 1.2.8 PROGRAM VARIABLES - 
C             1. KERR(3)  -  THE DATA BASE ERROR RETURN FLAGS. 
C             2. NDO(3)   -  THE DATA BASE RETURN ARRAY INDICES. 
C             3. IYY, IM, ID - TEMPORARY VARIABLES USED IN COMPUTING XJD.
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/17/77
C                    PETER DENATALE 07/18/77
C                    JIM RYAN 09/14/81
C                         CHANGED TO COMPUTE XJD, RATHER THAN 'GET' IT.
C                    SAVITA GOEL 06/04/87 (CDS FOR A900)
C                    Jim Ryan 89.07.26 Documentation simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                    implimented.
C                    D. Gordon 94.04.13 - Implicit none instituted. DFLOT
C                              changed to Fortran 77 DFLOTI.
C                    D. Gordon 98.07.29 - Convert to use JDY2K to convert year,
C                              month, day to Julian date. Year can be 2-digit
C                              or 4-digit.
C                    D. Gordon 98.11.04 Added GETI of 'UTC TAG4' (4-digit
C                              year). If not there (will not be there in Mark
C                              III system for a while) will get 'UTC TAG ' 
C                              (2-digit year) as before.
C
C     UTCTM Program Structure
C
C     GET the time tag information from the database. Check for db errors.
C    First try to get new time tag with 4-digit year. If not there, get old
C     time tag with 2-digit year. Doesn't really matter though.
      CALL GETI ('UTC TAG4      ', ITAG,   5, 1, 1, NDO, KERR(1) )
       If (KERR(1) .ne. 0)
     *  CALL GETI ('UTC TAG       ', ITAG, 5, 1, 1, NDO, KERR(1) )
      CALL GET4 ('SEC TAG       ', TAGSEC, 1, 1, 1, NDO, KERR(2) )
      DO 200  N = 1,2
        NN = N
        IF ( KERR(N) .EQ. 0 )  GO TO 200
        CALL CKILL (6HUTCTM , NN, KERR(NN) )
  200 CONTINUE
C
C  Compute the Julian date at 0 hours UTC for the year, month, day.
C  Use function JDY2K to convert year, month, day to Julian date. Year can be
C   either 2-digit or 4-digit.
      IYY = ITAG(1)
      IM  = ITAG(2)
      ID  = ITAG(3)
      XJD = JDY2K(IYY,IM,ID)
c     write(6,'("UTCTM: ITAG(1), New XJD = ",i5,f12.2)') IYY,XJD
C
C     Compute the UTC time as a fraction of the UTC day.
      UTC = ( DFLOAT( ITAG(4) ) * 3600.D0
     1      + DFLOAT( ITAG(5) ) * 60.D0
     2      + TAGSEC ) / SECDAY
C
C     Check for debug.
      IF ( KUTCD .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine UTCTM." )
      WRITE(6,7)' ITAG    ',ITAG
    7 FORMAT(A,15I8/(9X,15I8))
      WRITE(6,8) 'SECDAY',SECDAY
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' TAGSEC  ',TAGSEC
      WRITE ( 6, 9200 )  UTC, XJD
 9200 FORMAT (1X, "UTC  = ", D30.16, /, 1X,
     1            "XJD  = ", D30.16 )
C
  500 RETURN
      END
