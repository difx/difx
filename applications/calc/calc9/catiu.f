      SUBROUTINE ATIMA
      IMPLICIT None 
C 
C 1.    ATIMA 
C 
C 1.1   ATIMA PROGRAM SPECIFICATION 
C 
C 1.1.1 ATIMA ADDS ENTRIES TO THE TABLE OF CONTENTS FOR THE ATIME UTILITY
C       ROUTINE TEXT MESSAGE. IT ALSO ADDS ENTRIES TO THE TABLE OF CONTENTS
C       FOR THE FLOW CONTROL MESSAGE.
C 
C 1.2   ATIMA PROGRAM INTERFACE 
C 
C 1.2.4 DATA BASE ACCESS -
C            ACCESS CODES:
C              1.  'ATI MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 ATIME UTILITY ROUTINE TEXT MESSAGE. 
C              2.  'ATI CFLG'  -  THE DATA BASE ACCESS CODE FOR THE ATIME
C                                 UTILITY ROUTINE FLOW CONTROL MESSAGE.
C 
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: TOCUP
C             CALLED SUBROUTINES: ADDA
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  02/04/77
C                    PETER DENATALE 07/18/77
C                    BRUCE SCHUPLER 09/16/77
C                    SAVITA GOEL    06/04/87 (CDS FOR A900)
C                    Jim Ryan 89.07.25 Documentaton simplified.
C
C     ATIMA Program Structure
C
C     ADD for ATIME utility text message.
      CALL ADDA (1,'ATI MESS','ATIME Message Definition        ',
     1     40, 1, 1 )
C
C     ADD for ATIME utility flow control message.
      CALL ADDA (1,'ATI CFLG','ATIME Flow Control Message Def. ',
     1     40,1,1)
C
C     Normal Program Conclusion.
      RETURN
      END
C
C*******************************************************************************
      SUBROUTINE ATIMI
      IMPLICIT None
C
C 2.    ATIMI
C
C 2.1   ATIMI PROGRAM SPECIFICATION
C
C 2.1.1 ATIMI IS THE ATIME UTILITY ROUTINE INPUT AND INITIALIZATION SECTION.
C
C 2.2   ATIMI PROGRAM INTERFACE 
C 
C 2.2.2 COMMON BLOCKS USED -
C 
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
C           VARIABLES 'TO/FROM':
C            1. ATMUTC(3)   - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
C                             CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME
C                             RATE OF CHANGE OF 'TAI MINUS UTC'. 
C                             (DAYS, SEC, SEC/SEC)
C 
      INCLUDE 'ccon.i'               
C            VARIABLES 'FROM':
C              1. KATIC - THE ATIME UTILITY ROUTINE FLOW CONTROL FLAG.
C              2. KATID - THE ATIME UTILITY ROUTINE DEBUG OUTPUT FLAG.

      INCLUDE 'inputs.i'
C            Variables from:
C              1. Input_EOP - T/F logical flag telling whether to use external
C                             EOP file input
C
C 2.2.3 PROGRAM SPECIFICATIONS -
C
      INTEGER*2 NDO(3), Kerr
      INTEGER*2      LATIU(40),      LON(40),    LOFF(40)
      CHARACTER*40 C_LATIU(2),     C_LON(2),   C_LOFF(2)
      EQUIVALENCE (C_LATIU,LATIU),(C_LON,LON),(C_LOFF,LOFF)
C
      DATA C_LATIU /
     .'ATIME Utility routine - VERSION # 3, Las',
     .'t modification - 89:12:12 Jim Ryan.     '/
C
      DATA C_LON /
     .'ATIME Utility routine is turned on.     ',
     .'                                        '/
C
      DATA C_LOFF /
     .'ATIME Utility routine is turned off.    ',
     .'                                        '/
C
C 2.2.4 DATA BASE ACCESS -
C            'GET' VARIABLES:
C              1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. THIS ARRAY
C                             CONTAINS RESPECTIVELY THE EPOCH, VALUE, AND TIME 
C                             RATE OF CHANGE OF 'TAI MINUS UTC'.
C                             (DAYS, SEC, SEC/SEC)
C            'PUT' VARIABLES:
C              1. LATIU(40) - THE ATIME UTILITY ROUTINE TEXT MESSAGE.
C              2. LON(40)   - THE ATIME UTILITY 'TURNED ON' MESSAGE.
C              3. LOFF(40)  - THE ATIME UTILITY 'TURNED OFF' MESSAGE.
C            ACCESS CODES:
C              1. 'ATI MESS'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 ATIME UTILITY ROUTINE TEXT MESSAGE. 
C              2. 'TAI- UTC'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 'TAI MINUS UTC' INFORMATION ARRAY.
C              3. 'ATI CFLG'  -  THE DATA BASE ACCESS CODE FOR THE 
C                                 ATIME UTILITY FLOW CONTROL MESSAGE. 
C 
C 2.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: INITL 
C             CALLED SUBROUTINES: GET4, KILL, PUTA
C 
C 2.2.8 PROGRAM VARIABLES -
C           1. KERR   - THE DATA BASE ERROR RETURN FLAG.
C           2. NDO(3) - THE DATA BASE RETURN ARRAY INDICES.
C
C 2.2.9 PROGRAMMER - DALE MARKHAM  02/04/77
C                    PETER DENATALE 07/18/77
C                    BRUCE SCHUPLER 02/07/78
C                    Jim Ryan 89.07.25 Documentaton simplified.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    D. Gordon 96.02.27 Double dimensioning of ATMUTC(3)
C                             removed, found by Warwick Wilson, ATNF.
C                    D. Gordon 98.05.01 Expanded and renamed common block 
C                             ATICM to EOPCM. Pass in ATMUTC if using external
C                             EOP input.
C
C     ATIMI Program Structure
C
C     PUT the ATIME utility text message.
      CALL PUTA ('ATI MESS      ', LATIU, 40, 1, 1 )
C
C     PUT the flow control message based on the value of KATIC.
      IF (KATIC .NE. 1) CALL PUTA ('ATI CFLG      ',LON,40,1,1)
      IF (KATIC .EQ. 1) CALL PUTA ('ATI CFLG      ',LOFF,40,1,1)
C
      IF (.not. Input_EOP) THEN         ! Already have ATMUTC?
C      GET the 'TAI MINUS UTC' array from the database and check for errors.
        CALL GET4 ('TAI- UTC      ', ATMUTC, 3, 1, 1, NDO, KERR )
        IF ( KERR .EQ. 0 )  GO TO 300
         CALL CKILL (6HATIMI , 1, KERR )
      ENDIF                             ! Already have ATMUTC?
C
C     Check KATID for debug output.
  300 IF ( KATID .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for subroutine ATIMI." )
C
      WRITE(6,8)' ATMUTC  ',ATMUTC
    8 FORMAT(A,4D25.16/(7X,5D25.16))
C
C     Normal conclusion.
  500 RETURN
      END
C
C******************************************************************************
      SUBROUTINE ATIME ( UTC, XJD, AT, DUTCAT )
      IMPLICIT None
C
C 3.    ATIME
C
C 3.1   ATIME PROGRAM SPECIFICATION
C
C 3.1.1 ATIME IS THE UTILITY ROUTINE WHICH COMPUTES THE ATOMIC TIME
C       FROM THE UTC TIME AND ALSO CALCULATES THE PARTIAL DERIVATIVE
C       OF THE UTC TIME WITH RESPECT TO THE ATOMIC TIME.
C
C 3.2   ATIME PROGRAM INTERFACE 
C 
      Real*8 UTC, XJD, AT, DUTCAT 
C 
C 3.2.1 CALLING SEQUENCE -
C          INPUT VARIABLES:
C            1. UTC - THE UTC FRACTION OF THE UTC DAY. (SEC/SEC) 
C            2. XJD - THE JULIAN DATE AT 0:00 UTC OF THE DATE IN QUESTION.
C          OUTPUT VARIABLES: 
C            1. AT     - THE ATOMIC TIME FRACTION OF THE ATOMIC TIME DAY. (DAYS)
C            2. DUTCAT - THE PARTIAL DERIVATIVE OF THE UTC TIME WITH RESPECT TO
C                        THE ATOMIC TIME. (SEC/SEC) 
C 
C 3.2.2 COMMON BLOCKS USED -
C 
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY 
C           VARIABLES 'FROM':
C             1. SECDAY - THE CONVERSION FACTOR OF COORDINATE TIME SECONDS PER
C                         COORDINATE TIME DAY. (SEC/DAY)
C 
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
C           VARIABLES 'TO/FROM':
C             1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. CONTAINS
C                            RESPECTIVELY THE EPOCH, VALUE, AND TIME RATE OF
C                            CHANGE OF 'TAI MINUS UTC'. (DAYS, SEC, SEC/SEC) 
C 
      INCLUDE 'ccon.i'             
C           VARIABLES 'FROM':
C             1. KATIC - THE ATIME UTILITY ROUTINE FLOW CONTROL FLAG.
C             2. KATID - THE ATIME UTILITY ROUTINE DEBUG OUTPUT FLAG.
C 
C 3.2.3 PROGRAM SPECIFICATIONS -
C 
C 3.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: DRIVG
C             CALLED SUBROUTINES: NONE
C
C 3.2.7 CONSTANTS USED - SECDAY
C
C 3.2.8 PROGRAM VARIABLES - NONE
C
C 3.2.9 PROGRAMMER - DALE MARKHAM  02/04/77
C                    PETER DENATALE 07/18/77
C                    BRUCE SCHUPLER 02/07/78
C                    Jim Ryan 89.07.25 Documentaton simplified.
C
C   ATIME Program Structure
C
C     Compute the atomic time fraction of the atomic time day.
      AT = + ATMUTC(2) / SECDAY
     1     + ATMUTC(3) * ( XJD  -  ATMUTC(1) )
     2     + UTC
C
C     Compute the partial derivative of the UTC time with respect
C     to atomic time.
      DUTCAT = 1.D0 / ( 1.D0  +  ATMUTC(3) )
C
C     Check KATIC to see if the utility is to be turned off.
      IF ( KATIC .NE. 1 )  GO TO 400
        AT     = UTC
        DUTCAT = 1.D0
C
C     Check KATID for debug output.
  400 IF ( KATID .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility ATIME." )
C
      WRITE(6,8)' ATMUTC  ',ATMUTC
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' SECDAY  ',SECDAY
      WRITE ( 6, 9200 )  UTC, XJD, AT, DUTCAT
 9200 FORMAT (1X, "UTC    = ", D30.16, /, 1X,
     1            "XJD    = ", D30.16, /, 1X,
     2            "AT     = ", D30.16, /, 1X,
     3            "DUTCAT = ", D30.16 )
C
  500 RETURN
      END
