      SUBROUTINE ATIME ( UTC, XJD, AT, DUTCAT, TT )
      IMPLICIT None
!
! 3.    ATIME
! 3.1   ATIME PROGRAM SPECIFICATION
! 3.1.1 ATIME IS THE UTILITY ROUTINE WHICH COMPUTES THE ATOMIC TIME
!       FROM THE UTC TIME AND ALSO CALCULATES THE PARTIAL DERIVATIVE
!       OF THE UTC TIME WITH RESPECT TO THE ATOMIC TIME.
!
! 3.2   ATIME PROGRAM INTERFACE
      Real*8 UTC, XJD, AT, DUTCAT, TT
!
! 3.2.1 CALLING SEQUENCE -
!          INPUT VARIABLES:
!            1. UTC - THE UTC FRACTION OF THE UTC DAY. (SEC/SEC)
!            2. XJD - THE JULIAN DATE AT 0:00 UTC OF THE DATE IN QUESTION.
!          OUTPUT VARIABLES:
!            1. AT     - THE ATOMIC TIME FRACTION OF THE ATOMIC TIME DAY. (DAYS)
!            2. DUTCAT - THE PARTIAL DERIVATIVE OF THE UTC TIME WITH RESPECT TO
!                        THE ATOMIC TIME. (SEC/SEC)
!            3. TT     - Terrestrial Time (days).
!
! 3.2.2 COMMON BLOCKS USED -
!
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!           VARIABLES 'FROM':
!             1. SECDAY - THE CONVERSION FACTOR OF COORDINATE TIME SECONDS PER
!                         COORDINATE TIME DAY. (SEC/DAY)
!
      Real*8  ATMUTC(3), ROTEPH(2,20), A1UTC(3), A1DIFF(3)
      COMMON / EOPCM / ATMUTC, ROTEPH, A1UTC, A1DIFF
!           VARIABLES 'TO/FROM':
!             1. ATMUTC(3) - THE 'TAI MINUS UTC' INFORMATION ARRAY. CONTAINS
!                            RESPECTIVELY THE EPOCH, VALUE, AND TIME RATE OF
!                            CHANGE OF 'TAI MINUS UTC'. (DAYS, SEC, SEC/SEC)
!
      INCLUDE 'ccon.i'
!           VARIABLES 'FROM':
!             1. KATIC - THE ATIME UTILITY ROUTINE FLOW CONTROL FLAG.
!             2. KATID - THE ATIME UTILITY ROUTINE DEBUG OUTPUT FLAG.
!
! 3.2.3 PROGRAM SPECIFICATIONS -
! 3.2.6 SUBROUTINE INTERFACE -
!             CALLER SUBROUTINES: DRIVG
!             CALLED SUBROUTINES: NONE
! 3.2.7 CONSTANTS USED - SECDAY
! 3.2.8 PROGRAM VARIABLES - NONE
! 3.2.9 PROGRAMMER - DALE MARKHAM  02/04/77
!                    PETER DENATALE 07/18/77
!                    BRUCE SCHUPLER 02/07/78
!                    Jim Ryan 89.07.25 Documentaton simplified.
!                    Jim Ryan 02Sept Integer*4 mods.
!                    D. Gordon 2012  TT added.
!
!   ATIME Program Structure
!
!     Compute the atomic time fraction of the atomic time day.
      AT = + ATMUTC(2) / SECDAY                                         &
     &     + ATMUTC(3) * ( XJD  -  ATMUTC(1) )                          &
     &     + UTC
!
!     Compute the partial derivative of the UTC time with respect
!     to atomic time.
      DUTCAT = 1.D0 / ( 1.D0  +  ATMUTC(3) )
!
!     Compute TT, Terrestrial Time. 
       TT = AT + 32.184D0/86400.D0
!       write (6,*) 'ATIME: AT,TT (sec)', AT*86400.D0, TT*86400.D0
!
!     Check KATIC to see if the utility is to be turned off.
      IF ( KATIC .NE. 1 )  GO TO 399
        AT     = UTC
        DUTCAT = 1.D0
!
!     Check KATID for debug output.
 399  Continue
  400 IF ( KATID .EQ. 0 )  GO TO 500
      WRITE ( 6, 9)
    9 FORMAT (1X, "Debug output for utility ATIME." )
!
      WRITE(6,8)' ATMUTC  ',ATMUTC
    8 FORMAT(A,4D25.16/(7X,5D25.16))
      WRITE(6,8)' SECDAY  ',SECDAY
      WRITE ( 6, 9200 )  UTC, XJD, AT, TT, DUTCAT
 9200 FORMAT (1X, "UTC    = ", D30.16, /, 1X,                           &
     &            "XJD    = ", D30.16, /, 1X,                           &
     &            "AT     = ", D30.16, /, 1X,                           &
     &            "TT     = ", D30.16, /, 1X,                           &
     &            "DUTCAT = ", D30.16 )
!
  500 RETURN
      END
