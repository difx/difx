      BLOCK DATA CMATHB
      IMPLICIT None
!
! 1.    CMABD
!
! 1.1   CMABD PROGRAM SPECIFICATION
!
! 1.1.1 CMABD is the block data which initializes the mathematical
!       constants.  Rather than obtaining the mathematical constants
!       from the database, they must be initialized here because of
!       the problems with differing representations of the constants
!       in ASCII and binary.
!
! 1.1.2 RESTRICTIONS - NONE
!
! 1.1.3 REFERENCES - CHEMICAL RUBBER COMPANY HANDBOOK
!
! 1.2   CMABD PROGRAM INTERFACE
!
! 1.2.1 CALLING SEQUENCE - NONE
!
! 1.2.2 COMMON BLOCK
!
      Real*8         PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON /CMATH/ PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!
!     VARIABLES "TO":
!
!     1. PI     - THE MATHEMATICAL CONSTANT PI (UNITLESS)
!     2. TWOPI  - PI * 2.0D0 (UNITLESS)
!     3. HALFPI - PI / 2.0D0 (UNITLESS)
!     4. CONVD  - THE CONVERSION FACTOR FROM DEGREES TO RADIANS (RAD/DEG)
!     5. CONVDS - THE CONVERSION FACTOR FROM ARCSECONDS TO RADIANS
!                 (RAD/ARCSECOND)
!     6. CONVHS - THE CONVERSION FACTOR FROM TIME SECONDS TO RADIANS
!                 (RADIANS / TIME SECOND)
!     7. SECDAY - THE NUMBER OF TIME SECONDS PER DAY (SECONDS / DAY)
!
!     VARIABLES "FROM": NONE
!
! 1.2.3 PROGRAM SPECIFICATIONS -
!
      DATA PI     /3.1415926535897932D0/, &
     &     TWOPI  /6.2831853071795865D0/, &
     &     HALFPI /1.5707963267948966D0/, &
     &     CONVD  /1.7453292519943296D-02/, &
     &     CONVDS /4.8481368110953599D-06/, &
     &     CONVHS /7.2722052166430399D-05/, &
     &     SECDAY /8.6400D04/
!
! 1.2.4 CONSTANTS USED - PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
!
! 1.2.5 PROGRAMMER - BRUCE SCHUPLER 02/15/78
!                    SAVITA GOEL    06/04/87 (CDS FOR A900)
!                    David Gordon 94.04.14 Converted to Implicit None
!
! 1.3   PROGRAM STRUCTURE - NONE
!
      END
