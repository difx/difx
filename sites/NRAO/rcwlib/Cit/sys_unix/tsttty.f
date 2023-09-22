      LOGICAL FUNCTION TSTTTY(LUNIT)
      INTEGER LUNIT
C
C Return TRUE if the specified Fortran unit is connected to a terminal.
C------------------------------------------------------------------------
      LOGICAL ISATTY
C
      TSTTTY = ISATTY(LUNIT)
      END
