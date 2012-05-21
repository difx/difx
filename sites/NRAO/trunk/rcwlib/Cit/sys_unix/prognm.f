C*PROGNM -- return file name of current program [Convex-UNIX]
C+
      SUBROUTINE PROGNM (PROG)
      CHARACTER*(*) PROG
C
C Return the file name of the currently executing program, minus
C any directories.
C
C Argument:
C  PROG   (output) : receives the file name.
C
C Subroutines required:
C  GETARG (Convex)
C
C History:
C  1990 Jan  9 - TJP.
C-----------------------------------------------------------------------
      CHARACTER*255 T
      INTEGER I,J
C
      CALL GETARG(0, T)
      I = 1
   10 J = INDEX(T(I:),'/')
      IF (J.EQ.0) THEN
          PROG = T(I:)
      ELSE
          I = I+J
          GOTO 10
      END IF
C
      END
