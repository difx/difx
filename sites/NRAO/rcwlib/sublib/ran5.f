      REAL FUNCTION RAN5(IDUM)
      INTEGER IDUM
C-
C Park and Miller's "Minimal Standard" random number generator (Comm.
C ACM, 31, 1192, 1988) with Bays-Durham shuffle. Call with IDUM negative
C to initialize. Be sure to preserve IDUM between calls.
C Reference: Press and Farrar, Computers in Physics, Vol.4, No. 2, 
C p.190, 1990.
C-
      INTEGER IA, IM, IQ, IR, NTAB
      REAL    AM, ATAB
      PARAMETER (IA=16807, IM=2147483647, AM=1.0/IM)
      PARAMETER (IQ=127773, IR=2836, NTAB=32, ATAB=NTAB-1)
C-
      INTEGER J, K
      REAL V(NTAB), Y
      SAVE V, Y
      DATA V/NTAB*0.0/, Y/0.5/
C-
      IF (IDUM.LE.0) THEN
          IDUM = MAX(-IDUM,1)
          DO 12 J=NTAB,1,-1
              K = IDUM/IQ
              IDUM = IA*(IDUM-K*IQ) - IR*K
              IF (IDUM.LT.0) IDUM = IDUM+IM
              V(J) = AM*IDUM
   12     CONTINUE
          Y = V(1)
      END IF
    1 CONTINUE
          K = IDUM/IQ
          IDUM = IA*(IDUM-K*IQ) - IR*K
          IF (IDUM.LT.0) IDUM = IDUM+IM
          J = 1 + INT(ATAB*Y)
          Y = V(J)
          RAN5 = Y
          V(J) = AM*IDUM
      IF (RAN5.EQ.0.0 .OR. RAN5.EQ.1.0) GOTO 1
      RETURN
      END
