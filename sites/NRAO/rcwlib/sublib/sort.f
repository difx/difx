      SUBROUTINE SORT( N, RA )
C
C     Hand typed copy of Numerical Recipes routine of the same name.
C     It does a heapsort of an array RA of length N.
C
      INTEGER I, J, L, N, IR
      REAL RA(N), RRA
C
      L = N / 2 + 1
      IR = N
10    CONTINUE
         IF( L .GT. 1 ) THEN
            L = L - 1
            RRA = RA(L)
         ELSE
            RRA = RA(IR)
            RA(IR) = RA(1)
            IR = IR - 1
            IF( IR .EQ. 1 ) THEN
               RA(1) = RRA
               RETURN
            END IF
         END IF
         I = L
         J = L + L
20       IF( J .LE. IR ) THEN
            IF( J .LT. IR ) THEN
               IF( RA(J) .LT. RA(J+1)) J = J + 1
            END IF
            IF( RRA .LT. RA(J) ) THEN
               RA(I) = RA(J)
               I = J
               J = J + J
            ELSE
               J = IR + 1
            END IF
         GO TO 20
         END IF
         RA(I) = RRA
      GO TO 10
      END
