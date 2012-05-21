      SUBROUTINE SORTCW( N, RA, IND )
C
C     This started as a hand typed copy of Numerical Recipes 
C     routine of the same name.  But I modified it to also return the
C     input indices of the sorted array so that those indices can be
C     used as pointers to associated data.  The routine does a 
C     heapsort of an array RA of length N.  The output indices are 
C     in array IND also of length N.
C
C     There might be license issues with this routine so be care
C     should be taken on what I use it with (like maybe not the
C     distributed SCHED).  The original routine I believe was taken
C     from a copy of Numerical Recipes that predated the license
C     issues, but there are issues more recently.  Since I retyped
C     and changed it, I'm not really sure of the status.
C     These comments typed 2004mar01 when IND was added.  The original
C     is far older.  Craig Walker
C
      INTEGER I, J, L, N, IR, IND(N), IIND
      REAL RA(N), RRA
C -------------------------------------------------------------------
C
      L = N / 2 + 1
      IR = N
10    CONTINUE
         IF( L .GT. 1 ) THEN
            L = L - 1
            RRA = RA(L)
            IIND = IND(L)
         ELSE
            RRA = RA(IR)
            IIND = IND(IR)
            RA(IR) = RA(1)
            IND(IR) = IND(1)
            IR = IR - 1
            IF( IR .EQ. 1 ) THEN
               RA(1) = RRA
               IND(1) = IIND
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
               IND(I) = IND(J)
               I = J
               J = J + J
            ELSE
               J = IR + 1
            END IF
         GO TO 20
         END IF
         RA(I) = RRA
         IND(I) = IIND
      GO TO 10
      END
