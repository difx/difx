      SUBROUTINE KEYADD( NAME, DEFAULT, N, KD, KC, KI )
C
C     Routine originally for SCHED to help simplify setting up
C     the arrays for KEYIN.  This one adds N occurances of NAME
C     with DEFAULT to the portions of KD that will be used in
C     the KEYIN call for the NAMES (second half) and VALUE (first
C     half) arrays (this order changed, 25 June 1997 to avoid use
C     of equivalences - tickled a bug in a new SUN compiler).
C
C     The first three elements of KI are the counters needed by
C     the KEYIN related routine.  They are:
C      1)  MK       The declared size of the arrays (KD is twice this).
C      2)  NPARS    The number of parameters for KEYIN (one for each
C                   element of any arrays).
C      3)  NKEY     Number of distinct keys (one for each array)
C
C     Because of the above, the pointers in KI are shifted to start
C     in cell 4.  To avoid confusion, the keys in KC are also shifted.
C
C     The calling routine should initialize MK, NPARS, and NKEY
C     (KI(1) to KI(3)) to the declared array sizes, 0 and 3.
C
      DOUBLE PRECISION  KD(*), DEFAULT, R8NAME
      CHARACTER         NAME*(*), KC(*)*(*), ONAME*20
      INTEGER           KI(*), N
      INTEGER           MK, NPARS, NKEY, I, NCH
C ----------------------------------------------------------------------
C     Get the array size numbers prior to this call.
C
      MK    = KI(1)
      NPARS = KI(2)
      NKEY  = KI(3)
C
C     Check array bounds.
C
      IF( N .LE. 0 ) THEN
         CALL ERROR( 'KEYADD: 0 or negative array size' )
      END IF
      IF( NPARS + N .GT. MK ) THEN
         WRITE(*,*) 'NPARS, N, MK, NKEY, NAME: ',
     1            NPARS, N, MK, NKEY, ' ', NAME
         CALL ERROR( 'KEYADD: Overflowing KEYIN arrays.' )
      END IF
      IF( NKEY + 3 + 1 .GT. MK ) THEN
         WRITE(*,*) 'NPARS, N, MK, NKEY, NAME: ',
     1            NPARS, N, MK, NKEY, ' ', NAME
         CALL ERROR( 'KEYADD: Overflowing list of keys.' )
      END IF
C
C     Check length of NAME.
C
      NCH = LEN( NAME )
      IF( NCH .GT. 8 ) THEN
         ONAME = NAME
         CALL ERROR( 'KEYADD: NAME too long: '// ONAME )
      END IF
C
C     Convert the NAME to double precision using a keyin subroutine.
C     Also initialize R8NAME to blank first to get all characters.
C
      CALL KPACK( '        ', R8NAME )
      CALL KPACK( NAME, R8NAME )
C
C     Add N NAMEs and defaults to what will be the NAMES and
C     values arrays (combined into KD).
C
      DO I = 1, N
         KD(MK+NPARS+I) = R8NAME
         KD(NPARS+I) = DEFAULT
      END DO
C
C     Add to the list of keys and pointers.
C
      KC(NKEY+1) = NAME
      KI(NKEY+1) = NPARS+1
C
C     Increment counters.
C
      KI(2) = KI(2) + N
      KI(3) = KI(3) + 1      
C
      RETURN
      END
