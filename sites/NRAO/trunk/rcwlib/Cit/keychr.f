      SUBROUTINE KEYCHR( NAME, DEFAULT, NVCH, KD, KC, KI )
C
C     Routine originally for SCHED for adding a character string
C     to the arrays for KEYIN.  See KEYADD for details of setting
C     up the arrays.
C
C     This adds NAME to one element of the KEYIN arrays and blanks
C     to enough more to get enough characters for NCH (actually
C     a bit more up to something divisible by 8).  The maximum
C     string length is 256 characters.
C
      DOUBLE PRECISION  KD(*), R8NAME, BLANK
      CHARACTER         NAME*(*), KC(*)*(*), DEFAULT*(*), ONAME*20
      INTEGER           KI(*), NVCH, NCH, N8
      INTEGER           MK, NPARS, NKEY, I
C ------------------------------------------------------------------
      CALL KPACK( '        ', BLANK )
C
C     Get the array size numbers prior to this call.
C
      MK    = KI(1)
      NPARS = KI(2)
      NKEY  = KI(3)
C      write(*,*) 'KEYCHR: adding ', NAME, MK, NPARS, NKEY, NVCH
C
C     Get then number of double precision words that will be needed.
C
      N8 = 1 + ( NVCH - 1 ) / 8
C
C     Check array bounds.  Only the programmer should see this.
C
      IF( NPARS + N8 .GT. MK ) THEN
         WRITE(*,*) 'KEYCHR: NPARS, N8, MK, NKEY, NAME: ',
     1            NPARS, N8, MK, NKEY, ' ', NAME
         CALL ERROR( 'KEYCHR: Overflowing KEYIN arrays.' )
      END IF
      IF( NKEY + 3 + 1 .GT. MK ) THEN
         WRITE(*,*) 'KEYCHR: MK, NKEY, NAME: ',
     1            MK, NKEY, ' ', NAME
         CALL ERROR( 'KEYCHR: Overflowing list of keys.' )
      END IF
C
C     Check length of NAME and DEFAULT
C
      NCH = LEN( NAME )
      IF( NCH .GT. 8 ) THEN
         CALL ERROR( 'KEYCHR: NAME too long: ' // ONAME )
      END IF
      NCH = LEN( DEFAULT )
      IF( NCH .GT. NVCH ) THEN
         ONAME = NAME
         CALL ERROR( 'KEYCHR: DEFAULT for '//ONAME//' too long.' )
      END IF
C
C     Convert the NAME to double precision using a keyin subroutine.
C
      R8NAME = BLANK
      CALL KPACK( NAME, R8NAME )
C
C     Add NAME and enough blanks to what will be the NAMES array.
C     Also initialize the VALUE section to blanks.
C
      KD(MK+NPARS+1) = R8NAME
      KD(NPARS+1) = BLANK
      IF( N8 .GT. 1 ) THEN
         DO I = 2, N8
            KD(MK+NPARS+I) = BLANK
            KD(NPARS+I) = BLANK
         END DO
      END IF
C
C     Set the default.
C
      CALL KPACK( DEFAULT, KD(NPARS+1) )
C
C     Add to the list of keys and pointers.
C
      KC(NKEY+1) = NAME
      KI(NKEY+1) = NPARS+1
C
C     Increment counters.
C
      KI(2) = NPARS + N8
      KI(3) = NKEY + 1      
C
      RETURN
      END




