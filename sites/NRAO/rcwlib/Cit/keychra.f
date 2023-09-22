      SUBROUTINE KEYCHRA( NAME, DEFAULT, NVCH, N, KD, KC, KI )
C
C     Routine originally for SCHED for adding an array of character 
C     strings to the arrays for KEYIN.  See KEYADD for details of 
C     setting up the arrays.  Adapted from KEYCHR May 21, 2010. RCW
C
C     This adds NAME to one element of the KEYIN arrays and blanks
C     to enough more to get enough characters for NVCH (actually
C     a bit more up to something divisible by 8).  The maximum
C     string length is 256 characters.  It puts DEFAULT in the 
C     data array.  It then repeats those steps N times for an 
C     N element array.  Only one name is added to KC, the character
C     string versions of the keys.
C
      DOUBLE PRECISION  KD(*), R8NAME, BLANK
      CHARACTER         NAME*(*), KC(*)*(*), DEFAULT*(*), ONAME*20
      INTEGER           KI(*), NVCH, NCH, N8
      INTEGER           MK, NPARS, NKEY, I, J, N, NE
C ------------------------------------------------------------------
      CALL KPACK( '        ', BLANK )
C
C     Get the array size numbers prior to this call.
C
      MK    = KI(1)
      NPARS = KI(2)
      NKEY  = KI(3)
C      write(*,*) 'KEYCHRA: adding ', NAME, MK, NPARS, NKEY, NVCH
C
C     Get then number of double precision words that will be needed.
C     for each element
C
      N8 = 1 + ( NVCH - 1 ) / 8
C
C     Check array bounds.  Only the programmer should see this.
C
      IF( NPARS + N8 * N .GT. MK ) THEN
         WRITE(*,*) 'KEYCHR: NPARS, N8, N, MK, NKEY, NAME: ',
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
      DO J = 1, N
         NE = NPARS + ( J - 1) * N8 + 1
         KD(MK+NE) = R8NAME
         KD(NE) = BLANK
         IF( N8 .GT. 1 ) THEN
            DO I = 2, N8
               KD(MK+NE+I-1) = BLANK
               KD(NE+I-1) = BLANK
            END DO
         END IF
C
C        Set the default.
C
         CALL KPACK( DEFAULT, KD(NE) )
      END DO
C
C     Add to the list of keys and pointers.
C
      KC(NKEY+1) = NAME
      KI(NKEY+1) = NPARS+1
C
C     Increment counters.
C
      KI(2) = NPARS + N8 * N
      KI(3) = NKEY + 1      
C
      RETURN
      END




