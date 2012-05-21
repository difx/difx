
      SUBROUTINE GETBIT( WORD, BIT, TOUT )
C
C     Routine to break a word into component bits for decoding some
C     monitor data.  Input is REAL*8 because that is what is delivered
C     by GETNUM.  The input word was assumed to be in the form of two's
C     compliment with no byte swapping.
C
      REAL*8      WORD
      INTEGER     IWORD, ITEST, TOUT
      INTEGER     BIT(16), N
C
C     Initialize the bit array.
C
      DO 10 N = 1, 16
         BIT(N) = 0
10    CONTINUE
C
C     Test that the input word is in the reasonable range.
C
      IF( WORD .LT. -32768 .OR. WORD .GT. 65535 ) THEN 
          WRITE(TOUT,*)  ' Trying to convert too large a number to ',
     1         '16 bits.', WORD
          WORD = DMOD( WORD, 32768.D0 )
      END IF
C
C     A negative number, a, represented in two's compliment with n bits
C     (the first set to one) has the same bit pattern as a positive
C     number equal to  a + 2**n  (which, since a is negative, has a value
C     between  2**(n-1) and (2**n - 1).  Therefore, if WORD is negative,
C     add 2**16 = 65536 and then treat all bits equally.  While at it,
C     convert to an integer.
C
      IF( WORD .LT. 0 ) THEN
         IWORD = 65536 + WORD
      ELSE
         IWORD = WORD
      END IF
C
C     Now determine which bits are set. 
C
      DO 100 N = 16, 1, -1
         ITEST = 2**(N-1)
         BIT(N) = 0
         IF( IWORD .GE. ITEST ) THEN
            BIT(N) = 1
            IWORD = IWORD - ITEST
         END IF
100   CONTINUE
C   
      RETURN
      END



