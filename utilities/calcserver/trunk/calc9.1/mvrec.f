C
      BLOCK DATA KRECX
      COMMON /KMVREC/ KREC,INTREC
      DATA   KREC /0/
      END
C                                                                    
         SUBROUTINE MVREC (N1,N2,N3,KERR)
         INTEGER  N1,N2,N3,KERR, KREC, KHALT, INTREC
         COMMON  /KMVREC/ KREC,INTREC
         KERR  = 0
         INTREC = 1
         KHALT  = 14400
         KREC = KREC + 1
C         IF (KREC.GT.KHALT) KERR = 1
         RETURN
         END
C
         INTEGER*2 FUNCTION GETUNIT (I1)
C
         INTEGER*2 I1
         GETUNIT = 0
         RETURN
         END
         SUBROUTINE FINIS (IERR)
C
         RETURN
         END
C
         SUBROUTINE ASK (CHR8,I1,I2,I3,I4,I5,I6,I7,I8)
C
         CHARACTER*8 CHR8
         INTEGER*4 FSTRCMP
         INTEGER I1,I2,I3,I4,I5,I6,I7,I8
C
         I8 = 1
C
         RETURN
         END
         SUBROUTINE WRIDR
C
         RETURN
         END
