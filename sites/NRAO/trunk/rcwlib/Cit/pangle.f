C*PANGLE -- convert angle in radians to sexagesimal character string
C+
      SUBROUTINE PANGLE(FMT,ANGLE,IH,M)
      CHARACTER*(*) FMT
      DOUBLE PRECISION ANGLE
      CHARACTER*(*) IH
      INTEGER M
C
C Convert an angle in radians to hours, min, sec or degrees, min, sec
C for printing.  The hours (degrees), minutes, and seconds fields are
C separated by a single space, or by the character specified as the
C second character in the FMT argument.
C
C Arguments:
C  FMT    (input)  : 'H' for conversion to hhmmss, 
C                    'D' for conversion to ddmmss.
C  ANGLE  (input)  : the angle in radians.
C  IH     (output) : receives the formatted character string,
C                    right justified.
C  M      (input)  : the number of decimal places required in the
C                    seconds field (may be zero).
C
C History:
C  1987 Nov 12 - TJP.
C  1991 May 18 - TJP.
C  1994 Aug  8 - add FMT option for separator (TJP).
C-----------------------------------------------------------------------
      CHARACTER*1 I(0:9),IBLANK,IPOINT,IPLUS,IMINUS,ISIGN
      CHARACTER*1 SEP, TST
      CHARACTER*2 TMP
      DOUBLE PRECISION UNIT,RD,HRAD,DRAD
      LOGICAL    HOUR
      INTEGER N, K, KL, IUN, IMIN, ISEC, IDEC
      PARAMETER  (HRAD = 3.819718634D0, DRAD = 57.29577952D0)
      DATA       IBLANK/' '/,IPOINT/'.'/,IPLUS/' '/,IMINUS/'-'/,
     1             I/'0','1','2','3','4','5','6','7','8','9'/
C
C
      TMP = FMT
      TST = TMP(1:1)
      SEP = TMP(2:2)
      IF (TST.EQ.'H' .OR. TST.EQ.'h') THEN
          UNIT = ANGLE*HRAD
          HOUR = .TRUE.
      ELSE IF (TST.EQ.'D' .OR. TST.EQ.'d') THEN
          UNIT = ANGLE*DRAD
          HOUR = .FALSE.
      ELSE
          IH = '**ERROR**'
      END IF
C
C            Check that N and M are reasonable
      N = LEN(IH)
      IF(HOUR.AND.N.GE.10+M) GOTO 2
      IF(.NOT.HOUR .AND. N.GE.11+M) GOTO 2
      DO 3 K = 1,N
    3       IH(K:K) = '*'
      RETURN
C
C            Set sign
    2   ISIGN = IPLUS
      IF(UNIT.LT.0D0) ISIGN = IMINUS
      UNIT = DABS(UNIT)*3600D0
C
C            Round off
      RD = 10D0**(-M)
      IF(DMOD(UNIT/RD,1D0).GT.0.5D0) UNIT = UNIT+RD
C
C            Obtain the desired numbers
      IUN = UNIT/3600D0
      IMIN = (UNIT - IUN*3600D0)/60D0
      ISEC = (UNIT - IUN*3600D0 - IMIN*60D0)
      IDEC = (UNIT - IUN*3600D0 - IMIN*60D0 - ISEC)*10**M
C
C
C            Set up array IH
C
      IF(M.EQ.0) GOTO 10
C
      DO 6 K = 1,M
          IH(N-K+1:N-K+1) = I(MOD(IDEC,10**K)/(10**(K-1)))
    6       CONTINUE
C
   10 IH(N-M:N-M) = IPOINT
      IH(N-M-1:N-M-1) = I(MOD(ISEC,10))
      IH(N-M-2:N-M-2) = I(ISEC/10)
      IH(N-M-3:N-M-3) = SEP
      IH(N-M-4:N-M-4) = I(MOD(IMIN,10))
      IH(N-M-5:N-M-5) = I(IMIN/10)
      IH(N-M-6:N-M-6) = SEP
      IH(N-M-7:N-M-7) = I(MOD(IUN,10))
      IH(N-M-8:N-M-8) = I(MOD(IUN,100)/10)
C
      IF(.NOT.HOUR .AND.IUN.GT.100) GOTO 20
      IH(N-M-9:N-M-9) = ISIGN
      KL = N-M-10
      GOTO 21
C
   20 IH(N-M-9:N-M-9) = I(MOD(IUN,1000)/100)
      IH(N-M-10:N-M-10) = ISIGN
      KL = N-M-11
C
   21 IF(KL.LE.0) RETURN
      DO 22 K = 1,KL
   22       IH(K:K) = IBLANK
C-----------------------------------------------------------------------
      END
