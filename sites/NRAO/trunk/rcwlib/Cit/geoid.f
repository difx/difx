C*GEOID  -- convert geodetic coordinates to geocentric coordinates
C+
      SUBROUTINE GEOID (R, T)
      DOUBLE PRECISION R, T
C
C Convert geodetic coordinates to geocentric coordinates (used by VLB
C programs FAKE, HAZI, UPTIME).
C-----------------------------------------------------------------------
      DOUBLE PRECISION RD
      PARAMETER (RD=57.29577951D0)
      DOUBLE PRECISION CT(3),CR(3),CR0,R0,X,ZR,ZT
      INTEGER I
      DATA   CT/-692.7430D0,+1.1633D0,-0.0026D0/
      DATA   CR/+0.001676438D0,-0.000003519D0,+0.000000008D0/
      DATA   CR0/0.998327073D0/, R0/6378160.D0/
C
      X  =  T/RD * 2.D0
      ZR = CR0
      ZT = 0.D0
      DO 10 I=1,3
          ZR  =  ZR + CR(I)*DCOS(X*I)
          ZT  =  ZT + CT(I)*DSIN(X*I)
   10 CONTINUE
      R  =  R + ZR*R0
      T  =  T + ZT/3600.D0
C
      END
