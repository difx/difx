      SUBROUTINE sla_PREBN (BEP0, BEP1, RMATP)
*+
*     - - - - - - 
*      P R E B N
*     - - - - - -
*
*  Generate the matrix of precession between two epochs,
*  using the old, pre IAU 1976, Bessel-Newcomb model, in
*  Andoyer's formulation (double precision)
*
*  Given:
*     BEP0    dp         beginning Besselian epoch
*     BEP1    dp         ending Besselian epoch
*
*  Returned:
*     RMATP  dp(3,3)    precession matrix
*
*  The matrix is in the sense   V(BEP1)  =  RMATP * V(BEP0) .
*
*  Reference:
*     Smith et al 1989, Astron. J., 97, 1 p269.
*
*  Called:  sla_DEULER
*
*  P.T.Wallace   Starlink   12 April 1990
*-

      IMPLICIT NONE

      DOUBLE PRECISION BEP0,BEP1,RMATP(3,3)

*  Arc seconds to radians
      DOUBLE PRECISION AS2R
      PARAMETER (AS2R=0.4848136811095359949D-05)

      DOUBLE PRECISION BIGT,T,TAS2R,W,ZETA,Z,THETA



*  Interval between basic epoch B1850.0 and beginning epoch in TC
      BIGT = (BEP0-1850D0)/100D0

*  Interval over which precession required, in tropical centuries
      T = (BEP1-BEP0)/100D0

*  Euler angles
      TAS2R = T*AS2R
      W = 2303.5545D0+(1.39720D0+0.000060D0*BIGT)*BIGT

      ZETA = (W+(0.30240D0-0.000270D0*BIGT+0.017995D0*T)*T)*TAS2R
      Z = (W+(1.09480D0+0.000390D0*BIGT+0.018325D0*T)*T)*TAS2R
      THETA = (2005.112D0+(-0.8529D0-0.00037D0*BIGT)*BIGT+
     :        (-0.4265D0-0.00037D0*BIGT-0.04180D0*T)*T)*TAS2R

*  Rotation matrix
      CALL sla_DEULER('ZYZ',-ZETA,THETA,-Z,RMATP)

      END
