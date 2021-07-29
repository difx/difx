      SUBROUTINE C_HARDISP(CENT,UT1,UTC,OCEAMP,OCEPHS,FA2K,L,DZ,DW,DS)
!
!   This and the following subroutines are a modified version of 
!   program HARDISP, originally written by D. Agnew. It was obtained 
!   from the IERS Conventions (2010) software collection, at
!   ftp://tai.bipm.org/conv2010/chapter7. It has been converted from
!   Fortran 77 to Fortran 90. The inputs and outputs have been 
!   modified extensively for Calc usage. All routines were renamed
!   as below: 
!
!   Main HARDISP      ==> Subroutine C_HARDISP.
!   Subroutine ADMINT ==> Subroutine C_ADMINT.
!   Subroutine TDFRPH ==> Subroutine C_TDFRPH.
!   Subroutine SHELLS ==> Subroutine C_SHELLS.
!   Subroutine SPLINE ==> Subroutine C_SPLINE.
!   Subroutine RECURS ==> Subroutine C_RECURS.
!   Function EVAL     ==> Function C_EVAL.
!   
!
!  
!
!  This program uses a list of station displacements in the BLQ
!  format used by Scherneck and Bos for ocean loading, and outputs a
!  time series of computed tidal displacements, using an expanded set
!  of tidal constituents, whose amplitudes and phases are found by
!  spline interpolation of the tidal admittance.  A total of 342
!  constituent tides are included, which gives a precision of about
!  0.1%.
!
!  Given:
!     Input ocean loading coefficients.
!     Time in cenmturies from 2000.0 (CENT).
!     Fundamental arguments (FA2K(14)). 
!
!  Returned:
!     DZ - Radial tidal ocean loading displacement (meters)
!     DW - West tidal ocean loading displacement (meters)
!     DS - South tidal ocean loading displacement (meters)
!
!  Called:
!     C_ADMINT          Returns the ocean loading displacement amplitude,
!                       frequency, and phase of a set of tidal constituents
!     C_RECURS          Performs sine and cosine recursion
!
!  References:
!
!   Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
!   IERS Technical Note No. 36, BKG (2010)
!
!  Revisions:  
!  2005 August  Duncan Agnew     Original code, which is based on the 
!                                program hartid distributed with the 
!                                SPOTL loading package
!  2005 November D. Agnew        Corrected error in reading displacements
!  2005 December D. Agnew        Hartmann-Wenzel harmonics in admint
!                                subroutine
!  2007 December 17 G. Petit     Corrected 'intial' to 'initial' 
!                                (noted by T. Springer)
!  2008 June     D. Agnew        Corrected long-period tides, modernized
!                                control flow, added explicit typing, increased
!                                number of harmonics and added one decimal to
!                                their amplitudes
!  2009 February 16 G. Petit     Updated etutc subroutine for 2009.0 leap
!                                second
!  2009 June     25 B. Stetzler  Initial standardization of code
!  2009 June     26 B. Stetzler  Provided two test cases
!  2009 July     02 B. Stetzler  Capitalization for backwards compatibility
!  2009 August   19 B. Stetzler  Updated test cases
!  2012 November    D. Gordon    Changed to a subroutine with epoch and 
!                                ocean loading coefficients passed in 
!                                and converted to Fortran 90.
!-----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER*4 I,IDAY,IMONTH,IRNT,IRHI,IRLI,IT(5),LUO,NB,NL,NP,NT,     &
     &        NTIN,KK,NTOUT,MDAY, L
!+---------------------------------------------------------------------
!
!  Parameters below set the buffer size for computing the tides
!  recursively (NL), the number of harmonics used in the prediction
!  (NT; this must also be set in the subroutine admint) and the number
!  of harmonics read in (NTIN).
!
!----------------------------------------------------------------------
      PARAMETER (NL=600)
      PARAMETER (NT=342)
      PARAMETER (NTIN=11)

!     CHARACTER*40 DUMM
      REAL*4 SAMP
      REAL*8 F(NT),PZ(NT),PS(NT),PW(NT),SCR(3*NT)
      REAL*8 DR,PI

      REAL*8 OCEAMP(11,3,2), OCEPHS(11,3,2), CENT, FA2K(14), UT1,UTC
      REAL*4 TAMP(3,NTIN),TPH(3,NTIN)
      REAL*4 AMP(NTIN),PHASE(NTIN)
      REAL*4 AZ(NT),HCZ(2*NT)
      REAL*4 AS(NT),HCS(2*NT)
      REAL*4 AW(NT),HCW(2*NT)
      REAL*4 DZ(NL),DS(NL),DW(NL)
      REAL*4 WF(NT)
      INTEGER*4 IDT(6,NTIN)
!     COMMON/DATE/IT, CENT
      DATA DR/0.01745329252D0/,IRLI/1/
      PARAMETER ( PI = 3.1415926535897932384626433D0 ) 
      DATA LUO/6/

!  Cartwright-Tayler numbers of tides used in Scherneck lists:
!      M2, S2, N2, K2, K1, O1, P1, Q1, Mf, Mm, Ssa

      DATA IDT/                                                         &
     &  2, 0, 0, 0, 0, 0,   2, 2,-2, 0, 0, 0,   2,-1, 0, 1, 0, 0,       &
     &  2, 2, 0, 0, 0, 0,   1, 1, 0, 0, 0, 0,   1,-1, 0, 0, 0, 0,       &
     &  1, 1,-2, 0, 0, 0,   1,-2, 0, 1, 0, 0,   0, 2, 0, 0, 0, 0,       &
     &  0, 1, 0,-1, 0, 0,   0, 0, 2, 0, 0, 0/
!+----------------------------------------------------------------------
!
      IRNT = 1
      SAMP = 1.
!
!   Loop twice for the calculation of the displacements due to ocean
!   loading at sites #1 and 2.
!!!!  DO L=1,2      ! Start of Station Loop
!
       DO I=1,3
        DO KK=1,NTIN
         TAMP(I,KK) = OCEAMP(KK,I,L)
         TPH(I,KK)  = OCEPHS(KK,I,L)/DR
        ENDDO
       ENDDO
!
! Change sign for phase, to be negative for lags
       DO I=1,3
        DO KK=1,NTIN
          TPH(I,KK)=-TPH(I,KK)
        ENDDO
       ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Do I=1,3
!      Write(6,1021) (TAMP(I,KK), KK=1,11)
!     Enddo
!1021 Format('TAMP: ',11F8.5)
!     Do I=1,3
!      Write(6,1022) (TPH(I,KK),  KK=1,11)
!     Enddo
!1022 Format('TPH:  ',11F8.1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+---------------------------------------------------------------------
!
!  Find amplitudes and phases for all constituents, for each of the
!  three displacements. Note that the same frequencies are returned 
!  each time.
!
!  BLQ format order is vertical, horizontal EW, horizontal NS
!----------------------------------------------------------------------
      DO I=1,NTIN
        AMP(I)=TAMP(1,I)
        PHASE(I)=TPH(1,I)
      ENDDO
      CALL C_ADMINT(AMP,IDT,PHASE,CENT,FA2K,UTC,AZ,F,PZ,NTIN,NTOUT)
      DO I=1,NTIN
        AMP(I)=TAMP(2,I)
        PHASE(I)=TPH(2,I)
      ENDDO
      CALL C_ADMINT(AMP,IDT,PHASE,CENT,FA2K,UTC,AW,F,PW,NTIN,NTOUT)
      DO I=1,NTIN
        AMP(I)=TAMP(3,I)
        PHASE(I)=TPH(3,I)
      ENDDO
      CALL C_ADMINT(AMP,IDT,PHASE,CENT,FA2K,UTC,AS,F,PS,NTIN,NTOUT)
!
!  set up for recursion, by normalizing frequencies, and converting
!  phases to radians
!
      DO I=1,NTOUT
        PZ(I) = DR*PZ(I)
        PS(I) = DR*PS(I)
        PW(I) = DR*PW(I)
        F(I) = SAMP*PI*F(I)/43200.D0
        WF(I) = F(I)
      ENDDO
!
!+---------------------------------------------------------------------
!
!  Loop over times, nl output points at a time. At the start of each
!  such block, convert from amp and phase to sin and cos (hc array) at
!  the start of the block. The computation of values within each
!  block is done recursively, since the times are equi-spaced.
!
!----------------------------------------------------------------------
!
 11   IRHI = MIN(IRLI+NL-1,IRNT)
      NP = IRHI - IRLI + 1
!
! Set up harmonic coefficients, compute tide, and write out
      DO I=1,NT
        HCZ(2*I-1) = AZ(I)*DCOS(PZ(I))
        HCZ(2*I)  = -AZ(I)*DSIN(PZ(I))
        HCS(2*I-1) = AS(I)*DCOS(PS(I))
        HCS(2*I)  = -AS(I)*DSIN(PS(I))
        HCW(2*I-1) = AW(I)*DCOS(PW(I))
        HCW(2*I)  = -AW(I)*DSIN(PW(I))
      ENDDO
      CALL C_RECURS(DZ,NP,HCZ,NTOUT,WF,SCR)
      CALL C_RECURS(DS,NP,HCS,NTOUT,WF,SCR)
      CALL C_RECURS(DW,NP,HCW,NTOUT,WF,SCR)
!     WRITE(LUO,120) (DZ(I),DW(I),DS(I),I=1,NP)
 120  FORMAT('DZ,DW,DS: ',3F14.6)
!?    IF(IRHI.EQ.IRNT) STOP
!?    IRLI = IRHI + 1
!
!  Reset phases to the start of the new section
!?    DO I=1,NT
!?      PZ(I) = DMOD(PZ(I) + NP*F(I),2.D0*PI)
!?      PS(I) = DMOD(PS(I) + NP*F(I),2.D0*PI)
!?      PW(I) = DMOD(PW(I) + NP*F(I),2.D0*PI)
!?    ENDDO
!???  GO TO 11
!
!!!!   D_UEN(L,1) =  DZ
!!!!   D_UEN(L,2) = -DW
!!!!   D_UEN(L,3) = -DS
!
!!!!  ENDDO     ! End of Station Loop
!  Finished.
!
        RETURN
!       STOP
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!  ==================================
!  IERS Conventions Software License
!  ==================================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
!  WHICH APPLY TO ITS USE.
!
!  1. The Software is provided by the IERS Conventions Center ("the
!     Center").
!
!  2. Permission is granted to anyone to use the Software for any
!     purpose, including commercial applications, free of charge,
!     subject to the conditions and restrictions listed below.
!
!  3. You (the user) may adapt the Software and its algorithms for your
!     own purposes and you may distribute the resulting "derived work"
!     to others, provided that the derived work complies with the
!     following requirements:
!
!     a) Your work shall be clearly identified so that it cannot be
!        mistaken for IERS Conventions software and that it has been
!        neither distributed by nor endorsed by the Center.
!
!     b) Your work (including source code) must contain descriptions of
!        how the derived work is based upon and/or differs from the
!        original Software.
!
!     c) The name(s) of all modified routine(s) that you distribute
!        shall be changed.
! 
!     d) The origin of the IERS Conventions components of your derived
!        work must not be misrepresented; you must not claim that you
!        wrote the original Software.
!
!     e) The source code must be included for all routine(s) that you
!        distribute.  This notice must be reproduced intact in any
!        source distribution. 
!
!  4. In any published work produced by the user and which includes
!     results achieved by using the Software, you shall acknowledge
!     that the Software was used in obtaining those results.
!
!  5. The Software is provided to the user "as is" and the Center makes
!     no warranty as to its use or performance.   The Center does not
!     and cannot warrant the performance or results which the user may
!     obtain by using the Software.  The Center makes no warranties,
!     express or implied, as to non-infringement of third party rights,
!     merchantability, or fitness for any particular purpose.  In no
!     event will the Center be liable to the user for any consequential,
!     incidental, or special damages, including any lost profits or lost
!     savings, even if a Center representative has been advised of such
!     damages, or for any claim by any third party.
!
!  Correspondence concerning IERS Conventions software should be
!  addressed as follows:
!
!                     Gerard Petit
!     Internet email: gpetit[at]bipm.org
!     Postal address: IERS Conventions Center
!                     Time, frequency and gravimetry section, BIPM
!                     Pavillon de Breteuil
!                     92312 Sevres  FRANCE
!
!     or
!
!                     Brian Luzum
!     Internet email: brian.luzum[at]usno.navy.mil
!     Postal address: IERS Conventions Center
!                     Earth Orientation Department
!                     3450 Massachusetts Ave, NW
!                     Washington, DC 20392
!
      END
!
!-----------------------------------------------------------------------
      SUBROUTINE C_ADMINT(AMPIN,IDTIN,PHIN,CENT,FA2K,UTC,AMP,F,P,NIN,   &
     &           NOUT)
!
!
!  This routine is a modified version of subroutine ADMINT, taken 
!  from the International Earth Rotation and
!  Reference Systems Service (IERS) Conventions software collection.
!
!  This subroutine returns the ocean loading displacement amplitude,
!  frequency, and phase of a set of tidal constituents generated by
!  the Bos-Scherneck website at http://www.oso.chalmers.se/~loading/.
!  The variable NIN is input as the number wanted, and the variable 
!  NOUT is returned as the number provided.  The constituents used
!  are stored in the arrays IDD (Doodson number) and TAMP
!  (Cartwright-Edden amplitude).  The actual amp and phase of each
!  of these are determined by spline interpolation of the real and
!  imaginary part of the admittance, as specified at a subset of the
!  constituents.
!
!  Input:
!     AMPIN     R*4      Cartwright-Edden amplitude of tidal constituents
!     IDTIN     I*4      Doodson number of tidal constituents
!     PHIN      R*4      Phase of tidal constituents
!     NIN       I*4      Number of harmonics used
!     CENT      R*8      TT (Terrestrial Time) in fractional centuries
!                         from 2000.0.
!     FA2K(14)  R*8      The 14 fundamental arguments, consistent with
!                         IERS onventions (2010).
!     UTC       R*8      UTC time, in fractional days.
!
!  Output:  
!     AMP       R*4      Amplitude due to ocean loading
!     F         R*8      Frequency due to ocean loading
!     P         R*8      Phase due to ocean loading
!     NOUT      I*4      Number of harmonics returned
!
!  Called:
!     C_TDFRPH           Returns frequency and phase of a tidal
!                        constituent with given Doodson number            
!     C_SPLINE           Sets up array for cubic spline interpolation
!     C_EVAL             Performs cubic spline interpolation 
!     C_SHELLS           Sorts an array using Shell Sort
!
!  References:
!     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!     IERS Technical Note No. 32, BKG (2004)
!
!  Revisions:  
!  2009 June 17 B.E. Stetzler  Initial changes to header
!  2009 June 18 B.E. Stetzler  Used IMPLICIT NONE, declared more variables,
!                              and added D0 to DOUBLE PRECISION variables 
!  2009 August 19 B.E.Stetzler Capitalized all variables for FORTRAN 77
!                              compatibility
!  2012 November  D. Gordon    Converted to Fortran 90 and modified for 
!                              use in Calc 11.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER*4 I,          J,K,    LL,NCON,NDI,NIN,NLP,NOUT,NSD,NT,    &
     &          II,KK
!
!+----------------------------------------------------------------------
!  The parameters below set the number of harmonics used in the prediction
!  (nt; This must also be set in the main program) and the number of
!  constituents whose amp and phase may be specified (ncon)
!-----------------------------------------------------------------------
      PARAMETER (NT=342)
      PARAMETER (NCON=20)
!
      REAL*8    F(*),FR,P(*),PR
      REAL*8    CENT, FA2K(14), UTC
      REAL*4    DTR,C_EVAL,AM,RE,SF
!
      REAL*4    AMPIN(*),PHIN(*),AMP(*)
      INTEGER*4 IDTIN(6,*) 

!  Arrays containing information about all stored constituents
      REAL*4    TAMP(NT)
      INTEGER*4 IDD(6,NT)
!
!  Arrays containing information about the subset whose amp and phase may
!  be specified, and scratch arrays for the spline routines for which
!  at most ncon constituents may be specified.
!
      REAL*4    RL(NCON),AIM(NCON),RF(NCON),          SCR(NCON),        &
     & ZDI(NCON),ZDR(NCON),DI(NCON),DR(NCON),SDI(NCON),SDR(NCON)
      INTEGER*4 KEY(NCON)
      DATA DTR/.01745329252/
      DATA RL/NCON*0.0/,AIM/NCON*0.0/,RF/NCON*0.0/
      DATA ZDI/NCON*0.0/,ZDR/NCON*0.0/,DI/NCON*0.0/,DR/NCON*0.0/
      DATA SDI/NCON*0.0/,SDR/NCON*0.0/
      DATA TAMP/                                                        &
     &  .632208, .294107, .121046, .079915, .023818,-.023589, .022994,  &
     &  .019333,-.017871, .017192, .016018, .004671,-.004662,-.004519,  &
     &  .004470, .004467, .002589,-.002455,-.002172, .001972, .001947,  &
     &  .001914,-.001898, .001802, .001304, .001170, .001130, .001061,  &
     & -.001022,-.001017, .001014, .000901,-.000857, .000855, .000855,  &
     &  .000772, .000741, .000741,-.000721, .000698, .000658, .000654,  &
     & -.000653, .000633, .000626,-.000598, .000590, .000544, .000479,  &
     & -.000464, .000413,-.000390, .000373, .000366, .000366,-.000360,  &
     & -.000355, .000354, .000329, .000328, .000319, .000302, .000279,  &
     & -.000274,-.000272, .000248,-.000225, .000224,-.000223,-.000216,  &
     &  .000211, .000209, .000194, .000185,-.000174,-.000171, .000159,  &
     &  .000131, .000127, .000120, .000118, .000117, .000108, .000107,  &
     &  .000105,-.000102, .000102, .000099,-.000096, .000095,-.000089,  &
     & -.000085,-.000084,-.000081,-.000077,-.000072,-.000067, .000066,  &
     &  .000064, .000063, .000063, .000063, .000062, .000062,-.000060,  &
     &  .000056, .000053, .000051, .000050, .368645,-.262232,-.121995,  &
     & -.050208, .050031,-.049470, .020620, .020613, .011279,-.009530,  &
     & -.009469,-.008012, .007414,-.007300, .007227,-.007131,-.006644,  &
     &  .005249, .004137, .004087, .003944, .003943, .003420, .003418,  &
     &  .002885, .002884, .002160,-.001936, .001934,-.001798, .001690,  &
     &  .001689, .001516, .001514,-.001511, .001383, .001372, .001371,  &
     & -.001253,-.001075, .001020, .000901, .000865,-.000794, .000788,  &
     &  .000782,-.000747,-.000745, .000670,-.000603,-.000597, .000542,  &
     &  .000542,-.000541,-.000469,-.000440, .000438, .000422, .000410,  &
     & -.000374,-.000365, .000345, .000335,-.000321,-.000319, .000307,  &
     &  .000291, .000290,-.000289, .000286, .000275, .000271, .000263,  &
     & -.000245, .000225, .000225, .000221,-.000202,-.000200,-.000199,  &
     &  .000192, .000183, .000183, .000183,-.000170, .000169, .000168,  &
     &  .000162, .000149,-.000147,-.000141, .000138, .000136, .000136,  &
     &  .000127, .000127,-.000126,-.000121,-.000121, .000117,-.000116,  &
     & -.000114,-.000114,-.000114, .000114, .000113, .000109, .000108,  &
     &  .000106,-.000106,-.000106, .000105, .000104,-.000103,-.000100,  &
     & -.000100,-.000100, .000099,-.000098, .000093, .000093, .000090,  &
     & -.000088, .000083,-.000083,-.000082,-.000081,-.000079,-.000077,  &
     & -.000075,-.000075,-.000075, .000071, .000071,-.000071, .000068,  &
     &  .000068, .000065, .000065, .000064, .000064, .000064,-.000064,  &
     & -.000060, .000056, .000056, .000053, .000053, .000053,-.000053,  &
     &  .000053, .000053, .000052, .000050,-.066607,-.035184,-.030988,  &
     &  .027929,-.027616,-.012753,-.006728,-.005837,-.005286,-.004921,  &
     & -.002884,-.002583,-.002422, .002310, .002283,-.002037, .001883,  &
     & -.001811,-.001687,-.001004,-.000925,-.000844, .000766, .000766,  &
     & -.000700,-.000495,-.000492, .000491, .000483, .000437,-.000416,  &
     & -.000384, .000374,-.000312,-.000288,-.000273, .000259, .000245,  &
     & -.000232, .000229,-.000216, .000206,-.000204,-.000202, .000200,  &
     &  .000195,-.000190, .000187, .000180,-.000179, .000170, .000153,  &
     & -.000137,-.000119,-.000119,-.000112,-.000110,-.000110, .000107,  &
     & -.000095,-.000095,-.000091,-.000090,-.000081,-.000079,-.000079,  &
     &  .000077,-.000073, .000069,-.000067,-.000066, .000065, .000064,  &
     & -.000062, .000060, .000059,-.000056, .000055,-.000051/
      DATA IDD/                                                         &
     &  2, 0, 0, 0, 0, 0,   2, 2,-2, 0, 0, 0,   2,-1, 0, 1, 0, 0,       & 
     &  2, 2, 0, 0, 0, 0,   2, 2, 0, 0, 1, 0,   2, 0, 0, 0,-1, 0,       & 
     &  2,-1, 2,-1, 0, 0,   2,-2, 2, 0, 0, 0,   2, 1, 0,-1, 0, 0,       & 
     &  2, 2,-3, 0, 0, 1,   2,-2, 0, 2, 0, 0,   2,-3, 2, 1, 0, 0,       & 
     &  2, 1,-2, 1, 0, 0,   2,-1, 0, 1,-1, 0,   2, 3, 0,-1, 0, 0,       & 
     &  2, 1, 0, 1, 0, 0,   2, 2, 0, 0, 2, 0,   2, 2,-1, 0, 0,-1,       & 
     &  2, 0,-1, 0, 0, 1,   2, 1, 0, 1, 1, 0,   2, 3, 0,-1, 1, 0,       & 
     &  2, 0, 1, 0, 0,-1,   2, 0,-2, 2, 0, 0,   2,-3, 0, 3, 0, 0,       & 
     &  2,-2, 3, 0, 0,-1,   2, 4, 0, 0, 0, 0,   2,-1, 1, 1, 0,-1,       & 
     &  2,-1, 3,-1, 0,-1,   2, 2, 0, 0,-1, 0,   2,-1,-1, 1, 0, 1,       & 
     &  2, 4, 0, 0, 1, 0,   2,-3, 4,-1, 0, 0,   2,-1, 2,-1,-1, 0,       & 
     &  2, 3,-2, 1, 0, 0,   2, 1, 2,-1, 0, 0,   2,-4, 2, 2, 0, 0,       & 
     &  2, 4,-2, 0, 0, 0,   2, 0, 2, 0, 0, 0,   2,-2, 2, 0,-1, 0,       & 
     &  2, 2,-4, 0, 0, 2,   2, 2,-2, 0,-1, 0,   2, 1, 0,-1,-1, 0,       & 
     &  2,-1, 1, 0, 0, 0,   2, 2,-1, 0, 0, 1,   2, 2, 1, 0, 0,-1,       & 
     &  2,-2, 0, 2,-1, 0,   2,-2, 4,-2, 0, 0,   2, 2, 2, 0, 0, 0,       & 
     &  2,-4, 4, 0, 0, 0,   2,-1, 0,-1,-2, 0,   2, 1, 2,-1, 1, 0,       & 
     &  2,-1,-2, 3, 0, 0,   2, 3,-2, 1, 1, 0,   2, 4, 0,-2, 0, 0,       & 
     &  2, 0, 0, 2, 0, 0,   2, 0, 2,-2, 0, 0,   2, 0, 2, 0, 1, 0,       & 
     &  2,-3, 3, 1, 0,-1,   2, 0, 0, 0,-2, 0,   2, 4, 0, 0, 2, 0,       & 
     &  2, 4,-2, 0, 1, 0,   2, 0, 0, 0, 0, 2,   2, 1, 0, 1, 2, 0,       & 
     &  2, 0,-2, 0,-2, 0,   2,-2, 1, 0, 0, 1,   2,-2, 1, 2, 0,-1,       & 
     &  2,-1, 1,-1, 0, 1,   2, 5, 0,-1, 0, 0,   2, 1,-3, 1, 0, 1,       & 
     &  2,-2,-1, 2, 0, 1,   2, 3, 0,-1, 2, 0,   2, 1,-2, 1,-1, 0,       & 
     &  2, 5, 0,-1, 1, 0,   2,-4, 0, 4, 0, 0,   2,-3, 2, 1,-1, 0,       & 
     &  2,-2, 1, 1, 0, 0,   2, 4, 0,-2, 1, 0,   2, 0, 0, 2, 1, 0,       & 
     &  2,-5, 4, 1, 0, 0,   2, 0, 2, 0, 2, 0,   2,-1, 2, 1, 0, 0,       & 
     &  2, 5,-2,-1, 0, 0,   2, 1,-1, 0, 0, 0,   2, 2,-2, 0, 0, 2,       & 
     &  2,-5, 2, 3, 0, 0,   2,-1,-2, 1,-2, 0,   2,-3, 5,-1, 0,-1,       & 
     &  2,-1, 0, 0, 0, 1,   2,-2, 0, 0,-2, 0,   2, 0,-1, 1, 0, 0,       & 
     &  2,-3, 1, 1, 0, 1,   2, 3, 0,-1,-1, 0,   2, 1, 0, 1,-1, 0,       & 
     &  2,-1, 2, 1, 1, 0,   2, 0,-3, 2, 0, 1,   2, 1,-1,-1, 0, 1,       & 
     &  2,-3, 0, 3,-1, 0,   2, 0,-2, 2,-1, 0,   2,-4, 3, 2, 0,-1,       & 
     &  2,-1, 0, 1,-2, 0,   2, 5, 0,-1, 2, 0,   2,-4, 5, 0, 0,-1,       & 
     &  2,-2, 4, 0, 0,-2,   2,-1, 0, 1, 0, 2,   2,-2,-2, 4, 0, 0,       & 
     &  2, 3,-2,-1,-1, 0,   2,-2, 5,-2, 0,-1,   2, 0,-1, 0,-1, 1,       & 
     &  2, 5,-2,-1, 1, 0,   1, 1, 0, 0, 0, 0,   1,-1, 0, 0, 0, 0,       & 
     &  1, 1,-2, 0, 0, 0,   1,-2, 0, 1, 0, 0,   1, 1, 0, 0, 1, 0,       & 
     &  1,-1, 0, 0,-1, 0,   1, 2, 0,-1, 0, 0,   1, 0, 0, 1, 0, 0,       & 
     &  1, 3, 0, 0, 0, 0,   1,-2, 2,-1, 0, 0,   1,-2, 0, 1,-1, 0,       & 
     &  1,-3, 2, 0, 0, 0,   1, 0, 0,-1, 0, 0,   1, 1, 0, 0,-1, 0,       & 
     &  1, 3, 0, 0, 1, 0,   1, 1,-3, 0, 0, 1,   1,-3, 0, 2, 0, 0,       & 
     &  1, 1, 2, 0, 0, 0,   1, 0, 0, 1, 1, 0,   1, 2, 0,-1, 1, 0,       & 
     &  1, 0, 2,-1, 0, 0,   1, 2,-2, 1, 0, 0,   1, 3,-2, 0, 0, 0,       & 
     &  1,-1, 2, 0, 0, 0,   1, 1, 1, 0, 0,-1,   1, 1,-1, 0, 0, 1,       & 
     &  1, 4, 0,-1, 0, 0,   1,-4, 2, 1, 0, 0,   1, 0,-2, 1, 0, 0,       & 
     &  1,-2, 2,-1,-1, 0,   1, 3, 0,-2, 0, 0,   1,-1, 0, 2, 0, 0,       & 
     &  1,-1, 0, 0,-2, 0,   1, 3, 0, 0, 2, 0,   1,-3, 2, 0,-1, 0,       & 
     &  1, 4, 0,-1, 1, 0,   1, 0, 0,-1,-1, 0,   1, 1,-2, 0,-1, 0,       & 
     &  1,-3, 0, 2,-1, 0,   1, 1, 0, 0, 2, 0,   1, 1,-1, 0, 0,-1,       & 
     &  1,-1,-1, 0, 0, 1,   1, 0, 2,-1, 1, 0,   1,-1, 1, 0, 0,-1,       & 
     &  1,-1,-2, 2, 0, 0,   1, 2,-2, 1, 1, 0,   1,-4, 0, 3, 0, 0,       & 
     &  1,-1, 2, 0, 1, 0,   1, 3,-2, 0, 1, 0,   1, 2, 0,-1,-1, 0,       & 
     &  1, 0, 0, 1,-1, 0,   1,-2, 2, 1, 0, 0,   1, 4,-2,-1, 0, 0,       & 
     &  1,-3, 3, 0, 0,-1,   1,-2, 1, 1, 0,-1,   1,-2, 3,-1, 0,-1,       & 
     &  1, 0,-2, 1,-1, 0,   1,-2,-1, 1, 0, 1,   1, 4,-2, 1, 0, 0,       & 
     &  1,-4, 4,-1, 0, 0,   1,-4, 2, 1,-1, 0,   1, 5,-2, 0, 0, 0,       & 
     &  1, 3, 0,-2, 1, 0,   1,-5, 2, 2, 0, 0,   1, 2, 0, 1, 0, 0,       & 
     &  1, 1, 3, 0, 0,-1,   1,-2, 0, 1,-2, 0,   1, 4, 0,-1, 2, 0,       & 
     &  1, 1,-4, 0, 0, 2,   1, 5, 0,-2, 0, 0,   1,-1, 0, 2, 1, 0,       & 
     &  1,-2, 1, 0, 0, 0,   1, 4,-2, 1, 1, 0,   1,-3, 4,-2, 0, 0,       & 
     &  1,-1, 3, 0, 0,-1,   1, 3,-3, 0, 0, 1,   1, 5,-2, 0, 1, 0,       & 
     &  1, 1, 2, 0, 1, 0,   1, 2, 0, 1, 1, 0,   1,-5, 4, 0, 0, 0,       & 
     &  1,-2, 0,-1,-2, 0,   1, 5, 0,-2, 1, 0,   1, 1, 2,-2, 0, 0,       & 
     &  1, 1,-2, 2, 0, 0,   1,-2, 2, 1, 1, 0,   1, 0, 3,-1, 0,-1,       & 
     &  1, 2,-3, 1, 0, 1,   1,-2,-2, 3, 0, 0,   1,-1, 2,-2, 0, 0,       & 
     &  1,-4, 3, 1, 0,-1,   1,-4, 0, 3,-1, 0,   1,-1,-2, 2,-1, 0,       & 
     &  1,-2, 0, 3, 0, 0,   1, 4, 0,-3, 0, 0,   1, 0, 1, 1, 0,-1,       & 
     &  1, 2,-1,-1, 0, 1,   1, 2,-2, 1,-1, 0,   1, 0, 0,-1,-2, 0,       & 
     &  1, 2, 0, 1, 2, 0,   1, 2,-2,-1,-1, 0,   1, 0, 0, 1, 2, 0,       & 
     &  1, 0, 1, 0, 0, 0,   1, 2,-1, 0, 0, 0,   1, 0, 2,-1,-1, 0,       & 
     &  1,-1,-2, 0,-2, 0,   1,-3, 1, 0, 0, 1,   1, 3,-2, 0,-1, 0,       & 
     &  1,-1,-1, 0,-1, 1,   1, 4,-2,-1, 1, 0,   1, 2, 1,-1, 0,-1,       & 
     &  1, 0,-1, 1, 0, 1,   1,-2, 4,-1, 0, 0,   1, 4,-4, 1, 0, 0,       & 
     &  1,-3, 1, 2, 0,-1,   1,-3, 3, 0,-1,-1,   1, 1, 2, 0, 2, 0,       & 
     &  1, 1,-2, 0,-2, 0,   1, 3, 0, 0, 3, 0,   1,-1, 2, 0,-1, 0,       & 
     &  1,-2, 1,-1, 0, 1,   1, 0,-3, 1, 0, 1,   1,-3,-1, 2, 0, 1,       & 
     &  1, 2, 0,-1, 2, 0,   1, 6,-2,-1, 0, 0,   1, 2, 2,-1, 0, 0,       & 
     &  1,-1, 1, 0,-1,-1,   1,-2, 3,-1,-1,-1,   1,-1, 0, 0, 0, 2,       & 
     &  1,-5, 0, 4, 0, 0,   1, 1, 0, 0, 0,-2,   1,-2, 1, 1,-1,-1,       & 
     &  1, 1,-1, 0, 1, 1,   1, 1, 2, 0, 0,-2,   1,-3, 1, 1, 0, 0,       & 
     &  1,-4, 4,-1,-1, 0,   1, 1, 0,-2,-1, 0,   1,-2,-1, 1,-1, 1,       & 
     &  1,-3, 2, 2, 0, 0,   1, 5,-2,-2, 0, 0,   1, 3,-4, 2, 0, 0,       & 
     &  1, 1,-2, 0, 0, 2,   1,-1, 4,-2, 0, 0,   1, 2, 2,-1, 1, 0,       & 
     &  1,-5, 2, 2,-1, 0,   1, 1,-3, 0,-1, 1,   1, 1, 1, 0, 1,-1,       & 
     &  1, 6,-2,-1, 1, 0,   1,-2, 2,-1,-2, 0,   1, 4,-2, 1, 2, 0,       & 
     &  1,-6, 4, 1, 0, 0,   1, 5,-4, 0, 0, 0,   1,-3, 4, 0, 0, 0,       & 
     &  1, 1, 2,-2, 1, 0,   1,-2, 1, 0,-1, 0,   0, 2, 0, 0, 0, 0,       & 
     &  0, 1, 0,-1, 0, 0,   0, 0, 2, 0, 0, 0,   0, 0, 0, 0, 1, 0,       & 
     &  0, 2, 0, 0, 1, 0,   0, 3, 0,-1, 0, 0,   0, 1,-2, 1, 0, 0,       & 
     &  0, 2,-2, 0, 0, 0,   0, 3, 0,-1, 1, 0,   0, 0, 1, 0, 0,-1,       & 
     &  0, 2, 0,-2, 0, 0,   0, 2, 0, 0, 2, 0,   0, 3,-2, 1, 0, 0,       & 
     &  0, 1, 0,-1,-1, 0,   0, 1, 0,-1, 1, 0,   0, 4,-2, 0, 0, 0,       & 
     &  0, 1, 0, 1, 0, 0,   0, 0, 3, 0, 0,-1,   0, 4, 0,-2, 0, 0,       & 
     &  0, 3,-2, 1, 1, 0,   0, 3,-2,-1, 0, 0,   0, 4,-2, 0, 1, 0,       & 
     &  0, 0, 2, 0, 1, 0,   0, 1, 0, 1, 1, 0,   0, 4, 0,-2, 1, 0,       & 
     &  0, 3, 0,-1, 2, 0,   0, 5,-2,-1, 0, 0,   0, 1, 2,-1, 0, 0,       & 
     &  0, 1,-2, 1,-1, 0,   0, 1,-2, 1, 1, 0,   0, 2,-2, 0,-1, 0,       & 
     &  0, 2,-3, 0, 0, 1,   0, 2,-2, 0, 1, 0,   0, 0, 2,-2, 0, 0,       & 
     &  0, 1,-3, 1, 0, 1,   0, 0, 0, 0, 2, 0,   0, 0, 1, 0, 0, 1,       & 
     &  0, 1, 2,-1, 1, 0,   0, 3, 0,-3, 0, 0,   0, 2, 1, 0, 0,-1,       & 
     &  0, 1,-1,-1, 0, 1,   0, 1, 0, 1, 2, 0,   0, 5,-2,-1, 1, 0,       & 
     &  0, 2,-1, 0, 0, 1,   0, 2, 2,-2, 0, 0,   0, 1,-1, 0, 0, 0,       & 
     &  0, 5, 0,-3, 0, 0,   0, 2, 0,-2, 1, 0,   0, 1, 1,-1, 0,-1,       & 
     &  0, 3,-4, 1, 0, 0,   0, 0, 2, 0, 2, 0,   0, 2, 0,-2,-1, 0,       & 
     &  0, 4,-3, 0, 0, 1,   0, 3,-1,-1, 0, 1,   0, 0, 2, 0, 0,-2,       & 
     &  0, 3,-3, 1, 0, 1,   0, 2,-4, 2, 0, 0,   0, 4,-2,-2, 0, 0,       & 
     &  0, 3, 1,-1, 0,-1,   0, 5,-4, 1, 0, 0,   0, 3,-2,-1,-1, 0,       & 
     &  0, 3,-2, 1, 2, 0,   0, 4,-4, 0, 0, 0,   0, 6,-2,-2, 0, 0,       & 
     &  0, 5, 0,-3, 1, 0,   0, 4,-2, 0, 2, 0,   0, 2, 2,-2, 1, 0,       & 
     &  0, 0, 4, 0, 0,-2,   0, 3,-1, 0, 0, 0,   0, 3,-3,-1, 0, 1,       & 
     &  0, 4, 0,-2, 2, 0,   0, 1,-2,-1,-1, 0,   0, 2,-1, 0, 0,-1,       & 
     &  0, 4,-4, 2, 0, 0,   0, 2, 1, 0, 1,-1,   0, 3,-2,-1, 1, 0,       & 
     &  0, 4,-3, 0, 1, 1,   0, 2, 0, 0, 3, 0,   0, 6,-4, 0, 0, 0/
!
!  Initialize variables.
      K   = 0
      NLP = 0
      NDI = 0
      NSD = 0
!
      DO LL=1,NIN
!  See if Doodson numbers match
         DO KK=1,NT
            II = 0
            DO I=1,6
               II = II + IABS(IDD(I,KK)-IDTIN(I,LL))
            ENDDO
            IF(II.EQ.0) GO TO 5
         ENDDO
!  If you have a match, put line into array
 5       IF(II.EQ.0.AND.K.LT.NCON) THEN
            K = K + 1
            RL(K) = AMPIN(LL)*COS(DTR*PHIN(LL))/ABS(TAMP(KK))
            AIM(K)= AMPIN(LL)*SIN(DTR*PHIN(LL))/ABS(TAMP(KK))
!+---------------------------------------------------------------------
!  Now have real and imaginary parts of admittance, scaled by Cartwright-
!  Edden amplitude. Admittance phase is whatever was used in the original
!  expression. (Usually phase is given relative to some reference,
!  but amplitude is in absolute units). Next get frequency.
!----------------------------------------------------------------------
            CALL C_TDFRPH(IDD(1,KK),CENT,FA2K,UTC,FR,PR)
            RF(K) = FR
         ENDIF
      ENDDO
!+---------------------------------------------------------------------
!  Done going through constituents; there are k of them.
!  Have specified admittance at a number of points. Sort these by frequency
!  and separate diurnal and semidiurnal, recopying admittances to get them
!  in order using Shell Sort.
!----------------------------------------------------------------------
!
      CALL C_SHELLS(RF,KEY,K)
      DO I=1,K
         IF(RF(I).LT.0.5) NLP = NLP + 1
         IF(RF(I).LT.1.5.AND.RF(I).GT.0.5) NDI = NDI + 1
         IF(RF(I).LT.2.5.AND.RF(I).GT.1.5) NSD = NSD + 1
         SCR(I) = RL(KEY(I))
      ENDDO
      DO I=1,K
         RL(I) = SCR(I)
         SCR(I) = AIM(KEY(I))
      ENDDO
      DO I=1,K
         AIM(I) = SCR(I)
      ENDDO
!+---------------------------------------------------------------------
!  now set up splines (8 cases - four species, each real and imaginary)
!  We have to allow for the case when there are no constituent amplitudes
!  for the long-period tides.
!----------------------------------------------------------------------
      IF(NLP.NE.0) CALL C_SPLINE(NLP,RF,RL,ZDR,SCR)
      IF(NLP.NE.0) CALL C_SPLINE(NLP,RF,AIM,ZDI,SCR)
      CALL C_SPLINE(NDI,RF(NLP+1),RL(NLP+1),DR,SCR)
      CALL C_SPLINE(NDI,RF(NLP+1),AIM(NLP+1),DI,SCR)
      CALL C_SPLINE(NSD,RF(NLP+NDI+1),RL(NLP+NDI+1),SDR,SCR)
      CALL C_SPLINE(NSD,RF(NLP+NDI+1),AIM(NLP+NDI+1),SDI,SCR)
!  Evaluate all harmonics using the interpolated admittance
      J = 1
      DO I=1,NT
         IF(IDD(1,I).EQ.0.AND.NLP.EQ.0) GO TO 11
         CALL C_TDFRPH(IDD(1,I),CENT,FA2K,UTC,F(J),P(J))
!  Compute phase corrections to equilibrium tide using function C_EVAL
         IF(IDD(1,I).EQ.0) P(J) = P(J) + 180.
         IF(IDD(1,I).EQ.1) P(J) = P(J) + 90.
         SF = F(J)
         IF(IDD(1,I).EQ.0) RE = C_EVAL(SF,NLP,RF,RL,ZDR)
         IF(IDD(1,I).EQ.0) AM = C_EVAL(SF,NLP,RF,AIM,ZDI)
         IF(IDD(1,I).EQ.1) RE = C_EVAL(SF,NDI,RF(NLP+1),RL(NLP+1),DR)
         IF(IDD(1,I).EQ.1) AM = C_EVAL(SF,NDI,RF(NLP+1),AIM(NLP+1),DI)
         IF(IDD(1,I).EQ.2) RE =                                         &
     &      C_EVAL(SF,NSD,RF(NLP+NDI+1),RL(NLP+NDI+1),SDR)
         IF(IDD(1,I).EQ.2) AM =                                         &
     &      C_EVAL(SF,NSD,RF(NLP+NDI+1),AIM(NLP+NDI+1),SDI)
         AMP(J) = TAMP(I)*SQRT(RE**2+AM**2)
         P(J) = P(J) + ATAN2(AM,RE)/DTR
         IF(P(J).GT.180) P(J)=P(J)-360.
         J = J + 1
 11      CONTINUE
      ENDDO
      NOUT = J - 1
      RETURN
!
      END
!
!  Finished.
!
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!-----------------------------------------------------------------------
      SUBROUTINE C_TDFRPH (IDOOD,CENT,FA2K,UTC,FREQ,PHASE)
!+
!  - - - - - - - - - - -
!   T D F R P H 
!  - - - - - - - - - - -
!
!  This routine is part of the International Earth Rotation and
!  Reference Systems Service (IERS) Conventions software collection.
!
!  This subroutine returns the frequency and phase of a tidal
!  constituent when its Doodson number is given as input. 
!
!  Given:
!     idood       i      Doodson number of a tidal constituent
!     CENT
!     UTC
!     FA2k(14)
!
!  Returned:
!     freq        d      Frequency of a tidal constituent
!     phase       d      Phase of a tidal constituent (Note 1)
!
!  Notes:
!
!  1) The phases must be decreased by 90 degrees if the sum of the order 
!     and the species number is odd (as for the 2nd degree diurnals, and 
!     3rd degree low frequency and semidiurnals).
!     
!     These phases may need further adjustment to allow for the spherical
!     harmonic normalization used; e.g. for that used for the potential
!     by Cartwright and Tayler, 180 degrees must be added for (species,
!     order) = (1,2), (1,3), or (3,3). 
!
!
!  Test case:
!     given input: For June 25, 2009 0 Hr 0 Min, M2 tide
!                  DATA IDOOD = / 2, 0, 0, 0, 0, 0 /  
!
!     expected output: FREQ = 1.93227361605688D0
!                      PHASE = 132.8193176853237674D0
!
!  References:
!
!     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
!     IERS Technical Note No. 36, BKG (2010)
!
!  Revisions:  
!  2009 June   15 B.E.Stetzler  Initial changes to code 
!  2009 August 19 B.E.Stetzler  Capitalized all variables for FORTRAN
!                               77 compatibility
!  2010 March  19 B.E.Stetzler  Provided test case
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
      SAVE   D,DD,OLDCENT,T,DAYFR,F1,F2,F3,F4,F5,FD1,FD2,FD3,FD4,FD5
      INTEGER*4 I,INITIAL,ITM(5),JD,JULDAT,LEAP
      REAL*8 YEAR,DELTA,FREQ,PHASE,D(6),DAYFR,DD(6),DJD,F1,F2,F3,F4,F5, &
     &       FD1,FD2,FD3,FD4,FD5,T, CENT, FA2K(14), DTR, OLDCENT, UTC,  &
     &       DelC
      INTEGER*4 IDOOD(6),ITM2(6),ITMSAVE(5)
      DATA DTR/.01745329252/
      DATA OLDCENT/-1.0000000D0/
!
! Common block 'date' stores time information in Universal Time (UT)
!
!------------------------------------------------------------------------
!  Test to see if time has changed; if so, set the phases and frequencies
!  for each of the Doodson arguments
!------------------------------------------------------------------------
      INITIAL=0
!
      DelC = DABS(OLDCENT - CENT)
      IF (DelC .lt. 1.D-13) INITIAL=1
!
      IF (INITIAL .eq. 0) THEN      
!
! Convert times to Julian days (UT) then to Julian centuries from J2000.0
!   (ET)
!
!       CALL TOYMD(ITM,ITM2)
!       JD = JULDAT(ITM2)
!       DAYFR=  ITM(3)/24.0D0 + ITM(4)/1440.0D0 + ITM(5)/84600.0D0
!       YEAR=ITM(1)+(ITM(2)+DAYFR)/(365.0D0+LEAP(ITM(1)))
!       CALL ETUTC(YEAR,DELTA)
!       DJD= JD - 0.5D0 + DAYFR
!       T = (DJD - 2451545.0D0 + DELTA/86400.0D0)/36525.0D0
!
        T = CENT
        DAYFR = UTC
!
!
! IERS expressions for the Delaunay arguments, in degrees
!
!       F1 =     134.9634025100D0 +                                     &
!    &    T*( 477198.8675605000D0 +                                     &
!    &    T*(      0.0088553333D0 +                                     &
!    &    T*(      0.0000143431D0 +                                     &
!    &    T*(     -0.0000000680D0 ))))
!       F2 =     357.5291091806D0 +                                     &
!    &    T*(  35999.0502911389D0 +                                     &
!    &    T*(     -0.0001536667D0 +                                     &
!    &    T*(      0.0000000378D0 +                                     &
!    &    T*(     -0.0000000032D0 ))))
!       F3 =      93.2720906200D0 +                                     &
!    &    T*( 483202.0174577222D0 +                                     &
!    &    T*(     -0.0035420000D0 +                                     &
!    &    T*(     -0.0000002881D0 +                                     &
!    &    T*(      0.0000000012D0 ))))
!       F4 =     297.8501954694D0 +                                     &
!    &    T*( 445267.1114469445D0 +                                     &
!    &    T*(     -0.0017696111D0 +                                     &
!    &    T*(      0.0000018314D0 +                                     &
!    &    T*(     -0.0000000088D0 ))))
!       F5 =     125.0445550100D0 +                                     &
!    &    T*(  -1934.1362619722D0 +                                     &
!    &    T*(      0.0020756111D0 +                                     &
!    &    T*(      0.0000021394D0 +                                     &
!    &    T*(     -0.0000000165D0 ))))
!
!      write(6,1007) F1,F2,F3,F4,F5
 1007  format(' tdfrph.f: Original F1,F2,F3,F4,F5:',5F16.4)
       F1 = FA2K(1)/DTR
       F2 = FA2K(2)/DTR
       F3 = FA2K(3)/DTR
       F4 = FA2K(4)/DTR
       F5 = FA2K(5)/DTR
!      write(6,1008) F1,F2,F3,F4,F5
!1008  format('tdfrph.f: F1,F2,F3,F4,F5 from FA2K:',5F16.4)
!      write(6,1009) T, DAYFR
!1009  format('tdfrph.f: T,DAYFR:',2F20.12)
!
!  Convert to Doodson (Darwin) variables
!
        D(1) = 360.0D0*DAYFR - F4
        D(2) = F3 + F5
        D(3) = D(2) - F4
        D(4) = D(2) - F1
        D(5) = -F5
        D(6) = D(3) - F2
!
!       Write(6,1010) D
 1010   Format(' D= ',6F18.6)

!  Find frequencies of Delauney variables (in cycles/day), and from these
!  the same for the Doodson arguments
!
        FD1 =  0.0362916471D0 + 0.0000000013D0*T
        FD2 =  0.0027377786D0
        FD3 =  0.0367481951D0 - 0.0000000005D0*T
        FD4 =  0.0338631920D0 - 0.0000000003D0*T
        FD5 = -0.0001470938D0 + 0.0000000003D0*T
        DD(1) = 1.0D0 - FD4
        DD(2) = FD3 + FD5
        DD(3) = DD(2) - FD4
        DD(4) = DD(2) - FD1
        DD(5) = -FD5
        DD(6) = DD(3) - FD2
!
!       Write(6,1011) DD
 1011   Format(' DD= ',6F20.12)
!
      ENDIF
!
!  End of intialization (likely to be called only once)
!
!  Compute phase and frequency of the given tidal constituent
!
      FREQ=0.0D0
      PHASE=0.0D0
      DO I = 1,6
         FREQ =  FREQ + IDOOD(I)*DD(I)
         PHASE = PHASE + IDOOD(I)*D(I)
      ENDDO
!
! Adjust phases so that they fall in the positive range 0 to 360
      PHASE = DMOD(PHASE,360.0D0)
      IF(PHASE.LT.0.0D0) PHASE = PHASE + 360.0D0
!
       OLDCENT = CENT
!
      RETURN
!
      END
!
!  Finished.
!
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!-----------------------------------------------------------------------
!
      SUBROUTINE C_SHELLS (X,K,N)
!+
!  - - - - - - - - -
!   S H E L L S
!  - - - - - - - - -
!
!  This routine is part of the International Earth Rotation and
!  Reference Systems Service (IERS) Conventions software collection.
!
!  The subroutine sorts an array x, of length n, sorting upward,
!  and returns an array k which may be used to key another array
!  to the sorted pattern (i.e., if we had an array f to which x
!  corresponded before sorting, then after calling SHELLS,
!  f(k(1)) will be the element of f corresponding to the
!  smallest x, f(k(2)) the next smallest, and so on).
!
!
!  Given:
!     x              d      array to be sorted (Note 1)
!     n              i      length of the input array x
!
!  Returned:
!     k              i      sorted array that may be used to key another 
!                           array
!  Notes:
!
!  1) See the subroutine ADMINT.F header comments for detailed information.
! 
!  Called:
!     None
!
!  Test case:
!     Not provided for this subroutine.
!
!  References:
!
!     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
!     IERS Technical Note No. 36, BKG (2010)
!
!  Revisions:
!  1982 December 29              Revised so that array k is sorted in turn
!                                
!  2009 June   05 B.E. Stetzler    Added header and copyright
!  2009 August 19 B.E. Stetzler    Capitalized all variables for FORTRAN
!                                  77 compatibility
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
      INTEGER*4 I,IGAP,IEX,IK,IMAX,IPL,J,K(N),L,N
      REAL*4 SV,X(N)
!
      IGAP = N
!
      DO 1 I = 1,N
 1    K(I) = I
 5    IF(IGAP.LE.1) GO TO 25
!
      IGAP = IGAP/2
      IMAX = N - IGAP
 10   IEX = 0
      DO 20 I = 1,IMAX
      IPL = I + IGAP
      IF(X(I).LE.X(IPL)) GO TO 20
      SV = X(I)
      IK = K(I)
      X(I) = X(IPL)
      K(I) = K(IPL)
      X(IPL) = SV
      K(IPL) = IK
      IEX = IEX + 1
 20   CONTINUE
!
      IF(IEX.GT.0) GO TO 10
      GO TO 5
!
!  Now sort k's (for identical values of x, if any)
!
 25   J = 1
 30   IF(J.GE.N) RETURN
      IF(X(J).EQ.X(J+1)) GO TO 33
      J = J + 1
      GO TO 30
!  Have at least two x's with the same value. See how long this is true
 33   L = J
 35   IF(X(L).NE.X(L+1)) GO TO 38
      L = L + 1
      IF(L.LT.N) GO TO 35
!  j and l are the indices within which x(i) does not change - sort k
 38   IGAP = L - J + 1
 40   IF(IGAP.LE.1) J = L + 1
      IF(IGAP.LE.1) GO TO 30
!
      IGAP = IGAP/2
      IMAX = L-J+1 - IGAP
 45   IEX = 0
!
      DO 50 I=1,IMAX
      IPL = I + IGAP + J - 1
      IF(K(I+J-1).LE.K(IPL)) GO TO 50
      IK = K(I+J-1)
      K(I+J-1) = K(IPL)
      K(IPL) = IK
      IEX = IEX + 1
 50   CONTINUE
      IF(IEX.GT.0) GO TO 45
      GO TO 40
!
      END
!
! Finished.
!
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!
!-----------------------------------------------------------------------
      SUBROUTINE C_SPLINE (NN,X,U,S,A)
!+
!  - - - - - - - - -
!   S P L I N E
!  - - - - - - - - -
!
!  This routine is part of the International Earth Rotation and
!  Reference Systems Service (IERS) Conventions software collection.
!
!  The purpose of the subroutine is to find an array s for the spline
!  interpolator function C_EVAL.
!
!
!  Given: This is a support routine of the main program HARDISP.F.
!     nn             i      number of data points supplied, which may be
!                           negative (Note 1)
!     x              d      array containing x-coordinates where function
!                           is sampled (Note 2)
!     u              d      array containing sample values that are to be
!                           interpolated 
!     a              d      working space array of dimension at least nn
!
!  Returned:
!     s              d      output array of 2nd derivative at sample points 
!
!  Notes:
!
!  1) If the user wishes to force the derivatives at the ends of the series
!     to assume specified values, he or she should put du(1)/dx and du(n)/dx
!     in the variables s1 and s2 and call the subroutine with nn = -(number
!     of terms in the series).  Normally a parabola is fitted through the 
!     1st and last 3 points to find the slopes.  If less than 4 points are
!     supplied, straight lines are fitted.
! 
!  2) The sequence xx(1), xx(2), ... xx(nn) must be strictly increasing.
!
!  Called:
!     None
!
!  Test case:
!     Not provided for this subroutine.
!
!  References:
!
!     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
!     IERS Technical Note No. 36, BKG (2010)
!
!  Revisions:
!  2009 June   08 B.E.Stetzler    Added header and copyright
!  2009 August 19 B.E.Stetzler    Capitalized all variables for FORTRAN
!                                 77 compatibility
!  2009 August 26 B.E.Stetzler    Used IMPLICIT NONE and defined all variables
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER*4 I,J,N,N1,NN,NMAX
      PARAMETER (NMAX = 20)
      REAL*4 C,Q,Q1,QN,U1,U2,X1,X2
      REAL*4 X(NMAX),U(NMAX),S(NMAX),A(NMAX)
!
      Q(U1,X1,U2,X2)=(U1/X1**2-U2/X2**2)/(1.0/X1-1.0/X2)
!      
!
      N = IABS(NN)
!
      IF (N.LE.3) THEN
!
!  series too short for cubic spline - use straight lines.
         DO I=1,N
            S(I)=0.0
         ENDDO
         RETURN
      ENDIF
!
      Q1=Q(U(2)-U(1),X(2)-X(1),U(3)-U(1),X(3)-X(1))
      QN=Q(U(N-1)-U(N),X(N-1)-X(N),U(N-2)-U(N),X(N-2)-X(N))
!
      IF (NN.LE.0) THEN
         Q1=S(1)
         QN=S(2)
      ENDIF
!
      S(1)=6.0*((U(2)-U(1))/(X(2)-X(1)) - Q1)
      N1= N - 1
!
      DO I=2,N1
         S(I)= (U(I-1)/(X(I)-X(I-1)) - U(I)*(1.0/(X(I)-X(I-1))          &
     &   + 1.0/(X(I+1)-X(I))) + U(I+1)/(X(I+1)-X(I)))*6.0
      ENDDO
!
      S(N)=6.0*(QN + (U(N1)-U(N))/(X(N)-X(N1)))
      A(1)=2.0*(X(2)-X(1))
      A(2)=1.5*(X(2)-X(1)) + 2.0*(X(3)-X(2))
      S(2)=S(2) - 0.5*S(1)

      DO I=3,N1
         C=(X(I)-X(I-1))/A(I-1)
         A(I)=2.0*(X(I+1)-X(I-1)) - C*(X(I)-X(I-1))
         S(I)=S(I) - C*S(I-1)
      ENDDO
!
      C=(X(N)-X(N1))/A(N1)
      A(N)=(2.0-C)*(X(N)-X(N1))
      S(N)=S(N) - C*S(N1)
!
!  Back substitute
      S(N)= S(N)/A(N)
!
      DO J=1,N1
         I=N-J
         S(I) =(S(I) - (X(I+1)-X(I))*S(I+1))/A(I)
      ENDDO
      RETURN
!
      END
!
! Finished.
!
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!
!-----------------------------------------------------------------------
      REAL FUNCTION C_EVAL (Y,NN,X,U,S)
!+
!  - - - - - - - - - - -
!   E V A L 
!  - - - - - - - - - - -
!
!  This routine is part of the International Earth Rotation and
!  Reference Systems Service (IERS) Conventions software collection.
!
!  This function performs cubic spline interpolation of a given function
!  sampled at unequally spaced intervals.  The subroutine HSPLIN needs
!  to be called beforehand to set up the array s.
! 
!
!  Given:
!     y            d     the coordinate at which a function value is
!                        desired (Note 1)
!     nn           i     number of samples of the original function
!     x            d     array containing sample coordinates x(1),x(2),...
!                        x(nn) (Note 2)
!     s            d     array containing the 2nd derivatives at the sample
!                        points (Note 3)
!
!  Returned:
!     u            d     array containing samples of a function at the
!                        coordinates x(1),x(2),...x(nn)
!
!  Notes:
!
!  1) If y falls outside the range (x(1),x(nn)), the value at the nearest
!     endpoint of the series is used.
!
!  2) The sequence x(1),x(2),...x(nn) must be strictly increasing.
!
!  3) This array is found by the subroutine HSPLIN, which must be called
!     once before beginning this interpolation.
!
!  Called:
!     None
!
!  Test case:
!     
!  Not provided for this function.  This is a support routine of the main
!  program HARDISP.F.
!
!  References:
!
!     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
!     IERS Technical Note No. 36, BKG (2010)
!
!  Revisions:
!  2009 June   03 B.E.Stetzler Initial standardization of function
!  2009 August 19 B.E.Stetzler Capitalized all variables for FORTRAN 77
!                               compatibility
!-----------------------------------------------------------------------
!      
      IMPLICIT NONE
      INTEGER K,K1,K2,NN
      REAL*4 DELI,DK,DY,DY1,F1,F2,F3,FF1,FF2,Y
      REAL*4 X(*),U(*),S(*)
!
      NN = IABS(NN)
!
!     If y is out of range, substitute endpoint values
!
      IF (Y.LE.X(1)) THEN
         C_EVAL=U(1)
         RETURN
      ENDIF
!
      IF (Y.GE.X(NN)) THEN
         C_EVAL=U(NN)
         RETURN
      ENDIF
!
!    Locate interval (x(k1),x(k2)) which contains y
      DO 100 K=2,NN
         IF(X(K-1).LT.Y.AND.X(K).GE.Y) THEN
           K1=K-1
           K2=K
         ENDIF
100   CONTINUE
!
!    Evaluate and then interpolate
      DY=X(K2)-Y
      DY1=Y-X(K1)
      DK=X(K2)-X(K1)
      DELI=1.0D0/(6.0D0*DK)
      FF1=S(K1)*DY*DY*DY
      FF2=S(K2)*DY1*DY1*DY1
      F1=(FF1+FF2)*DELI
      F2=DY1*((U(K2)/DK)-(S(K2)*DK)/6.0D0)
      F3= DY*((U(K1)/DK)-(S(K1)*DK)/6.0D0)
      C_EVAL=F1+F2+F3
      RETURN
!
      END
!
! Finished.
!  
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!
!-----------------------------------------------------------------------
      SUBROUTINE C_RECURS(X,N,HC,NF,OM,SCR)
!+
!  - - - - - - - - -
!   R E C U R S
!  - - - - - - - - -
!
!  This routine is part of the International Earth Rotation and
!  Reference Systems Service (IERS) Conventions software collection.
!
!  The purpose of the subroutine is to perform sine and cosine recursion
!  to fill in data x, of length n, for nf sines and cosines with frequencies
!  om. 
!
!  In general, Class 1, 2, and 3 models represent physical effects that
!  act on geodetic parameters while canonical models provide lower-level
!  representations or basic computations that are used by Class 1, 2, or
!  3 models.
! 
!  Status: Canonical model	
! 
!     Class 1 models are those recommended to be used a priori in the
!     reduction of raw space geodetic data in order to determine
!     geodetic parameter estimates.
!     Class 2 models are those that eliminate an observational
!     singularity and are purely conventional in nature.
!     Class 3 models are those that are not required as either Class
!     1 or 2.
!     Canonical models are accepted as is and cannot be classified as a
!     Class 1, 2, or 3 model.
!
!  Given: This is a support routine of the main program HARDISP.F.
!     x              d      data provided from a file given as standard
!                           input from the MAIN program HARDISP.F (Note 1)
!     n              i      length of the data file x
!     hc             d      array containing alternating cosine and sine
!                           coefficients
!     nf             i      number of sine and cosine terms
!     om             d      sine and cosine frequencies (Note 2)  
!
!  Returned:
!     scr            d      scratch array of length 3 times nf which is
!                           returned as the recursion cr
!  Notes:
!
!  1) See the MAIN program HARDISP.F header comments for detailed information.
! 
!  2) The frequencies are normalized so that the Nyquist frequency is pi.
!
!  Called:
!     None
!
!  Test case:
!     Not provided for this subroutine.
!
!  References:
!
!     Petit, G. and Luzum, B. (eds.), IERS Conventions (2010),
!     IERS Technical Note No. 36, BKG (2010)
!
!  Revisions:
!  2009 June 05 B.E. Stetzler    Added header and copyright, used DCOS
!                                and DSIN exclusively, and replaced END 
!                                DO statements with CONTINUE statements
!  2009 August 19 B.E. Stetzler  Capitalized all variables for FORTRAN
!                                77 compatibility
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER*4 I,J,N,NF
      REAL*8 SC,SCR(*)
      REAL*4 X(*),HC(*),OM(*)
!
!  Set up for start of recursion by computing harmonic values
!  at starting point and just before it
!
      DO I = 1,NF
         SCR(3*I-2) = HC(2*I-1)
         SCR(3*I-1) = HC(2*I-1)*COS(OM(I)) -HC(2*I)*SIN(OM(I))
         SCR(3*I) = 2.*DCOS(DBLE(OM(I)))
      ENDDO
!
!  Do recursion over data
      DO I = 1,N
         X(I) = 0.
!  Then do recursive computation for each harmonic
         DO J  = 1,NF
            X(I) = X(I) + SCR(3*J-2)
            SC = SCR(3*J-2)
            SCR(3*J-2) = SCR(3*J)*SC-SCR(3*J-1)
            SCR(3*J-1) = SC
         ENDDO
      ENDDO
      RETURN
!
! Finished.
!
!+----------------------------------------------------------------------
!
!  Copyright (C) 2008
!  IERS Conventions Center
!
!  ==================================
!  IERS Conventions Software License
!  ==================================
!
!  NOTICE TO USER:
!
!  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
!  WHICH APPLY TO ITS USE.
!
!  1. The Software is provided by the IERS Conventions Center ("the
!     Center").
!
!  2. Permission is granted to anyone to use the Software for any
!     purpose, including commercial applications, free of charge,
!     subject to the conditions and restrictions listed below.
!
!  3. You (the user) may adapt the Software and its algorithms for your
!     own purposes and you may distribute the resulting "derived work"
!     to others, provided that the derived work complies with the
!     following requirements:
!
!     a) Your work shall be clearly identified so that it cannot be
!        mistaken for IERS Conventions software and that it has been
!        neither distributed by nor endorsed by the Center.
!
!     b) Your work (including source code) must contain descriptions of
!        how the derived work is based upon and/or differs from the
!        original Software.
!
!     c) The name(s) of all modified routine(s) that you distribute
!        shall be changed.
! 
!     d) The origin of the IERS Conventions components of your derived
!        work must not be misrepresented; you must not claim that you
!        wrote the original Software.
!
!     e) The source code must be included for all routine(s) that you
!        distribute.  This notice must be reproduced intact in any
!        source distribution. 
!
!  4. In any published work produced by the user and which includes
!     results achieved by using the Software, you shall acknowledge
!     that the Software was used in obtaining those results.
!
!  5. The Software is provided to the user "as is" and the Center makes
!     no warranty as to its use or performance.   The Center does not
!     and cannot warrant the performance or results which the user may
!     obtain by using the Software.  The Center makes no warranties,
!     express or implied, as to non-infringement of third party rights,
!     merchantability, or fitness for any particular purpose.  In no
!     event will the Center be liable to the user for any consequential,
!     incidental, or special damages, including any lost profits or lost
!     savings, even if a Center representative has been advised of such
!     damages, or for any claim by any third party.
!
!  Correspondence concerning IERS Conventions software should be
!  addressed as follows:
!
!                     Gerard Petit
!     Internet email: gpetit[at]bipm.org
!     Postal address: IERS Conventions Center
!                     Time, frequency and gravimetry section, BIPM
!                     Pavillon de Breteuil
!                     92312 Sevres  FRANCE
!
!     or
!
!                     Brian Luzum
!     Internet email: brian.luzum[at]usno.navy.mil
!     Postal address: IERS Conventions Center
!                     Earth Orientation Department
!                     3450 Massachusetts Ave, NW
!                     Washington, DC 20392
!
!
!-----------------------------------------------------------------------
      END

