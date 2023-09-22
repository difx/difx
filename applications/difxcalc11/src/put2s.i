!
!  Include file put2s.i
!
!   Include to hold all the database PUT's to the type 2 database records.
!    PUT's from the  'geometry' subroutines are are now moved into 
!     subroutine PUT_G using common block 'GPUTS'. 2012 Dec., D. Gordon 
!    PUT's from the  'partials' subroutines are are now moved into 
!     subroutine PUT_P using common block 'PPUTS'. 2013 Jan., D. Gordon 
!    PUT's from the  'contributions' subroutines are are now moved into 
!     subroutine PUT_C using common block 'CPUTS'. 2013 Jan., D. Gordon 
!
      Real*8    TDBgeo, EARTH1(3,3), SUN1(3,2), XMOON1(3,2), ATMUT_1,   &
     &          NUT6XYS(3,2),  WOBIN(2), R2Kput(3,3,3), XHOLD(3,2,2),   &
     &          U_V(2), Wb, AZ(2,2), ELEV(2,2), PANGL(2), FCONT(2),     &
     &          NUT2006(2,2), NUTWAHR(2,2)
!
      COMMON / GPUTS / TDBgeo, EARTH1, SUN1, XMOON1, ATMUT_1, NUT6XYS,  &
     &                 WOBIN, R2Kput, XHOLD, U_V, AZ, Wb, ELEV, PANGL,  &
     &                 FCONT, NUT2006, NUTWAHR
!
!
      Real*8  DNUXY(2,2), DSITP(3,2,2), DSTRP(2,2), DPLXP(2),           &
     &        DUT1P(2,2)
!
      COMMON / PPUTS / DNUXY, DSITP, DSTRP, DPLXP, DUT1P
!
!
      Real*8  DAXOC(2,2), Tcorrmv(2), DETDC(2), DPTDC(2),               &
     &        PTOLD(2), DOPTLC(2), DOCEC(2), CONTRIB_HOR(2,2),          &
     &        CONTRIB_VER(2,2), PMCONT(2), UT1tid(2), UT1lib(2),        &
     &        DWOBXC(2), DWOBYC(2), DWOBlib(2), DWOBorth(2), PLXCON(2), &
     &        Datmc_hmf(2,2), Datmc_wmf(2,2), DOCECold(2)
!
      COMMON / CPUTS / DAXOC, Tcorrmv, DETDC, DPTDC, PTOLD, DOPTLC,     &
     &                 DOCEC, CONTRIB_HOR, CONTRIB_VER, PMCONT,         &
     &                 UT1tid, UT1lib, DWOBXC, DWOBYC, DWOBlib,         &
     &                 DWOBorth, PLXCON, Datmc_hmf, Datmc_wmf, DOCECold
!
!
      Real*8  CONDEL(2), CONRAT, CON_CNTRB(2), CON_PART(2),             &
     &        SUN_CNTRB(2), BEND_PAR(2), SUNPLUS(2) 
!
      COMMON /THPUTS / CONDEL, CONRAT, CON_CNTRB, CON_PART,             &
     &        SUN_CNTRB, BEND_PAR, SUNPLUS
