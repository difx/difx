      SUBROUTINE d_out1(lu_out,fd_out,Kjob)
      IMPLICIT None
!
      Integer*4 I, J, K, I1, I6, UTCsec, ierr, lu_out, npoly, intrval,  &
     &          MJD, Isec, Numpoly, get4unit, N, M, II, L, ISRC, Kjob
!     Integer*4 Numtel,          Polynum, K1
      Integer*4 fitPoly, c_out1, c_out2, c_out3, ierr1, ierr2, ierr3
      Integer*4 fd_out
      Integer*4 calcVersion
      Integer*4 difxiowriteint0
      Integer*4 difxiowriteint1
      Integer*4 difxiowriteint2
      Integer*4 difxiowritestring0
      Integer*4 difxiowritestring1
      Integer*4 difxiowritestring2
      Integer*4 difxiowritepoly26
      Character*9 Site_c(36)
!     SAVE      lu_out, fd_out
!
      INCLUDE 'cmxst11.i'
!            Variables 'from':
!              1. NUMSIT - The total number of sites in the current job.

       INCLUDE 'd_input.i'
!
       INCLUDE 'c2poly.i'
!
!  Pass # of sites to c2poly.i. 
      Numsite = Numsit
!
      calcVersion = 11

       If (Kjob .eq. 1) lu_out = get4unit()
       Open(lu_out,File= IM_file_name,status='new',iostat=ierr)
        If (ierr .ne. 0) write (6,'(" IM file already exists.")')
!
      fd_out = fnum(lu_out)
!        write (6,*) 'd_out1: Kjob,lu_out,fd_out ', Kjob,lu_out,fd_out
        npoly = 5
        intrval = npoly * d_interval
!
       flush(lu_out)
       ierr = difxiowritestring0(fd_out, "CALC SERVER:", "NONE")
       ierr = difxiowritestring0(fd_out, "CALC PROGRAM:", "DIFXCALC")
       ierr = difxiowriteint0(fd_out, "CALC VERSION:", calcVersion)
       ierr = difxiowriteint0(fd_out, "START YEAR:", StartYr)
       ierr = difxiowriteint0(fd_out, "START MONTH:", StartMo)
       ierr = difxiowriteint0(fd_out, "START DAY:", StartDay)
       ierr = difxiowriteint0(fd_out, "START HOUR:", StartHr)
       ierr = difxiowriteint0(fd_out, "START MINUTE:", StartMin)
       ierr = difxiowriteint0(fd_out, "START SECOND:", StartSec)
       ierr = difxiowriteint0(fd_out, "POLYNOMIAL ORDER:", npoly)
       ierr = difxiowriteint0(fd_out, "INTERVAL (SECS):", intrval)
!
      if (UVW .eq. 'uncorr')                                            &
     & ierr = difxiowritestring0(fd_out, "ABERRATION CORR:",            &
     &                   "UNCORRECTED")
      if (UVW .eq. 'approx')                                            &
     & ierr = difxiowritestring0(fd_out, "ABERRATION CORR:",            &
     &                   "APPROXIMATE")
      if (UVW .eq. 'exact ')                                            &
     & ierr = difxiowritestring0(fd_out, "ABERRATION CORR:",            &
     &                   "EXACT")
      if (UVW .eq. 'noatmo')                                            &
     & ierr = difxiowritestring0(fd_out, "ABERRATION CORR:",            &
     &                   "NO ATMOS")
!
       ierr = difxiowriteint0(fd_out, "NUM TELESCOPES:", Numsite-1)
       Do I = 1, numsite-1
        ierr = difxiowritestring1(fd_out, "TELESCOPE %d NAME:",    &
     &                   I-1, Sites(I+1))
       Enddo
       ierr = difxiowriteint0(fd_out, "NUM SCANS:", NumScans)
!
      Return
      End
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE d_out2(Iscan,J2m,lu_out,fd_out)
      IMPLICIT None
!
      INCLUDE 'cmxsr11.i'
!       Variables 'to':
!         1. NUMSTR - The total number of stars (radio sources) in the
!                     data base.
!
      Character*20 SrcName(MAX_ARC_SRC)
      Equivalence (LNSTAR(1,1), SrcName(1))
!
      Character*28 Bufout
      Real*8 Delay6(10), Atmdry6(10), Atmwet6(10), Ubase6(10),          &
     &       Vbase6(10), Wbase6(10), Acoef(10), delta, El6(10), Az6(10)
      Real*8 JDY2K
      Real*8 Delcoef(40      ,10), Atmdrycoef(40      ,10),             &
     &       Atmwetcoef(40      ,10), Ubasecoef(40      ,10),           &
     &       Vbasecoef(40      ,10), Wbasecoef(40      ,10)
      Integer*4 Iscan, J2m, Iph
      Integer*4 I, J, K, I1, I6, UTCsec, ierr, lu_out, npoly, intrval,  &
     &          MJD, Isec, Numpoly, get4unit, N, M, II, L, ISRC
!     Integer*4 Numtel, Scannum, Polynum, K1
      Integer*4 Numtel,          Polynum, K1
      Integer*4 fitPoly, c_out1, c_out2, c_out3, ierr1, ierr2, ierr3
      Integer*4 fd_out
      Integer*4 calcVersion
      Integer*4 difxiowriteint0
      Integer*4 difxiowriteint1
      Integer*4 difxiowriteint2
      Integer*4 difxiowritestring0
      Integer*4 difxiowritestring1
      Integer*4 difxiowritestring2
      Integer*4 difxiowritepoly26
      Character*9 Site_c(36)
!
       INCLUDE 'c2poly.i'
!
       INCLUDE 'd_input.i'
!
       Numtel = Numsite - 1
       Do k1 = 1, Numsite
        Site_C(k1)(1:8) = Sites(k1+1)
        Site_C(k1)(9:9) = CHAR(0)
       Enddo
!
!
!       Numpoly = NumEpochs/5
        Numpoly = Intrvls2min
!
!
       If (J2m .eq. 1) Then
        ierr = difxiowritestring1(fd_out, "SCAN %d POINTING SRC:",    &
     &                Iscan-1, SrcName(PointingSrc) )
        ierr = difxiowriteint1(fd_out, "SCAN %d NUM PHS CTRS:",       &
     &                Iscan-1, NumPhCntr)
        Do Iph = 1,NumPhCntr
        ierr = difxiowritestring2(fd_out, "SCAN %d PHS CTR %d SRC:",  &
     &                Iscan-1, Iph-1, SrcName(PhCntr(Iph)) )
        Enddo
        ierr = difxiowriteint1(fd_out, "SCAN %d NUM POLY:",           &
     &                Iscan-1, Numpoly)
       Endif
!
!
       N = 6
       M = 1
       delta = 24.
!
       I1 = (I-1)*5 
!
!   MJD and seconds (out of 86400) at I1 time
          MJD = JDY2K(Iymdhms_f(1,1),Iymdhms_f(1,2),Iymdhms_f(1,3))  &
     &          - 2400000.D0 
          Isec = Iymdhms_f(1,6) + Iymdhms_f(1,5)*60. +            &
     &           Iymdhms_f(1,4)*3600. 
!
        flush(lu_out)
        ierr = difxiowriteint2(fd_out, "SCAN %d POLY %d MJD:", Iscan-1, J2m-1, MJD)
        ierr = difxiowriteint2(fd_out, "SCAN %d POLY %d SEC:", Iscan-1, J2m-1, Isec)
!
      Do ISRC = 1, (NumPhCntr+1)     ! Pointing source/Phase centers loop
        Do J = 1, Numtel                      ! Station loop 
!
          Do K = 1, 6 
           Delay6(K)  = -Delay_f(K,1,J,ISRC) * 1.D6
           Atmdry6(K) = Atmdryd_f(1,K,1,J,ISRC) * 1.D6
           Atmwet6(K) = Atmwetd_f(1,K,1,J,ISRC) * 1.D6
           Ubase6(K)  = Ubase_f(K,1,J,ISRC)
           Vbase6(K)  = Vbase_f(K,1,J,ISRC)
           Wbase6(K)  = Wbase_f(K,1,J,ISRC)
           El6(K)     = El_f(1,K,1,J,ISRC)
           Az6(K)     = Az_f(1,K,1,J,ISRC)
          Enddo
!
!    Send to C routine fitPoly to compute polynomial coefficients
      ierr = fitPoly(Acoef, Delay6, %VAL(n), %VAL(m), %VAL(delta))
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d DELAY (us):",   &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
      Do L = 1, N
       Delcoef(J,L) = Acoef(L)
      Enddo
!
      ierr = fitPoly(Acoef, Atmdry6, %VAL(n), %VAL(m), %VAL(delta))
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d DRY (us):",     &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
      Do L = 1, N
       Atmdrycoef(J,L) = Acoef(L)
      Enddo
!
      ierr = fitPoly(Acoef, Atmwet6, %VAL(n), %VAL(m), %VAL(delta))
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d WET (us):",     &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
      Do L = 1, N
       Atmwetcoef(J,L) = Acoef(L)
      Enddo
!
      ierr = fitPoly(Acoef, Az6, %VAL(n), %VAL(m), %VAL(delta))
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d AZ:     ",     &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
!
      ierr = fitPoly(Acoef, El6, %VAL(n), %VAL(m), %VAL(delta))
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d EL GEOM:",     &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
!
      ierr = fitPoly(Acoef, Ubase6, %VAL(n), %VAL(m), %VAL(delta))
!     If (IM_out .eq. 1)                                                &
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d U (m):",        &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
      Do L = 1, N
       Ubasecoef(J,L) = Acoef(L)
      Enddo
!
      ierr = fitPoly(Acoef, Vbase6, %VAL(n), %VAL(m), %VAL(delta))
!     If (IM_out .eq. 1)                                                &
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d V (m):",        &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
      Do L = 1, N
       Vbasecoef(J,L) = Acoef(L)
      Enddo
!
      ierr = fitPoly(Acoef, Wbase6, %VAL(n), %VAL(m), %VAL(delta))
!     If (IM_out .eq. 1)                                                &
        ierr = difxiowritepoly26(fd_out, "SRC %d ANT %d W (m):",        &
           ISRC-1, J-1, Acoef(1), Acoef(2), Acoef(3), Acoef(4),       &
           Acoef(5), Acoef(6))
      Do L = 1, N
       Wbasecoef(J,L) = Acoef(L)
      Enddo
!
      Enddo       ! Station loop
      Enddo    ! Source loop
!
      Return
      End
