      Subroutine dscan(Iscan, Kjob)  
      implicit none
!
!      Common blocks used -
!
      INCLUDE 'ccon.i'
!       Variables 'to':
!         1. ILUOUT - A flag controlling output.
!
      INCLUDE 'cmxst11.i'
!       Variables 'to':
!         1. NUMSIT    - The total number of sites in the data base.
!         2. Zero_site - The site number of the site at the geocenter.
!
      INCLUDE 'cmxsr11.i'
!       Variables 'to':
!         1. NUMSTR - The total number of stars (radio sources) in the
!                     data base.
      Character*20 SrcName(MAX_ARC_SRC)
      Equivalence (LNSTAR(1,1), SrcName(1))
!
      INCLUDE 'cmxut11.i'
!       Variables 'to':
!         1. Xintv(2)    - First and last Julian Date of the current scan.
!         2. Intrvl(5,2) - First and last time tag of data in the current
!                          scan. (First index: year, month, day,
!                          hour, minute. Second index: first, last.)
!
      INCLUDE 'd_input.i'
!           Input variables:
!             1) calc_file_name - Job file '.calc' file name.
!
      INCLUDE 'c2poly.i'
!
!
!   Program specifications -
!
      Character*200 Buf1
      CHARACTER*12  Scan12
      Real*8    JDY2K, Jsecstart, Jsecstop, Jsec2
      Real*8    FJD2, Fday
      Real*8    JD1, JD2, StrtUTCmin, StopUTCmin, ProcMin
      Integer*4 IX, IX1, IX2, Iscan, Min2, IOS, ScanN, Kjob
      Integer*4 JTAG1(5), JTAG2(5)
      Integer*4 get4unit
      Integer*4 I, N, Unit1, J
      SAVE      Unit1
!
!     PROGRAMMER - David Gordon January 2015 
!                  DG, 2016-July-07  Kjob added to conserve LU numbers
!
!
!     Initialize scan inputs.
        ScanID       = '          '
        ScanNum      = -1
        ScanStrt     = -1 
        ScanDur      = -1
        PointingSrc  = -1
        NumPhCntr    = -1
        PhCntrNum    = -1
        Do I = 1, 500
         PhCntr(I)   = -1
        Enddo
        Near_Far = 'Far-field '
!
      If (Iscan .gt. 1) Go to 200
!
!  First time: open the file and read down to the 'SCAN' section.
       If (Kjob .eq. 1) Unit1 = get4unit()
       OPEN(Unit1,FILE=calc_file_name,STATUS='OLD',ACTION='READ',IOSTAT=IOS)
       IF(IOS.ne.0) Write(6,'("Open Error for file: ",A128)') calc_file_name
!
 100  Continue
      Read(Unit1,'(A200)',end=200) Buf1
      If (Buf1(1:10).eq.'NUM SCANS:') Go to 200
      Go to 100
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 200  Continue
!
      Read (Unit1,'(A200)',end=300) Buf1
!
      If (Buf1(1:4).eq.'SCAN') Then
       IX = INDEX(Buf1,'IDENTIFIER:')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) ScanN
        If(ScanN+1 .eq. Iscan)  Then 
         ScanNum = ScanN + 1            ! For Fortran
         Scan12 = Buf1((IX+11):(IX+22)) 
         Scan12 = ADJUSTL(Scan12)
         ScanID = Scan12(1:10)
         Go To 200
        Else
         Backspace(Unit1)
         Go to 300 
        Endif
       Endif 
!
       IX = INDEX(Buf1,'START (S):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) ScanN
        If(ScanN+1 .eq. Iscan)  Then 
         ScanNum = ScanN + 1            ! For Fortran
         Read(Buf1((IX+10):(IX+17)),*) ScanStrt
         Go To 200
        Else           
         Backspace(Unit1)
         Go to 300
        Endif
       Endif 
!
       IX = INDEX(Buf1,'DUR (S):')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) ScanN
        If(ScanN+1 .eq. Iscan)  Then 
         ScanNum = ScanN + 1            ! For Fortran
         Read(Buf1((IX+8):(IX+17)),*) ScanDur
         Go To 200
        Else           
         Backspace(Unit1)
         Go to 300
        Endif
       Endif 
!
       IX = INDEX(Buf1,'POINTING SRC:')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) ScanN
        If(ScanN+1 .eq. Iscan)  Then 
         ScanNum = ScanN + 1            ! For Fortran
         Read(Buf1((IX+13):(IX+16)),*) PointingSrc 
          PointingSrc = PointingSrc + 1                    ! For Fortran
         Go To 200
        Else           
         Backspace(Unit1)
         Go to 300
        Endif
       Endif 
!
       IX = INDEX(Buf1,'NUM PHS CTRS:')
       If (IX .gt. 0) Then
        Read(Buf1(5:IX-1),*) ScanN
        If(ScanN+1 .eq. Iscan)  Then 
         ScanNum = ScanN + 1            ! For Fortran
         Read(Buf1((IX+13):(IX+16)),*) NumPhCntr
         Go To 200
        Else           
         Backspace(Unit1)
         Go to 300
        Endif
       Endif 
!
!      IX = INDEX(Buf1,'PHS CTR 0:')
       IX1 = INDEX(Buf1,'PHS CTR')
       IX2 = INDEX(Buf1,':')
       If (IX1 .gt. 0 .and. IX2 .gt. 0) Then
        Read(Buf1(5:IX1-1),*) ScanN
        If(ScanN+1 .eq. Iscan)  Then 
         ScanNum = ScanN + 1            ! For Fortran
         Read(Buf1((IX1+7):(IX2-1)),*) PhCntrNum
          PhCntrNum = PhCntrNum +1                         ! For Fortran
         Read(Buf1((IX2+1):(IX2+6)),*) PhCntr(PhCntrNum)
          PhCntr(PhCntrNum) = PhCntr(PhCntrNum) +1         ! For Fortran
         Go To 200
        Else           
         Backspace(Unit1)
         Go to 300
        Endif
       Endif 
!
      Endif 
!
      Go to 200
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 300  Continue
!
      If (Iscan .eq. NumScans) Close(Unit1)
!
!
!     Get the start and stop times, all on the even minute
       JTAG1(1) = StartYr
       JTAG1(2) = StartMo
       JTAG1(3) = StartDay
       JTAG1(4) = StartHr
       JTAG1(5) = StartMin
       Jsecstart = StartSec + ScanStrt 
! Recompute starting yr, month, day, hr, min, sec.
        IF (Jsecstart .ge. 59.999999999D0) Call FixEpoch(JTAG1,Jsecstart)
!
       JTAG2(1) = JTAG1(1)
       JTAG2(2) = JTAG1(2)
       JTAG2(3) = JTAG1(3)
       JTAG2(4) = JTAG1(4)
       JTAG2(5) = JTAG1(5)
! Stop time plus 1 second (to get an additional polynomial interval
!  if stop time is on the even minute).
       Jsecstop = Jsecstart + ScanDur + 1
        IF (Jsecstop .ge. 59.999999999D0) Call FixEpoch(JTAG2,Jsecstop)
!
! Convert start and stop times to the even minute before and after the
!  scan start and stop.
       Intrvl(1,1) = JTAG1(1)
       Intrvl(2,1) = JTAG1(2)
       Intrvl(3,1) = JTAG1(3)
       Intrvl(4,1) = JTAG1(4)
        Min2 = JTAG1(5)/2
        JTAG1(5) = Min2*2  
       Intrvl(5,1) = JTAG1(5)
!  Julian date of 2-minute interval start time
       JD1  = JDY2K (JTAG1(1),JTAG1(2),JTAG1(3)) 
       Xintv(1) = JD1 + JTAG1(4)/24.D0 + JTAG1(5)/1440.D0
!
        Jsec2 = Jsecstop + 119
        Call FixEpoch(JTAG2,Jsec2)
       Intrvl(1,2) = JTAG2(1)
       Intrvl(2,2) = JTAG2(2)
       Intrvl(3,2) = JTAG2(3)
       Intrvl(4,2) = JTAG2(4)
        Min2 = JTAG2(5)/2
        JTAG2(5) = Min2*2  
       Intrvl(5,2) = JTAG2(5)
!  Julian date of 2-minute interval stop time
       JD2  = JDY2K (JTAG2(1),JTAG2(2),JTAG2(3)) 
       Xintv(2) = JD2 + JTAG2(4)/24.D0 + JTAG2(5)/1440.D0
!  
! Start/Stop time in UTC minutes (1 day = 1440 minutes)
        StrtUTCmin = (Xintv(1) - JD1) * 1440.D0
        StopUTCmin = (Xintv(2) - JD1) * 1440.D0
        ProcMin = StopUTCmin - StrtUTCmin + .00001
!         # of 2-minute intervals this scan
        Intrvls2min = ProcMin/2 + 1   ! Add an extra 2-minutes
!         # of calc epochs this scan
        NumEpochs = ((ProcMin*60. + .001)/d_interval) + 1
!
!      write(6,*) 'dScan: Numsrc,Numstr,NumSpace: ', Numsrc,Numstr,NumSpace
!      write(6,*) 'dScan: SrcName: ', (SrcName(J), J=1,Numsrc)
!      write(6,*) 'dScan: SpName: ', (SpName(J), J=1,NumSpace)
!  Far-field or Near-field?
      If(NumSpace .ge. 1) Then
       Do J = 1, NumSpace
        If(SrcName(PointingSrc) .eq. SpName(J)) Then
         Near_Far = 'Near-field'
         If(Verbose .ge. 1) Write(6,1011) NF_Model, Iscan-1, SrcName(PointingSrc)
 1011    Format(' Using the ',A8,' Near-field model for scan ',I3,'  Source ',A20)
         Call SPACEI(J)
         Go to 310
        Endif
       Enddo
 310   Continue
      Endif
       If(Verbose .ge. 1 .and. Near_Far .eq. 'Far-field ')              &
     &            write(6,1012) Iscan-1, SrcName(PointingSrc)
 1012    Format(' Using the Far-field model for scan ',I3,'  Source ',A20)
!
!     Go back to the main.
      RETURN
      END
