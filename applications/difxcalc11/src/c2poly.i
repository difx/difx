!   c2poly.i
!
      Integer*4  Max_Epochs     ! Maximum number of epochs per 2-minute interval
      Parameter (Max_Epochs = 6)
!
      Integer*4  Nstation1      ! 1 for geocenter mode,
!                                 Maxstat-1 for baseline mode
      Parameter (Nstation1 = 1)     
!
!     Nstation2 must be kept in sync with Max_Stat, MaxStat from other files
      Integer*4  Nstation2      ! Maximum # of stations
      Parameter (Nstation2 = 254)
!
      Integer*4  Max_Source     ! Maximum # of sources, 
!                                 (pointing and phase center sources)
      Parameter (Max_Source = 1001)
!
      Integer*4  IB             ! 1 for geocenter mode, 2 for baseline
      Parameter (IB = 1)
!
!     Integer*4 Max_base
!     Parameter(Max_base = 40)
!  If geocenter mode or base_station mode is always used, 
!    set Max_base to the maximum number of antennas
!  If baseline mode is always/sometimes used, set Max_base to
!     the maximum # of baselines (N*(N-1)/2).
!
      Real*8 Delay_f(Max_Epochs,Nstation1,Nstation2,Max_Source),          &
     &   Rate_f(Max_Epochs,Nstation1,Nstation2,Max_Source),               &
     &   Atmdryd_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),         &
     &   Atmdryr_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),         &
     &   Atmwetd_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),         &
     &   Atmwetr_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),         &
     &   Ubase_f(Max_Epochs,Nstation1,Nstation2,Max_Source),              &
     &   Vbase_f(Max_Epochs,Nstation1,Nstation2,Max_Source),              &
     &   Wbase_f(Max_Epochs,Nstation1,Nstation2,Max_Source),              &
     &   El_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),              &
     &   Az_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),              &
     &   StaXt_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),           &
     &   StaYt_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),           &
     &   StaZt_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),           &
     &   StaX_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),            &
     &   StaY_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source),            &
     &   StaZ_f(IB,Max_Epochs,Nstation1,Nstation2,Max_Source)
      Character*20 Xsource
! ???????
      Character*8 Site1(Nstation2,Max_Epochs), Site2(Nstation2,Max_Epochs)
!
      Integer*4 Numsite, Iymdhms_f(Max_Epochs,6), Numbaseline,          &
     &          NumPhCenter 
!
      COMMON / OUT_C /Delay_f, Rate_f, Atmdryd_f, Atmdryr_f,            &
     &       Atmwetd_f, Atmwetr_f, Ubase_f, Vbase_f, Wbase_f,           &
     &       El_f, Az_f, StaX_f, StaY_f, StaZ_f,                        &
     &       StaXt_f, StaYt_f, StaZt_f,                                 &
     &       Iymdhms_f, Numsite, Numbaseline, NumPhCenter,              &
     &       Site1, Site2, Xsource
!
!       1. Delay_f(Max_base,Max_Epoch) -
